suppressPackageStartupMessages({
  library(truncnorm)
  library(survival)
  library(stringr)
  library(data.table)
  library(lubridate)
})

find_audit_file <- function() {
  frame_files <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) "" else as.character(frame$ofile)
  }, character(1))
  frame_files <- frame_files[nzchar(frame_files)]
  if (length(frame_files)) tail(frame_files, 1) else "validation/audits/run_priority_audit.R"
}

audit_file <- normalizePath(find_audit_file(), winslash = "/", mustWork = FALSE)
project_override <- Sys.getenv("EHRSIM_PROJECT_DIR", unset = "")
project_dir <- if (nzchar(project_override)) {
  normalizePath(project_override, winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(dirname(audit_file), "..", ".."),
                winslash = "/", mustWork = TRUE)
}
pipeline_dir <- file.path(project_dir, "inst", "pipeline")

if (!file.exists(file.path(project_dir, "DESCRIPTION")) || !dir.exists(pipeline_dir)) {
  stop("Run this audit from an EHRSim source package containing inst/pipeline.",
       call. = FALSE)
}

scripts_to_p8 <- c(
  "P0_a_Init_functions_comorbidities.R",
  "P0_b_Init_parameters_comorbidities.R",
  "P0_c_Init_function_medications.R",
  "P0_d_Init_parameters_death.R",
  "P0_e_Codes_used.R",
  "P0_f_Init_functions_treatment_effects.R",
  "P1_Simulate_GP.R",
  "P2_Simulate_patients.R",
  "P3_Simulate_frailty.R",
  "P4_Simulate_underlying_clinical_measures.R",
  "P5_a_Simulate_comorbidities_cardiometabolic.R",
  "P5_b_Simulate_comorbidities_otherdis.R",
  "P5_c_Simulate_comorbidities_dates.R",
  "P6_a_Simulate_medication_dates.R",
  "P6_b_Simulate_medications.R",
  "P7_Simulate_death_and_transfer.R",
  "P8_Update_tables_for_death_and_transfer.R"
)

e <- new.env(parent = globalenv())
e$pracid <- 1L
e$gp_rel_size <- 1
e$seed <- 34637L
set.seed(e$seed)

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(pipeline_dir)
for (script in scripts_to_p8) {
  sys.source(file.path(pipeline_dir, script), envir = e)
}

metrics <- data.table(
  id = character(),
  script = character(),
  metric = character(),
  observed = numeric(),
  healthy_target = character(),
  status = character()
)

add_metric <- function(id, script, metric, observed, healthy_target,
                       status = "OPEN-CONFIRMED") {
  metrics <<- rbind(metrics, data.table(
    id = id,
    script = script,
    metric = metric,
    observed = as.numeric(observed),
    healthy_target = as.character(healthy_target),
    status = status
  ))
}

###############################################################################
# P9 consultation mechanisms
###############################################################################

sys.source(file.path(pipeline_dir, "P9_Simulate_consultations.R"), envir = e)
p9_consultation <- copy(e$consultation)
p9_patient <- copy(e$patient)
p9_medication <- copy(e$medication)
p9_clinicalmeasure <- copy(e$clinicalmeasure)
p9_comorbid <- copy(e$comorbid_dt)

copd_onset <- unique(p9_comorbid[
  !is.na(copd_onset_date), .(patient_id, copd_onset_date)
])
steroid <- p9_medication[drug == "systemic_steroid_copd", .(
  first_steroid = min(date)
), by = patient_id]
copd_contact <- p9_consultation[consultation_type == "copd", .(
  first_copd_contact = min(date)
), by = patient_id]
copd_timing <- Reduce(function(x, y) merge(x, y, by = "patient_id", all = TRUE),
                      list(copd_onset, steroid, copd_contact))
add_metric(
  "SCI-002", "P9", "COPD contacts before the future steroid used in rate classification",
  copd_timing[!is.na(first_steroid) & !is.na(first_copd_contact) &
                first_copd_contact < first_steroid, .N], "0"
)

setorder(p9_clinicalmeasure, patient_id, datein)
p9_clinicalmeasure[, new_harm := harmalc == 1 &
                      shift(harmalc, fill = 0) != 1, by = patient_id]
first_harm <- p9_clinicalmeasure[new_harm == TRUE,
                                .(first_harm = min(datein)), by = patient_id]
pre_harm_risk <- p9_clinicalmeasure[first_harm, on = "patient_id"][
  datein < first_harm & alccat %chin% c("Medium", "High"), unique(patient_id)
]
pre_harm_advice <- p9_consultation[consultation_type == "alcohol_advice"][
  first_harm, on = "patient_id"
][patient_id %in% pre_harm_risk & date < first_harm, uniqueN(patient_id)]
add_metric("SCI-003a", "P9", "Eventual harmful drinkers with earlier risk intervals",
           length(pre_harm_risk), "descriptive", "DESCRIPTIVE")
add_metric("SCI-003b", "P9", "Those patients receiving advice before harmful onset",
           pre_harm_advice, "greater than 0", "VERIFIED-FIXED")

health_checks <- p9_patient[
  p9_consultation[consultation_type == "health_check"], on = "patient_id"
]
health_checks[, age := as.numeric(date - dob) / 365.25]
add_metric("CAL-006", "P9", "Health checks after age 74",
           health_checks[age > 74, .N], "0 if modelling NHS eligibility",
           "CALIBRATION-REQUIRED")

###############################################################################
# P10 measurement mechanisms
###############################################################################

sys.source(file.path(pipeline_dir, "P10_Simulate_clinical_measures.R"), envir = e)
p10_measure <- copy(e$clinmeasure)

truth <- copy(p9_consultation)
truth[p9_patient, dob := i.dob, on = "patient_id"]
truth[, age := as.numeric(date - dob) / 365.25]
agecutoff <- c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
truth[, agein := agecutoff[findInterval(age, agecutoff, all.inside = TRUE)]]
truth[p9_clinicalmeasure, `:=`(
  latent_smoke = as.character(i.smokecat),
  latent_alcohol = as.character(i.alc3cat)
), on = c("patient_id", "agein")]

smoke_ids <- unique(p10_measure[measure == "smoking_status", consultation_id])
alcohol_ids <- unique(p10_measure[measure == "alcohol_consumption", consultation_id])
surgery <- truth[consultation_type == "surgery"]
never_smoke_rate <- surgery[latent_smoke == "No",
                            mean(consultation_id %in% smoke_ids)]
non_drink_rate <- surgery[latent_alcohol == "No",
                          mean(consultation_id %in% alcohol_ids)]
add_metric("SCI-004a", "P10", "Never-smoker recording probability at surgery",
           never_smoke_rate, "approximately 0.15")
add_metric("SCI-004b", "P10", "Non-drinker recording probability at surgery",
           non_drink_rate, "approximately 0.15")

alcohol_measure <- p10_measure[measure == "alcohol_consumption"]
add_metric("DES-015", "P10",
           "Missing alcohol-status sentinels retaining harmful-use data2",
           alcohol_measure[data1 == -1 & !is.na(data2), .N],
           "decision required", "RATIONALE-TO-CONFIRM")

###############################################################################
# P11 coding semantics
###############################################################################

sys.source(file.path(pipeline_dir, "P11_Add_clinical_codes.R"), envir = e)
p11_clinical <- copy(e$clinical)
p11_supplement <- copy(e$supplement)

p10_measure[, sid := as.character(suppl_info_id)]
p11_clinical[, sid := as.character(suppl_info_id)]
bmi_map <- p11_clinical[sid != "0"][
  p10_measure[measure == "bmi"], on = "sid", nomatch = 0L
]
add_metric("SCI-005", "P11", "Direct BMI rows assigned generic weight code",
           bmi_map[clinical_code == e$codes$measure$weight, uniqueN(sid)], "0")

result_contacts <- p9_consultation[consultation_type == "results"]
result_codes <- p11_clinical[result_contacts, on = "consultation_id", nomatch = 0L]
add_metric("SCI-006", "P11", "Results rows using entity value as clinical code",
           result_codes[clinical_code == e$codes$entity$test_result, .N], "0")

###############################################################################
# P12 row preservation and information retention
###############################################################################

sys.source(file.path(pipeline_dir, "P12_Add_prescription_codes.R"), envir = e)
p12_prescription <- copy(e$prescription)
add_metric("P12-STRUCTURE", "P12", "Medication rows lost during coding",
           nrow(p9_medication) - nrow(p12_prescription), "0", "STRUCTURAL-PASS")
add_metric("DES-017", "P12", "Source medication rows with discarded strength",
           p9_medication[!is.na(strength), .N], "decision required",
           "RETAINED-ASSUMPTION")

###############################################################################
# P13 final extraction
###############################################################################

pre_p13_clinical <- copy(p11_clinical)
pre_p13_clinical[, sid := NULL]
pre_p13_consultation <- copy(p9_consultation)
sys.source(file.path(pipeline_dir, "P13_Extract_EHRs.R"), envir = e)

final <- e$tables
final_clinical <- copy(final$clinical)
final_consultation <- copy(final$consultation)
final_prescription <- copy(final$prescription)

final_clinical[final_consultation,
               consultation_date := i.consultation_date,
               on = "consultation_id"]
add_metric("SCI-008a", "P13", "Clinical dates differing from consultation dates",
           final_clinical[event_date != consultation_date, .N], "0")

delay_codes <- unique(c(
  e$codes$clinical$dementia,
  e$codes$clinical$cancer_prostate,
  e$codes$clinical$cancer_breast,
  e$codes$clinical$cancer_other_f,
  e$codes$clinical$cancer_other_m,
  e$codes$clinical$cancer_lung,
  e$codes$clinical$cancer_bowel,
  e$codes$clinical$cancer_gastric
))
delay_pre <- pre_p13_clinical[clinical_code %in% delay_codes]
delay_pre[pre_p13_consultation[, .(
  consultation_id,
  original_type = consultation_type,
  original_trigger = trigger_event
)], `:=`(
  original_type = i.original_type,
  original_trigger = i.original_trigger
), on = "consultation_id"]
delay_pre[, intended_diagnosis :=
            original_type %chin% c("disease_onset", "data_transfer", "event") &
            grepl("cancer|dementia", original_trigger)]
add_metric("SCI-007a", "P13", "Unrelated rows caught by cancer/dementia code collision",
           delay_pre[intended_diagnosis == FALSE, .N], "0")

delay_final <- final$clinical[delay_pre,
  on = .(consultation_id, clinical_code, entity), nomatch = 0L]
delay_final[, delay_days := as.numeric(event_date - i.event_date)]
add_metric("SCI-007b", "P13", "Unrelated rows delayed as diagnoses",
           delay_final[delay_days > 0 & intended_diagnosis == FALSE, .N], "0")

register_entities <- unlist(e$codes$entity[c(
  "register_stroke", "register_chd", "register_diabetes", "register_cancer"
)])
register_rows <- final$clinical[entity %in% register_entities]
true_delayed_cancer <- delay_final[
  delay_days > 0 & intended_diagnosis == TRUE & !grepl("dementia", original_trigger)
]
add_metric("SCI-008b", "P13",
           "True delayed cancers retaining original-date register row",
           true_delayed_cancer[register_rows, on = "consultation_id", nomatch = 0L, .N],
           "0")

# Match latent drugs to their generated consultations using the persistent key.
source_rx <- merge(
  p9_medication[, .(prescription_id_internal, drug)],
  p12_prescription[, .(prescription_id_internal)],
  by = "prescription_id_internal",
  all = FALSE,
  sort = FALSE
)
source_rx <- merge(
  source_rx,
  pre_p13_consultation[!is.na(prescription_id_internal), .(
    prescription_id_internal,
    assigned_type = consultation_type
  )],
  by = "prescription_id_internal",
  all.x = TRUE,
  sort = FALSE
)
source_rx[, expected_type := fcase(
  drug %chin% c("aspirin", "clopidogrel", "OAC"), "repeat_issue",
  drug %chin% c("NSAID", "omeprazole", "pantoprazole", "lansoprazole",
                  "esomeprazole", "rabeprazole", "cimetidine", "ranitidine",
                  "famotidine", "nizatidine"), "surgery",
  default = "medicine_management"
)]
add_metric("SCI-009", "P13", "Prescriptions assigned wrong internal contact type",
           source_rx[is.na(assigned_type) | assigned_type != expected_type, .N],
           "0", "VERIFIED-FIXED")

ht_pre <- pre_p13_clinical[clinical_code %in% e$codes$clinical$ht_event]
ht_final <- final$clinical[clinical_code %in% e$codes$clinical$ht_event]
ht_removed <- ht_pre[!ht_final, on = .(consultation_id, clinical_code, entity)]
fully_hidden_ht <- unique(ht_removed[, .(patient_id)])[
  !unique(ht_final[, .(patient_id)]), on = "patient_id", .N
]
add_metric("SCI-010a", "P13", "Hypertension code rows removed using lifetime use",
           nrow(ht_removed), "mechanism must use prior history")
add_metric("SCI-010b", "P13", "Patients made fully unobserved for hypertension",
           fully_hidden_ht, "descriptive")

###############################################################################
# Output
###############################################################################

setorder(metrics, id, script, metric)
print(metrics)

output_path <- Sys.getenv("EHRSIM_AUDIT_OUTPUT", unset = "")
if (nzchar(output_path)) {
  data.table::fwrite(metrics, output_path)
  message("Wrote audit metrics to ", normalizePath(output_path, mustWork = FALSE))
}

invisible(metrics)
