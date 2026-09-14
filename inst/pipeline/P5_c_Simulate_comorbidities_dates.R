################################################################################
#
#   Script:       P5_c_Simulate_comorbidities_dates.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R objects: comorbid, patient
#
#   Data created:	R objects: comorbid_dt, comorbid_meds
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script simulates the dates of patient comorbidities.  
#
################################################################################
#
#   Packages needed: lubridate, data.table
#
################################################################################




        

############################################
#  Lists of chronic and repeated diseases  #
############################################

# List of chronic comorbidities (those with an "onset")
chronic_dis <- c("diabetes",
                 "chd",
                 "heartfail",
                 "other_athero",
                 "pvd",
                 "fibrosis",
                 "copd",
                 "dementia",
                 "barretts",
                 "cancer_lung",
                 "cancer_prostate",
                 "cancer_breast",
                 "cancer_bowel",
                 "cancer_gastric", 
                 "cancer_oth")
onset_vars<- paste0(chronic_dis, "_onset")
onset_date_vars <- paste0(onset_vars, "_date")


# List of repeated events
events <- c("ht",
            "mi",
            "stroke",
            "pancreatitis",
            "hpylori",
            "gerd",
            "upper_gi_bleed",
            "oesophagitis",
            "peptic_ulcer")
event_vars <- paste0(events, "_event")
event_date_vars <- paste0(event_vars, "_date")
event_first_date_vars <- paste0(event_vars, "_first_date")



########################################
#  Generate exact dates of occurrence  #
########################################

# Use the patient's exact follow-up boundary when placing dates. Some interval
# dateouts are reconstructed from rounded ages and can drift slightly beyond the
# patient-level enddate_init.
comorbid[patient, enddate_init := i.enddate_init, on = "patient_id"]
comorbid[, dateout := pmin(dateout, enddate_init)]
dur <- pmax(as.numeric(comorbid$dateout - comorbid$datein), 0)

### Generate dates

# Onset and event dates: assign random date between datein and dateout  
for (i in c(onset_vars, event_vars)) {
  newvar <- paste0(i, "_date")
  rows <- which(comorbid[[i]] == 1)
  comorbid[, (newvar) := as.Date(NA)]

  if (length(rows) > 0) {
    comorbid[rows, (newvar) :=
          as.Date(comorbid$datein[rows] + round(runif(length(rows))*dur[rows]))]
  }
}

###  Mark first event dates (for repeated events) and onset dates (chronic)
comorbid[, (event_first_date_vars) := mget(event_date_vars)]
first <- c(onset_date_vars, event_first_date_vars)
comorbid[, (first) := lapply(first, function(col){
      vals <- get(col)
      if (all(is.na(vals))) as.Date(NA) else min(vals, na.rm = TRUE)
    }), by = patient_id]

###############
#  CKD dates  #
############### 

comorbid[, ckd_date := fifelse(!(ckd_change==1 | ckd_first==1), as.Date(NA),
                        as.Date(datein + runif(.N) * (dateout - datein)) ) ]




###############################################################
#  Generate dates of endoscopy around relevant comorbidities  #
###############################################################


# Date of endoscopy
comorbid[,u:=runif(.N)]
comorbid[, endoscopy_date := fcase(
  endoscopy_event == 1 & cancer_gastric_onset == 1,
  cancer_gastric_onset_date  -
    round(u * pmin(cancer_gastric_onset_date - datein, 60)),

  endoscopy_event == 1 & barretts_onset == 1,
  barretts_onset_date -
    round(u * pmin(barretts_onset_date - datein, 60)),
  
  endoscopy_event == 1 & upper_gi_bleed_event == 1,
  upper_gi_bleed_event_date +
    round(u * pmin(dateout - upper_gi_bleed_event_date, 60)),
  
  endoscopy_event == 1 & peptic_ulcer_event == 1,
  peptic_ulcer_event_date +
    round(u * pmin(dateout - peptic_ulcer_event_date, 60)),
  
  endoscopy_event == 1 & oesophagitis_event == 1,
  oesophagitis_event_date +
    round(u * pmin(dateout - oesophagitis_event_date, 60)),
  
  endoscopy_event == 1 & hpylori_event == 1,
  hpylori_event_date +
    round(u * pmin(dateout - hpylori_event_date, 60)),
  
  endoscopy_event == 1,
  as.Date(pmax(
    pmin(datein + round(u * (dateout - datein)), dateout),
    datein
  )),  default = as.Date(NA))]
comorbid[, u := NULL]




################
#  Keep dates  #
################

comorbid_meds <- copy(comorbid)

keepcols <- c(
  "patient_id",
  onset_date_vars,
  event_date_vars,
  event_first_date_vars,
  "ckd_date", "ckd",
  "endoscopy_date"
)
comorbid_dt <- copy(comorbid[, ..keepcols])
comorbid_meds[, enddate_init := NULL]


# Prepare CKD dates for death simulation
comorbid_dt[,ckd:= fifelse(is.na(ckd_date), NA, ckd)]
comorbid_dt[,ckd3a_date := fifelse(ckd=="Stage 3a", ckd_date, as.Date(NA))]
comorbid_dt[,ckd3b_date := fifelse(ckd=="Stage 3b", ckd_date, as.Date(NA))]
comorbid_dt[,ckd4_date := fifelse(ckd=="Stage 4", ckd_date, as.Date(NA))]
comorbid_dt[,ckd5_date := fifelse(ckd=="Stage 5", ckd_date, as.Date(NA))]
ckd_stage_date_vars <- c("ckd3a_date", "ckd3b_date", "ckd4_date", "ckd5_date")
comorbid_dt[, ckd_first_date := {
  vals <- unlist(.SD, use.names = FALSE)
  if (all(is.na(vals))) as.Date(NA) else min(vals, na.rm = TRUE)
}, by = patient_id, .SDcols = ckd_stage_date_vars]
comorbid_dt[,c("ckd_date", "ckd") := NULL]
rm(comorbid, ckd_stage_date_vars, dur)
