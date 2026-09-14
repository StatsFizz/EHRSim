################################################################################
#
#   Script:       P13_Extract_EHRs.R
#
#   Created by:   Elizabeth Williamson 27/11/2025
#
#   Data used:		patient, deathdate, consultation, clinical, 
#                 supplement, prescription
#
#   Data created:	R objects stored in parent list, indexed by prac-id
#                     patient_table, imd_table, death_linked_table,
#                     consultation_table, clinical_table, prescription
#                     supplementary_information_table  
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script implements typical measurement errors (missing data
#               and delays in entry etc) in GP data, and finalises the
#               tables for the EHR simulated data.
#
################################################################################







###################
#  Patient table  #
###################

setnames(patient, "practice", "practice_id", skip_absent=TRUE)
patient_table <- copy(as.data.table(patient))

# Impose: 
# - some ethnicity data missing
# - some error in GP death date (often later, occasionally earlier)
# - only keep death dates that are recorded to be prior to transfer out
# - only year of birth in release, not date
# - remove enddate 
# - add data-quality flag

### Make some ethnicity information missing 

# (more missing for males, few healthcare interactions)
consult_counts <- consultation[, .N, by = patient_id]  
patient_table[consult_counts, N := i.N,  on = "patient_id"]
patient_table[is.na(N), N := 0L]
patient_table[, miss:=1/(1 + exp(-(-1 + 0.25*(sex=="Male") -0.25*N))) > runif(.N)]
# (more declined for non-White groups)
patient_table[, decline:=1/(1 + exp(-(-1.5 + 0.05*(sex=="Male") 
                                   + 0.5*(ethnicity=="Asian") 
                                   + 0.5*(ethnicity=="Black")
                                   + 0.75*(ethnicity=="Mixed")
                                   + 0.78*(ethnicity=="Other")
                                   -0.1*N))) > runif(.N)]
patient_table[decline==TRUE, ethnicity:="Not stated"]
patient_table[miss==TRUE, ethnicity:=NA]
patient_table[, c("N", "miss", "decline") := NULL]

# Merge in (true) death-date
patient_table <- merge(patient_table, deathdate[, .(patient_id, deathdate)],
  by = "patient_id",  all.x = TRUE)

# Add error-prone death date for 10% of deaths (correct within a month)
patient_table[!is.na(deathdate), 
              death_date := fifelse(runif(.N)<0.9, deathdate, 
                              as.Date(deathdate + round(runif(.N, -7, 21))))]

# Remove death date if on or after transfer date
patient_table[,death_date:= fifelse(!is.na(transfer_date) & !is.na(death_date)
                                    & death_date>=transfer_date,
                                     NA, death_date)]

# Add a data-quality flag
patient_table[, data_flag := 1L]

# Order and select desired variables for final patient table
setorder(patient_table, patient_id)
patient_table <- patient_table[, .(patient_id, practice_id, sex,
  yob, ethnicity, reg_date, transfer_date, death_date, data_flag)]



###############
#  IMD table  #
###############

imd_table<-copy(patient)
imd_table<-imd_table[,.(patient_id, practice_id, deprivation_person, deprivation_prac)]

# Impose: 
# - Create 1-5 IMD indicator (higher=less deprivation) from deprivation 
#   index used in data generation (1-5, higher=more deprivation)
# - add some missing IMD person-level data (more for higher deprivation)

imd_table[,imd_person:= fcase(
  deprivation_person==1, 5,
  deprivation_person==2, 4,
  deprivation_person==3, 3,
  deprivation_person==4, 2,
  deprivation_person==5, 1,
  default=NA_integer_)][,deprivation_person:=NULL]

imd_table[,imd_practice:= fcase(
  deprivation_prac==1, 5,
  deprivation_prac==2, 4,
  deprivation_prac==3, 3,
  deprivation_prac==4, 2,
  deprivation_prac==5, 1,
  default=NA_integer_)][,deprivation_prac:=NULL]

# Set some patient-level IMD to missing (more so for more deprived)
imd_table[, imd_person:= fcase(
  imd_person==1 & runif(.N)<0.01,  NA_real_, 
  imd_person==2 & runif(.N)<0.008, NA_real_,
  imd_person==3 & runif(.N)<0.006, NA_real_,
  imd_person==4 & runif(.N)<0.004, NA_real_,
  imd_person==5 & runif(.N)<0.002, NA_real_,
  default = imd_person)]  

# Tidy
setorder(imd_table, patient_id)




#################
#  Death table  #
#################

# Tidy
death_linked_table <- copy(deathdate)
setnames(death_linked_table, 
         old=c("deathdate", "cod"),
         new=c("death_date_official", "cod_official"))
setorder(death_linked_table, patient_id)




#########################
#  Consultations table  #
#########################

consultation_table <- copy(consultation)

# Impose: 
# - Rename types of consultation
# - Set 1% of consultation types to "Data not entered"

consultation_table[, consultation_type := fcase(
  consultation_type=="administration", "Administration",
  consultation_type=="alcohol_advice", "Surgery",
  consultation_type=="chronic_disease_review", "Surgery",
  consultation_type=="clinic", "Clinic",
  consultation_type=="contraception", "Surgery",
  consultation_type=="copd",  "Clinic",  
  consultation_type=="data_transfer",  "Data Transferred from Other System",
  consultation_type=="diabetes", "Clinic",
  consultation_type=="disease_onset", "Initial post-discharge review",
  consultation_type=="event", "Initial post-discharge review",
  consultation_type=="fup_disease_onset","Surgery",
  consultation_type=="fup_event","Surgery",
  consultation_type=="harmful_alcohol_use", "Surgery",
  consultation_type=="health_check","Clinic",
  consultation_type=="hrt","Surgery",
  consultation_type=="mail_to_patient","Mail to patient",
  consultation_type=="medicine_management", "Surgery",
  consultation_type=="other","Other",
  consultation_type=="outpatients_letter","Outpatients letter",
  consultation_type=="phone_call_from_patient", "Phone call from patient",
  consultation_type=="phone_call_to_patient", "Phone call to patient",
  consultation_type=="referral_letter","Referral letter",
  consultation_type=="repeat_issue", "Surgery",
  consultation_type=="results","Test results",
  consultation_type=="smoke_cessation", "Surgery",
  consultation_type=="surgery", "Surgery",
  consultation_type=="third_party_consult", "Third party consultation",
  consultation_type=="weight_management", "Clinic",
  default=consultation_type)]

# Set some consultation types to "Data not entered" (1%)
consultation_table[,consultation_type:= fifelse(runif(.N)<0.01, 
                        "Data not entered", consultation_type)]
    

# Keep the medication-generated consultation rows for exact prescription matching.
prescription_consults <- consultation_table[
  !is.na(prescription_id_internal),
  .(prescription_id_internal, patient_id,
    prescription_date = date, consultation_id)
]
if (anyDuplicated(prescription_consults$prescription_id_internal)) {
  stop("Medication-generated consultation linkage IDs are duplicated.",
       call. = FALSE)
}

# Tidy
consultation_table[, c("trigger_event", "prescription_id_internal") := NULL]
setnames(consultation_table, old="date", new="consultation_date")
setorder(consultation_table, -consultation_id)




#####################################
#  Supplementary information table  #
#####################################

supplementary_information_table <- copy(supplement)

# Tidy
supplementary_information_table[,data1:=round(data1, 2)]
supplementary_information_table[,data2:=round(data2, 1)]

setorder(supplementary_information_table, -suppl_info_id)





#########################
#  Prescriptions table  #
#########################

prescription_table <- copy(prescription)

# Merge to add consultation ID  
setnames(prescription_table, old=c("date", "code_prescription", "days"), 
         new=c("prescription_date","prescription_code", "prescription_qty_days"))
if (anyDuplicated(prescription_table$prescription_id_internal)) {
  stop("Prescription linkage IDs are duplicated.", call. = FALSE)
}
prescription_rows_before_join <- nrow(prescription_table)
prescription_table <- merge(prescription_table,
                            prescription_consults,
                            by = c("prescription_id_internal", "patient_id",
                                   "prescription_date"),
                            all.x = TRUE, sort = FALSE)
if (nrow(prescription_table) != prescription_rows_before_join) {
  stop("Prescription linkage changed the number of prescription rows.",
       call. = FALSE)
}
prescription_table[, prescription_id_internal := NULL]

# Check all prescriptions have a consultation ID
if (any(is.na(prescription_table$consultation_id))) {
  stop("Some prescriptions have missing consultation IDs.", call. = FALSE)
}

# Tidy
setorder(prescription_table, prescription_date) 



####################
#  Clinical table  #
####################


# Impose: 
# - Hypertension often not diagnosed
# - Cancers diagnosed later, particularly higher deprivation
# - Dementia often diagnosed later, particularly lower deprivation 

clinical_table <- copy(clinical)

### Set some hypertension to be undiagnosed (less for high-healthcare and women)
clinical_table[consult_counts, N := i.N,  on = "patient_id"]
clinical_table <- merge(clinical_table, patient_table[, 
                      .(patient_id, sex, transfer_date, death_date)],
                       by = "patient_id",  all.x = TRUE)

clinical_table[clinical_code %in% codes$clinical$ht_event, 
               clinical_code:=fifelse(
                 1/(1 + exp(-(-1.5 + 0.05*(sex=="Male") -0.25*N))) > runif(.N),
                 NA_integer_, clinical_code)]

# Mark rows for undiagnosed hypertension
clinical_table[, diag_rm:= is.na(clinical_code)]

               
### Change some dementia dates (often later, particularly for lower deprivation)
clinical_table[patient, deprivation_person:=i.deprivation_person, on = "patient_id"]
clinical_table <- merge(clinical_table, deathdate[, .(patient_id, deathdate)],
                       by = "patient_id",  all.x = TRUE)
clinical_table[clinical_code %in% codes$clinical$dementia, 
               event_date:=fifelse(
                 plogis(-1.5 - 0.25*deprivation_person) > runif(.N),
                as.Date(event_date + round(abs(rnorm(.N, 0, 365.25)))), event_date)]


### Change some cancer dates (often later, particularly for higher deprivation)
clinical_table[clinical_code %in% c(codes$clinical$cancer_prostate,
                                    codes$clinical$cancer_breast,
                                    codes$clinical$cancer_other_f, 
                                    codes$clinical$cancer_other_m),
               event_date:=fifelse(
                 plogis(-1.5 + 0.25*deprivation_person) > runif(.N),
                 as.Date(event_date + round(abs(rnorm(.N, 0, 60)))), event_date)]

### Change some cancer dates (often later, particularly for higher deprivation)
clinical_table[clinical_code %in% c(codes$clinical$cancer_lung,
                                    codes$clinical$cancer_bowel,
                                    codes$clinical$cancer_gastric),
               event_date:=fifelse(
                 plogis(-1.5 + 0.25*deprivation_person) > runif(.N),
                 as.Date(event_date + round(abs(rnorm(.N, 0, 150)))), event_date)]


## Mark events (dementia and cancers) that happen post-transfer or death
clinical_table[, diag_rm := diag_rm |
  !((event_date <= pmin(transfer_date, deathdate, na.rm = TRUE) |
       (is.na(transfer_date) & is.na(deathdate))))] 

# Identify whether events have other clinical codes for same consultation
clinical_table[, n_codes_per_consult := .N, by = consultation_id]
clinical_table[, n_rm_codes_per_consult := .N, by = c("consultation_id", "diag_rm")]
consult_rm <- clinical_table[diag_rm==TRUE & n_codes_per_consult==n_rm_codes_per_consult,
                               .(patient_id, consultation_id, event_date, suppl_info_id)]

# Identify whether events with no other clinical codes for same consultation
# have prescriptions for that consultation; if not - delete consultation
consult_rm <- consult_rm[!prescription_table, 
                           on = .(patient_id, consultation_id)]
consult_rm<- consult_rm[,.(consultation_id)]

# Identify supplementary information to be removed
suppl_rm <- clinical_table[diag_rm==TRUE & suppl_info_id!=0]
suppl_rm<- suppl_rm[,.(suppl_info_id)]


# Remove: marked clinical events, consultations purely for those evnets 
# and supplementary information from those events
if(nrow(consult_rm)>=1) {
  consultation_table<-consultation_table[!consult_rm,
             on = .(consultation_id)]
}
if(nrow(suppl_rm)>=1) {
  supplementary_information_table<-supplementary_information_table[!suppl_rm,
                                         on = .(suppl_info_id)]
}
clinical_table<-clinical_table[diag_rm==FALSE]



# Tidy
clinical_table<-clinical_table[,.(patient_id, event_date, consultation_id, 
                                  clinical_code, entity, suppl_info_id)]
setorder(clinical_table, event_date)
rm(consult_counts, consult_rm, suppl_rm, prescription_consults,
   prescription_rows_before_join)






#################
#  Some checks  #
#################

# Check every patient ID in consultation/IMD/deathdate/clinical 
# is in the patient file
text <- "Patients in XXX but not patient_table: "

for (d in c("consultation_table", "imd_table",
            "death_linked_table", "clinical_table")) {

  DT <- get(d)
  problem_ids <- DT[!patient_id %chin% patient_table$patient_id,
    .(patient_id)][, unique(patient_id)]
    print(paste(gsub("XXX", d, text), length(problem_ids)))
}



# Check every clinical obs matches to a consultation 
text<- "Clinical observations with non-existent consultations" 
problem_clinical_obs <- clinical_table[ !consultation_table,
  on = "consultation_id",  .(consultation_id)]
print(paste(text, nrow(problem_clinical_obs)))


# Check consultations which don't map to a clinical observation
text<- "Consultations without a clinical code" 
unmatched_consult <- consultation_table[!clinical_table,
  on = "consultation_id"]
print(paste(text, nrow(unmatched_consult)))



##########################
#  Save tables to list   #
##########################

tables <- list()
tables[["patient"]] <- patient_table
tables[["imd"]] <- imd_table
tables[["death_linked"]] <- death_linked_table
tables[["consultation"]] <- consultation_table
tables[["clinical"]] <- clinical_table
tables[["prescription"]] <- prescription_table
tables[["supplementary"]] <- supplementary_information_table




##########################
#  Remove unneeded data  #
##########################

rm(patient_table, imd_table, death_linked_table, clinical_table,
   supplementary_information_table, prescription_table)







