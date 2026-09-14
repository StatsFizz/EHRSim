################################################################################
#
#   Script:       P9_Simulate_consultations.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R object: patient comorbid_dt clinicalmeasure medication
#                  frailty gp drug_groups
#
#   Data created:	R object: consultation
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script creates simulated consultations 
#					      
#
################################################################################


setDT(patient)
setDT(comorbid_dt)


########################################
#  Extract data for chronic disease    #
########################################

comorbid_onset_vars <- c("patient_id", 
                  "diabetes_onset_date", "mi_event_first_date", 
                  "stroke_event_first_date", 
                  "chd_onset_date", "heartfail_onset_date",
                  "other_athero_onset_date", "pvd_onset_date",
                  "pancreatitis_event_first_date", "fibrosis_onset_date",
                  "copd_onset_date", "dementia_onset_date", 
                  "barretts_onset_date",
                  "cancer_gastric_onset_date",
                  "cancer_lung_onset_date",
                  "cancer_breast_onset_date",
                  "cancer_prostate_onset_date",
                  "cancer_bowel_onset_date",
                  "cancer_oth_onset_date", 
                  "ckd3a_date", "ckd3b_date", 
                  "ckd4_date", "ckd5_date", 
                  "ckd_first_date")


comorbid_event_vars <- c("patient_id", "ht_event_date",  
                   "mi_event_date",  "stroke_event_date",
                   "hpylori_event_date", "gerd_event_date",  
                   "peptic_ulcer_event_date", "upper_gi_bleed_event_date",
                   "oesophagitis_event_date", 
                   "endoscopy_date", "ckd3a_date", "ckd3b_date", 
                   "ckd4_date", "ckd5_date", 
                   "ckd_first_date", "mi_event_first_date",
                   "stroke_event_first_date")

# Pick up dates from patient file and comorbidity dates
consult_onset_chronic_all <- patient[, .(patient_id, reg_date, enddate)
                                     ][comorbid_dt[, ..comorbid_onset_vars],
                            on = "patient_id"][, unique(.SD)]

consult_event_chronic_all <- patient[, .(patient_id, reg_date, enddate)
][comorbid_dt[, ..comorbid_event_vars],
  on = "patient_id"][, unique(.SD)]



#########################################
#  Consultations for chronic disease    #
#########################################


### Onset of chronic disease

consult_onset_chronic <- copy(consult_onset_chronic_all)

# CKD - only keep first and mark which type (for later clinical coding)
consult_onset_chronic[ckd3a_date != ckd_first_date,ckd3a_date := NA]
consult_onset_chronic[ckd3b_date != ckd_first_date,ckd3b_date := NA]
consult_onset_chronic[ckd4_date != ckd_first_date,ckd4_date := NA]
consult_onset_chronic[ckd5_date != ckd_first_date,ckd5_date := NA]
consult_onset_chronic[, c("ckd_first_date") := NULL]

# Put into long format
consult_onset_chronic <- melt(consult_onset_chronic,
  id.vars = c("patient_id", "reg_date", "enddate"),
  variable.name = "trigger_event",
  value.name = "date")

# Remove missing dates (non-existent events)
consult_onset_chronic<-consult_onset_chronic[!is.na(date),]

# Only keep consultations since before end date
# Mark consultations pre-registration as being via data transfer
consult_onset_chronic <- consult_onset_chronic[date<=enddate,]
consult_onset_chronic[, consultation_type := 
          fifelse(date>=reg_date, "disease_onset", "data_transfer")]
consult_onset_chronic <- unique(consult_onset_chronic)



### Initial follow-up  
  
# Add follow-up visits (around 3 months later)
consult_onset_chronic[  ,
        followup_date := date + round(rnorm(.N, mean = 3 * 30.5, sd = 7))]
consult_onset_chronic <- melt(
  consult_onset_chronic,
  measure.vars = c("date", "followup_date"),
  value.name = "date",
  variable.name = "var1",
  id.vars = c("patient_id", "reg_date", "enddate", "consultation_type", "trigger_event")
)
consult_onset_chronic[var1 == "followup_date", consultation_type := "fup_disease_onset"]

# Only keep follow-up consultations since current GP registration and before end date
consult_onset_chronic <- consult_onset_chronic[
  date >= reg_date | consultation_type%in% c("disease_onset", "data_transfer")
][date <= enddate]
consult_onset_chronic[, var1 := NULL]



### Events pre-1990 

# Mark as transferred from other system (initial event)
consult_onset_chronic<-consult_onset_chronic[
  date < as.Date("1990-01-01") & consultation_type == "disease_onset",
  consultation_type := "data_transfer"]

consult_onset_chronic <- consult_onset_chronic[
  date >= as.Date("1990-01-01") | consultation_type == "data_transfer"]



### Annual review

# Obtain the first date of onset of a chronic disease per person
consult_chronic_annreview <- consult_onset_chronic[
  consultation_type %in% c("disease_onset", "data_transfer")
][order(patient_id, date), .SD[1], by = patient_id][
  , consultation_type := NULL]

# Estimate number of annual visits required (assuming all during GP
# registration and all post 1990)
consult_chronic_annreview[  ,  num_an_visit :=
  as.numeric(round((enddate - pmax(reg_date, as.Date("1990-01-01"), date)) / 365.25))]

# Expand to one row per annual follow-up
consult_chronic_annreview<-consult_chronic_annreview[num_an_visit>0]
consult_chronic_annreview <- consult_chronic_annreview[
  rep(seq_len(.N), times = num_an_visit)]

 # Allocate dates of annual reviews
consult_chronic_annreview[ ,
  date :=  pmax(reg_date, as.Date("1990-01-01"), date) + seq_len(.N)*365.25,
  by = patient_id]
consult_chronic_annreview[, date := as.Date(date + rnorm(.N, mean = 0, sd = 7))]

# Remove consultations out of date range and mark remaining as annual reviews
consult_chronic_annreview <- consult_chronic_annreview[
  date <= enddate &  date > reg_date &  date > as.Date("1990-01-01"),
  .(patient_id, date, consultation_type = "chronic_disease_review",
    trigger_event)]
 
# Tidy datasets
consult_onset_chronic <- consult_onset_chronic[,
  .(patient_id, date, consultation_type, trigger_event)
][order(patient_id, date)]

consult_chronic_annreview <- consult_chronic_annreview[order(patient_id, date)]


 



#########################################
#  Consultations for repeated events    #
######################################### 

# First MI, stroke and CKD handled above (as onset)
consult_event_chronic <- copy(consult_event_chronic_all)
consult_event_chronic[stroke_event_date == stroke_event_first_date, stroke_event_date := NA]
consult_event_chronic[mi_event_date == mi_event_first_date, mi_event_date := NA]
consult_event_chronic[ckd3a_date == ckd_first_date,ckd3a_date := NA]
consult_event_chronic[ckd3b_date == ckd_first_date,ckd3b_date := NA]
consult_event_chronic[ckd4_date  == ckd_first_date,ckd4_date := NA]
consult_event_chronic[ckd5_date  == ckd_first_date,ckd5_date := NA]
consult_event_chronic[, c("stroke_event_first_date", "mi_event_first_date",
    "ckd_first_date") := NULL]

# Put into long format
consult_event_chronic <- melt(consult_event_chronic,
  id.vars = c("patient_id", "reg_date", "enddate"),
  variable.name = "trigger_event",
  value.name = "date")


# Only keep consultations since current GP registration and before end date
consult_event_chronic <- consult_event_chronic[
 !is.na(date) & date <= enddate & date >= reg_date &
   date >= as.Date("1990-01-01")]
consult_event_chronic <- unique(consult_event_chronic)

# Mark as event consultations
consult_event_chronic[,  consultation_type := "event"]

 


### Initial follow-up 

# Add follow-up visits (around 3 months later)
consult_event_chronic[,
  followup_date := as.Date(date + round(rnorm(.N, mean = 3 * 30.5, sd = 7)))]

# Put in long format 
consult_event_chronic <- melt(consult_event_chronic,
  id.vars = c("patient_id", "reg_date", "enddate",
              "trigger_event", "consultation_type"),
  measure.vars = c("date", "followup_date"),
  variable.name = "var1",
  value.name = "date")

# Mark follow-up consultations
consult_event_chronic[var1 == "followup_date", consultation_type := "fup_event"]

consult_event_chronic[, var1 := NULL]

# Tidy data
consult_event_chronic <- consult_event_chronic[date >= reg_date & date <= enddate,
  .(patient_id, date, consultation_type, trigger_event)
][order(patient_id, date)]





#################################
#  Consultations for diabetes   #
#################################

# Keep diabetes onset dates
consult_diab <- unique(consult_onset_chronic_all[!is.na(diabetes_onset_date), 
    .(patient_id, reg_date, enddate, date0 = diabetes_onset_date)])

# Only proceed if there are diabetes patients 
if (nrow(consult_diab) > 0) {
  
  # Create two follow-up timepoints (3 and 6 years later) 
  consult_diab[, date1 :=
                 as.Date(date0 + 3 * 365.25 + round(runif(.N, 0, 2) * 365.25))]
  consult_diab[, date2 :=
                 as.Date(date1 + 3 * 365.25 + round(runif(.N, 0, 2) * 365.25))]
  
  # Pivot longer (date0, date1, date2; diagnosis, 3-year and 6-year points)
  consult_diab <- melt(
    consult_diab,
    id.vars = c("patient_id", "reg_date", "enddate"),
    measure.vars = patterns("^date"),
    variable.name = "var1",
    value.name = "date"
  )
  
  # Obtain start and enddates of periods (diag-3-year, 3-year-6-year, 6-year-end)
  setorder(consult_diab, patient_id, date)
  consult_diab[, nextdate := shift(date, type = "lead"), by = patient_id]
  consult_diab <- consult_diab[date <= enddate]
  consult_diab[, nextdate := fifelse(seq_len(.N) == .N, enddate, nextdate), 
               by = patient_id]
  
  # Calculate number of appointments during each period
  consult_diab[, n_appoint :=
      fifelse(var1 == "date0", ceiling(as.numeric((nextdate - date) / 90)),
      fifelse(var1 == "date1", ceiling(as.numeric((nextdate - date) / 180)),
      fifelse(var1 == "date2", ceiling(as.numeric((nextdate - date) / 550)), 0)))]
  
  # Keep periods with at least one appointment
  consult_diab <- consult_diab[n_appoint > 0]
  
  # Create one row per appointment
  consult_diab<-consult_diab[n_appoint>0]
  consult_diab <- consult_diab[rep(seq_len(.N), times = n_appoint)]
  
  # Appointment dates
  consult_diab[, date_apt :=date + round(seq_len(.N) *
                     fifelse(var1 == "date0", 90,
                     fifelse(var1 == "date1", 180,
                     fifelse(var1 == "date2", 550, 0)))
                   + rnorm(.N, mean = 0, sd = 10)), by = .(patient_id, var1)]
  
  # Keep appointments between registration and exit only
  consult_diab <- consult_diab[date_apt < enddate & date_apt >= reg_date]
  
  # Tidy dataset
  consult_diab[, `:=`(date = date_apt,
                      consultation_type = "diabetes",
                      trigger_event = "diabetes")]
  consult_diab[, c("reg_date", "enddate", "var1",
                  "date_apt", "nextdate", "n_appoint") := NULL]
}

# Final output, taking into account possibility of no diabetes patients
if (nrow(consult_diab) > 0) {
  setorder(consult_diab, patient_id, date)
  consult_diab <- consult_diab[
    , .(patient_id, date, consultation_type, trigger_event)]
} else {
  consult_diab <- data.table(
    patient_id = character(),
    date = as.Date(character()),
    consultation_type = character(),
    trigger_event = character()
  )
}
  

#############################
#  Consultations for COPD   #
#############################

# These appointments are in addition to those for receiving any systematic steroids

# Keep data only on COPD consultations
consult_copd <- unique(consult_onset_chronic_all[!is.na(copd_onset_date),  
                            .(patient_id, reg_date, enddate, copd_onset_date)])

# Obtain rates of systemic steroid prescriptions for COPD patients
# as a measure of "severity" and need to see patient
ssteroid_copd_presc<-medication[drug=="systemic_steroid_copd"][,
                          .(patient_id, date)]

if (nrow(ssteroid_copd_presc)>0) {
  # Add COPD onset date, registration date and end date
  copd<-unique(comorbid_dt[,c("patient_id", "copd_onset_date")])[!is.na(copd_onset_date)]
  ssteroid_copd_presc[patient[, .(patient_id, reg_date, enddate)],
    `:=`(reg_date = i.reg_date,  enddate  = i.enddate), on="patient_id"]
  ssteroid_copd_presc[copd, copd_onset_date:=i.copd_onset_date, on="patient_id"]

  # Determine start of time "at risk" for COPD prescription
  ssteroid_copd_presc<-ssteroid_copd_presc[copd_onset_date<enddate]
  ssteroid_copd_presc[,copd_onset_date:=fifelse(copd_onset_date<reg_date, reg_date, copd_onset_date)]
  rm(copd)
  
  # Estimate prescription counts and rates 
  ssteroid_copd_presc[, npresc := .N, by = patient_id][,date:=NULL]
  ssteroid_copd_presc<-unique(ssteroid_copd_presc)  
  ssteroid_copd_presc[, rate_presc := 365.25*npresc/ (as.numeric(enddate - copd_onset_date))]
  q<-quantile(ssteroid_copd_presc$rate_presc, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
  q <- unique(q)
  if (length(q) > 1) {
    ssteroid_copd_presc[, rate_gp:= cut(rate_presc, breaks = q,
                                include.lowest = TRUE, labels=FALSE)]
  } else {
    ssteroid_copd_presc[, rate_gp := 1L]
  }
  ssteroid_copd_presc[,rate_gp:=fifelse(npresc==1, 1, rate_gp)]
  ssteroid_copd_presc<-ssteroid_copd_presc[,.(patient_id, rate_gp)]
} else {
  ssteroid_copd_presc <- data.table(patient_id = character(), rate_gp = integer())
}


# Only proceed if there are COPD patients 
if (nrow(consult_copd) > 0) {
  
  # Merge with grouped rate of steroid prescriptions (for COPD)
  consult_copd[ssteroid_copd_presc,  rate_gp := rate_gp,on = "patient_id" ]
  consult_copd[,rate_gp:=fifelse(is.na(rate_gp), 0, rate_gp)]

  consult_copd[, rate_appoint :=  fifelse(rate_gp==0, 0.75,
                                  fifelse(rate_gp==1, 1, 3))]
  
  # Calculate number of appointments based on prescription rates
  consult_copd[, n_appoint :=
          ceiling(rate_appoint*as.numeric((enddate - copd_onset_date)/365.25))]
}

# Draw appointment dates
if (nrow(consult_copd) > 0) {
  
  # Expand to one row per appointment
  consult_copd <- consult_copd[n_appoint>0]
  consult_copd <- consult_copd[rep(seq_len(.N), times = n_appoint)]
  
  # Draw date of appointment
  consult_copd[, date := as.Date(copd_onset_date +  
    round(seq_len(.N) * (enddate - copd_onset_date) / n_appoint + rnorm(.N, mean = 0, sd = 4))),
    by = patient_id]
  
  # Keep only appointments within allowed date range
  consult_copd <- consult_copd[date > reg_date & 
                    date <= enddate & date > as.Date("1990-01-01")]
  
  # Tidy data
  consult_copd[, `:=`(consultation_type = "copd",trigger_event = "copd" )]
}

# Final output, taking into account possibility of no COPD patients
if (nrow(consult_copd) > 0) {
  setorder(consult_copd, patient_id, date)
  consult_copd <- consult_copd[
    , .(patient_id, date, consultation_type, trigger_event)
  ]
} else {
  consult_copd <- data.table(
    patient_id = character(),
    date = as.Date(character()),
    consultation_type = character(),
    trigger_event = character()
  )
}
rm(ssteroid_copd_presc)



###################
#  Health checks  #
###################

#  Adults 40+ have checks approx every 5 years (until superseded
#        by annual chronic disease review)


# Date of first annual chronic disease review per patient
firstannual <- consult_chronic_annreview[order(patient_id, date),
  .SD[1],  by = patient_id]
setnames(firstannual, "date", "date_annreview")

# Date of first 40+ health check 
consult_health_check <- patient[, .(patient_id, dob, reg_date, enddate)]
consult_health_check[, date40 := as.Date(dob + 40 * 365.25),by = patient_id]

# Merge in first annual review date
consult_health_check <- merge(
  consult_health_check,
  firstannual[, .(patient_id, date_annreview)],
  by = "patient_id",
  all.x = TRUE
)
rm(firstannual)

# Number of 5-year checks before annual review starts
consult_health_check[, health_check_end := fcoalesce(date_annreview, enddate)]
consult_health_check[, n_checks := pmax(0,
    round(as.numeric(health_check_end - date40) / (5 * 365.25)))]
consult_health_check <- consult_health_check[n_checks > 0]

# Expand to one row per appointment
consult_health_check<-consult_health_check[n_checks>0]
consult_health_check <- consult_health_check[rep(seq_len(.N), times = n_checks)]

# Allocate appointment dates
consult_health_check[, date := as.Date(date40 +
      round((seq_len(.N) - 1) * as.numeric(health_check_end - date40) / n_checks +
          rnorm(.N, mean = 0, sd = 4))), by = patient_id]

# Apply date filters
consult_health_check <- consult_health_check[
    date > reg_date & date <= enddate & date > as.Date("1990-01-01")]

# Label consultation
consult_health_check[, `:=`(consultation_type = "health_check",
                            trigger_event = "health_check")]

# Randomly remove some appointments
consult_health_check[, u := runif(.N)]
consult_health_check <- consult_health_check[u < 0.9]

# Tidy output
consult_health_check <- consult_health_check[order(patient_id, date),
  .(patient_id, date, consultation_type, trigger_event)]





###########################
#  Health concern checks  #
###########################

### Weight management and referral

# Pick up BMI measurements during registration period
consult_bmi <- merge(
  patient[, .(patient_id, reg_date, enddate)],
  clinicalmeasure[, .(patient_id, datein, dateout, bmi)],
  by = "patient_id",
  all.x = TRUE
)

# Keep measures during registration period and adjust dates if needed
consult_bmi <- consult_bmi[ dateout >= reg_date & datein <= enddate]
consult_bmi[, `:=`(datein  = pmax(datein, reg_date),
                   dateout = pmin(dateout, enddate))]
consult_bmi <- consult_bmi[datein < dateout]

# Categorise BMI 
consult_bmi[, bmicat := fifelse(bmi < 18.5, "Underweight",
                        fifelse(bmi < 25, "Normal",
                        fifelse(bmi < 30, "Overweight",
                        fifelse(bmi < 35, "Obese I",
                        fifelse(bmi < 40, "Obese II",
                        fifelse(bmi >= 40, "Obese III", NA_character_))))))]

# Appointment rate weights by BMI category
consult_bmi[, rateweight_appt := fcase(
  bmicat == "Overweight", 1/5,
  bmicat == "Obese I",    2/5,
  bmicat == "Obese II",   3/5,
  bmicat == "Obese III",  4/5,
  default = 0)]

# Generate "willingness" to engage with weight management (at person level)
setorder(consult_bmi, patient_id, datein)
consult_bmi[, u := runif(.N)]
consult_bmi[, `:=`(unwilling = as.integer(u < 0.4),
      completelyunwilling = as.integer(u > 0.4 & u < 0.7)), by = patient_id]
consult_bmi[, `:=`(
  unwilling = fifelse(seq_len(.N) == 1, unwilling, 0),
  completelyunwilling = fifelse(seq_len(.N) == 1, completelyunwilling, 0)
), by = patient_id]
consult_bmi[, `:=`(
  unwilling = cummax(unwilling),
  completelyunwilling = cummax(completelyunwilling)
), by = patient_id]

# Adjust appointment rates based on willingness
consult_bmi[completelyunwilling == 1, rateweight_appt := 0]
consult_bmi[unwilling == 1, rateweight_appt := rateweight_appt/3]

# Generate number of appointments
consult_bmi[, n_appoint :=
   rpois(.N, rateweight_appt * as.numeric(dateout - datein) / 365.25)]

# Mark referrals after a certain number of weight-related appointments
consult_bmi[, cum_appoint := cumsum(n_appoint), by = patient_id]
consult_bmi[, lag_visit := shift(cum_appoint), by = patient_id]
consult_bmi[, refer := fifelse(
  (cum_appoint > 10 & lag_visit <= 10 |  cum_appoint > 10 & seq_len(.N) == 1) &
  bmicat %chin% c("Obese I", "Obese II", "Obese III"),  1, 0), by = patient_id]
consult_bmi[, c("cum_appoint", "lag_visit") := NULL]

# Expand to one row per appointment
consult_bmi <- consult_bmi[n_appoint>0]
consult_bmi <- consult_bmi[rep(seq_len(.N), times = n_appoint)]

# Generate dates of appointments
consult_bmi[, date := as.Date(datein + runif(.N) * as.numeric(dateout - datein))]

# Keep only appointments within allowed dates
consult_bmi <- consult_bmi[ date > reg_date &
                            date <= enddate &
                            date > as.Date("1990-01-01")]

# Tidy dates
consult_bmi[, `:=`(consultation_type = "weight_management",
                   trigger_event     = "weight_management")]
setorder(consult_bmi, patient_id, date)
consult_bmi <- consult_bmi[, .(patient_id, date, consultation_type, trigger_event)]




###  Smoking management

# Assume people with COPD have smoking cessation advice during routine 
# 	COPD follow-up

## People prior to COPD (or without)

copdonset <- comorbid_dt[ !is.na(copd_onset_date),  .SD[1], 
                by = patient_id, .SDcols = c("copd_onset_date")]

consult_smoke <- merge(
  patient[, .(patient_id, reg_date, enddate)],
  clinicalmeasure[, .(patient_id, datein, dateout, smokecat)],
  by = "patient_id",
  all.x = TRUE
)

# Keep measures during registration period and adjust dates if needed
consult_smoke <- consult_smoke[ dateout >= reg_date & datein <= enddate]
consult_smoke[, `:=`(datein  = pmax(datein, reg_date),
                     dateout = pmin(dateout, enddate))]
consult_smoke <- consult_smoke[datein < dateout]

# Restrict to time periods prior to any COPD diagnosis
consult_smoke[copdonset, on = "patient_id", copd_onset_date := i.copd_onset_date]
consult_smoke[!is.na(copd_onset_date), dateout := pmin(dateout, copd_onset_date)]
consult_smoke<-consult_smoke[datein < copd_onset_date | is.na(copd_onset_date)]
consult_smoke[, copd_onset_date := NULL]
consult_smoke <- consult_smoke[ dateout >= reg_date &  datein <= enddate]
rm(copdonset)

consult_smoke[, `:=`(datein  = pmax(datein, reg_date),
                     dateout = pmin(dateout, enddate))][ datein < dateout]
consult_smoke <- consult_smoke[datein < dateout]

# Appointment rate weights by smoking category
consult_smoke[, ratesmoke_appt := fifelse(smokecat == "Current", 0.5,
                                  fifelse(smokecat == "Ex", 0.2,  0 ))]

# Generate "willingness" to engage with smoking cessation (at person level)
consult_smoke[, u := runif(.N)]
consult_smoke[, `:=`(
    unwilling = as.integer(u < 0.4),
    completelyunwilling = as.integer(u > 0.4 & u < 0.7))]
consult_smoke[, `:=`(
  unwilling = fifelse(seq_len(.N) == 1, unwilling, 0),
  completelyunwilling = fifelse(seq_len(.N) == 1, completelyunwilling, 0)
), by = patient_id]
consult_smoke[, `:=`(
  unwilling = cummax(unwilling),
  completelyunwilling = cummax(completelyunwilling)
), by = patient_id]

# Adjust appointment rates based on willingness
consult_smoke[completelyunwilling == 1, ratesmoke_appt := 0]
consult_smoke[unwilling == 1, ratesmoke_appt := ratesmoke_appt / 3]

# Generate number of appointments
consult_smoke[, n_appoint := 
                rpois(.N,ratesmoke_appt * as.numeric(dateout - datein) / 365.25 )]

# Expand to one row per appointment
consult_smoke<-consult_smoke[n_appoint>0]
consult_smoke <- consult_smoke[rep(seq_len(.N), times = n_appoint)]

# Generate dates of appointments
consult_smoke[, date := as.Date(datein + 
                          runif(.N) * as.numeric(dateout - datein))]

# Keep only appointments within allowed dates
consult_smoke<- consult_smoke[date > reg_date &
                              date <= enddate &
                              date > as.Date("1990-01-01")]

# Tidy data
consult_smoke[, `:=`(consultation_type = "smoke_cessation",
                     trigger_event = "smoke_cessation")]
consult_smoke <-consult_smoke[, .(patient_id, date, consultation_type, trigger_event)]
setorder(consult_smoke, patient_id, date)




###  Alcohol


# Merge patient and clinical measure data
consult_alcohol <- merge(
  patient[, .(patient_id, reg_date, enddate)],
  clinicalmeasure[, .(patient_id, datein, dateout, harmalc, propensity_alc, alccat, alc3cat)],
  by = "patient_id",
  all.x = TRUE)

# Restrict dates to valid registration periods
consult_alcohol <- consult_alcohol[ dateout >= reg_date & datein <= enddate]
consult_alcohol[, `:=`(datein = pmax(datein, reg_date),
                       dateout = pmin(dateout, enddate))]
consult_alcohol<-consult_alcohol[datein < dateout]


### Harmful alcohol users

# Identify first harmful alcohol use per patient
consult_alcohol_harm <- consult_alcohol[order(patient_id, datein)][
  , first_harm_alc := (harmalc == 1 & shift(harmalc, fill=0) != 1) 
  | (harmalc == 1 & seq_len(.N) == 1),  by = patient_id][first_harm_alc == TRUE]

# Generate first harmful alcohol date (random within period)
consult_alcohol_harm[, date_first_harm_alc := 
                as.Date(datein + runif(.N) * as.numeric(dateout - datein))]

# Keep only necessary columns
consult_alcohol_harm <- consult_alcohol_harm[, .(patient_id, reg_date, enddate, date_first_harm_alc)]

# Willingness flags
consult_alcohol_harm[, u:= runif(.N)]
consult_alcohol_harm[, `:=`(unwilling = as.integer(u < 0.5),  
                            completelyunwilling = as.integer(u > 0.5 & u < 0.85))]

# Generate visits (3 per year for 3 years, then annual for 10 years)
n_patients <- nrow(consult_alcohol_harm)
visits_mat <- matrix(NA_real_, nrow = n_patients, ncol = 21)
visits_mat[, 1] <- as.numeric(consult_alcohol_harm$date_first_harm_alc)

for (i in 2:10) {
  visits_mat[, i] <- visits_mat[, i - 1] + round(rnorm(n_patients, mean = 3*30.5, sd = 7), 1)
}
for (i in 11:21) {
  visits_mat[, i] <- visits_mat[, i - 1] + round(rnorm(n_patients, mean = 365.25, sd = 14), 1)
}
rm(n_patients)

consult_alcohol_harm[, paste0("visit", 0:20) := as.data.table(visits_mat)]
consult_alcohol_harm[, c("u", "date_first_harm_alc") := NULL]

# Put visits in long format
consult_alcohol_harm <- melt(
  consult_alcohol_harm,
  id.vars = c("patient_id", "reg_date", "enddate", "unwilling", "completelyunwilling"),
  measure.vars = patterns("visit"),
  variable.name = "var1",
  value.name = "date"
)[date >= reg_date & date <= enddate]
consult_alcohol_harm[,date:=as.Date(date)]

# Apply willingness probabilities
consult_alcohol_harm <- consult_alcohol_harm[completelyunwilling == 0]
consult_alcohol_harm[, u := runif(.N)]
consult_alcohol_harm[, agree := fifelse(unwilling == 1 & u < 0.15, 1,
                                fifelse(unwilling != 1 & u < 0.6, 1, 0))]
consult_alcohol_harm[, u := NULL]
consult_alcohol_harm <- consult_alcohol_harm[agree == 1, .(patient_id, date)]

# Label consultation type
consult_alcohol_harm[, `:=`(consultation_type = "harmful_alcohol_use", 
                            trigger_event = "harmful_alcohol_use")]



### Medium/heavy drinkers not currently labelled as harmful

# Eligibility is interval-specific so later harmful use cannot suppress earlier advice.
consult_alcohol_heavy <- consult_alcohol[
  order(patient_id, datein)
][harmalc == 0 & alccat %chin% c("Medium", "High")]

# Categorize propensity
consult_alcohol_heavy[, rate_alcgen := fifelse(alccat=="High", 0.25, 0.125)]

# Generate number of visits
consult_alcohol_heavy[, nvisit := rpois(.N, rate_alcgen * as.numeric(dateout - datein)/365.25)]
consult_alcohol_heavy <- consult_alcohol_heavy[nvisit > 0]

# Willingness flags
consult_alcohol_heavy[, u := runif(.N)]
consult_alcohol_heavy[, `:=`(unwilling = as.integer(u < 0.5),
                             completelyunwilling = as.integer(u > 0.5 & u < 0.7))]
consult_alcohol_heavy[, u := NULL]
      
# Expand by number of visits
consult_alcohol_heavy<-consult_alcohol_heavy[nvisit>0]
consult_alcohol_heavy <- consult_alcohol_heavy[rep(seq_len(.N), times = nvisit)]

# Generate visit dates
consult_alcohol_heavy[, date := as.Date(datein 
                                  + runif(.N) * as.numeric(dateout - datein))]

# Filter out some visits based on willingness
consult_alcohol_heavy[, u := runif(.N)]
consult_alcohol_heavy[, attend := fifelse(unwilling==1 & u<0.5, 0,
                                  fifelse(completelyunwilling==1 & u<0.95, 0, 1))]
consult_alcohol_heavy <- consult_alcohol_heavy[attend == 1, .(patient_id, date)]
consult_alcohol_heavy[, `:=`(consultation_type = "alcohol_advice", 
                             trigger_event = "alcohol_advice")]

# Combine alcohol visit datasets
consult_alcohol <- rbindlist(list(consult_alcohol_harm, consult_alcohol_heavy))
setorder(consult_alcohol, patient_id, date)
rm(consult_alcohol_harm, consult_alcohol_heavy)






###################
#  Contraception  #
###################


# Start with female patients only
consult_contra <- copy(patient)[sex == "Female"]

# Generate initial dates and pill assignment
consult_contra[, `:=`(
  date48 = as.Date(dob + 48*365.25),
  first_date_pill = as.Date(rbeta(.N, 2, 5)*20*365.25 + dob + 18*365.25))]
consult_contra[, `:=`(
  age_first_pill  = as.numeric(first_date_pill - dob)/365.25,
  u = runif(.N))]

# Assign pill use and calculate last_date_pill
consult_contra[, `:=`(
  pill = fifelse(u < (0.35 + 0.02*(age_first_pill-18)), 0, 1),
  time_stop = rbeta(.N, 2, 5) * as.numeric(date48 - first_date_pill))]
consult_contra[, last_date_pill := as.Date(first_date_pill + time_stop)]

# Keep only those who take the pill
consult_contra <- consult_contra[pill == 1]

# Restrict to relevant observation period
consult_contra[, `:=`(
  first_date_pill = pmax(first_date_pill, as.Date("1990-01-01"), reg_date),
  last_date_pill = pmin(last_date_pill, enddate))]
consult_contra <- consult_contra[first_date_pill <= last_date_pill]

# Store true pill stop date before creating and thinning observed consultation rows
lastpill <- consult_contra[, .(pilldate = max(last_date_pill, na.rm = TRUE)),
                           by = patient_id]

# Generate number of "prescriptions" (assuming approx 6-months supply)
consult_contra[, npresc := ceiling(2*as.numeric(last_date_pill - first_date_pill)/365.25)]

# Expand to one row per prescription
consult_contra<-consult_contra[npresc>0]
consult_contra <- consult_contra[rep(seq_len(.N), times = ceiling(npresc))]

# Generate prescription dates
consult_contra[, date := as.Date(first_date_pill 
    + round((seq_len(.N)-1)*6*30.5 + rnorm(.N, mean=0, sd=7))), by = patient_id]

# Randomly drop missed prescriptions
consult_contra <- consult_contra[runif(.N) < 0.85]

# Restrict dates again to valid follow-up
consult_contra <- consult_contra[date <= enddate & 
                                 date >= reg_date & 
                                 date >= as.Date("1990-01-01")]

# Add consultation_type and trigger_event
consult_contra[, `:=`(consultation_type = "contraception",
                      trigger_event = "contraception")]

# Keep only relevant columns and order
consult_contra <- consult_contra[, 
                  .(patient_id, date, consultation_type, trigger_event)]
setorder(consult_contra, patient_id, date)





#################################
#  Hormone replacement therapy  #
#################################

# Keep females only
consult_hrt <- copy(patient)[sex == "Female", .(patient_id, reg_date, enddate, dob)]

# Merge with last pill date
consult_hrt <- merge(consult_hrt, lastpill, all.x = TRUE, by = "patient_id")

# Generate HRT start/end dates
consult_hrt[, `:=`(
  date40 = as.Date(dob + 40*365.25),
  date51 = as.Date(dob + 51*365.25))]
consult_hrt[, first_poss_start := fifelse(is.na(pilldate), date40, pmax(date40, pilldate+30.5*6))]
consult_hrt[, first_date_hrt := as.Date(first_poss_start + rbeta(.N, 5, 5)*15*365.25)]
consult_hrt[, age_first_date_hrt := as.numeric(first_date_hrt - dob)/365.25]

# Duration of HRT use (if start pre-45, continue to 51, otherwise draw enddate)
consult_hrt[, duration_days := fifelse(age_first_date_hrt < 45,
  as.numeric(date51 - first_date_hrt), rbeta(.N, 2, 5) * 7*365.25)]
consult_hrt[, last_date_hrt := as.Date(first_date_hrt + duration_days)]

# Only 20% of women take HRT, (5% of people < 45)
consult_hrt[, hrt := fifelse(age_first_date_hrt<45, runif(.N) < 0.05, runif(.N) < 0.2)]
consult_hrt <- consult_hrt[hrt == TRUE]

# Ensure dates are within observation window
consult_hrt[, `:=`(
  first_date_hrt = pmax(first_date_hrt, as.Date("1990-01-01"), reg_date),
  last_date_hrt = pmin(last_date_hrt, enddate))]
consult_hrt <- consult_hrt[last_date_hrt >= first_date_hrt]

# Generate number of prescriptions
consult_hrt[, npresc := ceiling(4*as.numeric(last_date_hrt - first_date_hrt)/365.25)]

# Expand to one row per prescription
consult_hrt<-consult_hrt[npresc>0]
consult_hrt <- consult_hrt[rep(seq_len(.N), times = npresc)]

# Generate prescription dates
consult_hrt[, date := as.Date(first_date_hrt 
    + round((seq_len(.N)-1)*3*30.5 + rnorm(.N, mean=0, sd=7))), by = patient_id]

# Randomly drop missed prescriptions
consult_hrt[, u := runif(.N)]
consult_hrt <- consult_hrt[u < 0.85]

# Restrict dates again
consult_hrt <- consult_hrt[
  date <= enddate & date >= reg_date & date >= as.Date("1990-01-01")]

# Add consultation_type and trigger_event
consult_hrt[, `:=`(consultation_type = "hrt",
                      trigger_event = "hrt")]

# Keep only relevant columns and order
consult_hrt <- consult_hrt[, .(patient_id, date, consultation_type, trigger_event)]
setorder(consult_hrt, patient_id, date)







#################
#  Medications  #
#################

# Keep a private key linking each medication row to its generated consultation.
medication[, prescription_id_internal := seq_len(.N)]
consult_prescription<-copy(medication)

# Assign consultation type
consult_prescription[, consultation_type := fifelse(
  drug %chin% c("aspirin", "clopidogrel", "OAC"), "repeat_issue",
  fifelse(drug %chin% c("NSAID", drug_groups[["ppi"]], drug_groups[["h2ra"]]), "surgery",
  fifelse(drug %chin% c("systemic_steroid_other", "systemic_steroid_copd","inhaled_steroid"), 
          "medicine_management",  NA_character_)))]

# Create trigger_event (same as consultation_type)
consult_prescription[, trigger_event := consultation_type]

# Keep only relevant columns and order
consult_prescription <- consult_prescription[, .(
  patient_id, date, consultation_type, trigger_event,
  prescription_id_internal
)]
setorder(consult_prescription, patient_id, date)



#########################
#  Other consultations  #
#########################

# Merge patient data with frailty information
consult_oth <- merge(patient, frailty, by = "patient_id", all.x = TRUE)

# Only consider other consultations happening post-1990 
consult_oth <- consult_oth[dateout > as.Date("1990-01-01")]
consult_oth[datein < as.Date("1990-01-01"), datein := as.Date("1990-01-01")]

# Assign consult_rate depending on frailty
setorder(consult_oth, patient_id, datein)
consult_oth[, consult_rate :=
              fifelse(seq_len(.N) == 1, runif(.N, 1, 4)/3, 0), by = patient_id]

consult_oth[, consult_rate := max(consult_rate), by = patient_id]
consult_oth[, consult_rate := frailty * consult_rate]
consult_oth[runif(.N) < 0.3 & frailty == 4, consult_rate := consult_rate * runif(.N, 1, 1.5)]

# Generate number of appointments
consult_oth[, n_appoint := rpois(.N, consult_rate * as.numeric(dateout - datein)/365.25)]
consult_oth[n_appoint > 15, n_appoint := 15 + round(rnorm(.N, 0, 2))]

# Expand to one row per appointment
consult_oth<-consult_oth[n_appoint>0]
consult_oth <- consult_oth[rep(seq_len(.N), times = ceiling(n_appoint))]

# Generate dates of appointments
consult_oth[, date := as.Date(datein + runif(.N) * as.numeric(dateout - datein)), 
            by = .(patient_id, datein)]

# Filter dates within correct time period
consult_oth <- consult_oth[date <= enddate & 
                             date >= reg_date & 
                             date >= as.Date("1990-01-01")]


# Assign consultation type according to a multinomial draw from the following:
ctypes <- c("surgery","repeat_issue","administration","other",
            "results", "third_party_consult","clinic", "outpatients_letter",
            "phone_call_from_patient", "phone_call_to_patient",
            "medicine_management","mail_to_patient", "referral_letter")
cprobs <- c(0, 0.26, 0.44, 0.60, 0.72, 0.83, 0.89, 0.92, 
            0.95, 0.96, 0.97, 0.98, 0.99, 1)
consult_oth[, u := runif(.N)]
consult_oth[, consultation_type := ctypes[findInterval(u, cprobs, rightmost.closed = TRUE)]]
consult_oth[, trigger_event := NA_character_]
consult_oth[,u:=NULL]

# Tidy dataset
consult_oth <- consult_oth[, .(patient_id, date, consultation_type, trigger_event)]
setorder(consult_oth, patient_id, date)






#####################################
#  Compile different consultations  #
#####################################


# Append all consultation datasets
datasets <- list(
  consult_onset_chronic, consult_chronic_annreview, 
  consult_event_chronic, 
  consult_health_check, consult_bmi, consult_smoke, consult_alcohol, 
  consult_copd, consult_diab, 
  consult_contra, consult_hrt, 
  consult_prescription, consult_oth
)
datasets <- lapply(datasets, function(dataset) {
  dataset <- copy(as.data.table(dataset))
  if (!"prescription_id_internal" %chin% names(dataset)) {
    dataset[, prescription_id_internal := NA_integer_]
  }
  dataset
})
consultation <- rbindlist(datasets, use.names=TRUE, fill=FALSE)


## Create a unique consultation ID

n <- nrow(consultation)
id <- sort(round(runif(1, 1, 5000)) + sample(1:(5*n), n))

# Standardize ID length: pracID + leading zeros + consID
desired_length <- 14
diff <- desired_length - nchar(max(id)) - nchar(as.character(gp$prac_id))
extra_zeros <- max(diff, 1)
pad_width <- nchar(as.character(max(id))) + extra_zeros
std_id <- str_pad(id, width = pad_width, side = "left", pad = "0")

# Sort consultations by date and
setorder(consultation, date)
consultation[, consultation_id := paste0(gp$prac_id, std_id)]


# Order and tidy columns
setorder(consultation, patient_id, date)
consultation <- consultation[, .(
  consultation_id,
  patient_id,
  date,
  consultation_type,
  trigger_event,
  prescription_id_internal
)]

# Check uniqueness of consultation ID
if(anyDuplicated(consultation$consultation_id) > 0) {
  warning("Some consultation IDs are duplicated!")
}



# Remove unneeded datasets
rm(list = c(
  "consult_chronic_annreview", "consult_onset_chronic", "consult_event_chronic",
  "consult_health_check", "consult_bmi", "consult_smoke", "consult_alcohol",
  "consult_copd", "consult_diab", "consult_contra", "consult_hrt", 
  "consult_prescription", "consult_oth", "comorbid_event_vars",
  "comorbid_onset_vars", "consult_event_chronic_all",
  "consult_onset_chronic_all"))

