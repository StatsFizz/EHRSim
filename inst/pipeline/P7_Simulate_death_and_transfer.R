################################################################################
#
#   Script:       P7_Simulate_death_and_transfer.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R object: patient, clinicalmeasure, frailty, comorbid_dt,
#                           medication,      
#
#   Data created:	R object: deathdate and transfer
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script simulates a dataset resembling real EHR data,
#					      containing 	containing information about the patient's 
#					      date and cause of death, and censoring due to transfering
#               out of the general practice.  
#
#               Simplifying assumption: that changes happen at start of 
#               time-period 
# 
################################################################################
#
#   Packages needed: data.table
#
################################################################################




##################################
#  Collate required information  #
##################################

# Gather information on:
#  Time-fixed contributors to death hazard (sex, deprivation, etc)
#  Time-varying clinical contributions (clinical measures, age)
#  Comorbidity onset and event dates
#  Prescription start and stop dates
#  Dates of entry and exit


###  Collate time-fixed hazard contributions  ###

# Note: sex only taken into account via different age distributions
time_fixed_var <- c("patient_id",  "dob", "ethnicity", "deprivation_person") 
time_fixed <- patient[, .SD, .SDcols = time_fixed_var]
setnames(time_fixed, old="dob", new="date")

# Create keys for categorical variables to allow efficient look-up of log-HRs
time_fixed[, `:=`(key_deprivation   = paste0("deprivation", deprivation_person),
                  key_eth   = paste0("eth", tolower(ethnicity)),
                  year2025   = (2025-year(date)))
][, c("deprivation_person", "ethnicity") := NULL]




### Collate time-varying clinical measures that modify hazard   ###


# Clinical measures: BMI, smoking, alcohol and age
time_updating_clin_var <- c("patient_id", "sex", "datein", "agein", 
                            "smokecat", "harmalc", "alccat", "bmi") 
time_updating_clin <- clinicalmeasure[, ..time_updating_clin_var]
setnames(time_updating_clin, old="datein", new="date")

# Centre BMI (around 25)
time_updating_clin[, bmi:=bmi-25]

# Create female- and male-specific ages, to have different death distributions
time_updating_clin[,mage:=fifelse(sex=="Male",   agein, 0)]
time_updating_clin[,fage:=fifelse(sex=="Female", agein, 0)]

# Clinical measures: add in frailty information 
frailty_dt <- frailty[, .(patient_id, datein, agein, frailty)]
setnames(frailty_dt, old="datein", new="date")
time_updating_clin <- frailty_dt[time_updating_clin, on = .(patient_id, agein), roll = Inf]
time_updating_clin[,c("date","agein"):=NULL]
rm(frailty_dt)

setnames(time_updating_clin, old="i.date", new="date")
setorder(time_updating_clin, patient_id, date)

# Create keys for categorical variables to allow efficient look-up of log-HRs
time_updating_clin[, `:=`(key_alc     = paste0("alccat", tolower(alccat)),
                          key_smoke   = paste0("smokecat", tolower(smokecat)),
                          key_frail   = paste0("frailty", frailty),
                          key_mage    = paste0("mage", mage),
                          key_fage    = paste0("fage", fage)
)][, c("frailty", "alccat", "smokecat", "mage", "fage") := NULL]




### Collate time-varying comorbidity events that modify hazard   ###

# Put dates of all comorbidities into long format
comorbidity_events <- melt(comorbid_dt,
  id.vars = "patient_id",              
  measure.vars = patterns("_date$"),  
  variable.name = "event",        
  value.name = "date"     
)
comorbidity_events <- comorbidity_events[!is.na(date)]
comorbidity_events <- unique(comorbidity_events, by = c("patient_id", "date", "event"))
comorbidity_events[, event := sub("_date$", "", event)]

# CKD death hazards are stage-specific; ckd_first is a helper date in comorbid_dt.
comorbidity_events <- comorbidity_events[event != "ckd_first"]


## Add rows for MI and stroke (to allow time-changing hazard increases)
new_rows1 <- copy(comorbidity_events[event %in% c("mi_event", "stroke_event"),])
new_rows2 <- copy(comorbidity_events[event %in% c("mi_event", "stroke_event"),])
new_rows1[, `:=` (date = date+7, event = paste0(event, "_b"))]
new_rows2[, `:=` (date = date+30, event = paste0(event, "_c"))]

comorbidity_events <- rbindlist(list(comorbidity_events, new_rows1, new_rows2))
rm(new_rows1, new_rows2)






### Collate prescription start and stop dates that modify hazard   ###


# Put drug names (PPI types, etc) into a look-up table
drug_map <- rbindlist(lapply(names(drug_groups), function(g)
    data.table(drug = drug_groups[[g]], drug_group = g)))

# Label each row of the medication data using the look-up table
setDT(medication)
unmatched_drugs <- medication[!drug_map, on = "drug", unique(drug)]
if (length(unmatched_drugs) > 0) {
  stop("Medication drugs missing from drug_groups: ",
       paste(unmatched_drugs, collapse = ", "), call. = FALSE)
}
med <- medication[drug_map, on = "drug", nomatch = 0L,
                  .(patient_id, drug_group, date, days)]

# Mark distinct prescription episodes (allowing for some gaps within episodes)
setorder(med, patient_id, drug_group, date)
mult_factor <- 0.25   # or a named vector per drug_group if needed
med[, date_end := date + days]
med[, grace_end := date_end + mult_factor*days]
med[, prev_grace_end := shift(cummax(as.numeric(grace_end))),
    by = .(patient_id, drug_group)]
med[, new_episode := is.na(prev_grace_end) |
      as.numeric(date) > prev_grace_end]
med[, episode := cumsum(new_episode), by = .(patient_id, drug_group)]
med[, prev_grace_end := NULL]

# Mark start and end dates of episodes (within person and drug class)
# at episode level
episodes <- med[  , .(start_episode = first(date),
                      end_episode   = max(grace_end)),
                  by = .(patient_id, drug_group, episode)]

# Identify start and stop dates of episodes
prescription_events <- rbindlist(list(
  episodes[, .(patient_id,
    event = paste0("presc_start_", drug_group),
    date  = start_episode)],
  episodes[, .(patient_id,
    event = paste0("presc_stop_", drug_group),
    date  = end_episode)]))
setorder(prescription_events, patient_id, date)

# Tidy up
rm(med, episodes, drug_map)



####  Dates of entry and exit  ####


## Add start and end dates
regdate_enddate <- patient[,c("patient_id", "reg_date", "enddate_init", "dob")]
regdate_enddate[,date_18:=as.Date(dob+18*365.25)]
regdate_enddate[,datein:=pmax(date_18, reg_date)]
regdate_enddate[,reg_date:=NULL]
setnames(regdate_enddate, old=c("datein", "enddate_init"), new=c("date1", "date2"))
# Put in long format
regdate_enddate <- melt(regdate_enddate,
  id.vars = "patient_id",        
  measure.vars = c("date1", "date2"), 
  variable.name = "event_type",   
  value.name = "date")



####################################
#  Calculate hazard contributions  #
####################################


###  Time-fixed characteristics  ###

# Function to add in the log-HRs for time-fixed hazard contributors
calc_lnhr_fix <- function(coefs, data) {
    coefs[data$key_deprivation] +
    coefs[data$key_eth] +
    coefs["year"]*data$year2025
}

# Add in log-HRs (communicable, non-communicable and injury/external)
time_fixed[, `:=`(lnhr_fix_1 = calc_lnhr_fix(lnhr_noncom_fix, .SD),
                  lnhr_fix_2 = calc_lnhr_fix(lnhr_comm_fix, .SD),
                  lnhr_fix_3 = calc_lnhr_fix(lnhr_ext_fix, .SD))]

# Remove unneeded variables
time_fixed[,c("key_deprivation","key_eth", "year2025"):=NULL]



### Time-varying clinical measures that modify hazard  ###

# Function to add in the log-HRs for time-varying clinical measures
calc_lnhr_clin <- function(coefs, data) {
    coefs[data$key_smoke] +
    coefs[data$key_alc] +
    coefs[data$key_frail] +
    coefs[data$key_mage] +
    coefs[data$key_fage] +
    coefs["bmi"]     * data$bmi +
    coefs["harmalc"] * data$harmalc
}

# Add in log-HRs (communicable, non-communicable and injury/external)
time_updating_clin[, `:=`(  lnhr_clin_1 = calc_lnhr_clin(lnhr_noncom_clin, .SD),
                            lnhr_clin_2 = calc_lnhr_clin(lnhr_comm_clin, .SD),
                            lnhr_clin_3 = calc_lnhr_clin(lnhr_ext_clin, .SD))]

# Remove unneeded variables
time_updating_clin[,c("sex","key_mage","key_fage", "key_frail", "key_alc",
                      "key_smoke", "harmalc", "bmi"):=NULL]




### Time-varying comorbidity events that modify hazard  ###

# Put log-HRs in a data table
vec_to_dt <- function(x, source) {
  data.table(event = names(x),
            lnhr  = unname(x),
            source = source )
}
warn_missing_coefs <- function(events, coefs, label) {
  missing_events <- setdiff(unique(events), names(coefs))
  if (length(missing_events) > 0) {
    warning(label, " missing log-HR coefficients for: ",
            paste(missing_events, collapse = ", "), call. = FALSE)
  }
}
lnhr_comorb <- rbindlist(list(
  vec_to_dt(lnhr_noncom_comorb, "noncom"),
  vec_to_dt(lnhr_comm_comorb,    "comm"),
  vec_to_dt(lnhr_ext_comorb, "ext")))

warn_missing_coefs(comorbidity_events$event, lnhr_noncom_comorb,
                   "Non-communicable comorbidity events")
warn_missing_coefs(comorbidity_events$event, lnhr_comm_comorb,
                   "Communicable comorbidity events")
warn_missing_coefs(comorbidity_events$event, lnhr_ext_comorb,
                   "External/injury comorbidity events")

# Merge log-HRs into the list of comorbidity events
comorbidity_events[lnhr_comorb[source == "noncom"], on = "event",  lnhr_cum_1 := i.lnhr]
comorbidity_events[lnhr_comorb[source == "comm"],   on = "event",  lnhr_cum_2 := i.lnhr]
comorbidity_events[lnhr_comorb[source == "ext"],    on = "event",  lnhr_cum_3 := i.lnhr]
comorbidity_events[,c("event"):=NULL]


### Time-varying prescription events that modify hazard  ###

lnhr_presc <- rbindlist(list( vec_to_dt(lnhr_noncom_presc, "noncom"),
                              vec_to_dt(lnhr_comm_presc,    "comm"),
                              vec_to_dt(lnhr_ext_presc, "ext")))

warn_missing_coefs(prescription_events$event, lnhr_noncom_presc,
                   "Non-communicable prescription events")
warn_missing_coefs(prescription_events$event, lnhr_comm_presc,
                   "Communicable prescription events")
warn_missing_coefs(prescription_events$event, lnhr_ext_presc,
                   "External/injury prescription events")

prescription_events[lnhr_presc[source == "noncom"], on = "event",  lnhr_cum_1 := i.lnhr]
prescription_events[lnhr_presc[source == "comm"],   on = "event",  lnhr_cum_2 := i.lnhr]
prescription_events[lnhr_presc[source == "ext"],    on = "event",  lnhr_cum_3 := i.lnhr]




##################################
#  Collate all hazard modifiers  #
##################################

# Combine all hazard-related data
all_hazard_mod <- rbindlist(list(time_fixed, time_updating_clin, 
                          comorbidity_events, prescription_events), fill=TRUE)
all_hazard_mod <- rbindlist(list(all_hazard_mod, regdate_enddate), fill=TRUE)

# Use last-observation-carried-forward to fill in missing fixed and clinical hazards
idx <- 1:3
fix_hr_vars <- paste0("lnhr_fix_", idx)
clin_hr_vars <- paste0("lnhr_clin_", idx)
add_hr_vars <- c(fix_hr_vars, clin_hr_vars)
setorder(all_hazard_mod, patient_id, date)
all_hazard_mod[, (add_hr_vars) := lapply(.SD, nafill, type = "locf"), 
               by = patient_id, .SDcols = add_hr_vars]

# Replace any remaining NAs with 0 and cumulate comorbidity and prescription effects
cum_hr_vars <- paste0("lnhr_cum_", idx)
zero_vars <- c(add_hr_vars, cum_hr_vars)
all_hazard_mod[, (zero_vars) := lapply(.SD, function(x) replace(x, is.na(x), 0)), 
               .SDcols = zero_vars]
all_hazard_mod[, (cum_hr_vars) := lapply(.SD, cumsum), by = patient_id, .SDcols = cum_hr_vars]

# Add to period-specific ln-HR additions, to obtain total ln-HR
all_hazard_mod[, paste0("lnhr_all_", idx) := mapply(function(x, y, z) get(x) + get(y) + get(z),
                        cum_hr_vars, fix_hr_vars, clin_hr_vars, SIMPLIFY =FALSE)]

# Remove components no longer needed
all_hazard_mod[, c(add_hr_vars, cum_hr_vars) := NULL]



# Remove time pre-registration/age 18 and any time after enddate:
# Indicate entry and exit date
all_hazard_mod[order(patient_id, date),tmp:=cumsum((!is.na(event_type))), by="patient_id"]
# Identify period end dates
all_hazard_mod[order(patient_id, date),date_end:=shift(date, n=-1), by="patient_id"]
#  Keep only time between entry and exit
all_hazard_mod<-all_hazard_mod[tmp==1,]

all_hazard_mod[,c("event_type", "tmp"):=NULL]
setorder(all_hazard_mod, patient_id, date)


# Measure time in years of age
setkey(time_fixed, patient_id)
all_hazard_mod<-all_hazard_mod[time_fixed[, .(patient_id, dob=date)], on = "patient_id"]
all_hazard_mod[,t_start:=(as.numeric(date)-as.numeric(dob))/365.25]
all_hazard_mod[,t_end:=(as.numeric(date_end)-as.numeric(dob))/365.25]
all_hazard_mod[,c("dob"):=NULL]
 

# Now we have a dataset with many rows per patient, from age 18 until their 
# study end, broken into non-overlapping adjacent time intervals with the 
# relevant total log-HR for each cause of death in each interval.



###########################################
#  Piecewise exponential model for death  #
###########################################

# Piecewise exponential, depending on age during time-period 

lnhr_cols <- paste0("lnhr_all_", 1:3)
accum_H_cols <- paste0("accum_H_", 1:3)
H_cols <- paste0("H_", 1:3)
S_cols <- paste0("S_", 1:3)
e_cols <- paste0("e_", 1:3)
first_dth_cols <- paste0("first_dth", 1:3)

# Calculate additional cumulative hazard in each interval
all_hazard_mod[, paste0("accum_H_", 1:3) := Map(function(hr_col) {
  (as.numeric(t_end) - as.numeric(t_start)) * exp(get(hr_col)) 
   }, lnhr_cols)]

# Calculate cumulative hazard and survival over time
setorder(all_hazard_mod, patient_id, t_start)
all_hazard_mod[ , (H_cols) := lapply(.SD, cumsum),by = patient_id,
                .SDcols = accum_H_cols]     
all_hazard_mod[ ,  (S_cols) := lapply(.SD, function(x) exp(-x)),
  .SDcols = H_cols]


# Generate an exponential random variable for each cause of death
all_hazard_mod[, paste0("e_", 1:3) := lapply(1:3, function(x) rexp(.N, 1))]

# Mark first instance where additional cumulative hazard exceeds E for each person
setorder(all_hazard_mod, patient_id, t_start)
all_hazard_mod[, paste0("first_dth", 1:3) := 
                 Map(function(accum_H, e) {
                   as.integer(get(accum_H) > get(e) & seq_len(.N) == which.max(get(accum_H) > get(e)))
                 }, accum_H_cols, e_cols), by=patient_id]

# Generate death age
all_hazard_mod[, paste0("dth_age", 1:3) := 
                 Map(function(first_dth, e, lnhr) {
                   fifelse(get(first_dth)==1, 
                           (get(e)/exp(get(lnhr)) + as.numeric(t_start)), NA)
                 }, first_dth_cols, e_cols, lnhr_cols)]

#  Tidy up
rm(lnhr_cols, e_cols, accum_H_cols, first_dth_cols)




############################
#  Overall cause of death  #
############################

first_death_age <- function(x) {
  if (all(is.na(x))) 10000 else min(x, na.rm = TRUE)
}

# Retain one row per person, with earliest death age per cause
deathdate <- all_hazard_mod[
  , .(dth_age_1 = first_death_age(dth_age1),
      dth_age_2 = first_death_age(dth_age2),
      dth_age_3 = first_death_age(dth_age3)),  by = patient_id]

deathdate[,dth_age:=pmin(dth_age_1, dth_age_2, dth_age_3)]

# Add in date of birth and calculate date of death
deathdate <- deathdate[patient, dob:=i.dob, on = .(patient_id)]
deathdate[,deathdate:=as.Date(dob + dth_age*365.25)]
deathdate<-deathdate[dth_age<10000,]

# Determine cause of death
deathdate[, cod := fcase(
  dth_age_1 <= pmin(dth_age_2, dth_age_3, na.rm = TRUE),  "Non-communicable",
  dth_age_2 <= pmin(dth_age_1, dth_age_3, na.rm = TRUE),  "Communicable",
  dth_age_3 <= pmin(dth_age_1, dth_age_2, na.rm = TRUE),  "Injury/External",
  default= "Alive")]

deathdate[, c("dth_age_1", "dth_age_2", "dth_age_3", "dth_age", "dob") := NULL]
deathdate<-unique(deathdate)
 
# Tidy up
rm(time_fixed, time_updating_clin, first_death_age,
   comorbidity_events, regdate_enddate,
   lnhr_comorb, lnhr_presc)






################################
#  Generate transfer out data  #
################################


transfer <- patient[, .(patient_id, reg_date, enddate_init)]
transfer[deathdate, deathdate := i.deathdate, on = .(patient_id)]

# Simulate transfer out dates
transfer[, timein:= (as.numeric(pmin(enddate_init, datestudylast, na.rm=TRUE) 
                - reg_date)/365.25)]
transfer[, years_to_transfer:= rexp(.N, gp$rate_tout)]
transfer[, transfer_candidate := as.Date(round(reg_date + years_to_transfer*365.25))]
transfer[, transfer_date := fifelse(
      transfer_candidate < pmin(enddate_init, datestudylast, na.rm = TRUE),
      transfer_candidate,
      as.Date(NA))]
transfer<-transfer[,.(patient_id, transfer_date)]


# Note: transfer dates can be after death but cannot be after date of end study
