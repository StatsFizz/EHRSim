################################################################################
#
#   Script:       P5_a_Simulate_comorbidities_cardiometabolic.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R object: patient frailty clinicalmeasure
#
#   Data created:	R object: comorbid_dt, comorbid (for further data generation)
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script simulates a dataset resembling real EHR data,
#					      containing information about the patient's 
#					      cardiometabolic comorbidities.  
#
################################################################################
#
#   Packages needed: lubridate, data.table
#
################################################################################

##############################
#  Gather the required data  #
##############################

setDT(clinicalmeasure)
setDT(patient)

# Start with clinical measure data, adding in date of birth
comorbid<-copy(clinicalmeasure)
comorbid[patient, `:=`(dob=i.dob, ethnicity=i.ethnicity,
  deprivation_person=i.deprivation_person), on="patient_id"]

# Add in dates of time period entry and exit
comorbid[, `:=`(datein  = as.Date(round(agein  * 365.25 + dob),
                dateout = as.Date(round(ageout * 365.25 + dob))))]
comorbid[patient, enddate_init := i.enddate_init, on = "patient_id"]
comorbid[, dateout := pmin(dateout, enddate_init)]
comorbid[, enddate_init := NULL]

# Add in year and age at mid-interval
comorbid[, yearmid := (year(datein) + year(dateout)) / 2]
comorbid[, agemid := (agein + ageout) / 2]

# Create male- and female- age variables
setnames(comorbid, old="agein", new="agein_det")
comorbid[, `:=`(mage = fifelse(sex=="Male", agein_det, 0),
                fage = fifelse(sex=="Female", agein_det, 0))]

# Create a more crudely grouped version of age-in (to merge frailty in)
comorbid[, agein := fifelse(agein_det %in% c(18, 25, 30, 35, 40, 45), 18,
                    fifelse(agein_det %in% c(50, 55, 60), 50,
                    fifelse(agein_det %in% c(65, 70, 75), 65,
                    fifelse(agein_det %in% c(80, 85, 90, 95), 80,
                    NA_real_))))]

# Add in frailty
comorbid[frailty, `:=`(frailty = i.frailty, agein = i.agein), 
         on = .(patient_id, agein)]

# Remove variables not needed
comorbid[,c("agein", "height_cm", "dob"):=NULL]

# Initialise variables to zero
comorbid_init <- as.data.frame(matrix(0, nrow = 1, ncol = 11))
init_vars <- c("ht_prev", "ht_event", "diabetes_prev", "mi_event",
               "mi_prev", "chd_prev","heartfail_prev","other_athero_prev",
               "pvd_prev","stroke_event", "stroke_prev")
names(comorbid_init)<- init_vars 
comorbid<-cbind(comorbid, comorbid_init)
rm(comorbid_init, init_vars)


################################
#  Generate hypertension data  #
################################


comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$ht_coefs, "logitht") 
comorbid<-genind(comorbid, "logitht", "ht_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", "ht_event", "ht_prev")

# Reduce the number of people who "recover" from hypertension
comorbid[order(patient_id, agein_det),
  ht_event := {
    u <- runif(.N)
    fifelse(u < 0.5 & ht_event == 0 & ht_prev == 1, 1L, ht_event)
  },
  by = patient_id]


############################
#  Generate diabetes data  #
############################


comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$diabetes_coefs,"logitdiab") 
comorbid<-genind(comorbid, "logitdiab", "diabetes_current") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "diabetes_current", "diabetes_prev", "diabetes_onset")
comorbid[, c("diabetes_current") := NULL]




#########################################
#  Generate myocardial infarction data  #
#########################################


comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$mi_coefs,"logitmi") 
comorbid<-genind(comorbid, "logitmi", "mi_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "mi_event", "mi_prev")



#####################################
#  Generate coronary heart disease  #
#####################################

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$chd_coefs,"logitchd") 
comorbid<-genind(comorbid, "logitchd", "chd_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "chd_event", "chd_prev", "chd_onset")
comorbid[, c("chd_event") := NULL]




############################
#  Generate heart failure  #
############################

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$heartfail_coefs,"logitheartfail") 
comorbid<-genind(comorbid, "logitheartfail", "heartfail_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "heartfail_event", "heartfail_prev", "heartfail_onset")
comorbid[, c("heartfail_event") := NULL]




####################################
#  Generate other atherosclerosis  #
####################################


comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$athero_coefs,"logitathero") 
comorbid<-genind(comorbid, "logitathero", "athero_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                    "athero_event", "other_athero_prev", "other_athero_onset")
comorbid[, c("athero_event") := NULL]





###########################################
#  Generate peripheral vascular disease   #
###########################################

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$pvd_coefs,"logitpvd") 
comorbid<-genind(comorbid, "logitpvd", "pvd_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "pvd_event", "pvd_prev", "pvd_onset")
comorbid[, c("pvd_event") := NULL]



##########################
#  Generate stroke/TIA   #
##########################

comorbid<-createlogit_cm(comorbid, comorbidity_coef_lists$stroke_coefs,"logitstroke") 
comorbid<-genind(comorbid, "logitstroke", "stroke_event") 
comorbid<-genprevonset(comorbid, "patient_id", "agein_det", 
                       "stroke_event", "stroke_prev")
