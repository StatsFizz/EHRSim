################################################################################
#
#   Script:       P10_Simulate_clinical_measures.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R object: patient consultation clinicalmeasure gp
#
#   Data created:	R object:  clinmeasure
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script creates measured values of weight, BMI, smoking, alcohol
#				        with intermittent measurement and some errors.	      
#
################################################################################




################################
#  Put required data together  #
################################

#  Start with consultation data and add date of birth and enddate
all_measure <- copy(consultation)
all_measure[patient, `:=`(dob = i.dob, enddate = i.enddate), on = "patient_id"]

# Calculate age at measurement and 5-year age groups
all_measure[, age := as.numeric(date - dob)/365.25]

# Categorise age in
agecutoff <- c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
all_measure[, agein := agecutoff[findInterval(age, agecutoff, all.inside=TRUE)]]

# Add BMI, smoking and alcohol values by age band
all_measure[clinicalmeasure, `:=`(
  height_cm = i.height_cm,
  bmi = i.bmi,
  weight = i.weight,
  smokecat = i.smokecat,
  propensity_alc = i.propensity_alc,
  harmalc = i.harmalc,
  alccat = i.alccat,
  alc3cat = i.alc3cat
), on = c("patient_id", "agein")]



#### Identify consultations where clinical measures are likely    #####


### Consultation types relevant to different measures

# Consultations where BMI/weight may be measured
bmi_always <- c("weight_management", "referral_weight", "health_check")
bmi_likely <- c("alcohol_advice","harmful_alcohol_use", "diabetes")        
bmi_sometimes <- c("contraception", "chronic_disease_review")
bmi_maybe <- c("disease_onset", "event", "fup_disease_onset", "fup_event", "surgery", "hrt")
bmi_occasionally <- c("clinic", "medicine_management", "other")

# Consultations where smoking status may be measured
smoke_always <- c("smoke_cessation", "copd", "health_check")
smoke_likely <- c("alcohol_advice", "harmful_alcohol_use", "diabetes")        
smoke_sometimes <- c("contraception", "chronic_disease_review")
smoke_maybe <- c("disease_onset", "event", "fup_disease_onset", "fup_event", "surgery", "hrt")
smoke_occasionally <- c("clinic", "medicine_management", "other")

# Consultations where alcohol consumption may be measured
alcohol_always <- c("alcohol_advice","harmful_alcohol_use", "copd", "health_check")
alcohol_likely <- c("fup_event", "event", "diabetes")        
alcohol_sometimes <- c("chronic_disease_review", "contraception")
alcohol_maybe <- c("disease_onset", "event", "fup_disease_onset", "fup_event", "surgery", "hrt")
alcohol_occasionally <- c("clinic", "medicine_management", "other")


### Label consultations where measures may be taken

# BMI measurement flags
all_measure[, `:=`(
  bmi_always      = as.integer(consultation_type %chin% bmi_always),
  bmi_likely      = as.integer(consultation_type %chin% bmi_likely),
  bmi_sometimes   = as.integer(consultation_type %chin% bmi_sometimes),
  bmi_maybe       = as.integer(consultation_type %chin% bmi_maybe),
  bmi_occasionally= as.integer(consultation_type %chin% bmi_occasionally))]
rm(bmi_always, bmi_likely, bmi_sometimes, bmi_maybe, bmi_occasionally)

# Smoking measurement flags
all_measure[, `:=`(
  smoke_always      = as.integer(consultation_type %chin% smoke_always),
  smoke_likely      = as.integer(consultation_type %chin% smoke_likely),
  smoke_sometimes   = as.integer(consultation_type %chin% smoke_sometimes),
  smoke_maybe       = as.integer(consultation_type %chin% smoke_maybe),
  smoke_occasionally= as.integer(consultation_type %chin% smoke_occasionally))]
rm(smoke_always, smoke_likely, smoke_sometimes, smoke_maybe, smoke_occasionally)

# In GP surgery visits, adjust smoking flags to depend on current smoking
all_measure[consultation_type=="surgery" & smokecat=="Current",  smoke_likely := 1]
all_measure[consultation_type=="surgery" & smokecat=="Ex",       smoke_sometimes := 1]
all_measure[consultation_type=="surgery" & smokecat=="No",       smoke_occasionally := 1]

# Alcohol measurement flags
all_measure[, `:=`(
  alcohol_always      = as.integer(consultation_type %chin% alcohol_always),
  alcohol_likely      = as.integer(consultation_type %chin% alcohol_likely),
  alcohol_sometimes   = as.integer(consultation_type %chin% alcohol_sometimes),
  alcohol_maybe       = as.integer(consultation_type %chin% alcohol_maybe),
  alcohol_occasionally= as.integer(consultation_type %chin% alcohol_occasionally))]
rm(alcohol_always, alcohol_likely, alcohol_sometimes, alcohol_maybe, alcohol_occasionally)

# In GP surgery visits, adjust alcohol flags to depend on current drinking
all_measure[consultation_type=="surgery" & alccat %in% c("Medium","High"), alcohol_likely := 1]
all_measure[consultation_type=="surgery" & alccat == "Low",        alcohol_sometimes := 1]
all_measure[consultation_type=="surgery" & alccat == "None",        alcohol_occasionally := 1]




#### Generate consultations where clinical measures were made   #####


# Appointments at which BMI was measured
all_measure[, u := runif(.N)]
bmi_measure <- all_measure[
    (bmi_always==1)               |
    (bmi_likely==1      & u<0.85) |
    (bmi_sometimes==1   & u<0.7)  |
    (bmi_maybe==1       & u<0.3)  |
    (bmi_occasionally==1 & u<0.15)
][, u := NULL, ][ , .(consultation_id, patient_id, date, consultation_type, 
                      height_cm, bmi, weight)]

# Appointments at which smoking was measured
all_measure[, u := runif(.N)]
smoke_measure <- all_measure[
    (smoke_always==1)                |
    (smoke_likely==1       & u<0.85) |
    (smoke_sometimes==1    & u<0.7)  |
    (smoke_maybe==1        & u<0.3)  |
    (smoke_occasionally==1 & u<0.15)
][, u := NULL, ][
  , .(consultation_id, patient_id, date, consultation_type, smokecat)]

# Appointments at which alcohol was measured
all_measure[, u := runif(.N)]
alcohol_measure <- all_measure[
    (alcohol_always==1)                |
    (alcohol_likely==1       & u<0.85) |
    (alcohol_sometimes==1    & u<0.7)  |
    (alcohol_maybe==1        & u<0.3)  |
    (alcohol_occasionally==1 & u<0.15)
][, u := NULL, ][
  , .(consultation_id, patient_id, date, consultation_type, 
      alccat, alc3cat, harmalc)]




################################
#  Generate BMI/weight data    #
################################

# Add a bit of noise to BMI and weight and categorise BMI
bmi_measure[, bmi := bmi + rnorm(.N, 0, 0.2)]
bmi_measure[, weight := weight + rnorm(.N, 0, 0.6)]
bmi_measure[, bmi_cat := fifelse(bmi < 18.5, "Underweight",
                         fifelse(bmi < 25, "Healthy",
                         fifelse(bmi < 30, "Overweight",
                         fifelse(bmi < 35, "Obese I",
                         fifelse(bmi < 40, "Obese II", "Obese III")))))]

# Decide whether weight or BMI is measured, add height sometimes
bmi_measure[, type   := fifelse(runif(.N) < 0.8, "weight", "bmi")]
bmi_measure[, data_weight := fifelse(type=="weight", weight,     NA_real_)]
bmi_measure[, data_bmi    := fifelse(type=="bmi",    bmi,        NA_real_)]
bmi_measure[, data_height := fifelse(type=="weight", height_cm,  NA_real_)]

# Once adult height is measured, assume rarely retaken
setorder(bmi_measure, patient_id, date) 
bmi_measure[, u := runif(.N)]
bmi_measure[, height_meas := cumsum(!is.na(data_height)), by=patient_id]
bmi_measure[height_meas > 1 & u < 0.7, data_height := NA_real_]
bmi_measure[height_meas > 5 & u < 0.95, data_height := NA_real_]
bmi_measure[, c("height_meas","u") := NULL]

# Tidy data
bmi_measure <- bmi_measure[, .(consultation_id, patient_id, date, 
                               data_bmi, data_height, data_weight, bmi_cat)]
setorder(bmi_measure, patient_id, date)



#############################
#  Generate smoking data    #
#############################

# Add some noise to smoking data (current and former)
smoke_measure[, `:=`(u1 = runif(.N), smokecat_orig = smokecat)]
smoke_measure[u1 > 0.9              & smokecat_orig == "Current", smokecat := "Ex"]
smoke_measure[u1 > 0.8 & u1 <= 0.9  & smokecat_orig == "Current", smokecat := "No"]
smoke_measure[u1 > 0.9              & smokecat_orig == "Ex",      smokecat := "Current"]
smoke_measure[u1 > 0.75 & u1 <= 0.9 & smokecat_orig == "Ex",      smokecat := "No"]
smoke_measure[, c("u1", "smokecat_orig") := NULL]

# Map to numeric code
smoke_measure[, data_smoke := fifelse(smokecat=="Current", 1,
                              fifelse(smokecat=="No", 0,
                              fifelse(smokecat=="Ex", 2, NA_integer_)))]

# Tidy data
smoke_measure <- smoke_measure[, .(consultation_id, patient_id, date, data_smoke)]
setorder(smoke_measure, patient_id, date)




#############################
#  Generate alcohol data    #
#############################

# Add some noise to the alcohol data
alcohol_measure[, `:=`(u1 = runif(.N), data_alcohol = as.character(alc3cat))]
alcohol_measure[, data_alcohol_orig := data_alcohol]
alcohol_measure[data_alcohol_orig == "Current" & u1 > 0.9, data_alcohol := "Ex"]
alcohol_measure[data_alcohol_orig == "Current" & u1 > 0.8 & u1 <= 0.9, data_alcohol := "No"]
alcohol_measure[data_alcohol_orig == "Ex" & u1 > 0.9, data_alcohol := "Current"]
alcohol_measure[data_alcohol_orig == "Ex" & u1 > 0.75 & u1 <= 0.9, data_alcohol := "No"]
alcohol_measure[, c("u1", "data_alcohol_orig") := NULL]

# Convert to numeric code
alcohol_measure[, data_alcohol := fifelse(data_alcohol=="Current", 1,
                                  fifelse(data_alcohol=="No", 0,
                                  fifelse(data_alcohol=="Ex", 2, NA_integer_)))]

# Keep harmalc column as data_harmalc
setnames(alcohol_measure, "harmalc", "data_harmalc")

# Tidy data
alcohol_measure <- alcohol_measure[, .(consultation_id, patient_id, date, data_alcohol, data_harmalc)]
setorder(alcohol_measure, patient_id, date)







#################################
#  Introduce recording errors   #
#################################


####--- BMI  ---####

###  Rounding error   
  
# Weight measured often to integer, sometimes 1 dp, occasionally 2
# BMI measured typically to 1dp, occasionally 0
# Height measured to 2dp 
# Draw one random number per row

# Round weight with probability bands
bmi_measure[, u := runif(.N)]
bmi_measure[!is.na(data_weight),
  data_weight := fifelse(u < 0.6, round(data_weight, 0),
                 fifelse(u < 0.7, round(data_weight, 1),
                 round(data_weight, 2)))]

# Round BMI with probability bands
bmi_measure[  !is.na(data_bmi),
  data_bmi := fifelse(u < 0.9, round(data_bmi, 1),  round(data_bmi, 0))]

# Round height (always nearest cm if present)
bmi_measure[!is.na(data_height),  data_height := round(data_height, 0)]
bmi_measure[, u := NULL]


  

#### Introduce errors to "true" BMI and weight  
  
# Errors:
#	- Mix up units
#	- Switch digits
#	- Two entries, one in cm one in m, e.g.
# - Some duplicates

## Occasionally report weight in pounds or stone (instead of kg)
# kg → lbs
bmi_measure[runif(.N) < 0.01 & !is.na(data_weight),
            data_weight := data_weight * 2.20462]
# kg → stone
bmi_measure[runif(.N) < 0.01 & !is.na(data_weight),
            data_weight := data_weight * 0.157473]
# cm → m
bmi_measure[runif(.N) < 0.01 & !is.na(data_height),
            data_height := data_height / 100]


## Switch digits - weight and BMI 
bmi_measure[, `:=`(u1 = runif(.N),  u2 = runif(.N),
  w  = ifelse(!is.na(data_weight), as.character(data_weight), NA_character_),
  b  = ifelse(!is.na(data_bmi), as.character(data_bmi), NA_character_))]

# Create "reversed" versions of weight and BMI (1st digit switched)
bmi_measure[, `:=`(
  rw = ifelse(!is.na(w),
              paste0(substr(w, 2, 2),
                     substr(w, 1, 1),
                     substr(w, 3, nchar(w))),
              NA_character_),
  rb = ifelse(!is.na(b),
              paste0(substr(b, 2, 2),
                     substr(b, 1, 1),
                     substr(b, 3, nchar(b))),
              NA_character_))]
# Apply the digit-switch-errors
bmi_measure[u1 < 0.01 & !is.na(data_weight), data_weight := as.numeric(rw)]
bmi_measure[u2 < 0.01 & !is.na(data_bmi), data_bmi := as.numeric(rb)]
bmi_measure[, c("u1", "u2", "w", "b", "rw", "rb") := NULL]


## Duplicate a small number of measurements
bmi_measure[, duplicate := (runif(.N) < 0.02) + 1]
bmi_measure <- bmi_measure[rep(seq_len(.N), times = duplicate)]
bmi_measure[, duplicate := NULL]

#  Change units for weight and height for some duplicated measures
# , add some random error in weight and BMI for others 
setorder(bmi_measure, patient_id, date)
bmi_measure[, `:=`(u1 = runif(.N),  u2 = runif(.N), ngrp = .N), 
            by = .(patient_id, date)]

# Height unit flip
bmi_measure[ngrp > 1 & u1 < 0.4 & !is.na(data_height),  
            data_height := data_height/100]

# Weight unit flips
bmi_measure[ngrp > 1 & u1 < 0.2 & !is.na(data_weight), 
            data_weight := data_weight * 2.20462]

bmi_measure[ngrp > 1 & u2 < 0.2 & !is.na(data_weight),
            data_weight := data_weight * 0.157473]

# Add noise
bmi_measure[ngrp > 1 & u1 > 0.8 & u2 > 0.8 & !is.na(data_weight),
            data_weight := data_weight + rnorm(.N, 0, 0.5)]

bmi_measure[ngrp > 1 & u1 > 0.8 & u2 > 0.8 & !is.na(data_bmi),
            data_bmi := data_bmi + rnorm(.N, 0, 0.5)]
bmi_measure[, c("u1", "u2", "ngrp") := NULL]


##  Add some missing data (for categorised clinical codes, set mostly missing)
bmi_measure[runif(.N) < 0.01 & !is.na(data_weight),  data_weight := 0]
bmi_measure[runif(.N) < 0.005 & !is.na(data_bmi),    data_bmi := 0]
bmi_measure[runif(.N) < 0.01 & !is.na(data_height),  data_height := 0]
bmi_measure[runif(.N) >= 0.3,  bmi_cat := NA_character_]





####--- Smoking  ---####

#  Add some missing data
smoke_measure[runif(.N) < 0.05 & !is.na(data_smoke),  data_smoke := -1]

# Add some transcribing / reporting errors
smoke_measure[, u:= runif(.N)]
smoke_measure[data_smoke == 0 & u < 0.005, data_smoke := 1]
smoke_measure[data_smoke == 0 & u < 0.01 & u>0.005,  data_smoke := 2]
smoke_measure[data_smoke == 1 & u < 0.005, data_smoke := 2]
smoke_measure[data_smoke == 1 & u < 0.01 & u>0.005,  data_smoke := 0]
smoke_measure[data_smoke == 2 & u < 0.005, data_smoke := 1]
smoke_measure[data_smoke == 2 & u < 0.01 & u>0.005, data_smoke := 0]
smoke_measure[, u := NULL]





####--- Alcohol  ---####

#  Add some missing data
alcohol_measure[runif(.N) < 0.025 & !is.na(data_alcohol),  data_alcohol := -1]

# Add some transcribing / reporting errors
alcohol_measure[, u:= runif(.N)]
alcohol_measure[data_alcohol == 0 & u < 0.005,  data_alcohol := 1]
alcohol_measure[data_alcohol == 0 & u < 0.01 & u>0.005,   data_alcohol := 2]
alcohol_measure[data_alcohol == 1 & u < 0.005,  data_alcohol := 2]
alcohol_measure[data_alcohol == 1 & u < 0.01 & u>0.005,   data_alcohol := 0]
alcohol_measure[data_alcohol == 2 & u < 0.005, data_alcohol := 1]
alcohol_measure[data_alcohol == 2 & u < 0.01 & u>0.005,  data_alcohol := 0]
alcohol_measure[, u := NULL]


# Clean up
rm(all_measure)



############################ 
#  Combine numerical data  #
############################ 


###  Tidy BMI/weight data 

# Put height, weight and BMI in long format as separate measures
height_measure <- bmi_measure[
  !is.na(data_height),
  .(consultation_id, patient_id, date,
    data1 = data_height,
    data2 = NA_real_,
    measure = "height",
    measure_detail = NA_character_)
]

weight_measure <- bmi_measure[
  !is.na(data_weight),
  .(consultation_id, patient_id, date,
    data1 = data_weight,
    data2 = NA_real_,
    measure = "weight",
    measure_detail = NA_character_)
]

bmi_measure_long <- bmi_measure[
  !is.na(data_bmi),
  .(consultation_id, patient_id, date,
    data1 = data_bmi,
    data2 = NA_real_,
    measure = "bmi",
    measure_detail = bmi_cat)
]

bmi_measure_long <- rbindlist(
  list(height_measure, weight_measure, bmi_measure_long),
  use.names = TRUE
)
rm(height_measure, weight_measure)


###  Tidy smoking data 

setnames(smoke_measure, "data_smoke", "data1")
smoke_measure <- smoke_measure[ , `:=`(
    measure = "smoking_status",
    data2 = NA_real_,
    measure_detail = NA_character_)]
smoke_measure <- smoke_measure[, .(consultation_id, patient_id, date,
      data1, data2, measure, measure_detail)]

###  Tidy alcohol data 

setnames(alcohol_measure,  old = c("data_alcohol", "data_harmalc"),
         new = c("data1", "data2"))
alcohol_measure <- alcohol_measure[ , `:=`(measure = "alcohol_consumption",
                                           measure_detail = NA_character_)]
alcohol_measure <- alcohol_measure[, .(consultation_id, patient_id, date,data1, 
                                       data2, measure, measure_detail)]


# Combine all clinical measures
clinmeasure <- rbindlist(list(bmi_measure_long, smoke_measure, alcohol_measure),
  use.names = TRUE)
setorder(clinmeasure, patient_id, date, measure)
rm(bmi_measure_long, smoke_measure, alcohol_measure)




### Create a supplemental information ID 

# Generate base IDs
id <- sort(rep(round(runif(1, 1, 5000)), nrow(clinmeasure)) +
  sample(seq_len(5 * nrow(clinmeasure)), nrow(clinmeasure)))
anyDuplicated(id)

# Pad to required length
desired_length <- 14
diff <- desired_length - nchar(as.character(max(id))) -
  nchar(as.character(gp$prac_id))
extra_zeros <- max(diff, 1)
pad_width <- nchar(as.character(max(id))) + extra_zeros

std_id <- stringr::str_pad(
  id,
  width = pad_width,
  side = "left",
  pad = "0")

# Add practice stem
suppl_info_id <- paste0(gp$prac_id, std_id)
anyDuplicated(suppl_info_id)
rm(id, diff, desired_length, extra_zeros, pad_width, std_id)


# Add the supplemental information ID to the table
clinmeasure <- clinmeasure[order(date)]
clinmeasure[, suppl_info_id := suppl_info_id]
anyDuplicated(clinmeasure$suppl_info_id)
rm(suppl_info_id)

# Tidy dataset
setorder(clinmeasure, patient_id, date)
clinmeasure <- clinmeasure[, .(suppl_info_id, consultation_id, patient_id, date,
                               data1, data2, measure, measure_detail)]

