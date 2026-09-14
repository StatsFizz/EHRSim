################################################################################
#
#   Script:       P4_Simulate_underlying_clinical_measures.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R object: patient, frailty 
#
#   Data created:	R object: clinicalmeasure
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script simulates information about the patient's height, 
#               weight, BMI, smoking and alcohol consumption.
#
#					      Mismeasured versions will be generated subsequently.
#
################################################################################
#
#	  Notes:			The cm data is simulated across the patient's life
#		  		  	  (from age 18), despite most patients not entering the 
#			    	  	data until later, and some patients will die/transfer out
#			    	  	before the end of their generated data. 
#
################################################################################


setDT(patient)
setDT(frailty)


################################################
#  Simulate height and weight data at age 18   #
################################################

height18 <- patient[, .(patient_id, reg_date, sex, dob, deprivation_person, ethnicity)]
height18[,year18:=year(as.Date(dob+18*365.25))]

# Mean height age 18, based on sex and deprivation, SD based on sex only
height18[, meanheight :=  fifelse(sex == "Female", 165, 178) +
        fcase(ethnicity == "White", 0,
              ethnicity == "Asian", -0.5,
              ethnicity == "Black",  0.25,
              ethnicity == "Mixed",  0,
              ethnicity == "Other", -0.25) - 
          (2025-year18)*0.01]
height18[, sdheight := fifelse(sex == "Female", 4.6, 3.3)]

# Generate height values (no extreme values allowed, i.e. 150, 200 for men)
height18[, lbound := fifelse(sex == "Female", 145, 150)]
height18[, ubound := fifelse(sex == "Female", 180, 200)]
height18[, height18 := rtruncnorm(.N, a = lbound, b = ubound,
                          mean = meanheight, sd = sdheight)]
height18[,c("lbound", "ubound", "meanheight", "sdheight"):=NULL]

# Mean weight age 18, based on sex and deprivation, SD based on sex only
weight18 <- patient[, .(patient_id, reg_date, sex, dob, deprivation_person, ethnicity)]
weight18[,year18:=year(as.Date(dob+18*365.25))]
weight18[, meanweight :=  fifelse(sex == "Female", 70, 80) +
           fcase(ethnicity == "White",  0,
                 ethnicity == "Asian",  0.25,
                 ethnicity == "Black",  2,
                 ethnicity == "Mixed",  0,
                 ethnicity == "Other", -0.5)*(sex=="Female") +  
           fcase(ethnicity == "White",  1,
                 ethnicity == "Asian",  1.5,
                 ethnicity == "Black", -0.25,
                 ethnicity == "Mixed",  0,
                 ethnicity == "Other", -0.25)*(sex=="Male") - 
           (2025-year18)*0.01*(sex=="Female") -
           (2025-year18)*0.005*(sex=="Male") + 
           fcase(deprivation_person == 1, -1,
                 deprivation_person == 2, -0.5,
                 deprivation_person == 3,  0,
                 deprivation_person == 4,  0.5,
                 deprivation_person == 5,  1)]
weight18[, sdweight := fifelse(sex == "Female", 10, 8)]
weight18[, lbound := fifelse(sex == "Female", 35, 40)]
weight18[, ubound := fifelse(sex == "Female", 102, 102)]
weight18[, weight18 := rtruncnorm(.N, a = lbound, b = ubound,
                           mean = meanweight, sd = sdweight)]
weight18[,c("lbound", "ubound", "meanweight", "sdweight"):=NULL]



measure18 <- merge(
  height18[, .(patient_id, reg_date, dob, height18, deprivation_person, ethnicity)],
  weight18[, .(patient_id, reg_date, dob, weight18, deprivation_person, ethnicity)],
  by = c("patient_id", "reg_date", "dob", "deprivation_person", "ethnicity"),
  all.x = TRUE
)
measure18[,bmi18 := weight18 / ( (height18/100)^2 )]
rm(height18, weight18)






#####################################
#  Simulate weight data over time   #
#####################################


### Add in frailty values

cm <- merge(measure18,
  frailty[, .(patient_id, frailty, sex, agein, ageout)],
  by = "patient_id",  all.x = TRUE)
cm[, traj := NA_real_]
rm(measure18)



### Generate a weight trajectory (gaining, stable, losing weight)


# Program to draw weight trajectories, by age, BMI (18yrs) and frailty groups
trajectory <-  function(indata, prob_cats, frailty_k, bmi_rk, age_rk) {
  
  # Subset to the group of interest, keeping just patient id and age
  ds <- indata[frailty %in% frailty_k & bmi18 >= bmi_rk[1] & bmi18 < bmi_rk[2] 
          & agein >= age_rk[1] & agein < age_rk[2], .(patient_id, agein)]
  
  # Draw frailty, according to provided distribution
  ds[, trajn := sample(1:3, .N, replace = TRUE, prob = prob_cats)]

  # Merge trajectories into original dataset and return modified dataset
  indata[ds, traj := fifelse(is.na(traj), trajn, traj), on = .(patient_id, agein)]
  indata[]
}


##  Define trajectory probabilities based on frailty and binary age
fp <- list(
  # Not frail
  uf = list(ay = list( # Younger
    bmi1 = c(0.5,0.35,0.15), bmi2 = c(0.65,0.27,0.08), bmi3 = c(0.7,0.2,0.1)
  ), ao = list( # Older
    bmi1 = c(0.5,0.35,0.15), bmi2 = c(0.65,0.27,0.08), bmi3 = c(0.7, 0.2,0.1)
  )), 
  # Frail
  f  = list(ay = list( # Younger
    bmi1 = c(0.5,0.35,0.15), bmi2 = c(0.65,0.27,0.08), bmi3 = c(0.7,0.2,0.1)
  ), ao = list( # Older
    bmi1 = c(0.35,0.5,0.15), bmi2 = c(0.27,0.65, 0.08),bmi3 = c(0.2,0.7,0.1)
  )), 
  # Very frail
  vf = list(ay = list( # Younger
    bmi1 = c(0.15,0.15,0.7), bmi2 = c(0.3,0.2,0.5), bmi3 = c(0.4,0.2,0.4)
  ), ao = list( # Older
    bmi1 = c(0.4,0.2,0.4), bmi2 = c(0.8,0.1,0.1), bmi3 = c(0.8,0.1,0.1)
  ))   
)

## Define groups to loop over
agegp   <- list(ay = c(0,50.5), ao = c(50,150))
frailgp <- list(uf = c(1,2),f = c(3), vf = c(4))
bmigp   <- list(bmi1 = c(0,25), bmi2 = c(25,50.5), bmi3 = c(50.5,150))

## Loop over groups to draw and assign trajectories
for (f in c("uf", "f", "vf")) {
  for (a in c("ay", "ao")) {
    for (b in c("bmi1", "bmi2", "bmi3")) {
      cm<-trajectory(cm,fp[[f]][[a]][[b]], frailgp[[f]], bmigp[[b]], agegp[[a]])
    }
  }
}
rm(fp, agegp, frailgp, bmigp, a, b, f)



###  Simulate projected weight  ###

# Draw rate change (increasing, stable, decreasing)
cm[, rate_change := fcase(traj == 1, rpois(.N, 100) / 350,
                          traj == 2, 0,
                          traj == 3, -rpois(.N, 100) / 400,
                          default = 0)]
cm[, max_weight := 110]
cm[, min_weight := 30]
cm[, ind := 1]


# Expand data by 5-year age-groups
# View(cm)
cm <- survSplit(
  Surv(agein, ageout, ind) ~ .,
  cm,
  cut = c(18,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,150),
  episode = "agegroup"
)
setDT(cm)


# Generate weight values over time
setorder(cm, patient_id, agein)
cm[, `:=`(datein  = as.Date(round(dob + agein  * 365.25)),
          dateout = as.Date(round(dob + ageout * 365.25)))]
cm[patient, enddate_init := i.enddate_init, on = "patient_id"]
cm[, dateout := pmin(dateout, enddate_init)]
cm[, `:=`(mid=(agein + ageout) / 2, 
           lag_mid = shift((agein + ageout) / 2)), by=patient_id]
cm[, gap := mid - lag_mid]
cm[, change := fifelse(is.na(gap), 0, rate_change * gap)]
cm[, cum_sum_change := cumsum(change), by = patient_id]
cm[, weight := pmax(pmin(weight18 + cum_sum_change, max_weight),min_weight)]

# Add a slight jitter to weight
cm[, weight:= weight + rnorm(.N,0,0.1)]
# cm[patient_id %in% 100000023261]

#####################################
#  Simulate height data over time   #
#####################################

# Slight decrease in height with age, after age 70
cm[, height_cm:= height18 - (agein>=70)*(agein-70)*0.05]




##############################
#  Determine BMI over time   #
##############################

cm[, bmi := weight / ( (height_cm/100)^2 )]
cm[, bmicat := factor(cut(bmi, breaks=c(0,18.5, 25, 30, 35,1000), right=FALSE), 
      labels=c("Underweight","Normal","Overweight","Obese","Severely Obese"))]

# Keep relevant variables only
cm<-cm[,.(patient_id, reg_date, frailty, sex, agein, ageout, mid, datein, 
          dateout, agegroup, weight, height_cm, bmi, bmicat, 
          deprivation_person, ethnicity)]


############################
#  Simulate smoking data   #
############################

### Based on age, sex, deprivation, BMI
  
setorder(cm, patient_id, agein)

cm[, `:=`(
  # Compute logits
  logitsmoke = -4
  + fcase(deprivation_person == 2, 0.08,
          deprivation_person == 3, 0.07,
          deprivation_person == 4, 0.12,
          deprivation_person == 5, 0.38,
          default = 0)
  + fcase(between(mid, 50, 60), 0.04,
          between(mid, 60, 70), 0.10,
          between(mid, 70, 80), 0.02,
          between(mid, 80, 150), -0.15,
          default = 0)
  + fcase(sex == "Male", 0.05, default = 0)
  + fcase(sex == "Male" & between(mid, 50, 60), 0.2,
          sex == "Male" & between(mid, 60, 70), 0.4,
          sex == "Male" & between(mid, 70, 80), 0.7,
          sex == "Male" & between(mid, 80, 150), 0.9,
          default = 0)
	  + fcase(between(bmi, 18.5, 25), 0.04,
	          between(bmi, 25, 30), 0.10,
	          between(bmi, 30, 35), 0.02,
	          between(bmi, 35, 120), 0.02,
	          default = 0)
  + fcase(ethnicity == "Asian", -0.9,
          ethnicity == "Black", -0.8,
          ethnicity == "Mixed", -0.6,
          ethnicity == "Other", -0.45,
          default = 0),
  logitquit = 0.25
  + fcase(deprivation_person == 2, -0.19,
          deprivation_person == 3, -0.17,
          deprivation_person == 4, -0.17,
          deprivation_person == 5, -0.44,
          default = 0)
  + fcase(between(mid, 50, 60), 0.25,
          between(mid, 60, 70), 0.70,
          between(mid, 70, 80), 1.15,
          between(mid, 80, 150), 1.6,
          default = 0)
	  + fcase(between(bmi, 18.5, 25), 0.7,
	          between(bmi, 25, 30), 1.1,
	          between(bmi, 30, 35), 1.4,
	          between(bmi, 35, 120), 1.4,
	          default = 0))]

# Compute probabilities and indicators
cm[, `:=`(smoke = rbinom(.N, 1, 1 / (1 + exp(-logitsmoke))),
          quit  = rbinom(.N, 1, 1 / (1 + exp(-logitquit))))]

# Cumulative smoking and categorical variable
cm[, cum_sum_smoke := cumsum(smoke), by = patient_id]
cm[, smoke_status := fcase(
  cum_sum_smoke == 0, 0,
  cum_sum_smoke == 1, 1,
  cum_sum_smoke > 1 & quit == 0, 1,
  cum_sum_smoke > 1 & quit == 1, 2
)]
cm[, smokecat := factor(
  smoke_status,
  levels = c(0, 1, 2),
  labels = c("No", "Current", "Ex")
)]

# Cleanup temporary columns
cm[, c("logitsmoke", "logitquit", "cum_sum_smoke", "smoke", "quit",
       "smoke_status") := NULL]






############################
#  Simulate alcohol data   #
############################



## Harmful alcohol use

setorder(cm, patient_id, agein)

cm[, propensity_alc:= -5.4
  + fcase(smokecat=="Current", 1.32,
          smokecat=="Ex",     0.76,
              default = 0) 
  + fcase(deprivation_person == 2, -0.07,
          deprivation_person == 3, 0.04,
          deprivation_person == 4, 0.25,
          deprivation_person == 5, 0.4,
          default = 0)
  + fcase(between(mid, 50, 60), 0.10,
          between(mid, 60, 70), -0.02,
          between(mid, 70, 80), -0.5,
          between(mid, 80, 150), -1,
          default = 0)
  + fcase(sex == "Male", 0.5, default = 0)
  + fcase(sex == "Male" & between(mid, 50, 60), 0.05,
          sex == "Male" & between(mid, 60, 70), 0.15,
          sex == "Male" & between(mid, 70, 80), 0.25,
          sex == "Male" & between(mid, 80, 150), 0.2,
          default = 0)
	  + fcase(between(bmi, 18.5, 25), -0.65,
	          between(bmi, 25, 30), -0.75,
	          between(bmi, 30, 35), -0.5,
	          between(bmi, 35, 120), -0.5,
	          default = 0)
  + fcase(ethnicity == "Asian", -1,
          ethnicity == "Black", -0.75,
          ethnicity == "Mixed", -0.75,
          ethnicity == "Other", -0.4,
          default = 0)]


# Compute probabilities and indicator of harmful alcohol use
cm[, harmalc:= rbinom(.N, 1, 1 / (1 + exp(-propensity_alc)))]
setorder(cm, patient_id, agein)
cm[,harmalc:=cummax(harmalc), by=patient_id]



# Probabilities of being non/current/ex drinker based on alcohol propensity
p_list <- list( c(0.4, 0.5, 0.05, 0.05),
                c(0.25, 0.25, 0.25, 0.25),
                c(0.1, 0.05, 0.8, 0.05),
                c(0.01, 0.01, 0.6, 0.38))

# Assign alcohol category based on categorised propensity
q<-quantile(cm$propensity_alc,  probs = c(0, 0.25, 0.5, 0.75, 1))
cm[,propalccat := cut(propensity_alc, breaks=q, labels=c(1,2,3,4),include.lowest = TRUE)]
cm[, u1 := runif(.N)]
cm[, alccat :=  fifelse(propalccat==1, 
                  fifelse(u1 < p_list[[1]][1], "None",
                  fifelse(u1 < sum(p_list[[1]][1:2]), "Low",
                  fifelse(u1 < sum(p_list[[1]][1:3]), "Medium", "High"))),
                fifelse(propalccat==2,
                  fifelse(u1 < p_list[[2]][1], "None",
                  fifelse(u1 < sum(p_list[[2]][1:2]), "Low",
                  fifelse(u1 < sum(p_list[[2]][1:3]), "Medium", "High"))),
                fifelse(propalccat==3,
                  fifelse(u1 < p_list[[3]][1], "None",
                  fifelse(u1 < sum(p_list[[3]][1:2]), "Low",
                  fifelse(u1 < sum(p_list[[3]][1:3]), "Medium", "High"))),
                fifelse(propalccat==4,
                  fifelse(u1 < p_list[[4]][1], "None",
                  fifelse(u1 < sum(p_list[[4]][1:2]), "Low",
                  fifelse(u1 < sum(p_list[[4]][1:3]), "Medium", "High"))),
                NA_character_))))]
cm[, c("propalccat", "u1") := NULL]
rm(p_list)


# Create Current, Ex and No categorisation
cm[, alc3cat := fifelse(alccat == "None", "No", "Current")]
setorder(cm, patient_id, datein)
cm[, ever := cummax(alc3cat == "Current"), by = patient_id]
cm[, alc3cat := fifelse(ever==1 & alc3cat!="Current", "Ex", alc3cat)]





########################################
#  Assemble clinical measures dataset  #
########################################

keepvars <- c("patient_id", "sex", "agein",  "ageout", "datein", "dateout", 
              "height_cm", "bmi", "bmicat", "weight", "smokecat", 
              "propensity_alc", "harmalc", "alccat", "alc3cat")

clinicalmeasure <- cm[,mget(keepvars)]    
clinicalmeasure <- unique(clinicalmeasure)

# Remove unneeded objects;
rm(trajectory, cm)
