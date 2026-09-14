################################################################################
#
#   Script:       P3_Simulate_frailty.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R object: patient 
#
#   Data created:	R object: frailty
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script simulates a dataset containing information about
#               the patient's (typically unknown) frailty level. 
#
################################################################################
#
#   Packages needed: survival, dplyr
#
################################################################################
#
#	  Notes:			The frailty data is simulated across the patient's life
#		  		  	  (from age 18), despite most patients not entering the 
#			    	  	data until later.
#
################################################################################

setDT(patient)  

#################################################
# Split person-time into age-defined intervals  #
#################################################
frailty <- patient[, .(patient_id, practice, dob, sex, enddate_init)]
frailty[, enddate_yearsold := round(as.numeric(enddate_init - dob)/365.25, 2)]
frailty[, ind := 1]

# Use survSplit to create age intervals
frailty <- survSplit(Surv(enddate_yearsold, ind) ~ ., data = frailty,
                cut = c(18, 50, 65, 80, 106),
                episode = "timegp")
frailty<- unique(frailty)

# Keep only intervals >= 18 years
setDT(frailty)  
frailty <- frailty[tstart >= 18,]
frailty[, timegp := factor(timegp - 1, levels = 1:4,
                           labels = c("a1", "a2", "a3", "a4"))]



##########################
# Frailty probabilities  #
##########################

fp <- list(
  male = list(
    a1 = c(0.70, 0.21, 0.08, 0.01),
    a2 = c(0.60, 0.28, 0.10, 0.02),
    a3 = c(0.40, 0.30, 0.25, 0.05),
    a4 = c(0.10, 0.35, 0.40, 0.15)
  ),
  female = list(
    a1 = c(0.64, 0.23, 0.10, 0.03),
    a2 = c(0.50, 0.28, 0.18, 0.04),
    a3 = c(0.29, 0.32, 0.30, 0.09),
    a4 = c(0.10, 0.35, 0.40, 0.15)
  )
)


###########################################
# Randomly draw frailty from multinomial  #
###########################################

frailty[, fraildraw := NA_integer_]
for (lsex in c("male","female")) {
  sex_label <- ifelse(lsex=="male","Male","Female")
  for (iage in c("a1","a2","a3","a4")) {
    idx <- frailty$sex == sex_label & frailty$timegp == iage
    nrows <- sum(idx)
    if (nrows > 0) {
      frailty[idx, fraildraw := sample(1:4, nrows, replace = TRUE, 
                                  prob = fp[[lsex]][[iage]])]
    }
  }
}



######################################
# Ensure frailty increases over age  #
######################################

setorder(frailty, patient_id, timegp)  
frailty[, agein := tstart]
frailty[, ageout := enddate_yearsold]

# Cumulative maximum per patient
frailty[, frailty := cummax(fraildraw), by = patient_id]
frailty[, c("fraildraw","ind","timegp","tstart") := NULL]


####################### 
# Add calendar dates  #
#######################

frailty[, datein  := as.Date(dob + agein*365.25)]
frailty[, dateout := as.Date(dob + ageout*365.25)]
frailty[, dateout := pmin(dateout, enddate_init)]



########
# Tidy #
########

frailty[ ,c("practice","dob","enddate_init", "enddate_yearsold"):=NULL]
setcolorder(frailty, c("patient_id","sex", "agein","ageout","datein","dateout","frailty"))
frailty <- unique(frailty)
