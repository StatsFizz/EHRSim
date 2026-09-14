################################################################################
#
#   Script:       P2_Simulate_patients.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R list: gp 
#
#   Data created:	R object: patient
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script simulates a dataset resembling real EHR data,
#					      containing basic information about the patients.
#
#					      The main output file contains information on sex, ethnicity, 
#				      	year of birth, registration and transfer out dates.
#
#			      		Note this initial file contains information not usually 
#			      		available (but useful for simulation purposes):
#				      	the fully observed ethnicity information (no missing values)
#				      	and the full date of birth. This file also contains 
#               simulated deprivation. 
#
################################################################################
#
#   Packages needed: 
#
################################################################################




####################
#  Simulate data   #
####################

# Set last study date as end of last registration year
datestudylast<-as.Date(ceiling((gp$last_regyear - 1969)*365.25-1))

# Sex
sex<-2 - rbinom(gp$prac_n, 1, gp$pmale)
sex<-factor(sex, levels = c(1,2), labels=c("Male", "Female"))

###  Ethnicity  ###

# Ethnicity 
prob<- c(gp$peth_white, gp$peth_asian, gp$peth_black,
                 gp$peth_other, gp$peth_mixed)
ethnicity <- sample(c("White", "Asian", "Black", "Other", "Mixed"), 
                            length(sex), prob=prob, replace=TRUE)
rm(prob)

###  Deprivation  ###

# Deprivation - person level - distribution differs according to practice-level
pdeprivation1<-c(0.7,  0.24, 0.03, 0.02, 0.01) 
pdeprivation2<-c(0.3,  0.5, 0.1, 0.06, 0.04)
pdeprivation3<-c(0.12, 0.16, 0.45, 0.16, 0.11)
pdeprivation4<-c(0.04, 0.08, 0.12, 0.56, 0.2)
pdeprivation5<-c(0.02, 0.03, 0.08,  0.17, 0.7)

deprivation_person <- sample(c(1,2,3,4,5), length(sex), 
                     get(paste0("pdeprivation",gp$prac_deprivation)), replace=TRUE)
rm(pdeprivation1, pdeprivation2, pdeprivation3, pdeprivation4, pdeprivation5)


###  Registration years ###

yrs_intake<- gp$max_reg  - gp$min_reg  + 1

prob_intake <- 1.1^(0:(yrs_intake-1))                     
reg_year <- sample(gp$min_reg:gp$max_reg, gp$prac_n, 
                       prob=prob_intake, replace=TRUE)

reg_date <- as.Date(paste0(reg_year, "-01-01"))
reg_date <- fifelse(year(reg_date)==year(datestudylast),
                as.Date(reg_date + sample(1:182, length(sex), replace=TRUE)),
                as.Date(reg_date + sample(1:365, length(sex), replace=TRUE)))



###  Date of birth ###

# Assume age at registration is between 18 and 95
bound_lower <- reg_year - 95
bound_upper <- reg_year - 18
mean1 <- reg_year - 30
mean2 <- reg_year - 60
sd1 <- 80
sd2 <- 80 

v1 <- rtruncnorm(gp$prac_n, a=bound_lower, b=bound_upper, mean=mean1, sd=sd1)
v2 <- rtruncnorm(gp$prac_n, a=bound_lower, b=bound_upper, mean=mean2, sd=sd2)
yob <- round(fifelse(runif(gp$prac_n)<0.4, v1, v2))
rm(v1,v2,bound_lower, bound_upper, mean1, mean2, sd1, sd2)

# Date of birth (days since 1 Jan 1970)
dob <- as.Date(runif(gp$prac_n, 0, 365.25) + 365.25*(yob-1970))

# Enforce the intended registration-age range on the exact date of birth, not
# only the year of birth. Otherwise late birthdays in the upper birth year can
# leave patients younger than 18 at registration.
dob_lower <- reg_date %m-% years(95)
dob_upper <- reg_date %m-% years(18)
dob <- pmin(pmax(dob, dob_lower), dob_upper)
yob <- as.numeric(format(dob, "%Y"))
rm(dob_lower, dob_upper)


### Initial end of follow-up

# Note: This is the date the patient reached 105 years old or last registration year 
enddate_init <- as.Date(pmin(dob+105*365.25, datestudylast))



### Patient ID 

# Generate unique IDs
id<-seq(gp$prac_n)

# Make ID length min 12: pracID (max 4 digits) + zeros (3) + ID (max 5 digits) 
# Standardise length of ID with leading zeros
desired_length <- 12 
diff <- desired_length - nchar(as.character(max(id))) - nchar(as.character(gp$prac_id)) 
extra_zeros <- max(diff, 1)
pad_width <- nchar(as.character(max(id))) + extra_zeros

std_id <- stringr::str_pad(id, 
                           width = pad_width, 
                           side = "left", 
                           pad = "0")

# Add a practice-specific stem (use practice ID)
patient_id <- paste0(gp$prac_id, std_id)

# Check this is a unique ID
if (anyDuplicated(patient_id)) {
  stop("Patient IDs are not unique.", call. = FALSE)
}

# Remove unneeded objects
rm(id, diff, desired_length, extra_zeros, pad_width, std_id)




#################
#  Some checks  #
#################

if (!all(dob < reg_date)) {
  stop("Some patients have date of birth on or after registration date.", call. = FALSE)
}

if (!all(dob <= reg_date %m-% years(18))) {
  stop("Some patients are younger than 18 at registration.", call. = FALSE)
}

if (!all(dob >= reg_date %m-% years(95))) {
  stop("Some patients are older than 95 at registration.", call. = FALSE)
}



##############################
#  Assemble patient dataset  #
##############################

patient <-data.frame(patient_id, practice=gp$prac_id, dob, yob, sex, 
                     ethnicity,  
                     deprivation_person, deprivation_prac=gp$prac_deprivation,
                     reg_date, enddate_init)


# Remove unneeded objects; keep only gp, patient and last_reg_year
rm(patient_id, dob, yob, sex, ethnicity, 
             deprivation_person, reg_date, enddate_init)

