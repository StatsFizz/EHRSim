################################################################################
#
#   Script:       P8_Update_tables_for_death_and_transfer.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:		R object: patient frailty clinicalmeasures comorbid deathdate transfer
#
#   Data created:	R object: patient frailty clinicalmeasures comorbid deathdate 
#                           (transfer information moved to patient table)
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script updates all tables to account for (i) the newly
#					      generated date of death (i.e. removes subsequent events) and
#               (ii) censoring due to transferring out of practice
#               (removing subsequent events other than externally-linked
#               death date).
#
################################################################################
#
#   Packages needed: data.table
#
################################################################################




##########################################################
#  Save tables used in data generation process to list   #
##########################################################

# Note: These datasets are not updated to account for death date
tables_dg <- list()
tables_dg[["patient"]] <- copy(patient)
tables_dg[["frailty"]] <- copy(frailty)
tables_dg[["clinicalmeasures"]] <- copy(clinicalmeasure)
tables_dg[["comorbid"]] <- copy(comorbid)
tables_dg[["medication"]] <- copy(medication)
tables_dg[["deathdate"]] <- copy(deathdate)
tables_dg[["transfer"]] <- copy(transfer)


##########################
#  Update patient table  #
##########################

# Pick up patient table and add death date and transfer date
patient[deathdate, deathdate := i.deathdate, on = "patient_id"]
patient[transfer, transfer_date := i.transfer_date, on = "patient_id"]

# Set post-death transfers to NA
patient[,transfer_date := fifelse(transfer_date>=deathdate & !is.na(deathdate),
                                  NA, transfer_date)]


### Update end of follow-up

# Note: This is the date the patient transferred out of practice or reached 105
# years old or last registration year or death date (whichever is earliest)
patient$enddate <- as.Date(pmin(patient$transfer_date,
                                patient$deathdate, 
                                as.Date(round(patient$dob + 105*365.25)), 
                                datestudylast, na.rm = TRUE))






####################################
#  Update clinical measures table  #
####################################

# Pick up death date and transfer out date
clinicalmeasure[deathdate, deathdate := i.deathdate, on = "patient_id"]
clinicalmeasure[transfer, transfer_date := i.transfer_date, on = "patient_id"]

# Delete entries with date-in after date of death
clinicalmeasure <- clinicalmeasure[(datein < deathdate | is.na(deathdate)),]

# Only keep entries starting before date of transfer out of GP practice
clinicalmeasure <- clinicalmeasure[(datein < transfer_date | is.na(transfer_date)),]

# Remove date of death and variables also in the patient dataset from the dataset
clinicalmeasure[,c("deathdate", "sex", "transfer_date"):= NULL]




################################
#  Update comorbidities table  #
################################

# Pick up death date and transfer out date
comorbid_dt[deathdate, deathdate := i.deathdate, on = "patient_id"]
comorbid_dt[transfer, transfer_date := i.transfer_date, on = "patient_id"]

### Events happening post-death or post-transfer-out (set to not happening)

# Function to set post-death/transfer events to NA
postdeath_dates <- function(date_var) {
  comorbid_dt <- comorbid_dt[, (date_var) := 
              fifelse(get(date_var) > deathdate & !is.na(deathdate),
                             NA, get(date_var))] 
  comorbid_dt <- comorbid_dt[, (date_var) := 
              fifelse(get(date_var) > transfer_date & !is.na(transfer_date),
                             NA, get(date_var))] 
}


# Loop over dates and set post-death events to NA using function above
cdates <- setdiff(grep("date", names(comorbid_dt), value=TRUE),
                  c("deathdate", "transfer_date"))
for (c in cdates) {
  postdeath_dates(c)
}

# Remove date of death and transfer from the dataset
comorbid_dt[,c("deathdate", "transfer_date"):= NULL]




##############################
#  Update medications table  #
##############################

# Pick up death date and transfer-out date
medication[deathdate, deathdate := i.deathdate, on = "patient_id"]
medication[transfer, transfer_date := i.transfer_date, on = "patient_id"]


### Events happening post-death/transfer (set to not happening)

# Function to set post-death/transfer events to NA
postdeath_dates <- function(date_var) {
  medication <- medication[, (date_var) := 
         fifelse(get(date_var) > deathdate & !is.na(deathdate),
                    NA, get(date_var))] 
  medication <- medication[, (date_var) := 
         fifelse(get(date_var) > transfer_date & !is.na(transfer_date),
                    NA, get(date_var))] 
}

# Loop over dates and set post-death events to NA using function above
cdates <- setdiff(grep("date", names(medication), value=TRUE),
                  c("deathdate", "transfer_date"))
for (c in cdates) {
  postdeath_dates(c)
}












#################
#  Some checks  #
#################


# Check date of birth is prior to registration
print(paste("Are all registration dates after date of birth?", 
      all(patient$dob < patient$reg_date)))

# Check date of registration is prior to (or on date of) transfer date
print(paste("Are all transfer dates after registration?",
      all(is.na(patient$transfer_date) | patient$reg_date <= patient$transfer_date)))

# Check registration is before date of death
print(paste("Are all death dates after registration?",
      all(patient$reg_date < patient$deathdate | is.na(patient$deathdate))))


# Check date of birth precedes date of death (in case of death)
print(paste("Are all DOB before death?",
      all(patient$dob[!is.na(patient$deathdate)]
          < patient$deathdate[!is.na(patient$deathdate)])))

# Check registration is before date of death (in case of death)
print(paste("Are all registration dates before death?",
      all(patient$reg_date[!is.na(patient$deathdate)] 
          < patient$deathdate[!is.na(patient$deathdate)])))

# Check transfer date is before death date (where both happen)
print(paste("Are all transfer dates before death?",
all(patient$transfer_date[!is.na(patient$deathdate) & !is.na(patient$transfer_date)] 
    < patient$deathdate[!is.na(patient$deathdate)  & !is.na(patient$transfer_date)])))

# Remove date of death and initial enddate from the dataset
patient[,c("deathdate", "enddate_init"):= NULL]

# Remove missing medications
medication<-medication[!is.na(date), ]

# Remove date of death/transfer from the dataset
medication[,c("deathdate", "transfer_date"):= NULL]
