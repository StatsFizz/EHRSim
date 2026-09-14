################################################################################
#
#   Script:       P0_c_Init_function_medications.R
#
#   Created by:   Elizabeth Williamson 14/04/2025
#
#   Data used:    R object: None
#
#   Data created: R objects: drug_groups, choose_ppi_h2ra, draw_next_presc
#   Other output: None
#
################################################################################
#
#   Purpose:      This script creates medication helper functions and lookup
#                 objects used in prescription simulation.
#
################################################################################
#
#   Packages needed: data.table, lubridate
#
################################################################################

##  Drug groups

drug_groups <- list(
  ppi        = c("omeprazole", "lansoprazole", "pantoprazole", "rabeprazole", "esomeprazole"),
  h2ra       = c("cimetidine", "ranitidine", "famotidine", "nizatidine"),
  aspirin    = "aspirin",
  oac        = "OAC",
  clopidogrel= "clopidogrel",
  nsaid      = "NSAID",
  isteroid   = "inhaled_steroid",
  ssteroid_copd   = "systemic_steroid_copd",
  ssteroid_other = "systemic_steroid_other"
)




## Function to sample a new PPI/H2RA
choose_ppi_h2ra <- function(this_drug_class="None", last_drug="None", presc_date) {
  
  ## Drug classes
  ppi_list  <- c("omeprazole", "lansoprazole", "pantoprazole", "rabeprazole", "esomeprazole")
  h2ra_list <- c("cimetidine", "ranitidine", "famotidine", "nizatidine")
  
  #  PPIs: 
  #   omeprazole   (losec, mepradec, mezzzopram, omeran)	1990+
  #   lansoprazole (zoton) 1996+
  #   pantoprazole (protium, protoloc) 2000+
  #   rabeprazole 	(pariet) 2000+
  #   esomeprazole (emozul, esomeprazole, nexium, ventra) 2005+
  
  
  ## PPI and H2RA class probabilities over time
  prob_ppi_early <- c(0.75, 0.25, 0, 0, 0)
  prob_ppi_mid   <- c(0.7, 0.25, 0.025, 0.025, 0)
  prob_ppi_late  <- c(0.65, 0.25, 0.033, 0.033,0.033)
  prob_h2ra <- c(0.4, 0.45, 0.037, 0.013)
  
  n <- length(presc_date)
  
  u <- runif(n)
  next_drug <- fifelse(this_drug_class == "PPI" | 
                         last_drug %in% ppi_list & u<0.1 | 
                         last_drug %in% h2ra_list & u>0.95,
                       fcase(year(presc_date)<=1995,  
                             sample(ppi_list, prob=prob_ppi_early, replace=TRUE, size=n),
                             year(presc_date) %between% c(1996, 2004),       
                             sample(ppi_list, prob=prob_ppi_mid, replace=TRUE, size=n),
                             year(presc_date)>2004,
                             sample(ppi_list, prob=prob_ppi_late, replace=TRUE, size=n),
                             default=NA_character_),
                       fifelse(this_drug_class == "H2RA" | 
                                 last_drug %in% ppi_list & u>0.95 | 
                                 last_drug %in% h2ra_list & u<0.1,
                               sample(h2ra_list, prob=prob_h2ra, replace=TRUE, size=n),
                               last_drug))
  return(next_drug)
}

# Input: 
# dates <- list(start_date, end_date, pdisc, maxgap_fn, maxgap_fn_args)
# duration <- list(length_presc, pchange, prescdays_fn, prescdays_fn_arg)
# Optional: strength <- list(current_strength, pnewstrength, strength_fn, strength_fn_args)
# Optional: drug_change <- list(nextdrug_fn, nextdrug_fn_args) 


draw_next_presc <- function(dates=list(), duration=list(), drug_change=NULL, strength=NULL) {
  n<- length(dates[[1]])
  
  # Determine next drug, if asked for 
  if (!is.null(drug_change)) {
    next_drug <- do.call(
      drug_change$nextdrug_fn,
      c(drug_change$nextdrug_fn_args))
  }
  
  ### Calculate next prescription date, with some discontinuation
  
  # P6b anchors start_date at the expected end of the current supply. Negative
  # gaps therefore represent early refills, but must not predate the issue that
  # generated the current supply.
  gap <- do.call(
    dates$maxgap_fn,
    c(list(n), dates$maxgap_fn_args)
  )
  gap <- pmax(gap, -as.numeric(duration$length_presc))
  
  # Discontinuation according to provided probability (number) or variable
  next_date <- fifelse(runif(n) > dates$pdisc & dates$start_date!=as.Date("3000-01-01"), 
                       as.Date(dates$start_date + gap),
                       as.Date("3000-01-01"))
  next_date <- fifelse(next_date>=dates$end_date, as.Date("3000-01-01"), next_date)
  rm(gap)
  
  ### Calculate next prescription length
  
  # Draw prescription length, according to provided function
  next_presc_length <-fifelse(runif(n)<duration$pchange, 
                              do.call(
                                duration$prescdays_fn,
                                c(list(n), duration$prescdays_fn_args)
                              ),
                              duration$length_presc
  )
  
  ### Calculate next prescription strength, if asked for
  
  if (!is.null(strength)) {
    next_strength <- fifelse(runif(n)<strength$pnewstrength, 
                             do.call(
                               strength$strength_fn,
                               c(list(n), strength$strength_fn_args)
                             ),
                             strength$current_strength)
  }
  
  
  ### Return outputs
  
  # If already stopped set drug details to "missing"
  next_date <- fifelse(dates$start_date==as.Date("3000-01-01"), 
                       as.Date("3000-01-01"), next_date)
  next_presc_length <- fifelse(dates$start_date==as.Date("3000-01-01"), 
                               NA_real_, next_presc_length)
  if (!is.null(drug_change)) {
    next_drug <- fifelse(dates$start_date==as.Date("3000-01-01"), 
                         NA_character_, next_drug) 
  }
  if (!is.null(strength)) {
    next_strength <- fifelse(dates$start_date==as.Date("3000-01-01"), 
                             NA_character_, next_strength) 
  } 
  rm(n)
  
  out <- NULL
  if (!is.null(drug_change) & !is.null(strength)) {
    out <- list(next_date, next_presc_length, next_drug, next_strength) 
  }
  if (!is.null(drug_change) & is.null(strength)) {
    out <- list(next_date, next_presc_length, next_drug)
  }
  if (is.null(drug_change) & !is.null(strength)) {
    out <- list(next_date, next_presc_length, next_strength)
  }
  if (is.null(drug_change) & is.null(strength)) {
    out <- list(next_date, next_presc_length)
  }
  return(out)
}
