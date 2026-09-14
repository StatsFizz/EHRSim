################################################################################
#
#   Script:       P1_Simulate_GP.R
#
#   Created by:   Elizabeth Williamson 26/11/2025
#
#   Data used:		None 
#
#   Data created:	R list: gp.
#	  Other output:	None
#
################################################################################
#
#   Purpose:		This script simulates the basic summary statistics for the GP.
#               These will be used to simulate the patients registered at this GP.
#               Summary statistics for the GP are saved as a list. 
#
################################################################################
#
#   Packages needed: truncnorm
#
################################################################################


####################################
#  Store key aspects of practice   #
####################################

# Save key summary statistics in a GP list

gp <- list()
gp$prac_id <- pracid
gp$rel_size <- gp_rel_size

# Dates setting dataset timeframes
gp$last_regyear <- 2025
gp$first_regyear <- 1970 
gp$mean_first_regyear <- 1985  # mean GP registration overall


####################
#  Simulate data   #
####################

# Size of extracted cohort from each GP 
gp$prac_n<- round(gp$rel_size*5000*(rchisq(1, df = 30)/30)/(rchisq(1, df = 20)/20))

# Proportion male 
gp$pmale <- rnorm(1, mean=0.45, sd=0.05)

# Year of first and last registrations at current general practice
gp$max_reg  <- round(gp$last_regyear - rbinom(1, 1, 0.02)*rexp(1, 0.25))
gp$min_reg <- round(rtruncnorm(1, a=gp$first_regyear, b=gp$last_regyear, 
                     mean=gp$mean_first_regyear, sd=15) )
if(gp$max_reg<=gp$min_reg) {
  gp$min_reg <- gp$min_reg - 1
}

# Rate of transferring out
gp$rate_tout <- exp(rnorm(1, mean=log(0.005), sd=0.5))

# Practice-proportion different ethnicities 
# Census groups used: White, Asian, Black, Mixed, Other (plus Undisclosed later)
# Approximately:, on average: White (82%), Asian (9%), Black (4%), 
# Mixed (2.9%), and Other (2.1%)  
peth <- c(0.82, 0.09, 0.04, 0.029, 0.021)

peth <- c(0.85, 0.05, 0.04, 0.03, 0.03)
eth_draw <- sample(seq_along(peth), size = 100, replace = TRUE, prob = peth)
prob_eth <- tabulate(eth_draw, nbins = length(peth)) / length(eth_draw)

gp$peth_white <- prob_eth[1]
gp$peth_asian <- prob_eth[2]
gp$peth_black <- prob_eth[3]
gp$peth_mixed <- prob_eth[4]
gp$peth_other <- prob_eth[5]
rm(peth, prob_eth)



# Practice level deprivation (1:5, higher = more deprived)
gp$prac_deprivation <- sample(1:5,1)

