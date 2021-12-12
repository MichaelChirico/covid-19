# PACKAGES
require(deSolve)
require(reshape2)
require(ggplot2)
require(tidyverse)
require(data.table)
library(randomcoloR)

contact_matrix_rivm <- read.table("C:/Users/ZELST007/Downloads/2020.05.18.20101501-1.tsv",sep="\t",header=T)
contact_matrix_rivm <- contact_matrix_rivm %>%
  filter(survey == "baseline") %>%
  filter(contact_type == "all") %>%
  mutate(m_est = m_est*0.75)

contact_matrix_rivm <- contact_matrix_rivm[,c(3:5)]
library(reshape2)
contact_matrix <- acast(contact_matrix_rivm,part_age~cont_age, value.var="m_est")

omicron.infectiousness <- 1.0


# Parameters
parameters <- c(b = 0.05*omicron.infectiousness,     # the probability of infection per contact is 5%
                contact_matrix = contact_matrix,   # the age-specific average number of daily contacts (defined above)
                gamma_e = 1/4,
                gamma = 1/5,  # the rate of recovery is 1/5 per day
                waning = 1/180,
                births = 1/70/365,
                deaths = 1/70/365)
# Run simulation for 3 months
times <- seq(from = 0, to = 500, by = 1)

# MODEL FUNCTION
sir_age_model <- function(time, state, parameters) {  
  
  with(as.list(parameters), {
    
    n_agegroups <- 10                                 # number of age groups
    S <- state[1:n_agegroups]                        # assign to S the first 3 numbers in the initial_state_values vector
    E <- state[(n_agegroups+1):(2*n_agegroups)]
    I <- state[(2*n_agegroups+1):(3*n_agegroups)]      # assign to I numbers 4 to 6 in the initial_state_values vector
    R <- state[(3*n_agegroups+1):(4*n_agegroups)]    # assign to R numbers 7 to 9 in the initial_state_values vector
    
    N <- S+E+I+R     # people in S, I and R are added separately by age group, so N is also a vector of length 3
    
    # Defining the force of infection
    rate_b <- b
    
    # Force of infection acting on susceptible children
    lambda <- rate_b * contact_matrix %*% as.matrix(I/N)
    new_infections <- lambda * S
    # %*% is used to multiply matrices in R
    # the lambda vector contains the forces of infection for children, adults and the elderly (length 3)
    
    # The differential equations
    # Rate of change in children:
    dS <- -new_infections + waning*R
    dE <- new_infections - gamma_e * E
    dI <- gamma_e * E - gamma * I
    dR <- gamma * I - waning*R  
    
    
    # Output
    return(list(c(dS, dE, dI, dR, new_infections))) 
  })
}

# 1) Vaccinating only children
# There are more doses of vaccine than children in the population, so the vaccine coverage will be 100% in children
# and 0% in the other groups as per the question


# Population size in total and for each age group:
N1 <- 857626 ## 0-5
N2 <- 899827 ## 6-10
N3 <- 1986013 ## 11-20
N4 <- 2240428 ## 21-30
N5 <- 2180575 ## 31-40
N6 <- 2166062 ## 41-50
N7 <- 2549688 ## 51-60
N8 <- 2141439 ## 61-70
N9 <- 1615096 ## 71-80
N10 <- 838661 ## 80+


vacc_cov1 <- 0                  # vaccine coverage in children
vacc_cov2 <- 0                  # vaccine coverage in adults
vacc_cov3 <- 0.60                  # vaccine coverage in the elderly
vacc_cov4 <- 0.72                  # vaccine coverage in children
vacc_cov5 <- 0.74                  # vaccine coverage in adults
vacc_cov6 <- 0.82                  # vaccine coverage in the elderly
vacc_cov7 <- 0.88                  # vaccine coverage in children
vacc_cov8 <- 0.89                  # vaccine coverage in adults
vacc_cov9 <- 0.93                  # vaccine coverage in the elderly
vacc_cov10 <- 0.90                  # vaccine coverage in the elderly


omikron.vac.escape <- 0.7

vacc_eff1 <- 0.90*omikron.vac.escape #vaccine efficacy against symptomatic disease
vacc_eff2 <- 0.9*omikron.vac.escape

# Effective vaccine coverage for each age group:
p1 <- vacc_cov1 * vacc_eff1
p2 <- vacc_cov2 * vacc_eff1
p3 <- vacc_cov3 * vacc_eff1
p4 <- vacc_cov4 * vacc_eff1
p5 <- vacc_cov5 * vacc_eff1
p6 <- vacc_cov6 * vacc_eff1
p7 <- vacc_cov7 * vacc_eff1
p8 <- vacc_cov8 * vacc_eff1
p9 <- vacc_cov9 * vacc_eff1
p10 <- vacc_cov10 * vacc_eff1


initial_state_values <- c(S1 = N1-p1*N1 - (N1-(p1*N1))*0.2,
                          S2 = N2-p2*N2 - (N2-(p2*N2))*0.2,  
                          S3 = N3-p3*N3 - (N3-(p3*N3))*0.2,
                          S4 = N4-p4*N4 - (N4-(p4*N4))*0.2,
                          S5 = N5-p5*N5 - (N5-(p5*N5))*0.2,
                          S6 = N6-p6*N6 - (N6-(p6*N6))*0.2,
                          S7 = N7-p7*N7 - (N7-(p7*N7))*0.2,
                          S8 = N8-p8*N8 - (N8-(p8*N8))*0.2,
                          S9 = N9-p9*N9 - (N9-(p9*N9))*0.2,
                          S10 = N10-p10*N10 - (N10-(p10*N10))*0.2,
                          E1 = 0,
                          E2 = 0,  
                          E3 = 0,
                          E4 = 0,
                          E5 = 0,
                          E6 = 0,
                          E7 = 0,
                          E8 = 0,
                          E9 = 0,
                          E10 = 0,
                          I1 = 10000,
                          I2 = 20000,  
                          I3 = 20000,
                          I4 = 15000,
                          I5 = 10000,
                          I6 = 10000,
                          I7 = 7500,
                          I8 = 5000,
                          I9 = 2500,
                          I10 = 2500,
                          R1 = p1*N1 + (N1-(p1*N1))*0.2,
                          R2 = p2*N2 + (N2-(p2*N2))*0.2,  
                          R3 = p3*N3 + (N3-(p3*N3))*0.2,
                          R4 = p4*N4 + (N4-(p4*N4))*0.2,
                          R5 = p5*N5 + (N5-(p5*N5))*0.2,
                          R6 = p6*N6 + (N6-(p6*N6))*0.2,
                          R7 = p7*N7 + (N7-(p7*N7))*0.2,
                          R8 = p8*N8 + (N8-(p8*N8))*0.2,
                          R9 = p9*N9 + (N9-(p9*N9))*0.2,
                          R10 = p10*N10 + (N10-(p10*N10))*0.2,
                          cum_incid1 = 0,
                          cum_incid2 = 0,
                          cum_incid3 = 0,
                          cum_incid4 = 0,
                          cum_incid5 = 0,
                          cum_incid6 = 0,
                          cum_incid7 = 0,
                          cum_incid8 = 0,
                          cum_incid9 = 0,
                          cum_incid10 = 0)

# Run model output
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_age_model,
                            parms = parameters))


# Turn output into long format
output_long <- melt(as.data.frame(output), id = "time") 


output_long <- output_long %>%
  mutate(group = substr(variable, 1,9)) %>%
  filter(group == "cum_incid") %>%
  group_by(variable) %>%
  mutate(incident_infections = c(0,diff(value, lag = 1)))



n <- 10
palette <- distinctColorPalette(n)


# Plot number of people in all compartments over time
output_long %>%
  #filter(str_detect(variable, "I1")) %>%
  #filter(variable == "I9" | variable == "I10") %>%
  ggplot(aes(x = time, y = incident_infections, colour = variable, group = variable)) +  
  geom_line(lwd = 1.0) +
  labs(x = "Tijd (dagen)",
       y = "Aantal infecties (per dag)",
       title = "Aantal infecties per dag (Omicron variant, 1x boosteren)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(labels = c("0-5","6-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","80+"), values = palette) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))


 +
  


test.maxincidence <- aggregate(incident_infections ~ time, data = output_long, FUN = sum)
max(test.maxincidence$incident_infections)



output$I <- rowSums(output[,c(22:31)])


## Plot number of people in all compartments over time
output %>%
  ggplot(aes(x = time, y = I)) +  
  geom_line(lwd = 1.0) +
  labs(x = "Tijd (dagen)",
       y = "Prevalentie (aantal besmettelijken)",
       title = "Aantal besmettelijken over tijd") +
  scale_y_continuous(expand = c(0, 200), limits = c(0, NA))


output_long <- output_long %>%
  mutate(group = substr(variable, 1,9)) %>%
  filter(group == "cum_incid") %>%
  group_by(variable) %>%
  mutate(incident_infections = c(0,diff(value, lag = 1)))


## Calculate hospitalization rate

output_wide_hosp <- output_long[,c("time","variable","incident_infections")]

output_wide_hosp <- output_wide_hosp %>%
  dcast(time ~ variable)

ve_hospitalization_adult <- 0.1/0.9
ve_hospitalization_elderly <- 0.3/0.9


vax.pos_1 <- 0.000276014
vax.pos_2 <- 0.000276014
vax.pos_3 <- 0.13504902
vax.pos_4 <- 0.432401379
vax.pos_5 <- 0.518927445
vax.pos_6 <- 0.659158662
vax.pos_7 <- 0.738579828
vax.pos_8 <- 0.839588919
vax.pos_9 <- 0.885622662
vax.pos_10 <- 0.881656805





output_wide_hosp <- output_wide_hosp %>%
  mutate(H1 = cum_incid1*0.0006*2) %>%
  mutate(H2 = cum_incid2*0.0006*2) %>%
  mutate(H3 = cum_incid3*0.0006*2*ve_hospitalization_adult*vax.pos_3 + 0.0006*(1-vax.pos_3)) %>%
  mutate(H4 = cum_incid4*0.0009*2*ve_hospitalization_adult*vax.pos_4 + 0.0009*(1-vax.pos_4)) %>%
  mutate(H5 = cum_incid5*0.00225*2*ve_hospitalization_adult*vax.pos_5 + 0.00225*(1-vax.pos_5)) %>%
  mutate(H6 = cum_incid6*0.006*2*ve_hospitalization_adult*vax.pos_6 + 0.006*(1-vax.pos_6)) %>%
  mutate(H7 = cum_incid7*0.0135*2*ve_hospitalization_adult*vax.pos_7 + 0.0135*(1-vax.pos_7)) %>%
  mutate(H8 = cum_incid8*0.016*2*ve_hospitalization_elderly*vax.pos_8 + 0.016*(1-vax.pos_8)) %>%
  mutate(H9 = cum_incid9*0.03*2*ve_hospitalization_elderly*vax.pos_9 + 0.03*(1-vax.pos_9)) %>%
  mutate(H10 = cum_incid10*0.047*2*ve_hospitalization_elderly*vax.pos_10 + 0.047*(1-vax.pos_10))



output_wide_hosp$H_total <- rowSums(output_wide_hosp[,c(12:21)])


## Plot number of people in all compartments over time
output_wide_hosp %>%
  ggplot(aes(x = time, y = H_total)) +  
  geom_line(lwd = 1.0) +
  labs(x = "Tijd (dagen)",
       y = "Aantal opnames (per dag)",
       title = "Aantal opnames per dag (Omicron variant, 1x boosteren)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

max(output_wide_hosp$H_total)


