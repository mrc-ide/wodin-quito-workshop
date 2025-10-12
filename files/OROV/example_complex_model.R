library(odin)

################################ 
## Example complex OROV model ##
################################

# human equations 

deriv(S_h) <- (- S_h * lambda_h) 

deriv(E_h) <- (S_h * lambda_h) -
              gamma * E_h
  
deriv(I_h_asym) <- (1-theta) * (gamma * E_h) - 
                (psi * sigma * I_h_asym) 

deriv(I_h_sym0) <- theta * (1-pi) * (gamma * E_h) - 
                (sigma * I_h_sym0) 

deriv(I_h_sym1) <- theta * pi * (gamma * E_h) - 
                (omega * I_h_sym1) 

deriv(R_h_temp) <- (omega * I_h_sym1) - 
                   (kappa * R_h_temp)
  
deriv(I_h_sym2) <-  (kappa * R_h_temp) - 
                  (epsilon * sigma * I_h_sym2) 
  
deriv(R_h) <- (psi * sigma * I_h_asym) +
              (sigma * I_h_sym0) +
              (epsilon * sigma * I_h_sym2) 

I_h_total <- I_h_asym + I_h_sym0 + I_h_sym1 + I_h_sym2
I_h_sym_total <- I_h_total - I_h_asym
N_h_total <- S_h + E_h + I_h_total + R_h_temp + R_h
N_v_total <- S_v + E_v + I_v
  
# vector equations 

deriv(S_v) <- e - lambda_v * S_v - mu * S_v 

deriv(E_v) <- lambda_v * S_v - lambda_v_delay * S_v_delay * alpha - mu * E_v 

deriv(I_v) <- lambda_v_delay * S_v_delay * alpha - mu * I_v 


lambda_h = m * b_h * a * I_v # human force of infection 

lambda_v = b_v * a * I_h_total # vector force of infection 

lambda_v_delay = b_v * a * I_h_delay # delayed vector force of infection 


I_h_delay = delay(I_h_total, n) 

S_v_delay = delay(S_v, n) 


# initial conditions 

initial(S_h) <- 1 - I_init_h 

initial(E_h) <- 0

initial(I_h_asym) <- 0

initial(I_h_sym0) <- I_init_h

initial(I_h_sym1) <- 0 

initial(I_h_sym2) <- 0 

initial(R_h_temp) <- 0 

initial(R_h) <- 0


initial(S_v) <- 1 - I_init_v 

initial(E_v) <- 0 

initial(I_v) <- I_init_v 


# parameter values
I_init_h <- 0 # proportion of infectious humans at start of epidemic 

I_init_v <- 0.001 # proportion of infectious vectors at start of epidemic 


gamma <- user(0.167) # gamma is 1/incubation period 
theta <- user(0.85) # theta is the proportion of symptomatic infections
pi <- user(0.5, min=0.3, max=0.7) # pi is the proportion of symptomatic infections who will eventually relapse
sigma <- user(0.2) # human rate to full recovery. 1/onset to recovery.
psi <- user(1, min=0.5, max=1.5) # psi is the modulation of the recovery rate, sigma, for asymptomatic people
epsilon <- user(1, min=0.5, max=1.5) # epsilon is the modulation of the recovery rate, sigma, for people who were recovered and relapsed
kappa <- user(0.125, min=0.036, max=0.5) # kappa is relapse rate 8 (2-28) days
omega <- user(0.2, min= 0.09, max=0.5) # omega is rate to temporary recovery 5 (1-11) days

mu <- user(0.03, min=0.01, 0.07) # vector death rate 
e <- mu # vector emergence rate 
n = 12 # extrinsic incubation period 
alpha <- exp(-mu*n)	# probability a vector survives the latent period 

m = user(10) # density of vectors per person 
a = user(0.3) # biting rate per vector. 1/gonotrophic cycle (1/3days) 
b_h = 0.2 # probability of infection in susceptible human given bite from infectious vector  
b_v = 0.05 # probability of infection in susceptible vector given bite on an infectious human  

# wodin outputs for checks
output(I_h) <- I_h_total
output(pop) <- N_h_total
output(pop_vector) <- S_v+I_v+E_v
output(symptomatic_cases) <- I_h_sym_total
