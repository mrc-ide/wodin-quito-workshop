# initial conditions
initial(S) <- N - I_init
initial(E) <- 0
initial(I) <- I_init
initial(R) <- 0

# equations
deriv(S) <- -beta * S * I / N
deriv(E) <- beta * S * I / N - gamma * E
deriv(I) <- gamma * E - sigma * I
deriv(R) <- sigma * I

# parameter values
R_0 <- user(1.5)
L <- user(1)
D <- user(1)
I_init <- 1 # default value
N <- 370

# convert parameters
gamma <- 1 / L
sigma <- 1 / D
beta <- R_0 * sigma

#Output
output(onset) <- if(t == 0) I_init else gamma * E
