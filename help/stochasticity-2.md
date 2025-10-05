## Example 1: Stochastic birth and death model

This model includes births and deaths at per-capita rates $r$ and $mu$ per week, respectively. The ODE for $N$ is made up of the difference between the mean birth rate and the mean death rate:

$$ \frac{dN}{dt}=rN-\mu N $$

The difference $(r-\mu)$ is the net birth rate and the solution is either exponential growth or decline i.e.,

$$ N(t) =N(0) \exp((r-\mu)t) $$

If births exceed deaths ($r > \mu$) the population grows. If deaths exceed births ($mu > r$) the population shrinks.

In the stochastic simulation we have to be more careful. Here, we are dealing with individuals, each of which has a probability of reproducing or dying at each time-step, $dt$, over the duration of the model run. However, it can’t die and then give birth! 

When two dependent events can happen to any given individual in a compartment of a stochastic models they are known as *competing hazards*. To calculate the number of births and deaths in a short time, we first have to calculate the total number of events (births and deaths) that will happen in the compartment population subject to competing hazards at time-step $dt$:

$$ \mbox{Births or deaths in }dt, n \sim \mathrm{Binomial}(N,(r + \mu)dt) $$

We then decide which of those events will be births and which will be deaths:

$$ \mbox{Deaths in }dt \sim \mathrm{Binomial}(n,\mu / (r + \mu)) $$
