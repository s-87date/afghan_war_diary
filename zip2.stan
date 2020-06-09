data {
   int<lower=1>  N1;
   int<lower=1>  N2;
   int<lower=0>  y1[N1];
   int<lower=0>  y2[N2];
}
parameters {
   real<lower=0, upper=1> theta1;
   real<lower=0, upper=1> theta2;
   real<lower=0>          lambda1;
   real<lower=0>          lambda2;
}
model {
// priors
theta1 ~ uniform(0, 1); lambda1 ~ uniform(0, 100); for (i in 1:N1) {
      if (y1[i] == 0) {
// not present
// Bernoulli(0|θ) + Bernoulli(1|θ) * Poisson(0|λ) increment_log_prob(log((1 - theta1) + theta1 * exp(-lambda1))); } else {
// present
// Bernoulli(1|θ) * Poisson(y|λ)
increment_log_prob(bernoulli_log(1, theta1)+ poisson_log(y1[i], lambda1));
} }
theta2 ~ uniform(0, 1); lambda2 ~ uniform(0, 100); for (i in 1:N2) {
      if (y2[i] == 0) {
// not present
// Bernoulli(0|θ) + Bernoulli(1|θ) * Poisson(0|λ) increment_log_prob(log((1 - theta2) + theta2 * exp(-lambda2)));
      } else {
// present
// Bernoulli(1|θ) * Poisson(y|λ)
increment_log_prob(bernoulli_log(1, theta2)+ poisson_log(y2[i], lambda2)); }
}
}
generated quantities{
real delta;
real p_delta;
delta <- lambda1 - lambda2; p_delta <- step(delta);
}
