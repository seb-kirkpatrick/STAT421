
data {
  int<lower=0> n;
  int y[n];
  real mu;
}


parameters {
  real<lower=0> lambda;
}


model {
  y ~ poisson(lambda);
  lambda ~ exponential(mu);
}

