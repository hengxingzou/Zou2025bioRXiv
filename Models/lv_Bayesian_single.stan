data {
  
  int <lower = 0> N; // number of repetitions
  vector <lower = 0> [N] Start; // initial populations
  vector <lower = 0> [N] Final; // final populations
  
}

parameters {
  
  real <lower = 0> lambda; // lambda is positive
  real <lower = 0, upper = 1> alpha_intra;
  real <lower = 0> sigma;
  
}

model {
  
  for (i in 1:N) {
    
    Final[i] ~ normal(Start[i] * lambda * (1 - alpha_intra * Start[i]), sigma); // Lotka-Volterra
  
}