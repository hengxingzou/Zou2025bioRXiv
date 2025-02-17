data {

  int <lower = 0> N; // number of repetitions
  matrix <lower = 0> [2, N] Start; // initial populations
  matrix <lower = 0> [2, N] Final; // final populations

}

transformed data { // transform integer array to matrix for matrix operations
  
  matrix[2, N] Start_t;
  Start_t = to_matrix(Start);

}

parameters {

  vector <lower = 0> [2] lambdas; // lambda is positive
  matrix <lower = 0, upper = 1> [2, 2] alphas; // 
  vector <lower = 0> [2] sigma; // this is only for normal distributed N_t

}

model {

  // priors
  
  // one generation
  
  // lambdas[1] ~ normal (0.715, 0.0715); // prior for lambda_T
  // lambdas[2] ~ normal (0.774, 0.0774); // prior for lambda_F
  
  // two generations
  
  // lambdas[1] ~ normal (1.582, 0.1582); // prior for lambda_T
  // lambdas[2] ~ normal (3.685, 0.3685); // prior for lambda_F
  
  // three generations
  
  lambdas[1] ~ normal (2.651, 0.2651); // prior for lambda_T
  lambdas[2] ~ normal (3.365, 0.3365); // prior for lambda_F

  // model
  
  for (i in 1:N) {
  
    Final[, i] ~ normal(Start_t[, i] .* lambdas .* to_vector(1 - alphas * Start_t[, i]), sigma); // Lotka-Volterra

  }
  
}

