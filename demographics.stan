data {
  int<lower=0> N; // num response obs
  int<lower=0> K; // num stratifying categories
  int<lower=0> N_cells; // num interaction cells
  int<lower=0> J_max; // max of num strat categories
  int<lower=0> J[K]; // num categories within strat
  vector[N_cells] wt; //
  int lookup[N_cells, K];
  int y[K, J_max];
  int obs[K, J_max];
} transformed data {
  int y_long[sum(J)];
  int obs_long[sum(J)];
  int i;
  real num;
  real denom;
  real tot_rel;
  vector[N_cells] beta_prior;
  
  i = 1;
  for (k in 1:K) {
    for (j in 1:J[k]) {
      y_long[i] = y[k, j];
      obs_long[i] = obs[k, j];
      i = i + 1;
    }
  }
  
  tot_rel = 0;
  for (n in 1:N_cells) {
    beta_prior[n] = 1;
    for (k in 1:K) {
      num = y[k, lookup[n, k]];
      denom = obs[k, lookup[n, k]];
      beta_prior[n] = beta_prior[n] * num/ denom;
      tot_rel = tot_rel + wt[n] / K * num / denom;
    }
  }
  beta_prior = beta_prior / (wt' * beta_prior) * tot_rel;
  
  for (n in 1:N_cells) {
    if (beta_prior[n] > .99) {
      beta_prior[n] = .99;
    }
    beta_prior[n] = beta_prior[n];
  }
} 
parameters {
  vector<lower=0, upper=1>[N_cells] beta;
}
model {
  vector[sum(J)] theta;
  real wt_tot;
  int d;
  beta ~ normal(beta_prior, .05);
  d = 1;
  for (k in 1:K) {
    for (j in 1:J[k]) {
      theta[d] = 0;
      wt_tot = 0;
      for (n in 1:N_cells) {
        if (lookup[n, k] == j) {
          theta[d] = theta[d] + wt[n] * beta[n];
          wt_tot = wt_tot + wt[n];
        }
      }
      theta[d] = theta[d] / wt_tot;
      d = d + 1;
    }
  }
  y_long ~ binomial(obs_long, theta);
}
