data {
  int<lower=0> N; 
  int<lower=0> K;
  int<lower=1> Yr;
  real<lower=0> y[N, Yr]; 
  real<lower=0> obs[N, K, Yr]; 
  real<lower=0> votes[N, Yr];
  real theta_prior[K, Yr];
  real<lower=0> theta_prior_sd[K, Yr];
  real theta_prior_votes[K, Yr];
  real<lower=0> theta_prior_sd_votes[K, Yr];
} transformed data {
  real<lower=0> sigma[N, K, Yr];
  real<lower=0> sigma_sum[N, Yr];
  real<lower=0> sigma_votes[N, K, Yr];
  real<lower=0> sigma_sum_votes[N, Yr];
  int votes_sum[N, Yr];
  for (n in 1:N) {
    for (yr in 1:Yr) {
      sigma_sum[n, yr] = 0;
      sigma_sum_votes[n, yr] = 0;
      for (k in 1:K) {
        sigma[n, k, yr] = sqrt(obs[n, k, yr] 
                               * inv_logit(theta_prior[k, yr]) 
                               * (1 - inv_logit(theta_prior[k, yr])));
        sigma_sum[n, yr] = sigma_sum[n, yr] + square(sigma[n, k, yr]);
        sigma_votes[n, k, yr] = sqrt(obs[n, k, yr] 
                                     * inv_logit(theta_prior[k, yr]) 
                                     * inv_logit(theta_prior_votes[k, yr]) 
                                     * (1 - inv_logit(theta_prior_votes[k, yr])));
        sigma_sum_votes[n, yr] = sigma_sum_votes[n, yr] + square(sigma_votes[n, k, yr]);
        }
    sigma_sum[n, yr] = sqrt(sigma_sum[n, yr]) * 10;
    sigma_sum_votes[n, yr] = sqrt(sigma_sum_votes[n, yr]) * 10;
    }
    
  }
}
parameters {
  real theta[K, Yr];
  real theta_votes[K, Yr];
  real s[N, K, Yr];
  real s_votes[N, K, Yr];
} 
transformed parameters {
  real sims[N, K, Yr];
  real sims_votes[N, K, Yr];
  vector[N] y_hat[Yr];
  vector[N] y_hat_votes[Yr];
  for (n in 1:N) {
    for (yr in 1:Yr) {
      for (k in 1:K) {
          sims[n, k, yr] = obs[n, k, yr] 
                            * inv_logit(s[n, k, yr] * .5 
                                        + theta[k, yr]);
          sims_votes[n, k ,yr] = sims[n, k, yr] 
                                  * inv_logit(s_votes[n, k, yr] 
                                              * .5
                                              +theta_votes[k, yr]);
      }
    y_hat[yr, n] = sum(sims[n, , yr]);
    y_hat_votes[yr, n] = sum(sims_votes[n, , yr]);
    }
  }
}
model {
  to_array_1d(theta) ~ normal(to_array_1d(theta_prior), to_array_1d(theta_prior_sd));
  to_array_1d(theta_votes) ~ normal(to_array_1d(theta_prior_votes), to_array_1d(theta_prior_sd_votes));
  to_array_1d(s) ~ normal(0, 1);
  to_array_1d(s_votes) ~ normal(0, 1);
  
  for (yr in 1:Yr) {
    y[, yr] ~ normal(y_hat[yr], sigma_sum[, yr]);
    votes[, yr] ~ normal(y_hat_votes[yr], sigma_sum_votes[, yr]);
  }
}
generated quantities {
  real theta_adj[K, Yr];
  real theta_votes_adj[K, Yr];
  for (yr in 1:Yr) {
    for (k in 1:K) {
      theta_adj[k, yr] = sum(sims[ , k, yr]) / sum(obs[ , k, yr]);
      theta_votes_adj[k, yr] = sum(sims_votes[ , k, yr]) / sum(sims[ , k, yr]);
    }
  }
}
