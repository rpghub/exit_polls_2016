data {
  int<lower=0> N; 
  int<lower=0> K;
  int<lower=1> Yr;
  int<lower=0> y[N, Yr]; 
  int<lower=0> obs[N, K, Yr]; 
  int<lower=0> votes[N, Yr];
  real<lower=0, upper=1> theta_prior[K, Yr];
  real<lower=0> theta_prior_sd[K, Yr];
  real<lower=0, upper=1> theta_prior_votes[K, Yr];
  real<lower=0> theta_prior_sd_votes[K, Yr];
} transformed data {
  real<lower=0> sigma[N, K, Yr];
  real<lower=0> sigma_sum[N, Yr];
  real<lower=0> sigma_votes[N, K, Yr];
  real<lower=0> sigma_sum_votes[N, Yr];
  int obs_sum[N, Yr];
  int votes_sum[N, Yr];
  for (n in 1:N) {
    for (yr in 1:Yr) {
      sigma_sum[n, yr] = 0;
      sigma_sum_votes[n, yr] = 0;
      for (k in 1:K) {
        sigma[n, k, yr] = sqrt(obs[n, k, yr] * theta_prior[k, yr] * (1 - theta_prior[k, yr]));
        sigma_sum[n, yr] = sigma_sum[n, yr] + square(sigma[n, k, yr]);
        sigma_votes[n, k, yr] = sqrt(obs[n, k, yr] * theta_prior[k, yr] * 
                                     theta_prior_votes[k, yr] *
                                     (1 - theta_prior_votes[k, yr]));
        sigma_sum_votes[n, yr] = sigma_sum_votes[n, yr] + square(sigma_votes[n, k, yr]);
        }
    sigma_sum[n, yr] = sqrt(sigma_sum[n, yr]);
    sigma_sum_votes[n, yr] = sqrt(sigma_sum_votes[n, yr]);
    obs_sum[n, yr] = sum(obs[n, , yr]);
    }
    
  }
}
parameters {
  real<lower=0, upper=1> theta[K, Yr];
  real<lower=0, upper=1> theta_votes[K, Yr];
  real<lower=.5, upper=1.5> alpha[N, Yr];
  real<lower=.5, upper=1.5> alpha_votes[N, Yr];
  //real<lower=0, upper= 2> sigma_alpha;
  //real<lower=0, upper= 2> sigma_alpha_votes;
  real s[N, K, Yr];
  real s_votes[N, K, Yr];
} 
transformed parameters {
  real sims[N, K, Yr];
  real sims_votes[N, K, Yr];
  for (n in 1:N) {
    for (k in 1:K) {
      for (yr in 1:Yr) {
        if (theta[k, yr] * alpha[n, yr] > 1) 
          sims[n, k, yr] = s[n, k, yr] * sigma[n, k, yr] +  obs[n, k, yr] ;
        else 
          sims[n, k, yr] = s[n, k, yr] * sigma[n, k, yr] +  obs[n, k, yr] * theta[k, yr] * alpha[n, yr];
        if (theta_votes[k, yr] * alpha_votes[n, yr] > 1)
          sims_votes[n, k ,yr] = s_votes[n, k, yr] * sigma_votes[n, k, yr] + sims[n, k, yr];
        else 
          sims_votes[n, k ,yr] = s_votes[n, k, yr] * sigma_votes[n, k, yr] + 
                                    sims[n, k, yr] * theta_votes[k, yr] * alpha_votes[n, yr];
      } 
    }
  }
}
model {
  real y_hat;
  real y_hat_votes;
  //sigma_alpha ~ normal(.01, .3);
  //sigma_alpha_votes ~ normal(.01, .3);
  to_array_1d(theta) ~ normal(to_array_1d(theta_prior), to_array_1d(theta_prior_sd));
  to_array_1d(theta_votes) ~ normal(to_array_1d(theta_prior_votes), to_array_1d(theta_prior_sd_votes));
  to_array_1d(alpha) ~ normal(1, .1);
  to_array_1d(alpha_votes) ~ normal(1, .1);
  to_array_1d(s) ~ normal(0, 1);
  to_array_1d(s_votes) ~ normal(0, 1);
  
  for (n in 1:N) {
    for (yr in 1:Yr) {
      y_hat = sum(sims[n, , yr]); 
      y_hat_votes = sum(sims_votes[n, , yr]); 
      y[n, yr] ~ normal(y_hat, sigma_sum[n, yr]); 
      votes[n, yr] ~ normal(y_hat_votes, sigma_sum_votes[n, yr]);
    }
  }
}
generated quantities {
  real y_fit[N, K, Yr];
  real y_fit_votes[N, K, Yr];
  real theta_adj[K, Yr];
  real theta_votes_adj[K, Yr];
  for (n in 1:N) {
    for (yr in 1:Yr) {
      for (k in 1:K) {
        if (theta[k, yr] * alpha[n, yr] > 1)
          y_fit[n, k, yr] = obs[n, k, yr];
        else
          y_fit[n, k, yr] = theta[k, yr] * alpha[n, yr] * obs[n, k, yr];
        if (theta_votes[k, yr] * alpha_votes[n, yr] > 1)
          y_fit_votes[n, k, yr] = y_fit[n, k, yr];
        else
          y_fit_votes[n, k, yr] = y_fit[n, k, yr] * theta_votes[k, yr] * alpha_votes[n, yr];
      }
    }
  }
  for (yr in 1:Yr) {
    for (k in 1:K) {
      theta_adj[k, yr] = sum(y_fit[ , k, yr]) / sum(obs[ , k, yr]);
      theta_votes_adj[k, yr] = sum(y_fit_votes[ , k, yr]) / sum(y_fit[ , k, yr]);
    }
  }
}
