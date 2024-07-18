
functions{
  real rmsd(vector x, vector y){
    return mean(sqrt((x-y).^2));
  }
}
data{
  int P;
  int I;
  int K;
  int J;
  array[P,I] int Y;
  real<lower=0> coef_hyper;
  real<lower=0> sd_hyper;
  vector[P] true_theta;
  row_vector[I] true_tau;
  row_vector[I] true_lambda;
  array[I] int X;
  array[P] int Z;
}
parameters{
  vector[P] u_p_theta;
  matrix[2,I] z_items;
  real<lower=0> sigma_tau;
  real<lower=0> sigma_lambda;
  vector[J-1] beta_j_theta_est;
  row_vector[K-1] beta_k_lambda_est;
  row_vector[K-1] beta_k_tau_est;
  vector[(J-1)*(K-1)] beta_jk_eta_est;
  cholesky_factor_corr[2] Omega_itemsL;
}
model{
  sigma_tau ~ gamma(1, sd_hyper);
  sigma_lambda ~ gamma(1, sd_hyper);
  beta_j_theta_est ~ normal(0, coef_hyper);
  beta_k_lambda_est ~ normal(1, coef_hyper);
  beta_k_tau_est ~ normal(0, coef_hyper);
  beta_jk_eta_est ~ normal(0, coef_hyper);
  Omega_itemsL ~ lkj_corr_cholesky(1.0);
  u_p_theta ~ std_normal();

  to_vector(z_items) ~ std_normal();
  matrix[2,I] u_items = diag_pre_multiply([sigma_lambda, sigma_tau], Omega_itemsL)*z_items;

  matrix[J,K] beta_jk_eta = rep_matrix(0.0, J, K);

  int ind = 1;

  for(j in 2:J){
    for(k in 2:K){
      beta_jk_eta[j,k] += beta_jk_eta_est[ind];
      ind += 1;
    }
  }

  vector[P] theta = append_row(0.0, beta_j_theta_est)[Z] + u_p_theta;
  row_vector[I] lambda = append_col(1.0, beta_k_lambda_est)[X] + u_items[1,];
  row_vector[I] tau = append_col(0.0, beta_k_tau_est)[X] + u_items[2,];
  matrix[P,I] psi = beta_jk_eta[Z,X];

  matrix[P,I] eta = theta*lambda + rep_vector(1.0, P)*tau + psi;

  for(i in 1:I){
    Y[,i] ~ bernoulli_logit(eta[,i]);
  }
}
generated quantities{

  matrix[2,I] u_items = diag_pre_multiply([sigma_lambda, sigma_tau], Omega_itemsL)*z_items;

  vector[P] theta = append_row(0.0, beta_j_theta_est)[Z] + u_p_theta;
  row_vector[I] lambda = append_col(1.0, beta_k_lambda_est)[X] + u_items[1,];
  row_vector[I] tau = append_col(0.0, beta_k_tau_est)[X] + u_items[2,];

  real rmsd_theta = rmsd(theta, true_theta);
  real rmsd_tau = rmsd(tau', true_tau');
  real rmsd_lambda = rmsd(lambda', true_lambda');

  matrix[2,2] Omega_items = multiply_lower_tri_self_transpose(Omega_itemsL);
  matrix[2,2] Sigma_items = multiply_lower_tri_self_transpose(diag_pre_multiply([sigma_lambda, sigma_tau], Omega_itemsL));
}
