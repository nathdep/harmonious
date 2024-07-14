
functions{
  real rmsd(vector x, vector y){
    return mean(sqrt((x-y).^2));
  }
}
data{
  int P;
  int I;
  int J;
  int K;
  array[I] int X;
  array[P] int Z;
  array[P,I] int Y;
  real<lower=0> coef_hyper;
  real<lower=0> sd_hyper;
  vector[P] true_theta;
  row_vector[I] true_tau;
  row_vector[I] true_lambda;
  vector[P] sum_score;
}
transformed data{
  matrix[2,2] Omega_itemsL = diag_matrix(rep_vector(1.0, 2));
}
parameters{
  matrix[2,I] z_items;
  real<lower=0> sigma_tau;
  real<lower=0> sigma_lambda;
  row_vector[K-1] beta_k_tau_est;
  row_vector[K-1] beta_k_lambda_est;
  matrix[J-1, K-1] beta_jk_eta_est;
}
model{
  sigma_lambda ~ gamma(1, sd_hyper);
  sigma_tau ~ gamma(1, sd_hyper);
  to_vector(z_items) ~ std_normal();
  beta_k_tau_est ~ normal(0, coef_hyper);
  beta_k_lambda_est ~ normal(1, coef_hyper);
  to_vector(beta_jk_eta_est) ~ normal(0, coef_hyper);

  matrix[J,K] beta_jk_eta = rep_matrix(0.0, J, K);

  for(j in 2:J){
    for(k in 2:K){
      beta_jk_eta[j,k] += beta_jk_eta_est[j-1,k-1];
    }
  }

  matrix[2,I] u_items = diag_pre_multiply([sigma_lambda, sigma_tau], Omega_itemsL)*z_items;

  row_vector[I] lambda = append_col(1.0, beta_k_lambda_est)[X] + u_items[1,];
  row_vector[I] tau = append_col(0.0, beta_k_tau_est)[X] + u_items[2,];
  matrix[P,I] eta = sum_score*lambda + rep_vector(1.0, P)*tau + beta_jk_eta[Z,X];

  for(i in 1:I){
    Y[,i] ~ bernoulli_logit(eta[,i]);
  }
}
generated quantities{

  matrix[2,I] u_items = diag_pre_multiply([sigma_lambda, sigma_tau], Omega_itemsL)*z_items;

  row_vector[I] lambda = append_col(1.0, beta_k_lambda_est)[X] + u_items[1,];
  row_vector[I] tau = append_col(0.0, beta_k_tau_est)[X] + u_items[2,];

  real rmsd_tau=rmsd(tau', true_tau');
  real rmsd_lambda=rmsd(lambda', true_lambda');

  real mean_bias_tau = mean(tau - true_tau);
  real mean_bias_lambda = mean(lambda - true_lambda);

  real sd_bias_tau=sd(tau-true_tau);
  real sd_bias_lambda=sd(lambda-true_lambda);

  matrix[2,2] Omega_items = multiply_lower_tri_self_transpose(Omega_itemsL);
  matrix[2,2] Sigma_items = multiply_lower_tri_self_transpose(diag_pre_multiply([sigma_lambda, sigma_tau], Omega_itemsL));
}
