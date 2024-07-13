
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
  array[P] int Z;
  array[I] int X;
  array[P,I] int Y;
  real<lower=0> coef_hyper;
  real<lower=0> sd_hyper;
  vector[P] true_theta;
  row_vector[I] true_tau;
  row_vector[I] true_lambda;
}
parameters{
  vector[P] u_p_theta;
  row_vector[I] u_i_tau;
  row_vector[I] u_i_lambda;
  real<lower=0> sigma_tau;
  real<lower=0> sigma_lambda;
  vector[J-1] beta_j_theta_est;
  row_vector[K-1] beta_k_tau_est;
  row_vector[K-1] beta_k_lambda_est;
  matrix[J-1, K-1] beta_jk_eta_est;
}
model{
  u_p_theta ~ std_normal();
  sigma_lambda ~ gamma(1, sd_hyper);
  sigma_tau ~ gamma(1, sd_hyper);
  u_i_lambda ~ normal(0, sigma_lambda);
  u_i_tau ~ normal(0, sigma_tau);
  beta_j_theta_est ~ normal(0, coef_hyper);
  beta_k_tau_est ~ normal(0, coef_hyper);
  beta_k_lambda_est ~ normal(1, coef_hyper);
  to_vector(beta_jk_eta_est) ~ normal(0, coef_hyper);

  matrix[J,K] beta_jk_eta = rep_matrix(0.0, J, K);

  for(j in 2:J){
    for(k in 2:K){
      beta_jk_eta[j,k] += beta_jk_eta_est[j-1,k-1];
    }
  }

  vector[P] theta = append_row(0.0, beta_j_theta_est)[Z] + u_p_theta;
  row_vector[I] tau = append_col(0.0, beta_k_tau_est)[X] + u_i_tau;
  row_vector[I] lambda = append_col(1.0, beta_k_lambda_est)[X] + u_i_lambda;
  matrix[P,I] eta = theta*lambda + rep_vector(1.0, P)*tau + beta_jk_eta[Z,X];

  for(i in 1:I){
    Y[,i] ~ bernoulli_logit(eta[,i]);
  }
}
generated quantities{

  vector[P] theta = append_row(0.0, beta_j_theta_est)[Z] + u_p_theta;
  row_vector[I] tau = append_col(0.0, beta_k_tau_est)[X] + u_i_tau;
  row_vector[I] lambda = append_col(1.0, beta_k_lambda_est)[X] + u_i_lambda;

  real rmsd_theta=rmsd(theta, true_theta);
  real rmsd_tau=rmsd(tau', true_tau');
  real rmsd_lambda=rmsd(lambda', true_lambda');

  real mean_bias_theta = mean(theta - true_theta);
  real mean_bias_tau = mean(tau - true_tau);
  real mean_bias_lambda = mean(lambda - true_lambda);

  real sd_bias_theta=sd(theta-true_theta);
  real sd_bias_tau=sd(tau-true_tau);
  real sd_bias_lambda=sd(lambda-true_lambda);
}
