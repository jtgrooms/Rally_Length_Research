
// STAN code for Bayesian Rally Length Model

functions {
  
  ## zero one modified log likelihopd
  real zero_truncated_one_modified_lpdf(real x, real oneProb, real geomProb) {
    
    if(x > 1) {
      return(log((1 - oneProb) / (1 - geomProb) * pow(1 - geomProb, x - 1) * geomProb));
    } else if(x == 1) {
      return(log(oneProb));
    } else {
      return log(0);
    }

  } 
  
}
data {
  
  int <lower = 0> N;
  int < lower = 0> rally_data[N];
  
}
parameters {
  
  real <lower = 0, upper = 1> p1;
  real <lower = 0, upper = 1> p;
  
}
model {
  
  ## zero_truncated_one modified model
    
  p1 ~ beta(20,40);
  p ~ beta(10,33); 
  
  for (i in 1:N) {
    rally_data[i] ~ zero_truncated_one_modified(p1, p);
  }
  
}

generated quantities {
  
  vector[N] log_lik;
  for (i in 1:N) log_lik[i] = zero_truncated_one_modified_lpdf(rally_data[i] | p1,p);
  
} 

