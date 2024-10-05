
// STAN code for Bayesian Rally Length Model

functions {
  
  ## zero one modified log likelihopd
  real zero_one_modified_lpdf(real x, real zeroProb, real oneProb, real geomProb) {
    
    if(x > 1) {
      return(log((1 - zeroProb - oneProb) * geomProb * pow(1 - geomProb, x - 2)));
    } else if(x == 1) {
      return(log(oneProb));
    } else if (x == 0) {
      return(log(zeroProb));
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
  
  real <lower = 0, upper = 1> p0;
  real <lower = 0, upper = 1> p1;
  real <lower = 0, upper = 1> p;
  
}
model {
  
  ## zero_one modified model
    
  p0 ~ beta(1,19);
  p1 ~ beta(20,40);
  p ~ beta(10,33); 
  
  for (i in 1:N) {
    rally_data[i] ~ zero_one_modified(p0, p1, p);
  }
  
}

generated quantities {
  
  vector[N] log_lik;
  for (i in 1:N) log_lik[i] = zero_one_modified_lpdf(rally_data[i] | p0,p1,p);
  
} 

