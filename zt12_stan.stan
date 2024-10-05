
// STAN code for Bayesian Rally Length Model

functions {
  
  ## zero truncated one two modified log likelihopd
  real zero_truncated_one_two_modified_lpdf(real x, real oneProb, real twoProb, real geomProb) {
    
    if(x > 2) {
      return(log((1 - oneProb - twoProb) / pow(1 - geomProb, 2) * pow(1 - geomProb, x - 1) * geomProb));
    } else if (x == 2) {
      return(log(twoProb));
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
  real <lower = 0, upper = 1> p2;
  real <lower = 0, upper = 1> p;
  
}
model {
    
  p1 ~ beta(20,40);
  p2 ~ beta(20, 46);
  p ~ beta(10,33); 
  
  for (i in 1:N) {
    rally_data[i] ~ zero_truncated_one_two_modified(p1, p2, p);
  }
  
}

generated quantities {
  
  vector[N] log_lik;
  for (i in 1:N) log_lik[i] = zero_truncated_one_two_modified_lpdf(rally_data[i] | p1,p2,p);
  
} 

