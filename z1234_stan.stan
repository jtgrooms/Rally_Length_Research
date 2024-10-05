
// STAN code for Bayesian Rally Length Model

functions {
  
  ## zero one two three modified log likelihopd
  real zero_one_two_three_four_modified_lpdf(real x, real zeroProb, real oneProb, real twoProb, real threeProb, real fourProb, real geomProb) {
    
    if(x > 4) {
      return(log((1 - zeroProb - oneProb - twoProb - threeProb - fourProb) * geomProb * pow(1 - geomProb, x - 5)));
    } else if (x == 4) {
      return(log(fourProb));
    } else if (x == 3) {
      return(log(threeProb));
    } else if (x == 2) {
      return(log(twoProb));
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
  real <lower = 0, upper = 1> p2;
  real <lower = 0, upper = 1> p3;
  real <lower = 0, upper = 1> p4;
  real <lower = 0, upper = 1> p;
  
}
model {
    
  p0 ~ beta(1,19);
  p1 ~ beta(20,40);
  p2 ~ beta(20, 46);
  p3 ~ beta(20, 50);
  p4 ~ beta(20, 55);
  p ~ beta(10,33); 
  
  for (i in 1:N) {
    rally_data[i] ~ zero_one_two_three_four_modified(p0, p1, p2, p3, p4, p);
  }
  
}

generated quantities {
  
  vector[N] log_lik;
  for (i in 1:N) log_lik[i] = zero_one_two_three_four_modified_lpdf(rally_data[i] | p0,p1,p2,p3,p4,p);
  
} 

