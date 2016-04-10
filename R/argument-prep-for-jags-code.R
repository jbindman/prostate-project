
#Run this script after data-prep-for-jags-code.R. If you want, they can be included in a single function. 

#General description of this script: To run a Gibbs sampler with JAGS, one need to provide values for a number of arguments. Our package will define these arguments in a standard way so that the only thing the user will need to do is take the output from our model and put it into a JAGS function


#### WORKFLOW
### 0. Load libraries
### 1. Define data to be sent to jags function
### 2. Initialize model parameters
### 3. Define parameters to be tracked
### 4. Define other jags settings?
### 5. Write model definition?



### 0. Load libraries
#These packages should be loaded automatically when someone loads our package
library("bayesm")





### 1. Define data to be sent to jags function

#The number of latent classes/ values of true cancer state
K<-2
#We've written this initial code only to handle a binary latent class. This might be something to extend later. (I have a code for ordered latent classes, but I don't know how much more complicated that will be to package)


jags_data<-list(K=K, n=n, eta_data=eta_data, n_eta_known=n_eta_known, n_obs_psa=n_obs_psa, Y=Y, subj_psa=subj_psa, Z=Z_data, X=X_data, d_Z=d_Z, d_X=d_X, I_d_Z=diag(d_Z), RC=RC, n_rc=n_rc, subj_rc=subj_rc, V_RC=V_RC_data, d_V_RC=d_V_RC)

names(pt.data)

### 2. Initialize model parameters
inits <- function(){
		
	p_eta<-rbeta(1,1,1)

	eta_hat<-pt.data$rc[is.na(pt.data$true.gs)]

	xi<-c(min(rlnorm(1),100), min(rlnorm(1),100))
	mu_raw<-as.matrix(cbind(rnorm(d_Z),rnorm(d_Z)))
	Tau_B_raw<-rwishart((d_Z+1),diag(d_Z)*var_vec)$W
	sigma_res<-min(rlnorm(1),1)

	beta<-rnorm(d_X)

	gamma_RC<-rnorm((d_V_RC+1), mean=0, sd=0.1)  #last coefficient is effect of eta=1

	list(p_eta=p_eta, eta_hat=eta_hat, xi=xi, mu_raw=mu_raw, Tau_B_raw=Tau_B_raw, sigma_res=sigma_res, beta=beta,  gamma_RC=gamma_RC)
}



### 3. Define parameters to be tracked
parameters.to.save <- c("p_eta", "eta_hat", "mu_int", "mu_slope", "sigma_int", "sigma_slope", "sigma_res", "rho_int_slope", "cov_int_slope", "b_vec", "beta",  "gamma_RC",  "p_rc")  




### 4. Define other jags settings?
# It is probably easier to let users define these settings themselves.

# change length; burn-in; number thinned; number of chains
#n.iter <- 50000; n.burnin <- 25000; n.thin <- 20; n.chains <- 1 




### 5. Write model definition?
#When running JAGS, it is also necessary to provide a .txt file that defines the model. The most straight forward thing to do at this point is to assume that users want to run the *exact* model I've already defined. In that case, we just need to provide this text file in the R package. Or, if it is not possible to include a .txt file, we can include an R script that writes the .txt file. I will send both to you.


### What this code should enable/ Next steps for users
#This function should return the following objects (that will then serves as arguments for the JAGS function): jags_data, inits, parameters.to.save, a model file ("UNADJ-jags-model.txt").

#Users would then load the library R2jags and execute the following command:
#jags(data=jags_data, inits=inits, parameters.to.save=parameters.to.save, model.file="UNADJ-jags-model.txt", n.chains, n.iter, n.burnin, n.thin)



#I ran the code below to make sure that this code version works!
#ex.jags<-jags(data=jags_data, inits=inits, parameters.to.save=parameters.to.save, model.file="UNADJ-jags-model.txt", n.chains=1, n.iter=50, n.burnin=10, n.thin=5)

#ex.out<-ex.jags$BUGSoutput

#str(ex.out$sims.list)

