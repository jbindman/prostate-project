#rm(list=ls())
#my.wd<-"/Users/ryc/Documents/inhealth/prediction-model/for-julia/IOP"
#setwd(my.wd)


#load libraries
library("splines")
library("dplyr")
#library("gtools")



load("mcmc-posteriors-and-candidates.Rda")
load("data-and-characteristics.Rda")



#define helper functions
expit <- function(x)
	return(exp(x)/(1+exp(x)))



## server functions
get_eta_pred <- function(psa, psa_age, vol, 
						BX, bx_time_int, bx_int_date_num, bx_int_age, bx_num_prev_bx_start,
						RC, rc_age, rc_time, rc_date,
						SURG, surg_num_prev_bx_end, surg_prev_rc){

	Y <- log(psa)
	n_psa <- length(Y)
	age_psa_std <- scale(psa_age, center=psa.age.mean, scale=psa.age.sd)
	Z <- as.matrix(cbind(rep(1,n_psa),  age_psa_std))
	X <- as.matrix(rep(scale(vol, center=vol.mean, scale=vol.sd), n_psa))

	n_bx <- length(BX)
	U <- as.matrix(cbind(rep(1, n_bx), 
			ns(bx_time_int, knots=bx.time.knots, Boundary.knots=bx.time.bknots), 
			ns(bx_int_date_num, knots=bx.date.knots, Boundary.knots=bx.date.bknots), 
			ns(bx_int_age, knots=bx.age.knots, Boundary.knots=bx.age.bknots), 
			ns(bx_num_prev_bx_start, knots=bx.npb.knots, Boundary.knots=bx.npb.bknots) 
			)) 
	dU <- dim(U)[2]	

	n_rc <- length(RC)
	V <- as.matrix(cbind(rep(1, n_rc), ns(rc_time, knots=rc.time.knots, Boundary.knots=rc.time.bknots), ns(rc_date, knots=rc.date.knots, Boundary.knots=rc.date.bknots), scale(rc_age, center=rc.age.mean, scale=rc.age.sd) )) 
	dV <- dim(V)[2]	

	n_surg <- length(SURG)
	W <- as.matrix(cbind(rep(1, n_surg), 
			ns(bx_time_int, knots=surg.time.knots, Boundary.knots=surg.time.bknots), 
			ns(bx_int_date_num, knots=surg.date.knots, Boundary.knots=surg.date.bknots), 
			ns(bx_int_age, knots=surg.age.knots, Boundary.knots=surg.age.bknots), 
			scale(surg_num_prev_bx_end, center=surg.npbe.mean, scale=surg.npbe.sd),
			surg_prev_rc )) 
	dW <- dim(W)[2]	

	
	Zb <- tcrossprod(cands_bvec,Z) #B*n.psa matrix
	Xbeta <- tcrossprod(beta,X)
	mu_psa <- c(t(Zb + Xbeta))
	log_lik_psa_j <- log(dnorm(rep(Y,B), mean=mu_psa, sd=rep(sigma_res, each=n_psa)))
	log_lik_psa <- (data.frame('log_lik_psa_all'=log_lik_psa_j, 'p_ind'=as.factor(rep(1:B, each=n_psa))) %>% group_by(p_ind) %>% dplyr::summarize(sum=sum(log_lik_psa_all)))$sum


	etanu <- rep(nu_BX[,(dU+1)]*cands_eta, each=n_bx)
	Unu <- c(tcrossprod(U, nu_BX[,1:dU]))
	logit_p_bx <- etanu + Unu
	p_bx <- c(t(expit(logit_p_bx)))
	log_lik_bx_j <- log(dbinom(x=rep(BX,times=B), size=1, prob=p_bx))
	log_lik_bx <- (data.frame('log_lik_bx_all'=c(t(log_lik_bx_j)), 'p_ind'=as.factor(rep(1:B, each=n_bx)) ) %>% group_by(p_ind) %>% dplyr::summarize(sum=sum(log_lik_bx_all)) )$sum 

	
	etagamma <- rep(gamma_RC[,(dV+1)]*cands_eta, each=n_rc)
	Vgamma <- c(tcrossprod(V,gamma_RC[,1:dV]))
	logit_p_rc <- etagamma + Vgamma
	p_rc <- c(t(expit(logit_p_rc)))
	log_lik_rc_j <- log(dbinom(x=rep(RC,times=B), size=1, prob=p_rc))
	log_lik_rc <- (data.frame('log_lik_rc_all'=c(t(log_lik_rc_j)), 'p_ind'=as.factor(rep(1:B, each=n_rc)) ) %>% group_by(p_ind) %>% dplyr::summarize(sum=sum(log_lik_rc_all)) )$sum 


	etaomega <- rep(omega_SURG[,(dW+1)]*cands_eta, each=n_surg)
	Womega <- c(tcrossprod(W, omega_SURG[,1:dW]))
	if(n_surg>1){Womegaint <- c( tcrossprod(W[,dW], omega_SURG[,(dW+2)]) ) * rep(cands_eta, each=n_surg)}else{Womegaint <- c( tcrossprod(W[dW], omega_SURG[,(dW+2)]) ) * rep(cands_eta, each=n_surg)}

	logit_p_surg <- etaomega + Womega + Womegaint
	p_surg <- c(t(expit(logit_p_surg)))
	log_lik_surg_j <- log(dbinom(x=rep(SURG,times=B), size=1, prob=p_surg))
	log_lik_surg <- (data.frame('log_lik_surg_all'=c(t(log_lik_surg_j)), 'p_ind'=as.factor(rep(1:B, each=n_surg)) ) %>% group_by(p_ind) %>% dplyr::summarize(sum=sum(log_lik_surg_all)) )$sum 

	
	lik <- exp(log_lik_psa + log_lik_bx + log_lik_rc + log_lik_surg)
	un <- lik
	std <- lik/sum(lik)
	eta.mean <- crossprod(cands_eta, std) 
	return(eta.mean=eta.mean)
}

