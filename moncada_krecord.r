## Frank Firke
## 9/5/2018

## PA estimates centered on 90, which is normal for PECOTA as of writing
## Probs come from 

PA <- seq(75,105,5)
probs <- c(.305,.319,.337)

# results_tie -- probability of tying or exceeding 223 Ks
# results_total -- probability of exceeding 223 Ks
# results_wsrate -- probability of exceeding CWS record for qualified K% (Dunn, 2012)
# results_wsrate2 -- probability of exceeding CWS record for adjusted qualified K% (Dunn, 2011, adding 6 PA with no Ks)
# results_allrate -- probability of exceeding MLB record for qualified K% (C Davis, 2017)

results_tie <- matrix(nrow=7,ncol=3,dimnames=list(PA,probs))
results_total <- matrix(nrow=7,ncol=3,dimnames=list(PA,probs))
results_wsrate <- matrix(nrow=7,ncol=3,dimnames=list(PA,probs))
results_wsrate2 <- matrix(nrow=7,ncol=3,dimnames=list(PA,probs))
results_allrate <- matrix(nrow=7,ncol=3,dimnames=list(PA,probs))

for (k in 1:length(PA)){
	for (j in 1:length(probs))
	{
		results_tie[k,j] <- 1-pbinom(33,PA[k],probs[j])
		results_total[k,j] <- 1-pbinom(34,PA[k],probs[j])
		results_wsrate[k,j] <- 1-pbinom(ceiling(.342*(563+PA[k])-190),PA[k],probs[j])
		results_wsrate2[k,j] <- 1-pbinom(ceiling(.353*(563+PA[k])-190),PA[k],probs[j])
		results_allrate[k,j] <- 1-pbinom(ceiling(.372*(563+PA[k])-190),PA[k],probs[j])
	}
}

round(results_tie,3)
round(results_total,3)
round(results_wsrate,3)
round(results_wsrate2,3)
round(results_allrate,3)



