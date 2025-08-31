# Power simulation for biomarker email: parp.R

# Set parameters
N = 220             # total sample size
No = round(.80*N)   # sample size in overexpressed group
Nn = round(.20*N)   # sample size in non-expressed group
Po = .10            # response rate in overexpressed group
Pn = .25            # response rate in non-expressed group

# Simulate one run of the experiment
Ro = rbinom(1,size=No,prob=Po)   # simulate responders in overexp group
Rn = rbinom(1,size=Nn,prob=Pn)   # simulate responders in non-exp group
prop.test(x=c(Ro,Rn),n=c(No,Nn),correct=F)  # chi-square test
prop.test(x=c(Ro,Rn),n=c(No,Nn),correct=T)  # adds continuity correction
prop.test(x=c(Ro,Rn),n=c(No,Nn),correct=F,alternative="less")   # one-sided
prop.test(x=c(Ro,Rn),n=c(No,Nn),correct=T,alternative="less")

# Simulate many runs of the experiment and calculate power
Ro = rbinom(1000,size=No,prob=Po)   # simulate responders in overexp group
Rn = rbinom(1000,size=Nn,prob=Pn)   # simulate responders in non-exp group
pvalnocor=rep(NA,1000)
pvalcor=rep(NA,1000)
for (i in 1:1000)  {
  pvalnocor[i]=prop.test(x=c(Ro[i],Rn[i]),n=c(No,Nn),correct=F)$p.value
  pvalcor[i]=prop.test(x=c(Ro[i],Rn[i]),n=c(No,Nn),correct=T)$p.value  }
sum(pvalnocor<.05)/1000    # power without continuity correction
sum(pvalcor<.05)/1000      # power with continuity correction

# Now that we can find power, what are the issues to consider:
# - alter proportion in overexpressed group and total sample size
# - decide if continuity correction needed
# - one-sided vs. two-sided
# - compare with Fisher?s exact test

