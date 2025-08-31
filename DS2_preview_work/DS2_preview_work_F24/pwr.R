# Examples using pwr() function in R

library(pwr)  # might have to install first

# gives sample size for comparing 2 means, assuming its equal in
#   both groups.  Note d = (mu1 - mu2) / sigma
pwr.t.test(d = .50, sig.level = .05, power = .80,
           type = "two.sample", alternative = "two.sided")

# gives power for comparing 2 proportions with p1 = .25 and p2 = .10

ES.h(p1 = .25, p2 = .10)   # effect size = 2arcsin(p1)-2arcsin(p2)
pwr.2p2n.test(h = .4037, n1 = 176, n2 = 44, sig.level = .05,
              alternative = "two.sided")
