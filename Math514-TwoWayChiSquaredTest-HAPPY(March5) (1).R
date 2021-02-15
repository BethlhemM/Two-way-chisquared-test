#=============================================================
# Chi-squared test for Independence
#=============================================================
# Is there an association (statistically independent) between a 
# person's sex and a person's happiness?
# 
# The following social survey shows how a subset of respondents  
# were classified with respect to the variables HAPPY and SEX.
#=============================================================
HA <- c(110, 277, 50, 163, 302, 63)
HAT <- matrix(data = HA, nrow = 2, byrow = TRUE)
dimnames(HAT) <- list(SEX = c("Male", "Female"),
                      Category = c("Very Happy", "Pretty Happy", "Not Too Happy"))
HAT
#=============================================================
# Expected or Theoretical frequencies using the formula
#calculate by plugging all the formula 
E <- outer(rowSums(HAT), colSums(HAT), "*")/sum(HAT)
E
#=============================================================
# Expected or Theoretical frequencies using the chisq.test() function
#below will calculate it for us without us plugging in the values 
chisq.test(HAT)$expected
#=============================================================
# Formula to calculate the Test statistic that compares the Observed 
# frequencies in the table with the Expected frequencies when Ho is true.
chi.obs <- sum((HAT - E)^2/E )
chi.obs
#=============================================================
# Under the assumption of independence, and when the observations in the cells 
# are sufficiently large (usually greater than 5), this Test statistic will have 
# Chi-Squared distribution with (2-1).(3-1)= 2 degrees of freedom. 
#
# Using the Chi-squared chart, the RR for Chi-squared distribution with alpha = 0.05 and df = 2 is
5.9915
# Using Quantile function for Chi-squared distribution the same could be obtained
#below gives us the rejection area 
RR <- qchisq(0.95, df =  2)
RR
#=============================================================
# The null hypothesis of independence is rejected when TS > RR (i.e. TS 
# falls inside the RR).
#
# Put another way, the null hypothesis of independence is rejected when 
# P-value < alpha-value. 
#quantile fubction, pchisq , the p valuehelps us compute critical value 
p.val <- pchisq(chi.obs, 2, lower = FALSE)
p.val
# Statistical conclusion using RR method: TS 4.312482 is not > RR 5.9915 (with alpha = 0.05 and df = 2).
# i.e. TS does not fall in the rejection region.
# Therefore we do not reject Ho: the null hypothesis of independence at a 5% significance level.
#=============================================================
# Using the chisq.test() function to test Ho: the null hypothesis of independence.
chisq.test(HAT)
# Statistical conclusion using p-value method: The p-value 0.1152 is not < alpha-value 0.05.
# Therefore we do not reject Ho: the null hypothesis of independence at a 5% significance level. 
#=============================================================
# English Conclusion:  There is insufficient evidence to indicate the variables 
# SEX and HAPPINESS are statistically dependent.
#
#=============================================================
# Bottom-line: The chisq.test() is easy to implement
#
# We use chisq.test(counts,p=prob) to Test for goodness of fit
# 
# We use chisq.test(TABLE, correct=0) to Test for independence
# 
# Where, TABLE is our contingency table and correct=0 
# option is the default option that uses the statistical 
# methodology we learned in class. If you change this option 
# a different method will be used that uses a continuity correction. 
#=============================================================
