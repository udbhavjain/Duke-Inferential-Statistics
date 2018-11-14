library(statsr)
library(dplyr)
library(ggplot2)

" A poll conducted by WIN-Gallup International surveyed 51,927 people from 57 countries. "

" All of the above methods ( Face to face, Telephone, Internet ) were used to gather information. "

" TRUE: These percentages appear to be sample statistics. "

" TRUE: To generalize the report’s findings to the global human population, We must assume that the 
  sample was a random sample from the entire population in order to be able to generalize the results 
  to the global human population. "

data(atheism) # load atheism index data set

" Each row of table 6 corresponds to a country. "
" Each row of the atheism dataset corresponds to an individual person. "

# respondents to the 2012 survey from the United States:
us12 <- atheism %>%
  filter(nationality == "United States" , atheism$year == "2012")

# percentage of people that are atheists
sum(us12$response == "atheist") * 100 / nrow(us12)

" The percentage of atheists is 4.99%, which matches the percentage in the survey when rounded up (5%). "

" Conditions for inference: "
  
" Independence: Random people were surveyed and the number of sample sizes are less than 10% for all the countries. "

sum(us12$response == "atheist")
sum(us12$response == "non-atheist")

" Skew: There are 50 atheists and 952 non-atheists, so the success-failure condition is met and sampling 
  distribution can be assumed to be nearly normal. "

# calculate confidence interval
x11()
inference(y = response, data = us12, statistic = "proportion", type = "ci", method = "theoretical", success = "atheist")

" A margin of error defines how much the calculated estimate may deviate from the true value. "

" 95% CI for the proportion of atheists in the US in 2012 is (0.0364 , 0.0634) and the margin of error is 0.0135. "


" Confidence intervals for two other countries: "


# respondents to the 2012 survey from Canada:
ca12 <- atheism %>%
  filter(nationality == "Canada" , atheism$year == "2012")

sum(ca12$response == "atheist")
sum(ca12$response == "non-atheist")

" Conditions for inference: 
  Independence - Random people were surveyed and sample size is less than 10% of the total population. 
  Skew - Number of successes and failures is more than 10; Sampling distribution is normal. "

# confidence interval
x11()
inference(y = response, data = ca12, statistic = "proportion", type = "ci", method = "theoretical", success = "atheist")

" 95% CI for Canada is (0.0364 , 0.0634). "

# respondents to the 2012 survey from Japan:
jp12 <- atheism %>%
  filter(nationality == "Japan" , atheism$year == "2012")

sum(jp12$response == "atheist")
sum(jp12$response == "non-atheist")

" Conditions for inference: 
  Independence - Random people were surveyed and sample size is less than 10% of the total population. 
  Skew - Number of successes and failures is more than 10; Sampling distribution is normal. "

# confidence interval
x11()
inference(y = response, data = jp12, statistic = "proportion", type = "ci", method = "theoretical", success = "atheist")

" 95% CI for Japan is (0.281 , 0.3329). "

# Relation between proportion and margin of error for a 95% CI
d <- data.frame(p <- seq(0, 1, 0.01))
n <- 1000
d <- d %>%
  mutate(me = 1.96*sqrt(p*(1 - p)/n))

x11()
ggplot(d, aes(x = p, y = me)) +
  geom_line()

" FALSE: The most conservative estimate when calculating a confidence interval occurs when p is set to 1. "

" Check if Spain has seen a change in its atheism index between 2005 and 2012: 

  Hypotheses: 
   (H0: p{2005}  = p{2012}) 
   (HA: p{2005} != p{2012}) "

es0512 <- atheism %>%
  filter(nationality == "Spain" , atheism$year == "2012" || atheism$year == "2005")

# check for success-failure condition
p_pool <- (sum(es0512$year == "2005" & es0512$response == "atheist") + 
  sum(es0512$year == "2012" & es0512$response == "atheist"))/nrow(es0512)

n1p <- sum(es0512$year == "2005") * p_pool
n1p_ <- sum(es0512$year == "2005") * (1-p_pool)

n2p <- sum(es0512$year == "2012") * p_pool
n2p_ <- sum(es0512$year == "2012") * (1-p_pool)

" Condition is satisfied. Sampling distribution is normal. "

# hypothesis test
x11()
inference(y = response, x = year, data = es0512, success = "atheist" , statistic = "proportion", type = "ht", 
          null = 0, alternative = "twosided", method = "theoretical")

" p value is 0.3966. Therefore we do not reject the null hypothesis. 
  Spain has not seen a change in atheism index between 2005 and 2012. "


" Check if USA has seen a change in its atheism index between 2005 and 2012: 

  Hypotheses: 
   (H0: p{2005}  = p{2012}) 
   (HA: p{2005} != p{2012}) "

us0512 <- atheism %>%
  filter(nationality == "United States" , atheism$year == "2012" || atheism$year == "2005")

# check for success-failure condition
p_pool <- (sum(us0512$year == "2005" & us0512$response == "atheist") + 
            sum(us0512$year == "2012" & us0512$response == "atheist"))/nrow(us0512)

n1p <- sum(us0512$year == "2005") * p_pool
n1p_ <- sum(us0512$year == "2005") * (1-p_pool)

n2p <- sum(us0512$year == "2012") * p_pool
n2p_ <- sum(us0512$year == "2012") * (1-p_pool)

" Condition is satisfied. Sampling distribution is normal. "

# hypothesis test
x11()
inference(y = response, x = year, data = us0512, success = "atheist" , statistic = "proportion", type = "ht", 
          null = 0, alternative = "twosided", method = "theoretical")

" p value is below 0.0001. Therefore we reject the null hypothesis. 
  USA has seen a change in atheism index between 2005 and 2012. "


" There are 39 countries in table 4. 0.05 * 39 = 1.95. "

" Minimum size of sample : "

# Sample size for maximum margin of error 1%
p_val <- 0.50 # highest ME is at p = 50%
z_val <- 1.96
ME <- 0.01

min_size <- z_val * z_val * p_val * (1-p_val) / (ME * ME) 

" Minimum sample size required is 9604. "
