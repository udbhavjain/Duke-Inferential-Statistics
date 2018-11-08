library(statsr)
library(dplyr)
library(ggplot2)

data(nc) # load NC birth info dataset

" The cases in the dataset represent the births that took place in North Carolina. "

str(nc)

# exploratory data analysis

# analyze weight gained by mothers throughout the pregnancy
summary(nc$gained)

" 27 observations have missing weight gain data. "

# side-by-side boxplots for weight, grouped by smoking habit
x11()
ggplot(data = nc, aes(y = weight, x = habit)) +
  geom_boxplot()

" FALSE: Both distributions are extremely right skewed. "

# group by smoking habit and calculate mean weight
nc %>%
  group_by(habit) %>%
  summarise(mean_weight = mean(weight))

" Hypotheses will be: 
 (H0: mu{smoking}  = mu{non-smoking}) 
 (HA: mu{smoking} != mu{non-smoking})
"

# conduct hypothesis test using the inference function
x11()
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ht", 
          null = 0, alternative = "twosided", method = "theoretical")

" p-value is 2%, so we reject the null hypothesis. There is indeed a difference between the weights. "

# confidence interval for the difference between the weights of babies
x11()
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ci", 
          method = "theoretical")

" 95% CI for (nonsmoker - smoker) is (0.0508 , 0.5803) 
  We are 95% confident that babies born to nonsmoker mothers are on average 
  0.05 to 0.58 pounds heavier at birth than babies born to smoker mothers.
"

# calculate interval for (smoker - nonsmoker)
x11()
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ci", 
          method = "theoretical", order = c("smoker","nonsmoker"))

# 99% confidence interval for the average length of pregnancies 
x11()
inference(y = weeks, data = nc, statistic = "mean", type = "ci", 
          method = "theoretical", conf_level = 0.99)

" 99% CI is (38.0952 , 38.5742) weeks. "

# 90% confidence interval for the average length of pregnancies
x11()
inference(y = weeks, data = nc, statistic = "mean", type = "ci", 
          method = "theoretical", conf_level = 0.90)

" 90% CI is (38.1819 , 38.4874), which is narrower than the 99% CI. "

# hypothesis test evaluating whether the average weight gained by younger mothers is 
# different than the average weight gained by mature mothers.

"Hypotheses will be: 
 (H0: mu{mature}  = mu{younger}) 
 (HA: mu{mature} != mu{younger})"

# hypothesis testing
x11()
inference(y = gained, x = mature, data = nc, statistic = "mean", type = "ht", 
          null = 0, alternative = "twosided", method = "theoretical")

" p-value is 17%, so we do not reject the null hypothesis.
  There is no difference between the average weight gained by mature and younger mothers. "

# confidence interval
x11()
inference(y = gained, x = mature, data = nc, statistic = "mean", type = "ci", 
          method = "theoretical")

" 95% CI for (mature mom - younger mom) is (-4.3137 , 0.7742).
  As the bounds of the CI are not on the same side of zero, the difference is not significant."

# determine age cutoff for younger and mature mothers
x11()
ggplot(data = nc, aes(y = fage, x = mature)) +
  geom_boxplot()

" According to the boxplot, the 25th percentile mark for mature mothers is 35 years.
  This is above the 75th percentile mark for young mothers. Thus, 35 years seems to be 
  a good cutoff point for categorisation. "


" Is there a difference between the weights of male and female newborns? "

"Hypotheses will be: 
 (H0: mu{male}  = mu{female}) 
 (HA: mu{male} != mu{female})"

# hypothesis testing
x11()
inference(y = weight, x = gender, data = nc, statistic = "mean", type = "ht", 
          null = 0, alternative = "twosided", method = "theoretical")

" p-value is less than 0.0001, so we reject the null hypothesis.
  The weights of female and male newborns are not equal. "

# confidence interval
x11()
inference(y = weight, x = gender, data = nc, statistic = "mean", type = "ci", 
          method = "theoretical", order = c("male","female"))

" 95% CI for (male - female) is (0.2126 , 0.5846). 

  We are 95% confident that male babies are on average 
  0.2126 to 0.5846 pounds heavier than female babies. "