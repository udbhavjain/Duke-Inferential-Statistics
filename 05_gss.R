library(dplyr)
library(ggplot2)
library(statsr)

# load general social survey dataset
load("gss.Rdata")

" The general society survey uses an area probability sample i.e clustered/multi-stage sampling.
  The observations are fairly random and represent people from different parts of the country. "

"------------------------------------------------------------------------------------------------------------------------"

" What was the average adult age in 2012? 

  year: GSS year for this respondent
  age: Age of respondent
"

# check missing data
sum(is.na(gss$year)) # 0% NA values
sum(is.na(gss$age)) # 0.35% NA values

# select age column for year 2012
ages2012 <- gss %>%
  filter(!is.na(age), year == "2012") %>%
  select(age)

# summary statistics
print.data.frame(
  ages2012 %>% summarise(mean = mean(age), sd = sd(age),
                         median = median(age), pc25 = quantile(age, 0.25), pc75 = quantile(age, 0.75))
)

# visualise distribution
x11()
ggplot(data = ages2012, aes(x = age)) +
  geom_histogram(binwidth = 5)
" lightly skewed to the right. "

# confidence interval for mean

" Condition check: 
  Independence: Observations are from a random sample and sample size < 10% of population. 
  Skew: Sample size is >30. "

inference(data = ages2012, y = age, type = "ci", statistic = "mean", method = "theoretical")

" 95% CI is (47.4118 , 48.9752). "

rm(ages2012) # cleanup

" The average adult age in 2012 was between 47 and 49 years old. "


"------------------------------------------------------------------------------------------------------------------------"


" Is there a major change in the average adult age of 2012 compared to 1972? "

# select age columns for 2012 and 1972
ages1272 <-gss %>%
  filter(!is.na(age), year == "2012" | year == "1972") %>%
  select(age, year)

# summary statistics
print.data.frame(
  ages1272 %>% group_by(year) %>%
    summarise(mean = mean(age), sd = sd(age),
                         median = median(age), pc25 = quantile(age, 0.25), pc75 = quantile(age, 0.75))
)

# visualise distributions
x11()
ggplot(data = ages1272 %>% filter(year == "1972"), aes(x = age)) +
  ggtitle("1972") +
  geom_histogram(binwidth = 5)

x11()
ggplot(data = ages1272 %>% filter(year == "2012"), aes(x = age)) +
  ggtitle("2012") +
  geom_histogram(binwidth = 5)

" Distribution for 1972 is slightly more right skewed. "

# hypothesis test

" Hypotheses:
  H0: mu(1972) =  mu(2012)
  HA: mu(1972) != mu(2012) "

" Condition check:
  Independence: Observations are randomly sampled, sample size < 10% of population, and groups are independent of each other.
  Skew: Sample size is sufficiently large (>30).
"

x11()
inference(data = ages1272, x = year, y = age ,statistic = "mean", type = "ht", 
          null = 0, alternative = "twosided", method = "theoretical")

" p value is < 0.0001. Therefore, we reject the null hypothesis. 
  The average adult age in 2012 is different compared to 1972. "


# confidence interval for difference 
inference(data = ages1272, x = year, y = age ,statistic = "mean", type = "ci", 
          null = 0, alternative = "twosided", method = "theoretical", order = c("2012, 1972"))

" 95% CI for 2012 - 1972 is (2.0976 , 4.3876). "

rm(ages1272) #cleanup

" There has been an increase of 2.0976 to 4.3876 years in the average adult age from 1972 to 2012. "


"------------------------------------------------------------------------------------------------------------------------"


" What percentage of people in 2012 were born in the country? 

  year: GSS year for this respondent
  born: Was R born in this country "


levels(gss$born) # categorical - "Yes" and "No"

# check number of NAs
sum(is.na(gss$born)) # 9257 observations

born2012 <- gss %>%
  filter(!is.na(born), year == "2012") %>%
  select(born)

# percentages
print.data.frame(born2012 %>% summarise(p = sum(born == "Yes")/n(), np = p * n(),
                                        "1-p" = 1-p, "n(1-p)" = n()* (1-p))
)

# visualisation
x11()
ggplot(data = born2012, aes(x = born2012$born)) + geom_bar()
  

# confidence interval for proportion

" Condition Check: 

  Independence: Observations are randomly sampled and sample size < 10% of population. 
  Skew: np and n(1-p) are both > 10. "

inference(data = born2012, y = born, type = "ci", statistic = "proportion", success = "Yes", method = "theoretical")

" 95% CI is (0.8473 , 0.8777). "

rm(born2012) #cleanup

" 84.73 to 87.77 percent of the population was born in the country in 2012. "


"------------------------------------------------------------------------------------------------------------------------"


" Has the proportion of people born in the country changed from 1982 to 2012? 

  year: GSS year for this respondent
  born: Was R born in this country 
"


# select data for 1982 and 2012
born1282 <- gss %>%
  filter(!is.na(born), year == "2012" | year == "1982") %>%
  select(born, year)

# conver year values from numbers to factors
born1282$year <- as.factor(born1282$year)

# percentages
print.data.frame(
  born1282 %>%
    group_by(year) %>%
    summarise(p = sum(born == "Yes")/n(), np = p * n(),
                                        "1-p" = 1-p, "n(1-p)" = n()* (1-p))
)

# visualisation 
x11()
ggplot(data = born1282, aes(x = year, fill = born)) + 
  geom_bar(position = "fill") + ylab(label = "proportion")

# hypothesis test

" hypotheses:
  H0: p(1982) =  p(2012)
  HA: p(1982) != p(2012) "

" Condition check: 
  Independence: Observations are randomly sampled, sample size < 10% of population, and groups are independent of each other.
  Skew: n1p1 & n1(1-p1) > 10; n2p2 & n2(1-p2) > 10
"

x11()
inference(data = born1282, y = born, x = year, type = "ht", statistic = "proportion",
          success = "Yes", method = "theoretical", null = 0, alternative = "twosided")

" p value is < 0.0001. We reject the null hypothesis. "

" The proportion of people born in the country has changed. "

# confidence interval
inference(data = born1282, y = born, x = year, type = "ci", statistic = "proportion",
          success = "Yes", method = "theoretical", order = c("1982","2012"))

" 95% CI is (0.0647 , 0.1015). "

rm(born1282) # cleanup

" The percentage of population born in the country has dropped by 6.47 to 10.15 percent from 1982 to 2012. "



"------------------------------------------------------------------------------------------------------------------------"



" Does the gender ratio of the population in 2012 deviate from 50:50?

  sex: Respondent's sex
  year: GSS year for this respondent

"

str(gss$sex) # categorical variable with 2 levels - "Male" and "Female"

# check number of NAs
sum(is.na(gss$sex)) # no missing values

# select the sex column for 2012
fm2012 <- gss %>%
  filter(year == "2012") %>%
  select(sex)

# proportion of males
sum(fm2012$sex == "Male")/nrow(fm2012)

" 44.88%, which is below 50%. "

# visualisation
x11()
ggplot(data = fm2012, aes(x = sex, fill = sex)) + geom_bar()

# simulation based hypothesis test

" Independence condition is satisfied. "

"Hypotheses:
 H0: p(Male} =  0.5
 HA: p(Male) != 0.5
"
x11()
inference(data = fm2012, y = sex, type = "ht", statistic = "proportion", success = "Male",
          null = 0.5, alternative = "twosided", method = "simulation", nsim = 10000, seed = 2012)

" p values is < 0.0001. Therefore, null hypothesis is rejected and proportion of males is below 0.5. "

rm(fm2012) # cleanup

" Proportion of males in the population is lower than the proportion of females.  "


"------------------------------------------------------------------------------------------------------------------------"



" Does party affiliation differ between males and females in 2012? 

  sex: Respondent's sex
  partyid: Political party affiliation "

# check NAs
sum(is.na(gss$partyid)) # low number of NAs

str(gss$partyid) # ordinal variable with 8 levels
levels(gss$partyid)

# select sex and party affiliation data for the year 2012
fm12party <- gss %>% 
  filter(year == "2012", !is.na(partyid)) %>%
  select(sex, partyid)

# proportions for affiliation to party, grouped by gender

fm12party %>%
  group_by(sex) %>%
  summarise(
    StrDem = sum(partyid == "Strong Democrat")/n(),
    NStrDem = sum(partyid == "Not Str Democrat")/n(),
    InNrDem = sum(partyid == "Ind,Near Dem")/n(),
    Indpt = sum(partyid == "Independent")/n(),
    InNrRep = sum(partyid == "Ind,Near Rep")/n(),
    NStrRep = sum(partyid == "Not Str Republican")/n(),
    StrRep = sum(partyid == "Strong Republican")/n(),
    OtPrty = sum(partyid == "Other Party")/n()
    )

# visualisation
x11()
ggplot(data = fm12party, aes(x = sex, fill = partyid)) +
  geom_bar(position = "fill") + ylab("proportion")
  

# chi-square test for independence

# observed counts
obsCounts <- fm12party %>%
  group_by(sex) %>%
  summarise(StrDem = sum(partyid == "Strong Democrat"),
            NStrDem = sum(partyid == "Not Str Democrat"),
            InNrDem = sum(partyid == "Ind,Near Dem"),
            Indpt = sum(partyid == "Independent"),
            InNrRep = sum(partyid == "Ind,Near Rep"),
            NStrRep = sum(partyid == "Not Str Republican"),
            StrRep = sum(partyid == "Strong Republican"),
            OtPrty = sum(partyid == "Other Party")
  )

obsCounts <- rbind(as.numeric(obsCounts[1,-1]),as.numeric(obsCounts[2,-1]))

rSum <- matrix(rowSums(obsCounts))
cSum <- matrix(colSums(obsCounts))
total = sum(rowSums(obsCounts))

expCounts <-  rSum %*% t(cSum)

expCounts <- expCounts/total
min(expCounts) # 24.21


" Condition Check:
  Independence: Samples are independent of each other, sample size < 10% of total population
  and each observation contributes to only one cell.

  Sample size: At least 5 expected cases for each cell.
"


" Hypotheses: 
  H0: Political affiliation does not vary by gender.
  HA: Political affiliation does vary by gender. 
"

x11()
inference(data = fm12party, y = sex, x = partyid, type = "ht", statistic = "proportion",
          alternative = "greater", method = "theoretical")

" p value is 0, so we reject the null hypothesis. "

rm(expCounts, obsCounts, rSum, cSum, total, fm12party) # cleanup

" According to the data for 2012, party affiliation seems to differ between males and females. "

rm(gss) # release GSS dataset from memory

