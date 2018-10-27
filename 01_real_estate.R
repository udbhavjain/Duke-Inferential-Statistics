library(statsr)
library(dplyr)
library(shiny)
library(ggplot2)

# load real estate dataset
data(ames)

# visualise distribution of living area of houses
x11()
ggplot(data = ames, aes(x = area)) +
  geom_histogram(binwidth = 250)

# calculate summary statistics
ames %>%
  summarise(mu = mean(area), pop_med = median(area), 
            sigma = sd(area), pop_iqr = IQR(area),
            pop_min = min(area), pop_max = max(area),
            pop_q1 = quantile(area, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(area, 0.75))  # third quartile, 75th percentile

"

'50% of houses in Ames are smaller than 1,499.69 square feet.'

As the population median is 1442 feet, the statement above is false:

"

# take a random sample of size 50
samp1 <- ames %>%
  sample_n(size = 50)

# visualise distribution of sample
x11()
ggplot(data = samp1, aes(x = area)) +
  geom_histogram(binwidth = 250)

# summarise statistics for sample

# calculate summary statistics
samp1 %>%
  summarise(x_bar = mean(area), pop_med = median(area), 
            sd = sd(area), pop_iqr = IQR(area),
            pop_min = min(area), pop_max = max(area),
            pop_q1 = quantile(area, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(area, 0.75))  # third quartile, 75th percentile

" slightly right skewed distribution. Summary statistics are close."

"The greater the size of the sample, the closer it's mean will be to 
that of the population, so 1000 samples."


# generate 15000 samples and calculate means for all of them
sample_means50 <- ames %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
  summarise(x_bar = mean(area))


ggplot(data = sample_means50, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)
"The distribution is almost perfectly normal, centred at 1500."

# generating smaller samples
sample_means_small <- ames %>%
  rep_sample_n(size = 10, reps = 25, replace = TRUE) %>%
  summarise(x_bar = mean(area))

print.data.frame(sample_means_small)
"Each observation is a mean of a random sample of 10 elements."

nrow(sample_means_small)
"There are 25 elements in this small sample."

"TRUE:
Each element represents a mean square footage from a simple random sample of 10 houses."



# distributions for sample sizes 10, 50 and 100 with 5000 observations

sample_5k_10 <- ames %>%
  rep_sample_n(size = 10, reps = 5000, replace = TRUE) %>%
  summarise(x_bar = mean(area))

sample_5k_50 <- ames %>%
  rep_sample_n(size = 50, reps = 5000, replace = TRUE) %>%
  summarise(x_bar = mean(area))

sample_5k_100 <- ames %>%
  rep_sample_n(size = 100, reps = 5000, replace = TRUE) %>%
  summarise(x_bar = mean(area))

x11()
ggplot(data = sample_5k_10, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)

x11()
ggplot(data = sample_5k_50, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)

x11()
ggplot(data = sample_5k_100, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)

sample_5k_10  %>% summarise(mean10 = mean(x_bar), se = sd(x_bar))
sample_5k_50  %>% summarise(mean50 = mean(x_bar), se = sd(x_bar))
sample_5k_100 %>% summarise(mean100 = mean(x_bar), se = sd(x_bar))

"As the sample size increases, the variability of the sampling distribution decreases."

# point estimate for mean of price
samp1 %>% summarise(price_mean = mean(price))

# take 5000 samples of size 50 and calculate means for price variable
sample_means50 <- ames %>%
  rep_sample_n(size = 50, reps = 5000, replace = TRUE) %>%
  summarise(x_bar = mean(price))

# plot the means of price
x11()
ggplot(data = sample_means50, aes(x = x_bar)) +
  geom_histogram(binwidth = 2000)
"Distribution is almost normal. Mean home price is about 180k."


# take 5000 samples of size 150 and calculate means for price variable
sample_means150 <- ames %>%
  rep_sample_n(size = 150, reps = 5000, replace = TRUE) %>%
  summarise(x_bar = mean(price))

# plot the means of price
x11()
ggplot(data = sample_means150, aes(x = x_bar)) +
  geom_histogram(binwidth = 2000)
"Distribution has narrowed down. Mean home price is about 180k"

# sample of size 15
sample15 <- ames %>%
  sample_n(size = 15, replace = TRUE) 

# calculate mean house price
sample15 %>% summarise(x_price = mean(price))
"Point estimate for house price is 147k"

# take 2000 samples of size 15 and compute means
sample_means15 <- ames %>%
  rep_sample_n(size = 15, reps = 2000, replace = TRUE) %>%
  summarise(x_bar = mean(price))

# plot the distribution
x11()
ggplot(data = sample_means15, aes(x = x_bar)) +
  geom_histogram(binwidth = 2000)
"Distribution is right skewed. Mean home price appears to be about 180k"

# calculate population mean
ames %>% summarise(mu = mean(price))
"Mean price for population is 180796."

# take 2000 samples of size 150 and compute means
sample_means150 <- ames %>%
  rep_sample_n(size = 150, reps = 2000, replace = TRUE) %>%
  summarise(x_bar = mean(price))

# plot the distribution
x11()
ggplot(data = sample_means150, aes(x = x_bar)) +
  geom_histogram(binwidth = 2000)
"Skew is gone, variability is lower and distribution is almost normal now. 
 Mean home price is about 180k."

"
The statement below is false:
   'The variability of the sampling distribution with the smaller sample 
   size (sample_means50) is smaller than the variability of the sampling 
   distribution with the larger sample size (sample_means150).'

The distribution with larger sample size (150) had lower variance than the one
with sample size 50. 

"



