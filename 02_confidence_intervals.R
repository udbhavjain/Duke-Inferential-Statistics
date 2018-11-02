library(statsr)
library(dplyr)
library(ggplot2)

set.seed(021118)

# load housing dataset
data(ames) 

# take random sample of size 60
samp <- sample_n(tbl = ames, size = 60)

# calculate summary statistics for areas in sample
print.data.frame(
  samp %>%
    summarise(x_bar = mean(area), pop_med = median(area), 
            sd = sd(area), pop_iqr = IQR(area),
            pop_min = min(area), pop_max = max(area),
            pop_q1 = quantile(area, 0.25),  
            pop_q3 = quantile(area, 0.75))  
  )

# visualisation of areas in sample
x11()
ggplot(data = samp, aes(x = area)) +
  geom_histogram(binwidth = 250)

" The distribution is right skewed. Typical size is 1527 square feet. 
  The mean size was considered as the typical size. "

" TRUE: My distribution should be similar to others’ distributions who also collect 
  random samples from this population, but it is likely not exactly the same since 
  it’s a random sample. "


# calculate z score for 95% confidence interval i.e for 97.5 percentile
z_star_95 <- qnorm(0.975) # value is 1.96, as expected

# calculate confidence interval using sample mean
print.data.frame(
  samp %>%
    summarise(lower = mean(area) - z_star_95 * (sd(area) / sqrt(60)),
            upper = mean(area) + z_star_95 * (sd(area) / sqrt(60)))
)

" For the confidence interval to be valid, the sample distribution does not have to be 
  nearly normal. "

" 95% confidence means that 95% of random samples of size 60 will yield confidence 
  intervals that contain the true average area of houses in Ames, Iowa. "

# population mean
params <- mean(ames$area)

" Confidence interval is (1400, 1653) and the population mean is 1500. 
 Thus, the interval captures the true mean of the population. "

" 95% of the intervals should capture the true mean. "

# calculate confidence intervals for 50 samples of size 60
ci <- ames %>%
  rep_sample_n(size = 60, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(area) - z_star_95 * (sd(area) / sqrt(60)),
            upper = mean(area) + z_star_95 * (sd(area) / sqrt(60)))

print.data.frame(ci)

# check whether lower bound is smaller than mean and upper bound is greater
ci <- ci %>%
  mutate(capture_mu = ifelse(lower < params & upper > params, "yes", "no"))

print.data.frame(ci)

# convert intervals to bounds
ci_data <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))

print.data.frame(ci_data)

# plot to check if sample captures true mean
x11()
ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params, color = "darkgray") # draw vertical line

" 100% of the intervals contain the population mean. This is greater than the confidence level. "

# critical value for 99% confidence interval
z_star_99 <- qnorm(0.995)

" Critical value for a 99% confidence intercal is 2.58 "

# calculate intervals for 99% confidence level

# calculate mean from bounds
ci <- ci %>%
  mutate(x_bar = (lower+upper)/2)

# calculate standard deviation
ci <- ci %>%
  mutate(stdev = (((upper - x_bar) * sqrt(60))/z_star_95))

# calculate new upper and lower bounds for a 99% confidence interval
ci <- ci %>%
  mutate(lower = x_bar - z_star_99 * (stdev / sqrt(60)),
         upper = x_bar + z_star_99 * (stdev / sqrt(60)))

# check whether lower bound is smaller than mean and upper bound is greater
ci <- ci %>%
  mutate(capture_mu = ifelse(lower < params & upper > params, "yes", "no"))

print.data.frame(ci)

# convert intervals to bounds
ci_data <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))

print.data.frame(ci_data)

# plot to check if sample captures true mean
x11()
ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params, color = "darkgray") # draw vertical line

"All the intervals contain the true population mean."

" TRUE : We would expect 99% of the intervals to contain the true population mean. "
