################################################################################
# Lab 10
# Caroline Devine
################################################################################

################################################################################
# Part 1: Basic Simulation
################################################################################
library(tidyverse)
library(patchwork)
library(nleqslv)

# True Probability
true.p <- 0.39

### First Sample Size ###
sample.size <- 1004
polls <- 10000

satisfied <- rbinom(polls, size = sample.size, prob = true.p)
proportion.satisfied <- satisfied/sample.size

og.sample <- tibble(proportion = proportion.satisfied)

sample.plot.1 <- ggplot(data = og.sample) + 
  geom_histogram(aes(x=proportion, y=after_stat(density)),
                 binwidth = 0.005,
                 color="grey")+
  geom_density(aes(x = proportion), color = "red")+
  geom_hline(yintercept=0)+
  theme_bw()+
  xlab("Sample Proportions")+
  ylab("Density")+
  ggtitle("Sampling Distribution (n = 1,004)")

#Shape is roughly normal (bell-shaped), centering around 0.39, symmetrical (small variation from random samping)
# Expected b/c of CLT 

# Range of the middle 95%
quant.nums <- quantile(proportion.satisfied, probs = c(0.025, 0.975))
range.middle <- quant.nums[2] - quant.nums[1]
margin.error <- range.middle/2
# This margin of error is approximately 3% which is lower than the 4% Gallup reported


### Second Sample Size ###
sample.size2 <- 2008 # doubled sample size

satisfied2 <- rbinom(polls, size = sample.size2, prob = true.p)
proportion.satisfied2 <- satisfied2/sample.size2

sample2 <- tibble(proportion = proportion.satisfied2)

sample.plot.2 <- ggplot(data = sample2) + 
  geom_histogram(aes(x=proportion, y=after_stat(density)),
                 binwidth = 0.005,
                 color="grey")+
  geom_density(aes(x = proportion), color = "red")+
  geom_hline(yintercept=0)+
  theme_bw()+
  xlab("Sample Proportions")+
  ylab("Density")+
  ggtitle("Sampling Distribution (n = 2,008)")

#Shape is roughly normal (bell-shaped), centering around 0.39, symmetrical (small variation from random samping)
# Expected b/c of CLT 

# Range of the middle 95%
quant.nums.2 <- quantile(proportion.satisfied2, probs = c(0.025, 0.975))
range.middle.2 <- quant.nums.2[2] - quant.nums.2[1]
margin.error.2 <- range.middle.2/2
# This margin of error is approximately 2.15% which is slightly higher than the 2% Gallup reported


# Two Samples Together
sample.distributions <- sample.plot.1 + sample.plot.2

# Table
table1 <- tibble(
  `Sample Size` = c(1004, 2008),
  `Range of middle 95%` = c(range.middle, range.middle.2), 
  `Margin of Error` = c(margin.error, margin.error.2)
)

################################################################################
# Part 2: Resampling
################################################################################

# Data from the Gallup Survey
samp.size <- 1004
perc.satisfied <- 0.39
perc.unsatisfied <- 0.59
perc.no.opinion <- 0.02 # is this relevant for the data

dat.gallup <- tibble(id = 1:samp.size,
                     response =  c(rep("satisfied", round(perc.satisfied*samp.size)),
                                   rep("unsatisfied", round(perc.unsatisfied*samp.size)),
                                   rep("no opinion", round(perc.no.opinion*samp.size)))
                    )

#dat.gallup |>
#  summarize(
#    proportion.sat = mean(response == "satisfied")
#  )

R <- 1000 # number of resamples
resamples <- tibble(p.hat = numeric(R))
for (i in 1:R){
  curr.resample <- sample(x = dat.gallup$response,
                          size = nrow(dat.gallup),
                          replace = TRUE)
  resamples$p.hat[i] <- mean(curr.resample == "satisfied")
}
resample.plot <- ggplot(resamples) + 
  geom_histogram(aes(x=p.hat, y=after_stat(density)),
                      bins = 30,
                      color="grey")+
  geom_density(aes(x = p.hat), color = "red")+
  geom_hline(yintercept=0)+
  theme_bw()+
  xlab(bquote(hat(p)))+
  ylab("Density")+
  ggtitle("Resampling Distribution of "~hat(p))


