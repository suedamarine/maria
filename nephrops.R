# load librarys
library(tidyverse)

# import some data
nephrops <- read.csv("data/nephrops.csv")

# explore data
nephrops %>%
  ggplot(aes(mass, color = sex, fill = sex)) + geom_histogram(binwidth = 1)

nephrops %>%
  ggplot(aes(x = sex, y = mass)) + geom_boxplot(aes(color = sex)) + geom_jitter(color = "black")

nephrops %>% 
  ggplot(aes(x = mass, y = smr, color = sex)) + geom_point() + facet_wrap(.~temp, ncol = 4)

# get some summary data

mass_summary <- nephrops %>%
  group_by(sex) %>%
  summarize(mean_mass = mean(mass, na.rm = TRUE),
            sd_mass = sd(mass, na.rm = TRUE))
head(mass_summary)

# check size range at p = 0.95 (for females)
qnorm(c(0.025, 0.975), 19.3, 5.68)

# check relationship using a crude linear regression
lmneph <- lm(smr ~ mass + temp + sex, data = nephrops)
summary(lmneph)
