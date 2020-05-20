# load librarys
library(tidyverse)
library(car)

# import some data
nephrops <- read.csv("data/nephrops.csv")

# check the data
str(nephrops)

# convert temp to a factor and recode the levels as "T10", "T13", "T16", "T19"
nephrops$temp <- factor(nephrops$temp, levels = c(10, 13, 16, 19), labels = c("T10", "T13", "T16", "T19"))
head(nephrops)

# explore data
nephrops %>%
  ggplot(aes(mass, color = sex, fill = sex)) + geom_histogram(binwidth = 1)

nephrops %>%
  ggplot(aes(x = temp, y = smr, color = sex)) + geom_boxplot() + geom_jitter(color = "black", alpha = 0.2)

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

# try a type III anova
my_anova <- aov(smr ~ temp * sex, data = nephrops)
Anova(my_anova, type = "III")

# prepare for a two way comparison
# first perform an omnibus test to assess main interactions
anova(lm(smr ~ temp * sex, nephrops))

# now run a pairwise.t.test to test pairwise comparisons between means
pairwise.t.test(nephrops$smr, nephrops$temp, p.adj = "none")

pairwise.t.test(nephrops$smr, nephrops$sex, p.adj = "none")

