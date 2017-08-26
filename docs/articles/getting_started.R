## ------------------------------------------------------------------------
library(regsim)

df <- read.csv("http://philippbroniecki.github.io/ML2017.io/data/communities.csv")

## ------------------------------------------------------------------------
m1 <- lm(PctUnemployed ~ pctUrban + householdsize + racePctWhite, data = df)

summary(m1)

## ------------------------------------------------------------------------
set.seed(123)

x <- list(
  pctUrban = seq(from = 0, to = 1, by = .1), 
  householdsize = mean(df$householdsize)
)

sim <- regsim(m1, x)

## ------------------------------------------------------------------------
plot(sim, ~pctUrban, ylim = c(0.3, 0.5), bty = "n", xlab = "Pct Urban", ylab = "Unemployment Rate")

## ------------------------------------------------------------------------
summary(sim)

## ------------------------------------------------------------------------
sim

