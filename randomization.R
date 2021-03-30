# Load tidyverse and MatchIt
# Feel free to load other libraries as you wish
library(tidyverse)
library(MatchIt)
library(ggplot2)
library(dplyr)

# Load ypsps data
ypsps <- read_csv('data/ypsps.csv')
head(ypsps)


# Generate a vector that randomly assigns each unit to treatment/control
# Treatment vector is called A
set.seed(5)

ypsps <- ypsps %>% mutate(A = 
          recode(as.numeric(rbernoulli(dim(.)[1], p = 0.5)), '0' = "Control", '1' = "Treatment"))


# plot histogram of baseline covariate of whether parent owned their home
# Visualize the distribution by treatment/control (ggplot)

ggplot(ypsps, aes(x=parent_OwnHome))+
  geom_histogram(color="black", fill="white", bins = 2)+
  facet_grid(A ~ .)





ypsps %>% filter(A == "Control") %>% select(parent_OwnHome) %>% sum() /
  ypsps %>% filter(A == "Treatment") %>% select(parent_OwnHome) %>% sum()


# Simulate first two steps 10,000 times (monte carlo simulation - see R Refresher for a hint)



iters <- 10000

n <- iters * dim(ypsps)[1]

# Create a matrix of random assignments, each column is an iteration
set.seed(5)
randomizations <- matrix(rep(as.numeric(rbernoulli(n, p = 0.5))), 
                         nrow = dim(ypsps)[1], ncol = iters)

# Drop the initial assignment
ypsps <- ypsps %>% select(-A)

# Initialize results vector (don't know if this is necessary since I use apply?)
results <- rep(0, 10000)

# Calculate ratio of control to treatment for each randomization iteration and store in results
results <- apply(randomizations, 2, function(x) 
  ypsps %>% filter(x == 0) %>% select(parent_OwnHome) %>% sum() /
    ypsps %>% filter(x == 1) %>% select(parent_OwnHome) %>% sum())


df(results)

# Visualize iterations
plot(density(results))


# looks like it is pretty balanced overall? 
# mostly centered between 0.9 and 1.1 across 10,000 iterations, meaning pretty balanced proportions 
# of parents own homes 




# old code


# for (i in 1:100000){ 
#   A <- recode(as.numeric(rbernoulli(dim(ypsps)[1], p = 0.5)), '0' = "Control", '1' = "Treatment")
#   
#   results[i] <-  ypsps %>% filter(A == "Control") %>% select(parent_OwnHome) %>% sum() /
#     ypsps %>% filter(A == "Treatment") %>% select(parent_OwnHome) %>% sum()
#   
#   }
