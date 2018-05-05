library(readr)
library(dplyr)

#### Some data preprocessing
df <- read_csv("train_sample2.csv")

df$outcome <- 1 * !is.na(df$attributed_time)

df %>% 
  group_by(ip) %>% 
  mutate(rank = dense_rank((click_time)))

df <- df %>% 
  group_by(ip) %>% 
  mutate(rank = dense_rank(desc(desc(click_time))))


df_tmp <- df %>% 
  group_by(ip) %>% 
  summarise(outcome_agg = sum(outcome)) %>% 
  ungroup() 


df <- df %>% 
  left_join(df_tmp, 
            by = 'ip') %>% 
  filter(rank == 1) %>%
    filter(app == 14 | app == 18)

df$is_train <- sample(0:1, 
                      nrow(df), 
                      replace = T, 
                      prob = c(0.5, 0.5)
                      )

df$app14 <- (df$app == 14) * 1

train <- df[df$is_train ==1, ]

#### Fit a simple bayesian logistic regression model
library(rstan)
options(mc.cores = parallel::detectCores())

modelString <- "

data {
  int<lower=0> N;
  vector[N] K;
  int<lower=0,upper=1> y[N];
}

parameters {
  vector[2] beta;
}

model {
  y ~ bernoulli_logit(beta[1] + beta[2] * K);
}
"

a <- nrow(train)

data_list <- list(N = a, 
                  K = train$app14[1:a], 
                  y = train$outcome_agg[1:a]
)

stan_samples <- stan(model_code = modelString, 
                     data = data_list)

posterior <- as.data.frame(stan_samples)
