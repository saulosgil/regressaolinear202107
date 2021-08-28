library(purrr)
library(broom)
library(tidyverse)
library(gganimate)
library(caret)
library(glmnet)
set.seed(9)
theme_set(theme_minimal(20))

# Criando banco de dados --------------------------------------------------

criar_amostra <- function(n) {
  tibble(
    x = runif(n, 0, 20),
    y = 500 + 0.4 * (x-10)^3 + rnorm(n, sd = 50)
  )
}

df_treino <- criar_amostra(20)

# LASSO ---------------------------------------------------------------------

X <- model.matrix(y ~ poly(x, 9), data = df_treino)
y <- df_treino$y
lasso <- cv.glmnet(X, y, nfolds = 5)

plot(lasso)

glance_cv <- glance(lasso)
tidied <- tidy(lasso$glmnet.fit)
tidied %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(lambda, estimate, group = term, colour = term)) +
  scale_x_log10() +
  geom_line(size = 1.5) +
  labs(y = "Betas", x  = "Lambda") +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_vline(xintercept = glance_cv$lambda.min) +
  geom_vline(xintercept = glance_cv$lambda.1se, lty = 2) 
