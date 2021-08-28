library(tidyverse)
library(gganimate)
library(broom)
library(rsample)
library(purrr)
library(modelr)
library(magick)
library(ggplot2)

set.seed(1)

cars_bs <- cars %>%
  bootstrap(1000) %>%
  mutate(
    lm = map(strap, ~lm(dist ~ speed, data = .)),
    coeficientes = map(lm, tidy)
    # predicoes = map(lm, augment)
  ) %>%
  unnest(coeficientes) %>%
  mutate(
    frame = parse_number(.id)
  ) 

retas <- cars_bs %>%
  select(frame, term, estimate) %>%
  spread(term, estimate) %>% 
  ggplot() +
  geom_point(aes(x = speed, y = dist), alpha = 0.5, data = cars) +
  geom_abline(aes(intercept = `(Intercept)`, slope = speed, group = frame), alpha = 0.2, colour = "salmon") +
  transition_states(frame, 1, 1) + 
  ggtitle("# retas = {frame}") +
  shadow_mark() 

retas_gif <- animate(retas, width = 270, height = 270, nframes = 1000, fps = 50, end_pause = 10)


dnorm2 <- function(x, ...) {
  dnorm(x, ...) * 2000
}

hist_data <- cars_bs %>%
  select(frame, term, estimate) %>%
  nest(terms = c(term, estimate)) %>%
  mutate(
    terms = accumulate(terms, ~ bind_rows(.x, .y))
  ) %>%
  unnest(terms) %>%
  filter(frame == 1000)

hist_summary <- hist_data %>%
  group_by(term) %>%
  summarise(
    mean = mean(estimate),
    sd = sd(estimate)
  )

hist_intercept <-hist_data %>%
  ggplot(aes(x = estimate)) +
  geom_histogram(aes(x = estimate, y = ..density..), bins = 15) +
  stat_function(aes(colour = "Beta0"), show.legend = FALSE, fun = dnorm, args = list(mean = hist_summary$mean[1], sd = hist_summary$sd[1])) +
  stat_function(aes(colour = "Beta1"), show.legend = FALSE, fun = dnorm, args = list(mean = hist_summary$mean[2], sd = hist_summary$sd[2])) +
  stat_function(aes(fill = "Beta0"), show.legend = FALSE, geom = "area", alpha = 0.3, fun = dnorm, args = list(mean = hist_summary$mean[1], sd = hist_summary$sd[1])) +
  stat_function(aes(fill = "Beta1"), show.legend = FALSE, geom = "area", alpha = 0.3, fun = dnorm, args = list(mean = hist_summary$mean[2], sd = hist_summary$sd[2])) +
  facet_wrap(~term, scales = "free", ncol = 1) +
  labs(
    x = "",
    y = "",
    fill = "Parâmetro"
  ) +
  theme(
    axis.text.y = element_blank()
  ) +
  theme_void(20)

hist_intercept_gif <- animate(hist_intercept, width = 2*272, height = 270, nframes = 1000, fps = 50, end_pause = 10, ref_frame = 999)

distrib_conj <- cars_bs %>%
  select(frame, term, estimate) %>%
  spread(term, estimate) %>% 
  ggplot() +
  geom_point(aes(x = `(Intercept)`, y = speed), alpha = 0.2, colour = "black", size = 2) +
  transition_states(frame, 1, 1) + 
  ggtitle("# retas = {frame}") +
  shadow_mark() +
  enter_grow() +
  exit_shrink(size = 1)

distrib_conj_gif <- animate(distrib_conj, width = 270, height = 270, nframes = 1000, fps = 50, end_pause = 10)

retas_mgif <- image_read("retas_gif.gif")
hist_intercept_mgif <- image_read("hist_intercept_gif.gif")
distrib_conj_gif <- image_read("distrib_conj_gif.gif")
combined_gif <- image_read("combined_gif.gif")
# combined_gif <- image_append(c(retas_mgif[1], hist_intercept_mgif[1]))
pb <- progress::progress_bar$new(total = 400)
for(i in 2:1000){
  pb$tick()
  combined <- image_append(c(retas_mgif[i], hist_intercept_mgif[i]))
  combined_gif <- c(combined_gif, combined)
}

gganimate::save_animation(combined_gif, "combined_gif.gif")

