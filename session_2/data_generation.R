library(rgeoboundaries)
library(tidyverse)
library(lubridate)

KSA_bound <- geoboundaries('Saudi Arabia', adm_lvl = 'adm1') 
KSA_bound$shapeName <- c('Eastern', 'Najran', 'Northern Borders', 'Ha’il', 'Riyadh', 'Asir', 'Mecca', 'Tabuk', 'Medina', 'Qasim', 'Al Bahah', 'Jizan', 'Al Jawf')
KSA_bound$population <- c(4105780, 505652, 320524, 597144, 6777146, 1913392, 6915006, 791535, 1777933, 1215858, 411888, 1365110, 440009)

times <- 36

log_mean_poisson <- function (t, i, y_prev, pop_frac) {
  b_0 <- 0 
  b_1 <- 0.9
  b_2 <- 1.15
  b_3 <- -0.1
  (b_0 + pop_frac[i] + (b_1 * log(y_prev[i] + 1)) + (b_2 * sin((2 * pi / 12) * t)) + (b_3 * cos((2 * pi / 12) * t)))
}

y <- matrix(data = NA, nrow = nrow(KSA_bound), ncol = times)
cond_m <- matrix(data = NA, nrow = nrow(KSA_bound), ncol = times)

set.seed(0)
y_prev <- rpois(n = nrow(KSA_bound), lambda = 20)
pop_frac <- KSA_bound$population / (sum(KSA_bound$population))

for (t in (1:times)) {
  for (i in (1:(nrow(KSA_bound)))) {
    cond_m[i, t] <- log_mean_poisson(t, i, y_prev, pop_frac)
    y[i, t] <- rpois(n = 1, lambda = exp(cond_m[i, t]))
  } 
  y_prev <- y[, t]
}

plot(NA, xlim = c(1, times), ylim = c(1, max(y)))
for(i in 1:nrow(KSA_bound)) { lines(1:times, y[i, ], type = 'l', col = i) }

y <- t(y)
colnames(y) <- KSA_bound$shapeName
y <- as_tibble(y)
y <- y %>% pivot_longer(cols = everything(), names_to = 'region', values_to = 'n_cases')
y <- y %>% add_column(date = rep(seq(ymd('2018-01-01'), ymd('2020-12-01'), by = '1 month'), each = nrow(KSA_bound)))
y <- y %>% select(c(date, region, n_cases))
y <- y %>% add_column(pop = rep(KSA_bound$population, times = times))
y <- y %>% add_column(men_prop = rep(rbeta(n = nrow(KSA_bound), shape1 = 10, shape2 = 10), times = times))
y <- y %>% add_column(n_deaths = floor(rbeta(n = nrow(KSA_bound) * times, shape1 = 10, shape2 = 50) * y$n_cases))
y                    

write.csv(x = y, file = 'data.csv', row.names = F)

ggplot(data = KSA_bound) + 
  geom_sf() + 
  geom_sf_label(aes(label = shapeName)) +
  theme_bw() + 
  labs(x = 'Longitude', y = 'Latitude') + 
  theme(text = element_text(family = 'LM Roman 10'), axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), 'mm')), axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), 'mm')))
