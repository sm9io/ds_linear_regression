options(digits = 3)
library(dslabs)
library(tidyverse)
library(Lahman)
library(broom)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1),
         BB = BB/G, 
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= .4 & HR <= 1.2)
dat %>%
  group_by(HR) %>%
  summarise(slope = cor(BB, R) * sd(R) / sd(BB))
dat %>%
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()
dat %>%
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2],
             se = summary(fit)$coefficient[2,2])
}
dat %>%
  group_by(HR) %>%
  do(get_slope(.))
dat %>%
  group_by(HR) %>%
  do(slope = get_slope(.))
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients,
             se = summary(fit)$coefficient[,2])
}
dat %>%
  group_by(HR) %>%
  do(get_lse(.))
fit <- lm(R ~ BB, data = dat)
tidy(fit)
tidy(fit, conf.int = TRUE)
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE))
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()
ggsave("plots/HR_slope_conf_int.jpg")
glance(fit)
