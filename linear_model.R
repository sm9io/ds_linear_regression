options(digits = 3)
library(dslabs)
library(tidyverse)
library(Lahman)
Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(singles = (H - HR - X2B - X3B) / G, BB = BB/G, HR = HR/G) %>%
  summarise(cor(BB, HR), cor(singles, HR), cor(BB, singles))
dat <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1),
         BB_per_game = BB/G,
         R_per_game = R/G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)
dat %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ HR_strata)
ggsave("plots/BB_R_1.jpg")
dat %>%
  group_by(HR_strata) %>%
  summarise(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))
dat1 <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1),
         HR_per_game = HR/G,
         R_per_game = R/G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9)
dat1 %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ BB_strata)
ggsave("plots/HR_R_1.jpg")
dat1 %>%
  group_by(BB_strata) %>%
  summarise(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game))
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0 + beta1 * galton_heights$father)
  return(sum(resid^2))
}
beta1 = seq(0, 1, len = nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) +
  geom_line(aes(beta1, rss), col = 2)
ggsave("plots/heights_beta1_beta0_25.jpg")
fit <- lm(son ~ father, data = galton_heights)
fit
summary(fit)
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son~ father, data = .) %>% .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])
library("gridExtra")
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = .1, color = "black")
grid.arrange(p1, p2, ncol = 2)
ggsave("plots/heights_beta0_beta1_montecarlo_hist.jpg")
sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son~ father, data = .) %>%
  summary
lse %>% summarise(se_0 = sd(beta_0), se_1 = sd(beta_1))
lse %>% summarize(cor(beta_0, beta_1))
lse_1 <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef
})
cor(lse_1[1,], lse_1[2,])
lse_2 <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father), son = son - mean(son)) %>%
    lm(son ~ father, data = .) %>% .$coef
})
cor(lse_2[1,], lse_2[2,])
galton_heights %>%
  ggplot(aes(father, son))+
  geom_point() +
  geom_smooth(method = "lm")
ggsave("plots/father_son_scatter_confidence.jpg")
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data = .))) %>%
  ggplot(aes(father, Y_hat)) +
  geom_line()
ggsave("plots/father_son_predict.jpg")
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

