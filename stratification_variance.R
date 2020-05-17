library(dslabs)
library(tidyverse)
library(HistData)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
conditional_avg <- galton_heights %>% filter(round(father) == 72) %>%
  summarise(avg_son = mean(son)) %>%
  .$avg_son
conditional_avg
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()
ggsave("plots/father_son_strata_hist.jpg")
galton_heights %>% mutate(father = round(father)) %>%
  group_by(father) %>%
  summarise(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()
ggsave("plots/father_son_strata_scatter_mean.jpg")
r1 <- galton_heights %>% summarise(r = cor(father, son)) %>% .$r
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarise(son = mean(son)) %>%
  mutate(z_father = scale(father), z_son = scale(son)) %>%
  ggplot(aes(z_father, z_son)) +
  geom_point() +
  geom_abline(intercept = 0, slope = r1)
ggsave("plots/father_son_strata_scatter_mean_line.jpg")
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r2 <- cor(galton_heights$father, galton_heights$son)
m1 <- r2 * mu_y / mu_x
b1 <- mu_y - m * mu_x
m2 <- r2 * mu_x / mu_y
b2 <- mu_x - m * mu_y
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b1, slope = m1)
ggsave("plots/father_son_scatter_line.jpg")
galton_heights %>%
  ggplot(aes(son, father)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b2, slope = m2)
ggsave("plots/father_son_scatter_line_2.jpg")
galton_heights %>%
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r2)
ggsave("plots/father_son_scaled_scatter_line.jpg")
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +
  stat_qq(aes(sample = son)) +
  facet_wrap(~z_father)
ggsave("plots/father_son_strata_qq.jpg")
