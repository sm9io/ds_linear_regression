library(dslabs)
library(tidyverse)
library(HistData)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
galton_heights %>%
  gather(key = "relation", value = "height") %>%
  group_by(relation) %>%
  summarise(mean = mean(height), sd = sd(height))
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.3)
ggsave("plots/father_son_height_1.jpg")
galton_heights %>% summarise(correlation = cor(father, son))
set.seed(1, sample.kind = "Rounding")
r <- replicate(10000, {
  sample_n(galton_heights, 25, replace = TRUE) %>%
    summarise(r = cor(father, son)) %>%
    .$r
})
data.frame(r) %>%
  ggplot(aes(r)) +
  geom_density(fill = "darkgrey", col = "black")
ggsave("plots/cor_montecarlo_density.jpg")
data.frame(r) %>%
  ggplot(aes(r)) +
  geom_histogram(binwidth = 0.05, fill = "darkgrey", col = "black")
ggsave("plots/cor_montecarlo_histogram.jpg")
mean(r)
sd(r)
data.frame(r) %>%
  ggplot(aes(sample = r)) +
  stat_qq() +
  geom_abline(intercept = mean(r), slope = sqrt((1-mean(r)^2)/(25-2)))
ggsave("plots/cor_montecarlo_qq.jpg")