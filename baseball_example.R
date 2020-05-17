library(Lahman)
library(dslabs)
library(tidyverse)
# runs per game vs home runs per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.2)
ggsave("plots/runs per game vs home runs per game.jpg")
# runs per game vs stolen bases per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB/G, R_per_game = R/G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.2)
ggsave("plots/runs per game vs stolen bases per game.jpg")
# runs per game vs bases on balls per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.2)
ggsave("plots/runs per game vs bases on balls per game.jpg")
