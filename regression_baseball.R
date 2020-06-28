options(digits = 3)
library(tidyverse)
library(Lahman)
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
tidy(fit, conf.int = TRUE)
Teams %>%
  filter(yearID %in% 2001) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + geom_point() + geom_abline() + geom_text(nudge_x=0.1, cex = 2)
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))
ggsave("plots/player_run_team_hist_qplot.jpg")
players %>%
  ggplot(aes(R_hat)) +
  geom_histogram(binwidth = .5, color = "black")
ggsave("plots/player_run_team_hist_ggplot.jpg")
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")
players <- Fielding %>%
  filter(yearID == 2002) %>%
  filter(!POS %in% c("OF", "P")) %>%
  group_by(playerID) %>%
  top_n(1, G) %>%
  filter(row_number(G) == 1) %>%
  ungroup() %>%
  select(playerID, POS) %>%
  right_join(players, by = "playerID") %>%
  filter(!is.na(POS) & !is.na(salary))
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  right_join(players, by = "playerID")
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10)
ds_theme_set()
players %>%
  ggplot(aes(salary, R_hat, col = POS)) +
  geom_point() +
  scale_x_log10()
ggsave("plots/player_salary_rhat_pos.jpg")
players %>%
  filter(debut < 1998) %>%
  ggplot(aes(salary, R_hat, col = POS)) +
  geom_point() +
  scale_x_log10()
ggsave("plots/player_salary_rhat_1998_pos.jpg")
