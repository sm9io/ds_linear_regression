options(digits = 3)
set.seed(1989)
library(HistData)
data("GaltonFamilies")
female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
mm <- mean(female_heights$mother)
mm
s_m <- sd(female_heights$mother)
s_m
md <- mean(female_heights$daughter)
md
s_d <- sd(female_heights$daughter)
s_d
r_md <- cor(female_heights$mother, female_heights$daughter)
r_md
m_md <- r_md * s_d / s_m
m_md
b_md <- md - m_md * mm
b_md
r_md_2 <- r_md^2*100
round(r_md_2)
m_md * 60 + b_md
