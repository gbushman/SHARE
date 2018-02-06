---
title: "SHARE Analysis"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = F, message = F, fig.width = 8)
```

```{r init, include = FALSE}
# Initialize / Import / Recode
# load libraries
library(tidyverse)
library(psych)
library(scales)
library(knitr)
library(lme4)

# load data
df <- read_csv("C:/users/gbushman/Desktop/SHARE Analysis/SHARE-extract-20180119.csv")
df <- df %>% filter(cohort != 9)
```

# Summaries / Descriptive Statistics
#### Summary by Gender / Cohort, with Attrition
```{r, demo-summary}
# breakdown by gender/cohort
sex_attrition <- df %>% 
  group_by(sex) %>% 
  summarise(
    n = n(), 
    w2_attr = percent(sum(is.na(W2_q3)/n)),
    w3_attr = percent(sum(is.na(W3_q3)/n)),
    w4_attr = percent(sum(is.na(W4_q3)/n))
  ) %>%
  rename(group = sex) %>%
  mutate(variable = "sex") %>%
  select(variable, group, n, w2_attr, w3_attr, w4_attr)

cohort_attrition <- df %>% 
  group_by(cohort) %>% 
  summarise(
    n = n(),
    w2_attr = percent(sum(is.na(W2_q3)/n)),
    w3_attr = percent(sum(is.na(W3_q3)/n)),
    w4_attr = percent(sum(is.na(W4_q3)/n))
  ) %>%
  rename(group = cohort) %>%
  mutate(
    variable = "cohort",
    group = as.character(group)
  ) %>%
  select(variable, group, n, w2_attr, w3_attr, w4_attr)

demographics <- bind_rows(sex_attrition, cohort_attrition)

kable(demographics)
```

#### Summary of Scaled Variables
```{r, variable-summary}
# summary of scaled variables
summary <- describe(df[,9:24]) %>%
  select(n, mean, sd, median, min, max)

summary <- cbind(rownames(summary), data.frame(summary, row.names=NULL))

colnames(summary)[1] <- "variables"

kable(summary, digits = 2)
```

# Violent Behavior
```{r}
npvb_melt <- df %>%
  gather(
    key = "wave", 
    value = "npvb", 
    c(npvb_w1, npvb_w2, npvb_w3, npvb_w4)
  ) %>%
  mutate(
    wave = tolower(wave),
    wave = gsub("npvb_", "", .$wave)
  ) %>%
  select(-c(W1_q3, W2_q3, W3_q3, W4_q3))

grade_melt <- df %>%
  gather(
    key = "wave", 
    value = "grade", 
    c(W1_q3, W2_q3, W3_q3, W4_q3)
  ) %>%
  select(surveyIDm, wave, grade) %>%
  mutate(
    grade = grade + 5,
    wave = gsub("_q.+", "", .$wave),
    wave = tolower(wave)
  )

npvb_frame <- left_join(npvb_melt, grade_melt, by = c("surveyIDm", "wave"))
```

```{r, npvb-box}
npvb_plot1 <- npvb_frame %>%
  filter(!is.na(grade)) %>%
  ggplot(aes(x = factor(grade), y = npvb)) +
  geom_boxplot() +
  labs(x = "Grade", y = "Violent Behavior")

npvb_plot1
```

```{r, npvb-line}
npvb_plot2 <- npvb_frame %>%
  filter(!is.na(grade)) %>%
  ggplot(aes(x = grade, y = npvb)) +
  geom_smooth(method = "lm") +
  labs(x = "Grade", y = "Violent Behavior") +
  scale_y_continuous(limits = c(0,4)) +
  scale_x_continuous(breaks = seq(6, 12, 1))

npvb_plot2
```

#Predictor Variables
#### Distributions
```{r}
df %>% 
  select(aod, masc_stress:dep_anx) %>%
  gather(key = "scales", value = "scores") %>%
  ggplot(aes(x = scores)) +
  geom_density() +
  facet_wrap(~scales)

df %>% 
  ggplot(aes(x = teen_ipv)) +
  geom_density()

df %>% 
  ggplot(aes(x = aces)) +
  geom_density()
```

<!-- #### Over Time -->
<!-- ```{r} -->
<!-- npvb_frame %>%  -->
<!--   select(aod:dep_anx, grade) %>% -->
<!--   gather(key = "scales", value = "scores", -c(grade)) %>% -->
<!--   ggplot(aes(x = grade, y = scores)) + -->
<!--   geom_col() + -->
<!--   facet_wrap(~scales) + -->
<!--   scale_y_continuous(limits = c(0,5)) -->

<!-- npvb_frame %>% -->
<!--   ggplot(aes(x = grade, y = aces)) + -->
<!--   geom_smooth(method = "lm", se = F) + -->
<!--   scale_y_continuous(limits = c(0, 18)) -->
<!-- ``` -->

# Mixed Model to Check for Subject-Level Variance
```{r, eval = T}
# mixed model 1 (linear)
npvb_frame <- filter(npvb_frame, !is.na(grade), !is.na(surveyIDm), !is.na(npvb))

npvb_frame$surveyIDm <- factor(npvb_frame$surveyIDm)

mm1 <- lmer(npvb ~ grade + (grade | surveyIDm), npvb_frame)

summary(mm1)
```

# Preliminary Linear Models (Fixed)
```{r, eval = T}
# fixed model 1 (linear)
fm1 <- lm(npvb ~ grade, npvb_frame)

summary(fm1)

# fixed model 2 (linear)
fm2 <- lm(npvb_w4 ~ 
            sex + 
            aod + 
            peer_supp + 
            prnt_supp + 
            prnt_suprv + 
            prnt_bond + 
            school_safe +
            nbhood_safe +
            dep_anx + 
            aces,
          data = df)

summary(fm2)
```
