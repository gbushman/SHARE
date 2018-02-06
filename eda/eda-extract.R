---
title: "SHARE Data"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Initialize/ Import/ Recode
```{r init}
# load libraries
library(tidyverse)
library(haven)
library(psych)
library(psy)
library(car)
library(corrplot)

# load data
#setwd("C:/Users/Greg/Documents/Research/PRC/SHARE")
df <- read_spss("//umroot/home/gbushman/Desktop/SHARE Analysis/CDC SHARE Year1234 merged 11-15-17 with scales.sav")

# recode items that need it
df$sex <- factor(df$W1_q1, levels = c(1, 0), labels = c("male", "female"))

df$W1_q27er <- recode(df$W1_q27e, '0=4; 1=3; 2=2; 3=1; 4=0; else = NA', as.numeric = T)
df$W1_q27cr <- recode(df$W1_q27c, '0=4; 1=3; 2=2; 3=1; 4=0; else = NA', as.numeric = T)
df$W1_q27dr <- recode(df$W1_q27d, '0=4; 1=3; 2=2; 3=1; 4=0; else = NA', as.numeric = T)

df$W1_q37gr <- recode(df$W1_q37g, '0=4; 1=3; 2=2; 3=1; 4=0; else = NA', as.numeric = T)
df$W1_q37kr <- recode(df$W1_q37k, '0=4; 1=3; 2=2; 3=1; 4=0; else = NA', as.numeric = T)
df$W1_q37lr <- recode(df$W1_q37l, '0=4; 1=3; 2=2; 3=1; 4=0; else = NA', as.numeric = T)

df$W1_q38gr <- recode(df$W1_q38g, '0=4; 1=3; 2=2; 3=1; 4=0; else = NA', as.numeric = T)
df$W1_q38kr <- recode(df$W1_q38k, '0=4; 1=3; 2=2; 3=1; 4=0; else = NA', as.numeric = T)
df$W1_q38lr <- recode(df$W1_q38l, '0=4; 1=3; 2=2; 3=1; 4=0; else = NA', as.numeric = T)
```

# Create Vectors for Subsetting Scales
```{r define-scales}
## drug use
aod_cols <- c(
  "W1_q31af1", 
  "W1_q31ag1", 
  "W1_q31ah1", 
  "W1_q31ai1"
  )

## non-partner violent behavior (wave1:q31, wave2: wave4:q33)
npvb_w1_cols <- c(
  "W1_q31i1", 
  "W1_q31k1", 
  "W1_q31m1", 
  "W1_q31q1",
  "W1_q31r1", 
  "W1_q31s1", 
  "W1_q31v1", 
  "W1_q31w1",
  "W1_q31x1"
  )

npvb_w2_cols <- c(
  "W2_q31i1", 
  "W2_q31k1", 
  "W2_q31m1", 
  "W2_q31q1",
  "W2_q31r1", 
  "W2_q31s1", 
  "W2_q31v1", 
  "W2_q31w1",
  "W2_q31x1"
  )

npvb_w3_cols <- c(
  "W3_q31i1", 
  "W3_q31k1", 
  "W3_q31m1", 
  "W3_q31q1",
  "W3_q31r1", 
  "W3_q31s1", 
  "W3_q31v1", 
  "W3_q31w1",
  "W3_q31x1"
  )

npvb_w4_cols <- c(
  "W4_q33i1", 
  "W4_q33k1", 
  "W4_q33m1", 
  "W4_q33q1",
  "W4_q33r1", 
  "W4_q33s1", 
  "W4_q33v1", 
  "W4_q33w1",
  "W4_q33x1"
  )

## teen intimate partner violence
teen_ipv_cols <- c(
  "W1_q32ac1", 
  "W1_q32ad1", 
  "W1_q32ae1", 
  "W1_q32af1",
  "W1_q32ag1", 
  "W1_q32ah1", 
  "W1_q32ai1", 
  "W1_q32aj1",
  "W1_q32ak1", 
  "W1_q32al1", 
  "W1_q32am1", 
  "W1_q32an1",
  "W1_q32ao1", 
  "W1_q32ap1", 
  "W1_q32aq1"
  )

## masculine discrepancy stress
masc_stress_cols <- c(
  "W1_q37a",
  "W1_q37b",
  "W1_q37c",
  "W1_q37d",
  "W1_q37e",
  #"W1_q37f",
  #"W1_q37gr", # recoded
  "W1_q37h",
  "W1_q37i",
  #"W1_q37j",
  #"W1_q37kr", # recoded
  #"W1_q37lr", # recoded
  "W1_q37m",
  "W1_q37n",
  "W1_q37o",
  "W1_q37p"
)

## fem discrepancy stress
fem_stress_cols <- c(
  "W1_q38a",
  "W1_q38b",
  "W1_q38c",
  "W1_q38d",
  "W1_q38e",
  #"W1_q38f",
  #"W1_q38gr", # recoded
  "W1_q38h",
  "W1_q38i",
  #"W1_q38j",
  #"W1_q38kr", # recoded
  #"W1_q38lr", # recoded
  "W1_q38m",
  "W1_q38n",
  "W1_q38o",
  "W1_q38p"
)

## peer support
peer_supp_cols <- c(
  "W1_q18d", 
  "W1_q18e", 
  "W1_q18f"
  )

## parental support
prnt_supp_cols <- c(
  "W1_q18a", 
  "W1_q18b", 
  "W1_q18c"
  )

## parental supervision
prnt_suprv_cols <- c(
  "W1_q17a", 
  "W1_q17b", 
  "W1_q17c", 
  "W1_q17d", 
  "W1_q17e", 
  "W1_q17f", 
  "W1_q17g", 
  "W1_q17h"
  )

## parental bonding
prnt_bond_cols <- c(
  "W1_q17i", 
  "w1_q17jr", # recoded
  "w1_q17kr", # recoded
  "w1_q17lr", # recoded
  "W1_q17m"
  )

## neighborhood safety
nbhood_safe_cols <- c(
  "W1_q27b", 
  "W1_q27er" # recoded
  )

## school safety
school_safe_cols <- c(
  "W1_q27cr", # recoded
  "W1_q27dr"  # recoded
  )

## mental health (depression/anxiety)
dep_anx_cols <- c(
  "W1_q29a", 
  "W1_q29b", 
  "W1_q29c", 
  "W1_q29d", 
  "W1_q29e", 
  "W1_q29f", 
  "W1_q29g"
  )

## ACES
aces_cols <- c(
  "W1_q46a",
  "W1_q46b",
  "W1_q46c",
  "W1_q46d",
  "W1_q46e",
  "W1_q46f",
  "W1_q46g",
  "W1_q46h",
  "W1_q46i",
  "W1_q46j",
  "W1_q46k",
  "W1_q46l",
  "W1_q46m",
  "W1_q46n",
  "W1_q46o",
  "W1_q46p",
  "W1_q46q",
  "W1_q46r"
)
```

# AOD
```{r aod}
# subset items
aod <- df %>%
  select(one_of(aod_cols)) 

# correlation
corrplot(cor(aod, use = "complete.obs"), method = "square")

# reliability
alpha(cov(aod, use = "complete.obs"))
```

# Non-partner Violent Behavior
```{r non-part-viol}
## Wave 1
# subset items
npvb_w1 <- df %>%
  select(one_of(npvb_w1_cols))

# correlation
corrplot(cor(npvb_w1, use = "complete.obs"), method = "square")

# reliability
alpha(cov(npvb_w1, use = "complete.obs"))


## Wave 2
# subset items
npvb_w2 <- df %>%
  select(one_of(npvb_w2_cols))

# correlation
corrplot(cor(npvb_w2, use = "complete.obs"), method = "square")

# reliability
alpha(cov(npvb_w2, use = "complete.obs"))


## Wave 3
# subset items
npvb_w3 <- df %>%
  select(one_of(npvb_w3_cols))

# correlation
corrplot(cor(npvb_w3, use = "complete.obs"), method = "square")

# reliability
alpha(cov(npvb_w3, use = "complete.obs"))


## Wave 4
# subset items
npvb_w4 <- df %>%
  select(one_of(npvb_w4_cols))

# correlation
corrplot(cor(npvb_w4, use = "complete.obs"), method = "square")

# reliability
alpha(cov(npvb_w4, use = "complete.obs"))
```

# Teen Dating Violence
```{r teen-dating-viol}
# subset items
teen_ipv <- df %>%
  select(one_of(teen_ipv_cols))

# correlation
corrplot(cor(teen_ipv, use = "complete.obs"), method = "square")

# reliability
alpha(cov(teen_ipv, use = "complete.obs"))
```

# Masculine Discrepency Stress
```{r masc-disc-stress}
# subset items
masc_stress <- df %>%
  select(one_of(masc_stress_cols))

# correlation
corrplot(cor(masc_stress, use = "complete.obs"), method = "square")

# reliability
alpha(cov(masc_stress, use = "complete.obs"))
```

# Feminine Discrepency Stress
```{r fem-disc-stress}
# subset items
fem_stress <- df %>%
  select(one_of(fem_stress_cols))

# correlation
corrplot(cor(fem_stress, use = "complete.obs"), method = "square")

# reliability
alpha(cov(fem_stress, use = "complete.obs"))
```

# Parental Support
```{r prnt-support}
# subset items
prnt_supp <- df %>%
  select(one_of(prnt_supp_cols))

# correlation
corrplot(cor(prnt_supp, use = "complete.obs"), method = "square")

# reliability
alpha(cov(prnt_supp, use = "complete.obs"))
```

# Peer Support
```{r peer-support}
# subset items
peer_supp <- df %>%
  select(one_of(peer_supp_cols))

# correlation
corrplot(cor(peer_supp, use = "complete.obs"), method = "square")

# reliability
alpha(cov(peer_supp, use = "complete.obs"))
```

# Parental Supervision
```{r prnt-supervision}
# subset items
prnt_suprv <- df %>%
  select(one_of(prnt_suprv_cols))

# correlation
corrplot(cor(prnt_suprv, use = "complete.obs"), method = "square")

# reliability
alpha(cov(prnt_suprv, use = "complete.obs"))
```

# Parental Bonding
```{r prnt-bonding}
# subset items
prnt_bond <- df %>%
  select(one_of(prnt_bond_cols))

# correlation
corrplot(cor(prnt_bond, use = "complete.obs"), method = "square")

# reliability
alpha(cov(prnt_bond, use = "complete.obs"))
```

# Neighborhood Safety
```{r nbhood-safety}
# subset items
nbhood_safe <- df %>%
  select(one_of(nbhood_safe_cols))

# correlation
corrplot(cor(nbhood_safe, use = "complete.obs"), method = "square")

# reliability
alpha(cov(nbhood_safe, use = "complete.obs"))
```

# School Safety
```{r school-safety}
# subset items
school_safe <- df %>%
  select(one_of(school_safe_cols))

# correlation
corrplot(cor(school_safe, use = "complete.obs"), method = "square")

# reliability
alpha(cov(school_safe, use = "complete.obs"))
```

# Depression/Anxiety
```{r depression-anx}
# subset items
dep_anx <- df %>%
  select(one_of(dep_anx_cols))

# correlation
corrplot(cor(dep_anx, use = "complete.obs"), method = "square")

# reliability
alpha(cov(dep_anx, use = "complete.obs"))
```

# Create Scales and SHARE Extract
```{r create-scales}
share_extract <- df %>%
  mutate(
    # drug use
    aod = rowMeans(.[aod_cols], na.rm = T),
    # non-partner violent behavior
    npvb_w1 = rowMeans(.[npvb_w1_cols], na.rm = T),
    npvb_w2 = rowMeans(.[npvb_w2_cols], na.rm = T),
    npvb_w3 = rowMeans(.[npvb_w3_cols], na.rm = T),
    npvb_w4 = rowMeans(.[npvb_w4_cols], na.rm = T),
    # teen intimate partner violence
    teen_ipv = rowMeans(.[teen_ipv_cols], na.rm = T),
    # masculine discrepancy stress
    masc_stress = rowMeans(.[masc_stress_cols], na.rm = T),
    # feminine discrepancy stress
    fem_stress = rowMeans(.[fem_stress_cols], na.rm = T),    
    # peer support
    peer_supp = rowMeans(.[peer_supp_cols], na.rm = T),
    # parental support
    prnt_supp = rowMeans(.[prnt_supp_cols], na.rm = T),
    # parental supervision
    prnt_suprv = rowMeans(.[prnt_suprv_cols], na.rm = T),
    # parental bonding
    prnt_bond = rowMeans(.[prnt_bond_cols], na.rm = T),
    # neighborhood safety
    nbhood_safe = rowMeans(.[nbhood_safe_cols], na.rm = T),
    # school safety
    school_safe = rowMeans(.[school_safe_cols], na.rm = T),
    # mental health (depression/anxiety)
    dep_anx = rowMeans(.[dep_anx_cols], na.rm = T),
    # ACES
    aces = rowSums(.[aces_cols], na.rm = T)
  ) %>%
  select(
    surveyIDm,
    sex,
    W1_q2,
    cohort,
    W1_q3,
    W2_q3,
    W3_q3,
    W4_q3,
    aod,
    npvb_w1,
    npvb_w2,
    npvb_w3,
    npvb_w4,
    teen_ipv,
    masc_stress,
    fem_stress,
    peer_supp,
    prnt_supp,
    prnt_suprv,
    prnt_bond,
    school_safe,
    nbhood_safe,
    dep_anx,
    aces
  )
```

```{r, eval = FALSE}
write_csv(share_extract, "//umroot/home/gbushman/Desktop/SHARE Analysis/SHARE-extract-20180119.csv")
```
