TIMES_SCORE = read.csv("TIMES_SCORE.csv", encoding="UTF-8")
CWUR_SCORE = read.csv("CWUR_SCORE.csv", encoding="UTF-8")

PRIVATE_CWUR = subset(CWUR_SCORE, CONTROL != "Public")

CWUR_SCORE = subset(CWUR_SCORE, CONTROL == "Public")

require(ggplot2)
qplot(TIMES_SCORE$female_male_ratio, TIMES_SCORE$ADM_RATE_ALL)
qplot(CWUR_SCORE$national_rank, CWUR_SCORE$ADM_RATE_ALL)

qplot(CWUR_SCORE$world_rank, CWUR_SCORE$NPT4_PUB, main = "World Rank vs Price Public", colour = CWUR_SCORE$CONTROL)

reg = lm(CWUR_SCORE$NPT4_PRIV ~ CWUR_SCORE$world_rank)
qplot(CWUR_SCORE$world_rank, CWUR_SCORE$NPT4_PRIV, main = "World Rank vs Price Private", colour = CWUR_SCORE$CONTROL)
abline(reg)

CWUR_SCORE$NPT4_PUB = as.numeric(CWUR_SCORE$NPT4_PUB)
CWUR_SCORE$NPT4_PRIV = as.numeric(CWUR_SCORE$NPT4_PRIV)
CWUR_SCORE[is.na(CWUR_SCORE)] <- 0

CWUR_SCORE$PRICE = CWUR_SCORE$NPT4_PUB + CWUR_SCORE$NPT4_PRIV
  
ggplot(CWUR_SCORE, aes(x=world_rank, y = PRICE)) +
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method=lm) 

ggplot(subset(wrong, NPT4_PRIV > 0), aes(x=world_rank, y = NPT4_PRIV)) +
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method=lm) 

reg = lm(CWUR_SCORE$ADM_RATE_ALL ~ CWUR_SCORE$world_rank)
plot(CWUR_SCORE$world_rank, CWUR_SCORE$ADM_RATE_ALL)
abline(v = 194, col = "red")
abline(h = 0.7861, col = "red")
abline(reg)

reg = lm(CWUR_SCORE$ADM_RATE_ALL, CWUR_SCORE$national_rank)
plot(CWUR_SCORE$national_rank, CWUR_SCORE$ADM_RATE_ALL)
abline(reg)

reg = lm(CWUR_SCORE$NPT4_PUB ~ CWUR_SCORE$world_rank)
plot(CWUR_SCORE$world_rank, CWUR_SCORE$NPT4_PUB)
abline(reg)

reg = lm(CWUR_SCORE$RET_FT4 ~ CWUR_SCORE$world_rank)
plot(CWUR_SCORE$world_rank, CWUR_SCORE$RET_FT4)
abline(reg)

#CWUR_SCORE$gt_25k_p6 = as.numeric(CWUR_SCORE$gt_25k_p6)
reg = lm(CWUR_SCORE$ACTCM75 ~ CWUR_SCORE$world_rank)
plot(CWUR_SCORE$world_rank, CWUR_SCORE$ACTCM75)
abline(reg)

smoothScatter(CWUR_SCORE$national_rank, CWUR_SCORE$ADM_RATE_ALL)

ggplot(PRIVATE_CWUR, aes(x=world_rank, y = ADM_RATE_ALL)) +
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method=lm)

ggplot(PRIVATE_CWUR, aes(x=world_rank, y = NPT4_PRIV)) +
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method=lm)

ggplot(PRIVATE_CWUR, aes(x = SAT_AVG, y = ADM_RATE_ALL)) +
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method=lm) + ggtitle("Admission Rate vs SAT Average")

