#Lee Przybylski 2/15/2021
library(ggplot2)
library(plyr)

teams <- read.csv("cbb.csv", header = T, sep = ",")
head(teams)
summary(teams)
#2015-2019 seasons
unique(teams$POSTSEASON)
unique(teams$SEED)
teams$tournament
teams$POSTSEASON <- as.character(teams$POSTSEASON)
head(teams)
unique(teams$POSTSEASON)
teams$Qtournament <- ifelse(!is.na(teams$POSTSEASON), 1,0)
tail(teams)

results <- c("Champions", "2ND", "F4", "E8", "S16", "R32", "R64", "R68")
teams$Q32 <- ifelse(teams$POSTSEASON %in% results[1:6], 1,0)
teams$Q16 <- ifelse(teams$POSTSEASON %in% results[1:5], 1,0)
teams$Q8 <- ifelse(teams$POSTSEASON %in% results[1:4], 1,0)
teams$Q4 <- ifelse(teams$POSTSEASON %in% results[1:3], 1,0)
teams$QF <- ifelse(teams$POSTSEASON %in% results[1:2], 1,0)

#What conferences do we see:
unique(teams$CONF)
power5 <-c("ACC", "B10", "B12", "SEC", "P12")
group5 <- c("Amer", "CUSA", "MWC", "SB", "MAC")
other5 <- c("WCC", "MVC", "Ivy", "Asun", "Ind")

#For each conference, count the number of final 4 appearnaces
confs <- unique(teams$CONF)
final4count <- function(conf){
  v <- teams$Q4[teams$CONF == conf]
  return(sum(v))
}
v <- teams$Q4[]
final4count("ACC")
final4count("B10")

confs <- as.character(unique(teams$CONF))
count <- rep(0, 33)
for (j in 1:33){
  count[j] <- final4count(confs[j])
}
final4s <- data.frame(confs = confs, count = count)
head(final4s)
summary(final4s)
final4s <- final4s[final4s$count >0, ]
final4s
#plot the final 4 counts
ggplot(data = final4s, aes(x = confs, y = count)) + geom_bar(stat = "identity")+
  labs(title = "Final 4 Appearances 2015 - 2019", xlab = "conference")

head(teams)
#Boxplot for offensive efficiency by conference
ggplot(data = teams, aes(x = CONF, y = ADJOE)) + geom_boxplot()
df_P5 <- teams[teams$CONF %in% power5,]
df_G5 <- teams[teams$CONF %in% group5,]
df_O5 <- teams[teams$CONF %in% other5,]
ggplot(data = df_P5, aes(x = CONF, y = ADJOE)) + geom_boxplot() + 
  labs(title = "Offensive Efficiency")
ggplot(data = df_G5, aes(x = CONF, y = ADJOE)) + geom_boxplot()+ 
  labs(title = "Offensive Efficiency")
ggplot(data = df_O5, aes(x = CONF, y = ADJOE)) + geom_boxplot()+ 
  labs(title = "Offensive Efficiency")

#Boxplot for deffencsive efficiency by conference
ggplot(data = df_P5, aes(x = CONF, y = ADJDE)) + geom_boxplot() + 
  labs(title = "Offensive Efficiency")
ggplot(data = df_G5, aes(x = CONF, y = ADJOE)) + geom_boxplot()+ 
  labs(title = "Offensive Efficiency")
ggplot(data = df_O5, aes(x = CONF, y = ADJOE)) + geom_boxplot()+ 
  labs(title = "Offensive Efficiency")

#Boxplot for tempo by conference
ggplot(data = df_P5, aes(x = CONF, y = ADJ_T)) + geom_boxplot() + 
  labs(title = "Adjusted Tempo")
ggplot(data = df_G5, aes(x = CONF, y = ADJ_T)) + geom_boxplot()+ 
  labs(title = "Adjusted Tempo")
ggplot(data = df_O5, aes(x = CONF, y = ADJ_T)) + geom_boxplot()+ 
  labs(title = "Adjusted Tempo")


#Plot where teams end up in the post season based on offense or defense
ggplot(data = teams, aes(x = ADJOE, y = ADJDE, col = POSTSEASON)) + geom_point(size = 2)

prob_16 <- glm(Q16 ~ ADJOE + ADJDE + ADJ_T, family = binomial(link = logit), data = teams)
summary(prob_16)

#Plot where all the teams S16 teams land in ADJOE, ADJDE
ggplot(data = teams, aes(x = ADJOE, y = ADJDE, col = Q16)) + geom_point(size = 2)

prob_4 <- glm(Q4 ~ ADJOE + ADJDE + ADJ_T, family = binomial(link = logit), data = teams)
summary(prob_4)

#Plot where all the teams F16 teams land in ADJOE, ADJDE
ggplot(data = teams, aes(x = ADJOE, y = ADJDE, col = Q4)) + geom_point(size = 2)

teams[teams$Q4 == 1,]

#Model to determine how factors relate to power rating
power_model <- lm(BARTHAG ~ ADJOE + ADJDE + ADJ_T, data = teams)
summary(power_model)
