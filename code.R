#Are there replicates. 
library(tidyverse)
altosid <- read.csv("./data/altosid.csv")
#Remove columns past 12
sumilarv <- read.csv("./data/sumilarv.csv")[,1:12]
names(sumilarv)[6] <- "Weeks.post"

#Remove rows with NA. 
#Remove the excess columns. 
sumilarv <- sumilarv %>% filter(!is.na(Late))

dat <- rbind(altosid, sumilarv)
sub <- dat %>% group_by(Treatment, Days.Post) %>% summarize(Emerged = sum(Emerged, na.rm = TRUE), 
                                          Pupae = sum(Pupae, na.rm = TRUE), 
                                          pkill = 1 - Emerged / Pupae)

ggplot(aes(x = Days.Post, y = pkill, color = Treatment),data = sub) + geom_point(aes(size = Pupae)) + geom_line()

dat <- dat %>% mutate(kill_prop = 1- Emerged/Pupae)

library(lme4)
a <- glm(cbind(Emerged, Pupae) ~ 0 + Treatment*(Days.Post + I(Days.Post^2) + I(Days.Post^3)), family = "binomial", data = dat)
#a <- glm(cbind(Pupae - Emerged, Pupae) ~ Treatment*(Days.Post), family = "binomial", data = dat)
summary(a)
x
pred <- rbind(data.frame(Treatment = "Sumilarv 3 WSP", Days.Post = c(0:200)),
data.frame(Treatment = "Altosid XR", Days.Post = c(0:100)))

pred$pkillhat <- 1 - predict(a, newdata = pred, type = "response")

ggplot(aes(x = Days.Post, y = pkill, color = Treatment),data = sub) + geom_point(aes(size = Pupae)) + geom_line() + geom_point(aes(x = Days.Post, y = pkillhat, color = Treatment), data= pred)

#Are these different?  If so how?   
#Sumilarv 3 WSP   Sumilarv WSP
#660 dont have a any treatment listed. 


length(table(altosid$Basin.Location))
length(table(sumilarv$Basin.Location))




summary(altosid$Emerged)
summary(sumilarv$Emerged)

summary(altosid$Pupae)
summary(sumilarv$Pupae)




altosid %>% filter(Basin.Location == "Brookhaven & Ridge")
sumilarv %>% filter(Treatment == "")

