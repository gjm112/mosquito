library(tidyverse)
altosid <- read.csv("./data/altosid.csv")
sumilarv <- read.csv("./data/sumilarv.csv")[,1:12]
names(sumilarv)[6] <- "Weeks.post"

dat <- rbind(altosid, sumilarv)
sub <- dat %>% group_by(Treatment, Days.Post) %>% summarize(Emerged = sum(Emerged, na.rm = TRUE), 
                                          Pupae = sum(Pupae, na.rm = TRUE), 
                                          pkill = 1 - Emerged / Pupae)

ggplot(aes(x = Days.Post, y = pkill, color = Treatment),data = sub) + geom_point(aes(size = Pupae)) + geom_line()

dat <- dat %>% mutate(kill_prop = 1- Emerged/Pupae)




#Are these different?  If so how?   
#Sumilarv 3 WSP   Sumilarv WSP
#660 dont have a any treatment listed. 


length(table(altosid$Basin.Location))
length(table(sumilarv$Basin.Location))




summary(altosid$Emerged)
summary(sumilarv$Emerged)

summary(altosid$Pupae)
summary(sumilarv$Pupae)

