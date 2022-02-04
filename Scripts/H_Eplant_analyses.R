#analyzing H~Eplant

library(ggplot2)
library(tidyverse)

data <- read.csv("Data/Cleaned/HeatFlux_21.csv")

ggplot(data, aes(x=Eplant, y=H)) +
  geom_point() 

#simple linear regression with whole data
basic.lm = lm(H ~ Eplant, data=data)
summary(basic.lm)

plot(basic.lm, which = 1) #to see residual distribution
result_basic_lm <- broom::tidy(basic.lm)

ggplot(data, aes(x=Eplant, y=H)) + 
  geom_point() +
  geom_smooth(method='lm', formula = y ~ x, size = 1, col="blue") + 
  xlab('Eplant') +
  ylab('H')

#simple linear regression with few ID´s

random_data <- data %>%
  filter(ID=="P05344"|ID=="P04186"|ID=="P00453"|ID=="P08117"|ID=="P04900"|ID=="P04004") %>%
  select(-"X")

random_mod = lm(H ~ Eplant, data=random_data);AIC(random_mod)
summary(random_mod)

ggplot(random_data, aes(x=Eplant, y=H)) + 
  geom_point() +
  geom_smooth(method='lm', formula = y ~ x, size = 1, col="green") + 
  xlab('Eplant') +
  ylab('H')

#simple linear regression with random data and ID as random factor. As we are analysing mixed effects models of continuous variables we use lmer() function of lme4 package 
library(lme4)

random_mod2 <- lmer(H ~ Eplant + (1|ID), data=random_data)
summary(random_mod2)

#see the difference between HW and non HW

random_data <- random_data %>%
  group_by(ID) %>%
  mutate(Date=as.Date(Date)) %>%
  mutate(heatwave = 
           case_when(Date >= ymd("2021-08-10") & Date < ymd("2018-08-16") ~ "hw",
                    TRUE ~ "nonhw")) #why doesnt filter

random_data <- random_data %>%
  group_by(heatwave) %>%
  mutate(H_mean=mean(H,na.rm=T))  

random_hw <- random_data %>%
  filter(heatwave =="hw")

ggplot(random_data, aes(x=Eplant, y=H)) + 
  geom_point() +
  geom_smooth(method='lm', formula = y ~ x, size = 1, col="green") + 
  xlab('Eplant') +
  ylab('H')

random_non_hw <- random_data %>%
  filter(heatwave == "nonhw")

ggplot(random_data, aes(x=Eplant, y=H)) + 
  geom_point() +
  geom_smooth(method='lm', formula = y ~ x, size = 1, col="green") + 
  xlab('Eplant') +
  ylab('H')

#ploteamos la distribucion de H segun Hw/non Hw
hist(x, main = "Dos variables", ylab = "Frecuencia")
hist(y, add = TRUE, col = rgb(1, 0, 0, alpha = 0.5))

#see H~Eplant according to the type of forest

#Needle-leaved montane forests

write.csv(random_data, "Data/Cleaned/small_HW.csv")





