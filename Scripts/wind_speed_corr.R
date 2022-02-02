### 
###
###
### Wind Speed
library(tidyverse)
library(ggplot2)
library(lubridate)

wsdat <- read.csv("Data/XEMA/wind_speed_xema.csv")

### Make the dataset smaller

wsdat2 <- wsdat %>%
  dplyr::select(c("CODI_ESTACIO","DATA_LECTURA","VALOR_LECTURA")) %>%
  mutate(Date=lubridate::dmy_hms(DATA_LECTURA)) %>%
  mutate(Hour=hour(Date),Month=month(Date),Year=year(Date),doy=lubridate::yday(Date)) %>% #yday function gives the day of the year
  filter(Month>4 & Month <10)

daily_means_ws <- wsdat2 %>%
  group_by(doy,Year,CODI_ESTACIO) %>%   #we group by year, day of the year and station code
  summarise(daily_ws=mean(VALOR_LECTURA,na.rm=T)) #we get the mean omiting na

midday <- wsdat2 %>% 
  filter (Hour>11 & Hour <17) %>%
  left_join(daily_means_ws,by=c("CODI_ESTACIO","doy","Year"))

summary(model1<-lm(VALOR_LECTURA~daily_ws,midday,AIC))   
results1 <- broom::tidy(model1)

model2 <- lm(VALOR_LECTURA~daily_ws + I(daily_ws^2),data=midday);AIC(model2)
summary(model2)
results2 <- broom::tidy(model2)

model3 <- lm(VALOR_LECTURA~daily_ws + I(daily_ws^2) + I(daily_ws^3),data=midday);AIC(model3)
summary(model3)
results3 <- broom::tidy(model3)

model4 <- lm(VALOR_LECTURA~daily_ws + I(daily_ws^2) + I(daily_ws^3) + I(daily_ws^4),data=midday);AIC(model4)
summary(model4)
results4 <- broom::tidy(model4)

model5 <- lm(VALOR_LECTURA~daily_ws + I(daily_ws^2) + I(daily_ws^3) + I(daily_ws^4) + I(daily_ws^5),data=midday);AIC(model5)
summary(model5)
results5 <- broom::tidy(model5)

model6 <- lm(VALOR_LECTURA~daily_ws + I(daily_ws^2) + I(daily_ws^3) + I(daily_ws^4)  + I(daily_ws^5) + I(daily_ws^6),data=midday);AIC(model6)
summary(model6)
results6 <- broom::tidy(model6)

model_results <- rbind(model1,model2,model3,model4,model5,model6) 

model7 <- lm(VALOR_LECTURA~log(daily_ws),data=midday, AIC(model7))
summary(model7)

ggplot(midday,aes(x=daily_ws,y=VALOR_LECTURA))+
  geom_smooth(method=loess)

### Coeffcients

### Intercept 3.963
#1.553
#-0.126
#0.0057