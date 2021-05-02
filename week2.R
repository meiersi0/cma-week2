library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

setwd("C:/Users/simon/Desktop/Simon/Simon/MSc/Pattern and Trends/week2/week2-rexercise")

wildschwein_BE <- read_delim("wildschwein_BE_2056.txt",",")
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)
wildschwein_BE

#Task 1
wildschwein_BE <- group_by(wildschwein_BE,TierID)
wildschwein_BE <- mutate(wildschwein_BE,timelag = as.numeric(difftime(DatetimeUTC, lead(DatetimeUTC), units = "secs")))
wildschwein_BE <- mutate(wildschwein_BE,timelag = as.integer(difftime(DatetimeUTC, lead(DatetimeUTC), units = "secs")))

wildschwein_BE

#How many individuals were tracked? -->3
str(wildschwein_BE)
  

#For how long were the individual tracked? Are there gaps? --> Wieso geht das nicht?
wildschwein_BE %>%
  group_by(TierName) %>%
  summarise(         
    total_obs = sum(timelag)
  )
    
#Were all individuals tracked concurrently or sequentially? --> Most of the time Yes (1st Qu. 896 3rd QU. 916 sec) But there are Exceptions
summary(wildschwein_BE)

#What is the temporal sampling interval between the locations? --> Mean 1408 sec

#Task 2 
#calculate Steplength

wildschwein_BE <- wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(steplength_E = E - lag(E))%>%
  mutate(steplength_N = N - lag(N))

wildschwein_BE <- wildschwein_BE %>%
  mutate(steplength_tot = (sqrt((steplength_E*steplength_E)+(steplength_N*steplength_N))))


#calculate speed
wildschwein_BE <- wildschwein_BE %>%
  mutate(speed = (steplength_tot/timelag))

#What speed unit do you get? m/s


#Task 3
caro <- read_delim("caro60.txt",",")
caro

caro_3 <- caro%>%
  slice(which(row_number() %% 3 == 1))
caro_6 <- caro%>%
  slice(which(row_number() %% 6 == 1))
caro_9 <- caro%>%
  slice(which(row_number() %% 9 == 1))

#caro
#Calculate timelag
caro <- mutate(caro,timelag = as.integer(difftime(DatetimeUTC,lag(DatetimeUTC), units = "secs")))

#calculate Steplength
caro <- caro %>%
  mutate(steplength_E = E - lag(E))%>%
  mutate(steplength_N = N - lag(N))
caro <- caro %>%
  mutate(steplength_tot = (sqrt((steplength_E*steplength_E)+(steplength_N*steplength_N))))

#calculate speed
caro <- caro %>%
  mutate(speed = (steplength_tot/timelag))

#caro_3
caro_3 <- mutate(caro_3,timelag = as.integer(difftime(DatetimeUTC,lag(DatetimeUTC), units = "secs")))

caro_3 <- caro_3 %>%
  mutate(steplength_E = E - lag(E))%>%
  mutate(steplength_N = N - lag(N))
caro_3 <- caro_3 %>%
  mutate(steplength_tot = (sqrt((steplength_E*steplength_E)+(steplength_N*steplength_N))))

caro_3 <- caro_3 %>%
  mutate(speed = (steplength_tot/timelag))

#caro_6
caro_6 <- mutate(caro_6,timelag = as.integer(difftime(DatetimeUTC,lag(DatetimeUTC), units = "secs")))

caro_6 <- caro_6 %>%
  mutate(steplength_E = E - lag(E))%>%
  mutate(steplength_N = N - lag(N))
caro_6 <- caro_6 %>%
  mutate(steplength_tot = (sqrt((steplength_E*steplength_E)+(steplength_N*steplength_N))))

caro_6 <- caro_6 %>%
  mutate(speed = (steplength_tot/timelag))

#caro_9
caro_9 <- mutate(caro_9,timelag = as.integer(difftime(DatetimeUTC,lag(DatetimeUTC), units = "secs")))

caro_9 <- caro_9 %>%
  mutate(steplength_E = E - lag(E))%>%
  mutate(steplength_N = N - lag(N))
caro_9 <- caro_9 %>%
  mutate(steplength_tot = (sqrt((steplength_E*steplength_E)+(steplength_N*steplength_N))))

caro_9 <- caro_9 %>%
  mutate(speed = (steplength_tot/timelag))

#compare caro to caro_3
caro_3 <- caro_3%>% mutate(TierName = "Caro_3")
caro_rbind_3 <- rbind(caro, caro_3)

ggplot(caro_rbind_3, aes(E,N, colour = TierName)) +
  geom_point()+
  geom_path()+
  theme(aspect.ratio=1)+
  labs(title = "comparing original- with 3 minutes-resampled data")
  
#compare caro to caro_6
caro_6 <- caro_6%>% mutate(TierName = "Caro_6")
caro_rbind_6 <- rbind(caro, caro_6)

ggplot(caro_rbind_6, aes(E,N, colour = TierName)) +
  geom_point()+
  geom_path()+
  theme(aspect.ratio=1)+
  labs(title = "comparing original- with 6 minutes-resampled data")

#compare caro to caro_9
caro_9 <- caro_9%>% mutate(TierName = "Caro_9")
caro_rbind_9 <- rbind(caro, caro_9)

ggplot(caro_rbind_9, aes(E,N, colour = TierName)) +
  geom_point()+
  geom_path()+
  theme(aspect.ratio=1)+
  labs(title = "comparing original- with 9 minutes-resampled data")
  
  
#compare speed
caro_rbind_all <- rbind(caro, caro_3, caro_6, caro_9)

ggplot(caro_rbind_all, aes(DatetimeUTC, speed, colour=TierName))+
  geom_point()+
  geom_path()+
  labs(title = "comparing derived speed at different sampling intervals")






