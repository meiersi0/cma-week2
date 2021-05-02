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
wildschwein_BE <- mutate(wildschwein_BE,timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))
wildschwein_BE <- mutate(wildschwein_BE,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

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

wildschwein_BE_steplength <- wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(steplength_E = E - lag(E))%>%#Question: Why did we use lead and nod lag?
  mutate(steplength_N = N - lag(N))

wildschwein_BE_steplength_tot <- wildschwein_BE_steplength %>%
  mutate(steplength_tot = (sqrt((steplength_E*steplength_E)+(steplength_N*steplength_N))))

rm(wildschwein_BE_steplength) 


#calculate speed
wildschwein_BE_speed <- wildschwein_BE_steplength_tot %>%
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











