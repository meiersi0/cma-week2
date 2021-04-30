library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

wildschwein_BE <- read_delim("wildschwein_BE_2056.txt",",")
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)
wildschwein_BE

#Task 1
wildschwein_BE <- group_by(wildschwein_BE,TierID)
wildschwein_BE <- mutate(wildschwein_BE,timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))
wildschwein_BE <- mutate(wildschwein_BE,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

wildschwein_BE

#How many individuals were tracked? -->3
#For how long were the individual tracked? Are there gaps? --> 

wildschwein_BE %>%
  group_by(TierName) %>%
  summarise(         
    total_obs = sum(timelag)
  )
    
#Were all individuals tracked concurrently or sequentially? --> Most of the time Yes (1st Qu. 896 3rd QU. 916 sec) But there are Exceptions

summary(wildschwein_BE)

#What is the temporal sampling interval between the locations? --> Mean 1408 sec

#Task 2


