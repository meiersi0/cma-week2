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


summarise(wildschwein_BE, mean = mean(timelag, na.rm = T))

wildschwein %>%                           # Take wildschwein...
  group_by(TierID) %>%                    # ...group it by TierID
  summarise(                              # Summarise the data...
    mean_timelag = mean(timelag,na.rm = T)# ...by calculating the mean timelag
  )


wildschwein_BE$timelag  <- as.numeric(difftime(lead(wildschwein$DatetimeUTC), wildschwein$DatetimeUTC))
