
rm(list=ls())
library(tidyverse)
library(stringr)

# data from https://fred.stlouisfed.org/series/MHINY36061A052NCEN

# MHI (impute a few missing values and manually add 2015)
MHI_ny_county <- read_csv("data/NY County MHI through 2014.csv")

MHI_ny_county<- 
  MHI_ny_county %>% 
  rename("MHI" = MHINY36061A052NCEN) %>% 
  mutate(DATE = as.Date(DATE, format = "%d/%m/%y")
         ,Year = lubridate::year(DATE)
         ,MHI = as.numeric(str_replace(MHI,"[.]",""))
  ) %>% 
  select(Year,MHI)

# from ACS:
MHI_ny_county_2015 <- data.frame("Year" = c(2015,2016), "MHI" = c(75575,NA))
MHI_ny_county <- bind_rows(MHI_ny_county, MHI_ny_county_2015)


# interpolate the missing values using a loess curve
empty_rows <- MHI_ny_county %>% filter(is.na(MHI))
complete_data <- MHI_ny_county %>% filter(!is.na(MHI))
complete_data$Type <- "compelte"
empty_rows$Type <- "Interp"


# change the 'span' parameter to increase the flexibility of the curve
f_loess <- loess(formula = MHI ~ Year
                 , span = 1
                 , data = complete_data
                 , control = loess.control(surface = "direct")
)

empty_rows$MHI <- round(predict(f_loess, empty_rows),0)


# recombine the data and view the results:
full_data <- bind_rows(complete_data,empty_rows)

full_data %>% 
  ggplot()+
  aes(x = Year, y = MHI, group = Type, color = Type, fill = Type)+
  geom_col()


