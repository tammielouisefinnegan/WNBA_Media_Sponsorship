## Set Working Directory ##
setwd("C:/Users/tlfin/OneDrive/Documents/Erdos Institute/Summer 2025/Data Science Bootcamp/Final Project")


## Load Packages ##
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)
library(texreg)


## Load Data (Use readxl) ##
Broadcast_Data_Raw <- read_excel("WNBA_Broadcast_Data.xlsx", col_names = FALSE)
Sponsorship_Data_Raw <- read_excel("WNBA_Sponsorship_Data.xlsx", col_names = FALSE)


## Restructure Column Headings in Both Data Frames (Use janitor) ##
Broadcast_Data <- row_to_names(Broadcast_Data_Raw, row_number = 3)
Sponsorship_Data <- row_to_names(Sponsorship_Data_Raw, row_number = 3)


## Convert Columns to Numeric ##
Broadcast_Data[("Year")] <- lapply(Broadcast_Data[("Year")], as.numeric)
Sponsorship_Data[c("Year", "Broadcast_Media_Worth", "Estimated_Total_Sponsorship_Revenue", "Estimated_Number_of_Sponsors")] <- lapply(Sponsorship_Data[c("Year", "Broadcast_Media_Worth", "Estimated_Total_Sponsorship_Revenue", "Estimated_Number_of_Sponsors")], as.numeric)


## Create Game Count Variable ##
Game_Count <- Broadcast_Data %>%
  filter(Game_Type %in% c("Regular Season", "Post Season")) %>%
  group_by(Year) %>%
  summarise(Game_Count = n())

Broadcast_Data <- left_join(Broadcast_Data, Game_Count, by = "Year")


## Merge Datasets ##
Merged_Data <- merge(Broadcast_Data, Sponsorship_Data, by = "Year")


## Basic Data Visualization ##
plot(Merged_Data$Year, Merged_Data$Count, main = "Estimated Annual Number of WNBA Sponsorships (2019-2024)", xlab = "Year", ylab = "Total")
hist((Merged_Data$Year), main = "Annual Number of WNBA Broadcasts (2019-2024)", xlab = "Year", ylab = "Count", col = "red", border = "black")


## Training/Test Split ##
N <- nrow(Merged_Data)
Train_Size <- floor(0.8 * N)

Train_Data <- Merged_Data[1:Train_Size, ]
Test_Data  <- Merged_Data[(Train_Size + 1):N, ]


## Modelling ##
# Poisson Model for Total Number of Broadcast Games / Estimated Total Sponsorship Revenue #
Model1 <- glm(Estimated_Total_Sponsorship_Revenue ~ Game_Count, data = Merged_Data, family = poisson(link = "log"))
summary(Model1)

Model2 <- glm(Estimated_Total_Sponsorship_Revenue ~ Game_Count + Broadcast_Media_Worth, data = Merged_Data, family = poisson(link = "log"))
summary(Model2)

Model3 <- glm(Estimated_Total_Sponsorship_Revenue ~ Game_Count + Broadcast_Media_Worth + Estimated_Number_of_Sponsors, data = Merged_Data, family = poisson(link = "log"))
summary(Model3)

# Stepwise Regression on Models to Find Best Performing Model #
step(Model1)
step(Model2)
step(Model3)

# Multiplicative Effect #
exp(coef(Model1))
exp(coef(Model2))
exp(coef(Model3))

# Create Regression Table #
texreg(Model2, custom.model.names = "Best Performing Model", custom.coef.map = list("(Intercept)" = "Intercept", "Game_Count" = "Game Count", "Broadcast_Media_Worth" = "Value of Broadcast Media Contracts"), stars = numeric(), bold = 0.05)
