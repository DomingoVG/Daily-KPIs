## ---------------------------
##
## Script name: getData.R
##
## Purpose of script:  Get data for the Pax-Crossings Dashboard
##
## Author: Domingo Velazquez
##
## Date Created: 2020-05-19
##
## ---------------------------

# Data wrangling for passenger count

df_pc <- "https://raw.githubusercontent.com/DomingoVG/Daily-KPIs/master/db_dashboard.csv" 
df_global <- read.csv(file = df_pc, stringsAsFactors = FALSE)
df_global$Date <- as.Date(as.character(df_global$Date), format = "%m/%d/%Y")
#write.csv(df_global, "df_global.csv")
#df_global <- read.csv("df_global.csv")

# Data wrangling for passenger heatmap

df_hm <- "https://raw.githubusercontent.com/DomingoVG/Daily-KPIs/master/db_heatmap.csv" 
df_heat <- read.csv(file = df_hm, stringsAsFactors = FALSE)
df_heat$Date <- as.Date(as.character(df_heat$Date), format = "%m/%d/%Y")
#write.csv(df_heat, "df_heat.csv")
#df_heat <- read.csv("df_heat.csv")
