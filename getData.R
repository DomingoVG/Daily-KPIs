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

df <- "https://raw.githubusercontent.com/DomingoVG/Daily-KPIs/master/db_dashboard.csv" 
df_global <- read.csv(file = df, stringsAsFactors = FALSE)
df_global$Date <- as.Date(as.character(df_global$Date), format = "%m/%d/%Y")
write.csv(df_global, "df_global.csv")
df_global <- read.csv("df_global.csv")