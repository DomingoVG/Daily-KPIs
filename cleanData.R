df_global <- read.csv("db_dashboard.csv")
df_global$Date <- as.Date(as.character(df_global$Date), format = "%m/%d/%Y")
write.csv(df_global, "df_global.csv")
