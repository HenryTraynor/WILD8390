
behavior_data <- read.csv("Combined_Plot_In_Out_Occurrences.csv")
str(behavior_data)

behavior_data$Plot <- substr(behavior_data$Source.File, 1, 4)
behavior_data$Plot <- factor(behavior_data$Plot, levels=c(unique(behavior_data$Plot), "YPC2"))
behavior_data$Treatment <- substr(behavior_data$Source.File, 3,3)
behavior_data$HabitatType <- substr(behavior_data$Source.File, 1,2)
behavior_data$Month <- substr(behavior_data$Date, 1,1)


table(behavior_data$Plot)

behavior_data$Date <- factor(behavior_data$Video_Date)

table(behavior_data$Date)

write.csv(behavior_data, "behavior_data.csv", row.names = FALSE)

behavior_data_in <- subset(behavior_data, In_Plot_Binary>0)

str(behavior_data_in)

occ_data <- table(behavior_data_in$Plot, behavior_data_in$Date)

occ_data[1:5, ]

occ_data
#_________________________________________

