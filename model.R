#Load functions and dependancies
source("functions.R")

#Load data
path <- "input/df.csv"
data <- read.csv(path)
df_w <- get_df_dataset(data)

df_w <- change_freuency(df_w) #Actual 2 horas

#Room specific df
df_A <- get_room_DF(df_w, "A")   
df_F <- get_room_DF(df_w, "F")

#Select cols to use in the model
col_interestA <- c("timestamp","RoomA.Sensor__room_temperature","Outdoor.Temperature_air",
                   "Outdoor.Wind__velocity","Outdoor.Solar__direct_radiation__east_façade",
                   "RoomA.Radiator__control_signal__motor_valve","RoomA.Damper__position",
                   "Ventilation.Fan__air_flow__supply","Ventilation.Sensor__air_temperature__supply")

col_interestF <- c("timestamp","RoomF.Sensor__room_temperature","Outdoor.Temperature_air",
                   "Outdoor.Wind__velocity","Outdoor.Solar__direct_radiation__east_façade",
                   "RoomF.Radiator__control_signal__motor_valve","RoomF.Damper__position",
                   "Ventilation.Fan__air_flow__supply","Ventilation.Sensor__air_temperature__supply")

#Change col names to names of model variables
col_remamesA <- c("timestamp","yTi","Te","Wv","Rs","valve","Dpos","AF","TAHU","t")
col_remamesF <- c("timestamp","yTi","Te","Wv","Rs","valve","Dpos","AF","TAHU","t")

#Get df ready for model training
df_A <- data_preprocess(df_A,columns = col_interestA, col_names = col_remamesA)
df_F <- data_preprocess(df_F,columns = col_interestF, col_names = col_remamesF)

#Basic interpolation of NA values
df_A <- interpolate_NA(df_A) 
df_F <- interpolate_NA(df_F) 


#Testing model room A