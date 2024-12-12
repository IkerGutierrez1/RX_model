#Load functions and dependancies
source("functions.R")

#Load data
path <- "input/df.csv"
data <- read.csv(path)
df_w <- get_df_dataset(data)

df_w <- change_freuency(df_w) 

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
input_var <- c("Te","Rs") 
output_var <- "yTi"

#Basic parameters and df preparation
sampling_freq <- 60
previous_hours <- 0
prev_observations <- previous_hours*60/sampling_freq
df_RX <- create_RX_df(df_A, prev_observations) 


#Creation of formla
for (j in 0:prev_observations){
  formula<-paste(output_var, " ~ 0 +", sep="")
  
  for (i in 1:length(input_var)){
    formula<-paste(formula, " ", input_var[i], sep="") 
    if (i<length(input_var)){
      formula<-paste(formula, " +", sep="")
    }
  }
  
  if (j>0){
    for (i in 1:j) {
      var_name<-paste(output_var, "_", i, sep="")
      formula<-paste(formula, " + ", var_name, sep="") 
      
      var_name<-paste(input_var, "_", i, sep="")
      
      for (k in 1:length(var_name)) {
        formula<-paste(formula, " + ",var_name[k], sep="")
      }
    }
  }
}

#Model
arx<-lm(formula, data = df_RX)
a<-summary(arx)

#Ignore Columns with na
x<-j+1

#Results vectors
Ti_pred <- predict(arx)
yTi <- df_RX$yTi[x:nrow(df_RX)]

#Parameter calc
max_p_value <- 0
for (i in 1:length(arx$coefficients)){
  if (a[["coefficients"]][i,4] > max_p_value){
    max_p_value<-a[["coefficients"]][i,4]
  }
}
mean_res<-mean(arx$residuals) #No valor absoluto?
std_res<-sd(arx$residuals)




row_names <- c("(Intercept)",names(arx$coefficients), "R2", "MAE",
                 "Mean residuals", "STD residuals",
                 "Variable number","Parameters max p value")

#df_results <- data.frame(matrix(nrow=length(row_names)))
df_results <- data.frame(matrix(nrow=length(names(arx$coefficients))+7))
rownames(df_results) <- row_names
colnames(df_results) <- "Iteration 1"






if (j==0){
  for (i in 1:nrow(df_results)){
    for (k in 1:length(names(arx$coefficients))){
      if (rownames(df_results[i,0])==names(arx$coefficients)[k]){
        df_results[i,1]<-arx$coefficients[k]
      }
    }
    if (rownames(df_results)[i] == "Variable number"){
      df_results[i,1]<-length(arx$coefficients)
    }
    if (rownames(df_results)[i] == "(Intercept)"){
      df_results[i,1]<-0
    }
    if (rownames(df_results)[i] == "R2"){
      df_results[i,1]<-summary(arx)$r.squared
    }
    if (rownames(df_results)[i] == "MAE"){
      df_results[i,1]<-mae(yTi, Ti_pred)
    }
    if (rownames(df_results)[i] == "Parameters max p value"){
      df_results[i,1]<-max_p_value
    }
    if (rownames(df_results)[i] == "Mean residuals"){
      df_results[i,1]<-mean_res
    }
    if (rownames(df_results)[i] == "STD residuals"){
      df_results[i,1]<-std_res
    }
  }
} else {
  col_name<-paste("Iteration ", j+1, sep="")
  df_results$mas<-rep(NA,nrow(df_results))
  colnames(df_results)[j+1]<-col_name
  for (i in 1:nrow(df_results)){
    for (k in 1:length(names(arx$coefficients))){
      if (rownames(df_results[i,0])==names(arx$coefficients)[k]){
        df_results[i,j+1]<-arx$coefficients[k]
      }
    }
    if (rownames(df_results)[i] == "Variable number"){
      df_results[i,j+1]<-length(arx$coefficients)
    }
    if (rownames(df_results)[i] == "(Intercept)"){
      df_results[i,j+1]<-0
    }
    if (rownames(df_results)[i] == "R2"){
      df_results[i,j+1]<-summary(arx)$r.squared
    }
    if (rownames(df_results)[i] == "MAE"){
      df_results[i,j+1]<-mae(yTi, Ti_pred)
    }
    if (rownames(df_results)[i] == "Parameters max p value"){
      df_results[i,j+1]<-max_p_value
    }
    if (rownames(df_results)[i] == "Mean residuals"){
      df_results[i,j+1]<-mean_res
    }
    if (rownames(df_results)[i] == "STD residuals"){
      df_results[i,j+1]<-std_res
    }
  }
}


#Plots
df_plot <- df_A[-(1:prev_observations),]
df_plot <- df_plot %>%
  mutate(Ti_pred = Ti_pred)  

# Graficar las dos series frente a timestamp
p <- ggplot(df_plot, aes(x = timestamp)) +
  geom_line(aes(y = yTi, color = "yTi"), linewidth = 1) +  
  geom_point(aes(y = Ti_pred, color = "Ti_pred"), size = 1) +  
  labs(
    x = "Timestamp",
    y = "Temperature",
    color = "Serie"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("yTi" = "blue", "Ti_pred" = "red"))  

p

residuals <- arx$residuals

df_plot_residuals <- df_A[-(1:prev_observations),] %>%
  mutate(residuals = residuals)  


p_res <- ggplot(df_plot_residuals, aes(x = timestamp, y = residuals)) +
  geom_point(color = "blue", size = 1) + 
  labs(
    title = "Gráfico de Residuals frente a Timestamp",
    x = "Timestamp",
    y = "Residuals"
  ) +
  theme_minimal()

p_res



#---------------------------------------------------------

#Optimize problem intead of lm
#Function for optimization
ll_lm <- function(par, X){

  beta_Te <- par[1]
  beta_Rs <- par[2]
  sigma <- par[3]
  intercept <- par[4]
  #Residual (Change function betwwen () for other models and change predict)
  R <- X$yTi - (intercept + beta_Te*X$Te + beta_Rs*X$Rs) 
  #Minimize the negative to maxime log
  - sum(dnorm(R, mean = 0, sigma, log = TRUE))
}

df_opt <- na.omit(df_RX)
#Use optim to minimize
mle_par <- optim(fn = ll_lm,
                 par = c(1,0,sd(df_opt$yTi),10),
                 X = df_opt)

coeff <- mle_par$par

#Predict
df_opt$Ti_pred <- coeff[1]*df_opt$Te + coeff[2]*df_opt$Rs + coeff[4]
#Rsiduals
df_opt$res <- df_opt$yTi - df_opt$Ti_pred

#Residual analysis
mean_res <- mean(df_opt$res)
std_res <- sd(df_opt$res)
R_sqr <- 1 - ((sum(df_opt$res^2))/(sum((df_opt$yTi-mean(df_opt$yTi))^2)))

#Plot prediction
start_date <- as.Date("2023-05-08")
end_date <- as.Date("2023-05-15")

start_date <- as.Date("2023-08-07")
end_date <- as.Date("2023-08-15")


df_plot <- df_opt[df_opt$timestamp >= start_date & df_opt$timestamp <= end_date, ]

p <- ggplot(df_plot, aes(x = timestamp)) + 
  geom_line(aes(y = yTi, color = "yTi")) + 
  geom_line(aes(y = Ti_pred, color = "Ti_pred"), linetype = "dashed") + 
  geom_point(aes(y = Ti_pred, color = "Ti_pred")) +
  labs(x = "Timestamp", y = "Temperature (ºC)", title = "yTi and Ti_pred") + 
  scale_color_manual(name = "Source", values = c("yTi" = "blue", "Ti_pred" = "red")) +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


