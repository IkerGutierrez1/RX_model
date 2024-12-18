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
previous_hours <- 6
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
mean_res<-mean(arx$residuals) #No cuadrado??
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
  
  #cat("Parameter evaluation:", par, "\n")
  
  beta_Te <- par[1]
  beta_Te_1 <- par[2]
  beta_yTi_2 <- par[3]
  beta_Te_2 <- par[4]
  beta_yTi_3 <- par[5]
  beta_Te_3 <- par[6]

  sigma <- par[7]
  intercept <- par[8]
  #Residual (Change function betwwen () for other models and change predict)
  R <- X$yTi -(intercept+beta_Te*X$Te  + beta_Te_1*X$Te_1  + beta_yTi_2*X$yTi_2 + beta_Te_2*X$Te_2 +
                beta_yTi_3*X$yTi_3 + beta_Te_3*X$Te_3)
  #Minimize the negative to maxime log
  - sum(dnorm(R, mean = 0, sigma, log = TRUE))
}

df_opt <- na.omit(df_RX)
coeff_names <- c("Te","Rs","AHU","yTi_1","Te_1","Rs_1","AHU_1","yTi_2","Te_2","Rs_2","AHU_2","yTi_3","Te_3","Rs_3","AHU_3","sigma","icep")
coeff_names <- c("Te","Te_1","yTi_2","Te_2","yTi_3","Te_3","sigma","icep")

#Bounds
lower = c(-0.5, -0.5,-0.5, -0.5, -0.5, -0.5, -0.5, 0.1,-50)
upper = c(0.5, 0.5,0.5, 0.5, 0.5, 0.5, 0.5, Inf,50)
inital_cond <- c(0.25,0.25,0.25,0.25,0.25,0.25,0.25,sd(df_opt$yTi),0)

#Use optim to minimize
mle_par <- optim(fn = ll_lm,
                 par = inital_cond,
                 X = df_opt,
                 method = "L-BFGS-B",
                 lower = lower, #Negative numbers do not always work
                 upper = upper, #Make then more restictive for future models
                 control = list(maxit = 20000))

coeff <- mle_par$par
print(paste("Finished optim convergence: ",mle_par$convergence))

head <- "name\tvalue \tinitial\tlower\tupper\n"
coeff_string <- paste(
  paste(coeff_names, coeff, inital_cond, lower, upper, sep = "\t"),
  collapse = "\n"                        
)
coeff_string <- paste(head, coeff_string)

#Predict
df_opt$Ti_pred <- coeff[1]*df_opt$Te + coeff[2]*df_opt$Te_1 + coeff[3]*df_opt$yTi_2 + coeff[4]*df_opt$Te_2+ 
  coeff[5]*df_opt$yTi_3 + coeff[6]*df_opt$Te_3 +coeff[8]

formula <- "X$yTi -(intercept + beta_Te*X$Te  + beta_Te_1*X$Te_1  + beta_yTi_2*X$yTi_2 + beta_Te_2*X$Te_2 +
                beta_yTi_3*X$yTi_3 + beta_Te_3*X$Te_3)" #Only use to identify the results
#Rsiduals
df_opt$res <- df_opt$yTi - df_opt$Ti_pred

#Residual analysis
MSE <- mean((df_opt$res)^2)
std_res <- sd(df_opt$res)
R_sqr <- 1 - ((sum(df_opt$res^2))/(sum((df_opt$yTi-mean(df_opt$yTi))^2)))


#Save results
save_dir <- "output/predictions/"
model_name <- "Lag3_Te_yt_2/"

file_path <- paste0(save_dir, model_name) 
if (!dir.exists(file_path)) {
  dir.create(file_path, recursive = TRUE)
}

#Save model summary
cat(paste0("Formula: ", formula,
           "\n\nMSE: \t\t", MSE,
           "\nSTD res: \t", std_res,
           "\nR^2: \t\t", R_sqr,
           "\n\n",coeff_string), file = paste0(file_path,"model_summary.txt"))

#Plot prediction
start_dates <- c("2023-05-08","2023-08-07")
end_dates <- c("2023-05-15","2023-08-14")

for (i in 1:length(start_dates)){
  df_plot <- df_opt[df_opt$timestamp >= start_dates[i] & df_opt$timestamp <= end_dates[i], ]

  p <- ggplot(df_plot, aes(x = timestamp)) + 
    geom_line(aes(y = yTi, color = "yTi")) + 
    geom_line(aes(y = Ti_pred, color = "Ti_pred"), linetype = "dashed") + 
    geom_point(aes(y = Ti_pred, color = "º")) +
    labs(x = "Timestamp", y = "Temperature (ºC)", title = "yTi and Ti_pred") + 
    scale_color_manual(name = "Source", values = c("yTi" = "blue", "Ti_pred" = "red")) +
    theme_bw() +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  
  ggsave(paste0(file_path,"weekly_plot_",i,".png"), plot = p, width = 6, height = 4, dpi = 300)}





