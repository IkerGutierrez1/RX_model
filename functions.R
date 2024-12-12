#Dependencies
libraries <- c("ggplot2", "dplyr", "lubridate","zoo","Metrics")
lapply(libraries, library, character.only = TRUE)

#Auxiliar functions

get_df_dataset <- function(data){
  #Función para obtener un dataframe apartir de un .csv, convierte los valores #N/A en Na para facilitar el 
  #procesado tambien se asegura que timestamp este en POSIXct, tambien añade una columna week_day para mostrar el 
  #día de la semana que es
  
  
  #Filter out rows with NA values in timestamp (this NA values only appear at the end after reading)
  data <- data %>%
    filter(!is.na(timestamp))
  
  #Add 00:00:00 to the timestamps which don´t have hour minute (it only happens miodnight)
  data$timestamp <- ifelse(grepl(" ", data$timestamp), data$timestamp, paste(data$timestamp, "00:00:00"))
  
  df <- data %>%
    mutate(timestamp = ymd_hms(timestamp, tz = "Europe/Madrid"))
  
  #Añade una columna con el dia de la semana Lun = 1, Mar = 2...
  df$week_day <- wday(df$timestamp, week_start = 1)
  
  # Convertir todas las columnas excepto 'timestamp' a caracteres
  df <- df %>%
    mutate(across(-c(timestamp), as.character))
  
  # Reemplazar "#N/A" con NA
  df <- df %>%
    mutate(across(-c(timestamp), ~ na_if(., "#N/A")))
  
  # Convertir las columnas de nuevo a numéricas
  df <- df %>%
    mutate(across(-c(timestamp), ~ as.numeric(.)))
  
  # Filtra las filas donde timestamp no es NA
  df <- df[!is.na(df$X), ]
  
  
  return (df)
  
}

change_freuency <- function(df){
  #Change data frecuency to1 hour
  df_w <- df
  
  df_w <- df_w %>%
    mutate(intervalo = floor_date(timestamp, unit = "1 hours")) %>%
    group_by(intervalo) %>%
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
  
  df_w <- df_w %>%
    mutate(intervalo_end = intervalo + hours(1), .after = "intervalo")
  
  df_w <- df_w %>%
    select(-timestamp) %>%  # Eliminar la columna timestamp
    rename(timestamp = intervalo_end)  # Renombrar intervalo_end a timestamp
}

ignore_columns <- function(df, ignore_col, ignore_col_containig) {
  #Función para eliminar del dataframe las columnas que no nos interesen,
  #recibe 2 arrays, ignore_col con el nombre exacto de la columna a elimnar
  #e ignore_col_containing que elimina columnas que contenga esos strings
  # Eliminar columnas con nombres exactos
  df_filtrado <- df %>% select(-one_of(ignore_col))
  
  # Loop para eliminar columnas que contienen cada substring
  for (col in ignore_col_containig) {
    df_filtrado <- df_filtrado %>% select(-contains(col))
  }
  
  return(df_filtrado)
}

get_columns_with_string <- function(dataframe, search_string,ignore_case = TRUE) {
  #FUnción para obtener los nombres de las columnas que contenga el substring deseado
  #por defecto ignora Mayusculas/minusculas pero cambiando ignore_case se puede cambiar
  # Verificar si el dataframe es válido
  if (!is.data.frame(dataframe)) {
    stop("El primer argumento debe ser un dataframe.")
  }
  
  # Obtener los nombres de las columnas que contienen el substring
  matching_columns <- names(dataframe)[grepl(search_string, names(dataframe), ignore.case = ignore_case)]
  
  return(matching_columns)
}

filter_rooms <- function(letter, rooms = c("RoomA","RoomB","RoomC","RoomD","RoomE","RoomF")) {
  # Funcion para obtener la lista habitaciones que no tenga esa letra
  #Se usa para obtener la lista de habitaciones a ignorar para la funcion get_room_DF
  filtered_rooms <- rooms[!grepl(letter, rooms)]
  return(filtered_rooms)
}

get_room_DF <- function(df, room_letter){
  #Funcion para obtener un dataframe con datos globales y de la habitacion
  #en cuestion
  rooms_to_ignore <- filter_rooms(letter = room_letter)
  return (ignore_columns(df,ignore_col = c(), ignore_col_containig = rooms_to_ignore))
}

data_preprocess <- function(df, columns, col_names, start_date = NULL, end_date = NULL){
  #Funcion para obtener dataframe listo para el modelo (menos por missing poinst)
  #Añade ademas la columna t para poder usarla en los modelos ctsmr
  
  # Si no se pasan fechas, seleccionar la primera y última
  if (is.null(start_date)) {
    start_date <- min(df$timestamp, na.rm = TRUE)
  }
  if (is.null(end_date)) {
    end_date <- max(df$timestamp, na.rm = TRUE)
  }
  
  # Filtrar el dataframe por el rango de fechas
  df <- df %>%
    filter(timestamp >= start_date & timestamp <= end_date)
  
  #Coger columnas seleccionadas y añdir t
  
  df <- df[columns]
  
  
  df$t <- as.numeric(df$timestamp)-as.numeric(df$timestamp[1])
  
  colnames(df) <- col_names
  
  return (df)
  
}

interpolate_NA <- function(df){
  df <- df %>%
    mutate(across(-timestamp, ~ as.numeric(as.character(.))))
  
  #Función para hacer una interpolación de los datos que tenga NA, asumen que timestamp
  #esta correctamente preprocesado
  col_NA <- colSums(is.na(df))
  print(col_NA)
  #Hay 13 valores de yTi que son NA y 195 de Te
  for(col in names(col_NA)){
    if(col_NA[col] > 0){
      print(paste("Columna:", col, "- Interpolando", col_NA[col], "valores NA"))
      df <- df %>%
        mutate(!!col := na.approx(.[[col]], na.rm = FALSE))
    }
  }
  
  # Eliminar filas donde todas las columnas (excepto 'timestamp' y 't') son NA, 
  #la interpolacion no funciona si no hay valoresm validos antes de NA, por lo que
  #los NA de la primera hora del dataset se eliminan aqui
  
  df <- df %>%
    filter(!rowSums(is.na(select(., -c(timestamp, t)))) == (ncol(df) - 2))
  
  return (df)
}

box_plot_for_continuous <-function(df, col, n_intervals = 15){
  #Funcion para obtner un box plots de un conjunto de variables continuas, en princio solo se llama  en plot_prediction_results
  #donde se usa para complementar los sactter plots de los errores para diferentes variables
  
  breaks <- seq(min(df[[col]]),max(df[[col]]),  length.out = n_intervals+1)
  df <- df %>%
    mutate(interval = cut(!!sym(col), breaks = breaks, include.lowest = TRUE))
  
  # Crear el boxplot
  p <- ggplot(df, aes(x = interval, y = err)) +
    geom_jitter(color = "blue", width = 0.2, size = 2) +
    geom_boxplot() +
    # Añadir puntos con jitter
    labs(title = paste("err Boxplot for ", col),
         x = paste("Interval", col),
         y = "err") +
    theme_minimal()
  
  return (p)
}

#End Auxiliar functinos

model_predictions <- function(df, fit){
  #Funcion para hacer las predicciones con el fit obtenido y añade los parametros
  #de la prediccion al dataframe
  pTi <- predict(fit)[[1]]
  df$err <- df$yTi -pTi$output$pred$yTi
  df$yTiHat <- pTi$output$pred$yTi
  
  df <- df %>%
    mutate(
      week_day = wday(timestamp, week_start = 1),  # Lunes = 1, ... Domingo = 7
      day_hour = hour(timestamp)  # Hora del día (0-23)
    )
  
  return (df)
}

calculate_rstos <- function(df, fit, model_name, save_dir = "output/predictions/", file_name = "ecuaciones.txt"){
  #Funcion para calcular los restos de la predicciones, los resultado de las
  #predicciones tienen que estar en df
  save_dir = paste0(save_dir,model_name)
  
  # Asegúrate de que el directorio existe
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  file_path <- file.path(save_dir, file_name)
  
  loglik <- fit$loglik
  
  cdetNum <- sum((df$err)^2)
  cdetDen <- sum((df$yTi-mean(df$yTi))^2)
  r2 <- 1 - (cdetNum/cdetDen)
  
  mae <- mean(abs(df$err))
  
  rmse <- sqrt(mean((df$err)^2))
  
  rstos <- data.frame(loglik, r2, mae, rmse)
  names(rstos) <- c("Logarithm Likelihood", "R2", "MAE", "RMSE")
  
  print(rstos)
  
  # Agregar una línea en blanco
  file_conn <- file(file_path, open = "a")  
  writeLines("\n", file_conn)
  
  # Agregar valores de parámetros
  writeLines("\n--- Residuals ---\n", file_conn)
  close(file_conn)  # Cerrar archivo
  
  write.table(
    rstos,
    file = file_path,
    sep = "\t",          
    col.names = TRUE,    
    row.names = FALSE,  
    append = TRUE,       
    quote = FALSE        
  )
  
  return (rstos)
}

plot_prediction_results <- function(df, model_name, save_dir = "output/predictions/", cols_to_plot = c("Te"),
                                    start_date = NULL, end_date = NULL){
  #Funcion para crear las graficas de los resultados de las prediciones
  #cols_to_plot son las columnas de las que se quieren hacer los scatterplots
  save_dir = paste0(save_dir,model_name)
  
  # Asegúrate de que el directorio existe
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Si no se pasan fechas, seleccionar la primera y última
  if (is.null(start_date)) {
    start_date <- min(df$timestamp, na.rm = TRUE)
  }
  if (is.null(end_date)) {
    end_date <- max(df$timestamp, na.rm = TRUE)
  }
  
  df_filtrado <- df %>%
    filter(timestamp >= as.POSIXct(start_date) & timestamp < as.POSIXct(end_date))
  
  #Grafica de valor predecido y medido
  p1 <- ggplot(df_filtrado, aes(x = timestamp)) +
    geom_point(aes(y = yTi, color = "yTi"), size = 1) +    # Línea para yTi
    geom_line(aes(y = yTiHat, color = "yTiHat"), linewidth = 1) +  # Línea para yTiHat
    labs(title = "Comparación de yTi y yTiHat",
         x = "Tiempo",
         y = "Valores") +
    scale_color_manual(name = "Leyenda", values = c("yTi" = "blue", "yTiHat" = "green")) +
    theme_minimal()+
    theme(
      panel.background = element_rect(fill = "white"),  # Fondo del panel
      plot.background = element_rect(fill = "white")    # Fondo del gráfico
    )
  
  filename <- file.path(save_dir, "Predicha_Medida.png")
  #save_dir = paste0(save_dir,"/","Predicha&Medida",".png")
  ggsave(filename = filename, plot = p1)
  
  # Gráfico para la columna err frente a timestamp en df_filtrado
  p2 <- ggplot(df_filtrado, aes(x = timestamp, y = err)) +
    geom_point() +  # Línea para err
    labs(title = "Gráfico de err frente al tiempo semanal",
         x = "Tiempo",
         y = "Error") +
    theme_minimal()+
    theme(
      panel.background = element_rect(fill = "white"),  # Fondo del panel
      plot.background = element_rect(fill = "white")    # Fondo del gráfico
    )
  
  filename <- file.path(save_dir, "Error_week.png")
  #save_dir = paste0(save_dir,"/","Error",".png")
  ggsave(filename = filename, plot = p2)
  
  # Gráfico para la columna err frente a timestamp en df_filtrado
  p3 <- ggplot(df, aes(x = timestamp, y = err)) +
    geom_point() +  # Línea para err
    labs(title = "Gráfico de err frente al tiempo",
         x = "Tiempo",
         y = "Error") +
    theme_minimal()+
    theme(
      panel.background = element_rect(fill = "white"),  # Fondo del panel
      plot.background = element_rect(fill = "white")    # Fondo del gráfico
    )
  
  filename <- file.path(save_dir, "Error.png")
  #save_dir = paste0(save_dir,"/","Error",".png")
  ggsave(filename = filename, plot = p3)
  
  
  # ACF y periodograma acumulado
  filename = paste0(save_dir,"/","ACF",".png")
  png(filename = filename, width = 800, height = 600)
  acf(df$err,150)
  dev.off()
  filename = paste0(save_dir,"/","PeriodogramaAcumulado",".png")
  png(filename = filename, width = 800, height = 600)
  cpgram(df$err)
  dev.off()
  
  
  filename <- file.path(save_dir, "Plots.pdf")
  pdf(filename, width = 10, height = 8)  # Ajusta el tamaño según necesites
  
  print(p1)
  print(p2)
  print(p3)
  
  # Supongamos que tu dataframe se llama df
  p_week <- ggplot(df, aes(x = factor(week_day), y = err)) + 
    geom_jitter(color = "darkred", size = 1, alpha = 0.5) +  # Añadir todos los puntos
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    labs(title = "Boxplot de err por Día de la Semana",
         x = "Día de la Semana",
         y = "Error") +
    scale_x_discrete(labels = c("1" = "Lun", "2" = "Mar", "3" = "Mié", "4" = "Jue", "5" = "Vie", "6" = "Sáb", "7" = "Dom")) +
    theme_minimal()
  
  print(p_week)
  
  p_day <- ggplot(df, aes(x = factor(day_hour), y = err)) + 
    geom_jitter(color = "darkred", size = 1, alpha = 0.5) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    labs(title = "Boxplot de err por Hora del Día",
         x = "Hora del Día",
         y = "Error") +
    scale_x_discrete(breaks = 0:23) +  # Para mostrar todas las horas
    theme_minimal()
  
  print(p_day)
  
  for (col in cols_to_plot){
    # Crear el scatterplot
    p <- ggplot(df, aes(x = !!sym(col), y = err)) +
      geom_point(color = "blue", size = 2, alpha = 0.5) +  # Puntos del scatterplot
      labs(title = paste("Scatterplot de Err vs ", col),
           x = col,
           y = "Error") +
      theme_minimal()
    
    p_int <- box_plot_for_continuous(df,col, n_intervals = 15)
    print(p)
    print(p_int)
  }
  
  acf(df$err,150)
  cpgram(df$err)
  # Cierra el dispositivo PDF
  dev.off()
  
  
}

save_initial_parameters <- function(mod, model_name, save_dir = "output/predictions/"){
  #Funcion para crear las graficas de los resultados de las prediciones
  save_dir = paste0(save_dir,model_name)
  
  # Asegúrate de que el directorio existe
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  file_name = file.path(save_dir, "parametros_iniciales.txt")
  write.table(mod$ParameterValues, file = file_name, sep = "\t", quote = FALSE, row.names = TRUE, col.names = TRUE)
  
}

save_equations <- function(mod, model_name, save_dir = "output/predictions/"){
  
  states <- mod$states
  inputs <- mod$inputs
  equ <- mod$sys.eqs
  
  save_dir = paste0(save_dir,model_name)
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Construir la ruta del archivo
  save_path <- file.path(save_dir,"ecuaciones.txt")
  
  
  for (state in mod$states) {
    equation <- mod$sys.eqs[[state]]$input
    inputs <- c(inputs,equation)
  }
  
  inputs <- as.character(inputs)
  
  file_conn <- file(save_path, open = "w")  
  writeLines("\n--- Inputs & Equations ---\n", file_conn)
  writeLines(inputs, file_conn)
  close(file_conn)  # Cerrar archivo
  
  # Agregar una línea en blanco
  file_conn <- file(save_path, open = "a")  
  writeLines("\n", file_conn)
  
  # Agregar valores de parámetros
  writeLines("\n--- Parameter values ---\n", file_conn)
  close(file_conn)  # Cerrar archivo
  
  # Escribir parámetros al archivo (append)
  write.table(mod$ParameterValues, file = save_path, sep = "\t", quote = FALSE, 
              row.names = TRUE, col.names = TRUE, append = TRUE)
  
  
  print(inputs)
  
}

model_estimation_result <- function(df, model, model_name, save_dir = "output/predictions/", cols_to_plot = c("Te"), start_date = NULL, end_date = NULL){
  #Funcion para realizar la prediciones con el modelo en los datos df
  #Guarda tanto el resumen como los rstos y graficas mostrando los resultados del modelo
  #los argumentos que son fechas se usan unicamente para elegir el periodo que se 
  #quiere ven en los plots
  
  # Si no se pasan fechas, seleccionar la primera y última
  if (is.null(start_date)) {
    start_date <- min(df$timestamp, na.rm = TRUE)
  }
  if (is.null(end_date)) {
    end_date <- max(df$timestamp, na.rm = TRUE)
  }
  
  
  save_initial_parameters(mod = model, model_name = model_name)
  save_equations(mod = model, model_name = model_name)
  #Devuelve el dataframe con las predicciones 
  
  
  fit <- model$estimate(data = df,)
  
  
  # Imprimir el resumen del modelo
  model_summary <- summary(fit, extended = TRUE)
  print(model_summary)
  
  save_dir = paste0(save_dir,model_name)
  # Crear el directorio si no existe
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Guardar el resumen en un archivo de texto
  capture.output({
    cat("Modelo Resumen:\n\n")
    print(model_summary)
  }, file = file.path(save_dir, "ecuaciones.txt"), append = TRUE)
  
  #Guarda predicciones y resultados
  print("Predicting...")
  df <- model_predictions(df, fit = fit)
  print("Calculating rstos...")
  rstos <- calculate_rstos(df, fit, model_name = model_name)
  print("Ploting....")
  plot_prediction_results(df,model_name,,cols_to_plot = cols_to_plot,start_date = start_date, 
                          end_date = end_date)
  
  return (df)                      
  
}

hourly_error_correlation <- function(df, target_hour){
  #Create a column with error from 2 hours prior
  df_err <- df %>%
    arrange(timestamp) %>%  
    mutate(err_t_2hours = lag(err, n = 2))  
  
  #Create column with difference in columns
  df_err <- df_err %>%
    mutate(delta_err = err - err_t_2hours)  
  
  df_err <- df_err[df_err$day_hour == target_hour, ]
  
  scatter_plot <- ggplot(df_err, aes(x = err, y = delta_err, color = as.factor(week_day))) +
    geom_point(size = 3) +
    labs(
      title = "Scatter Plot: err vs delta_err",
      x = "Error (err)",
      y = "Delta Error (delta_err)",
      color = "Día de la semana"  # Leyenda para los colores
    ) +
    coord_fixed(ratio = 1) +
    scale_color_manual(
      values = c(
        "1" = "red", "2" = "orange", "3" = "yellow",
        "4" = "green", "5" = "blue", "6" = "purple", "7" = "pink"
      ),
      labels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
    ) +
    theme_bw()
  
  # Guardar el gráfico en 800x800 píxeles
  ggsave("scatter_plot.png", plot = scatter_plot, width = 8, height = 8, dpi = 100)
  
  return (df_err)
}


create_RX_df <- function(df, prev_obs) {
  df_nuevo <- df
  col_names <- names(df)
  remove <- c("timestamp","t")
  col_names <- col_names[! col_names %in% remove]
  
  
  for (i in 1:prev_obs) {
    # Identificar las columnas originales (que no terminan en _i-1)
    columnas_originales <- names(df_nuevo)[!grepl(paste0("_", i - 1, "$"), names(df_nuevo))]
    
    # Crear nuevas columnas basadas en las originales
    df_nuevo <- df_nuevo %>%
      mutate(across(all_of(col_names), ~lag(., i), .names = "{.col}_{i}"))
  }
  
  return(df_nuevo)
}
