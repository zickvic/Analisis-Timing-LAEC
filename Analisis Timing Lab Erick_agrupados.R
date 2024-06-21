library(YEAB)
library(ggplot2)
library(cowplot)

# Inicializa una lista para almacenar los datos de todos los sujetos
all_subject_data <- list()

# Crear un data frame vacío para almacenar los resultados
gaussian_params_data <- data.frame(Subject = character(),
                                   Session = integer(),
                                   TrialType = character(),
                                   t0 = numeric(),
                                   Spread = numeric(),
                                   stringsAsFactors = FALSE)

# Data frame para almacenar los datos para crear boxplots
boxplot_data <- data.frame(condition = character(), t0 = numeric(), subject = character())

# Crear una lista para almacenar los gráficos individuales
plot_list <- list()

# Crear un nuevo data frame para almacenar todos los datos combinados
combined_list <- list()

# Variable para controlar si se guardan los gráficos
ind_plot_save <- TRUE
gen_plot_save <- FALSE
avg_plot_save <- FALSE
box_plot_save <- FALSE

# Obtener la lista de archivos en el directorio de trabajo
file_list <- list.files(pattern = "*.txt")

# Lee los archivos y los agrega a una lista con un data frame por cada uno
for (file_name in file_list) {
  
  # Leer el archivo y convertirlo en data frame
  data <- read.delim(file_name, header = TRUE, sep = ",", dec = ".")
  
  # Elimina las filas innecesarias así como los datos de identificación
  data <- data[!(data$TipoEnsayo %in% c('Skipped', 'Training', 'Wait')), ][,-c(1,2)]
  
  # Extraer las iniciales del nombre del archivo
  initials <- strsplit(file_name, "_")[[1]][1]
  
  # Agregar una columna "Session" al dataframe para identificar la sesión
  data$Session <- gsub(".txt", "", strsplit(file_name, "_")[[1]][2])
  
  # Agregar los datos al dataframe correspondiente al sujeto en la lista all_subject_data
  if (initials %in% names(all_subject_data)) {
    all_subject_data[[initials]] <- rbind(all_subject_data[[initials]], data)
  } else {
    all_subject_data[[initials]] <- data
  }
}

# Procesa y grafica los datos
for (initials in names(all_subject_data)) {
  
  # Se asigna un nombre de variable más manejable
  subject_data <- all_subject_data[[initials]]
  
  # Resolución de bines
  bin <- .25
  
  # Dividir los datos del sujeto en ensayos individuales
  trials_list <- lapply(split(subject_data, subject_data$TipoEnsayo), function(df) {
    # Ordenar los datos por tiempo
    df <- df[order(df$Tiempo), ]
    # Eliminar la columna 'TipoEnsayo'
    df <- df[, -which(names(df) == "TipoEnsayo")]
    return(df)
  })
  
  # Reescribe los valores de tiempo por su correspondiente bin
  binned_data <- lapply(trials_list, function(df) {
    df$Tiempo <- get_bins(df$Tiempo, 0, 30, bin)
    df$Tiempo <- df$Tiempo - 1
    return(df)
  })
  
  # Iterar sobre cada tipo de ensayo
  for (trial_type in names(binned_data)) {
    # Verificar si el tipo de ensayo ya está en combined_list
    if (trial_type %in% names(combined_list)) {
      # Si ya está presente, agregamos los datos de binned_data al dataframe existente en combined_list
      combined_list[[trial_type]] <- rbind(combined_list[[trial_type]], binned_data[[trial_type]])
    } else {
      # Si no está presente, creamos un nuevo dataframe en combined_list con los datos de binned_data
      combined_list[[trial_type]] <- binned_data[[trial_type]]
    }
  }
  
  # Calcular la tabla de frecuencias
  ftable_trials <- lapply(binned_data, function(df) {
    df_processed <- f_table(df$Tiempo, 0, 30, bin)
    #df_processed$freq <- df_processed$freq/length(df_processed$freq)
    df_processed$freq <- unity_normalization(df_processed$freq)
    return(df_processed)
  })
  
  # Definir los parámetros iniciales para el ajuste gaussiano
  initial_params <- list(a = 1, d = 0, t0 = 10, b = 0.8, c = 0)
  
  # Aplicar el ajuste gaussiano a los datos de frecuencia
  gaus_est_trials <- lapply(ftable_trials, function(df) {
    return(gaussian_fit(df$freq, df$bins, par = initial_params, max.iter = 10000))
  })
  
  # Obtener los valores para esta iteración
  peak_value <- gaus_est_trials$Peak[[3]]
  gap_value <- gaus_est_trials$Gap[[3]]
  db_value <- gaus_est_trials$dBInterruption[[3]]
  
  # Agregar una nueva fila al data frame
  boxplot_data <- rbind(boxplot_data, data.frame(condition = 'Pico', t0 = peak_value, subject = initials))
  boxplot_data <- rbind(boxplot_data, data.frame(condition = 'Interrupción de señal', t0 = gap_value, subject = initials))
  boxplot_data <- rbind(boxplot_data, data.frame(condition = 'Distractor auditivo', t0 = db_value, subject = initials))
  
  # Agregar datos al data frame gaussian_params_data
  for (trial_type in names(gaus_est_trials)) {
    t0_value <- gaus_est_trials[[trial_type]]$t0
    spread_value <- gaus_est_trials[[trial_type]]$b
    gaussian_params_data <- rbind(gaussian_params_data, data.frame(Subject = initials, 
                                                                   Session = session_num, 
                                                                   TrialType = trial_type, 
                                                                   t0 = t0_value, 
                                                                   Spread = spread_value))
  }
  
  # Definir la función para calcular la curva ajustada
  g_plus_lin <- function(par, tiempo) {
    par$a * exp(-0.5 * ((tiempo - par$t0) / par$b)^2) + par$c * (tiempo - par$t0) + par$d
  }
  
  # Generar puntos de tiempo para la curva ajustada
  time_points <- seq(0, 30, 0.1)
  
  # Calcular la curva ajustada para cada tipo de ensayo
  y_fit_trials <- lapply(gaus_est_trials, function(df) {
    return(g_plus_lin(df |> as.list(), time_points))
  })
  
  # Calcula el valor máximo del eje Y para ajustar las dimensiones del plot
  ymax <- max(c(y_fit_trials$dBInterruption, y_fit_trials$Peak, y_fit_trials$Gap))
  
  # Para controlar si se generan los plots individuales
  if (ind_plot_save == TRUE) {
    
    # Generar un nombre de archivo único para cada plot
    plot_filename <- paste0("plot_", initials, ".png")
    
    # Crear un data frame con los datos de y_fit_trials
    plot_data <- data.frame(time_points = time_points,
                            dBInterruption = y_fit_trials$dBInterruption,
                            Gap = y_fit_trials$Gap,
                            Peak = y_fit_trials$Peak)
    
    # Generar el gráfico individual
    p <- ggplot(plot_data) +
      geom_line(aes(x = time_points, y = dBInterruption, color = "dBInterruption")) +
      geom_line(aes(x = time_points, y = Gap, color = "Gap")) +
      geom_line(aes(x = time_points, y = Peak, color = "Peak")) +
      geom_point(data = ftable_trials$dBInterruption, aes(x = bins, y = freq), color = "red", size = 0.8) + # Agregar puntos rojos
      geom_point(data = ftable_trials$Gap, aes(x = bins, y = freq), color = "blue", size = 0.8) + # Agregar puntos azules
      geom_point(data = ftable_trials$Peak, aes(x = bins, y = freq), color = "black", size = 0.8) + # Agregar puntos negros
      labs(title = initials, x = "Time in trial", y = "R(t)") +
      scale_color_manual(values = c("dBInterruption" = "red", "Gap" = "blue", "Peak" = "black")) +
      theme_minimal() +
      geom_vline(xintercept = 10, linetype = 2, color = "black") +
      theme(
        legend.position = "topright",
        plot.title = element_text(hjust = 0.5)
      ) +
      coord_cartesian(ylim = c(0, ymax))
    
    # Guardar el plot en un archivo .png
    ggsave(filename = plot_filename, plot = p, width = 6, height = 4, units = "in", dpi = 200, bg = "white")
  }
  
  # Para controlar si se genera el plot general
  if (gen_plot_save == TRUE) {
    
    # Crear un data frame con los datos de y_fit_trials
    plot_data <- data.frame(time_points = time_points,
                            dBInterruption = y_fit_trials$dBInterruption,
                            Gap = y_fit_trials$Gap,
                            Peak = y_fit_trials$Peak)
    
    # Generar el gráfico individual
    p <- ggplot(plot_data) +
      geom_line(aes(x = time_points, y = dBInterruption, color = "dBInterruption")) +
      geom_line(aes(x = time_points, y = Gap, color = "Gap")) +
      geom_line(aes(x = time_points, y = Peak, color = "Peak")) +
      geom_point(data = ftable_trials$dBInterruption, aes(x = bins, y = freq), color = "red", size = 0.2) + # Agregar puntos rojos
      geom_point(data = ftable_trials$Gap, aes(x = bins, y = freq), color = "blue", size = 0.2) + # Agregar puntos azules
      geom_point(data = ftable_trials$Peak, aes(x = bins, y = freq), color = "black", size = 0.2) + # Agregar puntos negros      labs(title = initials, x = "Time in trial", y = "R(t)") +
      labs(title = initials, x = "Time in trial", y = "R(t)") +
      scale_color_manual(values = c("dBInterruption" = "red", "Gap" = "blue", "Peak" = "black")) +
      theme_minimal() +
      geom_vline(xintercept = 10,
                 linetype = 2,
                 color = "black") +
      theme(
        legend.position ='none',
        plot.title = element_text(hjust = 0.5)
      )
    
    # Añadir el gráfico a la lista
    plot_list[[initials]] <- p
  }
}

# Guarda el plot general
if (gen_plot_save == TRUE) {
  # Crear una cuadrícula de gráficos
  grid_plot <- plot_grid(plotlist = plot_list, ncol = 3)
  
  # Crear un data frame con la información de la leyenda
  legend_data <- data.frame(
    label = c("Peak", "Gap", "dBInterruption"),  # Etiquetas para cada tipo de ensayo
    color = c("red", "blue", "black"),  # Colores correspondientes
    y = 1  # Valor constante para el eje y
  )
  
  # Crear un gráfico solo para la leyenda
  legend_plot <- ggplot(legend_data, aes(x = label)) +
    geom_segment(aes(x = label, xend = label, y = y, yend = y, color = label), linewidth = 1) +  # Líneas de colores
    labs(color = NULL) +  # Eliminamos el título de la leyenda
    scale_color_manual(values = legend_data$color) +  # Especificamos los colores manualmente
    theme_minimal() +  # Cambiamos el tema para que se parezca más a la leyenda original
    theme(
      legend.position = "top",  # Posicionamos la leyenda en la parte superior
      legend.box = "horizontal",  # Colocamos la leyenda en una caja horizontal
      panel.grid = element_blank(),  # Eliminamos la cuadrícula
      axis.text = element_blank(),  # Eliminamos las etiquetas de los ejes
      axis.title = element_blank(),  # Eliminamos los títulos de los ejes
      axis.ticks = element_blank()  # Eliminamos los ticks de los ejes
    )

  # Agregar la leyenda al gráfico general
  grid_plot <- plot_grid(legend_plot, grid_plot, nrow = 2, rel_heights = c(0.1, 1))
  
  # Guardar el plot en un archivo .png
  ggsave("plot_general.png", grid_plot, width = 6, height = 4, units = "in", dpi = 200, bg = "white")
  
  # Imprimir la cuadrícula de gráficos
  print(grid_plot)
  
}

# Genera el plot con datos combinados
if (avg_plot_save == TRUE){
  combined_ftables <- lapply(combined_list, function(df) {
    df_processed <- f_table(df$Tiempo, 0, 30, bin)
    df_processed$freq <- unity_normalization(df_processed$freq)
    return(df_processed)
  })
  
  combined_params <- lapply(combined_ftables, function(df) {
    return(gaussian_fit(df$freq, df$bins, par = initial_params, max.iter = 10000))
  })  
  
  y_fit_combined <- lapply(combined_params, function(df) {
    return(g_plus_lin(df |> as.list(), time_points))
  })
  
  # Crear un data frame con los datos de y_fit_trials
  combined_plot_data <- data.frame(time_points = time_points,
                          dBInterruption = y_fit_combined$dBInterruption,
                          Gap = y_fit_combined$Gap,
                          Peak = y_fit_combined$Peak)
  
  # Generar el gráfico individual
  print(ggplot(combined_plot_data) +
    geom_line(aes(x = time_points, y = dBInterruption, color = "dBInterruption")) +
    geom_line(aes(x = time_points, y = Gap, color = "Gap")) +
    geom_line(aes(x = time_points, y = Peak, color = "Peak")) +
    #geom_point(data = combined_ftables$dBInterruption, aes(x = bins, y = freq), color = "red", size = 0.2) + # Agregar puntos rojos
    #geom_point(data = combined_ftables$Gap, aes(x = bins, y = freq), color = "blue", size = 0.2) + # Agregar puntos azules
    #geom_point(data = combined_ftables$Peak, aes(x = bins, y = freq), color = "black", size = 0.2) + # Agregar puntos negros      labs(title = initials, x = "Time in trial", y = "R(t)") +
    labs(title = 'Overall Average', x = "Time in trial", y = "R(t)") +
    scale_color_manual(values = c("dBInterruption" = "red", "Gap" = "blue", "Peak" = "black")) +
    theme_minimal() +
    geom_vline(xintercept = 10,
               linetype = 2,
               color = "black") +
    theme(
      legend.position=c(.8,.75),
      legend.title = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA),
      legend.background = element_rect(color = "black", size = .1),  # Agregar un margen alrededor del cuadro de la leyenda
      legend.box.background = element_rect(color = 'white'),
      plot.title = element_text(hjust = 0.5)
    ) +
    coord_cartesian(ylim = c(0, 1))  
  )

  ggsave("plot_average.png", plot = last_plot(), width = 6, height = 4, units = "in", dpi = 200, bg = "white")
}

#######################################################################################

# Genera boxplot
if (box_plot_save == TRUE){
  boxplot_t0 <- print(ggplot(boxplot_data, aes(x=condition, y=t0)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(color = "black", width = 0.07, alpha = 0.9, size = .7) + 
    geom_hline(yintercept = c(10, 15, 17), 
               linetype = c("dotdash", "dashed", "dotted"), 
               color = "black") +
    geom_text(aes(x = rep(c(1.5,2,2.5), length.out = nrow(boxplot_data)), 
                  y = rep(c(10, 15, 17), length.out = nrow(boxplot_data)), 
                  label = rep(c("Run", "Stop", "Reset"), 
                  length.out = nrow(boxplot_data))),
                  size = 3, vjust = -1.5,
                  color = "black")
    +
    labs(title = "",
         x = "Condición",
         y = "Tiempo de máxima respuesta") +
    ylim(6, 18) +
    theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(color = "black"))
  )
  
  ggsave("boxplot_peaks.png", plot = last_plot(), 
         width = 6, height = 4, units = "in", dpi = 200, bg = "white")
}

#boxplot_data_fried = boxplot_data
#boxplot_data_fried$condition = as.factor(boxplot_data_fried$condition)

#fried <- friedman.test(t0 ~ condition | subject, data = boxplot_data_fried)
#print(fried)