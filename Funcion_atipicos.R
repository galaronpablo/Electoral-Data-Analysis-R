outliers <- function(data, variable) {
  var_name <- data[[variable]]  # Acceder a la variable por nombre
  
  tot <- sum(!is.na(var_name))  # Total de observaciones no NA
  na1 <- sum(is.na(var_name))   # NA iniciales
  
  # Identificar outliers usando la regla de Tukey (coef = 1.5)
  stats_outliers <- boxplot.stats(var_name, coef = 1.5)
  outlier_values <- stats_outliers$out  
  prop_outliers <- round(length(outlier_values) / tot * 100, 2)  # Proporción de outliers
  
  # Identificar valores extremos usando la regla de Tukey (coef = 3.0)
  stats_extremes <- boxplot.stats(var_name, coef = 3.0)
  extreme_values <- stats_extremes$out  
  prop_extremes <- round(length(extreme_values) / tot * 100, 2)  # Proporción de valores extremos
  
  # Filtrar datos sin outliers ni valores extremos
  data_no_outliers <- 
    data.frame(value = var_name) |> 
    filter(!(value %in% c(outlier_values, extreme_values)))
  
  # Histograma con todos los datos
  p1 <- ggplot(data.frame(value = var_name), aes(x = value)) +
    geom_histogram(fill = "steelblue", color = "black", bins = 30, alpha = 0.7) +
    labs(title = "All Observations", x = variable, y = "Count") +
    theme_minimal()
  
  # Boxplot con outliers resaltados
  p2 <- ggplot(data.frame(value = var_name), aes(x = value)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
    labs(title = "Boxplot with Outliers", x = variable, y = "") +
    theme_minimal()
  
  # Mostrar gráficos juntos con patchwork
  print(p1 / p2)
  
  # Mostrar información en la consola
  cat("\n📌 Outliers identified in", variable, ":", length(outlier_values), "outliers\n")
  cat("📊 Proportion (%) of outliers:", prop_outliers, "%\n")
  cat("\n🚨 Extreme values identified in", variable, ":", length(extreme_values), "extreme values\n")
  cat("📊 Proportion (%) of extreme values:", prop_extremes, "%\n")
  
  return(list(outliers = outlier_values, extremes = extreme_values))  # Devolver ambos valores
}
