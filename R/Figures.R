library(plotly)
library(reshape2)

df <- blackgill_results_df
sample_size_n <- 500
subset_param <- "sel_2"
subset_param_value <- 21 
x_param <- "sel_1"
y_param <- "CV_Age"
z_param <- "mean_re_L_inf"

#3-D Surface plots
surface_plot <- function(df, sample_size_n, subset_param, subset_param_value, x_param, y_param, z_param){
  plot_data <- subset(df, sample_size == as.numeric(sample_size_n))
  plot_data <- subset(plot_data, get(subset_param) == subset_param_value)
  formula_str <- as.formula(paste0(x_param, "~", y_param, collapse = ""))
  surface_data <- dcast(plot_data, sel_1 ~ CV_Age, value.var = z_param)
  z_matrix <- as.matrix(surface_data[, -1])
  
  # Create the surface plot
  plot_ly(
    x = surface_data[[x_param]], 
    y = colnames(z_matrix),  # CV_Age values as y axis labels
    z = z_matrix, 
    type = "surface", 
    colorscale = "Viridis", 
    showscale = TRUE
  ) %>%
    layout(scene = list(
        xaxis = list(title = x_param),
        yaxis = list(title = y_param),
        zaxis = list(title = z_param)
      )
    )
}

blackgill_L_inf_plot <- surface_plot(blackgill_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_L_inf")
blackgill_k_plot <- surface_plot(blackgill_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_k")
blackgill_t_0_plot <- surface_plot(blackgill_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_t_0")
blackgill_CV_L_plot <- surface_plot(blackgill_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_CV_L")

save_image(blackgill_k_plot, "surface-plot.png")
htmlwidgets::saveWidget(blackgill_k_plot, file = "image.html")



# Create the 3D scatter plot
plot_ly(data = subset(blackgill_results_df, sample_size == 1000), 
        x = ~sel_1, 
        y = ~sel_2, 
        z = ~CV_Age,
        #color = ~mean_re_k,
        type = "scatter3d", 
        mode = "markers", 
        marker = list(color = ~mean_re_k, size = 7, colorscale = "Jet", showscale = TRUE)) %>%
  layout(scene = list(
           xaxis = list(title = "Selectivity 1"),
           yaxis = list(title = "Selectivity 2"),
           zaxis = list(title = "CV Age")
         ))
