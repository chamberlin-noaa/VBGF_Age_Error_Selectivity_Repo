library(plotly)

#3-D Surface plots




# Create the 3D scatter plot
plot_ly(data = blackgill_results_df, 
        x = ~sel_1, 
        y = ~sel_2, 
        z = ~CV_Age,
        color = ~mean_re_k,
        type = "scatter3d", 
        mode = "markers", 
        marker = list(size = 5, colorscale = "Viridis", showscale = FALSE)) %>%
  layout(title = "3D Plot of Selectivity and CV_Age",
         scene = list(
           xaxis = list(title = "sel_1"),
           yaxis = list(title = "sel_2"),
           zaxis = list(title = "CV_Age")
         ))

# Create the 3D scatter plot
plot_ly(data = blackgill_results_df, 
        x = ~sel_1, 
        y = ~CV_Age, 
        z = ~mean_re_k,
        type = "scatter3d", 
        mode = "markers", 
        marker = list(size = 5, color = ~mean_re_k, colorscale = "Viridis", showscale = TRUE)) %>%
  layout(title = "3D Plot of Selectivity and CV_Age",
         scene = list(
           xaxis = list(title = "sel_1"),
           yaxis = list(title = "CV_Age"),
           zaxis = list(title = "mean_re_k")
         ))

blackgill_results_df2 <- subset(blackgill_results_df, sample_size == 500)

library(reshape2)

surface_data <- dcast(blackgill_results_df2, sel_1 ~ CV_Age, value.var = "mean_re_k")

# Extract the matrix for surface plot
z_matrix <- as.matrix(surface_data[, -1])  # Remove sel_1 column (it should be x axis)

# Create the surface plot
plot_ly(
  x = surface_data$sel_1, 
  y = colnames(z_matrix),  # CV_Age values as y axis labels
  z = z_matrix, 
  type = "surface", 
  colorscale = "Viridis", 
  showscale = TRUE
) %>%
  layout(
    title = "Topographical Surface Plot of Selectivity and CV_Age",
    scene = list(
      xaxis = list(title = "sel_1"),
      yaxis = list(title = "CV_Age"),
      zaxis = list(title = "mean_re_k")
    )
  )


calico_results_df2 <- subset(calico_results_df, sample_size == 500)

library(reshape2)

surface_data <- dcast(calico_results_df2, sel_1 ~ CV_Age, value.var = "mean_re_k")


# Extract the matrix for surface plot
z_matrix <- as.matrix(surface_data[, -1])  # Remove sel_1 column (it should be x axis)
colnames(z_matrix) <- CV_Age

# Create the surface plot
plot_ly(
  x = surface_data$sel_1, 
  y = colnames(z_matrix),  # CV_Age values as y axis labels
  z = z_matrix, 
  type = "surface", 
  colorscale = "Viridis", 
  showscale = TRUE
) %>%
  layout(
    title = "Topographical Surface Plot of Selectivity and CV_Age",
    scene = list(
      xaxis = list(title = "sel_1"),
      yaxis = list(title = "CV_Age"),
      zaxis = list(title = "mean_re_k")
    )
  )

