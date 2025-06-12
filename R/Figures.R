#To Do: 
# ad code to save 2d plot
#plot other spp
#add red trace line to denot change from neg to pos
#or change code to standardize scales across panels
#plot sample size =100 and =500


library(ggplot2)
library(tidyr)
library(patchwork)
library(plotly)
library(reshape2)
library(cowplot)

setwd("C:/Users/Derek.Chamberlin/Work/Research/VBGF_Age_Error_Selectivity_Repo")

#quick figure
plot(blackgill_results[[6]][[1]][[2]],blackgill_results[[6]][[1]][[3]], xlim = c(0,100), ylim = c(0,1000)) #sampled true age and observed length
points(blackgill_results[[6]][[1]][[4]],blackgill_results[[6]][[1]][[3]], col = "red") #observed age and observed length
curve(blackgill_results[[6]][[1]][[5]][1]$vblinf * (1 - exp(-blackgill_results[[6]][[1]][[5]][2]$vbk * (x + blackgill_results[[6]][[1]][[5]][3]$vbto))), add = TRUE, col = "blue", lwd = 2)





plot(blackgill_results[[116]][[1]][[2]],blackgill_results[[116]][[1]][[3]], xlim = c(0,100), ylim = c(0,1000)) #sampled true age and observed length
points(blackgill_results[[116]][[1]][[4]],blackgill_results[[116]][[1]][[3]], col = "red") #observed age and observed length
curve(blackgill_results[[116]][[1]][[5]][1]$vblinf * (1 - exp(-blackgill_results[[116]][[1]][[5]][2]$vbk * (x + blackgill_results[[116]][[1]][[5]][3]$vbto))), add = TRUE, col = "blue", lwd = 2)

plot(blackgill_results[[117]][[1]][[2]],blackgill_results[[117]][[1]][[3]], xlim = c(0,100), ylim = c(0,1000)) #sampled true age and observed length
points(blackgill_results[[117]][[1]][[4]],blackgill_results[[117]][[1]][[3]], col = "red") #observed age and observed length
curve(blackgill_results[[117]][[1]][[5]][1]$vblinf * (1 - exp(-blackgill_results[[117]][[1]][[5]][2]$vbk * (x + blackgill_results[[117]][[1]][[5]][3]$vbto))), add = TRUE, col = "blue", lwd = 2)

plot(blackgill_results[[118]][[1]][[2]],blackgill_results[[118]][[1]][[3]], xlim = c(0,100), ylim = c(0,1000)) #sampled true age and observed length
points(blackgill_results[[118]][[1]][[4]],blackgill_results[[118]][[1]][[3]], col = "red") #observed age and observed length
curve(blackgill_results[[118]][[1]][[5]][1]$vblinf * (1 - exp(-blackgill_results[[118]][[1]][[5]][2]$vbk * (x + blackgill_results[[118]][[1]][[5]][3]$vbto))), add = TRUE, col = "blue", lwd = 2)

plot(blackgill_results[[119]][[1]][[2]],blackgill_results[[119]][[1]][[3]], xlim = c(0,100), ylim = c(0,1000)) #sampled true age and observed length
points(blackgill_results[[119]][[1]][[4]],blackgill_results[[119]][[1]][[3]], col = "red") #observed age and observed length
curve(blackgill_results[[119]][[1]][[5]][1]$vblinf * (1 - exp(-blackgill_results[[119]][[1]][[5]][2]$vbk * (x + blackgill_results[[119]][[1]][[5]][3]$vbto))), add = TRUE, col = "blue", lwd = 2)






#Boxplot
param_box_plot <- function(df, param){
  base_plot <- ggplot(df, aes(x = as.factor(sel_1), y = {{ param }}, fill = as.factor(sel_2))) +
    geom_boxplot() +
    geom_hline(yintercept = 0, color = "red", linewidth = 0.8) +
    facet_wrap(~ CV_Age, ncol = 5, labeller = as_labeller(function(value) paste("CV =", value))) +
    scale_y_continuous(limits = c(-1.5, 1.5), breaks = seq(-1.5, 1.5, by = 0.25)) +
    labs(fill = "sel_2") +
    theme_minimal() +
    theme(
      strip.placement = "outside",
      panel.spacing = unit(-1.75, "pt"),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      strip.text = element_text(size = 10)
    )
  
  return(base_plot)
}

blackgill_plot <- param_box_plot(subset(blackgill_flat, blackgill_flat$sample_size == 500 & blackgill_flat$sel_1 %in% c(0, 200, 400)), k_RE) + theme(legend.position = "none")
blue_plot <- param_box_plot(subset(blue_flat, blue_flat$sample_size == 500 & blue_flat$sel_1 %in% c(0, 200, 400)), k_RE) + theme(legend.position = "none")
calico_plot <- param_box_plot(subset(calico_flat, calico_flat$sample_size == 500 & calico_flat$sel_1 %in% c(0, 200, 400)), k_RE) + theme(legend.position = "none")
olive_plot <- param_box_plot(subset(olive_flat, olive_flat$sample_size == 500 & olive_flat$sel_1 %in% c(0, 200, 400)), k_RE) + theme(legend.position = "right")

# Create plots (3 w/o legend, 1 with legend)
p1 <- blackgill_plot + theme(legend.position = "none")
p2 <- blue_plot + theme(legend.position = "none")
p3 <- calico_plot + theme(legend.position = "none")
p4 <- olive_plot + theme(legend.position = "right")

# Combine the 4 vertically, collect legend
combined <- (blackgill_plot / blue_plot / olive_plot / calico_plot) + plot_layout(guides = "collect") & theme(legend.position = "right")

# Convert to ggdraw so we can manually place text
final_plot <- ggdraw() +
  draw_plot(combined, x = 0.02, y = 0.02, width = 0.99, height = 0.99) +
  draw_label("Selection 1 (sel_1)", x = 0.5, y = 0.01, vjust = 0, size = 16, fontface = "bold") +
  draw_label("Relative error in K", x = 0.01, y = 0.5, angle = 90, vjust = 1, size = 16, fontface = "bold")

# Show final plot
final_plot

ggsave("C:/Users/Derek.Chamberlin/Work/Research/VBGF_Age_Error_Selectivity_Repo/k_RE_boxplot.png", plot = final_plot, width = 8.5, height = 11, units = "in")














#2-D Heatmap
surface_plot_2d_ggplot <- function(df, sample_size_n, subset_param, subset_param_value, x_param, y_param, z_param){
  plot_data <- subset(df, sample_size == as.numeric(sample_size_n))
  plot_data <- subset(plot_data, get(subset_param) == subset_param_value)
  
  surface_data <- dcast(plot_data, sel_1 ~ CV_Age, value.var = z_param)
  
  long_data <- melt(surface_data, id.vars = "sel_1", variable.name = "CV_Age", value.name = "value")
  
  long_data$value <- as.numeric(long_data$value)
  
  ggplot(long_data, aes(x = sel_1, y = CV_Age, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(name = z_param) +#, limits = c(-0.5, 0.5), oob = scales::squish) +
    labs(title = paste(z_param), x = x_param, y = y_param) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1, size = 12),
      axis.text.y = element_text(angle = 0, size = 12),
      plot.title = element_text(hjust = 0.5),   # Center the title
      panel.grid = element_blank(),
      axis.title.x = element_text(size = 14),   # Increase font size of x-axis label
      axis.title.y = element_text(size = 14)
    ) +
    guides(fill = guide_colorbar(title = NULL))
}

blackgill_L_inf_plot_2D <- surface_plot_2d_ggplot(blackgill_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_L_inf")
blackgill_k_plot_2D <- surface_plot_2d_ggplot(blackgill_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_k")
blackgill_t_0_plot_2D <- surface_plot_2d_ggplot(blackgill_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_t_0")
blackgill_CV_L_plot_2D <- surface_plot_2d_ggplot(blackgill_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_CV_L")

blackgill_combined_plot <- (blackgill_L_inf_plot_2D | blackgill_k_plot_2D) / 
  (blackgill_t_0_plot_2D | blackgill_CV_L_plot_2D)
blackgill_combined_plot


blue_L_inf_plot_2D <- surface_plot_2d_ggplot(blue_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_L_inf")
blue_k_plot_2D <- surface_plot_2d_ggplot(blue_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_k")
blue_t_0_plot_2D <- surface_plot_2d_ggplot(blue_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_t_0")
blue_CV_L_plot_2D <- surface_plot_2d_ggplot(blue_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_CV_L")

blue_combined_plot <- (blue_L_inf_plot_2D | blue_k_plot_2D) / 
  (blue_t_0_plot_2D | blue_CV_L_plot_2D)
blue_combined_plot


olive_L_inf_plot_2D <- surface_plot_2d_ggplot(olive_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_L_inf")
olive_k_plot_2D <- surface_plot_2d_ggplot(olive_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_k")
olive_t_0_plot_2D <- surface_plot_2d_ggplot(olive_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_t_0")
olive_CV_L_plot_2D <- surface_plot_2d_ggplot(olive_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_CV_L")

olive_combined_plot <- (olive_L_inf_plot_2D | olive_k_plot_2D) / 
  (olive_t_0_plot_2D | olive_CV_L_plot_2D)
olive_combined_plot


calico_L_inf_plot_2D <- surface_plot_2d_ggplot(calico_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_L_inf")
calico_k_plot_2D <- surface_plot_2d_ggplot(calico_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_k")
calico_t_0_plot_2D <- surface_plot_2d_ggplot(calico_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_t_0")
calico_CV_L_plot_2D <- surface_plot_2d_ggplot(calico_results_df, 500, "sel_2", 10, "sel_1", "CV_Age", "mean_re_CV_L")

calico_combined_plot <- (calico_L_inf_plot_2D | calico_k_plot_2D) / 
  (calico_t_0_plot_2D | calico_CV_L_plot_2D)
calico_combined_plot






#3-D Surface plots
surface_plot <- function(df, sample_size_n, subset_param, subset_param_value, x_param, y_param, z_param){
  plot_data <- subset(df, sample_size == as.numeric(sample_size_n))
  plot_data <- subset(plot_data, get(subset_param) == subset_param_value)
  formula_str <- as.formula(paste0(x_param, "~", y_param, collapse = ""))#issue!!! Why isn't this being used?
  surface_data <- dcast(plot_data, CV_Age ~ sel_1, value.var = z_param)
  z_matrix <- as.matrix(surface_data[, -1])
  
  # Create the surface plot
  plot_ly(
    x = colnames(z_matrix), 
    y = surface_data[[y_param]],  # CV_Age values as y axis labels
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

htmlwidgets::saveWidget(blackgill_L_inf_plot, file = "blackgill_L_inf_plot.html")
htmlwidgets::saveWidget(blackgill_k_plot, file = "blackgill_k_plot.html")
htmlwidgets::saveWidget(blackgill_t_0_plot, file = "blackgill_t_0_plot.html")
htmlwidgets::saveWidget(blackgill_CV_L_plot, file = "blackgill_CV_L_plot.html")



# Create the 3D scatter plot
plot_ly(data = subset(blackgill_results_df, sample_size == 500), 
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
