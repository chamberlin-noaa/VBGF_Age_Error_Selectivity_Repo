library(ggplot2)
library(tidyr)
library(patchwork)
library(plotly)
library(reshape2)
library(cowplot)
library(gratia)


setwd("G:/My Drive/Research/VBGF_Age_Error_Selectivity_Repo")
source("./R/Functions.R")

#Boxplot
param_box_plot <- function(df, param, y_range, y_range_break){
  base_plot <- ggplot(df, aes(x = as.factor(sel_1), y = {{ param }}, fill = as.factor(sel_2))) +
    geom_boxplot() +
    geom_hline(yintercept = 0, color = "red", linewidth = 0.8) +
    facet_wrap(~ CV_Age, ncol = 5, labeller = label_bquote(CV[a]~"="~.(CV_Age))) +
    scale_y_continuous(limits = c(-y_range, y_range), breaks = seq(-y_range, y_range, by = y_range_break)) +
    labs(fill = expression(Phi[2])) +
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

create_comparison_plot <- function(param, y_axis_label, sample_size_sub, sel_1_sub, sel_2_sub, y_range, y_range_break) {
  param_symbol <- rlang::sym(param)
  
  blackgill_data <- subset(blackgill_flat_logistic, sample_size == sample_size_sub & sel_1 %in% sel_1_sub & round(sel_2, 3) %in% round(sel_2_sub, 3))
  blue_data <- subset(blue_flat_logistic, sample_size == sample_size_sub & sel_1 %in% sel_1_sub & round(sel_2, 3) %in% round(sel_2_sub, 3))
  olive_data <- subset(olive_flat_logistic, sample_size == sample_size_sub & sel_1 %in% sel_1_sub & round(sel_2, 3) %in% round(sel_2_sub, 3))
  calico_data <- subset(calico_flat_logistic, sample_size == sample_size_sub & sel_1 %in% sel_1_sub & round(sel_2, 3) %in% round(sel_2_sub, 3))
  
  p1 <- param_box_plot(blackgill_data, !!param_symbol, y_range, y_range_break)
  p2 <- param_box_plot(blue_data, !!param_symbol, y_range, y_range_break)
  p3 <- param_box_plot(olive_data, !!param_symbol, y_range, y_range_break)
  p4 <- param_box_plot(calico_data, !!param_symbol, y_range, y_range_break)
  
  combined <- (p1 / p2 / p3 / p4) + plot_layout(guides = "collect") & theme(legend.position = "right")
  
  final_plot <- ggdraw() +
    draw_plot(combined, x = 0.02, y = 0.02, width = 0.99, height = 0.99) +
    draw_label(expression(bold(Phi[1])), x = 0.5, y = 0.01, vjust = 0, size = 16, fontface = "bold") +
    draw_label(y_axis_label, x = 0.01, y = 0.5, angle = 90, vjust = 1, size = 16, fontface = "bold")
  
  return(final_plot)
}

sel_1_sub <- c(0, 200, 350)
sel_2_sub <- c(0.010, 0.060, 0.110, 0.160, 0.210)

k_re_plot <- create_comparison_plot("k_RE", expression("Relative error in " * italic(k)), 500, sel_1_sub, sel_2_sub,  1.5, 0.50)
k_re_plot
ggsave("k_RE_boxplot_n_500_logistic.png", plot = k_re_plot, width = 8.5, height = 11, units = "in", bg = "white")

L_inf_re_plot <- create_comparison_plot("L_inf_RE", expression("Relative error in " * italic(L[infinity])), 500, sel_1_sub, sel_2_sub, 1, 0.25)
L_inf_re_plot
ggsave("L_inf_RE_boxplot_n_500_logistic.png", plot = L_inf_re_plot, width = 8.5, height = 11, units = "in", bg = "white")

t_0_re_plot <- create_comparison_plot("t_0_RE", expression("Relative error in " * italic(t[0])), 500, sel_1_sub, sel_2_sub, 100, 25)
t_0_re_plot
ggsave("t_0_RE_boxplot_n_500_logistic.png", plot = t_0_re_plot, width = 8.5, height = 11, units = "in", bg = "white")

sel_1_sub <- c(0, 100, 200)
t_0_re_plot <- create_comparison_plot("t_0_RE", expression("Relative error in " * italic(t[0])), 500, sel_1_sub, sel_2_sub, 15, 5)
t_0_re_plot
ggsave("t_0_RE_boxplot_n_500_logistic_dif_subset.png", plot = t_0_re_plot, width = 8.5, height = 11, units = "in", bg = "white")

#Selectivity Figure
{
  size <- 1:750
  param_combinations_dome <- list(
    "B[1]==100*','~B[2]==-2*','~B[3]==10*','~B[4]==11" = c(B1 = 100, B2 = -2, B3 = 10, B4 = 11),
    "B[1]==200*','~B[2]==-2*','~B[3]==10*','~B[4]==11" = c(B1 = 200, B2 = -2, B3 = 10, B4 = 11),
    "B[1]==200*','~B[2]==-4*','~B[3]==8*','~B[4]==9"   = c(B1 = 200, B2 = -4, B3 = 8, B4 = 9),
    "B[1]==200*','~B[2]==-1*','~B[3]==11*','~B[4]==12"  = c(B1 = 200, B2 = -1, B3 = 11, B4 = 12),
    "B[1]==300*','~B[2]==-2*','~B[3]==10*','~B[4]==11" = c(B1 = 300, B2 = -2, B3 = 10, B4 = 11)
  )
  
  param_combinations_logistic <- list(
    "Phi[1]==100*','~Phi[2]==0.035" = c(L50 = 100, slope = 0.035),
    "Phi[1]==200*','~Phi[2]==0.01" = c(L50 = 200, slope = 0.01),
    "Phi[1]==200*','~Phi[2]==0.035" = c(L50 = 200, slope = 0.035),
    "Phi[1]==200*','~Phi[2]==0.21" = c(L50 = 200, slope = 0.21),
    "Phi[1]==300*','~Phi[2]==0.035" = c(L50 = 300, slope = 0.035)

  )
  
  df_dome <- do.call(rbind, lapply(names(param_combinations_dome), function(name) {
    params <- param_combinations_dome[[name]]
    data.frame(
      size = size,
      selectivity = dome_selectivity_function(size, params["B1"], params["B2"], params["B3"], params["B4"]),
      combination = name
    )
  }))
  
  df_logistic <- do.call(rbind, lapply(names(param_combinations_logistic), function(name) {
    params <- param_combinations_logistic[[name]]
    data.frame(
      size = size,
      selectivity = logistic_selectivity_function(size, params["L50"], params["slope"]),
      combination = name
    )
  }))
  
  df_dome$combination <- factor(df_dome$combination, levels = names(param_combinations_dome))
  df_logistic$combination <- factor(df_logistic$combination, levels = names(param_combinations_logistic))

  p1 <- ggplot(df_logistic, aes(x = size, y = selectivity, color = combination)) +
    geom_line(linewidth = 1) +
    scale_x_continuous(
      expand = c(0, 0), 
      limits = c(0, max(size)), 
      breaks = seq(0, 750, by = 100)
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1)) +
    labs(
      x = "Size (mm)",
      y = "Selectivity",
      color = "Parameters"
    ) +
    theme_minimal() +
    scale_color_viridis_d(labels = scales::parse_format()) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
    ) +
    annotate("text", x=20, y=1.02, label= "A", size = 5.5, fontface = "bold")
  
  p2 <- ggplot(df_dome, aes(x = size, y = selectivity, color = combination)) +
    geom_line(linewidth = 1) +
    scale_x_continuous(
      expand = c(0, 0), 
      limits = c(0, max(size)), 
      breaks = seq(0, 750, by = 100)
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1)) +
    labs(
      x = "Size (mm)",
      y = "Selectivity",
      color = "Parameters"
    ) +
    theme_minimal() +
    scale_color_viridis_d(labels = scales::parse_format()) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
    ) +
    annotate("text", x=20, y=1.02, label= "B", size = 5.5, fontface = "bold")
  
  final_plot <- p1 / p2
  
  final_plot <- ggdraw() +
    draw_plot(final_plot, x = 0.02, y = 0.02, width = 0.99, height = 0.99) +
    draw_label("Length (mm)", x = 0.5, y = 0.01, vjust = 0, size = 16, fontface = "bold") +
    draw_label("Selectivity", x = 0.01, y = 0.5, angle = 90, vjust = 1, size = 16, fontface = "bold")
  
  final_plot
  
  ggsave("selectivity_plot.png", plot = final_plot, width = 6.5, height = 4, units = "in", bg = "white")
}

format_gam_plot <- function(gam_model, y_label) {
  p <- draw(gam_model, rug = FALSE)
  
  p[[1]] <- p[[1]] + 
    labs(
      title = NULL, 
      subtitle = NULL, 
      caption = NULL,  
      x = expression(CV[Age]),
      y = y_label
    )
  
  p[[2]] <- p[[2]] + 
    labs(
      title = NULL, 
      subtitle = NULL, 
      caption = NULL,  
      x = expression(Phi[1]), 
      y = expression(Phi[2])
    )
  
  return(p)
}

p_k <- format_gam_plot(gam_k_clean, expression(paste("Effect on ", RE[k])))
p_L <- format_gam_plot(gam_L_clean, expression(paste("Effect on ", RE[L[infinity]])))
p_t <- format_gam_plot(gam_t_clean, expression(paste("Effect on ", RE[t[0]])))

final_combined_plot <- (p_k / p_L / p_t) & theme_classic()

final_combined_plot

ggsave("logistic_vbgf_gam.png", plot = final_combined_plot, width = 8.5, height = 11, units = "in", bg = "white")

format_size_bam_plot <- function(bam_model, y_label) {
  p <- draw(bam_model, rug = FALSE)
  
  p[[1]] <- p[[1]] + 
    labs(
      title = NULL, 
      subtitle = NULL, 
      caption = NULL,  
      x = "Age",
      y = y_label
    )
  
  p[[2]] <- p[[2]] + 
    labs(
      title = NULL, 
      subtitle = NULL, 
      caption = NULL,  
      x = expression(CV[Age]),
      y = "Partial effect"
    )
  
  p[[3]] <- p[[3]] + 
    labs(
      title = NULL, 
      subtitle = NULL, 
      caption = NULL,  
      x = "Age", 
      y = expression(Phi[1])
    )
  
  p[[4]] <- p[[4]] + 
    labs(
      title = NULL, 
      subtitle = NULL, 
      caption = NULL,  
      x = "Age", 
      y = expression(Phi[2])
    )
  
  return(p)
}

size_plot <- format_size_bam_plot(bam_size_at_age_relative_clean, expression(paste("Effect on ", RE[size-at-age])))

final_size_plot <- size_plot & theme_classic()

final_size_plot

ggsave("logistic_size_at_age_bam.png", plot = final_size_plot, width = 8.5, height = 8.5, units = "in", bg = "white")

