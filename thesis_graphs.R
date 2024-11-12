# Setting working directory
setwd("C:/Users/Eier/Documents/MAE4000/data/")

# Loading required libraries
library(mirt)
library(mirtCAT)
library(readxl)
library(ggplot2)
library(parallel)
library(gridExtra)
library(grid)
library(dplyr)
library(patchwork) 

# Import real response data of May 2023 session
may23 <- read_excel("may2023.xlsx",col_types = "numeric")

# These items were used in the real test
response <- may23[c(1:60, 68:80, 88:100)]

# Recoding polytomous items
response[, c(60, 73)] <- lapply(response[, c(60, 73)], function(col) {
  ifelse(col %in% c(2, 3, 4), 1, col)
})

# Replace 9s and 99s
response <- apply(response, 2, function(col) {
  col[col == 99] <- NA  # items that are not seen and not completed
  col[col == 9] <- 0    # items that are seen but not completed
  return(col)
})
# Rescoring TRUE/False to 1 and 0
response <- ifelse(response == TRUE, 1, ifelse(response == FALSE, 0, response))

# Defining the structure of the multi-stage test with items for each module
modules <- list(
  Pretest1 = 1:7,    # items 1 to 7 are for Pretest1
  `2A` = 8:14,       # items 8 to 14 are for module 2A
  `2B` = 15:22,      # items 15 to 22 are for module 2B
  A1A2 = 23:38,      # items 23 to 38 are for module A1A2
  A2B1 = 39:60,      # items 39 to 60 are for module A2B1
  B1B2 = 61:86       # items 61 to 86 are for module B1B2
)

# Defining routing rules based on the number of correct answers
routing_rules <- list(
  Pretest1 = function(score) {
    if (!is.na(score) && score <= 4) {
      return("2A")
    } else if (!is.na(score)) {
      return("2B")
    } else {
      return(NULL)
    }
  },
  `2A` = function(score) {
    if (!is.na(score) && score <= 5) {
      return("A1A2")
    } else {
      return("A2B1")
    }
  },
  `2B` = function(score) {
    if (!is.na(score) && score <= 4) {
      return("A2B1")
    } else {
      return("B1B2")
    }
  }
)

READM1 <- mirt(response[, 1:86], 1, itemtype = "2PL", SE = TRUE,
               technical = list(NCYCLES = 4000), module = modules, routing = routing_rules)
coef(READM1, IRTpars = TRUE, simplify = TRUE)
READPars1 <- as.data.frame(coef(READM1, IRTpars = TRUE, simplify = TRUE))
READPars1 <- READPars1[, 1:2]
colnames(READPars1) <- c("a1", "d")

###### Generate simulated responses for MST#####
#####Creating a hybrid data with entire response#####
thetas_mst <- fscores(READM1)
simulated_responses_mst <- matrix(NA, nrow = length(thetas_mst), ncol = 86)
for (i in 1:length(thetas_mst)) {
  pat_mst <- generate_pattern(READM1, Theta = thetas_mst[i])
  simulated_responses_mst[i, ] <- pat_mst
}
### Convert hybrid simulated responses to a data frame
simulated_responses_mst <- as.data.frame(simulated_responses_mst)
colnames(simulated_responses_mst) <- paste0("Item_", 1:86)
model_simulated_mst <- mirt(simulated_responses_mst[, c(1:86)], itemtype = "2PL")



########### TEST INFORMATION ##############

##### Calculate test information for each module

Theta <- matrix(seq(-4, 4, 0.01), ncol = 1)
Pretest1 <- testinfo(model_simulated_mst, Theta = Theta, which.items = c(1:7))
Pre2A <- testinfo(model_simulated_mst, Theta = Theta, which.items = c(8:14))
Pre2B <- testinfo(model_simulated_mst, Theta = Theta, which.items = c(15:22))
A1A2 <- testinfo(model_simulated_mst, Theta = Theta, which.items = c(23:38))
A2B1 <- testinfo(model_simulated_mst, Theta = Theta, which.items = c(39:60))
B1B2 <- testinfo(model_simulated_mst, Theta = Theta, which.items = c(61:86))

####TEST INFORMATION BY MODULE#####
# Combine data into a single data frame for the stages
data_reading <- data.frame(
  Theta = rep(Theta, 3),
  Information = c(Pretest1, Pre2A, Pre2B),
  Stage = factor(rep(c("Pretest 1", "2A", "2B"), each = length(Theta)))
)

data_reading$Stage <- factor(data_reading$Stage, levels = c("Pretest 1", "2A", "2B"))

# Define the line colors and types 
line_colors_reading <- c("Pretest 1" = "black", "2A" = "red", "2B" = "blue")
line_types_reading <- c("Pretest 1" = "solid", "2A" = "solid", "2B" = "solid")

# Create the plot
p_reading <- ggplot(data_reading, aes(x = Theta, y = Information, color = Stage, linetype = Stage)) +
  geom_line(size = 0.7) +
  scale_color_manual(values = line_colors_reading) +
  scale_linetype_manual(values = line_types_reading) +
  labs(x = expression(theta), y = "Test Information") +
  ylim(0, 40) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_blank(),  
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 14),
    legend.key.size = unit(1, "lines"),
    axis.line = element_line(color = "black")
  )

# Print the plot
print(p_reading)


data_reading2 <- data.frame(
  Theta = rep(Theta, 3),
  Information = c(A1A2, A2B1, B1B2),
  Stage = factor(rep(c("A1A2", "A2B1", "B1B2"), each = length(Theta)))
)

data_reading2$Stage <- factor(data_reading2$Stage, levels = c("A1A2", "A2B1", "B1B2"))

# Define the line colors and types 
line_colors_reading2 <- c("A1A2" = "black", "A2B1" = "red", "B1B2" = "blue")
line_types_reading2 <- c("A1A2" = "solid", "A2B1" = "solid", "B1B2" = "solid")

# Create the plot without a title and extend y-axis limits
p_reading2 <- ggplot(data_reading2, aes(x = Theta, y = Information, color = Stage, linetype = Stage)) +
  geom_line(size = 0.7) +
  scale_color_manual(values = line_colors_reading2) +
  scale_linetype_manual(values = line_types_reading2) +
  labs(x = expression(theta), y = "Test Information") +
  ylim(0, 40) +  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_blank(), 
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 14),
    legend.key.size = unit(1, "lines"),
    axis.line = element_line(color = "black")
  )

# Print the plot
print(p_reading2)
grid.arrange(p_reading, p_reading2, ncol = 2)

# Combine the plots using grid.arrange 
Info_module <- grid.arrange(p_reading, p_reading2, ncol = 2)

# Save the combined plot 
ggsave("Info_module.tiff", plot = Info_module, dpi = 300, width = 8, height = 5,units = "in")



#############LANDSCAPE PLOTS###############

# Load the combined_data
combined_data_loaded <- readRDS("combined_data.rds")

#####LANDSCAPE PLOT FOR SE#####

# Function to create landscape plot comparing SE between MST and another test format 
landscape_SE_ggplot <- function(data, x_test, y_test, ll = 0.1, hl = 0.6, 
                                xlab = "SE Administration X", ylab = "SE Administration Y", color_scale) {
  
  # Extract SE values for MST and the specified test format
  SE1 <- data$SE[data$Test == x_test]
  SE2 <- data$SE[data$Test == y_test]
  theta <- data$thetatrue[data$Test == x_test]  
  
  # Create a dataframe with the SE values and ability
  plot_data <- data.frame(SE1 = SE1, SE2 = SE2, theta = theta)
  
  # Calculate median points
  median_SE1 <- median(SE1, na.rm = TRUE)
  median_SE2 <- median(SE2, na.rm = TRUE)
  
  # Create the plot using ggplot2
  p <- ggplot(plot_data, aes(x = SE1, y = SE2, color = theta)) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "solid", size = 0.7) +  # Main diagonal line
    geom_vline(xintercept = median_SE1, color = "grey", size = 0.7) +  # Vertical median line
    geom_hline(yintercept = median_SE2, color = "grey", size = 0.7) +  # Horizontal median line
    scale_x_continuous(limits = c(ll, hl), breaks = seq(ll, hl, by = 0.1)) +
    scale_y_continuous(limits = c(ll, hl), breaks = seq(ll, hl, by = 0.1)) +
    coord_fixed(ratio = 1) +  
    color_scale +  
    labs(x = xlab, y = ylab) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),  
      axis.line = element_line(color = "black", size = 1),  
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14),  
      legend.position = "none"  
    )
  
  return(p)
}

# Define a common color scale for all plots
common_color_scale <- scale_color_gradientn(
  colors = c("lightblue", "#1f78b4", "#08306b"), 
  name = expression(theta),
  guide = guide_colorbar(ticks.colour = "black", barwidth = 10, barheight = 1)
)

# List of unique test types, excluding MST
test_types <- unique(combined_data_loaded$Test)
test_types <- test_types[test_types != "MST"]


plots <- list()

# Loop through each test type and create a plot comparing it with MST
for (test_type in test_types) {

  p <- landscape_SE_ggplot(data = combined_data_loaded, 
                           x_test = "MST", 
                           y_test = test_type, 
                           xlab = "MST: SE", 
                           ylab = paste(test_type, ": SE"),
                           color_scale = common_color_scale)  
 
  plots[[test_type]] <- p
}

# Function to extract legend 
get_legend <- function(myggplot) {
  tmp <- ggplotGrob(myggplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend_plot <- ggplot(combined_data_loaded, aes(x = SE, y = SE, color = thetatrue)) +
  geom_point() + common_color_scale + theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20),  
        legend.title = element_text(size = 20)  
  )

# Extract the legend
legend <- get_legend(legend_plot)

# Remove legends from individual plots
plots <- lapply(plots, function(p) p + theme(legend.position = "none"))

# Arrange all ggplot objects in a grid with a common horizontal legend
combined_plot_SE <- wrap_plots(plots, ncol = 3) & theme(legend.position = "none")
spacer <- plot_spacer()

# Arrange the first row with three plots
row1 <- wrap_plots(plots[1:3], ncol = 3) & theme(legend.position = "none")

# Arrange the second row with two plots and a spacer in the third position
row2 <- wrap_plots(plots[4:5], plot_spacer(), ncol = 3) & theme(legend.position = "none")

# Combine rows with a spacer in between and add the legend at the bottom
final_plot_SE <- (row1 / plot_spacer() / row2) / legend + 
  plot_layout(heights = c(10, 1, 10, 2))
print(final_plot_SE)
# Save the plot
ggsave("SE_comparison.tiff", plot = final_plot_SE,dpi = 100, width = 10, height = 8, units = "in")

#########################
##### TEST LENGTH (J) #####

# Function to create landscape plot comparing Test Length (J) between MST and another test format
landscape_test_length_ggplot <- function(data, x_test, y_test, ll = 0, hl = 86, 
                                         xlab = "Test Length: MST", ylab = "Test Length: Administration Y", 
                                         color_scale) {
  
  # Extract test lengths for MST and the specified test format
  J_MST <- data$J[data$Test == x_test]
  J_Other <- data$J[data$Test == y_test]
  theta <- data$thetatrue[data$Test == x_test]  # Ability level
  
  # Create a dataframe with the test length values and ability
  plot_data_length <- data.frame(J_MST = J_MST, J_Other = J_Other, theta = theta)
  
  # Calculate median points for MST and the other test
  median_J_MST <- median(J_MST, na.rm = TRUE)
  median_J_Other <- median(J_Other, na.rm = TRUE)
  
  # Create the plot using ggplot2
  p <- ggplot(plot_data_length, aes(x = J_MST, y = J_Other, color = theta)) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "solid", size = 0.7) +  # Main diagonal line
    geom_vline(xintercept = median_J_MST, color = "grey", size = 0.7) +  # Vertical median line for MST
    geom_hline(yintercept = median_J_Other, color = "grey", size = 0.7) +  # Horizontal median line for the other test
    scale_x_continuous(limits = c(ll, hl), breaks = seq(ll, hl, by = 10)) +
    scale_y_continuous(limits = c(ll, hl), breaks = seq(ll, hl, by = 10)) +
    coord_fixed(ratio = 1) +  # Fix the aspect ratio so x and y scales are the same
    color_scale +  # Common color scale based on ability level (theta)
    labs(x = xlab, y = ylab) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),  # Remove background grid
      axis.line = element_line(color = "black", size = 1),  # Add black axis lines
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14),  
      legend.position = "none"  # Hide the legend in individual plots
    )
  
  return(p)
}

# Define a common color scale for all plots based on ability (theta)
common_color_scale <- scale_color_gradientn(
  colors = c("lightblue", "#1f78b4", "#08306b"), 
  name = expression(theta),
  guide = guide_colorbar(ticks.colour = "black", barwidth = 10, barheight = 1)
)

# List of unique test types, excluding MST
test_types <- unique(combined_data_loaded$Test)
test_types <- test_types[test_types != "MST"]

length_plots <- list()

# Loop through each test type and create a plot comparing test length (J) with MST
for (test_type in test_types) {
  
  p <- landscape_test_length_ggplot(data = combined_data_loaded, 
                                    x_test = "MST", 
                                    y_test = test_type, 
                                    xlab = "MST: Test Length", 
                                    ylab = paste(test_type, ": Test Length"),
                                    color_scale = common_color_scale)  
  
  length_plots[[test_type]] <- p
}

# Function to extract the legend 
get_legend <- function(myggplot) {
  tmp <- ggplotGrob(myggplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend_plot <- ggplot(combined_data_loaded, aes(x = J, y = J, color = thetatrue)) +
  geom_point() + common_color_scale + theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20),  
        legend.title = element_text(size = 20)  
  )

# Extract the legend
legend_length_plot <- get_legend(legend_plot)

# Remove legends from individual plots
length_plots <- lapply(length_plots, function(p) p + theme(legend.position = "none"))

# Arrange all ggplot objects in a grid with a common horizontal legend
combined_plot_length <- wrap_plots(length_plots, ncol = 3) & theme(legend.position = "none")

# Arrange the first row with three plots
row1_length <- wrap_plots(length_plots[1:3], ncol = 3) & theme(legend.position = "none")

# Arrange the second row with two plots and a spacer in the third position
row2_length <- wrap_plots(length_plots[4:5], plot_spacer(), ncol = 3) & theme(legend.position = "none")

# Combine rows with a spacer in between and add the legend at the bottom
final_plot_length <- (row1_length / plot_spacer() / row2_length) / legend + 
  plot_layout(heights = c(10, 1, 10, 2))

# Display the final combined plot
print(final_plot_length)

# Save the plot as a high-resolution JPEG file
ggsave("TestLength_comparison.tiff", plot = final_plot_length, dpi = 100, width = 10, height = 8, units = "in")




##########################
#########BIAS###########

# Function to create landscape plot comparing BIAS between MST and another test format 
bias_comparison_plot <- function(data, x_test_name, y_test_name, lower_limit = -1.5, upper_limit = 1.5, 
                                 x_axis_label = "BIAS Method X", y_axis_label = "BIAS Method Y", bias_color_scale) {
  
  # Extract BIAS values for MST and the specified test format
  bias_values_x <- data$BIAS[data$Test == x_test_name]
  bias_values_y <- data$BIAS[data$Test == y_test_name]
  ability_values <- data$thetatrue[data$Test == x_test_name]  
  
  bias_plot_data <- data.frame(BIAS_X = bias_values_x, BIAS_Y = bias_values_y, Ability = ability_values)
  
  # Calculate median points
  median_bias_x <- median(bias_values_x, na.rm = TRUE)
  median_bias_y <- median(bias_values_y, na.rm = TRUE)
  
  # Create the plot 
  p_bias <- ggplot(bias_plot_data, aes(x = BIAS_X, y = BIAS_Y, color = Ability)) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "solid", size = 0.7) +  # Main diagonal line
    geom_vline(xintercept = median_bias_x, color = "grey", size = 0.7) +  # Vertical median line
    geom_hline(yintercept = median_bias_y, color = "grey", size = 0.7) +  # Horizontal median line
    scale_x_continuous(limits = c(lower_limit, upper_limit), breaks = seq(lower_limit, upper_limit, by = 1)) +
    scale_y_continuous(limits = c(lower_limit, upper_limit), breaks = seq(lower_limit, upper_limit, by = 1)) +
    coord_fixed(ratio = 1) +  
    bias_color_scale +  
    labs(x = x_axis_label, y = y_axis_label) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),  
      axis.line = element_line(color = "black", size = 1), 
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14), 
      legend.position = "none"  
    )
  
  return(p_bias)
}

# Define a common color scale for all BIAS plots
common_bias_color_scale <- scale_color_gradientn(
  colors = c("lightblue", "#1f78b4", "#08306b"), 
  name = expression(theta),
  guide = guide_colorbar(ticks.colour = "black", barwidth = 10, barheight = 1)
)

# List of unique test types, excluding MST
test_types_bias <- unique(combined_data_loaded$Test)
test_types_bias <- test_types_bias[test_types_bias != "MST"]


bias_plots <- list()

# Loop through each test type and create a plot comparing it with MST
for (bias_test_type in test_types_bias) {
  # Generate the ggplot for each comparison
  p_bias <- bias_comparison_plot(data = combined_data_loaded, 
                                 x_test_name = "MST", 
                                 y_test_name = bias_test_type, 
                                 x_axis_label = "MST: BIAS", 
                                 y_axis_label = paste(bias_test_type, ": BIAS"),
                                 bias_color_scale = common_bias_color_scale)  
  # Add the plot to the list
  bias_plots[[bias_test_type]] <- p_bias
}

# Function to extract legend from ggplot
extract_legend <- function(myggplot) {
  tmp <- ggplotGrob(myggplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Extract the legend from one of the plots
legend_bias_plot <- ggplot(combined_data_loaded, aes(x = BIAS, y = BIAS, color = thetatrue)) +
  geom_point() + common_bias_color_scale + theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20),  
    legend.title = element_text(size = 20)  
  )

# Extract the legend
bias_legend <- extract_legend(legend_bias_plot)

# Remove legends from individual plots
bias_plots <- lapply(bias_plots, function(p_bias) p_bias + theme(legend.position = "none"))

# Arrange all ggplot objects in a grid with a common horizontal legend
combined_bias_plot <- wrap_plots(bias_plots, ncol = 3) & theme(legend.position = "none")

# Arrange the first row with three plots
row1_bias <- wrap_plots(bias_plots[1:3], ncol = 3) & theme(legend.position = "none")

# Arrange the second row with two plots and a spacer in the third position
row2_bias <- wrap_plots(bias_plots[4:5], plot_spacer(), ncol = 3) & theme(legend.position = "none")

# Combine rows with a spacer in between and add the legend at the bottom
final_bias_plot <- (row1_bias / plot_spacer() / row2_bias) / legend + 
  plot_layout(heights = c(10, 1, 10, 2))


# Display the final combined plot
print(final_bias_plot)

# Save the plot 
ggsave("landscape_BIAS_comparison.tiff", plot = final_bias_plot, width = 10, height = 8, dpi = 100, units = "in")


##########Item Information#########

# Function to create landscape plot comparing average item information between MST and another test format
item_info_comparison_plot <- function(data, x_test_name, y_test_name, lower_limit = 0, upper_limit = 3, 
                                      x_axis_label = "Avg. Item Information Method X", y_axis_label = "Item Information Method Y", info_color_scale) {
  
  # Extract average item information values for MST and the specified test format
  info_values_x <- data$INFOPI[data$Test == x_test_name]
  info_values_y <- data$INFOPI[data$Test == y_test_name]
  ability_values <- data$thetatrue[data$Test == x_test_name]  
  
  # Create a dataframe with the item information values and ability
  info_plot_data <- data.frame(INFO_X = info_values_x, INFO_Y = info_values_y, Ability = ability_values)
  
  # Calculate median points
  median_info_x <- median(info_values_x, na.rm = TRUE)
  median_info_y <- median(info_values_y, na.rm = TRUE)
  
  # Create the plot using ggplot2
  p_info <- ggplot(info_plot_data, aes(x = INFO_X, y = INFO_Y, color = Ability)) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "solid", size = 0.7) +  # Main diagonal line
    geom_vline(xintercept = median_info_x, color = "grey", size = 0.7) +  # Vertical median line
    geom_hline(yintercept = median_info_y, color = "grey", size = 0.7) +  # Horizontal median line
    scale_x_continuous(limits = c(lower_limit, upper_limit), breaks = seq(lower_limit, upper_limit, by = 0.5)) +
    scale_y_continuous(limits = c(lower_limit, upper_limit), breaks = seq(lower_limit, upper_limit, by = 0.5)) +
    coord_fixed(ratio = 1) +  
    info_color_scale +  
    labs(x = x_axis_label, y = y_axis_label) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),  
      axis.line = element_line(color = "black", size = 1),  
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14),  
      legend.position = "none"  
    )
  
  return(p_info)
}

# Define a common color scale for all item information plots
common_info_color_scale <- scale_color_gradientn(
  colors = c("lightblue", "#1f78b4", "#08306b"), 
  name = expression(theta),
  guide = guide_colorbar(ticks.colour = "black", barwidth = 10, barheight = 1)
)

# List of unique test types, excluding MST
test_types_info <- unique(combined_data_loaded$Test)
test_types_info <- test_types_info[test_types_info != "MST"]

# Initialize a list to store ggplot objects
info_plots <- list()

# Loop through each test type and create a plot comparing it with MST
for (info_test_type in test_types_info) {
  # Generate the ggplot for each comparison
  p_info <- item_info_comparison_plot(data = combined_data_loaded, 
                                      x_test_name = "MST", 
                                      y_test_name = info_test_type, 
                                      x_axis_label = "MST:Avg. Item Information", 
                                      y_axis_label = paste(info_test_type, ":Avg. Item Information"),
                                      info_color_scale = common_info_color_scale)  
  # Add the plot to the list
  info_plots[[info_test_type]] <- p_info
}


extract_legend_info <- function(myggplot) {
  tmp <- ggplotGrob(myggplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Extract the legend from one of the plots
legend_info_plot <- ggplot(combined_data_loaded, aes(x = INFOPI, y = INFOPI, color = thetatrue)) +
  geom_point() + common_info_color_scale + theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20),  
    legend.title = element_text(size = 20)  
  )

# Extract the legend
info_legend <- extract_legend_info(legend_info_plot)

# Remove legends from individual plots
info_plots <- lapply(info_plots, function(p_info) p_info + theme(legend.position = "none"))

# Arrange all ggplot objects in a grid with a common horizontal legend
combined_info_plot <- wrap_plots(info_plots, ncol = 3) & theme(legend.position = "none")

# Arrange the first row with three plots
row1_info <- wrap_plots(info_plots[1:3], ncol = 3) & theme(legend.position = "none")

# Arrange the second row with two plots and a spacer in the third position
row2_info <- wrap_plots(info_plots[4:5], plot_spacer(), ncol = 3) & theme(legend.position = "none")

# Combine rows with a spacer in between and add the legend at the bottom
final_info_plot <- (row1_info / plot_spacer() / row2_info) / legend + 
  plot_layout(heights = c(10, 1, 10, 2))


# Display the final combined plot
print(final_info_plot)

# Save the plot 
ggsave("landscape_Info_comparison.tiff", plot = final_info_plot, width = 10, height = 8, dpi = 100, units = "in")

#####BOX PLOT FOR LENGTH#####

filtered_data <- combined_data_loaded[combined_data_loaded$Test != "FULL", ]

# Create the boxplot 
boxplot_test_length <- ggplot(filtered_data, aes(x = Test, y = J)) +
  geom_boxplot(outlier.size = 1, outlier.shape = 16, outlier.alpha = 0.6, color = "black") + 
  coord_flip() + 
  labs(x = "", y = "Test Length") + 
  scale_y_continuous(breaks = seq(0, 80, by = 5)) + 
  theme_minimal() + 
  theme(
    axis.line = element_line(color = "black", size = 1), 
    axis.text.y = element_text(size = 14), 
    axis.text.x = element_text(size = 14), 
    axis.title.x = element_text(size = 14), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()  
  )

# Display the boxplot
print(boxplot_test_length)

# Save the plot
ggsave("boxplot_test_length_comparison.tiff", plot = boxplot_test_length, width = 8, height = 6, dpi = 100, units = "in")

###############################
######PLOT FOR EXTREME PROFICIENCY LEVEL#######

# Filter the data for CAT-EP 
catvar_data <- combined_data_loaded[combined_data_loaded$Test == "CAT-EP", ]

# Check if there is data for CAT-EP tests
if (nrow(catvar_data) > 0) {
  # Create a column to categorize test takers based on items used
  catvar_data$Category <- ifelse(catvar_data$J > 30, "Above 30 Items", "30 Items or Below")
  
  # Create the scatter plot with different colors based on the category
  CATvar <- ggplot(catvar_data, aes(x = thetahat, y = J, color = Category)) +
    geom_point(alpha = 0.6, size = 3) +
    scale_color_manual(
      values = c("30 Items or Below" = "blue", "Above 30 Items" = "red"),
      name = "", # Legend title
      labels = c("30 items or fewer", "More than 30 items") 
    ) +
    labs(x = expression(theta), y = "Number of Items Used : CAT-EP") +
    scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 1)) +  
    scale_y_continuous(limits = c(0, 90)) +  
    theme_minimal() +
    theme(
      axis.line = element_line(color = "black", size = 0.8), 
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14),
      legend.position = "top", 
      legend.title = element_text(size = 14), 
      legend.text = element_text(size = 14) 
    )
} else {
  cat("No data available for CAT-EP tests in the dataset.\n")
}

# Print the plot
print(CATvar)

# Save the plot 
ggsave("CAT-EP.jpeg", plot = CATvar, width = 10, height = 7, dpi = 600, units = "in")





# Filter the data for CAT-P85 tests
cat_data <- combined_data_loaded[combined_data_loaded$Test == "CAT-P85", ]

# Check if there is data for CAT-P85 tests
if (nrow(cat_data) > 0) {
  # Create a column to categorize test takers based on items used
  cat_data$Category <- ifelse(cat_data$J > 30, "Above 30 Items", "30 Items or Below")
  
  # Create the scatter plot with different colors based on the category
  CAT_plot <- ggplot(cat_data, aes(x = thetahat, y = J, color = Category)) +
    geom_point(alpha = 0.6, size = 3) +
    scale_color_manual(
      values = c("30 Items or Below" = "blue", "Above 30 Items" = "red"),
      name = "", # Legend title
      labels = c("30 items or fewer", "More than 30 items") 
    ) +
    labs(x = expression(theta), y = "Number of Items Used : CAT-P85") +
    scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 1)) +  
    scale_y_continuous(limits = c(0, 90)) +  
    theme_minimal() +
    theme(
      axis.line = element_line(color = "black", size = 0.8), 
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14),
      legend.position = "top", 
      legend.title = element_text(size = 14), 
      legend.text = element_text(size = 14) 
    )
} else {
  cat("No data available for CAT-P85 tests in the dataset.\n")
}

# Print the plot
print(CAT_plot)

# Save the plot 
ggsave("CAT-P85.jpeg", plot = CAT_plot, width = 10, height = 7, dpi = 600, units = "in")
############################

grid.arrange(CATvar, CAT_plot, ncol = 2)

# Combine the plots using grid.arrange 
CATtogether <- grid.arrange(CATvar, CAT_plot, ncol = 2)

# Save the combined plot 
ggsave("CATtogether.tiff", plot = CATtogether, width = 10, height = 5, dpi = 100,units = "in")





