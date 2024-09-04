#read in data
library(tidyverse)
library(gtsummary)
library(here)
ikea <- read.csv(here::here("ikea.csv"))
view(ikea)
glimpse(ikea)

# gt summary table
tbl_summary(
	ikea,
	by = sellable_online,
	include = c(category, other_colors),
label = list(
	category ~"Furniture Category",
	other_colors ~"Other Colors Sold"
))

# regression and results
tbl_uvregression(
	ikea,
	y = price,
	include = c(category, sellable_online, other_colors, height, depth, width),
	method = lm,
	label = list(
		category ~"Furniture Category",
		other_colors ~"Other Colors Sold",
		sellable_online ~"Sellable Online",
  	height ~"Height in cm",
  	depth ~"Depth in cm",
  	width ~"Width in cm"),
) |>
	bold_labels()

#data figure: Basic histogram with custom titles and labels
hist(ikea$width,
		 main = "Distribution of IKEA Furniture Width",  # Title of the histogram
		 xlab = "Width (in cm)",                         # X-axis label
		 ylab = "Frequency",                             # Y-axis label
		 col = "#1E90FF",                                   # Color of the bars
		 border = "black")                               # Color of the bar borders

ggsave(here("ikeawidthdistr.png"))

#custom function for sd
custom_sd <- function(x) {
	n <- length(x)
	mean_x <- mean(x, na.rm = TRUE)
	sqrt(sum((x - mean_x)^2) / (n - 1))
}

# Usage:
custom_sd(ikea$price)
