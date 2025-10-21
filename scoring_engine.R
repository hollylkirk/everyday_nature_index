###### Setup ######

# Load required packages
library(openxlsx)
library(ggplot2)
library(ggimage)
library(geomtextpath)
library(dplyr)
library(lubridate)

`%ni%` <- Negate(`%in%`)


###### Prepare Data ######

# ##EXCEL##
# # Read in raw data from excel file
# raw_data <- read.xlsx("Westgarth_Survey_Result_Mod.xlsx",
#                       "Sheet1",
#                       rows = c(3:18))
# # Fix dates from excel
# raw_data[ , 1:2] <- lapply(1:2, function(x) convertToDateTime(raw_data[ , x], origin = "1899-12-30", tz = "Australia/Melbourne"))
# 
# # Reformat and rename columns
# colnames(raw_data) <- gsub("\\.", "_", colnames(raw_data))
# colnames(raw_data) <- c(colnames(raw_data)[1:2],
#                         "Duration",
#                         "Latitude",
#                         "Longitude",
#                         "Reference",
#                         "Percent_0_10cm",
#                         "Percent_10_50cm",
#                         "Percent_50_200cm",
#                         "Percent_200cm",
#                         "N_Species",
#                         "N_Native",
#                         "Difficult_Ident",
#                         "Dist_NB_Veg",
#                         "Dist_NB_Canopy",
#                         "Dist_IB_Veg",
#                         "N_Tot_Species",
#                         "N_Tot_Native")

##CSV##

generate_figures <- function(csv_file, data_dir, output_dir) {

# Read in CSV file
raw_data <- read.csv(paste0(data_dir, "/", csv_file), stringsAsFactors = FALSE)

# Remove duplicates and non-referenced rows
raw_data <- raw_data[grepl("^[[:digit:]]+", raw_data$REF) , ]

# Remove unused metadata
raw_data <- raw_data[ , -c(3:5, 7:13, 16:17)]

# Reformat and rename columns
colnames(raw_data) <- c("StartDate",
                        "EndDate",
                        "Duration",
                        "Latitude",
                        "Longitude",
                        "Reference",
                        "Percent_0_10cm",
                        "Percent_10_50cm",
                        "Percent_50_200cm",
                        "Percent_200cm",
                        "N_Species",
                        "N_Native",
                        "Difficult_Ident",
                        "Dist_NB_Veg_0_10cm",
                        "Dist_NB_Veg_10_50cm",
                        "Dist_NB_Veg_50_200cm",
                        "Dist_NB_Veg_200cm",
                        "Dist_IB_Veg",
                        "N_Tot_Species",
                        "N_Tot_Native")

# Convert eligible values to numeric
raw_data[ , -c(1:2, 6, 13)] <- apply(raw_data[ , -c(1:2, 6, 13)], 1:2, as.numeric)

# Extract total species and native species values for street
n_tot_species <- mean(raw_data[ , 19], na.rm = TRUE)
n_tot_native <- mean(raw_data[ , 20], na.rm = TRUE)

# Sort rows of data by block reference
block_nums <- ceiling(nrow(raw_data) / 2)
custom_order <- c(paste0(1:block_nums, "a"), paste0(block_nums:1, "b"))
raw_data <- arrange(raw_data, match(Reference, custom_order))

# Subset data to columns with data to analyse
excl_col_names <- c("Start_Date", "End_Date", "Duration", "Latitude",
                    "Longitude", "Difficult_Ident", "N_Tot_Species",
                    "N_Tot_Native")
data <- raw_data[ , colnames(raw_data) %ni% excl_col_names]

# Replace missing values in percentage coverage and species counts data
pct_n_col_names <- c("Percent_0_10cm",
                     "Percent_10_50cm",
                     "Percent_50_200cm",
                     "Percent_200cm",
                     "N_Species",
                     "N_Native")
data[ , pct_n_col_names][is.na(data[, pct_n_col_names])] <- 0

# Replace missing values with longest possible dimension (diagonal from bottom
# corner to opposite top corner) - example for 10m wide x 20m long x 20m tall is:
max_dist <- round(sqrt(sqrt(10^2 + 20^2)^2 + 20^2), 0) # equals approximately 30 meters
dist_col_names <- c("Dist_NB_Veg_0_10cm",
                    "Dist_NB_Veg_10_50cm",
                    "Dist_NB_Veg_50_200cm",
                    "Dist_NB_Veg_200cm",
                    "Dist_IB_Veg")
data[ , dist_col_names][is.na(data[, dist_col_names])] <- max_dist

# Get total number of blocks in survey
n_blocks <- nrow(data)

###### Calculate ######

# Compute score for percentage coverage - a mean of all of the percentage scores
pct_scores <- round(apply(as.matrix(data[ , pct_n_col_names[1:4]]), 1, mean), 0)
pct_score <- round(mean(pct_scores) , 0)

# Compute score for number of species - compares each area to a baseline value
# in number of expected species per unit area and then averages them. The
# baseline value is computed based on a the total survey area and an expectation
# of 500 species per hectare; the calculation is for each 10m x 20m blocks and
# the total survey area:
baseline_sp_blocks <- ((10 * 20) / 10000) * 500 # equals 10
baseline_sp_survey <- ((10 * 20 * n_blocks) / 10000) * 500 # varies based on number of blocks
n_values <- as.matrix(data[ , pct_n_col_names[5]])
n_species_scores <- round((n_values / baseline_sp_blocks) * 100, 0) # divided by baseline value
n_species_scores <- pmin(n_species_scores, 100) # limit scores to 100 
n_species_score <- round(mean(c(n_species_scores, n_tot_species / baseline_sp_survey * 100)), 0)
n_species_score <- pmin(n_species_score, 100) # limit scores to 100

# Compute score for number of native species - the average of the proportions of
# native species in each area
native_values <- as.matrix(data[ , pct_n_col_names[6]])
errors <- which(native_values - n_values > 0)
diffs <- native_values / n_values
diffs[is.nan(diffs)] <- 0
n_native_scores <- round(diffs * 100, 0)
n_native_score <- round(mean(c(n_native_scores, n_tot_native / n_tot_species * 100)), 0)

# Compute score for connectivity - average of each minimum (canopy, intra- and
# inter-site) compared to a baseline value and subtracted from one.
dist_values <- as.matrix(data[ , dist_col_names])
dist_values <- 1 - (dist_values / max_dist)
connectivity_scores <- round(apply(dist_values * 100, 1, mean))
connectivity_scores <- pmin(connectivity_scores, 100) # limit scores to 100 
connectivity_score <- round(mean(connectivity_scores), 0)

# Compute overall score based on mean of four scores
overall_score <- mean(c(pct_score, n_species_score, n_native_score, connectivity_score))
overall_score <- round(overall_score, 0)


###### Visualise ######

# Overall radial scorecard #
score_data <- data.frame(score = c(n_species_score, n_native_score, connectivity_score, pct_score),
                         quadrant = c("N", "E", "S", "W"))

quadrants <- c("N", "E", "S", "W")

r_scorecard_plot <- ggplot() + 
  geom_col(data = score_data,
           aes(x = factor(quadrant, levels = quadrants),
               y = score,
               fill = factor(quadrant, levels = quadrants)),
           position = "identity", width = 1) +
  scale_fill_manual(values = c("#93c287", "#d4c070", "#c79bac", "#939dac")) +
  geom_hline(yintercept = seq(0, 100, 20), colour = "gray80", linewidth = 0.2) +
  geom_segment(mapping = aes(x = seq(0.50, 3.5, 1),
                             xend = seq(0.50, 3.5, 1),
                             y = rep(0, 4),
                             yend = 100),
               colour = "gray80",
               linewidth = 0.2,
               inherit.aes = FALSE) +
  coord_polar(start = 0.785398) +
  theme_minimal() +
  theme(plot.title = element_blank(), 
        plot.subtitle = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_blank(), 
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  geom_text(data = data.frame(xx = 0.5,
                              yy = seq(20, 100, 20),
                              label = c("20", "40", "60", "80", "100")),
            mapping = aes(xx, yy, label = label),
            size = 3,
            color = "gray30",
            inherit.aes = FALSE,
            lineheight = 0.9) + 
  geom_text(data = data.frame(xx = seq(1, 4, 1),
                              yy = rep(50, 4),
                              label = c("DIVERSITY", "NATIVENESS", "CONNECTIVITY", "STRUCTURE")),
            mapping = aes(xx, yy, label = label),
            size = 3,
            inherit.aes = FALSE,
            lineheight = 0.9,
            colour = "gray30") +
  ylim(-25, 100) +
  geom_text(data = data.frame(xx = 0.5,
                              yy = -25,
                              label = overall_score),
            mapping = aes(xx, yy, label = label),
            size = 8,
            inherit.aes = FALSE,
            lineheight = 0.9)# +
  # geom_point(data = data.frame(xx = c(seq(0.5 + 1 / (n_blocks + 1), 1.5 - 1 / (n_blocks + 1), 1 / (n_blocks + 1)),
  #                                     seq(1.5 + 1 / (n_blocks + 1), 2.5 - 1 / (n_blocks + 1), 1 / (n_blocks + 1)),
  #                                     seq(2.5 + 1 / (n_blocks + 1), 3.5 - 1 / (n_blocks + 1), 1 / (n_blocks + 1)),
  #                                     seq(3.5 + 1 / (n_blocks + 1), 4.5 - 1 / (n_blocks + 1), 1 / (n_blocks + 1))),
  #                              yy = 115),
  #            mapping = aes(xx, yy),
  #            shape = 21,
  #            size = 5,
  #            inherit.aes = FALSE,
  #            colour = "gray80") +
  # geom_text(data = data.frame(xx = seq(0.5 + 1 / (n_blocks + 1), 1.5 - 1 / (n_blocks + 1), 1 / (n_blocks + 1)),
  #                             yy = 115,
  #                             label = n_species_scores),
  #           mapping = aes(xx, yy, label = label),
  #           size = 2.5,
  #           inherit.aes = FALSE,
  #           colour = "gray50",
  #           lineheight = 0.9) +
  # geom_text(data = data.frame(xx = seq(1.5 + 1 / (n_blocks + 1), 2.5 - 1 / (n_blocks + 1), 1 / (n_blocks + 1)),
  #                             yy = 115,
  #                             label = n_native_scores),
  #           mapping = aes(xx, yy, label = label),
  #           size = 2.5,
  #           inherit.aes = FALSE,
  #           colour = "gray50",
  #           lineheight = 0.9) +
  # geom_text(data = data.frame(xx = seq(2.5 + 1 / (n_blocks + 1), 3.5 - 1 / (n_blocks + 1), 1 / (n_blocks + 1)),
  #                             yy = 115,
  #                             label = connectivity_scores),
  #           mapping = aes(xx, yy, label = label),
  #           size = 2.5,
  #           inherit.aes = FALSE,
  #           colour = "gray50",
  #           lineheight = 0.9) +
  # geom_text(data = data.frame(xx = seq(3.5 + 1 / (n_blocks + 1), 4.5 - 1 / (n_blocks + 1), 1 / (n_blocks + 1)),
  #                             yy = 115,
  #                             label = pct_scores),
  #           mapping = aes(xx, yy, label = label),
  #           size = 2.5,
  #           inherit.aes = FALSE,
  #           colour = "gray50",
  #           lineheight = 0.9)

# Block scorecard #
score_type <- c("Nativeness", "Connectivity", "Diversity", "Structure")
block_scores <- cbind(n_native_scores, connectivity_scores, n_species_scores, pct_scores)

block_score_data <- cbind(expand.grid(score_type = score_type, block_no = raw_data$Reference), score = NA)
for(i in score_type) {
  block_score_data[score_type == i, "score"] <- block_scores[ , which(score_type == i)]
}

# Plot data with varying circle size and color
b_scorecard_plot <- ggplot(block_score_data,
       aes(x = block_no, 
           y = score_type,
           colour = score,
           size = score)) +
  geom_point() +
  geom_text(aes(label = score), 
            colour = "grey20", 
            size = 3) +
  geom_vline(xintercept = seq(1, n_blocks + 1, 1) - 0.5, colour = "gray80", linewidth = 0.2) +
  scale_size_continuous(range = c(1, 8)) +
  scale_color_gradientn(colours = c("white", colorRampPalette(c('#e3f2ce', '#546e30'))(99))) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())

# Alternative plot with fixed circle size and varying color
# ggplot(block_score_data,
#        aes(x = block_no, 
#            y = score_type,
#            colour = score)) +
#   geom_point(size = 10) +
#   geom_point(size = 10, colour = "black", pch = 21) +
#   geom_text(aes(label = score), 
#             colour = "grey20", 
#             size = 3) +
#   geom_vline(xintercept = seq(1, n_blocks + 1, 1) - 0.5, colour = "gray80", linewidth = 0.2) +
#   scale_color_gradient(low = "white", high = "chartreuse4") +
#   labs(x = NULL, y = NULL) +
#   theme(legend.position = "none",
#         panel.background = element_blank(),
#         panel.grid = element_blank(),
#         axis.ticks = element_blank())

pdf(paste0(output_dir, "/", tools::file_path_sans_ext(csv_file), "_radial_scorecard.pdf"), width = 8, height = 8, pointsize = 12)
print(r_scorecard_plot)
dev.off()

pdf(paste0(output_dir, "/", tools::file_path_sans_ext(csv_file), "_block_scorecard.pdf"), width = 8, height = 2.5, pointsize = 12)
print(b_scorecard_plot)
dev.off()

}