source("scoring_engine.R")

data_files <- list.files("data")
for(i in data_files) {
  generate_figures(csv_file = i,
                   data_dir = "data",
                   output_dir = "figs")
}
