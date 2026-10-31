# Analysis script for gasoline price setting factors in Poland in 2022
source("R/load_required_libraries.R")
source("R/visualisation_functions.R")
source("R/process_data_functions.R")
source("R/inflation_chart.R")

# the data is processed and read from the processed outputs
# process_data()

# hence we can just read it
data <- read_plot_data()

plot_model_price_comparison(data)
plot_wholesale_vs_brent_index(data)
plot_price_factor_shares(data)
plot_brent_in_pln(data)

# generate map visualisations based on the processed data
generate_map_visualisations()

inflation_chart(language_output = "en")
inflation_chart(region = c("Austria", "Malta", "Germany", "Poland", "Hungary"), language_output = "en")
