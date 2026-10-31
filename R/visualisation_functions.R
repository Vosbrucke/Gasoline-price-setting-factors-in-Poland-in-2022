#' Read the processed price dataset used by the plotting functions.
#'
#' @param file_name Character scalar with the location of the processed CSV file.
#' @return A data.table containing the project price series.
#' @examples
#' read_plot_data()
read_plot_data <- function(file_name = "Data/Processed/full_data.csv") {
  if (!file.exists(file_name)) {
    file_name <- file.path("Data", "Processed", "full_data.csv")
  }

  data <- data.table::fread(file_name)
  return(data)
}

#' Plot the modelled Pb95 price against the observed retail and wholesale series.
#'
#' The model price is the sum of all identified price-forming factors. This chart
#' makes it easy to compare the long-run relationship between the model and the
#' actual market price.
#'
#' @param data A data.table containing the relevant data.
#' @param start_date Date filter used for the plotted period.
#' @param save_path Output location for the saved PNG chart.
#' @return A ggplot object.
#' @examples
#' \dontrun{plot_model_price_comparison()}
plot_model_price_comparison <- function(data, start_date = "2020-01-01", save_path = "Plots/Model vs actual price.png") {

  data <- as.data.frame(data)
  data$Date <- as.Date(data$Date)

  data <- data %>%
    dplyr::filter(Date > as.Date(start_date))

  palette <- wesanderson::wes_palette("Zissou1", 7, type = "continuous")

  # The differences result from the assumptions of the model in the form of a fixed margin of
  # the station and the market realities - adjustment to fluctuations does not take place immediately.
  # The model price was calculated by summing up all the price factors for the Pb95 fuel.
  plot_obj <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = Date)) +
    ggplot2::geom_line(ggplot2::aes(y = price_model, color = "Model price"), linewidth = 0.6, linetype = 5) +
    ggplot2::geom_line(ggplot2::aes(y = price, color = "Retail price"), linewidth = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = price_hurt, color = "Wholesale price"), linewidth = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = excise, color = "Excise"), linewidth = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = VAT, color = "VAT"), linewidth = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = store_margin, color = "Station's overhead"), linewidth = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = fuel_surcharge, color = "Fuel fee"), linewidth = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = emission_tax, color = "Emission tax"), linewidth = 0.6) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 9), breaks = seq(0, 8, 2)) +
    ggplot2::scale_x_date(
      breaks = seq.Date(from = floor_date(min(data$Date), "month"), to = ceiling_date(max(data$Date), "month"), by = "6 months"),
      date_labels = "%b %y",
      minor_breaks = seq.Date(from = floor_date(min(data$Date), "month"), to = ceiling_date(max(data$Date), "month"), by = "month"),
      guide = ggplot2::guide_axis(minor.ticks = TRUE),
      limits = c(floor_date(min(data$Date), "month"), ceiling_date(max(data$Date), "month")),
      expand = c(0, 0)
    ) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c(
        "Model price" = "#666666",
        "Retail price" = palette[1],
        "Wholesale price" = palette[2],
        "Excise" = palette[3],
        "VAT" = palette[4],
        "Station's overhead" = palette[5],
        "Fuel fee" = palette[6],
        "Emission tax" = palette[7]
      )
    ) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Prices of individual factors included in the retail price of Pb95",
      subtitle = "The model price is the sum of the prices of the price-forming factors for Pb95",
      caption = "Source: Bankier, Lotos"
    ) +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.title = ggtext::element_markdown(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 8),
      plot.caption = ggplot2::element_text(hjust = 0, size = 4),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, color = "lightgrey"),
      axis.text.x = ggplot2::element_text(color = "black", angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = ggplot2::element_text(color = "black"),
      axis.ticks = ggplot2::element_line(color = "black"),
      text = ggplot2::element_text(color = "black")
    )

  ggplot2::ggsave(save_path, plot_obj, dpi = 900, width = 20, height = 10, units = "cm")
  return(plot_obj)
}

#' Compare the movement of the wholesale price index with Brent denominated in PLN.
#'
#' @param data A data.table containing the relevant data.
#' @param start_date The start date for the indexed comparison.
#' @param save_path Output location for the saved PNG chart.
#' @return A ggplot object.
#' @examples
#' \dontrun{plot_wholesale_vs_brent_index()}
plot_wholesale_vs_brent_index <- function(data, start_date = "2022-01-01", save_path = "Plots/Wholesale vs brent price.png") {

  data <- as.data.frame(data)
  data$Date <- as.Date(data$Date)
  data_growth <- data %>%
    dplyr::arrange(Date) %>%
    dplyr::filter(Date > as.Date(start_date)) %>%
    dplyr::mutate(
      last_price_hurt = 100 * price_hurt / dplyr::first(price_hurt),
      last_brentPLN = 100 * brentPLN / dplyr::first(brentPLN)
    )

  data_growth_last <- data_growth %>%
    dplyr::select(Date, last_price_hurt, last_brentPLN) %>%
    dplyr::filter(Date == max(Date))

  palette <- wesanderson::wes_palette("Zissou1", 2, type = "continuous")

  # This comparison highlights the divergence between the wholesale price and the Brent oil index
  # expressed in PLN since the start of 2022.
  last_line <- dplyr::bind_rows(data_growth_last, data_growth_last %>% dplyr::mutate(Date = Date + lubridate::days(15)))

  plot_obj <- data_growth %>%
    ggplot2::ggplot(ggplot2::aes(x = Date)) +
    ggplot2::geom_line(data = last_line, ggplot2::aes(y = last_price_hurt, color = "Wholesale Price"), linewidth = 0.3) +
    ggplot2::geom_text(data = last_line, ggplot2::aes(x = Date[1] + lubridate::days(18), y = last_price_hurt, label = "Wholesale Price", color = "Wholesale Price"), size = 3, hjust = 0) +
    ggplot2::geom_line(data = last_line, ggplot2::aes(y = last_brentPLN, color = "Brent Crude Oil in PLN"), linewidth = 0.3) +
    ggplot2::geom_text(data = last_line, ggplot2::aes(x = Date[1] + lubridate::days(18), y = last_brentPLN, label = "Brent Crude Oil in PLN", color = "Brent Crude Oil in PLN"), size = 3, hjust = 0) +
    ggplot2::geom_line(ggplot2::aes(y = last_price_hurt, color = "Wholesale Price"), linewidth = 0.75) +
    ggplot2::geom_line(ggplot2::aes(y = last_brentPLN, color = "Brent Crude Oil in PLN"), linewidth = 0.75) +
    ggplot2::geom_point(data = data_growth_last, ggplot2::aes(y = last_price_hurt, color = "Wholesale Price"), size = 1, shape = 21, fill = "white", stroke = 1) +
    ggplot2::geom_point(data = data_growth_last, ggplot2::aes(y = last_brentPLN, color = "Brent Crude Oil in PLN"), size = 1, shape = 21, fill = "white", stroke = 1) +
    ggplot2::scale_x_date(
      breaks = seq.Date(from = floor_date(min(data_growth$Date), "month"), to = ceiling_date(max(data_growth$Date), "month"), by = "3 months"),
      date_labels = "%b %y",
      minor_breaks = seq.Date(from = floor_date(min(data_growth$Date), "month"), to = ceiling_date(max(data_growth$Date) + months(2), "month"), by = "month"),
      guide = ggplot2::guide_axis(minor.ticks = TRUE),
      limits = c(floor_date(min(data_growth$Date), "month"), ceiling_date(max(data_growth$Date) + months(2), "month")),
      expand = c(0, 0)
    ) +
    ggplot2::scale_color_manual(name = NULL, values = c("Wholesale Price" = palette[1], "Brent Crude Oil in PLN" = "#E4B80E")) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = 'The wholesale price has "broken away" from Brent Oil price (expressed in PLN)',
      subtitle = "100 = price level on 01/01/2022",
      caption = "Source: EIA, Lotos, Stooq"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.title = ggtext::element_markdown(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 8),
      plot.caption = ggplot2::element_text(hjust = 0, size = 4),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, color = "lightgrey"),
      axis.text = ggplot2::element_text(color = "black"),
      axis.ticks = ggplot2::element_line(color = "black"),
      text = ggplot2::element_text(color = "black"),
      axis.text.x = ggplot2::element_text(angle = 0),
      legend.position = "none"
    )

  ggplot2::ggsave(save_path, plot_obj, dpi = 900, width = 20, height = 10, units = "cm")
  return(plot_obj)
}

#' Plot the share of each component in the total Pb95 retail price.
#'
#' @param data A data.table containing the relevant data.
#' @param start_date The start date for the chart.
#' @param save_path Output location for the saved PNG chart.
#' @return A ggplot object.
#' @examples
#' \dontrun{plot_price_factor_shares()}
plot_price_factor_shares <- function(data, start_date = "2020-01-01", save_path = "Plots/Price-generating factors Pb95.png") {

  data <- as.data.frame(data)
  data$Date <- as.Date(data$Date)

  # Data preparation
  share_data <- data %>%
    dplyr::select(Date, price_hurt, excise, VAT, store_margin, emission_tax, fuel_surcharge) %>%
    tidyr::pivot_longer(cols = -Date, names_to = "name", values_to = "price")

  share_data$name <- factor(share_data$name,
    levels = c("price_hurt", "excise", "VAT", "store_margin", "fuel_surcharge", "emission_tax"),
    labels = c("wholesale price", "excise", "VAT", "gas station's overhead", "fuel fee", "emission tax")
  )

  palette <- wesanderson::wes_palette("Zissou1", 6, type = "continuous")
  share_data <- share_data %>%
    dplyr::filter(Date > as.Date(start_date)) %>%
    dplyr::mutate(Date = floor_date(Date, unit = "month")) %>%
    dplyr::group_by(Date, name) %>%
    dplyr::summarise(price = sum(price, na.rm = TRUE), .groups = "drop") %>%
    tidyr::drop_na(price)

  plot_obj <- share_data %>%
    ggplot2::ggplot(ggplot2::aes(fill = name, x = Date, y = price)) +
    ggplot2::geom_bar(position = "fill", stat = "identity", width = 20) +
    ggplot2::geom_vline(xintercept = as.Date("2022-02-03"), color = "black") +
    ggplot2::geom_curve(
      data = data.frame(x = as.Date("2021-10-01"), y = 0.747391474149104, xend = as.Date("2022-01-09"), yend = 0.583470506718382),
      mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      curvature = 0.49,
      arrow = arrow(20L, unit(0.1, "inches"), "last", "closed"),
      inherit.aes = FALSE,
      color = "black"
    ) +
    ggplot2::geom_text(
      data = data.frame(x = as.Date("2021-09-28"), y = 0.818102479707454, label = "VAT reduction on fuel\nfrom 23% to 8%"),
      mapping = ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      color = "black"
    ) +
    ggplot2::scale_fill_manual(values = palette, labels = levels(share_data$name), name = NULL) +
    ggplot2::scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b %y",
      limits = c(floor_date(min(share_data$Date), "month") - months(1), ceiling_date(max(share_data$Date), "month") + months(1)),
      expand = c(0, 0)
    ) +
    ggplot2::labs(y = NULL, x = NULL, title = "Percentage share of individual price drivers for Pb95 fuel in Poland from 2020", caption = "Source: Bankier, e-Petrol, Lotos") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.title = ggtext::element_markdown(face = "bold", size = 9, hjust = 0.1),
      plot.caption = ggplot2::element_text(hjust = 0, size = 4),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(color = "black", angle = 90),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      text = ggplot2::element_text(color = "black")
    )

  ggplot2::ggsave(save_path, plot_obj, dpi = 900, width = 20, height = 10, units = "cm")
  return(plot_obj)
}

#' Plot a long-run view of Brent prices in PLN with a boxplot summary below.
#'
#' @param data A data.table containing the relevant data.
#' @param save_path Output location for the saved PNG chart.
#' @return A patchwork object combining the line and boxplot panels.
#' @examples
#' \dontrun{plot_brent_in_pln()}
plot_brent_in_pln <- function(data, save_path = "Plots/Brent in PLN.png") {

  data <- as.data.frame(data)
  data$Date <- as.Date(data$Date)

  # Line plot
  p1 <- data %>%
    dplyr::filter(!year == 2004) %>%
    ggplot2::ggplot(ggplot2::aes(x = Date, y = brentPLN)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::ggtitle("Brent Crude Oil in PLN 2005-2022") +
    ggplot2::scale_y_continuous(breaks = seq(0, 600, 150), limits = c(0, 650), expand = c(0, 0)) +
    ggplot2::scale_x_date(date_labels = "%Y", minor_breaks = seq.Date(as.Date("2005-01-01"), as.Date(now() + years(3)), by = "year"), guide = ggplot2::guide_axis(minor.ticks = TRUE)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.title = ggtext::element_markdown(),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, color = "lightgrey"),
      axis.text = ggplot2::element_text(color = "black"),
      axis.ticks = ggplot2::element_line(color = "black"),
      text = ggplot2::element_text(color = "black")
    )

  # Boxplot
  p2 <- data %>%
    dplyr::filter(!year == 2004) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = brentPLN, group = year)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = NULL, y = NULL, caption = "Source: EIA, stooq") +
    ggplot2::ggtitle("Brent Crude Oil in PLN 2005-2022") +
    ggplot2::scale_y_continuous(breaks = seq(0, 600, 150), limits = c(0, 650), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(minor_breaks = seq(2005, 2025, 1), guide = ggplot2::guide_axis(minor.ticks = TRUE)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.title = ggtext::element_markdown(),
      plot.caption = ggplot2::element_text(hjust = 0, size = 4),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, color = "lightgrey"),
      axis.text = ggplot2::element_text(color = "black"),
      axis.ticks = ggplot2::element_line(color = "black"),
      text = ggplot2::element_text(color = "black")
    )

  plot_obj <- p1 / p2
  ggplot2::ggsave(save_path, plot_obj, dpi = 900, width = 15, height = 20, units = "cm")
  return(plot_obj)
}

#' Create a generic univariate Europe map from a prepared data frame.
#'
#' @param data Optional data.frame with columns geo and values_x.
#' @param title Map title.
#' @param subtitle Subtitle text.
#' @param legend Legend title.
#' @param caption Source caption.
#' @return A ggplot object.
#' @examples
#' \dontrun{generate_univariate_map()}
generate_univariate_map <- function(data = NULL, title = "", subtitle = "", legend = "", caption = "") {
  if (is.null(data)) {
    data <- get_eurostat("prc_hicp_manr", filters = list(coicop = "CP00")) %>%
      dplyr::filter(time == as.Date("2022-08-01") - months(1), geo != "TR") %>%
      dplyr::rename(values_x = values) %>%
      tidyr::drop_na() %>%
      dplyr::select(geo, values_x)
  }

  p <- univariate_map_eu(data = data, title = title, subtitle = subtitle, legend = legend, caption = caption)
  return(p)
}

#' Create a generic bivariate Europe map from x and y series.
#'
#' @param data_x Data.frame with country code and values_x.
#' @param data_y Data.frame with country code and values_y.
#' @param title Map title.
#' @param legend_x Legend for the x-axis dimension.
#' @param legend_y Legend for the y-axis dimension.
#' @param caption Source caption.
#' @return A ggplot object.
#' @examples
#' \dontrun{generate_bivariate_map()}
generate_bivariate_map <- function(data_x = NULL, data_y = NULL, title = "", legend_x = "lower → higher\ninflation", legend_y = "lower → higher", caption = "") {
  if (is.null(data_x) || is.null(data_y)) {
    data_y <- get_eurostat("prc_hicp_manr", filters = list(coicop = "TOT_X_NRG_FOOD")) %>%
      dplyr::filter(time == as.Date("2022-09-01") - months(1)) %>%
      dplyr::filter(!geo %in% c("EA18", "EA19", "EU27_2020", "EU28", "EEA")) %>%
      tidyr::drop_na() %>%
      dplyr::rename(values_y = values) %>%
      dplyr::select(geo, values_y)

    data_x <- get_eurostat("prc_hicp_manr", filters = list(coicop = "CP00")) %>%
      dplyr::filter(time == as.Date("2022-09-01") - months(1)) %>%
      dplyr::filter(!geo %in% c("EA18", "EA19", "EU27_2020", "EU28", "EEA")) %>%
      tidyr::drop_na() %>%
      dplyr::rename(values_x = values) %>%
      dplyr::select(geo, values_x)
  }

  p <- bivariate_map_eu(data_x = data_x, data_y = data_y, title = title, legend_x = legend_x, legend_y = legend_y, caption = caption)
  return(p)
}

#' Generate the map-based visualisations used in the project.
#'
#' This includes the main univariate European fuel and inflation maps.
#'
#' @return Invisibly returns TRUE when the plots are created.
#' @examples
#' \dontrun{generate_map_visualisations()}
generate_map_visualisations <- function() {
  fuel_price_eu <- read.csv("Data/Processed/fuel_price_EU.csv")

  fuel_data <- fuel_price_eu %>%
    dplyr::group_by(code) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::rename(values_x = Euro_super_95, geo = code)

  cat("Generating fuel price map for Europe...\n")
  univariate_map_eu(
    data = fuel_data[, c("geo", "values_x")],
    title = "Petrol prices in Europe",
    subtitle = paste("Average weekly price of petrol Pb95 on", format(as.Date(unique(fuel_data$date)), "%d %B %Y")),
    legend = "Average price in €",
    caption = "Source: Eurostat"
  )

  inflation_data <- get_eurostat("prc_hicp_manr", filters = list(coicop = "CP00")) %>%
    dplyr::filter(time == as.Date("2022-08-01") - months(1), geo != "TR") %>%
    dplyr::rename(values_x = values) %>%
    tidyr::drop_na() %>%
    dplyr::select(geo, values_x)

  cat("Generating inflation map for Europe...\n")
  univariate_map_eu(
    data = inflation_data,
    title = "HICP inflation in Europe (August 2022)",
    subtitle = "",
    legend = "Inflation in %",
    caption = "Source: Eurostat"
  )

  invisible(TRUE)
}
