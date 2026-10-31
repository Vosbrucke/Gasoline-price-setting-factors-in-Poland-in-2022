#' Resolve the project root regardless of the current working directory.
#'
#' This helper is used by the ETL functions when they need to locate the
#' processed data folders from any script location inside the repository.
#'
#' @return Character scalar with the repository root path.
#' @examples
#' resolve_repo_root()
resolve_repo_root <- function() {
  candidates <- c(
    getwd(),
    file.path(getwd(), ".."),
    file.path(dirname(getwd()), "..")
  )

  for (candidate in candidates) {
    if (dir.exists(file.path(candidate, "Data", "Processed"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

#' Read the Bankier weekly Pb95 price series or fall back to the saved dataset.
#'
#' The function tries to use the cached processed files first and only reaches out
#' to the Bankier source when the local data is unavailable.
#'
#' @return A data.table containing the weekly Pb95 price series together with the
#'   year and week identifiers used by the downstream model.
#' @examples
#' \dontrun{read_bankier_pb95_prices()}
read_bankier_pb95_prices <- function() {
  fallback_path <- file.path("Data", "Processed", "full_data.csv")
  if (file.exists(fallback_path)) {
    fallback_dt <- data.table::fread(fallback_path)
    if ("Date" %in% names(fallback_dt) && "price" %in% names(fallback_dt)) {
      return(fallback_dt)
    }
  }

  page <- readLines("https://www.bankier.pl/gospodarka/wskazniki-makroekonomiczne/eu-95-pol")

  from_idx <- which(grepl("// Create the chart", page))[1]
  to_idx <- which(grepl("var intraday_dane", page))[2]

  if (is.na(from_idx) || is.na(to_idx)) {
    stop("Bankier price source no longer exposes the expected chart markers.")
  }

  raw_url <- page[from_idx:to_idx][2]
  raw_vec <- unlist(strsplit(raw_url, ","))
  raw_vec <- raw_vec[seq(1, length(raw_vec), by = 2)]

  price_values <- gsub(".*:", "", raw_vec[seq(1, length(raw_vec), by = 2)])
  date_values <- stringr::str_sub(raw_vec[seq(2, length(raw_vec), by = 2)], 9, 18)

  dt <- data.table(
    price = as.numeric(price_values),
    start = as.Date(date_values) - lubridate::days(7),
    end = as.Date(date_values)
  )

  dt[, `:=`(
    year = lubridate::year(end),
    week = lubridate::isoweek(end),
    price = as.numeric(price)
  )]
  setorder(dt, -start)

  return(dt)
}

#' Read the weekly USD/PLN exchange-rate series.
#'
#' @return A data.table with year and week columns plus the mean USD/PLN quote
#'   for each weekly period.
#' @examples
#' \dontrun{read_usd_pln_weekly()}
read_usd_pln_weekly <- function() {
  fallback_path <- file.path("Data", "Processed", "full_data.csv")
  if (file.exists(fallback_path)) {
    fallback_dt <- data.table::fread(fallback_path)
    if ("usdpln" %in% names(fallback_dt) || "brentPLN" %in% names(fallback_dt)) {
      return(fallback_dt[, .(year, week, usdpln = if ("usdpln" %in% names(fallback_dt)) usdpln else brentPLN / brent)])
    }
  }

  usd_pln <- data.table::fread("https://stooq.com/q/d/l/?s=usdpln&i=d")
  if ("Date" %in% names(usd_pln)) {
    setnames(usd_pln, "Date", "Date")
  }

  if ("Close" %in% names(usd_pln)) {
    setnames(usd_pln, "Close", "Close")
  }

  usd_pln <- usd_pln[, .(Date = as.Date(Date), Close = as.numeric(Close))]
  usd_pln[, `:=`(year = lubridate::year(Date), week = lubridate::isoweek(Date))]
  usd_pln <- usd_pln[, .(usdpln = mean(Close, na.rm = TRUE)), by = .(year, week)]

  return(usd_pln)
}

#' Read the weekly Brent crude oil series used in the model.
#'
#' @return A data.table with Brent prices aggregated by year and ISO week.
#' @examples
#' \dontrun{read_brent_weekly()}
read_brent_weekly <- function() {
  fallback_path <- file.path("Data", "Processed", "full_data.csv")
  if (file.exists(fallback_path)) {
    fallback_dt <- data.table::fread(fallback_path)
    if ("brent" %in% names(fallback_dt) && "Date" %in% names(fallback_dt)) {
      return(fallback_dt[, .(year, week, brent, Date)])
    }
  }

  brent <- as.data.table(gdata::read.xls(
    "http://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls",
    sheet = 2,
    skip = 2
  ))

  setnames(brent, c("Date", "brent"))
  brent[, Date := lubridate::mdy(Date)]
  brent <- brent[Date > as.Date("2004-01-01")]
  brent[, `:=`(year = lubridate::year(Date), week = lubridate::isoweek(Date))]

  brent <- brent[, .(brent = mean(brent, na.rm = TRUE), Date = min(Date)), by = .(year, week)]
  setorder(brent, -Date)

  return(brent)
}

#' Create the core quarterly/weekly model dataset for the Pb95 price analysis.
#'
#' @param pb95_prices Data.table with the retail Pb95 series.
#' @param usd_pln Data.table with USD/PLN exchange rates by week.
#' @param brent Data.table with Brent prices by week.
#' @return A data.table containing the joined fuel-price model inputs.
#' @examples
#' \dontrun{build_model_dataset(pb95_prices, usd_pln, brent)}
build_model_dataset <- function(pb95_prices, usd_pln, brent) {
  model_dt <- pb95_prices[brent, on = .(year, week)]
  model_dt <- model_dt[usd_pln, on = .(year, week)]
  model_dt[, brentPLN := brent * usdpln]
  model_dt[, c("usdpln", "start", "end") := NULL]

  setcolorder(model_dt, c("Date", "price", "year", "week", "brent", "brentPLN"))
  return(model_dt)
}

#' Read wholesale fuel prices from the Lotos archive.
#'
#' @return A data.table containing the wholesale price components used in the
#'   retail-price decomposition model.
#' @examples
#' \dontrun{read_wholesale_fuel_prices()}
read_wholesale_fuel_prices <- function() {
  page <- readLines("https://www.lotos.pl/145/type,oil_95/dla_biznesu/hurtowe_ceny_paliw/archiwum_cen_paliw")

  from_idx <- which(grepl("Opłata paliwowa", page))[1] + 6
  to_idx <- which(grepl("mainContent__footer", page))[1] - 6

  raw_rows <- page[from_idx:to_idx]
  raw_rows <- raw_rows[grepl("td", raw_rows)]
  raw_rows <- gsub("[^0-9,-]", "", raw_rows)
  raw_rows <- gsub(",", ".", raw_rows)

  matrix_data <- sapply(1:4, function(index) raw_rows[seq(index, length(raw_rows), by = 4)])
  wholesale_dt <- as.data.table(matrix_data)
  setnames(wholesale_dt, c("Date", "price_hurt", "excise", "fuel_surcharge"))

  wholesale_dt[, Date := as.Date(Date)]
  cols_to_numeric <- c("price_hurt", "excise", "fuel_surcharge")
  wholesale_dt[, (cols_to_numeric) := lapply(.SD, as.numeric), .SDcols = cols_to_numeric]
  wholesale_dt[, `:=`(
    year = lubridate::year(Date),
    week = lubridate::isoweek(Date)
  )]

  wholesale_dt <- wholesale_dt[, .(
    Date = min(Date),
    price_hurt = mean(price_hurt, na.rm = TRUE),
    excise = mean(excise, na.rm = TRUE),
    fuel_surcharge = mean(fuel_surcharge, na.rm = TRUE)
  ), by = .(year, week)]

  wholesale_dt[, `:=`(
    price_hurt = price_hurt / 1000,
    excise = excise / 1000,
    fuel_surcharge = fuel_surcharge / 1000
  )]
  wholesale_dt[, price_hurt := price_hurt - excise - fuel_surcharge]

  setorder(wholesale_dt, -Date)
  return(wholesale_dt)
}

#' Attach tax and margin components to the estimate of the Pb95 retail price.
#'
#' @param model_dt Data.table containing the fuel series and Brent pricing data.
#' @param wholesale_dt Data.table with wholesale-price components.
#' @return A data.table with the fitted model price and decomposition components.
#' @examples
#' \dontrun{attach_price_factors(model_dt, wholesale_dt)}
attach_price_factors <- function(model_dt, wholesale_dt) {
  model_dt <- copy(model_dt)
  wholesale_dt <- copy(wholesale_dt)
  setnames(wholesale_dt, "Date", "wholesale_date")

  merged_dt <- model_dt[wholesale_dt, on = .(year, week)]
  merged_dt[, Date := as.Date(Date)]

  times_after_vat_cut <- merged_dt[Date > as.Date("2022-02-01"), .N]
  times_after_emission_tax <- merged_dt[Date > as.Date("2019-01-01"), .N]

  vat_share <- c(rep(0.074, times_after_vat_cut), rep(0.187, nrow(merged_dt) - times_after_vat_cut))
  store_margin_share <- rep(0.03, nrow(merged_dt))
  emission_tax <- c(rep(0.1, times_after_emission_tax), rep(0, nrow(merged_dt) - times_after_emission_tax))

  shares_dt <- data.table(
    VAT_share = vat_share,
    store_margin_share = store_margin_share,
    emission_tax = emission_tax
  )
  merged_dt <- cbind(merged_dt, shares_dt)

  merged_dt[, `:=`(
    emission_tax_share = emission_tax / price,
    VAT = price * VAT_share,
    store_margin = store_margin_share * price,
    price_model = price_hurt + excise + fuel_surcharge + VAT + store_margin + emission_tax
  )]

  merged_dt[, c("wholesale_date") := NULL]
  return(merged_dt)
}

#' Read the Eurostat Oil Bulletin fuel-price history file.
#'
#' @return A data.table with country codes and Pb95 price levels for the EU.
#' @examples
#' \dontrun{read_oil_bulletin_fuel_prices()}
read_oil_bulletin_fuel_prices <- function() {
  oil_bulletin <- as.data.table(openxlsx::read.xlsx(
    "https://ec.europa.eu/energy/observatory/reports/Oil_Bulletin_Prices_History.xlsx",
    sheet = "Prices with taxes, per CTR"
  ))

  setnames(oil_bulletin, letters[1:min(8, ncol(oil_bulletin))])
  if (ncol(oil_bulletin) < 8) {
    stop("Oil bulletin sheet does not contain the expected columns.")
  }

  oil_price_values <- oil_bulletin[grepl("[0-9]+$", c), c]
  oil_price_values <- gsub(",", "", oil_price_values)
  date_values <- oil_bulletin[grepl("[0-9]+$", a), a]
  date_values <- as.Date(as.numeric(date_values), origin = "1899-12-30")
  date_values <- date_values[!is.na(date_values)]

  country_codes <- oil_bulletin[grepl("[A-Z]", a), a]
  country_codes <- country_codes[stringr::str_length(country_codes) == 2]

  counts <- as.data.table(table(date_values))
  setnames(counts, c("date", "count"))
  starting_date <- counts[count == 27, date][1]

  fuel_price_dt <- data.table(
    date = date_values,
    Euro_super_95 = as.numeric(oil_price_values)
  )
  fuel_price_dt <- fuel_price_dt[date >= as.Date(starting_date)]

  countries <- rep(country_codes, length.out = nrow(fuel_price_dt))
  countries <- sort(countries)
  fuel_price_dt[, code := countries]
  fuel_price_dt[, Euro_super_95 := round(Euro_super_95) / 1000]
  fuel_price_dt[code == "GR", code := "EL"]

  setcolorder(fuel_price_dt, c("code", "date", "Euro_super_95"))
  return(fuel_price_dt)
}

#' Read the standardized country reference table used for EU map and inflation plots.
#'
#' @return A data.table with country names and ISO-2 codes used across the
#'   project visualizations.
#' @examples
#' \dontrun{read_country_reference()}
read_country_reference <- function() {
  countries_dt <- data.table::fread(
    file.path("Data", "Processed", "slim-2_pl.csv"),
    sep = ";",
    na.strings = "blank"
  )

  countries_dt[, country_name := name]
  countries_dt[, country_name_pl := name_pl]
  countries_dt[, code := `alpha.2`]
  countries_dt[, c("country.code", "name", "name_pl", "alpha.2") := NULL]
  countries_dt[code == "GR", code := "EL"]

  reference_codes <- c(
    "AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR",
    "HR", "HU", "IE", "IS", "IT", "LT", "LU", "LV", "MK", "MT", "NL", "NO", "PL",
    "PT", "RO", "RS", "SE", "SI", "SK", "TR"
  )

  countries_dt <- countries_dt[code %in% reference_codes]
  additional <- data.table(
    code = c("EA19", "EA", "EU"),
    country_name = c("Euro area (19 countries)", "Euro area", "European Union"),
    country_name_pl = c("Kraje strefy euro (19 krajów)", "Kraje strefy euro", "Unia Europejska")
  )

  countries_dt <- rbindlist(list(countries_dt, additional), use.names = TRUE, fill = TRUE)
  setorder(countries_dt, code)
  return(countries_dt)
}

#' Read the special aggregates table used in the inflation analysis.
#'
#' @return A data.table with the Eurostat aggregate labels used in the HICP
#'   charts.
#' @examples
#' \dontrun{read_special_aggregates()}
read_special_aggregates <- function() {
  special_html <- rvest::read_html(
    "https://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=LST_NOM_DTL&StrNom=HICP_2000&StrLanguageCode=EN&IntPcKey=37598921&StrLayoutCode=HIERARCHIC"
  )

  text_values <- rvest::html_nodes(special_html, ".text") %>% rvest::html_text2()
  special_upper <- text_values[grepl("[[:upper:]]+$", text_values)]
  special_upper <- special_upper[-c(1:3)]

  special_lower <- text_values[grepl("[[:lower:]]", text_values)]
  special_lower <- special_lower[6:42]
  special_lower <- stringr::str_remove(special_lower, "\n")

  special_dt <- data.table(
    special_aggregates_u = special_upper,
    special_aggregates_l = special_lower
  )
  return(special_dt)
}

#' Write the processed project datasets to the processed-data folder.
#'
#' @param model_dt Data.table with the cleaned full dataset.
#' @param fuel_price_eu Data.table with EU fuel-price data.
#' @param countries_eu Data.table with country metadata.
#' @param special_aggregates Data.table with inflation aggregate labels.
#' @param repo_root Character path to the repository root.
#' @return Invisibly returns the output directory path.
#' @examples
#' \dontrun{write_processed_outputs(model_dt, fuel_price_eu, countries_eu, special_aggregates, repo_root)}
write_processed_outputs <- function(model_dt, fuel_price_eu, countries_eu, special_aggregates, repo_root) {
  out_dir <- file.path("Data", "Processed")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  data.table::fwrite(model_dt, file.path(out_dir, "full_data.csv"))
  data.table::fwrite(fuel_price_eu, file.path(out_dir, "fuel_price_EU.csv"))
  data.table::fwrite(countries_eu, file.path(out_dir, "countries_eu_inflation.csv"))
  data.table::fwrite(special_aggregates, file.path(out_dir, "special_aggregates.csv"))
  invisible(out_dir)
}

#' Run the full processing pipeline for the project data.
#'
#' The pipeline lazily loads each processed file if it already exists and only
#' recalculates the missing outputs when necessary.
#'
#' @return Invisibly returns a list with the processed datasets.
#' @examples
#' \dontrun{process_data()}
process_data <- function() {
  processed_dir <- file.path("Data", "Processed")

  full_data_path <- file.path(processed_dir, "full_data.csv")
  if (file.exists(full_data_path)) {
    model_data <- data.table::fread(full_data_path)
  } else {
    pb95_prices <- read_bankier_pb95_prices()
    usd_pln <- read_usd_pln_weekly()
    brent <- read_brent_weekly()

    model_data <- build_model_dataset(pb95_prices, usd_pln, brent)
    wholesale_fuel_prices <- read_wholesale_fuel_prices()
    model_data <- attach_price_factors(model_data, wholesale_fuel_prices)
    model_data <- model_data[, order(colnames(model_data)), with = FALSE]
    data.table::fwrite(model_data, full_data_path)
  }

  fuel_price_path <- file.path(processed_dir, "fuel_price_EU.csv")
  if (file.exists(fuel_price_path)) {
    fuel_price_eu <- data.table::fread(fuel_price_path)
  } else {
    fuel_price_eu <- read_oil_bulletin_fuel_prices()
    data.table::fwrite(fuel_price_eu, fuel_price_path)
  }

  countries_path <- file.path(processed_dir, "countries_eu_inflation.csv")
  if (file.exists(countries_path)) {
    countries_eu <- data.table::fread(countries_path)
  } else {
    countries_eu <- read_country_reference()
    data.table::fwrite(countries_eu, countries_path)
  }

  aggregates_path <- file.path(processed_dir, "special_aggregates.csv")
  if (file.exists(aggregates_path)) {
    special_aggregates <- data.table::fread(aggregates_path)
  } else {
    special_aggregates <- read_special_aggregates()
    data.table::fwrite(special_aggregates, aggregates_path)
  }

  invisible(list(
    model_data = model_data,
    fuel_price_eu = fuel_price_eu,
    countries_eu = countries_eu,
    special_aggregates = special_aggregates
  ))
}
