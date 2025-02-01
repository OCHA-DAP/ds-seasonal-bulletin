library(cumulus)
library(httr)
library(dplyr)
library(sf)
library(DBI)
library(RPostgres)
library(glue)
library(readr)
library(readxl)
library(jsonlite)

#' Get list of admin2 PCodes for March-April-May and Oct-Nov-Dec seasonal zones in Ethiopia
#' @return Character vector of admin2 PCodes
subset_adm2_ond_mam <- function() {
  df_mam_ond <- read_csv(
    file.path(
      Sys.getenv("AA_DATA_DIR"),
      "public", "exploration", "eth",
      "mam_ond_zones_fewsnet.csv"
    ),
    show_col_types = FALSE
  )
  ond_zones <- c(
    "ET0508", "ET0806", "ET0808", "ET0411", "ET0412", "ET0810", "ET0511",
    "ET0807", "ET0507", "ET0421", "ET0410", "ET0504", "ET0502", "ET0802",
    "ET0414", "ET0503", "ET0809", "ET0505", "ET0509", "ET0510", "ET0506",
    "ET0812", "ET0415", "ET0422", "ET0408", "ET0417", "ET1600", "ET0811"
  )

  return(c(df_mam_ond$admin2Pcode, ond_zones))
}


#' Load Ethiopian admin2 boundaries and 2024 Food Security PIN data,
#' joining them into a single sf object
#' @return sf object with admin2 geometries and Food Security PIN data
get_eth_gdf_pin <- function(year) {
  # Get the admin bounds
  eth_adm2 <- st_read(
    file.path(
      Sys.getenv("AA_DATA_DIR"),
      "public", "raw", "eth", "cod_ab", "Admin_2024.gdb.zip"
    ),
    layer = "eth_admbnda_adm2_csa_bofedb_2024"
  ) %>%
    filter(!(admin2Pcode %in% list("ET0000", "ET1000")))

  if (year == 2025) {
    fname <- "Food Security PIN and severity 2025.xlsx"
    sheet_name <- "WS - 3.1 PIN"
    cluster_pin_col <- "Food Security Cluster"
  } else if (year == 2024) {
    fname <- "Food Security_PIN_Severity_2024.xlsx"
    sheet_name <- "Cluster PiN"
    cluster_pin_col <- "Cluster PiN"
  }

  # Get the PiN data and join with the geodataframe
  df_pin_fs <- read_excel(
    path = file.path(Sys.getenv("AA_DATA_DIR"), "public", "exploration", "eth", "pin", fname),
    skip = 1,
    col_names = TRUE,
    sheet = sheet_name
  ) %>%
    group_by(`Admin 2 P-Code`) %>%
    summarise(total_pin = sum(!!sym(cluster_pin_col), na.rm = TRUE)) %>%
    select("Admin 2 P-Code", "total_pin")

  gdf_adm2 <- eth_adm2 %>%
    full_join(df_pin_fs, by = c("admin2Pcode" = "Admin 2 P-Code")) %>%
    rename(TotalPop = total_pin)

  return(gdf_adm2)
}


#' Query historical rainfall data for specified administrative zones
#' @param table Database table name
#' @param iso3 Three-letter country code
#' @param adm_level Administrative level
#' @param sel_zones Vector of zone PCodes to filter
#' @return Dataframe of historical rainfall data
get_historical_rainfall <- function(table, iso3, adm_level, sel_zones = NULL, stage = "prod") {
  # Get the historical rainfall data from the database to selected zones
  conn <- pg_con(stage = stage, write = FALSE)
  query <- glue("SELECT * from {table} WHERE iso3='{iso3}' AND adm_level={adm_level}")
  df_precip <- dbGetQuery(conn, query)
  if (!is.null(sel_zones)) {
    df_precip <- df_precip %>%
      filter(pcode %in% sel_zones)
  }
  return(df_precip)
}

#' Query HAPI to get population data per iso3 code and admin level
#' @param iso3 Three-letter country code
#' @param adm_level Administrative level
#' @return Dataframe of population
get_pop <- function(iso3, adm_level) {
  col <- glue("admin{adm_level}_code")
  url <- paste0(
    glue("https://hapi.humdata.org/api/v1/population-social/population?location_code={iso3}&admin_level={adm_level}&output_format=json&app_identifier="),
    Sys.getenv("HDX_APP_IDENTIFIER"),
    "&limit=10000&offset=0"
  )

  response <- GET(url)

  if (status_code(response) == 200) {
    json_data <- content(response, "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(json_data)
    df <- as.data.frame(data$data)
  } else {
    print(paste("Error: ", status_code(response)))
  }

  df_pop <- df %>%
    group_by(!!sym(col)) %>%
    summarise(
      TotalPop = sum(population, na.rm = TRUE),
    ) %>%
    ungroup()

  return(df_pop)
}

#' Load COD boundaries and population data for a given ISO3 and admin level,
#' joining them into a single sf object
#' @return sf object
get_som_gdf_pop <- function(adm_level) {
  gdf_som <- st_read(
    file.path(
      Sys.getenv("AA_DATA_DIR"),
      "public", "raw", "som", "cod_ab", "som_adm.shp.zip"
    ),
    layer = glue("som_admbnda_adm{adm_level}_ocha_20230308")
  )

  col1 <- glue("ADM{adm_level}_PCODE")
  col2 <- glue("admin{adm_level}_code")
  df_pop <- get_pop("SOM", adm_level)

  gdf_joined <- gdf_som %>%
    full_join(df_pop, by = setNames(col2, col1))

  return(gdf_joined)
}
