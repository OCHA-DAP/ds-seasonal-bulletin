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
#' Returns NULL for all other cases (so far we don't subset)
#' @return Character vector of admin2 PCodes
subset_pcodes <- function(params) {

  if (params$iso3 == "ETH" && params$adm_level == 2 && 
      (params$season == "MAM" || params$season == "OND")) {
    

    df_mam_ond <- cumulus::blob_read(
      name = "ds-seasonal-bulletin/ETH/misc/mam_ond_zones_fewsnet.csv",
      stage = "dev",
      container = "projects"
    )
    ond_zones <- c(
      "ET0508", "ET0806", "ET0808", "ET0411", "ET0412", "ET0810", "ET0511",
      "ET0807", "ET0507", "ET0421", "ET0410", "ET0504", "ET0502", "ET0802",
      "ET0414", "ET0503", "ET0809", "ET0505", "ET0509", "ET0510", "ET0506",
      "ET0812", "ET0415", "ET0422", "ET0408", "ET0417", "ET1600", "ET0811"
    )
    return(c(df_mam_ond$admin2Pcode, ond_zones))
  } else {
    return(NULL)
  }
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
get_gdf_pop <- function(iso3, adm_level) {
  # TODO: Should this really be returning a list?
  gdf <- cumulus::download_fieldmaps_sf(
    iso3,
    glue("{iso3}_adm{adm_level}")
  )[[1]]

  col1 <- glue("ADM{adm_level}_PCODE")
  col2 <- glue("admin{adm_level}_code")
  
  df_pop <- get_pop(iso3, adm_level)
  gdf_joined <- gdf %>%
    full_join(df_pop, by = setNames(col2, col1))

  return(gdf_joined)
}