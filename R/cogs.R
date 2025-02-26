library(terra)
library(dplyr)

cog_filename <- function(iso3, type, dataset, season, issued_month){
    return(paste0(iso3, "_", type, "_", dataset, "_", season, "_issued", issued_month, ".tif"))
}

cog_exists <- function(container, azure_filepath) {
    files <- AzureStor::list_storage_files(container, prefix = azure_filepath)
    return(azure_filepath %in% files$name)
}

upload_cog_to_azure <- function(r, container, src_cog_filename, dest_cog_filename, clobber = TRUE) {
    terra::writeRaster(r,
        filename = src_cog_filename,
        filetype = "COG",
        overwrite = clobber,
        gdal = c("COMPRESS=DEFLATE",
            "SPARSE_OK=YES",
            "OVERVIEW_RESAMPLING=AVERAGE")
    )
    
    AzureStor::storage_upload(
        container,
        src = src_cog_filename,
        dest = dest_cog_filename
    )
}

load_cog_from_azure <- function(container, src_cog_filename, dest_cog_filename, clobber = TRUE) {
    AzureStor::storage_download(
        container,
        src = src_cog_filename,
        dest = dest_cog_filename,
        overwrite = clobber
    )
    return(terra::rast(dest_cog_filename))
}

get_cogs_to_process <- function(container, dataset, months_to_include, issued_month) {
    
    # Get dataframe of all COGs available for that dataset
    dir_name <- paste0(dataset, "/monthly/processed")
    cog_df <- AzureStor::list_blobs(
        container = container,
        dir = dir_name
    )
    
    # Now filter down to only those we're interested in
    if (dataset == "seas5") {
        # Given the `issued_month` and the valid months we want to include, 
        # work backwards to calculate the leadtimes 
        lead_times <- (months_to_include + 12 * (months_to_include < issued_month)) - issued_month
        
        cog_df <- cog_df %>%
            dplyr::mutate(
                year = str_extract(name, "i(\\d{4})") %>% str_remove("i") %>% as.integer(), 
                month = str_extract(name, "-(\\d{2})-") %>% str_remove_all("-") %>% as.integer(),
                lt = str_extract(name, "lt\\d+") %>% str_remove("lt") %>% as.integer()
            ) %>%
            dplyr::filter(
                month == issued_month,
                lt %in% lead_times
            )
    
    } else if (dataset == "era5") {
    cog_df <- cog_df %>%
        dplyr::mutate(
            year = str_extract(name, "v(\\d{4})") %>% str_remove("v") %>% as.integer(),
            month = str_extract(name, "-(\\d{2})-") %>% str_remove_all("-") %>% as.integer()
        ) %>%
        dplyr::filter(month %in% months_to_include)
    }

    return(cog_df)
}