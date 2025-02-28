# Generate reports for multiple countries
# Define country-specific configurations
country_configs <- list(
    SOM = list(
        iso3 = "SOM",
        adm_level = 1
    ),
    ETH = list(
        iso3 = "ETH",
        adm_level = 2
    )
)

for (country_name in names(country_configs)) {
    country <- country_configs[[country_name]]
    
    params_list <- list(
        iso3 = country$iso3,
        adm_level = country$adm_level,
        dataset = "seas5",
        season = "MAM",
        issued_month = 2,
        year = 2025
    )

    output_filename <- paste0(
        params_list$iso3, "_", 
        params_list$season, "_", 
        params_list$year, 
        "_i",
        Sys.Date(),
        "_outlook.html"
    )

    rmarkdown::render(
        input = "analysis/seasonal_bulletin.Rmd",
        output_format = "html_document",
        output_file = output_filename,
        output_dir = "reports/",
        params = params_list
    )

    cat("Generated report for", country$iso3, "\n")
}