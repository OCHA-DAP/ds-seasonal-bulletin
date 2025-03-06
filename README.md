# ds-seasonal-bulletin

This repository contains code to create plots for seasonal bulletins of drought risk. 

## Usage 

1. Define the following environment variables in your `.Renviron` file

```
DS_AZ_BLOB_PROD_SAS_WRITE=<provided-on-request>
DS_AZ_BLOB_DEV_SAS=<provided-on-request>
DS_AZ_BLOB_DEV_SAS_WRITE=<provided-on-request>

DS_AZ_DB_PROD_PW=<provided-on-request>
DS_AZ_DB_PROD_UID=<provided-on-request>
DS_AZ_DB_DEV_HOST=<provided-on-request>
DS_AZ_DB_PROD_HOST=<provided-on-request>

HDX_APP_IDENTIFIER=<provided-on-request>
```

2. Run `render.R` to output the appropriate `.html` reports with summary content for each bulletin. 

> Note: These plots and summary statistics are manually copied into a Figma file to create the final bulletin.