import requests
import os
import pandas as pd


def get_pop(iso3, adm_level):
    endpoint = (
        "https://hapi.humdata.org/api/v2/geography-infrastructure/baseline-population"
    )
    params = {
        "app_identifier": os.getenv("HAPI_APP_IDENTIFIER"),
        "admin_level": adm_level,
        "output_format": "json",
        "limit": 10000,
        "offset": 0,
        "gender": "all",
    }
    if iso3:
        params["location_code"] = iso3
    # Check if the request was successful
    response = requests.get(endpoint, params=params)
    json_data = response.json()
    # Extract the data list from the JSON
    data_list = json_data.get("data", [])
    df_response = pd.DataFrame(data_list)

    if df_response.empty:
        raise Exception(f"No data available for {iso3}")

    df_response = df_response[df_response.age_range == "all"]

    return df_response
