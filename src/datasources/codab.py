import ocha_stratus as stratus
from typing import Literal
from dotenv import load_dotenv

load_dotenv()


def load_codab_from_blob(
    iso3: str, admin_level: int = 0, stage: Literal["dev", "prod"] = "prod"
):
    iso3 = iso3.lower()
    shapefile = f"{iso3}_adm{admin_level}.shp"
    gdf = stratus.load_shp_from_blob(
        container_name="polygon",
        blob_name=f"{iso3.lower()}_shp.zip",
        shapefile=shapefile,
        stage=stage,
    )
    return gdf


def subset_aoi(iso3: str):
    if iso3 == "ETH":
        df_mam_ond = stratus.load_csv_from_blob(
            blob_name="ds-seasonal-bulletin/ETH/misc/mam_ond_zones_fewsnet.csv"
        )

        additional_zones = [
            "ET0508",
            "ET0806",
            "ET0808",
            "ET0411",
            "ET0412",
            "ET0810",
            "ET0511",
            "ET0807",
            "ET0507",
            "ET0421",
            "ET0410",
            "ET0504",
            "ET0502",
            "ET0802",
            "ET0414",
            "ET0503",
            "ET0809",
            "ET0505",
            "ET0509",
            "ET0510",
            "ET0506",
            "ET0812",
            "ET0415",
            "ET0422",
            "ET0408",
            "ET0417",
            "ET1600",
            "ET0811",
        ]
    return list(df_mam_ond.admin2Pcode) + additional_zones
