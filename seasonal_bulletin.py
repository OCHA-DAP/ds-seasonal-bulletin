import marimo

__generated_with = "0.15.2"
app = marimo.App(width="medium")


@app.cell
def _():
    import marimo as mo
    return (mo,)


@app.cell
def inputs(calendar, mo):
    iso3_dropdown = mo.ui.dropdown(
        options={"Ethiopia": "ETH", "Somalia": "SOM"},
        value="Ethiopia",
        label="Analysis location",
    )
    adm_level_dropdown = mo.ui.dropdown(
        options=[0, 1, 2], value=2, label="Admin level"
    )
    season_dropdown = mo.ui.dropdown(
        options=["MAM", "OND"], value="OND", label="Season"
    )
    season_year_dropdown = mo.ui.dropdown(
        options=[2024, 2025], value=2025, label="Season year"
    )
    issued_month_dropdown = mo.ui.dropdown(
        options={calendar.month_abbr[x]: x for x in range(1, 13)},
        label="Issued month:",
        value="Sep",
    )

    mo.hstack(
        [
            iso3_dropdown,
            adm_level_dropdown,
            season_dropdown,
            season_year_dropdown,
            issued_month_dropdown,
        ],
        justify="center",
    )
    return (
        adm_level_dropdown,
        iso3_dropdown,
        issued_month_dropdown,
        season_dropdown,
        season_year_dropdown,
    )


@app.cell
def _(mo):
    mo.Html("<hr></hr><br>")
    return


@app.cell
def _(SEASON_YEAR, iso3_dropdown, mo, season_dropdown):
    mo.center(
        mo.md(
            f"""# {iso3_dropdown.selected_key}: {SEASON_YEAR} {season_dropdown.value} Season Outlook"""
        )
    )
    return


@app.cell
def _(SEASON_YEAR, issued_month_dropdown, mo):
    mo.center(
        mo.md(
            f"#### ECMWF Seasonal Forecast issued {issued_month_dropdown.selected_key} {SEASON_YEAR}"
        )
    )
    return


@app.cell
def imports():
    import pandas as pd
    import numpy as np
    from dotenv import load_dotenv, find_dotenv
    from datetime import datetime
    import requests
    import os
    from typing import Literal, List
    import xarray as xr
    from src.datasources import codab, hapi, seas5
    from src.utils import rp_calc, plot
    import ocha_stratus as stratus
    import calendar
    import plotly.express as px
    import plotly.graph_objects as go

    _ = load_dotenv(find_dotenv(usecwd=True))
    return calendar, codab, hapi, pd, plot, rp_calc, seas5, stratus


@app.cell
def cached_functions(mo, pd, stage, stratus):
    @mo.persistent_cache
    def get_cogs(dates, gdf):
        return stratus.stack_cogs(dataset="seas5", dates=dates, clip_gdf=gdf)


    @mo.persistent_cache
    def get_stats(iso3, adm_level):
        engine = stratus.get_engine(stage)
        with engine.connect() as conn:
            df = pd.read_sql(
                f"select * from seas5 where iso3='{iso3}' and adm_level={adm_level}",
                con=conn,
                parse_dates=["valid_date", "issued_date"],
            )
        return df
    return get_cogs, get_stats


@app.cell
def constants(
    adm_level_dropdown,
    iso3_dropdown,
    issued_month_dropdown,
    season_dropdown,
    season_year_dropdown,
):
    # INPUT PARAMETERS
    ISO3 = iso3_dropdown.value
    ADM_LEVEL = adm_level_dropdown.value
    stage = "prod"
    MONTHS = [10, 11, 12] if season_dropdown.value == "OND" else [3, 4, 5]
    SEASON_YEAR = season_year_dropdown.value
    ISSUED_MONTH = issued_month_dropdown.value

    # CONSTANTS
    dates = [
        f"{year}-{month:02d}-01" for year in range(1981, 2026) for month in MONTHS
    ]
    return ADM_LEVEL, ISO3, ISSUED_MONTH, MONTHS, SEASON_YEAR, dates, stage


@app.cell
def data_loading(ADM_LEVEL, ISO3, codab, dates, get_cogs, get_stats, hapi):
    # GET DATA
    df_pop = hapi.get_pop(ISO3, ADM_LEVEL)
    gdf = codab.load_codab_from_blob(ISO3, ADM_LEVEL)

    df_seas5 = get_stats(ISO3, ADM_LEVEL)
    ds_seas5 = get_cogs(dates, gdf)
    return df_pop, df_seas5, ds_seas5, gdf


@app.cell
def df_seas5_processed(
    ADM_LEVEL,
    ISO3,
    ISSUED_MONTH,
    MONTHS,
    codab,
    df_pop,
    df_seas5,
    seas5,
):
    df_seas5_processed = seas5.process_seas5(df_seas5, MONTHS, ISSUED_MONTH)

    # Merge in the population and identify cases where people are in the lower tercile
    df_seas5_processed = df_seas5_processed.merge(
        df_pop[["population", f"admin{ADM_LEVEL}_code", f"admin{ADM_LEVEL}_name"]],
        left_on="pcode",
        right_on=f"admin{ADM_LEVEL}_code",
    )
    df_seas5_processed["pop_lower_tercile"] = df_seas5_processed.apply(
        lambda x: x["population"] if x["is_lower_tercile"] else 0, axis=1
    )

    # Subset to MAM/OND zones only for Ethiopia
    if (ISO3 == "ETH") and (ADM_LEVEL == 2):
        _sel_aoi = codab.subset_aoi(ISO3)
        df_seas5_processed = df_seas5_processed[df_seas5_processed.pcode.isin(_sel_aoi)]
    return (df_seas5_processed,)


@app.cell
def df_annual_sum(SEASON_YEAR, df_seas5_processed, rp_calc):
    # Total people in below avg rainfall and get rp
    df_annual_sum = (
        df_seas5_processed.groupby("year")[["total_rainfall", "pop_lower_tercile"]]
        .sum()
        .reset_index()
    )
    _df = rp_calc.calculate_one_group_rp(
        df_annual_sum, "pop_lower_tercile", ascending=False
    )
    rp = _df.loc[_df["year"] == SEASON_YEAR]["pop_lower_tercile_rp"].values[0]
    pop = _df.loc[_df["year"] == SEASON_YEAR]["pop_lower_tercile"].values[0]
    return df_annual_sum, pop, rp


@app.cell
def _(mo, pop, rp, season_dropdown):
    mo.md(f"""### **{pop:,}** people are forecasted to experience lower tercile rainfall during the upcoming {season_dropdown.value} season. We see this level of people in need once every **{rp:.2f}** years.""")
    return


@app.cell
def df_pop_rps(df_seas5_processed, rp_calc):
    # Now calculate the return periods of population in the lower tercile of values
    _df = (
        df_seas5_processed.groupby("year")["pop_lower_tercile"].sum().reset_index()
    )
    df_pop_rps = rp_calc.calculate_one_group_rp(
        _df, "pop_lower_tercile", ascending=False
    )  # False because a higher number is worse
    return


@app.cell
def gdf_merged(ADM_LEVEL, SEASON_YEAR, df_seas5_processed, gdf):
    # Get the data for just this year and join with the geodataframe to plot
    _df = df_seas5_processed[df_seas5_processed.year == SEASON_YEAR]
    gdf_merged = gdf.merge(
        _df[["pcode", "total_rainfall_rp", "is_lower_tercile"]],
        left_on=f"ADM{ADM_LEVEL}_PCODE",
        right_on="pcode",
        how="left"
    )
    gdf_merged = gdf_merged[
        [
            f"ADM{ADM_LEVEL}_EN",
            f"ADM{ADM_LEVEL}_PCODE",
            "pcode",
            "total_rainfall_rp",
            "is_lower_tercile",
            "geometry",
        ]
    ]
    gdf_merged["geometry"] = gdf_merged["geometry"].simplify(tolerance=0.01)
    return (gdf_merged,)


@app.cell
def graph_rp(ADM_LEVEL, gdf_merged, plot):
    plot.plot_rp_map(gdf_merged, ADM_LEVEL)
    return


@app.cell
def graph_scatter(SEASON_YEAR, df_annual_sum, plot):
    plot.plot_annual_scatter(df_annual_sum, list(range(2020, 2025)), SEASON_YEAR)
    return


@app.cell
def _(ds_seas5):
    ds_seas5
    return


if __name__ == "__main__":
    app.run()
