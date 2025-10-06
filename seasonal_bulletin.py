import marimo

__generated_with = "0.15.2"
app = marimo.App(width="medium")


@app.cell
def _():
    import marimo as mo
    return (mo,)


@app.cell
def inputs(mo):
    seasons = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12]}
    countries = {"Ethiopia": "ETH", "Somalia": "SOM", "Burkina Faso": "BFA"}
    admin_levels = [1, 2]

    iso3_dropdown = mo.ui.dropdown(
        options=countries,
        value=list(countries.keys())[0],
        label="Analysis location",
    )
    adm_level_dropdown = mo.ui.dropdown(
        options=admin_levels, value=2, label="Admin level"
    )
    season_dropdown = mo.ui.dropdown(options=seasons, value="OND", label="Season")
    season_year_dropdown = mo.ui.dropdown(
        options=range(2020, 2027), value=2025, label="Season year"
    )
    data_source_dropdown = mo.ui.dropdown(
        label="Data source", options=["forecast", "reanalysis"], value="forecast"
    )

    mo.hstack(
        [
            iso3_dropdown,
            adm_level_dropdown,
            season_dropdown,
            season_year_dropdown,
            data_source_dropdown,
        ],
        justify="center",
    )
    return (
        adm_level_dropdown,
        data_source_dropdown,
        iso3_dropdown,
        season_dropdown,
        season_year_dropdown,
    )


@app.cell
def _(
    data_source_dropdown,
    datetime,
    mo,
    season_dropdown,
    season_year_dropdown,
):
    # Check if forecast or reanalysis data is available

    now = datetime.now()
    leadtime_month_dropdown = None

    if data_source_dropdown.value == "reanalysis":
        # Check if the reanalysis data is available
        reanalysis_available = (
            season_year_dropdown.value,
            season_dropdown.value[-1],
        ) <= (now.year, now.month - 1)

        mo.stop(not reanalysis_available, mo.center(mo.md("Reanalysis data not available yet!")))
    
    elif data_source_dropdown.value == "forecast":
        # Check if forecast data is available
        forecast_available = (
            season_year_dropdown.value,
            season_dropdown.value[0],
        ) <= (now.year, now.month)

        mo.stop(not forecast_available, mo.center(mo.md("Forecast data not available yet!")))
        leadtime_month_dropdown = mo.ui.dropdown(
            label="Forecast leadtime (months)", options=range(0, 7), value=1
        )
    mo.hstack([leadtime_month_dropdown], justify="center")
    return (leadtime_month_dropdown,)


@app.cell
def _(
    adm_level_dropdown,
    calendar,
    data_source_dropdown,
    iso3_dropdown,
    leadtime_month_dropdown,
    season_dropdown,
    season_year_dropdown,
):
    # INPUT PARAMETERS
    ISO3 = iso3_dropdown.value
    ADM_LEVEL = adm_level_dropdown.value
    stage = "prod"
    MONTHS = season_dropdown.value
    SEASON_YEAR = season_year_dropdown.value

    if data_source_dropdown.value == "forecast":
        ISSUED_MONTH = (MONTHS[0] - leadtime_month_dropdown.value)
        STACK_DATES = [f"{year}-{ISSUED_MONTH:02d}-01" for year in range(1981, 2026)]
        title = f"# {iso3_dropdown.selected_key}: {SEASON_YEAR} {season_dropdown.selected_key} Season Outlook"
        subtitle = f"#### ECMWF Seasonal Forecast issued {calendar.month_name[ISSUED_MONTH]} {SEASON_YEAR} ({leadtime_month_dropdown.value} month leadtime)"
    else:
        title = f"# {iso3_dropdown.selected_key}: {SEASON_YEAR} {season_dropdown.selected_key} Season Overview"
        subtitle = f"#### ECMWF ERA5 Reanalysis"
    return ADM_LEVEL, ISO3, ISSUED_MONTH, MONTHS, SEASON_YEAR, subtitle, title


@app.cell
def _(mo):
    mo.Html("<hr></hr><br>")
    return


@app.cell
def _(mo, title):
    mo.center(mo.md(title))
    return


@app.cell
def _(mo, subtitle):
    mo.center(mo.md(subtitle))
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
    from src.datasources import codab, hapi, seas5, era5
    from src.utils import rp_calc, plot
    import ocha_stratus as stratus
    import calendar
    import plotly.express as px
    import plotly.graph_objects as go
    from calendar import monthrange

    _ = load_dotenv(find_dotenv(usecwd=True))
    return (
        calendar,
        codab,
        datetime,
        era5,
        hapi,
        monthrange,
        np,
        pd,
        plot,
        px,
        rp_calc,
        seas5,
        stratus,
    )


@app.cell
def cached_functions(codab, hapi, mo, seas5, stratus):
    @mo.persistent_cache
    def get_cogs(dates, gdf):
        return stratus.stack_cogs(dataset="seas5", dates=dates, clip_gdf=gdf)


    @mo.persistent_cache
    def get_season_stats(iso3, adm_level, issued_month, valid_months):
        return seas5.get_season_stats(iso3, adm_level, issued_month, valid_months)


    @mo.persistent_cache
    def load_codab_from_blob(iso3, adm_level):
        return codab.load_codab_from_blob(iso3, adm_level)


    @mo.persistent_cache
    def get_pop(iso3, adm_level):
        return hapi.get_pop(iso3, adm_level)
    return get_pop, get_season_stats, load_codab_from_blob


@app.cell
def data_loading(
    ADM_LEVEL,
    ISO3,
    ISSUED_MONTH,
    MONTHS,
    get_pop,
    get_season_stats,
    load_codab_from_blob,
):
    # GET DATA
    df_pop = get_pop(ISO3, ADM_LEVEL)
    gdf = load_codab_from_blob(ISO3, ADM_LEVEL)
    df_seas5 = get_season_stats(ISO3, ADM_LEVEL, ISSUED_MONTH, MONTHS)
    # ds_seas5 = get_cogs(dates, gdf)
    return df_pop, df_seas5, gdf


@app.function
def lower_tercile_pop(df, df_pop, adm_level):
    # Merge in the population and identify cases where people are in the lower tercile
    _df = df.merge(
        df_pop[
            ["population", f"admin{adm_level}_code", f"admin{adm_level}_name"]
        ],
        left_on="pcode",
        right_on=f"admin{adm_level}_code",
    )
    _df["pop_lower_tercile"] = _df.apply(
        lambda x: x["population"] if x["meets_threshold"] else 0, axis=1
    )
    return _df


@app.cell
def _(ADM_LEVEL, ISO3, codab, df_pop, df_seas5, rp_calc, seas5):
    _df = seas5.total_seasonal_precip(df_seas5)
    _df = rp_calc.classify_groups_quantile(_df, q=0.33, column="sum_season")
    _df = rp_calc.calculate_groups_rp(_df, "pcode", "sum_season")
    df_seas5_processed = lower_tercile_pop(_df, df_pop, ADM_LEVEL)


    # Subset to MAM/OND zones only for Ethiopia zones
    if (ISO3 == "ETH") and (ADM_LEVEL == 2):
        _sel_aoi = codab.subset_aoi(ISO3)
        df_seas5_processed = df_seas5_processed[
            df_seas5_processed.pcode.isin(_sel_aoi)
        ]
    return (df_seas5_processed,)


@app.cell
def df_annual_sum_seas5(SEASON_YEAR, df_seas5_processed, rp_calc):
    _df = (
        df_seas5_processed.groupby("season")[["sum_season", "pop_lower_tercile"]]
        .sum()
        .reset_index()
    )
    df_annual_sum_seas5 = rp_calc.calculate_one_group_rp(
        _df, "pop_lower_tercile", ascending=False
    )
    rp = df_annual_sum_seas5.loc[df_annual_sum_seas5["season"] == SEASON_YEAR][
        "pop_lower_tercile_rp"
    ].values[0]
    pop = df_annual_sum_seas5.loc[df_annual_sum_seas5["season"] == SEASON_YEAR][
        "pop_lower_tercile"
    ].values[0]
    return df_annual_sum_seas5, pop, rp


@app.cell
def _(mo, pop, rp, season_dropdown):
    mo.md(f"""### **{pop:,}** people are forecasted to experience lower tercile rainfall during the upcoming {season_dropdown.selected_key} season. We see this level of people in need once every **{rp:.2f}** years.""")
    return


@app.cell
def gdf_merged(ADM_LEVEL, SEASON_YEAR, df_seas5_processed, gdf):
    # Get the data for just this year and join with the geodataframe to plot
    _df = df_seas5_processed[df_seas5_processed.season == SEASON_YEAR]
    gdf_merged = gdf.merge(
        _df[["pcode", "sum_season_rp", "meets_threshold", "population"]],
        left_on=f"ADM{ADM_LEVEL}_PCODE",
        right_on="pcode",
        how="left",
    )
    gdf_merged = gdf_merged[
        [
            f"ADM{ADM_LEVEL}_EN",
            f"ADM{ADM_LEVEL}_PCODE",
            "pcode",
            "sum_season_rp",
            "meets_threshold",
            "population",
            "geometry",
        ]
    ]
    gdf_merged["geometry"] = gdf_merged["geometry"].simplify(tolerance=0.01)
    return (gdf_merged,)


@app.cell
def _(mo):
    map_variable = mo.ui.radio(
        options=["population", "sum_season_rp"],
        label="Select variable to display:",
        value="sum_season_rp",
        inline=True,
    )
    map_variable
    return (map_variable,)


@app.cell
def graph_rp(ADM_LEVEL, gdf_merged, map_variable, plot):
    plot.plot_map(gdf_merged, ADM_LEVEL, map_variable.value)
    return


@app.cell
def graph_scatter(SEASON_YEAR, df_annual_sum_seas5, plot):
    plot.plot_annual_scatter(
        df_annual_sum_seas5, list(range(2020, 2025)), SEASON_YEAR
    )
    return


@app.cell
def _(ISSUED_MONTH, MONTHS, SEASON_YEAR, ds_seas5, monthrange, pd):
    da = ds_seas5.copy()
    # TODO: Does not handle year-crossing
    leadtimes = [month - ISSUED_MONTH for month in MONTHS]
    da = da.sel(leadtime=leadtimes)
    days_in_month = monthrange(SEASON_YEAR, ISSUED_MONTH)[1]
    da = da * days_in_month

    # Extract years from date coordinate
    years = pd.to_datetime(da.date.values).year

    # Add year coordinate and sum
    da_with_year = da.assign_coords(year=("date", years))
    da_yearly = da_with_year.groupby("year").sum(dim=["leadtime"])

    # Get current and average
    cur = da_yearly.sel(date=f"{SEASON_YEAR}-{str(ISSUED_MONTH).zfill(2)}-01")
    avg = da_yearly.mean(dim="date")
    return avg, cur


@app.cell
def _(avg, cur, gdf_merged, np, px):
    anom = cur - avg
    gdf_sel = gdf_merged[gdf_merged.pcode.notna()]
    anom_clipped = anom.rio.clip(gdf_sel.geometry.values, gdf_sel.crs, drop=True)

    # 2. Balance the color scale around zero
    vmax = np.abs(anom_clipped).max().values.item()
    vmin = -vmax

    # 3. Create the plot with balanced colors and no axis labels
    fig = px.imshow(
        anom_clipped,
        origin="lower",
        color_continuous_scale="RdBu",
        zmin=vmin,
        zmax=vmax,
        aspect="equal",
    )

    # Remove axis labels and ticks
    fig.update_xaxes(
        showticklabels=False,
        title="",
        showgrid=False,
        zeroline=False,
        showline=False,
        ticks="",
    )
    fig.update_yaxes(
        showticklabels=False,
        title="",
        showgrid=False,
        zeroline=False,
        showline=False,
        ticks="",
    )
    fig.update_coloraxes(
        colorbar=dict(
            title="Rainfall anomaly<br>(mm)",
        )
    )
    fig.update_layout(template="simple_white")

    fig
    return


@app.cell
def _(mo):
    mo.md(r"""# ERA5 MAM download""")
    return


@app.cell
def _(ADM_LEVEL, ISO3, codab, df_pop, era5, rp_calc):
    df_era5 = era5.get_season_stats(ISO3, ADM_LEVEL, [3, 4, 5])
    _df = era5.total_seasonal_precip(df_era5)
    _df = rp_calc.classify_groups_quantile(_df, q=0.33, column="sum_season")
    _df = rp_calc.calculate_groups_rp(_df, "pcode", "sum_season")


    # Merge in the population and identify cases where people are in the lower tercile
    df_era5_processed = _df.merge(
        df_pop[["population", f"admin{ADM_LEVEL}_code", f"admin{ADM_LEVEL}_name"]],
        left_on="pcode",
        right_on=f"admin{ADM_LEVEL}_code",
    )
    df_era5_processed["pop_lower_tercile"] = df_era5_processed.apply(
        lambda x: x["population"] if x["meets_threshold"] else 0, axis=1
    )

    # Subset to MAM/OND zones only for Ethiopia zones
    if (ISO3 == "ETH") and (ADM_LEVEL == 2):
        _sel_aoi = codab.subset_aoi(ISO3)
        df_era5_processed = df_era5_processed[
            df_era5_processed.pcode.isin(_sel_aoi)
        ]
    return (df_era5_processed,)


@app.cell
def _(df_era5_processed, rp_calc):
    _df = (
        df_era5_processed.groupby("season")[["sum_season", "pop_lower_tercile"]]
        .sum()
        .reset_index()
    )
    df_annual_sum_era5 = rp_calc.calculate_one_group_rp(
        _df, "pop_lower_tercile", ascending=False
    )
    return (df_annual_sum_era5,)


@app.cell
def _(df_annual_sum_era5):
    df_annual_sum_era5
    return


@app.cell
def _(df_annual_sum_era5, px):
    _fig = px.scatter(
        df_annual_sum_era5,
        x="pop_lower_tercile",
        y="pop_lower_tercile_rp",
        template="simple_white",
    )
    _fig.update_layout(
        height=300,
        yaxis_title="Return Period",
        xaxis_title="Population Exposed",
        title="Return Periods of MAM Population Exposed to Below Average Rainfall",
    )
    _fig
    return


@app.cell
def _(df_annual_sum_seas5, px):
    _fig = px.scatter(
        df_annual_sum_seas5,
        x="pop_lower_tercile",
        y="pop_lower_tercile_rp",
        template="simple_white",
    )
    _fig.update_layout(
        height=300,
        yaxis_title="Return Period",
        xaxis_title="Population Exposed",
        title="Return Periods of OND Population Exposed to Below Average Rainfall",
    )
    _fig
    return


@app.cell
def _(df_annual_sum_era5, df_annual_sum_seas5):
    df_annual_joined = df_annual_sum_seas5[["season", "pop_lower_tercile"]].merge(
        df_annual_sum_era5[["season", "pop_lower_tercile"]],
        on="season",
        suffixes=["_ond", "_mam"],
    )
    return (df_annual_joined,)


@app.cell
def _(df_annual_joined, pop, px):
    _fig = px.bar(
        df_annual_joined,
        x="season",
        y=["pop_lower_tercile_ond", "pop_lower_tercile_mam"],
        title="Wide-Form Input",
        barmode="group",
        template="simple_white",
    )

    _fig.add_hrect(
        y0=pop, y1=pop + (pop * 0.001), line_width=0, fillcolor="red", opacity=0.9
    )

    _fig.update_layout(
        height=500,
        yaxis_title="Population Exposed",
        xaxis_title="Season",
        title="Annual Population Exposed to Below Average Rainfall - OND and MAM",
    )
    _fig
    return


@app.cell
def _(df_annual_joined):
    df_annual_joined["max_exposed"] = df_annual_joined[
        ["pop_lower_tercile_mam", "pop_lower_tercile_ond"]
    ].max(axis=1)
    return


@app.cell
def _(df_annual_joined, rp_calc):
    rp_calc.calculate_one_group_rp(
        df_annual_joined, "max_exposed", ascending=False
    )
    return


if __name__ == "__main__":
    app.run()
