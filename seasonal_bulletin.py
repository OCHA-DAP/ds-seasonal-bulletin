import marimo

__generated_with = "0.18.1"
app = marimo.App(width="medium")


@app.cell
def imports():
    import calendar
    from datetime import datetime

    import marimo as mo
    import ocha_stratus as stratus
    import pandas as pd
    from dotenv import find_dotenv, load_dotenv

    from src.datasources import era5, hapi, seas5
    from src.utils import plot, precip, rp_calc

    _ = load_dotenv(find_dotenv(usecwd=True))
    return (
        calendar,
        datetime,
        era5,
        hapi,
        mo,
        pd,
        plot,
        precip,
        rp_calc,
        seas5,
        stratus,
    )


@app.cell
def inputs(mo):
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
    season_year_dropdown = mo.ui.dropdown(
        options=range(2020, 2027), value=2025, label="Season year"
    )
    data_source_dropdown = mo.ui.dropdown(
        label="Data source", options=["forecast", "reanalysis"], value="forecast"
    )

    season_range = mo.ui.range_slider(
        start=1,
        stop=12,
        step=1,
        show_value=True,
        value=[10, 12],
        label="Select month range",
        debounce=True,
    )

    mo.hstack(
        [
            iso3_dropdown,
            adm_level_dropdown,
            season_year_dropdown,
            data_source_dropdown,
            season_range,
        ],
        justify="center",
    )
    return (
        adm_level_dropdown,
        data_source_dropdown,
        iso3_dropdown,
        season_range,
        season_year_dropdown,
    )


@app.cell
def _(calendar, season_range):
    season_months = list(range(season_range.value[0], season_range.value[1] + 1))
    season_str = "".join(calendar.month_name[month][0] for month in season_months)
    return season_months, season_str


@app.cell
def _(data_source_dropdown, datetime, mo, season_months, season_year_dropdown):
    # Check if forecast or reanalysis data is available

    now = datetime.now()
    leadtime_month_dropdown = None

    if data_source_dropdown.value == "reanalysis":
        # Check if the reanalysis data is available
        reanalysis_available = (
            season_year_dropdown.value,
            season_months[-1],
        ) <= (now.year, now.month - 1)

        mo.stop(
            not reanalysis_available,
            mo.center(mo.callout("Reanalysis data not available yet!", kind="danger")),
        )

    elif data_source_dropdown.value == "forecast":
        # Check if forecast data is available
        forecast_available = (
            season_year_dropdown.value,
            season_months[0],
        ) <= (now.year, now.month)

        mo.stop(
            not forecast_available,
            mo.center(mo.md("Forecast data not available yet!")),
        )
        leadtime_month_dropdown = mo.ui.dropdown(
            label="Forecast leadtime (months)", options=range(0, 7), value=1
        )

    mo.hstack([leadtime_month_dropdown], justify="center")
    return (leadtime_month_dropdown,)


@app.cell
def _(mo):
    admin_filtering = mo.ui.switch(
        label="Filter to locations with bimodal seasons", value=True
    )
    mo.center(admin_filtering)
    return (admin_filtering,)


@app.cell
def _(
    adm_level_dropdown,
    calendar,
    data_source_dropdown,
    iso3_dropdown,
    leadtime_month_dropdown,
    season_months,
    season_str,
    season_year_dropdown,
):
    # INPUT PARAMETERS
    ISO3 = iso3_dropdown.value
    ADM_LEVEL = adm_level_dropdown.value
    MONTHS = season_months
    SEASON_YEAR = season_year_dropdown.value
    DATASET = data_source_dropdown.value
    CLIM_START = 1993  # Follows ECMWF
    CLIM_END = 2016  # Follows ECMWF

    if DATASET == "forecast":
        ISSUED_MONTH = MONTHS[0] - leadtime_month_dropdown.value
        CLIM_DATES = [
            f"{year}-{ISSUED_MONTH:02d}-01" for year in range(CLIM_START, CLIM_END + 1)
        ]
        CUR_DATES = [f"{SEASON_YEAR}-{ISSUED_MONTH:02d}-01"]
        title = (
            f"# {iso3_dropdown.selected_key}: {SEASON_YEAR} {season_str} Season Outlook"
        )
        subtitle = f"#### ECMWF Seasonal Forecast issued {calendar.month_name[ISSUED_MONTH]} {SEASON_YEAR} ({leadtime_month_dropdown.value} month leadtime)"
    else:
        ISSUED_MONTH = None
        CLIM_DATES = [
            f"{year}-{month:02d}-01"
            for year in range(CLIM_START, CLIM_END + 1)
            for month in MONTHS
        ]
        # TODO - Does not handle year crossing
        CUR_DATES = [f"{SEASON_YEAR}-{month:02d}-01" for month in MONTHS]
        title = f"# {iso3_dropdown.selected_key}: {SEASON_YEAR} {season_str} Season Overview"
        subtitle = "#### ECMWF ERA5 Reanalysis"
    return (
        ADM_LEVEL,
        CLIM_DATES,
        CUR_DATES,
        DATASET,
        ISO3,
        ISSUED_MONTH,
        MONTHS,
        SEASON_YEAR,
        subtitle,
        title,
    )


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
def cached_functions(era5, hapi, mo, seas5, stratus):
    @mo.cache
    def get_cogs(dates, gdf, dataset):
        source = "seas5" if dataset == "forecast" else "era5"
        return stratus.stack_cogs(dataset=source, dates=dates, clip_gdf=gdf)

    @mo.cache
    def get_season_stats(iso3, adm_level, valid_months, dataset, issued_month=None):
        if dataset == "forecast":
            return seas5.get_season_stats(iso3, adm_level, issued_month, valid_months)
        elif dataset == "reanalysis":
            return era5.get_season_stats(iso3, adm_level, valid_months)

    @mo.cache
    def load_codab_from_blob(iso3, adm_level):
        return stratus.codab.load_codab_from_blob(iso3, adm_level)

    @mo.cache
    def get_pop(iso3, adm_level):
        return hapi.get_pop(iso3, adm_level)

    return get_cogs, get_pop, get_season_stats, load_codab_from_blob


@app.cell
def _(ADM_LEVEL, era5, rp_calc, seas5):
    # Merge in the population and identify cases where people are in the lower tercile
    def lower_tercile_pop(df, df_pop, adm_level):
        _df = df.merge(
            df_pop[["population", f"admin{adm_level}_code", f"admin{adm_level}_name"]],
            left_on="pcode",
            right_on=f"admin{adm_level}_code",
        )
        _df["pop_lower_tercile"] = _df.apply(
            lambda x: x["population"] if x["meets_threshold"] else 0, axis=1
        )
        return _df

    def process_season_precip(df_precip, df_pop, dataset):
        if dataset == "forecast":
            _df = seas5.total_seasonal_precip(df_precip)
        elif dataset == "reanalysis":
            _df = era5.total_seasonal_precip(df_precip)

        _df = rp_calc.classify_groups_quantile(_df, q=0.33, column="sum_season")
        _df = rp_calc.calculate_groups_rp(_df, "pcode", "sum_season")
        return lower_tercile_pop(_df, df_pop, ADM_LEVEL)

    return (process_season_precip,)


@app.cell
def data_loading(
    ADM_LEVEL,
    DATASET,
    ISO3,
    ISSUED_MONTH,
    MONTHS,
    admin_filtering,
    get_pop,
    get_season_stats,
    load_codab_from_blob,
    process_season_precip,
    stratus,
):
    # GET DATA
    df_pop = get_pop(ISO3, ADM_LEVEL)
    gdf = load_codab_from_blob(ISO3, ADM_LEVEL)
    df_precip = get_season_stats(ISO3, ADM_LEVEL, MONTHS, DATASET, ISSUED_MONTH)
    df_precip_processed = process_season_precip(df_precip, df_pop, DATASET)

    if admin_filtering.value:
        fname = f"ds-seasonal-bulletin/harmonic_seasonality/{ISO3.lower()}_adm{ADM_LEVEL}_seasonality.csv"
        try:
            df_seasonality = stratus.load_csv_from_blob(fname, stage="dev")
            filter_pcodes = list(
                df_seasonality[df_seasonality.cluster == 1][f"ADM{ADM_LEVEL}_PCODE"]
            )
            df_precip_processed = df_precip_processed[
                df_precip_processed.pcode.isin(filter_pcodes)
            ]
        except Exception:
            print("Error reading seasonality file! Not filtering locations")
    return df_precip_processed, gdf


@app.cell
def df_annual_sum_seas5(SEASON_YEAR, df_precip_processed, rp_calc):
    # Get return periods on population exposed per season
    _df = (
        df_precip_processed.groupby("season")[["sum_season", "pop_lower_tercile"]]
        .sum()
        .reset_index()
    )
    df_annual_sum_precip = rp_calc.calculate_one_group_rp(
        _df, "pop_lower_tercile", ascending=False
    )
    rp = df_annual_sum_precip.loc[df_annual_sum_precip["season"] == SEASON_YEAR][
        "pop_lower_tercile_rp"
    ].values[0]
    pop = df_annual_sum_precip.loc[df_annual_sum_precip["season"] == SEASON_YEAR][
        "pop_lower_tercile"
    ].values[0]
    return df_annual_sum_precip, pop, rp


@app.cell
def _(mo):
    mo.md(
        r"""
    ## Total population impacted
    """
    )
    return


@app.cell
def _(mo):
    mo.Html("<hr></hr>")
    return


@app.cell
def _(mo, pop, rp, season_str):
    mo.md(
        f"""
    **{pop:,}** people are forecasted to experience below average (lower tercile) rainfall during the {season_str} season. We see this level of people in need once every **{rp:.2f}** years. See the plot below to understand how this level of impact compares with previous years. Interpretation of absolute values of seasonal precipitation should be done with caution as forecast and reanalysis products can be subject to significant bias. These precipitation values should instead be interpreted in relative terms.
    """
    )
    return


@app.cell
def graph_scatter(
    SEASON_YEAR,
    df_annual_sum_precip,
    iso3_dropdown,
    pd,
    plot,
    stratus,
):
    df_cerf_annual = None
    if iso3_dropdown.value == "ETH":
        df_cerf = stratus.load_csv_from_blob(
            "ds-seasonal-bulletin/misc/CERF Donor Contributions and Allocations - Ethiopia (Drought).csv"
        )
        df_cerf = df_cerf[df_cerf.Season.str.contains("OND")]
        df_cerf["Approved amount in US$"] = pd.to_numeric(
            df_cerf["Approved amount in US$"].str.replace(",", ""), errors="coerce"
        )
        df_cerf_annual = (
            df_cerf.groupby("SEASON_YEAR")["Approved amount in US$"].sum().reset_index()
        )

    plot.plot_annual_scatter(df_annual_sum_precip, SEASON_YEAR, df_cerf_annual)
    return


@app.cell
def _(mo):
    mo.md(
        r"""
    ## Return periods of rainfall per admin level
    """
    )
    return


@app.cell
def _(mo):
    mo.Html("<hr></hr>")
    return


@app.cell
def _(mo):
    mo.md(
        r"""
    The plot below shows the return periods of total seasonal rainfall per admin level. Admin regions experiencing lower tercile rainfall are highlighted. The total number of people impacted in the section above is the sum of the total population in these highlighted regions.
    """
    )
    return


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
def graph_rp(
    ADM_LEVEL,
    SEASON_YEAR,
    df_precip_processed,
    gdf,
    map_variable,
    plot,
):
    # Prep and plot geodata on map for current return periods
    _df = df_precip_processed[df_precip_processed.season == SEASON_YEAR]
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
    # Simplify geometry for faster visualization
    gdf_merged["geometry"] = gdf_merged["geometry"].simplify(tolerance=0.01)

    plot.plot_map(gdf_merged, ADM_LEVEL, map_variable.value)
    return (gdf_merged,)


@app.cell
def _(ADM_LEVEL, gdf_merged):
    gdf_merged[
        [
            f"ADM{ADM_LEVEL}_EN",
            "pcode",
            "sum_season_rp",
            "meets_threshold",
            "population",
        ]
    ].sort_values("sum_season_rp", ascending=False, axis=0).dropna()
    return


@app.cell
def _(mo):
    mo.Html("<br><br>")
    return


@app.cell
def _(mo):
    mo.md(
        r"""
    ## Gridded rainfall anomaly
    """
    )
    return


@app.cell
def _(mo):
    mo.Html("<hr></hr>")
    return


@app.cell
def _(mo):
    anomaly_switch = mo.ui.switch(
        label="Display anomaly? If not previously cached, the anomaly may take several minutes to compute!",
        value=False,
    )
    mo.callout(mo.hstack([anomaly_switch]), kind="warn")
    return (anomaly_switch,)


@app.cell
def _(
    CLIM_DATES,
    CUR_DATES,
    DATASET,
    ISSUED_MONTH,
    MONTHS,
    SEASON_YEAR,
    anomaly_switch,
    gdf,
    gdf_merged,
    get_cogs,
    mo,
    plot,
    precip,
):
    mo.stop(not anomaly_switch.value, mo.md(""))

    da_clim = get_cogs(CLIM_DATES, gdf, DATASET)
    da_cur = get_cogs(CUR_DATES, gdf, DATASET)

    da_clim_processed, da_cur_processed = precip.process_cogs(
        da_clim=da_clim,
        da_cur=da_cur,
        months=MONTHS,
        issued_month=ISSUED_MONTH,
        season_year=SEASON_YEAR,
    )
    da_anom = da_cur_processed - da_clim_processed

    gdf_sel = gdf_merged[gdf_merged.pcode.notna()]

    anom_plot = plot.plot_anomaly(da_anom, gdf_sel)
    clim_plot = plot.plot_climatology(da_clim_processed, gdf_sel)
    return anom_plot, clim_plot


@app.cell
def _(anom_plot, clim_plot, mo):
    mo.hstack([clim_plot, anom_plot])
    return


@app.cell
def _():
    return


@app.cell
def _():
    return


if __name__ == "__main__":
    app.run()
