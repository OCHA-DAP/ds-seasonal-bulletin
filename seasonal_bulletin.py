import marimo

__generated_with = "0.15.2"
app = marimo.App()

with app.setup:
    import calendar

    import marimo as mo
    import matplotlib.pyplot as plt
    import numpy as np
    import ocha_stratus as stratus
    import pandas as pd
    from dotenv import find_dotenv, load_dotenv

    from src.datasources import cerf, emdat, era5, hapi, seas5, codab
    from src.utils import plot, precip, rp_calc, timeseries

    _ = load_dotenv(find_dotenv(usecwd=True))

    # load admin data
    df_adms = pd.read_sql(
        "SELECT pcode, name, iso3, adm_level FROM public.polygon ORDER BY name ASC",
        stratus.get_engine(stage="prod"),
    )
    df_adm0 = df_adms.set_index("adm_level").loc[0]
    df_adm1 = df_adms.set_index("adm_level").loc[1]
    df_adm2 = df_adms.set_index("adm_level").loc[2]
    adm0_options = {
        row["name"]: row["pcode"]
        for _, row in df_adm0.iterrows()
        if row["name"] is not None
    }

    impact_col = "Total Affected"


@app.cell
def _():
    @mo.cache
    def get_cogs(dates, gdf, dataset):
        source = "seas5" if dataset == "forecast" else "era5"
        return stratus.stack_cogs(dataset=source, dates=dates, clip_gdf=gdf)

    # @mo.cache
    def get_season_stats(iso3, adm_level, valid_months, dataset, issued_month=None):
        if dataset == "forecast":
            df_raw = seas5.get_season_stats(iso3, adm_level, issued_month, valid_months)
            df_processed = seas5.aggregate_seas5_yearly(
                df_raw, issued_month, valid_months
            )
        elif dataset == "reanalysis":
            df_raw = era5.get_season_stats(iso3, adm_level, valid_months)
            df_processed = era5.aggregate_era5_yearly(df_raw, valid_months)
        return df_processed

    @mo.cache
    def load_codab_from_blob(iso3, adm_level):
        return stratus.codab.load_codab_from_blob(iso3, adm_level)

    @mo.cache
    def get_pop(iso3, adm_level):
        return hapi.get_pop(iso3, adm_level)

    return get_cogs, get_pop, get_season_stats, load_codab_from_blob


@app.cell
def _():
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

    def process_season_precip(df_precip, df_pop, adm_level, val_col="mean"):
        _df = df_precip.copy()
        _df = rp_calc.classify_groups_quantile(_df, q=0.33, column=val_col)
        _df = rp_calc.calculate_groups_rp(_df, "pcode", val_col)
        return lower_tercile_pop(_df, df_pop, adm_level)

    return (process_season_precip,)


@app.cell(hide_code=True)
def _():
    mo.md(
        r"""
    # ECMWF Seasonal Bulletin

    Exploration of ECMWF SEAS5 seasonal forecast.
    """
    )
    return


@app.cell(hide_code=True)
def _():
    mo.md(r"""## Parameter selection""")
    return


@app.cell
def _():
    adm0_dropdown = mo.ui.dropdown(
        options=adm0_options, label="Country", value="Ethiopia"
    )
    adm_level_dropdown_sk = mo.ui.dropdown(options=[1, 2], label="Admin level", value=1)

    mo.hstack(
        [mo.md("**Administrative division:**"), adm0_dropdown, adm_level_dropdown_sk],
        justify="start",
    )
    return adm0_dropdown, adm_level_dropdown_sk


@app.cell
def _():
    admin_filtering = mo.ui.switch(
        label="Filter to locations with bimodal seasons", value=True
    )
    mo.hstack(
        [mo.md("**Administrative subsetting:**"), admin_filtering], justify="start"
    )
    return (admin_filtering,)


@app.cell
def _(adm0_dropdown, adm_level_dropdown_sk):
    adm_level = adm_level_dropdown_sk.value
    adm0_pcode = adm0_dropdown.value
    adm0_name = adm0_dropdown.selected_key
    iso3 = df_adms[df_adms["pcode"] == adm0_pcode].iloc[0]["iso3"]
    return adm_level, iso3


@app.cell
def _():
    issued_month_dropdown_options, latest_issued_date = seas5.calculate_issued_months()

    issued_month_dropdown = mo.ui.dropdown(
        options=issued_month_dropdown_options,
        label="Issued month",
        value=f"{calendar.month_abbr[latest_issued_date.month]} {latest_issued_date.year}",
    )

    valid_months_slider = mo.ui.range_slider(
        steps=range(7), label="Leadtimes", value=(1, 3)
    )
    return issued_month_dropdown, valid_months_slider


@app.cell
def _(issued_month_dropdown, valid_mo_str, valid_months_slider):
    mo.hstack(
        [
            mo.md("**Date selection:**"),
            issued_month_dropdown,
            valid_months_slider,
            valid_mo_str,
        ],
        justify="start",
    )
    return


@app.cell
def _():
    disaster_type_dropdown = mo.ui.dropdown(
        options=["Flood", "Drought"], label="Disaster Type", value="Drought"
    )

    mo.hstack([mo.md("**Impact data:**"), disaster_type_dropdown], justify="start")
    return (disaster_type_dropdown,)


@app.cell
def _(disaster_type_dropdown, issued_month_dropdown, valid_months_slider):
    issued_month = issued_month_dropdown.value
    disaster_type = disaster_type_dropdown.value

    valid_months = [
        (issued_month + x - 1) % 12 + 1
        for x in range(valid_months_slider.value[0], valid_months_slider.value[1] + 1)
    ]

    if len(valid_months) < 3:
        valid_mo_str = "-".join([calendar.month_abbr[x] for x in valid_months])
    else:
        valid_mo_str = "".join([calendar.month_abbr[x][0] for x in valid_months])
    return disaster_type, issued_month, valid_mo_str, valid_months


@app.cell
def _():
    data_switch = mo.ui.switch(
        label="Retrieve data? Make sure your selections above are correct.",
        value=False,
    )
    mo.callout(mo.hstack([data_switch]), kind="warn")
    return (data_switch,)


@app.cell
def _(
    adm_level,
    data_switch,
    disaster_type,
    get_season_stats,
    iso3,
    issued_month,
    valid_months,
):
    mo.stop(not data_switch.value, mo.md(""))

    # --- Retrieve yearly summary stats
    df_forecast = get_season_stats(
        iso3, adm_level, valid_months, "forecast", issued_month
    )
    df_reanalysis = get_season_stats(iso3, adm_level, valid_months, "reanalysis")

    # --- Do we want to display the forecast or just the reanalysis?
    forecast_issued_year = df_forecast["year"].max()
    show_current_forecast = forecast_issued_year not in df_reanalysis["year"].values
    # valid_months_note = (
    #     ""
    #     if show_current_forecast
    #     else "_reanalysis available; current forecast line will not be shown_"
    # )
    max_index = (
        forecast_issued_year - 1 if show_current_forecast else forecast_issued_year
    )

    # --- Now also detrend the forecast data
    # TODO: Can we do this in the function??
    df_forecast = timeseries.detrend_column(
        df_forecast, "mean", index_col="year", max_index=max_index
    )

    # --- Load CERF and EM-DAT impact data
    df_emdat = emdat.load_emdat_yearly(
        iso3=iso3, disaster_type=disaster_type, col=impact_col
    )
    # TODO: Connect to real data
    df_cerf = cerf.load_cerf_yearly(emergency=disaster_type, iso3=iso3)

    # --- Merge the forecast and reanalysis datasets together,
    # --- and combine with CERF and EM-DAT impact data
    df_compare = (
        df_forecast.merge(
            df_reanalysis,
            on=["year", "pcode"],
            how="outer",
            suffixes=("_seas5", "_era5"),
        )
        .merge(df_emdat, how="outer")
        .merge(df_cerf, how="outer")
    )
    df_compare.loc[df_compare["year"] < 2006, "allocation"] = "pre-CERF"
    return df_compare, df_forecast, df_reanalysis, show_current_forecast


@app.cell(hide_code=True)
def _():
    mo.md(r"""## Population exposed""")
    return


@app.cell
def _(MONTHS):
    season_str = "".join(calendar.month_name[month][0] for month in MONTHS)
    return (season_str,)


@app.cell
def _(adm_level, iso3, issued_month, show_current_forecast, valid_months):
    # INPUT PARAMETERS
    # ISO3 = iso3_dropdown.value
    ISO3 = iso3
    # ADM_LEVEL = adm_level_dropdown.value
    ADM_LEVEL = adm_level
    # MONTHS = season_months
    MONTHS = valid_months
    # SEASON_YEAR = season_year_dropdown.value
    # SEASON_YEAR = int(forecast_issued_year)
    # DATASET = data_source_dropdown.value
    DATASET = "forecast" if show_current_forecast else "reanalysis"
    # CLIM_START = 1993  # Follows ECMWF
    # CLIM_END = 2016  # Follows ECMWF

    ISSUED_MONTH = issued_month

    # if DATASET == "forecast":
    #     ISSUED_MONTH = MONTHS[0] - leadtime_month_dropdown.value
    #     CLIM_DATES = [
    #         f"{year}-{ISSUED_MONTH:02d}-01" for year in range(CLIM_START, CLIM_END + 1)
    #     ]
    #     CUR_DATES = [f"{SEASON_YEAR}-{ISSUED_MONTH:02d}-01"]
    #     title = (
    #         f"# {iso3_dropdown.selected_key}: {SEASON_YEAR} {season_str} Season Outlook"
    #     )
    #     subtitle = f"#### ECMWF Seasonal Forecast issued {calendar.month_name[ISSUED_MONTH]} {SEASON_YEAR} ({leadtime_month_dropdown.value} month leadtime)"
    # else:
    #     ISSUED_MONTH = None
    #     CLIM_DATES = [
    #         f"{year}-{month:02d}-01"
    #         for year in range(CLIM_START, CLIM_END + 1)
    #         for month in MONTHS
    #     ]
    #     # TODO - Does not handle year crossing
    #     CUR_DATES = [f"{SEASON_YEAR}-{month:02d}-01" for month in MONTHS]
    #     title = f"# {iso3_dropdown.selected_key}: {SEASON_YEAR} {season_str} Season Overview"
    #     subtitle = "#### ECMWF ERA5 Reanalysis"
    return DATASET, ISSUED_MONTH, MONTHS


@app.cell
def _():
    val_col = "mean"  # or, mean_detrended
    return (val_col,)


@app.cell
def _(
    adm_level,
    admin_filtering,
    df_forecast,
    df_reanalysis,
    get_pop,
    iso3,
    load_codab_from_blob,
    process_season_precip,
    show_current_forecast,
    val_col,
):
    df_display = df_forecast if show_current_forecast else df_reanalysis
    df_pop = get_pop(iso3, adm_level)
    df_display = process_season_precip(df_display, df_pop, adm_level, val_col=val_col)
    gdf = load_codab_from_blob(iso3, adm_level)

    if admin_filtering.value:
        df_display = codab.filter_adm(iso3, adm_level, df_display)
    return df_display, gdf


@app.cell
def _(SEASON_YEAR, df_display, val_col):
    # Get return periods on population exposed per season
    _df = df_display.groupby("year")[[val_col, "pop_lower_tercile"]].sum().reset_index()
    df_annual_sum_precip = rp_calc.calculate_one_group_rp(
        _df, "pop_lower_tercile", ascending=False
    )
    rp = df_annual_sum_precip.loc[df_annual_sum_precip["year"] == SEASON_YEAR][
        "pop_lower_tercile_rp"
    ].values[0]
    pop = df_annual_sum_precip.loc[df_annual_sum_precip["year"] == SEASON_YEAR][
        "pop_lower_tercile"
    ].values[0]
    return df_annual_sum_precip, pop, rp


@app.cell
def _(pop, rp, season_str):
    mo.md(
        f"""**{pop:,}** people are forecasted to experience below average (lower tercile) rainfall during the {season_str} season. We see this level of people in need once every **{rp:.2f}** years. See the plot below to understand how this level of impact compares with previous years. Interpretation of absolute values of seasonal precipitation should be done with caution as forecast and reanalysis products can be subject to significant bias. These precipitation values should instead be interpreted in relative terms."""
    )
    return


@app.cell
def graph_scatter(SEASON_YEAR, df_annual_sum_precip):
    # df_cerf_annual = None
    # if ISO3 == "ETH":
    #     df_cerf = stratus.load_csv_from_blob(
    #         "ds-seasonal-bulletin/misc/CERF Donor Contributions and Allocations - Ethiopia (Drought).csv"
    #     )
    #     df_cerf = df_cerf[df_cerf.Season.str.contains("OND")]
    #     df_cerf["Approved amount in US$"] = pd.to_numeric(
    #         df_cerf["Approved amount in US$"].str.replace(",", ""), errors="coerce"
    #     )
    #     df_cerf_annual = (
    #         df_cerf.groupby("SEASON_YEAR")["Approved amount in US$"].sum().reset_index()
    #     )

    plot.plot_annual_scatter(df_annual_sum_precip, SEASON_YEAR, None)
    return


@app.cell
def _():
    mo.md(r"""## Return periods of rainfall per admin level""")
    return


@app.cell
def _():
    mo.Html("<hr></hr>")
    return


@app.cell
def _():
    mo.md(
        r"""The plot below shows the return periods of total seasonal rainfall per admin level. Admin regions experiencing lower tercile rainfall are highlighted. The total number of people impacted in the section above is the sum of the total population in these highlighted regions."""
    )
    return


@app.cell
def _(val_col):
    map_variable = mo.ui.radio(
        options=["population", f"{val_col}_rp"],
        label="Select variable to display:",
        value=f"{val_col}_rp",
        inline=True,
    )
    map_variable
    return (map_variable,)


@app.cell
def graph_rp(adm_level, df_display, gdf, map_variable, val_col):
    # Prep and plot geodata on map for current return periods
    _df = df_display[df_display.year == df_display.year.max()]

    gdf_merged = gdf.merge(
        _df[["pcode", f"{val_col}_rp", "meets_threshold", "population"]],
        left_on=f"ADM{adm_level}_PCODE",
        right_on="pcode",
        how="right",
    )

    gdf_merged = gdf_merged[
        [
            f"ADM{adm_level}_EN",
            f"ADM{adm_level}_PCODE",
            "pcode",
            f"{val_col}_rp",
            "meets_threshold",
            "population",
            "geometry",
        ]
    ]
    # Simplify geometry for faster visualization
    gdf_merged["geometry"] = gdf_merged["geometry"].simplify(tolerance=0.01)

    plot.plot_map(gdf_merged, adm_level, map_variable.value)
    return (gdf_merged,)


@app.cell
def _(adm_level, gdf_merged, val_col):
    gdf_merged[
        [
            f"ADM{adm_level}_EN",
            "pcode",
            f"{val_col}_rp",
            "meets_threshold",
            "population",
        ]
    ].sort_values(f"{val_col}_rp", ascending=False, axis=0).dropna()
    return


@app.cell
def _():
    mo.Html("<br><br>")
    return


@app.cell
def _():
    mo.md(r"""## Gridded rainfall anomaly""")
    return


@app.cell
def _():
    mo.Html("<hr></hr>")
    return


@app.cell
def _():
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
def _(anom_plot, clim_plot):
    mo.hstack([clim_plot, anom_plot])
    return


@app.cell(hide_code=True)
def _():
    mo.md(r"""## Skill Plot""")
    return


@app.cell
def _():
    high_tercile_selector = mo.ui.checkbox(label="Upper tercile")
    low_tercile_selector = mo.ui.checkbox(label="Lower tercile")

    mo.hstack(
        [
            mo.md("Show tercile boundaries:"),
            mo.vstack([high_tercile_selector, low_tercile_selector], gap=0),
        ],
        align="center",
    )
    return high_tercile_selector, low_tercile_selector


@app.cell
def _(gdf_merged):
    pcodes = dict(zip(gdf_merged["ADM2_EN"], gdf_merged["ADM2_PCODE"]))
    pcode_dropdown = mo.ui.dropdown(options=pcodes, label="Select an admin unit")
    return (pcode_dropdown,)


@app.cell
def _(pcode_dropdown):
    pcode_dropdown
    return


@app.cell
def _(high_tercile_selector, low_tercile_selector):
    show_high_tercile = high_tercile_selector.value
    show_low_tercile = low_tercile_selector.value
    return show_high_tercile, show_low_tercile


@app.cell
def _(adm_level):
    allow_impact = adm_level == 0
    options = ["Flood"] if allow_impact else []

    hazard_dropdown = mo.ui.dropdown(
        options=options,
        label="Display impact data: ",
    )
    hazard_note = "" if allow_impact else "_impact data only available for ADM0_"
    return hazard_dropdown, hazard_note


@app.cell
def _(hazard_dropdown, hazard_note):
    mo.hstack(
        [
            hazard_dropdown,
            mo.md(
                hazard_note,
            ),
        ],
        justify="start",
    )
    return


@app.cell
def _(hazard_dropdown):
    hazard = hazard_dropdown.value
    return (hazard,)


@app.cell
def _():
    min_year_selector = mo.ui.dropdown(
        options=range(1981, 2011),
        allow_select_none=False,
        value=2000,
        label="Start year: ",
    )
    return (min_year_selector,)


@app.cell
def _(min_year_selector):
    min_year = min_year_selector.value
    min_year_note = (
        "_note that impact data before 2000 is not shown_" if min_year < 2000 else ""
    )
    return (min_year,)


@app.cell
def _(df_compare, pcode_dropdown):
    df_compare_sel = df_compare[df_compare.pcode == pcode_dropdown.value]
    return (df_compare_sel,)


@app.cell
def _(
    df_compare_sel,
    hazard,
    iso3,
    issued_month,
    min_year,
    pcode_dropdown,
    show_current_forecast,
    show_high_tercile,
    show_low_tercile,
    valid_mo_str,
):
    adm_name_str = pcode_dropdown.selected_key
    issued_mo_str = calendar.month_abbr[issued_month]
    title = f"{adm_name_str} — $\\bf{{{valid_mo_str}}}$ observed vs. forecasted rainfall\nIssue month: $\\bf{{{issued_mo_str}}}$"

    CERF_ISO3S = ["SSD", "ETH"]

    if hazard == "Flood":
        sizecol = "Total Affected"
        if iso3 in CERF_ISO3S:
            colorcol = "allocation"
        else:
            colorcol = None
    else:
        sizecol, colorcol = None, None

    _fig, _ax = plot.plot_comparison(
        df_compare_sel,
        xcol="mean_detrended_seas5",
        ycol="mean_detrended_era5",
        sizecol=sizecol,
        colorcol=colorcol,
        title=title,
        min_year=min_year,
        show_high_tercile=show_high_tercile,
        show_low_tercile=show_low_tercile,
        show_current_forecast=show_current_forecast,
    )

    _fig
    return


@app.cell
def _(df_compare_sel, min_year):
    if min_year is not None:
        df_ref = df_compare_sel[df_compare_sel["year"] >= min_year]
    else:
        df_ref = df_compare_sel

    df_ref = df_ref.dropna(subset=["mean_detrended_seas5", "mean_detrended_era5"])

    metrics = {}
    metrics.update(
        {
            "corr": df_ref[["mean_detrended_seas5", "mean_detrended_era5"]]
            .corr()
            .iloc[0, 1]
        }
    )

    for _tercile in ["upper", "lower"]:
        q = 2 / 3 if _tercile == "upper" else 1 / 3
        seas5_thresh, era5_thresh = df_ref[
            ["mean_detrended_seas5", "mean_detrended_era5"]
        ].quantile(q)
        if _tercile == "upper":
            pp = df_ref["mean_detrended_seas5"] > seas5_thresh
            p = df_ref["mean_detrended_era5"] > era5_thresh
        else:
            pp = df_ref["mean_detrended_seas5"] < seas5_thresh
            p = df_ref["mean_detrended_era5"] < era5_thresh
        tp = pp & p
        tpr = tp.sum() / p.sum()
        metrics.update({f"{_tercile}_tpr": tpr})
    return df_ref, metrics


@app.cell
def _(df_compare_sel, df_ref, show_current_forecast):
    rps = {}

    if show_current_forecast:
        df_rp_calc = df_ref.copy()
        forecast_year = df_compare_sel["year"].max()
        current_val = df_compare_sel.set_index("year").loc[forecast_year][
            "mean_detrended_seas5"
        ]
        for _tercile in ["upper", "lower"]:
            df_rp_calc = rp_calc.calculate_one_group_rp(
                df_rp_calc,
                col_name="mean_detrended_seas5",
                ascending=_tercile == "lower",
            )
            df_rp_calc = df_rp_calc.sort_values("mean_detrended_seas5")
            _rp = np.interp(
                current_val,
                df_rp_calc["mean_detrended_seas5"],
                df_rp_calc["mean_detrended_seas5_rp"],
            )
            rps.update({_tercile: _rp})
    return (rps,)


@app.cell
def _(rps, show_current_forecast):
    if show_current_forecast:
        rp_table_str = f"""
        | Upper | Lower |
        |-|-|
        | {rps["upper"]:.1f} years | {rps["lower"]:.1f} years|
        """
    else:
        rp_table_str = "No RP shown as forecast no longer relevant"
    return (rp_table_str,)


@app.cell
def _(metrics, rp_table_str):
    mo.md(
        f"""
    ### Return Period

    {rp_table_str}

    ### Accuracy Metrics

    | Correlation | Upper tercile F1 | Lower tercile F1 |
    |-|-|-|
    | {metrics["corr"]:.2f} | {metrics["upper_tpr"]:.2f} | {metrics["lower_tpr"]:.2f} |
    """
    )
    return


@app.cell
def _():
    mo.md(
        r"""
    ### Notes

    #### Plot
    - The year shown is the year of the _first valid_ month. For example, a forecast issued in Nov 2025 would appear as the year:
        - 2026 if it is for JFM
        - 2025 it if is for DJF
    - If the full reanalysis for the relevant valid months is available, the most recent forecast will not be shown as a vertical line.
    - The shaded zones at the top, bottom, left, or right of the plots correspond to the upper or lower terciles of the distribution for the reanalysis and reforecast respectively. The tercile boundaries are calculated empricially using only the years shown on the plot (i.e. the years since "Start Year").
    - Both the reanalysis and reforecast have been de-trended (based on the full reference period since 1981), using a linear curve fit.
    - _[Flood only]_ The size of the bubbles corresponds to the total impact from "Flood" events in the EM-DAT database during that year. The legend shows the size of the largest bubble, and the corresponding maximum impact.
    - _[Flood only]_ Bubbles in red denote years with at least one "Rapid Response" CERF allocation for a "Flood" during that year. **Note that this has only been added for Ethiopia and South Sudan so far, all other countries will just show "pre-CERF".**

    #### Return period

    - Return periods are calculated empirically.
    - The "upper" return period indicates on average how often a value this high or higher is forecasted (for example, this is appropriate for predicting flood risk).
    - The "lower" return period indicates on average how often a value this low or lower is forecasted (for example, this is appropriate for predicting drought).

    #### Accuracy metrics

    - For the correlation, values less than 0 are **worse than random**
    - For the F1 score, values less than 0.33 are **worse than random**, because the threshold is the tercile boundary.
    - F1 scores are calculated based on predictions and observations in the respective tercile. Because tercile thresholds are set for both the forecast and the reanalysis, there will be the same number of _predicted positive_ and _positive_ years. Thus by definition the F1 score will be the same as the TPR and PPV.
    - For standard accuracy metric defitions see the table [here](https://en.wikipedia.org/wiki/Confusion_matrix).
    """
    )
    return


@app.cell
def _():
    ### Seasonal rainfall

    # max_full_year = df_era5_all["valid_date"].dt.year.max() - 1
    # df_era5_monthly = (
    #     df_era5_all[df_era5_all["valid_date"].dt.year <= max_full_year]
    #     .groupby(df_era5_all["valid_date"].dt.month)["mean"]
    #     .mean()
    #     .reset_index()
    # )
    # df_era5_monthly["valid_month_str"] = df_era5_monthly["valid_date"].apply(
    #     lambda x: calendar.month_abbr[x]
    # )
    return


@app.cell
def _():
    # _fig, _ax = plt.subplots(dpi=200)
    # df_era5_monthly.plot.bar(
    #     x="valid_month_str", y="mean", legend=False, ax=_ax, color="royalblue"
    # )
    # _ax.set_xlabel("Month")
    # _ax.set_ylabel("Mean daily rainfall per month (mm) [ERA5]")
    # _ax.set_title(
    #     f"{adm_name_str}: precipitation seasonality\n(reference period: {df_era5_all['valid_date'].dt.year.min()}-{max_full_year})"
    # )
    # _ax.spines["top"].set_visible(False)
    # _ax.spines["right"].set_visible(False)
    # _ax
    return


if __name__ == "__main__":
    app.run()
