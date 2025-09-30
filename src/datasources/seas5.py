from src.utils import rp_calc


def process_seas5(df_seas5, months, issued_month):

    _df_1 = df_seas5.copy()
    # Convert dates and create month/year columns
    _df_1["year"] = _df_1["valid_date"].dt.year
    _df_1["valid_month"] = _df_1["valid_date"].dt.month
    _df_1["issued_month"] = _df_1["issued_date"].dt.month

    # Filter to only the forecasts from the selected issue_month
    # and from the same season
    _df_1 = _df_1[_df_1["valid_month"].isin(months)]
    _df_1 = _df_1[_df_1["issued_month"] == issued_month]

    assert list(_df_1["valid_month"].unique()) == months

    # Transform from mm/day to mm/season
    # NOTE: DOESN'T HANDLE SEASONS THAT CROSS DEC-JAN
    _df_1["days_in_month"] = _df_1["valid_date"].dt.days_in_month
    _df_1["mm_month"] = _df_1["mean"] * _df_1["days_in_month"]
    _df_2 = (
        _df_1.groupby(["pcode", "year"])
        .agg({"mm_month": lambda x: x.sum()})
        .reset_index()
    )
    _df_2.rename(columns={"mm_month": "total_rainfall"}, inplace=True)

    # Identify cases in the lower tercile
    _df_2["lower_tercile_threshold"] = _df_2.groupby("pcode")[
        "total_rainfall"
    ].transform(lambda x: x.quantile(1 / 3))
    _df_2["is_lower_tercile"] = (
        _df_2["total_rainfall"] <= _df_2["lower_tercile_threshold"]
    )

    # Calculate return period
    _df_2 = rp_calc.calculate_groups_rp(_df_2, "pcode", "total_rainfall")

    return _df_2
