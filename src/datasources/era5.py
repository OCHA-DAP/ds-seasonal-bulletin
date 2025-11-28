from typing import List

import ocha_stratus as stratus
import pandas as pd

from src.utils.timeseries import detrend_column


def get_season_stats(iso3, adm_level, valid_months, stage="prod"):
    valid_months_str = ",".join(map(str, valid_months))
    engine = stratus.get_engine(stage)
    with engine.connect() as conn:
        df = pd.read_sql(
            f"""select *
            from era5
            where iso3='{iso3}'
            and adm_level={adm_level}
            and extract(month from valid_date) in ({valid_months_str})
            """,
            con=conn,
            parse_dates=["valid_date"],
        )
    return df


def total_seasonal_precip(df):
    _df = df.copy()
    # TODO: Handle Dec - Jan crossing
    _df["season"] = _df["valid_date"].dt.year
    _df["sum_month"] = _df["sum"] * _df["valid_date"].dt.days_in_month
    _df2 = (
        _df.groupby(["pcode", "season"])
        .agg({"sum_month": lambda x: x.sum()})
        .reset_index()
    )
    _df2 = _df2.rename(columns={"sum_month": "sum_season"})
    return _df2


def load_era5(
    pcode: str,
    valid_months: List[int] = None,
):
    if valid_months is None:
        valid_months = range(1, 13)

    query = """
    SELECT *
    FROM public.era5
    WHERE pcode = %s
      AND EXTRACT(MONTH FROM valid_date) IN %s
    """
    engine = stratus.get_engine("prod")
    with engine.connect() as conn:
        df = pd.read_sql(
            query,
            conn,
            params=(pcode, tuple(valid_months)),
            parse_dates=["valid_date"],
        )
    return df


def aggregate_era5_yearly(
    df: pd.DataFrame,
    valid_months: List[int],
):
    df_monthly = df[df["valid_date"].dt.month.isin(valid_months)].copy()
    df_monthly["year"] = df_monthly["valid_date"].dt.year
    df_monthly["month"] = df_monthly["valid_date"].dt.month

    # Ensure each year has *all* valid months
    complete_years = (
        df_monthly.groupby("year")["month"]
        .nunique()
        .loc[lambda x: x == len(valid_months)]
        .index
    )
    df_complete = df_monthly[df_monthly["year"].isin(complete_years)]

    if 1 in valid_months and 12 in valid_months:

        def shift_valid_year(row):
            year = row["year"]
            return year if row["month"] >= 7 else year - 1

        df_complete["season_year"] = df_complete.apply(shift_valid_year, axis=1)
    df_yearly = df_complete.groupby("year")["mean"].mean().reset_index()
    df_yearly = detrend_column(df_yearly, "mean", index_col="year")
    return df_yearly
