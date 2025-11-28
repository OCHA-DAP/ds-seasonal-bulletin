from typing import List

import ocha_stratus as stratus
import pandas as pd


def get_season_stats(iso3, adm_level, issued_month, valid_months, stage="prod"):
    valid_months_str = ",".join(map(str, valid_months))
    engine = stratus.get_engine(stage)
    with engine.connect() as conn:
        df = pd.read_sql(
            f"""select *
            from seas5
            where iso3='{iso3}'
            and adm_level={adm_level}
            and extract(month from issued_date)={issued_month}
            and extract(month from valid_date) in ({valid_months_str})
            """,
            con=conn,
            parse_dates=["valid_date", "issued_date"],
        )
    return df


def total_seasonal_precip(df):
    _df = df.copy()
    _df["season"] = _df.groupby("issued_date")["valid_date"].transform(
        lambda x: x.dt.year.min()
    )
    _df["sum_month"] = _df["sum"] * _df["valid_date"].dt.days_in_month
    _df2 = (
        _df.groupby(["pcode", "season"])
        .agg({"sum_month": lambda x: x.sum()})
        .reset_index()
    )
    _df2 = _df2.rename(columns={"sum_month": "sum_season"})
    return _df2


def load_seas5(
    pcode: str,
    issued_months: List[int] = None,
    valid_months: List[int] = None,
):
    if issued_months is None:
        issued_months = range(1, 13)  # Default to all months
    if valid_months is None:
        valid_months = range(1, 13)

    query = """
    SELECT *
    FROM public.seas5
    WHERE pcode = %s
      AND EXTRACT(MONTH FROM issued_date) IN %s
      AND EXTRACT(MONTH FROM valid_date) IN %s
    """
    engine = stratus.get_engine("prod")
    with engine.connect() as conn:
        df = pd.read_sql(
            query,
            conn,
            params=(pcode, tuple(issued_months), tuple(valid_months)),
            parse_dates=["valid_date", "issued_date"],
        )
    return df


def aggregate_seas5_yearly(
    df: pd.DataFrame,
    issued_month: int,
    valid_months: List[int],
):
    df_monthly = df[
        (df["issued_date"].dt.month == issued_month)
        & (df["valid_date"].dt.month.isin(valid_months))
    ]
    df_yearly = (
        df_monthly.groupby(df_monthly["issued_date"].dt.year)["mean"]
        .mean()
        .reset_index()
    )
    df_yearly = df_yearly.rename(columns={"issued_date": "year"})
    # max_year = df_yearly["year"].max()
    # df_yearly = detrend_column(
    #     df_yearly, "mean", index_col="year", max_index=max_index
    # )
    return df_yearly
