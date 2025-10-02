import ocha_stratus as stratus
import pandas as pd


def get_season_stats(iso3, adm_level, issued_month, valid_months, stage="prod"):
    valid_months_str = ','.join(map(str, valid_months))
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
    _df["season"] =  _df.groupby('issued_date')['valid_date'].transform(lambda x: x.dt.year.min()) 
    # TODO: Switch from 'mean' to 'sum'
    _df["sum_month"] = _df["mean"] * _df["valid_date"].dt.days_in_month
    _df2 = (
        _df.groupby(["pcode", "season"])
        .agg({"sum_month": lambda x: x.sum()})
        .reset_index()
    )
    _df2 = _df2.rename(columns={"sum_month": "sum_season"})
    return _df2

def classify_groups_quantile(df, column, q=0.33, condition="below"):
    _df = df.copy()
    _df["q_threshold"] = _df.groupby("pcode")[column].transform(lambda x: x.quantile(q))
    _df["meets_threshold"] = (_df[column] <= _df["q_threshold"]) if condition == "below" else (_df[column] >= _df["q_threshold"])
    return _df
