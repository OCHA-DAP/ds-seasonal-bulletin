import pandas as pd
from typing import List


def calculate_one_group_rp(group, col_name: str = "q", ascending: bool = True):
    """Calculate the empirical RP for a single group.

    Parameters
    ----------
    group : pd.DataFrame
        The group for which to calculate the RP.
    col_name : str, optional
        The name of the column for which to calculate the RP, by default "q".
    ascending : bool, optional
        Whether to rank the column in ascending order, by default True.
        Should be False for cases where a high number is severe
        (e.g. precipitation for flooding), and True for cases where a low
        number is severe (e.g. precipitation for drought).

    Returns
    -------
    pd.DataFrame
        The input group with the RP columns added.
    """
    _df = group.copy()
    _df[f"{col_name}_rank"] = _df[col_name].rank(ascending=ascending)
    _df[f"{col_name}_rp"] = (len(_df) + 1) / _df[f"{col_name}_rank"]
    return _df


def calculate_groups_rp(
    df: pd.DataFrame, by: List, col_name: str = "mean", ascending: bool = True
):
    """Calculate the empirical RP for each group in a DataFrame.

    Parameters
    ----------
    df : pd.DataFrame
        The DataFrame for which to calculate the RP.
    by : List
        The columns by which to group the DataFrame.

    Returns
    -------
    pd.DataFrame
        The input DataFrame with the RP columns added.
    """
    _df = df.copy()
    return (
        _df.groupby(by)
        .apply(
            calculate_one_group_rp,
            include_groups=False,
            col_name=col_name,
            ascending=ascending,
        )
        .reset_index()
        # .drop(columns="level_1")
    )


def classify_groups_quantile(df, column, q=0.33, condition="below"):
    _df = df.copy()
    _df["q_threshold"] = _df.groupby("pcode")[column].transform(lambda x: x.quantile(q))
    _df["meets_threshold"] = (
        (_df[column] <= _df["q_threshold"])
        if condition == "below"
        else (_df[column] >= _df["q_threshold"])
    )
    return _df
