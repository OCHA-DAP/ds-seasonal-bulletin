import pandas as pd
import xarray as xr
from calendar import monthrange

def process_cogs(da_clim, da_cur, months, issued_month=None, season_year=None):
    """
    Compute total seasonal precipitation for forecast or observational data.

    Parameters
    ----------
    da_clim : xr.DataArray
        Climatological/historical data.
        - Forecasts: (date, leadtime, y, x)
        - Observations: (date, y, x)
    da_cur : xr.DataArray
        Current forecast or recent observation.
    months : list[int]
        List of months in the target season (e.g., [10, 11, 12]).
    issued_month : int, optional
        Issuance month (required for forecast data).
    season_year : int, optional
        Year of the forecast (required for forecast data).

    Returns
    -------
    avg : xr.DataArray
        Climatological average total seasonal precipitation.
    cur : xr.DataArray
        Current total seasonal precipitation.
    """
    is_forecast = "leadtime" in da_clim.dims or "leadtime" in da_cur.dims

    if is_forecast:
        if issued_month is None or season_year is None:
            raise ValueError("issued_month and season_year are required for forecast data.")

        # Map each valid month to its leadtime offset
        leadtimes = [m - issued_month for m in months]
        leadtimes = [lt if lt >= 0 else lt + 12 for lt in leadtimes]

        # Function to aggregate forecast data
        def _process_forecast(da):
            da_sel = da.sel(leadtime=leadtimes)
            days_in_month = monthrange(season_year, issued_month)[1]
            da_weighted = da_sel * days_in_month
            years = pd.to_datetime(da_weighted.date.values).year
            da_with_year = da_weighted.assign_coords(year=("date", years))
            da_yearly = da_with_year.groupby("year").sum(dim=["leadtime"])
            return da_yearly

        # Process climatology
        da_clim_proc = _process_forecast(da_clim)
        avg = da_clim_proc.mean(dim="date")

        # Process current forecast
        da_cur_proc = _process_forecast(da_cur)
        cur = da_cur_proc.mean(dim="date")

    else:
        # --- Observational case ---
        def _process_obs(da):
            da = da.copy()
            da["date"] = pd.to_datetime(da["date"].values)
            da_weighted = da * xr.DataArray(da["date"].dt.days_in_month, dims="date")
            da_sel = da_weighted.sel(date=da_weighted["date"].dt.month.isin(months))
            da_with_year = da_sel.assign_coords(year=("date", da_sel["date"].dt.year.data))
            da_yearly = da_with_year.groupby("year").sum(dim="date")
            return da_yearly

        da_clim_proc = _process_obs(da_clim)
        avg = da_clim_proc.mean(dim="year")

        da_cur_proc = _process_obs(da_cur)
        cur = da_cur_proc.sel(year=da_cur_proc.year.max())

    return avg, cur
