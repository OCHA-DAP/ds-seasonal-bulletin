import pandas as pd
from dateutil.relativedelta import relativedelta

def _reshape_forecast(da):
    """Switch to using valid_date instead of the issued_date in the forecast data. Output should have x, y, and date coords."""
    da_ = da.copy()
    valid_dates = [pd.to_datetime(date) + relativedelta(months=leadtime) 
        for date in da_.date.values 
        for leadtime in da_.leadtime]

    # Stack the date and leadtime dimensions into a single dimension
    da_stacked = da_.stack(valid_date=('date', 'leadtime'))
    # Replace the multi-index with the computed valid_dates
    da_stacked = da_stacked.drop_vars(['date', 'leadtime']).assign_coords(valid_date=valid_dates)
    # Now rename back to date
    da_stacked = da_stacked.rename({'valid_date': 'date'})

    return da_stacked

def _parse_date(da):
    """Set month and year indices instead of date"""
    da_ = da.copy()
    da_['year'] = ('date', da_['date'].dt.year.values)
    da_['month'] = ('date', da_['date'].dt.month.values)
    da_reshaped = da_.set_index(date=['year', 'month']).unstack('date')
    return da_reshaped

def summarize_season(da, dataset, months):
    """
    Flatten an input xarray object to get the total seasonal 
    precipitation, averaged across all years
    """
    if dataset == "forecast":
        _da = _reshape_forecast(da)
    else:
        _da = da
        
    _da['date'] = _da['date'].astype('datetime64[ns]')
    _da = _da * _da['date'].dt.days_in_month     
    _da = _parse_date(_da)
    # Only get selected months
    _da = _da.sel(month=months)
    # Sum across all months, then take the average across all years
    return _da.sum(dim="month").mean(dim="year")