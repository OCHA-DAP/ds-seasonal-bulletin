import ocha_stratus as stratus


def filter_adm(iso3, adm_level, df_filter):
    fname = f"ds-seasonal-bulletin/harmonic_seasonality/{iso3.lower()}_adm{adm_level}_seasonality.csv"
    _df = df_filter.copy()
    try:
        df_seasonality = stratus.load_csv_from_blob(fname, stage="dev")
        filter_pcodes = list(
            df_seasonality[df_seasonality.cluster == 1][f"ADM{adm_level}_PCODE"]
        )
        _df = _df[
            _df.pcode.isin(filter_pcodes)
        ]
        return _df
    except Exception as e:
        print(e)
        print("Error reading seasonality file! Not filtering locations")
        return df_filter
    