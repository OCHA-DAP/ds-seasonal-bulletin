import numpy as np
import plotly.express as px
import plotly.graph_objects as go
import matplotlib.pyplot as plt

TOMATO = "#f2645a"
SAPPHIRE = "#007ce0"
SAPPHIRE_LIGHT = "#66b0ec"
MED_GRAY = "#cccccc"
LIGHT_GRAY = "#eeeeee"
DARK_GRAY = "#888888"


def plot_map(gdf, adm_level, variable, val_col):
    if variable == "population":
        color_scale = "Blues"
        color_range = [gdf.population.min(), gdf.population.max()]
    else:
        color_scale = "Reds"
        color_range = [1, 50]  # Because we have 45 seasons
    fig = px.choropleth_map(
        gdf,
        geojson=gdf.geometry,
        locations=gdf.index,
        color=variable,
        color_continuous_scale=color_scale,
        range_color=color_range,
        map_style="carto-voyager-nolabels",
        center={
            "lat": gdf.geometry.centroid.y.mean(),
            "lon": gdf.geometry.centroid.x.mean(),
        },
        zoom=4.5,
        opacity=0.9,
        labels={f"{val_col}_rp": "Return Period<br>(years)"},
        custom_data=[
            f"ADM{adm_level}_EN",
            f"{val_col}_rp",
            "meets_threshold",
            "population",
        ],
    )

    # Update hover template
    fig.update_traces(
        hovertemplate="<b>%{customdata[0]}</b><br>"
        + "RP: %{customdata[1]:.1f}<br>"
        + "Below average: %{customdata[2]}<br>"
        + "Population: %{customdata[3]:,}"
        + "<extra></extra>",
        selector=dict(type="choroplethmap"),
    )

    # Add red outlines with legend (only if there are lower tercile regions)
    lower_tercile_data = gdf[gdf["meets_threshold"]]

    if not lower_tercile_data.empty:
        first = True
        for _, row in lower_tercile_data.iterrows():
            polys = (
                [row.geometry]
                if row.geometry.geom_type == "Polygon"
                else list(row.geometry.geoms)
            )
            for poly in polys:
                x, y = poly.exterior.xy
                fig.add_trace(
                    go.Scattermap(
                        lon=list(x),
                        lat=list(y),
                        mode="lines",
                        line=dict(color="red", width=2),
                        name="Lower tercile rainfall",
                        showlegend=first,
                        hoverinfo="skip",
                    )
                )
                first = False

        # Add grey choropleth for null values
    null_data = gdf[gdf[f"{val_col}_rp"].isna()]

    if not null_data.empty:
        fig.add_trace(
            go.Choroplethmap(
                geojson=null_data.__geo_interface__,
                locations=null_data.index,
                z=[0] * len(null_data),
                colorscale=[[0, MED_GRAY], [1, MED_GRAY]],
                showscale=False,
                name="No Data",
                hovertemplate="<b>%{customdata[0]}</b><br>No data available<extra></extra>",
                customdata=null_data[[f"ADM{adm_level}_EN"]].values,
            )
        )

    fig.update_layout(
        margin=dict(l=0, r=0, t=40, b=0),
        title="<b>Rainfall Return Periods</b>",
        legend=dict(
            orientation="v",
            yanchor="top",
            y=0.99,
            xanchor="left",
            x=0.01,
            bgcolor="rgba(255,255,255,0.8)",
            itemsizing="constant",
            tracegroupgap=10,  # Add space between legend items
        ),
        coloraxis_colorbar=dict(
            x=0.01,  # Move colorbar to the right to avoid legend
            len=0.5,
            yanchor="middle",
            y=0.65,
            bgcolor="rgba(255,255,255,0.8)",
        ),
    )

    return fig


def plot_annual_scatter(
    df_annual_summary,
    highlight_year,
    val_col,
    df_cerf_annual=None,
):

    _df = df_annual_summary.copy()
    _df = _df[_df.year >= 2000]

    if df_cerf_annual is not None:
        _df = _df.merge(
            df_cerf_annual.rename(columns={"SEASON_YEAR": "year"}),
            on="year",
            how="left",
        )
        _df["has_cerf"] = ~_df["Approved amount in US$"].isna()
        # Scale size based on funding amount (you may need to adjust the scaling)
        _df["marker_size"] = (
            _df["Approved amount in US$"].fillna(0) / 2000000 + 8
        )  # Base size of 8
    else:
        _df["has_cerf"] = False
        _df["marker_size"] = 8

    def get_color(row):
        if row["has_cerf"]:
            return SAPPHIRE_LIGHT
        elif row["year"] == highlight_year:
            return TOMATO
        else:
            return SAPPHIRE

    _df["point_color"] = _df.apply(get_color, axis=1)  # Change to apply on rows

    # Create figure
    fig = go.Figure()

    # Add all points
    fig.add_trace(
        go.Scatter(
            x=_df[val_col],
            y=_df["pop_lower_tercile"],
            mode="markers",
            marker=dict(color=_df["point_color"], size=_df["marker_size"]),  # Add size
            showlegend=False,
            hovertemplate="Year: %{text}<br>Rainfall: %{x:,.0f} mm<br>Population affected: %{y:,.0f}<extra></extra>",
            text=_df["year"],
        )
    )

    years_to_label = [highlight_year]
    if df_cerf_annual is not None:
        years_to_label.extend(_df[_df["has_cerf"]]["year"].tolist())

    df_labeled = _df[_df["year"].isin(years_to_label)]

    # Create text with bold formatting for highlight year
    def get_label_text(row):
        if row["year"] == highlight_year:
            return f"<b>{row['year']}</b>"
        else:
            return str(row["year"])

    df_labeled_text = df_labeled.apply(get_label_text, axis=1)
    df_labeled["y_offset"] = df_labeled["pop_lower_tercile"] + (
        df_labeled["pop_lower_tercile"] * 0.05
    )  # 5% offset

    fig.add_trace(
        go.Scatter(
            x=df_labeled[val_col],
            y=df_labeled["y_offset"],
            mode="text",
            text=df_labeled_text,
            textposition="top center",
            textfont=dict(size=12, color=df_labeled["point_color"]),
            showlegend=False,
            hoverinfo="skip",
            cliponaxis=False,  # Allow text to extend beyond plot area
        )
    )

    # Format y-axis with millions
    fig.update_yaxes(tickformat=".2s")  # This will show as 1M, 2M, etc.

    fig.update_layout(
        title=f"<b>Total seasonal rainfall vs est. population impacted by drought</b><br><sub>From 2000 to {highlight_year}</sub>",
        margin=dict(l=0, r=0, t=50, b=0),
        xaxis=dict(
            title="Mean daily rainfall (mm)",
            showgrid=False,
        ),
        yaxis=dict(
            title="Est. Population Impacted by Drought",
            showgrid=True,
            gridcolor=LIGHT_GRAY,
            gridwidth=0.5,
            zeroline=True,
            zerolinecolor=MED_GRAY,
        ),
        plot_bgcolor="white",
        height=400,
        hovermode="closest",
    )

    return fig


def plot_anomaly(ds, gdf):
    anom_clipped = ds.rio.clip(gdf.geometry.values, gdf.crs, drop=True)

    # 2. Balance the color scale around zero
    vmax = np.abs(anom_clipped).max().values.item()
    vmin = -vmax

    # 3. Create the plot with balanced colors and no axis labels
    fig = px.imshow(
        anom_clipped,
        origin="lower",
        color_continuous_scale="RdBu",
        zmin=vmin,
        zmax=vmax,
        aspect="equal",
    )

    # Remove axis labels and ticks
    fig.update_xaxes(
        showticklabels=False,
        title="",
        showgrid=False,
        zeroline=False,
        showline=False,
        ticks="",
    )
    fig.update_yaxes(
        showticklabels=False,
        title="",
        showgrid=False,
        zeroline=False,
        showline=False,
        ticks="",
    )
    fig.update_coloraxes(
        colorbar=dict(
            title="Rainfall anomaly<br>(avg mm/day)",
        )
    )
    fig.update_layout(
        template="simple_white",
        title="Anomalous precipitation",
        width=500,
        margin=dict(l=0, r=0, t=40, b=0),
    )

    return fig


def plot_climatology(ds, gdf):
    clim_clipped = ds.rio.clip(gdf.geometry.values, gdf.crs, drop=True)

    fig = px.imshow(
        clim_clipped,
        origin="lower",
        color_continuous_scale="Blues",
        aspect="equal",
    )

    # Remove axis labels and ticks
    fig.update_xaxes(
        showticklabels=False,
        title="",
        showgrid=False,
        zeroline=False,
        showline=False,
        ticks="",
    )
    fig.update_yaxes(
        showticklabels=False,
        title="",
        showgrid=False,
        zeroline=False,
        showline=False,
        ticks="",
    )
    fig.update_coloraxes(
        colorbar=dict(
            title="Seasonal rainfall<br>(avg mm/day)",
        )
    )
    fig.update_layout(
        template="simple_white",
        title="Average historical precipitation",
        width=500,
        margin=dict(l=0, r=0, t=40, b=0),
    )

    return fig


def plot_comparison(
    df,
    xcol: str,
    ycol: str,
    colorcol: str = None,
    sizecol: str = None,
    rotation: int = 0,
    min_year: int = None,
    title: str = None,
    show_high_tercile: bool = False,
    show_low_tercile: bool = False,
    show_current_forecast: bool = True,
):

    tercile_colors = {"upper": "royalblue", "lower": "chocolate"}
    current_color = "mediumorchid"
    cerf_color_mapping = {
        "Yes": "crimson",
        "No": "k",
        "pre-CERF": "#595959",
        np.nan: "k",
    }

    col_to_label = {
        "mean_detrended_seas5": "Forecasted mean daily rainfall (mm) [SEAS5]",
        "mean_detrended_era5": "Observed mean daily rainfall (mm) [ERA5]",
    }

    _fig, _ax = plt.subplots(dpi=200, figsize=(4, 4))
    if min_year is not None:
        df = df[df["year"] >= min_year]
    df = df.copy()
    xmax, ymax = df[[xcol, ycol]].max()
    xmin, ymin = df[[xcol, ycol]].min()
    padding = 0.1
    xrange = xmax - xmin
    yrange = ymax - ymin
    xlim = (xmin - padding * xrange, xmax + padding * xrange)
    ylim = (ymin - padding * yrange, ymax + padding * yrange)
    if show_high_tercile and show_low_tercile:
        tercile_alpha = 0.05
    else:
        tercile_alpha = 0.1

    def show_tercile(level):
        df_ref = df.dropna(subset=[xcol, ycol])
        q = 2 / 3 if level == "upper" else 1 / 3
        x_thresh, y_thresh = df_ref[[xcol, ycol]].quantile(q)
        color = tercile_colors[level]
        _ax.axvspan(
            xmin=x_thresh if level == "upper" else xlim[0],
            xmax=xlim[1] if level == "upper" else x_thresh,
            facecolor=color,
            alpha=tercile_alpha,
            zorder=-2,
        )
        _ax.annotate(
            f"  {level} tercile",
            (x_thresh, ylim[0]),
            color=color,
            zorder=-1,
            fontsize=8,
            rotation=90,
            fontstyle="italic",
            alpha=0.5,
            ha="left" if level == "upper" else "right",
        )
        _ax.axhspan(
            ymin=y_thresh if level == "upper" else ylim[0],
            ymax=ylim[1] if level == "upper" else y_thresh,
            facecolor=color,
            alpha=tercile_alpha,
            zorder=-2,
        )
        _ax.annotate(
            f"  {level} tercile",
            (xlim[0], y_thresh),
            color=color,
            zorder=-1,
            fontsize=8,
            fontstyle="italic",
            alpha=0.5,
            va="bottom" if level == "upper" else "top",
        )

    if show_high_tercile:
        show_tercile("upper")
    if show_low_tercile:
        show_tercile("lower")

    max_bubble_size = 2000
    if sizecol is None:
        sizes = np.full(len(df), 0)
        max_size_value = None
    else:
        sizes = df[sizecol].fillna(0) / df[sizecol].max() * max_bubble_size
        max_size_value = df[sizecol].max()
    if colorcol is None:
        df["color"] = "k"
    else:
        df["color"] = df[colorcol].map(cerf_color_mapping)
    _ax.scatter(
        df[xcol],
        df[ycol],
        s=sizes,
        c=df["color"],
        alpha=0.3,
        edgecolor="none",
        zorder=2,
    )
    for year, row in df.set_index("year").iterrows():
        _ax.annotate(
            str(year),
            (row[xcol], row[ycol]),
            fontsize=8,
            ha="center",
            va="center",
            color=row["color"],
            rotation=rotation,
            zorder=3,
        )
    # if show_current_forecast and "seas5" in xcol:
    if show_current_forecast:
        # if 2025 in df["year"].to_list():
        forecast_year = df["year"].max()
        current_val = df.set_index("year").loc[forecast_year][xcol]
        _ax.axvline(current_val, color=current_color, linestyle="--", zorder=-1)
        _ax.annotate(
            f" {forecast_year} forecast",
            (current_val, ylim[0]),
            rotation=90,
            va="bottom",
            ha="right",
            color=current_color,
            zorder=-1,
            fontstyle="italic",
        )
    _ax.set_xlabel(col_to_label.get(xcol, xcol))
    _ax.set_ylabel(col_to_label.get(ycol, ycol))
    if title is not None:
        _ax.set_title(title)
    _ax.spines["top"].set_visible(False)
    _ax.spines["right"].set_visible(False)
    _ax.set_xlim(xlim)
    _ax.set_ylim(ylim)

    if sizecol is not None or colorcol is not None:

        def get_legend_y(row_num):
            return ylim[1] - yrange * 0.04 - yrange * row_num * 0.03

        legend_x = xlim[0] + xrange * 0.18

        def plot_legend_box(xstart, xwidth):
            rect = mpatches.Rectangle(
                (xstart, get_legend_y(4.7)),
                xwidth,
                yrange * 0.16,
                linewidth=0.5,
                color="white",
                zorder=0,
                alpha=0.5,
            )
            _ax.add_patch(rect)

    if colorcol is not None:
        _ax.annotate(
            "CERF allocation:",
            (legend_x, get_legend_y(0)),
            va="top",
            fontstyle="italic",
            fontsize=6,
        )
        for i, (label, color) in enumerate(cerf_color_mapping.items()):
            if str(label) == "nan":
                continue
            _ax.annotate(
                label,
                (legend_x, get_legend_y(i + 1)),
                va="top",
                color=color,
                fontsize=6,
            )
        plot_legend_box(legend_x, xrange * 0.16)
    if sizecol is not None:
        x_legend_bubble = legend_x - xrange * 0.08
        y_legend_bubble = get_legend_y(2)
        _ax.scatter(
            [x_legend_bubble],
            [y_legend_bubble],
            s=[max_bubble_size],
            facecolor="none",
            edgecolor="k",
            linewidth=0.5,
        )
        _ax.annotate(
            f"{sizecol}:\n{max_size_value:,.0f}",
            (x_legend_bubble, y_legend_bubble),
            ha="center",
            va="center",
            fontstyle="italic",
            fontsize=6,
        )
        plot_legend_box(legend_x - xrange * 0.16, xrange * 0.16)
        # rect = mpatches.Rectangle(
        #     (legend_x - xrange * 0.16, get_legend_y(4.7)),
        #     xrange * 0.32,
        #     yrange * 0.16,
        #     linewidth=0.5,
        #     color="white",
        #     zorder=0,
        #     alpha=0.5,
        # )
        # _ax.add_patch(rect)
    return (_fig, _ax)
