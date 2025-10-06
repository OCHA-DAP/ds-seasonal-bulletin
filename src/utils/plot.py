import plotly.graph_objects as go
import plotly.express as px

TOMATO = "#f2645a"
SAPPHIRE = "#007ce0"
SAPPHIRE_LIGHT = "#66b0ec"
MED_GRAY = "#cccccc"
LIGHT_GRAY = "#eeeeee"
DARK_GRAY = "#888888"


def plot_map(gdf, adm_level, variable):
    if variable == "population": 
        color_scale = "Blues"
        color_range = [gdf.population.min(), gdf.population.max()]
    elif variable == "sum_season_rp":
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
        labels={"sum_season_rp": "Return Period<br>(years)"},
        custom_data=[
            f"ADM{adm_level}_EN",
            "sum_season_rp",
            "meets_threshold",
            "population"
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
    lower_tercile_data = gdf[gdf["meets_threshold"] == True]

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
    null_data = gdf[gdf["sum_season_rp"].isna()]

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
    reference_years,
    highlight_year,
):

    _df = df_annual_summary.copy()
    _df = _df[_df.season >= 2000]

    # Assign colors based on year type
    def get_color(y):
        if y == highlight_year:
            return TOMATO
        elif y in reference_years:
            return SAPPHIRE_LIGHT
        else:
            return SAPPHIRE

    _df["point_color"] = _df["season"].apply(get_color)

    # Create figure
    fig = go.Figure()

    # Add all points
    fig.add_trace(
        go.Scatter(
            x=_df["sum_season"],
            y=_df["pop_lower_tercile"],
            mode="markers",
            marker=dict(color=_df["point_color"], size=8),
            showlegend=False,
            hovertemplate="Year: %{text}<br>Rainfall: %{x:,.0f} mm<br>Population affected: %{y:,.0f}<extra></extra>",
            text=_df["season"],
        )
    )

    # Add labels for reference and highlight years
    years_to_label = reference_years + [highlight_year]
    df_labeled = _df[df_annual_summary["season"].isin(years_to_label)]

    fig.add_trace(
        go.Scatter(
            x=df_labeled["sum_season"],
            y=df_labeled["pop_lower_tercile"],
            mode="text",
            text=df_labeled["season"],
            textposition="top center",
            textfont=dict(size=12, color=df_labeled["point_color"]),
            showlegend=False,
            hoverinfo="skip",
        )
    )

    # Format y-axis with millions
    fig.update_yaxes(tickformat=".2s")  # This will show as 1M, 2M, etc.

    fig.update_layout(
        title=f"<b>Total seasonal rainfall vs est. population impacted by drought</b><br><sub>From 2000 to {highlight_year}</sub>",
        margin=dict(l=0, r=0, t=50, b=0),
        xaxis=dict(
            title="Total Rainfall (mm)",
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
