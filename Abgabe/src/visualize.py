# Import libraries
import os
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import numpy as np
import pandas as pd
import geopandas as gpd
import datetime

# Import path constants from custom module 'src'
from src.make_dataset import DATA_PATH, PROJECT_PATH

# Set path to 'figures' folder
FIGURES_PATH = os.path.join(PROJECT_PATH, 'figures')

# Set path to shapefile for geographic plotting
SHAPEFILE = os.path.join(DATA_PATH, 'ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp')
# Import shapefile with geopandas, select only columns 'ADM0_A3', 'geometry' & transform to coordinate reference system
gdf = gpd.read_file(SHAPEFILE)[['ADM0_A3', 'geometry']].to_crs('+proj=robin')

def plot_choropleth_map(df, case_type, plot_date):
    # Set number of colors
    COLORS = 9
    # Set colormap
    cmap = ''
    # Set output figure size
    figsize = (16, 10)

    # Assign colormap value depending on selected case_type
    if case_type == 'confirmed':
        cmap = 'Blues'
    else:
        cmap = 'Reds'

    # Set title for plot
    title = 'Cumulative COVID-19 {} Cases (per 100K) by {}'.format(case_type.capitalize(), plot_date)

    # Select rows where 'CaseType' equals to function input case_type
    df = df[df['CaseType'] == case_type]

    # Select rows where 'Date' equals to function input date
    df = df[df['Date'] == plot_date]

    # Merge dataframe passed to function with dataframe from shapefile
    merged = gdf.merge(df, left_on='ADM0_A3', right_on='ISO3Code')

    # Select only rows where 'CumulativeReportedCasesPer100K' < 'CumulativeReportedCases'
    # This ensures that small countries with many cases don't skew the colorscale
    limited_merged = merged[merged['CumulativeReportedCasesPer100K'] < merged['CumulativeReportedCases']]

    # set range for the choropleth values
    vmin, vmax = limited_merged['CumulativeReportedCasesPer100K'].min(), limited_merged['CumulativeReportedCasesPer100K'].max()

    # Mapping the data
    # Create figure object and axes
    fig, ax = plt.subplots(1,1, figsize=figsize)

    # Set parameters for axis
    ax = merged.dropna().plot(
        column='CumulativeReportedCasesPer100K', categorical=True, cmap=cmap, 
        edgecolor='white',
        k=COLORS,
        legend=False,
        ax=ax,
        missing_kwds={'color': 'lightgrey'},
        rasterized=True
    )

    # Create colorbar legend
    # Set colormap, normalize color scale between minimum and maximum value
    sm = plt.cm.ScalarMappable(cmap=cmap, norm=plt.Normalize(vmin=vmin, vmax=320))
    # Set orientation of colorbar to horizontal & ajust its positioning to make more space for world map
    cbr = fig.colorbar(sm, orientation='horizontal', fraction=0.05, pad=0.04)
    # Set label font size for colorbar
    cbr.ax.tick_params(labelsize=16) 
    # Set label for colorbar
    cbr.set_label('Cumulative {} Cases per 100K'.format(case_type.capitalize()), fontsize=14)

    ax.set_axis_off()

    # Set title & title font size for plot
    plt.title(title, fontsize=20)
    plt.tight_layout()
    # Set filename depending on selected case type(function parameter)
    filename = 'world_{}_27042020.pdf'.format(case_type)
    # Export figure
    plt.savefig(os.path.join(FIGURES_PATH, filename), dpi=150)
    plt.close(fig)
    ax.get_figure()

    return fig

def plot_responses(korea_covid_df, korea_dates, korea_responses):
    # Create figure object and axes
    fig, ax = plt.subplots(figsize=(12,9))

    # Plot Cumulative Cases from input dataframe with selected column
    ax.plot(korea_covid_df['CumulativeReportedCasesPer100K'])

    # Set colors list for horizontal lines
    colors = ['b', 'g', 'r', 'c', 'm', 'y', 'k']

    # For each value in responses list (function parameter with response names) & their indices
    for idx, i in enumerate(korea_responses):
        # Add vertical line
        plt.axvline(
            # x-value equals dates_list element with matching index
            korea_dates[idx],
            # Set label for vertical line to be displayed in legend
            label = i + ': ' + datetime.datetime.strftime(korea_dates[idx], "%Y-%m-%d"),
            # Set color from colors list based on current index
            c = colors[idx]
        )

    # Set axis limits from start & end date of analysis
    start = datetime.datetime.strptime('2020-01-22', '%Y-%m-%d')
    end = datetime.datetime.strptime('2020-04-27', '%Y-%m-%d')
    ax.set_xlim(start, end)

    # Format ticks
    plt.tick_params(
        axis='both',          # changes apply to the x- & y-axis
        which='major',      # major ticks are affected
        bottom=True,      # ticks along the bottom edge are on
        top=False,         # ticks along the top edge are off
        left=True,         # ticks along the left edge are on
        right=False,        # ticks along the left edge are off
        labelbottom=True) # labels along the bottom edge are on

    # Set x-axis label and its font size
    ax.set_xlabel('Date', fontsize=16)
    # Set y-axis label and its font size
    ax.set_ylabel('Cumulative Reported Cases per 100K', fontsize=16)
    # Set title & title font size for plot
    plt.title('South Korea - Cumulative Reported Cases (per 100K) with Government Policies', fontsize=16)

    plt.tight_layout()
    # Adjust legend positioning outside of figure
    plt.legend(bbox_to_anchor=(1.04,0.5), loc="center left", borderaxespad=0)
    # 
    fig.autofmt_xdate()
        
    # Set filename
    filename = 'south_korea_example.pdf'
    # Export figure
    plt.savefig(os.path.join(FIGURES_PATH, filename), dpi=150, bbox_inches='tight')
    plt.close(fig)
    ax.get_figure()
    
    return fig
