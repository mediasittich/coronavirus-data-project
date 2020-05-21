import os
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import numpy as np
import pandas as pd
import geopandas as gpd
import datetime

from src.make_dataset import DATA_PATH, PROJECT_PATH
# from src.build_features import covid_data_countries

FIGURES_PATH = os.path.join(PROJECT_PATH, 'figures')

SHAPEFILE = os.path.join(DATA_PATH, 'ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp')
gdf = gpd.read_file(SHAPEFILE)[['ADM0_A3', 'geometry']].to_crs('+proj=robin')

def plot_choropleth_map(df, case_type, plot_date):
    COLORS = 9
    cmap = ''
    figsize = (16, 10)

    if case_type == 'confirmed':
        cmap = 'Blues'
    else:
        cmap = 'Reds'

    # Set title for plot
    title = 'Cumulative COVID-19 {} Cases (per 100K) by {}'.format(case_type.capitalize(), plot_date)

    # Select case type
    df = df[df['CaseType'] == case_type]

    # Select date
    df = df[df['Date'] == plot_date]

    merged = gdf.merge(df, left_on='ADM0_A3', right_on='ISO3Code')

    limited_merged = merged[merged['CumulativeReportedCasesPer100K'] < merged['CumulativeReportedCases']]

    # set range for the choropleth values
    vmin, vmax = limited_merged['CumulativeReportedCasesPer100K'].min(), limited_merged['CumulativeReportedCasesPer100K'].max()

    # Mapping the data
    # Create figure object and axes
    fig, ax = plt.subplots(1,1, figsize=figsize)

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
    sm = plt.cm.ScalarMappable(cmap=cmap, norm=plt.Normalize(vmin=vmin, vmax=320))
    cbr = fig.colorbar(sm, orientation='horizontal', fraction=0.05, pad=0.04) #)#, pad=0.2, aspect=30)
    cbr.ax.tick_params(labelsize=16) 
    cbr.set_label('Cumulative {} Cases per 100K'.format(case_type.capitalize()), fontsize=14)
    ax.set_axis_off()

    plt.title(title, fontsize=20)
    plt.tight_layout()
    filename = 'world_{}_27042020.pdf'.format(case_type)
    plt.savefig(os.path.join(FIGURES_PATH, filename), dpi=150)
    plt.close(fig)
    ax.get_figure()

    return fig

def plot_responses(korea_covid_df, korea_dates, korea_responses):
    fig, ax = plt.subplots(figsize=(12,9))

    # Plot Cumulative Cases
    ax.plot(korea_covid_df['CumulativeReportedCasesPer100K'])

    # Add horizontal lines
    colors = ['b', 'g', 'r', 'c', 'm', 'y', 'k']

    for idx, i in enumerate(korea_responses):
        plt.axvline(
            korea_dates[idx],
            label = i + ': ' + datetime.datetime.strftime(korea_dates[idx], "%Y-%m-%d"),
            c = colors[idx]
        )

    # Set axis limits
    start = datetime.datetime.strptime('2020-01-22', '%Y-%m-%d')
    end = datetime.datetime.strptime('2020-04-27', '%Y-%m-%d')
    ax.set_xlim(start, end)

    # Format ticks
    plt.tick_params(
        axis='both',          # changes apply to the x-axis
        which='major',      # both major and minor ticks are affected
        bottom=True,      # ticks along the bottom edge are off
        top=False,         # ticks along the top edge are off
        left=True,
        right=False,
        labelbottom=True) # labels along the bottom edge are off

    ax.set_xlabel('Date', fontsize=16)
    ax.set_ylabel('Cumulative Reported Cases per 100K', fontsize=16)
    plt.title('South Korea - Cumulative Reported Cases (per 100K) with Government Policies', fontsize=16)

    plt.tight_layout()
    plt.legend(bbox_to_anchor=(1.04,0.5), loc="center left", borderaxespad=0)
    fig.autofmt_xdate()
        
    filename = 'south_korea_example.pdf'
    plt.savefig(os.path.join(FIGURES_PATH, filename), dpi=150, bbox_inches='tight')
    plt.close(fig)
    ax.get_figure()
    
    return fig
