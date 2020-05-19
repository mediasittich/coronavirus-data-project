import os
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import numpy as np
import pandas as pd
import geopandas as gpd

from make_dataset import DATA_PATH
from build_features import covid_data_countries

SHAPEFILE = os.path.join(DATA_PATH, 'ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp')

COLORS = 9
cmap = ''
figsize = (16, 10)
start = '2020-01-22'
end = '2020-04-27'
confirmed = 'confirmed'
death = 'death'

plot_date = end
case_type = confirmed
if case_type == 'confirmed':
    cmap = 'Blues'
else:
    cmap = 'Reds'
title = 'Cumulative COVID-19 {} Cases (per 100K) by {}'.format(case_type.capitalize(), plot_date)

gdf = gpd.read_file(SHAPEFILE)[['ADM0_A3', 'geometry']].to_crs('+proj=robin')
df = covid_data_countries #[['DisplayName','ISO3Code', 'Continent', 'Date',
       #'CaseType', 'CumulativeReportedCases', 'CumulativeReportedCasesPer100K']]

# Select case type
df = df[df['CaseType'] == case_type]

# Select date
df = df[df['Date'] == plot_date]

merged = gdf.merge(df, left_on='ADM0_A3', right_on='ISO3Code')

limited_merged = merged[merged['CumulativeReportedCasesPer100K'] < merged['CumulativeReportedCases']]

# set range for the choropleth values
vmin, vmax = limited_merged['CumulativeReportedCasesPer100K'].min(), limited_merged['CumulativeReportedCasesPer100K'].max()

# Mapping the data
fig, ax = plt.subplots(1,1, figsize=figsize)

ax = merged.dropna().plot(column='CumulativeReportedCasesPer100K', categorical=True, cmap=cmap, 
                          edgecolor='white',
                          k=colors,
                         legend=False,
                          ax=ax,
                          missing_kwds={'color': 'lightgrey'},
                          rasterized=True
                         )

# Create colorbar legend
sm = plt.cm.ScalarMappable(cmap=cmap, norm=plt.Normalize(vmin=vmin, vmax=320))
cbr = fig.colorbar(sm, orientation='horizontal', fraction=0.05, pad=0.04)#)#, pad=0.2, aspect=30)
cbr.ax.tick_params(labelsize=16) 
cbr.set_label('Cumulative {} Cases per 100K'.format(case_type.capitalize()), fontsize=14)
ax.set_axis_off()


plt.title(title, fontsize=20)
plt.tight_layout()
plt.savefig('world_{}_27042020.pdf'.format(case_type), dpi=150)
plt.close(fig)
ax.get_figure()