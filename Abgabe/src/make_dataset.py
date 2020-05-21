import os
from pathlib import Path

import pandas as pd

# ========================================
#              LOAD DATA
# ========================================
PROJECT_PATH = Path(__file__).parent.parent.absolute()
DATA_PATH = os.path.join(PROJECT_PATH, 'data')

print(PROJECT_PATH)

# ============= COVID-19 Data =============
# COVID-19 Dataset (Source: RamiKrispin GitHub)
CORONA_DATA_URL = 'https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv'
CORONA_DATA = pd.read_csv(CORONA_DATA_URL)

# ============= Continents Data =============
# Population Data
# Source: UN 2019 Revision of World Population Prospects
# (https://population.un.org/wpp/Download/Standard/CSV/)
# https://population.un.org/wpp/DefinitionOfProjectionVariants/
POPULATION_DATA_FILENAME = os.path.join(
    DATA_PATH, 'WPP2019_TotalPopulationBySex.csv')
POPULATION_DATA = pd.read_csv(POPULATION_DATA_FILENAME)

# ============= Continents Data =============
# Load dataset with countries and continents
# Source: https://datahub.io/JohnSnowLabs/country-and-continent-codes-list#python
# Assign filename
CONTINENTS_URL = os.path.join(
    DATA_PATH, 'country-and-continent-codes-list-csv_csv.csv')

# Load dataset to dataframe
CONTINENTS_DATA = pd.read_csv(CONTINENTS_URL)

# ============= Continents Data =============
# Government Response Data
GOVERNMENT_RESPONSE_DATA_BASEURL = 'https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/'
GOVERNMENT_RESPONSE_TIMESERIES_BASEURL = GOVERNMENT_RESPONSE_DATA_BASEURL + 'timeseries/'

GOVERNMENT_RESPONSE_LATEST = pd.read_csv(GOVERNMENT_RESPONSE_DATA_BASEURL + 'OxCGRT_latest.csv')
GOVERNMENT_RESPONSE_C6 = pd.read_csv(GOVERNMENT_RESPONSE_TIMESERIES_BASEURL + 'c6_stayathomerequirements.csv')  # Social distancing