# Import libraries
import os
from pathlib import Path

import pandas as pd

# ========================================
#              LOAD DATA
# ========================================
# Set paths to project & data directories as constants
PROJECT_PATH = Path(__file__).parent.parent.absolute()
DATA_PATH = os.path.join(PROJECT_PATH, 'data')

# ============= COVID-19 Data =============
# COVID-19 Dataset (Source: RamiKrispin GitHub)
# Set URL for COVID dataset CSV file
CORONA_DATA_URL = 'https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv'
# Import COVID dataset as dataframe
CORONA_DATA = pd.read_csv(CORONA_DATA_URL)

# ============= Continents Data =============
# Population Data
# Source: UN 2019 Revision of World Population Prospects
# (https://population.un.org/wpp/Download/Standard/CSV/)
# https://population.un.org/wpp/DefinitionOfProjectionVariants/

# Set file path to population data CSV file
POPULATION_DATA_FILENAME = os.path.join(
    DATA_PATH, 'WPP2019_TotalPopulationBySex.csv')
# Import dataset as dataframe
POPULATION_DATA = pd.read_csv(POPULATION_DATA_FILENAME)

# ============= Continents Data =============
# Load dataset with countries and continents
# Source: https://datahub.io/JohnSnowLabs/country-and-continent-codes-list#python

# Set file path to continents data CSV file
CONTINENTS_URL = os.path.join(
    DATA_PATH, 'country-and-continent-codes-list-csv_csv.csv')

# Import dataset to dataframe
CONTINENTS_DATA = pd.read_csv(CONTINENTS_URL)

# ============= Government Response Data =============
# Set base URL for GitHub repository
GOVERNMENT_RESPONSE_DATA_BASEURL = 'https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/'
# Set URL for timeseries datasets in repository
GOVERNMENT_RESPONSE_TIMESERIES_BASEURL = GOVERNMENT_RESPONSE_DATA_BASEURL + 'timeseries/'
# Set URL for complete dataset of all responses with Stringency Indices
GOVERNMENT_RESPONSE_LATEST = pd.read_csv(GOVERNMENT_RESPONSE_DATA_BASEURL + 'OxCGRT_latest.csv')
# Set URL for timeseries data on indicator 'C6 - Social distancing'
GOVERNMENT_RESPONSE_C6 = pd.read_csv(GOVERNMENT_RESPONSE_TIMESERIES_BASEURL + 'c6_stayathomerequirements.csv')