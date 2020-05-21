# Import libraries
import os
import datetime
import pandas as pd

# Import data & functions from custom module 'src'
from src.make_dataset import GOVERNMENT_RESPONSE_C6, DATA_PATH # response C6 dataset & path for saving table
from src.build_features import covid_data_countries, response_data_all # datasets COVID cases & all responses 
from src.visualize import plot_choropleth_map, plot_responses # plot functions


# ========================================
#           CREATE WORLD MAPS
# ========================================

# Set target date
target_date = '2020-04-27'
# Create dataframe with selected columns from imported dataset
df = covid_data_countries[['DisplayName','ISO3Code', 'Continent', 'Date',
       'CaseType', 'CumulativeReportedCases', 'CumulativeReportedCasesPer100K']]

## Confirmed Cases
# Assign case type to select in dataset
case_type = 'confirmed'
# Set max value for color scale in plot
vmax = 320

# Call imported map plot
plot_choropleth_map(df, case_type, target_date)

# # Death Cases
# Assign case type to select in dataset
case_type = 'death'
# Set max value for color scale in plot
vmax = 120

# Call imported map plot
plot_choropleth_map(df, case_type, target_date)

# ================================================
#      OUTLOOK - GOVERNMENT RESPONSE EXAMPLES
# ================================================

# ---------- Example: Different Responses in South Korea ---------- 

# Create dataframe with data only for South Korea:
# From Government Response Data select only South Korea with CountryCode & assign it to new dataframe
korea_df = response_data_all[response_data_all['CountryCode'] == 'KOR']

# Drop all non-relevant columns:
# Create list with columns to drop
cols_to_drop = [ 'C1_Flag',
        'C2_Flag', 'C3_Flag',
        'C4_Flag',
        'C5_Flag', 'C6_Flag',
        'C7_Flag',
        'E1_Income support', 'E1_Flag',
        'E2_Debt/contract relief', 'E3_Fiscal measures',
        'E4_International support', 'H1_Public information campaigns',
        'H1_Flag', 'H2_Testing policy', 'H3_Contact tracing',
        'H4_Emergency investment in healthcare', 'H5_Investment in vaccines',
        'M1_Wildcard', 'ConfirmedCases', 'ConfirmedDeaths', 'StringencyIndex',
        'StringencyIndexForDisplay', 'LegacyStringencyIndex',
        'LegacyStringencyIndexForDisplay']
# Drop columns from list
korea_df.drop(cols_to_drop, axis=1, inplace=True)

# Create list with selected indicators
indicators = [
    'C1_School closing',
    'C2_Workplace closing',
    'C3_Cancel public events',
    'C4_Restrictions on gatherings',
    'C5_Close public transport',
    'C6_Stay at home requirements',
    'C7_Restrictions on internal movement',
    'C8_International travel controls'
]
# Create empty lists for response names & response start dates in South Korea
korea_responses = []
korea_dates = []
# Loop over all selected indicators
for ind in indicators:
    # Select all rows where the indicator is greater than 1
    # Select only 'Date' column
    # Get first element in Series and assign it to start_date
    start_date = korea_df[korea_df[ind] > 1]['Date'].head(1)
    # If start_date exists
    if not len(start_date) == 0:
        # Convert start_date from series object to string
        start_date = start_date.item()
        # Convert string to datetime object
        start_date = datetime.datetime.strptime(start_date, '%Y%m%d')
        # Append current indicator to response names list
        korea_responses.append(ind)
        # Append current start_date as date object to response dates list
        korea_dates.append(start_date.date())


# From COVID dataset select only Korea from COVID Dataset & only CaseType 'confirmed'
korea_covid_df = covid_data_countries[(covid_data_countries['ISO3Code'] == 'KOR') & (covid_data_countries['CaseType'] == 'confirmed')]
# Select only necessary columns
korea_covid_df = korea_covid_df[['Date', 'CumulativeReportedCasesPer100K']]
# Convert 'Date' column type to datetime format
korea_covid_df['Date'] = pd.to_datetime(korea_covid_df['Date'], infer_datetime_format=True)
# Set 'Date' column as dataframe index
korea_covid_df = korea_covid_df.set_index(pd.DatetimeIndex(pd.to_datetime(korea_covid_df['Date'])))

# Call imported funtion to plot selected indicator start dates in South Korea
plot_responses(korea_covid_df, korea_dates, korea_responses)


# ---------- Example: C6 Response in European Countries ---------- 
# From COVID dataset select all rows where 'Continent' is 'Europe'
europe_df = covid_data_countries[covid_data_countries['Continent'] == 'Europe']

# Create a set of all 'ISO3Codes' in previous selection (European countries)
iso_eu_set = set(europe_df['ISO3Code'].unique())


# Assign imported dataset of C6 responses to dataframe
response_c6 = GOVERNMENT_RESPONSE_C6
# Drop unneccesary column with country names
response_c6.drop(['Unnamed: 0'], axis=1, inplace=True)
# Rename column containing ISO3 codes to 'CountryCodes'
response_c6.rename(columns={'Unnamed: 1': 'CountryCodes'}, inplace=True)

# Convert data to long-format by melting
response_data_long = pd.melt(response_c6, id_vars=['CountryCodes'], var_name='Date', value_name='codes')

# Convert 'Date' column type to datetime format
response_data_long['Date'] = pd.to_datetime(response_data_long['Date'])

# Replace '.' values in 'codes' column with 0
response_data_long.replace({'.': 0}, inplace=True)
# Replace NaN values in 'codes' column with -1 and convert all other to integer
response_data_long['codes'] = response_data_long['codes'].fillna(-1).astype(int)

# Create a set of all 'CountryCodes' in C6 response dataset
iso_response_set = set(response_c6['CountryCodes'].unique())

# Create a list of all ISO3 codes common to both sets (from COVID- & C6-Responses-Datasets)
common_eu_codes = list(iso_eu_set.intersection(iso_response_set))

# Create empty lists for response names & response start dates in European countries
response_eu_codes = [] # will contain only codes where a value for the response exists
response_eu_dates = []
# Loop over all common ISO3 codes list
for code in common_eu_codes:
    # Select all rows where 'CountryCodes' column matches current code & the indicator is greater than 1
    # Select only 'Date' column
    # Get first element in Series and assign it to start_date
    start_date_eu = response_data_long[(response_data_long['CountryCodes'] == code) & (response_data_long['codes'] > 1)]['Date'].head(1)
    # If start_date_eu exists
    if not len(start_date_eu) == 0:
        # Convert start_date_eu from series object to string
        start_date_eu = start_date_eu.item()
        # Append current ISO3 code to ISO codes list
        response_eu_codes.append(code)
        # Append current start_date as date object to response dates list
        response_eu_dates.append(start_date_eu)

# Create tuple of previous list & convert resulting tuple to dictionary
response_eu_dict = dict(zip(response_eu_codes, response_eu_dates))

# Convert dictionary to dataframe
c6_start_dates = pd.DataFrame.from_dict(response_eu_dict, orient='index')
# Save dataframe to CSV file
c6_start_dates.to_csv(os.path.join(DATA_PATH, 'c6_start_dates.csv'))
