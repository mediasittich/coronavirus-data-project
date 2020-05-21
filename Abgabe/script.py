import os
import datetime
import pandas as pd

from src.make_dataset import GOVERNMENT_RESPONSE_C6, DATA_PATH
from src.build_features import covid_data_countries, response_data_all
from src.visualize import plot_choropleth_map, plot_responses


# ========================================
#           CREATE WORLD MAPS
# ========================================

target_date = '2020-04-27'
df = covid_data_countries[['DisplayName','ISO3Code', 'Continent', 'Date',
       'CaseType', 'CumulativeReportedCases', 'CumulativeReportedCasesPer100K']]

# # Confirmed Cases
case_type = 'confirmed'
vmax = 320

plot_choropleth_map(df, case_type, target_date)

# # Death Cases
case_type = 'death'
vmax = 120

plot_choropleth_map(df, case_type, target_date)

# ================================================
#      OUTLOOK - GOVERNMENT RESPONSE EXAMPLES
# ================================================

# ---------- Example: Different Responses in South Korea ---------- 
# Select South Korea with CountryCode
korea_df = response_data_all[response_data_all['CountryCode'] == 'KOR']

# Drop all non-relevant columns
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

korea_df.drop(cols_to_drop, axis=1, inplace=True)

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

korea_responses = []
korea_dates = []

for ind in indicators:
    start_date = korea_df[korea_df[ind] > 1]['Date'].head(1)
    if not len(start_date) == 0:
        start_date = start_date.item()
        start_date = datetime.datetime.strptime(start_date, '%Y%m%d')
        korea_responses.append(ind)
        korea_dates.append(start_date.date())

# Select only Korea from COVID Dataset
korea_covid_df = covid_data_countries[(covid_data_countries['ISO3Code'] == 'KOR') & (covid_data_countries['CaseType'] == 'confirmed')]
korea_covid_df = korea_covid_df[['Date', 'CumulativeReportedCasesPer100K']]
korea_covid_df['Date'] = pd.to_datetime(korea_covid_df['Date'], infer_datetime_format=True)
korea_covid_df = korea_covid_df.set_index(pd.DatetimeIndex(pd.to_datetime(korea_covid_df['Date'])))

plot_responses(korea_covid_df, korea_dates, korea_responses)

# ---------- Example: C6 Response in European Countries ---------- 
europe_df = covid_data_countries[covid_data_countries['Continent'] == 'Europe']

iso_eu_set = set(europe_df['ISO3Code'].unique())


response_c6 = GOVERNMENT_RESPONSE_C6
response_c6.drop(['Unnamed: 0'], axis=1, inplace=True)
response_c6.rename(columns={'Unnamed: 1': 'CountryCodes'}, inplace=True)

# Convert data to long-format
response_data_long = pd.melt(response_c6, id_vars=['CountryCodes'], var_name='Date', value_name='codes')

response_data_long['Date'] = pd.to_datetime(response_data_long['Date'])

# Replace '.' in 'codes' with 0
response_data_long.replace({'.': 0}, inplace=True)
# Replace NaN with -1 and convert all other to integer
response_data_long['codes'] = response_data_long['codes'].fillna(-1).astype(int)

iso_response_set = set(response_c6['CountryCodes'].unique())

common_eu_codes = list(iso_eu_set.intersection(iso_response_set))

response_eu_codes = []
response_eu_dates = []

for code in common_eu_codes:
    start_date_eu = response_data_long[(response_data_long['CountryCodes'] == code) & (response_data_long['codes'] > 1)]['Date'].head(1)
    if not len(start_date_eu) == 0:
        start_date_eu = start_date_eu.item()
        response_eu_codes.append(code)
        response_eu_dates.append(start_date_eu)

response_eu_dict = dict(zip(response_eu_codes, response_eu_dates))

c6_start_dates = pd.DataFrame.from_dict(response_eu_dict, orient='index')
c6_start_dates.to_csv(os.path.join(DATA_PATH, 'c6_start_dates.csv'))
