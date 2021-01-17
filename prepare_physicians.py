import pandas as pd

df = pd.read_csv('physicians.csv', delimiter=',')


# Rename "id" and "set" columns
df = df.rename(columns={'id': 'Physicians_ID', 'set': 'Set'})


# Drop name columnes
df = df.drop(columns=['First_Name', 'Middle_Name', 'Last_Name', 'Name_Suffix'])


# Primary_Specialty
df['Primary_Specialty'] = df['Primary_Specialty'].str.split('|')
# Count number of Primary_Specialty per physician
df['Primary_Specialty_Count'] = [
    0 if isinstance(row, float) else len(row) for row in df['Primary_Specialty']
]
# Unique list of all Primary_Specialty
# df['Primary_Specialty'].str.split('|').explode().unique()
# TODO: Should we add Primary_Specialty as boolean mapping or any better idea?
df = df.drop(columns=['Primary_Specialty'])

# Country
df['Country'].value_counts()
# UNITED STATES                           5999
# UNITED STATES MINOR OUTLYING ISLANDS       1
# Name: Country, dtype: int64
df = df.drop(columns=['Country'])

# Province
df['Province'].value_counts()
# Series([], Name: Province, dtype: int64)
df = df.drop(columns=['Province'])


# License_State
df = df.assign(
    License_State_Count=lambda df: sum(
        [
            df['License_State_1'].astype(str) != 'nan',
            df['License_State_2'].astype(str) != 'nan',
            df['License_State_3'].astype(str) != 'nan',
            df['License_State_4'].astype(str) != 'nan',
            df['License_State_5'].astype(str) != 'nan',
        ]
    )
)


# TODO: Geo information
# - State
# - Zipcode
# - City
# - License_State
df = df.drop(columns=['State', 'Zipcode', 'City'])
df = df.drop(
    columns=[
        'License_State_1',
        'License_State_2',
        'License_State_3',
        'License_State_4',
        'License_State_5',
    ]
)


# TEST and TRAIN dataset
df_train = df.loc[df['Set'] == 'train']
# len(df.loc[df['Set'] == 'train'])
# 5000

df_test = df.loc[df['Set'] == 'test']
# len(df.loc[df['Set'] == 'test'])
# 1000
df_test['Physicians_ID'].unique()
