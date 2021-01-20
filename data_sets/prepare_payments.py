import pandas as pd

df = pd.read_csv('payments.csv', delimiter=',')

# Set all ID columns as index
df = df.set_index(['Record_ID', 'Physician_ID', 'Company_ID'])


# Basic analysis
# for col in df.columns:
#     print('###', col)
#     print(df[col].value_counts())


# Aggregate via sums
df_sums = df.groupby('Physician_ID').sum()


# Ownership interest
df_ownership = (
    df[['Ownership_Indicator']][df['Ownership_Indicator'] == 'Yes']
    .groupby('Physician_ID')
    .count()
)

df_ownership = df_ownership.rename(
    columns={'Ownership_Indicator': 'Number_of_Ownership_Interests'}
)

df_ownership = df_ownership.assign(
    Has_Ownership_Interest=lambda df: df['Number_of_Ownership_Interests'] > 0
)
