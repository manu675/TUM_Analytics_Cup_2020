import pandas as pd

df = pd.read_csv('payments.csv', delimiter=',')

df.dtypes

df = df.set_index(['Record_ID', 'Physician_ID', 'Company_ID'])

df_sums = df.groupby('Physician_ID').sum()
