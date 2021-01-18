import pandas as pd

from prepare_physicians import df as df_physicians
from prepare_payments import df_sums as df_payment_sums

df = pd.merge(df_physicians, df_payment_sums, how='inner', on='Physician_ID')

df = df.set_index('Physician_ID')

df.to_csv('dataset.csv')
