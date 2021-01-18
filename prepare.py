import pandas as pd

from prepare_physicians import df as df_physicians
from prepare_payments import df_sums as df_payment_sums, df_ownership

df = pd.merge(df_physicians, df_payment_sums, how='inner', on='Physician_ID')

df = df.set_index('Physician_ID')


# Export train and test dataset, add ownership interest only to TRAIN set
df_train = df.merge(df_ownership, left_on='Physician_ID', right_index=True, how='outer')
df_train = df_train.loc[df['Set'] == 'train']

df_train.to_csv('dataset_train.csv')

df_test = df.loc[df['Set'] == 'test']
df_test.to_csv('dataset_test.csv')


# Export all the whole dataset with all columns possible
df = df.merge(df_ownership, left_on='Physician_ID', right_index=True, how='outer')
df.to_csv('dataset.csv')
