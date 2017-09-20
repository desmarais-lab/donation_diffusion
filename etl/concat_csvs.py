'''
Concatenate an arbitrary number of csv files. The files must have conforming
dimensions and header rows

usage: python concat_csvs.py file_1.csv file_2.csv ... output_file.csv
'''
import pandas as pd
import sys

dfs = [pd.read_csv(f) for f in sys.argv[1:-1]]
df = pd.concat(dfs)
df.to_csv(sys.argv[-1], index=False)
print('Summary: ')
for d in dfs:
    print(f'{d.shape}')
print('Resulting csv: ')
print('dtypes: ')
print(df.dtypes)
print(f'Shape: {df.shape}')
