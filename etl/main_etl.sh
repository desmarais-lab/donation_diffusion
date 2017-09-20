# Header file 
wget http://classic.fec.gov/finance/disclosure/metadata/pas2_header_file.csv

# Donation data 

## 2016
wget ftp://ftp.fec.gov/FEC/2016/pas216.zip
unzip pas216.zip
rm pas216.zip
python psv_to_csv.py itpas2.txt pas2_header_file.csv ../data/2016_pac_contributions.csv
rm itpas2.txt

## 2018
wget ftp://ftp.fec.gov/FEC/2018/pas218.zip
unzip pas218.zip
python psv_to_csv.py itpas2.txt pas2_header_file.csv ../data/2018_pac_contributions.csv

# Concatenate the datasets
python concat_csvs.py ../data/*_pac_contributions.csv ../data/pac_contributions.csv

# Clean up
rm pas218.zip
rm itpas2.txt
rm pas2_header_file.csv
rm ../data/*_pac_contributions.csv
