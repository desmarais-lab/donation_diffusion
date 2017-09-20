# Committee header file
wget http://classic.fec.gov/finance/disclosure/metadata/pas2_header_file.csv
# Candidate header file
wget http://classic.fec.gov/finance/disclosure/metadata/cn_header_file.csv

# Donation data 

## 2016

### Donation data
wget ftp://ftp.fec.gov/FEC/2016/pas216.zip
unzip pas216.zip
rm pas216.zip
python psv_to_csv.py itpas2.txt pas2_header_file.csv 2016_pac_contributions.csv
rm itpas2.txt

### Candidate data
wget ftp://ftp.fec.gov/FEC/2016/cn16.zip
unzip cn16.zip
rm cn16.zip
python psv_to_csv.py cn.txt cn_header_file.csv 2016_candidates.csv
rm cn.txt

### Join donation and candidate data
python join_donation_candidate.py 2016_pac_contributions.csv \
    2016_candidates.csv 2016_pac_contributions.csv

rm 2016_candidates.csv

## 2018

### Donation Data
wget ftp://ftp.fec.gov/FEC/2018/pas218.zip
unzip pas218.zip
rm pas218.zip
python psv_to_csv.py itpas2.txt pas2_header_file.csv 2018_pac_contributions.csv
rm itpas2.txt

### Candidate data
wget ftp://ftp.fec.gov/FEC/2018/cn18.zip
unzip cn18.zip
rm cn18.zip
python psv_to_csv.py cn.txt cn_header_file.csv 2018_candidates.csv
rm cn.txt

### Join donation and candidate data
python join_donation_candidate.py 2018_pac_contributions.csv \
    2018_candidates.csv 2018_pac_contributions.csv

rm 2018_candidates.csv

# Concatenate the datasets
python concat_csvs.py *_pac_contributions.csv ../data/pac_contributions.csv

# Clean up
rm pas2_header_file.csv
rm cn_header_file.csv
rm *_pac_contributions.csv
