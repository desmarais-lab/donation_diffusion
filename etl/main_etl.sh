# For 2016 (16) and 2018 (18) download the pac_donation and candidate master files 
# (+ headers) merge them with their respective headers, join on candidate id and 
# save in new file

# Committee header file
wget http://classic.fec.gov/finance/disclosure/metadata/pas2_header_file.csv
# Candidate header file
wget http://classic.fec.gov/finance/disclosure/metadata/cn_header_file.csv

for i in 16 18
do
    # Download donation archive
    ARCHIVE_NAME=pas2${i}.zip
    ARCHIVE_URL=ftp://ftp.fec.gov/FEC/20${i}/$ARCHIVE_NAME
    wget $ARCHIVE_URL
    unzip $ARCHIVE_NAME
    rm $ARCHIVE_NAME

    # Parse pipe separated, merge with header and write to csv
    python psv_to_csv.py itpas2.txt pas2_header_file.csv \
        20${i}_pac_contributions.csv
    rm itpas2.txt

    # Download candidate archive
    ARCHIVE_NAME=cn${i}.zip
    ARCHIVE_URL=ftp://ftp.fec.gov/FEC/20${i}/$ARCHIVE_NAME
    wget $ARCHIVE_URL
    unzip $ARCHIVE_NAME
    rm $ARCHIVE_NAME

    # Parse pipe separated, merge with header and write to csv
    python psv_to_csv.py cn.txt cn_header_file.csv 20${i}_candidates.csv
    rm cn.txt
    
    # Join donation and candidate data
    python join_donation_candidate.py 20${i}_pac_contributions.csv \
        20${i}_candidates.csv 20${i}_pac_contributions.csv

    rm 20${i}_candidates.csv
done

# Concatenate the datasets
python concat_csvs.py *_pac_contributions.csv ../data/pac_contributions.csv

# Clean up
rm pas2_header_file.csv
rm cn_header_file.csv
rm *_pac_contributions.csv
