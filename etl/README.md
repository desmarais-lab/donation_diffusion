# ETL for FEC data

`main_etl.sh` does the following:
* Download committee contribution data for 2015-2016 and 2017-2018 + header
    files
* Parses the pipe separated files to csv
* Adds headers
* Concatenates them to a single file
