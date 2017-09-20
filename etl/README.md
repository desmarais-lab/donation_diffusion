# ETL for FEC data

`main_etl.sh` does the following:
* Download committee contribution data for 2015-2016 and 2017-2018 + header
    files
* Downloads candidate master files for same years + headers
* Parses the pipe separated files to csv
* Adds headers
* joins candidate and donation files by candidate id
* Concatenates them to a single file
