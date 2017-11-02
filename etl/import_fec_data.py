import requests
import re
import sys
import os
import psycopg2
import db_credentials
import itertools
import operator
import zipfile

from datetime import datetime
from urllib.request import urlopen
from io import StringIO, BytesIO
from bs4 import BeautifulSoup

def insert_fec_row(table_name, data_dict):
    template = 'INSERT INTO {table_name} ({varnames}) VALUES({formatters})'
    varnames = [None]*len(data_dict)
    for varname in data_dict:
        _,_,position = data_dict[varname]
        position -= 1 # Change to 0-indexed
        varnames[position] = varname

    varnames = ','.join(varnames)
    formatters = ','.join(['%s']*len(data_dict))
    
    out = template.format(table_name=table_name, 
                          varnames=varnames, 
                          formatters=formatters)
    return(out)


def create_table(table_name, data_dict):
    '''
    Generates a SQL create table statement given the tablename and 
    variables, data types, and nullable argument
    table_name: str, name of the table
    data_dict: dictionary, 'variable_name': (data_type, nullable)
    ''' 
    varstrings = []
    # Insert variables sorted by position value from the original table
    for el in sorted(data_dict.items(), key=lambda x: x[1][2]):
        varname = el[0]
        dtype,nullable,_ = el[1] 
        if nullable:
            #varstrings.append(f'    {varname} {dtype} NOT NULL')
            # FEC doesn't keep their constraints in their bulk data files,
            # Therefore ignore nullability
            varstrings.append(f'    {varname} {dtype}')
        else:
            varstrings.append(f'    {varname} {dtype}')
   
    varstring = ',\n'.join(varstrings)
    return f"CREATE TABLE {table_name} (\n{varstring}\n);"


def get_fec_data_dictionary(table_name):

    url = (f'http://classic.fec.gov/finance/disclosure/metadata/DataDictionary'
           f'{table_name}.shtml')
    page = requests.get(url).content 
    soup = BeautifulSoup(page, 'lxml')
    valid_dtypes = re.compile('DATE|TEXT|DECIMAL')
    variables = {}
    for i,tr in enumerate(soup.find_all('tr')):
         if i == 0:
             continue

         tds = tr.find_all('td')
         name = re.sub('\s', '', tds[0].text)
         position = int(tds[2].text)
         nullable = tds[3].text
         dtype = re.sub('\s', '', tds[4].text)

         if nullable == "N":
             nullable = False
         elif nullable == 'Y':
             nullable = True
         else:
             print('Unexpected nullable value. Setting to True')
             nullable = True

         if 'VARCHAR' in dtype:
             dtype = 'TEXT'
         dtype = re.sub('NUMBER', 'DECIMAL', dtype)
         dtype = re.sub('Number', 'DECIMAL', dtype)

         if dtype == '':
             dtype = 'TEXT'

         variables[name] = (dtype, nullable, position)

    return variables

class InvalidLineError(Exception):
    def __init__(self,*args,**kwargs):
        Exception.__init__(self,*args,**kwargs)

def parse_fec_data_line(line, data_dict):
    line = line.strip('\n')
    n_vars = len(data_dict)
    fields = line.split('|')
    if len(fields) != n_vars:
        raise InvalidlineError()

    
    out = [None]*n_vars
    # Convert types right now only 3: Text, decimal and 
    for varname in data_dict:
        dtype,_,position = data_dict[varname]
        position -= 1
        string_value = fields[position]
        if 'TEXT' in dtype:
            out[position] = string_value
        elif 'DECIMAL' in dtype:
            out[position] = float(string_value)
        elif 'DATE' in dtype:
            try:
                out[position] = datetime.strptime(string_value, '%m%d%Y')
            except ValueError:
                out[position] = None

        else:
            raise ValueError(f'Unexpected dtype: {dtype}')

    return out
            

if __name__ == "__main__":

    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Config
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Inputs
    TABLES = [('CommitteeMaster', 'cm'), 
              ('CandidateMaster', 'cn'), 
              ('CandCmteLinkage', 'ccl'), 
              ('CommitteetoCommittee', 'oth'), 
              ('ContributionstoCandidates', 'pas2'),
              ('ContributionsbyIndividuals', 'indiv'), 
              ('OperatingExpenditures', 'oppexp')]
    YEARS = ['18', '16', '14']



    ## Download all data dictionaries
    dictionaries = {}
    for table_name,_ in TABLES:
        print(f'Processing {table_name}')
        dictionaries[table_name] = get_fec_data_dictionary(table_name)


    ## Establish db connection
    db_connection = psycopg2.connect((f"host='{db_credentials.DB_HOST}' "
                                      f"port='{db_credentials.DB_PORT}' "
                                      f"dbname='{db_credentials.DB_NAME}' "
                                      f"user='{db_credentials.DB_USER}' "
                                      f"password='{db_credentials.DB_PASSWORD}'"))
    cursor = db_connection.cursor()

       


    ## Download the data files and import to postgres
    invalid_lines = {}
    for table, year in itertools.product(TABLES, YEARS):
        
        print(f'Processing data for {table_name}{year}')
        archive_name = table[1]
        table_name = table[0]
        tab_year = table_name + str(year)
        invalid_lines[tab_year] = 0
        ### Create the table
        cursor.execute(f'DROP TABLE {table_name}{year};')
        cursor.execute(create_table(tab_year, dictionaries[table_name]))

        url = urlopen(f'ftp://ftp.fec.gov/FEC/20{year}/{archive_name}{year}.zip')
        with zipfile.ZipFile(BytesIO(url.read())) as z:
            a_name = z.namelist()[0]
            for i,line in enumerate(z.open(a_name)):
                line = line.decode("utf-8")
                try:
                    row = parse_fec_data_line(line, dictionaries[table_name])
                except InvalidLineError:
                    invalid_lines[tab_year] += 1
                    print('Invalid Line: {line}')

                cursor.execute(insert_fec_row(table_name+year, 
                                              dictionaries[table_name]), row)
        print(f'Processed {i} rows')
        n = invalid_lines[tab_year]
        print(f'{n} invalid lines')
    
        db_connection.commit()

    cursor.close()
    db_connection.close()

