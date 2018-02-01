import requests
import pandas as pd
import numpy as np
import time

from api_key import api_key


class FECApi(object):

    def __init__(self, api_key):
        self.base_url = 'https://api.open.fec.gov/v1/{path}/?api_key={api_key}'
        self.api_key = api_key
        
    def get_data(self, query):
        url = self.base_url.format(path=query, api_key=self.api_key)
        return requests.get(url)


if __name__ == "__main__":
    df = pd.read_csv('data/2016_PAC_CONTRIBUTIONS.csv')
    ids = np.random.choice(df.CMTE_ID.unique(), 500, replace=False)
    api = FECApi(api_key)
    
    data = []
    emails = []
    for id_ in ids:
        response = api.get_data(f'committee/{id_}')
        #data.append(response.json())
        #emails.append(response.json()['results'][0]['email'])
        limit = response.headers['X-RateLimit-Remaining']
        if limit == 0:
            break
        time.sleep(0.5)
