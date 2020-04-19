import requests
from datetime import datetime
from datetime import timedelta
import pandas as pd
import sys
from random import sample

import settings

# only want symbols with ~5 yrs of data
MIN_ROWS = 1250

def fetch_range(symbol, start, end):
    params = {
        'symbol': symbol,
        'resolution': 'D', # we only need per-day data
        'from': date_to_timestamp(start),
        'to': date_to_timestamp(end),
    }
    return request('stock/candle', params)

def date_to_timestamp(date_string):
    date = datetime.strptime(date_string + "-+0000", "%m-%d-%Y-%z")
    stamp = datetime.timestamp(date) 
    return int(stamp)

def request(endpoint, params):
    auth_param = {'token': settings.FINNHUB_API_KEY}
    params_with_token = {**params, **auth_param }
    r = requests.get('https://finnhub.io/api/v1/' + endpoint, params=params_with_token)
    print("DEBUG: Request: " + r.url)
    return r.json()

def to_df(results, symbol):
    if results.get('s') == 'no_data':
        print(f'skipping {symbol}, no data')
        return pd.DataFrame()
    if len(results.get('c')) < MIN_ROWS:
        print(f'skipping {symbol}, low data')
        return pd.DataFrame()
    data = {
        'low': results.get('l')[-MIN_ROWS:],
        'high': results.get('h')[-MIN_ROWS:],
        'open': results.get('o')[-MIN_ROWS:],
        'close': results.get('c')[-MIN_ROWS:],
        'volume': results.get('v')[-MIN_ROWS:],
        'timestamp': results.get('t')[-MIN_ROWS:],
        'datetime': pd.to_datetime(results.get('t')[-MIN_ROWS:], unit='s'),
        'symbol': symbol
    }
    return pd.DataFrame(data=data)

def all_symbols():
    return request('stock/symbol', {'exchange': 'US'})

def run(args):
    try:
        symbol = args[1]
        start = args[2]
        end = args[3]
        return to_df(fetch_range(symbol=symbol, start=start, end=end), symbol=symbol)
    except Exception:
        print("\nUsage example: pull.py AAPL 01-15-2020 02-25-2020 \n")
        raise

#import code; code.interact(local=dict(globals(), **locals()))

def random_sample(n):
    start = '01-01-2000'
    end = '01-01-2020'
    symbols = [ r['symbol'] for r in sample(all_symbols(), n) ]
    dfs = [ to_df(fetch_range(symbol=symbol, start=start, end=end), symbol=symbol) for symbol in symbols]
    return pd.concat(dfs)


random_sample(50).to_csv('sample.csv')
