import requests
from datetime import datetime
from datetime import timedelta
import pandas as pd
import sys
from random import sample

import settings

# for training we only want symbols with ~5 yrs of data
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

def to_df(results, symbol, minimum=False):
    min_rows = MIN_ROWS if minimum else 0
    if results.get('s') == 'no_data':
        print(f'skipping {symbol}, no data')
        return pd.DataFrame()
    if len(results.get('c')) < min_rows:
        print(f'skipping {symbol}, low data')
        return pd.DataFrame()
    data = {
        'low': results.get('l')[-min_rows:],
        'high': results.get('h')[-min_rows:],
        'open': results.get('o')[-min_rows:],
        'close': results.get('c')[-min_rows:],
        'volume': results.get('v')[-min_rows:],
        'timestamp': results.get('t')[-min_rows:],
        'datetime': pd.to_datetime(results.get('t')[-min_rows:], unit='s'),
        'symbol': symbol
    }
    return pd.DataFrame(data=data)

def all_symbols():
    return request('stock/symbol', {'exchange': 'US'})

def fetch_range_for_symbols(start, end, symbols, minimum=False):
    dfs = [ to_df(fetch_range(symbol=symbol, start=start, end=end), symbol=symbol, minimum=minimum) for symbol in symbols]
    return pd.concat(dfs)


def random_sample(n):
    start = '01-01-2000'
    end = '01-01-2020'
    symbols = [ r['symbol'] for r in sample(all_symbols(), n) ]
    return fetch_range_for_symbols(start, end, symbols, minimum=True)

def run(args):
    try:
        start = args[1]
        end = args[2]
        symbols = args[3:]
        return fetch_range_for_symbols(start, end, symbols)
    except Exception:
        print("\nUsage example: pull.py 01-15-2020 02-25-2020 AAPL TSLA\n")
        raise



#random_sample(40).to_csv('sample_new.csv')

run(sys.argv).to_csv('test_27.csv')
