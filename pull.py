import requests
import datetime
import pandas as pd
import sys

import settings


def fetch_range(symbol, start, end):
    params = {
        'symbol': symbol,
        'resolution': 'D', # we only need per-day data
        'from': date_to_timestamp(start),
        'to': date_to_timestamp(end),
        'token': settings.FINNHUB_API_KEY
    }
    return request(params)

def date_to_timestamp(date_string):
    date = datetime.datetime.strptime(date_string, "%m-%d-%Y")
    stamp = datetime.datetime.timestamp(date) 
    return int(stamp)

def request(params):
    r = requests.get('https://finnhub.io/api/v1/stock/candle', params=params)
    print("DEBUG: Request: " + r.url)
    return r.json()

def format(results):
    data = {
        'low': results['l'],
        'high': results['h'],
        'volume': results['v'],
        'timestamp': results['t']
    }
    return pd.DataFrame(data=data)

def run(args):
    try:
        symbol = args[1]
        start = args[2]
        end = args[3]
        return format(fetch_range(symbol=symbol, start=start, end=end))
    except Exception as e:
        print("Usage example: pull.py AAPL 01-15-2020 02-25-2020 \n")
        raise

print(run(sys.argv))