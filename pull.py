import requests
import datetime

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
    date = datetime.datetime.strptime(date_string, "%d-%m-%Y")
    stamp = datetime.datetime.timestamp(date) 
    return int(stamp)

def request(params):
    r = requests.get('https://finnhub.io/api/v1/stock/candle', params=params)
    print("DEBUG: Request: " + r.url)
    return r.json()

one_month = datetime.timedelta(days=30)
today = datetime.date.today()

results = fetch_range('AAPL', start = '01-01-2020', end = '01-02-2020')
print(results)