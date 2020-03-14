import requests
import settings

params = {
    'symbol': 'AAPL',
    'resolution': 'D', # we only need per-day data
    'from': '1521056462',
    'to': '1540992600',
    'token': settings.FINNHUB_API_KEY
}

r = requests.get('https://finnhub.io/api/v1/stock/candle', params=params)

print(r.json())