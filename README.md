# EUR/CHF forecast

<p align="center">
<img src="https://github.com/aritzLizoain/EUR-CHF-forecast/blob/main/PRED.png" width="1300"/>
</p>

![GitHub last commit](https://img.shields.io/github/last-commit/aritzLizoain/EUR-CHF-forecast)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/aritzLizoain/EUR-CHF-forecast)
[![](https://tokei.rs/b1/github/aritzLizoain/EUR-CHF-forecast?category=lines)](https://github.com/aritzLizoain/EUR-CHF-forecast) 
![GitHub Repo stars](https://img.shields.io/github/stars/aritzLizoain/EUR-CHF-forecast?style=social)
![GitHub watchers](https://img.shields.io/github/watchers/aritzLizoain/EUR-CHF-forecast?style=social)

## :currency_exchange: [EUR/CHF Currency exchange rate forecasting](https://github.com/aritzLizoain/EUR-CHF-forecast/blob/main/Report.pdf)

The goal of this study is to fit an **ARIMA model** to forecast the EUR/CHF currency exchange rate. Different models are obtained through two trend removal methods: a linear model, and differencing. The analysis reveals that differencing is the most appropriate method, leading to a white noise residual series; consequently, an ARIMA(0,0,0) is fitted. The EUR/CHF exchange rate predictions are obtained back-transforming the residual time series forecast.
