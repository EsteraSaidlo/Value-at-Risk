# Value-at-Risk

There are many methods for measuring currency risk. The most common is the Value at Risk(VaR) method. It uses statistical analysis of past exchange rate volatility to estimate the risk to which an investment is exposed. It determines with a high degree of probability the maximum loss to which an investment is exposed in a given time horizon. However, it does not tell us what is the expected value of the loss if it exceeds VaR. Estimated Shortfall, or conditional Value at Risk, is used for this. When calculating the value of VaR, it should be taken into account that the higher the confidence level and time horizon, the higher the value of VaR will be. It will also vary depending on the method used. The aim of this work is to use three different methods to calculate the VaR value and check with a back-test which of the methods gave the best VaR value.

The methods used:
- Historical,
- Historical with weights,
- Bootstrap,
- EWMA.

The analysis was conducted on the basis of exchange rates:
- EURO,
- THB (Thai Baht),
- UAH (Ukrainian Hryvnia).

# Technology
- R
