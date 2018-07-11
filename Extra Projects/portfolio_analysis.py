# -*- coding: utf-8 -*-
"""
This is a joint project done by Michael Gutierrez, Michelle Li and Nelson Tian
on optimizing a stock investment portfolio with stock data extracted from Yahoo! Finance
and presented to the members of Hylander Financial Group (HFG)
"""
# inline in console
# %matplotlib inline

# packages
import pandas_datareader.data as web
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# stocks used
stocks = ['BIO','BSCM','FCOM','FDIS','FEN','FMAT','FNCL','FREL',
          'FTEC','FUTY','HII','SKYW']

# extract data, date of extraction is 1/5/2018
data = web.DataReader(stocks,data_source='yahoo',start='01/01/2016')['Adj Close']
reversed_data = data.iloc[::-1]

# find the percent change in each stock
returns = reversed_data.pct_change()
mean_daily_returns = returns.mean()
cov_mat = returns.cov()
# number of simulations
num_portfolios = 25000

# the current portfolio allocation
results = np.zeros((4+len(stocks)-1,num_portfolios))
current_port_val = data.iloc[1] * [26,950,115,153,39,200,175,124,107,218,25,135]
current_port_val
current_port_alloc = current_port_val / current_port_val.sum()
# rename to cweights
cweights = current_port_alloc
cweights

# find the current allocation's return and sharpe
mean_daily_returns = returns.mean()
cov_matrix = returns.cov()
current_port_return = round(np.sum(mean_daily_returns * cweights) * 252,2)
current_port_std_dev = round(np.sqrt(np.dot(cweights.T,np.dot(cov_matrix, cweights))) * np.sqrt(252),2)
current_port_sharpe = current_port_return / current_port_std_dev
current_port_return
current_port_sharpe

# loop for all simulations
for i in range(num_portfolios):
    # randomize weights
    weights = np.array(np.random.random(12))
    weights = weights / np.sum(weights)
    # find returns and volatility for current allocation
    portfolio_return = np.sum(mean_daily_returns * weights) * 252
    portfolio_std_dev = np.sqrt(np.dot(weights.T,np.dot(cov_mat, weights))) * np.sqrt(252)
    # save the current allocation
    results[0,i] = portfolio_return
    results[1,i] = portfolio_std_dev
    results[2,i] = results[0,i] / results [1, i]
    
    for j in range(len(weights)):
        results[j+3,i] = weights[j]

# after simulation, save to dataframe
results_frame = pd.DataFrame(results.T, 
                             columns=['ret','stdev','sharpe', stocks[0],stocks[1],stocks[2],stocks[3],
                                      stocks[4],stocks[5],stocks[6],stocks[7],stocks[8],stocks[9],
                                      stocks[10],stocks[11]])
# find the allocations with the largest and smallest sharpe ratio
max_sharpe_port = results_frame.iloc[results_frame['sharpe'].idxmax()]
min_sharpe_port = results_frame.iloc[results_frame['sharpe'].idxmin()]

# make a scatter of volatility vs. returns for the simulation
plt.scatter(results_frame.stdev,results_frame.ret,c=results_frame.sharpe,cmap='RdYlBu')
plt.xlabel('Volatility')
plt.ylabel('Returns')
plt.colorbar()
# mark both the optimized and the current allocations
plt.scatter(current_port_std_dev,current_port_return,marker=(5,1,0),color='y',s=300)
plt.scatter(max_sharpe_port[1],max_sharpe_port[0],marker=(5,1,0),color='r',s=300)



