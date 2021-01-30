## Portfolio Optimization in R
# https://www.codingfinance.com/post/2018-05-31-portfolio-opt-in-r/

library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyr)
library(forcats)
tick <- c('AMZN', 'AAPL', 'NFLX', 'XOM', 'T') #Seleccionamos las acciones

price_data <- tq_get(tick, #Conseguidmos los precios
                     from = '2014-01-01', #fecha de inicio de precios
                     to = '2018-05-31',   #fecha final de precios
                     get = 'stock.prices') 
log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily', #periodo de analisis
               col_rename = 'ret',
               type = 'log') # Obtenemos los precios de cierre, en formato tidy
head(log_ret_tidy)

log_ret_xts <- log_ret_tidy %>% #conversión de tidy a spread, xts
  spread(symbol, value = ret) %>%
  tk_xts()
head(log_ret_xts)

mean_ret <- colMeans(log_ret_xts) # ganancias medias diarias
print(round(mean_ret, 5))

cov_mat <- cov(log_ret_xts) * 252 # matriz de covarianzas

print(round(cov_mat,4))


##To calculate the portfolio returns and risk (standard deviation) we will us need

### Mean assets returns
### Portfolio weights
### Covariance matrix of all assets
### Random weights
## Lets create random weights first.

wts <- runif(n = length(tick))
print(wts)
print(sum(wts))

## We created some random weights, but the problem is that their sum is more than 1. We can fix this as shown below.

wts <- wts/sum(wts)
print(wts)

## Next we will calculate the annualized portfolio returns.

port_returns <- (sum(wts * mean_ret) + 1)^252 - 1
## Next we will calculate the portfolio risk (Standard deviation). 
## This will be annualized Standard deviation for the portfolio. 
## We will use linear algebra to calculate our portfolio risk.

port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
print(port_risk)

##Next we will assume 0% risk free rate to calculate the Sharpe Ratio.
# Since Risk free rate is 0% 

sharpe_ratio <- port_returns/port_risk
print(sharpe_ratio)

### We have everything we need to perform our optimization. 
### All we need now is to run this code on 5000 random portfolios. 
### For that we will use a for loop.

## Before we do that, we need to create empty vectors and matrix for storing our values.

num_port <- 5000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)

## Next run the loop 5000 times

for (i in seq_along(port_returns)) {
  
  wts <- runif(length(tick))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

##All the heavy lifting has been done and now we can create a data table to store all the values together.

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

head(portfolio_values)

## Next lets look at the portfolios that matter the most.

min_var <- portfolio_values[which.min(portfolio_values$Risk),] # The minimum variance portfolio
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),] # The tangency portfolio (the portfolio with highest sharpe ratio)

p <- min_var %>% #gráfica de menor variacion
  gather(AAPL:XOM, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

p <- max_sr %>% # gráfica portafolio tangente
  gather(AAPL:XOM, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

p <- portfolio_values %>% #todos los portafolios y su frontera de eficiencia
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red') +
  annotate('text', x = 0.20, y = 0.42, label = "Tangency Portfolio") +
  annotate('text', x = 0.18, y = 0.01, label = "Minimum variance portfolio") +
  annotate(geom = 'segment', x = 0.14, xend = 0.135,  y = 0.01, 
           yend = 0.06, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.22, xend = 0.2275,  y = 0.405, 
           yend = 0.365, color = 'red', arrow = arrow(type = "open"))


ggplotly(p)








