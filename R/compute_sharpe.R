##' @description compute the Sharpe ratio based upon daily returns 
##' 
##' @param x a matrix of daily returns for the stocks of interest
##' @param weights a matrix of weights for the stocks of interest 
##' 
##' @export

compute_sharpe = function(data,
                          preds,
                          prices_data = cleaned_price_data,
                          type        = "long short") {
    
    ## Build first datafame - stocks and weights 
    ## Want top and bottom 10% of stocks 
    
    stocks = data.frame(ticker = data[["test_stocks"]],
                        test_date = data[["test_dates"]],
                        prediction = preds[["predictions"]])
    stocks$ticker = as.character(stocks$ticker)
    
    if(type == "long short") {
        
        ## Lowest to highest 
        stocks = arrange(stocks, prediction)
        
        ## 5 % of the total number of stocks in portfolio 
        number = floor(0.05 * nrow(stocks))
        
        ## Get indices
        short_indices = 1:number
        long_indices = (nrow(stocks) - number + 1):nrow(stocks)
        
        ## Get weight value
        weight_value = 0.5 / number
        weights = c(rep(-weight_value, number), rep(weight_value, number))
        
        ## Subset for indices 
        sharpe_data_one = stocks[c(short_indices, long_indices), ] %>% 
            mutate(weight = weights)
    } else if(type == "long only") {
        
        ## Highest to lowest 
        stocks = arrange(stocks, desc(prediction))
        
        ## 10% of the total number of stocks in portfolio 
        number = floor(0.10 * nrow(stocks))
        
        ## Highest 10% of predictions 
        index = 1:(number)
        
        ## Get weight value
        weight_value = 1 / number
        weights = rep(weight_value, number)
        
        ## Subset for indices 
        sharpe_data_one = stocks[index, ] %>% 
            mutate(weight = weights)
    }

    sharpe_data = sharpe_data_one

    ## Only keep information from this date
    weights_data = sharpe_data %>% arrange(ticker)
    tickers = weights_data$ticker
    weights = weights_data$weight
    
    ## Get return matrix with one year of returns and stocks we want 
    returns = prices_data %>% filter(date >= as.Date("2015-12-31") & date <= as.Date("2015-12-31") + 365,
                                     symbol %in% tickers) %>% 
        arrange(symbol)
    
    returns_ = returns %>% select(-close) %>% 
        spread(returns, key = date, value = Percentage_Change) 
    
    ## Allocate vector for returns 
    return_values = numeric()
    
    for(i in 2:ncol(returns_)) {
        
        test = data.frame(ticker = tickers,
                          myweights = weights, 
                          returns = returns_[, i] %>% unlist()) %>%
                            mutate(adjusted_returns = ifelse(myweights >= 0, returns + 1, returns - 1)) %>% 
                            mutate(real_return = myweights * adjusted_returns) %>%
                            mutate(new_weight = ifelse(myweights >= 0, real_return, -real_return))
        
        ## Return for the day
        day_return = ( sum(test$real_return) - sum(abs(test$myweights)) ) / sum(abs(test$myweights))
    
        return_values = c(return_values, day_return)
        
        ## Update the weights
        weights = test$new_weight
        
    }
    
    num_days = length(return_values)
    
    ## Compute the Sharpe ratio
    #sharpe = mean(return_values) / sd(return_values) * sqrt(num_days)
    
    ## Use return value from preds 
    yearly_return = preds$average_returns
    daily_sd      = sd(return_values)
    yearly_sd     = daily_sd * sqrt(num_days)
    sharpe        = yearly_return / yearly_sd
    
    
    return(list("yearly_return" = yearly_return,
                "yearly_sd" = yearly_sd,
                "sharpe" = sharpe,
                "data" = sharpe_data_one))
}
    
    
    
    
    
    
    