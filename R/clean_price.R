##' @description function the clean the price dataset for Sharpe ratio calcuations 
##' 
##' @export

clean_price = function(x) {
    
    ## Get variables of interest
    x = x %>% select(date, symbol, close) %>% arrange(symbol)

    ## Calculate the 1 day forward percentage change
    x = mutate(x, Row = 1:n()) %>%
            group_by(symbol) %>%
                mutate(Percentage_Change = (lead(close) - close) / close) %>%
                    ungroup() %>% 
                        select(-Row)
    
    ## Change NA values to 0
    x[is.na(x)] = 0
    
    return(x)
}

