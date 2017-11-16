##' @description process_data further prepares data for the NN 
##' 
##' 
##' 
##' @return a dataframe 


process_data = function(x, y) {
    
    ## First, make sure our dates are in the same format 
    ## Already have "%Y-%m-%d" in fundamentals data 
    y$date = as.Date(as.character(y$date), format = "%m/%d/%y")
    
    changes = numeric()
    
    ## Loop through each row, get the date, then find the price one year in the future 
    for(i in 1:nrow(x)) {
        
        this_date = x[i, ]$Period.Ending
        this_symbol = x[i, ]$Ticker.Symbol
        
        ## Get the % change in price 1 year in the future 
        price_now = filter(y, date == this_date, symbol == this_symbol) %>% select(close)
        price_future = filter(y, date == (this_date + 365), symbol == this_symbol) %>% select(close)
        
        ## This might not always work (weekends etc.)
        ## Keep trying the previous day until we get one that exists 
        j = 1
        while(nrow(price_now) == 0) {
            
            ## Keep trying one day in the future 
            price_now = filter(y, date == (this_date + j), symbol == this_symbol) %>% select(close)
            j = j + 1
            
            print(sprintf("Now Price %i row", i))
            print(sprintf("Now Price %i try", j))
        }
        
        k = 1
        while(nrow(price_future) == 0) {
            
            ## Keep trying one day in the past 
            price_future = filter(y, date == (this_date + 365 - k), symbol == this_symbol) %>% select(close)
            k = k + 1
            
            print(sprintf("Future Price %i row", i))
            print(sprintf("Future Price %i try", k))
        }
        
    ## Calculate percentage change
    percent_change = (price_future[1,1] - price_now[1,1]) / price_now[1,1]
    
    ## How to deal with stock splits 
    ## Assume this means more than 50% decrease 
    ## Let's just set to 0% change 
    if(percent_change <= -0.50) {
        percent_change = 0
    }
    
    changes = c(changes, percent_change)
    }
    
    ## Append this to dataframe 
    x$one_year_price = changes 
    
    return(x)
}
