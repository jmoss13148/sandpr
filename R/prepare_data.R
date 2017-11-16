##' @description Prepare data for neural network model 
##' 
##' @return a dataframe with the relavent columns 
##' @export

prepare_data = function(x, y) {

    ## Remove variables we don't want 
    x = x %>% select(-X, -For.Year)
    
    ## Coerce to correct classes
    x$Ticker.Symbol = as.character(x$Ticker.Symbol)
    y$symbol = as.character(y$symbol)
    
    ## Already have "%Y-%m-%d" in fundamentals data 
    y$date = as.Date(as.character(y$date), format = "%m/%d/%y")
    
    ## Convert date to date class
    x$Period.Ending = as.Date(as.character(x$Period.Ending), format = "%Y-%m-%d")
    
    ## Remove information for anything after 2015
    x = x %>% filter(Period.Ending <= "2015-12-31" & Period.Ending >= "2012-01-01")
    
    ## Only work with companies with price data that spans end of 2012 to end of 2016
    ## Get list of tickers that we want 
    companies = unique(y$symbol)
    companies_keep = character()
    
    lower_dates = as.Date(c("2013-12-31", "2013-12-30", "2013-12-29"), format = "%Y-%m-%d")
    upper_dates = as.Date(c("2016-12-31", "2016-12-30", "2016-12-29"), format = "%Y-%m-%d")
    
    ## Loop through all companies 
    for(i in 1:length(companies)) {
        
        this_company = filter(y, symbol == companies[i])
        this_company_dates = this_company$date
        
        ## If we have the lower and upper range of our
        if(sum(lower_dates %in% this_company_dates) > 0 && sum(upper_dates %in% this_company_dates) > 0) {
           companies_keep = c(companies_keep, companies[i]) 
        } 
    }
    
    ## Subset for the companies we want 
    x = x %>% filter(Ticker.Symbol %in% companies_keep)
    
    ## Deal with missing values 
    ## Set to average of dataset 
    columns = c("Cash.Ratio",
                "Current.Ratio",
                "Quick.Ratio",
                "Earnings.Per.Share",
                "Estimated.Shares.Outstanding")
    
    for(i in 1:length(columns)) {
        x[[columns[i]]][is.na(x[[columns[i]]])] = mean(x[[columns[i]]], na.rm = TRUE)
    }
    
    ## We should also normalize all fundamentals variables 
    ## Get column names of numeric variables 
    numeric_names = character()
    
    for(i in 1:(length(colnames(x)))) {
        
        if(is.numeric(x[, i])) {
            numeric_names = c(numeric_names, colnames(x[i]))
        }
    }
    
    ## Scale the numeric columns
    x <- x %>% mutate_each_(funs(scale(.) %>% as.vector), vars=numeric_names)
    
    return(x)
}
