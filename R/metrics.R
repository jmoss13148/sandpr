##' @description Average returns for long only portfolio
##' 
##' @export

returns_long_only <- function(y_true, y_pred) {
    
    ## Find integer value for top 20% of stocks 
    number = K$tf$cast(K$tf$multiply(K$tf$constant(0.10), K$tf$cast(K$tf$size(y_pred), dtype = K$tf$float32)), dtype = K$tf$int32)
    
    ## Get indices and values for top 20% of predictions
    top = K$tf$nn$top_k(input = K$tf$squeeze(y_pred), k = number)
    
    ## Values for top predictions
    top_values = K$tf$gather(params = y_pred, indices = top$indices)
    
    ## Weight factor for porKolio (equal weighting between top 20%)
    weight_factor = K$tf$divide(K$tf$constant(1), K$tf$cast(number, dtype = K$tf$float32))
    
    ## True return values for the top predictions
    return_values = K$tf$gather(params = y_true, indices = top$indices)
    
    ## Real returns with this porKolio 
    returns = K$tf$multiply(weight_factor, K$tf$cast(return_values, dtype = K$tf$float32))
    
    return(K$tf$reduce_sum(returns))
}

##' @description Average returns for a long short portolio
##' 
##' @export

returns_long_short <- function(y_true, y_pred) {
    
    ## Find integer value for top % of stocks 
    number = K$tf$cast(K$tf$multiply(K$tf$constant(0.05), K$tf$cast(K$tf$size(y_pred), dtype = K$tf$float32)), dtype = K$tf$int32)
    
    ## Get indices and values for top % of predictions
    top = K$tf$nn$top_k(input = K$tf$squeeze(y_pred), k = number)
    
    ## Get indices and values for the bottom % of predictions 
    bottom = K$tf$nn$top_k(input = K$tf$squeeze(-y_pred), k = number)
    bottom_vals = K$tf$negative(bottom$values)
    bottom_indices = bottom$indices
    
    ## Values for top predictions
    top_values = K$tf$gather(params = y_pred, indices = top$indices)
    
    ## Values for bottom predictions 
    bottom_values = K$tf$gather(params = y_pred, indices = bottom_indices)
    
    ## Weight factor for portfolio (equal weighting between top %/2 and bottom %/2)
    weight_factor = K$tf$divide(K$tf$constant(0.50), K$tf$cast(number, dtype = K$tf$float32))
    
    ## True return values for the top predictions
    return_values_top = K$tf$gather(params = y_true, indices = top$indices)
    
    ## True return values for the bottom predictions
    return_values_bottom = K$tf$gather(params = y_true, indices = bottom_indices)
    
    ## Real returns with this portfolio 
    total_returns_top = K$tf$multiply(weight_factor, K$tf$cast(return_values_top, dtype = K$tf$float32)) %>% 
        K$tf$reduce_sum()
    total_returns_bottom = K$tf$multiply(tf$negative(weight_factor), K$tf$cast(return_values_bottom, dtype = K$tf$float32)) %>% 
        K$tf$reduce_sum()
    
    ## Total returns
    return(tf$add(total_returns_top, total_returns_bottom))
}


