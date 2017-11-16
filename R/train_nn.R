##' @description 
##' 
##' 
##' 
##' @return dataframe - maybe graphs 
##' @export 

train_nn = function(x, train_proportion, layers = c(), dropout_rate = 0.2) { 

## First we should split the data into training and test sets 
train_size = floor(train_proportion * nrow(x))

## Training indices 
train_indices = sample(1:nrow(x), size = train_size)

train = x[train_indices, ]
test = x[-train_indices, ]

## Split into predictor and response datasets and select correct columns
train_x = select(train, -one_year_price, -Ticker.Symbol, -Period.Ending)
train_y = select(train, one_year_price, -Period.Ending)

test_x = select(test, -one_year_price, -Ticker.Symbol, -Period.Ending)
test_y = select(test, one_year_price, -Period.Ending)

model <- keras_model_sequential() 
model %>% 
    layer_dense(units = layers[1], activation = "relu", input_shape = c(75)) %>% 
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = layers[2], activation = "relu") %>% 
    layer_dropout(rate = dropout_rate) %>%
    
    layer_dense(units = 1)
    
# for(i in 1:(length(layers) - 1)) {
#     layer_dense(model, units = layers[i + 1], activation = "relu") %>%
#     layer_dropout(rate = dropout_rate) 
# }

## Final layer - should have output of 1
# model %>%
#     layer_dense(units = 1)

# Define custom metric using backend tensor functions
K <- backend()

returns = function(y_true, y_pred) {
    
    ## Find integer value for top 20% of stocks 
    number = K$tf$cast(K$tf$multiply(K$tf$constant(0.1), K$tf$cast(K$tf$size(y_pred), dtype = K$tf$float32)), dtype = K$tf$int32)
    
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
    
    return(K$tf$reduce_mean(returns))
}

## Compile the model
model %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam(lr = 0.0001),
    metrics = c("Average_Returns" = returns))

## Train the model 
history <- model %>% fit(
    as.matrix(train_x), as.matrix(train_y), 
    epochs = 5000, batch_size = 32, 
    validation_split = 0.25
)

plot(history)

predictions = model %>% predict(as.matrix(test_x))

history_df <- as.data.frame(history)

}








