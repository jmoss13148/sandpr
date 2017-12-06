##' @description Train a NN model using the Keras framework and Tensorflow backend 
##' 
##' @return dataframe - maybe graphs 
##' 
##' @param x processed fundamentals dataset 
##' 
##' @import keras
##' @import tensorflow
##' @import ggplot2
##' 
##' @export 

train_nn = function(x, 
                    layers                 = c(50), 
                    portfolio              = "long short",
                    dropout_rate           = 0.2,
                    activation             = "relu",
                    loss_function          = "mean_squared_error",
                    optimizer              = optimizer_adam,
                    learning_rate          = 0.0001,
                    n_epochs               = 50,
                    batch_size             = 32,
                    validation_prop        = 0.1,
                    seed                   = 42
                    ) { 
    
    ## For the purpose of reproducible results
    sess <- tf$Session(graph = tf$get_default_graph())
    
    ## Instruct Keras to use this session
    K = backend()
    K$set_session(sess)
    
    ## Reproducibility 
    tf$set_random_seed(seed)
    
    ## Use previous data to predict any stock in the future 
    ## Order in ascending date
    x = x %>% arrange(Period.Ending)
        
    ## Train before 2015, test 2015 
    train = filter(x, Period.Ending <= "2015-01-01")
    test = filter(x, Period.Ending >= "2015-01-01")
        
    ## Get test stocks for later analysis
    test_stocks = test$Ticker.Symbol
    test_dates = test$Period.Ending
    
    ## Split into predictor and response datasets and select correct columns
    train_x = select(train, -one_year_price, -Ticker.Symbol, -Period.Ending)
    train_y = select(train, one_year_price, -Period.Ending)
    
    test_x = select(test, -one_year_price, -Ticker.Symbol, -Period.Ending)
    test_y = select(test, one_year_price, -Period.Ending)
    
    model <- keras_model_sequential() 
    model %>% 
        ## First and second layers
        layer_dense(units = layers[1], 
                    activation = activation, 
                    input_shape = c(ncol(train_x)),
                    kernel_initializer = initializer_random_uniform(minval = -0.15, maxval = 0.15, seed = seed)) %>% 
        layer_dropout(rate = dropout_rate) 
    
    ## Rest of layers 
    if(length(layers) >= 2){
        
        ## Add as many new layers as we need
        for(i in 1:(length(layers) - 1)) {
        model %>%
            layer_dense(units = layers[i + 1],
                        activation = activation,
                        kernel_initializer=initializer_random_uniform(minval = -0.15, maxval = 0.15, seed = seed)) %>% 
            layer_dropout(rate = dropout_rate)
        }
    }
    
    ## Output layer 
    model %>% 
        layer_dense(units = 1,
                    kernel_initializer=initializer_random_uniform(minval = -0.15, maxval = 0.15, seed = seed))
    
    if(portfolio == "long only") {
        returns_metric = sandpr::returns_long_only
    } else if(portfolio == "long short") {
        returns_metric = sandpr::returns_long_short
    }
   
    ## Compile the model
    model %>% compile(
        loss = loss_function,
        optimizer = optimizer(lr = learning_rate),
        metrics = c("Annualized_Return" = returns_metric))
    
    ## In order to save the best 
    checkpointer = callback_model_checkpoint(filepath = sprintf("data/training_runs/weights.%s.{epoch:02d}.hdf5", gsub(" ", "", portfolio_type)),
                                             monitor = c("Average_Yearly_Return"),
                                             verbose = 1, 
                                             save_best_only = F)
    
    ## Train the model 
    history <- model %>% fit(
        as.matrix(train_x), as.matrix(train_y), 
        epochs = n_epochs, batch_size = batch_size, 
        validation_split = validation_prop,
        callbacks = checkpointer
    )
    
    ## Convert to dataframe 
    history_df <- as.data.frame(history)
    
    return(list("model"       = model, 
                "history"     = history, 
                "history_df"  = history_df,
                "test_x"      = test_x,
                "test_y"      = test_y,
                "test_stocks" = test_stocks,
                "test_dates"  = test_dates))

}








