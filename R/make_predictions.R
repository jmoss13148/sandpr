##' @description Make preditions from a Keras NN model
##' 
##' @param data list object returned from train_nn.R 
##' @param epoch the epoch that will be used to calibrate the weights 
##' 
##' @export 

make_predictions <- function(data,
                             epoch,
                             type = "long short") {

    model = data[["model"]]
    test_x = data[["test_x"]]
    test_y = data[["test_y"]]
    
    ## Load the weights from the model at the specified epoch 
    model %>%
        load_model_weights_hdf5(filepath = sprintf("data/training_runs/weights.%s.%d.hdf5", gsub(" ", "", type), epoch))
    
    ## Make predictions with the model weights at this epoch 
    predictions = model %>% predict(as.matrix(test_x))
    
    ## Start Tensorflow session
    sess = tf$Session()
    
    ## Calculate average yearly return
    if(type == "long short") {
        test_returns = sandpr::returns_long_short(y_true = tf$constant(as.matrix(test_y)),
                                                  y_pred = tf$constant(predictions))
    } else if(type == "long only") {
        test_returns = sandpr::returns_long_only(y_true = tf$constant(as.matrix(test_y)),
                                                  y_pred = tf$constant(predictions))
    } else {stop("Enter a correct parameter")
    }
    
    test_returns_real = sess$run(test_returns)
    
    return(list("predictions" = predictions, "average_returns" = test_returns_real))
}




