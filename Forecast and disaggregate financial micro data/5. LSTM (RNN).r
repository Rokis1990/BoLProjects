# tensorflow::install_tensorflow()
# keras::install_keras()

library(keras)
library(tensorflow)

reticulate::virtualenv_create("keras-tf")
reticulate::install_keras(method="virtualenv", envname="keras-tf")

use_condaenv("keras-tf", required = T)

imdb <- dataset_imdb(num_words = 500)


c(c(train_x, train_y), c(test_x, test_y)) %<-% imdb
length(train_x); length(test_x)



model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = 500, output_dim = 32) %>%
  layer_simple_rnn(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid")


model %>% compile(optimizer = "rmsprop",
                  loss = "binary_crossentropy",
                  metrics = c("acc"))


history <- model %>% fit(train_x, train_y,
                         epochs = 25,
                         batch_size = 128,
                         validation_split = 0.2);beepr::beep()
plot(history)


model %>% evaluate(train_x, train_y) 
pred <- model %>%   
  predict_classes(train_x) 
table(Predicted=pred, Actual=imdb$train$y)   
 
model %>% evaluate(test_x, test_y) 

pred1 <- model %>%   
  predict_classes(test_x) 
table(Predicted=pred1, Actual=imdb$test$y) 
