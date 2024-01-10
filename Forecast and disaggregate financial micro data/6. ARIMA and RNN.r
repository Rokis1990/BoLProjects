# ARIMA and RNN

# https://www.r-bloggers.com/2020/05/time-series-with-arima-and-rnn-models/

ssh <- suppressPackageStartupMessages
ssh(library(timeSeries))
ssh(library(tseries))
ssh(library(aTSA))
ssh(library(forecast))
ssh(library(rugarch))
ssh(library(ModelMetrics))
ssh(library(keras))


data(USDCHF)
length(USDCHF)

data(USDCHF)
data <- ts(USDCHF, frequency = 365)
plot(data)

adf.test(data)
pp.test(data )

acf(data)

pacf(data)

data_test <- data[(length(data)-99):length(data)]
data_train <- data[1:(length(data)-99-1)]
model_arima <- auto.arima(data_train)
summary(model_arima)

checkresiduals(model_arima)

arch.test(arima(data_train, order = c(0,1,2)))


model <- character()
AIC <- numeric()
for (p in 1:5){
  for(q in 1:5){
    model_g <- tseries::garch(model_arima$residuals, order = c(p,q), trace=F)
    model<-c(model,paste("mod_", p, q))
    AIC <- c(AIC, AIC(model_g))
    def <- tibble::tibble(model,AIC)
  }
}

def %>% dplyr::arrange(AIC)


model_garch <- tseries::garch(model_arima$residuals, order = c(1,1), trace=F)
## Warning in tseries::garch(model_arima$residuals, order = c(1, 1), trace = F):
## singular information
Box.test(model_garch$residuals)

acf(model_garch$residuals[-1])

garch1 <- ugarchspec(mean.model = list(armaOrder = c(0,2), include.mean = FALSE), 
                      variance.model = list(garchOrder = c(1,1))) 


Ddata_train <- diff(data_train)
garchfit <- ugarchfit(data=Ddata_train, spec = garch1, solver = "gosolnp",trace=F)
coef(garchfit)


fitted <- ugarchforecast(garchfit, n.ahead=100)
yh_test<-numeric()
for (i in 2:100){
  yh_test[1] <- data_train[length(data_train)]+fitted(fitted)[1]
  yh_test[i] <- yh_test[i-1]+fitted(fitted)[i]
}
df_eval <- tibble::tibble(y_test=data_test, yh_test=yh_test)
df_eval
plot(df_eval)
#write.csv(df_eval, "df_eval.csv")

######################
# RNN

maxlen <- 7
exch_matrix<- matrix(0, nrow = length(data_train)-maxlen-1, ncol = maxlen+1) 

for(i in 1:(length(data_train)-maxlen-1)){
  exch_matrix[i,] <- data_train[i:(i+maxlen)]
}
head(exch_matrix)  

x_train <- exch_matrix[, -ncol(exch_matrix)]
y_train <- exch_matrix[, ncol(exch_matrix)]


dim(x_train)

x_train <- array_reshape(x_train, dim = c((length(data_train)-maxlen-1), maxlen, 1))
dim(x_train)


model <- keras_model_sequential()
model %>% 
  layer_dense(input_shape = dim(x_train)[-1], units=maxlen) %>% 
  layer_simple_rnn(units=10) %>% 
  layer_dense(units = 1)
summary(model)


model %>% compile(
  loss = "mse",
  optimizer= "adam",
  metric = "mae" 
)
history <- model %>% 
  fit(x_train, y_train, epochs = 5, batch_size = 32, validation_split=0.1)


#save_model_hdf5(model, "rnn_model.h5")
rnn_model <- load_model_hdf5("rnn_model.h5")


####
# Prediction

maxlen <- 7
exch_matrix2<- matrix(0, nrow = length(data)-maxlen-1, ncol = maxlen+1) 
for(i in 1:(length(data)-maxlen-1)){
  exch_matrix2[i,] <- data[i:(i+maxlen)]
}
x_train2 <- exch_matrix2[, -ncol(exch_matrix2)]
y_train2 <- exch_matrix2[, ncol(exch_matrix2)]
x_train2 <- array_reshape(x_train2, dim = c((length(data)-maxlen-1), maxlen, 1))
pred <- rnn_model %>% predict(x_train2)
df_eval_rnn <- tibble::tibble(y_rnn=y_train2[(length(y_train2)-99):length(y_train2)],
                              yhat_rnn=as.vector(pred)[(length(y_train2)-99):length(y_train2)])

# Compare results

df_eval <- read.csv("df_eval.csv")
rmse <- c(rmse(df_eval$y_test, df_eval$yh_test), 
          rmse(df_eval_rnn$y_rnn, df_eval_rnn$yhat_rnn) )
mae <- c(mae(df_eval$y_test, df_eval$yh_test), 
         mae(df_eval_rnn$y_rnn, df_eval_rnn$yhat_rnn) )
df <- tibble::tibble(model=c("ARIMA", "RNN"), rmse, mae)
df

plot.ts(pred)


####################
# LB data

#data <- ts(USDCHF, frequency = 365)
data <- ts(y, frequency = 4)
plot(data)

adf.test(data)
pp.test(data )

acf(data)

pacf(data)


data_test <- data[(length(data)-8):length(data)]
data_train <- data[1:(length(data)-8-1)]
model_arima <- auto.arima(data_train)
summary(model_arima)

checkresiduals(model_arima)

arch.test(arima(data_train, order = c(0,1,2)))


model <- character()
AIC <- numeric()
for (p in 1:5){
  for(q in 1:5){
    model_g <- tseries::garch(model_arima$residuals, order = c(p,q), trace=F)
    model<-c(model,paste("mod_", p, q))
    AIC <- c(AIC, AIC(model_g))
    def <- tibble::tibble(model,AIC)
  }
}

def %>% dplyr::arrange(AIC)


model_garch <- tseries::garch(model_arima$residuals, order = c(1,1), trace=F)
## Warning in tseries::garch(model_arima$residuals, order = c(1, 1), trace = F):
## singular information
Box.test(model_garch$residuals)

acf(model_garch$residuals[-1])

garch1 <- ugarchspec(mean.model = list(armaOrder = c(0,2), include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1,1))) 


Ddata_train <- diff(data_train)
garchfit <- ugarchfit(data=Ddata_train, spec = garch1, solver = "gosolnp",trace=F)
coef(garchfit)


fitted <- ugarchforecast(garchfit, n.ahead=100)
yh_test<-numeric()
for (i in 2:100){
  yh_test[1] <- data_train[length(data_train)]+fitted(fitted)[1]
  yh_test[i] <- yh_test[i-1]+fitted(fitted)[i]
}
df_eval <- tibble::tibble(y_test=data_test, yh_test=yh_test)
df_eval

#write.csv(df_eval, "df_eval.csv")


