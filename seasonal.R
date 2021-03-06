library(ggplot2)
library(lubridate)
library(ggpubr)
library(ggpmisc)
library(plotly)
library(forecast)


decomp_function <- function(x){
  ts1 <- ts(x, freq=365.25/7, start=decimal_date(ymd("2017-01-08")))
  stl(ts1, s.window=13, l.window=13, outer=0, inner = 1, t.window=23)
}


fwdata$Date <- dmy(fwdata$Date)

Net_Sales_ts <- decomp_function(fwdata$Net_Sales)
Avg_Basket_ts <- decomp_function(fwdata$Avg_Basket)
Avg_Price_ts <- decomp_function(fwdata$Avg_Price)
Cr_Appr_ts <- decomp_function(fwdata$CR_Approved)


fwdata$Net_Sales_Seasonal <- as.integer(Net_Sales_ts$time.series[,1])
fwdata$Avg_Basket_Seasonal <- as.integer(Avg_Basket_ts$time.series[,1])
fwdata$Avg_Price_Seasonal <- as.integer(Avg_Price_ts$time.series[,1])
fwdata$Cr_Appr_Seasonal <- as.numeric(Cr_Appr_ts$time.series[,1])



fwdata$Net_Sales_sw <- as.integer(Net_Sales_ts$time.series[,1] + Net_Sales_ts$time.series[,2])
fwdata$Avg_Basket_sw <- as.integer(Avg_Basket_ts$time.series[,1] + Avg_Basket_ts$time.series[,2])
fwdata$Avg_Price_sw <- as.integer(Avg_Price_ts$time.series[,1] + Avg_Price_ts$time.series[,2])
fwdata$Cr_Appr_sw <- as.numeric(Cr_Appr_ts$time.series[,1] + Cr_Appr_ts$time.series[,2])



obs_plot <- function(m, n){
  ggplot(data=fwdata, aes(x=Date))+
    geom_point(aes(y=m, color = Events), size=3)+
    geom_line(aes(y=m))+
    scale_x_date(date_labels = "%b.%Y", date_breaks='1 week')+
    theme_classic()+
    stat_smooth(aes(y=m), color = "#FC4E07", fill = "#FC4E07",
                method = "loess")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_line(aes(y=n), alpha = 1/6, size=5)
}

season_plot <- function(f){
  ggplot(f, as.numeric=FALSE)+ geom_line(alpha = 1/3, size=1)+
    stat_peaks(colour='red')+
    stat_peaks(geom='text', colour='red', vjust = -0.5, x.label.fmt = "%d.%m.%Y")+
    stat_valleys(colour='blue')+
    stat_valleys(geom='text', colour='blue', vjust = 1.5, hjust=1, angle=45, x.label.fmt = "%d.%m.%Y")+
    scale_y_continuous(name="Seasonal changes")+
    theme_classic()
}


obs_net_sales <- ggplotly(obs_plot(fwdata$Net_Sales, fwdata$Net_Sales_sw)+ggtitle('Íàáëþäàåìûå èçìåíåíèÿ Net Sales & Seasonal Changes'))
seasonal_net_sales <- ggplotly(season_plot(Net_Sales_ts$time.series[,1]))


net_sales_plot <- subplot(
  obs_net_sales, 
  seasonal_net_sales, 
  nrows=2, 
  margin = 0.05, titleX=TRUE)



obs_avg_basket <- ggplotly(obs_plot(fwdata$Avg_Basket, fwdata$Avg_Basket_sw)+ggtitle('Íàáëþäàåìûå èçìåíåíèÿ Average Basket & Seasonal Changes'))
seasonal_avg_basket <- ggplotly(season_plot(Avg_Basket_ts$time.series[,1]))


avg_basket_plot <- subplot(
  obs_avg_basket, 
  seasonal_avg_basket, 
  nrows=2, 
  margin = 0.05, titleX=TRUE)



obs_avg_price <- ggplotly(obs_plot(fwdata$Avg_Price, fwdata$Avg_Price_sw)+ ggtitle('Íàáëþäàåìûå èçìåíåíèÿ Average Price & Seasonal Changes'))
seasonal_avg_price <- ggplotly(season_plot(Avg_Price_ts$time.series[,1]))


avg_price_plot <- subplot(
  obs_avg_price, 
  seasonal_avg_price, 
  nrows=2, 
  margin = 0.05, titleX=TRUE)


obs_cr_appr <- ggplotly(obs_plot(fwdata$CR_Approved, fwdata$Cr_Appr_sw)+ ggtitle('Íàáëþäàåìûå èçìåíåíèÿ Conversation Rate Approved & Seasonal Changes'))
seasonal_cr_appr <- ggplotly(season_plot(Cr_Appr_ts$time.series[,1]))


cr_approved_plot <- subplot(
  obs_cr_appr, 
  seasonal_cr_appr, 
  nrows=2, 
  margin = 0.05, titleX=TRUE)


y <- fwdata$Net_Sales
ARIMAfit <- auto.arima(y, approximation=FALSE, trace=FALSE)
summary(ARIMAfit)
pred <- predict(fwdata$Net_Sales)
pred$pred
