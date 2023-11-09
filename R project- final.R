# In order to study the effectiveness of various marketing channels 
# and their impact on sales, this dataset has been used and it contains 
# information on the amount spent on TV sponsorships, advertising during 
# cricket broadcasts, TV Run-of-Network, radio, non-print media, magazines,
# out-of-home advertising, social media, and display advertising 
# across various websites. 
# In addition, this marketing dataset was collected on a monthly 
# basis for a period of over 16 years, from January 2001 until August 2017
# and contains information about sales within the company.

#imported the necessary libraries
library(gridExtra)
library(ggplot2)
library(corrplot)
library(MASS)
library(dplyr)
library(tidyr)
library(forecast)
library(PerformanceAnalytics)
library(Robyn)
library(mctest)
library(lmtest)

#read and explored the data
data <- read.csv("C:/Users/chels/Desktop/R language/mediamix_sales.csv")
View(data)

dim(data)
str(data)
summary(data)
colSums(is.na(data))
table(duplicated(data))

#converted the"Time" variable to a date format
data$Time <- as.Date(data$Time, "%d/%m/%Y")

# plot 1: sales over time
ggplot(data, aes(x = Time, y = sales)) +
  geom_smooth(method = "auto", se = FALSE) +
  labs(title = "Sales Over Time",
       x = "Date",
       y = "Amount") 

#plot 2: average advertising spend by channel
data_avg <- data %>% 
  gather(key = "channel", value = "spend", -Time, -sales) %>% 
  group_by(channel) %>% 
  summarize(avg_spend = mean(spend))

ggplot(data_avg, aes(x = channel, y = avg_spend, fill = channel)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Average Advertising Spend by Channel",
       x = "Channel",
       y = "Average Spend") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# plot 3: scatter plot of advertising channels versus sales

df<-data[,-1]
plot_data <- data[, c("sales", colnames(df)[-which(colnames(df) == "sales")])]
scatter_plots <- list()
for (col in colnames(plot_data)[-1]) {
  scatter_plots[[col]] <- ggplot(plot_data, aes_string(x = col, y = "sales")) +
    geom_point()
}

grid.arrange(grobs = scatter_plots, ncol = 3)

#plot 4 : boxplot for sales by year
ggplot(data, aes(x = factor(format(Time, "%Y")), y = sales)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("Sales") +
  ggtitle("Sales Distribution by Year")

#removed outliers for every year
data <- data %>%
  group_by(year = format(Time, "%Y")) %>%
  filter(!sales %in% boxplot.stats(sales)$out) %>%
  ungroup() %>%
  select(-year)


#plot 5 : histogram and qq plot
hist <- ggplot(data, aes(x = sales)) +
  geom_histogram() +
  xlab("Sales") +
  ylab("Frequency") +
  ggtitle("Histogram of Sales")


qq <- ggplot(data, aes(sample = sales)) +
  geom_qq() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  ggtitle("QQ Plot of Sales")


grid.arrange(hist, qq)

#plot 6: Time Series decomposition for all the dataset 
ts_data <- ts(data$sales, start = c(2001, 1), end = c(2017, 8), frequency = 12)
decomp_data <- decompose(ts_data, "multiplicative")
autoplot(decomp_data) +
  ggtitle("Time Series Decomposition") +
  theme(plot.title = element_text(hjust = 0.5))

#Time Series decomposition for a 2-year time window
ts_data_2 <- ts(data$sales, start = c(2004, 1), end = c(2005, 12), frequency = 12)
decomp_data_2 <- decompose(ts_data_2)
autoplot(decomp_data_2) +
  ggtitle("Time Series Decomposition") +
  theme(plot.title = element_text(hjust = 0.5))

# plot 7: heatmap of correlation matrix
corr_matrix <- cor(data[,sapply(data, is.numeric)])
corrplot(corr_matrix, method = "color", tl.col="black", tl.cex = 0.8)
chart.Correlation(data[,sapply(data, is.numeric)], histogram = TRUE)


#MODELING

#Multiple Linear Regression
linear_model <- lm(sales ~ . -sales -Time, data = data)
summary(linear_model)


###multicollinearity
imcdiag(linear_model, method = "VIF")

# we are more interested in the overall impact 
# of each type of marketing spend and how to 
# improve resource allocation to maximize efficiency 
# than the model’s predictive capacity. 
# So we decide not to remove the variables 
# and come to a conclusion that the radio advertising
# increases sales the most, while magazines have a 
# negative coefficient and don’t contribute to its augmentation.


###heteroskedasticity
bptest(linear_model)  #Breusch-Pagan test

# p-value bigger than significance level
# no heteroskedasticity

#Using Robyn 
library("reticulate")
use_virtualenv("C:/Users/chels/Desktop/R language/r-reticulate", required = TRUE)
Sys.setenv(RETICULATE_PYTHON = "C:/Users/chels/Desktop/R language/r-reticulate/Scripts/python.exe")
py_config()

data("dt_prophet_holidays")   
head(dt_prophet_holidays)

# mapped our input data into robyn_inputs fields (prophet variables, dependent variable, date variable, paid media spends...)
InputCollect <- robyn_inputs(
  dt_input = data
  ,dt_holidays = dt_prophet_holidays
  ,date_var = "Time" 
  ,dep_var = "sales" 
  ,dep_var_type = "revenue" 
  ,prophet_vars = c("trend", "season", "holiday") 
  ,prophet_country = "US"
  ,paid_media_vars = c("tv_sponsorships", "tv_cricket", "tv_RON", "radio", "NPP", "Magazines", "OOH", "Social",  "Display_Rest")
  ,paid_media_spends = c("tv_sponsorships", "tv_cricket", "tv_RON", "radio", "NPP", "Magazines", "OOH", "Social", "Display_Rest" )
  ,cores=4
  ,window_start = "2001-01-01"
  ,window_end = "2017-08-01"
  ,adstock = "geometric"
)

# checked the hyperparameters' names and limits and set the range for each
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
hyper_limits()
hyperparameters<-list(
  tv_sponsorships_alphas = c(0.5, 3),
  tv_sponsorships_gammas = c(0.3, 1),
  tv_sponsorships_thetas = c(0.1, 0.4),
  
  tv_cricket_alphas = c(0.5, 3),
  tv_cricket_gammas = c(0.3, 1),
  tv_cricket_thetas = c(0.1, 0.4),
  
  tv_RON_alphas = c(0.5, 3),
  tv_RON_gammas = c(0.3, 1),
  tv_RON_thetas = c(0.1, 0.4),
  
  radio_alphas = c(0.5, 3),
  radio_gammas = c(0.3, 1),
  radio_thetas = c(0.1, 0.4),
  
  NPP_alphas = c(0.5, 3),
  NPP_gammas = c(0.3, 1),
  NPP_thetas = c(0.1, 0.4),
  
  Magazines_alphas = c(0.5, 3),
  Magazines_gammas = c(0.3, 1),
  Magazines_thetas = c(0.1, 0.4),
  
  OOH_alphas = c(0.5, 3),
  OOH_gammas = c(0.3, 1),
  OOH_thetas = c(0.1, 0.4),
  
  Social_alphas = c(0.5, 3),
  Social_gammas = c(0.3, 1),
  Social_thetas = c(0.1, 0.4),
  
  Display_Rest_alphas = c(0.5, 3),
  Display_Rest_gammas = c(0.3, 1),
  Display_Rest_thetas = c(0.1, 0.4),
  
  train_size = c(0.5, 0.8))
  

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)

# ran the model
OutputModels <- robyn_run(
  InputCollect = InputCollect 
  ,ts_validation = TRUE
  ,outputs = FALSE 
  ,iterations=3000      
  ,trials=5
)

# This step generates samples of hyperparameters, conducts media 
# transformation within each loop, fits the Ridge regression, 
# calibrates the model optionally, 
# decomposes responses and collects the result.
 
# The performance of a model is decided based on two metrics - 
# NRMSE which measures model error, and DECOMP RSSD, which takes into 
# account how realistic a given model is.

# The comparison is made on the Pareto front, a concept used in 
# multi-objective optimization. It allows the designer to 
# restrict attention to the set of efficient choices 
# and to make tradeoffs within this set rather 
# than considering the full range of every parameter.

print(OutputModels)

# created csv files and multiple plots
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels
  , pareto_fronts = "auto"
  , csv_out = "pareto" 
  , clusters = TRUE
  , export = TRUE
  , plot_pareto = TRUE 
  , plot_folder = "/." 
)

print(OutputCollect)
# got 4 clusters

#selected the best model and exported it
select_model <- "1_162_11"
myOnePager <- robyn_onepagers(InputCollect, OutputCollect, select_model, export = FALSE)

ExportedModel <- robyn_write(InputCollect, OutputCollect, "1_162_11", export = TRUE)
print(ExportedModel)

json_file = "./Robyn_202305032102_init/RobynModel-1_162_11.json"

# What's the optimal media spend allocation 
# given the same average spend level in history?
# We used the saved model and the average budget spent over the years
AllocatorCollect <- robyn_allocator(
  json_file = json_file, 
  dt_input = data,
  dt_holidays = dt_prophet_holidays,
  date_min = "2001-1-1",
  date_max= "2017-8-1",
  scenario = "max_historical_response",
  export = TRUE
)

print(AllocatorCollect)
plot(AllocatorCollect)

