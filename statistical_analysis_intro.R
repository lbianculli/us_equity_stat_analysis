library(readr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyquant)
library(nortest)
library(fGarch)

# load data
quantile_df <- read_csv("C:/Users/lbianculli/dev/us_equities/stat_analysis_input_q.csv")
score_df <- read_csv("C:/Users/lbianculli/dev/us_equities/stat_analysis_input.csv")
vix_df <- read_csv("C:/Users/lbianculli/dev/us_equities/VIX_hist.csv")
spy_data <- read_csv("C:/Users/lbianculli/dev/us_equities/spy_rets.csv")

# rename cols beginning with int
quantile_df <- quantile_df %>%
  select(-X1) %>%
  rename(mo_fwd_rets="1mo_fwd_rets", mo_fwd_log_rets="1mo_fwd_log_rets")

score_df <- score_df %>%
  select(-X1) %>%
  rename(mo_fwd_rets="1mo_fwd_rets", mo_fwd_log_rets="1mo_fwd_log_rets")

############################
###### Distribution Analysis

# check normality
print(ad.test(quantile_df$mo_fwd_log_rets))$p.value

#qq plots
qqnorm(quantile_df$mo_fwd_log_rets, pch = 1, frame = FALSE)
qqline(quantile_df$mo_fwd_log_rets, col = "red", lwd = 2)

# Comments: After performing a Anderson-Darling test of normality, we observe 
# a p-value < 0.05, which is indicative of non-normal data. 
# We also see that the tails of the QQ plot are convex-concave, indicative of 
# fat tails. Both of these observations are expected.

### Fitting distributions to our data (Student's T)

# return the negative log-likelihood for the student's t (std) model
# this will be our optimization function
log_likelihood_std <- function(x) {
  f <- -sum(log(dstd(quantile_df$mo_fwd_log_rets, x[1], x[2], x[3])))
  f}

# initial values of mean, std, and 4 dof
start <- c(mean(Y), sd(Y), 20)  

# fit optimal t-dist (returns df of params, values, etc)
fit_std <- optim(start, log_likelihood_std, method="L-BFGS-B",
                 lower=c(-.1, .001, 2, .1),  
                 upper=c(.1, 1, 20))  # lower and upper bounds for optimizer method

print(c("MLE parameters =", round(fit_std$par, 4)))

# get params and summary values
mle_mean <- fit_std$par[1]
mle_stdev <- fit_std$par[2]
mle_dof <- fit_std$par[3]
m_logL_std <- fit_std$value  # minimum value of the objective function
AIC_std <- 2*m_logL_std + 2*length(fit_std$par)

# Comments: as we can see, when assuming a T-distribution, our data is best
# parameterized by a T-dist with a mean of 0.014, a standard deviation of 0.075
# and 20 degrees of freedom.

# plot our fit T-dist, compare to T dist and Normal Dist
# set the plotting area into a 1x3 array
par(mfrow=c(1,3))   
# set up theoretical T-dist and normal-dist data
x_t <- seq(-4, 4, length=100)
y_t <- dt(x_t, df=5)
x_n <- seq(-4, 4, length=100)
y_n <- dnorm(x_n)

plot(x_n, y_n, lwd = 2, type="l", main="Standard Normal Distribution", 
     xlab = "", ylab = "")
plot(x_t, y_t, lwd = 2, type="l", main="Standard T Distribution", 
     xlab = "", ylab = "")
plot(density(quantile_df$mo_fwd_log_rets),main="Log Return Distribution", 
     col="steelblue", lwd = 2, xlab = "", ylab = "")

# Comments: As we can see, the T-distribution has fatter tails than the normal 
# distribution. However, the tails of a T-dist are still not nearly as fat as 
# those of the empirical data.

##############################
### Factor Analysis Intro

# Overlay VIX data/Vol regime analysis
# filter to only the dates we have in our stat data
vix_df_trunc <- vix_df %>%
  dplyr::filter((Date <= max(quantile_df$date_dt)) & Date >= min(quantile_df$date_dt))

# keep only the columns we need
vix_closes <- vix_df_trunc %>% 
  select(Date, 'Adj Close') %>%
  rename("date_dt"=Date, "close"='Adj Close')

# break out vol data into Low, med, high periods
vix_quantiles <- quantile(vix_closes$close, probs=c(.2, .8))
lower_bound <- vix_quantiles[1]
upper_bound <- vix_quantiles[2]

# add column denoting regime
vix_closes$regime <- ifelse(vix_closes$close < lower_bound, "low_vol", 
                            ifelse(vix_closes$close > upper_bound, "high_vol", "med_vol"))
score_df <- merge(score_df, vix_closes, by="date_dt")
quantile_df <- merge(quantile_df, vix_closes, by="date_dt")

# Layer in SPY return data
spy_rets <- spy_data %>%
    rename("date_dt"=Date, "spy_rets"=rets) %>%
    select(date_dt, spy_rets)

# merge again
score_vol_df <- merge(score_df, spy_rets, by="date_dt")
quantile_vol_df <- merge(quantile_df, spy_rets, by="date_dt")

# Define Function that calculates performance of top quantile of a factor
factor_performance <- function(data, factor_name) {
  
  # filter data to just what we are concerned with
  data <- select(data, factor_name, mo_fwd_log_rets, regime)
  
  # get overall sharpe of top quantile for context
  factor <- get(factor_name, data)
  top_quantile <- dplyr::filter(data, factor==5)

  # Get mean rets and standard deviation and annualize
  top_q_mean_rets <- mean(top_quantile$mo_fwd_log_rets)
  top_q_ret_stdev <- sd(top_quantile$mo_fwd_log_rets)
  top_q_ann_rets <- top_q_mean_rets * 12
  top_q_ann_stdev <- top_q_ret_stdev*sqrt(12)
  top_q_sharpe <- top_q_ann_rets / top_q_ann_stdev
  
  print(c("Overall Mean Monthly Return (%):", round(top_q_ann_rets, 3)*100), quote=FALSE)
  print(c("Overall Mean Monthly St.Dev (%):", round(top_q_ann_stdev, 3)*100), quote=FALSE)
  print(c("Overall Sharpe:", round(top_q_sharpe, 2)), quote=FALSE)
  print("", quote=FALSE)

  c(top_q_mean_rets, top_q_sharpe)
}

# set up lists and count
factor_sharpes <- c()
factor_mean_rets <- c()
i <- 1

# loop through factors to get all of the sharpes
while (i <= length(colnames(quantile_vol_df))) {
  # get column name and run the function
  factor_name <- colnames(quantile_vol_df)[i]
  sharpes <- factor_performance(data=quantile_vol_df, factor_name=factor_name)
  
  # unpack sharpes
  factor_mean_rets[[factor_name]] <- sharpes[1]
  factor_sharpes[[factor_name]] <- sharpes[2]
  i <- i+1
}

### Volatility Regime Analysis
# get the top factors in each regime according to sharpe ratio
top_sharpes <- head(factor_sharpes[order(unlist(factor_sharpes),decreasing=TRUE)], 3)
top_rets<- head(factor_mean_rets[order(unlist(factor_mean_rets),decreasing=TRUE)], 3)

# Now that we have top factors, lets make some visualizations
# instantiate vectors
top_sharpe_factor_names <- c() 

# get best low-vol factor names
for (factor in names(top_sharpes)) {
  factor_name <- str_sub(factor,1,-10)
  top_sharpe_factor_names <- c(top_sharpe_factor_names, factor_name)
}

# plot distribution of factor returns according to regime
top_roa_df <- quantile_vol_df %>%
  dplyr::filter(roa_quantile==5) %>%
  select(date_dt, roa_quantile, mo_fwd_rets, regime, spy_rets)

viz <- ggplot(top_roa_df, aes(x=mo_fwd_rets, color=regime)) +
  geom_density(lwd = 1) + 
  labs(title="Distribution of Forward Returns", 
       subtitle="Top Securities According to ROA", x="Next Month Returns", y="")

viz

# split data by regime
low_vol <- dplyr::filter(top_roa_df, regime=="low_vol")
med_vol <- dplyr::filter(top_roa_df, regime=="med_vol")
high_vol <- dplyr::filter(top_roa_df, regime=="high_vol")

# Calculate correlations between factor returns and S&P500 returns
overall_cor <- cor(top_roa_df$mo_fwd_rets, top_roa_df$spy_rets)
low_cor <- cor(low_vol$mo_fwd_rets, low_vol$spy_rets)
med_cor <- cor(med_vol$mo_fwd_rets, med_vol$spy_rets)
high_cor <- cor(high_vol$mo_fwd_rets, high_vol$spy_rets)

# Report summary stats for each regime
print(c("Overall Skew:", round(skewness(top_roa_df$mo_fwd_rets)[1], 3)), quote=FALSE)
print(c("Overall Kurtosis:", round(kurtosis(top_roa_df$mo_fwd_rets)[1], 3)), quote=FALSE)
print(c("Overall Correlation to S&P:", round(overall_cor, 3)), quote=FALSE)

print(c("Low Vol Skew:", round(skewness(low_vol$mo_fwd_rets)[1], 3)), quote=FALSE)
print(c("Low Vol Kurtosis:", round(kurtosis(low_vol$mo_fwd_rets)[1], 3)), quote=FALSE)
print(c("Low Vol Correlation to S&P:", round(low_cor, 3)), quote=FALSE)

print(c("Med Vol Skew:", round(skewness(med_vol$mo_fwd_rets)[1], 3)), quote=FALSE)
print(c("Med Vol Kurtosis:", round(kurtosis(med_vol$mo_fwd_rets)[1], 3)), quote=FALSE)
print(c("Med Vol Correlation to S&P:", round(med_cor, 3)), quote=FALSE)

print(c("High Vol Skew:", round(skewness(high_vol$mo_fwd_rets)[1], 3)), quote=FALSE)
print(c("High Vol Kurtosis:", round(kurtosis(high_vol$mo_fwd_rets)[1], 3)), quote=FALSE)
print(c("High Vol Correlation to S&P:", round(high_cor, 3)), quote=FALSE)

### Correlation Analysis
# Finally, lets define a function that calculates the correlation between factors
quantile_cor <- function(df, x1_name, x2_name) {
  x1 <- get(x1_name, df)
  x2 <- get(x2_name, df)
  fac1_data <- dplyr::filter(df, x1==5)
  fac2_data <- dplyr::filter(df, x2==5)
  
  fac1_rets <- select(fac1_data, mo_fwd_rets)
  fac2_rets <- select(fac2_data, mo_fwd_rets)
  
  cor <- cor(fac1_rets, fac2_rets)[1]
  sprintf("Correlation between %s and %s: %s",x1_name,x2_name, round(cor, 3))
}

# Check the correlation between the top two factors
quantile_cor(quantile_df, names(top_sharpes)[1], names(top_sharpes)[2])

# filter data to just the top factors
top_factor_data <- (select(quantile_vol_df, date_dt, mo_fwd_rets, spy_rets,
                           names(top_sharpes)))

# get returns associated with top quantile of ROA
top_factor_rets <- top_factor_data %>%
  dplyr::filter(roa_quantile==5) %>%
  select(date_dt, spy_rets, mo_fwd_rets) 

# get returns for the rest of the top factors
for (factor_name in names(top_sharpes)[2:length(names(top_sharpes))]) {
  print(factor_name)
  factor <- get(factor_name, top_factor_data)
  
  # filter on data for only this factor
  fwd_rets <- top_factor_data %>%
              dplyr::filter(factor==5) %>%
              select(mo_fwd_rets)
            
  # bind to dataframe
  top_factor_rets  <- cbind(top_factor_rets, fwd_rets)
}

# reset col names
colnames(top_factor_rets) <- c("date_dt", "spy", names(top_sharpes))

# group by month so we can get correlation
top_monthly_returns <- top_factor_rets %>% 
  group_by(date_dt) %>%
  summarise(across(everything(), mean))

# visualize factors correlations to each other and S&P500
pairs(top_monthly_returns[2:length(colnames(top_monthly_returns))])

# The correlation between the last two variables looks strong. Lets confirm.
cor(top_monthly_returns$net_cash_flow_from_operations_quantile, 
    top_monthly_returns$consolidated_income_quantile)

# Comment: We can see that None of the factors have a strong correlation with 
# the S&P500, which would be desireable when putting a portfolio together.
# However, some of the factors have very strong pairwise correlation.
# This indicates that some of the factors are redundant and it would be helpful 
# to remove them from statistical analyses and from ML modeling.
