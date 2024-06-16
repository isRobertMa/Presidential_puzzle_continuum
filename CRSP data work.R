library(ggplot2)
library(ggpubr)
library(dplyr)
library(lme4)
library(tseries)
library(forecast)

# Value-weighted portfolio 
value_data <- read.csv("Value_weighted.csv")
# Equal-weighted portfolio 
equal_data <- read.csv("Equal_weighted.csv")
# Three month T-bill Rate 
TB3MS <- read.csv("TB3MS.csv")
# president data republican = 1, democrat = 0
president_data <- read.csv("Presidential_Party_by_Month.csv")
party_status <- president_data$Party[2:1080]
T_bill <- TB3MS$TB3MS


one <- ggplot(TB3MS, aes(x=seq(1,1080), y=TB3MS))+
  geom_line() +
  
  labs(title="Plot of average excess equal-weighted monthly return",
       x ="# of months since 1934", y = "Excess Equal-Weighted monthly return" 
  )
two <- ggplot(value_data, aes(x=seq(1,1080), y=COL1))+
  geom_line() + labs(title="Plot of value-weighted portfolio",
                     x ="# of months since 1934", y = "Excess  monthly return")

three <- ggplot(equal_data, aes(x=seq(1,1080), y=COL1))+
  geom_line() + labs(title="Plot of equal-weighted portfolio",
                     x ="# of months since 1934", y = "Excess monthly return")

example <- ggarrange(two,
                    three,
                    ncol = 2,
                    nrow = 1,
                    # First row with line plot
                    labels = c("A","B") # Label of the line plot
)

example


x <- seq(1, 1079);
T_df <- data.frame(T_bill);
n <- nrow(T_df)
ccret <- log(T_df[2:n, 1]) - log(T_df[1:(n-1), 1])
ccret_df <- data.frame(ccret)

T_mreturn <- ccret_df$ccret
# Excluding first month, because it is unobtainable 
# from the T-bill data. 
v_return <- value_data$COL1[2:1080]
e_return <- equal_data$COL1[2:1080]

V_T <-  v_return-T_mreturn
E_T <- e_return - T_mreturn

df_combined <- data.frame(T_mreturn, v_return, e_return, V_T, E_T, party_status)
democrat_data <- df_combined %>% filter(party_status == 0)
republican_data <- df_combined %>% filter(party_status == 1)


ap <- ggplot(df_combined, aes(x = seq(1,1079), y = V_T)) + 
  geom_line(aes(color = party_status, group = 1))+
  labs(title="Plot of value-weighted portfolio",
       x ="# of months since 1934", y = "Excess monthly return" 
  )


bp <- ggplot(df_combined, aes(x = seq(1,1079), y = E_T)) + 
  geom_line(aes(color = party_status, group = 1)) + 
  labs(title="Plot of equal-weighted portfolio",
       x ="# of months since 1934", y = "Excess monthly return" 
       )


cp <- ggplot() + 
  geom_density(df_combined, mapping = aes(x = V_T), color="darkblue", fill="lightblue", alpha=0.1) +
  geom_density(df_combined, mapping = aes(x = E_T), color="darkred", fill="red", alpha=0.1) +
  ggtitle("Distributions of Monthly Log Returns")

figure <- ggarrange(ap,
                    bp,
                    cp,
                    ncol = 2,
                    nrow = 2
)

figure

t.test(df_combined$E_T, mu = 0)
t.test(df_combined$V_T, mu = 0)

lm_value_weighted <- lm(E_T ~ party_status, data = df_combined)
summary(lm_value_weighted)


lm_equal_weighted <- lm(V_T ~ party_status, data = df_combined)
summary(lm_equal_weighted)


e_anova = aov(E_T ~ party_status, data = df_combined)
summary(e_anova)

v_anova = aov(V_T ~ party_status, data = df_combined)
summary(v_anova)

adf.test(df_combined$E_T)
acf(df_combined$E_T)
pacf(df_combined$E_T)

arima_m = auto.arima(df_combined$E_T, d = 0, max.p = 3, max.q = 3, seasonal = T)
summary(arima_m)
checkresiduals(arima_m)
tsdiag(arima_m)

arima_d = auto.arima(democrat_data$E_T, d = 0, max.p = 3, max.q = 3, seasonal = T)
summary(arima_d)
checkresiduals(arima_d)
tsdiag(arima_d)

arima_r = auto.arima(republican_data$E_T, d = 0, max.p = 3, max.q = 3, seasonal = T)
summary(arima_r)
checkresiduals(arima_r)
tsdiag(arima_r)

head(df_combined, n = 10)
