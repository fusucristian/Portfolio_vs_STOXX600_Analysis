install.packages("tidyquant")
install.packages("tidyverse")
library(tidyquant)
library(tidyverse)
tickers <- c("VWCG.DE","GRID","DFNS.SW","XAIX","SGBS.MI")   #data imported from Yahoo Finance
benchmark <- "^STOXX"
prices <- tq_get(c(tickers, benchmark), from = "2023-01-01", to = "2026-01-17", get = "stock.prices")
returns <- prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "weekly", col_rename = "ret")

ra <- returns %>% filter(symbol %in% tickers)         
rb <- returns %>% filter(symbol == benchmark)         

library(estimatr)

ra1 = ra[ra$symbol=="VWCG.DE",]      #the data gets imported in an only data frame
ra2 = ra[ra$symbol=="GRID",]         #one must separate each asset's weekly return in different data frames
ra3 = ra[ra$symbol=="DFNS.SW",]
ra4 = ra[ra$symbol=="XAIX",]
ra5 = ra[ra$symbol=="SGBS.MI",]

dfret1 <- inner_join(ra1, ra2, by="date")             #the data frames are joined by matching dates
dfret2 <- inner_join(dfret1, ra3, by="date")
dfret3 <- inner_join(dfret2, ra4, by="date")
df <- inner_join(dfret3, ra5, by="date")
df_clean <- df[, -c(1,2,4,6,8,10)]
colnames(df_clean) <- c("VWCG.DE","GRID","DFNS.SW","XAIX","SGBS.MI")   #this is the main data frame used for the assets
dfretbench <- inner_join(df, rb, by="date")                           
stoxxret <- dfretbench[, -c(1:12)]
stoxxr <- stoxxret$ret.y.y.y
stoxxrdf <- as.data.frame(stoxxr)
colnames(stoxxrdf) <- "STOXX"    #this is the main data frame used for the benchmark

olsa1 = lm_robust(VWCG.DE ~ stoxxr, df_clean)  #beta estimation through robust linear regression
summary(olsa1)

olsa2 <- lm_robust(GRID ~ stoxxr, data = df_clean)
summary(olsa2)

olsa3 <- lm_robust(DFNS.SW ~ stoxxr, data = df_clean)
summary(olsa3)

olsa4 <- lm_robust(XAIX ~ stoxxr, data=df_clean)
summary(olsa4)

olsa5 <- lm_robust(SGBS.MI ~ stoxxr, df_clean)
summary(olsa5)

vweSTOXX <- sd(stoxxrdf$STOXX, na.rm=TRUE)    #annualized volatilities
sigmaSTOXX <- vweSTOXX*sqrt(52)

vwea1 <- sd(df_clean$VWCG.DE, na.rm=TRUE)
sigmaa1 <- vwea1*sqrt(52)

vwea2 <- sd(df_clean$GRID, na.rm=TRUE)
sigmaa2 <- vwea2*sqrt(52)

vwea3 <- sd(df_clean$DFNS.SW, na.rm=TRUE)
sigmaa3 <- vwea3*sqrt(52)

vwea4 <- sd(df_clean$XAIX, na.rm=TRUE)
sigmaa4 <- vwea4*sqrt(52)

vwea5 <- sd(df_clean$SGBS.MI, na.rm=TRUE)
sigmaa5 <- vwea5*sqrt(52)

cor_matrix <- cor(df_clean)             #portfolio variance calculation
cov_weekly <- cov(df_clean)
cov_annual <- cov_weekly*52
weights <- c(0.55,0.15,0.15,0.10,0.05)
portvariance <- t(weights) %*% cov_annual %*% weights
sigmap <- sqrt(portvariance)

ind_vol <- sqrt(diag(cov_annual))            #diversification benefit & ratio
weighted_vol_average <- sum(weights*ind_vol)
div_benefit <- weighted_vol_average - sigmap
div_ratio <- weighted_vol_average/sigmap

meanret_weekly <- colMeans(df_clean)          #Portfolio Sharpe Ratio
annret <- (1 + meanret_weekly)^52 - 1
annretp <- sum(annret * weights)
rf <- 0.0275
sharpe_portfolio <- (annretp - rf)/as.numeric(sigmap)

meanretb <- mean(stoxxr)                  #Benchmark Sharpe Ratio
annretb <- (1 + meanretb)^52 - 1  
sharpeb <- (annretb - rf)/sigmaSTOXX

portfoliobeta <- sum(weights * c(0.9912202,0.896794,1.028175,0.946116,0.16554))
alpha <- annretp - (rf + portfoliobeta*(annretb - rf))   #Jensen's Alpha

install.packages("ggcorrplot")          #Correlation matrix heatmap plot
library("ggcorrplot")
corr_mat <- cor(df_clean)
heatmap <- ggcorrplot(corr_mat, 
                      hc.order = TRUE,          
                      type = "lower",            
                      lab = TRUE,                
                      lab_size = 4,              
                      method = "square",         
                      colors = c("#E4672E", "white", "#6D9EC1"),
                      ggtheme = ggplot2::theme_minimal())
print(heatmap)
ggsave("Heatmap.png", plot = heatmap, width = 8, height = 6, dpi = 300)

returnpweekly <- as.matrix(df_clean) %*% weights   #Cumulative returns plot
library(ggplot2)
portfolio_value <- cumprod(c(1,1+returnpweekly))*100
benchmark_value <- cumprod(c(1,1+stoxxrdf$STOXX))*100
date_vector <- df$date
date_zero <- date_vector[1] - 7
date_plot <- c(date_zero,date_vector)
df_graph <- data.frame(
  Date = date_plot,
  Portfolio = portfolio_value,
  Benchmark = benchmark_value
)
comparison_graph = ggplot(df_graph, aes(x=Date)) + 
  geom_line(aes(y= Portfolio, color="Portfolio"), linewidth=1) +
  geom_line(aes(y= Benchmark, color="STOXX 600"), linewidth =0.8) +
  scale_color_manual(values=c("Portfolio" = "dark blue", "STOXX 600" = "orange")) +
  labs(title="Cumulative Performance",
       subtitle="Base 100",
       x="Date",
       y="Value",
       color="Asset") +
  theme_minimal() +
  theme(legend.position="bottom")
print(comparison_graph)
ggsave("cumulativeperformance.png", dpi=300)           

asset_returns <- colMeans(df_clean) * 52              #Risk-Return Plot
asset_vols <- apply(df_clean, 2, sd) * sqrt(52)
bench_ret <- mean(stoxxrdf$STOXX) * 52
bench_vol <- sd(stoxxrdf$STOXX) * sqrt(52)
df_risk_ret <- data.frame(
  Asset = c(colnames(df_clean), "PORTFOLIO", "STOXX 600"),
  Return = c(asset_returns, 0.304, bench_ret), 
  Volatility = c(asset_vols, 0.141, bench_vol),
  Type = c(rep("Asset", 5), "Portfolio", "Benchmark")
)
install.packages("ggrepel")
library(ggrepel)
riskret <- ggplot(df_risk_ret, aes(x = Volatility, y = Return, color = Type)) +
  geom_point(aes(shape = Type), size = 6) +
  geom_text_repel(aes(label = Asset), fontface = "bold", size = 5, box.padding = 0.5) +
  scale_color_manual(values = c("Benchmark" = "black", "Portfolio" = "red", "Asset" = "steelblue")) +
  scale_x_continuous(labels = scales::percent, limits = c(0.10, max(df_risk_ret$Volatility) * 1.1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, max(df_risk_ret$Return)*1.1)) +
  labs(title = "Risk-Return Analysis",
       subtitle = "Single Assets, Portfolio, Benchmark",
       x = "Annualized volatility",
       y = "Annualized return",
       caption = "Syncronized weekly data, annualized on base 52") +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray30"),
    panel.grid.minor = element_blank()
  )
print(riskret)
ggsave("riskret.png", 
       plot = riskret, 
       width = 12,     
       height = 7,     
       dpi = 300,       
       bg = "white")   