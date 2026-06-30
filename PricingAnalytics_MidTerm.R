packages <- c('ggplot2','maps','mapdata','gridExtra', 'tidyr', 'dplyr',
              'Rmisc','viridis')
for (p in packages) {
  if ((p %in% rownames(installed.packages())) == FALSE) {
    install.packages(p, dependencies = TRUE)
  } 
  library(p, character.only = TRUE)
}
library(tidyverse)
library(gganimate)
library(ggplot2)
library(broom)
library(lfe)
library(tidyr)

plotnationaldata = function(dataframe, var1, var2, title, scale = FALSE) {
  
  vars = c("v1","v2")
  
  dataframe$v1 = unlist(dataframe[,var1])
  dataframe$v2 = unlist(dataframe[,var2])
  p1 = dataframe %>%
    group_by(Date) %>%
    summarize(
      v1 = mean(v1)/ifelse(scale,max(dataframe$v1),1),
      v2 = mean(v2)/ifelse(scale,max(dataframe$v2),1)
    ) %>% 
    gather("variable", "value", vars) %>%
    mutate(
      data = ifelse(variable == "v1", var1, var2)
    ) %>%
    ggplot(aes(x = Date, y = value, color = data)) +
    geom_line(stat = "identity") +
    geom_point() + theme_minimal() +
    labs(title = title,subtitle = "over time", y = "")
  
  p2 = dataframe %>%
    group_by(Month) %>%
    summarize(
      v1 = mean(v1)/ifelse(scale,max(dataframe$v1),1),
      v2 = mean(v2)/ifelse(scale,max(dataframe$v2),1)
    ) %>%
    gather("variable", "value", vars) %>%
    mutate(
      data = ifelse(variable == "v1", var1, var2)
    ) %>%
    ggplot(aes(x = Month, y = value, fill = data)) +
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
    labs(subtitle = "variation by month", y = "")
  
  grid.arrange(p1,p2)
}



plotregionaldata = function(dataframe, var1, var2, title, scale = FALSE) {
  
  vars = c("v1","v2")
  
  dataframe$v1 = unlist(dataframe[,var1])
  dataframe$v2 = unlist(dataframe[,var2])
  p1 = dataframe %>%
    group_by(Date, Region) %>%
    summarize(
      v1 = mean(v1)/ifelse(scale,max(dataframe$v1),1),
      v2 = mean(v2)/ifelse(scale,max(dataframe$v2),1)
    ) %>% 
    gather("variable", "value", vars) %>%
    mutate(
      data = ifelse(variable == "v1", var1, var2)
    ) %>%
    ggplot(aes(x = Date, y = value, color = data)) +
    geom_line(stat = "identity") +
    geom_point() + theme_minimal() +
    facet_wrap(~Region, scales = "free")+
    labs(title = title,subtitle = "over time", y = "")
  
  p2 = dataframe %>%
    group_by(Month,Region) %>%
    summarize(
      v1 = mean(v1)/ifelse(scale,mean(dataframe$v1),1),
      v2 = mean(v2)/ifelse(scale,mean(dataframe$v2),1)
    ) %>%
    gather("variable", "value", vars) %>%
    mutate(
      data = ifelse(variable == "v1", var1, var2)
    ) %>%
    ggplot(aes(x = Month, y = value, fill = data)) +
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
    facet_wrap(~Region, scales = "free")+
    labs(subtitle = "variation by month", y = "")
  
  grid.arrange(p1,p2)
}

makeimpliedprofit = function(elasticity, modeldesc){
  price = c(0:200)*0.01+1
  impliedprofit = (price-1)*(price)^elasticity
  
  p = ggplot(data = NULL, aes(x = price, y= impliedprofit))+
    geom_line(lwd = 2)+
    theme_economist()+
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()
    )+
    labs(title = "implied profit from regression model", 
         subtitle = modeldesc,
         caption = "implied profit =(price-1)*(price)^b + constant \n red line is current price; green line is implied optimal")+
    geom_vline(aes(xintercept = mean(nationaldata$Price.Own)), col = "darkred", 
               lwd =2, lty = 5)+
    geom_vline(aes(xintercept = price[impliedprofit == max(impliedprofit)]),
               col = "darkgreen", lwd =2, lty = 5)
  
  p
}



plotmodelsummary = function(models){
  summodel = function(model){
    tidy(model,conf.int = T)[,c("estimate",
                                "conf.low", "conf.high")]
  }
  fixedeffects = function(model){
    paste(ls(model$fe), collapse= " & ")
  }
  estimates = matrix(unlist(lapply(models, summodel)),ncol = 3, byrow = T)
  
  chartdata = data.frame(
    order = c(1:length(models)),
    name = unlist(lapply(models,fixedeffects)),
    estimate = estimates[,1],
    ci.low = estimates[,2],
    ci.high = estimates[,3]
  )  
  
  ggplot(chartdata, aes(y = estimate, x = reorder(name,-order)))+
    geom_bar(stat = "identity", fill = "grey80")+
    geom_errorbar(aes(ymin = ci.low, ymax = ci.high),
                  width = 0.2)+
    coord_flip()+
    theme_minimal()+
    labs(title = "summary of the price estimate across models")+
    theme(axis.title.y = element_blank())
}

data <- read.csv("allregion.csv")

model = felm(logSales_NB ~logPrice_NB + logPrice_SB | Region , data = data )

T <- tidy(model,conf.int = T)

conf <- confint(model)

conf[1,2]

modelsummary = function(model){
  fixedeffect = paste0(ls((model$fe)),
                       " n f.e. =", unlist(lapply(lapply(model$fe, levels), length)))
  rsq = round(summary(model)$r.squared,2)               
  est = tidy(model, conf.int = T)
  list(
    est = est,
    fixedeffect = fixedeffect,
    rsq = rsq
  )
}


#optimal price formula
e = model$coefficients[1]
optprice = 4*(e/(1+e))
print(paste0("Optimal price = $",prettyNum(optprice,digits = 3)))
modelsummary(model)

model2 = felm(logSales_SB ~logPrice_SB + logPrice_NB | Region , data = data )

e2 = model2$coefficients[1]
optprice2 = 2.5*(e2/(1+e2))
print(paste0("Optimal price = $",prettyNum(optprice2,digits = 3)))
modelsummary(model2)

summary_region_NB <- summarySE(data, 
                               measurevar="Sales_NB", groupvars= "Region")

summary_region_NB %>%
  ggplot(aes(x= Region, y = Sales_NB)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = Sales_NB - ci, ymax = Sales_NB+ci),
                position = position_dodge(0.9), width = 0.1)+
  theme_minimal()+
  labs(title = "mean share for flat or non-flat stores")+
  scale_fill_viridis(discrete=TRUE)

summary_region_SB <- summarySE(data, 
                               measurevar="Sales_SB", groupvars= "Region")

summary_region_SB %>%
  ggplot(aes(x= Region, y = Sales_SB)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = Sales_SB - ci, ymax = Sales_SB+ci),
                position = position_dodge(0.9), width = 0.1)+
  theme_minimal()+
  labs(title = "mean share for flat or non-flat stores")+
  scale_fill_viridis(discrete=TRUE)

summary_week_NB <- summarySE(data, 
                             measurevar="Sales_NB", groupvars= "Week")

summary_week_NB %>%
  ggplot(aes(x= Week, y = Sales_NB)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = Sales_NB - ci, ymax = Sales_NB+ci),
                position = position_dodge(0.9), width = 0.1)+
  theme_minimal()+
  labs(title = "mean share for flat or non-flat stores")+
  scale_fill_viridis(discrete=TRUE)

summary_week_SB <- summarySE(data, 
                             measurevar="Sales_SB", groupvars= "Week")

summary_week_SB %>%
  ggplot(aes(x= Week, y = Sales_SB)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = Sales_SB - ci, ymax = Sales_SB+ci),
                position = position_dodge(0.9), width = 0.1)+
  theme_minimal()+
  labs(title = "mean share for flat or non-flat stores")+
  scale_fill_viridis(discrete=TRUE)