library(skimr)
library("PerformanceAnalytics")
library(caret)
library(dplyr)
library(gdata)
library(tidyverse)
library(Amelia)
library(mice)
library(kableExtra)
library(data.table)
library(e1071)
library(corrplot)
library(MASS)
library(caret)
library(tidyr)
library(dplyr)
library(data.table)
library(maps)
library("Hmisc")




load_chi2018 <- function(var_select = c("water_score", "sanitation_score", "child_mortality", "Year", "ISO3", "CountryName")) {
  
  
  library(tidyr)
  library(dplyr)
  library(data.table)
  
  
  set.seed(1234567890)
  
  df = read.csv("https://raw.githubusercontent.com/davidblumenstiel/CUNY-MSDS-DATA-621/main/Final_Project/chi-2018.csv")
  
  #Took some code from: https://stackoverflow.com/questions/50010196/replacing-na-values-from-another-dataframe-by-id
  #and https://stackoverflow.com/questions/25908772/r-column-mean-by-factor
  
  x1 = df %>%
    pivot_longer(
      cols = starts_with("wat_"), 
      names_to = "Year",
      names_prefix = "wat_",
      values_to = "water_score"
    ) 
  x1 = x1 %>% 
    left_join(setDT(x1)[, mean(water_score, na.rm = TRUE), by=CountryName], by = "CountryName") %>%
    mutate(water_score = ifelse(is.na(water_score), V1, water_score)) %>%
    dplyr::select(Year, water_score, CountryName)   

  x2 = df %>%
    pivot_longer(
      cols = starts_with("san_"), 
      names_to = "Year",
      names_prefix = "san_",
      values_to = "sanitation_score"
    ) 
  x2 = x2 %>%
    left_join(setDT(x2)[, mean(sanitation_score, na.rm = TRUE), by=CountryName], by = "CountryName") %>%
    mutate(sanitation_score = ifelse(is.na(sanitation_score), V1, sanitation_score)) %>% #Replace NA with mean of values if available
    dplyr::select("Year", "sanitation_score", "CountryName")    
  
  
  
  x3 = df %>%
    pivot_longer(
      cols = starts_with("chmort_"), 
      names_to = "Year",
      names_prefix = "chmort_",
      values_to = "child_mortality"
    ) 
  x3 = x3 %>%
    left_join(setDT(x3)[, mean(child_mortality, na.rm = TRUE), by=CountryName], by = "CountryName") %>%
    mutate(child_mortality = ifelse(is.na(child_mortality), V1, child_mortality)) %>% #Replace NA with mean of values if available
    dplyr::select("Year", "child_mortality", "CountryName")   
  
  
  
  x4 = df %>%
    pivot_longer(
      cols = starts_with("mortality_"), 
      names_to = "Year",
      names_prefix = "mortality_",
      values_to = "mortality_score"
    ) 
  x4 = x4 %>%
    left_join(setDT(x4)[, mean(mortality_score, na.rm = TRUE), by=CountryName], by = "CountryName") %>%
    mutate(mortality_score = ifelse(is.na(mortality_score), V1, mortality_score)) %>% #Replace NA with mean of values if available
    dplyr::select("Year", "mortality_score", "CountryName")   
  
  x5 = df %>%
    pivot_longer(
      cols = starts_with("CHI_v2018_"), 
      names_to = "Year",
      names_prefix = "CHI_v2018_",
      values_to = "CHI_v2018"
    ) 
  x5 = x5 %>%
    left_join(setDT(x5)[, mean(CHI_v2018, na.rm = TRUE), by=CountryName], by = "CountryName") %>%
    mutate(CHI_v2018 = ifelse(is.na(CHI_v2018), V1, CHI_v2018)) %>% #Replace NA with mean of values if available
    dplyr::select("Year", "CHI_v2018", "CountryName")   
  
  out = x1 %>% merge(x2, by = c("CountryName", "Year")) %>%
    merge(x3, by = c("CountryName", "Year")) %>%
    merge(x4, by = c("CountryName", "Year")) %>%
    merge(x5, by = c("CountryName", "Year"))
  
  out = as.data.frame(out)
  
  
  #Adds back ISO3 abbreviations 
  out <- out %>% merge(x = out, y = df[,1:2], by.x = "CountryName", by.y = "CountryName")
  colnames(out)[8] <- "ISO3"
  
  #NA dropping
  out <- data.frame(out[,var_select]) %>% drop_na()
  
  
  return(out)
  
}



df_input <- load_chi2018(var_select=c("water_score", "sanitation_score", "child_mortality", "Year", "ISO3", "CountryName", "mortality_score", "CHI_v2018"))

## Summary Statistics 

skim(df_input)

df_input %>% head() %>% kable() %>% kable_material()

colSums(is.na(df_input))
missmap(df_input, main="Missing Values")
df_input$Year <- paste("20",df_input$Year,sep="",collapse=NULL)
df_input$Year <- as.numeric(df_input$Year)


indx<- which(sapply(df_input, is.numeric))
setcolorder(df_input, indx)

nonbinary <- c(1:6)
X <- df_input[1:6]

par(mfrow = c(3,3))
for (i in nonbinary) {
  hist(X[,i], xlab = names(X[i]), main = names(X[i]))
}

par(mfrow = c(3,3))
for (i in nonbinary) {
  d <- density(X[,i])
  plot(d, main = names(X[i]))
  polygon(d, col="red")
}

res2 <- rcorr(as.matrix(X))
res2$r
res2$P
colSums(is.na(X))
chart.Correlation(X, histogram=TRUE, pch=19)


sapply(X, skewness, function(x) skewness(x))
par(mfrow = c(1,1))

X %>% 
  cor(., use = "complete.obs") %>%
  corrplot(., method = "color", type = "upper", tl.col = "black", tl.cex=.8, diag = FALSE)

yearly_avg <- X %>%
  dplyr::select(Year,water_score,sanitation_score,child_mortality) %>%
  group_by(Year) %>%
  summarise(water_avg=mean(water_score),sanitation_avg=mean(sanitation_score),child_mortality_avg = mean(child_mortality)) %>%
  dplyr::select(Year,water_avg,sanitation_avg,child_mortality_avg) 

ggp1<-ggplot(yearly_avg, aes(x=Year, y=water_avg)) +  geom_point(aes(size=child_mortality_avg),shape=18,color="red")  +   geom_smooth(method=lm,  linetype="dashed",  color="darkgreen", fill="gray") + xlab("Year") + ylab("Water Resource Scale")   + ggtitle("Yearly trend of Water Resource impact over Child Mortality")
ggp2<-ggplot(yearly_avg, aes(x=Year, y=sanitation_avg)) +  geom_point(aes(size=child_mortality_avg),shape=18,color="blue")  +   geom_smooth(method=lm,  linetype="dashed",  color="darkgreen", fill="gray")+ xlab("Year") + ylab("Sanitation Scale")   + ggtitle("Yearly trend of Sanitation impact over Child Mortality")

library(gridExtra)
grid.arrange(ggp1,ggp2,ncol=2)


