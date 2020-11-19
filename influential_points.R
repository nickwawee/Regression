## Title: infl_analysis
## Author: Nick Wawee
## Description:
#The following function will obtain leverage, influential, and outlier points as determine by six different methods. The methods include hat values, internal and external standardized residuals, dffts, and covariance ratios. 
#The input is a linear model object (from lm() function).
#The output is a dataframe that identifies all leverage, influential, and outlier points which can be used in ggplot. 
#The plot displays which points are determined to be leverage, outlier, or influential which is useful for identifying points that violate assumptions in regression analysis.
#Example usage is displayed after function end.

infl_analysis = function(l_m){
  #Model Values
  k = length(l_m$coefficients)-1
  n = length(l_m$fitted.values)
  row_num = 1:n
  
  #Leverage points
  hatdf = data.frame(Values = hatvalues(l_m), Row_Num = row_num, Type = rep('Hat Values', length(row_num)), Point_Type = rep('Leverage', length(row_num)), Bound1 = 2*(k+1)/n, Bound2 = 2*(k+1)/n)
  hatdf$Label = NA
  inds = which(hatvalues(l_m)>2*(k+1)/n)
  if(length(inds)!= 0){hatdf$Label[inds] = row_num[inds]}
  
  #Outliers
  instdf = data.frame(Values = rstandard(l_m), Row_Num = row_num, Type = rep('Internally Standardized Residuals', length(row_num)), Point_Type = rep('Outlier', length(row_num)), Bound1 = 3, Bound2 = -3)
  instdf$Label = NA
  inds = which(rstandard(l_m) > 3 | rstandard(l_m) < -3)
  if(length(inds)!=0){instdf$Label[inds] = row_num[inds]}
  
  extdf = data.frame(Values = rstudent(l_m), Row_Num = row_num, Type = rep('Externally Standardized Residuals', length(row_num)), Point_Type = rep('Outlier', length(row_num)), Bound1 = 3, Bound2 = -3)
  extdf$Label = NA
  inds = which(rstudent(l_m) > 3 | rstudent(l_m) < -3)
  if(length(inds)!=0){extdf$Label[inds] = row_num[inds]}
  
  #Influential
  dfitsdf = data.frame(Values = dffits(l_m), Row_Num = row_num, Type = rep('DEFFITS', length(row_num)),Point_Type = rep('Influential', length(row_num)), Bound1 = 2*sqrt((k+2)/(n-k-2)), Bound2 = -2*sqrt((k+2)/(n-k-2)))
  dfitsdf$Label = NA
  inds = which(dffits(l_m) > 2*sqrt((k+2)/(n-k-2)) | dffits(l_m) < -2*sqrt((k+2)/(n-k-2)))
  if(length(inds)!=0){dfitsdf$Label[inds] = row_num[inds]}
  
  cddf = data.frame(Values = cooks.distance(l_m), Row_Num = row_num, Type = rep("Cook's Distance", length(row_num)),Point_Type = rep('Influential', length(row_num)), Bound1 = 1, Bound2 = 1)
  cddf$Label = NA
  inds = cooks.distance(l_m) > 1
  if(length(inds)!=0){cddf$Label[inds] = row_num[inds]}
  
  cvdf = data.frame(Values = covratio(l_m), Row_Num = row_num, Type = rep("Covariance Ratio", length(row_num)),Point_Type = rep('Influential', length(row_num)), Bound1 = 1 + 3*(k+1)/n, Bound2 = 1 - 3*(k+1)/n)
  cvdf$Label = NA
  inds = covratio(l_m) > 1 + 3*(k+1)/n | covratio(l_m) < 1 - 3*(k+1)/n
  if(length(inds)!=0){cvdf$Label[inds] = row_num[inds]}
  
  ret_df = rbind(hatdf, instdf, extdf, dfitsdf, cddf, cvdf)
  return(ret_df)
}

ret_df = infl_analysis(mlr)
library(ggplot2)
#library(ggrepel)#uncomment if you would like to have repelled labels on the graph
ggplot(data= ret_df, aes(x= Row_Num, y = Values))+
  geom_point()+
  facet_wrap(~Type, scales = "free_y")+geom_line(aes(y=Bound1))+geom_line(aes(y=Bound2))+
  #geom_label_repel(aes(label=Label))+#uncomment if you wish to label the points on the graph
  labs(title = 'Influential Point Analysis', x = 'Observation Number')
