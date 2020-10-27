## Title: infl_analysis
## Author: Nick Wawee
## Description:
#The following function will obtain leverage, influential, and outlier points as determine by six different methods. The methods include hat values, internal and external standardized residuals, dffts, and covariance ratios. 
#The inputs are a linear model object (from lm() funciton) and a dataframe that the corresponding linear model was fit.
#The output is a dataframe that identifies all leverage, influential, and outlier points which can be used in ggplot. 
#The plot displays which points are determined to be leverage, outlier, or influential which is useful for identifying points that violate assumptions in regression analysis.
#Example usage is displayed after function end.

infl_analysis = function(l_m, df){
  k = length(l_m$coefficients)-1
  n = nrow(df)
  row_num = 1:n
  response_v = df[colnames(df)==l_m$terms[[2]]]
  #Leverage points
  hatdf = data.frame(Values = hatvalues(l_m), Row_Num = row_num, Type = rep('Hat Values', length(row_num)), Point_Type = rep('Leverage', length(row_num)), Bound1 = 2*(k+1)/n, Bound2 = 2*(k+1)/n)
  hatdf$Label = NA
  inds = which(hatvalues(l_m)>2*(k+1)/n)
  if(length(inds)!= 0){hatdf$Label[inds] = response_v$y[inds]}
  #Outliers
  instdf = data.frame(Values = rstandard(l_m), Row_Num = row_num, Type = rep('Internally Standardized Residuals', length(row_num)), Point_Type = rep('Outlier', length(row_num)), Bound1 = 3, Bound2 = -3)
  instdf$Label = NA
  inds = which(rstandard(l_m) > 3 | rstandard(l_m) < -3)
  if(length(inds)!=0){instdf$Label[inds] = response_v$y[inds]}
  
  extdf = data.frame(Values = rstudent(l_m), Row_Num = row_num, Type = rep('Externally Standardized Residuals', length(row_num)), Point_Type = rep('Outlier', length(row_num)), Bound1 = 3, Bound2 = -3)
  extdf$Label = NA
  inds = which(rstandard(l_m) > 3 | rstandard(l_m) < -3)
  if(length(inds)!=0){extdf$Label[inds] = response_v$y[inds]}
  
  #Influential
  dfitsdf = data.frame(Values = dffits(l_m), Row_Num = row_num, Type = rep('DEFFITS', length(row_num)),Point_Type = rep('Influential', length(row_num)), Bound1 = 2*sqrt((k+2)/(n-k-2)), Bound2 = -2*sqrt((k+2)/(n-k-2)))
  dfitsdf$Label = NA
  inds = which(dffits(l_m) > 2*sqrt((k+2)/(n-k-2)) | dffits(l_m) < -2*sqrt((k+2)/(n-k-2)))
  if(length(inds)!=0){dfitsdf$Label[inds] = response_v$y[inds]}
  
  cddf = data.frame(Values = cooks.distance(l_m), Row_Num = row_num, Type = rep("Cook's Distance", length(row_num)),Point_Type = rep('Influential', length(row_num)), Bound1 = 1, Bound2 = 1)
  cddf$Label = NA
  inds = cooks.distance(l_m) > 1
  if(length(inds)!=0){cddf$Label[inds] = response_v$y[inds]}
  
  cvdf = data.frame(Values = covratio(l_m), Row_Num = row_num, Type = rep("Covariance Ratio", length(row_num)),Point_Type = rep('Influential', length(row_num)), Bound1 = 1 + 3*(k+1)/n, Bound2 = 1 - 3*(k+1)/n)
  cvdf$Label = NA
  inds = covratio(l_m) > 1 + 3*(k+1)/n | covratio(l_m) < 1 - 3*(k+1)/n
  if(length(inds)!=0){cvdf$Label[inds] = response_v$y[inds]}
  
  ret_df = rbind(hatdf, instdf, extdf, dfitsdf, cddf, cvdf)
  return(ret_df)
}

ret_df = infl_analysis(mlr, df)
library(ggplot2)
library(ggrepel)
ggplot(data= ret_df, aes(x= Row_Num, y = Values))+
  geom_point()+
  facet_wrap(~Type, scales = "free_y")+plot_opts+geom_line(aes(y=Bound1))+geom_line(aes(y=Bound2))+
  geom_label_repel(aes(label=Label))+
  labs(title = 'Influential Point Analysis', x = 'Observation Number')
