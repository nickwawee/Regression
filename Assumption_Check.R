## Title: Assumption_Check
## Author: Nick Wawee
## Description:
#The following function will check four assumptions regarding linear models by creating graphs for each.
#The inputs are a linear model object (from lm() function) and a path to the directory where you would like the plots to be saved (must have / on the end).
#The function returns a ggarrange object that combines all of the plots. It also outputs each of the plots (as well as the combined plot) to the path specified by outp. 
#It outputs a total of five plots which analyze the following: linearity check of each regressor, bias and scedasticity, correlation of errors for each regressor, and the normality of residuals.
#The plot sizes in the function may have to be adjusted based on the number of regressors in your model.
#Example usage is displayed after function end.

Assumption_Check = function(l_m, outp){
  df = l_m[['model']]
  df$residuals = l_m[["residuals"]]
  df = df[,-which(colnames(df)==l_m[["call"]][["formula"]][[2]])]
  
  #Assumption 1 - Linearity Check
  a1pls = list()#assumption 1 plotlist
  for (col in colnames(df)[-which(colnames(df)=='residuals')]){
    dfnew = data.frame(x = df[,col], y = df$residuals)
    a1pls[[col]] = ggplot(data = dfnew, aes(x =x, y=y))+
        geom_jitter(size = 0.3)+plot_opts+labs(x = col, y = 'Residuals')+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      geom_hline(yintercept=0,linetype="dashed",color = "red", size=1)+theme(axis.title.x = element_text(size = 10))
  }
  p1 = ggarrange(plotlist= a1pls)
  p1 = annotate_figure(p1,text_grob("Linearity Checks", color = "red", face = "bold", size = 20))
  ggsave(filename = paste(outp,'linearity.png',sep=""), plot = p1, dpi = 600, height = length(a1pls), width = length(a1pls)*1.5, units = 'in')
  
  #Assumption 2 - Bias and Scedasticity 
  df2 = data.frame(Fit = l_m$fitted.values, Residuals = l_m$residuals)
  p2 = ggplot(data = df2, aes(x = Fit, y = Residuals))+
    geom_point()+plot_opts+geom_hline(yintercept=0,linetype="dashed",color = "red", size=1)+labs(title = 'Bias and Scedasticity\nCheck')+theme(plot.title = element_text(color = 'red'))
  ggsave(filename = paste(outp,'bias_sced.png',sep=""), height = 4, width = 4, units = 'in', plot = p2, dpi = 600)
  
  #Assumption 3 - Correlation of Errors
  a3pls = list()
  for (n in colnames(df[,-which(colnames(df)=='residuals')])){
    dfnew = data.frame(y = df$residuals[order(df[,n])], x = 1:nrow(df))
    a3pls[[n]] = ggplot(data = dfnew, aes(x = x, y = y))+geom_jitter(size = 0.3)+
      plot_opts+labs(title = paste('Sorted By:\n', n), x = 'Index', y='Residuals')+geom_hline(yintercept=0,linetype="dashed",color = "red", size=1)+theme(plot.title = element_text(size = 10))
  }
  p3 = ggarrange(plotlist = a3pls)
  p3 = annotate_figure(p3,text_grob("Error Independence Check", color = "red", face = "bold", size = 20))
  ggsave(filename = paste(outp,'inderror.png',sep=""), plot = p3, dpi = 600, height = 1.4*length(a3pls), width = 1.4*length(a3pls)*1.5, units = 'in')

  #Assumption 4 - Normality of Residuals
  shapres = shapiro.test(l_m$residuals)
  p4 = ggplot(df)+
    geom_qq(aes(sample = residuals))+geom_qq_line(aes(sample= residuals))+
    plot_opts+ labs(title = paste('Normality of Residuals\n', 'Shapiro Wilks Results:\n W = ', as.character(round(shapres$statistic,3)), ', p = ', as.character(round(shapres$p.value,5))), x = 'Theoretical Values', y = 'Sample Values')+ theme(plot.title = element_text(color = 'red'))
  ggsave(filename = paste(outp,'normres.png',sep=""), plot = p4, dpi = 600, height = 5, width = 5, units = 'in')
  
  pfinal = ggarrange(plotlist = list(p1, p2, p3, p4))
  pfinal= annotate_figure(pfinal,text_grob("Model Assumption Check", face = "bold", size = 26))
  ggsave(filename = paste(outp,'all_assum.png',sep=""), plot = pfinal, dpi = 600, width = 16, height = 16, units = 'in')
  return(pfinal)
}
