data<-read.csv("100-sales-records.csv")
View(data)


# Change the value of a and b then test will automatically change according to the input
a=data$Sales_Channel
b=data$Total_Profit
library(MASS)
library(dplyr)
library(reshape)
library(reshape2)
library(stringr)
if (is.character(a)|is.factor(a))
  {
  if(is.numeric(b)|is.integer(b))
  {
    if (length(unique(a))>2)
    {
      print("Anova")
      anova_rel=aov(b ~ a,data)
      summary(anova_rel)
      anova_pvalue1=anova(anova_rel)$'Pr(>F)'[1]
      anova_pvalue2=anova()$'Pr(>F)'[2]
      
      d<-c(anova_pvalue1,anova_pvalue2)
      
      for(i in d){anova_rel
        if(i<0.05)
        {
          cat("Significant difference exist for",i)
        }else
        {
          cat("\n no significant difference for",i)
        }
      }
      
      
    }else{
      print("Ttest")
      ttest_rel1=t.test(b ~ a)   #Canbe written like this also
      
      ttest_rel
      ttest_pvalue=ttest_rel$p.value
      ttest_pvalue
      
      if(ttest_pvalue<0.05)
      {
        print("Significant difference exist")
      }else
      {
        print("no significant difference")
      }
    }
  }else{
    print("Chi")
    chi_rel=chisq.test(a,b)
    chi_pvalue=chi_rel$p.value
    chi_rel
    
    if(chi_pvalue<0.05)
    {
      cat("Significant difference exist for",chi_pvalue)
    }else
    {
      cat("\nno significant difference for",chi_pvalue)
    }
    
  }
  
}else{
  if(is.character(b)|is.factor(b)){
    if(length(unique(b))>2)
    {
      print("Anova")
      anova_rel=aov(b ~ a,data)
      summary(anova_rel)
      anova_pvalue1=anova(anova_rel)$'Pr(>F)'[1]
      anova_pvalue2=anova(anova_rel)$'Pr(>F)'[2]
      
      d<-c(anova_pvalue1,anova_pvalue2)
      
      for(i in d){
        if(i<0.05)
        {
          cat("Significant difference exist for",i)
        }else
        {
          cat("\nno significant difference for",i)
        }
      }
      
    }else{
      print("Ttest")
      ttest_rel1=t.test(b ~ a)   #Canbe written like this also
      
      ttest_rel
      ttest_pvalue=ttest_rel$p.value
      ttest_pvalue
      
      if(ttest_pvalue<0.05)
      {
        print("Significant difference exist")
      }else
      {
        print("no significant difference")
      }
      
      
    }
  }else
  {
    print("Correlation")
    CR=cor(a,b)
    CR
    
    if(CR>0.5){
      cat("There is strong positive corelation between",names(data)[8],"and",names(data)[9])
    }else
    {
      cat("There is strong negative corelation between",names(data)[8],"and",names(data)[9])
    }
  }
}

