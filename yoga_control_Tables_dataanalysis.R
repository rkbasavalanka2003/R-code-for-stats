#install.packages('foreign')
rm(list = all())
setwd("C:/Users/MY HOME/Desktop/yoga")
## loading libraries
library(foreign)
library(dplyr)
library(tidyr)
library(gmodels)
library(tableHTML)
options(digits = 3)

## Reading .sav data
dataset = read.spss("Final raw data1.sav", to.data.frame=TRUE)

#write.csv(dataset,file = "rawdata.csv")
## understanding the data
colnames(dataset)
str(dataset)

## separating numeric variables from factor variables
num_data = dataset[ ,sapply(dataset, is.numeric)]
str(num_data)

##  pre_post_data  - including  gender and group variables
x <- c("pre", "Pre", "post","Post")
pre_post_data <- num_data %>% dplyr:: select(grep(paste(x, collapse = "|"),names(num_data)))
pre_post_data$gender = dataset$Sex
pre_post_data$group = dataset$groups

## ordering columns and selecting columns
##so that pre and post treatment variables are side by side
ordered_colnames = c("group","gender","WtKgPre","WtkgPost","BMIPre","BMIpost","HbA1cpre","HbA1cpost",
                     "FBSpre","FBSpost","PPBSpre","PPBSpost","BPPreSBP","BPPostSBP","BPPreDBP","BPPostDBP",
                     "T.Cholpre","T.cholpost", "TGpre" ,"TGPost","LDLPre","LDLpost","HDLpre" ,"HDLpost","VLDLpre","VLDLpost", 
                     "HbPre"  ,    "HbPost" ,"Ureapre" ,"Ureapost","PulsePre","PulsePost","ESRPre","ESRPost","Datepre","Datepost",             
                     "Creatpre","Creatpost")
pre_post_data = pre_post_data %>% select(ordered_colnames)
str(pre_post_data)
colSums(is.na(pre_post_data))# 4 missing values in HBpost
#write.csv(pre_post_data,file = "pre_post_data.csv")

#### listing pretreatment and posttreatment variables
prelist = ordered_colnames[grepl('pre',ordered_colnames,ignore.case = TRUE)]
prelist
postlist = ordered_colnames[grepl('post',ordered_colnames,ignore.case = TRUE)]
postlist

## separating yoga data and control data and also for male and female and pre and post 
## total 8 data sets
yoga = pre_post_data %>% filter(group == "Experiment")
str(yoga)
#write.csv(yoga,"yoga.csv")
control = pre_post_data %>% filter(group == "Control   ")
str(control)
#write.csv(control,"control.csv")
###############
yoga_male = yoga %>% filter(gender == "Male  ")
str(yoga_male)
control_male = control %>% filter(gender == "Male  ") 
str(control_male)
yoga_female = yoga %>% filter(gender == "Female")
str(yoga_female)
control_female = control %>% filter(gender == "Female") 
str(control_female)

######
yoga_male_pre = yoga_male[prelist]
str(yoga_male_pre)
yoga_male_post = yoga_male[postlist]
str(yoga_male_post)
control_male_pre = control_male[prelist]
str(control_male_pre)
control_male_post = control_male[postlist]
str(control_male_post)

yoga_female_pre = yoga_female[prelist]
str(yoga_female_pre)
yoga_female_post = yoga_female[postlist]
str(yoga_female_post)
control_female_pre = control_female[prelist]
str(control_female_pre)
control_female_post = control_female[postlist]
str(control_female_post)

############## creating pretreatmentdata and posttreatment data
pretreatdata = pre_post_data[prelist]
pretreatdata$group = pre_post_data$group
pretreatdata$gender = pre_post_data$gender

posttreatdata = pre_post_data[postlist]
posttreatdata$group = pre_post_data$group
posttreatdata$gender = pre_post_data$gender


### creating 4 datasets - predata_yoga, postdata_yoga, predata_control, postdata_control

predata_yoga = yoga[prelist]
str(predata_yoga)
predata_control = control[prelist]
str(predata_control)

postdata_yoga = yoga[postlist]
str(postdata_yoga)
postdata_control = control[postlist]
str(postdata_control)
vars = names(pre_post_data)
vars = vars[-c(1,2)]
##########################################################
#rownames = prelist
#rownames = gsub('pre', "_", rownames, ignore.case = TRUE)
########################################################
### loading functions for mean, sd cohensDES and ftest

mean_sd = function(df){
  df_means = df %>% summarise_at(names(df), mean.default)
  df_means = as.data.frame(t(df_means))
  df_sd = df %>% summarise_at(names(df), sd)
  df_sd = as.data.frame(t(df_sd))
  df_ci = df %>% summarise_at(names(df), ci)
  df_ci = as.data.frame(t(df_ci))
  ### as there 4 columns in ci results = estimate, lower, upper and stderror
  #####we want only Lower and upper
  df_ci = df_ci[,c(2,3)]
  df_results = cbind(df_means,df_sd,df_ci)
  colnames(df_results) = c("mean", "sd", "lb","ub")
  row.names(rownames)
  df_results = round(df_results,3)
  df_results = df_results %>% 
    unite(M_SD, mean, sd, sep = "+/-", remove = TRUE)
  df_results = df_results %>% 
    unite(CI_LB_UB, lb, ub, sep = "-", remove = TRUE)
  return (df_results)
}

#######################################################
cohensDES = function(df1,df2){
  cohensES = data.frame()
  for (i in 1:length(df1)){
    cohnsd =  cohens_d(df1[,i],df2[,i])
    cohnsd[1,1] = round(cohnsd[1,1],3)
    cohensES <- rbind(cohensES,cohnsd[1,1])
    options(digits = 3)
    count = count+1
  }
  colnames(cohensES) = 'cohendES'
  return(cohensES)
}

#############################################  
ftest = function(df1,df2){
  Ftest = data.frame()
  for (i in 1:length(df1)){
    ftest =  var.test(df1[,i],df2[,i])
    ftest$statistic[[1]] = round(ftest$statistic[[1]], digits = 3)
    if(ftest$p.value < 0.001 && ftest$p.value > 0.01) {
      Ftest <- rbind(Ftest,paste(as.character(ftest$statistic[[1]]),'**'))
    } else if( ftest$p.value < 0.001) {
      Ftest <- rbind(Ftest,paste(as.character(ftest$statistic[[1]]),'***'))
    } else {
      Ftest <- rbind(Ftest,ftest$statistic[[1]])
    }
  }
  colnames(Ftest) = 'Fvalue'
  return(Ftest)
}
###################################################  
## 
mytable = function(df1,df2,df3,df4){
  a = mean_sd(df1)
  b = mean_sd(df2)
  c = cohensDES(df1,df2)
  d = ftest(df1,df2)
  e = mean_sd(df3)
  f = mean_sd(df4)
  g = cohensDES(df3,df4)
  h = ftest(df3,df4) 
  varnames = colnames(df1)
  varnames = gsub('pre', "_", varnames, ignore.case = TRUE)
  table = cbind(varnames,a,b,c,d,e,f,g,h)
  colnames(table) = c('Variables','Pre M ±SD','95% C.I.(LB to UB)','Post M± SD','95% CI(L.B -U.B)',
                      '(ES)', 'Fvalue','Pre M ±SD','95% C.I.(LB to UB)','Post M± SD',
                      '95% CI(L.B -U.B)','(ES)', 'Fvalue')
  
  return(table)
}
######################################################

##############################################
#Table1

control_yoga = mytable(predata_yoga,postdata_yoga,predata_control,postdata_control)

control_yoga = tableHTML(control_yoga, class = 'table1',
                          rownames = FALSE,
                          second_header = list(c(1,6,13), c('group','Yoga Group', 'Control Group')),
                         width = rep(100,13),
                         footer = 'Legend:**p> 0.01, ***p<0.001 shows significant
                         improvement on the respective parameter 
                              within group and between group comparisons.  
                              ES = Effect size Cohen's  D; WtKg_=  Body Weight ; 
                              BMI_= Body Mass Index; HbA1c_ = Glycosylated Haemoglobin; 
                              FBS_ = Fasting Blood Glucose; PPBS_ = Post Prandial Blood Glucose;
                              BP_SBP = Systolic Blood Glucose; BP_DBP = Diastolic Blood Glucose; 
                              T.Chol_ = Total cholesterol; TG_ = Triglycerides;  
                              LDL = Low Density Lipoprotein; HDL = High Density Lipoprotein; 
                              VLDL = Very Low Density Lipoprotein',caption = 'Pre and Post results of anthropometric and clinical parameters
                               of T2DM patients of Yoga(n=151)intervention and control(n=153)' ) 
control_yoga = add_theme(control_yoga)
print(control_yoga)
write_tableHTML(control_yoga, file = 'control_yoga.html')

###############################################################################

##Table2
control_yoga_male = mytable(yoga_male_pre,yoga_male_post,
                            control_male_pre,control_male_post)


control_yoga_male = tableHTML(control_yoga_male,
                         rownames = FALSE,
                         second_header = list(c(1,6,12), c('group','Yoga Group (n=79) Male', 'Control Group (n= 60) Male')),
                         footer = 'Legend:** p> 0.01, *** p<0.001 shows significant
                          improvement on the respective parameter 
                              within group and between group comparisons.  
                              ES = Effect size Cohen's  D; WtKg_=  Body Weight ; 
                              BMI_= Body Mass Index; HbA1c_ = Glycosylated Haemoglobin; 
                              FBS_ = Fasting Blood Glucose; PPBS_ = Post Prandial Blood Glucose;
                              BP_SBP = Systolic Blood Glucose; BP_DBP = Diastolic Blood Glucose; 
                              T.Chol_ = Total cholesterol; TG_ = Triglycerides;  
                              LDL = Low Density Lipoprotein; HDL = High Density Lipoprotein; 
                              VLDL = Very Low Density Lipoprotein',
                         caption = "Pre and Post results of anthropometric and clinical parameters of
                         T2DM patients of Yoga (n=151) intervention and control (n=153) for male")
control_yoga_male
print(control_yoga_male)
write_tableHTML(control_yoga_male, file = 'control_yoga_male.html')
#########################################################
##Table3

control_yoga_female = mytable(yoga_female_pre,yoga_female_post,
                            control_female_pre,control_female_post)


control_yoga_female = tableHTML(control_yoga_female,
                              rownames = FALSE,
                              second_header = list(c(1,6,12), c('group','Yoga Group (n=72) Female', 'Control Group (n= 93) Female')),
                              footer = 'Legend:** p> 0.01, *** p<0.001 shows significant
                          improvement on the respective parameter 
                              within group and between group comparisons.  
                              ES = Effect size Cohen's  D; WtKg_=  Body Weight ; 
                              BMI_= Body Mass Index; HbA1c_ = Glycosylated Haemoglobin; 
                              FBS_ = Fasting Blood Glucose; PPBS_ = Post Prandial Blood Glucose;
                              BP_SBP = Systolic Blood Glucose; BP_DBP = Diastolic Blood Glucose; 
                              T.Chol_ = Total cholesterol; TG_ = Triglycerides;  
                              LDL = Low Density Lipoprotein; HDL = High Density Lipoprotein; 
                              VLDL = Very Low Density Lipoprotein',
                              caption = "Pre and Post results of anthropometric and clinical parameters of
                         T2DM patients of Yoga (n=151) intervention and control (n=153) for Female")

control_yoga_female
print(control_yoga_female)
write_tableHTML(control_yoga_female, file = 'control_yoga_female.html')
##################*************************##############################################


