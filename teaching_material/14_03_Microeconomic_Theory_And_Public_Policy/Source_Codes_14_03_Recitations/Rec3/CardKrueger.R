########### PRELIMINARY: SET WORKING DIRECTORY #################

path='/Users/andreamanera/Dropbox (MIT)/14-03 Spring 2020 TA Folder/5. Recitations - Spring 2020/Rec3'
outputPath = paste(path,'/out', sep='')
dataPath = paste(path,'/data', sep='')

setwd(path)
getwd()

######## LOADING DATA AND PACKAGES, INSTALL IF NOT PRESENT ##############

# Data Manipulation
if (inherits(try(library(dplyr)), "try-error")){
  install.packages('dplyr')
}
library('dplyr')

# Data Manipulation
if (inherits(try(library(foreign)), "try-error")){
  install.packages('foreign')
}
library('foreign')

# AER
if (inherits(try(library(AER)), "try-error")){
  install.packages('AER')
}
library('AER')




### FOR GRAPHS
library('ggplot2')
library("gridExtra")

########### IMPORT DATA ###########
data<- read.dta(paste(dataPath, '/fastfood.dta', sep=''))

####### GENERATE USEFUL DATA #########

# use mutate to add Full time Empl. before (FTE) and after the law (FTE2)
# According to CK (1994) definition
data <- mutate(data, FTE =  nmgrs + empft + (0.5 * emppt), 
               FTE2 =  nmgrs2 + empft2 + (0.5 * emppt2))

# state is 1 if stores are in NJ
data_NJ <- data %>% subset(state==1)
data_PA <- data %>% subset(state==0)


####### LOOK AT MEANS ########
data %>% group_by(state) %>% 
  summarize(mean(FTE, na.rm=T),
            mean(FTE2, na.rm=T))

data %>% group_by(state) %>% 
  summarize(sd(FTE, na.rm=T),
            sd(FTE2, na.rm=T))


########### T-TESTS #######

##### TEST FOR WHERE THE MEAN EMPLOYMENT IS IN NJ
t.test(data_NJ$FTE, mu=20, na.rm=T)

####### BUILD CHANGE IN FTE
data <- data %>% mutate(dFTE = FTE2-FTE)

######## TEST FOR DIFFERENCES
# state is 1 if stores are in NJ
data_NJ <- data %>% subset(state==1)
data_PA <- data %>% subset(state==0)

###### CARRY OUT T-TEST ASSUMING TRUE MEANS ARE THE SAME(corresponds to row 4. column (iii))

# the option var.equal tells r to use all the observations to compute 
# the variance (the variance of employment is assumed to be the same). Will give same p-val than regression below

t.test(data_NJ$dFTE, data_PA$dFTE, mu=0, var.equal = T)


###### SIMILAR RESULT WITH REGRESSION IN DIFFERENCES (
## the value of the estimate for state is the diff-in-diff, t-stat changes because we are also estimating a constant)
modelDiff = lm(formula = dFTE ~ state, data = data)
coeftest(modelDiff)

## Restrict the same to  only observations with non-missing wages in both periods like Card and Krueger, almost replicates table 4, col 1
dataAllWages = data[complete.cases(data.frame(data$wage_st,data$wage_st2, data$FTE, data$FTE2)),]
modelDiff = lm(formula = dFTE ~ state, data = dataAllWages)
coeftest(modelDiff)


######### RUN THE REGRESSION NOT IN DIFFERENCES (AS SHOWN IN RECITATION), standard errors increase ############
############ REGRESSION ANALYSIS ##########

#Reshape data to make them in long form

#### Create dataframe with employment before the change
dataBefore <- data.frame(id = data$sheet,
                          chain = data$chain,
                          state = data$state, 
                          empl = data$FTE) %>% 
  mutate(after = 0)

#### Create dataframe with employment after the change
dataAfter <- data.frame(id = data$sheet,
                        chain = data$chain,
                        state = data$state,
                          empl = data$FTE2) %>% 
  mutate(after = 1)

##### Concatenate vertically
dataReg <- data.frame(rbind(dataBefore, dataAfter))


### Specify the naive model of interest
model <- lm(formula = empl ~ after,
            data =dataReg,
            subset = 
              (dataReg$state == 1))

# obtain a robust summary
coeftest(model, vcov. = vcovHC, type = "HC1")

## Note, the t-stat is the same as doing this!:
t.test(data_NJ$FTE, data_NJ$FTE2, var.equal = T)

### Specify the CK model of interest
modelCK <- lm(formula = empl ~ state + after +  state*after,
            data =dataReg)
coeftest(modelCK, vcov. = vcovHC, type = "HC1")
coeftest(modelCK)

###### EXTRA: GRAPHS ###########
dataGraph <- dataReg%>% group_by(state)

ggplot(data= dataGraph, aes(after, empl, colour = factor(state)))+
  geom_jitter()+
  geom_abline(aes(intercept=mean(data_NJ$FTE, na.rm=T),
                  slope = (mean(data_NJ$FTE2, na.rm=T)-mean(data_NJ$FTE, na.rm=T)) ,
              colour = "reg"),size = 1.5)+
  geom_abline(aes(intercept=mean(data_PA$FTE, na.rm=T),
                  slope = (mean(data_PA$FTE2, na.rm=T)-mean(data_PA$FTE, na.rm=T)) ,
                  colour = "reg2"),size = 1.5)+
  geom_abline(aes(intercept=mean(data_NJ$FTE, na.rm=T),
                  slope = (mean(data_PA$FTE2, na.rm=T)-mean(data_PA$FTE, na.rm=T)) * 
                    mean(data_NJ$FTE, na.rm=T)/mean(data_PA$FTE, na.rm=T),
                  colour = "reg3"),linetype = "dashed",size = 1.5)+
  scale_color_manual( name = "",values = c( "palegreen2","salmon1", "palegreen2","salmon1","seagreen3" ),
                      labels = c("PA","NJ","Trend in NJ","Trend in PA","Counterfactual NJ"  ),
                      guide = guide_legend(override.aes = list(
                        linetype = c(rep("blank", 2), "solid", "solid","dashed"),
                        shape = c(rep(1, 2), NA, NA,NA),
                        size = c(rep(1,5)))))+
  xlab("Time")+
  ylab("Number of Employees")+
  theme(legend.position="bottom")+
  ylim(5,50)
  
### Graphs with regressions together
ggplot(NULL, aes(x = after, y=empl))+
  geom_jitter(data = subset(dataReg, state==1), aes(colour = "NJ"))+
  geom_jitter(data = subset(dataReg, state==0), aes(colour = "PA"))+
   geom_smooth(data = subset(dataReg, state==1), method='lm',formula = y~x, na.rm = T,
              aes(colour = "reg1"))+
  geom_smooth(data = subset(dataReg, state==0), method='lm',formula = y~x, na.rm = T,linetype = "dashed",
              aes(colour = "reg2"))+
  scale_color_manual( name = "",values = c("palegreen2","salmon1","palegreen3", "salmon3"),
                      labels = c("NJ","PA","Trend in NJ","Trend in PA"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("blank", "blank","solid", "solid"),
                        shape = c(1,1,NA,NA)
                      )))+
  coord_cartesian( ylim=c(5,40))+
  xlab("Time")+
  ylab("Number of Employees")+
  theme(legend.position="bottom")
  

## graphs with regression separated
p1 =
  ggplot(NULL, aes(x = after, y=empl))+
  geom_jitter(data = subset(dataReg, state==1), aes(colour = "NJ"))+
  geom_smooth(data = subset(dataReg, state==1), aes(colour = "reg"), method='lm',formula = y~x, na.rm = T,fullrange=TRUE)+
  coord_cartesian( ylim=c(5,40))+
  scale_color_manual( name = "",values = c( "palegreen2", "palegreen3"),
                      labels = c("NJ","Trend in NJ"),
                      guide = guide_legend(override.aes = list(
                        linetype = c(rep("blank", 1), "solid"),
                        shape = c(1, NA)
                        )))+
  xlab("Time")+
  ylab("Number of Employees")+
  theme(legend.position="bottom")
p2 = 
  ggplot(NULL, aes(x = after, y=empl))+
  geom_jitter(data = subset(dataReg, state==0), aes(colour = "PA"))+
  geom_smooth(data = subset(dataReg, state==0),aes(colour = "reg"), method='lm',formula = y~x, na.rm = T,linetype = "dashed",fullrange=TRUE)+
  coord_cartesian( ylim=c(5,40))+
  scale_color_manual( name = "",values = c( "salmon1", "salmon3"),
                      labels = c("PA","Trend in PA"),
                      guide = guide_legend(override.aes = list(
                        linetype = c(rep("blank", 1), "dashed"),
                        shape = c(1, NA)
                      )))+
  xlab("Time")+
  ylab("Number of Employees")+
  theme(legend.position="bottom")

grid.arrange(p1,p2,nrow = 1)


###### DIFF-in-DIFF graph ############

#offset to subtract from the fit to get the counterfactual NJ
diffmeans0 = mean(data_PA$FTE, na.rm = T) - mean(data_NJ$FTE, na.rm = T) 

  ggplot(NULL, aes(x = after, y=empl))+
  geom_jitter(data = subset(dataReg, state==1), aes(colour = "NJ"))+
  geom_jitter(data = subset(dataReg, state==0), aes(colour = "PA"))+
  geom_smooth(data = subset(dataReg, state==1), method='lm',formula = y~x, na.rm = T,
              aes(colour = "reg1"))+
  geom_smooth(data = subset(dataReg, state==0), method='lm',formula = y - diffmeans0~x, na.rm = T,linetype = "dashed",
              aes(colour = "reg2"))+
  scale_color_manual( name = "",values = c("palegreen2","salmon1","palegreen3", "salmon3"),
                      labels = c("NJ","PA","Trend in NJ","Counterfactual NJ"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("blank", "blank","solid", "dashed"),
                        shape = c(1,1,NA,NA)
                      )))+
  coord_cartesian( ylim=c(10,30))+
  xlab("Time")+
  ylab("Number of Employees")+
  theme(legend.position="bottom")

#### Only Lines ####
  
  #offset to subtract from the fit to get the counterfactual NJ
  diffmeans0 = mean(data_PA$FTE, na.rm = T) - mean(data_NJ$FTE, na.rm = T) 
  
  ggplot(NULL, aes(x = after, y=empl))+
    geom_smooth(data = subset(dataReg, state==1), method='lm',formula = y~x, na.rm = T,
                aes(colour = "reg1"))+
    geom_smooth(data = subset(dataReg, state==0), method='lm',formula = y - diffmeans0~x, na.rm = T,linetype = "dashed",
                aes(colour = "reg2"))+
    scale_color_manual( name = "",values = c("blue", "blue"),
                        labels = c("Trend in NJ","Counterfactual NJ"),
                        guide = guide_legend(override.aes = list(
                          linetype = c("solid", "dashed"),
                          shape = c(NA,NA)
                        )))+
    coord_cartesian( ylim=c(15,25))+
    xlab("Time")+
    ylab("Number of Employees")+
    theme(legend.position="bottom")
  


