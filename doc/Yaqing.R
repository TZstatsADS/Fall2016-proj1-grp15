library(data.table)
library(dplyr)
library(ggplot2)
library(fmsb)
library(choroplethr)
library(choroplethrMaps)

#read data
cols <- c("WAGP", "ST", "AGEP", "ESR", "CIT", "YOEP", "COW", "SEX" , "WKHP","WKW","SOCP","POWSP","POVPIP","SCHL")

before_data11<-fread("/Users/YaqingXie/Desktop/Applied Data Science/Week1/csv_pus(2007)/ss07pusa.csv", select=cols)
before_data12<-fread("/Users/YaqingXie/Desktop/Applied Data Science/Week1/csv_pus(2007)/ss07pusa.csv", select=c(160:239))
before_data21<-fread("/Users/YaqingXie/Desktop/Applied Data Science/Week1/csv_pus(2007)/ss07pusb.csv", select=cols)
before_data22<-fread("/Users/YaqingXie/Desktop/Applied Data Science/Week1/csv_pus(2007)/ss07pusb.csv", select=c(160:239))
before_data1 <- cbind(before_data11, before_data12)
before_data2 <- cbind(before_data21, before_data22)
before_data<- subset(rbind(before_data1, before_data2))

after_data11<-fread("/Users/YaqingXie/Desktop/Applied Data Science/Week1/csv_pus/ss14pusa.csv", select=cols)
after_data12<-fread("/Users/YaqingXie/Desktop/Applied Data Science/Week1/csv_pus/ss14pusa.csv", select=c(205:284))
after_data21<-fread("/Users/YaqingXie/Desktop/Applied Data Science/Week1/csv_pus/ss14pusb.csv", select=cols)
after_data22<-fread("/Users/YaqingXie/Desktop/Applied Data Science/Week1/csv_pus/ss14pusb.csv", select=c(205:284))
after_data1 <- cbind(after_data11, after_data12)
after_data2 <- cbind(after_data21, after_data22)
after_data<- subset(rbind(after_data1, after_data2))

#filter data: only people currenlty employeed in US
before_data <- before_data[!(before_data$ESR %in% c(3,6,NA))]
before_data <- before_data[before_data$POWSP >= 1 & before_data$POWSP <= 56]
after_data <- after_data[!(after_data$ESR %in% c(3,6,NA))]
after_data <- after_data[after_data$POWSP >= 1 & after_data$POWSP <= 56]

#job code for STEM occupations
#source: http://www.bls.gov/soc/Attachment_C_STEM.pdf
before_stem_job_codes = c('113021','119041','119111','119121','151021','151030','151041','151061','151071','151081','1510XX','152011','152031','1520XX','171010','171020','172011','172041','172051','172061','172070','172081','1720XX','172110','172121','172131','172141','1721XX','1721YY','173010','173020','173031','191010','191020','191030','191040','192010','192021','192030','192040','192099','193011','193020','193030','193051','1930XX','194011','194021','194031','194041','1940XX','254010','291011','291020','291031','291041','291051','291060','291071','291081','291111','291121','291122','291123','291124','291125','291126','291127','291129','291131','291199','292010','292021','292030','292041','292050','292061','292071','292081','292090','299000','414010','419031')
after_stem_job_codes = c('113021','119041','119121','151111','151121','151122','151131','151132','151133','151134','151141','151142','151143','151151','151152','151199','152011','152021','152031','152041','152099','171021','171022','172011','172021','172031','172041','172051','172061','172071','172072','172081','172111','172112','172121','172131','172141','172151','172161','172171','172199','173012','173013','173019','173021','173022','173023','173024','173025','173026','173027','173029','173031','191011','191012','191012','191021','191022','191023','191029','191031','191032','191041','191042','191099','192011','192012','192021','192031','192032','192041','192042','192043','192099','194011','194021','194031','194041','194051','194091','194092','194093','251021','251022','251032','251041','251042','251043','251051','251052','251053','251054','414011','419031')

#categorize job into stem and non-stem
before_data$SOCP[!(before_data$SOCP %in% before_stem_job_codes) & (before_data$SOCP!="")] <- "NON-STEM"
before_data$SOCP[before_data$SOCP %in% before_stem_job_codes] <- "STEM"
after_data$SOCP[!(after_data$SOCP %in% after_stem_job_codes) & (after_data$SOCP!="")] <- "NON-STEM"
after_data$SOCP[after_data$SOCP %in% after_stem_job_codes] <- "STEM"

#rename gender
before_data$SEX[before_data$SEX == 1] <- "Male"
before_data$SEX[before_data$SEX == 2] <- "Female"
after_data$SEX[after_data$SEX == 1] <- "Male"
after_data$SEX[after_data$SEX == 2] <- "Female"

#rename class of worker
before_data$COW[before_data$COW == 1] <- "For-profit Company"
before_data$COW[before_data$COW == 2] <- "Not-for-profit Organization"
before_data$COW[before_data$COW == 3] <- "Local Government"
before_data$COW[before_data$COW == 4] <- "State Government"
before_data$COW[before_data$COW == 5] <- "Federal Government"
before_data$COW[before_data$COW == 6] <- "Self-employeed & Not Incorporated"
before_data$COW[before_data$COW == 7] <- "Self-employeed & Incorporated"
before_data$COW[before_data$COW == 8] <- "Family Business"
after_data$COW[after_data$COW == 1] <- "For-profit Company"
after_data$COW[after_data$COW == 2] <- "Not-for-profit Organization"
after_data$COW[after_data$COW == 3] <- "Local Government"
after_data$COW[after_data$COW == 4] <- "State Government"
after_data$COW[after_data$COW == 5] <- "Federal Government"
after_data$COW[after_data$COW == 6] <- "Self-employeed & Not Incorporated"
after_data$COW[after_data$COW == 7] <- "Self-employeed & Incorporated"
after_data$COW[after_data$COW == 8] <- "Family Business"

#recode number of weeks worked in the past year
after_data$WKW[after_data$WKW==1] <- 51
after_data$WKW[after_data$WKW==2] <- 48.5
after_data$WKW[after_data$WKW==3] <- 43.5
after_data$WKW[after_data$WKW==4] <- 33
after_data$WKW[after_data$WKW==5] <- 20
after_data$WKW[after_data$WKW==6] <- 7

#plotting
#1 POWSP-stem/nonstem-before/after  MAP
before_stem <- before_data[before_data$SOCP == 'STEM']
before_state_stem <- subset(before_stem, select=c("ST","POWSP"))
before_nonstem <- before_data[before_data$SOCP == 'NON-STEM']
before_state_nonstem <- subset(before_nonstem, select=c("ST","POWSP"))
after_stem <- after_data[after_data$SOCP == 'STEM']
after_state_stem <- subset(after_stem, select=c("ST","POWSP"))
after_nonstem <- after_data[after_data$SOCP == 'NON-STEM']
after_state_nonstem <- subset(after_nonstem, select=c("ST","POWSP"))

statenames <- data.frame(cbind(state.regions[,1],state.regions[,3]))
names(statenames) <- c('region', 'region_code')

getstate <- function(vector){
  temp <- data.frame(vector)
  names(temp) <- c("region_code")
  new_dataframe <- merge(temp,statenames, by.x="region_code", by.y='region_code')
  new_dataframe <- table(new_dataframe$region)
  new_dataframe <- data.frame(new_dataframe)
  names(new_dataframe) <- c('region','value')
  return(new_dataframe)
}

before_stem_ST <- getstate(before_state_stem$ST)
before_stem_POW <- getstate(before_state_stem$POWSP)
before_nonstem_ST <- getstate(before_state_nonstem$ST)
before_nonstem_POW <- getstate(before_state_nonstem$POWSP)
after_stem_ST <- getstate(after_state_stem$ST)
after_stem_POW <- getstate(after_state_stem$POWSP)
after_nonstem_ST <- getstate(after_state_nonstem$ST)
after_nonstem_POW <- getstate(after_state_nonstem$POWSP)

state_choropleth(before_nonstem_POW,
                 title      = "Before STEM Policy: NON-STEM Job Allocation",
                 legend     = "Number of Occupations",
                 num_colors = 1)
state_choropleth(after_nonstem_POW,
                 title      = "After STEM Policy: NON-STEM Job Allocation",
                 legend     = "Number of Occupations",
                 num_colors = 1)
state_choropleth(before_stem_POW,
                 title      = "Before STEM Policy: STEM Job Allocation",
                 legend     = "Number of Occupations",
                 num_colors = 1)
state_choropleth(after_stem_POW,
                 title      = "After STEM Policy: STEM Job Allocation",
                 legend     = "Number of Occupations",
                 num_colors = 1)

#2 wage&wkhp&wkw&povpip&schl-stem/nonstem-before/after
#survey weight
library(survey)
df_before<-svrepdesign(variables=before_data[,1:16], 
                 repweights=before_data[,17:86], type="BRR",combined.weights=TRUE,
                 weights=before_data$PWGTP)
summary(df_before)
svymean(~ WAGP,df_before, na.rm = T)
df_after<-svrepdesign(variables=after_data[,1:16], 
                       repweights=after_data[,17:86], type="BRR",combined.weights=TRUE,
                       weights=after_data$PWGTP)
summary(df_before)
svymean(~ WAGP,df_before, na.rm = T)

radardata <- data.frame(group = character(), wage = numeric(), hr_per_wk = numeric, wk_per_yr = numeric(), income_to_poverty = numeric(), degree = numeric())
radardata <- rbind(radardata, data.frame(group = "before_stem", wage = mean(before_stem$WAGP), hr_per_wk=mean(before_stem$WKHP), wk_per_yr=mean(before_stem$WKW), income_to_poverty=mean(before_stem$POVPIP,na.rm=TRUE), degree=mean(before_stem$SCHL)))
radardata <- rbind(radardata, data.frame(group = "before_nonstem", wage = mean(before_nonstem$WAGP), hr_per_wk=mean(before_nonstem$WKHP), wk_per_yr=mean(before_nonstem$WKW), income_to_poverty=mean(before_nonstem$POVPIP,na.rm=TRUE), degree=mean(before_nonstem$SCHL)))
radardata <- rbind(radardata, data.frame(group = "after_stem", wage = mean(after_stem$WAGP), hr_per_wk=mean(after_stem$WKHP), wk_per_yr=mean(after_stem$WKW), income_to_poverty=mean(after_stem$POVPIP,na.rm=TRUE), degree=mean(after_stem$SCHL)))
radardata <- rbind(radardata, data.frame(group = "after_nonstem", wage = mean(after_nonstem$WAGP), hr_per_wk=mean(after_nonstem$WKHP), wk_per_yr=mean(after_nonstem$WKW), income_to_poverty=mean(after_nonstem$POVPIP,na.rm=TRUE), degree=mean(after_nonstem$SCHL)))
radardata$wage <- radardata$wage / 1000
radardata2 <- rbind(c(max(radardata$wage),max(radardata$hr_per_wk),max(radardata$wk_per_yr),max(radardata$income_to_poverty),max(radardata$degree)),
                    c(min(radardata$wage),min(radardata$hr_per_wk),min(radardata$wk_per_yr),min(radardata$income_to_poverty),min(radardata$degree)), 
                    radardata[,c(2:6)])

colors_border=c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.5,0.2,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4), rgb(0.5,0.2,0.1,0.4) )
radarchart(radardata2 , axistype=1 , 
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1,
           #custom the grid
           cglcol="grey", cglty=2, axislabcol="grey", 
           caxislabels = c('Min            ','','','','Max         '),
           cglwd=0.5
)
legend(x=0.9, y=1.4, legend = radardata[,1], bty = "n", pch=20 , col=colors_border , cex=0.9, pt.cex=1)
title("Changes in Working Conditions After STEM Policy Went Out ", cex.main=1)

#3 cow, sex
cow <- data.frame(table(before_stem$COW))
cow <- cbind(cow,table(before_nonstem$COW))
cow <- cbind(cow,table(after_stem$COW))
cow <- cbind(cow,table(after_nonstem$COW))
cow <- cow[,c(1,2,4,6,8)]
names(cow) <- c('Class', 'before_stem',  'before_nonstem','after_stem','after_nonstem')
cow[,2] = cow[,2]/sum(cow[,2])
cow[,3] = cow[,3]/sum(cow[,3])
cow[,4] = cow[,4]/sum(cow[,4])
cow[,5] = cow[,5]/sum(cow[,5])

sex <- data.frame(table(before_stem$SEX))
sex <- cbind(sex,table(before_nonstem$SEX))
sex <- cbind(sex,table(after_stem$SEX))
sex <- cbind(sex,table(after_nonstem$SEX))
sex <- sex[,c(1,2,4,6,8)]
names(sex) <- c('Sex', 'before_stem',  'before_nonstem','after_stem','after_nonstem')
sex[,2] = sex[,2]/sum(sex[,2])
sex[,3] = sex[,3]/sum(sex[,3])
sex[,4] = sex[,4]/sum(sex[,4])
sex[,5] = sex[,5]/sum(sex[,5])

cow_melt = melt(cow, id.vars = c('Class'))
ggplot(datm,aes(x = variable, y = value,fill = Class)) + 
  geom_bar(position = "fill",stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Changes in Class of Work After STEM Policy Went Out") +
  theme(plot.title = element_text(size = rel(1),face="bold")) +
  xlab("") +
  ylab("Component Proportion")

colors_in1=c('rosybrown1', 'lightsteelblue2')
colors_in2=c('rosybrown3', 'lightsteelblue4')
colors_in3=c('tan1','seagreen1')
colors_in4=c('tan3','seagreen3')
colors_in=c('rosybrown1', 'lightsteelblue2','rosybrown3', 'lightsteelblue4')
pie(x=c(sex[1,2],sex[2,2]),labels=c(""),cex=1,radius=1,col=colors_in1)
par(new=TRUE)
pie(x=c(sex[1,3],sex[2,3]),labels=c(""), cex=0.8,radius=0.75,col=colors_in2)
par(new=TRUE)
pie(x=c(sex[1,4],sex[2,4]),labels=c(""),cex=1,radius=0.5,col=colors_in3)
par(new=TRUE)
pie(x=c(sex[1,5],sex[2,5]),labels=c(""),cex=0.8,radius=0.25,col=colors_in4)
legend(x=-1.3, y=-1, legend = c('',''), col = colors_in1, bty = "n", pch=20 ,cex=0.8, pt.cex=2)
legend(x=-1, y=-1, legend = c('',''), col = colors_in3, bty = "n", pch=20 ,cex=0.8, pt.cex=2)
legend(x=-1.8, y=-1, legend = c('female','male'), col = c('white','white'), bty = "n", pch=20 ,cex=0.8, pt.cex=2)
text('2007', x=-1.2, y=-1,cex=0.8)
text('2014', x=-0.9, y=-1,cex=0.8)
legend(x=1.,y=-1,legend=c('stem','non-stem'),col=c('white','grey'),bty = "n",  pch=20 ,cex=0.8, pt.cex=2)
legend(x=0.98,y=-0.98,pt.cex=1.4, bty = "n", legend = c(""), pch = 21)
title("Changes in Gender Proportion of Jobs After STEM Policy Went Out")

#4 year of entry-stem/nonstem-before/after (for those who naturalized)
before_naturalize <- before_data[before_data$CIT == 4]
after_naturalize <- after_data[after_data$CIT == 4]
before_naturalize$AGEOFENTRY <- before_naturalize$YOEP - (2007 - before_naturalize$AGEP)
after_naturalize$AGEOFENTRY <- after_naturalize$YOEP - (2014 - after_naturalize$AGEP)

before_naturalize_stem <- before_naturalized[before_naturalize$SOCP == 'STEM']
before_naturalize_nonstem <- before_naturalized[before_naturalize$SOCP == 'NON-STEM']
after_naturalize_stem <- after_naturalized[after_naturalize$SOCP == 'STEM']
after_naturalize_nonstem <- after_naturalized[after_naturalize$SOCP == 'NON-STEM']

before_naturalize_age_stem <- cut(before_naturalize_stem$AGEOFENTRY, seq(0,100,length.out=21))
before_naturalize_age_stem <- data.frame(table(before_naturalize_age_stem))
before_naturalize_age_nonstem <- cut(before_naturalize_nonstem$AGEOFENTRY, seq(0,100,length.out=21))
before_naturalize_age_nonstem <- data.frame(table(before_naturalize_age_nonstem))
after_naturalize_age_stem <- cut(after_naturalize_stem$AGEOFENTRY, seq(0,100,length.out=21))
after_naturalize_age_stem <- data.frame(table(after_naturalize_age_stem))
after_naturalize_age_nonstem <- cut(after_naturalize_nonstem$AGEOFENTRY, seq(0,100,length.out=21))
after_naturalize_age_nonstem <- data.frame(table(after_naturalize_age_nonstem))

naturalized_age_stem <- merge(before_naturalize_age_stem,after_naturalize_age_stem, by.x="before_naturalize_age_stem", by.y='after_naturalize_age_stem')
names(naturalized_age_stem) <- c('age_range','before_stem','after_stem')
naturalized_age_stem$before_stem = naturalized_age_stem$before_stem/sum(naturalized_age_stem$before_stem)
naturalized_age_stem$after_stem = naturalized_age_stem$after_stem/sum(naturalized_age_stem$after_stem)
naturalized_age_nonstem <- merge(before_naturalize_age_nonstem,after_naturalize_age_nonstem, by.x="before_naturalize_age_nonstem", by.y='after_naturalize_age_nonstem')
names(naturalized_age_nonstem) <- c('age_range','before_nonstem','after_nonstem')
naturalized_age_nonstem$before_nonstem = naturalized_age_nonstem$before_nonstem/sum(naturalized_age_nonstem$before_nonstem)
naturalized_age_nonstem$after_nonstem = naturalized_age_nonstem$after_nonstem/sum(naturalized_age_nonstem$after_nonstem)

naturalized_age <- cbind(naturalized_age_stem,naturalized_age_nonstem)
naturalized_age <- naturalized_age[,c(1,2,5,3,6)]
naturalized_age <- melt(naturalized_age, id='age_range')
ggplot(data=naturalized_age,
       aes(x=age_range, y=value, col=variable, group=variable)) +
       geom_line()+
       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
       ggtitle("Changes in Age of Entry Among Immigrants") +
       theme(plot.title = element_text(size = rel(1),face="bold")) +
       ylab("Proportion")

