if (!('tidyverse' %in% installed.packages())) install.packages('tidyverse')
library(tidyverse)
if (!('ggplot2' %in% installed.packages())) install.packages('ggplot2')
library(ggplot2)
if (!('ggridges' %in% installed.packages())) install.packages('ggridges')
library(ggridges)
if (!('ggmosaic' %in% installed.packages())) install.packages('ggmosaic')
library(ggmosaic)
if (!('moments' %in% installed.packages())) install.packages('moments')
library(moments)

mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)),
                      legend.title = element_text(colour = "steelblue", face = "bold.italic",
                                                  family = "Helvetica"),
                      legend.text = element_text(face = "italic", colour="steelblue4",family =
                                                   "Helvetica"),
                      axis.title = element_text(family = "Helvetica", size = (14), colour =
                                                  "steelblue4"),
                                                axis.text = element_text(family = "Courier", colour = "cornflowerblue", size =(10)))

df <- read.csv("tourn_1_calibration_csv.csv", header = T, stringsAsFactors = TRUE)
dim(df)
str(df)

#Function to calculate mode
calmode <- function(a) {
  v <- unique(a)  
  v[which.max(tabulate(match(a,v)))]  
}

df$churn<-as.factor(df$churn)
summary(df$eqpdays)
str(df$churn)
skewness(df$eqpdays,na.rm=T)
skewness(df$totrev,na.rm=T)

####1D
a<-table(df$churn)
a<-as.data.frame(a)
ggplot(a,aes(x=5,y=Freq,fill=Var1))+
  geom_col(aes(fill=Var1))+
  coord_polar(theta="y")+
  xlim(c(0.2,7))+
  labs(title='Donut Chart of Churn Level of Customers',
       x=NULL,y='Frequency')+
  theme(legend.position = 'right')+
  guides(fill=guide_legend(title='Churn'))


ggplot(df) +
  geom_density(aes(totmou,fill=churn,colour=churn),alpha=0.4)+
  xlim(0,25000)+
  labs(title='Density plot of Minutes of Usage',y='Density',x='Total Minutes of Usage')
  
df %>%
  filter(!(is.na(income))) %>%
  ggplot() +
  geom_bar(aes(as.factor(income),fill=as.factor(income)))+
  labs(title='Bar Diagram of Income Level',y='Frequency',x='Income Level')+
  guides(fill=guide_legend(title='Income Level'))+ylim(0,20000)

df %>%
  filter(!(is.na(totrev))) %>%
  ggplot() +
  geom_density(aes(totrev),fill='darkorchid1',colour='darkorchid3',alpha=0.6) + xlim(0,5000) +
  labs(title='Distribution of Total Revenue',x='Total Revenue')+mynamestheme

df %>%
  filter(!(is.na(totmrc_Mean))) %>%
  ggplot() +
  geom_density(aes(totmrc_Mean),fill='dodgerblue',colour='dodgerblue2',alpha=0.6)+ xlim(0,200)+
  labs(title='Density of Mean monthly recurring charge',x='Total Mean monthly recurring charge')

#2D
df %>%
  group_by(as.factor(churn)) %>%
  ggplot() + geom_freqpoly(aes(age1,group=as.factor(churn),colour=as.factor(churn)),bins=50,na.rm=TRUE)+xlim(18,120)+
  labs(title = "Frequency polygon of age 1", x = 'Age', y = 'Count',col="Churn")+mynamestheme

df %>%
  group_by(as.factor(churn)) %>%
  ggplot() + geom_freqpoly(aes(age2,group=as.factor(churn),colour=as.factor(churn)),bins=50,na.rm=TRUE)+xlim(18,120)+
  labs(title = " Frequency polygon of age 2",
     x = 'Age', y = 'Count',col="Churn")+mynamestheme

df %>%
  group_by(as.factor(churn)) %>%
  ggplot() +
  geom_freqpoly(aes(x=months,group=churn,colour=churn))+
  labs(title = " Frequency distribution of months in service", x = 'Months', y = 'Frequency')+mynamestheme

df%>%
  filter(ethnic!='')%>%
  ggplot() +
  geom_bar(aes(x=ethnic,fill=churn),na.rm=TRUE,position = 'fill') +
  labs(title = " Stacked Bar-Diagram of Ethnicity ",x='Ethnicity',y='Proportion')+mynamestheme

df%>%
  filter(ethnic!='')%>%
  ggplot() +
  geom_bar(aes(x=reorder(ethnic,churn==1),fill=churn),na.rm=TRUE,position = 'fill') +
  labs(title = "Stacked Bar-Diagram of Ethnicity ",x='Ethnicity',y='Proportion')+mynamestheme


df%>%
  filter(area!='')%>%
  ggplot() +
  geom_bar(aes(x=area,fill=churn),na.rm=TRUE,position='dodge') +
  labs(title = " Stacked Bar-Diagram by Area",xlab='Area',y='Frequency')+
  theme(axis.text.x = element_text(size=8,angle=80,vjust=0.5))+scale_fill_manual(values=c('darkolivegreen3', 'purple'))

df %>%
  filter(actvsubs>0) %>%
  ggplot() +
  geom_bar(aes(as.factor(actvsubs)),fill='skyblue') +
  ggtitle(' Bar diagram of number of active subscribers in household') + 
  xlab('No. of active subs in a household') + ylab('Count') +
  theme(plot.title = element_text(hjust=0.5,size=15)) +
  theme(plot.caption=element_text(size=16, hjust=0.5))+mynamestheme

ggplot(df, aes(asl_flag, fill = churn)) +
  geom_bar(position = "dodge")+
  labs(title = "Multiple Bar Diagram of Churn based on Account Spending Limit",x = 'Account spending limit',y='NUMBER OF CUSTOMERS')+mynamestheme

df %>%
  filter(car_buy!="") %>%
  ggplot(aes(car_buy, fill = churn)) +
  geom_bar(position = "dodge")+
  labs(title='Multiple Bar Plot of Churn vs Type of Car',x='Type of Car')+mynamestheme+scale_fill_manual(values=c('blue', 'springgreen'))

df %>%
  filter(dualband!="") %>%
  ggplot(aes(x=reorder(dualband,churn==1), fill = churn)) +
  geom_bar(position = "fill")+
  labs(title = "Stacked Bar Plot of Churn based on Dualband Type",x = 'Dualband Type',y='Proportion')+mynamestheme+scale_fill_manual(values=c('orange','cyan3'))

df %>%
  filter(dwllsize!="") %>%
  ggplot(aes(x=reorder(dwllsize,churn==1), fill = churn)) +
  geom_bar(position = "fill")+
  labs(title = "Stacked Bar Diagram of Dwelling Type vs Churn",x = 'Dwelling size',y='Proportion')+mynamestheme+scale_fill_manual(values=c('orange','green'))

df %>%
  filter(!(is.na(income))) %>%
  ggplot(aes(x=reorder(as.factor(income),churn==1), fill = churn)) +
  geom_bar(position = "fill")+
  labs(title = "Stacked bar plot for Customer Churn based on their Estimated Income",y='Proportion',x = 'Estimated income')+mynamestheme+scale_fill_manual(values=c('forestgreen','cyan'))

df %>%
  ggplot(aes(as.factor(new_cell), fill = churn)) +
  geom_bar(position = "dodge")+
  labs(title = " MULTIPLE BAR PLOT FOR CUSTOMERS CHURNED BASED ON PHONE USER",x = 'New cell phone user')+mynamestheme+scale_fill_manual(values=c('gold','cyan'))

df %>%
  filter(ownrent!="") %>%
  ggplot(aes(as.factor(ownrent), fill = churn)) +
  geom_bar(position = 'fill')+
  labs(title = " STACKED BAR PLOT FOR CUSTOMERS BASED ON HOUSING STATUS",x = 'Home owner/renter status',y='Proportion')+mynamestheme+scale_fill_manual(values=c('steelblue','purple'))

df %>%
  filter(!(is.na(phones))) %>%
  ggplot(aes(as.factor(phones), fill = churn)) +
  geom_bar(position = "fill")+
  labs(title = " STACKED BAR PLOT FOR CUSTOMERS BASED ON THEIR NUMBER OF HANDSET", x = 'Number of handsets issued')+mynamestheme+scale_fill_manual(values=c('steelblue','magenta'))

ggplot(df)+
  geom_point(aes(rev_Mean,roam_Mean),colour="steelblue",alpha=0.5)+
  geom_rug(aes(rev_Mean,roam_Mean),colour="steelblue",sides='bl')+
  xlim(0,1500)+ylim(0,1500)+mynamestheme+labs(title='Rug plot between mean monthly revenue vs mean number of roaming calls',x='Mean monthly revenue',y='Mean number of roaming calls')

df %>%
  filter(!(is.na(models))) %>%
  ggplot(aes(as.factor(models), fill = churn)) +
  geom_bar(position = "fill")+
  labs(title = " STACKED BAR PLOT FOR CUSTOMERS BASED ON ISSUED MUDELS COUNT", x = 'Number of models issued',y='Proportion')+mynamestheme+scale_fill_manual(values=c('cyan','green'))

df %>%
  filter(prizm_social_one!="") %>%
  ggplot(aes(prizm_social_one, fill = churn)) +
  geom_bar(position = "fill")+
  labs(title = " STACKED BAR PLOT FOR CUSTOMERS BASED ON SOCIAL GROUP ",x = 'Social group ',y='Proportion')+mynamestheme+scale_fill_manual(values=c('cyan','olivedrab'))

df %>%
  filter(refurb_new!="") %>%
  ggplot(aes(refurb_new, fill = churn)) +
  geom_bar(position = "fill")+
  labs(title = " STACKED BAR PLOT FOR CUSTOMERS BASED ON PHONE'S CONDITION",x = 'Handset: refurbished or new',y='Proportion')+mynamestheme+scale_fill_manual(values=c('cyan','olivedrab'))
  
ggplot(df,aes(uniqsubs, fill = churn)) +
  geom_bar(position = "fill",na.rm = T) +
  xlim(0,20)+
  labs(title = " STACKED BAR PLOT FOR CUSTOMERS BASED ON UNIQUE SUBSCRIBERS", 
       x = 'Number of unique subscribers in the household', y='Proportion')+mynamestheme+scale_fill_manual(values=c('cyan','olivedrab'))

df %>%
  filter(creditcd!="") %>%
  ggplot(aes(creditcd, fill = churn)) +
  geom_bar(position = "fill")+
  labs(title = " STACKED BAR PLOT FOR CUSTOMERS BASED ON CREDIT CARD", x = 'Credit card indicator',y='Proportion')+mynamestheme+scale_fill_manual(values=c('cyan','olivedrab'))

df %>%
  filter(crclscod!="") %>%
  ggplot(aes(reorder(crclscod,churn==1), fill = churn)) +
  geom_bar(position = "fill")+
  labs(title = " STACKED BAR PLOT FOR CUSTOMERS BASED ON CREDIT-CLASS", x = 'Credit class code',y='Proportion')+mynamestheme+scale_fill_manual(values=c('cyan','olivedrab'))

ggplot(df, mapping=aes(churn, y=drop_dat_Mean,color=churn))+
  geom_boxplot(na.rm=TRUE)+scale_y_continuous(trans = "log10")+
  labs(title = " Boxplot of dropped data calls vs churn", x = 'Churn', y = 'Mean number of dropped data calls')+mynamestheme+scale_color_manual(values=c('red','green'))

ggplot(df, mapping=aes(churn, y=totmrc_Mean,color=churn))+
  geom_boxplot(na.rm=TRUE)+
  scale_y_continuous(trans = "log10")+
  labs(title = " Boxplot of Mean monthly recurring charge", x = 'Churn', y = 'Mean of total monthly recurring charge')+mynamestheme+scale_color_manual(values=c('steelblue','orange'))

ggplot(df, mapping=aes(churn, eqpdays,color=churn))+
  geom_violin(na.rm=TRUE)+
  stat_summary(fun='mean',geom = 'crossbar',width=0.6,aes(color='mean'))+
  stat_summary(fun='median',geom = 'crossbar',width=0.6,aes(color='median'))+
  labs(title = " Violin plot for age of current equiment",x = 'Churn', y = 'Age of current equipment(in days)')+mynamestheme+scale_color_manual(values=c('steelblue','orange','green','red'))


levels(df$csa)
#Creating a new column with state codes from CSA column
df$csanew <- substr(df$csa,1,3)
levels(as.factor(df$csanew))
df %>%
  filter(csanew!="") %>%
  ggplot(aes(reorder(csanew,churn==1), fill = churn)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle=90))+
  labs(title=' Barplot of state code',x='State Code')+mynamestheme

ggplot(df, mapping=aes(churn, adjmou,color=churn))+
  geom_boxplot(na.rm=TRUE)+scale_y_log10()+
  labs(title=' Boxplot of Churn vs Billing adjusted total minutes of use',x='Churn',y='Total minutes used over the life of the customer')+mynamestheme+scale_color_manual(values=c('steelblue','orange'))

ggplot(df, mapping=aes(churn, drop_blk_Mean,color=churn))+
  geom_boxplot(na.rm=TRUE)+scale_y_log10()+
  labs(title=' Boxplot of Churn vs Average Droppped Calls',x='churn',y='Mean number of blocked or dropped calls')+mynamestheme+scale_color_manual(values=c('steelblue','orange'))

ggplot(df, mapping=aes(churn, ovrrev_Range,color=churn))+
  geom_boxplot(na.rm=TRUE)+
  labs(title = " Boxplot of Range of overage revenue", y = 'Range of overage revenue')+scale_y_log10()+mynamestheme+scale_color_manual(values=c('green','orange'))

#Creating a categorical column from total revenue column
df$totrevnew <- cut(df$totrev,breaks=quantile(df$totrev, probs = seq(0,1,0.25)),labels=c("Low",'Medium',"High","Premium"), include.lowest = T)
df %>%
  ggplot(aes(x=totrevnew, fill = churn)) +
  geom_bar(position = "fill")+
  labs(title=' Bar diagram of Total Revenue Category',x='Total Revenue OF Customer Level',y='Proportion of customers')+mynamestheme+scale_color_manual(values=c('green','orange'))

cor(df$totrev,df$totmrc_Mean,use="pairwise.complete.obs")
ggplot(df,mapping = aes(months,totrev))+geom_smooth()+
  labs(title=' Smooth scatter plot of Total Revenue vs Months',x="Months",y="Total Revenue")+mynamestheme

ggplot(df,mapping = aes(totrev,mou_Mean))+geom_smooth(na.rm=T)+
  labs(title = ' Smooth scatter plot of Total Revenue vs Mean monthly use duration',x="Total Revenue",y='Mean number of monthly minutes  use')+mynamestheme

#3D
df %>% 
  filter(area!=''&!(is.na(income))) %>%
  ggplot(aes(x=income,fill=area)) + 
  geom_bar(na.rm=T) + 
  facet_wrap(~area) + 
  labs(x = "Income",title = ' Distribution of income vs area') + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(),vjust=-0.5,size = 2)+
  theme(legend.position = "none")+mynamestheme

ggplot(df, mapping=aes(churn, retdays,color=churn))+
  geom_boxplot(na.rm=TRUE)+scale_y_continuous(trans = "log10")+
  labs(title=' Number of days from last retention call vs churn',y='Number of days since last retention call')+mynamestheme+scale_color_manual(values=c('blue','orange'))

df$tot_ret[is.na(df$tot_ret)]<-0
df %>%
  filter(!(is.na(tot_ret))) %>%
  ggplot(aes(x=as.factor(tot_ret), fill = churn)) +
  geom_bar(position = "fill")+
  labs(title=' Bar plot of Total calls into retention team',x='Number of calls',y='Proportion of customers')+
  mynamestheme+scale_color_manual(values=c('blue','orange'))

#Replacing missing values by the column mean
df$eqpdays[is.na(df$eqpdays)]<-mean(df$eqpdays,na.rm=T)
ggplot(df, mapping=aes(churn, eqpdays))+
  geom_violin(aes(colour=churn),alpha=0.5)+
  stat_summary(fun='mean',geom = 'crossbar',width=0.6,aes(color='mean'))+
  stat_summary(fun='median',geom = 'crossbar',width=0.6,aes(color='median'))+
  labs(title = " Violin plot of age of current equiment",x = 'Churn', y = 'Age of current equipment(in days)')+mynamestheme+scale_color_manual(values=c('blue','orange','green','gold'))

df$actvsubs_new<-ifelse(df$actvsubs<2,"1","more than 1")
df %>%
  ggplot(aes(as.factor(actvsubs_new), fill = churn)) +
  geom_bar(position = 'dodge')+
  labs(title=" Bar diagram of active subscribers",x='Count of Active Subscribers')+mynamestheme+scale_color_manual(values=c('blue','orange'))
#Replacing missing values by mode of categorical column
df$ownrent[df$ownrent==""] <- calmode(df$ownrent)
df %>%
  ggplot(aes(as.factor(ownrent), fill = churn)) +
  geom_bar(position = "fill")+
  labs(title=' Bar Diagram of Housing Status',x='Owner or Renter Status')+mynamestheme

df <- df %>%
  mutate(occu1_cat=ifelse(occu1==""|occu1=="Z",occu1,ifelse(occu1 %in% LETTERS[3:12],"self","sal")))
table(df$occu1_cat)
df$occu1_cat<-as.factor(df$occu1_cat)
levels(df$occu1_cat) <- c("Unknown","Other","Salaried","Self-Employed")
df %>%
  filter(dualband!="") %>%
  ggplot()+
  geom_point(aes(occu1_cat,dualband))+
  geom_count(aes(occu1_cat,dualband,size=after_stat(prop),colour=after_stat(prop),group=1))+
  scale_size_area(max_size = 20)+mynamestheme+labs(title='Scatter Plot Matrix between occupation and dualband', x='Occupation',y='Dualband')

df$age1[df$age1==0]<-mean(df$age1,na.rm=T)
df$age1_cat <- cut(df$age1,breaks=5)
levels(df$age1_cat)<-c("A","B","C","D","E")
df %>%
  filter(!(is.na(age1_cat))) %>%
  ggplot(aes(rev_Mean,age1_cat,fill=age1_cat)) +
  geom_density_ridges(alpha=0.5)+
  theme()+
  xlim(0,100)+mynamestheme+labs(title='Ridge plot Mean Monthly Revenue VS  Age',x='Mean monthly revenue',y='Age')+
  guides(fill=guide_legend(title='Age Category'))

a<-table(df$marital)
a<-as.data.frame(a)
a<-a[a$Var1!="",]
a
ggplot(a,aes(x=5,y=Freq,fill=Var1))+
  geom_col(aes(fill=Var1),alpha=0.7)+
  coord_polar(theta="y")+
  xlim(c(0.2,7))+labs(title='Polar co-ordinate plot of Marital status count',x=NULL,y='Count of customers')+
  guides(fill=guide_legend(title='Marital Status'))


df %>%
  filter(!(cartype=='')) %>%
  ggplot() +
  geom_bar(aes(cartype,fill=cartype))+coord_polar()+
  labs(title='Co-ordinate bar-plot for Count of customers based upon cartype',y='Customer count',x='')+
  guides(fill=guide_legend(title='Type of Car'))

ggplot(df)+
  geom_density(aes(comp_vce_Mean),fill="thistle3",colour="thistle4",alpha=0.5)+xlim(0,500)+mynamestheme+labs(x='Mean number of completed voice calls',y='Density',title='Density plot for Mean number of completed voice calls')

ggplot(df)+
  geom_density(aes(eqpdays),fill="plum1",colour="plum4",alpha=0.6)+xlim(0,1000)+mynamestheme+labs(title='Density plot for Number of days (age) of current equipment',x='Number of days (age) of current equipment',y='Density ')

df %>%
  filter(!(is.na(asl_flag))) %>%
  ggplot(aes(x=rev_Mean,y=mou_Mean,colour=as.factor(asl_flag))) + 
  geom_point(alpha=0.5) +xlim(0,500)+ylim(0,4000)+theme()+mynamestheme+labs(title='Scatter plot',x='Mean monthly revenue',y=' Mean number of monthly minutes of use',col='Account spending limit')

df %>%
  filter(!(is.na(income))) %>%
  ggplot(aes(x=as.factor(income),y=rev_Mean,fill=income)) + 
  geom_boxplot(notch=T) +
  ylim(0,100)+labs(title='Box plot Income level vs Mean monthly revenue',x='Income level',y='Mean monthly revenue',col='Income level')

df %>%
  filter(!(dualband=="")) %>%
  ggplot(aes(x=comp_vce_Mean,y=reorder(dualband, comp_vce_Mean, FUN=median),colour=dualband,fill=dualband)) + 
  geom_violin(draw_quantiles=c(0.5),alpha=0.5)+
  scale_x_log10()+mynamestheme+labs(title='Violin plot for Mean number of completed voice calls',y='Dualbands',x='Mean number of completed voice calls')

df %>%
  filter(age2!=0)%>%
  ggplot()+
  geom_jitter(aes(age1,age2),colour='slateblue1',alpha=0.1)+
  labs(title='Jitter plot for age of first and second household member',x='Age of first household member',y='Age of second household member')

qqnorm(df$eqpdays,pch=1,frame=FALSE,col="mediumpurple2")
qqline(df$eqpdays,col='steelblue',lwd=2)

df %>%
  filter(!(proptype=="")) %>%
  ggplot()+
  geom_boxplot(aes(y=reorder(proptype, peak_vce_Mean, FUN=median),x=peak_vce_Mean,colour=proptype))+
  scale_x_log10()+mynamestheme+labs(title='Boxplot for Property type vs Mean number of peak voice calls',x='Mean number of inbound and outbound peak voice calls',y='Property type',col='Property type')

df %>%
  filter(cartype != "" & marital != "") %>%
  ggplot()+
  geom_mosaic(aes(x=product(marital,cartype),fill=marital))+mynamestheme+labs(title='Mosaic plot for Marital status vs car type',y='Marital status',x='Car Type',col='Marital status')


df %>%
  filter(dualband!="") %>%
  ggplot(aes(x=dualband, y=drop_vce_Mean)) +
  geom_point(col="turquoise", size=3) +
  geom_segment(aes(x=dualband,xend=dualband,y=min(drop_vce_Mean),yend=max(drop_vce_Mean)),size=0.05) +
  labs(title="Dualband Vs Drop Calls") +  
  coord_flip()+
  xlab('Dropped Voice Calls')+
  ylab('Dualband')

df$income_new <- as.factor(df$income)
df <- df %>%
  mutate(income_new=ifelse(is.na(income),income,ifelse(income %in% c(1:3),"Low",ifelse(income %in% c(4:6),"Middle","Upper"))))
mo <- calmode(df$income_new[!is.na(df$income_new)]);mo
df <- df %>%
  mutate(income_new=ifelse(is.na(income_new),mo,income_new))

df %>%
  ggplot(aes(y=ovrrev_Mean,x=complete_Mean,colour=churn))+
  geom_point(alpha=0.5)+
  ylim(0,600)+
  facet_wrap(~income_new)+
  labs(title='Scatteplot of Completed Calls and Overage Revenue',x="Mean number of Completed Calls",y="Mean Overage Revenue")

df %>%
  ggplot(aes(rev_Mean,income_new,fill=income_new,colour=income_new)) +
  geom_density_ridges(alpha=0.5,quantile_lines=T)+
  theme_ridges()+
  labs(title='Ridgeplot of Mean Revenue and Income',x="Revenue",y="Income")+
  xlim(0,100)

#Removing columns with more than 50% missing values
f1 <- function(x) {
  sum(is.na(x)|x=="")<50000
}
m <- sapply(df,f1)
df <- subset(df, select = names(df)[m])

table(df$churn,df$income)
table(df$educ1,df$cartype)
