# Preprocessing of data for Demo Dashboard
library(ggplot2)

# Load raw data
setwd("~/git/DashboardDemoWithR")
rd1 <- read.csv2('RawData/tageswerte_EB_00403_akt/produkt_erdbo_tag_20161112_20180515_00403.txt',as.is=TRUE) # Berlin
rd2 <- read.csv2('RawData/tageswerte_EB_01981_akt/produkt_erdbo_tag_20161112_20180515_01981.txt',as.is=TRUE) # Hamburg
rd3 <- read.csv2('RawData/tageswerte_EB_03379_akt/produkt_erdbo_tag_20161112_20180515_03379.txt',as.is=TRUE) # München

# Inspect data
head(rd1)
head(rd2)
head(rd3)
tail(rd1)
tail(rd2)
tail(rd3)
summary(rd1)
summary(rd2)
summary(rd3)
sum(!complete.cases(rd1))
sum(!complete.cases(rd2))
sum(!complete.cases(rd3))

# Convert MESSDATUM into Date format
short_to_date <- function(x){
  # No format checking, use nchar if needed, requires always 10 chars
  xyear <- sapply(x,substr,start=0,stop=4)
  xmonth <- sapply(x,substr,start=5,stop=6)
  xday <- sapply(x,substr,start=7,stop=10)
  xdate <- as.Date(paste(xyear,xmonth,xday,sep='-'))
  # return value
  xdate
}
rd1$DATUM <- short_to_date(rd1$MESS_DATUM)
rd2$DATUM <- short_to_date(rd2$MESS_DATUM)
rd3$DATUM <- short_to_date(rd3$MESS_DATUM)

# Convert to numeric
rd1$cm_05 <- as.numeric(rd1$V_TE005M)
rd2$cm_05 <- as.numeric(rd2$V_TE005M)
rd3$cm_05 <- as.numeric(rd3$V_TE005M)
rd1$cm_10 <- as.numeric(rd1$V_TE010M)
rd2$cm_10 <- as.numeric(rd2$V_TE010M)
rd3$cm_10 <- as.numeric(rd3$V_TE010M)
rd1$cm_20 <- as.numeric(rd1$V_TE020M)
rd2$cm_20 <- as.numeric(rd2$V_TE020M)
rd3$cm_20 <- as.numeric(rd3$V_TE020M)
rd1$cm_50 <- as.numeric(rd1$V_TE050M)
rd2$cm_50 <- as.numeric(rd2$V_TE050M)
rd3$cm_50 <- as.numeric(rd3$V_TE050M)

# Example plots 
color_labels <- c('05 cm','10 cm','20 cm','50 cm')
ggplot()+geom_point(aes(rd1$DATUM,rd1$cm_05,color=color_labels[1]))+
  geom_point(aes(rd1$DATUM,rd1$cm_10,color=color_labels[2]))+
  geom_point(aes(rd1$DATUM,rd1$cm_20,color=color_labels[3]))+
  geom_point(aes(rd1$DATUM,rd1$cm_50,color=color_labels[4]))+
  scale_x_date(date_breaks = '2 weeks')+
  xlab('Datum [yyyy-mm-dd]')+ylab('Temperatur [°C]')+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))+
  guides(color=guide_legend(title=NULL))
ggplot()+geom_point(aes(rd1$DATUM,rd1$cm_05,color='cm_05'))+
  geom_point(aes(rd1$DATUM,rd1$cm_10,color='cm_10'))+
  geom_point(aes(rd1$DATUM,rd1$cm_20,color='cm_20'))+
  geom_point(aes(rd1$DATUM,rd1$cm_50,color='cm_50'))+
  scale_x_date(date_breaks = '2 weeks')+
  xlab('Datum [yyyy-mm-dd]')+ylab('Temperatur [°C]')+
  theme(#axis.title.x = element_text(face="bold", size=15),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=10))

# Rename and save data 
full_data_berlin <- rd1
full_data_hamburg <- rd2
full_data_munich <- rd3
full_data_berlin$STATION_NAME <- 'Berlin'
full_data_hamburg$STATION_NAME <- 'Hamburg'
full_data_munich$STATION_NAME <- 'Munich'
columns_to_keep <- c('STATION_NAME','DATUM','cm_05','cm_10','cm_20','cm_50')
data_berlin <- full_data_berlin[,columns_to_keep]
data_hamburg <- full_data_hamburg[,columns_to_keep]
data_munich <- full_data_munich[,columns_to_keep]
write.csv(data_berlin,file='ProcessedData/data_berlin.csv',row.names = F)
write.csv(data_hamburg,file='ProcessedData/data_hamburg.csv',row.names = F)
write.csv(data_munich,file='ProcessedData/data_munich.csv',row.names = F)

# More example plots
city_labels <- c('Berlin','Hamburg','Munich')
ggplot()+geom_point(aes(data_berlin$DATUM,data_berlin$cm_50,color=city_labels[1]))+
  geom_point(aes(data_hamburg$DATUM,data_hamburg$cm_50,color=city_labels[2]))+
  geom_point(aes(data_munich$DATUM,data_munich$cm_50,color=city_labels[3]))+
  geom_point(aes(data_munich$DATUM,data_munich$cm_50-data_hamburg$cm_50,color='X: Delta Munich Hamburg'))+
  geom_point(aes(data_munich$DATUM,data_munich$cm_50-data_berlin$cm_50,color='Y: Delta Munich Berlin'))+
  geom_point(aes(data_munich$DATUM,data_berlin$cm_50-data_hamburg$cm_50,color='Z: Delta Berlin Hamburg'))+
  scale_x_date(date_breaks = '2 weeks')+
  xlab('Datum [yyyy-mm-dd]')+ylab('Temperatur [°C]')+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))+
  guides(color=guide_legend(title=NULL))
ggplot()+geom_smooth(aes(data_berlin$DATUM,data_berlin$cm_50,color=city_labels[1]),se=F,span=0.2)+
  geom_smooth(aes(data_hamburg$DATUM,data_hamburg$cm_50,color=city_labels[2]),se=F,span=0.2)+
  geom_smooth(aes(data_munich$DATUM,data_munich$cm_50,color=city_labels[3]),se=F,span=0.2)+
  geom_smooth(aes(data_munich$DATUM,data_munich$cm_50-data_hamburg$cm_50,color='X: Delta Munich Hamburg'),se=F,span=0.2)+
  geom_smooth(aes(data_munich$DATUM,data_munich$cm_50-data_berlin$cm_50,color='Y: Delta Munich Berlin'),se=F,span=0.2)+
  geom_smooth(aes(data_munich$DATUM,data_berlin$cm_50-data_hamburg$cm_50,color='Z: Delta Berlin Hamburg'),se=F,span=0.2)+
  scale_x_date(date_breaks = '2 weeks')+
  xlab('Datum [yyyy-mm-dd]')+ylab('Temperatur [°C]')+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))+
  guides(color=guide_legend(title=NULL))+
  ggtitle('Temperature Comparison 50 cm above Ground')

ggplot()+geom_smooth(aes(data_berlin$DATUM,data_berlin$cm_10,color=city_labels[1]),se=F,span=0.2)+
  geom_smooth(aes(data_hamburg$DATUM,data_hamburg$cm_10,color=city_labels[2]),se=F,span=0.2)+
  geom_smooth(aes(data_munich$DATUM,data_munich$cm_10,color=city_labels[3]),se=F,span=0.2)+
  geom_smooth(aes(data_munich$DATUM,data_munich$cm_10-data_hamburg$cm_10,color='X: Delta Munich Hamburg'),se=F,span=0.2)+
  geom_smooth(aes(data_munich$DATUM,data_munich$cm_10-data_berlin$cm_10,color='Y: Delta Munich Berlin'),se=F,span=0.2)+
  geom_smooth(aes(data_munich$DATUM,data_berlin$cm_10-data_hamburg$cm_10,color='Z: Delta Berlin Hamburg'),se=F,span=0.2)+
  scale_x_date(date_breaks = '2 weeks')+
  xlab('Datum [yyyy-mm-dd]')+ylab('Temperatur [°C]')+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))+
  guides(color=guide_legend(title=NULL))+
  ggtitle('Temperature Comparison 10 cm above Ground')
