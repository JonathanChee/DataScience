# Install and use ggplot2
# install.packages("ggplot2")
library(ggplot2)

# Install and use gridExtra
# install.packages("gridExtra")
library(gridExtra)

# Install and use lubridate
# install.packages("lubridate")
library(lubridate)

# Install and use dplyr
# install.packages("dplyr")
library(dplyr)

#Install and use corrplot
#install.packages("corrplot")
library(corrplot)

#Install and use ggplot2 and Rcpp
#install.packages("ggplot2")
#install.packages("Rcpp")
library(Rcpp)
library(ggplot2)

#Install and use stringr
#install.packages("stringr")
library(stringr)


setwd("C:\\Jon\\Course\\TP\\SwDevAppliedAI\\5 DataScienceEssential\\Assignment")
realEstate<-read.csv("REALIS_Oct17toOct20.csv")

#names(realEstate)
colnames(realEstate)[2]<-"Transacted.Price"
colnames(realEstate)[3]<-"Area.SQFT"
colnames(realEstate)[4]<-"Unit.Price.PSF"
colnames(realEstate)[10]<-"Unit.Price.PSM"
colnames(realEstate)[9]<-"Area.SQM"
colnames(realEstate)[11]<-"Nett.Price"

#remove not useful column
realEstate <- realEstate[, -c(11, 15)]
names(realEstate)

#Remove comma and change to numeric
realEstate$Transacted.Price<-as.numeric(gsub(",", "", realEstate$Transacted.Price))
realEstate$Area.SQFT<-as.numeric(gsub(",", "", realEstate$Area.SQFT))
realEstate$Area.SQM<-as.numeric(gsub(",", "", realEstate$Area.SQM))
realEstate$Unit.Price.PSF<-as.numeric(gsub(",", "", realEstate$Unit.Price.PSF))
realEstate$Unit.Price.PSM<-as.numeric(gsub(",", "", realEstate$Unit.Price.PSM))
realEstate$Number.of.Units<-as.numeric(realEstate$Number.of.Units)

# Change attribute from character type to Factor type
realEstate$Planning.Region <- as.factor(gsub(" Region", "", realEstate$Planning.Region))
realEstate$Type.of.Sale <- factor(realEstate$Type.of.Sale)
realEstate$Type.of.Area <- factor(realEstate$Type.of.Area)
realEstate$Property.Type <- factor(realEstate$Property.Type)
realEstate$Postal.District <- factor(realEstate$Postal.District)
realEstate$Postal.Sector <- factor(realEstate$Postal.Sector)

realEstate$Postal.Code <- as.character(realEstate$Postal.Code)
#Convert NA for urchaser.Address.Indicator to Others
realEstate$Purchaser.Address.Indicator <- gsub("N.A", "Others", realEstate$Purchaser.Address.Indicator)
realEstate$Purchaser.Address.Indicator <- factor(realEstate$Purchaser.Address.Indicator)

#Date Transformation 
#Change attribute from character type to Date type
realEstate$Sale.Date <- as.Date(realEstate$Sale.Date, "%d-%b-%y")
# Create character attribute for Sale.Mth and factorize it
realEstate$Sale.Mth <- format(realEstate$Sale.Date,"%b")
realEstate$Sale.Mth = factor(realEstate$Sale.Mth, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
#Create a Year column to indicate the year only
realEstate$Year.of.Sale <- as.integer(format(realEstate$Sale.Date,"%Y"))

# Change month to quarter
realEstate$Sale.Quarter <- gsub("Jan", "Quarter-1", realEstate$Sale.Mth)
realEstate$Sale.Quarter <- gsub("Feb", "Quarter-1", realEstate$Sale.Mth)
realEstate$Sale.Quarter <- gsub("Mar", "Quarter-1", realEstate$Sale.Mth)
realEstate$Sale.Quarter <- gsub("Apr", "Quarter-2", realEstate$Sale.Mth)
realEstate$Sale.Quarter <- gsub("May", "Quarter-2", realEstate$Sale.Mth)
realEstate$Sale.Quarter <- gsub("Jun", "Quarter-2", realEstate$Sale.Mth)
realEstate$Sale.Quarter <- gsub("Jul", "Quarter-3", realEstate$Sale.Mth)
realEstate$Sale.Quarter <- gsub("Aug", "Quarter-3", realEstate$Sale.Mth)
realEstate$Sale.Quarter <- gsub("Sep", "Quarter-3", realEstate$Sale.Mth)
realEstate$Sale.Quarter <- gsub("Oct", "Quarter-4", realEstate$Sale.Mth)
realEstate$Sale.Quarter <- gsub("Nov", "Quarter-4", realEstate$Sale.Mth)
realEstate$Sale.Quarter <- gsub("Dec", "Quarter-4", realEstate$Sale.Mth)
realEstate$Sale.Quarter <- factor(realEstate$Sale.Quarter)
names(realEstate)


#---------------------------Part 2a (Data Preparation)----------------------------------------
#Transform and create some useful column attributes
#Extract Storey from Address column
realEstate$Storey <- str_extract(realEstate$Address,regex("[#][:digit:][:digit:]"))
realEstate$Storey[is.na(realEstate$Storey)] = "#0"
realEstate$Storey = as.numeric(gsub("#", "", realEstate$Storey))

#Convert those 999999 and 9999 to 999
realEstate$Tenure <- gsub("999999 yrs", "999 yrs", realEstate$Tenure)
realEstate$Tenure <- gsub("9999 yrs", "999 yrs", realEstate$Tenure)

tenure.start <- str_extract(realEstate$Tenure,regex("[:digit:][:digit:][/][:digit:][:digit:][/][:digit:][:digit:][:digit:][:digit:]"))
tenure.start <- gsub("/","-", tenure.start)
tenure.start <- as.Date(tenure.start, "%d-%m-%Y")
tenure.start.year <- as.integer(format(tenure.start,"%Y"))

tenure.duration <- as.integer(str_extract(realEstate$Tenure,regex("[:digit:][:digit:]+")))

tenure.end <-ymd(tenure.start)+years(tenure.duration)
tenure.end <- as.Date(tenure.end, "%d-%m-%Y")
tenure.end.year <- as.integer(format(tenure.end,"%Y"))

remaining.lease.year <- as.integer(tenure.end.year - (as.integer(format(today(),"%Y"))))

#Convert those NA result from Freehold status to 9999
remaining.lease.year[is.na(remaining.lease.year)] = 9999

#Create a column call Lease.Length
realEstate$Lease.Length <- remaining.lease.year
realEstate <- realEstate %>% mutate(Lease.Length= ifelse(Lease.Length < 30, "Very Short",
                                                         ifelse(Lease.Length >= 30 & Lease.Length < 60, "Short",
                                                                ifelse(Lease.Length >= 60 & Lease.Length < 110, "Long",
                                                                       ifelse(Lease.Length >= 110 & Lease.Length < 1000, "Very Long", "Freehold")))))

realEstate$Lease.Length <- factor(realEstate$Lease.Length)

#-----------------------------------------------------------------------------


# Calculate statistics value for Unit.Price.PSF
medianUnitPricePSF <- median(realEstate$Unit.Price.PSF)
sdUnitPricePSF <- as.integer(sd(realEstate$Unit.Price.PSF))


# Takeaway 1 : Did the unit price (PSF) impact the number of transaction (see histogram)?
tky1 <- ggplot(realEstate, aes(x = Unit.Price.PSF))
tky1 <- tky1 + geom_histogram(binwidth = 200, color="green", fill="lightblue")
tky1 <- tky1 + ggtitle("Transaction Volume by Unit Price (PSF) - (histogram:binwidth = 200)")
tky1 <- tky1 + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
tky1 <- tky1 + labs(x = "Unit Price (PSF)", y = "Number of Transactions")
tky1 <- tky1 + theme(axis.title = element_text(color = "brown", size = 20, face = "bold"))
tky1 <- tky1 + theme(axis.text.x = element_text(color = "brown", size = 20, face = "bold", angle = 0, vjust = 0.0))
tky1 <- tky1 + theme(axis.text.y = element_text(color = "brown", size = 20, face = "bold"))

tky1 <- tky1 + geom_vline(aes(xintercept = medianUnitPricePSF), color = "brown", linetype = "longdash", size = 1.5)
tky1 <- tky1 + geom_vline(aes(xintercept = medianUnitPricePSF - sdUnitPricePSF), color = "orange", linetype = "dotted", size = 1.5)
tky1 <- tky1 + geom_vline(aes(xintercept = medianUnitPricePSF + sdUnitPricePSF), color = "orange", linetype = "dotted", size = 1.5)
tky1 <- tky1 + geom_hline(aes(yintercept = 11700), color = "blue", linetype = "longdash", size = 1)

tky1 <- tky1 + geom_text(aes(x = 2500, y = 9000, label = paste("Median = ", medianUnitPricePSF)), colour="chocolate", hjust = -0.1, size = 6)
tky1 <- tky1 + geom_text(aes(x = 2500, y = 10000, label = paste("Std Dev = ", sdUnitPricePSF)), colour="darkorange2", hjust = -0.1, size = 6)
tky1 <- tky1 + geom_text(aes(x = 5500, y = 11720, label = paste("11700")), colour="blue", vjust = -0.2, size = 6)
tky1 <- tky1 + geom_text(aes(x = medianUnitPricePSF - sdUnitPricePSF, y = 0, label = paste(medianUnitPricePSF - sdUnitPricePSF)), colour="darkorange2", vjust = 1.0, size = 5)
tky1 <- tky1 + geom_text(aes(x = medianUnitPricePSF + sdUnitPricePSF, y = 0, label = paste(medianUnitPricePSF + sdUnitPricePSF)), colour="darkorange2", vjust = 1.0, size = 5)
tky1


# Takeaway 2 : Is there certain quarter prefer by property owner to have a transaction done?
tbl2 <- realEstate %>% select(Sale.Quarter, Number.of.Units)
tbl2 <- tbl2 %>% group_by(Sale.Quarter) %>% summarise(transactionVolume = sum(Number.of.Units))

tky2 <- ggplot(tbl2, aes(x = Sale.Quarter, y = transactionVolume))
tky2 <- tky2 + geom_bar(stat="identity", color="slategrey", fill="deepskyblue2", width = 0.75, position = "dodge")
tky2 <- tky2 + ggtitle("Transaction Volume by Quarter for the past 3-years")
tky2 <- tky2 + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))
tky2 <- tky2 + labs(x = "", y = "Transaction Volume")
tky2 <- tky2 + theme(axis.title = element_text(color = "brown", size = 20, face = "bold"))
tky2 <- tky2 + theme(axis.text.x = element_text(color = "brown", size = 20, face = "bold"))
tky2 <- tky2 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
tky2 <- tky2 + geom_text(aes(label = transactionVolume), position = position_dodge(width = 0.9), vjust = -0.25, size = 6)
tky2


# Takeaway 3 : Does higher floor property has any impact on the Unit.Price.PSF?
tbl3 <- realEstate %>% select(Storey, Unit.Price.PSF)
tbl3 <- tbl3 %>% group_by(Storey) %>% summarise(meanUnitPricePSF = mean(Unit.Price.PSF))

tky3 <- ggplot(tbl3, aes(x = Storey, y = meanUnitPricePSF))
tky3 <- tky3 + geom_bar(stat="identity", color="slategrey", fill="deepskyblue2", width = 0.75, position = "dodge")
tky3 <- tky3 + ggtitle("Average Unit Price (PSF) vs Floor Level")
tky3 <- tky3 + theme(plot.title = element_text(size = 20, face = "bold.italic", hjust = 0.5))
tky3 <- tky3 + labs(x = "Floor Level", y = "Average Unit Price (PSF)")
tky3 <- tky3 + theme(axis.title = element_text(color = "brown", size = 16, face = "bold"))
tky3 <- tky3 + theme(axis.text.x = element_text(color = "brown", size = 16, face = "bold"))
tky3 <- tky3 + theme(axis.text.y = element_text(color = "brown", size = 16, face = "bold"))
tky3



# Takeaway 4 : Any impact of average unit price of condo/exec condo for past 3-years across the region
realEstateCondo <- realEstate %>% filter(Property.Type=='Condominium' | Property.Type=='Executive Condominium')
tbl4 <- realEstateCondo %>% select(Planning.Region, Year.of.Sale, Unit.Price.PSF)
tbl4 <- tbl4 %>% filter(Year.of.Sale != 2017)
tbl4 <- tbl4 %>% group_by(Planning.Region, Year.of.Sale) %>% summarise(Median.Unit.Price.PSF = median(Unit.Price.PSF))
tbl4$Year.of.Sale<-factor(tbl4$Year.of.Sale)

tky4 <- ggplot(tbl4, aes(x = Planning.Region, y = Median.Unit.Price.PSF, fill = Year.of.Sale))
tky4 <- tky4 + geom_col(width = 0.8, position = "dodge", color="slategrey")
tky4 <- tky4 + ggtitle("Average Unit Price (PSF) of Condo/EC across the Planning Region")
tky4 <- tky4 + theme(plot.title = element_text(color = "black", size = 20, face = "bold.italic", hjust = 0.5))
tky4 <- tky4 + labs(x = "Region", y = "Median Unit Price ($ PSF)")
tky4 <- tky4 + theme(axis.title = element_text(color = "brown", size = 20, face = "bold"))
tky4 <- tky4 + theme(axis.text.x = element_text(color = "brown", size = 20, face = "bold"))
tky4 <- tky4 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
tky4 <- tky4 + geom_text(aes(label = round(Median.Unit.Price.PSF, 0)), position = position_dodge(width = 0.8), angle = 90, vjust = 0.0, hjust = -0.1, size = 6)
tky4 <- tky4 + guides(fill=guide_legend(title="Year of Sale"))
tky4 <- tky4 + theme(legend.title = element_text(size = 20, face = "bold"))
tky4 <- tky4 + ylim(0, (1.1 * max(tbl4$Median.Unit.Price.PSF)))
tky4

#Takeaway5 : Box-plot shows Unit.Price.PSF by Property.Type with Type.of.Sales
tbl5 <- realEstateCondo %>% select(Planning.Region, Type.of.Sale, Unit.Price.PSF)
tbl5 <- tbl5 %>% filter(Type.of.Sale != 'Sub Sale')
graph5 <- ggplot(tbl5, aes(Planning.Region, Unit.Price.PSF, fill=Type.of.Sale)) + geom_boxplot() + theme_gray()
graph5 <- graph5 + labs(x = "Region")
graph5 <- graph5 + theme(axis.title = element_text(color = "brown", size = 20, face = "bold")) #axis title attribute
graph5 <- graph5 + theme(axis.text.x = element_text(color = "brown", size = 20, face = "bold")) #x-axis scale text attribute
graph5 <- graph5 + theme(axis.text.y = element_text(color = "brown", size = 20, face = "bold"))  #y-axis scale text attribute
graph5 <- graph5 + theme(legend.title = element_text(size = 20, face = "bold"))
graph5 <- graph5 + ggtitle("Box-plot : Unit Price (PSF) By Region for Condo/EC") + theme(plot.title = element_text(hjust = 0.5, face = "bold.italic", size =20))
graph5
ggsave("Trend5.png", width = 8, height = 5)


#---------------------------Part 2b (Correlation Analysis)----------------------------------------
realEstate$Transacted.Price.Per.Unit <-(realEstate$Transacted.Price / realEstate$Number.of.Units)
realEstate$Area.SQFT.Per.Unit <-(realEstate$Area.SQFT / realEstate$Number.of.Units)
tbl_corr <- realEstate %>% select(Transacted.Price.Per.Unit, Unit.Price.PSF, Area.SQFT.Per.Unit, Storey)
m<-cor(tbl_corr)
# plot the matrix
#Plot the ellipse correlation matrix
#corrplot(m, method = "ellipse")
corrplot(m, method = "ellipse",tl.col="red", mar=c(0,0,0,0), tl.offset = 1)
mtext("Correlation Matrix For Property In Singapore", at=2.5, line=-0.5, cex=1.5)
#Plot the number correlation matrix
#corrplot(m, method = "number")
corrplot(m, method = "number",tl.col="red", mar=c(0,0,0,0), tl.offset = 1)
mtext("Correlation Matrix For Property In Singapore", at=2.5, line=-0.5, cex=1.5)


#--------------------------- Part 3 Regression -------------------------------------------------
#-----------model 1: Regression for transactied price--------------
model1<-lm(Transacted.Price.Per.Unit~ Area.SQFT.Per.Unit + Property.Type + Lease.Length + Storey,  data=realEstate)
summary(model1)
ggplot(realEstate, aes(Area.SQFT.Per.Unit, Transacted.Price.Per.Unit)) + geom_point() + stat_smooth(method = lm)

realEstateReg2 <- realEstate %>% filter(Area.SQFT.Per.Unit > 8000)
model1_Above8000sqft<-lm(Transacted.Price.Per.Unit~ Area.SQFT.Per.Unit + Property.Type + Lease.Length, data=realEstateReg2)
summary(model1_Above8000sqft)
ggplot(realEstateReg2, aes(Area.SQFT.Per.Unit, Transacted.Price.Per.Unit)) + geom_point() + stat_smooth(method = lm)

#--------------------------Prediction using the generated regression model-------------------------
#Record 19367
predict(model1, newdata = data.frame(Area.SQFT.Per.Unit=968.76, Property.Type="Condominium", Lease.Length = "Long", Storey=21))

#Record 33756
predict(model1, newdata = data.frame(Area.SQFT.Per.Unit=1033.34, Property.Type="Condominium", Lease.Length = "Long", Storey=5))

#Record 25779
predict(model1, newdata = data.frame(Area.SQFT.Per.Unit=52059.01, Property.Type="Detached House", Lease.Length = "Freehold", Storey=0))
predict(model1_Above8000sqft, newdata = data.frame(Area.SQFT.Per.Unit=52059.01, Property.Type="Detached House", Lease.Length = "Freehold"))

#--------------------------- Part 3 Clustering -------------------------------------------------
realEstateCluster <- realEstate %>% select(Transacted.Price.Per.Unit, Area.SQFT.Per.Unit, Storey, Planning.Region, Property.Type)
realEstateCluster$Planning.Region <- as.numeric(as.factor(realEstateCluster$Planning.Region))
realEstateCluster$Property.Type <- as.numeric(as.factor(realEstateCluster$Property.Type))
names(realEstateCluster)

normalize <- function(x) {
  return ((x - min(x))/ (max(x) - min(x))) }
#Normalize Data
z <- realEstateCluster
normalizeRealEstateCluster <- cbind(as.data.frame(lapply(z[,1:3],normalize)))

#Run K-means cluster
set.seed(125) #Set the random order

kmc1 <- kmeans(normalizeRealEstateCluster, 6)
kmc1
plot(Transacted.Price.Per.Unit~Storey, realEstateCluster, col=kmc1$cluster)
plot(Transacted.Price.Per.Unit~Property.Type, realEstateCluster, col=kmc1$cluster)

#--------------------------------Less Transacted.Price.Per.Unit--------------------------------------
realEstateCluster <- realEstate %>% select(Unit.Price.PSF, Area.SQFT.Per.Unit, Storey, Planning.Region, Property.Type)
realEstateCluster$Planning.Region <- as.numeric(as.factor(realEstateCluster$Planning.Region))
realEstateCluster$Property.Type <- as.numeric(as.factor(realEstateCluster$Property.Type))
names(realEstateCluster)

normalize <- function(x) {
  return ((x - min(x))/ (max(x) - min(x))) }
#Normalize Data
z <- realEstateCluster
normalizeRealEstateCluster <- cbind(as.data.frame(lapply(z[,1:3],normalize)))

#Run K-means cluster
set.seed(125) #Set the random order

kmc2 <- kmeans(normalizeRealEstateCluster, 6)
kmc2
#plot(Unit.Price.PSF~Area.SQFT.Per.Unit, realEstateCluster, col=kmc2$cluster)
plot(Unit.Price.PSF~Storey, realEstateCluster, col=kmc2$cluster)
