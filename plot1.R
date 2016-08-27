#Install the lubridate package for ease of working with date data 
install.packages("lubridate")
library("lubridate")

#Read in the data set from the working directory; the data is delimited by semi-colons
data<-read.table("./household_power_consumption.txt",header=TRUE, colClasses=c(rep("character")),sep=";")

#Get rid of any observations that contain missing values, designated with a "?" 
keep<-subset(data,data[,1]!="?" & data[,2]!="?" & data[,3]!="?" & data[,4]!="?" & data[,5]!="?" & data[,6]!="?"
            & data[,7]!="?" & data[,8]!="?" & data[,9]!="?")

#Only keep data associated with Feb 1/2 of 2007. proper.days is a logical vector holding 'true' for the right combo
#of month/day/year
keep$Date<-as.Date(keep$Date,format="%d/%m/%Y")
proper.days<-month(keep$Date)==2 & (day(keep$Date)==1 | day(keep$Date)==2) & year(keep$Date)==2007
graph.info<-cbind(keep,proper.days)
graph.info<-subset(graph.info,proper.days=="TRUE")

#We are asked to graph a histogram of the Global_active_power variable from the final data set for Feb 1 or 2, 2007

png(file="plot1.png",width=480,height=480)
hist(as.numeric(graph.info$Global_active_power),col="red",main="Global Active Power",xlab="Global active power (kilowatts)")
dev.off()