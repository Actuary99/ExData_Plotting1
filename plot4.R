#Install the lubridate package for ease of working with date data 
install.packages("lubridate")
library("lubridate")

#Read in the data set from the working directory; the data is delimited by semi-colons
data<-read.table("./household_power_consumption.txt",header=TRUE, colClasses=c(rep("character")),sep=";")

#Get rid of any observations that contain missing values, designated with a "?" 
keep<-subset(data,data[,1]!="?" & data[,2]!="?" & data[,3]!="?" & data[,4]!="?" & data[,5]!="?" & data[,6]!="?"
            & data[,7]!="?" & data[,8]!="?" & data[,9]!="?")

#Only keep data associated with Feb 1/2 of 2007. proper.days is a logical vector holding 'true' for the right combo
#of month/day/year.  The first function recasts the keep$Date information into a day/month/year format so that we
#can easily select the proper day/month/year of 02/02/2007 or 02/01/2007 for our final data set (2nd step below)
keep$Date<-as.Date(keep$Date,format="%d/%m/%Y")
proper.days<-month(keep$Date)==2 & (day(keep$Date)==1 | day(keep$Date)==2) & year(keep$Date)==2007

#We want to bind the original data set with the logical vector designating the proper month/day/year combo.  We also
#Want the weekday associated with the final dataset.
graph.info<-cbind(keep,proper.days)
graph.info<-subset(graph.info,proper.days=="TRUE")

#For all graphs, we will convert the associated data to numeric before the plot is called.  We will graph 4 plots, 
#in a 2 x 2 panel layout

png(file="plot4.png",height=480,width=480)

par(mfrow=c(2,2))

#Plot number 1 is of the Global_active_power by day 
  graph.info$Global_active_power<-as.numeric(graph.info$Global_active_power)
  tpower<-ts(graph.info$Global_active_power)
  plot(tpower,ylab="Global active power (kilowatts)",xlab=" ",xaxt="n")
  axis(side=1,at=c(0,1500,2880),labels=c("Thu","Fri","Sat"),tick=TRUE,lwd=1) 

  
  
#Plot number 2 is of the Voltage by day
  graph.info$Voltage<-as.numeric(graph.info$Voltage)
  tpower<-ts(graph.info$Global_active_power)
  plot(tVoltage,ylab="Voltage",xlab="datetime",xaxt="n")
  axis(side=1,at=c(0,1500,2880),labels=c("Thu","Fri","Sat"),tick=TRUE,lwd=1) 
  
#Plot number 3 is of the Energy by sub_meter 
  graph.info$Sub_metering_1<-as.numeric(graph.info$Sub_metering_1)
  graph.info$Sub_metering_2<-as.numeric(graph.info$Sub_metering_2)
  graph.info$Sub_metering_3<-as.numeric(graph.info$Sub_metering_3)
  
  sub1<-ts(graph.info$Sub_metering_1)
  sub2<-ts(graph.info$Sub_metering_2)
  sub3<-ts(graph.info$Sub_metering_3)
  
  plot(sub1,xlab=" ",xaxt="n",col="black",ylab="Energy sub metering")
  lines(sub2,xlab=" ",xaxt="n",col="red")
  lines(sub3,xlab=" ",xaxt="n",col="blue")
  
  axis(side=1,at=c(0,1500,2880),labels=c("Thu","Fri","Sat"),tick=TRUE,lwd=1) 
  legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1,col=c("black","red","blue"),bty="n",cex=.75)

#Plot number 4 is the Global reactive power by Day
  
  graph.info$Global_reactive_power<-as.numeric(graph.info$Global_reactive_power)
  trepower<-ts(graph.info$Global_reactive_power)
  plot(trepower,ylab="Global_reactive_power",xlab="datetime",xaxt="n")
  axis(side=1,at=c(0,1500,2880),labels=c("Thu","Fri","Sat"),tick=TRUE,lwd=1) 
  axis(side=2,at=c(0.0,0.1,0.2,0.3,0.4,0.5),labels=c("0.0","0.1","0.2","0.3","0.4","0.5"),tick=TRUE,lwd=1)

dev.off()