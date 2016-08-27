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

#We want to bind the original data set with the logical vector designating the proper month/day/year combo.  
graph.info<-cbind(keep,proper.days)
graph.info<-subset(graph.info,proper.days=="TRUE")

#Set up data frames for each of the metering stations - to graph, the associated data must be recast to class 'numeric'
graph.info$Sub_metering_1<-as.numeric(graph.info$Sub_metering_1)
graph.info$Sub_metering_2<-as.numeric(graph.info$Sub_metering_2)
graph.info$Sub_metering_3<-as.numeric(graph.info$Sub_metering_3)

#The graph we want to build requires a conversion to a time series, designated with the 'ts' command.
#The graph will plot all 2880 obs and these will be spread out over the x tick lines.  For now, we suppress
#these observations from printing out by using the 'xaxt="n"' command.  Later, we will replace them with 
#the labels we want.  We use 'Thursday' for observations 1-1500, 'Friday' for obs 1501-2800' and 'Saturday' for the end.
sub1<-ts(graph.info$Sub_metering_1)
sub2<-ts(graph.info$Sub_metering_2)
sub3<-ts(graph.info$Sub_metering_3)

png(file="plot3.png",height=480,width=480)

#We want to overlay various plots.  The first call to 'plot' is with the plot command.  To overlay subsequent plots,
#we make a call to the 'lines' command
par(mar=c(6,4,2,1))

plot(sub1,xlab=" ",xaxt="n",col="black",ylab="Energy sub metering")
lines(sub2,xlab=" ",xaxt="n",col="red")
lines(sub3,xlab=" ",xaxt="n",col="blue")

#Change the x axis tick labels and insert a legend in the top right of the plot
axis(side=1,at=c(0,1500,2880),labels=c("Thu","Fri","Sat"),tick=TRUE,lwd=1) 
legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1,col=c("black","red","blue"),bty="n",cex=.75)

dev.off()