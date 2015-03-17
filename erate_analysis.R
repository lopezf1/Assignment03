setwd("~/Coursera")
library(ggplot2)

df <- read.csv('erate.csv')

str(df)

# Output deals greater than $10,000,000.  Create column, titled "Duplicate" 
# to filter out any duplicate entries.  CONSIDER PUTTING THIS INTO A FUNCTION CALL.

df$Duplicate <- 1
big <-df[df$Opty.Revenue..TOV. > 10000000, c("Opportunity.Account.Name", 
                                             "Opty.Revenue..TOV.", 
                                             "Opportunity.ID",
                                             "Duplicate",
                                             "SR.WR.ID",
                                             "Won.Loss.No.Sale.Status")]
big <- format(big,big.mark=",")
big <- big[order(big$Opportunity.ID, big$SR.WR.ID),]
big$Duplicate <- duplicated(big$Opportunity.ID)
big[big$Duplicate==FALSE,]

write.csv(df, 'erate_02.csv')
write.csv(big, 'big_01.csv')

# Convert ON.SR.WR.Status.Status.Detail.Start.Date from Factor to POSIXlt.  Create a new
# column, titled "Month"  Determine month from POSIXlt, abbreviate it and then convert it
# to a categorical factor for future plotting.  CONSIDER PUTTING THESE INTO A FUNCTION CALL.

startDate <- as.character(df$ON.SR.WR.Status.Status.Detail.Start.Date)
startDate <- strptime(startDate, format=("%m/%d/%Y %H:%M"))
df$ON.SR.WR.Status.Status.Detail.Start.Date <- startDate
df$Month <- months(startDate, abbreviate=TRUE)
df$Month <- factor(df$Month, month.abb)

write.csv(df, 'erate_03.csv')

# Convert SR.WR.Status.Status.Detail.Start.Week from Factor to Character  Create a new
# column, titled "Week".  Capture only the week number.  CONSIDER PUTTING THESE INTO A FUNCTION CALL.

df$Week <- df[,"SR.WR.Status.Status.Detail.Start.Week"]
df$Week <- as.character(df$Week)
df$Week <- substr(df$Week, start = 10, stop = 12)

# Identify valid E-Rate contributors and redefine data frame to just those valid members.  THIS IS A PRE
# DEFINED LIST PROVIDED BY MANAAGEMENT.

valid_member <- read.csv('validmember.csv')
df <- merge(df, valid_member, by = c('SR.WR.Owner.Name'), all.x = TRUE)
df$Valid.Member <- as.character(df$Valid.Member)

# Plot inbound SR/WR volumes per week and month using ggplot2.  FIX ISSUE WITH VALID MEMBERS.
# REMOVE ANY DATA NOT PERTINENT TO VALID MEMBERS.


pdf <- df[df$Valid.Member=="Yes",]
head(pdf, 10)
p1 <- ggplot(df, aes(Week, fill = X.My.Position.2..Employee.Name))
p1<- p1 + geom_histogram() +
  labs(x="Week", y="Inbound SR/WR", title="Plot of Weekly Inbound Volume per Factory Tower")
print(p1)

p2 <- ggplot(df, aes(Month, fill=X.My.Position.2..Employee.Name))
p2 <- p2 + geom_histogram() +
  labs(x="Month", y="Inbound SR/WR", title="Plot of Monthly Inbound Volume per Factory Tower")
print(p2)

ggsave(file="Weekly Inbound.png", plot=p1)
ggsave(file="Monthly Inbound.png", plot=p2) 


