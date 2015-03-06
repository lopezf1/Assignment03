df <- read.csv('erate.csv')

names(df)
head(df)

# Deals greater than $10,000,000

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

# Create Month column and set datetime to POSIXlt and POSIXt format for analysis.

df$Month <- 1
df$ON.SR.WR.Status.Status.Detail.Start.Date <- as.character(df$ON.SR.WR.Status.Status.Detail.Start.Date)
df$ON.SR.WR.Status.Status.Detail.Start.Date <- strptime(df$ON.SR.WR.Status.Status.Detail.Start.Date, format=("%m/%d/%Y %H:%M"))
df$Month <- df$ON.SR.WR.Status.Status.Detail.Start.Date$mon+1
#df$Month <- month.abb[df$Month]

write.csv(df, 'erate_03.csv')

# Identify valid E-Rate contributors and redefine data frame to just those valid members.

valid_member <- read.csv('validmember.csv')
df <- merge(df, valid_member, by = c('SR.WR.Owner.Name'), all.x = TRUE)

# Add Week column

df$Week <- 1
df$Week <- df[,"SR.WR.Status.Status.Detail.Start.Week"]
df$Week <- as.character(df$Week)
df$Week <- substr(df$Week, start = 10, stop = 12)

# Plot inbound SR/WR volumes per week and month

library(ggplot2)
p1 <- ggplot(df, aes(Week, fill = X.My.Position.2..Employee.Name))
p1 + geom_histogram()

p2 <- ggplot(df, aes(Month, fill = X.My.Position.3..Employee.Name))
p2 + geom_histogram()

p3 <- qplot(data=df, x=Month, facets=.~Valid.Member, fill=X.My.Position.2..Employee.Name)
ggsave(file='test.png', plot=p3)  # Using qplot (very basic)

p4 <- ggplot(df, aes(Month, fill=X.My.Position.2..Employee.Name))
p4 <- p4 + geom_histogram() +
  facet_grid(.~Valid.Member) +
  theme_bw(base_family="Times", base_size=14) +
  labs(x="Month", y="Inbound SR/WR", title="Plot of Inbound Volume per Factory Tower") +
  xlim(1, 12)
print(p4)
ggsave(file="test2.png", plot=p4)  

# Practise only

mpg
qplot(data=mpg, x=hwy, y=displ, facets=.~drv, geom=c("point", "smooth"), method="lm")
g1 <- ggplot(mpg, aes(x=displ, y=hwy))
g1 + geom_point(color="steelblue", size=3, alpha=1/2) + 
  geom_smooth(method="lm") +
  facet_grid(.~drv)

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=drv), size=3, alpha=1/2) + 
  labs(title="How HWY Mileage is Affected by Displacement", x="Displacement", y="Highway Mileage") + geom_smooth(method="lm") + 
  facet_wrap(~trans, nrow=2, ncol=5) + theme_bw(base_family="Times")

str(mpg)
