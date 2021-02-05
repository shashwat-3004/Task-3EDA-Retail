
# **Attaching the packages**
library(tidyverse)
library(GGally)
library(gridExtra)


# **Read the data**


data<-read.csv("SampleSuperstore.csv")





head(data)


# **Basic summary of the data**

str(data)


summary(data)

# **Exploratory Data Analysis**

- Different colours in the plots denote different regions of the United States.

num_data<-data[,10:13]
ggpairs(num_data,aes(colour=data$Region))+theme_bw()



p1<-ggplot(data)+geom_bar(aes(Ship.Mode,fill=Region),position = "dodge")+
  theme(axis.text.x = element_text(angle=45,hjust=0.9))+xlab("Shipment Mode")+
  ylab("Frequency")
p2<-ggplot(data)+geom_bar(aes(Ship.Mode,fill=Segment),position="dodge")+
  theme(axis.text.x = element_text(angle=45,hjust=0.9))+xlab("Shipment Mode")+
  ylab("Frequency")
p3<-ggplot(data)+geom_bar(aes(Ship.Mode,fill=Category),position="dodge")+
  theme(axis.text.x = element_text(angle=45,hjust=0.9))+xlab("Shipment Mode")+
  ylab("Frequency")
grid.arrange(p1,p2,p3,nrow=2)



plot1<-ggplot(data)+geom_bar(aes(Segment,fill=Category),position="dodge")+ ylab("Frequency")
plot1





pl1<-ggplot(data)+geom_bar(aes(Region,fill=Segment),position = "dodge")+ ylab("Frequency")
pl2<-ggplot(data)+geom_bar(aes(Region,fill=Category),position="dodge")+ ylab("Frequency")
grid.arrange(pl1,pl2,ncol=2)





seg_sale_region<-ggplot(data)+geom_col(aes(x=Segment,y=Sales,fill=Category))+facet_grid(.~Region)+
  theme(axis.text.x = element_text(angle=45,hjust=0.9))
seg_sale_region+ggtitle("Sales vs Segment(Region-Wise)")



region_sales<-ggplot(data)+geom_col(aes(x=Region,y=Sales,fill=Category))
region_sales+ggtitle("Sales vs Region(Category-Wise)")





profit_sum<-data%>%group_by(Category)%>% summarise(Profits=sum(Profit))
profit_sum
profit_sum%>%ggplot()+geom_col(aes(x=Category,y=Profits),fill="#fc6203")+ggtitle("Profits vs Category")




region_profit<-data%>% group_by(Region)%>% summarise(Profits=sum(Profit))
region_profit
region_profit%>%ggplot()+geom_col(aes(x=Region,y=Profits),fill="darkgreen")+ggtitle("Profits vs Region")



ggplot()+geom_col(data=region_profit,aes(x=Region,y=Profits),fill="green")+
  geom_col(data=data,aes(x=Region,y=Sales),fill="orange",alpha=0.5)+
  ggtitle("Profits/Sales vs Region")+ylab("Profits/Sales")



ggplot()+geom_col(data=profit_sum,aes(x=Category,y=Profits),fill="green")+
  geom_col(data=data,aes(x=Category,y=Sales),fill="red",alpha=0.5)+
  ylab("Profits/Sales vs Category")+ggtitle("Profits/Sales vs Category")




ggplot(data)+geom_bar(aes(x=Segment,fill=Sub.Category),position = "dodge")



subCategory_profit<-data%>%group_by(Sub.Category)%>%summarise(Profits=sum(Profit),Sale=sum(Sales))

subCategory_profit%>%ggplot()+geom_col(aes(x=Sub.Category,y=Profits),fill="blue")+
  geom_col(aes(x=Sub.Category,y=Sale),fill="orange",alpha=0.6)+
  ggtitle("Profits/Sales vs Sub Category")+theme(axis.text.x = element_text(angle=45,hjust=0.9))




salesprofit_city<-data%>%group_by(City)%>%summarise(Sales=sum(Sales),Profit=sum(Profit))

head(salesprofit_city)





sale1<-salesprofit_city%>%slice_max(Sales,n=5)%>%ggplot()+geom_col(aes(x=City,y=Sales),fill="green")+
  coord_flip()+ggtitle("Top 5 cities with highest sales")


sale2<-salesprofit_city%>%slice_min(Sales,n=5)%>%ggplot()+geom_col(aes(x=City,y=Sales),fill="green")+
  coord_flip()+ggtitle("Top 5 cities with least sales")


profit1<-salesprofit_city%>%slice_max(Profit,n=5)%>%ggplot()+geom_col(aes(x=City,y=Profit),fill="orange")+
  coord_flip()+ggtitle("Top 5 cities with max profit")


profit2<-salesprofit_city%>%slice_min(Profit,n=5)%>%ggplot()+geom_col(aes(x=City,y=Profit),fill="orange")+
  coord_flip()+ggtitle("Top 5 cities with least profit")
grid.arrange(sale1,profit1,sale2,profit2,ncol=2)




ggplot(data)+geom_smooth(aes(x=Profit,y=Discount),colour="red")+facet_grid(.~Region)+
  ggtitle("Profit vs Discount(Region-Wise)")




ggplot(data)+geom_smooth(aes(x=Quantity,y=Discount),colour="darkgreen")+facet_grid(.~Region)+
  ggtitle("Quantity vs Discount(Region-Wise)")



ggplot(data)+geom_smooth(aes(x=Quantity,y=Profit),colour="orange")+facet_grid(.~Region)+
  ggtitle("Quantity vs Profit(Region-Wise)")
