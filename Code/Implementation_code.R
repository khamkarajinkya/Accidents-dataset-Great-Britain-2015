#Raw Code for the UK Accidents dataset 

library(ggplot2)
library(clue)
library(chron)
library(ggmap)
library(viridis)
library(knitr)

df.hex = read.csv('Kaagle_Upload.csv',header = TRUE)


hex.subset=df.hex[,c("date", "time", "vehicle_type","day_of_week","sex_of_driver","age_of_driver","age_of_vehicle","NUmber_of_Casualities_unique_to_accident_index","No_of_Vehicles_involved_unique_to_accident_index","urban_or_rural_area","accident_severity","longitude","latitude")]

df.hex.subset=as.data.frame(hex.subset)


#Subsetting data
df.hex.subset$vehicle_category[df.hex.subset$vehicle_type %in% c(1,2,3,4,5,22,23,97)]="bikes"
df.hex.subset$vehicle_category[df.hex.subset$vehicle_type %in% c(8,9)]="cars"
df.hex.subset$vehicle_category[df.hex.subset$vehicle_type %in% c(10,11,18,19,20,21,98)]="heavy vehicles"
df.hex.subset$vehicle_category[df.hex.subset$vehicle_category %in% c(NA)]="others"

df.hex.subset=df.hex.subset[df.hex.subset$age_of_driver!=-1,]
df.hex.subset=df.hex.subset[df.hex.subset$sex_of_driver!=-1,]
df.hex.subset=df.hex.subset[df.hex.subset$age_of_vehicle!=-1,]
df.hex.subset=df.hex.subset[df.hex.subset$urban_or_rural_area!=-1,]
df.hex.subset=df.hex.subset[df.hex.subset$accident_severity!=-1,]
df.hex.subset=df.hex.subset[df.hex.subset$NUmber_of_Casualities_unique_to_accident_index!=-1,]
df.hex.subset=df.hex.subset[complete.cases(df.hex.subset[,c("longitude","latitude")]),]

df.hex.subset=df.hex.subset[df.hex.subset$vehicle_category=="heavy vehicles",]

df.hex.subset$region_type[df.hex.subset$urban_or_rural_area==1]="Urban"
df.hex.subset$region_type[df.hex.subset$urban_or_rural_area==2]="Rural"

#k-means and adding region variables.
model=kmeans(df.hex.subset[,c("longitude","latitude")],10)
model_region=clue::cl_predict(model,df.hex.subset[,c("longitude","latitude")])
model_region=data.frame(factor(model_region))
df.hex.subset=as.data.frame(cbind(df.hex.subset,model_region))
df.hex.subset$factor.model_region.[is.na(df.hex.subset$factor.model_region.)]=5
colnames(df.hex.subset)[colnames(df.hex.subset) == 'factor.model_region.']= 'region'

#vehicle type
df.hex.subset$vehicle[df.hex.subset$vehicle_type==10]="MiniBus"
df.hex.subset$vehicle[df.hex.subset$vehicle_type==11]="Coach"
df.hex.subset$vehicle[df.hex.subset$vehicle_type==19]="Vans"
df.hex.subset$vehicle[df.hex.subset$vehicle_type==18]="Tram"
df.hex.subset$vehicle[df.hex.subset$vehicle_type==20]="Trucks  over 3.5 tn"
df.hex.subset$vehicle[df.hex.subset$vehicle_type==21]="Trucks  over 7.5 tn"
df.hex.subset$vehicle[df.hex.subset$vehicle_type==98]="Trucks unknown weight"

#combining severity levels
df1.hex.subset=df.hex.subset

df1.hex.subset$category[df1.hex.subset$vehicle_type %in% c(10,11)]="Bus"
df1.hex.subset$category[df1.hex.subset$vehicle_type %in% c(20,21,98)]="Truck"
df1.hex.subset$category[df1.hex.subset$vehicle_type %in% c(18)]="Tram"
df1.hex.subset$category[df1.hex.subset$vehicle_type %in% c(19)]="Vans"

df1.hex.subset$accident_severity[df1.hex.subset$accident_severity==2]=1

df1.hex.subset$accident_severity[df1.hex.subset$accident_severity==1] = "Fatal Accident"
df1.hex.subset$accident_severity[df1.hex.subset$accident_severity==3] = "Slight Accident"

df1.hex.subset$femmal[df1.hex.subset$sex_of_driver=="1"]="Male"
df1.hex.subset$femmal[df1.hex.subset$sex_of_driver=="2"]="Female"
df1.hex.subset$femmal[df1.hex.subset$sex_of_driver=="3"]="NA"


newdata.time.agg <- aggregate(x = df1.hex.subset[c("time")], FUN = length, 
                              by = list(Group.time = df1.hex.subset$time))

newdata.time.agg$Group.time <- strftime(newdata.time.agg$Group.time,"%H:%M:%S")


newdata.time.agg$Group.time <- chron(times = newdata.time.agg$Group.time)

ggplot(newdata.time.agg, aes(x= Group.time, y= log(time))) + 
  geom_point() + 
  geom_smooth()+
  xlab("Time of the day")+
  ylab("Frequency of accidents")+
  ggtitle("Number of accidents varying by the time of the day")+
  theme(plot.title = element_text(hjust=0.5),axis.text.x = element_text(angle = 90, hjust = 1))

##For number of casualties
ggplot(df1.hex.subset,aes(x=NUmber_of_Casualities_unique_to_accident_index))
+geom_histogram(color="darkblue", fill="lightblue", binwidth = 5, aes(y = ..density..))
+xlab("Number of casualties")+ggtitle("Distribution of casualties across accidents")
+theme(plot.title = element_text(hjust=0.5))

##Age
ggplot(df1.hex.subset,aes(x=age_of_driver))
+geom_density(color="darkblue", fill="lightblue")
+xlab("Age of driver")
+ggtitle("Distribution of age of each driver")
+theme(plot.title = element_text(hjust=0.5))

#Count of accidents against Sex
as.matrix(table(df1.hex.subset$femmal))

#Count of accidents against Vehicle Type
as.matrix(table(df1.hex.subset$vehicle))

##Days of the week.
ggplot(df1.hex.subset,aes(x=day_of_week))
+geom_histogram(stat="count",color="darkblue", fill="lightblue")
+xlab("Day")+ggtitle("Distribution of accidents for the day of the week")
+theme(plot.title = element_text(hjust=0.5))

##vehicle age.
ggplot(df1.hex.subset,aes(x=age_of_vehicle))
+geom_histogram(color="darkblue", fill="lightblue",binwidth = 7)
+xlab("Age of Vehicle")+ggtitle("Distribution of vehicle age")
+theme(plot.title = element_text(hjust=0.5))

#Count of accidents against Region Type
as.matrix(table(df1.hex.subset$region_type))

#location clustering
location=data.frame(locations=df1.hex.subset$region)
location.table= data.frame(table(location))


fatal=data.frame(fatal_count=df1.hex.subset$accident_severity,class=df1.hex.subset$region)
fatal.table= data.frame(table(fatal))

fatal.table=fatal.table[fatal.table$fatal_count=="Fatal Accident",]

df2=merge(fatal.table,location.table,by.x="class",by.y = "location")
df2$percentage=(df2$Freq.x/df2$Freq.y)*100

df1.hex.subset=merge(df1.hex.subset,df2,by.x = "region",by.y ="class" )


UK=ggmap::get_map('UK',maptype=c("roadmap"))
ggmap(UK)+geom_point(aes(x=longitude, y=latitude,color=percentage) ,data=df1.hex.subset)
+scale_x_continuous( limits = c(-7.5,1.5) , expand = c( 0 , 0 ) )
+scale_y_continuous( limits = c(50,58) , expand = c( 0 , 0 ) )
+scale_color_viridis(option="plasma") + theme_bw() 
+ ggtitle("Percentage distribution of fatal accidents across UK") 
+ theme(plot.title = element_text(hjust=0.5))

#number of vehicles against type of vehicle faceted by severity
ggplot(df1.hex.subset,aes(x= accident_severity, fill = accident_severity)) 
+ geom_histogram(stat = "count") + xlab("Vehicle Category") 
+ facet_wrap(~category) + ylab("Count") 
+ ggtitle("Variation of accident severity with vehicle category") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

#number of deaths
ggplot(df1.hex.subset,aes(y=factor(NUmber_of_Casualities_unique_to_accident_index),x=accident_severity,color=category))
+geom_point()+geom_jitter()
+xlab("Accident severity")+ylab("Number of casualties")
+ggtitle("Accident severity vs number of casualties colored by vehicle category")

#Accident severity across Region type, faceted by vehicle category
ggplot(df1.hex.subset,aes(x= region_type, fill = accident_severity, group = accident_severity)) 
+ geom_histogram(stat = "count", position = "dodge")
+ xlab("Region Type") + facet_wrap(~category) + ylab("Frequency") 
+ ggtitle("Accident severity across Region type, faceted by vehicle category")

#Accident density against age faceted by accident severity, colored by gender
ggplot(df1.hex.subset,aes(x=(age_of_driver),group =femmal, fill =femmal))+
geom_histogram(binwidth = 10,aes(y=..density..),position = "dodge")+
facet_wrap(~accident_severity)+theme(plot.title = element_text(hjust=0.5))
+xlab("age")+ylab("density")
+ggtitle("Accident density against age faceted by accident severity, colored by gender")

#model
model2=glm(factor(accident_severity, levels = c("Slight Accident", "Fatal Accident")) ~ category+region_type+age_of_driver+femmal+region+age_of_vehicle,
           data=df1.hex.subset,family = binomial(link = "logit" ))

model.df2=data.frame(fitted=fitted.values(model2),resid=residuals(model2,type = "response"))
model.df2=cbind(df1.hex.subset,model.df2)

#Fitted vs Resid
ggplot(model.df2,aes(x=fitted,y=resid))+geom_point()+geom_jitter()
+geom_smooth()+ylab("Residuals")+xlab("fitted values")


#grid implementation
grid.df = expand.grid(age_of_driver=seq(21,69,3), category = unique(df1.hex.subset$category),region_type=unique(df1.hex.subset$region_type),age_of_vehicle=seq(0, 20, 1),region=unique(df1.hex.subset$region),femmal=c("Male", "Female"))

model.pred = predict(model2, type = "response", newdata = grid.df)

grid.pred.df = data.frame(grid.df, fatal.prob = as.vector(model.pred))


grid1=aggregate(grid.pred.df$fatal.prob,list(grid.pred.df$age_of_vehicle, grid.pred.df$category, grid.pred.df$region),mean)

colnames(grid1) = c("Vehicle_Age", "Vehicle_Category", "Region", "Probability")

ggplot(grid1, aes(x = Vehicle_Age, y =Probability, group = Vehicle_Category, color = Vehicle_Category)) 
+ geom_line() + facet_wrap(~Region, ncol = 5) 
+ ggtitle("Fitted values against vehicle age, region, and vehicle category")

#grid implementation 2
grid2=aggregate(grid.pred.df$fatal.prob,list(grid.pred.df$age_of_driver,grid.pred.df$femmal, grid.pred.df$region_type),mean)

colnames(grid2) = c("Driver_Age", "Gender", "Region_Type","Probability")

ggplot(grid2, aes(x = Driver_Age, y = Probability, group = Gender, color = Gender)) 
+ geom_line() + facet_wrap(~Region_Type) 
+ ggtitle("Fitted values against driver age, region types, and sex of driver")

