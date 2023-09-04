districtLinePlot<-function(dataset,plot_district,selecteddate)    
{

  
  districtFilteredData <- dataset%>% 
    
    filter(District == plot_district)
  
  
  plot_df<-districtFilteredData %>%
            filter(Date <= as.character(selecteddate))
  
  
  ##Subset the dataset
  
  #plot_df = as.data.frame(dataset[1:selectedRow,c(1,districtName)] )  #districtLineData
  plotData<-as.data.frame(plot_df[2:3])
  
  
  colnames(plotData)<-c("Date","Count")
  
  plot_ly(plotData,x=~Date,y=~Count,mode="line") %>% layout(title=paste("Patient Growth In",plot_district),
                                                              xaxis=list(title="Month"),
                                                              yaxis=list(title="Count of Infected Individuals"))
  
  
  
  
}
