districtMapSL<-function(shape_dataframe,selecteddate)
{
  
  print(selecteddate)
  
  
  #shape_dataframe %>% filter(Date==selecteddate) %>% 
   # tm_shape()+tm_polygons("Count",style="quantile",title="District distribution")
   # +tm_text(text = "District", size = 0.5)-> t_map
  
  
  
  t_map <- shape_dataframe %>%
    filter(Date == selecteddate) %>%
    tm_shape() +
    tm_polygons("Count", style = "quantile", title = "District distribution", palette = "YlOrRd") +  # Use a color palette
    tm_text(text = "District", size = 0.5)   # Display district names with a specified size
   # tm_layout(title = selected_district)  # Display the selected district name as the title
  
  
  
  t_map+ tmap_mode("plot")
  #t_map + tm_view(set.view =7.5) ### TODO :: Need to select suitable value based on screen size of user (viewer)
  
  
  
}