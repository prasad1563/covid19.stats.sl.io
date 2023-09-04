clusterPieChart <- function(dataset, plot_date) {
  if (plot_date == "2020-11-13")
    print("Set date")
  
  plot_df <- subset(dataset, Date <= plot_date)
  
  lastDayCum <- plot_df[nrow(plot_df), 2:5]
  previousDayCum <- plot_df[nrow(plot_df) - 1, 2:5]
  
  lastDay <- lastDayCum - previousDayCum
  
  clusterName <- c("District Cases", "Imported Cases", "Other cases")
  clusterValues <- c(lastDay[1, 2], lastDay[1, 3], lastDay[1, 4])
  
  # Create a data frame with all category colors set to "white"
  lastDayDF <- data.frame(
    cluster = clusterName,
    count = clusterValues,
    color = "white"
  )
  
  # Create a pie chart with a hole (donut) and transparent inner colors
  pie_chart <- plot_ly(
    labels = ~lastDayDF$cluster, values = ~lastDayDF$count,
    type = 'pie',
    marker = list(line = list(width = 0, color = "white")),
    textposition = 'outside', textinfo = 'label+percent',
    hoverinfo = 'label+percent+value',  # Show value on hover
    domain = list(row = 0, column = 0.5),
    hole = 0.4  # Adjust the size of the hole for the donut (smaller hole)
  )
  
  return(pie_chart)
}
