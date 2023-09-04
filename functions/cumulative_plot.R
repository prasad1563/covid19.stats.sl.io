cumulative_plot <- function(dataset, plot_date) {
  plot_df <- subset(dataset, Date <= plot_date)
  
  color <- c("infectedCount" = "Red", "districtSum" = "Yellow", "importedCase" = "Blue", "otherCluster" = "Green")
  
  selectedDatePlot <- plot_ly(data = plot_df) %>%
    add_lines(x = ~Date, y = ~`infectedCount`, name = "Total Infected Count", line = list(color = "Red"), legendgroup = "Infected") %>%
    add_lines(x = ~Date, y = ~`districtSum`, name = "District Cases", line = list(color = "Yellow"), legendgroup = "District") %>%
    add_lines(x = ~Date, y = ~`importedCase`, name = "Imported Cases", line = list(color = "Blue"), legendgroup = "Imported") %>%
    add_lines(x = ~Date, y = ~`otherCluster`, name = "Other Cases", line = list(color = "Green"), legendgroup = "Other") %>%
    layout(title = "Case Distributions Of Patients-Cumulative",
           xaxis = list(title = "Month"),
           yaxis = list(title = "Count Of Individuals"),
           legend = list(title = "Category"),
           showlegend = TRUE,
           titlefont = list(family = "Arial", weight = "bold"))
  
  selectedDatePlot
}
