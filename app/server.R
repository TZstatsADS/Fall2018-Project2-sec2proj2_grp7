
library(ggplot2)
library(ggthemes)


function(input, output){
  
  # Map output
  
  output$geo_plot = renderGvis({
    data = in_total %>%
      filter(DRG.Definition == input$select_DRG & year == input$select_year)
    
    gvisGeoChart(data, locationvar = "Provider.State",
                 colorvar = c("Average.Covered.Charges"),
                 options=list(region="US",
                              colors="['red']",
                              displayMode="regions",
                              resolution="provinces",
                              titleTextStyle="{fontSize:16}",
                              width="100%"))
    
      
  })
  
  output$g_plot = renderGvis({
    d = in_total %>% 
      filter(DRG.Definition == input$select_DRG & year == input$select_year)
    gvisGeoChart(d, locationvar = "Provider.State",
                 colorvar = "Average.Medicare.Payments",
                 options=list(region="US",
                              colors="['blue']",
                              titleTextStyle="{fontSize:28}",
                              displayMode="regions",
                              resolution="provinces",
                              width="100%"))
    
  })
  
  output$hist_plot = renderGvis({
    h = in_total %>% 
      filter(DRG.Definition == input$select_DRG & year == input$select_year) %>%
      select(Average.Covered.Charges)
    gvisHistogram(h,
                  options=list(
                    colors="['red']",
                    legend="{position:'top'}",
                    titleTextStyle="{fontSize:16}")
    )
    
  })
  
  output$histogram_plot = renderGvis({
    h = in_total %>% 
      filter(DRG.Definition == input$select_DRG & year == input$select_year) %>%
      select(Average.Medicare.Payments)
    gvisHistogram(h,
                  options=list(
                    colors="['blue']",
                    legend="{position:'top'}"
                  ))
    
  })
  
  output$col_plot = renderGvis(
    gvisBarChart(
      in_total %>% filter(DRG.Definition == input$select_DRG &
                            year == input$select_year) %>%
        arrange(desc(Average.Covered.Charges)),
      
      xvar = "Provider.State",
      yvar = c("Average.Covered.Charges","Average.Medicare.Payments"),
      options = list(
        colors = "['red','blue']",
        height = 600,
        width = "100%",
        legend = "{position: 'top'}",
        hAxis = paste("{",
                      "slantedText: true",
                      "}"))
    )
  )
  
  output$con_plot = renderGvis(
    gvisBarChart(
      in_total %>% filter(DRG.Definition == input$select_DRG &
                            year == input$select_year) %>%
        arrange(desc(Average.Covered.Charges)),
      xvar = "Provider.State",
      yvar = "Average.Total.Payments",
      options = list(
        colors = "['green']",
        height = 600,
        width = "100%",
        legend = "{position: 'top'}",
        hAxis = paste("{",
                      "slantedText: true",
                      "}"),
        vAxis = paste("{",
                      "slantedText: true",
                      "}"))
    )
  )
  
  output$sum_table = renderDataTable(
    in_total,
    options = list(pageLength = 10)
    
  )
  
  output$bubble_plot = renderGvis({
    
    data_1 = switch(as.character(input$select_year),
                    "2011" = in_2011_spending,
                    "2012" = in_2012_spending,
                    "2013" = in_2013_spending,
                    "2014" = in_2014_spending,
                    "2015" = in_2015_spending,
                    "2016" = in_2016_spending)
    
    gvisBubbleChart(
      
      data_1[1:input$select_top, ],
      xvar = "Average.Medicare.Payments",
      yvar = "Average.Covered.Charges",
      colorvar = "drg",
      sizevar = "Total.Spending",
      options = list(
        vAxis = "{title:'Total Discharges'}",
        hAxis = "{title: 'Average Medicare Payments per Discharge'}",
        height = 500,
        width = "100%",
        legend = "{position: 'top'}",
        bubble = "{textStyle:{color: 'none'}}"
      ))
  })
  
  output$bar_plot = renderGvis(
    gvisColumnChart(
      in_total_hospital %>% filter(DRG.Definition == input$select_DRG_2 &
                                     year == input$select_year_2 &
                                     # year == input$select_year_2 &
                                     Provider.Name %in% input$select_hospital),
      xvar="Provider.Name",
      yvar=c("Average.Covered.Charges","Average.Medicare.Payments"),
      options = list(
        height = 600,
        legend = "{position: 'top'}",
        hAxis = paste("{",
                      "slantedText: true",
                      "}"))
    )
  )
  
  output$line_plot = renderGvis({
    lineplot_data =
      in_total_hospital %>%
      filter(DRG.Definition == input$select_DRG_3 &
               Provider.Name == input$select_hospital_2)
    
    gvisLineChart(lineplot_data,
                  'year',
                  c('Average.Medicare.Payments','Average.Covered.Charges'),
                  options = list(
                    height = 600,
                    legend = "{position: 'top'}",
                    tooltip="{isHtml:'True'}",
                    pointSize = 10,
                    series = "{0: { pointShape: 'circle'},
                    1:{ pointShape: 'triangle'}}",
                    hAxis = paste("{",
                                  "slantedText: true",
                                  "}")),chartid="Points")
  })
  
  barplot_tbl = reactive(
    in_total_hospital %>%
      filter(DRG.Definition == input$select_DRG_2 &
               year == input$select_year_2 &
               Provider.Name %in% input$select_hospital)
  )
  output$subtable1 = renderGvis({
    gvisTable(barplot_tbl(),
              options=list(page='enable', height='automatic',width='automatic'))
  })
  
  
  
}