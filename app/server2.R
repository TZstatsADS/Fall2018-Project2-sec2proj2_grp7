
library(ggplot2)
library(ggthemes)


function(input, output){
  
  output$read0<- renderText({"We totally understand that you are upset to get sick and the hardest thing is to find a fitted hospital that you can go quickly."})
  output$read1<- renderText({"Don't worry! Please look at here to find the best hospitals around you based on your preference."})
  
  output$read2<- renderText({"-> Step 1: Select the state you live in or you wish to go to and the cost range you are comfortable to pay."})
  output$read3<- renderText({"-> Step 2: Choose one of the most cared hospital qualities and your diagnosis related group."})
  output$read4<- renderText({"-> Step 3: Check the search result table and the map for the basic information of all hospitals."})
  output$read5<- renderText({"-> Step 4: Click one you are interested in from the table to zoom it in on the map to see the exact location of the hospital."})
  
  output$read6<- renderText({"Want more information about spending? Please click the Overview tab to explore more!"} )
  
  output$read7<- renderText({"Part 1: Check the State Overview to get a overview of average charges and average medicare payments by states"})
  output$read8<- renderText({"-> Select the year and diagnosis related group you are interested in.You can also search one state to see the detailed charges and payments inforamtion."})
  output$read9<- renderText({"Part 2: Take a look at our Hospital Comparison by selecting year, DRG, and the hospitals you are cared about"})
  output$read10<- renderText({"Part 3: Click the Spending Exploration to learn the total spending for each DRG and the year_trend spending based on selected DRG and hospital."})
  
  
  
  
  output$team0<- renderText({"About Team"})
  output$team1<- renderText({"We are five excellent students from Columbia University in Statistics and the owners of the Version 2.0 medicare app. We aim to make data more straightforward and convinent for medicare searching.)"})
  output$team2<-renderText({"Please contact us if you have any questions or recommendations:"})
  output$team3<- renderText({"-> Xinwei Feng (email: xf2168@columbia.edu)"})
  output$team4<- renderText({"-> Lujia Wang (email: lw2772@columbia.edu)"})
  output$team5<- renderText({"-> Binhan Wang (email: bw2544@columbia.edu)"})
  output$team6<- renderText({"-> Rui Zhang (email: rz2406@columbia.edu)"})
  output$team7<- renderText({"-> Yixin Zhang (email: yz3223@columbia.edu)"})
  output$team8<- renderText({"Thank you for using!"})
  
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