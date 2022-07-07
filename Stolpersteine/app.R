#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#





#### UI ----

ui <- navbarPage(title = "Berlins Stolpersteine",
  theme = my_theme2,
  #useShinydashboard(),

  #### |> Karte ----
    tabPanel("Karte",
             sidebarLayout(
               sidebarPanel(
                      DT::DTOutput("karte_stolpi_DT")
                      ),
               mainPanel(
                      leafletOutput("stolpisKarte", height = 800)
                      )
               )),

  #### |> Verlegungen ----
   tabPanel("Verlegungen",
           titlePanel(h1("Wann und wo wurden die 8740 Stolpersteine verlegt?", align = "center")),
           fluidRow(
             column(4, plotly::plotlyOutput("verlegungsdatum_hist")
              ),
             column(4, plotly::plotlyOutput("verlegungen_pro_monat")
             ),
             column(4, plotly::plotlyOutput("verlegungsdatum_pro_jahr")
             ),
           ),

           fluidRow(
             column(4, plotly::plotlyOutput("bezirk_count")
             ),
             column(4, plotOutput("verlegungsdatum_boxplot")

             ),
             column(4, selectInput("verlegungen_select_year",
                                   "Jahr für obigenen Plot auswählen",
                                   choices = c(1996, 2000:2022),
                                   selected = 2013)
             ),
           )
           ),

  #### |> Über die Erinnerten ----
  tabPanel("Über die Erinnerten",
           #titlePanel(h1("Wer", align = "center")),
           tags$style(".small-box.bg-blue { background-color: #8A9590 !important}"), #color: #000000 !important;
           fluidRow(
             column(2, uiOutput("box_gebjahr")),
             column(2, uiOutput("box_females")),
             column(2, uiOutput("box_males")),
             column(2, uiOutput("box_murdered")),
             column(2, uiOutput("box_death_age")),
             column(2, uiOutput("box_survived"))

           ),

           fluidRow(
             column(4, plotly::plotlyOutput("histo_jahrgaenge")),
             column(4, plotly::plotlyOutput("dest_sex")),
             column(4, plotly::plotlyOutput("geburtsorte")),
             ),

           fluidRow(
             column(8, plotly::plotlyOutput("berufe_sex")),
             column(4,
                    wellPanel(
                      radioButtons("sex_fuer_berufe", "Top Berufe bei",
                                   #c("Allen", "Frauen", "Männern"), selected, inline, width,
                                   choiceNames = c("Allen", "Frauen", "Männern"),
                                   choiceValues = c("Alle","w", "m")),
                      sliderInput("top_n_berufe", "Beschränke auf die ersten X.", 1, 15,
                                  10)
                    )
           )


           )


  ),

  #### |> Deportation ----
  tabPanel("Deportation",
           #titlePanel(h1("Wer", align = "center")),


           fluidRow(
             column(3, plotly::plotlyOutput("deported_when")),
             column(3, plotly::plotlyOutput("deported_where")),
             column(3, plotly::plotlyOutput("murdered_when")),
             column(3, plotly::plotlyOutput("murdered_where"))


           ),

           fluidRow(
             column(4, plotly::plotlyOutput("deportated_per_day")),
             column(8,
                    wellPanel(
                      h5("Die zwei größten Transitlager"),
                      HTML(paste("<p>Insgesamt wurden", nrow(unequal_dep_dest), "Personen nicht an dem Ort ermordet, zu dem sie zuerst deportiert worden sind.
                            Den größten Teil macht hierbei Theresienstadt aus, was auch die Balkendiagramme oben zeigen.
                            Aber auch das sogenannte Ghetto Litzmannstadt diente vor allem als Transitlager, was auf
                            <a href='https://de.wikipedia.org/wiki/Ghetto_Litzmannstadt' target = '_blank'>Wikipedia</a> nachgelesen werden kann.
                                 Die Tabellen zeigen jeweils die sieben häuigsten Transit- bzw. Vernichtungslager, bei denen der erste Deportations- und Ermordungsort
                                 nicht übereinstimmen, sowie die sieben häufigsten Kombinationen. </p>")),
                      fluidRow(
                        column(3, tableOutput("unequal_dep_dest_count_dep")),
                        column(3, tableOutput("unequal_dep_dest_count_dest")),
                        column(6, tableOutput("unequal_dep_dest_count_both"))
                      )

                    ),

             ),
             #column(6, )


           ),

           fluidRow(



           ),
           p(),

           sidebarLayout(
             sidebarPanel(
               fluidRow(
                 h5("Tage von Deportation bis zur Ermordung"),
                 p("Bei den Menschen, bei denen Deportations- und Ermordungsort übereinstimmen und Deportations-
                   und Ermordungsdatum exakt bekannt sind, lassen sich die Tage zwischen Deportation und Ermordung berechnen:"),
                 column(4, sliderInput("mindest_n_opfer",
                                       "Beschränke Anzahl",
                                       1, 21, 10)),
                 column(4, radioButtons("time_dep_murder_auf_ab", "Sortiere",
                                        choiceNames = c("aufsteigend", "absteigend"),
                                        choiceValues = c("auf","ab"))),
                 column(4, radioButtons("time_dep_murder_sort", "Sortiere nach",
                                        choiceNames = c("Anzahl Ermordeter", "Median", "Max"),
                                        choiceValues = c("n","med", "max")))


               ),
               tableOutput("ranking_explained")

               # fluidRow(
               #   column(6, sliderInput("mindest_n_opfer",
               #                         "Mindestanzahl Ermordeter pro Konzentrationslager",
               #                         1, 21, 10),
               #          radioButtons("time_dep_murder_auf_ab", "Sortiere",
               #                          choiceNames = c("aufsteigend", "absteigend"),
               #                          choiceValues = c("auf","ab")),
               #   radioButtons("time_dep_murder_sort", "Sortiere nach",
               #                choiceNames = c("Anzahl Datenpunkte", "Median", "Max"),
               #                choiceValues = c("n","med", "max"))
               #   ),
               #   column(6, )
               #
               #
               # )



             ),
             mainPanel(
               plotly::plotlyOutput("time_from_dep_to_murder")

               # fluidRow(
               #   column(6, ),
               #   column(6, plotly::plotlyOutput("time_from_dep_to_murder"))
               # )



             )
           )
  ),

  #### |> Vor- und Nachnamen ----
    tabPanel("Vor- und Nachnamen",
           titlePanel(h1("Häufigkeit von Vor- und Nachnamen", align = "center")),
           fluidRow(

             column(4,
                    wellPanel(
                      fluidRow(
                        column(6, radioButtons("sex_fuer_vornamen", "Top Vornamen bei",
                                               choiceNames = c("Allen", "Frauen", "Männern"),
                                               choiceValues = c("Alle","w", "m"))),
                        column(6, sliderInput("vornamen_sex_n",
                                              "Anzahl Vornamen:",
                                              min = 1,
                                              max = 20,
                                              value = 10))
                      ),
                      fluidRow(
                        column(6, radioButtons("nachnamen_oder_geburtsname", "Top Nachnamen bei",
                                               choiceNames = c("Geburtsnamen", "Nachnamen (Geburtsnamen + Angeheiratete)"),
                                               choiceValues = c("birthname","lastname"))),
                        column(6,  sliderInput("geburtsnamen_n",
                                               "Anzahl Nachnamen:",
                                               min = 1,
                                               max = 20,
                                               value = 10))
                      )
                    )
                    ,
             ),
             column(4, plotly::plotlyOutput("vornamen_sex")),
             column(4, plotly::plotlyOutput("geburtsnamen"))
             ),

           sidebarLayout(
             sidebarPanel(
               h4("Vornamen auswählen, um deren Häufigkeit nach Geburtsjahr zu vergleichen:"),
               textInput("vorname_1", value = "Gertrud", label = "Vorname 1 auswählen:"),
               #conditionalPanel(
               #  condition = "vorname1_not_exist == 'TRUE'",
                 textOutput("conditional_vorname1"),
               p(),
               #),
               textInput("vorname_2", value = "Ruth", label = "Vorname 2 auswählen:"),
               #conditionalPanel(
               #  condition = "vorname2_not_exist == 'TRUE'",
                 textOutput("conditional_vorname2")
               #)
             ),
             mainPanel(
               plotly::plotlyOutput("ggplot")
             )
           )



    ),


  #### |> Dump ----
    # tabPanel("base",
    #          tags$style(".small-box.bg-blue { background-color: #8A9590 !important}"), #color: #000000 !important;
    #          dropdownButton(
    #            tags$h3("List of Input"),
    #            selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
    #            selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
    #            sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
    #            circle = TRUE, status = "danger", icon = icon("cog"), width = "300px",
    #            tooltip = tooltipOptions(title = "Click to see inputs !")
    #          ),
    #          fluidRow(
    #            column(4,
    #                   wellPanel(
    #                     sliderInput(
    #                       "bins", label = "Number of bins:",
    #                       min = 1, value = 30, max = 50
    #                     )
    #                   )
    #            ),
    #            column(8, plotOutput("base"))
    #          )
    #          #fluidRow(
    #          #  column(4,valueBoxOutput("boxOutputTest")),
    #          #  column(4, valueBoxOutput("ueberlebensrate")),
    #          #  column(4)
    #
    #          )



)





#### Server ----

server <- function(input, output) {


  #### Map tab ----


  output$stolpisKarte <- renderLeaflet({
    stolpis %>%
      leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addCircleMarkers(lat = ~ lat,
                       lng = ~ lng,
                       radius = 2,
                       color = ~ pal(sublocality_level_1),
                       popup = ~ paste0("<b>", Name, "</b>",
                                        "<br/>", query_address,
                                        "<br/>Bezirk ", sublocality_level_1))
                       #clusterOptions = markerClusterOptions())
  })


  get_marker_click <- reactive({
    marker_click <- input$stolpisKarte_marker_click
    if(!is.null(marker_click)){
      marker_lat <- marker_click$lat
      marker_lng <- marker_click$lng

    } else {
      marker_lat <- 0
      marker_lng <- 0
    }
    c(marker_lat, marker_lng)
  })

  output$karte_stolpi_text <- renderText({
    marker_click <- get_marker_click()
    paste0("Lat: ", marker_click[1], ", Lng.: ", marker_click[2])
  })

  output$karte_stolpi_DT <- DT::renderDT({
    marker_click <- get_marker_click()
    max_diff <- 0.0002
    stolpis %>%
      filter(lat >= marker_click[1] - max_diff & lat <= marker_click[1] + max_diff,
             lng >= marker_click[2] - max_diff & lng <= marker_click[2] + max_diff) %>%
      select(Name, Gebjahr, url)
  })


  #### Verlegungen Output ----

  output$bezirk_count <- plotly::renderPlotly({
    stolpis %>%
      mutate(Bezirk = fct_rev(Bezirk)) %>%
      count(Bezirk) %>%
      rename(Anzahl = n) %>%
      ggplot(aes(x = Bezirk, y = Anzahl)) +
      geom_col() +
      coord_flip() +
      theme(legend.position = "none") +
      labs(title = "Stolpersteine pro Bezirk", x = "", y = "")
  })

  output$verlegungsdatum_hist <- plotly::renderPlotly({
    stolpis %>%
      filter(Jahr > 1995, Jahr < 2023) %>%
      count(Jahr) %>%
      arrange(Jahr) %>%
      rename(Anzahl = n) %>%
      ggplot(aes(Jahr, Anzahl)) + geom_col() +
      labs(title = "Verlegungen pro Jahr", x = "", y = "")
      #plotly::plot_ly(x = ~ date) %>% # specify aesthetics
      #plotly::add_histogram()
  })

  output$verlegungen_pro_monat <- plotly::renderPlotly({
    stolpis %>%
      filter(Jahr > 1995, Jahr < 2023) %>%
      count(Monat) %>%
      #arrange(Jahr) %>%
      rename(Anzahl = n) %>%
      ggplot(aes(Monat, Anzahl)) + geom_col() +
      labs(title = "Verlegungen pro Monat", x = "", y = "")
    #plotly::plot_ly(x = ~ date) %>% # specify aesthetics
    #plotly::add_histogram()
  })

  output$verlegungsdatum_boxplot <- renderPlot({
    stolpis %>%
      mutate(Bezirk = fct_rev(Bezirk)) %>%
      mutate(Jahr = lubridate::year(date)) %>%
      filter(Jahr > 1995, Jahr < 2023) %>%
      ggplot(aes(date, Bezirk)) + geom_boxplot() +
      labs(title = "Verlegungen pro Jahr", x = "", y = "")
  })

  output$verlegungsdatum_pro_jahr <- plotly::renderPlotly({
    stolpis %>%
      mutate(Jahr = lubridate::year(date)) %>%
      filter(Jahr == input$verlegungen_select_year) %>%
      count(date) %>%
      rename(Anzahl = n) %>%
      ggplot(aes(date, Anzahl)) + geom_col() +
      labs(title = "Verlegungen im ausgewählten Jahr", x = "", y = "")
  })


  #### Erinnerten Output ----
  #### |> valueBox ----

  output$box_gebjahr <- renderValueBox({
    valueBox(
      tags$p("Geburtsjahr (median)", style = "font-size: 75%;"),
      tags$p(median_gebjahr, style = "font-size: 200%;"),
      color = "blue",
      width = 12#,
      #icon = icon("star-of-life")
    )
  })

  output$box_females <- renderValueBox({
    valueBox(
      tags$p("Anzahl Frauen", style = "font-size: 75%;"),
      tags$p(females_count, style = "font-size: 200%;"),
      #1887,
      color = "blue",
      width = 12#,
      #icon = icon("venus")
    )
  })

  output$box_males <- renderValueBox({
    valueBox(
      tags$p("Anzahl Männer", style = "font-size: 75%;"),
      tags$p(males_count, style = "font-size: 200%;"),
      #1887,
      color = "blue",
      width = 12#,
      #icon = icon("mars")
    )
  })

  output$box_murdered <- renderValueBox({
    valueBox(
      tags$p("Ermordet", style = "font-size: 75%;"),
      tags$p(murdered_count, style = "font-size: 200%;"),
      #1887,
      color = "blue",
      width = 12#,
      #icon = icon("rainbow")
    )
  })

  output$box_survived <- renderValueBox({
    valueBox(
      tags$p("Überlebt", style = "font-size: 75%;"),
      tags$p(survived_count, style = "font-size: 200%;"),
      color = "blue",
      width = 12#,
      #icon = icon("heart")
    )
  })

  output$box_death_age <- renderValueBox({
    valueBox(
      tags$p("Mittl. Alter bei Ermordung", style = "font-size: 75%;"),
      tags$p(mean_death_age, style = "font-size: 200%;"),
      #subtitle = "Mittelwert",
      color = "blue",
      width = 12#,
      #icon = icon("cross")
    )
  })

  #### |> Plots ----

  output$dest_sex <- plotly::renderPlotly({
    erinnerte %>%
      filter(!is.na(sex)) %>%
      mutate(destiny_type_fac = fct_recode(
        destiny_type_fac,
        Ermordet = "murdered",
        Verstorben = "deceased",
        Überlebt = "survived",
        Unbekannt = "unknown"
        ),
        sex = fct_collapse(
          sex,
          Weiblich = "w",
          Männlich = "m"
        )
      ) %>%
      count(sex, destiny_type_fac) %>%
      rename(Anzahl = n, Geschlecht = sex, Schicksal = destiny_type_fac) %>%
      ggplot(aes(Anzahl, Geschlecht, fill = Schicksal)) +
      geom_col() +
      labs(title = "Schicksale nach Geschlecht", x = "", y = "") +
      theme(legend.position = "none")
  })

  output$geburtsorte <- plotly::renderPlotly({
    erinnerte %>%
      filter(!is.na(geburtsort)) %>%
      count(geburtsort, sort = TRUE) %>%
      head(10) %>%
      rename(Anzahl = n) %>%
      mutate(Geburtsort = as_factor(geburtsort) %>%
               fct_infreq() %>%
               fct_rev()) %>%
      ggplot(aes(Geburtsort, Anzahl)) +
      geom_col() +
      coord_flip() +
      labs(title = "Die 10 häufigsten Geburtsorte", x = "", y = "") +
      theme(legend.position = "none")
  })

  output$berufe_sex <- plotly::renderPlotly({
    my_test_func(input$sex_fuer_berufe, input$top_n_berufe)
  })

  output$histo_jahrgaenge <- plotly::renderPlotly({
    erinnerte %>%
      filter(!is.na(geburtsjahr)) %>%
      count(geburtsjahr) %>%
      rename(Jahrgang = geburtsjahr, Anzahl = n) %>%
      ggplot(aes(Jahrgang, Anzahl)) +
      geom_col() +
      labs(title = "Anzahl der Jahrgänge", x = "", y = "") +
      theme(legend.position = "none")
  })


  ## Deportation ----

  output$deported_when <- plotly::renderPlotly({
    plot_deportated_where()
  })

  output$deported_where <- plotly::renderPlotly({
    plot_deportated_when()
  })

  output$murdered_when <- plotly::renderPlotly({
    plot_murdered_where()
  })

  output$murdered_where <- plotly::renderPlotly({
    plot_murdered_when()
  })

  output$deportated_per_day <- plotly::renderPlotly({
    plot_deportated_per_day()
  })

  output$unequal_dep_dest_count_both <- renderTable({
    unequal_dep_dest %>%
      count(dep_fac, dest_fac, sort = T) %>%
      head(7) %>%
      #filter(n > 11) %>%
      rename(Deportation = dep_fac,
             Ermordung = dest_fac,
             Anzahl = n)
  })

  output$unequal_dep_dest_count_dep <- renderTable({
    unequal_dep_dest %>%
      count(dep_fac, sort = T) %>%
      head(7) %>%
      #filter(n > 11) %>%
      rename(Deportation = dep_fac,
             Anzahl = n)
  })

  output$unequal_dep_dest_count_dest <- renderTable({
    unequal_dep_dest %>%
      count(dest_fac, sort = T) %>%
      head(7) %>%
      #filter(n > 11) %>%
      rename(Ermordung = dest_fac,
             Anzahl = n)
  })


  ## ||>> time_dep_murder ----

  equal_dep_dest <- reactive({
    df <- erinnerte_time_from_dep_to_murder %>%
      filter(time_dep_murder >= 0,
             as.character(dest_fac) == as.character(dep_fac)
      ) %>%
      add_count(dest_fac) %>%
      group_by(dest_fac) %>%
      mutate(median_time_dep_murder = median(time_dep_murder),
             max_time_dep_murder = max(time_dep_murder)) %>%
      ungroup()


    if (input$time_dep_murder_auf_ab == "ab") {
      if (input$time_dep_murder_sort == "n") {
        test <- df %>%
          count(dest_fac, sort = TRUE) %>%
          head(input$mindest_n_opfer) %>%
          pull()
        df <- df %>%
          #mutate(kz_rank = rank(n, ties.method = "min")) %>%
          filter(n >= test[input$mindest_n_opfer]) %>%
          mutate(dest_fac = dest_fac %>% fct_reorder(n, max))
      } else if(input$time_dep_murder_sort == "med") {
        test <- df %>%
          group_by(dest_fac) %>%
          summarise(mean = mean(median_time_dep_murder)) %>%
          arrange(desc(mean)) %>%
          head(input$mindest_n_opfer) %>%
          pull(mean)
        df <- df %>%
          mutate(dest_fac = dest_fac %>% fct_reorder(median_time_dep_murder, max)) %>%
          filter(median_time_dep_murder >= test[input$mindest_n_opfer])

      } else { ## input$time_dep_murder_sort == "max"
        test <- df %>%
          group_by(dest_fac) %>%
          summarise(mean = mean(max_time_dep_murder)) %>%
          arrange(desc(mean)) %>%
          head(input$mindest_n_opfer) %>%
          pull(mean)
        df <- df %>%
          mutate(dest_fac = dest_fac %>% fct_reorder(max_time_dep_murder, max)) %>%
          filter(max_time_dep_murder >= test[input$mindest_n_opfer])
      }
    } else { ## input$time_dep_murder_auf_ab == "auf"
      if (input$time_dep_murder_sort == "n") {
        # df <- df %>%
        #   mutate(kz_rank = rank(n, ties.method = "min")) %>%
        #   filter(kz_rank <= input$mindest_n_opfer) %>%
        #   mutate(dest_fac = dest_fac %>% fct_reorder(kz_rank, max) %>% fct_rev())
        test <- df %>%
          count(dest_fac, sort = TRUE) %>%
          tail(input$mindest_n_opfer) %>%
          pull()
        df <- df %>%
          #mutate(kz_rank = rank(n, ties.method = "min")) %>%
          filter(n <= test[1]) %>%
          mutate(dest_fac = dest_fac %>% fct_reorder(n, max) %>% fct_rev())
      } else if(input$time_dep_murder_sort == "med") {
        test <- df %>%
          group_by(dest_fac) %>%
          summarise(mean = mean(median_time_dep_murder)) %>%
          arrange(mean) %>%
          head(input$mindest_n_opfer) %>%
          pull(mean)
        df <- df %>%
          mutate(dest_fac = dest_fac %>% fct_reorder(median_time_dep_murder, min) %>% fct_rev()) %>%
          filter(median_time_dep_murder <= test[input$mindest_n_opfer])
      } else { ## input$time_dep_murder_sort == "max"
        test <- df %>%
          group_by(dest_fac) %>%
          summarise(mean = mean(max_time_dep_murder)) %>%
          arrange(mean) %>%
          head(input$mindest_n_opfer) %>%
          pull(mean)
        df <- df %>%
          mutate(dest_fac = dest_fac %>% fct_reorder(max_time_dep_murder, min) %>% fct_rev()) %>%
          filter(max_time_dep_murder <= test[input$mindest_n_opfer])
      }
    }


  })


  output$ranking_explained <- renderTable({
    # if (input$time_dep_murder_sort == "n") {
    #   p <- equal_dep_dest() %>%
    #     ggplot(aes(n, dest_fac))
    # } else if (input$time_dep_murder_sort == "med") {
    #   p <- equal_dep_dest() %>%
    #     ggplot(aes(median_time_dep_murder, dest_fac))
    # } else {
    #   p <- equal_dep_dest() %>%
    #     ggplot(aes(max_time_dep_murder, dest_fac))
    # }
    #p + geom_col()

      df <- equal_dep_dest() %>%
        count(dest_fac, median_time_dep_murder, max_time_dep_murder) %>%
        rename(Med = median_time_dep_murder,
             Max = max_time_dep_murder,
             Anzahl = n,
             Lager = dest_fac)
      if (input$time_dep_murder_auf_ab == "ab") {
        if (input$time_dep_murder_sort == "n") {
          df %>% arrange(desc(Anzahl))
        } else if(input$time_dep_murder_sort == "med"){
          df %>% arrange(desc(Med))
        } else {
          df %>% arrange(desc(Max))
        }
      } else {
        if (input$time_dep_murder_sort == "n") {
          df %>% arrange(Anzahl)
        } else if(input$time_dep_murder_sort == "med"){
          df %>% arrange(Med)
        } else {
          df %>% arrange(Max)
        }
      }

  })

  output$time_from_dep_to_murder <- plotly::renderPlotly({
    # boxplot_equal_dep_dest()
    # input$mindest_n_opfer,
    # input$time_dep_murder_auf_ab,
    # input$time_dep_murder_sort
    df <- equal_dep_dest()
    df %>%
      ggplot(aes(dest_fac, time_dep_murder)) +
      geom_boxplot() +
      coord_flip() +
      labs(title = "Anzahl der Tage zwischen Deportation und Ermordung", x = "", y = "")
  })


  #### babynames ----

  output$ggplot <- plotly::renderPlotly({
    babynames_subset <- babynames %>%
      filter(!is.na(middlename_0)) %>%
      group_by(Gebjahr) %>%
      filter(middlename_0 %in% c(input$vorname_1, input$vorname_2))
    if(nrow(babynames_subset) == 0){

    } else {
      babynames_subset %>%
        mutate(Vorname = factor(middlename_0, levels = c(input$vorname_1, input$vorname_2))) %>%
        count(Vorname) %>%
        rename(Anzahl = n, Geburtsjahr = Gebjahr) %>%
        ggplot(aes(x = Geburtsjahr, y = Anzahl, color = Vorname)) +
        geom_col() +
        facet_grid(rows = vars(Vorname)) +
        theme(legend.position = "none") +
        labs(title = "Häufigkeit der Vornamen pro Geburtsjahr")
    }
  })

  output$conditional_vorname1 <- renderText({
    if(nrow(babynames %>%
            filter(middlename_0 == input$vorname_1)) == 0){
      paste(input$vorname_1,
            "existiert im Datenset nicht. Bitte anderen Vornamen probieren.")
    } else {
      ""
    }
  })

  output$conditional_vorname2 <- renderText({
    if(nrow(babynames %>%
            filter(middlename_0 == input$vorname_2)) == 0){
      paste(input$vorname_2,
            "existiert im Datenset nicht. Bitte anderen Vornamen probieren.")
    } else {
      ""
    }
  })

  output$vornamen_sex <- plotly::renderPlotly({
    plot_vornamen_sex(input$sex_fuer_vornamen, input$vornamen_sex_n)
  })

  output$geburtsnamen <- plotly::renderPlotly({
    plot_nachnamen(input$nachnamen_oder_geburtsname, input$geburtsnamen_n)
  })


  #### next----


  # output$base <- renderPlot({
  #   image(volcano, col = thematic_get_option("sequential"))
  # })
}









# Run the application
shinyApp(ui = ui, server = server)
