library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)

bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

bird_counts <- bird_counts[complete.cases(bird_counts), ]

species <- sort(unique(bird_counts$species))
year <- sort(unique(bird_counts$year))


panel1 <- bird_counts %>%  group_by(species) %>%
  summarize(how_many_counted = sum(how_many_counted)) %>%
  arrange(desc(how_many_counted)) %>% slice(1:25)

panel2 <- bird_counts %>% mutate(counts_hr = round(bird_counts$how_many_counted_by_hour, digits = 2))


ui <- fluidPage(theme = shinytheme("cerulean"),
                br(),
                fluidRow(
                  navbarPage(
                    "Birds Counted in Christmas",

                    tabPanel("Panel 1",
                             column(width = 3,
                                    selectInput(inputId = "Panel_1",
                                                label = "Select Species",
                                                choices = panel1$species,
                                                selected = c("European Starling", "Mallard",
                                                             "Rock Pigeon", "American Robin"),

                                                multiple = TRUE)),

                             h3("Comparison of most watched birds"),
                             plotOutput("myplot")),



                    tabPanel("Panel 2",
                             column(width = 4,
                                    selectizeInput(inputId = "Panel_2",
                                                   "Select Species",
                                                   choices = species)),

                             h3("Birds counted per hour since 1921"),
                             plotOutput("myplot2")),



                    tabPanel("Panel 3",
                             column(width = 4,
                                    selectizeInput(inputId = "Panel_3",
                                                   "Select Species",
                                                   choices = species)),

                             h3("Trend of birds counted since 1921"),
                             plotOutput("myplot3")),



                    tabPanel("Panel 4",
                             column(width = 4,
                                    selectizeInput(inputId = "Panel_4",
                                                   "Select Species",
                                                   choices = species)),

                             h3("Correlation between bird count and hours"),
                             plotOutput("myplot4")),


                  )

                )
)


server <- function(input, output, session) {

  data_1 <- reactive({

    req(input$Panel_1)
    df1 <- panel1 %>%
      filter(species %in% input$Panel_1) %>%
      group_by(species)

  })

  output$myplot <- renderPlot({

    ggplot(data_1(),
           aes(x = how_many_counted,
               y = species,
               fill = species)) +
      geom_col() +
      theme_bw() +
      theme(legend.position = "bottom") +
      scale_fill_brewer(palette = "Dark2") +
      theme(text = element_text(size = 20)) +
      labs(x = "Birds Counted",
           y = "Bird Name")
  })


  data_2 <- reactive({

    req(input$Panel_2)
    df2 <- panel2 %>%
      select(species, year, counts_hr) %>%
      filter(species %in% input$Panel_2) %>%
      group_by(species, year)
  })

  output$myplot2 <- renderPlot({

    g2 <- ggplot(data_2(), aes(x = year,
                               y = counts_hr,
                               fill = species))
    g2 +  geom_col() +
      theme_bw() +
      theme(legend.position = "bottom") +
      theme(text = element_text(size = 20)) +
      labs(x = "Year",
           y = "Birds counted per hr")
  })


  data_3 <- reactive({
    req(input$Panel_3)
    df3 <- bird_counts %>%
      filter(species %in% input$Panel_3) %>%
      group_by(species, year)
  })

  output$myplot3 <- renderPlot({

    g3 <- ggplot(data_3(),
                 aes(x = year,
                     y = how_many_counted,
                     color = species))
    g3 + geom_line(size = 2) +
      theme_bw() +
      theme(legend.position = "bottom") +
      theme(text = element_text(size = 20)) +
      labs(x = "Year",
           y = "Number of Birds Counted")
  })


  data_4 <- reactive({
    req(input$Panel_4)
    df4 <- bird_counts %>%
      select(species, how_many_counted, total_hours) %>%
      filter(species %in% input$Panel_4)
  })

  output$myplot4 <- renderPlot({

    g4 <- ggplot(data_4(),
                 aes(x = total_hours,
                     y = how_many_counted))
    g4 + geom_point() +
      geom_smooth(method = "lm",
                  se = FALSE) +
      theme_bw() +
      theme(legend.position = "bottom") +
      theme(text = element_text(size = 20)) +
      labs(x = "Hours",
           y = "Bird Count")
  })


    output$about <- renderUI({
      knitr::knit("about.Rmd", quiet = TRUE) %>%
        markdown::markdownToHTML(fragment.only = TRUE) %>%
        HTML()
    })
}

shinyApp(ui = ui, server = server)
