pacman::p_load(shiny, sf, tmap, tidyverse)

mpsz <- st_read(dsn = "data/geospatial",
                layer = "MP14_SUBZONE_WEB_PL")
popdata <- read_csv("data/aspatial/respopagesextod2011to2020.csv")

pop <- popdata %>%
  filter(Time == 2019) %>%
  group_by(PA, SZ, AG) %>%
  summarise(`POP` = sum(`Pop`)) %>%
  ungroup()%>%
  pivot_wider(names_from=AG,
              values_from=POP) %>%
  mutate(YOUNG = rowSums(.[3:6])
         +rowSums(.[12])) %>%
  mutate(`ECONOMY ACTIVE` = rowSums(.[7:11])+
           rowSums(.[13:15]))%>%
  mutate(`AGED`=rowSums(.[16:21])) %>%
  mutate(`TOTAL`=rowSums(.[3:21])) %>%
  mutate(`DEPENDENCY` = (`YOUNG` + `AGED`)
         /`ECONOMY ACTIVE`) %>%
  select(`PA`, `SZ`, `YOUNG`,
         `ECONOMY ACTIVE`, `AGED`,
         `TOTAL`, `DEPENDENCY`)

popdata2019 <- pop %>%
  mutate_at(.vars = vars(PA, SZ),
            .funs = funs(toupper)) %>%
  filter(`ECONOMY ACTIVE` > 0)

mpsz_pop2019 <- left_join(mpsz, popdata2019,
                          by = c("SUBZONE_N" = "SZ"))



ui <- fluidPage(
  titlePanel("Choropleth Mapping System"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId="variable",
        label = "Mapping variable",
        choices = c("Young" = "YOUNG",
                    "Economy" = "ECONOMY ACTIVE",
                    "Aged" = "AGED",
                    "Dependency" = "DEPENDENCY"),
        selected = "DEPENDENCY",
        multiple = FALSE
      ),
      selectInput(
        inputId="classification",
        label = "Classification Method",
        choices = c("sd" = "sd",
                    "equal" = "equal",
                    "pretty" = "pretty",
                    "quartile" = "quartile",
                    "kmeans" = "kmeans",
                    "hclus" = "hclus",
                    "bclus" = "bclus",
                    "fisher" = "fisher",
                    "jenks" = "jenks"),
        selected = "pretty",
      ),
      sliderInput(
        inputId="classes",
        label = "Number of classes",
        min = 6,
        max = 12,
        value = c(6)
      ),
      selectInput(
        inputId="colour",
        label = "Colour scheme:",
        choices = list("blues" = "Blues",
                       "reds" = "Reds",
                       "greens" = "Greens",
                       "Yellow-Orange-Red" = "YlOrRd",
                       "Yellow-Orange-Brown" = "YlOrBr",
                       "Yellow-Green" = "YlGn",
                       "Orange-Red" = "OrRd"),
        selected = "reds"
      )
    ),
    mainPanel(
      tmapOutput("mapPlot",
                 width = "100%",
                 height = 400)
    )
  )
)

server <-function(input, output) {
  output$mapPlot <- renderTmap({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(mpsz_pop2019)+
      tm_fill(input$variable,
              n = input$classes,
              style = input$classification,
              palette = input$colour) +
      tm_borders(lwd = 0.1, alpha = 1) +
      tm_view(set.zoom.limits = c(11, 14))
  })

}

shinyApp(ui = ui, server = server)
