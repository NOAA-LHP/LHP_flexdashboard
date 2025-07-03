# This code runs the biosamnpling shinny app with an online interface

# The code is separated into UI (user interface) and server (back end)

#This separation of concerns helps with:

# Easier debugging
# Code reusability
# Collaboration 


# -----------  Loading libraries ------
rm(list=ls())
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(readxl)
library(plotly)
library(this.path)
library(stringr)
library(RColorBrewer)
library(Polychrome)
library(tidyr)
library(gridExtra)
# ------------------------------------


# -----------  Loading data ----------
#Seeting up a directory
root_dir <- here(..=0) #Root directory is one folder above where this script is

# 1.Loading and editing data
data <- read.csv(file.path(root_dir,"data/all_samples_12_2024_commonname_datefixed.csv"))
data<-data[complete.cases(data$ScientificName),] 
unique(data$Month)
data$Month<-as.character(data$Month)
data$Month<-factor(data$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
data <- data[-which(data$ScientificName=="Calotomus carolinus" & data$Length.cm.>80),]

data <- data[-which(data$ScientificName=="Acanthurus lineatus" & data$Length.cm.>28),]

names(data)[names(data) == 'CommonName'] <- 'CN'
str(data)
unique(sort(data$ScientificName))

summ<-data %>% 
  dplyr::group_by(ScientificName, Region, Month) %>%
  summarise(n=length(Length.cm.))
#pull BMUS and priority non-BMUS species
#test first with BMUS only
BMUS<-c("Aphareus rutilans","Aprion virescens", 
        "Caranx ignobilis",  
        "Caranx lugubris",  
        "Etelis carbunculus",  
        "Etelis coruscans",  
        "Lethrinus rubrioperculatus" , 
        "Lutjanus kasmira"  ,
        "Pristipomoides auricilla"  ,
        "Pristipomoides filamentosus"  ,
        "Pristipomoides flavipinnis"  ,
        "Pristipomoides sieboldii"  ,
        "Pristipomoides zonatus"  ,
        "Variola louti")

#Guam ecosystem species >=50n
Gcoral<-c("Caranx melampygus", 
          "Cheilinus undulatus",
          "Epinephelus fasciatus",
          "Lethrinus obsoletus",
          "Siganus punctatus")

#CNMI ecosystem species >=50n
Ccoral<-c("Acanthurus lineatus",
          "Acanthurus nigricauda",
          "Acanthurus triostegus",
          "Calotomus carolinus",
          "Caranx melampygus",
          "Cephalopholis argus",
          "Cheilinus trilobatus",
          "Cheilinus undulatus",
          "Chlorurus sordidus",
          "Kyphosus cinerascens",
          "Lethrinus obsoletus",
          "Monotaxis grandoculis",
          "Mulloidichthus vanicolensis",
          "Naso lituratus",
          "Naso unicornis",
          "Sargocentron spiniferum",
          "Sargocentron tiere",
          "Scarus ruboviolaceus", 
          "Siganus punctatus",
          "Siganus spinus",
          "Zanclus cornutus")

all<-c("Aphareus rutilans","Aprion virescens", 
       "Caranx ignobilis",  
       "Caranx lugubris",  
       "Etelis carbunculus",  
       "Etelis coruscans",  
       "Hyporthodus quernus",
       "Lethrinus rubrioperculatus" , 
       "Lutjanus kasmira",
       "Pristipomoides auricilla" ,
       "Pristipomoides filamentosus" ,
       "Pristipomoides flavipinnis",
       "Pristipomoides sieboldii" ,
       "Pristipomoides zonatus" ,
       "Variola louti",
       "Caranx melampygus",
       "Cheilinus undulatus",
       "Epinephelus fasciatus",
       "Lethrinus obsoletus",
       "Siganus punctatus",
       "Acanthurus lineatus",
       "Acanthurus nigricauda",
       "Acanthurus triostegus",
       "Calotomus carolinus",
       "Caranx melampygus",
       "Cephalopholis argus",
       "Cheilinus trilobatus",
       "Cheilinus undulatus",
       "Chlorurus sordidus",
       "Hipposcarus longiceps",
       "Kyphosus cinerascens",
       "Monotaxis grandoculis",
       "Mulloidichthys vanicolensis",
       "Naso lituratus",
       "Naso unicornis",
       "Sargocentron spiniferum",
       "Sargocentron tiere",
       "Scarus rubroviolaceus",
       "Siganus spinus",
       "Zanclus cornutus")


#subset data 
data<-data |> subset(ScientificName %in% all)
sort(unique(data$ScientificName))
str(data)
#fix common names to only one per scientific name
#unique(data$CommonName)

cname<-c("SILVERMOUTH", "UKU","GIANT TREVALLY", "BLACK JACK", "EHU SNAPPER", "ONAGA SNAPPER", "REDGILL EMPEROR", "BLUELINED SNAPPER", "YELLOWTAIL KALIKALI", "PINK OPAKAPAKA", "YELLOWEYE OPAKAPAKA","KALIKALI","GINDAI", "LYRETAIL GROUPER")

Gcoral_cn<-c("BLUEFIN TREVALLY","NAPOLEON WRASSE", "BLACKTIP GROUPER", "ORANGE-STRIPED EMPEROR","GOLD-SPOTTED RABBITFISH")

Ccoral_cn<-c("BLUEBANDED SURGEONFISH", "EPAULETTE SURGEONFISH", "CONVICT TANG", "BUCKTOOTH PARROTFISH", "BLUEFIN TREVALLY", "PEACOCK GROUPER", "TRIPLETAIL WRASSE", "NAPOLEON WRASSE","BULLETHEAD PARROTFISH", "HIGHFIN RUDDERFISH", "BIGEYE EMPEROR","YELLOWFIN GOATFISH", "ORANGESPINE UNICORNFISH", "BLUESPINE UNICORNFISH", "SABER SQUIRRELFISH", "TAHITIAN SQUIRRELFISH", "REDLIP PARROTFISH", "SCRIBBLED RABBITFISH", "MOORISH IDOL")

all_cn<-c("SILVERMOUTH",
          "UKU",
          "GIANT TREVALLY", 
          "BLACK JACK",
          "EHU SNAPPER",
          "ONAGA SNAPPER",
          "HAPU",
          "REDGILL EMPEROR", 
          "BLUELINED SNAPPER", 
          "YELLOWTAIL KALIKALI",
          "PINK OPAKAPAKA", 
          "YELLOWEYE OPAKAPAKA",
          "KALIKALI",
          "GINDAI", 
          "LYRETAIL GROUPER",
          "BLUEFIN TREVALLY",
          "NAPOLEON WRASSE",
          "BLACKTIP GROUPER", 
          "ORANGE-STRIPED EMPEROR",
          "GOLD-SPOTTED RABBITFISH",
          "BLUEBANDED SURGEONFISH",
          "EPAULETTE SURGEONFISH", 
          "CONVICT TANG", 
          "BUCKTOOTH PARROTFISH", 
          "BLUEFIN TREVALLY", 
          "PEACOCK GROUPER", 
          "TRIPLETAIL WRASSE", 
          "NAPOLEON WRASSE",
          "BULLETHEAD PARROTFISH",
          "PACIFIC LONGNOSE PARROTFISH",
          "HIGHFIN RUDDERFISH", 
          "BIGEYE EMPEROR",
          "YELLOWFIN GOATFISH", 
          "ORANGESPINE UNICORNFISH", 
          "BLUESPINE UNICORNFISH", 
          "SABER SQUIRRELFISH",
          "TAHITIAN SQUIRRELFISH", 
          "REDLIP PARROTFISH", 
          "SCRIBBLED RABBITFISH", 
          "MOORISH IDOL")


temp<-data.frame(ScientificName=all, CommonName=all_cn)
str(temp)
sort(unique(temp$ScientificName))
str(data)
sort(unique(data$ScientificName))


data<-data%>% dplyr::select(ScientificName, Sex, Length.cm., Weight.g., GonWeight, Year, Month, Region)

data<-merge(data,temp, by="ScientificName")

data<-data|> dplyr::arrange(ScientificName)


# #set up species and region lists for dropdown menu
region_list <- c("A.Samoa", "A.Samoa_unfished", "Guam", "CNMI", "CNMI_unfished", "MHI")
regions <- region_list

# Combine scientific and common names
CombinedName <- as.matrix(str_c(data$ScientificName, data$CommonName, sep = ' - '))
data <- cbind(data, CombinedName)


# EXCLUDE ROW WHEN YEAR AND MONTH ARE NAS #
data <- data %>% filter(!is.na(Year) & !is.na(Month))
# ------------------------------------

#------ UI (User Interface) ----------
# The ui defines how the app looks
# It controls user interaction. It determines what the user can see and do

# Defining UI
ui <- navbarPage("NOAA Biosampling Species Summaries",
                 
                 # ---- TAB 1: Species Summary ----
                 tabPanel("Species Summary",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h3("Choose a region, species and years"),
                                selectInput('selected_region', 'Select Region', choices = unique(data$Region)),
                                selectInput("selected_species", "Select Species:", choices = NULL),
                                checkboxGroupInput("selected_years", "Select Years:", choices = NULL),
                                #actionButton("run_button", "Run")
                              ),
                              
                              mainPanel(
                                plotOutput("length_dist_plot_selected", height = "500px"),
                                br(),
                                h4("Summary Table"),
                                tableOutput("summary_table")
                              )
                            )
                          )
                 ),
                 
                 # ---- TAB 2: About ----
                 tabPanel("About",
                          fluidPage(
                            h2("About This App"),
                            p("Appropriate collections of biological samples are crucial for understanding fish life history and population 
                            dynamics needed for sustainable fisheries management. These samples provide estimates of length at age, growth 
                            rates, longevity, aspects of reproduction (size and age at maturity, fecundity, spawning season), and mortality. 
                            This information is used to inform stock assessments, including those that use a data-poor approach. Life history 
                            information is also important to local management agencies when setting size limits and closed seasons to protect 
                            fish while they are spawning and ultimately increase fish population productivity. Finally, fish life history is 
                            expected to change in response to climate change; therefore, providing baseline information under current conditions 
                            is needed to document and understand future impacts."),
                            
                            p("Pacific Islands Fisheries Science Center, Life History Program samples insular fish species in the U.S. Pacific 
                              jurisdictions via the Commercial Fisheries Biosampling Programs (CFBS) and NOAA Life History Program research 
                              surveys. This dashboard summaries our biosampling collections to date for Guam, The Common Wealth of the 
                              Northern Mariana Islands (CNMI), and American Samoa. For more information please see the full biosamling 
                              inventory report available at: <https://repository.library.noaa.gov/view/noaa/56518>.")
                            
                          )
                 ),
                 
                 # ---- TAB 3: Life History ----
                 tabPanel("Life History",
                          fluidPage(
                            h2("Species Life History Information"),
                            
                            fluidRow(
                              column(4,
                                     tags$img(src = "images/Gindai.jpg", height = "300px")
                              ),
                              column(10,
                                     span(
                                       tags$b("Gindai ("),
                                       tags$b(tags$i("Pristipomoides zonatus")),
                                       tags$b(")")
                                     ),
                                     p("Updates to gindai life history are continuing with additional sampling and updates to the 2021 life history 
                            assessment. Ginadi are moderately long lived, with maximum age recorded of 30 years in Guam. Males obtain a larger 
                            average size and a larger asymptotic size (+3.03 cm, 1.2 inches) fork length (FL) than females. Gindai have a long 
                            spawning season and spawn often, suggesting high reproductive output."),
                                     p("Highlights", tags$br(),
                                       "-   Size ranged from 11.5 to 40.4 cm. (4.5 – 15.9 inches) FL.", tags$br(),
                                       "-   Maximum age for gindai from Guam was 30 years, but individuals in the Northern Marianas Islands have been found as old as 38 years.", tags$br(),
                                       "-   Female size at maturity was 24.0 cm FL at 3.4 years. - Spawning occurred May through September.")
                              )
                            ),
                            
                            hr(),  # Horizontal line separator
                            
                            fluidRow(
                              column(4,
                                     tags$img(src = "images/VALO_2_together.png", height = "300px")
                              ),
                              column(10,
                                     span(
                                       tags$b("Yellow-Edged Lyretail ("),
                                       tags$b(tags$i("Variola louti")),
                                       tags$b(")")
                                     ),
                                     p("Groupers (Family Epinephelidae) are important to commercial, subsistence, and recreational fisheries throughout the world. 
           Grouper species have complex life histories that make them more vulnerable to exploitation. Age, growth, and reproduction were 
           assessed from fishery-dependent samples collected around Guam from 2010 to 2017."),
                                     p("Highlights", tags$br(),
                                       "-   Size ranged from 19.4 to 49.7 cm. (7.6 – 19.6 inches) FL.", tags$br(),
                                       "-   Maximum age was 17 years, but individuals over 10 years old are rare.", tags$br(),
                                       "-   Yellow-edged lyretail is a protogynous hermaphrodite (all males are derived from females) with an average size at 
           sex change of 35.3 cm (13.9 inches) FL at 6.1 years.", tags$br(),
                                       "-   Some spawning occurs all year but there is a peak from October through March.")
                              )
                            ),
                            
                            hr(),  # Horizontal line separator
                            
                            
                            fluidRow(
                              column(4,
                                     tags$img(src = "images/PRAU_photo.png", height = "300px")
                              ),
                              column(10,
                                     span(
                                       tags$b("Yellowtail Kalekale, Goldenflag Snapper ("),
                                       tags$b(tags$i("Pristipomoides auricilla")),
                                       tags$b(")")
                                     ),
                                     p("Goldenflag jobfish are younger in fished areas (Guam and Saipan) compared to unfished areas in the Northern Marianas Islands. 
           Maximum age in the Marianas found to date is 32 years, however the oldest individual from around Guam was only 10 years. The 
           reproduction is currently being assessed. There is a long spawning season from spring through summer with spawning occurring 
           every few days during the spawning season."),
                                     p("Highlights", tags$br(),
                                       "-   Maximum size in the Marianas Islands was found to be 40.3 cm (15.9 inches) FL and maximum age was 32 years.", tags$br(),
                                       "-   The preliminary size at maturity is 23.7 cm (9.3 inches) FL.")
                              )
                            ),
                            
                            hr(),  # Horizontal line separator
                          )
                 ),
                 
                 # ---- TAB 4: Report Issues ----
                 tabPanel("Report Issues",
                          fluidPage(
                            h2("Report an Issue"),
                            p("If you encounter problems or bugs while using the app, please contact:"),
                            #tags$ul(
                            #  tags$li("Email: yourname@noaa.gov"),
                            #  tags$li("GitHub: github.com/yourrepo")
                            #),
                            p("Please use this [form](https://docs.google.com/forms/d/e/1FAIpQLSdcBZ1ALlllMbj6u1cGPRksrO_4ebY8AtLL7rzKnIuS4ISy-w/viewform?usp=sf_link) 
                              to report issues or make suggestions.")
                          )
                 )
)


# ----- Server ---------------------
# Back-end logic of your app. It is a function that takes inputs from the UI, processes them, 
# and returns outputs.

# It handles reactivity (automatic updates when inputs change).
# It does calculations, filtering, and rendering. It keeps UI and data in sync.

# Define Server
server <- function(input, output, session) {
  
  # Update SPECIES dropdown
  species_choices <- reactive({
    req(input$selected_region)  # Ensure inputs exist
    
    df <- data %>%
      filter(Region == input$selected_region) %>%
      pull(CombinedName) %>%
      trimws() %>%
      unique()
    
    sort(df)
  })
  
  observeEvent(species_choices(), {
    isolate({  # Don't trigger other observers
      new_choices <- species_choices()
      
      # If old species is still available, keep it
      if (input$selected_species %in% new_choices) {
        updateSelectInput(session, "selected_species",
                          choices = new_choices,
                          selected = input$selected_species)
      } else {
        # Otherwise select first species (or none)
        updateSelectInput(session, "selected_species",
                          choices = new_choices,
                          selected = if (length(new_choices) > 0) new_choices[1] else NULL)
      }
    })
  })
  
  # Update YEAR checkboxes
  observeEvent(input$selected_species, {
    year_choices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Year) %>%
      unique() %>%
      sort()
    
    updateCheckboxGroupInput(session, "selected_years", choices = year_choices, selected = year_choices)  # Default: All years selected
  }, ignoreInit = TRUE)
  
  
  
  # Reactive plot data that only updates on Run button click
  # filtered_plot_data <- eventReactive(input$run_button, {
  #   req(
  #     input$selected_region, 
  #     input$selected_species,
  #     input$selected_years
  #   )
  #   
  #   filtered_data <- data %>%
  #     filter(Region == input$selected_region,
  #            CommonName == input$selected_species)
  #   
  #   list(
  #     data = filtered_data,
  #     species = input$selected_species,
  #     region = input$selected_region
  #   )
  # })
  
  
  # Length Distribution for Selected Years
  output$length_dist_plot_selected <- renderPlot({
    req(input$selected_region,
        input$selected_species, input$selected_years)  # Include bin width as a required input
    
    temp <- data %>%
      filter(Region == input$selected_region, 
             CombinedName == input$selected_species, 
             Year %in% input$selected_years) 
    
    temp <- temp[complete.cases(temp$Length.cm.),]
    
    tempm<-temp
    # na.omit(temp)
    Months<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    Mn=c(1:10, 1)
    Months<-as.data.frame(Months)
    Months<-as.factor(Months$Months)
    tempm<-tempm %>%
      dplyr::group_by(Month)%>%
      dplyr::summarize(N_month=length(Length.cm.))
    maxm<-max(tempm$N_month, na.rm=TRUE)
    maxm<-if (maxm >=20) maxm else 20
    tempm<-complete(tempm, Month, fill=list(N_Month=0))
    gsi<-temp %>% 
      dplyr::mutate(GSI=(GonWeight/Weight.g.)*100)
    gsi<-gsi %>% subset(GSI<30) #removing data errors - not best practice to do it here
    #join into list
    build_list<-list(temp, tempm, gsi)
    names(build_list)<- c('temp','tempm','gsi')
    final_list<-build_list
    length_min<-min(final_list$temp$Length.cm., na.rm=TRUE)
    length_max<-max(final_list$temp$Length.cm., na.rm=TRUE)
    
    bin_width=2
    nbins <- seq(length_min - bin_width,
                 length_max + bin_width,
                 by = bin_width)
    
    p1 <- ggplot(data=temp, aes(x=Length.cm.))+geom_histogram(binwidth=2,breaks=nbins, color="black", aes(fill=Sex)) + 
      xlab("Fork Length (cm)") +  ylab("Frequency") + ggtitle("Size Distribution")+
      scale_fill_manual(values=c("red", "blue", "white", "black"))+
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")+scale_x_continuous(limits = c(length_min-2, length_max+2))
    
    
    #monthly samples
    level_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    p2<-ggplot(tempm, aes(x=factor( Month, level=(level_order)), y=tempm$N_month)) + 
      geom_bar(stat = "identity")+xlab("") +  ylab("Frequency") + ggtitle("Monthly Sample Distribution")+
      geom_hline(yintercept = 20, colour="red", linetype = "dashed")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")
    
    #GSI
    p3<-ggplot(gsi, aes(x=Length.cm., y=GSI, color=Sex)) +  xlab("Fork Length (cm)") +  ylab("GSI") + ggtitle("GSI & Fish Length")+
      geom_point(aes(colour=Sex), size=2) +
      geom_point(shape = 1,size=2, colour = "black") + scale_color_manual(values=c("red", "blue", "white", "black"))+
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")
    
    
    #Spawning Season
    gsi_f<-gsi %>% 
      subset(Sex=="F")
    #gsi_f$Month<-month(gsi_f$Date, label=TRUE)
    
    p4<-ggplot(gsi_f, aes(x=as.factor( Month), y=GSI)) + 
      geom_boxplot(fill="red") +
      xlab("") +  ylab("GSI") + ggtitle("Spawning Season")+
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")+scale_x_discrete(limits = month.abb)
    
    grid.arrange(p1,p2,p3,p4 , nrow = 2)
  })
  
  
  # output$summary_table <- renderTable({
  summary_table <- reactive({
    req(input$selected_region, input$selected_species, input$selected_years)
    
    # Filter data based on inputs
    filtered_data <- data %>%
      filter(Region == input$selected_region,
             CombinedName == input$selected_species,
             Year %in% input$selected_years)
    
    filtered_data_all_years <- data %>%
      filter(Region == input$selected_region,
             CombinedName == input$selected_species)
    
    # Summary grouped by Year
    summary_by_year <- filtered_data %>%
      group_by(Year) %>%
      summarise(
        Total_Samples = n(),
        Females = sum(Sex == "F", na.rm = TRUE),
        Males = sum(Sex == "M", na.rm = TRUE),
        Unknown_Sex = sum(is.na(Sex) | !(Sex %in% c("F", "M"))),
        Median_Length = median(Length.cm., na.rm = TRUE),
        Min_Length = min(Length.cm., na.rm = TRUE),
        Max_Length = max(Length.cm., na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(Year = as.factor(Year))
    
    # Summary for ALL years combined
    all_years_summary <- filtered_data_all_years %>%
      summarise(
        Year = "All Years",
        Total_Samples = n(),
        Females = sum(Sex == "F", na.rm = TRUE),
        Males = sum(Sex == "M", na.rm = TRUE),
        Unknown_Sex = sum(is.na(Sex) | !(Sex %in% c("F", "M"))),
        Median_Length = median(Length.cm., na.rm = TRUE),
        Min_Length = min(Length.cm., na.rm = TRUE),
        Max_Length = max(Length.cm., na.rm = TRUE)
      ) %>%
      mutate(Year = as.factor(Year))
    
    # Summary for SELECTED years combined
    selected_years_summary <- data %>%
      filter(Region == input$selected_region,
             CombinedName == input$selected_species,
             Year %in% input$selected_years) %>%
      summarise(
        Year = "Selected Years",
        Total_Samples = n(),
        Females = sum(Sex == "F", na.rm = TRUE),
        Males = sum(Sex == "M", na.rm = TRUE),
        Unknown_Sex = sum(is.na(Sex) | !(Sex %in% c("F", "M"))),
        Median_Length = median(Length.cm., na.rm = TRUE),
        Min_Length = min(Length.cm., na.rm = TRUE),
        Max_Length = max(Length.cm., na.rm = TRUE)
      ) %>%
      mutate(Year = as.factor(Year))
    
    # Combine all summaries
    final_summary <- bind_rows(summary_by_year, all_years_summary, selected_years_summary)
    
    final_summary
  })
  
  output$summary_table <- renderTable({
    summary_table()
  })
  
}
# -------------------------------------- #    

# ------ Run the application -----------------
# This function launches the app by combining the UI and server: 

shinyApp(ui = ui, server = server)

# --------------------------------------------    


# The end
