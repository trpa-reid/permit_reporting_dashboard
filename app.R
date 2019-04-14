library(shinythemes)
library(tidyverse)
library(shiny)
library(DT)
library(shinydashboard)
library(plotly)
  
tree <- read_csv("quarterly reporting/Q1_2019/Tree Permit Activity.csv")

current <- read_csv("quarterly reporting/Q1_2019/Detailed Workflow History.csv") %>%
  filter(PARCEL_NUMBER != "1318-26-101-011") %>% # remove the TRPA apps
  filter( !str_detect(B1_ALT_ID, ".*TMP.*")) %>% #remove the tmp apps
  filter(!Project_Description %in% c("This is a test", "this is a test of ACA", "Test" )) %>% # remove the test apps
  mutate(STATUS_DATE=as.Date(STATUS_DATE, "%m/%d/%Y"), OPEN_DATE=as.Date(OPEN_DATE, "%m/%d/%Y")) %>%
  mutate(B1_PER_CATEGORY=case_when(B1_ALT_ID %in% 
                          c("ALLC2017-1123","ALLC2017-0502","ALLC2017-0497","ALLC2016-0282", "ALLC2015-1110","ALLC2015-0647", "ALLC2015-0442","ALLC2014-0534","ALLC2014-0268","ALLC2014-0285","ALLC2014-0840","ALLC2013-0753")~
                                "Allocation Assignment",
                          TRUE ~ B1_PER_CATEGORY)) %>%  ## change these apps from allocation to allocation assignment
  mutate(B1_ALT_ID1= ifelse(str_detect(B1_ALT_ID,".*ERSP.*") & is.na(B1_PER_CATEGORY),
                            str_sub(B1_ALT_ID, 1, str_length(B1_ALT_ID)-3), B1_ALT_ID)) # remove the last two digits
IDS<- current %>% filter(!is.na(B1_PER_CATEGORY)) %>%
  distinct(B1_ALT_ID, B1_PER_CATEGORY) # get all of the unique category, app combinations
na <- current %>% filter(is.na(B1_PER_CATEGORY)) # get all of the apps that don't have a category name
nas<- na %>% left_join(IDS, by=c("B1_ALT_ID1"="B1_ALT_ID")) %>%
  mutate(B1_PER_CATEGORY=B1_PER_CATEGORY.y) %>%
  select(-c(B1_PER_CATEGORY.y, B1_PER_CATEGORY.x,B1_ALT_ID1)) %>%
  select(B1_ALT_ID, B1_PER_CATEGORY, everything()) # assign a category to all of the ones without categories


ui <- dashboardPage(skin = "black",
  dashboardHeader(title="Permit Dashboard"),
  dashboardSidebar(tags$style('.input-sm {font-size: 15px; }'),
                   width=250,dateRangeInput("date", "Select Date Range:", 
                                            end=Sys.Date(), start=Sys.Date() - 365)),
  dashboardBody(tabBox(width=12, tabPanel("Tree Permit Reporting",
                                fluidRow(
                                  valueBoxOutput("box1", width=3),
                                  valueBoxOutput("box2", width=3),
                                  valueBoxOutput("box3", width=3)),
                                fluidRow(
                                  box(width=5,dataTableOutput("table")),
                                  box(width=7,plotlyOutput("graph")))),
                        tabPanel("Current Planning Permit Reporting",
                                 fluidRow(valueBoxOutput("box4", width=3)),
                                 fluidRow(
                                   box(width=4,dataTableOutput("table1")),
                                   box(width=8,plotlyOutput("graph1"))))
                        )))
server = function(input, output) {
  dataInput <- reactive({tree %>%
      mutate(OPEN_DATE=as.Date(OPEN_DATE, "%m/%d/%Y")) %>% 
      filter(OPEN_DATE >= input$date[1] & OPEN_DATE <= input$date[2]) %>%
      filter(PARCEL_NUMBER != "1318-26-101-011"  | B1_CREATED_BY != "BBARR" |
               !Project_Description %in% c("This is a test", "this is a test of ACA", "Test" ) |
               !str_detect(B1_ALT_ID, ".*TMP.*"))
  })
  dataInput1 <- reactive({bind_rows(current %>% filter(!is.na(B1_PER_CATEGORY)), nas) %>%
      mutate(B1_PER_CATEGORY=case_when(
        B1_ALT_ID=="ERSP2008-1019"~ "Rec-Public Service",
        B1_ALT_ID== "ERSP2016-0311-01"~"Res Dwelling", #give categories to ones that didn't receive one 
        B1_ALT_ID=="SUBD2015-0092-01"~"Subdiv of Existing Structure",
        TRUE~ B1_PER_CATEGORY)) %>%
      filter(OPEN_DATE >= input$date[1] & OPEN_DATE <= input$date[2]) %>% # filter everything in the last calender year
      filter(B1_PER_CATEGORY %in% 
               c("Res Dwelling", "Residential Dwelling","Allocation Assignment", "Transfer Of Development", "Soils-Hydrology","Transfer Of Development", "V&B of Uses","Rec-Public Service","V&B of Coverage","Historic", "EIP Construction","LLA","IPES Ltd Incentive","IPES","Land Cap Verification","Grading Project","Scenic Assessment",
"Underground Tank Removal","Site Assessment","Partial Site Assessment","Res Drive-Paving","Land Cap Challenge","Shore-Lakezone","Commercial","Temporary Uses","Subdiv of Existing Structure","Signs","CEP Project", "ROW Abondonment", "Resource Management", "Mooring Permit","Verification of Mooring Permit")) %>% 
      mutate(JURISDICTION=case_when(
        PARCEL_NUMBER %in% c("1318-27-001-015","1318-22-002-108","007-190-07")~"Douglas County",
        PARCEL_NUMBER %in% c("124-041-29","130-010-13","122-591-02")~"Washoe County",
        PARCEL_NUMBER %in% c("115-060-020","115-060-021","097-130-005")~ "Placer County",
        JURISDICTION== "DOUGLAS"~"Douglas County", 
        PARCEL_NUMBER %in% c("031-236-15","023-903-01")~"City of South Lake Tahoe",
        TRUE ~ JURISDICTION)) %>%
      mutate(category=case_when(
        B1_PER_CATEGORY %in% c("Soils-Hydrology","IPES","Land Cap Verification", "Land Cap Challenge","V&B of Coverage","V&B of Uses","Site Assessment","IPES Ltd Incentive", "Partial Site Assessment") ~ "Verifications & Banking",
        B1_PER_CATEGORY %in% c("LLA","Temporary Uses","Underground Tank Removal","Subdiv of Existing Structure","Signs","Allocation Assignment","Historic") ~ "Other",
        B1_PER_CATEGORY %in% c("RESIDENTIAL","Res Dwelling","Res Drive-Paving")~"Residential Projects",
        B1_PER_CATEGORY %in% c("Commercial", "CEP Project")~ "Commercial Projects",
        B1_PER_CATEGORY %in% c("Rec-Public Service")~ "Recreation/Public Service Projects",
        B1_PER_CATEGORY %in% c("EIP Construction")~ "Environmental Improvement Construction Projects",
        B1_PER_CATEGORY %in% c("Shore-Lakezone")~ "Shorezone/Lakezone Projects",
        B1_PER_CATEGORY %in% c("Grading Project")~ "Grading Projects",
        B1_PER_CATEGORY %in% c("Transfer Of Development")~ "Transfers of Development",
        TRUE ~ "Unknown")) 
  })
  output$table <-renderDataTable({
    datatable(dataInput() %>%
                filter(B1_APPL_STATUS=="Approved") %>%
                mutate(total_trees_approved=sum(Tree_Total)) %>%
                gather(category, checked, 6:11) %>%
                mutate(checked=ifelse(is.na(checked), NA, category)) %>%
                filter(!is.na(checked) ) %>%
                group_by(checked, total_trees_approved) %>%
                summarise(trees_approved_reasons=sum(Tree_Total)) %>% 
                mutate(percent_trees_approved_of_requested=
                         paste0(round(((trees_approved_reasons/total_trees_approved)*100),1),"%")),
              options=list(dom = 'tip',pageLength=10,buttons = c('csv','pdf')),
              colnames = c("Removal Reason","Total Approved","# Removed","Percent"), rownames=F)
  })
  output$table1 <-renderDataTable({
    datatable(dataInput1() %>%
                distinct(B1_ALT_ID,category) %>%
                group_by(category) %>%
                tally(),
              options=list(dom = 'tip',pageLength=10,buttons = c('csv','pdf')),
              colnames = c("Category","Applications Received"), rownames=F)
  })
  output$graph <-renderPlotly({
   a<-dataInput() %>%
      filter(B1_APPL_STATUS=="Approved") %>%
      mutate(month_year = format(OPEN_DATE, "%Y-%m")) %>%
      group_by(month_year) %>%
      mutate(total_trees_approved=sum(Tree_Total)) %>% 
      gather(category, checked, 6:11) %>% 
      mutate(checked=ifelse(is.na(checked), NA, category)) %>% 
      filter(!is.na(checked) ) %>% 
      group_by(checked, total_trees_approved, month_year) %>%
      summarise(trees_approved_reasons=sum(Tree_Total)) %>% 
     ungroup() %>%
      mutate(percent_trees_approved_of_requested=
               trees_approved_reasons/total_trees_approved) %>% 
    ggplot(aes(x=month_year, y=percent_trees_approved_of_requested, group=checked, color=checked,
               text = paste("<b> Month:</b>", month_year, "\n" ,
                            "<b>Category:</b>", checked, "\n",
                            "<b>Percent of Removal Reasons:</b>", round(percent_trees_approved_of_requested * 100,1),"%"))) + 
      geom_line(size=1) + theme_minimal() + geom_point() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), axis.text.x= element_text(angle=45, hjust=1,size=10),legend.title=element_blank(),legend.position="bottom", axis.text.y=element_text(size=10)) + ggtitle("Permitted Tree Removals: Reasons for Removal") + scale_y_continuous(labels = scales::percent)
   ggplotly(a, tooltip="text") %>%
     layout(legend = list(orientation = "h", x = 0.0, y = -0.3)) %>%
     style(legendgroup = NULL)
  })
  output$graph1 <-renderPlotly({
    b<-dataInput1() %>%
      distinct(B1_ALT_ID,category, OPEN_DATE) %>%
      mutate(month_year = format(OPEN_DATE, "%Y-%m")) %>%
      group_by(category, month_year) %>%
      summarise(n=n()) %>% 
      ungroup() %>%
      ggplot(aes(x=month_year, y=n, group=category, color=category,
                 text = paste("<b> Month:</b>", month_year, "\n" ,
                              "<b>Category:</b>", category, "\n",
                              "<b>Applications:</b>", n))) + 
      geom_line(size=1) + 
      geom_point() + 
      theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), axis.text.x= element_text(angle=45, hjust=1,size=10),legend.title=element_blank(), legend.position = "bottom",axis.text.y=element_text(size=10)) + ggtitle("Number of Permit Applications Received")
    ggplotly(b, height= 500, tooltip="text") %>%
      layout(legend = list(orientation = "h", x = 0.0, y = -0.3)) %>%
      style(legendgroup = NULL)
  })
  output$box1<- renderValueBox({
    valueBox(dataInput() %>% summarise(total_apps=n_distinct(B1_ALT_ID)), "Tree Removal Applications Received", color="light-blue")
  })
  output$box2<- renderValueBox({
    valueBox(dataInput() %>% filter(B1_APPL_STATUS=="Approved") %>%
               summarise(total=sum(Tree_Total)), "Number of Trees Permitted for Removal", color="light-blue")
  })
  output$box3<- renderValueBox({
    valueBox(dataInput() %>% 
               mutate(online=ifelse(str_detect(B1_CREATED_BY,".*PUBLICUSER.*"), "Online", "Person")) %>%
               group_by(online) %>%
               tally() %>%
               mutate(percent=paste0(round((n/sum(n))*100,1),"%")) %>%
               select(percent), "Percent Applications Submitted Online", color="light-blue")
  })
  output$box4<- renderValueBox({
    valueBox(dataInput1() %>% distinct(B1_ALT_ID,category) %>%
               group_by(category) %>%
               tally() %>%
               summarise(total=sum(n)), "Total Applications Received", color="maroon")
  })
}
shinyApp(ui, server)

b<-bind_rows(current %>% filter(!is.na(B1_PER_CATEGORY)), nas) %>%
  mutate(B1_PER_CATEGORY=case_when(
    B1_ALT_ID=="ERSP2008-1019"~ "Rec-Public Service",
    B1_ALT_ID== "ERSP2016-0311-01"~"Res Dwelling", #give categories to ones that didn't receive one 
    B1_ALT_ID=="SUBD2015-0092-01"~"Subdiv of Existing Structure",
    TRUE~ B1_PER_CATEGORY)) %>%
  filter(OPEN_DATE >= "2018-03-03" & OPEN_DATE <= "2019-03-03") %>% # filter everything in the last calender year
  filter(B1_PER_CATEGORY %in% 
           c("Res Dwelling", "Residential Dwelling","Allocation Assignment", "Transfer Of Development", "Soils-Hydrology","Transfer Of Development", "V&B of Uses","Rec-Public Service","V&B of Coverage","Historic", "EIP Construction","LLA","IPES Ltd Incentive","IPES","Land Cap Verification","Grading Project","Scenic Assessment",
             "Underground Tank Removal","Site Assessment","Partial Site Assessment","Res Drive-Paving","Land Cap Challenge","Shore-Lakezone","Commercial","Temporary Uses","Subdiv of Existing Structure","Signs","CEP Project", "ROW Abondonment", "Resource Management", "Mooring Permit","Verification of Mooring Permit")) %>% 
  mutate(JURISDICTION=case_when(
    PARCEL_NUMBER %in% c("1318-27-001-015","1318-22-002-108","007-190-07")~"Douglas County",
    PARCEL_NUMBER %in% c("124-041-29","130-010-13","122-591-02")~"Washoe County",
    PARCEL_NUMBER %in% c("115-060-020","115-060-021","097-130-005")~ "Placer County",
    JURISDICTION== "DOUGLAS"~"Douglas County", 
    PARCEL_NUMBER %in% c("031-236-15","023-903-01")~"City of South Lake Tahoe",
    TRUE ~ JURISDICTION)) %>%
  mutate(category=case_when(
    B1_PER_CATEGORY %in% c("Soils-Hydrology","IPES","Land Cap Verification", "Land Cap Challenge","V&B of Coverage","V&B of Uses","Site Assessment","IPES Ltd Incentive", "Partial Site Assessment") ~ "Verifications & Banking",
    B1_PER_CATEGORY %in% c("LLA","Temporary Uses","Underground Tank Removal","Subdiv of Existing Structure","Signs","Allocation Assignment","Historic") ~ "Other",
    B1_PER_CATEGORY %in% c("RESIDENTIAL","Res Dwelling","Res Drive-Paving")~"Residential Projects",
    B1_PER_CATEGORY %in% c("Commercial", "CEP Project")~ "Commercial Projects",
    B1_PER_CATEGORY %in% c("Rec-Public Service")~ "Recreation/Public Service Projects",
    B1_PER_CATEGORY %in% c("EIP Construction")~ "Environmental Improvement Construction Projects",
    B1_PER_CATEGORY %in% c("Shore-Lakezone")~ "Shorezone/Lakezone Projects",
    B1_PER_CATEGORY %in% c("Grading Project")~ "Grading Projects",
    B1_PER_CATEGORY %in% c("Transfer Of Development")~ "Transfers of Development",
    TRUE ~ "Unknown"))  %>%
  distinct(B1_ALT_ID,category, OPEN_DATE) %>%
  mutate(month_year = format(OPEN_DATE, "%Y-%m")) %>%
  group_by(category, month_year) %>%
  summarise(n=n()) %>% 
  ungroup() %>%
  ggplot(aes(x=month_year, y=n, group=category, color=category)) + 
  geom_line(size=1) + 
  geom_point() + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), axis.text.x= element_text(angle=45, hjust=1,size=10),legend.title=element_blank(),legend.position="bottom", axis.text.y=element_text(size=12)) + ggtitle("Number of Permit Applications Received")
ggplotly(b) %>%
  layout(legend = list(orientation = "h", x = 0.0, y = -0.3)) %>%
  style(legendgroup = NULL)

