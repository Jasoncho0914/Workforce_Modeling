library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)
library(stringr)
library(shinyWidgets)
library(lubridate)
library(foreach)
library(gridExtra)
library(bizdays)

### main CODE ####
text_title_size = 15
xaxis_text = 12
yaxis_text = 12
axis_title = 15
plottitle = 18
legend_text = 15



simulate_simplified_wa <- function(df,
                                   HVAC_initial,plumber_initial,elec_initial,
                                   HVAC_nw, plumber_nw,elec_nw,
                                   daily_hours_spent, 
                                   reps = 100,
                                   working_dates = w_days,
                                   promotion_dates =  new_workers[-1],
                                   Assumptions_l = lowerbounds,
                                   Assumptions_u = upperbounds,
                                   seed = 100){
  set.seed(seed)
  firstdate_y = working_dates[which(!duplicated(format(as_date(working_dates),"%Y")))]
  HVAC_s = HVAC_initial
  plumber_s =plumber_initial
  elec_s = elec_initial
  completed_buildings = c()
  paths_working = list()
  paths_notworking = list()
  paths = list()
  completion_time = c()
  withProgress(message = 'Calculation in progress',{
    for(k in 1:reps){
      # initiallizing
      # 1) number of hours needed
      # 2) number of skilled and unskilled workers
      # 3) ordering the work to be done.
      df$hours = 
        floor(runif(nrow(df),
                    min = as.vector(Assumptions_l["hours",df$Description2]),
                    max = as.vector((Assumptions_u["hours",df$Description2] + 1))))
      df$HVAC = 
        floor(runif(nrow(df),
                    min = as.vector(Assumptions_l["HVAC",df$Description2]),
                    max = as.vector((Assumptions_u["HVAC",df$Description2] + 1))))
      df$Electrician = 
        floor(runif(nrow(df),
                    min = as.vector(Assumptions_l["Electrician",df$Description2]),
                    max = as.vector((Assumptions_u["Electrician",df$Description2] + 1))))
      df$Plumber = 
        floor(runif(nrow(df),
                    min = as.vector(Assumptions_l["Plumber",df$Description2]),
                    max = as.vector((Assumptions_u["Plumber",df$Description2] + 1))))
      df$partition = floor(runif(nrow(df),min = 1,max = 8))
      #df = foreach(pa = unique(df$partition),.combine = 'rbind')%do%(return(subset(df,partition == pa)[order(subset(df,partition == pa)$hours_needed),] ))
      
      # how many buildings are being introduced each year
      df = foreach(pa = unique(df$partition),.combine = 'rbind')%do%(
        return(subset(df,partition == pa)[sample.int(nrow(subset(df,partition == pa)),
                                                     nrow(subset(df,partition == pa)),
                                                     replace = F),]))
      
      # Intiailizing number of workers based on the assumption
      HVAC_s_c = HVAC_s
      plumber_s_c = plumber_s
      elec_s_c = elec_s 
      
      jobs = data.frame() #number of hours left on each building)
      progress = c()
      ret = c()
      working = data.frame()
      notworking = data.frame()
      
      j = 1
      # starting the job
      for(t in 1:length(working_dates)){
        # beginning of year
        if(working_dates[t] %in% firstdate_y){
          pp = which(working_dates[t] == firstdate_y)
          df_p = subset(df,partition == pp)
          jobs = rbind(jobs,
                       data.frame(status = rep(0,nrow(df_p)),
                                  HVAC = 0,
                                  Electrician = 0,
                                  Plumber = 0,
                                  description = df_p$Description2))
          progress = c(progress, df_p$hours)
        }
        if(working_dates[t] %in% promotion_dates){
          HVAC_s_c = HVAC_s_c + HVAC_nw
          plumber_s_c = plumber_s_c + plumber_nw
          elec_s_c = elec_s_c + elec_nw
        }
        
        
        # worker allocation
        #First see if there is next job
        while(TRUE){
          if(j > nrow(jobs)){
            break
          }else{
            if((HVAC_s_c >= df[j,"HVAC"]) & 
               (elec_s_c >= df[j,"Electrician"]) & 
               (plumber_s_c >= df[j,"Plumber"])){
              
              HVAC_s_c = HVAC_s_c - df[j,"HVAC"]
              elec_s_c =  elec_s_c - df[j,"Electrician"]
              plumber_s_c = plumber_s_c - df[j,"Plumber"] 
              jobs[j, "HVAC"] =  df[j,"HVAC"] 
              jobs[j, "Electrician"] =  df[j,"Electrician"] 
              jobs[j, "Plumber"] =  df[j,"Plumber"] 
              jobs[j, "status"] = 1
              j = j + 1
              
            }else{
              break
            }
          }
        }
        # for sanity check
        working = rbind(working,
                        apply(jobs[which(jobs[,"status"] == 1),
                                   c("HVAC","Electrician","Plumber")], 2, sum))
        notworking = rbind(notworking, 
                           c(HVAC_s_c, elec_s_c,plumber_s_c))
        
        #worker allocation ends
        about_to_finish = which((progress <= daily_hours_spent) & (jobs[,"status"] == 1)) # These are work that are about to finish today, 
        HVAC_s_c = HVAC_s_c + sum(jobs[about_to_finish,"HVAC"])
        elec_s_c = elec_s_c + sum(jobs[about_to_finish,"Electrician"])
        plumber_s_c = plumber_s_c + sum(jobs[about_to_finish,"Plumber"])
        
        # decrease the number of hours needed to work on the building
        progress[which(jobs[,"status"] == 1)] = progress[which(jobs[,"status"] == 1)] - daily_hours_spent
        # flag the ones about to finish that they just finished
        jobs[about_to_finish,"status"] = 2
        jobs[about_to_finish,c("HVAC","Electrician","Plumber")] = 0 
        ret = c(ret,sum(progress <= 0))
      }
      colnames(working) = c("HVAC","Electrician","Plumber")
      colnames(notworking) = c("HVAC","Electrician","Plumber")
      
      paths[[k]] = ret
      paths_working[[k]] = working
      paths_notworking[[k]] = notworking
      ct = which(ret == nrow(df))
      if(any(ct)){
        completion_time = c(completion_time,min(ct))
      }else{
        completion_time = c(completion_time,NA)
      }
      incProgress(amount = 1/reps)
    }
  })
  
  stats = data.frame(hvac_mu = apply(sapply(paths_working,function(x) x[,1]),1,mean),
                     hvac_lower = apply(sapply(paths_working,function(x) x[,1]),1,quantile,0.005),
                     hvac_upper = apply(sapply(paths_working,function(x) x[,1]),1,quantile,0.995),
                     elec_mu = apply(sapply(paths_working,function(x) x[,2]),1,mean),
                     elec_lower = apply(sapply(paths_working,function(x) x[,2]),1,quantile,0.005),
                     elec_upper = apply(sapply(paths_working,function(x) x[,2]),1,quantile,0.995),
                     plumb_mu = apply(sapply(paths_working,function(x) x[,3]),1,mean),
                     plumb_lower = apply(sapply(paths_working,function(x) x[,3]),1,quantile,0.005),
                     plumb_upper = apply(sapply(paths_working,function(x) x[,3]),1,quantile,0.995),
                     total_workers = (paths_working[[1]] + paths_notworking[[1]]),
                     buildings_mu = apply(as.data.frame(paths,col.names = 1:reps),1,mean),
                     buildings_lower = apply(as.data.frame(paths,col.names = 1:reps),1,quantile,0.01),
                     date = as_date(working_dates),
                     year = format(working_dates,"%Y"))
  return(list(stats = stats,completion_time = completion_time))
}
#######



### Data Generation
holidays <- lubridate::as_date(
  c("2024-01-01","2024-01-15","2024-02-12","2024-02-19","2024-05-27","2024-06-19","2024-07-04",
    "2024-09-02","2024-10-14","2024-11-05","2024-11-11","2024-11-28","2024-12-25",
    "2025-01-01","2025-01-20","2025-02-12","2025-02-17","2025-05-26","2025-06-19","2025-07-04",
    "2025-09-01","2025-10-13","2025-11-04","2025-11-11","2025-11-27","2025-12-25",
    "2026-01-01","2026-01-19","2026-02-12","2026-02-16","2026-05-25","2026-06-19","2026-07-03",
    "2026-09-07","2026-10-12","2026-11-03","2026-11-11","2026-11-26","2026-12-25",
    "2027-01-01","2027-01-18","2027-02-12","2027-02-15","2027-05-31","2027-06-18","2027-07-05",
    "2027-09-06","2027-10-11","2027-11-02","2027-11-11","2027-11-25","2027-12-24",
    "2027-12-31","2028-01-17","2028-02-11","2028-02-21","2028-05-29","2028-06-19","2028-07-04",
    "2028-09-04","2028-10-09","2028-11-07","2028-11-10","2028-11-23","2028-12-25",
    "2029-01-01","2029-01-15","2029-02-12","2029-02-19","2029-05-28","2029-06-19","2029-07-04",
    "2029-09-03","2029-10-08","2029-11-06","2029-11-12","2029-11-22","2029-12-25",
    "2030-01-01","2030-01-21","2030-02-12","2030-02-18","2030-05-27","2030-06-19","2030-07-04",
    "2030-09-02","2030-10-14","2030-11-05","2030-11-11","2030-11-28","2030-12-25"))
bizcal <- bizdays::create.calendar('my_calendar', 
                                   weekdays = c('saturday','sunday'),
                                   holidays = holidays,
                                   start.date = "2024-01-01",
                                   end.date =  "2030-12-31")
w_days <- bizdays::bizseq(from = '2024-01-01', 
                          to = '2030-12-31', 
                          cal = bizcal)
total_bizdays = length(w_days)
firstdate_y = w_days[which(!duplicated(format(as_date(w_days),"%Y")))+1]
Q1 = w_days[which(format(as_date(w_days),"%W") == "13" & !duplicated(format(as_date(w_days),"%Y-%W")))]
Q2 = w_days[which(format(as_date(w_days),"%W") == "26" & !duplicated(format(as_date(w_days),"%Y-%W")))]
Q3 = w_days[which(format(as_date(w_days),"%W") == "39" & !duplicated(format(as_date(w_days),"%Y-%W")))]
Q4 = w_days[which(format(as_date(w_days),"%W") == "52" & !duplicated(format(as_date(w_days),"%Y-%W")))]

new_workers = sort(c(Q1,Q2,Q3,Q4))
promotion_dates = new_workers[-1]
rm(bizcal)


generate_data_residential <- function(single,
                                      small_multiple,
                                      large_multiple){
  
  return(
    data.frame(Description2 = c(rep("Single",single),
                                rep("Small Multiple",small_multiple),
                                rep("Large Multiple",large_multiple)))
  )
}


buildings = data.frame("Single-Family Home"= 2579,
                       "Small Multi-Family Home (<= 10 units)" = 1038,
                       "Large Multi-Family Home (> 10 units)" = 625,check.names = FALSE)
Assumptions <- matrix(rep(NA,21),nrow = 7)
colnames(Assumptions) = c("Single","Small Multiple","Large Multiple")
rownames(Assumptions) = c("hours_needed","skilled_hvac","unskilled_hvac","skilled_elec",
                          "unskilled_elec", "skilled_plumb", "unskilled_plumb")

lowerbounds = Assumptions
lowerbounds[,"Single"] = c(40 ,3, 1,1,1,4,1)
lowerbounds[,"Small Multiple"] = c(120,6, 1,1,1,4,1)
lowerbounds[,"Large Multiple"] = c(200,10,1,3,1,5,1)

upperbounds = Assumptions
upperbounds[,"Single"] = c(60, 4, 3,2,1,4,2)
upperbounds[,"Small Multiple"] = c(150,10,3,2,1,5,2)
upperbounds[,"Large Multiple"] = c(250,14,3,5,1,8,2)
combined_df = cbind(lowerbounds,upperbounds)
combined_df = combined_df[,c(1,4,2,5,3,6)]
combined_df <- rbind(hours = combined_df[1,],
                     HVAC = apply(combined_df[c(2,3),],2,sum),
                     Electrician = apply(combined_df[c(4,5),],2,sum),
                     Plumber = apply(combined_df[c(6,7),],2,sum))

Worker_Summary <- rbind(Initial = floor(c(80,110,150)*0.1),
                        Increase = c(25,18,8))
colnames(Worker_Summary) <- c("HVAC","Plumbers","Electrician")

#####
# Data VIZ
E_by_year <- function(outcome_df,n_buildings,title){
  pp = ggplot(outcome_df, aes(x = date, y= buildings_mu/n_buildings*100)) + 
    geom_ribbon(aes(ymin = buildings_lower/n_buildings*100, 
                    ymax = buildings_mu/n_buildings*100,fill = "1% Lower Bound"), alpha = 0.7) +
    scale_fill_manual("",values="steelblue") + 
    facet_wrap(~year,scales = "free")+geom_line() + 
    xlab("Date") + ggtitle(paste0("Average Percentage of Buildings Electrified By Year\n",
                                  round(tail(outcome_df$buildings_mu,1)/n_buildings,4)*100,"% of Residential Buildings Electrified by 2030")) + 
    ylab("Percentage") + theme_bw() + 
    theme(strip.text.x = element_text(size = text_title_size),
          axis.text.x = element_text(size = xaxis_text, angle = 45, hjust = 1),
          axis.text.y = element_text(size = yaxis_text),
          plot.title = element_text(size = plottitle),
          legend.text = element_text(size  = legend_text),
          axis.title = element_text(size = axis_title),
          legend.position="top")
  return(pp)
}

generate_long <- function(outcome_df){
  long_df = data.frame(Means = c(outcome_df$hvac_mu, outcome_df$elec_mu, outcome_df$plumb_mu),
                       Lowers = c(outcome_df$hvac_lower, outcome_df$elec_lower, outcome_df$plumb_lower),
                       Uppers = c(outcome_df$hvac_upper, outcome_df$elec_upper, outcome_df$plumb_upper),
                       identifier = rep(c("HVAC","Electrician","Plumber"),each = nrow(outcome_df)),
                       total = c(outcome_df$total_workers.HVAC,
                                 outcome_df$total_workers.Electrician,
                                 outcome_df$total_workers.Plumber),
                       date = rep(outcome_df$date,3))
  long_df$Means = long_df$Means/long_df$total
  long_df$Lowers = long_df$Lowers/long_df$total
  long_df$Uppers = long_df$Uppers/long_df$total
  return(long_df)
}


active_worker_percentage <- function(outcome_df){
  long_df <- generate_long(outcome_df)
  pp <- ggplot(long_df, aes(x = date, y= Means*100)) + 
    geom_ribbon(aes(ymin = Lowers*100, 
                    ymax = Uppers*100,fill = "1% Lower Bound"), alpha = 0.7) +
    scale_fill_manual("",values="steelblue") + 
    facet_wrap(~identifier,scales = "free")+geom_line() + 
    xlab("Date") + ggtitle("Percentage of Total Workers working on a Project") + 
    ylab("Percentage") + theme_bw() + 
    theme(strip.text.x = element_text(size = text_title_size),
          axis.text.x = element_text(size = xaxis_text, angle = 45, hjust = 1),
          axis.text.y = element_text(size = yaxis_text),
          plot.title = element_text(size = plottitle),
          legend.text = element_text(size  = legend_text),
          axis.title = element_text(size = axis_title),
          legend.position="top")
  return(pp)
}

worker_growth <- function(outcome_df){
  long_df <- generate_long(outcome_df)
  pp <- ggplot(long_df, aes(x = date, y= total)) + 
    facet_wrap(~identifier,scales = "free")+geom_line() + 
    xlab("Date") + ggtitle("Total Number of Workers") + 
    ylab("Number of Workers") + theme_bw() + 
    theme(strip.text.x = element_text(size = text_title_size),
          axis.text.x = element_text(size = xaxis_text, angle = 45, hjust = 1),
          axis.text.y = element_text(size = yaxis_text),
          plot.title = element_text(size = plottitle),
          legend.text = element_text(size  = legend_text),
          axis.title = element_text(size = axis_title),
          legend.position="top")
  return(pp)
}

#############


ui <- navbarPage("Workforce Modelling for 2030 Electrification Goal",
                 tabPanel("Overview",
                          mainPanel(
                            htmlOutput("overview")
                            )
                          ),
                 tabPanel("Report",
                          sidebarLayout(
                            sidebarPanel(
                              # Input: Decimal interval with step value ----
                              h3(textOutput("table1_text")),
                              DTOutput("my_datatable"),
                              h3(textOutput("table2_text")),
                              DTOutput("Workers"),
                              br(),
                              actionButton("fast", "Generate Preliminary Report (~ 1 minute)"),
                              br(),
                              actionButton("slow", "Generate Detailed Report (> 10 minutes)"),
                              br()
                            ),
                            
                            mainPanel(
                              tags$head(tags$script(src = "message-handler.js")),
                              h2(textOutput("Header1")),
                              p(textOutput("overview1"), style = "font-family: 'times'; font-si50pt"),
                              p(htmlOutput("overview2"), style = "font-family: 'times'; font-si50pt"),
                              h3(textOutput("Header2")),
                              plotOutput("plot1",height = 600, width = 800),
                              p(textOutput("plot1text"), style = "font-family: 'times'; font-si50pt"),
                              br(),
                              plotOutput("plot2",height = 300, width = 800),
                              p(textOutput("plot2text1"), style = "font-family: 'times'; font-si50pt"),
                              p(textOutput("plot2text2"), style = "font-family: 'times'; font-si50pt"),
                              br(),
                              plotOutput("plot3",height = 300, width = 800),
                              p(textOutput("plot3text"), style = "font-family: 'times'; font-si50pt")
                              )
                            )
                          ),
                 tabPanel("Assumptions",
                          mainPanel(
                            h3(textOutput("slidertext")),
                            sliderInput("workinghours", "",
                                        min = 0, max = 24,
                                        value = 7.3, step = 0.1),
                            h3(textOutput("assumptionstext")),
                            DTOutput("assumptions")
                            )
                          )
)


server <- function(input, output,session) {
  #initialize a blank dataframe
  holidays <- lubridate::as_date(
    c("2024-01-01","2024-01-15","2024-02-12","2024-02-19","2024-05-27","2024-06-19","2024-07-04",
      "2024-09-02","2024-10-14","2024-11-05","2024-11-11","2024-11-28","2024-12-25",
      "2025-01-01","2025-01-20","2025-02-12","2025-02-17","2025-05-26","2025-06-19","2025-07-04",
      "2025-09-01","2025-10-13","2025-11-04","2025-11-11","2025-11-27","2025-12-25",
      "2026-01-01","2026-01-19","2026-02-12","2026-02-16","2026-05-25","2026-06-19","2026-07-03",
      "2026-09-07","2026-10-12","2026-11-03","2026-11-11","2026-11-26","2026-12-25",
      "2027-01-01","2027-01-18","2027-02-12","2027-02-15","2027-05-31","2027-06-18","2027-07-05",
      "2027-09-06","2027-10-11","2027-11-02","2027-11-11","2027-11-25","2027-12-24",
      "2027-12-31","2028-01-17","2028-02-11","2028-02-21","2028-05-29","2028-06-19","2028-07-04",
      "2028-09-04","2028-10-09","2028-11-07","2028-11-10","2028-11-23","2028-12-25",
      "2029-01-01","2029-01-15","2029-02-12","2029-02-19","2029-05-28","2029-06-19","2029-07-04",
      "2029-09-03","2029-10-08","2029-11-06","2029-11-12","2029-11-22","2029-12-25",
      "2030-01-01","2030-01-21","2030-02-12","2030-02-18","2030-05-27","2030-06-19","2030-07-04",
      "2030-09-02","2030-10-14","2030-11-05","2030-11-11","2030-11-28","2030-12-25"))
  bizcal <- bizdays::create.calendar('my_calendar', 
                                     weekdays = c('saturday','sunday'),
                                     holidays = holidays,
                                     start.date = "2024-01-01",
                                     end.date =  "2030-12-31")
  w_days <- bizdays::bizseq(from = '2024-01-01', 
                            to = '2030-12-31', 
                            cal = bizcal)
  firstdate_y = w_days[which(!duplicated(format(as_date(w_days),"%Y")))]
  
  
  v <- reactiveValues(data = {buildings})
  combined_dfv <- reactiveValues(data = {combined_df})
  workers_v <- reactiveValues(data = {Worker_Summary})
  summaryt <- reactiveValues(data = {data.frame()})
  #output the datatable based on the dataframe (and make it editable)
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE,
                  style = "auto",
                  options = list(dom = 't'),
                  rownames = FALSE,
                  escape= FALSE,
                  caption = 'Table 1: # of buildings and building type to be electrified')
  })
  output$assumptions <- renderDT({
    DT::datatable(combined_dfv$data, editable = "cell",
                  style = "auto",
                  options = list(dom = 't'),
                  rownames = c("Hours",
                               "Skilled HVAC",
                               "Skilled Electrician", 
                               "Skilled Plumber"),
                  colnames = c("Single (Min)", "Single (Max)",
                               "Small Multiple (Min)", "Small Multiple (Max)",
                               "Large Multiple (Min)", "Large Multiple (Max)"),
                  escape= FALSE,
                  caption = 'Table 3: Minimum and maximum 
                  # of hours, 
                  # of skilled HVAC workers, electricians, and 
                  plumbers required to to electrify each building type.\n
                  Note that a range of values need to be provided.')
  })
  output$Workers <- renderDT({
    DT::datatable(workers_v$data,editable = TRUE,
                 style = "auto",
                 options = list(dom = 't'),
                 escape= FALSE,
                 caption = 'Table 2: The first row represents the number of workers available for building electrification as of 2024/01/01. 
                 The value provided is the 10% of the total workforce for the city of Ithaca. 
                 The second row represents the additional number of workers to be introduced every quarter starting in the end of June, 2024.')
  })
  
  
  sliderValues <- reactive({
    input$workinghours
    })
    
  
  observeEvent(input$my_datatable_cell_edit, {
    info = input$my_datatable_cell_edit
    i = info$row
    j = info$col
    v$data[i,j+1] <- info$value
  })
  
  observeEvent(input$assumptions_cell_edit, {
    info = input$assumptions_cell_edit
    i = info$row
    j = info$col
    combined_dfv$data[i,j] <- info$value
  })
  
  observeEvent(input$Workers_cell_edit, {
    info = input$Workers_cell_edit
    i = info$row
    j = info$col
    workers_v$data[i,j] <- info$value
  })
  
  
  observeEvent(input$fast,{
    showModal(modalDialog(title = "Generating preliminary report (~10 iterations)",
                          HTML("This should take around 1 minute. <br>
                               This is to test various inputs and assumptions rather than final detailed analysis. <br>
                               The generated confidence interval should not be trusted.<br> 
                               Generate detailed report for better uncertainty quantification!"), 
                          footer=NULL))
    generated = simulate_simplified_wa(generate_data_residential(v$data[1],v$data[2],v$data[3]),
                                        HVAC_initial = workers_v$data[1,1],
                                        plumber_initial = workers_v$data[1,2],
                                        elec_initial = workers_v$data[1,3],
                                        HVAC_nw = workers_v$data[2,1],
                                        plumber_nw = workers_v$data[2,2],
                                        elec_nw = workers_v$data[2,3],
                                        daily_hours_spent = sliderValues(),
                                        reps = 10,
                                        working_dates = w_days,
                                        promotion_dates =  new_workers[-1],
                                        Assumptions_l = combined_dfv$data[,c(1,3,5)],
                                        Assumptions_u = combined_dfv$data[,c(2,4,6)],
                                        seed = sample.int(10000,1))
    removeModal()
    summaryt$data = generated
    
  })
  
  observeEvent(input$slow,{
    showModal(modalDialog(title = "Generating detailed report (~ 300 iterations)",
                            HTML("This might take 10 to 15 minutes<br> 
                                 You might want to get yourself a cup of coffee :)."),
                            footer=NULL))
    generated <- simulate_simplified_wa(generate_data_residential(v$data[1],v$data[2],v$data[3]),
                                       HVAC_initial = workers_v$data[1,1],
                                       plumber_initial = workers_v$data[1,2],
                                       elec_initial = workers_v$data[1,3],
                                       HVAC_nw = workers_v$data[2,1],
                                       plumber_nw = workers_v$data[2,2],
                                       elec_nw = workers_v$data[2,3],
                                       daily_hours_spent = sliderValues(),
                                       reps = 300,
                                       working_dates = w_days,
                                       promotion_dates =  new_workers[-1],
                                       Assumptions_l = combined_dfv$data[,c(1,3,5)],
                                       Assumptions_u = combined_dfv$data[,c(2,4,6)],
                                       seed = sample.int(10000,1))
    removeModal()
    summaryt$data = generated
  })
  output$Header1 <- renderText({
    if(is.null(summaryt$data$stats)){
      ""
      }else{
      "Summary"
    }
  })
  
  output$overview <- renderUI({
    includeHTML(path = "Overview.html")
  })
  
  
  output$overview1 <- renderText({
    if(is.null(summaryt$data$stats)){
     "" 
    }else{
      if(input$slow){
        paste0("300 iterations of simulations were performed to calculate the percentage of residential buildings 
        electrified in the given area as of year 2030. There were ",v$data[1]," single-family homes ",v$data[2],
               " multi-family homes (<=10 units) and ",v$data[3], " multi-family homes (> 10 units) to be electrified. 
               Electrification of a building require 3 primary workfoce: HVAC technicians, electricians and plumbers: ",
               workers_v$data[1,1], " HVAC technicians ", workers_v$data[1,2], " plumbers and ",
               workers_v$data[1,3], " electricians were available to start the project as of 01/01/2024. 
               The number of daily working hours were assumed to be ", sliderValues(),
               " . Each quarter (end of March, June September and December) additional ",
               workers_v$data[2,1]," HVAC technicians ", workers_v$data[2,2]," plumbers and ", workers_v$data[2,3] ,
               " electricians were assumed to have entered the workforce.")
      }else{
        paste0("10 iterations of simulations were performed to calculate the percentage of residential buildings 
        electrified in the given area as of year 2030. There were ",v$data[1]," single-family homes ",v$data[2],
               " multi-family homes (<=10 units) and ",v$data[3], " multi-family homes (> 10 units) to be electrified. 
               Electrification of a building require 3 primary workfoce: HVAC technicians, electricians and plumbers: ",
               workers_v$data[1,1], " HVAC technicians ", workers_v$data[1,2], " plumbers and ",
               workers_v$data[1,3], " electricians were available to start the project as of 01/01/2024. 
               The number of daily working hours were assumed to be ", sliderValues(),
               " . Each quarter (end of March, June September and December) additional ",
               workers_v$data[2,1]," HVAC technicians ", workers_v$data[2,2]," plumbers and ", workers_v$data[2,3] ,
               " electricians were assumed to have entered the workforce.")
      }
    }
  })
  output$overview2 <- renderText({
    if(is.null(summaryt$data$stats)){
      ""
    }else{
      if(input$slow){
        main_text1 = paste0("Out of ",length(summaryt$data$completion_time)," iterations of simulation, ",
                            sum(!is.na(summaryt$data$completion_time))," iterations (about ",
                           round(sum(!is.na(summaryt$data$completion_time))/length(summaryt$data$completion_time)*100,2),
                           "% of the iterations) resulted in the 100% electrifications of all ", 
                           sum(v$data[1], v$data[2], v$data[3]), " buildings in the area. ") 
        if(sum(!is.na(summaryt$data$completion_time))/length(summaryt$data$completion_time) >= 0.9){
        main_text = paste0("When the 100 percentage electrification goal was reached, On average, it took ", round(mean(summaryt$data$completion_time,na.rm = T)),
                           " working days were required. Thus, ", 
                           as.character(add.bizdays(lubridate::as_date("2024-01-01"),
                                                    round(mean(summaryt$data$completion_time,na.rm = T)),
                                                    bizcal)), 
                           " was the expected completion date. ") 
        }else{
          last_n = round(tail(summaryt$data$stats$buildings_mu/sum(v$data[1], v$data[2], v$data[3])*100,1),2)
          lower = round(tail(summaryt$data$stats$buildings_lower/sum(v$data[1], v$data[2], v$data[3])*100,1),2)
          main_text = paste0("This means that more workers need to be introduced to reach the electrification goal by 2030.
                             Based on the current assumption, on average ", last_n,"% of the total buildings are expected to be electrified by 2030 and
                             with 99% confidence, at least ", lower, "% of the total buildings are expected to be electrified by 2030. ")

        }
        worker_statement =  paste0("As per the number of workers, by the end of 2030, there will be additional ",
                                   tail(summaryt$data$stats$total_workers.HVAC,1)-workers_v$data[1,1],
                                   " HVAC technicians ", tail(summaryt$data$stats$total_workers.Plumber,1) - workers_v$data[1,2], 
                                   " plumbers, and ", tail(summaryt$data$stats$total_workers.Electrician,1) - workers_v$data[1,3] , " electricians introduced. ")
        paste0("<b>",main_text1,main_text,worker_statement,"</b>")
      }else{
        main_text = paste0("Out of ",length(summaryt$data$completion_time)," iterations of simulation, ",
                           sum(!is.na(summaryt$data$completion_time))," iterations resulted in the 100% electrifications of all ", 
                           sum(v$data[1], v$data[2], v$data[3]), " buildings in the area. However, the results are based on a small 
                           number of iterations; please generate the detailed report for more in-depth analysis. ")
        
        worker_statement =  paste0("As per the number of workers, by the end of 2030, there will be additional ",
                                   tail(summaryt$data$stats$total_workers.HVAC,1)-workers_v$data[1,1]," HVAC technicians ", 
                                   tail(summaryt$data$stats$total_workers.Plumber,1) - workers_v$data[1,2], 
                                   " plumbers, and ", tail(summaryt$data$stats$total_workers.Electrician,1) - workers_v$data[1,3] , " electricians introduced. ")
        paste0("<b>",main_text,worker_statement,"</b>")
          
      }
    }
    
  })
  output$Header2 <- renderText({
    if(is.null(summaryt$data$stats)){
      ""
    }else{
      "Figures"
    }
  })
  output$plot1text <- renderText({
    if(is.null(summaryt$data$stats)){
      ""
    }else{
      tempp = summaryt$data$stats[order(summaryt$data$stats$date,decreasing = T),]
      lastday_df = tempp[which(!duplicated(format(sort(summaryt$data$stats$date,decreasing = T),"%Y"))),]
      mu_last = sort(lastday_df$buildings_mu)
      lower_last = sort(lastday_df$buildings_lower)
      
      figures = paste0(paste0(round(
        mu_last/sum(v$data[1], v$data[2], v$data[3])*100,2)[-7],
        "%",collapse  = ", "), ", and ",round(mu_last/sum(v$data[1], v$data[2], v$data[3])*100,2)[7], "%")
      
      figures_l = paste0(paste0(round(
        lower_last/sum(v$data[1], v$data[2], v$data[3])*100,2)[-7],
        "%",collapse  = ", "),", and ",round(lower_last/sum(v$data[1], v$data[2], v$data[3])*100,2)[7], "%")
      
      paste0("The figure shows the time series of percentage of the buildings electrified each each year from 2024-01-01 and 2030-12-31.
      From 2024 to 2030, ",figures, " of the buildings were electrified by the end of the year respectively. With 99% confidence, the
      electrification completion were at least, ", figures_l, " respectively from 2024 to 2030. ")
    }
  })
  output$plot2text1 <- renderText({
    if(is.null(summaryt$data$stats)){
      ""
    }else{
      starting_statement = paste0(
      "The figure shows the percentage of workers on a building electrification project over time. 
      Ideally, we would want all three workforce to be close to 100 (~90) percent, which would suggest that close to all workforce being 
      introduced every quarter are being utilized for building electrification. When the 100% building electrification is reached,
      all three perecentages would reach and stay on 0%. ")
      starting_statement
    }
  })
  
  output$plot2text2 <- renderText({
    if(is.null(summaryt$data$stats)){
      ""
    }else{
      HVAC_statement = paste0("On average ", 
                              round(mean(summaryt$data$stats$hvac_mu/summaryt$data$stats$total_workers.HVAC)*100,2),"% of HVAC 
             technicians were used, and with 99% confidence, the percentage of HVAC technicians on a building electrification project 
      were between ", 
                              round(mean(summaryt$data$stats$hvac_lower/summaryt$data$stats$total_workers.HVAC)*100,2), "% and ",
                              round(mean(summaryt$data$stats$hvac_upper/summaryt$data$stats$total_workers.HVAC)*100,2), "%. ")
      
      plumb_statement = paste0("For plumbers, the average was ", 
                               round(mean(summaryt$data$stats$plumb_mu/summaryt$data$stats$total_workers.Plumber)*100,2),
                               "% with 99% confidence interval of (",
                               round(mean(summaryt$data$stats$plumb_lower/summaryt$data$stats$total_workers.Plumber)*100,2), "%, ",
                               round(mean(summaryt$data$stats$plumb_upper/summaryt$data$stats$total_workers.Plumber)*100,2), "%). ")
      
      elec_statement = paste0("For electricians, the average was ", 
                              round(mean(summaryt$data$stats$elec_mu/summaryt$data$stats$total_workers.Electrician)*100,2),
                              "% with 99% confidence interval of (",
                              round(mean(summaryt$data$stats$elec_lower/summaryt$data$stats$total_workers.Electrician)*100,2), "%, ",
                              round(mean(summaryt$data$stats$elec_upper/summaryt$data$stats$total_workers.Electrician)*100,2), "%). ")
      
      
      avv = c("Plumber" = mean(summaryt$data$stats$plumb_mu/summaryt$data$stats$total_workers.Plumber),
              "Electrician" = mean(summaryt$data$stats$elec_mu/summaryt$data$stats$total_workers.Electrician),
              "HVAC Technician" = mean(summaryt$data$stats$hvac_mu/summaryt$data$stats$total_workers.HVAC))
      
      highest = names(sort(avv,decreasing = T))[[1]]
      lowest = names(sort(avv,decreasing = T))[[3]]
      all_90 = all(avv>=0.8)
      if(all_90){
        last_statement = paste0("Note that on average, all 3 workfoce has over 80% utilization.")
      }else{
        last_statement = paste0(highest, " has the highest while ", lowest, " has the lowest utilization. We would want to
                                either increase the number of ", highest, " being introduced to the workforce or decrease the 
                                number of ",lowest, "in order for the optimal utilization of the workforce. ")
      }
      paste0(HVAC_statement,plumb_statement,elec_statement,last_statement)
    }
  })
  
  output$table1_text <- renderText({
    "Building Counts"
  })
  output$table2_text <- renderText({
    "Initial and Additional Workers"
  })
  
  output$plot3text <- renderText({
    if(is.null(summaryt$data$stats)){
      ""
    }else{
      paste0("The figure shows the total number of electricians, HVAC techincians, and plumbers over time. The number of workers
             are assumed to grow every quarter, starting on the last working day of June, 2024. By the end of 2030, there will be additional ",
             tail(summaryt$data$stats$total_workers.HVAC,1)-workers_v$data[1,1]," HVAC technicians ",
             tail(summaryt$data$stats$total_workers.Plumber,1) - workers_v$data[1,2], " plumbers, and ", 
             tail(summaryt$data$stats$total_workers.Electrician,1) - workers_v$data[1,3] , " electricians introduced. ")
    }
  })
  output$slidertext <- renderText({
    "Daily Working Hours"
  })
  
  output$assumptionstext <- renderText({
    "Workforce Need and Time Commitment"
  })
  
  

  
  
  
  output$plot1 <- renderPlot({
    if(is.null(summaryt$data$stats)){
      ggplot()+theme_bw() + theme(axis.line = element_line(colour = "white"),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_blank(),
                                  panel.background = element_blank()) 
      
    }else{
      E_by_year(summaryt$data$stats,sum(v$data[1],v$data[2],v$data[3]))
    }
  },height = 600, width = 800)
  output$plot2 <- renderPlot({
    if(is.null(summaryt$data$stats)){
      ggplot()+theme_bw() + theme(axis.line = element_line(colour = "white"),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_blank(),
                                  panel.background = element_blank()) 
      
    }else{
      active_worker_percentage(summaryt$data$stats)
    }
  },height = 300, width = 800)
  output$plot3 <- renderPlot({
    if(is.null(summaryt$data$stats)){
      ggplot()+theme_bw() + theme(axis.line = element_line(colour = "white"),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_blank(),
                                  panel.background = element_blank()) 
      
    }else{
      worker_growth(summaryt$data$stats)
    }
  },height = 300, width = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)