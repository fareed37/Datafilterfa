source("./functions.R")

shinyServer(function(input, output, session) { 
  
  
  #--------------------------------------------------------------UI-------------------------------------------------#  
  #Date UI
  output$dateRange<-renderUI({
    dateRangeInput('daterng',label = shiny::HTML("<p> <span style='color: black'>Date Range</span></p>"),
                   start = min(cp_Spend[,Date_var]), end = max(cp_Spend[,Date_var]),
                   min = min(cp_Spend[,Date_var]), max = max(cp_Spend[,Date_var]), sep='to',
                   #format='M-dd-yy'
    )
  })
  
  
  
  #Filtering  UI
  observeEvent(input$submitplan, {
    #observe({
    if(is.null(input$heir1)) return(NULL);
    updateSelectInput(session,"heir1",choices = Granularity_names,selected=input$heir1)
    updateSelectInput(session,"heir2",choices = c("Weekly", "Monthly", "Yearly","Total"),selected=input$heir2)
    updatePickerInput(session,"Activity_list",choices = Activity_list_main,selected = input$Activity_list)
    updatePickerInput(session,"Channel_list",choices = Channel_list_main,selected = input$Channel_list)
  })
  
  observeEvent(input$RC_granualrity, {
    #observe({
    if(is.null(input$RC_granualrity)) return(NULL);
    updatePickerInput(session,"RC_granualrity_susbet:",choices = c(unique((simulated_data()[[input$RC_granualrity]]))),selected = input$RC_granualrity_susbet)
    #updatePickerInput(session,"Channel_list",choices = Channel_list_main,selected = input$Channel_list)
  })
  


  observeEvent(input$heir1, {
    print(input$heir1)
  if (length(input$heir1)>1 ){
    output$granulairty_filter1<-renderUI({
    pickerInput("Activity_list", 
                shiny::HTML("<p> <span style='color: black'>Activity</span></p>"),
                choices =Activity_list_main, options = list(`actions-box` = TRUE,size = 9),multiple = T,
                choicesOpt = list(style = rep(("color: black; background: white; font-weight: bold;"),length(Activity_list_main))))
    })
    output$granulairty_filter2<- renderUI({
      pickerInput("Channel_list", 
                  shiny::HTML("<p> <span style='color: black'>Channels</span></p>"),
                  choices =Channel_list_main, options = list(`actions-box` = TRUE,size = 9),multiple = T,
                  choicesOpt = list(style = rep(("color: black; background:white ; font-weight: bold;"),length(Channel_list_main))))
    })
  }else if(input$heir1 == 'Channel'){
    output$granulairty_filter1<- renderUI({
    pickerInput("Channel_list", 
                shiny::HTML("<p> <span style='color: black'>Channels</span></p>"),
                choices =Channel_list_main, options = list(`actions-box` = TRUE,size = 9),multiple = T,
                choicesOpt = list(style = rep(("color: black; background: white; font-weight: bold;"),length(Channel_list_main))))
    })
  } else if (input$heir1 == 'Activity_Type'){
    output$granulairty_filter2<-renderUI({
    pickerInput("Activity_list", 
                shiny::HTML("<p> <span style='color: black'>Activity</span></p>"),
                choices =Activity_list_main, options = list(`actions-box` = TRUE,size = 9),multiple = T,
                choicesOpt = list(style = rep(("color: black; background: white; font-weight: bold;"),length(Activity_list_main))))
    })
  }
  })
  
  
  ##creating Summary_Table for Data tabs
  Summary_Table<-eventReactive(input$submitplan, {
    # browser()
    #Summary_Table<-reactive({
    if(input$heir2!="Total"){
      vars_roll_up<-c(input$heir2,input$heir1,numeric_var)
      vars_roll_up_lvl<-c(input$heir2,input$heir1)
    }else {vars_roll_up<-c(input$heir1,numeric_var)
    vars_roll_up_lvl<-c(input$heir1)}
    print(input$Activity_list)
    print(input$Channel_list)
    
    #vars_roll_up_master<-c(vars_roll_up_lvl,"Powered_Revenue")
    if(!is.null(input$Channel_list))
    {cp_Spend<-cp_Spend[cp_Spend$Channel %in% input$Channel_list, ]} ##hardcoded
    if(!is.null(input$Activity_list))
    {cp_Spend<-cp_Spend[cp_Spend$Activity_Type %in% input$Activity_list, ]} ##hardcoded
    
    cp_Spend1<-cp_Spend[cp_Spend[,Date_var]>=input$daterng[1] & cp_Spend[,Date_var]<=input$daterng[2],vars_roll_up]
    #master1<-master[master$reportedDate>=input$daterng[1] & master$reportedDate<=input$daterng[2],c(vars_roll_up_master)]
    Summary_Table_spend<-cp_Spend1 %>%
      dplyr::group_by_(.dots=vars_roll_up_lvl) %>%
      dplyr::summarise_all(funs(sum(.,na.rm = T)))
    
    # Summary_Table_master<-master1 %>%
    #   dplyr::group_by_(.dots=vars_roll_up_lvl) %>%
    #   dplyr::summarise_all(funs(sum(.,na.rm = T)))
    
    Summary_Table<-Summary_Table_spend
    #setnames(Summary_Table,old=c("Contribution","spend"), new=c("Contribution","Spend"))
    cp_Spend<-cp_Spend[rowSums(cp_Spend[,numeric_var])!=0,]
    Summary_Table<-Summary_Table[rowSums(Summary_Table[,numeric_var])!=0,]
    if(input$heir2!="Total"){
      Summary_Table<- Summary_Table %>%
        dplyr::arrange_((input$heir2))
      Summary_Table<-Summary_Table[,c(input$heir2,input$heir1,numeric_var)]
    }else {Summary_Table<-Summary_Table[,c(input$heir1,numeric_var)]
    }
    Summary_Table[,numeric_var]<-round(Summary_Table[,numeric_var],2)
    Summary_Table$Contri_Perc<-round(Summary_Table$Contribution/sum(Summary_Table$Contribution),4)
    
    #Summary_Table<-Summary_Table[Summary_Table$channel %in% input$Channel_list & Summary_Table$Activity_Type %in% input$Activity_list, ]
    Summary_Table$Selected<-TRUE
    Summary_Table<-unique(Summary_Table[(Summary_Table$Selected || !(is.na(Summary_Table$Spend))),])
    cat("Head of Summary Table: \n ")
    print(head(Summary_Table))
    Summary_Table
  })
  
  revals <- reactiveValues();
  
  observeEvent(input$submitplan ,{
    input$Summary_Table_edit
    Summary_Table<-Summary_Table()
    names(Summary_Table)[names(Summary_Table)=="Contribution"]="Application_Count"
    #names(Summary_Table)[names(Summary_Table)=="Revenue_Calc"]="Revenue"
    revals$Summary_Table <- Summary_Table
  
    edits <- data.frame(Row = c("", ""), Column = (c("", "")), Value = (c("", "")), stringsAsFactors = FALSE);
    rownames(edits) <- c("Fail", "Success");
    
    revals$edits <- edits;
    
    output$Summary_Table <- renderD3tf({
      validate(need(input$submitplan > 0,""))
      validate(need(input$submitplan > 0 & nrow(Summary_Table()),""))
      
      Summary_Table$Application_Count<-format(as.numeric(Summary_Table$Application_Count), nsmall=2, big.mark=",")
      #Summary_Table$Powered_Revenue<-format(as.numeric(Summary_Table$Powered_Revenue), nsmall=2, big.mark=",")
      
      Summary_Table$Spend<-paste0('$',format(as.numeric(Summary_Table$Spend), nsmall=2, big.mark=","))
      Summary_Table$Contri_Perc<-paste0(Summary_Table$Contri_Perc*100,'%')
      #Summary_Table$Spend<-comma(Summary_Table$Spend)
      
      # Summary_Table$Simulated_Revenue<-format(as.numeric(Summary_Table$Simulated_Revenue), nsmall=2, big.mark=",")
      # Summary_Table$Simulated_Spend<-format(as.numeric(Summary_Table$Simulated_Spend), nsmall=2, big.mark=",")
      #names(Summary_Table)[names(Summary_Table)=="Contribution"]="Application_Count"
      
      #cat("Head of Actual Spend of summary Table: \n", Summary_Table$Spend )
      
      # data_current_rval=revals$Summary_Table
      # Summary_Table$Selected=revals$Summary_Table$Selected
      # Summary_Table$contri_perc<-percent(Summary_Table$Application_Count/sum(Summary_Table[Summary_Table$Selected,]$Application_Count))
      
      print_columns1=names(Summary_Table)[!names(Summary_Table) %in% c("Powered_Revenue")]
      Summary_Table=Summary_Table[,print_columns1]
      table_Props <- list(
        # appearence
        btn_reset = TRUE,
        btn_reset_text = "Clear",
        # behaviour
        on_change = TRUE,
        #btn = FALSE,
        enter_key = TRUE,
        on_keyup = TRUE,
        #on_keyup_delay = 1500,
        highlight_keywords = TRUE,
        loader = TRUE,
        loader_text = "Filtering data...",
        case_sensitive=F,
        # paging
        paging = FALSE,
        # grid=T,
        # grid_layout=T,
        #alternate_rows=T,
        rows_counter =T
        # ,
        # col_number_format= list(NULL,NULL,'US','US','US',NULL)
        #rows_counter_text="Total Obs: ",
        #themes='Stylesheet01'
        #grid_width='100%',
        # grid_height='400px'
        #responsive=T,
        #grid_enable_cols_resizer=T
      );

      extensions <-  list(
        list(name = "sort"));
            
      d3tf(
        Summary_Table,
        table_Props,
        enableTf = TRUE,
        #edit ="",
        showRowNames = FALSE,
        checkBoxes =paste0("col_",(ncol(Summary_Table)-1)) , #"Selected"
        edit = paste0("col_",(ncol(Summary_Table)-1)),
        filterInput = TRUE,
        selectableRows = "multi",
        #colNames = list(start_date="Start Date",end_date="End Date",TYPE="Type",Variable.Name="Variable Name"),
        selectableRowsClass = "info",
        tableStyle = "table table-bordered table-condensed",
        extensions = extensions
      );
      
     
    })
    
       output$edits <- renderTable({
      if(is.null(revals$edits)) return(invisible());
      revals$edits;
    });
   
  
  })
  

  
  observe({
    if(is.null(input$Summary_Table_filter)) return(NULL);
    revals$rowIndex <- unlist(input$Summary_Table_filter$validRows);

    filterSettings <-input$Summary_Table_filter$filterSettings;
    tmp <- lapply(filterSettings, function(x) data.frame(Column = x$column, Filter = x$value, stringsAsFactors = FALSE));
    revals$filters <- do.call("rbind", tmp);
  })
  
  
  observe({
    if(is.null(input$Summary_Table_edit)) return(NULL);
    edit <- input$Summary_Table_edit;
    #browser()
    isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col)+1;
      val <- edit$val;
      #reject edit
      #if (col==(ncol(Summary_Table())-1)){
        if ( sum(is.na(revals$Summary_Table[row, c(1,2)])) ){
          oldval <- revals$Summary_Table[row, col]
          rejectEdit(session, tbl = "Summary_Table", row = row, col = col, id = id, value = oldval)
          return(NULL)}
      #}
      
      # accept edits
      #if(col==(ncol(Summary_Table())-1)) {
        if( !is.logical(val)){
          val<-as.logical(revals$Summary_Table[row, col]);
          #print(class(val))
        }else{revals$Summary_Table[row, col] <- val;}
        #}
      # confirm edits
      confirmEdit(session, tbl = "Summary_Table", row = row, col = col, id = id,value = val);
      revals$edits["Success", "Row"] <- row;
      revals$edits["Success", "Column"] <- col;
      revals$edits["Success", "Value"] <- val;
    })
    
  })
  
  
  output$Spend_Input_nav <- renderValueBox({
    validate(need(input$submitplan > 0,""))
    data_current_rval=revals$Summary_Table
    data_current=unique(data_current_rval[data_current_rval$Selected==TRUE,1:(ncol(data_current_rval)-1)])
    
    valueBox(tags$p(dollar(sum(data_current[,numeric_var[3]])/1000000, suffix = "M", prefix = "AU$ "), style = "font-size: 60%;")
             , "Total Spend                               ", 
             # icon = icon("coins"),
             color = "yellow"
    )
    
  })
  
  output$Contribution_Input_nav <- renderValueBox({
    validate(need(input$submitplan > 0,""))
    data_current_rval=revals$Summary_Table
    data_current=unique(data_current_rval[data_current_rval$Selected==TRUE,1:(ncol(data_current_rval)-1)])
    
    valueBox(tags$p(dollar(sum(data_current$Application_Count)/1000000, suffix = "M",prefix = ""), style = "font-size: 60%;")
             , "Total Acc. Opened", 
             # icon = icon("coins"),
             color = "yellow"
    )
    
  })
  
  output$CPA_Input_nav <- renderValueBox({
    validate(need(input$submitplan > 0,""))
    data_current_rval=revals$Summary_Table
    data_current=unique(data_current_rval[data_current_rval$Selected==TRUE,1:(ncol(data_current_rval)-1)])
    valueBox(tags$p(round(sum(data_current$Spend)/sum(data_current$Application_Count),2), style = "font-size: 60%;")
             , "CPA",
             # icon = icon("coins"),
             color = "yellow"
    )
  })
  
  
  output$Revenue_Input_nav <- renderValueBox({
    validate(need(input$submitplan > 0,""))
    data_current_rval=revals$Summary_Table
    data_current=unique(data_current_rval[data_current_rval$Selected==TRUE,1:(ncol(data_current_rval)-1)])
    
    valueBox(tags$p(dollar(sum(data_current$Revenue_Calc)/1000000, suffix = "M",prefix = "AU$ "), style = "font-size: 60%;")
             , "Total RAR", 
             # icon = icon("coins"),
             color = "yellow"
    )
    
  })
  
  output$ROMI_Input_nav <- renderValueBox({
    validate(need(input$submitplan > 0,""))
    data_current_rval=revals$Summary_Table
    data_current=unique(data_current_rval[data_current_rval$Selected==TRUE,1:(ncol(data_current_rval)-1)])
    valueBox(tags$p(round(sum(data_current$Revenue_Calc)/sum(data_current$Spend),2), style = "font-size: 60%;")
             , "ROMI",
             # icon = icon("coins"),
             color = "yellow"
    )
  })
  
  
   observeEvent(input$export_button, {
    Summary_Table_rval<-revals$Summary_Table
    final<<-unique(Summary_Table_rval[Summary_Table_rval$Selected==TRUE,1:(ncol(Summary_Table_rval)-1)])
    # print("head of  Summary table just before  exporting:\n")
    # print(head(final))
    write.csv(final, paste0("./Intermediate/","Optimisation_Table",input$heir2,"_",Sys.Date(),".csv"),row.names = F,na="")
    })

   values_to_reject=0
   
 ##################################################################################################################################  # Deepak's server.R bit
   
   myData <- reactive({
     inFile <- input$base_scenario
     # browser()
     if(!(input$export_button%in% values_to_reject)){
       Summary_Table_rval<-revals$Summary_Table
       output_ranjan=unique(Summary_Table_rval[Summary_Table_rval$Selected==TRUE,])
       
       data=output_ranjan[complete.cases(output_ranjan), ]
       values_to_reject<<-input$export_button
       #print(input$export_button)
     }else if(is.null(inFile)) {return(NULL)} else {
     data <- read.csv(inFile$datapath)}
     names(data)[names(data)=="Application_Count"]="Contribution" ##renmed back again to ensure at the backend
     
     subset_columns=names(data)[!(names(data) %in% c("Weekly", "Monthly", "Yearly","Selected","Contri_Perc"))]
     subset_columns_var_lvl=names(data)[!(names(data) %in% c("Weekly", "Monthly", "Yearly","Selected","Contri_Perc",numeric_var))]
     cat("subset_columns_var_lvl: \n",subset_columns_var_lvl)
     cat("subset_columns: \n", subset_columns)
     data<-data[,c(subset_columns)] %>%
           dplyr::group_by_(.dots=subset_columns_var_lvl) %>%
           dplyr::summarise_all(funs(sum(.,na.rm = T)))
     
     
     # cat("dimesnion of data : ", dim(data))
     # print(head(data))
     data
   })
   
   
   
   output$SBO_Budget_selector <- renderUI({
     selectInput("SBO_Budget_selector", "Select the Granularity:", as.list(names(myData())[lapply(myData(),class)=="character"]))
   })
   
   output$GBO_Budget_selector <- renderUI({
     selectInput("GBO_Budget_selector", "Select the Granularity:", as.list(names(myData())[lapply(myData(),class)=="character"]))
   })
   
   
   
   ntext <- eventReactive(input$simulate_button, {
     input$simulated_spend
   })
   old_export_val=0
   #observeEvent((input$export_button >= 0 || input$simulate_button >=0) ,{
   simulated_data=eventReactive( c(input$simulate_button,input$export_button), {
     cat("input$simulate_button: \t",input$simulate_button)
     cat("input$export_button: \t", input$export_button)
     #reactive({
     uploaded_file=myData()
     uploaded_file$Power=uploaded_file$Powered_Revenue/uploaded_file$Contribution
     uploaded_file$Coef=uploaded_file$Contribution/(uploaded_file$Spend^uploaded_file$Power)
     uploaded_file$Proportion=uploaded_file$Spend/sum(uploaded_file$Spend)
     uploaded_file$Multiplier=uploaded_file$Revenue_Calc/uploaded_file$Contribution
     
     if((input$export_button > old_export_val)){
       uploaded_file$Simulated_Spend=uploaded_file$Spend
       old_export_val<<-input$export_button
     }else(
       uploaded_file$Simulated_Spend=uploaded_file$Proportion*input$simulated_spend)
     uploaded_file$Simulated_Revenue=uploaded_file$Coef*(uploaded_file$Simulated_Spend^uploaded_file$Power)
     uploaded_file$Simulated_Revenue_Calc=uploaded_file$Multiplier*uploaded_file$Simulated_Revenue #19052021
     uploaded_file$Contri_Perc<-round(uploaded_file$Simulated_Revenue/sum(uploaded_file$Simulated_Revenue),4)
     uploaded_file$LB<-input$lower_bound
     uploaded_file$UB<-input$upper_bound
     
     print(head(uploaded_file))
     uploaded_file
     
   })
   
   output$RC_selector <- renderUI({
     selectInput("RC_granualrity", "Response Curve Granularity:", as.list(names(myData())[lapply(myData(),class)=="character"]))
   })
   #observeEvent(input$RC_granualrity, {
     output$RC_selector_subset<-renderUI({
       pickerInput("RC_granualrity_susbet", "Response Curve Granularity Subset:", choices =c(unique((simulated_data()[[input$RC_granualrity]]))), options = list(`actions-box` = TRUE,size = 9),multiple = T
                   #choicesOpt = list(style = rep(("color: black; background: white; font-weight: bold;"),length(Activity_list_main)))
                                     )
     })
   #})
   
   output$RC_plot <- renderPlot({
     #browser()
     if(is.null(input$RC_granualrity)){
     first_one=names(myData())[lapply(myData(),class)=="character"][2]}
     else{first_one=input$RC_granualrity}
     cat("input$RC_granualrity: ", input$RC_granualrity)
     rc_input=as.data.frame(simulated_data()[,c(first_one,"Powered_Revenue","Spend","Contribution")] %>%  group_by_(first_one) %>% summarise_all(funs(sum)))
     setnames(rc_input,"Contribution","Revenue")
     cat("rc_input: \n")
     print(rc_input)
     cat("input$RC_granualrity_susbet: ",input$RC_granualrity_susbet)
     if(!is.null(input$RC_granualrity_susbet)){
     if(first_one=="Channel"){
     rc_input=rc_input[rc_input$Channel %in% input$RC_granualrity_susbet,]
     }else if(first_one=="Account"){
       rc_input=rc_input[rc_input$Account %in% input$RC_granualrity_susbet,]
     }else if(first_one=="Activity_Type") {
       rc_input=rc_input[rc_input$Activity_Type %in% input$RC_granualrity_susbet,]}
     }else{print("RC_granualrity_susbet is getting NULL Value")}
     print(rc_input)
     rc_output=response_curves_data(rc_input,first_one)
     print(rc_output)
     ggplot(rc_output, aes_string(x="Spend", y="Revenue", group = first_one, colour = first_one)) +
       geom_line() +
       geom_point(data=rc_output[rc_output$Point==10,c("Spend","Revenue",first_one) ], size = 3,shape=17)
   })
   
   output$SBO_rev_level <- renderPlotly({
     # data_for_chart<<-SBO_opti_data()
     if(is.null(input$SBO_Budget_selector)){
       first_one=names(myData())[lapply(myData(),class)=="character"][1]}
     else{first_one=input$SBO_Budget_selector}
     A=SBO_opti_data()[,c(first_one,"Simulated_Revenue","Optimised_Revenue")] %>%  group_by_(first_one) %>% summarise_all(funs(sum))
     A$Simulated_Revenue=round(A$Simulated_Revenue,0)
     A$Optimised_Revenue=round(A$Optimised_Revenue,0)
     A$diff=percent((A$Optimised_Revenue-A$Simulated_Revenue)/A$Simulated_Revenue)
     #browser()
     #xax_is_list_old=sapply(A$Channel, FUN = function(x) {paste(strwrap(x, width = 8), collapse = "<br>")})
     xax_is_list=sapply( A[[first_one]], FUN = function(x) {paste(strwrap(x, width = 8), collapse = "<br>")})
     fig <- A %>% plot_ly()
     fig <- fig %>% add_trace(x =xax_is_list,y = A$Simulated_Revenue, type = 'bar', name = 'Simulated Acc. Opened', text = lapply(A$Simulated_Revenue,function(x)   units_to_M_K(x)) , textposition = 'outside',
                              marker = list(color = 'rgb(243,156,18)',
                                            line = list(color = 'rgb(243,156,18)', width = 0.5)))
     fig <- fig %>% add_trace(x =xax_is_list,y = A$Optimised_Revenue, type = 'bar', name = 'Optimised Acc. Opened', text = lapply(A$Optimised_Revenue,function(x)   units_to_M_K(x)) , textposition = 'outside',
                              marker = list(color = 'rgb(0,166,90)',
                                            line = list(color = 'rgb(0,166,90)', width = 0.5)))
     fig <- fig %>% layout(yax_is = list(title = 'Contribution'), barmode = 'group')
     fig<- fig %>% layout(legend = list(x = 0.05, y = 1.0))
     fig <- fig %>% add_annotations(x = xax_is_list,y = A$Optimised_Revenue,text = sprintf("<b>%s</b>",A$diff),showarrow = T,arrowside='none',arrowsize=4,arrowwidth=0.1,
                                    xanchor='left',xshift=8)
     fig
   })
   
   
   output$SBO_spend_level <- renderPlotly({
     # data_for_chart<<-SBO_opti_data()
     if(is.null(input$SBO_Budget_selector)){
       first_one=names(myData())[lapply(myData(),class)=="character"][1]}
     else{first_one=input$SBO_Budget_selector}
     A=SBO_opti_data()[,c(first_one,"Simulated_Spend","Optimised_Spend")] %>%  group_by_(first_one) %>% summarise_all(funs(sum))
     A$Simulated_Spend=round(A$Simulated_Spend,0)
     A$Optimised_Spend=round(A$Optimised_Spend,0)
     A$diff=percent((A$Optimised_Spend-A$Simulated_Spend)/A$Simulated_Spend)
     #browser()
     #xax_is_list_old=sapply(A$Channel, FUN = function(x) {paste(strwrap(x, width = 8), collapse = "<br>")})
     xax_is_list=sapply( A[[first_one]], FUN = function(x) {paste(strwrap(x, width = 8), collapse = "<br>")})
     fig <- A %>% plot_ly()
     fig <- fig %>% add_trace(x =xax_is_list,y = A$Simulated_Spend, type = 'bar', name = 'What If Spend', text = lapply(A$Simulated_Spend,function(x)   units_to_M_K(x)) , textposition = 'outside',
                              marker = list(color = 'rgb(243,156,18)',
                                            line = list(color = 'rgb(243,156,18)', width = 0.5)))
     fig <- fig %>% add_trace(x =xax_is_list,y = A$Optimised_Spend, type = 'bar', name = 'Optimised Spend', text = lapply(A$Optimised_Spend,function(x)   units_to_M_K(x)) , textposition = 'outside',
                              marker = list(color = 'rgb(0,166,90)',
                                            line = list(color = 'rgb(0,166,90)', width = 0.5)))
     fig <- fig %>% layout(yax_is = list(title = 'Spend'), barmode = 'group')
     fig<- fig %>% layout(legend = list(x = 0.05, y = 1.0))
     
     
     
     # fig <- fig %>% add_annotations(x = xax_is_list,y = A$Optimised_Spend,text = sprintf("<b>%s</b>",lapply(A$diff,function(x)   units_to_M_K(x))),showarrow = TRUE,
     #                                xanchor='left', xshift=25)
     
     fig <- fig %>% add_annotations(x = xax_is_list,y = A$Optimised_Spend,text = sprintf("<b>%s</b>",A$diff),showarrow = T,arrowside='none',arrowsize=4,arrowwidth=0.1,
                                    xanchor='left',xshift=8)
     fig
   })
   
   output$GBO_rev_level <- renderPlotly({
     if(is.null(input$GBO_Budget_selector)){
       first_one=names(myData())[lapply(myData(),class)=="character"][1]}
     else{first_one=input$GBO_Budget_selector}
     A=GBO_opti_data()[,c(first_one,"Simulated_Revenue","Optimised_Revenue")] %>%  group_by_(first_one) %>% summarise_all(funs(sum))
     A$Simulated_Revenue=round(A$Simulated_Revenue,0)
     A$Optimised_Revenue=round(A$Optimised_Revenue,0)
     A$diff=percent((A$Optimised_Revenue-A$Simulated_Revenue)/A$Simulated_Revenue)
     #browser()
     #xax_is_list_old=sapply(A$Channel, FUN = function(x) {paste(strwrap(x, width = 8), collapse = "<br>")})
     xax_is_list=sapply( A[[first_one]], FUN = function(x) {paste(strwrap(x, width = 8), collapse = "<br>")})
     fig <- A %>% plot_ly()
     fig <- fig %>% add_trace(x =xax_is_list,y = A$Simulated_Revenue, type = 'bar', name = 'Simulated Acc. Opened', text = lapply(A$Simulated_Revenue,function(x)   units_to_M_K(x)) , textposition = 'outside',
                              marker = list(color = 'rgb(243,156,18)',
                                            line = list(color = 'rgb(243,156,18)', width = 0.5)))
     fig <- fig %>% add_trace(x =xax_is_list,y = A$Optimised_Revenue, type = 'bar', name = 'Optimised Acc. Opened', text = lapply(A$Optimised_Revenue,function(x)   units_to_M_K(x)) , textposition = 'outside',
                              marker = list(color = 'rgb(0,166,90)',
                                            line = list(color = 'rgb(0,166,90)', width = 0.5)))
     fig <- fig %>% layout(yax_is = list(title = 'Contribution'), barmode = 'group')
     fig<- fig %>% layout(legend = list(x = 0.05, y = 1.0))
     fig <- fig %>% add_annotations(x = xax_is_list,y = A$Optimised_Revenue,text = sprintf("<b>%s</b>",A$diff),showarrow = TRUE,arrowside='none',arrowsize=4,arrowwidth=0.1,
                                    xanchor='left', xshift=8)
     fig
   })
   
   output$GBO_spend_level <- renderPlotly({
     if(is.null(input$GBO_Budget_selector)){
       first_one=names(myData())[lapply(myData(),class)=="character"][1]}
     else{first_one=input$GBO_Budget_selector}
     A=GBO_opti_data()[,c(first_one,"Simulated_Spend","Optimised_Spend")] %>%  group_by_(first_one) %>% summarise_all(funs(sum))
     A$Simulated_Spend=round(A$Simulated_Spend,0)
     A$Optimised_Spend=round(A$Optimised_Spend,0)
     A$diff=percent((A$Optimised_Spend-A$Simulated_Spend)/A$Simulated_Spend)
     #browser()
     #xax_is_list_old=sapply(A$Channel, FUN = function(x) {paste(strwrap(x, width = 8), collapse = "<br>")})
     xax_is_list=sapply( A[[first_one]], FUN = function(x) {paste(strwrap(x, width = 8), collapse = "<br>")})
     fig <- A %>% plot_ly()
     fig <- fig %>% add_trace(x =xax_is_list,y = A$Simulated_Spend, type = 'bar', name = 'What If Spend', text = lapply(A$Simulated_Spend,function(x)   units_to_M_K(x)) , textposition = 'outside',
                              marker = list(color = 'rgb(243,156,18)',
                                            line = list(color = 'rgb(243,156,18)', width = 0.5)))
     fig <- fig %>% add_trace(x =xax_is_list,y = A$Optimised_Spend, type = 'bar', name = 'Optimised Spend', text = lapply(A$Optimised_Spend,function(x)   units_to_M_K(x)) , textposition = 'outside',
                              marker = list(color = 'rgb(0,166,90)',
                                            line = list(color = 'rgb(0,166,90)', width = 0.5)))
     fig <- fig %>% layout(yax_is = list(title = 'Spend'), barmode = 'group')
     fig<- fig %>% layout(legend = list(x = 0.05, y = 1.0))
     fig <- fig %>% add_annotations(x = xax_is_list,y = A$Optimised_Spend,text = sprintf("<b>%s</b>",A$diff),showarrow = TRUE,arrowside='none',arrowsize=4,arrowwidth=0.1,
                                    xanchor='left', xshift=8)
     fig
   })
   
   observeEvent(c(input$simulate_button,input$export_button),{
     contents=simulated_data()
     #names(Summary_Table)[names(Summary_Table)=="Contribution"]="Application_Count"
     #contents$Simulated_Spend=round(contents$Simulated_Spend,2)
     contents$Simulated_Spend=dollar(contents$Simulated_Spend)
     #contents$Spend=round(contents$Spend,2)
     contents$Spend=dollar(contents$Spend)
     contents$Revenue_Calc=dollar(contents$Revenue_Calc)
     contents$Simulated_Revenue_Calc=dollar(contents$Simulated_Revenue_Calc)
     revals$contents=contents
     ##val=as.numeric(gsub("[\\$,]", "", val))
     
     # revals$contents[,c("Simulated_Spend","Spend")] <- as.numeric(gsub("[\\$,]", "", revals$contents[,c("Simulated_Spend","Spend")]))

     
   output$contents <- renderD3tf({
     
     
     print_columns=c(names(simulated_data()[, sapply(simulated_data(), class) == 'character']),"Contribution","Spend","Revenue_Calc","Simulated_Revenue","Simulated_Revenue_Calc","Simulated_Spend","Contri_Perc","LB","UB")
     #print_columns=names(contents)[!names(contents) %in% c("Powered_Revenue")]
     contents=contents[,print_columns]
     contents$Contribution=round(contents$Contribution,0)
     
     contents$Simulated_Revenue=round(contents$Simulated_Revenue,0)
     # contents$Simulated_Spend=round(contents$Simulated_Spend,2)
     
     contents$Contribution<-format(as.numeric(contents$Contribution), big.mark=",")
     #contents$Spend<-comma(contents$Spend)
     #contents$Spend<-paste0('$',format(as.numeric(contents$Spend), nsmall=2, big.mark=","))
     contents$Contri_Perc<-paste0(contents$Contri_Perc*100,'%')
     
     contents$Simulated_Revenue<-format(as.numeric(contents$Simulated_Revenue), big.mark=",")
     #contents$Simulated_Spend<-paste0('$',format(as.numeric(contents$Simulated_Spend), nsmall=2, big.mark=","))
     names(contents)[names(contents)=="Simulated_Revenue"]="What If Application_Count"
     names(contents)[names(contents)=="Simulated_Spend"]="What If Spend"
     names(contents)[names(contents)=="Contribution"]="Application_Count"
     names(contents)[names(contents)=="Revenue_Calc"]="Revenue"
     names(contents)[names(contents)=="Simulated_Revenue_Calc"]="Simulated_Revenue"
     table_Props <- list(
       btn_reset = TRUE,
       rows_counter =T
       );
     
     extensions <-  list(
       list(name = "sort"));
     
     d3tf(
       contents,
       table_Props,
       edit = c(paste0("col_",(ncol(contents)-4)),paste0("col_",(ncol(contents)-2)),paste0("col_",(ncol(contents)-1))),
       enableTf = TRUE,
       showRowNames = FALSE,
       selectableRows = "multiple",
       selectableRowsClass = "info",
       tableStyle = "table table-bordered table-condensed",
       filterInput = TRUE,
       extensions = extensions
     )
     
   })
   })
   
   observe({
     if(is.null(input$contents_edit)) return(NULL);
     edit <- input$contents_edit;
     #browser()
     isolate({
       # need isolate, otherwise this observer would run twice
       # for each edit
       id <- edit$id;
       row <- as.integer(edit$row);
       col <- as.integer(edit$col);
       val <- edit$val;
       #reject edit
       #if (col==(ncol(contents())-1)){
       if ( sum(is.na(revals$contents[row, c(1,2)])) ){
         oldval <- revals$contents[row, col];
         rejectEdit(session, tbl = "contents", row = row, col = col, id = id, value = oldval);
         return(NULL);}
       if (col %in% c(ncol(revals$contents)-5)){
         val<-as.numeric(val)
       if ( val<0.0 || val >1.0 ){
         oldval <- as.numeric(revals$contents[row, (col+5)]);
         print(oldval)
         rejectEdit(session, tbl = "contents", row = row, col = col, id = id, value = oldval);
         print("1st loop")
         print(edit)
         print(col)
         print(class(col))
         print(revals$contents)
         return(NULL);}
       }
       if (col %in% c(ncol(revals$contents)-4)){
         val<-as.numeric(val)
         if ( val<1.0 || val >2.0 ){
           oldval <- as.numeric(revals$contents[row, (col+5)]);
           rejectEdit(session, tbl = "contents", row = row, col = col, id = id, value = oldval)
           print(revals$contents)
           return(NULL)}
       }
       
       # accept edits
       #if(col==(ncol(contents())-1)) {
       #val=as.numeric(val)
       if(col %in% c(ncol(revals$contents)-5,ncol(revals$contents)-4)){
       val<-as.numeric(val)
       revals$contents[row, col+5] <- val
       }
       
       # if(col %in% c(ncol(revals$contents)-5,ncol(revals$contents)-4)){
       #   revals$contents[row, col+4] <- val
       # }
       # 
       if(col %in% c(ncol(revals$contents)-7)){
         print(val)
         print(class(val))
         print(head( revals$contents))
         #val=as.numeric(gsub("[\\$,]", "", val))
         print(val)
         revals$contents[row, col+4] <- val
         print( revals$contents[row, col+4])
       }
      
     
       # confirm edits
       confirmEdit(session, tbl = "contents", row = row, col = col, id = id,value = val);
       print(head( revals$contents))
       revals$edits["Success", "Row"] <- row;
       revals$edits["Success", "Column"] <- col;
       revals$edits["Success", "Value"] <- val;
     })
   })
   

 
   
  
   output$Spend <- renderValueBox({
     #data_current=simulated_data()
     data_current=revals$contents
     data_current$Simulated_Spend<-as.numeric(gsub("[\\$,]", "", data_current$Simulated_Spend))
     data_current$Spend<-as.numeric(gsub("[\\$,]", "", data_current$Spend))
     
     
     # opt_data_input$Simulated_Spend<-as.numeric(gsub("[\\$,]", "", opt_data_input$Simulated_Spend))
     # opt_data_input$Spend<-as.numeric(gsub("[\\$,]", "", opt_data_input$Spend))
     valueBox(tags$p(dollar(sum(data_current$Simulated_Spend)/1000000, suffix = "M", prefix = "AU$ "), style = "font-size: 60%;")
              , "Total Simulated Spend", 
              # icon = icon("coins"),
              color = "yellow"
     )
     
   })
   
   output$Contribution <- renderValueBox({
     #data_current=simulated_data()
     data_current=revals$contents
     data_current$Simulated_Spend<-as.numeric(gsub("[\\$,]", "", data_current$Simulated_Spend))
     data_current$Spend<-as.numeric(gsub("[\\$,]", "", data_current$Spend))
     data_current$Simulated_Revenue=data_current$Coef*(data_current$Simulated_Spend^data_current$Power)
     valueBox(tags$p(dollar(sum(data_current$Simulated_Revenue)/1000000, suffix = "M",prefix = ""), style = "font-size: 60%;")
              , "Total Simulated Acc. Opened", 
              # icon = icon("coins"),
              color = "yellow"
     )
     
   })
   
   
   output$CPA_Input_Assess <- renderValueBox({
     #data_current=simulated_data()
     data_current=revals$contents
     data_current$Simulated_Spend<-as.numeric(gsub("[\\$,]", "", data_current$Simulated_Spend))
     data_current$Spend<-as.numeric(gsub("[\\$,]", "", data_current$Spend))
     data_current$Simulated_Revenue=data_current$Coef*(data_current$Simulated_Spend^data_current$Power)
     
     valueBox(
       tags$p(round(sum(data_current$Simulated_Spend)/sum(data_current$Simulated_Revenue),2), style = "font-size: 60%;")
       , "CPA",
       # icon = icon("coins"),
       color = "yellow"
     )

   })
   
   output$Revenue <- renderValueBox({
     #data_current=simulated_data()
     data_current=revals$contents
     data_current$Simulated_Spend<-as.numeric(gsub("[\\$,]", "", data_current$Simulated_Spend))
     data_current$Spend<-as.numeric(gsub("[\\$,]", "", data_current$Spend))
     data_current$Simulated_Revenue=data_current$Coef*(data_current$Simulated_Spend^data_current$Power)
     data_current$Simulated_Revenue_Calc=data_current$Multiplier*data_current$Simulated_Revenue #19052021
     
     valueBox(tags$p(dollar(sum(data_current$Simulated_Revenue_Calc)/1000000, suffix = "M",prefix = "AU$ "), style = "font-size: 60%;")
              , "Total Simulated RAR",
              # icon = icon("coins"),
              color = "yellow"
     )

   })


   output$ROMI_Input_Assess <- renderValueBox({
     #data_current=simulated_data()
     data_current=revals$contents
     data_current$Simulated_Spend<-as.numeric(gsub("[\\$,]", "", data_current$Simulated_Spend))
     data_current$Spend<-as.numeric(gsub("[\\$,]", "", data_current$Spend))
     data_current$Simulated_Revenue=data_current$Coef*(data_current$Simulated_Spend^data_current$Power)
     data_current$Simulated_Revenue_Calc=data_current$Multiplier*data_current$Simulated_Revenue #19052021
     
     valueBox(
       tags$p(round(sum(data_current$Simulated_Revenue_Calc)/sum(data_current$Simulated_Spend),2), style = "font-size: 60%;")
       , "ROMI",
       # icon = icon("coins"),
       color = "yellow"
     )

   })
   # 
   
   observeEvent(input$jumptoopt, {
     updateTabItems(session, "tabs",selected = "Opti_SBO")
   })
   
   observeEvent(input$export_button, {
     # updateTabItems(session, "tabs",selected = "data")
     updateTabsetPanel(session,"navid",selected= "Optimisation_Tab")
   })
   
   SBO_opti_data=eventReactive(input$jumptoopt, {
     #opt_data_input=simulated_data()
     #revals$contents[,c("Simulated_Spend","Spend")] <- as.numeric(gsub("[\\$,]", "", revals$contents[,c("Simulated_Spend","Spend")]))
     opt_data_input=revals$contents
     opt_data_input$Simulated_Spend<-as.numeric(gsub("[\\$,]", "", opt_data_input$Simulated_Spend))
     opt_data_input$Spend<-as.numeric(gsub("[\\$,]", "", opt_data_input$Spend))
     opt_data_input$Simulated_Revenue=opt_data_input$Coef*(opt_data_input$Simulated_Spend^opt_data_input$Power)
     opt_data_input$Revenue_Calc<-as.numeric(gsub("[\\$,]", "", opt_data_input$Revenue_Calc))
     opt_data_input$Simulated_Revenue_Calc=as.numeric(gsub("[\\$,]", "", opt_data_input$Simulated_Revenue_Calc))
     
     print(head(opt_data_input))
     character_vars=names(opt_data_input)[lapply(opt_data_input,class)=="character"]
     
     #opt_data_input=opt_data_input[,c(character_vars,"Simulated_Spend","Simulated_Revenue","Power","Coef")]
     opt_data_input=opt_data_input[,c(character_vars,"Spend","Contribution","Revenue_Calc","Simulated_Spend","Simulated_Revenue","Simulated_Revenue_Calc","Power","Coef","LB","UB","Multiplier")]
     
     opt_data_input$UB=opt_data_input$UB*opt_data_input$Simulated_Spend
     opt_data_input$LB=opt_data_input$LB*opt_data_input$Simulated_Spend
     print(dim(opt_data_input))
     print(head(opt_data_input))
     
     optimised=SBO_opti_function(opt_data_input)
     opt_data_input$Optimised_Spend= optimised$solution
     opt_data_input$Optimised_Revenue=opt_data_input$Coef*(opt_data_input$Optimised_Spend^opt_data_input$Power)
     opt_data_input$Optimised_Revenue_Calc=opt_data_input$Multiplier*opt_data_input$Optimised_Revenue
     
     opt_data_input
   })
   
   GBO_opti_data=eventReactive(input$jumptogbo, {
     #revals$contents[,c("Simulated_Spend","Spend")] <- as.numeric(gsub("[\\$,]", "", revals$contents[,c("Simulated_Spend","Spend")]))
     #opt_data_input=simulated_data()
     opt_data_input=revals$contents
     opt_data_input$Simulated_Spend<-as.numeric(gsub("[\\$,]", "", opt_data_input$Simulated_Spend))
     opt_data_input$Spend<-as.numeric(gsub("[\\$,]", "", opt_data_input$Spend))
     opt_data_input$Simulated_Revenue=opt_data_input$Coef*(opt_data_input$Simulated_Spend^opt_data_input$Power)
     opt_data_input$Revenue_Calc<-as.numeric(gsub("[\\$,]", "", opt_data_input$Revenue_Calc))
     opt_data_input$Simulated_Revenue_Calc=as.numeric(gsub("[\\$,]", "", opt_data_input$Simulated_Revenue_Calc))
     
     print(head(opt_data_input))
     character_vars=names(opt_data_input)[lapply(opt_data_input,class)=="character"]
     
     #opt_data_input=opt_data_input[,c(character_vars,"Simulated_Spend","Simulated_Revenue","Power","Coef")]
     opt_data_input=opt_data_input[,c(character_vars,"Spend","Contribution","Revenue_Calc","Simulated_Spend","Simulated_Revenue","Simulated_Revenue_Calc","Power","Coef","LB","UB","Multiplier")]
     
     opt_data_input$UB=opt_data_input$UB*opt_data_input$Simulated_Spend
     opt_data_input$LB=opt_data_input$LB*opt_data_input$Simulated_Spend
     print(head(opt_data_input))
     optimised=GBO_opti_function(opt_data_input,input$Target_Revenue)
     opt_data_input$Optimised_Spend= optimised$solution
     opt_data_input$Optimised_Revenue=opt_data_input$Coef*(opt_data_input$Optimised_Spend^opt_data_input$Power)
     opt_data_input$Optimised_Revenue_Calc=opt_data_input$Multiplier*opt_data_input$Optimised_Revenue
     opt_data_input
   })
   
   GBO_Opti_Data_Granular=eventReactive(input$jumptogbo, {
     GBO_opti_data_export<-GBO_opti_data()
     drop_vars=c("Powered_Revenue","Power",	"Coef",	"LB",	"UB")
     GBO_opti_data_export<-GBO_opti_data_export[,!names(GBO_opti_data_export) %in% drop_vars]
     temp=data.frame(t(colSums(GBO_opti_data_export[,(length(input$heir1)+1):ncol(GBO_opti_data_export)])))
     GBO_opti_data_export=rbind.fill(GBO_opti_data_export,temp)
     
     GBO_opti_data_export$Spend_change_after_simulation<-percent((GBO_opti_data_export$Simulated_Spend-GBO_opti_data_export$Spend)/GBO_opti_data_export$Spend)
     GBO_opti_data_export$Application_count_change_after_simulation<-percent((GBO_opti_data_export$Simulated_Revenue-GBO_opti_data_export$Contribution)/GBO_opti_data_export$Contribution)
     GBO_opti_data_export$Revenue_change_after_simulation<-percent((GBO_opti_data_export$Simulated_Revenue_Calc-GBO_opti_data_export$Revenue_Calc)/GBO_opti_data_export$Revenue_Calc)
     
     GBO_opti_data_export$Spend_change_after_optimisation<-percent((GBO_opti_data_export$Optimised_Spend-GBO_opti_data_export$Simulated_Spend)/GBO_opti_data_export$Simulated_Spend)
     GBO_opti_data_export$Application_count_change_after_optimisation<-percent((GBO_opti_data_export$Optimised_Revenue-GBO_opti_data_export$Simulated_Revenue)/GBO_opti_data_export$Simulated_Revenue)
     GBO_opti_data_export$Revenue_change_after_optimisation<-percent((GBO_opti_data_export$Optimised_Revenue_Calc-GBO_opti_data_export$Simulated_Revenue_Calc)/GBO_opti_data_export$Simulated_Revenue_Calc)
     
     
     GBO_opti_data_export$Contri_Perc_Actual<-percent(GBO_opti_data_export$Contribution/sum(GBO_opti_data_export$Contribution[-nrow(GBO_opti_data_export)]))
     GBO_opti_data_export$Contri_Perc_Simulated<-percent(GBO_opti_data_export$Simulated_Revenue/sum(GBO_opti_data_export$Simulated_Revenue[-nrow(GBO_opti_data_export)]))
     GBO_opti_data_export$Contri_Perc_Optimised<-percent(GBO_opti_data_export$Optimised_Revenue/sum(GBO_opti_data_export$Optimised_Revenue[-nrow(GBO_opti_data_export)]))
     
     setnames(GBO_opti_data_export,c("Contribution","Simulated_Revenue","Optimised_Revenue","Optimised_Revenue_Calc","Simulated_Revenue_Calc","Revenue_Calc"),c("Application_Count","Simulated_Application_Count","Optimised_Application_Count","Revenue","Simulated_Revenue","Optimised_Revenue"))
     GBO_opti_data_export
   })
   
   SBO_Opti_Data_Granular=eventReactive(input$jumptoopt, {
     SBO_opti_data_export<-SBO_opti_data()
     drop_vars=c("Powered_Revenue","Power",	"Coef",	"LB",	"UB")
     SBO_opti_data_export<-SBO_opti_data_export[,!names(SBO_opti_data_export) %in% drop_vars]
     temp=data.frame(t(colSums(SBO_opti_data_export[,(length(input$heir1)+1):ncol(SBO_opti_data_export)])))
     SBO_opti_data_export=rbind.fill(SBO_opti_data_export,temp)
     
     SBO_opti_data_export$Spend_change_after_simulation<-percent((SBO_opti_data_export$Simulated_Spend-SBO_opti_data_export$Spend)/SBO_opti_data_export$Spend)
     SBO_opti_data_export$Application_count_change_after_simulation<-percent((SBO_opti_data_export$Simulated_Revenue-SBO_opti_data_export$Contribution)/SBO_opti_data_export$Contribution)
     SBO_opti_data_export$Revenue_change_after_simulation<-percent((SBO_opti_data_export$Simulated_Revenue_Calc-SBO_opti_data_export$Revenue_Calc)/SBO_opti_data_export$Revenue_Calc)
     
     SBO_opti_data_export$Spend_change_after_optimisation<-percent((SBO_opti_data_export$Optimised_Spend-SBO_opti_data_export$Simulated_Spend)/SBO_opti_data_export$Simulated_Spend)
     SBO_opti_data_export$Application_count_change_after_optimisation<-percent((SBO_opti_data_export$Optimised_Revenue-SBO_opti_data_export$Simulated_Revenue)/SBO_opti_data_export$Simulated_Revenue)
     SBO_opti_data_export$Revenue_change_after_optimisation<-percent((SBO_opti_data_export$Optimised_Revenue_Calc-SBO_opti_data_export$Simulated_Revenue_Calc)/SBO_opti_data_export$Simulated_Revenue_Calc)
     
     
     SBO_opti_data_export$Contri_Perc_Actual<-percent(SBO_opti_data_export$Contribution/sum(SBO_opti_data_export$Contribution[-nrow(SBO_opti_data_export)]))
     SBO_opti_data_export$Contri_Perc_Simulated<-percent(SBO_opti_data_export$Simulated_Revenue/sum(SBO_opti_data_export$Simulated_Revenue[-nrow(SBO_opti_data_export)]))
     SBO_opti_data_export$Contri_Perc_Optimised<-percent(SBO_opti_data_export$Optimised_Revenue/sum(SBO_opti_data_export$Optimised_Revenue[-nrow(SBO_opti_data_export)]))
     
     setnames(SBO_opti_data_export,c("Contribution","Simulated_Revenue","Optimised_Revenue","Optimised_Revenue_Calc","Simulated_Revenue_Calc","Revenue_Calc"),c("Application_Count","Simulated_Application_Count","Optimised_Application_Count","Revenue","Simulated_Revenue","Optimised_Revenue"))
     SBO_opti_data_export
   })
   
   output$SBO_Opti_Granular <- renderD3tf({
     SBO_opti_data_display<-SBO_Opti_Data_Granular()
     # print("Column name of SBO before filtering: \n")
     # print(names(SBO_opti_data_display))
     print_columns=c(names(SBO_opti_data_display[sapply(SBO_opti_data_display, class) == 'character']),"Simulated_Spend","Simulated_Application_Count","Simulated_Revenue","Optimised_Spend","Optimised_Application_Count","Optimised_Revenue","Spend_change_after_optimisation","Application_count_change_after_optimisation","Revenue_change_after_optimisation")
     #print("Column name of SBO After filtering: \n")
     print(print_columns)
     #print_columns=names(SBO_opti_data_display)[!names(SBO_opti_data_display) %in% c("Powered_Application_Count")]
     SBO_opti_data_display=SBO_opti_data_display[,print_columns]
     # SBO_opti_data_display$Application_Count=round(SBO_opti_data_display$Application_Count,0)
     # SBO_opti_data_display$Spend=round(SBO_opti_data_display$Spend,2)
     SBO_opti_data_display$Simulated_Application_Count=round(SBO_opti_data_display$Simulated_Application_Count,0)
     SBO_opti_data_display$Simulated_Spend=round(SBO_opti_data_display$Simulated_Spend,2)
     SBO_opti_data_display$Optimised_Application_Count=round(SBO_opti_data_display$Optimised_Application_Count,0)
     SBO_opti_data_display$Optimised_Spend=round(SBO_opti_data_display$Optimised_Spend,2)
     
     # SBO_opti_data_display$Application_Count<-format(as.numeric(SBO_opti_data_display$Application_Count), big.mark=",")
     # SBO_opti_data_display$Spend<-paste0('$',format(as.numeric(SBO_opti_data_display$Spend), nsmall=2, big.mark=","))
     SBO_opti_data_display$Simulated_Application_Count<-format(as.numeric(SBO_opti_data_display$Simulated_Application_Count), big.mark=",")
     SBO_opti_data_display$Simulated_Spend<-paste0('$',format(as.numeric(SBO_opti_data_display$Simulated_Spend), nsmall=2, big.mark=","))
     SBO_opti_data_display$Simulated_Revenue<-paste0('$',format(as.numeric(SBO_opti_data_display$Simulated_Revenue), nsmall=2, big.mark=","))
     
     SBO_opti_data_display$Optimised_Application_Count<-format(as.numeric(SBO_opti_data_display$Optimised_Application_Count), big.mark=",")
     SBO_opti_data_display$Optimised_Spend<-paste0('$',format(as.numeric(SBO_opti_data_display$Optimised_Spend), nsmall=2, big.mark=","))
     SBO_opti_data_display$Optimised_Revenue<-paste0('$',format(as.numeric(SBO_opti_data_display$Optimised_Revenue), nsmall=2, big.mark=","))
     
     # SBO_opti_data_display$Contri_Perc_Actual<-paste0(SBO_opti_data_display$Contri_Perc_Actual,'%')
     # SBO_opti_data_display$Contri_Perc_Simulated<-paste0(SBO_opti_data_display$Contri_Perc_Simulated,'%')
     # SBO_opti_data_display$Contri_Perc_Optimised<-paste0(SBO_opti_data_display$Contri_Perc_Optimised,'%')
     # 
     # SBO_opti_data_display$Spend_change_after_simulation<-paste0(SBO_opti_data_display$Spend_change_after_simulation,'%')
     # SBO_opti_data_display$Application_count_change_after_simulation<-paste0(SBO_opti_data_display$Application_count_change_after_simulation,'%')
     SBO_opti_data_display$Application_count_change_after_optimisation<-paste0(SBO_opti_data_display$Application_count_change_after_optimisation,'')
     SBO_opti_data_display$Spend_change_after_optimisation<-paste0(SBO_opti_data_display$Spend_change_after_optimisation,'')
     SBO_opti_data_display$Revenue_change_after_optimisation<-paste0(SBO_opti_data_display$Revenue_change_after_optimisation,'')
     
     names(SBO_opti_data_display)[names(SBO_opti_data_display)=="Simulated_Revenue"]="What If Revenue"
     names(SBO_opti_data_display)[names(SBO_opti_data_display)=="Simulated_Application_Count"]="What If Application_Count"
     names(SBO_opti_data_display)[names(SBO_opti_data_display)=="Simulated_Spend"]="What If Spend"
     names(SBO_opti_data_display)[names(SBO_opti_data_display)=="Spend_change_after_optimisation"]="Spend_Change"
     names(SBO_opti_data_display)[names(SBO_opti_data_display)=="Application_count_change_after_optimisation"]="Application_Count_Change"
     names(SBO_opti_data_display)[names(SBO_opti_data_display)=="Revenue_change_after_optimisation"]="Revenue_Change"
     
     table_Props <- list(
       btn_reset = TRUE,
       rows_counter =T
     );
     
     extensions <-  list(
       list(name = "sort"));
     
     d3tf(
       SBO_opti_data_display,
       table_Props,
       enableTf = TRUE,
       showRowNames = FALSE,
       selectableRows = "multiple",
       selectableRowsClass = "info",
       tableStyle = "table table-bordered",
       filterInput = TRUE,
       extensions=extensions
     )
   })
   
   output$GBO_Opti_Granular <- renderD3tf({
     GBO_opti_data_display<-GBO_Opti_Data_Granular()
     print_columns=c(names(GBO_opti_data_display[sapply(GBO_opti_data_display, class) == 'character']),"Simulated_Spend","Simulated_Application_Count","Simulated_Revenue","Optimised_Spend","Optimised_Application_Count","Optimised_Revenue","Spend_change_after_optimisation","Application_count_change_after_optimisation","Revenue_change_after_optimisation")
     print(print_columns)
     #print_columns=names(GBO_opti_data_display)[!names(GBO_opti_data_display) %in% c("Powered_Application_Count")]
     GBO_opti_data_display=GBO_opti_data_display[,print_columns]
     # GBO_opti_data_display$Application_Count=round(GBO_opti_data_display$Application_Count,0)
     # GBO_opti_data_display$Spend=round(GBO_opti_data_display$Spend,2)
     GBO_opti_data_display$Simulated_Application_Count=round(GBO_opti_data_display$Simulated_Application_Count,0)
     GBO_opti_data_display$Simulated_Spend=round(GBO_opti_data_display$Simulated_Spend,2)
     GBO_opti_data_display$Optimised_Application_Count=round(GBO_opti_data_display$Optimised_Application_Count,0)
     GBO_opti_data_display$Optimised_Spend=round(GBO_opti_data_display$Optimised_Spend,2)
     
     GBO_opti_data_display$Simulated_Application_Count<-format(as.numeric(GBO_opti_data_display$Simulated_Application_Count), big.mark=",")
     GBO_opti_data_display$Simulated_Spend<-paste0('$',format(as.numeric(GBO_opti_data_display$Simulated_Spend), nsmall=2, big.mark=","))
     GBO_opti_data_display$Simulated_Revenue<-paste0('$',format(as.numeric(GBO_opti_data_display$Simulated_Revenue), nsmall=2, big.mark=","))
     
     GBO_opti_data_display$Optimised_Application_Count<-format(as.numeric(GBO_opti_data_display$Optimised_Application_Count), big.mark=",")
     GBO_opti_data_display$Optimised_Spend<-paste0('$',format(as.numeric(GBO_opti_data_display$Optimised_Spend), nsmall=2, big.mark=","))
     GBO_opti_data_display$Optimised_Revenue<-paste0('$',format(as.numeric(GBO_opti_data_display$Optimised_Revenue), nsmall=2, big.mark=","))
     
     
     GBO_opti_data_display$Application_count_change_after_optimisation<-paste0(GBO_opti_data_display$Application_count_change_after_optimisation,'')
     GBO_opti_data_display$Spend_change_after_optimisation<-paste0(GBO_opti_data_display$Spend_change_after_optimisation,'')
     GBO_opti_data_display$Revenue_change_after_optimisation<-paste0(GBO_opti_data_display$Revenue_change_after_optimisation,'')
     
     names(GBO_opti_data_display)[names(GBO_opti_data_display)=="Simulated_Revenue"]="What If Revenue"
     names(GBO_opti_data_display)[names(GBO_opti_data_display)=="Simulated_Application_Count"]="What If Application_Count"
     names(GBO_opti_data_display)[names(GBO_opti_data_display)=="Simulated_Spend"]="What If Spend"
     names(GBO_opti_data_display)[names(GBO_opti_data_display)=="Spend_change_after_optimisation"]="Spend_Change"
     names(GBO_opti_data_display)[names(GBO_opti_data_display)=="Application_count_change_after_optimisation"]="Application_Count_Change"
     names(GBO_opti_data_display)[names(GBO_opti_data_display)=="Revenue_change_after_optimisation"]="Revenue_Change"
     
     table_Props <- list(
       btn_reset = TRUE,
       rows_counter =T
     );
     
     extensions <-  list(
       list(name = "sort"));
     
     d3tf(
       GBO_opti_data_display,
       table_Props,
       enableTf = TRUE,
       showRowNames = FALSE,
       selectableRows = "multiple",
       selectableRowsClass = "info",
       tableStyle = "table table-bordered",
       filterInput = TRUE,
       extensions=extensions
     )
   })
   
   
   output$Export_SBO <- downloadHandler(
     filename = function() {
       paste0("./Output/","SBO_Optimisation_Table_",input$simulated_spend,"_",input$jumptoopt,"_",Sys.Date(),".csv")
     },
     content = function(file) {
       write.csv(SBO_Opti_Data_Granular(), file,row.names = F,na="")
     }
   )
   
   output$Export_GBO <- downloadHandler(
     filename = function() {
       paste0("./Output/","GBO_Optimisation_Table_",input$Target_Revenue,"_",input$jumptogbo,"_",Sys.Date(),".csv")
     },
     content = function(file) {
       write.csv(GBO_Opti_Data_Granular(), file,row.names = F,na="")
     }
   )
   
   
   # observeEvent(input$jumptoopt, {
   #   SBO_opti_data_export<-SBO_Opti_Data_Granular()
   #   # print("head of  SBO table just before  exporting:\n")
   #   # print(head(SBO_opti_data_export))
   #   write.csv(SBO_opti_data_export, paste0("./Output/","SBO_Optimisation_Table_",input$simulated_spend,"_",input$jumptoopt,"_",Sys.Date(),".csv"),row.names = F,na="")
   # })
   # 
   # observeEvent(input$jumptogbo, {
   #   GBO_opti_data_export<-GBO_Opti_Data_Granular()
   #   # print("head of  GBO table just before  exporting:\n")
   #   # print(head(GBO_opti_data_export))
   #   write.csv(GBO_opti_data_export, paste0("./Output/","GBO_Optimisation_Table_",input$Target_Revenue,"_",input$jumptogbo,"_",Sys.Date(),".csv"),row.names = F,na="")
   # })
   
   output$Optimised_Contribution_SBO <- renderValueBox({
     valueBox(tags$p(dollar(sum(SBO_opti_data()$Optimised_Revenue)/1000000, suffix = "M", prefix = ""), style = "font-size: 60%;")
              , "Opt. Acc. Opened",
              # icon = icon("coins"),
              color = "green"
     )
   })
   output$Optimised_Spend <- renderValueBox({
     # print(oti)
     valueBox(tags$p(dollar(sum(SBO_opti_data()$Optimised_Spend)/1000000, suffix = "M", prefix = "AU$ "), style = "font-size: 60%;")
              , "Optimised Spend",
              # icon = icon("coins"),
              color = "purple"
     )
   })
   output$Optimised_CPA_SBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(round(sum(SBO_opti_data()$Optimised_Spend)/sum(SBO_opti_data()$Optimised_Revenue),2), style = "font-size: 60%;")
              , "Optimised CPA",
              # icon = icon("coins"),
              color = "green"
     )
   })
   
   output$Optimised_Revenue_SBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(dollar(sum(SBO_opti_data()$Optimised_Revenue_Calc)/1000000, suffix = "M", prefix = "AU$ "), style = "font-size: 60%;")
              , "Optimised RAR",
              # icon = icon("coins"),
              color = "green"
     )
   })
   output$Optimised_ROMI_SBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(round(sum(SBO_opti_data()$Optimised_Revenue_Calc)/sum(SBO_opti_data()$Optimised_Spend),2), style = "font-size: 60%;")
              , "Optimised ROMI",
              # icon = icon("coins"),
              color = "green"
     )
   })
   
   
   output$Simulated_Spend_SBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(dollar(sum(SBO_opti_data()$Simulated_Spend)/1000000, suffix = "M", prefix = "AU$ "), style = "font-size: 60%;")
              , "Simulated Spend",
              # icon = icon("coins"),
              color = "yellow"
     )
   })
   output$Simulated_Contribution_SBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(dollar(sum(SBO_opti_data()$Simulated_Revenue)/1000000, suffix = "M", prefix = ""), style = "font-size: 60%;")
              , "Sim. Acc. Opened",
              # icon = icon("coins"),
              color = "yellow"
     )
   })
   output$Simulated_CPA_SBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(round(sum(SBO_opti_data()$Simulated_Spend)/sum(SBO_opti_data()$Simulated_Revenue),2), style = "font-size: 60%;")
              , "Simulated CPA",
              # icon = icon("coins"),
              color = "yellow"
     )
   })
   
   output$Uplift_SBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p( dollar(round(((sum(SBO_opti_data()$Optimised_Revenue)-sum(SBO_opti_data()$Simulated_Revenue))/sum(SBO_opti_data()$Simulated_Revenue))*100,2), suffix = "%", prefix = ""), style = "font-size: 60%;")
              , "Sales Uplift",
              # icon = icon("coins"),
              color = "green"
     )
   })

   
   output$Simulated_Revenue_SBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(dollar(sum(SBO_opti_data()$Simulated_Revenue_Calc)/1000000, suffix = "M", prefix = "AU$ "), style = "font-size: 60%;")
              , "Simulated RAR",
              # icon = icon("coins"),
              color = "yellow"
     )
   })
   output$Simulated_ROMI_SBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(round(sum(SBO_opti_data()$Simulated_Revenue_Calc)/sum(SBO_opti_data()$Simulated_Spend),2), style = "font-size: 60%;")
              , "Simulated ROMI",
              # icon = icon("coins"),
              color = "yellow"
     )
   })
   
   
   output$ui_sbo <- renderUI({
     if(input$jumptoopt == 0) return()
     sidebarMenu(id = 'tabs',
                 menuItem(text="Spend Based Optimisation",tabName = 'Opti_SBO', icon = icon("bolt"))
     )
   })
   output$ui_gbo <- renderUI({
     if(input$jumptogbo == 0) return()
     sidebarMenu(id = 'tabs',
                 menuItem(text="Goal Based Optimisation",tabName = 'Opti_GBO', icon = icon("bolt"))
     )
   })
   
   observeEvent(input$jumptogbo, {
     updateTabItems(session, "tabs",selected = "Opti_GBO")
   })
   
   output$Optimised_Revenue_GBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(dollar(sum(GBO_opti_data()$Optimised_Revenue_Calc)/1000000, suffix = "M", prefix = "AU$ "), style = "font-size: 60%;")
              , "Optimised RAR",
              # icon = icon("coins"),
              color = "green"
     )
   })
   
   output$Optimised_Contribution_GBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(dollar(sum(GBO_opti_data()$Optimised_Revenue)/1000000, suffix = "M", prefix = ""), style = "font-size: 60%;")
              , "Opt. Acc. Opened",
              # icon = icon("coins"),
              color = "green"
     )
   })
   output$Optimised_Spend_GBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(dollar(sum(GBO_opti_data()$Optimised_Spend)/1000000, suffix = "M", prefix = "AU$ "), style = "font-size: 60%;")
              , "Optimised Spend",
              # icon = icon("coins"),
              color = "green"
     )
   })
   output$Optimised_CPA_GBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(round(sum(GBO_opti_data()$Optimised_Spend)/sum(GBO_opti_data()$Optimised_Revenue),2), style = "font-size: 60%;")
              , "Optimised CPA",
              # icon = icon("coins"),
              color = "green"
     )
   })
   
   output$Optimised_ROMI_GBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(round(sum(GBO_opti_data()$Optimised_Revenue_Calc/sum(GBO_opti_data()$Optimised_Spend)),2), style = "font-size: 60%;")
              , "Optimised ROMI",
              # icon = icon("coins"),
              color = "green"
     )
   })
   
   output$Simulated_Spend_GBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(dollar(sum(GBO_opti_data()$Simulated_Spend)/1000000, suffix = "M", prefix = "AU$ "), style = "font-size: 60%;")
              , "Simulated Spend",
              # icon = icon("coins"),
              color = "yellow"
     )
   })
   
   output$Simulated_Revenue_GBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(dollar(sum(GBO_opti_data()$Simulated_Revenue_Calc)/1000000, suffix = "M", prefix = "AU$ "), style = "font-size: 60%;")
              , "Simulated RAR",
              # icon = icon("coins"),
              color = "yellow"
     )
   })
   
   output$Simulated_Contribution_GBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(dollar(sum(GBO_opti_data()$Simulated_Revenue)/1000000, suffix = "M", prefix = ""), style = "font-size: 60%;")
              , "Sim. Acc. Opened",
              # icon = icon("coins"),
              color = "yellow"
     )
   })
   output$Simulated_CPA_GBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(round(sum(GBO_opti_data()$Simulated_Spend)/sum(GBO_opti_data()$Simulated_Revenue),2), style = "font-size: 60%;")
              , "Simulated CPA",
              # icon = icon("coins"),
              color = "yellow"
     )
   })
   
   output$Simulated_ROMI_GBO <- renderValueBox({
     # print(oti)
     valueBox(tags$p(round(sum(GBO_opti_data()$Simulated_Revenue_Calc/sum(GBO_opti_data()$Simulated_Spend)),2), style = "font-size: 60%;")
              , "Simulated ROMI",
              # icon = icon("coins"),
              color = "yellow"
     )
   })
   
   output$Uplift_GBO_Contribution <- renderValueBox({
     # print(oti)
     valueBox(tags$p( dollar(round(((sum(GBO_opti_data()$Optimised_Revenue)-sum(GBO_opti_data()$Simulated_Revenue))/sum(GBO_opti_data()$Simulated_Revenue))*100,2), suffix = "%", prefix = ""), style = "font-size: 60%;")
              , "Sales Uplift",
              # icon = icon("coins"),
              color = "light-blue"
     )
   })
   
   output$Uplift_GBO_Revenue <- renderValueBox({
     # print(oti)
     valueBox(tags$p( dollar(round(((sum(GBO_opti_data()$Optimised_Revenue_Calc)-sum(GBO_opti_data()$Simulated_Revenue_Calc))/sum(GBO_opti_data()$Simulated_Revenue_Calc))*100,2), suffix = "%", prefix = ""), style = "font-size: 60%;")
              , "RAR Uplift",
              # icon = icon("coins"),
              color = "light-blue"
     )
   })
   
   output$Target_Contribution_GBO <- renderValueBox({
     valueBox(tags$p(dollar(input$Target_Revenue/1000000, suffix = "M", prefix = ""), style = "font-size: 60%;")
              , "Target Accounts",
              # icon = icon("coins"),
              color = "light-blue"
     )
   })
   
   
   output$Uplift_GBO_Spend <- renderValueBox({
     # print(oti)
     valueBox(tags$p( dollar(round(((sum(GBO_opti_data()$Optimised_Spend)-sum(GBO_opti_data()$Simulated_Spend))/sum(GBO_opti_data()$Simulated_Spend))*100,2), suffix = "%", prefix = ""), style = "font-size: 60%;")
              , "Spend Uplift",
              # icon = icon("coins"),
              color = "light-blue"
     )
   })
   
   
})


