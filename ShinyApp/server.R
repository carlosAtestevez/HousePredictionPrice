server <- function(input, output, session) {
  #Loading data
 
  loading_training_file = reactive({
    trainingFile = input$cflTrFile
    if(is.null(trainingFile)){
      
      shinyFeedback::feedbackDanger("cflTrFile",TRUE,"Select a file please!")
    }
    
    req(trainingFile)
    tr_file <- tools::file_ext(trainingFile$datapath)
    df_tr_file = read.csv(trainingFile$datapath,header = TRUE)
    df_tr_file
  })
  
  loading_testing_file = reactive({
    testingFile = input$cflTstFile
    if(is.null(testingFile)){
      
      shinyFeedback::feedbackDanger("cflTstFile",TRUE,"Select a file please!")
    }
    req(testingFile)
    tst_file <- tools::file_ext(testingFile$datapath)
    df_tst_file = read.csv(testingFile$datapath,header = TRUE)
    df_tst_file
  })
  
  loading_both_files = reactive({
    training_data = loading_training_file()
    testing_data = loading_testing_file()
    
    lst_file = list("training"=training_data,"testing"=testing_data)
    lst_file
  })
  
  running_model_ana_1 = reactive({
    list_files = event_run_ana_1()
    df_hp_raw_0 = list_files$training
    df_hp_raw_test_0 = list_files$testing
    df_hp_raw_ana1 = df_hp_raw_0
    df_hp_sp_1 = df_hp_raw_ana1 %>% filter(Neighborhood == "NAmes" | Neighborhood == "Edwards" | 
                                             Neighborhood == "BrkSide") %>% dplyr::select(SalePrice,GrLivArea,Neighborhood) 
    
    lm_tent_model_1 = lm(SalePrice~GrLivArea,data=df_hp_sp_1)
    lm_tent_model_1
  })
  running_model_ana_2 = reactive({
    list_files = event_run_ana_1()
    df_hp_raw_0 = list_files$training
    df_hp_raw_test_0 = list_files$testing
    df_hp_raw_ana1 = df_hp_raw_0
    df_hp_sp_1 = df_hp_raw_ana1 %>% filter(Neighborhood == "NAmes" | Neighborhood == "Edwards" | 
                                             Neighborhood == "BrkSide") %>% dplyr::select(SalePrice,GrLivArea,Neighborhood) 
    
    df_hp_sp_clean_1 = df_hp_sp_1[c(-131,-339,-169,-190),] 
    lm_tent_model_1 = lm(SalePrice~GrLivArea*Neighborhood,data=df_hp_sp_clean_1)
    lm_tent_model_1 
    
  })
  
  
  load_data_over =  eventReactive(input$btnLoadData,{
    #showNotification("Running model...")
    loading_both_files()
  })
  
  # load_data_over = reactive({
  #   showNotification("Loading files...")
  #   df_hp_raw_0 = loading_training_file()
  #   df_hp_test_kaggle = loading_training_file()
  #   df_hp_raw_0
  # })
  
 
  
  output$cboLoadData=renderPrint({
    list_files = load_data_over()
    df_hp_raw_0 = list_files$training
    df_hp_raw_test_0 = list_files$testing

    nr_obs = nrow(df_hp_raw_0)
    nr_obs_test = nrow(df_hp_raw_test_0 )
    str_format = sprintf("Training observations: %i, Testing Observations: %i",nr_obs,nr_obs_test)
    str_format
    
     
  })
  


  
  output$plotData1=renderPlot({
    list_files = load_data_over()
    df_hp_raw_0 = list_files$training
    df_hp_raw_test_0 = list_files$testing
    
    # df_hp_raw_0 = loading_training_file()
    # df_hp_raw_test_0 = loading_testing_file()
    
    df_hp_raw_ana1 = df_hp_raw_0
    df_hp_sp_1 = df_hp_raw_ana1 %>% filter(Neighborhood == "NAmes" | Neighborhood == "Edwards" | 
                                             Neighborhood == "BrkSide") %>% dplyr::select(SalePrice,GrLivArea,Neighborhood) 
    
    gr_a = df_hp_sp_1%>%ggplot()+geom_point(aes(x=GrLivArea,y=SalePrice,color=Neighborhood))+geom_smooth(aes(x=GrLivArea,y=SalePrice),method = "lm")+
      scale_x_continuous(labels = scales::comma_format())+scale_y_continuous(labels = scales::dollar_format())+labs(title="Sales Price vs Living area square feet",subtitle="Before outlier Correction",x="Living Area in Feet",y="Sales Price($US)")
    
    gr_a    
    
  })
  
  
  event_run_ana_1 =  eventReactive(input$btnDoAna1,{
    showNotification("Running model...")
    loading_both_files()
  })
  
  output$plotDataAna1=renderPlot({
    list_files = event_run_ana_1()
    df_hp_raw_0 = list_files$training
    df_hp_raw_test_0 = list_files$testing
    
    chkName = input$cchName
    chkEd = input$cchEd
    chkBrk = input$cchBrk
    
    lst_options = c()
    
    if(chkName){
      lst_options = c(lst_options,"NAmes")
    }
    if(chkEd){
      lst_options = c(lst_options,"Edwards")
    }
    if(chkBrk){
      lst_options = c(lst_options,"BrkSide")
    }

    df_hp_raw_ana1 = df_hp_raw_0
    # df_hp_sp_1 = df_hp_raw_ana1 %>% filter(Neighborhood == "NAmes" | Neighborhood == "Edwards" |
    #                                          Neighborhood == "BrkSide") %>% dplyr::select(SalePrice,GrLivArea,Neighborhood)
    
    df_hp_sp_1 = df_hp_raw_ana1 %>% filter(Neighborhood %in% lst_options ) %>% dplyr::select(SalePrice,GrLivArea,Neighborhood)

    gr_a = df_hp_sp_1%>%ggplot()+geom_point(aes(x=GrLivArea,y=SalePrice,color=Neighborhood))+geom_smooth(aes(x=GrLivArea,y=SalePrice),method = "lm")+
      scale_x_continuous(labels = scales::comma_format())+scale_y_continuous(labels = scales::dollar_format())+labs(title="Sales Price vs Living area square feet",subtitle="Before outlier Correction",x="Living Area in Feet",y="Sales Price($US)")

    gr_a
    
  })
 
  
  output$plotDataBeforeOuta1=renderPlot({
    lm_tent_model_1 = running_model_ana_1()
     par(mfrow=c(2,2))
     plot(lm_tent_model_1)
    # ols_plot_resid_lev(lm_tent_model_1)
  })
  output$plotDataBeforeOuta2=renderPlot({
    lm_tent_model_1 = running_model_ana_1()
    # par(mfrow=c(2,2))
    # plot(lm_tent_model_1)
    ols_plot_resid_lev(lm_tent_model_1)
  })
  output$plotDataBeforeoutb1=renderPlot({
    # list_files = event_run_ana_1()
    lm_tent_model_1 = running_model_ana_2()
    ols_plot_resid_lev(lm_tent_model_1)
  })
  
  output$plotDataAnab1=renderPlot({
    #event_run_ana_1
    # list_files = event_run_ana_1()
    lm_tent_model_1 = running_model_ana_2()
    par(mfrow=c(2,2))
    plot(lm_tent_model_1)
  })
  
  output$resultModel=renderPrint({
    lm_tent_model_1 = running_model_ana_2()
    summary(lm_tent_model_1)
    
  })

  
  





}
