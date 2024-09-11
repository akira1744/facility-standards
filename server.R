server <- function(input, output, session) {
  
  ##############################################################################
  # マニュアルページ
  ##############################################################################
  
  # いつのデータを使用しているのかを示すためのUIを作成
  output$kouseikyoku_update_date <- renderUI({
    # Create a list of HTML text elements
    tagList(
      lapply(str_kouseikyoku_update_date$text, function(txt) {
        tags$p(txt) # Wrap each text in a <p> tag
      })
    )
  })
  
  # 元データのダウンロード機能
  output$download_original_data <- downloadHandler(
    filename = '施設基準_全件データ.xlsx'
    ,content = function(file){
      list(
        '医療機関一覧'=mst_latest_sisetu
        ,'施設基準届出一覧'=mst_latest_todokede
        ,'使用データ' = mst_pref_update_date
      ) %>% 
        writexl::write_xlsx(file)
    }
  )
  
  ##############################################################################
  # 自院のdf作成
  rt_my_sisetu <- reactive({
    if(input$my_sisetu==''){
      tibble('医療機関コード'='-','施設名'='','都道府県名'='','住所'='')
    }else{
      df_latest_sisetu %>% 
        filter(str_detect(施設名,input$my_sisetu)) %>% 
        select(医療機関コード,施設名,都道府県名,住所)
    }
  })
  
  # 自院が1施設に特定されていない場合に表示するメッセージを作成
  rt_my_sisetu_message <- reactive({
    if(input$my_sisetu==''){
      HTML('<span style="color: red;">自院を入力してください</span>')
    } else if ( nrow(rt_my_sisetu())!=1 ){
      HTML('<span style="color: red;">自院を1施設に特定してください</span>')
    } else{
      HTML(str_glue('<span style="color: blue;">自院設定:{rt_my_sisetu()$施設名}</span>'))
    }
  })
  
  output$my_sisetu_message <- renderUI({rt_my_sisetu_message()})
  # 別ページにも表示させる用にもう一つ作成
  output$my_sisetu_message2 <- renderUI({rt_my_sisetu_message()})
  
  # 自院のtb作成
  output$tb_my_sisetu <- renderDT(
    mydatatable(rt_my_sisetu(),row=5)
  )
  
  # 自院の施設基準を抽出
  rt_my_todokede <- reactive({
    
    my_codes <- rt_my_sisetu() %>% 
      filter(医療機関コード!='-') %>% 
      pull(医療機関コード)
    
    if (length(my_codes)==1){
      df_latest_todokede %>% 
        filter(医療機関コード %in% my_codes) %>% 
        select(受理届出コード) %>% 
        inner_join(df_mst_todokede,by='受理届出コード') %>% 
        select(受理届出名称)
      
    }else{
      tibble('受理届出名称'='')
    }
    
  })
  
  # 自院施設基準のtb作成
  output$tb_my_todokede <- renderDT(
    mydatatable(rt_my_todokede(),row=25)
  )
  
  ##############################################################################
  
  # 厚生局選択に応じて,df_mst_prefを絞り込み
  rt_mst_pref <- reactive({
    if(input$target_kouseikyoku=='すべて'){
      df_mst_pref
    }else{
      df_mst_pref %>% 
        filter(厚生局==input$target_kouseikyoku)
    }
  })
  
  # sidebar用に都道府県一覧を作成
  rt_sidelist_prefs <- reactive({
    rt_mst_pref() %>% 
      pull(都道府県名) %>% 
      c('すべて',.)
  })
  
  # rt_sidelist_prefsが更新されたとき、サイドバーの更新
  observe({
    updateSelectInput(session,'target_pref',choices=rt_sidelist_prefs())
  })
  
  ##############################################################################
  
  # 県で施設リストをしぼりこみ
  rt_target_sisetu1 <- reactive({
      if(input$target_pref=='すべて'){
        df_latest_sisetu %>% 
          filter(都道府県名 %in% rt_mst_pref()$都道府県名)
      }else{
        df_latest_sisetu %>% 
          filter(都道府県名 %in% input$target_pref) 
      }
  })

  ################################################################################
  
  # 比較対象の施設名入力 → 施設リストの絞り込み
  rt_target_sisetu2 <- reactive({
    if(input$target_sisetu==''){
      rt_target_sisetu1()
    }else{
      rt_target_sisetu1() %>% 
        filter(str_detect(施設名,input$target_sisetu))
    }
  })
  
################################################################################
  
  # 施設基準で絞込(1)
  rt_target_todokede1 <- reactive({
    regex_todokede1 <- make_beginning_match_regex(input$target_todokede1)
    df_mst_todokede %>% 
      filter(str_detect(受理届出名称,regex_todokede1)) %>% 
      select(受理届出コード,受理届出名称)
  })
  
  # 絞り込み用の受理届出名称のtableを作成
  output$tb_target_todokede1 <- renderDT(
    if(nrow(rt_target_todokede1()>0)){
      rt_target_todokede1() %>% 
        select(受理届出名称) %>% 
        mydatatable(row=5)
    }else{
      tibble('受理届出名称'='-') %>% 
      mydatatable(row=5)
    }
  )
  
  # rt_target_todokede1で施設を絞り込み
  rt_target_sisetu3 <- reactive({
    
    # rt_target_todokede1の医療機関コードを取得し追加
    if(nrow(rt_target_todokede1()>0)){
      
      target_todokede_codes1 <- rt_target_todokede1() %>% 
        pull(受理届出コード)
      
      target_todokede_sisetu_codes1 <- df_latest_todokede %>% 
        filter(受理届出コード %in% target_todokede_codes1) %>%
        distinct(医療機関コード) %>% 
        pull(医療機関コード)
      
      rt_target_sisetu2() %>%
        filter(医療機関コード %in% target_todokede_sisetu_codes1)
      
    }else{
      rt_target_sisetu2()
    }
  })
  
  ##############################################################################
  
  # 施設基準で絞込(2)
  rt_target_todokede2 <- reactive({
    regex_todokede2 <- make_beginning_match_regex(input$target_todokede2)
    df_mst_todokede %>% 
      filter(str_detect(受理届出名称,regex_todokede2)) %>% 
      select(受理届出コード,受理届出名称)
  })
  
  # 絞り込み用の受理届出名称のtableを作成
  output$tb_target_todokede2 <- renderDT(
    if(nrow(rt_target_todokede2()>0)){
      rt_target_todokede2() %>% 
        select(受理届出名称) %>% 
        mydatatable(row=5)
    }else{
      tibble('受理届出名称'='-') %>% 
        mydatatable(row=5)
    }
  )
  
  # rt_target_todokede2で施設を絞り込み
  rt_target_sisetu4 <- reactive({
    
    # rt_target_todokede1の医療機関コードを取得し追加
    if(nrow(rt_target_todokede2()>0)){
      
      target_todokede_codes2 <- rt_target_todokede2() %>% 
        pull(受理届出コード)
      
      target_todokede_sisetu_codes2 <- df_latest_todokede %>% 
        filter(受理届出コード %in% target_todokede_codes2) %>%
        distinct(医療機関コード) %>% 
        pull(医療機関コード)
      
      rt_target_sisetu3() %>%
        filter(医療機関コード %in% target_todokede_sisetu_codes2)
      
    }else{
      rt_target_sisetu3()
    }
  })

  ##############################################################################
  
  # 施設基準で絞込(3)
  rt_target_todokede3 <- reactive({
    regex_todokede3 <- make_beginning_match_regex(input$target_todokede3)
    df_mst_todokede %>% 
      filter(str_detect(受理届出名称,regex_todokede3)) %>% 
      select(受理届出コード,受理届出名称)
  })
  
  # 絞り込み用の受理届出名称のtableを作成
  output$tb_target_todokede3 <- renderDT(
    if(nrow(rt_target_todokede3()>0)){
      rt_target_todokede3() %>% 
        select(受理届出名称) %>% 
        mydatatable(row=5)
    }else{
      tibble('受理届出名称'='-') %>% 
        mydatatable(row=5)
    }
  )
  
  # rt_target_todokede3で施設を絞り込み
  rt_target_sisetu5 <- reactive({
    
    # rt_target_todokede1の医療機関コードを取得し追加
    if(nrow(rt_target_todokede3()>0)){
      
      target_todokede_codes3 <- rt_target_todokede3() %>% 
        pull(受理届出コード)
      
      target_todokede_sisetu_codes3 <- df_latest_todokede %>% 
        filter(受理届出コード %in% target_todokede_codes3) %>%
        distinct(医療機関コード) %>% 
        pull(医療機関コード)
      
      rt_target_sisetu4() %>%
        filter(医療機関コード %in% target_todokede_sisetu_codes3)
      
    }else{
      rt_target_sisetu4()
    }
  })
  
  ##############################################################################
  
  # 比較対象のtb作成
  output$tb_target_sisetu <- renderDT(
    mydatatable(rt_target_sisetu5(),row=5)
  )
  
  # 比較対象が0施設の場合のメッセージ
  target_sisetu_message <- reactive({
    target_sisetu_count <- nrow(rt_target_sisetu5())
    if(target_sisetu_count==0){
      HTML('<span style="color: red;">比較対象が0施設です</span>')
    } else{
      HTML(str_glue('<span style="color: blue;">比較対象設定: {formatC(target_sisetu_count, format = "d", big.mark = ",")}施設</span>'))
    }
  })

  output$target_sisetu_message <- renderUI({target_sisetu_message()})
  # 別ページにも表示させる用にもう一つ作成
  output$target_sisetu_message2 <- renderUI({target_sisetu_message()})
  
  ##############################################################################
  
  # 比較対象の施設基準を抽出
  rt_target_todokede_agg <- reactive({
    target_sisetu_count <- nrow(rt_target_sisetu5())
    if(target_sisetu_count==0){
      tibble('受理届出コード'=0)
    }else{
      df_latest_todokede %>%
        filter(医療機関コード %in% rt_target_sisetu5()$医療機関コード) %>%
        select(医療機関コード,受理届出コード) %>%
        group_by(受理届出コード) %>%
        summarise(
          算定率 = n() / target_sisetu_count
          ,算定施設数=n()
        ) %>% 
        inner_join(df_mst_todokede,by='受理届出コード') %>% 
        select(-受理届出コード)
    }
  })
  
  # # 結合
  rt_compare_todokede <- reactive({
    
    rt_my_todokede() %>% 
      mutate(自院算定='〇') %>% 
      full_join(rt_target_todokede_agg(),by='受理届出名称') %>%
      replace_na(list(
        自院算定='×',算定施設数=0,算定率=0
      )) %>% 
      filter(受理届出名称!='') %>% 
      arrange(desc(算定率),受理届出名称) %>% 
      filter(受理届出名称!='なし')
  })
  # 
  # # 比較対象のtb作成
  output$tb_compare_todokede <- renderDT(
    mydatatable(rt_compare_todokede(),row=25,pctcol='算定率')
  )
  
  ##############################################################################
  
  # 絞り込み条件をまとめたdfを作成する
  rt_sidebar<- reactive({
    tibble(
      絞込条件 = c(
        '自院'
        ,'比較対象の厚生局'
        ,'比較対象の都道府県'
        ,'比較対象の施設名'
        ,'比較対象の施設基準で絞り込み(1)'
        ,'比較対象の施設基準で絞り込み(2)'
        ,'比較対象の施設基準で絞り込み(3)'
      )
      ,入力 = c(
        input$my_sisetu
        ,input$target_kouseikyoku
        ,input$target_pref
        ,input$target_sisetu
        ,input$target_todokede1
        ,input$target_todokede2
        ,input$target_todokede3
      )
    )
  })
  ##############################################################################
  # 施設基準比較データのダウンロード
  output$download_compare_todokede <- downloadHandler(
    filename = '施設基準比較.xlsx'
    ,content = function(file){
      list(
        '施設基準比較'=rt_compare_todokede()
        ,'絞込条件' = rt_sidebar()
        ,'自院' = rt_my_sisetu()
        ,'比較対象' = rt_target_sisetu5()
        ,'施設基準で絞り込み(1)'= rt_target_todokede1()
        ,'施設基準で絞り込み(2)'= rt_target_todokede2()
        ,'施設基準で絞り込み(3)'= rt_target_todokede3()
        ,'使用データ' = mst_pref_update_date
      ) %>% 
        writexl::write_xlsx(file)
    }
  )
}
