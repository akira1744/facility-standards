server <- function(input, output, session) {
  
  # my_sisetuの選択肢をserver側で設定
  updateSelectizeInput(session, "my_sisetu", choices=sidelist_sisetu, server=TRUE)
  updateSelectizeInput(session, "target_sisetu", choices=sidelist_sisetu, server=TRUE)
  
  # target_todokedeの選択肢をserver側で設定
  updateSelectizeInput(session, "target_todokede1", choices=sidelist_todokede, server=TRUE)
  updateSelectizeInput(session, "target_todokede2", choices=sidelist_todokede, server=TRUE)
  updateSelectizeInput(session, "target_todokede3", choices=sidelist_todokede, server=TRUE)
  
  ##############################################################################
  # 施設基準検索のページ
  ##############################################################################

  # input$kensaku_wordとinput$kensaku_methodから受理届出コードを抽出
  rt_kensaku_todokede <- reactive({
    
    kensaku_word_fullwidth <- stringi::stri_trans_general(input$kensaku_word,'Halfwidth-Fullwidth')
    
    if(input$kensaku_word==''){
      tibble("受理届出コード" = "-","整理番号"='-', "受理届出名称" = "-")
    }else if(input$kensaku_method=='前方一致'){
      df_mst_todokede %>% 
        filter(
          str_detect(受理届出名称,str_glue('^{input$kensaku_word}'))
          | str_detect(受理届出名称,str_glue('^{kensaku_word_fullwidth}'))
        ) 
    }else if(input$kensaku_method=='部分一致'){
      df_mst_todokede %>% 
        filter(
          str_detect(受理届出名称,input$kensaku_word)
          | str_detect(受理届出名称,kensaku_word_fullwidth)
        )
    }else{
      df_mst_todokede %>% 
        filter(
          受理届出名称 == input$kensaku_word
          | 受理届出名称 == kensaku_word_fullwidth
        ) 
    }
  })
  
  ##############################################################################
  
  # 厚生局選択に応じて,df_mst_prefを絞り込み
  rt_kensaku_pref <- reactive({
    if (input$kensaku_kouseikyoku == "すべて") {
      df_mst_pref
    } else {
      df_mst_pref %>%
        filter(厚生局 == input$kensaku_kouseikyoku)
    }
  })
  
  # sidebar用に都道府県一覧を作成
  rt_kensaku_sidelist_prefs <- reactive({
    rt_kensaku_pref() %>%
      pull(都道府県名) %>%
      c("すべて", .)
  })
  
  # rt_sidelist_prefsが更新されたとき、サイドバーの更新
  observe({
    updateSelectInput(session, "kensaku_pref", choices = rt_kensaku_sidelist_prefs())
  })
  
  # 県で施設リストをしぼりこみ
  rt_filter_sisetu <- reactive({
    
    tmp <- df_latest_sisetu %>%
      filter(between(総病床数,input$kensaku_bed_range[1],input$kensaku_bed_range[2]))
    
    
    if (input$kensaku_pref == "すべて") {
      tmp %>% 
        filter(都道府県名 %in% rt_kensaku_pref()$都道府県名)
    } else {
      tmp %>% 
        filter(都道府県名 == input$kensaku_pref)
    }
  })
  
  ##############################################################################

  
  # 届出医療機関一覧のdfを作成
  rt_kensaku_sisetu <- reactive({
    if(input$kensaku_word=='' | nrow(df_latest_todokede)==0){
      tibble("整理番号" = "-", "受理届出名称" = "-", "医療機関コード" = "-", "施設名" = "-", "都道府県名" = "-", "住所" = "-", "病床" = "-", "総病床数" = "-")
    }else{
      
      rt_kensaku_todokede() %>% 
        inner_join(df_latest_todokede,by='受理届出コード') %>% 
        inner_join(rt_filter_sisetu(),by='医療機関コード') %>% 
        select(整理番号,受理届出名称,医療機関コード,施設名,都道府県名,住所,病床数,総病床数) %>% 
        arrange(整理番号,医療機関コード)
    }
  })
  
  # 届出医療機関一覧のDTを作成
  output$tb_kensaku_sisetu <- renderDT(
    mydatatable(rt_kensaku_sisetu(), row = 10)
  )
  
  # 届出集計表を作成
  rt_kensaku_agg <- reactive({
    if(input$kensaku_word=='' | nrow(rt_kensaku_todokede())==0){
      tibble("整理番号" = "-", "受理届出名称" = "-", "施設数" = "-")
    }else{
      
      rt_kensaku_sisetu() %>% 
        group_by(整理番号,受理届出名称) %>% 
        summarise(施設数 = n(),.groups='drop') %>%
        arrange(整理番号)
    }
  })
  
  # 届出集計表のDTを作成
  output$tb_kensaku_agg <- renderDT(
    mydatatable(rt_kensaku_agg(), row = 10)
  )
  
  rt_kensaku_filename <- reactive({
    str_glue("施設基準検索_{input$kensaku_word}({input$kensaku_method}).xlsx")
  })
  
  # 施設基準検索のダウンロード機能
  output$download_kensaku_todokede <- downloadHandler(
    filename = function(){
      rt_kensaku_filename()
    },
    content = function(file) {
      list(
        "集計表"=rt_kensaku_agg(),
        "施設一覧"=rt_kensaku_sisetu(),
        "使用データ" = mst_pref_update_date
      ) %>%
        writexl::write_xlsx(file)
    }
  )
  
  ##############################################################################
  # マニュアルページ
  ##############################################################################
  # 自院のdf作成
  rt_my_sisetu <- reactive({
    if (input$my_sisetu == "") {
      tibble("医療機関コード" = "-", "施設名" = "", "都道府県名" = "", "住所" = "",'病床数'='','総病床数'='')
    } else {
      df_latest_sisetu %>%
        filter(施設名==input$my_sisetu) %>%
        select(医療機関コード, 施設名, 都道府県名, 住所,病床数,総病床数)
    }
  })

  # 自院が1施設に特定されていない場合に表示するメッセージを作成
  rt_my_sisetu_message <- reactive({
    if (input$my_sisetu == "") {
      HTML('<span style="color: red;">自院施設名を選択してください</span>')
    } else if (nrow(rt_my_sisetu()) != 1) {
      HTML('<span style="color: red;">自院を1施設に特定してください</span>')
    } else {
      HTML(str_glue('<span style="color: blue;">自院設定:{rt_my_sisetu()$施設名}</span>'))
    }
  })

  output$my_sisetu_message <- renderUI({
    rt_my_sisetu_message()
  })
  # 別ページにも表示させる用にもう一つ作成
  output$my_sisetu_message2 <- renderUI({
    rt_my_sisetu_message()
  })

  # 自院のtb作成
  output$tb_my_sisetu <- renderDT(
    mydatatable(rt_my_sisetu(), row = 5)
  )

  # 自院の施設基準を抽出
  rt_my_todokede <- reactive({
    my_codes <- rt_my_sisetu() %>%
      filter(医療機関コード != "-") %>%
      pull(医療機関コード)

    if (length(my_codes) == 1) {
      my_todokede <- df_latest_todokede %>%
        filter(医療機関コード %in% my_codes) %>%
        select(受理届出コード) %>%
        inner_join(df_mst_todokede, by = "受理届出コード") %>%
        select(整理番号,受理届出名称) %>% 
        arrange(整理番号)
    } else {
      my_todokede <- tibble("整理番号"='',"受理届出名称" = "")
    }
    
    # 表示する施設基準で絞り込み
    if (input$display_todokede=='基本診療料'){
      my_todokede %>% 
        filter(str_detect(整理番号,'^1'))
    }else if (input$display_todokede=='特掲診療料'){
      my_todokede %>% 
        filter(str_detect(整理番号,'^2'))
    }else{
      my_todokede
    }
    
  })

  # 自院施設基準のtb作成
  output$tb_my_todokede <- renderDT(
    mydatatable(rt_my_todokede(), row = 10)
  )

  ##############################################################################

  # 厚生局選択に応じて,df_mst_prefを絞り込み
  rt_mst_pref <- reactive({
    if (input$target_kouseikyoku == "すべて") {
      df_mst_pref
    } else {
      df_mst_pref %>%
        filter(厚生局 == input$target_kouseikyoku)
    }
  })

  # sidebar用に都道府県一覧を作成
  rt_sidelist_prefs <- reactive({
    rt_mst_pref() %>%
      pull(都道府県名) %>%
      c("すべて", .)
  })

  # rt_sidelist_prefsが更新されたとき、サイドバーの更新
  observe({
    updateSelectInput(session, "target_pref", choices = rt_sidelist_prefs())
  })

  ##############################################################################

  # 県で施設リストをしぼりこみ
  rt_target_sisetu1 <- reactive({
    if (input$target_pref == "すべて") {
      df_latest_sisetu %>%
        filter(都道府県名 %in% rt_mst_pref()$都道府県名)
    } else {
      df_latest_sisetu %>%
        filter(都道府県名 == input$target_pref)
    }
  })

  ################################################################################

  # 比較対象の施設名入力 → 施設リストの絞り込み
  rt_target_sisetu2 <- reactive({
    if (input$target_sisetu == "") {
      rt_target_sisetu1()
    } else {
      rt_target_sisetu1() %>%
        filter(施設名== input$target_sisetu)
    }
  })

  ##############################################################################
  
  # 施設基準で比較対象を絞込
  
  ##############################################################################
  
  # 施設基準で絞込の受理届出コードを取得
  rt_target_todokede_codes <- reactive({
    
    if (input$target_todokede == "") {
      c()
    } else {
      
      target_todokede_fullwidth <- stringi::stri_trans_general(input$target_todokede,'Halfwidth-Fullwidth')
      
      if(input$todokede_kensaku=='前方一致'){
        target_todokede_code <- df_mst_todokede %>%
          filter(
            str_detect(受理届出名称,str_glue('^{input$target_todokede}'))
            | str_detect(受理届出名称,str_glue('^{target_todokede_fullwidth}'))
          ) %>% 
          select(受理届出コード,整理番号)
        
      }else if(input$todokede_kensaku=='完全一致'){
        target_todokede_code <- df_mst_todokede %>%
          filter(
            受理届出名称==input$target_todokede
            | 受理届出名称==target_todokede_fullwidth
          ) %>% 
          select(受理届出コード,整理番号)
        
      }else{
        target_todokede_code <- df_mst_todokede %>%
          filter(
            str_detect(受理届出名称,input$target_todokede)
            | str_detect(受理届出名称,target_todokede_fullwidth)
          ) %>% 
          select(受理届出コード,整理番号)
      }
      
      # 表示する施設基準で絞り込み
      if (input$display_todokede=='基本診療料'){
        target_todokede_code <- target_todokede_code %>% 
          filter(str_detect(整理番号,'^1'))
      }else if (input$display_todokede=='特掲診療料'){
        target_todokede_code <- target_todokede_code %>% 
          filter(str_detect(整理番号,'^2'))
      }else{
        target_todokede_code
      }
      
      if(nrow(target_todokede_code)==0){
        c()
      }else{
        target_todokede_code %>% 
          pull(受理届出コード)
      }
    }
  })
  
  ##############################################################################
  # 施設基準で絞込(1)
  rt_target_sisetu3 <- reactive({
    if (length(rt_target_todokede_codes()) == 0){
      rt_target_sisetu2()
    } else {
      target_todokede_sisetu_codes <- df_latest_todokede %>%
        filter(受理届出コード %in% rt_target_todokede_codes()) %>%
        pull(医療機関コード)
      rt_target_sisetu2() %>%
        filter(医療機関コード %in% target_todokede_sisetu_codes)
    }
  })
  ##############################################################################
  
  # 病床数で絞り込み
  rt_target_sisetu4 <- reactive({
    rt_target_sisetu3() %>% 
      filter(between(総病床数,input$bed_range[1],input$bed_range[2]))
  })
  
  ##############################################################################
  
  output$tb_target_sisetu <- renderDT(
    mydatatable(rt_target_sisetu4(), row = 5)
  )
  
  ##############################################################################
  
  # 施設基準で絞り込みの算定施設数集計dfを作成
  rt_target_todokede_agg <- reactive({
    if(input$target_todokede == ""){
      tibble(受理届出名称='絞込用の施設基準が入力されていません。')
    }else if (length(rt_target_todokede_codes())==0){
      tibble(受理届出名称='該当する施設基準がありません。')
    }else{
      df_latest_todokede %>% 
        filter(医療機関コード %in% unique(rt_target_sisetu4()$医療機関コード)) %>% 
        filter(受理届出コード %in% rt_target_todokede_codes()) %>% 
        group_by(受理届出コード) %>% 
        summarise(算定施設数 = n()) %>% 
        left_join(df_mst_todokede,by='受理届出コード') %>% 
        select(整理番号,受理届出名称,算定施設数) %>% 
        arrange(desc(算定施設数),整理番号)
    }
  })
  
  # 比較対象のtb作成
  output$tb_target_todokede_agg <- renderDT(
    mydatatable(rt_target_todokede_agg(), row = 10)
  )
  
  ##############################################################################

  # 比較対象が0施設の場合のメッセージ
  target_sisetu_message <- reactive({
    target_sisetu_count <- nrow(rt_target_sisetu4())
    if (target_sisetu_count == 0) {
      HTML('<span style="color: red;">比較対象が0施設です</span>')
    } else {
      HTML(str_glue('<span style="color: blue;">比較対象設定: {formatC(target_sisetu_count, format = "d", big.mark = ",")}施設</span>'))
    }
  })

  output$target_sisetu_message <- renderUI({
    target_sisetu_message()
  })
  # 別ページにも表示させる用にもう一つ作成
  output$target_sisetu_message2 <- renderUI({
    target_sisetu_message()
  })

  ##############################################################################

  # 比較対象の施設基準を抽出
  rt_target_todokede_all <- reactive({
    target_sisetu_count <- nrow(rt_target_sisetu4())
    if (target_sisetu_count == 0) {
      target_todokede <- tibble("整理番号"='',"受理届出名称" = '')
    } else {
      target_todokede <- df_latest_todokede %>%
        filter(医療機関コード %in% rt_target_sisetu4()$医療機関コード) %>%
        select(医療機関コード, 受理届出コード) %>%
        group_by(受理届出コード) %>%
        summarise(
          算定率 = n() / target_sisetu_count,
          算定施設数 = n()
        ) %>%
        inner_join(df_mst_todokede, by = "受理届出コード") %>%
        select(整理番号,受理届出名称,算定率,算定施設数) %>% 
        arrange(desc(算定施設数))
    }
    
    if (input$display_todokede=='基本診療料'){
      target_todokede %>% 
        filter(str_detect(整理番号,'^1'))
    }else if (input$display_todokede=='特掲診療料'){
      target_todokede %>% 
        filter(str_detect(整理番号,'^2'))
    }else{
      target_todokede
    }
  })
  
  ##############################################################################

  # # 自院と比較対象施設の集計表を結合
  # rt_compare_todokede <- reactive({
  #   rt_my_todokede() %>%
  #     mutate(自院算定 = "〇") %>%
  #     full_join(rt_target_todokede_all(), by = c("整理番号","受理届出名称")) %>%
  #     replace_na(list(
  #       自院算定 = "×", 算定施設数 = 0, 算定率 = 0
  #     )) %>%
  #     filter(受理届出名称 != "") %>%
  #     arrange(desc(算定率), 整理番号,受理届出名称) %>%
  #     filter(受理届出名称 != "なし")
  # })
  
  # 自院と比較対象施設の集計表を結合
  rt_compare_todokede <- reactive({
    rt_my_todokede() %>%
      mutate(ziin=1) %>%
      full_join(rt_target_todokede_all(), by = c("整理番号","受理届出名称")) %>%
      replace_na(list(ziin = 0, 算定施設数 = 0, 算定率 = 0)) %>%
      filter(受理届出名称 != "") %>%
      arrange(desc(算定率), 整理番号,受理届出名称) %>%
      filter(受理届出名称 != "なし") %>%
      left_join(df_mst_todokede_group,by=c("整理番号","受理届出名称")) %>%
      arrange(分類,分類番号) %>%
      group_by(分類) %>%
      mutate(ziin_cumsum = cumsum(ziin)) %>%
      ungroup() %>%
      filter(!(ziin_cumsum>0 & ziin==0)) %>%
      mutate(自院算定 = if_else(ziin==1,'〇','×')) %>%
      select(整理番号,受理届出名称,自院算定,算定率,算定施設数) %>% 
      arrange(desc(算定率), 整理番号,受理届出名称) %>%
      filter(受理届出名称 != "なし")
  })
  
  # 比較対象のtb作成
  output$tb_compare_todokede <- renderDT(
    mydatatable(rt_compare_todokede(), row = 15, pctcol = "算定率")
  )

  ##############################################################################

  # 絞り込み条件をまとめたdfを作成する
  rt_sidebar <- reactive({
    tibble(
      絞込条件 = c(
        "自院",
        "比較対象の厚生局",
        "比較対象の都道府県",
        "比較対象の施設名",
        "比較対象を施設基準で絞り込み",
        "施設基準絞込の検索条件"
      ),
      入力 = c(
        input$my_sisetu,
        input$target_kouseikyoku,
        input$target_pref,
        input$target_sisetu,
        input$target_todokede,
        input$todokede_kensaku
      )
    )
  })
  ##############################################################################
  # 施設基準比較データのダウンロード
  output$download_compare_todokede <- downloadHandler(
    filename = "施設基準比較.xlsx",
    content = function(file) {
      list(
        "施設基準比較" = rt_compare_todokede(),
        "絞込条件" = rt_sidebar(),
        "自院" = rt_my_sisetu(),
        "比較対象" = rt_target_sisetu4(),
        "使用データ" = mst_pref_update_date
      ) %>%
        writexl::write_xlsx(file)
    }
  )
  ##############################################################################
  
  # データダウンロードページの処理
  
  ##############################################################################
  
  # 厚生局ごとに、いつの時点のデータがDBに格納されているのかを表示するDTを作成
  output$tb_update_date_wide <- renderDT(
    DT::datatable(update_date_wide)
  )
  
  ##############################################################################
  
  # データダウンロードでダウンロードする医療機関一覧を作成
  rt_download_sisetu <- reactive({
    if(input$target_update_date=='各厚生局の最新時点'){
      tbl(con,'latest_sisetu_main') %>% 
        inner_join(tbl(con,'latest_sisetu_sub'),by=c('update_date','医療機関コード')) %>% 
        inner_join(tbl(con,'sisetu_bed'),by=c('update_date','医療機関コード')) %>% 
        inner_join(tbl(con,'mst_pref'),by=c('厚生局コード','都道府県コード')) %>% 
        select(医療機関コード,医療機関名称,都道府県名,住所,電話番号,病床数,総病床数=bed) %>% 
        collect() %>% 
        arrange(医療機関コード) 
    }else{
      tbl(con,'sisetu_main') %>% 
        filter(update_date == input$target_update_date) %>% 
        inner_join(tbl(con,'sisetu_sub'),by=c('update_date','医療機関コード')) %>% 
        inner_join(tbl(con,'sisetu_bed'),by=c('update_date','医療機関コード')) %>% 
        inner_join(tbl(con,'mst_pref'),by=c('厚生局コード','都道府県コード')) %>% 
        select(医療機関コード,医療機関名称,都道府県名,住所,電話番号,病床数,総病床数=bed) %>% 
        collect() %>% 
        arrange(医療機関コード) 
    }
  })
  
  # # データダウンロードでダウンロードする届出一覧を作成
  rt_download_todokede <- reactive({
    if(input$target_update_date=='各厚生局の最新時点'){
      tbl(con,'latest_todokede') %>% 
        inner_join(tbl(con,'mst_todokede'),by='受理届出コード') %>% 
        inner_join(select(tbl(con,'latest_sisetu_sub'),医療機関コード,医療機関名称),by='医療機関コード') %>% 
        select(医療機関コード,医療機関名称,受理届出名称,算定開始年月日=西暦算定開始年月日) %>% 
        collect() %>% 
        arrange(医療機関コード,受理届出名称)
    }else{
      tbl(con,'todokede') %>% 
        filter(update_date== input$target_update_date) %>% 
        inner_join(tbl(con,'mst_todokede'),by='受理届出コード') %>% 
        inner_join(select(tbl(con,'sisetu_sub'),update_date,医療機関コード,医療機関名称),by=c('update_date','医療機関コード')) %>% 
        select(医療機関コード,医療機関名称,受理届出名称,算定開始年月日=西暦算定開始年月日) %>% 
        collect() %>% 
        arrange(医療機関コード,受理届出名称)
    }
  })
  
  
  # データダウンロードでダウンロードするデータ時点一覧を作成
  rt_download_update_date <- reactive({
    
    # 厚生局ごとのupdate_dateを取得
    if(input$target_update_date=='各厚生局の最新時点'){
      kouseikyoku_update_date <- tbl(con,'mst_update_date') %>% 
        group_by(厚生局) %>% 
        summarise(update_date = max(update_date,na.rm=T)) %>% 
        collect()
    }else{
      kouseikyoku_update_date <- tbl(con,'mst_update_date') %>% 
        filter(update_date == input$target_update_date) %>% 
        collect()
    }
    # 都道府県マスタと結合して、データ時点一覧を作成
    df_mst_pref %>% 
      left_join(kouseikyoku_update_date,by='厚生局') %>% 
      replace_na(list(update_date='DB未格納')) %>% 
      arrange(厚生局コード,都道府県コード) %>% 
      select(厚生局,都道府県名,データ時点=update_date)
    
  })
  
  # データダウンロードでダウンロードするファイル名を作成
  rt_download_filename <- reactive({
    if(input$target_update_date=='各厚生局の最新時点'){
      str_glue('施設基準届出_各厚生局の最新時点.xlsx')
    }else{
      str_glue('施設基準届出_{input$target_update_date}時点.xlsx')
    }
  })
  
  # データダウンロードのダウンロード機能
  output$download_original_data <- downloadHandler(
    filename = function(){
      rt_download_filename()
    },
    content = function(file) {
      list(
        "医療機関一覧" = rt_download_sisetu(),
        "施設基準届出一覧" = rt_download_todokede(),
        "データ時点" = rt_download_update_date()
      ) %>%
        writexl::write_xlsx(file)
    }
  )
}
