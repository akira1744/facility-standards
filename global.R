pacman::p_load(DT,DBI,duckdb,duckplyr,arrow,tidyverse,shiny)

source('myfunc.R')

con <- DBI::dbConnect(duckdb::duckdb(),'sisetukijun.duckdb',read_only=TRUE)

################################################################################

# sidebarの準備

################################################################################

# 厚生局のdf
df_mst_kouseikyoku <- tbl(con,'mst_kouseikyoku') %>% 
  collect() %>% 
  arrange(厚生局コード) %>% 
  pull(厚生局)

sidelist_kouseikyokus <- c('すべて',df_mst_kouseikyoku)

# 都道府県のdf
df_mst_pref <- tbl(con,'mst_pref') %>% 
  inner_join(tbl(con,'mst_kouseikyoku'),by='厚生局コード') %>% 
  arrange(厚生局コード,都道府県コード) %>% 
  select(厚生局コード,厚生局,都道府県コード,都道府県名) %>% 
  collect()

# df_mst_pref

sidelist_prefs <- c('すべて',df_mst_pref$都道府県名)

# 届出マスタ
df_mst_todokede <- tbl(con,'mst_todokede') %>%
  select(受理届出コード,整理番号,受理届出名称) %>%
  collect() 

# df_mst_todokede

sidelist_todokede <- df_mst_todokede %>% 
  filter(受理届出名称!='なし') %>% 
  pull(受理届出名称) %>% 
  c('',.)

# server処理用にsisetuのdfを準備
df_latest_sisetu <- tbl(con,'latest_sisetu_main') %>% 
  inner_join(tbl(con,'latest_sisetu_sub'),by=c('update_date','医療機関コード')) %>% 
  inner_join(tbl(con,'sisetu_bed'),by=c('update_date','医療機関コード')) %>% 
  inner_join(tbl(con,'mst_pref'),by='都道府県コード') %>% 
  select(医療機関コード,施設名,都道府県名,住所,病床数,総病床数=bed) %>% 
  arrange(医療機関コード) %>% 
  collect()

# df_latest_sisetu

# tbl(con,'latest_sisetu_main') %>% 
#   filter(str_detect(施設名,'北海道大学'))
# 
# tbl(con,'latest_sisetu_sub') %>% 
#   filter(str_detect(医療機関名称,'北海道大学'))
# 
# tbl(con,'sisetu_bed') %>% 
#   filter(医療機関コード=='0118010016')

# df_latest_sisetu

max_bed <- max(df_latest_sisetu$総病床数)

# sidebarの施設名一覧
sidelist_sisetu <- df_latest_sisetu %>% 
  pull(施設名) %>% 
  c('',.)

# server処理用にtodokedeのdfを準備
df_latest_todokede <- tbl(con,'latest_todokede') %>% 
  select(医療機関コード,受理届出コード) %>% 
  collect()

################################################################################

# df_mst_todokede
# df_latest_todokede
# df_latest_sisetu

################################################################################

# 元データページ用

################################################################################

# 過去データダウンロードで使用するupdate_dateのリスト
update_dates <- tbl(con,'mst_update_date') %>% 
  distinct(update_date) %>% 
  collect() %>% 
  arrange(desc(update_date)) %>% 
  pull(update_date)

# ダウンロード対象を選択するための選択肢
choices_update_dates <- c('各厚生局の最新時点',update_dates)

# 厚生局ごとにいつのデータが入っているのかを示すdf
update_date_wide <- tbl(con,'mst_update_date') %>% 
  left_join(tbl(con,'mst_kouseikyoku'),by='厚生局') %>% 
  collect() %>% 
  mutate(value='〇') %>% 
  arrange(厚生局コード) %>% 
  pivot_wider(
    id_cols=update_date
    ,names_from='厚生局'
    ,values_from = value
    ,values_fill='×'
  ) %>% 
  arrange(desc(update_date)) %>% 
  rename(時点=update_date)

################################################################################

# 最新データの時点を示すために、mst_pref_update_dateを作成
mst_pref_update_date <- tbl(con,'mst_update_date') %>% 
  group_by(厚生局) %>% 
  summarise(update_date = max(update_date,na.rm=T)) %>%
  inner_join(tbl(con,'mst_kouseikyoku'),by='厚生局') %>%
  inner_join(tbl(con,'mst_pref'),by='厚生局コード') %>%
  arrange(厚生局コード,都道府県コード) %>% 
  select(厚生局,都道府県名,時点=update_date) %>% 
  collect() 

################################################################################

# compare_todokedeの処理ロジック変更のために検証

################################################################################

# 届出Groupをmemoryに載せる
df_mst_todokede_group <- tbl(con,'mst_todokede_group') %>% 
  select(整理番号,受理届出名称,分類,分類番号) %>% 
  collect() %>% 
  print()
# 
# df_latest_sisetu %>% 
#   filter(str_detect(施設名,'埼玉石心会'))
# 
# my_codes <- '1112702298'
# 
# my_todokede <- df_latest_todokede %>%
#   filter(医療機関コード %in% my_codes) %>%
#   select(受理届出コード) %>%
#   inner_join(df_mst_todokede, by = "受理届出コード") %>%
#   select(整理番号,受理届出名称) %>% 
#   arrange(整理番号) %>% 
#   print()
# 
# target_sisetu_count <- nrow(df_latest_todokede)
# 
# target_todokede_all <- df_latest_todokede %>%
#   select(医療機関コード, 受理届出コード) %>%
#   group_by(受理届出コード) %>%
#   summarise(
#     算定率 = n() / target_sisetu_count,
#     算定施設数 = n()
#   ) %>%
#   inner_join(df_mst_todokede, by = "受理届出コード") %>%
#   select(整理番号,受理届出名称,算定率,算定施設数) %>% 
#   arrange(desc(算定施設数)) %>% 
#   print()

# my_todokede %>%
#   mutate(ziin=1) %>% 
#   full_join(target_todokede_all, by = c("整理番号","受理届出名称")) %>%
#   replace_na(list(
#     ziin = 0, 算定施設数 = 0, 算定率 = 0
#   )) %>%
#   filter(受理届出名称 != "") %>%
#   arrange(desc(算定率), 整理番号,受理届出名称) %>%
#   filter(受理届出名称 != "なし") %>% 
#   left_join(df_mst_todokede_group,by=c("整理番号","受理届出名称")) %>% 
#   arrange(分類,分類番号) %>% 
#   group_by(分類) %>% 
#   mutate(ziin_cumsum = cumsum(ziin)) %>% 
#   ungroup() %>% 
#   filter(!(ziin_cumsum>0 & ziin==0)) %>% 
#   mutate(自院算定 = if_else(ziin==1,'〇','×')) %>% 
#   select(整理番号,受理届出名称,自院算定,算定率,算定施設数) %>% 
#   print()
  
# 
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


################################################################################

# # table一覧
# DBI::dbListTables(con)

################################################################################

# 格納データシート

# df_mst_pref
# tbl(con,'mst_update_date')
# 
# target_update_date <- '2024-09-01'
# 
# # 最新の場合
# kouseikyoku_update_date <- tbl(con,'mst_update_date') %>% 
#   group_by(厚生局) %>% 
#   summarise(update_date = max(update_date,na.rm=T)) %>% 
#   collect()
# 
# # 最新以外の場合
# kouseikyoku_update_date <- tbl(con,'mst_update_date') %>% 
#   filter(update_date == target_update_date) %>% 
#   collect()
# 
# kouseikyoku_update_date
# 
# df_mst_pref %>% 
#   left_join(kouseikyoku_update_date,by='厚生局') %>% 
#   replace_na(list(update_date='DB未格納')) %>% 
#   arrange(厚生局コード,都道府県コード) %>% 
#   select(厚生局,都道府県名,データ時点=update_date)

