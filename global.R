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

sidelist_prefs <- c('すべて',df_mst_pref$都道府県名)

# 届出マスタ
df_mst_todokede <- tbl(con,'mst_todokede') %>%
  select(受理届出コード,整理番号,受理届出名称) %>%
  collect() 

sidelist_todokede <- df_mst_todokede %>% 
  filter(受理届出名称!='なし') %>% 
  pull(受理届出名称) %>% 
  c('',.)

# server処理用にsisetuのdfを準備
df_latest_sisetu <- tbl(con,'latest_sisetu_main') %>% 
  inner_join(tbl(con,'latest_sisetu_sub'),by=c('update_date','医療機関コード')) %>% 
  inner_join(tbl(con,'sisetu_bed',by=c('update_date','医療機関コード'))) %>% 
  inner_join(tbl(con,'mst_pref'),by='都道府県コード') %>% 
  select(医療機関コード,施設名,都道府県名,住所,病床数,総病床数=bed) %>% 
  collect()

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

# 元データページ用

################################################################################

# 過去データダウンロードで使用するupdate_dateのリスト
update_dates <- tbl(con,'mst_update_date') %>% 
  distinct(update_date) %>% 
  collect() %>% 
  arrange(desc(update_date)) %>% 
  pull(update_date)

# ダウンロード対象を選択するための選択肢
choices_update_dates <- c('最新',update_dates)

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

# table一覧
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

