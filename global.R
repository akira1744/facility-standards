pacman::p_load(DT,DBI,duckdb,duckplyr,tidyverse,shiny)

source('myfunc.R')

con <- DBI::dbConnect(duckdb::duckdb(),'sisetukijun.duckdb',read_only=TRUE)

################################################################################

# table一覧
# DBI::dbListTables(con)

################################################################################

# 厚生局のdf
mst_kouseikyoku <- tbl(con,'mst_kouseikyoku') %>% 
  collect() %>% 
  arrange(厚生局コード) %>% 
  pull(厚生局)

sidelist_kouseikyokus <- c('すべて',mst_kouseikyoku)

# 厚生局ごとの最新のupdate_date
mst_kouseikyoku_update_date <- tbl(con,'mst_kouseikyoku_update_date') %>% 
  group_by(厚生局コード) %>% 
  summarise(update_date = max(update_date,na.rm=T)) %>% 
  inner_join(tbl(con,'mst_kouseikyoku'),by='厚生局コード') %>% 
  arrange(厚生局コード) %>% 
  select(厚生局,使用データ = update_date) %>% 
  collect()

# マニュアルページ用に使用データの文字列を作る
str_kouseikyoku_update_date <- mst_kouseikyoku_update_date %>% 
  mutate(text = str_glue('・{厚生局}厚生局: {使用データ}')) 

# 都道府県のdf
df_mst_pref <- tbl(con,'mst_pref') %>% 
  inner_join(tbl(con,'mst_kouseikyoku'),by='厚生局コード') %>% 
  arrange(厚生局コード,都道府県コード) %>% 
  select(厚生局コード,厚生局,都道府県コード,都道府県名) %>% 
  collect()

sidelist_prefs <- c('すべて',df_mst_pref$都道府県名)

mst_pref_update_date <- mst_kouseikyoku_update_date %>% 
  inner_join(df_mst_pref,by='厚生局') %>% 
  arrange(厚生局コード,都道府県コード) %>% 
  select(厚生局,都道府県名,使用データ) 

# 届出マスタ
df_mst_todokede <- tbl(con,'mst_todokede') %>%
  select(受理届出コード,受理届出名称) %>%
  collect() 

# df_mst_todokede

sidelist_todokede <- df_mst_todokede %>% 
  filter(受理届出名称!='なし') %>% 
  pull(受理届出名称) %>% 
  c('',.)

# sidelist_todokede

# 施設のdf
mst_latest_sisetu <- tbl(con,'latest_sisetu_main') %>% 
  inner_join(tbl(con,'latest_sisetu_sub'),by=c('update_date','医療機関コード')) %>% 
  select(update_date,医療機関コード,施設名,医療機関名称,都道府県コード,住所,電話番号,病床数) %>% 
  collect() %>% 
  left_join(df_mst_pref,by='都道府県コード') %>% 
  arrange(厚生局コード,都道府県コード,施設名) %>% 
  select(医療機関コード,施設名,医療機関名称,都道府県名,住所,電話番号,病床数) 

# sidebarの施設名一覧
sidelist_sisetu <- mst_latest_sisetu %>% 
  pull(施設名) %>% 
  c('',.)

# 処理用
df_latest_sisetu <- mst_latest_sisetu %>% 
  select(医療機関コード,施設名,都道府県名,住所) 

# 元データ提供用は施設名列を消しておく
mst_latest_sisetu <- mst_latest_sisetu %>% 
  select(医療機関コード,医療機関名称,都道府県名,住所,電話番号,病床数)

# 最新の届出のdf
mst_latest_todokede <- tbl(con,'latest_todokede') %>% 
  select(医療機関コード,受理届出コード,西暦算定開始年月日) %>% 
  collect()

df_latest_todokede <- mst_latest_todokede %>% 
  select(医療機関コード,受理届出コード) 

# 元データ提供用の届出一覧
mst_latest_todokede <- mst_latest_todokede %>% 
  inner_join(df_mst_todokede,by='受理届出コード') %>% 
  inner_join(select(mst_latest_sisetu,医療機関コード,医療機関名称),by='医療機関コード') %>% 
  select(医療機関コード,医療機関名称,受理届出名称,算定開始年月日=西暦算定開始年月日)

# mst_latest_todokede
