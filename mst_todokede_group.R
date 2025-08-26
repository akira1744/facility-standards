rm(list=ls())

pacman::p_load(
  here
  ,DBI
  ,writexl
  ,lubridate
  ,duckdb
  ,duckplyr
  ,arrow
  ,tidyverse
  ,tidylog
)

output_dir <- here('output') %>% print()

################################################################################

# normalize後のdataを格納するdb
db_path <- 'sisetukijun.duckdb'

con <- dbConnect(duckdb(),db_path,read_only=TRUE)

# table一覧を確認
db_tables <- DBI::dbListTables(con) %>% print()

################################################################################

mst_todokede<- tbl(con,"mst_todokede") %>% 
  collect()


# Close database connection
dbDisconnect(con)

# 基本診療料
mst_todokede %>% 
  filter(str_detect(整理番号,'^1'))

# 特掲診療料
mst_todokede %>% 
  filter(str_detect(整理番号,'^2'))

mst_todokede %>% 
  write_xlsx('mst_todokede.xlsx')


rm(list=ls())

pacman::p_load(
  here
  ,DBI
  ,writexl
  ,lubridate
  ,duckdb
  ,duckplyr
  ,arrow
  ,tidyverse
  ,tidylog
)

output_dir <- here('output') %>% print()

################################################################################

# normalize後のdataを格納するdb
db_path <- 'sisetukijun.duckdb'

con <- dbConnect(duckdb(),db_path,read_only=TRUE)

# table一覧を確認
db_tables <- DBI::dbListTables(con) %>% print()

################################################################################

mst_todokede<- tbl(con,"mst_todokede") %>% 
  collect()

# Close database connection
dbDisconnect(con)

# 基本診療料
mst_todokede %>% 
  filter(str_detect(整理番号,'^1'))

# 特掲診療料
mst_todokede %>% 
  filter(str_detect(整理番号,'^2'))

mst_todokede %>% 
  write_xlsx('mst_todokede.xlsx')

################################################################################

mst_todokede_ranked <- mst_todokede %>% 
  mutate(rank = case_when(
    受理記号=='ＡＡＶ９' ~ NA_character_
    ,str_detect(受理記号, "[１-９]$") ~ str_extract(受理記号, "[１-９]$")
    ,str_detect(受理記号, "[Ⅰ-Ⅸ]$") ~ str_extract(受理記号, "[Ⅰ-Ⅸ]$")
    ,受理記号=='がん指イ' ~ '１'
    ,受理記号=='がん指ロ' ~ '２'
    ,受理記号=='がん指ハ' ~ '３'
    ,受理記号=='がん指ニ' ~ '４'
  ))

mst_todokede_ranked %>% 
  filter(str_detect(受理届出名称,'輸血管理料'))

mst_todokede_ranked %>% 
  count(rank)

mst_todokede_ranked %>% 
  write_xlsx('mst_todokede_ranked.xlsx')

