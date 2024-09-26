# カスタマイズDT関数
mydatatable <- function(df,pctcol,hidecol,select_hpname,row=15){
  
  if(missing(hidecol)){
    dt <- DT::datatable(
      data=df
      # ,filter='top'
      ,selection='none' # 行選択を無効にして、文字のcopyをしやすいUIに
      ,rownames=F # 行数を誤ってcopyすることがおおいので、非表示のUIに変更した
      # ,extensions = c('Buttons')
      # ,extensions = c('Buttons','KeyTable') # 拡張機能1:出力用のボタン,# 拡張機能:カーソル機能
      ,options=list(
        # dom='Bfrtip' # Buttonsのボタンの設置?
        # ,buttons=c('print') # Buttonsのボタンの種類を指定
        lengthMenu = list(
          c(5, 10, 15, 20, 25, 30, 40, 50),
          c('5', '10', '15', '20', '25', '30', '40', '50')
        )  # 行数の選択肢
        ,pageLength=row # 表示行数をセット
        ,keys=TRUE # KerTableに必要なoption
      )
    )  
  }else{
    dt <- DT::datatable(
      data=df
      # ,filter='top'
      ,selection='none'　# 行選択を無効にして、文字のcopyをしやすいUIに
      ,rownames=F # 行数を誤ってcopyすることがおおいので、非表示のUIに変更した
      # ,extensions = c('Buttons')
      # ,extensions = c('Buttons','KeyTable') # 拡張機能1:出力用のボタン,# 拡張機能:カーソル機能
      ,options=list(
        # dom='Bfrtip' # Buttonsのボタンの設置?
        # ,buttons=c('print') # Buttonsのボタンの種類を指定
        lengthMenu = list(
          c(5, 10, 15, 20, 25, 30, 40, 50),
          c('5', '10', '15', '20', '25', '30', '40', '50')
        )  # 行数の選択肢
        ,pageLength=row # 表示行数をセット
        ,keys=TRUE # KerTableに必要なoption
        ,columnDefs = list(list(targets=hidecol, visible=FALSE)) #列を非表示にする 
      )
    )
  }
  
  # パーセンテージ表示
  if (!missing(pctcol)){
    dt <- formatPercentage(dt,columns=pctcol,digits=1)
  }
  
  # dtの病院列がselect_hpと一致するとき,太字にする
  if (!missing(select_hpname)){
    dt <- formatStyle(
      dt,
      columns='病院',
      target = 'row',
      fontWeight = styleEqual(select_hpname, 'bold')
    )
  }
  
  return(dt)
}


# inputからregexを作成(前方一致)
make_beginning_match_regex <- function(input){
  if(input==''){
    return('^$')
  } else{
    return(str_glue('^{input}'))
  }
}
