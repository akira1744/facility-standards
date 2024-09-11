ui <- 
  navbarPage(
    title='施設基準分析',
    tabPanel(
      title='施設基準比較',
      sidebarLayout(
        sidebarPanel(
          width=2,
          h4('自院設定'),
          textInput(
            inputId='my_sisetu',
            label='自院施設名',
            value='' 
          ),
          hr(style = "border: 1px solid gray;"),
          h4('比較対象設定'),
          selectInput(
            inputId='target_kouseikyoku',
            label='厚生局',
            choices=sidelist_kouseikyokus,
            selected='すべて'
          ),
          selectInput(
            inputId='target_pref',
            label='都道府県',
            choices=sidelist_prefs,
            selected='すべて'
          ),
          textInput(
            inputId='target_sisetu',
            label='比較対象施設名',
            value='' 
          ),
          textInput(
            inputId='target_todokede1',
            label='施設基準で絞込(1)',
            value=''
          ),
          textInput(
            inputId='target_todokede2',
            label='& 施設基準で絞込(2)',
            value=''
          ),
          textInput(
            inputId='target_todokede3',
            label='& 施設基準で絞込(3)',
            value=''
          ),
        ),
        mainPanel(
          width=10,
          tabsetPanel(
            type='tabs',
            tabPanel(
              title='マニュアル',
              h3('概要'),
              p('本アプリは施設基準を分析するためのアプリです。'),
              p('自院と比較対象の医療機関を設定し、施設基準の届出有無を比較することができます。'),
              h3('使用方法'),
              p('1.自院設定'),
              p('・自院施設名を入力してください'),
              p('・自院設定のタブで、選択された施設とその施設基準を確認できます'),
              p('2.比較対象設定'),
              p('・厚生局・都道府県・施設名・施設基準で比較対象を絞り込んでください。'),
              p('・比較対象設定のタブで、選択された施設を確認できます'),
              p('・例:施設基準で絞込(1)に特定集中治療室管理料１すれば、比較対象を特定集中治療室管理料１を届出ている施設に絞り込むことができます。'),
              p('・施設基準で絞込を複数入力した場合、＆条件で比較対象施設を絞り込みます。'),
              p('・施設基準で絞込の文字列検索は前方一致検索です'),
              p('3.施設基準比較'),
              p('・施設基準比較のタブで、自院と比較対象施設の施設基準の届出状況を比較することができます。'),
              p('・最下部のDownloadボタンで比較結果を出力することも可能です。'),
              h3('データソース'),
              a("・北海道厚生局", href = "https://kouseikyoku.mhlw.go.jp/hokkaido/gyomu/gyomu/hoken_kikan/todokede_juri_ichiran.html", target = "_blank"),
              br(),
              a("・東北厚生局", href = "https://kouseikyoku.mhlw.go.jp/tohoku/gyomu/gyomu/hoken_kikan/documents/201805koushin.html", target = "_blank"),
              br(),
              a("・関東信越厚生局", href = "https://kouseikyoku.mhlw.go.jp/kantoshinetsu/chousa/kijyun.html", target = "_blank"),
              br(),
              a("・東海北陸厚生局", href = "https://kouseikyoku.mhlw.go.jp/tokaihokuriku/newpage_00349.html", target = "_blank"),
              br(),
              a("・近畿厚生局", href = "https://kouseikyoku.mhlw.go.jp/kinki/gyomu/gyomu/hoken_kikan/shitei_jokyo_00004.html", target = "_blank"),
              br(),
              a("・中国四国厚生局", href = "https://kouseikyoku.mhlw.go.jp/chugokushikoku/chousaka/shisetsukijunjuri.html", target = "_blank"),
              br(),
              a("・四国厚生局", href = "https://kouseikyoku.mhlw.go.jp/shikoku/gyomu/gyomu/hoken_kikan/shitei/index.html", target = "_blank"),
              br(),
              a("・九州厚生局", href = "https://kouseikyoku.mhlw.go.jp/kyushu/gyomu/gyomu/hoken_kikan/index_00007.html", target = "_blank"),
              br(),
              h3('本アプリが使用しているデータ'),
              p('本アプリでは各厚生局の最新のデータを使用しています。(更新頻度は週に1回程度)'),
              uiOutput('kouseikyoku_update_date'),
              h3('全件データダウンロード'),
              downloadButton('download_original_data',label='Download'),
              h3('問い合わせ先'),
              HTML("
                <ul>
                  <li>Twitter: <a href='https://x.com/akira1744' target='_blank'>@akira1744</a></li>
                  <li>GitHub: <a href='https://github.com/akira1744' target='_blank'>https://github.com/akira1744</a></li>
                </ul>
              "),
            ),
            tabPanel(
              title='自院設定',
              h3('自院'),
              uiOutput('my_sisetu_message'),
              DTOutput('tb_my_sisetu'),
              h3('自院施設基準'),
              DTOutput('tb_my_todokede')
            ),
            tabPanel(
              title='比較対象設定',
              h3('比較施設'),
              uiOutput('target_sisetu_message'),
              DTOutput('tb_target_sisetu'),
              hr(),
              h3('施設基準で絞込(1)'),
              DTOutput('tb_target_todokede1'),
              h3('& 施設基準で絞込(2)'),
              DTOutput('tb_target_todokede2'),
              h3('& 施設基準で絞込(3)'),
              DTOutput('tb_target_todokede3'),
            ),
            tabPanel(
              title='施設基準比較',
              uiOutput('my_sisetu_message2'),
              uiOutput('target_sisetu_message2'),
              DTOutput('tb_compare_todokede'),
              br(),
              downloadButton('download_compare_todokede',label='Download'),
              br()
            )
          )
        )
      )
    )
  )
