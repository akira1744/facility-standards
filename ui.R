ui <- 
  navbarPage(
    title='施設基準分析',
    tabPanel(
      title='施設基準比較',
      sidebarLayout(
        sidebarPanel(
          width=3,
          h4('自院設定'),
          ######################################################################
          # DefaultのselectizeInputで全角入力をすると、入力の挙動がおかしくなるときがあった。
          # selectizeInputが選択された時に内容を削除するようにしたところ、入力の挙動がおかしくなることがなくなった。
          tags$head(
            tags$script(HTML("
              $(document).on('shiny:connected', function(event) {
                // マウスクリックによるフォーカスを検出するフラグ
                var clicked = false;
        
                $('#my_sisetu').next('.selectize-control').mousedown(function() {
                  clicked = true;
                });
        
                $('#my_sisetu').selectize()[0].selectize.on('focus', function() {
                  if (clicked) {
                    this.clear();
                    clicked = false; // フラグをリセット
                  }
                });
                // キーボード操作によるフォーカスではフラグをリセット
                $('#my_sisetu').next('.selectize-control').keydown(function() {
                  clicked = false;
                });
              });
            "))
          ),
          ######################################################################
          selectizeInput(
            inputId='my_sisetu',
            label='自院施設名',
            choices=NULL,
          ),
          ######################################################################
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
          ######################################################################
          # DefaultのselectizeInputで全角入力をすると、入力の挙動がおかしくなるときがあった。
          # selectizeInputが選択された時に内容を削除するようにしたところ、入力の挙動がおかしくなることがなくなった。
          tags$head(
            tags$script(HTML("
              $(document).on('shiny:connected', function(event) {
                // マウスクリックによるフォーカスを検出するフラグ
                var clicked = false;
        
                $('#target_sisetu').next('.selectize-control').mousedown(function() {
                  clicked = true;
                });
        
                $('#target_sisetu').selectize()[0].selectize.on('focus', function() {
                  if (clicked) {
                    this.clear();
                    clicked = false; // フラグをリセット
                  }
                });
                // キーボード操作によるフォーカスではフラグをリセット
                $('#target_sisetu').next('.selectize-control').keydown(function() {
                  clicked = false;
                });
              });
            "))
          ),
          ######################################################################
          selectizeInput(
            inputId='target_sisetu',
            label='比較対象施設名',
            choices=NULL,
          ),

          ######################################################################
          h4('比較対象を施設基準で絞込'),
          textInput(
            inputId='target_todokede',
            label='施設基準',
          ),
          selectInput(
            inputId='todokede_kensaku',
            label='施設基準絞込の検索条件',
            choices=c('前方一致','部分一致','完全一致'),
            selected = '前方一致',
          ),
          ######################################################################
          sliderInput(
            inputId='bed_range',
            label='総病床数',
            min=0,
            max=max_bed,
            value=c(0,max_bed),
            step=10,
          ),
          ########################################################################
          hr(style = "border: 1px solid gray;"),
          selectInput(
            inputId='display_todokede',
            label='表示対象の施設基準',
            choices=c('基本診療料','特掲診療料','すべて'),
            selected = '基本診療料'
          ),
        ),
        ########################################################################
        mainPanel(
          width=9,
          tabsetPanel(
            type='tabs',
            tabPanel(
              title='マニュアル',
              h3('概要'),
              p('本アプリは施設基準を分析するためのアプリです。'),
              p('「施設基準比較」では、自院と比較対象の施設基準を比較することができます。'),
              p('「施設基準比較」は各厚生局の最新のデータを使用しています。'),
              p('「データダウンロード」では、2024/8/1以降の全厚生局の施設基準届出データをダウンロードすることができます。'),
              h3('施設基準比較の使用方法'),
              p('1.自院設定'),
              p('・自院施設名を選択してください。'),
              p('・自院設定のタブで、選択された施設とその施設基準を確認できます。'),
              p('2.比較対象設定'),
              p('・厚生局・都道府県・施設名・施設基準で比較対象を絞り込んでください。'),
              p('・比較対象設定のタブで、選択された施設を確認できます。'),
              p('・算定している施設基準で、比較対象を絞り込むことも可能です。'),
              p('3.施設基準比較'),
              p('・施設基準比較のタブで、自院と比較対象施設の施設基準の届出状況を比較することができます。'),
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
              p('・整理番号は、関東信越厚生局のものを使用しています。'),
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
              h3('比較対象を施設基準で絞込'),
              DTOutput('tb_target_todokede_agg')
            ),
            tabPanel(
              title='施設基準比較',
              ##################################################################
              # 2段構成に変更し,1段名は2列に変更
              ##################################################################
              # before
              # uiOutput('my_sisetu_message2'),
              # uiOutput('target_sisetu_message2'),
              # downloadButton('download_compare_todokede',label='Download'),
              # DTOutput('tb_compare_todokede')
              ##################################################################
              # 上段 2列構成
              fluidRow(
                column(
                  width = 10, align = 'left',
                  uiOutput('my_sisetu_message2'),
                  uiOutput('target_sisetu_message2')
                ),
                column(
                  width = 2, align = 'right',
                  downloadButton('download_compare_todokede', label = 'Download')
                )
              ),
              
              # 下段 1列構成
              fluidRow(
                column(
                  width = 12,
                  DTOutput('tb_compare_todokede')
                )
              )
              ##################################################################
            )
          )
        )
      )
    ),
    tabPanel(
      title='データダウンロード',
      sidebarLayout(
        sidebarPanel(
          width=3,
          h3('ダウンロード対象'),
          selectInput(
            inputId='target_update_date',
            label='時点',
            choices=choices_update_dates,
          ),
          downloadButton('download_original_data',label='Download'),
        ),
        mainPanel(
          width=9,
          h3('ダウンロード可能データ'),
          DTOutput('tb_update_date_wide')
        ),
      ),
    )
    # ##########################################################################
    # tabPanelのフォーマット
    # tabPanel(
    #   title='',
    #   sidebarLayout(
    #     sidebarPanel(
    #       width=3,
    #     ),
    #     mainPanel(
    #       width=9,
    #       tabsetPanel(
    #         type='tabs',
    #         tabPanel(
    #           title='',
    #         ),
    #       ),
    #     ),
    #   ),
    # )
    # ##########################################################################
  )
