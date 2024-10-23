ui <- 
  navbarPage(
    title='施設基準届出分析',
    ############################################################################
    tabPanel(
      title='TOP',
      sidebarLayout(
        sidebarPanel(
          width=3,
          h3('元データダウンロード'),
          selectInput(
            inputId='target_update_date',
            label='時点',
            choices=choices_update_dates,
          ),
          downloadButton('download_original_data',label='Download'),
        ),
        mainPanel(
          width=9,
          h3('概要'),
          p('本アプリは、施設基準届出のデータを検索・分析をするためのアプリです。'),
          p('「施設基準検索」では、対象の施設基準を届け出ている医療機関を検索できます。'),
          p('「施設基準他院比較」では、自院と他院の施設基準を比較することができます。'),
          p('「施設基準検索」・「施設基準他院比較」では各厚生局の最新データを用いています。'),
          h3('データベース格納データ'),
          p('2024年8月1日以降のデータを蓄積しています。'),
          p('週に1回程度、最新データを取得しデータベースを更新しています。'),
          p('左のDownloadボタンで、元データをダウンロードするができます。'),
          DTOutput('tb_update_date_wide'),
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
          p('(整理番号は、関東信越厚生局のものを使用しています)'),
          h3('問い合わせ先'),
          HTML("
                <ul>
                  <li>Twitter: <a href='https://x.com/akira1744' target='_blank'>@akira1744</a></li>
                  <li>GitHub: <a href='https://github.com/akira1744' target='_blank'>https://github.com/akira1744</a></li>
                </ul>
              "),
        ),
      ),
    ),
    ############################################################################
    tabPanel(
      title='施設基準検索',
      sidebarLayout(
        sidebarPanel(
          width=3,
          textInput(
            inputId='kensaku_word',
            label='検索ワード',
          ),
          selectInput(
            inputId='kensaku_method',
            label='検索方法',
            choices=c('前方一致','部分一致','完全一致'),
            selected = '前方一致',
          ),
          selectInput(
            inputId='kensaku_kouseikyoku',
            label='厚生局',
            choices=sidelist_kouseikyokus,
            selected='すべて'
          ),
          selectInput(
            inputId='kensaku_pref',
            label='都道府県',
            choices=sidelist_prefs,
            selected='すべて'
          ),
          sliderInput(
            inputId='kensaku_bed_range',
            label='総病床数',
            min=0,
            max=max_bed,
            value=c(0,max_bed),
            step=10,
          ),
          downloadButton('download_kensaku_todokede', label = 'Download')
        ),
        mainPanel(
          width=9,
          DTOutput('tb_kensaku_agg'),
          DTOutput('tb_kensaku_sisetu'),
        ),
      ),
    ),
    ############################################################################
    tabPanel(
      title='施設基準他院比較',
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
            label='施設基準絞込の検索方法',
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
            choices=c('すべて','基本診療料','特掲診療料'),
            selected = 'すべて'
          ),
          hr(style = "border: 1px solid gray;"),
          h4('データダウンロード'),
          downloadButton('download_compare_todokede',label='Download'),
        ),
        ########################################################################
        mainPanel(
          width=9,
          tabsetPanel(
            type='tabs',
            tabPanel(
              title='使用方法',
              h3('Step1: 自院設定'),
              p('・サイドバーに自院施設名を入力してください。'),
              p('・自院設定のタブで、選択された施設とその施設基準を確認できます。'),
              h3('Step2: 比較対象設定'),
              p('・サイドバーで比較対象設定を絞り込んでください。'),
              p('・比較対象設定のタブで、選択された施設を確認できます。'),
              h3('Step3: 施設基準比較'),
              p('・施設基準比較のタブで、自院と比較対象施設の施設基準の届出状況を比較することができます。'),
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
              title='施設基準の他院比較',
              ##################################################################
              # 2段構成に変更し,1段名は2列に変更
              ##################################################################
              # before
              uiOutput('my_sisetu_message2'),
              uiOutput('target_sisetu_message2'),
              DTOutput('tb_compare_todokede')
              ##################################################################
              # downloadボタンを右上に配置したときに使用したcodeを念のため残しておく
              ##################################################################
              # # 上段 2列構成
              # fluidRow(
              #   column(
              #     width = 10, align = 'left',
              #     uiOutput('my_sisetu_message2'),
              #     uiOutput('target_sisetu_message2')
              #   ),
              #   column(
              #     width = 2, align = 'right',
              #     downloadButton('download_compare_todokede', label = 'Download')
              #   )
              # ),
              # 
              # # 下段 1列構成
              # fluidRow(
              #   column(
              #     width = 12,
              #     DTOutput('tb_compare_todokede')
              #   )
              # )
              ##################################################################
            )
          )
        )
      )
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
