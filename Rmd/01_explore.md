Exploring the structure of 国土数値情報 website
================

# Top page

``` r
library(polite)
library(rvest)

session <- bow("https://nlftp.mlit.go.jp/ksj/")

result <- scrape(session)
links <- result %>% 
  html_elements("main ul.collapsible a") %>% 
  html_attr("href")

print(links)
```

    ##   [1] "./gml/datalist/KsjTmplt-C23.html"        
    ##   [2] "./gml/datalist/KsjTmplt-P23.html"        
    ##   [3] "./gml/datalist/KsjTmplt-W09-v2_2.html"   
    ##   [4] "./gml/datalist/KsjTmplt-W07.html"        
    ##   [5] "./gml/datalist/KsjTmplt-W01.html"        
    ##   [6] "./gml/datalist/KsjTmplt-W05.html"        
    ##   [7] "./gml/datalist/KsjTmplt-G04-a.html"      
    ##   [8] "./gml/datalist/KsjTmplt-G04-c.html"      
    ##   [9] "./gml/datalist/KsjTmplt-G04-d.html"      
    ##  [10] "./gml/datalist/KsjTmplt-G08-v1_0.html"   
    ##  [11] "./gml/datalist/KsjTmplt-L03-a.html"      
    ##  [12] "./gml/datalist/KsjTmplt-L03-b.html"      
    ##  [13] "./gml/datalist/KsjTmplt-L03-b_r.html"    
    ##  [14] "./gml/datalist/KsjTmplt-L03-b-u.html"    
    ##  [15] "./gml/datalist/KsjTmplt-L03-b-c.html"    
    ##  [16] "./gml/datalist/KsjTmplt-A13.html"        
    ##  [17] "./gml/datalist/KsjTmplt-A45.html"        
    ##  [18] "./gml/datalist/KsjTmplt-A12.html"        
    ##  [19] "./gml/datalist/KsjTmplt-A09.html"        
    ##  [20] "./gml/datalist/KsjTmplt-A29-v2_1.html"   
    ##  [21] "./gml/datalist/KsjTmplt-L01-v2_5.html"   
    ##  [22] "./gml/datalist/KsjTmplt-L02-v2_7.html"   
    ##  [23] "./gml/datalist/KsjTmplt-N03-v2_4.html"   
    ##  [24] "./gml/datalist/KsjTmplt-A16-v2_3.html"   
    ##  [25] "./gml/datalist/KsjTmplt-A32-v2_0.html"   
    ##  [26] "./gml/datalist/KsjTmplt-A27-v2_1.html"   
    ##  [27] "./gml/datalist/KsjTmplt-A38.html"        
    ##  [28] "./gml/datalist/KsjTmplt-A35a.html"       
    ##  [29] "./gml/datalist/KsjTmplt-A35b.html"       
    ##  [30] "./gml/datalist/KsjTmplt-A35c.html"       
    ##  [31] "./gml/datalist/KsjTmplt-A42.html"        
    ##  [32] "./gml/datalist/KsjTmplt-A43.html"        
    ##  [33] "./gml/datalist/KsjTmplt-A44.html"        
    ##  [34] "./gml/datalist/KsjTmplt-A03.html"        
    ##  [35] "./gml/datalist/KsjTmplt-A17-v4_0.html"   
    ##  [36] "./gml/datalist/KsjTmplt-A24-v3_0.html"   
    ##  [37] "./gml/datalist/KsjTmplt-A25-v3_0.html"   
    ##  [38] "./gml/datalist/KsjTmplt-A19-v4_0.html"   
    ##  [39] "./gml/datalist/KsjTmplt-A19s.html"       
    ##  [40] "./gml/datalist/KsjTmplt-A21.html"        
    ##  [41] "./gml/datalist/KsjTmplt-A21s.html"       
    ##  [42] "./gml/datalist/KsjTmplt-A20.html"        
    ##  [43] "./gml/datalist/KsjTmplt-A20s.html"       
    ##  [44] "./gml/datalist/KsjTmplt-A18-v4_0.html"   
    ##  [45] "./gml/datalist/KsjTmplt-A18s-a.html"     
    ##  [46] "./gml/datalist/KsjTmplt-A37.html"        
    ##  [47] "./gml/datalist/KsjTmplt-A22-v3_0.html"   
    ##  [48] "./gml/datalist/KsjTmplt-A22-m.html"      
    ##  [49] "./gml/datalist/KsjTmplt-A22s.html"       
    ##  [50] "./gml/datalist/KsjTmplt-A23-v3_0.html"   
    ##  [51] "./gml/datalist/KsjTmplt-A39-v1_1.html"   
    ##  [52] "./gml/datalist/KsjTmplt-P20.html"        
    ##  [53] "./gml/datalist/KsjTmplt-G02.html"        
    ##  [54] "./gml/datalist/KsjTmplt-A30b.html"       
    ##  [55] "./gml/datalist/KsjTmplt-A30a5.html"      
    ##  [56] "./gml/datalist/KsjTmplt-A26.html"        
    ##  [57] "./gml/datalist/KsjTmplt-A33-v1_3.html"   
    ##  [58] "./gml/datalist/KsjTmplt-A31-v2_1.html"   
    ##  [59] "./gml/datalist/KsjTmplt-A40.html"        
    ##  [60] "./gml/datalist/KsjTmplt-P28.html"        
    ##  [61] "./gml/datalist/KsjTmplt-P05.html"        
    ##  [62] "./gml/datalist/KsjTmplt-P34.html"        
    ##  [63] "./gml/datalist/KsjTmplt-P02-v4_0.html"   
    ##  [64] "./gml/datalist/KsjTmplt-P18.html"        
    ##  [65] "./gml/datalist/KsjTmplt-P17.html"        
    ##  [66] "./gml/datalist/KsjTmplt-P30.html"        
    ##  [67] "./gml/datalist/KsjTmplt-P04-v2_1.html"   
    ##  [68] "./gml/datalist/KsjTmplt-P14.html"        
    ##  [69] "./gml/datalist/KsjTmplt-P27.html"        
    ##  [70] "./gml/datalist/KsjTmplt-P29.html"        
    ##  [71] "./gml/datalist/KsjTmplt-P13.html"        
    ##  [72] "./gml/datalist/KsjTmplt-P21.html"        
    ##  [73] "./gml/datalist/KsjTmplt-P22.html"        
    ##  [74] "./gml/datalist/KsjTmplt-P15.html"        
    ##  [75] "./gml/datalist/KsjTmplt-P03.html"        
    ##  [76] "./gml/datalist/KsjTmplt-P07-v2_1.html"   
    ##  [77] "./gml/datalist/KsjTmplt-P26.html"        
    ##  [78] "./gml/datalist/KsjTmplt-L05.html"        
    ##  [79] "./gml/datalist/KsjTmplt-P16.html"        
    ##  [80] "./gml/datalist/KsjTmplt-P24.html"        
    ##  [81] "./gml/datalist/KsjTmplt-P31.html"        
    ##  [82] "./gml/datalist/KsjTmplt-P33.html"        
    ##  [83] "./gml/datalist/KsjTmplt-P35.html"        
    ##  [84] "./gml/datalist/KsjTmplt-P32.html"        
    ##  [85] "./gml/datalist/KsjTmplt-A34-v1_2.html"   
    ##  [86] "./gml/datalist/KsjTmplt-A28.html"        
    ##  [87] "./gml/datalist/KsjTmplt-P12-v2_2.html"   
    ##  [88] "./gml/datalist/KsjTmplt-P09.html"        
    ##  [89] "./gml/datalist/KsjTmplt-P19.html"        
    ##  [90] "./gml/datalist/KsjTmplt-A10-v3_1.html"   
    ##  [91] "./gml/datalist/KsjTmplt-A11.html"        
    ##  [92] "./gml/datalist/KsjTmplt-A15.html"        
    ##  [93] "./gml/datalist/KsjTmplt-N06-v1_2.html"   
    ##  [94] "./gml/datalist/KsjTmplt-N10-v1_1.html"   
    ##  [95] "./gml/datalist/KsjTmplt-N04.html"        
    ##  [96] "./gml/datalist/KsjTmplt-P11.html"        
    ##  [97] "./gml/datalist/KsjTmplt-N07.html"        
    ##  [98] "./gml/datalist/KsjTmplt-N02-v2_3.html"   
    ##  [99] "./gml/datalist/KsjTmplt-N05-v1_3.html"   
    ## [100] "./gml/datalist/KsjTmplt-S12-v2_6.html"   
    ## [101] "./gml/datalist/KsjTmplt-S05-c.html"      
    ## [102] "./gml/datalist/KsjTmplt-C28-v2_4.html"   
    ## [103] "./gml/datalist/KsjTmplt-N08-v1_4.html"   
    ## [104] "./gml/datalist/KsjTmplt-S10b-v1_1.html"  
    ## [105] "./gml/datalist/KsjTmplt-N11.html"        
    ## [106] "./gml/datalist/KsjTmplt-C02-v3_2.html"   
    ## [107] "./gml/datalist/KsjTmplt-C09.html"        
    ## [108] "./gml/datalist/KsjTmplt-S10a-v1_2.html"  
    ## [109] "./gml/datalist/KsjTmplt-N09.html"        
    ## [110] "./gml/datalist/KsjTmplt-S05-a-v2_2.html" 
    ## [111] "./gml/datalist/KsjTmplt-S05-b-v2_2.html" 
    ## [112] "./gml/datalist/KsjTmplt-S05-d-v2_2.html" 
    ## [113] "./gml/datalist/KsjTmplt-mesh1000.html"   
    ## [114] "./gml/datalist/KsjTmplt-mesh500.html"    
    ## [115] "./gml/datalist/KsjTmplt-mesh1000h30.html"
    ## [116] "./gml/datalist/KsjTmplt-mesh500h30.html"

# Metadata page

`nod()`にパスを渡したら相対パスもいい感じに処理してくれるのかと思いきやそんなことはなかったので自分でなんとかする。

``` r
library(stringr)
links <- str_replace(links, fixed("./"), "/ksj/")
```

``` r
page <- nod(session, links[1]) %>% 
  scrape(verbose = TRUE)
```

地物名と属性名は`<table class="tablelist">`の中にある`<table>`。
まずは行ごと取ってくる。

``` r
info <- page %>% 
  html_elements("table.tablelist tr") %>%
  purrr::keep(~ {
    length(html_element(.x, "table")) > 0
  })

info
```

    ## {xml_nodeset (2)}
    ## [1] <tr>\n<th>地物情報</th>\r\n\t\t\t\t\t\t\t\t\t\t<td>\r\n\t\t\t\t\t\t\t\t\t\t\t ...
    ## [2] <tr>\n<th>属性情報</th>\r\n\t\t\t\t\t\t\t\t\t\t<td>\r\n\t\t\t\t\t\t\t\t\t\t\t ...

`<th>`の部分がテーブル名。`<td>`に入っているテーブルは`html_table()`で読み出す。

``` r
tables <- info %>% 
  purrr::map(~ {
    list(
      name = html_text2(html_element(.x, "th")),
      table = html_table(.x)
    )
  })

tables %>% 
  purrr::walk(~ {
    cat(glue::glue("### {.x$name}\n"))
    print(knitr::kable(.x$table))
  })
```

### 地物情報

| 地物名 | 説明 |
|:-------|:-----|
| 海岸線 | NA   |

### 属性情報

| 属性名                     | 説明                                                                                                                                                                                                                                                                                                                 | 属性の型                                       |
|:---------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------|
| 場所                       | 海岸線の位置 陸と海との境界を言う。海岸線は海の干満によって変化する。地形図に記載された海岸線は満潮時のものである。                                                                                                                                                                                                  | 曲線型（GM\_Curve）                            |
| 行政区域コード             | 都道府県コードと市区町村コードからなる、行政区を特定するためのコード                                                                                                                                                                                                                                                 | コードリスト「行政コード」                     |
| 所管官庁                   | 海岸線を所管する官庁を特定するコード                                                                                                                                                                                                                                                                                 | コードリスト「所管官庁コード」                 |
| 海岸保全区域番号           | 海岸法に基づく海岸保全区域の番号                                                                                                                                                                                                                                                                                     | 整数型　※不明の場合、“9999”とする。            |
| 海岸保全区域・海岸名       | 海岸保全区域の海岸名称海岸保全区域とは、海岸法に基づき津波、高潮、波浪その他海水又は地盤の変動による被害から海岸を保護し、国土の保全に資するため必要があると認めて都道府県知事が指定した一定の海岸の区域をいう。原則として、陸地においては満潮時の水際線から50ｍ、水面においては干潮時の水際線から50ｍとされている。 | 文字列型                                       |
| 海岸保全区域・海岸管理者名 | 海岸保全区域の海岸管理者を区分するためのコード                                                                                                                                                                                                                                                                       | コードリスト「海岸保全区域・海岸管理者コード」 |
| 河口                       | 河口部かどうかの区別。                                                                                                                                                                                                                                                                                               | 真偽値型（true：河口部、false：その他）        |
