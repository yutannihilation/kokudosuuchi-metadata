Format of datalist pages
================

ページのフォーマットには4種類ある

1.  「地物情報」「属性情報」の2つのテーブルが入れ子になって入っている（C23のみ）
2.  「地物情報」「属性情報」が1つのテーブルに入っていて、カラム名がある（大部分はこれ、カラム名で判別できる）
3.  「地物情報」「属性情報」が1つのテーブルに入っていて、カラム名がない（A09、A20）
4.  「地物情報」「属性情報」が1つのテーブルに入っていて、カラム名があるが、繰り返し登場する（A31-v2\_1）

``` r
library(rvest)

detect_format_type <- function(f) {
  page <- read_html(f)
  
  type1_tables <- page %>% 
    html_elements("table.tablelist tr") %>%
    purrr::keep(~ length(html_element(.x, "table")) > 0)
  
  if (length(type1_tables) >= 2) {
    th <- type1_tables %>% 
      html_elements("th") %>% 
      html_text2()
    
    if ("地物情報" %in% th && "属性情報" %in% th) {
      return("type1")
    }
  }
  
  type2_tables <- page %>% 
    html_table() %>% 
    purrr::keep(~ "地物情報" == colnames(.x)[1])

  if (length(type2_tables) == 1) {
    return("type2")
  } else if(length(type2_tables) > 1) {
    return("type4")
  }

  type3_tables <- page %>% 
    # header を指定しないと <th> がカラム名に使われる、TRUE だと最初の行が使われる
    html_table(header = TRUE) %>% 
    purrr::keep(~ "地物情報" == colnames(.x)[1])
  
  if (length(type3_tables) == 1) {
    return("type3")
  }
  
  "unknown"
}

# 動作確認
detect_format_type(here::here("data-raw", "datalist", "KsjTmplt-C23.html"))
```

    ## [1] "type1"

``` r
detect_format_type(here::here("data-raw", "datalist", "KsjTmplt-W07.html"))
```

    ## [1] "type2"

``` r
detect_format_type(here::here("data-raw", "datalist", "KsjTmplt-A09.html"))
```

    ## [1] "type3"

``` r
detect_format_type(here::here("data-raw", "datalist", "KsjTmplt-A31-v2_1.html"))
```

    ## [1] "type4"

``` r
datalist_files <- list.files(here::here("data-raw", "datalist"), full.names = TRUE)
names(datalist_files) <- basename(datalist_files)

split(datalist_files, purrr::map_chr(datalist_files, detect_format_type))
```

    ## $type1
    ##                                                             KsjTmplt-C23.html 
    ## "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-C23.html" 
    ## 
    ## $type2
    ##                                                                     KsjTmplt-A03.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A03.html" 
    ##                                                                KsjTmplt-A10-v3_1.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A10-v3_1.html" 
    ##                                                                     KsjTmplt-A11.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A11.html" 
    ##                                                                     KsjTmplt-A12.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A12.html" 
    ##                                                                     KsjTmplt-A13.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A13.html" 
    ##                                                                     KsjTmplt-A15.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A15.html" 
    ##                                                                KsjTmplt-A16-v2_3.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A16-v2_3.html" 
    ##                                                                KsjTmplt-A17-v4_0.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A17-v4_0.html" 
    ##                                                                KsjTmplt-A18-v4_0.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A18-v4_0.html" 
    ##                                                                  KsjTmplt-A18s-a.html 
    ##      "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A18s-a.html" 
    ##                                                                KsjTmplt-A19-v4_0.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A19-v4_0.html" 
    ##                                                                    KsjTmplt-A19s.html 
    ##        "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A19s.html" 
    ##                                                                    KsjTmplt-A20s.html 
    ##        "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A20s.html" 
    ##                                                                     KsjTmplt-A21.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A21.html" 
    ##                                                                    KsjTmplt-A21s.html 
    ##        "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A21s.html" 
    ##                                                                   KsjTmplt-A22-m.html 
    ##       "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A22-m.html" 
    ##                                                                KsjTmplt-A22-v3_0.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A22-v3_0.html" 
    ##                                                                    KsjTmplt-A22s.html 
    ##        "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A22s.html" 
    ##                                                                KsjTmplt-A23-v3_0.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A23-v3_0.html" 
    ##                                                                KsjTmplt-A24-v3_0.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A24-v3_0.html" 
    ##                                                                KsjTmplt-A25-v3_0.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A25-v3_0.html" 
    ##                                                                     KsjTmplt-A26.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A26.html" 
    ##                                                                KsjTmplt-A27-v2_1.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A27-v2_1.html" 
    ##                                                                     KsjTmplt-A28.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A28.html" 
    ##                                                                KsjTmplt-A29-v2_1.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A29-v2_1.html" 
    ##                                                                   KsjTmplt-A30a5.html 
    ##       "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A30a5.html" 
    ##                                                                    KsjTmplt-A30b.html 
    ##        "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A30b.html" 
    ##                                                                KsjTmplt-A32-v2_0.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A32-v2_0.html" 
    ##                                                                KsjTmplt-A33-v1_3.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A33-v1_3.html" 
    ##                                                                KsjTmplt-A34-v1_2.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A34-v1_2.html" 
    ##                                                                    KsjTmplt-A35a.html 
    ##        "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A35a.html" 
    ##                                                                    KsjTmplt-A35b.html 
    ##        "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A35b.html" 
    ##                                                                    KsjTmplt-A35c.html 
    ##        "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A35c.html" 
    ##                                                                     KsjTmplt-A37.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A37.html" 
    ##                                                                     KsjTmplt-A38.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A38.html" 
    ##                                                                KsjTmplt-A39-v1_1.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A39-v1_1.html" 
    ##                                                                     KsjTmplt-A40.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A40.html" 
    ##                                                                     KsjTmplt-A42.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A42.html" 
    ##                                                                     KsjTmplt-A43.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A43.html" 
    ##                                                                     KsjTmplt-A44.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A44.html" 
    ##                                                                     KsjTmplt-A45.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A45.html" 
    ##                                                                KsjTmplt-C02-v3_2.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-C02-v3_2.html" 
    ##                                                                     KsjTmplt-C09.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-C09.html" 
    ##                                                                KsjTmplt-C28-v2_4.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-C28-v2_4.html" 
    ##                                                                     KsjTmplt-G02.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-G02.html" 
    ##                                                                   KsjTmplt-G04-a.html 
    ##       "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-G04-a.html" 
    ##                                                                   KsjTmplt-G04-c.html 
    ##       "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-G04-c.html" 
    ##                                                                   KsjTmplt-G04-d.html 
    ##       "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-G04-d.html" 
    ##                                                                KsjTmplt-G08-v1_0.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-G08-v1_0.html" 
    ##                                                                KsjTmplt-L01-v2_5.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-L01-v2_5.html" 
    ##                                                                KsjTmplt-L02-v2_7.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-L02-v2_7.html" 
    ##                                                                   KsjTmplt-L03-a.html 
    ##       "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-L03-a.html" 
    ##                                                                 KsjTmplt-L03-b_r.html 
    ##     "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-L03-b_r.html" 
    ##                                                                 KsjTmplt-L03-b-c.html 
    ##     "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-L03-b-c.html" 
    ##                                                                 KsjTmplt-L03-b-u.html 
    ##     "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-L03-b-u.html" 
    ##                                                                   KsjTmplt-L03-b.html 
    ##       "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-L03-b.html" 
    ##                                                                     KsjTmplt-L05.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-L05.html" 
    ##                                                                KsjTmplt-mesh1000.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-mesh1000.html" 
    ##                                                             KsjTmplt-mesh1000h30.html 
    ## "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-mesh1000h30.html" 
    ##                                                                 KsjTmplt-mesh500.html 
    ##     "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-mesh500.html" 
    ##                                                              KsjTmplt-mesh500h30.html 
    ##  "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-mesh500h30.html" 
    ##                                                                KsjTmplt-N02-v2_3.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-N02-v2_3.html" 
    ##                                                                KsjTmplt-N03-v2_4.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-N03-v2_4.html" 
    ##                                                                     KsjTmplt-N04.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-N04.html" 
    ##                                                                KsjTmplt-N05-v1_3.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-N05-v1_3.html" 
    ##                                                                KsjTmplt-N06-v1_2.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-N06-v1_2.html" 
    ##                                                                     KsjTmplt-N07.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-N07.html" 
    ##                                                                KsjTmplt-N08-v1_4.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-N08-v1_4.html" 
    ##                                                                     KsjTmplt-N09.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-N09.html" 
    ##                                                                KsjTmplt-N10-v1_1.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-N10-v1_1.html" 
    ##                                                                     KsjTmplt-N11.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-N11.html" 
    ##                                                                KsjTmplt-P02-v4_0.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P02-v4_0.html" 
    ##                                                                     KsjTmplt-P03.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P03.html" 
    ##                                                                KsjTmplt-P04-v2_1.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P04-v2_1.html" 
    ##                                                                     KsjTmplt-P05.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P05.html" 
    ##                                                                KsjTmplt-P07-v2_1.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P07-v2_1.html" 
    ##                                                                     KsjTmplt-P09.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P09.html" 
    ##                                                                     KsjTmplt-P11.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P11.html" 
    ##                                                                KsjTmplt-P12-v2_2.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P12-v2_2.html" 
    ##                                                                     KsjTmplt-P13.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P13.html" 
    ##                                                                     KsjTmplt-P14.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P14.html" 
    ##                                                                     KsjTmplt-P15.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P15.html" 
    ##                                                                     KsjTmplt-P16.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P16.html" 
    ##                                                                     KsjTmplt-P17.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P17.html" 
    ##                                                                     KsjTmplt-P18.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P18.html" 
    ##                                                                     KsjTmplt-P19.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P19.html" 
    ##                                                                     KsjTmplt-P20.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P20.html" 
    ##                                                                     KsjTmplt-P21.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P21.html" 
    ##                                                                     KsjTmplt-P22.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P22.html" 
    ##                                                                     KsjTmplt-P23.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P23.html" 
    ##                                                                     KsjTmplt-P24.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P24.html" 
    ##                                                                     KsjTmplt-P26.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P26.html" 
    ##                                                                     KsjTmplt-P27.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P27.html" 
    ##                                                                     KsjTmplt-P28.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P28.html" 
    ##                                                                     KsjTmplt-P29.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P29.html" 
    ##                                                                     KsjTmplt-P30.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P30.html" 
    ##                                                                     KsjTmplt-P31.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P31.html" 
    ##                                                                     KsjTmplt-P32.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P32.html" 
    ##                                                                     KsjTmplt-P33.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P33.html" 
    ##                                                                     KsjTmplt-P34.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P34.html" 
    ##                                                                     KsjTmplt-P35.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-P35.html" 
    ##                                                              KsjTmplt-S05-a-v2_2.html 
    ##  "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-S05-a-v2_2.html" 
    ##                                                              KsjTmplt-S05-b-v2_2.html 
    ##  "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-S05-b-v2_2.html" 
    ##                                                                   KsjTmplt-S05-c.html 
    ##       "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-S05-c.html" 
    ##                                                              KsjTmplt-S05-d-v2_2.html 
    ##  "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-S05-d-v2_2.html" 
    ##                                                               KsjTmplt-S10b-v1_1.html 
    ##   "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-S10b-v1_1.html" 
    ##                                                                KsjTmplt-S12-v2_6.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-S12-v2_6.html" 
    ##                                                                     KsjTmplt-W01.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-W01.html" 
    ##                                                                     KsjTmplt-W05.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-W05.html" 
    ##                                                                     KsjTmplt-W07.html 
    ##         "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-W07.html" 
    ##                                                                KsjTmplt-W09-v2_2.html 
    ##    "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-W09-v2_2.html" 
    ## 
    ## $type3
    ##                                                             KsjTmplt-A09.html 
    ## "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A09.html" 
    ##                                                             KsjTmplt-A20.html 
    ## "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A20.html" 
    ## 
    ## $type4
    ##                                                              KsjTmplt-A31-v2_1.html 
    ##  "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-A31-v2_1.html" 
    ##                                                             KsjTmplt-S10a-v1_2.html 
    ## "/home/yutani/repo/kokudosuuchi-metadata/data-raw/datalist/KsjTmplt-S10a-v1_2.html"
