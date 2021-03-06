Extract data from C23
================

``` r
library(rvest)
library(dplyr, warn.conflicts = FALSE)

dir.create(here::here("data", "attrs"), recursive = TRUE, showWarnings = FALSE)

input <- here::here("data-raw", "datalist", "KsjTmplt-C23.html")
id <- stringr::str_replace(basename(input), "(?:KsjTmplt-)(.*?)(?:-v.*)?(?:\\.html)", "\\1")
output <- here::here("data", "attrs", glue::glue("{id}.csv"))

attr_table <- read_html(input) %>% 
  html_elements("table.tablelist tr") %>%
  purrr::keep(~ {
    html_text2(html_element(.x, "th")) == "属性情報" &&
      length(html_element(.x, "table")) > 0
  }) %>% 
  html_table(convert = FALSE)

stopifnot(length(attr_table) == 1)

knitr::kable(attr_table[[1]])
```

| 属性名                     | 説明                                                                                                                                                                                                                                                                                                                 | 属性の型                                       |
|:---------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------|
| 場所                       | 海岸線の位置 陸と海との境界を言う。海岸線は海の干満によって変化する。地形図に記載された海岸線は満潮時のものである。                                                                                                                                                                                                  | 曲線型（GM\_Curve）                            |
| 行政区域コード             | 都道府県コードと市区町村コードからなる、行政区を特定するためのコード                                                                                                                                                                                                                                                 | コードリスト「行政コード」                     |
| 所管官庁                   | 海岸線を所管する官庁を特定するコード                                                                                                                                                                                                                                                                                 | コードリスト「所管官庁コード」                 |
| 海岸保全区域番号           | 海岸法に基づく海岸保全区域の番号                                                                                                                                                                                                                                                                                     | 整数型　※不明の場合、“9999”とする。            |
| 海岸保全区域・海岸名       | 海岸保全区域の海岸名称海岸保全区域とは、海岸法に基づき津波、高潮、波浪その他海水又は地盤の変動による被害から海岸を保護し、国土の保全に資するため必要があると認めて都道府県知事が指定した一定の海岸の区域をいう。原則として、陸地においては満潮時の水際線から50ｍ、水面においては干潮時の水際線から50ｍとされている。 | 文字列型                                       |
| 海岸保全区域・海岸管理者名 | 海岸保全区域の海岸管理者を区分するためのコード                                                                                                                                                                                                                                                                       | コードリスト「海岸保全区域・海岸管理者コード」 |
| 河口                       | 河口部かどうかの区別。                                                                                                                                                                                                                                                                                               | 真偽値型（true：河口部、false：その他）        |

``` r
attr_table[[1]] %>% 
  filter(
    !stringr::str_detect(stringr::str_remove_all(属性の型, "\\s+"), "^(地物|曲?面型|曲?線型|点型|GM_Surface|GM_Curve|GM_Point)")
  ) %>%
  dplyr::transmute(
    name = 属性名,
    code = NA,
    description = 説明,
    type = 属性の型
  ) %>% 
  readr::write_csv(output)
```
