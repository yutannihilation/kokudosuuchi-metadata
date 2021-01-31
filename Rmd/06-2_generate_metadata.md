Generate metadata
================

## 方針

-   `exact`:
    列名のコードが与えられている場合はコードと直接マッチさせられる
-   `positional`:
    列名のコードが全くない場合は、列名を順番にあてはめていく
-   `pattern`:
    以下の2パターンがある。全体から見ればそれほど多くはないので、それぞれ専用の関数をつくることにする。
    -   列名をパターンでマッチさせる
    -   列の中身を見ていい感じに判断（例：L01はどこまでが調査価格でどこからが属性移動なのか判別できない）

## 実際それでいけそうか確認

``` r
library(readr)
library(stringr)
library(dplyr, warn.conflicts = FALSE)

csv_files <- list.files(here::here("data", "attrs"), full.names = TRUE)
names(csv_files) <- tools::file_path_sans_ext(basename(csv_files))

id_exception <- c("A22-m", "A34", "A35a", "A35b", "A37", "P15", "P16", "P17", "P18", "L03-a", "mesh1000", "mesh500", "P21", "N05")

col_types <- cols(
  name = col_character(),
  code = col_character(),
  description = col_character(),
  type = col_character()
)

d <- purrr::map_dfr(csv_files, read_csv, col_types = col_types, .id = "id")

id_types <- d %>% 
  group_by(id) %>% 
  summarise(
    type = case_when(
      # 目視でがんばって見つけた特殊そうなやつ
      unique(id) %in% id_exception ~ "other",
      # すべてコードがわかっている場合は名前でマッチさせる
      sum(is.na(code)) == 0   ~ "exact",
      # すべてコードがわからない場合は順序でマッチさせる
      sum(is.na(code)) == n() ~ "positional",
      # その他はひとつひとつ対処（つらい）
      TRUE ~ "other"
    )
  ) %>% 
  { split(.$id, .$type) }

id_types
```

    ## $exact
    ##  [1] "A15"     "A22"     "A27"     "A29"     "A30a5"   "A30b"    "A31"    
    ##  [8] "A32"     "A33"     "A35c"    "A39"     "A40"     "A42"     "A43"    
    ## [15] "A44"     "A45"     "G04-a"   "G04-c"   "G04-d"   "G08"     "L03-b-c"
    ## [22] "L05"     "N02"     "N03"     "N06"     "N07"     "N08"     "N09"    
    ## [29] "N10"     "N11"     "P04"     "P13"     "P14"     "P19"     "P20"    
    ## [36] "P24"     "P26"     "P27"     "P28"     "P29"     "P30"     "P31"    
    ## [43] "P32"     "P34"     "P35"     "S05-d"   "S10a"    "S10b"    "S12"    
    ## [50] "W01"    
    ## 
    ## $other
    ##  [1] "A22-m"    "A34"      "A35a"     "A35b"     "A37"      "A38"     
    ##  [7] "C28"      "L03-a"    "mesh1000" "mesh500"  "N05"      "P15"     
    ## [13] "P16"      "P17"      "P18"      "P21"      "W09"     
    ## 
    ## $positional
    ##  [1] "A03"     "A10"     "A11"     "A12"     "A13"     "A16"     "A17"    
    ##  [8] "A18"     "A18s-a"  "A19"     "A19s"    "A20s"    "A21"     "A21s"   
    ## [15] "A22s"    "A23"     "A24"     "A25"     "A26"     "A28"     "C02"    
    ## [22] "C09"     "C23"     "G02"     "L01"     "L02"     "L03-b"   "L03-b-u"
    ## [29] "N04"     "P02"     "P05"     "P07"     "P09"     "P11"     "S05-a"  
    ## [36] "S05-b"   "S05-c"   "W05"     "W07"

``` r
out_exact <- here::here("data", "colnames_exact")
out_positional <- here::here("data", "colnames_positional")

dir.create(out_exact, showWarnings = FALSE)
dir.create(out_positional, showWarnings = FALSE)

detect_codelist <- function(type) {
  str_detect(type, "コードリスト|(行政|参照資料|産廃施設|都道府県|特別管理|不燃領域率定義|防災再開発促進地区指定)コード")
}
```

## `exact`

``` r
purrr::walk(id_types$exact, ~ {
  d %>% 
    filter(id == {{ .x }}) %>% 
    mutate(
      codelist = detect_codelist(type)
    ) %>% 
    readr::write_csv(file.path(out_exact, glue::glue("{.x}.csv")))
})
```

## `positional`

positional match
がどれくらいうまくいくかは謎。あとで検証する必要がある。

``` r
purrr::walk(id_types$positional, ~ {
  d %>% 
    filter(id == {{ .x }}) %>% 
    mutate(
      codelist = detect_codelist(type)
    ) %>% 
    readr::write_csv(file.path(out_positional, glue::glue("{.x}.csv")))
})
```
