Extract data from A10, A11, A12, A13
================

``` r
library(rvest)
library(dplyr, warn.conflicts = FALSE)

dir.create(here::here("data", "attrs"), recursive = TRUE, showWarnings = FALSE)
```

### `A10`, `A11`, `A12`, `A13`

<https://nlftp.mlit.go.jp/ksj/gml/5Area_shape_property.pdf> に定義がある

``` r
out_dir <- here::here("data", "attrs")

A10_type <- c("A10", "A11", "A12", "A13")

d_A10 <- tibble::tribble(
      ~name,        ~code,
  "都道府県コード",  "PREFEC_CD",
    "地区コード",    "AREA_CD",
     "市町村名",   "CTV_NAME",
       "年度",   "FIS_YEAR",
     "主題番号",   "THEMA_NO",
    "レイヤ番号",   "LAYER_NO",
       "名称",   "OBJ_NAME",
   "ポリゴン面積",  "AREA_SIZE",
     "内外区分", "IOSIDE_DIV",
       "備考", "REMARK_STR"
)

purrr::walk(A10_type, function(id) {
  d_A10 %>% 
  transmute(
    name,
    code,
    description = NA,
    type = NA,
  ) %>% 
    readr::write_csv(file.path(out_dir, paste0(id, ".csv")))
})
```
