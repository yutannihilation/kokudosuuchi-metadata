Extract codelist: shape\_property\_table.xls
================

コードとカラム名の対応は
<https://nlftp.mlit.go.jp/ksj/gml/shape_property_table.xls>
にも含まれている。
こちらはデータの型については記載がないので、基本はHTMLから抜き出した情報を使う。
Excelにしか載っていないやつはこっちを使う。

注意点として、HTMLに書かれていることとExcelの情報が一致しているとは限らないし、一致していても実データと同じとは限らない。

``` r
curl::curl_download(
  "https://nlftp.mlit.go.jp/ksj/gml/shape_property_table.xls",
  destfile = excel_file
)
```

``` r
library(dplyr, warn.conflicts = FALSE)

sheets <- readxl::excel_sheets(excel_file)
names(sheets) <- sheets

l <- purrr::map_dfr(sheets, ~ {
  readxl::read_excel(excel_file, sheet = .x, skip = 4) %>% 
    select(
      id = 識別子,
      item = データ項目,
      tag = タグ名,
      code = 対応番号,
      name = 属性名
    )
}, .id = "sheet")
```

    ## New names:
    ## * `` -> ...6
    ## New names:
    ## * `` -> ...6

## `A21`

``` r
id <- "A21"

out <- here::here("data", "attrs", glue::glue("{id}.csv"))

l %>% 
  filter(id == {{ id }}) %>% 
  transmute(
    name,
    code,
    description = NA,
    type = NA
  ) %>% 
  readr::write_csv(out)  
```
