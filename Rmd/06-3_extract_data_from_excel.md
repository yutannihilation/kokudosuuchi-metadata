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

dir.create(here::here("data", "excel"))
```

    ## Warning in dir.create(here::here("data", "excel")): '/home/yutani/repo/
    ## kokudosuuchi-metadata/data/excel' already exists

``` r
l <- purrr::map_dfr(sheets, ~ {
  readxl::read_excel(excel_file, sheet = .x, skip = 4) %>% 
    select(
      id = 識別子,
      item = データ項目,
      tag = タグ名,
      code = 対応番号,
      name = 属性名
    ) %>% 
    tidyr::fill(id, item, tag)
}, .id = "sheet")
```

    ## New names:
    ## * `` -> ...6
    ## New names:
    ## * `` -> ...6

## `A20`

A20はHTMLにほぼ情報がないのでExcelから抜き出した情報を使う

``` r
id <- "A20"

out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

l %>% 
  filter(id == {{ id }}) %>% 
  transmute(
    name,
    code,
    description = NA,
    type = NA,
    codelist = NA
  ) %>% 
  readr::write_csv(out)
```

## `A21`

A21はHTMLにほぼ情報がないのでExcelから抜き出した情報を使う

``` r
id <- "A21"

out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

l %>% 
  filter(id == {{ id }}) %>% 
  transmute(
    name,
    code,
    description = NA,
    type = NA,
    codelist = NA
  ) %>% 
  readr::write_csv(out)
```

## `N04`

年度によってカラムが違う。
いずれもExcelにあるので、別途書き出す。`joined.csv`には含めない。

``` r
id <- "N04"

out <- here::here("data", glue::glue("{id}.csv"))

l_tmp <- l %>%
  filter(id == {{ id }}) %>% 
  group_by(sub_id = cumsum(code == "N04_001")) %>% 
  mutate(
    columns = n()
  ) %>% 
  ungroup() %>% 
  transmute(
    name,
    code,
    columns
  ) %>% 
  readr::write_csv(out)
```

## `P09`

``` r
id <- "P09"

out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

l %>% 
  filter(id == {{ id }}) %>% 
  transmute(
    name,
    code,
    description = NA,
    type = NA,
    codelist = NA
  ) %>% 
  readr::write_csv(out)
```

## `C02`

``` r
id <- "C02"

out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

l %>% 
  filter(id == {{ id }}) %>% 
  transmute(
    name,
    code,
    description = NA,
    type = NA,
    codelist = NA
  ) %>% 
  readr::write_csv(out)
```

## `C09`

``` r
id <- "C09"

out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

l %>% 
  filter(id == {{ id }}) %>% 
  transmute(
    name,
    code,
    description = NA,
    type = NA,
    codelist = NA
  ) %>% 
  readr::write_csv(out)
```

## `S05-a`

年度によってカラムが違う。
いずれもExcelにあるので、別途書き出す。`joined.csv`には含めない。

よくわからないが、HTMLと見比べた感じ47カラムの方は「不明」のカラムが足りてなさそうなので追加する。

``` r
id <- "S05-a"

out <- here::here("data", glue::glue("{id}.csv"))

l_tmp <- l %>%
  # id に S05-b が入っているのでcodeで絞り込む
  filter(stringr::str_detect(code, "^S05a")) %>% 
  group_by(sub_id = cumsum(code == "S05a_001")) %>% 
  mutate(
    columns = n()
  ) %>% 
  ungroup() %>% 
  transmute(
    name,
    code,
    columns
  ) %>% 
  split(.$columns)

stopifnot(identical(sort(names(l_tmp)), c("35", "47")))

missing <- tibble::tribble(
                ~name,      ~code,
"不明-出勤トリップ数", "S05a_047",
"不明-登校トリップ数", "S05a_048",
"不明-自由トリップ数", "S05a_049",
"不明-業務トリップ数", "S05a_050",
"不明-帰宅トリップ数", "S05a_051",
"不明-不明トリップ数", "S05a_052",
"不明-合計トリップ数", "S05a_053",
"全トリップ数",        "S05a_054"
)

missing$columns <- 54L

l_tmp$`47` %>% 
  # 列がずれてるっぽい
  filter(code != "S05a_047") %>% 
  mutate(columns = 54L) %>% 
  bind_rows(
    missing,
    l_tmp$`35`
  ) %>% 
  readr::write_csv(out)
```

## `S05-b`

同上

``` r
id <- "S05-b"

out <- here::here("data", glue::glue("{id}.csv"))

l_tmp <- l %>%
  # id に S05-b が入っているのでcodeで絞り込む
  filter(stringr::str_detect(code, "^S05b")) %>% 
  group_by(sub_id = cumsum(code == "S05b_001")) %>% 
  mutate(
    columns = n()
  ) %>% 
  ungroup() %>% 
  transmute(
    name,
    code,
    columns
  ) %>% 
  split(.$columns)

stopifnot(identical(sort(names(l_tmp)), c("35", "47")))

missing <- tibble::tribble(
                ~name,      ~code,
"不明-出勤トリップ数", "S05b_047",
"不明-登校トリップ数", "S05b_048",
"不明-自由トリップ数", "S05b_049",
"不明-業務トリップ数", "S05b_050",
"不明-帰宅トリップ数", "S05b_051",
"不明-不明トリップ数", "S05b_052",
"不明-合計トリップ数", "S05b_053",
"全トリップ数",        "S05b_054"
)

missing$columns <- 54L

l_tmp$`47` %>% 
  # 列がずれてるっぽい
  filter(code != "S05b_047") %>% 
  mutate(columns = 54L) %>% 
  bind_rows(
    missing,
    l_tmp$`35`
  ) %>% 
  readr::write_csv(out)
```

## `S05-c`

首都圏と中部でコードの範囲が違っているが、HTML版には載っていない情報。
Excelにのってるコードでマッチさせないといけない。

駅コードはコードリスト型だが、駅名が載っているのでそのままで問題ない。

``` r
id <- "S05-c"

out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

l %>% 
  filter(id == {{ id }}) %>% 
  transmute(
    name,
    code,
    description = NA,
    type = NA,
    codelist = NA
  ) %>% 
  readr::write_csv(out)
```

## `W05`

`W05_002`は河川名が併記されているので変換しなくて問題なさそう。

``` r
id <- "W05"

out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

l %>% 
  filter(id == {{ id }}) %>% 
  transmute(
    name,
    code,
    description = NA,
    type = NA,
    codelist = code %in% c("W05_001", "W05_003", "W05_005")
  ) %>% 
  readr::write_csv(out)
```

## `W07`

`W07_002`, `W07_003`
は水域名、河川名が併記されているので変換しなくて問題なさそう。

``` r
id <- "W07"

out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

l %>% 
  filter(id == {{ id }}) %>% 
  transmute(
    name,
    code,
    description = NA,
    type = NA,
    codelist = NA,
  ) %>% 
  readr::write_csv(out)
```

## `A18s-a`

`A18s-a`の定義はエクセルにある

``` r
id <- "A18s-a"
out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

excel_file_A18s_a <- here::here("data-raw", "codelist", "A18s-a_property_table.xls")

if (!file.exists(excel_file_A18s_a)) {
  curl::curl_download(
    "https://nlftp.mlit.go.jp/ksj/gml/datalist/A18s-a_property_table.xls",
    destfile = excel_file_A18s_a
  )
}

d <- readxl::read_excel(excel_file_A18s_a)
```

    ## New names:
    ## * `` -> ...1

``` r
d %>% 
  select(
    name = 2,
    code = 4,
    type = 3
  ) %>% 
  filter(stringr::str_detect(code, "^A18")) %>% 
  arrange(code) %>% 
  transmute(
    name,
    code,
    description = NA,
    type,
    codelist = NA,
  ) %>% 
  readr::write_csv(out)
```

## `A19s`

`A19s`の定義はエクセルにある

``` r
id <- "A19s"
out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

excel_file_A19s <- here::here("data-raw", "codelist", "A19s_property_table.xls")

if (!file.exists(excel_file_A19s)) {
  curl::curl_download(
    "https://nlftp.mlit.go.jp/ksj/gml/datalist/A19s_property_table.xls",
    destfile = excel_file_A19s
  )
}

d <- readxl::read_excel(excel_file_A19s)
```

    ## New names:
    ## * `` -> ...2

``` r
d %>% 
  select(
    name = 2,
    code = 4,
    type = 3
  ) %>% 
  filter(stringr::str_detect(code, "^A19")) %>% 
  arrange(code) %>% 
  transmute(
    name,
    code,
    description = NA,
    type,
    codelist = NA,
  ) %>% 
  readr::write_csv(out)
```

## `A20s`

`A20s`の定義はエクセルにある

``` r
id <- "A20s"
out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

excel_file_A20s <- here::here("data-raw", "codelist", "A20s_property_table.xls")

if (!file.exists(excel_file_A20s)) {
  curl::curl_download(
    "https://nlftp.mlit.go.jp/ksj/gml/datalist/A20s_property_table.xls",
    destfile = excel_file_A20s
  )
}

d <- readxl::read_excel(excel_file_A20s)
```

    ## New names:
    ## * `` -> ...1

``` r
d %>% 
  select(
    name = 2,
    code = 4,
    type = 3
  ) %>% 
  filter(stringr::str_detect(code, "^A20")) %>% 
  arrange(code) %>% 
  transmute(
    name,
    code,
    description = NA,
    type,
    codelist = NA,
  ) %>% 
  readr::write_csv(out)
```

## `A21s`

`A21s`の定義はエクセルにある

``` r
id <- "A21s"
out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

excel_file_A21s <- here::here("data-raw", "codelist", "A21s_property_table.xls")

if (!file.exists(excel_file_A21s)) {
  curl::curl_download(
    "https://nlftp.mlit.go.jp/ksj/gml/datalist/A21s_property_table.xls",
    destfile = excel_file_A21s
  )
}

d <- readxl::read_excel(excel_file_A21s)
```

    ## New names:
    ## * `` -> ...1
    ## * `` -> ...4

``` r
d %>% 
  select(
    name = 2,
    code = 5,
    type1 = 3,
    type2 = 4,
  ) %>% 
  filter(stringr::str_detect(code, "^A21")) %>% 
  arrange(code) %>% 
  transmute(
    name,
    code,
    description = NA,
    type = coalesce(type1, type2),
    codelist = NA,
  ) %>% 
  readr::write_csv(out)
```

## `A22s`

`A22s`の定義はエクセルにある

``` r
id <- "A22s"
out <- here::here("data", "colnames_exact", glue::glue("{id}.csv"))

excel_file_A22s <- here::here("data-raw", "codelist", "A22s_property_table.xls")

if (!file.exists(excel_file_A22s)) {
  curl::curl_download(
    "https://nlftp.mlit.go.jp/ksj/gml/datalist/A22s_property_table.xls",
    destfile = excel_file_A22s
  )
}

d <- readxl::read_excel(excel_file_A22s)
```

    ## New names:
    ## * `` -> ...1

``` r
d %>% 
  select(
    name = 2,
    code = 4,
    type = 3
  ) %>% 
  filter(stringr::str_detect(code, "^A22")) %>% 
  arrange(code) %>% 
  transmute(
    name,
    code,
    description = NA,
    type,
    codelist = NA,
  ) %>% 
  readr::write_csv(out)
```
