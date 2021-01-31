Generate metadata
================

## 方針

### 列をどうマッチさせるか

主に以下の4種類があると推測

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

col_types <- cols(
  name = col_character(),
  code = col_character(),
  description = col_character(),
  type = col_character()
)

detect_codelist <- function(type) {
  str_detect(type, "コードリスト|(行政|参照資料|産廃施設|都道府県|特別管理|不燃領域率定義|防災再開発促進地区指定)コード")
}
```

### `exact`の例: A22

``` r
d <- read_csv(csv_files["A22"], col_types = col_types)

d %>% 
  mutate(
    name,
    code,
    match_type = "exact",
    description,
    codelist = detect_codelist(type),
  ) %>% 
  knitr::kable()
```

| name           | code     | description                                                          | type                           | match\_type | codelist |
|:---------------|:---------|:---------------------------------------------------------------------|:-------------------------------|:------------|:---------|
| 豪雪地帯ID     | A22\_001 | 豪雪地帯を一意に識別するためのコード                                 | 文字列型（CharacterString）    | exact       | FALSE    |
| 行政区域コード | A22\_002 | 都道府県コードと市区町村コードからなる，行政区を特定するためのコード | コードリスト「行政コード」     | exact       | TRUE     |
| 都道府県名     | A22\_003 | データ整備時点の当該区域を含む都道府県名称                           | 文字列型（CharacterString）    | exact       | FALSE    |
| 支庁・振興局名 | A22\_004 | データ整備時点の当該区域を含む北海道の振興局の名称                   | 文字列型（CharacterString）    | exact       | FALSE    |
| 郡・政令都市名 | A22\_005 | データ整備時点の当該区域を含む都道府県名称                           | 文字列型（CharacterString）    | exact       | FALSE    |
| 市区町村名     | A22\_006 | データ整備時点の市区町村名称                                         | 文字列型（CharacterString）    | exact       | FALSE    |
| 原典市区町村名 | A22\_007 | 原典資料が作成された時点の当該区域を含む市区町村名称                 | 文字列型（CharacterString）    | exact       | FALSE    |
| 旧市町村名     | A22\_008 | 地域が指定された時点の市町村名                                       | 文字列型（CharacterString）    | exact       | FALSE    |
| 豪雪区分コード | A22\_009 | 豪雪地帯の分類区分                                                   | コードリスト「豪雪区分コード」 | exact       | TRUE     |

### `pattern`の例1: A34

IDにはいくつかバリエーションがあるけど列名は固定の場合

``` r
d <- read_csv(csv_files["A34"], col_types = col_types)

d %>% 
  mutate(name = str_replace(name, "（A34._([0-9]*)）", "/A34.*_\\1")) %>%
  tidyr::separate(name, into = c("name", "code_pattern"), sep = "/") %>% 
  transmute(
    name,
    code = coalesce(code_pattern, code),
    match_type = if_else(is.na(code_pattern), "exact", "pattern"),
    description,
    codelist = detect_codelist(type),
  ) %>% 
  knitr::kable()
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 4 rows [10, 11,
    ## 12, 13].

| name             | code        | match\_type | description                                                       | codelist |
|:-----------------|:------------|:------------|:------------------------------------------------------------------|:---------|
| 構成資産範囲ID   | A34.\*\_001 | pattern     | 構成資産範囲ごとの整理番号                                        | FALSE    |
| 世界遺産番号     | A34.\*\_002 | pattern     | 世界文化遺産への登録順の番号                                      | FALSE    |
| 世界文化遺産名   | A34.\*\_003 | pattern     | 世界文化遺産の名称                                                | FALSE    |
| 地区名           | A34.\*\_004 | pattern     | 世界文化遺産内で区分されている地区名                              | FALSE    |
| 都道府県名       | A34.\*\_005 | pattern     | 世界文化遺産が所在する都道府県の名称                              | FALSE    |
| 構成資産範囲面積 | A34.\*\_006 | pattern     | 構成資産範囲の面積（単位：ha、小数点以下1桁（第二位で四捨五入）） | FALSE    |
| 構成資産         | A34.\*\_007 | pattern     | 構成資産の名称                                                    | FALSE    |
| 登録基準区分     | A34.\*\_008 | pattern     | 登録基準に関する区分番号                                          | FALSE    |
| 記載年月         | A34.\*\_009 | pattern     | 世界文化遺産文化財に登録された記載年月                            | FALSE    |
| 構成資産ID       | A34e\_001   | exact       | 構成資産ごとの整理番号                                            | FALSE    |
| 世界遺産番号     | A34e\_002   | exact       | 世界文化遺産への登録順の番号                                      | FALSE    |
| 世界文化遺産名   | A34e\_003   | exact       | 世界文化遺産の名称                                                | FALSE    |
| 構成資産名       | A34e\_004   | exact       | 構成資産の名称                                                    | FALSE    |
| 緩衝地帯ID       | A34.\*\_001 | pattern     | 緩衝地帯ごとの整理番号                                            | FALSE    |
| 世界遺産番号     | A34.\*\_002 | pattern     | 世界文化遺産への登録順の番号                                      | FALSE    |
| 世界文化遺産名   | A34.\*\_003 | pattern     | 世界文化遺産の名称                                                | FALSE    |
| 地区名           | A34.\*\_004 | pattern     | 世界文化遺産内で区分されている地区名                              | FALSE    |
| 緩衝地帯面積     | A34.\*\_005 | pattern     | 緩衝地帯の面積（単位：ha、小数点以下1桁（第二位で四捨五入））     | FALSE    |
