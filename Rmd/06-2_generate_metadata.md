Generate metadata
================

``` r
library(readr)
library(stringr)
library(dplyr, warn.conflicts = FALSE)

csv_files <- list.files(here::here("data", "attrs"), full.names = TRUE)
names(csv_files) <- tools::file_path_sans_ext(basename(csv_files))

id_exception <- c(
  "30b", "A16", "A34", "A35a", "A35b", "A37", "C02", "C09", "C23", "G02", "L03-a",
  "mesh1000", "mesh500", "N05", "P09", "P11", "P15", "P16", "P17", "P18",
  "P21", "S05-a", "S05-b", "S05-c", "W05", "W07"
)

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
    ##  [1] "A09"     "A10"     "A11"     "A12"     "A13"     "A15"     "A21"    
    ##  [8] "A22"     "A27"     "A29"     "A30a5"   "A30b"    "A31"     "A32"    
    ## [15] "A33"     "A35c"    "A39"     "A40"     "A42"     "A43"     "A44"    
    ## [22] "A45"     "G04-a"   "G04-c"   "G04-d"   "G08"     "L03-b-c" "L05"    
    ## [29] "N02"     "N03"     "N06"     "N07"     "N08"     "N09"     "N10"    
    ## [36] "N11"     "P03"     "P04"     "P13"     "P14"     "P19"     "P20"    
    ## [43] "P24"     "P26"     "P27"     "P28"     "P29"     "P30"     "P31"    
    ## [50] "P32"     "P34"     "P35"     "S05-d"   "S10a"    "S10b"    "S12"    
    ## [57] "W01"    
    ## 
    ## $other
    ##  [1] "A16"      "A22-m"    "A34"      "A35a"     "A35b"     "A37"     
    ##  [7] "A38"      "C02"      "C09"      "C23"      "C28"      "G02"     
    ## [13] "L03-a"    "mesh1000" "mesh500"  "N05"      "P09"      "P11"     
    ## [19] "P15"      "P16"      "P17"      "P18"      "P21"      "S05-a"   
    ## [25] "S05-b"    "S05-c"    "W05"      "W07"      "W09"     
    ## 
    ## $positional
    ##  [1] "A03"     "A17"     "A18"     "A18s-a"  "A19"     "A19s"    "A20s"   
    ##  [8] "A21s"    "A22s"    "A23"     "A24"     "A25"     "A26"     "A28"    
    ## [15] "L01"     "L02"     "L03-b"   "L03-b-u" "P02"     "P05"     "P07"

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
    select(!id) %>% 
    distinct() %>% 
    arrange(code) %>% 
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
    select(!id) %>% 
    readr::write_csv(file.path(out_positional, glue::glue("{.x}.csv")))
})
```

## `other`

``` r
out_other <- here::here("data", "colnames_other")

dir.create(out_other, showWarnings = FALSE)
```

### `A09`

A09は<https://nlftp.mlit.go.jp/ksj/gml/5Area_shape_property.pdf>に定義がある。
A10〜13と違って大文字な点に注意。

``` r
tibble::tribble(
      ~name,        ~code,
  "都道府県コード",  "prefec_cd",
    "地区コード",    "area_cd",
    "レイヤ番号",   "layer_no"
) %>% 
  transmute(
    name,
    code,
    description = NA,
    type = NA,
    codelist = TRUE,
  ) %>% 
  readr::write_csv(file.path(out_exact, "A09.csv"))
```

### `A16`

どうやら実データと列の数が合わないっぽい。見比べた結果、「都道府県コード」がなさそう。

``` r
d %>%
  filter(id == "A16", name != "都道府県コード") %>% 
  mutate(
    codelist = detect_codelist(type)
  ) %>% 
  select(!id) %>% 
  readr::write_csv(file.path(out_positional, "A16.csv"))
```

### `A22-m`

`A22-m`は正規表現を使う必要があるが、それ以外の列は exact match
すればいい。 あと、 `A22_050001` が `A22_500001`
と間違えられているらしい。

``` r
d %>%
  filter(id == "A22-m", is.na(code)) %>% 
  knitr::kable()
```

| id    | name                                                          | code | description                                                                                                     | type              |
|:------|:--------------------------------------------------------------|:-----|:----------------------------------------------------------------------------------------------------------------|:------------------|
| A22-m | 各年度別最深積雪（A22\_01XXXX）XXXX：西暦                     | NA   | 最古年～平成25（2013）年における各年度別の積雪深の最大値（cm）※欠測の場合：99999998統計資料がない場合：99999999 | 実数型（Real）    |
| A22-m | 各年度別累計降雪量（A22\_02XXXX）XXXX：西暦                   | NA   | 最古年～平成25（2013）年における各年度別の累計降雪量（cm）※欠測の場合：99999998統計資料がない場合：99999999     | 実数型（Real）    |
| A22-m | 各年度別最低気温（A22\_03XXXX）XXXX：西暦                     | NA   | 最古年～平成25（2013）年における各年度別の気温の最低値（℃）※欠測の場合：99999998統計資料がない場合：99999999    | 実数型（Real）    |
| A22-m | 各年度別平均風速（A22\_04XXXX）XXXX：西暦                     | NA   | 最古年～平成25（2013）年における各年度別の平均風速（m/s）※欠測の場合：99999998統計資料がない場合：99999999      | 実数型（Real）    |
| A22-m | 各年別死者数（A22\_10XXXX）XXXX：西暦                         | NA   | 最古年～平成25（2013）年の各年別の雪害による死者数死者数が不明の場合は、「-1」                                  | 整数型（Integer） |
| A22-m | 各年別行方不明者数（A22\_11XXXX）XXXX：西暦                   | NA   | 最古年～平成25（2013）年の各年別の雪害による行方不明者数行方不明者数が不明の場合は、「-1」                      | 整数型（Integer） |
| A22-m | 各年別重傷者数（A22\_12XXXX）XXXX：西暦                       | NA   | 最古年～平成25（2013）年の各年別の雪害による行方不明者数行方不明者数が不明の場合は、「-1」                      | 整数型（Integer） |
| A22-m | 各年別軽傷者数（A22\_13XXXX）XXXX：西暦                       | NA   | 最古年～平成25（2013）年の各年別の雪害による軽傷者数軽傷者数が不明の場合は、「-1」                              | 整数型（Integer） |
| A22-m | 各年別住家全壊棟数（A22\_14XXXX）XXXX：西暦                   | NA   | 最古年～平成25（2013）年の各年別の雪害による住家の全壊棟数住家全壊棟数が不明の場合は、「-1」                    | 整数型（Integer） |
| A22-m | 各年別住家半壊棟数（A22\_15XXXX）XXXX：西暦                   | NA   | 最古年～平成25（2013）年の各年別の雪害による住家の半壊棟数住家半壊棟数が不明の場合は、「-1」                    | 整数型（Integer） |
| A22-m | 各年別住家一部破損数（A22\_16XXXX）XXXX：西暦                 | NA   | 各年の雪害による住家の一部破損棟数不明の場合は、「-1」                                                          | 整数型（Integer） |
| A22-m | 各年別除雪ボランティア団体数（A22\_17XXXX）XXXX：西暦         | NA   | 社会福祉協議会に登録されている除雪ボランティア団体数不明の場合は、「-1」                                        | 整数型（Integer） |
| A22-m | 各年別除雪ボランティア登録人数（A22\_18XXXX）XXXX：西暦       | NA   | 社会福祉協議会に登録されている除雪ボランティア登録人数不明の場合は、「-1」                                      | 整数型（Integer） |
| A22-m | 各年別除雪ボランティア活動回数（A22\_19XXXX）XXXX：西暦       | NA   | 社会福祉協議会に登録されている除雪ボランティア活動の年別回数不明の場合は、「-1」                                | 整数型（Integer） |
| A22-m | 各年別除雪ボランティアの延べ参加人数（A22\_20XXXX）XXXX：西暦 | NA   | 社会福祉協議会に登録されている除雪ボランティア活動の年別参加人数不明の場合は、「-1」                            | 整数型（Integer） |

``` r
d %>%
  filter(id == "A22-m", !is.na(code)) %>% 
  mutate(
    code = if_else(code == "A22_500001", "A22_050001", code),
    codelist = detect_codelist(type),
  ) %>% 
  select(!id) %>% 
  readr::write_csv(file.path(out_other, "A22-m.csv"))
```

### `A30b`

備考欄にあるやつをうまく抜き出せていない。

``` r
d_tmp <- tibble::tribble(
       ~code,       ~name,
  "A30b_027", "発生位置緯度誤差",
  "A30b_028", "発生位置経度誤差",
  "A30b_029",   "発生位置緯度",
  "A30b_030",   "発生位置経度",
  "A30b_031", "消滅位置緯度誤差",
  "A30b_032", "消滅位置経度誤差",
  "A30b_033",   "消滅位置緯度",
  "A30b_034",   "消滅位置経度"
) %>% 
  transmute(
    name,
    code,
    description = NA,
    type = NA,
  )

d %>%
  filter(id == "A30b") %>% 
  select(!id) %>% 
  bind_rows(d_tmp) %>% 
  mutate(
    codelist = detect_codelist(type)
  ) %>% 
  readr::write_csv(file.path(out_exact, "A30b.csv"))
```

### `A34`

`A34`は、<https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A34-v1_2.html>によれば`※`はa\~d、`*`はf,gなので展開してしまって`exact`に置く。

``` r
d %>%
  filter(id == "A34") %>% 
  mutate(
    variants = case_when(
      !is.na(code) ~ "e",
      str_detect(name, "※")   ~ "a,b,c,d",
      str_detect(name, "\\*") ~ "f,g"
    ),
    code = coalesce(code, str_extract(name, "A34[※\\*]_\\d+")),
    name = str_remove(name, "（A34[※\\*]_\\d+）")
  ) %>% 
  tidyr::separate_rows(variants, sep = ",") %>% 
  mutate(code = str_replace(code, "[※\\*]", variants)) %>% 
  arrange(code) %>% 
  transmute(
    name,
    code,
    description,
    type,
    codelist = detect_codelist(type)
  ) %>% 
  readr::write_csv(file.path(out_exact, "A34.csv"))
```

### `A35a`

これも同様(c.f.
<https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A35a.html>)

``` r
d %>%
  filter(id == "A35a") %>% 
  mutate(
    variants = case_when(
      !is.na(code) ~ "c",
      str_detect(name, "※")   ~ "a,b"
    ),
    code = coalesce(code, str_extract(name, "A35[※\\*]_\\d+")),
    name = str_remove(name, "（A35[※\\*]_\\d+）")
  ) %>% 
  tidyr::separate_rows(variants, sep = ",") %>% 
  mutate(code = str_replace(code, "[※\\*]", variants)) %>% 
  arrange(code) %>% 
  transmute(
    name,
    code,
    description,
    type,
    codelist = detect_codelist(type)
  ) %>% 
  readr::write_csv(file.path(out_exact, "A35a.csv"))
```

### `A35b`

同様（c.f.
<https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A35b.html>）

``` r
d %>%
  filter(id == "A35b") %>% 
  mutate(
    variants = case_when(
      !is.na(code) ~ "f",
      str_detect(name, "※")   ~ "d,e"
    ),
    code = coalesce(code, str_extract(name, "A35[※\\*]_\\d+")),
    name = str_remove(name, "（A35[※\\*]_\\d+）")
  ) %>% 
  tidyr::separate_rows(variants, sep = ",") %>%
  mutate(code = str_replace(code, "[※\\*]", variants)) %>%
  arrange(code) %>% 
  transmute(
    name,
    code,
    description,
    type,
    codelist = detect_codelist(type)
  ) %>% 
  readr::write_csv(file.path(out_exact, "A35b.csv"))
```

### `A37`

正規表現とexact match

``` r
d %>%
  filter(id == "A37", is.na(code)) %>% 
  knitr::kable()
```

| id  | name                                                         | code | description                                                                                                                     | type              |
|:----|:-------------------------------------------------------------|:-----|:--------------------------------------------------------------------------------------------------------------------------------|:------------------|
| A37 | 各年別救急車出動件数（A37\_34XXXX）※XXXX：該当年の西暦       | NA   | 指定地域の市町村単位に集計した救急自動車で傷病者を搬送した年間出動件数（指定年次～2013年）                                      | 整数型（Integer） |
| A37 | 各年別消防防災ヘリ出動件数（A37\_35XXXX）※XXXX：該当年の西暦 | NA   | 地域指定当時の市町村単位で集計した消防防災ヘリコプターで傷病者を救急搬送した年間の出動件数（指定年次\~2013年）                  | 整数型（Integer） |
| A37 | 各年別平均現場到着所要時間（A37\_36XXXX）※XXXX：該当年の西暦 | NA   | 地域指定当時の市町村単位で集計した救急事故の覚知（119番通報）から現場に到着するまでに要した平均時間（分）（地域指定年～2013年） | 実数型（Real）    |
| A37 | 各年別平均病院収容時間（A37\_37XXXX）※XXXX：該当年の西暦     | NA   | 地域指定当時の市町村単位で集計した覚知（119番通報）から病院収容までの平均時間（分）（地域指定年～2013年）                       | 実数型（Real）    |

``` r
d %>%
  filter(id == "A37", !is.na(code)) %>% 
  mutate(
    codelist = detect_codelist(type)
  ) %>% 
  select(!id) %>% 
  readr::write_csv(file.path(out_other, "A37.csv"))
```

### `A38`

一次医療圏、二次医療圏、三次医療圏、を名前の前につける必要がある。

``` r
d %>%
  filter(id == "A38") %>% 
  # code がないところが暗黙のサブテーブルのヘッダになっている
  group_by(subgroup = cumsum(is.na(code))) %>% 
  # そのサブテーブルのヘッダを prefix として各名前にくっつける
  mutate(name = paste(first(name), name, sep = "_")) %>% 
  # くっつけたらもう不要なので削除
  slice(-1) %>% 
  ungroup() %>% 
  transmute(
    name,
    code,
    description,
    type,
    codelist = detect_codelist(type)
  ) %>% 
  readr::write_csv(file.path(out_exact, "A38.csv"))
```

### `C23`

これはよくわからないけど、「海岸保全区域・海岸管理者名」の前に「海岸保全区域・海岸管理者コード」が入ってるっぽい。

``` r
d_tmp <- d %>%
  filter(id == "C23")

idx <- which(d_tmp$name == "海岸保全区域・海岸管理者名")

d_tmp_new <- d_tmp[idx, ]
d_tmp_new$name <- "海岸保全区域・海岸管理者コード"

bind_rows(
  d_tmp[1:(idx - 1L), ],
  d_tmp_new,
  d_tmp[idx:nrow(d_tmp), ]
) %>% 
  mutate(
    codelist = detect_codelist(type)
  ) %>% 
  select(!id) %>% 
  readr::write_csv(file.path(out_other, "C23.csv"))
```

### `C28`

どうやら `C28_000` がIDで、`C28_101`〜`C28_104`
それぞれが各データのIDを指しているらしい。
ここは手動で加えることにする。

``` r
d %>%
  filter(id == "C28", !is.na(code)) %>% 
  mutate(
    codelist = detect_codelist(type)
  ) %>% 
  select(!id) %>% 
  rows_insert(
    tribble(
      ~name, ~code, ~description, ~type, ~codelist,
      "ID",                "C28_000", NA, "文字列型", FALSE,
      "標点ID",            "C28_101", NA, "文字列型", FALSE,
      "ターミナルビルID",  "C28_102", NA, "文字列型", FALSE,
      "調査内容ID",        "C28_103", NA, "文字列型", FALSE,
      "滑走路ID",          "C28_104", NA, "文字列型", FALSE
    )
  ) %>% 
  readr::write_csv(file.path(out_exact, "C28.csv"))
```

    ## Matching, by = "name"

### `G02`

これは
HTML、エクセルともに定義を見つけられなかったので、GMLに入っていたものを使う（`grep -E "<gml:(Category|Count)List[^>]*>" cache/G02-12_4229-jgd_GML/G02-12_4229-jgd_GML/G02-12_4229-jgd.xml | sed 's/^.*>\(.*\)<.*$/\1/'`）。

``` r
G02_names <- c(
  "3次メッシュコード",
  "1月降水量",
  "2月降水量",
  "3月降水量",
  "4月降水量",
  "5月降水量",
  "6月降水量",
  "7月降水量",
  "8月降水量",
  "9月降水量",
  "10月降水量",
  "11月降水量",
  "12月降水量",
  "年降水量",
  "1月最高気温",
  "1月最低気温",
  "1月平均気温",
  "2月最高気温",
  "2月最低気温",
  "2月平均気温",
  "3月最高気温",
  "3月最低気温",
  "3月平均気温",
  "4月最高気温",
  "4月最低気温",
  "4月平均気温",
  "5月最高気温",
  "5月最低気温",
  "5月平均気温",
  "6月最高気温",
  "6月最低気温",
  "6月平均気温",
  "7月最高気温",
  "7月最低気温",
  "7月平均気温",
  "8月最高気温",
  "8月最低気温",
  "8月平均気温",
  "9月最高気温",
  "9月最低気温",
  "9月平均気温",
  "10月最高気温",
  "10月最低気温",
  "10月平均気温",
  "11月最高気温",
  "11月最低気温",
  "11月平均気温",
  "12月最高気温",
  "12月最低気温",
  "12月平均気温",
  "年最高気温",
  "年最低気温",
  "年平均気温",
  "1月最深積雪",
  "2月最深積雪",
  "3月最深積雪",
  "12月最深積雪",
  "年最深積雪",
  "1月日照時間",
  "2月日照時間",
  "3月日照時間",
  "4月日照時間",
  "5月日照時間",
  "6月日照時間",
  "7月日照時間",
  "8月日照時間",
  "9月日照時間",
  "10月日照時間",
  "11月日照時間",
  "12月日照時間",
  "年合計日照時間",
  "1月全天日射量",
  "2月全天日射量",
  "3月全天日射量",
  "4月全天日射量",
  "5月全天日射量",
  "6月全天日射量",
  "7月全天日射量",
  "8月全天日射量",
  "9月全天日射量",
  "10月全天日射量",
  "11月全天日射量",
  "12月全天日射量",
  "年平均全天日射量"
)

tibble(name = G02_names, code = NA, description = NA, type = NA, codelist = FALSE) %>% 
  readr::write_csv(file.path(out_positional, "G02.csv"))
```

### `L03-a`

`L03-a`はコードではなく普通の名前で入ってるので使わなくて問題なさそう

### `mesh1000`, `mesh500`

ここは列名そのままのほうが処理しやすそうなのでこのままにする

### `N05`

1つのセルの中に2つの値が入っている。手動で展開する。

``` r
d_list <- d %>%
  filter(id == "N05") %>%
  split(.$code == "N05_005e")

bind_rows(
  select(d_list$`FALSE`, !id),
  tibble(
    name = c("設置期間（設置開始）", "設置期間（設置終了）"),
    code = c("N05_005b", "N05_005e"),
    description = d_list$`TRUE`$description,
    type = d_list$`TRUE`$type
  )
) %>% 
  arrange(code) %>% 
  mutate(
    codelist = detect_codelist(type)
  ) %>% 
  readr::write_csv(file.path(out_exact, "N05.csv"))
```

### `P11`

Excelに範囲で入っているコードを手動展開

``` r
d_list <- d %>%
  filter(id == "P11")

codes <- bind_rows(
  tibble(name = c("バス停名", "バス区分"), code = c("P11_001", "P11_002"), num = NA_integer_),
  tibble(name = "事業者名", code = "P11_003", num = 1:19),
  tibble(name = "バス系統", code = "P11_004", num = 1:19),
)

d_list %>% 
  select(!code) %>% 
  inner_join(codes, by = "name") %>% 
  arrange(code) %>% 
  transmute(
    id,
    name = if_else(is.na(num), name, paste(name, num, sep = "_")),
    code = if_else(is.na(num), code, paste(code, num, sep = "_")),
    description,
    type,
    codelist = detect_codelist(type)
  ) %>% 
  readr::write_csv(file.path(out_exact, "P11.csv"))
```

### `P15`

〜で示されている範囲を展開する必要がある。

``` r
d %>%
  filter(id == "P15", is.na(code)) %>% 
  knitr::kable()
```

| id  | name                           | code | description                          | type                        |
|:----|:-------------------------------|:-----|:-------------------------------------|:----------------------------|
| P15 | 取扱品目1～15（P15\_019～033） | NA   | 産業廃棄物処理施設が取り扱える品目１ | 文字列型（CharacterString） |

``` r
d_tmp <- d %>%
  filter(id == "P15") %>% 
  split(is.na(.$code))

name <- d_tmp$`TRUE`$name
code <- str_extract(name, "(?<=（)[A-Z0-9_\\-～〜~]+(?=）$)")
name <- str_remove(name, "（[A-Z0-9_\\-～〜~]+）$")

expand_range <- function(x) {
  x_range <- str_extract_all(x, "\\d+[\\-～〜~]\\d+")[[1]]
  
  x_range <- str_split(x_range, "[\\-～〜~]")[[1]]
  x_begin <- x_range[1]
  x_end   <- x_range[2]
  
  # フォーマットを推測
  if (nchar(x_begin) > 1  && startsWith(x_begin, "0")) {
    format <- sprintf("%%0%dd", nchar(x_begin))
  } else {
    format <- "%d"
  }
  
  x_range_expanded <- sprintf(format, seq(as.integer(x_begin), as.integer(x_end)))
  str_replace(x, "\\d+[\\-～〜~]\\d+", x_range_expanded)
}

name_expanded <- expand_range(name)
code_expanded <- expand_range(code)

# 末尾の数字を置き換える
description_expanded <- d_tmp$`TRUE`$description %>% 
  str_replace("[0-9０-９]$", as.character(seq_along(name_expanded)))

d_expanded <- tibble(
  name = name_expanded,
  code = code_expanded,
  description = description_expanded,
  type = d_tmp$`TRUE`$type
)

d_tmp2 <- bind_rows(
  select(d_tmp$`FALSE`, !id),
  d_expanded
)

knitr::kable(d_tmp2)
```

| name              | code     | description                                               | type                                                                 |
|:------------------|:---------|:----------------------------------------------------------|:---------------------------------------------------------------------|
| 施設名称          | P15\_001 | 廃棄物処理施設の名称                                      | 文字列型（CharacterString）                                          |
| 地方公共団体名    | P15\_002 | 廃棄物処理施設の管理地方公共団体名                        | 文字列型（CharacterString）                                          |
| 施設種別          | P15\_003 | 施設種別コード                                            | コードリスト「施設種別コード」                                       |
| 施設タイプ        | P15\_004 | 廃棄物処理施設の種類                                      | 文字列型（CharacterString）                                          |
| 処理能力（t/日）  | P15\_005 | 廃棄物処理能力のうち、t/日の単位で示されるもの            | 実数型（Real）                                                       |
| 屋内面積          | P15\_006 | 保管施設の「屋内面積」                                    | 実数型（Real）                                                       |
| 屋外面積          | P15\_007 | 保管施設の「屋外面積」                                    | 実数型（Real）                                                       |
| 全体容量          | P15\_008 | 最終処分場の「全体容量（m3）」                            | 実数型（Real）                                                       |
| 処理能力（kL/日） | P15\_009 | 廃棄物処理能力のうち、kL/日の単位で示されるもの           | 実数型（Real）                                                       |
| 計画最大汚水量    | P15\_010 | コミュニティプラントの処理能力「計画最大汚水量（m3/日）」 | 実数型（Real）                                                       |
| 処理物            | P15\_011 | 廃棄物処理施設が処理できる対象物                          | 文字列型（CharacterString）                                          |
| 処理方式          | P15\_012 | 廃棄物処理施設の処理方式                                  | 文字列型（CharacterString）                                          |
| 炉形式            | P15\_013 | 廃棄物処理施設の焼却施設の炉形式                          | 文字列型（CharacterString）                                          |
| 発電能力          | P15\_014 | 焼却施設の発電能力（kw）                                  | 実数型（Real）                                                       |
| 事業者名          | P15\_015 | 廃棄物処理施設の名称                                      | 文字列型（CharacterString）                                          |
| 所在地            | P15\_016 | 廃棄物処理施設の住所                                      | 文字列型（CharacterString）                                          |
| 産廃施設種別      | P15\_017 | 産廃施設コード                                            | 産廃施設コード中間処理施設=1、最終処理施設=2                         |
| 特別管理          | P15\_018 | 特別管理コード                                            | 特別管理コード特別管理産業廃棄物処理以外=0、特別管理産業廃棄物処理=1 |
| 取扱品目1         | P15\_019 | 産業廃棄物処理施設が取り扱える品目1                       | 文字列型（CharacterString）                                          |
| 取扱品目2         | P15\_020 | 産業廃棄物処理施設が取り扱える品目2                       | 文字列型（CharacterString）                                          |
| 取扱品目3         | P15\_021 | 産業廃棄物処理施設が取り扱える品目3                       | 文字列型（CharacterString）                                          |
| 取扱品目4         | P15\_022 | 産業廃棄物処理施設が取り扱える品目4                       | 文字列型（CharacterString）                                          |
| 取扱品目5         | P15\_023 | 産業廃棄物処理施設が取り扱える品目5                       | 文字列型（CharacterString）                                          |
| 取扱品目6         | P15\_024 | 産業廃棄物処理施設が取り扱える品目6                       | 文字列型（CharacterString）                                          |
| 取扱品目7         | P15\_025 | 産業廃棄物処理施設が取り扱える品目7                       | 文字列型（CharacterString）                                          |
| 取扱品目8         | P15\_026 | 産業廃棄物処理施設が取り扱える品目8                       | 文字列型（CharacterString）                                          |
| 取扱品目9         | P15\_027 | 産業廃棄物処理施設が取り扱える品目9                       | 文字列型（CharacterString）                                          |
| 取扱品目10        | P15\_028 | 産業廃棄物処理施設が取り扱える品目10                      | 文字列型（CharacterString）                                          |
| 取扱品目11        | P15\_029 | 産業廃棄物処理施設が取り扱える品目11                      | 文字列型（CharacterString）                                          |
| 取扱品目12        | P15\_030 | 産業廃棄物処理施設が取り扱える品目12                      | 文字列型（CharacterString）                                          |
| 取扱品目13        | P15\_031 | 産業廃棄物処理施設が取り扱える品目13                      | 文字列型（CharacterString）                                          |
| 取扱品目14        | P15\_032 | 産業廃棄物処理施設が取り扱える品目14                      | 文字列型（CharacterString）                                          |
| 取扱品目15        | P15\_033 | 産業廃棄物処理施設が取り扱える品目15                      | 文字列型（CharacterString）                                          |

``` r
d_tmp2 %>% 
  mutate(
    codelist = detect_codelist(type)
  ) %>% 
  readr::write_csv(file.path(out_exact, "P15.csv"))
```

### `P16`

〜で示されている範囲を展開する必要がある。

``` r
d %>%
  filter(id == "P16", is.na(code)) %>% 
  knitr::kable()
```

| id  | name                      | code | description      | type                        |
|:----|:--------------------------|:-----|:-----------------|:----------------------------|
| P16 | 備考1-12（P16\_015～026） | NA   | 研究分野等の備考 | 文字列型（CharacterString） |

``` r
d_tmp <- d %>%
  filter(id == "P16") %>% 
  split(is.na(.$code))

name <- d_tmp$`TRUE`$name
code <- str_extract(name, "(?<=（)[A-Z0-9_\\-～〜~]+(?=）$)")
name <- str_remove(name, "（[A-Z0-9_\\-～〜~]+）$")

name_expanded <- expand_range(name)
code_expanded <- expand_range(code)

# 末尾の数字を置き換える
description_expanded <- d_tmp$`TRUE`$description %>% 
  str_replace("[0-9０-９]$", as.character(seq_along(name_expanded)))

d_expanded <- tibble(
  name = name_expanded,
  code = code_expanded,
  description = description_expanded,
  type = d_tmp$`TRUE`$type
)

d_tmp2 <- bind_rows(
  select(d_tmp$`FALSE`, !id),
  d_expanded
)

knitr::kable(d_tmp2)
```

| name           | code     | description                                             | type                                              |
|:---------------|:---------|:--------------------------------------------------------|:--------------------------------------------------|
| 名称           | P16\_001 | 研究機関の名称                                          | 文字列型（CharacterString）                       |
| 主体コード     | P16\_002 | 研究機関の設立主体                                      | 数値型（Decimal）コードリスト「主体コードリスト」 |
| 都道府県コード | P16\_003 | 地方公共団体コード                                      | 数値型（Decimal）                                 |
| 所在地         | P16\_004 | 研究機関の所在地                                        | 文字列型（CharacterString）                       |
| ID             | P16\_005 | 研究機関のID                                            | 文字列型（CharacterString）                       |
| 原典1          | P16\_006 | 原典資料が「当該研究機関が公開している情報」の場合“1”   | 数値型（Decimal）                                 |
| 原典2          | P16\_007 | 原典資料が「ReaDリスト」の場合“1”                       | 数値型（Decimal）                                 |
| 原典3          | P16\_008 | 原典資料が「J-SBIRリスト」の場合“1”                     | 数値型（Decimal）                                 |
| 原典4          | P16\_009 | 原典資料が「都道府県への問い合わせによる資料」の場合“1” | 数値型（Decimal）                                 |
| 原典5          | P16\_010 | 原典資料が「KAKENリスト」の場合“1”                      | 数値型（Decimal）                                 |
| 原典6          | P16\_011 | 原典資料が「日本学術振興会リスト」の場合“1”             | 数値型（Decimal）                                 |
| 原典7          | P16\_012 | 原典資料が「独立行政法人一覧」の場合“1”                 | 数値型（Decimal）                                 |
| 原典8          | P16\_013 | 原典資料が「大学共同利用機関法人」の場合“1”             | 数値型（Decimal）                                 |
| 原典9          | P16\_014 | 原典資料がその他の場合“1”                               | 数値型（Decimal）                                 |
| 備考1          | P16\_015 | 研究分野等の備考                                        | 文字列型（CharacterString）                       |
| 備考2          | P16\_016 | 研究分野等の備考                                        | 文字列型（CharacterString）                       |
| 備考3          | P16\_017 | 研究分野等の備考                                        | 文字列型（CharacterString）                       |
| 備考4          | P16\_018 | 研究分野等の備考                                        | 文字列型（CharacterString）                       |
| 備考5          | P16\_019 | 研究分野等の備考                                        | 文字列型（CharacterString）                       |
| 備考6          | P16\_020 | 研究分野等の備考                                        | 文字列型（CharacterString）                       |
| 備考7          | P16\_021 | 研究分野等の備考                                        | 文字列型（CharacterString）                       |
| 備考8          | P16\_022 | 研究分野等の備考                                        | 文字列型（CharacterString）                       |
| 備考9          | P16\_023 | 研究分野等の備考                                        | 文字列型（CharacterString）                       |
| 備考10         | P16\_024 | 研究分野等の備考                                        | 文字列型（CharacterString）                       |
| 備考11         | P16\_025 | 研究分野等の備考                                        | 文字列型（CharacterString）                       |
| 備考12         | P16\_026 | 研究分野等の備考                                        | 文字列型（CharacterString）                       |

``` r
d_tmp2 %>% 
  mutate(
    codelist = detect_codelist(type)
  ) %>% 
  readr::write_csv(file.path(out_exact, "P16.csv"))
```

### `P17`

〜の範囲が定まっていないのでexact
matchできない。それ以外の列についてはexact matchで大丈夫。

``` r
d %>%
  filter(id == "P17", is.na(code)) %>% 
  knitr::kable()
```

| id  | name                           | code | description                          | type                        |
|:----|:-------------------------------|:-----|:-------------------------------------|:----------------------------|
| P17 | 管轄範囲（1～n）（P17\_006～） | NA   | 各消防署の管轄範囲を記載（町丁目）。 | 文字列型（CharacterString） |

``` r
d %>%
  filter(id == "P17", !is.na(code)) %>% 
  mutate(
    codelist = detect_codelist(type)
  ) %>% 
  select(!id) %>% 
  readr::write_csv(file.path(out_other, "P17.csv"))
```

### `P18`

〜の範囲が定まっていないのでexact
matchできない。それ以外の列についてはexact matchで大丈夫。

``` r
d %>%
  filter(id == "P18", is.na(code)) %>% 
  knitr::kable()
```

| id  | name                           | code | description                          | type                        |
|:----|:-------------------------------|:-----|:-------------------------------------|:----------------------------|
| P18 | 管轄範囲（1～n）（P18\_006～） | NA   | 各警察署の管轄範囲を記載（町丁目）。 | 文字列型（CharacterString） |

``` r
d %>%
  filter(id == "P18", !is.na(code)) %>% 
  mutate(
    codelist = detect_codelist(type)
  ) %>% 
  select(!id) %>% 
  readr::write_csv(file.path(out_other, "P18.csv"))
```

### `P21`

タイポなので手動で修正

``` r
d %>%
  filter(id == "P21", is.na(code)) %>%
  knitr::kable()
```

| id  | name                     | code | description                                                         | type                                                                           |
|:----|:-------------------------|:-----|:--------------------------------------------------------------------|:-------------------------------------------------------------------------------|
| P21 | 事業主体（P21a-001）     | NA   | 当該給水区域の事業主体                                              | 文字列型                                                                       |
| P21 | 事業名称（P21a-002）     | NA   | 当該給水区域の水道事業の名称                                        | 文字列型                                                                       |
| P21 | 種別（P21a-003）         | NA   | 当該給水区域の種別から得られた種別コード                            | コードリスト型（種別コード）上水道=1、簡易水道（公営）=2、簡易水道（非公営）=3 |
| P21 | 給水人口（P21a-004）     | NA   | 給水区域内に居住し、水道により給水を受けている人口。                | 整数型                                                                         |
| P21 | 日最大給水量（P21a-005） | NA   | 一日に給水することができる最大の水量（水道法第三条6項二）。単位：m3 | 実数型                                                                         |
| P21 | 事業主体（P21b-001）     | NA   | 当該給水区域の事業主体                                              | 文字列型                                                                       |
| P21 | 事業名称（P21b-002）     | NA   | 当該給水区域の水道事業の名称                                        | 文字列型                                                                       |
| P21 | 施設名称（P21b-003）     | NA   | 当該施設の名称                                                      | 文字列型                                                                       |
| P21 | 日最大給水量（P21b-004） | NA   | 一日に給水することができる最大の水量（水道法第三条6項二）。単位：m3 | 実数型                                                                         |

``` r
d %>% 
  filter(id == "P21", is.na(code)) %>%
  mutate(
    code = str_extract(name, "(?<=（)[A-Za-z0-9_\\-]+(?=）$)"),
    # a, bを大文字に
    code = str_to_upper(code),
    # - を _ に
    code = str_replace(code, "-", "_"),
    name = str_remove(name, "（.*）$")
  ) %>% 
  mutate(
    codelist = detect_codelist(type)
  ) %>% 
  select(!id) %>% 
  readr::write_csv(file.path(out_exact, "P21.csv"))
```

### `W09`

`ID`列は変換する必要がなさそうなので除外する。

``` r
d %>%
  filter(id == "W09", is.na(code)) %>%
  knitr::kable()
```

| id  | name | code | description        | type                        |
|:----|:-----|:-----|:-------------------|:----------------------------|
| W09 | iD   | NA   | 湖沼ごとの整理番号 | 文字列型（CharacterString） |

``` r
d %>%
  filter(id == "W09", !is.na(code)) %>%
  mutate(
    codelist = detect_codelist(type)
  ) %>% 
  select(!id) %>% 
  readr::write_csv(file.path(out_exact, "W09.csv"))
```
