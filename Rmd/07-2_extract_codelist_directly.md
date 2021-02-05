Extract codelist
================

## 方針

コードリストは、2つタイプがある。
そのページに直接列挙されているタイプと、リンク先に詳細があるタイプ。
リンク先に詳細があるタイプは、`html_table()`を使った時点で失われてしまうので、別途収集する。

## 展開

### 直接列挙タイプ

``` r
library(readr)
library(stringr)
library(dplyr, warn.conflicts = FALSE)

csv_files <- c(
  list.files(here::here("data", "colnames_exact"), full.names = TRUE),
  list.files(here::here("data", "colnames_positional"), full.names = TRUE),
  list.files(here::here("data", "colnames_other"), full.names = TRUE)
)
names(csv_files) <- tools::file_path_sans_ext(basename(csv_files))

col_types <- cols(
  name = col_character(),
  code = col_character(),
  description = col_character(),
  type = col_character()
)

d <- purrr::map_dfr(csv_files, read_csv, col_types = col_types, .id = "id")
```

``` r
out_dir <- here::here("data", "codelist")
dir.create(out_dir, showWarnings = FALSE)
```

### `G04a_005`, `G04a_005`, `G04d_005`

``` r
d %>% 
  filter(str_detect(type, "海面下"))
```

    ## # A tibble: 3 x 6
    ##   id    name     code   description                      type           codelist
    ##   <chr> <chr>    <chr>  <chr>                            <chr>          <lgl>   
    ## 1 G04-a 最低標高コード… G04a_… 最低標高が海面下であるかどうかの区分。※基盤地図情報の標高では… コードリスト型（海面下=5… TRUE    
    ## 2 G04-c 最低標高コード… G04c_… 最低標高が海面下であるかどうかの区分。※基盤地図情報の標高では… コードリスト型（海面下=5… TRUE    
    ## 3 G04-d 最低標高コード… G04d_… 最低標高が海面下であるかどうかの区分。※基盤地図情報の標高では… コードリスト型（海面下=5… TRUE

``` r
tibble(
  code  = c("0", "5", "unknown"),
  label = c("その他", "海面下", NA)
) %>% 
  readr::write_csv(file.path(out_dir, "undersea.csv"))
```

### `G04a_007`, `G04a_009`,`G04c_007`,`G04c_009`,`G04d_007`,`G04d_009`

``` r
d %>% 
  filter(str_detect(type, "方向なし"))
```

    ## # A tibble: 6 x 6
    ##   id    name    code   description                type                  codelist
    ##   <chr> <chr>   <chr>  <chr>                      <chr>                 <lgl>   
    ## 1 G04-a 最大傾斜方向… G04a_… 標高傾斜度5次メッシュ（250m）の標高から算出す… コードリスト型（0=方向なし、1=北、2… TRUE    
    ## 2 G04-a 最小傾斜方向… G04a_… 標高傾斜度5次メッシュ（250m）の標高から算出す… コードリスト型（0=方向なし、1=北、2… TRUE    
    ## 3 G04-c 最大傾斜方向… G04c_… 標高傾斜度5次メッシュ（250m）の標高から算出す… コードリスト型（0=方向なし、1=北、2… TRUE    
    ## 4 G04-c 最小傾斜方向… G04c_… 標高傾斜度5次メッシュ（250m）の標高から算出す… コードリスト型（0=方向なし、1=北、2… TRUE    
    ## 5 G04-d 最大傾斜方向… G04d_… 10mメッシュ標高をリサンプリングした50mメッシ… コードリスト型（0=方向なし、1=北、2… TRUE    
    ## 6 G04-d 最小傾斜方向… G04d_… 10mメッシュ標高をリサンプリングした50mメッシ… コードリスト型（0=方向なし、1=北、2… TRUE

``` r
tibble(
  code  = c(as.character(0:8), "unknown"),
  label = c("方向なし", "北", "北東", "東", "東南", "南", "南西", "西", "北西", NA)
) %>% 
  readr::write_csv(file.path(out_dir, "direction.csv"))
```

### `A15_003`

``` r
d %>% 
  filter(str_detect(type, "指定機関コード"))
```

    ## # A tibble: 1 x 6
    ##   id    name           code   description              type             codelist
    ##   <chr> <chr>          <chr>  <chr>                    <chr>            <lgl>   
    ## 1 A15   指定機関：指定機関コード… A15_0… 国指定及び県指定の区分1：国指定　2：県指定… コードリスト「指定機関コード」… TRUE

``` r
tibble(
  code  = c("1", "2"),
  label = c("国指定", "県指定")
) %>% 
  readr::write_csv(file.path(out_dir, "authority_type.csv"))
```

### `A15_004`

``` r
d %>% 
  filter(str_detect(type, "保護区分コード"))
```

    ## # A tibble: 1 x 6
    ##   id    name        code   description                     type         codelist
    ##   <chr> <chr>       <chr>  <chr>                           <chr>        <lgl>   
    ## 1 A15   保護区分：保護区分コ… A15_0… 鳥獣保護区と特別保護区域，休猟区の区別1：鳥獣保護区　2：特… コードリスト「保護区分… TRUE

``` r
tibble(
  code  = c("1", "2", "3", "4"),
  label = c("鳥獣保護区",　"特別保護地区", "休猟区", "特例休猟区")
) %>% 
  readr::write_csv(file.path(out_dir, "protection_area_type.csv"))
```

### `P13_009`

``` r
d %>% 
  filter(str_detect(type, "都市計画決定"))
```

    ## # A tibble: 1 x 6
    ##   id    name     code   description    type                             codelist
    ##   <chr> <chr>    <chr>  <chr>          <chr>                            <lgl>   
    ## 1 P13   都市計画決定… P13_0… 都市計画決定コード… コードリスト「都市計画決定コード」（０＝未確認、１＝決定、２＝… TRUE

``` r
tibble(
  code  = c("0", "1", "2"),
  label = c("未確認", "決定", "未決定")
) %>% 
  readr::write_csv(file.path(out_dir, "urban_planning_decided.csv"))
```

### `P15_017`

``` r
d %>% 
  filter(str_detect(type, "産廃施設コード"))
```

    ## # A tibble: 1 x 6
    ##   id    name       code   description   type                            codelist
    ##   <chr> <chr>      <chr>  <chr>         <chr>                           <lgl>   
    ## 1 P15   産廃施設種別… P15_0… 産廃施設コード… 産廃施設コード中間処理施設=1、最終処理施設=2… TRUE

``` r
tibble(
  code  = c("1", "2"),
  label = c("中間処理施設", "最終処理施設")
) %>% 
  readr::write_csv(file.path(out_dir, "industrial_waste_disposal.csv"))
```

### `P15_018`

``` r
d %>% 
  filter(str_detect(type, "特別管理コード"))
```

    ## # A tibble: 1 x 6
    ##   id    name    code   description  type                                codelist
    ##   <chr> <chr>   <chr>  <chr>        <chr>                               <lgl>   
    ## 1 P15   特別管理… P15_0… 特別管理コード… 特別管理コード特別管理産業廃棄物処理以外=0、特別管理産業廃棄物処理… TRUE

``` r
tibble(
  code  = c("0", "1"),
  label = c("特別管理産業廃棄物処理以外", "特別管理産業廃棄物処理")
) %>% 
  readr::write_csv(file.path(out_dir, "industrial_waste_special_treatment.csv"))
```

### `P17_003`

``` r
d %>% 
  filter(str_detect(type, "消防本部"))
```

    ## # A tibble: 1 x 6
    ##   id    name    code   description                  type                codelist
    ##   <chr> <chr>   <chr>  <chr>                        <chr>               <lgl>   
    ## 1 P17   種別コード… P17_0… 消防施設の区分。各消防本部で施設区分が異なるため、H2… コードリスト「種別コード」消防本部=… TRUE

``` r
tibble(
  code  = c("1", "2", "3"),
  label = c("消防本部", "消防署", "分署・出張所")
) %>% 
  readr::write_csv(file.path(out_dir, "firehouse_type.csv"))
```

### `P21A_003`

``` r
d %>% 
  filter(str_detect(type, "簡易水道"))
```

    ## # A tibble: 1 x 6
    ##   id    name  code   description           type                         codelist
    ##   <chr> <chr> <chr>  <chr>                 <chr>                        <lgl>   
    ## 1 P21   種別  P21A_… 当該給水区域の種別から得られた種別コード… コードリスト型（種別コード）上水道=1、簡易水道（公営… TRUE

``` r
tibble(
  code  = c("1", "2", "3"),
  label = c("上水道", "簡易水道（公営）", "簡易水道（非公営）")
) %>% 
  readr::write_csv(file.path(out_dir, "water_supply_type.csv"))
```

### `P24_011`

``` r
d %>% 
  filter(str_detect(type, "グリーン・ツーリズムデータベース"))
```

    ## # A tibble: 1 x 6
    ##   id    name    code   description       type                           codelist
    ##   <chr> <chr>   <chr>  <chr>             <chr>                          <lgl>   
    ## 1 P24   原典コード… P24_0… データの作成に使用した原典資料… コードリスト1:グリーン・ツーリズムデータベース2:農業協… TRUE

``` r
tibble(
  code  = c("1", "2", "3"),
  label = c("グリーン・ツーリズムデータベース", "農業協同組合名鑑", "全国市民農園リスト")
) %>% 
  readr::write_csv(file.path(out_dir, "refereced_from_agri.csv"))
```