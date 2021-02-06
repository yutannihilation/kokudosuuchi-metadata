Validate codelist
================

codelistがカラムとマッチするか確認する

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

いくつか重複があるので、`id`, `code`,
`name`の組がユニークになるようにまとめなおす

``` r
d %>%
  group_by(id, code, name) %>%
  filter(n() > 1) %>%
  knitr::kable()
```

| id    | name                      | code | description                                                              | type                                                           | codelist |
|:------|:--------------------------|:-----|:-------------------------------------------------------------------------|:---------------------------------------------------------------|:---------|
| C02   | 港湾コード                | NA   | 港湾を特定するためのコード都道府県コード番号（2桁）＋一意番号（3桁）     | 整数型                                                         | FALSE    |
| C02   | 県コード                  | NA   | 都道府県を一意に識別するために付された番号                               | コードリスト「都道府県コード」                                 | TRUE     |
| C02   | 港湾コード                | NA   | 港湾を特定するためのコード都道府県コード番号（2桁）＋一意番号（3桁）     | 整数型                                                         | FALSE    |
| C02   | 県コード                  | NA   | 都道府県を一意に識別するために付された番号                               | コードリスト「都道府県コード」                                 | TRUE     |
| C02   | 港湾コード                | NA   | 港湾を特定するためのコード都道府県コード番号（2桁）＋一意番号（3桁）     | 整数型                                                         | FALSE    |
| S05-b | 原付バイク-登校トリップ数 | NA   | 交通手段「原付バイク」、目的「登校」のトリップ数                         | 整数型　（\*2）                                                | FALSE    |
| S05-b | 原付バイク-登校トリップ数 | NA   | 交通手段「原付バイク」、目的「不明」のトリップ数                         | 整数型　（\*2）                                                | FALSE    |
| S05-c | 調査年度                  | NA   | パーソントリップ調査の実施年度                                           | 整数型                                                         | FALSE    |
| S05-c | 駅コード                  | NA   | 駅のコード                                                               | コードリスト「東京都市圏駅コード」                             | TRUE     |
| S05-c | 駅名                      | NA   | 駅の名称                                                                 | コードリスト「東京都市圏駅コード」                             | TRUE     |
| S05-c | 乗車\_タクシー・ハイヤー  | NA   | 端末交通手段「タクシー・ハイヤー」を降りて対象駅から乗車するトリップ数   | 整数型                                                         | FALSE    |
| S05-c | 乗車\_自転車              | NA   | 端末交通手段「自転車」を降りて対象駅から乗車するトリップ数               | 整数型                                                         | FALSE    |
| S05-c | 乗車\_徒歩                | NA   | 端末交通手段「徒歩」を降りて対象駅から乗車するトリップ数                 | 整数型                                                         | FALSE    |
| S05-c | 乗車\_その他              | NA   | その他の端末交通手段を降りて対象駅から乗車するトリップ数                 | 整数型                                                         | FALSE    |
| S05-c | 乗車\_合計                | NA   | 端末交通手段を降りて対象駅から乗車する合計トリップ数                     | 整数型                                                         | FALSE    |
| S05-c | 降車\_タクシー・ハイヤー  | NA   | 対象駅で降車し、端末交通手段「タクシー・ハイヤー」に乗り換えるトリップ数 | 整数型                                                         | FALSE    |
| S05-c | 降車\_自転車              | NA   | 対象駅で降車し、端末交通手段「自転車」に乗り換えるトリップ数             | 整数型                                                         | FALSE    |
| S05-c | 降車\_徒歩                | NA   | 対象駅で降車し、端末交通手段「徒歩」に乗り換えるトリップ数               | 整数型                                                         | FALSE    |
| S05-c | 降車\_その他              | NA   | 対象駅で降車し、その他の端末交通手段に乗り換えるトリップ数               | 整数型                                                         | FALSE    |
| S05-c | 降車\_合計                | NA   | 対象駅で降車し、端末交通手段に乗り換える合計トリップ数                   | 整数型                                                         | FALSE    |
| S05-c | 調査年度                  | NA   | パーソントリップ調査の実施年度                                           | 整数型                                                         | FALSE    |
| S05-c | 駅コード                  | NA   | 駅のコード                                                               | コードリスト「近畿圏H12年度駅コード」「近畿圏H22年度駅コード」 | TRUE     |
| S05-c | 駅名                      | NA   | 駅の名称                                                                 | コードリスト「近畿圏H12年度駅コード」「近畿圏H22年度駅コード」 | TRUE     |
| S05-c | 乗車\_タクシー・ハイヤー  | NA   | 端末交通手段「タクシー・ハイヤー」を降りて対象駅から乗車するトリップ数   | 整数型                                                         | FALSE    |
| S05-c | 乗車\_自転車              | NA   | 端末交通手段「自転車」を降りて対象駅から乗車するトリップ数               | 整数型                                                         | FALSE    |
| S05-c | 乗車\_徒歩                | NA   | 端末交通手段「徒歩」を降りて対象駅から乗車するトリップ数                 | 整数型                                                         | FALSE    |
| S05-c | 乗車\_その他              | NA   | その他の端末交通手段を降りて対象駅から乗車するトリップ数                 | 整数型                                                         | FALSE    |
| S05-c | 乗車\_合計                | NA   | 端末交通手段を降りて対象駅から乗車する合計トリップ数                     | 整数型                                                         | FALSE    |
| S05-c | 降車\_タクシー・ハイヤー  | NA   | 対象駅で降車し、端末交通手段「タクシー・ハイヤー」に乗り換えるトリップ数 | 整数型                                                         | FALSE    |
| S05-c | 降車\_自転車              | NA   | 対象駅で降車し、端末交通手段「自転車」に乗り換えるトリップ数             | 整数型                                                         | FALSE    |
| S05-c | 降車\_徒歩                | NA   | 対象駅で降車し、端末交通手段「徒歩」に乗り換えるトリップ数               | 整数型                                                         | FALSE    |
| S05-c | 降車\_その他              | NA   | 対象駅で降車し、その他の端末交通手段に乗り換えるトリップ数               | 整数型                                                         | FALSE    |
| S05-c | 降車\_合計                | NA   | 対象駅で降車し、端末交通手段に乗り換える合計トリップ数                   | 整数型                                                         | FALSE    |

``` r
d <- d %>% 
  group_by(id, code, name) %>% 
  summarise(
    across(.fns = first),
    .groups = "drop"
  )
```

個別対応したものは先に紐づける。ここで紐付かなかったものに対して、対応表を正規表現でマッチさせる。

``` r
d <- d %>%
  mutate(
    type = str_squish(type),
    codelist_id_manual = case_when(
      code %in% c("G04a_005", "G04c_005", "G04d_005") ~ "undersea",
      code %in% c("G04a_007", "G04a_009","G04c_007","G04c_009","G04d_007","G04d_009") ~ "direction",
      code == "A15_003" ~ "authority_type",
      code == "A15_004" ~ "protection_area_type",
      code == "P13_009" ~ "urban_planning_decided",
      code == "P15_017" ~ "industrial_waste_disposal",
      code == "P15_018" ~ "industrial_waste_special_treatment",
      code == "P17_003" ~ "firehouse_type",
      code == "P21A_003" ~ "water_supply_type",
      code == "P24_011" ~ "refereced_from_agri",
      code == "L03b_c_002" ~ "tweak_LandUseCd",
      code %in% c("P17_002", "P18_002", "P20_001", "P24_002", "P32_003") ~ "AdminAreaCd",
      code == "P19_002" ~ "PrefCd",
      code == "L01_005" ~ "seaside_type",

      id == "A35b" & name %in% c("種別", "種別コード") ~ "landscape_district_type",
      id %in% c("C02", "C09", "C23") & name == "行政区域コード" ~ "AdminAreaCd",
      id == "L01" & name == "標準地行政区域コード" ~ "AdminAreaCd",
      id == "L02" & name == "基準地行政区域コード" ~ "AdminAreaCd",
      id == "P02" & name == "行政区域コード" ~ "AdminAreaCd",
      id == "P02" & name == "行政区域コード" ~ "AdminAreaCd",
      id == "P02" & name == "行政区域コード" ~ "AdminAreaCd",
    )
  )
```

URLから取ってきた分を`d`に紐付ける。
まずは`codelist_list.csv`を読み込み。

``` r
d_codelist <- readr::read_csv(here::here("data", "codelist_list.csv"), col_types = "ccc")

# いくつか除外したやつもあるので、実際に存在するやつだけに絞り込む
codelist_existing <- fs::dir_ls(here::here("data", "codelist")) %>% 
  basename() %>% 
  tools::file_path_sans_ext()

# LandUseCd は tweak_LandUseCd の方を使うので除外
# SectionTypeCd/SectionCd も年度によって微妙に違うので個別対応が必要、ここでは除外する
exclude_pattern <- "^LandUseCd|^SectionTypeCd|^SectionCd"

d_codelist <- d_codelist %>% 
  filter(
    codelist_id %in% codelist_existing,
    !str_detect(codelist_id, exclude_pattern)
  ) %>% 
  mutate(type_part = str_trim(name), .keep = "unused")
```

次に、データを手動で紐づけたものとそれ以外に分割し、それ以外の方に正規表現でくっつける。その際、複数マッチする可能性があるのでなんとかがんばる。

``` r
d_joined_manual <- d %>% 
  filter(!is.na(codelist_id_manual)) %>% 
  select(id, code, name, type, codelist, codelist_id = codelist_id_manual)

d_joined_regex <- d %>% 
  filter(is.na(codelist_id_manual)) %>% 
  left_join(d_codelist, by = "id") %>% 
  mutate(
    matched = str_detect(type, fixed(type_part)),
    # 複数のパターンにマッチする場合は長い方を採用する（P31のみのはず）
    matched_size = if_else(matched, nchar(type_part), 0L)
  ) %>% 
  group_by(id, code, name, type, codelist) %>% 
  summarise(
    codelist_id = if (sum(matched, na.rm = TRUE) > 1) {
      list(codelist_id[which.max(matched_size)])
    } else {
      list(codelist_id[matched])
    },
    .groups = "drop"
  )

# data に入っているのが 0 か 1 であるのを確認してから展開
stopifnot(lengths(d_joined_regex$codelist_id) <= 1)

nrow_before <- nrow(d_joined_regex)

d_joined_regex <- d_joined_regex %>%
  tidyr::unchop(codelist_id, keep_empty = TRUE)

stopifnot(nrow_before == nrow(d_joined_regex))

d_joined <- bind_rows(d_joined_manual, d_joined_regex)
```

コードリスト型であると判定されているのに `codelist_id`
がないのはおかしそう。ただし、以下は例外。

-   `A03`の「区域コード」「区域区分」はあとで個別対応
-   `L03-b`, `L03-b-c`の「土地利用種別」はあとで個別対応
-   `P14`の「福祉施設細分類」はあとで個別対応
-   `S05-a`, `S05-b`, `S05-c`, `S05-d`はあとで個別対応
-   別カラムに都道府県などがあるので対応不要:
    -   `A03`, `A17~A25`
    -   `A38a_001`, `A38b_001`, `A39_001`, `A39_002`, `A45_002`,
        `A45_003`
-   `A45`の「局名称コード」「署名称コード」は別途ラベルもあるので対応不要
-   `A45`の「樹種コード」

``` r
# 元と同じ行数のはず
stopifnot(nrow(d) == nrow(d_joined))

d_joined %>%
  filter(
    codelist,
    is.na(codelist_id),
    !(id == "A03" & name %in% c("区域コード", "区域区分")),
    !(id %in% c("A03", "A17", "A18", "A19", "A22", "A23", "A24", "A25") & name == "行政区域コード"),
    !code %in% c("A38a_001", "A38b_001", "A39_001", "A39_002", "A45_002", "A45_003", "A45_008", "A45_009", "A45_015", "A45_018", "A45_021", "L05_005", "P14_006"),
    !(id %in% c("L03-b", "L03-b-u") & name == "土地利用種別"),
    !(str_detect(id, "S05-[a-d]") & str_detect(name, "ゾーンコード")),
    !(id == "S05-c" & name %in% c("運営会社", "駅コード", "駅名"))
  ) %>%
  { stopifnot(nrow(.) == 0) }

d_joined %>% 
  select(!codelist) %>% 
  readr::write_csv(here::here("data", "joined.csv"))
```
