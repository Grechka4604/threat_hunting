# pr 04 sokolov
sokol46532@yandex.ru

## Цель работы

1.  Закрепить практические навыки применения языка программирования R
    для обработки данных.
2.  Закрепить знания базовых функций обработки данных в экосистеме
    `tidyverse` языка R.
3.  Закрепить навыки анализа метаданных DNS-трафика.

## Исходные данные

1.  Операционная система: Windows 11
2.  IDE: RStudio
3.  Версия R: 4.5.2.
4.  Данные о DNS-трафике во внутренней сети Доброй Организации.

## Задание

С использованием пакета `dplyr` освоить анализ DNS-логов средствами
языка программирования R.

## Ход работы

### Подготовка данных

1.  Импортируйте данные DNS:
    <https://storage.yandexcloud.net/dataset.ctfsec/dns.zip>
2.  Добавьте недостающую информацию о структуре данных (назначение
    столбцов).
3.  Приведите данные в столбцах к требуемым форматам.
4.  Просмотрите общую структуру данных с помощью функции `glimpse()`.
5.  Оформите отчет в соответствии с шаблоном.

### Анализ данных

1.  Сколько участников информационного обмена присутствует в сети Доброй
    Организации?
2.  Каково соотношение участников обмена внутри сети и участников,
    обращающихся к внешним ресурсам?
3.  Определите топ-10 участников сети с наибольшей сетевой активностью.
4.  Определите топ-10 доменов, к которым обращаются пользователи сети, и
    соответствующее количество запросов.
5.  Рассчитайте базовые статистические характеристики (функция
    `summary()`) интервала времени между последовательными обращениями к
    топ-10 доменам.
6.  Зачастую вредоносное ПО использует DNS-канал как канал управления,
    периодически отправляя запросы на контролируемый злоумышленником
    DNS-сервер. По регулярным запросам к одному и тому же домену можно
    выявлять скрытый DNS-канал. Есть ли такие IP-адреса в исследуемом
    датасете?

### Обогащение данных

1.  Определите местоположение (страна, город) и организацию-провайдера
    для топ-10 доменов. Для этого можно использовать сторонние сервисы,
    например <http://ip-api.com> (API-эндпоинт —
    <http://ip-api.com/json>).

### Шаг 1

Подключаем пакет, который будем использовать для обработки данных:

``` r
suppressPackageStartupMessages(library(dplyr))
```

Считываем DNS-логи из файла `dns.log` и сохраняем их в таблицу:

``` r
dns_data <- read.table(
file = "dns.log",
sep = "\t",
header = FALSE,
fill = TRUE,
quote = "",
comment.char = "",
stringsAsFactors = FALSE
)
```

Задаём названия столбцов в соответствии со структурой DNS-логов:

``` r
column_names <- c(
"ts","uid","id.orig_h","id.orig_p","id.resp_h","id.resp_p",
"proto","trans_id","query","qclass","qclass_name","qtype",
"qtype_name","rcode","rcode_name","AA","TC","RD","RA","Z",
"answers","TTLs","rejected"
)
names(dns_data) <- column_names[seq_len(ncol(dns_data))]
```

Приводим типы данных к корректным значениям (время и порты), чтобы
дальше было удобно анализировать:

``` r
dns_data <- dns_data %>%
mutate(
ts = as.POSIXct(as.numeric(ts), origin = "1970-01-01", tz = "UTC"),
id.orig_p = suppressWarnings(as.integer(id.orig_p)),
id.resp_p = suppressWarnings(as.integer(id.resp_p))
)
```

Выводим общую информацию о структуре и типах столбцов:

``` r
glimpse(dns_data)
```

    Rows: 427,935
    Columns: 23
    $ ts          <dttm> 2012-03-16 12:30:05, 2012-03-16 12:30:15, 2012-03-16 12:3…
    $ uid         <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jljz7Bs…
    $ id.orig_h   <chr> "192.168.202.100", "192.168.202.76", "192.168.202.76", "19…
    $ id.orig_p   <int> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 1…
    $ id.resp_h   <chr> "192.168.27.203", "192.168.202.255", "192.168.202.255", "1…
    $ id.resp_p   <int> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137…
    $ proto       <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "udp", "u…
    $ trans_id    <int> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 62187, 62…
    $ query       <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\…
    $ qclass      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"…
    $ qclass_name <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTERNET", "C…
    $ qtype       <chr> "33", "32", "32", "32", "32", "32", "32", "32", "32", "32"…
    $ qtype_name  <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB…
    $ rcode       <chr> "0", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ rcode_name  <chr> "NOERROR", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-…
    $ AA          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ TC          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ RD          <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRU…
    $ RA          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ Z           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0…
    $ answers     <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ TTLs        <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ rejected    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…

### Шаг 2

#### Сколько участников информационного обмена в сети Доброй Организации?

Соберём все уникальные IP-адреса, встречающиеся в полях источника и
получателя, и посчитаем их количество:

``` r
all_ips <- dns_data %>%
select(id.orig_h, id.resp_h) %>%
unlist(use.names = FALSE) %>%
unique()

total_participants <- length(all_ips)
total_participants
```

    [1] 1359

#### 5. Какое соотношение участников обмена внутри сети и участников обращений к внешним ресурсам?

IP-адреса разделим на приватные и публичные. К приватным относятся
диапазоны:

-   192.168.0.0/16
-   10.0.0.0/8
-   172.16.0.0/12

Подключаем пакет для работы с IP-адресами:

``` r
library(ipaddress)
```

Берём уникальные IP получателей, приводим их к типу `ip_address`, затем
считаем доли внутренних и внешних адресов:

``` r
unique_resp_ips <- dns_data %>%
distinct(id.resp_h) %>%
pull(id.resp_h)

ip_objects <- ip_address(unique_resp_ips)

internal_count <- sum(is_private(ip_objects), na.rm = TRUE)
external_count <- sum(!is_private(ip_objects), na.rm = TRUE)

internal_ratio <- internal_count / (internal_count + external_count)
internal_ratio
```

    [1] 0.9658537

#### Найдите топ-10 участников сети, проявляющих наибольшую сетевую активность.

Объединим всех участников (и источники, и получатели), отфильтруем
пустые значения, посчитаем число появлений и выведем топ-10:

``` r
top10_act <- dns_data %>%
transmute(ip = id.orig_h) %>%
bind_rows(dns_data %>% transmute(ip = id.resp_h)) %>%
filter(!is.na(ip), ip != "-") %>%
count(ip, name = "act") %>%
slice_max(act, n = 10)

top10_act
```

                    ip    act
    1    192.168.207.4 266627
    2    10.10.117.210  75943
    3  192.168.202.255  68720
    4   192.168.202.93  26522
    5     172.19.1.100  25481
    6  192.168.202.103  18121
    7   192.168.202.76  16978
    8   192.168.202.97  16176
    9  192.168.202.141  14976
    10 192.168.202.110  14493

#### Найдите топ-10 доменов, к которым обращаются пользователи сети и соответственное количество обращений

Группируем по домену, считаем запросы и выбираем 10 самых частых:

``` r
top_dmn <- dns_data %>%
filter(!is.na(query), query != "-") %>%
count(query, name = "hits") %>%
arrange(desc(hits)) %>%
head(10)

top_dmn
```

                                                                         query
    1                                                teredo.ipv6.microsoft.com
    2                                                         tools.google.com
    3                                                            www.apple.com
    4                                                           time.apple.com
    5                                          safebrowsing.clients.google.com
    6  *\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00
    7                                                                     WPAD
    8                                              44.206.168.192.in-addr.arpa
    9                                                                 HPE8AA67
    10                                                                  ISATAP
        hits
    1  39273
    2  14057
    3  13390
    4  13109
    5  11658
    6  10401
    7   9134
    8   7248
    9   6929
    10  6569

#### Опеределите базовые статистические характеристики (функция summary() ) интервала времени между последовательными обращениями к топ-10 доменам.

Оставим только топовые домены, отсортируем по времени, вычислим разницу
между соседними обращениями и посмотрим сводную статистику:

``` r
time_int <- dns_data %>%
filter(query %in% top_dmn$query) %>%
arrange(query, ts) %>%
group_by(query) %>%
mutate(time_diff = as.numeric(difftime(ts, lag(ts), units = "secs"))) %>%
ungroup()

summary(time_int$time_diff)
```

         Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
        0.000     0.000     0.750     8.758     1.740 52723.500        10 

#### Поиск возможного скрытого DNS-канала

Идея: если запросы к одному домену идут с почти одинаковым интервалом,
это может выглядеть как «периодический маяк». Посчитаем для доменов
средний интервал, стандартное отклонение и число интервалов, затем
отберём наиболее регулярные:

``` r
periodic_sus <- dns_data %>%
filter(!is.na(query), query != "-") %>%
arrange(query, ts) %>%
group_by(query) %>%
mutate(interval = as.numeric(difftime(ts, lag(ts), units = "secs"))) %>%
summarise(
mean_int = mean(interval, na.rm = TRUE),
sd_int   = sd(interval, na.rm = TRUE),
n        = sum(!is.na(interval))
) %>%
filter(n > 30, sd_int < 1) %>%
arrange(sd_int, desc(n))

periodic_sus
```

    # A tibble: 6 × 4
      query             mean_int  sd_int     n
      <chr>                <dbl>   <dbl> <int>
    1 input.mozilla.com  5.00    0.00475    31
    2 www.hkparts.net    0.00371 0.0124     35
    3 lifehacker.com     0.00612 0.0171     67
    4 hq.h               0.00265 0.0515    377
    5 httphq.hec.net     0.00990 0.100     102
    6 www.h              0.0156  0.125      64

Наиболее подозрительными могут выглядеть домены `hq.h` и
`httphq.hec.net`: по ним наблюдается много обращений с очень близкими
временными интервалами. Также `input.mozilla.com` может давать
регулярные запросы (часто это телеметрия браузера).

------------------------------------------------------------------------

### Шаг 3

#### Определить местоположение и организацию-провайдера для топ-10 доменов

Подключаем пакеты для HTTP-запросов и парсинга JSON:

``` r
library(httr)
library(jsonlite)
```

Опишем функцию, которая делает запрос к API и аккуратно извлекает нужные
поля, после чего применим её к топ-10 доменам:

``` r
get_domain_info <- function(domain) {
api_url <- paste0("http://ip-api.com/json/", domain)
api_response <- httr::GET(api_url)
response_data <- jsonlite::fromJSON(httr::content(api_response, as = "text", encoding = "UTF-8"))

tibble(
domain  = domain,
country = response_data$country,
city    = response_data$city,
isp     = response_data$isp,
org     = response_data$org,
as      = response_data$as
)
}

enriched_data <- purrr::map_df(top_dmn$query, get_domain_info)

enriched_data
```

    # A tibble: 10 × 6
       domain                                        country city  isp   org   as   
       <chr>                                         <chr>   <chr> <chr> <chr> <chr>
     1 "teredo.ipv6.microsoft.com"                   <NA>    <NA>  <NA>  <NA>  <NA> 
     2 "tools.google.com"                            United… Wash… Goog… Goog… AS15…
     3 "www.apple.com"                               United… Seat… Akam… Akam… AS20…
     4 "time.apple.com"                              United… Ashb… Appl… Appl… AS61…
     5 "safebrowsing.clients.google.com"             United… Los … Goog… Goog… AS15…
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\… <NA>    <NA>  <NA>  <NA>  <NA> 
     7 "WPAD"                                        <NA>    <NA>  <NA>  <NA>  <NA> 
     8 "44.206.168.192.in-addr.arpa"                 <NA>    <NA>  <NA>  <NA>  <NA> 
     9 "HPE8AA67"                                    <NA>    <NA>  <NA>  <NA>  <NA> 
    10 "ISATAP"                                      <NA>    <NA>  <NA>  <NA>  <NA> 

------------------------------------------------------------------------

### Шаг 4

Отчёт подготовлен и оформлен в соответствии с требованиями.

## Оценка результатов

Практическая работа выполнена на языке R с использованием пакетов
`dplyr`, `ipaddress`, `httr`, `jsonlite`. В ходе выполнения закреплены
навыки предобработки и анализа DNS-логов, а также основы обогащения
данных через внешнее API.

## Вывод

В рамках работы я отработал базовый анализ DNS-трафика с применением
`dplyr`: определил участников обмена, выделил наиболее активных, получил
топ доменов, оценил интервалы обращений и выполнил обогащение информации
по доменам.
