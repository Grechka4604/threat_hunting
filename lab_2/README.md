# Практическая работа №2
sokol46532@yandex.ru

## Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Развить практические навыки использования функций обработки данных
    пакета dplyr – функции select(), filter(), mutate(), arrange(),
    group_by()

## Исходные данные

1.  Программное обеспечение Microsoft Windows 11 Pro
2.  RStudio Desktop
3.  Интерпретатор языка R 4.5.2

## Задание

Проанализировать встроенный в пакет dplyr набор данных starwars с
помощью языка R и ответить на вопросы

## Ход работы

1.  Проанализировать встроенный в пакет dplyr набор данных starwars с
    помощью языка R и ответить на вопросы:
    -   Сколько строк в датафрейме?
    -   Сколько столбцов в датафрейме?
    -   Как просмотреть примерный вид датафрейма?
    -   Сколько уникальных рас персонажей (species) представлено в
        данных?
    -   Найти самого высокого персонажа.
    -   Найти всех персонажей ниже 170
    -   Подсчитать ИМТ (индекс массы тела) для всех персонажей.
    -   Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по
        отношению массы (mass) к росту (height) персонажей
    -   Найти средний возраст персонажей каждой расы вселенной Звездных
        войн
    -   Найти самый распространенный цвет глаз персонажей вселенной
        Звездных войн.
    -   Подсчитать среднюю длину имени в каждой расе вселенной Звездных
        войн.
2.  Оценить результаты и сделать вывод

## Шаги:

### Шаг №1

Установка пакета dplyr и загрузка набора данных starwars

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

``` r
starwars_data <- starwars
```

Сколько строк в датафрейме?

``` r
nrow(starwars_data)
```

    [1] 87

Сколько столбцов в датафрейме?

``` r
ncol(starwars_data)
```

    [1] 14

Как просмотреть примерный вид датафрейма?

``` r
glimpse(starwars_data)
```

    Rows: 87
    Columns: 14
    $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
    $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
    $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
    $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
    $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
    $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
    $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
    $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
    $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
    $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
    $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
    $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
    $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
    $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…

Сколько уникальных рас персонажей (species) представлено в данных?

``` r
starwars_data %>% pull(species) %>% unique() %>% length()
```

    [1] 38

Найти самого высокого персонажа.

``` r
starwars_data %>% filter(!is.na(height)) %>% slice_max(height, n = 1, with_ties = FALSE) %>% pull(name)
```

    [1] "Yarael Poof"

Найти всех персонажей ниже 170

    > starwars_data %>%
    +     filter(!is.na(height), height < 170) %>%
    +     select(name, height)
    # A tibble: 22 × 2
       name                  height
       <chr>                  <int>
     1 C-3PO                    167
     2 R2-D2                     96
     3 Leia Organa              150
     4 Beru Whitesun Lars       165
     5 R5-D4                     97
     6 Yoda                      66
     7 Mon Mothma               150
     8 Wicket Systri Warrick     88
     9 Nien Nunb                160
    10 Watto                    137
    # ℹ 12 more rows
    # ℹ Use `print(n = ...)` to see more rows

Подсчитать ИМТ (индекс массы тела) для всех персонажей.

``` r
starwars_data %>%
pull(species) %>%
unique() %>%
length()
```

    [1] 38

``` r
starwars_data %>% filter(!is.na(height)) %>% slice_max(height, n = 1, with_ties = FALSE) %>% pull(name)
```

    [1] "Yarael Poof"

``` r
starwars_data %>%
filter(!is.na(height), height < 170) %>%
select(name, height)
```

    # A tibble: 22 × 2
       name                  height
       <chr>                  <int>
     1 C-3PO                    167
     2 R2-D2                     96
     3 Leia Organa              150
     4 Beru Whitesun Lars       165
     5 R5-D4                     97
     6 Yoda                      66
     7 Mon Mothma               150
     8 Wicket Systri Warrick     88
     9 Nien Nunb                160
    10 Watto                    137
    # ℹ 12 more rows

``` r
starwars_data %>%
mutate(
bmi = mass / ( (height / 100)^2 )
) %>%
select(name, mass, height, bmi)
```

    # A tibble: 87 × 4
       name                mass height   bmi
       <chr>              <dbl>  <int> <dbl>
     1 Luke Skywalker        77    172  26.0
     2 C-3PO                 75    167  26.9
     3 R2-D2                 32     96  34.7
     4 Darth Vader          136    202  33.3
     5 Leia Organa           49    150  21.8
     6 Owen Lars            120    178  37.9
     7 Beru Whitesun Lars    75    165  27.5
     8 R5-D4                 32     97  34.0
     9 Biggs Darklighter     84    183  25.1
    10 Obi-Wan Kenobi        77    182  23.2
    # ℹ 77 more rows

Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по
отношению массы (mass) к росту (height) персонажей

``` r
starwars_data %>%
mutate(stretch_ratio = mass / height) %>%
filter(!is.na(stretch_ratio)) %>%
arrange(desc(stretch_ratio)) %>%
slice_head(n = 10) %>%
select(name, mass, height, stretch_ratio)
```

    # A tibble: 10 × 4
       name                   mass height stretch_ratio
       <chr>                 <dbl>  <int>         <dbl>
     1 Jabba Desilijic Tiure  1358    175         7.76 
     2 Grievous                159    216         0.736
     3 IG-88                   140    200         0.7  
     4 Owen Lars               120    178         0.674
     5 Darth Vader             136    202         0.673
     6 Jek Tono Porkins        110    180         0.611
     7 Bossk                   113    190         0.595
     8 Tarfful                 136    234         0.581
     9 Dexter Jettster         102    198         0.515
    10 Chewbacca               112    228         0.491

Найти средний возраст персонажей каждой расы вселенной Звездных войн

``` r
starwars_data %>%
mutate(age = 100 + birth_year) %>%
filter(!is.na(age), !is.na(species)) %>%
group_by(species) %>%
summarise(
average_age = mean(age, na.rm = TRUE),
count = n()
) %>%
arrange(desc(average_age))
```

    # A tibble: 15 × 3
       species        average_age count
       <chr>                <dbl> <int>
     1 Yoda's species        996      1
     2 Hutt                  700      1
     3 Wookiee               300      1
     4 Cerean                192      1
     5 Zabrak                154      1
     6 Human                 154.    26
     7 Droid                 153.     3
     8 Trandoshan            153      1
     9 Gungan                152      1
    10 Mirialan              149      2
    11 Twi'lek               148      1
    12 Rodian                144      1
    13 Mon Calamari          141      1
    14 Kel Dor               122      1
    15 Ewok                  108      1

Найти самый распространенный цвет глаз персонажей вселенной Звездных
войн.

``` r
starwars_data %>%
filter(!is.na(eye_color)) %>%
count(eye_color, sort = TRUE) %>%
slice_max(n, n = 1) %>%
pull(eye_color)
```

    [1] "brown"

Подсчитать среднюю длину имени в каждой расе вселенной Звездных войн.

``` r
starwars_data %>%
mutate(name_length = nchar(name)) %>%
filter(!is.na(species)) %>%
group_by(species) %>%
summarise(
avg_name_length = mean(name_length, na.rm = TRUE),
count = n()
) %>%
arrange(desc(avg_name_length))
```

    # A tibble: 37 × 3
       species   avg_name_length count
       <chr>               <dbl> <int>
     1 Ewok                 21       1
     2 Hutt                 21       1
     3 Geonosian            17       1
     4 Besalisk             15       1
     5 Mirialan             14       2
     6 Toong                14       1
     7 Aleena               12       1
     8 Cerean               12       1
     9 Gungan               11.7     3
    10 Human                11.3    35
    # ℹ 27 more rows

## Шаг №2

### Оценка результатов

В ходе выполнения практической работы были отработаны навыки
использования языка R для анализа и обработки табличных данных, а также
изучены особенности набора данных starwars.

### Вывод

В результате работы мы закрепили применение базовых функций пакета dplyr
для обработки данных: select(), filter(), mutate(), arrange(),
group_by(), и научились использовать их на реальном примере.
