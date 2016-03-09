# prime_generator

Генератор простых чисел в заданном диапазоне

## run

    cd prime_generator
    ./rebar deps
    ./rebar compile
    ./run

## Настройка

Диапазон чисел, название структур для хранения данных и параметры для подсоединения к редис хранятся в src/number_generator.app.src

Пример:
  {mod, { number_generator_app, [1000000000000]}},
  {env, [{redis_host, "localhost"},
          {redis_port, 7004},
          {redis_password, "password"},
          {redis_db, 0},
          {queue_key, "number_list"},
          {result_set_key, "number_set"}
        ]}
где
1000000000000 - верхний диапазн
localhost - хост, на которм запущен редис
7004 - порт, который слушает редис
password - пароль для подсоединеия к редису
0 - номер БД редиса
number_list - имя структуры списка
number_set - имя структуры множества