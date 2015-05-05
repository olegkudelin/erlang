# erlang
Цифровой Светофор

Сборка
make

Запуск сервиса
make run - порт забит в make файле

Запуск тестов
make test

Проверка работоспособности:

Запрос:
	curl -H "Content-Type: application/json" -X POST -d "{}" http://127.0.0.1:8888/sequence/create
Ответ:
	{'status': 'ok', 'response': {'sequence': '718ec2f0-f023-4b91-bc88-415c60186c82'}}
Запрос:
	curl -H "Content-Type: application/json" -X POST -d "{'observation': {'color': 'green','numbers': ['1110111', '0011101']},'sequence': '718ec2f0-f023-4b91-bc88-415c60186c82'}" http://127.0.0.1:8888/observation/add
Ответ:
	{'status': 'ok', 'response': {'start': [2,8,82,88], 'missing': ['0', '1000000']}}
Запрос:
	curl -H "Content-Type: application/json" -X POST -d "{'observation': {'color': 'green','numbers': ['1110111', '0010000']},'sequence': '718ec2f0-f023-4b91-bc88-415c60186c82'}" http://127.0.0.1:8888/observation/add
Ответ:
	{'status': 'ok', 'response': {'start': [2,82], 'missing': ['0', '1000010']}}
Запрос:
	curl -H "Content-Type: application/json" -X POST -d "{'observation': {'color': 'red','sequence': '718ec2f0-f023-4b91-bc88-415c60186c82'}" http://127.0.0.1:8888/observation/add
Ответ:
	{'status': 'ok', 'response': {'start': [2], 'missing': ['0', '1000010']}}

