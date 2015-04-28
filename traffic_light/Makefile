CC=erlc
SOURCE=src/*.erl src/utils/*.erl
TEST=test/*.erl
OUTPUT=ebin
.PHONY: test

compile:
	$(CC) -o $(OUTPUT) $(SOURCE)

run:
	erl -noshell -pa $(OUTPUT) -eval "http_server:start(8888)"

clean:
	rm -rf $(OUTPUT)/*.beam

test:
	$(CC) +export_all -o $(OUTPUT) $(SOURCE) $(TEST)
	erl -noshell -pa $(OUTPUT) -eval "eunit:test(number, [verbose])" -s init stop

