all: petstore.v3.html

petstore.v3.html:
	which redoc-cli || npm install -g redoc-cli
	redoc-cli bundle ./petstore.v3.json -o petstore.v3.html

clean:
	rm petstore.v3.html
