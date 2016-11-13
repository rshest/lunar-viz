default:
	elm-make src/Main.elm --yes --output gen/main.js

test:
	elm-make test/TestMain.elm --yes --output=gen/test.js

setup:
	npm install