build:
	stack build --file-watch

run:
	stack exec pitchskell-exe

ghcid-test:
	ghcid \
		--command "stack ghci pitchskell:lib pitchskell:test:pitchskell-test" \
		--test "main"

