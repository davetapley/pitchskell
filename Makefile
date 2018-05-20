ghcid-test:
	ghcid \
		--command "stack ghci pitchskell:lib pitchskell:test:pitchskell-test" \
		--test "main"

