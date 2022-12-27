.PHONY: ghcid ghcid-test

ghcid:
	ghcid \
		--command="stack ghci ray-tracer-challenge --ghci-options=-fno-code"

ghcid-test:
		ghcid \
			--command="stack ghci ray-tracer-challenge:lib ray-tracer-challenge:test:ray-tracer-challenge-test --ghci-options=-fobject-code" \
			--test="main" \
			--warnings
