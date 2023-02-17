.PHONY: ghcid_test

# install_ghcid:
# 	@if ! stack exec which ghcid > /dev/null; then stack install ghcid; fi

format:
	find * -name '*.hs' | xargs -P0 fourmolu --no-cabal -i

ghcid:
	stack exec ghcid -- \
		--command="stack ghci --ghc-options='-j -fno-write-ide-info -fno-code' ray-tracer-challenge:lib ray-tracer-challenge:exe:ray-tracer" \

ghcid_test:
	stack exec ghcid -- \
		--command="stack ghci --ghc-options='-j -fno-write-ide-info' ray-tracer-challenge:lib ray-tracer-challenge:test:spec" \
		--test="main"
