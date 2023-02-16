.PHONY: ghcid

# install_ghcid:
# 	@if ! stack exec which ghcid > /dev/null; then stack install ghcid; fi

ghcid:
	stack exec ghcid -- \
		--command="stack ghci  --ghci-options='-j -fno-write-ide-info -fobject-code' ray-tracer-challenge:lib ray-tracer-challenge:exe:ray-tracer ray-tracer-challenge:test:spec --main-is ray-tracer-challenge:test:spec" \
		--test="Main.main" \
		--warnings
