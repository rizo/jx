
.PHONY: test
test:
	dune build --profile=release @runtest

.PHONY: test-watch
test-watch:
	dune build -w --profile=release @runtest

.PHONY: watch
watch:
	dune build -w --profile=release @runtest @all @doc

.PHONY: promote
promote:
	cat _build/default/js_ffi/tests/index.bc.js > js_ffi/tests/index.expected.bc.js
