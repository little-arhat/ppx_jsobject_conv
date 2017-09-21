NAME := ppx_jsobject_conv
PREFIX = $(shell opam config var prefix)
TEST_CMD := node

build:
	opam config subst pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true

test: build
	rm -rf _build/src_test/
	ocaml pkg/build_tests.ml native=false native-dynlink=false
	js_of_ocaml $(JSOO_OPTS) +js_of_ocaml/weak.js test.byte -o test.js
	$(TEST_CMD) test.js

$(NAME).install:
	$(MAKE) build

clean:
	ocamlbuild -clean
	rm -f $(NAME).install
	rm -f pkg/META
	rm -f test.js

install: $(NAME).install
	opam-installer -i --prefix $(PREFIX) $(NAME).install

uninstall: $(NAME).install
	opam-installer -u --prefix $(PREFIX) $(NAME).install

reinstall: $(NAME).install
	opam-installer -u --prefix $(PREFIX) $(NAME).install &> /dev/null || true
	opam-installer -i --prefix $(PREFIX) $(NAME).install

.PHONY: build driver test clean


VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push origin v$(VERSION)
# opam publish prepare $(NAME_VERSION) $(ARCHIVE)
# opam publish submit $(NAME_VERSION)
# rm -rf $(NAME_VERSION)

.PHONY: release
