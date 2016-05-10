NAME := ppx_jsobject_conv
PREFIX = $(shell opam config var prefix)

build:
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=false native-dynlink=false


test: build
	rm -rf _build/src_test/
	ocamlbuild -j 0 -use-ocamlfind -classic-display \
						 src_test/test.byte
	js_of_ocaml --noinline --opt=1 ./_build/src_test/test.byte -o _build/src_test/test.js
	ln -s _build/src_test/test.js test.js
	node test.js

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
