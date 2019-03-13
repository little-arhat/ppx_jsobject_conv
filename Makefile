NAME := ppx_jsobject_conv
PREFIX = $(shell opam config var prefix)
RUNNER := dune

build:
	$(RUNNER) build

test:
	$(RUNNER) build @runtest

$(NAME).install:
	$(MAKE) build

clean:
	$(RUNNER) clean
	rm -f $(NAME).install
	rm -f $(NAME)-tests.install

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
