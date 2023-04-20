# Makefile for common dev tasks (code formatting, linting, etc)

CLANG_FORMAT ?= clang-format
IWYU ?= iwyu-tool

.PHONY: clang-format muon iwyu test clean re configure build

build: configure
	meson compile -C build

configure:
	meson setup --reconfigure build || meson setup build

clang-format:
	fd -e c -e h -X $(CLANG_FORMAT) -i --verbose

muon:
	fd -g meson.build -x muon fmt -i

iwyu:
	$(IWYU) -p build | tee build/iwyu-report.txt

test: configure
	meson test -C build -v

clean:
	rm -rf build

re: clean build
