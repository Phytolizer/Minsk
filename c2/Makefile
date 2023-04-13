# Makefile for common dev tasks (code formatting, linting, etc)
# NOT a build script.

CLANG_FORMAT ?= clang-format
IWYU ?= iwyu-tool

.PHONY: clang-format muon iwyu

clang-format:
	fd -e c -e h -X $(CLANG_FORMAT) -i --verbose

muon:
	fd -g meson.build -x muon fmt -i

iwyu:
	$(IWYU) -p build | tee build/iwyu-report.txt
