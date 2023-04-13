# Makefile for common dev tasks (code formatting, linting, etc)
# NOT a build script.

.PHONY: clang-format muon iwyu

clang-format:
	fd -e c -e h -X clang-format -i --verbose

muon:
	fd -g meson.build -x muon fmt -i

iwyu:
	iwyu-tool -p build | tee build/iwyu-report.txt
