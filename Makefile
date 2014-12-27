bin=haskoban
version=0.1

distname=$(bin)-$(version)
compiler=ghc
src=haskoban.hs
levels=./levels/*

PREFIX ?= /usr/local
BIN_DIR ?= $(PREFIX)/bin/
LEVEL_DIR ?= $(PREFIX)/share/$(bin)/levels/
MAN_DIR ?= $(PREFIX)/share/man/
FLAGS += -cpp -D'LEVEL_DIR="$(LEVEL_DIR)"'


all: $(bin)


$(bin):
	$(compiler) $(FLAGS) $(src) -o $@


install: all
	install -d $(BIN_DIR)
	install $(bin) $(BIN_DIR)
	install -d $(LEVEL_DIR)
	install -m644 $(levels) $(LEVEL_DIR)
	install -d $(MAN_DIR)/man1
	install haskoban.1 $(MAN_DIR)/man1


clean:
	rm -f *.o *.hi haskoban
