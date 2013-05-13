
MAIN = main
EXE = my_prog

BUILD = ocamlbuild
NAT = $(MAIN).native
BYTE = $(MAIN).byte
DIRS = . src src/utilities src/utilities/BlockML	\
	src/utilities/BlockML/Examples lib
TMPS = $(DIRS:=/*~)

TARGET = $(NAT)

BIN_DIR ?= /usr/local/bin
LIB_DIR ?= /usr/local/lib/$(EXE)
LIB = grammar.blg
IMPORT = src/import.ml


all:
	@ echo "let lib = \"$(LIB_DIR)/$(LIB)\"" > $(IMPORT)
	$(BUILD) $(TARGET)
	cp $(NAT) $(EXE)

clean:
	$(BUILD) -clean

distclean: clean
	rm -rf $(EXE) $(TMPS)

install: all
	mkdir -p $(LIB_DIR)
	cp lib/$(LIB) $(LIB_DIR)
	cp $(EXE) $(BIN_DIR)

uninstall:
	rm -rf $(LIB_DIR)
	rm -f $(BIN_DIR)/$(EXE)


.PHONY: all clean distclean install uninstall
