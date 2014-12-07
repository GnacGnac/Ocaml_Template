
MAIN = main
EXE = my_prog

BUILD = ocamlbuild
NAT = $(MAIN).native
BYTE = $(MAIN).byte
JS = $(MAIN).js
HTML = $(MAIN).html
ML = src/$(MAIN).ml
DIRS = . src src/utilities src/utilities/BlockML	\
	src/utilities/BlockML/Examples lib Examples Drivers
TMPS = $(DIRS:=/*~)

TARGET = $(NAT)

BIN_DIR ?= /usr/local/bin
LIB_DIR ?= /usr/local/lib/$(EXE)
LIB = grammar.blg
IMPORT = src/import.ml


all: local

local:
	@ echo "let lib = \"$(LIB_DIR)/$(LIB)\"" > $(IMPORT)
	@ echo "let () = Main_local.run ()" > $(ML)
	$(BUILD) $(TARGET)
	cp $(TARGET) $(EXE)

js:
	@ echo "let lib = \"$(LIB_DIR)/$(LIB)\"" > $(IMPORT)
	@ echo "let () = Main_js.run ()" > $(ML)
	$(BUILD) -use-ocamlfind -package js_of_ocaml	\
		-package js_of_ocaml.syntax -syntax camlp4o $(BYTE)
	js_of_ocaml $(BYTE)

html: js
	@ echo "<html><head><script>" > $(HTML)
	@ cat Drivers/javascript_missing_primitives >> $(HTML)
	@ cat $(JS) >> $(HTML)
	@ echo "</script></head></html>" >> $(HTML)

clean:
	$(BUILD) -clean

distclean: clean
	rm -rf $(BYTE) $(NAT) $(EXE) $(TMPS) $(JS) $(HTML) $(ML)

install: all
	mkdir -p $(LIB_DIR)
	cp lib/$(LIB) $(LIB_DIR)
	cp $(EXE) $(BIN_DIR)

uninstall:
	rm -rf $(LIB_DIR)
	rm -f $(BIN_DIR)/$(EXE)


.PHONY: all clean distclean install uninstall
