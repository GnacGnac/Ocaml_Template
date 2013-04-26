
MAIN = main
EXE = main

BUILD = ocamlbuild
NAT = $(MAIN).native
BYTE = $(MAIN).byte
DIRS = . src src/utilities src/utilities/BlockML src/utilities/BlockML/Examples
TMPS = $(DIRS:=/*~)

TARGET = $(NAT)


all:
	$(BUILD) $(TARGET)
	cp $(NAT) $(EXE)

clean:
	$(BUILD) -clean

distclean: clean
	rm -rf $(EXE) $(TMPS)


.PHONY: all clean distclean
