ELM_SOURCE_DIR = src
ELM_SOURCES = \
	$(wildcard $(ELM_SOURCE_DIR)/*.elm) \
	$(wildcard $(ELM_SOURCE_DIR)/*/*.elm)
ELM_OUTPUT = elm.js


.PHONY: all clean distclean format

all: $(ELM_OUTPUT)

clean:
	rm -f $(ELM_OUTPUT)

distclean: clean
	rm -rf elm-stuff

format:
	elm-format --yes $(ELM_SOURCES)

$(ELM_OUTPUT): $(ELM_SOURCES)
	elm-make --output $@ $(ELM_SOURCE_DIR)/Main.elm
