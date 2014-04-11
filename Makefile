# Makefile
# Mathijs Saey
# Multicore Programming Project

#############
# Variables #
#############

INPUT_DIR  = src/
OUTPUT_DIR = beam/

SRC_FILES  = $(wildcard $(INPUT_DIR)*.erl)
BEAM_FILES = $(foreach file, $(SRC_FILES), $(file:$(INPUT_DIR)%.erl=$(OUTPUT_DIR)%.beam))
BEAM_DIRS  = $(sort $(dir $(BEAM_FILES)))

COMPILE_INVOCATION = erlc -I $(INPUT_DIR) -o $(dir $@)

###########
# Targets #
###########

all: directory $(BEAM_FILES)

directory:
	@ mkdir -p $(BEAM_DIRS)

clean:
	- rm $(BEAM_FILES)
	- rm -r $(BEAM_DIRS)

.PHONY: clean

################
# Dependencies #
################

$(OUTPUT_DIR)%.beam : $(INPUT_DIR)%.erl
	$(COMPILE_INVOCATION) $<
