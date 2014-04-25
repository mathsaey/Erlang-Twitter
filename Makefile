# Makefile
# Mathijs Saey
# Multicore Programming Project

#############
# Variables #
#############

INPUT_DIR  = src/
OUTPUT_DIR = src/

# Use if you want your .beam files in a 
# seperate directory (e.g. for a release)
# OUTPUT_DIR = beam/

SRC_FILES  = $(wildcard $(INPUT_DIR)*.erl)
BEAM_FILES = $(foreach file, $(SRC_FILES), $(file:$(INPUT_DIR)%.erl=$(OUTPUT_DIR)%.beam))

COMPILE_INVOCATION = erlc +debug_info -I $(INPUT_DIR) -o $(dir $@)
TEST_INVOCATION = erl -noshell -s $(basename $@) test -s init stop

###########
# Targets #
###########

compile: directory $(BEAM_FILES)

test: compile tweet.test view.test userView.test tweetView.test viewGroup.test

directory:
	@ mkdir -p $(OUTPUT_DIR)

clean:
	- rm $(BEAM_FILES)

.PHONY: clean

################
# Dependencies #
################

$(OUTPUT_DIR)%.beam : $(INPUT_DIR)%.erl
	$(COMPILE_INVOCATION) $<

# Fake targets to run unit tests
%.test : $(OUTPUT_DIR)%.beam
	cd $(OUTPUT_DIR) ; $(TEST_INVOCATION)