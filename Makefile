.PHONY: conv
# Convert tptp problems in ./tptp/mixed to Coq file and save in ./converted/
conv: 
	./conv.sh ./tptp/mixed ./converted

# Remove generated files
.PHONY: clean
clean:
	$(RM) -r ./converted

# Port to tptp2coqp
.PHONY: build install
build:
	$(MAKE) --directory=tptp2coq all

install:
	$(MAKE) --directory=tptp2coq install
