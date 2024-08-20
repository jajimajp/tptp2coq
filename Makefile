.PHONY: conv
# Convert tptp problems in ./tptp/mixed to Coq file and save in ./converted/
conv: 
	./conv.sh ./tptp/mixed ./converted

# Remove generated files
.PHONY: clean
clean:
	$(RM) -r ./converted
