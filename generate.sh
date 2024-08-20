#!/bin/bash -e

# If converted exists, rename it with timestamp.
if [ -d converted ]; then
  name=converted_$(date +%Y%m%d%H%M%S)
  mv converted $name
  echo "Renamed converted/ to $name/"
fi

# Convert tptp/mixed to converted
./conv.sh tptp/mixed converted

# Remove empty files
./remove_empties.sh ./converted

# Convert SMT.v files
./smtconv.sh
