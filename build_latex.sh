#!/usr/bin/env bash

for file in examples/*.tex; do
  echo "Compile $file"
  pdflatex -halt-on-error -output-directory examples "$file" >/dev/null
done

rm examples/*.aux examples/*.log
echo "Done"
