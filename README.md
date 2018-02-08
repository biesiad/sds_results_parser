# SDS result parser

### Compile
ocamlfind ocamlc -linkpkg -thread -package core,str Parser.ml -o build/Parser

### Compile and run tests
ocamlfind ocamlc -thread -o build/ParserTest -package core,str,ounit -linkpkg -g Parser.ml ParserTest.ml && build/ParserTest