[![Build Status](https://travis-ci.org/balhoff/dosdp-scala.svg?branch=master)](https://travis-ci.org/balhoff/dosdp-scala)

# dosdp-scala

Given a YAML design pattern following the [DOSDP spec](https://github.com/dosumis/dead_simple_owl_design_patterns), generate a SPARQL query that will return all slot fillers for that pattern

For context, see:
https://github.com/dosumis/dead_simple_owl_design_patterns/issues/9

## Releases
dosdp-scala is still somewhat experimental, but pre-packaged releases can be downloaded from [here](https://github.com/balhoff/dosdp-scala/releases).

## Building

Install `sbt` on your system. For Mac OS X, it is easily done using [Homebrew](http://brew.sh): `brew install sbt`

To compile and build the executable package, run:

`sbt stage`

You will find executables for Unix and Windows in `target/universal/stage/bin/`. These depend on the libraries in `target/universal/stage/lib`.

## Usage

```
Usage

 dosdp-scala [options] : query an ontology for terms matching a Dead Simple OWL Design Pattern

Options

   --ontology    : OWL ontology to query
   --print-query : Print generated query without running against ontology
   --reasoner    : Reasoner to use for expanding variable constraints (currently only valid option is `elk`)
   --template    : DOSDP file (YAML)
```

Example: `dosdp-scala --template=entity_attribute_location.yaml --print-query`

Output:
```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT DISTINCT ?attribute ?entity ?entity_attribute_location ?location
WHERE {
?entity_attribute_location owl:equivalentClass ?935f1722f77e4a5d83a952bd05468061 .
?935f1722f77e4a5d83a952bd05468061 owl:intersectionOf/rdf:rest*/rdf:first ?589c79cc99244f42aca7ca788c4c5de7 .
?935f1722f77e4a5d83a952bd05468061 owl:intersectionOf/rdf:rest*/rdf:first ?attribute .
?935f1722f77e4a5d83a952bd05468061 owl:intersectionOf/rdf:rest/rdf:rest rdf:nil .
?589c79cc99244f42aca7ca788c4c5de7 owl:onProperty <http://purl.obolibrary.org/obo/RO_0000052> .
?589c79cc99244f42aca7ca788c4c5de7 owl:someValuesFrom ?c3ce064ef0e54bb580fd6e78c53646b2 .
?c3ce064ef0e54bb580fd6e78c53646b2 owl:intersectionOf/rdf:rest*/rdf:first ?entity .
?c3ce064ef0e54bb580fd6e78c53646b2 owl:intersectionOf/rdf:rest*/rdf:first ?1336ea47488043ecb3149ada5d1102ae .
?c3ce064ef0e54bb580fd6e78c53646b2 owl:intersectionOf/rdf:rest/rdf:rest rdf:nil .
?1336ea47488043ecb3149ada5d1102ae owl:onProperty <http://purl.obolibrary.org/obo/BFO_0000050> .
?1336ea47488043ecb3149ada5d1102ae owl:someValuesFrom ?location .
FILTER(?entity != ?1336ea47488043ecb3149ada5d1102ae)
FILTER(?attribute != ?589c79cc99244f42aca7ca788c4c5de7)
}
```