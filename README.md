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
   --prefixes    : CURIE prefixes (YAML)
```

Example: `dosdp-scala --template=entity_attribute_location.yaml --prefixes=prefixes.yaml --print-query`

Output:
```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT DISTINCT ?defined_class (STR(?defined_class__label) AS ?defined_class_label) ?attribute (STR(?attribute__label) AS ?attribute_label) ?entity (STR(?entity__label) AS ?entity_label) ?location (STR(?location__label) AS ?location_label)
WHERE {
?defined_class owl:equivalentClass ?ba95b4a7b13f4a86af50c3e0e1182838 .
?ba95b4a7b13f4a86af50c3e0e1182838 owl:intersectionOf/rdf:rest*/rdf:first ?c90d72646ac64cc19318ca3273f11cd6 .
?ba95b4a7b13f4a86af50c3e0e1182838 owl:intersectionOf/rdf:rest*/rdf:first ?attribute .
?ba95b4a7b13f4a86af50c3e0e1182838 owl:intersectionOf/rdf:rest/rdf:rest rdf:nil .
?c90d72646ac64cc19318ca3273f11cd6 owl:onProperty <http://purl.obolibrary.org/obo/RO_0000052> .
?c90d72646ac64cc19318ca3273f11cd6 owl:someValuesFrom ?7f7e348c174d42f0a76663cc59f21e37 .
?7f7e348c174d42f0a76663cc59f21e37 owl:intersectionOf/rdf:rest*/rdf:first ?75f4eb2eae374a77878c36acda870206 .
?7f7e348c174d42f0a76663cc59f21e37 owl:intersectionOf/rdf:rest*/rdf:first ?entity .
?7f7e348c174d42f0a76663cc59f21e37 owl:intersectionOf/rdf:rest/rdf:rest rdf:nil .
?75f4eb2eae374a77878c36acda870206 owl:onProperty <http://purl.obolibrary.org/obo/BFO_0000050> .
?75f4eb2eae374a77878c36acda870206 owl:someValuesFrom ?location .
FILTER(?entity != ?75f4eb2eae374a77878c36acda870206)
FILTER(?c90d72646ac64cc19318ca3273f11cd6 != ?attribute)
OPTIONAL { ?defined_class rdfs:label ?defined_class__label . }
OPTIONAL { ?attribute rdfs:label ?attribute__label . }
OPTIONAL { ?entity rdfs:label ?entity__label . }
OPTIONAL { ?location rdfs:label ?location__label . }
}
```
