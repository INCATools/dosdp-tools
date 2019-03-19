[![Build Status](https://travis-ci.org/INCATools/dosdp-tools.svg?branch=master)](https://travis-ci.org/INCATools/dosdp-tools)

# dosdp-tools

`dosdp-tools` is a command-line utility for working with DOSDP design patterns and OWL ontologies. Given a YAML design pattern following the [DOSDP spec](https://github.com/dosumis/dead_simple_owl_design_patterns), it can either generate ontology axioms from an input table of terms, or instead generate a SPARQL query that can be used to query an ontology for terms matching the pattern.

## Releases
Pre-packaged releases can be downloaded from [here](https://github.com/INCATools/dosdp-tools/releases).

## Building

If you don't already have it, install `sbt` on your system. For Mac OS X, it is easily done using [Homebrew](http://brew.sh): `brew install sbt`

Clone the project from GitHub (`git clone git@github.com:INCATools/dosdp-tools.git`) and then change into the project directory: `cd dosdp-tools`. To compile and build the executable package, run:

`sbt stage`

You will find executables for Unix and Windows in `target/universal/stage/bin/`. These depend on the libraries in `target/universal/stage/lib`.

## Usage

```
Usage

 dosdp-tools [options] command [command options]

Options

   --catalog             : catalog file to use for resolving ontology locations
   --obo-prefixes        : Assume prefixes are OBO ontologies; predefine rdf, rdfs, owl, dc, dct, skos, obo, and oio.
   --ontology            : OWL ontology (provide labels, query axioms)
   --outfile             : Output file (OWL or TSV)
   --prefixes            : CURIE prefixes (YAML)
   --table-format=STRING : Tabular format: TSV (default) or CSV
   --template=STRING     : DOSDP file (YAML). If a local file is not found at the given path, the path will be attempted as a URL.

Commands

   generate [command options] : generate ontology axioms for TSV input to a Dead Simple OWL Design Pattern
      --add-axiom-source-annotation             : Add axiom annotation to generated axioms linking to pattern IRI
      --axiom-source-annotation-property=STRING : IRI for annotation property to use to link generated axioms to pattern IRI
      --generate-defined-class                  : Computed defined class IRI from pattern IRI and variable fillers
      --infile                                  : Input file (TSV or CSV)
      --restrict-axioms-column                  : Data column containing local axiom output restrictions
      --restrict-axioms-to=STRING               : Restrict generated axioms to 'logical', 'annotation', or 'all' (default)

   prototype : output "prototype" axioms using default fillers for a pattern or folder of patterns

   query [command options] : query an ontology for terms matching a Dead Simple OWL Design Pattern
      --print-query : Print generated query without running against ontology
      --reasoner    : Reasoner to use for expanding variable constraints. Valid options are ELK, HermiT, or JFact.

   terms [command options] : dump terms referenced in TSV input and a Dead Simple OWL Design Pattern
      --infile : Input file (TSV or CSV)
```

`dosdp-tools` has four modes: **generate**, **query**, **prototype**, and **terms**:

### Generate

The `generate` command creates an ontology from a DOSDP pattern file and an input file of tab-separated filler values. Column names in the TSV file need to match variables defined in the DOSDP. A special `defined_class` column should provide the IRI for the newly created class instantiating the pattern. A provided ontology is used to find labels for terms, needed to generate annotation values. Example:

```bash
dosdp-tools generate --obo-prefixes=true --template=exposure_to_chemical.yaml --infile=exposure_to_chemical.tsv --outfile=exposure_to_chemical.ofn --ontology=chebi_import.owl
```

### Query

The `query` command queries an ontology for terms that meet the logical patterns defined in a DOSDP pattern file. Instead of performing the query, you can also simply print the generated SPARQL. Example:

```bash
dosdp-tools query --template=entity_attribute_location.yaml --prefixes=prefixes.yaml --print-query
```

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
