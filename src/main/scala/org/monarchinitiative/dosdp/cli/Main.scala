package org.monarchinitiative.dosdp.cli

import java.io.File
import java.io.FileReader

import org.backuity.clist._
import org.phenoscape.owlet.Owlet
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.yaml.parser
import org.apache.jena.riot.RDFDataMgr

import java.io.FileOutputStream
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.ResultSetFormatter

import scala.collection.JavaConverters._
import org.semanticweb.owlapi.model.OWLOntology
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.system.JenaSystem

object Main extends App {

  JenaSystem.init()
  Cli.parse(args).withProgramName("dosdp-tools").withCommands(Generate, Query).foreach(_.run)

}