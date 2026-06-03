package presifeur.web

import org.scalajs.dom
import org.scalajs.dom.{document, html}

def div(cls: String): html.Div =
  val el = document.createElement("div").asInstanceOf[html.Div]
  el.className = cls
  el

def h1(text: String): html.Heading =
  val el = document.createElement("h1").asInstanceOf[html.Heading]
  el.textContent = text
  el

def h2(text: String): html.Heading =
  val el = document.createElement("h2").asInstanceOf[html.Heading]
  el.textContent = text
  el

def p(text: String): html.Paragraph =
  val el = document.createElement("p").asInstanceOf[html.Paragraph]
  el.textContent = text
  el

def button(text: String, classes: List[String] = Nil): html.Button =
  val el = document.createElement("button").asInstanceOf[html.Button]
  el.textContent = text
  el.className = classes.mkString(" ")
  el

def inputText(value: String): html.Input =
  val el = document.createElement("input").asInstanceOf[html.Input]
  el.value = value
  el

def actionRow(buttons: html.Button*): html.Div =
  val row = div("actions")
  buttons.foreach(row.appendChild)
  row
