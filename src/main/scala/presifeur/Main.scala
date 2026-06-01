package presifeur

import presifeur.web.BrowserApp

object Main:
  def main(args: Array[String]): Unit =
    BrowserApp.mount()
