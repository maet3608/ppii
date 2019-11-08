package au.edu.imb.db.xml

import scala.util.control.Breaks._
import org.basex.core._
import org.basex.query.QueryProcessor
import org.basex.core.Commands.CmdIndex.PATH 
import org.basex.api.dom.BXText
import proc.{CreateIndex, XQuery, DropDB, List => BXList, InfoDB, Close, Open, CreateDB}

/**
 * Wrapper around the BaseX XML DB.
 * http://www.inf.uni-konstanz.de/dbis/basex/index
 * http://www.inf.uni-konstanz.de/dbis/basex/faq
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 18/08/2010
 */
class BX {
  
}


/**
 * Usage example
 */
object BX extends App {
  println("running...")
  val context = new Context()
  //val namespace = "net:sf:psidev:mi"
  val namespace = ""

  def xquery(querystr:String) {
    val ns = """declare default element namespace "%s"; """ format namespace
    println("Query:\n"+ns+querystr)
    println("\nResults:")
    val processor = new QueryProcessor(ns+querystr, context)
    val iter = processor.iter
    breakable {
      while(true) {
        val node = iter.next
        if(node == null) break
        val value = node.java match {
          case text:BXText => text.asInstanceOf[BXText].getWholeText
          case string:String => string
          case x => x.toString
        }
        println("value: "+value)
      }
    }
    processor.close()
  }

  //new CreateDB("f:/PPII/Data/Idmapping/idmapping_test.xml", "idmapping_test").execute(context)
  //new CreateDB("f:/PPII/Data/Idmapping/idmapping.xml", "idmapping").execute(context)
  //new CreateDB("c:/Maet/Temp/yeast.xml", "yeast").execute(context)
  //new CreateDB("c:/Maet/Temp/herpes.xml", "herpes").execute(context)
  //new CreateDB("c:/Maet/Temp/factbook.xml", "factbook").execute(context)
  //new Open("books").execute(context)
  //new Close().execute(context)

  new Open("idmapping_test").execute(context)
  xquery("for $e in collection('idmapping_test')/map/accession[@value='Q197F7']/id[@name='GI'] " +
              "return $e/@value/string()")
  //xquery("for $e in collection('idmapping_test')/map/accession/id[@value='106073504'] " +
  //           "return ($e/@name/string(), $e/../@value/string())")

  //new Open("yeast").execute(context)
  //xquery("for $e in collection('yeast')/entrySet/entry/interactorList/interactor " +
  //            "return $e/names/shortLabel/string()")
  //xquery("count(for $e in collection('yeast')/entrySet/entry/interactionList/interaction " +
  //            "return $e/@id/string())")

  //new Open("books").execute(context)
  //xquery("for $e in collection('books')/catalog/book return ($e/author/string(), $e/price/text())")

  //new Open("factbook").execute(context)
  //xquery("for $e in collection('factbook')/mondial/country return $e/@name/string()")


  new InfoDB().execute(context, System.out)
  //new DropDB("books1").execute(context)
  //new BXList().execute(context, System.out)

  new Close().execute(context)

  println("finished.")
}