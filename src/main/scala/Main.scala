package mincaml
import java.io._

object Main {
  var limit = 1000

   def iter(n:Int, e:KNormal.T):KNormal.T = {
    if (n == 0) {
      e
    } else {
      val e1 = Elim(ConstFold(Inline(Assoc(Beta(e)))))
      if (e == e1) {
        e
      } else {
        iter(n-1, e1)
      }
    }
  }

  def lexbuf(outchan:PrintWriter, inchan:FileReader):Unit = {
    Id.counter = 0
    Typing.extenv = Map()

    Emit(outchan, RegAlloc(Simm(Virtual(Closure(iter(limit, Alpha(KNormal(Typing(Parser(inchan))))))))))
  }

  def file(f:String):Unit = {
    val inchain = new FileReader(f+".ml")
    val outchain = new FileWriter(f+".s")

    try {
      lexbuf(new PrintWriter(new BufferedWriter(outchain)), inchain)
      inchain.close()
      outchain.close()
    } catch {
      case e => inchain.close(); outchain.close(); throw e
    }
  }

  def main(argv:Array[String]):Unit = {

    def arg(l:List[String], rc:List[String]):List[String] = l match {
      case List() => rc
      case "-inline"::x::xs => Inline.threshold = x.toInt; arg(xs, rc)
      case "-iter"::x::xs => limit = x.toInt; arg(xs, rc)
      case x :: xs => arg(xs, x::rc)
    }

    arg(argv.toList, List()) match {
      case List() => println("Usage: mincaml [-inline m] [-iter n] ...filenames without \".ml\"...")
      case files => files.foreach(file)
    }
  }
}
