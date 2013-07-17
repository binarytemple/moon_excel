object Main {
  def main(args: Array[String]) {


    import moon._
    import Model._
    import Viewer._
    val m = new Model

    var out = ""
    for {i <- Range(0,Cols) }
//      out += "\n"

       yield for( j <- m.data(i).zipWithIndex.map(_.swap) ) {

      out += (if(j._1 == 0) "\n" + j._2 else j._2)
    }

    println(out)


  }
}
