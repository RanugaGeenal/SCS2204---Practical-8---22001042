object q2{
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Please provide an integer input.")
    } else {
      val input = args(0).toInt

      val categorize: Int => String = {
        case n if n % 3 == 0 && n % 5 == 0 => "Multiple of Both Three and Five"
        case n if n % 3 == 0 => "Multiple of Three"
        case n if n % 5 == 0 => "Multiple of Five"
        case _ => "Not a Multiple of Three or Five"
      }

      println(categorize(input))
    }
  }
}
