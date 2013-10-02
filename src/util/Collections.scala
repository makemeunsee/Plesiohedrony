package util

object Collections {

  def successivePairsCycling[A](list: List[A]): List[(A, A)] = {
    def successivePairsCyclingRec(rem: List[A], first: A): List[(A, A)] =
      rem match {
        case List(last) => List((last, first))
        case h1 :: h2 :: t => {
          (h1, h2) :: successivePairsCyclingRec(h2 :: t, first)
        }
      }
    list match {
      case List()  => List()
      case List(_) => List()
      case h1 :: h2 :: t => {
        (h1, h2) :: successivePairsCyclingRec(h2 :: t, h1)
      }
    }
  }
}