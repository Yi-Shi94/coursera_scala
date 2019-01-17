package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal{
      b()*b()-4*a()*c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Signal {
      val d = delta()
      d match {
        case c if (c>0) => Set((-b() + d) / (2 * a()), (-b() - d) / (2 * a()))
        case 0 => Set(-b() / 2 * a())
        case _ => Set()
      }
    }
  }
}
