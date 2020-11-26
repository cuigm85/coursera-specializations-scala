package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      b() * b() - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) Set[Double]()
      else if (delta() == 0) Set( -0.5 * b() / a())
      else Set( -0.5 * (b() + math.sqrt(delta())) / a(), -0.5 * (b() - math.sqrt(delta())) / a() )
    }
  }
}
