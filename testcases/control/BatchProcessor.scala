
import daisy.lang._
import Real._


object BatchProcessor {

  // s1, s2, s3, s4, y1, y2: <1, 16, 11>
  def out1(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10)
    (-0.0429) * s1 + (-0.9170) * s2 + (-0.3299) * s3 + (-0.8206) * s4
  }

  def out2(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10)
    2.4908 * s1 + 0.0751 * s2 + 1.7481 * s3 + (-1.1433) * s4
  }

  def state1(s1: Real, s2: Real, s3: Real, s4: Real, y1: Real) = {
    require(-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10 &&
      -10 <= y1 && y1 <= 10)
    0.9670 * s1 + (-0.0019) * s2 + 0.0187 * s3 + (-0.0088) * s4 + 0.0447 * y1
  }

  def state2(s1: Real, s2: Real, s3: Real, s4: Real, y1: Real, y2: Real) = {
    require(-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10 &&
      -10 <= y1 && y1 <= 10 && -10 <= y2 && y2 <= 10)
    (-0.0078) * s1 + 0.9052 * s2 + (-0.0181) * s3 + (-0.0392) * s4 + (-0.0003) * y1 + 0.0020 * y2
    //((0.9052 * s2) + (((s3 * -0.0181) + (-0.0078 * s1)) + (((-0.0392 * s4) + (-0.0003 * y1)) + (0.002 * y2))))
  }

  def state3(s1: Real, s2: Real, s3: Real, s4: Real, y1: Real, y2: Real) = {
    require(-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10 &&
      -10 <= y1 && y1 <= 10 && -10 <= y2 && y2 <= 10)
    (-0.0830) * s1 + 0.0222 * s2 + 0.8620 * s3 + 0.0978 * s4 + 0.0170 * y1 + 0.0058 * y2
  }

  def state4(s1: Real, s2: Real, s3: Real, s4: Real, y1: Real, y2: Real) = {
    require(-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10 &&
      -10 <= y1 && y1 <= 10 && -10 <= y2 && y2 <= 10)
    (-0.0133) * s1 + 0.0243 * s2 + (-0.0043) * s3 + 0.9824 * s4 + 0.0127 * y1 + 0.0059 * y2
  }
}