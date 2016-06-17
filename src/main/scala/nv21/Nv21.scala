package nv21

/**
  * Created by HanHan on 2016/6/17.
  */
package nv21

/**
  * Created by HanHan on 2016/6/17.
  */
object Test{

  implicit def clamp(x: Double): Short = Math.round(0.0 max x min 255).toShort


  def nv21_to_rgb(src: Array[Short], width: Int): Array[Short] = {
    assert(width % 2 == 0)
    assert(src.length % (width * 3) == 0)
    val height = src.length/width/3*2
    val size = width * height

    def to_rgb(y: Short, u: Short, v: Short): Array[Short] = {
      val r = y + 1.402 * (v - 128)
      val g = y - 0.344 * (u - 128) - 0.714 * (v - 128)
      val b = y + 1.772 * (u - 128)
      Array[Short](r, g, b)
    }
    val result = for (i <- 0 until size) yield {
      val x = i % width
      val y = i / width
      to_rgb(src(y * width + x), src((height + y-y%2) * width +(x-x%2+1)), src((height + y-y%2) * width + (x-x%2)))
    }
    result.flatten.toArray
  }

  def main(args: Array[String]) = {
    assert(nv21_to_rgb(Array.fill[Short](6)(0), 2) sameElements Array[Short](0, 135, 0, 0, 135, 0, 0, 135, 0, 0, 135, 0))
    assert(nv21_to_rgb(Array[Short](126, 127, 128, 129, 130, 131), 2) sameElements Array[Short](129, 124, 131, 130, 125, 132, 131, 126, 133, 132, 127, 134))
    assert(nv21_to_rgb(Array[Short](30, 60, 90, 120, 150, 250), 2) sameElements Array[Short](61, 0, 246, 91, 2, 255, 121, 32, 255, 151, 62, 255))

  }
}
