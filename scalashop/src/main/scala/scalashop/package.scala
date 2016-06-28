

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }


  // First version of method implemenation

    /** Computes the blurred RGBA value of a single pixel of the input image. */
    def boxBlurKernel1(src: Img, x: Int, y: Int, radius: Int): RGBA = {

      // TODO I should implement it in more functional and concise way :-)

      var redSet = Set[RGBA]()
      var greenSet = Set[RGBA]()
      var blueSet = Set[RGBA]()
      var alphaSet = Set[RGBA]()

      var startY = y - radius
      while (startY <= y + radius) {
        var startX = x - radius
        while (startX <= x + radius) {
          val pixelY = clamp(startY, 0, src.height - 1)
          val pixelX = clamp(startX, 0, src.width - 1)

          redSet += red(src(pixelX, pixelY))
          greenSet += green(src(pixelX, pixelY))
          blueSet += blue(src(pixelX, pixelY))
          alphaSet += alpha(src(pixelX, pixelY))

          startX = startX + 1
        }
        startY = startY + 1
      }

      rgba(
        redSet.seq.sum / redSet.size,
        greenSet.seq.sum / greenSet.size,
        blueSet.seq.sum / blueSet.size,
        alphaSet.seq.sum / alphaSet.size
      )
    }

  // Second version more functional :-)

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {

    val tuples = (-radius to radius)
      .flatMap(i => (-radius to radius).map(j => (clamp(x + i, 0, src.width - 1), clamp(y + j, 0, src.height - 1))))
      .distinct
      .map({
        case (x, y) =>
          val pixel = src(x, y)
          (red(pixel), green(pixel), blue(pixel), alpha(pixel))
      })

    rgba(
      tuples.map(_._1).sum / tuples.length,
      tuples.map(_._2).sum / tuples.length,
      tuples.map(_._3).sum / tuples.length,
      tuples.map(_._4).sum / tuples.length
    )
  }

}
