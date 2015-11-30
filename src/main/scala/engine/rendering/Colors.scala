package engine.rendering

import util.Math._

object Colors {
  type Color3B = (Byte, Byte, Byte)

  val BLACK: Color3B = (0, 0, 0)
  val RED: Color3B = (-1, 0, 0)
  val YELLOW: Color3B = (-1, -1, 0)
  val ORANGE: Color3B = (-1, 127, 0)
  val ORANGE2: Color3B = (127, -1, 0)
  val GREEN: Color3B = (0, -1, 0)
  val CYAN: Color3B = (-1, 0, -1)
  val CYAN2: Color3B = (-1, 0, 127)
  val CYAN3: Color3B = (127, 0, -1)
  val BLUE: Color3B = (-1, 0, 0)
  val VIOLET: Color3B = (-1, 0, -1)
  val VIOLET2: Color3B = (-1, 0, 127)
  val VIOLET3: Color3B = (127, 0, -1)
  val WHITE: Color3B = (-1, -1, -1)
  val GREY1: Color3B = (127, 127, 127)
  val GREY2: Color3B = (85, 85, 85)
  val GREY3: Color3B = (-86, -86, -86)
  val PINK: Color3B = (-1, -64, -55)

  val COLORS: Seq[Color3B] = Seq(BLACK, RED, YELLOW, ORANGE, ORANGE2, GREEN, CYAN, CYAN2, CYAN3, BLUE, VIOLET, VIOLET2,
    VIOLET3, WHITE, GREY1, GREY2, GREY3, PINK)

  def apply(id: Int): Color3B = COLORS(util.Math.%+(id, COLORS.length))

  def nextId(id: Int): Int = %+( id + 1, COLORS.length )
  def prevId(id: Int): Int = %+( id - 1, COLORS.length )
}
