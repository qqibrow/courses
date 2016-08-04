package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for(y <- from until end) {
      for( x <- 0 until src.width) {
        dst.update(x, y, boxBlurKernel(src, x, y, radius))
      }
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val spans = cutInterval(src.height, numTasks)
    val tasks = spans.map { case (start, end) =>
      task { blur(src, dst, start, end, radius)}
    }
    tasks.foreach(_.join())
  }

  /** Divide the interval (0, end] to parts spans, each span is a (Int, Int) tuple represents [start, end) of the span.
    *
    * @param end
    * @param parts
    * @return
    */

  def cutInterval(end: Int, parts: Int):List[(Int, Int)] = {
    val reminder = end % parts
    val denominator = end / parts
    val initListWithEqualSteps = List.fill(parts)(denominator)
    val finalSteps = initListWithEqualSteps.take(reminder).map(_ + 1) ::: initListWithEqualSteps.drop(reminder)
    val allSpanEnds = accumulatedSum(finalSteps)
    val allSpanStarts = List(0) ::: allSpanEnds.init
    val spans = allSpanStarts.zip(allSpanEnds)
    spans
  }

  def accumulatedSum(list : List[Int]): List[Int] = {
    list.foldLeft((0, List[Int]()))
    {(acu,i)=>(i+acu._1, i+acu._1 :: acu._2)}._2.reverse
  }
}
