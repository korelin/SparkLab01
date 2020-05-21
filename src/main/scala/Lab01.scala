import scala.io.Source
import io.circe.syntax._
import io.circe.generic.auto._
import java.io.PrintWriter

case class Histogram(hist_film: Seq[Int], hist_all: Seq[Int])

object Lab01 extends App{
  def orderBy(x:Map[Int,Int]) = x.toSeq.sortBy(_._1).map(_._2)
  val data = Source.fromFile("u.data")
  val records = (for (line <- data.getLines) yield (line.split("\t")(1) toInt, line.split("\t")(2) toInt)) toList
  val itemsRaiting = records.groupBy(_._1) //first group by
  val allItemsRaiting = for ((k,v) <- itemsRaiting) yield (k, v.groupBy(_._2).mapValues(_.size)) //second group by
  val totalRaitings = (allItemsRaiting toList) map(_._2) flatten
  val total = totalRaitings.groupBy(_._1) mapValues(_.map(_._2).sum)
  val hist = Histogram(orderBy(allItemsRaiting.get(64).get), orderBy(total))
  new PrintWriter("lab01.json") { write(hist.asJson.toString); close }
}

