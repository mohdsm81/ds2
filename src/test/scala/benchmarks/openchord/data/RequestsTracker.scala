package benchmarks.openchord.data
import scala.collection.mutable

case class RequestsTracker() {
  // idString -> RequestRecord; idString is made using makeRequestID()
  val readRequests: mutable.Map[String, RequestRecord] = mutable.Map[String, RequestRecord]()
  // idString -> RequestRecord; idString is made using makeRequestID()
  val writeRequests: mutable.Map[String, RequestRecord] = mutable.Map[String, RequestRecord]()
}