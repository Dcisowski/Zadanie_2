package models

import play.api.libs.json._

case class Product(name: String, description: String, price: Double, id: Option[Long], category:String)

object Product {
  implicit val productFormat: Format[Product] = Json.format[Product]
}
