package controllers

import javax.inject._
import play.api.mvc._
import play.api.libs.json._
import models.Product
import scala.collection.mutable
import scala.concurrent.ExecutionContext

case class Category(id: Long, name: String)

object Category {
  implicit val categoryFormat: Format[Category] = Json.format[Category]
}

@Singleton
class ProductController @Inject()(controllerComponents: ControllerComponents)(implicit ec: ExecutionContext) extends AbstractController(controllerComponents) {

  private val productList = mutable.ListBuffer[Product]()
  private val categoryList = mutable.ListBuffer[Category]()
  private val shoppingCart = mutable.Map[Long, Int]().withDefaultValue(0)

  def getAllProducts: Action[AnyContent] = Action {
    Ok(Json.toJson(productList))
  }

  def fetchProductById(productId: Long): Action[AnyContent] = Action {
    productList.find(_.id.contains(productId)) match {
      case Some(product) => Ok(Json.toJson(product))
      case None => NotFound(Json.obj("error" -> "Product not found"))
    }
  }

  def addNewProduct: Action[JsValue] = Action(parse.json) { request =>
    request.body.validate[Product].fold(
      errors => BadRequest(Json.obj("error" -> JsError.toJson(errors))),
      product => {
        val newId = productList.map(_.id.getOrElse(0L)).foldLeft(0L)(Math.max) + 1
        val newProduct = product.copy(id = Some(newId))
        productList += newProduct

        if (!categoryList.exists(_.name == product.category)) {
          categoryList += Category(categoryList.size + 1, product.category)
        }

        Created(Json.toJson(newProduct))
      }
    )
  }

  def modifyProduct(productId: Long): Action[JsValue] = Action(parse.json) { request =>
    request.body.validate[Product].fold(
      errors => BadRequest(Json.obj("error" -> JsError.toJson(errors))),
      updatedProduct => {
        productList.indexWhere(_.id.contains(productId)) match {
          case -1 => NotFound(Json.obj("error" -> "Product not found"))
          case index =>
            val oldProduct = productList(index)
            val oldCategory = oldProduct.category
            val newCategory = updatedProduct.category

            // Usuwanie starej kategorii, jeśli nie jest już używana
            if (!productList.exists(p => p.category == oldCategory && p.id != Some(productId))) {
              categoryList.indexWhere(_.name == oldCategory) match {
                case -1 => // Nie powinno się zdarzyć
                case categoryIndex => categoryList.remove(categoryIndex)
              }
            }

            // Dodawanie nowej kategorii, jeśli nie istnieje
            if (!categoryList.exists(_.name == newCategory)) {
              categoryList += Category(categoryList.size + 1, newCategory)
            }

            productList.update(index, updatedProduct.copy(id = Some(productId)))
            Ok(Json.toJson(updatedProduct))
        }
      }
    )
  }

  def removeProduct(productId: Long): Action[AnyContent] = Action {
    productList.indexWhere(_.id.contains(productId)) match {
      case -1 => NotFound(Json.obj("error" -> "Product not found"))
      case index =>
        productList.remove(index)
        NoContent
    }
  }

  def fetchProductsByCategory(categoryName: String): Action[AnyContent] = Action {
    val filteredProducts = productList.filter(_.category == categoryName)
    Ok(Json.toJson(filteredProducts))
  }

  def createCategory(categoryName: String): Action[AnyContent] = Action {
    if (categoryList.exists(_.name == categoryName)) {
      Conflict(Json.obj("error" -> "Category already exists"))
    } else {
      categoryList += Category(categoryList.size + 1, categoryName)
      Created(Json.obj("message" -> "Category added successfully"))
    }
  }

  def getAllCategories: Action[AnyContent] = Action {
    val categoryNames = categoryList.map(_.name)
    Ok(Json.toJson(categoryNames))
  }

  def addItemToCart: Action[JsValue] = Action(parse.json) { request =>
    val productIdOpt = (request.body \ "productId").asOpt[Long]
    val quantityOpt = (request.body \ "quantity").asOpt[Int]

    (productIdOpt, quantityOpt) match {
      case (Some(productId), Some(quantity)) =>
        if (!productList.exists(_.id.contains(productId))) {
          NotFound(Json.obj("error" -> "Product not found"))
        } else {
          shoppingCart(productId) += quantity
          Ok(Json.obj("message" -> s"$quantity units of product with ID $productId added to cart"))
        }
      case _ =>
        BadRequest(Json.obj("error" -> "Invalid JSON format"))
    }
  }

  def removeItemFromCart: Action[JsValue] = Action(parse.json) { request =>
    val productIdOpt = (request.body \ "productId").asOpt[Long]
    val quantityOpt = (request.body \ "quantity").asOpt[Int]

    (productIdOpt, quantityOpt) match {
      case (Some(productId), Some(quantity)) =>
        if (!productList.exists(_.id.contains(productId))) {
          NotFound(Json.obj("error" -> "Product not found"))
        } else {
          if (shoppingCart(productId) <= quantity) {
            shoppingCart -= productId
          } else {
            shoppingCart(productId) -= quantity
          }
          Ok(Json.obj("message" -> s"$quantity units of product with ID $productId removed from cart"))
        }
      case _ =>
        BadRequest(Json.obj("error" -> "Invalid JSON format"))
    }
  }

  def displayCartContents: Action[AnyContent] = Action {
    val cartItems = shoppingCart.flatMap { case (productId, quantity) =>
      productList.find(_.id.contains(productId)).map(product => (product, quantity))
    }
    Ok(Json.toJson(cartItems))
  }
}
