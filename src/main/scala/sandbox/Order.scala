package sandbox

import cats.Monoid

case class Order(totalCost: Double, quantity: Double)

object Order {
  implicit val ordersMonoid: Monoid[Order] = new Monoid[Order] {
    def empty = Order(0, 0)

    def combine(order1: Order, order2: Order) = Order(
      order1.totalCost + order2.totalCost,
      order1.quantity + order2.quantity
    )
  }

  implicit class OrdersMonoid(order: Order) {
    def combine(otherOrder: Order)(ordersMonoid: Monoid[Order]) =
      ordersMonoid.combine(order, otherOrder)
  }
}
