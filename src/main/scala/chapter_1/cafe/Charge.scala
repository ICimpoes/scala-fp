package chapter_1.cafe

/**
  * Created by iliacimpoes on 06.11.16.
  */
case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge) = {
    if (other.cc == cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Cannot combine charges of different credit cards")
  }
}
