import Duration.Monthly

@main def hello(): Unit =
  print("normal calculation: 4.8-4.7-0.1 = ")
  doNormalCalc()
  print("Spire calculation: 4.8-4.7-0.1 = ")
  doSpireCalc()
  showComplicatedPricing()

def doNormalCalc(): Unit = {
  println(4.8 - 4.7 - 0.1)
}

def doSpireCalc(): Unit = {
  import spire._
  import spire.math._
  import spire.implicits._

  println(Rational(48, 10) - Rational(47, 10) - Rational(1, 10))
  print("another Spire calculation: ")
  println(poly"4.8 - 4.7 - 0.1".apply(0))
  print("more another Spire calculation: ")
  println(r"4.8" - r"4.7" - r"0.1")
}

enum Person:
  case Adult
  case Child

enum Duration:
  case Annual
  case Monthly

def calculateComplicatedMonthlyPricingDiscount(
    client: Person,
    planDuration: Duration,
    hasCoupon: Boolean,
): spire.math.Polynomial[spire.math.Rational] = {
  // We are mobile network carrier company.
  // We have many complecated payment pricing and discount.
  import spire._
  import spire.math._
  import spire.implicits._

  val ageDiscount = client match
    case Person.Adult => poly"1x"
    case Person.Child => poly"0.9x"

  val durationDiscount = planDuration match
    case Duration.Annual =>
      val constantDiscount = r"2000/12" // caveat: no space between 2000 and /.
      poly"x - $constantDiscount" // Discount 2000 yen from annual fee.
    case Duration.Monthly => poly"x"

  val couponDiscount = if hasCoupon then poly"0.99x" else poly"x"

  // First apply duration discount, next age discount, then coupon discount.
  couponDiscount.compose(ageDiscount).compose(durationDiscount)
}

def showComplicatedPricing(): Unit = {
  import spire.syntax.literals.r
  val basePrice = r"2000"
  println("adult, monthly, no coupon")
  println(
    calculateComplicatedMonthlyPricingDiscount(
      Person.Adult,
      Duration.Monthly,
      hasCoupon = false,
    )(basePrice),
  ) // => No discount. 2000 yen per month.

  println("adult, monthly, with coupon")
  println(
    calculateComplicatedMonthlyPricingDiscount(
      Person.Adult,
      Duration.Monthly,
      hasCoupon = true,
    )(basePrice),
  ) // => 1980 yen per month because of 0.99x discount.

  println("child, monthly, no coupon")
  println(
    calculateComplicatedMonthlyPricingDiscount(
      Person.Child,
      Duration.Monthly,
      hasCoupon = false,
    )(basePrice),
  ) // => 1800 yen per month because of 0.9x discount.

  // more complex pattern...
  println("child, annual, with coupon")
  val discounted = calculateComplicatedMonthlyPricingDiscount(
    Person.Child,
    Duration.Annual,
    hasCoupon = true,
  )(basePrice)

  println(
    discounted.toBigDecimal(4, java.math.RoundingMode.HALF_EVEN),
  ) // => (2000 - (2000 / 12)) * 0.9 * 0.99 yen per month because of (2000 / 12) yen discount and 0.9x discount and 0.99x discount.
  // => 1633.50
  println("without Spire:")
  println(
    (2000 - (2000 / 12)) * 0.9 * 0.99,
  ) // 1634.094
}
