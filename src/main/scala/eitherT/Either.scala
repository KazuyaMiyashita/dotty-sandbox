package eitherT

sealed trait Either[+A, +B] {
  def isRight: Boolean
  def map[BB](f: B => BB): Either[A, BB]
  def flatMap[AA >: A, BB](f: B => Either[AA, BB]): Either[AA, BB]
}

final case class Left[+A, +B](value: A) extends Either[A, Nothing] {
  override def isRight = false
  override def map[BB](f: Nothing => BB): Either[A, BB] = this
  override def flatMap[AA >: A, BB](f: Nothing => Either[AA, BB]): Either[AA, BB] = this
}

final case class Right[+A, +B](value: B) extends Either[Nothing, B] {
  override def isRight = true
  override def map[BB](f: B => BB): Either[Nothing, BB] = Right(f(value))
  override def flatMap[AA, BB](f: B => Either[AA, BB]): Either[AA, BB] = f(value)
}
