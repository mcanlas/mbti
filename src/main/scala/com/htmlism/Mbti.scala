package com.htmlism

object Mbti extends App {
  val A = List[ExtrovertIntrovert](Extrovert, Introvert)
  val B = List[SensingIntuition](Sensing, Intuition)
  val C = List[ThinkingFeeling](Thinking, Feeling)
  val D = List[JudgingPerceiving](Judging, Perceiving)

  implicit class Ops[A : SingleLetter](x: A) {
    def s: String =
      implicitly[SingleLetter[A]].s(x)
  }

  implicit class OppOps[A : Opposite](x: A) {
    def opp: A =
      implicitly[Opposite[A]].opp(x)
  }

  println
  println("When the last three letters are stable, X$$$, the two types will use the same pair of cognitive functions, just in different order")
  for {
    b <- B
    c <- C
    d <- D
    a <- A
  } {
    printAnalysis(a, b, c, d)
  }

  println
  println("When second letter is ambiguous, $X$$, it just controls which leads for that part of the sandwich")
  for {
    a <- A
    c <- C
    d <- D
    b <- B
  } {
    printAnalysis(a, b, c, d)
  }

  println
  println("Similarly, When third letter is ambiguous, $$X$, it just controls which leads for that part of the sandwich")
  for {
    a <- A
    b <- B
    d <- D
    c <- C
  } {
    printAnalysis(a, b, c, d)
  }

  println
  println("When the fourth letter is ambiguous, $$$X, the order and extroversion of the pair of cognitive functions will be flipped")
  for {
    a <- A
    b <- B
    c <- C
    d <- D
  } {
    printAnalysis(a, b, c, d)
  }

  def printAnalysis(a: ExtrovertIntrovert, b: SensingIntuition, c: ThinkingFeeling, d: JudgingPerceiving): Unit = {
    print(a.s + b.s + c.s + d.s)
    print(": ")

    // STEP 1: introversion and extroversion will alternate, starting with your letter
    val eiBase = List(a, opposite(a))
    val eiFour = eiBase ::: eiBase

    // STEP 2a: form a judgement sandwich, following your extroversion JPPJ or PJJP
    // STEP 2b: introverts will place their judgement as the "meat" _JJ_ or _PP_
    // STEP 3: fill in your judgement with TF and your perceiving with SN, starting with your preferred letter
    val functionsFour: List[ExtrovertIntrovert => CognitiveFunction] =
    (a, d) match {
      case (Extrovert, Judging) | (Introvert, Perceiving) =>
        List(JudgingFunction(c, _), PerceivingFunction(b, _), PerceivingFunction(b.opp, _), JudgingFunction(c.opp, _))

      case (Extrovert, Perceiving) | (Introvert, Judging) =>
        List(PerceivingFunction(b, _), JudgingFunction(c, _), JudgingFunction(c.opp, _), PerceivingFunction(b.opp, _))
    }

    val cognitiveFunctions =
      for (i <- 0 until 4) yield {
        functionsFour(i)(eiFour(i))
      }

    println {
      cognitiveFunctions
        .map(CognitiveShow.show)
        .mkString(" - ")
    }
  }


  def opposite(x: ExtrovertIntrovert) =
    x match {
      case Extrovert =>
        Introvert

      case Introvert =>
        Extrovert
    }
}

trait SingleLetter[A] {
  def s(x: A): String
}

object SingleLetter {
  implicit val ei: SingleLetter[ExtrovertIntrovert] =
    {
      case Extrovert => "E"
      case Introvert => "I"
    }

  implicit val sn: SingleLetter[SensingIntuition] =
    {
      case Sensing => "S"
      case Intuition => "N"
    }

  implicit val tf: SingleLetter[ThinkingFeeling] =
    {
      case Thinking => "T"
      case Feeling => "F"
    }

  implicit val jp: SingleLetter[JudgingPerceiving] =
    {
      case Judging => "J"
      case Perceiving => "P"
    }
}

trait Opposite[A] {
  def opp(x: A): A
}

object Opposite {
  implicit val ei: Opposite[ExtrovertIntrovert] =
  {
    case Extrovert => Introvert
    case Introvert => Extrovert
  }

  implicit val sn: Opposite[SensingIntuition] =
  {
    case Sensing => Intuition
    case Intuition => Sensing
  }

  implicit val tf: Opposite[ThinkingFeeling] =
  {
    case Thinking => Feeling
    case Feeling => Thinking
  }

  implicit val jp: Opposite[JudgingPerceiving] =
  {
    case Judging => Perceiving
    case Perceiving => Judging
  }
}

sealed trait ExtrovertIntrovert

case object Extrovert extends ExtrovertIntrovert
case object Introvert extends ExtrovertIntrovert

sealed trait SensingIntuition

case object Sensing extends SensingIntuition
case object Intuition extends SensingIntuition

sealed trait ThinkingFeeling

case object Thinking extends ThinkingFeeling
case object Feeling extends ThinkingFeeling

sealed trait JudgingPerceiving

case object Perceiving extends JudgingPerceiving
case object Judging extends JudgingPerceiving

sealed trait CognitiveFunction

case class PerceivingFunction(sn: SensingIntuition, ei: ExtrovertIntrovert) extends CognitiveFunction
case class JudgingFunction(tf: ThinkingFeeling, ei: ExtrovertIntrovert) extends CognitiveFunction

trait CognitiveShow[A] {
  def show(x: A): String
}

object CognitiveShow {
  def show(x: CognitiveFunction): String =
    x match {
      case PerceivingFunction(sn, ei) =>
        implicitly[SingleLetter[SensingIntuition]]
          .s(sn) + implicitly[SingleLetter[ExtrovertIntrovert]]
          .s(ei).toLowerCase()

      case JudgingFunction(tf, ei) =>
        implicitly[SingleLetter[ThinkingFeeling]]
          .s(tf) + implicitly[SingleLetter[ExtrovertIntrovert]]
          .s(ei).toLowerCase()
    }
}