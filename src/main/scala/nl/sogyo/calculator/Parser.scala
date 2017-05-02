package nl.sogyo.calculator

import scala.collection._
import scala.util.{ Try, Success, Failure }

object Parser {
  var nonNumberStack = List[Token]()

  def parse(input: String): Try[Seq[Token]] = {
    nonNumberStack = List[Operator]()
    try {
      Success(parseList(input.split(' ').toList))
    } catch {
      case e: Exception => Failure(e)
    }
  }

  def handleOperator(o: Operator): Seq[Operator] = {
    nonNumberStack match {
      case Nil => addToStackAndReturnEmptyList(o)
      case h :: t => h match {
        case op: Operator => {
          if ((o.isLeftAssociative && o.precedence <= op.precedence) || (!o.isLeftAssociative && o.precedence < op.precedence)) {
            nonNumberStack = nonNumberStack.tail
            op +: handleOperator(o)
          } else addToStackAndReturnEmptyList(o)
        }
        case _ => addToStackAndReturnEmptyList(o)
      }
    }
  }

  def addToStackAndReturnEmptyList(o: Operator): Seq[Operator] = {
    nonNumberStack = o +: nonNumberStack
    Nil
  }

  private def parseList(lst: List[String]): Seq[Token] = {
    lst match {
      case x :: xs => {
        val tkn = Token(x)
        tkn match {
          case Success(t) => {
            t match {
              case n: Number => t +: parseList(xs)
              case o: Operator => {
                handleOperator(o) ++ parseList(xs)
              }
              case p: Parens => p match {
                case LeftParens => {
                  nonNumberStack = p +: nonNumberStack
                  parseList(xs)
                }
                case rightParens => {
                  popStackUntilLeftParens ++ parseList(xs)
                }
              }
            }
          }
          case Failure(t) => throw t
        }
      }
      case _ => nonNumberStack
    }
  }

  private def popStackUntilLeftParens: Seq[Token] = {
    nonNumberStack match {
      case t :: ts => t match {
        case LeftParens => {
          nonNumberStack = ts
          Nil
        }
        case _ => {
          nonNumberStack = ts
          t +: popStackUntilLeftParens
        }
      }
      case _ => throw new exceptions.MismatchedParenthesesException()
    }
  }
}