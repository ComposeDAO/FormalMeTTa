import java.net.URI
import java.util.UUID

import scala.collection.MultiSet
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ArraySeq



case class Space(ts: MultiSet[Term])
object Space:
  def apply(ts: Term*) = new Space(MultiSet.from(ts))

sealed trait Term

// case class Expr(ts: Vector[Term]) extends Term
// object Expr:
//   def apply(t: Term, ts: Term*): Expr = new Expr(t +: ts.toVector)
//   def nest(t: Term, ts: Term*): Term = (t +: ts).reduceRight((x, y) => Expr(Vector(x, y)))


/*
val modifiedTs = t match {
      case expr: Expr if expr.ts.nonEmpty && expr.ts.headOption.exists(_.isInstanceOf[Builtin]) && expr.ts.head.asInstanceOf[Builtin] == sealedVars =>
        expr.ts.tail.map {
          case Var(name) => {
            System.out.println("var to convert = " + name)
            Var("xyz")
          }
          case term => term
        }
      case _ => Vector(t)
    }
    new Expr(modifiedTs ++ ts.toVector)




    t match {
      case expr: Expr if expr.ts.nonEmpty && expr.ts.headOption.exists(_.isInstanceOf[Builtin]) && expr.ts.head.asInstanceOf[Builtin] == sealedVars =>
        // Handle when t is of type sealedVars
        println("Type of t: " + t.getClass.getName) 
        System.out.println("t is of type sealedVars")
        System.out.println("T VAL = " + t)
        System.out.println("Ts VAL = " + ts)
        // Additional logic specific to sealedVars case
      case _ =>
        // Handle other cases
        //System.out.println("t is not of type sealedVars")
        //System.out.println("T VAL = " + t)
        //System.out.println("Ts VAL = " + ts)
        // Additional logic for other cases
    }

*/


case class Expr(ts: Vector[Term]) extends Term
object Expr {
  def apply(t: Term, ts: Term*): Expr = {
    val uniqueVars: Map[String, String] = Map.empty[String, String]
    val varsArray: ArrayBuffer[Term] = ArrayBuffer.empty[Term]


    if (t.isInstanceOf[Builtin] && t.asInstanceOf[Builtin] == sealedVars) {
      System.out.println("hitting apply for t = " + t)
      System.out.println("hitting apply for ts = " + ts)


      filterVarsAndExprs(ts.toVector,varsArray)

      System.out.println("varsArray = " + varsArray)
      varsArray.foreach {
        case Var(v) => {
            System.out.println("Var hit = " + v)
            val varValue: Var = uniqueVars.get(v) match {
              case Some(value) => Var(value) // Value exists in the map, create Var with the existing value
              case None =>
                System.out.println("v= " +v)
                val newVarId = UUID.randomUUID().toString()
                uniqueVars(v)= newVarId // Add new key-value pair to the map
                Var(newVarId) // Create Var with the new UUID
            }
          }
        case _ => {}
      }

      System.out.println("uniqueVars = " + uniqueVars)

      val modifiedTerms = ts.map {
          case Var(v) => {
            System.out.println("Var hit = " + v)
            val varValue: Var = uniqueVars.get(v) match {
              case Some(value) => Var(value) // Value exists in the map, create Var with the existing value
              case None => Var(v)
                //shouldnt get here
                // System.out.println("v= " +v)
                // val newVarId = UUID.randomUUID().toString()
                // uniqueVars(v)= newVarId // Add new key-value pair to the map
                // Var(newVarId) // Create Var with the new UUID
            }
            varValue
          }
          case nestedExpr: Expr => {
            System.out.println("Expr hit = " + nestedExpr)
            System.out.println("Expr hit ts= " + nestedExpr.ts)
            Expr(modifyVarsInExpr(nestedExpr, uniqueVars))
          }
          case term => {
            System.out.println("term hit = " + term)
            term
          }
        }
        System.out.println("modifiedTerms = " +modifiedTerms)
        Expr(modifiedTerms.toVector)
    } else {
      Expr(t +: ts.toVector)
    }

    // val modifiedTs = t match {
    //   case expr: Expr if expr.ts.nonEmpty && expr.ts.headOption.exists(_.isInstanceOf[Builtin]) && expr.ts.head.asInstanceOf[Builtin] == sealedVars =>
    //     System.out.println("expr.ts = " + expr.ts)
    //     val modifiedTerms = expr.ts.tail.map {
    //       case Var(v) => {
    //         val varValue: Var = uniqueVars.get(v) match {
    //           case Some(value) => Var(value) // Value exists in the map, create Var with the existing value
    //           case None =>
    //             System.out.println("v= " +v)
    //             val newVarId = UUID.randomUUID().toString()
    //             uniqueVars(v)= newVarId // Add new key-value pair to the map
    //             Var(newVarId) // Create Var with the new UUID
    //         }
    //         varValue
    //       }
    //       case nestedExpr: Expr => Expr(modifyVarsInExpr(nestedExpr, uniqueVars))
    //       case term => term
    //     }
    //     System.out.println("modifiedTerms = " +modifiedTerms)
    //     modifiedTerms
    //   case _ => Vector(t)
    // }
   //Expr(modifiedTs ++ ts.toVector)
  }

  private def filterVarsAndExprs(terms: Vector[Term], varsArray: ArrayBuffer[Term]): ArrayBuffer[Term] = {
    val termsVector: Vector[Term] = terms.toVector
    for (term <- termsVector) {
      term match {
        case v: Var =>
          varsArray += v
        case expr: Expr =>
          filterVarsAndExprs(expr.ts, varsArray) // Recursively process the terms in the Expr
        case _ =>
          // Ignore non-Var, non-Expr terms
      }
    }
    varsArray
  }

  private def modifyVarsInExpr(expr: Expr, varMap: Map[String, String]): Vector[Term] = {
    expr.ts.map {
      case Var(v) => {
            val varValue: Var = varMap.get(v) match {
              case Some(value) => Var(value) // Value exists in the map
              case None =>
                Var(v)
            }
            varValue
          }
      case nestedExpr: Expr => Expr(modifyVarsInExpr(nestedExpr,varMap))
      case term => term
    }
  }

  def nest(t: Term, ts: Term*): Term = (t +: ts).reduceRight((x, y) => Expr(Vector(x, y)))
}

sealed trait Atom extends Term

case class Var(name: String) extends Atom

case class Symbol(name: String) extends Atom

sealed trait Builtin extends Atom
case object transform extends Builtin
case object === extends Builtin
case object addAtom extends Builtin
case object remAtom extends Builtin
case object sealedVars extends Builtin

sealed trait Ground extends Atom
case object Mul extends Ground  // not in doc
case class BoolLiteral(value: Boolean) extends Ground
case class DoubleLiteral(value: Double) extends Ground
case class LongLiteral(value: Long) extends Ground
case class StringLiteral(value: String) extends Ground
