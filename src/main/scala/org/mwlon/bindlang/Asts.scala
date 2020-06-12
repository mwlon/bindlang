package org.mwlon.bindlang


import org.mwlon.bindlang.Tokens.TypedToken

import scala.collection.mutable

object Asts {
  trait Ast extends Serializable {
    val children: Array[Ast]
    lazy val variables: Set[Variable] = {
      val res = mutable.Set.empty[Variable]
      children.foreach(child => {
        res.addAll(child.variables)
      })
      res.toSet
    }
    def toLlvmIr: String
    lazy val java: String = toLlvmIr
  }

  case class GlobalAst(children: Array[Ast]) extends Ast {
    override def toLlvmIr: String = {
      children.map(_.toLlvmIr).mkString("\n")
    }
  }

  case class ClosureAst(children: Array[Ast]) extends Ast {
    override def toLlvmIr: String = {
      s"closure {\n${children.map(_.toLlvmIr).mkString("\n")}\n}"
    }
  }


  case class AssignAst(name: String, expr: Ast) extends Ast {
    override val children: Array[Ast] = Array(expr)
    override def toLlvmIr: String = {
      s"$name = ${expr.toLlvmIr}"
    }
  }

  case class FunctionDefAst(argNames: Array[String], closure: Ast) extends Ast {
    override val children: Array[Ast] = Array(closure)

    override def toLlvmIr: String = {
      s"def (${argNames.mkString(",")}) => ${closure.toLlvmIr}"
    }
  }

  case class WhileAst(conditionAst: Ast, effectAst: Ast) extends Ast {
    override val children: Array[Ast] = Array(conditionAst, effectAst)

    override def toLlvmIr: String = {
      s"while ${conditionAst.toLlvmIr}\n${effectAst.toLlvmIr}"
    }
  }

  case class FunctionCallAst(name: String, args: Array[Ast]) extends Ast {
    override val children: Array[Ast] = args

    override def toLlvmIr: String = {
      s"invoke $name (${args.map(_.toLlvmIr).mkString(",")})"
    }
  }

  case class UnaryOpAst(opName: String, child: Ast) extends Ast {
    override val children: Array[Ast] = Array(child)

    override def toLlvmIr: String = {
      s"$opName ${child.toLlvmIr}"
    }
  }

  case class VarAst(name: String) extends Ast {
    override lazy val variables: Set[Variable] = Set(Variable(name))
    override val children: Array[Ast] = Array()

    override def toLlvmIr: String = name
  }

  case class ConstantAst[T](token: TypedToken[T]) extends Ast {
    override val children: Array[Ast] = Array()
    override def toLlvmIr: String = token.toString
  }

  case class UnaryBinaryAst(unary0: Ast, unary1: Ast, binaryOp: String) extends Ast {
    override val children: Array[Ast] = Array(unary0, unary1)

    override def toLlvmIr: String = {
      s"(${unary0.toLlvmIr} $binaryOp ${unary1.toLlvmIr})"
    }
  }

  object UnaryBinaryAst {
    def from(unaries: Array[Ast], binaries: Array[String]): Ast = {
      assert(unaries.length == binaries.length + 1)
      if (unaries.length == 1) {
        unaries(0)
      } else {
        val opOrder = binaries.zipWithIndex.sortBy(_._1)
        val lowers = unaries.indices.toArray
        val uppers = unaries.indices.toArray
        val merged = unaries
        var res: Option[UnaryBinaryAst] = None
        opOrder.foreach({ case (opName, ind) =>
          val applied = UnaryBinaryAst(merged(ind), merged(ind + 1), opName)
          res = Some(applied)
          merged(lowers(ind)) = applied
          merged(uppers(ind + 1)) = applied
          for (i <- lowers(ind) until uppers(ind + 1)) {
            uppers(i) = uppers(ind + 1)
            lowers(i) = lowers(ind)
          }
        })
        res.get
      }
    }
  }
}
