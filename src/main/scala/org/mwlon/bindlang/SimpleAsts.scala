package org.mwlon.bindlang


import org.mwlon.bindlang.Tokens.TypedToken

import scala.collection.mutable

object SimpleAsts {
  trait SimpleAst extends Serializable {
    val children: Array[SimpleAst]
  }

  case class GlobalAst(children: Array[SimpleAst]) extends SimpleAst {
    scope = Some(mutable.Set())
    override def toBindlang: String = {
      children.map(_.bindlang).mkString("\n")
    }
  }

  case class ClosureAst(children: Array[SimpleAst]) extends SimpleAst {
    override def toBindlang: String = {
      s"{\n${children.map(_.bindlang).mkString("\n")}\n}"
    }

    override def initialize(): Unit = {
      val scopeForChildren = scope.get.clone()
      children.foreach(child => {
        child.parent = Some(this)
        assert(child.variables.forall(variable => scopeForChildren.contains(variable)))
        child match {
          case x: AssignAst =>
            val variable = Variable(x.name, x.expr)
            scopeForChildren += variable
          case x: VarAst =>
            x.variables.foreach(variable => {
              variable.addUsage(child)
            })
        }
      })
    }
  }

  case class AssignAst(name: String, expr: SimpleAst) extends SimpleAst {
    override val children: Array[SimpleAst] = Array(expr)
    override def toBindlang: String = {
      s"$name=${expr.bindlang}"
    }
  }

  case class FunctionDefAst(argNames: Array[String], closure: SimpleAst) extends SimpleAst {
    override val children: Array[SimpleAst] = Array(closure)
    var bindable = false

    override def toBindlang: String = {
      s"${if (bindable) "bindable" else ""}(${argNames.mkString(",")})${closure.bindlang}"
    }

    def markBindable(): Unit = {
      bindable = true
    }
  }

  case class WhileAst(conditionAst: SimpleAst, effectAst: SimpleAst) extends SimpleAst {
    override val children: Array[SimpleAst] = Array(conditionAst, effectAst)

    override def toBindlang: String = {
      s"while${conditionAst.bindlang}${effectAst.bindlang}"
    }
  }

  case class ForAst(name: String, countAst: SimpleAst, effectAst: SimpleAst) extends SimpleAst {
    override val children: Array[SimpleAst] = Array(countAst, effectAst)

    override def toBindlang: String = {
      s"for($name,${countAst.bindlang})\n${effectAst.bindlang}"
    }
  }

  case class FunctionCallAst(name: String, args: Array[SimpleAst]) extends SimpleAst {
    override val children: Array[SimpleAst] = args

    override def toBindlang: String = {
      s"$name(${args.map(_.bindlang).mkString(",")})"
    }
  }

  case class UnaryOpAst(opName: String, child: SimpleAst) extends SimpleAst {
    override val children: Array[SimpleAst] = Array(child)

    override def toBindlang: String = {
      s"$opName ${child.bindlang}"
    }
  }

  case class VarAst(scope: Scope, name: String) extends SimpleAst {
    override lazy val variables: Set[Variable] = Set(variable)
    override val children: Array[SimpleAst] = Array()

    override def toBindlang: String = name
  }

  case class ConstantAst[T](token: TypedToken[T]) extends SimpleAst {
    override val children: Array[SimpleAst] = Array()
    override def toBindlang: String = token.value.toString //not quite right
  }

  case class BinaryAst(unary0: SimpleAst, unary1: SimpleAst, binaryOp: String) extends SimpleAst {
    override val children: Array[SimpleAst] = Array(unary0, unary1)

    override def toBindlang: String = {
      s"(${unary0.bindlang}$binaryOp${unary1.bindlang})"
    }
  }

  object BinaryAst {
    def from(unaries: Array[SimpleAst], binaries: Array[String]): SimpleAst = {
      assert(unaries.length == binaries.length + 1)
      if (unaries.length == 1) {
        unaries(0)
      } else {
        val opOrder = binaries.zipWithIndex.sortBy(_._1)
        val lowers = unaries.indices.toArray
        val uppers = unaries.indices.toArray
        val merged = unaries
        var res: Option[BinaryAst] = None
        opOrder.foreach({ case (opName, ind) =>
          val applied = BinaryAst(merged(ind), merged(ind + 1), opName)
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
