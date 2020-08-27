This project is still very incomplete.

# The ideas

* a lot of our modern tools basically do a real-time compilation step; e.g. tensorflow graphs, spark, airflow
* we should have a language where staged compilation is a simple language feature
* write arbitrarily compilated logic, bind or logic data to it, and do a quick runtime compilation step to make it an optimized operation
* define your own optimizer rules; e.g., `identityMatrix * otherMatrix = otherMatrix`
* have less distinction between data and logic

# Some scattered thoughts

For most modern compiled languages, we're basically forcing the user to work in this grid:

|| run time | compile time |
|| --- | --- |
| data | not used to optimize | usually not used to optimize |
| logic | maybe partially optimized by JIT | optimized by compiler |

Since compile time logic is the only thing well optimized, we often coerce the developer to write a complex class hierarchy to make their code efficient.
Let's look at this Scala example (and I love Scala):

```
trait DoubleOp {
  def eval(x: Double): Double
}

class Adder(y: Double) extends DoubleOp {
  override def eval(x: Double): Double = {
    x + y
  }
}

object BuiltInOps {
  val identityAdder = new Adder(0.0)
  val anotherAdder = new Adder(3.0)
}
```

We've written our code with a clear distinction between logic (`x + y`) and data (`y`). 
But notice that `identityAdder` could have a more optimized implementation, since adding `0.0` does nothing.
The JIT might figure this out if we run the function a million times, but it also might not in more complicated cases
If we plan to use `identityAdder` a lot and want to be certain it has the most efficient implementation, we would have to define yet another class:

```
class IdentityAdder extends DoubleOp {
  override def eval(x: Double): Double = {
    x
  }
}
...
  val identityAdder = new IdentityAdder()
...
```

But this brings another challenge - what if we wanted to know that IdentityAdder is also an `Adder`? We would need to define yet another trait, `AdderLike`, that both `Adder` and `IdentityAdder` inherit from, making this simple code even more complicated.

## Improving on it

Here's what the table will become for bindlang:
|| run time | compile time |
|| --- | --- |
| data | used to optimize if requested | optimized by compiler |
| logic | used to optimize if requested | optimized by compiler |

Here's (roughly) how I expect the above code will be written in bindlang, once finished:

```
trait DoubleOp {
  eval: (x: Double): Double
}

class Adder(y: Double) {
  eval = (x: Double): Double {
    x + y
  }
}

val identityAdder = Adder(0.0) //will be optimized by bindlang since this is available at compile time
val otherAdder = Adder(3.0)
```

Bindlang will optimize on all the data it can infer from the global environment, and if it's more optimal than the general implementation, it will use the optimized implementation.

And if we define an `Adder` and want to be certain it has the fastest implementation, we can request `myAdder = Adder.bind(y=myY)`.
By explicitly using `.bind` and knowing the exact value of `y`, we will benefit from any optimizer rules that apply to `myY`.

And unlike Scala, I plan to have traits capable of requiring data members from their implementations.
This will eliminate the distinction between trait and abstract class.
For instance,

```
trait A {
  data: MyDataType
}

trait B(d: MyDataType) extends A {}
trait C extends A {
  data = constantData
}
```