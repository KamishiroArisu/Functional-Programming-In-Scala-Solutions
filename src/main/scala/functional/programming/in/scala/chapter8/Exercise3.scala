package functional.programming.in.scala.chapter8

object Exercise3 {
    trait Prop {
        def check: Boolean
        def &&(p: Prop): Prop = new Prop {
            override def check: Boolean = Prop.this.check && p.check
        }
    }
}
