import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

class HelloSpec extends Specification with ScalaCheck {
  "Hello" should {
    "say hello" in prop { name: String =>
     true
    }
  }
}
