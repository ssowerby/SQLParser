import com.spuriouscode.sql.model.Select
import com.spuriouscode.sql.parser.Parser

trait ParsingInTests {

  protected def parseSelect( sql: String ) : Select = {
    val p = new Parser()
    val r = p.parseAll(p.select, sql)
    if (!r.successful) {
      sys.error("Failed to parse : " + r)
    }
    r.get
  }

}
