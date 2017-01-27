package chapter_9

object Main extends App {
  val input =
    """
               {
      |"qwe" : 9,
      |"wer" :    1.2,
      |
      |"asda"   :  44,
      |"xx" :  42
      |
      | }
      |
    """.stripMargin

  val empty =
    """
      |
      | {
      |
      |  }
      |
      | """.stripMargin

  val complexJs =
    """
      |
      |[
      |	{
      |		"id": "0001",
      |		"type": "donut",
      |		"name": "Cake",
      |		"ppu": 0.55,
      |		"batters":
      |			{
      |				"batter":
      |					[
      |						{ "id": "1001", "type": "Regular" },
      |						{ "id": "1002", "type": "Chocolate" },
      |						{ "id": "1003", "type": "Blueberry" },
      |						{ "id": "1004", "type": "Devil's Food" }
      |					]
      |			},
      |		"topping":
      |			[
      |				{ "id": "5001", "type": "None" },
      |				{ "id": "5002", "type": "Glazed" },
      |				{ "id": "5005", "type": "Sugar" },
      |				{ "id": "5007", "type": "Powdered Sugar" },
      |				{ "id": "5006", "type": "Chocolate with Sprinkles" },
      |				{ "id": "5003", "type": "Chocolate" },
      |				{ "id": "5004", "type": "Maple" }
      |			]
      |	},
      |	{
      |		"id": "0002",
      |		"type": "donut",
      |		"name": "Raised",
      |		"ppu": 0.55,
      |		"batters":
      |			{
      |				"batter":
      |					[
      |						{ "id": "1001", "type": "Regular" }
      |					]
      |			},
      |		"topping":
      |			[
      |				{ "id": "5001", "type": "None" },
      |				{ "id": "5002", "type": "Glazed" },
      |				{ "id": "5005", "type": "Sugar" },
      |				{ "id": "5003", "type": "Chocolate" },
      |				{ "id": "5004", "type": "Maple" }
      |			]
      |	},
      |	{
      |		"id": "0003",
      |		"type": "donut",
      |		"name": "Old Fashioned",
      |		"ppu": 0.55,
      |		"batters":
      |			{
      |				"batter":
      |					[
      |						{ "id": "1001", "type": "Regular" },
      |						{ "id": "1002", "type": "Chocolate" }
      |					]
      |			},
      |		"topping":
      |			[
      |				{ "id": "5001", "type": "None" },
      |				{ "id": "5002", "type": "Glazed" },
      |				{ "id": "5003", "type": "Chocolate" },
      |				{ "id": "5004", "type": "Maple" }
      |			]
      |	}
      |]
      |
      |
    """.stripMargin


  val js =
    """
      |{
      |
      |"Company":
      |     {
      |       "name" : "Microsoft Corporation"
      |     },
      |"Ticker" : "MSFT",
      |"Active" : true,
      |"Price" : 30.66,
      |"Shares outstanding" : 8.38,
      |"Related companies" :
      |     ["HPQ" ,"IBM" ,"YHOO" ,"DELL" ,"GOOG"]
      |
      |}
      | """.stripMargin

  val simpleJs = """{"a":1}"""

  import SimpleParser._
  import parser._

//  println(parser.run(jsonParser(parser))(complexJs))

  val tupleParser: Parser[(String, Double)] = string.skipThat(':') ** double
  val mapParser: Parser[Map[String, Double]] = '{'.<*>.skipThis(tupleParser.manyWithSeparator(",")).skipThat('}'.<*>).map(_.toMap)

//  println(parser.run(mapParser)(input))
//  println(parser.run(mapParser)(empty))
//
//  println(run(parser.string("aaacc").many.slice.map(_.toUpperCase))("aaaccaaaccaaaccsadds"))
//
//  println(run((parser.string("blah") ** parser.impl.contextSensitive ** char('h').many).slice)("blah3aaahhba"))


  import MyParser.{parser => myParser}
  import myParser._

  println(myParser.run(jsonParser(myParser))(complexJs))


}
