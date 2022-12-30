// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import edu.gemini.grackle.syntax._
import cats.tests.CatsSuite
import cats.effect.unsafe.implicits.global
import cats.effect.IO
import java.time.ZonedDateTime
import io.circe.Json

// Higher-level example to demonstrate building a read-only mapping backed by effectful functions
object BlogMapping {
  val schema =
    schema"""
      type Query {
        blog(id: ID): Blog
        blogs(before: DateTime, ids: [ID!]): [Blog!]!
      }

      scalar Uri
      scalar DateTime

      type Blog {
        id: ID!
        title: String!
        link: Uri!
        date: DateTime!
      }
    """

  def blogs: List[Blog] =
    List(
      Blog("hello-world", "", "", ZonedDateTime.now())
    )

  def blogProcessor(id: String): IO[Option[Blog]] =
    IO {
      blogs.find(_.id == id)
    }

  def blogsProcessor(maybeBefore: Option[ZonedDateTime], ids: Option[List[String]]): IO[List[Blog]] =
    IO {
      val before = maybeBefore.getOrElse(ZonedDateTime.now())
      ids match {
        case None        => blogs.filter(_.dateTime.isBefore(before))
        case Some(value) => blogs.filter(x => x.dateTime.isBefore(before) && value.contains(x.id))
      }
    }

}

final case class Blog(id: String, title: String, link: String, dateTime: ZonedDateTime)

object Blog {
  // TODO: It's not currently possible to write this instance due to the scope of `semiauto`
  // and there not being a way to manually define a `CursorBuilder`. This is the final piece, other than polishing the API
  // (Clean up argument building, extract type classes, etc).
  implicit def cb: CursorBuilder[Blog] = ???
}

final class BlogMappingSpec extends CatsSuite {
  import BlogMapping._

  test("generic effect") {
    val query = """
      query {
        blog(id: "hello-world") {
          title
          dateTime
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "foo" : {
            "s" : "hi",
            "n" : 42
          }
        }
      }
    """

    val blogsQuery: EffQuery[IO] = EffQueryBuilder
      .noArgs[IO]
      .withArg[Option[ZonedDateTime]]("before")
      .withArg[Option[List[String]]]("ids")
      .build("blogs", blogsProcessor(_, _))

    val blogQuery: EffQuery[IO] =
      EffQueryBuilder.noArgs[IO].withString("id").build("blog", blogProcessor(_))

    val thingy: GenericMapping[IO] =
      EffMappingBuilder
        .single[IO](blogQuery)
        .withQuery(blogsQuery)
        .build(BlogMapping.schema)

    val result: Json = thingy.compileAndRun(query).unsafeRunSync()
    // println(result)

    assert(result == expected)
  }
}
