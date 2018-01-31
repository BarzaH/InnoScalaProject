package com.example.app

import org.scalatra._

import org.json4s._
import org.scalatra.json._
import org.json4s.{DefaultFormats, Formats}

case class Message(id: Int, text: String)

case class CreateMessage(text: String)
case class MessageCreated(id: Int)

case class UpdateMessage(text: String)
case class MessageUpdated(index: Int)


class MyScalatraServlet extends ScalatraServlet  with JacksonJsonSupport {

  protected implicit lazy val jsonFormats: Formats = DefaultFormats

  var messages: List[Message]  = List(Message(1, "Good morning"), Message(2, "Good evening"))

  before() {
    contentType = formats("json")
  }

  get("/") {
    views.html.hello()
  }

  post("/messages")
  {
    val messageData = parsedBody.extract[CreateMessage]
    val id = messages.foldLeft (0) ( (maxId, m) => if (m.id > maxId) m.id else maxId ) + 1

    messages = Message(id, messageData.text) :: messages

    MessageCreated(id)
  }

  get("/messages")
  {
      messages
  }

  get("/messages/:id") {
    messages.find(m => m.id == params("id").toInt) match {
      case Some(message) => Ok(message)
      case None       => NotFound("User not found")
    }
  }

  put("/messages/:id") {
    val id = params("id").toInt
    val matchingIndex = messages.zipWithIndex.collect { case (Message(`id`, _), i) => i }
    if (matchingIndex.isEmpty)
      NotFound("User not found")
    else {
      val index = matchingIndex.head
      val newUserData = parsedBody.extract[UpdateMessage]

      messages = messages.updated(index, Message(id, newUserData.text))

      Ok(MessageUpdated(index))
    }
  }

  delete("/messages/:id") {
    messages = messages.filter( m => m.id != params("id").toInt)
    Ok("Done")
  }


}
