package com.example.app

import java.util.{Calendar, Date}

import org.scalatra._
import org.json4s._
import org.scalatra.json._
import org.json4s.{DefaultFormats, Formats}

case class Message(text: String)

case class User(var id: Int, var nickname: String, var password: String, var email: String)

case class Subscriber(var subscriberId: Int, var subscribedId: Int)

case class Retwit(var userId: Int, twit: Twit)

case class Twit(var id: Int, var text: String, var author: Int, var sub_time: Date)


case class LogIn(log: String, pass: String)

case class CreateMessage(text: String)

case class MessageCreated(id: Int)

case class UpdateMessage(text: String)

case class MessageUpdated(index: Int)


class MyScalatraServlet extends ScalatraServlet with JacksonJsonSupport {
  protected implicit lazy val jsonFormats: Formats = DefaultFormats

  //db for twits / subsctiptions / users retweets.
  var twits: List[Twit] = List(
    Twit(1, "huinya", 2, Calendar.getInstance.getTime),
    Twit(2, "huita",3,Calendar.getInstance().getTime),
    Twit(3, "realno", 3, Calendar.getInstance().getTime)
  )
  var subscriptions: List[Subscriber] = List(
    Subscriber(1, 2),
    Subscriber(2, 3),
  )
  var users: List[User] = List(
    User(1, "Oboltus", "qwe", "her@te"),
    User(2, "NeDurak", "omg", "ba@tak"),
    User(3, "Hahah", "dangYO", "ku@key")
  )
  var retweets: List[Retwit] = List(
    Retwit(1, twits(1)),
    Retwit(2, twits(2)),
  )

  val currentUser = users.head

  before() {
    contentType = formats("json")
  }

  post("/sessions"){
//    val a =10
    val userData = parsedBody.extract[LogIn]

    if(users.exists(x => x.nickname == userData.log && x.password == userData.pass))
      {
        val user = users.find(x => x.nickname == userData.log && x.password == userData.pass).get
        val jwt = new JWTAuth(user)
//        var dd = jwt.validateJWTToken(jwt.jwt)
        Ok(String.format("Token: %S", jwt.jwt))
      }


  }

  get("/") {
    views.html.hello()
  }

  //create tweet
  //checked
  post("/twit") {
    if (currentUser != null) {
      val message = parsedBody.extract[Message]
      val id = twits.foldLeft(0)((maxId, m) => if (m.id > maxId) m.id else maxId) + 1
      val newTwit = Twit(id, message.text, currentUser.id, Calendar.getInstance().getTime)
      twits = newTwit :: twits
      twits
    }
  }

  //edit my twits
  //checked
  put("/twit/:id") {
    val id = params("id").toInt
    val matchingIndex = twits.zipWithIndex.collect { case (Twit(`id`, _, _, _), i) => i }
    if (matchingIndex.isEmpty)
      NotFound("User not found")
    else {
      val index = matchingIndex.head
      val newUserData = parsedBody.extract[Message]
      if(twits(index).author == currentUser.id) {
        twits(index).text = newUserData.text
        twits
      }else{
        "Her te v rilo"
      }
    }

  }

  //retweet
  //ckecked
  post("/twit/retweet") {
    val retweet = parsedBody.extract[Retwit]
    retweets = retweet :: retweets
    retweets
  }

  //delete my twit with id
  //checked
  delete("/twit/:id") {
    twits = twits.filter(m => (m.id != params("id").toInt) || (m.author != currentUser.id))
    twits
  }

  //subscribe to user with id
  //checked
  post("/subscribe/:id") {
    val to = params("id").toInt
    val subscribeTo = users.filter(u => u.id == to)
    if(subscribeTo.nonEmpty){
      if(!subscriptions.exists(s => (s.subscriberId == currentUser.id) && (s.subscribedId == to))){
        subscriptions = Subscriber(currentUser.id,to) :: subscriptions
      }else{
        "tipa otlovil oshibku"
      }
    }
    subscriptions
  }

  //get feed of other users
  //checked
  get("/feed/:id") {
    val usersPosts = twits.filter(t => t.author == params("id").toInt)
    usersPosts
  }

  //get my feed
  //checked
  get("/feed") {
    val mySubscribtions = subscriptions.filter(s=>s.subscriberId == currentUser.id)
    val subsId = mySubscribtions.map(s => s.subscribedId)
    var feed = twits.filter(twit => subsId.contains(twit.author))
    val additional = retweets.filter(retwit => subsId.contains(retwit.userId)).map(r => r.twit)
    feed = additional ++ feed
    if(feed.isEmpty){
        "Your feed is empty lul"
    }else{
      feed
    }
  }




}
