package com.example.app

import java.util.{Calendar, Date}

import authentikat.jwt.{JsonWebToken, JwtHeader}
import org.scalatra._
import org.json4s._
import org.scalatra.json._
import org.json4s.{DefaultFormats, Formats}

case class Message(text: String)

case class User(var id: Int, var nickname: String, var password: String, var email: String)

case class Subscriber(var subscriberId: Int, var subscribedId: Int)

case class Retwit(var userId: Int, twit: Twit)

case class Twit(var id: Int, var text: String, var author: Int, var sub_time: Date)

case class Auth(token:String)

case class LogIn(log: String, pass: String)

case class RetwitModel(authorId : Int, twitId : Int)


class MyScalatraServlet extends ScalatraServlet with JacksonJsonSupport {
  protected implicit lazy val jsonFormats: Formats = DefaultFormats

  //db for twits / subsctiptions / users retweets.
  var twits: List[Twit] = List(
    Twit(1, "Some text", 2, Calendar.getInstance.getTime),
    Twit(2, "Another text",3,Calendar.getInstance().getTime),
    Twit(3, "Yet another text", 3, Calendar.getInstance().getTime),
    Twit(4, "twit that will be deleted", 1 ,Calendar.getInstance().getTime)
  )
  var subscriptions: List[Subscriber] = List(
    Subscriber(1, 2),
    Subscriber(2, 3),
  )
  var users: List[User] = List(
    User(1, "NambaVan", "qwe", "hik@tak"),
    User(2, "NambaTu", "omg", "kak@tak"),
    User(3, "Namba3", "dangYO", "kk@k")
  )
  var retweets: List[Retwit] = List(
    Retwit(1, twits(1)),
    Retwit(2, twits(2)),
  )

  before() {
    contentType = formats("json")
  }
  val authorizer = new JWTAuth()

  def validate() : Int = {
    val jwt = request.getHeader("token")
    val curId = authorizer.validateJWTToken(jwt)
    curId
  }


  post("/sessions"){
    val userData = parsedBody.extract[LogIn]

    if(users.exists(x => x.nickname == userData.log && x.password == userData.pass))
      {
        val user = users.find(x => x.nickname == userData.log && x.password == userData.pass).get
        val jwtToken : String = authorizer.authorize(user)
//        var dd = authorizer.validateJWTToken(jwtToken)
        Ok(String.format("Token: %s", jwtToken))
      }else
      {
        NotAcceptable("Wrong login or password");
      }
  }

  get("/") {
    views.html.hello()
  }

  //create tweet
  //checked
  post("/twit") {
    val curId = validate()
    if(curId>0) {
      val message = parsedBody.extract[Message]
      val id = twits.foldLeft(0)((maxId, m) => if (m.id > maxId) m.id else maxId) + 1
      val newTwit = Twit(id, message.text, curId, Calendar.getInstance().getTime)
      twits = newTwit :: twits
      Ok(String.format("New twit: %s", newTwit))
    }else{
      "You are not logged in"
    }
  }

  //edit my twits
  //checked
  put("/twit/:id") {
    val currentUserId = validate()
    if(currentUserId>0) {
      val id = params("id").toInt
      val matchingIndex = twits.zipWithIndex.collect { case (Twit(`id`, _, _, _), i) => i }
      if (matchingIndex.isEmpty)
        NotFound("User not found")
      else {
        val index = matchingIndex.head
        val newUserData = parsedBody.extract[Message]
        if (twits(index).author == currentUserId) {
          twits(index).text = newUserData.text
          twits
        } else {
          "Her te v rilo"
        }
      }
    }else{
      "You are not logged in"
    }
  }

  //retweet
  //ckecked
  post("/twit/retweet") {
    val currentUserId = validate()
    if(currentUserId>0) {
      val retweetData = parsedBody.extract[RetwitModel]
      val twit = twits.filter(t => t.author == retweetData.authorId && t.id == retweetData.twitId)
      if(subscriptions.exists(s => s.subscriberId == currentUserId) && twit.nonEmpty){
        val retweet = Retwit(currentUserId,twit.head)
        if(!retweets.contains(retweet)) {
          retweets = retweet :: retweets
          retweets
        }else{
          "You cannot retwit message twice"
        }
      }else{
        "You cannot retwit this message"
      }
    }else{
      "You are not logged in"
    }
  }

  delete("/twit/retweet/:id"){
    val currentUserId = validate()
    if(currentUserId>0) {
      retweets = retweets.filter(rt => (rt.userId != currentUserId) || rt.twit.id!=params("id").toInt)
      retweets
    }else{
      "You are not logged in"
    }
  }

  //delete my twit with id
  //checked
  delete("/twit/:id") {
    val currentUserId = validate()
    if(currentUserId>0) {
      twits = twits.filter(m => (m.id != params("id").toInt) || (m.author != currentUserId))
      twits
    }else{
      "You are not logged in"
    }
  }

  //subscribe to user with id
  //checked
  post("/subscribe/:id") {
    val currentUserId = validate()
    if(currentUserId>0) {
      val to = params("id").toInt
      val subscribeTo = users.filter(u => u.id == to)
      if (subscribeTo.nonEmpty) {
        if (!subscriptions.exists(s => (s.subscriberId == currentUserId) && (s.subscribedId == to))) {
          subscriptions = Subscriber(currentUserId, to) :: subscriptions
        } else {
          "tipa otlovil oshibku"
        }
      }
      subscriptions
    }else{
      "You are not logged in"
    }
  }

  //get feed of other users
  //checked
  get("/feed/:id") {
    if(validate()>0) {
      val id = params("id").toInt
      val usersPosts = twits.filter(t => t.author == id)
      val retwited  =  retweets.filter(twit => twit.userId == id).map(r => r.twit)
      val total  = usersPosts ++ retwited
      total
    }else{
      "You are not logged in"
    }
  }

  //get my feed
  //checked
  get("/feed") {
    val currentUserId = validate()
    if(currentUserId>0) {
      val mySubscribtions = subscriptions.filter(s => s.subscriberId == currentUserId)
      val subsId = mySubscribtions.map(s => s.subscribedId)
      var feed = twits.filter(twit => subsId.contains(twit.author))
      val additional = retweets.filter(retwit => subsId.contains(retwit.userId)).map(r => r.twit)
      feed = additional ++ feed
      if (feed.isEmpty) {
        "Your feed is empty lul"
      } else {
        feed
      }
    }else{
      "You are not logged in"
    }
  }
}
