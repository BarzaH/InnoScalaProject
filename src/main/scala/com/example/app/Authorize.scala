package com.example.app
import authentikat.jwt._
import org.json4s.JsonDSL._
import org.json4s.JValue
import com.example.app.MyScalatraServlet

class JWTAuth(user: User) {
  val header = JwtHeader("HS256")
  val secret = "AlphaTeam"

  val claimsSet = JwtClaimsSet(Map("username" -> user.nickname, "id" -> user.id))
  val jwt: String = JsonWebToken(header, claimsSet, secret)

//  def getJWTToken(user: User): String = {
//
//    jwt
//  }

  def validateJWTToken(token: String): Int = {
    val isValid = JsonWebToken.validate(token, secret)

    if (isValid) {
      val claims: Option[Map[String, String]] = jwt match {
        case JsonWebToken(header, claimsSet, signature) =>
          claimsSet.asSimpleMap.toOption
        case x =>
          None
      }

      if (claims == null){
        0
      }
      else {
        val id = claims.getOrElse(Map.empty[String, String])("id").toInt
        id
      }
    }
    else {
      0
    }
  }
}