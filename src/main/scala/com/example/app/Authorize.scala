package com.example.app
import authentikat.jwt._

class JWTAuth() {
  val header = JwtHeader("HS256")
  val secret = "AlphaTeam"

  def authorize(user:User): String ={
    val claimsSet = JwtClaimsSet(Map("username" -> user.nickname, "id" -> user.id))
    val jwt: String = JsonWebToken(header, claimsSet, secret)
    jwt
  }

  def validate(token:String): Boolean ={
    JsonWebToken.validate(token,secret)
  }
  def validateJWTToken(token: String): Int = {
    val isValid = JsonWebToken.validate(token, secret)

    if (isValid) {
      val claims: Option[Map[String, String]] = token match {
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