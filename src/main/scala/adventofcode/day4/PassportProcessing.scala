package adventofcode.day4

import scala.io.Source

case class Passport(byr : Option[Int],
                    iyr : Option[Int],
                    eyr : Option[Int],
                    hgt : Option[String],
                    hcl : Option[String],
                    ecl : Option[String],
                    pid : Option[String],
                    cid : Option[String])

object PassportProcessing {

  type PassportContent = List[(String, String)]

  def validateBirthYear(passport: Passport): Boolean =
    passport.byr.exists(byr => byr >= 1920 && byr <= 2002)

  def validateIssueYear(passport: Passport): Boolean =
    passport.iyr.exists(iyr => iyr >= 210 && iyr <= 2020)

  def validateExpirationYear(passport: Passport): Boolean =
    passport.eyr.exists(eyr => eyr >= 2020 && eyr <= 2030)

  def validateHeight(passport: Passport) : Boolean = passport.hgt match {
    case Some(hgt) => {
      val require = "cm" :: "in" :: Nil
      val measureRequire = Map("cm" -> (150, 193), "in" -> (59, 76))

      require.filter(x => passport.hgt.contains(x)) match {
        case Nil => false
        case List(x) =>
          val height = hgt.split(x).head.toInt
          height >= measureRequire(x)._1 && height <= measureRequire(x)._2
      }
    }

    case None => false
  }

  def validateHairColor(passport : Passport) : Boolean =
    passport.hcl.filter(hcl => hcl.length == 7)
      .filter(hcl => hcl.head == '#')
      .map(hcl => hcl.drop(0))
      .exists(hcl => hcl.forall(x => x.isDigit))

  def validateEyeColor(passport: Passport) : Boolean = passport.eyr match {
    case Some(eyr) => {
      val require = "amb" :: "blu" :: "brn" :: "gry" :: "grn" :: "hzl" :: "oth" :: Nil
      require.contains(eyr)
    }

    case None => false
  }

  def validatePassportID(passport: Passport): Boolean =
    passport.pid.exists(pid => pid.length == 9 && pid.forall(x => x.isDigit))

  def passportContainsRequireFields(passport : List[(String, String)]): Boolean = {
    val requiredFields = "byr" :: "iyr" :: "eyr" :: "hgt" :: "hcl" :: "ecl" :: "pid" :: Nil
    val fieldsInPassword = passport.map(x => x._1)

    requiredFields.intersect(fieldsInPassword).length >= requiredFields.length
  }

  def getPassportFromContent(passportInfo: PassportContent) : Passport = {
    def getFieldValue(fieldName : String) : Option[String] =
      passportInfo.find(x => x._1 == fieldName).map(x => x._2)

    Passport(
      getFieldValue("byr").map(x => x.toInt),
      getFieldValue("iyr").map(x => x.toInt),
      getFieldValue("eyr").map(x => x.toInt),
      getFieldValue("hgt"),
      getFieldValue("hcl"),
      getFieldValue("ecl"),
      getFieldValue("pid"),
      getFieldValue("cid")
    )
  }

  def getPart1(info: List[PassportContent]): Int = 0 // TODO

  def getPart2(info: List[PassportContent]): Int = 0 // TODO

  def main(args: Array[String]): Unit = {

    val bufferedResource = Source.fromResource("day4.txt")
    val input = bufferedResource.getLines().toList
    bufferedResource.close()


    val contents = input.map(x => if (x == "") "\n" else x)
      .reduce((a,b) => a + " " + b)
      .split("\n ")
      .map(x => x.split(" ").sorted.toList)
      .map(x => x.map {
        y => y.split(':') match { case Array(a, b) => (a, b) }
      })
      .toList

    println(getPart1(contents))
    println(getPart2(contents))
  }
}
