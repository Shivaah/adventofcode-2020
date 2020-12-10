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
    passport.iyr.exists(iyr => iyr >= 2010 && iyr <= 2020)

  def validateExpirationYear(passport: Passport): Boolean =
    passport.eyr.exists(eyr => eyr >= 2020 && eyr <= 2030)

  def validateHeight(passport: Passport) : Boolean = passport.hgt match {
    case Some(hgt) => {
      val require = "cm" :: "in" :: Nil
      val measureRequire = Map("cm" -> (150, 193), "in" -> (59, 76))

      require.filter(x => hgt.contains(x)) match {
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
      .map(hcl => hcl.drop(1))
      .exists(hcl => hcl.forall(x => x.isLetterOrDigit))

  def validateEyeColor(passport: Passport) : Boolean = passport.ecl match {
    case Some(eyr) => {
      val required = "amb" :: "blu" :: "brn" :: "gry" :: "grn" :: "hzl" :: "oth" :: Nil
      required.contains(eyr)
    }

    case None => false
  }

  def validatePassportID(passport: Passport): Boolean =
    passport.pid.exists(pid => pid.length == 9 && pid.forall(x => x.isDigit))

  def passportContainsRequireFields(passport : List[(String, String)]): Boolean = {
    val requiredFields = "byr" :: "iyr" :: "eyr" :: "hgt" :: "hcl" :: "ecl" :: "pid" :: Nil
    val fieldsInPassport = passport.map(x => x._1)

    requiredFields.intersect(fieldsInPassport).length >= requiredFields.length
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

  def getPart1(contents: List[PassportContent]): Int =
    contents.count(x => passportContainsRequireFields(x))

  def getPart2(contents: List[PassportContent]): Int = {
    val passports = contents.map(x => getPassportFromContent(x))

    passports.filter(x => validateBirthYear(x))
      .filter(x => validateExpirationYear(x))
      .filter(x => validateIssueYear(x))
      .filter(x => validateEyeColor(x))
      .filter(x => validateHairColor(x))
      .filter(x => validateHeight(x))
      .count(x => validatePassportID(x))
  }

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
