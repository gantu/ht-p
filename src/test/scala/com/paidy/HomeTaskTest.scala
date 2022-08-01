package com.paidy

import org.scalatest.funsuite.AnyFunSuite

class HomeTaskTest extends AnyFunSuite{

  test("HomeTask.toOrderString") {
    assert(HomeTask.toOrderString(1) === "1st")
    assert(HomeTask.toOrderString(2) === "2nd")
    assert(HomeTask.toOrderString(3) === "3rd")
    assert(HomeTask.toOrderString(4) === "4th")
    assert(HomeTask.toOrderString(11) === "11th")
    assert(HomeTask.toOrderString(12) === "12th")
    assert(HomeTask.toOrderString(13) === "13th")
    assert(HomeTask.toOrderString(101) === "101st")
    assert(HomeTask.toOrderString(102) === "102nd")
    assert(HomeTask.toOrderString(103) === "103rd")
    assert(HomeTask.toOrderString(104) === "104th")
}

  test("HomeTask.countNumberOfSundays") {
    assert(HomeTask.countNumberOfSundays("30-07-2022","31-07-2022") === 1)
    assert(HomeTask.countNumberOfSundays("30-07-2022","30-07-2022") === 0)
    assert(HomeTask.countNumberOfSundays("01-08-2022","05-08-2022") === 0)
    assert(HomeTask.countNumberOfSundays("30-07-2022","30-08-2022") === 5)

  }

  test("HomeTask.obfuscateEmail") {
    assert(HomeTask.obfuscateEmail("test@test.com") === "t*****t@test.com")
    assertThrows[Exception](HomeTask.obfuscateEmail("testtest.com"))
    assert(HomeTask.obfuscateEmail("tt@test.com") === "t*****t@test.com")
    assert(HomeTask.obfuscateEmail("local-part@domain-name.com") === "l*****t@domain-name.com")
  }

  test("HomeTask.obfuscatePhone") {
    assert(HomeTask.obfuscatePhone("+1 123 456 7") === "+*-***-456-7")
    assert(HomeTask.obfuscatePhone("+44 123 456 789") === "+**-***-**6-789")
    assertThrows[Exception](HomeTask.obfuscatePhone("+12333440y"))
  }

}
