package com.mayreh.stone

/**
 * Created by hokada on 3/6/15.
 */
class ParseException(msg: String) extends Exception(msg) {
  def this() = this("")
}
