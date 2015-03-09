package com.mayreh.stone

import scala.util.matching.Regex.Match

/**
 * Created by hokada on 3/8/15.
 */
object <> {
  def unapplySeq(m: Match): Option[Seq[Option[String]]] =
    if (m.groupCount > 0) Some(1 to m.groupCount map (i => Option(m.group(i)))) else None
}
