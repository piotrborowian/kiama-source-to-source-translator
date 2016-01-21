/**
 * This file is part of dsinfo.
 *
 * Copyright (C) 2013-2014 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2013-2014 Matthew Roberts, Macquarie University.
 *
 * dsname is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * dsname is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with dsinfo.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package org.bitbucket.inkytonik.dsinfo

case class NamedInt (name : String, i : Int) {

    import scala.language.experimental.macros

    def increment (n : Int) : NamedInt =
        macro NamedIntMaker.incrementMacro

    def increment (name : String, n : Int) : NamedInt =
        NamedInt (name, i + n)

    def decrement (n : Int) : NamedInt =
        macro NamedIntMaker.decrementMacro

    def decrement (name : String, n : Int) : NamedInt =
        NamedInt (name, i - n)

}

object NamedIntMaker {

    import scala.reflect.macros.blackbox.Context
    import DSInfo.{makeCallWithName, makeThisCallWithName}

    // Use explicit "this" method spec

    def incrementMacro (c : Context) (n : c.Expr[Int]) : c.Expr[NamedInt] =
        makeCallWithName (c, "this.increment")

    // Use implicit "this" method spec

    def decrementMacro (c : Context) (n : c.Expr[Int]) : c.Expr[NamedInt] =
        makeThisCallWithName (c)

}
