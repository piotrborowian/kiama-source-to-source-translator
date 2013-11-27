/**
 * This file is part of dsinfo.
 *
 * Copyright (C) 2013 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2013 Matthew Roberts, Macquarie University.
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

case class ThreeArgs (name : String, i : Int, b : Boolean, s : String)

object ThreeArgsMaker {

    import scala.language.experimental.macros
    import scala.reflect.macros.Context
    import DSInfo.makeCallWithName

    def threeargs (i : Int) (b : Boolean, s : String) : ThreeArgs =
        macro makeThreeArgsWithName

    def makeThreeArgsWithName (c : Context) (i : c.Expr[Int]) (b : c.Expr[Boolean], s : c.Expr[String]) : c.Expr[ThreeArgs] =
        makeCallWithName (c)

    def threeargs (name : String, i : Int) (b : Boolean, s : String) : ThreeArgs =
        ThreeArgs (name, i, b, s)

}
