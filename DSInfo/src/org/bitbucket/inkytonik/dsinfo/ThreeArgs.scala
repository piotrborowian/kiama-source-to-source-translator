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

case class ThreeArgs[T] (name : String, i : T, b : Boolean, s : String)

object ThreeArgsMaker {

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox.Context
    import DSInfo.makeCallWithName

    def threeargs[T] (i : T) (b : Boolean, s : String) : ThreeArgs[T] =
        macro makeThreeArgsWithName[T]

    def makeThreeArgsWithName[T] (c : Context) (i : c.Expr[T]) (b : c.Expr[Boolean], s : c.Expr[String]) : c.Expr[ThreeArgs[T]] =
        makeCallWithName (c)

    def threeargs[T] (name : String, i : T) (b : Boolean, s : String) : ThreeArgs[T] =
        ThreeArgs (name, i, b, s)

}
