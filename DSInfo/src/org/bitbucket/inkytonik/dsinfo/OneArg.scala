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

case class Arg (i : Int)

case class OneArg (name : String, a : Arg)

object OneArgMaker {

    import scala.language.experimental.macros
    import scala.reflect.macros.Context
    import DSInfo.makeCallWithName

    def onearg (a : Arg) : OneArg =
        macro makeOneArgWithName

    def makeOneArgWithName (c : Context) (a : c.Expr[Arg]) : c.Expr[OneArg] =
        makeCallWithName (c, "org.bitbucket.inkytonik.dsinfo.OneArg.apply")

}
