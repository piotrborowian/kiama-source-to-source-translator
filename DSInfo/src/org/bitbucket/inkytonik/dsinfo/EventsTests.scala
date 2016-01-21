/**
 * This file is part of dsprofile.
 *
 * Copyright (C) 2012-2014 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2012-2014 Matthew Roberts, Macquarie University.
 *
 * dsprofile is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * dsprofile is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with dsprofile.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package org.bitbucket.inkytonik.dsprofile

import org.scalatest.{BeforeAndAfter, FunSuiteLike}

class EventsTests extends FunSuiteLike with BeforeAndAfter {

    import scala.collection.immutable.Seq

    before {
        Events.profiling = true
        Events.reset
        val i = Events.start (Seq (("one", 1), ("two", 2)))
        Events.finish (i)
        Events.profiling = false
    }

    after {
        Events.reset
    }

    test ("length is two") {
        assertResult (2) {Events.events.result ().length}
    }

    test ("length after reset") {
        Events.reset
        assertResult (0) {Events.events.result ().length}
    }

    test ("test event ids are unique") {
        Events.profiling = true
        assertResult (1000) {
            ((((1 to 1000).toList).map {i => Events.start ()}).toSet).size
        }
    }

    test ("wrapper interface") {
        def do_it () {
            val i = 1
        }
        Events.profiling = true
        Events.wrap (Seq ("one" -> 10, "two" -> 20)) {do_it}
        Events.profiling = false
        assertResult (4) {Events.events.result.length}
    }

}
