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

/**
 * The interface and default implementation for a profiler's access to profiled
 * values. Clients can provide implementations of these operations to override
 * or customise the treatment of values for specific purposes. E.g., derived
 * dimensions can be added by overriding `dimValue` and defaulting to the
 * version here.
 */
trait Values {

    import Events.{Dimension, Dimensions, Value}
    import scala.collection.immutable.Seq

    /**
     * A record of execution of some particular type of action during profiling.
     * It represents a `Start` event followed some time later by a `Finish` event
     * with the same id. `stime` is the difference between times of these two
     * events, not including any time spent in descendants (self time). `allDescs`
     * is all descendants. `dimensions` is the union of the two event's dimensions.
     *
     * Descendant records are the records that resulted from execution that happened
     * while the execution represented by this record was going on. I.e., the
     * descendant executions were nested between the start and finish events that
     * resulted in this record.
     *
     * The descendant values are in the sceond argument list so that they don't
     * participate in the hashCode of the record. They slow things down a lot...
     */
    case class Record (stime : Long, dimensions : Dimensions) (
                       val dirDescs : List[Record], val allDescs : List[Record])

    /**
     * Given a record, an event type and a dimension name, return the value of
     * the record at that dimension. By default, the name is just looked up in
     * the record's intrinsic dimensions. If the dimension is not known, an error
     * message string is returned as the value. Override this method to add derived
     * dimensions or to change the interpretation of intrinsic ones.
     */
    def dimValue (record : Record, dim : Dimension) : Value =
        if (record.dimensions contains dim)
            record.dimensions (dim)
        else
            "unknown dimension \"" + dim + "\""

    /**
     * Check the event type of a record by looking for its `event` intrinsic
     * dimension and comparing to the given `event` string. Return true if
     * it is of the given event type, false otherwise.
     */
    def isEventType (record : Record, eventtype : String) : Boolean =
        (record.dimensions contains "event") &&
            (record.dimensions ("event") == eventtype)

    /**
     * Check for an intrinsic dimension `needed` on `record`, while looking for
     * a value of the derived dimension `dim`. If `needed` is found, get its value
     * and pass it to `f`, using the `f`'s return value as the value of the `dim`
     * dimension. Otherwise, use `error` to report the missing dimension. If
     * `eventtype` is empty, accept any event.
     */
    def checkFor (record : Record, dim : Dimension, eventtype : String,
                  needed : Dimension) (f : Value => Value) : Value =
        if (record.dimensions contains "event")
            if (eventtype.isEmpty || (record.dimensions ("event") == eventtype))
                if (record.dimensions contains needed)
                    f (record.dimensions (needed))
                else
                    "\"" + needed + "\" dimension not available in \"" +
                        eventtype + "\" event, so \"" + dim +
                        "\" cannot be derived"
            else
                "record is not of event \"" + eventtype + "\", so \"" +
                    dim + "\" cannot be derived from \"" + needed + "\""
        else
            "record does not have \"event\" dimension, so \"" + dim +
                "\" cannot be derived from \"" + needed + "\""

    /**
     * Convert an arbitrary value to a string using `toString`. Override this
     * method to customise how values are shown.
     */
    def valueToString (a : Value) : String =
        if (a == null)
            "null"
        else
            a.toString

    /**
     * Whether or not to print the profile tables. Some special profile
     * dimensions produce their own output rather than using the default
     * table style. They can turn this flag off in the `startReport`
     * method to inhibit the default output.
     */
    var printTables = true

    /**
     * Called when the report writing is about to start. By default this
     * method does nothing. The method is passed the dimension names that
     * have been requested so that can react to them.
     */
    def startReport (dimensionNames : Seq[Dimension]) {
        // Do nothing
    }

    /**
     * Called when the report writing is about to finish. By default this
     * method does nothing.
     */
    def finishReport () {
        // Do nothing
    }

    /**
     * Send string to the profile output.
     */
    def output (str : String) {
        System.err.print (str)
    }

    /**
     * Send string and a newline to the profile output. Default: print an
     * empty line.
     */
    def outputln (str : String = "") {
        System.err.println (str)
    }

}
