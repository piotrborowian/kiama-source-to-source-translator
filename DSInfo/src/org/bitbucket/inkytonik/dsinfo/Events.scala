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

object Events {

    import java.lang.System.nanoTime
    import scala.collection.mutable.ArrayBuffer
    import scala.collection.immutable.Seq

    /**
     * Flag to control whether profiling data is stored or not. When this
     * is false (the default) the event generation routines `start` and
     * `finish` can be called, but will not store anything. If you want
     * profiling, you need to turn this flag on explicitly, or by calling
     * the `Profiler.profiler` method which will turn the flag on while
     * a profiled computation is being evaluated.
     */
    var profiling = false

    /**
     * Flag to control whether received events are logged to standard
     * error or not. This option provides a way to get step-by-step
     * information about the computation. It is also possible to use the
     * profiler's `trace` facility, but that only works if the profiled
     * computation terminates. Logging can be used for computations that
     * are long running or end up in inifinite loops.
     */
    var logging = false

    /**
     * Supply of unique event identifiers.
     */
    private object uniqueId {

        /**
         * Unique id seed.
         */
        private var uid : Long = 0

        /**
         * Return the next unique id.
         */
        def apply () : Long = {
            uid = uid + 1
            uid
        }

        /**
         * Reset the seed to zero.
         */
        def reset () {
            uid = 0
        }

    }

    /**
     * The type of a dimension name.
     */
    type Dimension = String

    /**
     * The type of a dimension value.
     */
    type Value = Any

    /**
     * Type of collection of event dimension names and their values. Dimensions
     * are used when producing reports, which can summarise along one or more
     * dimensions.
     */
    type Dimensions = Map[Dimension,Value]

    /**
     * An empty dimensions collection.
     */
    val emptyDimensions : Dimensions = Map.empty

    /**
     * Pairs of dimension name and value.
     */
    type DimPair = (Dimension, Value)

    /**
     * Dimension pair sequence.
     */
    type DimPairs = Seq[DimPair]

    /**
     * Event types.
     */
    sealed abstract class EventKind
    case object Start extends EventKind
    case object Finish extends EventKind

    /**
     * Base class of profiling events.
     */
    class Event (val id : Long, val kind : EventKind, dimPairs : Seq[DimPair]) {

        /**
         * The dimensions of this event.
         */
        val dimensions : Dimensions =
            Map (dimPairs : _*)

        /**
         * The time in milliseconds when this event was created.
         */
        val time = nanoTime

        /**
         * Render an event in a readable fashion. Dimensions are canonicalised
         * by puting the `event` dimension first (if present) and sorting the
         * rest.
         */
        lazy val renderedString : String = {
            val eventDimension = dimensions.getOrElse ("event", "")
            val nonEventDimensions = dimensions - "event"
            val sortedDimensions = nonEventDimensions.map (_.toString).toArray.sorted
            "%5d: %-6s %10s %s".format (id, kind, eventDimension, sortedDimensions.mkString (" "))
        }

        override def toString : String =
            renderedString

    }

    /**
     * Event buffer.
     */
    val events = new ArrayBuffer[Event] ()

    /**
     * Overhead time spent while generating events. Will be amortised across all
     * events when the profile is produced.
     */
    var overhead = 0L

    /**
     * Reset the event buffer.
     */
    def reset () {
        events.clear ()
        uniqueId.reset ()
        overhead = 0L
    }

    /**
     * Generate a `Start` event with the given dimensions.
     * The dimension values are supplied as a sequence that is
     * not evaluated unless profiling or logging is being performed.
     */
    @inline
    def start (dimPairs : => Seq[DimPair] = Seq.empty) : Long = {
        val startTime = nanoTime
        if (profiling || logging) {
            val i = uniqueId ()
            val event = new Event (i, Start, dimPairs)
            if (profiling)
                events += event
            if (logging)
                println (event.toString)
            overhead += nanoTime - startTime
            i
        } else
            0
    }

    /**
     * Generate a `Finish` event which matches the start event
     * with the given id, and which has the given dimensions.
     * The dimension values are supplied as a sequence that is
     * not evaluated unless profiling or logging is being performed.
     */
    @inline
    def finish (i : Long, dimPairs : => Seq[DimPair] = Seq.empty) {
        val startTime = nanoTime
        if (profiling || logging) {
            val event = new Event (i, Finish, dimPairs)
            if (profiling)
                events += event
            if (logging)
                println (event.toString)
            overhead += nanoTime - startTime
        }
    }

    /**
     * Wrap an execution of `c` by a `Start` event defined by the dimension
     * values given by `dimPairs` and a corresponding `Finish` event with no
     * extra dimension values. The dimension values are supplied as a
     * sequence that is not evaluated unless profiling or logging is being
     * performed.
     */
    def wrap[T] (dimPairs : => Seq[DimPair]) (c : => T) : T = {
        val i = start (dimPairs)
        val r = c
        finish (i)
        r
    }

}
