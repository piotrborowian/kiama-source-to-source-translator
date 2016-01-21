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
 * General profile support for domain-specific programs.
 */
trait Profiler extends Values {

    import Events._
    import java.lang.System.nanoTime
    import scala.collection.mutable.{HashMap, HashSet, ListBuffer}
    import scala.collection.immutable.Seq
    import scala.io.StdIn.readLine
    import scala.math.{pow, sqrt}

    var startTime = nanoTime

    /**
     * Return true if the profile tables should include timings.
     * Default: yes.
     */
    def includeTimings : Boolean =
        true

    /**
     * Process a profiling command-line option value. By convention we use the
     * `-p` option to introduce this value, but that is not checked here and the
     * option itself should not be present in the value. The option value is
     * interpreted as a comma-separated list of dimension names. The list of
     * names is returned. If the inupt value is the empty strnig, we return an
     * empty sequence, not a sequence containing an empty string.
     */
    def parseProfileOption (value : String) : Seq[Dimension] =
        if (value.isEmpty)
            Seq ()
        else
            value.split (",").toIndexedSeq

    /**
     * Start profiling by turning on the profiling system, resetting the events
     * buffer and recording the start time. The `logging` argument specifies
     * whether event logging should be turned on or not (default: false).
     */
    def profileStart (logging : Boolean = false) {
        Events.profiling = true
        Events.logging = logging

        // Clear the event buffer
        Events.reset ()

        startTime = nanoTime
    }

    /**
     * Stop profiling and generate a profile report for the given dimension
     * names.
     */
    def profileStop (dimensionNames : Seq[Dimension]) {
        profileStop () (dimensionNames)
    }

    /**
     * Stop profiling and generate profile reports based on interactively
     * specified dimensions.
     */
    def profileStopInteractive () {
        val profiler = profileStop ()
        outputln ("Profiler: enter a comma-separated list of dimension names, then enter (:q to exit)")
        var ok = true
        while (ok) {
            output ("> ")
            val ln = readLine ()
            ok = ln != ":q"
            if (ok)
                profiler ((ln.split (",")).map {_.trim()}.toIndexedSeq)
        }
    }

    /**
     * Stop profiling by turning off the profiling system and recording the
     * total execution time. Return a function that can be used to generate
     * a profile report when given the relevant dimension names.
     */
    def profileStop () : Seq[Dimension] => Unit = {
        import scala.collection.mutable.Stack

        // Calculate total execution time to this point
        val totalTime = nanoTime - startTime

        // Reset the profiling system
        Events.profiling = false
        Events.logging = false

        // Get the final collection of events
        val allevents = Events.events.result ()

        // Calculate overhead per record
        val overheadPerRecord =
            if (allevents.isEmpty)
                0
            else
                Events.overhead / allevents.size / 2

        // Compute the execution records from the events

        // Stack of the start events we have seen but for which we have not seen finish events
        val startStack = new Stack[Event] ()

        // Stacks of record lists to hold the records that have been created since each start
        // event on stack was seen. The dummy entries are there so that root records have
        // somewhere to add themselves. The dir one is for direct descendants and the all
        // one is for all descendants.
        val dirDescsStack = Stack (new ListBuffer[Record] ())
        val allDescsStack = Stack (new ListBuffer[Record] ())

        // Process all of the event looking for matching start and finish events in
        // a LIFO structure
        for (e <- allevents) {
            e.kind match {
                case Start =>
                    startStack.push (e)
                    dirDescsStack.push (new ListBuffer[Record] ())
                    allDescsStack.push (new ListBuffer[Record] ())
                case Finish =>
                    if (startStack.isEmpty)
                        sys.error ("profile: empty stack looking for Start event for " +
                                   e.kind + " " + e.dimensions)
                    val es = startStack.pop ()
                    es.kind match {
                        case Start if (es.id == e.id) =>
                            val dirDescs = dirDescsStack.pop ().result ()
                            val allDescs = allDescsStack.pop ().result ()
                            val dtime = allDescs.map (_.stime).sum
                            val rtime = (e.time - es.time - dtime - overheadPerRecord) max 0L
                            val r = Record (rtime, (e.dimensions ++ es.dimensions)) (dirDescs, allDescs)
                            dirDescsStack.top.append (r)
                            allDescsStack.top.appendAll (allDescs)
                            allDescsStack.top.append (r)
                        case _ =>
                            sys.error ("profile: found " + e.kind + " " + e.dimensions +
                                       " while looking for Start event for " + es.kind +
                                       " " + es.dimensions)
                    }
            }
        }
        if (!startStack.isEmpty)
            sys.error ("profile: event stack was not empty at end of grouping")

        // The top element of the allDescStack now contains all of the records
        val records = allDescsStack.pop ().result ()

        // Make a function that can print reports when given dimension names
        (dimensionNames : Seq[Dimension]) =>
            printReports (totalTime, dimensionNames, records)
    }

    /**
     * Profile `computation` along the given dimensions and produce reports.
     * If `dimensionsNames` is empty, run the computation and then enter an
     * interactive shell to allow reports to be produced. The `logging`
     * argument specifies whether event logging should be turned on or not
     * (default: false).
     */
    def profile[T] (computation : => T, dimensionNames : Seq[Dimension],
                    logging : Boolean = false) : T = {
        profileStart (logging)
        val computedResult = computation
        if (dimensionNames.isEmpty)
            profileStopInteractive ()
        else
            profileStop (dimensionNames)
        computedResult
    }

    /**
     * Print a trace of the events for which a given predicate is true. If the
     * predicate is omitted it defaults to one that is always true.
     */
    def trace (predicate : Event => Boolean = (_ => true)) {
        events.map (event =>
            if (predicate (event))
                outputln (event.toString)
        )
    }

    /**
     * Run `computation` and report timings. `computation` is first run `init`
     * times to warm up the JVM. Then it is run `n` more times. The `discard`
     * biggest and smallest values are then discarded and the resulting values
     * are printed.
     */
    def time[T] (computation : => T, warmup : Int = 10, n : Int = 24, discard : Int = 2) {

        /**
         * Print timing value summary information with mean, variance, max and min.
         */
        def printTimings (a : Array[Long]) {
            outputln ("Num samples     = %d".format (n))
            outputln ("Warmup samples  = %d".format (warmup))
            outputln ("Discard samples = %d".format (discard))
            outputln ("Include samples = %d".format (a.length))
            outputln ("Min             = %d".format (a.min))
            outputln ("Max             = %d".format (a.max))
            val m = mean (a)
            val s = stddev (a)
            outputln ("Mean            = %.2f".format (m))
            outputln ("Std dev         = %.2f".format (s))
            outputln ("Coeff var       = %.2f".format (s / m))
        }

        /**
         * Discard the `discard` biggest and smallest samples in the given array.
         * If there aren't at least `2 * dicsard` elements, return an empty array.
         */
        def discardSamples (a : Array[Long], discard : Int) : Array[Long] = {
            val length = a.length
            if (length >= 2 * discard)
                a.sorted.slice (discard, length - discard)
            else
                Array.empty
        }

        /**
         * Return the mean of a samples array.
         */
        def mean (a : Array[Long]) : Double =
            a.sum.toDouble / a.length

        /**
         * Return the standard deviation of a samples array.
         */
        def stddev (a : Array[Long]) : Double = {
            val m = mean (a)
            sqrt (a.map (v => pow (v - m, 2)).sum / (a.length - 1))
        }

        var i = 0
        while (i < warmup) {
            computation
            i = i + 1
        }
        val samples = Array.fill (n) (0L)
        i = 0
        while (i < n) {
            val startTime = nanoTime
            computation
            samples (i) = nanoTime - startTime
            i = i + 1
        }
        val timings = discardSamples (samples, discard)
        if (timings.length == 0)
            outputln ("No samples to print")
        else {
            outputln (timings.toList.toString)
            printTimings (timings)
        }

    }

    // Format a percentage
    def percent (v : Long, total : Long) : String =
        if (total == 0L)
            "100"
        else
            "%.1f".format (v * 100.0 / total)

    /**
     * Convert a nanoTime difference to ms.
     */
    def nanoToMs (nano : Long) : Long =
        nano / 1000000

    /**
     * Print the profiling report by summarising along the requested dimensions.
     * FIXME: avoid multiple passes through the data?
     */
    def printReports (totalTime : Long, dimensionNames : Seq[Dimension], records : List[Record]) {
        if (dimensionNames.nonEmpty) {

            val nrecords = records.length
            val profiledTime = records.foldLeft (0L) (_ + _.stime)

            startReport (dimensionNames)

            if (printTables) {
                outputln ()
                outputln ("%6d ms total time".format (nanoToMs (totalTime)))
                outputln ("%6d ms profiled time (%s%%)".format (
                               nanoToMs (profiledTime),
                               percent (profiledTime, totalTime)))
                outputln ("%6d profile records".format (nrecords))
                outputln ()
            }

            summariseAlongDims (dimensionNames, records, nrecords, profiledTime)

            finishReport ()

        }
    }

    /**
     * Summarise attribute evaluations along a list of dimensions.
     */
    def summariseAlongDims (dimensionNames : Seq[Dimension], records : List[Record],
                            nrecords : Int, profiledTime : Long) {

        import java.util.IdentityHashMap

        // A record for each value of the dimension
        class DimData {

            /**
             * The evaluations that are summarised here
             */
            val records = new ListBuffer[Record] ()

            /**
             * Total number of records associated with this value
             */
            var nrecords = 0

            /**
             * The descendant records that are accounted for here. We don't really
             */
            var allDescs = new HashSet[Record] ()

            /**
             * The total time in milliseconds of execution that is apportioned to
             * this value. I.e., the sum of the self time and the descendant time.
             */
            def time : Long =
                stime + dtime

            /**
             * The time in millieseconds that belong just to these evaluations,
             * not including any descendant ones.
             */
            var stime = 0L

            /**
             * The time in milliseconds that descendant records took.
             */
            var dtime = 0L

            /**
             * toString for debugging
             */
            override def toString : String =
                records.toString + ", " + nrecords + ", " + time + "," + dtime

        }

        /**
         * Bucket of a dimension value and its associated data.
         */
        type Bucket = (Value,DimData)

        /**
         * A list of buckets.
         */
        type Buckets = List[Bucket]

        /**
         * Aggregate the data in the event list according to the given dimension.
         * Return a sorted list of the aggregated data paired with the dimension
         * values.
         */
        def aggregate (records : List[Record], dimension : String) : Buckets = {

            // Map of data per dimension value
            val dimMap = new HashMap[Any,DimData] ()

            // Get a dimension map entry for a given key, creating it in
            // the map if it didn't exist already
            def dimEntry (a : Value) : DimData =
                dimMap.getOrElseUpdate (a, new DimData)

            // Get the dimension map entry for a given record, by looking up the
            // dimension value first
            def getDimEntry (r : Record) : DimData =
                dimEntry (dimValue (r, dimension))

            // Summarise by proecssing each of the root records and their descendants
            for (r <- records) {

                // Look up this record to get the appropriate bucket entry
                val entry = getDimEntry (r)

                // Record this record and its time with in the entry
                entry.records += r
                entry.nrecords += 1
                entry.stime += r.stime

                // Take into account the descendants. We group them into two groups,
                // those that are in the same bucket entry as r (and hence don't
                // count as descendant time) and those that aren't, which do. The
                // latter's self time is accumulated in the descendant time of the
                // entry. Avoid adding a descendant more than once if it is
                // encountered.
                for (d <- r.allDescs) {
                    val dentry = getDimEntry (d)
                    if ((entry != dentry) && (! (entry.allDescs contains d))) {
                        entry.dtime += d.stime
                        entry.allDescs.add (d)
                    }
                }

            }

            // Sort the data for in decreasing order of time expended. Ignore zero
            // count ones, these are the attributes that demanded the ones we are
            // interested in but are not included in this aggregation
            dimMap.toList.filter {
                _._2.nrecords != 0
            }.sortWith {
                _._2.time > _._2.time
            }

        }

        /**
         * List of footnote entries. If a value is to be printed in a table and
         * it is too long to fit comfortably on a line with the rest of the
         * information, we put its string representation here and print the
         * footnotes after the table.
         */
        val footnotes = new ListBuffer[String]

        /**
         * Footnote numbers indexed by entry so that we can reuse a number if
         * the entry shows up more than once.
         */
        val footnotesByValue = new IdentityHashMap[Value,Int] ()

        /**
         * If `a` is a new footnote, add its string representation `as` to the
         * footnotes and return its footnote number. Otherwise, return the footnote
         * number previously used for this value.
         */
        def addFootnote (a : Value, as : String) : Int =
            if (footnotesByValue containsKey a)
                footnotesByValue.get (a)
            else {
                footnotes.append (as)
                val i = footnotes.length
                footnotesByValue.put (a, i)
                i
            }

        /**
         * Format the value `a`. The opening gambit is to use the result of
         * `valueToString`. However, if that is too long or contains a newline,
         * we make that string into a footnote and use the footnote number
         * instead.
         * FIXME: "too long" is hard-coded
         */
        def formattedValue (a : Value) : String = {
            val as = valueToString (a)
            val width = if (includeTimings) 25 else 62
            if ((as contains '\n') || (as.length > width)) {
                val fn = addFootnote (a, as)
                "[%d]".format (fn)
            } else
                as
        }

        /**
         * Print an aggregated table given a list of its data.
         */
        def printtable (buckets : Buckets) {
            if (includeTimings) {
                outputln ("%6s%6s%6s%6s%6s%6s%6s%6s".format (
                   "Total", "Total", "Self", "Self", "Desc", "Desc", "Count", "Count"
                ))
                outputln ("%6s%6s%6s%6s%6s%6s%6s%6s".format (
                      "ms",   "  %",   "ms",    "%",   "ms",    "%",      "",     "%"
                ))
                var ctime = 0L
                for ((a, ad) <- buckets)
                    outputln ("%6d%6s%6d%6s%6d%6s%6d%6s  %s".format (
                        nanoToMs (ad.time), percent (ad.time, profiledTime),
                        nanoToMs (ad.stime), percent (ad.stime, profiledTime),
                        nanoToMs (ad.dtime), percent (ad.dtime, profiledTime),
                        ad.nrecords, percent (ad.nrecords, nrecords),
                        formattedValue (a))
                    )
                outputln ()
            } else {
                outputln ("%6s%6s".format ("Count", "Count"))
                outputln ("%6s%6s".format (     "",     "%"))
                for ((a, ad) <- buckets)
                    outputln ("%6d%6s  %s".format (
                        ad.nrecords, percent (ad.nrecords, nrecords),
                        formattedValue (a))
                    )
                outputln ()
            }
        }

        // Print tables for all of the reqeusted dimensions

        def aggregateAndPrint (dim : Dimension, records : List[Record], value : String) : Buckets = {
            if (printTables) {
                outputln ("By %s%s:".format (dim, if (value == "") "" else " for " + value))
                outputln ()
            }
            val buckets = aggregate (records, dim)
            if (printTables) {
                printtable (buckets)
            }
            buckets
        }

        def aggregateAndPrintAll (dimcount : Int, dimensionNames : Seq[Dimension],
                                  records : List[Record], value : String = "") {
            val dimension = dimensionNames.head
            val buckets = aggregateAndPrint (dimension, records, value)
            if (dimcount > 1)
                for ((a, ad) <- buckets) {
                    val formattedA = formattedValue (a)
                    val newValue =
                        if (value == "")
                            formattedA
                        else
                            value + " and " + formattedA
                    aggregateAndPrintAll (dimcount - 1, dimensionNames.tail,
                                          ad.records.result, newValue)
                }
        }

        aggregateAndPrintAll (dimensionNames.length, dimensionNames, records)

        // Print the footnote table. If the string contains a newline it is
        // started on the line after the footnote number, otherwise on the
        // same line.
        if (printTables)
            for ((as, i) <- footnotes.zipWithIndex) {
                val nlOrSpace = if (as contains '\n') "\n" else " "
                outputln ("[%d]%s%s".format (i + 1, nlOrSpace, as))
                outputln ()
            }

    }

    /**
     * Given a record, an event type and a dimension name, return the value of
     * the record at that dimension. This implementation provides standard
     * derived dimensions and defaults to just looking up intrinsic dimensions.
     * The supported derived dimensions are: `type` (the prefix of a Product
     * given by the record's `subject` dimension).
     */
    override def dimValue (record : Record, dim : Dimension) : Value =
        dim match {

            // `type` dimension is the node type of the record's subject.
            case "type" =>
                checkFor (record, dim, "", "subject") {
                    case p : Product => p.productPrefix
                    case _           => "unknown type"
                }

            // Otherwise, dispatch to the value to handle it as an intrinsic
            // dimension
            case _ =>
                super.dimValue (record, dim)

        }

}

/**
 * The default profiler, useful for testing via the REPL.
 */
object Profiler extends Profiler
