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

object DSInfo {

    import scala.reflect.macros.blackbox.Context

    /**
     * The pattern that will be replaced by the macro name (`"\$macro"`).
     */
    val macroNamePat = "$macro"

    /**
     * As for `makeCallWithName`, but uses the method specifier "this." followed
     * by `macroNamePat`.
     */
    def makeThisCallWithName[T] (c : Context) : c.Expr[T] =
        makeCallWithName (c, s"this.$macroNamePat")

    /**
     * Make an AST fragment that calls the specified method with the name of
     * the enclosing `val` or `def` and the original arguments to the macro
     * call given by `c`.
     *
     * The method specifier has the following forms: If it is an unqualified
     * name, then the method with that name in the calling context will be
     * used. If it is a name qualified by object or package name, then the
     * method in the named object or package will be called. If it is qualified
     * and begins with `"this"`, then the method will be called on the same
     * object as the macro invocation.
     *
     * Before the method specifier is interpreted, all occurrences of the
     * string `macroNamePat` are replaced by the name of the macro.
     *
     * If the method specifiier is omitted, it defaults to `macroNamePat`.
     */
    def makeCallWithName[T] (c : Context, methodSpec : String = macroNamePat) : c.Expr[T] = {

        import c.{universe => u}
        import u._

        import scala.reflect.NameTransformer.{encode, LOCAL_SUFFIX_STRING}

        /*
         * Helper function to do the real work once the macro name and arguments
         * have been determined. `obj` is the object to which the macro was
         * applied. `macroName` is the name that was supplied in the call.
         * `args1` is the first argument list; the name is pre-pended to this
         * list. `argsn` is the other argument lists. They are passed unchanged.
         */
        def constructCall[U] (obj : c.Tree, macroName : Name, args1 : List[c.Tree],
                              argsn : List[List[c.Tree]]) : c.Expr[U] = {

            /*
             * The string of the macro name.
             */
            val macroNameStr = macroName.decodedName.toString

            /*
             * Is this tree this macro invocation?
             */
            def isThisInvocation (tree : c.Tree) : Boolean =
                tree.pos == c.enclosingPosition

            /*
             * If the given tree is a value definition that has this macro
             * application on the right-hand side, return its name, otherwise be
             * undefined.
             */
            val isThisVal : PartialFunction[c.Tree,String] = {

                case d @ ValDef (_, name, _, rhs) if isThisInvocation (rhs) =>
                    name.decodedName.toString

            }

            /*
             * Try to find this invocation in a `val` in a list of trees. If found,
             * return `Some (name)` where `name` is the name of the `val`, otherwise
             * return `None`.
             */
            def optFindValNameIn (body : List[c.Tree]) : Option[String] =
                body.collectFirst (isThisVal)

            /*
             * Try to find this invocation in a `val` in a list of trees. If found,
             * return the name of the `val`, otherwise return the macro name.
             */
            def getValNameIn (body : List[c.Tree]) : String =
                optFindValNameIn (body).getOrElse (macroNameStr)

            /**
             * Traverser that looks for the macro invocation as the right-hand side of a
             * value definition anywhere in a given tree. If found, record the name in
             * `optName`, which defaults to `None`.
             */
            class FindValDefTraverser extends Traverser {

                var optName : Option[String] = None

                override def traverse (tree : c.Tree) =
                    tree match {

                        case ValDef (_, valname, _, rhs) if isThisInvocation (rhs) =>
                            optName = Some (valname.decodedName.toString)

                        case _ =>
                            super.traverse (tree)

                    }

            }

            /*
             * Run a val def traverser on a list of trees and if a matching val def is found,
             * return the name of that def. Otherwise, return the macro name. The traversal
             * is breadth-first, so we will find the most common case of val defs at the top
             * of a template first, before descending deeper into the tree.
             */
            def getValDefNameInTrees (body : List[c.Tree]) : String = {
                val traverser = new FindValDefTraverser
                traverser.traverseTrees (body)
                traverser.optName.getOrElse (macroNameStr)
            }

            /*
             * The suffix used internally in the Scala compiler for the names of
             * lazy values. Doesn't appear in the public API at the moment but
             * leaks out when we get the name of lazy owner.
             */
            val LAZY_SUFFIX_STRING = "$lzy"

            /*
             * Find the name of the entity for which this macro application is
             * the right-hand side, or the macro name if one can't be found.
             * For some reason some symbol names come with a space on the end
             * of the name, and lazy values come with $lzy suffixes. If either
             * are there, trim the unwanted suffix.
             */
            def nameOfEnclosing : String = {
                val str = c.internal.enclosingOwner.name.decodedName.toString
                str.stripSuffix (LOCAL_SUFFIX_STRING).stripSuffix (LAZY_SUFFIX_STRING)
            }

            /*
             * Make the call, given a tree for the method.
             */
            def makeCall[V] (method : c.Tree) : c.Expr[V] = {

                // The base expression: the method applied to the first argument list
                // with the name pre-pended
                val base = Apply (method, Literal (Constant (nameOfEnclosing)) :: args1)

                // Wrap the base in as many applications as are needed to pass the
                // other argument lists (if any)
                val result = argsn.foldLeft (base) {
                                 case (t, a) => Apply (t, a)
                             }

                // Hack to avoid Scala bug SI-6743. Set the position of the result tree
                // to some position. This avoids a validation error if the -Yrangepos
                // option is given.
                c.Expr[V] (atPos (c.enclosingPosition) (result))

            }

            // Replace the macro name placeholder with the macro name
            val methodNameStr = methodSpec.replaceAllLiterally (macroNamePat, macroNameStr)

            // Build the method call
            methodNameStr.split ('.') match {

                case Array () =>
                    c.error (c.enclosingPosition,
                             s"makeCallWithName: illegal call with empty method name")
                    null

                case components =>

                    // Make the AST for the method reference
                    val head : c.Tree =
                        if (components.head == "this")
                            obj
                        else
                            Ident (TermName (encode (components.head)))
                    val method =
                        components.tail.foldLeft (head) {
                            case (t, s) => Select (t, TermName (encode (s)))
                        }

                    // println (s"method = $method")
                    // println (s"method = ${u.showRaw (method)}")

                    makeCall (method)

            }

        }

        // println (s"c.enclosingClass = ${c.enclosingClass}")
        // println (s"c.enclosingClass = ${u.showRaw (c.enclosingClass)}")

        // println (s"c.enclosingMethod = ${c.enclosingMethod}")
        // println (s"c.enclosingMethod = ${u.showRaw (c.enclosingMethod)}")

        // println (s"c.macroApplication = $c.macroApplication")
        // println (s"c.macroApplication = ${u.showRaw (c.macroApplication)}")

        // Extract macro name and arguments from the application and then
        // construct the call

        c.macroApplication match {

            // No arguments
            case Select (obj, macroName) =>
                constructCall (obj, macroName, Nil, Nil)

            // One argument list
            case Apply (Select (obj, macroName), args) =>
                constructCall (obj, macroName, args, Nil)

            // Two argument lists
            case Apply (Apply (Select (obj, macroName), args1), args2) =>
                constructCall (obj, macroName, args1, List (args2))

            // Three argument lists
            case Apply (Apply (Apply (Select (obj, macroName), args1), args2), args3) =>
                constructCall (obj, macroName, args1, List (args2, args3))

            // Four argument lists
            case Apply (Apply (Apply (Apply (Select (obj, macroName), args1), args2), args3), args4) =>
                constructCall (obj, macroName, args1, List (args2, args3, args4))

            // One argument list + type application
            case Apply (TypeApply (Select (obj, macroName), _), args1) =>
                constructCall (obj, macroName, args1, Nil)

            // Two arguments list + type application
            case Apply (Apply (TypeApply (Select (obj, macroName), _), args1), args2) =>
                constructCall (obj, macroName, args1, List (args2))

            // Three argument lists + type application
            case Apply (Apply (Apply (TypeApply (Select (obj, macroName), _), args1), args2), args3) =>
                constructCall (obj, macroName, args1, List (args2, args3))

            // Four argument lists + type application
            case Apply (Apply (Apply (Apply (TypeApply (Select (obj, macroName), _), args1), args2), args3), args4) =>
                constructCall (obj, macroName, args1, List (args2, args3, args4))

            case t =>
                c.error (c.enclosingPosition,
                         s"makeCallWithName: unexpected macro application structure ${u.showRaw (t)}")
                null

        }

    }

}
