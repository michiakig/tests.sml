(*
 * a very lightweight testing framework
 *)

structure Test =
   struct
      (* general index, requiring an equality predicate and a show fn  *)
      type 'a genidx = {eq: 'a Eq.t, show: 'a Show.t}
      (* eqtype index, requiring only a show fn *)
      type ''a polyidx = {show: ''a Show.t}

      (* type shorthand for thunks, used to delay test case execution *)
      type 'a thunk = unit -> 'a

      type 'a testcase = {actual: 'a, expected: 'a} thunk
      datatype result = Pass | Fail of string
      (* comparator knows how to evaluate a test case for success/failure *)
      type 'a comparator = 'a testcase -> result

      (* a suite is a list of thunks returning (name, verbose, concise) *)
      type suite = (string * string * string) thunk list

      (* comparator fn constructors *)
      val genEq: 'a genidx -> 'a comparator =
       fn {eq, show} =>
          fn testcase =>
             (let
                val {actual, expected} = testcase ()
             in
                if eq (actual, expected)
                   then Pass
                else Fail ("expected: " ^ show expected ^ ", but got: " ^ show actual)
             end) handle e => Fail ("uncaught exception: " ^ exnName e)

      val polyEq: ''a polyidx -> ''a comparator =
       fn {show} => genEq {eq=op =,show=show}

      (* actually evaluates test *)
      val run: string * 'a comparator * 'a testcase -> string * string * string =
          fn (name, cmp, testcase) =>
             case cmp testcase of
                 Pass => (name, "pass", ".")
               | Fail msg => (name, "FAIL " ^ msg, "F")

      val concat: suite list -> suite = List.concat

      (* make a singleton test suite *)
      val single: string * 'a comparator * 'a testcase -> suite =
       fn t => [fn () => run t]

      (* make a test suite from a group of testcases *)
      val group: string * 'a comparator * 'a testcase list -> suite =
       fn (name, assert, cases) =>
          map (fn c => fn () => run (name, assert, c)) cases

      local
         val bool = genEq {eq = Eq.bool, show = Show.bool}
      in
         (* makes test suite from boolean assertions... *)
         val assertTrue: string * (bool thunk) -> suite =
             fn (name, actual) => (* wrap user's thunk in a testcase and a thunk *)
                [fn () => run (name, bool, fn () => {actual = actual (), expected = true})]

         val assertFalse: string * (bool thunk) -> suite =
             fn (name, actual) =>
                [fn () => run (name, bool, fn () => {actual = actual (), expected = false})]

      end

      (* actually run a test suite, printing either verbose or concise results *)
      val runTestSuite: bool * suite -> unit =
       fn (verbose, suite) =>
          let
             fun p th =
                 case th () of
                     (n, v, c) => if verbose
                                     then print ("[" ^ n ^ "] " ^ v ^ "\n")
                                  else print c
          in
           (List.app p suite; print "\n")
          end

   end
