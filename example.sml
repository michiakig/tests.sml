structure Example =
struct

val rev = Test.single ("rev", Test.polyAssertEq {show = Show.list Show.int},
                       {expected = [3,2,1],
                        actual = List.rev [1,2,3]})

fun main _ = (Test.runTestSuite (true, rev); OS.Process.success)

end
