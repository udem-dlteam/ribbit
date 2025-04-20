import java.io.*
import java.nio.file.Files
import kotlin.io.path.Path
import kotlin.io.path.nameWithoutExtension


data class Directive(val input: String, val expected: String)

fun getDirective(filename: String): Directive {
    for (path in Files.walk(Path("."))) {
        if (path.fileName.toString() == filename) {
            val lines = path
                .toFile()
                .readLines()

            var input = ""
            var expected = ""
            var collectExpected = false
            for (line in lines ) {
                val trimmed = line.dropWhile { ch -> ch==';' }
                if (trimmed.contains(":")) {
                    when (trimmed.split(":")[0]) {
                        "input" -> {
                            collectExpected=false
                            input = trimmed.split(":")[1]
                        }
                        "expected" -> collectExpected = true
                    }
                } else {
                    if (collectExpected) {
                        expected = "$expected$trimmed\n"
                    }
                }
            }
            return Directive(input, expected)
        }
    }
    TODO("path not found")
}

fun main(args: Array<String>) {
    var i = 0
    var pass = 0
    var fail = 0
    val stdout = System.out
    Files.list(Path("src/host/kt/testData"))
        .forEach { path ->
            val file = path.toFile()
            val line = file.readLines()[1]
            val bytecode = line.substring(1, line.length - 1)
            i += 1
            Input = bytecode
            pos = 0
            val output = ByteArrayOutputStream()
            System.setOut(PrintStream(output))


            val directive = getDirective(path.fileName.nameWithoutExtension)
            val excepted = directive.expected
            val input = directive.input


            stdin = InputStreamReader(input.byteInputStream())

            val result = runCatching {
                initVm()
            }
            System.setOut(stdout)




            if (result.isSuccess && excepted == output.toString()) {
                println("--------- Test ${path.fileName} succeeded --------------")
                pass += 1
            } else if (result.isFailure) {
                println("--------- Test ${path.fileName} fail --------------")
                result.exceptionOrNull()!!.printStackTrace()
                fail += 1
            } else {
                println("--------- Test ${path.fileName} fail --------------")
                fail += 1
                println("expected")
                println(excepted)
                println("got")
                println(output.toString())
            }
        }

    println("run $i test. $pass pass, $fail fail")

    ProcessBuilder(listOf(
        "pwd"
    )).redirectOutput(ProcessBuilder.Redirect.INHERIT)
        .start()
        .waitFor()
    //println(args.contentToString())
}
