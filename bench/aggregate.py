#!/bin/python3
import glob


def scheme_name_of(file_name: str):
    first_dash_idx = file_name.index("-")
    first_dot_idx = file_name.index(".", first_dash_idx)
    return file_name[first_dash_idx + 1: first_dot_idx]


def benchmark_name_of(command_line: str):
    chopped = command_line.split(" ")
    return chopped[1].split(".")[0]


def bench_triplet(benchmark_line: str):
    chopped = benchmark_line.split(",")
    bench = benchmark_name_of(chopped[0])
    mean_secs = float(chopped[1])
    stddev_secs = float(chopped[2])
    return bench, mean_secs, stddev_secs


def build_map(scheme_and_files):
    result_map = {}
    for tool, file in scheme_and_files:
        with open(file, "r") as fh:
            print("")
            lines = fh.readlines()
            benches = list(map(bench_triplet, lines[1:]))

            tool_map = {}

            for triplet in benches:
                tool_map[triplet[0]] = triplet

            result_map[tool] = tool_map

    return result_map


known = ["rvm", "rvm3", "minischeme", "tinyscheme",
         "mitscm", "siod", "gsi", "bit", "pico"]


def existing_schemes(scheme):
    """
    Sort them in the order we want in the article.
    If not in the known list, append last
    :param scheme: the scheme impl name
    :return: the index of the scheme in our table
    """
    if scheme in known:
        return known.index(scheme)
    else:
        return len(known)


def print_global_table(benchmarks, results):
    schemes = sorted(results.keys(), key=existing_schemes)

    printable_schemes = list(schemes)
    printable_schemes.insert(0, "")
    max_len = max(max(map(lambda scheme_name: len(scheme_name), printable_schemes)),
                  max(map(lambda bench_name: len(bench_name), benchmarks)))

    printable_schemes = list(
        map(lambda scheme_name: str(scheme_name).center(int(max_len * 1.2), " "), printable_schemes))

    space = "    "
    print(space.join(printable_schemes))

    for bench in benchmarks:
        line = bench.ljust(max_len)

        for impl in schemes:
            line += space
            mean = results[impl][bench][1]
            stddev = results[impl][bench][2]
            rsd = (stddev * 100) / mean
            total_l = int(max_len * 1.2)
            plus_or_minus = "~$\\pm$"
            printable_mean = f"{mean:0.2f}"
            printable_rsd = f"{rsd:0.1f}\\%"

            if impl == "rvm3" or impl == "pico" or impl == "bit":
                line += f"{printable_mean}s{plus_or_minus}{printable_rsd}".rjust(
                    total_l)
            else:
                mean_of_rvm3 = results["rvm3"][bench][1]
                factor = mean / mean_of_rvm3
                relative_factor = f"${factor:.1f}\\times$"
                line += f"{relative_factor}{plus_or_minus}{printable_rsd}".rjust(
                    total_l)
        print(line)

    # print(printable_schemes)
    # print(max_len)


def print_benchmark_table(benchmark: str, results):
    schemes = sorted(results.keys())
    printable_schemes = list(schemes)
    printable_schemes.insert(0, "")
    max_len = max(max(map(lambda scheme_name: len(scheme_name), printable_schemes)),
                  max(map(lambda bench_name: len(bench_name), [benchmark])))

    space = "    "
    print(benchmark)
    for scheme in schemes:
        mean = f"{(results[scheme][benchmark][1]):03f}"
        line = f"{scheme.ljust(max_len, ' ')}{space}{mean.rjust(int(max_len * 1.2), ' ')}"
        print(line)


def main():
    csv_files = [file for file in glob.glob("*.csv")]
    scheme_and_files = list(map(lambda x: (scheme_name_of(x), x), csv_files))
    benchmarks = [file.split(".")[0] for file in glob.glob("*.scm")]
    result_map = build_map(scheme_and_files)
    print_global_table(benchmarks, result_map)


if __name__ == '__main__':
    main()