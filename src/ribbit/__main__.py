
import csv
import argparse
import sys

order = [
  # "Input", 
  # "Utils", 
  "VM definitions", 
  "Primitives", 
  "Decode Symbol Table", 
  "Decode instruction graph", 
  "Execute RVM instructions"
]

def fixup_code(code, lang="py"):
    if check_code(code, lang):
        return code
    # try extracting code block
    fixed_code = extract_code_block(code, lang)
    
    if fixed_code and check_code(fixed_code, lang):
        return fixed_code
    
    # Try with no tag
    fixed_code = extract_code_block(code, lang, "")
    if fixed_code and check_code(fixed_code, lang):
        return fixed_code
    return False

def check_code(code, lang="py"):
    if lang != "py":
        return False

    try:
        compile(code, "", "exec")
        return True
    except Exception as e:
        return False

lang_to_tag = {
    "py": "python",
    "rb": "ruby",
    "js": "javascript",
}

def extract_code_block(code, lang="py", tag=None):
    if tag is None:
        tag = lang_to_tag[lang]
    import re
    pattern = r"```" + tag + r"\n(.*?)\n```"
    match = re.search(pattern, code, re.DOTALL)
    if match:
        return match.group(1)
    else:
        return False

class RVM:
    def __init__(self, parts, target):
        self.parts = parts
        self.target = target
    
    def replace_part(self, part_name, new_part):
        return RVM({**self.parts, part_name: new_part}, self.target)
    
    
    def run(self, RIBBIT_ROOT):
        import time
        import subprocess
        import os

        # Generate a random id to avoid collisions
        id = time.time_ns()
        FILENAME = f"gen_{id}.{self.target}"  
        FILEPATH=f"{RIBBIT_ROOT}/src/ribbit/gen/{FILENAME}"
        os.makedirs(f"{RIBBIT_ROOT}/src/ribbit/gen", exist_ok=True)

        full_content = "\n".join(map(lambda x : self.parts[x],order))
        with open(FILEPATH, "w") as file:
            file.write(full_content)

        timeout_expired = False
        
        try:
            val = subprocess.run(
              f'HOST={self.target} TEMP_DIR=".run.{id}" RSC_COMPILER="./rsc.exe --rvm {FILEPATH} -f+ arity-check -e original" RSC_MUST_TEST_FEATURES="," make check',
              cwd=f"{RIBBIT_ROOT}/src",
              shell=True,
              capture_output=True,
              text=True,
              timeout=60    
              )
        except subprocess.TimeoutExpired:
            timeout_expired = True

        os.makedirs(f"{RIBBIT_ROOT}/src/ribbit/logs", exist_ok=True)
        timeout_string = "TIMEOUT" if timeout_expired else ""
        log_file_name = f"{RIBBIT_ROOT}/src/ribbit/logs/logs.{id}{timeout_string}.txt"
        with open(log_file_name, "a") as file:
            file.write("==== NEW ENTRY ====\n")
            file.write("!!! full content\n")
            file.write(full_content + "\n")

            if timeout_expired:
                return -1, -1

            file.write("!!! stdout\n")
            file.write(val.stdout + "\n")
            file.write("!!! stderr\n")
            file.write(val.stderr + "\n")

        pattern = "!!!"
        elem = list(filter(lambda x : x.startswith(pattern), val.stdout.splitlines()))[0]
        sucess, num_test = list(map(int, elem.split()[1].split("/")))
        return sucess, num_test
        
def getRVM(target, RIBBIT_ROOT):
    csv_data = get_csv_data([target], RIBBIT_ROOT)
    parts = {}
    for data in csv_data:
        parts[data['Section Name']] = data['Content']

    return RVM(parts, target)

def get_csv_data(targets, RIBBIT_ROOT, tag="simple"):
    csv_data = []
    for target in targets:
        file_path = f"{RIBBIT_ROOT}/src/host/{target}/rvm.{tag}.{target}"
        with open(file_path, 'r') as file:
            content = file.read()
            current_buffer=""
            current_buffer_name=""
            for line in content.splitlines():
                if '#=# ' in line:

                    if current_buffer_name:
                        csv_data.append({
                            "Section Name" : current_buffer_name,
                            "Content" : current_buffer,
                            'Target': target,
                            'File Name': file_path
                        })

                    current_buffer_name = line.split('#=# ', 1)[1].strip()
                    current_buffer = ""
                else:
                    current_buffer = current_buffer + "\n" + line
            if current_buffer_name:
                csv_data.append({
                   "Section Name" : current_buffer_name,
                   "Content" : current_buffer,
                   'Target': target,
                   'File Name': file_path
                })
    return csv_data

def files_as_csv(targets, out, RIBBIT_ROOT, tag="simple"):
    csv_data = get_csv_data(targets, RIBBIT_ROOT, tag)

    writer = csv.DictWriter(out, fieldnames=list(csv_data[0].keys()))
    writer.writeheader()
    writer.writerows(csv_data)

def main():
    parser = argparse.ArgumentParser(description='Generate CSV with file name and content')
    parser.add_argument('--gen-csv', nargs='+', help='List of files to include in the CSV')
    parser.add_argument('--output', help='File to write the CSV to')
    parser.add_argument('--tag', help='Tag to use when extracting code blocks')
    args = parser.parse_args()

    if args.gen_csv:
        if args.output:
            with open(args.output, 'w') as out_file:
                files_as_csv(args.gen_csv, out_file, RIBBIT_ROOT = "..", tag=args.tag)
        else:
            files_as_csv(args.gen_csv, sys.stdout, RIBBIT_ROOT = "..", tag=args.tag)

if __name__ == '__main__':
    main()

    