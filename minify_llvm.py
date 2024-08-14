import re
import sys

def minify_llvm_ir(input_file, output_file):
    with open(input_file, 'r') as file:
        content = file.read()

    content = re.sub(r';.*', '', content)

    content = re.sub(r'\s+', ' ', content).strip()

    with open(output_file, 'w') as file:
        file.write(content)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python minify_llvm.py [input_file] [output_file]")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    minify_llvm_ir(input_file, output_file)