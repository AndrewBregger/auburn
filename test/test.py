#!/usr/bin/python3
import pathlib
import subprocess

run_path = "target/debug/aubc"

def collect_tests():
    return sorted(pathlib.Path('./test').glob('**/*.au'))

def run_test(test):
    with open("test/output.rrn", 'a') as f:
        p = subprocess.run([run_path, 'run', test], stdout=f, stderr=f)
        return p.returncode

def rebuild():
    return subprocess.run(['cargo', 'b'])

def main():
    rebuild()
    tests = collect_tests()
    open("test/output.rrn", 'w')
    for test in tests:
        print("Running ", test, end='\t')
        code = run_test(test)
        if code == 0:
            print('succcess')
        else:
            print(f'failed ({code})')

if __name__ == '__main__':
    main()