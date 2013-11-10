#!/usr/bin/env python
import subprocess,shlex,os,sys

# Authors: Bill Warner, Adam Weiss

class ParserTest:

    def __init__(self, exec_file, filename):
        self.exec_file = exec_file
        self.filename = filename
        self.expected_out_file = filename + ".out"
        self.expected_err_file = filename + ".err"

    def test(self):
        with open(self.filename, "r") as f:
            input_code = f.read()

        p = subprocess.Popen(shlex.split(self.exec_file), stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.PIPE)
        p.stdin.write(input_code)
        parser_out, parser_err = p.communicate()[:2]

        expected_out = ""
        with open(self.expected_out_file) as f:
            expected_out = f.read()

        expected_err = ""
        with open(self.expected_err_file) as f:
            expected_err = f.read()

        if expected_out != parser_out or expected_err != parser_err:
            raise Exception("test failed for {0}:".format(self.filename))

        return p.returncode

if __name__ == '__main__':
    test_types = sys.argv[1:] if len(sys.argv) > 1 else "parser validation cWriter setupBaseTests".split()
    failures = 0
    passes   = 0

    for test_type in test_types:
        exec_file = {"parser": "../setup.native", "validation": "../validationtest.native", "cWriter": "../cWriter.sh", "setupBaseTests": "../testBase.sh"}[test_type]
        for expectation in "expect_success expect_failure".split():
            for filename in ["{0}/{1}/{2}".format(test_type, expectation, f) for f in os.listdir("{0}/{1}".format(test_type,expectation))]:
                if not filename.endswith(".su") and not test_type == "setupBaseTests":
                    continue
                if not filename.endswith(".cpp") and test_type == "setupBaseTests":
                    continue
                if os.path.isfile(filename):
                    test_passed = "yup"
                    msg = ""
                    t = ParserTest(exec_file, filename)
                    try:
                        t.test()
                    except Exception as e:
                        test_passed = "nope"
                        msg = ": " + str(e)
                    if test_passed == "nope":
                        failures += 1
                    else:
                        passes += 1
                    print "{0}: {1}{2}".format(test_passed, filename, msg)
    sys.stderr.write("{0} tests passed\n".format( passes ))
    sys.stderr.write("{0} tests failed\n".format( failures ))
    
    sys.exit(-failures)
