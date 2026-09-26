(in-package :sextant/tests)

(in-suite :sextant-tests)

(test json-parse-numbers
  (is (eql 42 (json-parse "42")))
  (is (eql -7 (json-parse " -7 ")))
  (is (= 1500.0 (json-parse "1.5e3")))
  (is (= 0.0015 (json-parse "1.5E-3")))
  (is (= 300 (json-parse "3e2")))
  (is (= 1.5 (json-parse "1.5"))))

(test json-parse-strings-and-escapes
  (is (string= "a\"b\\c/d" (json-parse "\"a\\\"b\\\\c\\/d\"")))
  (is (string= "xaybz" (json-parse "\"x\\u0061y\\u0062z\"")))
  ;; Surrogate pairs combine into one character outside the BMP
  (is (string= "😀" (json-parse "\"\\ud83d\\ude00\"")))
  ;; Lone surrogates are rejected
  (signals error (json-parse "\"\\ud83d\""))
  (signals error (json-parse "\"\\ude00\""))
  (signals error (json-parse "\"unterminated")))

(test json-parse-containers
  (let ((obj (json-parse "{\"a\": 1, \"b\": [true, false, null], \"c\": {\"d\": \"x\"}}")))
    (is (= 1 (json-get obj "a")))
    (is (equal '(t :false nil) (json-get obj "b")))
    (is (string= "x" (json-get (json-get obj "c") "d"))))
  (is (equal '(1 2 3) (json-parse "[1, 2, 3]")))
  (is (null (json-parse "[]")))
  (is (null (json-parse "{}"))))

(test json-write-roundtrip
  (is (string= "null" (json-to-string nil)))
  (is (string= "[]" (json-to-string (json-empty-array))))
  (is (string= "true" (json-to-string t)))
  (is (string= "false" (json-to-string :false)))
  (is (string= "42" (json-to-string 42)))
  (is (string= "[1,2,3]" (json-to-string '(1 2 3))))
  (is (string= "{\"a\":1}" (json-to-string '(("a" . 1)))))
  ;; Strings are escaped
  (is (string= "\"a\\\"b\"" (json-to-string "a\"b")))
  (is (string= "\"a\\nb\"" (json-to-string (format nil "a~cb" #\Newline)))))

(test json-array-helper
  (is (eq +json-empty-array+ (json-array nil)))
  (is (equal '(1 2) (json-array '(1 2))))
  ;; Semantic-token-style empty payloads must serialize to [] not null
  (is (string= "{\"data\":[]}" (json-to-string (make-json-object "data" (json-array nil))))))
