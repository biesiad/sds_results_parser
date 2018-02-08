open OUnit
open Parser

let tests =
  "Parser" >::: [
    "is_sample_line" >:: (fun () -> 
        assert_equal false (valid_sample "");
        assert_equal false (valid_sample "abc");
        assert_equal false (valid_sample "0abc");
        assert_equal true (valid_sample "1	A1	01	SYBR...");
      );

    "sample_row" >:: (fun () -> 
        assert_equal "A" (sample_row "1	A1	01	SYBR...");
        assert_equal "B" (sample_row "123	B123	01	SYBR...");
      );

    "get_column_count" >:: (fun () -> 
       assert_equal 3 (column_count ["1	A1"; "2	A2"; "2	A3"; "4	B1"]);
       assert_equal 1 (column_count ["1	A1"; "4	B1"]);
       assert_equal 1 (column_count ["1	A1"]);
       assert_equal 0 (column_count []);
      );

    "get_column_count" >:: (fun () -> 
       assert_equal [[1;2;3];[4;5;6];[7;8;9]] (slice 3 [1;2;3;4;5;6;7;8;9]);
       assert_equal [[1];[2;3;4]] (slice 3 [1;2;3;4]);
       assert_equal [[1;2]] (slice 3 [1;2]);
       assert_equal [] (slice 3 []);
      );
  ]

let _ = run_test_tt tests
