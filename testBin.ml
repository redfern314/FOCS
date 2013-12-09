(* dredfern - binary adder TM - test suite

   All tests labeled testF should be rejected;
    all tests labeled testT should be accepted.

   Tests T9 and T10 will only succeed if your TM
    handles |w3|=/=|w2|=/=|w1|. They are commented.

   Tests F12, T7, and T8 are huge and will obscure output from the
    other tests, so they are commented by default.
*)
let testBin () =
    let bin = (binary_sum ()) in
    let testF0 = (run bin "##") in
    let testF1 = (run bin "1#1#") in
    let testF2 = (run bin "#1#1") in
    let testF3 = (run bin "1##1") in
    let testF4 = (run bin "!#!#!") in
    let testF5 = (run bin "000") in
    let testF6 = (run bin "000#000") in
    let testF7 = (run bin "000##000") in
    let testF8 = (run bin "001#000#000") in
    let testF9 = (run bin "011#001#000") in
    let testF10 = (run bin "011#001#001") in
    let testF11 = (run bin "110#001#001") in
    (* let testF12 = (run bin "11110#00111#10101") in *)
    let testT0 = (run bin "000#000#000") in
    let testT1 = (run bin "001#000#001") in
    let testT2 = (run bin "001#001#000") in
    let testT3 = (run bin "001#000#001") in
    let testT4 = (run bin "010#001#001") in
    let testT5 = (run bin "110#101#001") in
    let testT6 = (run bin "111#101#010") in
    (*let testT7 = (run bin "11100#00111#10101") in
    let testT8 = (run bin "1001011111#010110111#110101000") in
    let testT9 = (run bin "111#101#10") in
    let testT10 = (run bin "101#100#000001") in *)

    begin 
        if (testF0 || testF1 || testF2 || testF3 || testF4 || testF5 || 
            testF6 || testF7 || testF8 || testF9 || testF10 || testF11 (*|| testF12*)) then false
        else if ((not testT0) || (not testT1) || (not testT2) || (not testT3) || 
                    (not testT4) || (not testT5) || (not testT6) (*|| (not testT7) || 
                    (not testT8) || (not testT9) || (not testT10) *)) then false
        else true
    end
;;