{-# OPTIONS_GHC -w #-}
module Parser where
import qualified Token as T
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t8 t13 t14
	= HappyTerminal (T.Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (Program)
	| HappyAbsSyn5 (FuncDecl)
	| HappyAbsSyn6 ([Var])
	| HappyAbsSyn7 (Term)
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 (Lit)
	| HappyAbsSyn10 ([Term])
	| HappyAbsSyn11 ([Clause])
	| HappyAbsSyn12 (Pattern)
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 ([Pattern])

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,790) ([0,1,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,512,0,0,0,256,0,0,0,0,1024,0,0,0,0,0,0,32,0,0,0,0,30976,24453,0,9,48256,12226,40959,4,0,0,0,0,44832,3056,8192,1,0,0,32768,0,0,0,16384,0,0,0,8192,0,0,0,4096,0,0,0,2048,32768,49852,47,1152,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61952,48906,0,18,30976,24453,0,9,48256,12226,32768,4,0,0,0,0,44832,52210,10239,1,22416,62968,37887,0,0,1024,0,0,5604,63998,9471,0,6898,64703,4735,32768,0,0,0,0,0,0,64,0,0,0,32,0,0,0,16,0,8,0,0,55296,64555,65522,73,0,0,0,0,61952,48906,0,18,30976,24453,0,9,48256,12226,32768,4,24128,6113,16384,2,44832,3056,8192,1,22416,1528,36864,0,11208,764,18432,0,5604,382,9216,0,2802,191,4608,0,34169,95,2304,32768,49852,47,1152,16384,57694,23,576,8192,61615,11,288,36864,63575,65509,147,51200,64555,32770,72,58368,32277,18401,36,61952,48906,9200,18,30976,24453,4600,9,48256,12226,34928,4,24128,6113,17408,2,44832,3056,8704,1,22416,1528,37120,0,11208,764,18567,0,5604,33150,9283,0,2802,61631,4671,0,34169,64607,2335,32768,49852,47,1152,16384,57694,23,576,8192,61615,11,288,36864,63575,5,144,51200,64555,2,72,58368,32277,1,36,0,32,0,0,0,0,0,0,0,0,0,0,24128,6113,16384,2,0,0,0,1,0,0,8192,0,0,0,8192,0,0,0,0,0,32768,0,0,0,8192,80,2048,32768,50108,65327,1183,16384,57694,65431,607,8192,61631,65483,295,36864,63583,65509,147,51200,64555,65522,73,62464,32277,65529,36,61952,48906,0,18,30976,24453,0,9,48256,12226,32768,4,0,0,0,2,44832,3056,8192,1,2048,0,0,0,0,0,128,0,0,0,0,0,0,0,4096,0,8192,80,2048,0,4096,40,1024,0,0,0,0,0,0,0,0,36864,63575,5,144,0,0,0,0,58368,32277,65533,36,0,0,2,0,0,32768,0,0,0,4096,0,0,0,5120,0,2,0,2564,0,1,22416,1528,36864,0,11208,62204,18943,0,0,0,8192,0,2802,64703,4735,0,34169,65119,2367,32768,49852,65327,1183,0,32,0,0,8192,62127,65483,295,0,0,0,0,0,32768,2,64,0,0,0,0,0,0,0,0,0,0,0,0,0,10256,0,4,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2802,191,4608,0,34169,65119,2367,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTok","Program","FuncDecl","ArgList","Term","Binders","Lit","ListLit","Clauses","Pattern","Pattern1","Pattern2","Pattern2s","PatternsWithComma","def","':='","if","then","else","fun","exist","forall","for","'=>'","let","in","match","with","'|'","'#'","end","int","float","string","true","false","'('","')'","'['","']'","','","'||'","'&&'","'+'","'-'","'*'","'/'","'%'","'++'","'<'","'>'","'=='","'::'","';'","'@'","':'","'{'","'}'","'<-'","id","%eof"]
        bit_start = st Prelude.* 63
        bit_end = (st Prelude.+ 1) Prelude.* 63
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..62]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (17) = happyShift action_4
action_0 (4) = happyGoto action_2
action_0 (5) = happyGoto action_3
action_0 _ = happyReduce_1

action_1 _ = happyFail (happyExpListPerState 1)

action_2 (63) = happyAccept
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (17) = happyShift action_4
action_3 (4) = happyGoto action_6
action_3 (5) = happyGoto action_3
action_3 _ = happyReduce_1

action_4 (62) = happyShift action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (62) = happyShift action_8
action_5 (6) = happyGoto action_7
action_5 _ = happyReduce_4

action_6 _ = happyReduce_2

action_7 (18) = happyShift action_10
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (62) = happyShift action_8
action_8 (6) = happyGoto action_9
action_8 _ = happyReduce_4

action_9 _ = happyReduce_5

action_10 (19) = happyShift action_13
action_10 (22) = happyShift action_14
action_10 (23) = happyShift action_15
action_10 (24) = happyShift action_16
action_10 (25) = happyShift action_17
action_10 (27) = happyShift action_18
action_10 (29) = happyShift action_19
action_10 (34) = happyShift action_20
action_10 (35) = happyShift action_21
action_10 (36) = happyShift action_22
action_10 (37) = happyShift action_23
action_10 (38) = happyShift action_24
action_10 (39) = happyShift action_25
action_10 (41) = happyShift action_26
action_10 (59) = happyShift action_27
action_10 (62) = happyShift action_28
action_10 (7) = happyGoto action_11
action_10 (9) = happyGoto action_12
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (19) = happyShift action_13
action_11 (22) = happyShift action_14
action_11 (23) = happyShift action_15
action_11 (24) = happyShift action_16
action_11 (25) = happyShift action_17
action_11 (27) = happyShift action_18
action_11 (29) = happyShift action_19
action_11 (34) = happyShift action_20
action_11 (35) = happyShift action_21
action_11 (36) = happyShift action_22
action_11 (37) = happyShift action_23
action_11 (38) = happyShift action_24
action_11 (39) = happyShift action_25
action_11 (41) = happyShift action_26
action_11 (44) = happyShift action_41
action_11 (45) = happyShift action_42
action_11 (46) = happyShift action_43
action_11 (47) = happyShift action_44
action_11 (48) = happyShift action_45
action_11 (49) = happyShift action_46
action_11 (50) = happyShift action_47
action_11 (51) = happyShift action_48
action_11 (52) = happyShift action_49
action_11 (53) = happyShift action_50
action_11 (54) = happyShift action_51
action_11 (55) = happyShift action_52
action_11 (56) = happyShift action_53
action_11 (59) = happyShift action_27
action_11 (62) = happyShift action_28
action_11 (7) = happyGoto action_40
action_11 (9) = happyGoto action_12
action_11 _ = happyReduce_3

action_12 _ = happyReduce_29

action_13 (19) = happyShift action_13
action_13 (22) = happyShift action_14
action_13 (23) = happyShift action_15
action_13 (24) = happyShift action_16
action_13 (25) = happyShift action_17
action_13 (27) = happyShift action_18
action_13 (29) = happyShift action_19
action_13 (34) = happyShift action_20
action_13 (35) = happyShift action_21
action_13 (36) = happyShift action_22
action_13 (37) = happyShift action_23
action_13 (38) = happyShift action_24
action_13 (39) = happyShift action_25
action_13 (41) = happyShift action_26
action_13 (59) = happyShift action_27
action_13 (62) = happyShift action_28
action_13 (7) = happyGoto action_39
action_13 (9) = happyGoto action_12
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (62) = happyShift action_38
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (62) = happyShift action_37
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (62) = happyShift action_36
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (62) = happyShift action_35
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (62) = happyShift action_34
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (19) = happyShift action_13
action_19 (22) = happyShift action_14
action_19 (23) = happyShift action_15
action_19 (24) = happyShift action_16
action_19 (25) = happyShift action_17
action_19 (27) = happyShift action_18
action_19 (29) = happyShift action_19
action_19 (34) = happyShift action_20
action_19 (35) = happyShift action_21
action_19 (36) = happyShift action_22
action_19 (37) = happyShift action_23
action_19 (38) = happyShift action_24
action_19 (39) = happyShift action_25
action_19 (41) = happyShift action_26
action_19 (59) = happyShift action_27
action_19 (62) = happyShift action_28
action_19 (7) = happyGoto action_33
action_19 (9) = happyGoto action_12
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_35

action_21 _ = happyReduce_34

action_22 _ = happyReduce_33

action_23 _ = happyReduce_36

action_24 _ = happyReduce_37

action_25 (19) = happyShift action_13
action_25 (22) = happyShift action_14
action_25 (23) = happyShift action_15
action_25 (24) = happyShift action_16
action_25 (25) = happyShift action_17
action_25 (27) = happyShift action_18
action_25 (29) = happyShift action_19
action_25 (34) = happyShift action_20
action_25 (35) = happyShift action_21
action_25 (36) = happyShift action_22
action_25 (37) = happyShift action_23
action_25 (38) = happyShift action_24
action_25 (39) = happyShift action_25
action_25 (41) = happyShift action_26
action_25 (59) = happyShift action_27
action_25 (62) = happyShift action_28
action_25 (7) = happyGoto action_32
action_25 (9) = happyGoto action_12
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (19) = happyShift action_13
action_26 (22) = happyShift action_14
action_26 (23) = happyShift action_15
action_26 (24) = happyShift action_16
action_26 (25) = happyShift action_17
action_26 (27) = happyShift action_18
action_26 (29) = happyShift action_19
action_26 (34) = happyShift action_20
action_26 (35) = happyShift action_21
action_26 (36) = happyShift action_22
action_26 (37) = happyShift action_23
action_26 (38) = happyShift action_24
action_26 (39) = happyShift action_25
action_26 (41) = happyShift action_26
action_26 (59) = happyShift action_27
action_26 (62) = happyShift action_28
action_26 (7) = happyGoto action_30
action_26 (9) = happyGoto action_12
action_26 (10) = happyGoto action_31
action_26 _ = happyReduce_39

action_27 (19) = happyShift action_13
action_27 (22) = happyShift action_14
action_27 (23) = happyShift action_15
action_27 (24) = happyShift action_16
action_27 (25) = happyShift action_17
action_27 (27) = happyShift action_18
action_27 (29) = happyShift action_19
action_27 (34) = happyShift action_20
action_27 (35) = happyShift action_21
action_27 (36) = happyShift action_22
action_27 (37) = happyShift action_23
action_27 (38) = happyShift action_24
action_27 (39) = happyShift action_25
action_27 (41) = happyShift action_26
action_27 (59) = happyShift action_27
action_27 (62) = happyShift action_28
action_27 (7) = happyGoto action_29
action_27 (9) = happyGoto action_12
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_30

action_29 (19) = happyShift action_13
action_29 (22) = happyShift action_14
action_29 (23) = happyShift action_15
action_29 (24) = happyShift action_16
action_29 (25) = happyShift action_17
action_29 (27) = happyShift action_18
action_29 (29) = happyShift action_19
action_29 (31) = happyShift action_77
action_29 (34) = happyShift action_20
action_29 (35) = happyShift action_21
action_29 (36) = happyShift action_22
action_29 (37) = happyShift action_23
action_29 (38) = happyShift action_24
action_29 (39) = happyShift action_25
action_29 (41) = happyShift action_26
action_29 (44) = happyShift action_41
action_29 (45) = happyShift action_42
action_29 (46) = happyShift action_43
action_29 (47) = happyShift action_44
action_29 (48) = happyShift action_45
action_29 (49) = happyShift action_46
action_29 (50) = happyShift action_47
action_29 (51) = happyShift action_48
action_29 (52) = happyShift action_49
action_29 (53) = happyShift action_50
action_29 (54) = happyShift action_51
action_29 (55) = happyShift action_52
action_29 (56) = happyShift action_53
action_29 (59) = happyShift action_27
action_29 (62) = happyShift action_28
action_29 (7) = happyGoto action_40
action_29 (9) = happyGoto action_12
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (19) = happyShift action_13
action_30 (22) = happyShift action_14
action_30 (23) = happyShift action_15
action_30 (24) = happyShift action_16
action_30 (25) = happyShift action_17
action_30 (27) = happyShift action_18
action_30 (29) = happyShift action_19
action_30 (34) = happyShift action_20
action_30 (35) = happyShift action_21
action_30 (36) = happyShift action_22
action_30 (37) = happyShift action_23
action_30 (38) = happyShift action_24
action_30 (39) = happyShift action_25
action_30 (41) = happyShift action_26
action_30 (43) = happyShift action_76
action_30 (44) = happyShift action_41
action_30 (45) = happyShift action_42
action_30 (46) = happyShift action_43
action_30 (47) = happyShift action_44
action_30 (48) = happyShift action_45
action_30 (49) = happyShift action_46
action_30 (50) = happyShift action_47
action_30 (51) = happyShift action_48
action_30 (52) = happyShift action_49
action_30 (53) = happyShift action_50
action_30 (54) = happyShift action_51
action_30 (55) = happyShift action_52
action_30 (56) = happyShift action_53
action_30 (59) = happyShift action_27
action_30 (62) = happyShift action_28
action_30 (7) = happyGoto action_40
action_30 (9) = happyGoto action_12
action_30 _ = happyReduce_40

action_31 (42) = happyShift action_75
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (19) = happyShift action_13
action_32 (22) = happyShift action_14
action_32 (23) = happyShift action_15
action_32 (24) = happyShift action_16
action_32 (25) = happyShift action_17
action_32 (27) = happyShift action_18
action_32 (29) = happyShift action_19
action_32 (34) = happyShift action_20
action_32 (35) = happyShift action_21
action_32 (36) = happyShift action_22
action_32 (37) = happyShift action_23
action_32 (38) = happyShift action_24
action_32 (39) = happyShift action_25
action_32 (40) = happyShift action_74
action_32 (41) = happyShift action_26
action_32 (44) = happyShift action_41
action_32 (45) = happyShift action_42
action_32 (46) = happyShift action_43
action_32 (47) = happyShift action_44
action_32 (48) = happyShift action_45
action_32 (49) = happyShift action_46
action_32 (50) = happyShift action_47
action_32 (51) = happyShift action_48
action_32 (52) = happyShift action_49
action_32 (53) = happyShift action_50
action_32 (54) = happyShift action_51
action_32 (55) = happyShift action_52
action_32 (56) = happyShift action_53
action_32 (59) = happyShift action_27
action_32 (62) = happyShift action_28
action_32 (7) = happyGoto action_40
action_32 (9) = happyGoto action_12
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (19) = happyShift action_13
action_33 (22) = happyShift action_14
action_33 (23) = happyShift action_15
action_33 (24) = happyShift action_16
action_33 (25) = happyShift action_17
action_33 (27) = happyShift action_18
action_33 (29) = happyShift action_19
action_33 (30) = happyShift action_73
action_33 (34) = happyShift action_20
action_33 (35) = happyShift action_21
action_33 (36) = happyShift action_22
action_33 (37) = happyShift action_23
action_33 (38) = happyShift action_24
action_33 (39) = happyShift action_25
action_33 (41) = happyShift action_26
action_33 (44) = happyShift action_41
action_33 (45) = happyShift action_42
action_33 (46) = happyShift action_43
action_33 (47) = happyShift action_44
action_33 (48) = happyShift action_45
action_33 (49) = happyShift action_46
action_33 (50) = happyShift action_47
action_33 (51) = happyShift action_48
action_33 (52) = happyShift action_49
action_33 (53) = happyShift action_50
action_33 (54) = happyShift action_51
action_33 (55) = happyShift action_52
action_33 (56) = happyShift action_53
action_33 (59) = happyShift action_27
action_33 (62) = happyShift action_28
action_33 (7) = happyGoto action_40
action_33 (9) = happyGoto action_12
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (18) = happyShift action_72
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (58) = happyShift action_71
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (58) = happyShift action_70
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (58) = happyShift action_69
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (26) = happyShift action_68
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (19) = happyShift action_13
action_39 (20) = happyShift action_67
action_39 (22) = happyShift action_14
action_39 (23) = happyShift action_15
action_39 (24) = happyShift action_16
action_39 (25) = happyShift action_17
action_39 (27) = happyShift action_18
action_39 (29) = happyShift action_19
action_39 (34) = happyShift action_20
action_39 (35) = happyShift action_21
action_39 (36) = happyShift action_22
action_39 (37) = happyShift action_23
action_39 (38) = happyShift action_24
action_39 (39) = happyShift action_25
action_39 (41) = happyShift action_26
action_39 (44) = happyShift action_41
action_39 (45) = happyShift action_42
action_39 (46) = happyShift action_43
action_39 (47) = happyShift action_44
action_39 (48) = happyShift action_45
action_39 (49) = happyShift action_46
action_39 (50) = happyShift action_47
action_39 (51) = happyShift action_48
action_39 (52) = happyShift action_49
action_39 (53) = happyShift action_50
action_39 (54) = happyShift action_51
action_39 (55) = happyShift action_52
action_39 (56) = happyShift action_53
action_39 (59) = happyShift action_27
action_39 (62) = happyShift action_28
action_39 (7) = happyGoto action_40
action_39 (9) = happyGoto action_12
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (7) = happyGoto action_40
action_40 (9) = happyGoto action_12
action_40 _ = happyReduce_13

action_41 (19) = happyShift action_13
action_41 (22) = happyShift action_14
action_41 (23) = happyShift action_15
action_41 (24) = happyShift action_16
action_41 (25) = happyShift action_17
action_41 (27) = happyShift action_18
action_41 (29) = happyShift action_19
action_41 (34) = happyShift action_20
action_41 (35) = happyShift action_21
action_41 (36) = happyShift action_22
action_41 (37) = happyShift action_23
action_41 (38) = happyShift action_24
action_41 (39) = happyShift action_25
action_41 (41) = happyShift action_26
action_41 (59) = happyShift action_27
action_41 (62) = happyShift action_28
action_41 (7) = happyGoto action_66
action_41 (9) = happyGoto action_12
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (19) = happyShift action_13
action_42 (22) = happyShift action_14
action_42 (23) = happyShift action_15
action_42 (24) = happyShift action_16
action_42 (25) = happyShift action_17
action_42 (27) = happyShift action_18
action_42 (29) = happyShift action_19
action_42 (34) = happyShift action_20
action_42 (35) = happyShift action_21
action_42 (36) = happyShift action_22
action_42 (37) = happyShift action_23
action_42 (38) = happyShift action_24
action_42 (39) = happyShift action_25
action_42 (41) = happyShift action_26
action_42 (59) = happyShift action_27
action_42 (62) = happyShift action_28
action_42 (7) = happyGoto action_65
action_42 (9) = happyGoto action_12
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (19) = happyShift action_13
action_43 (22) = happyShift action_14
action_43 (23) = happyShift action_15
action_43 (24) = happyShift action_16
action_43 (25) = happyShift action_17
action_43 (27) = happyShift action_18
action_43 (29) = happyShift action_19
action_43 (34) = happyShift action_20
action_43 (35) = happyShift action_21
action_43 (36) = happyShift action_22
action_43 (37) = happyShift action_23
action_43 (38) = happyShift action_24
action_43 (39) = happyShift action_25
action_43 (41) = happyShift action_26
action_43 (59) = happyShift action_27
action_43 (62) = happyShift action_28
action_43 (7) = happyGoto action_64
action_43 (9) = happyGoto action_12
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (19) = happyShift action_13
action_44 (22) = happyShift action_14
action_44 (23) = happyShift action_15
action_44 (24) = happyShift action_16
action_44 (25) = happyShift action_17
action_44 (27) = happyShift action_18
action_44 (29) = happyShift action_19
action_44 (34) = happyShift action_20
action_44 (35) = happyShift action_21
action_44 (36) = happyShift action_22
action_44 (37) = happyShift action_23
action_44 (38) = happyShift action_24
action_44 (39) = happyShift action_25
action_44 (41) = happyShift action_26
action_44 (59) = happyShift action_27
action_44 (62) = happyShift action_28
action_44 (7) = happyGoto action_63
action_44 (9) = happyGoto action_12
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (19) = happyShift action_13
action_45 (22) = happyShift action_14
action_45 (23) = happyShift action_15
action_45 (24) = happyShift action_16
action_45 (25) = happyShift action_17
action_45 (27) = happyShift action_18
action_45 (29) = happyShift action_19
action_45 (34) = happyShift action_20
action_45 (35) = happyShift action_21
action_45 (36) = happyShift action_22
action_45 (37) = happyShift action_23
action_45 (38) = happyShift action_24
action_45 (39) = happyShift action_25
action_45 (41) = happyShift action_26
action_45 (59) = happyShift action_27
action_45 (62) = happyShift action_28
action_45 (7) = happyGoto action_62
action_45 (9) = happyGoto action_12
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (19) = happyShift action_13
action_46 (22) = happyShift action_14
action_46 (23) = happyShift action_15
action_46 (24) = happyShift action_16
action_46 (25) = happyShift action_17
action_46 (27) = happyShift action_18
action_46 (29) = happyShift action_19
action_46 (34) = happyShift action_20
action_46 (35) = happyShift action_21
action_46 (36) = happyShift action_22
action_46 (37) = happyShift action_23
action_46 (38) = happyShift action_24
action_46 (39) = happyShift action_25
action_46 (41) = happyShift action_26
action_46 (59) = happyShift action_27
action_46 (62) = happyShift action_28
action_46 (7) = happyGoto action_61
action_46 (9) = happyGoto action_12
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (19) = happyShift action_13
action_47 (22) = happyShift action_14
action_47 (23) = happyShift action_15
action_47 (24) = happyShift action_16
action_47 (25) = happyShift action_17
action_47 (27) = happyShift action_18
action_47 (29) = happyShift action_19
action_47 (34) = happyShift action_20
action_47 (35) = happyShift action_21
action_47 (36) = happyShift action_22
action_47 (37) = happyShift action_23
action_47 (38) = happyShift action_24
action_47 (39) = happyShift action_25
action_47 (41) = happyShift action_26
action_47 (59) = happyShift action_27
action_47 (62) = happyShift action_28
action_47 (7) = happyGoto action_60
action_47 (9) = happyGoto action_12
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (19) = happyShift action_13
action_48 (22) = happyShift action_14
action_48 (23) = happyShift action_15
action_48 (24) = happyShift action_16
action_48 (25) = happyShift action_17
action_48 (27) = happyShift action_18
action_48 (29) = happyShift action_19
action_48 (34) = happyShift action_20
action_48 (35) = happyShift action_21
action_48 (36) = happyShift action_22
action_48 (37) = happyShift action_23
action_48 (38) = happyShift action_24
action_48 (39) = happyShift action_25
action_48 (41) = happyShift action_26
action_48 (59) = happyShift action_27
action_48 (62) = happyShift action_28
action_48 (7) = happyGoto action_59
action_48 (9) = happyGoto action_12
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (19) = happyShift action_13
action_49 (22) = happyShift action_14
action_49 (23) = happyShift action_15
action_49 (24) = happyShift action_16
action_49 (25) = happyShift action_17
action_49 (27) = happyShift action_18
action_49 (29) = happyShift action_19
action_49 (34) = happyShift action_20
action_49 (35) = happyShift action_21
action_49 (36) = happyShift action_22
action_49 (37) = happyShift action_23
action_49 (38) = happyShift action_24
action_49 (39) = happyShift action_25
action_49 (41) = happyShift action_26
action_49 (59) = happyShift action_27
action_49 (62) = happyShift action_28
action_49 (7) = happyGoto action_58
action_49 (9) = happyGoto action_12
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (19) = happyShift action_13
action_50 (22) = happyShift action_14
action_50 (23) = happyShift action_15
action_50 (24) = happyShift action_16
action_50 (25) = happyShift action_17
action_50 (27) = happyShift action_18
action_50 (29) = happyShift action_19
action_50 (34) = happyShift action_20
action_50 (35) = happyShift action_21
action_50 (36) = happyShift action_22
action_50 (37) = happyShift action_23
action_50 (38) = happyShift action_24
action_50 (39) = happyShift action_25
action_50 (41) = happyShift action_26
action_50 (59) = happyShift action_27
action_50 (62) = happyShift action_28
action_50 (7) = happyGoto action_57
action_50 (9) = happyGoto action_12
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (19) = happyShift action_13
action_51 (22) = happyShift action_14
action_51 (23) = happyShift action_15
action_51 (24) = happyShift action_16
action_51 (25) = happyShift action_17
action_51 (27) = happyShift action_18
action_51 (29) = happyShift action_19
action_51 (34) = happyShift action_20
action_51 (35) = happyShift action_21
action_51 (36) = happyShift action_22
action_51 (37) = happyShift action_23
action_51 (38) = happyShift action_24
action_51 (39) = happyShift action_25
action_51 (41) = happyShift action_26
action_51 (59) = happyShift action_27
action_51 (62) = happyShift action_28
action_51 (7) = happyGoto action_56
action_51 (9) = happyGoto action_12
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (19) = happyShift action_13
action_52 (22) = happyShift action_14
action_52 (23) = happyShift action_15
action_52 (24) = happyShift action_16
action_52 (25) = happyShift action_17
action_52 (27) = happyShift action_18
action_52 (29) = happyShift action_19
action_52 (34) = happyShift action_20
action_52 (35) = happyShift action_21
action_52 (36) = happyShift action_22
action_52 (37) = happyShift action_23
action_52 (38) = happyShift action_24
action_52 (39) = happyShift action_25
action_52 (41) = happyShift action_26
action_52 (59) = happyShift action_27
action_52 (62) = happyShift action_28
action_52 (7) = happyGoto action_55
action_52 (9) = happyGoto action_12
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (19) = happyShift action_13
action_53 (22) = happyShift action_14
action_53 (23) = happyShift action_15
action_53 (24) = happyShift action_16
action_53 (25) = happyShift action_17
action_53 (27) = happyShift action_18
action_53 (29) = happyShift action_19
action_53 (34) = happyShift action_20
action_53 (35) = happyShift action_21
action_53 (36) = happyShift action_22
action_53 (37) = happyShift action_23
action_53 (38) = happyShift action_24
action_53 (39) = happyShift action_25
action_53 (41) = happyShift action_26
action_53 (59) = happyShift action_27
action_53 (62) = happyShift action_28
action_53 (7) = happyGoto action_54
action_53 (9) = happyGoto action_12
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (19) = happyShift action_13
action_54 (22) = happyShift action_14
action_54 (23) = happyShift action_15
action_54 (24) = happyShift action_16
action_54 (25) = happyShift action_17
action_54 (27) = happyShift action_18
action_54 (29) = happyShift action_19
action_54 (34) = happyShift action_20
action_54 (35) = happyShift action_21
action_54 (36) = happyShift action_22
action_54 (37) = happyShift action_23
action_54 (38) = happyShift action_24
action_54 (39) = happyShift action_25
action_54 (41) = happyShift action_26
action_54 (44) = happyShift action_41
action_54 (45) = happyShift action_42
action_54 (46) = happyShift action_43
action_54 (47) = happyShift action_44
action_54 (48) = happyShift action_45
action_54 (49) = happyShift action_46
action_54 (50) = happyShift action_47
action_54 (51) = happyShift action_48
action_54 (52) = happyShift action_49
action_54 (53) = happyShift action_50
action_54 (54) = happyShift action_51
action_54 (55) = happyShift action_52
action_54 (56) = happyShift action_53
action_54 (59) = happyShift action_27
action_54 (62) = happyShift action_28
action_54 (7) = happyGoto action_40
action_54 (9) = happyGoto action_12
action_54 _ = happyReduce_26

action_55 (19) = happyShift action_13
action_55 (22) = happyShift action_14
action_55 (23) = happyShift action_15
action_55 (24) = happyShift action_16
action_55 (25) = happyShift action_17
action_55 (27) = happyShift action_18
action_55 (29) = happyShift action_19
action_55 (34) = happyShift action_20
action_55 (35) = happyShift action_21
action_55 (36) = happyShift action_22
action_55 (37) = happyShift action_23
action_55 (38) = happyShift action_24
action_55 (39) = happyShift action_25
action_55 (41) = happyShift action_26
action_55 (55) = happyShift action_52
action_55 (59) = happyShift action_27
action_55 (62) = happyShift action_28
action_55 (7) = happyGoto action_40
action_55 (9) = happyGoto action_12
action_55 _ = happyReduce_25

action_56 (19) = happyShift action_13
action_56 (22) = happyShift action_14
action_56 (23) = happyShift action_15
action_56 (24) = happyShift action_16
action_56 (25) = happyShift action_17
action_56 (27) = happyShift action_18
action_56 (29) = happyShift action_19
action_56 (34) = happyShift action_20
action_56 (35) = happyShift action_21
action_56 (36) = happyShift action_22
action_56 (37) = happyShift action_23
action_56 (38) = happyShift action_24
action_56 (39) = happyShift action_25
action_56 (41) = happyShift action_26
action_56 (46) = happyShift action_43
action_56 (47) = happyShift action_44
action_56 (48) = happyShift action_45
action_56 (49) = happyShift action_46
action_56 (50) = happyShift action_47
action_56 (51) = happyShift action_48
action_56 (52) = happyFail []
action_56 (53) = happyFail []
action_56 (54) = happyFail []
action_56 (55) = happyShift action_52
action_56 (59) = happyShift action_27
action_56 (62) = happyShift action_28
action_56 (7) = happyGoto action_40
action_56 (9) = happyGoto action_12
action_56 _ = happyReduce_24

action_57 (19) = happyShift action_13
action_57 (22) = happyShift action_14
action_57 (23) = happyShift action_15
action_57 (24) = happyShift action_16
action_57 (25) = happyShift action_17
action_57 (27) = happyShift action_18
action_57 (29) = happyShift action_19
action_57 (34) = happyShift action_20
action_57 (35) = happyShift action_21
action_57 (36) = happyShift action_22
action_57 (37) = happyShift action_23
action_57 (38) = happyShift action_24
action_57 (39) = happyShift action_25
action_57 (41) = happyShift action_26
action_57 (46) = happyShift action_43
action_57 (47) = happyShift action_44
action_57 (48) = happyShift action_45
action_57 (49) = happyShift action_46
action_57 (50) = happyShift action_47
action_57 (51) = happyShift action_48
action_57 (52) = happyFail []
action_57 (53) = happyFail []
action_57 (54) = happyFail []
action_57 (55) = happyShift action_52
action_57 (59) = happyShift action_27
action_57 (62) = happyShift action_28
action_57 (7) = happyGoto action_40
action_57 (9) = happyGoto action_12
action_57 _ = happyReduce_23

action_58 (19) = happyShift action_13
action_58 (22) = happyShift action_14
action_58 (23) = happyShift action_15
action_58 (24) = happyShift action_16
action_58 (25) = happyShift action_17
action_58 (27) = happyShift action_18
action_58 (29) = happyShift action_19
action_58 (34) = happyShift action_20
action_58 (35) = happyShift action_21
action_58 (36) = happyShift action_22
action_58 (37) = happyShift action_23
action_58 (38) = happyShift action_24
action_58 (39) = happyShift action_25
action_58 (41) = happyShift action_26
action_58 (46) = happyShift action_43
action_58 (47) = happyShift action_44
action_58 (48) = happyShift action_45
action_58 (49) = happyShift action_46
action_58 (50) = happyShift action_47
action_58 (51) = happyShift action_48
action_58 (52) = happyFail []
action_58 (53) = happyFail []
action_58 (54) = happyFail []
action_58 (55) = happyShift action_52
action_58 (59) = happyShift action_27
action_58 (62) = happyShift action_28
action_58 (7) = happyGoto action_40
action_58 (9) = happyGoto action_12
action_58 _ = happyReduce_22

action_59 (19) = happyShift action_13
action_59 (22) = happyShift action_14
action_59 (23) = happyShift action_15
action_59 (24) = happyShift action_16
action_59 (25) = happyShift action_17
action_59 (27) = happyShift action_18
action_59 (29) = happyShift action_19
action_59 (34) = happyShift action_20
action_59 (35) = happyShift action_21
action_59 (36) = happyShift action_22
action_59 (37) = happyShift action_23
action_59 (38) = happyShift action_24
action_59 (39) = happyShift action_25
action_59 (41) = happyShift action_26
action_59 (48) = happyShift action_45
action_59 (49) = happyShift action_46
action_59 (50) = happyShift action_47
action_59 (55) = happyShift action_52
action_59 (59) = happyShift action_27
action_59 (62) = happyShift action_28
action_59 (7) = happyGoto action_40
action_59 (9) = happyGoto action_12
action_59 _ = happyReduce_21

action_60 (19) = happyShift action_13
action_60 (22) = happyShift action_14
action_60 (23) = happyShift action_15
action_60 (24) = happyShift action_16
action_60 (25) = happyShift action_17
action_60 (27) = happyShift action_18
action_60 (29) = happyShift action_19
action_60 (34) = happyShift action_20
action_60 (35) = happyShift action_21
action_60 (36) = happyShift action_22
action_60 (37) = happyShift action_23
action_60 (38) = happyShift action_24
action_60 (39) = happyShift action_25
action_60 (41) = happyShift action_26
action_60 (55) = happyShift action_52
action_60 (59) = happyShift action_27
action_60 (62) = happyShift action_28
action_60 (7) = happyGoto action_40
action_60 (9) = happyGoto action_12
action_60 _ = happyReduce_20

action_61 (19) = happyShift action_13
action_61 (22) = happyShift action_14
action_61 (23) = happyShift action_15
action_61 (24) = happyShift action_16
action_61 (25) = happyShift action_17
action_61 (27) = happyShift action_18
action_61 (29) = happyShift action_19
action_61 (34) = happyShift action_20
action_61 (35) = happyShift action_21
action_61 (36) = happyShift action_22
action_61 (37) = happyShift action_23
action_61 (38) = happyShift action_24
action_61 (39) = happyShift action_25
action_61 (41) = happyShift action_26
action_61 (55) = happyShift action_52
action_61 (59) = happyShift action_27
action_61 (62) = happyShift action_28
action_61 (7) = happyGoto action_40
action_61 (9) = happyGoto action_12
action_61 _ = happyReduce_19

action_62 (19) = happyShift action_13
action_62 (22) = happyShift action_14
action_62 (23) = happyShift action_15
action_62 (24) = happyShift action_16
action_62 (25) = happyShift action_17
action_62 (27) = happyShift action_18
action_62 (29) = happyShift action_19
action_62 (34) = happyShift action_20
action_62 (35) = happyShift action_21
action_62 (36) = happyShift action_22
action_62 (37) = happyShift action_23
action_62 (38) = happyShift action_24
action_62 (39) = happyShift action_25
action_62 (41) = happyShift action_26
action_62 (55) = happyShift action_52
action_62 (59) = happyShift action_27
action_62 (62) = happyShift action_28
action_62 (7) = happyGoto action_40
action_62 (9) = happyGoto action_12
action_62 _ = happyReduce_18

action_63 (19) = happyShift action_13
action_63 (22) = happyShift action_14
action_63 (23) = happyShift action_15
action_63 (24) = happyShift action_16
action_63 (25) = happyShift action_17
action_63 (27) = happyShift action_18
action_63 (29) = happyShift action_19
action_63 (34) = happyShift action_20
action_63 (35) = happyShift action_21
action_63 (36) = happyShift action_22
action_63 (37) = happyShift action_23
action_63 (38) = happyShift action_24
action_63 (39) = happyShift action_25
action_63 (41) = happyShift action_26
action_63 (48) = happyShift action_45
action_63 (49) = happyShift action_46
action_63 (50) = happyShift action_47
action_63 (55) = happyShift action_52
action_63 (59) = happyShift action_27
action_63 (62) = happyShift action_28
action_63 (7) = happyGoto action_40
action_63 (9) = happyGoto action_12
action_63 _ = happyReduce_17

action_64 (19) = happyShift action_13
action_64 (22) = happyShift action_14
action_64 (23) = happyShift action_15
action_64 (24) = happyShift action_16
action_64 (25) = happyShift action_17
action_64 (27) = happyShift action_18
action_64 (29) = happyShift action_19
action_64 (34) = happyShift action_20
action_64 (35) = happyShift action_21
action_64 (36) = happyShift action_22
action_64 (37) = happyShift action_23
action_64 (38) = happyShift action_24
action_64 (39) = happyShift action_25
action_64 (41) = happyShift action_26
action_64 (48) = happyShift action_45
action_64 (49) = happyShift action_46
action_64 (50) = happyShift action_47
action_64 (55) = happyShift action_52
action_64 (59) = happyShift action_27
action_64 (62) = happyShift action_28
action_64 (7) = happyGoto action_40
action_64 (9) = happyGoto action_12
action_64 _ = happyReduce_16

action_65 (19) = happyShift action_13
action_65 (22) = happyShift action_14
action_65 (23) = happyShift action_15
action_65 (24) = happyShift action_16
action_65 (25) = happyShift action_17
action_65 (27) = happyShift action_18
action_65 (29) = happyShift action_19
action_65 (34) = happyShift action_20
action_65 (35) = happyShift action_21
action_65 (36) = happyShift action_22
action_65 (37) = happyShift action_23
action_65 (38) = happyShift action_24
action_65 (39) = happyShift action_25
action_65 (41) = happyShift action_26
action_65 (46) = happyShift action_43
action_65 (47) = happyShift action_44
action_65 (48) = happyShift action_45
action_65 (49) = happyShift action_46
action_65 (50) = happyShift action_47
action_65 (51) = happyShift action_48
action_65 (52) = happyShift action_49
action_65 (53) = happyShift action_50
action_65 (54) = happyShift action_51
action_65 (55) = happyShift action_52
action_65 (59) = happyShift action_27
action_65 (62) = happyShift action_28
action_65 (7) = happyGoto action_40
action_65 (9) = happyGoto action_12
action_65 _ = happyReduce_15

action_66 (19) = happyShift action_13
action_66 (22) = happyShift action_14
action_66 (23) = happyShift action_15
action_66 (24) = happyShift action_16
action_66 (25) = happyShift action_17
action_66 (27) = happyShift action_18
action_66 (29) = happyShift action_19
action_66 (34) = happyShift action_20
action_66 (35) = happyShift action_21
action_66 (36) = happyShift action_22
action_66 (37) = happyShift action_23
action_66 (38) = happyShift action_24
action_66 (39) = happyShift action_25
action_66 (41) = happyShift action_26
action_66 (45) = happyShift action_42
action_66 (46) = happyShift action_43
action_66 (47) = happyShift action_44
action_66 (48) = happyShift action_45
action_66 (49) = happyShift action_46
action_66 (50) = happyShift action_47
action_66 (51) = happyShift action_48
action_66 (52) = happyShift action_49
action_66 (53) = happyShift action_50
action_66 (54) = happyShift action_51
action_66 (55) = happyShift action_52
action_66 (59) = happyShift action_27
action_66 (62) = happyShift action_28
action_66 (7) = happyGoto action_40
action_66 (9) = happyGoto action_12
action_66 _ = happyReduce_14

action_67 (19) = happyShift action_13
action_67 (22) = happyShift action_14
action_67 (23) = happyShift action_15
action_67 (24) = happyShift action_16
action_67 (25) = happyShift action_17
action_67 (27) = happyShift action_18
action_67 (29) = happyShift action_19
action_67 (34) = happyShift action_20
action_67 (35) = happyShift action_21
action_67 (36) = happyShift action_22
action_67 (37) = happyShift action_23
action_67 (38) = happyShift action_24
action_67 (39) = happyShift action_25
action_67 (41) = happyShift action_26
action_67 (59) = happyShift action_27
action_67 (62) = happyShift action_28
action_67 (7) = happyGoto action_88
action_67 (9) = happyGoto action_12
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (19) = happyShift action_13
action_68 (22) = happyShift action_14
action_68 (23) = happyShift action_15
action_68 (24) = happyShift action_16
action_68 (25) = happyShift action_17
action_68 (27) = happyShift action_18
action_68 (29) = happyShift action_19
action_68 (34) = happyShift action_20
action_68 (35) = happyShift action_21
action_68 (36) = happyShift action_22
action_68 (37) = happyShift action_23
action_68 (38) = happyShift action_24
action_68 (39) = happyShift action_25
action_68 (41) = happyShift action_26
action_68 (59) = happyShift action_27
action_68 (62) = happyShift action_28
action_68 (7) = happyGoto action_87
action_68 (9) = happyGoto action_12
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (19) = happyShift action_13
action_69 (22) = happyShift action_14
action_69 (23) = happyShift action_15
action_69 (24) = happyShift action_16
action_69 (25) = happyShift action_17
action_69 (27) = happyShift action_18
action_69 (29) = happyShift action_19
action_69 (34) = happyShift action_20
action_69 (35) = happyShift action_21
action_69 (36) = happyShift action_22
action_69 (37) = happyShift action_23
action_69 (38) = happyShift action_24
action_69 (39) = happyShift action_25
action_69 (41) = happyShift action_26
action_69 (59) = happyShift action_27
action_69 (62) = happyShift action_28
action_69 (7) = happyGoto action_86
action_69 (9) = happyGoto action_12
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (19) = happyShift action_13
action_70 (22) = happyShift action_14
action_70 (23) = happyShift action_15
action_70 (24) = happyShift action_16
action_70 (25) = happyShift action_17
action_70 (27) = happyShift action_18
action_70 (29) = happyShift action_19
action_70 (34) = happyShift action_20
action_70 (35) = happyShift action_21
action_70 (36) = happyShift action_22
action_70 (37) = happyShift action_23
action_70 (38) = happyShift action_24
action_70 (39) = happyShift action_25
action_70 (41) = happyShift action_26
action_70 (59) = happyShift action_27
action_70 (62) = happyShift action_28
action_70 (7) = happyGoto action_85
action_70 (9) = happyGoto action_12
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (19) = happyShift action_13
action_71 (22) = happyShift action_14
action_71 (23) = happyShift action_15
action_71 (24) = happyShift action_16
action_71 (25) = happyShift action_17
action_71 (27) = happyShift action_18
action_71 (29) = happyShift action_19
action_71 (34) = happyShift action_20
action_71 (35) = happyShift action_21
action_71 (36) = happyShift action_22
action_71 (37) = happyShift action_23
action_71 (38) = happyShift action_24
action_71 (39) = happyShift action_25
action_71 (41) = happyShift action_26
action_71 (59) = happyShift action_27
action_71 (62) = happyShift action_28
action_71 (7) = happyGoto action_84
action_71 (9) = happyGoto action_12
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (19) = happyShift action_13
action_72 (22) = happyShift action_14
action_72 (23) = happyShift action_15
action_72 (24) = happyShift action_16
action_72 (25) = happyShift action_17
action_72 (27) = happyShift action_18
action_72 (29) = happyShift action_19
action_72 (34) = happyShift action_20
action_72 (35) = happyShift action_21
action_72 (36) = happyShift action_22
action_72 (37) = happyShift action_23
action_72 (38) = happyShift action_24
action_72 (39) = happyShift action_25
action_72 (41) = happyShift action_26
action_72 (59) = happyShift action_27
action_72 (62) = happyShift action_28
action_72 (7) = happyGoto action_83
action_72 (9) = happyGoto action_12
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (31) = happyShift action_82
action_73 (11) = happyGoto action_81
action_73 _ = happyReduce_42

action_74 _ = happyReduce_27

action_75 _ = happyReduce_38

action_76 (19) = happyShift action_13
action_76 (22) = happyShift action_14
action_76 (23) = happyShift action_15
action_76 (24) = happyShift action_16
action_76 (25) = happyShift action_17
action_76 (27) = happyShift action_18
action_76 (29) = happyShift action_19
action_76 (34) = happyShift action_20
action_76 (35) = happyShift action_21
action_76 (36) = happyShift action_22
action_76 (37) = happyShift action_23
action_76 (38) = happyShift action_24
action_76 (39) = happyShift action_25
action_76 (41) = happyShift action_26
action_76 (59) = happyShift action_27
action_76 (62) = happyShift action_28
action_76 (7) = happyGoto action_30
action_76 (9) = happyGoto action_12
action_76 (10) = happyGoto action_80
action_76 _ = happyReduce_39

action_77 (62) = happyShift action_79
action_77 (8) = happyGoto action_78
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (60) = happyShift action_103
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (61) = happyShift action_102
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_41

action_81 (33) = happyShift action_101
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (32) = happyShift action_97
action_82 (39) = happyShift action_98
action_82 (41) = happyShift action_99
action_82 (62) = happyShift action_100
action_82 (12) = happyGoto action_94
action_82 (13) = happyGoto action_95
action_82 (14) = happyGoto action_96
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (19) = happyShift action_13
action_83 (22) = happyShift action_14
action_83 (23) = happyShift action_15
action_83 (24) = happyShift action_16
action_83 (25) = happyShift action_17
action_83 (27) = happyShift action_18
action_83 (28) = happyShift action_93
action_83 (29) = happyShift action_19
action_83 (34) = happyShift action_20
action_83 (35) = happyShift action_21
action_83 (36) = happyShift action_22
action_83 (37) = happyShift action_23
action_83 (38) = happyShift action_24
action_83 (39) = happyShift action_25
action_83 (41) = happyShift action_26
action_83 (44) = happyShift action_41
action_83 (45) = happyShift action_42
action_83 (46) = happyShift action_43
action_83 (47) = happyShift action_44
action_83 (48) = happyShift action_45
action_83 (49) = happyShift action_46
action_83 (50) = happyShift action_47
action_83 (51) = happyShift action_48
action_83 (52) = happyShift action_49
action_83 (53) = happyShift action_50
action_83 (54) = happyShift action_51
action_83 (55) = happyShift action_52
action_83 (56) = happyShift action_53
action_83 (59) = happyShift action_27
action_83 (62) = happyShift action_28
action_83 (7) = happyGoto action_40
action_83 (9) = happyGoto action_12
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (19) = happyShift action_13
action_84 (22) = happyShift action_14
action_84 (23) = happyShift action_15
action_84 (24) = happyShift action_16
action_84 (25) = happyShift action_17
action_84 (27) = happyShift action_18
action_84 (29) = happyShift action_19
action_84 (34) = happyShift action_20
action_84 (35) = happyShift action_21
action_84 (36) = happyShift action_22
action_84 (37) = happyShift action_23
action_84 (38) = happyShift action_24
action_84 (39) = happyShift action_25
action_84 (41) = happyShift action_26
action_84 (44) = happyShift action_41
action_84 (45) = happyShift action_42
action_84 (46) = happyShift action_43
action_84 (47) = happyShift action_44
action_84 (48) = happyShift action_45
action_84 (49) = happyShift action_46
action_84 (50) = happyShift action_47
action_84 (51) = happyShift action_48
action_84 (52) = happyShift action_49
action_84 (53) = happyShift action_50
action_84 (54) = happyShift action_51
action_84 (55) = happyShift action_52
action_84 (56) = happyShift action_53
action_84 (57) = happyShift action_92
action_84 (59) = happyShift action_27
action_84 (62) = happyShift action_28
action_84 (7) = happyGoto action_40
action_84 (9) = happyGoto action_12
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (19) = happyShift action_13
action_85 (22) = happyShift action_14
action_85 (23) = happyShift action_15
action_85 (24) = happyShift action_16
action_85 (25) = happyShift action_17
action_85 (26) = happyShift action_91
action_85 (27) = happyShift action_18
action_85 (29) = happyShift action_19
action_85 (34) = happyShift action_20
action_85 (35) = happyShift action_21
action_85 (36) = happyShift action_22
action_85 (37) = happyShift action_23
action_85 (38) = happyShift action_24
action_85 (39) = happyShift action_25
action_85 (41) = happyShift action_26
action_85 (44) = happyShift action_41
action_85 (45) = happyShift action_42
action_85 (46) = happyShift action_43
action_85 (47) = happyShift action_44
action_85 (48) = happyShift action_45
action_85 (49) = happyShift action_46
action_85 (50) = happyShift action_47
action_85 (51) = happyShift action_48
action_85 (52) = happyShift action_49
action_85 (53) = happyShift action_50
action_85 (54) = happyShift action_51
action_85 (55) = happyShift action_52
action_85 (56) = happyShift action_53
action_85 (59) = happyShift action_27
action_85 (62) = happyShift action_28
action_85 (7) = happyGoto action_40
action_85 (9) = happyGoto action_12
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (19) = happyShift action_13
action_86 (22) = happyShift action_14
action_86 (23) = happyShift action_15
action_86 (24) = happyShift action_16
action_86 (25) = happyShift action_17
action_86 (26) = happyShift action_90
action_86 (27) = happyShift action_18
action_86 (29) = happyShift action_19
action_86 (34) = happyShift action_20
action_86 (35) = happyShift action_21
action_86 (36) = happyShift action_22
action_86 (37) = happyShift action_23
action_86 (38) = happyShift action_24
action_86 (39) = happyShift action_25
action_86 (41) = happyShift action_26
action_86 (44) = happyShift action_41
action_86 (45) = happyShift action_42
action_86 (46) = happyShift action_43
action_86 (47) = happyShift action_44
action_86 (48) = happyShift action_45
action_86 (49) = happyShift action_46
action_86 (50) = happyShift action_47
action_86 (51) = happyShift action_48
action_86 (52) = happyShift action_49
action_86 (53) = happyShift action_50
action_86 (54) = happyShift action_51
action_86 (55) = happyShift action_52
action_86 (56) = happyShift action_53
action_86 (59) = happyShift action_27
action_86 (62) = happyShift action_28
action_86 (7) = happyGoto action_40
action_86 (9) = happyGoto action_12
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (19) = happyShift action_13
action_87 (22) = happyShift action_14
action_87 (23) = happyShift action_15
action_87 (24) = happyShift action_16
action_87 (25) = happyShift action_17
action_87 (27) = happyShift action_18
action_87 (29) = happyShift action_19
action_87 (34) = happyShift action_20
action_87 (35) = happyShift action_21
action_87 (36) = happyShift action_22
action_87 (37) = happyShift action_23
action_87 (38) = happyShift action_24
action_87 (39) = happyShift action_25
action_87 (41) = happyShift action_26
action_87 (44) = happyShift action_41
action_87 (45) = happyShift action_42
action_87 (46) = happyShift action_43
action_87 (47) = happyShift action_44
action_87 (48) = happyShift action_45
action_87 (49) = happyShift action_46
action_87 (50) = happyShift action_47
action_87 (51) = happyShift action_48
action_87 (52) = happyShift action_49
action_87 (53) = happyShift action_50
action_87 (54) = happyShift action_51
action_87 (55) = happyShift action_52
action_87 (56) = happyShift action_53
action_87 (59) = happyShift action_27
action_87 (62) = happyShift action_28
action_87 (7) = happyGoto action_40
action_87 (9) = happyGoto action_12
action_87 _ = happyReduce_8

action_88 (19) = happyShift action_13
action_88 (21) = happyShift action_89
action_88 (22) = happyShift action_14
action_88 (23) = happyShift action_15
action_88 (24) = happyShift action_16
action_88 (25) = happyShift action_17
action_88 (27) = happyShift action_18
action_88 (29) = happyShift action_19
action_88 (34) = happyShift action_20
action_88 (35) = happyShift action_21
action_88 (36) = happyShift action_22
action_88 (37) = happyShift action_23
action_88 (38) = happyShift action_24
action_88 (39) = happyShift action_25
action_88 (41) = happyShift action_26
action_88 (44) = happyShift action_41
action_88 (45) = happyShift action_42
action_88 (46) = happyShift action_43
action_88 (47) = happyShift action_44
action_88 (48) = happyShift action_45
action_88 (49) = happyShift action_46
action_88 (50) = happyShift action_47
action_88 (51) = happyShift action_48
action_88 (52) = happyShift action_49
action_88 (53) = happyShift action_50
action_88 (54) = happyShift action_51
action_88 (55) = happyShift action_52
action_88 (56) = happyShift action_53
action_88 (59) = happyShift action_27
action_88 (62) = happyShift action_28
action_88 (7) = happyGoto action_40
action_88 (9) = happyGoto action_12
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (19) = happyShift action_13
action_89 (22) = happyShift action_14
action_89 (23) = happyShift action_15
action_89 (24) = happyShift action_16
action_89 (25) = happyShift action_17
action_89 (27) = happyShift action_18
action_89 (29) = happyShift action_19
action_89 (34) = happyShift action_20
action_89 (35) = happyShift action_21
action_89 (36) = happyShift action_22
action_89 (37) = happyShift action_23
action_89 (38) = happyShift action_24
action_89 (39) = happyShift action_25
action_89 (41) = happyShift action_26
action_89 (59) = happyShift action_27
action_89 (62) = happyShift action_28
action_89 (7) = happyGoto action_115
action_89 (9) = happyGoto action_12
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (19) = happyShift action_13
action_90 (22) = happyShift action_14
action_90 (23) = happyShift action_15
action_90 (24) = happyShift action_16
action_90 (25) = happyShift action_17
action_90 (27) = happyShift action_18
action_90 (29) = happyShift action_19
action_90 (34) = happyShift action_20
action_90 (35) = happyShift action_21
action_90 (36) = happyShift action_22
action_90 (37) = happyShift action_23
action_90 (38) = happyShift action_24
action_90 (39) = happyShift action_25
action_90 (41) = happyShift action_26
action_90 (59) = happyShift action_27
action_90 (62) = happyShift action_28
action_90 (7) = happyGoto action_114
action_90 (9) = happyGoto action_12
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (19) = happyShift action_13
action_91 (22) = happyShift action_14
action_91 (23) = happyShift action_15
action_91 (24) = happyShift action_16
action_91 (25) = happyShift action_17
action_91 (27) = happyShift action_18
action_91 (29) = happyShift action_19
action_91 (34) = happyShift action_20
action_91 (35) = happyShift action_21
action_91 (36) = happyShift action_22
action_91 (37) = happyShift action_23
action_91 (38) = happyShift action_24
action_91 (39) = happyShift action_25
action_91 (41) = happyShift action_26
action_91 (59) = happyShift action_27
action_91 (62) = happyShift action_28
action_91 (7) = happyGoto action_113
action_91 (9) = happyGoto action_12
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (62) = happyShift action_112
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (19) = happyShift action_13
action_93 (22) = happyShift action_14
action_93 (23) = happyShift action_15
action_93 (24) = happyShift action_16
action_93 (25) = happyShift action_17
action_93 (27) = happyShift action_18
action_93 (29) = happyShift action_19
action_93 (34) = happyShift action_20
action_93 (35) = happyShift action_21
action_93 (36) = happyShift action_22
action_93 (37) = happyShift action_23
action_93 (38) = happyShift action_24
action_93 (39) = happyShift action_25
action_93 (41) = happyShift action_26
action_93 (59) = happyShift action_27
action_93 (62) = happyShift action_28
action_93 (7) = happyGoto action_111
action_93 (9) = happyGoto action_12
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (26) = happyShift action_110
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (55) = happyShift action_109
action_95 _ = happyReduce_45

action_96 _ = happyReduce_47

action_97 (62) = happyShift action_108
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (32) = happyShift action_97
action_98 (39) = happyShift action_98
action_98 (41) = happyShift action_99
action_98 (62) = happyShift action_100
action_98 (12) = happyGoto action_107
action_98 (13) = happyGoto action_95
action_98 (14) = happyGoto action_96
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (32) = happyShift action_97
action_99 (39) = happyShift action_98
action_99 (41) = happyShift action_99
action_99 (62) = happyShift action_100
action_99 (12) = happyGoto action_105
action_99 (13) = happyGoto action_95
action_99 (14) = happyGoto action_96
action_99 (16) = happyGoto action_106
action_99 _ = happyReduce_53

action_100 _ = happyReduce_50

action_101 _ = happyReduce_12

action_102 (19) = happyShift action_13
action_102 (22) = happyShift action_14
action_102 (23) = happyShift action_15
action_102 (24) = happyShift action_16
action_102 (25) = happyShift action_17
action_102 (27) = happyShift action_18
action_102 (29) = happyShift action_19
action_102 (34) = happyShift action_20
action_102 (35) = happyShift action_21
action_102 (36) = happyShift action_22
action_102 (37) = happyShift action_23
action_102 (38) = happyShift action_24
action_102 (39) = happyShift action_25
action_102 (41) = happyShift action_26
action_102 (59) = happyShift action_27
action_102 (62) = happyShift action_28
action_102 (7) = happyGoto action_104
action_102 (9) = happyGoto action_12
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_28

action_104 (19) = happyShift action_13
action_104 (22) = happyShift action_14
action_104 (23) = happyShift action_15
action_104 (24) = happyShift action_16
action_104 (25) = happyShift action_17
action_104 (27) = happyShift action_18
action_104 (29) = happyShift action_19
action_104 (34) = happyShift action_20
action_104 (35) = happyShift action_21
action_104 (36) = happyShift action_22
action_104 (37) = happyShift action_23
action_104 (38) = happyShift action_24
action_104 (39) = happyShift action_25
action_104 (41) = happyShift action_26
action_104 (43) = happyShift action_124
action_104 (44) = happyShift action_41
action_104 (45) = happyShift action_42
action_104 (46) = happyShift action_43
action_104 (47) = happyShift action_44
action_104 (48) = happyShift action_45
action_104 (49) = happyShift action_46
action_104 (50) = happyShift action_47
action_104 (51) = happyShift action_48
action_104 (52) = happyShift action_49
action_104 (53) = happyShift action_50
action_104 (54) = happyShift action_51
action_104 (55) = happyShift action_52
action_104 (56) = happyShift action_53
action_104 (59) = happyShift action_27
action_104 (62) = happyShift action_28
action_104 (7) = happyGoto action_40
action_104 (9) = happyGoto action_12
action_104 _ = happyReduce_31

action_105 (43) = happyShift action_123
action_105 _ = happyReduce_54

action_106 (42) = happyShift action_122
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (40) = happyShift action_121
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (39) = happyShift action_98
action_108 (41) = happyShift action_99
action_108 (62) = happyShift action_100
action_108 (14) = happyGoto action_119
action_108 (15) = happyGoto action_120
action_108 _ = happyReduce_51

action_109 (32) = happyShift action_97
action_109 (39) = happyShift action_98
action_109 (41) = happyShift action_99
action_109 (62) = happyShift action_100
action_109 (12) = happyGoto action_118
action_109 (13) = happyGoto action_95
action_109 (14) = happyGoto action_96
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (19) = happyShift action_13
action_110 (22) = happyShift action_14
action_110 (23) = happyShift action_15
action_110 (24) = happyShift action_16
action_110 (25) = happyShift action_17
action_110 (27) = happyShift action_18
action_110 (29) = happyShift action_19
action_110 (34) = happyShift action_20
action_110 (35) = happyShift action_21
action_110 (36) = happyShift action_22
action_110 (37) = happyShift action_23
action_110 (38) = happyShift action_24
action_110 (39) = happyShift action_25
action_110 (41) = happyShift action_26
action_110 (59) = happyShift action_27
action_110 (62) = happyShift action_28
action_110 (7) = happyGoto action_117
action_110 (9) = happyGoto action_12
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (19) = happyShift action_13
action_111 (22) = happyShift action_14
action_111 (23) = happyShift action_15
action_111 (24) = happyShift action_16
action_111 (25) = happyShift action_17
action_111 (27) = happyShift action_18
action_111 (29) = happyShift action_19
action_111 (34) = happyShift action_20
action_111 (35) = happyShift action_21
action_111 (36) = happyShift action_22
action_111 (37) = happyShift action_23
action_111 (38) = happyShift action_24
action_111 (39) = happyShift action_25
action_111 (41) = happyShift action_26
action_111 (44) = happyShift action_41
action_111 (45) = happyShift action_42
action_111 (46) = happyShift action_43
action_111 (47) = happyShift action_44
action_111 (48) = happyShift action_45
action_111 (49) = happyShift action_46
action_111 (50) = happyShift action_47
action_111 (51) = happyShift action_48
action_111 (52) = happyShift action_49
action_111 (53) = happyShift action_50
action_111 (54) = happyShift action_51
action_111 (55) = happyShift action_52
action_111 (56) = happyShift action_53
action_111 (59) = happyShift action_27
action_111 (62) = happyShift action_28
action_111 (7) = happyGoto action_40
action_111 (9) = happyGoto action_12
action_111 _ = happyReduce_6

action_112 (62) = happyShift action_8
action_112 (6) = happyGoto action_116
action_112 _ = happyReduce_4

action_113 (19) = happyShift action_13
action_113 (22) = happyShift action_14
action_113 (23) = happyShift action_15
action_113 (24) = happyShift action_16
action_113 (25) = happyShift action_17
action_113 (27) = happyShift action_18
action_113 (29) = happyShift action_19
action_113 (34) = happyShift action_20
action_113 (35) = happyShift action_21
action_113 (36) = happyShift action_22
action_113 (37) = happyShift action_23
action_113 (38) = happyShift action_24
action_113 (39) = happyShift action_25
action_113 (41) = happyShift action_26
action_113 (44) = happyShift action_41
action_113 (45) = happyShift action_42
action_113 (46) = happyShift action_43
action_113 (47) = happyShift action_44
action_113 (48) = happyShift action_45
action_113 (49) = happyShift action_46
action_113 (50) = happyShift action_47
action_113 (51) = happyShift action_48
action_113 (52) = happyShift action_49
action_113 (53) = happyShift action_50
action_113 (54) = happyShift action_51
action_113 (55) = happyShift action_52
action_113 (56) = happyShift action_53
action_113 (59) = happyShift action_27
action_113 (62) = happyShift action_28
action_113 (7) = happyGoto action_40
action_113 (9) = happyGoto action_12
action_113 _ = happyReduce_9

action_114 (19) = happyShift action_13
action_114 (22) = happyShift action_14
action_114 (23) = happyShift action_15
action_114 (24) = happyShift action_16
action_114 (25) = happyShift action_17
action_114 (27) = happyShift action_18
action_114 (29) = happyShift action_19
action_114 (34) = happyShift action_20
action_114 (35) = happyShift action_21
action_114 (36) = happyShift action_22
action_114 (37) = happyShift action_23
action_114 (38) = happyShift action_24
action_114 (39) = happyShift action_25
action_114 (41) = happyShift action_26
action_114 (44) = happyShift action_41
action_114 (45) = happyShift action_42
action_114 (46) = happyShift action_43
action_114 (47) = happyShift action_44
action_114 (48) = happyShift action_45
action_114 (49) = happyShift action_46
action_114 (50) = happyShift action_47
action_114 (51) = happyShift action_48
action_114 (52) = happyShift action_49
action_114 (53) = happyShift action_50
action_114 (54) = happyShift action_51
action_114 (55) = happyShift action_52
action_114 (56) = happyShift action_53
action_114 (59) = happyShift action_27
action_114 (62) = happyShift action_28
action_114 (7) = happyGoto action_40
action_114 (9) = happyGoto action_12
action_114 _ = happyReduce_10

action_115 (19) = happyShift action_13
action_115 (22) = happyShift action_14
action_115 (23) = happyShift action_15
action_115 (24) = happyShift action_16
action_115 (25) = happyShift action_17
action_115 (27) = happyShift action_18
action_115 (29) = happyShift action_19
action_115 (34) = happyShift action_20
action_115 (35) = happyShift action_21
action_115 (36) = happyShift action_22
action_115 (37) = happyShift action_23
action_115 (38) = happyShift action_24
action_115 (39) = happyShift action_25
action_115 (41) = happyShift action_26
action_115 (44) = happyShift action_41
action_115 (45) = happyShift action_42
action_115 (46) = happyShift action_43
action_115 (47) = happyShift action_44
action_115 (48) = happyShift action_45
action_115 (49) = happyShift action_46
action_115 (50) = happyShift action_47
action_115 (51) = happyShift action_48
action_115 (52) = happyShift action_49
action_115 (53) = happyShift action_50
action_115 (54) = happyShift action_51
action_115 (55) = happyShift action_52
action_115 (56) = happyShift action_53
action_115 (59) = happyShift action_27
action_115 (62) = happyShift action_28
action_115 (7) = happyGoto action_40
action_115 (9) = happyGoto action_12
action_115 _ = happyReduce_7

action_116 (26) = happyShift action_129
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (19) = happyShift action_13
action_117 (22) = happyShift action_14
action_117 (23) = happyShift action_15
action_117 (24) = happyShift action_16
action_117 (25) = happyShift action_17
action_117 (27) = happyShift action_18
action_117 (29) = happyShift action_19
action_117 (31) = happyShift action_82
action_117 (34) = happyShift action_20
action_117 (35) = happyShift action_21
action_117 (36) = happyShift action_22
action_117 (37) = happyShift action_23
action_117 (38) = happyShift action_24
action_117 (39) = happyShift action_25
action_117 (41) = happyShift action_26
action_117 (44) = happyShift action_41
action_117 (45) = happyShift action_42
action_117 (46) = happyShift action_43
action_117 (47) = happyShift action_44
action_117 (48) = happyShift action_45
action_117 (49) = happyShift action_46
action_117 (50) = happyShift action_47
action_117 (51) = happyShift action_48
action_117 (52) = happyShift action_49
action_117 (53) = happyShift action_50
action_117 (54) = happyShift action_51
action_117 (55) = happyShift action_52
action_117 (56) = happyShift action_53
action_117 (59) = happyShift action_27
action_117 (62) = happyShift action_28
action_117 (7) = happyGoto action_40
action_117 (9) = happyGoto action_12
action_117 (11) = happyGoto action_128
action_117 _ = happyReduce_42

action_118 _ = happyReduce_44

action_119 (39) = happyShift action_98
action_119 (41) = happyShift action_99
action_119 (62) = happyShift action_100
action_119 (14) = happyGoto action_119
action_119 (15) = happyGoto action_127
action_119 _ = happyReduce_51

action_120 _ = happyReduce_46

action_121 _ = happyReduce_48

action_122 _ = happyReduce_49

action_123 (32) = happyShift action_97
action_123 (39) = happyShift action_98
action_123 (41) = happyShift action_99
action_123 (62) = happyShift action_100
action_123 (12) = happyGoto action_105
action_123 (13) = happyGoto action_95
action_123 (14) = happyGoto action_96
action_123 (16) = happyGoto action_126
action_123 _ = happyReduce_53

action_124 (62) = happyShift action_79
action_124 (8) = happyGoto action_125
action_124 _ = happyFail (happyExpListPerState 124)

action_125 _ = happyReduce_32

action_126 _ = happyReduce_55

action_127 _ = happyReduce_52

action_128 _ = happyReduce_43

action_129 (19) = happyShift action_13
action_129 (22) = happyShift action_14
action_129 (23) = happyShift action_15
action_129 (24) = happyShift action_16
action_129 (25) = happyShift action_17
action_129 (27) = happyShift action_18
action_129 (29) = happyShift action_19
action_129 (34) = happyShift action_20
action_129 (35) = happyShift action_21
action_129 (36) = happyShift action_22
action_129 (37) = happyShift action_23
action_129 (38) = happyShift action_24
action_129 (39) = happyShift action_25
action_129 (41) = happyShift action_26
action_129 (59) = happyShift action_27
action_129 (62) = happyShift action_28
action_129 (7) = happyGoto action_130
action_129 (9) = happyGoto action_12
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (19) = happyShift action_13
action_130 (22) = happyShift action_14
action_130 (23) = happyShift action_15
action_130 (24) = happyShift action_16
action_130 (25) = happyShift action_17
action_130 (27) = happyShift action_18
action_130 (29) = happyShift action_19
action_130 (34) = happyShift action_20
action_130 (35) = happyShift action_21
action_130 (36) = happyShift action_22
action_130 (37) = happyShift action_23
action_130 (38) = happyShift action_24
action_130 (39) = happyShift action_25
action_130 (41) = happyShift action_26
action_130 (44) = happyShift action_41
action_130 (45) = happyShift action_42
action_130 (46) = happyShift action_43
action_130 (47) = happyShift action_44
action_130 (48) = happyShift action_45
action_130 (49) = happyShift action_46
action_130 (50) = happyShift action_47
action_130 (51) = happyShift action_48
action_130 (52) = happyShift action_49
action_130 (53) = happyShift action_50
action_130 (54) = happyShift action_51
action_130 (55) = happyShift action_52
action_130 (56) = happyShift action_53
action_130 (59) = happyShift action_27
action_130 (62) = happyShift action_28
action_130 (7) = happyGoto action_40
action_130 (9) = happyGoto action_12
action_130 _ = happyReduce_11

happyReduce_1 = happyMonadReduce 0 4 happyReduction_1
happyReduction_1 (happyRest) tk
	 = happyThen ((( pure []))
	) (\r -> happyReturn (HappyAbsSyn4 r))

happyReduce_2 = happyMonadReduce 2 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ happy_var_1 : happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn4 r))

happyReduce_3 = happyMonadReduce 5 5 happyReduction_3
happyReduction_3 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyTerminal (T.Id happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ FuncDecl happy_var_2 happy_var_3 happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_4 = happyMonadReduce 0 6 happyReduction_4
happyReduction_4 (happyRest) tk
	 = happyThen ((( pure []))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_5 = happyMonadReduce 2 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyTerminal (T.Id happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ happy_var_1 : happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_6 = happyMonadReduce 6 7 happyReduction_6
happyReduction_6 ((HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ Let happy_var_2 happy_var_4 happy_var_6))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_7 = happyMonadReduce 6 7 happyReduction_7
happyReduction_7 ((HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ If happy_var_2 happy_var_4 happy_var_6))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_8 = happyMonadReduce 4 7 happyReduction_8
happyReduction_8 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ Lam happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_9 = happyMonadReduce 6 7 happyReduction_9
happyReduction_9 ((HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ Forall happy_var_2 happy_var_4 happy_var_6))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_10 = happyMonadReduce 6 7 happyReduction_10
happyReduction_10 ((HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ Exist happy_var_2 happy_var_4 happy_var_6))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_11 = happyMonadReduce 9 7 happyReduction_11
happyReduction_11 ((HappyAbsSyn7  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_7) `HappyStk`
	(HappyTerminal (T.Id happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ For happy_var_2 happy_var_4 (happy_var_6 : happy_var_7) happy_var_9))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_12 = happyMonadReduce 5 7 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ Match happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_13 = happyMonadReduce 2 7 happyReduction_13
happyReduction_13 ((HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ App happy_var_1 happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_14 = happyMonadReduce 3 7 happyReduction_14
happyReduction_14 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpOr))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_15 = happyMonadReduce 3 7 happyReduction_15
happyReduction_15 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpAnd))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_16 = happyMonadReduce 3 7 happyReduction_16
happyReduction_16 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpPlus))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_17 = happyMonadReduce 3 7 happyReduction_17
happyReduction_17 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpMin))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_18 = happyMonadReduce 3 7 happyReduction_18
happyReduction_18 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpMul))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_19 = happyMonadReduce 3 7 happyReduction_19
happyReduction_19 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpDiv))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_20 = happyMonadReduce 3 7 happyReduction_20
happyReduction_20 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpMod))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_21 = happyMonadReduce 3 7 happyReduction_21
happyReduction_21 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpAppend))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_22 = happyMonadReduce 3 7 happyReduction_22
happyReduction_22 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpLT))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_23 = happyMonadReduce 3 7 happyReduction_23
happyReduction_23 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpGT))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_24 = happyMonadReduce 3 7 happyReduction_24
happyReduction_24 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpEQ))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_25 = happyMonadReduce 3 7 happyReduction_25
happyReduction_25 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpCons))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_26 = happyMonadReduce 3 7 happyReduction_26
happyReduction_26 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ makeOp happy_var_1 happy_var_3 OpSeq))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_27 = happyMonadReduce 3 7 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_28 = happyMonadReduce 5 7 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ ListComp happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_29 = happyMonadReduce 1 7 happyReduction_29
happyReduction_29 ((HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ Lit happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_30 = happyMonadReduce 1 7 happyReduction_30
happyReduction_30 ((HappyTerminal (T.Id happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ Id happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_31 = happyMonadReduce 3 8 happyReduction_31
happyReduction_31 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ [(happy_var_1, happy_var_3)]))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_32 = happyMonadReduce 5 8 happyReduction_32
happyReduction_32 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Id happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ (happy_var_1, happy_var_3) : happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_33 = happyMonadReduce 1 9 happyReduction_33
happyReduction_33 ((HappyTerminal (T.String happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ LitStr happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_34 = happyMonadReduce 1 9 happyReduction_34
happyReduction_34 ((HappyTerminal (T.Float happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ LitFloat happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_35 = happyMonadReduce 1 9 happyReduction_35
happyReduction_35 ((HappyTerminal (T.Int happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ LitInt happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_36 = happyMonadReduce 1 9 happyReduction_36
happyReduction_36 (_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ LitBool True))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_37 = happyMonadReduce 1 9 happyReduction_37
happyReduction_37 (_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ LitBool False))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_38 = happyMonadReduce 3 9 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ LitList happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_39 = happyMonadReduce 0 10 happyReduction_39
happyReduction_39 (happyRest) tk
	 = happyThen ((( pure $ []))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_40 = happyMonadReduce 1 10 happyReduction_40
happyReduction_40 ((HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_41 = happyMonadReduce 3 10 happyReduction_41
happyReduction_41 ((HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ happy_var_1 : happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_42 = happyMonadReduce 0 11 happyReduction_42
happyReduction_42 (happyRest) tk
	 = happyThen ((( pure $ []))
	) (\r -> happyReturn (HappyAbsSyn11 r))

happyReduce_43 = happyMonadReduce 5 11 happyReduction_43
happyReduction_43 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ Clause happy_var_2 happy_var_4 : happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn11 r))

happyReduce_44 = happyMonadReduce 3 12 happyReduction_44
happyReduction_44 ((HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ PListCons happy_var_1 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_45 = happyMonadReduce 1 12 happyReduction_45
happyReduction_45 ((HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_46 = happyMonadReduce 3 13 happyReduction_46
happyReduction_46 ((HappyAbsSyn15  happy_var_3) `HappyStk`
	(HappyTerminal (T.Id happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ PCon happy_var_2 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_47 = happyMonadReduce 1 13 happyReduction_47
happyReduction_47 ((HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_48 = happyMonadReduce 3 14 happyReduction_48
happyReduction_48 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn14 r))

happyReduce_49 = happyMonadReduce 3 14 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ PList happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn14 r))

happyReduce_50 = happyMonadReduce 1 14 happyReduction_50
happyReduction_50 ((HappyTerminal (T.Id happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ PVar happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn14 r))

happyReduce_51 = happyMonadReduce 0 15 happyReduction_51
happyReduction_51 (happyRest) tk
	 = happyThen ((( pure $ []))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_52 = happyMonadReduce 2 15 happyReduction_52
happyReduction_52 ((HappyAbsSyn15  happy_var_2) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ happy_var_1 : happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_53 = happyMonadReduce 0 16 happyReduction_53
happyReduction_53 (happyRest) tk
	 = happyThen ((( pure $ []))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_54 = happyMonadReduce 1 16 happyReduction_54
happyReduction_54 ((HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ [happy_var_1]))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_55 = happyMonadReduce 3 16 happyReduction_55
happyReduction_55 ((HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pure $ happy_var_1 : happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyNewToken action sts stk [] =
	action 63 63 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	T.Def -> cont 17;
	T.DefEq -> cont 18;
	T.If -> cont 19;
	T.Then -> cont 20;
	T.Else -> cont 21;
	T.Fun -> cont 22;
	T.Exist -> cont 23;
	T.Forall -> cont 24;
	T.For -> cont 25;
	T.FunTo -> cont 26;
	T.Let -> cont 27;
	T.In -> cont 28;
	T.Match -> cont 29;
	T.With -> cont 30;
	T.Bar -> cont 31;
	T.ConsLead -> cont 32;
	T.End -> cont 33;
	T.Int happy_dollar_dollar -> cont 34;
	T.Float happy_dollar_dollar -> cont 35;
	T.String happy_dollar_dollar -> cont 36;
	T.TTrue -> cont 37;
	T.TFalse -> cont 38;
	T.ParenL -> cont 39;
	T.ParenR -> cont 40;
	T.BracketL -> cont 41;
	T.BracketR -> cont 42;
	T.Comma -> cont 43;
	T.Op "||" -> cont 44;
	T.Op "&&" -> cont 45;
	T.Op "+" -> cont 46;
	T.Op "-" -> cont 47;
	T.Op "*" -> cont 48;
	T.Op "/" -> cont 49;
	T.Op "%" -> cont 50;
	T.Op "++" -> cont 51;
	T.Op "<" -> cont 52;
	T.Op ">" -> cont 53;
	T.Op "==" -> cont 54;
	T.Op "::" -> cont 55;
	T.Op ";" -> cont 56;
	T.At -> cont 57;
	T.Anno -> cont 58;
	T.BraceL -> cont 59;
	T.BraceR -> cont 60;
	T.LArrow -> cont 61;
	T.Id happy_dollar_dollar -> cont 62;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 63 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Either String a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(T.Token)], [Prelude.String]) -> Either String a
happyError' = (\(tokens, _) -> parseError tokens)
parseTok tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


makeOp :: Term -> Term -> Op -> Term
makeOp l r op = BiOp op l r   

parse :: String -> Either String Program
parse = parseTok . T.alexScanTokens

parseError tok = Left $ "Parse error in: " ++ show tok ++ "\nExpecting: " -- ++ show exp

data Op = OpPlus | OpMin | OpMul | OpDiv | OpLT | OpGT | OpEQ
        | OpOr   | OpAnd | OpMod | OpCons | OpAppend | OpSeq
  deriving (Show, Eq)

type Var = String
type Constructor = String

type Program = [FuncDecl]

data FuncDecl = FuncDecl
  { funcName :: String
  , arguments :: [Var]
  , funcBody :: Term
  } deriving Show

type ArgList = [String]

data Pattern 
  = PVar Var 
  | PCon Constructor [Pattern]
  | PList [Pattern]
  | PListCons Pattern Pattern 
  deriving (Show, Eq)

data Clause = Clause Pattern Term 
  deriving (Show, Eq)
  
data Lit 
  = LitStr String 
  | LitInt Int 
  | LitFloat Double 
  | LitBool Bool
  | LitList [Term]
  deriving (Show, Eq)

data Term 
  = If Term Term Term 
  | Lit Lit
  | Lam Var Term 
  | Id Var   
  | App Term Term 
  | BiOp Op Term Term
  | Let Var Term Term 
  | Match Term [Clause]

  | Forall Var Term Term 
  | Exist Var Term Term 
  | For Var Term [Var] Term

  | ListComp Term [(Var, Term)]

  deriving (Show, Eq)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
