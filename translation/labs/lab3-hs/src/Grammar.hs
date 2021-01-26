{-# OPTIONS_GHC -w #-}
module Grammar where
import Lexer
import Expression
import Data.List.NonEmpty
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,183) ([0,1024,0,0,2,0,0,0,0,1,0,0,0,0,2,0,0,0,0,1,0,8,0,0,0,0,2,0,0,0,0,0,0,128,0,0,0,0,64,0,0,2,0,4,0,4096,0,20480,1027,0,0,0,16384,0,0,0,0,0,1,32768,8218,0,0,0,0,0,0,0,0,0,448,0,64,0,16384,0,0,512,0,4096,0,0,8,0,0,0,0,1027,0,0,0,0,129,0,0,0,12288,64,0,8216,0,1024,0,0,2054,0,32768,1,0,0,0,192,1,24576,128,0,0,0,0,0,0,0,0,0,5,0,0,0,384,2,0,128,0,128,0,0,4,0,0,0,0,0,0,1032,0,1024,2,0,0,0,512,0,0,1,0,16432,0,0,1,0,2560,0,0,0,0,0,0,128,2,0,0,0,32874,0,13568,64,0,128,0,16384,0,0,0,0,32,0,0,0,0,512,0,27136,128,0,256,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","SourceFile","Class","FunctionsR","Functions","Function","FunctionsDefinitionArgsR","FunctionsDefinitionArgs","FunctionsDefinitionArg","Block","BlockR","Expression","Expression1","BaseExpression","OpExpression","FuncCallArgs","FuncCallArgsR","FuncCallArg","Type","PathIdentifier","PathIdentifierR","if","else","while","classT","identifier","'('","')'","'{'","'}'","'.'","','","';'","'='","sign","literal","%eof"]
        bit_start = st Prelude.* 39
        bit_end = (st Prelude.+ 1) Prelude.* 39
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..38]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (27) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (27) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (28) = happyShift action_5
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (39) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (31) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (6) = happyGoto action_7
action_6 (7) = happyGoto action_8
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (32) = happyShift action_12
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (28) = happyShift action_11
action_8 (8) = happyGoto action_9
action_8 (21) = happyGoto action_10
action_8 _ = happyReduce_3

action_9 _ = happyReduce_4

action_10 (28) = happyShift action_13
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_32

action_12 _ = happyReduce_2

action_13 (29) = happyShift action_14
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (9) = happyGoto action_15
action_14 (10) = happyGoto action_16
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (30) = happyShift action_18
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (34) = happyShift action_17
action_16 _ = happyReduce_6

action_17 (28) = happyShift action_11
action_17 (11) = happyGoto action_20
action_17 (21) = happyGoto action_21
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (31) = happyShift action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (24) = happyShift action_32
action_19 (26) = happyShift action_33
action_19 (28) = happyShift action_34
action_19 (29) = happyShift action_35
action_19 (38) = happyShift action_36
action_19 (12) = happyGoto action_23
action_19 (13) = happyGoto action_24
action_19 (14) = happyGoto action_25
action_19 (15) = happyGoto action_26
action_19 (16) = happyGoto action_27
action_19 (17) = happyGoto action_28
action_19 (21) = happyGoto action_29
action_19 (22) = happyGoto action_30
action_19 (23) = happyGoto action_31
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_7

action_21 (28) = happyShift action_22
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_8

action_23 (32) = happyShift action_48
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (24) = happyShift action_32
action_24 (26) = happyShift action_33
action_24 (28) = happyShift action_34
action_24 (29) = happyShift action_35
action_24 (38) = happyShift action_36
action_24 (14) = happyGoto action_47
action_24 (15) = happyGoto action_26
action_24 (16) = happyGoto action_27
action_24 (17) = happyGoto action_28
action_24 (21) = happyGoto action_29
action_24 (22) = happyGoto action_30
action_24 (23) = happyGoto action_31
action_24 _ = happyReduce_9

action_25 _ = happyReduce_10

action_26 _ = happyReduce_25

action_27 _ = happyReduce_19

action_28 (35) = happyShift action_44
action_28 (36) = happyShift action_45
action_28 (37) = happyShift action_46
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (28) = happyShift action_43
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (29) = happyShift action_42
action_30 _ = happyReduce_21

action_31 (33) = happyShift action_41
action_31 _ = happyReduce_33

action_32 (29) = happyShift action_40
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (29) = happyShift action_39
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (29) = happyReduce_34
action_34 (33) = happyReduce_34
action_34 (35) = happyReduce_34
action_34 (36) = happyReduce_34
action_34 (37) = happyReduce_34
action_34 _ = happyReduce_32

action_35 (28) = happyShift action_38
action_35 (29) = happyShift action_35
action_35 (38) = happyShift action_36
action_35 (15) = happyGoto action_26
action_35 (16) = happyGoto action_27
action_35 (17) = happyGoto action_37
action_35 (22) = happyGoto action_30
action_35 (23) = happyGoto action_31
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_24

action_37 (30) = happyShift action_60
action_37 (37) = happyShift action_46
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_34

action_39 (28) = happyShift action_38
action_39 (29) = happyShift action_35
action_39 (38) = happyShift action_36
action_39 (15) = happyGoto action_26
action_39 (16) = happyGoto action_27
action_39 (17) = happyGoto action_59
action_39 (22) = happyGoto action_30
action_39 (23) = happyGoto action_31
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (28) = happyShift action_38
action_40 (29) = happyShift action_35
action_40 (38) = happyShift action_36
action_40 (15) = happyGoto action_26
action_40 (16) = happyGoto action_27
action_40 (17) = happyGoto action_58
action_40 (22) = happyGoto action_30
action_40 (23) = happyGoto action_31
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (28) = happyShift action_57
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (28) = happyShift action_38
action_42 (29) = happyShift action_35
action_42 (38) = happyShift action_36
action_42 (15) = happyGoto action_26
action_42 (16) = happyGoto action_27
action_42 (17) = happyGoto action_53
action_42 (18) = happyGoto action_54
action_42 (19) = happyGoto action_55
action_42 (20) = happyGoto action_56
action_42 (22) = happyGoto action_30
action_42 (23) = happyGoto action_31
action_42 _ = happyReduce_28

action_43 (35) = happyShift action_51
action_43 (36) = happyShift action_52
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_15

action_45 (28) = happyShift action_38
action_45 (29) = happyShift action_35
action_45 (38) = happyShift action_36
action_45 (15) = happyGoto action_26
action_45 (16) = happyGoto action_27
action_45 (17) = happyGoto action_50
action_45 (22) = happyGoto action_30
action_45 (23) = happyGoto action_31
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (28) = happyShift action_38
action_46 (29) = happyShift action_35
action_46 (38) = happyShift action_36
action_46 (15) = happyGoto action_49
action_46 (16) = happyGoto action_27
action_46 (22) = happyGoto action_30
action_46 (23) = happyGoto action_31
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_11

action_48 _ = happyReduce_5

action_49 _ = happyReduce_26

action_50 (35) = happyShift action_66
action_50 (37) = happyShift action_46
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_17

action_52 (28) = happyShift action_38
action_52 (29) = happyShift action_35
action_52 (38) = happyShift action_36
action_52 (15) = happyGoto action_26
action_52 (16) = happyGoto action_27
action_52 (17) = happyGoto action_65
action_52 (22) = happyGoto action_30
action_52 (23) = happyGoto action_31
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (37) = happyShift action_46
action_53 _ = happyReduce_31

action_54 (30) = happyShift action_64
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (34) = happyShift action_63
action_55 _ = happyReduce_27

action_56 _ = happyReduce_29

action_57 _ = happyReduce_35

action_58 (30) = happyShift action_62
action_58 (37) = happyShift action_46
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (30) = happyShift action_61
action_59 (37) = happyShift action_46
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_20

action_61 (31) = happyShift action_71
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (31) = happyShift action_70
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (28) = happyShift action_38
action_63 (29) = happyShift action_35
action_63 (38) = happyShift action_36
action_63 (15) = happyGoto action_26
action_63 (16) = happyGoto action_27
action_63 (17) = happyGoto action_53
action_63 (20) = happyGoto action_69
action_63 (22) = happyGoto action_30
action_63 (23) = happyGoto action_31
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (33) = happyShift action_68
action_64 _ = happyReduce_22

action_65 (35) = happyShift action_67
action_65 (37) = happyShift action_46
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_18

action_67 _ = happyReduce_16

action_68 (28) = happyShift action_38
action_68 (38) = happyShift action_36
action_68 (16) = happyGoto action_74
action_68 (22) = happyGoto action_30
action_68 (23) = happyGoto action_31
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_30

action_70 (24) = happyShift action_32
action_70 (26) = happyShift action_33
action_70 (28) = happyShift action_34
action_70 (29) = happyShift action_35
action_70 (38) = happyShift action_36
action_70 (12) = happyGoto action_73
action_70 (13) = happyGoto action_24
action_70 (14) = happyGoto action_25
action_70 (15) = happyGoto action_26
action_70 (16) = happyGoto action_27
action_70 (17) = happyGoto action_28
action_70 (21) = happyGoto action_29
action_70 (22) = happyGoto action_30
action_70 (23) = happyGoto action_31
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (24) = happyShift action_32
action_71 (26) = happyShift action_33
action_71 (28) = happyShift action_34
action_71 (29) = happyShift action_35
action_71 (38) = happyShift action_36
action_71 (12) = happyGoto action_72
action_71 (13) = happyGoto action_24
action_71 (14) = happyGoto action_25
action_71 (15) = happyGoto action_26
action_71 (16) = happyGoto action_27
action_71 (17) = happyGoto action_28
action_71 (21) = happyGoto action_29
action_71 (22) = happyGoto action_30
action_71 (23) = happyGoto action_31
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (32) = happyShift action_76
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (32) = happyShift action_75
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_23

action_75 (25) = happyShift action_77
action_75 _ = happyReduce_12

action_76 _ = happyReduce_13

action_77 (31) = happyShift action_78
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (24) = happyShift action_32
action_78 (26) = happyShift action_33
action_78 (28) = happyShift action_34
action_78 (29) = happyShift action_35
action_78 (38) = happyShift action_36
action_78 (12) = happyGoto action_79
action_78 (13) = happyGoto action_24
action_78 (14) = happyGoto action_25
action_78 (15) = happyGoto action_26
action_78 (16) = happyGoto action_27
action_78 (17) = happyGoto action_28
action_78 (21) = happyGoto action_29
action_78 (22) = happyGoto action_30
action_78 (23) = happyGoto action_31
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (32) = happyShift action_80
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_14

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happyReduce 5 5 happyReduction_2
happyReduction_2 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Class happy_var_2 Functions
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Prelude.reverse happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  7 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_1
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 8 8 happyReduction_5
happyReduction_5 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_5) `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier happy_var_2)) `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Function happy_var_1 happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Prelude.reverse happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  10 happyReduction_7
happyReduction_7 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_3 : happy_var_1
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  11 happyReduction_8
happyReduction_8 (HappyTerminal (TokIdentifier happy_var_2))
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn11
		 ((happy_var_1, happy_var_2)
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  12 happyReduction_9
happyReduction_9 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (Prelude.reverse happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  13 happyReduction_10
happyReduction_10 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  13 happyReduction_11
happyReduction_11 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 7 14 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (If happy_var_3 (Block happy_var_6)
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 7 14 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (While happy_var_3 (Block happy_var_6)
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 11 14 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (IfElse happy_var_3 (Block happy_var_6) (Block happy_var_10)
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_2  14 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 5 14 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier happy_var_2)) `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (DefinitionInitialized happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  14 happyReduction_17
happyReduction_17 _
	(HappyTerminal (TokIdentifier happy_var_2))
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn14
		 (Definition happy_var_1 happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 14 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Assign happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  15 happyReduction_19
happyReduction_19 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (Variable happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  15 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (BracketExpression happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  16 happyReduction_21
happyReduction_21 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn16
		 (Field happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 16 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (FunctionCall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 6 16 happyReduction_23
happyReduction_23 ((HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Extended (FunctionCall happy_var_1 happy_var_3) happy_var_6
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  16 happyReduction_24
happyReduction_24 (HappyTerminal (TokLiteral happy_var_1))
	 =  HappyAbsSyn16
		 (Literal happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  17 happyReduction_25
happyReduction_25 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  17 happyReduction_26
happyReduction_26 (HappyAbsSyn15  happy_var_3)
	(HappyTerminal (TokSign happy_var_2))
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (OpExpression happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  18 happyReduction_27
happyReduction_27 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (Prelude.reverse happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  19 happyReduction_28
happyReduction_28  =  HappyAbsSyn19
		 ([]
	)

happyReduce_29 = happySpecReduce_1  19 happyReduction_29
happyReduction_29 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  19 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_3 : happy_var_1
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  20 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  21 happyReduction_32
happyReduction_32 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn21
		 (Type happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  22 happyReduction_33
happyReduction_33 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (Data.List.NonEmpty.reverse happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  23 happyReduction_34
happyReduction_34 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn23
		 (happy_var_1 :| []
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  23 happyReduction_35
happyReduction_35 (HappyTerminal (TokIdentifier happy_var_3))
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_3 <| happy_var_1
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 39 39 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokIf -> cont 24;
	TokElse -> cont 25;
	TokWhile -> cont 26;
	TokClass -> cont 27;
	TokIdentifier happy_dollar_dollar -> cont 28;
	TokLBracket -> cont 29;
	TokRBracket -> cont 30;
	TokLBrace -> cont 31;
	TokRBrace -> cont 32;
	TokDot -> cont 33;
	TokComma -> cont 34;
	TokDotComma -> cont 35;
	TokAssign -> cont 36;
	TokSign happy_dollar_dollar -> cont 37;
	TokLiteral happy_dollar_dollar -> cont 38;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 39 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError x = error $ "Parse error" ++ show x
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
