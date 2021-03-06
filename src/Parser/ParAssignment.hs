{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser.ParAssignment where
import Parser.AbsAssignment as AbsAssignment
import Parser.LexAssignment
import Parser.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn13 :: (Ident) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (Ident)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (Integer) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (Integer)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Prog) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Prog)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ([Fun]) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ([Fun])
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Fun) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Fun)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Exp) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Exp)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (Term) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (Term)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Factor) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Factor)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ([Exp]) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ([Exp])
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ([Ident]) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ([Ident])
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (BExp) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (BExp)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (BTerm) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (BTerm)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xcd\x00\xcd\x00\xcd\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\xc1\x00\x04\x00\x04\x00\xc1\x00\x00\x00\xd8\x00\x00\x00\x7f\x00\x10\x00\x00\x00\xb8\x00\xfa\xff\x00\x00\x04\x00\xb6\x00\x00\x00\x00\x00\x01\x00\x00\x00\xcc\x00\xa1\x00\x6f\x00\xa1\x00\xa9\x00\x89\x00\xff\xff\x0b\x00\x89\x00\x80\x00\x73\x00\x74\x00\x6e\x00\x00\x00\x5b\x00\x6b\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x4b\x00\x04\x00\x04\x00\x53\x00\x0e\x00\x04\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x5a\x00\x2f\x00\x2f\x00\x2f\x00\x09\x00\x44\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\xce\x00\x00\x00\x00\x00\x10\x00\x10\x00\x3b\x00\x00\x00\x4a\x00\x00\x00\x33\x00\x0f\x00\x00\x00\x00\x00\x2a\x00\x19\x00\x1d\x00\x0f\x00\x0f\x00\xf7\xff\xed\xff\x27\x00\x2f\x00\x00\x00\x0f\x00\x26\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xd5\x00\x84\x00\x07\x00\xb9\x00\xc5\x00\xcf\x00\x7b\x00\x35\x00\x49\x00\x61\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x00\x00\x00\xc3\x00\xbb\x00\xc7\x00\x7e\x00\xb1\x00\x72\x00\x1c\x00\x59\x00\x51\x00\x05\x00\x00\x00\x32\x00\xae\x00\xa6\x00\xa3\x00\x69\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8d\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\xe0\xff\x00\x00\x00\x00\x00\x00\xf5\xff\xe8\xff\xe9\xff\x00\x00\xed\xff\xea\xff\x00\x00\x00\x00\xd5\xff\x00\x00\x00\x00\xd6\xff\xf4\xff\x00\x00\xdb\xff\xdf\xff\x00\x00\xe2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\x00\x00\xf3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\xe0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\x00\x00\xda\xff\xd8\xff\xd9\xff\x00\x00\x00\x00\x00\x00\xdc\xff\xdd\xff\xde\xff\xe1\xff\x00\x00\xeb\xff\xec\xff\xee\xff\xef\xff\xe0\xff\xf1\xff\x00\x00\xe7\xff\x00\x00\x00\x00\xd7\xff\xe6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\xe5\xff\x00\x00\x00\x00\xe4\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x07\x00\x01\x00\x04\x00\x17\x00\x0e\x00\x02\x00\x08\x00\x03\x00\x04\x00\x01\x00\x04\x00\x03\x00\x00\x00\x14\x00\x01\x00\x05\x00\x02\x00\x07\x00\x0f\x00\x04\x00\x11\x00\x09\x00\x13\x00\x08\x00\x18\x00\x16\x00\x1c\x00\x00\x00\x1c\x00\x1a\x00\x1b\x00\x11\x00\x18\x00\x13\x00\x15\x00\x00\x00\x09\x00\x18\x00\x1c\x00\x0b\x00\x1a\x00\x1b\x00\x05\x00\x05\x00\x07\x00\x07\x00\x05\x00\x17\x00\x07\x00\x00\x00\x01\x00\x05\x00\x00\x00\x07\x00\x05\x00\x06\x00\x07\x00\x03\x00\x04\x00\x0a\x00\x0b\x00\x09\x00\x19\x00\x19\x00\x00\x00\x01\x00\x19\x00\x19\x00\x12\x00\x05\x00\x06\x00\x07\x00\x00\x00\x01\x00\x0a\x00\x0b\x00\x03\x00\x05\x00\x06\x00\x07\x00\x00\x00\x01\x00\x0a\x00\x0b\x00\x1a\x00\x05\x00\x06\x00\x07\x00\x00\x00\x01\x00\x17\x00\x0b\x00\x03\x00\x05\x00\x06\x00\x07\x00\x00\x00\x01\x00\x10\x00\x0b\x00\x1a\x00\x05\x00\x06\x00\x07\x00\x00\x00\x01\x00\x10\x00\x0b\x00\x02\x00\x05\x00\x06\x00\x07\x00\x08\x00\x00\x00\x01\x00\x05\x00\x06\x00\x07\x00\x05\x00\x06\x00\x07\x00\x08\x00\x00\x00\x01\x00\x09\x00\x00\x00\x01\x00\x05\x00\x06\x00\x07\x00\x08\x00\x05\x00\x07\x00\x07\x00\x03\x00\x04\x00\x0a\x00\x1c\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x1c\x00\x00\x00\x01\x00\x05\x00\x06\x00\x07\x00\x05\x00\x06\x00\x07\x00\x00\x00\x01\x00\x1a\x00\x00\x00\x01\x00\x05\x00\x06\x00\x07\x00\x05\x00\x06\x00\x07\x00\x00\x00\x01\x00\x1c\x00\x00\x00\x01\x00\x05\x00\x06\x00\x07\x00\x05\x00\x06\x00\x07\x00\x00\x00\x01\x00\x07\x00\x00\x00\x01\x00\x05\x00\x06\x00\x07\x00\x05\x00\x06\x00\x07\x00\x00\x00\x01\x00\x00\x00\x01\x00\x1c\x00\x05\x00\x06\x00\x07\x00\x06\x00\x07\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x06\x00\x07\x00\x06\x00\x07\x00\x17\x00\x07\x00\x00\x00\x01\x00\x03\x00\x06\x00\x05\x00\x1c\x00\x07\x00\x07\x00\x02\x00\x03\x00\x04\x00\x02\x00\x1a\x00\xff\xff\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x2f\x00\x32\x00\x2d\x00\x5d\x00\x59\x00\x13\x00\x2e\x00\x40\x00\x25\x00\x32\x00\x22\x00\x51\x00\x1a\x00\x36\x00\x32\x00\x2b\x00\x1f\x00\x2c\x00\x14\x00\x2d\x00\x15\x00\x4c\x00\x16\x00\x2e\x00\x33\x00\x17\x00\xff\xff\x1a\x00\xff\xff\x0c\x00\x18\x00\x15\x00\x33\x00\x16\x00\x40\x00\x29\x00\x43\x00\x33\x00\xff\xff\x56\x00\x0c\x00\x18\x00\x2b\x00\x2b\x00\x2c\x00\x2c\x00\x2b\x00\x57\x00\x2c\x00\x0c\x00\x0d\x00\x2b\x00\x1a\x00\x2c\x00\x0e\x00\x0f\x00\x10\x00\x4b\x00\x25\x00\x3e\x00\x19\x00\x1b\x00\x5f\x00\x5c\x00\x0c\x00\x0d\x00\x58\x00\x4f\x00\x54\x00\x0e\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\x34\x00\x19\x00\x55\x00\x0e\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\x18\x00\x19\x00\x0c\x00\x0e\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\x50\x00\x41\x00\x52\x00\x0e\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\x24\x00\x42\x00\x0c\x00\x0e\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\x24\x00\x11\x00\x4b\x00\x1c\x00\x0f\x00\x10\x00\x3a\x00\x0c\x00\x0d\x00\x2b\x00\x30\x00\x2c\x00\x1c\x00\x0f\x00\x10\x00\x44\x00\x0c\x00\x0d\x00\x29\x00\x0c\x00\x0d\x00\x1c\x00\x0f\x00\x10\x00\x1d\x00\x2b\x00\x46\x00\x2c\x00\x24\x00\x25\x00\x37\x00\xff\xff\x38\x00\x39\x00\x0c\x00\x0d\x00\xff\xff\x0c\x00\x0d\x00\x5d\x00\x0f\x00\x10\x00\x59\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\x0c\x00\x0c\x00\x0d\x00\x5a\x00\x0f\x00\x10\x00\x52\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\xff\xff\x0c\x00\x0d\x00\x3b\x00\x0f\x00\x10\x00\x3c\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\x2f\x00\x0c\x00\x0d\x00\x3d\x00\x0f\x00\x10\x00\x45\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\x0c\x00\x0d\x00\xff\xff\x21\x00\x0f\x00\x10\x00\x48\x00\x10\x00\x0c\x00\x0d\x00\x0c\x00\x0d\x00\x0c\x00\x0d\x00\x49\x00\x10\x00\x20\x00\x10\x00\x34\x00\x47\x00\x0c\x00\x0d\x00\x4e\x00\x31\x00\x2b\x00\xff\xff\x2c\x00\x1f\x00\x26\x00\x27\x00\x25\x00\x3a\x00\x0c\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (10, 42) [
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42)
	]

happy_n_terms = 29 :: Int
happy_n_nonterms = 12 :: Int

happyReduce_10 = happySpecReduce_1  0# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn13
		 (Ident happy_var_1
	)}

happyReduce_11 = happySpecReduce_1  1# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn14
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_12 = happySpecReduce_1  2# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (AbsAssignment.PROG happy_var_1
	)}

happyReduce_13 = happySpecReduce_1  3# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 ((:[]) happy_var_1
	)}

happyReduce_14 = happySpecReduce_3  3# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_15 = happyReduce 7# 4# happyReduction_15
happyReduction_15 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_2 of { happy_var_2 -> 
	case happyOut22 happy_x_4 of { happy_var_4 -> 
	case happyOut18 happy_x_7 of { happy_var_7 -> 
	happyIn17
		 (AbsAssignment.FUNS happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_16 = happySpecReduce_3  5# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (AbsAssignment.EXP_ADD happy_var_1 happy_var_3
	)}}

happyReduce_17 = happySpecReduce_3  5# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (AbsAssignment.EXP_SUB happy_var_1 happy_var_3
	)}}

happyReduce_18 = happySpecReduce_1  5# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (AbsAssignment.EXP_Term happy_var_1
	)}

happyReduce_19 = happySpecReduce_3  6# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 (AbsAssignment.TERM_MUL happy_var_1 happy_var_3
	)}}

happyReduce_20 = happySpecReduce_3  6# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 (AbsAssignment.TERM_DIV happy_var_1 happy_var_3
	)}}

happyReduce_21 = happySpecReduce_1  6# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (AbsAssignment.TERM_FACT happy_var_1
	)}

happyReduce_22 = happySpecReduce_1  7# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (AbsAssignment.FACT_CONST happy_var_1
	)}

happyReduce_23 = happySpecReduce_1  7# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (AbsAssignment.FACT_VAR happy_var_1
	)}

happyReduce_24 = happyReduce 4# 7# happyReduction_24
happyReduction_24 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (AbsAssignment.FACT_NEG happy_var_3
	) `HappyStk` happyRest}

happyReduce_25 = happyReduce 4# 7# happyReduction_25
happyReduction_25 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (AbsAssignment.FACT_APP happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_26 = happyReduce 8# 7# happyReduction_26
happyReduction_26 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_3 of { happy_var_3 -> 
	case happyOut18 happy_x_7 of { happy_var_7 -> 
	happyIn20
		 (AbsAssignment.FACT_LET happy_var_3 happy_var_7
	) `HappyStk` happyRest}}

happyReduce_27 = happyReduce 10# 7# happyReduction_27
happyReduction_27 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_5 of { happy_var_5 -> 
	case happyOut18 happy_x_9 of { happy_var_9 -> 
	happyIn20
		 (AbsAssignment.EXP_COND happy_var_2 happy_var_5 happy_var_9
	) `HappyStk` happyRest}}}

happyReduce_28 = happySpecReduce_0  8# happyReduction_28
happyReduction_28  =  happyIn21
		 ([]
	)

happyReduce_29 = happySpecReduce_1  8# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 ((:[]) happy_var_1
	)}

happyReduce_30 = happySpecReduce_3  8# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_0  9# happyReduction_31
happyReduction_31  =  happyIn22
		 ([]
	)

happyReduce_32 = happySpecReduce_1  9# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((:[]) happy_var_1
	)}

happyReduce_33 = happySpecReduce_3  9# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_34 = happySpecReduce_3  10# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (AbsAssignment.BEXP_AND happy_var_1 happy_var_3
	)}}

happyReduce_35 = happySpecReduce_3  10# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (AbsAssignment.BEXP_OR happy_var_1 happy_var_3
	)}}

happyReduce_36 = happySpecReduce_1  10# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (AbsAssignment.BEXP_TERM happy_var_1
	)}

happyReduce_37 = happySpecReduce_3  11# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (AbsAssignment.BTERM_GT happy_var_1 happy_var_3
	)}}

happyReduce_38 = happySpecReduce_3  11# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (AbsAssignment.BTERM_LT happy_var_1 happy_var_3
	)}}

happyReduce_39 = happySpecReduce_3  11# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (AbsAssignment.BTERM_EQ happy_var_1 happy_var_3
	)}}

happyReduce_40 = happyReduce 4# 11# happyReduction_40
happyReduction_40 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (AbsAssignment.BTERM_NOT happy_var_3
	) `HappyStk` happyRest}

happyReduce_41 = happySpecReduce_1  11# happyReduction_41
happyReduction_41 happy_x_1
	 =  happyIn24
		 (AbsAssignment.BTERM_TRUE
	)

happyReduce_42 = happySpecReduce_1  11# happyReduction_42
happyReduction_42 happy_x_1
	 =  happyIn24
		 (AbsAssignment.BTERM_FALSE
	)

happyNewToken action sts stk [] =
	happyDoAction 28# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TV happy_dollar_dollar) -> cont 26#;
	PT _ (TI happy_dollar_dollar) -> cont 27#;
	_ -> happyError' (tk:tks)
	}

happyError_ 28# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProg tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut15 x))

pListFun tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut16 x))

pFun tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut17 x))

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut18 x))

pTerm tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut19 x))

pFactor tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut20 x))

pListExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut21 x))

pListIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut22 x))

pBExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut23 x))

pBTerm tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut24 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 11 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/opt/ghc/7.10.3/lib/ghc-7.10.3/include/ghcversion.h" #-}

















{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
