theory Treehash
  imports Main
begin

typedecl Hash
consts concatHash :: "Hash \<Rightarrow> Hash \<Rightarrow> Hash" (infix "\<oplus>" 50)
consts emptyHash :: Hash

fun concatInPairs :: "Hash list \<Rightarrow> Hash list" where
  "concatInPairs [] = []"
| "concatInPairs [h] = [h]"
| "concatInPairs (h1#h2#hs) = (h1 \<oplus> h2) # concatInPairs hs"

lemma wf_less_length: "wf (inv_image {(l1,l2). l1 < (l2::nat)} length)"
  by (intro wf_inv_image wf_less)

lemma length_concatInPairs: "length (concatInPairs xs) \<le> length xs"
  using wf_less_length
proof (induct xs rule: wf_induct_rule)
  case (less xs) show ?case proof (cases xs)
    case Cons': (Cons x' xs') thus ?thesis proof (cases xs')
      case Cons'': (Cons x'' xs'')
      have "length (concatInPairs xs'') \<le> length xs''" by (intro less, auto simp add: Cons' Cons'')
      thus ?thesis by (simp add: Cons' Cons'')
    qed simp
  qed simp
qed

function (sequential) naiveTreeHash :: "Hash list \<Rightarrow> Hash" where
  "naiveTreeHash [] = emptyHash"
| "naiveTreeHash [h] = h"
| "naiveTreeHash hs = naiveTreeHash (concatInPairs hs)"
  by pat_completeness auto

termination naiveTreeHash
  using length_concatInPairs by (intro local.termination [OF wf_less_length], auto simp add: less_Suc_eq_le)

record LevelHash =
  level :: nat
  hashValue  :: Hash

definition concatNextLevel :: "LevelHash \<Rightarrow> LevelHash \<Rightarrow> LevelHash" where
  "concatNextLevel lh0 lh1 = \<lparr> level = level lh0 + 1, hashValue = hashValue lh1 \<oplus> hashValue lh0 \<rparr>"

fun combineHashes :: "LevelHash \<Rightarrow> LevelHash list \<Rightarrow> LevelHash list" where
  "combineHashes lh0 [] = [lh0]"
| "combineHashes lh0 (lh1#lhs) = (if level lh0 = level lh1 then combineHashes (concatNextLevel lh0 lh1) lhs else (lh0 # lh1 # lhs))"

fun collapse :: "Hash list \<Rightarrow> Hash" where
  "collapse [] = emptyHash"
| "collapse (h0#hs) = foldr (op \<oplus>) hs h0"

definition level0 :: "Hash \<Rightarrow> LevelHash"
  where "level0 h = \<lparr> level = 0, hashValue = h \<rparr>"

fun treeHash_worker :: "LevelHash list \<Rightarrow> Hash list \<Rightarrow> LevelHash list" where
  "treeHash_worker lhs [] = lhs"
| "treeHash_worker lhs (h#hs) = treeHash_worker (combineHashes (level0 h) lhs) hs"

fun treeHash :: "Hash list \<Rightarrow> Hash" where
  "treeHash [] = emptyHash"
| "treeHash (h0#hs) = collapse (map hashValue (treeHash_worker [level0 h0] hs))"

lemma "naiveTreeHash [] = emptyHash" by simp
lemma "naiveTreeHash [h] = h" by simp
lemma "naiveTreeHash [h1, h2] = (h1 \<oplus> h2)" by simp
lemma "naiveTreeHash [h1, h2, h3] = ((h1 \<oplus> h2) \<oplus> h3)" by simp
lemma "naiveTreeHash [h1, h2, h3, h4] = ((h1 \<oplus> h2) \<oplus> (h3 \<oplus> h4))" by simp
lemma "naiveTreeHash [h1, h2, h3, h4, h5] = (((h1 \<oplus> h2) \<oplus> (h3 \<oplus> h4)) \<oplus> h5)" by simp
lemma "naiveTreeHash [h1, h2, h3, h4, h5, h6] = (((h1 \<oplus> h2) \<oplus> (h3 \<oplus> h4)) \<oplus> (h5 \<oplus> h6))" by simp

lemma "naiveTreeHash [] = treeHash []" by simp
lemma "naiveTreeHash [h1] = treeHash [h1]" by (simp add: level0_def)
lemma "naiveTreeHash [h1, h2] = treeHash [h1, h2]" by (simp add: level0_def concatNextLevel_def)
lemma "naiveTreeHash [h1, h2, h3] = treeHash [h1, h2, h3]" by (simp add: level0_def concatNextLevel_def)
lemma "naiveTreeHash [h1, h2, h3, h4] = treeHash [h1, h2, h3, h4]" by (simp add: level0_def concatNextLevel_def)
lemma "naiveTreeHash [h1, h2, h3, h4, h5] = treeHash [h1, h2, h3, h4, h5]" by (simp add: level0_def concatNextLevel_def)
lemma "naiveTreeHash [h1, h2, h3, h4, h5, h6] = treeHash [h1, h2, h3, h4, h5, h6]" by (simp add: level0_def concatNextLevel_def)
