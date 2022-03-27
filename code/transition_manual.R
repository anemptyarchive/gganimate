
# transition_manual関数 -------------------------------------------------------

# 利用するパッケージ
library(tidyverse)
library(gganimate)


# データフレームの作成 --------------------------------------------------------------

# データフレームを作成
df <- tibble::tibble(
  x = 0:5, 
  y = 0:5, 
  frame = 0:5
)


# 折れ線グラフを作成
ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 5, color = "hotpink") + # 散布図
  geom_path(color = "hotpink") + # 散布図
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "geom_point()")


# ラベル変数 ---------------------------------------------------------------------

# ラベル変数を指定
lv <- "previous_frame" # 前のデータの値
lv <- "current_frame"  # 現在のデータの値
lv <- "next_frame"     # 次のデータの値

# ラベル変数の設定
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  geom_path(color = "hotpink") + # 折れ線グラフ
  gganimate::transition_manual(frames = frame, cumulative = TRUE) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_manual() + labs(", lv, ")"), 
       subtitle = paste0("frame : {", lv, "}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 6, fps = 2)


# 引数 ----------------------------------------------------------------------

### ・cumulative -----

# 論理値を指定:(デフォルト:FALSE)
b <- TRUE
b <- FALSE

# cumulative(過去データの利用)の設定
anim <- ggplot(df, aes(x = x, y = y, group = 1)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  gganimate::transition_manual(frames = frame, cumulative = b) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_manual(cumulative = ", b, ")"), 
       subtitle = paste0("current_frame : {current_frame}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 6, fps = 2)


### ・frames -----

# データ数を指定
N <- 15

# ラベル用にランダムに整数を生成
label_int <- sample(x = 1:N, size = N, replace = FALSE)
label_int <- sample(x = 1:(N*10), size = N, replace = FALSE)
label_int <- sample(x = 1:N, size = N, replace = TRUE)
sort(label_int)

# ラベル用の文字列を作成
label_chr <- paste0("n=", 1:N)
sort(label_chr)

# ラベル用に文字列を因子に変換
label_fct <- factor(label_chr, levels = label_chr)
sort(label_fct)

# データフレームを作成
df <- tibble::tibble(
  x = 1:N, 
  y = runif(n = N, min = 0, max = 1), 
  frame = label_int
  #frame = label_chr
  #frame = label_fct
)

# frames(フレーム切替)の設定
anim <- ggplot(df, aes(x = x, y = y, group = 1)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  geom_path(color = "hotpink") + # 折れ線グラフ
  gganimate::transition_manual(frames = frame, cumulative = TRUE) + # フレーム
  scale_x_continuous(breaks = 1:N, minor_breaks = FALSE) + # x軸目盛
  labs(title = "transition_manual(frames)", 
       subtitle = "frame : {current_frame}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N, fps = 3)


# 利用例：サイコロ --------------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)
library(gganimate)


### ・データの生成 -----

# 試行回数を指定
N <- 30

# 面の数を指定
V <- 6

# サイコロを振る
result_mat <- rmultinom(n = N, size = 1, prob = rep(1 / V, times = V)) %>% 
  t()

# 出目を抽出
result_vec <- which(t(result_mat) == 1, arr.ind = TRUE)[, "row"]


### ・折れ線グラフによる可視化 -----

# 結果を格納
result_df <- tibble::tibble(
  v = c(NA_integer_, result_vec), 
  n = 0:N
)


# 折れ線グラフを作成
anim <- ggplot(result_df, aes(x = n, y = v)) + 
  geom_path(color = "hotpink") + # 折れ線グラフ
  geom_point(mapping = aes(color = factor(v)), size = 5) + # 散布図
  scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  gganimate::transition_manual(frames = n, cumulative = TRUE) + 
  labs(title = "geom_point() + geom_path() + transition_manual()", 
       subtitle = "n = {current_frame}", 
       color = "v")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N+1, fps = 5)


# 出目の出現回数を集計
count_df <- result_mat %>% 
  tibble::as_tibble() %>% # データフレームに変換
  cumsum() %>% # 試行ごとに集計
  dplyr::mutate(n = 1:N) %>% # 試行回数列を追加
  tidyr::pivot_longer(
    cols = -n, 
    names_to = "v", 
    names_prefix = "V", 
    names_ptypes = list(v = factor()), 
    values_to = "count"
  ) %>% # 縦型のデータフレームに変換
  rbind(
    tibble::tibble(
      n = rep(0, times = V), 
      v = factor(1:V), 
      count = rep(0, times = V)
    ), 
    .
  ) # 0回目の結果を追加


# 出目の推移を作図
anim <- ggplot() + 
  geom_line(data = count_df, mapping = aes(x = n, y = count, color = v), size = 1) + # 折れ線グラフ
  scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  gganimate::transition_manual(frames = n, cumulative = TRUE) + 
  labs(title = "geom_line() + transition_manual()", 
       subtitle = "n = {current_frame}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N+1, fps = 5)


# フレーム切替用の列を追加
count_df <- count_df %>% 
  dplyr::mutate(frame = n)

# 過去の試行を複製
count_df2 <- tibble::tibble()
for(i in 0:N) {
  # i回目までの結果を抽出
  tmp_df <- count_df %>% 
    dplyr::filter(n <= i) %>% # i回目までの結果を抽出
    dplyr::mutate(frame = i) # フレーム切替用の列を追加
  
  # 結果を結合
  count_df2 <- rbind(count_df2, tmp_df)
}


# 出目の出現回数の推移を作図
anim <- ggplot() + 
  geom_point(data = count_df, mapping = aes(x = n, y = count, color = v), 
             size = 5) + # 散布図
  geom_path(data = count_df2, mapping = aes(x = n, y = count, color = v), 
            size = 1) + # 折れ線グラフ
  gganimate::transition_manual(frames = frame, cumulative = FALSE) + 
  #scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  labs(title = "geom_point() + geom_path() + transition_manual()", 
       subtitle = "n = {current_frame}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N+1, fps = 5)


# 出目の出現回数の推移を作図
anim <- ggplot() + 
  geom_bar(data = count_df, mapping = aes(x = v, y = count, fill = v, color = v), 
           stat = "identity") + # 棒グラフ
  geom_point(data = result_df, mapping = aes(x = v, y = 0), 
             color = "hotpink", size = 5) + # 散布図
  #scale_fill_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # バーの色:(不必要)
  #scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  gganimate::transition_manual(frames = n) + 
  labs(title = "geom_bar() + geom_point() + transition_states()", 
       subtitle = "n = {current_frame}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N+1, fps = 5)


# 出目の数により順位付け
rank_df <- count_df %>% 
  dplyr::group_by(n) %>% # グループ化
  dplyr::mutate(ranking = dplyr::row_number(-count)) %>% # 順位列を追加
  dplyr::ungroup() # グループ化の解除

# 出目の数により順位付け
rank_df2 <- count_df2 %>% 
  dplyr::group_by(n, frame) %>% # グループ化
  dplyr::mutate(ranking = dplyr::row_number(-count)) %>% # 順位列を追加
  dplyr::ungroup() # グループ化の解除


# 出目の出現回数の順位の推移を作図
anim <- ggplot() + 
  geom_point(data = rank_df, mapping = aes(x = n, y = ranking, color = v, group = v), 
             size = 5) + # 散布図
  geom_path(data = rank_df2, mapping = aes(x = n, y = ranking, color = v, group = v), 
            size = 1) + # 折れ線グラフ
  gganimate::transition_manual(frames = frame, cumulative = FALSE) + 
  scale_y_reverse(breaks = 1:V, minor_breaks = FALSE) + # 
  #scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  labs(title = "geom_point() + geom_path() + transition_manual()", 
       subtitle = "n = {current_frame}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N+1, fps = 5)


### ・ラベル変数の加工 -----

# 出目の出現回数を集計
count_label_df <- result_mat %>% 
  tibble::as_tibble() %>% # データフレームに変換
  cumsum() %>% # 試行ごとに集計
  dplyr::mutate(n = 1:N) %>% # 試行回数列を追加
  dplyr::group_by(n) %>% # 試行回数でグループ化
  dplyr::mutate(label = paste0(dplyr::across(V1:V6), collapse = ", ")) %>% # ラベルの作成:1
  dplyr::mutate(label = paste0("n=", n, "=(", label, ")")) %>% # ラベルの作成:2
  dplyr::mutate(label = as.factor(label)) %>% # ラベルを因子型に変換
  tidyr::pivot_longer(
    cols = -c(n, label), 
    names_to = "v", 
    names_prefix = "V", 
    names_ptypes = list(v = factor()), 
    values_to = "count"
  ) %>% # 縦型のデータフレームに変換
  rbind(
    tibble::tibble(
      n = rep(0, times = V), 
      label = paste0("n=0=(", paste0(rep(0, times = V), collapse = ", "), ")") %>% 
        as.factor(), 
      v = factor(1:V), 
      count = rep(0, times = V)
    ), 
    .
  ) # 0回目の結果を追加

# 過去の試行を複製
count_label_df2 <- tibble::tibble()
label_vec <- unique(count_label_df[["label"]])
for(i in 0:N) {
  # i回目までの結果を抽出
  tmp_df <- count_label_df %>% 
    dplyr::filter(n <= i) %>% # i回目までの結果を抽出
    dplyr::mutate(label = label_vec[i+1]) # フレーム切替用の列を追加
  
  # 結果を結合
  count_label_df2 <- rbind(count_label_df2, tmp_df)
}


# 出目の出現回数の推移を作図
anim <- ggplot() + 
  geom_point(data = count_label_df, mapping = aes(x = n, y = count, color = v), 
             size = 5) + # 散布図
  geom_path(data = count_label_df2, mapping = aes(x = n, y = count, color = v), 
            size = 1) + # 折れ線グラフ
  gganimate::transition_manual(frames = label) + 
  #scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  labs(title = "geom_point() + geom_path() + transition_manual()", 
       subtitle = "{current_frame}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N+1, fps = 5)



# 結果を格納
result_label_df <- tibble::tibble(
  v = c(NA_integer_, result_vec), 
  n = 0:N, 
  label = unique(count_label_df[["label"]])
)


# 出目の出現回数の推移を作図
anim <- ggplot() + 
  geom_bar(data = count_label_df, mapping = aes(x = v, y = count, fill = v, color = v), 
           stat = "identity") + # 棒グラフ
  geom_point(data = result_label_df, mapping = aes(x = v, y = 0), 
             color = "hotpink", size = 5) + # 散布図
  #scale_fill_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # バーの色:(不必要)
  #scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  gganimate::transition_manual(frames = label) + 
  labs(title = "geom_bar() + geom_point() + transition_states()", 
       subtitle = "{current_frame}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N+1, fps = 5)


# ランク付け
rank_label_df <- count_label_df %>% 
  dplyr::group_by(n) %>% # グループ化
  dplyr::mutate(ranking = dplyr::row_number(-count)) %>% # ランキング列を追加
  dplyr::ungroup() # グループ化の解除

# ランク付け
rank_label_df2 <- count_label_df2 %>% 
  dplyr::group_by(n, label) %>% # グループ化
  dplyr::mutate(ranking = dplyr::row_number(-count)) %>% # ランキング列を追加
  dplyr::ungroup() # グループ化の解除


# 出目の出現回数の順位の推移を作図
anim <- ggplot() + 
  geom_point(data = rank_label_df, mapping = aes(x = n, y = ranking, color = v, group = v), 
             size = 5) + # 散布図
  geom_path(data = rank_label_df2, mapping = aes(x = n, y = ranking, color = v, group = v), 
            size = 1) + # 散布図
  scale_y_reverse() + 
  scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  gganimate::transition_manual(frames = label, cumulative = FALSE) + 
  labs(title = "geom_point() + geom_path + transition_manual()", 
       subtitle = "{current_frame}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N+1, fps = 5)

