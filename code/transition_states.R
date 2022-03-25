
# transition_states関数 -------------------------------------------------------

# 利用するパッケージ
library(ggplot2)
library(gganimate)


# データフレームの作成 --------------------------------------------------------------

# データフレームを作成
df <- tibble::tibble(
  x = 0:5, 
  y = 0:5, 
  state = 0:5
)


# 散布図を作成
ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 5, color = "hotpink") + # 散布図
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "geom_point()")

# 棒グラフを作成
ggplot(df, aes(x = 1, y = y, group = y)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "hotpink", color = "hotpink") + # 棒グラフ
  scale_x_continuous(breaks = 1, minor_breaks = FALSE) + # x軸目盛
  labs(title = "geom_bar()", 
       fill = "y", color = "y")

# 棒グラフの代用を作成
ggplot(df, aes(x = x, y = y / 2, height = y, fill = as.factor(x), color = as.factor(x))) + 
  geom_tile(width = 0.9) + # 棒グラフの代用
  coord_flip() + # 軸の入れ変え
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  labs(title = "geom_tile()", 
       y = "y", fill = "x", color = "x")


# 散布図の場合 ---------------------------------------------------------------------

### ・ラベル変数 -----

# ラベル変数を指定
lv <- "transitioning"  # 遷移中かどうか
lv <- "previous_state" # 前のデータの値
lv <- "closest_state"  # 近いデータの値
lv <- "next_state"     # 次のデータの値

# ラベル変数の設定
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  gganimate::transition_states(states = state) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_states() + labs(", lv, ")"), 
       subtitle = paste0("state : {", lv, "}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 60, fps = 6)


### ・引数 -----

# 値を指定:(デフォルト:1)
tl <- 10
#tl <- 0:5

# transition_length(データ間の遷移)の設定
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  gganimate::transition_states(states = state, transition_length = tl, wrap = FALSE) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_states(transition_length = ", tl, ")"), 
       subtitle = "transitioning : {transitioning}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = 60, fps = 6)


# 値を指定:(デフォルト:1)
sl <- 10
#sl <- 0:5

# state_length(データ点での一時停止)の設定
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  gganimate::transition_states(states = state, state_length = sl, wrap = FALSE) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_states(state_length = ", sl, ")"), 
       subtitle = "transitioning : {transitioning}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = 60, fps = 6)


# 値を指定:(デフォルト:1)
tl <- 100
sl <- 100

# state_length(データ点での一時停止)の設定
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  gganimate::transition_states(states = state, transition_length = tl, state_length = sl, wrap = FALSE) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_states(transition_states = ", tl, ", state_length = ", sl, ")"), 
       subtitle = "transitioning : {transitioning}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = 60, fps = 6)


# 論理値を指定:(デフォルト:TRUE)
b <- TRUE
b <- FALSE

# wrap(最後のデータと最初のデータの遷移)の設定
anim <- ggplot(head(df, 3), aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  gganimate::transition_states(states = state, wrap = b) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_states(wrap = ", b, ")"), 
       subtitle = "closest_state : {closest_state}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = 30, fps = 10)


# 棒グラフの場合 --------------------------------------------------------------------

### ・ラベル変数 -----

# ラベル変数を指定
lv <- "transitioning"  # 遷移中かどうか
lv <- "previous_state" # 前のデータの値
lv <- "closest_state"  # 近いデータの値
lv <- "next_state"     # 次のデータの値

# ラベル変数の設定
anim <- ggplot(df, aes(x = 1, y = y)) + 
  geom_bar(stat = "identity", fill = "hotpink", color = "hotpink") + # 棒グラフ
  gganimate::transition_states(states = state) + # フレーム
  scale_x_continuous(breaks = 1, minor_breaks = FALSE) + # x軸目盛
  labs(title = paste0("transition_states() + labs(", lv, ")"), 
       subtitle = paste0("state : {", lv, "}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 120, fps = 12, width = 250)


### ・引数 -----

# 値を指定:(デフォルト:1)
tl <- 10
#tl <- 0:5

# transition_length(データ間の遷移)の設定
anim <- ggplot(df, aes(x = 1, y = y)) + 
  geom_bar(stat = "identity", fill = "hotpink", color = "hotpink") + # 棒グラフ
  gganimate::transition_states(states = state, transition_length = tl, wrap = FALSE) + # フレーム
  scale_x_continuous(breaks = 1, minor_breaks = FALSE) + # x軸目盛
  labs(title = paste0("transition_states(transition_length = ", tl, ")"), 
       subtitle = "transitioning : {transitioning}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = 120, fps = 12, width = 300)


# 値を指定:(デフォルト:1)
sl <- 10
#sl <- 0:5

# state_length(データ点での一時停止)の設定
anim <- ggplot(df, aes(x = 1, y = y)) + 
  geom_bar(stat = "identity", fill = "hotpink", color = "hotpink") + # 棒グラフ
  gganimate::transition_states(states = state, state_length = sl, wrap = FALSE) + # フレーム
  scale_x_continuous(breaks = 1, minor_breaks = FALSE) + # x軸目盛
  labs(title = "transition_states(state_length)", 
       subtitle = "transitioning : {transitioning}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = 120, fps = 12, width = 300)


# 論理値を指定:(デフォルト:TRUE)
b <- TRUE
b <- FALSE

# wrap(最後のデータと最初のデータの遷移)の設定
anim <- ggplot(head(df, 3), aes(x = 1, y = y)) + 
  geom_bar(stat = "identity", fill = "hotpink", color = "hotpink") + # 棒グラフ
  gganimate::transition_states(states = state, wrap = b) +  # フレーム
  scale_x_continuous(breaks = 1, minor_breaks = FALSE) + # x軸目盛
  labs(title = paste0("transition_states(wrap = ", b, ")"), 
       subtitle = "closest_state : {closest_state}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = 120, fps = 12, width = 300)


# 2軸方向への変化
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_bar(stat = "identity", fill = "hotpink", color = "hotpink") + # 棒グラフ
  gganimate::transition_states(states = state) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  labs(title = paste0("transition_states()"), 
       subtitle = "state = {closest_state}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = 120, fps = 12)


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


### ・棒グラフによる可視化 -----

# 結果を格納
result_df <- tibble::tibble(
  v = c(NA_integer_, result_vec), 
  n = 0:N
)

# 出目を集計
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


# 遷移の数を指定
d <- 10

# 棒グラフを作成
anim <- ggplot() + 
  geom_bar(data = count_df, mapping = aes(x = v, y = count, fill = v, color = v), 
           stat = "identity") + # 棒グラフ
  geom_point(data = result_df, mapping = aes(x = v, y = 0), 
             color = "hotpink", size = 5) + # 散布図
  scale_fill_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # バーの色:(不必要)
  scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  gganimate::transition_states(states = n, transition_length = d) + 
  labs(title = "geom_bar() + geom_point() + transition_states()", 
       subtitle = "n = {closest_state}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N*d, fps = d)


### ・バーチャートレースによる可視化 -----

# ランク付け
rank_df <- count_df %>% 
  dplyr::group_by(n) %>% # グループ化
  dplyr::mutate(ranking = dplyr::row_number(-count)) %>% # ランキング列を追加
  dplyr::ungroup() # グループ化の解除


# バーチャートレースを作成
anim <- ggplot(rank_df, aes(x = ranking, y = count, fill = v, color = v)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 棒グラフ
  gganimate::transition_states(states = n, transition_length = d) + # フレーム
  coord_flip() + # 軸の入れ変え
  scale_x_reverse(breaks = 1:V, minor_breaks = FALSE) + # x軸を反転
  scale_fill_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # バーの色:(不必要)
  scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  labs(title = "geom_bar() + transition_states()", 
       subtitle = "n = {closest_state}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N*d, fps = d)


# バーチャートレースを作成
anim <- ggplot(rank_df, aes(x = ranking, y = count / 2, height = count, fill = v, color = v)) + 
  geom_tile(width = 0.9, alpha = 0.8) + # 棒グラフの代用
  gganimate::transition_states(states = n, transition_length = d) + # フレーム
  coord_flip() + # 軸の入れ変え
  scale_x_reverse(breaks = 1:V, minor_breaks = FALSE) + # x軸を反転
  scale_fill_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # バーの色:(不必要)
  scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  labs(title = "geom_tile() + transition_states()", 
       subtitle = "n = {closest_state}", 
       y = "count")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N*d, fps = d)

