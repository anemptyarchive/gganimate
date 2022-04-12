
# ease_aes ----------------------------------------------------------------

# 利用パッケージ
library(gganimate)
library(ggplot2)

# チェック用
library(ggplot2)
library(magrittr)


# データフレームの作成 --------------------------------------------------------------

# データフレームを作成
df <- tibble::tibble(
  x = 0:1, 
  y = 0:1, 
  frame = 0:1
)

# 散布図を作成
ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 5, color = "hotpink") + # 散布図
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "geom_point()") # ラベル

# 棒グラフを作成
ggplot(df, aes(x = 1, y = y, group = y)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "hotpink", color = "hotpink") + # 棒グラフ
  scale_x_continuous(breaks = 1, minor_breaks = FALSE) + # x軸目盛
  coord_flip() + # 軸の反転
  labs(title = "geom_bar()") # ラベル


# 引数 ----------------------------------------------------------------------

# 始点と終点用のデータフレームを作成
point_df <- df %>% 
  dplyr::select(!frame) # フレーム列を削除


# イージング関数を指定
ef <- "quadratic"
ef <- "cubic"
ef <- "quartic"
ef <- "quintic"
ef <- "sine"
ef <- "circular"
ef <- "exponential"
ef <- "elastic"
ef <- "back"
ef <- "bounce"

# 変化のスタイルを指定
em <- "-in"
em <- "-out"
em <- "-in-out"


# イージングの設定:散布図
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(data = point_df, mapping = aes(x = x, y = y), 
             size = 5, shape = 4, color = "red") + # 始点と終点
  geom_point(data = df, mapping = aes(x = x, y = y), 
             size = 10, alpha = 0.8, color = "hotpink") + # 移動点
  gganimate::transition_states(states = frame, transition_length = 9, state_length = 1) + # フレーム
  gganimate::ease_aes(paste0(ef, em)) + # 変化の緩急
  #xlim(c(-0.5, 1.5)) + # x軸の描画範囲
  #ylim(c(-0.5, 1.5)) + # y軸の描画範囲
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0('transition_states() + ease_aes("', ef, em, '")'), 
       subtitle = "transition : {transitioning}") # ラベル

# gif画像を作成
gganimate::animate(plot = anim, nframes = 40, fps = 10)


# イージングの設定:棒グラフ
anim <- ggplot() + 
  geom_bar(data = df, mapping = aes(x = 1, y = y), 
           stat = "identity", width = 0.9, alpha = 0.8, fill = "hotpink", color = "hotpink") + # 伸縮バー
  geom_hline(data = point_df, mapping = aes(yintercept = y), 
             size = 1, color = "red", linetype = "dashed") + # 始点と終点
  gganimate::transition_states(states = frame, transition_length = 9, state_length = 1) + # フレーム
  gganimate::ease_aes(paste0(ef, em)) + # 変化の緩急
  scale_x_continuous(breaks = 1, minor_breaks = FALSE, limits = c(0.5, 1.5)) + # x軸目盛
  #ylim(c(-0.5, 1.5)) + # y軸の描画範囲
  coord_flip() + # 軸の反転
  labs(title = paste0('transition_states() + ease_aes("', ef, em, '")'), 
       subtitle = "transition : {transitioning}") # ラベル

# gif画像を作成
gganimate::animate(plot = anim, nframes = 40, fps = 10, height = 150)


# 複数データの場合 ----------------------------------------------------------------

###　・複数データ間の遷移 -----

# データ数を指定
N <- 10

# ランダムに値を作成
df <- tibble::tibble(
  x = sample(x = 0:N, size = N, replace = FALSE), # x軸の値
  y = sample(x = 0:N, size = N, replace = FALSE), # y軸の値
  frame = 1:N # フレーム番号
)

# 目印用のデータフレームを作成
mark_df <- df %>% 
  dplyr::select(!frame) # フレーム列を削除


# イージング関数を指定
efm <- "circular-in"

# 散布図を作成
anim <- ggplot() + 
  geom_point(data = mark_df, mapping = aes(x = x, y = y), 
             size = 5, shape = 4, color = "red") + # 実データの点
  geom_path(data = mark_df, mapping = aes(x = x, y = y), 
            size = 1, color = "red", linetype = "dotted") + # 実データ間(軌道)の線
  geom_point(data = df, mapping = aes(x = x, y = y), 
             size = 10, alpha = 0.8, color = "hotpink") + # 移動点
  gganimate::transition_states(states = frame, transition_length = 9, state_length = 1) + # フレーム
  gganimate::ease_aes(efm) + # 変化の緩急
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0('transition_states() + ease_aes("', efm, '")'), 
       subtitle = "transition : {transitioning}") # ラベル

# gif画像を作成
gganimate::animate(plot = anim, nframes = (N+1)*10, fps = 10)


# 棒グラフを作成
anim <- ggplot() + 
  geom_bar(data = df, mapping = aes(x = 1, y = y), 
           stat = "identity", width = 0.9, alpha = 0.8, fill = "hotpink", color = "hotpink") + # 伸縮バー
  geom_hline(data = mark_df, mapping = aes(yintercept = y), 
             size = 1, color = "red", linetype = "dashed") + # 実データの線
  gganimate::transition_states(states = frame, transition_length = 9, state_length = 1) + # フレーム
  gganimate::ease_aes(efm) + # 変化の緩急
  scale_x_continuous(breaks = 1, minor_breaks = FALSE, limits = c(0.5, 1.5)) + # x軸目盛
  coord_flip() + # 軸の反転
  labs(title = paste0('transition_states() + ease_aes("', efm, '")'), 
       subtitle = "transition : {transitioning}") # ラベル

# gif画像を作成
gganimate::animate(plot = anim, nframes = (N+1)*10, fps = 10, height = 150)


###　・複数データの遷移 -----

# データ数を指定
N_data <- 10

# フレーム数を指定
N_frame <- 10

# ランダムに値を作成
df <- tibble::tibble(
  x = sample(x = 0:N_data, size = N_data*N_frame, replace = TRUE), # x軸の値
  y = sample(x = 0:N_data, size = N_data*N_frame, replace = TRUE), # y軸の値
  id = rep(1:N_data, times = N_frame) %>% 
    as.factor(), # データ番号
  frame = rep(1:N_data, each = N_frame) # フレーム番号
)


# イージング関数を指定
efm <- "cubic-in-out"

# 散布図を作成
anim <- ggplot() + 
  geom_point(data = df, mapping = aes(x = x, y = y, color = id), 
             size = 10, alpha = 0.8, show.legend = FALSE) + # 移動点
  gganimate::transition_states(states = frame, transition_length = 9, state_length = 1) + # フレーム
  gganimate::ease_aes(efm) + # 変化の緩急
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0('transition_states() + ease_aes("', efm, '")'), 
       subtitle = "transition : {transitioning}") # ラベル

# gif画像を作成
gganimate::animate(plot = anim, nframes = (N_frame+1)*10, fps = 10)


# 棒グラフを作成
anim <- ggplot() + 
  geom_bar(data = df, mapping = aes(x = id, y = y, fill = id, color = id), 
           stat = "identity", width = 0.9, alpha = 0.8, show.legend = FALSE) + # 伸縮バー
  gganimate::transition_states(states = frame, transition_length = 9, state_length = 1) + # フレーム
  gganimate::ease_aes(efm) + # 変化の緩急
  coord_flip() + # 軸の反転
  labs(title = paste0('transition_states() + ease_aes("', efm, '")'), 
       subtitle = "transition : {transitioning}") # ラベル

# gif画像を作成
gganimate::animate(plot = anim, nframes = (N_frame+1)*10, fps = 10)


# 利用例：Data Saurus ---------------------------------------------------------------------

# 利用パッケージ
library(gganimate)
library(datasauRus)
library(tidyverse)


# 作図用のデータセットを作成
df <- datasauRus::datasaurus_dozen %>% 
  tibble::add_column(
    n = rep(sample(x = 1:142, size = 142, replace = FALSE), times = 13), 
    id = rep(1:13, each = 142)
  ) # データ番号とデータセット番号を追加

# アニメーションを作成
anim <- ggplot(df, aes(x = x, y = y, color = as.factor(n))) + 
  geom_point(size = 3, show.legend = FALSE) + # 散布図
  gganimate::transition_states(states = id, transition_length = 9, state_length = 1, wrap = TRUE) + # フレーム
  gganimate::ease_aes("elastic-in") + # 変化の緩急
  coord_fixed(ratio = 1) + # アスペクト比
  xlim(c(0, 100)) + # x軸の表示範囲
  ylim(c(0, 100)) + # y軸の表示範囲
  labs(title = paste0('transition_states() + ease_aes("elastic-in")'), 
       subtitle = "transition : {transitioning}", 
       caption = "datasauRus")

# gif画像を作成
gganimate::animate(plot = anim, nframes = 140, fps = 10)


# バーチャートレース ---------------------------------------------------------------

## BarCharRace.Rを参照してください。

