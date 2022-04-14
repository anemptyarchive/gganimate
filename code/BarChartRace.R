
# バーチャートレースの作図 ------------------------------------------------------------

# 利用パッケージ
library(gganimate)
library(tidyverse)

# チェック用
library(magrittr)
library(ggplot2)


# データの準備 ------------------------------------------------------------------

# 試行回数を指定
N <- 100

# 面の数を指定
V <- 6

# サイコロを振る
result_mat <- rmultinom(n = N, size = 1, prob = rep(1/V, times = V))

# 出現回数で順位付け
rank_df <- tibble::tibble(
  iteration = rep(0:N, each = V), 
  v = rep(1:V, times = N+1) %>% 
    factor(), 
  result = c(rep(0, times = V), as.vector(result_mat))
) %>% # 0回目の結果を追加
  dplyr::group_by(v) %>% # 面でグループ化
  dplyr::mutate(count = cumsum(result)) %>% # 試行ごとに集計
  dplyr::group_by(iteration) %>% # 試行回数でグループ化
  dplyr::mutate(ranking = dplyr::row_number(-count)) %>% # 出現回数で順位付け
  dplyr::ungroup() # グループ化の解除


# アニメーションの作成 --------------------------------------------------------------

### ・バーチャートレース：y軸固定 -----

# 遷移フレーム数を指定
t <- 10

# 実データでの停止フレーム数を指定
s <- 5

# 最終結果での停止フレーム数を指定
e <- 50

# バーチャートレースを作成:(y軸固定)
anim <- ggplot(rank_df, aes(x = ranking, y = count, fill = v, color = v)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 出現回数のバー
  geom_text(aes(y = 0, label = paste(v, " ")), hjust = 1) + # カテゴリ名
  geom_text(aes(y = 0, label = paste(" ", round(count))), hjust = 0, color = "white") + # 出現回数
  gganimate::transition_states(states = iteration, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  scale_fill_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # バーの色:(不必要)
  scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  theme(
    axis.title.y = element_blank(), # 縦軸のラベル
    axis.text.y = element_blank(), # 縦軸の目盛ラベル
    #panel.grid.major.x = element_line(color = "grey", size = 0.1), # 横軸の主目盛線
    panel.grid.major.y = element_blank(), # 縦軸の主目盛線
    panel.grid.minor.x = element_blank(), # 横軸の補助目盛線
    panel.grid.minor.y = element_blank(), # 縦軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 10, b = 10, l = 40, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  coord_flip(clip = "off", expand = FALSE) + # 軸の入れ替え
  scale_x_reverse(breaks = 1:V) + # x軸を反転
  labs(title = "Bar Chart Race", 
       subtitle = "iteration = {closest_state}") # ラベル

# gif画像を作成
gganimate::animate(
  plot = anim, 
  nframes = (N+1)*(t+s)+e, end_pause = e, fps = (t+s)*2, 
  width = 800, height = 600
)


### ・バーチャートレース：y軸可変 -----

# 遷移フレーム数を指定
t <- 10

# 実データでの停止フレーム数を指定
s <- 5

# 最終結果での停止フレーム数を指定
e <- 50

# バーチャートレースを作成:(y軸可変)
anim <- ggplot(rank_df, aes(x = ranking, y = count, fill = v, color = v)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 出現回数のバー
  geom_text(aes(y = 0, label = paste(v, " ")), hjust = 1) + # カテゴリ名
  geom_text(aes(label = paste(" ", round(count))), hjust = 0) + # 出現回数
  gganimate::transition_states(states = iteration, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  scale_fill_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # バーの色:(不必要)
  scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  theme(
    axis.title.x = element_blank(), # 横軸のラベル
    axis.title.y = element_blank(), # 縦軸のラベル
    axis.text.x = element_blank(), # 横軸の目盛ラベル
    axis.text.y = element_blank(), # 縦軸の目盛ラベル
    axis.ticks.x = element_blank(), # 横軸の目盛指示線
    axis.ticks.y = element_blank(), # 縦軸の目盛指示線
    #panel.grid.major.x = element_line(color = "grey", size = 0.1), # 横軸の主目盛線
    panel.grid.major.y = element_blank(), # 縦軸の主目盛線
    panel.grid.minor.x = element_blank(), # 横軸の補助目盛線
    panel.grid.minor.y = element_blank(), # 縦軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 40, b = 10, l = 40, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  coord_flip(clip = "off", expand = FALSE) + # 軸の入れ替え
  scale_x_reverse() + # x軸を反転
  gganimate::view_follow(fixed_x = TRUE) + # フレームごとに表示範囲を調整
  labs(title = "Bar Chart Race", 
       subtitle = "iteration = {closest_state}") # ラベル

# gif画像を作成
gganimate::animate(
  plot = anim, 
  nframes = (N+1)*(t+s)+e, end_pause = e, fps = (t+s)*2, 
  width = 800, height = 600
)

warnings()


### ・ラインチャートレース：順位 -----

# 遷移フレーム数を指定
t <- 10

# 最終結果での停止フレーム数を指定
e <- 50

# ラインチャートレースを作成
anim <- ggplot(rank_df, aes(x = iteration, y = ranking, color = v)) + 
  geom_line(size = 1, alpha = 0.5) + # 順位の推移
  geom_point(size = 5) + # 順位
  geom_segment(mapping = aes(xend = N, yend = ranking), linetype = "dashed") + # 指示線
  geom_text(mapping = aes(x = N, label = paste("  ", v)), hjust = 0, vjust = 0) + # カテゴリ名
  geom_text(mapping = aes(label = paste("  ", count)), hjust = 0, vjust = 1) + # 出現回数
  gganimate::transition_reveal(along = iteration) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  scale_y_reverse(breaks = 1:V) + # x軸を反転
  scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  theme(
    panel.grid.minor.y = element_blank(), # 縦軸の補助目盛線
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  xlim(c(0, N+5)) + # x軸の表示範囲
  labs(title = "Line Chart Race", 
       subtitle ="iteration = {frame_along}") # ラベル

# gif画像を作成
gganimate::animate(
  plot = anim, 
  nframes = (N+1)*t+e, end_pause = e, fps = t*5, 
  width = 800, height = 600
)


### ・ラインチャートレース：出現回数 -----

# 遷移フレーム数を指定
t <- 10

# 最終結果での停止フレーム数を指定
e <- 50

# ラインチャートレースを作成
anim <- ggplot(rank_df, aes(x = iteration, y = count, color = v)) + 
  geom_line(size = 1, alpha = 0.5) + # 出現回数の推移
  geom_point(size = 5, alpha = 0.5) + # 出現回数
  geom_segment(mapping = aes(xend = N, yend = count), linetype = "dashed") + # 指示線
  geom_text(mapping = aes(x = N, label = paste("  ", v)), hjust = 0, vjust = 0) + # カテゴリ名
  geom_text(mapping = aes(label = paste("  ", count)), hjust = 0, vjust = 1) + # 出現回数
  gganimate::transition_reveal(along = iteration) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  scale_color_manual(values = c("pink", "limegreen", "red", "orange", "mediumblue", "yellow")) + # 枠線の色:(不必要)
  theme(
    panel.grid.minor.y = element_blank(), # 縦軸の補助目盛線
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  xlim(c(0, N+5)) + # x軸の表示範囲
  labs(title = "Line Chart Race", 
       subtitle ="iteration = {frame_along}") # ラベル

# gif画像を作成
gganimate::animate(
  plot = anim, 
  nframes = (N+1)*t+e, end_pause = e, fps = t*5, 
  width = 800, height = 600
)


