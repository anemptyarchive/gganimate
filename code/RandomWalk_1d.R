
# ランダムウォークの作図 -------------------------------------------------------------

# 利用パッケージ
library(gganimate)
library(tidyverse)

# パッケージ名の省略用
library(ggplot2)


# 1次元ランダムウォーク ----------------------------------------------------------------

# 試行回数を指定
max_iter <- 600

# サンプルサイズを指定
sample_size <- 12

# 乱数の生成と集計
trace_df <- tibble::tibble(
  id =  factor(1:sample_size) # サンプル番号
) |> 
  dplyr::reframe(
    iter = 0:max_iter, # 試行番号
    r    = c(0, sample(x = c(-1, 1), size = max_iter, replace = TRUE)), # 初期値0, 乱数±1
    .by = id
  ) |> # 移動量を生成
  dplyr::mutate(
    y = cumsum(r), .by = id
  )  # 総移動量を計算


# グラフサイズを設定
axis_size <- trace_df[["y"]] |> 
  abs() |> 
  max()


# 最終結果を抽出
res_df <- trace_df |> 
  dplyr::filter(iter == max_iter)

# 1次元ランダムウォークを作図
graph <- ggplot() + 
  geom_path(data = trace_df, mapping = aes(x = iter, y = y, color = id), 
            size = 1, alpha = 0.3) + # 軌跡
  geom_point(data = res_df, 
             mapping = aes(x = iter, y = y, color = id), 
             size = 4) + # 最終地点
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 初期値
  guides(color = "none") + 
  coord_cartesian(ylim = c(-axis_size, axis_size)) + # 描画範囲
  labs(title = "Random Walk", 
       subtitle = paste("iteration:", max_iter), 
       x = "iteration", y = "y")
graph

# グラフを書出
ggplot2::ggsave(
  plot = graph, filename = "output/RandomWalk/1d.png", 
  width = 12, height = 9, units = "in", dpi = 250
)


# 1次元ランダムウォークのアニメーションを作図
anim <- ggplot() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 初期値
  geom_path(data = trace_df, 
            mapping = aes(x = iter, y = y, color = id), 
            size = 1, alpha = 0.3) + # 軌跡
  geom_point(data = trace_df, 
             mapping = aes(x = iter, y = y, color = id), 
             size = 4) + # 現在地点
  gganimate::transition_reveal(along = iter) + # フレーム切替
  guides(color = "none") + 
  coord_cartesian(ylim = c(-axis_size, axis_size)) + # 描画範囲
  labs(title = "Random Walk", 
       subtitle = "iteration: {frame_along}", 
       x = "iteration", y = "y")

# mp4動画を作成
gganimate::animate(
  plot = anim, nframes = max_iter+1, fps = 10, 
  width = 12, height = 9, units = "in", res = 250, 
  renderer = gganimate::av_renderer(file = "output/RandomWalk/1d.mp4")
)


# 2次元ランダムウォーク：2方向移動 -------------------------------------------------------------

# 試行回数を指定
max_iter <- 600

# サンプルサイズを指定
sample_size <- 12

# 乱数の生成と集計
trace_df <- tibble::tibble(
  id = factor(1:sample_size) # サンプル番号
) |> 
  dplyr::reframe(
    iter = c(0, 0, 1:max_iter), # 試行番号
    r    = c(0, 0, sample(x = c(-1, 1), size = max_iter, replace = TRUE)), # 初期値0, 乱数±1
    axis = c("x", "y", sample(x = c("x", "y"), size = max_iter, replace = TRUE)), # x軸・y軸方向
    .by = id
  ) |> # 移動量を生成
  tidyr::pivot_wider(
    names_from  = axis, 
    values_from = r, 
    values_fill = 0
  ) |> # 軸ごとの列に分割
  dplyr::mutate(
    x = cumsum(x), 
    y = cumsum(y), 
    .by = id
  ) # 総移動量を計算


# グラフサイズを設定
axis_size <- c(trace_df[["x"]], trace_df[["y"]]) |> 
  abs() |> 
  max()


# 最終結果を抽出
res_df <- trace_df |> 
  dplyr::filter(iter == max_iter)

# 2次元ランダムウォークを作図
graph <- ggplot() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 初期値
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") + # 初期値
  geom_path(data = trace_df, 
            mapping = aes(x = x, y = y, color = id), 
            size = 1, alpha = 0.3) + # 軌跡
  geom_point(data = res_df, 
             mapping = aes(x = x, y = y, color = id), 
             size = 4) + # 現在地点
  guides(color = "none") + 
  coord_fixed(ratio = 1, 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + # アスペクト比
  labs(title = "Random Walk", 
       subtitle = paste("iteration:", max_iter), 
       x = "x", y = "y")
graph

# グラフを書出
ggplot2::ggsave(
  plot = graph, filename = "output/RandomWalk/2d_90.png", 
  width = 10, height = 10, units = "in", dpi = 250
)


# 2次元ランダムウォークのアニメーションを作図
anim <- ggplot() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 初期値
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") + # 初期値
  geom_path(data = trace_df, 
            mapping = aes(x = x, y = y, color = id), 
            size = 1, alpha = 0.3) + # 軌跡
  geom_point(data = trace_df, 
             mapping = aes(x = x, y = y, color = id), 
             size = 4) + # 現在地点
  gganimate::transition_reveal(along = iter) + # フレーム切替
  guides(color = "none") + 
  coord_fixed(ratio = 1, 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + # アスペクト比
  labs(title = "Random Walk", 
       subtitle = "iteration: {frame_along}", 
       x = "x", y = "y")

# mp4動画を作成
gganimate::animate(
  plot = anim, nframes = max_iter+1, fps = 10, 
  width = 10, height = 10, units = "in", res = 250, 
  renderer = gganimate::av_renderer(file = "output/RandomWalk/2d_90.mp4")
)


# 2次元ランダムウォーク：全方向移動 -------------------------------------------------------------

# 試行回数を指定
max_iter <- 600

# サンプルサイズを指定
sample_size <- 12

# 乱数の生成と集計
trace_df <- tibble::tibble(
  id = factor(1:sample_size) # サンプル番号
) |> 
  dplyr::reframe(
    iter = 0:max_iter, # 試行番号
    t    = c(NA, runif(n = max_iter, min = 0, max = 2*pi)), # 初期値NA, 乱数0から2π
    .by = id
  ) |> # 移動量用の乱数を生成
  dplyr::mutate(
    x = dplyr::if_else(iter > 0, true = cos(t), false = 0), 
    y = dplyr::if_else(iter > 0, true = sin(t), false = 0)
  ) |> # 移動量を計算
  dplyr::mutate(
    x = cumsum(x), 
    y = cumsum(y), .by = id
  ) # 総移動量を計算


# 最大移動量を取得
max_val <- c(trace_df[["x"]], trace_df[["y"]]) |> 
  abs() |> 
  max()

# 目盛間隔を設定
tick_major_val <- 25
tick_minor_val <- 0.5 * tick_major_val

# グラフサイズを設定:(目盛間隔で切り上げ)
axis_size <- ceiling(max_val / tick_minor_val) * tick_minor_val

# ノルム目盛線の座標を作成
circle_df <- tidyr::expand_grid(
  r = seq(from = tick_minor_val, to = axis_size, by = tick_minor_val), # 半径
  t = seq(from = 0, to = 2*pi, length.out = 361), # 1周期分のラジアン
) |> # ノルムごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t), 
    grid  = dplyr::if_else(
      r%%tick_major_val == 0, true = "major", false = "minor"
    ) # 目盛カテゴリ
  ) # 円周の座標を計算

# 半円(範囲π)における目盛数を指定
tick_num <- 6

# 角度目盛線の座標を作成
diag_df <- tibble::tibble(
  i = seq(from = 0, to = tick_num-0.5, by = 0.5), # 目盛番号
  t = i/tick_num * pi, # 半周期分のラジアン
  a = tan(t), # 目盛線の傾き
  grid = dplyr::if_else(
    i%%1 == 0, true = "major", false = "minor"
  ) # 目盛カテゴリ
)


# 最終結果を抽出
res_df <- trace_df |> 
  dplyr::filter(iter == max_iter)

# 2次元ランダムウォークを作図
graph <- ggplot() + 
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y, linewidth = grid, group = r), 
            color = "white", show.legend = FALSE) + # ノルム目盛線
  geom_abline(data = diag_df, 
              mapping = aes(slope = a, intercept = 0, linewidth = grid), 
              color = "white", show.legend = FALSE) + # 角度目盛線
  geom_path(data = trace_df, 
            mapping = aes(x = x, y = y, color = id), 
            linewidth = 0.8, alpha = 0.3) + # 軌跡
  geom_point(data = res_df, 
             mapping = aes(x = x, y = y, color = id), 
             size = 4) + # 最終地点
  scale_linewidth_manual(breaks = c("major", "minor"), 
                         values = c(0.5, 0.25)) + # 主・補助目盛線用
  guides(color = "none") + 
  coord_fixed(ratio = 1, 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + # アスペクト比
  labs(title = "Random Walk", 
       subtitle = paste("iteration: ", max_iter), 
       x = "x", y = "y")
graph

# グラフを書出
ggplot2::ggsave(
  plot = graph, filename = "output/RandomWalk/2d_360.png", 
  width = 10, height = 10, units = "in", dpi = 250
)


# 2次元ランダムウォークのアニメーションを作図
anim <- ggplot() + 
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y, linewidth = grid, group = r), 
            color = "white", show.legend = FALSE) + # ノルム目盛線
  geom_abline(data = diag_df, 
              mapping = aes(slope = a, intercept = 0, linewidth = grid), 
              color = "white", show.legend = FALSE) + # 角度目盛線
  geom_path(data = trace_df,
            mapping = aes(x = x, y = y, color = id),
            linewidth = 0.8, alpha = 0.3) + # 軌跡
  geom_point(data = trace_df,
             mapping = aes(x = x, y = y, color = id),
             size = 4, show.legend = FALSE) + # 現在地点
  gganimate::transition_reveal(along = iter) + # フレーム切替
  scale_color_manual(breaks = factor(1:sample_size), values = color_df[["color_code"]]) + # (資料作成用)
  scale_linewidth_manual(breaks = c("major", "minor"), 
                         values = c(0.5, 0.25)) + # 主・補助目盛線用
  guides(color = "none") + 
  coord_fixed(ratio = 1, 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + # アスペクト比
  labs(title = "Random Walk", 
       subtitle = "iteration: {frame_along}", 
       color = "id", 
       x = "x", y = "y")

# 動画を作成
gganimate::animate(
  plot = anim, nframes = max_iter+1, fps = 10, 
  width = 10, height = 10, units = "in", res = 250, 
  renderer = gganimate::av_renderer(file = "output/RandomWalk/2d_360.mp4")
)


