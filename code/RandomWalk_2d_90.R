
# 2次元(2方向)ランダムウォークの作図 -------------------------------------------------------------

# 利用パッケージ
library(gganimate)
library(tidyverse)

# パッケージ名の省略用
library(ggplot2)


# 作図 -------------------------------------------------------------

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

# 2Dランダムウォークを作図
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


# 2Dランダムウォークのアニメーションを作図
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


