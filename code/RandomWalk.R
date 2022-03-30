
# ランダムウォークの作図 -------------------------------------------------------------

# 利用パッケージ
library(gganimate)
library(tidyverse)


# 1次元ランダムウォーク ----------------------------------------------------------------

### ・1サンプル -----

# 試行回数を指定
max_iter <- 100

# ランダムに値を生成
random_vec <- sample(x = c(-1, 1), size = max_iter, replace = TRUE)

# 試行ごとに集計
random_df <- tibble::tibble(
  iteration = 0:max_iter, 
  random_val = c(0, random_vec)
) %>% 
  dplyr::mutate(value = cumsum(random_val)) # 各試行までの合計

# 1次元ランダムウォークを作図
anim <- ggplot(random_df, aes(x = iteration, y = value)) + 
  geom_point(color = "hotpink", size = 5) + # 散布図
  geom_path(color = "hotpink", size = 1, alpha = 0.5) + # 折れ線
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 水平線
  gganimate::transition_reveal(along = iteration) + # フレーム
  labs(title = "Random Walk", 
       subtitle = "iter : {frame_along}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = max_iter+1, fps = 10)


### ・複数サンプル -----

# 試行回数を指定
max_iter <- 100

# サンプルサイズを指定
sample_size <- 10

# ランダムに値を生成
random_vec <-  sample(x = c(-1, 1), size = max_iter*sample_size, replace = TRUE)

# 試行ごとに集計
random_df <- tibble::tibble(
  iteration = rep(0:max_iter, each = sample_size), 
  id = rep(1:sample_size, times = max_iter+1) %>% 
    as.factor(), 
  random_val = c(rep(0, sample_size), random_vec)
) %>% 
  dplyr::group_by(id) %>% # サンプルごとにグループ化
  dplyr::mutate(value = cumsum(random_val)) %>% # 各試行までの合計
  dplyr::group_by() # グループ化の解除

# 2次元ランダムウォークを作図
anim <- ggplot(random_df, aes(x = iteration, y = value, color = id)) + 
  geom_point(size = 5, show.legend = FALSE) + # 散布図
  geom_path(size = 1, alpha = 0.5, show.legend = FALSE) + # 折れ線
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 水平線
  gganimate::transition_reveal(along = iteration) + # フレーム
  labs(title = "Random Walk", 
       subtitle = "iter : {frame_along}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = max_iter+1, fps = 10)


# 最終結果
random_df %>% 
  dplyr::filter(iteration == max_iter) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = iteration, y = value, color = id), 
             size = 5, show.legend = FALSE) + # 散布図
  geom_path(data = random_df, mapping = aes(x = iteration, y = value, color = id), 
            size = 1, alpha = 0.5, show.legend = FALSE) + # 折れ線
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 水平線
  labs(title = "Random Walk", 
       subtitle = paste0("iter : ", max_iter))


# 2次元ランダムウォーク：2方向移動 -------------------------------------------------------------

### ・1サンプル -----

# 試行回数を指定
max_iter <- 100

# ランダムに値を生成
random_val_vec <- sample(c(-1, 1), size = max_iter, replace = TRUE)

# ランダムに軸を生成
random_axis_vec <- sample(c("x", "y"), size = max_iter, replace = TRUE)

# 試行ごとに集計
random_df <- tibble::tibble(
  iteration = c(0, 0, 1:max_iter), 
  random_val = c(0, 0, random_val_vec), 
  axis = c("x", "y", random_axis_vec)
) %>% 
  tidyr::pivot_wider(
    names_from = axis, 
    values_from = random_val, 
    values_fill = 0
  ) %>% # 横型に変形
  dplyr::mutate(
    x = cumsum(x), 
    y = cumsum(y)
  ) # 各試行までの合計

# 2次元ランダムウォークを作図
anim <- ggplot(random_df, aes(x = x, y = y)) + 
  geom_point(color = "hotpink", size = 5) + # 散布図
  geom_path(color = "hotpink", size = 1, alpha = 0.5) + # 折れ線
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 水平線
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") + # 垂直線
  gganimate::transition_reveal(along = iteration) + # フレーム
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Random Walk", 
       subtitle = "iter : {frame_along}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = max_iter+1, fps = 10)


### ・複数サンプル -----

# 試行回数を指定
max_iter <- 100

# サンプルサイズを指定
sample_size <- 10

# ランダムに値を生成
random_value_vec <- sample(x = c(-1, 1), size = max_iter*sample_size, replace = TRUE)

# ランダムに軸を生成
random_axis_vec <- sample(x = c("x", "y"), size = max_iter*sample_size, replace = TRUE)

# 試行ごとに集計
random_df <- tidyr::tibble(
  iteration = rep(c(0, 0, 1:max_iter), each = sample_size), 
  id = rep(1:sample_size, times = max_iter+2) %>% 
    as.factor(), 
  random_val = c(rep(0, times = sample_size*2), random_value_vec), 
  axis = c(rep(c("x", "y"), each = sample_size), random_axis_vec)
) %>% 
  tidyr::pivot_wider(
    names_from = axis, 
    values_from = random_val, 
    values_fill = 0
  ) %>% # 横型に変形
  dplyr::group_by(id) %>% # サンプルごとにグループ化
  dplyr::mutate(
    x = cumsum(x), 
    y = cumsum(y)
  ) %>%  # 各試行までの合計
  dplyr::ungroup() # グループ化の解除

# 2次元ランダムウォークを作図
anim <- ggplot(random_df, aes(x = x, y = y, color = id)) + 
  geom_point(size = 5, show.legend = FALSE) + # 散布図
  geom_path(size = 1, alpha = 0.3, show.legend = FALSE) + # 折れ線
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 水平線
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") + # 垂直線
  gganimate::transition_reveal(along = iteration) + # フレーム
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Random Walk", 
       subtitle = "iter : {frame_along}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = max_iter+1, fps = 10)


# 最終結果
random_df %>% 
  dplyr::filter(iteration == max_iter) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = x, y = y, color = id), 
             size = 5, show.legend = FALSE) + # 散布図
  geom_path(data = random_df, mapping = aes(x = x, y = y, color = id), 
            size = 1, alpha = 0.3, show.legend = FALSE) + # 折れ線
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 水平線
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") + # 垂直線
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Random Walk", 
       subtitle = paste0("iter : ", max_iter))


# 2次元ランダムウォーク：全方向移動 -------------------------------------------------------------

### ・全方向移動の確認 -----

# フレーム数を指定
N <- 101

# 単位円上の点を作成
df <- tibble::tibble(
  n = 1:N, # 時計回りならN:1
  d = seq(from = 0, to = 1, length.out = N), 
  r = 2 * pi * d, 
  x = cos(r), 
  y = sin(r)
)


# 単位円上の点を作図
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(color = "red", size = 5) + # 散布図
  geom_path(color = "hotpink") + # 折れ線
  gganimate::transition_reveal(d) + # フレーム
  coord_fixed(ratio = 1) + # アスペクト比
  labs(subtitle = "d = {frame_along}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N, fps = 10)


# 原点から単位円上の点の方向を作図
anim <- df %>% 
  rbind(tibble::tibble(n = 1:N, d = NA, r = NA, x = 0, y = 0)) %>% # 原点を追加
  ggplot(aes(x = x, y = y)) + 
  geom_line(color = "hotpink") + # 折れ線グラフ
  geom_point(color = "red", fill = "red", size = 5) + # 散布図
  gganimate::transition_manual(n) + # フレーム
  coord_fixed(ratio = 1) + # アスペクト比
  labs(subtitle = "n = {current_frame}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = N, fps = 10)


### ・1サンプル -----

# 試行回数を指定
max_iter <- 100

# ランダムに値を生成
random_vec <- 2 * pi * runif(n = max_iter, min = 0, max = 1)

# 試行ごとに集計
random_df <- tibble::tibble(
  iteration = 0:max_iter, 
  x = c(0, cos(random_vec)), 
  y = c(0, sin(random_vec))
) %>% 
  dplyr::mutate(
    x = cumsum(x), 
    y = cumsum(y)
  ) # 各試行までの合計

# 2次元ランダムウォークを作図
anim <- ggplot(random_df, aes(x = x, y = y)) + 
  geom_point(color = "hotpink", size = 5) + # 散布図
  geom_path(color = "hotpink", size = 1, alpha = 0.5) + # 折れ線
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 水平線
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") + # 垂直線
  gganimate::transition_reveal(along = iteration) + # フレーム
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Random Walk", 
       subtitle = "iter : {frame_along}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = max_iter+1, fps = 10)


### ・複数サンプル -----

# 試行回数を指定
max_iter <- 100

# サンプルサイズを指定
sample_size <- 10

# ランダムに値を生成
random_vec <- 2 * pi * runif(n = max_iter*sample_size, min = 0, max = 1)

# 試行ごとに集計
random_df <- tibble::tibble(
  iteration = rep(0:max_iter, each = sample_size), 
  id = rep(1:sample_size, times = max_iter+1) %>% 
    as.factor(), 
  x = c(rep(0, times = sample_size), cos(random_vec)), 
  y = c(rep(0, times = sample_size), sin(random_vec))
) %>% 
  dplyr::group_by(id) %>% # サンプルごとにグループ化
  dplyr::mutate(
    x = cumsum(x), 
    y = cumsum(y)
  ) %>% # 各試行までの合計
  dplyr::ungroup() # グループ化の解除

# 2次元ランダムウォークを作図
anim <- ggplot(random_df, aes(x = x, y = y, color = id)) + 
  geom_point(size = 5, show.legend = FALSE) + # 散布図
  geom_path(size = 1, alpha = 0.3, show.legend = FALSE) + # 折れ線
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 水平線
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") + # 垂直線
  gganimate::transition_reveal(along = iteration) + # フレーム
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Random Walk", 
       subtitle = "iter : {frame_along}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = max_iter+1, fps = 10)


# 最終結果
random_df %>% 
  dplyr::filter(iteration == max_iter) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = x, y = y, color = id), 
             size = 5, show.legend = FALSE) + # 散布図
  geom_path(data = random_df, mapping = aes(x = x, y = y, color = id), 
            size = 1, alpha = 0.3, show.legend = FALSE) + # 折れ線
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + # 水平線
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") + # 垂直線
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Random Walk", 
       subtitle = paste0("iter : ", max_iter))


