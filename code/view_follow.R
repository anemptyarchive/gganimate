
# view_follow -------------------------------------------------------------

# 利用パッケージ
library(gganimate)
library(tidyverse)

# チェック用
library(magrittr)
library(ggplot2)


# データフレームの作成 --------------------------------------------------------------

# データフレームを作成
df <- tibble::tibble(
  x = 0:5, 
  y = 0:5, 
  frame = 0:5
)

# 散布図を作成
ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 5, color = "hotpink") + # 散布図
  geom_path(color = "hotpink") + # 折れ線
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "geom_point() + geom_path()")


# 引数 ----------------------------------------------------------------------


### ・fixed -----

# 論理値を指定:(デフォルト:FALSE)
fx <- TRUE
fx <- FALSE
fy <- TRUE
fy <- FALSE


# fixed(描画範囲の固定)の設定
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  geom_path(size = 1, color = "hotpink") + # 折れ線
  gganimate::transition_manual(frame = frame, cumulative = TRUE) + # フレーム
  gganimate::view_follow(fixed_x = fx, fixed_y = fy) + # 描画範囲
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  labs(title = paste0("transition_manual() + view_follow(fixed_x = ", fx, ", fixed_y = ", fy, ")"), 
       subtitle = paste0("frame : {current_frame}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 6, fps = 1)


# fixed(描画範囲の固定)の設定
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  geom_path(size = 1, color = "hotpink") + # 折れ線
  gganimate::transition_reveal(along = frame) + # フレーム
  gganimate::view_follow(fixed_x = fx, fixed_y = fy) + # 描画範囲
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  labs(title = paste0("transition_reveal() + view_follow(fixed_x = ", fx, ", fixed_y = ", fy, ")"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 60, fps = 10)


# fixed(描画範囲の固定)の設定
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  gganimate::transition_states(states = frame) + # フレーム
  gganimate::view_follow(fixed_x = fx, fixed_y = fy) + # 描画範囲
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  labs(title = paste0("transition_states() + view_follow(fixed_x = ", fx, ", fixed_y = ", fy, ")"), 
       subtitle = paste0("frame : {closest_state}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 60, fps = 10)


# 値を指定:(デフォルト:FALSE)
fx <- FALSE
fx <- c(-1, 10)
fy <- FALSE
fy <- c(2, NA)

# fixed(描画範囲の固定)の設定
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  geom_path(size = 1, color = "hotpink") + # 折れ線
  gganimate::transition_reveal(along = frame) + # フレーム
  gganimate::view_follow(fixed_x = fx, fixed_y = fy) + # 描画範囲
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  labs(
    title = paste0(
      "transition_reveal() + ", 
      "view_follow(fixed_x = c(", paste0(fx, collapse = ", "), "), fixed_y = c(", paste0(fy, collapse = ", "), "))"
    ), 
    subtitle = paste0("frame : {frame_along}")
  )

# gif画像を作成
gganimate::animate(plot = anim, nframes = 60, fps = 10)


### ・exclude_layer：謎 -----
tmp_df <- df %>% 
  dplyr::select(!frame)
# 値を指定:(デフォルト：NULL)
el <- NULL
el <- c(1L)

# exclude_layer(除外するレイヤ)の設定
anim <- ggplot(df, aes(x = x)) + 
  geom_point(aes(y = y+1), size = 10, color = "hotpink") + # 散布図
  geom_point(aes(y = y*2), size = 10, color = "orange") + # 散布図
  gganimate::transition_manual(frame = frame, cumulative = TRUE) + # フレーム
  gganimate::view_follow(exclude_layer = el) + # 描画範囲
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_manual() + view_follow(exclude_layer = c(", paste0(el, collapse = ", "), "))"), 
       subtitle = paste0("frame : {current_frame}"), 
       y = "y")

# gif画像を作成
gganimate::animate(plot = anim, nframes = 6, fps = 1)


### ・aspect_ratio -----

# 正方形の点を作成
square_df <- tibble::tibble(
  x = c(1, 1, -1, -1, 1), 
  y = c(1, -1, -1, 1, 1), 
  frame = 1:5
)

# 値を指定:(デフォルト：NULL)
ar <- 0.2

# aspect_ratio(アスペクト比)の設定
anim <- ggplot(square_df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 棒グラフ
  geom_path(size = 1, color = "hotpink") + # 折れ線
  gganimate::transition_reveal(along = frame) + # フレーム
  gganimate::view_follow(aspect_ratio = ar) + # 描画範囲
  coord_fixed(ratio = 1) + # アスペクト比
  scale_x_continuous(breaks = square_df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = square_df[["y"]], minor_breaks = FALSE) + # y軸目盛
  labs(title = paste0("transition_reveal() + view_follow(aspect_ratio = ", ar, ")"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 40, fps = 10)


# フレーム数を指定
N <- 101

# 単位円の点を作成
circle_df <- tibble::tibble(
  r = seq(from = pi*2.5, to = pi*0.5, length.out = N), 
  x = cos(r), 
  y = sin(r), 
  frame = 1:N
)

# 値を指定:(デフォルト：NULL)
ar <- 5

# aspect_ratio(アスペクト比)の設定
anim <- ggplot(circle_df, aes(x = x, y = y)) + 
  geom_point(size = 5, color = "hotpink") + # 散布図
  geom_path(size = 1, color = "hotpink") + # 折れ線
  gganimate::transition_reveal(along = frame) + # フレーム
  gganimate::view_follow(aspect_ratio = ar) + # 描画範囲
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal() + view_follow(aspect_ratio = ", ar, ")"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = N, fps = 10)


# 利用例：確率分布 --------------------------------------------------------------------

# x軸の値を作成
x_vals <- seq(from = -5, to = 5, by = 0.1)

# 標準偏差として利用する値を作成
sd_vec <- seq(from = 0.5, to = 3, by = 0.2)

# 標準偏差ごとにガウス分布の計算
dens_df <- tibble::tibble()
for(sd in sd_vec) {
  # ガウス分布の確率密度を計算
  tmp_df <- tibble::tibble(
    x = x_vals, 
    sigma = sd, 
    density = dnorm(x = x_vals, mean = 0, sd = sd)
  )
  
  # 計算結果を結合
  dens_df <- rbind(dens_df, tmp_df)
}

# ガウス分布のアニメーションを作成:折れ線グラフ
anim <- ggplot(dens_df, aes(x = x, y = density, color = as.factor(sigma), group = sigma)) + 
  geom_point(show.legend = FALSE) + # 散布図
  geom_line(show.legend = FALSE) + # 折れ線グラフ
  gganimate::transition_reveal(along = x) + # フレーム
  gganimate::view_follow() + # 描画範囲
  labs(title = "transition_reveal() + view_follow()", 
       subtitle = "x = {round(frame_along, 1)}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = length(x_vals), fps = 10)


# 利用例：ランダムウォーク ----------------------------------------------------------------

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
  gganimate::view_follow() + # 描画範囲
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "transition_reveal() + view_follow()", 
       subtitle = "iter : {frame_along}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = max_iter+1, fps = 10)


