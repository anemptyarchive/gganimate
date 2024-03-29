
# イージング --------------------------------------------------------------

# 利用パッケージ
library(gganimate)
library(ggplot2)

# チェック用
library(magrittr)
library(ggplot2)


# 元の関数の作図 -----------------------------------------------------------------

# x軸の値を作成
x_vec <- seq(from = -2, to = 2, by = 0.01)

# 元の関数のデータフレームを作成
tmp_df <- tibble::tibble(
  x = x_vec, 
  y = x
)

# イージング関数として利用する領域用のデータフレームを作成
area_df <- tibble::tibble(
  x_from = c(0, 0, 1, 1), 
  y_from = c(0, 1, 1, 0), 
  x_to = c(0, 1, 1, 0), 
  y_to = c(1, 1, 0, 0)
)

# 元の関数を作図
ggplot() + 
  geom_line(data = tmp_df, mapping = aes(x = x, y = y), 
            color = "hotpink", size = 1) + # 元の関数
  geom_segment(data = area_df, mapping = aes(x = x_from, xend = x_to, y = y_from, yend = y_to), 
               color = "red", size = 1) + # 対象の領域
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = expression(f(x) == x), 
       y = expression(f(x))) # ラベル


# イージング関数の設定 --------------------------------------------------------------------

### ・時間ベクトルの設定 -----

# 時間変化の値を作成:(0 <= t <= 1)
t_vec <- seq(from = 0, to = 1, by = 0.01)
t_vec <- seq(from = 0, to = 1, length.out = 21)


### ・linear -----

# Linear
linear_df <- tibble::tibble(
  t = t_vec, 
  y = t, 
  easing_fnc = "linear"
)


### ・quadratic -----

# Quadratic-in
quad_in_df <- tibble::tibble(
  t = t_vec, 
  y = t^2, 
  easing_fnc = "quad-in"
)

# Quadratic-out
quad_out_df <- tibble::tibble(
  t = t_vec, 
  y = t * (2 - t), 
  easing_fnc = "quad-out"
)

# Quadratic-inout
quad_inout_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::if_else(
    condition = t < 0.5, 
    true = 2 * t^2, 
    false = 1 - (2 - 2 * t)^2 * 0.5
  ), 
  easing_fnc = "quad-in-out"
)


### ・cubic -----

# Cubic-in
cubic_in_df <- tibble::tibble(
  t = t_vec, 
  y = t^3, 
  easing_fnc = "cubic-in"
)

# Cubic-out
cubic_out_df <- tibble::tibble(
  t = t_vec, 
  y = 1 - (1 - t)^3, 
  easing_fnc = "cubic-out"
)

# Cubic-inout
cubic_inout_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::if_else(
    condition = t < 0.5, 
    true = 4 * t^3, 
    false = 1 - (2 - 2 * t)^3 * 0.5
  ), 
  easing_fnc = "cubic-in-out"
)


### ・quartic -----

# Quartic-in
quart_in_df <- tibble::tibble(
  t = t_vec, 
  y = t^4, 
  easing_fnc = "quart-in"
)

# Quartic-out
quart_out_df <- tibble::tibble(
  t = t_vec, 
  y = 1 - (1 - t)^4, 
  easing_fnc = "quart-out"
)

# Quartic-inout
quart_inout_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::if_else(
    condition = t < 0.5, 
    true = 8 * t^4, 
    false = 1 - (2 - 2 * t)^4 * 0.5
  ), 
  easing_fnc = "quart-in-out"
)


### ・quintic -----

# Quintic-in
quint_in_df <- tibble::tibble(
  t = t_vec, 
  y = t^5, 
  easing_fnc = "quint-in"
)

# Quintic-out
quint_out_df <- tibble::tibble(
  t = t_vec, 
  y = 1 - (1 - t)^5, 
  easing_fnc = "quint-out"
)

# Quintic-inout
quint_inout_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::if_else(
    condition = t < 0.5, 
    true = 16 * t^5, 
    false = 1 - (2 - 2 * t)^5 * 0.5
  ), 
  easing_fnc = "quint-in-out"
)


### ・exponential -----

# Exponential-in
expo_in_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::if_else(
    condition = t == 0, 
    true = 0, 
    false = 2^(10 * t - 10)
  ), 
  easing_fnc = "expo-in"
)

# Exponential-out
expo_out_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::if_else(
    condition = t == 1, 
    true = 1, 
    false = 1 - 2^(-10 * t)
  ), 
  easing_fnc = "expo-out"
)

# Exponential-inout
expo_inout_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::case_when(
    t == 0 ~ 0, 
    t == 1 ~ 1, 
    t < 0.5 ~ 2^(20 * t - 11), 
    t >= 0.5 ~ 1 - 2^(9 - 20 * t)
  ), 
  easing_fnc = "expo-in-out"
)


### ・sine -----

# Sine-in
sine_in_df <- tibble::tibble(
  t = t_vec, 
  y = 1 - cos(0.5 * pi * t), 
  easing_fnc = "sine-in"
)

# Sine-out
sine_out_df <- tibble::tibble(
  t = t_vec, 
  y = sin(0.5 * pi * t), 
  easing_fnc = "sine-out"
)

# Sine-inout
sine_inout_df <- tibble::tibble(
  t = t_vec, 
  y = (1 - cos(pi * t)) * 0.5, 
  easing_fnc = "sine-in-out"
)


### ・circular -----

# Circular-in
circ_in_df <- tibble::tibble(
  t = t_vec, 
  y = 1 - sqrt(1 - t^2), 
  easing_fnc = "circ-in"
)

# Circular-out
circ_out_df <- tibble::tibble(
  t = t_vec, 
  y = sqrt(t * (2 - t)), 
  easing_fnc = "circ-out"
)

# Circular-inout
circ_inout_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::if_else(
    condition = t < 0.5, 
    true = (1 - sqrt(1 - 4 * t^2)) * 0.5, 
    false = (sqrt(1 - (2 - 2 * t)^2) + 1) * 0.5
  ), 
  easing_fnc = "circ-in-out"
)


### ・back -----

# Back-in
back_in_df <- tibble::tibble(
  t = t_vec, 
  y = (1.70158 + 1) * t^3 - 1.70158 * t^2, 
  easing_fnc = "back-in"
)

# Back-out
back_out_df <- tibble::tibble(
  t = t_vec, 
  y = 1 - (1.70158 + 1) * (1 - t)^3 + 1.70158 * (1 - t)^2, 
  easing_fnc = "back-out"
)

# Back-inout
back_inout_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::if_else(
    condition = t < 0.5, 
    true = 4 * (1.70158 + 1) * t^3 - 2 * 1.70158 * t^2, 
    false = 1 - 4 * (1.70158 + 1) * (1 - t)^3 + 2 * 1.70158 * (1 - t)^2
  ), 
  easing_fnc = "back-in-out"
)


### ・elastic -----

# Elastic-in
elastic_in_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::if_else(
    condition = t == 0, 
    true = 0, 
    false = -2^(10 * t - 10) * sin((10 * t - 10.75) * 2 * pi / 3)
  ), 
  easing_fnc = "elastic-in"
)

# Elastic-out
elastic_out_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::if_else(
    condition = t == 1, 
    true = 1, 
    false = 1 - 2^(-10 * t) * sin((10 * t + 0.75) * 2 * pi / 3)
  ), 
  easing_fnc = "elastic-out"
)

# Elastic-inout:(未完)
elastic_inout_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::case_when(
    t == 0 ~ 0, 
    t == 1 ~ 1, 
    t < 0.5 ~ -2^(20 * t - 11) * sin((20 * t - 10.75) * 2 * pi / 3), 
    t >= 0.5 ~ 1 + 2^(-20 * t + 9) * sin((20 * t - 9.25) * 2 * pi / 3)
  ), 
  easing_fnc = "elastic-in-out"
)

# Elastic-in関数を作成
f_in <- function(t) {
  return(
    -2^(10 * t - 10) * sin((10 * t - 10.75) * 2 * pi / 3)
  )
}

# Elastic-in関数を作成
f_out <- function(t) {
  return(
    1 - 2^(-10 * t) * sin((10 * t + 0.75) * 2 * pi / 3)
  )
}

# Elastic-inout
elastic_inout_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::case_when(
    t == 0 ~ 0, 
    t == 1 ~ 1, 
    t < 0.5 ~ f_in(2 * t) * 0.5, 
    t >= 0.5 ~ (f_out(2 * t - 1) + 1) * 0.5
  ), 
  easing_fnc = "elastic-in-out"
)


### ・bounce -----

# Bounce-out関数を作成
f_out <- function(t_vec) {
  # 定数を設定
  c1 <- 7.5625
  c2 <- 2.75
  
  # 受け皿を作成
  y_vec <- rep(NA, times = length(t_vec))
  
  # 要素ごとに関数を計算
  for(i in seq_along(t_vec)) {
    # i番目の要素を取得
    t <- t_vec[i]
    
    # 定義式を計算
    if(t < 1 / c2) {
      y_vec[i] <- c1 * t^2
    } else if(t < 2 / c2) {
      y_vec[i] <- c1 * (t - 1.5 / c2)^2 + 0.75
    } else if(t < 2.5 / c2) {
      y_vec[i] <- c1 * (t - 2.25 / c2)^2 + 0.9375
    } else {
      y_vec[i] <- c1 * (t - 2.625 / c2)^2 + 0.984375
    }
  }
  
  # 出力
  return(y_vec)
}

# Bounce-in
bounce_in_df <- tibble::tibble(
  t = t_vec, 
  y = 1 - f_out(1 - t), 
  easing_fnc = "bounce-in"
)

# Bounce-out
bounce_out_df <- tibble::tibble(
  t = t_vec, 
  y = f_out(t), 
  easing_fnc = "bounce-out"
)

# Bounce-inout
bounce_inout_df <- tibble::tibble(
  t = t_vec, 
  y = dplyr::if_else(
    condition = t < 0.5, 
    true = (1 - f_out(1 - 2 * t)) * 0.5, 
    false = (1 + f_out(2 * t - 1)) * 0.5
  ), 
  easing_fnc = "bounce-in-out"
)


### ・イージング関数の可視化 -----

# イージング関数を指定
point_df <- elastic_inout_df

# イージング曲線用のデータフレームを作成
line_df <- point_df %>% 
  dplyr::rename(x = t) # フレーム用の列をx軸用の列に変更

# イージング関数を作図
anim <- ggplot(point_df, aes(x = t, y = y)) + 
  geom_line(data = line_df, mapping = aes(x = x, y = y), 
            color = "hotpink", size = 1) + # イージング曲線
  geom_vline(mapping = aes(xintercept = t), 
             color = "orange", size = 1, linetype = "dashed") + # 経過時間の線
  geom_hline(mapping = aes(yintercept = y), 
             color = "red", size = 1, linetype = "dashed") + # 変化量の線
  geom_point(mapping = aes(y = 0), color = "orange", size = 5) + # 経過時間の点
  geom_point(mapping = aes(x = 1), color = "red", size = 5) + # 変化量の点
  geom_point(color = "hotpink", size = 5) + # イージング曲線上の点
  gganimate::transition_reveal(along = t) + # フレーム
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Easing Function", 
       subtitle = "t = {round(frame_along, 2)}", 
       color = "easing function", 
       x = "time", y = expression(f(t)))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 101, fps = 50)


# 比較対象の設定 -----------------------------------------------------------------

# Quadratic関数を結合
easing_df <- rbind(
  linear_df, 
  quad_in_df, 
  quad_out_df, 
  quad_inout_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(easing_fnc, level = c("linear", "quad-in", "quad-out", "quad-in-out"))
  ) # 因子型に変換

# Cubic関数を結合
easing_df <- rbind(
  linear_df, 
  cubic_in_df, 
  cubic_out_df, 
  cubic_inout_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(easing_fnc, level = c("linear", "cubic-in", "cubic-out", "cubic-in-out"))
  ) # 因子型に変換

# Quartic関数を結合
easing_df <- rbind(
  linear_df, 
  quart_in_df, 
  quart_out_df, 
  quart_inout_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(easing_fnc, level = c("linear", "quart-in", "quart-out", "quart-in-out"))
  ) # 因子型に変換

# Quintic関数を結合
easing_df <- rbind(
  linear_df, 
  quint_in_df, 
  quint_out_df, 
  quint_inout_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(easing_fnc, level = c("linear", "quint-in", "quint-out", "quint-in-out"))
  ) # 因子型に変換

# Exponential関数を結合
easing_df <- rbind(
  linear_df, 
  expo_in_df, 
  expo_out_df, 
  expo_inout_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(easing_fnc, level = c("linear", "expo-in", "expo-out", "expo-in-out"))
  ) # 因子型に変換

# Sine関数を結合
easing_df <- rbind(
  linear_df, 
  sine_in_df, 
  sine_out_df, 
  sine_inout_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(easing_fnc, level = c("linear", "sine-in", "sine-out", "sine-in-out"))
  ) # 因子型に変換

# Circular関数を結合
easing_df <- rbind(
  linear_df, 
  circ_in_df, 
  circ_out_df, 
  circ_inout_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(easing_fnc, level = c("linear", "circ-in", "circ-out", "circ-in-out"))
  ) # 因子型に変換

# Back関数を結合
easing_df <- rbind(
  linear_df, 
  back_in_df, 
  back_out_df, 
  back_inout_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(easing_fnc, level = c("linear", "back-in", "back-out", "back-in-out"))
  ) # 因子型に変換

# Elastic関数を結合
easing_df <- rbind(
  linear_df, 
  elastic_in_df, 
  elastic_out_df, 
  elastic_inout_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(easing_fnc, level = c("linear", "elastic-in", "elastic-out", "elastic-in-out"))
  ) # 因子型に変換

# Bounce関数を結合
easing_df <- rbind(
  linear_df, 
  bounce_in_df, 
  bounce_out_df, 
  bounce_inout_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(easing_fnc, level = c("linear", "bounce-in", "bounce-out", "bounce-in-out"))
  ) # 因子型に変換


# Inタイプの関数を結合
easing_df <- rbind(
  linear_df, 
  quad_in_df, 
  cubic_in_df, 
  quart_in_df, 
  quint_in_df, 
  expo_in_df, 
  sine_in_df, 
  circ_in_df, 
  back_in_df, 
  elastic_in_df, 
  bounce_in_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(
      easing_fnc, 
      level = c(
        "linear", paste0(c("quad", "cubic", "quart", "quint", "expo", "sine", "circ", "back", "elastic", "bounce"), "-in"))
    )
  ) # 因子型に変換

# Outタイプの関数を結合
easing_df <- rbind(
  linear_df, 
  quad_out_df, 
  cubic_out_df, 
  quart_out_df, 
  quoutt_out_df, 
  expo_out_df, 
  sine_out_df, 
  circ_out_df, 
  back_out_df, 
  elastic_out_df, 
  bounce_out_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(
      easing_fnc, 
      level = c(
        "linear", paste0(c("quad", "cubic", "quart", "quint", "expo", "sine", "circ", "back", "elastic", "bounce"), "-out"))
    )
  ) # 因子型に変換

# InOutタイプの関数を結合
easing_df <- rbind(
  linear_df, 
  quad_inout_df, 
  cubic_inout_df, 
  quart_inout_df, 
  quint_inout_df, 
  expo_inout_df, 
  sine_inout_df, 
  circ_inout_df, 
  back_inout_df, 
  elastic_inout_df, 
  bounce_inout_df
) %>% 
  dplyr::mutate(
    easing_fnc = factor(
      easing_fnc, 
      level = c(
        "linear", paste0(c("quad", "cubic", "quart", "quint", "expo", "sine", "circ", "back", "elastic", "bounce"), "-in-out"))
    )
  ) # 因子型に変換

unique(easing_df[["easing_fnc"]])


# バーの伸縮による可視化と比較 ----------------------------------------------------------------

### ・作図用のデータフレームの作成 -----

# イージング曲線用のデータフレームを作成
line_df <- easing_df %>% 
  dplyr::rename(x = t) # フレーム用の列をx軸用の列に変更

# イージング関数の数を取得
fnc_size <- length(unique(easing_df[["easing_fnc"]]))

# イージングバー用のデータフレームを作成
bar_df <- easing_df %>% 
  dplyr::mutate(x = (rep(1:fnc_size, each = length(t_vec)) * 2 - 1) / (fnc_size * 2)) # x軸の値を等間隔に設定


### ・イージング処理の可視化 -----

# イージング関数を作図
anim <- ggplot(easing_df, aes(x = t, y = y, color = easing_fnc)) + 
  geom_vline(mapping = aes(xintercept = t), 
             color = "pink", size = 1, linetype = "dashed") + # 経過時間の線
  geom_hline(mapping = aes(yintercept = y, color = easing_fnc), linetype = "dashed") + 
  geom_line(data = line_df, mapping = aes(x = x, y = y, color = easing_fnc), size = 1) + # イージング曲線
  geom_point(mapping = aes(x = 1), size = 5) + 
  geom_point(mapping = aes(y = 0), color = "pink", size = 5) + 
  geom_point(size = 5) + # イージング曲線上の点
  gganimate::transition_reveal(along = t) + # フレーム
  scale_color_manual(values = c("red", "limegreen", "orange", "mediumblue")) + # 点と線の色:(不必要)
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Easing Functions", 
       subtitle = "t = {round(frame_along, 2)}", 
       color = "easing function", 
       x = "time", y = expression(f(t))) # ラベル

# gif画像を作成
g <- gganimate::animate(plot = anim, nframes = length(t_vec), fps = 50, width = 600, height = 600)

# gif画像を保存
gganimate::anim_save(filename = "output/Easing/_line.gif", animation = g)


# 動画を作成
m <- gganimate::animate(plot = anim, nframes = length(t_vec), fps = 30, width = 600, height = 600, renderer = gganimate::av_renderer())

# 動画を保存
gganimate::anim_save(filename = "output/Easing/_line.mp4", animation = m)


# イージング処理を作図
anim <- ggplot() + 
  geom_tile(data = bar_df, mapping = aes(x = x, y = y/2, height = y, fill = easing_fnc), 
           alpha = 0.7, width = 0.9/fnc_size) + # イージングバー
  geom_text(data = bar_df, mapping = aes(x = x, y = 0, label = easing_fnc, color = easing_fnc), 
            vjust = 1) + # 関数名:(横向き)
  #geom_text(data = bar_df, mapping = aes(x = x, y = 0, label = easing_fnc, color = easing_fnc), 
  #          hjust = 1, angle = 90) + # 関数名:()
  geom_hline(data = easing_df, mapping = aes(yintercept = y, color = easing_fnc), 
             linetype = "dashed") + # 変化量の線
  geom_line(data = line_df, mapping = aes(x = x, y = y, color = easing_fnc)) + # イージング曲線
  geom_point(data = easing_df, mapping = aes(x = t, y = y, color = easing_fnc), 
             alpha = 0.5, size = 5) + # イージング曲線上の点
  gganimate::transition_reveal(along = t) + # フレーム
  scale_color_manual(values = c("red", "limegreen", "orange", "mediumblue")) + # 点と線の色:(不必要)
  scale_fill_manual(values = c("red", "limegreen", "orange", "mediumblue")) + # バーの色:(不必要)
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Easing Functions", 
       subtitle = "t = {round(frame_along, 2)}", 
       fill = "easing function", color = "easing function", 
       x = "time", y = expression(f(x))) # ラベル

# gif画像を作成
g <- gganimate::animate(plot = anim, nframes = length(t_vec), fps = 50, width = 600, height = 600)

# gif画像を保存
gganimate::anim_save(filename = "output/Easing/_bar.gif", animation = g)


# 動画を作成
m <- gganimate::animate(plot = anim, nframes = length(t_vec), fps = 30, width = 600, height = 600, renderer = gganimate::av_renderer())

# 動画を保存
gganimate::anim_save(filename = "output/Easing/_bar.mp4", animation = m)


# 点の移動による可視化と比較 ---------------------------------------------------------

### ・作図用のデータフレームの作成 -----

# イージング関数の数を取得
fnc_size <- length(unique(easing_df[["easing_fnc"]]))

# 移動点用のデータフレームを作成
point_df <- easing_df %>% 
  dplyr::mutate(
    x = rep(1:fnc_size, each = length(t_vec)), 
    frame = t
  ) %>% # プロット位置とフレーム列を追加
  dplyr::select(t, x, y, easing_fnc, frame) # 列を並べ替え

# 点の軌跡用のデータフレームを作成
trace_point_df <- easing_df %>% 
  tibble::add_column(
    x = rep(1:fnc_size, each = length(t_vec)), 
    n = rep((length(t_vec)-1):0, times = fnc_size)
  ) %>% # 複製する数を追加
  tidyr::uncount(n) %>% # 過去フレーム用に複製
  dplyr::group_by(t, easing_fnc) %>% # 時間と関数でグループ化
  dplyr::mutate(
    idx = length(t_vec) + 1 - dplyr::row_number(), 
    frame = t_vec[idx]
  ) %>% # フレーム列を追加
  dplyr::ungroup() %>% # グループ化を解除
  dplyr::arrange(frame, x) %>% # 昇順に並び替え
  dplyr::select(t, x, y, easing_fnc, frame) # 利用する列を選択


### ・イージング処理の可視化 -----

# イージングされた点移動を作図
anim <- ggplot(point_df, aes(x = x, y = y, color = easing_fnc)) + 
  geom_hline(mapping = aes(yintercept = t), 
             color = "pink", size = 1, linetype = "dashed") + # 経過時間の線
  geom_point(size = 5, show.legend = FALSE) + # イージング移動点
  geom_point(data = trace_point_df, mapping = aes(x = x, y = y, color = easing_fnc), 
             size = 5, alpha = 0.2, show.legend = FALSE) + # 点の軌跡
  gganimate::transition_manual(frame = frame) + # フレーム
  scale_x_reverse(breaks = unique(point_df[["x"]]), labels = unique(point_df[["easing_fnc"]]), minor_breaks = FALSE) + # x軸目盛の反転
  scale_color_manual(values = c("red", "limegreen", "orange", "mediumblue")) + # 点と線の色:(不必要)
  scale_fill_manual(values = c("red", "limegreen", "orange", "mediumblue")) + # 点と線の色:(不必要)
  coord_flip() + # 軸の入れ替え
  labs(title = "Easing Functions", 
       subtitle = "t = {current_frame}", 
       x = "Easing Function", y = expression(f(t)))

# gif画像を作成
g <- gganimate::animate(plot = anim, nframes = length(t_vec)+10, fps = 10, end_pause = 10, width = 600, height = 600)

# gif画像を保存
gganimate::anim_save(filename = "output/Easing/_point.gif", animation = g)


# 動画を作成
m <- gganimate::animate(plot = anim, nframes = length(t_vec)+20, fps = 10, 
                        start_pause = 10, end_pause = 10, 
                        width = 600, height = 600, renderer = gganimate::av_renderer())

# 動画を保存
gganimate::anim_save(filename = "output/Easing/_point.mp4", animation = m)


# サイズの変化による可視化と比較 ---------------------------------------------------------------------

# イージング関数の数を取得
fnc_size <- length(unique(easing_df[["easing_fnc"]]))

# 散布図用のデータフレームを作成
point_df <- easing_df %>% 
  dplyr::mutate(
    x = rep(1:fnc_size, each = length(t_vec))
  ) %>% 
  dplyr::select(t, x, y, easing_fnc) # 列を並び替え

# イージングされた拡大する点を作図
anim <- ggplot() + 
  geom_point(data = point_df, mapping = aes(x = x, y = 0, size = y, color = easing_fnc)) + # 散布図
  gganimate::transition_reveal(along = t) + # フレーム
  scale_size_continuous(range = c(0, 20)) + # 点のサイズ
  scale_x_continuous(breaks = unique(easing_df2[["x"]]), labels = unique(easing_df2[["easing_fnc"]]), 
                     minor_breaks = FALSE, limits = c(0, fnc_size+1)) + # x軸目盛
  scale_color_manual(values = c("red", "limegreen", "orange", "mediumblue")) + # 点と線の色:(不必要)
  labs(title = "Easing Functions", 
       subtitle = "t = {frame_along}", 
       color = "easing function", 
       size = expression(f(t))) # ラベル

# gif画像を作成
gganimate::animate(plot = anim, nframes = length(t_vec), fps = 10)


