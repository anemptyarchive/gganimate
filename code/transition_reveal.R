
# transition_reveal -------------------------------------------------------

# 利用パッケージ
library(ggplot2)
library(gganimate)


# データフレームの作成 --------------------------------------------------------------

# データフレームを作成
df <- tibble::tibble(
  x = 0:5, 
  y = 0:5, 
  along = 0:5
)


# 折れ線グラフを作成
ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 5, color = "hotpink") + # 散布図
  geom_path(color = "hotpink") + # 折れ線
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "geom_point()")


# グラフとの関係 ---------------------------------------------------------------------

# 散布図を作成
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 棒グラフ
  gganimate::transition_reveal(along = along) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal() + geom_point()"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 50, fps = 10)


# 棒グラフを作成
anim <- ggplot(df, aes(x = 1, y = y)) + 
  geom_bar(stat = "identity", fill = "hotpink", color = "hotpink") + # 棒グラフ
  gganimate::transition_reveal(along = along) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal() + geom_bar()"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 50, fps = 10)


# 折れ線グラフを作成
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_line(size = 1, color = "hotpink") + # 棒グラフ
  gganimate::transition_reveal(along = along) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal() + geom_line()"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 50, fps = 10)


# 折れ線グラフを作成
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 棒グラフ
  geom_path(size = 1, color = "hotpink") + # 棒グラフ
  gganimate::transition_reveal(along = along) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal() + geom_point() + geom_path()"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 50, fps = 10)


# ラベル変数 ---------------------------------------------------------------------

# ラベル変数を指定
lv <- "frame_along" # 現フレームの値

# ラベル変数の設定
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  geom_path(color = "hotpink") + # 折れ線
  gganimate::transition_reveal(along = along) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal() + labs(", lv, ")"), 
       subtitle = paste0("frame : {", lv, "}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 50, fps = 10)


# 引数 ----------------------------------------------------------------------

### ・range -----

# 値を指定:(デフォルト:NULL)
r <- NULL
r <- c(2L, 3L)

# range(描画範囲)の設定
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  geom_path(color = "hotpink") + # 折れ線
  gganimate::transition_reveal(along = along, range = r) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal(range = c(", paste0(r, collapse = ", "), "))"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 10, fps = 10)


### ・keep_last -----

# 値を指定:(デフォルト:TRUE)
b <- TRUE
b <- FALSE

# keep_last(最後の描画)の設定
anim <- ggplot(head(df, 3), aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  geom_path(color = "hotpink") + # 折れ線
  gganimate::transition_reveal(along = along, keep_last = b) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal(keep_last = ", b, ")"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 3, fps = 1)


### ・along -----

# 等間隔でないフレーム列を作成
df <- tibble::tibble(
  x = c(1, 2, 10), 
  y = c(1, 2, 10), 
  along = c(1, 2, 10)
)

# 折れ線グラフを作成
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  geom_path(color = "hotpink") + # 折れ線
  gganimate::transition_reveal(along = along) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal()"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 10, fps = 1)


# 棒グラフを作成
anim <- ggplot(df, aes(x = 1, y = y)) + 
  geom_bar(stat = "identity", fill = "hotpink", color = "hotpink") + # 棒グラフ
  gganimate::transition_reveal(along = along) + # フレーム
  scale_x_continuous(breaks = 1, minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal()"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 101, fps = 10)


# 確認
seq(from = 1, to = 10, length.out = 100)


# 等間隔でないフレーム列を作成
df <- tibble::tibble(
  x = c(1, 2, 3, 4, 6, 9, 13), 
  y = c(2, 1, 0, 1, 3, 6, 10), 
  along = 1:7
)

# 折れ線グラフを作成
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  geom_path(color = "hotpink") + # 折れ線
  gganimate::transition_reveal(along = along) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal()"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 120, fps = 10)


# 不規則なフレーム列を作成
df <- tibble::tibble(
  x = 1:5, 
  y = 1:5, 
  along = c(1, 3, 2, 5, 4)
)

# 折れ線グラフを作成
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  geom_path(color = "hotpink") + # 折れ線
  #geom_text(aes(label = y), hjust = 0) + # データラベル
  gganimate::transition_reveal(along = along) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal()"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 40, fps = 10)


# 重複するフレーム列を作成
df <- tibble::tibble(
  x = 1:6, 
  y = 1:6, 
  along = c(1, 1, 2, 3, 4, 3)
)

# 折れ線グラフを作成
anim <- ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 10, color = "hotpink") + # 散布図
  #geom_path(color = "hotpink") + # 折れ線
  gganimate::transition_reveal(along = along) + # フレーム
  scale_x_continuous(breaks = df[["x"]], minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = df[["y"]], minor_breaks = FALSE) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = paste0("transition_reveal()"), 
       subtitle = paste0("frame : {frame_along}"))

# gif画像を作成
gganimate::animate(plot = anim, nframes = 50, fps = 10)



# 利用例：確率分布 ----------------------------------------------------------------------

# 利用パッケージ
library(ggplot2)
library(gganimate)


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


# ガウス分布を作図
ggplot(dens_df, aes(x = x, y = density, color = as.factor(sigma))) + 
  geom_line() + # 折れ線グラフ
  labs(title = "Gaussian Distribution", 
       color = "sigma")


# ガウス分布のアニメーションを作成:折れ線グラフ
anim <- ggplot(dens_df, aes(x = x, y = density, color = as.factor(sigma))) + 
  geom_line(show.legend = FALSE) + # 折れ線グラフ
  gganimate::transition_reveal(along = sigma) + # フレーム
  labs(title = "transition_reveal() + geom_line()", 
       subtitle = "sigma = {frame_along}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = length(sd_vec), fps = 10)


# ガウス分布のアニメーションを作成:散布図
anim <- ggplot(dens_df, aes(x = x, y = density, color = as.factor(sigma), group = x)) + 
  geom_point(show.legend = FALSE) + # 散布図
  gganimate::transition_reveal(along = sigma) + # フレーム
  labs(title = "transition_reveal() + geom_point()", 
       subtitle = "sigma = {frame_along}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = length(sd_vec), fps = 10)


# ガウス分布のアニメーションを作成:散布図
anim <- ggplot(dens_df, aes(x = x, y = density, color = as.factor(sigma), group = sigma)) + 
  geom_point(show.legend = FALSE) + # 散布図
  geom_path(show.legend = FALSE) + # 折れ線
  gganimate::transition_reveal(along = x) + # フレーム
  labs(title = "transition_reveal() + geom_point() + geom_path()", 
       subtitle = "x = {round(frame_along, 1)}")

# gif画像を作成
gganimate::animate(plot = anim, nframes = length(x_vals), fps = 10)


# 利用例：ランダムウォーク ------------------------------------------------------------

# RandomWalk.Rを参照


