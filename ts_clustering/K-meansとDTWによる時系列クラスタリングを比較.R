# ***********************************************************************************************
# Title     : 単純なK-meansと{TSclust}のDTWによる時系列クラスタリングとではどう違うのか実験してみた（2019-1-18）
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/05
# URL       : https://tjo.hatenablog.com/entry/2019/01/18/190000
# ***********************************************************************************************


# ＜概要＞
# - 時系列クラスタリングを膨大な行数のデータに対して実行する際にどれほど厳密にやるべきか？
#   --- 動的時間伸縮法(DTW)で距離行列を算出してhclust関数でクラスタリングするのが王道
#   --- いずれも大規模データでは動作が遅い
#   --- k-means(動作が速い)でも似た結果が出るか確認


# ＜目次＞
# 0 準備
# 1 時系列データ生成
# 2 k-meansで分類
# 3 TSclustで分類


# 0 準備 ---------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(TSclust)


# 1 時系列データ生成 -----------------------------------------------------------

# ＜ポイント＞
# - 時系列データセットを用意して1つのデータフレームに収める
#   --- 上昇トレンド
#   --- 平均回帰
#   --- 下降トレンド
#   --- 季節調整あり時系列


# パラメータ定義
# --- 各グループの系列数
# --- 系列の長さ
# --- トレンドが上昇/ 下降する時の平均値
N <- 20
SPAN <- 24
TREND <- 0.5

# 関数定義
# --- 時系列データの生成器
generate_ts <- function(m, label) {

    # ランダムな AR 成分を追加
    .add.ar <- function(x) {
        x + arima.sim(n = SPAN, list(ar = runif(2, -0.5, 0.5)))
    }

    # 平均が偏った 乱数を cumsum してトレンドとする
    d <-
      rnorm(SPAN * N, mean = m, sd = 1) %>%
        matrix(ncol = N) %>%
        data.frame() %>%
        cumsum() %>%
        apply( 2, .add.ar) %>%
        set_colnames(paste0(label, seq(1, N)))

    return(d)
}

# 系列群-1
# --- 上昇トレンド
set.seed(101)
group1 <- generate_ts(TREND, label = 'U')

# 系列群-2
# --- 平均回帰
set.seed(102)
group2 <- generate_ts(0, label = 'N')

# 系列群-3
# --- 下落トレンド
set.seed(103)
group3 <- generate_ts(-TREND, label = 'D')

# 系列群-4
# --- 季節調整あり時系列（短いsin波）
set.seed(104)
group4 <- generate_ts(0, label = 'S1_')
group4 <- group4 + 5 * sin(seq(0, 4 * pi, length.out = SPAN))

# 系列群-5
# --- 季節調整あり時系列（長いsin波）
set.seed(105)
group5 <- generate_ts(0, label = 'S2_')
group5 <- group5 + 5 * sin(seq(0, 8 * pi, length.out = SPAN))

# データ結合
data <-
  group1 %>%
    bind_cols(group2) %>%
    bind_cols(group3) %>%
    bind_cols(group4) %>%
    bind_cols(group5) %>%
    as_tibble()

# クラスターラベル
# --- 正解
true.cluster <-
  c(rep(1, 20),
    rep(2, 20),
    rep(3, 20),
    rep(4, 20),
    rep(5, 20))

# プロット
# --- 正解クラスタで色付け
data %>% matplot(type = 'l', lty = 1, col = true.cluster)


# 2 k-meansで分類 -----------------------------------------------------------------

# ＜ポイント＞
# -

# クラスタリング
model.km <-
  data %>%
    t() %>%
    kmeans(centers = 5)

# クラスタ表示
model.km$cluster

# クラスタカウント
true.cluster %>% table()

# 正解とのマトリックス比較
true.cluster %>% table(model.km$cluster)

# プロット作成
# --- k-meansで推定したクラスタで色付け
data %>% matplot(type = 'l', lty = 1, col = model.km$cluster)


# 3 TSclustで分類 -----------------------------------------------------------------

# ＜ポイント＞
# - 王道のやり方

# DTW 距離で距離行列を作成
# --- 動的タイムワープ距離 +
# --- 最遠隣法
h <- data %>% diss("DTWARP") %>% hclust()

# プロット作成
# ---
par(cex=0.6)
h %>% plot(hang = -1)

# プロット作成
# ---
h.cluster <- h %>% cutree( 5)
h.cluster
