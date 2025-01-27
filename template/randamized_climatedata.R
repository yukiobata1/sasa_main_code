# 必要なライブラリをロード
library(dplyr)

# CSVファイルを読み込む
data <- read.csv("sapporo_historical_1985_1994_Janstar.csv", header=FALSE)

d <- split(data, rep(1:365, each=365))

# データの行数とブロックサイズを定義
total_rows <- nrow(data)
block_size <- 365
num_blocks <- total_rows %/% block_size  # 整数除算でブロック数を計算
num_samples_per_block <- 365  # 各ブロックからサンプリングする行数

# サンプリング結果を格納するデータフレームを初期化
sampled_data <- data.frame()

# 各ブロックからランダムにサンプリング
set.seed(123) # 再現性のためにシードを設定
for (i in 1:num_blocks) {
  start_row <- (i - 1) * block_size + 1
  end_row <- i * block_size
  block <- data[start_row:end_row, ]
  
  # 各ブロックの行数を確認
  print(paste("Block", i, "has", nrow(block), "rows"))
  
  # 各ブロックからそのままの順序で100回サンプリング
  for (j in 1:num_samples_per_block) {
    sampled_data <- rbind(sampled_data, block)
  }
}

# 最終結果をCSVファイルに書き出す
write.csv(sampled_data, "sapporo_randomized_36500.csv", row.names = FALSE)

# サンプルの行数を確認
print(nrow(sampled_data))
