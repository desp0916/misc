#
# 「輿情監控日報」關鍵字加權排序
#
# Usage:
#
#   依據「關鍵字清單」中的「關鍵字」與「權重」設定，以及關鍵字出現在 raw data
#   （下載自 OpView）每列「標題」與「內容」欄位的次數，先計算出中每一列的「加
#   權總計」(WeightSum)。然後再依據每一列的「加權總計」 欄位值，由高而低進行
#   排序，最後再產出排序後的 raw data。
#
# Features:
#
#   1. 可依據需求，自由調整「關鍵字」於「標題」或「內容」欄位的權重。針對想特別
#      關注的關鍵字，可增加其在「標題」或「內容」欄位的權重，以使排序提前。
#
#   2. 依據「加權總計」欄位 (WeightSum) 排序，可縮短人工雜訊過濾作業的時間。
#
# @package		PublicOpinionMonitor
# @author		Gary Liu
# @since		2015/01/03
# @copyright	Copyright (c) 2015, President Information Corp.
# @link			http://garyliutw.blogspot.tw
#
# $Id$
#

# 設定工作目錄
setwd("E:/workspace_r/PublicOpinionMonitor")

# 相關檔案
rawDataFile <- "RawData.xls"					# Input: raw data
keyWordwEightFile <- "KeywordWeight.xls"		# Input: 關鍵字清單
SortedRawDataFile <- "RawDataSorted.xls"		# Output: 依據「加權總計」排序的 raw data

# 載入套件
library(xlsx)
library(tm)
library(tmcn)
library(Rwordseg)

# 載入「關鍵字清單」，含「標題權重」(weight_titile)與「內容權重」(weight_content)
keyWords <- read.xlsx2(keyWordwEightFile, 1, stringsAsFactors=FALSE)
row.names(keyWords) <- keyWords$keyword  # 在每列開頭加上列名稱（關鍵字）
insertWords(keyWords$keyword)            # 將關鍵字清單加入 Rwordseg 的使用者自訂辭典

# 安裝或載入其他辭典
#
# 1. 建議先安裝「台北市城市信息精选」詞庫：http://pinyin.sogou.com/dict/cell.php?id=19444
#    安裝指令如下：
#
#     installDict("E://workspace_r//PublicOpinionMonitor//sogou_dict//台北市城市信息精选.scel", "taipei_city_info")
#     installDict("E://workspace_r//PublicOpinionMonitor//mydict.txt", "mydict")
#     installDict("E://workspace_r//PublicOpinionMonitor//keywords.txt", "7eleven")
#     listDict()
#
# 2. 刪除辭典指令如下：
#
#   uninstallDict()
#
# 3. Rwordseg 的官方網站：
#
#   http://jliblog.com/app/rwordseg
#

# 載入 raw data
rawData <- read.xlsx2(rawDataFile, 1, stringsAsFactors=FALSE)

# 依據：
#   (1) 「關鍵字清單」設定
#   (2) 某一關鍵字出現在「標題」或「內容」欄位的次數
# 計算出「加權分數」
titleOrContentWeight <- function(TitleOrContent, Occurs, i) {
  k <- names(Occurs)[i]  # 關鍵字
  f <- Occurs[[i]]       # 關鍵字出現次數
  #   print(paste("f: ", f, ", k: ", k));
  #   print(as.integer(keyWords[k, "weight_title"]))
  if (TitleOrContent == "title") {
    return (f * as.integer(keyWords[k, "weight_title"]))	# 關鍵字出現「標題」欄位的加權分數
  } else if (TitleOrContent == "content") {
    #     print(keyWords[k, "weight_content"])
    return (f * as.integer(keyWords[k, "weight_content"]))	# 關鍵字出現「內容」欄位的加權分數
  } else if (TitleOrContent == "author") {
    return (f * as.integer(keyWords[k, "weight_author"]))	# 關鍵字出現「author」欄位的加權分數
  } else if (TitleOrContent == "website") {
    return (f * as.integer(keyWords[k, "weight_website"]))	# 關鍵字出現「website」欄位的加權分數
  } else {
    write("ERROR!", stderr())
  }
}

# 計算關鍵字出現在某列「標題」欄位時的加權分數
rowTitleWeight <- function (row) {
  segment.options(isQuantifierRecognition = FALSE)
  seg <- segmentCN(rawData[, 2][row], nosymbol = FALSE)            # 斷詞
  ct <- table(seg[seg %in% keyWords$keyword])    # 列聯表(Contingency Table)，關鍵字次數表
  tsw <- 0
  if (is.table(ct) && length(ct) > 0) {
    # 逐個關鍵字計算
    for (i in 1:length(ct)) {
      #     print(paste("ct[i]:", ct[i]))
      tsw <- tsw + titleOrContentWeight("title", ct, i)
    }
  }
  #print(paste("title weight:", tsw))
  (ct)
  return(tsw)
}


# segmentCN(rawData[, 2][8])

# 計算關鍵字出現在某列「內容」欄位時的加權分數
rowContentWeight <- function (row) {
  segment.options(isQuantifierRecognition = FALSE)
  seg <- segmentCN(rawData[, 7][row], nosymbol = FALSE)           # 斷詞
  ct <- table(seg[seg %in% keyWords$keyword])   # 列聯表(Contingency Table)，關鍵字次數表
  # print(ct)
  # print(paste("names(ct)[1]: ", names(ct)[1], ", is.na(ct): " , is.na(ct)))
  # kc <- dimnames(ct)
  # print(paste(length(kc), "---"))
  tsw <- 0
  if (is.table(ct) && length(ct) > 0) {
    # 逐個關鍵字計算
    for (i in 1:length(ct)) {
      #     print(paste("ct[i]:", ct[i]))
      tsw <- tsw + titleOrContentWeight("content", ct, i)
    }
  }
  return(tsw)
}

# 計算關鍵字出現在某列「author」欄位時的加權分數
rowAuthorWeight <- function (row) {
  segment.options(isQuantifierRecognition = FALSE)
  seg <- segmentCN(rawData[, 6][row], nosymbol = FALSE)            # 斷詞
  ct <- table(seg[seg %in% keyWords$keyword])    # 列聯表(Contingency Table)，關鍵字次數表
  tsw <- 0
  #print(length(ct))
  if (is.table(ct) && length(ct) > 0) {
    # 逐個關鍵字計算
    for (i in 1:length(ct)) {
      #print(paste("ct[i]:", ct[i]))
      tsw <- tsw + titleOrContentWeight("author", ct, i)
    }
  }
  return(tsw)
}

# 計算關鍵字出現在某列「website」欄位時的加權分數
rowWebsiteWeight <- function (row) {
  segment.options(isQuantifierRecognition = FALSE)
  seg <- segmentCN(rawData[, 9][row], nosymbol = FALSE)            # 斷詞
  ct <- table(seg[seg %in% keyWords$keyword])    # 列聯表(Contingency Table)，關鍵字次數表
  tsw <- 0
#  print(length(ct))
  if (is.table(ct) && length(ct) > 0) {
    # 逐個關鍵字計算
    for (i in 1:length(ct)) {
      #print(paste("ct[i]:", ct[i]))
      tsw <- tsw + titleOrContentWeight("website", ct, i)
    }
  }
  return(tsw)
}

# 初始化一個「加權總計」的空向量：WeightSum
WeightSum <- c()

# 逐列進行「加權總計」欄位的計算
for (line in 1:nrow(rawData)) {
  # 過濾標題、作者、內容三個欄位內的特殊符號（Emoji）
  rawData[, 2][line] <- iconv(rawData[, 2][line], 'UTF-8', 'UTF-8', sub="")
  rawData[, 7][line] <- iconv(rawData[, 7][line], 'UTF-8', 'UTF-8', sub="")
  WeightSum[line] <- rowTitleWeight(line) + rowContentWeight(line) + rowAuthorWeight(line) + rowWebsiteWeight(line)
}

# 在 raw data 最後新增一個「WeightSum」的欄位。
# （將「加權總計」的向量附加到 raw data 最後）
rawData["WeightSum"] <- WeightSum
rawDataLength <- length(rawData)    # cache this value

#rawData[order(as.integer(rawData[,rawDataLength]), decreasing = TRUE), ]
#head(rawData[,rawDataLength])

# 依據「WeightSum」欄位由大至小排序，寫入 rankedDataFile
write.xlsx(rawData[order(as.integer(rawData[, rawDataLength]), decreasing = TRUE), ], SortedRawDataFile)

