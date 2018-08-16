library(stringr)        #文字列検索用
library(data.table)     #csvからの高速呼び込み用
library(ggplot2)        #グラフの表示用

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# メイン関数
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

######################################################
# read_csvfile(path)
# csvファイルからデータを読み込むルーチン
######################################################
read_csvfile <- function(path)
{
    # ファイルからの読み込み方の決定
    dfs <- lapply(path, fread, integer64="numeric", data.table=FALSE,
                  stringsAsFactors=TRUE, na.strings = c("","NULL"))
    df <- do.call(rbind,dfs)

    #元データの1列目、「番号」列は省いておく
    df <- df[,-1]
    
    # データの列名を決定
    names(df) <- c("種類","地域","code","県名","市名","地区名","駅名","駅分","取引総額","坪単価","間取り","地積",
              "土地単価","土地形状","間口","延床面積","建築年","建物構造","建物用途","今後","道路方位","道路種類",
              "道路幅員","都市計画","建蔽率","容積率","取引時点","備考","その他")
    
    return(df)
}

########################################################
# get_LBdata(path, type)
# 土地及び建物(Land & Building)データを取得するルーチン
# typeは"住宅地"or"商業地"
#
# 備考
# 基本的なデータ削除はしておく
# 土地建物として分析する際に
# 必要となるデータ列を作成して追加
#
# TODO
# 築後年がマイナスになるデータの処理
########################################################
get_LBdata <- function(path, type = "住宅地", timecount = FALSE)
{
    t_start <-proc.time()

    ##########################################
    # data読み込み
    ##########################################
    df <- read_csvfile(path)
    
    #　土地建物データにフィルタリング
    df <- subset(df, df$種類 == "宅地(土地と建物)")
    
    # タイプの選別
    if(type == "住宅地"){
        df <- subset(df, df$地域 == "住宅地")
    }else if(type == "商業地"){
        df <- subset(df, df$地域 == "商業地")
    }
        
    ##########################################
    # NAデータ等削除
    # 列名は要確認
    ##########################################
    df <- subset(df, !is.na(df$建築年))
    df <- subset(df, df$建築年 != "戦前")
    df <- subset(df, !is.na(df$駅分))
    df <- subset(df, !is.na(df$地積))
    df <- subset(df, !is.na(df$延床面積))

    df <- subset(df, !is.na(df$道路幅員))
    df <- subset(df, !is.na(df$建蔽率))
    df <- subset(df, !is.na(df$容積率))
    df <- subset(df, !is.na(df$都市計画))
    df <- subset(df, !is.na(df$道路種類))
    df <- subset(df, !is.na(df$地区名))
    
    df <- subset(df, !grepl("以上",as.character(df$地積)))
    df <- subset(df, !grepl("以上",as.character(df$延床面積)))
    df <- subset(df, !grepl("未満",as.character(df$延床面積)))

    ##########################################
    # 列処理
    ##########################################
    df <- df %>% add_date_cols %>% add_station_data_cols
    
    ##########################################
    # 数値化
    ##########################################
    df$地積 <- as.numeric(as.character(df$地積))
    df$延床面積 <- as.numeric(as.character(df$延床面積))

    df <- subset(df, !is.na(df$地積))
    df <- subset(df, !is.na(df$延床面積))

    
    # 「築年」、「築後年」列の追加
    build_year <- conv_JC_to_AD(df$建築年)
    築後年 <- df$tori_year - build_year
    df <- cbind(df, build_year, 築後年)
    
    # 異常値（マイナス）を除外
    # TODO: マイナス値を０に含めてしまう処理の方がよいか？
    df <- subset(df, df$築後年 >= 0) %>% droplevels
    

    t_ans <- proc.time() - t_start
    if(timecount == TRUE){
        print(t_ans)
    }
    
    return(df)
}

################################################
# set_start_qtr(df, qtr_string)
# 指定した期間以降のデータに絞り込むための関数
###############################################
set_start_qtr <- function(df, qtr_string){
    ans <- subset(df, as.character(df$qtr) >= qtr_string) %>% droplevels
    return(ans)
}

####################################
# make_transaction_index(df, term)
# 取引指数を作る関数
####################################
make_transaction_index <- function(df, term, timecount = FALSE){

    t_start <-proc.time()
    
    # この名前もチェックの必要あり？
    date_name <- names(table(df$qtr))
    
    # 分析結果
    data_list <- df %>% analyse_by_rolling_window(term)

    ###################################
    # 取引時点係数を取り出す
    ###################################
    coef_list <- list()
    
    if(term > 2){
        for(i in seq_along(data_list)){
            coef_list[[i]] <- data_list[[i]]$coef[2:(2+term-1-1)]
        }
    }else{
        last_index <- qtr_seq_check(data_list[[1]])
        coef_list[[1]] <- data_list[[1]]$coef[2:last_index]
    }
    
    ans <- c(1, exp(coef_list[[1]]) )
    
    for (elm in coef_list[-1]){
        tmp <- ans[length(ans)] * exp(elm[length(elm)]) / exp(elm[length(elm) - 1])
        ans <- c(ans, tmp)
    }
    names(ans) <- date_name
    
    t_ans <- proc.time() - t_start
    
    if(timecount == TRUE){
        print(t_ans)
    }
    
    return (ans)
}

################################################################
# analyse_by_rolling_window(df, term)
# rollingWindow法による重回帰分析
#
# 備考
# termを１以下にすると全体で重回帰分析
# 結果はlmの結果を期間ごとのリストとして返す
################################################################
analyse_by_rolling_window <- function(df, term){
    
    # 期間設定が３以上の時は、ローリングウインドウ法
    # それ以外の場合、全体期間の重回帰分析を行う
    if( term > 2){
        df_list <- devide_data_by_term(df, term)
    }else{
        df_list <- list()
        df_list[[1]] <- df
    }

    ########################
    # モデルをここで定義
    ########################
    .my_lm <- function(df){
        mod <- paste("log(取引総額/10000) ~ qtr +",
                     ifelse(length(unique(df$駅名))==1, "", "駅名 + "),
                     ifelse(length(unique(df$TSD))==1, "", "TSD +"),
                     ifelse(length(unique(df$地区名))==1, "","地区名 + "),
                     "地積 + 延床面積 + 築後年 + 道路幅員", sep = "")

        return(lm(mod, data=df))
    }   
    
    ########################
    # lmの実処理
    ########################
    ans_list <- list()
    for(i in seq_along(df_list)){
        ans_list[[i]] <- .my_lm(df_list[[i]])
    }

    return(ans_list)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 可視化のメイン関数
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#############################################################################
# LBdata_graph(df)
# 分析用まとめグラフの表示
#############################################################################
LBdata_graph <- function(df)
{
    backup_mfrow <- par()$mfrow
    par(mfrow = c(3,2))
    
    hist(df$取引総額, xlab="取引価格の総額")
    hist(df$地積, xlab="面積(地積)")
    hist(df$延床面積, xlab="延床面積")
    hist(df$築後年, xlab="築後年")
    
    plot(df$地積, df$取引総額, xlab="面積(地積)", ylab="取引価格の総額")
    plot(df$地積, df$取引総額/df$地積, xlab="地積", ylab="取引総額／地積")

    par(mfrow = backup_mfrow)
}

#############################################################################
# LBdata_summary(df)
# サマリーの表示
#############################################################################
LBdata_summary <- function(df)
{
    s_data <- data.frame(P  = df$取引総額/10000, #円→万円に変換
                         L  = df$地積,
                         PPL= (df$取引総額/10000) / df$地積,
                         S  = df$延床面積,
                         A  = df$築後年)
    
    s <- cbind(apply(s_data,2,mean),
               apply(s_data,2,sd),
               apply(s_data,2,min),
               apply(s_data,2,max))

    # 列名に単位等の情報を入れるかどうかは検討すべき
    # 表示が目的ならそれでよいかも
    rownames(s) <- c("P: 取引価格の総額(万円)",
                     "L: 面積(㎡)",
                     "P/L: ㎡単価(万円/㎡)",
                     "S: 延床面積(㎡)",
                     "Age: 建築後年数(年)")

    colnames(s) <- c("平均","標準偏差","最小値","最大値")

    return(s)
}

#############################################################################
# draw_transaction_index_graph(index_vector)
# 取引指数をグラフで表示
#############################################################################
draw_transaction_index_graph <- function(index_data){
    # 取引期の文字列を数値に変換
    qtr_strings <- names(index_data)
    tmp <- str_match(qtr_strings, "(\\d{4})Q(.)")
    qtr_num <- as.numeric(tmp[,2])  + (as.numeric(tmp[,3]) - 1) * 0.25 + 0.125
    
    # データフレームの作成
    df <- data.frame(transaction_term = qtr_num, index = index_data)
    
    #print(df)
   
    ggplot(data=df, aes(x=transaction_term, y=index) ) + 
        geom_line(linetype=1,size=0.5) +
        geom_point(size=1,alpha=0.5) +
        geom_hline(yintercept = 1,size=1)
    
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 内部ルーチン
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#############################################################
# add_data_cols(df)
# 取引期:qtr、取引年:tori_year、取引時点数値:jiten
# 以上３列のデータを追加する実装
#############################################################
add_date_cols <- function(df)
{
    ##################################
    # 「取引年」tori_year列の作成
    date_string <- as.character(df$取引時点)
    tori_year <- conv_JC_to_AD(date_string)
    
    ##################################
    # 「取引期」qtr列の作成
    q_str <- date_string
    q_str[grep("第１四半期",q_str)] <- "1"
    q_str[grep("第２四半期",q_str)] <- "2"
    q_str[grep("第３四半期",q_str)] <- "3"
    q_str[grep("第４四半期",q_str)] <- "4"

    qtr <- paste0(tori_year, "Q", q_str)

    ##################################
    # 「取引時点数値」jiten列の作成
    #  取引期の初期の
    jiten <- as.integer(tori_year) + (as.numeric(q_str) - 1) * 0.25
    
    ##################################
    # データに合成
    df <- cbind(df, tori_year, qtr, jiten)

    return(df)
}

################################################
# add_station_data_cols(df)
# 最寄り駅データ処理、TSD列とeki_fun列の追加
#
# 備考
# オリジナルデータ内で最寄駅分がNAのものもあるが、
# その行を削除せず、そのままNAで返す
###############################################
add_station_data_cols <- function(df)
{

    # 3バイト以上を文字列として認識して、区分
    # eki_hun列で何分かを把握、３０分以上はNA
    moyori <- as.character(df$駅分)
    eki_hun <- ifelse(nchar(moyori) > 2, NA, moyori) %>% as.integer

    # TSDは時間距離を区分
    TSD <- moyori

    TSD <- ifelse(TSD == "30分?60分", "4.30分-60分未満",       TSD)
    TSD <- ifelse(TSD == "1H?1H30",   "5.1時間-1時間30分未満", TSD)
    TSD <- ifelse(TSD == "1H30?2H",   "6.1時間30分-2時間未満", TSD)
    TSD <- ifelse(TSD == "2H?",       "7.2時間-",          TSD)
  
    TSD <- ifelse(0<=eki_hun & eki_hun<10,     "1.10分未満",            TSD)
    TSD <- ifelse(10<=eki_hun & eki_hun<20,    "2.10分-20分未満",       TSD)
    TSD <- ifelse(20<=eki_hun & eki_hun<30,    "3.20分-30分未満",       TSD)

    #元のデータフレームに追加する形で戻す
    df <- cbind(df, TSD, eki_hun)
    return(df)
}

#####################################################
# devide_data_by_term(df, term)
# 既存データフレームを期間毎に再配分
# ローリングの１期間をtermで渡す
# 
# TODO
# 各タームの連続性をここではチェックしていない
# どこでチェックを入れるべきかを統一しておくべき
#####################################################
devide_data_by_term <- function(df, term){
    ans_list <- list()

    # TODO
    # qtr_vecが各時点のインデックスになるが、
    # 連続性は担保されているのか不明
    # names(table())を使うと、名前順に並ぶのか不明
    # 独自処理に書き換える方が安全かも
    qtr_vec <- names(table(df$qtr))
    
    # 期間ごとに期間内のデータを探す
    for (i in 1:(length(qtr_vec) - (term - 1))){
        ans_list[[i]] <- subset(df,
                                qtr_vec[i] <= as.character( df$qtr ) &
                                as.character( df$qtr ) <= qtr_vec[i + (term - 1)])
    }
    
    # 期首の時点が期間の名前（必要ないかも）
    names(ans_list) <- qtr_vec[1:(length(qtr_vec) - (term - 1))]
    
    return(ans_list)
}

####################################################
# qtr_seq_check(lm_result)
# coefからqtrのチェック
# 
# 備考
# lm分析の結果オブジェクト内のcoefficientsリストで
# 当該モデルでは、切片の次にある
# 「qtr*」が時点についての係数を表すので、
# その範囲を得るためのルーチン
#　併せて、連続性もチェックしており、
# 連続していない場合は、qtr*が続いていても
# 途切れた部分のindexを返す
####################################################
qtr_seq_check <- function(lm_res){
    .names <- names(lm_res$coef)
    ans <- 2

    #qtrの最初の文字は２列目
    next_word <- make_next(.names[2])
    
    #もし２列目にqtr文字がない場合NA
    if(is.na(next_word)){
        return(NA)
    }
    
    #最初の２列を外す
    .names <- .names[c(-1,-2)]

    for (i in seq_along(.names)){
        if(.names[i] == next_word){
            next_word <- make_next(.names[i])
            ans <- ans + 1
        }else{
            return(ans)
        }
    }
}

#########################################################
# make_next(qtr_string)
# 与えられたqtr文字列の時期にあたる文字列を返す
# 
# 備考
# coefのqtr要素の連続性をチェックするためのユティリティ
#
#########################################################
make_next <- function(s){
    tmp <- str_match(s,"qtr(\\d{4})Q(.)")

    if (is.na(tmp[1])){
        return(NA)
    }

    if (as.integer(tmp[3]) >= 4){
        y <- as.integer(tmp[2]) + 1
        ans <- paste0("qtr",y,"Q1")
    }else{
        ans <- paste0("qtr",tmp[2],"Q",as.integer(tmp[3]) + 1)
    }
    
    return(ans)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 内部汎用ユティリティ
# データフレーム構造に依存しないもの
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#############################################################
# 和暦（Japanese Clendar）西暦(AD)変換のルーチン
# データ内に和暦がでてくるところで使いやすいように作っておく
# jc_To_ad 和暦-＞西暦を変換する関数を作る関数
# 近々、元号が変わるのでそれに対処できるように
#############################################################

# 元号変換する関数を作成する汎用関数
jc_To_ad <- function(gengou, first_year)
{
        # 後方参照用かっこ付きのマッチングパターンを準備
    # "昭和(.+)年"みたいな感じのマッチングパターンになる
    match_str <- paste0(gengou,"(.+)年")

    ans_fnc <- function(input_str)
    {
        input_str <- as.character(input_str)
        str <- sub("元年","1年",input_str)
        tmp <- str_match(str,match_str)
        
        #str_matchの戻り値は１列目がマッチ全体、２列目以降が後方参照
        ans <- ifelse(is.na(tmp[,1]), input_str, as.integer(tmp[,2]) + first_year - 1)
    
        return(ans)
    }
    return(ans_fnc)
}

# 和暦から西暦に変換する一般関数
# 新しい元号を加える場合、ここで調整すればよい
conv_JC_to_AD <- function(str)
{
    # 個別の変換関数を作成
    syouwa_to_ad <- jc_To_ad("昭和",1926)
    heisei_to_ad <- jc_To_ad("平成",1989)
    
    return(str %>% syouwa_to_ad %>% heisei_to_ad %>% as.integer)
}

