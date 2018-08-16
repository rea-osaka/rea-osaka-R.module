library(stringr)        #�����񌟍��p
library(data.table)     #csv����̍����Ăэ��ݗp
library(ggplot2)        #�O���t�̕\���p

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ���C���֐�
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

######################################################
# read_csvfile(path)
# csv�t�@�C������f�[�^��ǂݍ��ރ��[�`��
######################################################
read_csvfile <- function(path)
{
    # �t�@�C������̓ǂݍ��ݕ��̌���
    dfs <- lapply(path, fread, integer64="numeric", data.table=FALSE,
                  stringsAsFactors=TRUE, na.strings = c("","NULL"))
    df <- do.call(rbind,dfs)

    #���f�[�^��1��ځA�u�ԍ��v��͏Ȃ��Ă���
    df <- df[,-1]
    
    # �f�[�^�̗񖼂�����
    names(df) <- c("���","�n��","code","����","�s��","�n�於","�w��","�w��","������z","�ؒP��","�Ԏ��","�n��",
              "�y�n�P��","�y�n�`��","�Ԍ�","�����ʐ�","���z�N","�����\��","�����p�r","����","���H����","���H���",
              "���H����","�s�s�v��","������","�e�ϗ�","������_","���l","���̑�")
    
    return(df)
}

########################################################
# get_LBdata(path, type)
# �y�n�y�ь���(Land & Building)�f�[�^���擾���郋�[�`��
# type��"�Z��n"or"���ƒn"
#
# ���l
# ��{�I�ȃf�[�^�폜�͂��Ă���
# �y�n�����Ƃ��ĕ��͂���ۂ�
# �K�v�ƂȂ�f�[�^����쐬���Ēǉ�
#
# TODO
# �z��N���}�C�i�X�ɂȂ�f�[�^�̏���
########################################################
get_LBdata <- function(path, type = "�Z��n", timecount = FALSE)
{
    t_start <-proc.time()

    ##########################################
    # data�ǂݍ���
    ##########################################
    df <- read_csvfile(path)
    
    #�@�y�n�����f�[�^�Ƀt�B���^�����O
    df <- subset(df, df$��� == "��n(�y�n�ƌ���)")
    
    # �^�C�v�̑I��
    if(type == "�Z��n"){
        df <- subset(df, df$�n�� == "�Z��n")
    }else if(type == "���ƒn"){
        df <- subset(df, df$�n�� == "���ƒn")
    }
        
    ##########################################
    # NA�f�[�^���폜
    # �񖼂͗v�m�F
    ##########################################
    df <- subset(df, !is.na(df$���z�N))
    df <- subset(df, df$���z�N != "��O")
    df <- subset(df, !is.na(df$�w��))
    df <- subset(df, !is.na(df$�n��))
    df <- subset(df, !is.na(df$�����ʐ�))

    df <- subset(df, !is.na(df$���H����))
    df <- subset(df, !is.na(df$������))
    df <- subset(df, !is.na(df$�e�ϗ�))
    df <- subset(df, !is.na(df$�s�s�v��))
    df <- subset(df, !is.na(df$���H���))
    df <- subset(df, !is.na(df$�n�於))
    
    df <- subset(df, !grepl("�ȏ�",as.character(df$�n��)))
    df <- subset(df, !grepl("�ȏ�",as.character(df$�����ʐ�)))
    df <- subset(df, !grepl("����",as.character(df$�����ʐ�)))

    ##########################################
    # �񏈗�
    ##########################################
    df <- df %>% add_date_cols %>% add_station_data_cols
    
    ##########################################
    # ���l��
    ##########################################
    df$�n�� <- as.numeric(as.character(df$�n��))
    df$�����ʐ� <- as.numeric(as.character(df$�����ʐ�))

    df <- subset(df, !is.na(df$�n��))
    df <- subset(df, !is.na(df$�����ʐ�))

    
    # �u�z�N�v�A�u�z��N�v��̒ǉ�
    build_year <- conv_JC_to_AD(df$���z�N)
    �z��N <- df$tori_year - build_year
    df <- cbind(df, build_year, �z��N)
    
    # �ُ�l�i�}�C�i�X�j�����O
    # TODO: �}�C�i�X�l���O�Ɋ܂߂Ă��܂������̕����悢���H
    df <- subset(df, df$�z��N >= 0) %>% droplevels
    

    t_ans <- proc.time() - t_start
    if(timecount == TRUE){
        print(t_ans)
    }
    
    return(df)
}

################################################
# set_start_qtr(df, qtr_string)
# �w�肵�����Ԉȍ~�̃f�[�^�ɍi�荞�ނ��߂̊֐�
###############################################
set_start_qtr <- function(df, qtr_string){
    ans <- subset(df, as.character(df$qtr) >= qtr_string) %>% droplevels
    return(ans)
}

####################################
# make_transaction_index(df, term)
# ����w�������֐�
####################################
make_transaction_index <- function(df, term, timecount = FALSE){

    t_start <-proc.time()
    
    # ���̖��O���`�F�b�N�̕K�v����H
    date_name <- names(table(df$qtr))
    
    # ���͌���
    data_list <- df %>% analyse_by_rolling_window(term)

    ###################################
    # ������_�W�������o��
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
# rollingWindow�@�ɂ��d��A����
#
# ���l
# term���P�ȉ��ɂ���ƑS�̂ŏd��A����
# ���ʂ�lm�̌��ʂ����Ԃ��Ƃ̃��X�g�Ƃ��ĕԂ�
################################################################
analyse_by_rolling_window <- function(df, term){
    
    # ���Ԑݒ肪�R�ȏ�̎��́A���[�����O�E�C���h�E�@
    # ����ȊO�̏ꍇ�A�S�̊��Ԃ̏d��A���͂��s��
    if( term > 2){
        df_list <- devide_data_by_term(df, term)
    }else{
        df_list <- list()
        df_list[[1]] <- df
    }

    ########################
    # ���f���������Œ�`
    ########################
    .my_lm <- function(df){
        mod <- paste("log(������z/10000) ~ qtr +",
                     ifelse(length(unique(df$�w��))==1, "", "�w�� + "),
                     ifelse(length(unique(df$TSD))==1, "", "TSD +"),
                     ifelse(length(unique(df$�n�於))==1, "","�n�於 + "),
                     "�n�� + �����ʐ� + �z��N + ���H����", sep = "")

        return(lm(mod, data=df))
    }   
    
    ########################
    # lm�̎�����
    ########################
    ans_list <- list()
    for(i in seq_along(df_list)){
        ans_list[[i]] <- .my_lm(df_list[[i]])
    }

    return(ans_list)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# �����̃��C���֐�
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#############################################################################
# LBdata_graph(df)
# ���͗p�܂Ƃ߃O���t�̕\��
#############################################################################
LBdata_graph <- function(df)
{
    backup_mfrow <- par()$mfrow
    par(mfrow = c(3,2))
    
    hist(df$������z, xlab="������i�̑��z")
    hist(df$�n��, xlab="�ʐ�(�n��)")
    hist(df$�����ʐ�, xlab="�����ʐ�")
    hist(df$�z��N, xlab="�z��N")
    
    plot(df$�n��, df$������z, xlab="�ʐ�(�n��)", ylab="������i�̑��z")
    plot(df$�n��, df$������z/df$�n��, xlab="�n��", ylab="������z�^�n��")

    par(mfrow = backup_mfrow)
}

#############################################################################
# LBdata_summary(df)
# �T�}���[�̕\��
#############################################################################
LBdata_summary <- function(df)
{
    s_data <- data.frame(P  = df$������z/10000, #�~�����~�ɕϊ�
                         L  = df$�n��,
                         PPL= (df$������z/10000) / df$�n��,
                         S  = df$�����ʐ�,
                         A  = df$�z��N)
    
    s <- cbind(apply(s_data,2,mean),
               apply(s_data,2,sd),
               apply(s_data,2,min),
               apply(s_data,2,max))

    # �񖼂ɒP�ʓ��̏������邩�ǂ����͌������ׂ�
    # �\�����ړI�Ȃ炻��ł悢����
    rownames(s) <- c("P: ������i�̑��z(���~)",
                     "L: �ʐ�(�u)",
                     "P/L: �u�P��(���~/�u)",
                     "S: �����ʐ�(�u)",
                     "Age: ���z��N��(�N)")

    colnames(s) <- c("����","�W���΍�","�ŏ��l","�ő�l")

    return(s)
}

#############################################################################
# draw_transaction_index_graph(index_vector)
# ����w�����O���t�ŕ\��
#############################################################################
draw_transaction_index_graph <- function(index_data){
    # ������̕�����𐔒l�ɕϊ�
    qtr_strings <- names(index_data)
    tmp <- str_match(qtr_strings, "(\\d{4})Q(.)")
    qtr_num <- as.numeric(tmp[,2])  + (as.numeric(tmp[,3]) - 1) * 0.25 + 0.125
    
    # �f�[�^�t���[���̍쐬
    df <- data.frame(transaction_term = qtr_num, index = index_data)
    
    #print(df)
   
    ggplot(data=df, aes(x=transaction_term, y=index) ) + 
        geom_line(linetype=1,size=0.5) +
        geom_point(size=1,alpha=0.5) +
        geom_hline(yintercept = 1,size=1)
    
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# �������[�`��
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#############################################################
# add_data_cols(df)
# �����:qtr�A����N:tori_year�A������_���l:jiten
# �ȏ�R��̃f�[�^��ǉ��������
#############################################################
add_date_cols <- function(df)
{
    ##################################
    # �u����N�vtori_year��̍쐬
    date_string <- as.character(df$������_)
    tori_year <- conv_JC_to_AD(date_string)
    
    ##################################
    # �u������vqtr��̍쐬
    q_str <- date_string
    q_str[grep("��P�l����",q_str)] <- "1"
    q_str[grep("��Q�l����",q_str)] <- "2"
    q_str[grep("��R�l����",q_str)] <- "3"
    q_str[grep("��S�l����",q_str)] <- "4"

    qtr <- paste0(tori_year, "Q", q_str)

    ##################################
    # �u������_���l�vjiten��̍쐬
    #  ������̏�����
    jiten <- as.integer(tori_year) + (as.numeric(q_str) - 1) * 0.25
    
    ##################################
    # �f�[�^�ɍ���
    df <- cbind(df, tori_year, qtr, jiten)

    return(df)
}

################################################
# add_station_data_cols(df)
# �Ŋ��w�f�[�^�����ATSD���eki_fun��̒ǉ�
#
# ���l
# �I���W�i���f�[�^���ōŊ�w����NA�̂��̂����邪�A
# ���̍s���폜�����A���̂܂�NA�ŕԂ�
###############################################
add_station_data_cols <- function(df)
{

    # 3�o�C�g�ȏ�𕶎���Ƃ��ĔF�����āA�敪
    # eki_hun��ŉ�������c���A�R�O���ȏ��NA
    moyori <- as.character(df$�w��)
    eki_hun <- ifelse(nchar(moyori) > 2, NA, moyori) %>% as.integer

    # TSD�͎��ԋ������敪
    TSD <- moyori

    TSD <- ifelse(TSD == "30��?60��", "4.30��-60������",       TSD)
    TSD <- ifelse(TSD == "1H?1H30",   "5.1����-1����30������", TSD)
    TSD <- ifelse(TSD == "1H30?2H",   "6.1����30��-2���Ԗ���", TSD)
    TSD <- ifelse(TSD == "2H?",       "7.2����-",          TSD)
  
    TSD <- ifelse(0<=eki_hun & eki_hun<10,     "1.10������",            TSD)
    TSD <- ifelse(10<=eki_hun & eki_hun<20,    "2.10��-20������",       TSD)
    TSD <- ifelse(20<=eki_hun & eki_hun<30,    "3.20��-30������",       TSD)

    #���̃f�[�^�t���[���ɒǉ�����`�Ŗ߂�
    df <- cbind(df, TSD, eki_hun)
    return(df)
}

#####################################################
# devide_data_by_term(df, term)
# �����f�[�^�t���[�������Ԗ��ɍĔz��
# ���[�����O�̂P���Ԃ�term�œn��
# 
# TODO
# �e�^�[���̘A�����������ł̓`�F�b�N���Ă��Ȃ�
# �ǂ��Ń`�F�b�N������ׂ����𓝈ꂵ�Ă����ׂ�
#####################################################
devide_data_by_term <- function(df, term){
    ans_list <- list()

    # TODO
    # qtr_vec���e���_�̃C���f�b�N�X�ɂȂ邪�A
    # �A�����͒S�ۂ���Ă���̂��s��
    # names(table())���g���ƁA���O���ɕ��Ԃ̂��s��
    # �Ǝ������ɏ���������������S����
    qtr_vec <- names(table(df$qtr))
    
    # ���Ԃ��ƂɊ��ԓ��̃f�[�^��T��
    for (i in 1:(length(qtr_vec) - (term - 1))){
        ans_list[[i]] <- subset(df,
                                qtr_vec[i] <= as.character( df$qtr ) &
                                as.character( df$qtr ) <= qtr_vec[i + (term - 1)])
    }
    
    # ����̎��_�����Ԃ̖��O�i�K�v�Ȃ������j
    names(ans_list) <- qtr_vec[1:(length(qtr_vec) - (term - 1))]
    
    return(ans_list)
}

####################################################
# qtr_seq_check(lm_result)
# coef����qtr�̃`�F�b�N
# 
# ���l
# lm���͂̌��ʃI�u�W�F�N�g����coefficients���X�g��
# ���Y���f���ł́A�ؕЂ̎��ɂ���
# �uqtr*�v�����_�ɂ��Ă̌W����\���̂ŁA
# ���͈̔͂𓾂邽�߂̃��[�`��
#�@�����āA�A�������`�F�b�N���Ă���A
# �A�����Ă��Ȃ��ꍇ�́Aqtr*�������Ă��Ă�
# �r�؂ꂽ������index��Ԃ�
####################################################
qtr_seq_check <- function(lm_res){
    .names <- names(lm_res$coef)
    ans <- 2

    #qtr�̍ŏ��̕����͂Q���
    next_word <- make_next(.names[2])
    
    #�����Q��ڂ�qtr�������Ȃ��ꍇNA
    if(is.na(next_word)){
        return(NA)
    }
    
    #�ŏ��̂Q����O��
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
# �^����ꂽqtr������̎����ɂ����镶�����Ԃ�
# 
# ���l
# coef��qtr�v�f�̘A�������`�F�b�N���邽�߂̃��e�B���e�B
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
# �����ėp���e�B���e�B
# �f�[�^�t���[���\���Ɉˑ����Ȃ�����
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#############################################################
# �a��iJapanese Clendar�j����(AD)�ϊ��̃��[�`��
# �f�[�^���ɘa��łĂ���Ƃ���Ŏg���₷���悤�ɍ���Ă���
# jc_To_ad �a��-�������ϊ�����֐������֐�
# �߁X�A�������ς��̂ł���ɑΏ��ł���悤��
#############################################################

# �����ϊ�����֐����쐬����ėp�֐�
jc_To_ad <- function(gengou, first_year)
{
        # ����Q�Ɨp�������t���̃}�b�`���O�p�^�[��������
    # "���a(.+)�N"�݂����Ȋ����̃}�b�`���O�p�^�[���ɂȂ�
    match_str <- paste0(gengou,"(.+)�N")

    ans_fnc <- function(input_str)
    {
        input_str <- as.character(input_str)
        str <- sub("���N","1�N",input_str)
        tmp <- str_match(str,match_str)
        
        #str_match�̖߂�l�͂P��ڂ��}�b�`�S�́A�Q��ڈȍ~������Q��
        ans <- ifelse(is.na(tmp[,1]), input_str, as.integer(tmp[,2]) + first_year - 1)
    
        return(ans)
    }
    return(ans_fnc)
}

# �a��琼��ɕϊ������ʊ֐�
# �V����������������ꍇ�A�����Œ�������΂悢
conv_JC_to_AD <- function(str)
{
    # �ʂ̕ϊ��֐����쐬
    syouwa_to_ad <- jc_To_ad("���a",1926)
    heisei_to_ad <- jc_To_ad("����",1989)
    
    return(str %>% syouwa_to_ad %>% heisei_to_ad %>% as.integer)
}
