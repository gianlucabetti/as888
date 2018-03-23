000100090812     D/COPY GAITRASRC/SRCPROTOPR,UBJDBC_H
000200081118     D*------------------
000300101004     D* PASSAGGIO PARAMETRI A PROCEDURE UBBVSQL01
000400081118     D*------------------
000500100601     D pInKeyword      S             64A
000600090812     D pInConn         S                   like(Connection)
000700090812     D pInProcesso     S              5I 0
000800090812     D pInCodCli       S              8A
000900090812     D pInTipFile      S              2A
001000100601     D pInUtente       S             20A
001100100601     D pInPassword     S             20A
001200100601     D pInPath         S            100A
001300100601     D pInDescr        S            100A
001400100601     D pInFlgDTF       S              1A
001500100601     D pInFlgFEB       S              1A
001600100601     D pInFlgANN       S              1A
001700100916     D pInTipTrasf     S              2A
001800100916     D pInMaskChk      S             50A
001900100916     D pInMaskFile     S            150A
002000100916     D pInMaskSem      S            150A
002100100916     D pInPercorso     S            100A
002200100916     D pInEmail1       S            100A
002300100916     D pInEmail2       S            100A
002400100916     D pInEmail3       S            100A
002500100916     D pInEmail4       S            100A
002600100916     D pInEmail5       S            100A
002700100916     D pInSoggetto     S            100A
002800100916     D pInTesto        S            100A
002900100916     D pInFtpAddr      S            100A
003000100916     D pInFtpDir       S            100A
003100100916     D pInFtpUser      S             50A
003200100916     D pInFtpPwd       S             20A
003300100916     D pInFtpFilAS4    S             50A
003400100916     D pInPercorsOld   S            100A
003500100916     D pInTentativi    S              5I 0
003600100916     D pInRitardo      S              5I 0
003700100916     D pInLibAS4       S             50A
003800100916     D pInFilAS4       S             50A
003900100916     D pInMbrAS4       S             50A
004000100916     D pInSchedul      S            100A
004100100916     D pInAccorpam     S              5I 0
004200100916     D pInMaskZip      S            150A
004300100916     D pInDataStr      S             10A
004400100916     D pInDataEnd      S             10A
004500100916     D pInCodCompr     S             10A
004600100916     D pInFtpCmd1      S            100A
004700100916     D pInEmailErr     S            100A
004800101004     D pInHttpUrlTo    S            512A
004900100916     D pInHttpUsrTo    S             50A
005000100916     D pInHttpPwdTo    S             20A
005100100916     D pInHttpConTyp   S             50A
005200100916     D pInHttpSndEXE   S             50A
005300100916     D pInHttpSndPrm   S             50A
005400100916     D pInHttpUsPrxy   S              1A
005500100916     D pInUsrIns       S             20A
005600101004     D pInNote         S            256A
005700100916     D pInFtpPort      S              5I 0
005800100916     D pInFtpTyp       S              5I 0
005900100916     D pInKeywParam    S             50A
006000101004     D pInTipJob       S              5I 0
006100101022     D pInUsrPwd       S             20A
006200101004     D*
006300090812     D pOutConn        S                   like(Connection)
006400090812     D pOutIpAddress   S             15A
006500090812     D pOutPorta       S             10I 0
006600090812     D pOutNumRec      S             10I 0
006700100916     D pOutValParam    S            200A
006800101004     D pOutProcesso    S              5I 0
006900101004     D pOutNumJob      S              5I 0
007000101004     D pOutTipTrasf    S              2A
007100101004     D pOutStsJob      S              1A
007200101022     D pOutUsrPwd      S             20A
007300101025     D pOutUsrPath     S            100A
007400101025     D pOutUsrDesc     S            100A
007500101025     D pOutUsrFAnn     S              1A
007600101025     D pOutUsrEmail    S            100A
007700101025     D pOutUsrNote     S           1000A
007800101025     D pOutUsrFList    S            200A
007900101025     D*
008000101025     D pOutCliTipTras  S              2A
008100101025     D pOutCliMskChk   S             50A
008200101025     D pOutCliMskFil   S            150A
008300101025     D pOutCliMskSem   S            150A
008400101025     D pOutCliPerc     S            100A
008500101025     D pOutCliEml1     S            100A
008600101025     D pOutCliEml2     S            100A
008700101025     D pOutCliEml3     S            100A
008800101025     D pOutCliEml4     S            100A
008900101025     D pOutCliEml5     S            100A
009000101025     D pOutCliSogg     S            100A
009100101025     D pOutCliTesto    S            100A
009200101025     D pOutCliFtpAddr  S            100A
009300101025     D pOutCliFtpDir   S            100A
009400101025     D pOutCliFtpUsr   S             50A
009500101025     D pOutCliFtpPwd   S             20A
009600101025     D pOutCliFtpFAS4  S             50A
009700101025     D pOutCliPercOld  S            100A
009800101025     D pOutCliTentat   S              5I 0
009900101025     D pOutCliRitardo  S              5I 0
010000101025     D pOutCliLibAS4   S             50A
010100101025     D pOutCliFilAS4   S             50A
010200101025     D pOutCliMbrAS4   S             50A
010300101025     D pOutCliProc     S              5I 0
010400101025     D pOutCliSched    S            100A
010500101025     D pOutCliAccorp   S              5I 0
010600101025     D pOutCliMskZip   S            150A
010700101025     D pOutCliDataStr  S             10A
010800101025     D pOutCliDataEnd  S             10A
010900101025     D pOutCliCodComp  S             10A
011000101025     D pOutCliFtpCmd1  S            100A
011100101025     D pOutCliEmlErr   S            100A
011200101025     D pOutCliHtUrlTo  S            512A
011300101025     D pOutCliHtUsrTo  S             50A
011400101025     D pOutCliHtPwdTo  S             20A
011500101025     D pOutCliHtConTy  S             50A
011600101025     D pOutCliHtSndEx  S             50A
011700101025     D pOutCliHtSndPr  S             50A
011800101025     D pOutCliHtUsPxy  S              1A
011900101025     D pOutCliUsrIns   S             20A
012000101026     D pOutCliUsrUpd   S             20A
012100101026     D pOutCliDatUpd   S             19A
012200101025     D pOutCliNote     S            256A
012300101026     D pOutCliFtpPrt   S              5A
012400101026     D pOutCliFtpTy    S              5A
