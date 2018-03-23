000100090127     H DFTACTGRP(*NO) BNDDIR('PRNPGM')
000200990322     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300990602     Ftii318D   CF   E             WORKSTN
000400951219     FTABEL00F  IF   E           K DISK
000500000000     D PARAM2          DS
000600990323     D  VIDNFT                 1      7  0
000700990323     D  VIDDFT                 8     15  0
000800000406     D  VIDTIP0               16     18    inz('000')
000900000406     D  VIDTIP1               19     19
001000000406     D  VIDTIp                16     19
001100990323     D  VIDDBL                20     27  0
001200990323     d  wsocieta              28     30
001300870826     D*
001400000000     D KPJBA         E DS
001500951219     D*-------------
001600951219     D* DS DATI VARI CONTABILITA'
001700951219     D*-------------
001800951219     D DSQC1         E DS
001900951219     D DSQC2         E DS
002000951219     D*-------------
002100951219     D* DS DATI VARI TASSAZIONE
002200951219     D*-------------
002300870826     D*---- PASSAGGIO PARAMETRI
002400870826     D  LIBSYS                99    107
002500870826     D  MSGERR               453    502
002600990322     d dataiso         s               d   datfmt(*iso)
002700990322     d dataeur         s               d   datfmt(*eur)
002800990322     D  XNMDAG         S               d   datfmt(*iso)
002900990322     d  XNMDAT         S               d   datfmt(*iso)
003000990322     D soc001        e ds                  extname(xsoc001ds)
003100990322     D xsocds          DS          1000
003200990323    >D Ctb             S              2
003300990323     D TpReg           S              1
003400990323     D DtReg           S             10
003500990323     D DtLGio          S             10
003600990323     D Esito           S              1
003700990323     D Gesmsg          S              1
003800870826     C******************************************************
003900000000     C     *ENTRY        PLIST                                                  *
004000000000     C                   PARM                    KPJBA                          *
004100990322     C     PNMR          PLIST
004200990322     C                   PARM                    XNMRIC            1
004300990322     C                   PARM                    XNMSOC
004400990322     C                   PARM                    XNMUNI
004500990322     C                   PARM                    XNMCTB
004600990322     C                   PARM                    XNMKEY
004700990322     C                   PARM                    XNMSER
004800990322     C                   PARM                    XNMCMT            1
004900990322     C                   PARM                    XNMDAT
005000990322     C                   PARM                    XNMIVA            1
005100990322     C                   PARM                    XNMREG            1
005200990322     C                   PARM                    XNMERR            1
005300990322     C                   PARM                    XNMNUM            9 0
005400990322     C                   PARM                    XNMAUT            1
005500990322     C                   PARM                    XNMDAG                         opzionale
005600990322     C*---------- RICERCA DITTA :
005700990322     C                   MOVEL     'SOC001'      TIPXSC
005800990322     C                   MOVEL     *blank        SOCXSC
005900990322     C                   EXSR      REPSOC
006000990322     C     RTNXSC        IFNE      '1'
006100990322     C                   MOVEL     XSOCDS        SOC001
006200990323     C                   MOVEL     xscrgs        rsut
006300990323     c                   move      xscsoc        wsocieta          3
006400990322     c                   end
006500990322     C*----------                                          *
006600990322     c                   time                    como14           14 0
006700990322     c                   move      como14        woggi             8 0
006800870825     C*                                                    *
006900951219     C     KTABEL        KLIST
007000951219     C                   KFLD                    CODUT
007100951219     C                   KFLD                    CODTAB            2
007200951219     C                   KFLD                    CODCOD            8
007300891215     C** RICERCA NUMERI FATTURA
007400000000     C******************************************************
007500891215     C                   Z-ADD     1             CODUT             1 0
007600951219     C*
007700951219     C* CARICAMENTO DATI VARI CONTABILITA'
007800951219     C                   EXSR      CARQC
007900951219     C*
008000951219     C* CARICAMENTO DATI VARI TASSAZIONE
008100951219     C*
008200990323     C                   MOVEl     §QCRIV        wserie            4            REG.IVA VEND.
008300990323     C                   MOVE      §QCFI3        wserie                         LIBRO IVA VEND.
008400000000     C******************************************************
008500000000     C     START         TAG
008600870902     C                   Z-ADD     0             VIDNFT
008700870902     C                   Z-ADD     0             VIDDFT
008800870902     C                   Z-ADD     0             VIDDBL
008900000406     C                   MOVEL     *BLANKS       VIDTIP1
009000870826     C******************************************************
009100870826     C*PRELIEVO IL 1° NUMERO DI FATTURA DALLA DATA AREA DA
009200870826     C*PRESENTARE A VIDEO
009300990322     C                   z-add     woggi         VIDDFT
009400990322     C                   Z-ADD     woggi         VIDDBL
009500870826     C     SU            TAG
009600870826     C                   EXFMT     OCO316D1
009700990322     C   Kl              GOTO      FINE
009800870826     C** FINE LAVORO
009900870826     C******************************************************
010000870826     C*CONTROLLO CHE LA DATA FATTURA SIA SEMPRE MAGGIORE O
010100870826     C*UGUALE ALL'ULTIMA DATA DI FATTURAZIONE CHE SI TROVA
010200891215     C* IN CNNUM DEL LIBRO IVA
010300090127     c*    *eur          test(d)                 viddft                 72
010400090127     c*                  if        *in72
010500090127     c*    *dmy          test(d)                 viddft                 72
010600090127     c* n72*dmy          move      viddft        dataeur
010700090127     c* n72              move      dataeur       viddft
010800090127     c*                  else
010900090127     c*                  move      viddft        dataeur
011000090127     c*                  end
011100090127     C*  72              GOTO      su
011200090127     C*                  move      dataeur       dataiso
011300090127     C*                  setoff                                       70
011400090127     c*                  exsr      mvc
011500090127     c*                  if        esito = *on
011600090127     C*                  seton                                        70
011700090127     C*                  GOTO      su
011800090127     c*                  end
011900090127     C*                  move      DATAIso       DTAFAT            8 0
012000090127     C*                  move      dtafat        xnmdat
012100090127     C*                  move      dtafat        xnmdag
012200090127     C*                  exsr      repnum
012300090127     C*                  Z-ADD     xnmnum        NUMFAT            7 0
012400090127     C*                  Z-ADD     NUMFAT        VIDNFT
012500090127     C*                  ADD       1             VIDNFT
012600870826     C******************************************************
012700870826     C*CONTROLLO CHE LA DATA BOLLA SIA SEMPRE MINORE O UGUA-
012800870826     C*LE DELLA DATA FATTURA
012900990323     c     *eur          test(d)                 viddbl                 73
013000990323     c                   if        *in73
013100090127     c     *eur          test(d)                 viddbl                 73
013200090127     c  n73*eur          move      viddbl        dataeur
013300990323     c  n73              move      dataeur       viddbl
013400990323     c                   else
013500990323     c                   move      viddbl        dataeur
013600990323     c                   end
013700990323     C   73              GOTO      su
013800990323     C                   move      dataeur       dataiso
013900990323     C                   move      DATAIso       DTABOL            8 0
014000090127     C*    DTABOL        IFGT      DTAFAT
014100090127     C*                  SETON                                        74
014200090127     C*                  GOTO      SU
014300090127     C*                  END
014400870826     C******************************************************
014500870826     C*CONTROLLI SUPERATI
014600870826     C                   SETON                                        98
014700870826     C   98
014800870826     CANNKA              GOTO      SU
014900870826     C   KA              DO
015000990413     C                   MOVEL     'I319'        KCOAZ
015100870826     C                   MOVEL     PARAM2        KPJBU
015200000403     C                   CALL      'BCH09'
015300000403     C                   PARM                    KPJBA
015400010622     C                   CALL      'BCH10'
015500010622     C                   PARM                    KPJBA
015600870826     C                   END
015700870826     C******************************************************
015800870826     C*FINE PROGRAMMA
015900870826     C     FINE          TAG
016000870826     C                   SETON                                        LR
016100951219     C*------------------------------------------------------------------------*
016200951219     C* CARQC -  CARICAMENTO DATI VARI CONTABILITA'
016300951219     C*------------------------------------------------------------------------*
016400951219     C     CARQC         BEGSR
016500951219     C* DATI VARI 1
016600951219     C                   CLEAR                   DSQC1
016700951219     C                   Z-ADD     1             CODUT
016800951219     C                   MOVEL     'QC'          CODTAB
016900951219     C                   MOVEL(P)  '1'           CODCOD
017000951219     C     KTABEL        CHAIN     TABEL00F                           55
017100000403IF  1C     *IN55         IFEQ      *OFF
017200951219     C                   MOVEL     TBLUNI        DSQC1
017300951219E   1C                   ENDIF
017400951219     C* DATI VARI 2
017500951219     C                   CLEAR                   DSQC2
017600951219     C                   MOVEL     'QC'          CODTAB
017700951219     C                   MOVEL(P)  '2'           CODCOD
017800951219     C     KTABEL        CHAIN     TABEL00F                           55
017900951219IF  1C     *IN55         IFEQ      *OFF
018000951219     C                   MOVEL     TBLUNI        DSQC2
018100951219E   1C                   ENDIF
018200951219     C*
018300951219     C                   ENDSR
018400990322     C*----------------------------------------------------*
018500990322     C* Reperisco numeratore
018600990322     C*----------------------------------------------------*
018700990322     c     repnum        begsr
018800990323     c                   move      '1'           xnmric
018900990323     C                   MOVE      wsocieta      XNMSOC            3
019000990322     C                   MOVE      *BLANKS       XNMUNI            8
019100990322     C                   MOVE      'CG'          XNMCTB            2
019200990322     C                   MOVE      *BLANKS       XNMKEY            8
019300990322     C                   MOVE      WSERIE        XNMSER            4
019400990322     C                   MOVE      *OFF          XNMCMT            1
019500990322     C                   MOVE      *On           XNMIVA            1
019600990322     C                   MOVE      *OFF          XNMREG            1
019700990322     C                   MOVE      '0'           XNMerr            1
019800990322     C*
019900990322     C                   CALLB     'XNMR'        PNMR
020000990322     C*
020100990322     C     xnmerr        ifne      '0'
020200990322     C                   seton                                        h5
020300990322     C                   end
020400990322      *
020500990322     C                   endsr
020600990323     C*----------------------------------------------------*
020700990323     C* controllo libro giornale
020800990323     C*----------------------------------------------------*
020900990323     c     mvc           begsr
021000990323     c                   move      dataiso       dtreg
021100990323     c                   move      dataiso       dtlgio
021200990323     C                   CALLB     'NDMVC106'
021300990323     C                   PARM                    wsocieta
021400990323     C                   PARM      'CG'          CTB
021500990323    >C                   PARM      'D'           TpReg
021600990323    >C                   PARM                    DtLGio
021700990323    >C                   PARM                    DtReg
021800990323     C                   PARM      *on           GesMsg
021900990323     C                   PARM                    Esito
022000990323     c                   endsr
022100990322     C*----------------------------------------------------*
022200990322     C* Reperimento dati società
022300990322     C*----------------------------------------------------*
022400990322     C     REPSOC        BEGSR
022500990322     C*
022600990322     C                   CALLB     'XSOC'
022700990322     C                   PARM                    TIPXSC            6
022800990322     C                   PARM                    SOCXSC            3
022900990322     C                   PARM                    CDSXSC            9 0
023000990322     C                   PARM                    MODXSC            3
023100990322     C                   PARM      *blanks       RTNXSC            1
023200990322     C                   PARM                    XSOCDS
023300990322     C                   PARM                    KPJBA
023400990322     C*
023500990322     C                   ENDSR
