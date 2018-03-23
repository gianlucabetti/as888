000100100210     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200100210     H*PARMS CVTOPT(*NONE)
000300100210     H*
000400100210     H*
000500100210     H DECEDIT('0,') DATEDIT(*DMY/)
000501170529      *
000502170529R258  * Questo pgm è stato creato con il task R258 Comunicaz. Spesometro Trimestrale
000503170529      *
000600100210     D*-------
000700100210     D KPJBA         E DS
000800100210     D*-------
000900100210     D* DS Interna per dati di output di SDGSOCDR
001000100210     D XSOCDS          DS          1000
001100100210     D*-------
001200100210     D* Dati di ambiente ottenuti da SDGSOCDR
001300100210     D SOC001        E DS                  EXTNAME(SDGSOCDS )
001400100210     D*-------
001500100210     DANPARds        E DS
001600100210     D*-------
001700100210     D* DS x pgm che reperisce dati (£par)
001800100210     D XPAOUT          DS          2000
001900100210?    c*---------------------------------------------------------------*
002000100210?    c*  CICLO PRINCIPALE                                             *
002100100210?    c*---------------------------------------------------------------*
002200100210?    C                   EXSR      inzvar
002300100210     C* legge amprm00f
002400100210     C                   EXSR      readpar
002500100210     C*
002600100210     c* richiama programma gestione video
002700100210     C* passando opzione che ha reimpostato xpar
002800100210     C                   eval      anpopz=xpaopz
002900170529     c                   call      'SDGPA12R'
003000100210     C                   PARM                    ANPARds
003100100210     C                   PARM                    KPJBA
003200100210     c* scrive anprm00f
003300100210     C                   EXSR      aggpar
003400100210     C*
003500100210     C                   SETON                                        LR
003600100210     C/EJECT
003700100210?    C*---------------------------------------------------------------*
003800100210?    C*  operazioni iniziali
003900100210?    C*---------------------------------------------------------------*
004000100210     C     INZVAR        BEGSR
004100100210     C*
004200100210     C     *ENTRY        PLIST
004300100210     C                   PARM                    KPJBA
004400100210    >C                   MOVEL     'SOC001'      TIPXSC
004500100210     C                   MOVEL     *BLANK        SOCXSC
004600100210     C*
004700100210     C                   CALL      'SDGSOCDR'
004800100210     C                   PARM                    TIPXSC            6
004900100210     C                   PARM                    SOCXSC            3
005000100210     C                   PARM                    CDSXSC            9 0
005100100210     C                   PARM                    MODXSC            3
005200100210     C                   PARM      *BLANKS       RTNXSC            1
005300100210     C                   PARM                    XSOCDS
005400100210     C                   PARM                    KPJBA
005500100210     C*
005600100210     C     RTNXSC        IfNe      '1'
005700100210    >C                   MOVEL     XSOCDS        SOC001
005800100210     C                   Else
005900100210     C                   Seton                                        LR
006000100210     C                   return
006100100210     C                   EndIf
006200100210     c*
006300100210     C                   ENDSR
006400100210     C************************************************************
006500100210     C* REPERIMENTO DATI tramite xpar
006600100210     C************************************************************
006700100210     C     Readpar       BEGSR
006800100210     C*
006900100210     C                   CALLB     'XPAR'
007000100210     C                   PARM      xscsoc        xpasoc            3
007100170529     C                   PARM      'SDSPETRI'    XPAPAR            8
007200100210     C                   PARM      *blanks       XPAOUT
007300100210     C                   PARM                    XPAERR            1
007400100210     C                   PARM      *blanks       XPARIC            1
007500100210     C                   PARM                    KPJBA
007600100210     C                   PARM      '02'          xpaopz            2
007700100210     c*
007800100210     C     XPAERR        IFEQ      '0'
007900100210     C                   MOVEL     XSCSoc        ANPSocieta
008000100210     C                   MOVEL     XPApar        ANPparame
008100100210     C                   MOVEL     XPAOUT        ANPdati
008200100210     C                   ELSE
008300100210     C                   SETON                                        LR
008400100210     C                   RETURN
008500100210     C                   ENDIF
008600100210     C*
008700100210     C                   ENDSR
008800100210     C/EJECT
008900100210     C************************************************************
009000100210     C* aggiornamento tramite xpar
009100100210     C************************************************************
009200100210     C     Aggpar        BEGSR
009300100210     C*
009400100210     c                   movel     anpdati       xpaout
009500100210     c                   movel     '1'           xparic
009600100210     C                   CALLB     'XPAR'
009700100210     C                   PARM                    xpasoc
009800170529     C                   PARM      'SDSPETRI'    XPAPAR
009900100210     C                   PARM                    XPAOUT
010000100210     C                   PARM                    XPAERR
010100100210     C                   PARM                    XPARIC
010200100210     C*
010300100210     C                   ENDSR
