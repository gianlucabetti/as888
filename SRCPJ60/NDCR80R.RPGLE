000100981214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300951124     H*PARMS CVTOPT(*DATETIME)
000400951124     H DECEDIT('0,') DATEDIT(*DMY.)
000500951124     F*---------------------------------------------------------------*
000600951124     F*                LANCIO ESTRAZIONE ARCHIVI                      *
000700951124     F*                  PER MOD. 770 GENERALE                        *
000800920514     F*---------------------------------------------------------------*
000900951124     Fndcr80d   CF   E             WORKSTN
001000951124     D*---------------------------------------------------------------*
001100920514     D KPJBA         E DS
001200951124     D*-----------
001300951124     D ndcr80ds      E DS
001400951124     D*-------------
001500951124     D* Dati di ambiente ottenuti da XSOC
001600951124    >D SOC001        E DS                  EXTNAME(XSOC001DS)
001700951124     D*-------------
001800951124     D* DS Interna per dati di output di XSOC
001900951124     D XSOCDS          DS          1000
002000951124     D*-------------
002100951124     D* Reperimento nome PGM
002200951124     D STATUS         SDS           333
002300951124     D  DSPGM            *PROC
002400951124     D  PARMS            *PARMS
002500951124     C*---------------------------------------------------------------*
002600951124     C                   EXSR      INZVAR                                       *
002700951124     C*
002800920514     C     INIZIO        TAG                                                    *
002900951124     C                   Z-ADD     0             d1anno                         *
003000020607D1143X**** ex C1478      Move      XScOn         D1On1
003100020607  "  X**** ex C1478      Move      XScOff        D1Off1
003200020607D1143X**** ex C1478      Move      XScOff        D1Euro
003300140509R148 C**** ex D1143      Move      XScOn         D1On2
003400140509R148 C**** ex D1143      Move      XScOff        D1Off2
003500140509R148 C**** ex D1143      Move      XScOff        D1Tronc
003600951124     C*
003700920514     C     VIDEO         TAG                                                    *
003800951124     C*
003900951124     C     *IN99         IFEQ      '1'
004000951124     C                   SETOFF                                       99
004100951124     C                   WRITE     CR80D1
004200951124     C                   SETON                                        99
004300951124     C                   ENDIF
004400951124     C*
004500920514     C                   EXFMT     CR80D1                                       *
004600951124     C*
004700920514     C   KC              GOTO      FINE                                         *
004800951129     C   KN              GOTO      FINE                                         *
004900951124     C*
005000951124     C* F21 = Parametri batch
005100951124     C     *INKV         IFEQ      '1'
005200951129     C                   MOVE      'CR81'        KCOAZ
005300951124     C                   CALL      'BCH09'
005400951124     C                   PARM                    KPJBA
005500951124     C                   GOTO      VIDEO
005600951124     C                   ENDIF
005700951124     C*
005800951124     C                   setoff                                       99
005900951124     C     d1anno        ifeq      0
006000951124     C                   seton                                        5099
006100951124     C                   endif
006200020607D1143X**** ex C1478      If        D1Euro    <> XScOn and
006300020607  "  X**** ex C1478                D1Euro    <> XScOff
006400020607  "  X**** ex C1478      SetOn                                        5199
006500020607D1143X**** ex C1478      EndIf
006600020607D1143C*
006700140509R148 C**** ex D1143      If        D1Tronc   <> XScOn and
006800140509R148 C**** ex D1143                D1Tronc   <> XScOff
006900140509R148 C**** ex D1143      SetOn                                        5299
007000140509R148 C**** ex D1143      EndIf
007100951124     C*
007200951124     C  NKF
007300951124     COR 99              GOTO      VIDEO
007400951124     C*
007500951124     C                   move      xscsoc        socR80
007600951124     C                   z-add     1             per1R80
007700951124     C                   movel     d1anno        per1R80
007800951124     C                   z-add     12            per2R80
007900951124     C                   movel     d1anno        per2R80
008000020607D1143X**** ex C1478      If        D1Euro    = XScOn
008100020607  "  X**** ex C1478      Move      *On           EuroR80
008200020607  "  X**** ex C1478      Else
008300020607  "  X**** ex C1478      Move      *Off          EuroR80
008400020607D1143X**** ex C1478      EndIf
008500140509R148 C**** ex D1143      If        D1Tronc   = XScOn
008600140509R148 C**** ex D1143      Move      *On           TroncR80
008700140509R148 C**** ex D1143      Else
008800140509D1143C                   Move      *Off          TroncR80
008900140509R148 C**** ex D1143      EndIf
009000951124     C                   MOVEL     ndcr80ds      KPJBU
009100951124     C*
009200951124     C                   MOVE      'CR81'        KCOAZ
009300951128     C                   CALL      'BCH10'
009400951124     C                   PARM                    KPJBA
009500951124     C*
009600920514     C     FINE          TAG                                                    *
009700920514     C                   SETON                                        LR        *
009800920514     C*----------------------------------------------------*
009900920514     C* SUBROUTINE INIZIALE DEFINIZIONE CAMPI E CHIAVI     *
010000920514     C*----------------------------------------------------*
010100951124     C     INZVAR        BEGSR                                                  *
010200951124     C*
010300920514     C     *ENTRY        PLIST                                                  *
010400920514     C                   PARM                    KPJBA                          *
010500951124     C*
010600951124     C* Reperimento dati società
010700951124     C*
010800951124    >C                   MOVEL     'SOC001'      TIPXSC
010900951124     C                   MOVEL     *BLANK        SOCXSC
011000951124     C                   CALL      'XSOC'
011100951124     C                   PARM                    TIPXSC            6
011200951124     C                   PARM                    SOCXSC            3
011300951124     C                   PARM                    CDSXSC            9 0
011400951124     C                   PARM                    MODXSC            3
011500951124     C                   PARM      *BLANKS       RTNXSC            1
011600951124     C                   PARM                    XSOCDS
011700951124     C                   PARM                    KPJBA
011800951124     C     RTNXSC        IFNE      '1'
011900951124     C                   MOVEL     XSOCDS        SOC001
012000951124     C                   ELSE
012100951124     C                   RETURN
012200951124     C                   ENDIF
012300951124     C*
012400951124     C* Valorizzazione campi univoci testate
012500951124     C*
012600951124     C                   MOVEL     KNSIF         NOMSIF
012700951124     C                   MOVEL     XSCDSI        NOMDIT
012800951124     C                   MOVEL     DSPGM         NOMPGM
012900020607D1143X****
013000020607  "  X**Nascondo il campo 'Versamenti in Euro' (non dovrebbe più servire)
013100020607D1143X****               SETOFF                                       11
013200951124     C*
013300920514     C                   ENDSR                                                  *
