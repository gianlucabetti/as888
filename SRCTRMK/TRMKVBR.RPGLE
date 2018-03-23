000100000606     H DECEDIT('0,') DATEDIT(*DMY.)
000200091019     H* TRMKVBR *----------------------------------------------------*
000300091019     H* crea WORK-FILE per visite di clienti in comproprietà          *
000400000000     H*--------------------------------------------------------------*
000500091019     FtrmkvbD   CF   E             WORKSTN
000600941223     D*
000700920925     D WLBDAT          DS
000800941223     D  G02DAT                 1      8  0
000900941223     D  G02INV                 9     16  0
001000941223     D  G02ERR                17     17
001100941223     D  G02TGI                18     22  0
001200900517     D KPJBA         E DS
001300051114     d Azuteds       e ds                  Extname(Azute00f)
001400051114     d dDatiute      e ds
001500051114     d Tibs34ds      e ds                  Inz
001600051114     D*-------------------
001700051114     D* DS PARAMETRI DI LANCIO PGM
001800051114     D*-------------------
001900051114     D PARAM           DS
002000051114     D  PARdda                 1      8  0
002100051114     D  PARdal                 9     16  0
002200091020     D  VIDdSD                17     24  0
002300091020     D  VIDdSA                25     32  0
002400051114     d
002500051114     D comdsd          S                   LIKE(viddsd) inz
002600051114     D comdsa          S                   LIKE(viddsa) inz
002700091019     d
002800091019     d Qcmd            s            100    inz
002900091019     d Qlen            s             15  5 inz(100)
003000000000     C*---------------------------------------------------------------*
003100000000     C     *ENTRY        PLIST
003200000000     C                   PARM                    KPJBA
003300051114     c     *dtaara       define    §azute        azuteds
003400051114     c     *dtaara       define    §datiute      ddatiute
003500051114     c                   in(E)     *dtaara
003600051114     c                   If        %error  or RSUT = *blanks
003700051114     c                   CLEAR                   tibs34ds
003800051114     c                   CALL      'TIBS34R'
003900051114     c                   PARM                    tibs34ds
004000051114     c                   in        *dtaara
004100051114     c                   EndIf
004200941223     C*
004300900511     C*---------------------------------------------------------------*
004400920925     C*
004500900524     C                   Z-ADD     0             VIDDSD
004600900524     C                   Z-ADD     0             VIDDSA
004700000615     C*
004800900509     C     FOR01         TAG
004900091019     C                   EXFMT     mkvbD01
005000941223     C*
005100091019     C** F3 - FINE LAVORO
005200900509     C   KC              GOTO      FINE
005300091019     c
005400091019     c* f9 - Visulizza file
005500091019     c                   if        *inki=*on
005600091019     c                   EXSR      F09d01
005700091019     c                   goto      for01
005800091019     c                   endif
005900900524     C* CONTROLLO
006000900524     C                   EXSR      CONTR
006100941223     C  NKF
006200900524     COR 90              GOTO      FOR01
006300091019     C** F6 - ELABORAZIONE
006400051114     C                   Z-ADD     comDSD        pardda
006500051114     C                   Z-ADD     comdsa        pardal
006600000606     C                   CLEAR                   KPJBU
006700051114     C                   MOVEL     param         KPJBU
006800091019     C                   MOVEL     'MKVC'        KCOAZ
006900091020     C                   CALL      'TRMKVCR'
007000900523     C                   PARM                    KPJBA
007100920925     C*
007200920925     C     FINE          TAG
007300000000     C                   SETON                                        LR
007400900524     C*
007500900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
007600900524     C     CONTR         BEGSR
007700900524     C                   SETOFF                                       90
007800091019     C* DATA visita DAL OBBLIGATORIA
007900920925     C                   MOVE      VIDDSD        G02DAT
008000920925     C                   MOVEL     *BLANK        G02ERR
008100941223     C                   CALL      'XSRDA8'
008200920925     C                   PARM                    WLBDAT
008300920925     C     G02ERR        IFEQ      '1'
008400920925     C                   SETON                                        40  90
008500920925     C                   GOTO      ENDCTR
008600920925     C                   END
008700051114     C                   Z-ADD     G02INV        comdsd
008800941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
008900941223     C                   Z-ADD     G02DAT        VIDDSD
009000900524     C*
009100091019     C* DATA visita AL OBBLIGATORIA
009200051114     C                   MOVE      VIDDSA        G02DAT
009300051114     C                   MOVEL     *BLANK        G02ERR
009400051114     C                   CALL      'XSRDA8'
009500051114     C                   PARM                    WLBDAT
009600051114     C     G02ERR        IFEQ      '1'
009700051114     C                   SETON                                        41  90
009800051114     C                   GOTO      ENDCTR
009900051114     C                   END
010000051114     C                   Z-ADD     G02INV        comdsa
010100051114     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
010200051114     C                   Z-ADD     G02DAT        VIDDSa
010300941223     C*
010400900524     C* DATA DAL NO MAGGIORE DATA AL
010500051114     C     comdsd        IFGT      comdsa
010600900524     C                   SETON                                        4290
010700900524     C                   GOTO      ENDCTR
010800900524     C                   END
010900941223     C*
011000900524     C     ENDCTR        ENDSR
011100091019      *---------------------------------------------------------------*
011200091019      * Visualizza file di WORK c
011300091019      *---------------------------------------------------------------*
011400091019     c     F09D01        BEGSR
011500091019      *
011600091019     c                   eval      Qcmd    =  'DSPDBF FILE'
011700091019     c                                     +  '(*LIBL/WFVCC00F) '
011800091019     c                                     +  'MBR(*FIRST) '
011900091019     c                                     +  'OUTPUT(*) '
012000091019     c                   call      'QCMDEXC'                            90
012100091019     c                   parm                    Qcmd
012200091019     c                   parm                    Qlen
012300091019      *
012400091019     c                   ENDSR
012500091019      *
012600920925     C*
