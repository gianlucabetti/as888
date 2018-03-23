000100010221     H DECEDIT('0,') DATEDIT(*DMY.)
000200020808
000300010221     Ffnfgv01l  IF   E           K DISK
000400010221     Ffnfwa01l  IF   E           K DISK
000500010221     Ffnfva01l  IF   E           K DISK
000600010221     FTABEL00F  IF   E           K DISK
000700010221     Fazorg01l  IF   E           K DISK
000800010221     Ftrtcs2d   CF   E             workstn
000900020808
001000020808      *
001100020808      * Schiere
001200020808      *
001300010221     D ERR             S             78    DIM(10) CTDATA PERRCD(1)             MSG video
001400020808      *
001500020808      * DS esterne
001600020808      *
001700010221     D UT§DSE0F      E DS
001800010221     D  TCU                  398    697    DIM(50)                              PARAM FLG7/8,TIPC
001900010221     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    PARAM CAPI CONTO
002000020808     D CNCR80        E DS
002100020808     D DSTV          E DS
002200020808     D OG143         E DS
002300020808     D KPJBA         E DS
002400020808     D trtcs2ds      E DS
002500020808      *
002600020808      * DS interne
002700020808      *
002800010221      * PASSAGGIO DATI A INTERROGAZIONE FOGLI V. PARTENZE - FNLS53R -
002900010221     D PARAM5          DS
003000010221     D  PA5FLG                 1      1    INZ('R')
003100010221     D  PA5FL2                 2      2    INZ
003200010221     D  PA5NFV                 3      5P 0 INZ
003300010221     D  PA5DIN                 6     10P 0 INZ
003400010221     D  PA5DFI                11     15P 0 INZ(31129999)
003500010221     D  PA5TFV                16     16    INZ('2')
003600010221     D  PA5FGS               242    244  0
003700010221     D***
003800010221     D WLBDAT          DS                  INZ
003900010221     D  G02DAT                 1      8  0
004000010221     D  G02INV                 9     16  0
004100010221     D  G02ERR                17     17
004200010221     D  G02TGI                18     22  0
004300010221     D* LINEE DI ARRIVO  PER FNFVA00F
004400010221     D                 DS
004500010221     D  FVALNA                 1      3  0
004600010221     D  FVAFFV                 4    243
004700010221     D  FVAFF2               244    450
004800010221     D  FWAFF3               451    690
004900010221     D  FWAFF4               691    900
005000010221     D  VUOTO                901    903
005100010221     D  FFV                    1    903
005200010221     D                                     DIM(301)
005300020808      *
005400020808      * Variabili
005500020808      *
005600020808     D KNFV            S                   LIKE(FGVNFV)
005700020808     D COD             S                   LIKE(TBLCOD)
005800020808     D KEY             S                   LIKE(TBLKEY)
005900020808
006000010221     c* inizializzo campi del video
006100010221     c                   clear                   vidnfd
006200010221     c                   clear                   vidlnp
006300010221     c                   clear                   vidnfv
006400010221     c                   clear                   vidlna
006500010221     c     for01         tag
006600010221     c                   exfmt     tcs2d01
006700010221     c                   clear                   vidmsg
006800010221     c     *inkc         cabeq     *on           fine
006900010221     c                   exsr      ctrd01
007000010221     c  nkf
007100010221     cor 29
007200010221     cor 28              goto      for01
007300010221     c* sottometto pgm batch
007400010221     c                   clear                   trtcs2ds
007500010221     c                   move      vidnfd        ds2nfd
007600010221     c                   z-add     simfel        ds2fgd
007700010221     c                   z-add     1             ds2npd
007800010221     c                   z-add     vidlnp        ds2fgs
007900010221     c                   z-add     1             ds2npg
008000010221     c                   z-add     vidnfv        ds2nfv
008100010221     c                   z-add     vidlna        ds2lna
008200010221     c                   movel     trtcs2ds      kpjbu
008300010221     C                   MOVEL     'TCS3'        KCOAZ
008400010221     c                   call      'BCH10'
008500010221     c                   parm                    kpjba
008600010221     c     fine          tag
008700020808     c                   eval      *inlr = *on
008800010221     c**********************************************************************
008900010221     c     ctrd01        begsr
009000010221     c                   setoff                                       284041
009100010221     c                   setoff                                       424329
009200010221     c* FOGLIO DI DEFLUENZA
009300010221     c* ricerca con ?
009400010221     C     '?'           SCAN      Vidnfd                                 32
009500010221    1C     *IN32         IFEQ      '1'
009600010221     C                   RESET                   PARAM5
009700010221     C                   Z-ADD     SIMFEL        PA5FGS
009800010221     C                   MOVEL     PARAM5        KPJBU
009900010221     C                   CALL      'FNLS53R'
010000010221     C                   PARM                    KPJBA
010100010221     C                   MOVEL     KPJBU         PARAM5
010200010221     C                   MOVEL     PA5NFV        Vidnfd
010300010221     C                   SETON                                        29
010400010221     C                   CLEAR                   vidmsg
010500010221     C                   GOTO      ENDCTR
010600010221    1C                   ENDIF
010700010221     C* Controllo valdità numerica
010800010221     C                   TESTN                   Vidnfd               31
010900010221    1C     *IN31         IFEQ      *ON
011000010221     C                   MOVE      Vidnfd        W001A             1
011100010221    2C     W001A         IFLT      '0'
011200010221     C                   SETOFF                                       31
011300010221    2C                   END
011400010221    1C                   END
011500010221     C*
011600010221    1C     *IN31         IFEQ      *OFF
011700010221     C                   MOVEL     ERR(4)        vidmsg
011800010221     C                   SETON                                        2840
011900010221     C                   GOTO      ENDCTR
012000010221    1C                   END
012100010221     C* Numero foglio viaggio esiste
012200010221     C                   MOVEL     Vidnfd        KNFV
012300020808     ***C     KNFV          CHAIN     FNFGV01L                           30
012400020808     C     K02FGV01      chain     FNFGV01L                           30
012500010221    1C     *IN30         IFEQ      '1'
012600010221     C     *IN30         OREQ      '0'
012700010221     C     FGVATB        ANDNE     *BLANKS
012800010221     C                   SETON                                        4028
012900010221     C                   MOVEL     ERR(1)        vidmsg
013000010221     C                   GOTO      ENDCTR
013100010221    1C                   ENDIF
013200010221     C* Il foglio viaggio deve essere di defluenza
013300010221     C                   MOVEL     'TV'          COD
013400010221     C                   MOVEL(P)  FGVTTR        KEY
013500010221     C     KTABE         CHAIN     TABEL                              83
013600010221     C     *IN83         IFEQ      *OFF
013700010221     C     TBLFLG        ANDEQ     ' '
013800010221     C                   MOVEL     TBLUNI        DSTV
013900010221     C                   ELSE
014000010221     C                   CLEAR                   DSTV
014100010221     C                   ENDIF
014200010221    1C     §TVDEF        IFNE      'S'
014300010221     C                   SETON                                        4028
014400010221     C                   MOVEL     ERR(3)        VIDMSG
014500010221     C                   GOTO      ENDCTR
014600010221    1C                   ENDIF
014700010221     C* Già  chiuso
014800010221    1C     FGVFCF        IFNE      ' '
014900010221     C                   SETON                                        4028
015000010221     C                   MOVEL     ERR(2)        VIDMSG
015100010221     C                   GOTO      ENDCTR
015200010221    1C                   ENDIF
015300010221     C* FOGLIO VIAGGIO PATENZA
015400010221     c     vidlnp        ifeq      *zeros
015500010221     c                   seton                                        4128
015600010221     c                   movel     err(5)        vidmsg
015700010221     C                   GOTO      ENDCTR
015800010221     c                   endif
015900010221     c     vidnfv        ifeq      *zeros
016000010221     c                   seton                                        4228
016100010221     c                   movel     err(6)        vidmsg
016200010221     C                   GOTO      ENDCTR
016300010221     c                   endif
016400010221     c     kfva          chain     fnfva01l                           31
016500010221     c     *in31         ifeq      *on
016600010221     c                   seton                                        4128
016700010221     c                   movel     err(7)        vidmsg
016800010221     C                   GOTO      ENDCTR
016900010221     c                   endif
017000010221     c     kfva          chain     fnfwa01l                           30
017100010221     c   30              clear                   fnfwa000
017200010221     c* LINEA DI ARRIVO
017300010221     c     vidlna        ifeq      *zeros
017400010221     c                   seton                                        4328
017500010221     c                   movel     err(8)        vidmsg
017600010221     C                   GOTO      ENDCTR
017700010221     c                   endif
017800010221     c                   move      vidlna        w003a             3
017900010221     c     w003a         lookup    ffv                                    30
018000010221     c     *in30         ifeq      *off
018100010221     c                   seton                                        4328
018200010221     c                   movel     err(9)        vidmsg
018300010221     C                   GOTO      ENDCTR
018400010221     c                   endif
018500010221     c* deve essere estera
018600010221     c     vidlna        chain     azorg01l                           30
018700020211     c  n30              movel     orgde3        og143
018800020211     c   30              clear                   og143
018900020211     c     §ogntw        ifne      'EEX'
019000020219     c     §ogntw        andne     'EUP'
019100020219     c     §ogntw        andne     'DPD'
019200020219     c     §ogntw        andne     'FED'
019300010221     c                   seton                                        4328
019400010221     c                   movel     err(10)       vidmsg
019500010221     C                   GOTO      ENDCTR
019600010221     c                   endif
019700010221     c     endctr        tag
019800010221     c                   endsr
019900010221     c**********************************************************************
020000010221     c     *inzsr        begsr
020100010221     c**********************************************************************
020200010221     c*
020300010221     C     *ENTRY        PLIST
020400010221     C                   PARM                    KPJBA
020500010221     C* Richiamo XPARUT
020600010221     C                   Z-ADD     1             CODUT
020700010221     C                   CALL      'X§PARUT'
020800010221     C                   PARM                    UT§DSE0F
020900010221     C                   MOVEL     REC80         CNCR80
021000010221     C                   MOVEL     RAGUT         RSUT
021100020808     c*
021200020808     c* Imposto il P.O. in gestione
021300020808     c                   z-add     SIMpou        VIDpog
021400020808     c                   clear                   DESpog
021500020808     c     SIMpou        chain     AZORG
021600020808    1c                   if             %found(AZORG01L)
021700020808     c                             and  ORGfva = *blanks
021800020808     c                             and (ORGfag = 'A'
021900020808     c                              or  ORGfag = 'F')
022000020808     c                   movel     ORGdes        VIDpog
022100020808    1c                   endif
022200010221     c*
022300010221     C     KTABE         KLIST
022400010221     C                   KFLD                    CODUT
022500010221     C                   KFLD                    COD
022600010221     C                   KFLD                    KEY
022700010221     c     kfva          klist
022800010221     c                   kfld                    vidlnp
022900010221     c                   kfld                    vidnfv
023000020808     c     K02FGV01      klist
023100020808     c                   kfld                    KNFV
023200020808     c                   kfld                    SIMfel
023300010221     c*
023400010221     c                   endsr
023500010221**
023600010221Foglio Errato o Annullato                                                     01
023700010221Foglio già chiuso                                                             02
023800010221Immettere un foglio viaggio di DEFLUENZA                                      19
023900010221Numero foglio viaggio presenta dei caratteri non validi                       04
024000010221Immettere la linea di partenza del foglio viaggio
024100010221Immettere il numero foglio
024200010221Foglio viaggio partenza errato
024300010221Immettere la linea di arrivo
024400010221Linea di arrivo non presente nel foglio viaggio partenza immesso
024500010221Immettere una linea di arrivo estera
