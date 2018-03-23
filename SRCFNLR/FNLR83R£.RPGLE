000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000530      * FNLR83R *-----------------------------------------------------*
000300000530      *                                                               *
000400000530      *             PULIZIE ARRIVI (CALCOLO DATE)       -  MAIN  -    *
000500000530      *                                                               *
000600000530      *---------------------------------------------------------------*
000700930805     FTABEL00F  IF   E           K DISK
000800000530     D*---------------------------------------------------------------*
000900000530     D* PARAMETRI LANCIO PULIZIA               - FNLR84C/FNLRE4R -    *
001000000530     D*---------------------------------------------------------------*
001100920226     D PARAM1          DS
001200070514      * Data pulizia log Tellus
001300070514     d  ComTlLg                1      8  0
001400961204     D* DATA PULIZIA BOLLE ARRIVI (CON LNA NON ESTERA)
001500941213     D  COMDPB                17     24  0
001600941215     D* DATA PULIZIA BOLLE TRANSITO
001700941213     D  COMDPT                25     32  0
001800941215     D* DATA PULIZIA F.VIAGGIO PARTENZA
001900941215     D  COMFGV                35     42  0
002000941215     D* DATA PULIZIA FOGLI VARI
002100941215     D  COMFVV                43     50  0
002200941215     D* DATA PULIZIA F.VIAGGIO PARTENZA IN ARRIVO
002300941215     D  COMFVA                51     58  0
002400941215     D* DATA PULIZIA STATISTICHE ARRIVI
002500040923     D****                                   60  670COMDPS
002600961204     D* DATA PULIZIA BOLLE ARRIVI (CON LNA ESTERA - BOLLE EXPORT)
002700961204     D  COMPBE                68     75  0
002800990126     D* DATA PULIZIA BOLLE ARRIVI CON RISERVA
002900990707     D  COMARR               191    198  0
003000990126     D* DATA PULIZIA BOLLE ARRIVI CON C.A.
003100990707     D  COMBCA               199    206  0
003200000530     D* DATA PULIZIA BOLLE ARRIVI - POSTE
003300000530     D  POSDPB               207    214  0
003400000530     D* DATA PULIZIA BOLLE ARRIVI - BOLLE EXPORT - POSTE
003500000530     D  POSPBE               215    222  0
003600000530     D* DATA PULIZIA BOLLE ARRIVI CON RISERVA - POSTE
003700000530     D  POSARR               223    230  0
003800000530     D* DATA PULIZIA BOLLE ARRIVI CON C.A. - POSTE
003900000530     D  POSBCA               231    238  0
004000040423     D* DATA PULIZIA BOLLE ARRIVI CON RECLAMI
004100040423     D  COMREC               239    246  0
004200000530     D*---------------------------------------------------------------*
004300000530     D* PARAMETRI CONTROLLO E INVERSIONE DATE                         *
004400000530     D*---------------------------------------------------------------*
004500920226     D WLBDAT          DS
004600941213     D  G02DAT                 1      8  0
004700941213     D  G02INV                 9     16  0
004800941213     D  G02ERR                17     17
004900941213     D  G02TGI                18     22  0
005000920226     D WGIDAT          DS
005100941213     D  GIODAT                 1      8  0
005200941213     D  GIOINV                 9     16  0
005300941213     D  GIOTGI                17     21  0
005400000530     D*---------------------------------------------------------------*
005500000530     D* PARAMETRI ESTERNI                                             *
005600000530     D*---------------------------------------------------------------*
005700930803     D KPJBA         E DS
005800000530     D*---------------------------------------------------------------*
005900000530     D* DS GIORNI DI PULIZIA                                          *
006000000530     D*---------------------------------------------------------------*
006100930803     D DS5A          E DS
006200000530     D DS5A2         E DS
006300000530     D DS5APT        E DS
006400100618     D dvpopuladd    E DS
006500070514      *---------------------------------------------------------------*
006600070514      * DS PARAMETRI UTENTE                                           *
006700070514      *---------------------------------------------------------------*
006800070514      * - Parametri x Controllo profilo utenti
006900070514     d TIBS34ds      e ds                  inz
007000070514      * - Ds di riferimento al file esterno AZUTE00F
007100070514     d AZUTEds       e ds                  extname(AZUTE00F)
007200070514      * - Ds per dati organigramma
007300070514     d dDatiUte      e ds
007400000000     C*---------------------------------------------------------------*
007500930729     C*                    MAIN                                       *
007600930803     C* PROGRAMMA PRINCIPALE STAMPA E PULIZIA ARRIVI                  *
007700930729     C*---------------------------------------------------------------*
007800930729     C*
007900930729     C* DEFINIZIONE VARIABILI, KLIST E PLIST
008000930729     C                   EXSR      DEFVAR
008100920226     C*
008200000530     C* CARICAMENTO GG ARRETRAMENTO - 1
008300070514     c                   z-add     1             §KUT
008400930416     C                   MOVEL     '5A'          §COD
008500920226     C                   MOVEL     '1       '    §KEY
008600930803     C     KTAB          CHAIN     TABEL00F                           30
008700930804     C  N30              MOVEL     TBLUNI        DS5A
008800930804     C   30              MOVEL     *ZEROS        DS5A
008900940201      *
009000000530     C* CARICAMENTO GG ARRETRAMENTO - 2
009100000530     C                   MOVEL     '5A'          §COD
009200000530     C                   MOVEL     '2       '    §KEY
009300000530     C     KTAB          CHAIN     TABEL00F                           30
009400000530     C  N30              MOVEL     TBLUNI        DS5A2
009500000530     C   30              MOVEL     *ZEROS        DS5A2
009600000530      *
009700000530     C* CARICAMENTO GG ARRETRAMENTO - POSTE
009800000530     C                   MOVEL     '5A'          §COD
009900000530     C                   MOVEL     'PT      '    §KEY
010000000530     C     KTAB          CHAIN     TABEL00F                           30
010100000530     C  N30              MOVEL     TBLUNI        DS5APT
010200000530     C   30              MOVEL     *ZEROS        DS5APT
010300970623     C*
010400920226     C* DATA PULIZIA = DATA GIORNO - GIORNI PULIZIA
010500000530     C* CON XSRDA8 TRASFORMO LA DATA ODIERNA IN NR.GIORNI (G02TGI)
010600050829     C                   Z-ADD     O61DRF        G02INV
010700050829     C                   MOVEL     '3'           G02ERR
010800941213     C                   CALL      'XSRDA8'
010900920226     C                   PARM                    WLBDAT
011000920226     C*
011100980722     C* BOLLE ARRIVI ITALIA E IMPORT
011200930416     C     G02TGI        SUB       §5AARB        GIOTGI
011300100618     c                   sub       §vpogg        GIOTGI
011400920226     C*
011500941213     C                   CALL      'XSRGI8'
011600920226     C                   PARM                    WGIDAT
011700920226     C*
011800920306     C                   Z-ADD     GIOINV        COMDPB
011900000530     C*
012000980722     C* BOLLE CON RISERVA
012100980722     C     G02TGI        SUB       §5AARR        GIOTGI
012200100618     c                   sub       §vpogg        GIOTGI
012300980722     C*
012400980722     C                   CALL      'XSRGI8'
012500980722     C                   PARM                    WGIDAT
012600980722     C*
012700980722     C                   Z-ADD     GIOINV        COMARR
012800000530     C*
012900990126     C* BOLLE CON C.A.
013000990126     C     G02TGI        SUB       §5ABCA        GIOTGI
013100990126     C*
013200990126     C                   CALL      'XSRGI8'
013300990126     C                   PARM                    WGIDAT
013400990126     C*
013500990126     C                   Z-ADD     GIOINV        COMBCA
013600000530     C*
013700980722     C* BOLLE ARRIVI (PER LE BOLLE EXPORT)
013800970718     C     G02TGI        SUB       §5AARE        GIOTGI
013900100618     c                   sub       §vpogg        GIOTGI
014000961204     C*
014100961204     C                   CALL      'XSRGI8'
014200961204     C                   PARM                    WGIDAT
014300961204     C*
014400961204     C                   Z-ADD     GIOINV        COMPBE
014500000530     C*
014600000530     C* BOLLE ARRIVI ITALIA E IMPORT - POSTE
014700000530     C     G02TGI        SUB       §5APT5        GIOTGI
014800000530     C*
014900000530     C                   CALL      'XSRGI8'
015000000530     C                   PARM                    WGIDAT
015100000530     C*
015200000530     C                   Z-ADD     GIOINV        POSDPB
015300000530     C*
015400000530     C* BOLLE CON RISERVA - POSTE
015500000530     C     G02TGI        SUB       §5APT7        GIOTGI
015600000530     C*
015700000530     C                   CALL      'XSRGI8'
015800000530     C                   PARM                    WGIDAT
015900000530     C*
016000000530     C                   Z-ADD     GIOINV        POSARR
016100000530     C*
016200000530     C* BOLLE CON C.A. - POSTE
016300000530     C     G02TGI        SUB       §5APT8        GIOTGI
016400000530     C*
016500000530     C                   CALL      'XSRGI8'
016600000530     C                   PARM                    WGIDAT
016700000530     C*
016800000530     C                   Z-ADD     GIOINV        POSBCA
016900000530     C*
017000000530     C* BOLLE ARRIVI (PER LE BOLLE EXPORT) - POSTE
017100000530     C     G02TGI        SUB       §5APT6        GIOTGI
017200000530     C*
017300000530     C                   CALL      'XSRGI8'
017400000530     C                   PARM                    WGIDAT
017500000530     C*
017600000530     C                   Z-ADD     GIOINV        POSPBE
017700920226     C*
017800980722     C* PULIZIA TRANSITI
017900930416     C     G02TGI        SUB       §5ABTP        GIOTGI
018000100618     c                   sub       §vpogg        GIOTGI
018100920226     C*
018200941213     C                   CALL      'XSRGI8'
018300920226     C                   PARM                    WGIDAT
018400920226     C*
018500920306     C                   Z-ADD     GIOINV        COMDPT
018600921111     C*
018700980722     C* FOGLI VIAGGIO PARTENZA
018800941215     C     G02TGI        SUB       §5AFGV        GIOTGI
018900921111     C*
019000941213     C                   CALL      'XSRGI8'
019100921111     C                   PARM                    WGIDAT
019200921111     C*
019300941215     C                   Z-ADD     GIOINV        COMFGV
019400941215     C*
019500980722     C* PULIZIA FOGLI VARI
019600941215     C     G02TGI        SUB       §5AFVV        GIOTGI
019700941215     C*
019800941215     C                   CALL      'XSRGI8'
019900941215     C                   PARM                    WGIDAT
020000941215     C*
020100941215     C                   Z-ADD     GIOINV        COMFVV
020200941215     C*
020300980722     C* FOGLI VIAGGIO PARTENZA IN ARRIVO
020400941215     C     G02TGI        SUB       §5AFVA        GIOTGI
020500941215     C*
020600941215     C                   CALL      'XSRGI8'
020700941215     C                   PARM                    WGIDAT
020800941215     C*
020900941215     C                   Z-ADD     GIOINV        COMFVA
021000040423     C*
021100040423     C* BOLLE CON RECLAMI
021200040423     C     G02TGI        SUB       §5AGPR        GIOTGI
021300100618     c                   sub       §vpogg        GIOTGI
021400040423     C*
021500040423     C                   CALL      'XSRGI8'
021600040423     C                   PARM                    WGIDAT
021700040423     C*
021800040423     C                   Z-ADD     GIOINV        COMREC
021900070514      *
022000070514      * Log di Tellus
022100070514     c                   clear                   wGiDat
022200070514     c                   eval      GioTGI  = G02tgi - §5ATlLg
022300070514     c                   call      'XSRGI8'
022400070514     c                   parm                    wGiDat
022500070514     c                   eval      ComTlLg = GioInv
022600070514      *_______________________________________________________________
022700000530      *
022800920327     C* LANCIO PULIZIA
022900920423     C                   MOVEL     PARAM1        KPJBU
023000050829     C                   CALL      'FNLR84C'
023100000530     C                   PARM                    KPJBA
023200940202      *
023300070514      * FINE PGM
023400070514     c                   eval      *inLR = *on
023500930729     C*---------------------------------------------------------------*
023600930729     C*                    DEFVAR                                     *
023700930729     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
023800930729     C*---------------------------------------------------------------*
023900930729     C     DEFVAR        BEGSR
024000930729     C*
024100930729     C     *ENTRY        PLIST
024200930729     C                   PARM                    KPJBA
024300930729     C*
024400930803     C* TABEL00F
024500930729     C     KTAB          KLIST
024600930729     C                   KFLD                    §KUT
024700930729     C                   KFLD                    §COD
024800930729     C                   KFLD                    §KEY
024900930729     C*
025000930729     C     *LIKE         DEFINE    TBLKUT        §KUT
025100930729     C     *LIKE         DEFINE    TBLCOD        §COD
025200930729     C     *LIKE         DEFINE    TBLKEY        §KEY
025300070514      *
025400070514      * Reperimento dati job
025500070514     c                   exsr      DatiJob
025600930802     C*
025700050829     C* RICHIAMO PGM PER DETERMINAZIONE DATA DI RIFERIMENTO DA USARE
025800050829     C* PER IL CALCOLO DATE DI PULIZIA
025900050829     C                   CALL      'FNLV61R'
026000050829     C                   PARM                    O61DRF            8 0
026100100618     C                   PARM      ' '           i61rep            1
026200100618     C                   PARM                    dvpopuladd
026300941213     C*
026400930729     C                   ENDSR
026500070514      *
026600070514      *---------------------------------------------------------------*
026700070514      *?Reperimento Dati del job (Utente/Operativi)                  ?*
026800070514      *---------------------------------------------------------------*
026900070514     c     DatiJob       BEGSR
027000070514      *
027100070514      * Reperisce dati job
027200070514     c     *dtaara       define    §azute        AZUTEds
027300070514     c     *dtaara       define    §datiute      dDATIUTE
027400070514      *
027500070514     c                   in(E)     *dtaara
027600070514     c                   IF        %ERROR or RSUT = *blanks
027700070514     c                   clear                   Tibs34Ds
027800070514     c                   call      'TIBS34R'
027900070514     c                   parm                    Tibs34Ds
028000070514     c                   in        *dtaara
028100070514     c                   ENDIF
028200070514      *
028300070514     c                   ENDSR
