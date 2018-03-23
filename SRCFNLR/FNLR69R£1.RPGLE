000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200950104     H* FNLR69R *-----------------------------------------------------*
000300930728     H*         STAMPA ELENCO SPEDIZIONI ARRIVATE     - FILTRO -      *
000400930726     H*---------------------------------------------------------------*
000500950104     FFNLR69D   CF   E             WORKSTN
000600920415     FTABEL00F  IF   E           K DISK
000700961126     FAZORG01L  IF   E           K DISK
000800961126     D*
000900961126     D* DEFINIZIONE SCHIERE
001000961126     D L1              S              3  0 DIM(30)                              FIL.GESTITE
001100961126     D MSG             S             78    DIM(12) CTDATA PERRCD(1)             MESSAGGI ERRORE
001200961126     D*
001300961126     D****
001400961126     D* RICHIAMO ALL'ELABORAZIONE BATCH               - FNLR61R -
001500961126     D****
001600900523     D PARAM           DS
001700961126     D* DATA CONSEGNA   DAL/AL  (GG/MM/AAAA)
001800961126     D  VIDDBD                 1      8  0
001900961126     D  VIDDBA                 9     16  0
002000961126     D* DATA CONSEGNA   DAL/AL  (AAAA/MM/GG)
002100961126     D  COMDBD                17     24  0
002200961126     D  COMDBA                25     32  0
002300961126     D* LINEA DI ARRIVO
002400961126     D  VIDFIL                33     35  0
002500961126     D* VIDFLG = '1' --> SUDDIVISIONE STAMPA PER DATA SPEDIZIONE
002600961126     D  VIDFLG                36     36
002700961126     D* DATA SPEDIZIONE DAL/AL  (AAAA/MM/GG)
002800961126     D  COMDSD                37     44  0
002900961126     D  COMDSA                45     52  0
003000961126     D* LINEA DI PARTENZA
003100961126     D  VIDLNP                53     55  0
003200961213     D* DATA SPEDIZIONE DAL/AL  (GG/MM/AAAA)
003300961213     D  VIDDSD                56     63  0
003400961213     D  VIDDSA                64     71  0
003500961126     D*
003600920505     D WLBDAT          DS
003700961126     D  G02DAT                 1      8  0
003800961126     D  G02INV                 9     16  0
003900961126     D  G02ERR                17     17
004000961126     D  G02TGI                18     22  0
004100930914     D WGIDAT          DS
004200961126     D  GIODAT                 1      8  0
004300961126     D  GIOINV                 9     16  0
004400961126     D  GIOTGI                17     21  0
004500961126     D*
004600961126     D* DS PER TRUL06R - CARICAMENTO £1
004700961126     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004800961126     D  LIN                    1     90  0
004900961126     D                                     DIM(30)                              SKI COMODO
005000961126     D*
005100900511     D KPJBA         E DS
005200930914     D DS5A          E DS
005300970718     D DS5A2         E DS
005400900523     D CNCR80        E DS
005500080121     D og143         E DS
005600900523     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005700080121     d XX              s              3  0
005800930726     C*---------------------------------------------------------------*
005900930726     C* INDICATORI USATI                                              *
006000930726     C*---------------------------------------------------------------*
006100080121     C* 09 - indicatore per selezionare linea di arrivo 999=tutte estere
006200080121     C*      se presente almeno una in £1
006300961126     C* 28 - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
006400930727     C* 30 - LETTURA FILE
006500961126     C* 35 - USATO PER LOKUP
006600930727     C* 40 - DATA CONSEGNA DAL ERRATA
006700930727     C* 41 - DATA CONSEGNA AL  ERRATA
006800930727     C* 42 - DATA SPEDIZIONE DAL ERRATA
006900930727     C* 43 - DATA CONSEGNA DAL SUPERIORE DATA AL
007000930727     C* 44 - DATA SPEDIZIONE AL  ERRATA
007100930727     C* 45 - LINEA DI ARRIVO ERRATA
007200930727     C* 46 - SE DATA DAL NON IMMESSA NON IMMETTERE DATA AL
007300930727     C* 47 - LA DATA AL DEVE ESSERE MAGGIORE DELLA DATA DAL
007400930728     C* 48 - DATA SPEDIZIONE NO MAGGIORE DELLA DATA CONSEGNA
007500930914     C* 49 - ELABORAZIONE MOMENTANEAMENTE NON EFFETTUABILE
007600930914     C* 50 - ELABORAZIONE NON EFFETTUABILE PER MANCANZA DI DATI
007700930727     C* 90 - GENERICO DI ERRORE
007800930726     C*---------------------------------------------------------------*
007900930726     C*
008000000000     C     *ENTRY        PLIST
008100000000     C                   PARM                    KPJBA
008200900523     C*
008300080121     C* CARICO TABELLA FILIALI GESTITE £1  e vedo se presenti estere
008400920415     C                   EXSR      CARTAB
008500920415     C*
008600900509     C     INIZIO        TAG
008700930917     C                   Z-ADD     0             VIDFIL
008800930917     C                   Z-ADD     0             VIDDSD
008900930917     C                   Z-ADD     0             VIDDSA
009000930917     C                   Z-ADD     0             VIDDBD
009100930917     C                   Z-ADD     0             VIDDBA
009200930916     C                   Z-ADD     0             COMDBA
009300930916     C                   Z-ADD     0             COMDSD
009400930916     C                   Z-ADD     0             COMDSA
009500961126     C                   Z-ADD     0             VIDLNP
009600920415     C                   MOVEL     '0'           VIDFLG
009700930726     C*
009800961127     C                   CLEAR                   COMDBD
009900961127     C                   CLEAR                   COMDBA
010000961127     C                   CLEAR                   COMDSD
010100961127     C                   CLEAR                   COMDSA
010200961127     C*
010300900509     C     FOR01         TAG
010400920415     C                   EXFMT     FR69D01
010500961126     C*
010600961126     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
010700961126     C                   CLEAR                   VIDMSG
010800961126     C                   SETOFF                                       284041
010900961126     C                   SETOFF                                       424344
011000961126     C                   SETOFF                                       45
011100930726     C*
011200930726     C* CMD3 - FINE LAVORO
011300950104     C   KC              GOTO      FINE
011400930726     C*
011500900509     C* CONTROLLI FORMATO1
011600900509     C                   EXSR      CONTR
011700950104     C  NKF
011800900522     COR 90              GOTO      FOR01
011900930726     C*
012000950104     C* CMD6 - ELABORAZIONE
012100900523     C                   MOVEL     PARAM         KPJBU
012200950104     C                   MOVEL     'LR61'        KCOAZ
012300900523     C                   CALL      'BCH10'
012400900523     C                   PARM                    KPJBA
012500930727     C*
012600000000     C     FINE          TAG
012700000000     C                   SETON                                        LR
012800930727     C*---------------------------------------------------------------*
012900930727     C*                    CARTAB                                     *
013000930727     C* ROUTINE DI CARICAMENTO TABELLA FILIALI IN GESTIONE     -L1-   *
013100930727     C*---------------------------------------------------------------*
013200920415     C     CARTAB        BEGSR
013300961126     C                   CLEAR                   DSUL06
013400961126     C                   MOVE      '£1'          D06COD
013500961126     C                   MOVEL     SIMFEL        D06KEY
013600961126     C                   MOVEL     DSUL06        KPJBU
013700961126     C                   CALL      'TRUL06R'
013800961126     C                   PARM                    KPJBA
013900961126     C                   MOVEL     KPJBU         DSUL06
014000961126     C                   MOVEA     LIN           L1
014100080121     c
014200080121     c                   z-add     1             xx
014300080121     c* se in l1 presente almeno una  filiale estera accendo lo 09
014400080121     c                   dow       l1(xx)>0
014500080121     c     l1(xx)        chain     azorg01l
014600080121     c                   if        %found(azorg01l)
014700080121     c                   movel     orgde3        og143
014800080121     c                   if        §ogntw='EEX' or §ogntw='DPD' or
014900080121     c                             §ogntw='FED'
015000080121     c                   seton                                        09
015100080121     c                   z-add     29            xx
015200080121     c                   endif
015300080121     c                   endif
015400080121     c
015500080121     c                   add       1             xx
015600080121     c                   enddo
015700080121     c
015800920415     C                   ENDSR
015900930727     C*---------------------------------------------------------------*
016000930727     C*                    CONTR                                      *
016100930727     C* ROUTINE DI CONTROLLO PRIMO FORMATO VIDEO                      *
016200930727     C*---------------------------------------------------------------*
016300900509     C     CONTR         BEGSR
016400920415     C                   SETOFF                                       90
016500920415     C*
016600961126     C****  LINEA DI ARRIVO  ****
016700920415     C* CONTROLLO VALIDITA' FILIALE IMMESSA
016800961126    1C     VIDFIL        IFGT      0
016900080121     c
017000961126    2C     *IN09         IFEQ      *OFF
017100961126     C     *IN09         OREQ      *ON                                          *MI-SUD
017200961126     C     VIDFIL        ANDNE     999
017300961126     C*
017400920415     C     VIDFIL        LOOKUP    L1                                     35
017500080121     c* chain in azorg per avere il network
017600080121     c     vidfil        chain     azorg01l
017700080121    3c                   if        not %found(azorg01l)  or not *in35
017800080121    3C***  *IN35         IFEQ      *OFF
017900961126     C                   MOVEL     MSG(1)        VIDMSG
018000961126     C                   SETON                                        402890
018100961126     C                   GOTO      ENDCTR
018200961126    3C                   ENDIF
018300080121     c
018400080121     c                   movel     orgde3        og143
018500961126    2C                   ENDIF
018600080121     c
018700961126    1C                   ENDIF
018800920415     C*
018900961126     C****  IMMETTERE O LA DATA CONSEGNA O LA DATA SPEDIZIONE  ****
019000961126    1C     VIDDBD        IFEQ      0
019100961126     C     VIDDSD        ANDEQ     0
019200961126     C                   MOVEL     MSG(2)        VIDMSG
019300961126     C                   SETON                                        412890
019400961126     C                   GOTO      ENDCTR
019500961126    1C                   ENDIF
019600961126     C*
019700961126     C****  DATA CONSEGNA  DAL  ****
019800961126    1C     VIDDBD        IFGT      0
019900920505     C                   MOVE      VIDDBD        G02DAT
020000920505     C                   MOVEL     *BLANK        G02ERR
020100961126     C                   CALL      'XSRDA8'
020200920505     C                   PARM                    WLBDAT
020300961126    2C     G02ERR        IFEQ      '1'
020400961126     C                   MOVEL     MSG(3)        VIDMSG
020500961126     C                   SETON                                        412890
020600920505     C                   GOTO      ENDCTR
020700961126    2C                   ENDIF
020800961126     C                   Z-ADD     G02DAT        VIDDBD
020900961126     C                   Z-ADD     G02INV        COMDBD
021000930727     C*
021100961126     C****  DATA CONSEGNA   AL  ****
021200961126    2C     VIDDBA        IFGT      0
021300920505     C                   MOVE      VIDDBA        G02DAT
021400920505     C                   MOVEL     *BLANK        G02ERR
021500961126     C                   CALL      'XSRDA8'
021600920505     C                   PARM                    WLBDAT
021700961126    3C     G02ERR        IFEQ      '1'
021800961126     C                   MOVEL     MSG(4)        VIDMSG
021900961126     C                   SETON                                        422890
022000920505     C                   GOTO      ENDCTR
022100961126    3C                   ENDIF
022200961126     C                   Z-ADD     G02DAT        VIDDBA
022300961126     C                   Z-ADD     G02INV        COMDBA
022400930726     C*
022500900523     C* DATA DAL NO MAGGIORE DATA AL
022600961126    3C     COMDBD        IFGT      COMDBA
022700961126     C                   MOVEL     MSG(5)        VIDMSG
022800961127     C                   SETON                                        422890
022900900523     C                   GOTO      ENDCTR
023000961126    3C                   ENDIF
023100961126    2C                   ENDIF
023200961126     C*
023300961126   X1C                   ELSE
023400961126     C* DATA CONSEGNA DAL NON IMMESSA: NON IMMETTERE NEMMENO DATA AL
023500961126    2C     VIDDBA        IFGT      0
023600961126     C                   MOVEL     MSG(3)        VIDMSG
023700961126     C                   SETON                                        412890
023800961126     C                   GOTO      ENDCTR
023900961126    2C                   ENDIF
024000961127    1C                   ENDIF
024100930914     C*
024200930914     C*
024300961126     C****  DATA SPEDIZIONE  DAL  ****
024400961126    1C     VIDDSD        IFGT      0
024500930727     C                   MOVE      VIDDSD        G02DAT
024600930727     C                   MOVEL     *BLANK        G02ERR
024700961126     C                   CALL      'XSRDA8'
024800930727     C                   PARM                    WLBDAT
024900961126    2C     G02ERR        IFEQ      '1'
025000961126     C                   MOVEL     MSG(8)        VIDMSG
025100961126     C                   SETON                                        432890
025200930727     C                   GOTO      ENDCTR
025300961126    2C                   ENDIF
025400961126     C                   Z-ADD     G02DAT        VIDDSD
025500961126     C                   Z-ADD     G02INV        COMDSD
025600930727     C*
025700961126     C****  DATA SPEDIZIONE   AL  ****
025800961126    2C     VIDDSA        IFGT      0
025900930727     C                   MOVE      VIDDSA        G02DAT
026000930727     C                   MOVEL     *BLANK        G02ERR
026100961126     C                   CALL      'XSRDA8'
026200930727     C                   PARM                    WLBDAT
026300961126    3C     G02ERR        IFEQ      '1'
026400961126     C                   MOVEL     MSG(9)        VIDMSG
026500961126     C                   SETON                                        442890
026600930727     C                   GOTO      ENDCTR
026700961126    3C                   ENDIF
026800961126     C                   Z-ADD     G02DAT        VIDDSA
026900961126     C                   Z-ADD     G02INV        COMDSA
027000930727     C*
027100961126   X2C                   ELSE
027200961126     C* SE NON IMMESSA DATA AL --> IMPOSTO DATA AL UGUALE A DATA DAL
027300930727     C                   Z-ADD     COMDSD        COMDSA
027400961126    2C                   ENDIF
027500930727     C*
027600930727     C* DATA AL NON MINORE DATA DAL
027700961126    2C     COMDSD        IFGT      COMDSA
027800961126     C                   MOVEL     MSG(10)       VIDMSG
027900961127     C                   SETON                                        442890
028000930727     C                   GOTO      ENDCTR
028100961126    2C                   ENDIF
028200930728     C*
028300930728     C* DATA SPEDIZIONE NON MAGGIORE DELLA DATA CONSEGNA
028400961127    2C     COMDBD        IFGT      0
028500961127     C     COMDSD        ANDGT     COMDBD
028600961126     C                   MOVEL     MSG(11)       VIDMSG
028700961126     C                   SETON                                        432890
028800930728     C                   GOTO      ENDCTR
028900961126    2C                   ENDIF
029000930727     C*
029100961126   X1C                   ELSE
029200961126     C* DATA SPEDIZIONE DAL NON IMMESSA: NON IMMETTERE NEMMENO DATA AL
029300961126    2C     VIDDSA        IFGT      *ZEROS
029400961126     C                   MOVEL     MSG(8)        VIDMSG
029500961127     C                   SETON                                        432890
029600930727     C                   GOTO      ENDCTR
029700961126    2C                   ENDIF
029800961126    1C                   ENDIF
029900930727     C*
030000961212     C****  REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA BOLLE  ****
030100961212    1C     VIDDBD        IFGT      0
030200961212     C     VIDDSD        ORGT      0
030300080121     C***                MOVEL     'FNLR84R'     PGM
030400080121     C***                MOVEL     *BLANKS       DAT
030500080121     C***                MOVEL     *BLANKS       ERR
030600961212     C*
030700080121     C***                CALL      'TRUL49C'
030800080121     C***                PARM                    PGM              10
030900080121     C***                PARM                    DAT               8
031000080121     C***                PARM                    ERR               1
031100080121    2C***  ERR           IFEQ      '1'
031200080121     C***                MOVEL     MSG(6)        VIDMSG
031300080121     C***                SETON                                        412890
031400080121     C***                GOTO      ENDCTR
031500080121   X2C***                ELSE
031600080121     c***
031700080121     C* PER IL CALCOLO DATE DI PULIZIA
031800080121     C                   CALL      'FNLV61R'
031900080121     C                   PARM                    O61DRF            8 0
032000080121     C*
032100961212     C*
032200961212     C* PROGRAMMA IN USO: IMPOSSIBILE REPERIRE LA DATA
032300961212     C*
032400961212     C* SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
032500961212     C* PULIZIA E TROVO LA DATA AL DI SOTTO DELLA QUALE NON SI POSSONO
032600961212     C* LANCIARE ELABORAZIONI PER MANCANZA DI DATI
032700961212     C                   CLEAR                   G02INV
032800080121     C****               MOVE      DAT           G02INV
032900080121     C                   MOVE      o61drf        G02INV
033000961212     C*
033100961212     C* SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
033200961212    3C     G02INV        IFEQ      *ZEROS
033300961212     C                   Z-ADD     *ZEROS        GIOINV
033400961212   X3C                   ELSE
033500961212     C*
033600961212     C                   Z-ADD     *ZEROS        G02DAT
033700961212     C                   Z-ADD     *ZEROS        G02TGI
033800961212     C                   MOVEL     '3'           G02ERR
033900961212     C                   CALL      'XSRDA8'
034000961212     C                   PARM                    WLBDAT
034100961212     C*
034200961212     C                   MOVEL     '5A'          COD
034300961212     C                   MOVEL(P)  '1'           KEY
034400961212     C     KTAB2         CHAIN     TABEL00F                           30
034500961212    4C     *IN30         IFEQ      *ON
034600961212     C                   MOVEL     *BLANKS       DS5A
034700961212   X4C                   ELSE
034800961212     C                   MOVEL     TBLUNI        DS5A
034900961212    4C                   ENDIF
035000961212     C*
035100961212     C                   Z-ADD     §5AARB        WGPUL
035200961212     C*
035300961212     C* SE RICHIESTA UNA LINEA ARRIVO ESTERA OPPURE TUTTE LE ESTERE
035400961212     C*   PRENDO I GIORNI DI PULIZIA RELATIVI ALLE BOLLE ARRIVI EXPORT
035500080121    4c                   if        vidfil>0 and (vidfil=999 or §ogntw='FED'
035600080121     c                             or §ogntw='EEX' or §ogntw='DPD')
035700080121    4C**   VIDFIL        IFGE      300
035800080121     C**   VIDFIL        OREQ      999
035900970718     C                   MOVEL     '5A'          COD
036000970718     C                   MOVEL(P)  '2'           KEY
036100961212     C     KTAB2         CHAIN     TABEL00F                           30
036200961212    5C     *IN30         IFEQ      *ON
036300970718     C                   CLEAR                   DS5A2
036400961212   X5C                   ELSE
036500970718     C                   MOVEL     TBLUNI        DS5A2
036600961212    5C                   ENDIF
036700961212     C*
036800970718     C                   Z-ADD     §5AARE        WGPUL
036900080121     C*******            ADD       §5AAES        WGPUL
037000961212    4C                   ENDIF
037100961212     C*
037200961212     C* SOTTRAGGO I GIORNI DI PULIZIA
037300961212     C                   SUB       WGPUL         G02TGI
037400961212     C*
037500961212     C                   Z-ADD     *ZEROS        GIODAT
037600961212     C                   Z-ADD     *ZEROS        GIOINV
037700961212     C                   Z-ADD     G02TGI        GIOTGI
037800961212     C*
037900961212     C                   CALL      'XSRGI8'
038000961212     C                   PARM                    WGIDAT
038100961212    3C                   ENDIF
038200961212     C*
038300961212     C* SE C'E' LA DATA CONSEGNA LA PRENDO ALTRIMENTI PRENDO DATA SPED.
038400961212    3C     COMDBD        IFGT      0
038500961212     C                   Z-ADD     COMDBD        WDAT
038600961212   X3C                   ELSE
038700961212     C                   Z-ADD     COMDSD        WDAT
038800961212    3C                   ENDIF
038900961212     C*
039000961212     C* SE LA DATA IMMESSA A VIDEO E' MINORE DI QUELLA CALCOLATA EMETTO
039100961212     C*   MESSAGGIO FORZABILE
039200961212    3C     WDAT          IFLE      GIOINV
039300961212     C     WDAT          ANDNE     SAWDAT
039400961212     C                   MOVEL     MSG(7)        VIDMSG
039500961212     C                   SETON                                        412890
039600961212     C                   MOVEL     WDAT          SAWDAT
039700961212     C                   GOTO      ENDCTR
039800961212    3C                   ENDIF
039900080121    2C***                ENDIF
040000961212    1C                   ENDIF
040100961212     C*
040200961126     C****  LINEA DI PARTENZA  ****
040300961126    1C     VIDLNP        IFGT      0
040400961126     C     VIDLNP        CHAIN     AZORG01L                           31
040500961126     C     *IN31         IFEQ      *ON
040600961126     C     ORGFVA        ORNE      ' '
040700961126     C                   MOVEL     MSG(12)       VIDMSG
040800961126     C                   SETON                                        452890
040900961126     C                   GOTO      ENDCTR
041000961126     C                   ENDIF
041100961126    1C                   ENDIF
041200961126     C*
041300900509     C     ENDCTR        ENDSR
041400961126     C*---------------------------------------------------------------*
041500961126     C*                    INZSR                                      *
041600961126     C* SUBROUTINE INIZIALE                                           *
041700961126     C*---------------------------------------------------------------*
041800950104     C     *INZSR        BEGSR
041900950104     C                   Z-ADD     1             CODUT
042000950104     C*
042100950104     C                   CALL      'X§PARUT'
042200950104     C                   PARM                    UT§DSE
042300950104     C                   MOVEL     RAGUT         RSUT             20
042400950104     C                   MOVEL     REC80         CNCR80
042500950104     C*
042600950104     C* TABEL00F
042700950104     C     KTAB2         KLIST
042800950104     C                   KFLD                    CODUT
042900961126     C                   KFLD                    COD
043000961126     C                   KFLD                    KEY
043100950104     C*
043200961126     C* DEFINIZIONE CAMPI
043300961126     C     *LIKE         DEFINE    TBLCOD        COD
043400961126     C     *LIKE         DEFINE    TBLKEY        KEY
043500961212     C     *LIKE         DEFINE    COMDBD        WDAT
043600961212     C     *LIKE         DEFINE    COMDBD        SAWDAT
043700961212     C     *LIKE         DEFINE    §5AARB        WGPUL
043800950104     C                   ENDSR
043900961126     C*---------------------------------------------------------------*
044000961126**  MSG
044100961126Linea di Arrivo errata o non in gestione                                      1
044200961126Immettere o la data consegna o la data spedizione                             2
044300961126Data Consegna DAL errata                                                      3
044400961126Data Consegna AL errata                                                       4
044500961127Data Consegna AL non può essere minore della Data Consegna DAL                5
044600961126Elaborazione momentaneamente non effettuabile                                 6
044700961212L'elaborazione può essere incompleta causa probabile mancanza dati:Enter forza7
044800961126Data Spedizione DAL errata                                                    8
044900961126Data Spedizione AL errata                                                     9
045000961127Data Spedizione AL non può essere minore della Data Spedizione DAL            10
045100961126La Data Spedizione deve essere minore della Data Consegna                     11
045200961126Linea di Partenza errata                                                      12
