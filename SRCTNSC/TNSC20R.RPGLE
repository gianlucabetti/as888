000100931008     H DECEDIT('0,') DATEDIT(*DMY.)
000200931008?     *--------------------------------------------------------------*
000300941102      * TNSC20R                                                      *
000400931008      *       *-----------------------------------------*            *
000500931008      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600931008      *       *-----------------------------------------*            *
000700931008      *                                                              *
000800931008?     *--------------------------------------------------------------*
000900020909      * 37 -Errore su data spedizione da selezione 2                 *
001000020909      * 38 -Errore su data spedizione a  selezione 2                 *
001100020909      * 39 -Errore su linea di partenza selezione 2                  *
001200960917      * 40 -Errore su linea di partenza                              *
001300960917      * 41 -Errore su numero spedizione                              *
001400960917      * 42 -Errore su numero riferimento mittente                    *
001500960917      * 43 -Errore su data spedizione da                             *
001600960917      * 44 -Errore su data spedizione a                              *
001700960917      * 45 -Errore su codice cliente (dati spedizione)               *
001800960917      * 46 -Errore su ragione sociale cliente (dati spedizione)      *
001900960917      * 47 -Errore su data pagamento da                              *
002000960917      * 48 -Errore su data pagamento a                               *
002100960917      * 49 -Errore su codice pagamento                               *
002200960917      * 50 -Errore su ragione sociale cliente pagamento              *
002300960917      * 28 -Errore generico                                          *
002400931008?     *--------------------------------------------------------------*
002500931008      * KC -FINE LAVORO                                              *
002600931008      * KG -INTERROGAZIONE BOLLE                                     *
002700931008      * KJ -STAMPA                                                   *
002800931008?     *--------------------------------------------------------------*
002900941102     FTNSC20D   CF   E             WORKSTN
003000960918     FTABEL00F  IF   E           K DISK
003100960917?     *--------------------------------------------------------------*
003200960917?     *  SCHIERE                                                     *
003300960917?     *--------------------------------------------------------------*
003400991230     D ERR             S             70    DIM(15) CTDATA PERRCD(1)
003500960918     D C4S             S              1  0 DIM(100)
003600991230     D CCV             S              3    DIM(200)
003700931008?     *--------------------------------------------------------------*
003800931008?     *  DS                                                          *
003900931008?     *--------------------------------------------------------------*
004000931008     D KPJBA         E DS
004100931008      *
004200960918     D DS4S          E DS
004300960918     D TNSC20        E DS                  EXTNAME(TNSC20DS)
004400931008      *
004500931008     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004600931008     D CNCR80        E DS
004700931008      *
004800941102     D WLBDA8          DS
004900941102     D  G02DAT                 1      8  0
005000941102     D  G02INV                 9     16  0
005100941102     D  G02ERR                17     17
005200941102     D  G02TGI                18     22  0
005300931008     D                SDS
005400931008     D  NOMPG                  1     10
005500931008      *---------------------------------------------------------------*
005600931008      *   C I C L O      P R I N C I P A L E                          *
005700931008      *---------------------------------------------------------------*
005800931008     C*  INIZIALIZZAZIONE VIDEATA
005900931008     C                   EXSR      INZVID
006000931008     C*
006100931008     C*  LOOP
006200931008     C     £FINE         DOUEQ     'S'
006300931008     C*
006400931008     C     TIPVID        CASEQ     '1'           GESVD1
006500931008     C                   END
006600931008     C*
006700931008     C                   END
006800931008     C*
006900931008     C                   SETON                                        LR
007000931008      *---------------------------------------------------------------*
007100931008      *  INZVID: INIZIALIZZAZIONE PARAMETRI VIDEO                     *
007200931008      *---------------------------------------------------------------*
007300931008     C     INZVID        BEGSR
007400931008     C*
007500020909     C                   SETOFF                                       373839
007600960917     C                   SETOFF                                       404142
007700960917     C                   SETOFF                                       434445
007800960917     C                   SETOFF                                       464748
007900960917     C                   SETOFF                                       495051
008000960917     C                   SETOFF                                       525354
008100991230     C                   SETOFF                                       555628
008200960917     C                   MOVEL     *BLANKS       $MSG
008300931008     C                   MOVEL     NOMPG         NOMPGM
008400931008     C                   Z-ADD     0             VSCL
008500931008     C                   Z-ADD     0             SPDDA
008600931008     C                   Z-ADD     0             SPADA
008700931008     C                   Z-ADD     0             VLNP
008800020909     C                   Z-ADD     0             SPDDA2
008900020909     C                   Z-ADD     0             SPADA2
009000020909     C                   Z-ADD     0             VLNP2
009100941102     C                   MOVE      WHHDAT        VANN
009200931008     C                   Z-ADD     0             VSER
009300931011     C                   Z-ADD     0             DSPE
009400931011     C                   Z-ADD     0             CODMP
009500931011     C                   Z-ADD     0             CODMS
009600931011     C                   Z-ADD     0             NRDOC
009700931011     C                   Z-ADD     0             NRMIT
009800931011     C                   Z-ADD     0             PGDDA
009900931011     C                   Z-ADD     0             PGADA
010000931011     C                   MOVEL     *BLANK        VTIP
010100931011     C                   MOVEL     *BLANK        RAGMP
010200931011     C                   MOVEL     *BLANK        RAGMS
010300931011     C                   MOVEL     *BLANK        TINT
010400991230     C                   MOVEL     *BLANK        SPDIV
010500991230     C                   MOVEL     *BLANK        PGDIV
010600960919     C                   Z-ADD     0             V1CSTA
010700960919     C                   Z-ADD     0             V1CCDI
010800960919     C                   MOVEL     *BLANK        V1CRSM
010900931011     C*
011000931011     C                   ENDSR
011100931011      *---------------------------------------------------------------*
011200931011      *  GESVD1: GESTIONE PRIMA VIDEATA RICHIESTA PARAMETRI           *
011300931011      *---------------------------------------------------------------*
011400931011     C     GESVD1        BEGSR
011500931011     C*
011600941102     C                   EXFMT     SC20D01
011700931011     C                   SETOFF                                       010203
011800960917     C                   SETOFF                                       042898
011900020909     C                   SETOFF                                       373839
012000960917     C                   SETOFF                                       404142
012100960917     C                   SETOFF                                       434445
012200960917     C                   SETOFF                                       464748
012300960917     C                   SETOFF                                       495051
012400960917     C                   SETOFF                                       525354
012500991230     C                   SETOFF                                       5556
012600931011     C*  Fine elaborazione
012700931011     C     *INKC         IFEQ      '1'
012800931011     C                   MOVEL     'S'           £FINE
012900931011     C                   GOTO      FINVD1
013000931011     C                   END
013100931011     C*  Interrogazione bolle
013200931011     C     *INKG         IFEQ      '1'
013300170124     C****               CALL      'FNLS04R'
013400170124     C****               PARM                    KPJBA
013401170124     C                   CALL      'FNLRU6R'
013402170124     C                   PARM                    KPJBA
013500931111     C                   GOTO      FINVD1
013600931011     C                   END
013700931011     C*  Controlli
013800931011     C                   EXSR      CNTVD1
013900960917     C   28
014000931012     COR 98              GOTO      FINVD1
014100931012     C*  Stampa
014200931012     C     *INKJ         IFEQ      '1'
014300931012     C                   EXSR      STAMPA
014400931013     C                   ELSE
014500931011     C*  Se non ci sono errori procedo a richiamare i pgm. di interrogazione
014600931012     C                   EXSR      CALLIN
014700931104     C                   EXSR      INZVID
014800931013     C                   END
014900931011     C*
015000931014     C                   MOVEL     *BLANK        VTIP
015100931011     C     FINVD1        ENDSR
015200931011      *---------------------------------------------------------------*
015300931011      *  CNTVD1: CONTROLLI PRIMA VIDEATA                              *
015400931011      *---------------------------------------------------------------*
015500931011     C     CNTVD1        BEGSR
015600931011     C*----------------------------------------------------*
015700931011     C*  Selezione x numero spedizione                     *
015800931011     C     VSCL          IFEQ      1
015900931011     C*----------------------------------------------------*
016000931012     C*  Stampa non permessa se seleziono x numero spedizione
016100931012     C     *INKJ         IFEQ      '1'
016200960917     C                   SETON                                        5428
016300960917     C                   MOVEL     ERR(11)       $MSG
016400931012     C                   GOTO      FINCT1
016500931012     C                   END
016600941102     C*  Anno di spedizione
016700941102     C                   MOVEL     VANN          WAA2              2 0
016800941102     C     WAA2          IFEQ      0
016900941102     C     VANN          IFGT      60
017000941102     C                   ADD       1900          VANN
017100941102     C                   ELSE
017200941102     C                   ADD       2000          VANN
017300941102     C                   END
017400941102     C                   END
017500931011     C*  Linea di partenza obbligatoria
017600931011     C     VLNP          IFEQ      0
017700960917     C                   SETON                                        4028
017800960917     C                   MOVEL     ERR(1)        $MSG
017900931011     C                   GOTO      FINCT1
018000931011     C                   END
018100931011     C*  Data di spedizione:
018200931011     C     DSPE          IFEQ      0
018300960917     C                   SETON                                        4128
018400960917     C                   MOVEL     ERR(2)        $MSG
018500931011     C                   GOTO      FINCT1
018600931011     C                   END
018700931011     C*----------------------------------------------------*
018800931011     C                   END
018900931011     C*----------------------------------------------------*
019000931011     C*  Selezione x dati pagamento                        *
019100931012     C     VSCL          IFEQ      4
019200931011     C*----------------------------------------------------*
019300931011     C*  Controllo data di pagamento da ....
019400931011     C     PGDDA         IFEQ      0
019500960917     C                   SETON                                        4728
019600960917     C                   MOVEL     ERR(3)        $MSG
019700931011     C                   GOTO      FINCT1
019800931011     C                   END
019900931011     C*
020000931011     C                   MOVE      PGDDA         G02DAT
020100931011     C                   MOVEL     *BLANK        G02ERR
020200941102     C                   CALL      'XSRDA8'
020300941102     C                   PARM                    WLBDA8
020400931011     C     G02ERR        IFEQ      '1'
020500960917     C                   SETON                                        4728
020600960917     C                   MOVEL     ERR(4)        $MSG
020700931011     C                   GOTO      FINCT1
020800931011     C                   ELSE
020900931011     C                   Z-ADD     G02INV        PDDAT
021000931011     C                   Z-ADD     G02TGI        TGIDA
021100941102     C                   Z-ADD     G02DAT        PGDDA
021200931011     C                   END
021300931011     C*
021400931011     C*  Controllo data di pagamento a ....
021500931011     C     PGADA         IFEQ      0
021600931011     C                   MOVE      PGDDA         PGADA
021700931011     C                   SETON                                        98
021800931011     C                   END
021900931011     C*
022000931011     C                   MOVE      PGADA         G02DAT
022100931011     C                   MOVEL     *BLANK        G02ERR
022200941102     C                   CALL      'XSRDA8'
022300941102     C                   PARM                    WLBDA8
022400931011     C     G02ERR        IFEQ      '1'
022500960917     C                   SETON                                        4828
022600960917     C                   MOVEL     ERR(4)        $MSG
022700931011     C                   GOTO      FINCT1
022800931011     C                   ELSE
022900931011     C                   Z-ADD     G02INV        PADAT
023000931011     C                   Z-ADD     G02TGI        TGIA
023100941102     C                   Z-ADD     G02DAT        PGADA
023200931011     C                   END
023300931011     C*
023400931011     C*  Controllo limite delle due date
023500931011     C     PDDAT         IFGT      PADAT
023600960917     C                   SETON                                        474828
023700960917     C                   MOVEL     ERR(5)        $MSG
023800931011     C                   GOTO      FINCT1
023900931011     C                   END
024000931011     C*
024100960918     C     TGIA          SUB       TGIDA         DIFDAT
024200931011     C     DIFDAT        IFGT      7
024300960917     C                   SETON                                        474828
024400960917     C                   MOVEL     ERR(9)        $MSG
024500931011     C                   GOTO      FINCT1
024600931011     C                   END
024700931011     C*
024800931011     C*  Controllo selezione codice ragione sociale
024900931011     C     CODMP         IFNE      0
025000931011     C     RAGMP         ANDNE     *BLANK
025100960917     C                   SETON                                        495028
025200960917     C                   MOVEL     ERR(6)        $MSG
025300931011     C                   GOTO      FINCT1
025400931011     C                   END
025500991230     C* Controllo la divisa se immessa
025600991230     C     PGDIV         IFNE      *BLANKS
025700991230     C                   Z-ADD     1             X
025800991230     C     PGDIV         LOOKUP    CCV(X)                                 32
025900991230     C     *IN32         IFEQ      '0'
026000991230     C                   SETON                                        5628
026100991230     C                   MOVEL     ERR(15)       $MSG
026200991230     C                   GOTO      FINCT1
026300991230     C                   END
026400991230     C                   END
026500931011     C*----------------------------------------------------*
026600931011     C                   END
026700931011     C*----------------------------------------------------*
026800931011     C*  Selezione x dati spedizione                       *
026900931011     C     VSCL          IFEQ      3
027000931011     C*----------------------------------------------------*
027100931011     C*  Controllo data di spedizione da....
027200931011     C     SPDDA         IFEQ      0
027300960917     C                   SETON                                        4328
027400960917     C                   MOVEL     ERR(10)       $MSG
027500931011     C                   GOTO      FINCT1
027600931011     C                   END
027700931011     C*
027800931011     C                   MOVE      SPDDA         G02DAT
027900931011     C                   MOVEL     *BLANK        G02ERR
028000941102     C                   CALL      'XSRDA8'
028100941102     C                   PARM                    WLBDA8
028200931011     C     G02ERR        IFEQ      '1'
028300960917     C                   SETON                                        4328
028400960917     C                   MOVEL     ERR(4)        $MSG
028500931011     C                   GOTO      FINCT1
028600931011     C                   ELSE
028700931011     C                   Z-ADD     G02INV        SDDAT
028800931011     C                   Z-ADD     G02TGI        TGIDA
028900941102     C                   Z-ADD     G02DAT        SPDDA
029000931011     C                   END
029100931011     C*
029200931011     C*  Controllo data di spedizione a ....
029300931011     C     SPADA         IFEQ      0
029400931011     C                   MOVE      SPDDA         SPADA
029500931011     C                   SETON                                        98
029600931011     C                   END
029700931011     C*
029800931011     C                   MOVE      SPADA         G02DAT
029900931011     C                   MOVEL     *BLANK        G02ERR
030000941102     C                   CALL      'XSRDA8'
030100941102     C                   PARM                    WLBDA8
030200931011     C     G02ERR        IFEQ      '1'
030300960917     C                   SETON                                        4428
030400960917     C                   MOVEL     ERR(4)        $MSG
030500931011     C                   GOTO      FINCT1
030600931011     C                   ELSE
030700931011     C                   Z-ADD     G02INV        SADAT
030800931011     C                   Z-ADD     G02TGI        TGIA
030900941102     C                   Z-ADD     G02DAT        SPADA
031000931011     C                   END
031100931011     C*
031200931011     C*  Controllo limite delle due date
031300931011     C     SDDAT         IFGT      SADAT
031400960917     C                   SETON                                        434428
031500960917     C                   MOVEL     ERR(5)        $MSG
031600931011     C                   GOTO      FINCT1
031700931011     C                   END
031800020909     C*
031900020909     C     TGIA          SUB       TGIDA         DIFDAT
032000020909     C     DIFDAT        IFGT      7
032100020909     C                   SETON                                        434428
032200020909     C                   MOVEL     ERR(9)        $MSG
032300020909     C                   GOTO      FINCT1
032400020909     C                   END
032500020909     C*
032600931011     C*
032700931011     C*  Controllo selezione codice ragione sociale
032800931011     C     CODMS         IFNE      0
032900931011     C     RAGMS         ANDNE     *BLANK
033000960917     C                   SETON                                        454628
033100960917     C                   MOVEL     ERR(6)        $MSG
033200931011     C                   GOTO      FINCT1
033300931011     C                   END
033400991230     C* Controllo la divisa se immessa
033500991230     C     SPDIV         IFNE      *BLANKS
033600991230     C                   Z-ADD     1             X
033700991230     C     SPDIV         LOOKUP    CCV(X)                                 32
033800991230     C     *IN32         IFEQ      '0'
033900991230     C                   SETON                                        5528
034000991230     C                   MOVEL     ERR(15)       $MSG
034100991230     C                   GOTO      FINCT1
034200991230     C                   END
034300991230     C                   END
034400931011     C*----------------------------------------------------*
034500931011     C                   END
034600931011     C*----------------------------------------------------*
034700931011     C*  Selezione x dati numero riferimento mittente      *
034800931012     C     VSCL          IFEQ      2
034900931011     C*----------------------------------------------------*
035000931011     C*  Controllo numero riferimento mittente
035100931011     C     NRMIT         IFEQ      0
035200960917     C                   SETON                                        4228
035300960917     C                   MOVEL     ERR(8)        $MSG
035400960917     C                   GOTO      FINCT1
035500931011     C                   END
035600020909     C*  Linea di partenza obbligatoria
035700020909     C     VLNP2         IFEQ      0
035800020909     C                   SETON                                        3928
035900020909     C                   MOVEL     ERR(1)        $MSG
036000020909     C                   GOTO      FINCT1
036100020909     C                   END
036200020909     C*  Controllo data di spedizione da....
036300020909     C     SPDDA2        IFEQ      0
036400020909     C                   SETON                                        3728
036500020909     C                   MOVEL     ERR(10)       $MSG
036600020909     C                   GOTO      FINCT1
036700020909     C                   END
036800020909     C*
036900020909     C                   MOVE      SPDDA2        G02DAT
037000020909     C                   MOVEL     *BLANK        G02ERR
037100020909     C                   CALL      'XSRDA8'
037200020909     C                   PARM                    WLBDA8
037300020909     C     G02ERR        IFEQ      '1'
037400020909     C                   SETON                                        3728
037500020909     C                   MOVEL     ERR(4)        $MSG
037600020909     C                   GOTO      FINCT1
037700020909     C                   ELSE
037800020909     C                   Z-ADD     G02INV        SDDAT
037900020909     C                   Z-ADD     G02TGI        TGIDA
038000020909     C                   Z-ADD     G02DAT        SPDDA2
038100020909     C                   END
038200020909     C*
038300020909     C*  Controllo data di spedizione a ....
038400020909     C     SPADA2        IFEQ      0
038500020909     C                   MOVE      SPDDA2        SPADA2
038600020909     C                   SETON                                        98
038700020909     C                   END
038800020909     C*
038900020909     C                   MOVE      SPADA2        G02DAT
039000020909     C                   MOVEL     *BLANK        G02ERR
039100020909     C                   CALL      'XSRDA8'
039200020909     C                   PARM                    WLBDA8
039300020909     C     G02ERR        IFEQ      '1'
039400020909     C                   SETON                                        3828
039500020909     C                   MOVEL     ERR(4)        $MSG
039600020909     C                   GOTO      FINCT1
039700020909     C                   ELSE
039800020909     C                   Z-ADD     G02INV        SADAT
039900020909     C                   Z-ADD     G02TGI        TGIA
040000020909     C                   Z-ADD     G02DAT        SPADA2
040100020909     C                   END
040200020909     C*
040300020909     C*  Controllo limite delle due date
040400020909     C     SDDAT         IFGT      SADAT
040500020909     C                   SETON                                        373828
040600020909     C                   MOVEL     ERR(5)        $MSG
040700020909     C                   GOTO      FINCT1
040800020909     C                   END
040900020909     C*
041000020909     C     TGIA          SUB       TGIDA         DIFDAT
041100020909     C     DIFDAT        IFGT      7
041200020909     C                   SETON                                        373828
041300020909     C                   MOVEL     ERR(9)        $MSG
041400020909     C                   GOTO      FINCT1
041500020909     C                   END
041600960917     C*----------------------------------------------------*
041700960917     C                   END
041800960917     C*----------------------------------------------------*
041900960917     C*  Selezione x stato/cliente                         *
042000960917     C     VSCL          IFEQ      5
042100960917     C*----------------------------------------------------*
042200960917     C*  Controllo se immesso stato
042300960918     C     V1CSTA        IFEQ      *ZEROS
042400960917     C                   SETON                                        5328
042500960917     C                   MOVEL     ERR(12)       $MSG
042600960917     C                   GOTO      FINCT1
042700960917     C                   END
042800960918     C*  Stato corretto
042900960918     C     V1CSTA        LOOKUP    C4S                                    32
043000960918     C     *IN32         IFEQ      '0'
043100960918     C                   SETON                                        5328
043200960918     C                   MOVEL     ERR(14)       $MSG
043300960918     C                   GOTO      FINCT1
043400960918     C                   END
043500960917     C*  Non possibile parzializzare per tipo C/Ass.
043600960918     C     VTIP          IFNE      *BLANKS
043700960917     C                   SETON                                        5428
043800960917     C                   MOVEL     ERR(13)       $MSG
043900960917     C                   GOTO      FINCT1
044000960917     C                   END
044100960917     C*  Controllo selezione codice ragione sociale
044200960917     C     V1CCDI        IFNE      0
044300960917     C     V1CRSM        ANDNE     *BLANK
044400960917     C                   SETON                                        515228
044500960917     C                   MOVEL     ERR(6)        $MSG
044600960917     C                   GOTO      FINCT1
044700960917     C                   END
044800960917     C*----------------------------------------------------*
044900960917     C                   END
045000960917     C*----------------------------------------------------*
045100931011     C     FINCT1        ENDSR
045200931012      *---------------------------------------------------------------*
045300931012      *  STAMPA: RICHIAMO I VARI PROGRAMMI DI STAMPA                  *
045400931012      *---------------------------------------------------------------*
045500931012     C     STAMPA        BEGSR
045600931012     C*
045700941102     C                   CLEAR                   TNSC20
045800941102     C                   MOVEL     VSCL          D20INT
045900941102     C                   MOVEL     VTIP          D20TCR
046000931012     C                   MOVEL     *BLANK        KPJBU
046100931012     C*----------------------------------------------------*
046200931012     C     VSCL          IFEQ      2
046300941102     C                   MOVE      NRMIT         D20NRM
046400020910     C                   EVAL      D20LNP = VLNP2
046500020910     C                   EVAL      D20SDD = SDDAT
046600020910     C                   EVAL      D20SDA = SADAT
046700941102     C                   MOVEL     TNSC20        KPJBU
046800941102     C                   CALL      'TNSC22R2'
046900931012     C                   PARM                    KPJBA
047000931012     C                   END
047100931012     C*----------------------------------------------------*
047200931012     C     VSCL          IFEQ      3
047300941102     C                   MOVE      SDDAT         D20SDD
047400941102     C                   MOVE      SADAT         D20SDA
047500941102     C                   MOVE      CODMS         D20SCD
047600941102     C                   MOVE      RAGMS         D20SRA
047700991230     C                   MOVE      SPDIV         D20SDV
047800941102     C                   MOVEL     TNSC20        KPJBU
047900941102     C                   CALL      'TNSC22R3'
048000931012     C                   PARM                    KPJBA
048100931012     C                   END
048200931012     C*----------------------------------------------------*
048300931012     C     VSCL          IFEQ      4
048400941102     C                   MOVE      PDDAT         D20PDD
048500941102     C                   MOVE      PADAT         D20PDA
048600941102     C                   MOVE      TINT          D20PTN
048700941102     C                   MOVE      NRDOC         D20PND
048800941102     C                   MOVE      CODMP         D20PCD
048900941102     C                   MOVE      RAGMP         D20PRA
049000991230     C                   MOVE      PGDIV         D20PDV
049100941102     C                   MOVEL     TNSC20        KPJBU
049200941102     C                   CALL      'TNSC22R4'
049300931012     C                   PARM                    KPJBA
049400931012     C                   END
049500960917     C*----------------------------------------------------*
049600960917     C     VSCL          IFEQ      5
049700960917     C                   MOVE      V1CSTA        D20STA
049800960917     C                   MOVE      V1CCDI        D20TCD
049900960917     C                   MOVE      V1CRSM        D20TRA
050000960917     C                   MOVEL     TNSC20        KPJBU
050100960917     C                   CALL      'TNSC22R5'
050200960917     C                   PARM                    KPJBA
050300960917     C                   END
050400931012     C*----------------------------------------------------*
050500941102     C     D20CMD        IFEQ      3
050600931012     C                   MOVE      'S'           £FINE
050700931012     C                   END
050800931012     C*----------------------------------------------------*
050900931012     C                   ENDSR
051000931011      *---------------------------------------------------------------*
051100931011      *  CALLIN: RICHIAMO I VARI PROGRAMMI DI INTERROGAZIONE          *
051200931011      *---------------------------------------------------------------*
051300931011     C     CALLIN        BEGSR
051400931011     C*
051500941102     C                   CLEAR                   TNSC20
051600941102     C                   MOVEL     VSCL          D20INT
051700941102     C                   MOVEL     VTIP          D20TCR
051800931011     C                   MOVEL     *BLANK        KPJBU
051900931011     C*----------------------------------------------------*
052000931011     C     VSCL          IFEQ      1
052100941102     C                   MOVE      VANN          D20ANN
052200941102     C                   MOVE      VLNP          D20LNP
052300941102     C                   MOVE      VSER          D20NSR
052400941102     C                   MOVE      DSPE          D20NSP
052500941102     C                   MOVEL     TNSC20        KPJBU
052600941102     C                   CALL      'TNSC23R'
052700931011     C                   PARM                    KPJBA
052800931011     C                   END
052900931012     C*----------------------------------------------------*
053000931012     C     VSCL          IFEQ      2
053100941102     C                   MOVE      NRMIT         D20NRM
053200020909     C                   EVAL      D20LNP = VLNP2
053300020909     C                   EVAL      D20SDD = SDDAT
053400020909     C                   EVAL      D20SDA = SADAT
053500941102     C                   MOVEL     TNSC20        KPJBU
053600941102     C                   CALL      'TNSC21R2'
053700931012     C                   PARM                    KPJBA
053800931012     C                   END
053900931011     C*----------------------------------------------------*
054000931011     C     VSCL          IFEQ      3
054100941102     C                   MOVE      SDDAT         D20SDD
054200941102     C                   MOVE      SADAT         D20SDA
054300941102     C                   MOVE      CODMS         D20SCD
054400941102     C                   MOVE      RAGMS         D20SRA
054500991231     C                   MOVE      SPDIV         D20SDV
054600941102     C                   MOVEL     TNSC20        KPJBU
054700941102     C                   CALL      'TNSC21R3'
054800931011     C                   PARM                    KPJBA
054900931011     C                   END
055000931012     C*----------------------------------------------------*
055100931012     C     VSCL          IFEQ      4
055200931012     C*
055300941102     C                   MOVE      PDDAT         D20PDD
055400941102     C                   MOVE      PADAT         D20PDA
055500941102     C                   MOVE      TINT          D20PTN
055600941102     C                   MOVE      NRDOC         D20PND
055700941102     C                   MOVE      CODMP         D20PCD
055800941102     C                   MOVE      RAGMP         D20PRA
055900991231     C                   MOVE      PGDIV         D20PDV
056000941102     C                   MOVEL     TNSC20        KPJBU
056100931012     C     NRDOC         IFEQ      0
056200941102     C                   CALL      'TNSC21R1'
056300931012     C                   PARM                    KPJBA
056400931012     C                   ELSE
056500941102     C                   CALL      'TNSC21R4'
056600931012     C                   PARM                    KPJBA
056700931012     C                   END
056800931012     C                   END
056900960917     C*----------------------------------------------------*
057000960917     C     VSCL          IFEQ      5
057100960917     C                   MOVE      V1CSTA        D20STA
057200960917     C                   MOVE      V1CCDI        D20TCD
057300960917     C                   MOVE      V1CRSM        D20TRA
057400960917     C                   MOVEL     TNSC20        KPJBU
057500960918     C                   CALL      'TNSC21R5'
057600960917     C                   PARM                    KPJBA
057700960917     C                   END
057800931011     C*----------------------------------------------------*
057900941102     C                   MOVEL     KPJBU         TNSC20
058000941102     C     D20CMD        IFEQ      3
058100931011     C                   MOVE      'S'           £FINE
058200931011     C                   END
058300931011     C*----------------------------------------------------*
058400931011     C                   ENDSR
058500931011      *---------------------------------------------------------------*
058600941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
058700931011      *---------------------------------------------------------------*
058800941102     C     *INZSR        BEGSR
058900931011      *---------------------------------------------------------------*
059000941102     C     *ENTRY        PLIST
059100931011     C                   PARM                    KPJBA
059200931011      *
059300931011     C                   Z-ADD     1             CODUT
059400931011     C                   CALL      'X§PARUT'
059500931011     C                   PARM                    UT§DSE
059600931011     C*          CODUT     CABEQ-1        FINE
059700931011     C                   MOVEL     RAGUT         RSUT             20
059800931011     C                   MOVEL     REC80         CNCR80
059900931011     C                   MOVEL     KNMUS         §SEI              6
060000960918     C*
060100960918     C*  Definizione chiave di accesso  TABEL
060200960918     C     KTAB          KLIST
060300960918     C                   KFLD                    KKUT
060400960918     C                   KFLD                    KCOD
060500931011     C*----------------------------------------------------*
060600931011     C*  Definizione variabili
060700941102     C     *LIKE         DEFINE    D20PDD        PDDAT
060800941102     C     *LIKE         DEFINE    D20PDD        PADAT
060900941102     C     *LIKE         DEFINE    D20PDD        SDDAT
061000941102     C     *LIKE         DEFINE    D20PDD        SADAT
061100931011     C     *LIKE         DEFINE    G02TGI        DIFDAT
061200931011     C     *LIKE         DEFINE    G02TGI        TGIA
061300931011     C     *LIKE         DEFINE    G02TGI        TGIDA
061400960918     C     *LIKE         DEFINE    TBLKUT        KKUT
061500960918     C     *LIKE         DEFINE    TBLCOD        KCOD
061600931011     C*----------------------------------------------------*
061700931011     C                   MOVEL     'N'           £FINE             1
061800931011     C                   MOVEL     '1'           TIPVID            1
061900941102     C                   TIME                    WHHDAT           14 0
062000960918     C*----------------------------------------------------*
062100960918     C*  Carico tabella stato C/Assegni
062200960918     C                   Z-ADD     1             KKUT
062300960918     C                   MOVEL     '4S'          KCOD
062400960918     C                   MOVEL     *BLANKS       C4S
062500960918     C                   Z-ADD     0             X                 3 0
062600960918     C     KTAB          CHAIN     TABEL00F                           31
062700960918     C     *IN31         DOWEQ     '0'
062800960918     C     TBLFLG        IFEQ      ' '
062900960918     C                   MOVEL     TBLUNI        DS4S
063000960918     C     §4SFIL        IFEQ      'S'
063100960918     C                   ADD       1             X
063200960918     C                   MOVE      TBLKEY        C4S(X)
063300960918     C                   END
063400960918     C                   END
063500960918     C     KTAB          READE     TABEL00F                               31
063600960918     C                   END
063700991230     C*  Carico tabella delle divise
063800991230     C                   Z-ADD     1             KKUT
063900991230     C                   MOVEL     'CV'          KCOD
064000991230     C                   MOVEL     *BLANKS       CCV
064100991230     C                   Z-ADD     0             X                 3 0
064200991230     C     KTAB          CHAIN     TABEL00F                           31
064300991230     C     *IN31         DOWEQ     '0'
064400991230     C     TBLFLG        IFEQ      ' '
064500991230     C     TBLKEY        ANDNE     *BLANKS
064600991230     C                   ADD       1             X
064700991230     C                   MOVEL     TBLKEY        CCV(X)
064800991230     C                   END
064900991230     C     KTAB          READE     TABEL00F                               31
065000991230     C                   END
065100931011     C*----------------------------------------------------*
065200931011     C                   ENDSR
065300991230**
065400960917Linea di partenza obbligatoria                                         01
065500960917Numero di spedizione obbligatorio                                      02
065600960917Immettere limite inferiore data di pagamento                           03
065700960917Data non valida                                                        04
065800960917Prima data maggiore della seconda                                      05
065900960917Impossibile immettere sia codice che ragione sociale                   06
066000960917Immettere stato cliente                                                07
066100960917Immettere numero riferimento mittente                                  08
066200960917Range di date superiore a 7 giorni                                     09
066300960917Immettere limite inferiore data di spedizione                          10
066400960917Stampa non prevista per selezione numero spedizione                    11
066500960917Immettere stato contrassegno                                           12
066600960917Parzializzazione per tipo non possibile con selezione per stato        13
066700960918Stato C/Assegno errato                                                 14
066800991230Divisa inesistente                                                     15
