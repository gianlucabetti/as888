000100040625     H DECEDIT('0,') DATEDIT(*yMd.)
000200000602      *--------------------------------------------------------------*
000300000602      *                                                              *
000400020211      * PGM/UTILITY:   ROUTINE PER CONTROLLO FILIALE POSTE, DPD,fedex*
000500000608      * ============   EURO EXPRESS, BARTOLINI/SDI; LIVELLO P.O.     *
000600000608      *                GESTIONE; SE E' POSSIBILE ELABORARE POSTE     *
000700000607      *                                                              *
000800000602      *--------------------------------------------------------------*
000900000602      *                                                              *
001000000602      * IN BASE AL RISULTATO CHE SI VUOLE OTTENERE E' NECESSARIO IM- *
001100000602      * POSTARE CORRETTAMENTE I VALORI DI INPUT.                     *
001200000602      *                                                              *
001300000602      *--------------------------------------------------------------*
001400000602      *                                                              *
001500000608      * PARAMETRI DS FNLSC4DS (INPUT):                               *
001600000602      * ------------------------------                               *
001700000602      *                                                              *
001800000608      * - IC4LIN: LINEA PER LA QUALE SI RICHIEDE L'ELABORAZIONE.     *
001900000602      *           VALORI VALIDI:                                     *
002000000602      *             UNA LINEA VALIDA                                 *
002100000602      *                                                              *
002200000608      * - IC4FLG: FLAG ELABORAZIONE RICHIESTA.                       *
002300000602      *           VALORI VALIDI:                                     *
002400000602      *             "I"=ELABORAZIONE INTERATTIVA                     *
002500000602      *             "B"=ELABORAZIONE BATCH                           *
002600040216      *             "N"=ELABORAZIONE BATCH - non fare lookup in £1   *
002700000602      *                                                              *
002800000608      * - IC4TPE: FLAG TIPO ELABORAZIONE E CHIUSURA.                 *
002900000602      *           VALORI VALIDI:                                     *
003000000602      *             "C"=CHIUDO CON SETON LR                          *
003100000602      *             ALTRO=CHIUDO CON SETON RT                        *
003200000602      *                                                              *
003300000608      * PARAMETRI DS FNLSC4DS (OUTPUT):                              *
003400000602      * -------------------------------                              *
003500000602      *                                                              *
003600040625      * - OC4POG: P.O. utente   DI CHI RICHIEDE L'ELABORAZIONE.      *
003700000606      *           E'NECESSARIO CHE SIA VALIDO ALMENO:                *
003800000606      *             "KNMUS ": PROFILO UTENTE                         *
003900000602      *           VALORI IN OUTPUT RESTITUITI:                       *
004000000602      *             P.O. GESTIONE VALIDO                             *
004100000602      *             *ZEROS=VALORE NON VALIDO                         *
004200000602      *                                                              *
004300000608      * - OC4PFL: FLAG LIVELLO P.O. GESTIONE.                        *
004400000606      *           E'NECESSARIO CHE SIA VALIDO ALMENO:                *
004500000606      *             "KNMUS ": PROFILO UTENTE                         *
004600000602      *           VALORI IN OUTPUT POSSIBILI:                        *
004700000608      *             "1"=1° LIVELLO                                   *
004800000608      *             "2"=2° LIVELLO                                   *
004900000602      *             " "=VALORE NON VALIDO                            *
005000000602      *                                                              *
005100000608      * - OC4PT1: FLAG CHE INDICA SE LA LINEA PER LA QUALE SI RI-    *
005200000602      *           CHIEDE L'ELABORAZIONE E' POSTE OPPURE NO.          *
005300000602      *           E'NECESSARIO CHE IN INPUT SIA VALIDO ALMENO:       *
005400000608      *             "IC4LIN": LINEA                                  *
005500000602      *           VALORI IN OUTPUT POSSIBILI:                        *
005600000602      *             "S"=SI POSTE                                     *
005700000602      *             "N"=NO POSTE                                     *
005800000602      *             " "=VALORE NON VALIDO                            *
005900000602      *                                                              *
006000000608      * - OC4DPD: FLAG CHE INDICA SE LA LINEA PER LA QUALE SI RI-    *
006100000607      *           CHIEDE L'ELABORAZIONE E' DPD OPPURE NO.            *
006200000607      *           E'NECESSARIO CHE IN INPUT SIA VALIDO ALMENO:       *
006300000608      *             "IC4LIN": LINEA                                  *
006400000607      *           VALORI IN OUTPUT POSSIBILI:                        *
006500000607      *             "S"=SI DPD                                       *
006600000607      *             "N"=NO DPD                                       *
006700000607      *             " "=VALORE NON VALIDO                            *
006800000607      *                                                              *
006900020211      * - OC4FED: FLAG CHE INDICA SE LA LINEA PER LA QUALE SI RI-    *
007000020211      *           CHIEDE L'ELABORAZIONE E' FEDEX OPPURE NO.            *
007100020211      *           E'NECESSARIO CHE IN INPUT SIA VALIDO ALMENO:       *
007200020211      *             "IC4LIN": LINEA                                  *
007300020211      *           VALORI IN OUTPUT POSSIBILI:                        *
007400020211      *             "S"=SI DPD                                       *
007500020211      *             "N"=NO DPD                                       *
007600020211      *             " "=VALORE NON VALIDO                            *
007700020211      *                                                              *
007800000608      * - OC4EXP: FLAG CHE INDICA SE LA LINEA PER LA QUALE SI RI-    *
007900000607      *           CHIEDE L'ELABORAZIONE E' EURO EXPR. OPPURE NO.     *
008000000607      *           E'NECESSARIO CHE IN INPUT SIA VALIDO ALMENO:       *
008100000608      *             "IC4LIN": LINEA                                  *
008200000607      *           VALORI IN OUTPUT POSSIBILI:                        *
008300020211      *             "S"=SI EURO EXPRESS/EUROPOLITAN                  *
008400020211      *             "N"=NO EURO EXPRESS/EUROPOLITAN                  *
008500000607      *             " "=VALORE NON VALIDO                            *
008600000607      *                                                              *
008700000608      * - OC4BAR: FLAG CHE INDICA SE LA LINEA PER LA QUALE SI RI-    *
008800020211      *           CHIEDE L'ELABORAZIONE E' BARTOLINI/SDI ITALIA   .  *
008900000607      *           E'NECESSARIO CHE IN INPUT SIA VALIDO ALMENO:       *
009000000608      *             "IC4LIN": LINEA                                  *
009100000607      *           VALORI IN OUTPUT POSSIBILI:                        *
009200000608      *             "S"=SI BARTOLINI/SDI                             *
009300000608      *             "N"=NO BARTOLINI/SDI                             *
009400000607      *             " "=VALORE NON VALIDO                            *
009500000607      *                                                              *
009600000608      * - OC4PT2: FLAG CHE INDICA SE IL P.O. GESTIONE DI CHI RI-     *
009700000602      *           CHIEDE L'ELABORAZIONE E' POSTE OPPURE NO.          *
009800000606      *           E'NECESSARIO CHE SIA VALIDO ALMENO:                *
009900000606      *             "KNMUS ": PROFILO UTENTE                         *
010000000602      *           VALORI IN OUTPUT POSSIBILI:                        *
010100000602      *             "S"=SI POSTE                                     *
010200000602      *             "N"=NO POSTE                                     *
010300000602      *             " "=VALORE NON VALIDO                            *
010400000602      *                                                              *
010500000608      * - OC4ESE: FLAG CHE INDICA SE L'ELABORAZIONE PUO' ESSERE      *
010600000602      *           ESEGUITA.                                          *
010700000602      *           E'NECESSARIO CHE IN INPUT SIA VALIDO ALMENO:       *
010800000608      *             "IC4LIN": LINEA                                  *
010900000606      *             "KNMUS ": PROFILO UTENTE                         *
011000000608      *             "IC4FLG": FLAG ELABORAZIONE                      *
011100000602      *           VALORI IN OUTPUT POSSIBILI:                        *
011200000602      *             "S"=ELABORAZIONE ESEGUIBILE                      *
011300000602      *             "N"=ELABORAZIONE NON ESEGUIBILE                  *
011400000602      *             " "=VALORE NON VALIDO                            *
011500000605      *                                                              *
011600000605      *           N.B.: PER LE REGOLE DI ELABORAZIONE VEDI COMMENTI  *
011700000605      *                 IN SUBROUTINE "ESESN"                        *
011800000605      *                                                              *
011900000608      * - OC4MSG: MESSAGGIO DI ERRORE CHE DESCRIVE IL PERCHE' NON    *
012000000602      *           E' POSSIBILE ESEGUIRE L'ELABORAZIONE.              *
012100000602      *           E'NECESSARIO CHE IN INPUT SIA VALIDO ALMENO:       *
012200000608      *             "IC4LIN": LINEA                                  *
012300000606      *             "KNMUS ": PROFILO UTENTE                         *
012400000608      *             "IC4FLG": FLAG ELABORAZIONE                      *
012500000602      *           VALORI IN OUTPUT POSSIBILI:                        *
012600000602      *             MESSAGGIO=DESCRIZIONE ELABORAZIONE NON POSSIBILE *
012700000602      *             *BLANKS=ELABORAZ. POSSIBILE O VALORE NON VALIDO  *
012800000602      *                                                              *
012900000605      *--------------------------------------------------------------*
013000000605      *                                                              *
013100000605      * UTILIZZO DEGLI INDICATORI:                                   *
013200000605      * --------------------------                                   *
013300000605      *                                                              *
013400000605      * 30 - CHAIN AZORG01L                                          *
013500000606      * 31 - UTILIZZATO PER RICERCA FILIALE IN L1                    *
013600000605      *                                                              *
013700000605      *--------------------------------------------------------------*
013800040625     FTNTBE01L  IF   E           K DISK
013900000602     FAZORG01L  IF   E           K DISK
014000000606     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
014100000606     D L1              S              3  0 DIM(30)
014200000605      **************
014300000605      *  DS con parametri Input/Output
014400000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
014500000605      **************
014600000605      *  DS di comodo
014700000606     D KPJBA         E DS
014800000606     D OG143         E DS
014900000605     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
015000040625     D Dvpoppt       E DS
015100000605     D CNCR80        E DS
015200000606     D DSUL06        E DS                  EXTNAME(TRUL06DS)
015300000606     D  LIN                    1     90  0
015400000606     D                                     DIM(30)
015500040625     d savkpjbu        S                   like(kpjbu)
015600050802     d IC4DRF          S              8S 0
015700000605      ****************************************************************
015800000605      *                                                              *
015900000605      *  O P E R A Z I O N I   I N I Z I Z A L I                     *
016000000605      *                                                              *
016100000605      ****************************************************************
016200000605     C     *ENTRY        PLIST
016300000606     C                   PARM                    KPJBA
016400050802     C                   PARM                    ic4drf
016500050802     c
016600000608     C                   MOVEL     KPJBU         FNLSC4
016700050802     c*
016800050802     C                   CLEAR                   DSUL06
016900050802     C                   MOVE      '£1'          D06COD
017000050802     C                   MOVEL     SIMFEL        D06KEY
017100050802     C                   MOVEL     'L'           D06TLA
017200050802     c
017300050802     c                   if        %parms=2
017400050802     c                   move      'S'           d06key
017500050802     c                   movel     ic4drf        d06drf
017600050802     c                   endif
017700050802     c
017800050802     C                   MOVEL(P)  DSUL06        KPJBU
017900050802     C                   CALL      'TRUL06R'
018000050802     C                   PARM                    KPJBA
018100050802     C                   MOVEL     KPJBU         DSUL06
018200050802     C                   MOVEA     LIN           L1
018300000605      *
018400000605      *  INIZIALIZZO I CAMPI DI OUTPUT
018500000605      *
018600000608     C                   Z-ADD     0             OC4POG
018700000608     C                   MOVE      *BLANK        OC4PFL
018800000608     C                   MOVE      *BLANK        OC4PT1
018900000608     C                   MOVE      *BLANK        OC4DPD
019000000608     C                   MOVE      *BLANK        OC4EXP
019100020211     C                   MOVE      *BLANK        OC4FED
019200020211     C                   MOVE      *BLANK        OC4BAR
019300000608     C                   MOVE      *BLANK        OC4PT2
019400000608     C                   MOVE      *BLANK        OC4ESE
019500000608     C                   MOVE      *BLANKS       OC4MSG
019600000605      ****************************************************************
019700000605      *                                                              *
019800000605      *  R O U T I N E   P R I N C I P A L E                         *
019900000605      *                                                              *
020000000605      ****************************************************************
020100000605      *
020200000605      *  VERIFICO SE LINEA E' CORRETTA E SE E' POSTE
020300000605      *
020400000605     C                   MOVE      '1'           WORK01            1
020500000605     C                   EXSR      CHKPT
020600000605      *                  ===============
020700000605     C     *IN30         IFEQ      *ON
020800000608     C                   MOVE      'N'           OC4ESE
020900000608     C                   MOVEL(P)  MSG(4)        OC4MSG
021000000605     C                   ENDIF
021100000606      *
021200000606      *  CONTROLLO SE LINEA E' GESTITA DAL P.O.
021300000606      *
021400000608     C     OC4ESE        IFNE      'N'
021500040216     c     ic4flg        andne     'N'
021600000608     C     IC4LIN        LOOKUP    L1                                     31
021700000606     C     *IN31         IFEQ      *OFF
021800000608     C                   MOVE      'N'           OC4ESE
021900000608     C                   MOVEL(P)  MSG(5)        OC4MSG
022000000606     C                   ENDIF
022100000606     C                   ENDIF
022200000605      *
022300000606      *  CONTROLLO UTENTE PER SAPERE CODICE P.O. GESTIONE
022400000608      *  E INFORMAZIONE SE E' 1° O 2° LIVELLO
022500000606      *
022600020429     c                   move      simpou        oc4pog
022700000605      *
022800000608      *  P.O. GESTIONE 2° LIVELLO
022900000605      *
023000020429     c     simtpp        ifeq      '2'
023100020429     c     simtpp        oreq      *blanks
023200000608     C                   MOVE      '2'           OC4PFL
023300000605      *
023400000608      *  P.O. GESTIONE 1° LIVELLO
023500000605      *
023600000605     C                   ELSE
023700000608     C                   MOVE      '1'           OC4PFL
023800020429     C                   ENDIF
023900000605      *
024000000605      *  VERIFICO SE P.O. E' O NON E' POSTE
024100000605      *
024200000605     C                   MOVE      '2'           WORK01
024300000605     C                   EXSR      CHKPT
024400000605      *                  ===============
024500000605      *
024600000605      *  VERIFICO SE E' POSSIBILE ESEGUIRE L'ELABORAZIONE PER
024700000605      *  LA LINEA RICHIESTA
024800000605      *
024900000605     C                   EXSR      ESESN
025000000605      *                  ===============
025100000605      *
025200000605      *  FINE PROGRAMMA (RT O LR IN BASE A CAMPO DI INPUT)
025300000605      *
025400000608     C                   MOVEL(P)  FNLSC4        KPJBU
025500000606      *
025600000608     C     IC4TPE        IFEQ      'C'
025700000605     C                   SETON                                        LR
025800000605     C                   ELSE
025900000605     C                   SETON                                        RT
026000000605     C                   ENDIF
026100000605      ****************************************************************
026200000605      *                                                              *
026300000605      *  S U B R O U T I N E S                                       *
026400000605      *                                                              *
026500000605      ****************************************************************
026600000605      *
026700000605      *--------------------------------------------------------------*
026800000605      *  CHKPT - SUBROUTINE VERIFICA SE FILIALE E' POSTE             *
026900000605      *--------------------------------------------------------------*
027000000605     C     CHKPT         BEGSR
027100000605      *
027200000605     C                   CLEAR                   OG143
027300000605     C     WORK01        IFEQ      '1'
027400000608     C     IC4LIN        CHAIN     AZORG01L                           30
027500000605     C                   ELSE
027600000608     C     OC4POG        CHAIN     AZORG01L                           30
027700000605     C                   ENDIF
027800000607      ***
027900000605     C     *IN30         IFEQ      *OFF
028000000605     C                   MOVEL     ORGDE3        OG143
028100020211     C     §OGNTW        IFEQ      'PPT'
028200000605     C     WORK01        IFEQ      '1'
028300000608     C                   MOVE      'S'           OC4PT1
028400000605     C                   ELSE
028500000608     C                   MOVE      'S'           OC4PT2
028600000605     C                   ENDIF
028700000605     C                   ELSE
028800000605     C     WORK01        IFEQ      '1'
028900000608     C                   MOVE      'N'           OC4PT1
029000000605     C                   ELSE
029100000608     C                   MOVE      'N'           OC4PT2
029200000605     C                   ENDIF
029300000605     C                   ENDIF
029400000607      * - - -
029500000607     C     WORK01        IFEQ      '1'
029600020211     C***  §OGDPD        IFEQ      'S'
029700020211     C     §OGNTW        IFEQ      'DPD'
029800000608     C                   MOVE      'S'           OC4DPD
029900000607     C                   ELSE
030000000608     C                   MOVE      'N'           OC4DPD
030100000607     C                   ENDIF
030200020211      * - - -
030300020211     C     §OGNTW        IFEQ      'FED'
030400020211     C                   MOVE      'S'           OC4FED
030500020211     C                   ELSE
030600020211     C                   MOVE      'N'           OC4FED
030700020211     C                   ENDIF
030800020211      * - - -
030900020211     C**   ORGFL1        IFEQ      'E'
031000020211     C     §OGNTW        IFEQ      'EEX'
031100020211     C     §OGNTW        OREQ      'EUP'
031200000608     C                   MOVE      'S'           OC4EXP
031300000607     C                   ELSE
031400000608     C                   MOVE      'N'           OC4EXP
031500000607     C                   ENDIF
031600020211      * - - -
031700000608     C     OC4PT1        IFEQ      'N'
031800000608     C     OC4DPD        ANDEQ     'N'
031900000608     C     OC4EXP        ANDEQ     'N'
032000020211     C     OC4FED        ANDEQ     'N'
032100000608     C                   MOVE      'S'           OC4BAR
032200000607     C                   ELSE
032300000608     C                   MOVE      'N'           OC4BAR
032400000607     C                   ENDIF
032500000607     C                   ENDIF
032600000607      * - - -
032700000605     C                   ENDIF
032800000607      ***
032900000605     C                   ENDSR
033000000605      *--------------------------------------------------------------*
033100000605      *  ESESN - SUBROUTINE VERIFICA SE E' POSSIBILE L'ELABORAZIONE  *
033200000605      *--------------------------------------------------------------*
033300000605     C     ESESN         BEGSR
033400000605      *
033500000605      *  SUBROUTINE ESEGUIBILE SOLO SE VALIDI I CAMPI DI OUTPUT:
033600000608      *  "OC4PFL", "OC4PT1", "OC4PT2"
033700000605      *  E IL CAMPO DI INPUT:
033800000608      *  "IC4FLG"
033900000605      *
034000000608     C     IC4FLG        IFEQ      'I'
034100000608     C     IC4FLG        OREQ      'B'
034200040216     C     IC4FLG        OREQ      'N'
034300000608     C     OC4PFL        IFEQ      '1'
034400000608     C     OC4PFL        OREQ      '2'
034500000608     C     OC4PT1        IFEQ      'S'
034600000608     C     OC4PT1        OREQ      'N'
034700000608     C     OC4PT2        IFEQ      'S'
034800000608     C     OC4PT2        OREQ      'N'
034900000605      *
035000000608     C     OC4ESE        IFNE      'N'
035100000608     C                   MOVE      'S'           OC4ESE
035200000605      *
035300000605      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
035400000605      *
035500000608      *  SE P.O. GESTIONE E' 2° LIV. PUO' LANCIARE SOLO SE STESSO
035600000605      *
035700000608     C     OC4PFL        IFEQ      '2'
035800000608     C     IC4LIN        ANDNE     OC4POG
035900000608     C                   MOVE      'N'           OC4ESE
036000000608     C                   MOVEL(P)  MSG(1)        OC4MSG
036100000605     C                   ENDIF
036200000605      *
036300000605      *  SE P.O. GESTIONE E' POSTE NON PUO' LANCIARE LINEA NO POSTE
036400000605      *
036500000608     C     OC4ESE        IFEQ      'S'
036600000608     C     OC4PT2        ANDEQ     'S'
036700000608     C     OC4PT1        ANDEQ     'N'
036800000608     C                   MOVE      'N'           OC4ESE
036900000608     C                   MOVEL(P)  MSG(2)        OC4MSG
037000000605     C                   ENDIF
037100000605      *
037200000605      *  SE P.O. GESTIONE NON E' POSTE NON PUO' LANCIARE I BATCH POSTE
037300040625     c*   lo può invece fare se data > data ultima fase poste
037400000605      *
037500040625     c                   if        dateu<§vpoptp
037600000608     C     OC4ESE        IFEQ      'S'
037700000608     C     OC4PT2        ANDEQ     'N'
037800000608     C     IC4FLG        ANDEQ     'B'
037900000608     C     OC4PT1        ANDEQ     'S'
038000040216     C     OC4ESE        oreq      'S'
038100040216     C     OC4PT2        ANDEQ     'N'
038200040216     C     IC4FLG        ANDEQ     'N'
038300040216     C     OC4PT1        ANDEQ     'S'
038400000608     C                   MOVE      'N'           OC4ESE
038500000608     C                   MOVEL(P)  MSG(3)        OC4MSG
038600000605     C                   ENDIF
038700040625     C                   ENDIF
038800000605      *
038900000605      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
039000000605      *
039100000605     C                   ENDIF
039200000605      *
039300000605     C                   ENDIF
039400000605     C                   ENDIF
039500000605     C                   ENDIF
039600000605     C                   ENDIF
039700000605      *
039800000605     C                   ENDSR
039900040625      *--------------------------------------------------------------*
040000040625      *  elaborazioni da eseguire una volta sola
040100040625      *--------------------------------------------------------------*
040200040625     c     *inzsr        begsr
040300040625     c                   movel     kpjbu         savkpjbu
040400040625     C     KTBE          KLIST
040500040625     C                   KFLD                    KCOD1
040600040625     C                   KFLD                    KKE1
040700040625     C     *LIKE         DEFINE    TBECOD        KCOD1
040800040625     C     *LIKE         DEFINE    TBEKE1        KKE1
040900040625      *
041000040625     C                   CLEAR                   CNCR80
041100040625     C                   CLEAR                   UT§DSE
041200040625     c**
041300040625     C                   Z-ADD     1             CODUT
041400040625     C                   CALL      'X§PARUT'
041500040625     C                   PARM                    UT§DSE
041600040625     C                   MOVEL     REC80         CNCR80
041700040625     C*
041800040625     C*  Tabelle VPOPPT x reperimento data fase ultima poste
041900040625     C                   movel     'VPO'         kcod1
042000040625     C                   movel(P)  'PPT'         kke1
042100040625     C     ktbe          chain     tntbe01l                           31
042200040625     C                   clear                   dvpoppt
042300040625     C  N31              movel     tbeuni        dvpoppt
042400040625     c* Prendo data di oggi
042500040625     c                   z-add     *date         dateu             8 0
042600040625     c
042700040625     c                   movel     savkpjbu      kpjbu
042800040625     C                   ENDSR
042900000605**
043000000605Il Punto Operativo remoto può richiedere solo se stesso
043100000605Il Punto Operativo Poste può richiedere solo elaboraz. Poste
043200000605Il Punto Operativo non Poste non può richiedere elaboraz. Poste
043300000605Punto Operativo inesistente
043400000605Punto Operativo in gestione ad altra filiale
