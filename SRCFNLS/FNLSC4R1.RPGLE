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
014000061221     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
014100000606     D L1              S              3  0 DIM(30)
014200000605      **************
014300000605      *  DS con parametri Input/Output
014400000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
014500000605      **************
014600000605      *  DS di comodo
014700000606     D KPJBA         E DS
014800000606     D OG143         E DS
014900061221     D OG148         E DS
015000000605     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
015100040625     D Dvpoppt       E DS
015200000605     D CNCR80        E DS
015300000606     D DSUL06        E DS                  EXTNAME(TRUL06DS)
015400000606     D  LIN                    1     90  0
015500000606     D                                     DIM(30)
015600040625     d savkpjbu        S                   like(kpjbu)
015700050802     d IC4DRF          S              8S 0
015800000605      ****************************************************************
015900000605      *                                                              *
016000000605      *  O P E R A Z I O N I   I N I Z I Z A L I                     *
016100000605      *                                                              *
016200000605      ****************************************************************
016300000605     C     *ENTRY        PLIST
016400000606     C                   PARM                    KPJBA
016500050802     C                   PARM                    ic4drf
016600050802     c
016700000608     C                   MOVEL     KPJBU         FNLSC4
016800050802     c*
016900050802     C                   CLEAR                   DSUL06
017000050802     C                   MOVE      '£1'          D06COD
017100050802     C                   MOVEL     SIMFEL        D06KEY
017200050802     C                   MOVEL     'L'           D06TLA
017300050802     c
017400050802     c                   if        %parms=2
017500050802     c                   move      'S'           d06key
017600050802     c                   movel     ic4drf        d06drf
017700050802     c                   endif
017800050802     c
017900050802     C                   MOVEL(P)  DSUL06        KPJBU
018000050802     C                   CALL      'TRUL06R'
018100050802     C                   PARM                    KPJBA
018200050802     C                   MOVEL     KPJBU         DSUL06
018300050802     C                   MOVEA     LIN           L1
018400000605      *
018500000605      *  INIZIALIZZO I CAMPI DI OUTPUT
018600000605      *
018700000608     C                   Z-ADD     0             OC4POG
018800000608     C                   MOVE      *BLANK        OC4PFL
018900000608     C                   MOVE      *BLANK        OC4PT1
019000000608     C                   MOVE      *BLANK        OC4DPD
019100000608     C                   MOVE      *BLANK        OC4EXP
019200020211     C                   MOVE      *BLANK        OC4FED
019300020211     C                   MOVE      *BLANK        OC4BAR
019400000608     C                   MOVE      *BLANK        OC4PT2
019500000608     C                   MOVE      *BLANK        OC4ESE
019600000608     C                   MOVE      *BLANKS       OC4MSG
019700000605      ****************************************************************
019800000605      *                                                              *
019900000605      *  R O U T I N E   P R I N C I P A L E                         *
020000000605      *                                                              *
020100000605      ****************************************************************
020200000605      *
020300000605      *  VERIFICO SE LINEA E' CORRETTA E SE E' POSTE
020400000605      *
020500000605     C                   MOVE      '1'           WORK01            1
020600000605     C                   EXSR      CHKPT
020700000605      *                  ===============
020800000605     C     *IN30         IFEQ      *ON
020900000608     C                   MOVE      'N'           OC4ESE
021000000608     C                   MOVEL(P)  MSG(4)        OC4MSG
021100061221     c                   else
021200061221     c* se tipo p.o. blanks --> errore, p.o. non operativo
021300070111     c* I p.o. esteri, però, anche se non operativi li deve lo stesso
021400070111     c* elaborare perchè di fatto sono operativi ai fini delle statistiche
021500070111     c                   if        §oglpo=*blanks and §ogntw<>'DPD' and
021600070111     c                             §ogntw<>'FED' and §ogntw<>'EEX'
021700061221     C                   MOVE      'N'           OC4ESE
021800061221     C                   MOVEL(P)  MSG(6)        OC4MSG
021900061221     c                   endif
022000000605     C                   ENDIF
022100000606      *
022200000606      *  CONTROLLO SE LINEA E' GESTITA DAL P.O.
022300000606      *
022400000608     C     OC4ESE        IFNE      'N'
022500040216     c     ic4flg        andne     'N'
022600000608     C     IC4LIN        LOOKUP    L1                                     31
022700000606     C     *IN31         IFEQ      *OFF
022800000608     C                   MOVE      'N'           OC4ESE
022900000608     C                   MOVEL(P)  MSG(5)        OC4MSG
023000000606     C                   ENDIF
023100000606     C                   ENDIF
023200000605      *
023300000606      *  CONTROLLO UTENTE PER SAPERE CODICE P.O. GESTIONE
023400000608      *  E INFORMAZIONE SE E' 1° O 2° LIVELLO
023500000606      *
023600020429     c                   move      simpou        oc4pog
023700000605      *
023800000608      *  P.O. GESTIONE 2° LIVELLO
023900000605      *
024000020429     c     simtpp        ifeq      '2'
024100020429     c     simtpp        oreq      *blanks
024200000608     C                   MOVE      '2'           OC4PFL
024300000605      *
024400000608      *  P.O. GESTIONE 1° LIVELLO
024500000605      *
024600000605     C                   ELSE
024700000608     C                   MOVE      '1'           OC4PFL
024800020429     C                   ENDIF
024900000605      *
025000000605      *  VERIFICO SE P.O. E' O NON E' POSTE
025100000605      *
025200000605     C                   MOVE      '2'           WORK01
025300000605     C                   EXSR      CHKPT
025400000605      *                  ===============
025500000605      *
025600000605      *  VERIFICO SE E' POSSIBILE ESEGUIRE L'ELABORAZIONE PER
025700000605      *  LA LINEA RICHIESTA
025800000605      *
025900000605     C                   EXSR      ESESN
026000000605      *                  ===============
026100000605      *
026200000605      *  FINE PROGRAMMA (RT O LR IN BASE A CAMPO DI INPUT)
026300000605      *
026400000608     C                   MOVEL(P)  FNLSC4        KPJBU
026500000606      *
026600000608     C     IC4TPE        IFEQ      'C'
026700000605     C                   SETON                                        LR
026800000605     C                   ELSE
026900000605     C                   SETON                                        RT
027000000605     C                   ENDIF
027100000605      ****************************************************************
027200000605      *                                                              *
027300000605      *  S U B R O U T I N E S                                       *
027400000605      *                                                              *
027500000605      ****************************************************************
027600000605      *
027700000605      *--------------------------------------------------------------*
027800000605      *  CHKPT - SUBROUTINE VERIFICA SE FILIALE E' POSTE             *
027900000605      *--------------------------------------------------------------*
028000000605     C     CHKPT         BEGSR
028100000605      *
028200000605     C                   CLEAR                   OG143
028300061221     C                   CLEAR                   OG148
028400000605     C     WORK01        IFEQ      '1'
028500000608     C     IC4LIN        CHAIN     AZORG01L                           30
028600000605     C                   ELSE
028700000608     C     OC4POG        CHAIN     AZORG01L                           30
028800000605     C                   ENDIF
028900000607      ***
029000000605     C     *IN30         IFEQ      *OFF
029100000605     C                   MOVEL     ORGDE3        OG143
029200061221     C                   MOVEL     ORGDE8        OG148
029300020211     C     §OGNTW        IFEQ      'PPT'
029400000605     C     WORK01        IFEQ      '1'
029500000608     C                   MOVE      'S'           OC4PT1
029600000605     C                   ELSE
029700000608     C                   MOVE      'S'           OC4PT2
029800000605     C                   ENDIF
029900000605     C                   ELSE
030000000605     C     WORK01        IFEQ      '1'
030100000608     C                   MOVE      'N'           OC4PT1
030200000605     C                   ELSE
030300000608     C                   MOVE      'N'           OC4PT2
030400000605     C                   ENDIF
030500000605     C                   ENDIF
030600000607      * - - -
030700000607     C     WORK01        IFEQ      '1'
030800020211     C***  §OGDPD        IFEQ      'S'
030900020211     C     §OGNTW        IFEQ      'DPD'
031000000608     C                   MOVE      'S'           OC4DPD
031100000607     C                   ELSE
031200000608     C                   MOVE      'N'           OC4DPD
031300000607     C                   ENDIF
031400020211      * - - -
031500020211     C     §OGNTW        IFEQ      'FED'
031600020211     C                   MOVE      'S'           OC4FED
031700020211     C                   ELSE
031800020211     C                   MOVE      'N'           OC4FED
031900020211     C                   ENDIF
032000020211      * - - -
032100020211     C**   ORGFL1        IFEQ      'E'
032200020211     C     §OGNTW        IFEQ      'EEX'
032300020211     C     §OGNTW        OREQ      'EUP'
032400000608     C                   MOVE      'S'           OC4EXP
032500000607     C                   ELSE
032600000608     C                   MOVE      'N'           OC4EXP
032700000607     C                   ENDIF
032800020211      * - - -
032900000608     C     OC4PT1        IFEQ      'N'
033000000608     C     OC4DPD        ANDEQ     'N'
033100000608     C     OC4EXP        ANDEQ     'N'
033200020211     C     OC4FED        ANDEQ     'N'
033300000608     C                   MOVE      'S'           OC4BAR
033400000607     C                   ELSE
033500000608     C                   MOVE      'N'           OC4BAR
033600000607     C                   ENDIF
033700000607     C                   ENDIF
033800000607      * - - -
033900000605     C                   ENDIF
034000000607      ***
034100000605     C                   ENDSR
034200000605      *--------------------------------------------------------------*
034300000605      *  ESESN - SUBROUTINE VERIFICA SE E' POSSIBILE L'ELABORAZIONE  *
034400000605      *--------------------------------------------------------------*
034500000605     C     ESESN         BEGSR
034600000605      *
034700000605      *  SUBROUTINE ESEGUIBILE SOLO SE VALIDI I CAMPI DI OUTPUT:
034800000608      *  "OC4PFL", "OC4PT1", "OC4PT2"
034900000605      *  E IL CAMPO DI INPUT:
035000000608      *  "IC4FLG"
035100000605      *
035200000608     C     IC4FLG        IFEQ      'I'
035300000608     C     IC4FLG        OREQ      'B'
035400040216     C     IC4FLG        OREQ      'N'
035500000608     C     OC4PFL        IFEQ      '1'
035600000608     C     OC4PFL        OREQ      '2'
035700000608     C     OC4PT1        IFEQ      'S'
035800000608     C     OC4PT1        OREQ      'N'
035900000608     C     OC4PT2        IFEQ      'S'
036000000608     C     OC4PT2        OREQ      'N'
036100000605      *
036200000608     C     OC4ESE        IFNE      'N'
036300000608     C                   MOVE      'S'           OC4ESE
036400000605      *
036500000605      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
036600000605      *
036700000608      *  SE P.O. GESTIONE E' 2° LIV. PUO' LANCIARE SOLO SE STESSO
036800000605      *
036900000608     C     OC4PFL        IFEQ      '2'
037000000608     C     IC4LIN        ANDNE     OC4POG
037100000608     C                   MOVE      'N'           OC4ESE
037200000608     C                   MOVEL(P)  MSG(1)        OC4MSG
037300000605     C                   ENDIF
037400000605      *
037500000605      *  SE P.O. GESTIONE E' POSTE NON PUO' LANCIARE LINEA NO POSTE
037600000605      *
037700000608     C     OC4ESE        IFEQ      'S'
037800000608     C     OC4PT2        ANDEQ     'S'
037900000608     C     OC4PT1        ANDEQ     'N'
038000000608     C                   MOVE      'N'           OC4ESE
038100000608     C                   MOVEL(P)  MSG(2)        OC4MSG
038200000605     C                   ENDIF
038300000605      *
038400000605      *  SE P.O. GESTIONE NON E' POSTE NON PUO' LANCIARE I BATCH POSTE
038500040625     c*   lo può invece fare se data > data ultima fase poste
038600000605      *
038700040625     c                   if        dateu<§vpoptp
038800000608     C     OC4ESE        IFEQ      'S'
038900000608     C     OC4PT2        ANDEQ     'N'
039000000608     C     IC4FLG        ANDEQ     'B'
039100000608     C     OC4PT1        ANDEQ     'S'
039200040216     C     OC4ESE        oreq      'S'
039300040216     C     OC4PT2        ANDEQ     'N'
039400040216     C     IC4FLG        ANDEQ     'N'
039500040216     C     OC4PT1        ANDEQ     'S'
039600000608     C                   MOVE      'N'           OC4ESE
039700000608     C                   MOVEL(P)  MSG(3)        OC4MSG
039800000605     C                   ENDIF
039900040625     C                   ENDIF
040000000605      *
040100000605      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
040200000605      *
040300000605     C                   ENDIF
040400000605      *
040500000605     C                   ENDIF
040600000605     C                   ENDIF
040700000605     C                   ENDIF
040800000605     C                   ENDIF
040900000605      *
041000000605     C                   ENDSR
041100040625      *--------------------------------------------------------------*
041200040625      *  elaborazioni da eseguire una volta sola
041300040625      *--------------------------------------------------------------*
041400040625     c     *inzsr        begsr
041500040625     c                   movel     kpjbu         savkpjbu
041600040625     C     KTBE          KLIST
041700040625     C                   KFLD                    KCOD1
041800040625     C                   KFLD                    KKE1
041900040625     C     *LIKE         DEFINE    TBECOD        KCOD1
042000040625     C     *LIKE         DEFINE    TBEKE1        KKE1
042100040625      *
042200040625     C                   CLEAR                   CNCR80
042300040625     C                   CLEAR                   UT§DSE
042400040625     c**
042500040625     C                   Z-ADD     1             CODUT
042600040625     C                   CALL      'X§PARUT'
042700040625     C                   PARM                    UT§DSE
042800040625     C                   MOVEL     REC80         CNCR80
042900040625     C*
043000040625     C*  Tabelle VPOPPT x reperimento data fase ultima poste
043100040625     C                   movel     'VPO'         kcod1
043200040625     C                   movel(P)  'PPT'         kke1
043300040625     C     ktbe          chain     tntbe01l                           31
043400040625     C                   clear                   dvpoppt
043500040625     C  N31              movel     tbeuni        dvpoppt
043600040625     c* Prendo data di oggi
043700040625     c                   z-add     *date         dateu             8 0
043800040625     c
043900040625     c                   movel     savkpjbu      kpjbu
044000040625     C                   ENDSR
044100000605**
044200000605Il Punto Operativo remoto può richiedere solo se stesso
044300000605Il Punto Operativo Poste può richiedere solo elaboraz. Poste
044400000605Il Punto Operativo non Poste non può richiedere elaboraz. Poste
044500000605Punto Operativo inesistente
044600000605Punto Operativo in gestione ad altra filiale
044700061221Punto Operativo non operativo
