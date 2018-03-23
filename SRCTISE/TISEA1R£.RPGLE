000100130130     /*END
000200000000     H*------------------------------------------------------------------------*
000300000000     H* STATISTICHE DELTA PER FASCE DI PESO - RICHIESTA PARAMETRI              *
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700010508     FTISEA1D   CF   E             WORKSTN INFDS(DEVDS1)
000800010508     F                                     SFILE(SEA1DVM:MSGULT)
000900000000     FTABEL00F  IF   E           K DISK
001000000000     D*------------------------------------------------------------------------*
001100010508     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             *MESSAGGI
001200000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001300000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
001400000000     D FFU             S              6    DIM(31)                              *FUNZIONE
001500000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZIONE
001600000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
001700000000     D* TABELLA DESCRIZIONI A VIDEO
001800000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
001900000000     D* TABELLA TASTI FUNZIONALI PREMUTI
002000000000     D KEYS            S              1    DIM(40)
002100000000     D*------------------------------------------------------------------------*
002200000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
002300000000     D DSFUN         E DS
002400000000     D  FUNRIG                 1     40
002500000000     D*------------------------------------------------------------------------*
002600000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
002700000000     D DSMSG         E DS
002800000000     D*------------------------------------------------------------------------*
002900020916     D* DS REPERIMENTO DATI UTENTE
003000020916     D*-------------------
003100020916     D TIBS34DS      E DS                                                       *Profili utente
003200020916     D DDATIUTE      E DS                                                       *Dati utente
003300020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003400000000     D*------------------------------------------------------------------------*
003500000000     D* DS PER GESTIONE TASTI FUNZIONALI
003600000000     D DSKEYS        E DS
003700000000     D  KEYRIG                 1     40
003800000000     D*------------------------------------------------------------------------*
003900000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004000000000     D                SDS
004100000000     D  USER                 254    263                                         UTENTE
004200000000     D*------------------------------------------------------------------------*
004300000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
004400000000     D DEVDS1          DS
004500000000     D* CHIAVE FUNZIONALE BATTUTA
004600000000     D  KEY                  369    369
004700000000     D* POSIZIONE CURSORE (IN HEX)
004800000000     D  CURSOR               370    371
004900000000     D*------------------------------------------------------------------------*
005000000000     D* ARCHITETTURA
005100000000     D KPJBA         E DS
005200000000     D*------------------------------------------------------------------------*
005300010410     D* DS DATI VARI PER STATISTICHE -DI GRUPPO-
005400010410     D DS5S          E DS
005500980129     D*------------------------------------------------------------------------*
005600990524     D* DS CONTROLLO DATA (8)
005700990524     D WLBDA8          DS                  INZ
005800990524     D  G08DAT                 1      8  0
005900990524     D  G08INV                 9     16  0
006000990524     D  G08ERR                17     17
006100990524     D  G08TGI                18     22  0
006200981117     D*------------------------------------------------------------------------*
006300981117     D* DS DATA ULTIMO AGGIORNAMENTO SALDI
006400981117     D                 DS
006500990524     D  AGGAA                  1      4  0
006600981117     D  AGGMM                  5      6  0
006700990524     D  AGGAM                  1      6  0
006800981117     D  AGGGG                  7      8  0
006900981117     D  WDAC                   1      8  0
007000981117     D*------------------------------------------------------------------------*
007100981117     D* DS DATA ULTIMA CANCELLAZIONE SALDI
007200981117     D                 DS
007300990524     D  CANAA                  1      4  0
007400981117     D  CANMM                  5      6  0
007500990524     D  CANAM                  1      6  0
007600981117     D  CANGG                  7      8  0
007700981117     D  WDCC                   1      8  0
007800000000     D*------------------------------------------------------------------------*
007900000000     D* PARAMETRI LANCIO
008000000000     D PARAM           DS                  INZ
008100990524     D  PARDAT                 1      6  0
008200990524     D  PARDA2                 7     12  0
008300990524     D  PARIOE                13     13
008400000614     D  PARPT                 14     14
008500130130     D  PARTOT                15     15    INZ
008600000000     C*------------------------------------------------------------------------*
008700000000     C* MAIN LINE
008800000000     C*------------------------------------------------------------------------*
008900000000     C*
009000000000     C* CICLO FINO A CHE:
009100000000DO  1C     $FINE         DOUEQ     '1'
009200000000     C*
009300000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009400000000     C*
009500000000     C* CONTROLLA GESTIONE VIDEO
009600000000IF  2C     $GES          IFEQ      'V1'
009700000000     C                   EXSR      GESV1                                        *PARAMETRI
009800000000E   2C                   ENDIF
009900000000     C*
010000000000E   1C                   ENDDO
010100000000     C*
010200000000     C                   SETON                                        LR
010300000000     C*------------------------------------------------------------------------*
010400000000     C* GESV1 - GESIONE PRIMO VIDEO
010500000000     C*------------------------------------------------------------------------*
010600000000     C     GESV1         BEGSR
010700000000     C*
010800000000IF  1C     $TESVD        IFEQ      '1'                                          TESTATA DA RIEMETTERE
010900000000     C                   EXSR      CARV1                                        *CARICA VIDEO
011000000000E   1C                   ENDIF
011100000000     C*
011200000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGGI
011300000000     C                   MOVEL     *BLANKS       $MSG
011400000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011500000000E   1C                   ENDIF
011600000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011700000000     C*
011800010508     C                   EXFMT     SEA1DV1                                      *PARAMETRI
011900000000     C*
012000000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUNZ
012100000000     C*
012200000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012300000000SL  2C                   SELECT
012400000000     C     KEY           WHENEQ    ENTER
012500000000     C                   EXSR      ENTV1                                        *ENTER--> DECODIFICA
012600000000     C     KEY           WHENEQ    F6
012700000000     C                   EXSR      F06GES                                       *F06 --> LANCIO
012800000000     C                   OTHER
012900000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013000000000E   2C                   ENDSL
013100000000E   1C                   ENDIF
013200000000     C*
013300000000     C                   ENDSR
013400000000     C*------------------------------------------------------------------------*
013500000000     C* CARV1 - CARICAMENTO PRIMO VIDEO
013600000000     C*------------------------------------------------------------------------*
013700000000     C     CARV1         BEGSR
013800000000     C*
013900000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014000000000     C*
014100000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014200000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014300000000     C*
014400010508     C                   WRITE     SEA1DVT                                      *TESTATA
014500010508     C                   WRITE     SEA1DVF                                      *FUNZIONI
014600000000     C*
014700000000     C                   ENDSR
014800000000     C*------------------------------------------------------------------------*
014900000000     C* ENTV1 - GESTIONE TASTO "ENTER" PRIMO VIDEO
015000000000     C*------------------------------------------------------------------------*
015100000000     C     ENTV1         BEGSR
015200000000     C*
015300000000     C* CONTROLLA I CAMPI IMMESSI
015400000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
015500000000     C*
015600000000     C                   ENDSR
015700000000     C*------------------------------------------------------------------------*
015800000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
015900000000     C*------------------------------------------------------------------------*
016000000000     C     F06GES        BEGSR
016100000000     C*
016200000000     C* CONTROLLA I CAMPI IMMESSI
016300000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016400000000     C*
016500000000     C* NON CI SONO ERRORI
016600000000IF  1C     $ERR          IFEQ      '0'
016700000000     C*
016800000000     C* LANCIA PROGRAMMA
016900000000     C                   Z-ADD     V1CDAT        PARDAT                         *AA/MM INIZIALE
017000000000     C                   Z-ADD     V1CDA2        PARDA2                         *AA/MM FINALE
017100000000     C                   MOVEL     V1CIOE        PARIOE                         *ITALIA/ESTERO/TUTTO
017200000614     C                   MOVEL     VIDPT         PARPT                          *STATISCTICA X POSTE
017300130208     c                   movel     V1Ctot        ParTOT                         *Stampa solo TOTALI
017400010508     C                   MOVEL     'SEA2'        KCOAZ
017500000000     C                   MOVEL     PARAM         KPJBU
017600000614     C                   CALL      'BCH10'
017700010508 MOM C*                    CALL 'TISEA2R'
017800000000     C                   PARM                    KPJBA
017900000000     C*
018000000000     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
018100000000E   1C                   ENDIF
018200000000     C*
018300000000     C                   ENDSR
018400000000     C*------------------------------------------------------------------------*
018500000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
018600000000     C*------------------------------------------------------------------------*
018700000000     C     DECODI        BEGSR
018800000000     C*
018900000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
019000000000     C                   SETOFF                                       6061
019100000614     C                   SETOFF                                       7172
019200000000     C                   Z-ADD     *ZEROS        RISUL             4 0
019300000000     C*--------------------
019400000000     C* SALDI AGGIORNATI
019500000000     C*--------------------
019600000000     C* NON POSSO LANCIARE LE STATISTICHE SE I SALDI NON SONO AGGIORNATI CON
019700000000     C* RETTIFICHE E NOTE DI ACCREDITO
019800000000IF  1C     $ERR          IFEQ      '0'
019900981117IF  2C     WFSC          IFEQ      'N'                                          *SALDI NON AGGIORNATI
020000000000     C                   MOVEL     '1'           $ERR
020100000000     C                   MOVEL     MSG(22)       $MSG
020200000000     C                   EXSR      CARMSG
020300000000E   2C                   ENDIF
020400000000E   1C                   ENDIF
020500000000     C*--------------------
020600000000     C* ANNO/MESE INIZIALE
020700000000     C*--------------------
020800000000     C* OBBLIGATORIO
020900000000IF  1C     $ERR          IFEQ      '0'
021000000000IF  2C     V1CDAT        IFEQ      *ZEROS
021100000000     C                   MOVEL     '1'           $ERR
021200000000     C                   MOVEL     MSG(7)        $MSG
021300000000     C                   EXSR      CARMSG
021400000000     C                   SETON                                        60
021500000000E   2C                   ENDIF
021600000000E   1C                   ENDIF
021700000000     C* CORRETTO
021800000000IF  1C     $ERR          IFEQ      '0'
021900990524     C                   MOVEL     V1CDAT        G08INV
022000990524     C                   MOVE      01            G08INV
022100990524     C                   Z-ADD     *ZEROS        G08DAT
022200990524     C                   MOVEL     '3'           G08ERR
022300990524     C                   CALL      'XSRDA8'
022400990524     C                   PARM                    WLBDA8
022500990524IF  2C     G08ERR        IFEQ      '1'
022600000000     C                   MOVEL     '1'           $ERR
022700000000     C                   MOVEL     MSG(8)        $MSG
022800000000     C                   EXSR      CARMSG
022900000000     C                   SETON                                        60
023000000000E   2C                   ENDIF
023100000000E   1C                   ENDIF
023200000000     C*--------------------
023300000000     C* ANNO/MESE FINALE
023400000000     C*--------------------
023500000000     C* OBBLIGATORIO
023600000000IF  1C     $ERR          IFEQ      '0'
023700000000IF  2C     V1CDA2        IFEQ      *ZEROS
023800000000     C                   MOVEL     '1'           $ERR
023900000000     C                   MOVEL     MSG(7)        $MSG
024000000000     C                   EXSR      CARMSG
024100000000     C                   SETON                                        61
024200000000E   2C                   ENDIF
024300000000E   1C                   ENDIF
024400000000     C* CORRETTO
024500000000IF  1C     $ERR          IFEQ      '0'
024600990524     C                   MOVEL     V1CDA2        G08INV
024700990524     C                   MOVE      01            G08INV
024800990524     C                   Z-ADD     *ZEROS        G08DAT
024900990524     C                   MOVEL     '3'           G08ERR
025000990524     C                   CALL      'XSRDA8'
025100990524     C                   PARM                    WLBDA8
025200990524IF  2C     G08ERR        IFEQ      '1'
025300000000     C                   MOVEL     '1'           $ERR
025400000000     C                   MOVEL     MSG(8)        $MSG
025500000000     C                   EXSR      CARMSG
025600000000     C                   SETON                                        61
025700000000E   2C                   ENDIF
025800000000E   1C                   ENDIF
025900000000     C*
026000000000     C* ANNO/MESE FINALE SUPERIORE/UGUALE A QUELLO INIZIALE
026100000000IF  1C     $ERR          IFEQ      '0'
026200000000IF  2C     V1CDA2        IFLT      V1CDAT
026300000000     C                   MOVEL     '1'           $ERR
026400000000     C                   MOVEL     MSG(11)       $MSG
026500000000     C                   EXSR      CARMSG
026600000000     C                   SETON                                        61
026700000000E   2C                   ENDIF
026800000000E   1C                   ENDIF
026900000000     C*
027000000000     C* ANNO/MESE INIZIALE SUPERIORE/UGUALE A QUELLO CANCELLAZIONE + 1
027100000000IF  1C     $ERR          IFEQ      '0'
027200990524IF  2C     V1CDAT        IFLT      CANAM
027300000000     C                   MOVEL     '1'           $ERR
027400000000     C                   MOVEL     MSG(21)       $MSG
027500000000     C                   EXSR      CARMSG
027600000000     C                   SETON                                        60
027700000000E   2C                   ENDIF
027800000000E   1C                   ENDIF
027900000000     C*
028000000000     C* RICHIEDERE NON PIU' DI DODICI MESI
028100000000IF  1C     $ERR          IFEQ      '0'
028200000000     C     V1CDA2        SUB       V1CDAT        RISUL
028300000000IF  2C     RISUL         IFGE      100
028400000000     C                   MOVEL     '1'           $ERR
028500000000     C                   MOVEL     MSG(19)       $MSG
028600000000     C                   EXSR      CARMSG
028700000000     C                   SETON                                        6160
028800000000E   2C                   ENDIF
028900000000E   1C                   ENDIF
029000020325     C*
029100020325     C* ANNO INIZIALE DIVERSO DA ANNO FINALE (NN E' + POSSIBILE LANCIARE LA STATISTICA A CAVALLO D'ANNO)
029200020325IF  1C     $ERR          IFEQ      '0'
029300020325     C                   MOVEL     V1CDAT        ANNODA            4 0
029400020325     C                   MOVEL     V1CDA2        ANNOA             4 0
029500020325IF  2C     ANNODA        IFNE      ANNOA
029600020325     C                   MOVEL     '1'           $ERR
029700020325     C                   MOVEL     MSG(24)       $MSG
029800020325     C                   EXSR      CARMSG
029900020325     C                   SETON                                        6061
030000020325E   2C                   ENDIF
030100020325E   1C                   ENDIF
030200000000     C*--------------------
030300000000     C* SELEZIONI TIPO DELTA
030400000000     C*--------------------
030500000000IF  1C     $ERR          IFEQ      '0'
030600000000IF  2C     V1CIOE        IFEQ      'I'
030700000000     C     V1CIOE        OREQ      'E'
030800000000     C     V1CIOE        OREQ      *BLANKS
030900000000X   2C                   ELSE
031000000000     C                   MOVEL     '1'           $ERR
031100000000     C                   MOVEL     MSG(29)       $MSG
031200000000     C                   EXSR      CARMSG
031300000000     C                   SETON                                        72
031400000000E   2C                   ENDIF
031500000000E   1C                   ENDIF
031600000000     C*--------------------
031700000000     C* ANNO/MESE FINALE INFERIORE/UGUALE A QUELLO AGGIORNAMENTO - 1
031800000000     C*--------------------
031900000000     C* SOLO MESSAGGIO DI AVVERTIMENTO PER POTERE ACCODARE I LAVORI IN NOTTE
032000000000IF  1C     $ERR          IFEQ      '0'
032100990524IF  2C     V1CDA2        IFGT      AGGAM
032200000000     C                   MOVEL     MSG(9)        $MSG
032300000000     C                   EXSR      CARMSG
032400000000E   2C                   ENDIF
032500000000E   1C                   ENDIF
032600000000     C*
032700000614     C* CONTROLLO OBBLIGATORIETA' INDICAZIONE FLAG X POSTE
032800000614IF  1C     $ERR          IFEQ      '0'
032900000614IF  2C     VIDPT         IFNE      'S'
033000000614     C     VIDPT         ANDNE     'N'
033100000614     C                   SETON                                        71
033200000614     C                   MOVEL     MSG(23)       $MSG
033300000614     C                   EXSR      CARMSG
033400000614E   2C                   ENDIF
033500000614E   1C                   ENDIF
033600130208      *
033700130208      * CONTROLLO OBBLIGATORIETA' INDICAZIONE FLAG X SOLO TOTALI
033800130208IF  1c     $ERR          ifEQ      '0'
033900130208IF  2c     V1Ctot        ifNE      'S'
034000130208     c     V1Ctot        andNE     'N'
034100130208     c                   eval      *in62 = *on
034200130208     c                   eval      $Msg  = MSG(23)
034300130208     c                   exsr      CarMSG
034400130208E   2c                   endif
034500130208E   1c                   ENDIF
034600000614     C*
034700000000     C                   ENDSR
034800000000     C*------------------------------------------------------------------------*
034900950214     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
035000000000     C*------------------------------------------------------------------------*
035100000000     C     IMPVID        BEGSR
035200000614     C*
035300000614     C* IMPOSTO DI DEFAULT IL FLAG POSTE A 'N' ( = BARTOLINI )
035400000614     C                   MOVEL     'N'           VIDPT
035500130208      * IMPOSTO DI DEFAULT IL FLAG SOLO TOTALI A 'N'
035600130208     c                   movel     'N'           V1Ctot
035700981117     C*
035800010410     C     *LIKE         DEFINE    §5SFSC        WFSC
035900000000     C*--------------------
036000000000     C* RAGIONE SOCIALE
036100000000     C*--------------------
036200020916     C                   MOVEL     RSUT          VTDRSU                         *RAG SOCIALE UTENTE
036300000000     C*--------------------
036400000000     C*                      \  DATA ULTIMO AGGIORNAMENTO
036500000000     C* REPERISCO DA TABELLA: ! DATA ULTIMA CANCELLAZIONE
036600000000     C*                      /  FLAG DI AGGIORNAMENTO
036700000000     C*--------------------
036800020916     C                   Z-ADD     1             KTBKUT
036900010410     C                   MOVEL     '5S'          KTBCOD
037000000000     C     KEYTA2        CHAIN     TABEL00F                           99
037100000000IF  1C     *IN99         IFEQ      *ON                                          *TABELLA NON TROVATA
037200981117     C                   MOVEL     'N'           WFSC                           *NON PERMETTO LANCIO
037300020701     C                   Z-ADD     99999999      WDCC              8 0
037400000000X   1C                   ELSE
037500010410     C                   MOVEL     TBLUNI        DS5S
037600010410     C                   Z-ADD     §5SDAC        WDAC                           *ULTIMO AGGIORNAMENTO
037700010410     C                   MOVEL     §5SFSC        WFSC                           *FLAG AGGIORNAMENTO
037800020701     C                   Z-ADD     §5SDCC        WDCC                           *ULTIMA CANCELLAZIONE
037900000000E   1C                   ENDIF
038000000000     C*
038100000000     C* DATA ULTIMO AGGIORNAMENTO - 1 MESE
038200000000     C                   SUB       1             AGGMM
038300000000IF  1C     AGGMM         IFEQ      *ZEROS                                       *SE GENNAIO
038400000000     C                   Z-ADD     12            AGGMM                          *IMPOSTO DICEMBRE
038500000000     C                   SUB       1             AGGAA                          *DELL'ANNO PRECEDENTE
038600000000E   1C                   ENDIF
038700000000     C*
038800000000     C* DATA ULTIMA CANCELLAZIONE + 1 MESE
038900000000     C                   ADD       1             CANMM
039000000000IF  1C     CANMM         IFEQ      13                                           *SE DICEMBRE
039100000000     C                   Z-ADD     01            CANMM                          *IMPOSTO GENNAIO
039200000000     C                   ADD       1             CANAA                          *DELL'ANNO SUCESSIVO
039300000000E   1C                   ENDIF
039400000000     C*--------------------
039500000000     C* DATE DI ELABORAZIONE
039600000000     C*--------------------
039700000000     C***
039800000000     C* AA/MM FINALE   --> ULTIMO AGGIORNAMENTO - 1 MESE
039900000000     C* AA/MM INIZIALE --> INIZIO ANNO oppure DATA ULTIMA CANCELLAZIONE + 1 MESE
040000000000     C***
040100000000     C*--------------------
040200000000     C* D A T A   F I N E
040300000000     C*--------------------
040400990524     C                   Z-ADD     AGGAM         V1CDA2
040500000000     C*--------------------
040600000000     C* D A T A   I N I Z I O
040700000000     C*--------------------
040800990524     C* CONFRONTO FRA INIZIO ANNO E ULTIMA CANCELLAZIONE
040900990524     C                   Z-ADD     AGGAM         AAMM1             6 0          *INIZIO ANNO
041000990524     C                   MOVE      01            AAMM1
041100990524     C                   Z-ADD     CANAM         AAMM2             6 0          *ULTIMA CANCELLAZIONE
041200000000IF  1C     AAMM2         IFGT      AAMM1
041300990524     C                   Z-ADD     CANAM         V1CDAT
041400000000     C                   ELSE
041500990622     C                   Z-ADD     AAMM1         V1CDAT
041600000000E   1C                   ENDIF
041700000000     C*--------------------
041800000000     C* PARZIALIZZAZIONI
041900000000     C*--------------------
042000000000     C                   MOVEL     *BLANKS       V1CIOE                         DELTA ITALIA/ESTERO
042100000000     C*
042200000000     C                   ENDSR
042300020916     C*--------------------------------------------------------------------------------------------*
042400020916     C* REPERISCE I DATI UTENTE
042500020916     C*--------------------------------------------------------------------------------------------*
042600020916     C     REPDATIUTE    BEGSR
042700020916     C*
042800020916     C* INIZIALIZZA VARIABILI DI WRK
042900020916     C                   CLEAR                   TIBS34DS
043000020916     C                   CLEAR                   AZUTEDS
043100020916     C                   CLEAR                   DDATIUTE
043200020916     C*
043300020916     C     *DTAARA       DEFINE    §azute        azuteds
043400020916     C     *DTAARA       DEFINE    §datiute      ddatiute
043500020916     C                   IN(E)     *DTAARA
043600020916if  1C                   IF        %Error
043700020916     c                   EVAL      I34Tla = 'L'
043800020916     C                   CALL      'TIBS34R'
043900020916     C                   PARM                    Tibs34Ds
044000020916     C                   IN        *DTAARA
044100020916e   1C                   ENDIF
044200020916     C*
044300020916     C                   ENDSR
044400000000     C*------------------------------------------------------------------------*
044500000000     C* *INZSR - ROUTINE INIZIALE
044600000000     C*------------------------------------------------------------------------*
044700000000     C     *INZSR        BEGSR
044800000000     C*
044900000000     C* RICEVIMENTO PARAMETRI
045000000000     C     *ENTRY        PLIST
045100000000     C                   PARM                    KPJBA
045200000000     C*
045300000000     C* DEFINIZIONI VARIABILI CHIAVI DI LETTURA
045400000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
045500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
045600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
045700000000     C*
045800000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
045900000000     C     KEYTAB        KLIST
046000000000     C                   KFLD                    KTBKUT
046100000000     C                   KFLD                    KTBCOD
046200000000     C                   KFLD                    KTBKEY
046300000000     C*
046400000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
046500000000     C     KEYTA2        KLIST
046600000000     C                   KFLD                    KTBKUT
046700000000     C                   KFLD                    KTBCOD
046800000000     C*
046900000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
047000000000     C                   EXSR      INZSTD
047100000000     C*
047200000000     C* IMPOSTAZIONI INIZIALI VIDEO
047300000000     C                   EXSR      IMPVID
047400000000     C*
047500000000     C* EMETTO PRIMO FORMATO VIDEO
047600000000     C                   EXSR      CARV1
047700000000     C*
047800000000     C                   ENDSR
047900000000     C*------------------------------------------------------------------------*
048000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
048100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
048200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
048300000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
048400000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
048500000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
048600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
048700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
048800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
048900000000     C*------------------------------------------------------------------------*
049000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
049100000000     C*------------------------------------------------------------------------*
049200000000     C     INZSTD        BEGSR
049300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
049400000000     C                   CALL      'XKEYS'
049500000000     C                   PARM                    DSKEYS
049600000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ.
049700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
049800000000     C     XMSGPA        PLIST
049900000000     C                   PARM                    DSMSMS
050000000000     C                   PARM                    DSMSPG
050100000000     C                   PARM                    DSMSKE
050200000000     C                   PARM                    DSMSER
050300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
050400010508     C                   MOVEL     'TISEA1R'     DSMSPG                         *NOME PGM CHIAMANTE
050500010508     C                   MOVEL     'TISEA1R'     VMCQUE                         *CODA MESSAGGI DESTIN
050600000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
050700000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
050800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
050900000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
051000000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
051100000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
051200000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
051300000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
051400000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDEO
051500000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
051600000000     C* LANCIO ROUTINE PER VERIFICARE SE
051700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
051800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
051900020916     C                   EXSR      REPDATIUTE
052000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
052100000000     C                   BITOFF    '01234567'    POSMI1            1
052200000000     C                   BITOFF    '01234567'    POSMI2            1
052300000000     C                   BITOFF    '01234567'    POSMA1            1
052400000000     C                   BITOFF    '01234567'    POSMA2            1
052500000000     C                   BITON     '34'          POSMI1            1            *18
052600000000     C                   BITON     '7'           POSMI2            1            *01
052700000000     C                   BITON     '34'          POSMA1            1            *18
052800000000     C                   BITON     '0'           POSMA2            1            *80
052900000000     C                   MOVE      POSMI2        POSMIN            2
053000000000     C                   MOVEL     POSMI1        POSMIN
053100000000     C                   MOVE      POSMA2        POSMAX            2
053200000000     C                   MOVEL     POSMA1        POSMAX
053300000000     C                   ENDSR
053400000000     C*------------------------------------------------------------------------*
053500000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
053600000000     C*------------------------------------------------------------------------*
053700000000     C     FUNSTD        BEGSR
053800000000     C                   SELECT
053900000000     C     KEY           WHENEQ    F3
054000000000     C                   EXSR      F03STD
054100000000     C     KEY           WHENEQ    F12
054200000000     C                   EXSR      F12STD
054300000000     C     KEY           WHENEQ    F24
054400000000     C                   EXSR      F24STD
054500000000     C                   OTHER
054600000000     C                   MOVEL     MSG(2)        $MSG
054700000000     C                   EXSR      CARMSG
054800000000     C                   ENDSL
054900000000     C                   ENDSR
055000000000     C*------------------------------------------------------------------------*
055100000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
055200000000     C*------------------------------------------------------------------------*
055300000000     C     F03STD        BEGSR
055400000000     C*
055500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
055600000000     C                   MOVEL     '1'           $FINE
055700000000     C*
055800000000     C                   ENDSR
055900000000     C*------------------------------------------------------------------------*
056000000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
056100000000     C*------------------------------------------------------------------------*
056200000000     C     F12STD        BEGSR
056300000000     C*
056400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
056500000000     C                   MOVEL     '1'           $FINE
056600000000     C*
056700000000     C                   ENDSR
056800000000     C*------------------------------------------------------------------------*
056900000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
057000000000     C*------------------------------------------------------------------------*
057100000000     C     F24STD        BEGSR
057200000000     C*
057300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
057400000000     C                   EXSR      OUTFUN
057500000000     C*
057600000000     C                   ENDSR
057700000000     C*------------------------------------------------------------------------*
057800000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
057900000000     C*          CODA MESSAGGI DEL PGM
058000000000     C*------------------------------------------------------------------------*
058100000000     C     CARMSG        BEGSR
058200000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
058300000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
058400000000     C     MSGULT        IFEQ      0
058500000000     C                   MOVEL     '1'           *IN40
058600010508     C                   WRITE     SEA1DVK
058700000000     C                   MOVEL     '0'           *IN40
058800000000     C                   ENDIF
058900000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
059000000000     C                   MOVEL     $MSG          DSMSMS
059100000000     C                   CALL      'XMSG'        XMSGPA
059200000000     C     DSMSER        IFEQ      0
059300000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGIO
059400000000     C                   ENDIF
059500000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
059600000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
059700010508     C                   WRITE     SEA1DVM                                      *SFL MESS
059800000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
059900000000     C                   MOVEL     *BLANKS       $MSG
060000000000     C                   ENDSR
060100000000     C*------------------------------------------------------------------------*
060200000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
060300000000     C*------------------------------------------------------------------------*
060400000000     C     OUTMSG        BEGSR
060500000000     C* VISUALIZZAZIONE MESSAGGIO
060600000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
060700010508     C                   WRITE     SEA1DVK                                      *CONTR MESS
060800000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
060900000000     C                   Z-ADD     *ZEROS        MSGULT
061000000000     C                   ENDSR
061100000000     C*------------------------------------------------------------------------*
061200000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
061300000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
061400000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
061500000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
061600000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
061700000000     C*------------------------------------------------------------------------*
061800000000     C     CARFUN        BEGSR
061900000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL.
062000000000     C     1             DO        31            I                 5 0          --- 1 -->
062100000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
062200000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
062300000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
062400020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
062500000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
062600000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
062700000000     C                   ELSE                                                   *INTERROG.
062800000000     C                   MOVEL     FUNASI        FAT(I)
062900000000     C                   ENDIF
063000000000     C                   ELSE                                                   <-- 2 --> *FILIALE
063100000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
063200000000     C                   MOVEL     FUNAFG        FAT(I)
063300000000     C                   ELSE                                                   *INTERROG.
063400000000     C                   MOVEL     FUNAFI        FAT(I)
063500000000     C                   ENDIF
063600000000     C                   ENDIF                                                  <-- 2 ---
063700000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
063800000000     C                   MOVEL     USER          WEDP              3            *PROFILO
063900000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
064000000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
064100000000     C                   ELSE
064200000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
064300000000     C                   ENDIF
064400000000     C                   ENDIF
064500000000     C                   ENDDO                                                  <-- 1 ---
064600000000     C                   ENDSR
064700000000     C*------------------------------------------------------------------------*
064800000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
064900000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
065000000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
065100000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
065200000000     C*------------------------------------------------------------------------*
065300000000     C     OUTFUN        BEGSR
065400000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
065500000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
065600000000     C                   ENDDO
065700000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
065800000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
065900000000     C     1             DO        31            I
066000000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
066100000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZIONE
066200000000     C                   ADD       1             FUNNAT
066300000000     C                   ENDIF
066400000000     C                   ENDDO
066500000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
066600000000     C     FUNNAT        IFGT      8
066700000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
066800000000     C                   ENDIF
066900000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
067000000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
067100000000     C                   Z-ADD     8             FUNVIS            5 0
067200000000     C     FAT(23)       IFEQ      'S'
067300000000     C                   SUB       1             FUNVIS
067400000000     C                   ENDIF
067500000000     C     FAT(24)       IFEQ      'S'
067600000000     C                   SUB       1             FUNVIS
067700000000     C                   ENDIF
067800000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
067900000000     C                   Z-ADD     *ZEROS        W                 5 0
068000000000     C                   ADD       1             FUNULT
068100000000     C     FUNULT        IFGT      31
068200000000     C                   Z-ADD     1             FUNULT
068300000000     C                   ENDIF
068400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
068500000000     C     FUNULT        DO        31            I                              --- 1 -->
068600000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
068700000000     C     W             IFLT      FUNVIS                                       --- 2 -->
068800000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
068900000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
069000000000     C     FDE(I)        ANDNE     *BLANKS
069100000000     C     I             IFEQ      23                                           *TOLGO F23
069200000000     C     I             OREQ      24                                           *E F24
069300000000     C                   ELSE
069400000000     C                   ADD       1             W
069500000000     C                   MOVEL     FDE(I)        FUV(W)
069600000000     C                   ENDIF
069700000000     C                   ENDIF                                                  <-- 3 ---
069800000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
069900000000     C                   Z-ADD     I             FUNULT
070000000000     C                   ENDIF                                                  <-- 2 ---
070100000000     C                   ENDDO                                                  <-- 1 ---
070200000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
070300000000     C                   MOVEL     FUV(1)        VFD001
070400000000     C                   MOVEL     FUV(2)        VFD002
070500000000     C                   MOVEL     FUV(3)        VFD003
070600000000     C                   MOVEL     FUV(4)        VFD004
070700000000     C                   MOVEL     FUV(5)        VFD005
070800000000     C                   MOVEL     FUV(6)        VFD006
070900000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
071000000000     C     FAT(24)       ANDEQ     'N'
071100000000     C                   MOVEL     FUV(7)        VFD007
071200000000     C                   MOVEL     FUV(8)        VFD008
071300000000     C                   ELSE                                                   <-- 1 -->
071400000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
071500000000     C     FAT(24)       ANDEQ     'S'
071600000000     C                   MOVEL     FDE(23)       VFD007
071700000000     C                   MOVEL     FDE(24)       VFD008
071800000000     C                   ELSE                                                   <-- 2 -->
071900000000     C                   MOVEL     FUV(7)        VFD007
072000000000     C     FAT(23)       IFEQ      'S'
072100000000     C                   MOVEL     FDE(23)       VFD008
072200000000     C                   ELSE
072300000000     C                   MOVEL     FDE(24)       VFD008
072400000000     C                   ENDIF
072500000000     C                   ENDIF                                                  <-- 2 ---
072600000000     C                   ENDIF                                                  <-- 1 ---
072700000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TESTA
072800000000     C                   ENDSR
072900000000     C*------------------------------------------------------------------------*
073000000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
073100000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
073200000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
073300000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
073400000000     C*------------------------------------------------------------------------*
073500000000     C     CONFUN        BEGSR
073600000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
073700000000     C     1             DO        31            I                              --- 1 -->
073800000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
073900000000     C* CONTROLLO SE IL TASTO E' ATTIVO
074000000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
074100000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABILE
074200000000     C                   EXSR      CARMSG
074300000000     C                   MOVEL     '1'           $ERR
074400000000     C                   ENDIF                                                  <-- 3 ---
074500000000     C                   ENDIF                                                  <-- 2 ---
074600000000     C                   ENDDO                                                  <-- 1 ---
074700000000     C                   ENDSR
074800000000**   SCHIERA: MSG  (MESSAGGI)
074900010508TISEA1R- Tasto funzionale non valido.                                          1
075000010508TISEA1R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
075100010508TISEA1R- Opzione non valida.                                                   3
075200010508TISEA1R- Opzione abilitata ma non gestita. Avvertire CED.                      4
075300010508TISEA1R- Inizio elenco.                                                        5
075400010508TISEA1R- Fine elenco.                                                          6
075500010508TISEA1R- Anno/Mese obbligatorio                                                7
075600010508TISEA1R- Anno/Mese errato                                                      8
075700010508TISEA1R- Data superiore ultimo agg.to: lanciare aggiornamento da contabilità   9
075800010508TISEA1R- 10
075900010508TISEA1R- Anno/Mese finale inferiore all'Anno/Mese iniziale                    11
076000010508TISEA1R- Codice filiale errato o inesistente                                  12
076100010508TISEA1R- Codice area errato o inesistente                                     13
076200010508TISEA1R- Codice divisione errato o inesistente                                14
076300010508TISEA1R- Non si possono lanciare le statistiche di gruppo in SDI !!!          15
076400010508TISEA1R- 16
076500010508TISE90R- Immettere solo una selezione per filiale, area o divisione           17
076600010508TISEA1R- 18
076700010508TISEA1R- Impossibile richiedere più di 12 mesi                                19
076800010508TISEA1R- 20
076900010508TISEA1R- Saldi non presenti: data inferiore all'ultima cancellazione          21
077000010508TISEA1R- Saldi cliente non aggiornati: impossibile lanciare questa statistica 22
077100010508TISEA1R- Opzione obbligatoria.                                                23
077200020325TISEA1R- Impossibile lanciare la statistica su anni differenti                24
077300010508TISEA1R- 25
077400010508TISEA1R- 26
077500010508TISEA1R- 27
077600010508TISEA1R- 28
077700010508TISEA1R- 'I' per elaborare le bolle Italia, 'E' per le estere, ' ' per tutte  29
077800010508TISEA1R- 30
077900000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
078000000000F1    -                   -    NN     NN
078100000000F2    -                   -    NN     NN
078200000000F3    -F3=Fine            -    SS     SS
078300000000F4    -                   -    NN     NN
078400000000F5    -                   -    NN     NN
078500000000F6    -F6=Lancio          -    SS     SS
078600000000F7    -                   -    NN     NN
078700000000F8    -                   -    NN     NN
078800000000F9    -                   -    NN     NN
078900000000F10   -                   -    NN     NN
079000000000F11   -                   -    NN     NN
079100000000F12   -F12=Ritorno        -    SS     SS
079200000000F13   -                   -    NN     NN
079300000000F14   -                   -    NN     NN
079400000000F15   -                   -    NN     NN
079500000000F16   -                   -    NN     NN
079600000000F17   -                   -    NN     NN
079700000000F18   -                   -    NN     NN
079800000000F19   -                   -    NN     NN
079900000000F20   -                   -    NN     NN
080000000000F21   -                   -    NN     NN
080100000000F22   -                   -    NN     NN
080200000000F23   -                   -    NN     NN
080300000000F24   -F24=Altri tasti    -    NN     NN
080400000000ENTER -                   -    SS     SS
080500000000ROLLUP-                   -    NN     NN
080600000000ROLLDN-                   -    NN     NN
080700000000HELP  -                   -    NN     NN
080800000000PRINT -                   -    NN     NN
080900000000BSPACE-                   -    NN     NN
081000000000CLEAR -                   -    NN     NN
