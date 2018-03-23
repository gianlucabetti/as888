000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICA TRAINI - RICHIESTA PARAMETRI                                *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNSS74D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(SS74DVM:MSGULT)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*-------------------
001400000000     D* MESSAGGI E FUNZIONI
001500000000     D*-------------------
001600000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001700000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001800000000     D*-------------------
001900000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002000000000     D*-------------------
002100000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002200000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002300000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002400000000     D*-------------------
002500000000     D* TABELLA DESCRIZIONI A VIDEO
002600000000     D*-------------------
002700000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002800000000     D*-------------------
002900000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003000000000     D*-------------------
003100000000     D KEYS            S              1    DIM(40)
003200000000     D*------------------------------------------------------------------------*
003300000000     D* INPUT
003400000000     D*------------------------------------------------------------------------*
003500000000     D*-------------------
003600000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003700000000     D*-------------------
003800000000     D DSFUN         E DS
003900000000     D  FUNRIG                 1     40
004000000000     D*-------------------
004100000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004200000000     D*-------------------
004300000000     D DSMSG         E DS
004400000000     D*-------------------
004500000000     D* DS PER GESTIONE TASTI FUNZIONALI
004600000000     D*-------------------
004700000000     D DSKEYS        E DS
004800000000     D  KEYRIG                 1     40
004900000000     D*-------------------
005000000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005100000000     D*-------------------
005200000000     D                SDS
005300000000     D  USER                 254    263
005400000000     D*-------------------
005500000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005600000000     D*-------------------
005700000000     D DEVDS1          DS
005800000000     D  KEY                  369    369
005900000000     D  CURSOR               370    371
006000000000     D*-------------------
006100000000     D* ARCHITETTURA
006200000000     D*-------------------
006300000000     D KPJBA         E DS
006400000000     D*-------------------
006500020920     D* DS REPERIMENTO DATI UTENTE
006600020920     D*---------------------
006700020920     D TIBS34DS      E DS                                                       *Profili utente
006800020920     D DDATIUTE      E DS                                                       *Dati utente
006900020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007000980403     D*-------------------
007100000000     D* DS PARAMETRI LANCIO
007200000000     D*-------------------
007300000000     D PARAM           DS                  INZ
007400990603     D  PARDAT                 1      6  0
007500990603     D  PARDA2                 7     12  0
007600990603     D  PARMPA                13     13
007700990603     D  PARFPC                14     14
007800990603     D  PARCRE                15     15
007900000706     D  PARPOS                16     16
008000990603     D*-------------------
008100990603     D* DS CONTROLLO DATA (8)
008200990603     D*-------------------
008300990603     D WLBDA8          DS                  INZ
008400990603     D  G08DAT                 1      8  0
008500990603     D  G08INV                 9     16  0
008600990603     D  G08ERR                17     17
008700990603     D  G08TGI                18     22  0
008800990610     D*------------------
008900990610     D* DS DATA GENERICA(8) IN GG/MM/AAAA
009000990610     D*------------------
009100990610     D                 DS
009200990610     D  DGIOR8                 1      2  0
009300990610     D  DMESE8                 3      4  0
009400990610     D  DANNO8                 5      8  0
009500990610     D  DDATA8                 1      8  0
009600000000     C*------------------------------------------------------------------------*
009700000000     C* MAIN LINE
009800000000     C*------------------------------------------------------------------------*
009900000000     C*
010000000000     C* CICLO FINO A CHE:
010100000000DO  1C     $FINE         DOUEQ     '1'
010200000000     C*
010300000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010400000000     C*
010500000000     C* CONTROLLA GESTIONE VIDEO
010600000000IF  2C     $GES          IFEQ      'V1'
010700000000     C                   EXSR      GESV1                                        *PARAMETRI
010800000000E   2C                   ENDIF
010900000000     C*
011000000000E   1C                   ENDDO
011100000000     C*
011200000000     C                   SETON                                        LR
011300000000     C*------------------------------------------------------------------------*
011400000000     C* GESV1 - GESTIONE VIDEO UNO
011500000000     C*------------------------------------------------------------------------*
011600000000     C     GESV1         BEGSR
011700000000     C*
011800000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
011900000000     C                   EXSR      CARV1                                        *CARICA VIDEO
012000000000E   1C                   ENDIF
012100000000     C*
012200000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012300000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012400000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012500000000E   1C                   ENDIF
012600000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012700000000     C*
012800000000     C                   EXFMT     SS74DV1                                      *PARAMETRI
012900000000     C*
013000000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013100000000     C*
013200000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013300000000SL  2C                   SELECT
013400000000     C     KEY           WHENEQ    ENTER
013500000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013600000000     C     KEY           WHENEQ    F6
013700000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
013800000000     C                   OTHER
013900000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
014000000000E   2C                   ENDSL
014100000000E   1C                   ENDIF
014200000000     C*
014300000000     C                   ENDSR
014400000000     C*------------------------------------------------------------------------*
014500000000     C* CARV1 - CARICA VIDEO UNO
014600000000     C*------------------------------------------------------------------------*
014700000000     C     CARV1         BEGSR
014800000000     C*
014900000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015000000000     C*
015100000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015200000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015300000000     C*
015400000000     C                   WRITE     SS74DVT                                      *TESTATA
015500000000     C                   WRITE     SS74DVF                                      *FUNZIONI
015600000000     C*
015700000000     C                   ENDSR
015800000000     C*------------------------------------------------------------------------*
015900000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
016000000000     C*------------------------------------------------------------------------*
016100000000     C     ENTV1         BEGSR
016200000000     C*
016300000000     C* CONTROLLA I CAMPI IMMESSI
016400000000     C                   EXSR      DECODI
016500000000     C*
016600000000     C                   ENDSR
016700000000     C*------------------------------------------------------------------------*
016800000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
016900000000     C*------------------------------------------------------------------------*
017000000000     C     F06GES        BEGSR
017100000000     C*
017200000000     C* CONTROLLA I CAMPI IMMESSI
017300000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
017400000000     C*
017500000000     C* LANCIA IL PROGRAMMA DI STAMPA
017600000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
017700000000     C*
017800000000     C                   Z-ADD     V1CDAT        PARDAT                         *AA/MM INIZIALE
017900000000     C                   Z-ADD     V1CDA2        PARDA2                         *AA/MM INIZIALE
018000000000     C                   MOVEL     V1CMPA        PARMPA                         *TIPO STATISTICA
018100000000     C                   MOVEL     V1CFPC        PARFPC                         *CREAZIONE FILE PC
018200000000     C                   MOVEL     V1CCRE        PARCRE                         *CREAZIONE WORKFILE
018300000706     C                   MOVEL     V1CPOS        PARPOS                         *DATI POSTE
018400000000     C                   MOVEL     PARAM         KPJBU
018500000000     C                   MOVE      'SS75'        KCOAZ
018600000000     C                   MOVEL     PARAM         KPJBU
018700000000     C                   CALL      'BCH10'
018800000000     C                   PARM                    KPJBA
018900000000     C*
019000000000     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
019100000000E   1C                   ENDIF
019200000000     C*
019300000000     C                   ENDSR
019400000000     C*------------------------------------------------------------------------*
019500000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
019600000000     C*------------------------------------------------------------------------*
019700000000     C     DECODI        BEGSR
019800000000     C*
019900000000     C* IMPOSTA INDICATORI DI ERRORE SUI CAMPI
020000000000     C                   SETOFF                                       606162
020100000706     C                   SETOFF                                       636465
020200000000     C                   Z-ADD     *ZEROS        RISUL             4 0
020300000000     C*--------------------
020400000000     C* ANNO/MESE INIZIALE
020500000000     C*--------------------
020600000000IF  1C     $ERR          IFEQ      '0'
020700990603     C                   MOVEL     V1CDAT        G08INV
020800990603     C                   MOVE      01            G08INV
020900990603     C                   Z-ADD     *ZEROS        G08DAT
021000990603     C                   MOVEL     '3'           G08ERR
021100990603     C                   CALL      'XSRDA8'
021200990610     C                   PARM                    WLBDA8
021300990603IF  2C     G08ERR        IFEQ      '1'
021400000000     C                   MOVEL     '1'           $ERR
021500000000     C                   MOVEL     MSG(7)        $MSG
021600000000     C                   EXSR      CARMSG
021700000000     C                   SETON                                        60
021800000000E   2C                   ENDIF
021900000000E   1C                   ENDIF
022000000000     C*--------------------
022100000000     C* ANNO/MESE FINALE
022200000000     C*--------------------
022300000000IF  1C     $ERR          IFEQ      '0'
022400990603     C                   MOVEL     V1CDA2        G08INV
022500990603     C                   MOVE      01            G08INV
022600990603     C                   Z-ADD     *ZEROS        G08DAT
022700990603     C                   MOVEL     '3'           G08ERR
022800990603     C                   CALL      'XSRDA8'
022900990610     C                   PARM                    WLBDA8
023000990603IF  2C     G08ERR        IFEQ      '1'
023100000000     C                   MOVEL     '1'           $ERR
023200000000     C                   MOVEL     MSG(8)        $MSG
023300000000     C                   EXSR      CARMSG
023400000000     C                   SETON                                        61
023500000000E   2C                   ENDIF
023600000000E   1C                   ENDIF
023700000000     C*
023800000000     C* ANNO/MESE FINALE INFERIORE/UGUALE A QUELLO CORRENTE (SOLO INFORMATIVO)
023900000000IF  1C     $ERR          IFEQ      '0'
024000000000IF  2C     V1CDA2        IFGT      AMCOR
024100000000     C                   MOVEL     MSG(11)       $MSG
024200000000     C                   EXSR      CARMSG
024300000000E   2C                   ENDIF
024400000000E   1C                   ENDIF
024500000000     C*--------------------
024600000000     C* ANNO/MESE INIZIALE E FINALE
024700000000     C*--------------------
024800000000     C* ANNO/MESE FINALE SUPERIORE/UGUALE A QUELLO INIZIALE
024900000000IF  1C     $ERR          IFEQ      '0'
025000000000IF  2C     V1CDA2        IFLT      V1CDAT
025100000000     C                   MOVEL     '1'           $ERR
025200000000     C                   MOVEL     MSG(9)        $MSG
025300000000     C                   EXSR      CARMSG
025400000000     C                   SETON                                        61
025500000000E   2C                   ENDIF
025600000000E   1C                   ENDIF
025700000000     C*
025800000000     C* RICHIEDERE NON PIU' DI DODICI MESI
025900000000IF  1C     $ERR          IFEQ      '0'
026000000000     C     V1CDA2        SUB       V1CDAT        RISUL
026100000000IF  2C     RISUL         IFGE      100
026200000000     C                   MOVEL     '1'           $ERR
026300000000     C                   MOVEL     MSG(10)       $MSG
026400000000     C                   EXSR      CARMSG
026500000000     C                   SETON                                        6061
026600000000E   2C                   ENDIF
026700000000E   1C                   ENDIF
026800020326     C*
026900020326     C* ANNO INIZIALE DIVERSO DA ANNO FINALE (NN E' + POSSIBILE LANCIARE LA STATISTICA A CAVALLO D'A
027000020326IF  1C     $ERR          IFEQ      '0'
027100020326     C                   MOVEL     V1CDAT        ANNODA            4 0
027200020326     C                   MOVEL     V1CDA2        ANNOA             4 0
027300020326IF  2C     ANNODA        IFNE      ANNOA
027400020326     C                   MOVEL     '1'           $ERR
027500020326     C                   MOVEL     MSG(16)       $MSG
027600020326     C                   EXSR      CARMSG
027700020326     C                   SETON                                        6061
027800020326E   2C                   ENDIF
027900020326E   1C                   ENDIF
028000000000     C*--------------------
028100000000     C* CRAZIONE FILE PC
028200000000     C*--------------------
028300000000IF  1C     $ERR          IFEQ      '0'
028400000000IF  2C     V1CFPC        IFEQ      'S'
028500000000     C     V1CFPC        OREQ      'N'
028600000000X   2C                   ELSE
028700000000     C                   MOVEL     '1'           $ERR
028800000000     C                   MOVEL     MSG(13)       $MSG
028900000000     C                   EXSR      CARMSG
029000000000     C                   SETON                                        63
029100000000E   2C                   ENDIF
029200000000E   1C                   ENDIF
029300000706     C*--------------------
029400000706     C* CRAZIONE WORK FILE
029500000706     C*--------------------
029600000706IF  1C     $ERR          IFEQ      '0'
029700000706IF  2C     V1CCRE        IFEQ      'S'
029800000706     C     V1CCRE        OREQ      'N'
029900000706X   2C                   ELSE
030000000706     C                   MOVEL     '1'           $ERR
030100000706     C                   MOVEL     MSG(14)       $MSG
030200000706     C                   EXSR      CARMSG
030300000706     C                   SETON                                        64
030400000706E   2C                   ENDIF
030500000706E   1C                   ENDIF
030600000706     C*--------------------
030700000706     C* TIPO STATISTICA
030800000706     C*--------------------
030900000706IF  1C     $ERR          IFEQ      '0'
031000000706IF  2C     V1CMPA        IFEQ      'P'
031100000706     C     V1CMPA        OREQ      'A'
031200000706X   2C                   ELSE
031300000706     C                   MOVEL     '1'           $ERR
031400000706     C                   MOVEL     MSG(12)       $MSG
031500000706     C                   EXSR      CARMSG
031600000706     C                   SETON                                        62
031700000706E   2C                   ENDIF
031800000706E   1C                   ENDIF
031900000706     C*--------------------
032000000706     C* DATI POSTE
032100000706     C*--------------------
032200000706IF  1C     $ERR          IFEQ      '0'
032300000706IF  2C     V1CPOS        IFEQ      'S'
032400000706     C     V1CPOS        OREQ      'N'
032500000706X   2C                   ELSE
032600000706     C                   MOVEL     '1'           $ERR
032700000706     C                   MOVEL     MSG(12)       $MSG
032800000706     C                   EXSR      CARMSG
032900000706     C                   SETON                                        65
033000000706E   2C                   ENDIF
033100000706E   1C                   ENDIF
033200000706     C*
033300000000     C                   ENDSR
033400000000     C*------------------------------------------------------------------------*
033500000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
033600000000     C*------------------------------------------------------------------------*
033700000000     C     IMPVID        BEGSR
033800990604     C*--------------------
033900990604     C* D A T A  F I N E --> ANNO/MESE CORRENTE - 1 MESE
034000990604     C*--------------------
034100990604     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
034200990604     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
034300990604     C                   Z-ADD     N8            DDATA8                         *SCOMPOSIZIONE DATA
034400990604     C*
034500990604     C                   Z-ADD     DANNO8        ANNO              4 0
034600990604     C                   Z-ADD     DMESE8        MESE              2 0
034700990604     C                   SUB       1             MESE
034800990604IF  1C     MESE          IFEQ      *ZEROS                                       *SE GENNAIO
034900990604     C                   Z-ADD     12            MESE                           *IMPOSTO DICEMBRE
035000990604     C                   SUB       1             ANNO                           *DELL'ANNO PRECEDENT
035100990604E   1C                   ENDIF
035200990604     C                   MOVEL     ANNO          V1CDA2
035300990604     C                   MOVE      MESE          V1CDA2
035400990604     C                   Z-ADD     V1CDA2        AMCOR             6 0          *ANN/MESE CORRENTE
035500990604     C*--------------------
035600990604     C* D A T A  I N I Z I O --> INIIZO DELL'ANNO
035700990604     C*--------------------
035800990604     C                   Z-ADD     V1CDA2        V1CDAT
035900990604     C                   MOVE      01            V1CDAT
036000990610     C*--------------------
036100990610     C* ALTRE SELEZIONI
036200990610     C*--------------------
036300000000     C                   MOVEL     'S'           V1CFPC                         *CREAZIONE FILE SU P
036400000000     C                   MOVEL     'N'           V1CCRE                         *CREAZIONE WORKFILE
036500000000     C                   MOVEL     'P'           V1CMPA                         *TIPO STATISTICA
036600000706     C                   MOVEL     'N'           V1CPOS                         *DATI POSTE
036700000706     C*
036800000000     C                   ENDSR
036900020920     C*--------------------------------------------------------------------------------------------*
037000020920     C* REPERISCE I DATI UTENTE
037100020920     C*--------------------------------------------------------------------------------------------*
037200020920     C     REPDATIUTE    BEGSR
037300020920     C*
037400020920     C* INIZIALIZZA VARIABILI DI WRK
037500020920     C                   CLEAR                   TIBS34DS
037600020920     C                   CLEAR                   AZUTEDS
037700020920     C                   CLEAR                   DDATIUTE
037800020920     C*
037900020920     C     *DTAARA       DEFINE    §azute        azuteds
038000020920     C     *DTAARA       DEFINE    §datiute      ddatiute
038100020920     C                   IN(E)     *DTAARA
038200020920if  1C                   IF        %Error
038300020920     c                   EVAL      I34Tla = 'L'
038400020920     C                   CALL      'TIBS34R'
038500020920     C                   PARM                    Tibs34Ds
038600020920     C                   IN        *DTAARA
038700020920e   1C                   ENDIF
038800020920     C*
038900020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
039000020920     C                   MOVEL(P)  RSUT          DSFIRS           20
039100020920     C*
039200020920     C                   ENDSR
039300000000     C*------------------------------------------------------------------------*
039400000000     C* *INZSR  - ROUTINE INIZIALE
039500000000     C*------------------------------------------------------------------------*
039600000000     C     *INZSR        BEGSR
039700000000     C*--------------------
039800000000     C* RICEVIMENTO PARAMETRI
039900000000     C*--------------------
040000000000     C     *ENTRY        PLIST
040100000000     C                   PARM                    KPJBA
040200000000     C*
040300000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
040400000000     C                   EXSR      INZSTD
040500000000     C*
040600000000     C* IMPOSTAZIONE INIZIALE VIDEO
040700000000     C                   EXSR      IMPVID
040800000000     C*
040900000000     C* EMISSIONE FORMATO VIDEO
041000000000     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
041100000000     C*
041200000000     C                   ENDSR
041300000000     C*------------------------------------------------------------------------*
041400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
041500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
041600000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
041700000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
041800000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
041900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
042000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
042100000000     C*------------------------------------------------------------------------*
042200000000     C* INZSTD - ROUTINE INIZIALE STANDARD
042300000000     C*------------------------------------------------------------------------*
042400000000     C     INZSTD        BEGSR
042500000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
042600000000     C                   CALL      'XKEYS'
042700000000     C                   PARM                    DSKEYS
042800000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
042900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
043000000000     C     XMSGPA        PLIST
043100000000     C                   PARM                    DSMSMS
043200000000     C                   PARM                    DSMSPG
043300000000     C                   PARM                    DSMSKE
043400000000     C                   PARM                    DSMSER
043500000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
043600000000     C                   MOVEL     'TNSS74R'     DSMSPG                         *NOME PGM CHIAMANTE
043700000000     C                   MOVEL     'TNSS74R'     VMCQUE                         *CODA MESSAGGI DESTI
043800000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
043900000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
044000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
044100000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
044200000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
044300000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
044400000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
044500000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
044600000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
044700000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
044800000000     C* LANCIO ROUTINE PER VERIFICARE SE
044900000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
045000000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
045100020920     C                   EXSR      REPDATIUTE
045200000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
045300000000     C                   BITOFF    '01234567'    POSMI1            1
045400000000     C                   BITOFF    '01234567'    POSMI2            1
045500000000     C                   BITOFF    '01234567'    POSMA1            1
045600000000     C                   BITOFF    '01234567'    POSMA2            1
045700000000     C                   BITON     '34'          POSMI1            1            *18
045800000000     C                   BITON     '7'           POSMI2            1            *01
045900000000     C                   BITON     '34'          POSMA1            1            *18
046000000000     C                   BITON     '0'           POSMA2            1            *80
046100000000     C                   MOVE      POSMI2        POSMIN            2
046200000000     C                   MOVEL     POSMI1        POSMIN
046300000000     C                   MOVE      POSMA2        POSMAX            2
046400000000     C                   MOVEL     POSMA1        POSMAX
046500000000     C                   ENDSR
046600000000     C*------------------------------------------------------------------------*
046700000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
046800000000     C*------------------------------------------------------------------------*
046900000000     C     FUNSTD        BEGSR
047000000000     C                   SELECT
047100000000     C     KEY           WHENEQ    F3
047200000000     C                   EXSR      F03STD
047300000000     C     KEY           WHENEQ    F12
047400000000     C                   EXSR      F12STD
047500000000     C     KEY           WHENEQ    F24
047600000000     C                   EXSR      F24STD
047700000000     C                   OTHER
047800000000     C                   MOVEL     MSG(2)        $MSG
047900000000     C                   EXSR      CARMSG
048000000000     C                   ENDSL
048100000000     C                   ENDSR
048200000000     C*------------------------------------------------------------------------*
048300000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
048400000000     C*------------------------------------------------------------------------*
048500000000     C     F03STD        BEGSR
048600000000     C*
048700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
048800000000     C                   MOVEL     '1'           $FINE
048900000000     C*
049000000000     C                   ENDSR
049100000000     C*------------------------------------------------------------------------*
049200000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
049300000000     C*------------------------------------------------------------------------*
049400000000     C     F12STD        BEGSR
049500000000     C*
049600000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
049700000000     C                   MOVEL     '1'           $FINE
049800000000     C*
049900000000     C                   ENDSR
050000000000     C*------------------------------------------------------------------------*
050100000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
050200000000     C*------------------------------------------------------------------------*
050300000000     C     F24STD        BEGSR
050400000000     C*
050500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
050600000000     C                   EXSR      OUTFUN
050700000000     C*
050800000000     C                   ENDSR
050900000000     C*------------------------------------------------------------------------*
051000000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
051100000000     C*          CODA MESSAGGI DEL PGM
051200000000     C*------------------------------------------------------------------------*
051300000000     C     CARMSG        BEGSR
051400000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
051500000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
051600000000     C     MSGULT        IFEQ      0
051700000000     C                   MOVEL     '1'           *IN40
051800000000     C                   WRITE     SS74DVK
051900000000     C                   MOVEL     '0'           *IN40
052000000000     C                   ENDIF
052100000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
052200000000     C                   MOVEL     $MSG          DSMSMS
052300000000     C                   CALL      'XMSG'        XMSGPA
052400000000     C     DSMSER        IFEQ      0
052500000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
052600000000     C                   ENDIF
052700000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
052800000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
052900000000     C                   WRITE     SS74DVM                                      *SFL MESS
053000000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
053100000000     C                   MOVEL     *BLANKS       $MSG
053200000000     C                   ENDSR
053300000000     C*------------------------------------------------------------------------*
053400000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
053500000000     C*------------------------------------------------------------------------*
053600000000     C     OUTMSG        BEGSR
053700000000     C* VISUALIZZAZIONE MESSAGGIO
053800000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
053900000000     C                   WRITE     SS74DVK                                      *CONTR MESS
054000000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
054100000000     C                   Z-ADD     *ZEROS        MSGULT
054200000000     C                   ENDSR
054300000000     C*------------------------------------------------------------------------*
054400000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
054500000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
054600000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
054700000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
054800000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
054900000000     C*------------------------------------------------------------------------*
055000000000     C     CARFUN        BEGSR
055100000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
055200000000     C     1             DO        31            I                 5 0          --- 1 -->
055300000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
055400000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
055500000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
055600020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
055700000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
055800000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
055900000000     C                   ELSE                                                   *INTERROG.
056000000000     C                   MOVEL     FUNASI        FAT(I)
056100000000     C                   ENDIF
056200000000     C                   ELSE                                                   <-- 2 --> *FILIALE
056300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
056400000000     C                   MOVEL     FUNAFG        FAT(I)
056500000000     C                   ELSE                                                   *INTERROG.
056600000000     C                   MOVEL     FUNAFI        FAT(I)
056700000000     C                   ENDIF
056800000000     C                   ENDIF                                                  <-- 2 ---
056900000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
057000000000     C                   MOVEL     USER          WEDP              3            *PROFILO
057100000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
057200000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
057300000000     C                   ELSE
057400000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
057500000000     C                   ENDIF
057600000000     C                   ENDIF
057700000000     C                   ENDDO                                                  <-- 1 ---
057800000000     C                   ENDSR
057900000000     C*------------------------------------------------------------------------*
058000000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
058100000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
058200000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
058300000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
058400000000     C*------------------------------------------------------------------------*
058500000000     C     OUTFUN        BEGSR
058600000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
058700000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
058800000000     C                   ENDDO
058900000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
059000000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
059100000000     C     1             DO        31            I
059200000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
059300000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
059400000000     C                   ADD       1             FUNNAT
059500000000     C                   ENDIF
059600000000     C                   ENDDO
059700000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
059800000000     C     FUNNAT        IFGT      8
059900000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
060000000000     C                   ENDIF
060100000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
060200000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
060300000000     C                   Z-ADD     8             FUNVIS            5 0
060400000000     C     FAT(23)       IFEQ      'S'
060500000000     C                   SUB       1             FUNVIS
060600000000     C                   ENDIF
060700000000     C     FAT(24)       IFEQ      'S'
060800000000     C                   SUB       1             FUNVIS
060900000000     C                   ENDIF
061000000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
061100000000     C                   Z-ADD     *ZEROS        W                 5 0
061200000000     C                   ADD       1             FUNULT
061300000000     C     FUNULT        IFGT      31
061400000000     C                   Z-ADD     1             FUNULT
061500000000     C                   ENDIF
061600000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
061700000000     C     FUNULT        DO        31            I                              --- 1 -->
061800000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
061900000000     C     W             IFLT      FUNVIS                                       --- 2 -->
062000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
062100000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
062200000000     C     FDE(I)        ANDNE     *BLANKS
062300000000     C     I             IFEQ      23                                           *TOLGO F23
062400000000     C     I             OREQ      24                                           *E F24
062500000000     C                   ELSE
062600000000     C                   ADD       1             W
062700000000     C                   MOVEL     FDE(I)        FUV(W)
062800000000     C                   ENDIF
062900000000     C                   ENDIF                                                  <-- 3 ---
063000000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
063100000000     C                   Z-ADD     I             FUNULT
063200000000     C                   ENDIF                                                  <-- 2 ---
063300000000     C                   ENDDO                                                  <-- 1 ---
063400000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
063500000000     C                   MOVEL     FUV(1)        VFD001
063600000000     C                   MOVEL     FUV(2)        VFD002
063700000000     C                   MOVEL     FUV(3)        VFD003
063800000000     C                   MOVEL     FUV(4)        VFD004
063900000000     C                   MOVEL     FUV(5)        VFD005
064000000000     C                   MOVEL     FUV(6)        VFD006
064100000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
064200000000     C     FAT(24)       ANDEQ     'N'
064300000000     C                   MOVEL     FUV(7)        VFD007
064400000000     C                   MOVEL     FUV(8)        VFD008
064500000000     C                   ELSE                                                   <-- 1 -->
064600000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
064700000000     C     FAT(24)       ANDEQ     'S'
064800000000     C                   MOVEL     FDE(23)       VFD007
064900000000     C                   MOVEL     FDE(24)       VFD008
065000000000     C                   ELSE                                                   <-- 2 -->
065100000000     C                   MOVEL     FUV(7)        VFD007
065200000000     C     FAT(23)       IFEQ      'S'
065300000000     C                   MOVEL     FDE(23)       VFD008
065400000000     C                   ELSE
065500000000     C                   MOVEL     FDE(24)       VFD008
065600000000     C                   ENDIF
065700000000     C                   ENDIF                                                  <-- 2 ---
065800000000     C                   ENDIF                                                  <-- 1 ---
065900000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
066000000000     C                   ENDSR
066100000000     C*------------------------------------------------------------------------*
066200000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
066300000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
066400000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
066500000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
066600000000     C*------------------------------------------------------------------------*
066700000000     C     CONFUN        BEGSR
066800000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
066900000000     C     1             DO        31            I                              --- 1 -->
067000000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
067100000000     C* CONTROLLO SE IL TASTO E' ATTIVO
067200000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
067300000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
067400000000     C                   EXSR      CARMSG
067500000000     C                   MOVEL     '1'           $ERR
067600000000     C                   ENDIF                                                  <-- 3 ---
067700000000     C                   ENDIF                                                  <-- 2 ---
067800000000     C                   ENDDO                                                  <-- 1 ---
067900000000     C                   ENDSR
068000000000     O*------------------------------------------------------------------------*
068100000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
068200000000TNSS74R- Tasto funzionale non valido.                                          1
068300000000TNSS74R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
068400000000TNSS74R- Opzione non valida.                                                   3
068500000000TNSS74R- Opzione abilitata ma non gestita. Avvertire CED.                      4
068600000000TNSS74R- Inizio elenco.                                                        5
068700000000TNSS74R- Fine elenco.                                                          6
068800000000TNSS74R- Anno/Mese iniziale errato                                             7
068900000000TNSS74R- Anno/Mese finale errato                                               8
069000000000TNSS74R- Anno/Mese finale inferiore all'Anno/Mese iniziale                     9
069100000000TNSS74R- Impossibile elaborare più di 12 mesi                                 10
069200000000TNSS74R- Data superiore alla data creazione saldi                             11
069300000000TNSS74R- Immettere uno fra i valori possibili                                 12
069400000000TNSS74R- 'S' per creare il file su PC, 'N' per non crearlo                    13
069500000000TNSS74R- 'S' per creare il file di lavoro, 'N' per non crearlo                14
069600980403TNSS74R- Non si possono lanciare le statistiche di gruppo in SDI !!!          15
069700020326TNSS74R- Impossibile lanciare la statistica su anni differenti                16
069800000000TNSS74R- 17
069900000000TNSS74R- 18
070000000000TNSS74R- 19
070100000000TNSS74R- 20
070200000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
070300000000F1    -                   -    NN     NN
070400000000F2    -                   -    NN     NN
070500000000F3    -F3=Fine            -    SS     SS
070600000000F4    -                   -    NN     NN
070700000000F5    -                   -    NN     NN
070800000000F6    -F6=Lancio          -    SS     SS
070900000000F7    -                   -    NN     NN
071000000000F8    -                   -    NN     NN
071100000000F9    -                   -    NN     NN
071200000000F10   -                   -    NN     NN
071300000000F11   -                   -    NN     NN
071400000000F12   -F12-Ritorno        -    SS     SS
071500000000F13   -                   -    NN     NN
071600000000F14   -                   -    NN     NN
071700000000F15   -                   -    NN     NN
071800000000F16   -                   -    NN     NN
071900000000F17   -                   -    NN     NN
072000000000F18   -                   -    NN     NN
072100000000F19   -                   -    NN     NN
072200000000F20   -                   -    NN     NN
072300000000F21   -                   -    NN     NN
072400000000F22   -                   -    NN     NN
072500000000F23   -                   -    NN     NN
072600000000F24   -F24=Altri tasti    -    NN     NN
072700000000ENTER -                   -    SS     SS
072800000000ROLLUP-                   -    NN     NN
072900000000ROLLDN-                   -    NN     NN
073000000000HELP  -                   -    NN     NN
073100000000PRINT -                   -    NN     NN
073200000000BSPACE-                   -    NN     NN
073300000000CLEAR -                   -    NN     NN
