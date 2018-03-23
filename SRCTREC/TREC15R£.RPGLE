000100000000     H*------------------------------------------------------------------------*
000200000000     h*?NB: Da compilare in PJBAROBJ
000300000000     H*------------------------------------------------------------------------*
000400000000     H*         - LANCIO AGGIORNAMENTI SALDI CLIENTE DA CONTABILITA' -         *
000500000000     H*------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700000000     H*------------------------------------------------------------------------*
000800000000     H* INDICATORI USATI
000900000000     H*------------------------------------------------------------------------*
001000000000     H* 40/44 - GESTIONE VIDEO
001100000000     H* 60/63 - ERRORE CAMPI VIDEO
001200000000     F*------------------------------------------------------------------------*
001300000000     F* DATA BASE
001400000000     F*------------------------------------------------------------------------*
001500000000     FYCO901D   CF   E             WORKSTN INFDS(DEVDS1)
001600000000     F                                     SFILE(O901DVM:MSGULT)
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002100000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002200000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002300000000     D FFU             S              6    DIM(31)
002400000000     D FDE             S             19    DIM(31)
002500000000     D FAT             S              1    DIM(31)
002600000000     D* TABELLA DESCRIZIONI A VIDEO
002700000000     D FUV             S             19    DIM(8)
002800000000     D* TABELLA TASTI FUNZIONALI PREMUTI
002900000000     D KEYS            S              1    DIM(40)
003000000000     D*------------------------------------------------------------------------*
003100000000     D* INPUT
003200000000     D*------------------------------------------------------------------------*
003300000000     D*-------------
003400000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003500000000     D*-------------
003600000000     D DSFUN         E DS
003700000000     D  FUNRIG                 1     40
003800000000     D*-------------
003900000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004000000000     D*-------------
004100000000     D DSMSG         E DS
004200000000     D*-------------
004300020923     D* DS REPERIMENTO DATI UTENTE
004400020923     D*-------------------
004500020923     D TIBS34DS      E DS
004600020923     D DDATIUTE      E DS
004700020923     D AZUTEDS       E DS                  extname(AZUTE00F)
004800000000     D*-------------
004900000000     D* DS PER GESTIONE TASTI FUNZIONALI
005000000000     D*-------------
005100000000     D DSKEYS        E DS
005200000000     D  KEYRIG                 1     40
005300000000     D*-------------
005400000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005500000000     D*-------------
005600000000     D                SDS
005700000000     D  USER                 254    263
005800000000     D*-------------
005900000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006000000000     D*-------------
006100000000     D DEVDS1          DS
006200000000     D* CHIAVE FUNZIONALE BATTUTA
006300000000     D  KEY                  369    369
006400000000     D* POSIZIONE CURSORE (IN HEX)
006500000000     D  CURSOR               370    371
006600000000     D*-------------
006700990602     D* CONTROLLO DATE -XSRDA8-
006800000000     D*-------------
006900990602     D WLBDA8          DS                  INZ
007000990602     D  G08DAT                 1      8  0
007100990602     D  G08INV                 9     16  0
007200990602     D  G08ERR                17     17
007300990602     D  G08TGI                18     22  0
007400990602     D*------------------
007500990602     D* DS DATA GENERICA(8) IN GG/MM/AAAA
007600990602     D*------------------
007700990602     D                 DS
007800990602     D  DGIOR8                 1      2  0
007900990602     D  DMESE8                 3      4  0
008000990602     D  DANNO8                 5      8  0
008100990602     D  DDATA8                 1      8  0
008200000000     D*-------------
008300000000     D* ARCHITETTURA
008400000000     D*-------------
008500000000     D KPJBA         E DS
008600000000     D*-------------
008700000000     D* PARAMETRI DI PASSAGGIO CL DI AGGIORNAMENTO
008800000000     D*-------------
008900000000     D PARAM           DS                  INZ
009000990602     D  PARAAI                 1      4  0
009100990602     D  PARMMI                 5      6  0
009200990602     D  PARAAF                 7     10  0
009300990602     D  PARMMF                11     12  0
009400990602     D  PARAGG                13     13
009500000000     C*------------------------------------------------------------------------*
009600000000     C* MAIN LINE
009700000000     C*------------------------------------------------------------------------*
009800000000     C*
009900000000     C* CICLO FINO A CHE:
010000000000DO  1C     $FINE         DOUEQ     '1'
010100000000     C*
010200000000     C                   MOVEL     '0'           $ERR
010300000000     C*
010400000000     C* CONTROLLA GESTIONE VIDEO
010500000000IF  2C     $GES          IFEQ      'V1'
010600000000     C                   EXSR      GESV1
010700000000E   2C                   ENDIF
010800000000     C*
010900000000E   1C                   ENDDO
011000000000     C*
011100000000     C* FINE MAIN DEL PGM
011200000000     C                   SETON                                        LR
011300000000     C*------------------------------------------------------------------------*
011400000000     C* GESV1 - GESTIONE VIDEO 'PARAMETRI'
011500000000     C*------------------------------------------------------------------------*
011600000000     C     GESV1         BEGSR
011700000000     C*
011800000000IF  1C     $TESVD        IFEQ      '1'
011900000000     C                   EXSR      CARV1
012000000000E   1C                   ENDIF
012100000000     C*
012200000000IF  1C     MSGULT        IFEQ      *ZEROS
012300000000     C                   MOVEL     *BLANKS       $MSG
012400000000     C                   EXSR      CARMSG
012500000000E   1C                   ENDIF
012600000000     C                   EXSR      OUTMSG
012700000000     C*
012800000000     C                   EXFMT     O901DV1
012900000000     C*
013000000000     C                   EXSR      CONFUN
013100000000     C*
013200000000IF  1C     $ERR          IFEQ      '0'
013300000000SL  2C                   SELECT
013400000000     C     KEY           WHENEQ    ENTER
013500000000     C                   EXSR      ENTV1
013600000000     C     KEY           WHENEQ    F6
013700000000     C                   EXSR      F06GES
013800000000     C                   OTHER
013900000000     C                   EXSR      FUNSTD
014000000000E   2C                   ENDSL
014100000000E   1C                   ENDIF
014200000000     C*
014300000000     C                   ENDSR
014400000000     C*------------------------------------------------------------------------*
014500000000     C* CARV1 - CARICA VIDEO 'PARAMETRI'
014600000000     C*------------------------------------------------------------------------*
014700000000     C     CARV1         BEGSR
014800000000     C*
014900000000     C                   MOVEL     'V1'          $GES
015000000000     C*
015100000000     C                   EXSR      CARFUN
015200000000     C                   EXSR      OUTFUN
015300000000     C*
015400000000     C                   WRITE     O901DVT
015500000000     C                   WRITE     O901DVF
015600000000     C*
015700000000     C                   ENDSR
015800000000     C*------------------------------------------------------------------------*
015900000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
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
017300000000     C                   EXSR      DECODI
017400000000     C*
017500000000     C* NON CI SONO ERRORI
017600000000IF  1C     $ERR          IFEQ      '0'
017700000000     C*
017800000000     C* LANCIO PROGRAMMA DI STAMPA
017900000000     C                   MOVEL     V1CDAI        PARAAI
018000000000     C                   MOVE      V1CDAI        PARMMI
018100000000     C                   MOVEL     V1CDAF        PARAAF
018200000000     C                   MOVE      V1CDAF        PARMMF
018300000000     C                   MOVEL     'S'           PARAGG
018400000000     C                   MOVEL     'Y902'        KCOAZ
018500000000     C                   MOVEL     PARAM         KPJBU
018600000000     C                   CALL      'BCH10'
018700000000     C                   PARM                    KPJBA
018800000000     C                   MOVEL     '1'           $FINE
018900000000E   1C                   ENDIF
019000000000     C*
019100000000     C                   ENDSR
019200000000     C*------------------------------------------------------------------------*
019300000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
019400000000     C*------------------------------------------------------------------------*
019500000000     C     DECODI        BEGSR
019600000000     C*
019700000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
019800000000     C                   SETOFF                                       606162
019900000000     C*--------------------
020000000000     C* ANNO/MESE INIZIALE
020100000000     C*--------------------
020200000000     C* OBBLIGATORIO
020300000000IF  1C     V1CDAI        IFEQ      *ZEROS
020400000000     C                   MOVEL     '1'           $ERR
020500000000     C                   MOVEL     MSG(7)        $MSG
020600000000     C                   EXSR      CARMSG
020700000000     C                   SETON                                        60
020800000000E   1C                   ENDIF
020900000000     C* ESATTO
021000000000IF  1C     $ERR          IFEQ      '0'
021100990602     C                   MOVEL     V1CDAI        G08INV
021200990602     C                   MOVE      01            G08INV
021300990602     C                   Z-ADD     *ZEROS        G08DAT
021400990602     C                   MOVEL     '3'           G08ERR
021500990602     C                   CALL      'XSRDA8'
021600990602     C                   PARM                    WLBDA8
021700000000     C*
021800990602IF  2C     G08ERR        IFEQ      '1'
021900000000     C                   MOVEL     '1'           $ERR
022000000000     C                   MOVEL     MSG(9)        $MSG
022100000000     C                   EXSR      CARMSG
022200000000     C                   SETON                                        60
022300000000E   2C                   ENDIF
022400000000E   1C                   ENDIF
022500000000     C*--------------------
022600000000     C* ANNO/MESE FINALE
022700000000     C*--------------------
022800000000     C* OBBLIGATORIO
022900000000IF  1C     $ERR          IFEQ      '0'
023000000000IF  2C     V1CDAF        IFEQ      *ZEROS
023100000000     C                   MOVEL     '1'           $ERR
023200000000     C                   MOVEL     MSG(7)        $MSG
023300000000     C                   EXSR      CARMSG
023400000000     C                   SETON                                        61
023500000000E   2C                   ENDIF
023600000000E   2C                   ENDIF
023700000000     C* ESATTO
023800000000IF  1C     $ERR          IFEQ      '0'
023900990602     C                   MOVEL     V1CDAF        G08INV
024000990602     C                   MOVE      01            G08INV
024100990602     C                   Z-ADD     *ZEROS        G08DAT
024200990602     C                   MOVEL     '3'           G08ERR
024300990602     C                   CALL      'XSRDA8'
024400990602     C                   PARM                    WLBDA8
024500000000     C*
024600990602IF  2C     G08ERR        IFEQ      '1'
024700000000     C                   MOVEL     '1'           $ERR
024800000000     C                   MOVEL     MSG(9)        $MSG
024900000000     C                   EXSR      CARMSG
025000000000     C                   SETON                                        61
025100000000E   2C                   ENDIF
025200000000E   1C                   ENDIF
025300000000     C* MINORE/UGUALE DATA CORRENTE - 1
025400000000IF  1C     $ERR          IFEQ      '0'
025500000000IF  2C     V1CDAF        IFGT      AMCOR
025600000000     C                   MOVEL     '1'           $ERR
025700000000     C                   MOVEL     MSG(11)       $MSG
025800000000     C                   EXSR      CARMSG
025900000000     C                   SETON                                        61
026000000000E   2C                   ENDIF
026100000000E   1C                   ENDIF
026200000000     C* MAGGIORE/UGUALE DATA INIZIALE
026300000000IF  1C     $ERR          IFEQ      '0'
026400000000IF  2C     V1CDAF        IFLT      V1CDAI
026500000000     C                   MOVEL     '1'           $ERR
026600000000     C                   MOVEL     MSG(10)       $MSG
026700000000     C                   EXSR      CARMSG
026800000000     C                   SETON                                        61
026900000000E   2C                   ENDIF
027000000000E   1C                   ENDIF
027100000000     C*
027200000000     C                   ENDSR
027300020923     C*-------------------------------------------------------------------------
027400020923     C* REPERISCE I DATI UTENTE
027500020923     C*-------------------------------------------------------------------------
027600020923     C     REPDATIUTE    BEGSR
027700020923     C*
027800020923     C* INIZIALIZZA VARIABILI DI WRK
027900020923     C                   CLEAR                   TIBS34DS
028000020923     C                   CLEAR                   AZUTEDS
028100020923     C                   CLEAR                   DDATIUTE
028200020923     C*
028300020923     C     *DTAARA       DEFINE    §azute        azuteds
028400020923     C     *DTAARA       DEFINE    §datiute      ddatiute
028500020923     C                   IN(E)     *DTAARA
028600020923if  1C                   IF        %Error
028700020923     c                   EVAL      I34Tla = 'L'
028800020923     C                   CALL      'TIBS34R'
028900020923     C                   PARM                    Tibs34Ds
029000020923     C                   IN        *DTAARA
029100020923e   1C                   ENDIF
029200020923     C*
029300020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
029400020923     C                   MOVEL(P)  RSUT          DSFIRS           20
029500020923     C*
029600020923     C                   ENDSR
029700000000     C*------------------------------------------------------------------------*
029800000000     C* *INZSR  - ROUTINE INIZIALE
029900000000     C*------------------------------------------------------------------------*
030000000000     C     *INZSR        BEGSR
030100000000     C*
030200000000     C* RICEVIMENTO PARAMETRI
030300000000     C     *ENTRY        PLIST
030400000000     C                   PARM                    KPJBA
030500000000     C*
030600000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
030700000000     C                   EXSR      INZSTD
030800000000     C*
030900000000     C* IMPOSTAZIONE INIZIALE VIDEO
031000000000     C                   EXSR      IMPVID
031100000000     C*
031200000000     C* EMETTO FORMATO VIDEO
031300000000     C                   EXSR      CARV1
031400000000     C*
031500000000     C                   ENDSR
031600000000     C*------------------------------------------------------------------------*
031700000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
031800000000     C*------------------------------------------------------------------------*
031900000000     C     IMPVID        BEGSR
032000990602     C*
032100990602     C                   TIME                    N14              14 0
032200990602     C                   MOVE      N14           N8                8 0
032300990602     C                   Z-ADD     N8            DDATA8
032400000000     C*--------------------
032500000000     C* ANNO/MESE INIZIALE (INIZIO DELL'ANNO)
032600000000     C*--------------------
032700990602     C                   Z-ADD     DANNO8        ANNO              4 0
032800990602     C                   Z-ADD     DMESE8        MESE              2 0
032900000000IF  1C     MESE          IFEQ      01
033000000000     C                   SUB       1             ANNO
033100000000X   1C                   ELSE
033200000000     C                   Z-ADD     01            MESE
033300000000E   1C                   ENDIF
033400000000     C                   MOVEL     ANNO          V1CDAI
033500000000     C                   MOVE      MESE          V1CDAI
033600000000     C*--------------------
033700000000     C* ANNO/MESE FINALE (ANNO/MESE CORRENTE - 1)
033800000000     C*--------------------
033900990602     C                   Z-ADD     DANNO8        ANNO
034000990602     C                   Z-ADD     DMESE8        MESE
034100000000     C                   SUB       1             MESE
034200000000IF  1C     MESE          IFEQ      *ZEROS
034300000000     C                   Z-ADD     12            MESE
034400000000     C                   SUB       1             ANNO
034500000000E   1C                   ENDIF
034600990602     C                   MOVEL     ANNO          V1CDAF
034700990602     C                   MOVE      MESE          V1CDAF
034800990602     C                   Z-ADD     V1CDAF        AMCOR             4 0
034900000000     C*--------------------
035000000000     C* FLAG DI AGGIORNAMENTO
035100000000     C*--------------------
035200000000     C*--------------------MOVEL'N'       V1CAGG           *AGGIORNAMENTO NO
035300000000     C*
035400000000     C                   ENDSR
035500000000     C*------------------------------------------------------------------------*
035600000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
035700000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
035800000000     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
035900000000     C* STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
036000000000     C* NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
036100000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
036200000000     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
036300000000     C*------------------------------------------------------------------------*
036400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
036500000000     C*------------------------------------------------------------------------*
036600000000     C     INZSTD        BEGSR
036700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
036800000000     C                   CALL      'XKEYS'
036900000000     C                   PARM                    DSKEYS
037000000000     C                   MOVEA     KEYRIG        KEYS
037100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
037200000000     C     XMSGPA        PLIST
037300000000     C                   PARM                    DSMSMS
037400000000     C                   PARM                    DSMSPG
037500000000     C                   PARM                    DSMSKE
037600000000     C                   PARM                    DSMSER
037700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
037800000000     C                   MOVEL     'YCO901R'     DSMSPG
037900000000     C                   MOVEL     'YCO901R'     VMCQUE
038000000000     C                   MOVEL     *BLANKS       VMCKEY
038100000000     C                   MOVEL     *BLANKS       DSMSMS
038200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
038300000000     C                   MOVEL     '0'           $ERR              1
038400000000     C                   MOVEL     '0'           $F03              1
038500000000     C                   MOVEL     *BLANKS       $GES              2
038600000000     C                   MOVEL     *BLANKS       $MSG             80
038700000000     C                   MOVEL     '0'           $FINE             1
038800000000     C                   MOVEL     '1'           $TESVD            1
038900000000     C                   MOVEL     'I'           $TGES             1
039000000000     C* LANCIO ROUTINE PER VERIFICARE SE
039100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
039200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
039300020923     C                   EXSR      REPDATIUTE
039400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
039500000000     C                   BITOFF    '01234567'    POSMI1            1
039600000000     C                   BITOFF    '01234567'    POSMI2            1
039700000000     C                   BITOFF    '01234567'    POSMA1            1
039800000000     C                   BITOFF    '01234567'    POSMA2            1
039900000000     C                   BITON     '34'          POSMI1            1
040000000000     C                   BITON     '7'           POSMI2            1
040100000000     C                   BITON     '34'          POSMA1            1
040200000000     C                   BITON     '0'           POSMA2            1
040300000000     C                   MOVE      POSMI2        POSMIN            2
040400000000     C                   MOVEL     POSMI1        POSMIN
040500000000     C                   MOVE      POSMA2        POSMAX            2
040600000000     C                   MOVEL     POSMA1        POSMAX
040700000000     C                   ENDSR
040800000000     C*------------------------------------------------------------------------*
040900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
041000000000     C*------------------------------------------------------------------------*
041100000000     C     FUNSTD        BEGSR
041200000000     C                   SELECT
041300000000     C     KEY           WHENEQ    F3
041400000000     C                   EXSR      F03STD
041500000000     C     KEY           WHENEQ    F12
041600000000     C                   EXSR      F12STD
041700000000     C     KEY           WHENEQ    F24
041800000000     C                   EXSR      F24STD
041900000000     C                   OTHER
042000000000     C                   MOVEL     MSG(2)        $MSG
042100000000     C                   EXSR      CARMSG
042200000000     C                   ENDSL
042300000000     C                   ENDSR
042400000000     C*------------------------------------------------------------------------*
042500000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
042600000000     C*------------------------------------------------------------------------*
042700000000     C     F03STD        BEGSR
042800000000     C*
042900000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
043000000000     C                   MOVEL     '1'           $FINE
043100000000     C*
043200000000     C                   ENDSR
043300000000     C*------------------------------------------------------------------------*
043400000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
043500000000     C*------------------------------------------------------------------------*
043600000000     C     F12STD        BEGSR
043700000000     C*
043800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
043900000000     C                   MOVEL     '1'           $FINE
044000000000     C*
044100000000     C                   ENDSR
044200000000     C*------------------------------------------------------------------------*
044300000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
044400000000     C*------------------------------------------------------------------------*
044500000000     C     F24STD        BEGSR
044600000000     C*
044700000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
044800000000     C                   EXSR      OUTFUN
044900000000     C*
045000000000     C                   ENDSR
045100000000     C*------------------------------------------------------------------------*
045200000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
045300000000     C*          CODA MESSAGGI DEL PGM
045400000000     C*------------------------------------------------------------------------*
045500000000     C     CARMSG        BEGSR
045600000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
045700000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
045800000000     C     MSGULT        IFEQ      0
045900000000     C                   MOVEL     '1'           *IN40
046000000000     C                   WRITE     O901DVK
046100000000     C                   MOVEL     '0'           *IN40
046200000000     C                   ENDIF
046300000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
046400000000     C                   MOVEL     $MSG          DSMSMS
046500000000     C                   CALL      'XMSG'        XMSGPA
046600000000     C     DSMSER        IFEQ      0
046700000000     C                   MOVEL     DSMSKE        VMCKEY
046800000000     C                   ENDIF
046900000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
047000000000     C                   ADD       1             MSGULT            4 0
047100000000     C                   WRITE     O901DVM
047200000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
047300000000     C                   MOVEL     *BLANKS       $MSG
047400000000     C                   ENDSR
047500000000     C*------------------------------------------------------------------------*
047600000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
047700000000     C*------------------------------------------------------------------------*
047800000000     C     OUTMSG        BEGSR
047900000000     C* VISUALIZZAZIONE MESSAGGIO
048000000000     C                   MOVEL     '1'           *IN44
048100000000     C                   WRITE     O901DVK
048200000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
048300000000     C                   Z-ADD     *ZEROS        MSGULT
048400000000     C                   ENDSR
048500000000     C*------------------------------------------------------------------------*
048600000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
048700000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
048800000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
048900000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
049000000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
049100000000     C*------------------------------------------------------------------------*
049200000000     C     CARFUN        BEGSR
049300000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
049400000000     C     1             DO        31            I                 5 0
049500000000     C                   MOVEL     FUN(I)        FUNRIG
049600000000     C                   MOVEL     FUNFUN        FFU(I)
049700000000     C                   MOVEL     FUNDES        FDE(I)
049800020923     C     DUTLPO        IFEQ      'S'
049900000000     C     $TGES         IFEQ      'G'
050000000000     C                   MOVEL     FUNASG        FAT(I)
050100000000     C                   ELSE
050200000000     C                   MOVEL     FUNASI        FAT(I)
050300000000     C                   ENDIF
050400000000     C                   ELSE
050500000000     C     $TGES         IFEQ      'G'
050600000000     C                   MOVEL     FUNAFG        FAT(I)
050700000000     C                   ELSE
050800000000     C                   MOVEL     FUNAFI        FAT(I)
050900000000     C                   ENDIF
051000000000     C                   ENDIF
051100000000     C     FAT(I)        IFEQ      'E'
051200000000     C                   MOVEL     USER          WEDP              3
051300000000     C     WEDP          IFEQ      'EDP'
051400000000     C                   MOVEL     'S'           FAT(I)
051500000000     C                   ELSE
051600000000     C                   MOVEL     'N'           FAT(I)
051700000000     C                   ENDIF
051800000000     C                   ENDIF
051900000000     C                   ENDDO
052000000000     C                   ENDSR
052100000000     C*------------------------------------------------------------------------*
052200000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
052300000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
052400000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
052500000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
052600000000     C*------------------------------------------------------------------------*
052700000000     C     OUTFUN        BEGSR
052800000000     C     1             DO        8             I
052900000000     C                   MOVEL     *BLANKS       FUV(I)
053000000000     C                   ENDDO
053100000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
053200000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
053300000000     C     1             DO        31            I
053400000000     C     FAT(I)        IFEQ      'S'
053500000000     C     FDE(I)        ANDNE     *BLANKS
053600000000     C                   ADD       1             FUNNAT
053700000000     C                   ENDIF
053800000000     C                   ENDDO
053900000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
054000000000     C     FUNNAT        IFGT      8
054100000000     C                   MOVEL     'S'           FAT(24)
054200000000     C                   ENDIF
054300000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
054400000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
054500000000     C                   Z-ADD     8             FUNVIS            5 0
054600000000     C     FAT(23)       IFEQ      'S'
054700000000     C                   SUB       1             FUNVIS
054800000000     C                   ENDIF
054900000000     C     FAT(24)       IFEQ      'S'
055000000000     C                   SUB       1             FUNVIS
055100000000     C                   ENDIF
055200000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
055300000000     C                   Z-ADD     *ZEROS        W                 5 0
055400000000     C                   ADD       1             FUNULT
055500000000     C     FUNULT        IFGT      31
055600000000     C                   Z-ADD     1             FUNULT
055700000000     C                   ENDIF
055800000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
055900000000     C     FUNULT        DO        31            I
056000000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
056100000000     C     W             IFLT      FUNVIS
056200000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
056300000000     C     FAT(I)        IFEQ      'S'
056400000000     C     FDE(I)        ANDNE     *BLANKS
056500000000     C     I             IFEQ      23
056600000000     C     I             OREQ      24
056700000000     C                   ELSE
056800000000     C                   ADD       1             W
056900000000     C                   MOVEL     FDE(I)        FUV(W)
057000000000     C                   ENDIF
057100000000     C                   ENDIF
057200000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
057300000000     C                   Z-ADD     I             FUNULT
057400000000     C                   ENDIF
057500000000     C                   ENDDO
057600000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
057700000000     C                   MOVEL     FUV(1)        VFD001
057800000000     C                   MOVEL     FUV(2)        VFD002
057900000000     C                   MOVEL     FUV(3)        VFD003
058000000000     C                   MOVEL     FUV(4)        VFD004
058100000000     C                   MOVEL     FUV(5)        VFD005
058200000000     C                   MOVEL     FUV(6)        VFD006
058300000000     C     FAT(23)       IFEQ      'N'
058400000000     C     FAT(24)       ANDEQ     'N'
058500000000     C                   MOVEL     FUV(7)        VFD007
058600000000     C                   MOVEL     FUV(8)        VFD008
058700000000     C                   ELSE
058800000000     C     FAT(23)       IFEQ      'S'
058900000000     C     FAT(24)       ANDEQ     'S'
059000000000     C                   MOVEL     FDE(23)       VFD007
059100000000     C                   MOVEL     FDE(24)       VFD008
059200000000     C                   ELSE
059300000000     C                   MOVEL     FUV(7)        VFD007
059400000000     C     FAT(23)       IFEQ      'S'
059500000000     C                   MOVEL     FDE(23)       VFD008
059600000000     C                   ELSE
059700000000     C                   MOVEL     FDE(24)       VFD008
059800000000     C                   ENDIF
059900000000     C                   ENDIF
060000000000     C                   ENDIF
060100000000     C                   MOVEL     '0'           $TESVD
060200000000     C                   ENDSR
060300000000     C*------------------------------------------------------------------------*
060400000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
060500000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
060600000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
060700000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
060800000000     C*------------------------------------------------------------------------*
060900000000     C     CONFUN        BEGSR
061000000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
061100000000     C     1             DO        31            I
061200000000     C     KEY           IFEQ      KEYS(I)
061300000000     C* CONTROLLO SE IL TASTO E' ATTIVO
061400000000     C     FAT(I)        IFNE      'S'
061500000000     C                   MOVEL     MSG(1)        $MSG
061600000000     C                   EXSR      CARMSG
061700000000     C                   MOVEL     '1'           $ERR
061800000000     C                   ENDIF
061900000000     C                   ENDIF
062000000000     C                   ENDDO
062100000000     C                   ENDSR
062200000000     O*------------------------------------------------------------------------*
062300000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
062400000000YCO901R- Tasto funzionale non valido.                                          1
062500000000YCO901R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
062600000000YCO901R- Opzione non valida.                                                   3
062700000000YCO901R- Opzione abilitata ma non gestita. Avvertire CED.                      4
062800000000YCO901R- Inizio elenco.                                                        5
062900000000YCO901R- Fine elenco.                                                          6
063000000000YCO901R- Anno/mese obbligatorio                                                7
063100000000YCO901R- 8
063200000000YCO901R- Anno/mese errati                                                      9
063300000000YCO901R- Anno/mese finale minore dell'Anno/mese iniziale                      10
063400000000YCO901R- Anno/mese finale superiore alla data corrente                        11
063500000000YCO901R- 12
063600000000YCO901R- 13
063700000000YCO901R- 14
063800000000YCO901R- 15
063900000000YCO901R- 16
064000000000YCO901R- 17
064100000000YCO901R- 18
064200000000YCO901R- 19
064300000000YCO901R- 20
064400000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
064500000000F1    -                   -    NN     NN
064600000000F2    -                   -    NN     NN
064700000000F3    -F3=Fine            -    SS     SS
064800000000F4    -                   -    NN     NN
064900000000F5    -                   -    NN     NN
065000000000F6    -F6=Lancio          -    SS     SS
065100000000F7    -                   -    NN     NN
065200000000F8    -                   -    NN     NN
065300000000F9    -                   -    NN     NN
065400000000F10   -                   -    NN     NN
065500000000F11   -                   -    NN     NN
065600000000F12   -F12-Ritorno        -    SS     SS
065700000000F13   -                   -    NN     NN
065800000000F14   -                   -    NN     NN
065900000000F15   -                   -    NN     NN
066000000000F16   -                   -    NN     NN
066100000000F17   -                   -    NN     NN
066200000000F18   -                   -    NN     NN
066300000000F19   -                   -    NN     NN
066400000000F20   -                   -    NN     NN
066500000000F21   -                   -    NN     NN
066600000000F22   -                   -    NN     NN
066700000000F23   -                   -    NN     NN
066800000000F24   -F24=Altri tasti    -    NN     NN
066900000000ENTER -                   -    SS     SS
067000000000ROLLUP-                   -    NN     NN
067100000000ROLLDN-                   -    NN     NN
067200000000HELP  -                   -    NN     NN
067300000000PRINT -                   -    NN     NN
067400000000BSPACE-                   -    NN     NN
067500000000CLEAR -                   -    NN     NN
