000100000000     H*------------------------------------------------------------------------*
000200121120     H* RICHIESTA STATISTICA SPEDIZIONI CLIENTI PARTICOLARI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800121120     FTNSSL4D   CF   E             WORKSTN INFDS(DEVDS1)
000900121120     F                                     SFILE(SSL4DVM:MSGULT)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*-------------------
001400000000     D* MESSAGGI, COMANDI E FUNZIONI
001500000000     D*-------------------
001600090901     D MSG             S             78    DIM(13) CTDATA PERRCD(1)             *MESSAGGI
001700000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
001800000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001900000000     D*-------------------
002000000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002100000000     D*-------------------
002200000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002300000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002400000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002500000000     D*-------------------
002600000000     D* TABELLA DESCRIZIONI A VIDEO
002700000000     D*-------------------
002800000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002900000000     D*-------------------
003000000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003100000000     D*-------------------
003200000000     D KEYS            S              1    DIM(40)
003300000000     D*------------------------------------------------------------------------*
003400000000     D* INPUT
003500000000     D*------------------------------------------------------------------------*
003600000000     D*---------------------
003700000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003800000000     D*---------------------
003900000000     D DSFUN         E DS
004000000000     D  FUNRIG                 1     40
004100000000     D*---------------------
004200000000     D* DS PER GESTIONE TASTI FUNZIONALI
004300000000     D*---------------------
004400000000     D DSKEYS        E DS
004500000000     D  KEYRIG                 1     40
004600000000     D*---------------------
004700000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004800000000     D*---------------------
004900000000     D                SDS
005000000000     D  USER                 254    263
005100000000     D*---------------------
005200000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005300000000     D*---------------------
005400000000     D DEVDS1          DS
005500000000     D  KEY                  369    369
005600000000     D  CURSOR               370    371
005700000000     D*---------------------
005800000000     D* ARCHITETTURA
005900000000     D*---------------------
006000000000     D KPJBA         E DS
006100000000     D*---------------------
006200000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
006300000000     D*---------------------
006400000000     D DSMSG         E DS
006500000000     D*---------------------
006600020917     D* DS REPERIMENTO DATI UTENTE
006700020917     D*-------------------
006800020917     D TIBS34DS      E DS                                                       *Profili utente
006900020917     D DDATIUTE      E DS                                                       *Dati utente
007000020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007100000000     D*---------------------
007200000000     D* DS PARAMETRI DI LANCIO
007300000000     D*------------------
007400000000     D PARAM           DS                  INZ
007500000000     D  PARDTI                 1      8  0
007600000000     D  PARDTF                 9     16  0
007700970529     D  PAROPZ                17     17
007800090601     D  PARtipo               18     18
007900000000     D*---------------------
008000000000     D* DS CONTROLLO DATA (8)
008100000000     D*------------------
008200000000     D WLBDA8          DS                  INZ
008300000000     D  G08DAT                 1      8  0
008400000000     D  G08INV                 9     16  0
008500000000     D  G08ERR                17     17
008600000000     D  G08TGI                18     22  0
008700000000     C*------------------------------------------------------------------------*
008800000000     C* MAIN LINE
008900000000     C*------------------------------------------------------------------------*
009000090901     c                   setoff                                       87
009100000000     C*
009200000000     C* CICLO FINO A CHE:
009300000000DO  1C     $FINE         DOUEQ     '1'
009400000000     C*
009500000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009600000000     C*
009700000000     C* CONTROLLA GESTIONE VIDEO
009800000000IF  2C     $GES          IFEQ      'V1'
009900000000     C                   EXSR      GESV1                                        *PARAMETRI
010000000000E   2C                   ENDIF
010100000000     C*
010200000000E   1C                   ENDDO
010300000000     C*
010400000000     C                   SETON                                        LR
010500000000     C*------------------------------------------------------------------------*
010600000000     C* GESV1 - GESTIONE VIDEO UNO
010700000000     C*------------------------------------------------------------------------*
010800000000     C     GESV1         BEGSR
010900000000     C*
011000000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
011100000000     C                   EXSR      CARV1                                        *CARICA VIDEO
011200000000E   1C                   ENDIF
011300000000     C*
011400000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011500000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011600000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011700000000E   1C                   ENDIF
011800000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011900000000     C*
012000121120     C                   EXFMT     SSL4DV1                                      *PARAMETRI
012100000000     C*
012200000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012300000000     C*
012400000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012500000000SL  2C                   SELECT
012600000000     C     KEY           WHENEQ    ENTER
012700000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012800000000     C     KEY           WHENEQ    F6
012900000000     C                   EXSR      F06GES                                       *CONFERMA
013000000000     C     KEY           WHENEQ    F22
013100000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
013200090527     C     KEY           WHENEQ    F9
013300090527     C                   EXSR      F09GES                                       *GESTIONE skedula
013400000000     C                   OTHER
013500000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013600000000E   2C                   ENDSL
013700000000E   1C                   ENDIF
013800000000     C*
013900000000     C                   ENDSR
014000000000     C*------------------------------------------------------------------------*
014100000000     C* CARV1 - CARICA VIDEO UNO
014200000000     C*------------------------------------------------------------------------*
014300000000     C     CARV1         BEGSR
014400000000     C*
014500000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014600000000     C*
014700000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014800000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014900000000     C*
015000121120     C                   WRITE     SSL4DVT                                      *TESTATA
015100121120     C                   WRITE     SSL4DVF                                      *FUNZIONI
015200000000     C*
015300000000     C                   ENDSR
015400000000     C*------------------------------------------------------------------------*
015500000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
015600000000     C*------------------------------------------------------------------------*
015700000000     C     ENTV1         BEGSR
015800000000     C*
015900000000     C* CONTROLLA I CAMPI IMMESSI
016000000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016100000000     C*
016200000000     C                   ENDSR
016300000000     C*------------------------------------------------------------------------*
016400000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
016500000000     C*------------------------------------------------------------------------*
016600000000     C     F06GES        BEGSR
016700000000     C*
016800000000     C* CONTROLLA I CAMPI IMMESSI
016900000000     C                   EXSR      DECODI
017000000000     C*
017100000000     C* LANCIA IL PROGRAMMA DI STAMPA
017200000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
017300000000     C                   Z-ADD     DTIV1C        PARDTI
017400000000     C                   Z-ADD     DTFV1C        PARDTF
017500121120     C                   MOVE      '4'           PAROPZ
017600090601     C                   MOVE      V1tipo        PARtipo
017700000000     C*
017800121120     C                   MOVEL     'SSL7'        KCOAZ
017900110125
018000000000     C                   MOVEL     PARAM         KPJBU
018100000000     C                   CALL      'BCH10'
018200000000     C                   PARM                    KPJBA
018300000000     C*
018400000000     C                   MOVEL     MSG(10)       $MSG                           *STAMPA LANCIATA
018500090901     c
018600090901     c                   setoff                                       87
018700000000E   1C                   ENDIF
018800000000     C*
018900000000     C                   ENDSR
019000000000     C*------------------------------------------------------------------------*
019100000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
019200000000     C*------------------------------------------------------------------------*
019300000000     C     F22GES        BEGSR
019400000000     C*
019500000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
019600000000     C                   Z-ADD     80            LENGH            15 5
019700000000     C                   CALL      'QCAEXEC'
019800000000     C                   PARM                    CMD(1)
019900000000     C                   PARM                    LENGH
020000000000     C*
020100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
020200000000     C*
020300000000     C                   ENDSR
020400000000     C*------------------------------------------------------------------------*
020500000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
020600000000     C*------------------------------------------------------------------------*
020700000000     C     DECODI        BEGSR
020800000000     C*
020900000000     C* REIMPOSTA INDICATORI DI ERRORE
021000000000     C                   SETOFF                                       606162
021100090601     C                   SETOFF                                       63
021200000000     C*--------------------
021300000000     C* DATA INIZIO
021400000000     C*--------------------
021500000000     C     *LIKE         DEFINE    V1CDTI        DTIV1C
021600000000     C                   Z-ADD     *ZEROS        DTIV1C
021700000000IF  1C     $ERR          IFEQ      '0'
021800000000     C                   Z-ADD     V1CDTI        WDA8
021900000000     C                   Z-ADD     *ZEROS        DA8W
022000000000     C                   EXSR      CHKDAT
022100000000     C                   Z-ADD     WDA8          V1CDTI
022200000000     C                   Z-ADD     DA8W          DTIV1C
022300000000IF  2C     $ERR          IFEQ      '1'
022400000000     C                   SETON                                        60
022500000000     C                   MOVEL     MSG(7)        $MSG
022600000000     C                   EXSR      CARMSG
022700000000E   2C                   ENDIF
022800000000E   1C                   ENDIF
022900000000     C*--------------------
023000000000     C* DATA FINE
023100000000     C*--------------------
023200000000     C     *LIKE         DEFINE    V1CDTF        DTFV1C
023300000000     C                   Z-ADD     *ZEROS        DTFV1C
023400000000IF  1C     $ERR          IFEQ      '0'
023500000000     C                   Z-ADD     V1CDTF        WDA8
023600000000     C                   Z-ADD     *ZEROS        DA8W
023700000000     C                   EXSR      CHKDAT
023800000000     C                   Z-ADD     WDA8          V1CDTF
023900000000     C                   Z-ADD     DA8W          DTFV1C
024000000000IF  2C     $ERR          IFEQ      '1'
024100000000     C                   SETON                                        61
024200000000     C                   MOVEL     MSG(7)        $MSG
024300000000     C                   EXSR      CARMSG
024400000000E   2C                   ENDIF
024500000000E   1C                   ENDIF
024600090601
024700090601     c* Data fine non può essere < data inizio
024800090601IF  1C     $ERR          IFEQ      '0'
024900090601     c                   if        dtfv1c<dtiv1c
025000090601     C                   MOVEL     '1'           $ERR
025100090601     C                   SETON                                        61
025200090601     C                   MOVEL     MSG(7)        $MSG
025300090601     C                   EXSR      CARMSG
025400090601E   2C                   ENDIF
025500090601E   1C                   ENDIF
025600000000     C*--------------------
025700000000     C* NUMERO BOLLE MINIMO GIORNALIERE
025800000000     C*--------------------
025900090601     c
026000090601     c* Tipo opzione in base alle date
026100090601IF  1C     $ERR          IFEQ      '0'
026200121120     c                   if        v1tipo=' '
026300090601     C                   MOVEL     '1'           $ERR
026400090601     C                   SETON                                        63
026500090601     C                   MOVEL     MSG(12)       $MSG
026600090601     C                   EXSR      CARMSG
026700090601E   2C                   ENDIF
026800121120E   2C                   ENDIF
026900090601     c*
027000090601IF  1C     $ERR          IFEQ      '0'
027100090601     c                   if        dtFv1c>dtiv1c and v1tipo='G'
027200090601     C                   MOVEL     '1'           $ERR
027300090601     C                   SETON                                        63
027400090601     C                   MOVEL     MSG(11)       $MSG
027500090601     C                   EXSR      CARMSG
027600090601E   2C                   ENDIF
027700090601E   1C                   ENDIF
027800000000     C*
027900000000     C                   ENDSR
028000000000     C*------------------------------------------------------------------------*
028100000000     C* CHKDAT - CONTROLLA LA DATA
028200000000     C*          INPUT  -> WDA8 - DATA (GG/MM/AAAA)
028300000000     C*          OUTPUT -> DA8W - DATA (AAAA/MM/GG)
028400000000     C*------------------------------------------------------------------------*
028500000000     C     CHKDAT        BEGSR
028600000000     C*
028700000000     C     *LIKE         DEFINE    V1CDTI        WDA8
028800000000     C     *LIKE         DEFINE    V1CDTI        DA8W
028900000000     C*
029000000000     C                   Z-ADD     *ZEROS        DA8W
029100000000     C*
029200000000     C                   Z-ADD     WDA8          G08DAT
029300000000     C                   Z-ADD     *ZEROS        G08INV
029400000000     C                   MOVEL     '0'           G08ERR
029500000000     C                   CALL      'XSRDA8'
029600000000     C                   PARM                    WLBDA8
029700000000     C                   Z-ADD     G08DAT        WDA8
029800000000IF  1C     G08ERR        IFEQ      '1'
029900000000     C                   MOVEL     '1'           $ERR
030000000000     C                   ELSE
030100000000     C                   Z-ADD     G08INV        DA8W
030200000000E   1C                   ENDIF
030300000000     C*
030400000000     C                   ENDSR
030500000000     C*------------------------------------------------------------------------*
030600000000     C* IMPVID - IMPOSTAZIONI VIDEO
030700000000     C*------------------------------------------------------------------------*
030800000000     C     IMPVID        BEGSR
030900000000     C*
031000090601     c                   MOVE      'R'           V1tipo                         *lancio
031100000000     C*
031200000000     C                   ENDSR
031300020917     C*--------------------------------------------------------------------------------------------*
031400020917     C* REPERISCE I DATI UTENTE
031500020917     C*--------------------------------------------------------------------------------------------*
031600020917     C     REPDATIUTE    BEGSR
031700020917     C*
031800020917     C* INIZIALIZZA VARIABILI DI WRK
031900020917     C                   CLEAR                   TIBS34DS
032000020917     C                   CLEAR                   AZUTEDS
032100020917     C                   CLEAR                   DDATIUTE
032200020917     C*
032300020917     C     *DTAARA       DEFINE    §azute        azuteds
032400020917     C     *DTAARA       DEFINE    §datiute      ddatiute
032500020917     C                   IN(E)     *DTAARA
032600020917if  1C                   IF        %Error
032700020917     c                   EVAL      I34Tla = 'L'
032800020917     C                   CALL      'TIBS34R'
032900020917     C                   PARM                    Tibs34Ds
033000020917     C                   IN        *DTAARA
033100020917e   1C                   ENDIF
033200020917     C*
033300020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
033400020917     C                   MOVEL(P)  RSUT          DSFIRS           20
033500020917     C*
033600020917     C                   ENDSR
033700000000     C*------------------------------------------------------------------------*
033800000000     C* *INZSR - ROUTINE INIZIALE
033900000000     C*------------------------------------------------------------------------*
034000000000     C     *INZSR        BEGSR
034100000000     C*--------------------
034200000000     C* RICEVIMENTO PARAMETRI
034300000000     C*--------------------
034400000000     C     *ENTRY        PLIST
034500000000     C                   PARM                    KPJBA
034600000000     C*--------------------
034700000000     C* VARIABILI DI CONTROLLO
034800000000     C*--------------------
034900000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
035000000000     C***
035100000000     C* IMPOSTAZIONI VIDEO
035200000000     C***
035300000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
035400000000     C*
035500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
035600000000     C                   EXSR      INZSTD
035700000000     C*
035800000000     C* IMPOSTAZIONI VIDEO
035900000000     C                   EXSR      IMPVID
036000000000     C*
036100000000     C* EMETTE VIDEATA
036200000000     C                   EXSR      CARV1
036300000000     C*
036400000000     C                   ENDSR
036500000000     C*------------------------------------------------------------------------*
036600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
036700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
036800000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
036900000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
037000000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
037100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
037200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
037300000000     C*------------------------------------------------------------------------*
037400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
037500000000     C*------------------------------------------------------------------------*
037600000000     C     INZSTD        BEGSR
037700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
037800000000     C                   CALL      'XKEYS'
037900000000     C                   PARM                    DSKEYS
038000000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
038100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
038200000000     C     XMSGPA        PLIST
038300000000     C                   PARM                    DSMSMS
038400000000     C                   PARM                    DSMSPG
038500000000     C                   PARM                    DSMSKE
038600000000     C                   PARM                    DSMSER
038700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
038800121120     C                   MOVEL     'TNSSL4R'     DSMSPG                         *NOME PGM CHIAMANTE
038900121120     C                   MOVEL     'TNSSL4R'     VMCQUE                         *CODA MESSAGGI DESTI
039000000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
039100000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
039200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
039300000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
039400000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
039500000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
039600000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
039700000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
039800000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
039900000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
040000000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
040100000000     C* LANCIO ROUTINE PER VERIFICARE SE
040200000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
040300000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
040400020917     C                   EXSR      REPDATIUTE
040500000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
040600000000     C                   BITOFF    '01234567'    POSMI1            1
040700000000     C                   BITOFF    '01234567'    POSMI2            1
040800000000     C                   BITOFF    '01234567'    POSMA1            1
040900000000     C                   BITOFF    '01234567'    POSMA2            1
041000000000     C                   BITON     '34'          POSMI1            1            *18
041100000000     C                   BITON     '7'           POSMI2            1            *01
041200000000     C                   BITON     '34'          POSMA1            1            *18
041300000000     C                   BITON     '0'           POSMA2            1            *80
041400000000     C                   MOVE      POSMI2        POSMIN            2
041500000000     C                   MOVEL     POSMI1        POSMIN
041600000000     C                   MOVE      POSMA2        POSMAX            2
041700000000     C                   MOVEL     POSMA1        POSMAX
041800000000     C                   ENDSR
041900000000     C*------------------------------------------------------------------------*
042000000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
042100000000     C*------------------------------------------------------------------------*
042200000000     C     FUNSTD        BEGSR
042300000000     C                   SELECT
042400000000     C     KEY           WHENEQ    F3
042500000000     C                   EXSR      F03STD
042600000000     C     KEY           WHENEQ    F12
042700000000     C                   EXSR      F12STD
042800000000     C     KEY           WHENEQ    F24
042900000000     C                   EXSR      F24STD
043000000000     C                   OTHER
043100000000     C                   MOVEL     MSG(2)        $MSG
043200000000     C                   EXSR      CARMSG
043300000000     C                   ENDSL
043400000000     C                   ENDSR
043500000000     C*------------------------------------------------------------------------*
043600000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
043700000000     C*------------------------------------------------------------------------*
043800000000     C     F03STD        BEGSR
043900000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
044000000000     C                   MOVEL     '1'           $F03
044100000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
044200000000     C                   MOVEL     '1'           $FINE
044300000000     C                   ENDSR
044400000000     C*------------------------------------------------------------------------*
044500000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
044600000000     C*------------------------------------------------------------------------*
044700000000     C     F12STD        BEGSR
044800000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
044900000000     C                   MOVEL     '1'           $F12
045000000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
045100000000     C                   MOVEL     '1'           $FINE
045200000000     C                   ENDSR
045300000000     C*------------------------------------------------------------------------*
045400000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
045500000000     C*------------------------------------------------------------------------*
045600000000     C     F24STD        BEGSR
045700000000     C*
045800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
045900000000     C                   EXSR      OUTFUN
046000000000     C*
046100000000     C                   ENDSR
046200090527     C*------------------------------------------------------------------------*
046300090527     C* F09GEs - GEST. TASTO FUNZ. F09          ==> skedulazione
046400090527     C*------------------------------------------------------------------------*
046500090527     C     F09GES        BEGSR
046600090527     C*
046700090527     C                   MOVE      '1'           KRITB
046800121120     C                   MOVEL     'SSL7'        KCOAZ
046900090527     c
047000090527     c                   call      'BCH09'
047100090527     c                   parm                    kpjba
047200090527     C*
047300090527     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
047400090527     C*
047500090527     C                   ENDSR
047600000000     C*------------------------------------------------------------------------*
047700000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
047800000000     C*          CODA MESSAGGI DEL PGM
047900000000     C*------------------------------------------------------------------------*
048000000000     C     CARMSG        BEGSR
048100000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
048200000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
048300000000     C     MSGULT        IFEQ      0
048400000000     C                   MOVEL     '1'           *IN40
048500121120     C                   WRITE     SSL4DVK
048600000000     C                   MOVEL     '0'           *IN40
048700000000     C                   ENDIF
048800000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
048900000000     C                   MOVEL     $MSG          DSMSMS
049000000000     C                   CALL      'XMSG'        XMSGPA
049100000000     C     DSMSER        IFEQ      0
049200000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
049300000000     C                   ENDIF
049400000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
049500000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
049600121120     C                   WRITE     SSL4DVM                                      *SFL MESS
049700000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
049800000000     C                   MOVEL     *BLANKS       $MSG
049900000000     C                   ENDSR
050000000000     C*------------------------------------------------------------------------*
050100000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
050200000000     C*------------------------------------------------------------------------*
050300000000     C     OUTMSG        BEGSR
050400000000     C* VISUALIZZAZIONE MESSAGGIO
050500000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
050600121120     C                   WRITE     SSL4DVK                                      *CONTR MESS
050700000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
050800000000     C                   Z-ADD     *ZEROS        MSGULT
050900000000     C                   ENDSR
051000000000     C*------------------------------------------------------------------------*
051100000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
051200000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
051300000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
051400000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
051500000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
051600000000     C*------------------------------------------------------------------------*
051700000000     C     CARFUN        BEGSR
051800000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
051900000000     C     1             DO        31            I                 5 0          --- 1 -->
052000000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
052100000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
052200000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
052300020917     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
052400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
052500000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
052600000000     C                   ELSE                                                   *INTERROG.
052700000000     C                   MOVEL     FUNASI        FAT(I)
052800000000     C                   ENDIF
052900000000     C                   ELSE                                                   <-- 2 --> *FILIALE
053000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
053100000000     C                   MOVEL     FUNAFG        FAT(I)
053200000000     C                   ELSE                                                   *INTERROG.
053300000000     C                   MOVEL     FUNAFI        FAT(I)
053400000000     C                   ENDIF
053500000000     C                   ENDIF                                                  <-- 2 ---
053600000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
053700000000     C                   MOVEL     USER          WEDP              3            *PROFILO
053800000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
053900000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
054000000000     C                   ELSE
054100000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
054200000000     C                   ENDIF
054300000000     C                   ENDIF
054400000000     C                   ENDDO                                                  <-- 1 ---
054500000000     C                   ENDSR
054600000000     C*------------------------------------------------------------------------*
054700000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
054800000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
054900000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
055000000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
055100000000     C*------------------------------------------------------------------------*
055200000000     C     OUTFUN        BEGSR
055300000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
055400000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
055500000000     C                   ENDDO
055600000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
055700000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
055800000000     C     1             DO        31            I
055900000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
056000000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
056100000000     C                   ADD       1             FUNNAT
056200000000     C                   ENDIF
056300000000     C                   ENDDO
056400000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
056500000000     C     FUNNAT        IFGT      8
056600000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
056700000000     C                   ENDIF
056800000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
056900000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
057000000000     C                   Z-ADD     8             FUNVIS            5 0
057100000000     C     FAT(23)       IFEQ      'S'
057200000000     C                   SUB       1             FUNVIS
057300000000     C                   ENDIF
057400000000     C     FAT(24)       IFEQ      'S'
057500000000     C                   SUB       1             FUNVIS
057600000000     C                   ENDIF
057700000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
057800000000     C                   Z-ADD     *ZEROS        W                 5 0
057900000000     C                   ADD       1             FUNULT
058000000000     C     FUNULT        IFGT      31
058100000000     C                   Z-ADD     1             FUNULT
058200000000     C                   ENDIF
058300000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
058400000000     C     FUNULT        DO        31            I                              --- 1 -->
058500000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
058600000000     C     W             IFLT      FUNVIS                                       --- 2 -->
058700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
058800000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
058900000000     C     FDE(I)        ANDNE     *BLANKS
059000000000     C     I             IFEQ      23                                           *TOLGO F23
059100000000     C     I             OREQ      24                                           *E F24
059200000000     C                   ELSE
059300000000     C                   ADD       1             W
059400000000     C                   MOVEL     FDE(I)        FUV(W)
059500000000     C                   ENDIF
059600000000     C                   ENDIF                                                  <-- 3 ---
059700000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
059800000000     C                   Z-ADD     I             FUNULT
059900000000     C                   ENDIF                                                  <-- 2 ---
060000000000     C                   ENDDO                                                  <-- 1 ---
060100000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
060200000000     C                   MOVEL     FUV(1)        VFD001
060300000000     C                   MOVEL     FUV(2)        VFD002
060400000000     C                   MOVEL     FUV(3)        VFD003
060500000000     C                   MOVEL     FUV(4)        VFD004
060600000000     C                   MOVEL     FUV(5)        VFD005
060700000000     C                   MOVEL     FUV(6)        VFD006
060800000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
060900000000     C     FAT(24)       ANDEQ     'N'
061000000000     C                   MOVEL     FUV(7)        VFD007
061100000000     C                   MOVEL     FUV(8)        VFD008
061200000000     C                   ELSE                                                   <-- 1 -->
061300000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
061400000000     C     FAT(24)       ANDEQ     'S'
061500000000     C                   MOVEL     FDE(23)       VFD007
061600000000     C                   MOVEL     FDE(24)       VFD008
061700000000     C                   ELSE                                                   <-- 2 -->
061800000000     C                   MOVEL     FUV(7)        VFD007
061900000000     C     FAT(23)       IFEQ      'S'
062000000000     C                   MOVEL     FDE(23)       VFD008
062100000000     C                   ELSE
062200000000     C                   MOVEL     FDE(24)       VFD008
062300000000     C                   ENDIF
062400000000     C                   ENDIF                                                  <-- 2 ---
062500000000     C                   ENDIF                                                  <-- 1 ---
062600000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
062700000000     C                   ENDSR
062800000000     C*------------------------------------------------------------------------*
062900000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
063000000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
063100000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
063200000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
063300000000     C*------------------------------------------------------------------------*
063400000000     C     CONFUN        BEGSR
063500000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
063600000000     C     1             DO        31            I                              --- 1 -->
063700000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
063800000000     C* CONTROLLO SE IL TASTO E' ATTIVO
063900000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
064000000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
064100000000     C                   EXSR      CARMSG
064200000000     C                   MOVEL     '1'           $ERR
064300000000     C                   ENDIF                                                  <-- 3 ---
064400000000     C                   ENDIF                                                  <-- 2 ---
064500000000     C                   ENDDO                                                  <-- 1 ---
064600000000     C                   ENDSR
064700000000     O*------------------------------------------------------------------------*
064800000000** MSG - (MESSAGGI)
064900121120TNSSL4R- Tasto funzionale non valido.                                          1
065000121120TNSSL4R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
065100121120TNSSL4R- Opzione non valida.                                                   3
065200121120TNSSL4R- Opzione abilitata ma non gestita. Avvertire CED.                      4
065300121120TNSSL4R- Inizio elenco.                                                        5
065400121120TNSSL4R- Fine elenco.                                                          6
065500121120TNSSL4R- Data errata                                                           7
065600121120TNSSL4R- Opzione Errata                                                        8
065700121120TNSSL4R- Se richiesta solo una data impossibile elaborare tipo "R"             9
065800121120TNSSL4R- Stampa lanciata ....                                                 10
065900121120TNSSL4R- Se richiesto range di date impossibile elaborare tipo "G"            11
066000121120Indicare il tipo di elaborazione
066100000000** CMD  (COMANDI)
066200000000WRKSPLF
066300000000
066400000000
066500000000
066600000000
066700000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
066800000000F1    -                   -    NN     NN
066900000000F2    -                   -    NN     NN
067000000000F3    -F3=Fine            -    SS     SS
067100000000F4    -                   -    NN     NN
067200000000F5    -                   -    NN     NN
067300000000F6    -F6=Conferma        -    SS     SS
067400000000F7    -                   -    NN     NN
067500000000F8    -                   -    NN     NN
067600090527F9    -F9=Gest.Skedulaz.  -    SS     SS
067700000000F10   -                   -    NN     NN
067800000000F11   -                   -    NN     NN
067900000000F12   -F12=Ritorno        -    SS     SS
068000000000F13   -                   -    NN     NN
068100000000F14   -                   -    NN     NN
068200000000F15   -                   -    NN     NN
068300000000F16   -                   -    NN     NN
068400121120F17   -                   -    NN     NN
068500000000F18   -                   -    NN     NN
068600000000F19   -                   -    NN     NN
068700000000F20   -                   -    NN     NN
068800000000F21   -                   -    NN     NN
068900000000F22   -F22=Gestione stampe-    EE     EE
069000000000F23   -                   -    NN     NN
069100000000F24   -F24=Altri tasti    -    NN     NN
069200000000ENTER -                   -    SS     SS
069300000000ROLLUP-                   -    NN     NN
069400000000ROLLDN-                   -    NN     NN
069500000000HELP  -                   -    NN     NN
069600000000PRINT -                   -    NN     NN
069700000000BSPACE-                   -    NN     NN
069800000000CLEAR -                   -    NN     NN
