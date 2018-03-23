000100000000     H*------------------------------------------------------------------------*
000200970529     H* RICHIESTA PARAMETRI STATISTICHE PERIODICHE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800970529     FTNSSL0D   CF   E             WORKSTN INFDS(DEVDS1)
000900970529     F                                     SFILE(SSL0DVM:MSGULT)
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
012000970529     C                   EXFMT     SSL0DV1                                      *PARAMETRI
012100000000     C*
012200000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012300000000     C*
012400000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012500000000SL  2C                   SELECT
012600000000     C     KEY           WHENEQ    ENTER
012700000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012800000000     C     KEY           WHENEQ    F6
012900000000     C                   EXSR      F06GES                                       *CONFERMA
013000090901     C     KEY           WHENEQ    F17
013100090901     C                   EXSR      F17GES                                       *attiva opzione 2
013200000000     C     KEY           WHENEQ    F22
013300000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
013400090527     C     KEY           WHENEQ    F9
013500090527     C                   EXSR      F09GES                                       *GESTIONE skedula
013600000000     C                   OTHER
013700000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013800000000E   2C                   ENDSL
013900000000E   1C                   ENDIF
014000000000     C*
014100000000     C                   ENDSR
014200000000     C*------------------------------------------------------------------------*
014300000000     C* CARV1 - CARICA VIDEO UNO
014400000000     C*------------------------------------------------------------------------*
014500000000     C     CARV1         BEGSR
014600000000     C*
014700000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014800000000     C*
014900000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015000000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015100000000     C*
015200970529     C                   WRITE     SSL0DVT                                      *TESTATA
015300970529     C                   WRITE     SSL0DVF                                      *FUNZIONI
015400000000     C*
015500000000     C                   ENDSR
015600000000     C*------------------------------------------------------------------------*
015700000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
015800000000     C*------------------------------------------------------------------------*
015900000000     C     ENTV1         BEGSR
016000000000     C*
016100000000     C* CONTROLLA I CAMPI IMMESSI
016200000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016300000000     C*
016400000000     C                   ENDSR
016500000000     C*------------------------------------------------------------------------*
016600000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
016700000000     C*------------------------------------------------------------------------*
016800000000     C     F06GES        BEGSR
016900000000     C*
017000000000     C* CONTROLLA I CAMPI IMMESSI
017100000000     C                   EXSR      DECODI
017200000000     C*
017300000000     C* LANCIA IL PROGRAMMA DI STAMPA
017400000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
017500000000     C                   Z-ADD     DTIV1C        PARDTI
017600000000     C                   Z-ADD     DTFV1C        PARDTF
017700970529     C                   MOVE      V1OPZ         PAROPZ
017800090601     C                   MOVE      V1tipo        PARtipo
017900000000     C*
018000970529     C     PAROPZ        IFEQ      '1'                                          DDT
018100970529     C                   MOVEL     'SSL1'        KCOAZ
018200970529     C                   ENDIF
018300970529     C     PAROPZ        IFEQ      '2'                                          STAT.SETTIMANALE
018400970529     C                   MOVEL     'SSL3'        KCOAZ
018500970529     C                   ENDIF
018600090206     C     PAROPZ        IFEQ      '3'
018700120905     C     PAROPZ        oreq      '4'
018800090206     C                   MOVEL     'SSLR'        KCOAZ
018900090206     C                   ENDIF
019000110125
019100000000     C                   MOVEL     PARAM         KPJBU
019200000000     C                   CALL      'BCH10'
019300000000     C                   PARM                    KPJBA
019400000000     C*
019500000000     C                   MOVEL     MSG(10)       $MSG                           *STAMPA LANCIATA
019600090901     c
019700090901     c                   setoff                                       87
019800000000E   1C                   ENDIF
019900000000     C*
020000000000     C                   ENDSR
020100090901     C*------------------------------------------------------------------------*
020200090901     C* F17GES -attiva opzione 2 per elaborazione wslss00F
020300090901     C*------------------------------------------------------------------------*
020400090901     C     F17GES        BEGSR
020500090901     c* Accendo l'indicatore che mi attiva la possibilità ddi richiedere
020600090901     c*  l'opzione "2"
020700090901     c                   seton                                        87
020800090901     C*
020900090901     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
021000090901     C*
021100090901     C                   ENDSR
021200000000     C*------------------------------------------------------------------------*
021300000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
021400000000     C*------------------------------------------------------------------------*
021500000000     C     F22GES        BEGSR
021600000000     C*
021700000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
021800000000     C                   Z-ADD     80            LENGH            15 5
021900000000     C                   CALL      'QCAEXEC'
022000000000     C                   PARM                    CMD(1)
022100000000     C                   PARM                    LENGH
022200000000     C*
022300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
022400000000     C*
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
022800000000     C*------------------------------------------------------------------------*
022900000000     C     DECODI        BEGSR
023000000000     C*
023100000000     C* REIMPOSTA INDICATORI DI ERRORE
023200000000     C                   SETOFF                                       606162
023300090601     C                   SETOFF                                       63
023400000000     C*--------------------
023500000000     C* DATA INIZIO
023600000000     C*--------------------
023700000000     C     *LIKE         DEFINE    V1CDTI        DTIV1C
023800000000     C                   Z-ADD     *ZEROS        DTIV1C
023900000000IF  1C     $ERR          IFEQ      '0'
024000000000     C                   Z-ADD     V1CDTI        WDA8
024100000000     C                   Z-ADD     *ZEROS        DA8W
024200000000     C                   EXSR      CHKDAT
024300000000     C                   Z-ADD     WDA8          V1CDTI
024400000000     C                   Z-ADD     DA8W          DTIV1C
024500000000IF  2C     $ERR          IFEQ      '1'
024600000000     C                   SETON                                        60
024700000000     C                   MOVEL     MSG(7)        $MSG
024800000000     C                   EXSR      CARMSG
024900000000E   2C                   ENDIF
025000000000E   1C                   ENDIF
025100000000     C*--------------------
025200000000     C* DATA FINE
025300000000     C*--------------------
025400000000     C     *LIKE         DEFINE    V1CDTF        DTFV1C
025500000000     C                   Z-ADD     *ZEROS        DTFV1C
025600000000IF  1C     $ERR          IFEQ      '0'
025700000000     C                   Z-ADD     V1CDTF        WDA8
025800000000     C                   Z-ADD     *ZEROS        DA8W
025900000000     C                   EXSR      CHKDAT
026000000000     C                   Z-ADD     WDA8          V1CDTF
026100000000     C                   Z-ADD     DA8W          DTFV1C
026200000000IF  2C     $ERR          IFEQ      '1'
026300000000     C                   SETON                                        61
026400000000     C                   MOVEL     MSG(7)        $MSG
026500000000     C                   EXSR      CARMSG
026600000000E   2C                   ENDIF
026700000000E   1C                   ENDIF
026800090601
026900090601     c* Data fine non può essere < data inizio
027000090601IF  1C     $ERR          IFEQ      '0'
027100090601     c                   if        dtfv1c<dtiv1c
027200090601     C                   MOVEL     '1'           $ERR
027300090601     C                   SETON                                        61
027400090601     C                   MOVEL     MSG(7)        $MSG
027500090601     C                   EXSR      CARMSG
027600090601E   2C                   ENDIF
027700090601E   1C                   ENDIF
027800000000     C*--------------------
027900000000     C* NUMERO BOLLE MINIMO GIORNALIERE
028000000000     C*--------------------
028100000000IF  1C     $ERR          IFEQ      '0'
028200970529IF  2C     V1OPZ         IFEQ      *BLANK
028300000000     C                   MOVEL     '1'           $ERR
028400000000     C                   SETON                                        62
028500000000     C                   MOVEL     MSG(8)        $MSG
028600000000     C                   EXSR      CARMSG
028700000000E   2C                   ENDIF
028800000000E   1C                   ENDIF
028900090901     c
029000090901     c* Se richiesta  opzione 2 si deve aver premutoF17
029100090901     c                   if        v1opz='2' and *in87=*off
029200090901     C                   MOVEL     '1'           $ERR
029300090901     C                   SETON                                        62
029400090901     C                   MOVEL     MSG(13)       $MSG
029500090901     C                   EXSR      CARMSG
029600090901E   2C                   ENDIF
029700090601     c
029800090601     c* Tipo opzione in base alle date
029900090601IF  1C     $ERR          IFEQ      '0'
030000090601     c                   if        v1tipo=' '  and v1opz='3'
030100090601     C                   MOVEL     '1'           $ERR
030200090601     C                   SETON                                        63
030300090601     C                   MOVEL     MSG(12)       $MSG
030400090601     C                   EXSR      CARMSG
030500090601E   2C                   ENDIF
030600090601E   1C                   ENDIF
030700090601     c*
030800100707IF  1C**   $ERR          IFEQ      '0'
030900100707     c**                 if        dtIv1c=dtfv1c and v1tipo='R'
031000100707     c**                           and v1opz='3'
031100100707     C**                 MOVEL     '1'           $ERR
031200100707     C**                 SETON                                        63
031300100707     C**                 MOVEL     MSG(9)        $MSG
031400100707     C**                 EXSR      CARMSG
031500100707E   2C**                 ENDIF
031600100707E   1C**                 ENDIF
031700090601     c*
031800090601IF  1C     $ERR          IFEQ      '0'
031900090601     c                   if        dtFv1c>dtiv1c and v1tipo='G'
032000090603     c                             and v1opz='3'
032100090601     C                   MOVEL     '1'           $ERR
032200090601     C                   SETON                                        63
032300090601     C                   MOVEL     MSG(11)       $MSG
032400090601     C                   EXSR      CARMSG
032500090601E   2C                   ENDIF
032600090601E   1C                   ENDIF
032700090901c
032800090901     c* Se richeista opzione "3" disabilito opzione "2"
032900090901IF  1C     $ERR          IFEQ      '0'
033000090901     c                   if        v1opz='3'
033100090901     c                   setoff                                       87
033200090901     c                   endif
033300090901     c                   endif
033400000000     C*
033500000000     C                   ENDSR
033600000000     C*------------------------------------------------------------------------*
033700000000     C* CHKDAT - CONTROLLA LA DATA
033800000000     C*          INPUT  -> WDA8 - DATA (GG/MM/AAAA)
033900000000     C*          OUTPUT -> DA8W - DATA (AAAA/MM/GG)
034000000000     C*------------------------------------------------------------------------*
034100000000     C     CHKDAT        BEGSR
034200000000     C*
034300000000     C     *LIKE         DEFINE    V1CDTI        WDA8
034400000000     C     *LIKE         DEFINE    V1CDTI        DA8W
034500000000     C*
034600000000     C                   Z-ADD     *ZEROS        DA8W
034700000000     C*
034800000000     C                   Z-ADD     WDA8          G08DAT
034900000000     C                   Z-ADD     *ZEROS        G08INV
035000000000     C                   MOVEL     '0'           G08ERR
035100000000     C                   CALL      'XSRDA8'
035200000000     C                   PARM                    WLBDA8
035300000000     C                   Z-ADD     G08DAT        WDA8
035400000000IF  1C     G08ERR        IFEQ      '1'
035500000000     C                   MOVEL     '1'           $ERR
035600000000     C                   ELSE
035700000000     C                   Z-ADD     G08INV        DA8W
035800000000E   1C                   ENDIF
035900000000     C*
036000000000     C                   ENDSR
036100000000     C*------------------------------------------------------------------------*
036200000000     C* IMPVID - IMPOSTAZIONI VIDEO
036300000000     C*------------------------------------------------------------------------*
036400000000     C     IMPVID        BEGSR
036500000000     C*
036600090601     C                   MOVE      '3'           V1OPZ                          *lancio
036700090601     c                   MOVE      'R'           V1tipo                         *lancio
036800000000     C*
036900000000     C                   ENDSR
037000020917     C*--------------------------------------------------------------------------------------------*
037100020917     C* REPERISCE I DATI UTENTE
037200020917     C*--------------------------------------------------------------------------------------------*
037300020917     C     REPDATIUTE    BEGSR
037400020917     C*
037500020917     C* INIZIALIZZA VARIABILI DI WRK
037600020917     C                   CLEAR                   TIBS34DS
037700020917     C                   CLEAR                   AZUTEDS
037800020917     C                   CLEAR                   DDATIUTE
037900020917     C*
038000020917     C     *DTAARA       DEFINE    §azute        azuteds
038100020917     C     *DTAARA       DEFINE    §datiute      ddatiute
038200020917     C                   IN(E)     *DTAARA
038300020917if  1C                   IF        %Error
038400020917     c                   EVAL      I34Tla = 'L'
038500020917     C                   CALL      'TIBS34R'
038600020917     C                   PARM                    Tibs34Ds
038700020917     C                   IN        *DTAARA
038800020917e   1C                   ENDIF
038900020917     C*
039000020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
039100020917     C                   MOVEL(P)  RSUT          DSFIRS           20
039200020917     C*
039300020917     C                   ENDSR
039400000000     C*------------------------------------------------------------------------*
039500000000     C* *INZSR - ROUTINE INIZIALE
039600000000     C*------------------------------------------------------------------------*
039700000000     C     *INZSR        BEGSR
039800000000     C*--------------------
039900000000     C* RICEVIMENTO PARAMETRI
040000000000     C*--------------------
040100000000     C     *ENTRY        PLIST
040200000000     C                   PARM                    KPJBA
040300000000     C*--------------------
040400000000     C* VARIABILI DI CONTROLLO
040500000000     C*--------------------
040600000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
040700000000     C***
040800000000     C* IMPOSTAZIONI VIDEO
040900000000     C***
041000000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
041100000000     C*
041200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
041300000000     C                   EXSR      INZSTD
041400000000     C*
041500000000     C* IMPOSTAZIONI VIDEO
041600000000     C                   EXSR      IMPVID
041700000000     C*
041800000000     C* EMETTE VIDEATA
041900000000     C                   EXSR      CARV1
042000000000     C*
042100000000     C                   ENDSR
042200000000     C*------------------------------------------------------------------------*
042300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
042400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
042500000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
042600000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
042700000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
042800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
042900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043000000000     C*------------------------------------------------------------------------*
043100000000     C* INZSTD - ROUTINE INIZIALE STANDARD
043200000000     C*------------------------------------------------------------------------*
043300000000     C     INZSTD        BEGSR
043400000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
043500000000     C                   CALL      'XKEYS'
043600000000     C                   PARM                    DSKEYS
043700000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
043800000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
043900000000     C     XMSGPA        PLIST
044000000000     C                   PARM                    DSMSMS
044100000000     C                   PARM                    DSMSPG
044200000000     C                   PARM                    DSMSKE
044300000000     C                   PARM                    DSMSER
044400000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
044500970529     C                   MOVEL     'TNSSL0R'     DSMSPG                         *NOME PGM CHIAMANTE
044600970529     C                   MOVEL     'TNSSL0R'     VMCQUE                         *CODA MESSAGGI DESTI
044700000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
044800000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
044900000000     C* INIZIALIZZO FLAG CONTROLLO PGM
045000000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
045100000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
045200000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
045300000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
045400000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
045500000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
045600000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
045700000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
045800000000     C* LANCIO ROUTINE PER VERIFICARE SE
045900000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
046000000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
046100020917     C                   EXSR      REPDATIUTE
046200000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
046300000000     C                   BITOFF    '01234567'    POSMI1            1
046400000000     C                   BITOFF    '01234567'    POSMI2            1
046500000000     C                   BITOFF    '01234567'    POSMA1            1
046600000000     C                   BITOFF    '01234567'    POSMA2            1
046700000000     C                   BITON     '34'          POSMI1            1            *18
046800000000     C                   BITON     '7'           POSMI2            1            *01
046900000000     C                   BITON     '34'          POSMA1            1            *18
047000000000     C                   BITON     '0'           POSMA2            1            *80
047100000000     C                   MOVE      POSMI2        POSMIN            2
047200000000     C                   MOVEL     POSMI1        POSMIN
047300000000     C                   MOVE      POSMA2        POSMAX            2
047400000000     C                   MOVEL     POSMA1        POSMAX
047500000000     C                   ENDSR
047600000000     C*------------------------------------------------------------------------*
047700000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
047800000000     C*------------------------------------------------------------------------*
047900000000     C     FUNSTD        BEGSR
048000000000     C                   SELECT
048100000000     C     KEY           WHENEQ    F3
048200000000     C                   EXSR      F03STD
048300000000     C     KEY           WHENEQ    F12
048400000000     C                   EXSR      F12STD
048500000000     C     KEY           WHENEQ    F24
048600000000     C                   EXSR      F24STD
048700000000     C                   OTHER
048800000000     C                   MOVEL     MSG(2)        $MSG
048900000000     C                   EXSR      CARMSG
049000000000     C                   ENDSL
049100000000     C                   ENDSR
049200000000     C*------------------------------------------------------------------------*
049300000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
049400000000     C*------------------------------------------------------------------------*
049500000000     C     F03STD        BEGSR
049600000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
049700000000     C                   MOVEL     '1'           $F03
049800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
049900000000     C                   MOVEL     '1'           $FINE
050000000000     C                   ENDSR
050100000000     C*------------------------------------------------------------------------*
050200000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
050300000000     C*------------------------------------------------------------------------*
050400000000     C     F12STD        BEGSR
050500000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
050600000000     C                   MOVEL     '1'           $F12
050700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
050800000000     C                   MOVEL     '1'           $FINE
050900000000     C                   ENDSR
051000000000     C*------------------------------------------------------------------------*
051100000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
051200000000     C*------------------------------------------------------------------------*
051300000000     C     F24STD        BEGSR
051400000000     C*
051500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
051600000000     C                   EXSR      OUTFUN
051700000000     C*
051800000000     C                   ENDSR
051900090527     C*------------------------------------------------------------------------*
052000090527     C* F09GEs - GEST. TASTO FUNZ. F09          ==> skedulazione
052100090527     C*------------------------------------------------------------------------*
052200090527     C     F09GES        BEGSR
052300090527     C*
052400090527     C                   MOVE      '1'           KRITB
052500090527     C     v1OPZ         IFEQ      '1'                                          DDT
052600090527     C                   MOVEL     'SSL1'        KCOAZ
052700090527     C                   ENDIF
052800090527     C     v1OPZ         IFEQ      '2'                                          STAT.SETTIMANALE
052900090527     C                   MOVEL     'SSL3'        KCOAZ
053000090527     C                   ENDIF
053100090527     C     v1OPZ         IFEQ      '3'
053200090527     C                   MOVEL     'SSLR'        KCOAZ
053300090527     C                   ENDIF
053400090527     c
053500090527     c                   call      'BCH09'
053600090527     c                   parm                    kpjba
053700090527     C*
053800090527     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
053900090527     C*
054000090527     C                   ENDSR
054100000000     C*------------------------------------------------------------------------*
054200000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
054300000000     C*          CODA MESSAGGI DEL PGM
054400000000     C*------------------------------------------------------------------------*
054500000000     C     CARMSG        BEGSR
054600000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
054700000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
054800000000     C     MSGULT        IFEQ      0
054900000000     C                   MOVEL     '1'           *IN40
055000970529     C                   WRITE     SSL0DVK
055100000000     C                   MOVEL     '0'           *IN40
055200000000     C                   ENDIF
055300000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
055400000000     C                   MOVEL     $MSG          DSMSMS
055500000000     C                   CALL      'XMSG'        XMSGPA
055600000000     C     DSMSER        IFEQ      0
055700000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
055800000000     C                   ENDIF
055900000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
056000000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
056100970529     C                   WRITE     SSL0DVM                                      *SFL MESS
056200000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
056300000000     C                   MOVEL     *BLANKS       $MSG
056400000000     C                   ENDSR
056500000000     C*------------------------------------------------------------------------*
056600000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
056700000000     C*------------------------------------------------------------------------*
056800000000     C     OUTMSG        BEGSR
056900000000     C* VISUALIZZAZIONE MESSAGGIO
057000000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
057100970529     C                   WRITE     SSL0DVK                                      *CONTR MESS
057200000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
057300000000     C                   Z-ADD     *ZEROS        MSGULT
057400000000     C                   ENDSR
057500000000     C*------------------------------------------------------------------------*
057600000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
057700000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
057800000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
057900000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
058000000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
058100000000     C*------------------------------------------------------------------------*
058200000000     C     CARFUN        BEGSR
058300000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
058400000000     C     1             DO        31            I                 5 0          --- 1 -->
058500000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
058600000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
058700000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
058800020917     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
058900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
059000000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
059100000000     C                   ELSE                                                   *INTERROG.
059200000000     C                   MOVEL     FUNASI        FAT(I)
059300000000     C                   ENDIF
059400000000     C                   ELSE                                                   <-- 2 --> *FILIALE
059500000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
059600000000     C                   MOVEL     FUNAFG        FAT(I)
059700000000     C                   ELSE                                                   *INTERROG.
059800000000     C                   MOVEL     FUNAFI        FAT(I)
059900000000     C                   ENDIF
060000000000     C                   ENDIF                                                  <-- 2 ---
060100000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
060200000000     C                   MOVEL     USER          WEDP              3            *PROFILO
060300000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
060400000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
060500000000     C                   ELSE
060600000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
060700000000     C                   ENDIF
060800000000     C                   ENDIF
060900000000     C                   ENDDO                                                  <-- 1 ---
061000000000     C                   ENDSR
061100000000     C*------------------------------------------------------------------------*
061200000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
061300000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
061400000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
061500000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
061600000000     C*------------------------------------------------------------------------*
061700000000     C     OUTFUN        BEGSR
061800000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
061900000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
062000000000     C                   ENDDO
062100000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
062200000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
062300000000     C     1             DO        31            I
062400000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
062500000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
062600000000     C                   ADD       1             FUNNAT
062700000000     C                   ENDIF
062800000000     C                   ENDDO
062900000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
063000000000     C     FUNNAT        IFGT      8
063100000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
063200000000     C                   ENDIF
063300000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
063400000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
063500000000     C                   Z-ADD     8             FUNVIS            5 0
063600000000     C     FAT(23)       IFEQ      'S'
063700000000     C                   SUB       1             FUNVIS
063800000000     C                   ENDIF
063900000000     C     FAT(24)       IFEQ      'S'
064000000000     C                   SUB       1             FUNVIS
064100000000     C                   ENDIF
064200000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
064300000000     C                   Z-ADD     *ZEROS        W                 5 0
064400000000     C                   ADD       1             FUNULT
064500000000     C     FUNULT        IFGT      31
064600000000     C                   Z-ADD     1             FUNULT
064700000000     C                   ENDIF
064800000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
064900000000     C     FUNULT        DO        31            I                              --- 1 -->
065000000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
065100000000     C     W             IFLT      FUNVIS                                       --- 2 -->
065200000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
065300000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
065400000000     C     FDE(I)        ANDNE     *BLANKS
065500000000     C     I             IFEQ      23                                           *TOLGO F23
065600000000     C     I             OREQ      24                                           *E F24
065700000000     C                   ELSE
065800000000     C                   ADD       1             W
065900000000     C                   MOVEL     FDE(I)        FUV(W)
066000000000     C                   ENDIF
066100000000     C                   ENDIF                                                  <-- 3 ---
066200000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
066300000000     C                   Z-ADD     I             FUNULT
066400000000     C                   ENDIF                                                  <-- 2 ---
066500000000     C                   ENDDO                                                  <-- 1 ---
066600000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
066700000000     C                   MOVEL     FUV(1)        VFD001
066800000000     C                   MOVEL     FUV(2)        VFD002
066900000000     C                   MOVEL     FUV(3)        VFD003
067000000000     C                   MOVEL     FUV(4)        VFD004
067100000000     C                   MOVEL     FUV(5)        VFD005
067200000000     C                   MOVEL     FUV(6)        VFD006
067300000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
067400000000     C     FAT(24)       ANDEQ     'N'
067500000000     C                   MOVEL     FUV(7)        VFD007
067600000000     C                   MOVEL     FUV(8)        VFD008
067700000000     C                   ELSE                                                   <-- 1 -->
067800000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
067900000000     C     FAT(24)       ANDEQ     'S'
068000000000     C                   MOVEL     FDE(23)       VFD007
068100000000     C                   MOVEL     FDE(24)       VFD008
068200000000     C                   ELSE                                                   <-- 2 -->
068300000000     C                   MOVEL     FUV(7)        VFD007
068400000000     C     FAT(23)       IFEQ      'S'
068500000000     C                   MOVEL     FDE(23)       VFD008
068600000000     C                   ELSE
068700000000     C                   MOVEL     FDE(24)       VFD008
068800000000     C                   ENDIF
068900000000     C                   ENDIF                                                  <-- 2 ---
069000000000     C                   ENDIF                                                  <-- 1 ---
069100000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
069200000000     C                   ENDSR
069300000000     C*------------------------------------------------------------------------*
069400000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
069500000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
069600000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
069700000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
069800000000     C*------------------------------------------------------------------------*
069900000000     C     CONFUN        BEGSR
070000000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
070100000000     C     1             DO        31            I                              --- 1 -->
070200000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
070300000000     C* CONTROLLO SE IL TASTO E' ATTIVO
070400000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
070500000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
070600000000     C                   EXSR      CARMSG
070700000000     C                   MOVEL     '1'           $ERR
070800000000     C                   ENDIF                                                  <-- 3 ---
070900000000     C                   ENDIF                                                  <-- 2 ---
071000000000     C                   ENDDO                                                  <-- 1 ---
071100000000     C                   ENDSR
071200000000     O*------------------------------------------------------------------------*
071300000000** MSG - (MESSAGGI)
071400970529TNSSL0R- Tasto funzionale non valido.                                          1
071500970529TNSSL0R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
071600970529TNSSL0R- Opzione non valida.                                                   3
071700970529TNSSL0R- Opzione abilitata ma non gestita. Avvertire CED.                      4
071800970529TNSSL0R- Inizio elenco.                                                        5
071900970529TNSSL0R- Fine elenco.                                                          6
072000970529TNSSL0R- Data errata                                                           7
072100970529TNSSL0R- Opzione Errata                                                        8
072200090601TNSSL0R- Se richiesta solo una data impossibile elaborare tipo "R"             9
072300970529TNSSL0R- Stampa lanciata ....                                                 10
072400090601TNSSL0R- Se richiesto range di date impossibile elaborare tipo "G"            11
072500090601TNSSL0R- Per opzione "3" indicare il tipo di elaborazione                     12
072600090901Per abilitare l'elaborazione dell'opzione "2" --> premere  F17                13
072700000000** CMD  (COMANDI)
072800000000WRKSPLF
072900000000
073000000000
073100000000
073200000000
073300000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
073400000000F1    -                   -    NN     NN
073500000000F2    -                   -    NN     NN
073600000000F3    -F3=Fine            -    SS     SS
073700000000F4    -                   -    NN     NN
073800000000F5    -                   -    NN     NN
073900000000F6    -F6=Conferma        -    SS     SS
074000000000F7    -                   -    NN     NN
074100000000F8    -                   -    NN     NN
074200090527F9    -F9=Gest.Skedulaz.  -    SS     SS
074300000000F10   -                   -    NN     NN
074400000000F11   -                   -    NN     NN
074500000000F12   -F12=Ritorno        -    SS     SS
074600000000F13   -                   -    NN     NN
074700000000F14   -                   -    NN     NN
074800000000F15   -                   -    NN     NN
074900000000F16   -                   -    NN     NN
075000090901F17   -F17=Attiva opz."2" -    SS     SS
075100000000F18   -                   -    NN     NN
075200000000F19   -                   -    NN     NN
075300000000F20   -                   -    NN     NN
075400000000F21   -                   -    NN     NN
075500000000F22   -F22=Gestione stampe-    EE     EE
075600000000F23   -                   -    NN     NN
075700000000F24   -F24=Altri tasti    -    NN     NN
075800000000ENTER -                   -    SS     SS
075900000000ROLLUP-                   -    NN     NN
076000000000ROLLDN-                   -    NN     NN
076100000000HELP  -                   -    NN     NN
076200000000PRINT -                   -    NN     NN
076300000000BSPACE-                   -    NN     NN
076400000000CLEAR -                   -    NN     NN
