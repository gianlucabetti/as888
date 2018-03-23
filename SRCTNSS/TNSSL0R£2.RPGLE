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
001900110125     D CHKO            s             40    dim(1) ctdata  perrcd(1)
002000000000     D*-------------------
002100000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002200000000     D*-------------------
002300000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002400000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002500000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002600000000     D*-------------------
002700000000     D* TABELLA DESCRIZIONI A VIDEO
002800000000     D*-------------------
002900000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003000000000     D*-------------------
003100000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003200000000     D*-------------------
003300000000     D KEYS            S              1    DIM(40)
003400000000     D*------------------------------------------------------------------------*
003500000000     D* INPUT
003600000000     D*------------------------------------------------------------------------*
003700000000     D*---------------------
003800000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003900000000     D*---------------------
004000000000     D DSFUN         E DS
004100000000     D  FUNRIG                 1     40
004200000000     D*---------------------
004300000000     D* DS PER GESTIONE TASTI FUNZIONALI
004400000000     D*---------------------
004500000000     D DSKEYS        E DS
004600000000     D  KEYRIG                 1     40
004700000000     D*---------------------
004800000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004900000000     D*---------------------
005000000000     D                SDS
005100000000     D  USER                 254    263
005200000000     D*---------------------
005300000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005400000000     D*---------------------
005500000000     D DEVDS1          DS
005600000000     D  KEY                  369    369
005700000000     D  CURSOR               370    371
005800000000     D*---------------------
005900000000     D* ARCHITETTURA
006000000000     D*---------------------
006100000000     D KPJBA         E DS
006200000000     D*---------------------
006300000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
006400000000     D*---------------------
006500000000     D DSMSG         E DS
006600000000     D*---------------------
006700020917     D* DS REPERIMENTO DATI UTENTE
006800020917     D*-------------------
006900020917     D TIBS34DS      E DS                                                       *Profili utente
007000020917     D DDATIUTE      E DS                                                       *Dati utente
007100020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007200000000     D*---------------------
007300000000     D* DS PARAMETRI DI LANCIO
007400000000     D*------------------
007500000000     D PARAM           DS                  INZ
007600000000     D  PARDTI                 1      8  0
007700000000     D  PARDTF                 9     16  0
007800970529     D  PAROPZ                17     17
007900090601     D  PARtipo               18     18
008000000000     D*---------------------
008100000000     D* DS CONTROLLO DATA (8)
008200000000     D*------------------
008300000000     D WLBDA8          DS                  INZ
008400000000     D  G08DAT                 1      8  0
008500000000     D  G08INV                 9     16  0
008600000000     D  G08ERR                17     17
008700000000     D  G08TGI                18     22  0
008800000000     C*------------------------------------------------------------------------*
008900000000     C* MAIN LINE
009000000000     C*------------------------------------------------------------------------*
009100090901     c                   setoff                                       87
009200000000     C*
009300000000     C* CICLO FINO A CHE:
009400000000DO  1C     $FINE         DOUEQ     '1'
009500000000     C*
009600000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009700000000     C*
009800000000     C* CONTROLLA GESTIONE VIDEO
009900000000IF  2C     $GES          IFEQ      'V1'
010000000000     C                   EXSR      GESV1                                        *PARAMETRI
010100000000E   2C                   ENDIF
010200000000     C*
010300000000E   1C                   ENDDO
010400000000     C*
010500000000     C                   SETON                                        LR
010600000000     C*------------------------------------------------------------------------*
010700000000     C* GESV1 - GESTIONE VIDEO UNO
010800000000     C*------------------------------------------------------------------------*
010900000000     C     GESV1         BEGSR
011000000000     C*
011100000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
011200000000     C                   EXSR      CARV1                                        *CARICA VIDEO
011300000000E   1C                   ENDIF
011400000000     C*
011500000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011600000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011700000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011800000000E   1C                   ENDIF
011900000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012000000000     C*
012100970529     C                   EXFMT     SSL0DV1                                      *PARAMETRI
012200000000     C*
012300000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012400000000     C*
012500000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012600000000SL  2C                   SELECT
012700000000     C     KEY           WHENEQ    ENTER
012800000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012900000000     C     KEY           WHENEQ    F6
013000000000     C                   EXSR      F06GES                                       *CONFERMA
013100090901     C     KEY           WHENEQ    F17
013200090901     C                   EXSR      F17GES                                       *attiva opzione 2
013300000000     C     KEY           WHENEQ    F22
013400000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
013500090527     C     KEY           WHENEQ    F9
013600090527     C                   EXSR      F09GES                                       *GESTIONE skedula
013700000000     C                   OTHER
013800000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013900000000E   2C                   ENDSL
014000000000E   1C                   ENDIF
014100000000     C*
014200000000     C                   ENDSR
014300000000     C*------------------------------------------------------------------------*
014400000000     C* CARV1 - CARICA VIDEO UNO
014500000000     C*------------------------------------------------------------------------*
014600000000     C     CARV1         BEGSR
014700000000     C*
014800000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014900000000     C*
015000000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015100000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015200000000     C*
015300970529     C                   WRITE     SSL0DVT                                      *TESTATA
015400970529     C                   WRITE     SSL0DVF                                      *FUNZIONI
015500000000     C*
015600000000     C                   ENDSR
015700000000     C*------------------------------------------------------------------------*
015800000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
015900000000     C*------------------------------------------------------------------------*
016000000000     C     ENTV1         BEGSR
016100000000     C*
016200000000     C* CONTROLLA I CAMPI IMMESSI
016300000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016400000000     C*
016500000000     C                   ENDSR
016600000000     C*------------------------------------------------------------------------*
016700000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
016800000000     C*------------------------------------------------------------------------*
016900000000     C     F06GES        BEGSR
017000000000     C*
017100000000     C* CONTROLLA I CAMPI IMMESSI
017200000000     C                   EXSR      DECODI
017300000000     C*
017400000000     C* LANCIA IL PROGRAMMA DI STAMPA
017500000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
017600000000     C                   Z-ADD     DTIV1C        PARDTI
017700000000     C                   Z-ADD     DTFV1C        PARDTF
017800970529     C                   MOVE      V1OPZ         PAROPZ
017900090601     C                   MOVE      V1tipo        PARtipo
018000000000     C*
018100970529     C     PAROPZ        IFEQ      '1'                                          DDT
018200970529     C                   MOVEL     'SSL1'        KCOAZ
018300970529     C                   ENDIF
018400970529     C     PAROPZ        IFEQ      '2'                                          STAT.SETTIMANALE
018500970529     C                   MOVEL     'SSL3'        KCOAZ
018600970529     C                   ENDIF
018700090206     C     PAROPZ        IFEQ      '3'
018800090206     C                   MOVEL     'SSLR'        KCOAZ
018900090206     C                   ENDIF
019000110125     c
019100110125     c                   eval      comman=chko(1)
019200110125     c                   call(e)   'QCMDEXC'
019300110125     c                   parm                    comman           80
019400110125     c                   parm      40            lung             15 5
019500110125     c                   if        not %error
019600110125     c                   eval      kqieb='K018SDINO'
019700110125     c                   endif
019800110125
019900000000     C                   MOVEL     PARAM         KPJBU
020000000000     C                   CALL      'BCH10'
020100000000     C                   PARM                    KPJBA
020200000000     C*
020300000000     C                   MOVEL     MSG(10)       $MSG                           *STAMPA LANCIATA
020400090901     c
020500090901     c                   setoff                                       87
020600000000E   1C                   ENDIF
020700000000     C*
020800000000     C                   ENDSR
020900090901     C*------------------------------------------------------------------------*
021000090901     C* F17GES -attiva opzione 2 per elaborazione wslss00F
021100090901     C*------------------------------------------------------------------------*
021200090901     C     F17GES        BEGSR
021300090901     c* Accendo l'indicatore che mi attiva la possibilità ddi richiedere
021400090901     c*  l'opzione "2"
021500090901     c                   seton                                        87
021600090901     C*
021700090901     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
021800090901     C*
021900090901     C                   ENDSR
022000000000     C*------------------------------------------------------------------------*
022100000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
022200000000     C*------------------------------------------------------------------------*
022300000000     C     F22GES        BEGSR
022400000000     C*
022500000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
022600000000     C                   Z-ADD     80            LENGH            15 5
022700000000     C                   CALL      'QCAEXEC'
022800000000     C                   PARM                    CMD(1)
022900000000     C                   PARM                    LENGH
023000000000     C*
023100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
023200000000     C*
023300000000     C                   ENDSR
023400000000     C*------------------------------------------------------------------------*
023500000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023600000000     C*------------------------------------------------------------------------*
023700000000     C     DECODI        BEGSR
023800000000     C*
023900000000     C* REIMPOSTA INDICATORI DI ERRORE
024000000000     C                   SETOFF                                       606162
024100090601     C                   SETOFF                                       63
024200000000     C*--------------------
024300000000     C* DATA INIZIO
024400000000     C*--------------------
024500000000     C     *LIKE         DEFINE    V1CDTI        DTIV1C
024600000000     C                   Z-ADD     *ZEROS        DTIV1C
024700000000IF  1C     $ERR          IFEQ      '0'
024800000000     C                   Z-ADD     V1CDTI        WDA8
024900000000     C                   Z-ADD     *ZEROS        DA8W
025000000000     C                   EXSR      CHKDAT
025100000000     C                   Z-ADD     WDA8          V1CDTI
025200000000     C                   Z-ADD     DA8W          DTIV1C
025300000000IF  2C     $ERR          IFEQ      '1'
025400000000     C                   SETON                                        60
025500000000     C                   MOVEL     MSG(7)        $MSG
025600000000     C                   EXSR      CARMSG
025700000000E   2C                   ENDIF
025800000000E   1C                   ENDIF
025900000000     C*--------------------
026000000000     C* DATA FINE
026100000000     C*--------------------
026200000000     C     *LIKE         DEFINE    V1CDTF        DTFV1C
026300000000     C                   Z-ADD     *ZEROS        DTFV1C
026400000000IF  1C     $ERR          IFEQ      '0'
026500000000     C                   Z-ADD     V1CDTF        WDA8
026600000000     C                   Z-ADD     *ZEROS        DA8W
026700000000     C                   EXSR      CHKDAT
026800000000     C                   Z-ADD     WDA8          V1CDTF
026900000000     C                   Z-ADD     DA8W          DTFV1C
027000000000IF  2C     $ERR          IFEQ      '1'
027100000000     C                   SETON                                        61
027200000000     C                   MOVEL     MSG(7)        $MSG
027300000000     C                   EXSR      CARMSG
027400000000E   2C                   ENDIF
027500000000E   1C                   ENDIF
027600090601
027700090601     c* Data fine non può essere < data inizio
027800090601IF  1C     $ERR          IFEQ      '0'
027900090601     c                   if        dtfv1c<dtiv1c
028000090601     C                   MOVEL     '1'           $ERR
028100090601     C                   SETON                                        61
028200090601     C                   MOVEL     MSG(7)        $MSG
028300090601     C                   EXSR      CARMSG
028400090601E   2C                   ENDIF
028500090601E   1C                   ENDIF
028600000000     C*--------------------
028700000000     C* NUMERO BOLLE MINIMO GIORNALIERE
028800000000     C*--------------------
028900000000IF  1C     $ERR          IFEQ      '0'
029000970529IF  2C     V1OPZ         IFEQ      *BLANK
029100000000     C                   MOVEL     '1'           $ERR
029200000000     C                   SETON                                        62
029300000000     C                   MOVEL     MSG(8)        $MSG
029400000000     C                   EXSR      CARMSG
029500000000E   2C                   ENDIF
029600000000E   1C                   ENDIF
029700090901     c
029800090901     c* Se richiesta  opzione 2 si deve aver premutoF17
029900090901     c                   if        v1opz='2' and *in87=*off
030000090901     C                   MOVEL     '1'           $ERR
030100090901     C                   SETON                                        62
030200090901     C                   MOVEL     MSG(13)       $MSG
030300090901     C                   EXSR      CARMSG
030400090901E   2C                   ENDIF
030500090601     c
030600090601     c* Tipo opzione in base alle date
030700090601IF  1C     $ERR          IFEQ      '0'
030800090601     c                   if        v1tipo=' '  and v1opz='3'
030900090601     C                   MOVEL     '1'           $ERR
031000090601     C                   SETON                                        63
031100090601     C                   MOVEL     MSG(12)       $MSG
031200090601     C                   EXSR      CARMSG
031300090601E   2C                   ENDIF
031400090601E   1C                   ENDIF
031500090601     c*
031600100707IF  1C**   $ERR          IFEQ      '0'
031700100707     c**                 if        dtIv1c=dtfv1c and v1tipo='R'
031800100707     c**                           and v1opz='3'
031900100707     C**                 MOVEL     '1'           $ERR
032000100707     C**                 SETON                                        63
032100100707     C**                 MOVEL     MSG(9)        $MSG
032200100707     C**                 EXSR      CARMSG
032300100707E   2C**                 ENDIF
032400100707E   1C**                 ENDIF
032500090601     c*
032600090601IF  1C     $ERR          IFEQ      '0'
032700090601     c                   if        dtFv1c>dtiv1c and v1tipo='G'
032800090603     c                             and v1opz='3'
032900090601     C                   MOVEL     '1'           $ERR
033000090601     C                   SETON                                        63
033100090601     C                   MOVEL     MSG(11)       $MSG
033200090601     C                   EXSR      CARMSG
033300090601E   2C                   ENDIF
033400090601E   1C                   ENDIF
033500090901c
033600090901     c* Se richeista opzione "3" disabilito opzione "2"
033700090901IF  1C     $ERR          IFEQ      '0'
033800090901     c                   if        v1opz='3'
033900090901     c                   setoff                                       87
034000090901     c                   endif
034100090901     c                   endif
034200000000     C*
034300000000     C                   ENDSR
034400000000     C*------------------------------------------------------------------------*
034500000000     C* CHKDAT - CONTROLLA LA DATA
034600000000     C*          INPUT  -> WDA8 - DATA (GG/MM/AAAA)
034700000000     C*          OUTPUT -> DA8W - DATA (AAAA/MM/GG)
034800000000     C*------------------------------------------------------------------------*
034900000000     C     CHKDAT        BEGSR
035000000000     C*
035100000000     C     *LIKE         DEFINE    V1CDTI        WDA8
035200000000     C     *LIKE         DEFINE    V1CDTI        DA8W
035300000000     C*
035400000000     C                   Z-ADD     *ZEROS        DA8W
035500000000     C*
035600000000     C                   Z-ADD     WDA8          G08DAT
035700000000     C                   Z-ADD     *ZEROS        G08INV
035800000000     C                   MOVEL     '0'           G08ERR
035900000000     C                   CALL      'XSRDA8'
036000000000     C                   PARM                    WLBDA8
036100000000     C                   Z-ADD     G08DAT        WDA8
036200000000IF  1C     G08ERR        IFEQ      '1'
036300000000     C                   MOVEL     '1'           $ERR
036400000000     C                   ELSE
036500000000     C                   Z-ADD     G08INV        DA8W
036600000000E   1C                   ENDIF
036700000000     C*
036800000000     C                   ENDSR
036900000000     C*------------------------------------------------------------------------*
037000000000     C* IMPVID - IMPOSTAZIONI VIDEO
037100000000     C*------------------------------------------------------------------------*
037200000000     C     IMPVID        BEGSR
037300000000     C*
037400090601     C                   MOVE      '3'           V1OPZ                          *lancio
037500090601     c                   MOVE      'R'           V1tipo                         *lancio
037600000000     C*
037700000000     C                   ENDSR
037800020917     C*--------------------------------------------------------------------------------------------*
037900020917     C* REPERISCE I DATI UTENTE
038000020917     C*--------------------------------------------------------------------------------------------*
038100020917     C     REPDATIUTE    BEGSR
038200020917     C*
038300020917     C* INIZIALIZZA VARIABILI DI WRK
038400020917     C                   CLEAR                   TIBS34DS
038500020917     C                   CLEAR                   AZUTEDS
038600020917     C                   CLEAR                   DDATIUTE
038700020917     C*
038800020917     C     *DTAARA       DEFINE    §azute        azuteds
038900020917     C     *DTAARA       DEFINE    §datiute      ddatiute
039000020917     C                   IN(E)     *DTAARA
039100020917if  1C                   IF        %Error
039200020917     c                   EVAL      I34Tla = 'L'
039300020917     C                   CALL      'TIBS34R'
039400020917     C                   PARM                    Tibs34Ds
039500020917     C                   IN        *DTAARA
039600020917e   1C                   ENDIF
039700020917     C*
039800020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
039900020917     C                   MOVEL(P)  RSUT          DSFIRS           20
040000020917     C*
040100020917     C                   ENDSR
040200000000     C*------------------------------------------------------------------------*
040300000000     C* *INZSR - ROUTINE INIZIALE
040400000000     C*------------------------------------------------------------------------*
040500000000     C     *INZSR        BEGSR
040600000000     C*--------------------
040700000000     C* RICEVIMENTO PARAMETRI
040800000000     C*--------------------
040900000000     C     *ENTRY        PLIST
041000000000     C                   PARM                    KPJBA
041100000000     C*--------------------
041200000000     C* VARIABILI DI CONTROLLO
041300000000     C*--------------------
041400000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
041500000000     C***
041600000000     C* IMPOSTAZIONI VIDEO
041700000000     C***
041800000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
041900000000     C*
042000000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
042100000000     C                   EXSR      INZSTD
042200000000     C*
042300000000     C* IMPOSTAZIONI VIDEO
042400000000     C                   EXSR      IMPVID
042500000000     C*
042600000000     C* EMETTE VIDEATA
042700000000     C                   EXSR      CARV1
042800000000     C*
042900000000     C                   ENDSR
043000000000     C*------------------------------------------------------------------------*
043100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043300000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
043400000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
043500000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
043600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043800000000     C*------------------------------------------------------------------------*
043900000000     C* INZSTD - ROUTINE INIZIALE STANDARD
044000000000     C*------------------------------------------------------------------------*
044100000000     C     INZSTD        BEGSR
044200000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
044300000000     C                   CALL      'XKEYS'
044400000000     C                   PARM                    DSKEYS
044500000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
044600000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
044700000000     C     XMSGPA        PLIST
044800000000     C                   PARM                    DSMSMS
044900000000     C                   PARM                    DSMSPG
045000000000     C                   PARM                    DSMSKE
045100000000     C                   PARM                    DSMSER
045200000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
045300970529     C                   MOVEL     'TNSSL0R'     DSMSPG                         *NOME PGM CHIAMANTE
045400970529     C                   MOVEL     'TNSSL0R'     VMCQUE                         *CODA MESSAGGI DESTI
045500000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
045600000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
045700000000     C* INIZIALIZZO FLAG CONTROLLO PGM
045800000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
045900000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
046000000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
046100000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
046200000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
046300000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
046400000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
046500000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
046600000000     C* LANCIO ROUTINE PER VERIFICARE SE
046700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
046800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
046900020917     C                   EXSR      REPDATIUTE
047000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
047100000000     C                   BITOFF    '01234567'    POSMI1            1
047200000000     C                   BITOFF    '01234567'    POSMI2            1
047300000000     C                   BITOFF    '01234567'    POSMA1            1
047400000000     C                   BITOFF    '01234567'    POSMA2            1
047500000000     C                   BITON     '34'          POSMI1            1            *18
047600000000     C                   BITON     '7'           POSMI2            1            *01
047700000000     C                   BITON     '34'          POSMA1            1            *18
047800000000     C                   BITON     '0'           POSMA2            1            *80
047900000000     C                   MOVE      POSMI2        POSMIN            2
048000000000     C                   MOVEL     POSMI1        POSMIN
048100000000     C                   MOVE      POSMA2        POSMAX            2
048200000000     C                   MOVEL     POSMA1        POSMAX
048300000000     C                   ENDSR
048400000000     C*------------------------------------------------------------------------*
048500000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
048600000000     C*------------------------------------------------------------------------*
048700000000     C     FUNSTD        BEGSR
048800000000     C                   SELECT
048900000000     C     KEY           WHENEQ    F3
049000000000     C                   EXSR      F03STD
049100000000     C     KEY           WHENEQ    F12
049200000000     C                   EXSR      F12STD
049300000000     C     KEY           WHENEQ    F24
049400000000     C                   EXSR      F24STD
049500000000     C                   OTHER
049600000000     C                   MOVEL     MSG(2)        $MSG
049700000000     C                   EXSR      CARMSG
049800000000     C                   ENDSL
049900000000     C                   ENDSR
050000000000     C*------------------------------------------------------------------------*
050100000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
050200000000     C*------------------------------------------------------------------------*
050300000000     C     F03STD        BEGSR
050400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
050500000000     C                   MOVEL     '1'           $F03
050600000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
050700000000     C                   MOVEL     '1'           $FINE
050800000000     C                   ENDSR
050900000000     C*------------------------------------------------------------------------*
051000000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
051100000000     C*------------------------------------------------------------------------*
051200000000     C     F12STD        BEGSR
051300000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
051400000000     C                   MOVEL     '1'           $F12
051500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
051600000000     C                   MOVEL     '1'           $FINE
051700000000     C                   ENDSR
051800000000     C*------------------------------------------------------------------------*
051900000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
052000000000     C*------------------------------------------------------------------------*
052100000000     C     F24STD        BEGSR
052200000000     C*
052300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
052400000000     C                   EXSR      OUTFUN
052500000000     C*
052600000000     C                   ENDSR
052700090527     C*------------------------------------------------------------------------*
052800090527     C* F09GEs - GEST. TASTO FUNZ. F09          ==> skedulazione
052900090527     C*------------------------------------------------------------------------*
053000090527     C     F09GES        BEGSR
053100090527     C*
053200090527     C                   MOVE      '1'           KRITB
053300090527     C     v1OPZ         IFEQ      '1'                                          DDT
053400090527     C                   MOVEL     'SSL1'        KCOAZ
053500090527     C                   ENDIF
053600090527     C     v1OPZ         IFEQ      '2'                                          STAT.SETTIMANALE
053700090527     C                   MOVEL     'SSL3'        KCOAZ
053800090527     C                   ENDIF
053900090527     C     v1OPZ         IFEQ      '3'
054000090527     C                   MOVEL     'SSLR'        KCOAZ
054100090527     C                   ENDIF
054200110125     c                   if        %subst(knsif:7:1)='2'
054300110125     c                   eval      kqieb='K018SDINO'
054400110125     c                   endif
054500090527     c
054600090527     c                   call      'BCH09'
054700090527     c                   parm                    kpjba
054800090527     C*
054900090527     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
055000090527     C*
055100090527     C                   ENDSR
055200000000     C*------------------------------------------------------------------------*
055300000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
055400000000     C*          CODA MESSAGGI DEL PGM
055500000000     C*------------------------------------------------------------------------*
055600000000     C     CARMSG        BEGSR
055700000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
055800000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
055900000000     C     MSGULT        IFEQ      0
056000000000     C                   MOVEL     '1'           *IN40
056100970529     C                   WRITE     SSL0DVK
056200000000     C                   MOVEL     '0'           *IN40
056300000000     C                   ENDIF
056400000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
056500000000     C                   MOVEL     $MSG          DSMSMS
056600000000     C                   CALL      'XMSG'        XMSGPA
056700000000     C     DSMSER        IFEQ      0
056800000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
056900000000     C                   ENDIF
057000000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
057100000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
057200970529     C                   WRITE     SSL0DVM                                      *SFL MESS
057300000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
057400000000     C                   MOVEL     *BLANKS       $MSG
057500000000     C                   ENDSR
057600000000     C*------------------------------------------------------------------------*
057700000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
057800000000     C*------------------------------------------------------------------------*
057900000000     C     OUTMSG        BEGSR
058000000000     C* VISUALIZZAZIONE MESSAGGIO
058100000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
058200970529     C                   WRITE     SSL0DVK                                      *CONTR MESS
058300000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
058400000000     C                   Z-ADD     *ZEROS        MSGULT
058500000000     C                   ENDSR
058600000000     C*------------------------------------------------------------------------*
058700000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
058800000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
058900000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
059000000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
059100000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
059200000000     C*------------------------------------------------------------------------*
059300000000     C     CARFUN        BEGSR
059400000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
059500000000     C     1             DO        31            I                 5 0          --- 1 -->
059600000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
059700000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
059800000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
059900020917     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
060000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
060100000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
060200000000     C                   ELSE                                                   *INTERROG.
060300000000     C                   MOVEL     FUNASI        FAT(I)
060400000000     C                   ENDIF
060500000000     C                   ELSE                                                   <-- 2 --> *FILIALE
060600000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
060700000000     C                   MOVEL     FUNAFG        FAT(I)
060800000000     C                   ELSE                                                   *INTERROG.
060900000000     C                   MOVEL     FUNAFI        FAT(I)
061000000000     C                   ENDIF
061100000000     C                   ENDIF                                                  <-- 2 ---
061200000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
061300000000     C                   MOVEL     USER          WEDP              3            *PROFILO
061400000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
061500000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
061600000000     C                   ELSE
061700000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
061800000000     C                   ENDIF
061900000000     C                   ENDIF
062000000000     C                   ENDDO                                                  <-- 1 ---
062100000000     C                   ENDSR
062200000000     C*------------------------------------------------------------------------*
062300000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
062400000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
062500000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
062600000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
062700000000     C*------------------------------------------------------------------------*
062800000000     C     OUTFUN        BEGSR
062900000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
063000000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
063100000000     C                   ENDDO
063200000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
063300000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
063400000000     C     1             DO        31            I
063500000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
063600000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
063700000000     C                   ADD       1             FUNNAT
063800000000     C                   ENDIF
063900000000     C                   ENDDO
064000000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
064100000000     C     FUNNAT        IFGT      8
064200000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
064300000000     C                   ENDIF
064400000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
064500000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
064600000000     C                   Z-ADD     8             FUNVIS            5 0
064700000000     C     FAT(23)       IFEQ      'S'
064800000000     C                   SUB       1             FUNVIS
064900000000     C                   ENDIF
065000000000     C     FAT(24)       IFEQ      'S'
065100000000     C                   SUB       1             FUNVIS
065200000000     C                   ENDIF
065300000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
065400000000     C                   Z-ADD     *ZEROS        W                 5 0
065500000000     C                   ADD       1             FUNULT
065600000000     C     FUNULT        IFGT      31
065700000000     C                   Z-ADD     1             FUNULT
065800000000     C                   ENDIF
065900000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
066000000000     C     FUNULT        DO        31            I                              --- 1 -->
066100000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
066200000000     C     W             IFLT      FUNVIS                                       --- 2 -->
066300000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
066400000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
066500000000     C     FDE(I)        ANDNE     *BLANKS
066600000000     C     I             IFEQ      23                                           *TOLGO F23
066700000000     C     I             OREQ      24                                           *E F24
066800000000     C                   ELSE
066900000000     C                   ADD       1             W
067000000000     C                   MOVEL     FDE(I)        FUV(W)
067100000000     C                   ENDIF
067200000000     C                   ENDIF                                                  <-- 3 ---
067300000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
067400000000     C                   Z-ADD     I             FUNULT
067500000000     C                   ENDIF                                                  <-- 2 ---
067600000000     C                   ENDDO                                                  <-- 1 ---
067700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
067800000000     C                   MOVEL     FUV(1)        VFD001
067900000000     C                   MOVEL     FUV(2)        VFD002
068000000000     C                   MOVEL     FUV(3)        VFD003
068100000000     C                   MOVEL     FUV(4)        VFD004
068200000000     C                   MOVEL     FUV(5)        VFD005
068300000000     C                   MOVEL     FUV(6)        VFD006
068400000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
068500000000     C     FAT(24)       ANDEQ     'N'
068600000000     C                   MOVEL     FUV(7)        VFD007
068700000000     C                   MOVEL     FUV(8)        VFD008
068800000000     C                   ELSE                                                   <-- 1 -->
068900000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
069000000000     C     FAT(24)       ANDEQ     'S'
069100000000     C                   MOVEL     FDE(23)       VFD007
069200000000     C                   MOVEL     FDE(24)       VFD008
069300000000     C                   ELSE                                                   <-- 2 -->
069400000000     C                   MOVEL     FUV(7)        VFD007
069500000000     C     FAT(23)       IFEQ      'S'
069600000000     C                   MOVEL     FDE(23)       VFD008
069700000000     C                   ELSE
069800000000     C                   MOVEL     FDE(24)       VFD008
069900000000     C                   ENDIF
070000000000     C                   ENDIF                                                  <-- 2 ---
070100000000     C                   ENDIF                                                  <-- 1 ---
070200000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
070300000000     C                   ENDSR
070400000000     C*------------------------------------------------------------------------*
070500000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
070600000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
070700000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
070800000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
070900000000     C*------------------------------------------------------------------------*
071000000000     C     CONFUN        BEGSR
071100000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
071200000000     C     1             DO        31            I                              --- 1 -->
071300000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
071400000000     C* CONTROLLO SE IL TASTO E' ATTIVO
071500000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
071600000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
071700000000     C                   EXSR      CARMSG
071800000000     C                   MOVEL     '1'           $ERR
071900000000     C                   ENDIF                                                  <-- 3 ---
072000000000     C                   ENDIF                                                  <-- 2 ---
072100000000     C                   ENDDO                                                  <-- 1 ---
072200000000     C                   ENDSR
072300000000     O*------------------------------------------------------------------------*
072400000000** MSG - (MESSAGGI)
072500970529TNSSL0R- Tasto funzionale non valido.                                          1
072600970529TNSSL0R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
072700970529TNSSL0R- Opzione non valida.                                                   3
072800970529TNSSL0R- Opzione abilitata ma non gestita. Avvertire CED.                      4
072900970529TNSSL0R- Inizio elenco.                                                        5
073000970529TNSSL0R- Fine elenco.                                                          6
073100970529TNSSL0R- Data errata                                                           7
073200970529TNSSL0R- Opzione Errata                                                        8
073300090601TNSSL0R- Se richiesta solo una data impossibile elaborare tipo "R"             9
073400970529TNSSL0R- Stampa lanciata ....                                                 10
073500090601TNSSL0R- Se richiesto range di date impossibile elaborare tipo "G"            11
073600090601TNSSL0R- Per opzione "3" indicare il tipo di elaborazione                     12
073700090901Per abilitare l'elaborazione dell'opzione "2" --> premere  F17                13
073800000000** CMD  (COMANDI)
073900000000WRKSPLF
074000000000
074100000000
074200000000
074300000000
074400000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
074500000000F1    -                   -    NN     NN
074600000000F2    -                   -    NN     NN
074700000000F3    -F3=Fine            -    SS     SS
074800000000F4    -                   -    NN     NN
074900000000F5    -                   -    NN     NN
075000000000F6    -F6=Conferma        -    SS     SS
075100000000F7    -                   -    NN     NN
075200000000F8    -                   -    NN     NN
075300090527F9    -F9=Gest.Skedulaz.  -    SS     SS
075400000000F10   -                   -    NN     NN
075500000000F11   -                   -    NN     NN
075600000000F12   -F12=Ritorno        -    SS     SS
075700000000F13   -                   -    NN     NN
075800000000F14   -                   -    NN     NN
075900000000F15   -                   -    NN     NN
076000000000F16   -                   -    NN     NN
076100090901F17   -F17=Attiva opz."2" -    SS     SS
076200000000F18   -                   -    NN     NN
076300000000F19   -                   -    NN     NN
076400000000F20   -                   -    NN     NN
076500000000F21   -                   -    NN     NN
076600000000F22   -F22=Gestione stampe-    EE     EE
076700000000F23   -                   -    NN     NN
076800000000F24   -F24=Altri tasti    -    NN     NN
076900000000ENTER -                   -    SS     SS
077000000000ROLLUP-                   -    NN     NN
077100000000ROLLDN-                   -    NN     NN
077200000000HELP  -                   -    NN     NN
077300000000PRINT -                   -    NN     NN
077400000000BSPACE-                   -    NN     NN
077500000000CLEAR -                   -    NN     NN
077600110125**
077700110125CHKOBJ OBJ(K018SDINO) OBJTYPE(*JOBQ)
