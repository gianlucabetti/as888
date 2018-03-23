000100971121     H*------------------------------------------------------------------------*
000200971121     H* AUTOMATISMO TEMPI DI TRAZIONE - RICHIESTA PARAMETRI                    *
000300971121     H*------------------------------------------------------------------------*
000400971121     H DECEDIT('0,') DATEDIT(*DMY.)
000500971121     F*------------------------------------------------------------------------*
000600971121     F* DATA BASE
000700971121     F*------------------------------------------------------------------------*
000800971121     FTISI27D   CF   E             WORKSTN INFDS(DEVDS1)
000900971121     F                                     SFILE(SI27DVM:MSGULT)
001000971121     D*------------------------------------------------------------------------*
001100971121     D* SCHIERE
001200971121     D*------------------------------------------------------------------------*
001300971121     D*-------------------
001400971121     D* MESSAGGI, COMANDI E FUNZIONI
001500971121     D*-------------------
001600971121     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             *MESSAGGI
001700971121     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
001800971121     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001900971121     D*-------------------
002000971121     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002100971121     D*-------------------
002200971121     D FFU             S              6    DIM(31)                              *FUNZIONE
002300971121     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002400971121     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002500971121     D*-------------------
002600971121     D* TABELLA DESCRIZIONI A VIDEO
002700971121     D*-------------------
002800971121     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002900971121     D*-------------------
003000971121     D* TABELLA TASTI FUNZIONALI PREMUTI
003100971121     D*-------------------
003200971121     D KEYS            S              1    DIM(40)
003300971121     D*------------------------------------------------------------------------*
003400971121     D* INPUT
003500971121     D*------------------------------------------------------------------------*
003600971121     D*---------------------
003700971121     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003800971121     D*---------------------
003900971121     D DSFUN         E DS
004000971121     D  FUNRIG                 1     40
004100971121     D*---------------------
004200971121     D* DS PER GESTIONE TASTI FUNZIONALI
004300971121     D*---------------------
004400971121     D DSKEYS        E DS
004500971121     D  KEYRIG                 1     40
004600971121     D*---------------------
004700971121     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004800971121     D*---------------------
004900971121     D                SDS
005000971121     D  USER                 254    263
005100971121     D*---------------------
005200971121     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005300971121     D*---------------------
005400971121     D DEVDS1          DS
005500971121     D  KEY                  369    369
005600971121     D  CURSOR               370    371
005700971121     D*---------------------
005800971121     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
005900971121     D*---------------------
006000971121     D DSMSG         E DS
006100971121     D*---------------------
006200020919     D* DS REPERIMENTO DATI UTENTE
006300971121     D*---------------------
006400020919     D TIBS34DS      E DS                                                       *Profili utente
006500020919     D DDATIUTE      E DS                                                       *Dati utente
006600020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006700051216     D WLBDA8          DS                  INZ
006800051216     D  G08DAT                 1      8  0
006900051216     D  G08INV                 9     16  0
007000051216     D  G08ERR                17     17
007100051216     D  G08TGI                18     22  0
007200971121     D*---------------------
007300971121     D* ARCHITETTURA
007400971121     D*---------------------
007500971121     D KPJBA         E DS
007600051216     d
007700051216     d pardde          s              8s 0
007800971121     C*------------------------------------------------------------------------*
007900971121     C* MAIN LINE
008000971121     C*------------------------------------------------------------------------*
008100971121     C*
008200971121     C* CICLO FINO A CHE:
008300971121DO  1C     $FINE         DOUEQ     '1'
008400971121     C*
008500971121     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
008600971121     C*
008700971121     C* CONTROLLA GESTIONE VIDEO
008800971121IF  2C     $GES          IFEQ      'V1'
008900971121     C                   EXSR      GESV1                                        *PARAMETRI
009000971121E   2C                   ENDIF
009100971121E   1C                   ENDDO
009200971121     C*
009300971121     C                   SETON                                        LR
009400971121     C*------------------------------------------------------------------------*
009500971121     C* GESV1 - GESTIONE VIDEO UNO
009600971121     C*------------------------------------------------------------------------*
009700971121     C     GESV1         BEGSR
009800971121     C*
009900971121IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
010000971121     C                   EXSR      CARV1                                        *CARICA VIDEO
010100971121E   1C                   ENDIF
010200971121     C*
010300971121IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
010400971121     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
010500971121     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
010600971121E   1C                   ENDIF
010700971121     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
010800971121     C*
010900971121     C                   EXFMT     SI27DV1                                      *PARAMETRI
011000971121     C*
011100971121     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
011200971121     C*
011300971121IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
011400971121SL  2C                   SELECT
011500971121     C     KEY           WHENEQ    ENTER
011600971121     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
011700971121     C     KEY           WHENEQ    F6
011800971121     C                   EXSR      F06GES                                       *CONFERMA
011900971121     C     KEY           WHENEQ    F22
012000971121     C                   EXSR      F22GES                                       *GESTIONE STAMPE
012100971121     C                   OTHER
012200971121     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
012300971121E   2C                   ENDSL
012400971121E   1C                   ENDIF
012500971121     C*
012600971121     C                   ENDSR
012700971121     C*------------------------------------------------------------------------*
012800971121     C* CARV1 - CARICA VIDEO UNO
012900971121     C*------------------------------------------------------------------------*
013000971121     C     CARV1         BEGSR
013100971121     C*
013200971121     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013300971121     C*
013400971121     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
013500971121     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
013600971121     C*
013700971121     C                   WRITE     SI27DVT                                      *TESTATA
013800971121     C                   WRITE     SI27DVF                                      *FUNZIONI
013900971121     C*
014000971121     C                   ENDSR
014100971121     C*------------------------------------------------------------------------*
014200971121     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
014300971121     C*------------------------------------------------------------------------*
014400971121     C     ENTV1         BEGSR
014500971121     C*
014600971121     C* CONTROLLA I CAMPI IMMESSI
014700971121     C                   EXSR      DECODI
014800971121     C*
014900971121     C                   ENDSR
015000971121     C*------------------------------------------------------------------------*
015100971121     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
015200971121     C*------------------------------------------------------------------------*
015300971121     C     F06GES        BEGSR
015400971121     C*
015500971121     C* CONTROLLA I CAMPI IMMESSI
015600971121     C                   EXSR      DECODI
015700971121     C*
015800971121     C* LANCIA IL PROGRAMMA
015900971121IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
016000971121     C                   MOVEL     V1CTEM        PARTEM                         *TIPO TEMPO
016100971121     C                   MOVEL     V1CSIM        PARSIM                         *SIMULAZIONE
016200051216     C                   z-add     ddevid        PARdde                         *SIMULAZIONE
016300971121     C                   CALL      'TISI28R'
016400971121     C                   PARM                    PARTEM            1
016500971121     C                   PARM                    PARSIM            1
016600051216     C                   PARM                    PARdde
016700971121     C                   MOVEL     MSG(10)       $MSG
016800971121     C                   EXSR      CARMSG
016900971121E   1C                   ENDIF
017000971121     C*
017100971121     C                   ENDSR
017200971121     C*------------------------------------------------------------------------*
017300971121     C* F22GES - GESTIONE TASTO FUNZIONALE F22 ==> GESTIONE STAMPE
017400971121     C*------------------------------------------------------------------------*
017500971121     C     F22GES        BEGSR
017600971121     C*
017700971121     C                   Z-ADD     80            LENGH            15 5
017800971121     C                   CALL      'QCAEXEC'
017900971121     C                   PARM                    CMD(1)
018000971121     C                   PARM                    LENGH
018100971121     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
018200971121     C*
018300971121     C                   ENDSR
018400971121     C*------------------------------------------------------------------------*
018500971121     C* DECODI - CONTROLLA CAMPI E DECODIFICA
018600971121     C*------------------------------------------------------------------------*
018700971121     C     DECODI        BEGSR
018800971121     C*
018900971121     C* REIMPOSTA INDICATORI DI ERRORE
019000051216     C                   SETOFF                                       606162
019100971121     C*--------------------
019200971121     C* TIPO TEMPO
019300971121     C*--------------------
019400971121IF  1C     $ERR          IFEQ      '0'
019500971121IF  2C     V1CTEM        IFEQ      'P'
019600971121     C     V1CTEM        OREQ      'T'
019700971121X   2C                   ELSE
019800971121     C                   SETON                                        60
019900971121     C                   MOVEL     '1'           $ERR
020000971121     C                   MOVEL     MSG(7)        $MSG
020100971121     C                   EXSR      CARMSG
020200971121E   2C                   ENDIF
020300971121E   1C                   ENDIF
020400971121     C*--------------------
020500971121     C* AGGIORNAMENTO TEMPI
020600971121     C*--------------------
020700971121IF  1C     $ERR          IFEQ      '0'
020800971121IF  2C     V1CSIM        IFEQ      'S'
020900971121     C     V1CSIM        OREQ      'N'
021000971121X   2C                   ELSE
021100971121     C                   SETON                                        61
021200971121     C                   MOVEL     '1'           $ERR
021300971121     C                   MOVEL     MSG(7)        $MSG
021400971121     C                   EXSR      CARMSG
021500971121E   2C                   ENDIF
021600971121E   1C                   ENDIF
021700051216     C*--------------------
021800051216     C* DATA DECORRENZA
021900051216     C*--------------------
022000051216     c     *like         define    v1cdde        ddevid
022100051216IF  1C     $ERR          IFEQ      '0'
022200051216IF  2C     V1Cdde        IFEQ      *zeros
022300051216     C                   SETON                                        62
022400051216     C                   MOVEL     '1'           $ERR
022500051216     C                   MOVEL     MSG(8)        $MSG
022600051216     C                   EXSR      CARMSG
022700051216X   2C                   ELSE
022800051216     C                   z-add     v1cdde        wda8
022900051216     C                   z-add     *zeros        da8w
023000051216     C                   exsr      chkda8
023100051216     C                   z-add     wda8          v1cdde
023200051216     C                   z-add     da8w          ddevid
023300051216IF  2C     g08err        IFEQ      '1'
023400051216     C                   SETON                                        62
023500051216     C                   MOVEL     '1'           $ERR
023600051216     C                   MOVEL     MSG(8)        $MSG
023700051216     C                   EXSR      CARMSG
023800051216E   2C                   ENDIF
023900051216E   1C                   ENDIF
024000051216E   1C                   ENDIF
024100971121     C*
024200971121     C                   ENDSR
024300971121     C*------------------------------------------------------------------------*
024400971121     C* IMPVID - IMPOSTAZIONI VIDEO
024500971121     C*------------------------------------------------------------------------*
024600971121     C     IMPVID        BEGSR
024700971121     C*
024800971121     C                   MOVEL     'T'           V1CTEM                         *TIPO TEMPO
024900971121     C                   MOVEL     'S'           V1CSIM                         *SIMULAZIONE
025000051216     c                   time                    w0140            14 0
025100051216     c                   move      w0140         v1cdde
025200971121     C*
025300971121     C                   ENDSR
025400971121     C*------------------------------------------------------------------------*
025500971121     C* CARTAB - CARICA LE TABELLE
025600971121     C*------------------------------------------------------------------------*
025700971121     C     CARTAB        BEGSR
025800971121     C                   ENDSR
025900020919     C*--------------------------------------------------------------------------------------------*
026000020919     C* REPERISCE I DATI UTENTE
026100020919     C*--------------------------------------------------------------------------------------------*
026200020919     C     REPDATIUTE    BEGSR
026300020919     C*
026400020919     C* INIZIALIZZA VARIABILI DI WRK
026500020919     C                   CLEAR                   TIBS34DS
026600020919     C                   CLEAR                   AZUTEDS
026700020919     C                   CLEAR                   DDATIUTE
026800020919     C*
026900020919     C     *DTAARA       DEFINE    §azute        azuteds
027000020919     C     *DTAARA       DEFINE    §datiute      ddatiute
027100020919     C                   IN(E)     *DTAARA
027200020919if  1C                   IF        %Error
027300020919     c                   EVAL      I34Tla = 'L'
027400020919     C                   CALL      'TIBS34R'
027500020919     C                   PARM                    Tibs34Ds
027600020919     C                   IN        *DTAARA
027700020919e   1C                   ENDIF
027800020919     C*
027900020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
028000020919     C                   MOVEL(P)  RSUT          DSFIRS           20
028100020919     C*
028200020919     C                   ENDSR
028300051216     C*------------------------------------------------------------------------*
028400051216     C* CHKDA8 - CONTROLLA LA DATA (8)
028500051216     C*------------------------------------------------------------------------*
028600051216     C     chkda8        begsr
028700051216     C*
028800051216     C     *like         define    v1cdde        wda8
028900051216     C     *LIKE         define    v1cdde        da8w
029000051216     C*
029100051216     C                   z-add     wda8          G08DAT
029200051216     C                   z-add     *zeros        G08INV
029300051216     C                   movel     '0'           G08ERR
029400051216     C                   call      'XSRDA8'
029500051216     C                   parm                    WLBDA8
029600051216     C                   z-add     g08dat        WDA8
029700051216     C                   z-add     g08inv        DA8W
029800051216     C*
029900051216     C                   endsr
030000971121     C*------------------------------------------------------------------------*
030100971121     C* *INZSR - ROUTINE INIZIALE
030200971121     C*------------------------------------------------------------------------*
030300971121     C     *INZSR        BEGSR
030400971121     C*--------------------
030500971121     C* RICEVIMENTO PARAMETRI
030600971121     C*--------------------
030700971121     C     *ENTRY        PLIST
030800971121     C                   PARM                    KPJBA
030900971121     C*--------------------
031000971121     C* VARIABILI DI CONTROLLO
031100971121     C*--------------------
031200971121     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
031300971121     C***
031400971121     C* IMPOSTAZIONI VIDEO
031500971121     C***
031600971121     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
031700971121     C*
031800971121     C* ESEGUE OPERAZIONI INIZIALI STANDARD
031900971121     C                   EXSR      INZSTD
032000971121     C*
032100971121     C* CARICA LE TABELLE
032200971121     C                   EXSR      CARTAB
032300971121     C*
032400971121     C* IMPOSTAZIONI VIDEO
032500971121     C                   EXSR      IMPVID
032600971121     C*
032700971121     C* EMETTE VIDEATA
032800971121     C                   EXSR      CARV1
032900971121     C*
033000971121     C                   ENDSR
033100971121     C*------------------------------------------------------------------------*
033200971121     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
033300971121     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
033400971121     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
033500971121     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
033600971121     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
033700971121     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
033800971121     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
033900971121     C*------------------------------------------------------------------------*
034000971121     C* INZSTD - ROUTINE INIZIALE STANDARD
034100971121     C*------------------------------------------------------------------------*
034200971121     C     INZSTD        BEGSR
034300971121     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
034400971121     C                   CALL      'XKEYS'
034500971121     C                   PARM                    DSKEYS
034600971121     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
034700971121     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
034800971121     C     XMSGPA        PLIST
034900971121     C                   PARM                    DSMSMS
035000971121     C                   PARM                    DSMSPG
035100971121     C                   PARM                    DSMSKE
035200971121     C                   PARM                    DSMSER
035300971121     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
035400971121     C                   MOVEL     'TISI27R'     DSMSPG                         *NOME PGM CHIAMANTE
035500971121     C                   MOVEL     'TISI27R'     VMCQUE                         *CODA MESSAGGI DESTI
035600971121     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
035700971121     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
035800971121     C* INIZIALIZZO FLAG CONTROLLO PGM
035900971121     C                   MOVEL     '0'           $ERR              1            *ERRORE
036000971121     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
036100971121     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
036200971121     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
036300971121     C                   MOVEL     '0'           $FINE             1            *FINE PGM
036400971121     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
036500971121     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
036600971121     C* LANCIO ROUTINE PER VERIFICARE SE
036700971121     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
036800971121     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
036900020919     C                   EXSR      REPDATIUTE
037000971121     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
037100971121     C                   BITOFF    '01234567'    POSMI1            1
037200971121     C                   BITOFF    '01234567'    POSMI2            1
037300971121     C                   BITOFF    '01234567'    POSMA1            1
037400971121     C                   BITOFF    '01234567'    POSMA2            1
037500971121     C                   BITON     '34'          POSMI1            1            *18
037600971121     C                   BITON     '7'           POSMI2            1            *01
037700971121     C                   BITON     '34'          POSMA1            1            *18
037800971121     C                   BITON     '0'           POSMA2            1            *80
037900971121     C                   MOVE      POSMI2        POSMIN            2
038000971121     C                   MOVEL     POSMI1        POSMIN
038100971121     C                   MOVE      POSMA2        POSMAX            2
038200971121     C                   MOVEL     POSMA1        POSMAX
038300971121     C                   ENDSR
038400971121     C*------------------------------------------------------------------------*
038500971121     C* FUNSTD - LANCIA SUBROUTINE STANDARD
038600971121     C*------------------------------------------------------------------------*
038700971121     C     FUNSTD        BEGSR
038800971121     C                   SELECT
038900971121     C     KEY           WHENEQ    F3
039000971121     C                   EXSR      F03STD
039100971121     C     KEY           WHENEQ    F12
039200971121     C                   EXSR      F12STD
039300971121     C     KEY           WHENEQ    F24
039400971121     C                   EXSR      F24STD
039500971121     C                   OTHER
039600971121     C                   MOVEL     MSG(2)        $MSG
039700971121     C                   EXSR      CARMSG
039800971121     C                   ENDSL
039900971121     C                   ENDSR
040000971121     C*------------------------------------------------------------------------*
040100971121     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
040200971121     C*------------------------------------------------------------------------*
040300971121     C     F03STD        BEGSR
040400971121     C* IMPOSTO FLAG DI PREMUTO TASTO F03
040500971121     C                   MOVEL     '1'           $F03
040600971121     C* IMPOSTO FLAG DI FINE PROGRAMMA
040700971121     C                   MOVEL     '1'           $FINE
040800971121     C* PULISCE IL MESSAGGIO
040900971121     C                   ENDSR
041000971121     C*------------------------------------------------------------------------*
041100971121     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
041200971121     C*------------------------------------------------------------------------*
041300971121     C     F12STD        BEGSR
041400971121     C* IMPOSTO FLAG DI FINE PROGRAMMA
041500971121     C                   MOVEL     '1'           $FINE
041600971121     C                   ENDSR
041700971121     C*------------------------------------------------------------------------*
041800971121     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
041900971121     C*------------------------------------------------------------------------*
042000971121     C     F24STD        BEGSR
042100971121     C*
042200971121     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
042300971121     C                   EXSR      OUTFUN
042400971121     C*
042500971121     C                   ENDSR
042600971121     C*------------------------------------------------------------------------*
042700971121     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
042800971121     C*          CODA MESSAGGI DEL PGM
042900971121     C*------------------------------------------------------------------------*
043000971121     C     CARMSG        BEGSR
043100971121     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
043200971121     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
043300971121     C     MSGULT        IFEQ      0
043400971121     C                   MOVEL     '1'           *IN40
043500971121     C                   WRITE     SI27DVK
043600971121     C                   MOVEL     '0'           *IN40
043700971121     C                   ENDIF
043800971121     C* INVIA MESSAGGIO ALLA CODA DEL PGM
043900971121     C                   MOVEL     $MSG          DSMSMS
044000971121     C                   CALL      'XMSG'        XMSGPA
044100971121     C     DSMSER        IFEQ      0
044200971121     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
044300971121     C                   ENDIF
044400971121     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
044500971121     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
044600971121     C                   WRITE     SI27DVM                                      *SFL MESS
044700971121     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
044800971121     C                   MOVEL     *BLANKS       $MSG
044900971121     C                   ENDSR
045000971121     C*------------------------------------------------------------------------*
045100971121     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
045200971121     C*------------------------------------------------------------------------*
045300971121     C     OUTMSG        BEGSR
045400971121     C* VISUALIZZAZIONE MESSAGGIO
045500971121     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
045600971121     C                   WRITE     SI27DVK                                      *CONTR MESS
045700971121     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
045800971121     C                   Z-ADD     *ZEROS        MSGULT
045900971121     C                   ENDSR
046000971121     C*------------------------------------------------------------------------*
046100971121     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
046200971121     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
046300971121     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
046400971121     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
046500971121     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
046600971121     C*------------------------------------------------------------------------*
046700971121     C     CARFUN        BEGSR
046800971121     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
046900971121     C     1             DO        31            I                 5 0          --- 1 -->
047000971121     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
047100971121     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
047200971121     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
047300020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
047400971121     C     $TGES         IFEQ      'G'                                          *GESTIONE
047500971121     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
047600971121     C                   ELSE                                                   *INTERROG.
047700971121     C                   MOVEL     FUNASI        FAT(I)
047800971121     C                   ENDIF
047900971121     C                   ELSE                                                   <-- 2 --> *FILIALE
048000971121     C     $TGES         IFEQ      'G'                                          *GESTIONE
048100971121     C                   MOVEL     FUNAFG        FAT(I)
048200971121     C                   ELSE                                                   *INTERROG.
048300971121     C                   MOVEL     FUNAFI        FAT(I)
048400971121     C                   ENDIF
048500971121     C                   ENDIF                                                  <-- 2 ---
048600971121     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
048700971121     C                   MOVEL     USER          WEDP              3            *PROFILO
048800971121     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
048900971121     C                   MOVEL     'S'           FAT(I)                         *ABILITO
049000971121     C                   ELSE
049100971121     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
049200971121     C                   ENDIF
049300971121     C                   ENDIF
049400971121     C                   ENDDO                                                  <-- 1 ---
049500971121     C                   ENDSR
049600971121     C*------------------------------------------------------------------------*
049700971121     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
049800971121     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
049900971121     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
050000971121     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
050100971121     C*------------------------------------------------------------------------*
050200971121     C     OUTFUN        BEGSR
050300971121     C     1             DO        8             I                              *PULIZIA FUNZIONI A
050400971121     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
050500971121     C                   ENDDO
050600971121     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
050700971121     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
050800971121     C     1             DO        31            I
050900971121     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
051000971121     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
051100971121     C                   ADD       1             FUNNAT
051200971121     C                   ENDIF
051300971121     C                   ENDDO
051400971121     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
051500971121     C     FUNNAT        IFGT      8
051600971121     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
051700971121     C                   ENDIF
051800971121     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
051900971121     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
052000971121     C                   Z-ADD     8             FUNVIS            5 0
052100971121     C     FAT(23)       IFEQ      'S'
052200971121     C                   SUB       1             FUNVIS
052300971121     C                   ENDIF
052400971121     C     FAT(24)       IFEQ      'S'
052500971121     C                   SUB       1             FUNVIS
052600971121     C                   ENDIF
052700971121     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
052800971121     C                   Z-ADD     *ZEROS        W                 5 0
052900971121     C                   ADD       1             FUNULT
053000971121     C     FUNULT        IFGT      31
053100971121     C                   Z-ADD     1             FUNULT
053200971121     C                   ENDIF
053300971121     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
053400971121     C     FUNULT        DO        31            I                              --- 1 -->
053500971121     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
053600971121     C     W             IFLT      FUNVIS                                       --- 2 -->
053700971121     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
053800971121     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
053900971121     C     FDE(I)        ANDNE     *BLANKS
054000971121     C     I             IFEQ      23                                           *TOLGO F23
054100971121     C     I             OREQ      24                                           *E F24
054200971121     C                   ELSE
054300971121     C                   ADD       1             W
054400971121     C                   MOVEL     FDE(I)        FUV(W)
054500971121     C                   ENDIF
054600971121     C                   ENDIF                                                  <-- 3 ---
054700971121     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
054800971121     C                   Z-ADD     I             FUNULT
054900971121     C                   ENDIF                                                  <-- 2 ---
055000971121     C                   ENDDO                                                  <-- 1 ---
055100971121     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
055200971121     C                   MOVEL     FUV(1)        VFD001
055300971121     C                   MOVEL     FUV(2)        VFD002
055400971121     C                   MOVEL     FUV(3)        VFD003
055500971121     C                   MOVEL     FUV(4)        VFD004
055600971121     C                   MOVEL     FUV(5)        VFD005
055700971121     C                   MOVEL     FUV(6)        VFD006
055800971121     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
055900971121     C     FAT(24)       ANDEQ     'N'
056000971121     C                   MOVEL     FUV(7)        VFD007
056100971121     C                   MOVEL     FUV(8)        VFD008
056200971121     C                   ELSE                                                   <-- 1 -->
056300971121     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
056400971121     C     FAT(24)       ANDEQ     'S'
056500971121     C                   MOVEL     FDE(23)       VFD007
056600971121     C                   MOVEL     FDE(24)       VFD008
056700971121     C                   ELSE                                                   <-- 2 -->
056800971121     C                   MOVEL     FUV(7)        VFD007
056900971121     C     FAT(23)       IFEQ      'S'
057000971121     C                   MOVEL     FDE(23)       VFD008
057100971121     C                   ELSE
057200971121     C                   MOVEL     FDE(24)       VFD008
057300971121     C                   ENDIF
057400971121     C                   ENDIF                                                  <-- 2 ---
057500971121     C                   ENDIF                                                  <-- 1 ---
057600971121     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
057700971121     C                   ENDSR
057800971121     C*------------------------------------------------------------------------*
057900971121     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
058000971121     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
058100971121     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
058200971121     C*                   - ERRORE SE FUNZIONE NON ATTIVA
058300971121     C*------------------------------------------------------------------------*
058400971121     C     CONFUN        BEGSR
058500971121     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
058600971121     C     1             DO        31            I                              --- 1 -->
058700971121     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
058800971121     C* CONTROLLO SE IL TASTO E' ATTIVO
058900971121     C     FAT(I)        IFNE      'S'                                          --- 3 -->
059000971121     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
059100971121     C                   EXSR      CARMSG
059200971121     C                   MOVEL     '1'           $ERR
059300971121     C                   ENDIF                                                  <-- 3 ---
059400971121     C                   ENDIF                                                  <-- 2 ---
059500971121     C                   ENDDO                                                  <-- 1 ---
059600971121     C                   ENDSR
059700971121     O*------------------------------------------------------------------------*
059800971121** MSG - (MESSAGGI)
059900971121TISI27R- Tasto funzionale non valido.                                          1
060000971121TISI27R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
060100971121TISI27R- Opzione non valida.                                                   3
060200971121TISI27R- Opzione abilitata ma non gestita. Avvertire CED.                      4
060300971121TISI27R- Inizio elenco.                                                        5
060400971121TISI27R- Fine elenco.                                                          6
060500971121TISI27R- Immettere uno dei valori possibili                                    7
060600051216TISI27R- Data mancante o errata
060700971121TISI27R- 9
060800971121TISI27R- Automatismo applicato ...                                             10
060900971121** CMD  (COMANDI)
061000971121WRKSPLF
0611009711212
0612009711213
0613009711214
0614009711215
061500971121** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
061600971121F1    -                   -    NN     NN
061700971121F2    -                   -    NN     NN
061800971121F3    -F3=Fine            -    SS     SS
061900971121F4    -                   -    NN     NN
062000971121F5    -                   -    NN     NN
062100971121F6    -F6=Conferma        -    SS     SS
062200971121F7    -                   -    NN     NN
062300971121F8    -                   -    NN     NN
062400971121F9    -                   -    NN     NN
062500971121F10   -                   -    NN     NN
062600971121F11   -                   -    NN     NN
062700971121F12   -F12=Ritorno        -    SS     SS
062800971121F13   -                   -    NN     NN
062900971121F14   -                   -    NN     NN
063000971121F15   -                   -    NN     NN
063100971121F16   -                   -    NN     NN
063200971121F17   -                   -    NN     NN
063300971121F18   -                   -    NN     NN
063400971121F19   -                   -    NN     NN
063500971121F20   -                   -    NN     NN
063600971121F21   -                   -    NN     NN
063700971121F22   -F22=Gestione stampe-    EE     EE
063800971121F23   -                   -    NN     NN
063900971121F24   -F24=Altri tasti    -    NN     NN
064000971121ENTER -                   -    SS     SS
064100971121ROLLUP-                   -    NN     NN
064200971121ROLLDN-                   -    NN     NN
064300971121HELP  -                   -    NN     NN
064400971121PRINT -                   -    NN     NN
064500971121BSPACE-                   -    NN     NN
064600971121CLEAR -                   -    NN     NN
