000100970603     H*--------------------------------------------------------------------------------------------*
000200000201     H* STAMPA ELENCO CLIENTI ABILITATI/UNIFICANTI
000300970603     H*--------------------------------------------------------------------------------------------*
000400970603     H DECEDIT('0,') DATEDIT(*DMY.)
000500970603     F*--------------------------------------------------------------------------------------------*
000600970603     F* DATA BASE
000700970603     F*--------------------------------------------------------------------------------------------*
000800000201     FTIS726D   CF   E             WORKSTN INFDS(DEVDS1)
000900000201     F                                     SFILE(S726DVM:MSGULT)
001000090803     Ftivss03L  IF   E           K DISK
001100090803     Ftikun01L  IF   E           K DISK
001200090803     Ftikun03L  IF   E           K DISK
001300000202     F                                     RENAME(tikun000:tikunxxx)
001400000201     FTIS726P   O    E             PRINTER OFLIND(*IN05)
001500970603     D*--------------------------------------------------------------------------------------------*
001600970603     D* SCHIERE
001700970603     D*--------------------------------------------------------------------------------------------*
001800970603     D*-------------------
001900970603     D* MESSAGGI E FUNZIONI
002000970603     D*-------------------
002100970603     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002200970603     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002300000202     D DES             S             78    DIM(5) CTDATA PERRCD(1)              *MESSAGGI
002400970603     D*-------------------
002500970603     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002600970603     D*-------------------
002700970603     D FFU             S              6    DIM(31)                              *FUNZIONE
002800970603     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002900970603     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003000970603     D*-------------------
003100970603     D* TABELLA DESCRIZIONI A VIDEO
003200970603     D*-------------------
003300970603     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003400970603     D*-------------------
003500970603     D* TABELLA TASTI FUNZIONALI PREMUTI
003600970603     D*-------------------
003700970603     D KEYS            S              1    DIM(40)
003800970603     D*--------------------------------------------------------------------------------------------*
003900970603     D* INPUT
004000970603     D*--------------------------------------------------------------------------------------------*
004100970603     D*-------------------
004200970603     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004300970603     D*-------------------
004400970603     D DSFUN         E DS
004500970603     D  FUNRIG                 1     40
004600970603     D*-------------------
004700970603     D* DS PER GESTIONE TASTI FUNZIONALI
004800970603     D*-------------------
004900970603     D DSKEYS        E DS
005000970603     D  KEYRIG                 1     40
005100970603     D*-------------------
005200970603     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005300970603     D*-------------------
005400970603     D                SDS
005500970603     D  USER                 254    263
005600970603     D*-------------------
005700970603     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005800970603     D*-------------------
005900970603     D DEVDS1          DS
006000970603     D  KEY                  369    369
006100970603     D  CURSOR               370    371
006200970603     D*---------------------
006300970603     D* ARCHITETTURA
006400970603     D*---------------------
006500970603     D KPJBA         E DS
006600970603     D*---------------------
006700000201     D* DS ESTERNA X REPERIMENTO DATI ANAGRAFICI CLIENTI
006800970603     D*---------------------
006900020916     DBS69DS         e ds                  extname(TIBS69DS)
007000020916     DACODS          e ds                  extname(cnaco00f)
007100020916     DINDDS          e ds                  extname(cnind00f)
007200020916     DCLPDS          e ds                  extname(cnclp00f)
007300020916     DCLSDS          e ds                  extname(fncls00f)
007400000201     D TIBS02DS      E DS
007500970603     D*-------------------
007600020916     D* DS REPERIMENTO DATI UTENTE
007700970603     D*-------------------
007800020916     D TIBS34DS      E DS                                                       *Profili utente
007900020916     D DDATIUTE      E DS                                                       *Dati utente
008000020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008100970603     D*-------------------
008200970603     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
008300970603     D*-------------------
008400970603     D DSMSG         E DS
008500970603     D*------------------
008600970603     D* DS "XSRDA8" - CONTROLLA DATA (8)
008700970603     D*------------------
008800970603     D WLBDA8          DS
008900970603     D  G08DAT                 1      8  0
009000970603     D  G08INV                 9     16  0
009100970603     D  G08ERR                17     17
009200970603     D  G08TGI                18     22  0
009300970603     C*--------------------------------------------------------------------------------------------*
009400970603     C* MAIN LINE
009500970603     C*--------------------------------------------------------------------------------------------*
009600970603     C*
009700970611     C* SE NON CI SONO ERRORI IN ENTRATA
009800970611IF  1C     $FINE         IFEQ      '0'
009900000201     C                   WRITE     S726DVT                                      *TESTATA
010000970611DO  2C     $FINE         DOUEQ     '1'
010100970603     C*
010200970603     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010300970611IF  3C     $GES          IFEQ      'V1'
010400970603     C                   EXSR      GESV1
010500970611E   3C                   ENDIF
010600970603     C*
010700970611E   2C                   ENDDO
010800970603     C*
010900970611E   1C                   ENDIF
011000970603     C* FINE PGM
011100970603     C                   SETON                                        LR
011200970603     C*--------------------------------------------------------------------------------------------*
011300970603     C* GESV1 - GESTIONE VIDEO UNO
011400970603     C*--------------------------------------------------------------------------------------------*
011500970603     C     GESV1         BEGSR
011600970603     C*
011700970603IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
011800970603     C                   EXSR      CARV1                                        *CARICA VIDEO
011900970603E   1C                   ENDIF
012000970603     C*
012100970603IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012200970603     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012300970603     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012400970603E   1C                   ENDIF
012500970603     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012600970603     C*
012700000201     C                   EXFMT     S726DV1                                      *PARAMETRI
012800970603     C*
012900970603     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013000970603     C*
013100970603IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013200970603SL  2C                   SELECT
013300970603     C     KEY           WHENEQ    ENTER
013400970603     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013500970603     C     KEY           WHENEQ    F6
013600970603     C                   EXSR      F06GES
013700970603     C                   OTHER
013800970603     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013900970603E   2C                   ENDSL
014000970603E   1C                   ENDIF
014100970603     C*
014200970603     C                   ENDSR
014300970603     C*--------------------------------------------------------------------------------------------*
014400970603     C* CARV1 - CARICA VIDEO UNO
014500970603     C*--------------------------------------------------------------------------------------------*
014600970603     C     CARV1         BEGSR
014700970603     C*
014800970603     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014900970603     C*
015000970603     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015100970603     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015200970603     C*
015300970603     C                   ENDSR
015400970603     C*--------------------------------------------------------------------------------------------*
015500970603     C* ENTV1 - GESTIONE TASTO ENTER
015600970603     C*--------------------------------------------------------------------------------------------*
015700970603     C     ENTV1         BEGSR
015800970603     C*
015900970603     C* CONTROLLA I CAMPI IMMESSI
016000970603     C                   EXSR      DECODI
016100970603     C*
016200970603     C                   ENDSR
016300970603     C*--------------------------------------------------------------------------------------------*
016400970603     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
016500970603     C*--------------------------------------------------------------------------------------------*
016600970603     C     F06GES        BEGSR
016700970603     C*
016800970603     C* CONTROLLA I CAMPI IMMESSI
016900970603     C                   EXSR      DECODI
017000970603     C*
017100000202     C* ESEGUE ROUTINE PRINCIPALE
017200000201     C                   EXSR      ESEGUI
017300970603     C*
017400000202     C* ESCE DOPO AVER ESGUITO LA STAMPA
017500000202     C                   EXSR      F12STD
017600000202     C*
017700970603     C                   ENDSR
017800000201     C*--------------------------------------------------------------------------------------------*
017900000202     C* ESEGUI - OPERAZIONI PRINCIPALI
018000000201     C*--------------------------------------------------------------------------------------------*
018100000201     C     ESEGUI        BEGSR
018200000202     C*
018300000203     C*  GIRO LA DATA PRESA A VIDEO PER POTER ESEGUIRE I CINFRONTI A FILE (tivss)
018400000202     C                   Z-ADD     VIDDAT        G08DAT
018500000202     C                   Z-ADD     *ZEROS        G08INV
018600000202     C                   MOVEL     '0'           G08ERR
018700000202     C                   CALL      'XSRDA8'
018800000202     C                   PARM                    WLBDA8
018900000202     C                   Z-ADD     G08INV        INVDAT            8 0
019000000202     C     G08ERR        IFEQ      '1'
019100000202     C                   MOVEL     '1'           $ERR
019200000202     C                   ENDIF
019300000202     C*
019400000203     C*  VALORIZZO I CAMPI DI TESTATA DI STAMPA
019500000202     C                   Z-ADD     VIDDAT        STPDAT
019600000202     C                   MOVEL     VIDKUN        STPSON
019700000202     C     VIDISV        IFEQ      *blanks
019800000202     C                   MOVEL     '*all'        STPSRV
019900000202     C                   MOVEL     *blanks       STPSRVD
020000000202     C                   ELSE
020100000202     C                   MOVEL     VIDISV        STPSRV
020200000202     C                   MOVEL     DESSRV        STPSRVD
020300000202     C                   ENDIF
020400000202     C*
020500000203     C*  SCRIVO LA TESTATA
020600000202     C                   WRITE     S726TE
020700000202     C*
020800000203     C*  INIZIO A SCORRERE INTEGRALMENTE IL FILE DEI CLIENTI ABILITATI (tivss)
020900000201     C     *loval        SETLL     TIVSS03L
021000000201     C                   READ      TIVSS03L                               70
021100000201     C                   MOVEL     VSSKSU        SAVKSU            8
021200000201     C                   MOVEL     VSSTLE        SAVTLE            2
021300000203     C*
021400000203     C*  CICLO FINO A FINE FILE
021500000201     C     *IN70         DOWEQ     *OFF
021600000203     C*
021700000203     C*  ROTTURA DI CODICE PER CLIENTE+LEGAME SALVO FINE FILE
021800000201     C     *IN70         DOWEQ     *OFF
021900000201     C     SAVKSU        ANDEQ     VSSKSU
022000000202     C     SAVTLE        ANDEQ     VSSTLE
022100000203     C*  VERIFICO VALIDITA' DEL RECORD IN ESAME RISPETTO ALLE RESTRIZIONI A VIDEO
022200000203     C*  - (ora la data)
022300000202     C     VSSDDE        IFLE      INVDAT
022400000202     C     VSSDSC        ANDGE     INVDAT
022500000203     C*  SE DATA OK FORZO INIZIALIZZAZIONE *IN75 E PASSO AI CONTROLLI SUCCESSIVI
022600000203     C                   SETOFF                                       75
022700000203     C*  - (ora il servizio)
022800000202     C     VIDISV        IFNE      *blanks
022900000202     C     VSSISV        ANDNE     VIDISV
023000000203     C*  SE DATA OK MA SERVIZIO NO ACCENDO INDICATORI DI SERVIZIO
023100000203     C*  => *IN75 X SUCCESSIVA ESCLUSIONE DEI POSSIBILI FIGLI RELATIVI
023200000203     C*  => *IN76 X STAMPA O MENO RIGA DI SEPARAZIONE
023300000202     C                   SETON                                        7576
023400000202     C                   ELSE
023500000203     C*  SE DATA E SERVIZIO OK FORZO INIZIALIZZAZIONE *IN75 E *IN76
023600000203     C*  E PASSO ALL'ESECUZIONE DEI VERI E PROPRI PROCESSI DI STAMPA
023700000202     C                   SETOFF                                       7576
023800000203     C*  "SBIANCO" IL RECORD E RIEMPIO IL BUFFER DI STAMPA X "VSS"
023900000202     C                   CLEAR                   S726VSS
024000000201     C                   MOVEL     VSSISV        STAISV
024100000201     C                   MOVEL     VSSSUN        STASUN
024200000201     C                   MOVEL     VSSTLE        STATLE
024300000202     C                   MOVEL     VSSKSU        STAKSU
024400000202     C                   MOVE      VSSKSU        POGCOD
024500000203     C*  REPERISCO I DATI ANAGRAFICI DEL CLIENTE ABILITATO
024600000201     C                   EXSR      dectib
024700000203     C*  INVERTO LE DATE DECORRENZA/SCADENZA X CORRETTA ESPOSIZIONE IN STAMPA
024800000202     C                   Z-ADD     VSSDDE        G08INV
024900000202     C                   Z-ADD     *ZEROS        G08DAT
025000000202     C                   MOVEL     '3'           G08ERR
025100000202     C                   CALL      'XSRDA8'
025200000202     C                   PARM                    WLBDA8
025300000202     C                   Z-ADD     G08DAT        STADDE
025400000202     C     G08ERR        IFEQ      '1'
025500000202     C                   MOVEL     '1'           $ERR
025600000202     C                   ENDIF
025700000202     C                   Z-ADD     VSSDSC        G08INV
025800000202     C                   Z-ADD     *ZEROS        G08DAT
025900000202     C                   MOVEL     '3'           G08ERR
026000000202     C                   CALL      'XSRDA8'
026100000202     C                   PARM                    WLBDA8
026200000202     C                   Z-ADD     G08DAT        STADSC
026300000202     C     G08ERR        IFEQ      '1'
026400000202     C                   MOVEL     '1'           $ERR
026500000202     C                   ENDIF
026600000203     C*  VERIFICO CHE IL CLIENTE ABILITATO IN CORSO ABIIA FIGLI LEGATI
026700000202     C                   EXSR      chkkun
026800000203     C*  PRIMA DI STAMPARE LA RIGA DI CORPO (DETTAGLIO) VERIFICO DI NON ESSERE A FINE
026900000203     C*  PAGINA, SE SI => PRIMA DEL DETTAGLIO STAMPO RIGA DI CHIUSURA PAGINA E TESTATA
027000000203     C*  A PAGINA NUOVA
027100000202     C     *IN05         IFEQ      *ON
027200000202     C                   WRITE     S726RIG
027300000202     C                   WRITE     S726TE
027400000202     C                   SETOFF                                       05
027500000202     C                   ENDIF
027600000202     C                   WRITE     S726VSS
027700000202     C                   ENDIF
027800000202     C                   ELSE
027900000203     C*  SE DATA NON OK ACCENDO *IN75 X NON CERCARE/STAMPARE EVENTUALI FIGLI COLL.
028000000202     C                   SETON                                        75
028100000202     C                   ENDIF
028200000202     C                   READ      TIVSS03L                               70
028300000202     C                   ENDDO
028400000203     C*  A ROTTURA VERIFICO EFFETTIVA RICHIESTA A VIDEO DI STAMOARE ANCHE I "FIGLI"
028500000203     C*  E STATO *IN75 X DECISIONE REPERIMENTO E STAMPA CLIENTI FIGLI
028600000202     C     *IN75         IFEQ      *OFF
028700000202     C     VIDKUN        ANDEQ     'S'
028800000202     C                   MOVE      SAVKSU        kkucop
028900000202     C                   MOVEL     SAVTLE        kkutle
029000000202     C     KEYKUN        SETLL     tikun03l
029100000202     C     KEYKUN        READE     tikun03l                               71
029200000202     C     *IN71         DOWEQ     *OFF
029300000203     C*  "SBIANCO" RECORD DI STAMPA E NE RIEMPIO BUFFER
029400000202     C                   CLEAR                   S726KUN
029500000202     C                   MOVEL     KUNCOF        STACOF
029600000202     C                   MOVE      KUNCOF        POGCOD
029700000203     C*  REPERISCO I DATI ANAGRAFICI DEL CLIENTE FIGLIO
029800000202     C                   EXSR      dectib
029900000203     C*  INVERTO LE DATE DECORRENZA/SCADENZA X CORRETTA ESPOSIZIONE IN STAMPA
030000000202     C                   Z-ADD     KUNDDE        G08INV
030100000202     C                   Z-ADD     *ZEROS        G08DAT
030200000202     C                   MOVEL     '3'           G08ERR
030300000202     C                   CALL      'XSRDA8'
030400000202     C                   PARM                    WLBDA8
030500000202     C                   Z-ADD     G08DAT        STADDE
030600000202     C     G08ERR        IFEQ      '1'
030700000202     C                   MOVEL     '1'           $ERR
030800000202     C                   ENDIF
030900000202     C                   Z-ADD     KUNDSC        G08INV
031000000202     C                   Z-ADD     *ZEROS        G08DAT
031100000202     C                   MOVEL     '3'           G08ERR
031200000202     C                   CALL      'XSRDA8'
031300000202     C                   PARM                    WLBDA8
031400000202     C                   Z-ADD     G08DAT        STADSC
031500000202     C     G08ERR        IFEQ      '1'
031600000202     C                   MOVEL     '1'           $ERR
031700000202     C                   ENDIF
031800000203     C*  PRIMA DI STAMPARE LA RIGA DI CORPO (DETTAGLIO) VERIFICO DI NON ESSERE A FINE
031900000203     C*  PAGINA, SE SI => PRIMA DEL DETTAGLIO STAMPO RIGA DI CHIUSURA PAGINA E TESTATA
032000000203     C*  A PAGINA NUOVA
032100000202     C     *IN05         IFEQ      *ON
032200000202     C                   WRITE     S726RIG
032300000202     C                   WRITE     S726TE
032400000202     C                   SETOFF                                       05
032500000202     C                   ENDIF
032600000202     C                   WRITE     S726KUN
032700000202     C     KEYKUN        READE     tikun03l                               71
032800000202     C                   ENDDO
032900000202     C                   ENDIF
033000000203     C*  SE *IN76 SPENTO => STAMPO RIGA VUOTA DI SEPARAZIONE, DEVO CONDIZIONARLO
033100000203     C*  ALTRIMENTI STAMPEREBBE RIGHE DI SEPARAZIONE ANCHE PER I CLIENTI ABILITATI
033200000203     C*  LETTI MA NON ANDATI EFFETTIVAMENTE IN STAMPA
033300000202     C  N76              WRITE     S726BLK
033400000203     C*  ALLA FINE DI TUTTE LE OPERAZIONI RI-SALVO I VALORI CHIAVE X NUOVA ROTTURA
033500000202     C                   MOVEL     VSSKSU        SAVKSU
033600000202     C                   MOVEL     VSSTLE        SAVTLE
033700000202     C                   ENDDO
033800000203     C*  UNA VOLTA FUORI CICLO ESEGUO LE OPERAZIONI FINALI DI CHIUSURA-COMPLETAMENTO
033900000203     C*  STAMPA => STAMPO RIGA DI CHIUSURA E RIGA DI <FINE PROSPETTO>
034000000202     C                   WRITE     S726RIG
034100000202     C                   WRITE     S726FIN
034200000202     C*
034300000202     C                   ENDSR
034400000202     C*------------------------------------------------------------------------*
034500000202     C* chkkun - Control esistenza figli per padri abilitati con legami
034600000202     C*------------------------------------------------------------------------*
034700000202     C     chkkun        BEGSR
034800000202     C*
034900000202     C* ESEGUE CONTROLLO SU TIKUN PER I TIPI LEGAMI
035000000202     C                   movel     *blanks       STANOTE
035100000202     C                   setoff                                       75
035200000202     C*
035300000202     C     vsstle        ifne      *blanks
035400000202     C                   MOVEL     vsstle        kkutle
035500000202     C                   MOVEL     *blanks       kkuazp
035600000202     C                   MOVE      vssksu        kkucop
035700000202     C     KEYku1        chain     tikun01l                           77
035800000202     C     *in77         ifeq      *on
035900000202     C                   movel     DES(1)        STANOTE
036000000202     C                   seton                                        75
036100000202     C                   endif
036200000202     C                   endif
036300000202     C*
036400000202     C                   ENDSR
036500000201     C*------------------------------------------------------------------------*
036600000201     C* dectib - Controlla esistenza codice cliente
036700000201     C*------------------------------------------------------------------------*
036800000201     C     dectib        BEGSR
036900000201     C*
037000000201     C                   CLEAR                   BS69DS
037100000201     C                   CLEAR                   ACODS
037200000201     C                   MOVEL     KNSIF         I69SIF
037300000202     C                   MOVE      POGCOD        I69KAC
037400000201     C                   CALL      'TIBS69R'
037500000201     C                   PARM                    BS69DS
037600000201     C                   PARM                    ACODS
037700000201     C                   PARM                    INDDS
037800000201     C                   PARM                    CLPDS
037900000201     C                   PARM                    CLSDS
038000000201     C     O69ERR        IFNE      '1'
038100000202     C                   MOVEL     ACORAG        STARAG
038200000201     C                   ELSE
038300000201     C                   MOVEL     *ALL'*'       STARAG
038400000201     C                   ENDIF
038500000201     C*
038600000201     C                   ENDSR
038700970603     C*--------------------------------------------------------------------------------------------*
038800970603     C* DECODI - CONTROLLA CAMPI E DECODIFICA
038900970603     C*--------------------------------------------------------------------------------------------*
039000970603     C     DECODI        BEGSR
039100970603     C*
039200970603     C* REIMPOSTA INDICATORI DI ERRORE
039300000201     C                   SETOFF                                       6061
039400970603     C*--------------------
039500000201     C* DATA
039600970603     C*--------------------
039700970603IF  1C     $ERR          IFEQ      '0'
039800000201     C                   Z-ADD     VIDDAT        G08DAT
039900970603     C                   Z-ADD     *ZEROS        G08INV
040000970603     C                   MOVEL     '0'           G08ERR
040100970603     C                   CALL      'XSRDA8'
040200970603     C                   PARM                    WLBDA8
040300000201     C                   Z-ADD     G08DAT        VIDDAT
040400970603IF  2C     G08ERR        IFEQ      '1'
040500970603     C                   MOVEL     '1'           $ERR
040600970603     C                   MOVEL     MSG(8)        $MSG
040700970603     C                   EXSR      CARMSG
040800000201     C                   SETON                                        60
040900970603E   2C                   ENDIF
041000970603E   1C                   ENDIF
041100970603     C*--------------------
041200000201     C* TIPO SERVIZIO
041300970603     C*--------------------
041400000202IF  1C     $ERR          IFEQ      '0'
041500000201IF  2C     VIDisv        IFne      *blank
041600000201     c                   exsr      cntisv
041700000201X   2C                   ELSE
041800000202     C                   MOVEL     '1'           $ERR
041900000201     C                   SETON                                        61
042000000201     C                   MOVEL     MSG(12)       DSMSMS
042100000201E   2C                   ENDIF
042200000201E   1C                   ENDIF
042300000201     C*
042400970603     C                   ENDSR
042500000201     C*------------------------------------------------------------------------*
042600000201     C* Cntisv - CONTROLLA codice tipo servizio
042700000201     C*------------------------------------------------------------------------*
042800000201     c     cntisv        begsr
042900000201     c                   clear                   TIBS02DS
043000000201      *
043100000201     C     '?'           SCAN      VIDISV                                 30
043200000201      *  Ricerca
043300000201     c                   eval      t02tla = 'L'
043400000202     C                   IF        *IN30 = *ON
043500000201     c                   movel     'R'           t02mod
043600000201     c                   else
043700000201     c                   movel     'C'           t02mod
043800000201     c                   movel(p)  vidisv        t02ke1
043900000201     c                   end
044000000201     C                   movel     'ISV'         t02cod
044100000201     c                   movel     knsif         t02sif
044200000201     C                   CALL      'TIBS02R'
044300000201     c                   parm                    KPJBA
044400000201     c                   parm                    TIBS02DS
044500000201     c
044600000201      *
044700000201    2C                   IF        T02err = *blanks
044800000201     C                   movel     T02ke1        Vidisv
044900000202     C                   movel     T02uni        DESSRV           40
045000000201     c                   else
045100000202     c                   movel     '1'           $ERR
045200000201     c                   seton                                        62
045300000201     C                   MOVEL     MSG(9)        DSMSMS
045400000201     C                   ENDIF
045500000201     c
045600000201     c                   endsr
045700970603     C*--------------------------------------------------------------------------------------------*
045800970603     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
045900970603     C*--------------------------------------------------------------------------------------------*
046000970603     C     IMPVID        BEGSR
046100970603     C*
046200970603     C* CALCOLA LA DATA CORRENTE
046300970603     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
046400970603     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
046500970603     C                   Z-ADD     WNUM8         G08DAT
046600970603     C                   Z-ADD     *ZEROS        G08INV
046700970603     C                   MOVEL     '0'           G08ERR
046800970603     C                   CALL      'XSRDA8'
046900970603     C                   PARM                    WLBDA8
047000000202     C                   Z-ADD     G08DAT        VIDDAT                         *DATA CORRENTE AA/M/
047100970603     C*
047200000201     C                   MOVEL     'S'           VIDKUN
047300970603     C*
047400970603     C                   ENDSR
047500020916     C*--------------------------------------------------------------------------------------------*
047600020916     C* REPERISCE I DATI UTENTE
047700020916     C*--------------------------------------------------------------------------------------------*
047800020916     C     REPDATIUTE    BEGSR
047900020916     C*
048000020916     C* INIZIALIZZA VARIABILI DI WRK
048100020916     C                   CLEAR                   TIBS34DS
048200020916     C                   CLEAR                   AZUTEDS
048300020916     C                   CLEAR                   DDATIUTE
048400020916     C*
048500020916     C     *DTAARA       DEFINE    §azute        azuteds
048600020916     C     *DTAARA       DEFINE    §datiute      ddatiute
048700020916     C                   IN(E)     *DTAARA
048800020916if  1C                   IF        %Error
048900020916     c                   EVAL      I34Tla = 'L'
049000020916     C                   CALL      'TIBS34R'
049100020916     C                   PARM                    Tibs34Ds
049200020916     C                   IN        *DTAARA
049300020916e   1C                   ENDIF
049400020916     C*
049500020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
049600020916     C                   MOVEL(P)  RSUT          DSFIRS           20
049700020916     C*
049800020916     C                   ENDSR
049900970603     C*--------------------------------------------------------------------------------------------*
050000970603     C* *INZSR  - ROUTINE INIZIALE
050100970603     C*--------------------------------------------------------------------------------------------*
050200970603     C     *INZSR        BEGSR
050300970603     C*--------------------
050400970603     C* RICEVIMENTO PARAMETRI
050500970603     C*--------------------
050600970603     C     *ENTRY        PLIST
050700970612     C                   PARM                    KPJBA
050800970603     C*--------------------
050900970603     C* VARIABILI RIFERITE AL DATA BASE
051000970603     C*--------------------
051100000201     C     *LIKE         DEFINE    VSSSUN        Ksssun                         *DPCPR01L
051200000201     C     *LIKE         DEFINE    VSSTLE        Ksstle                         *DPCPR01L
051300000201     C     *LIKE         DEFINE    VSSKSU        Kssksu                         *DPCPR01L
051400000201     C     *LIKE         DEFINE    KUNTLE        Kkutle                         *DPCPR01L
051500000202     C     *LIKE         DEFINE    KUNCOP        Kkucop                         *DPCPR01L
051600000202     C     *LIKE         DEFINE    KUNAZP        Kkuazp                         *DPCPR01L
051700000202     C     *LIKE         DEFINE    VSSKSU        POGCOD                         *DPCPR01L
051800970603     C*--------------------
051900970603     C* CHIAVI DI LETTURA
052000970603     C*--------------------
052100000201     C* LETTURA TIVSS03L - COMPLETA
052200000201     C     KEYVSS        KLIST
052300000201     C                   KFLD                    Kssksu                         *VERSIONE
052400000201     C                   KFLD                    Ksssun                         *VERSIONE
052500000201     C                   KFLD                    Ksstle                         *VERSIONE
052600000201     C* LETTURA TIKUN03L - COMPLETA
052700000201     C     KEYKUN        KLIST
052800000201     C                   KFLD                    Kkucop                         *VERSIONE
052900000201     C                   KFLD                    Kkutle                         *VERSIONE
053000000202     C* LETTURA TIKUN01L - COMPLETA
053100000202     C     KEYKU1        KLIST
053200000202     C                   KFLD                    kkutle                         *TERMINAL PARTENZA
053300000202     C                   KFLD                    kkuazp                         *PARTENZA/ARRIVO
053400000202     C                   KFLD                    kkucop                         *PARTENZA/ARRIVO
053500000201     C*
053600970603     C*
053700970603     C* ESEGUE OPERAZIONI INIZIALI STANDARD
053800970603     C                   EXSR      INZSTD
053900970603     C*
054000970603     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
054100970603     C                   EXSR      IMPVID
054200970603     C*
054300970603     C* EMETTO PRIMO FORMATO VIDEO
054400970603     C                   EXSR      CARV1
054500970603     C*
054600970603     C                   ENDSR
054700970603     C*--------------------------------------------------------------------------------------------*
054800970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
054900970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
055000970603     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
055100970603     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
055200970603     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
055300970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
055400970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
055500970603     C*--------------------------------------------------------------------------------------------*
055600970603     C* INZSTD - ROUTINE INIZIALE STANDARD
055700970603     C*--------------------------------------------------------------------------------------------*
055800970603     C     INZSTD        BEGSR
055900970603     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
056000970603     C                   CALL      'XKEYS'
056100970603     C                   PARM                    DSKEYS
056200970603     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
056300970603     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
056400970603     C     XMSGPA        PLIST
056500970603     C                   PARM                    DSMSMS
056600970603     C                   PARM                    DSMSPG
056700970603     C                   PARM                    DSMSKE
056800970603     C                   PARM                    DSMSER
056900970603     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
057000000201     C                   MOVEL     'TIS726R'     DSMSPG                         *NOME PGM CHIAMANTE
057100000201     C                   MOVEL     'TIS726R'     VMCQUE                         *CODA MESSAGGI DESTI
057200970603     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
057300970603     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
057400970603     C* INIZIALIZZO FLAG CONTROLLO PGM
057500970603     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
057600970603     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
057700970603     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
057800970603     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
057900970603     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
058000970603     C                   MOVEL     '0'           $FINE             1            *FINE PGM
058100970603     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
058200970603     C* LANCIO ROUTINE PER VERIFICARE SE
058300970603     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
058400970603     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
058500020916     C                   EXSR      REPDATIUTE
058600970603     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
058700970603     C                   BITOFF    '01234567'    POSMI1            1
058800970603     C                   BITOFF    '01234567'    POSMI2            1
058900970603     C                   BITOFF    '01234567'    POSMA1            1
059000970603     C                   BITOFF    '01234567'    POSMA2            1
059100970603     C                   BITON     '34'          POSMI1            1            *18
059200970603     C                   BITON     '7'           POSMI2            1            *01
059300970603     C                   BITON     '34'          POSMA1            1            *18
059400970603     C                   BITON     '0'           POSMA2            1            *80
059500970603     C                   MOVE      POSMI2        POSMIN            2
059600970603     C                   MOVEL     POSMI1        POSMIN
059700970603     C                   MOVE      POSMA2        POSMAX            2
059800970603     C                   MOVEL     POSMA1        POSMAX
059900970603     C                   ENDSR
060000970603     C*--------------------------------------------------------------------------------------------*
060100970603     C* FUNSTD - LANCIA SUBROUTINE STANDARD
060200970603     C*--------------------------------------------------------------------------------------------*
060300970603     C     FUNSTD        BEGSR
060400970603     C                   SELECT
060500970603     C     KEY           WHENEQ    F3
060600970603     C                   EXSR      F03STD
060700970603     C     KEY           WHENEQ    F12
060800970603     C                   EXSR      F12STD
060900970603     C     KEY           WHENEQ    F24
061000970603     C                   EXSR      F24STD
061100970603     C                   OTHER
061200970603     C                   MOVEL     MSG(2)        $MSG
061300970603     C                   EXSR      CARMSG
061400970603     C                   ENDSL
061500970603     C                   ENDSR
061600970603     C*--------------------------------------------------------------------------------------------*
061700970603     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
061800970603     C*--------------------------------------------------------------------------------------------*
061900970603     C     F03STD        BEGSR
062000970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
062100970603     C                   MOVEL     '1'           $FINE
062200970603     C* IMPOSTA FLAG DI FINE PROCEDURA
062300970603     C                   MOVEL     '1'           $F03
062400970603     C                   ENDSR
062500970603     C*--------------------------------------------------------------------------------------------*
062600970603     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
062700970603     C*--------------------------------------------------------------------------------------------*
062800970603     C     F12STD        BEGSR
062900970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
063000970603     C                   MOVEL     '1'           $FINE
063100970603     C* IMPOSTA FLAG DI FINE PROCEDURA
063200970603     C                   MOVEL     '1'           $F12
063300970603     C                   ENDSR
063400970603     C*--------------------------------------------------------------------------------------------*
063500970603     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
063600970603     C*--------------------------------------------------------------------------------------------*
063700970603     C     F24STD        BEGSR
063800970603     C*
063900970603     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
064000970603     C                   EXSR      OUTFUN
064100970603     C*
064200970603     C                   ENDSR
064300970603     C*--------------------------------------------------------------------------------------------*
064400970603     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
064500970603     C*          CODA MESSAGGI DEL PGM
064600970603     C*--------------------------------------------------------------------------------------------*
064700970603     C     CARMSG        BEGSR
064800970603     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
064900970603     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
065000970603     C     MSGULT        IFEQ      0
065100970603     C                   MOVEL     '1'           *IN40
065200000201     C                   WRITE     S726DVK
065300970603     C                   MOVEL     '0'           *IN40
065400970603     C                   ENDIF
065500970603     C* INVIA MESSAGGIO ALLA CODA DEL PGM
065600970603     C                   MOVEL     $MSG          DSMSMS
065700970603     C                   CALL      'XMSG'        XMSGPA
065800970603     C     DSMSER        IFEQ      0
065900970603     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
066000970603     C                   ENDIF
066100970603     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
066200970603     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
066300000201     C                   WRITE     S726DVM                                      *SFL MESS
066400970603     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
066500970603     C                   MOVEL     *BLANKS       $MSG
066600970603     C                   ENDSR
066700970603     C*--------------------------------------------------------------------------------------------*
066800970603     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
066900970603     C*--------------------------------------------------------------------------------------------*
067000970603     C     OUTMSG        BEGSR
067100970603     C* VISUALIZZAZIONE MESSAGGIO
067200970603     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
067300000201     C                   WRITE     S726DVK                                      *CONTR MESS
067400970603     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
067500970603     C                   Z-ADD     *ZEROS        MSGULT
067600970603     C                   ENDSR
067700970603     C*--------------------------------------------------------------------------------------------*
067800970603     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
067900970603     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
068000970603     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
068100970603     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
068200970603     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
068300970603     C*--------------------------------------------------------------------------------------------*
068400970603     C     CARFUN        BEGSR
068500970603     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
068600970603     C     1             DO        31            I                 5 0          --- 1 -->
068700970603     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
068800970603     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
068900970603     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
069000020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
069100970603     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
069200970603     C                   ELSE                                                   <-- 2 --> *FILIALE
069300970603     C                   MOVEL     FUNAFG        FAT(I)
069400970603     C                   ENDIF                                                  <-- 2 ---
069500970603     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
069600970603     C                   MOVEL     USER          WEDP              3            *PROFILO
069700970603     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
069800970603     C                   MOVEL     'S'           FAT(I)                         *ABILITO
069900970603     C                   ELSE
070000970603     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
070100970603     C                   ENDIF
070200970603     C                   ENDIF
070300970603     C                   ENDDO                                                  <-- 1 ---
070400970603     C                   ENDSR
070500970603     C*--------------------------------------------------------------------------------------------*
070600970603     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
070700970603     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
070800970603     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
070900970603     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
071000970603     C*--------------------------------------------------------------------------------------------*
071100970603     C     OUTFUN        BEGSR
071200970603     C     1             DO        8             I                              *PULIZIA FUNZIONI A
071300970603     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
071400970603     C                   ENDDO
071500970603     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
071600970603     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
071700970603     C     1             DO        31            I
071800970603     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
071900970603     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
072000970603     C                   ADD       1             FUNNAT
072100970603     C                   ENDIF
072200970603     C                   ENDDO
072300970603     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
072400970603     C     FUNNAT        IFGT      8
072500970603     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
072600970603     C                   ENDIF
072700970603     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
072800970603     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
072900970603     C                   Z-ADD     8             FUNVIS            5 0
073000970603     C     FAT(23)       IFEQ      'S'
073100970603     C                   SUB       1             FUNVIS
073200970603     C                   ENDIF
073300970603     C     FAT(24)       IFEQ      'S'
073400970603     C                   SUB       1             FUNVIS
073500970603     C                   ENDIF
073600970603     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
073700970603     C                   Z-ADD     *ZEROS        W                 5 0
073800970603     C                   ADD       1             FUNULT
073900970603     C     FUNULT        IFGT      31
074000970603     C                   Z-ADD     1             FUNULT
074100970603     C                   ENDIF
074200970603     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
074300970603     C     FUNULT        DO        31            I                              --- 1 -->
074400970603     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
074500970603     C     W             IFLT      FUNVIS                                       --- 2 -->
074600970603     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
074700970603     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
074800970603     C     FDE(I)        ANDNE     *BLANKS
074900970603     C     I             IFEQ      23                                           *TOLGO F23
075000970603     C     I             OREQ      24                                           *E F24
075100970603     C                   ELSE
075200970603     C                   ADD       1             W
075300970603     C                   MOVEL     FDE(I)        FUV(W)
075400970603     C                   ENDIF
075500970603     C                   ENDIF                                                  <-- 3 ---
075600970603     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
075700970603     C                   Z-ADD     I             FUNULT
075800970603     C                   ENDIF                                                  <-- 2 ---
075900970603     C                   ENDDO                                                  <-- 1 ---
076000970603     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
076100970603     C                   MOVEL     FUV(1)        VFD001
076200970603     C                   MOVEL     FUV(2)        VFD002
076300970603     C                   MOVEL     FUV(3)        VFD003
076400970603     C                   MOVEL     FUV(4)        VFD004
076500970603     C                   MOVEL     FUV(5)        VFD005
076600970603     C                   MOVEL     FUV(6)        VFD006
076700970603     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
076800970603     C     FAT(24)       ANDEQ     'N'
076900970603     C                   MOVEL     FUV(7)        VFD007
077000970603     C                   MOVEL     FUV(8)        VFD008
077100970603     C                   ELSE                                                   <-- 1 -->
077200970603     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
077300970603     C     FAT(24)       ANDEQ     'S'
077400970603     C                   MOVEL     FDE(23)       VFD007
077500970603     C                   MOVEL     FDE(24)       VFD008
077600970603     C                   ELSE                                                   <-- 2 -->
077700970603     C                   MOVEL     FUV(7)        VFD007
077800970603     C     FAT(23)       IFEQ      'S'
077900970603     C                   MOVEL     FDE(23)       VFD008
078000970603     C                   ELSE
078100970603     C                   MOVEL     FDE(24)       VFD008
078200970603     C                   ENDIF
078300970603     C                   ENDIF                                                  <-- 2 ---
078400970603     C                   ENDIF                                                  <-- 1 ---
078500970603     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
078600970603     C                   ENDSR
078700970603     C*--------------------------------------------------------------------------------------------*
078800970603     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
078900970603     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
079000970603     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
079100970603     C*                   - ERRORE SE FUNZIONE NON ATTIVA
079200970603     C*--------------------------------------------------------------------------------------------*
079300970603     C     CONFUN        BEGSR
079400970603     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
079500970603     C     1             DO        31            I                              --- 1 -->
079600970603     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
079700970603     C* CONTROLLO SE IL TASTO E' ATTIVO
079800970603     C     FAT(I)        IFNE      'S'                                          --- 3 -->
079900970603     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
080000970603     C                   EXSR      CARMSG
080100970603     C                   MOVEL     '1'           $ERR
080200970603     C                   ENDIF                                                  <-- 3 ---
080300970603     C                   ENDIF                                                  <-- 2 ---
080400970603     C                   ENDDO                                                  <-- 1 ---
080500970603     C                   ENDSR
080600970603     O*--------------------------------------------------------------------------------------------*
080700970603** -MSG- (MESSAGGI)
080800000201TIS726R- Tasto funzionale non valido.                                          1
080900000201TIS726R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
081000000201TIS726R-                                                                       3
081100000201TIS726R-                                                                       4
081200000201TIS726R-                                                                       5
081300000201TIS726R-                                                                       6
081400000201TIS726R-                                                                       7
081500000201TIS726R- Data errata                                                           8
081600000201TIS726R-                                                                       9
081700000201TIS726R-                                                                      10
081800000201TIS726R-                                                                      11
081900000201TIS726R-                                                                      12
082000000201TIS726R-                                                                      13
082100000201TIS726R-                                                                      14
082200000201TIS726R-                                                                      15
082300000201TIS726R-                                                                      16
082400000201TIS726R-                                                                      17
082500000201TIS726R-                                                                      18
082600000201TIS726R-                                                                      19
082700000201TIS726R-                                                                      20
082800970603** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
082900970603F1    -                   -    NN     NN
083000970603F2    -                   -    NN     NN
083100970603F3    -F3=Fine            -    NN     NN
083200970603F4    -                   -    NN     NN
083300970603F5    -                   -    NN     NN
083400970603F6    -F6=Conferma        -    SN     SN
083500970603F7    -                   -    NN     NN
083600970603F8    -                   -    NN     NN
083700970603F9    -                   -    NN     NN
083800970603F10   -                   -    NN     NN
083900970603F11   -                   -    NN     NN
084000970603F12   -F12=Ritorno        -    SS     SS
084100970603F13   -                   -    NN     NN
084200970603F14   -                   -    NN     NN
084300970603F15   -                   -    NN     NN
084400970603F16   -                   -    NN     NN
084500970603F17   -                   -    NN     NN
084600970603F18   -                   -    NN     NN
084700970603F19   -                   -    NN     NN
084800970603F20   -                   -    NN     NN
084900970603F21   -                   -    NN     NN
085000970603F22   -                   -    NN     NN
085100970603F23   -                   -    NN     NN
085200970603F24   -F24=Altri tasti    -    NN     NN
085300970603ENTER -                   -    SS     SS
085400970603ROLLUP-                   -    NN     NN
085500970603ROLLDN-                   -    NN     NN
085600970603HELP  -                   -    NN     NN
085700970603PRINT -                   -    NN     NN
085800970603BSPACE-                   -    NN     NN
085900970603CLEAR -                   -    NN     NN
086000000202** -DES- (NOTE)
086100000202ATTENZIONE: PRESENZA DI LEGAMI IN ASSENZA DI FIGLI COLLEGATI.                  1
086200000202                                                                               2
086300000202                                                                               3
086400000202                                                                               4
086500000202                                                                               5
