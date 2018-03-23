000100000000     H*------------------------------------------------------------------------*
000200000000     H*  Nome programma:  TREC87R                                              *
000300000000     H*  Descrizione   :  Subfile di scelta errori di Conto Economico          *
000400000000     H*  Autore        :  RAFFAELLA (EDPRN)                                    *
000500000000     H*------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700000000     F*------------------------------------------------------------------------*
000800000000     F* DATA BASE
000900000000     F*------------------------------------------------------------------------*
001000000000     FTREC87D   CF   E             WORKSTN INFDS(DEVDS1)
001100000000     F                                     SFILE(EC87DVS:SFLULT)
001200000000     F                                     SFILE(EC87DVM:MSGULT)
001300990812     FTITAS30C  IF   E           K DISK
001400020619     F                                     IGNORE(TITASP00)
001500000000     F*-----------
001600000000     F* DAT+TIM+PRG
001700000000     F*-----------
001800000000     FECERR01L  IF   E           K DISK
001900000000     F                                     RENAME(ECERR:ECERR1)
002000000000     F*-----------
002100970902     F* AAS+LNP+NRS+NSP+TBL+DAT+TIM+PRG
002200000000     F*-----------
002300970901     FECERR02L  UF   E           K DISK
002400000000     F                                     RENAME(ECERR:ECERR2)
002500000000     F*-----------
002600000000     F* AAS+LNP+NRS+NSP+DAM+ORM+PRG
002700000000     F*-----------
002800000000     FECERR03L  IF   E           K DISK
002900000000     F                                     RENAME(ECERR:ECERR3)
003000000000     D*------------------------------------------------------------------------*
003100000000     D* SCHIERE
003200000000     D*------------------------------------------------------------------------*
003300000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
003400000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003500000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)
003600000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)
003700000000     D FUN             S             40    DIM(31)
003800000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003900000000     D FFU             S              6    DIM(31)
004000000000     D FDE             S             19    DIM(31)
004100000000     D FAT             S              1    DIM(31)
004200000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
004300000000     D OPD             S              2    DIM(24)
004400000000     D OPS             S              2    DIM(24)
004500000000     D ODE             S             19    DIM(24)
004600000000     D OAT             S              1    DIM(24)
004700000000     D* TABELLA DESCRIZIONI A VIDEO
004800000000     D FUV             S             19    DIM(8)
004900000000     D OPV             S             19    DIM(8)
005000000000     D* TABELLA TASTI FUNZIONALI PREMUTI
005100000000     D KEYS            S              1    DIM(40)
005200000000     D*------------------------------------------------------------------------*
005300000000     D* INPUT
005400000000     D*------------------------------------------------------------------------*
005500000000     D*---------------------
005600000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
005700000000     D*---------------------
005800000000     D DSOPZ         E DS
005900000000     D  OPZRIG                 1     40
006000000000     D*---------------------
006100000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
006200000000     D*---------------------
006300000000     D DSFUN         E DS
006400000000     D  FUNRIG                 1     40
006500000000     D*---------------------
006600000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006700000000     D*---------------------
006800000000     D DSMSG         E DS
006900000000     D*---------------------
007000020923     D* DS REPERIMENTO DATI UTENTE
007100020923     D*-------------------
007200080908     D Tnsb50ds      E DS
007201080908     D TIBS34DS      E DS
007300020923     D DDATIUTE      E DS
007400020923     D AZUTEDS       E DS                  extname(AZUTE00F)
007500000000     D*---------------------
007600000000     D* DS PER GESTIONE TASTI FUNZIONALI
007700000000     D*---------------------
007800000000     D DSKEYS        E DS
007900000000     D  KEYRIG                 1     40
008000000000     D*---------------------
008100000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
008200000000     D*---------------------
008300000000     D                SDS
008400000000     D  $PGM                   1     10
008500000000     D  $USER                254    263
008600000000     D*---------------------
008700000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
008800000000     D*---------------------
008900000000     D DEVDS1          DS
009000000000     D* CHIAVE FUNZIONALE BATTUTA
009100000000     D  KEY                  369    369
009200000000     D* POSIZIONE CURSORE (IN HEX)
009300000000     D  CURSOR               370    371
009400000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
009500000000     D  FRTSFL               378    379B 0
009600000000     D*---------------------
009700000000     D* ARCHITETTURA
009800000000     D*---------------------
009900000000     D KPJBA         E DS
010000000000     D*------------------
010100000000     D* DS DI PROCEDURA
010200000000     D*------------------
010300000000     D DS87          E DS                  EXTNAME(TREC87DS)
010400000000     D*-------------------
010500000000     D* DS ESTERNA PER GUFO
010600000000     D*-------------------
010700000000     D DS£ECO        E DS
010800000000     D*------------------
010900000000     D* DS CONTROLLO DATA (8)
011000000000     D*------------------
011100000000     D WLBDA8          DS                  INZ
011200000000     D  G08DAT                 1      8  0
011300000000     D  G08INV                 9     16  0
011400000000     D  G08ERR                17     17
011500000000     D  G08TGI                18     22  0
011600991223     D*------------------
011700991223     D* S/COMPOSIZIONE DATA
011800991223     D*------------------
011900991223     D                 DS
012000991223     D  DSANN8                 1      4  0
012100991223     D  DSMES8                 5      6  0
012200991223     D  DSGIO8                 7      8  0
012300991223     D  DSDAT8                 1      8  0
012400991223     D                 DS
012500991223     D  DSGIO6                 1      2  0
012600991223     D  DSMES6                 3      4  0
012700991223     D  DSANN6                 5      6  0
012800991223     D  DSDAT6                 1      6  0
012900000000     D*-------------------
013000000000     D* DS DESCRIZIONE MESSAGGIO
013100000000     D*-------------------
013200000000     D                 DS
013300000000     D  DSERR                  1     80
013400000000     D  DSVOCE                29     31
013500000000     D  DSTT1                 33     35
013600000000     D  DSTT2                 37     39
013700000000     D  DSDER                  1     74
013800000000     D  DSCER                 75     78
013900000000     D*-------------------
014000000000     D* DS CODICE ERRORE
014100000000     D*-------------------
014200000000     D                 DS
014300000000     D  ERRDAM                 1      6  0
014400000000     D  ERRORM                 7     12  0
014500000000     D  ERRPRG                13     17  0
014600000000     D  ERRCOD                 1     12  0
014700000000     D*-------------------
014800000000     D* DS CODICE ERRORE DI LAVORO
014900000000     D*-------------------
015000000000     D                 DS
015100000000     D  VSHDAM                 1      6  0
015200000000     D  VSHORM                 7     12  0
015300000000     D  VSHPRG                13     17  0
015400000000     D  VSHCOD                 1     12  0
015500000000     D*-------------------
015600990812     D* DS VALORIZZAZIONE IMMAGINE ERRORE DA TITAS
015700000000     D*-------------------
015800000000     D TRECDS        E DS
015900000000     D  D$PDS1                 1    250
016000000000     D  D$PDS2               251    500
016100000000     D  D$DDS1               501    750
016200000000     D  D$DDS2               751   1000
016300000000     D*-------------------
016400030123     D* DS VALORIZZAZIONE IMMAGINE ERRORE DA TNCSB
016500000000     D*-------------------
016600000000     D DS$CSB        E DS
016700000000     D  DCPDS1                 1    250
016800000000     D  DCPDS2               251    500
016900000000     D  DCDDS1               501    750
017000000000     D  DCDDS2               751   1000
017100030123     D*-------------------
017200030123     D* DS VALORIZZAZIONE IMMAGINE ERRORE DA FIAR5 - RECORD TRANSITI
017300030123     D*-------------------
017400030123     D DS$AR5T       E DS
017500030123     D  D$PAT1                 1    250
017600030123     D  D$DAT1               251    500
017700030123     D*-------------------
017800030123     D* DS VALORIZZAZIONE IMMAGINE ERRORE DA FIAR5 - RECORD BANCALI
017900030123     D*-------------------
018000030123     D DS$AR5B       E DS
018100030123     D  D$PAB1                 1    250
018200030123     D  D$DAB1               251    500
018300000000     C*------------------------------------------------------------------------*
018400000000     C* MAIN LINE
018500000000     C*------------------------------------------------------------------------*
018600000000     C*
018700000000DO  1C     $FINE         DOUEQ     '1'
018800000000     C*
018900000000     C                   MOVEL     '0'           $ERR
019000000000     C*
019100000000     C* CONTROLLA GESTIONE VIDEO
019200000000IF  2C     $GES          IFEQ      'VS'
019300000000     C                   EXSR      GESVS
019400000000E   2C                   ENDIF
019500000000     C*
019600000000E   1C                   ENDDO
019700000000     C*
019800000000     C                   SETON                                        LR
019900000000     C*------------------------------------------------------------------------*
020000000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
020100000000     C*------------------------------------------------------------------------*
020200000000     C     CARVS         BEGSR
020300000000     C*
020400000000     C                   MOVEL     'VS'          $GES
020500000000     C*
020600000000     C                   EXSR      CAROPZ
020700000000     C                   EXSR      OUTOPZ
020800000000     C                   MOVEA     FUN2          FUN
020900000000     C                   EXSR      CARFUN
021000000000     C                   EXSR      OUTFUN
021100000000     C*
021200000000     C                   WRITE     EC87DVT
021300000000     C                   WRITE     EC87DVO
021400000000     C                   WRITE     EC87DVF
021500000000     C*
021600000000     C                   EXSR      POSSFL
021700000000     C*
021800000000     C                   ENDSR
021900000000     C*------------------------------------------------------------------------*
022000000000     C* GESVS  - GESTIONE SUBFILE
022100000000     C*------------------------------------------------------------------------*
022200000000     C     GESVS         BEGSR
022300000000     C*
022400000000IF  1C     $TESVD        IFEQ      '1'
022500000000     C                   WRITE     EC87DVT
022600000000     C                   WRITE     EC87DVO
022700000000     C                   WRITE     EC87DVF
022800000000E   1C                   ENDIF
022900000000     C*
023000000000     C                   EXSR      OUTSFL
023100000000     C*
023200000000     C                   EXSR      CONFUN
023300000000     C*
023400000000IF  1C     $ERR          IFEQ      '0'
023500000000SL  2C                   SELECT
023600000000     C                   OTHER
023700000000     C                   EXSR      FUNSFL
023800000000E   2C                   ENDSL
023900000000E   1C                   ENDIF
024000000000     C*
024100000000     C                   ENDSR
024200000000     C*------------------------------------------------------------------------*
024300000000     C*  GESOPZ - GESTIONE OPZIONI
024400000000     C*------------------------------------------------------------------------*
024500000000     C     GESOPZ        BEGSR
024600000000     C*
024700000000SL  1C                   SELECT
024800000000     C     VSCOPZ        WHENEQ    ' 1'
024900000000     C                   EXSR      O01GES
025000000000     C     VSCOPZ        WHENEQ    ' 2'
025100000000     C                   EXSR      O02GES
025200000000     C     VSCOPZ        WHENEQ    ' 4'
025300000000     C                   EXSR      O04GES
025400950519     C     VSCOPZ        WHENEQ    ' 5'
025500950519     C                   EXSR      O05GES
025600020607     C     VSCOPZ        WHENEQ    ' 6'
025700020607     C                   EXSR      O06GES
025701080908     C     VSCOPZ        WHENEQ    ' 7'
025702080908     C                   EXSR      O07GES
025800000000     C                   OTHER
025900000000     C                   MOVEL     MSG(4)        $MSG
026000000000     C                   EXSR      CARMSG
026100000000E   1C                   ENDSL
026200000000     C*
026300000000     C                   ENDSR
026400000000     C*------------------------------------------------------------------------*
026500000000     C* O01GES - GESTIONE OPZIONE 1=SCELTA RILANCIO CONTO ECONOMICO
026600000000     C*------------------------------------------------------------------------*
026700000000     C     O01GES        BEGSR
026800000000     C*
026900000000     C* E' POSSIBILE SELEZIONARE IL RECORD SOLO SE ANCORA PRESENTE IN ARCHIVIO
027000000000IF  1C     VSCGIR        IFNE      *BLANKS
027100000000     C                   MOVEL     '1'           $ERR
027200000000     C                   MOVEL     MSG(17)       $MSG
027300000000     C                   EXSR      CARMSG
027400000000E   1C                   ENDIF
027500000000     C*
027600000000     C* E' POSSIBILE SELEZIONARE IL RECORD SOLO SE E'IL PRIMO ERRORE DELLA BOLLA
027700000103IF  1C     $ERR          IFEQ      '0'
027800000103     C                   Z-ADD     VSHAAS        KE2AAS
027900000103     C                   Z-ADD     VSCLNP        KE2LNP
028000000000     C                   Z-ADD     VSCNRS        KE2NRS
028100000000     C                   Z-ADD     VSCNSP        KE2NSP
028200950329     C                   MOVEL     VSCTBL        KE2TBL
028300000000     C                   MOVEL     '0'           WPREC
028400950329     C     KEYER2        SETLL     ECERR02L
028500970901     C     KEYER2        READE(N)  ECERR02L                               96
028600000000DO  2C     *IN96         DOWEQ     *OFF
028700000000     C     WPREC         ANDEQ     '0'
028800000000IF  3C     ERRCOD        IFLT      VSHCOD
028900000000     C     ERRFIL        ANDEQ     VSHFIL
029000000000     C                   MOVEL     '1'           WPREC
029100000000E   3C                   ENDIF
029200970901     C     KEYER2        READE(N)  ECERR02L                               96
029300000000E   2C                   ENDDO
029400000000     C*
029500000000     C* EMETTE MESSAGGIO DI ERRORE
029600000000IF  2C     WPREC         IFEQ      '1'
029700000000     C                   MOVEL     '1'           $ERR
029800000000     C                   MOVEL     MSG(12)       $MSG
029900000000     C                   EXSR      CARMSG
030000000000E   2C                   ENDIF
030100000000E   1C                   ENDIF
030200000000     C*
030300020619     C* SE IL RECORD SELEZIONATO E' DI "INSERIMENTO" ED E' SU "TITAS*" SEGUE
030400000000     C* UNA PROCEDURA PARTICOLARE
030500000000IF  1C     $ERR          IFEQ      '0'
030600000000IF  2C     VSCSTS        IFEQ      'I'
030700000000     C     VSCFIL        ANDEQ     'T'
030800000000     C                   EXSR      STSINS
030900000000E   2C                   ENDIF
031000000000E   1C                   ENDIF
031100000000     C*
031200000000     C* SE NON CI SONO ERRORI PROCEDE AL RILANCIO DEL CONTO ECONOMICO
031300000000IF  1C     $ERR          IFEQ      '0'
031400000000     C                   EXSR      SCHOIS
031500000000E   1C                   ENDIF
031600000000     C*
031700000000     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
031800000000     C*
031900000000     C                   ENDSR
032000000000     C*------------------------------------------------------------------------*
032100000000     C* O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE SINGOLO ERRORE
032200000000     C*------------------------------------------------------------------------*
032300000000     C     O04GES        BEGSR
032400000000     C*
032500000000     C* E' POSSIBILE SELEZIONARE L'ERRORE SOLO SE ANCORA PRESENTE IN ARCHIVIO
032600000000IF  1C     VSCGIR        IFNE      *BLANKS
032700000000     C                   MOVEL     '1'           $ERR
032800000000     C                   MOVEL     MSG(17)       $MSG
032900000000     C                   EXSR      CARMSG
033000000000E   1C                   ENDIF
033100000000     C*
033200000000     C* E' POSSIBILE SELEZIONARE IL RECORD SOLO SE E'IL PRIMO ERRORE DELLA BOLLA
033300000000IF  1C     $ERR          IFEQ      '0'
033400000103     C                   Z-ADD     VSHAAS        KE2AAS
033500000000     C                   Z-ADD     VSCLNP        KE2LNP
033600000000     C                   Z-ADD     VSCNRS        KE2NRS
033700000000     C                   Z-ADD     VSCNSP        KE2NSP
033800950329     C                   MOVEL     VSCTBL        KE2TBL
033900000000     C                   MOVEL     '0'           WPREC
034000950329     C     KEYER2        SETLL     ECERR02L
034100970901     C     KEYER2        READE(N)  ECERR02L                               96
034200000000DO  2C     *IN96         DOWEQ     *OFF
034300000000     C     WPREC         ANDEQ     '0'
034400000000IF  3C     ERRCOD        IFLT      VSHCOD
034500000000     C     ERRFIL        ANDEQ     VSHFIL
034600000000     C                   MOVEL     '1'           WPREC
034700000000E   3C                   ENDIF
034800970901     C     KEYER2        READE(N)  ECERR02L                               96
034900000000E   2C                   ENDDO
035000000000     C*
035100000000     C* EMETTE MESSAGGIO DI ERRORE
035200000000IF  2C     WPREC         IFEQ      '1'
035300000000     C                   MOVEL     '1'           $ERR
035400000000     C                   MOVEL     MSG(14)       $MSG
035500000000     C                   EXSR      CARMSG
035600000000E   2C                   ENDIF
035700000000E   1C                   ENDIF
035800000000     C*
035900020619     C* SE IL RECORD SELEZIONATO E' DI "INSERIMENTO" ED E' SU "TITAS*" SEGUE
036000000000     C* UNA PROCEDURA PARTICOLARE
036100000000IF  1C     $ERR          IFEQ      '0'
036200000000IF  2C     VSCSTS        IFEQ      'I'
036300000000     C     VSCFIL        ANDEQ     'T'
036400000000     C                   EXSR      STSINS
036500000000E   2C                   ENDIF
036600000000E   1C                   ENDIF
036700000000     C*
036800000000     C* SE NON CI SONO ERRORI CANCELLA FISICAMENTE L'ERRORE DI CONTO ECONOMICO
036900000000IF  1C     $ERR          IFEQ      '0'
037000000000     C                   EXSR      CANCEL
037100000000E   1C                   ENDIF
037200000000     C*
037300000000     C                   ENDSR
037400000000     C*------------------------------------------------------------------------*
037500000000     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD DI TNCSB00F
037600000000     C*------------------------------------------------------------------------*
037700000000     C     O02GES        BEGSR
037800000000     C*
037900000000     C* E' POSSIBILE SELEZIONARE L'ERRORE SOLO SE ANCORA PRESENTE IN ARCHIVIO
038000000000IF  1C     VSCGIR        IFNE      *BLANKS
038100000000     C                   MOVEL     '1'           $ERR
038200000000     C                   MOVEL     MSG(17)       $MSG
038300000000     C                   EXSR      CARMSG
038400000000E   1C                   ENDIF
038500000000     C*
038600000000     C* E' POSSIBILE SELEZIONARE IL RECORD SOLO SE E'DI TIPO "INSERIMENTO" E SE
038700000000     C* IL FILE MODIFICATO E' TNCSB
038800000000IF  1C     $ERR          IFEQ      '0'
038900000000IF  2C     VSCSTS        IFEQ      'I'
039000000000     C     VSCFIL        ANDEQ     'C'
039100000000X   2C                   ELSE
039200000000     C                   MOVEL     '1'           $ERR
039300000000     C                   MOVEL     MSG(9)        $MSG
039400000000     C                   EXSR      CARMSG
039500000000E   2C                   ENDIF
039600000000E   1C                   ENDIF
039700000000     C*
039800000000     C* ESEGUE LA MODIFICA
039900000000IF  1C     $ERR          IFEQ      '0'
040000000103     C                   Z-ADD     VSHAAS        WAAS
040100000000     C                   Z-ADD     VSCLNP        WLNP
040200000000     C                   Z-ADD     VSCNRS        WNRS
040300950329     C                   Z-ADD     VSCNSP        WNSP
040400950329     C                   MOVEL     VSCTBL        WTBL
040500000000     C                   Z-ADD     VSHDAT        WDAT
040600000000     C                   Z-ADD     VSHTIM        WTIM
040700000000     C                   Z-ADD     VSHPRG        WPRG
040800000000     C                   Z-ADD     VSHDAM        WDAM
040900000000     C                   Z-ADD     VSHORM        WORM
041000000000     C                   MOVEL     *BLANKS       WOP0
041100000000     C                   MOVEL     'O02'         WOP1
041200020621     C                   MOVEL(P)  'TREC88R'     WPGM              8
041300000000     C                   EXSR      LANPGM
041400000000     C*
041500000000     C* CANCELLA TUTTE LE VARIAZIONI SUCESSIVE ALL'INS. CON IL TIPO BOLLA ERRATO
041600000000IF  2C     $ERR          IFEQ      '0'
041700000000     C                   Z-ADD     WAAS          KE2AAS
041800000000     C                   Z-ADD     WLNP          KE2LNP
041900000000     C                   Z-ADD     WNRS          KE2NRS
042000000000     C                   Z-ADD     WNSP          KE2NSP
042100950329     C                   MOVEL     WTBL          KE2TBL
042200950329     C     KEYER2        SETLL     ECERR02L
042300950329     C     KEYER2        READE     ECERR02L                               96
042400000000DO  3C     *IN96         DOWEQ     *OFF
042500950329IF  4C     ERRSTS        IFEQ      'U'
042600950329IF  4C     ERRFIL        ANDEQ     'TNCSB00F'
042700970901     C                   DELETE    ECERR2
042800000000E   4C                   ENDIF
042900950329     C     KEYER2        READE     ECERR02L                               96
043000000000E   3C                   ENDDO
043100000000     C*
043200000000     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
043300000000E   2C                   ENDIF
043400000000E   1C                   ENDIF
043500000000     C*
043600000000     C                   ENDSR
043700950519     C*------------------------------------------------------------------------*
043800950519     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZA RECORD
043900950519     C*------------------------------------------------------------------------*
044000950519     C     O05GES        BEGSR
044100950519     C*
044200950519     C* E' POSSIBILE SELEZIONARE L'ERRORE SOLO SE ANCORA PRESENTE IN ARCHIVIO
044300950519IF  1C     VSCGIR        IFNE      *BLANKS
044400950519     C                   MOVEL     '1'           $ERR
044500950519     C                   MOVEL     MSG(17)       $MSG
044600950519     C                   EXSR      CARMSG
044700950519E   1C                   ENDIF
044800950519     C*
044900950519     C* LANCIA LA VISUALIZZAZIONE
045000950519IF  1C     $ERR          IFEQ      '0'
045100000103     C                   Z-ADD     VSHAAS        WAAS
045200950519     C                   Z-ADD     VSCLNP        WLNP
045300950519     C                   Z-ADD     VSCNRS        WNRS
045400950519     C                   Z-ADD     VSCNSP        WNSP
045500950519     C                   MOVEL     VSCTBL        WTBL
045600950519     C                   Z-ADD     VSHDAT        WDAT
045700950519     C                   Z-ADD     VSHTIM        WTIM
045800950519     C                   Z-ADD     VSHPRG        WPRG
045900950519     C                   Z-ADD     VSHDAM        WDAM
046000950519     C                   Z-ADD     VSHORM        WORM
046100950519     C                   MOVEL     *BLANKS       WOP0
046200950519     C                   MOVEL     'O05'         WOP1
046300020621     C                   MOVEL(P)  'TREC88R'     WPGM              8
046400950519     C                   EXSR      LANPGM
046500950519     C*
046600950519     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
046700950519E   1C                   ENDIF
046800950519     C*
046900950519     C                   ENDSR
047000020607     C*------------------------------------------------------------------------*
047100020610     C* O06GES - GESTIONE OPZIONE 6=STAMPA BOLLA
047200020607     C*------------------------------------------------------------------------*
047300020607     C     O06GES        BEGSR
047400020607     C*
047500020607     C* E' POSSIBILE SELEZIONARE L'ERRORE SOLO SE ANCORA PRESENTE IN ARCHIVIO
047600020607IF  1C     VSCGIR        IFNE      *BLANKS
047700020607     C                   MOVEL     '1'           $ERR
047800020607     C                   MOVEL     MSG(17)       $MSG
047900020607     C                   EXSR      CARMSG
048000020607E   1C                   ENDIF
048100020607     C*
048200020610     C* LANCIA LA STAMPA
048300020607IF  1C     $ERR          IFEQ      '0'
048400020607     C                   Z-ADD     VSHAAS        WAAS
048500020607     C                   Z-ADD     VSCLNP        WLNP
048600020607     C                   Z-ADD     VSCNRS        WNRS
048700020607     C                   Z-ADD     VSCNSP        WNSP
048800020607     C                   MOVEL     VSCTBL        WTBL
048900020607     C                   Z-ADD     VSHDAT        WDAT
049000020607     C                   Z-ADD     VSHTIM        WTIM
049100020607     C                   Z-ADD     VSHPRG        WPRG
049200020607     C                   Z-ADD     VSHDAM        WDAM
049300020607     C                   Z-ADD     VSHORM        WORM
049400020607     C                   MOVEL     *BLANKS       WOP0
049500020607     C                   MOVEL     'O05'         WOP1
049600020621     C                   MOVEL(P)  'TREC87R1'    WPGM              8
049700020607     C                   EXSR      LANPGM
049800020607     C*
049900020607     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
050000020607E   1C                   ENDIF
050100020607     C*
050200020607     C                   ENDSR
050201080908     C*------------------------------------------------------------------------*
050202080908     C* O07GES - GESTIONE OPZIONE 7=interrogazione bolla
050203080908     C*------------------------------------------------------------------------*
050204080908     C     O07GES        BEGSR
050212080908     C*
050213080908     c                   clear                   tnsb50ds
050215080908     C                   Z-ADD     VSHAAS        d50AAS
050216080908     C                   Z-ADD     VSCLNP        d50LNP
050217080908     C                   Z-ADD     VSCNRS        d50NRS
050218080908     C                   Z-ADD     VSCNSP        d50NSP
050219080908     C                   MOVEL     VSCTBL        d50TBL
050226080908     C                   MOVEL     'O05'         I50OP0
050228080908     C                   movel(p)  tnsb50ds      kpjbu
050229080908     c                   call      'TNSB50R'
050230080908     c                   parm                    kpjba
050232080908     C*
050233080908     C                   ENDSR
050300000000     C*------------------------------------------------------------------------*
050400000000     C* STSINS - TRATTA L'ERRORE DI TIPO "INSERIMENTO"
050500000000     C*------------------------------------------------------------------------*
050600000000     C     STSINS        BEGSR
050700000000     C*
050800000000     C* SE E'UN ERRORE DI INSERIMENTO, CARICA LA SITUAZIONE ATTUALE DELLA BOLLA
050900000000     C* NELLA DS -DOPO- DELL'ERRORE E CANCELLA LE VARIAZIONI SUCESSIVE ("U").
051000000000     C* SE POI TROVA UN ERRORE DI CANCELLAZIONE ("D") CANCELLA ANCHE QUESTO
051100000000     C* INSIEME AL RECORD DI INSERIMENTO APPENA CARICATO E DA' ERRORE PER NON
051200000000     C* FARE ESEGUIRE L'OPZIONE RICHIESTA (RILANCIO C/E O CANCELLAZIONE).
051300000000     C***
051400000000     C* LEGGE LA BOLLA
051500000000     C***
051600000103     C                   Z-ADD     VSHAAS        KTAAAS
051700000000     C                   Z-ADD     VSCLNP        KTALNP
051800950329     C                   Z-ADD     VSCNRS        KTANRS
051900950329     C                   Z-ADD     VSCNSP        KTANSP
052000000000     C                   MOVEL     VSCTBL        KTATBL
052100990812     C     KEYTAS        CHAIN     TITAS30C                           97
052200000000     C***
052300000000     C* LEGGE L'ERRORE
052400000000     C***
052500000000IF  1C     *IN97         IFEQ      *OFF
052600000103     C                   Z-ADD     VSHAAS        KE2AAS
052700970901     C                   Z-ADD     VSCLNP        KE2LNP
052800970901     C                   Z-ADD     VSCNRS        KE2NRS
052900970901     C                   Z-ADD     VSCNSP        KE2NSP
053000970901     C                   MOVEL     VSCTBL        KE2TBL
053100970902     C                   Z-ADD     VSHDAT        KE2DAT
053200970902     C                   Z-ADD     VSHTIM        KE2TIM
053300970902     C                   Z-ADD     VSHPRG        KE2PRG
053400970901     C     KE2ER2        CHAIN     ECERR02L                           97
053500970901IF  2C     *IN97         IFEQ      *OFF
053600000000     C                   EXSR      APDTAQ
053700000000     C                   EXSR      CDDTAQ
053800000000     C                   MOVEL     D$PDS1        ERRRP1
053900000000     C                   MOVEL     D$PDS2        ERRRP2
054000000000     C                   MOVEL     D$DDS1        ERRRD1
054100000000     C                   MOVEL     D$DDS2        ERRRD2
054200000000     C***
054300000000     C* AGGIORNA L'ERRORE
054400000000     C***
054500000000     C                   EXCEPT    AGGERR
054600000000     C***
054700000000     C* LEGGE RECORD SUCESSIVI
054800000000     C***
054900000000     C* LEGGE FINO A FINE FILE O SE TROVA UNA "CANCELLAZIONE"
055000000103     C                   Z-ADD     VSHAAS        KW3AAS
055100000000     C                   Z-ADD     VSCLNP        KW3LNP
055200000000     C                   Z-ADD     VSCNRS        KW3NRS
055300000000     C                   Z-ADD     VSCNSP        KW3NSP
055400000000     C                   MOVEL     '0'           $PROS
055500000000     C     KEYEW3        SETLL     ECERR03L
055600000000     C     KEYEW3        READE     ECERR03L                               96
055700970901DO  3C     *IN96         DOWEQ     *OFF
055800000000     C     $PROS         ANDEQ     '0'
055900000000     C*
056000000000     C* ANALIZZA SOLO ERRORI DI VARIAZIONE O DI CANCELLAZIONE
056100970901IF  4C     ERRTBL        IFEQ      VSCTBL
056200970901IF  5C     ERRSTS        IFEQ      'D'
056300000000     C     ERRSTS        OREQ      'U'
056400000000     C*
056500000000     C* CANCELLA L'ERRORE "D" O "U"
056600970901     C                   Z-ADD     ERRAAS        KE2AAS
056700970901     C                   Z-ADD     ERRLNP        KE2LNP
056800970901     C                   Z-ADD     ERRNRS        KE2NRS
056900970901     C                   Z-ADD     ERRNSP        KE2NSP
057000970901     C                   MOVEL     ERRTBL        KE2TBL
057100970902     C                   Z-ADD     ERRDAT        KE2DAT
057200970902     C                   Z-ADD     ERRTIM        KE2TIM
057300970902     C                   Z-ADD     ERRPRG        KE2PRG
057400970901     C     KE2ER2        CHAIN     ECERR02L                           97
057500970901     C                   DELETE    ECERR2
057600000000     C*
057700000000     C* SE TROVA UNA CANCELLAZIONE, SI RICORDA DI CANCELLARE ANCHE LA "I", E DI
057800000000     C* NON PROSEGUIRE USCENDO DAL CICLO
057900970901IF  6C     ERRSTS        IFEQ      'D'
058000000000     C                   MOVEL     '1'           $PROS
058100000000     C                   MOVEL     '1'           $ERR
058200000103     C                   Z-ADD     VSHAAS        KE2AAS
058300970901     C                   Z-ADD     VSCLNP        KE2LNP
058400970901     C                   Z-ADD     VSCNRS        KE2NRS
058500970901     C                   Z-ADD     VSCNSP        KE2NSP
058600970901     C                   MOVEL     VSCTBL        KE2TBL
058700970902     C                   Z-ADD     VSHDAT        KE2DAT
058800970902     C                   Z-ADD     VSHTIM        KE2TIM
058900970902     C                   Z-ADD     VSHPRG        KE2PRG
059000970901     C     KE2ER2        CHAIN     ECERR02L                           97
059100970901     C                   DELETE    ECERR2
059200970901E   6C                   ENDIF
059300970901E   5C                   ENDIF
059400970901E   4C                   ENDIF
059500000000     C*
059600000000     C* LEGGE UN ALTRO RECORD SE DEVE PROSEGUIRE
059700970901IF  4C     $PROS         IFEQ      '0'
059800000000     C     KEYEW3        READE     ECERR03L                               96
059900970901E   4C                   ENDIF
060000970901E   3C                   ENDDO
060100970901E   2C                   ENDIF
060200000000     C*
060300000000     C* SI RIPOSIZIONA SU ECERR03L DALL'ULTIMO RECORD CARICATO NEL SUBFIL
060400000000     C     KEYER3        SETGT     ECERR03L                           98
060500000000     C*
060600000000X   1C                   ELSE
060700040624     c* se non trova la bolla in titas ma sono in cancellazione non metto
060800040624     c* l'errore
060900040624     c     VSCOPZ        ifne      ' 4'
061000000000     C                   MOVEL     '1'           $ERR
061100040624     C                   MOVEL     MSG(13)       $MSG
061200040624     C                   EXSR      CARMSG
061300000000E   1C                   ENDIF
061400040624E   1C                   ENDIF
061500000000     C*
061600000000     C                   ENDSR
061700000000     C*------------------------------------------------------------------------*
061800000000     C* SCHOIS - SCELTA DI CALCOLO DI CONTO ECONOMICO DI UNA SOLA BOLLA
061900000000     C*------------------------------------------------------------------------*
062000000000     C     SCHOIS        BEGSR
062100000000     C*
062200000000     C* LEGGE IL RECORD SELEZIONATO
062300000103     C                   Z-ADD     VSHAAS        KE2AAS
062400970901     C                   Z-ADD     VSCLNP        KE2LNP
062500970901     C                   Z-ADD     VSCNRS        KE2NRS
062600970901     C                   Z-ADD     VSCNSP        KE2NSP
062700970901     C                   MOVEL     VSCTBL        KE2TBL
062800970902     C                   Z-ADD     VSHDAT        KE2DAT
062900970902     C                   Z-ADD     VSHTIM        KE2TIM
063000970902     C                   Z-ADD     VSHPRG        KE2PRG
063100970901     C     KE2ER2        CHAIN     ECERR02L                           97
063200000000IF  1C     *IN97         IFEQ      *OFF
063300000000     C                   EXSR      GUFO
063400970901     C                   DELETE    ECERR2
063500000000     C* MESSAGGIO
063600000000     C                   MOVEL     '*'           VSCGIR
063700000000     C                   MOVEL     MSG(11)       $MSG
063800000000     C                   EXSR      CARMSG
063900000000E   1C                   ENDIF
064000000000     C*
064100000000     C                   ENDSR
064200000000     C*------------------------------------------------------------------------*
064300000000     C* LANPGM - LANCIA UN PROGRAMMA
064400000000     C*------------------------------------------------------------------------*
064500000000     C     LANPGM        BEGSR
064600000000     C*--------------------
064700000000     C* PARAMETRI INFORMATIVI
064800000000     C*--------------------
064900000000     C                   MOVEL     WOP0          D87OP0
065000000000     C                   MOVEL     WOP1          D87OP1
065100000000     C                   MOVEL     '0'           D87F03
065200000000     C                   MOVEL     '0'           D87F12
065300000000     C                   MOVEL     '0'           D87ERR
065400000000     C                   MOVEL     *BLANKS       D87MSG
065500000000     C*--------------------
065600000000     C* PARAMETRI
065700000000     C*--------------------
065800000000     C                   Z-ADD     WAAS          D87AAS
065900000000     C                   Z-ADD     WLNP          D87LNP
066000000000     C                   Z-ADD     WNRS          D87NRS
066100000000     C                   Z-ADD     WNSP          D87NSP
066200000000     C                   MOVEL     WTBL          D87TBL
066300000000     C                   Z-ADD     WDAT          D87DAT
066400000000     C                   Z-ADD     WTIM          D87TIM
066500000000     C                   Z-ADD     WPRG          D87PRG
066600000000     C                   Z-ADD     WDAM          D87DAM
066700000000     C                   Z-ADD     WORM          D87ORM
066800000000     C* LANCIO
066900000000     C                   MOVEL     DS87          KPJBU
067000000000     C                   CALL      WPGM
067100000000     C                   PARM                    KPJBA
067200000000     C                   MOVEL     KPJBU         DS87
067300000000     C*
067400000000     C* SE RITORNA PREMUTO F3 ESCE DAL PROGRAMMA, SE F12 SI FERMA
067500000000IF  1C     D87F03        IFEQ      '1'
067600000000     C     D87F12        OREQ      '1'
067700000000     C                   MOVEL     '1'           $ERR
067800000000IF  2C     D87F03        IFEQ      '1'
067900000000     C                   MOVEL     '1'           $F03
068000000000     C                   MOVEL     '1'           $FINE
068100000000E   2C                   ENDIF
068200000000     C*
068300000000     C* ALTRIMENTI MANDA MESSAGGIO RICEVUTO DAL PGM CHIAMATO
068400000000X   1C                   ELSE
068500000000     C                   MOVEL     D87ERR        $ERR
068600000000     C                   MOVEL     D87MSG        $MSG
068700000000     C                   EXSR      CARMSG
068800000000E   1C                   ENDIF
068900000000     C*
069000000000     C                   ENDSR
069100000000     C*------------------------------------------------------------------------*
069200000000     C* CANCEL - CANCELLA FISICAMENTE L'ERRORE DI CONTO ECONOMICO
069300000000     C*------------------------------------------------------------------------*
069400000000     C     CANCEL        BEGSR
069500000000     C*
069600000000     C* LEGGE IL RECORD SELEZIONATO
069700000103     C                   Z-ADD     VSHAAS        KE2AAS
069800970901     C                   Z-ADD     VSCLNP        KE2LNP
069900970901     C                   Z-ADD     VSCNRS        KE2NRS
070000970901     C                   Z-ADD     VSCNSP        KE2NSP
070100970901     C                   MOVEL     VSCTBL        KE2TBL
070200970902     C                   Z-ADD     VSHDAT        KE2DAT
070300970902     C                   Z-ADD     VSHTIM        KE2TIM
070400970902     C                   Z-ADD     VSHPRG        KE2PRG
070500970901     C     KE2ER2        CHAIN     ECERR02L                           97
070600000000IF  1C     *IN97         IFEQ      *OFF
070700970901     C                   DELETE    ECERR2
070800000000     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
070900000000     C                   MOVEL     'C'           VSCGIR
071000000000     C                   MOVEL     MSG(10)       $MSG
071100000000     C                   EXSR      CARMSG
071200000000E   1C                   ENDIF
071300000000     C*
071400000000     C                   ENDSR
071500000000     C*------------------------------------------------------------------------*
071600000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
071700000000     C*------------------------------------------------------------------------*
071800000000     C     NEWPOS        BEGSR
071900000000     C*
072000000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
072100000000     C*        1 = RICHIESTO POSIZIONAMENTO
072200000000     C                   Z-ADD     0             SWPOSI            7 0
072300000000     C*
072400000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
072500000000     C*            NON OCCORRE RICARICARE IL SUBFILE
072600000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
072700000000     C                   Z-ADD     0             SWMODI            1 0
072800000000     C*
072900000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO O NUOVA SELEZIONE
073000000000IF  1C     VCCDAT        IFNE      DEPDAT
073100000000     C     VCCAAS        ORNE      DEPAAS
073200000000     C     VCCLNP        ORNE      DEPLNP
073300000000     C     VCCNRS        ORNE      DEPNRS
073400000000     C     VCCNSP        ORNE      DEPNSP
073500000000     C     VCCTBL        ORNE      DEPTBL
073600000000     C* SELEZIONI
073700000000     C     VCCCER        ORNE      DEPCER
073800000000     C     VCCDSP        ORNE      DEPDSP
073900000000     C     VCCPGF        ORNE      DEPPGF
074000000000     C     VCCSTS        ORNE      DEPSTS
074100000000     C*
074200000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
074300000000     C                   EXSR      CHKPAR
074400000000     C*
074500000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
074600000000IF  2C     $ERR          IFEQ      '0'
074700000000     C                   Z-ADD     1             SWPOSI
074800000000     C                   Z-ADD     VCCDAT        DEPDAT
074900000000     C                   Z-ADD     VCCAAS        DEPAAS
075000000000     C                   Z-ADD     VCCLNP        DEPLNP
075100000000     C                   Z-ADD     VCCNRS        DEPNRS
075200000000     C                   Z-ADD     VCCNSP        DEPNSP
075300000000     C                   MOVEL     VCCTBL        DEPTBL
075400000000     C* SELEZIONI
075500000000     C                   MOVEL     VCCCER        DEPCER
075600000000     C                   Z-ADD     VCCDSP        DEPDSP
075700000000     C                   MOVEL     VCCPGF        DEPPGF
075800000000     C                   MOVEL     VCCSTS        DEPSTS
075900000000E   2C                   ENDIF
076000000000E   1C                   ENDIF
076100000000     C*
076200000000     C                   ENDSR
076300000000     C*------------------------------------------------------------------------*
076400000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
076500000000     C*------------------------------------------------------------------------*
076600000000     C     CHKPAR        BEGSR
076700000000     C*
076800000000     C* IMPOSTO INDICATORI EVIDENZAZIONE ERRORI SUI PARAMETRI
076900000000     C                   SETOFF                                       808182
077000000000     C                   SETOFF                                       838485
077100000000     C                   SETOFF                                       86
077200000000     C*
077300000000     C* IMPOSTO INDICATORI EVIDENZAZIONE ORDINAMENTO
077400000000     C                   SETOFF                                       5051
077500000000     C*
077600000000     C* PULISCO DATE INVERTITE
077700000000     C                   Z-ADD     *ZEROS        DATVCC
077800000000     C*--------------------
077900000000     C* ORDINAMENTO
078000000000     C*--------------------
078100000000     C* SE TUTTI I PARAMETRI SONO UGUALI A BLANK O ZERO, UTILIZZO CHIAVE "DAT"
078200000000IF  1C     VCCDAT        IFEQ      *ZEROS
078300000000     C     VCCTIM        ANDEQ     *ZEROS
078400000000     C     VCCAAS        ANDEQ     *ZEROS
078500000000     C     VCCLNP        ANDEQ     *ZEROS
078600000000     C     VCCNRS        ANDEQ     *ZEROS
078700000000     C     VCCNSP        ANDEQ     *ZEROS
078800000000     C     VCCTBL        ANDEQ     *BLANKS
078801040914     C     VCCDSP        ANDEQ     *zeros
078900000000     C                   MOVEL     'DAT'         WTIPRI            3
079000000000     C                   SETON                                        50
079100000000     C*
079200000000X   1C                   ELSE
079300000000     C*
079400000000     C* ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
079500000000IF  2C     VCCAAS        IFNE      *ZEROS
079600000000     C     VCCLNP        ORNE      *ZEROS
079700000000     C     VCCNRS        ORNE      *ZEROS
079800000000     C     VCCNSP        ORNE      *ZEROS
079900000000     C     VCCTBL        ORNE      *BLANKS
080000000000IF  3C     VCCDAT        IFNE      *ZEROS
080100000000     C     VCCTIM        ORNE      *ZEROS
080101040914     C     VCCdsp        ORNE      *ZEROS
080200000000     C                   MOVEL     MSG(7)        $MSG
080300000000     C                   EXSR      CARMSG
080400000000     C                   MOVEL     '1'           $ERR
080500000000     C                   SETON                                        8081
080600000000E   3C                   ENDIF
080700000000E   2C                   ENDIF
080800000000     C*
080900000000IF  2C     $ERR          IFEQ      '0'
081000000000     C*
081100000000     C* INIZIO ELENCO PER  "D A T A   E R R O R E"
081200000000IF  3C     VCCDAT        IFNE      *ZEROS
081300000000     C     VCCTIM        ORNE      *ZEROS
081400000000     C                   MOVEL     'DAT'         WTIPRI
081500000000     C                   SETON                                        50
081600000000     C* CONTROLLA ESATTEZZA
081700000000     C                   Z-ADD     VCCDAT        G08DAT
081800000000     C                   Z-ADD     *ZEROS        G08INV
081900000000     C                   MOVEL     '0'           G08ERR
082000000000     C                   CALL      'XSRDA8'
082100000000     C                   PARM                    WLBDA8
082200000000     C     G08ERR        IFEQ      '1'
082300000000     C                   MOVEL     '1'           $ERR
082400000000     C                   MOVEL     MSG(8)        $MSG
082500000000     C                   EXSR      CARMSG
082600000000     C                   SETON                                        80
082700000000     C                   ELSE
082800000000     C                   Z-ADD     G08DAT        VCCDAT
082900000000     C                   Z-ADD     G08INV        DATVCC            8 0
083000000000     C                   ENDIF
083100000000E   3C                   ENDIF
083200000000     C*
083300000000     C* INIZIO ELENCO PER  "B O L L A"
083400000000IF  3C     VCCAAS        IFNE      *ZEROS
083500000000     C     VCCLNP        ORNE      *ZEROS
083600000000     C     VCCNRS        ORNE      *ZEROS
083700000000     C     VCCNSP        ORNE      *ZEROS
083800000000     C     VCCTBL        ORNE      *BLANKS
083801040914     C     VCCdsp        ORNE      *zeros
083900000000     C                   MOVEL     'BOL'         WTIPRI
084000000000     C                   SETON                                        51
084100000000E   3C                   ENDIF
084200000000     C*
084300000000E   2C                   ENDIF
084400000000E   1C                   ENDIF
084500000000     C*--------------------
084600000000     C* SELEZIONE
084700000000     C*--------------------
084800000000     C                   Z-ADD     *ZEROS        DSPVCC
084900000000     C*
085000000000     C* DATA SPEDIZIONE
085100000000IF  1C     VCCDSP        IFNE      *ZEROS
085200000000     C                   Z-ADD     VCCDSP        G08DAT
085300000000     C                   Z-ADD     *ZEROS        G08INV
085400000000     C                   MOVEL     '0'           G08ERR
085500000000     C                   CALL      'XSRDA8'
085600000000     C                   PARM                    WLBDA8
085700000000IF  2C     G08ERR        IFEQ      '1'
085800000000     C                   MOVEL     '1'           $ERR
085900000000     C                   MOVEL     MSG(8)        $MSG
086000000000     C                   EXSR      CARMSG
086100000000     C                   SETON                                        83
086200000000X   2C                   ELSE
086300000000     C                   Z-ADD     G08DAT        VCCDAT
086400000000     C                   Z-ADD     G08INV        DSPVCC            8 0
086500000000E   2C                   ENDIF
086600000000E   1C                   ENDIF
086700000000     C*
086800000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
086900000000     C* CURSORE SUL CAMPO DEL CONTROL
087000000000     C     $ERR          IFEQ      '1'
087100000000     C                   MOVEL     '0'           *IN43
087200000000     C                   ELSE
087300000000     C                   MOVEL     '1'           *IN43
087400000000     C                   END
087500000000     C*
087600000000     C                   ENDSR
087700000000     C*------------------------------------------------------------------------*
087800000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
087900000000     C*------------------------------------------------------------------------*
088000000000     C     SETFIL        BEGSR
088100000000     C*
088200000000     C                   MOVEL     '1'           *IN43
088300000000     C                   SETOFF                                       98
088400000000     C*--------------------
088500000000     C* POSIZIONAMENTO X DATA ERRORE
088600000000     C*--------------------
088700000000IF  1C     WTIPRI        IFEQ      'DAT'
088800000000     C                   Z-ADD     DATVCC        KE1DAT
088900000000     C                   Z-ADD     VCCTIM        KE1TIM
089000000000     C                   Z-ADD     *ZEROS        KE1PRG
089100000000     C     KEYER1        SETGT     ECERR01L                           98
089200000000E   1C                   ENDIF
089300000000     C*--------------------
089400000000     C* POSIZIONAMENTO X BOLLA + CODICE MODIFICA
089500000000     C*--------------------
089600000000IF  1C     WTIPRI        IFEQ      'BOL'
089700000000     C                   Z-ADD     VCCAAS        KE3AAS
089800000000     C                   Z-ADD     VCCLNP        KE3LNP
089900000000     C                   Z-ADD     VCCNRS        KE3NRS
090000000000     C                   Z-ADD     VCCNSP        KE3NSP
090100000000     C                   Z-ADD     *ZEROS        KE3DAM
090200000000     C                   Z-ADD     *ZEROS        KE3ORM
090300000000     C                   Z-ADD     *ZEROS        KE3PRG
090400000000     C     KEYER3        SETGT     ECERR03L                           98
090500000000E   1C                   ENDIF
090600000000     C*
090700000000     C  N98              EXSR      LETFIL
090800000000     C*
090900000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
091000000000IF  1C     *IN98         IFEQ      '1'
091100000000     C                   Z-ADD     1             SFLEOF
091200000000E   1C                   ENDIF
091300000000     C*
091400000000     C                   ENDSR
091500000000     C*------------------------------------------------------------------------*
091600000000     C* LETFIL - LEGGE RECORD SUCESSIVO
091700000000     C*------------------------------------------------------------------------*
091800000000     C     LETFIL        BEGSR
091900000000     C*
092000000000     C* LEGGO FINO A:
092100000000     C*     - FINE FILE
092200000000     C*     - TROVATO RECORD VALIDO
092300000000     C                   MOVEL     '0'           WRECOK
092400000000DO  1C     *IN98         DOWEQ     *OFF
092500000000     C     WRECOK        ANDEQ     '0'
092600000000     C*--------------------
092700000000     C* LETTURA X DATA ERRORE
092800000000     C*--------------------
092900000000IF  1C     WTIPRI        IFEQ      'DAT'
093000000000     C                   READ      ECERR01L                               98
093100000000E   1C                   ENDIF
093200000000     C*--------------------
093300000000     C* LETTURA X BOLLA + CODICE MODIFICA
093400000000     C*--------------------
093500000000IF  1C     WTIPRI        IFEQ      'BOL'
093600000000     C                   READ      ECERR03L                               98
093700000000E   1C                   ENDIF
093800000000     C*
093900000000     C* FINE FILE  O TABELLA DIVERSA
094000000000IF  2C     *IN98         IFEQ      *ON
094100000000     C                   Z-ADD     1             SFLEOF
094200000000X   2C                   ELSE
094300000000     C                   EXSR      CHKREC
094400000000E   2C                   ENDIF
094500000000     C*
094600000000E   1C                   ENDDO
094700000000     C*
094800000000     C                   ENDSR
094900000000     C*------------------------------------------------------------------------*
095000000000     C* CHKREC - CONTROLLA VALIDITA' RECORD VOCE DI C/E
095100000000     C*------------------------------------------------------------------------*
095200000000     C     CHKREC        BEGSR
095300000000     C*
095400000000     C                   MOVEL     '1'           WRECOK
095500000000     C* CODICE ERRORE
095600000000IF  1C     VCCCER        IFNE      *BLANKS
095700000000IF  2C     ERRCER        IFNE      VCCCER
095800000000     C                   MOVEL     '0'           WRECOK
095900000000E   2C                   ENDIF
096000000000E   1C                   ENDIF
096100000000     C* DATA SPEDIZIONE
096200000000IF  1C     VCCDSP        IFNE      *ZEROS
096300040914IF  2C*    ERRDSP        IFNE      DSPVCC
096301040914IF  2C     ERRDSP        IFlt      DSPVCC
096400000000     C                   MOVEL     '0'           WRECOK
096500000000E   2C                   ENDIF
096600000000E   1C                   ENDIF
096700000000     C* PGM DI MODIFICA
096800000000IF  1C     VCCPGF        IFNE      *BLANKS
096900000000IF  2C     ERRPGF        IFNE      VCCPGF
097000000000     C                   MOVEL     '0'           WRECOK
097100000000E   2C                   ENDIF
097200000000E   1C                   ENDIF
097300000000     C* TIPO ERRORE
097400000000IF  1C     VCCSTS        IFNE      *BLANKS
097500000000IF  2C     ERRSTS        IFNE      VCCSTS
097600000000     C                   MOVEL     '0'           WRECOK
097700000000E   2C                   ENDIF
097800000000E   1C                   ENDIF
097900000000     C*
098000000000     C                   ENDSR
098100000000     C*------------------------------------------------------------------------*
098200000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
098300000000     C*------------------------------------------------------------------------*
098400000000     C     IRESFL        BEGSR
098500000000     C*
098600000000     C* CAMPI NASCOSTI
098700000103     C                   Z-ADD     ERRAAS        VSHAAS
098800000103     C                   Z-ADD     ERRDAT        VSHDAT
098900000000     C                   Z-ADD     ERRTIM        VSHTIM
099000000000     C                   Z-ADD     ERRDAM        VSHDAM
099100000000     C                   Z-ADD     ERRORM        VSHORM
099200000000     C                   Z-ADD     ERRPRG        VSHPRG
099300000000     C                   MOVEL     ERRFIL        VSHFIL
099400000107     C                   MOVEL     *BLANKS       VSCOPZ
099500000107     C*---                 MOVEL' 1'      VSCOPZ           *OPZIONE
099600000107     C*---                 SETON                     41
099700991223     C                   Z-ADD     ERRDAT        DSDAT8
099800991223     C                   Z-ADD     DSGIO8        DSGIO6
099900991223     C                   Z-ADD     DSMES8        DSMES6
100000991223     C                   Z-ADD     DSANN8        DSANN6
100100991223     C                   Z-ADD     DSDAT6        VSCDAT
100200000103     C                   Z-ADD     ERRAAS        VSCAAS
100300000000     C                   Z-ADD     ERRLNP        VSCLNP
100400000000     C                   Z-ADD     ERRNRS        VSCNRS
100500000000     C                   Z-ADD     ERRNSP        VSCNSP
100600000000     C                   MOVEL     ERRTBL        VSCTBL
100700991223     C                   Z-ADD     ERRDSP        DSDAT8
100800991223     C                   Z-ADD     DSGIO8        DSGIO6
100900991223     C                   Z-ADD     DSMES8        DSMES6
101000991223     C                   Z-ADD     DSANN8        DSANN6
101100991223     C                   Z-ADD     DSDAT6        VSCDSP
101200000000     C                   MOVEL     ERRSTS        VSCSTS
101300950508     C                   MOVEL     *BLANKS       VSCFIL
101400020619IF  1C     ERRFIL        IFEQ      'TITAS10F'
101500020619IF  1C     ERRFIL        OREQ      'TITAS00F'
101600000000     C                   MOVEL     'T'           VSCFIL
101700000000X   1C                   ELSE
101800000000IF  2C     ERRFIL        IFEQ      'TNCSB00F'
101900000000     C                   MOVEL     'C'           VSCFIL
102000000000E   2C                   ENDIF
102100030123IF  2C     ERRFIL        IFEQ      'FIAR500F'
102200030123     C                   MOVEL     'A'           VSCFIL
102300030123E   2C                   ENDIF
102400000000E   1C                   ENDIF
102500000000     C                   MOVE      ERRDER        A71              71
102600000000     C                   MOVEL     A71           VSCDER
102700000000     C                   MOVEL     *BLANKS       VSCGIR
102800000000     C*
102900000000     C* DEPOSITA NELLE CHIAVI DI CARICAMENTO SUBFILE L'ULTIMO RECORD CARICATO
103000000000IF  1C     WTIPRI        IFEQ      'DAT'
103100000000     C                   Z-ADD     ERRDAT        KE1DAT
103200000000     C                   Z-ADD     ERRTIM        KE1TIM
103300000000     C                   Z-ADD     ERRPRG        KE1PRG
103400000000X  1 C                   ELSE
103500000000     C                   Z-ADD     ERRAAS        KE3AAS
103600000000     C                   Z-ADD     ERRLNP        KE3LNP
103700000000     C                   Z-ADD     ERRNRS        KE3NRS
103800000000     C                   Z-ADD     ERRNSP        KE3NSP
103900000000     C                   Z-ADD     ERRDAM        KE3DAM
104000000000     C                   Z-ADD     ERRORM        KE3ORM
104100000000     C                   Z-ADD     ERRPRG        KE3PRG
104200000000E   1C                   ENDIF
104300000000     C*
104400000000     C                   ENDSR
104500000000     C*------------------------------------------------------------------------*
104600000000     C* GUFO - LANCIA IL PGM CHE RIMETTE LA BOLLA IL CODA PER ESSERE ELABORATA
104700000000     C*------------------------------------------------------------------------*
104800000000     C     GUFO          BEGSR
104900000000     C*
105000000000     C* LANCIA IL PROGRAMMA DI ACCODAMENTO
105100000000     C                   MOVEL     ERRSTS        D£GSTS
105200000000     C                   Z-ADD     ERRDAM        D£GDAT
105300000000     C                   Z-ADD     ERRORM        D£GTIM
105400000000     C                   MOVEL     ERRUSR        D£GUSR
105500000000     C                   MOVEL     ERRPGF        D£GPGF
105600000000     C                   MOVEL     ERRFIL        D£GFIL
105700000000     C                   MOVEL     ERRMBR        D£GMBR
105800000000     C                   MOVEL     ERRLIB        D£GLIB
105900000000     C                   MOVEL     'S'           D£GPDE
106000020731     C                   MOVEL     'I'           D£GTJB
106100000000     C                   MOVEL     *BLANKS       D£GXXX
106200000000     C                   MOVEL     ERRRP1        D£PRE1
106300000000     C                   MOVEL     ERRRP2        D£PRE2
106400000000     C                   MOVEL     ERRRD1        D£DRE1
106500000000     C                   MOVEL     ERRRD2        D£DRE2
106600000000     C*
106700020618     C                   CALL      'TRECD2R'
106800000000     C                   PARM                    DS£ECO
106900000000     C*
107000000000     C                   ENDSR
107100020923     C*-------------------------------------------------------------------------
107200020923     C* REPERISCE I DATI UTENTE
107300020923     C*-------------------------------------------------------------------------
107400020923     C     REPDATIUTE    BEGSR
107500020923     C*
107600020923     C* INIZIALIZZA VARIABILI DI WRK
107700020923     C                   CLEAR                   TIBS34DS
107800020923     C                   CLEAR                   AZUTEDS
107900020923     C                   CLEAR                   DDATIUTE
108000020923     C*
108100020923     C     *DTAARA       DEFINE    §azute        azuteds
108200020923     C     *DTAARA       DEFINE    §datiute      ddatiute
108300020923     C                   IN(E)     *DTAARA
108400020923if  1C                   IF        %Error
108500020923     c                   EVAL      I34Tla = 'L'
108600020923     C                   CALL      'TIBS34R'
108700020923     C                   PARM                    Tibs34Ds
108800020923     C                   IN        *DTAARA
108900020923e   1C                   ENDIF
109000020923     C*
109100020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
109200020923     C                   MOVEL(P)  RSUT          DSFIRS           20
109300020923     C*
109400020923     C                   ENDSR
109500000000     C*------------------------------------------------------------------------*
109600000000     C*  *INZSR - ROUTINE INIZIALE
109700000000     C*------------------------------------------------------------------------*
109800000000     C     *INZSR        BEGSR
109900000000     C*--------------------
110000000000     C* RICEVIMENTO PARAMETRI
110100000000     C*--------------------
110200000000     C     *ENTRY        PLIST
110300000000     C                   PARM                    KPJBA
110400000000     C*--------------------
110500000000     C* CAMPI RIFERITI AL DATA BASE
110600000000     C*--------------------
110700000000     C     *LIKE         DEFINE    ERRDAT        KE1DAT
110800000000     C     *LIKE         DEFINE    ERRTIM        KE1TIM
110900950329     C     *LIKE         DEFINE    ERRPRG        KE1PRG
111000000000     C     *LIKE         DEFINE    ERRAAS        KE2AAS
111100000000     C     *LIKE         DEFINE    ERRLNP        KE2LNP
111200000000     C     *LIKE         DEFINE    ERRNRS        KE2NRS
111300000000     C     *LIKE         DEFINE    ERRNSP        KE2NSP
111400000000     C     *LIKE         DEFINE    ERRTBL        KE2TBL
111500970902     C     *LIKE         DEFINE    ERRDAT        KE2DAT
111600970902     C     *LIKE         DEFINE    ERRTIM        KE2TIM
111700970902     C     *LIKE         DEFINE    ERRPRG        KE2PRG
111800000000     C     *LIKE         DEFINE    ERRAAS        KE3AAS
111900000000     C     *LIKE         DEFINE    ERRLNP        KE3LNP
112000000000     C     *LIKE         DEFINE    ERRNRS        KE3NRS
112100000000     C     *LIKE         DEFINE    ERRNSP        KE3NSP
112200000000     C     *LIKE         DEFINE    ERRDAM        KE3DAM
112300000000     C     *LIKE         DEFINE    ERRORM        KE3ORM
112400000000     C     *LIKE         DEFINE    ERRPRG        KE3PRG
112500990812     C     *LIKE         DEFINE    TASAAS        KTAAAS
112600000000     C     *LIKE         DEFINE    TASLNP        KTALNP
112700000000     C     *LIKE         DEFINE    TASNRS        KTANRS
112800000000     C     *LIKE         DEFINE    TASNSP        KTANSP
112900000000     C     *LIKE         DEFINE    TASTBL        KTATBL
113000000000     C     *LIKE         DEFINE    ERRAAS        KW3AAS
113100000000     C     *LIKE         DEFINE    ERRLNP        KW3LNP
113200000000     C     *LIKE         DEFINE    ERRNRS        KW3NRS
113300000000     C     *LIKE         DEFINE    ERRNSP        KW3NSP
113400000000     C*
113500000000     C     *LIKE         DEFINE    VCCDAT        DEPDAT
113600000000     C     *LIKE         DEFINE    VCCAAS        DEPAAS
113700000000     C     *LIKE         DEFINE    VCCLNP        DEPLNP
113800000000     C     *LIKE         DEFINE    VCCNRS        DEPNRS
113900000000     C     *LIKE         DEFINE    VCCNSP        DEPNSP
114000000000     C     *LIKE         DEFINE    VCCTBL        DEPTBL
114100000000     C     *LIKE         DEFINE    VCCCER        DEPCER
114200000000     C     *LIKE         DEFINE    VCCDSP        DEPDSP
114300000000     C     *LIKE         DEFINE    VCCPGF        DEPPGF
114400000000     C     *LIKE         DEFINE    VCCSTS        DEPSTS
114500000000     C*
114600000000     C     *LIKE         DEFINE    D87OP0        WOP0
114700000000     C     *LIKE         DEFINE    D87OP1        WOP1
114800000000     C     *LIKE         DEFINE    ERRAAS        WAAS
114900000000     C     *LIKE         DEFINE    ERRLNP        WLNP
115000000000     C     *LIKE         DEFINE    ERRNRS        WNRS
115100000000     C     *LIKE         DEFINE    ERRNSP        WNSP
115200000000     C     *LIKE         DEFINE    ERRTBL        WTBL
115300000000     C     *LIKE         DEFINE    ERRDAT        WDAT
115400000000     C     *LIKE         DEFINE    ERRTIM        WTIM
115500000000     C     *LIKE         DEFINE    ERRPRG        WPRG
115600000000     C     *LIKE         DEFINE    ERRDAM        WDAM
115700000000     C     *LIKE         DEFINE    ERRORM        WORM
115800000000     C*--------------------
115900000000     C* CHIAVI DI LETTURA
116000000000     C*--------------------
116100000000     C* ECERR01L - PARZIALE
116200000000     C     KEYER1        KLIST
116300000000     C                   KFLD                    KE1DAT
116400000000     C                   KFLD                    KE1TIM
116500000000     C                   KFLD                    KE1PRG
116600000000     C* ECERR02L - COMPLETA
116700970901     C     KE2ER2        KLIST
116800000000     C                   KFLD                    KE2AAS
116900000000     C                   KFLD                    KE2LNP
117000000000     C                   KFLD                    KE2NRS
117100000000     C                   KFLD                    KE2NSP
117200000000     C                   KFLD                    KE2TBL
117300970902     C                   KFLD                    KE2DAT
117400970902     C                   KFLD                    KE2TIM
117500970902     C                   KFLD                    KE2PRG
117600970901     C* ECERR02L - PARZIALE
117700970901     C     KEYER2        KLIST
117800970901     C                   KFLD                    KE2AAS
117900970901     C                   KFLD                    KE2LNP
118000970901     C                   KFLD                    KE2NRS
118100970901     C                   KFLD                    KE2NSP
118200970901     C                   KFLD                    KE2TBL
118300000000     C* ECERR03L - COMPLETA
118400000000     C     KEYER3        KLIST
118500000000     C                   KFLD                    KE3AAS
118600000000     C                   KFLD                    KE3LNP
118700000000     C                   KFLD                    KE3NRS
118800000000     C                   KFLD                    KE3NSP
118900000000     C                   KFLD                    KE3DAM
119000000000     C                   KFLD                    KE3ORM
119100000000     C                   KFLD                    KE3PRG
119200000000     C* ECERR03L - PARZIALE (DI LAVORO)
119300000000     C     KEYEW3        KLIST
119400000000     C                   KFLD                    KW3AAS
119500000000     C                   KFLD                    KW3LNP
119600000000     C                   KFLD                    KW3NRS
119700000000     C                   KFLD                    KW3NSP
119800990812     C* TITAS30C - COMPLETA
119900000000     C     KEYTAS        KLIST
120000000000     C                   KFLD                    KTAAAS
120100000000     C                   KFLD                    KTALNP
120200000000     C                   KFLD                    KTANRS
120300000000     C                   KFLD                    KTANSP
120400000000     C                   KFLD                    KTATBL
120500000000     C*--------------------
120600000000     C* CAMPI DI CONTROLLO
120700000000     C*--------------------
120800000000     C                   MOVEL     'G'           $TGES             1
120900000000     C                   MOVEL     '1'           *INOF
121000000000     C                   Z-ADD     UDATE         DATA              6 0
121100000000     C                   TIME                    ORA               6 0
121200000000     C                   MOVEL     '0'           WRECOK            1
121300000000     C                   MOVEL     '1'           $PROS             1
121400000000     C*
121500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
121600000000     C                   EXSR      INZSTD
121700000000     C*
121800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
121900000000     C                   EXSR      INZSFL
122000000000     C*
122100000000     C* IMPOSTO CAMPI TESTATA VIDEO
122200000000     C                   MOVEL     DSFIRS        $RSU
122300020923     C                   Z-ADD     DUTPOU        $FIL
122400000000     C*
122500000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
122600000000     C                   Z-ADD     *ZEROS        VCCDAT
122700000000     C                   Z-ADD     *ZEROS        VCCAAS
122800000000     C                   Z-ADD     *ZEROS        VCCLNP
122900000000     C                   Z-ADD     *ZEROS        VCCNRS
123000000000     C                   Z-ADD     *ZEROS        VCCNSP
123100000000     C                   MOVEL     *BLANKS       VCCTBL
123200000000     C*
123300000000     C* EMETTO VIDEATA SUBFILE
123400000000     C                   EXSR      CHKPAR
123500000000     C                   EXSR      CARVS
123600000000     C*
123700000000     C                   ENDSR
123800000000     C*------------------------------------------------------------------------*
123900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
124000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
124100000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
124200000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
124300000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
124400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
124500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
124600000000     C*------------------------------------------------------------------------*
124700000000     C* INZSTD - ROUTINE INIZIALE STANDARD
124800000000     C*------------------------------------------------------------------------*
124900000000     C     INZSTD        BEGSR
125000000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
125100000000     C                   CALL      'XKEYS'
125200000000     C                   PARM                    DSKEYS
125300000000     C                   MOVEA     KEYRIG        KEYS
125400000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
125500000000     C     XMSGPA        PLIST
125600000000     C                   PARM                    DSMSMS
125700000000     C                   PARM                    DSMSPG
125800000000     C                   PARM                    DSMSKE
125900000000     C                   PARM                    DSMSER
126000000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
126100000000     C                   MOVEL     'TREC87R'     DSMSPG
126200000000     C                   MOVEL     'TREC87R'     VMCQUE
126300000000     C                   MOVEL     *BLANKS       VMCKEY
126400000000     C                   MOVEL     *BLANKS       DSMSMS
126500000000     C* INIZIALIZZO FLAG CONTROLLO PGM
126600000000     C                   MOVEL     '0'           $ERR              1
126700000000     C                   MOVEL     '0'           $F03              1
126800000000     C                   MOVEL     *BLANKS       $GES              2
126900000000     C                   MOVEL     *BLANKS       $MSG             80
127000000000     C                   MOVEL     '0'           $FINE             1
127100000000     C                   MOVEL     '0'           WPREC             1
127200000000     C                   MOVEL     '0'           $TESVD            1
127300000000     C* LANCIO ROUTINE PER VERIFICARE SE
127400000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
127500000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
127600020923     C                   EXSR      REPDATIUTE
127700000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
127800000000     C                   BITOFF    '01234567'    POSMI1            1
127900000000     C                   BITOFF    '01234567'    POSMI2            1
128000000000     C                   BITOFF    '01234567'    POSMA1            1
128100000000     C                   BITOFF    '01234567'    POSMA2            1
128200000000     C                   BITON     '34'          POSMI1            1
128300000000     C                   BITON     '7'           POSMI2            1
128400000000     C                   BITON     '34'          POSMA1            1
128500000000     C                   BITON     '0'           POSMA2            1
128600000000     C                   MOVE      POSMI2        POSMIN            2
128700000000     C                   MOVEL     POSMI1        POSMIN
128800000000     C                   MOVE      POSMA2        POSMAX            2
128900000000     C                   MOVEL     POSMA1        POSMAX
129000000000     C                   ENDSR
129100000000     C*------------------------------------------------------------------------*
129200000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
129300000000     C*------------------------------------------------------------------------*
129400000000     C     FUNSTD        BEGSR
129500000000     C                   SELECT
129600000000     C     KEY           WHENEQ    F3
129700000000     C                   EXSR      F03STD
129800000000     C     KEY           WHENEQ    F12
129900000000     C                   EXSR      F12STD
130000000000     C     KEY           WHENEQ    F24
130100000000     C                   EXSR      F24STD
130200000000     C                   OTHER
130300000000     C                   MOVEL     MSG(2)        $MSG
130400000000     C                   EXSR      CARMSG
130500000000     C                   ENDSL
130600000000     C                   ENDSR
130700000000     C*------------------------------------------------------------------------*
130800000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
130900000000     C*------------------------------------------------------------------------*
131000000000     C     F03STD        BEGSR
131100000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
131200000000     C                   MOVEL     '1'           $F03
131300000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
131400000000     C                   MOVEL     '1'           $FINE
131500000000     C                   ENDSR
131600000000     C*------------------------------------------------------------------------*
131700000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
131800000000     C*------------------------------------------------------------------------*
131900000000     C     F12STD        BEGSR
132000000000     C* IMPOSTO FLAG DI FIN PGM
132100000000     C                   MOVEL     '1'           $FINE
132200000000     C                   ENDSR
132300000000     C*------------------------------------------------------------------------*
132400000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
132500000000     C*------------------------------------------------------------------------*
132600000000     C     F24STD        BEGSR
132700000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
132800000000     C                   EXSR      OUTFUN
132900000000     C                   ENDSR
133000000000     C*------------------------------------------------------------------------*
133100000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
133200000000     C*------------------------------------------------------------------------*
133300000000     C     INZSFL        BEGSR
133400000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
133500000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
133600000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
133700000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
133800000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
133900000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
134000000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
134100000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
134200000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
134300000000     C                   Z-ADD     12            SFLPAG
134400000000     C                   ENDSR
134500000000     C*------------------------------------------------------------------------*
134600000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
134700000000     C*------------------------------------------------------------------------*
134800000000     C     FUNSFL        BEGSR
134900000000     C                   SELECT
135000000000     C     KEY           WHENEQ    ENTER
135100000000     C                   EXSR      ENTSFL
135200000000     C     KEY           WHENEQ    F3
135300000000     C                   EXSR      F03STD
135400000000     C     KEY           WHENEQ    F5
135500000000     C                   EXSR      F05STD
135600000000     C     KEY           WHENEQ    F12
135700000000     C                   EXSR      F12STD
135800000000     C     KEY           WHENEQ    F17
135900000000     C                   EXSR      F17STD
136000000000     C     KEY           WHENEQ    F18
136100000000     C                   EXSR      F18STD
136200000000     C     KEY           WHENEQ    F23
136300000000     C                   EXSR      F23STD
136400000000     C     KEY           WHENEQ    F24
136500000000     C                   EXSR      F24STD
136600000000     C     KEY           WHENEQ    ROLLUP
136700000000     C                   EXSR      RUPSTD
136800000000     C     KEY           WHENEQ    ROLLDN
136900000000     C                   EXSR      RDNSTD
137000000000     C                   OTHER
137100000000     C                   MOVEL     MSG(2)        $MSG
137200000000     C                   EXSR      CARMSG
137300000000     C                   ENDSL
137400000000     C                   ENDSR
137500000000     C*------------------------------------------------------------------------*
137600000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
137700000000     C*------------------------------------------------------------------------*
137800000000     C     F05STD        BEGSR
137900000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
138000000000     C                   EXSR      POSSFL
138100000000     C                   ENDSR
138200000000     C*------------------------------------------------------------------------*
138300000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
138400000000     C*------------------------------------------------------------------------*
138500000000     C     F17STD        BEGSR
138600000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
138700000000     C                   Z-ADD     1             VCCNBR
138800000000     C                   ENDSR
138900000000     C*------------------------------------------------------------------------*
139000000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
139100000000     C*------------------------------------------------------------------------*
139200000000     C     F18STD        BEGSR
139300000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
139400000000     C                   Z-ADD     SFLULT        VCCNBR
139500000000     C                   ENDSR
139600000000     C*------------------------------------------------------------------------*
139700000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
139800000000     C*------------------------------------------------------------------------*
139900000000     C     F23STD        BEGSR
140000000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
140100000000     C                   EXSR      OUTOPZ
140200000000     C                   ENDSR
140300000000     C*------------------------------------------------------------------------*
140400000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
140500000000     C*------------------------------------------------------------------------*
140600000000     C     RUPSTD        BEGSR
140700000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
140800000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
140900000000     C     CURSOR        IFGE      POSMIN
141000000000     C     CURSOR        ANDLE     POSMAX
141100000000     C                   ELSE
141200000000     C                   EXSR      CARSFL
141300000000     C                   ENDIF
141400000000     C                   ENDSR
141500000000     C*------------------------------------------------------------------------*
141600000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
141700000000     C*------------------------------------------------------------------------*
141800000000     C     RDNSTD        BEGSR
141900000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
142000000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
142100000000     C     CURSOR        IFGE      POSMIN
142200000000     C     CURSOR        ANDLE     POSMAX
142300000000     C                   ELSE
142400000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
142500000000     C                   MOVEL     '1'           $ERR
142600000000     C                   MOVEL     MSG(5)        $MSG
142700000000     C                   EXSR      CARMSG
142800000000     C                   ENDIF
142900000000     C                   ENDSR
143000000000     C*------------------------------------------------------------------------*
143100000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
143200000000     C*------------------------------------------------------------------------*
143300000000     C     ENTSFL        BEGSR
143400000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
143500000000     C                   EXSR      NEWPOS
143600000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
143700000000     C     $ERR          IFEQ      '0'
143800000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
143900000000     C     SWPOSI        IFNE      0
144000000000     C                   EXSR      POSSFL
144100000000     C                   ELSE
144200000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
144300000000     C     SFLULT        IFNE      0
144400000000     C                   EXSR      OPZSFL
144500000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
144600000000     C     SWMODI        IFNE      0
144700000000     C                   EXSR      POSSFL
144800000000     C                   ENDIF
144900000000     C                   ENDIF
145000000000     C                   ENDIF
145100000000     C                   ENDIF
145200000000     C                   ENDSR
145300000000     C*------------------------------------------------------------------------*
145400000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
145500000000     C*------------------------------------------------------------------------*
145600000000     C     POSSFL        BEGSR
145700000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
145800000000     C                   EXSR      CLRSFL
145900000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
146000000000     C                   EXSR      SETFIL
146100000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
146200000000     C     SFLEOF        IFEQ      0
146300000000     C                   EXSR      CARSFL
146400000000     C                   ENDIF
146500000000     C                   ENDSR
146600000000     C*------------------------------------------------------------------------*
146700000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
146800000000     C*------------------------------------------------------------------------*
146900000000     C     OPZSFL        BEGSR
147000000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
147100000000     C                   MOVEL     'S'           OPZOK             1
147200000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
147300000000     C                   Z-ADD     0             SFLVAR            5 0
147400000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
147500000000     C                   Z-ADD     0             SFLERR
147600000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
147700000000     C                   Z-ADD     SFLULT        WFLULT
147800000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
147900000000     C                   READC     EC87DVS                                49
148000000000     C     *IN49         DOWEQ     '0'
148100000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
148200000000     C                   ADD       1             SFLVAR
148300000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
148400000000     C     OPZOK         IFEQ      'S'
148500000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
148600000000     C     VSCOPZ        IFNE      *BLANKS
148700000000     C                   EXSR      CONOPZ
148800000000     C     OPZOK         IFEQ      'S'
148900000000     C                   EXSR      GESOPZ
149000000000     C                   ENDIF
149100000000     C                   ENDIF
149200000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
149300000000     C*            2. PULIZIA SCELTA IN SFL
149400000000     C     OPZOK         IFEQ      'S'
149500000000     C                   MOVEL     '0'           *IN41
149600000000     C                   MOVEL     *BLANKS       VSCOPZ
149700000000     C                   ELSE
149800000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
149900000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
150000000000     C                   MOVEL     '1'           *IN41
150100000000     C                   Z-ADD     SFLULT        SFLERR
150200000000     C                   ENDIF
150300000000     C                   ELSE
150400000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
150500000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
150600000000     C                   MOVEL     '1'           *IN41
150700000000     C                   ENDIF
150800000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
150900000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
151000000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
151100000000     C     OPZOK         IFNE      'S'
151200000000     C                   Z-ADD     SFLERR        VCCNBR
151300000000     C                   ELSE
151400000000     C                   Z-ADD     SFLULT        VCCNBR
151500000000     C                   ENDIF
151600000000     C* AGGIORNO RECORD SFL
151700000000     C                   UPDATE    EC87DVS
151800000000     C                   READC     EC87DVS                                49
151900000000     C                   ENDDO
152000000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
152100000000     C                   Z-ADD     WFLULT        SFLULT
152200000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
152300000000     C     SFLVAR        IFEQ      0
152400000000     C                   MOVEL     '1'           $FINE
152500000000     C                   ENDIF
152600000000     C                   ENDSR
152700000000     C*------------------------------------------------------------------------*
152800000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
152900000000     C*------------------------------------------------------------------------*
153000000000     C     CLRSFL        BEGSR
153100000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
153200000000     C* (0=NON FINE FILE  1=FINE FILE)
153300000000     C                   Z-ADD     0             SFLEOF
153400000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
153500000000     C                   Z-ADD     0             SFLULT
153600000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
153700000000     C                   MOVEL     '1'           *IN40
153800000000     C                   WRITE     EC87DVC
153900000000     C                   MOVEL     '0'           *IN40
154000000000     C                   ENDSR
154100000000     C*------------------------------------------------------------------------*
154200000000     C* CARSFL - CARICAMENTO SFL
154300000000     C*------------------------------------------------------------------------*
154400000000     C     CARSFL        BEGSR
154500000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
154600000000     C     SFLULT        IFEQ      0
154700000000     C                   EXSR      SETFIL
154800000000     C                   ENDIF
154900000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
155000000000     C                   Z-ADD     SFLULT        WFLULT
155100000000     C* CICLO:
155200000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
155300000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
155400000000     C                   Z-ADD     1             SFLRIG
155500970901     C     SFLRIG        DOWLE     SFLPAG
155600970901     C     SFLEOF        ANDEQ     0
155700000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
155800000000     C                   MOVEL     *BLANKS       VSCOPZ
155900000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
156000000000     C                   EXSR      IRESFL
156100000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
156200000000     C                   ADD       1             SFLULT
156300000000     C* SCRITTURA RECORD SFL
156400000000     C                   WRITE     EC87DVS
156500000000     C                   ADD       1             SFLRIG
156600000000     C* LETTURA FILE DA CARICARE
156700000000     C                   EXSR      LETFIL
156800000000     C                   ENDDO
156900000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
157000000000     C     WFLULT        IFNE      SFLULT
157100000000     C                   Z-ADD     WFLULT        VCCNBR
157200000000     C                   ADD       1             VCCNBR
157300000000     C                   ENDIF
157400000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
157500000000     C     SFLEOF        IFEQ      1
157600000000     C     SFLULT        ANDGT     *ZEROS
157700000000     C                   MOVEL     '1'           $ERR
157800000000     C                   MOVEL     MSG(6)        $MSG
157900000000     C                   EXSR      CARMSG
158000000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
158100000000     C     SFLULT        CHAIN     EC87DVS                            99
158200000000     C     *IN99         IFEQ      *ON
158300000000     C                   SETON                                        45
158400000000     C                   UPDATE    EC87DVS
158500000000     C                   SETOFF                                       45
158600000000     C                   ENDIF
158700000000     C                   ENDIF
158800000000     C                   ENDSR
158900000000     C*------------------------------------------------------------------------*
159000000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
159100000000     C*------------------------------------------------------------------------*
159200000000     C     OUTSFL        BEGSR
159300000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
159400000000     C     MSGULT        IFNE      *ZEROS
159500000000     C                   EXSR      OUTMSG
159600000000     C                   ENDIF
159700000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
159800000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
159900000000     C     SFLEOF        IFEQ      0
160000000000     C                   MOVEL     '0'           *IN42
160100000000     C                   ELSE
160200000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
160300000000     C                   MOVEL     '1'           *IN42
160400000000     C                   ENDIF
160500000000     C* EMISSIONE SUBFILE
160600000000     C     SFLULT        IFLE      0
160700000000     C*  --> CASO DI SFL VUOTO
160800000000     C                   MOVEL     '0'           *IN43
160900000000     C                   WRITE     EC87DVV
161000000000     C                   WRITE     EC87DVC
161100000000     C                   READ      EC87DVC                                99
161200000000     C                   ELSE
161300000000     C*  --> CASO DI SFL CON RECORDS
161400000000     C                   WRITE     EC87DVC
161500000000     C                   READ      EC87DVC                                99
161600000000     C                   ENDIF
161700000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
161800000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
161900000000     C                   Z-ADD     FRTSFL        VCCNBR
162000000000     C                   ENDSR
162100000000     C*------------------------------------------------------------------------*
162200000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
162300000000     C*          CODA MESSAGGI DEL PGM
162400000000     C*------------------------------------------------------------------------*
162500000000     C     CARMSG        BEGSR
162600000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
162700000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
162800000000     C     MSGULT        IFEQ      0
162900000000     C                   MOVEL     '1'           *IN40
163000000000     C                   WRITE     EC87DVK
163100000000     C                   MOVEL     '0'           *IN40
163200000000     C                   ENDIF
163300000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
163400000000     C                   MOVEL     $MSG          DSMSMS
163500000000     C                   CALL      'XMSG'        XMSGPA
163600000000     C     DSMSER        IFEQ      0
163700000000     C                   MOVEL     DSMSKE        VMCKEY
163800000000     C                   ENDIF
163900000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
164000000000     C                   ADD       1             MSGULT            4 0
164100000000     C                   WRITE     EC87DVM
164200000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
164300000000     C                   MOVEL     *BLANKS       $MSG
164400000000     C                   ENDSR
164500000000     C*------------------------------------------------------------------------*
164600000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
164700000000     C*------------------------------------------------------------------------*
164800000000     C     OUTMSG        BEGSR
164900000000     C* VISUALIZZAZIONE MESSAGGIO
165000000000     C                   MOVEL     '1'           *IN44
165100000000     C                   WRITE     EC87DVK
165200000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
165300000000     C                   Z-ADD     *ZEROS        MSGULT
165400000000     C                   ENDSR
165500000000     C*------------------------------------------------------------------------*
165600000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
165700000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
165800000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
165900000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
166000000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
166100000000     C*------------------------------------------------------------------------*
166200000000     C     CARFUN        BEGSR
166300000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
166400000000     C     1             DO        31            I                 5 0
166500000000     C                   MOVEL     FUN(I)        FUNRIG
166600000000     C                   MOVEL     FUNFUN        FFU(I)
166700000000     C                   MOVEL     FUNDES        FDE(I)
166800020923     C     DUTLPO        IFEQ      'S'
166900000000     C     $TGES         IFEQ      'G'
167000000000     C                   MOVEL     FUNASG        FAT(I)
167100000000     C                   ELSE
167200000000     C                   MOVEL     FUNASI        FAT(I)
167300000000     C                   ENDIF
167400000000     C                   ELSE
167500000000     C     $TGES         IFEQ      'G'
167600000000     C                   MOVEL     FUNAFG        FAT(I)
167700000000     C                   ELSE
167800000000     C                   MOVEL     FUNAFI        FAT(I)
167900000000     C                   ENDIF
168000000000     C                   ENDIF
168100000000     C     FAT(I)        IFEQ      'E'
168200000000     C                   MOVEL     $USER         WEDP              3
168300000000     C     WEDP          IFEQ      'EDP'
168400000000     C                   MOVEL     'S'           FAT(I)
168500000000     C                   ELSE
168600000000     C                   MOVEL     'N'           FAT(I)
168700000000     C                   ENDIF
168800000000     C                   ENDIF
168900000000     C                   ENDDO
169000000000     C                   ENDSR
169100000000     C*------------------------------------------------------------------------*
169200000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
169300000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
169400000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
169500000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
169600000000     C*------------------------------------------------------------------------*
169700000000     C     OUTFUN        BEGSR
169800000000     C     1             DO        8             I
169900000000     C                   MOVEL     *BLANKS       FUV(I)
170000000000     C                   ENDDO
170100000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
170200000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
170300000000     C     1             DO        31            I
170400000000     C     FAT(I)        IFEQ      'S'
170500000000     C     FDE(I)        ANDNE     *BLANKS
170600000000     C                   ADD       1             FUNNAT
170700000000     C                   ENDIF
170800000000     C                   ENDDO
170900000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
171000000000     C     FUNNAT        IFGT      8
171100000000     C                   MOVEL     'S'           FAT(24)
171200000000     C                   ENDIF
171300000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
171400000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
171500000000     C                   Z-ADD     8             FUNVIS            5 0
171600000000     C     FAT(23)       IFEQ      'S'
171700000000     C                   SUB       1             FUNVIS
171800000000     C                   ENDIF
171900000000     C     FAT(24)       IFEQ      'S'
172000000000     C                   SUB       1             FUNVIS
172100000000     C                   ENDIF
172200000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
172300000000     C                   Z-ADD     *ZEROS        W                 5 0
172400000000     C                   ADD       1             FUNULT
172500000000     C     FUNULT        IFGT      31
172600000000     C                   Z-ADD     1             FUNULT
172700000000     C                   ENDIF
172800000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
172900000000     C     FUNULT        DO        31            I
173000000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
173100000000     C     W             IFLT      FUNVIS
173200000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
173300000000     C     FAT(I)        IFEQ      'S'
173400000000     C     FDE(I)        ANDNE     *BLANKS
173500000000     C     I             IFEQ      23
173600000000     C     I             OREQ      24
173700000000     C                   ELSE
173800000000     C                   ADD       1             W
173900000000     C                   MOVEL     FDE(I)        FUV(W)
174000000000     C                   ENDIF
174100000000     C                   ENDIF
174200000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
174300000000     C                   Z-ADD     I             FUNULT
174400000000     C                   ENDIF
174500000000     C                   ENDDO
174600000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
174700000000     C                   MOVEL     FUV(1)        VFD001
174800000000     C                   MOVEL     FUV(2)        VFD002
174900000000     C                   MOVEL     FUV(3)        VFD003
175000000000     C                   MOVEL     FUV(4)        VFD004
175100000000     C                   MOVEL     FUV(5)        VFD005
175200000000     C                   MOVEL     FUV(6)        VFD006
175300000000     C     FAT(23)       IFEQ      'N'
175400000000     C     FAT(24)       ANDEQ     'N'
175500000000     C                   MOVEL     FUV(7)        VFD007
175600000000     C                   MOVEL     FUV(8)        VFD008
175700000000     C                   ELSE
175800000000     C     FAT(23)       IFEQ      'S'
175900000000     C     FAT(24)       ANDEQ     'S'
176000000000     C                   MOVEL     FDE(23)       VFD007
176100000000     C                   MOVEL     FDE(24)       VFD008
176200000000     C                   ELSE
176300000000     C                   MOVEL     FUV(7)        VFD007
176400000000     C     FAT(23)       IFEQ      'S'
176500000000     C                   MOVEL     FDE(23)       VFD008
176600000000     C                   ELSE
176700000000     C                   MOVEL     FDE(24)       VFD008
176800000000     C                   ENDIF
176900000000     C                   ENDIF
177000000000     C                   ENDIF
177100000000     C                   MOVEL     '1'           $TESVD
177200000000     C                   ENDSR
177300000000     C*------------------------------------------------------------------------*
177400000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
177500000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
177600000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
177700000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
177800000000     C*------------------------------------------------------------------------*
177900000000     C     CONFUN        BEGSR
178000000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
178100000000     C     1             DO        31            I
178200000000     C     KEY           IFEQ      KEYS(I)
178300000000     C* CONTROLLO SE IL TASTO E' ATTIVO
178400000000     C     FAT(I)        IFNE      'S'
178500000000     C                   MOVEL     MSG(1)        $MSG
178600000000     C                   EXSR      CARMSG
178700000000     C                   MOVEL     '1'           $ERR
178800000000     C                   ENDIF
178900000000     C                   ENDIF
179000000000     C                   ENDDO
179100000000     C                   ENDSR
179200000000     C*------------------------------------------------------------------------*
179300000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
179400000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
179500000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
179600000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
179700000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
179800000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
179900000000     C*------------------------------------------------------------------------*
180000000000     C     CAROPZ        BEGSR
180100000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
180200000000     C     1             DO        24            I
180300000000     C                   MOVEL     OPZ(I)        OPZRIG
180400000000     C                   MOVEL     OPZOPD        OPD(I)
180500000000     C                   MOVEL     OPZOPS        OPS(I)
180600000000     C                   MOVEL     OPZDES        ODE(I)
180700020923     C     DUTLPO        IFEQ      'S'
180800000000     C     $TGES         IFEQ      'G'
180900000000     C                   MOVEL     OPZASG        OAT(I)
181000000000     C                   ELSE
181100000000     C                   MOVEL     OPZASI        OAT(I)
181200000000     C                   ENDIF
181300000000     C                   ELSE
181400000000     C     $TGES         IFEQ      'G'
181500000000     C                   MOVEL     OPZAFG        OAT(I)
181600000000     C                   ELSE
181700000000     C                   MOVEL     OPZAFI        OAT(I)
181800000000     C                   ENDIF
181900000000     C                   ENDIF
182000000000     C     OAT(I)        IFEQ      'E'
182100000000     C                   MOVEL     $USER         WEDP              3
182200000000     C     WEDP          IFEQ      'EDP'
182300000000     C                   MOVEL     'S'           OAT(I)
182400000000     C                   ELSE
182500000000     C                   MOVEL     'N'           OAT(I)
182600000000     C                   ENDIF
182700000000     C                   ENDIF
182800000000     C                   ENDDO
182900000000     C                   ENDSR
183000000000     C*------------------------------------------------------------------------*
183100000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
183200000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
183300000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
183400000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
183500000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
183600000000     C*------------------------------------------------------------------------*
183700000000     C     OUTOPZ        BEGSR
183800000000     C     1             DO        8             I
183900000000     C                   MOVEL     *BLANKS       OPV(I)
184000000000     C                   ENDDO
184100000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
184200000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
184300000000     C                   ADD       1             OPZULT
184400000000     C     OPZULT        IFGT      24
184500000000     C                   Z-ADD     1             OPZULT
184600000000     C                   END
184700000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
184800000000     C     1             DO        24            I
184900000000     C     OAT(I)        IFEQ      'S'
185000000000     C     ODE(I)        ANDNE     *BLANKS
185100000000     C                   ADD       1             OPZNAT
185200000000     C                   ENDIF
185300000000     C                   ENDDO
185400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
185500000000     C                   Z-ADD     0             W
185600000000     C     OPZULT        DO        24            I
185700000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
185800000000     C     W             IFLT      8
185900000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
186000000000     C     OAT(I)        IFEQ      'S'
186100000000     C     ODE(I)        ANDNE     *BLANKS
186200000000     C                   ADD       1             W
186300000000     C                   MOVEL     ODE(I)        OPV(W)
186400000000     C                   ENDIF
186500000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
186600000000     C                   Z-ADD     I             OPZULT
186700000000     C                   ENDIF
186800000000     C                   ENDDO
186900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
187000000000     C                   MOVEL     OPV(1)        VOD001
187100000000     C                   MOVEL     OPV(2)        VOD002
187200000000     C                   MOVEL     OPV(3)        VOD003
187300000000     C                   MOVEL     OPV(4)        VOD004
187400000000     C                   MOVEL     OPV(5)        VOD005
187500000000     C                   MOVEL     OPV(6)        VOD006
187600000000     C                   MOVEL     OPV(7)        VOD007
187700000000     C                   MOVEL     OPV(8)        VOD008
187800000000     C                   MOVEL     '1'           $TESVD
187900000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
188000000000     C     OPZNAT        IFGT      8
188100000000     C                   MOVEL     'S'           FAT(23)
188200000000     C                   ENDIF
188300000000     C                   ENDSR
188400000000     C*------------------------------------------------------------------------*
188500000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
188600000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
188700000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
188800000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
188900000000     C*------------------------------------------------------------------------*
189000000000     C     CONOPZ        BEGSR
189100000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
189200000000     C                   MOVEL     *BLANKS       OPZOK             1
189300000000     C     1             DO        24            I                 5 0
189400000000     C     VSCOPZ        IFEQ      OPD(I)
189500000000     C     VSCOPZ        OREQ      OPS(I)
189600000000     C     OAT(I)        IFEQ      'S'
189700000000     C                   MOVEL     OPD(I)        VSCOPZ
189800000000     C                   MOVEL     'S'           OPZOK
189900000000     C                   ELSE
190000000000     C                   MOVEL     'N'           OPZOK             1
190100000000     C                   ENDIF
190200000000     C                   ENDIF
190300000000     C                   ENDDO
190400000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
190500000000     C     OPZOK         IFNE      'S'
190600000000     C                   MOVEL     MSG(3)        $MSG
190700000000     C                   EXSR      CARMSG
190800000000     C                   MOVEL     '1'           $ERR
190900000000     C                   MOVEL     'N'           OPZOK             1
191000000000     C                   ENDIF
191100000000     C                   ENDSR
191200000000     C*------------------------------------------------------------------------*
191300000000     C* ROUTINE GENERALIZZATA PASSAGGIO IMMAGINI BOLLA DI SEDE
191400000000     C*------------------------------------------------------------------------*
191500000000      /COPY GAITRASRC/SRCTREC,TRECDT
191600000000     O*------------------------------------------------------------------------*
191700000000     O* AGGIORNAMENTO RECORD ERRORE CON L'IMMAGINE -PRIMA- E -DOPO-
191800000000     O*------------------------------------------------------------------------*
191900970901     OECERR2    E            AGGERR
192000000000     O                       ERRRP1
192100000000     O                       ERRRP2
192200000000     O                       ERRRD1
192300000000     O                       ERRRD2
192400000000** MSG - MESSAGGI -------------------------------------------------------------*
192500000000TREC87R- Tasto funzionale non valido.                                          1
192600000000TREC87R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
192700000000TREC87R- Opzione non valida.                                                   3
192800000000TREC87R- Opzione abilitata ma non gestita. Avvertire CED.                      4
192900000000TREC87R- Inizio elenco.                                                        5
193000000000TREC87R- Fine elenco.                                                          6
193100000000TREC87R- Accettato solo un parametro di inizio elenco.                         7
193200000000TREC87R- Data errata                                                           8
193300000000TREC87R- Si possono variare solo errori di inserimento (I) e di TNCSB (C)      9
193400000000TREC87R- Cancellazione effettuata                                             10
193500000000TREC87R- C/Economico rilanciato                                               11
193600000000TREC87R- C/Econ non lanciato perchè questo non è il primo errore della bolla  12
193700040624TREC87R- Non trovata bolla
193800000000TREC87R- Errore non cancellato perchè non è il primo della bolla              14
193900020607TREC87R- 15
194000000000TREC87R- 16
194100000000TREC87R- Errore non presente in archivio: F05 per avere la situazione aggior. 17
194200020607TREC87R- 18
194300000000TREC87R- 19
194400000000TREC87R- 20
194500000000** OPZ - OPZIONI          SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.) -----------*
194600000000- 1-1 -1=Lancio C/E       -    SN     SN
194700000000- 2-2 -2=Modifica TNCSB   -    SN     SN
194800000000- 3-3 -                   -    NN     NN
194900000000- 4-4 -4=Cancellazione    -    SN     SN
195000950519- 5-5 -5=Visualizza       -    SS     SS
195100020610- 6-6 -6=Stampa bolla     -    SS     SS
195200080908- 7-7 -7=Inter. bolla     -    SS     NN
195300000000- 8-8 -                   -    NN     NN
195400000000- 9-9 -                   -    NN     NN
195500000000-10-10-                   -    NN     NN
195600000000-11-  -                   -    NN     NN
195700000000-12-  -                   -    NN     NN
195800000000-13-  -                   -    NN     NN
195900000000-14-  -                   -    NN     NN
196000000000-15-  -                   -    NN     NN
196100000000-16-  -                   -    NN     NN
196200000000-17-  -                   -    NN     NN
196300000000-18-  -                   -    NN     NN
196400000000-19-  -                   -    NN     NN
196500000000-20-  -                   -    NN     NN
196600000000-21-  -                   -    NN     NN
196700000000-22-  -                   -    NN     NN
196800000000-23-  -                   -    NN     NN
196900000000-24-  -                   -    NN     NN
197000000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.) -----------*
197100000000F1    -                   -    NN     NN
197200000000F2    -                   -    NN     NN
197300000000F3    -F3=Fine            -    SS     SS
197400000000F4    -                   -    NN     NN
197500000000F5    -F5=Rivisualizza    -    SS     SS
197600000000F6    -                   -    NN     NN
197700000000F7    -                   -    NN     NN
197800000000F8    -                   -    NN     NN
197900000000F9    -                   -    NN     NN
198000000000F10   -                   -    NN     NN
198100000000F11   -                   -    NN     NN
198200000000F12   -F12=Ritorno        -    SS     SS
198300000000F13   -                   -    NN     NN
198400000000F14   -                   -    NN     NN
198500000000F15   -                   -    NN     NN
198600000000F16   -                   -    NN     NN
198700000000F17   -F17=Inizio dati    -    SS     SS
198800000000F18   -F18=Fine dati      -    SS     SS
198900000000F19   -                   -    NN     NN
199000000000F20   -                   -    NN     NN
199100000000F21   -                   -    NN     NN
199200000000F22                       -    NN     NN
199300000000F23   -F23=Altre opzioni  -    NN     NN
199400000000F24   -F24=Altri tasti    -    NN     NN
199500000000ENTER -                   -    SS     SS
199600000000ROLLUP-                   -    SS     SS
199700000000ROLLDN-                   -    SS     SS
199800000000HELP  -                   -    NN     NN
199900000000PRINT -                   -    NN     NN
200000000000BSPACE-                   -    NN     NN
200100000000CLEAR -                   -    NN     NN
