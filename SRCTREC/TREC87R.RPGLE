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
007300080908     D TIBS34DS      E DS
007400020923     D DDATIUTE      E DS
007500020923     D AZUTEDS       E DS                  extname(AZUTE00F)
007600000000     D*---------------------
007700000000     D* DS PER GESTIONE TASTI FUNZIONALI
007800000000     D*---------------------
007900000000     D DSKEYS        E DS
008000000000     D  KEYRIG                 1     40
008100000000     D*---------------------
008200000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
008300000000     D*---------------------
008400000000     D                SDS
008500000000     D  $PGM                   1     10
008600000000     D  $USER                254    263
008700000000     D*---------------------
008800000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
008900000000     D*---------------------
009000000000     D DEVDS1          DS
009100000000     D* CHIAVE FUNZIONALE BATTUTA
009200000000     D  KEY                  369    369
009300000000     D* POSIZIONE CURSORE (IN HEX)
009400000000     D  CURSOR               370    371
009500000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
009600000000     D  FRTSFL               378    379B 0
009700000000     D*---------------------
009800000000     D* ARCHITETTURA
009900000000     D*---------------------
010000000000     D KPJBA         E DS
010100000000     D*------------------
010200000000     D* DS DI PROCEDURA
010300000000     D*------------------
010400000000     D DS87          E DS                  EXTNAME(TREC87DS)
010500000000     D*-------------------
010600000000     D* DS ESTERNA PER GUFO
010700000000     D*-------------------
010800000000     D DS£ECO        E DS
010900000000     D*------------------
011000000000     D* DS CONTROLLO DATA (8)
011100000000     D*------------------
011200000000     D WLBDA8          DS                  INZ
011300000000     D  G08DAT                 1      8  0
011400000000     D  G08INV                 9     16  0
011500000000     D  G08ERR                17     17
011600000000     D  G08TGI                18     22  0
011700991223     D*------------------
011800991223     D* S/COMPOSIZIONE DATA
011900991223     D*------------------
012000991223     D                 DS
012100991223     D  DSANN8                 1      4  0
012200991223     D  DSMES8                 5      6  0
012300991223     D  DSGIO8                 7      8  0
012400991223     D  DSDAT8                 1      8  0
012500991223     D                 DS
012600991223     D  DSGIO6                 1      2  0
012700991223     D  DSMES6                 3      4  0
012800991223     D  DSANN6                 5      6  0
012900991223     D  DSDAT6                 1      6  0
013000000000     D*-------------------
013100000000     D* DS DESCRIZIONE MESSAGGIO
013200000000     D*-------------------
013300000000     D                 DS
013400000000     D  DSERR                  1     80
013500000000     D  DSVOCE                29     31
013600000000     D  DSTT1                 33     35
013700000000     D  DSTT2                 37     39
013800000000     D  DSDER                  1     74
013900000000     D  DSCER                 75     78
014000000000     D*-------------------
014100000000     D* DS CODICE ERRORE
014200000000     D*-------------------
014300000000     D                 DS
014400000000     D  ERRDAM                 1      6  0
014500000000     D  ERRORM                 7     12  0
014600000000     D  ERRPRG                13     17  0
014700000000     D  ERRCOD                 1     12  0
014800000000     D*-------------------
014900000000     D* DS CODICE ERRORE DI LAVORO
015000000000     D*-------------------
015100000000     D                 DS
015200000000     D  VSHDAM                 1      6  0
015300000000     D  VSHORM                 7     12  0
015400000000     D  VSHPRG                13     17  0
015500000000     D  VSHCOD                 1     12  0
015600000000     D*-------------------
015700990812     D* DS VALORIZZAZIONE IMMAGINE ERRORE DA TITAS
015800000000     D*-------------------
015900000000     D TRECDS        E DS
016000000000     D  D$PDS1                 1    250
016100000000     D  D$PDS2               251    500
016200000000     D  D$DDS1               501    750
016300000000     D  D$DDS2               751   1000
016400000000     D*-------------------
016500030123     D* DS VALORIZZAZIONE IMMAGINE ERRORE DA TNCSB
016600000000     D*-------------------
016700000000     D DS$CSB        E DS
016800000000     D  DCPDS1                 1    250
016900000000     D  DCPDS2               251    500
017000000000     D  DCDDS1               501    750
017100000000     D  DCDDS2               751   1000
017200030123     D*-------------------
017300030123     D* DS VALORIZZAZIONE IMMAGINE ERRORE DA FIAR5 - RECORD TRANSITI
017400030123     D*-------------------
017500030123     D DS$AR5T       E DS
017600030123     D  D$PAT1                 1    250
017700030123     D  D$DAT1               251    500
017800030123     D*-------------------
017900030123     D* DS VALORIZZAZIONE IMMAGINE ERRORE DA FIAR5 - RECORD BANCALI
018000030123     D*-------------------
018100030123     D DS$AR5B       E DS
018200030123     D  D$PAB1                 1    250
018300030123     D  D$DAB1               251    500
018400120130     D*-------------------
018500120130     D* DS KPJBU per visulizzare manca tariffa
018600120130     D*-------------------
018700120130     D param           DS
018800120130     D  parges                        1
018900120130     D  parcer                        4
019000000000     C*------------------------------------------------------------------------*
019100000000     C* MAIN LINE
019200000000     C*------------------------------------------------------------------------*
019300000000     C*
019400000000DO  1C     $FINE         DOUEQ     '1'
019500000000     C*
019600000000     C                   MOVEL     '0'           $ERR
019700000000     C*
019800000000     C* CONTROLLA GESTIONE VIDEO
019900000000IF  2C     $GES          IFEQ      'VS'
020000000000     C                   EXSR      GESVS
020100000000E   2C                   ENDIF
020200000000     C*
020300000000E   1C                   ENDDO
020400000000     C*
020500000000     C                   SETON                                        LR
020600000000     C*------------------------------------------------------------------------*
020700000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
020800000000     C*------------------------------------------------------------------------*
020900000000     C     CARVS         BEGSR
021000000000     C*
021100000000     C                   MOVEL     'VS'          $GES
021200000000     C*
021300000000     C                   EXSR      CAROPZ
021400000000     C                   EXSR      OUTOPZ
021500000000     C                   MOVEA     FUN2          FUN
021600000000     C                   EXSR      CARFUN
021700000000     C                   EXSR      OUTFUN
021800000000     C*
021900000000     C                   WRITE     EC87DVT
022000000000     C                   WRITE     EC87DVO
022100000000     C                   WRITE     EC87DVF
022200000000     C*
022300000000     C                   EXSR      POSSFL
022400000000     C*
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700000000     C* GESVS  - GESTIONE SUBFILE
022800000000     C*------------------------------------------------------------------------*
022900000000     C     GESVS         BEGSR
023000000000     C*
023100000000IF  1C     $TESVD        IFEQ      '1'
023200000000     C                   WRITE     EC87DVT
023300000000     C                   WRITE     EC87DVO
023400000000     C                   WRITE     EC87DVF
023500000000E   1C                   ENDIF
023600000000     C*
023700000000     C                   EXSR      OUTSFL
023800000000     C*
023900000000     C                   EXSR      CONFUN
024000000000     C*
024100000000IF  1C     $ERR          IFEQ      '0'
024200000000SL  2C                   SELECT
024300000000     C                   OTHER
024400000000     C                   EXSR      FUNSFL
024500000000E   2C                   ENDSL
024600000000E   1C                   ENDIF
024700000000     C*
024800000000     C                   ENDSR
024900000000     C*------------------------------------------------------------------------*
025000000000     C*  GESOPZ - GESTIONE OPZIONI
025100000000     C*------------------------------------------------------------------------*
025200000000     C     GESOPZ        BEGSR
025300000000     C*
025400000000SL  1C                   SELECT
025500000000     C     VSCOPZ        WHENEQ    ' 1'
025600000000     C                   EXSR      O01GES
025700000000     C     VSCOPZ        WHENEQ    ' 2'
025800000000     C                   EXSR      O02GES
025900000000     C     VSCOPZ        WHENEQ    ' 4'
026000000000     C                   EXSR      O04GES
026100950519     C     VSCOPZ        WHENEQ    ' 5'
026200950519     C                   EXSR      O05GES
026300020607     C     VSCOPZ        WHENEQ    ' 6'
026400020607     C                   EXSR      O06GES
026500080908     C     VSCOPZ        WHENEQ    ' 7'
026600080908     C                   EXSR      O07GES
026700000000     C                   OTHER
026800000000     C                   MOVEL     MSG(4)        $MSG
026900000000     C                   EXSR      CARMSG
027000000000E   1C                   ENDSL
027100000000     C*
027200000000     C                   ENDSR
027300000000     C*------------------------------------------------------------------------*
027400000000     C* O01GES - GESTIONE OPZIONE 1=SCELTA RILANCIO CONTO ECONOMICO
027500000000     C*------------------------------------------------------------------------*
027600000000     C     O01GES        BEGSR
027700000000     C*
027800000000     C* E' POSSIBILE SELEZIONARE IL RECORD SOLO SE ANCORA PRESENTE IN ARCHIVIO
027900000000IF  1C     VSCGIR        IFNE      *BLANKS
028000000000     C                   MOVEL     '1'           $ERR
028100000000     C                   MOVEL     MSG(17)       $MSG
028200000000     C                   EXSR      CARMSG
028300000000E   1C                   ENDIF
028400000000     C*
028500000000     C* E' POSSIBILE SELEZIONARE IL RECORD SOLO SE E'IL PRIMO ERRORE DELLA BOLLA
028600000103IF  1C     $ERR          IFEQ      '0'
028700000103     C                   Z-ADD     VSHAAS        KE2AAS
028800000103     C                   Z-ADD     VSCLNP        KE2LNP
028900000000     C                   Z-ADD     VSCNRS        KE2NRS
029000000000     C                   Z-ADD     VSCNSP        KE2NSP
029100950329     C                   MOVEL     VSCTBL        KE2TBL
029200000000     C                   MOVEL     '0'           WPREC
029300950329     C     KEYER2        SETLL     ECERR02L
029400970901     C     KEYER2        READE(N)  ECERR02L                               96
029500000000DO  2C     *IN96         DOWEQ     *OFF
029600000000     C     WPREC         ANDEQ     '0'
029700000000IF  3C     ERRCOD        IFLT      VSHCOD
029800000000     C     ERRFIL        ANDEQ     VSHFIL
029900000000     C                   MOVEL     '1'           WPREC
030000000000E   3C                   ENDIF
030100970901     C     KEYER2        READE(N)  ECERR02L                               96
030200000000E   2C                   ENDDO
030300000000     C*
030400000000     C* EMETTE MESSAGGIO DI ERRORE
030500000000IF  2C     WPREC         IFEQ      '1'
030600000000     C                   MOVEL     '1'           $ERR
030700000000     C                   MOVEL     MSG(12)       $MSG
030800000000     C                   EXSR      CARMSG
030900000000E   2C                   ENDIF
031000000000E   1C                   ENDIF
031100000000     C*
031200020619     C* SE IL RECORD SELEZIONATO E' DI "INSERIMENTO" ED E' SU "TITAS*" SEGUE
031300000000     C* UNA PROCEDURA PARTICOLARE
031400000000IF  1C     $ERR          IFEQ      '0'
031500000000IF  2C     VSCSTS        IFEQ      'I'
031600000000     C     VSCFIL        ANDEQ     'T'
031700000000     C                   EXSR      STSINS
031800000000E   2C                   ENDIF
031900000000E   1C                   ENDIF
032000000000     C*
032100000000     C* SE NON CI SONO ERRORI PROCEDE AL RILANCIO DEL CONTO ECONOMICO
032200000000IF  1C     $ERR          IFEQ      '0'
032300000000     C                   EXSR      SCHOIS
032400000000E   1C                   ENDIF
032500000000     C*
032600000000     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
032700000000     C*
032800000000     C                   ENDSR
032900000000     C*------------------------------------------------------------------------*
033000000000     C* O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE SINGOLO ERRORE
033100000000     C*------------------------------------------------------------------------*
033200000000     C     O04GES        BEGSR
033300000000     C*
033400000000     C* E' POSSIBILE SELEZIONARE L'ERRORE SOLO SE ANCORA PRESENTE IN ARCHIVIO
033500000000IF  1C     VSCGIR        IFNE      *BLANKS
033600000000     C                   MOVEL     '1'           $ERR
033700000000     C                   MOVEL     MSG(17)       $MSG
033800000000     C                   EXSR      CARMSG
033900000000E   1C                   ENDIF
034000000000     C*
034100000000     C* E' POSSIBILE SELEZIONARE IL RECORD SOLO SE E'IL PRIMO ERRORE DELLA BOLLA
034200000000IF  1C     $ERR          IFEQ      '0'
034300000103     C                   Z-ADD     VSHAAS        KE2AAS
034400000000     C                   Z-ADD     VSCLNP        KE2LNP
034500000000     C                   Z-ADD     VSCNRS        KE2NRS
034600000000     C                   Z-ADD     VSCNSP        KE2NSP
034700950329     C                   MOVEL     VSCTBL        KE2TBL
034800000000     C                   MOVEL     '0'           WPREC
034900950329     C     KEYER2        SETLL     ECERR02L
035000970901     C     KEYER2        READE(N)  ECERR02L                               96
035100000000DO  2C     *IN96         DOWEQ     *OFF
035200000000     C     WPREC         ANDEQ     '0'
035300000000IF  3C     ERRCOD        IFLT      VSHCOD
035400000000     C     ERRFIL        ANDEQ     VSHFIL
035500000000     C                   MOVEL     '1'           WPREC
035600000000E   3C                   ENDIF
035700970901     C     KEYER2        READE(N)  ECERR02L                               96
035800000000E   2C                   ENDDO
035900000000     C*
036000000000     C* EMETTE MESSAGGIO DI ERRORE
036100000000IF  2C     WPREC         IFEQ      '1'
036200000000     C                   MOVEL     '1'           $ERR
036300000000     C                   MOVEL     MSG(14)       $MSG
036400000000     C                   EXSR      CARMSG
036500000000E   2C                   ENDIF
036600000000E   1C                   ENDIF
036700000000     C*
036800020619     C* SE IL RECORD SELEZIONATO E' DI "INSERIMENTO" ED E' SU "TITAS*" SEGUE
036900000000     C* UNA PROCEDURA PARTICOLARE
037000000000IF  1C     $ERR          IFEQ      '0'
037100000000IF  2C     VSCSTS        IFEQ      'I'
037200000000     C     VSCFIL        ANDEQ     'T'
037300000000     C                   EXSR      STSINS
037400000000E   2C                   ENDIF
037500000000E   1C                   ENDIF
037600000000     C*
037700000000     C* SE NON CI SONO ERRORI CANCELLA FISICAMENTE L'ERRORE DI CONTO ECONOMICO
037800000000IF  1C     $ERR          IFEQ      '0'
037900000000     C                   EXSR      CANCEL
038000000000E   1C                   ENDIF
038100000000     C*
038200000000     C                   ENDSR
038300000000     C*------------------------------------------------------------------------*
038400000000     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD DI TNCSB00F
038500000000     C*------------------------------------------------------------------------*
038600000000     C     O02GES        BEGSR
038700000000     C*
038800000000     C* E' POSSIBILE SELEZIONARE L'ERRORE SOLO SE ANCORA PRESENTE IN ARCHIVIO
038900000000IF  1C     VSCGIR        IFNE      *BLANKS
039000000000     C                   MOVEL     '1'           $ERR
039100000000     C                   MOVEL     MSG(17)       $MSG
039200000000     C                   EXSR      CARMSG
039300000000E   1C                   ENDIF
039400000000     C*
039500000000     C* E' POSSIBILE SELEZIONARE IL RECORD SOLO SE E'DI TIPO "INSERIMENTO" E SE
039600000000     C* IL FILE MODIFICATO E' TNCSB
039700000000IF  1C     $ERR          IFEQ      '0'
039800000000IF  2C     VSCSTS        IFEQ      'I'
039900000000     C     VSCFIL        ANDEQ     'C'
040000000000X   2C                   ELSE
040100000000     C                   MOVEL     '1'           $ERR
040200000000     C                   MOVEL     MSG(9)        $MSG
040300000000     C                   EXSR      CARMSG
040400000000E   2C                   ENDIF
040500000000E   1C                   ENDIF
040600000000     C*
040700000000     C* ESEGUE LA MODIFICA
040800000000IF  1C     $ERR          IFEQ      '0'
040900000103     C                   Z-ADD     VSHAAS        WAAS
041000000000     C                   Z-ADD     VSCLNP        WLNP
041100000000     C                   Z-ADD     VSCNRS        WNRS
041200950329     C                   Z-ADD     VSCNSP        WNSP
041300950329     C                   MOVEL     VSCTBL        WTBL
041400000000     C                   Z-ADD     VSHDAT        WDAT
041500000000     C                   Z-ADD     VSHTIM        WTIM
041600000000     C                   Z-ADD     VSHPRG        WPRG
041700000000     C                   Z-ADD     VSHDAM        WDAM
041800000000     C                   Z-ADD     VSHORM        WORM
041900000000     C                   MOVEL     *BLANKS       WOP0
042000000000     C                   MOVEL     'O02'         WOP1
042100020621     C                   MOVEL(P)  'TREC88R'     WPGM              8
042200000000     C                   EXSR      LANPGM
042300000000     C*
042400000000     C* CANCELLA TUTTE LE VARIAZIONI SUCESSIVE ALL'INS. CON IL TIPO BOLLA ERRATO
042500000000IF  2C     $ERR          IFEQ      '0'
042600000000     C                   Z-ADD     WAAS          KE2AAS
042700000000     C                   Z-ADD     WLNP          KE2LNP
042800000000     C                   Z-ADD     WNRS          KE2NRS
042900000000     C                   Z-ADD     WNSP          KE2NSP
043000950329     C                   MOVEL     WTBL          KE2TBL
043100950329     C     KEYER2        SETLL     ECERR02L
043200950329     C     KEYER2        READE     ECERR02L                               96
043300000000DO  3C     *IN96         DOWEQ     *OFF
043400950329IF  4C     ERRSTS        IFEQ      'U'
043500950329IF  4C     ERRFIL        ANDEQ     'TNCSB00F'
043600970901     C                   DELETE    ECERR2
043700000000E   4C                   ENDIF
043800950329     C     KEYER2        READE     ECERR02L                               96
043900000000E   3C                   ENDDO
044000000000     C*
044100000000     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
044200000000E   2C                   ENDIF
044300000000E   1C                   ENDIF
044400000000     C*
044500000000     C                   ENDSR
044600950519     C*------------------------------------------------------------------------*
044700950519     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZA RECORD
044800950519     C*------------------------------------------------------------------------*
044900950519     C     O05GES        BEGSR
045000950519     C*
045100950519     C* E' POSSIBILE SELEZIONARE L'ERRORE SOLO SE ANCORA PRESENTE IN ARCHIVIO
045200950519IF  1C     VSCGIR        IFNE      *BLANKS
045300950519     C                   MOVEL     '1'           $ERR
045400950519     C                   MOVEL     MSG(17)       $MSG
045500950519     C                   EXSR      CARMSG
045600950519E   1C                   ENDIF
045700950519     C*
045800950519     C* LANCIA LA VISUALIZZAZIONE
045900950519IF  1C     $ERR          IFEQ      '0'
046000000103     C                   Z-ADD     VSHAAS        WAAS
046100950519     C                   Z-ADD     VSCLNP        WLNP
046200950519     C                   Z-ADD     VSCNRS        WNRS
046300950519     C                   Z-ADD     VSCNSP        WNSP
046400950519     C                   MOVEL     VSCTBL        WTBL
046500950519     C                   Z-ADD     VSHDAT        WDAT
046600950519     C                   Z-ADD     VSHTIM        WTIM
046700950519     C                   Z-ADD     VSHPRG        WPRG
046800950519     C                   Z-ADD     VSHDAM        WDAM
046900950519     C                   Z-ADD     VSHORM        WORM
047000950519     C                   MOVEL     *BLANKS       WOP0
047100950519     C                   MOVEL     'O05'         WOP1
047200020621     C                   MOVEL(P)  'TREC88R'     WPGM              8
047300950519     C                   EXSR      LANPGM
047400950519     C*
047500950519     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
047600950519E   1C                   ENDIF
047700950519     C*
047800950519     C                   ENDSR
047900020607     C*------------------------------------------------------------------------*
048000020610     C* O06GES - GESTIONE OPZIONE 6=STAMPA BOLLA
048100020607     C*------------------------------------------------------------------------*
048200020607     C     O06GES        BEGSR
048300020607     C*
048400020607     C* E' POSSIBILE SELEZIONARE L'ERRORE SOLO SE ANCORA PRESENTE IN ARCHIVIO
048500020607IF  1C     VSCGIR        IFNE      *BLANKS
048600020607     C                   MOVEL     '1'           $ERR
048700020607     C                   MOVEL     MSG(17)       $MSG
048800020607     C                   EXSR      CARMSG
048900020607E   1C                   ENDIF
049000020607     C*
049100020610     C* LANCIA LA STAMPA
049200020607IF  1C     $ERR          IFEQ      '0'
049300020607     C                   Z-ADD     VSHAAS        WAAS
049400020607     C                   Z-ADD     VSCLNP        WLNP
049500020607     C                   Z-ADD     VSCNRS        WNRS
049600020607     C                   Z-ADD     VSCNSP        WNSP
049700020607     C                   MOVEL     VSCTBL        WTBL
049800020607     C                   Z-ADD     VSHDAT        WDAT
049900020607     C                   Z-ADD     VSHTIM        WTIM
050000020607     C                   Z-ADD     VSHPRG        WPRG
050100020607     C                   Z-ADD     VSHDAM        WDAM
050200020607     C                   Z-ADD     VSHORM        WORM
050300020607     C                   MOVEL     *BLANKS       WOP0
050400020607     C                   MOVEL     'O05'         WOP1
050500020621     C                   MOVEL(P)  'TREC87R1'    WPGM              8
050600020607     C                   EXSR      LANPGM
050700020607     C*
050800020607     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
050900020607E   1C                   ENDIF
051000020607     C*
051100020607     C                   ENDSR
051200080908     C*------------------------------------------------------------------------*
051300080908     C* O07GES - GESTIONE OPZIONE 7=interrogazione bolla
051400080908     C*------------------------------------------------------------------------*
051500080908     C     O07GES        BEGSR
051600080908     C*
051700080908     c                   clear                   tnsb50ds
051800080908     C                   Z-ADD     VSHAAS        d50AAS
051900080908     C                   Z-ADD     VSCLNP        d50LNP
052000080908     C                   Z-ADD     VSCNRS        d50NRS
052100080908     C                   Z-ADD     VSCNSP        d50NSP
052200080908     C                   MOVEL     VSCTBL        d50TBL
052300080908     C                   MOVEL     'O05'         I50OP0
052400080908     C                   movel(p)  tnsb50ds      kpjbu
052500080908     c                   call      'TNSB50R'
052600080908     c                   parm                    kpjba
052700080908     C*
052800080908     C                   ENDSR
052900000000     C*------------------------------------------------------------------------*
053000000000     C* STSINS - TRATTA L'ERRORE DI TIPO "INSERIMENTO"
053100000000     C*------------------------------------------------------------------------*
053200000000     C     STSINS        BEGSR
053300000000     C*
053400000000     C* SE E'UN ERRORE DI INSERIMENTO, CARICA LA SITUAZIONE ATTUALE DELLA BOLLA
053500000000     C* NELLA DS -DOPO- DELL'ERRORE E CANCELLA LE VARIAZIONI SUCESSIVE ("U").
053600000000     C* SE POI TROVA UN ERRORE DI CANCELLAZIONE ("D") CANCELLA ANCHE QUESTO
053700000000     C* INSIEME AL RECORD DI INSERIMENTO APPENA CARICATO E DA' ERRORE PER NON
053800000000     C* FARE ESEGUIRE L'OPZIONE RICHIESTA (RILANCIO C/E O CANCELLAZIONE).
053900000000     C***
054000000000     C* LEGGE LA BOLLA
054100000000     C***
054200000103     C                   Z-ADD     VSHAAS        KTAAAS
054300000000     C                   Z-ADD     VSCLNP        KTALNP
054400950329     C                   Z-ADD     VSCNRS        KTANRS
054500950329     C                   Z-ADD     VSCNSP        KTANSP
054600000000     C                   MOVEL     VSCTBL        KTATBL
054700990812     C     KEYTAS        CHAIN     TITAS30C                           97
054800000000     C***
054900000000     C* LEGGE L'ERRORE
055000000000     C***
055100000000IF  1C     *IN97         IFEQ      *OFF
055200000103     C                   Z-ADD     VSHAAS        KE2AAS
055300970901     C                   Z-ADD     VSCLNP        KE2LNP
055400970901     C                   Z-ADD     VSCNRS        KE2NRS
055500970901     C                   Z-ADD     VSCNSP        KE2NSP
055600970901     C                   MOVEL     VSCTBL        KE2TBL
055700970902     C                   Z-ADD     VSHDAT        KE2DAT
055800970902     C                   Z-ADD     VSHTIM        KE2TIM
055900970902     C                   Z-ADD     VSHPRG        KE2PRG
056000970901     C     KE2ER2        CHAIN     ECERR02L                           97
056100970901IF  2C     *IN97         IFEQ      *OFF
056200000000     C                   EXSR      APDTAQ
056300000000     C                   EXSR      CDDTAQ
056400000000     C                   MOVEL     D$PDS1        ERRRP1
056500000000     C                   MOVEL     D$PDS2        ERRRP2
056600000000     C                   MOVEL     D$DDS1        ERRRD1
056700000000     C                   MOVEL     D$DDS2        ERRRD2
056800000000     C***
056900000000     C* AGGIORNA L'ERRORE
057000000000     C***
057100000000     C                   EXCEPT    AGGERR
057200000000     C***
057300000000     C* LEGGE RECORD SUCESSIVI
057400000000     C***
057500000000     C* LEGGE FINO A FINE FILE O SE TROVA UNA "CANCELLAZIONE"
057600000103     C                   Z-ADD     VSHAAS        KW3AAS
057700000000     C                   Z-ADD     VSCLNP        KW3LNP
057800000000     C                   Z-ADD     VSCNRS        KW3NRS
057900000000     C                   Z-ADD     VSCNSP        KW3NSP
058000000000     C                   MOVEL     '0'           $PROS
058100000000     C     KEYEW3        SETLL     ECERR03L
058200000000     C     KEYEW3        READE     ECERR03L                               96
058300970901DO  3C     *IN96         DOWEQ     *OFF
058400000000     C     $PROS         ANDEQ     '0'
058500000000     C*
058600000000     C* ANALIZZA SOLO ERRORI DI VARIAZIONE O DI CANCELLAZIONE
058700970901IF  4C     ERRTBL        IFEQ      VSCTBL
058800970901IF  5C     ERRSTS        IFEQ      'D'
058900000000     C     ERRSTS        OREQ      'U'
059000000000     C*
059100000000     C* CANCELLA L'ERRORE "D" O "U"
059200970901     C                   Z-ADD     ERRAAS        KE2AAS
059300970901     C                   Z-ADD     ERRLNP        KE2LNP
059400970901     C                   Z-ADD     ERRNRS        KE2NRS
059500970901     C                   Z-ADD     ERRNSP        KE2NSP
059600970901     C                   MOVEL     ERRTBL        KE2TBL
059700970902     C                   Z-ADD     ERRDAT        KE2DAT
059800970902     C                   Z-ADD     ERRTIM        KE2TIM
059900970902     C                   Z-ADD     ERRPRG        KE2PRG
060000970901     C     KE2ER2        CHAIN     ECERR02L                           97
060100970901     C                   DELETE    ECERR2
060200000000     C*
060300000000     C* SE TROVA UNA CANCELLAZIONE, SI RICORDA DI CANCELLARE ANCHE LA "I", E DI
060400000000     C* NON PROSEGUIRE USCENDO DAL CICLO
060500970901IF  6C     ERRSTS        IFEQ      'D'
060600000000     C                   MOVEL     '1'           $PROS
060700000000     C                   MOVEL     '1'           $ERR
060800000103     C                   Z-ADD     VSHAAS        KE2AAS
060900970901     C                   Z-ADD     VSCLNP        KE2LNP
061000970901     C                   Z-ADD     VSCNRS        KE2NRS
061100970901     C                   Z-ADD     VSCNSP        KE2NSP
061200970901     C                   MOVEL     VSCTBL        KE2TBL
061300970902     C                   Z-ADD     VSHDAT        KE2DAT
061400970902     C                   Z-ADD     VSHTIM        KE2TIM
061500970902     C                   Z-ADD     VSHPRG        KE2PRG
061600970901     C     KE2ER2        CHAIN     ECERR02L                           97
061700970901     C                   DELETE    ECERR2
061800970901E   6C                   ENDIF
061900970901E   5C                   ENDIF
062000970901E   4C                   ENDIF
062100000000     C*
062200000000     C* LEGGE UN ALTRO RECORD SE DEVE PROSEGUIRE
062300970901IF  4C     $PROS         IFEQ      '0'
062400000000     C     KEYEW3        READE     ECERR03L                               96
062500970901E   4C                   ENDIF
062600970901E   3C                   ENDDO
062700970901E   2C                   ENDIF
062800000000     C*
062900000000     C* SI RIPOSIZIONA SU ECERR03L DALL'ULTIMO RECORD CARICATO NEL SUBFIL
063000000000     C     KEYER3        SETGT     ECERR03L                           98
063100000000     C*
063200000000X   1C                   ELSE
063300040624     c* se non trova la bolla in titas ma sono in cancellazione non metto
063400040624     c* l'errore
063500040624     c     VSCOPZ        ifne      ' 4'
063600000000     C                   MOVEL     '1'           $ERR
063700040624     C                   MOVEL     MSG(13)       $MSG
063800040624     C                   EXSR      CARMSG
063900000000E   1C                   ENDIF
064000040624E   1C                   ENDIF
064100000000     C*
064200000000     C                   ENDSR
064300000000     C*------------------------------------------------------------------------*
064400000000     C* SCHOIS - SCELTA DI CALCOLO DI CONTO ECONOMICO DI UNA SOLA BOLLA
064500000000     C*------------------------------------------------------------------------*
064600000000     C     SCHOIS        BEGSR
064700000000     C*
064800000000     C* LEGGE IL RECORD SELEZIONATO
064900000103     C                   Z-ADD     VSHAAS        KE2AAS
065000970901     C                   Z-ADD     VSCLNP        KE2LNP
065100970901     C                   Z-ADD     VSCNRS        KE2NRS
065200970901     C                   Z-ADD     VSCNSP        KE2NSP
065300970901     C                   MOVEL     VSCTBL        KE2TBL
065400970902     C                   Z-ADD     VSHDAT        KE2DAT
065500970902     C                   Z-ADD     VSHTIM        KE2TIM
065600970902     C                   Z-ADD     VSHPRG        KE2PRG
065700970901     C     KE2ER2        CHAIN     ECERR02L                           97
065800000000IF  1C     *IN97         IFEQ      *OFF
065900000000     C                   EXSR      GUFO
066000970901     C                   DELETE    ECERR2
066100000000     C* MESSAGGIO
066200000000     C                   MOVEL     '*'           VSCGIR
066300000000     C                   MOVEL     MSG(11)       $MSG
066400000000     C                   EXSR      CARMSG
066500000000E   1C                   ENDIF
066600000000     C*
066700000000     C                   ENDSR
066800000000     C*------------------------------------------------------------------------*
066900000000     C* LANPGM - LANCIA UN PROGRAMMA
067000000000     C*------------------------------------------------------------------------*
067100000000     C     LANPGM        BEGSR
067200000000     C*--------------------
067300000000     C* PARAMETRI INFORMATIVI
067400000000     C*--------------------
067500000000     C                   MOVEL     WOP0          D87OP0
067600000000     C                   MOVEL     WOP1          D87OP1
067700000000     C                   MOVEL     '0'           D87F03
067800000000     C                   MOVEL     '0'           D87F12
067900000000     C                   MOVEL     '0'           D87ERR
068000000000     C                   MOVEL     *BLANKS       D87MSG
068100000000     C*--------------------
068200000000     C* PARAMETRI
068300000000     C*--------------------
068400000000     C                   Z-ADD     WAAS          D87AAS
068500000000     C                   Z-ADD     WLNP          D87LNP
068600000000     C                   Z-ADD     WNRS          D87NRS
068700000000     C                   Z-ADD     WNSP          D87NSP
068800000000     C                   MOVEL     WTBL          D87TBL
068900000000     C                   Z-ADD     WDAT          D87DAT
069000000000     C                   Z-ADD     WTIM          D87TIM
069100000000     C                   Z-ADD     WPRG          D87PRG
069200000000     C                   Z-ADD     WDAM          D87DAM
069300000000     C                   Z-ADD     WORM          D87ORM
069400000000     C* LANCIO
069500000000     C                   MOVEL     DS87          KPJBU
069600000000     C                   CALL      WPGM
069700000000     C                   PARM                    KPJBA
069800000000     C                   MOVEL     KPJBU         DS87
069900000000     C*
070000000000     C* SE RITORNA PREMUTO F3 ESCE DAL PROGRAMMA, SE F12 SI FERMA
070100000000IF  1C     D87F03        IFEQ      '1'
070200000000     C     D87F12        OREQ      '1'
070300000000     C                   MOVEL     '1'           $ERR
070400000000IF  2C     D87F03        IFEQ      '1'
070500000000     C                   MOVEL     '1'           $F03
070600000000     C                   MOVEL     '1'           $FINE
070700000000E   2C                   ENDIF
070800000000     C*
070900000000     C* ALTRIMENTI MANDA MESSAGGIO RICEVUTO DAL PGM CHIAMATO
071000000000X   1C                   ELSE
071100000000     C                   MOVEL     D87ERR        $ERR
071200000000     C                   MOVEL     D87MSG        $MSG
071300000000     C                   EXSR      CARMSG
071400000000E   1C                   ENDIF
071500000000     C*
071600000000     C                   ENDSR
071700000000     C*------------------------------------------------------------------------*
071800000000     C* CANCEL - CANCELLA FISICAMENTE L'ERRORE DI CONTO ECONOMICO
071900000000     C*------------------------------------------------------------------------*
072000000000     C     CANCEL        BEGSR
072100000000     C*
072200000000     C* LEGGE IL RECORD SELEZIONATO
072300000103     C                   Z-ADD     VSHAAS        KE2AAS
072400970901     C                   Z-ADD     VSCLNP        KE2LNP
072500970901     C                   Z-ADD     VSCNRS        KE2NRS
072600970901     C                   Z-ADD     VSCNSP        KE2NSP
072700970901     C                   MOVEL     VSCTBL        KE2TBL
072800970902     C                   Z-ADD     VSHDAT        KE2DAT
072900970902     C                   Z-ADD     VSHTIM        KE2TIM
073000970902     C                   Z-ADD     VSHPRG        KE2PRG
073100970901     C     KE2ER2        CHAIN     ECERR02L                           97
073200000000IF  1C     *IN97         IFEQ      *OFF
073300970901     C                   DELETE    ECERR2
073400000000     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
073500000000     C                   MOVEL     'C'           VSCGIR
073600000000     C                   MOVEL     MSG(10)       $MSG
073700000000     C                   EXSR      CARMSG
073800000000E   1C                   ENDIF
073900000000     C*
074000000000     C                   ENDSR
074100000000     C*------------------------------------------------------------------------*
074200000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
074300000000     C*------------------------------------------------------------------------*
074400000000     C     NEWPOS        BEGSR
074500000000     C*
074600000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
074700000000     C*        1 = RICHIESTO POSIZIONAMENTO
074800000000     C                   Z-ADD     0             SWPOSI            7 0
074900000000     C*
075000000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
075100000000     C*            NON OCCORRE RICARICARE IL SUBFILE
075200000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
075300000000     C                   Z-ADD     0             SWMODI            1 0
075400000000     C*
075500000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO O NUOVA SELEZIONE
075600000000IF  1C     VCCDAT        IFNE      DEPDAT
075700000000     C     VCCAAS        ORNE      DEPAAS
075800000000     C     VCCLNP        ORNE      DEPLNP
075900000000     C     VCCNRS        ORNE      DEPNRS
076000000000     C     VCCNSP        ORNE      DEPNSP
076100000000     C     VCCTBL        ORNE      DEPTBL
076200000000     C* SELEZIONI
076300000000     C     VCCCER        ORNE      DEPCER
076400000000     C     VCCDSP        ORNE      DEPDSP
076500000000     C     VCCPGF        ORNE      DEPPGF
076600000000     C     VCCSTS        ORNE      DEPSTS
076700000000     C*
076800000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
076900000000     C                   EXSR      CHKPAR
077000000000     C*
077100000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
077200000000IF  2C     $ERR          IFEQ      '0'
077300000000     C                   Z-ADD     1             SWPOSI
077400000000     C                   Z-ADD     VCCDAT        DEPDAT
077500000000     C                   Z-ADD     VCCAAS        DEPAAS
077600000000     C                   Z-ADD     VCCLNP        DEPLNP
077700000000     C                   Z-ADD     VCCNRS        DEPNRS
077800000000     C                   Z-ADD     VCCNSP        DEPNSP
077900000000     C                   MOVEL     VCCTBL        DEPTBL
078000000000     C* SELEZIONI
078100000000     C                   MOVEL     VCCCER        DEPCER
078200000000     C                   Z-ADD     VCCDSP        DEPDSP
078300000000     C                   MOVEL     VCCPGF        DEPPGF
078400000000     C                   MOVEL     VCCSTS        DEPSTS
078500000000E   2C                   ENDIF
078600000000E   1C                   ENDIF
078700000000     C*
078800000000     C                   ENDSR
078900000000     C*------------------------------------------------------------------------*
079000000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
079100000000     C*------------------------------------------------------------------------*
079200000000     C     CHKPAR        BEGSR
079300000000     C*
079400000000     C* IMPOSTO INDICATORI EVIDENZAZIONE ERRORI SUI PARAMETRI
079500000000     C                   SETOFF                                       808182
079600000000     C                   SETOFF                                       838485
079700000000     C                   SETOFF                                       86
079800000000     C*
079900000000     C* IMPOSTO INDICATORI EVIDENZAZIONE ORDINAMENTO
080000000000     C                   SETOFF                                       5051
080100000000     C*
080200000000     C* PULISCO DATE INVERTITE
080300000000     C                   Z-ADD     *ZEROS        DATVCC
080400000000     C*--------------------
080500000000     C* ORDINAMENTO
080600000000     C*--------------------
080700000000     C* SE TUTTI I PARAMETRI SONO UGUALI A BLANK O ZERO, UTILIZZO CHIAVE "DAT"
080800000000IF  1C     VCCDAT        IFEQ      *ZEROS
080900000000     C     VCCTIM        ANDEQ     *ZEROS
081000000000     C     VCCAAS        ANDEQ     *ZEROS
081100000000     C     VCCLNP        ANDEQ     *ZEROS
081200000000     C     VCCNRS        ANDEQ     *ZEROS
081300000000     C     VCCNSP        ANDEQ     *ZEROS
081400000000     C     VCCTBL        ANDEQ     *BLANKS
081500040914     C     VCCDSP        ANDEQ     *zeros
081600000000     C                   MOVEL     'DAT'         WTIPRI            3
081700000000     C                   SETON                                        50
081800000000     C*
081900000000X   1C                   ELSE
082000000000     C*
082100000000     C* ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
082200000000IF  2C     VCCAAS        IFNE      *ZEROS
082300000000     C     VCCLNP        ORNE      *ZEROS
082400000000     C     VCCNRS        ORNE      *ZEROS
082500000000     C     VCCNSP        ORNE      *ZEROS
082600000000     C     VCCTBL        ORNE      *BLANKS
082700000000IF  3C     VCCDAT        IFNE      *ZEROS
082800000000     C     VCCTIM        ORNE      *ZEROS
082900040914     C     VCCdsp        ORNE      *ZEROS
083000000000     C                   MOVEL     MSG(7)        $MSG
083100000000     C                   EXSR      CARMSG
083200000000     C                   MOVEL     '1'           $ERR
083300000000     C                   SETON                                        8081
083400000000E   3C                   ENDIF
083500000000E   2C                   ENDIF
083600000000     C*
083700000000IF  2C     $ERR          IFEQ      '0'
083800000000     C*
083900000000     C* INIZIO ELENCO PER  "D A T A   E R R O R E"
084000000000IF  3C     VCCDAT        IFNE      *ZEROS
084100000000     C     VCCTIM        ORNE      *ZEROS
084200000000     C                   MOVEL     'DAT'         WTIPRI
084300000000     C                   SETON                                        50
084400000000     C* CONTROLLA ESATTEZZA
084500000000     C                   Z-ADD     VCCDAT        G08DAT
084600000000     C                   Z-ADD     *ZEROS        G08INV
084700000000     C                   MOVEL     '0'           G08ERR
084800000000     C                   CALL      'XSRDA8'
084900000000     C                   PARM                    WLBDA8
085000000000     C     G08ERR        IFEQ      '1'
085100000000     C                   MOVEL     '1'           $ERR
085200000000     C                   MOVEL     MSG(8)        $MSG
085300000000     C                   EXSR      CARMSG
085400000000     C                   SETON                                        80
085500000000     C                   ELSE
085600000000     C                   Z-ADD     G08DAT        VCCDAT
085700000000     C                   Z-ADD     G08INV        DATVCC            8 0
085800000000     C                   ENDIF
085900000000E   3C                   ENDIF
086000000000     C*
086100000000     C* INIZIO ELENCO PER  "B O L L A"
086200000000IF  3C     VCCAAS        IFNE      *ZEROS
086300000000     C     VCCLNP        ORNE      *ZEROS
086400000000     C     VCCNRS        ORNE      *ZEROS
086500000000     C     VCCNSP        ORNE      *ZEROS
086600000000     C     VCCTBL        ORNE      *BLANKS
086700040914     C     VCCdsp        ORNE      *zeros
086800000000     C                   MOVEL     'BOL'         WTIPRI
086900000000     C                   SETON                                        51
087000000000E   3C                   ENDIF
087100000000     C*
087200000000E   2C                   ENDIF
087300000000E   1C                   ENDIF
087400000000     C*--------------------
087500000000     C* SELEZIONE
087600000000     C*--------------------
087700000000     C                   Z-ADD     *ZEROS        DSPVCC
087800000000     C*
087900000000     C* DATA SPEDIZIONE
088000000000IF  1C     VCCDSP        IFNE      *ZEROS
088100000000     C                   Z-ADD     VCCDSP        G08DAT
088200000000     C                   Z-ADD     *ZEROS        G08INV
088300000000     C                   MOVEL     '0'           G08ERR
088400000000     C                   CALL      'XSRDA8'
088500000000     C                   PARM                    WLBDA8
088600000000IF  2C     G08ERR        IFEQ      '1'
088700000000     C                   MOVEL     '1'           $ERR
088800000000     C                   MOVEL     MSG(8)        $MSG
088900000000     C                   EXSR      CARMSG
089000000000     C                   SETON                                        83
089100000000X   2C                   ELSE
089200000000     C                   Z-ADD     G08DAT        VCCDAT
089300000000     C                   Z-ADD     G08INV        DSPVCC            8 0
089400000000E   2C                   ENDIF
089500000000E   1C                   ENDIF
089600000000     C*
089700000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
089800000000     C* CURSORE SUL CAMPO DEL CONTROL
089900000000     C     $ERR          IFEQ      '1'
090000000000     C                   MOVEL     '0'           *IN43
090100000000     C                   ELSE
090200000000     C                   MOVEL     '1'           *IN43
090300000000     C                   END
090400000000     C*
090500000000     C                   ENDSR
090600000000     C*------------------------------------------------------------------------*
090700000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
090800000000     C*------------------------------------------------------------------------*
090900000000     C     SETFIL        BEGSR
091000000000     C*
091100000000     C                   MOVEL     '1'           *IN43
091200000000     C                   SETOFF                                       98
091300000000     C*--------------------
091400000000     C* POSIZIONAMENTO X DATA ERRORE
091500000000     C*--------------------
091600000000IF  1C     WTIPRI        IFEQ      'DAT'
091700000000     C                   Z-ADD     DATVCC        KE1DAT
091800000000     C                   Z-ADD     VCCTIM        KE1TIM
091900000000     C                   Z-ADD     *ZEROS        KE1PRG
092000000000     C     KEYER1        SETGT     ECERR01L                           98
092100000000E   1C                   ENDIF
092200000000     C*--------------------
092300000000     C* POSIZIONAMENTO X BOLLA + CODICE MODIFICA
092400000000     C*--------------------
092500000000IF  1C     WTIPRI        IFEQ      'BOL'
092600000000     C                   Z-ADD     VCCAAS        KE3AAS
092700000000     C                   Z-ADD     VCCLNP        KE3LNP
092800000000     C                   Z-ADD     VCCNRS        KE3NRS
092900000000     C                   Z-ADD     VCCNSP        KE3NSP
093000000000     C                   Z-ADD     *ZEROS        KE3DAM
093100000000     C                   Z-ADD     *ZEROS        KE3ORM
093200000000     C                   Z-ADD     *ZEROS        KE3PRG
093300000000     C     KEYER3        SETGT     ECERR03L                           98
093400000000E   1C                   ENDIF
093500000000     C*
093600000000     C  N98              EXSR      LETFIL
093700000000     C*
093800000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
093900000000IF  1C     *IN98         IFEQ      '1'
094000000000     C                   Z-ADD     1             SFLEOF
094100000000E   1C                   ENDIF
094200000000     C*
094300000000     C                   ENDSR
094400000000     C*------------------------------------------------------------------------*
094500000000     C* LETFIL - LEGGE RECORD SUCESSIVO
094600000000     C*------------------------------------------------------------------------*
094700000000     C     LETFIL        BEGSR
094800000000     C*
094900000000     C* LEGGO FINO A:
095000000000     C*     - FINE FILE
095100000000     C*     - TROVATO RECORD VALIDO
095200000000     C                   MOVEL     '0'           WRECOK
095300000000DO  1C     *IN98         DOWEQ     *OFF
095400000000     C     WRECOK        ANDEQ     '0'
095500000000     C*--------------------
095600000000     C* LETTURA X DATA ERRORE
095700000000     C*--------------------
095800000000IF  1C     WTIPRI        IFEQ      'DAT'
095900000000     C                   READ      ECERR01L                               98
096000000000E   1C                   ENDIF
096100000000     C*--------------------
096200000000     C* LETTURA X BOLLA + CODICE MODIFICA
096300000000     C*--------------------
096400000000IF  1C     WTIPRI        IFEQ      'BOL'
096500000000     C                   READ      ECERR03L                               98
096600000000E   1C                   ENDIF
096700000000     C*
096800000000     C* FINE FILE  O TABELLA DIVERSA
096900000000IF  2C     *IN98         IFEQ      *ON
097000000000     C                   Z-ADD     1             SFLEOF
097100000000X   2C                   ELSE
097200000000     C                   EXSR      CHKREC
097300000000E   2C                   ENDIF
097400000000     C*
097500000000E   1C                   ENDDO
097600000000     C*
097700000000     C                   ENDSR
097800000000     C*------------------------------------------------------------------------*
097900000000     C* CHKREC - CONTROLLA VALIDITA' RECORD VOCE DI C/E
098000000000     C*------------------------------------------------------------------------*
098100000000     C     CHKREC        BEGSR
098200000000     C*
098300000000     C                   MOVEL     '1'           WRECOK
098400000000     C* CODICE ERRORE
098500000000IF  1C     VCCCER        IFNE      *BLANKS
098600000000IF  2C     ERRCER        IFNE      VCCCER
098700000000     C                   MOVEL     '0'           WRECOK
098800000000E   2C                   ENDIF
098900000000E   1C                   ENDIF
099000000000     C* DATA SPEDIZIONE
099100000000IF  1C     VCCDSP        IFNE      *ZEROS
099200040914IF  2C*    ERRDSP        IFNE      DSPVCC
099300040914IF  2C     ERRDSP        IFlt      DSPVCC
099400000000     C                   MOVEL     '0'           WRECOK
099500000000E   2C                   ENDIF
099600000000E   1C                   ENDIF
099700000000     C* PGM DI MODIFICA
099800000000IF  1C     VCCPGF        IFNE      *BLANKS
099900000000IF  2C     ERRPGF        IFNE      VCCPGF
100000000000     C                   MOVEL     '0'           WRECOK
100100000000E   2C                   ENDIF
100200000000E   1C                   ENDIF
100300000000     C* TIPO ERRORE
100400000000IF  1C     VCCSTS        IFNE      *BLANKS
100500000000IF  2C     ERRSTS        IFNE      VCCSTS
100600000000     C                   MOVEL     '0'           WRECOK
100700000000E   2C                   ENDIF
100800000000E   1C                   ENDIF
100900000000     C*
101000000000     C                   ENDSR
101100000000     C*------------------------------------------------------------------------*
101200000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
101300000000     C*------------------------------------------------------------------------*
101400000000     C     IRESFL        BEGSR
101500000000     C*
101600000000     C* CAMPI NASCOSTI
101700000103     C                   Z-ADD     ERRAAS        VSHAAS
101800000103     C                   Z-ADD     ERRDAT        VSHDAT
101900000000     C                   Z-ADD     ERRTIM        VSHTIM
102000000000     C                   Z-ADD     ERRDAM        VSHDAM
102100000000     C                   Z-ADD     ERRORM        VSHORM
102200000000     C                   Z-ADD     ERRPRG        VSHPRG
102300000000     C                   MOVEL     ERRFIL        VSHFIL
102400000107     C                   MOVEL     *BLANKS       VSCOPZ
102500000107     C*---                 MOVEL' 1'      VSCOPZ           *OPZIONE
102600000107     C*---                 SETON                     41
102700991223     C                   Z-ADD     ERRDAT        DSDAT8
102800991223     C                   Z-ADD     DSGIO8        DSGIO6
102900991223     C                   Z-ADD     DSMES8        DSMES6
103000991223     C                   Z-ADD     DSANN8        DSANN6
103100991223     C                   Z-ADD     DSDAT6        VSCDAT
103200000103     C                   Z-ADD     ERRAAS        VSCAAS
103300000000     C                   Z-ADD     ERRLNP        VSCLNP
103400000000     C                   Z-ADD     ERRNRS        VSCNRS
103500000000     C                   Z-ADD     ERRNSP        VSCNSP
103600000000     C                   MOVEL     ERRTBL        VSCTBL
103700991223     C                   Z-ADD     ERRDSP        DSDAT8
103800991223     C                   Z-ADD     DSGIO8        DSGIO6
103900991223     C                   Z-ADD     DSMES8        DSMES6
104000991223     C                   Z-ADD     DSANN8        DSANN6
104100991223     C                   Z-ADD     DSDAT6        VSCDSP
104200000000     C                   MOVEL     ERRSTS        VSCSTS
104300950508     C                   MOVEL     *BLANKS       VSCFIL
104400020619IF  1C     ERRFIL        IFEQ      'TITAS10F'
104500020619IF  1C     ERRFIL        OREQ      'TITAS00F'
104600000000     C                   MOVEL     'T'           VSCFIL
104700000000X   1C                   ELSE
104800000000IF  2C     ERRFIL        IFEQ      'TNCSB00F'
104900000000     C                   MOVEL     'C'           VSCFIL
105000000000E   2C                   ENDIF
105100030123IF  2C     ERRFIL        IFEQ      'FIAR500F'
105200030123     C                   MOVEL     'A'           VSCFIL
105300030123E   2C                   ENDIF
105400000000E   1C                   ENDIF
105500000000     C                   MOVE      ERRDER        A71              71
105600000000     C                   MOVEL     A71           VSCDER
105700000000     C                   MOVEL     *BLANKS       VSCGIR
105800000000     C*
105900000000     C* DEPOSITA NELLE CHIAVI DI CARICAMENTO SUBFILE L'ULTIMO RECORD CARICATO
106000000000IF  1C     WTIPRI        IFEQ      'DAT'
106100000000     C                   Z-ADD     ERRDAT        KE1DAT
106200000000     C                   Z-ADD     ERRTIM        KE1TIM
106300000000     C                   Z-ADD     ERRPRG        KE1PRG
106400000000X  1 C                   ELSE
106500000000     C                   Z-ADD     ERRAAS        KE3AAS
106600000000     C                   Z-ADD     ERRLNP        KE3LNP
106700000000     C                   Z-ADD     ERRNRS        KE3NRS
106800000000     C                   Z-ADD     ERRNSP        KE3NSP
106900000000     C                   Z-ADD     ERRDAM        KE3DAM
107000000000     C                   Z-ADD     ERRORM        KE3ORM
107100000000     C                   Z-ADD     ERRPRG        KE3PRG
107200000000E   1C                   ENDIF
107300000000     C*
107400000000     C                   ENDSR
107500000000     C*------------------------------------------------------------------------*
107600000000     C* GUFO - LANCIA IL PGM CHE RIMETTE LA BOLLA IL CODA PER ESSERE ELABORATA
107700000000     C*------------------------------------------------------------------------*
107800000000     C     GUFO          BEGSR
107900000000     C*
108000000000     C* LANCIA IL PROGRAMMA DI ACCODAMENTO
108100000000     C                   MOVEL     ERRSTS        D£GSTS
108200000000     C                   Z-ADD     ERRDAM        D£GDAT
108300000000     C                   Z-ADD     ERRORM        D£GTIM
108400000000     C                   MOVEL     ERRUSR        D£GUSR
108500000000     C                   MOVEL     ERRPGF        D£GPGF
108600000000     C                   MOVEL     ERRFIL        D£GFIL
108700000000     C                   MOVEL     ERRMBR        D£GMBR
108800000000     C                   MOVEL     ERRLIB        D£GLIB
108900000000     C                   MOVEL     'S'           D£GPDE
109000020731     C                   MOVEL     'I'           D£GTJB
109100000000     C                   MOVEL     *BLANKS       D£GXXX
109200000000     C                   MOVEL     ERRRP1        D£PRE1
109300000000     C                   MOVEL     ERRRP2        D£PRE2
109400000000     C                   MOVEL     ERRRD1        D£DRE1
109500000000     C                   MOVEL     ERRRD2        D£DRE2
109600000000     C*
109700020618     C                   CALL      'TRECD2R'
109800000000     C                   PARM                    DS£ECO
109900000000     C*
110000000000     C                   ENDSR
110100020923     C*-------------------------------------------------------------------------
110200020923     C* REPERISCE I DATI UTENTE
110300020923     C*-------------------------------------------------------------------------
110400020923     C     REPDATIUTE    BEGSR
110500020923     C*
110600020923     C* INIZIALIZZA VARIABILI DI WRK
110700020923     C                   CLEAR                   TIBS34DS
110800020923     C                   CLEAR                   AZUTEDS
110900020923     C                   CLEAR                   DDATIUTE
111000020923     C*
111100020923     C     *DTAARA       DEFINE    §azute        azuteds
111200020923     C     *DTAARA       DEFINE    §datiute      ddatiute
111300020923     C                   IN(E)     *DTAARA
111400020923if  1C                   IF        %Error
111500020923     c                   EVAL      I34Tla = 'L'
111600020923     C                   CALL      'TIBS34R'
111700020923     C                   PARM                    Tibs34Ds
111800020923     C                   IN        *DTAARA
111900020923e   1C                   ENDIF
112000020923     C*
112100020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
112200020923     C                   MOVEL(P)  RSUT          DSFIRS           20
112300020923     C*
112400020923     C                   ENDSR
112500000000     C*------------------------------------------------------------------------*
112600000000     C*  *INZSR - ROUTINE INIZIALE
112700000000     C*------------------------------------------------------------------------*
112800000000     C     *INZSR        BEGSR
112900000000     C*--------------------
113000000000     C* RICEVIMENTO PARAMETRI
113100000000     C*--------------------
113200000000     C     *ENTRY        PLIST
113300000000     C                   PARM                    KPJBA
113400000000     C*--------------------
113500000000     C* CAMPI RIFERITI AL DATA BASE
113600000000     C*--------------------
113700000000     C     *LIKE         DEFINE    ERRDAT        KE1DAT
113800000000     C     *LIKE         DEFINE    ERRTIM        KE1TIM
113900950329     C     *LIKE         DEFINE    ERRPRG        KE1PRG
114000000000     C     *LIKE         DEFINE    ERRAAS        KE2AAS
114100000000     C     *LIKE         DEFINE    ERRLNP        KE2LNP
114200000000     C     *LIKE         DEFINE    ERRNRS        KE2NRS
114300000000     C     *LIKE         DEFINE    ERRNSP        KE2NSP
114400000000     C     *LIKE         DEFINE    ERRTBL        KE2TBL
114500970902     C     *LIKE         DEFINE    ERRDAT        KE2DAT
114600970902     C     *LIKE         DEFINE    ERRTIM        KE2TIM
114700970902     C     *LIKE         DEFINE    ERRPRG        KE2PRG
114800000000     C     *LIKE         DEFINE    ERRAAS        KE3AAS
114900000000     C     *LIKE         DEFINE    ERRLNP        KE3LNP
115000000000     C     *LIKE         DEFINE    ERRNRS        KE3NRS
115100000000     C     *LIKE         DEFINE    ERRNSP        KE3NSP
115200000000     C     *LIKE         DEFINE    ERRDAM        KE3DAM
115300000000     C     *LIKE         DEFINE    ERRORM        KE3ORM
115400000000     C     *LIKE         DEFINE    ERRPRG        KE3PRG
115500990812     C     *LIKE         DEFINE    TASAAS        KTAAAS
115600000000     C     *LIKE         DEFINE    TASLNP        KTALNP
115700000000     C     *LIKE         DEFINE    TASNRS        KTANRS
115800000000     C     *LIKE         DEFINE    TASNSP        KTANSP
115900000000     C     *LIKE         DEFINE    TASTBL        KTATBL
116000000000     C     *LIKE         DEFINE    ERRAAS        KW3AAS
116100000000     C     *LIKE         DEFINE    ERRLNP        KW3LNP
116200000000     C     *LIKE         DEFINE    ERRNRS        KW3NRS
116300000000     C     *LIKE         DEFINE    ERRNSP        KW3NSP
116400000000     C*
116500000000     C     *LIKE         DEFINE    VCCDAT        DEPDAT
116600000000     C     *LIKE         DEFINE    VCCAAS        DEPAAS
116700000000     C     *LIKE         DEFINE    VCCLNP        DEPLNP
116800000000     C     *LIKE         DEFINE    VCCNRS        DEPNRS
116900000000     C     *LIKE         DEFINE    VCCNSP        DEPNSP
117000000000     C     *LIKE         DEFINE    VCCTBL        DEPTBL
117100000000     C     *LIKE         DEFINE    VCCCER        DEPCER
117200000000     C     *LIKE         DEFINE    VCCDSP        DEPDSP
117300000000     C     *LIKE         DEFINE    VCCPGF        DEPPGF
117400000000     C     *LIKE         DEFINE    VCCSTS        DEPSTS
117500000000     C*
117600000000     C     *LIKE         DEFINE    D87OP0        WOP0
117700000000     C     *LIKE         DEFINE    D87OP1        WOP1
117800000000     C     *LIKE         DEFINE    ERRAAS        WAAS
117900000000     C     *LIKE         DEFINE    ERRLNP        WLNP
118000000000     C     *LIKE         DEFINE    ERRNRS        WNRS
118100000000     C     *LIKE         DEFINE    ERRNSP        WNSP
118200000000     C     *LIKE         DEFINE    ERRTBL        WTBL
118300000000     C     *LIKE         DEFINE    ERRDAT        WDAT
118400000000     C     *LIKE         DEFINE    ERRTIM        WTIM
118500000000     C     *LIKE         DEFINE    ERRPRG        WPRG
118600000000     C     *LIKE         DEFINE    ERRDAM        WDAM
118700000000     C     *LIKE         DEFINE    ERRORM        WORM
118800000000     C*--------------------
118900000000     C* CHIAVI DI LETTURA
119000000000     C*--------------------
119100000000     C* ECERR01L - PARZIALE
119200000000     C     KEYER1        KLIST
119300000000     C                   KFLD                    KE1DAT
119400000000     C                   KFLD                    KE1TIM
119500000000     C                   KFLD                    KE1PRG
119600000000     C* ECERR02L - COMPLETA
119700970901     C     KE2ER2        KLIST
119800000000     C                   KFLD                    KE2AAS
119900000000     C                   KFLD                    KE2LNP
120000000000     C                   KFLD                    KE2NRS
120100000000     C                   KFLD                    KE2NSP
120200000000     C                   KFLD                    KE2TBL
120300970902     C                   KFLD                    KE2DAT
120400970902     C                   KFLD                    KE2TIM
120500970902     C                   KFLD                    KE2PRG
120600970901     C* ECERR02L - PARZIALE
120700970901     C     KEYER2        KLIST
120800970901     C                   KFLD                    KE2AAS
120900970901     C                   KFLD                    KE2LNP
121000970901     C                   KFLD                    KE2NRS
121100970901     C                   KFLD                    KE2NSP
121200970901     C                   KFLD                    KE2TBL
121300000000     C* ECERR03L - COMPLETA
121400000000     C     KEYER3        KLIST
121500000000     C                   KFLD                    KE3AAS
121600000000     C                   KFLD                    KE3LNP
121700000000     C                   KFLD                    KE3NRS
121800000000     C                   KFLD                    KE3NSP
121900000000     C                   KFLD                    KE3DAM
122000000000     C                   KFLD                    KE3ORM
122100000000     C                   KFLD                    KE3PRG
122200000000     C* ECERR03L - PARZIALE (DI LAVORO)
122300000000     C     KEYEW3        KLIST
122400000000     C                   KFLD                    KW3AAS
122500000000     C                   KFLD                    KW3LNP
122600000000     C                   KFLD                    KW3NRS
122700000000     C                   KFLD                    KW3NSP
122800990812     C* TITAS30C - COMPLETA
122900000000     C     KEYTAS        KLIST
123000000000     C                   KFLD                    KTAAAS
123100000000     C                   KFLD                    KTALNP
123200000000     C                   KFLD                    KTANRS
123300000000     C                   KFLD                    KTANSP
123400000000     C                   KFLD                    KTATBL
123500000000     C*--------------------
123600000000     C* CAMPI DI CONTROLLO
123700000000     C*--------------------
123800120130     c* visualizza manca tariffa
123900120130     c                   if        kpjbu <> ' '
124000120130     c                   movel     kpjbu         param
124100120130     c                   eval      $tges = parges
124200120130     c                   eval      VCCCER= parcer
124300120130     c                   seton                                        10
124400120130     c                   else
124500120130     c                   setoff                                       10
124600120130     c                   clear                   vcccer
124700000000     C                   MOVEL     'G'           $TGES             1
124800120130     c                   end
124900000000     C                   MOVEL     '1'           *INOF
125000000000     C                   Z-ADD     UDATE         DATA              6 0
125100000000     C                   TIME                    ORA               6 0
125200000000     C                   MOVEL     '0'           WRECOK            1
125300000000     C                   MOVEL     '1'           $PROS             1
125400000000     C*
125500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
125600000000     C                   EXSR      INZSTD
125700000000     C*
125800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
125900000000     C                   EXSR      INZSFL
126000000000     C*
126100000000     C* IMPOSTO CAMPI TESTATA VIDEO
126200000000     C                   MOVEL     DSFIRS        $RSU
126300020923     C                   Z-ADD     DUTPOU        $FIL
126400000000     C*
126500000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
126600000000     C                   Z-ADD     *ZEROS        VCCDAT
126700000000     C                   Z-ADD     *ZEROS        VCCAAS
126800000000     C                   Z-ADD     *ZEROS        VCCLNP
126900000000     C                   Z-ADD     *ZEROS        VCCNRS
127000000000     C                   Z-ADD     *ZEROS        VCCNSP
127100000000     C                   MOVEL     *BLANKS       VCCTBL
127200000000     C*
127300000000     C* EMETTO VIDEATA SUBFILE
127400000000     C                   EXSR      CHKPAR
127500000000     C                   EXSR      CARVS
127600000000     C*
127700000000     C                   ENDSR
127800000000     C*------------------------------------------------------------------------*
127900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
128000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
128100000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
128200000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
128300000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
128400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
128500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
128600000000     C*------------------------------------------------------------------------*
128700000000     C* INZSTD - ROUTINE INIZIALE STANDARD
128800000000     C*------------------------------------------------------------------------*
128900000000     C     INZSTD        BEGSR
129000000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
129100000000     C                   CALL      'XKEYS'
129200000000     C                   PARM                    DSKEYS
129300000000     C                   MOVEA     KEYRIG        KEYS
129400000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
129500000000     C     XMSGPA        PLIST
129600000000     C                   PARM                    DSMSMS
129700000000     C                   PARM                    DSMSPG
129800000000     C                   PARM                    DSMSKE
129900000000     C                   PARM                    DSMSER
130000000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
130100000000     C                   MOVEL     'TREC87R'     DSMSPG
130200000000     C                   MOVEL     'TREC87R'     VMCQUE
130300000000     C                   MOVEL     *BLANKS       VMCKEY
130400000000     C                   MOVEL     *BLANKS       DSMSMS
130500000000     C* INIZIALIZZO FLAG CONTROLLO PGM
130600000000     C                   MOVEL     '0'           $ERR              1
130700000000     C                   MOVEL     '0'           $F03              1
130800000000     C                   MOVEL     *BLANKS       $GES              2
130900000000     C                   MOVEL     *BLANKS       $MSG             80
131000000000     C                   MOVEL     '0'           $FINE             1
131100000000     C                   MOVEL     '0'           WPREC             1
131200000000     C                   MOVEL     '0'           $TESVD            1
131300000000     C* LANCIO ROUTINE PER VERIFICARE SE
131400000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
131500000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
131600020923     C                   EXSR      REPDATIUTE
131700000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
131800000000     C                   BITOFF    '01234567'    POSMI1            1
131900000000     C                   BITOFF    '01234567'    POSMI2            1
132000000000     C                   BITOFF    '01234567'    POSMA1            1
132100000000     C                   BITOFF    '01234567'    POSMA2            1
132200000000     C                   BITON     '34'          POSMI1            1
132300000000     C                   BITON     '7'           POSMI2            1
132400000000     C                   BITON     '34'          POSMA1            1
132500000000     C                   BITON     '0'           POSMA2            1
132600000000     C                   MOVE      POSMI2        POSMIN            2
132700000000     C                   MOVEL     POSMI1        POSMIN
132800000000     C                   MOVE      POSMA2        POSMAX            2
132900000000     C                   MOVEL     POSMA1        POSMAX
133000000000     C                   ENDSR
133100000000     C*------------------------------------------------------------------------*
133200000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
133300000000     C*------------------------------------------------------------------------*
133400000000     C     FUNSTD        BEGSR
133500000000     C                   SELECT
133600000000     C     KEY           WHENEQ    F3
133700000000     C                   EXSR      F03STD
133800000000     C     KEY           WHENEQ    F12
133900000000     C                   EXSR      F12STD
134000000000     C     KEY           WHENEQ    F24
134100000000     C                   EXSR      F24STD
134200000000     C                   OTHER
134300000000     C                   MOVEL     MSG(2)        $MSG
134400000000     C                   EXSR      CARMSG
134500000000     C                   ENDSL
134600000000     C                   ENDSR
134700000000     C*------------------------------------------------------------------------*
134800000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
134900000000     C*------------------------------------------------------------------------*
135000000000     C     F03STD        BEGSR
135100000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
135200000000     C                   MOVEL     '1'           $F03
135300000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
135400000000     C                   MOVEL     '1'           $FINE
135500000000     C                   ENDSR
135600000000     C*------------------------------------------------------------------------*
135700000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
135800000000     C*------------------------------------------------------------------------*
135900000000     C     F12STD        BEGSR
136000000000     C* IMPOSTO FLAG DI FIN PGM
136100000000     C                   MOVEL     '1'           $FINE
136200000000     C                   ENDSR
136300000000     C*------------------------------------------------------------------------*
136400000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
136500000000     C*------------------------------------------------------------------------*
136600000000     C     F24STD        BEGSR
136700000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
136800000000     C                   EXSR      OUTFUN
136900000000     C                   ENDSR
137000000000     C*------------------------------------------------------------------------*
137100000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
137200000000     C*------------------------------------------------------------------------*
137300000000     C     INZSFL        BEGSR
137400000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
137500000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
137600000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
137700000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
137800000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
137900000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
138000000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
138100000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
138200000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
138300000000     C                   Z-ADD     12            SFLPAG
138400000000     C                   ENDSR
138500000000     C*------------------------------------------------------------------------*
138600000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
138700000000     C*------------------------------------------------------------------------*
138800000000     C     FUNSFL        BEGSR
138900000000     C                   SELECT
139000000000     C     KEY           WHENEQ    ENTER
139100000000     C                   EXSR      ENTSFL
139200000000     C     KEY           WHENEQ    F3
139300000000     C                   EXSR      F03STD
139400000000     C     KEY           WHENEQ    F5
139500000000     C                   EXSR      F05STD
139600000000     C     KEY           WHENEQ    F12
139700000000     C                   EXSR      F12STD
139800000000     C     KEY           WHENEQ    F17
139900000000     C                   EXSR      F17STD
140000000000     C     KEY           WHENEQ    F18
140100000000     C                   EXSR      F18STD
140200000000     C     KEY           WHENEQ    F23
140300000000     C                   EXSR      F23STD
140400000000     C     KEY           WHENEQ    F24
140500000000     C                   EXSR      F24STD
140600000000     C     KEY           WHENEQ    ROLLUP
140700000000     C                   EXSR      RUPSTD
140800000000     C     KEY           WHENEQ    ROLLDN
140900000000     C                   EXSR      RDNSTD
141000000000     C                   OTHER
141100000000     C                   MOVEL     MSG(2)        $MSG
141200000000     C                   EXSR      CARMSG
141300000000     C                   ENDSL
141400000000     C                   ENDSR
141500000000     C*------------------------------------------------------------------------*
141600000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
141700000000     C*------------------------------------------------------------------------*
141800000000     C     F05STD        BEGSR
141900000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
142000000000     C                   EXSR      POSSFL
142100000000     C                   ENDSR
142200000000     C*------------------------------------------------------------------------*
142300000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
142400000000     C*------------------------------------------------------------------------*
142500000000     C     F17STD        BEGSR
142600000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
142700000000     C                   Z-ADD     1             VCCNBR
142800000000     C                   ENDSR
142900000000     C*------------------------------------------------------------------------*
143000000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
143100000000     C*------------------------------------------------------------------------*
143200000000     C     F18STD        BEGSR
143300000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
143400000000     C                   Z-ADD     SFLULT        VCCNBR
143500000000     C                   ENDSR
143600000000     C*------------------------------------------------------------------------*
143700000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
143800000000     C*------------------------------------------------------------------------*
143900000000     C     F23STD        BEGSR
144000000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
144100000000     C                   EXSR      OUTOPZ
144200000000     C                   ENDSR
144300000000     C*------------------------------------------------------------------------*
144400000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
144500000000     C*------------------------------------------------------------------------*
144600000000     C     RUPSTD        BEGSR
144700000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
144800000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
144900000000     C     CURSOR        IFGE      POSMIN
145000000000     C     CURSOR        ANDLE     POSMAX
145100000000     C                   ELSE
145200000000     C                   EXSR      CARSFL
145300000000     C                   ENDIF
145400000000     C                   ENDSR
145500000000     C*------------------------------------------------------------------------*
145600000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
145700000000     C*------------------------------------------------------------------------*
145800000000     C     RDNSTD        BEGSR
145900000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
146000000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
146100000000     C     CURSOR        IFGE      POSMIN
146200000000     C     CURSOR        ANDLE     POSMAX
146300000000     C                   ELSE
146400000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
146500000000     C                   MOVEL     '1'           $ERR
146600000000     C                   MOVEL     MSG(5)        $MSG
146700000000     C                   EXSR      CARMSG
146800000000     C                   ENDIF
146900000000     C                   ENDSR
147000000000     C*------------------------------------------------------------------------*
147100000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
147200000000     C*------------------------------------------------------------------------*
147300000000     C     ENTSFL        BEGSR
147400000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
147500000000     C                   EXSR      NEWPOS
147600000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
147700000000     C     $ERR          IFEQ      '0'
147800000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
147900000000     C     SWPOSI        IFNE      0
148000000000     C                   EXSR      POSSFL
148100000000     C                   ELSE
148200000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
148300000000     C     SFLULT        IFNE      0
148400000000     C                   EXSR      OPZSFL
148500000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
148600000000     C     SWMODI        IFNE      0
148700000000     C                   EXSR      POSSFL
148800000000     C                   ENDIF
148900000000     C                   ENDIF
149000000000     C                   ENDIF
149100000000     C                   ENDIF
149200000000     C                   ENDSR
149300000000     C*------------------------------------------------------------------------*
149400000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
149500000000     C*------------------------------------------------------------------------*
149600000000     C     POSSFL        BEGSR
149700000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
149800000000     C                   EXSR      CLRSFL
149900000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
150000000000     C                   EXSR      SETFIL
150100000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
150200000000     C     SFLEOF        IFEQ      0
150300000000     C                   EXSR      CARSFL
150400000000     C                   ENDIF
150500000000     C                   ENDSR
150600000000     C*------------------------------------------------------------------------*
150700000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
150800000000     C*------------------------------------------------------------------------*
150900000000     C     OPZSFL        BEGSR
151000000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
151100000000     C                   MOVEL     'S'           OPZOK             1
151200000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
151300000000     C                   Z-ADD     0             SFLVAR            5 0
151400000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
151500000000     C                   Z-ADD     0             SFLERR
151600000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
151700000000     C                   Z-ADD     SFLULT        WFLULT
151800000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
151900000000     C                   READC     EC87DVS                                49
152000000000     C     *IN49         DOWEQ     '0'
152100000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
152200000000     C                   ADD       1             SFLVAR
152300000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
152400000000     C     OPZOK         IFEQ      'S'
152500000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
152600000000     C     VSCOPZ        IFNE      *BLANKS
152700000000     C                   EXSR      CONOPZ
152800000000     C     OPZOK         IFEQ      'S'
152900000000     C                   EXSR      GESOPZ
153000000000     C                   ENDIF
153100000000     C                   ENDIF
153200000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
153300000000     C*            2. PULIZIA SCELTA IN SFL
153400000000     C     OPZOK         IFEQ      'S'
153500000000     C                   MOVEL     '0'           *IN41
153600000000     C                   MOVEL     *BLANKS       VSCOPZ
153700000000     C                   ELSE
153800000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
153900000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
154000000000     C                   MOVEL     '1'           *IN41
154100000000     C                   Z-ADD     SFLULT        SFLERR
154200000000     C                   ENDIF
154300000000     C                   ELSE
154400000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
154500000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
154600000000     C                   MOVEL     '1'           *IN41
154700000000     C                   ENDIF
154800000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
154900000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
155000000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
155100000000     C     OPZOK         IFNE      'S'
155200000000     C                   Z-ADD     SFLERR        VCCNBR
155300000000     C                   ELSE
155400000000     C                   Z-ADD     SFLULT        VCCNBR
155500000000     C                   ENDIF
155600000000     C* AGGIORNO RECORD SFL
155700000000     C                   UPDATE    EC87DVS
155800000000     C                   READC     EC87DVS                                49
155900000000     C                   ENDDO
156000000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
156100000000     C                   Z-ADD     WFLULT        SFLULT
156200000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
156300000000     C     SFLVAR        IFEQ      0
156400000000     C                   MOVEL     '1'           $FINE
156500000000     C                   ENDIF
156600000000     C                   ENDSR
156700000000     C*------------------------------------------------------------------------*
156800000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
156900000000     C*------------------------------------------------------------------------*
157000000000     C     CLRSFL        BEGSR
157100000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
157200000000     C* (0=NON FINE FILE  1=FINE FILE)
157300000000     C                   Z-ADD     0             SFLEOF
157400000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
157500000000     C                   Z-ADD     0             SFLULT
157600000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
157700000000     C                   MOVEL     '1'           *IN40
157800000000     C                   WRITE     EC87DVC
157900000000     C                   MOVEL     '0'           *IN40
158000000000     C                   ENDSR
158100000000     C*------------------------------------------------------------------------*
158200000000     C* CARSFL - CARICAMENTO SFL
158300000000     C*------------------------------------------------------------------------*
158400000000     C     CARSFL        BEGSR
158500000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
158600000000     C     SFLULT        IFEQ      0
158700000000     C                   EXSR      SETFIL
158800000000     C                   ENDIF
158900000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
159000000000     C                   Z-ADD     SFLULT        WFLULT
159100000000     C* CICLO:
159200000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
159300000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
159400000000     C                   Z-ADD     1             SFLRIG
159500970901     C     SFLRIG        DOWLE     SFLPAG
159600970901     C     SFLEOF        ANDEQ     0
159700000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
159800000000     C                   MOVEL     *BLANKS       VSCOPZ
159900000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
160000000000     C                   EXSR      IRESFL
160100000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
160200000000     C                   ADD       1             SFLULT
160300000000     C* SCRITTURA RECORD SFL
160400000000     C                   WRITE     EC87DVS
160500000000     C                   ADD       1             SFLRIG
160600000000     C* LETTURA FILE DA CARICARE
160700000000     C                   EXSR      LETFIL
160800000000     C                   ENDDO
160900000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
161000000000     C     WFLULT        IFNE      SFLULT
161100000000     C                   Z-ADD     WFLULT        VCCNBR
161200000000     C                   ADD       1             VCCNBR
161300000000     C                   ENDIF
161400000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
161500000000     C     SFLEOF        IFEQ      1
161600000000     C     SFLULT        ANDGT     *ZEROS
161700000000     C                   MOVEL     '1'           $ERR
161800000000     C                   MOVEL     MSG(6)        $MSG
161900000000     C                   EXSR      CARMSG
162000000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
162100000000     C     SFLULT        CHAIN     EC87DVS                            99
162200000000     C     *IN99         IFEQ      *ON
162300000000     C                   SETON                                        45
162400000000     C                   UPDATE    EC87DVS
162500000000     C                   SETOFF                                       45
162600000000     C                   ENDIF
162700000000     C                   ENDIF
162800000000     C                   ENDSR
162900000000     C*------------------------------------------------------------------------*
163000000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
163100000000     C*------------------------------------------------------------------------*
163200000000     C     OUTSFL        BEGSR
163300000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
163400000000     C     MSGULT        IFNE      *ZEROS
163500000000     C                   EXSR      OUTMSG
163600000000     C                   ENDIF
163700000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
163800000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
163900000000     C     SFLEOF        IFEQ      0
164000000000     C                   MOVEL     '0'           *IN42
164100000000     C                   ELSE
164200000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
164300000000     C                   MOVEL     '1'           *IN42
164400000000     C                   ENDIF
164500000000     C* EMISSIONE SUBFILE
164600000000     C     SFLULT        IFLE      0
164700000000     C*  --> CASO DI SFL VUOTO
164800000000     C                   MOVEL     '0'           *IN43
164900000000     C                   WRITE     EC87DVV
165000000000     C                   WRITE     EC87DVC
165100000000     C                   READ      EC87DVC                                99
165200000000     C                   ELSE
165300000000     C*  --> CASO DI SFL CON RECORDS
165400000000     C                   WRITE     EC87DVC
165500000000     C                   READ      EC87DVC                                99
165600000000     C                   ENDIF
165700000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
165800000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
165900000000     C                   Z-ADD     FRTSFL        VCCNBR
166000000000     C                   ENDSR
166100000000     C*------------------------------------------------------------------------*
166200000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
166300000000     C*          CODA MESSAGGI DEL PGM
166400000000     C*------------------------------------------------------------------------*
166500000000     C     CARMSG        BEGSR
166600000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
166700000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
166800000000     C     MSGULT        IFEQ      0
166900000000     C                   MOVEL     '1'           *IN40
167000000000     C                   WRITE     EC87DVK
167100000000     C                   MOVEL     '0'           *IN40
167200000000     C                   ENDIF
167300000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
167400000000     C                   MOVEL     $MSG          DSMSMS
167500000000     C                   CALL      'XMSG'        XMSGPA
167600000000     C     DSMSER        IFEQ      0
167700000000     C                   MOVEL     DSMSKE        VMCKEY
167800000000     C                   ENDIF
167900000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
168000000000     C                   ADD       1             MSGULT            4 0
168100000000     C                   WRITE     EC87DVM
168200000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
168300000000     C                   MOVEL     *BLANKS       $MSG
168400000000     C                   ENDSR
168500000000     C*------------------------------------------------------------------------*
168600000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
168700000000     C*------------------------------------------------------------------------*
168800000000     C     OUTMSG        BEGSR
168900000000     C* VISUALIZZAZIONE MESSAGGIO
169000000000     C                   MOVEL     '1'           *IN44
169100000000     C                   WRITE     EC87DVK
169200000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
169300000000     C                   Z-ADD     *ZEROS        MSGULT
169400000000     C                   ENDSR
169500000000     C*------------------------------------------------------------------------*
169600000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
169700000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
169800000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
169900000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
170000000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
170100000000     C*------------------------------------------------------------------------*
170200000000     C     CARFUN        BEGSR
170300000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
170400000000     C     1             DO        31            I                 5 0
170500000000     C                   MOVEL     FUN(I)        FUNRIG
170600000000     C                   MOVEL     FUNFUN        FFU(I)
170700000000     C                   MOVEL     FUNDES        FDE(I)
170800020923     C     DUTLPO        IFEQ      'S'
170900000000     C     $TGES         IFEQ      'G'
171000000000     C                   MOVEL     FUNASG        FAT(I)
171100000000     C                   ELSE
171200000000     C                   MOVEL     FUNASI        FAT(I)
171300000000     C                   ENDIF
171400000000     C                   ELSE
171500000000     C     $TGES         IFEQ      'G'
171600000000     C                   MOVEL     FUNAFG        FAT(I)
171700000000     C                   ELSE
171800000000     C                   MOVEL     FUNAFI        FAT(I)
171900000000     C                   ENDIF
172000000000     C                   ENDIF
172100000000     C     FAT(I)        IFEQ      'E'
172200000000     C                   MOVEL     $USER         WEDP              3
172300000000     C     WEDP          IFEQ      'EDP'
172400000000     C                   MOVEL     'S'           FAT(I)
172500000000     C                   ELSE
172600000000     C                   MOVEL     'N'           FAT(I)
172700000000     C                   ENDIF
172800000000     C                   ENDIF
172900000000     C                   ENDDO
173000000000     C                   ENDSR
173100000000     C*------------------------------------------------------------------------*
173200000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
173300000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
173400000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
173500000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
173600000000     C*------------------------------------------------------------------------*
173700000000     C     OUTFUN        BEGSR
173800000000     C     1             DO        8             I
173900000000     C                   MOVEL     *BLANKS       FUV(I)
174000000000     C                   ENDDO
174100000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
174200000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
174300000000     C     1             DO        31            I
174400000000     C     FAT(I)        IFEQ      'S'
174500000000     C     FDE(I)        ANDNE     *BLANKS
174600000000     C                   ADD       1             FUNNAT
174700000000     C                   ENDIF
174800000000     C                   ENDDO
174900000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
175000000000     C     FUNNAT        IFGT      8
175100000000     C                   MOVEL     'S'           FAT(24)
175200000000     C                   ENDIF
175300000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
175400000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
175500000000     C                   Z-ADD     8             FUNVIS            5 0
175600000000     C     FAT(23)       IFEQ      'S'
175700000000     C                   SUB       1             FUNVIS
175800000000     C                   ENDIF
175900000000     C     FAT(24)       IFEQ      'S'
176000000000     C                   SUB       1             FUNVIS
176100000000     C                   ENDIF
176200000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
176300000000     C                   Z-ADD     *ZEROS        W                 5 0
176400000000     C                   ADD       1             FUNULT
176500000000     C     FUNULT        IFGT      31
176600000000     C                   Z-ADD     1             FUNULT
176700000000     C                   ENDIF
176800000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
176900000000     C     FUNULT        DO        31            I
177000000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
177100000000     C     W             IFLT      FUNVIS
177200000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
177300000000     C     FAT(I)        IFEQ      'S'
177400000000     C     FDE(I)        ANDNE     *BLANKS
177500000000     C     I             IFEQ      23
177600000000     C     I             OREQ      24
177700000000     C                   ELSE
177800000000     C                   ADD       1             W
177900000000     C                   MOVEL     FDE(I)        FUV(W)
178000000000     C                   ENDIF
178100000000     C                   ENDIF
178200000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
178300000000     C                   Z-ADD     I             FUNULT
178400000000     C                   ENDIF
178500000000     C                   ENDDO
178600000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
178700000000     C                   MOVEL     FUV(1)        VFD001
178800000000     C                   MOVEL     FUV(2)        VFD002
178900000000     C                   MOVEL     FUV(3)        VFD003
179000000000     C                   MOVEL     FUV(4)        VFD004
179100000000     C                   MOVEL     FUV(5)        VFD005
179200000000     C                   MOVEL     FUV(6)        VFD006
179300000000     C     FAT(23)       IFEQ      'N'
179400000000     C     FAT(24)       ANDEQ     'N'
179500000000     C                   MOVEL     FUV(7)        VFD007
179600000000     C                   MOVEL     FUV(8)        VFD008
179700000000     C                   ELSE
179800000000     C     FAT(23)       IFEQ      'S'
179900000000     C     FAT(24)       ANDEQ     'S'
180000000000     C                   MOVEL     FDE(23)       VFD007
180100000000     C                   MOVEL     FDE(24)       VFD008
180200000000     C                   ELSE
180300000000     C                   MOVEL     FUV(7)        VFD007
180400000000     C     FAT(23)       IFEQ      'S'
180500000000     C                   MOVEL     FDE(23)       VFD008
180600000000     C                   ELSE
180700000000     C                   MOVEL     FDE(24)       VFD008
180800000000     C                   ENDIF
180900000000     C                   ENDIF
181000000000     C                   ENDIF
181100000000     C                   MOVEL     '1'           $TESVD
181200000000     C                   ENDSR
181300000000     C*------------------------------------------------------------------------*
181400000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
181500000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
181600000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
181700000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
181800000000     C*------------------------------------------------------------------------*
181900000000     C     CONFUN        BEGSR
182000000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
182100000000     C     1             DO        31            I
182200000000     C     KEY           IFEQ      KEYS(I)
182300000000     C* CONTROLLO SE IL TASTO E' ATTIVO
182400000000     C     FAT(I)        IFNE      'S'
182500000000     C                   MOVEL     MSG(1)        $MSG
182600000000     C                   EXSR      CARMSG
182700000000     C                   MOVEL     '1'           $ERR
182800000000     C                   ENDIF
182900000000     C                   ENDIF
183000000000     C                   ENDDO
183100000000     C                   ENDSR
183200000000     C*------------------------------------------------------------------------*
183300000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
183400000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
183500000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
183600000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
183700000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
183800000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
183900000000     C*------------------------------------------------------------------------*
184000000000     C     CAROPZ        BEGSR
184100000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
184200000000     C     1             DO        24            I
184300000000     C                   MOVEL     OPZ(I)        OPZRIG
184400000000     C                   MOVEL     OPZOPD        OPD(I)
184500000000     C                   MOVEL     OPZOPS        OPS(I)
184600000000     C                   MOVEL     OPZDES        ODE(I)
184700020923     C     DUTLPO        IFEQ      'S'
184800000000     C     $TGES         IFEQ      'G'
184900000000     C                   MOVEL     OPZASG        OAT(I)
185000000000     C                   ELSE
185100000000     C                   MOVEL     OPZASI        OAT(I)
185200000000     C                   ENDIF
185300000000     C                   ELSE
185400000000     C     $TGES         IFEQ      'G'
185500000000     C                   MOVEL     OPZAFG        OAT(I)
185600000000     C                   ELSE
185700000000     C                   MOVEL     OPZAFI        OAT(I)
185800000000     C                   ENDIF
185900000000     C                   ENDIF
186000000000     C     OAT(I)        IFEQ      'E'
186100000000     C                   MOVEL     $USER         WEDP              3
186200000000     C     WEDP          IFEQ      'EDP'
186300000000     C                   MOVEL     'S'           OAT(I)
186400000000     C                   ELSE
186500000000     C                   MOVEL     'N'           OAT(I)
186600000000     C                   ENDIF
186700000000     C                   ENDIF
186800000000     C                   ENDDO
186900000000     C                   ENDSR
187000000000     C*------------------------------------------------------------------------*
187100000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
187200000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
187300000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
187400000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
187500000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
187600000000     C*------------------------------------------------------------------------*
187700000000     C     OUTOPZ        BEGSR
187800000000     C     1             DO        8             I
187900000000     C                   MOVEL     *BLANKS       OPV(I)
188000000000     C                   ENDDO
188100000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
188200000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
188300000000     C                   ADD       1             OPZULT
188400000000     C     OPZULT        IFGT      24
188500000000     C                   Z-ADD     1             OPZULT
188600000000     C                   END
188700000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
188800000000     C     1             DO        24            I
188900000000     C     OAT(I)        IFEQ      'S'
189000000000     C     ODE(I)        ANDNE     *BLANKS
189100000000     C                   ADD       1             OPZNAT
189200000000     C                   ENDIF
189300000000     C                   ENDDO
189400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
189500000000     C                   Z-ADD     0             W
189600000000     C     OPZULT        DO        24            I
189700000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
189800000000     C     W             IFLT      8
189900000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
190000000000     C     OAT(I)        IFEQ      'S'
190100000000     C     ODE(I)        ANDNE     *BLANKS
190200000000     C                   ADD       1             W
190300000000     C                   MOVEL     ODE(I)        OPV(W)
190400000000     C                   ENDIF
190500000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
190600000000     C                   Z-ADD     I             OPZULT
190700000000     C                   ENDIF
190800000000     C                   ENDDO
190900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
191000000000     C                   MOVEL     OPV(1)        VOD001
191100000000     C                   MOVEL     OPV(2)        VOD002
191200000000     C                   MOVEL     OPV(3)        VOD003
191300000000     C                   MOVEL     OPV(4)        VOD004
191400000000     C                   MOVEL     OPV(5)        VOD005
191500000000     C                   MOVEL     OPV(6)        VOD006
191600000000     C                   MOVEL     OPV(7)        VOD007
191700000000     C                   MOVEL     OPV(8)        VOD008
191800000000     C                   MOVEL     '1'           $TESVD
191900000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
192000000000     C     OPZNAT        IFGT      8
192100000000     C                   MOVEL     'S'           FAT(23)
192200000000     C                   ENDIF
192300000000     C                   ENDSR
192400000000     C*------------------------------------------------------------------------*
192500000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
192600000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
192700000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
192800000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
192900000000     C*------------------------------------------------------------------------*
193000000000     C     CONOPZ        BEGSR
193100000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
193200000000     C                   MOVEL     *BLANKS       OPZOK             1
193300000000     C     1             DO        24            I                 5 0
193400000000     C     VSCOPZ        IFEQ      OPD(I)
193500000000     C     VSCOPZ        OREQ      OPS(I)
193600000000     C     OAT(I)        IFEQ      'S'
193700000000     C                   MOVEL     OPD(I)        VSCOPZ
193800000000     C                   MOVEL     'S'           OPZOK
193900000000     C                   ELSE
194000000000     C                   MOVEL     'N'           OPZOK             1
194100000000     C                   ENDIF
194200000000     C                   ENDIF
194300000000     C                   ENDDO
194400000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
194500000000     C     OPZOK         IFNE      'S'
194600000000     C                   MOVEL     MSG(3)        $MSG
194700000000     C                   EXSR      CARMSG
194800000000     C                   MOVEL     '1'           $ERR
194900000000     C                   MOVEL     'N'           OPZOK             1
195000000000     C                   ENDIF
195100000000     C                   ENDSR
195200000000     C*------------------------------------------------------------------------*
195300000000     C* ROUTINE GENERALIZZATA PASSAGGIO IMMAGINI BOLLA DI SEDE
195400000000     C*------------------------------------------------------------------------*
195500000000      /COPY GAITRASRC/SRCTREC,TRECDT
195600000000     O*------------------------------------------------------------------------*
195700000000     O* AGGIORNAMENTO RECORD ERRORE CON L'IMMAGINE -PRIMA- E -DOPO-
195800000000     O*------------------------------------------------------------------------*
195900970901     OECERR2    E            AGGERR
196000000000     O                       ERRRP1
196100000000     O                       ERRRP2
196200000000     O                       ERRRD1
196300000000     O                       ERRRD2
196400000000** MSG - MESSAGGI -------------------------------------------------------------*
196500000000TREC87R- Tasto funzionale non valido.                                          1
196600000000TREC87R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
196700000000TREC87R- Opzione non valida.                                                   3
196800000000TREC87R- Opzione abilitata ma non gestita. Avvertire CED.                      4
196900000000TREC87R- Inizio elenco.                                                        5
197000000000TREC87R- Fine elenco.                                                          6
197100000000TREC87R- Accettato solo un parametro di inizio elenco.                         7
197200000000TREC87R- Data errata                                                           8
197300000000TREC87R- Si possono variare solo errori di inserimento (I) e di TNCSB (C)      9
197400000000TREC87R- Cancellazione effettuata                                             10
197500000000TREC87R- C/Economico rilanciato                                               11
197600000000TREC87R- C/Econ non lanciato perchè questo non è il primo errore della bolla  12
197700040624TREC87R- Non trovata bolla
197800000000TREC87R- Errore non cancellato perchè non è il primo della bolla              14
197900020607TREC87R- 15
198000000000TREC87R- 16
198100000000TREC87R- Errore non presente in archivio: F05 per avere la situazione aggior. 17
198200020607TREC87R- 18
198300000000TREC87R- 19
198400000000TREC87R- 20
198500000000** OPZ - OPZIONI          SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.) -----------*
198600000000- 1-1 -1=Lancio C/E       -    SN     SN
198700000000- 2-2 -2=Modifica TNCSB   -    SN     SN
198800000000- 3-3 -                   -    NN     NN
198900000000- 4-4 -4=Cancellazione    -    SN     SN
199000950519- 5-5 -5=Visualizza       -    SS     SS
199100020610- 6-6 -6=Stampa bolla     -    SS     SS
199200080908- 7-7 -7=Inter. bolla     -    SS     NN
199300000000- 8-8 -                   -    NN     NN
199400000000- 9-9 -                   -    NN     NN
199500000000-10-10-                   -    NN     NN
199600000000-11-  -                   -    NN     NN
199700000000-12-  -                   -    NN     NN
199800000000-13-  -                   -    NN     NN
199900000000-14-  -                   -    NN     NN
200000000000-15-  -                   -    NN     NN
200100000000-16-  -                   -    NN     NN
200200000000-17-  -                   -    NN     NN
200300000000-18-  -                   -    NN     NN
200400000000-19-  -                   -    NN     NN
200500000000-20-  -                   -    NN     NN
200600000000-21-  -                   -    NN     NN
200700000000-22-  -                   -    NN     NN
200800000000-23-  -                   -    NN     NN
200900000000-24-  -                   -    NN     NN
201000000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.) -----------*
201100000000F1    -                   -    NN     NN
201200000000F2    -                   -    NN     NN
201300000000F3    -F3=Fine            -    SS     SS
201400000000F4    -                   -    NN     NN
201500000000F5    -F5=Rivisualizza    -    SS     SS
201600000000F6    -                   -    NN     NN
201700000000F7    -                   -    NN     NN
201800000000F8    -                   -    NN     NN
201900000000F9    -                   -    NN     NN
202000000000F10   -                   -    NN     NN
202100000000F11   -                   -    NN     NN
202200000000F12   -F12=Ritorno        -    SS     SS
202300000000F13   -                   -    NN     NN
202400000000F14   -                   -    NN     NN
202500000000F15   -                   -    NN     NN
202600000000F16   -                   -    NN     NN
202700000000F17   -F17=Inizio dati    -    SS     SS
202800000000F18   -F18=Fine dati      -    SS     SS
202900000000F19   -                   -    NN     NN
203000000000F20   -                   -    NN     NN
203100000000F21   -                   -    NN     NN
203200000000F22                       -    NN     NN
203300000000F23   -F23=Altre opzioni  -    NN     NN
203400000000F24   -F24=Altri tasti    -    NN     NN
203500000000ENTER -                   -    SS     SS
203600000000ROLLUP-                   -    SS     SS
203700000000ROLLDN-                   -    SS     SS
203800000000HELP  -                   -    NN     NN
203900000000PRINT -                   -    NN     NN
204000000000BSPACE-                   -    NN     NN
204100000000CLEAR -                   -    NN     NN
