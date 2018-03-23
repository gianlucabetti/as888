000100970526     H*------------------------------------------------------------------------*
000200021217     H* Gestione file movimenti Gabbie e Bancali (AZMGB00F)
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800021217     FTISIG3D   CF   E             WORKSTN INFDS(DEVDS1)
000900021217     F                                     SFILE(SIG3D05:SFLULT)
001000021217     F                                     SFILE(SIG3DMS:MSGULT)
001100970526     F*------------
001200030325     Fcnaco00f  IF   E           K DISK
001300021230     Fazmgb01l  IF   E           K DISK
001400021217     Fazdst04l  IF   E           K DISK
001500021217     Fazagb01l  IF   E           K DISK
001600021217     Fazorg01l  IF   E           K DISK
001700030123     Ftntbe01l  IF   E           K DISK
001800970526     D*------------------------------------------------------------------------*
001900970526     D* SCHIERE
002000970526     D*------------------------------------------------------------------------*
002100970526     D*------------
002200970526     D* DECODIFICHE
002300970526     D*------------
002400970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002500970526     D*------------
002600970526     D* MESSAGGI
002700970526     D*------------
002800970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002900970528     D*------------
003000970528     D* COMANDI
003100970528     D*------------
003200970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003300970526     D*------------
003400970526     D* DESCRIZIONI OPZIONI
003500970526     D*------------
003600970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003700970526     D*------------
003800970526     D* DESCRIZIONI TASTI FUNZIONALI
003900970526     D*------------
004000970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004100970526     D*------------
004200970526     D* OPZIONI DA CARICARE A VIDEO
004300970526     D*------------
004400970526     D OPV             S             19    DIM(24)
004500970526     D*------------
004600970526     D* FUNZIONI DA CARICARE A VIDEO
004700970526     D*------------
004800970527     D FUV             S             19    DIM(24)
004900991108     D*------------
005000970526     D*------------------------------------------------------------------------*
005100970526     D* INPUT
005200970526     D*------------------------------------------------------------------------*
005300970526     D*------------
005400970526     D* RIDEFINIZIONE OPZIONI VIDEO
005500970526     D*------------
005600970526     D OPDDS           DS
005700970526     D  OPDOPZ                 1      2
005800970526     D  OPDDE1                 3     19
005900970526     D  OPDOP1                21     22
006000970526     D  OPDFI0                23     34
006100970526     D  OPDATT                35     35
006200970526     D  OPDFI1                36     80
006300970526     D  OPDDES                 1     19
006400970526     D  OPDRIG                 1     80
006500970526     D*------------
006600970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006700970526     D*------------
006800970526     D FUDDS           DS
006900970526     D  FUDDE1                 1      1
007000970526     D  FUDFUN                 2      3
007100970526     D  FUDDE2                 4     19
007200970526     D  FUDFI0                20     34
007300970526     D  FUDATT                35     35
007400970526     D  FUDFI1                36     80
007500970526     D  FUDDES                 1     19
007600970526     D  FUDRIG                 1     80
007700970526     D*------------
007800970526     D* DS GESTIONE TASTI FUNZIONALI
007900970526     D*------------
008000970526     D DSKEY         E DS
008100970526     D DEVDS1          DS
008200970526     D  KEY                  369    369
008300970526     D*------------
008400970526     D* DS "XMSG" - GESTIONE MESSAGGI
008500970526     D*------------
008600970526     D DSMSG         E DS
008700970526     D*------------
008800020917     D* DS REPERIMENTO DATI UTENTE
008900970526     D*------------
009000020917     D TIBS34DS      E DS                                                       *Profili utente
009100020917     D DDATIUTE      E DS                                                       *Dati utente
009200020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009300150212     D dute01        E DS                                                       *Utenti
009400150212     d dLat          e ds
009500150212     d TIBS02DS      E DS
009600030704     D*------------------
009700030704     D* DS CARICAMENTO P.O. GESTITI
009800030704     D*-------------------
009900030704     D TRUL31DS      E DS                  INZ
010000150212     D TRUL31DS2     E DS
010100030704     D*-------------------
010200030704     D* FILIALI DEI CLIENTI DA VISUALIZZARE
010300030704     D*-------------------
010400030704     D L1              S              3    DIM(250) INZ(*zeros)
010500970526     D*------------
010600970526     D* ARCHITETTURA
010700970526     D*------------
010800970526     D KPJBA         E DS
010900021217     D*------------
011000021217     D* TABELLE "XCLIR"- RICERCA CLIENTI
011100021217     D*------------
011200021219     D KSC             S              7  0 dim(30)
011300021219     D KSA             S              4    dim(30)
011400021217     D*------------------
011500021217     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
011600021217     D*------------------
011700021219     D DSXC          E DS                  extname(XCLIRDS)
011800021217     D*------------------
011900021217     D                 DS
012000021217     D DSKSN                   1      4p 0
012100021217     D DSKSA                   1      4
012200021217     D*------------------
012300021217     D* DS REPERIMENTO DATI UTENTE
012400021217     D*-------------------
012500021219     D BS69DS        E DS                  EXTNAME(TIBS69DS)
012600021219     D ACODS         E DS                  EXTNAME(CNACO00F)
012700021219     D INDDS         E DS                  EXTNAME(CNIND00F)
012800021219     D CLPDS         E DS                  EXTNAME(CNCLP00F)
012900021219     D CLSDS         E DS                  EXTNAME(FNCLS00F)
013000030128     D*------------------
013100030128     D* DS CONTROLLO DATA (8)
013200030128     D*------------------
013300030128     D WLBDA8          DS                  INZ
013400030128     D  G08DAT                 1      8  0
013500030128     D  G08INV                 9     16  0
013600030128     D  G08ERR                17     17
013700030128     D  G08TGI                18     22  0
013800021211     D*------------------
013900021211     D* PASSAGGIO PARAMETRI
014000021211     D*------------------
014100021211     D PARAM           DS                  INZ
014200021211     D  POP1                   1      3
014300030122     D  PPRG                   4     18  0
014400030122     D  PMSG                  19     98
014500030123     D PARAM2          DS                  INZ
014600030123     D  P2OP1                  1      3
014700030123     D  P2CAU                  4      7
014800030123     D  P2DES                  8     42
014900030123     D  P2MSG                 43    122
015000970526     C*------------------------------------------------------------------------*
015100970526     C* MAIN LINES
015200970526     C*------------------------------------------------------------------------*
015300970526     C*
015400970526     C* SE NON CI SONO ERRORI, PROSEGUE
015500970526IF  1C     ERRL00        IFNE      '1'
015600970526     C*
015700970526     C* CARICA IL SFILE
015800970526     C                   EXSR      POSSFL
015900970526     C*
016000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
016100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
016200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
016300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
016400970526     C*
016500970526     C* REIMPOSTA ERRORE DI PROCERURA
016600970526     C                   MOVEL     '0'           ERRL00
016700970526     C*
016800970526     C* EMETTE IL SFILE
016900970526     C                   EXSR      OUTSFL
017000970526     C*
017100970526     C* GESTIONE TASTI FUNZIONALI
017200970526     C                   EXSR      FUNCON
017300970526     C*
017400970526E   2C                   ENDDO
017500970527     C*
017600970526E   1C                   ENDIF
017700991108     C*
017800970526     C* FINE PGM
017900970526     C                   SETON                                        LR
018000970526     C*------------------------------------------------------------------------*
018100970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
018200970526     C*------------------------------------------------------------------------*
018300970526     C     F03GES        BEGSR
018400970526     C*
018500970526     C* IMPOSTA "PREMUTO F03"
018600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018700970526     C*
018800970526     C                   ENDSR
018900970526     C*------------------------------------------------------------------------*
019000970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
019100970526     C*------------------------------------------------------------------------*
019200970526     C     F05GES        BEGSR
019300970526     C*
019400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019500970526     C*
019600970526     C                   ENDSR
019700970526     C*------------------------------------------------------------------------*
019800970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
019900970526     C*------------------------------------------------------------------------*
020000000407     C     F06GES        BEGSR
020100970526     C*
020200101006     ***C* OPERAZIONE CONSENTITO SOLO IN SEDE
020300101006     ***   *-=»> NON PIÙ: ORA CONSENTE F6=INSERIMENTO <«=-*
020400101006     ***C     SIMFEL        IFNE      *zeros
020500101006     ***C                   MOVEL     '1'           ERRL00
020600101006     ***C                   MOVEL     MSG(3)        DSMSMS
020700101006     ***C                   ENDIF
020800000407     C*
020900021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
021000030122     C                   Z-ADD     *zeros        PPRG
021100000407     C                   MOVEL     'F06'         POP1
021200000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
021300021217     C                   CALL      'TISIG4R'
021400000407     C                   PARM                    KPJBA
021500021211     C                   MOVEL     KPJBU         PARAM
021600000407     C                   EXSR      POSSFL                                       *RICARICA SFILE
021700021211     C                   ENDIF
021800000407     C*
021900970526     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100991108     C* F12GES - GESTIONE F12 -> RITORNO
022200970526     C*------------------------------------------------------------------------*
022300970526     C     F12GES        BEGSR
022400970526     C*
022500970526     C* IMPOSTA "PREMUTO F12"
022600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022700970526     C*
022800970526     C                   ENDSR
022900970528     C*------------------------------------------------------------------------*
023000970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
023100970528     C*------------------------------------------------------------------------*
023200970528     C     F21GES        BEGSR
023300970612     C*
023400991117     C* NON GESTITA
023500970528     C*
023600970528     C                   ENDSR
023700970528     C*------------------------------------------------------------------------*
023800970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
023900970528     C*------------------------------------------------------------------------*
024000970528     C     F22GES        BEGSR
024100970528     C*
024200021211     C* NON GESTITA
024300970528     C*
024400970528     C                   ENDSR
024500970526     C*------------------------------------------------------------------------*
024600970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
024700970526     C*------------------------------------------------------------------------*
024800970526     C     F23GES        BEGSR
024900970526     C*
025000970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
025100970526     C*
025200970526     C                   ENDSR
025300970526     C*------------------------------------------------------------------------*
025400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025500970526     C*------------------------------------------------------------------------*
025600970526     C     F24GES        BEGSR
025700970526     C*
025800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025900970526     C*
026000970526     C                   ENDSR
026100970526     C*------------------------------------------------------------------------*
026200970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
026300970526     C*------------------------------------------------------------------------*
026400970526     C     RUPGES        BEGSR
026500970526     C*
026600970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
026700970526     C*
026800970526     C                   ENDSR
026900970526     C*------------------------------------------------------------------------*
027000991108     C* ENTGES - GESTIONE ENTER -> INVIO
027100970526     C*------------------------------------------------------------------------*
027200970526     C     ENTGES        BEGSR
027300000407     C*
027400000407     C                   MOVEL     '0'           ERRL00
027500991108     C*
027600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
027700970526     C                   EXSR      NEWPOS
027800021211     C*
027900021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
028000021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028100021211     C     SWPOSI        IFNE      0
028200021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
028300021211     C                   ELSE
028400021211     C     SFLULT        IFNE      0
028500021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
028600021211     C*
028700021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
028800021211     C     SWMODI        IFNE      0
028900021211     C                   EXSR      POSSFL
029000021211     C                   ENDIF
029100021211     C                   ENDIF
029200021211     C                   ENDIF
029300021211     C                   ENDIF
029400021211     C*
029500021211     C                   ENDSR
029600160727     C*------------------------------------------------------------------------*
029700160727     C* O04GES - GESTIONE OPZIONE 4=Cancellazione   RECORD
029800160727     C*------------------------------------------------------------------------*
029900160727     C     O04GES        BEGSR
030000160727     C*
030100160727     C* ESEGUO ROUTINE DI CONTROLLO
030200160727     C                   EXSR      DECODI
030300160727     C*
030400160727     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030500160727     C                   MOVEL     'O04'         POP1
030600160727     C                   Z-ADD     VIHPRG        PPRG
030700160727     C                   EXSR      PASPAR
030800160727     C                   CALL      'TISIG4R'
030900160727     C                   PARM                    KPJBA
031000160727     c                   else
031100160727      * se errore sul cliente forzo opzione non ok così gestico la forzatura
031200160727     C                   Z-ADD     1             OPZOK
031300160727     C                   ENDIF
031400160803     C                   z-add     1             swmodi
031500160727     C*
031600160727     C                   ENDSR
031700991014     C*------------------------------------------------------------------------*
031800000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
031900970526     C*------------------------------------------------------------------------*
032000970526     C     O05GES        BEGSR
032100000407     C*
032200030704     C* ESEGUO ROUTINE DI CONTROLLO
032300030704     C                   EXSR      DECODI
032400030704     C*
032500021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032600021217     C                   MOVEL     'O05'         POP1
032700030122     C                   Z-ADD     VIHPRG        PPRG
032800040702     C                   EXSR      PASPAR
032900021217     C                   CALL      'TISIG4R'
033000000407     C                   PARM                    KPJBA
033100070509     c                   else
033200070509      * se errore sul cliente forzo opzione non ok così gestico la forzatura
033300070509     C                   Z-ADD     1             OPZOK
033400021211     C                   ENDIF
033500000407     C*
033600970526     C                   ENDSR
033700030704     C*------------------------------------------------------------------------*
033800030704     C* DECODI - CONTROLLA CAMPI E DECODIFICA
033900030704     C*------------------------------------------------------------------------*
034000030704     C     DECODI        BEGSR
034100030704     C*
034200030704     C                   MOVEL     '0'           ERRL00
034300030704     C*--------------------
034400030704     C* CONTROLLO AUTORIZZAZIONI UTENTE SU CODICE SOGGETTO MOVIMENTO "PRINCIPALE"
034500030704     C*--------------------
034600030704     C     ERRL00        IFEQ      '0'
034700030704     C* I CONTROLLI SI RIFERISCONO SOLO A SOGGETTI DI TIPO "F" O DI TIPO "C"
034800030704     C                   IF        VIHTIP = 'F'
034900030704     C                   MOVEL     VIDSOG        WN3               3
035000030704     C                   SETOFF                                           38
035100030704     C     WN3           LOOKUP    L1                                     38
035200030704     C     *IN38         IFEQ      *OFF
035300070509     c     vihforza      andeq     *off
035400030704     C                   MOVEL     '1'           ERRL00
035500030704     C                   MOVEL     MSG(4)        PMSG
035600070509     c                   eval      vihforza = *on
035700030704     C                   ENDIF
035800030704     C                   ENDIF
035900030704     C                   IF        VIHTIP = 'C'
036000030704     C                   MOVEL     VIDSOG        WA4               4
036100030704     C                   MOVE      WA4           WN3               3
036200030704     C                   SETOFF                                           38
036300030704     C     WN3           LOOKUP    L1                                     38
036400030704     C     *IN38         IFEQ      *OFF
036500070509     c     vihforza      andeq     *off
036600030704     C                   MOVEL     '1'           ERRL00
036700030704     C                   MOVEL     MSG(4)        PMSG
036800070509     c                   eval      vihforza = *on
036900030704     C                   ENDIF
037000030704     C                   ENDIF
037100030704     C                   ENDIF
037200030704     C*
037300030704     C                   ENDSR
037400991028     C*------------------------------------------------------------------------*
037500991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
037600991112     C*------------------------------------------------------------------------*
037700991112     C     PASPAR        BEGSR
037800991112     C*
037900000407     C                   MOVEL     PARAM         KPJBU
038000991112     C*
038100991112     C                   ENDSR
038200970526     C*------------------------------------------------------------------------*
038300970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
038400970526     C*------------------------------------------------------------------------*
038500970526     C     NEWPOS        BEGSR
038600970526     C*
038700970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
038800970526     C*         1= RICHIESTO POSIZIONAMENTO
038900970526     C                   Z-ADD     0             SWPOSI            7 0
039000970526     C*
039100991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
039200970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
039300970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
039400970526     C                   Z-ADD     0             SWMODI            1 0
039500970530     C*
039600970530     C* REIMPOSTA INDICATORI DI ERRORE
039700021217     C                   SETOFF                                       818283
039800021217     C                   SETOFF                                       848586
039900030128     C                   SETOFF                                       878889
040000030305     C                   SETOFF                                       80
040100970526     C*
040200970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
040300021230     C                   IF        VIDDA1 <> depda1 OR
040400030128     C                             VIDPD2 <> deppd2 OR
040500030128     C                             VIDPA2 <> deppa2 OR
040600030305     C                             VIDPI2 <> deppi2 OR
040700021230     C                             VIDPO2 <> deppo2 OR
040800021230     C                             VIDKS2 <> depks2 OR
040900021230     C                             VIDDS2 <> depds2 OR
041000021230     C                             VIDCA2 <> depca2 OR
041100021230     C                             VIDCO2 <> depco2 OR
041200021230     C                             VIDUT2 <> deput2 OR
041300030305     C                             %SCAN('?':VIDPI2) > 0 OR
041400021217     C                             %SCAN('?':VIDPO2) > 0 OR
041500021217     C                             %SCAN('?':VIDKS2) > 0 OR
041600021217     C                             %SCAN('?':VIDDS2) > 0 OR
041700021217     C                             %SCAN('?':VIDCA2) > 0 OR
041800021217     C                             %SCAN('?':VIDCO2) > 0
041900150403     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
042000021211     C     ERRL00        IFEQ      '0'                                          *NO ERORI
042100970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
042200021217     C                   Z-ADD     VIDDA1        depda1                         *DEPOSITI
042300030128     C                   MOVEL     VIDPD2        deppd2                         *DEPOSITI
042400030128     C                   MOVEL     VIDPA2        deppa2                         *DEPOSITI
042500030305     C                   MOVEL     VIDPI2        deppi2                         *DEPOSITI
042600021217     C                   MOVEL     VIDPO2        deppo2                         *DEPOSITI
042700021217     C                   MOVEL     VIDKS2        depks2                         *DEPOSITI
042800021217     C                   MOVEL     VIDDS2        depds2                         *DEPOSITI
042900021217     C                   MOVEL     VIDCA2        depca2                         *DEPOSITI
043000021217     C                   MOVEL     VIDCO2        depco2                         *DEPOSITI
043100021217     C                   MOVEL     VIDUT2        deput2                         *DEPOSITI
043200021211     C                   ENDIF
043300021211     C                   ENDIF
043400970526     C*
043500970526     C                   ENDSR
043600021217     C*------------------------------------------------------------------------*
043700021217     C* CHKKSC - CONTROLLA CODICE CLIENTE
043800021217     C*------------------------------------------------------------------------*
043900021217     C     CHKKSC        BEGSR
044000021217     C*
044100021217     C                   MOVEL(P)  VIDKS2        CHKKS2            7
044200021217     C*
044300021217     C* RICHIESTO ELENCO
044400021217     C                   SETOFF                                           28
044500021217     C     '?'           SCAN      CHKKS2                                 28
044600021217     C     *IN28         IFEQ      *ON
044700021217     C*
044800021217     C* RICHIESTO ELENCO CLIENTI
044900021217     C                   CLEAR                   DSXC
045000030124     C                   Z-ADD     DUTKCI        DXCCAP
045100021217     C                   CALL      'XCLIR'
045200021217     C                   PARM                    DSXC
045300021217     C* SCOMPATTA
045400021217     C                   MOVEA     DXCKSC        KSA
045500021217     C     1             DO        30            J                 3 0
045600021217     C                   MOVE      KSA(J)        DSKSA
045700021217     C                   Z-ADD     DSKSN         KSC(J)
045800021217     C                   ENDDO
045900030305     C                   IF        KSC(1) > *ZEROS
046000021217     C                   MOVE(P)   KSC(1)        CHKKS2
046100030305     C                   ELSE
046200030305     C                   MOVEL     *BLANKS       CHKKS2
046300030305     C                   ENDIF
046400021217     C                   MOVEL     '1'           $TESVD            1
046500021217     C                   ELSE
046600021217     C*
046700021217     C* CODICE CLIENTE NUMERICO
046800021217     C     CHKKS2        IFLT      *ZEROS
046900021217     C                   MOVEL     '1'           W_ERR             1
047000021217     C                   ELSE
047100021217     C*
047200021217     C* SOLO CARATTERI NUMERICI
047300021217     C   28              MOVE      *all'0'       CHKKS2
047400021217     C   28              MOVE      VIDKS2        CHKKS2
047500021217     C                   SETOFF                                       28
047600021217     C                   TESTN                   CHKKS2               28
047700021217     C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
047800021217     C                   MOVEL     '1'           W_ERR
047900021217     C                   ELSE                                                   *E'NUMERO DA TESTN
048000021217     C                   MOVE      CHKKS2        W001              1
048100021217     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
048200021217     C                   MOVEL     '1'           W_ERR
048300021217     C                   ELSE                                                   *NUMERO
048400021217     C*
048500021217     C* CONTROLLA ESISTENZA CLIENTE
048600021217     C                   EXSR      DECTIB
048700021217     C                   ENDIF
048800021217     C                   ENDIF
048900021217     C                   ENDIF
049000021217     C                   ENDIF
049100021217     C*
049200021217     C                   ENDSR
049300021217     C*------------------------------------------------------------------------*
049400021217     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
049500021217     C*------------------------------------------------------------------------*
049600021217     C     DECTIB        BEGSR
049700021217     C*
049800021217     C                   CLEAR                   BS69DS
049900021217     C                   CLEAR                   ACODS
050000021217     C                   MOVEL     KNSIF         I69SIF
050100021217     C                   MOVE(P)   CHKKS2        I69KAC
050200021217     C                   CALL      'TIBS69R'
050300021217     C                   PARM                    BS69DS
050400021217     C                   PARM                    ACODS
050500021217     C                   PARM                    INDDS
050600021217     C                   PARM                    CLPDS
050700021217     C                   PARM                    CLSDS
050800021217     C     O69ERR        IFNE      '1'
050900021217     C                   ELSE
051000021217     C                   MOVEL     '1'           W_ERR
051100021217     C                   ENDIF
051200021217     C*
051300021217     C                   ENDSR
051400021217     C*------------------------------------------------------------------------*
051500021217     C* CHKDST - CONTROLLA CODICE DESTINATARIO
051600021217     C*------------------------------------------------------------------------*
051700021217     C     CHKDST        BEGSR
051800021217     C*
051900021217     C* RICHIESTO ELENCO
052000021217     C                   SETOFF                                           28
052100021217     C     '?'           SCAN      VIDDS2                                 28
052200021217     C     *IN28         IFEQ      *ON
052300021217     C*
052400021217     C* RICHIESTO ELENCO DESTINATARI
052500021217     C                   MOVEL     KPJBU         SAVKPJBU
052600021217     C                   MOVEL(P)  'P01'         KPJBU
052700021217     C                   CALL      'TISIT1R'
052800021217     C                   PARM                    KPJBA
052900021217     C                   EVAL      VIDDS2 = %subst(KPJBU:4:7)
053000021217     C                   MOVEL     SAVKPJBU      KPJBU
053100021217     C                   ELSE
053200021217     C                   MOVE(P)   VIDDS2        CHKDS2            7 0
053300021217     C     CHKDS2        SETLL     azdst04l                               55
053400021217     C                   IF        *IN55 = *OFF
053500021217     C                   EVAL      W_ERR = '1'
053600021217     C                   ENDIF
053700021217     C                   ENDIF
053800021217     C*
053900021217     C                   ENDSR
054000021217     C*------------------------------------------------------------------------*
054100021217     C* CHKCOD - CONTROLLA CODICE ARTICOLO
054200021217     C*------------------------------------------------------------------------*
054300021217     C     CHKCOD        BEGSR
054400021217     C*
054500021217     C* RICHIESTO ELENCO
054600021217     C                   SETOFF                                           28
054700021217     C     '?'           SCAN      VIDCO2                                 28
054800021217     C     *IN28         IFEQ      *ON
054900021217     C*
055000021217     C* RICHIESTO ELENCO ARTICOLI
055100021217     C                   MOVEL     KPJBU         SAVKPJBU
055200021217     C                   MOVEL(P)  'P01'         KPJBU
055300021217     C                   CALL      'TISIG1R'
055400021217     C                   PARM                    KPJBA
055500021217     C                   EVAL      VIDCO2 = %subst(KPJBU:4:15)
055600021217     C                   MOVEL     SAVKPJBU      KPJBU
055700021217     C                   ELSE
055800021217     C                   MOVE(P)   VIDCO2        CHKCO2           15
055900021217     C     CHKCO2        SETLL     azagb01l                               55
056000021217     C                   IF        *IN55 = *OFF
056100021217     C                   EVAL      W_ERR = '1'
056200021217     C                   ENDIF
056300021217     C                   ENDIF
056400021217     C*
056500021217     C                   ENDSR
056600021217     C*------------------------------------------------------------------------*
056700021217     C* CHKPO - CONTROLLA CODICE P.O.
056800021217     C*------------------------------------------------------------------------*
056900021217     C     CHKPO         BEGSR
057000021217     C*
057100021217     C* RICHIESTO ELENCO
057200021217     C                   SETOFF                                           28
057300030305     C     '?'           SCAN      wChkPO                                 28
057400021217     C     *IN28         IFEQ      *ON
057500021217     C*
057600021217     C* RICHIESTO ELENCO P.O.
057700021217     C                   CALL      'TNSD24R'
057800021217     C                   PARM                    §COD              3
057900021217     C                   PARM      'F'           §TIP              1
058000021217     C                   PARM                    §§DES            25
058100021217     C                   PARM                    §§PRA             3
058200030305     C                   MOVEL     §COD          wChkPO
058300021217     C                   ELSE
058400030305     C                   MOVE(P)   wChkPO        CHKPO2            3 0
058500030305     C     CHKPO2        SETLL     azorg01l                               55
058600021217     C                   IF        *IN55 = *OFF
058700021217     C                   EVAL      W_ERR = '1'
058800021217     C                   ENDIF
058900021217     C                   ENDIF
059000021217     C*
059100021217     C                   ENDSR
059200021219     C*------------------------------------------------------------------------*
059300021219     C* CHKCAU - CONTROLLA CAUSALE MOVIMENTO
059400021219     C*------------------------------------------------------------------------*
059500021219     C     CHKCAU        BEGSR
059600030123     C*
059700030123     C* RICHIESTO ELENCO
059800030123     C                   SETOFF                                           28
059900030123     C     '?'           SCAN      VIDCA2                                 28
060000030123     C     *IN28         IFEQ      *ON
060100021219     C*
060200030123     C* VALORIZZO I PARAMETRI DI PASSAGGIO
060300030123     C                   CLEAR                   PARAM2
060400030123     C                   EVAL      P2OP1 = 'P01'
060500030123     C                   MOVEL     PARAM2        KPJBU
060600030123     C                   CALL      'TNTB31R'
060700030123     C                   PARM                    KPJBA
060800021219     C*
060900030123     C                   MOVEL     KPJBU         PARAM2
061000030123     C                   MOVEL     P2CAU         VIDCA2
061100030123     C*
061200030123     C* ALTRIMENTI CONTROLLO CODICE IMMESSO
061300030123     C                   ELSE
061400030123     C                   EVAL      tbeCOD = 'GGB'
061500030123     C                   EVAL      tbeKE1 = VIDCA2
061600030123     C     KEYTBEP       CHAIN     tntbe01l
061700030123     C                   IF        %found(tntbe01l)
061800030123     C                   ELSE
061900030123     C                   EVAL      W_ERR = '1'
062000030123     C                   ENDIF
062100030123     C                   ENDIF
062200021219     C*
062300021219     C                   ENDSR
062400970526     C*------------------------------------------------------------------------*
062500021217     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
062600970526     C*------------------------------------------------------------------------*
062700970528     C     SETFIL        BEGSR
062800970526     C*
062900970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
063000021217     C     VIDDA1        IFEQ      *zeros
063100150403     C                   Z-ADD     *hival        KEYDAT
063200991108     C                   ELSE
063300030305     C                   Z-ADD     DA1VID        KEYDAT
063400991108     C                   ENDIF
063500021217     C     WTIPRI        IFEQ      'DAT'
063600030305     C     KEYMGBP       SETLL     azmgb01l                           99
063700021211     C                   ENDIF
063800980107     C*
063900021211     C* LETTURA RECORD
064000021211     C     *IN99         IFEQ      *OFF
064100970526     C                   EXSR      LETFIL
064200021211     C                   ENDIF
064300970526     C*
064400970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
064500021211     C     *IN99         IFEQ      *ON
064600970526     C                   Z-ADD     1             SFLEOF
064700021211     C                   ENDIF
064800970526     C*
064900970526     C                   ENDSR
065000970526     C*------------------------------------------------------------------------*
065100970526     C* LETFIL - LETTURA RECORD
065200970526     C*------------------------------------------------------------------------*
065300970526     C     LETFIL        BEGSR
065400970526     C*
065500970526     C* LEGGE FINO A:
065600970526     C*     - FINE FILE
065700970526     C*     - TROVATO RECORD VALIDO
065800970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
065900021211     C     *IN99         DOWEQ     *OFF
066000970526     C     WRECOK        ANDEQ     'N'
066100970526     C*
066200970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
066300021217     C     WTIPRI        IFEQ      'DAT'                                        *PER DATA/ORA/UTENTE
066400030305     C                   READ      azmgb01l                               99
066500021211     C                   ENDIF
066600970528     C*
066700970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
066800021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
066900970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
067000021211     C                   ELSE
067100970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
067200021211     C                   ENDIF
067300021211     C                   ENDDO
067400970526     C*
067500970526     C                   ENDSR
067600970526     C*------------------------------------------------------------------------*
067700021217     C* CHKREC - CONTROLLA VALIDITA' RECORD
067800970526     C*------------------------------------------------------------------------*
067900970526     C     CHKREC        BEGSR
068000970526     C*
068100970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
068200030128     C* FILTRA SE RICHIESTO PERIODO DA->A
068300030305     C                   if        PD2VID = *zeros AND PA2VID = *zeros
068400030128     C                   else
068500150403     C                   if        mgbdat >= PD2VID  AND mgbDAT <= PA2VID
068600030128     C                   else
068700030128     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
068800030128     C                   endif
068900030128     C                   endif
069000030305     C* FILTRA SE RICHIESTO UN P.O. INSERIMENTO
069100030305     C     VIDPI2        ifgt      *zeros
069200030305     C                   move(p)   VIDPI2        numPI2            3 0
069300030305     C                   if        mgbPOU <> numPI2
069400030305     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
069500030305     C                   endif
069600030305     C                   endif
069700021217     C* FILTRA SE RICHIESTO UN P.O.
069800021217     C     VIDPO2        ifeq      *blanks
069900021230     C     VIDPO2        oreq      *zeros
070000021217     C                   else
070100021218     C                   movel     *all'0'       CHKSOG
070200021218     C                   move      VIDPO2        CHKSOG
070300021230     C***                if        mgbTIP = 'F' AND
070400021230     C                   if        mgbSOG <> CHKSOG
070500021217     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
070600021217     C                   endif
070700021217     C                   endif
070800021217     C* FILTRA SE RICHIESTO UN CLIENTE
070900021217     C     VIDKS2        ifeq      *blanks
071000021230     C     VIDKS2        oreq      *zeros
071100021217     C                   else
071200021218     C                   movel     *all'0'       CHKSOG
071300021218     C                   move      VIDKS2        CHKSOG
071400021230     C***                if        mgbTIP = 'C' AND
071500021230     C                   if        mgbSOG <> CHKSOG
071600021217     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
071700021217     C                   endif
071800021217     C                   endif
071900021217     C* FILTRA SE RICHIESTO UN DESTINATARIO
072000021217     C     VIDDS2        ifeq      *blanks
072100021230     C     VIDDS2        oreq      *zeros
072200021217     C                   else
072300021218     C                   movel     *all'0'       CHKSOG
072400021218     C                   move      VIDDS2        CHKSOG
072500021230     C***                if        mgbTIP = 'D' AND
072600021230     C                   if        mgbSOG <> CHKSOG
072700021217     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
072800021217     C                   endif
072900021217     C                   endif
073000021217     C* FILTRA SE RICHIESTO UNA CAUSALE
073100021217     C     VIDCA2        ifeq      *blanks
073200021217     C                   else
073300021217     C     mgbCAU        ifne      VIDCA2
073400021217     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
073500021217     C                   endif
073600021217     C                   endif
073700021211     C* FILTRA SE RICHIESTO UN TIPO ARTICOLO
073800021217     C     VIDCO2        ifeq      *blanks
073900021211     C                   else
074000021217     C     mgbCOD        ifne      VIDCO2
074100021211     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
074200021211     C                   endif
074300021211     C                   endif
074400021217     C* FILTRA SE RICHIESTO UN UTENTE
074500021217     C     VIDUT2        ifeq      *blanks
074600021217     C                   else
074700021217     C     mgbUTE        ifne      VIDUT2
074800021217     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
074900021217     C                   endif
075000021217     C                   endif
075100991108     C*
075200970526     C                   ENDSR
075300970526     C*------------------------------------------------------------------------*
075400000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
075500970526     C*------------------------------------------------------------------------*
075600970526     C     IRESFL        BEGSR
075700970526     C*
075800030325     C* Reperisco la decodifica del soggetto
075900030325     C* ...cliente
076000030325     C                   IF        mgbTIP = 'C'
076100030325     C                   Z-ADD     1             acoKUT
076200030325     C                   Z-ADD     DUTKCI        acoKCC
076300030325     C                   MOVE(p)   mgbSOG        acoKSC
076400030325     C     KEYACOT       CHAIN     cnaco00f
076500030325     C                   IF        %found(cnaco00f)
076600030325     C                   EVAL      VIDRAG = acoRAG
076700030325     C                   ELSE
076800030325     C                   MOVEL     *all'*'       VIDRAG
076900030325     C                   ENDIF
077000030325     C                   ENDIF
077100030325     C* ...destinatario
077200030325     C                   IF        mgbTIP = 'D'
077300030325     C                   MOVE(p)   mgbSOG        dstCOD
077400030325     C     dstCOD        CHAIN     azdst04l
077500030325     C                   IF        %found(azdst04l)
077600030325     C                   EVAL      VIDRAG = dstRAG
077700030325     C                   ELSE
077800030325     C                   MOVEL     *all'*'       VIDRAG
077900030325     C                   ENDIF
078000030325     C                   ENDIF
078100030325     C* ...P.O.
078200030325     C                   IF        mgbTIP = 'F'
078300030325     C                   MOVE(p)   mgbSOG        orgFIL
078400030325     C     orgFIL        CHAIN     azorg01l
078500030325     C                   IF        %found(azorg01l)
078600030325     C                   EVAL      VIDRAG = orgDES
078700030325     C                   ELSE
078800030325     C                   MOVEL     *all'*'       VIDRAG
078900030325     C                   ENDIF
079000030325     C                   ENDIF
079100030325     C*
079200000407     C                   MOVEL     *blanks       OPZKEY
079300021217     C                   Z-ADD     mgbDAT        VIDDAT
079400021217     C                   Z-ADD     mgbORA        VIDORA
079500021217     C                   MOVEL(P)  mgbCAU        VIDCAU
079600021217     C                   MOVEL(P)  mgbTIM        VIDTIM
079700021217     C                   MOVEL(P)  mgbCOD        VIDCOD
079800021217     C                   Z-ADD     mgbQTA        VIDQTA
079900021218     C                   MOVEL(P)  mgbSOG        VIDSOG
080000150408     C*                  IF        mgbTIP = 'C'
080100150408     C*                  MOVEL(P)  'CLI'         VIDDSO
080200150408     C*                  ENDIF
080300150408     C*                  IF        mgbTIP = 'D'
080400150408     C*                  MOVEL(P)  'DES'         VIDDSO
080500150408     C*                  ENDIF
080600150408     C*                  IF        mgbTIP = 'F'
080700150408     C*                  MOVEL(P)  'PO'          VIDDSO
080800150408     C*                  ENDIF
080900150408     C                   MOVEL     mgbpou        VIddso
081000021217     C                   MOVEL     mgbUTE        VIHUTE
081100030122     C                   Z-ADD     mgbPRG        VIHPRG
081200030704     C                   MOVEL     mgbTIP        VIHTIP
081300070509
081400070509     c                   eval      vihforza = *off
081500991108     C*
081600970526     C                   ENDSR
081700970526     C*------------------------------------------------------------------------*
081800991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
081900970526     C*------------------------------------------------------------------------*
082000970526     C     CHKPAR        BEGSR
082100021218     C*
082200021218     C* INIZIALIZZO FLAG GENERICO DI ERRORE SELEZIONI
082300021218     C                   MOVEL     '0'           W_ERR
082400970528     C*
082500021211     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *01L (COD)
082600021217     C                   MOVEL     'DAT'         WTIPRI            3
082700030128     C*
082800030128     C* CONTROLLO VALIDITA DATA INIZIO ELENCO
082900030128     C                   IF        W_ERR = '0'
083000030128     C                   IF        VIDDA1 > *zeros
083100030128     C                   Z-ADD     VIDDA1        DAW8
083200030128     C                   EXSR      CHKDA8
083300030305     C                   Z-ADD     WDA8          VIDDA1
083400030305     C                   Z-ADD     DAW8          DA1VID            8 0
083500030128     C                   IF        G08ERR = '1'
083600030128     C                   EVAL      W_ERR = '1'
083700030128     C                   SETON                                        81
083800030128     C                   ENDIF
083900030128     C                   ENDIF
084000030128     C                   ENDIF
084100030128     C*
084200030128     C* CONTROLLO VALIDITA PERIODO DA
084300030128     C                   IF        W_ERR = '0'
084400030128     C                   IF        VIDPD2 > *zeros
084500030128     C                   Z-ADD     VIDPD2        DAW8
084600030128     C                   EXSR      CHKDA8
084700030305     C                   Z-ADD     WDA8          VIDPD2
084800030305     C                   Z-ADD     DAW8          PD2VID            8 0
084900030128     C                   IF        G08ERR = '1'
085000030128     C                   EVAL      W_ERR = '1'
085100030128     C                   SETON                                        88
085200030128     C                   ENDIF
085300030128     C                   ENDIF
085400030128     C                   ENDIF
085500030128     C*
085600030128     C* CONTROLLO VALIDITA PERIODO A
085700030128     C                   IF        W_ERR = '0'
085800030128     C                   IF        VIDPA2 > *zeros
085900030128     C                   Z-ADD     VIDPA2        DAW8
086000030128     C                   EXSR      CHKDA8
086100030305     C                   Z-ADD     WDA8          VIDPA2
086200030305     C                   Z-ADD     DAW8          PA2VID            8 0
086300030128     C                   IF        G08ERR = '1'
086400030128     C                   EVAL      W_ERR = '1'
086500030128     C                   SETON                                        89
086600030128     C                   ENDIF
086700030128     C                   ENDIF
086800030128     C                   ENDIF
086900030128     C*
087000030128     C* CONTROLLO PERIODO DA <= PERIODO A
087100030128     C                   IF        W_ERR = '0'
087200030128     C                   IF        VIDPD2 > VIDPA2
087300030128     C                   EVAL      W_ERR = '1'
087400030128     C                   SETON                                        8889
087500030128     C                   ENDIF
087600030128     C                   ENDIF
087700021217     C*
087800021217     C* CONTROLLO SE RICHIESTO CLIENTE
087900021218     C                   IF        W_ERR = '0'
088000021217     C                   IF        VIDKS2 <> *BLANKS AND
088100021217     C                             VIDKS2 <> *ZEROS
088200021217     C                   EXSR      CHKKSC
088300021217     C                   IF        W_ERR = '1'
088400021217     C                   SETON                                        83
088500021217     C                   ELSE
088600021217     C                   MOVEL(P)  CHKKS2        VIDKS2
088700021217     C                   ENDIF
088800021217     C                   ENDIF
088900021218     C                   ENDIF
089000021217     C*
089100021217     C* CONTROLLO SE RICHIESTO DESTINATARIO
089200021218     C                   IF        W_ERR = '0'
089300021217     C                   IF        VIDDS2 <> *BLANKS AND
089400021217     C                             VIDDS2 <> *ZEROS
089500021217     C                   EXSR      CHKDST
089600021217     C                   IF        W_ERR = '1'
089700021217     C                   SETON                                        84
089800021217     C                   ENDIF
089900021218     C                   ENDIF
090000021217     C                   ENDIF
090100021217     C*
090200021217     C* CONTROLLO SE RICHIESTO ARTICOLO
090300021218     C                   IF        W_ERR = '0'
090400021217     C                   IF        VIDCO2 <> *BLANKS AND
090500021217     C                             VIDCO2 <> *ZEROS
090600021217     C                   EXSR      CHKCOD
090700021217     C                   IF        W_ERR = '1'
090800021217     C                   SETON                                        86
090900021217     C                   ENDIF
091000021218     C                   ENDIF
091100021217     C                   ENDIF
091200030305     C*
091300030305     C* CONTROLLO SE RICHIESTO P.O. INSERIMENTO
091400030305     C                   IF        W_ERR = '0'
091500030305     C                   IF        VIDPI2 <> *BLANKS AND
091600030305     C                             VIDPI2 <> *ZEROS
091700030305     C                   MOVEL     VIDPI2        wChkPO            3
091800030305     C                   EXSR      CHKPO
091900030305     C                   MOVEL     wChkPO        VIDPI2
092000030305     C                   IF        W_ERR = '1'
092100030305     C                   SETON                                        80
092200030305     C                   ENDIF
092300030305     C                   ENDIF
092400030305     C                   ENDIF
092500021217     C*
092600021217     C* CONTROLLO SE RICHIESTO P.O.
092700021218     C                   IF        W_ERR = '0'
092800021217     C                   IF        VIDPO2 <> *BLANKS AND
092900021217     C                             VIDPO2 <> *ZEROS
093000030305     C                   MOVEL     VIDPO2        wChkPO            3
093100021217     C                   EXSR      CHKPO
093200030305     C                   MOVEL     wChkPO        VIDPO2
093300021217     C                   IF        W_ERR = '1'
093400021217     C                   SETON                                        82
093500021217     C                   ENDIF
093600021218     C                   ENDIF
093700021217     C                   ENDIF
093800021219     C*
093900021219     C* CONTROLLO SE RICHIESTA CAUSALE
094000021219     C                   IF        W_ERR = '0'
094100021219     C                   IF        VIDCA2 <> *BLANKS AND
094200021219     C                             VIDCA2 <> *ZEROS
094300021219     C                   EXSR      CHKCAU
094400021219     C                   IF        W_ERR = '1'
094500021219     C                   SETON                                        85
094600021219     C                   ENDIF
094700021219     C                   ENDIF
094800021219     C                   ENDIF
094900000407     C*
095000970526     C                   ENDSR
095100970526     C*------------------------------------------------------------------------*
095200970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
095300970526     C*------------------------------------------------------------------------*
095400970526     C     CHKFOP        BEGSR
095500991112     C*
095600991112     C* OPZIONI
095700970626     C*
095800000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
095900021211     C     DEPOP0        IFEQ      'P01'
096000000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
096100000420     C                   MOVEL     'S'           OPDATT
096200000420     C                   MOVEL     OPDRIG        OPD(1)
096300021211     C                   ELSE
096400000420     C* OPZIONI
096500970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
096600970626     C                   MOVEL     'S'           OPDATT
096700970626     C                   MOVEL     OPDRIG        OPD(5)
096800021211     C                   ENDIF
096900970626     C*
097000000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
097100021211     C     DEPOP0        IFEQ      'P02'
097200160808     C                   IF        §utecanc = 'S'
097300160727     C                   MOVEL     OPD(4)        OPDRIG                         *INTERROGAZIONE
097400160727     C                   MOVEL     'S'           OPDATT
097500160727     C                   MOVEL     OPDRIG        OPD(4)
097600160727     c                   endif
097700991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
097800991112     C                   MOVEL     'S'           OPDATT
097900991112     C                   MOVEL     OPDRIG        OPD(5)
098000021211     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
098100000407     C                   MOVEL     'S'           FUDATT
098200000407     C                   MOVEL     FUDRIG        FUD(6)
098300021211     C                   ENDIF
098400101006     ***C*
098500101006     ***C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
098600101006     ***   *-=»> NON PIÙ: ORA CONSENTE F6=INSERIMENTO <«=-*
098700101006     ***C     SIMFEL        IFNE      *zeros                                       *FILIALE
098800101006     ***C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
098900101006     ***C                   MOVEL     'N'           OPDATT
099000101006     ***C                   MOVEL     FUDRIG        FUD(6)
099100101006     ***C                   ENDIF
099200970526     C*
099300970526     C* GESTIONE OPZIONI A VIDEO
099400970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
099500970526     C                   EXSR      OPZGES
099600970526     C*
099700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
099800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
099900970526     C                   EXSR      FUNGES
100000970526     C*
100100970526     C                   ENDSR
100200020917     C*--------------------------------------------------------------------------------------------*
100300020917     C* REPERISCE I DATI UTENTE
100400020917     C*--------------------------------------------------------------------------------------------*
100500020917     C     REPDATIUTE    BEGSR
100600020917     C*
100700020917     C* INIZIALIZZA VARIABILI DI WRK
100800020917     C                   CLEAR                   TIBS34DS
100900020917     C                   CLEAR                   AZUTEDS
101000020917     C                   CLEAR                   DDATIUTE
101100150212     C                   CLEAR                   dute01
101200020917     C*
101300020917     C     *DTAARA       DEFINE    §azute        azuteds
101400020917     C     *DTAARA       DEFINE    §datiute      ddatiute
101500020917     C                   IN(E)     *DTAARA
101600021211     C                   IF        %Error
101700021211     C                   EVAL      I34Tla = 'L'
101800020917     C                   CALL      'TIBS34R'
101900020917     C                   PARM                    Tibs34Ds
102000020917     C                   IN        *DTAARA
102100021211     C                   ENDIF
102200030704     C*
102300030704     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
102400030704     C                   CLEAR                   L1
102500030704     C                   CLEAR                   WAbi              2
102600150212     c                   Clear                   dLat
102700030704     C*
102800150212      * Verifica errori e autorità profilo
102900150212s   1c                   Select
103000150212      * se ho errori nei dati utente esco dal pgm
103100150212w   1c                   When      DutErr = 'E'
103200150212     c                   seton                                        lr
103300150212     c                   return
103400150212      * se non c'è l'abilitazione
103500150212      * --> se 1° livello, abilitazioni al terminal
103600150212      *     se 2° livello, abilitazioni al punto operativo
103700150212w   1c                   When      UteAut = *Blanks
103800150212if  2c                   If        DutLpo = '1'
103900150212     c                   Eval      wabi   = 'TP'
104000150212e   2c                   EndIf
104100150212if  2c                   If        DutLpo = '2'
104200150212     c                   Eval      wabi   = 'PO'
104300150212e   2c                   EndIf
104400150212if  2c                   If        DutLpo = 'S'
104500150212     c                   Eval      wabi   = 'AZ'
104600150212e   2c                   EndIf
104700150212      * carica le abilitazioni del profilo
104800150212x   1c                   Other
104900150212     c                   Movel     UteFaf        Dute01
105000150212     c                   If        §Utemgb <> *Blanks
105100150212     c                   Eval      wabi = §Utemgb
105200150212     c                   Else
105300150212     c                   Eval      wabi = UteAut
105400150212     c                   EndIf
105500150212e   1c                   EndSl
105600150212      * controllo se ok l'abilitazione dell'utente
105700150212     c                   Clear                   Tibs02ds
105800150212     c                   Eval      T02Mod = 'C'
105900150212     c                   Eval      T02Sif = knsif
106000150212     c                   Eval      T02Cod = 'LAT'
106100150212     c                   Movel(p)  wabi          T02Ke1
106200150212     c                   Call      'TIBS02R'
106300150212     c                   Parm                    kpjba
106400150212     c                   Parm                    Tibs02ds
106500150212if  1c                   If        T02Err = *Blanks
106600150212     c                   Eval      dLat = T02Uni
106700150212e   1c                   EndIf
106800150212      * errore o non abilitato (impossibile xchè controllato nel filtro)
106900150212if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
107000150212     C                   MOVEL     'P05'         DEPOP0
107100150212   x1c                   Else
107200030704     C* REPERISCO I P.O. GESTIBILI DALL'UTENTE
107300030704     C                   CLEAR                   TRUL31DS
107400150212     c                   clear                   TRUL31DS2
107500030704     C                   EVAL      I31abi = WAbi
107600150212     c                   eval      I31cdi = DUTdis
107700150212     c                   eval      I31car = DUTare
107800030704     C                   EVAL      I31cpo = DUTPOU
107900030704     C                   CALL      'TRUL31R'
108000030704     C                   PARM                    KPJBA
108100030704     C                   PARM                    TRUL31DS
108200150212     c                   parm                    TRUL31DS2
108300030704     C                   IF        O31pog > *zeros
108400030704     C                   MOVEA     O31pog        L1
108500030704     C                   ELSE
108600030704     C                   EVAL      FINL00 = '1'
108700030704     C                   ENDIF
108800150212     C                   ENDIF
108900020917     C*
109000020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
109100020917     C                   MOVEL(P)  RSUT          DSFIRS           20
109200020917     C*
109300020917     C                   ENDSR
109400030128     C*------------------------------------------------------------------------*
109500030128     C* CHKDA8 - CONTROLLA LA DATA (8)
109600030128     C*------------------------------------------------------------------------*
109700030128     C     CHKDA8        BEGSR
109800030128     C*
109900030305     C                   Z-ADD     DAW8          G08DAT
110000030305     C                   Z-ADD     *ZEROS        G08INV
110100030305     C                   MOVEL     '0'           G08ERR
110200030128     C                   CALL      'XSRDA8'
110300030128     C                   PARM                    WLBDA8
110400030305     C                   Z-ADD     G08DAT        WDA8
110500030305     C                   Z-ADD     G08INV        DAW8
110600030128     C*
110700030128     C                   ENDSR
110800970526     C*------------------------------------------------------------------------*
110900970526     C* *INZSR - OPERAZIONI INIZIALI
111000970526     C*------------------------------------------------------------------------*
111100970526     C     *INZSR        BEGSR
111200970526     C*---
111300970526     C* RICEVIMENTO PARAMETRI
111400970526     C*---
111500970526     C     *ENTRY        PLIST
111600970612     C                   PARM                    KPJBA
111700021211     C                   MOVEL     KPJBU         PARAM
111800021211     C                   MOVEL     POP1          DEPOP0            3
111900970526     C*
112000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
112100970526     C     XMSGPA        PLIST
112200970526     C                   PARM                    DSMSMS
112300970526     C                   PARM                    DSMSPG
112400970526     C                   PARM                    DSMSKE
112500970526     C                   PARM                    DSMSER
112600970526     C*
112700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
112800970526     C                   CALL      'XKEY'
112900970526     C                   PARM                    DSKEY
113000970526     C*---
113100970526     C* VARIABILI NON RIFERITE AL DATA BASE
113200970526     C*---
113300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
113400970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
113500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
113600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
113700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
113800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
113900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
114000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
114100970526     C*---
114200021217     C     *LIKE         DEFINE    VIDDA1        depda1
114300030128     C     *LIKE         DEFINE    VIDDA1        DAW8
114400030128     C     *LIKE         DEFINE    VIDDA1        WDA8
114500030128     C     *LIKE         DEFINE    VIDPD2        deppd2
114600030128     C     *LIKE         DEFINE    VIDPA2        deppa2
114700030305     C     *LIKE         DEFINE    VIDPI2        deppi2
114800021217     C     *LIKE         DEFINE    VIDPO2        deppo2
114900021217     C     *LIKE         DEFINE    VIDKS2        depks2
115000021217     C     *LIKE         DEFINE    VIDDS2        depds2
115100021217     C     *LIKE         DEFINE    VIDCA2        depca2
115200021217     C     *LIKE         DEFINE    VIDCO2        depco2
115300021217     C     *LIKE         DEFINE    VIDUT2        deput2
115400021217     C     *LIKE         DEFINE    MGBDAT        KEYDAT
115500021218     C     *LIKE         DEFINE    MGBSOG        CHKSOG
115600021217     C     *LIKE         DEFINE    KPJBU         SAVKPJBU
115700991108     C*---
115800970526     C* VARIABILI DI CONTROLLO
115900970526     C*---
116000970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
116100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
116200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
116300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
116400970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
116500150403     C                   Z-ADD     8             SFLPAG                         *N°RIGHE X PAG
116600970526     C*
116700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
116800021217     C                   MOVEL     'TISIG3R'     DSMSPG
116900021217     C                   MOVEL     'TISIG3R'     MSGQUE
117000970526     C                   MOVEL     *BLANKS       MSGKEY
117100970526     C                   MOVEL     *BLANKS       DSMSMS
117200970526     C                   MOVEL     *BLANKS       DSMSKE
117300970526     C                   Z-ADD     *ZEROS        DSMSER
117400970526     C*---
117500970526     C* CHIAVI DI LETTURA
117600970526     C*---
117700021217     C* LETTURA azmgb01l - PARZIALE
117800021211     C     KEYMGBP       KLIST
117900021217     C                   KFLD                    KEYDAT
118000030123     C* LETTURA tntbe01l - PARZIALE
118100030123     C     KEYTBEP       KLIST
118200030123     C                   KFLD                    tbeCOD
118300030123     C                   KFLD                    tbeKE1
118400030325     C* LETTURA cnaco00f - COMPLETA
118500030325     C     KEYACOT       KLIST
118600030325     C                   KFLD                    acoKUT
118700030325     C                   KFLD                    acoKCC
118800030325     C                   KFLD                    acoKSC
118900991108     C*
119000970527     C* REPERIMENTO DATI UTENTE
119100020917     C                   EXSR      REPDATIUTE
119200970526     C*
119300970526     C* IMPOSTA I PARAMETRI DI INGRESSO
119400970526     C                   EXSR      CHKPAR
119500991115     C*
119600970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
119700970526     C                   EXSR      CHKFOP
119800970526     C*
119900970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
120000021211     C     DEPOP0        IFEQ      'P01'
120100970530     C                   MOVEL     DEC(3)        VIDING
120200021211     C                   ENDIF
120300021211     C     DEPOP0        IFEQ      'P02'
120400970526     C                   MOVEL     DEC(1)        VIDING
120500021211     C                   ENDIF
120600021211     C     DEPOP0        IFEQ      'P05'
120700970526     C                   MOVEL     DEC(2)        VIDING
120800021211     C                   ENDIF
120900970526     C*
121000970526     C                   ENDSR
121100970526     C*------------------------------------------------------------------------*
121200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
121300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
121400970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
121500970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
121600970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
121700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
121800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
121900970526     C*------------------------------------------------------------------------*
122000970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
122100970526     C*------------------------------------------------------------------------*
122200970526     C     POSSFL        BEGSR
122300970526     C*
122400970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
122500970526     C                   EXSR      CLRSFL
122600030128     C                   Z-ADD     *zeros        VIDSAL
122700030305     C*
122800030305     C* SE NN EFFETTUATE SELEZIONI EMETTO VIDEATA DI RICHIESTA
122900030305     C                   IF        VIDPD2 = *zeros                       AND
123000030305     C                             VIDPA2 = *zeros                       AND
123100150403     C                             VIDda1 = *zeros                       AND
123200030305     C                             (VIDPI2 = *blanks OR VIDPI2 = *zeros) AND
123300030305     C                             (VIDPO2 = *blanks OR VIDPO2 = *zeros) AND
123400030305     C                             (VIDKS2 = *blanks OR VIDKS2 = *zeros) AND
123500030305     C                             (VIDDS2 = *blanks OR VIDDS2 = *zeros) AND
123600030305     C                             (VIDCA2 = *blanks OR VIDCA2 = *zeros) AND
123700030305     C                             (VIDCO2 = *blanks OR VIDCO2 = *zeros) AND
123800030305     C                             (VIDUT2 = *blanks OR VIDUT2 = *zeros)
123900030305     C                   SETON                                        97
124000030305     C                   ELSE
124100030305     C                   SETOFF                                       97
124200970526     C*
124300970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
124400970526     C                   EXSR      SETFIL
124500970526     C*
124600970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
124700970526     C     SFLEOF        IFEQ      0
124800970526     C                   EXSR      CARSFL
124900030305     C                   ENDIF
125000030305     C                   ENDIF
125100970526     C*
125200970526     C                   ENDSR
125300970526     C*------------------------------------------------------------------------*
125400970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
125500970526     C*------------------------------------------------------------------------*
125600970526     C     ENTATT        BEGSR
125700970526     C*
125800970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
125900970526     C                   Z-ADD     0             OPZOK             1 0
126000970526     C*
126100970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
126200970526     C                   Z-ADD     0             SFLVAR            5 0
126300970526     C*
126400970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
126500970526     C                   Z-ADD     0             SFLERR
126600970526     C*
126700970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
126800970526     C                   Z-ADD     SFLULT        WFLULT
126900970526     C*
127000970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
127100021217     C                   READC     SIG3D05                                98
127200970526     C     *IN98         DOWEQ     '0'
127300970526     C*
127400970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
127500970526     C                   ADD       1             SFLVAR
127600970526     C*
127700970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
127800970526     C     OPZOK         IFEQ      0
127900970526     C*
128000970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
128100970526     C     OPZKEY        IFNE      *BLANKS
128200970526     C                   EXSR      OPZCON
128300970526     C                   END
128400970526     C*
128500970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
128600970526     C*            2. PULIZIA SCELTA IN SFL
128700970526     C     OPZOK         IFEQ      0
128800970526     C                   MOVEL     '0'           *IN41
128900970526     C                   MOVEL     *BLANKS       OPZKEY
129000970526     C                   ELSE
129100970526     C*
129200970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
129300970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
129400970526     C                   MOVEL     '1'           *IN41
129500970526     C                   Z-ADD     SFLULT        SFLERR
129600970526     C                   END
129700970526     C                   ELSE
129800970526     C*
129900970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
130000970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
130100970526     C                   MOVEL     '1'           *IN41
130200970526     C                   END                                                    OPZOK=0
130300970526     C*
130400970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
130500970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
130600970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
130700970526     C     OPZOK         IFNE      0
130800970526     C                   Z-ADD     SFLERR        SFLNBR
130900970526     C                   ELSE
131000970526     C                   Z-ADD     SFLULT        SFLNBR
131100970526     C                   END
131200970526     C*
131300970526     C* AGGIORNO RECORD SFL
131400021217     C                   UPDATE    SIG3D05
131500970526     C*
131600021217     C                   READC     SIG3D05                                98
131700970526     C                   END                                                    DO WHILE
131800970526     C*
131900970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
132000970526     C                   Z-ADD     WFLULT        SFLULT
132100970526     C*
132200970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
132300970526     C     SFLVAR        IFEQ      0
132400970526     C                   END
132500970526     C*
132600040702     C***  OPZKEY        IFNE      ' 5'
132700040702     C***                EXSR      F05GES
132800040702     C***                ENDIF
132900991112     C*
133000970526     C                   ENDSR
133100970526     C*------------------------------------------------------------------------*
133200970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
133300970526     C*------------------------------------------------------------------------*
133400970526     C     CLRSFL        BEGSR
133500970526     C*
133600970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
133700970526     C* (0=NON FINE FILE  1=FINE FILE)
133800970526     C                   Z-ADD     0             SFLEOF
133900970526     C*
134000970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
134100970526     C                   Z-ADD     0             SFLULT
134200970526     C*
134300970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
134400970526     C                   MOVEL     '1'           *IN40
134500021217     C                   WRITE     SIG3D06
134600970526     C                   MOVEL     '0'           *IN40
134700970526     C*
134800970526     C                   ENDSR
134900970526     C*------------------------------------------------------------------------*
135000970526     C* CARSFL - CARICAMENTO SFL
135100970526     C*------------------------------------------------------------------------*
135200970526     C     CARSFL        BEGSR
135300970526     C*
135400970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
135500970526     C     SFLULT        IFEQ      0
135600970526     C                   EXSR      SETFIL
135700970526     C                   END
135800970526     C*
135900970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
136000970526     C                   Z-ADD     SFLULT        WFLULT
136100970526     C*
136200970526     C* CICLO:
136300970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
136400970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
136500040225     C                   Z-ADD     1             SFLRIG            5 0
136600150417 XXX C***  SFLRIG        DOWLE     SFLPAG
136700150417     C***  SFLEOF        ANDEQ     0
136800150417     C     SFLEOF        DOWEQ     0
136900150417     C* SE NN SUPERATO NUMERO MAX RECORD IN SUB-FILE
137000150417     C     SFLRIG        IFLE      9999
137100970526     C*
137200970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
137300991116     C                   MOVEL     *BLANKS       OPZKEY
137400970526     C*
137500040225     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
137600970526     C                   EXSR      IRESFL
137700970526     C*
137800970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
137900970526     C                   ADD       1             SFLULT
138000970526     C*
138100970526     C* SCRITTURA RECORD SFL
138200021217     C                   WRITE     SIG3D05
138300970526     C                   ADD       1             SFLRIG
138400150417     C* SE RAGGIUNTO NUMERO MAX RECORD IN SUB-FILE => EMETTO MESSAGGIO
138500150417     C                   ELSE
138600150417     C                   MOVEL     MSG(5)        DSMSMS
138700150417     C                   ENDIF
138800040225     C*
138900040225     C* INCREMENTO IL SALDO A VIDEO INDIPENDENTEMENTE DAL CARICAMENTO DEI RECORD DEL SUB-FILE
139000040225     C                   ADD       mgbQTA        VIDSAL
139100970526     C*
139200970526     C* LETTURA FILE DA CARICARE
139300970526     C                   EXSR      LETFIL
139400970526     C                   END                                                    DO WHILE
139500970526     C*
139600970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
139700970526     C     WFLULT        IFNE      SFLULT
139800970526     C                   Z-ADD     WFLULT        SFLNBR
139900970526     C                   ADD       1             SFLNBR
140000970526     C                   END
140100970526     C*
140200970526     C                   ENDSR
140300970526     C*------------------------------------------------------------------------*
140400970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
140500970526     C*------------------------------------------------------------------------*
140600970526     C     OUTSFL        BEGSR
140700970526     C*
140800970526     C* WRITE RECORD TESTATA VIDEO
140900021217     C                   WRITE     SIG3D01
141000970526     C*
141100970526     C* VISUALIZZAZIONE MESSAGGIO
141200970526     C                   EXSR      SNDMSG
141300970526     C*
141400970526     C* WRITE RECORD OPZIONI VIDEO
141500021217     C                   WRITE     SIG3D02
141600970526     C*
141700970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
141800021217     C                   WRITE     SIG3D03
141900970526     C*
142000970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
142100021217     C                   WRITE     SIG3D04
142200970526     C*
142300970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
142400970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
142500970526     C     SFLEOF        IFEQ      0
142600970526     C                   MOVEL     '0'           *IN42
142700970526     C                   ELSE
142800970526     C*
142900970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
143000970526     C                   MOVEL     '1'           *IN42
143100970526     C                   END
143200970526     C*
143300970526     C* WRITE RECORD TASTI FUNZIONALI
143400021217     C                   WRITE     SIG3D09
143500970526     C*
143600970526     C* CASO DI SFL VUOTO
143700970526     C     SFLULT        IFLE      0
143800970526     C*
143900970526     C*    - WRITE RECORD VUOTO
144000021217     C                   WRITE     SIG3D0A
144100021217     C                   READ      SIG3D0A                                99
144200970526     C*
144300970526     C                   ELSE
144400970526     C*
144500970526     C* CASO DI SFL CON RECORDS
144600970526     C*
144700970526     C*    - WRITE RECORD CONTROLLO SFL
144800021217     C                   WRITE     SIG3D06
144900021217     C                   READ      SIG3D06                                99
145000970526     C                   END
145100970526     C*
145200970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
145300021217     C                   READ      SIG3D03                                99
145400970526     C*
145500970526     C                   ENDSR
145600970526     C*------------------------------------------------------------------------*
145700970526     C* SNDMSG - MANDA MESSAGGIO
145800970526     C*------------------------------------------------------------------------*
145900970526     C     SNDMSG        BEGSR
146000970526     C*
146100970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
146200970526     C                   MOVEL     '1'           *IN40
146300021217     C                   WRITE     SIG3DMC
146400970526     C                   MOVEL     '0'           *IN40
146500970526     C*
146600970526     C* VISUALIZZAZIONE MESSAGGIO
146700970526     C                   Z-ADD     0             MSGULT
146800970526     C                   CALL      'XMSG'        XMSGPA
146900970526     C     DSMSER        IFEQ      0
147000970526     C                   MOVEL     DSMSKE        MSGKEY
147100970526     C                   END
147200970526     C                   Z-ADD     1             MSGULT
147300021217     C                   WRITE     SIG3DMS                                      SFL MESS
147400021217     C                   WRITE     SIG3DMC                                      CONTR MESS
147500970526     C*
147600970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
147700970526     C                   MOVEL     *BLANKS       DSMSMS
147800970526     C*
147900970526     C                   ENDSR
148000970526     C*------------------------------------------------------------------------*
148100970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
148200970526     C*------------------------------------------------------------------------*
148300970526     C     FUNCON        BEGSR
148400970526     C*
148500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
148600970526     C                   EXSR      FUNVAR
148700970526     C*
148800970526     C* PULISCO CAMPI DI LAVORO
148900970526     C                   Z-ADD     0             I                 5 0
149000970526     C                   Z-ADD     0             W                 5 0
149100970526     C*
149200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
149300970526     C     FUNKEY        IFNE      *BLANKS
149400970526     C     1             DO        24            I
149500970526     C                   MOVEL     FUD(I)        FUDRIG
149600970526     C*
149700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
149800970526     C     FUNKEY        IFEQ      FUDFUN
149900970526     C     FUDATT        ANDEQ     'S'
150000970526     C                   Z-ADD     I             W
150100970526     C                   END
150200970526     C                   END                                                    DO WHILE
150300970526     C*
150400970526     C* FUNZIONE NON ATTIVA (ERRORE)
150500970526     C     W             IFEQ      0
150600970526     C*
150700970526     C* EMISSIONE MESSAGGIO DI ERRORE
150800970526     C                   MOVEL     MSG(2)        DSMSMS
150900970526     C                   END                                                    W=0
151000970526     C                   END                                                    FUNKEY NE *BLANKS
151100970526     C*
151200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
151300970526     C     W             IFNE      0
151400970526     C     FUNKEY        OREQ      *BLANKS
151500970526     C*
151600970526     C     FUNKEY        IFEQ      *BLANKS
151700970526     C                   EXSR      ENTGES
151800970526     C                   END
151900970526     C     FUNKEY        IFEQ      '03'
152000970526     C                   EXSR      F03GES
152100970526     C                   END
152200970526     C     FUNKEY        IFEQ      '05'
152300970526     C                   EXSR      F05GES
152400970526     C                   END
152500970526     C     FUNKEY        IFEQ      '06'
152600970526     C                   EXSR      F06GES
152700970526     C                   END
152800970526     C     FUNKEY        IFEQ      '12'
152900970526     C                   EXSR      F12GES
153000970526     C                   END
153100970528     C     FUNKEY        IFEQ      '21'
153200970528     C                   EXSR      F21GES
153300970528     C                   END
153400970528     C     FUNKEY        IFEQ      '22'
153500970528     C                   EXSR      F22GES
153600970528     C                   END
153700970526     C     FUNKEY        IFEQ      '23'
153800970526     C                   EXSR      F23GES
153900970526     C                   END
154000970526     C     FUNKEY        IFEQ      '24'
154100970526     C                   EXSR      F24GES
154200970526     C                   END
154300970526     C     FUNKEY        IFEQ      'RU'
154400970526     C                   EXSR      RUPGES
154500970526     C                   END
154600970526     C*
154700970526     C                   END
154800970526     C*
154900970526     C                   ENDSR
155000970526     C*------------------------------------------------------------------------*
155100970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
155200970526     C*------------------------------------------------------------------------*
155300970526     C     FUNVAR        BEGSR
155400970526     C*
155500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
155600970526     C     KEY           IFEQ      ENTER
155700970526     C                   MOVEL     *BLANKS       FUNKEY
155800970526     C                   END
155900970526     C     KEY           IFEQ      F1
156000970526     C                   MOVEL     '01'          FUNKEY
156100970526     C                   END
156200970526     C     KEY           IFEQ      F2
156300970526     C                   MOVEL     '02'          FUNKEY
156400970526     C                   END
156500970526     C     KEY           IFEQ      F3
156600970526     C                   MOVEL     '03'          FUNKEY
156700970526     C                   END
156800970526     C     KEY           IFEQ      F4
156900970526     C                   MOVEL     '04'          FUNKEY
157000970526     C                   END
157100970526     C     KEY           IFEQ      F5
157200970526     C                   MOVEL     '05'          FUNKEY
157300970526     C                   END
157400970526     C     KEY           IFEQ      F6
157500970526     C                   MOVEL     '06'          FUNKEY
157600970526     C                   END
157700970526     C     KEY           IFEQ      F7
157800970526     C                   MOVEL     '07'          FUNKEY
157900970526     C                   END
158000970526     C     KEY           IFEQ      F8
158100970526     C                   MOVEL     '08'          FUNKEY
158200970526     C                   END
158300970526     C     KEY           IFEQ      F9
158400970526     C                   MOVEL     '09'          FUNKEY
158500970526     C                   END
158600970526     C     KEY           IFEQ      F10
158700970526     C                   MOVEL     '10'          FUNKEY
158800970526     C                   END
158900970526     C     KEY           IFEQ      F11
159000970526     C                   MOVEL     '11'          FUNKEY
159100970526     C                   END
159200970526     C     KEY           IFEQ      F12
159300970526     C                   MOVEL     '12'          FUNKEY
159400970526     C                   END
159500970526     C     KEY           IFEQ      F13
159600970526     C                   MOVEL     '13'          FUNKEY
159700970526     C                   END
159800970526     C     KEY           IFEQ      F14
159900970526     C                   MOVEL     '14'          FUNKEY
160000970526     C                   END
160100970526     C     KEY           IFEQ      F15
160200970526     C                   MOVEL     '15'          FUNKEY
160300970526     C                   END
160400970526     C     KEY           IFEQ      F16
160500970526     C                   MOVEL     '16'          FUNKEY
160600970526     C                   END
160700970526     C     KEY           IFEQ      F17
160800970526     C                   MOVEL     '17'          FUNKEY
160900970526     C                   END
161000970526     C     KEY           IFEQ      F18
161100970526     C                   MOVEL     '18'          FUNKEY
161200970526     C                   END
161300970526     C     KEY           IFEQ      F19
161400970526     C                   MOVEL     '19'          FUNKEY
161500970526     C                   END
161600970526     C     KEY           IFEQ      F20
161700970526     C                   MOVEL     '20'          FUNKEY
161800970526     C                   END
161900970526     C     KEY           IFEQ      F21
162000970526     C                   MOVEL     '21'          FUNKEY
162100970526     C                   END
162200970526     C     KEY           IFEQ      F22
162300970526     C                   MOVEL     '22'          FUNKEY
162400970526     C                   END
162500970526     C     KEY           IFEQ      F23
162600970526     C                   MOVEL     '23'          FUNKEY
162700970526     C                   END
162800970526     C     KEY           IFEQ      F24
162900970526     C                   MOVEL     '24'          FUNKEY
163000970526     C                   END
163100970526     C     KEY           IFEQ      ROLLUP
163200970526     C                   MOVEL     'RU'          FUNKEY
163300970526     C                   END
163400970526     C     KEY           IFEQ      HELP
163500970526     C                   MOVEL     'HP'          FUNKEY
163600970526     C                   END
163700970526     C*
163800970526     C                   ENDSR
163900970526     C*------------------------------------------------------------------------*
164000970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
164100970526     C*------------------------------------------------------------------------*
164200970526     C     FUNGES        BEGSR
164300970526     C*
164400970526     C* PULISCO CAMPI DI LAVORO
164500970526     C                   Z-ADD     0             W                 5 0
164600970526     C*
164700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
164800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
164900970526     C     1             DO        24            I
165000970526     C                   MOVEL     FUD(I)        FUDRIG
165100970526     C*
165200970526     C* PULIZIA FUNZIONI PRECEDENTI
165300970526     C                   MOVEL     *BLANKS       FUV(I)
165400970526     C*
165500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
165600970526     C* (SALTA F23 E F24)
165700970526     C     FUDATT        IFEQ      'S'
165800970526     C     FUDFUN        ANDNE     '23'
165900970526     C     FUDFUN        ANDNE     '24'
166000970526     C                   ADD       1             W
166100970526     C                   MOVEL     FUDDES        FUV(W)
166200970526     C                   END
166300970526     C                   END                                                    DO WHILE
166400970526     C*
166500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
166600970526     C* ALTRIMENTI DISATTIVO F24
166700970526     C     1             DO        24            I
166800970526     C                   MOVEL     FUD(I)        FUDRIG
166900970526     C*
167000970526     C     FUDFUN        IFEQ      '24'
167100970526     C     W             IFGT      6
167200970526     C                   MOVEL     'S'           FUDATT
167300970526     C                   ELSE
167400970526     C                   MOVEL     *BLANKS       FUDATT
167500970526     C                   END                                                    W GT 8
167600970526     C                   MOVEL     FUDRIG        FUD(I)
167700970526     C                   END
167800970526     C                   END                                                    DO WHILE
167900970526     C*
168000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
168100970526     C     W             DIV       6             I
168200970526     C                   MVR                     Z
168300970526     C*
168400970526     C     Z             IFNE      0
168500970526     C     I             ADD       1             Z
168600970526     C                   ELSE
168700970526     C                   Z-ADD     I             Z
168800970526     C                   END
168900970526     C*
169000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
169100970526     C                   ADD       1             FUNGRU
169200970526     C     FUNGRU        IFGT      Z
169300970526     C     FUNGRU        ORLT      1
169400970526     C                   Z-ADD     1             FUNGRU
169500970526     C                   END
169600970526     C*
169700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
169800970526     C     FUNGRU        IFEQ      1
169900970526     C                   MOVEL     FUV(1)        FUN001
170000970526     C                   MOVEL     FUV(2)        FUN002
170100970526     C                   MOVEL     FUV(3)        FUN003
170200970526     C                   MOVEL     FUV(4)        FUN004
170300970526     C                   MOVEL     FUV(5)        FUN005
170400970526     C                   MOVEL     FUV(6)        FUN006
170500970526     C                   END
170600970526     C*
170700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
170800970526     C     FUNGRU        IFEQ      2
170900970526     C                   MOVEL     FUV(7)        FUN001
171000970526     C                   MOVEL     FUV(8)        FUN002
171100970526     C                   MOVEL     FUV(9)        FUN003
171200970526     C                   MOVEL     FUV(10)       FUN004
171300970526     C                   MOVEL     FUV(11)       FUN005
171400970526     C                   MOVEL     FUV(12)       FUN006
171500970526     C                   END
171600970526     C*
171700970526     C* TERZO GRUPPO DI 6 FUNZIONI
171800970526     C     FUNGRU        IFEQ      3
171900970526     C                   MOVEL     FUV(13)       FUN001
172000970526     C                   MOVEL     FUV(14)       FUN002
172100970526     C                   MOVEL     FUV(15)       FUN003
172200970526     C                   MOVEL     FUV(16)       FUN004
172300970526     C                   MOVEL     FUV(17)       FUN005
172400970526     C                   MOVEL     FUV(18)       FUN006
172500970526     C                   END
172600970526     C*
172700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
172800970526     C     FUNGRU        IFEQ      4
172900970526     C                   MOVEL     FUV(19)       FUN001
173000970526     C                   MOVEL     FUV(20)       FUN002
173100970526     C                   MOVEL     FUV(21)       FUN003
173200970526     C                   MOVEL     FUV(22)       FUN004
173300970526     C                   MOVEL     *BLANKS       FUN005
173400970526     C                   MOVEL     *BLANKS       FUN006
173500970526     C                   END
173600970526     C*
173700970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
173800970526     C                   MOVEL     *BLANKS       FUN007
173900970526     C                   MOVEL     *BLANKS       FUN008
174000970526     C*
174100970526     C     1             DO        24            I
174200970526     C                   MOVEL     FUD(I)        FUDRIG
174300970526     C     FUDFUN        IFEQ      '23'
174400970526     C     FUDATT        ANDEQ     'S'
174500970526     C                   MOVEL     FUDDES        FUN007
174600970526     C                   END
174700970526     C     FUDFUN        IFEQ      '24'
174800970526     C     FUDATT        ANDEQ     'S'
174900970526     C                   MOVEL     FUDDES        FUN008
175000970526     C                   END
175100970526     C                   END                                                    DO
175200970526     C*
175300970526     C                   ENDSR
175400970526     C*------------------------------------------------------------------------*
175500970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
175600970526     C*------------------------------------------------------------------------*
175700970526     C     OPZGES        BEGSR
175800970526     C*
175900970526     C* PULISCO CAMPI DI LAVORO
176000970526     C                   Z-ADD     0             W                 5 0
176100970526     C*
176200970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
176300970526     C* E CARICO TABELLA OPZIONI ATTIVE
176400991115     C     1             DO        24            I
176500970526     C                   MOVEL     OPD(I)        OPDRIG
176600970526     C*
176700970526     C* PULIZIA OPZIONI PRECEDENTI
176800970526     C                   MOVEL     *BLANKS       OPV(I)
176900970526     C*
177000970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
177100970526     C     OPDATT        IFEQ      'S'
177200970526     C                   ADD       1             W
177300970526     C                   MOVEL     OPDDES        OPV(W)
177400970526     C                   END
177500970526     C                   END                                                    DO WHILE
177600970526     C*
177700970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
177800970526     C* ALTRIMENTI DISATTIVO F23
177900991115     C     1             DO        24            I
178000970526     C                   MOVEL     FUD(I)        FUDRIG
178100970526     C*
178200970526     C     FUDFUN        IFEQ      '23'
178300970526     C     W             IFGT      8
178400970526     C                   MOVEL     'S'           FUDATT
178500970526     C                   ELSE
178600970526     C                   MOVEL     *BLANKS       FUDATT
178700970526     C                   END                                                    W GT 8
178800970526     C                   MOVEL     FUDRIG        FUD(I)
178900970526     C                   END
179000970526     C                   END                                                    DO WHILE
179100970526     C*
179200970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
179300970526     C     W             DIV       8             I
179400970526     C                   MVR                     Z                 5 0
179500970526     C*
179600970526     C     Z             IFNE      0
179700970526     C     I             ADD       1             Z
179800970526     C                   ELSE
179900970526     C                   Z-ADD     I             Z
180000970526     C                   END
180100970526     C*
180200970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
180300970526     C                   ADD       1             OPZGRU
180400970526     C     OPZGRU        IFGT      Z
180500970526     C     OPZGRU        ORLT      1
180600970526     C                   Z-ADD     1             OPZGRU
180700970526     C                   END
180800970526     C*
180900970526     C* PRIMO GRUPPO DI 8 OPZIONI
181000970526     C     OPZGRU        IFEQ      1
181100970526     C                   MOVEL     OPV(1)        OPZ001
181200970526     C                   MOVEL     OPV(2)        OPZ002
181300970526     C                   MOVEL     OPV(3)        OPZ003
181400970526     C                   MOVEL     OPV(4)        OPZ004
181500970526     C                   MOVEL     OPV(5)        OPZ005
181600970526     C                   MOVEL     OPV(6)        OPZ006
181700970526     C                   MOVEL     OPV(7)        OPZ007
181800970526     C                   MOVEL     OPV(8)        OPZ008
181900970526     C                   END
182000970526     C*
182100970526     C* SECONDO GRUPPO DI 8 OPZIONI
182200970526     C     OPZGRU        IFEQ      2
182300970526     C                   MOVEL     OPV(9)        OPZ001
182400970526     C                   MOVEL     OPV(10)       OPZ002
182500970526     C                   MOVEL     OPV(11)       OPZ003
182600970526     C                   MOVEL     OPV(12)       OPZ004
182700970526     C                   MOVEL     OPV(13)       OPZ005
182800970526     C                   MOVEL     OPV(14)       OPZ006
182900970526     C                   MOVEL     OPV(15)       OPZ007
183000970526     C                   MOVEL     OPV(16)       OPZ008
183100970526     C                   END
183200970526     C*
183300970526     C* TERZO GRUPPO DI 8 OPZIONI
183400970526     C     OPZGRU        IFEQ      3
183500970526     C                   MOVEL     OPV(17)       OPZ001
183600970526     C                   MOVEL     OPV(18)       OPZ002
183700970526     C                   MOVEL     OPV(19)       OPZ003
183800970526     C                   MOVEL     OPV(20)       OPZ004
183900970526     C                   MOVEL     OPV(21)       OPZ005
184000970526     C                   MOVEL     OPV(22)       OPZ006
184100970526     C                   MOVEL     OPV(23)       OPZ007
184200970526     C                   MOVEL     OPV(24)       OPZ008
184300970526     C                   END
184400970526     C*
184500970526     C                   ENDSR
184600970526     C*------------------------------------------------------------------------*
184700970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
184800970526     C*------------------------------------------------------------------------*
184900970526     C     OPZCON        BEGSR
185000970526     C*
185100970526     C* PULISCO CAMPI DI LAVORO
185200970526     C                   Z-ADD     0             I                 5 0
185300970526     C                   Z-ADD     0             W                 5 0
185400970526     C*
185500970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
185600970526     C     1             DO        24            I
185700970526     C                   MOVEL     OPD(I)        OPDRIG
185800970526     C*
185900970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
186000970526     C     OPZKEY        IFEQ      OPDOPZ
186100970526     C     OPZKEY        OREQ      OPDOP1
186200970526     C*
186300970526     C     OPDATT        IFEQ      'S'
186400970526     C                   MOVEL     OPDOPZ        OPZKEY
186500970526     C                   Z-ADD     I             W
186600970526     C                   END
186700970526     C                   END
186800970526     C*
186900970526     C                   END                                                    DO WHILE
187000970526     C*
187100970526     C* OPZIONE NON ATTIVA (ERRORE)
187200970526     C     W             IFEQ      0
187300970526     C*
187400970526     C* OPZIONE NON OK
187500970526     C                   Z-ADD     1             OPZOK
187600970526     C*
187700970526     C* EMISSIONE MESSAGGIO DI ERRORE
187800970526     C                   MOVEL     MSG(1)        DSMSMS
187900970526     C                   ELSE
188000970526     C*
188100970526     C* OPZIONE ATTIVA (PROCEDI ...)
188200970526     C*
188300021211     C                   MOVEL     *BLANKS       PMSG
188400160727     C     OPZKEY        IFEQ      ' 4'
188500160727     C                   EXSR      O04GES
188600160727     C                   END
188700970526     C     OPZKEY        IFEQ      ' 5'
188800970526     C                   EXSR      O05GES
188900970526     C                   END
189000021211     C                   MOVEL     PMSG          DSMSMS
189100021211     C                   END                                                    OPZ.NON ATTIVA
189200970526     C*
189300970526     C                   ENDSR
189400970526     C*------------------------------------------------------------------------*
189500970526     C* NOTE COSTRUZIONE SCHIERE
189600970526     C*------------------------------------------------------------------------*
189700970526     C*
189800970526     C* -MSG-MESSAGGI
189900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
190000970526     C* (NON TOCCARE MSG,1 E MSG,2)
190100970526     C*
190200970526     C* -CMD-COMANDI
190300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
190400970526     C*
190500970526     C* -OPD-OPZIONI
190600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
190700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
190800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
190900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
191000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
191100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
191200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
191300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
191400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
191500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
191600970526     C*
191700970526     C* -FUD-FUNZIONI
191800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
191900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
192000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
192100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
192200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
192300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
192400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
192500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
192600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
192700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
192800991108     C*
192900991115     C* -STS-CODICI STATI DEI RECORD DI LOG
193000991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
193100991110     C* ================> IMPORTANTE <=================
193200991115     C* per codici di errore in -STS- indicare in -DES- sempre
193300991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
193400991110     C* ERR
193500991108     C*
193600970526     O*------------------------------------------------------------------------*
193700970526** DEC - DECODIFICHE
193800970526    GESTIONE                                                     1
193900000407 INTERROGAZIONE                                                  2
194000970530     SCELTA                                                      3
1941009705264
1942009705265
194300970526** MSG - MESSAGGI
194400021217TISIG3R- Opzione non gestita.                                                  1
194500021217TISIG3R- Tasto funzionale non gestito.                                         2
194600021217TISIG3R- Non abilitato all inserimento in filiale.                             3
194700070509TISIG3R- Soggetto movimento non in gestione all'utente. Enter per forzare!     4
194800040225TISIG3R- Presenti troppi record, alcuni non verranno visualizzati.             5
194900021217TISIG3R- 6                                                                     6
195000021217TISIG3R- 7                                                                     7
195100021217TISIG3R- 8                                                                     8
195200021217TISIG3R- 9                                                                     9
195300021217TISIG3R- 10                                                                   10
195400021217TISIG3R- 11                                                                   11
195500021217TISIG3R- 12                                                                   12
195600021217TISIG3R- 13                                                                   13
195700021217TISIG3R- 14                                                                   14
195800021217TISIG3R- 15                                                                   15
195900021217TISIG3R- 16                                                                   16
196000021217TISIG3R- 17                                                                   17
196100021217TISIG3R- 18                                                                   18
196200021217TISIG3R- 19                                                                   19
196300021217TISIG3R- 20                                                                   20
196400970528** CMD - COMANDI CL
196500021211
196600021211
196700991110
196800970528
196900970528
197000970526** OPD - OPZIONI
197100000420 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
197200000407 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
197300021211 3=Copia           -3 -     (S/N)=N                        opz. 03¬
197400000407 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
197500000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
197600991115                   -  -     (S/N)=N                        opz. 06¬
197700021211                   -  -     (S/N)=N                        opz. 07¬
197800991110                   -  -     (S/N)=N                        opz. 08¬
197900991110                   -  -     (S/N)=N                        opz. 09¬
198000991110                   -  -     (S/N)=N                        opz. 10¬
198100000407                   -  -     (S/N)=N                        opz. 11¬
198200970526                   -  -     (S/N)=N                        opz. 12¬
198300970526                   -  -     (S/N)=N                        opz. 13¬
198400000407                   -  -     (S/N)=N                        opz. 14¬
198500000407                   -  -     (S/N)=N                        opz. 15¬
198600970526                   -  -     (S/N)=N                        opz. 16¬
198700970526                   -  -     (S/N)=N                        opz. 17¬
198800970526                   -  -     (S/N)=N                        opz. 18¬
198900970526                   -  -     (S/N)=N                        opz. 19¬
199000970526                   -  -     (S/N)=N                        opz. 20¬
199100970526                   -  -     (S/N)=N                        opz. 21¬
199200970526                   -  -     (S/N)=N                        opz. 22¬
199300970526                   -  -     (S/N)=N                        opz. 23¬
199400000407                   -  -     (S/N)=N                        opz. 25¬
199500970526** FUD - FUNZIONI
199600970526                   -        (S/N)=N                        funz.01¬
199700970526                   -        (S/N)=N                        funz.02¬
199800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
199900970526                   -        (S/N)=N                        funz.04¬
200000970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
200100101006F06=Inserimento    -        (S/N)=N  <==                   funz.06¬
200200970526                   -        (S/N)=N                        funz.07¬
200300970527                   -        (S/N)=N                        funz.08¬
200400030325F09=Altri dati     -        (S/N)=S  <== S-FISSO           funz.09¬
200500970527                   -        (S/N)=N                        funz.10¬
200600970526                   -        (S/N)=N                        funz.11¬
200700970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
200800991116                   -        (S/N)=N                        funz.13¬
200900970526                   -        (S/N)=N                        funz.14¬
201000970526                   -        (S/N)=N                        funz.15¬
201100970526                   -        (S/N)=N                        funz.16¬
201200970526                   -        (S/N)=N                        funz.17¬
201300970526                   -        (S/N)=N                        funz.18¬
201400970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
201500970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
201600000407                   -        (S/N)=N                        funz.21¬
201700991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
201800991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
201900991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
