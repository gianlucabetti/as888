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
029600991014     C*------------------------------------------------------------------------*
029700000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
029800970526     C*------------------------------------------------------------------------*
029900970526     C     O05GES        BEGSR
030000000407     C*
030100030704     C* ESEGUO ROUTINE DI CONTROLLO
030200030704     C                   EXSR      DECODI
030300030704     C*
030400021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030500021217     C                   MOVEL     'O05'         POP1
030600030122     C                   Z-ADD     VIHPRG        PPRG
030700040702     C                   EXSR      PASPAR
030800021217     C                   CALL      'TISIG4R'
030900000407     C                   PARM                    KPJBA
031000070509     c                   else
031100070509      * se errore sul cliente forzo opzione non ok così gestico la forzatura
031200070509     C                   Z-ADD     1             OPZOK
031300021211     C                   ENDIF
031400000407     C*
031500970526     C                   ENDSR
031600030704     C*------------------------------------------------------------------------*
031700030704     C* DECODI - CONTROLLA CAMPI E DECODIFICA
031800030704     C*------------------------------------------------------------------------*
031900030704     C     DECODI        BEGSR
032000030704     C*
032100030704     C                   MOVEL     '0'           ERRL00
032200030704     C*--------------------
032300030704     C* CONTROLLO AUTORIZZAZIONI UTENTE SU CODICE SOGGETTO MOVIMENTO "PRINCIPALE"
032400030704     C*--------------------
032500030704     C     ERRL00        IFEQ      '0'
032600030704     C* I CONTROLLI SI RIFERISCONO SOLO A SOGGETTI DI TIPO "F" O DI TIPO "C"
032700030704     C                   IF        VIHTIP = 'F'
032800030704     C                   MOVEL     VIDSOG        WN3               3
032900030704     C                   SETOFF                                           38
033000030704     C     WN3           LOOKUP    L1                                     38
033100030704     C     *IN38         IFEQ      *OFF
033200070509     c     vihforza      andeq     *off
033300030704     C                   MOVEL     '1'           ERRL00
033400030704     C                   MOVEL     MSG(4)        PMSG
033500070509     c                   eval      vihforza = *on
033600030704     C                   ENDIF
033700030704     C                   ENDIF
033800030704     C                   IF        VIHTIP = 'C'
033900030704     C                   MOVEL     VIDSOG        WA4               4
034000030704     C                   MOVE      WA4           WN3               3
034100030704     C                   SETOFF                                           38
034200030704     C     WN3           LOOKUP    L1                                     38
034300030704     C     *IN38         IFEQ      *OFF
034400070509     c     vihforza      andeq     *off
034500030704     C                   MOVEL     '1'           ERRL00
034600030704     C                   MOVEL     MSG(4)        PMSG
034700070509     c                   eval      vihforza = *on
034800030704     C                   ENDIF
034900030704     C                   ENDIF
035000030704     C                   ENDIF
035100030704     C*
035200030704     C                   ENDSR
035300991028     C*------------------------------------------------------------------------*
035400991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
035500991112     C*------------------------------------------------------------------------*
035600991112     C     PASPAR        BEGSR
035700991112     C*
035800000407     C                   MOVEL     PARAM         KPJBU
035900991112     C*
036000991112     C                   ENDSR
036100970526     C*------------------------------------------------------------------------*
036200970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
036300970526     C*------------------------------------------------------------------------*
036400970526     C     NEWPOS        BEGSR
036500970526     C*
036600970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
036700970526     C*         1= RICHIESTO POSIZIONAMENTO
036800970526     C                   Z-ADD     0             SWPOSI            7 0
036900970526     C*
037000991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
037100970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
037200970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
037300970526     C                   Z-ADD     0             SWMODI            1 0
037400970530     C*
037500970530     C* REIMPOSTA INDICATORI DI ERRORE
037600021217     C                   SETOFF                                       818283
037700021217     C                   SETOFF                                       848586
037800030128     C                   SETOFF                                       878889
037900030305     C                   SETOFF                                       80
038000970526     C*
038100970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
038200021230     C                   IF        VIDDA1 <> depda1 OR
038300030128     C                             VIDPD2 <> deppd2 OR
038400030128     C                             VIDPA2 <> deppa2 OR
038500030305     C                             VIDPI2 <> deppi2 OR
038600021230     C                             VIDPO2 <> deppo2 OR
038700021230     C                             VIDKS2 <> depks2 OR
038800021230     C                             VIDDS2 <> depds2 OR
038900021230     C                             VIDCA2 <> depca2 OR
039000021230     C                             VIDCO2 <> depco2 OR
039100021230     C                             VIDUT2 <> deput2 OR
039200030305     C                             %SCAN('?':VIDPI2) > 0 OR
039300021217     C                             %SCAN('?':VIDPO2) > 0 OR
039400021217     C                             %SCAN('?':VIDKS2) > 0 OR
039500021217     C                             %SCAN('?':VIDDS2) > 0 OR
039600021217     C                             %SCAN('?':VIDCA2) > 0 OR
039700021217     C                             %SCAN('?':VIDCO2) > 0
039800150403     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
039900021211     C     ERRL00        IFEQ      '0'                                          *NO ERORI
040000970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
040100021217     C                   Z-ADD     VIDDA1        depda1                         *DEPOSITI
040200030128     C                   MOVEL     VIDPD2        deppd2                         *DEPOSITI
040300030128     C                   MOVEL     VIDPA2        deppa2                         *DEPOSITI
040400030305     C                   MOVEL     VIDPI2        deppi2                         *DEPOSITI
040500021217     C                   MOVEL     VIDPO2        deppo2                         *DEPOSITI
040600021217     C                   MOVEL     VIDKS2        depks2                         *DEPOSITI
040700021217     C                   MOVEL     VIDDS2        depds2                         *DEPOSITI
040800021217     C                   MOVEL     VIDCA2        depca2                         *DEPOSITI
040900021217     C                   MOVEL     VIDCO2        depco2                         *DEPOSITI
041000021217     C                   MOVEL     VIDUT2        deput2                         *DEPOSITI
041100021211     C                   ENDIF
041200021211     C                   ENDIF
041300970526     C*
041400970526     C                   ENDSR
041500021217     C*------------------------------------------------------------------------*
041600021217     C* CHKKSC - CONTROLLA CODICE CLIENTE
041700021217     C*------------------------------------------------------------------------*
041800021217     C     CHKKSC        BEGSR
041900021217     C*
042000021217     C                   MOVEL(P)  VIDKS2        CHKKS2            7
042100021217     C*
042200021217     C* RICHIESTO ELENCO
042300021217     C                   SETOFF                                           28
042400021217     C     '?'           SCAN      CHKKS2                                 28
042500021217     C     *IN28         IFEQ      *ON
042600021217     C*
042700021217     C* RICHIESTO ELENCO CLIENTI
042800021217     C                   CLEAR                   DSXC
042900030124     C                   Z-ADD     DUTKCI        DXCCAP
043000021217     C                   CALL      'XCLIR'
043100021217     C                   PARM                    DSXC
043200021217     C* SCOMPATTA
043300021217     C                   MOVEA     DXCKSC        KSA
043400021217     C     1             DO        30            J                 3 0
043500021217     C                   MOVE      KSA(J)        DSKSA
043600021217     C                   Z-ADD     DSKSN         KSC(J)
043700021217     C                   ENDDO
043800030305     C                   IF        KSC(1) > *ZEROS
043900021217     C                   MOVE(P)   KSC(1)        CHKKS2
044000030305     C                   ELSE
044100030305     C                   MOVEL     *BLANKS       CHKKS2
044200030305     C                   ENDIF
044300021217     C                   MOVEL     '1'           $TESVD            1
044400021217     C                   ELSE
044500021217     C*
044600021217     C* CODICE CLIENTE NUMERICO
044700021217     C     CHKKS2        IFLT      *ZEROS
044800021217     C                   MOVEL     '1'           W_ERR             1
044900021217     C                   ELSE
045000021217     C*
045100021217     C* SOLO CARATTERI NUMERICI
045200021217     C   28              MOVE      *all'0'       CHKKS2
045300021217     C   28              MOVE      VIDKS2        CHKKS2
045400021217     C                   SETOFF                                       28
045500021217     C                   TESTN                   CHKKS2               28
045600021217     C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
045700021217     C                   MOVEL     '1'           W_ERR
045800021217     C                   ELSE                                                   *E'NUMERO DA TESTN
045900021217     C                   MOVE      CHKKS2        W001              1
046000021217     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
046100021217     C                   MOVEL     '1'           W_ERR
046200021217     C                   ELSE                                                   *NUMERO
046300021217     C*
046400021217     C* CONTROLLA ESISTENZA CLIENTE
046500021217     C                   EXSR      DECTIB
046600021217     C                   ENDIF
046700021217     C                   ENDIF
046800021217     C                   ENDIF
046900021217     C                   ENDIF
047000021217     C*
047100021217     C                   ENDSR
047200021217     C*------------------------------------------------------------------------*
047300021217     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
047400021217     C*------------------------------------------------------------------------*
047500021217     C     DECTIB        BEGSR
047600021217     C*
047700021217     C                   CLEAR                   BS69DS
047800021217     C                   CLEAR                   ACODS
047900021217     C                   MOVEL     KNSIF         I69SIF
048000021217     C                   MOVE(P)   CHKKS2        I69KAC
048100021217     C                   CALL      'TIBS69R'
048200021217     C                   PARM                    BS69DS
048300021217     C                   PARM                    ACODS
048400021217     C                   PARM                    INDDS
048500021217     C                   PARM                    CLPDS
048600021217     C                   PARM                    CLSDS
048700021217     C     O69ERR        IFNE      '1'
048800021217     C                   ELSE
048900021217     C                   MOVEL     '1'           W_ERR
049000021217     C                   ENDIF
049100021217     C*
049200021217     C                   ENDSR
049300021217     C*------------------------------------------------------------------------*
049400021217     C* CHKDST - CONTROLLA CODICE DESTINATARIO
049500021217     C*------------------------------------------------------------------------*
049600021217     C     CHKDST        BEGSR
049700021217     C*
049800021217     C* RICHIESTO ELENCO
049900021217     C                   SETOFF                                           28
050000021217     C     '?'           SCAN      VIDDS2                                 28
050100021217     C     *IN28         IFEQ      *ON
050200021217     C*
050300021217     C* RICHIESTO ELENCO DESTINATARI
050400021217     C                   MOVEL     KPJBU         SAVKPJBU
050500021217     C                   MOVEL(P)  'P01'         KPJBU
050600021217     C                   CALL      'TISIT1R'
050700021217     C                   PARM                    KPJBA
050800021217     C                   EVAL      VIDDS2 = %subst(KPJBU:4:7)
050900021217     C                   MOVEL     SAVKPJBU      KPJBU
051000021217     C                   ELSE
051100021217     C                   MOVE(P)   VIDDS2        CHKDS2            7 0
051200021217     C     CHKDS2        SETLL     azdst04l                               55
051300021217     C                   IF        *IN55 = *OFF
051400021217     C                   EVAL      W_ERR = '1'
051500021217     C                   ENDIF
051600021217     C                   ENDIF
051700021217     C*
051800021217     C                   ENDSR
051900021217     C*------------------------------------------------------------------------*
052000021217     C* CHKCOD - CONTROLLA CODICE ARTICOLO
052100021217     C*------------------------------------------------------------------------*
052200021217     C     CHKCOD        BEGSR
052300021217     C*
052400021217     C* RICHIESTO ELENCO
052500021217     C                   SETOFF                                           28
052600021217     C     '?'           SCAN      VIDCO2                                 28
052700021217     C     *IN28         IFEQ      *ON
052800021217     C*
052900021217     C* RICHIESTO ELENCO ARTICOLI
053000021217     C                   MOVEL     KPJBU         SAVKPJBU
053100021217     C                   MOVEL(P)  'P01'         KPJBU
053200021217     C                   CALL      'TISIG1R'
053300021217     C                   PARM                    KPJBA
053400021217     C                   EVAL      VIDCO2 = %subst(KPJBU:4:15)
053500021217     C                   MOVEL     SAVKPJBU      KPJBU
053600021217     C                   ELSE
053700021217     C                   MOVE(P)   VIDCO2        CHKCO2           15
053800021217     C     CHKCO2        SETLL     azagb01l                               55
053900021217     C                   IF        *IN55 = *OFF
054000021217     C                   EVAL      W_ERR = '1'
054100021217     C                   ENDIF
054200021217     C                   ENDIF
054300021217     C*
054400021217     C                   ENDSR
054500021217     C*------------------------------------------------------------------------*
054600021217     C* CHKPO - CONTROLLA CODICE P.O.
054700021217     C*------------------------------------------------------------------------*
054800021217     C     CHKPO         BEGSR
054900021217     C*
055000021217     C* RICHIESTO ELENCO
055100021217     C                   SETOFF                                           28
055200030305     C     '?'           SCAN      wChkPO                                 28
055300021217     C     *IN28         IFEQ      *ON
055400021217     C*
055500021217     C* RICHIESTO ELENCO P.O.
055600021217     C                   CALL      'TNSD24R'
055700021217     C                   PARM                    §COD              3
055800021217     C                   PARM      'F'           §TIP              1
055900021217     C                   PARM                    §§DES            25
056000021217     C                   PARM                    §§PRA             3
056100030305     C                   MOVEL     §COD          wChkPO
056200021217     C                   ELSE
056300030305     C                   MOVE(P)   wChkPO        CHKPO2            3 0
056400030305     C     CHKPO2        SETLL     azorg01l                               55
056500021217     C                   IF        *IN55 = *OFF
056600021217     C                   EVAL      W_ERR = '1'
056700021217     C                   ENDIF
056800021217     C                   ENDIF
056900021217     C*
057000021217     C                   ENDSR
057100021219     C*------------------------------------------------------------------------*
057200021219     C* CHKCAU - CONTROLLA CAUSALE MOVIMENTO
057300021219     C*------------------------------------------------------------------------*
057400021219     C     CHKCAU        BEGSR
057500030123     C*
057600030123     C* RICHIESTO ELENCO
057700030123     C                   SETOFF                                           28
057800030123     C     '?'           SCAN      VIDCA2                                 28
057900030123     C     *IN28         IFEQ      *ON
058000021219     C*
058100030123     C* VALORIZZO I PARAMETRI DI PASSAGGIO
058200030123     C                   CLEAR                   PARAM2
058300030123     C                   EVAL      P2OP1 = 'P01'
058400030123     C                   MOVEL     PARAM2        KPJBU
058500030123     C                   CALL      'TNTB31R'
058600030123     C                   PARM                    KPJBA
058700021219     C*
058800030123     C                   MOVEL     KPJBU         PARAM2
058900030123     C                   MOVEL     P2CAU         VIDCA2
059000030123     C*
059100030123     C* ALTRIMENTI CONTROLLO CODICE IMMESSO
059200030123     C                   ELSE
059300030123     C                   EVAL      tbeCOD = 'GGB'
059400030123     C                   EVAL      tbeKE1 = VIDCA2
059500030123     C     KEYTBEP       CHAIN     tntbe01l
059600030123     C                   IF        %found(tntbe01l)
059700030123     C                   ELSE
059800030123     C                   EVAL      W_ERR = '1'
059900030123     C                   ENDIF
060000030123     C                   ENDIF
060100021219     C*
060200021219     C                   ENDSR
060300970526     C*------------------------------------------------------------------------*
060400021217     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
060500970526     C*------------------------------------------------------------------------*
060600970528     C     SETFIL        BEGSR
060700970526     C*
060800970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
060900021217     C     VIDDA1        IFEQ      *zeros
061000150403     C                   Z-ADD     *hival        KEYDAT
061100991108     C                   ELSE
061200030305     C                   Z-ADD     DA1VID        KEYDAT
061300991108     C                   ENDIF
061400021217     C     WTIPRI        IFEQ      'DAT'
061500030305     C     KEYMGBP       SETLL     azmgb01l                           99
061600021211     C                   ENDIF
061700980107     C*
061800021211     C* LETTURA RECORD
061900021211     C     *IN99         IFEQ      *OFF
062000970526     C                   EXSR      LETFIL
062100021211     C                   ENDIF
062200970526     C*
062300970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
062400021211     C     *IN99         IFEQ      *ON
062500970526     C                   Z-ADD     1             SFLEOF
062600021211     C                   ENDIF
062700970526     C*
062800970526     C                   ENDSR
062900970526     C*------------------------------------------------------------------------*
063000970526     C* LETFIL - LETTURA RECORD
063100970526     C*------------------------------------------------------------------------*
063200970526     C     LETFIL        BEGSR
063300970526     C*
063400970526     C* LEGGE FINO A:
063500970526     C*     - FINE FILE
063600970526     C*     - TROVATO RECORD VALIDO
063700970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
063800021211     C     *IN99         DOWEQ     *OFF
063900970526     C     WRECOK        ANDEQ     'N'
064000970526     C*
064100970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
064200021217     C     WTIPRI        IFEQ      'DAT'                                        *PER DATA/ORA/UTENTE
064300030305     C                   READ      azmgb01l                               99
064400021211     C                   ENDIF
064500970528     C*
064600970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
064700021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
064800970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
064900021211     C                   ELSE
065000970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
065100021211     C                   ENDIF
065200021211     C                   ENDDO
065300970526     C*
065400970526     C                   ENDSR
065500970526     C*------------------------------------------------------------------------*
065600021217     C* CHKREC - CONTROLLA VALIDITA' RECORD
065700970526     C*------------------------------------------------------------------------*
065800970526     C     CHKREC        BEGSR
065900970526     C*
066000970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
066100030128     C* FILTRA SE RICHIESTO PERIODO DA->A
066200030305     C                   if        PD2VID = *zeros AND PA2VID = *zeros
066300030128     C                   else
066400150403     C                   if        mgbdat >= PD2VID  AND mgbDAT <= PA2VID
066500030128     C                   else
066600030128     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
066700030128     C                   endif
066800030128     C                   endif
066900030305     C* FILTRA SE RICHIESTO UN P.O. INSERIMENTO
067000030305     C     VIDPI2        ifgt      *zeros
067100030305     C                   move(p)   VIDPI2        numPI2            3 0
067200030305     C                   if        mgbPOU <> numPI2
067300030305     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
067400030305     C                   endif
067500030305     C                   endif
067600021217     C* FILTRA SE RICHIESTO UN P.O.
067700021217     C     VIDPO2        ifeq      *blanks
067800021230     C     VIDPO2        oreq      *zeros
067900021217     C                   else
068000021218     C                   movel     *all'0'       CHKSOG
068100021218     C                   move      VIDPO2        CHKSOG
068200021230     C***                if        mgbTIP = 'F' AND
068300021230     C                   if        mgbSOG <> CHKSOG
068400021217     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
068500021217     C                   endif
068600021217     C                   endif
068700021217     C* FILTRA SE RICHIESTO UN CLIENTE
068800021217     C     VIDKS2        ifeq      *blanks
068900021230     C     VIDKS2        oreq      *zeros
069000021217     C                   else
069100021218     C                   movel     *all'0'       CHKSOG
069200021218     C                   move      VIDKS2        CHKSOG
069300021230     C***                if        mgbTIP = 'C' AND
069400021230     C                   if        mgbSOG <> CHKSOG
069500021217     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
069600021217     C                   endif
069700021217     C                   endif
069800021217     C* FILTRA SE RICHIESTO UN DESTINATARIO
069900021217     C     VIDDS2        ifeq      *blanks
070000021230     C     VIDDS2        oreq      *zeros
070100021217     C                   else
070200021218     C                   movel     *all'0'       CHKSOG
070300021218     C                   move      VIDDS2        CHKSOG
070400021230     C***                if        mgbTIP = 'D' AND
070500021230     C                   if        mgbSOG <> CHKSOG
070600021217     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
070700021217     C                   endif
070800021217     C                   endif
070900021217     C* FILTRA SE RICHIESTO UNA CAUSALE
071000021217     C     VIDCA2        ifeq      *blanks
071100021217     C                   else
071200021217     C     mgbCAU        ifne      VIDCA2
071300021217     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
071400021217     C                   endif
071500021217     C                   endif
071600021211     C* FILTRA SE RICHIESTO UN TIPO ARTICOLO
071700021217     C     VIDCO2        ifeq      *blanks
071800021211     C                   else
071900021217     C     mgbCOD        ifne      VIDCO2
072000021211     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
072100021211     C                   endif
072200021211     C                   endif
072300021217     C* FILTRA SE RICHIESTO UN UTENTE
072400021217     C     VIDUT2        ifeq      *blanks
072500021217     C                   else
072600021217     C     mgbUTE        ifne      VIDUT2
072700021217     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
072800021217     C                   endif
072900021217     C                   endif
073000991108     C*
073100970526     C                   ENDSR
073200970526     C*------------------------------------------------------------------------*
073300000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
073400970526     C*------------------------------------------------------------------------*
073500970526     C     IRESFL        BEGSR
073600970526     C*
073700030325     C* Reperisco la decodifica del soggetto
073800030325     C* ...cliente
073900030325     C                   IF        mgbTIP = 'C'
074000030325     C                   Z-ADD     1             acoKUT
074100030325     C                   Z-ADD     DUTKCI        acoKCC
074200030325     C                   MOVE(p)   mgbSOG        acoKSC
074300030325     C     KEYACOT       CHAIN     cnaco00f
074400030325     C                   IF        %found(cnaco00f)
074500030325     C                   EVAL      VIDRAG = acoRAG
074600030325     C                   ELSE
074700030325     C                   MOVEL     *all'*'       VIDRAG
074800030325     C                   ENDIF
074900030325     C                   ENDIF
075000030325     C* ...destinatario
075100030325     C                   IF        mgbTIP = 'D'
075200030325     C                   MOVE(p)   mgbSOG        dstCOD
075300030325     C     dstCOD        CHAIN     azdst04l
075400030325     C                   IF        %found(azdst04l)
075500030325     C                   EVAL      VIDRAG = dstRAG
075600030325     C                   ELSE
075700030325     C                   MOVEL     *all'*'       VIDRAG
075800030325     C                   ENDIF
075900030325     C                   ENDIF
076000030325     C* ...P.O.
076100030325     C                   IF        mgbTIP = 'F'
076200030325     C                   MOVE(p)   mgbSOG        orgFIL
076300030325     C     orgFIL        CHAIN     azorg01l
076400030325     C                   IF        %found(azorg01l)
076500030325     C                   EVAL      VIDRAG = orgDES
076600030325     C                   ELSE
076700030325     C                   MOVEL     *all'*'       VIDRAG
076800030325     C                   ENDIF
076900030325     C                   ENDIF
077000030325     C*
077100000407     C                   MOVEL     *blanks       OPZKEY
077200021217     C                   Z-ADD     mgbDAT        VIDDAT
077300021217     C                   Z-ADD     mgbORA        VIDORA
077400021217     C                   MOVEL(P)  mgbCAU        VIDCAU
077500021217     C                   MOVEL(P)  mgbTIM        VIDTIM
077600021217     C                   MOVEL(P)  mgbCOD        VIDCOD
077700021217     C                   Z-ADD     mgbQTA        VIDQTA
077800021218     C                   MOVEL(P)  mgbSOG        VIDSOG
077900150408     C*                  IF        mgbTIP = 'C'
078000150408     C*                  MOVEL(P)  'CLI'         VIDDSO
078100150408     C*                  ENDIF
078200150408     C*                  IF        mgbTIP = 'D'
078300150408     C*                  MOVEL(P)  'DES'         VIDDSO
078400150408     C*                  ENDIF
078500150408     C*                  IF        mgbTIP = 'F'
078600150408     C*                  MOVEL(P)  'PO'          VIDDSO
078700150408     C*                  ENDIF
078800150408     C                   MOVEL     mgbpou        VIddso
078900021217     C                   MOVEL     mgbUTE        VIHUTE
079000030122     C                   Z-ADD     mgbPRG        VIHPRG
079100030704     C                   MOVEL     mgbTIP        VIHTIP
079200070509
079300070509     c                   eval      vihforza = *off
079400991108     C*
079500970526     C                   ENDSR
079600970526     C*------------------------------------------------------------------------*
079700991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
079800970526     C*------------------------------------------------------------------------*
079900970526     C     CHKPAR        BEGSR
080000021218     C*
080100021218     C* INIZIALIZZO FLAG GENERICO DI ERRORE SELEZIONI
080200021218     C                   MOVEL     '0'           W_ERR
080300970528     C*
080400021211     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *01L (COD)
080500021217     C                   MOVEL     'DAT'         WTIPRI            3
080600030128     C*
080700030128     C* CONTROLLO VALIDITA DATA INIZIO ELENCO
080800030128     C                   IF        W_ERR = '0'
080900030128     C                   IF        VIDDA1 > *zeros
081000030128     C                   Z-ADD     VIDDA1        DAW8
081100030128     C                   EXSR      CHKDA8
081200030305     C                   Z-ADD     WDA8          VIDDA1
081300030305     C                   Z-ADD     DAW8          DA1VID            8 0
081400030128     C                   IF        G08ERR = '1'
081500030128     C                   EVAL      W_ERR = '1'
081600030128     C                   SETON                                        81
081700030128     C                   ENDIF
081800030128     C                   ENDIF
081900030128     C                   ENDIF
082000030128     C*
082100030128     C* CONTROLLO VALIDITA PERIODO DA
082200030128     C                   IF        W_ERR = '0'
082300030128     C                   IF        VIDPD2 > *zeros
082400030128     C                   Z-ADD     VIDPD2        DAW8
082500030128     C                   EXSR      CHKDA8
082600030305     C                   Z-ADD     WDA8          VIDPD2
082700030305     C                   Z-ADD     DAW8          PD2VID            8 0
082800030128     C                   IF        G08ERR = '1'
082900030128     C                   EVAL      W_ERR = '1'
083000030128     C                   SETON                                        88
083100030128     C                   ENDIF
083200030128     C                   ENDIF
083300030128     C                   ENDIF
083400030128     C*
083500030128     C* CONTROLLO VALIDITA PERIODO A
083600030128     C                   IF        W_ERR = '0'
083700030128     C                   IF        VIDPA2 > *zeros
083800030128     C                   Z-ADD     VIDPA2        DAW8
083900030128     C                   EXSR      CHKDA8
084000030305     C                   Z-ADD     WDA8          VIDPA2
084100030305     C                   Z-ADD     DAW8          PA2VID            8 0
084200030128     C                   IF        G08ERR = '1'
084300030128     C                   EVAL      W_ERR = '1'
084400030128     C                   SETON                                        89
084500030128     C                   ENDIF
084600030128     C                   ENDIF
084700030128     C                   ENDIF
084800030128     C*
084900030128     C* CONTROLLO PERIODO DA <= PERIODO A
085000030128     C                   IF        W_ERR = '0'
085100030128     C                   IF        VIDPD2 > VIDPA2
085200030128     C                   EVAL      W_ERR = '1'
085300030128     C                   SETON                                        8889
085400030128     C                   ENDIF
085500030128     C                   ENDIF
085600021217     C*
085700021217     C* CONTROLLO SE RICHIESTO CLIENTE
085800021218     C                   IF        W_ERR = '0'
085900021217     C                   IF        VIDKS2 <> *BLANKS AND
086000021217     C                             VIDKS2 <> *ZEROS
086100021217     C                   EXSR      CHKKSC
086200021217     C                   IF        W_ERR = '1'
086300021217     C                   SETON                                        83
086400021217     C                   ELSE
086500021217     C                   MOVEL(P)  CHKKS2        VIDKS2
086600021217     C                   ENDIF
086700021217     C                   ENDIF
086800021218     C                   ENDIF
086900021217     C*
087000021217     C* CONTROLLO SE RICHIESTO DESTINATARIO
087100021218     C                   IF        W_ERR = '0'
087200021217     C                   IF        VIDDS2 <> *BLANKS AND
087300021217     C                             VIDDS2 <> *ZEROS
087400021217     C                   EXSR      CHKDST
087500021217     C                   IF        W_ERR = '1'
087600021217     C                   SETON                                        84
087700021217     C                   ENDIF
087800021218     C                   ENDIF
087900021217     C                   ENDIF
088000021217     C*
088100021217     C* CONTROLLO SE RICHIESTO ARTICOLO
088200021218     C                   IF        W_ERR = '0'
088300021217     C                   IF        VIDCO2 <> *BLANKS AND
088400021217     C                             VIDCO2 <> *ZEROS
088500021217     C                   EXSR      CHKCOD
088600021217     C                   IF        W_ERR = '1'
088700021217     C                   SETON                                        86
088800021217     C                   ENDIF
088900021218     C                   ENDIF
089000021217     C                   ENDIF
089100030305     C*
089200030305     C* CONTROLLO SE RICHIESTO P.O. INSERIMENTO
089300030305     C                   IF        W_ERR = '0'
089400030305     C                   IF        VIDPI2 <> *BLANKS AND
089500030305     C                             VIDPI2 <> *ZEROS
089600030305     C                   MOVEL     VIDPI2        wChkPO            3
089700030305     C                   EXSR      CHKPO
089800030305     C                   MOVEL     wChkPO        VIDPI2
089900030305     C                   IF        W_ERR = '1'
090000030305     C                   SETON                                        80
090100030305     C                   ENDIF
090200030305     C                   ENDIF
090300030305     C                   ENDIF
090400021217     C*
090500021217     C* CONTROLLO SE RICHIESTO P.O.
090600021218     C                   IF        W_ERR = '0'
090700021217     C                   IF        VIDPO2 <> *BLANKS AND
090800021217     C                             VIDPO2 <> *ZEROS
090900030305     C                   MOVEL     VIDPO2        wChkPO            3
091000021217     C                   EXSR      CHKPO
091100030305     C                   MOVEL     wChkPO        VIDPO2
091200021217     C                   IF        W_ERR = '1'
091300021217     C                   SETON                                        82
091400021217     C                   ENDIF
091500021218     C                   ENDIF
091600021217     C                   ENDIF
091700021219     C*
091800021219     C* CONTROLLO SE RICHIESTA CAUSALE
091900021219     C                   IF        W_ERR = '0'
092000021219     C                   IF        VIDCA2 <> *BLANKS AND
092100021219     C                             VIDCA2 <> *ZEROS
092200021219     C                   EXSR      CHKCAU
092300021219     C                   IF        W_ERR = '1'
092400021219     C                   SETON                                        85
092500021219     C                   ENDIF
092600021219     C                   ENDIF
092700021219     C                   ENDIF
092800000407     C*
092900970526     C                   ENDSR
093000970526     C*------------------------------------------------------------------------*
093100970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
093200970526     C*------------------------------------------------------------------------*
093300970526     C     CHKFOP        BEGSR
093400991112     C*
093500991112     C* OPZIONI
093600970626     C*
093700000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
093800021211     C     DEPOP0        IFEQ      'P01'
093900000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
094000000420     C                   MOVEL     'S'           OPDATT
094100000420     C                   MOVEL     OPDRIG        OPD(1)
094200021211     C                   ELSE
094300000420     C* OPZIONI
094400970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
094500970626     C                   MOVEL     'S'           OPDATT
094600970626     C                   MOVEL     OPDRIG        OPD(5)
094700021211     C                   ENDIF
094800970626     C*
094900000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
095000021211     C     DEPOP0        IFEQ      'P02'
095100991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
095200991112     C                   MOVEL     'S'           OPDATT
095300991112     C                   MOVEL     OPDRIG        OPD(5)
095400021211     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
095500000407     C                   MOVEL     'S'           FUDATT
095600000407     C                   MOVEL     FUDRIG        FUD(6)
095700021211     C                   ENDIF
095800101006     ***C*
095900101006     ***C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
096000101006     ***   *-=»> NON PIÙ: ORA CONSENTE F6=INSERIMENTO <«=-*
096100101006     ***C     SIMFEL        IFNE      *zeros                                       *FILIALE
096200101006     ***C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
096300101006     ***C                   MOVEL     'N'           OPDATT
096400101006     ***C                   MOVEL     FUDRIG        FUD(6)
096500101006     ***C                   ENDIF
096600970526     C*
096700970526     C* GESTIONE OPZIONI A VIDEO
096800970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
096900970526     C                   EXSR      OPZGES
097000970526     C*
097100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
097200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
097300970526     C                   EXSR      FUNGES
097400970526     C*
097500970526     C                   ENDSR
097600020917     C*--------------------------------------------------------------------------------------------*
097700020917     C* REPERISCE I DATI UTENTE
097800020917     C*--------------------------------------------------------------------------------------------*
097900020917     C     REPDATIUTE    BEGSR
098000020917     C*
098100020917     C* INIZIALIZZA VARIABILI DI WRK
098200020917     C                   CLEAR                   TIBS34DS
098300020917     C                   CLEAR                   AZUTEDS
098400020917     C                   CLEAR                   DDATIUTE
098500150212     C                   CLEAR                   dute01
098600020917     C*
098700020917     C     *DTAARA       DEFINE    §azute        azuteds
098800020917     C     *DTAARA       DEFINE    §datiute      ddatiute
098900020917     C                   IN(E)     *DTAARA
099000021211     C                   IF        %Error
099100021211     C                   EVAL      I34Tla = 'L'
099200020917     C                   CALL      'TIBS34R'
099300020917     C                   PARM                    Tibs34Ds
099400020917     C                   IN        *DTAARA
099500021211     C                   ENDIF
099600030704     C*
099700030704     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
099800030704     C                   CLEAR                   L1
099900030704     C                   CLEAR                   WAbi              2
100000150212     c                   Clear                   dLat
100100030704     C*
100200150212      * Verifica errori e autorità profilo
100300150212s   1c                   Select
100400150212      * se ho errori nei dati utente esco dal pgm
100500150212w   1c                   When      DutErr = 'E'
100600150212     c                   seton                                        lr
100700150212     c                   return
100800150212      * se non c'è l'abilitazione
100900150212      * --> se 1° livello, abilitazioni al terminal
101000150212      *     se 2° livello, abilitazioni al punto operativo
101100150212w   1c                   When      UteAut = *Blanks
101200150212if  2c                   If        DutLpo = '1'
101300150212     c                   Eval      wabi   = 'TP'
101400150212e   2c                   EndIf
101500150212if  2c                   If        DutLpo = '2'
101600150212     c                   Eval      wabi   = 'PO'
101700150212e   2c                   EndIf
101800150212if  2c                   If        DutLpo = 'S'
101900150212     c                   Eval      wabi   = 'AZ'
102000150212e   2c                   EndIf
102100150212      * carica le abilitazioni del profilo
102200150212x   1c                   Other
102300150212     c                   Movel     UteFaf        Dute01
102400150212     c                   If        §Utemgb <> *Blanks
102500150212     c                   Eval      wabi = §Utemgb
102600150212     c                   Else
102700150212     c                   Eval      wabi = UteAut
102800150212     c                   EndIf
102900150212e   1c                   EndSl
103000150212      * controllo se ok l'abilitazione dell'utente
103100150212     c                   Clear                   Tibs02ds
103200150212     c                   Eval      T02Mod = 'C'
103300150212     c                   Eval      T02Sif = knsif
103400150212     c                   Eval      T02Cod = 'LAT'
103500150212     c                   Movel(p)  wabi          T02Ke1
103600150212     c                   Call      'TIBS02R'
103700150212     c                   Parm                    kpjba
103800150212     c                   Parm                    Tibs02ds
103900150212if  1c                   If        T02Err = *Blanks
104000150212     c                   Eval      dLat = T02Uni
104100150212e   1c                   EndIf
104200150212      * errore o non abilitato (impossibile xchè controllato nel filtro)
104300150212if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
104400150212     C                   MOVEL     'P05'         DEPOP0
104500150212   x1c                   Else
104600030704     C* REPERISCO I P.O. GESTIBILI DALL'UTENTE
104700030704     C                   CLEAR                   TRUL31DS
104800150212     c                   clear                   TRUL31DS2
104900030704     C                   EVAL      I31abi = WAbi
105000150212     c                   eval      I31cdi = DUTdis
105100150212     c                   eval      I31car = DUTare
105200030704     C                   EVAL      I31cpo = DUTPOU
105300030704     C                   CALL      'TRUL31R'
105400030704     C                   PARM                    KPJBA
105500030704     C                   PARM                    TRUL31DS
105600150212     c                   parm                    TRUL31DS2
105700030704     C                   IF        O31pog > *zeros
105800030704     C                   MOVEA     O31pog        L1
105900030704     C                   ELSE
106000030704     C                   EVAL      FINL00 = '1'
106100030704     C                   ENDIF
106200150212     C                   ENDIF
106300020917     C*
106400020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
106500020917     C                   MOVEL(P)  RSUT          DSFIRS           20
106600020917     C*
106700020917     C                   ENDSR
106800030128     C*------------------------------------------------------------------------*
106900030128     C* CHKDA8 - CONTROLLA LA DATA (8)
107000030128     C*------------------------------------------------------------------------*
107100030128     C     CHKDA8        BEGSR
107200030128     C*
107300030305     C                   Z-ADD     DAW8          G08DAT
107400030305     C                   Z-ADD     *ZEROS        G08INV
107500030305     C                   MOVEL     '0'           G08ERR
107600030128     C                   CALL      'XSRDA8'
107700030128     C                   PARM                    WLBDA8
107800030305     C                   Z-ADD     G08DAT        WDA8
107900030305     C                   Z-ADD     G08INV        DAW8
108000030128     C*
108100030128     C                   ENDSR
108200970526     C*------------------------------------------------------------------------*
108300970526     C* *INZSR - OPERAZIONI INIZIALI
108400970526     C*------------------------------------------------------------------------*
108500970526     C     *INZSR        BEGSR
108600970526     C*---
108700970526     C* RICEVIMENTO PARAMETRI
108800970526     C*---
108900970526     C     *ENTRY        PLIST
109000970612     C                   PARM                    KPJBA
109100021211     C                   MOVEL     KPJBU         PARAM
109200021211     C                   MOVEL     POP1          DEPOP0            3
109300970526     C*
109400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
109500970526     C     XMSGPA        PLIST
109600970526     C                   PARM                    DSMSMS
109700970526     C                   PARM                    DSMSPG
109800970526     C                   PARM                    DSMSKE
109900970526     C                   PARM                    DSMSER
110000970526     C*
110100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
110200970526     C                   CALL      'XKEY'
110300970526     C                   PARM                    DSKEY
110400970526     C*---
110500970526     C* VARIABILI NON RIFERITE AL DATA BASE
110600970526     C*---
110700970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
110800970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
110900970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
111000970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
111100970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
111200970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
111300970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
111400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
111500970526     C*---
111600021217     C     *LIKE         DEFINE    VIDDA1        depda1
111700030128     C     *LIKE         DEFINE    VIDDA1        DAW8
111800030128     C     *LIKE         DEFINE    VIDDA1        WDA8
111900030128     C     *LIKE         DEFINE    VIDPD2        deppd2
112000030128     C     *LIKE         DEFINE    VIDPA2        deppa2
112100030305     C     *LIKE         DEFINE    VIDPI2        deppi2
112200021217     C     *LIKE         DEFINE    VIDPO2        deppo2
112300021217     C     *LIKE         DEFINE    VIDKS2        depks2
112400021217     C     *LIKE         DEFINE    VIDDS2        depds2
112500021217     C     *LIKE         DEFINE    VIDCA2        depca2
112600021217     C     *LIKE         DEFINE    VIDCO2        depco2
112700021217     C     *LIKE         DEFINE    VIDUT2        deput2
112800021217     C     *LIKE         DEFINE    MGBDAT        KEYDAT
112900021218     C     *LIKE         DEFINE    MGBSOG        CHKSOG
113000021217     C     *LIKE         DEFINE    KPJBU         SAVKPJBU
113100991108     C*---
113200970526     C* VARIABILI DI CONTROLLO
113300970526     C*---
113400970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
113500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
113600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
113700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
113800970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
113900150403     C                   Z-ADD     8             SFLPAG                         *N°RIGHE X PAG
114000970526     C*
114100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
114200021217     C                   MOVEL     'TISIG3R'     DSMSPG
114300021217     C                   MOVEL     'TISIG3R'     MSGQUE
114400970526     C                   MOVEL     *BLANKS       MSGKEY
114500970526     C                   MOVEL     *BLANKS       DSMSMS
114600970526     C                   MOVEL     *BLANKS       DSMSKE
114700970526     C                   Z-ADD     *ZEROS        DSMSER
114800970526     C*---
114900970526     C* CHIAVI DI LETTURA
115000970526     C*---
115100021217     C* LETTURA azmgb01l - PARZIALE
115200021211     C     KEYMGBP       KLIST
115300021217     C                   KFLD                    KEYDAT
115400030123     C* LETTURA tntbe01l - PARZIALE
115500030123     C     KEYTBEP       KLIST
115600030123     C                   KFLD                    tbeCOD
115700030123     C                   KFLD                    tbeKE1
115800030325     C* LETTURA cnaco00f - COMPLETA
115900030325     C     KEYACOT       KLIST
116000030325     C                   KFLD                    acoKUT
116100030325     C                   KFLD                    acoKCC
116200030325     C                   KFLD                    acoKSC
116300991108     C*
116400970527     C* REPERIMENTO DATI UTENTE
116500020917     C                   EXSR      REPDATIUTE
116600970526     C*
116700970526     C* IMPOSTA I PARAMETRI DI INGRESSO
116800970526     C                   EXSR      CHKPAR
116900991115     C*
117000970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
117100970526     C                   EXSR      CHKFOP
117200970526     C*
117300970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
117400021211     C     DEPOP0        IFEQ      'P01'
117500970530     C                   MOVEL     DEC(3)        VIDING
117600021211     C                   ENDIF
117700021211     C     DEPOP0        IFEQ      'P02'
117800970526     C                   MOVEL     DEC(1)        VIDING
117900021211     C                   ENDIF
118000021211     C     DEPOP0        IFEQ      'P05'
118100970526     C                   MOVEL     DEC(2)        VIDING
118200021211     C                   ENDIF
118300970526     C*
118400970526     C                   ENDSR
118500970526     C*------------------------------------------------------------------------*
118600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
118700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
118800970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
118900970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
119000970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
119100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
119200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
119300970526     C*------------------------------------------------------------------------*
119400970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
119500970526     C*------------------------------------------------------------------------*
119600970526     C     POSSFL        BEGSR
119700970526     C*
119800970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
119900970526     C                   EXSR      CLRSFL
120000030128     C                   Z-ADD     *zeros        VIDSAL
120100030305     C*
120200030305     C* SE NN EFFETTUATE SELEZIONI EMETTO VIDEATA DI RICHIESTA
120300030305     C                   IF        VIDPD2 = *zeros                       AND
120400030305     C                             VIDPA2 = *zeros                       AND
120500150403     C                             VIDda1 = *zeros                       AND
120600030305     C                             (VIDPI2 = *blanks OR VIDPI2 = *zeros) AND
120700030305     C                             (VIDPO2 = *blanks OR VIDPO2 = *zeros) AND
120800030305     C                             (VIDKS2 = *blanks OR VIDKS2 = *zeros) AND
120900030305     C                             (VIDDS2 = *blanks OR VIDDS2 = *zeros) AND
121000030305     C                             (VIDCA2 = *blanks OR VIDCA2 = *zeros) AND
121100030305     C                             (VIDCO2 = *blanks OR VIDCO2 = *zeros) AND
121200030305     C                             (VIDUT2 = *blanks OR VIDUT2 = *zeros)
121300030305     C                   SETON                                        97
121400030305     C                   ELSE
121500030305     C                   SETOFF                                       97
121600970526     C*
121700970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
121800970526     C                   EXSR      SETFIL
121900970526     C*
122000970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
122100970526     C     SFLEOF        IFEQ      0
122200970526     C                   EXSR      CARSFL
122300030305     C                   ENDIF
122400030305     C                   ENDIF
122500970526     C*
122600970526     C                   ENDSR
122700970526     C*------------------------------------------------------------------------*
122800970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
122900970526     C*------------------------------------------------------------------------*
123000970526     C     ENTATT        BEGSR
123100970526     C*
123200970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
123300970526     C                   Z-ADD     0             OPZOK             1 0
123400970526     C*
123500970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
123600970526     C                   Z-ADD     0             SFLVAR            5 0
123700970526     C*
123800970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
123900970526     C                   Z-ADD     0             SFLERR
124000970526     C*
124100970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
124200970526     C                   Z-ADD     SFLULT        WFLULT
124300970526     C*
124400970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
124500021217     C                   READC     SIG3D05                                98
124600970526     C     *IN98         DOWEQ     '0'
124700970526     C*
124800970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
124900970526     C                   ADD       1             SFLVAR
125000970526     C*
125100970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
125200970526     C     OPZOK         IFEQ      0
125300970526     C*
125400970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
125500970526     C     OPZKEY        IFNE      *BLANKS
125600970526     C                   EXSR      OPZCON
125700970526     C                   END
125800970526     C*
125900970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
126000970526     C*            2. PULIZIA SCELTA IN SFL
126100970526     C     OPZOK         IFEQ      0
126200970526     C                   MOVEL     '0'           *IN41
126300970526     C                   MOVEL     *BLANKS       OPZKEY
126400970526     C                   ELSE
126500970526     C*
126600970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
126700970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
126800970526     C                   MOVEL     '1'           *IN41
126900970526     C                   Z-ADD     SFLULT        SFLERR
127000970526     C                   END
127100970526     C                   ELSE
127200970526     C*
127300970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
127400970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
127500970526     C                   MOVEL     '1'           *IN41
127600970526     C                   END                                                    OPZOK=0
127700970526     C*
127800970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
127900970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
128000970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
128100970526     C     OPZOK         IFNE      0
128200970526     C                   Z-ADD     SFLERR        SFLNBR
128300970526     C                   ELSE
128400970526     C                   Z-ADD     SFLULT        SFLNBR
128500970526     C                   END
128600970526     C*
128700970526     C* AGGIORNO RECORD SFL
128800021217     C                   UPDATE    SIG3D05
128900970526     C*
129000021217     C                   READC     SIG3D05                                98
129100970526     C                   END                                                    DO WHILE
129200970526     C*
129300970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
129400970526     C                   Z-ADD     WFLULT        SFLULT
129500970526     C*
129600970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
129700970526     C     SFLVAR        IFEQ      0
129800970526     C                   END
129900970526     C*
130000040702     C***  OPZKEY        IFNE      ' 5'
130100040702     C***                EXSR      F05GES
130200040702     C***                ENDIF
130300991112     C*
130400970526     C                   ENDSR
130500970526     C*------------------------------------------------------------------------*
130600970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
130700970526     C*------------------------------------------------------------------------*
130800970526     C     CLRSFL        BEGSR
130900970526     C*
131000970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
131100970526     C* (0=NON FINE FILE  1=FINE FILE)
131200970526     C                   Z-ADD     0             SFLEOF
131300970526     C*
131400970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
131500970526     C                   Z-ADD     0             SFLULT
131600970526     C*
131700970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
131800970526     C                   MOVEL     '1'           *IN40
131900021217     C                   WRITE     SIG3D06
132000970526     C                   MOVEL     '0'           *IN40
132100970526     C*
132200970526     C                   ENDSR
132300970526     C*------------------------------------------------------------------------*
132400970526     C* CARSFL - CARICAMENTO SFL
132500970526     C*------------------------------------------------------------------------*
132600970526     C     CARSFL        BEGSR
132700970526     C*
132800970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
132900970526     C     SFLULT        IFEQ      0
133000970526     C                   EXSR      SETFIL
133100970526     C                   END
133200970526     C*
133300970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
133400970526     C                   Z-ADD     SFLULT        WFLULT
133500970526     C*
133600970526     C* CICLO:
133700970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
133800970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
133900040225     C                   Z-ADD     1             SFLRIG            5 0
134000150417 XXX C***  SFLRIG        DOWLE     SFLPAG
134100150417     C***  SFLEOF        ANDEQ     0
134200150417     C     SFLEOF        DOWEQ     0
134300150417     C* SE NN SUPERATO NUMERO MAX RECORD IN SUB-FILE
134400150417     C     SFLRIG        IFLE      9999
134500970526     C*
134600970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
134700991116     C                   MOVEL     *BLANKS       OPZKEY
134800970526     C*
134900040225     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
135000970526     C                   EXSR      IRESFL
135100970526     C*
135200970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
135300970526     C                   ADD       1             SFLULT
135400970526     C*
135500970526     C* SCRITTURA RECORD SFL
135600021217     C                   WRITE     SIG3D05
135700970526     C                   ADD       1             SFLRIG
135800150417     C* SE RAGGIUNTO NUMERO MAX RECORD IN SUB-FILE => EMETTO MESSAGGIO
135900150417     C                   ELSE
136000150417     C                   MOVEL     MSG(5)        DSMSMS
136100150417     C                   ENDIF
136200040225     C*
136300040225     C* INCREMENTO IL SALDO A VIDEO INDIPENDENTEMENTE DAL CARICAMENTO DEI RECORD DEL SUB-FILE
136400040225     C                   ADD       mgbQTA        VIDSAL
136500970526     C*
136600970526     C* LETTURA FILE DA CARICARE
136700970526     C                   EXSR      LETFIL
136800970526     C                   END                                                    DO WHILE
136900970526     C*
137000970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
137100970526     C     WFLULT        IFNE      SFLULT
137200970526     C                   Z-ADD     WFLULT        SFLNBR
137300970526     C                   ADD       1             SFLNBR
137400970526     C                   END
137500970526     C*
137600970526     C                   ENDSR
137700970526     C*------------------------------------------------------------------------*
137800970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
137900970526     C*------------------------------------------------------------------------*
138000970526     C     OUTSFL        BEGSR
138100970526     C*
138200970526     C* WRITE RECORD TESTATA VIDEO
138300021217     C                   WRITE     SIG3D01
138400970526     C*
138500970526     C* VISUALIZZAZIONE MESSAGGIO
138600970526     C                   EXSR      SNDMSG
138700970526     C*
138800970526     C* WRITE RECORD OPZIONI VIDEO
138900021217     C                   WRITE     SIG3D02
139000970526     C*
139100970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
139200021217     C                   WRITE     SIG3D03
139300970526     C*
139400970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
139500021217     C                   WRITE     SIG3D04
139600970526     C*
139700970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
139800970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
139900970526     C     SFLEOF        IFEQ      0
140000970526     C                   MOVEL     '0'           *IN42
140100970526     C                   ELSE
140200970526     C*
140300970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
140400970526     C                   MOVEL     '1'           *IN42
140500970526     C                   END
140600970526     C*
140700970526     C* WRITE RECORD TASTI FUNZIONALI
140800021217     C                   WRITE     SIG3D09
140900970526     C*
141000970526     C* CASO DI SFL VUOTO
141100970526     C     SFLULT        IFLE      0
141200970526     C*
141300970526     C*    - WRITE RECORD VUOTO
141400021217     C                   WRITE     SIG3D0A
141500021217     C                   READ      SIG3D0A                                99
141600970526     C*
141700970526     C                   ELSE
141800970526     C*
141900970526     C* CASO DI SFL CON RECORDS
142000970526     C*
142100970526     C*    - WRITE RECORD CONTROLLO SFL
142200021217     C                   WRITE     SIG3D06
142300021217     C                   READ      SIG3D06                                99
142400970526     C                   END
142500970526     C*
142600970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
142700021217     C                   READ      SIG3D03                                99
142800970526     C*
142900970526     C                   ENDSR
143000970526     C*------------------------------------------------------------------------*
143100970526     C* SNDMSG - MANDA MESSAGGIO
143200970526     C*------------------------------------------------------------------------*
143300970526     C     SNDMSG        BEGSR
143400970526     C*
143500970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
143600970526     C                   MOVEL     '1'           *IN40
143700021217     C                   WRITE     SIG3DMC
143800970526     C                   MOVEL     '0'           *IN40
143900970526     C*
144000970526     C* VISUALIZZAZIONE MESSAGGIO
144100970526     C                   Z-ADD     0             MSGULT
144200970526     C                   CALL      'XMSG'        XMSGPA
144300970526     C     DSMSER        IFEQ      0
144400970526     C                   MOVEL     DSMSKE        MSGKEY
144500970526     C                   END
144600970526     C                   Z-ADD     1             MSGULT
144700021217     C                   WRITE     SIG3DMS                                      SFL MESS
144800021217     C                   WRITE     SIG3DMC                                      CONTR MESS
144900970526     C*
145000970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
145100970526     C                   MOVEL     *BLANKS       DSMSMS
145200970526     C*
145300970526     C                   ENDSR
145400970526     C*------------------------------------------------------------------------*
145500970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
145600970526     C*------------------------------------------------------------------------*
145700970526     C     FUNCON        BEGSR
145800970526     C*
145900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
146000970526     C                   EXSR      FUNVAR
146100970526     C*
146200970526     C* PULISCO CAMPI DI LAVORO
146300970526     C                   Z-ADD     0             I                 5 0
146400970526     C                   Z-ADD     0             W                 5 0
146500970526     C*
146600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
146700970526     C     FUNKEY        IFNE      *BLANKS
146800970526     C     1             DO        24            I
146900970526     C                   MOVEL     FUD(I)        FUDRIG
147000970526     C*
147100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
147200970526     C     FUNKEY        IFEQ      FUDFUN
147300970526     C     FUDATT        ANDEQ     'S'
147400970526     C                   Z-ADD     I             W
147500970526     C                   END
147600970526     C                   END                                                    DO WHILE
147700970526     C*
147800970526     C* FUNZIONE NON ATTIVA (ERRORE)
147900970526     C     W             IFEQ      0
148000970526     C*
148100970526     C* EMISSIONE MESSAGGIO DI ERRORE
148200970526     C                   MOVEL     MSG(2)        DSMSMS
148300970526     C                   END                                                    W=0
148400970526     C                   END                                                    FUNKEY NE *BLANKS
148500970526     C*
148600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
148700970526     C     W             IFNE      0
148800970526     C     FUNKEY        OREQ      *BLANKS
148900970526     C*
149000970526     C     FUNKEY        IFEQ      *BLANKS
149100970526     C                   EXSR      ENTGES
149200970526     C                   END
149300970526     C     FUNKEY        IFEQ      '03'
149400970526     C                   EXSR      F03GES
149500970526     C                   END
149600970526     C     FUNKEY        IFEQ      '05'
149700970526     C                   EXSR      F05GES
149800970526     C                   END
149900970526     C     FUNKEY        IFEQ      '06'
150000970526     C                   EXSR      F06GES
150100970526     C                   END
150200970526     C     FUNKEY        IFEQ      '12'
150300970526     C                   EXSR      F12GES
150400970526     C                   END
150500970528     C     FUNKEY        IFEQ      '21'
150600970528     C                   EXSR      F21GES
150700970528     C                   END
150800970528     C     FUNKEY        IFEQ      '22'
150900970528     C                   EXSR      F22GES
151000970528     C                   END
151100970526     C     FUNKEY        IFEQ      '23'
151200970526     C                   EXSR      F23GES
151300970526     C                   END
151400970526     C     FUNKEY        IFEQ      '24'
151500970526     C                   EXSR      F24GES
151600970526     C                   END
151700970526     C     FUNKEY        IFEQ      'RU'
151800970526     C                   EXSR      RUPGES
151900970526     C                   END
152000970526     C*
152100970526     C                   END
152200970526     C*
152300970526     C                   ENDSR
152400970526     C*------------------------------------------------------------------------*
152500970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
152600970526     C*------------------------------------------------------------------------*
152700970526     C     FUNVAR        BEGSR
152800970526     C*
152900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
153000970526     C     KEY           IFEQ      ENTER
153100970526     C                   MOVEL     *BLANKS       FUNKEY
153200970526     C                   END
153300970526     C     KEY           IFEQ      F1
153400970526     C                   MOVEL     '01'          FUNKEY
153500970526     C                   END
153600970526     C     KEY           IFEQ      F2
153700970526     C                   MOVEL     '02'          FUNKEY
153800970526     C                   END
153900970526     C     KEY           IFEQ      F3
154000970526     C                   MOVEL     '03'          FUNKEY
154100970526     C                   END
154200970526     C     KEY           IFEQ      F4
154300970526     C                   MOVEL     '04'          FUNKEY
154400970526     C                   END
154500970526     C     KEY           IFEQ      F5
154600970526     C                   MOVEL     '05'          FUNKEY
154700970526     C                   END
154800970526     C     KEY           IFEQ      F6
154900970526     C                   MOVEL     '06'          FUNKEY
155000970526     C                   END
155100970526     C     KEY           IFEQ      F7
155200970526     C                   MOVEL     '07'          FUNKEY
155300970526     C                   END
155400970526     C     KEY           IFEQ      F8
155500970526     C                   MOVEL     '08'          FUNKEY
155600970526     C                   END
155700970526     C     KEY           IFEQ      F9
155800970526     C                   MOVEL     '09'          FUNKEY
155900970526     C                   END
156000970526     C     KEY           IFEQ      F10
156100970526     C                   MOVEL     '10'          FUNKEY
156200970526     C                   END
156300970526     C     KEY           IFEQ      F11
156400970526     C                   MOVEL     '11'          FUNKEY
156500970526     C                   END
156600970526     C     KEY           IFEQ      F12
156700970526     C                   MOVEL     '12'          FUNKEY
156800970526     C                   END
156900970526     C     KEY           IFEQ      F13
157000970526     C                   MOVEL     '13'          FUNKEY
157100970526     C                   END
157200970526     C     KEY           IFEQ      F14
157300970526     C                   MOVEL     '14'          FUNKEY
157400970526     C                   END
157500970526     C     KEY           IFEQ      F15
157600970526     C                   MOVEL     '15'          FUNKEY
157700970526     C                   END
157800970526     C     KEY           IFEQ      F16
157900970526     C                   MOVEL     '16'          FUNKEY
158000970526     C                   END
158100970526     C     KEY           IFEQ      F17
158200970526     C                   MOVEL     '17'          FUNKEY
158300970526     C                   END
158400970526     C     KEY           IFEQ      F18
158500970526     C                   MOVEL     '18'          FUNKEY
158600970526     C                   END
158700970526     C     KEY           IFEQ      F19
158800970526     C                   MOVEL     '19'          FUNKEY
158900970526     C                   END
159000970526     C     KEY           IFEQ      F20
159100970526     C                   MOVEL     '20'          FUNKEY
159200970526     C                   END
159300970526     C     KEY           IFEQ      F21
159400970526     C                   MOVEL     '21'          FUNKEY
159500970526     C                   END
159600970526     C     KEY           IFEQ      F22
159700970526     C                   MOVEL     '22'          FUNKEY
159800970526     C                   END
159900970526     C     KEY           IFEQ      F23
160000970526     C                   MOVEL     '23'          FUNKEY
160100970526     C                   END
160200970526     C     KEY           IFEQ      F24
160300970526     C                   MOVEL     '24'          FUNKEY
160400970526     C                   END
160500970526     C     KEY           IFEQ      ROLLUP
160600970526     C                   MOVEL     'RU'          FUNKEY
160700970526     C                   END
160800970526     C     KEY           IFEQ      HELP
160900970526     C                   MOVEL     'HP'          FUNKEY
161000970526     C                   END
161100970526     C*
161200970526     C                   ENDSR
161300970526     C*------------------------------------------------------------------------*
161400970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
161500970526     C*------------------------------------------------------------------------*
161600970526     C     FUNGES        BEGSR
161700970526     C*
161800970526     C* PULISCO CAMPI DI LAVORO
161900970526     C                   Z-ADD     0             W                 5 0
162000970526     C*
162100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
162200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
162300970526     C     1             DO        24            I
162400970526     C                   MOVEL     FUD(I)        FUDRIG
162500970526     C*
162600970526     C* PULIZIA FUNZIONI PRECEDENTI
162700970526     C                   MOVEL     *BLANKS       FUV(I)
162800970526     C*
162900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
163000970526     C* (SALTA F23 E F24)
163100970526     C     FUDATT        IFEQ      'S'
163200970526     C     FUDFUN        ANDNE     '23'
163300970526     C     FUDFUN        ANDNE     '24'
163400970526     C                   ADD       1             W
163500970526     C                   MOVEL     FUDDES        FUV(W)
163600970526     C                   END
163700970526     C                   END                                                    DO WHILE
163800970526     C*
163900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
164000970526     C* ALTRIMENTI DISATTIVO F24
164100970526     C     1             DO        24            I
164200970526     C                   MOVEL     FUD(I)        FUDRIG
164300970526     C*
164400970526     C     FUDFUN        IFEQ      '24'
164500970526     C     W             IFGT      6
164600970526     C                   MOVEL     'S'           FUDATT
164700970526     C                   ELSE
164800970526     C                   MOVEL     *BLANKS       FUDATT
164900970526     C                   END                                                    W GT 8
165000970526     C                   MOVEL     FUDRIG        FUD(I)
165100970526     C                   END
165200970526     C                   END                                                    DO WHILE
165300970526     C*
165400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
165500970526     C     W             DIV       6             I
165600970526     C                   MVR                     Z
165700970526     C*
165800970526     C     Z             IFNE      0
165900970526     C     I             ADD       1             Z
166000970526     C                   ELSE
166100970526     C                   Z-ADD     I             Z
166200970526     C                   END
166300970526     C*
166400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
166500970526     C                   ADD       1             FUNGRU
166600970526     C     FUNGRU        IFGT      Z
166700970526     C     FUNGRU        ORLT      1
166800970526     C                   Z-ADD     1             FUNGRU
166900970526     C                   END
167000970526     C*
167100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
167200970526     C     FUNGRU        IFEQ      1
167300970526     C                   MOVEL     FUV(1)        FUN001
167400970526     C                   MOVEL     FUV(2)        FUN002
167500970526     C                   MOVEL     FUV(3)        FUN003
167600970526     C                   MOVEL     FUV(4)        FUN004
167700970526     C                   MOVEL     FUV(5)        FUN005
167800970526     C                   MOVEL     FUV(6)        FUN006
167900970526     C                   END
168000970526     C*
168100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
168200970526     C     FUNGRU        IFEQ      2
168300970526     C                   MOVEL     FUV(7)        FUN001
168400970526     C                   MOVEL     FUV(8)        FUN002
168500970526     C                   MOVEL     FUV(9)        FUN003
168600970526     C                   MOVEL     FUV(10)       FUN004
168700970526     C                   MOVEL     FUV(11)       FUN005
168800970526     C                   MOVEL     FUV(12)       FUN006
168900970526     C                   END
169000970526     C*
169100970526     C* TERZO GRUPPO DI 6 FUNZIONI
169200970526     C     FUNGRU        IFEQ      3
169300970526     C                   MOVEL     FUV(13)       FUN001
169400970526     C                   MOVEL     FUV(14)       FUN002
169500970526     C                   MOVEL     FUV(15)       FUN003
169600970526     C                   MOVEL     FUV(16)       FUN004
169700970526     C                   MOVEL     FUV(17)       FUN005
169800970526     C                   MOVEL     FUV(18)       FUN006
169900970526     C                   END
170000970526     C*
170100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
170200970526     C     FUNGRU        IFEQ      4
170300970526     C                   MOVEL     FUV(19)       FUN001
170400970526     C                   MOVEL     FUV(20)       FUN002
170500970526     C                   MOVEL     FUV(21)       FUN003
170600970526     C                   MOVEL     FUV(22)       FUN004
170700970526     C                   MOVEL     *BLANKS       FUN005
170800970526     C                   MOVEL     *BLANKS       FUN006
170900970526     C                   END
171000970526     C*
171100970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
171200970526     C                   MOVEL     *BLANKS       FUN007
171300970526     C                   MOVEL     *BLANKS       FUN008
171400970526     C*
171500970526     C     1             DO        24            I
171600970526     C                   MOVEL     FUD(I)        FUDRIG
171700970526     C     FUDFUN        IFEQ      '23'
171800970526     C     FUDATT        ANDEQ     'S'
171900970526     C                   MOVEL     FUDDES        FUN007
172000970526     C                   END
172100970526     C     FUDFUN        IFEQ      '24'
172200970526     C     FUDATT        ANDEQ     'S'
172300970526     C                   MOVEL     FUDDES        FUN008
172400970526     C                   END
172500970526     C                   END                                                    DO
172600970526     C*
172700970526     C                   ENDSR
172800970526     C*------------------------------------------------------------------------*
172900970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
173000970526     C*------------------------------------------------------------------------*
173100970526     C     OPZGES        BEGSR
173200970526     C*
173300970526     C* PULISCO CAMPI DI LAVORO
173400970526     C                   Z-ADD     0             W                 5 0
173500970526     C*
173600970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
173700970526     C* E CARICO TABELLA OPZIONI ATTIVE
173800991115     C     1             DO        24            I
173900970526     C                   MOVEL     OPD(I)        OPDRIG
174000970526     C*
174100970526     C* PULIZIA OPZIONI PRECEDENTI
174200970526     C                   MOVEL     *BLANKS       OPV(I)
174300970526     C*
174400970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
174500970526     C     OPDATT        IFEQ      'S'
174600970526     C                   ADD       1             W
174700970526     C                   MOVEL     OPDDES        OPV(W)
174800970526     C                   END
174900970526     C                   END                                                    DO WHILE
175000970526     C*
175100970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
175200970526     C* ALTRIMENTI DISATTIVO F23
175300991115     C     1             DO        24            I
175400970526     C                   MOVEL     FUD(I)        FUDRIG
175500970526     C*
175600970526     C     FUDFUN        IFEQ      '23'
175700970526     C     W             IFGT      8
175800970526     C                   MOVEL     'S'           FUDATT
175900970526     C                   ELSE
176000970526     C                   MOVEL     *BLANKS       FUDATT
176100970526     C                   END                                                    W GT 8
176200970526     C                   MOVEL     FUDRIG        FUD(I)
176300970526     C                   END
176400970526     C                   END                                                    DO WHILE
176500970526     C*
176600970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
176700970526     C     W             DIV       8             I
176800970526     C                   MVR                     Z                 5 0
176900970526     C*
177000970526     C     Z             IFNE      0
177100970526     C     I             ADD       1             Z
177200970526     C                   ELSE
177300970526     C                   Z-ADD     I             Z
177400970526     C                   END
177500970526     C*
177600970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
177700970526     C                   ADD       1             OPZGRU
177800970526     C     OPZGRU        IFGT      Z
177900970526     C     OPZGRU        ORLT      1
178000970526     C                   Z-ADD     1             OPZGRU
178100970526     C                   END
178200970526     C*
178300970526     C* PRIMO GRUPPO DI 8 OPZIONI
178400970526     C     OPZGRU        IFEQ      1
178500970526     C                   MOVEL     OPV(1)        OPZ001
178600970526     C                   MOVEL     OPV(2)        OPZ002
178700970526     C                   MOVEL     OPV(3)        OPZ003
178800970526     C                   MOVEL     OPV(4)        OPZ004
178900970526     C                   MOVEL     OPV(5)        OPZ005
179000970526     C                   MOVEL     OPV(6)        OPZ006
179100970526     C                   MOVEL     OPV(7)        OPZ007
179200970526     C                   MOVEL     OPV(8)        OPZ008
179300970526     C                   END
179400970526     C*
179500970526     C* SECONDO GRUPPO DI 8 OPZIONI
179600970526     C     OPZGRU        IFEQ      2
179700970526     C                   MOVEL     OPV(9)        OPZ001
179800970526     C                   MOVEL     OPV(10)       OPZ002
179900970526     C                   MOVEL     OPV(11)       OPZ003
180000970526     C                   MOVEL     OPV(12)       OPZ004
180100970526     C                   MOVEL     OPV(13)       OPZ005
180200970526     C                   MOVEL     OPV(14)       OPZ006
180300970526     C                   MOVEL     OPV(15)       OPZ007
180400970526     C                   MOVEL     OPV(16)       OPZ008
180500970526     C                   END
180600970526     C*
180700970526     C* TERZO GRUPPO DI 8 OPZIONI
180800970526     C     OPZGRU        IFEQ      3
180900970526     C                   MOVEL     OPV(17)       OPZ001
181000970526     C                   MOVEL     OPV(18)       OPZ002
181100970526     C                   MOVEL     OPV(19)       OPZ003
181200970526     C                   MOVEL     OPV(20)       OPZ004
181300970526     C                   MOVEL     OPV(21)       OPZ005
181400970526     C                   MOVEL     OPV(22)       OPZ006
181500970526     C                   MOVEL     OPV(23)       OPZ007
181600970526     C                   MOVEL     OPV(24)       OPZ008
181700970526     C                   END
181800970526     C*
181900970526     C                   ENDSR
182000970526     C*------------------------------------------------------------------------*
182100970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
182200970526     C*------------------------------------------------------------------------*
182300970526     C     OPZCON        BEGSR
182400970526     C*
182500970526     C* PULISCO CAMPI DI LAVORO
182600970526     C                   Z-ADD     0             I                 5 0
182700970526     C                   Z-ADD     0             W                 5 0
182800970526     C*
182900970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
183000970526     C     1             DO        24            I
183100970526     C                   MOVEL     OPD(I)        OPDRIG
183200970526     C*
183300970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
183400970526     C     OPZKEY        IFEQ      OPDOPZ
183500970526     C     OPZKEY        OREQ      OPDOP1
183600970526     C*
183700970526     C     OPDATT        IFEQ      'S'
183800970526     C                   MOVEL     OPDOPZ        OPZKEY
183900970526     C                   Z-ADD     I             W
184000970526     C                   END
184100970526     C                   END
184200970526     C*
184300970526     C                   END                                                    DO WHILE
184400970526     C*
184500970526     C* OPZIONE NON ATTIVA (ERRORE)
184600970526     C     W             IFEQ      0
184700970526     C*
184800970526     C* OPZIONE NON OK
184900970526     C                   Z-ADD     1             OPZOK
185000970526     C*
185100970526     C* EMISSIONE MESSAGGIO DI ERRORE
185200970526     C                   MOVEL     MSG(1)        DSMSMS
185300970526     C                   ELSE
185400970526     C*
185500970526     C* OPZIONE ATTIVA (PROCEDI ...)
185600970526     C*
185700021211     C                   MOVEL     *BLANKS       PMSG
185800970526     C     OPZKEY        IFEQ      ' 5'
185900970526     C                   EXSR      O05GES
186000970526     C                   END
186100021211     C                   MOVEL     PMSG          DSMSMS
186200021211     C                   END                                                    OPZ.NON ATTIVA
186300970526     C*
186400970526     C                   ENDSR
186500970526     C*------------------------------------------------------------------------*
186600970526     C* NOTE COSTRUZIONE SCHIERE
186700970526     C*------------------------------------------------------------------------*
186800970526     C*
186900970526     C* -MSG-MESSAGGI
187000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
187100970526     C* (NON TOCCARE MSG,1 E MSG,2)
187200970526     C*
187300970526     C* -CMD-COMANDI
187400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
187500970526     C*
187600970526     C* -OPD-OPZIONI
187700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
187800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
187900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
188000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
188100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
188200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
188300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
188400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
188500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
188600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
188700970526     C*
188800970526     C* -FUD-FUNZIONI
188900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
189000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
189100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
189200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
189300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
189400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
189500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
189600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
189700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
189800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
189900991108     C*
190000991115     C* -STS-CODICI STATI DEI RECORD DI LOG
190100991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
190200991110     C* ================> IMPORTANTE <=================
190300991115     C* per codici di errore in -STS- indicare in -DES- sempre
190400991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
190500991110     C* ERR
190600991108     C*
190700970526     O*------------------------------------------------------------------------*
190800970526** DEC - DECODIFICHE
190900970526    GESTIONE                                                     1
191000000407 INTERROGAZIONE                                                  2
191100970530     SCELTA                                                      3
1912009705264
1913009705265
191400970526** MSG - MESSAGGI
191500021217TISIG3R- Opzione non gestita.                                                  1
191600021217TISIG3R- Tasto funzionale non gestito.                                         2
191700021217TISIG3R- Non abilitato all inserimento in filiale.                             3
191800070509TISIG3R- Soggetto movimento non in gestione all'utente. Enter per forzare!     4
191900040225TISIG3R- Presenti troppi record, alcuni non verranno visualizzati.             5
192000021217TISIG3R- 6                                                                     6
192100021217TISIG3R- 7                                                                     7
192200021217TISIG3R- 8                                                                     8
192300021217TISIG3R- 9                                                                     9
192400021217TISIG3R- 10                                                                   10
192500021217TISIG3R- 11                                                                   11
192600021217TISIG3R- 12                                                                   12
192700021217TISIG3R- 13                                                                   13
192800021217TISIG3R- 14                                                                   14
192900021217TISIG3R- 15                                                                   15
193000021217TISIG3R- 16                                                                   16
193100021217TISIG3R- 17                                                                   17
193200021217TISIG3R- 18                                                                   18
193300021217TISIG3R- 19                                                                   19
193400021217TISIG3R- 20                                                                   20
193500970528** CMD - COMANDI CL
193600021211
193700021211
193800991110
193900970528
194000970528
194100970526** OPD - OPZIONI
194200000420 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
194300000407 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
194400021211 3=Copia           -3 -     (S/N)=N                        opz. 03¬
194500000407 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
194600000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
194700991115                   -  -     (S/N)=N                        opz. 06¬
194800021211                   -  -     (S/N)=N                        opz. 07¬
194900991110                   -  -     (S/N)=N                        opz. 08¬
195000991110                   -  -     (S/N)=N                        opz. 09¬
195100991110                   -  -     (S/N)=N                        opz. 10¬
195200000407                   -  -     (S/N)=N                        opz. 11¬
195300970526                   -  -     (S/N)=N                        opz. 12¬
195400970526                   -  -     (S/N)=N                        opz. 13¬
195500000407                   -  -     (S/N)=N                        opz. 14¬
195600000407                   -  -     (S/N)=N                        opz. 15¬
195700970526                   -  -     (S/N)=N                        opz. 16¬
195800970526                   -  -     (S/N)=N                        opz. 17¬
195900970526                   -  -     (S/N)=N                        opz. 18¬
196000970526                   -  -     (S/N)=N                        opz. 19¬
196100970526                   -  -     (S/N)=N                        opz. 20¬
196200970526                   -  -     (S/N)=N                        opz. 21¬
196300970526                   -  -     (S/N)=N                        opz. 22¬
196400970526                   -  -     (S/N)=N                        opz. 23¬
196500000407                   -  -     (S/N)=N                        opz. 25¬
196600970526** FUD - FUNZIONI
196700970526                   -        (S/N)=N                        funz.01¬
196800970526                   -        (S/N)=N                        funz.02¬
196900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
197000970526                   -        (S/N)=N                        funz.04¬
197100970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
197200101006F06=Inserimento    -        (S/N)=N  <==                   funz.06¬
197300970526                   -        (S/N)=N                        funz.07¬
197400970527                   -        (S/N)=N                        funz.08¬
197500030325F09=Altri dati     -        (S/N)=S  <== S-FISSO           funz.09¬
197600970527                   -        (S/N)=N                        funz.10¬
197700970526                   -        (S/N)=N                        funz.11¬
197800970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
197900991116                   -        (S/N)=N                        funz.13¬
198000970526                   -        (S/N)=N                        funz.14¬
198100970526                   -        (S/N)=N                        funz.15¬
198200970526                   -        (S/N)=N                        funz.16¬
198300970526                   -        (S/N)=N                        funz.17¬
198400970526                   -        (S/N)=N                        funz.18¬
198500970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
198600970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
198700000407                   -        (S/N)=N                        funz.21¬
198800991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
198900991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
199000991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
