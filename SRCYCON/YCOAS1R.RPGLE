000100150427     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200150427     H*PARMS CVTOPT(*DATETIME) OPTION(*NOXREF)
000300150429     H DECEDIT('0,') DATEDIT(*DMY.) DATFMT(*ISO)
000400970526     H*------------------------------------------------------------------------*
000500150424     H* Gestione file (tnass00F)
000600970526     H*------------------------------------------------------------------------*
000700970526     F*------------------------------------------------------------------------*
000800970526     F* DATA BASE
000900970526     F*------------------------------------------------------------------------*
001000150424     Fycoas1d   CF   E             WORKSTN INFDS(DEVDS1)
001100021217     F                                     SFILE(SIG3D05:SFLULT)
001200021217     F                                     SFILE(SIG3DMS:MSGULT)
001300970526     F*------------
001400030325     Fcnaco00f  IF   E           K DISK
001500150423     Ftnass01l  IF   E           K DISK
001600150428     Fyficl03l  IF   E           K DISK
001700150424     Ffiinc01l  IF   E           K DISK
001800021217     Fazorg01l  IF   E           K DISK
001900150428     Fcnabi01l  IF   E           K DISK
002000970526     D*------------------------------------------------------------------------*
002100970526     D* SCHIERE
002200970526     D*------------------------------------------------------------------------*
002300970526     D*------------
002400970526     D* DECODIFICHE
002500970526     D*------------
002600970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* MESSAGGI
002900970526     D*------------
003000970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003100970528     D*------------
003200970528     D* COMANDI
003300970528     D*------------
003400970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003500970526     D*------------
003600970526     D* DESCRIZIONI OPZIONI
003700970526     D*------------
003800970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003900970526     D*------------
004000970526     D* DESCRIZIONI TASTI FUNZIONALI
004100970526     D*------------
004200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004300970526     D*------------
004400970526     D* OPZIONI DA CARICARE A VIDEO
004500970526     D*------------
004600970526     D OPV             S             19    DIM(24)
004700970526     D*------------
004800970526     D* FUNZIONI DA CARICARE A VIDEO
004900970526     D*------------
005000970527     D FUV             S             19    DIM(24)
005100150427     D*------------
005200150427     d dataeur         s               d   datfmt(*eur)
005300150427     d dataiso         s               d   datfmt(*iso)
005400991108     D*------------
005500150424     D CURSOR          DS
005600150424     D  RIRI                   1      2B 0 INZ
005700150424     D  R$$                    2      2
005800150424     D  COCO                   3      4B 0 INZ
005900150424     D  C$$                    4      4
006000150427      * DS per richiamo pgm FNLR36R
006100150428     D tnsb50ds      e DS
006200970526     D*------------------------------------------------------------------------*
006300970526     D* INPUT
006400970526     D*------------------------------------------------------------------------*
006500970526     D*------------
006600970526     D* RIDEFINIZIONE OPZIONI VIDEO
006700970526     D*------------
006800970526     D OPDDS           DS
006900970526     D  OPDOPZ                 1      2
007000970526     D  OPDDE1                 3     19
007100970526     D  OPDOP1                21     22
007200970526     D  OPDFI0                23     34
007300970526     D  OPDATT                35     35
007400970526     D  OPDFI1                36     80
007500970526     D  OPDDES                 1     19
007600970526     D  OPDRIG                 1     80
007700970526     D*------------
007800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
007900970526     D*------------
008000970526     D FUDDS           DS
008100970526     D  FUDDE1                 1      1
008200970526     D  FUDFUN                 2      3
008300970526     D  FUDDE2                 4     19
008400970526     D  FUDFI0                20     34
008500970526     D  FUDATT                35     35
008600970526     D  FUDFI1                36     80
008700970526     D  FUDDES                 1     19
008800970526     D  FUDRIG                 1     80
008900970526     D*------------
009000970526     D* DS GESTIONE TASTI FUNZIONALI
009100970526     D*------------
009200970526     D DSKEY         E DS
009300970526     D DEVDS1          DS
009400970526     D  KEY                  369    369
009500150427     D  NRG                  370    370
009600150427     D  NCL                  371    371
009700150427     D  SFLNRR               378    379B 0
009800970526     D*------------
009900970526     D* DS "XMSG" - GESTIONE MESSAGGI
010000970526     D*------------
010100970526     D DSMSG         E DS
010200970526     D*------------
010300020917     D* DS REPERIMENTO DATI UTENTE
010400970526     D*------------
010500020917     D TIBS34DS      E DS                                                       *Profili utente
010600020917     D DDATIUTE      E DS                                                       *Dati utente
010700020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
010800150212     D dute01        E DS                                                       *Utenti
010900150212     d dLat          e ds
011000150212     d TIBS02DS      E DS
011100030704     D*------------------
011200030704     D* DS CARICAMENTO P.O. GESTITI
011300030704     D*-------------------
011400030704     D TRUL31DS      E DS                  INZ
011500150212     D TRUL31DS2     E DS
011600150427      *
011700150428     D ndc001Ds      E DS                  Inz prefix(x)
011800150429     D  dtreg1001    E                     Inz('0001-01-01')
011900150429     D  dtreg2001    E                     Inz('0001-01-01')
012000150429     D  dtlgio1001   E                     Inz('0001-01-01')
012100150429     D  dtlgio2001   E                     Inz('0001-01-01')
012200150429     D  dtulmo1001   E                     Inz('0001-01-01')
012300150429     D  dtulmo2001   E                     Inz('0001-01-01')
012400150429     D  dtdoc001     E                     Inz('0001-01-01')
012500150427     D* Passaggio Parametri al PGM ANA009R
012600150427     D Ana009Ds      E DS                  Inz
012700150427     D  Err009       E                     Inz('0')
012800150427     D*-------------
012900150427     D* Passaggio Parametri al PGM ANA194R
013000150427     D Ana194Ds      E DS                  Inz
013100150427     D  Err194       E                     Inz('0')
013200150427     D*-------------
013300150427     D* Passaggio Parametri al PGM ANA196R
013400150427     D Ana196Ds      E DS                  Inz
013500150427     D  Err196       E                     Inz('0')
013600970526     D*------------
013700970526     D* ARCHITETTURA
013800970526     D*------------
013900970526     D KPJBA         E DS
014000021217     D*------------------
014100150427     D* Dati di ambiente ottenuti da XSOC
014200150427    >D SOC001        E DS                  EXTNAME(XSOC001DS)
014300150427     D*-------------
014400150427     D* DS Interna per dati di output di XSOC
014500150427     D XSOCDS          DS          1000
014600030128     D*------------------
014700030128     D* DS CONTROLLO DATA (8)
014800030128     D*------------------
014900030128     D WLBDA8          DS                  INZ
015000030128     D  G08DAT                 1      8  0
015100030128     D  G08INV                 9     16  0
015200030128     D  G08ERR                17     17
015300030128     D  G08TGI                18     22  0
015400021211     D*------------------
015500021211     D* PASSAGGIO PARAMETRI
015600021211     D*------------------
015700021211     D PARAM           DS                  INZ
015800150424     D  Plnp                          3  0
015900150424     D  Pnrs                          2  0
016000150424     D  Pnsp                          7  0
016100150424     D  Paas                          4  0
016200150427     D  PMSG                         80
016300970526     C*------------------------------------------------------------------------*
016400970526     C* MAIN LINES
016500970526     C*------------------------------------------------------------------------*
016600970526     C*
016700970526     C* SE NON CI SONO ERRORI, PROSEGUE
016800970526IF  1C     ERRL00        IFNE      '1'
016900970526     C*
017000970526     C* CARICA IL SFILE
017100970526     C                   EXSR      POSSFL
017200970526     C*
017300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
017400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
017500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
017600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
017700970526     C*
017800970526     C* REIMPOSTA ERRORE DI PROCERURA
017900970526     C                   MOVEL     '0'           ERRL00
018000970526     C*
018100970526     C* EMETTE IL SFILE
018200970526     C                   EXSR      OUTSFL
018300970526     C*
018400970526     C* GESTIONE TASTI FUNZIONALI
018500970526     C                   EXSR      FUNCON
018600970526     C*
018700970526E   2C                   ENDDO
018800970527     C*
018900970526E   1C                   ENDIF
019000991108     C*
019100970526     C* FINE PGM
019200970526     C                   SETON                                        LR
019300970526     C*------------------------------------------------------------------------*
019400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
019500970526     C*------------------------------------------------------------------------*
019600970526     C     F03GES        BEGSR
019700970526     C*
019800970526     C* IMPOSTA "PREMUTO F03"
019900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020000970526     C*
020100970526     C                   ENDSR
020200150424     C*------------------------------------------------------------------------*
020300150424     C* F04GES - GESTIONE F04 -> ricerche
020400150424     C*------------------------------------------------------------------------*
020500150424     C     F04GES        BEGSR
020600150424     C*
020700150424     C*
020800150424     C* determino Riga/Colonna del cursore
020900150424     C                   MOVE      NRG           R$$
021000150424     C                   MOVE      NCL           C$$
021100150424     C                   Z-ADD     RIRI          CURR
021200150424     C                   Z-ADD     COCO          CURC
021300150424     C*
021400150424   B1C                   SELECT
021500150424     C*
021600150424    >C*--------------------------------------------------------------
021700150424    >C* inserire le routine di ricerca
021800150424    >C*--------------------------------------------------------------
021900150424    >C* FMTD1  - MODULO
022000150424     C*
022100150424       // Ricerca Unità.
022200150427     C     H1NMFL        WHENEQ    'SIDFIL'
022300150427     C                   RESET                   Ana009ds
022400150424     C                   MOVEL     '01'          OPZ009
022500150424     C                   MOVEL     XSCSOC        SOC009
022600150427     C                   MOVEL     Ana009ds      KPJBU
022700150424     C                   CALL      'ANA009R'
022800150424     C                   PARM                    KPJBA
022900150427     C                   MOVEL     KPJBU         Ana009ds
023000150424     C*
023100150424     C     UNI009        IFNE      *BLANK
023200150427     C                   MOVEL     UNI009        sidfil
023300150424     C                   ENDIF
023400150424     C*
023500150424     C* imposto pos. del cursore
023600150424     C                   Z-ADD     CURR          H1RIGA
023700150424     C                   Z-ADD     CURC          H1COLO
023800150428     C* ricerca abi
023900150427     C     H1NMFL        WhenEq    'SIDABI'
024000150427     C                   Reset                   Ana194Ds
024100150427     C                   Movel     '01'          Opz194
024200150427     C                   Movel     xscSOC        SOCIETA194
024300150427     C                   Movel     Ana194Ds      KPJBU
024400150427     C                   Call      'ANA194R'
024500150427     C                   Parm                    KPJBA
024600150427     C                   Movel     KPJBU         Ana194Ds
024700150427     C                   If        Err194 <> '1'
024800150427     C                             and Abi194 <> *blank
024900150427     C                   Move      ABI194        sidabi
025000150427     C                   EndIf
025100150428     C                   Z-ADD     CURR          H1RIGA
025200150428     C                   Z-ADD     CURC          H1COLO
025300150428     C* ricerca CAB
025400150427     C     H1NMFL        WhenEq    'SIDCAB'
025500150427     C                   Reset                   Ana196Ds
025600150427     C                   Movel     '01'          Opz196
025700150427     C                   Movel     xscsoc        SOCIETA196
025800150430     c                   if        sidabi > *zeros
025900150430     c                   move      sidabi        abi196
026000150430     c                   endif
026100150427     C                   Movel     Ana196Ds      KPJBU
026200150427     C                   Call      'ANA196R'
026300150427     C                   Parm                    KPJBA
026400150427     C                   Movel     KPJBU         Ana196Ds
026500150427     C                   If        Err196 <> '1'
026600150427     C                             and CAB196 <> *blank
026700150427     C                   Move      CAB196        sidCAB
026800150427     C                   Move      abi196        sidabi
026900150427     C                   If        ABI196 <> *Blank
027000150428     C                             and sidabi = *zero
027100150427     C                   Move      ABI196        SIDABI
027200150427     C                   EndIf
027300150427     C                   EndIf
027400150427      *
027500150427      * imposto pos. del cursore
027600150427     C                   Z-ADD     CURR          H1RIGA
027700150427     C                   Z-ADD     CURC          H1COLO
027800150428     C                   Endsl
027900150424     C*
028000150424     C                   ENDSR
028100970526     C*------------------------------------------------------------------------*
028200970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
028300970526     C*------------------------------------------------------------------------*
028400970526     C     F05GES        BEGSR
028500970526     C*
028600970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
028700970526     C*
028800970526     C                   ENDSR
028900970526     C*------------------------------------------------------------------------*
029000970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
029100970526     C*------------------------------------------------------------------------*
029200000407     C     F06GES        BEGSR
029300000407     C*
029400021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029500000407     C                   EXSR      POSSFL                                       *RICARICA SFILE
029600021211     C                   ENDIF
029700000407     C*
029800970526     C                   ENDSR
029900970526     C*------------------------------------------------------------------------*
030000991108     C* F12GES - GESTIONE F12 -> RITORNO
030100970526     C*------------------------------------------------------------------------*
030200970526     C     F12GES        BEGSR
030300970526     C*
030400970526     C* IMPOSTA "PREMUTO F12"
030500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
030600970526     C*
030700970526     C                   ENDSR
030800970528     C*------------------------------------------------------------------------*
030900970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
031000970528     C*------------------------------------------------------------------------*
031100970528     C     F21GES        BEGSR
031200970612     C*
031300991117     C* NON GESTITA
031400970528     C*
031500970528     C                   ENDSR
031600970528     C*------------------------------------------------------------------------*
031700970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
031800970528     C*------------------------------------------------------------------------*
031900970528     C     F22GES        BEGSR
032000970528     C*
032100021211     C* NON GESTITA
032200970528     C*
032300970528     C                   ENDSR
032400970526     C*------------------------------------------------------------------------*
032500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
032600970526     C*------------------------------------------------------------------------*
032700970526     C     F23GES        BEGSR
032800970526     C*
032900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
033000970526     C*
033100970526     C                   ENDSR
033200970526     C*------------------------------------------------------------------------*
033300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
033400970526     C*------------------------------------------------------------------------*
033500970526     C     F24GES        BEGSR
033600970526     C*
033700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
033800970526     C*
033900970526     C                   ENDSR
034000970526     C*------------------------------------------------------------------------*
034100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
034200970526     C*------------------------------------------------------------------------*
034300970526     C     RUPGES        BEGSR
034400970526     C*
034500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
034600970526     C*
034700970526     C                   ENDSR
034800970526     C*------------------------------------------------------------------------*
034900991108     C* ENTGES - GESTIONE ENTER -> INVIO
035000970526     C*------------------------------------------------------------------------*
035100970526     C     ENTGES        BEGSR
035200000407     C*
035300000407     C                   MOVEL     '0'           ERRL00
035400991108     C*
035500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
035600970526     C                   EXSR      NEWPOS
035700021211     C*
035800021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
035900021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036000021211     C     SWPOSI        IFNE      0
036100021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
036200021211     C                   ELSE
036300021211     C     SFLULT        IFNE      0
036400021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
036500021211     C*
036600021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
036700021211     C     SWMODI        IFNE      0
036800021211     C                   EXSR      POSSFL
036900021211     C                   ENDIF
037000021211     C                   ENDIF
037100021211     C                   ENDIF
037200021211     C                   ENDIF
037300021211     C*
037400021211     C                   ENDSR
037500991014     C*------------------------------------------------------------------------*
037600000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
037700970526     C*------------------------------------------------------------------------*
037800970526     C     O05GES        BEGSR
037900000407     C*
038000030704     C* ESEGUO ROUTINE DI CONTROLLO
038100030704     C                   EXSR      DECODI
038200030704     C*
038300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038400070509     c                   else
038500070509      * se errore sul cliente forzo opzione non ok così gestico la forzatura
038600070509     C                   Z-ADD     1             OPZOK
038700021211     C                   ENDIF
038800000407     C*
038900970526     C                   ENDSR
039000150427     C*------------------------------------------------------------------------*
039100150427     C* O06GES - GESTIONE OPZIONE 6=VISUALIZZAZIONE RECORD
039200150427     C*------------------------------------------------------------------------*
039300150427     C     O06GES        BEGSR
039400150427     C*
039500150427     C     vidtpd        IFeq      'F'
039600150427     C                   MOVEL     '1'           ERRL00
039700150429     C                   MOVEL     MSG(7)        pmsg
039800150427     C                   ENDIF
039900150427     C*
040000150427     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040100150427     C                   movel     kpjbu         savkpjbu                       *RICARICA SFILE
040200150428     C                   movel     'R05'         I50OP0
040300150428     c                   move      vhaas         d50aas
040400150428     c                   move      vhlnp         d50LNP
040500150428     c                   move      vhnrs         d50NRS
040600150428     c                   move      vhnsp         d50NSP
040700150428     C                   MOVEL     tnsb50ds      KPJBU
040800150428     C                   CALL      'TNSB50R'
040900150427     C                   PARM                    KPJBA
041000150428     C                   movel     kpjbu         tnsb50ds                       *RICARICA SFILE
041100150428     c                   if        o50err <> *blank
041200150428     c                   movel     o50msg        pmsg
041300150428     c                   endif
041400150427     c                   else
041500150427      * se errore sul cliente forzo opzione non ok così gestico la forzatura
041600150427     C                   Z-ADD     1             OPZOK
041700150427     C                   ENDIF
041800150427     C*
041900150427     C                   ENDSR
042000150427     C*------------------------------------------------------------------------*
042100150427     C* O07GES - GESTIONE OPZIONE V=Versamento
042200150427     C*------------------------------------------------------------------------*
042300150427     C     O07GES        BEGSR
042400150427     C*
042500150427     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042600150427     C                   movel     kpjbu         savkpjbu                       *RICARICA SFILE
042700150428     c                   move      vhdrg         dataiso
042800150428     C                   reset                   ndc001ds
042900150428     c                   move      '05'          xopz001
043000150428     c                   move      vhnrg         xNRREG001
043100150428     c                   move      dataiso       xDTREG1001
043200150428     c                   move      dataiso       xDTREG2001
043300150428     c                   move      'COGE'        xSERREG001
043400150428     c                   move      'CG'          xCTB001
043500150429     c                   move      '201'         xsocieta001
043600150427     C                   MOVEL     ndc001ds      KPJBU
043700150427     C                   CALL      'NDC001R'
043800150427     C                   PARM                    KPJBA
043900150427     C                   MOVEL     kpjbu         ndc001ds
044000150427     C                   movel     savkpjbu      kpjbu
044100150427     c                   else
044200150427      * se errore sul cliente forzo opzione non ok così gestico la forzatura
044300150427     C                   Z-ADD     1             OPZOK
044400150427     C                   ENDIF
044500150427     C*
044600150427     C                   ENDSR
044700150427     C*------------------------------------------------------------------------*
044800150427     C* O08GES - GESTIONE OPZIONE P=Partita
044900150427     C*------------------------------------------------------------------------*
045000150427     C     O08GES        BEGSR
045100150427     C*
045200150429     C*
045300150430     C     vidtpd        IFne      'P'
045400150430     C     vidtpd        andne     'F'
045500150429     C                   MOVEL     '1'           ERRL00
045600150429     C                   MOVEL     MSG(7)        pmsg
045700150429     C                   ENDIF
045800150427     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
045900150427     C                   movel     kpjbu         savkpjbu                       *RICARICA SFILE
046000150428     C                   reset                   ndc001ds
046100150428     c                   move      '05'          xopz001
046200150428     c                   move      vhreass       xNRASREG001
046300150427     C                   MOVEL     ndc001ds      KPJBU
046400150427     C                   CALL      'NDC001R'
046500150427     C                   PARM                    KPJBA
046600150427     C                   MOVEL     kpjbu         ndc001ds
046700150427     C                   movel     savkpjbu      kpjbu
046800150427     c                   else
046900150427      * se errore sul cliente forzo opzione non ok così gestico la forzatura
047000150427     C                   Z-ADD     1             OPZOK
047100150427     C                   ENDIF
047200150427     C*
047300150427     C                   ENDSR
047400030704     C*------------------------------------------------------------------------*
047500030704     C* DECODI - CONTROLLA CAMPI E DECODIFICA
047600030704     C*------------------------------------------------------------------------*
047700030704     C     DECODI        BEGSR
047800030704     C*
047900030704     C                   MOVEL     '0'           ERRL00
048000030704     C*--------------------
048100030704     C* CONTROLLO AUTORIZZAZIONI UTENTE SU CODICE SOGGETTO MOVIMENTO "PRINCIPALE"
048200030704     C*--------------------
048300030704     C     ERRL00        IFEQ      '0'
048400030704     C* I CONTROLLI SI RIFERISCONO SOLO A SOGGETTI DI TIPO "F" O DI TIPO "C"
048500150424      *
048600030704     C                   ENDIF
048700150424     C     ERRL00        IFEQ      '0'
048800150424     C                   ENDIF
048900030704     C*
049000030704     C                   ENDSR
049100991028     C*------------------------------------------------------------------------*
049200991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
049300991112     C*------------------------------------------------------------------------*
049400991112     C     PASPAR        BEGSR
049500991112     C*
049600000407     C                   MOVEL     PARAM         KPJBU
049700991112     C*
049800991112     C                   ENDSR
049900970526     C*------------------------------------------------------------------------*
050000970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
050100970526     C*------------------------------------------------------------------------*
050200970526     C     NEWPOS        BEGSR
050300970526     C*
050400970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
050500970526     C*         1= RICHIESTO POSIZIONAMENTO
050600970526     C                   Z-ADD     0             SWPOSI            7 0
050700970526     C*
050800991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
050900970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
051000970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
051100970526     C                   Z-ADD     0             SWMODI            1 0
051200970530     C*
051300970530     C* REIMPOSTA INDICATORI DI ERRORE
051400150504     C                   SETOFF                                       818283
051500150504     C                   SETOFF                                       848586
051600150504     C                   SETOFF                                       878889
051700150504     C                   SETOFF                                       80
051800970526     C*
051900970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
052000150504     C                   IF        sidreg <> depreg OR
052100150504     C                             sidda1 <> depda1 OR
052200150504     C                             sidfil <> depfil OR
052300150504     C                             sidabi <> depabi OR
052400150504     C                             sidcab <> depcab OR
052500150504     C                             siddta <> depdta OR
052600150504     C                             sidnas <> depnas OR
052700150504     C                             sidre1 <> depre1 OR
052800150504     C                             sidre2 <> depre2
052900150504     C                   EXSR      CHKPAR
053000150504     C     ERRL00        IFEQ      '0'
053100150504     C                   Z-ADD     1             SWPOSI
053200150504     C                   Z-ADD     sidreg        depreg
053300150504     C                   MOVEL     sidda1        depda1
053400150504     C                   MOVEL     sidfil        depfil
053500150504     C                   MOVEL     sidabi        depabi
053600150504     C                   MOVEL     sidcab        depcab
053700150504     C                   MOVEL     siddta        depdta
053800150504     C                   MOVEL     sidnas        depnas
053900150504     C                   MOVEL     sidre1        depre1
054000150504     C                   MOVEL     sidre2        depre2
054100150504     C                   ENDIF
054200150504     C                   ENDIF
054300150504     C*
054400150504     C                   ENDSR
054500021217     C*------------------------------------------------------------------------*
054600021217     C* CHKPO - CONTROLLA CODICE P.O.
054700021217     C*------------------------------------------------------------------------*
054800021217     C     CHKPO         BEGSR
054900021217     C*
055000150430     C     sidfil        SETLL     azorg01l                               55
055100150430     C                   IF        *IN55 = *OFF
055200150430     C                   EVAL      W_ERR = '1'
055300150430     C                   ENDIF
055400021217     C*
055500021217     C                   ENDSR
055600970526     C*------------------------------------------------------------------------*
055700021217     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
055800970526     C*------------------------------------------------------------------------*
055900970528     C     SETFIL        BEGSR
056000970526     C*
056100970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
056200150424     C     WTIPRI        IFEQ      'FIL'
056300150424     C     sidfil        SETLL     tnass01l                           99
056400021211     C                   ENDIF
056500980107     C*
056600021211     C* LETTURA RECORD
056700021211     C     *IN99         IFEQ      *OFF
056800970526     C                   EXSR      LETFIL
056900021211     C                   ENDIF
057000970526     C*
057100970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
057200021211     C     *IN99         IFEQ      *ON
057300970526     C                   Z-ADD     1             SFLEOF
057400021211     C                   ENDIF
057500970526     C*
057600970526     C                   ENDSR
057700970526     C*------------------------------------------------------------------------*
057800970526     C* LETFIL - LETTURA RECORD
057900970526     C*------------------------------------------------------------------------*
058000970526     C     LETFIL        BEGSR
058100970526     C*
058200970526     C* LEGGE FINO A:
058300970526     C*     - FINE FILE
058400970526     C*     - TROVATO RECORD VALIDO
058500970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
058600021211     C     *IN99         DOWEQ     *OFF
058700970526     C     WRECOK        ANDEQ     'N'
058800970526     C*
058900970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
059000150424     C     WTIPRI        IFEQ      'FIL'                                        *PER DATA/ORA/UTENTE
059100150424     C                   READ      tnass01l                               99
059200021211     C                   ENDIF
059300970528     C*
059400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
059500021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
059600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
059700021211     C                   ELSE
059800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
059900021211     C                   ENDIF
060000021211     C                   ENDDO
060100970526     C*
060200970526     C                   ENDSR
060300970526     C*------------------------------------------------------------------------*
060400021217     C* CHKREC - CONTROLLA VALIDITA' RECORD
060500970526     C*------------------------------------------------------------------------*
060600970526     C     CHKREC        BEGSR
060700970526     C*
060800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
060900150424     C* FILTRA SE RICHIESTO un P.O
061000150424     C                   if        sidfil = *zeros
061100030128     C                   else
061200150424     C                   if        assfle = sidfil
061300030128     C                   else
061400030128     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
061500030128     C                   endif
061600030128     C                   endif
061700150424     C* FILTRA SE RICHIESTa una data registrazione
061800150424     C                   if        sidda1 = *zeros
061900150424     C                   else
062000150424     C                   if        sidda1g = assddv
062100150424     C                   else
062200150424     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
062300150424     C                   endif
062400150424     C                   endif
062500150424     C* FILTRA SE RICHIESTa una registrazione
062600150424     C                   if        sidreg = *zeros
062700150424     C                   else
062800150424     C                   if        sidreg = assndv
062900150424     C                   else
063000150424     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
063100150424     C                   endif
063200150424     C                   endif
063300150424     C* FILTRA SE RICHIESTo ABI
063400150424     C                   if        sidabi = *zeros
063500150424     C                   else
063600150424     C                   if        sidabi = assabi
063700150424     C                   else
063800150424     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
063900150424     C                   endif
064000150424     C                   endif
064100150424     C* FILTRA SE RICHIESTo CAB
064200150424     C                   if        sidcab = *zeros
064300150424     C                   else
064400150424     C                   if        sidcab = asscab
064500150424     C                   else
064600150424     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
064700150424     C                   endif
064800150424     C                   endif
064900150424     C* FILTRA SE RICHIESTa data assegno
065000150424     C                   if        siddta = *zeros
065100150424     C                   else
065200150424     C                   if        siddtag = assdte
065300150424     C                   else
065400150424     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
065500150424     C                   endif
065600150424     C                   endif
065700150424     C* FILTRA SE RICHIESTa data assegno
065800150428     C                   if        sidnas = *zeros
065900150424     C                   else
066000150428     c                   movel     assnra        assnra4           4 0
066100150428     C                   if        sidnas = assnra4
066200150424     C                   else
066300150424     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
066400150424     C                   endif
066500150424     C                   endif
066600150424     C* FILTRA SE RICHIESTo range importi
066700150424     C                   if        sidre1 = *zeros and sidre2 = 0
066800150424     C                   else
066900150424     C                   if        asscas >= sidre1 and asscas <= sidre2
067000150424     C                   else
067100150424     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
067200150424     C                   endif
067300150424     C                   endif
067400991108     C*
067500970526     C                   ENDSR
067600970526     C*------------------------------------------------------------------------*
067700000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
067800970526     C*------------------------------------------------------------------------*
067900970526     C     IRESFL        BEGSR
068000150424     c                   move      assndv        VHNRG
068100150430     c                   movel     assfle        vhnrg
068200150424     c                   move      assddv        VHDRG
068300150428     c                   movel     20            VHDRG
068400150424     c                   move      assAAS        VHAAS
068500150424     c                   move      assLNP        VHLNP
068600150424     c                   move      assNRS        VHNRS
068700150424     c                   move      assNSP        VHNSP
068800150427     c                   move      assdte        vhdte
068900970526     C*
069000150424     c                   move      assndv        vidndv
069100150424     c                   move      asstpd        vidtpd
069200150424     c                   move      assabi        vidabi
069300150424     c                   move      asscab        vidcab
069400150428     c                   movel     assnra        vidnra
069500150429     c                   move      asstpa        vidtpa
069600150429     c                   z-add     asscas        vidcas
069700150424     c                   clear                   viddte
069800150424     c                   if        assdte > 0
069900150424     c                   move      assdte        dataiso
070000150424     c                   move      dataiso       dataeur
070100150424     c                   move      dataeur       viddte
070200150424     c                   endif
070300150424     c                   clear                   vidddv
070400150424     c                   if        assddv > 0
070500150428     c     *ymd          move      assddv        dataiso
070600150424     c                   move      dataiso       dataeur
070700150424     c                   move      dataeur       vidddv
070800150424     c                   endif
070900150424     c                   clear                   vidspcl
071000150430     c                   if        vidtpd <>'F'
071100150424     c                   eval      vidspcl = %trim(%editc(asslnp:'X')) + '-' +
071200150424     c                             %editc(assnrs:'X') + '-' + %editc(assnsp:'X')
071300150424     c                   endif
071400150507      *
071500150507      * decodifica filiale di versamento
071600150507     c                   move      assfle        vidfle
071700150507     C                   clear                   VIDfled
071800150507     c                   if        vidfle > 0
071900150507     C     vidfle        CHAIN     azorg01l
072000150507     C                   IF        %found(azorg01l)
072100150507     C                   EVAL      VIDfled = orgDES
072200150507     C                   ELSE
072300150507     C                   MOVEL     *all'*'       VIDfled
072400150507     C                   ENDIF
072500150507     C                   endif
072600150507      * decodifica filiale dei campi di selezione
072700150507     C                   clear                   VIDRAG
072800150507     c                   if        sidfil > 0
072900150428     C     sidfil        CHAIN     azorg01l
073000150424     C                   IF        %found(azorg01l)
073100150424     C                   EVAL      VIDRAG = orgDES
073200150424     C                   ELSE
073300150424     C                   MOVEL     *all'*'       VIDRAG
073400150424     C                   ENDIF
073500150507     C                   endif
073600150427      * file incassi
073700150427     c                   if        vidtpd = 'P'
073800150427     C     ksped         CHAIN     fiinc01l
073900150427     C                   IF        %found(fiinc01l)
074000150428     C                   EVAL      vhreass = INCNRASREG
074100150427     C                   endif
074200150427     c                   endif
074300150427     C*
074400150427     c                   if        vidtpd = 'F'
074500150427     c                   movel     vidnra        knra
074600150427     C     kass          CHAIN     yficl03l
074700150427     C                   IF        %found(yficl03l)
074800150428     C                   EVAL      vhreass = ICLNRASREG
074900150428     c                   move      iclkcc        acokcc
075000150428     c                   move      iclksc        acoksc
075100150427     C     kaco          CHAIN     cnaco00f
075200150427     C                   IF        %found(cnaco00f)
075300150430     C                   EVAL      vidspcl = %trim(%editc(acoksc:'X')) + '-'
075400150427     c                             + acorag
075500150427     C                   endif
075600150427     c                   endif
075700150428     c                   endif
075800070509     c                   eval      vihforza = *off
075900991108     C*
076000970526     C                   ENDSR
076100970526     C*------------------------------------------------------------------------*
076200991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
076300970526     C*------------------------------------------------------------------------*
076400970526     C     CHKPAR        BEGSR
076500021218     C*
076600021218     C* INIZIALIZZO FLAG GENERICO DI ERRORE SELEZIONI
076700150428     C                   MOVEL     '0'           W_ERR             1
076800970528     C*
076900021211     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *01L (COD)
077000150428     C                   MOVEL     'FIL'         WTIPRI            3
077100030128     C*
077200150427     C*
077300150427     C* CONTROLLO SE RICHIESTO P.O. INSERIMENTO
077400150427     C                   IF        W_ERR = '0'
077500150427     C                   IF        sidfil <> 0
077600150427     C                   EXSR      CHKPO
077700150427     C                   IF        W_ERR = '1'
077800150427     C                   SETON                                        80
077900150427     C                   ENDIF
078000150427     C                   ENDIF
078100150427     C                   ENDIF
078200030128     C* CONTROLLO VALIDITA DATA INIZIO ELENCO
078300030128     C                   IF        W_ERR = '0'
078400150427     C                   IF        sIDDA1 > *zeros
078500150427     C                   Z-ADD     siDDA1        DAW8
078600030128     C                   EXSR      CHKDA8
078700150427     C                   Z-ADD     WDA8          sidDA1
078800150428     C                   move      DAW8          sidda1g           6 0
078900030128     C                   IF        G08ERR = '1'
079000030128     C                   EVAL      W_ERR = '1'
079100030128     C                   SETON                                        81
079200030128     C                   ENDIF
079300030128     C                   ENDIF
079400030128     C                   ENDIF
079500030128     C*
079600021217     C* CONTROLLO SE RICHIESTO CLIENTE
079700021218     C                   IF        W_ERR = '0'
079800150427     C                   IF        sidabi <> 0
079900150427     C                   EXSR      CHKabi
080000021217     C                   ENDIF
080100021218     C                   ENDIF
080200150427     C*
080300150427     C* CONTROLLO SE RICHIESTO CLIENTE
080400150427     C                   IF        W_ERR = '0'
080500150427     C                   IF        sidcab <> 0
080600150428     C                   EXSR      CHKabi
080700150427     C                   ENDIF
080800150427     C                   ENDIF
080900000407     C*
081000150427     C* CONTROLLO VALIDITA DATA INIZIO ELENCO
081100150427     C                   IF        W_ERR = '0'
081200150427     C                   IF        sIDDta > *zeros
081300150427     C                   Z-ADD     siDDta        DAW8
081400150427     C                   EXSR      CHKDA8
081500150427     C                   Z-ADD     WDA8          sidDta
081600150427     C                   Z-ADD     DAW8          siddtag           8 0
081700150427     C                   IF        G08ERR = '1'
081800150427     C                   EVAL      W_ERR = '1'
081900150427     C                   SETON                                        84
082000150427     C                   ENDIF
082100150427     C                   ENDIF
082200150427     C                   ENDIF
082300150429     C*
082400150429     C* CONTROLLO range importi
082500150429     C                   IF        W_ERR = '0'
082600150429     C                   IF        sidre1  > sidre2
082700150429     C                   eval      errl00 = '1'
082800150429     C                   MOVEL     MSG(8)        dsmsms
082900150429     c                   seton                                        8586
083000150429     C                   ENDIF
083100150429     C                   ENDIF
083200970526     C                   ENDSR
083300970526     C*------------------------------------------------------------------------*
083400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
083500970526     C*------------------------------------------------------------------------*
083600970526     C     CHKFOP        BEGSR
083700991112     C*
083800991112     C* OPZIONI
083900970626     C*
084000000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
084100021211     C     DEPOP0        IFEQ      'P01'
084200000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
084300000420     C                   MOVEL     'S'           OPDATT
084400000420     C                   MOVEL     OPDRIG        OPD(1)
084500021211     C                   ELSE
084600000420     C* OPZIONI
084700150429     C                   MOVEL     OPD(6)        OPDRIG                         *INTERROGAZIONE
084800150429     C                   MOVEL     'S'           OPDATT
084900150429     C                   MOVEL     OPDRIG        OPD(6)
085000150429     C                   MOVEL     OPD(7)        OPDRIG                         *INTERROGAZIONE
085100150429     C                   MOVEL     'S'           OPDATT
085200150429     C                   MOVEL     OPDRIG        OPD(7)
085300150429     C                   MOVEL     OPD(8)        OPDRIG                         *INTERROGAZIONE
085400150429     C                   MOVEL     'S'           OPDATT
085500150429     C                   MOVEL     OPDRIG        OPD(8)
085600150429     C*                  MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
085700150429     C*                  MOVEL     'S'           OPDATT
085800150429     C*                  MOVEL     OPDRIG        OPD(5)
085900150429     C                   ENDIF
086000970626     C*
086100000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
086200021211     C     DEPOP0        IFEQ      'P02'
086300150427     C*                  MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
086400150427     C*                  MOVEL     'S'           FUDATT
086500150427     C*                  MOVEL     FUDRIG        FUD(6)
086600021211     C                   ENDIF
086700970526     C*
086800970526     C* GESTIONE OPZIONI A VIDEO
086900970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
087000970526     C                   EXSR      OPZGES
087100970526     C*
087200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
087300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
087400970526     C                   EXSR      FUNGES
087500970526     C*
087600970526     C                   ENDSR
087700020917     C*--------------------------------------------------------------------------------------------*
087800020917     C* REPERISCE I DATI UTENTE
087900020917     C*--------------------------------------------------------------------------------------------*
088000020917     C     REPDATIUTE    BEGSR
088100020917     C*
088200020917     C* INIZIALIZZA VARIABILI DI WRK
088300020917     C                   CLEAR                   TIBS34DS
088400020917     C                   CLEAR                   AZUTEDS
088500020917     C                   CLEAR                   DDATIUTE
088600150212     C                   CLEAR                   dute01
088700020917     C*
088800020917     C     *DTAARA       DEFINE    §azute        azuteds
088900020917     C     *DTAARA       DEFINE    §datiute      ddatiute
089000020917     C                   IN(E)     *DTAARA
089100021211     C                   IF        %Error
089200021211     C                   EVAL      I34Tla = 'L'
089300020917     C                   CALL      'TIBS34R'
089400020917     C                   PARM                    Tibs34Ds
089500020917     C                   IN        *DTAARA
089600021211     C                   ENDIF
089700020917     C*
089800020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
089900020917     C                   MOVEL(P)  RSUT          DSFIRS           20
090000020917     C*
090100020917     C                   ENDSR
090200030128     C*------------------------------------------------------------------------*
090300030128     C* CHKDA8 - CONTROLLA LA DATA (8)
090400030128     C*------------------------------------------------------------------------*
090500030128     C     CHKDA8        BEGSR
090600030128     C*
090700030305     C                   Z-ADD     DAW8          G08DAT
090800030305     C                   Z-ADD     *ZEROS        G08INV
090900030305     C                   MOVEL     '0'           G08ERR
091000030128     C                   CALL      'XSRDA8'
091100030128     C                   PARM                    WLBDA8
091200150427     C                   Z-ADD     G08DAT        WDA8              8 0
091300150427     C                   Z-ADD     G08INV        DAW8              8 0
091400030128     C*
091500030128     C                   ENDSR
091600150427     C*------------------------------------------------------------------------*
091700150428     C* CHKabi - CONTROLLA abi cab
091800150427     C*------------------------------------------------------------------------*
091900150427     C     CHKabi        BEGSR
092000150427     C*
092100150428     C                   if        SIDCAB = 0
092200150428     C     sidabi        chain     cnabi01l
092300150428     c                   if        not%found(cnabi01l)
092400150428     C                   MOVEL     '1'           ERRL00
092500150428     C                   MOVEL     MSG(6)        dsmsms
092600150428     C                   EVAL      W_ERR = '1'
092700150428     c                   seton                                        88
092800150428     C                   EndIf
092900150428     C                   else
093000150428     C     kabicab       chain     cnabi01l
093100150428     c                   if        not%found(cnabi01l)
093200150428     C                   EVAL      W_ERR = '1'
093300150428     C                   MOVEL     '1'           ERRL00
093400150428     C                   MOVEL     MSG(6)        dsmsms
093500150428     c                   seton                                        89
093600150428     C                   EndIf
093700150427     C                   EndIf
093800150427     C*
093900150427     C                   ENDSR
094000150427     C************************************************************
094100150427     C* REPERIMENTO DATI SOCIETA'
094200150427     C************************************************************
094300150427     C     REPSOC        BEGSR
094400150427     C*
094500150427     C                   CALLB     'XSOC'
094600150427     C                   PARM                    TIPXSC            6
094700150427     C                   PARM                    SOCXSC            3
094800150427     C                   PARM                    CDSXSC            9 0
094900150427     C                   PARM                    MODXSC            3
095000150427     C                   PARM      *BLANKS       RTNXSC            1
095100150427     C                   PARM                    XSOCDS
095200150427     C                   PARM                    KPJBA
095300150427     C*
095400150427     C                   ENDSR
095500970526     C*------------------------------------------------------------------------*
095600970526     C* *INZSR - OPERAZIONI INIZIALI
095700970526     C*------------------------------------------------------------------------*
095800970526     C     *INZSR        BEGSR
095900970526     C*---
096000970526     C* RICEVIMENTO PARAMETRI
096100970526     C*---
096200970526     C     *ENTRY        PLIST
096300970612     C                   PARM                    KPJBA
096400150427     C* Reperimento dati società
096500150427     C*
096600150427    >C                   MOVEL     'SOC001'      TIPXSC
096700150427     C                   MOVEL     *BLANK        SOCXSC
096800150427     C                   EXSR      REPSOC
096900150427     C     RTNXSC        IFNE      '1'
097000150427     C                   MOVEL     XSOCDS        SOC001
097100150427     C                   ELSE
097200150427     C                   return
097300150427     C                   ENDIF
097400150427     C*
097500150427     C                   Z-ADD     0             CURR              2 0
097600150427     C                   Z-ADD     0             CURC              2 0
097700021211     C                   MOVEL     KPJBU         PARAM
097800150427     C                   MOVEL     'P02'         DEPOP0            3
097900970526     C*
098000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
098100970526     C     XMSGPA        PLIST
098200970526     C                   PARM                    DSMSMS
098300970526     C                   PARM                    DSMSPG
098400970526     C                   PARM                    DSMSKE
098500970526     C                   PARM                    DSMSER
098600970526     C*
098700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
098800970526     C                   CALL      'XKEY'
098900970526     C                   PARM                    DSKEY
099000970526     C*---
099100970526     C* VARIABILI NON RIFERITE AL DATA BASE
099200970526     C*---
099300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
099400970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
099500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
099600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
099700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
099800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
099900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
100000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
100100970526     C*---
100200150424     C     *LIKE         DEFINE    sidreg        depreg
100300150424     C     *LIKE         DEFINE    sidda1        depda1
100400150424     C     *LIKE         DEFINE    sidfil        depfil
100500150424     C     *LIKE         DEFINE    sidabi        depabi
100600150424     C     *LIKE         DEFINE    sidcab        depcab
100700150424     C     *LIKE         DEFINE    siddta        depdta
100800150428     C     *LIKE         DEFINE    sidnas        depnas
100900150424     C     *LIKE         DEFINE    sidre1        depre1
101000150424     C     *LIKE         DEFINE    sidre2        depre2
101100021217     C     *LIKE         DEFINE    KPJBU         SAVKPJBU
101200991108     C*---
101300970526     C* VARIABILI DI CONTROLLO
101400970526     C*---
101500970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
101600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
101700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
101800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
101900970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
102000150403     C                   Z-ADD     8             SFLPAG                         *N°RIGHE X PAG
102100970526     C*
102200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
102300150427     C                   MOVEL     'YCOAS1R'     DSMSPG
102400150427     C                   MOVEL     'YCOAS1R'     MSGQUE
102500970526     C                   MOVEL     *BLANKS       MSGKEY
102600970526     C                   MOVEL     *BLANKS       DSMSMS
102700970526     C                   MOVEL     *BLANKS       DSMSKE
102800970526     C                   Z-ADD     *ZEROS        DSMSER
102900970526     C*---
103000970526     C* CHIAVI DI LETTURA
103100970526     C*---
103200030325     C* LETTURA cnaco00f - COMPLETA
103300150427     C     Kaco          KLIST
103400030325     C                   KFLD                    acoKUT
103500150428     C                   KFLD                    acoKCC
103600150428     C                   KFLD                    acoKSC
103700150427     C                   z-add     1             acoKUT
103800991108     C*
103900150427     C* LETTURA cnaco00f - COMPLETA
104000150427     C     Ksped         KLIST
104100150427     C                   KFLD                    vhAAS
104200150427     C                   KFLD                    vhLNP
104300150427     C                   KFLD                    vhNRS
104400150427     C                   KFLD                    vhNSP
104500150427     C                   KFLD                    vidTPD
104600150427      *
104700150428     C     Kabicab       KLIST
104800150428     C                   KFLD                    sidabi
104900150428     C                   KFLD                    sidcab
105000150428      *
105100150427     C     Kass          KLIST
105200150427     C                   KFLD                    vidabi
105300150428     C                   KFLD                    vidcab
105400150427     C                   KFLD                    vhdte
105500150427     C                   KFLD                    knra             10
105600150427     C*
105700970527     C* REPERIMENTO DATI UTENTE
105800020917     C                   EXSR      REPDATIUTE
105900970526     C*
106000970526     C* IMPOSTA I PARAMETRI DI INGRESSO
106100970526     C                   EXSR      CHKPAR
106200991115     C*
106300970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
106400970526     C                   EXSR      CHKFOP
106500970526     C*
106600970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
106700021211     C     DEPOP0        IFEQ      'P01'
106800970530     C                   MOVEL     DEC(3)        VIDING
106900021211     C                   ENDIF
107000021211     C     DEPOP0        IFEQ      'P02'
107100970526     C                   MOVEL     DEC(1)        VIDING
107200021211     C                   ENDIF
107300021211     C     DEPOP0        IFEQ      'P05'
107400970526     C                   MOVEL     DEC(2)        VIDING
107500021211     C                   ENDIF
107600970526     C*
107700970526     C                   ENDSR
107800970526     C*------------------------------------------------------------------------*
107900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108100970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
108200970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
108300970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
108400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108600970526     C*------------------------------------------------------------------------*
108700970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
108800970526     C*------------------------------------------------------------------------*
108900970526     C     POSSFL        BEGSR
109000970526     C*
109100970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
109200970526     C                   EXSR      CLRSFL
109300030128     C                   Z-ADD     *zeros        VIDSAL
109400150507     C                   clear                   VIDrag
109500030305     C*
109600030305     C* SE NN EFFETTUATE SELEZIONI EMETTO VIDEATA DI RICHIESTA
109700150424     C                   IF        sidreg = *zeros AND
109800150424     C                             sidda1 = *zeros AND
109900150424     C                             sidfil = *zeros AND
110000150424     C                             sidabi = *zeros AND
110100150424     C                             sidcab = *zeros AND
110200150424     C                             siddta = *zeros AND
110300150428     C                             sidnas = *zeros AND
110400150424     C                             sidre1 = *zeros AND
110500150424     C                             sidre2 = *zeros
110600030305     C                   SETON                                        97
110700030305     C                   ELSE
110800030305     C                   SETOFF                                       97
110900970526     C*
111000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
111100970526     C                   EXSR      SETFIL
111200970526     C*
111300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
111400970526     C     SFLEOF        IFEQ      0
111500970526     C                   EXSR      CARSFL
111600030305     C                   ENDIF
111700030305     C                   ENDIF
111800970526     C*
111900970526     C                   ENDSR
112000970526     C*------------------------------------------------------------------------*
112100970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
112200970526     C*------------------------------------------------------------------------*
112300970526     C     ENTATT        BEGSR
112400970526     C*
112500970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
112600970526     C                   Z-ADD     0             OPZOK             1 0
112700970526     C*
112800970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
112900970526     C                   Z-ADD     0             SFLVAR            5 0
113000970526     C*
113100970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
113200970526     C                   Z-ADD     0             SFLERR
113300970526     C*
113400970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
113500970526     C                   Z-ADD     SFLULT        WFLULT
113600970526     C*
113700970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
113800021217     C                   READC     SIG3D05                                98
113900970526     C     *IN98         DOWEQ     '0'
114000970526     C*
114100970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
114200970526     C                   ADD       1             SFLVAR
114300970526     C*
114400970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
114500970526     C     OPZOK         IFEQ      0
114600970526     C*
114700970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
114800970526     C     OPZKEY        IFNE      *BLANKS
114900970526     C                   EXSR      OPZCON
115000970526     C                   END
115100970526     C*
115200970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
115300970526     C*            2. PULIZIA SCELTA IN SFL
115400970526     C     OPZOK         IFEQ      0
115500970526     C                   MOVEL     '0'           *IN41
115600970526     C                   MOVEL     *BLANKS       OPZKEY
115700970526     C                   ELSE
115800970526     C*
115900970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
116000970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
116100970526     C                   MOVEL     '1'           *IN41
116200970526     C                   Z-ADD     SFLULT        SFLERR
116300970526     C                   END
116400970526     C                   ELSE
116500970526     C*
116600970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
116700970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
116800970526     C                   MOVEL     '1'           *IN41
116900970526     C                   END                                                    OPZOK=0
117000970526     C*
117100970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
117200970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
117300970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
117400970526     C     OPZOK         IFNE      0
117500970526     C                   Z-ADD     SFLERR        SFLNBR
117600970526     C                   ELSE
117700970526     C                   Z-ADD     SFLULT        SFLNBR
117800970526     C                   END
117900970526     C*
118000970526     C* AGGIORNO RECORD SFL
118100021217     C                   UPDATE    SIG3D05
118200970526     C*
118300021217     C                   READC     SIG3D05                                98
118400970526     C                   END                                                    DO WHILE
118500970526     C*
118600970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
118700970526     C                   Z-ADD     WFLULT        SFLULT
118800970526     C*
118900970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
119000970526     C     SFLVAR        IFEQ      0
119100970526     C                   END
119200970526     C*
119300040702     C***  OPZKEY        IFNE      ' 5'
119400040702     C***                EXSR      F05GES
119500040702     C***                ENDIF
119600991112     C*
119700970526     C                   ENDSR
119800970526     C*------------------------------------------------------------------------*
119900970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
120000970526     C*------------------------------------------------------------------------*
120100970526     C     CLRSFL        BEGSR
120200970526     C*
120300970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
120400970526     C* (0=NON FINE FILE  1=FINE FILE)
120500970526     C                   Z-ADD     0             SFLEOF
120600970526     C*
120700970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
120800970526     C                   Z-ADD     0             SFLULT
120900970526     C*
121000970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
121100970526     C                   MOVEL     '1'           *IN40
121200021217     C                   WRITE     SIG3D06
121300970526     C                   MOVEL     '0'           *IN40
121400970526     C*
121500970526     C                   ENDSR
121600970526     C*------------------------------------------------------------------------*
121700970526     C* CARSFL - CARICAMENTO SFL
121800970526     C*------------------------------------------------------------------------*
121900970526     C     CARSFL        BEGSR
122000970526     C*
122100970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
122200970526     C     SFLULT        IFEQ      0
122300970526     C                   EXSR      SETFIL
122400970526     C                   END
122500970526     C*
122600970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
122700970526     C                   Z-ADD     SFLULT        WFLULT
122800970526     C*
122900970526     C* CICLO:
123000970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
123100970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
123200040225     C                   Z-ADD     1             SFLRIG            5 0
123300150417 XXX C***  SFLRIG        DOWLE     SFLPAG
123400150417     C***  SFLEOF        ANDEQ     0
123500150417     C     SFLEOF        DOWEQ     0
123600150417     C* SE NN SUPERATO NUMERO MAX RECORD IN SUB-FILE
123700150417     C     SFLRIG        IFLE      9999
123800970526     C*
123900970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
124000991116     C                   MOVEL     *BLANKS       OPZKEY
124100970526     C*
124200040225     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
124300970526     C                   EXSR      IRESFL
124400970526     C*
124500970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
124600970526     C                   ADD       1             SFLULT
124700970526     C*
124800970526     C* SCRITTURA RECORD SFL
124900021217     C                   WRITE     SIG3D05
125000970526     C                   ADD       1             SFLRIG
125100150417     C* SE RAGGIUNTO NUMERO MAX RECORD IN SUB-FILE => EMETTO MESSAGGIO
125200150417     C                   ELSE
125300150417     C                   MOVEL     MSG(5)        DSMSMS
125400150417     C                   ENDIF
125500040225     C*
125600040225     C* INCREMENTO IL SALDO A VIDEO INDIPENDENTEMENTE DAL CARICAMENTO DEI RECORD DEL SUB-FILE
125700150424     C                   ADD       asscas        VIDSAL
125800970526     C*
125900970526     C* LETTURA FILE DA CARICARE
126000970526     C                   EXSR      LETFIL
126100970526     C                   END                                                    DO WHILE
126200970526     C*
126300970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
126400970526     C     WFLULT        IFNE      SFLULT
126500970526     C                   Z-ADD     WFLULT        SFLNBR
126600970526     C                   ADD       1             SFLNBR
126700970526     C                   END
126800970526     C*
126900970526     C                   ENDSR
127000970526     C*------------------------------------------------------------------------*
127100970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
127200970526     C*------------------------------------------------------------------------*
127300970526     C     OUTSFL        BEGSR
127400970526     C*
127500970526     C* WRITE RECORD TESTATA VIDEO
127600021217     C                   WRITE     SIG3D01
127700970526     C*
127800970526     C* VISUALIZZAZIONE MESSAGGIO
127900970526     C                   EXSR      SNDMSG
128000970526     C*
128100970526     C* WRITE RECORD OPZIONI VIDEO
128200021217     C                   WRITE     SIG3D02
128300970526     C*
128400970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
128500021217     C                   WRITE     SIG3D03
128600970526     C*
128700970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
128800021217     C                   WRITE     SIG3D04
128900970526     C*
129000970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
129100970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
129200970526     C     SFLEOF        IFEQ      0
129300970526     C                   MOVEL     '0'           *IN42
129400970526     C                   ELSE
129500970526     C*
129600970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
129700970526     C                   MOVEL     '1'           *IN42
129800970526     C                   END
129900970526     C*
130000970526     C* WRITE RECORD TASTI FUNZIONALI
130100021217     C                   WRITE     SIG3D09
130200970526     C*
130300970526     C* CASO DI SFL VUOTO
130400970526     C     SFLULT        IFLE      0
130500970526     C*
130600970526     C*    - WRITE RECORD VUOTO
130700021217     C                   WRITE     SIG3D0A
130800021217     C                   READ      SIG3D0A                                99
130900970526     C*
131000970526     C                   ELSE
131100970526     C*
131200970526     C* CASO DI SFL CON RECORDS
131300970526     C*
131400970526     C*    - WRITE RECORD CONTROLLO SFL
131500021217     C                   WRITE     SIG3D06
131600021217     C                   READ      SIG3D06                                99
131700970526     C                   END
131800970526     C*
131900970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
132000021217     C                   READ      SIG3D03                                99
132100970526     C*
132200970526     C                   ENDSR
132300970526     C*------------------------------------------------------------------------*
132400970526     C* SNDMSG - MANDA MESSAGGIO
132500970526     C*------------------------------------------------------------------------*
132600970526     C     SNDMSG        BEGSR
132700970526     C*
132800970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
132900970526     C                   MOVEL     '1'           *IN40
133000021217     C                   WRITE     SIG3DMC
133100970526     C                   MOVEL     '0'           *IN40
133200970526     C*
133300970526     C* VISUALIZZAZIONE MESSAGGIO
133400970526     C                   Z-ADD     0             MSGULT
133500970526     C                   CALL      'XMSG'        XMSGPA
133600970526     C     DSMSER        IFEQ      0
133700970526     C                   MOVEL     DSMSKE        MSGKEY
133800970526     C                   END
133900970526     C                   Z-ADD     1             MSGULT
134000021217     C                   WRITE     SIG3DMS                                      SFL MESS
134100021217     C                   WRITE     SIG3DMC                                      CONTR MESS
134200970526     C*
134300970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
134400970526     C                   MOVEL     *BLANKS       DSMSMS
134500970526     C*
134600970526     C                   ENDSR
134700970526     C*------------------------------------------------------------------------*
134800970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
134900970526     C*------------------------------------------------------------------------*
135000970526     C     FUNCON        BEGSR
135100970526     C*
135200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
135300970526     C                   EXSR      FUNVAR
135400970526     C*
135500970526     C* PULISCO CAMPI DI LAVORO
135600970526     C                   Z-ADD     0             I                 5 0
135700970526     C                   Z-ADD     0             W                 5 0
135800970526     C*
135900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
136000970526     C     FUNKEY        IFNE      *BLANKS
136100970526     C     1             DO        24            I
136200970526     C                   MOVEL     FUD(I)        FUDRIG
136300970526     C*
136400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
136500970526     C     FUNKEY        IFEQ      FUDFUN
136600970526     C     FUDATT        ANDEQ     'S'
136700970526     C                   Z-ADD     I             W
136800970526     C                   END
136900970526     C                   END                                                    DO WHILE
137000970526     C*
137100970526     C* FUNZIONE NON ATTIVA (ERRORE)
137200970526     C     W             IFEQ      0
137300970526     C*
137400970526     C* EMISSIONE MESSAGGIO DI ERRORE
137500970526     C                   MOVEL     MSG(2)        DSMSMS
137600970526     C                   END                                                    W=0
137700970526     C                   END                                                    FUNKEY NE *BLANKS
137800970526     C*
137900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
138000970526     C     W             IFNE      0
138100970526     C     FUNKEY        OREQ      *BLANKS
138200970526     C*
138300970526     C     FUNKEY        IFEQ      *BLANKS
138400970526     C                   EXSR      ENTGES
138500970526     C                   END
138600970526     C     FUNKEY        IFEQ      '03'
138700970526     C                   EXSR      F03GES
138800970526     C                   END
138900150428     C     FUNKEY        IFEQ      '04'
139000150428     C                   EXSR      F04GES
139100150428     C                   END
139200970526     C     FUNKEY        IFEQ      '05'
139300970526     C                   EXSR      F05GES
139400970526     C                   END
139500970526     C     FUNKEY        IFEQ      '06'
139600970526     C                   EXSR      F06GES
139700970526     C                   END
139800970526     C     FUNKEY        IFEQ      '12'
139900970526     C                   EXSR      F12GES
140000970526     C                   END
140100970528     C     FUNKEY        IFEQ      '21'
140200970528     C                   EXSR      F21GES
140300970528     C                   END
140400970528     C     FUNKEY        IFEQ      '22'
140500970528     C                   EXSR      F22GES
140600970528     C                   END
140700970526     C     FUNKEY        IFEQ      '23'
140800970526     C                   EXSR      F23GES
140900970526     C                   END
141000970526     C     FUNKEY        IFEQ      '24'
141100970526     C                   EXSR      F24GES
141200970526     C                   END
141300970526     C     FUNKEY        IFEQ      'RU'
141400970526     C                   EXSR      RUPGES
141500970526     C                   END
141600970526     C*
141700970526     C                   END
141800970526     C*
141900970526     C                   ENDSR
142000970526     C*------------------------------------------------------------------------*
142100970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
142200970526     C*------------------------------------------------------------------------*
142300970526     C     FUNVAR        BEGSR
142400970526     C*
142500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
142600970526     C     KEY           IFEQ      ENTER
142700970526     C                   MOVEL     *BLANKS       FUNKEY
142800970526     C                   END
142900970526     C     KEY           IFEQ      F1
143000970526     C                   MOVEL     '01'          FUNKEY
143100970526     C                   END
143200970526     C     KEY           IFEQ      F2
143300970526     C                   MOVEL     '02'          FUNKEY
143400970526     C                   END
143500970526     C     KEY           IFEQ      F3
143600970526     C                   MOVEL     '03'          FUNKEY
143700970526     C                   END
143800970526     C     KEY           IFEQ      F4
143900970526     C                   MOVEL     '04'          FUNKEY
144000970526     C                   END
144100970526     C     KEY           IFEQ      F5
144200970526     C                   MOVEL     '05'          FUNKEY
144300970526     C                   END
144400970526     C     KEY           IFEQ      F6
144500970526     C                   MOVEL     '06'          FUNKEY
144600970526     C                   END
144700970526     C     KEY           IFEQ      F7
144800970526     C                   MOVEL     '07'          FUNKEY
144900970526     C                   END
145000970526     C     KEY           IFEQ      F8
145100970526     C                   MOVEL     '08'          FUNKEY
145200970526     C                   END
145300970526     C     KEY           IFEQ      F9
145400970526     C                   MOVEL     '09'          FUNKEY
145500970526     C                   END
145600970526     C     KEY           IFEQ      F10
145700970526     C                   MOVEL     '10'          FUNKEY
145800970526     C                   END
145900970526     C     KEY           IFEQ      F11
146000970526     C                   MOVEL     '11'          FUNKEY
146100970526     C                   END
146200970526     C     KEY           IFEQ      F12
146300970526     C                   MOVEL     '12'          FUNKEY
146400970526     C                   END
146500970526     C     KEY           IFEQ      F13
146600970526     C                   MOVEL     '13'          FUNKEY
146700970526     C                   END
146800970526     C     KEY           IFEQ      F14
146900970526     C                   MOVEL     '14'          FUNKEY
147000970526     C                   END
147100970526     C     KEY           IFEQ      F15
147200970526     C                   MOVEL     '15'          FUNKEY
147300970526     C                   END
147400970526     C     KEY           IFEQ      F16
147500970526     C                   MOVEL     '16'          FUNKEY
147600970526     C                   END
147700970526     C     KEY           IFEQ      F17
147800970526     C                   MOVEL     '17'          FUNKEY
147900970526     C                   END
148000970526     C     KEY           IFEQ      F18
148100970526     C                   MOVEL     '18'          FUNKEY
148200970526     C                   END
148300970526     C     KEY           IFEQ      F19
148400970526     C                   MOVEL     '19'          FUNKEY
148500970526     C                   END
148600970526     C     KEY           IFEQ      F20
148700970526     C                   MOVEL     '20'          FUNKEY
148800970526     C                   END
148900970526     C     KEY           IFEQ      F21
149000970526     C                   MOVEL     '21'          FUNKEY
149100970526     C                   END
149200970526     C     KEY           IFEQ      F22
149300970526     C                   MOVEL     '22'          FUNKEY
149400970526     C                   END
149500970526     C     KEY           IFEQ      F23
149600970526     C                   MOVEL     '23'          FUNKEY
149700970526     C                   END
149800970526     C     KEY           IFEQ      F24
149900970526     C                   MOVEL     '24'          FUNKEY
150000970526     C                   END
150100970526     C     KEY           IFEQ      ROLLUP
150200970526     C                   MOVEL     'RU'          FUNKEY
150300970526     C                   END
150400970526     C     KEY           IFEQ      HELP
150500970526     C                   MOVEL     'HP'          FUNKEY
150600970526     C                   END
150700970526     C*
150800970526     C                   ENDSR
150900970526     C*------------------------------------------------------------------------*
151000970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
151100970526     C*------------------------------------------------------------------------*
151200970526     C     FUNGES        BEGSR
151300970526     C*
151400970526     C* PULISCO CAMPI DI LAVORO
151500970526     C                   Z-ADD     0             W                 5 0
151600970526     C*
151700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
151800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
151900970526     C     1             DO        24            I
152000970526     C                   MOVEL     FUD(I)        FUDRIG
152100970526     C*
152200970526     C* PULIZIA FUNZIONI PRECEDENTI
152300970526     C                   MOVEL     *BLANKS       FUV(I)
152400970526     C*
152500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
152600970526     C* (SALTA F23 E F24)
152700970526     C     FUDATT        IFEQ      'S'
152800970526     C     FUDFUN        ANDNE     '23'
152900970526     C     FUDFUN        ANDNE     '24'
153000970526     C                   ADD       1             W
153100970526     C                   MOVEL     FUDDES        FUV(W)
153200970526     C                   END
153300970526     C                   END                                                    DO WHILE
153400970526     C*
153500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
153600970526     C* ALTRIMENTI DISATTIVO F24
153700970526     C     1             DO        24            I
153800970526     C                   MOVEL     FUD(I)        FUDRIG
153900970526     C*
154000970526     C     FUDFUN        IFEQ      '24'
154100970526     C     W             IFGT      6
154200970526     C                   MOVEL     'S'           FUDATT
154300970526     C                   ELSE
154400970526     C                   MOVEL     *BLANKS       FUDATT
154500970526     C                   END                                                    W GT 8
154600970526     C                   MOVEL     FUDRIG        FUD(I)
154700970526     C                   END
154800970526     C                   END                                                    DO WHILE
154900970526     C*
155000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
155100970526     C     W             DIV       6             I
155200970526     C                   MVR                     Z
155300970526     C*
155400970526     C     Z             IFNE      0
155500970526     C     I             ADD       1             Z
155600970526     C                   ELSE
155700970526     C                   Z-ADD     I             Z
155800970526     C                   END
155900970526     C*
156000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
156100970526     C                   ADD       1             FUNGRU
156200970526     C     FUNGRU        IFGT      Z
156300970526     C     FUNGRU        ORLT      1
156400970526     C                   Z-ADD     1             FUNGRU
156500970526     C                   END
156600970526     C*
156700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
156800970526     C     FUNGRU        IFEQ      1
156900970526     C                   MOVEL     FUV(1)        FUN001
157000970526     C                   MOVEL     FUV(2)        FUN002
157100970526     C                   MOVEL     FUV(3)        FUN003
157200970526     C                   MOVEL     FUV(4)        FUN004
157300970526     C                   MOVEL     FUV(5)        FUN005
157400970526     C                   MOVEL     FUV(6)        FUN006
157500970526     C                   END
157600970526     C*
157700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
157800970526     C     FUNGRU        IFEQ      2
157900970526     C                   MOVEL     FUV(7)        FUN001
158000970526     C                   MOVEL     FUV(8)        FUN002
158100970526     C                   MOVEL     FUV(9)        FUN003
158200970526     C                   MOVEL     FUV(10)       FUN004
158300970526     C                   MOVEL     FUV(11)       FUN005
158400970526     C                   MOVEL     FUV(12)       FUN006
158500970526     C                   END
158600970526     C*
158700970526     C* TERZO GRUPPO DI 6 FUNZIONI
158800970526     C     FUNGRU        IFEQ      3
158900970526     C                   MOVEL     FUV(13)       FUN001
159000970526     C                   MOVEL     FUV(14)       FUN002
159100970526     C                   MOVEL     FUV(15)       FUN003
159200970526     C                   MOVEL     FUV(16)       FUN004
159300970526     C                   MOVEL     FUV(17)       FUN005
159400970526     C                   MOVEL     FUV(18)       FUN006
159500970526     C                   END
159600970526     C*
159700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
159800970526     C     FUNGRU        IFEQ      4
159900970526     C                   MOVEL     FUV(19)       FUN001
160000970526     C                   MOVEL     FUV(20)       FUN002
160100970526     C                   MOVEL     FUV(21)       FUN003
160200970526     C                   MOVEL     FUV(22)       FUN004
160300970526     C                   MOVEL     *BLANKS       FUN005
160400970526     C                   MOVEL     *BLANKS       FUN006
160500970526     C                   END
160600970526     C*
160700970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
160800970526     C                   MOVEL     *BLANKS       FUN007
160900970526     C                   MOVEL     *BLANKS       FUN008
161000970526     C*
161100970526     C     1             DO        24            I
161200970526     C                   MOVEL     FUD(I)        FUDRIG
161300970526     C     FUDFUN        IFEQ      '23'
161400970526     C     FUDATT        ANDEQ     'S'
161500970526     C                   MOVEL     FUDDES        FUN007
161600970526     C                   END
161700970526     C     FUDFUN        IFEQ      '24'
161800970526     C     FUDATT        ANDEQ     'S'
161900970526     C                   MOVEL     FUDDES        FUN008
162000970526     C                   END
162100970526     C                   END                                                    DO
162200970526     C*
162300970526     C                   ENDSR
162400970526     C*------------------------------------------------------------------------*
162500970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
162600970526     C*------------------------------------------------------------------------*
162700970526     C     OPZGES        BEGSR
162800970526     C*
162900970526     C* PULISCO CAMPI DI LAVORO
163000970526     C                   Z-ADD     0             W                 5 0
163100970526     C*
163200970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
163300970526     C* E CARICO TABELLA OPZIONI ATTIVE
163400991115     C     1             DO        24            I
163500970526     C                   MOVEL     OPD(I)        OPDRIG
163600970526     C*
163700970526     C* PULIZIA OPZIONI PRECEDENTI
163800970526     C                   MOVEL     *BLANKS       OPV(I)
163900970526     C*
164000970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
164100970526     C     OPDATT        IFEQ      'S'
164200970526     C                   ADD       1             W
164300970526     C                   MOVEL     OPDDES        OPV(W)
164400970526     C                   END
164500970526     C                   END                                                    DO WHILE
164600970526     C*
164700970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
164800970526     C* ALTRIMENTI DISATTIVO F23
164900991115     C     1             DO        24            I
165000970526     C                   MOVEL     FUD(I)        FUDRIG
165100970526     C*
165200970526     C     FUDFUN        IFEQ      '23'
165300970526     C     W             IFGT      8
165400970526     C                   MOVEL     'S'           FUDATT
165500970526     C                   ELSE
165600970526     C                   MOVEL     *BLANKS       FUDATT
165700970526     C                   END                                                    W GT 8
165800970526     C                   MOVEL     FUDRIG        FUD(I)
165900970526     C                   END
166000970526     C                   END                                                    DO WHILE
166100970526     C*
166200970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
166300970526     C     W             DIV       8             I
166400970526     C                   MVR                     Z                 5 0
166500970526     C*
166600970526     C     Z             IFNE      0
166700970526     C     I             ADD       1             Z
166800970526     C                   ELSE
166900970526     C                   Z-ADD     I             Z
167000970526     C                   END
167100970526     C*
167200970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
167300970526     C                   ADD       1             OPZGRU
167400970526     C     OPZGRU        IFGT      Z
167500970526     C     OPZGRU        ORLT      1
167600970526     C                   Z-ADD     1             OPZGRU
167700970526     C                   END
167800970526     C*
167900970526     C* PRIMO GRUPPO DI 8 OPZIONI
168000970526     C     OPZGRU        IFEQ      1
168100970526     C                   MOVEL     OPV(1)        OPZ001
168200970526     C                   MOVEL     OPV(2)        OPZ002
168300970526     C                   MOVEL     OPV(3)        OPZ003
168400970526     C                   MOVEL     OPV(4)        OPZ004
168500970526     C                   MOVEL     OPV(5)        OPZ005
168600970526     C                   MOVEL     OPV(6)        OPZ006
168700970526     C                   MOVEL     OPV(7)        OPZ007
168800970526     C                   MOVEL     OPV(8)        OPZ008
168900970526     C                   END
169000970526     C*
169100970526     C* SECONDO GRUPPO DI 8 OPZIONI
169200970526     C     OPZGRU        IFEQ      2
169300970526     C                   MOVEL     OPV(9)        OPZ001
169400970526     C                   MOVEL     OPV(10)       OPZ002
169500970526     C                   MOVEL     OPV(11)       OPZ003
169600970526     C                   MOVEL     OPV(12)       OPZ004
169700970526     C                   MOVEL     OPV(13)       OPZ005
169800970526     C                   MOVEL     OPV(14)       OPZ006
169900970526     C                   MOVEL     OPV(15)       OPZ007
170000970526     C                   MOVEL     OPV(16)       OPZ008
170100970526     C                   END
170200970526     C*
170300970526     C* TERZO GRUPPO DI 8 OPZIONI
170400970526     C     OPZGRU        IFEQ      3
170500970526     C                   MOVEL     OPV(17)       OPZ001
170600970526     C                   MOVEL     OPV(18)       OPZ002
170700970526     C                   MOVEL     OPV(19)       OPZ003
170800970526     C                   MOVEL     OPV(20)       OPZ004
170900970526     C                   MOVEL     OPV(21)       OPZ005
171000970526     C                   MOVEL     OPV(22)       OPZ006
171100970526     C                   MOVEL     OPV(23)       OPZ007
171200970526     C                   MOVEL     OPV(24)       OPZ008
171300970526     C                   END
171400970526     C*
171500970526     C                   ENDSR
171600970526     C*------------------------------------------------------------------------*
171700970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
171800970526     C*------------------------------------------------------------------------*
171900970526     C     OPZCON        BEGSR
172000970526     C*
172100970526     C* PULISCO CAMPI DI LAVORO
172200970526     C                   Z-ADD     0             I                 5 0
172300970526     C                   Z-ADD     0             W                 5 0
172400970526     C*
172500970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
172600970526     C     1             DO        24            I
172700970526     C                   MOVEL     OPD(I)        OPDRIG
172800970526     C*
172900970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
173000970526     C     OPZKEY        IFEQ      OPDOPZ
173100970526     C     OPZKEY        OREQ      OPDOP1
173200970526     C*
173300970526     C     OPDATT        IFEQ      'S'
173400970526     C                   MOVEL     OPDOPZ        OPZKEY
173500970526     C                   Z-ADD     I             W
173600970526     C                   END
173700970526     C                   END
173800970526     C*
173900970526     C                   END                                                    DO WHILE
174000970526     C*
174100970526     C* OPZIONE NON ATTIVA (ERRORE)
174200970526     C     W             IFEQ      0
174300970526     C*
174400970526     C* OPZIONE NON OK
174500970526     C                   Z-ADD     1             OPZOK
174600970526     C*
174700970526     C* EMISSIONE MESSAGGIO DI ERRORE
174800970526     C                   MOVEL     MSG(1)        DSMSMS
174900970526     C                   ELSE
175000970526     C*
175100970526     C* OPZIONE ATTIVA (PROCEDI ...)
175200970526     C*
175300021211     C                   MOVEL     *BLANKS       PMSG
175400150427     C     OPZKEY        IFEQ      ' S'
175500150427     C                   EXSR      O06GES
175600150427     C                   END
175700150427     C     OPZKEY        IFEQ      ' V'
175800150427     C                   EXSR      O07GES
175900970526     C                   END
176000150428     C     OPZKEY        IFEQ      ' P'
176100150427     C                   EXSR      O08GES
176200150427     C                   END
176300021211     C                   MOVEL     PMSG          DSMSMS
176400021211     C                   END                                                    OPZ.NON ATTIVA
176500970526     C*
176600970526     C                   ENDSR
176700970526     C*------------------------------------------------------------------------*
176800970526     C* NOTE COSTRUZIONE SCHIERE
176900970526     C*------------------------------------------------------------------------*
177000970526     C*
177100970526     C* -MSG-MESSAGGI
177200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
177300970526     C* (NON TOCCARE MSG,1 E MSG,2)
177400970526     C*
177500970526     C* -CMD-COMANDI
177600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
177700970526     C*
177800970526     C* -OPD-OPZIONI
177900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
178000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
178100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
178200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
178300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
178400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
178500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
178600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
178700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
178800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
178900970526     C*
179000970526     C* -FUD-FUNZIONI
179100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
179200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
179300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
179400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
179500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
179600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
179700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
179800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
179900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
180000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
180100991108     C*
180200991115     C* -STS-CODICI STATI DEI RECORD DI LOG
180300991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
180400991110     C* ================> IMPORTANTE <=================
180500991115     C* per codici di errore in -STS- indicare in -DES- sempre
180600991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
180700991110     C* ERR
180800991108     C*
180900970526     O*------------------------------------------------------------------------*
181000970526** DEC - DECODIFICHE
181100970526    GESTIONE                                                     1
181200000407 INTERROGAZIONE                                                  2
181300970530     SCELTA                                                      3
1814009705264
1815009705265
181600970526** MSG - MESSAGGI
181700150428YCOAS1R- Opzione non gestita.                                                  1
181800150428YCOAS1R- Tasto funzionale non gestito.                                         2
181900150428YCOAS1R- Non abilitato all inserimento in filiale.                             3
182000150428YCOAS1R- Soggetto movimento non in gestione all'utente. Enter per forzare!     4
182100150428YCOAS1R- Presenti troppi record, alcuni non verranno visualizzati.             5
182200150428YCOAS1R- Errore nella selezione di ABI o CAB                                   6
182300150429YCOAS1R- Opzione non ammessa per il tipo documento scelto                      7
182400150429YCOAS1R- Il range inserito è errato                                            8
182500150428YCOAS1R- 9                                                                     9
182600150428YCOAS1R- 10                                                                   10
182700150428YCOAS1R- 11                                                                   11
182800150428YCOAS1R- 12                                                                   12
182900150428YCOAS1R- 13                                                                   13
183000150428YCOAS1R- 14                                                                   14
183100150428YCOAS1R- 15                                                                   15
183200150428YCOAS1R- 16                                                                   16
183300150428YCOAS1R- 17                                                                   17
183400150428YCOAS1R- 18                                                                   18
183500150428YCOAS1R- 19                                                                   19
183600150428       - 20                                                                   20
183700970528** CMD - COMANDI CL
183800021211
183900021211
184000991110
184100970528
184200970528
184300970526** OPD - OPZIONI
184400000420 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
184500000407 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
184600021211 3=Copia           -3 -     (S/N)=N                        opz. 03¬
184700000407 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
184800000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
184900150424 S=Spedizione      -S -     (S/N)=N                        opz. 06¬
185000150424 V=Versamento      -V -     (S/N)=N                        opz. 07¬
185100150424 P=Partita         -P -     (S/N)=N                        opz. 08¬
185200991110                   -  -     (S/N)=N                        opz. 09¬
185300991110                   -  -     (S/N)=N                        opz. 10¬
185400000407                   -  -     (S/N)=N                        opz. 11¬
185500970526                   -  -     (S/N)=N                        opz. 12¬
185600970526                   -  -     (S/N)=N                        opz. 13¬
185700000407                   -  -     (S/N)=N                        opz. 14¬
185800000407                   -  -     (S/N)=N                        opz. 15¬
185900970526                   -  -     (S/N)=N                        opz. 16¬
186000970526                   -  -     (S/N)=N                        opz. 17¬
186100970526                   -  -     (S/N)=N                        opz. 18¬
186200970526                   -  -     (S/N)=N                        opz. 19¬
186300970526                   -  -     (S/N)=N                        opz. 20¬
186400970526                   -  -     (S/N)=N                        opz. 21¬
186500970526                   -  -     (S/N)=N                        opz. 22¬
186600970526                   -  -     (S/N)=N                        opz. 23¬
186700000407                   -  -     (S/N)=N                        opz. 25¬
186800970526** FUD - FUNZIONI
186900970526                   -        (S/N)=N                        funz.01¬
187000970526                   -        (S/N)=N                        funz.02¬
187100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
187200150428F04=Ricerca        -        (S/N)=S  <== S-FISSO           funz.04¬
187300970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
187400101006F06=Inserimento    -        (S/N)=N  <==                   funz.06¬
187500970526                   -        (S/N)=N                        funz.07¬
187600970527                   -        (S/N)=N                        funz.08¬
187700030325F09=Altri dati     -        (S/N)=S  <== S-FISSO           funz.09¬
187800970527                   -        (S/N)=N                        funz.10¬
187900970526                   -        (S/N)=N                        funz.11¬
188000970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
188100991116                   -        (S/N)=N                        funz.13¬
188200970526                   -        (S/N)=N                        funz.14¬
188300970526                   -        (S/N)=N                        funz.15¬
188400970526                   -        (S/N)=N                        funz.16¬
188500970526                   -        (S/N)=N                        funz.17¬
188600970526                   -        (S/N)=N                        funz.18¬
188700970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
188800970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
188900000407                   -        (S/N)=N                        funz.21¬
189000991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
189100991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
189200991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
