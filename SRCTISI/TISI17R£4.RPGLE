000100020418     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020418      *------------------------------------------------------------------------*
000300020418      * CAPPARIO - GESTIONE TERMINAL - SCELTA
000400020418      *------------------------------------------------------------------------*
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970528     FTISI17D   CF   E             WORKSTN INFDS(DEVDS1)
000900970528     F                                     SFILE(SI17D05:SFLULT)
001000060929     F                                     SFILE(SI17s11:nrr)
001100970528     F                                     SFILE(SI17DMS:MSGULT)
001200970528     FAZORG01L  IF   E           K DISK
001300970526     F*------------
001400970528     F* ECCEZIONI TERMINAL
001500970526     F*------------
001600970618     FAZCAE01L  IF   E           K DISK
001700970618     FAZCAE02L  IF   E           K DISK
001800970618     F                                     RENAME(AZCAE000:AZCAE2)
001900980107     FAZCAE05L  IF   E           K DISK
002000980107     F                                     RENAME(AZCAE000:AZCAE5)
002100970526     D*------------------------------------------------------------------------*
002200970526     D* SCHIERE
002300970526     D*------------------------------------------------------------------------*
002400970526     D*------------
002500970526     D* DECODIFICHE
002600970526     D*------------
002700970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* MESSAGGI
003000970526     D*------------
003100970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003200970528     D*------------
003300970528     D* COMANDI
003400970528     D*------------
003500970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* DESCRIZIONI OPZIONI
003800970526     D*------------
003900970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
004000970526     D*------------
004100970526     D* DESCRIZIONI TASTI FUNZIONALI
004200970526     D*------------
004300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004400970526     D*------------
004500970526     D* OPZIONI DA CARICARE A VIDEO
004600970526     D*------------
004700970526     D OPV             S             19    DIM(24)
004800970526     D*------------
004900970526     D* FUNZIONI DA CARICARE A VIDEO
005000970526     D*------------
005100970527     D FUV             S             19    DIM(24)
005200970528     D*------------
005300970528     D* ORGANIGRAMMA
005400970528     D*------------
005500971118     D ORG             S              3  0 DIM(999)                             *CODICE
005600971118     D DORG            S             30    DIM(999)                             *DESCRIZIONE
005700970526     D*------------------------------------------------------------------------*
005800970526     D* INPUT
005900970526     D*------------------------------------------------------------------------*
006000970526     D*------------
006100970526     D* RIDEFINIZIONE OPZIONI VIDEO
006200970526     D*------------
006300970526     D OPDDS           DS
006400970526     D  OPDOPZ                 1      2
006500970526     D  OPDDE1                 3     19
006600970526     D  OPDOP1                21     22
006700970526     D  OPDFI0                23     34
006800970526     D  OPDATT                35     35
006900970526     D  OPDFI1                36     80
007000970526     D  OPDDES                 1     19
007100970526     D  OPDRIG                 1     80
007200970526     D*------------
007300970526     D* RIDEFINIZIONE FUNZIONI VIDEO
007400970526     D*------------
007500970526     D FUDDS           DS
007600970526     D  FUDDE1                 1      1
007700970526     D  FUDFUN                 2      3
007800970526     D  FUDDE2                 4     19
007900970526     D  FUDFI0                20     34
008000970526     D  FUDATT                35     35
008100970526     D  FUDFI1                36     80
008200970526     D  FUDDES                 1     19
008300970526     D  FUDRIG                 1     80
008400970526     D*------------
008500970526     D* DS GESTIONE TASTI FUNZIONALI
008600970526     D*------------
008700970526     D DSKEY         E DS
008800970526     D DEVDS1          DS
008900970526     D  KEY                  369    369
009000970526     D*------------
009100970526     D* DS "XMSG" - GESTIONE MESSAGGI
009200970526     D*------------
009300970526     D DSMSG         E DS
009400970527     D*------------------
009500970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
009600970527     D*------------------
009700970527     D WLBDA8          DS                  INZ
009800970527     D  G08DAT                 1      8  0
009900970527     D  G08INV                 9     16  0
010000970527     D  G08ERR                17     17
010100970527     D  G08TGI                18     22  0
010200970526     D*------------
010300970526     D* DS DI PROCEDURA
010400970526     D*------------
010500970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
010600970526     D*------------
010700970526     D* ARCHITETTURA
010800970526     D*------------
010900970526     D KPJBA         E DS
011000120904     D*
011100060929     D SAVING          S                   LIKE(VIDING)
011200120921     D wSflUlt         S                   LIKE(sflult)
011300060929     D SAVepa          S                   LIKE(videpa)
011400120904     D Rip18R1         S              2
011500120904     D Err18R1         S              1
011600120904     D S11NrRcd        S              5  0
011700120904     D DDeV11          S                   like(V11DDe)
011800120904     D NwTFE           S                   like(CAETFE)
011900120921     D ForzaMsg17      S               n
012000120921     D Dsc11Vid_sav    S              8s 0
012100970526     C*------------------------------------------------------------------------*
012200970526     C* MAIN LINES
012300970526     C*------------------------------------------------------------------------*
012400970526     C*
012500970526     C* SE NON CI SONO ERRORI, PROSEGUE
012600970526IF  1C     ERRL00        IFNE      '1'
012700970526     C*
012800970526     C* CARICA IL SFILE
012900970526     C                   EXSR      POSSFL
013000970526     C*
013100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013200970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013500970526     C*
013600970526     C* REIMPOSTA ERRORE DI PROCERURA
013700970526     C                   MOVEL     '0'           ERRL00
013800970526     C*
013900970526     C* EMETTE IL SFILE
014000970526     C                   EXSR      OUTSFL
014100970526     C*
014200970526     C* GESTIONE TASTI FUNZIONALI
014300970526     C                   EXSR      FUNCON
014400970526     C*
014500970526E   2C                   ENDDO
014600970527     C*
014700970527     C* CI SONO ERRORI IN ENTRATA
014800970527X   1C                   ELSE
014900970527     C                   MOVEL     '1'           D00ERR
015000970527     C                   MOVEL     DSMSMS        D00MSG
015100970526E   1C                   ENDIF
015200970526     C* FINE PGM
015300970526     C                   SETON                                        LR
015400970526     C*------------------------------------------------------------------------*
015500970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
015600970526     C*------------------------------------------------------------------------*
015700970526     C     F03GES        BEGSR
015800970526     C*
015900970526     C* IMPOSTA "PREMUTO F03"
016000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016100970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
016200970526     C*
016300970526     C                   ENDSR
016400970526     C*------------------------------------------------------------------------*
016500970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
016600970526     C*------------------------------------------------------------------------*
016700970526     C     F05GES        BEGSR
016800970526     C*
016900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017000970526     C*
017100970526     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
017400970526     C*------------------------------------------------------------------------*
017500970526     C     F06GES        BEGSR
017600970526     C*
017700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
017800970602     C                   MOVEL     *BLANKS       DEPA
017900970602     C                   Z-ADD     *ZEROS        DETP
018000970602     C                   Z-ADD     *ZEROS        DETA
018100970602     C                   Z-ADD     *ZEROS        DEDD
018200970602     C                   MOVEL     DEPOP0        WOP0
018300970529     C                   MOVEL     'F06'         WOP1
018400970528     C                   MOVEL     'TISI18R'     WPGM              8
018500970526     C                   EXSR      LANPGM                                       *LANCIA PGM
018600970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018700970526E   1C                   ENDIF
018800970526     C*
018900970526     C                   ENDSR
019000970526     C*------------------------------------------------------------------------*
019100970526     C* F12GES - GESTIONE F12 -> RITORNO
019200970526     C*------------------------------------------------------------------------*
019300970526     C     F12GES        BEGSR
019400970526     C*
019500970526     C* IMPOSTA "PREMUTO F12"
019600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019700970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
019800970526     C*
019900970526     C                   ENDSR
020000970528     C*------------------------------------------------------------------------*
020100970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
020200970528     C*------------------------------------------------------------------------*
020300970528     C     F21GES        BEGSR
020400970612     C*
020500980206     C                   MOVEL     *BLANKS       DEPA
020600980206     C                   Z-ADD     *ZEROS        DETP
020700980206     C                   Z-ADD     *ZEROS        DETA
020800980206     C                   Z-ADD     *ZEROS        DEDD
020900970612     C                   MOVEL     DEPOP0        WOP0
021000970612     C                   MOVE      '03'          WOP0
021100970612     C                   MOVEL     'F21'         WOP1
021200970612     C                   MOVEL     'TISI19R'     WPGM              8
021300970612     C                   EXSR      LANPGM
021400970528     C*
021500970528     C                   ENDSR
021600970528     C*------------------------------------------------------------------------*
021700970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
021800970528     C*------------------------------------------------------------------------*
021900970528     C     F22GES        BEGSR
022000970528     C*
022100970528     C                   Z-ADD     80            LENGH            15 5
022200970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
022300970528     C                   PARM                    CMD(1)
022400970528     C                   PARM                    LENGH
022500970528     C*
022600970528     C                   ENDSR
022700970526     C*------------------------------------------------------------------------*
022800970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
022900970526     C*------------------------------------------------------------------------*
023000970526     C     F23GES        BEGSR
023100970526     C*
023200970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
023300970526     C*
023400970526     C                   ENDSR
023500970526     C*------------------------------------------------------------------------*
023600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023700970526     C*------------------------------------------------------------------------*
023800970526     C     F24GES        BEGSR
023900970526     C*
024000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024100970526     C*
024200970526     C                   ENDSR
024300970526     C*------------------------------------------------------------------------*
024400970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
024500970526     C*------------------------------------------------------------------------*
024600970526     C     RUPGES        BEGSR
024700970526     C*
024800970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
024900970526     C*
025000970526     C                   ENDSR
025100970526     C*------------------------------------------------------------------------*
025200970526     C* ENTGES - GESTIONE ENTER -> INVIO
025300970526     C*------------------------------------------------------------------------*
025400970526     C     ENTGES        BEGSR
025500970526     C*
025600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025700970526     C                   EXSR      NEWPOS
025800970526     C*
025900970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026100970526IF  2C     SWPOSI        IFNE      0
026200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
026300970526X   2C                   ELSE
026400970526IF  3C     SFLULT        IFNE      0
026500970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026600970526     C*
026700970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026800970526IF  4C     SWMODI        IFNE      0
026900970526     C                   EXSR      POSSFL
027000970526E   4C                   ENDIF
027100970526E   3C                   ENDIF
027200970526E   2C                   ENDIF
027300970526E   1C                   ENDIF
027400970526     C*
027500970526     C                   ENDSR
027600970526     C*------------------------------------------------------------------------*
027700970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
027800970526     C*------------------------------------------------------------------------*
027900970526     C     O01GES        BEGSR
028000970526     C*
028100970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
028200970528     C                   MOVEL     VIDEPA        D00EPA
028300970602     C                   Z-ADD     VIDTFP        D00ETP
028400970528     C                   Z-ADD     VIDTFA        D00ETA
028500970528     C                   Z-ADD     VIHDDE        D00EDD
028600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
028700970526     C*
028800970526     C                   ENDSR
028900970526     C*------------------------------------------------------------------------*
029000970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
029100970526     C*------------------------------------------------------------------------*
029200970526     C     O02GES        BEGSR
029300970526     C*
029400970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
029500970526IF  1C     VIDATB        IFEQ      'A'
029600970526     C                   MOVEL     '1'           ERRL00
029700970526     C                   MOVEL     MSG(4)        DSMSMS
029800970526E   1C                   ENDIF
029900970527     C* MODIFICA
030000970526IF  1C     ERRL00        IFEQ      '0'
030100970602     C                   MOVEL     VIDEPA        DEPA
030200970602     C                   Z-ADD     VIDTFP        DETP
030300970602     C                   Z-ADD     VIDTFA        DETA
030400970602     C                   Z-ADD     VIHDDE        DEDD
030500970602     C                   MOVEL     DEPOP0        WOP0
030600970526     C                   MOVEL     'O02'         WOP1
030700970528     C                   MOVEL     'TISI18R'     WPGM
030800970526     C                   EXSR      LANPGM
030900970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
031000970526E   1C                   ENDIF
031100970526     C*
031200970526     C                   ENDSR
031300970526     C*------------------------------------------------------------------------*
031400970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
031500970526     C*------------------------------------------------------------------------*
031600970526     C     O04GES        BEGSR
031700970526     C*
031800970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
031900970526IF  1C     VIDATB        IFEQ      'A'
032000970526     C                   MOVEL     '1'           ERRL00
032100970526     C                   MOVEL     MSG(4)        DSMSMS
032200970526E   1C                   ENDIF
032300970527     C* ANNULLAMENTO
032400970526IF  1C     ERRL00        IFEQ      '0'
032500970602     C                   MOVEL     VIDEPA        DEPA
032600970602     C                   Z-ADD     VIDTFP        DETP
032700970602     C                   Z-ADD     VIDTFA        DETA
032800970602     C                   Z-ADD     VIHDDE        DEDD
032900970602     C                   MOVEL     DEPOP0        WOP0
033000970526     C                   MOVEL     'O04'         WOP1
033100970528     C                   MOVEL     'TISI18R'     WPGM
033200970526     C                   EXSR      LANPGM
033300970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
033400970526E   1C                   ENDIF
033500970526     C*
033600970526     C                   ENDSR
033700970526     C*------------------------------------------------------------------------*
033800970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
033900970526     C*------------------------------------------------------------------------*
034000970526     C     O05GES        BEGSR
034100970526     C*
034200970602     C                   MOVEL     VIDEPA        DEPA
034300970602     C                   Z-ADD     VIDTFP        DETP
034400970602     C                   Z-ADD     VIDTFA        DETA
034500970602     C                   Z-ADD     VIHDDE        DEDD
034600970602     C                   MOVEL     DEPOP0        WOP0
034700970526     C                   MOVEL     'O05'         WOP1
034800970528     C                   MOVEL     'TISI18R'     WPGM
034900970526     C                   EXSR      LANPGM
035000970526     C*
035100970526     C                   ENDSR
035200970526     C*------------------------------------------------------------------------*
035300970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
035400970526     C*------------------------------------------------------------------------*
035500970526     C     O07GES        BEGSR
035600970526     C*
035700970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
035800970526IF  1C     VIDATB        IFNE      'A'
035900970526     C                   MOVEL     '1'           ERRL00
036000970526     C                   MOVEL     MSG(5)        DSMSMS
036100970526E   1C                   ENDIF
036200970526     C* RIPRISITINO
036300970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036400970602     C                   MOVEL     VIDEPA        DEPA
036500970602     C                   Z-ADD     VIDTFP        DETP
036600970602     C                   Z-ADD     VIDTFA        DETA
036700970602     C                   Z-ADD     VIHDDE        DEDD
036800970602     C                   MOVEL     DEPOP0        WOP0
036900970526     C                   MOVEL     'O07'         WOP1
037000970528     C                   MOVEL     'TISI18R'     WPGM
037100970526     C                   EXSR      LANPGM
037200970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
037300970526E   1C                   ENDIF
037400970526     C*
037500970526     C                   ENDSR
037600060804     C*------------------------------------------------------------------------*
037700060804     C* OSCGES - GESTIONE OPZIONE SC= scadenza terminal
037800060804     C*------------------------------------------------------------------------*
037900060804     C     OSCGES        BEGSR
038000120912     c* Per poter dare l'opzione SC si deve avere la selezione per data decorrenza
038100120913     C* in modo che sia più difficile (non impossibile) che l'utente crei delle sovrapposizioni
038200120912     c***                if        vidppa=' ' or  vidpdd=0
038300120912     C                   IF        VIDPDD=0
038400120912     C                   MOVEL     '1'           ERRL00
038500120912     C                   MOVEL     MSG(8)        DSMSMS
038600120913     C* segnalo al resto del pgm che l'opzione ha dato un errore
038700120913     c                   movel     '1'           opzok
038800120913     C* esco dalla routine
038900120913     C                   LEAVESR
039000120913     c***                goto      endscade
039100120912     c                   ENDIF
039200060929     c
039300120921     C                   EVAL      ForzaMsg17 = *off
039400120921     C                   EVAL      Dsc11Vid_sav = 0
039500060928     c* Leggo dal sfl tutti i record che hanno "SC" e devono avere
039600060928     c*  lo stesso tipo terminal
039700060929     c                   eval      savING=viding
039800060929     c                   movel     dec(4)        viding
039900060929     c
040000060929     c                   clear                   nrr               5 0
040100060929     c                   clear                   savepa
040200060929     c* pulisco il sfl di scadenza terminal
040300060928     c                   seton                                        70
040400060929     c                   write     si17c11
040500060928     c                   setoff                                       70
040600120913     C*
040700120921     c                   do        wflult        wSflUlt
040800120921     c     wSflUlt       chain     si17d05
040900060928     c                   if        opzkey='SC'
041000120903     c* La prima volta salvo alcuni dati del record
041100120903     c                   if        savepa=*blanks
041200120903     c                   eval      savepa=videpa
041300120903     c                   eval      v11epa=videpa
041400120903E   1C                   ENDIF
041500120903     c* Do errore se:
041600120921     c* - se il rcd è annullato logicamente
041700120921     c                   IF        VidATB <> *blank
041800120904     C                   MOVEL     '1'           ERRL00
041900120921     C                   MOVEL     MSG(4)        DSMSMS
042000120912     c                   leave
042100120904E   1C                   ENDIF
042200120921     c* - il tipo terminal è <> da 'A' e 'P'
042300120921     c                   IF        (VidEPA <> 'A' and
042400120921     c                              VidEPA <> 'P')
042500120921     C                   MOVEL     '1'           ERRL00
042600120921     C                   MOVEL     MSG(7)        DSMSMS
042700120921     c                   leave
042800120921E   1C                   ENDIF
042900120903     c* - il tipo terminal è <> dal precedente (deve essere 1 solo tipo)
043000120904     c                   IF        savepa<>VidEPA
043100120904     C                   MOVEL     '1'           ERRL00
043200120904     C                   MOVEL     MSG(11)       DSMSMS
043300120912     c                   leave
043400120904E   1C                   ENDIF
043500120903     c* - se il rcd è scaduto
043600120904     c                   IF        %date(VidDSC:*eur) < %date()
043700060928     C                   MOVEL     '1'           ERRL00
043800120904     C                   MOVEL     MSG(13)       DSMSMS
043900120912     c                   leave
044000060928E   1C                   ENDIF
044100120921     c* - se il rcd non è l'ultimo tra quelli di pari EPA/TFE/TFA/TFP
044200120913     C*   in questo modo cerco di evitare sovrapposizioni
044300120913     C                   MOVEL     VIDEPA        CAEEPA
044400120924     C***                Z-ADD     VIDTFE        CAETFE
044500120921     C                   Z-ADD     VIDTFA        CAETFA
044600120924     C                   Z-ADD     VIDTFP        CAETFP
044700120921     C* mi posiziono a quello letto sul sfl impostando la sua data decorrenza
044800120913     c                   EVAL      CAEDDE=%dec(%date(VidDDe:*eur):*iso)
044900120924     C     K04CAE01      CHAIN     AZCAE01L
045000120924     C                   DOU       %eof(AZCAE01L)
045100120924     C     K03CAE01      READE     AZCAE01L
045200120921     C* se leggo un rcd con EPA/TFE/TFA/TFE vuol dire solo che ha una decorrenza > , quindi non è
045300120913     C* l'ultimo
045400120921     C* non considero gli annullati
045500120921     c                   IF        not %eof and
045600120921     C                             CAEATB = *blank
045700120913     C                   MOVEL     '1'           ERRL00
045800120913     C                   MOVEL     MSG(15)       DSMSMS
045900120913     c                   leave
046000120921     C                   ENDIF
046100120921     C                   ENDDO
046200120924     c* ogni volta che ho un errore esco dal ciclo principale, quindi se dal ciclo precedente
046300120924     C* sono uscito con errore, devo uscire anche dal ciclo principale
046400120924     C                   IF        ERRL00 = '1'
046500120924     c                   leave
046600120924E   1C                   ENDIF
046700060929     c
046800120904     c* se sono qui NON ho errori pre emissione
046900120913     C* per cui scrivo un rcd del sfl dei terminal da far scadere
047000120904     c                   movel     vidtfp        v11tfp                         filiale partenza
047100060929     c                   movel     viddtp        v11dtp
047200120904     c                   movel     vidtfa        v11tfa                         filiale arrivo
047300060929     c                   movel     viddta        v11dta
047400120904     c                   movel     vidtfe        v11tfe                         filiale eccezione
047500060929     c                   movel     viddte        v11dte
047600120912     c                   EVAL      V11DDe = VidDDe                              data decorrenza H
047700120921     c                   EVAL      V11SDSc = VidDSc                             data scadenza
047800060929     c                   add       1             nrr
047900060929     c                   write     si17s11
048000060929     c                   endif
048100060929     c                   enddo
048200060928     c*
048300120912     c* se non ho avuto errori durante il caricamento del sfl
048400120912     C                   IF        ERRL00 <> '1'
048500120904     c* salvo il nr.rcd di questo sfl
048600120904     C                   EVAL      s11nrrcd = nrr
048700120904     c*
048800060929     c                   z-add     1             rec1
048900060929     c                   eval      v11nwrip='SI'
049000060929     c                   clear                   v11dsc
049100060929     c                   clear                   v11nwdsc
049200060929     c                   clear                   v11nwtfe
049300120912     c                   clear                   v11nwdte
049400060929     c
049500120904     c* se non ci sono errori o non ho premuto F12, ciclo
049600120904     c                   DOU       *inkf = *on or *inkl = *on
049700120904     c                   write     si17d01
049800120904     c                   write     si17z11
049900060929     c                   exfmt     si17c11
050000060929     c                   setoff                                       2890
050100060929     c                   setoff                                       646566
050200060929     C                   MOVEL     '0'           ERRL00
050300060929     c* F12 - ritorno
050400060929     c                   if        *inkl
050500120912     c***                goto      endscade
050600120912     c                   LEAVE
050700060929     c                   endif
050800060929     c* controllo le selezioni fatte
050900060929     c                   exsr      CTRSCADE
051000060929     c
051100120904     c* se non ci sono errori e ho premuto F6
051200120904     c                   IF        ErrL00 <> *on and
051300120904     c                             *inkf
051400120904     c* leggo il sfl ed effettuo le modifiche
051500120904     c                   EXSR      GenNewScad
051600060929     c                   endif
051700120904     c                   ENDDO
051800060928     c
051900120913     C* se ho avuto errori durante il caricamento del sfl
052000120913     C                   ELSE
052100120913     C* riciclo per riaccendere il sflnxtchg nelle righe già lette avento un'opzione SC
052200120921     c                   DO        RigaLetta     wSflUlt
052300120921     c     wSflUlt       chain     si17d05
052400120913     c                   if        opzkey='SC'
052500120913     C* riaccendo il sflnxtchg
052600120913     C                   SETON                                        41
052700120913     C                   UPDATE    SI17D05
052800120913E   1C                   ENDIF
052900120913E   1C                   ENDDO
053000120913     C* rileggo la riga che ha dato errore perché serve letta al resto del pgm
053100120913     c     RigaLetta     chain     si17d05
053200120913     C*
053300120912     C                   ENDIF
053400120912     c
053500120913     c***  endscade      tag
053600060929     c
053700120912     c                   if        dsmsms=*blank
053800060929     c                   movel     saving        viding
053900120921     c***                z-add     savult        sflult
054000060929     c                   movel     '1'           swmodi
054100120912     c                   else
054200120912     c                   movel     '1'           opzok
054300120912     c                   endif
054400120904     C*
054500120904     C                   ENDSR
054600120904     C*------------------------------------------------------------------------*
054700120904     C* CONTROLLO DATI PER SCADENZA TERMINAL
054800120904     C*------------------------------------------------------------------------*
054900120904     C     CTRSCADE      BEGSR
055000120904     C*
055100120904     C* Ctrl formale nuova data scadenza x tutte le linee
055200120904     C     *LIKE         DEFINE    VIDDSC        DSC11VID
055300060929IF  1C     ERRL00        IFEQ      '0'
055400120904     C                   Z-ADD     V11DSC        WDA8
055500060929     C                   Z-ADD     *ZEROS        DA8W
055600060929     C                   EXSR      CHKDA8
055700120904     C                   Z-ADD     WDA8          V11DSC
055800120904     C                   Z-ADD     DA8W          DSC11VID
055900060929IF  2C     ERRL00        IFEQ      '1'
056000060929     C                   SETON                                        6428
056100060929     C                   MOVEL     MSG(12)       vidmsg
056200120904     c                   LEAVESR
056300120921     C                   ELSE
056400120921     C* memorizzo la nuova data scadenza se diversa dall'ultima scritta
056500120921     C                   IF        Dsc11Vid <> Dsc11Vid_sav
056600120921     C                   EVAL      Dsc11Vid_sav = Dsc11Vid
056700120921     C                   EVAL      ForzaMsg17 = *off
056800120921     C                   ENDIF
056900120921     C* se data corretta formalmente, deve essere > o = a oggi ma ctrl forzabile
057000120921IF  3C                   IF        %date(V11DSC:*eur) < %date()
057100120921     C                             and ForzaMsg17 = *off
057200120921     C                   EVAL      ForzaMsg17 = *on
057300120921     C                   SETON                                        6428
057400120921     C                   MOVEL     MSG(17)       vidmsg
057500120921E   3C                   ENDIF
057600060929E   2C                   ENDIF
057700060929E   1C                   ENDIF
057800060929     c
057900120904     c* Ctrl formale data scadenza nuova associazione
058000120904     C     *LIKE         DEFINE    V11nwDSC      DSC11NwVID
058100060929IF  1C     ERRL00        IFEQ      '0'
058200060929     C                   Z-ADD     V11nwDSC      WDA8
058300060929     C                   Z-ADD     *ZEROS        DA8W
058400060929     C                   EXSR      CHKDA8
058500060929     C                   Z-ADD     WDA8          V11nwDSC
058600120904     C                   Z-ADD     DA8W          DSC11NwVID
058700060929IF  2C     ERRL00        IFEQ      '1'
058800060929     C                   SETON                                        6528
058900060929     C                   MOVEL     MSG(12)       vidmsg
059000120904     c                   LEAVESR
059100120921E   2C                   ENDIF
059200060929E   1C                   ENDIF
059300060929     c
059400120912     C* La data scadenza nuovo terminal deve essere > scadenza terminal precedente
059500120904    1c                   if        dsc11nwvid<=dsc11vid
059600120913     C                   SETON                                        6528
059700060929     C                   MOVEL     MSG(9)        vidmsg
059800120912     C* precedente msg, ora sostituito
059900120912     C* TISI17R- La nuova data decorrenza deve essere maggiore della prima             9
060000120904     C                   MOVEL     '1'           ERRL00
060100120904     c                   LEAVESR
060200120904E   1C                   ELSE
060300120904     c
060400120904     c* se richiesto il ripristino precedente associazione
060500120904     c* la data scadenza nuova associazione non deve essere > nuova data scadenza di più di 1 mese
060600120904     c* (facciamo 31 gg e non 1 mese variabile) altrimenti non è una cosa temporanea
060700120904     C                   IF        V11NwRip = 'SI'              and
060800120904     C                             %dec(%date(dsc11nwvid):*jul) -
060900120904     C                             %dec(%date(dsc11vid):*jul)   >
061000120904     C                             31
061100120921     C                   SETON                                          6528
061200120904     C                   MOVEL     MSG(14)       vidmsg
061300120904     C                   MOVEL     '1'           ERRL00
061400120904     c                   LEAVESR
061500120904E   1C                   ENDIF
061600120904E   1C                   ENDIF
061700060929     C*--------------------
061800060929     C* TERMINAL / LINEA ECCEZIONE
061900060929     C*--------------------
062000060929IF  1C     ERRL00        IFEQ      '0'
062100060929     C                   MOVEL     V11nwTFE      WLIN              3
062200060929     C                   MOVEL     *BLANKS       WDLIN
062300060929     C                   EXSR      CHKLIN
062400060929     C                   MOVEL     WLIN          V11nwTFE
062500060929     C                   MOVEL     WDLIN         V11nwdtE
062600060929IF  2C     ERRL00        IFEQ      '1'
062700060929     C                   SETON                                        6628
062800060929     C                   MOVEL     MSG(10)       vidmsg
062900120904     c                   LEAVESR
063000060929E   2C                   ENDIF
063100060929E   1C                   ENDIF
063200120913     C*
063300120913     C* se siamo qui, i singoli campi del CTL sono corretto. Adesso controllo i rcd del SFL
063400120913     C* con i dati del CTL
063500120921     C                   DO        s11nrrcd      wSflUlt
063600120921     C     wSflUlt       chain     si17s11
063700120913     C*
063800120913     C* controllo la nuova data scadenza (la prima delle 2) sia  < = di quella presente sul SFL
063900120921     C                   IF        DSc11VID > %dec(%date(V11SDSc:*eur):*iso)
064000120913     C                   SETON                                        6428
064100120913     C                   MOVEL     MSG(16)       vidmsg
064200120913     c                   LEAVESR
064300120913E   2C                   ENDIF
064400120913     C*
064500120913E   1C                   ENDDO
064600060929     c
064700060929     C                   ENDSR
064800120913     C*------------------------------------------------------------------------*
064900120913     C* OSCGES_old - GESTIONE OPZIONE SC= scadenza terminal
065000120913     C*------------------------------------------------------------------------*
065100120913     C     OSCGES_O      BEGSR
065200120913     c* Per poter dare l'opzione SC si deve avere la selezione per data decorrenza
065300120913     C* in modo che sia più difficile (non impossibile) che l'utente crei delle sovrapposizioni
065400120913     c***                if        vidppa=' ' or  vidpdd=0
065500120913     C                   IF        VIDPDD=0
065600120913     C                   MOVEL     '1'           ERRL00
065700120913     C                   MOVEL     MSG(8)        DSMSMS
065800120913     c                   goto      endscade
065900120913     c                   ENDIF
066000120913     c
066100120913     c* Leggo dal sfl tutti i record che hanno "SC" e devono avere
066200120913     c*  lo stesso tipo terminal
066300120913     c                   eval      savING=viding
066400120913     c                   movel     dec(4)        viding
066500120913     c
066600120913     c                   z-add     1             sflult
066700120913     c                   clear                   nrr               5 0
066800120913     c                   clear                   savepa
066900120913     c* pulisco il sfl di scadenza terminal
067000120913     c                   seton                                        70
067100120913     c                   write     si17c11
067200120913     c                   setoff                                       70
067300120913     c
067400120913     c                   do        wflult        sflult
067500120913     c     sflult        chain     si17d05
067600120913     c                   if        opzkey='SC'
067700120913     c* La prima volta salvo alcuni dati del record
067800120913     c                   if        savepa=*blanks
067900120913     c                   eval      savepa=videpa
068000120913     c                   eval      v11epa=videpa
068100120913E   1C                   ENDIF
068200120913     c* Do errore se:
068300120913     c* - il tipo terminal è <> da 'A' e 'P'
068400120913     c                   IF        (VidEPA <> 'A' and
068500120913     c                              VidEPA <> 'P')
068600120913     C                   MOVEL     '1'           ERRL00
068700120913     C                   MOVEL     MSG(7)        DSMSMS
068800120913     c                   leave
068900120913E   1C                   ENDIF
069000120913     c* - il tipo terminal è <> dal precedente (deve essere 1 solo tipo)
069100120913     c                   IF        savepa<>VidEPA
069200120913     C                   MOVEL     '1'           ERRL00
069300120913     C                   MOVEL     MSG(11)       DSMSMS
069400120913     c                   leave
069500120913E   1C                   ENDIF
069600120913     c* - se il rcd è scaduto
069700120913     c                   IF        %date(VidDSC:*eur) < %date()
069800120913     C                   MOVEL     '1'           ERRL00
069900120913     C                   MOVEL     MSG(13)       DSMSMS
070000120913     c                   leave
070100120913E   1C                   ENDIF
070200120913     c
070300120913     c* se sono qui NON ho errori pre emissione
070400120913     c                   movel     vidtfp        v11tfp                         filiale partenza
070500120913     c                   movel     viddtp        v11dtp
070600120913     c                   movel     vidtfa        v11tfa                         filiale arrivo
070700120913     c                   movel     viddta        v11dta
070800120913     c                   movel     vidtfe        v11tfe                         filiale eccezione
070900120913     c                   movel     viddte        v11dte
071000120913     c                   EVAL      V11DDe = VidDDe                              data decorrenza H
071100120913     c                   EVAL      V11DSc = VidDSc                              data scadenza
071200120913     c                   add       1             nrr
071300120913     c                   write     si17s11
071400120913     c                   endif
071500120913     c                   enddo
071600120913     c*
071700120913     c* se non ho avuto errori durante il caricamento del sfl
071800120913     C                   IF        ERRL00 <> '1'
071900120913     c* salvo il nr.rcd di questo sfl
072000120913     C                   EVAL      s11nrrcd = nrr
072100120913     c*
072200120913     c                   z-add     1             rec1
072300120913     c                   eval      v11nwrip='SI'
072400120913     c                   clear                   v11dsc
072500120913     c                   clear                   v11nwdsc
072600120913     c                   clear                   v11nwtfe
072700120913     c                   clear                   v11nwdte
072800120913     c
072900120913     c* se non ci sono errori o non ho premuto F12, ciclo
073000120913     c                   DOU       *inkf = *on or *inkl = *on
073100120913     c                   write     si17d01
073200120913     c                   write     si17z11
073300120913     c                   exfmt     si17c11
073400120913     c                   setoff                                       2890
073500120913     c                   setoff                                       646566
073600120913     C                   MOVEL     '0'           ERRL00
073700120913     c* F12 - ritorno
073800120913     c                   if        *inkl
073900120913     c***                goto      endscade
074000120913     c                   LEAVE
074100120913     c                   endif
074200120913     c* controllo le selezioni fatte
074300120913     c                   exsr      CTRSCADE
074400120913     c
074500120913     c* se non ci sono errori e ho premuto F6
074600120913     c                   IF        ErrL00 <> *on and
074700120913     c                             *inkf
074800120913     c* leggo il sfl ed effettuo le modifiche
074900120913     c                   EXSR      GenNewScad
075000120913     c                   endif
075100120913     c                   ENDDO
075200120913     c
075300120913     C                   ENDIF
075400120913     c
075500120913     c     endscade      tag
075600120913     c
075700120913     c                   if        dsmsms=*blank
075800120913     c                   movel     saving        viding
075900120921     c******             z-add     savult        sflult
076000120913     c                   movel     '1'           swmodi
076100120913     c                   else
076200120913     c                   movel     '1'           opzok
076300120913     c                   endif
076400120913     C*
076500120913     C                   ENDSR
076600120913     C*------------------------------------------------------------------------*
076700120913     C* CONTROLLO DATI PER SCADENZA TERMINAL       OLD!
076800120913     C*------------------------------------------------------------------------*
076900120913     C     CTRSCADE_o    BEGSR
077000120913     C*
077100120913     C* Ctrl formale nuova data scadenza x tutte le linee
077200120913     C     *LIKE         DEFINE    VIDDSC        DSC11VID
077300120913IF  1C     ERRL00        IFEQ      '0'
077400120913     C                   Z-ADD     V11DSC        WDA8
077500120913     C                   Z-ADD     *ZEROS        DA8W
077600120913     C                   EXSR      CHKDA8
077700120913     C                   Z-ADD     WDA8          V11DSC
077800120913     C                   Z-ADD     DA8W          DSC11VID
077900120913IF  2C     ERRL00        IFEQ      '1'
078000120913     C                   SETON                                        6428
078100120913     C                   MOVEL     MSG(12)       vidmsg
078200120913     c                   LEAVESR
078300120913E   2C                   ENDIF
078400120913E   1C                   ENDIF
078500120913     c
078600120913     c* Ctrl formale data scadenza nuova associazione
078700120913     C     *LIKE         DEFINE    V11nwDSC      DSC11NwVID
078800120913IF  1C     ERRL00        IFEQ      '0'
078900120913     C                   Z-ADD     V11nwDSC      WDA8
079000120913     C                   Z-ADD     *ZEROS        DA8W
079100120913     C                   EXSR      CHKDA8
079200120913     C                   Z-ADD     WDA8          V11nwDSC
079300120913     C                   Z-ADD     DA8W          DSC11NwVID
079400120913IF  2C     ERRL00        IFEQ      '1'
079500120913     C                   SETON                                        6528
079600120913     C                   MOVEL     MSG(12)       vidmsg
079700120913     c                   LEAVESR
079800120913E   2C                   ENDIF
079900120913E   1C                   ENDIF
080000120913     c
080100120913     C* La data scadenza nuovo terminal deve essere > scadenza terminal precedente
080200120913    1c                   if        dsc11nwvid<=dsc11vid
080300120913     C                   SETON                                        646528
080400120913     C                   MOVEL     MSG(9)        vidmsg
080500120913     C* precedente msg, ora sostituito
080600120913     C* TISI17R- La nuova data decorrenza deve essere maggiore della prima             9
080700120913     C                   MOVEL     '1'           ERRL00
080800120913     c                   LEAVESR
080900120913E   1C                   ELSE
081000120913     c
081100120913     c* se richiesto il ripristino precedente associazione
081200120913     c* la data scadenza nuova associazione non deve essere > nuova data scadenza di più di 1 mese
081300120913     c* (facciamo 31 gg e non 1 mese variabile) altrimenti non è una cosa temporanea
081400120913     C                   IF        V11NwRip = 'SI'              and
081500120913     C                             %dec(%date(dsc11nwvid):*jul) -
081600120913     C                             %dec(%date(dsc11vid):*jul)   >
081700120913     C                             31
081800120913     C                   SETON                                        646528
081900120913     C                   MOVEL     MSG(14)       vidmsg
082000120913     C                   MOVEL     '1'           ERRL00
082100120913     c                   LEAVESR
082200120913E   1C                   ENDIF
082300120913E   1C                   ENDIF
082400120913     C*--------------------
082500120913     C* TERMINAL / LINEA ECCEZIONE
082600120913     C*--------------------
082700120913IF  1C     ERRL00        IFEQ      '0'
082800120913     C                   MOVEL     V11nwTFE      WLIN              3
082900120913     C                   MOVEL     *BLANKS       WDLIN
083000120913     C                   EXSR      CHKLIN
083100120913     C                   MOVEL     WLIN          V11nwTFE
083200120913     C                   MOVEL     WDLIN         V11nwdtE
083300120913IF  2C     ERRL00        IFEQ      '1'
083400120913     C                   SETON                                        6628
083500120913     C                   MOVEL     MSG(10)       vidmsg
083600120913     c                   LEAVESR
083700120913E   2C                   ENDIF
083800120913E   1C                   ENDIF
083900120913     c
084000120913     C                   ENDSR
084100120904     C*------------------------------------------------------------------------*
084200120904     C* Genero nuove scadenze a fronte dell'opz.SC
084300120904     C*------------------------------------------------------------------------*
084400120904     C     GenNewScad    BEGSR
084500120904     C*
084600120904     C* ciclo per tutti i rcd del sfl
084700120904     C                   DO        s11nrrcd      sflult
084800120904     C     sflult        chain     si17s11
084900120904     C*
085000120904     C* richiamo pgm che fa:
085100120904     C*
085200120904     C* modifico l'attuale data scadenza con quella a video
085300120904     C*
085400120904     C* scrivo una nuova associazione con:
085500120904     C* - terminal = quello indicato a video
085600120904     C* - data decorrenza = la scadenza impostata + 1
085700120904     C* - data scadenza = quella indicata a video come data scadenza nuova associazione
085800120904     C*
085900120904     C* se richiesto il ripristino associazione precedente,
086000120904     C* scrivo una nuova associazione con:
086100120904     C* - dati di partenza, arrivo e terminal = quelli su sfl
086200120904     C* - data decorrenza = la scadenza impostata sulla nuova associazione + 1
086300120904     C* - data scadenza = 31/12/2039
086400120904     C                   EVAL      DDeV11 = %dec(%date(V11DDe:*eur):*iso)
086500120904     C                   EVAL      NWTFE = %dec(V11NWTFE:3:0)
086600120904     C                   CALL      'TISI18R1'
086700120904     C                   PARM                    V11EPA
086800120904     C                   PARM                    V11TFP
086900120904     C                   PARM                    V11TFA
087000120904     C                   PARM                    DDeV11
087100120904     C                   PARM                    NwTFE
087200120904     C                   PARM                    DSC11Vid
087300120904     C                   PARM                    DSC11NwVid
087400120904     C                   PARM                    V11NwRip
087500120904     C                   PARM                    Err18R1
087600120904     C                   ENDDO
087700120904     C*
087800120904     C                   ENDSR
087900060929     C*------------------------------------------------------------------------*
088000060929     C* CHKDA8 - CONTROLLA LA DATA (8)
088100060929     C*------------------------------------------------------------------------*
088200060929     C     CHKDA8        BEGSR
088300060929     C*
088400060929     C     *LIKE         DEFINE    VIDDDE        WDA8
088500060929     C     *LIKE         DEFINE    VIDDDE        DA8W
088600060929     C*
088700060929     C                   Z-ADD     WDA8          G08DAT
088800060929     C                   Z-ADD     *ZEROS        G08INV
088900060929     C                   MOVEL     '0'           G08ERR
089000060929     C                   CALL      'XSRDA8'
089100060929     C                   PARM                    WLBDA8
089200060929     C                   Z-ADD     G08DAT        WDA8
089300060929     C                   Z-ADD     G08INV        DA8W
089400060929IF  1C     G08ERR        IFEQ      '1'
089500060929     C                   MOVEL     '1'           ERRL00
089600060929E   1C                   ENDIF
089700060929     C*
089800060929     C                   ENDSR
089900060929     C*------------------------------------------------------------------------*
090000060929     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
090100060929     C*------------------------------------------------------------------------*
090200060929     C     CHKLIN        BEGSR
090300060929     C*
090400060929     C     *LIKE         DEFINE    Viddta        WDLIN
090500060929     C*
090600060929     C* RICHIESTO ELENCO
090700120904     C***                SETOFF                                           28
090800120904     C***  '?'           SCAN      WLIN                                   28
090900120904IF  1C***  *IN28         IFEQ      *ON
091000120904IF  1C                   IF        %scan('?':wLin) > 0
091100060929     C                   MOVEL     *BLANKS       §COD1             3
091200060929     C                   MOVEL     *BLANKS       §TIP              1
091300060929     C                   MOVEL     *BLANKS       §DES1            25
091400060929     C                   MOVEL     *BLANKS       §PRA              3
091500060929     C                   CALL      'TNSD24R'
091600060929     C                   PARM                    §COD1
091700060929     C                   PARM                    §TIP
091800060929     C                   PARM                    §DES1
091900060929     C                   PARM                    §PRA
092000060929     C                   MOVEL     §COD1         WLIN
092100060929     C                   MOVEL     §DES1         WDLIN
092200060929E   1C                   ENDIF
092300060929     C*
092400060929     C* SOLO CARATTERI NUMERICI
092500120904     C                   SETOFF                                       66
092600120904     C                   TESTN                   WLIN                 66
092700120904IF  1C     *IN66         IFEQ      *OFF                                         *NON E'NUMERO
092800060929     C                   MOVEL     '1'           ERRL00
092900060929X   1C                   ELSE                                                   *E'NUMERO DA TESTN
093000060929     C                   MOVE      WLIN          W001              1
093100060929IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
093200120904     C                   SETON                                        66
093300060929     C                   MOVEL     '1'           ERRL00
093400060929X   2C                   ELSE                                                   *NUMERO
093500060929     C*
093600060929     C* CONTROLLA ESISTENZA CODICE
093700060929     C                   MOVEL     WLIN          KORFIL
093800060929     C     KEYORG        CHAIN     AZORG01L                           99
093900060929IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
094000060929     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
094100060929     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
094200060929     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
094300060929X   3C                   ELSE
094400060929     C                   MOVEL     '1'           ERRL00
094500060929E   3C                   ENDIF
094600060929E   2C                   ENDIF
094700060929E   1C                   ENDIF
094800060929     C*
094900060929     C                   ENDSR
095000970526     C*------------------------------------------------------------------------*
095100970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
095200970526     C*------------------------------------------------------------------------*
095300970526     C     NEWPOS        BEGSR
095400970526     C*
095500970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
095600970526     C*         1= RICHIESTO POSIZIONAMENTO
095700970526     C                   Z-ADD     0             SWPOSI            7 0
095800970526     C*
095900970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
096000970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
096100970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
096200970526     C                   Z-ADD     0             SWMODI            1 0
096300970530     C*
096400970530     C* REIMPOSTA INDICATORI DI ERRORE
096500970530     C                   SETOFF                                       808182
096600970530     C                   SETOFF                                       83
096700970526     C*
096800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
096900970528IF  1C     VIDPPA        IFNE      DEPEPA
097000970528     C     VIDPTP        ORNE      DEPTFP
097100970528     C     VIDPTA        ORNE      DEPTFA
097200980107     C     VIDPTE        ORNE      DEPTFE
097300970528     C     VIDPDD        ORNE      DEPDDE
097400970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
097500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
097600970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
097700970528     C                   MOVEL     VIDPPA        DEPEPA                         *DEPOSITI
097800970528     C                   Z-ADD     VIDPTP        DEPTFP
097900970528     C                   Z-ADD     VIDPTA        DEPTFA
098000980107     C                   Z-ADD     VIDPTE        DEPTFE
098100970528     C                   Z-ADD     VIDPDD        DEPDDE
098200970526E   2C                   ENDIF
098300970526E   1C                   ENDIF
098400970526     C*
098500970526     C                   ENDSR
098600970526     C*------------------------------------------------------------------------*
098700970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
098800970526     C*------------------------------------------------------------------------*
098900970528     C     SETFIL        BEGSR
099000970526     C*
099100970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
099200970528IF  1C     WTIPRI        IFEQ      'PAR'                                        *PER PARTENZA
099300970618     C                   MOVEL     VIDPPA        KCEEPA
099400970618     C                   Z-ADD     VIDPTP        KCETFP
099500980108     C                   Z-ADD     VIDPTA        KCETFA
099600970618     C     KEYCAE        SETLL     AZCAE01L                           99
099700970528E   1C                   ENDIF
099800970528     C*
099900970528IF  1C     WTIPRI        IFEQ      'ARR'                                        *PER ARRIVO
100000970618     C                   MOVEL     VIDPPA        KCEEPA
100100970618     C                   Z-ADD     VIDPTA        KCETFA
100200980108     C                   Z-ADD     VIDPTP        KCETFP
100300970618     C     KE2CAE        SETLL     AZCAE02L                           99
100400970528E   1C                   ENDIF
100500980107     C*
100600980107IF  1C     WTIPRI        IFEQ      'ECC'                                        *PER ARRIVO
100700980107     C                   MOVEL     VIDPPA        KCEEPA
100800980107     C                   Z-ADD     VIDPTE        KCETFE
100900980108     C                   Z-ADD     VIDPTA        KCETFA
101000980108     C                   Z-ADD     VIDPTP        KCETFP
101100980107     C     KE5CAE        SETLL     AZCAE05L                           99
101200980107E   1C                   ENDIF
101300970526     C* LETTURA RECORD
101400970526IF  1C     *IN99         IFEQ      *OFF
101500970526     C                   EXSR      LETFIL
101600970526E   1C                   ENDIF
101700970526     C*
101800970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
101900970526IF  1C     *IN99         IFEQ      *ON
102000970526     C                   Z-ADD     1             SFLEOF
102100970526E   1C                   ENDIF
102200970526     C*
102300970526     C                   ENDSR
102400970526     C*------------------------------------------------------------------------*
102500970526     C* LETFIL - LETTURA RECORD
102600970526     C*------------------------------------------------------------------------*
102700970526     C     LETFIL        BEGSR
102800970526     C*
102900970526     C* LEGGE FINO A:
103000970526     C*     - FINE FILE
103100970526     C*     - TROVATO RECORD VALIDO
103200970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
103300970526DO  1C     *IN99         DOWEQ     *OFF
103400970526     C     WRECOK        ANDEQ     'N'
103500970526     C*
103600970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
103700970528IF  2C     WTIPRI        IFEQ      'PAR'                                        *PER PARTENZA
103800970618     C                   READ      AZCAE01L                               99
103900970528E   2C                   ENDIF
104000970526     C*
104100970528IF  2C     WTIPRI        IFEQ      'ARR'                                        *PER ARRIVO
104200970618     C                   READ      AZCAE02L                               99
104300970528E   2C                   ENDIF
104400980107     C*
104500980107IF  2C     WTIPRI        IFEQ      'ECC'                                        *PER ECCEZIONE
104600980107     C                   READ      AZCAE05L                               99
104700980107E   2C                   ENDIF
104800970528     C*
104900970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
105000970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
105100970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
105200970530X   2C                   ELSE
105300970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
105400970530E   2C                   ENDIF
105500970526E   1C                   ENDDO
105600970526     C*
105700970526     C                   ENDSR
105800970526     C*------------------------------------------------------------------------*
105900970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
106000970526     C*------------------------------------------------------------------------*
106100970526     C     CHKREC        BEGSR
106200970526     C*
106300970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
106400970530     C*
106500970530     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
106600970530IF  1C     VIDPDD        IFGT      *ZEROS
106700970618IF  2C     PDDVID        IFGE      CAEDDE                                       *IN DECORRENZA
106800970618     C     PDDVID        ANDLE     CAEDSC                                       *ALLA DATA
106900970530X   2C                   ELSE
107000970530     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
107100970530E   2C                   ENDIF
107200970530E   1C                   ENDIF
107300970526     C*
107400970526     C                   ENDSR
107500970526     C*------------------------------------------------------------------------*
107600970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
107700970526     C*------------------------------------------------------------------------*
107800970526     C     IRESFL        BEGSR
107900970526     C*
108000970526     C                   MOVEL     *BLANKS       OPZKEY
108100970618     C                   Z-ADD     CAEDDE        VIHDDE
108200970618     C                   MOVEL     CAEATB        VIDATB
108300970618     C                   MOVEL     CAEEPA        VIDEPA
108400970618     C                   Z-ADD     CAETFP        VIDTFP
108500970618     C                   Z-ADD     CAETFA        VIDTFA
108600970618     C                   Z-ADD     CAETFE        VIDTFE
108700970618     C                   Z-ADD     CAEDDE        G08INV
108800970618     C                   Z-ADD     *ZEROS        G08DAT
108900970618     C                   MOVEL     '3'           G08ERR
109000970618     C                   CALL      'XSRDA8'
109100970618     C                   PARM                    WLBDA8
109200970618     C                   Z-ADD     G08DAT        VIDDDE
109300970618     C                   Z-ADD     CAEDSC        G08INV
109400970618     C                   Z-ADD     *ZEROS        G08DAT
109500970618     C                   MOVEL     '3'           G08ERR
109600970618     C                   CALL      'XSRDA8'
109700970618     C                   PARM                    WLBDA8
109800970618     C                   Z-ADD     G08DAT        VIDDSC
109900970528     C* DECODIFICHE
110000970618     C                   MOVEL     *BLANKS       VIDDTP
110100970618IF  1C     CAETFP        IFGT      *ZEROS
110200060929     C                   SETOFF                                           30
110300970528     C                   Z-ADD     1             I
110400060929     C     CAETFP        LOOKUP    ORG(I)                                 30
110500060929IF  2C     *IN30         IFEQ      *ON                                          *TROVATO
110600970528     C                   MOVEL     DORG(I)       VIDDTP
110700970618E   2C                   ENDIF
110800970618E   1C                   ENDIF
110900970526     C*
111000970618     C                   MOVEL     *BLANKS       VIDDTA
111100970618IF  1C     CAETFA        IFGT      *ZEROS
111200060929     C                   SETOFF                                           30
111300970528     C                   Z-ADD     1             I
111400060929     C     CAETFA        LOOKUP    ORG(I)                                 30
111500060929IF  2C     *IN30         IFEQ      *ON                                          *TROVATO
111600970528     C                   MOVEL     DORG(I)       VIDDTA
111700970618E   2C                   ENDIF
111800970618E   1C                   ENDIF
111900970528     C*
112000970618     C                   MOVEL     *BLANKS       VIDDTE
112100060929     C                   SETOFF                                           30
112200970528     C                   Z-ADD     1             I
112300060929     C     CAETFE        LOOKUP    ORG(I)                                 30
112400060929IF  1C     *IN30         IFEQ      *ON                                          *TROVATO
112500970528     C                   MOVEL     DORG(I)       VIDDTE
112600970528E   1C                   ENDIF
112700970528     C*
112800970526     C                   ENDSR
112900970526     C*------------------------------------------------------------------------*
113000970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
113100970526     C*------------------------------------------------------------------------*
113200970526     C     CHKPAR        BEGSR
113300970528     C*
113400970528     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER PARTENZA
113500970529IF  1C     VIDPPA        IFEQ      *BLANKS
113600970529     C     VIDPTP        ANDEQ     *ZEROS
113700970528     C     VIDPTA        ANDEQ     *ZEROS
113800980107     C     VIDPTE        ANDEQ     *ZEROS
113900970528     C                   MOVEL     'PAR'         WTIPRI            3
114000970528E   1C                   ENDIF                                                  *ORDINAMENTO RICHIES
114100970528     C*
114200970528     C* CONTROLLA LA DATA DECORRENZA
114300970528     C     *LIKE         DEFINE    VIDPDD        PDDVID                         *DATA IN A/M/G
114400970528     C                   Z-ADD     *ZEROS        PDDVID
114500970528IF  1C     ERRL00        IFEQ      '0'
114600970528IF  2C     VIDPDD        IFGT      *ZEROS
114700970528     C                   Z-ADD     VIDPDD        G08DAT
114800970528     C                   Z-ADD     *ZEROS        G08INV
114900970528     C                   MOVEL     '0'           G08ERR
115000970528     C                   CALL      'XSRDA8'
115100970528     C                   PARM                    WLBDA8
115200970528     C                   Z-ADD     G08DAT        VIDPDD
115300970528     C                   Z-ADD     G08INV        PDDVID
115400970528IF  3C     G08ERR        IFEQ      '1'
115500970528     C                   MOVEL     '1'           ERRL00
115600970528     C                   MOVEL     MSG(12)       DSMSMS
115700970528     C                   SETON                                        83
115800970528E   3C                   ENDIF
115900970528E   2C                   ENDIF
116000970528E   1C                   ENDIF
116100970528     C*
116200970528     C* CONTROLLA IL TIPO RICERCA
116300970528IF  1C     ERRL00        IFEQ      '0'
116400970528IF  2C     VIDPTP        IFNE      *ZEROS
116500980107     C                   MOVEL     'PAR'         WTIPRI
116600970528E   2C                   ENDIF
116700970528E   1C                   ENDIF
116800980107     C*
116900980107IF  1C     ERRL00        IFEQ      '0'
117000980107IF  2C     VIDPTA        IFNE      *ZEROS
117100970528     C                   MOVEL     'ARR'         WTIPRI
117200980107E   2C                   ENDIF
117300980107E   1C                   ENDIF
117400980107     C*
117500980107IF  1C     ERRL00        IFEQ      '0'
117600980107IF  2C     VIDPTE        IFNE      *ZEROS
117700980107     C                   MOVEL     'ECC'         WTIPRI
117800980107E   2C                   ENDIF
117900980107E   1C                   ENDIF
118000970526     C*
118100970526     C                   ENDSR
118200970526     C*------------------------------------------------------------------------*
118300970526     C* LANPGM - LANCIA UN PROGRAMMA
118400970526     C*------------------------------------------------------------------------*
118500970526     C     LANPGM        BEGSR
118600970526     C*
118700970526     C* IMPOSTA LA DS DI PROCEDURA
118800970602     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
118900970602     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
119000970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
119100970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
119200970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
119300970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
119400970602     C                   MOVEL     DEPA          D00EPA                         *PARTENZA/ARRIVO
119500970602     C                   Z-ADD     DETP          D00ETP                         *TERMINAL PARTENZA
119600970602     C                   Z-ADD     DETA          D00ETA                         *TERMINAL ARRIVO
119700970602     C                   Z-ADD     DEDD          D00EDD                         *DECORRENZA
119800970602     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI DATI
119900970530     C                   MOVEL     *BLANKS       D00TSP
120000970530     C                   Z-ADD     *ZEROS        D00TFP
120100970530     C                   MOVEL     *BLANKS       D00LOC
120200970603     C                   MOVEL     *BLANKS       D00SIN
120300970530     C                   MOVEL     *BLANKS       D00SPT
120400970530     C                   Z-ADD     *ZEROS        D00SLP
120500970530     C                   Z-ADD     *ZEROS        D00SLA
120600970530     C                   MOVEL     *BLANKS       D00STS
120700970530     C                   Z-ADD     *ZEROS        D00SDD
120800970526     C*
120900970526     C                   CALL      WPGM
121000970612     C                   PARM                    KPJBA
121100970612     C                   PARM                    DSSI00
121200970526     C*
121300970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
121400970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
121500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
121600970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
121700970526X   1C                   ELSE                                                   *NON PREMUTO F03
121800970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
121900970526E   1C                   ENDIF
122000970526     C*
122100970526     C                   ENDSR
122200970527     C*------------------------------------------------------------------------*
122300970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
122400970527     C*------------------------------------------------------------------------*
122500970527     C     CARTAB        BEGSR
122600970528     C*---
122700970528     C* ORGANIGRAMMA
122800970528     C*---
122900970528     C                   Z-ADD     *ZEROS        I                 5 0
123000970528     C                   Z-ADD     *ZEROS        ORG
123100970528     C                   MOVEL     *BLANKS       DORG
123200970528     C     *LOVAL        SETLL     AZORG01L
123300970528     C                   READ      AZORG01L                               99
123400970528DO  1C     *IN99         DOWEQ     *OFF
123500970528IF  2C     ORGFVA        IFEQ      *BLANKS
123600970528     C     ORGFAG        ANDNE     'V'
123700970528     C                   ADD       1             I
123800970528     C                   Z-ADD     ORGFIL        ORG(I)
123900970528     C                   MOVEL     ORGDES        DORG(I)
124000970528E   2C                   ENDIF
124100970528     C                   READ      AZORG01L                               99
124200970528E   1C                   ENDDO                                                  *FINE ORGANIGRAMMA
124300970527     C*
124400970527     C                   ENDSR
124500970526     C*------------------------------------------------------------------------*
124600970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
124700970526     C*------------------------------------------------------------------------*
124800970526     C     CHKFOP        BEGSR
124900970626     C*
125000970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
125100970626IF  1C     DEPOP0        IFEQ      'P01'
125200970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
125300970626     C                   MOVEL     'S'           OPDATT
125400970626     C                   MOVEL     OPDRIG        OPD(1)
125500970626X   1C                   ELSE
125600970626     C* OPZIONI
125700970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
125800970626     C                   MOVEL     'S'           OPDATT
125900970626     C                   MOVEL     OPDRIG        OPD(5)
126000970626     C* FUNZIONI
126100970626E   1C                   ENDIF
126200970626     C*
126300970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
126400970626IF  1C     DEPOP0        IFEQ      'P02'
126500970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
126600970626     C                   MOVEL     'S'           OPDATT
126700970626     C                   MOVEL     OPDRIG        OPD(2)
126800970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
126900970626     C                   MOVEL     'S'           OPDATT
127000970626     C                   MOVEL     OPDRIG        OPD(4)
127100970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
127200970626     C                   MOVEL     'S'           OPDATT
127300970626     C                   MOVEL     OPDRIG        OPD(7)
127400060929     C                   MOVEL     OPD(10)       OPDRIG                         *SCELTA
127500060929     C                   MOVEL     'S'           OPDATT
127600060929     C                   MOVEL     OPDRIG        OPD(10)
127700970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
127800970626     C                   MOVEL     'S'           FUDATT
127900970626     C                   MOVEL     FUDRIG        FUD(6)
128000970626E   1C                   ENDIF
128100970526     C*
128200970526     C* GESTIONE OPZIONI A VIDEO
128300970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
128400970526     C                   EXSR      OPZGES
128500970526     C*
128600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
128700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
128800970526     C                   EXSR      FUNGES
128900970526     C*
129000970526     C                   ENDSR
129100970526     C*------------------------------------------------------------------------*
129200970526     C* *INZSR - OPERAZIONI INIZIALI
129300970526     C*------------------------------------------------------------------------*
129400970526     C     *INZSR        BEGSR
129500970526     C*---
129600970526     C* RICEVIMENTO PARAMETRI
129700970526     C*---
129800970526     C     *ENTRY        PLIST
129900970612     C                   PARM                    KPJBA
130000970612     C                   PARM                    DSSI00
130100970526     C*
130200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
130300970526     C     XMSGPA        PLIST
130400970526     C                   PARM                    DSMSMS
130500970526     C                   PARM                    DSMSPG
130600970526     C                   PARM                    DSMSKE
130700970526     C                   PARM                    DSMSER
130800970526     C*
130900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
131000970526     C                   CALL      'XKEY'
131100970526     C                   PARM                    DSKEY
131200970526     C*---
131300970526     C* VARIABILI NON RIFERITE AL DATA BASE
131400970526     C*---
131500970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
131600970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
131700970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
131800970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
131900970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
132000970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
132100120913     C     *LIKE         DEFINE    SFLNBR        RigaLetta
132200970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
132300970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
132400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
132500970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
132600970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
132700970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
132800970529     C     *LIKE         DEFINE    D00OP1        WOP1
132900970602     C     *LIKE         DEFINE    D00EPA        DEPA
133000970602     C     *LIKE         DEFINE    D00ETP        DETP
133100970602     C     *LIKE         DEFINE    D00ETA        DETA
133200970602     C     *LIKE         DEFINE    D00EDD        DEDD
133300970526     C*---
133400970526     C* VARIABILI DI CONTROLLO
133500970526     C*---
133600970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
133700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
133800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
133900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
134000970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
134100970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
134200970526     C*
134300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
134400970528     C                   MOVEL     'TISI17R'     DSMSPG
134500970528     C                   MOVEL     'TISI17R'     MSGQUE
134600970526     C                   MOVEL     *BLANKS       MSGKEY
134700970526     C                   MOVEL     *BLANKS       DSMSMS
134800970526     C                   MOVEL     *BLANKS       DSMSKE
134900970526     C                   Z-ADD     *ZEROS        DSMSER
135000970526     C*---
135100970526     C* VARIABILI RIFERITE AL DATA BASE
135200970526     C*---
135300970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
135400970618     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         *AZCAE00F
135500970618     C     *LIKE         DEFINE    CAETFP        KCETFP
135600970618     C     *LIKE         DEFINE    CAETFA        KCETFA
135700980107     C     *LIKE         DEFINE    CAETFE        KCETFE
135800970618     C     *LIKE         DEFINE    CAEEPA        DEPEPA                         *DEPOSITI
135900970618     C     *LIKE         DEFINE    CAETFP        DEPTFP
136000970618     C     *LIKE         DEFINE    CAETFA        DEPTFA
136100980107     C     *LIKE         DEFINE    CAETFE        DEPTFE
136200970618     C     *LIKE         DEFINE    CAEDDE        DEPDDE
136300970526     C*---
136400970526     C* CHIAVI DI LETTURA
136500970526     C*---
136600970528     C* LETTURA AZORG01L - COMPLETA
136700970528     C     KEYORG        KLIST
136800970528     C                   KFLD                    KORFIL                         *FILIALE
136900970618     C* LETTURA AZCAE01L - PARZIALE
137000970618     C     KEYCAE        KLIST
137100970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
137200970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
137300980108     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
137400120913     C* LETTURA AZCAE01L - TOTALE
137500120913     C     K04CAE01      KLIST
137600120913     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
137700120913     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
137800120913     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
137900120913     C                   KFLD                    CAEDDE                         *DECORRENZA
138000120913     C* LETTURA AZCAE01L - Parziale
138100120913     C     K03CAE01      KLIST
138200120913     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
138300120913     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
138400120913     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
138500970618     C* LETTURA AZCAE02L - PARZIALE
138600970618     C     KE2CAE        KLIST
138700970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
138800970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
138900980108     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
139000980107     C* LETTURA AZCAE05L - PARZIALE
139100980107     C     KE5CAE        KLIST
139200980107     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
139300980107     C                   KFLD                    KCETFE                         *TERMINAL ECCEZIONE
139400980108     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
139500980108     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
139600120921     C* LETTURA AZCAE05L - TOTALE
139700120921     C     K05CAE05      KLIST
139800120921     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
139900120921     C                   KFLD                    CAETFE                         *TERMINAL ECCEZIONE
140000120921     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
140100120921     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
140200120921     C                   KFLD                    CAEDDE                         *DECORRENZA
140300120921     C* LETTURA AZCAE05L - PARZIALE
140400120921     C     K04CAE05      KLIST
140500120921     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
140600120921     C                   KFLD                    CAETFE                         *TERMINAL ECCEZIONE
140700120921     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
140800120921     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
140900970527     C*
141000970527     C* CARICA LE TABELLE OCCORRENTI
141100970527     C                   EXSR      CARTAB
141200970526     C*
141300970526     C* DEPOSITA LE OPZIONI DI ENTRATA
141400970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
141500970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
141600970526     C*
141700970526     C* CALCOLA LA DATA CORRENTE
141800970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
141900970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
142000970526     C                   Z-ADD     WN8           G08DAT
142100970526     C                   Z-ADD     *ZEROS        G08INV
142200970526     C                   MOVEL     '0'           G08ERR
142300970526     C                   CALL      'XSRDA8'
142400970526     C                   PARM                    WLBDA8
142500970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
142600970526     C*
142700970526     C* IMPOSTA I PARAMETRI DI INGRESSO
142800970528     C                   MOVEL     D00EPA        VIDPPA                         *PARTENZA/ARRIVO
142900970528     C                   Z-ADD     D00ETP        VIDPTP                         *TERMINAL PARTENZA
143000970528     C                   Z-ADD     D00ETA        VIDPTA                         *TERMINAL ARRIVO
143100980107     C                   Z-ADD     *ZEROS        VIDPTE                         *TERMINAL ECCEZIONE
143200970528     C                   Z-ADD     *ZEROS        VIDPDD                         *DATA DECORRENZA
143300970528IF  1C     D00EDD        IFGT      *ZEROS
143400970528     C                   Z-ADD     D00EDD        G08INV
143500970528     C                   Z-ADD     *ZEROS        G08DAT
143600970528     C                   MOVEL     '3'           G08ERR
143700970528     C                   CALL      'XSRDA8'
143800970528     C                   PARM                    WLBDA8
143900970528     C                   Z-ADD     G08DAT        VIDPDD
144000120903     C* se non è stata passata la data decorrenza, la imposto a video = oggi
144100120903E   1C                   ELSE
144200120903     C                   EVAL      VidPDD = %dec(%date():*eur)
144300120903E   1C                   ENDIF
144400970526     C                   EXSR      CHKPAR
144500970526IF  1C     ERRL00        IFEQ      '1'
144600970526     C                   MOVEL     '1'           D00ERR
144700970526     C                   MOVEL     DSMSMS        D00MSG
144800970526X   1C                   ELSE
144900970528     C                   MOVEL     VIDPPA        DEPEPA                         *DEPOSITI
145000970528     C                   Z-ADD     VIDPTP        DEPTFP
145100970528     C                   Z-ADD     VIDPTA        DEPTFA
145200980107     C                   Z-ADD     *ZEROS        DEPTFE
145300970528     C                   Z-ADD     VIDPDD        DEPDDE
145400970526E   1C                   ENDIF
145500970526     C*
145600970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
145700970526     C                   EXSR      CHKFOP
145800970526     C*
145900120903     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
146000970530IF  1C     DEPOP0        IFEQ      'P01'
146100970530     C                   MOVEL     DEC(3)        VIDING
146200970530E   1C                   ENDIF
146300970530IF  1C     DEPOP0        IFEQ      'P02'
146400970526     C                   MOVEL     DEC(1)        VIDING
146500970530E   1C                   ENDIF
146600970530IF  1C     DEPOP0        IFEQ      'P05'
146700970526     C                   MOVEL     DEC(2)        VIDING
146800970526E   1C                   ENDIF
146900970526     C*
147000970526     C                   ENDSR
147100970526     C*------------------------------------------------------------------------*
147200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
147300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
147400970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
147500970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
147600970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
147700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
147800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
147900970526     C*------------------------------------------------------------------------*
148000970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
148100970526     C*------------------------------------------------------------------------*
148200970526     C     POSSFL        BEGSR
148300970526     C*
148400970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
148500970526     C                   EXSR      CLRSFL
148600970526     C*
148700970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
148800970526     C                   EXSR      SETFIL
148900970526     C*
149000970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
149100970526     C     SFLEOF        IFEQ      0
149200970526     C                   EXSR      CARSFL
149300970526     C                   END
149400970526     C*
149500970526     C                   ENDSR
149600970526     C*------------------------------------------------------------------------*
149700970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
149800970526     C*------------------------------------------------------------------------*
149900970526     C     ENTATT        BEGSR
150000970526     C*
150100970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
150200970526     C                   Z-ADD     0             OPZOK             1 0
150300970526     C*
150400970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
150500970526     C                   Z-ADD     0             SFLVAR            5 0
150600970526     C*
150700970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
150800970526     C                   Z-ADD     0             SFLERR
150900970526     C*
151000970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
151100970526     C                   Z-ADD     SFLULT        WFLULT
151200970526     C*
151300970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
151400970528     C                   READC     SI17D05                                98
151500970526     C     *IN98         DOWEQ     '0'
151600970526     C*
151700970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
151800970526     C                   ADD       1             SFLVAR
151900970526     C*
152000970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
152100970526     C     OPZOK         IFEQ      0
152200970526     C*
152300970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
152400970526     C     OPZKEY        IFNE      *BLANKS
152500970526     C                   EXSR      OPZCON
152600970526     C                   END
152700970526     C*
152800970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
152900970526     C*            2. PULIZIA SCELTA IN SFL
153000970526     C     OPZOK         IFEQ      0
153100970526     C                   MOVEL     '0'           *IN41
153200970526     C                   MOVEL     *BLANKS       OPZKEY
153300970526     C                   ELSE
153400970526     C*
153500970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
153600970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
153700970526     C                   MOVEL     '1'           *IN41
153800970526     C                   Z-ADD     SFLULT        SFLERR
153900970526     C                   END
154000970526     C                   ELSE
154100970526     C*
154200970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
154300970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
154400970526     C                   MOVEL     '1'           *IN41
154500970526     C                   END                                                    OPZOK=0
154600970526     C*
154700970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
154800970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
154900970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
155000970526     C     OPZOK         IFNE      0
155100970526     C                   Z-ADD     SFLERR        SFLNBR
155200970526     C                   ELSE
155300970526     C                   Z-ADD     SFLULT        SFLNBR
155400970526     C                   END
155500970526     C*
155600970526     C* AGGIORNO RECORD SFL
155700970528     C                   UPDATE    SI17D05
155800970526     C*
155900970528     C                   READC     SI17D05                                98
156000970526     C                   END                                                    DO WHILE
156100970526     C*
156200970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
156300970526     C                   Z-ADD     WFLULT        SFLULT
156400970526     C*
156500970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
156600970526     C     SFLVAR        IFEQ      0
156700970526     C*---                 MOVEL'1'       FINL00
156800970526     C                   END
156900970526     C*
157000970526     C                   ENDSR
157100970526     C*------------------------------------------------------------------------*
157200970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
157300970526     C*------------------------------------------------------------------------*
157400970526     C     CLRSFL        BEGSR
157500970526     C*
157600970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
157700970526     C* (0=NON FINE FILE  1=FINE FILE)
157800970526     C                   Z-ADD     0             SFLEOF
157900970526     C*
158000970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
158100970526     C                   Z-ADD     0             SFLULT
158200970526     C*
158300970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
158400970526     C                   MOVEL     '1'           *IN40
158500970528     C                   WRITE     SI17D06
158600970526     C                   MOVEL     '0'           *IN40
158700970526     C*
158800970526     C                   ENDSR
158900970526     C*------------------------------------------------------------------------*
159000970526     C* CARSFL - CARICAMENTO SFL
159100970526     C*------------------------------------------------------------------------*
159200970526     C     CARSFL        BEGSR
159300970526     C*
159400970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
159500970526     C     SFLULT        IFEQ      0
159600970526     C                   EXSR      SETFIL
159700970526     C                   END
159800970526     C*
159900970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
160000970526     C                   Z-ADD     SFLULT        WFLULT
160100970526     C*
160200970526     C* CICLO:
160300970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
160400970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
160500970526     C                   Z-ADD     1             SFLRIG
160600970526     C     SFLRIG        DOWLE     SFLPAG
160700970526     C     SFLEOF        ANDEQ     0
160800970526     C*
160900970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
161000970526     C                   MOVEL     *BLANKS       OPZKEY
161100970526     C*
161200970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
161300970526     C                   EXSR      IRESFL
161400970526     C*
161500970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
161600970526     C                   ADD       1             SFLULT
161700970526     C*
161800970526     C* SCRITTURA RECORD SFL
161900970528     C                   WRITE     SI17D05
162000970526     C                   ADD       1             SFLRIG
162100970526     C*
162200970526     C* LETTURA FILE DA CARICARE
162300970526     C                   EXSR      LETFIL
162400060706     c* se cambiato tipo, lascio una riga vuota se non ho finito la
162500060706     c* pagina
162600060706     c                   if        caeepa<>videpa     and sfleof=0 and
162700060706     C                             sflrig<=sflpag
162800060706     c                   seton                                        01
162900060706     C                   MOVEL     *BLANKS       OPZKEY
163000060706     c                   clear                   vidatb
163100060706     c                   clear                   videpa
163200060706     c                   clear                   vidtfp
163300060706     c                   clear                   viddtp
163400060706     c                   clear                   vidtfa
163500060706     c                   clear                   viddta
163600060706     c                   clear                   vidtfe
163700060706     c                   clear                   viddte
163800060706     c                   clear                   viddde
163900060706     c                   clear                   viddsc
164000060706     C* SCRITTURA RECORD SFL
164100060706     C                   ADD       1             SFLULT
164200060706     C                   WRITE     SI17D05
164300060706     C                   ADD       1             SFLRIG
164400060706     c                   setoff                                       01
164500060706     C                   ENDif                                                  DO WHILE
164600060706     C                   END                                                    DO WHILE
164700970526     C*
164800970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
164900970526     C     WFLULT        IFNE      SFLULT
165000970526     C                   Z-ADD     WFLULT        SFLNBR
165100970526     C                   ADD       1             SFLNBR
165200970526     C                   END
165300970526     C*
165400970526     C                   ENDSR
165500970526     C*------------------------------------------------------------------------*
165600970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
165700970526     C*------------------------------------------------------------------------*
165800970526     C     OUTSFL        BEGSR
165900970526     C*
166000970526     C* WRITE RECORD TESTATA VIDEO
166100970528     C                   WRITE     SI17D01
166200970526     C*
166300970526     C* VISUALIZZAZIONE MESSAGGIO
166400970526     C                   EXSR      SNDMSG
166500970526     C*
166600970526     C* WRITE RECORD OPZIONI VIDEO
166700970528     C                   WRITE     SI17D02
166800970526     C*
166900970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
167000970528     C                   WRITE     SI17D03
167100970526     C*
167200970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
167300970528     C                   WRITE     SI17D04
167400970526     C*
167500970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
167600970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
167700970526     C     SFLEOF        IFEQ      0
167800970526     C                   MOVEL     '0'           *IN42
167900970526     C                   ELSE
168000970526     C*
168100970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
168200970526     C                   MOVEL     '1'           *IN42
168300970526     C                   END
168400970526     C*
168500970526     C* WRITE RECORD TASTI FUNZIONALI
168600970528     C                   WRITE     SI17D09
168700970526     C*
168800970526     C* CASO DI SFL VUOTO
168900970526     C     SFLULT        IFLE      0
169000970526     C*
169100970526     C*    - WRITE RECORD VUOTO
169200970528     C                   WRITE     SI17D0A
169300970528     C                   READ      SI17D0A                                99
169400970526     C*
169500970526     C                   ELSE
169600970526     C*
169700970526     C* CASO DI SFL CON RECORDS
169800970526     C*
169900970526     C*    - WRITE RECORD CONTROLLO SFL
170000970528     C                   WRITE     SI17D06
170100970528     C                   READ      SI17D06                                99
170200970526     C                   END
170300970526     C*
170400970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
170500970528     C                   READ      SI17D03                                99
170600970526     C*
170700970526     C                   ENDSR
170800970526     C*------------------------------------------------------------------------*
170900970526     C* SNDMSG - MANDA MESSAGGIO
171000970526     C*------------------------------------------------------------------------*
171100970526     C     SNDMSG        BEGSR
171200970526     C*
171300970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
171400970526     C                   MOVEL     '1'           *IN40
171500970528     C                   WRITE     SI17DMC
171600970526     C                   MOVEL     '0'           *IN40
171700970526     C*
171800970526     C* VISUALIZZAZIONE MESSAGGIO
171900970526     C                   Z-ADD     0             MSGULT
172000970526     C                   CALL      'XMSG'        XMSGPA
172100970526     C     DSMSER        IFEQ      0
172200970526     C                   MOVEL     DSMSKE        MSGKEY
172300970526     C                   END
172400970526     C                   Z-ADD     1             MSGULT
172500970528     C                   WRITE     SI17DMS                                      SFL MESS
172600970528     C                   WRITE     SI17DMC                                      CONTR MESS
172700970526     C*
172800970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
172900970526     C                   MOVEL     *BLANKS       DSMSMS
173000970526     C*
173100970526     C                   ENDSR
173200970526     C*------------------------------------------------------------------------*
173300970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
173400970526     C*------------------------------------------------------------------------*
173500970526     C     FUNCON        BEGSR
173600970526     C*
173700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
173800970526     C                   EXSR      FUNVAR
173900970526     C*
174000970526     C* PULISCO CAMPI DI LAVORO
174100970526     C                   Z-ADD     0             I                 5 0
174200970526     C                   Z-ADD     0             W                 5 0
174300970526     C*
174400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
174500970526     C     FUNKEY        IFNE      *BLANKS
174600970526     C     1             DO        24            I
174700970526     C                   MOVEL     FUD(I)        FUDRIG
174800970526     C*
174900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
175000970526     C     FUNKEY        IFEQ      FUDFUN
175100970526     C     FUDATT        ANDEQ     'S'
175200970526     C                   Z-ADD     I             W
175300970526     C                   END
175400970526     C                   END                                                    DO WHILE
175500970526     C*
175600970526     C* FUNZIONE NON ATTIVA (ERRORE)
175700970526     C     W             IFEQ      0
175800970526     C*
175900970526     C* EMISSIONE MESSAGGIO DI ERRORE
176000970526     C                   MOVEL     MSG(2)        DSMSMS
176100970526     C                   END                                                    W=0
176200970526     C                   END                                                    FUNKEY NE *BLANKS
176300970526     C*
176400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
176500970526     C     W             IFNE      0
176600970526     C     FUNKEY        OREQ      *BLANKS
176700970526     C*
176800970526     C     FUNKEY        IFEQ      *BLANKS
176900970526     C                   EXSR      ENTGES
177000970526     C                   END
177100970526     C     FUNKEY        IFEQ      '03'
177200970526     C                   EXSR      F03GES
177300970526     C                   END
177400970526     C     FUNKEY        IFEQ      '05'
177500970526     C                   EXSR      F05GES
177600970526     C                   END
177700970526     C     FUNKEY        IFEQ      '06'
177800970526     C                   EXSR      F06GES
177900970526     C                   END
178000970526     C     FUNKEY        IFEQ      '12'
178100970526     C                   EXSR      F12GES
178200970526     C                   END
178300970528     C     FUNKEY        IFEQ      '21'
178400970528     C                   EXSR      F21GES
178500970528     C                   END
178600970528     C     FUNKEY        IFEQ      '22'
178700970528     C                   EXSR      F22GES
178800970528     C                   END
178900970526     C     FUNKEY        IFEQ      '23'
179000970526     C                   EXSR      F23GES
179100970526     C                   END
179200970526     C     FUNKEY        IFEQ      '24'
179300970526     C                   EXSR      F24GES
179400970526     C                   END
179500970526     C     FUNKEY        IFEQ      'RU'
179600970526     C                   EXSR      RUPGES
179700970526     C                   END
179800970526     C*
179900970526     C                   END
180000970526     C*
180100970526     C                   ENDSR
180200970526     C*------------------------------------------------------------------------*
180300970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
180400970526     C*------------------------------------------------------------------------*
180500970526     C     FUNVAR        BEGSR
180600970526     C*
180700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
180800970526     C     KEY           IFEQ      ENTER
180900970526     C                   MOVEL     *BLANKS       FUNKEY
181000970526     C                   END
181100970526     C     KEY           IFEQ      F1
181200970526     C                   MOVEL     '01'          FUNKEY
181300970526     C                   END
181400970526     C     KEY           IFEQ      F2
181500970526     C                   MOVEL     '02'          FUNKEY
181600970526     C                   END
181700970526     C     KEY           IFEQ      F3
181800970526     C                   MOVEL     '03'          FUNKEY
181900970526     C                   END
182000970526     C     KEY           IFEQ      F4
182100970526     C                   MOVEL     '04'          FUNKEY
182200970526     C                   END
182300970526     C     KEY           IFEQ      F5
182400970526     C                   MOVEL     '05'          FUNKEY
182500970526     C                   END
182600970526     C     KEY           IFEQ      F6
182700970526     C                   MOVEL     '06'          FUNKEY
182800970526     C                   END
182900970526     C     KEY           IFEQ      F7
183000970526     C                   MOVEL     '07'          FUNKEY
183100970526     C                   END
183200970526     C     KEY           IFEQ      F8
183300970526     C                   MOVEL     '08'          FUNKEY
183400970526     C                   END
183500970526     C     KEY           IFEQ      F9
183600970526     C                   MOVEL     '09'          FUNKEY
183700970526     C                   END
183800970526     C     KEY           IFEQ      F10
183900970526     C                   MOVEL     '10'          FUNKEY
184000970526     C                   END
184100970526     C     KEY           IFEQ      F11
184200970526     C                   MOVEL     '11'          FUNKEY
184300970526     C                   END
184400970526     C     KEY           IFEQ      F12
184500970526     C                   MOVEL     '12'          FUNKEY
184600970526     C                   END
184700970526     C     KEY           IFEQ      F13
184800970526     C                   MOVEL     '13'          FUNKEY
184900970526     C                   END
185000970526     C     KEY           IFEQ      F14
185100970526     C                   MOVEL     '14'          FUNKEY
185200970526     C                   END
185300970526     C     KEY           IFEQ      F15
185400970526     C                   MOVEL     '15'          FUNKEY
185500970526     C                   END
185600970526     C     KEY           IFEQ      F16
185700970526     C                   MOVEL     '16'          FUNKEY
185800970526     C                   END
185900970526     C     KEY           IFEQ      F17
186000970526     C                   MOVEL     '17'          FUNKEY
186100970526     C                   END
186200970526     C     KEY           IFEQ      F18
186300970526     C                   MOVEL     '18'          FUNKEY
186400970526     C                   END
186500970526     C     KEY           IFEQ      F19
186600970526     C                   MOVEL     '19'          FUNKEY
186700970526     C                   END
186800970526     C     KEY           IFEQ      F20
186900970526     C                   MOVEL     '20'          FUNKEY
187000970526     C                   END
187100970526     C     KEY           IFEQ      F21
187200970526     C                   MOVEL     '21'          FUNKEY
187300970526     C                   END
187400970526     C     KEY           IFEQ      F22
187500970526     C                   MOVEL     '22'          FUNKEY
187600970526     C                   END
187700970526     C     KEY           IFEQ      F23
187800970526     C                   MOVEL     '23'          FUNKEY
187900970526     C                   END
188000970526     C     KEY           IFEQ      F24
188100970526     C                   MOVEL     '24'          FUNKEY
188200970526     C                   END
188300970526     C     KEY           IFEQ      ROLLUP
188400970526     C                   MOVEL     'RU'          FUNKEY
188500970526     C                   END
188600970526     C     KEY           IFEQ      HELP
188700970526     C                   MOVEL     'HP'          FUNKEY
188800970526     C                   END
188900970526     C*
189000970526     C                   ENDSR
189100970526     C*------------------------------------------------------------------------*
189200970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
189300970526     C*------------------------------------------------------------------------*
189400970526     C     FUNGES        BEGSR
189500970526     C*
189600970526     C* PULISCO CAMPI DI LAVORO
189700970526     C                   Z-ADD     0             W                 5 0
189800970526     C*
189900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
190000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
190100970526     C     1             DO        24            I
190200970526     C                   MOVEL     FUD(I)        FUDRIG
190300970526     C*
190400970526     C* PULIZIA FUNZIONI PRECEDENTI
190500970526     C                   MOVEL     *BLANKS       FUV(I)
190600970526     C*
190700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
190800970526     C* (SALTA F23 E F24)
190900970526     C     FUDATT        IFEQ      'S'
191000970526     C     FUDFUN        ANDNE     '23'
191100970526     C     FUDFUN        ANDNE     '24'
191200970526     C                   ADD       1             W
191300970526     C                   MOVEL     FUDDES        FUV(W)
191400970526     C                   END
191500970526     C                   END                                                    DO WHILE
191600970526     C*
191700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
191800970526     C* ALTRIMENTI DISATTIVO F24
191900970526     C     1             DO        24            I
192000970526     C                   MOVEL     FUD(I)        FUDRIG
192100970526     C*
192200970526     C     FUDFUN        IFEQ      '24'
192300970526     C     W             IFGT      6
192400970526     C                   MOVEL     'S'           FUDATT
192500970526     C                   ELSE
192600970526     C                   MOVEL     *BLANKS       FUDATT
192700970526     C                   END                                                    W GT 8
192800970526     C                   MOVEL     FUDRIG        FUD(I)
192900970526     C                   END
193000970526     C                   END                                                    DO WHILE
193100970526     C*
193200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
193300970526     C     W             DIV       6             I
193400970526     C                   MVR                     Z
193500970526     C*
193600970526     C     Z             IFNE      0
193700970526     C     I             ADD       1             Z
193800970526     C                   ELSE
193900970526     C                   Z-ADD     I             Z
194000970526     C                   END
194100970526     C*
194200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
194300970526     C                   ADD       1             FUNGRU
194400970526     C     FUNGRU        IFGT      Z
194500970526     C     FUNGRU        ORLT      1
194600970526     C                   Z-ADD     1             FUNGRU
194700970526     C                   END
194800970526     C*
194900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
195000970526     C     FUNGRU        IFEQ      1
195100970526     C                   MOVEL     FUV(1)        FUN001
195200970526     C                   MOVEL     FUV(2)        FUN002
195300970526     C                   MOVEL     FUV(3)        FUN003
195400970526     C                   MOVEL     FUV(4)        FUN004
195500970526     C                   MOVEL     FUV(5)        FUN005
195600970526     C                   MOVEL     FUV(6)        FUN006
195700970526     C                   END
195800970526     C*
195900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
196000970526     C     FUNGRU        IFEQ      2
196100970526     C                   MOVEL     FUV(7)        FUN001
196200970526     C                   MOVEL     FUV(8)        FUN002
196300970526     C                   MOVEL     FUV(9)        FUN003
196400970526     C                   MOVEL     FUV(10)       FUN004
196500970526     C                   MOVEL     FUV(11)       FUN005
196600970526     C                   MOVEL     FUV(12)       FUN006
196700970526     C                   END
196800970526     C*
196900970526     C* TERZO GRUPPO DI 6 FUNZIONI
197000970526     C     FUNGRU        IFEQ      3
197100970526     C                   MOVEL     FUV(13)       FUN001
197200970526     C                   MOVEL     FUV(14)       FUN002
197300970526     C                   MOVEL     FUV(15)       FUN003
197400970526     C                   MOVEL     FUV(16)       FUN004
197500970526     C                   MOVEL     FUV(17)       FUN005
197600970526     C                   MOVEL     FUV(18)       FUN006
197700970526     C                   END
197800970526     C*
197900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
198000970526     C     FUNGRU        IFEQ      4
198100970526     C                   MOVEL     FUV(19)       FUN001
198200970526     C                   MOVEL     FUV(20)       FUN002
198300970526     C                   MOVEL     FUV(21)       FUN003
198400970526     C                   MOVEL     FUV(22)       FUN004
198500970526     C                   MOVEL     *BLANKS       FUN005
198600970526     C                   MOVEL     *BLANKS       FUN006
198700970526     C                   END
198800970526     C*
198900970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
199000970526     C                   MOVEL     *BLANKS       FUN007
199100970526     C                   MOVEL     *BLANKS       FUN008
199200970526     C*
199300970526     C     1             DO        24            I
199400970526     C                   MOVEL     FUD(I)        FUDRIG
199500970526     C     FUDFUN        IFEQ      '23'
199600970526     C     FUDATT        ANDEQ     'S'
199700970526     C                   MOVEL     FUDDES        FUN007
199800970526     C                   END
199900970526     C     FUDFUN        IFEQ      '24'
200000970526     C     FUDATT        ANDEQ     'S'
200100970526     C                   MOVEL     FUDDES        FUN008
200200970526     C                   END
200300970526     C                   END                                                    DO
200400970526     C*
200500970526     C                   ENDSR
200600970526     C*------------------------------------------------------------------------*
200700970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
200800970526     C*------------------------------------------------------------------------*
200900970526     C     OPZGES        BEGSR
201000970526     C*
201100970526     C* PULISCO CAMPI DI LAVORO
201200970526     C                   Z-ADD     0             W                 5 0
201300970526     C*
201400970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
201500970526     C* E CARICO TABELLA OPZIONI ATTIVE
201600970526     C     1             DO        24            I
201700970526     C                   MOVEL     OPD(I)        OPDRIG
201800970526     C*
201900970526     C* PULIZIA OPZIONI PRECEDENTI
202000970526     C                   MOVEL     *BLANKS       OPV(I)
202100970526     C*
202200970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
202300970526     C     OPDATT        IFEQ      'S'
202400970526     C                   ADD       1             W
202500970526     C                   MOVEL     OPDDES        OPV(W)
202600970526     C                   END
202700970526     C                   END                                                    DO WHILE
202800970526     C*
202900970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
203000970526     C* ALTRIMENTI DISATTIVO F23
203100970526     C     1             DO        24            I
203200970526     C                   MOVEL     FUD(I)        FUDRIG
203300970526     C*
203400970526     C     FUDFUN        IFEQ      '23'
203500970526     C     W             IFGT      8
203600970526     C                   MOVEL     'S'           FUDATT
203700970526     C                   ELSE
203800970526     C                   MOVEL     *BLANKS       FUDATT
203900970526     C                   END                                                    W GT 8
204000970526     C                   MOVEL     FUDRIG        FUD(I)
204100970526     C                   END
204200970526     C                   END                                                    DO WHILE
204300970526     C*
204400970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
204500970526     C     W             DIV       8             I
204600970526     C                   MVR                     Z                 5 0
204700970526     C*
204800970526     C     Z             IFNE      0
204900970526     C     I             ADD       1             Z
205000970526     C                   ELSE
205100970526     C                   Z-ADD     I             Z
205200970526     C                   END
205300970526     C*
205400970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
205500970526     C                   ADD       1             OPZGRU
205600970526     C     OPZGRU        IFGT      Z
205700970526     C     OPZGRU        ORLT      1
205800970526     C                   Z-ADD     1             OPZGRU
205900970526     C                   END
206000970526     C*
206100970526     C* PRIMO GRUPPO DI 8 OPZIONI
206200970526     C     OPZGRU        IFEQ      1
206300970526     C                   MOVEL     OPV(1)        OPZ001
206400970526     C                   MOVEL     OPV(2)        OPZ002
206500970526     C                   MOVEL     OPV(3)        OPZ003
206600970526     C                   MOVEL     OPV(4)        OPZ004
206700970526     C                   MOVEL     OPV(5)        OPZ005
206800970526     C                   MOVEL     OPV(6)        OPZ006
206900970526     C                   MOVEL     OPV(7)        OPZ007
207000970526     C                   MOVEL     OPV(8)        OPZ008
207100970526     C                   END
207200970526     C*
207300970526     C* SECONDO GRUPPO DI 8 OPZIONI
207400970526     C     OPZGRU        IFEQ      2
207500970526     C                   MOVEL     OPV(9)        OPZ001
207600970526     C                   MOVEL     OPV(10)       OPZ002
207700970526     C                   MOVEL     OPV(11)       OPZ003
207800970526     C                   MOVEL     OPV(12)       OPZ004
207900970526     C                   MOVEL     OPV(13)       OPZ005
208000970526     C                   MOVEL     OPV(14)       OPZ006
208100970526     C                   MOVEL     OPV(15)       OPZ007
208200970526     C                   MOVEL     OPV(16)       OPZ008
208300970526     C                   END
208400970526     C*
208500970526     C* TERZO GRUPPO DI 8 OPZIONI
208600970526     C     OPZGRU        IFEQ      3
208700970526     C                   MOVEL     OPV(17)       OPZ001
208800970526     C                   MOVEL     OPV(18)       OPZ002
208900970526     C                   MOVEL     OPV(19)       OPZ003
209000970526     C                   MOVEL     OPV(20)       OPZ004
209100970526     C                   MOVEL     OPV(21)       OPZ005
209200970526     C                   MOVEL     OPV(22)       OPZ006
209300970526     C                   MOVEL     OPV(23)       OPZ007
209400970526     C                   MOVEL     OPV(24)       OPZ008
209500970526     C                   END
209600970526     C*
209700970526     C                   ENDSR
209800970526     C*------------------------------------------------------------------------*
209900970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
210000970526     C*------------------------------------------------------------------------*
210100970526     C     OPZCON        BEGSR
210200970526     C*
210300970526     C* PULISCO CAMPI DI LAVORO
210400970526     C                   Z-ADD     0             I                 5 0
210500970526     C                   Z-ADD     0             W                 5 0
210600970526     C*
210700970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
210800970526     C     1             DO        24            I
210900970526     C                   MOVEL     OPD(I)        OPDRIG
211000970526     C*
211100970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
211200970526     C     OPZKEY        IFEQ      OPDOPZ
211300970526     C     OPZKEY        OREQ      OPDOP1
211400970526     C*
211500970526     C     OPDATT        IFEQ      'S'
211600970526     C                   MOVEL     OPDOPZ        OPZKEY
211700970526     C                   Z-ADD     I             W
211800970526     C                   END
211900970526     C                   END
212000970526     C*
212100970526     C                   END                                                    DO WHILE
212200970526     C*
212300970526     C* OPZIONE NON ATTIVA (ERRORE)
212400970526     C     W             IFEQ      0
212500970526     C*
212600970526     C* OPZIONE NON OK
212700970526     C                   Z-ADD     1             OPZOK
212800970526     C*
212900970526     C* EMISSIONE MESSAGGIO DI ERRORE
213000970526     C                   MOVEL     MSG(1)        DSMSMS
213100970526     C                   ELSE
213200970526     C*
213300970526     C* OPZIONE ATTIVA (PROCEDI ...)
213400970526     C*
213500970526     C     OPZKEY        IFEQ      ' 1'
213600970526     C                   EXSR      O01GES
213700970526     C                   END
213800970526     C     OPZKEY        IFEQ      ' 2'
213900970526     C                   EXSR      O02GES
214000970526     C                   END
214100970526     C     OPZKEY        IFEQ      ' 4'
214200970526     C                   EXSR      O04GES
214300970526     C                   END
214400970526     C     OPZKEY        IFEQ      ' 5'
214500970526     C                   EXSR      O05GES
214600970526     C                   END
214700970526     C     OPZKEY        IFEQ      ' 7'
214800970526     C                   EXSR      O07GES
214900970526     C                   END
215000060804     C     OPZKEY        IFEQ      'SC'
215100060804     C                   EXSR      OSCGES
215200060804     C                   END
215300970526     C                   END                                                    OPZ.NON ATTIVA
215400970526     C*
215500970526     C                   ENDSR
215600970526     C*------------------------------------------------------------------------*
215700970526     C* NOTE COSTRUZIONE SCHIERE
215800970526     C*------------------------------------------------------------------------*
215900970526     C*
216000970526     C* -MSG-MESSAGGI
216100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
216200970526     C* (NON TOCCARE MSG,1 E MSG,2)
216300970526     C*
216400970526     C* -CMD-COMANDI
216500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
216600970526     C*
216700970526     C* -OPD-OPZIONI
216800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
216900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
217000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
217100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
217200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
217300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
217400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
217500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
217600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
217700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
217800970526     C*
217900970526     C* -FUD-FUNZIONI
218000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
218100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
218200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
218300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
218400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
218500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
218600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
218700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
218800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
218900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
219000970526     O*------------------------------------------------------------------------*
219100970526** DEC - DECODIFICHE
219200970526    GESTIONE                                                     1
219300970526INTERROGAZIONE                                                   2
219400970530     SCELTA                                                      3
219500060929ScadenzaTerminal
2196009705265
219700970526** MSG - MESSAGGI
219800970528TISI17R- Opzione non gestita                                                   1
219900970528TISI17R- Tasto funzionale non gestito                                          2
220000970528TISI17R- Non abilitato alla gestione di questo CAP                             3
220100970528TISI17R- Record annullato. Consentito solo il ripristino                       4
220200970528TISI17R- Record non annullato. Impossibile ripristinare                        5
220300970528TISI17R- Accettato solo un parametro di inizio lista                           6
220400120913Opz. SC=Scadenza terminal: i record devono avere tipo A o P                    7
220500120913Opz. SC=Scadenza terminal: si deve selezionare per Data Decorrenza             8
220600120912La data scadenza nuovo terminal deve essere > scadenza terminal precedente     9
220700060929TISI17R- Terminal / linea errati                                              10
220800120913Opz. SC=Scadenza terminal: i record devono avere lo stesso tipo               11
220900970528TISI17R- Data errata                                                          12
221000120913Opz. SC=Scadenza terminal: i record non devono essere scaduti                 13
221100120921Nuova data scadenza non può essere > 31 gg dalla precedente                   14
221200120913Opz. SC=Scadenza terminal: i record devono essere gli ultimi a decorrere      15
221300120913La nuova scadenza deve essere < o = a tutte le scadenze selezionate           16
221400120921Data scadenza < oggi. ENTER forza                                             17
221500120921TISI17R- 18                                                                   18
221600970528TISI17R- 19                                                                   19
221700970528TISI17R- 20                                                                   20
221800970528** CMD - COMANDI CL
221900970528WRKSPLF
222000970528
222100970528
222200970528
222300970528
222400970526** OPD - OPZIONI
222500970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
222600970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
222700970526                   -3 -     (S/N)=N                        opz. 03¬
222800970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
222900970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
223000970526                   -6 -     (S/N)=N                        opz. 06¬
223100970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
223200970527                   -8 -     (S/N)=N                        opz. 08¬
223300970527                   -9 -     (S/N)=N                        opz. 09¬
223400060929SC=Scadenza termin -10-     (S/N)=N                        opz. 10¬
223500970526                   -  -     (S/N)=N                        opz. 11¬
223600970526                   -  -     (S/N)=N                        opz. 12¬
223700970526                   -  -     (S/N)=N                        opz. 13¬
223800970526                   -  -     (S/N)=N                        opz. 14¬
223900970526                   -  -     (S/N)=N                        opz. 15¬
224000970526                   -  -     (S/N)=N                        opz. 16¬
224100970526                   -  -     (S/N)=N                        opz. 17¬
224200970526                   -  -     (S/N)=N                        opz. 18¬
224300970526                   -  -     (S/N)=N                        opz. 19¬
224400970526                   -  -     (S/N)=N                        opz. 20¬
224500970526                   -  -     (S/N)=N                        opz. 21¬
224600970526                   -  -     (S/N)=N                        opz. 22¬
224700970526                   -  -     (S/N)=N                        opz. 23¬
224800970526                   -  -     (S/N)=N                        opz. 24¬
224900970526** FUD - FUNZIONI
225000970526                   -        (S/N)=N                        funz.01¬
225100970526                   -        (S/N)=N                        funz.02¬
225200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
225300970526                   -        (S/N)=N                        funz.04¬
225400970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
225500020506F06=Inserimento    -        (S/N)=S                        funz.06¬
225600970526                   -        (S/N)=N                        funz.07¬
225700970527                   -        (S/N)=N                        funz.08¬
225800970527                   -        (S/N)=N                        funz.09¬
225900970527                   -        (S/N)=N                        funz.10¬
226000970526                   -        (S/N)=N                        funz.11¬
226100970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
226200970526                   -        (S/N)=N                        funz.13¬
226300970526                   -        (S/N)=N                        funz.14¬
226400970526                   -        (S/N)=N                        funz.15¬
226500970526                   -        (S/N)=N                        funz.16¬
226600970526                   -        (S/N)=N                        funz.17¬
226700970526                   -        (S/N)=N                        funz.18¬
226800970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
226900970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
227000980512F21=Stampa elenco  -        (S/N)=S                        funz.21¬
227100970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
227200970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
227300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
