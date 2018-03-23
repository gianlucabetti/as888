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
005700130913
005800130913     D fil_e           S              3  0 DIM(8)
005900970526     D*------------------------------------------------------------------------*
006000970526     D* INPUT
006100970526     D*------------------------------------------------------------------------*
006200970526     D*------------
006300970526     D* RIDEFINIZIONE OPZIONI VIDEO
006400970526     D*------------
006500970526     D OPDDS           DS
006600970526     D  OPDOPZ                 1      2
006700970526     D  OPDDE1                 3     19
006800970526     D  OPDOP1                21     22
006900970526     D  OPDFI0                23     34
007000970526     D  OPDATT                35     35
007100970526     D  OPDFI1                36     80
007200970526     D  OPDDES                 1     19
007300970526     D  OPDRIG                 1     80
007400970526     D*------------
007500970526     D* RIDEFINIZIONE FUNZIONI VIDEO
007600970526     D*------------
007700970526     D FUDDS           DS
007800970526     D  FUDDE1                 1      1
007900970526     D  FUDFUN                 2      3
008000970526     D  FUDDE2                 4     19
008100970526     D  FUDFI0                20     34
008200970526     D  FUDATT                35     35
008300970526     D  FUDFI1                36     80
008400970526     D  FUDDES                 1     19
008500970526     D  FUDRIG                 1     80
008600970526     D*------------
008700970526     D* DS GESTIONE TASTI FUNZIONALI
008800970526     D*------------
008900970526     D DSKEY         E DS
009000970526     D DEVDS1          DS
009100970526     D  KEY                  369    369
009200970526     D*------------
009300970526     D* DS "XMSG" - GESTIONE MESSAGGI
009400970526     D*------------
009500970526     D DSMSG         E DS
009600970527     D*------------------
009700970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
009800970527     D*------------------
009900970527     D WLBDA8          DS                  INZ
010000970527     D  G08DAT                 1      8  0
010100970527     D  G08INV                 9     16  0
010200970527     D  G08ERR                17     17
010300970527     D  G08TGI                18     22  0
010400970526     D*------------
010500970526     D* DS DI PROCEDURA
010600970526     D*------------
010700970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
010800970526     D*------------
010900970526     D* ARCHITETTURA
011000970526     D*------------
011100970526     D KPJBA         E DS
011200180223      *------------
011300180223      * Richiamo *pgm gestione/selezione tab. "ETF"
011400180223      *------------
011500180223     d TNTB47ds      e ds                  inz
011600180223      *------------
011700120904     D*
011800060929     D SAVING          S                   LIKE(VIDING)
011900120921     D wSflUlt         S                   LIKE(sflult)
012000060929     D SAVepa          S                   LIKE(videpa)
012100120904     D Rip18R1         S              2
012200120904     D Err18R1         S              1
012300120904     D S11NrRcd        S              5  0
012400120904     D DDeV11          S                   like(V11DDe)
012500120904     D NwTFE           S                   like(CAETFE)
012600120921     D ForzaMsg17      S               n
012700120921     D Dsc11Vid_sav    S              8s 0
012800970526     C*------------------------------------------------------------------------*
012900970526     C* MAIN LINES
013000970526     C*------------------------------------------------------------------------*
013100970526     C*
013200970526     C* SE NON CI SONO ERRORI, PROSEGUE
013300970526IF  1C     ERRL00        IFNE      '1'
013400970526     C*
013500970526     C* CARICA IL SFILE
013600970526     C                   EXSR      POSSFL
013700970526     C*
013800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013900970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014200970526     C*
014300970526     C* REIMPOSTA ERRORE DI PROCERURA
014400970526     C                   MOVEL     '0'           ERRL00
014500970526     C*
014600970526     C* EMETTE IL SFILE
014700970526     C                   EXSR      OUTSFL
014800970526     C*
014900970526     C* GESTIONE TASTI FUNZIONALI
015000970526     C                   EXSR      FUNCON
015100970526     C*
015200970526E   2C                   ENDDO
015300970527     C*
015400970527     C* CI SONO ERRORI IN ENTRATA
015500970527X   1C                   ELSE
015600970527     C                   MOVEL     '1'           D00ERR
015700970527     C                   MOVEL     DSMSMS        D00MSG
015800970526E   1C                   ENDIF
015900970526     C* FINE PGM
016000970526     C                   SETON                                        LR
016100970526     C*------------------------------------------------------------------------*
016200970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
016300970526     C*------------------------------------------------------------------------*
016400970526     C     F03GES        BEGSR
016500970526     C*
016600970526     C* IMPOSTA "PREMUTO F03"
016700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016800970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
016900970526     C*
017000970526     C                   ENDSR
017100970526     C*------------------------------------------------------------------------*
017200970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
017300970526     C*------------------------------------------------------------------------*
017400970526     C     F05GES        BEGSR
017500970526     C*
017600970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017700970526     C*
017800970526     C                   ENDSR
017900970526     C*------------------------------------------------------------------------*
018000970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
018100970526     C*------------------------------------------------------------------------*
018200970526     C     F06GES        BEGSR
018300970526     C*
018400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
018500970602     C                   MOVEL     *BLANKS       DEPA
018600970602     C                   Z-ADD     *ZEROS        DETP
018700970602     C                   Z-ADD     *ZEROS        DETA
018800970602     C                   Z-ADD     *ZEROS        DEDD
018900970602     C                   MOVEL     DEPOP0        WOP0
019000970529     C                   MOVEL     'F06'         WOP1
019100970528     C                   MOVEL     'TISI18R'     WPGM              8
019200970526     C                   EXSR      LANPGM                                       *LANCIA PGM
019300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019400970526E   1C                   ENDIF
019500970526     C*
019600970526     C                   ENDSR
019700180223      *------------------------------------------------------------------------*
019800180223      * F09GES - GESTIONE F09 -> TRANSITO TERMINAL
019900180223      *------------------------------------------------------------------------*
020000180223     c     F09GES        BEGSR
020100180223      *
020200180223     c                   clear                   TNTB47ds
020300180223IF  1c                   if        DEPOP0 = 'P02'
020400180223     c                   eval      I47opz = '2'
020500180223     c                   else
020600180223     c                   eval      I47opz = '5'
020700180223     c                   endif
020800180223     c                   eval      KPJBU  = TNTB47ds
020900180223      *
021000180223     c                   call      'TNTB47R'
021100180223     c                   parm                    KPJBA
021200180223     c                   eval      TNTB47ds = KPJBU
021300180223      *
021400180223      * SE ritorna premuto F3, chiude il programma
021500180223IF  1c                   if        O47fxx = '03'                                *PREMUTO F03
021600180223     c                   movel     '1'           ERRL00                         *ERRORE
021700180223     c                   movel     '1'           F03L00                         *FINE PGM
021800180223X   1c                   else                                                   *NON PREMUTO F03
021900180223     c                   movel     O47msg        DSMSMS                         *MESSAGGIO DI RITORN
022000180223E   1c                   endif
022100180223      *
022200180223     c                   ENDSR
022300970526     C*------------------------------------------------------------------------*
022400970526     C* F12GES - GESTIONE F12 -> RITORNO
022500970526     C*------------------------------------------------------------------------*
022600970526     C     F12GES        BEGSR
022700970526     C*
022800970526     C* IMPOSTA "PREMUTO F12"
022900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023000970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
023100970526     C*
023200970526     C                   ENDSR
023300970528     C*------------------------------------------------------------------------*
023400970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
023500970528     C*------------------------------------------------------------------------*
023600970528     C     F21GES        BEGSR
023700970612     C*
023800980206     C                   MOVEL     *BLANKS       DEPA
023900980206     C                   Z-ADD     *ZEROS        DETP
024000980206     C                   Z-ADD     *ZEROS        DETA
024100980206     C                   Z-ADD     *ZEROS        DEDD
024200970612     C                   MOVEL     DEPOP0        WOP0
024300970612     C                   MOVE      '03'          WOP0
024400970612     C                   MOVEL     'F21'         WOP1
024500970612     C                   MOVEL     'TISI19R'     WPGM              8
024600970612     C                   EXSR      LANPGM
024700970528     C*
024800970528     C                   ENDSR
024900970528     C*------------------------------------------------------------------------*
025000970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
025100970528     C*------------------------------------------------------------------------*
025200970528     C     F22GES        BEGSR
025300970528     C*
025400970528     C                   Z-ADD     80            LENGH            15 5
025500970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
025600970528     C                   PARM                    CMD(1)
025700970528     C                   PARM                    LENGH
025800970528     C*
025900970528     C                   ENDSR
026000970526     C*------------------------------------------------------------------------*
026100970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
026200970526     C*------------------------------------------------------------------------*
026300970526     C     F23GES        BEGSR
026400970526     C*
026500970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
026600970526     C*
026700970526     C                   ENDSR
026800970526     C*------------------------------------------------------------------------*
026900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
027000970526     C*------------------------------------------------------------------------*
027100970526     C     F24GES        BEGSR
027200970526     C*
027300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
027400970526     C*
027500970526     C                   ENDSR
027600970526     C*------------------------------------------------------------------------*
027700970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
027800970526     C*------------------------------------------------------------------------*
027900970526     C     RUPGES        BEGSR
028000970526     C*
028100970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
028200970526     C*
028300970526     C                   ENDSR
028400970526     C*------------------------------------------------------------------------*
028500970526     C* ENTGES - GESTIONE ENTER -> INVIO
028600970526     C*------------------------------------------------------------------------*
028700970526     C     ENTGES        BEGSR
028800970526     C*
028900970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
029000970526     C                   EXSR      NEWPOS
029100970526     C*
029200970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
029300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029400970526IF  2C     SWPOSI        IFNE      0
029500970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
029600970526X   2C                   ELSE
029700970526IF  3C     SFLULT        IFNE      0
029800970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
029900970526     C*
030000970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
030100970526IF  4C     SWMODI        IFNE      0
030200970526     C                   EXSR      POSSFL
030300970526E   4C                   ENDIF
030400970526E   3C                   ENDIF
030500970526E   2C                   ENDIF
030600970526E   1C                   ENDIF
030700970526     C*
030800970526     C                   ENDSR
030900970526     C*------------------------------------------------------------------------*
031000970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
031100970526     C*------------------------------------------------------------------------*
031200970526     C     O01GES        BEGSR
031300970526     C*
031400970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
031500970528     C                   MOVEL     VIDEPA        D00EPA
031600970602     C                   Z-ADD     VIDTFP        D00ETP
031700970528     C                   Z-ADD     VIDTFA        D00ETA
031800970528     C                   Z-ADD     VIHDDE        D00EDD
031900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
032000970526     C*
032100970526     C                   ENDSR
032200970526     C*------------------------------------------------------------------------*
032300970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
032400970526     C*------------------------------------------------------------------------*
032500970526     C     O02GES        BEGSR
032600970526     C*
032700970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
032800970526IF  1C     VIDATB        IFEQ      'A'
032900970526     C                   MOVEL     '1'           ERRL00
033000970526     C                   MOVEL     MSG(4)        DSMSMS
033100970526E   1C                   ENDIF
033200970527     C* MODIFICA
033300970526IF  1C     ERRL00        IFEQ      '0'
033400970602     C                   MOVEL     VIDEPA        DEPA
033500970602     C                   Z-ADD     VIDTFP        DETP
033600970602     C                   Z-ADD     VIDTFA        DETA
033700970602     C                   Z-ADD     VIHDDE        DEDD
033800970602     C                   MOVEL     DEPOP0        WOP0
033900970526     C                   MOVEL     'O02'         WOP1
034000970528     C                   MOVEL     'TISI18R'     WPGM
034100970526     C                   EXSR      LANPGM
034200970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
034300131015     c                   eval      swmodi=1
034400970526E   1C                   ENDIF
034500970526     C*
034600970526     C                   ENDSR
034700970526     C*------------------------------------------------------------------------*
034800970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
034900970526     C*------------------------------------------------------------------------*
035000970526     C     O04GES        BEGSR
035100970526     C*
035200970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
035300970526IF  1C     VIDATB        IFEQ      'A'
035400970526     C                   MOVEL     '1'           ERRL00
035500970526     C                   MOVEL     MSG(4)        DSMSMS
035600970526E   1C                   ENDIF
035700970527     C* ANNULLAMENTO
035800970526IF  1C     ERRL00        IFEQ      '0'
035900970602     C                   MOVEL     VIDEPA        DEPA
036000970602     C                   Z-ADD     VIDTFP        DETP
036100970602     C                   Z-ADD     VIDTFA        DETA
036200970602     C                   Z-ADD     VIHDDE        DEDD
036300970602     C                   MOVEL     DEPOP0        WOP0
036400970526     C                   MOVEL     'O04'         WOP1
036500970528     C                   MOVEL     'TISI18R'     WPGM
036600970526     C                   EXSR      LANPGM
036700970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
036800970526E   1C                   ENDIF
036900970526     C*
037000970526     C                   ENDSR
037100970526     C*------------------------------------------------------------------------*
037200970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
037300970526     C*------------------------------------------------------------------------*
037400970526     C     O05GES        BEGSR
037500970526     C*
037600970602     C                   MOVEL     VIDEPA        DEPA
037700970602     C                   Z-ADD     VIDTFP        DETP
037800970602     C                   Z-ADD     VIDTFA        DETA
037900970602     C                   Z-ADD     VIHDDE        DEDD
038000970602     C                   MOVEL     DEPOP0        WOP0
038100970526     C                   MOVEL     'O05'         WOP1
038200970528     C                   MOVEL     'TISI18R'     WPGM
038300970526     C                   EXSR      LANPGM
038400970526     C*
038500970526     C                   ENDSR
038600970526     C*------------------------------------------------------------------------*
038700970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
038800970526     C*------------------------------------------------------------------------*
038900970526     C     O07GES        BEGSR
039000970526     C*
039100970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
039200970526IF  1C     VIDATB        IFNE      'A'
039300970526     C                   MOVEL     '1'           ERRL00
039400970526     C                   MOVEL     MSG(5)        DSMSMS
039500970526E   1C                   ENDIF
039600970526     C* RIPRISITINO
039700970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039800970602     C                   MOVEL     VIDEPA        DEPA
039900970602     C                   Z-ADD     VIDTFP        DETP
040000970602     C                   Z-ADD     VIDTFA        DETA
040100970602     C                   Z-ADD     VIHDDE        DEDD
040200970602     C                   MOVEL     DEPOP0        WOP0
040300970526     C                   MOVEL     'O07'         WOP1
040400970528     C                   MOVEL     'TISI18R'     WPGM
040500970526     C                   EXSR      LANPGM
040600970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
040700970526E   1C                   ENDIF
040800970526     C*
040900970526     C                   ENDSR
041000131009     C*----------------------------------------------------------------------------*
041100131015     C* Op1GES - GESTIONE OPZIONE "+1" =Postocipa decorrenza
041200131009     C*----------------------------------------------------------------------------*
041300131009     C     OP1GES        BEGSR
041400131009   0ac                   if        opzkey = '+1'
041500131009     c* Do errore se:
041600131009     c* - se il rcd è annullato logicamente
041700131009     c                   IF        VidATB <> *blank
041800131009     C                   MOVEL     '1'           ERRL00
041900131009     C                   MOVEL     MSG(4)        DSMSMS
042000131015     c                   movel     '1'           opzok
042100131015     c                   leavesr
042200131009E   1C                   ENDIF
042300131009     c* - il tipo terminal è <> da 'A' e 'P'
042400131009     c                   IF        (VidEPA <> 'A' and
042500131009     c                              VidEPA <> 'P')
042600131009     C                   MOVEL     '1'           ERRL00
042700131009     C                   MOVEL     MSG(7)        DSMSMS
042800131015     c                   movel     '1'           opzok
042900131015     c                   leavesr
043000131009E   1C                   ENDIF
043100131009     c* - se il rcd è scaduto
043200131009     c                   IF        %date(VidDSC:*eur) < %date()
043300131009     C                   MOVEL     '1'           ERRL00
043400131009     C                   MOVEL     MSG(13)       DSMSMS
043500131015     c                   movel     '1'           opzok
043600131015     c                   leavesr
043700131009E   1C                   ENDIF
043800131009     c* - se il rcd non è l'ultimo tra quelli di pari EPA/TFE/TFA/TFP
043900131009     C*   in questo modo cerco di evitare sovrapposizioni
044000131009     C                   MOVEL     VIDEPA        CAEEPA
044100131009     C                   Z-ADD     VIDTFA        CAETFA
044200131009     C                   Z-ADD     VIDTFP        CAETFP
044300131009     C* mi posiziono a quello letto sul sfl impostando la sua data decorrenza
044400131009     c                   EVAL      CAEDDE=%dec(%date(VidDDe:*eur):*iso)
044500131009     C     K04CAE01      CHAIN     AZCAE01L
044600131009     C                   DOU       %eof(AZCAE01L)
044700131009     C     K03CAE01      READE     AZCAE01L
044800131009     C* se leggo un rcd con EPA/TFE/TFA/TFE vuol dire solo che ha una decorrenza > , quindi non è
044900131009     C* l'ultimo
045000131009     C* non considero gli annullati
045100131009     c                   IF        not %eof and
045200131009     C                             CAEATB = *blank
045300131009     C                   MOVEL     '1'           ERRL00
045400131009     C                   MOVEL     MSG(15)       DSMSMS
045500131015     c                   movel     '1'           opzok
045600131015     c                   leavesr
045700131009     C                   ENDIF
045800131009     C                   ENDDO
045900131009     c* ogni volta che ho un errore esco dal ciclo principale, quindi se dal ciclo precedente
046000131009     C* sono uscito con errore, devo uscire anche dal ciclo principale
046100131009     C                   IF        ERRL00 = '1'
046200131015     c                   leavesr
046300131009E   1C                   ENDIF
046400131009     c*
046500131015     c* se non ho avuto errori richiamo aggiornamento
046600131015    1C                   if        ERRL00 <> '1'
046700131015     C                   MOVEL     VIDEPA        DEPA
046800131015     C                   Z-ADD     VIDTFP        DETP
046900131015     C                   Z-ADD     VIDTFA        DETA
047000131015     C                   Z-ADD     VIHDDE        DEDD
047100131015     C                   MOVEL     DEPOP0        WOP0
047200131015     C                   MOVEL     'O+1'         WOP1
047300131015     C                   MOVEL     'TISI18R'     WPGM
047400131015     C                   EXSR      LANPGM
047500131015     c                   eval      swmodi=1
047600131009    1c                   endif
047700131015   0ac                   endif
047800131009     C                   ENDSR
047900131009     C*----------------------------------------------------------------------------*
048000131009     C* OSCGES - GESTIONE OPZIONE SC= scadenza terminal
048100131009     C*----------------------------------------------------------------------------*
048200060804     C     OSCGES        BEGSR
048300120912     c* Per poter dare l'opzione SC si deve avere la selezione per data decorrenza
048400120913     C* in modo che sia più difficile (non impossibile) che l'utente crei delle sovrapposizioni
048500120912     c***                if        vidppa=' ' or  vidpdd=0
048600130116     C**                 IF        VIDPDD=0
048700130116     C**                 MOVEL     '1'           ERRL00
048800130116     C**                 MOVEL     MSG(8)        DSMSMS
048900120913     C* segnalo al resto del pgm che l'opzione ha dato un errore
049000130116     c**                 movel     '1'           opzok
049100120913     C* esco dalla routine
049200130116     C**                 LEAVESR
049300120913     c***                goto      endscade
049400130116     c**                 ENDIF
049500060929     c
049600120921     C                   EVAL      ForzaMsg17 = *off
049700120921     C                   EVAL      Dsc11Vid_sav = 0
049800060928     c* Leggo dal sfl tutti i record che hanno "SC" e devono avere
049900060928     c*  lo stesso tipo terminal
050000060929     c                   eval      savING=viding
050100060929     c                   movel     dec(4)        viding
050200060929     c
050300060929     c                   clear                   nrr               5 0
050400060929     c                   clear                   savepa
050500060929     c* pulisco il sfl di scadenza terminal
050600060928     c                   seton                                        70
050700060929     c                   write     si17c11
050800060928     c                   setoff                                       70
050900120913     C*
051000120921     c                   do        wflult        wSflUlt
051100120921     c     wSflUlt       chain     si17d05
051200131009     c                   if        opzkey='SC'
051300120903     c* La prima volta salvo alcuni dati del record
051400120903     c                   if        savepa=*blanks
051500120903     c                   eval      savepa=videpa
051600120903     c                   eval      v11epa=videpa
051700120903E   1C                   ENDIF
051800120903     c* Do errore se:
051900120921     c* - se il rcd è annullato logicamente
052000120921     c                   IF        VidATB <> *blank
052100120904     C                   MOVEL     '1'           ERRL00
052200120921     C                   MOVEL     MSG(4)        DSMSMS
052300120912     c                   leave
052400120904E   1C                   ENDIF
052500120921     c* - il tipo terminal è <> da 'A' e 'P'
052600120921     c                   IF        (VidEPA <> 'A' and
052700120921     c                              VidEPA <> 'P')
052800120921     C                   MOVEL     '1'           ERRL00
052900120921     C                   MOVEL     MSG(7)        DSMSMS
053000120921     c                   leave
053100120921E   1C                   ENDIF
053200120903     c* - il tipo terminal è <> dal precedente (deve essere 1 solo tipo)
053300120904     c                   IF        savepa<>VidEPA
053400120904     C                   MOVEL     '1'           ERRL00
053500120904     C                   MOVEL     MSG(11)       DSMSMS
053600120912     c                   leave
053700120904E   1C                   ENDIF
053800120903     c* - se il rcd è scaduto
053900120904     c                   IF        %date(VidDSC:*eur) < %date()
054000060928     C                   MOVEL     '1'           ERRL00
054100120904     C                   MOVEL     MSG(13)       DSMSMS
054200120912     c                   leave
054300060928E   1C                   ENDIF
054400120921     c* - se il rcd non è l'ultimo tra quelli di pari EPA/TFE/TFA/TFP
054500120913     C*   in questo modo cerco di evitare sovrapposizioni
054600120913     C                   MOVEL     VIDEPA        CAEEPA
054700120924     C***                Z-ADD     VIDTFE        CAETFE
054800120921     C                   Z-ADD     VIDTFA        CAETFA
054900120924     C                   Z-ADD     VIDTFP        CAETFP
055000120921     C* mi posiziono a quello letto sul sfl impostando la sua data decorrenza
055100120913     c                   EVAL      CAEDDE=%dec(%date(VidDDe:*eur):*iso)
055200120924     C     K04CAE01      CHAIN     AZCAE01L
055300120924     C                   DOU       %eof(AZCAE01L)
055400120924     C     K03CAE01      READE     AZCAE01L
055500120921     C* se leggo un rcd con EPA/TFE/TFA/TFE vuol dire solo che ha una decorrenza > , quindi non è
055600120913     C* l'ultimo
055700120921     C* non considero gli annullati
055800120921     c                   IF        not %eof and
055900120921     C                             CAEATB = *blank
056000120913     C                   MOVEL     '1'           ERRL00
056100120913     C                   MOVEL     MSG(15)       DSMSMS
056200120913     c                   leave
056300120921     C                   ENDIF
056400120921     C                   ENDDO
056500120924     c* ogni volta che ho un errore esco dal ciclo principale, quindi se dal ciclo precedente
056600120924     C* sono uscito con errore, devo uscire anche dal ciclo principale
056700120924     C                   IF        ERRL00 = '1'
056800120924     c                   leave
056900120924E   1C                   ENDIF
057000060929     c
057100120904     c* se sono qui NON ho errori pre emissione
057200120913     C* per cui scrivo un rcd del sfl dei terminal da far scadere
057300120904     c                   movel     vidtfp        v11tfp                         filiale partenza
057400060929     c                   movel     viddtp        v11dtp
057500120904     c                   movel     vidtfa        v11tfa                         filiale arrivo
057600060929     c                   movel     viddta        v11dta
057700120904     c                   movel     vidtfe        v11tfe                         filiale eccezione
057800060929     c                   movel     viddte        v11dte
057900120912     c                   EVAL      V11DDe = VidDDe                              data decorrenza H
058000120921     c                   EVAL      V11SDSc = VidDSc                             data scadenza
058100060929     c                   add       1             nrr
058200060929     c                   write     si17s11
058300060929     c                   endif
058400060929     c                   enddo
058500060928     c*
058600120912     c* se non ho avuto errori durante il caricamento del sfl
058700120912     C                   IF        ERRL00 <> '1'
058800120904     c* salvo il nr.rcd di questo sfl
058900120904     C                   EVAL      s11nrrcd = nrr
059000120904     c*
059100060929     c                   z-add     1             rec1
059200060929     c                   eval      v11nwrip='SI'
059300060929     c                   clear                   v11dsc
059400060929     c                   clear                   v11nwdsc
059500060929     c                   clear                   v11nwtfe
059600120912     c                   clear                   v11nwdte
059700060929     c
059800120904     c* se non ci sono errori o non ho premuto F12, ciclo
059900130913     c                   DOU       (*inkf = *on and *in28=*off) or *inkl=*on
060000120904     c                   write     si17d01
060100120904     c                   write     si17z11
060200060929     c                   exfmt     si17c11
060300060929     c                   setoff                                       2890
060400060929     c                   setoff                                       646566
060500060929     C                   MOVEL     '0'           ERRL00
060600060929     c* F12 - ritorno
060700060929     c                   if        *inkl
060800120912     c***                goto      endscade
060900120912     c                   LEAVE
061000060929     c                   endif
061100060929     c* controllo le selezioni fatte
061200060929     c                   exsr      CTRSCADE
061300060929     c
061400120904     c* se non ci sono errori e ho premuto F6
061500120904     c                   IF        ErrL00 <> *on and
061600120904     c                             *inkf
061700120904     c* leggo il sfl ed effettuo le modifiche
061800120904     c                   EXSR      GenNewScad
061900130913     c                   if        fil_e(1) > 0
062000130913     c                   seton                                        28
062100130913     c                   eval      vidmsg=msg(18)
062200130913     c                   z-add     1             EE
062300130913     c                   z-add     43            xx                4 0
062400130913     c                   dow       fil_e(EE)>0
062500130913     c                   add       4             xx
062600130913     c                   eval      %subst(vidmsg:xx:3)=%editc(fil_e(EE):'X')
062700130913     c                   add       1             EE
062800130913     c                   enddo
062900130913     c                   endif
063000130913     c
063100130913     c
063200060929     c                   endif
063300120904     c                   ENDDO
063400060928     c
063500120913     C* se ho avuto errori durante il caricamento del sfl
063600120913     C                   ELSE
063700120913     C* riciclo per riaccendere il sflnxtchg nelle righe già lette avento un'opzione SC
063800120921     c                   DO        RigaLetta     wSflUlt
063900120921     c     wSflUlt       chain     si17d05
064000120913     c                   if        opzkey='SC'
064100120913     C* riaccendo il sflnxtchg
064200120913     C                   SETON                                        41
064300120913     C                   UPDATE    SI17D05
064400120913E   1C                   ENDIF
064500120913E   1C                   ENDDO
064600120913     C* rileggo la riga che ha dato errore perché serve letta al resto del pgm
064700120913     c     RigaLetta     chain     si17d05
064800120913     C*
064900120912     C                   ENDIF
065000120912     c
065100120913     c***  endscade      tag
065200060929     c
065300120912     c                   if        dsmsms=*blank
065400060929     c                   movel     saving        viding
065500120921     c***                z-add     savult        sflult
065600060929     c                   movel     '1'           swmodi
065700131015     c                   eval      swmodi =1
065800120912     c                   else
065900120912     c                   movel     '1'           opzok
066000120912     c                   endif
066100120904     C*
066200120904     C                   ENDSR
066300120904     C*------------------------------------------------------------------------*
066400120904     C* CONTROLLO DATI PER SCADENZA TERMINAL
066500120904     C*------------------------------------------------------------------------*
066600120904     C     CTRSCADE      BEGSR
066700120904     C*
066800120904     C* Ctrl formale nuova data scadenza x tutte le linee
066900120904     C     *LIKE         DEFINE    VIDDSC        DSC11VID
067000060929IF  1C     ERRL00        IFEQ      '0'
067100120904     C                   Z-ADD     V11DSC        WDA8
067200060929     C                   Z-ADD     *ZEROS        DA8W
067300060929     C                   EXSR      CHKDA8
067400120904     C                   Z-ADD     WDA8          V11DSC
067500120904     C                   Z-ADD     DA8W          DSC11VID
067600060929IF  2C     ERRL00        IFEQ      '1'
067700060929     C                   SETON                                        6428
067800060929     C                   MOVEL     MSG(12)       vidmsg
067900120904     c                   LEAVESR
068000120921     C                   ELSE
068100120921     C* memorizzo la nuova data scadenza se diversa dall'ultima scritta
068200120921     C                   IF        Dsc11Vid <> Dsc11Vid_sav
068300120921     C                   EVAL      Dsc11Vid_sav = Dsc11Vid
068400120921     C                   EVAL      ForzaMsg17 = *off
068500120921     C                   ENDIF
068600120921     C* se data corretta formalmente, deve essere > o = a oggi ma ctrl forzabile
068700120921IF  3C                   IF        %date(V11DSC:*eur) < %date()
068800120921     C                             and ForzaMsg17 = *off
068900120921     C                   EVAL      ForzaMsg17 = *on
069000120921     C                   SETON                                        6428
069100120921     C                   MOVEL     MSG(17)       vidmsg
069200120921E   3C                   ENDIF
069300060929E   2C                   ENDIF
069400060929E   1C                   ENDIF
069500060929     c
069600120904     c* Ctrl formale data scadenza nuova associazione
069700120904     C     *LIKE         DEFINE    V11nwDSC      DSC11NwVID
069800060929IF  1C     ERRL00        IFEQ      '0'
069900060929     C                   Z-ADD     V11nwDSC      WDA8
070000060929     C                   Z-ADD     *ZEROS        DA8W
070100060929     C                   EXSR      CHKDA8
070200060929     C                   Z-ADD     WDA8          V11nwDSC
070300120904     C                   Z-ADD     DA8W          DSC11NwVID
070400060929IF  2C     ERRL00        IFEQ      '1'
070500060929     C                   SETON                                        6528
070600060929     C                   MOVEL     MSG(12)       vidmsg
070700120904     c                   LEAVESR
070800120921E   2C                   ENDIF
070900060929E   1C                   ENDIF
071000060929     c
071100120912     C* La data scadenza nuovo terminal deve essere > scadenza terminal precedente
071200120904    1c                   if        dsc11nwvid<=dsc11vid
071300120913     C                   SETON                                        6528
071400060929     C                   MOVEL     MSG(9)        vidmsg
071500120912     C* precedente msg, ora sostituito
071600120912     C* TISI17R- La nuova data decorrenza deve essere maggiore della prima             9
071700120904     C                   MOVEL     '1'           ERRL00
071800120904     c                   LEAVESR
071900120904E   1C                   ELSE
072000120904     c
072100120904     c* se richiesto il ripristino precedente associazione
072200120904     c* la data scadenza nuova associazione non deve essere > nuova data scadenza di più di 1 mese
072300120904     c* (facciamo 31 gg e non 1 mese variabile) altrimenti non è una cosa temporanea
072400120904     C                   IF        V11NwRip = 'SI'              and
072500120904     C                             %dec(%date(dsc11nwvid):*jul) -
072600120904     C                             %dec(%date(dsc11vid):*jul)   >
072700120904     C                             31
072800120921     C                   SETON                                          6528
072900120904     C                   MOVEL     MSG(14)       vidmsg
073000120904     C                   MOVEL     '1'           ERRL00
073100120904     c                   LEAVESR
073200120904E   1C                   ENDIF
073300120904E   1C                   ENDIF
073400060929     C*--------------------
073500060929     C* TERMINAL / LINEA ECCEZIONE
073600060929     C*--------------------
073700060929IF  1C     ERRL00        IFEQ      '0'
073800060929     C                   MOVEL     V11nwTFE      WLIN              3
073900060929     C                   MOVEL     *BLANKS       WDLIN
074000060929     C                   EXSR      CHKLIN
074100060929     C                   MOVEL     WLIN          V11nwTFE
074200060929     C                   MOVEL     WDLIN         V11nwdtE
074300060929IF  2C     ERRL00        IFEQ      '1'
074400060929     C                   SETON                                        6628
074500060929     C                   MOVEL     MSG(10)       vidmsg
074600120904     c                   LEAVESR
074700060929E   2C                   ENDIF
074800060929E   1C                   ENDIF
074900120913     C*
075000120913     C* se siamo qui, i singoli campi del CTL sono corretto. Adesso controllo i rcd del SFL
075100120913     C* con i dati del CTL
075200120921     C                   DO        s11nrrcd      wSflUlt
075300120921     C     wSflUlt       chain     si17s11
075400120913     C*
075500120913     C* controllo la nuova data scadenza (la prima delle 2) sia  < = di quella presente sul SFL
075600120921     C                   IF        DSc11VID > %dec(%date(V11SDSc:*eur):*iso)
075700120913     C                   SETON                                        6428
075800120913     C                   MOVEL     MSG(16)       vidmsg
075900120913     c                   LEAVESR
076000120913E   2C                   ENDIF
076100120913     C*
076200120913E   1C                   ENDDO
076300060929     c
076400060929     C                   ENDSR
076500120913     C*------------------------------------------------------------------------*
076600120913     C* OSCGES_old - GESTIONE OPZIONE SC= scadenza terminal
076700120913     C*------------------------------------------------------------------------*
076800120913     C     OSCGES_O      BEGSR
076900120913     c* Per poter dare l'opzione SC si deve avere la selezione per data decorrenza
077000120913     C* in modo che sia più difficile (non impossibile) che l'utente crei delle sovrapposizioni
077100120913     c***                if        vidppa=' ' or  vidpdd=0
077200120913     C                   IF        VIDPDD=0
077300120913     C                   MOVEL     '1'           ERRL00
077400120913     C                   MOVEL     MSG(8)        DSMSMS
077500120913     c                   goto      endscade
077600120913     c                   ENDIF
077700120913     c
077800120913     c* Leggo dal sfl tutti i record che hanno "SC" e devono avere
077900120913     c*  lo stesso tipo terminal
078000120913     c                   eval      savING=viding
078100120913     c                   movel     dec(4)        viding
078200120913     c
078300120913     c                   z-add     1             sflult
078400120913     c                   clear                   nrr               5 0
078500120913     c                   clear                   savepa
078600120913     c* pulisco il sfl di scadenza terminal
078700120913     c                   seton                                        70
078800120913     c                   write     si17c11
078900120913     c                   setoff                                       70
079000120913     c
079100120913     c                   do        wflult        sflult
079200120913     c     sflult        chain     si17d05
079300120913     c                   if        opzkey='SC'
079400120913     c* La prima volta salvo alcuni dati del record
079500120913     c                   if        savepa=*blanks
079600120913     c                   eval      savepa=videpa
079700120913     c                   eval      v11epa=videpa
079800120913E   1C                   ENDIF
079900120913     c* Do errore se:
080000120913     c* - il tipo terminal è <> da 'A' e 'P'
080100120913     c                   IF        (VidEPA <> 'A' and
080200120913     c                              VidEPA <> 'P')
080300120913     C                   MOVEL     '1'           ERRL00
080400120913     C                   MOVEL     MSG(7)        DSMSMS
080500120913     c                   leave
080600120913E   1C                   ENDIF
080700120913     c* - il tipo terminal è <> dal precedente (deve essere 1 solo tipo)
080800120913     c                   IF        savepa<>VidEPA
080900120913     C                   MOVEL     '1'           ERRL00
081000120913     C                   MOVEL     MSG(11)       DSMSMS
081100120913     c                   leave
081200120913E   1C                   ENDIF
081300120913     c* - se il rcd è scaduto
081400120913     c                   IF        %date(VidDSC:*eur) < %date()
081500120913     C                   MOVEL     '1'           ERRL00
081600120913     C                   MOVEL     MSG(13)       DSMSMS
081700120913     c                   leave
081800120913E   1C                   ENDIF
081900120913     c
082000120913     c* se sono qui NON ho errori pre emissione
082100120913     c                   movel     vidtfp        v11tfp                         filiale partenza
082200120913     c                   movel     viddtp        v11dtp
082300120913     c                   movel     vidtfa        v11tfa                         filiale arrivo
082400120913     c                   movel     viddta        v11dta
082500120913     c                   movel     vidtfe        v11tfe                         filiale eccezione
082600120913     c                   movel     viddte        v11dte
082700120913     c                   EVAL      V11DDe = VidDDe                              data decorrenza H
082800120913     c                   EVAL      V11DSc = VidDSc                              data scadenza
082900120913     c                   add       1             nrr
083000120913     c                   write     si17s11
083100120913     c                   endif
083200120913     c                   enddo
083300120913     c*
083400120913     c* se non ho avuto errori durante il caricamento del sfl
083500120913     C                   IF        ERRL00 <> '1'
083600120913     c* salvo il nr.rcd di questo sfl
083700120913     C                   EVAL      s11nrrcd = nrr
083800120913     c*
083900120913     c                   z-add     1             rec1
084000120913     c                   eval      v11nwrip='SI'
084100120913     c                   clear                   v11dsc
084200120913     c                   clear                   v11nwdsc
084300120913     c                   clear                   v11nwtfe
084400120913     c                   clear                   v11nwdte
084500120913     c
084600120913     c* se non ci sono errori o non ho premuto F12, ciclo
084700120913     c                   DOU       *inkf = *on or *inkl = *on
084800120913     c                   write     si17d01
084900120913     c                   write     si17z11
085000120913     c                   exfmt     si17c11
085100120913     c                   setoff                                       2890
085200120913     c                   setoff                                       646566
085300120913     C                   MOVEL     '0'           ERRL00
085400120913     c* F12 - ritorno
085500120913     c                   if        *inkl
085600120913     c***                goto      endscade
085700120913     c                   LEAVE
085800120913     c                   endif
085900120913     c* controllo le selezioni fatte
086000120913     c                   exsr      CTRSCADE
086100120913     c
086200120913     c* se non ci sono errori e ho premuto F6
086300120913     c                   IF        ErrL00 <> *on and
086400120913     c                             *inkf
086500120913     c* leggo il sfl ed effettuo le modifiche
086600120913     c                   EXSR      GenNewScad
086700120913     c                   endif
086800120913     c                   ENDDO
086900120913     c
087000120913     C                   ENDIF
087100120913     c
087200120913     c     endscade      tag
087300120913     c
087400120913     c                   if        dsmsms=*blank
087500120913     c                   movel     saving        viding
087600120921     c******             z-add     savult        sflult
087700131015     c                   movel     1             swmodi
087800120913     c                   else
087900120913     c                   movel     '1'           opzok
088000120913     c                   endif
088100120913     C*
088200120913     C                   ENDSR
088300120913     C*------------------------------------------------------------------------*
088400120913     C* CONTROLLO DATI PER SCADENZA TERMINAL       OLD!
088500120913     C*------------------------------------------------------------------------*
088600120913     C     CTRSCADE_o    BEGSR
088700120913     C*
088800120913     C* Ctrl formale nuova data scadenza x tutte le linee
088900120913     C     *LIKE         DEFINE    VIDDSC        DSC11VID
089000120913IF  1C     ERRL00        IFEQ      '0'
089100120913     C                   Z-ADD     V11DSC        WDA8
089200120913     C                   Z-ADD     *ZEROS        DA8W
089300120913     C                   EXSR      CHKDA8
089400120913     C                   Z-ADD     WDA8          V11DSC
089500120913     C                   Z-ADD     DA8W          DSC11VID
089600120913IF  2C     ERRL00        IFEQ      '1'
089700120913     C                   SETON                                        6428
089800120913     C                   MOVEL     MSG(12)       vidmsg
089900120913     c                   LEAVESR
090000120913E   2C                   ENDIF
090100120913E   1C                   ENDIF
090200120913     c
090300120913     c* Ctrl formale data scadenza nuova associazione
090400120913     C     *LIKE         DEFINE    V11nwDSC      DSC11NwVID
090500120913IF  1C     ERRL00        IFEQ      '0'
090600120913     C                   Z-ADD     V11nwDSC      WDA8
090700120913     C                   Z-ADD     *ZEROS        DA8W
090800120913     C                   EXSR      CHKDA8
090900120913     C                   Z-ADD     WDA8          V11nwDSC
091000120913     C                   Z-ADD     DA8W          DSC11NwVID
091100120913IF  2C     ERRL00        IFEQ      '1'
091200120913     C                   SETON                                        6528
091300120913     C                   MOVEL     MSG(12)       vidmsg
091400120913     c                   LEAVESR
091500120913E   2C                   ENDIF
091600120913E   1C                   ENDIF
091700120913     c
091800120913     C* La data scadenza nuovo terminal deve essere > scadenza terminal precedente
091900120913    1c                   if        dsc11nwvid<=dsc11vid
092000120913     C                   SETON                                        646528
092100120913     C                   MOVEL     MSG(9)        vidmsg
092200120913     C* precedente msg, ora sostituito
092300120913     C* TISI17R- La nuova data decorrenza deve essere maggiore della prima             9
092400120913     C                   MOVEL     '1'           ERRL00
092500120913     c                   LEAVESR
092600120913E   1C                   ELSE
092700120913     c
092800120913     c* se richiesto il ripristino precedente associazione
092900120913     c* la data scadenza nuova associazione non deve essere > nuova data scadenza di più di 1 mese
093000120913     c* (facciamo 31 gg e non 1 mese variabile) altrimenti non è una cosa temporanea
093100120913     C                   IF        V11NwRip = 'SI'              and
093200120913     C                             %dec(%date(dsc11nwvid):*jul) -
093300120913     C                             %dec(%date(dsc11vid):*jul)   >
093400120913     C                             31
093500120913     C                   SETON                                        646528
093600120913     C                   MOVEL     MSG(14)       vidmsg
093700120913     C                   MOVEL     '1'           ERRL00
093800120913     c                   LEAVESR
093900120913E   1C                   ENDIF
094000120913E   1C                   ENDIF
094100120913     C*--------------------
094200120913     C* TERMINAL / LINEA ECCEZIONE
094300120913     C*--------------------
094400120913IF  1C     ERRL00        IFEQ      '0'
094500120913     C                   MOVEL     V11nwTFE      WLIN              3
094600120913     C                   MOVEL     *BLANKS       WDLIN
094700120913     C                   EXSR      CHKLIN
094800120913     C                   MOVEL     WLIN          V11nwTFE
094900120913     C                   MOVEL     WDLIN         V11nwdtE
095000120913IF  2C     ERRL00        IFEQ      '1'
095100120913     C                   SETON                                        6628
095200120913     C                   MOVEL     MSG(10)       vidmsg
095300120913     c                   LEAVESR
095400120913E   2C                   ENDIF
095500120913E   1C                   ENDIF
095600120913     c
095700120913     C                   ENDSR
095800120904     C*------------------------------------------------------------------------*
095900120904     C* Genero nuove scadenze a fronte dell'opz.SC
096000120904     C*------------------------------------------------------------------------*
096100120904     C     GenNewScad    BEGSR
096200120904     C*
096300130913     c                   clear                   fil_e
096400130913     c                   clear                   EE                4 0
096500120904     C* ciclo per tutti i rcd del sfl
096600120904     C                   DO        s11nrrcd      sflult
096700120904     C     sflult        chain     si17s11
096800120904     C*
096900120904     C* richiamo pgm che fa:
097000120904     C*
097100120904     C* modifico l'attuale data scadenza con quella a video
097200120904     C*
097300120904     C* scrivo una nuova associazione con:
097400120904     C* - terminal = quello indicato a video
097500120904     C* - data decorrenza = la scadenza impostata + 1
097600120904     C* - data scadenza = quella indicata a video come data scadenza nuova associazione
097700120904     C*
097800120904     C* se richiesto il ripristino associazione precedente,
097900120904     C* scrivo una nuova associazione con:
098000120904     C* - dati di partenza, arrivo e terminal = quelli su sfl
098100120904     C* - data decorrenza = la scadenza impostata sulla nuova associazione + 1
098200120904     C* - data scadenza = 31/12/2039
098300120904     C                   EVAL      DDeV11 = %dec(%date(V11DDe:*eur):*iso)
098400120904     C                   EVAL      NWTFE = %dec(V11NWTFE:3:0)
098500120904     C                   CALL      'TISI18R1'
098600120904     C                   PARM                    V11EPA
098700120904     C                   PARM                    V11TFP
098800120904     C                   PARM                    V11TFA
098900120904     C                   PARM                    DDeV11
099000120904     C                   PARM                    NwTFE
099100120904     C                   PARM                    DSC11Vid
099200120904     C                   PARM                    DSC11NwVid
099300120904     C                   PARM                    V11NwRip
099400120904     C                   PARM                    Err18R1
099500130913     c                   if        Err18r1='1'
099600130913     c                   add       1             EE
099700130913     c                   eval      fil_e(ee)=v11tfp
099800130913
099900130913     c                   if        v11tfp=0
100000130913     c                   eval      fil_e(ee)=v11tfa
100100130913     c                   endif
100200130913     c                   endif
100300130913
100400120904     C                   ENDDO
100500120904     C*
100600120904     C                   ENDSR
100700060929     C*------------------------------------------------------------------------*
100800060929     C* CHKDA8 - CONTROLLA LA DATA (8)
100900060929     C*------------------------------------------------------------------------*
101000060929     C     CHKDA8        BEGSR
101100060929     C*
101200060929     C     *LIKE         DEFINE    VIDDDE        WDA8
101300060929     C     *LIKE         DEFINE    VIDDDE        DA8W
101400060929     C*
101500060929     C                   Z-ADD     WDA8          G08DAT
101600060929     C                   Z-ADD     *ZEROS        G08INV
101700060929     C                   MOVEL     '0'           G08ERR
101800060929     C                   CALL      'XSRDA8'
101900060929     C                   PARM                    WLBDA8
102000060929     C                   Z-ADD     G08DAT        WDA8
102100060929     C                   Z-ADD     G08INV        DA8W
102200060929IF  1C     G08ERR        IFEQ      '1'
102300060929     C                   MOVEL     '1'           ERRL00
102400060929E   1C                   ENDIF
102500060929     C*
102600060929     C                   ENDSR
102700060929     C*------------------------------------------------------------------------*
102800060929     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
102900060929     C*------------------------------------------------------------------------*
103000060929     C     CHKLIN        BEGSR
103100060929     C*
103200060929     C     *LIKE         DEFINE    Viddta        WDLIN
103300060929     C*
103400060929     C* RICHIESTO ELENCO
103500120904     C***                SETOFF                                           28
103600120904     C***  '?'           SCAN      WLIN                                   28
103700120904IF  1C***  *IN28         IFEQ      *ON
103800120904IF  1C                   IF        %scan('?':wLin) > 0
103900060929     C                   MOVEL     *BLANKS       §COD1             3
104000060929     C                   MOVEL     *BLANKS       §TIP              1
104100060929     C                   MOVEL     *BLANKS       §DES1            25
104200060929     C                   MOVEL     *BLANKS       §PRA              3
104300060929     C                   CALL      'TNSD24R'
104400060929     C                   PARM                    §COD1
104500060929     C                   PARM                    §TIP
104600060929     C                   PARM                    §DES1
104700060929     C                   PARM                    §PRA
104800060929     C                   MOVEL     §COD1         WLIN
104900060929     C                   MOVEL     §DES1         WDLIN
105000060929E   1C                   ENDIF
105100060929     C*
105200060929     C* SOLO CARATTERI NUMERICI
105300120904     C                   SETOFF                                       66
105400120904     C                   TESTN                   WLIN                 66
105500120904IF  1C     *IN66         IFEQ      *OFF                                         *NON E'NUMERO
105600060929     C                   MOVEL     '1'           ERRL00
105700060929X   1C                   ELSE                                                   *E'NUMERO DA TESTN
105800060929     C                   MOVE      WLIN          W001              1
105900060929IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
106000120904     C                   SETON                                        66
106100060929     C                   MOVEL     '1'           ERRL00
106200060929X   2C                   ELSE                                                   *NUMERO
106300060929     C*
106400060929     C* CONTROLLA ESISTENZA CODICE
106500060929     C                   MOVEL     WLIN          KORFIL
106600060929     C     KEYORG        CHAIN     AZORG01L                           99
106700060929IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
106800060929     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
106900060929     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
107000060929     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
107100060929X   3C                   ELSE
107200060929     C                   MOVEL     '1'           ERRL00
107300060929E   3C                   ENDIF
107400060929E   2C                   ENDIF
107500060929E   1C                   ENDIF
107600060929     C*
107700060929     C                   ENDSR
107800970526     C*------------------------------------------------------------------------*
107900970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
108000970526     C*------------------------------------------------------------------------*
108100970526     C     NEWPOS        BEGSR
108200970526     C*
108300970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
108400970526     C*         1= RICHIESTO POSIZIONAMENTO
108500970526     C                   Z-ADD     0             SWPOSI            7 0
108600970526     C*
108700970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
108800970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
108900970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
109000970526     C                   Z-ADD     0             SWMODI            1 0
109100970530     C*
109200970530     C* REIMPOSTA INDICATORI DI ERRORE
109300970530     C                   SETOFF                                       808182
109400970530     C                   SETOFF                                       83
109500970526     C*
109600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
109700970528IF  1C     VIDPPA        IFNE      DEPEPA
109800970528     C     VIDPTP        ORNE      DEPTFP
109900970528     C     VIDPTA        ORNE      DEPTFA
110000980107     C     VIDPTE        ORNE      DEPTFE
110100970528     C     VIDPDD        ORNE      DEPDDE
110200970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
110300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
110400970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
110500970528     C                   MOVEL     VIDPPA        DEPEPA                         *DEPOSITI
110600970528     C                   Z-ADD     VIDPTP        DEPTFP
110700970528     C                   Z-ADD     VIDPTA        DEPTFA
110800980107     C                   Z-ADD     VIDPTE        DEPTFE
110900970528     C                   Z-ADD     VIDPDD        DEPDDE
111000970526E   2C                   ENDIF
111100970526E   1C                   ENDIF
111200970526     C*
111300970526     C                   ENDSR
111400970526     C*------------------------------------------------------------------------*
111500970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
111600970526     C*------------------------------------------------------------------------*
111700970528     C     SETFIL        BEGSR
111800970526     C*
111900970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
112000970528IF  1C     WTIPRI        IFEQ      'PAR'                                        *PER PARTENZA
112100970618     C                   MOVEL     VIDPPA        KCEEPA
112200970618     C                   Z-ADD     VIDPTP        KCETFP
112300980108     C                   Z-ADD     VIDPTA        KCETFA
112400970618     C     KEYCAE        SETLL     AZCAE01L                           99
112500970528E   1C                   ENDIF
112600970528     C*
112700970528IF  1C     WTIPRI        IFEQ      'ARR'                                        *PER ARRIVO
112800970618     C                   MOVEL     VIDPPA        KCEEPA
112900970618     C                   Z-ADD     VIDPTA        KCETFA
113000980108     C                   Z-ADD     VIDPTP        KCETFP
113100970618     C     KE2CAE        SETLL     AZCAE02L                           99
113200970528E   1C                   ENDIF
113300980107     C*
113400980107IF  1C     WTIPRI        IFEQ      'ECC'                                        *PER ARRIVO
113500980107     C                   MOVEL     VIDPPA        KCEEPA
113600980107     C                   Z-ADD     VIDPTE        KCETFE
113700980108     C                   Z-ADD     VIDPTA        KCETFA
113800980108     C                   Z-ADD     VIDPTP        KCETFP
113900980107     C     KE5CAE        SETLL     AZCAE05L                           99
114000980107E   1C                   ENDIF
114100970526     C* LETTURA RECORD
114200970526IF  1C     *IN99         IFEQ      *OFF
114300970526     C                   EXSR      LETFIL
114400970526E   1C                   ENDIF
114500970526     C*
114600970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
114700970526IF  1C     *IN99         IFEQ      *ON
114800970526     C                   Z-ADD     1             SFLEOF
114900970526E   1C                   ENDIF
115000970526     C*
115100970526     C                   ENDSR
115200970526     C*------------------------------------------------------------------------*
115300970526     C* LETFIL - LETTURA RECORD
115400970526     C*------------------------------------------------------------------------*
115500970526     C     LETFIL        BEGSR
115600970526     C*
115700970526     C* LEGGE FINO A:
115800970526     C*     - FINE FILE
115900970526     C*     - TROVATO RECORD VALIDO
116000970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
116100970526DO  1C     *IN99         DOWEQ     *OFF
116200970526     C     WRECOK        ANDEQ     'N'
116300970526     C*
116400970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
116500970528IF  2C     WTIPRI        IFEQ      'PAR'                                        *PER PARTENZA
116600970618     C                   READ      AZCAE01L                               99
116700970528E   2C                   ENDIF
116800970526     C*
116900970528IF  2C     WTIPRI        IFEQ      'ARR'                                        *PER ARRIVO
117000970618     C                   READ      AZCAE02L                               99
117100970528E   2C                   ENDIF
117200980107     C*
117300980107IF  2C     WTIPRI        IFEQ      'ECC'                                        *PER ECCEZIONE
117400980107     C                   READ      AZCAE05L                               99
117500980107E   2C                   ENDIF
117600970528     C*
117700970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
117800970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
117900970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
118000970530X   2C                   ELSE
118100970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
118200970530E   2C                   ENDIF
118300970526E   1C                   ENDDO
118400970526     C*
118500970526     C                   ENDSR
118600970526     C*------------------------------------------------------------------------*
118700970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
118800970526     C*------------------------------------------------------------------------*
118900970526     C     CHKREC        BEGSR
119000970526     C*
119100970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
119200970530     C*
119300970530     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
119400970530IF  1C     VIDPDD        IFGT      *ZEROS
119500970618IF  2C     PDDVID        IFGE      CAEDDE                                       *IN DECORRENZA
119600970618     C     PDDVID        ANDLE     CAEDSC                                       *ALLA DATA
119700970530X   2C                   ELSE
119800970530     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
119900970530E   2C                   ENDIF
120000970530E   1C                   ENDIF
120100970526     C*
120200970526     C                   ENDSR
120300970526     C*------------------------------------------------------------------------*
120400970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
120500970526     C*------------------------------------------------------------------------*
120600970526     C     IRESFL        BEGSR
120700970526     C*
120800970526     C                   MOVEL     *BLANKS       OPZKEY
120900970618     C                   Z-ADD     CAEDDE        VIHDDE
121000970618     C                   MOVEL     CAEATB        VIDATB
121100970618     C                   MOVEL     CAEEPA        VIDEPA
121200970618     C                   Z-ADD     CAETFP        VIDTFP
121300970618     C                   Z-ADD     CAETFA        VIDTFA
121400970618     C                   Z-ADD     CAETFE        VIDTFE
121500970618     C                   Z-ADD     CAEDDE        G08INV
121600970618     C                   Z-ADD     *ZEROS        G08DAT
121700970618     C                   MOVEL     '3'           G08ERR
121800970618     C                   CALL      'XSRDA8'
121900970618     C                   PARM                    WLBDA8
122000970618     C                   Z-ADD     G08DAT        VIDDDE
122100970618     C                   Z-ADD     CAEDSC        G08INV
122200970618     C                   Z-ADD     *ZEROS        G08DAT
122300970618     C                   MOVEL     '3'           G08ERR
122400970618     C                   CALL      'XSRDA8'
122500970618     C                   PARM                    WLBDA8
122600970618     C                   Z-ADD     G08DAT        VIDDSC
122700970528     C* DECODIFICHE
122800970618     C                   MOVEL     *BLANKS       VIDDTP
122900970618IF  1C     CAETFP        IFGT      *ZEROS
123000060929     C                   SETOFF                                           30
123100970528     C                   Z-ADD     1             I
123200060929     C     CAETFP        LOOKUP    ORG(I)                                 30
123300060929IF  2C     *IN30         IFEQ      *ON                                          *TROVATO
123400970528     C                   MOVEL     DORG(I)       VIDDTP
123500970618E   2C                   ENDIF
123600970618E   1C                   ENDIF
123700970526     C*
123800970618     C                   MOVEL     *BLANKS       VIDDTA
123900970618IF  1C     CAETFA        IFGT      *ZEROS
124000060929     C                   SETOFF                                           30
124100970528     C                   Z-ADD     1             I
124200060929     C     CAETFA        LOOKUP    ORG(I)                                 30
124300060929IF  2C     *IN30         IFEQ      *ON                                          *TROVATO
124400970528     C                   MOVEL     DORG(I)       VIDDTA
124500970618E   2C                   ENDIF
124600970618E   1C                   ENDIF
124700970528     C*
124800970618     C                   MOVEL     *BLANKS       VIDDTE
124900060929     C                   SETOFF                                           30
125000970528     C                   Z-ADD     1             I
125100060929     C     CAETFE        LOOKUP    ORG(I)                                 30
125200060929IF  1C     *IN30         IFEQ      *ON                                          *TROVATO
125300970528     C                   MOVEL     DORG(I)       VIDDTE
125400970528E   1C                   ENDIF
125500970528     C*
125600970526     C                   ENDSR
125700970526     C*------------------------------------------------------------------------*
125800970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
125900970526     C*------------------------------------------------------------------------*
126000970526     C     CHKPAR        BEGSR
126100970528     C*
126200970528     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER PARTENZA
126300970529IF  1C     VIDPPA        IFEQ      *BLANKS
126400970529     C     VIDPTP        ANDEQ     *ZEROS
126500970528     C     VIDPTA        ANDEQ     *ZEROS
126600980107     C     VIDPTE        ANDEQ     *ZEROS
126700970528     C                   MOVEL     'PAR'         WTIPRI            3
126800970528E   1C                   ENDIF                                                  *ORDINAMENTO RICHIES
126900970528     C*
127000970528     C* CONTROLLA LA DATA DECORRENZA
127100970528     C     *LIKE         DEFINE    VIDPDD        PDDVID                         *DATA IN A/M/G
127200970528     C                   Z-ADD     *ZEROS        PDDVID
127300970528IF  1C     ERRL00        IFEQ      '0'
127400970528IF  2C     VIDPDD        IFGT      *ZEROS
127500970528     C                   Z-ADD     VIDPDD        G08DAT
127600970528     C                   Z-ADD     *ZEROS        G08INV
127700970528     C                   MOVEL     '0'           G08ERR
127800970528     C                   CALL      'XSRDA8'
127900970528     C                   PARM                    WLBDA8
128000970528     C                   Z-ADD     G08DAT        VIDPDD
128100970528     C                   Z-ADD     G08INV        PDDVID
128200970528IF  3C     G08ERR        IFEQ      '1'
128300970528     C                   MOVEL     '1'           ERRL00
128400970528     C                   MOVEL     MSG(12)       DSMSMS
128500970528     C                   SETON                                        83
128600970528E   3C                   ENDIF
128700970528E   2C                   ENDIF
128800970528E   1C                   ENDIF
128900970528     C*
129000970528     C* CONTROLLA IL TIPO RICERCA
129100970528IF  1C     ERRL00        IFEQ      '0'
129200970528IF  2C     VIDPTP        IFNE      *ZEROS
129300980107     C                   MOVEL     'PAR'         WTIPRI
129400970528E   2C                   ENDIF
129500970528E   1C                   ENDIF
129600980107     C*
129700980107IF  1C     ERRL00        IFEQ      '0'
129800980107IF  2C     VIDPTA        IFNE      *ZEROS
129900970528     C                   MOVEL     'ARR'         WTIPRI
130000980107E   2C                   ENDIF
130100980107E   1C                   ENDIF
130200980107     C*
130300980107IF  1C     ERRL00        IFEQ      '0'
130400980107IF  2C     VIDPTE        IFNE      *ZEROS
130500980107     C                   MOVEL     'ECC'         WTIPRI
130600980107E   2C                   ENDIF
130700980107E   1C                   ENDIF
130800970526     C*
130900970526     C                   ENDSR
131000970526     C*------------------------------------------------------------------------*
131100970526     C* LANPGM - LANCIA UN PROGRAMMA
131200970526     C*------------------------------------------------------------------------*
131300970526     C     LANPGM        BEGSR
131400970526     C*
131500970526     C* IMPOSTA LA DS DI PROCEDURA
131600970602     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
131700970602     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
131800970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
131900970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
132000970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
132100970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
132200970602     C                   MOVEL     DEPA          D00EPA                         *PARTENZA/ARRIVO
132300970602     C                   Z-ADD     DETP          D00ETP                         *TERMINAL PARTENZA
132400970602     C                   Z-ADD     DETA          D00ETA                         *TERMINAL ARRIVO
132500970602     C                   Z-ADD     DEDD          D00EDD                         *DECORRENZA
132600970602     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI DATI
132700970530     C                   MOVEL     *BLANKS       D00TSP
132800970530     C                   Z-ADD     *ZEROS        D00TFP
132900970530     C                   MOVEL     *BLANKS       D00LOC
133000970603     C                   MOVEL     *BLANKS       D00SIN
133100970530     C                   MOVEL     *BLANKS       D00SPT
133200970530     C                   Z-ADD     *ZEROS        D00SLP
133300970530     C                   Z-ADD     *ZEROS        D00SLA
133400970530     C                   MOVEL     *BLANKS       D00STS
133500970530     C                   Z-ADD     *ZEROS        D00SDD
133600970526     C*
133700970526     C                   CALL      WPGM
133800970612     C                   PARM                    KPJBA
133900970612     C                   PARM                    DSSI00
134000970526     C*
134100970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
134200970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
134300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
134400970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
134500970526X   1C                   ELSE                                                   *NON PREMUTO F03
134600970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
134700970526E   1C                   ENDIF
134800970526     C*
134900970526     C                   ENDSR
135000970527     C*------------------------------------------------------------------------*
135100970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
135200970527     C*------------------------------------------------------------------------*
135300970527     C     CARTAB        BEGSR
135400970528     C*---
135500970528     C* ORGANIGRAMMA
135600970528     C*---
135700970528     C                   Z-ADD     *ZEROS        I                 5 0
135800970528     C                   Z-ADD     *ZEROS        ORG
135900970528     C                   MOVEL     *BLANKS       DORG
136000970528     C     *LOVAL        SETLL     AZORG01L
136100970528     C                   READ      AZORG01L                               99
136200970528DO  1C     *IN99         DOWEQ     *OFF
136300970528IF  2C     ORGFVA        IFEQ      *BLANKS
136400970528     C     ORGFAG        ANDNE     'V'
136500970528     C                   ADD       1             I
136600970528     C                   Z-ADD     ORGFIL        ORG(I)
136700970528     C                   MOVEL     ORGDES        DORG(I)
136800970528E   2C                   ENDIF
136900970528     C                   READ      AZORG01L                               99
137000970528E   1C                   ENDDO                                                  *FINE ORGANIGRAMMA
137100970527     C*
137200970527     C                   ENDSR
137300970526     C*------------------------------------------------------------------------*
137400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
137500970526     C*------------------------------------------------------------------------*
137600970526     C     CHKFOP        BEGSR
137700970626     C*
137800970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
137900970626IF  1C     DEPOP0        IFEQ      'P01'
138000970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
138100970626     C                   MOVEL     'S'           OPDATT
138200970626     C                   MOVEL     OPDRIG        OPD(1)
138300970626X   1C                   ELSE
138400970626     C* OPZIONI
138500970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
138600970626     C                   MOVEL     'S'           OPDATT
138700970626     C                   MOVEL     OPDRIG        OPD(5)
138800970626     C* FUNZIONI
138900180223     C                   MOVEL     FUD(9)        FUDRIG                         *TRANSITO TERMINAL
139000180223     C                   MOVEL     'S'           FUDATT
139100180223     C                   MOVEL     FUDRIG        FUD(9)
139200970626E   1C                   ENDIF
139300970626     C*
139400970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
139500970626IF  1C     DEPOP0        IFEQ      'P02'
139600970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
139700970626     C                   MOVEL     'S'           OPDATT
139800970626     C                   MOVEL     OPDRIG        OPD(2)
139900970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
140000970626     C                   MOVEL     'S'           OPDATT
140100970626     C                   MOVEL     OPDRIG        OPD(4)
140200970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
140300970626     C                   MOVEL     'S'           OPDATT
140400970626     C                   MOVEL     OPDRIG        OPD(7)
140500131008     C                   MOVEL     OPD(10)       OPDRIG                         *opzione SC
140600060929     C                   MOVEL     'S'           OPDATT
140700060929     C                   MOVEL     OPDRIG        OPD(10)
140800131008     C                   MOVEL     OPD(11)       OPDRIG                         *opzine +1
140900131008     C                   MOVEL     'S'           OPDATT
141000131008     C                   MOVEL     OPDRIG        OPD(11)
141100970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
141200970626     C                   MOVEL     'S'           FUDATT
141300970626     C                   MOVEL     FUDRIG        FUD(6)
141400180223     C                   MOVEL     FUD(9)        FUDRIG                         *TRANSITO TERMINAL
141500180223     C                   MOVEL     'S'           FUDATT
141600180223     C                   MOVEL     FUDRIG        FUD(9)
141700970626E   1C                   ENDIF
141800970526     C*
141900970526     C* GESTIONE OPZIONI A VIDEO
142000970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
142100970526     C                   EXSR      OPZGES
142200970526     C*
142300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
142400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
142500970526     C                   EXSR      FUNGES
142600970526     C*
142700970526     C                   ENDSR
142800970526     C*------------------------------------------------------------------------*
142900970526     C* *INZSR - OPERAZIONI INIZIALI
143000970526     C*------------------------------------------------------------------------*
143100970526     C     *INZSR        BEGSR
143200970526     C*---
143300970526     C* RICEVIMENTO PARAMETRI
143400970526     C*---
143500970526     C     *ENTRY        PLIST
143600970612     C                   PARM                    KPJBA
143700970612     C                   PARM                    DSSI00
143800970526     C*
143900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
144000970526     C     XMSGPA        PLIST
144100970526     C                   PARM                    DSMSMS
144200970526     C                   PARM                    DSMSPG
144300970526     C                   PARM                    DSMSKE
144400970526     C                   PARM                    DSMSER
144500970526     C*
144600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
144700970526     C                   CALL      'XKEY'
144800970526     C                   PARM                    DSKEY
144900970526     C*---
145000970526     C* VARIABILI NON RIFERITE AL DATA BASE
145100970526     C*---
145200970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
145300970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
145400970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
145500970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
145600970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
145700970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
145800120913     C     *LIKE         DEFINE    SFLNBR        RigaLetta
145900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
146000970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
146100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
146200970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
146300970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
146400970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
146500970529     C     *LIKE         DEFINE    D00OP1        WOP1
146600970602     C     *LIKE         DEFINE    D00EPA        DEPA
146700970602     C     *LIKE         DEFINE    D00ETP        DETP
146800970602     C     *LIKE         DEFINE    D00ETA        DETA
146900970602     C     *LIKE         DEFINE    D00EDD        DEDD
147000970526     C*---
147100970526     C* VARIABILI DI CONTROLLO
147200970526     C*---
147300970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
147400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
147500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
147600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
147700970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
147800970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
147900970526     C*
148000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
148100970528     C                   MOVEL     'TISI17R'     DSMSPG
148200970528     C                   MOVEL     'TISI17R'     MSGQUE
148300970526     C                   MOVEL     *BLANKS       MSGKEY
148400970526     C                   MOVEL     *BLANKS       DSMSMS
148500970526     C                   MOVEL     *BLANKS       DSMSKE
148600970526     C                   Z-ADD     *ZEROS        DSMSER
148700970526     C*---
148800970526     C* VARIABILI RIFERITE AL DATA BASE
148900970526     C*---
149000970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
149100970618     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         *AZCAE00F
149200970618     C     *LIKE         DEFINE    CAETFP        KCETFP
149300970618     C     *LIKE         DEFINE    CAETFA        KCETFA
149400980107     C     *LIKE         DEFINE    CAETFE        KCETFE
149500970618     C     *LIKE         DEFINE    CAEEPA        DEPEPA                         *DEPOSITI
149600970618     C     *LIKE         DEFINE    CAETFP        DEPTFP
149700970618     C     *LIKE         DEFINE    CAETFA        DEPTFA
149800980107     C     *LIKE         DEFINE    CAETFE        DEPTFE
149900970618     C     *LIKE         DEFINE    CAEDDE        DEPDDE
150000970526     C*---
150100970526     C* CHIAVI DI LETTURA
150200970526     C*---
150300970528     C* LETTURA AZORG01L - COMPLETA
150400970528     C     KEYORG        KLIST
150500970528     C                   KFLD                    KORFIL                         *FILIALE
150600970618     C* LETTURA AZCAE01L - PARZIALE
150700970618     C     KEYCAE        KLIST
150800970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
150900970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
151000980108     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
151100120913     C* LETTURA AZCAE01L - TOTALE
151200120913     C     K04CAE01      KLIST
151300120913     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
151400120913     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
151500120913     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
151600120913     C                   KFLD                    CAEDDE                         *DECORRENZA
151700120913     C* LETTURA AZCAE01L - Parziale
151800120913     C     K03CAE01      KLIST
151900120913     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
152000120913     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
152100120913     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
152200970618     C* LETTURA AZCAE02L - PARZIALE
152300970618     C     KE2CAE        KLIST
152400970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
152500970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
152600980108     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
152700980107     C* LETTURA AZCAE05L - PARZIALE
152800980107     C     KE5CAE        KLIST
152900980107     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
153000980107     C                   KFLD                    KCETFE                         *TERMINAL ECCEZIONE
153100980108     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
153200980108     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
153300120921     C* LETTURA AZCAE05L - TOTALE
153400120921     C     K05CAE05      KLIST
153500120921     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
153600120921     C                   KFLD                    CAETFE                         *TERMINAL ECCEZIONE
153700120921     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
153800120921     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
153900120921     C                   KFLD                    CAEDDE                         *DECORRENZA
154000120921     C* LETTURA AZCAE05L - PARZIALE
154100120921     C     K04CAE05      KLIST
154200120921     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
154300120921     C                   KFLD                    CAETFE                         *TERMINAL ECCEZIONE
154400120921     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
154500120921     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
154600970527     C*
154700970527     C* CARICA LE TABELLE OCCORRENTI
154800970527     C                   EXSR      CARTAB
154900970526     C*
155000970526     C* DEPOSITA LE OPZIONI DI ENTRATA
155100970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
155200970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
155300970526     C*
155400970526     C* CALCOLA LA DATA CORRENTE
155500970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
155600970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
155700970526     C                   Z-ADD     WN8           G08DAT
155800970526     C                   Z-ADD     *ZEROS        G08INV
155900970526     C                   MOVEL     '0'           G08ERR
156000970526     C                   CALL      'XSRDA8'
156100970526     C                   PARM                    WLBDA8
156200970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
156300970526     C*
156400970526     C* IMPOSTA I PARAMETRI DI INGRESSO
156500970528     C                   MOVEL     D00EPA        VIDPPA                         *PARTENZA/ARRIVO
156600970528     C                   Z-ADD     D00ETP        VIDPTP                         *TERMINAL PARTENZA
156700970528     C                   Z-ADD     D00ETA        VIDPTA                         *TERMINAL ARRIVO
156800980107     C                   Z-ADD     *ZEROS        VIDPTE                         *TERMINAL ECCEZIONE
156900970528     C                   Z-ADD     *ZEROS        VIDPDD                         *DATA DECORRENZA
157000970528IF  1C     D00EDD        IFGT      *ZEROS
157100970528     C                   Z-ADD     D00EDD        G08INV
157200970528     C                   Z-ADD     *ZEROS        G08DAT
157300970528     C                   MOVEL     '3'           G08ERR
157400970528     C                   CALL      'XSRDA8'
157500970528     C                   PARM                    WLBDA8
157600970528     C                   Z-ADD     G08DAT        VIDPDD
157700120903     C* se non è stata passata la data decorrenza, la imposto a video = oggi
157800120903E   1C                   ELSE
157900120903     C                   EVAL      VidPDD = %dec(%date():*eur)
158000120903E   1C                   ENDIF
158100970526     C                   EXSR      CHKPAR
158200970526IF  1C     ERRL00        IFEQ      '1'
158300970526     C                   MOVEL     '1'           D00ERR
158400970526     C                   MOVEL     DSMSMS        D00MSG
158500970526X   1C                   ELSE
158600970528     C                   MOVEL     VIDPPA        DEPEPA                         *DEPOSITI
158700970528     C                   Z-ADD     VIDPTP        DEPTFP
158800970528     C                   Z-ADD     VIDPTA        DEPTFA
158900980107     C                   Z-ADD     *ZEROS        DEPTFE
159000970528     C                   Z-ADD     VIDPDD        DEPDDE
159100970526E   1C                   ENDIF
159200970526     C*
159300970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
159400970526     C                   EXSR      CHKFOP
159500970526     C*
159600120903     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
159700970530IF  1C     DEPOP0        IFEQ      'P01'
159800970530     C                   MOVEL     DEC(3)        VIDING
159900970530E   1C                   ENDIF
160000970530IF  1C     DEPOP0        IFEQ      'P02'
160100970526     C                   MOVEL     DEC(1)        VIDING
160200970530E   1C                   ENDIF
160300970530IF  1C     DEPOP0        IFEQ      'P05'
160400970526     C                   MOVEL     DEC(2)        VIDING
160500970526E   1C                   ENDIF
160600970526     C*
160700970526     C                   ENDSR
160800970526     C*------------------------------------------------------------------------*
160900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
161000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
161100970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
161200970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
161300970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
161400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
161500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
161600970526     C*------------------------------------------------------------------------*
161700970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
161800970526     C*------------------------------------------------------------------------*
161900970526     C     POSSFL        BEGSR
162000970526     C*
162100970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
162200970526     C                   EXSR      CLRSFL
162300970526     C*
162400970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
162500970526     C                   EXSR      SETFIL
162600970526     C*
162700970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
162800970526     C     SFLEOF        IFEQ      0
162900970526     C                   EXSR      CARSFL
163000970526     C                   END
163100970526     C*
163200970526     C                   ENDSR
163300970526     C*------------------------------------------------------------------------*
163400970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
163500970526     C*------------------------------------------------------------------------*
163600970526     C     ENTATT        BEGSR
163700970526     C*
163800970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
163900970526     C                   Z-ADD     0             OPZOK             1 0
164000970526     C*
164100970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
164200970526     C                   Z-ADD     0             SFLVAR            5 0
164300970526     C*
164400970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
164500970526     C                   Z-ADD     0             SFLERR
164600970526     C*
164700970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
164800970526     C                   Z-ADD     SFLULT        WFLULT
164900970526     C*
165000970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
165100970528     C                   READC     SI17D05                                98
165200970526     C     *IN98         DOWEQ     '0'
165300970526     C*
165400970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
165500970526     C                   ADD       1             SFLVAR
165600970526     C*
165700970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
165800970526     C     OPZOK         IFEQ      0
165900970526     C*
166000970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
166100970526     C     OPZKEY        IFNE      *BLANKS
166200970526     C                   EXSR      OPZCON
166300970526     C                   END
166400970526     C*
166500970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
166600970526     C*            2. PULIZIA SCELTA IN SFL
166700970526     C     OPZOK         IFEQ      0
166800970526     C                   MOVEL     '0'           *IN41
166900970526     C                   MOVEL     *BLANKS       OPZKEY
167000970526     C                   ELSE
167100970526     C*
167200970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
167300970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
167400970526     C                   MOVEL     '1'           *IN41
167500970526     C                   Z-ADD     SFLULT        SFLERR
167600970526     C                   END
167700970526     C                   ELSE
167800970526     C*
167900970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
168000970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
168100970526     C                   MOVEL     '1'           *IN41
168200970526     C                   END                                                    OPZOK=0
168300970526     C*
168400970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
168500970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
168600970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
168700970526     C     OPZOK         IFNE      0
168800970526     C                   Z-ADD     SFLERR        SFLNBR
168900970526     C                   ELSE
169000970526     C                   Z-ADD     SFLULT        SFLNBR
169100970526     C                   END
169200970526     C*
169300970526     C* AGGIORNO RECORD SFL
169400970528     C                   UPDATE    SI17D05
169500970526     C*
169600970528     C                   READC     SI17D05                                98
169700970526     C                   END                                                    DO WHILE
169800970526     C*
169900970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
170000970526     C                   Z-ADD     WFLULT        SFLULT
170100970526     C*
170200970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
170300970526     C     SFLVAR        IFEQ      0
170400970526     C*---                 MOVEL'1'       FINL00
170500970526     C                   END
170600970526     C*
170700970526     C                   ENDSR
170800970526     C*------------------------------------------------------------------------*
170900970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
171000970526     C*------------------------------------------------------------------------*
171100970526     C     CLRSFL        BEGSR
171200970526     C*
171300970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
171400970526     C* (0=NON FINE FILE  1=FINE FILE)
171500970526     C                   Z-ADD     0             SFLEOF
171600970526     C*
171700970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
171800970526     C                   Z-ADD     0             SFLULT
171900970526     C*
172000970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
172100970526     C                   MOVEL     '1'           *IN40
172200970528     C                   WRITE     SI17D06
172300970526     C                   MOVEL     '0'           *IN40
172400970526     C*
172500970526     C                   ENDSR
172600970526     C*------------------------------------------------------------------------*
172700970526     C* CARSFL - CARICAMENTO SFL
172800970526     C*------------------------------------------------------------------------*
172900970526     C     CARSFL        BEGSR
173000970526     C*
173100970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
173200970526     C     SFLULT        IFEQ      0
173300970526     C                   EXSR      SETFIL
173400970526     C                   END
173500970526     C*
173600970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
173700970526     C                   Z-ADD     SFLULT        WFLULT
173800970526     C*
173900970526     C* CICLO:
174000970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
174100970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
174200970526     C                   Z-ADD     1             SFLRIG
174300970526     C     SFLRIG        DOWLE     SFLPAG
174400970526     C     SFLEOF        ANDEQ     0
174500970526     C*
174600970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
174700970526     C                   MOVEL     *BLANKS       OPZKEY
174800970526     C*
174900970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
175000970526     C                   EXSR      IRESFL
175100970526     C*
175200970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
175300970526     C                   ADD       1             SFLULT
175400970526     C*
175500970526     C* SCRITTURA RECORD SFL
175600970528     C                   WRITE     SI17D05
175700970526     C                   ADD       1             SFLRIG
175800970526     C*
175900970526     C* LETTURA FILE DA CARICARE
176000970526     C                   EXSR      LETFIL
176100060706     c* se cambiato tipo, lascio una riga vuota se non ho finito la
176200060706     c* pagina
176300060706     c                   if        caeepa<>videpa     and sfleof=0 and
176400060706     C                             sflrig<=sflpag
176500060706     c                   seton                                        01
176600060706     C                   MOVEL     *BLANKS       OPZKEY
176700060706     c                   clear                   vidatb
176800060706     c                   clear                   videpa
176900060706     c                   clear                   vidtfp
177000060706     c                   clear                   viddtp
177100060706     c                   clear                   vidtfa
177200060706     c                   clear                   viddta
177300060706     c                   clear                   vidtfe
177400060706     c                   clear                   viddte
177500060706     c                   clear                   viddde
177600060706     c                   clear                   viddsc
177700060706     C* SCRITTURA RECORD SFL
177800060706     C                   ADD       1             SFLULT
177900060706     C                   WRITE     SI17D05
178000060706     C                   ADD       1             SFLRIG
178100060706     c                   setoff                                       01
178200060706     C                   ENDif                                                  DO WHILE
178300060706     C                   END                                                    DO WHILE
178400970526     C*
178500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
178600970526     C     WFLULT        IFNE      SFLULT
178700970526     C                   Z-ADD     WFLULT        SFLNBR
178800970526     C                   ADD       1             SFLNBR
178900970526     C                   END
179000970526     C*
179100970526     C                   ENDSR
179200970526     C*------------------------------------------------------------------------*
179300970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
179400970526     C*------------------------------------------------------------------------*
179500970526     C     OUTSFL        BEGSR
179600970526     C*
179700970526     C* WRITE RECORD TESTATA VIDEO
179800970528     C                   WRITE     SI17D01
179900970526     C*
180000970526     C* VISUALIZZAZIONE MESSAGGIO
180100970526     C                   EXSR      SNDMSG
180200970526     C*
180300970526     C* WRITE RECORD OPZIONI VIDEO
180400970528     C                   WRITE     SI17D02
180500970526     C*
180600970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
180700970528     C                   WRITE     SI17D03
180800970526     C*
180900970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
181000970528     C                   WRITE     SI17D04
181100970526     C*
181200970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
181300970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
181400970526     C     SFLEOF        IFEQ      0
181500970526     C                   MOVEL     '0'           *IN42
181600970526     C                   ELSE
181700970526     C*
181800970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
181900970526     C                   MOVEL     '1'           *IN42
182000970526     C                   END
182100970526     C*
182200970526     C* WRITE RECORD TASTI FUNZIONALI
182300970528     C                   WRITE     SI17D09
182400970526     C*
182500970526     C* CASO DI SFL VUOTO
182600970526     C     SFLULT        IFLE      0
182700970526     C*
182800970526     C*    - WRITE RECORD VUOTO
182900970528     C                   WRITE     SI17D0A
183000970528     C                   READ      SI17D0A                                99
183100970526     C*
183200970526     C                   ELSE
183300970526     C*
183400970526     C* CASO DI SFL CON RECORDS
183500970526     C*
183600970526     C*    - WRITE RECORD CONTROLLO SFL
183700970528     C                   WRITE     SI17D06
183800970528     C                   READ      SI17D06                                99
183900970526     C                   END
184000970526     C*
184100970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
184200970528     C                   READ      SI17D03                                99
184300970526     C*
184400970526     C                   ENDSR
184500970526     C*------------------------------------------------------------------------*
184600970526     C* SNDMSG - MANDA MESSAGGIO
184700970526     C*------------------------------------------------------------------------*
184800970526     C     SNDMSG        BEGSR
184900970526     C*
185000970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
185100970526     C                   MOVEL     '1'           *IN40
185200970528     C                   WRITE     SI17DMC
185300970526     C                   MOVEL     '0'           *IN40
185400970526     C*
185500970526     C* VISUALIZZAZIONE MESSAGGIO
185600970526     C                   Z-ADD     0             MSGULT
185700970526     C                   CALL      'XMSG'        XMSGPA
185800970526     C     DSMSER        IFEQ      0
185900970526     C                   MOVEL     DSMSKE        MSGKEY
186000970526     C                   END
186100970526     C                   Z-ADD     1             MSGULT
186200970528     C                   WRITE     SI17DMS                                      SFL MESS
186300970528     C                   WRITE     SI17DMC                                      CONTR MESS
186400970526     C*
186500970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
186600970526     C                   MOVEL     *BLANKS       DSMSMS
186700970526     C*
186800970526     C                   ENDSR
186900970526     C*------------------------------------------------------------------------*
187000970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
187100970526     C*------------------------------------------------------------------------*
187200970526     C     FUNCON        BEGSR
187300970526     C*
187400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
187500970526     C                   EXSR      FUNVAR
187600970526     C*
187700970526     C* PULISCO CAMPI DI LAVORO
187800970526     C                   Z-ADD     0             I                 5 0
187900970526     C                   Z-ADD     0             W                 5 0
188000970526     C*
188100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
188200970526     C     FUNKEY        IFNE      *BLANKS
188300970526     C     1             DO        24            I
188400970526     C                   MOVEL     FUD(I)        FUDRIG
188500970526     C*
188600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
188700970526     C     FUNKEY        IFEQ      FUDFUN
188800970526     C     FUDATT        ANDEQ     'S'
188900970526     C                   Z-ADD     I             W
189000970526     C                   END
189100970526     C                   END                                                    DO WHILE
189200970526     C*
189300970526     C* FUNZIONE NON ATTIVA (ERRORE)
189400970526     C     W             IFEQ      0
189500970526     C*
189600970526     C* EMISSIONE MESSAGGIO DI ERRORE
189700970526     C                   MOVEL     MSG(2)        DSMSMS
189800970526     C                   END                                                    W=0
189900970526     C                   END                                                    FUNKEY NE *BLANKS
190000970526     C*
190100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
190200970526     C     W             IFNE      0
190300970526     C     FUNKEY        OREQ      *BLANKS
190400970526     C*
190500970526     C     FUNKEY        IFEQ      *BLANKS
190600970526     C                   EXSR      ENTGES
190700970526     C                   END
190800970526     C     FUNKEY        IFEQ      '03'
190900970526     C                   EXSR      F03GES
191000970526     C                   END
191100970526     C     FUNKEY        IFEQ      '05'
191200970526     C                   EXSR      F05GES
191300970526     C                   END
191400970526     C     FUNKEY        IFEQ      '06'
191500970526     C                   EXSR      F06GES
191600970526     C                   END
191700180223     C     FUNKEY        IFEQ      '09'
191800180223     C                   EXSR      F09GES
191900180223     C                   ENDIF
192000970526     C     FUNKEY        IFEQ      '12'
192100970526     C                   EXSR      F12GES
192200970526     C                   END
192300970528     C     FUNKEY        IFEQ      '21'
192400970528     C                   EXSR      F21GES
192500970528     C                   END
192600970528     C     FUNKEY        IFEQ      '22'
192700970528     C                   EXSR      F22GES
192800970528     C                   END
192900970526     C     FUNKEY        IFEQ      '23'
193000970526     C                   EXSR      F23GES
193100970526     C                   END
193200970526     C     FUNKEY        IFEQ      '24'
193300970526     C                   EXSR      F24GES
193400970526     C                   END
193500970526     C     FUNKEY        IFEQ      'RU'
193600970526     C                   EXSR      RUPGES
193700970526     C                   END
193800970526     C*
193900970526     C                   END
194000970526     C*
194100970526     C                   ENDSR
194200970526     C*------------------------------------------------------------------------*
194300970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
194400970526     C*------------------------------------------------------------------------*
194500970526     C     FUNVAR        BEGSR
194600970526     C*
194700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
194800970526     C     KEY           IFEQ      ENTER
194900970526     C                   MOVEL     *BLANKS       FUNKEY
195000970526     C                   END
195100970526     C     KEY           IFEQ      F1
195200970526     C                   MOVEL     '01'          FUNKEY
195300970526     C                   END
195400970526     C     KEY           IFEQ      F2
195500970526     C                   MOVEL     '02'          FUNKEY
195600970526     C                   END
195700970526     C     KEY           IFEQ      F3
195800970526     C                   MOVEL     '03'          FUNKEY
195900970526     C                   END
196000970526     C     KEY           IFEQ      F4
196100970526     C                   MOVEL     '04'          FUNKEY
196200970526     C                   END
196300970526     C     KEY           IFEQ      F5
196400970526     C                   MOVEL     '05'          FUNKEY
196500970526     C                   END
196600970526     C     KEY           IFEQ      F6
196700970526     C                   MOVEL     '06'          FUNKEY
196800970526     C                   END
196900970526     C     KEY           IFEQ      F7
197000970526     C                   MOVEL     '07'          FUNKEY
197100970526     C                   END
197200970526     C     KEY           IFEQ      F8
197300970526     C                   MOVEL     '08'          FUNKEY
197400970526     C                   END
197500970526     C     KEY           IFEQ      F9
197600970526     C                   MOVEL     '09'          FUNKEY
197700970526     C                   END
197800970526     C     KEY           IFEQ      F10
197900970526     C                   MOVEL     '10'          FUNKEY
198000970526     C                   END
198100970526     C     KEY           IFEQ      F11
198200970526     C                   MOVEL     '11'          FUNKEY
198300970526     C                   END
198400970526     C     KEY           IFEQ      F12
198500970526     C                   MOVEL     '12'          FUNKEY
198600970526     C                   END
198700970526     C     KEY           IFEQ      F13
198800970526     C                   MOVEL     '13'          FUNKEY
198900970526     C                   END
199000970526     C     KEY           IFEQ      F14
199100970526     C                   MOVEL     '14'          FUNKEY
199200970526     C                   END
199300970526     C     KEY           IFEQ      F15
199400970526     C                   MOVEL     '15'          FUNKEY
199500970526     C                   END
199600970526     C     KEY           IFEQ      F16
199700970526     C                   MOVEL     '16'          FUNKEY
199800970526     C                   END
199900970526     C     KEY           IFEQ      F17
200000970526     C                   MOVEL     '17'          FUNKEY
200100970526     C                   END
200200970526     C     KEY           IFEQ      F18
200300970526     C                   MOVEL     '18'          FUNKEY
200400970526     C                   END
200500970526     C     KEY           IFEQ      F19
200600970526     C                   MOVEL     '19'          FUNKEY
200700970526     C                   END
200800970526     C     KEY           IFEQ      F20
200900970526     C                   MOVEL     '20'          FUNKEY
201000970526     C                   END
201100970526     C     KEY           IFEQ      F21
201200970526     C                   MOVEL     '21'          FUNKEY
201300970526     C                   END
201400970526     C     KEY           IFEQ      F22
201500970526     C                   MOVEL     '22'          FUNKEY
201600970526     C                   END
201700970526     C     KEY           IFEQ      F23
201800970526     C                   MOVEL     '23'          FUNKEY
201900970526     C                   END
202000970526     C     KEY           IFEQ      F24
202100970526     C                   MOVEL     '24'          FUNKEY
202200970526     C                   END
202300970526     C     KEY           IFEQ      ROLLUP
202400970526     C                   MOVEL     'RU'          FUNKEY
202500970526     C                   END
202600970526     C     KEY           IFEQ      HELP
202700970526     C                   MOVEL     'HP'          FUNKEY
202800970526     C                   END
202900970526     C*
203000970526     C                   ENDSR
203100970526     C*------------------------------------------------------------------------*
203200970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
203300970526     C*------------------------------------------------------------------------*
203400970526     C     FUNGES        BEGSR
203500970526     C*
203600970526     C* PULISCO CAMPI DI LAVORO
203700970526     C                   Z-ADD     0             W                 5 0
203800970526     C*
203900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
204000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
204100970526     C     1             DO        24            I
204200970526     C                   MOVEL     FUD(I)        FUDRIG
204300970526     C*
204400970526     C* PULIZIA FUNZIONI PRECEDENTI
204500970526     C                   MOVEL     *BLANKS       FUV(I)
204600970526     C*
204700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
204800970526     C* (SALTA F23 E F24)
204900970526     C     FUDATT        IFEQ      'S'
205000970526     C     FUDFUN        ANDNE     '23'
205100970526     C     FUDFUN        ANDNE     '24'
205200970526     C                   ADD       1             W
205300970526     C                   MOVEL     FUDDES        FUV(W)
205400970526     C                   END
205500970526     C                   END                                                    DO WHILE
205600970526     C*
205700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
205800970526     C* ALTRIMENTI DISATTIVO F24
205900970526     C     1             DO        24            I
206000970526     C                   MOVEL     FUD(I)        FUDRIG
206100970526     C*
206200970526     C     FUDFUN        IFEQ      '24'
206300970526     C     W             IFGT      6
206400970526     C                   MOVEL     'S'           FUDATT
206500970526     C                   ELSE
206600970526     C                   MOVEL     *BLANKS       FUDATT
206700970526     C                   END                                                    W GT 8
206800970526     C                   MOVEL     FUDRIG        FUD(I)
206900970526     C                   END
207000970526     C                   END                                                    DO WHILE
207100970526     C*
207200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
207300970526     C     W             DIV       6             I
207400970526     C                   MVR                     Z
207500970526     C*
207600970526     C     Z             IFNE      0
207700970526     C     I             ADD       1             Z
207800970526     C                   ELSE
207900970526     C                   Z-ADD     I             Z
208000970526     C                   END
208100970526     C*
208200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
208300970526     C                   ADD       1             FUNGRU
208400970526     C     FUNGRU        IFGT      Z
208500970526     C     FUNGRU        ORLT      1
208600970526     C                   Z-ADD     1             FUNGRU
208700970526     C                   END
208800970526     C*
208900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
209000970526     C     FUNGRU        IFEQ      1
209100970526     C                   MOVEL     FUV(1)        FUN001
209200970526     C                   MOVEL     FUV(2)        FUN002
209300970526     C                   MOVEL     FUV(3)        FUN003
209400970526     C                   MOVEL     FUV(4)        FUN004
209500970526     C                   MOVEL     FUV(5)        FUN005
209600970526     C                   MOVEL     FUV(6)        FUN006
209700970526     C                   END
209800970526     C*
209900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
210000970526     C     FUNGRU        IFEQ      2
210100970526     C                   MOVEL     FUV(7)        FUN001
210200970526     C                   MOVEL     FUV(8)        FUN002
210300970526     C                   MOVEL     FUV(9)        FUN003
210400970526     C                   MOVEL     FUV(10)       FUN004
210500970526     C                   MOVEL     FUV(11)       FUN005
210600970526     C                   MOVEL     FUV(12)       FUN006
210700970526     C                   END
210800970526     C*
210900970526     C* TERZO GRUPPO DI 6 FUNZIONI
211000970526     C     FUNGRU        IFEQ      3
211100970526     C                   MOVEL     FUV(13)       FUN001
211200970526     C                   MOVEL     FUV(14)       FUN002
211300970526     C                   MOVEL     FUV(15)       FUN003
211400970526     C                   MOVEL     FUV(16)       FUN004
211500970526     C                   MOVEL     FUV(17)       FUN005
211600970526     C                   MOVEL     FUV(18)       FUN006
211700970526     C                   END
211800970526     C*
211900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
212000970526     C     FUNGRU        IFEQ      4
212100970526     C                   MOVEL     FUV(19)       FUN001
212200970526     C                   MOVEL     FUV(20)       FUN002
212300970526     C                   MOVEL     FUV(21)       FUN003
212400970526     C                   MOVEL     FUV(22)       FUN004
212500970526     C                   MOVEL     *BLANKS       FUN005
212600970526     C                   MOVEL     *BLANKS       FUN006
212700970526     C                   END
212800970526     C*
212900970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
213000970526     C                   MOVEL     *BLANKS       FUN007
213100970526     C                   MOVEL     *BLANKS       FUN008
213200970526     C*
213300970526     C     1             DO        24            I
213400970526     C                   MOVEL     FUD(I)        FUDRIG
213500970526     C     FUDFUN        IFEQ      '23'
213600970526     C     FUDATT        ANDEQ     'S'
213700970526     C                   MOVEL     FUDDES        FUN007
213800970526     C                   END
213900970526     C     FUDFUN        IFEQ      '24'
214000970526     C     FUDATT        ANDEQ     'S'
214100970526     C                   MOVEL     FUDDES        FUN008
214200970526     C                   END
214300970526     C                   END                                                    DO
214400970526     C*
214500970526     C                   ENDSR
214600970526     C*------------------------------------------------------------------------*
214700970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
214800970526     C*------------------------------------------------------------------------*
214900970526     C     OPZGES        BEGSR
215000970526     C*
215100970526     C* PULISCO CAMPI DI LAVORO
215200970526     C                   Z-ADD     0             W                 5 0
215300970526     C*
215400970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
215500970526     C* E CARICO TABELLA OPZIONI ATTIVE
215600970526     C     1             DO        24            I
215700970526     C                   MOVEL     OPD(I)        OPDRIG
215800970526     C*
215900970526     C* PULIZIA OPZIONI PRECEDENTI
216000970526     C                   MOVEL     *BLANKS       OPV(I)
216100970526     C*
216200970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
216300970526     C     OPDATT        IFEQ      'S'
216400970526     C                   ADD       1             W
216500970526     C                   MOVEL     OPDDES        OPV(W)
216600970526     C                   END
216700970526     C                   END                                                    DO WHILE
216800970526     C*
216900970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
217000970526     C* ALTRIMENTI DISATTIVO F23
217100970526     C     1             DO        24            I
217200970526     C                   MOVEL     FUD(I)        FUDRIG
217300970526     C*
217400970526     C     FUDFUN        IFEQ      '23'
217500970526     C     W             IFGT      8
217600970526     C                   MOVEL     'S'           FUDATT
217700970526     C                   ELSE
217800970526     C                   MOVEL     *BLANKS       FUDATT
217900970526     C                   END                                                    W GT 8
218000970526     C                   MOVEL     FUDRIG        FUD(I)
218100970526     C                   END
218200970526     C                   END                                                    DO WHILE
218300970526     C*
218400970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
218500970526     C     W             DIV       8             I
218600970526     C                   MVR                     Z                 5 0
218700970526     C*
218800970526     C     Z             IFNE      0
218900970526     C     I             ADD       1             Z
219000970526     C                   ELSE
219100970526     C                   Z-ADD     I             Z
219200970526     C                   END
219300970526     C*
219400970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
219500970526     C                   ADD       1             OPZGRU
219600970526     C     OPZGRU        IFGT      Z
219700970526     C     OPZGRU        ORLT      1
219800970526     C                   Z-ADD     1             OPZGRU
219900970526     C                   END
220000970526     C*
220100970526     C* PRIMO GRUPPO DI 8 OPZIONI
220200970526     C     OPZGRU        IFEQ      1
220300970526     C                   MOVEL     OPV(1)        OPZ001
220400970526     C                   MOVEL     OPV(2)        OPZ002
220500970526     C                   MOVEL     OPV(3)        OPZ003
220600970526     C                   MOVEL     OPV(4)        OPZ004
220700970526     C                   MOVEL     OPV(5)        OPZ005
220800970526     C                   MOVEL     OPV(6)        OPZ006
220900970526     C                   MOVEL     OPV(7)        OPZ007
221000970526     C                   MOVEL     OPV(8)        OPZ008
221100970526     C                   END
221200970526     C*
221300970526     C* SECONDO GRUPPO DI 8 OPZIONI
221400970526     C     OPZGRU        IFEQ      2
221500970526     C                   MOVEL     OPV(9)        OPZ001
221600970526     C                   MOVEL     OPV(10)       OPZ002
221700970526     C                   MOVEL     OPV(11)       OPZ003
221800970526     C                   MOVEL     OPV(12)       OPZ004
221900970526     C                   MOVEL     OPV(13)       OPZ005
222000970526     C                   MOVEL     OPV(14)       OPZ006
222100970526     C                   MOVEL     OPV(15)       OPZ007
222200970526     C                   MOVEL     OPV(16)       OPZ008
222300970526     C                   END
222400970526     C*
222500970526     C* TERZO GRUPPO DI 8 OPZIONI
222600970526     C     OPZGRU        IFEQ      3
222700970526     C                   MOVEL     OPV(17)       OPZ001
222800970526     C                   MOVEL     OPV(18)       OPZ002
222900970526     C                   MOVEL     OPV(19)       OPZ003
223000970526     C                   MOVEL     OPV(20)       OPZ004
223100970526     C                   MOVEL     OPV(21)       OPZ005
223200970526     C                   MOVEL     OPV(22)       OPZ006
223300970526     C                   MOVEL     OPV(23)       OPZ007
223400970526     C                   MOVEL     OPV(24)       OPZ008
223500970526     C                   END
223600970526     C*
223700970526     C                   ENDSR
223800970526     C*------------------------------------------------------------------------*
223900970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
224000970526     C*------------------------------------------------------------------------*
224100970526     C     OPZCON        BEGSR
224200970526     C*
224300970526     C* PULISCO CAMPI DI LAVORO
224400970526     C                   Z-ADD     0             I                 5 0
224500970526     C                   Z-ADD     0             W                 5 0
224600970526     C*
224700970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
224800970526     C     1             DO        24            I
224900970526     C                   MOVEL     OPD(I)        OPDRIG
225000970526     C*
225100970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
225200970526     C     OPZKEY        IFEQ      OPDOPZ
225300970526     C     OPZKEY        OREQ      OPDOP1
225400970526     C*
225500970526     C     OPDATT        IFEQ      'S'
225600970526     C                   MOVEL     OPDOPZ        OPZKEY
225700970526     C                   Z-ADD     I             W
225800970526     C                   END
225900970526     C                   END
226000970526     C*
226100970526     C                   END                                                    DO WHILE
226200970526     C*
226300970526     C* OPZIONE NON ATTIVA (ERRORE)
226400970526     C     W             IFEQ      0
226500970526     C*
226600970526     C* OPZIONE NON OK
226700970526     C                   Z-ADD     1             OPZOK
226800970526     C*
226900970526     C* EMISSIONE MESSAGGIO DI ERRORE
227000970526     C                   MOVEL     MSG(1)        DSMSMS
227100970526     C                   ELSE
227200970526     C*
227300970526     C* OPZIONE ATTIVA (PROCEDI ...)
227400970526     C*
227500970526     C     OPZKEY        IFEQ      ' 1'
227600970526     C                   EXSR      O01GES
227700970526     C                   END
227800970526     C     OPZKEY        IFEQ      ' 2'
227900970526     C                   EXSR      O02GES
228000970526     C                   END
228100970526     C     OPZKEY        IFEQ      ' 4'
228200970526     C                   EXSR      O04GES
228300970526     C                   END
228400970526     C     OPZKEY        IFEQ      ' 5'
228500970526     C                   EXSR      O05GES
228600970526     C                   END
228700970526     C     OPZKEY        IFEQ      ' 7'
228800970526     C                   EXSR      O07GES
228900970526     C                   END
229000060804     C     OPZKEY        IFEQ      'SC'
229100060804     C                   EXSR      OSCGES
229200060804     C                   END
229300131009     C     OPZKEY        IFEQ      '+1'
229400131015     C                   EXSR      Op1GES
229500131009     C                   END
229600970526     C                   END                                                    OPZ.NON ATTIVA
229700970526     C*
229800970526     C                   ENDSR
229900970526     C*------------------------------------------------------------------------*
230000970526     C* NOTE COSTRUZIONE SCHIERE
230100970526     C*------------------------------------------------------------------------*
230200970526     C*
230300970526     C* -MSG-MESSAGGI
230400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
230500970526     C* (NON TOCCARE MSG,1 E MSG,2)
230600970526     C*
230700970526     C* -CMD-COMANDI
230800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
230900970526     C*
231000970526     C* -OPD-OPZIONI
231100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
231200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
231300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
231400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
231500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
231600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
231700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
231800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
231900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
232000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
232100970526     C*
232200970526     C* -FUD-FUNZIONI
232300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
232400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
232500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
232600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
232700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
232800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
232900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
233000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
233100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
233200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
233300970526     O*------------------------------------------------------------------------*
233400970526** DEC - DECODIFICHE
233500970526    GESTIONE                                                     1
233600970526INTERROGAZIONE                                                   2
233700970530     SCELTA                                                      3
233800060929ScadenzaTerminal
2339009705265
234000970526** MSG - MESSAGGI
234100970528TISI17R- Opzione non gestita                                                   1
234200970528TISI17R- Tasto funzionale non gestito                                          2
234300970528TISI17R- Non abilitato alla gestione di questo CAP                             3
234400970528TISI17R- Record annullato. Consentito solo il ripristino                       4
234500970528TISI17R- Record non annullato. Impossibile ripristinare                        5
234600970528TISI17R- Accettato solo un parametro di inizio lista                           6
234700131009Opz. "SC=ScadenzaT" o "+1=PosticipaD": i record devono avere tipo A o P        7
234800131009Opz. "SC=ScadenzaTerminal": si deve selezionare per Data Decorrenza
234900120912La data scadenza nuovo terminal deve essere > scadenza terminal precedente     9
235000060929TISI17R- Terminal / linea errati                                              10
235100120913Opz. SC=Scadenza terminal: i record devono avere lo stesso tipo               11
235200970528TISI17R- Data errata                                                          12
235300131009Opz. "SC=ScadenzaT" o "+1=PosticipaD": i record non devono essere scaduti     13
235400120921Nuova data scadenza non può essere > 31 gg dalla precedente                   14
235500131009Opz."SC=ScadenzaT" o "+1=PosticipaD":i record devono essere gli ultimi decorr 15
235600120913La nuova scadenza deve essere < o = a tutte le scadenze selezionate           16
235700120921Data scadenza < oggi. ENTER forza                                             17
235800130913Aggiornamento non effettuabile per le filiali:
235900970528TISI17R- 19                                                                   19
236000970528TISI17R- 20                                                                   20
236100970528** CMD - COMANDI CL
236200970528WRKSPLF
236300970528
236400970528
236500970528
236600970528
236700970526** OPD - OPZIONI
236800970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
236900970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
237000970526                   -3 -     (S/N)=N                        opz. 03¬
237100970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
237200970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
237300970526                   -6 -     (S/N)=N                        opz. 06¬
237400970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
237500970527                   -8 -     (S/N)=N                        opz. 08¬
237600970527                   -9 -     (S/N)=N                        opz. 09¬
237700060929SC=Scadenza termin -10-     (S/N)=N                        opz. 10¬
237800131008+1=PosticipaDecorr -11-     (S/N)=N                        opz. 11¬
237900970526                   -  -     (S/N)=N                        opz. 12¬
238000970526                   -  -     (S/N)=N                        opz. 13¬
238100970526                   -  -     (S/N)=N                        opz. 14¬
238200970526                   -  -     (S/N)=N                        opz. 15¬
238300970526                   -  -     (S/N)=N                        opz. 16¬
238400970526                   -  -     (S/N)=N                        opz. 17¬
238500970526                   -  -     (S/N)=N                        opz. 18¬
238600970526                   -  -     (S/N)=N                        opz. 19¬
238700970526                   -  -     (S/N)=N                        opz. 20¬
238800970526                   -  -     (S/N)=N                        opz. 21¬
238900970526                   -  -     (S/N)=N                        opz. 22¬
239000970526                   -  -     (S/N)=N                        opz. 23¬
239100970526                   -  -     (S/N)=N                        opz. 24¬
239200970526** FUD - FUNZIONI
239300970526                   -        (S/N)=N                        funz.01¬
239400970526                   -        (S/N)=N                        funz.02¬
239500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
239600970526                   -        (S/N)=N                        funz.04¬
239700970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
239800020506F06=Inserimento    -        (S/N)=S                        funz.06¬
239900970526                   -        (S/N)=N                        funz.07¬
240000970527                   -        (S/N)=N                        funz.08¬
240100180312F09=TRANSITO-TERM. -        (S/N)=S                        funz.09¬
240200970527                   -        (S/N)=N                        funz.10¬
240300970526                   -        (S/N)=N                        funz.11¬
240400970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
240500970526                   -        (S/N)=N                        funz.13¬
240600970526                   -        (S/N)=N                        funz.14¬
240700970526                   -        (S/N)=N                        funz.15¬
240800970526                   -        (S/N)=N                        funz.16¬
240900970526                   -        (S/N)=N                        funz.17¬
241000970526                   -        (S/N)=N                        funz.18¬
241100970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
241200970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
241300980512F21=Stampa elenco  -        (S/N)=S                        funz.21¬
241400970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
241500970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
241600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
