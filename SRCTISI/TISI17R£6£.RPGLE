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
011200120904     D*
011300060929     D SAVING          S                   LIKE(VIDING)
011400120921     D wSflUlt         S                   LIKE(sflult)
011500060929     D SAVepa          S                   LIKE(videpa)
011600120904     D Rip18R1         S              2
011700120904     D Err18R1         S              1
011800120904     D S11NrRcd        S              5  0
011900120904     D DDeV11          S                   like(V11DDe)
012000120904     D NwTFE           S                   like(CAETFE)
012100120921     D ForzaMsg17      S               n
012200120921     D Dsc11Vid_sav    S              8s 0
012300970526     C*------------------------------------------------------------------------*
012400970526     C* MAIN LINES
012500970526     C*------------------------------------------------------------------------*
012600970526     C*
012700970526     C* SE NON CI SONO ERRORI, PROSEGUE
012800970526IF  1C     ERRL00        IFNE      '1'
012900970526     C*
013000970526     C* CARICA IL SFILE
013100970526     C                   EXSR      POSSFL
013200970526     C*
013300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013700970526     C*
013800970526     C* REIMPOSTA ERRORE DI PROCERURA
013900970526     C                   MOVEL     '0'           ERRL00
014000970526     C*
014100970526     C* EMETTE IL SFILE
014200970526     C                   EXSR      OUTSFL
014300970526     C*
014400970526     C* GESTIONE TASTI FUNZIONALI
014500970526     C                   EXSR      FUNCON
014600970526     C*
014700970526E   2C                   ENDDO
014800970527     C*
014900970527     C* CI SONO ERRORI IN ENTRATA
015000970527X   1C                   ELSE
015100970527     C                   MOVEL     '1'           D00ERR
015200970527     C                   MOVEL     DSMSMS        D00MSG
015300970526E   1C                   ENDIF
015400970526     C* FINE PGM
015500970526     C                   SETON                                        LR
015600970526     C*------------------------------------------------------------------------*
015700970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
015800970526     C*------------------------------------------------------------------------*
015900970526     C     F03GES        BEGSR
016000970526     C*
016100970526     C* IMPOSTA "PREMUTO F03"
016200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016300970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
016400970526     C*
016500970526     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
016800970526     C*------------------------------------------------------------------------*
016900970526     C     F05GES        BEGSR
017000970526     C*
017100970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017200970526     C*
017300970526     C                   ENDSR
017400970526     C*------------------------------------------------------------------------*
017500970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
017600970526     C*------------------------------------------------------------------------*
017700970526     C     F06GES        BEGSR
017800970526     C*
017900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
018000970602     C                   MOVEL     *BLANKS       DEPA
018100970602     C                   Z-ADD     *ZEROS        DETP
018200970602     C                   Z-ADD     *ZEROS        DETA
018300970602     C                   Z-ADD     *ZEROS        DEDD
018400970602     C                   MOVEL     DEPOP0        WOP0
018500970529     C                   MOVEL     'F06'         WOP1
018600970528     C                   MOVEL     'TISI18R'     WPGM              8
018700970526     C                   EXSR      LANPGM                                       *LANCIA PGM
018800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018900970526E   1C                   ENDIF
019000970526     C*
019100970526     C                   ENDSR
019200970526     C*------------------------------------------------------------------------*
019300970526     C* F12GES - GESTIONE F12 -> RITORNO
019400970526     C*------------------------------------------------------------------------*
019500970526     C     F12GES        BEGSR
019600970526     C*
019700970526     C* IMPOSTA "PREMUTO F12"
019800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019900970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
020000970526     C*
020100970526     C                   ENDSR
020200970528     C*------------------------------------------------------------------------*
020300970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
020400970528     C*------------------------------------------------------------------------*
020500970528     C     F21GES        BEGSR
020600970612     C*
020700980206     C                   MOVEL     *BLANKS       DEPA
020800980206     C                   Z-ADD     *ZEROS        DETP
020900980206     C                   Z-ADD     *ZEROS        DETA
021000980206     C                   Z-ADD     *ZEROS        DEDD
021100970612     C                   MOVEL     DEPOP0        WOP0
021200970612     C                   MOVE      '03'          WOP0
021300970612     C                   MOVEL     'F21'         WOP1
021400970612     C                   MOVEL     'TISI19R'     WPGM              8
021500970612     C                   EXSR      LANPGM
021600970528     C*
021700970528     C                   ENDSR
021800970528     C*------------------------------------------------------------------------*
021900970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
022000970528     C*------------------------------------------------------------------------*
022100970528     C     F22GES        BEGSR
022200970528     C*
022300970528     C                   Z-ADD     80            LENGH            15 5
022400970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
022500970528     C                   PARM                    CMD(1)
022600970528     C                   PARM                    LENGH
022700970528     C*
022800970528     C                   ENDSR
022900970526     C*------------------------------------------------------------------------*
023000970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
023100970526     C*------------------------------------------------------------------------*
023200970526     C     F23GES        BEGSR
023300970526     C*
023400970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
023500970526     C*
023600970526     C                   ENDSR
023700970526     C*------------------------------------------------------------------------*
023800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023900970526     C*------------------------------------------------------------------------*
024000970526     C     F24GES        BEGSR
024100970526     C*
024200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024300970526     C*
024400970526     C                   ENDSR
024500970526     C*------------------------------------------------------------------------*
024600970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
024700970526     C*------------------------------------------------------------------------*
024800970526     C     RUPGES        BEGSR
024900970526     C*
025000970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
025100970526     C*
025200970526     C                   ENDSR
025300970526     C*------------------------------------------------------------------------*
025400970526     C* ENTGES - GESTIONE ENTER -> INVIO
025500970526     C*------------------------------------------------------------------------*
025600970526     C     ENTGES        BEGSR
025700970526     C*
025800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025900970526     C                   EXSR      NEWPOS
026000970526     C*
026100970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026300970526IF  2C     SWPOSI        IFNE      0
026400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
026500970526X   2C                   ELSE
026600970526IF  3C     SFLULT        IFNE      0
026700970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026800970526     C*
026900970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
027000970526IF  4C     SWMODI        IFNE      0
027100970526     C                   EXSR      POSSFL
027200970526E   4C                   ENDIF
027300970526E   3C                   ENDIF
027400970526E   2C                   ENDIF
027500970526E   1C                   ENDIF
027600970526     C*
027700970526     C                   ENDSR
027800970526     C*------------------------------------------------------------------------*
027900970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
028000970526     C*------------------------------------------------------------------------*
028100970526     C     O01GES        BEGSR
028200970526     C*
028300970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
028400970528     C                   MOVEL     VIDEPA        D00EPA
028500970602     C                   Z-ADD     VIDTFP        D00ETP
028600970528     C                   Z-ADD     VIDTFA        D00ETA
028700970528     C                   Z-ADD     VIHDDE        D00EDD
028800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
028900970526     C*
029000970526     C                   ENDSR
029100970526     C*------------------------------------------------------------------------*
029200970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
029300970526     C*------------------------------------------------------------------------*
029400970526     C     O02GES        BEGSR
029500970526     C*
029600970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
029700970526IF  1C     VIDATB        IFEQ      'A'
029800970526     C                   MOVEL     '1'           ERRL00
029900970526     C                   MOVEL     MSG(4)        DSMSMS
030000970526E   1C                   ENDIF
030100970527     C* MODIFICA
030200970526IF  1C     ERRL00        IFEQ      '0'
030300970602     C                   MOVEL     VIDEPA        DEPA
030400970602     C                   Z-ADD     VIDTFP        DETP
030500970602     C                   Z-ADD     VIDTFA        DETA
030600970602     C                   Z-ADD     VIHDDE        DEDD
030700970602     C                   MOVEL     DEPOP0        WOP0
030800970526     C                   MOVEL     'O02'         WOP1
030900970528     C                   MOVEL     'TISI18R'     WPGM
031000970526     C                   EXSR      LANPGM
031100970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
031200131015     c                   eval      swmodi=1
031300970526E   1C                   ENDIF
031400970526     C*
031500970526     C                   ENDSR
031600970526     C*------------------------------------------------------------------------*
031700970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
031800970526     C*------------------------------------------------------------------------*
031900970526     C     O04GES        BEGSR
032000970526     C*
032100970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
032200970526IF  1C     VIDATB        IFEQ      'A'
032300970526     C                   MOVEL     '1'           ERRL00
032400970526     C                   MOVEL     MSG(4)        DSMSMS
032500970526E   1C                   ENDIF
032600970527     C* ANNULLAMENTO
032700970526IF  1C     ERRL00        IFEQ      '0'
032800970602     C                   MOVEL     VIDEPA        DEPA
032900970602     C                   Z-ADD     VIDTFP        DETP
033000970602     C                   Z-ADD     VIDTFA        DETA
033100970602     C                   Z-ADD     VIHDDE        DEDD
033200970602     C                   MOVEL     DEPOP0        WOP0
033300970526     C                   MOVEL     'O04'         WOP1
033400970528     C                   MOVEL     'TISI18R'     WPGM
033500970526     C                   EXSR      LANPGM
033600970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
033700970526E   1C                   ENDIF
033800970526     C*
033900970526     C                   ENDSR
034000970526     C*------------------------------------------------------------------------*
034100970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
034200970526     C*------------------------------------------------------------------------*
034300970526     C     O05GES        BEGSR
034400970526     C*
034500970602     C                   MOVEL     VIDEPA        DEPA
034600970602     C                   Z-ADD     VIDTFP        DETP
034700970602     C                   Z-ADD     VIDTFA        DETA
034800970602     C                   Z-ADD     VIHDDE        DEDD
034900970602     C                   MOVEL     DEPOP0        WOP0
035000970526     C                   MOVEL     'O05'         WOP1
035100970528     C                   MOVEL     'TISI18R'     WPGM
035200970526     C                   EXSR      LANPGM
035300970526     C*
035400970526     C                   ENDSR
035500970526     C*------------------------------------------------------------------------*
035600970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
035700970526     C*------------------------------------------------------------------------*
035800970526     C     O07GES        BEGSR
035900970526     C*
036000970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
036100970526IF  1C     VIDATB        IFNE      'A'
036200970526     C                   MOVEL     '1'           ERRL00
036300970526     C                   MOVEL     MSG(5)        DSMSMS
036400970526E   1C                   ENDIF
036500970526     C* RIPRISITINO
036600970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036700970602     C                   MOVEL     VIDEPA        DEPA
036800970602     C                   Z-ADD     VIDTFP        DETP
036900970602     C                   Z-ADD     VIDTFA        DETA
037000970602     C                   Z-ADD     VIHDDE        DEDD
037100970602     C                   MOVEL     DEPOP0        WOP0
037200970526     C                   MOVEL     'O07'         WOP1
037300970528     C                   MOVEL     'TISI18R'     WPGM
037400970526     C                   EXSR      LANPGM
037500970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
037600970526E   1C                   ENDIF
037700970526     C*
037800970526     C                   ENDSR
037900131009     C*----------------------------------------------------------------------------*
038000131015     C* Op1GES - GESTIONE OPZIONE "+1" =Postocipa decorrenza
038100131009     C*----------------------------------------------------------------------------*
038200131009     C     OP1GES        BEGSR
038300131009   0ac                   if        opzkey = '+1'
038400131009     c* Do errore se:
038500131009     c* - se il rcd è annullato logicamente
038600131009     c                   IF        VidATB <> *blank
038700131009     C                   MOVEL     '1'           ERRL00
038800131009     C                   MOVEL     MSG(4)        DSMSMS
038900131015     c                   movel     '1'           opzok
039000131015     c                   leavesr
039100131009E   1C                   ENDIF
039200131009     c* - il tipo terminal è <> da 'A' e 'P'
039300131009     c                   IF        (VidEPA <> 'A' and
039400131009     c                              VidEPA <> 'P')
039500131009     C                   MOVEL     '1'           ERRL00
039600131009     C                   MOVEL     MSG(7)        DSMSMS
039700131015     c                   movel     '1'           opzok
039800131015     c                   leavesr
039900131009E   1C                   ENDIF
040000131009     c* - se il rcd è scaduto
040100131009     c                   IF        %date(VidDSC:*eur) < %date()
040200131009     C                   MOVEL     '1'           ERRL00
040300131009     C                   MOVEL     MSG(13)       DSMSMS
040400131015     c                   movel     '1'           opzok
040500131015     c                   leavesr
040600131009E   1C                   ENDIF
040700131009     c* - se il rcd non è l'ultimo tra quelli di pari EPA/TFE/TFA/TFP
040800131009     C*   in questo modo cerco di evitare sovrapposizioni
040900131009     C                   MOVEL     VIDEPA        CAEEPA
041000131009     C                   Z-ADD     VIDTFA        CAETFA
041100131009     C                   Z-ADD     VIDTFP        CAETFP
041200131009     C* mi posiziono a quello letto sul sfl impostando la sua data decorrenza
041300131009     c                   EVAL      CAEDDE=%dec(%date(VidDDe:*eur):*iso)
041400131009     C     K04CAE01      CHAIN     AZCAE01L
041500131009     C                   DOU       %eof(AZCAE01L)
041600131009     C     K03CAE01      READE     AZCAE01L
041700131009     C* se leggo un rcd con EPA/TFE/TFA/TFE vuol dire solo che ha una decorrenza > , quindi non è
041800131009     C* l'ultimo
041900131009     C* non considero gli annullati
042000131009     c                   IF        not %eof and
042100131009     C                             CAEATB = *blank
042200131009     C                   MOVEL     '1'           ERRL00
042300131009     C                   MOVEL     MSG(15)       DSMSMS
042400131015     c                   movel     '1'           opzok
042500131015     c                   leavesr
042600131009     C                   ENDIF
042700131009     C                   ENDDO
042800131009     c* ogni volta che ho un errore esco dal ciclo principale, quindi se dal ciclo precedente
042900131009     C* sono uscito con errore, devo uscire anche dal ciclo principale
043000131009     C                   IF        ERRL00 = '1'
043100131015     c                   leavesr
043200131009E   1C                   ENDIF
043300131009     c*
043400131015     c* se non ho avuto errori richiamo aggiornamento
043500131015    1C                   if        ERRL00 <> '1'
043600131015     C                   MOVEL     VIDEPA        DEPA
043700131015     C                   Z-ADD     VIDTFP        DETP
043800131015     C                   Z-ADD     VIDTFA        DETA
043900131015     C                   Z-ADD     VIHDDE        DEDD
044000131015     C                   MOVEL     DEPOP0        WOP0
044100131015     C                   MOVEL     'O+1'         WOP1
044200131015     C                   MOVEL     'TISI18R'     WPGM
044300131015     C                   EXSR      LANPGM
044400131015     c                   eval      swmodi=1
044500131009    1c                   endif
044600131015   0ac                   endif
044700131009     C                   ENDSR
044800131009     C*----------------------------------------------------------------------------*
044900131009     C* OSCGES - GESTIONE OPZIONE SC= scadenza terminal
045000131009     C*----------------------------------------------------------------------------*
045100060804     C     OSCGES        BEGSR
045200120912     c* Per poter dare l'opzione SC si deve avere la selezione per data decorrenza
045300120913     C* in modo che sia più difficile (non impossibile) che l'utente crei delle sovrapposizioni
045400120912     c***                if        vidppa=' ' or  vidpdd=0
045500130116     C**                 IF        VIDPDD=0
045600130116     C**                 MOVEL     '1'           ERRL00
045700130116     C**                 MOVEL     MSG(8)        DSMSMS
045800120913     C* segnalo al resto del pgm che l'opzione ha dato un errore
045900130116     c**                 movel     '1'           opzok
046000120913     C* esco dalla routine
046100130116     C**                 LEAVESR
046200120913     c***                goto      endscade
046300130116     c**                 ENDIF
046400060929     c
046500120921     C                   EVAL      ForzaMsg17 = *off
046600120921     C                   EVAL      Dsc11Vid_sav = 0
046700060928     c* Leggo dal sfl tutti i record che hanno "SC" e devono avere
046800060928     c*  lo stesso tipo terminal
046900060929     c                   eval      savING=viding
047000060929     c                   movel     dec(4)        viding
047100060929     c
047200060929     c                   clear                   nrr               5 0
047300060929     c                   clear                   savepa
047400060929     c* pulisco il sfl di scadenza terminal
047500060928     c                   seton                                        70
047600060929     c                   write     si17c11
047700060928     c                   setoff                                       70
047800120913     C*
047900120921     c                   do        wflult        wSflUlt
048000120921     c     wSflUlt       chain     si17d05
048100131009     c                   if        opzkey='SC'
048200120903     c* La prima volta salvo alcuni dati del record
048300120903     c                   if        savepa=*blanks
048400120903     c                   eval      savepa=videpa
048500120903     c                   eval      v11epa=videpa
048600120903E   1C                   ENDIF
048700120903     c* Do errore se:
048800120921     c* - se il rcd è annullato logicamente
048900120921     c                   IF        VidATB <> *blank
049000120904     C                   MOVEL     '1'           ERRL00
049100120921     C                   MOVEL     MSG(4)        DSMSMS
049200120912     c                   leave
049300120904E   1C                   ENDIF
049400120921     c* - il tipo terminal è <> da 'A' e 'P'
049500120921     c                   IF        (VidEPA <> 'A' and
049600120921     c                              VidEPA <> 'P')
049700120921     C                   MOVEL     '1'           ERRL00
049800120921     C                   MOVEL     MSG(7)        DSMSMS
049900120921     c                   leave
050000120921E   1C                   ENDIF
050100120903     c* - il tipo terminal è <> dal precedente (deve essere 1 solo tipo)
050200120904     c                   IF        savepa<>VidEPA
050300120904     C                   MOVEL     '1'           ERRL00
050400120904     C                   MOVEL     MSG(11)       DSMSMS
050500120912     c                   leave
050600120904E   1C                   ENDIF
050700120903     c* - se il rcd è scaduto
050800120904     c                   IF        %date(VidDSC:*eur) < %date()
050900060928     C                   MOVEL     '1'           ERRL00
051000120904     C                   MOVEL     MSG(13)       DSMSMS
051100120912     c                   leave
051200060928E   1C                   ENDIF
051300120921     c* - se il rcd non è l'ultimo tra quelli di pari EPA/TFE/TFA/TFP
051400120913     C*   in questo modo cerco di evitare sovrapposizioni
051500120913     C                   MOVEL     VIDEPA        CAEEPA
051600120924     C***                Z-ADD     VIDTFE        CAETFE
051700120921     C                   Z-ADD     VIDTFA        CAETFA
051800120924     C                   Z-ADD     VIDTFP        CAETFP
051900120921     C* mi posiziono a quello letto sul sfl impostando la sua data decorrenza
052000120913     c                   EVAL      CAEDDE=%dec(%date(VidDDe:*eur):*iso)
052100120924     C     K04CAE01      CHAIN     AZCAE01L
052200120924     C                   DOU       %eof(AZCAE01L)
052300120924     C     K03CAE01      READE     AZCAE01L
052400120921     C* se leggo un rcd con EPA/TFE/TFA/TFE vuol dire solo che ha una decorrenza > , quindi non è
052500120913     C* l'ultimo
052600120921     C* non considero gli annullati
052700120921     c                   IF        not %eof and
052800120921     C                             CAEATB = *blank
052900120913     C                   MOVEL     '1'           ERRL00
053000120913     C                   MOVEL     MSG(15)       DSMSMS
053100120913     c                   leave
053200120921     C                   ENDIF
053300120921     C                   ENDDO
053400120924     c* ogni volta che ho un errore esco dal ciclo principale, quindi se dal ciclo precedente
053500120924     C* sono uscito con errore, devo uscire anche dal ciclo principale
053600120924     C                   IF        ERRL00 = '1'
053700120924     c                   leave
053800120924E   1C                   ENDIF
053900060929     c
054000120904     c* se sono qui NON ho errori pre emissione
054100120913     C* per cui scrivo un rcd del sfl dei terminal da far scadere
054200120904     c                   movel     vidtfp        v11tfp                         filiale partenza
054300060929     c                   movel     viddtp        v11dtp
054400120904     c                   movel     vidtfa        v11tfa                         filiale arrivo
054500060929     c                   movel     viddta        v11dta
054600120904     c                   movel     vidtfe        v11tfe                         filiale eccezione
054700060929     c                   movel     viddte        v11dte
054800120912     c                   EVAL      V11DDe = VidDDe                              data decorrenza H
054900120921     c                   EVAL      V11SDSc = VidDSc                             data scadenza
055000060929     c                   add       1             nrr
055100060929     c                   write     si17s11
055200060929     c                   endif
055300060929     c                   enddo
055400060928     c*
055500120912     c* se non ho avuto errori durante il caricamento del sfl
055600120912     C                   IF        ERRL00 <> '1'
055700120904     c* salvo il nr.rcd di questo sfl
055800120904     C                   EVAL      s11nrrcd = nrr
055900120904     c*
056000060929     c                   z-add     1             rec1
056100060929     c                   eval      v11nwrip='SI'
056200060929     c                   clear                   v11dsc
056300060929     c                   clear                   v11nwdsc
056400060929     c                   clear                   v11nwtfe
056500120912     c                   clear                   v11nwdte
056600060929     c
056700120904     c* se non ci sono errori o non ho premuto F12, ciclo
056800130913     c                   DOU       (*inkf = *on and *in28=*off) or *inkl=*on
056900120904     c                   write     si17d01
057000120904     c                   write     si17z11
057100060929     c                   exfmt     si17c11
057200060929     c                   setoff                                       2890
057300060929     c                   setoff                                       646566
057400060929     C                   MOVEL     '0'           ERRL00
057500060929     c* F12 - ritorno
057600060929     c                   if        *inkl
057700120912     c***                goto      endscade
057800120912     c                   LEAVE
057900060929     c                   endif
058000060929     c* controllo le selezioni fatte
058100060929     c                   exsr      CTRSCADE
058200060929     c
058300120904     c* se non ci sono errori e ho premuto F6
058400120904     c                   IF        ErrL00 <> *on and
058500120904     c                             *inkf
058600120904     c* leggo il sfl ed effettuo le modifiche
058700120904     c                   EXSR      GenNewScad
058800130913     c                   if        fil_e(1) > 0
058900130913     c                   seton                                        28
059000130913     c                   eval      vidmsg=msg(18)
059100130913     c                   z-add     1             EE
059200130913     c                   z-add     43            xx                4 0
059300130913     c                   dow       fil_e(EE)>0
059400130913     c                   add       4             xx
059500130913     c                   eval      %subst(vidmsg:xx:3)=%editc(fil_e(EE):'X')
059600130913     c                   add       1             EE
059700130913     c                   enddo
059800130913     c                   endif
059900130913     c
060000130913     c
060100060929     c                   endif
060200120904     c                   ENDDO
060300060928     c
060400120913     C* se ho avuto errori durante il caricamento del sfl
060500120913     C                   ELSE
060600120913     C* riciclo per riaccendere il sflnxtchg nelle righe già lette avento un'opzione SC
060700120921     c                   DO        RigaLetta     wSflUlt
060800120921     c     wSflUlt       chain     si17d05
060900120913     c                   if        opzkey='SC'
061000120913     C* riaccendo il sflnxtchg
061100120913     C                   SETON                                        41
061200120913     C                   UPDATE    SI17D05
061300120913E   1C                   ENDIF
061400120913E   1C                   ENDDO
061500120913     C* rileggo la riga che ha dato errore perché serve letta al resto del pgm
061600120913     c     RigaLetta     chain     si17d05
061700120913     C*
061800120912     C                   ENDIF
061900120912     c
062000120913     c***  endscade      tag
062100060929     c
062200120912     c                   if        dsmsms=*blank
062300060929     c                   movel     saving        viding
062400120921     c***                z-add     savult        sflult
062500060929     c                   movel     '1'           swmodi
062600131015     c                   eval      swmodi =1
062700120912     c                   else
062800120912     c                   movel     '1'           opzok
062900120912     c                   endif
063000120904     C*
063100120904     C                   ENDSR
063200120904     C*------------------------------------------------------------------------*
063300120904     C* CONTROLLO DATI PER SCADENZA TERMINAL
063400120904     C*------------------------------------------------------------------------*
063500120904     C     CTRSCADE      BEGSR
063600120904     C*
063700120904     C* Ctrl formale nuova data scadenza x tutte le linee
063800120904     C     *LIKE         DEFINE    VIDDSC        DSC11VID
063900060929IF  1C     ERRL00        IFEQ      '0'
064000120904     C                   Z-ADD     V11DSC        WDA8
064100060929     C                   Z-ADD     *ZEROS        DA8W
064200060929     C                   EXSR      CHKDA8
064300120904     C                   Z-ADD     WDA8          V11DSC
064400120904     C                   Z-ADD     DA8W          DSC11VID
064500060929IF  2C     ERRL00        IFEQ      '1'
064600060929     C                   SETON                                        6428
064700060929     C                   MOVEL     MSG(12)       vidmsg
064800120904     c                   LEAVESR
064900120921     C                   ELSE
065000120921     C* memorizzo la nuova data scadenza se diversa dall'ultima scritta
065100120921     C                   IF        Dsc11Vid <> Dsc11Vid_sav
065200120921     C                   EVAL      Dsc11Vid_sav = Dsc11Vid
065300120921     C                   EVAL      ForzaMsg17 = *off
065400120921     C                   ENDIF
065500120921     C* se data corretta formalmente, deve essere > o = a oggi ma ctrl forzabile
065600120921IF  3C                   IF        %date(V11DSC:*eur) < %date()
065700120921     C                             and ForzaMsg17 = *off
065800120921     C                   EVAL      ForzaMsg17 = *on
065900120921     C                   SETON                                        6428
066000120921     C                   MOVEL     MSG(17)       vidmsg
066100120921E   3C                   ENDIF
066200060929E   2C                   ENDIF
066300060929E   1C                   ENDIF
066400060929     c
066500120904     c* Ctrl formale data scadenza nuova associazione
066600120904     C     *LIKE         DEFINE    V11nwDSC      DSC11NwVID
066700060929IF  1C     ERRL00        IFEQ      '0'
066800060929     C                   Z-ADD     V11nwDSC      WDA8
066900060929     C                   Z-ADD     *ZEROS        DA8W
067000060929     C                   EXSR      CHKDA8
067100060929     C                   Z-ADD     WDA8          V11nwDSC
067200120904     C                   Z-ADD     DA8W          DSC11NwVID
067300060929IF  2C     ERRL00        IFEQ      '1'
067400060929     C                   SETON                                        6528
067500060929     C                   MOVEL     MSG(12)       vidmsg
067600120904     c                   LEAVESR
067700120921E   2C                   ENDIF
067800060929E   1C                   ENDIF
067900060929     c
068000120912     C* La data scadenza nuovo terminal deve essere > scadenza terminal precedente
068100120904    1c                   if        dsc11nwvid<=dsc11vid
068200120913     C                   SETON                                        6528
068300060929     C                   MOVEL     MSG(9)        vidmsg
068400120912     C* precedente msg, ora sostituito
068500120912     C* TISI17R- La nuova data decorrenza deve essere maggiore della prima             9
068600120904     C                   MOVEL     '1'           ERRL00
068700120904     c                   LEAVESR
068800120904E   1C                   ELSE
068900120904     c
069000120904     c* se richiesto il ripristino precedente associazione
069100120904     c* la data scadenza nuova associazione non deve essere > nuova data scadenza di più di 1 mese
069200120904     c* (facciamo 31 gg e non 1 mese variabile) altrimenti non è una cosa temporanea
069300120904     C                   IF        V11NwRip = 'SI'              and
069400120904     C                             %dec(%date(dsc11nwvid):*jul) -
069500120904     C                             %dec(%date(dsc11vid):*jul)   >
069600120904     C                             31
069700120921     C                   SETON                                          6528
069800120904     C                   MOVEL     MSG(14)       vidmsg
069900120904     C                   MOVEL     '1'           ERRL00
070000120904     c                   LEAVESR
070100120904E   1C                   ENDIF
070200120904E   1C                   ENDIF
070300060929     C*--------------------
070400060929     C* TERMINAL / LINEA ECCEZIONE
070500060929     C*--------------------
070600060929IF  1C     ERRL00        IFEQ      '0'
070700060929     C                   MOVEL     V11nwTFE      WLIN              3
070800060929     C                   MOVEL     *BLANKS       WDLIN
070900060929     C                   EXSR      CHKLIN
071000060929     C                   MOVEL     WLIN          V11nwTFE
071100060929     C                   MOVEL     WDLIN         V11nwdtE
071200060929IF  2C     ERRL00        IFEQ      '1'
071300060929     C                   SETON                                        6628
071400060929     C                   MOVEL     MSG(10)       vidmsg
071500120904     c                   LEAVESR
071600060929E   2C                   ENDIF
071700060929E   1C                   ENDIF
071800120913     C*
071900120913     C* se siamo qui, i singoli campi del CTL sono corretto. Adesso controllo i rcd del SFL
072000120913     C* con i dati del CTL
072100120921     C                   DO        s11nrrcd      wSflUlt
072200120921     C     wSflUlt       chain     si17s11
072300120913     C*
072400120913     C* controllo la nuova data scadenza (la prima delle 2) sia  < = di quella presente sul SFL
072500120921     C                   IF        DSc11VID > %dec(%date(V11SDSc:*eur):*iso)
072600120913     C                   SETON                                        6428
072700120913     C                   MOVEL     MSG(16)       vidmsg
072800120913     c                   LEAVESR
072900120913E   2C                   ENDIF
073000120913     C*
073100120913E   1C                   ENDDO
073200060929     c
073300060929     C                   ENDSR
073400120913     C*------------------------------------------------------------------------*
073500120913     C* OSCGES_old - GESTIONE OPZIONE SC= scadenza terminal
073600120913     C*------------------------------------------------------------------------*
073700120913     C     OSCGES_O      BEGSR
073800120913     c* Per poter dare l'opzione SC si deve avere la selezione per data decorrenza
073900120913     C* in modo che sia più difficile (non impossibile) che l'utente crei delle sovrapposizioni
074000120913     c***                if        vidppa=' ' or  vidpdd=0
074100120913     C                   IF        VIDPDD=0
074200120913     C                   MOVEL     '1'           ERRL00
074300120913     C                   MOVEL     MSG(8)        DSMSMS
074400120913     c                   goto      endscade
074500120913     c                   ENDIF
074600120913     c
074700120913     c* Leggo dal sfl tutti i record che hanno "SC" e devono avere
074800120913     c*  lo stesso tipo terminal
074900120913     c                   eval      savING=viding
075000120913     c                   movel     dec(4)        viding
075100120913     c
075200120913     c                   z-add     1             sflult
075300120913     c                   clear                   nrr               5 0
075400120913     c                   clear                   savepa
075500120913     c* pulisco il sfl di scadenza terminal
075600120913     c                   seton                                        70
075700120913     c                   write     si17c11
075800120913     c                   setoff                                       70
075900120913     c
076000120913     c                   do        wflult        sflult
076100120913     c     sflult        chain     si17d05
076200120913     c                   if        opzkey='SC'
076300120913     c* La prima volta salvo alcuni dati del record
076400120913     c                   if        savepa=*blanks
076500120913     c                   eval      savepa=videpa
076600120913     c                   eval      v11epa=videpa
076700120913E   1C                   ENDIF
076800120913     c* Do errore se:
076900120913     c* - il tipo terminal è <> da 'A' e 'P'
077000120913     c                   IF        (VidEPA <> 'A' and
077100120913     c                              VidEPA <> 'P')
077200120913     C                   MOVEL     '1'           ERRL00
077300120913     C                   MOVEL     MSG(7)        DSMSMS
077400120913     c                   leave
077500120913E   1C                   ENDIF
077600120913     c* - il tipo terminal è <> dal precedente (deve essere 1 solo tipo)
077700120913     c                   IF        savepa<>VidEPA
077800120913     C                   MOVEL     '1'           ERRL00
077900120913     C                   MOVEL     MSG(11)       DSMSMS
078000120913     c                   leave
078100120913E   1C                   ENDIF
078200120913     c* - se il rcd è scaduto
078300120913     c                   IF        %date(VidDSC:*eur) < %date()
078400120913     C                   MOVEL     '1'           ERRL00
078500120913     C                   MOVEL     MSG(13)       DSMSMS
078600120913     c                   leave
078700120913E   1C                   ENDIF
078800120913     c
078900120913     c* se sono qui NON ho errori pre emissione
079000120913     c                   movel     vidtfp        v11tfp                         filiale partenza
079100120913     c                   movel     viddtp        v11dtp
079200120913     c                   movel     vidtfa        v11tfa                         filiale arrivo
079300120913     c                   movel     viddta        v11dta
079400120913     c                   movel     vidtfe        v11tfe                         filiale eccezione
079500120913     c                   movel     viddte        v11dte
079600120913     c                   EVAL      V11DDe = VidDDe                              data decorrenza H
079700120913     c                   EVAL      V11DSc = VidDSc                              data scadenza
079800120913     c                   add       1             nrr
079900120913     c                   write     si17s11
080000120913     c                   endif
080100120913     c                   enddo
080200120913     c*
080300120913     c* se non ho avuto errori durante il caricamento del sfl
080400120913     C                   IF        ERRL00 <> '1'
080500120913     c* salvo il nr.rcd di questo sfl
080600120913     C                   EVAL      s11nrrcd = nrr
080700120913     c*
080800120913     c                   z-add     1             rec1
080900120913     c                   eval      v11nwrip='SI'
081000120913     c                   clear                   v11dsc
081100120913     c                   clear                   v11nwdsc
081200120913     c                   clear                   v11nwtfe
081300120913     c                   clear                   v11nwdte
081400120913     c
081500120913     c* se non ci sono errori o non ho premuto F12, ciclo
081600120913     c                   DOU       *inkf = *on or *inkl = *on
081700120913     c                   write     si17d01
081800120913     c                   write     si17z11
081900120913     c                   exfmt     si17c11
082000120913     c                   setoff                                       2890
082100120913     c                   setoff                                       646566
082200120913     C                   MOVEL     '0'           ERRL00
082300120913     c* F12 - ritorno
082400120913     c                   if        *inkl
082500120913     c***                goto      endscade
082600120913     c                   LEAVE
082700120913     c                   endif
082800120913     c* controllo le selezioni fatte
082900120913     c                   exsr      CTRSCADE
083000120913     c
083100120913     c* se non ci sono errori e ho premuto F6
083200120913     c                   IF        ErrL00 <> *on and
083300120913     c                             *inkf
083400120913     c* leggo il sfl ed effettuo le modifiche
083500120913     c                   EXSR      GenNewScad
083600120913     c                   endif
083700120913     c                   ENDDO
083800120913     c
083900120913     C                   ENDIF
084000120913     c
084100120913     c     endscade      tag
084200120913     c
084300120913     c                   if        dsmsms=*blank
084400120913     c                   movel     saving        viding
084500120921     c******             z-add     savult        sflult
084600131015     c                   movel     1             swmodi
084700120913     c                   else
084800120913     c                   movel     '1'           opzok
084900120913     c                   endif
085000120913     C*
085100120913     C                   ENDSR
085200120913     C*------------------------------------------------------------------------*
085300120913     C* CONTROLLO DATI PER SCADENZA TERMINAL       OLD!
085400120913     C*------------------------------------------------------------------------*
085500120913     C     CTRSCADE_o    BEGSR
085600120913     C*
085700120913     C* Ctrl formale nuova data scadenza x tutte le linee
085800120913     C     *LIKE         DEFINE    VIDDSC        DSC11VID
085900120913IF  1C     ERRL00        IFEQ      '0'
086000120913     C                   Z-ADD     V11DSC        WDA8
086100120913     C                   Z-ADD     *ZEROS        DA8W
086200120913     C                   EXSR      CHKDA8
086300120913     C                   Z-ADD     WDA8          V11DSC
086400120913     C                   Z-ADD     DA8W          DSC11VID
086500120913IF  2C     ERRL00        IFEQ      '1'
086600120913     C                   SETON                                        6428
086700120913     C                   MOVEL     MSG(12)       vidmsg
086800120913     c                   LEAVESR
086900120913E   2C                   ENDIF
087000120913E   1C                   ENDIF
087100120913     c
087200120913     c* Ctrl formale data scadenza nuova associazione
087300120913     C     *LIKE         DEFINE    V11nwDSC      DSC11NwVID
087400120913IF  1C     ERRL00        IFEQ      '0'
087500120913     C                   Z-ADD     V11nwDSC      WDA8
087600120913     C                   Z-ADD     *ZEROS        DA8W
087700120913     C                   EXSR      CHKDA8
087800120913     C                   Z-ADD     WDA8          V11nwDSC
087900120913     C                   Z-ADD     DA8W          DSC11NwVID
088000120913IF  2C     ERRL00        IFEQ      '1'
088100120913     C                   SETON                                        6528
088200120913     C                   MOVEL     MSG(12)       vidmsg
088300120913     c                   LEAVESR
088400120913E   2C                   ENDIF
088500120913E   1C                   ENDIF
088600120913     c
088700120913     C* La data scadenza nuovo terminal deve essere > scadenza terminal precedente
088800120913    1c                   if        dsc11nwvid<=dsc11vid
088900120913     C                   SETON                                        646528
089000120913     C                   MOVEL     MSG(9)        vidmsg
089100120913     C* precedente msg, ora sostituito
089200120913     C* TISI17R- La nuova data decorrenza deve essere maggiore della prima             9
089300120913     C                   MOVEL     '1'           ERRL00
089400120913     c                   LEAVESR
089500120913E   1C                   ELSE
089600120913     c
089700120913     c* se richiesto il ripristino precedente associazione
089800120913     c* la data scadenza nuova associazione non deve essere > nuova data scadenza di più di 1 mese
089900120913     c* (facciamo 31 gg e non 1 mese variabile) altrimenti non è una cosa temporanea
090000120913     C                   IF        V11NwRip = 'SI'              and
090100120913     C                             %dec(%date(dsc11nwvid):*jul) -
090200120913     C                             %dec(%date(dsc11vid):*jul)   >
090300120913     C                             31
090400120913     C                   SETON                                        646528
090500120913     C                   MOVEL     MSG(14)       vidmsg
090600120913     C                   MOVEL     '1'           ERRL00
090700120913     c                   LEAVESR
090800120913E   1C                   ENDIF
090900120913E   1C                   ENDIF
091000120913     C*--------------------
091100120913     C* TERMINAL / LINEA ECCEZIONE
091200120913     C*--------------------
091300120913IF  1C     ERRL00        IFEQ      '0'
091400120913     C                   MOVEL     V11nwTFE      WLIN              3
091500120913     C                   MOVEL     *BLANKS       WDLIN
091600120913     C                   EXSR      CHKLIN
091700120913     C                   MOVEL     WLIN          V11nwTFE
091800120913     C                   MOVEL     WDLIN         V11nwdtE
091900120913IF  2C     ERRL00        IFEQ      '1'
092000120913     C                   SETON                                        6628
092100120913     C                   MOVEL     MSG(10)       vidmsg
092200120913     c                   LEAVESR
092300120913E   2C                   ENDIF
092400120913E   1C                   ENDIF
092500120913     c
092600120913     C                   ENDSR
092700120904     C*------------------------------------------------------------------------*
092800120904     C* Genero nuove scadenze a fronte dell'opz.SC
092900120904     C*------------------------------------------------------------------------*
093000120904     C     GenNewScad    BEGSR
093100120904     C*
093200130913     c                   clear                   fil_e
093300130913     c                   clear                   EE                4 0
093400120904     C* ciclo per tutti i rcd del sfl
093500120904     C                   DO        s11nrrcd      sflult
093600120904     C     sflult        chain     si17s11
093700120904     C*
093800120904     C* richiamo pgm che fa:
093900120904     C*
094000120904     C* modifico l'attuale data scadenza con quella a video
094100120904     C*
094200120904     C* scrivo una nuova associazione con:
094300120904     C* - terminal = quello indicato a video
094400120904     C* - data decorrenza = la scadenza impostata + 1
094500120904     C* - data scadenza = quella indicata a video come data scadenza nuova associazione
094600120904     C*
094700120904     C* se richiesto il ripristino associazione precedente,
094800120904     C* scrivo una nuova associazione con:
094900120904     C* - dati di partenza, arrivo e terminal = quelli su sfl
095000120904     C* - data decorrenza = la scadenza impostata sulla nuova associazione + 1
095100120904     C* - data scadenza = 31/12/2039
095200120904     C                   EVAL      DDeV11 = %dec(%date(V11DDe:*eur):*iso)
095300120904     C                   EVAL      NWTFE = %dec(V11NWTFE:3:0)
095400120904     C                   CALL      'TISI18R1'
095500120904     C                   PARM                    V11EPA
095600120904     C                   PARM                    V11TFP
095700120904     C                   PARM                    V11TFA
095800120904     C                   PARM                    DDeV11
095900120904     C                   PARM                    NwTFE
096000120904     C                   PARM                    DSC11Vid
096100120904     C                   PARM                    DSC11NwVid
096200120904     C                   PARM                    V11NwRip
096300120904     C                   PARM                    Err18R1
096400130913     c                   if        Err18r1='1'
096500130913     c                   add       1             EE
096600130913     c                   eval      fil_e(ee)=v11tfp
096700130913
096800130913     c                   if        v11tfp=0
096900130913     c                   eval      fil_e(ee)=v11tfa
097000130913     c                   endif
097100130913     c                   endif
097200130913
097300120904     C                   ENDDO
097400120904     C*
097500120904     C                   ENDSR
097600060929     C*------------------------------------------------------------------------*
097700060929     C* CHKDA8 - CONTROLLA LA DATA (8)
097800060929     C*------------------------------------------------------------------------*
097900060929     C     CHKDA8        BEGSR
098000060929     C*
098100060929     C     *LIKE         DEFINE    VIDDDE        WDA8
098200060929     C     *LIKE         DEFINE    VIDDDE        DA8W
098300060929     C*
098400060929     C                   Z-ADD     WDA8          G08DAT
098500060929     C                   Z-ADD     *ZEROS        G08INV
098600060929     C                   MOVEL     '0'           G08ERR
098700060929     C                   CALL      'XSRDA8'
098800060929     C                   PARM                    WLBDA8
098900060929     C                   Z-ADD     G08DAT        WDA8
099000060929     C                   Z-ADD     G08INV        DA8W
099100060929IF  1C     G08ERR        IFEQ      '1'
099200060929     C                   MOVEL     '1'           ERRL00
099300060929E   1C                   ENDIF
099400060929     C*
099500060929     C                   ENDSR
099600060929     C*------------------------------------------------------------------------*
099700060929     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
099800060929     C*------------------------------------------------------------------------*
099900060929     C     CHKLIN        BEGSR
100000060929     C*
100100060929     C     *LIKE         DEFINE    Viddta        WDLIN
100200060929     C*
100300060929     C* RICHIESTO ELENCO
100400120904     C***                SETOFF                                           28
100500120904     C***  '?'           SCAN      WLIN                                   28
100600120904IF  1C***  *IN28         IFEQ      *ON
100700120904IF  1C                   IF        %scan('?':wLin) > 0
100800060929     C                   MOVEL     *BLANKS       §COD1             3
100900060929     C                   MOVEL     *BLANKS       §TIP              1
101000060929     C                   MOVEL     *BLANKS       §DES1            25
101100060929     C                   MOVEL     *BLANKS       §PRA              3
101200060929     C                   CALL      'TNSD24R'
101300060929     C                   PARM                    §COD1
101400060929     C                   PARM                    §TIP
101500060929     C                   PARM                    §DES1
101600060929     C                   PARM                    §PRA
101700060929     C                   MOVEL     §COD1         WLIN
101800060929     C                   MOVEL     §DES1         WDLIN
101900060929E   1C                   ENDIF
102000060929     C*
102100060929     C* SOLO CARATTERI NUMERICI
102200120904     C                   SETOFF                                       66
102300120904     C                   TESTN                   WLIN                 66
102400120904IF  1C     *IN66         IFEQ      *OFF                                         *NON E'NUMERO
102500060929     C                   MOVEL     '1'           ERRL00
102600060929X   1C                   ELSE                                                   *E'NUMERO DA TESTN
102700060929     C                   MOVE      WLIN          W001              1
102800060929IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
102900120904     C                   SETON                                        66
103000060929     C                   MOVEL     '1'           ERRL00
103100060929X   2C                   ELSE                                                   *NUMERO
103200060929     C*
103300060929     C* CONTROLLA ESISTENZA CODICE
103400060929     C                   MOVEL     WLIN          KORFIL
103500060929     C     KEYORG        CHAIN     AZORG01L                           99
103600060929IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
103700060929     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
103800060929     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
103900060929     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
104000060929X   3C                   ELSE
104100060929     C                   MOVEL     '1'           ERRL00
104200060929E   3C                   ENDIF
104300060929E   2C                   ENDIF
104400060929E   1C                   ENDIF
104500060929     C*
104600060929     C                   ENDSR
104700970526     C*------------------------------------------------------------------------*
104800970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
104900970526     C*------------------------------------------------------------------------*
105000970526     C     NEWPOS        BEGSR
105100970526     C*
105200970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
105300970526     C*         1= RICHIESTO POSIZIONAMENTO
105400970526     C                   Z-ADD     0             SWPOSI            7 0
105500970526     C*
105600970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
105700970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
105800970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
105900970526     C                   Z-ADD     0             SWMODI            1 0
106000970530     C*
106100970530     C* REIMPOSTA INDICATORI DI ERRORE
106200970530     C                   SETOFF                                       808182
106300970530     C                   SETOFF                                       83
106400970526     C*
106500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
106600970528IF  1C     VIDPPA        IFNE      DEPEPA
106700970528     C     VIDPTP        ORNE      DEPTFP
106800970528     C     VIDPTA        ORNE      DEPTFA
106900980107     C     VIDPTE        ORNE      DEPTFE
107000970528     C     VIDPDD        ORNE      DEPDDE
107100970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
107200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
107300970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
107400970528     C                   MOVEL     VIDPPA        DEPEPA                         *DEPOSITI
107500970528     C                   Z-ADD     VIDPTP        DEPTFP
107600970528     C                   Z-ADD     VIDPTA        DEPTFA
107700980107     C                   Z-ADD     VIDPTE        DEPTFE
107800970528     C                   Z-ADD     VIDPDD        DEPDDE
107900970526E   2C                   ENDIF
108000970526E   1C                   ENDIF
108100970526     C*
108200970526     C                   ENDSR
108300970526     C*------------------------------------------------------------------------*
108400970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
108500970526     C*------------------------------------------------------------------------*
108600970528     C     SETFIL        BEGSR
108700970526     C*
108800970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
108900970528IF  1C     WTIPRI        IFEQ      'PAR'                                        *PER PARTENZA
109000970618     C                   MOVEL     VIDPPA        KCEEPA
109100970618     C                   Z-ADD     VIDPTP        KCETFP
109200980108     C                   Z-ADD     VIDPTA        KCETFA
109300970618     C     KEYCAE        SETLL     AZCAE01L                           99
109400970528E   1C                   ENDIF
109500970528     C*
109600970528IF  1C     WTIPRI        IFEQ      'ARR'                                        *PER ARRIVO
109700970618     C                   MOVEL     VIDPPA        KCEEPA
109800970618     C                   Z-ADD     VIDPTA        KCETFA
109900980108     C                   Z-ADD     VIDPTP        KCETFP
110000970618     C     KE2CAE        SETLL     AZCAE02L                           99
110100970528E   1C                   ENDIF
110200980107     C*
110300980107IF  1C     WTIPRI        IFEQ      'ECC'                                        *PER ARRIVO
110400980107     C                   MOVEL     VIDPPA        KCEEPA
110500980107     C                   Z-ADD     VIDPTE        KCETFE
110600980108     C                   Z-ADD     VIDPTA        KCETFA
110700980108     C                   Z-ADD     VIDPTP        KCETFP
110800980107     C     KE5CAE        SETLL     AZCAE05L                           99
110900980107E   1C                   ENDIF
111000970526     C* LETTURA RECORD
111100970526IF  1C     *IN99         IFEQ      *OFF
111200970526     C                   EXSR      LETFIL
111300970526E   1C                   ENDIF
111400970526     C*
111500970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
111600970526IF  1C     *IN99         IFEQ      *ON
111700970526     C                   Z-ADD     1             SFLEOF
111800970526E   1C                   ENDIF
111900970526     C*
112000970526     C                   ENDSR
112100970526     C*------------------------------------------------------------------------*
112200970526     C* LETFIL - LETTURA RECORD
112300970526     C*------------------------------------------------------------------------*
112400970526     C     LETFIL        BEGSR
112500970526     C*
112600970526     C* LEGGE FINO A:
112700970526     C*     - FINE FILE
112800970526     C*     - TROVATO RECORD VALIDO
112900970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
113000970526DO  1C     *IN99         DOWEQ     *OFF
113100970526     C     WRECOK        ANDEQ     'N'
113200970526     C*
113300970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
113400970528IF  2C     WTIPRI        IFEQ      'PAR'                                        *PER PARTENZA
113500970618     C                   READ      AZCAE01L                               99
113600970528E   2C                   ENDIF
113700970526     C*
113800970528IF  2C     WTIPRI        IFEQ      'ARR'                                        *PER ARRIVO
113900970618     C                   READ      AZCAE02L                               99
114000970528E   2C                   ENDIF
114100980107     C*
114200980107IF  2C     WTIPRI        IFEQ      'ECC'                                        *PER ECCEZIONE
114300980107     C                   READ      AZCAE05L                               99
114400980107E   2C                   ENDIF
114500970528     C*
114600970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
114700970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
114800970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
114900970530X   2C                   ELSE
115000970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
115100970530E   2C                   ENDIF
115200970526E   1C                   ENDDO
115300970526     C*
115400970526     C                   ENDSR
115500970526     C*------------------------------------------------------------------------*
115600970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
115700970526     C*------------------------------------------------------------------------*
115800970526     C     CHKREC        BEGSR
115900970526     C*
116000970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
116100970530     C*
116200970530     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
116300970530IF  1C     VIDPDD        IFGT      *ZEROS
116400970618IF  2C     PDDVID        IFGE      CAEDDE                                       *IN DECORRENZA
116500970618     C     PDDVID        ANDLE     CAEDSC                                       *ALLA DATA
116600970530X   2C                   ELSE
116700970530     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
116800970530E   2C                   ENDIF
116900970530E   1C                   ENDIF
117000970526     C*
117100970526     C                   ENDSR
117200970526     C*------------------------------------------------------------------------*
117300970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
117400970526     C*------------------------------------------------------------------------*
117500970526     C     IRESFL        BEGSR
117600970526     C*
117700970526     C                   MOVEL     *BLANKS       OPZKEY
117800970618     C                   Z-ADD     CAEDDE        VIHDDE
117900970618     C                   MOVEL     CAEATB        VIDATB
118000970618     C                   MOVEL     CAEEPA        VIDEPA
118100970618     C                   Z-ADD     CAETFP        VIDTFP
118200970618     C                   Z-ADD     CAETFA        VIDTFA
118300970618     C                   Z-ADD     CAETFE        VIDTFE
118400970618     C                   Z-ADD     CAEDDE        G08INV
118500970618     C                   Z-ADD     *ZEROS        G08DAT
118600970618     C                   MOVEL     '3'           G08ERR
118700970618     C                   CALL      'XSRDA8'
118800970618     C                   PARM                    WLBDA8
118900970618     C                   Z-ADD     G08DAT        VIDDDE
119000970618     C                   Z-ADD     CAEDSC        G08INV
119100970618     C                   Z-ADD     *ZEROS        G08DAT
119200970618     C                   MOVEL     '3'           G08ERR
119300970618     C                   CALL      'XSRDA8'
119400970618     C                   PARM                    WLBDA8
119500970618     C                   Z-ADD     G08DAT        VIDDSC
119600970528     C* DECODIFICHE
119700970618     C                   MOVEL     *BLANKS       VIDDTP
119800970618IF  1C     CAETFP        IFGT      *ZEROS
119900060929     C                   SETOFF                                           30
120000970528     C                   Z-ADD     1             I
120100060929     C     CAETFP        LOOKUP    ORG(I)                                 30
120200060929IF  2C     *IN30         IFEQ      *ON                                          *TROVATO
120300970528     C                   MOVEL     DORG(I)       VIDDTP
120400970618E   2C                   ENDIF
120500970618E   1C                   ENDIF
120600970526     C*
120700970618     C                   MOVEL     *BLANKS       VIDDTA
120800970618IF  1C     CAETFA        IFGT      *ZEROS
120900060929     C                   SETOFF                                           30
121000970528     C                   Z-ADD     1             I
121100060929     C     CAETFA        LOOKUP    ORG(I)                                 30
121200060929IF  2C     *IN30         IFEQ      *ON                                          *TROVATO
121300970528     C                   MOVEL     DORG(I)       VIDDTA
121400970618E   2C                   ENDIF
121500970618E   1C                   ENDIF
121600970528     C*
121700970618     C                   MOVEL     *BLANKS       VIDDTE
121800060929     C                   SETOFF                                           30
121900970528     C                   Z-ADD     1             I
122000060929     C     CAETFE        LOOKUP    ORG(I)                                 30
122100060929IF  1C     *IN30         IFEQ      *ON                                          *TROVATO
122200970528     C                   MOVEL     DORG(I)       VIDDTE
122300970528E   1C                   ENDIF
122400970528     C*
122500970526     C                   ENDSR
122600970526     C*------------------------------------------------------------------------*
122700970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
122800970526     C*------------------------------------------------------------------------*
122900970526     C     CHKPAR        BEGSR
123000970528     C*
123100970528     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER PARTENZA
123200970529IF  1C     VIDPPA        IFEQ      *BLANKS
123300970529     C     VIDPTP        ANDEQ     *ZEROS
123400970528     C     VIDPTA        ANDEQ     *ZEROS
123500980107     C     VIDPTE        ANDEQ     *ZEROS
123600970528     C                   MOVEL     'PAR'         WTIPRI            3
123700970528E   1C                   ENDIF                                                  *ORDINAMENTO RICHIES
123800970528     C*
123900970528     C* CONTROLLA LA DATA DECORRENZA
124000970528     C     *LIKE         DEFINE    VIDPDD        PDDVID                         *DATA IN A/M/G
124100970528     C                   Z-ADD     *ZEROS        PDDVID
124200970528IF  1C     ERRL00        IFEQ      '0'
124300970528IF  2C     VIDPDD        IFGT      *ZEROS
124400970528     C                   Z-ADD     VIDPDD        G08DAT
124500970528     C                   Z-ADD     *ZEROS        G08INV
124600970528     C                   MOVEL     '0'           G08ERR
124700970528     C                   CALL      'XSRDA8'
124800970528     C                   PARM                    WLBDA8
124900970528     C                   Z-ADD     G08DAT        VIDPDD
125000970528     C                   Z-ADD     G08INV        PDDVID
125100970528IF  3C     G08ERR        IFEQ      '1'
125200970528     C                   MOVEL     '1'           ERRL00
125300970528     C                   MOVEL     MSG(12)       DSMSMS
125400970528     C                   SETON                                        83
125500970528E   3C                   ENDIF
125600970528E   2C                   ENDIF
125700970528E   1C                   ENDIF
125800970528     C*
125900970528     C* CONTROLLA IL TIPO RICERCA
126000970528IF  1C     ERRL00        IFEQ      '0'
126100970528IF  2C     VIDPTP        IFNE      *ZEROS
126200980107     C                   MOVEL     'PAR'         WTIPRI
126300970528E   2C                   ENDIF
126400970528E   1C                   ENDIF
126500980107     C*
126600980107IF  1C     ERRL00        IFEQ      '0'
126700980107IF  2C     VIDPTA        IFNE      *ZEROS
126800970528     C                   MOVEL     'ARR'         WTIPRI
126900980107E   2C                   ENDIF
127000980107E   1C                   ENDIF
127100980107     C*
127200980107IF  1C     ERRL00        IFEQ      '0'
127300980107IF  2C     VIDPTE        IFNE      *ZEROS
127400980107     C                   MOVEL     'ECC'         WTIPRI
127500980107E   2C                   ENDIF
127600980107E   1C                   ENDIF
127700970526     C*
127800970526     C                   ENDSR
127900970526     C*------------------------------------------------------------------------*
128000970526     C* LANPGM - LANCIA UN PROGRAMMA
128100970526     C*------------------------------------------------------------------------*
128200970526     C     LANPGM        BEGSR
128300970526     C*
128400970526     C* IMPOSTA LA DS DI PROCEDURA
128500970602     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
128600970602     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
128700970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
128800970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
128900970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
129000970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
129100970602     C                   MOVEL     DEPA          D00EPA                         *PARTENZA/ARRIVO
129200970602     C                   Z-ADD     DETP          D00ETP                         *TERMINAL PARTENZA
129300970602     C                   Z-ADD     DETA          D00ETA                         *TERMINAL ARRIVO
129400970602     C                   Z-ADD     DEDD          D00EDD                         *DECORRENZA
129500970602     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI DATI
129600970530     C                   MOVEL     *BLANKS       D00TSP
129700970530     C                   Z-ADD     *ZEROS        D00TFP
129800970530     C                   MOVEL     *BLANKS       D00LOC
129900970603     C                   MOVEL     *BLANKS       D00SIN
130000970530     C                   MOVEL     *BLANKS       D00SPT
130100970530     C                   Z-ADD     *ZEROS        D00SLP
130200970530     C                   Z-ADD     *ZEROS        D00SLA
130300970530     C                   MOVEL     *BLANKS       D00STS
130400970530     C                   Z-ADD     *ZEROS        D00SDD
130500970526     C*
130600970526     C                   CALL      WPGM
130700970612     C                   PARM                    KPJBA
130800970612     C                   PARM                    DSSI00
130900970526     C*
131000970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
131100970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
131200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
131300970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
131400970526X   1C                   ELSE                                                   *NON PREMUTO F03
131500970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
131600970526E   1C                   ENDIF
131700970526     C*
131800970526     C                   ENDSR
131900970527     C*------------------------------------------------------------------------*
132000970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
132100970527     C*------------------------------------------------------------------------*
132200970527     C     CARTAB        BEGSR
132300970528     C*---
132400970528     C* ORGANIGRAMMA
132500970528     C*---
132600970528     C                   Z-ADD     *ZEROS        I                 5 0
132700970528     C                   Z-ADD     *ZEROS        ORG
132800970528     C                   MOVEL     *BLANKS       DORG
132900970528     C     *LOVAL        SETLL     AZORG01L
133000970528     C                   READ      AZORG01L                               99
133100970528DO  1C     *IN99         DOWEQ     *OFF
133200970528IF  2C     ORGFVA        IFEQ      *BLANKS
133300970528     C     ORGFAG        ANDNE     'V'
133400970528     C                   ADD       1             I
133500970528     C                   Z-ADD     ORGFIL        ORG(I)
133600970528     C                   MOVEL     ORGDES        DORG(I)
133700970528E   2C                   ENDIF
133800970528     C                   READ      AZORG01L                               99
133900970528E   1C                   ENDDO                                                  *FINE ORGANIGRAMMA
134000970527     C*
134100970527     C                   ENDSR
134200970526     C*------------------------------------------------------------------------*
134300970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
134400970526     C*------------------------------------------------------------------------*
134500970526     C     CHKFOP        BEGSR
134600970626     C*
134700970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
134800970626IF  1C     DEPOP0        IFEQ      'P01'
134900970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
135000970626     C                   MOVEL     'S'           OPDATT
135100970626     C                   MOVEL     OPDRIG        OPD(1)
135200970626X   1C                   ELSE
135300970626     C* OPZIONI
135400970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
135500970626     C                   MOVEL     'S'           OPDATT
135600970626     C                   MOVEL     OPDRIG        OPD(5)
135700970626     C* FUNZIONI
135800970626E   1C                   ENDIF
135900970626     C*
136000970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
136100970626IF  1C     DEPOP0        IFEQ      'P02'
136200970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
136300970626     C                   MOVEL     'S'           OPDATT
136400970626     C                   MOVEL     OPDRIG        OPD(2)
136500970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
136600970626     C                   MOVEL     'S'           OPDATT
136700970626     C                   MOVEL     OPDRIG        OPD(4)
136800970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
136900970626     C                   MOVEL     'S'           OPDATT
137000970626     C                   MOVEL     OPDRIG        OPD(7)
137100131008     C                   MOVEL     OPD(10)       OPDRIG                         *opzione SC
137200060929     C                   MOVEL     'S'           OPDATT
137300060929     C                   MOVEL     OPDRIG        OPD(10)
137400131008     C                   MOVEL     OPD(11)       OPDRIG                         *opzine +1
137500131008     C                   MOVEL     'S'           OPDATT
137600131008     C                   MOVEL     OPDRIG        OPD(11)
137700970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
137800970626     C                   MOVEL     'S'           FUDATT
137900970626     C                   MOVEL     FUDRIG        FUD(6)
138000970626E   1C                   ENDIF
138100970526     C*
138200970526     C* GESTIONE OPZIONI A VIDEO
138300970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
138400970526     C                   EXSR      OPZGES
138500970526     C*
138600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
138700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
138800970526     C                   EXSR      FUNGES
138900970526     C*
139000970526     C                   ENDSR
139100970526     C*------------------------------------------------------------------------*
139200970526     C* *INZSR - OPERAZIONI INIZIALI
139300970526     C*------------------------------------------------------------------------*
139400970526     C     *INZSR        BEGSR
139500970526     C*---
139600970526     C* RICEVIMENTO PARAMETRI
139700970526     C*---
139800970526     C     *ENTRY        PLIST
139900970612     C                   PARM                    KPJBA
140000970612     C                   PARM                    DSSI00
140100970526     C*
140200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
140300970526     C     XMSGPA        PLIST
140400970526     C                   PARM                    DSMSMS
140500970526     C                   PARM                    DSMSPG
140600970526     C                   PARM                    DSMSKE
140700970526     C                   PARM                    DSMSER
140800970526     C*
140900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
141000970526     C                   CALL      'XKEY'
141100970526     C                   PARM                    DSKEY
141200970526     C*---
141300970526     C* VARIABILI NON RIFERITE AL DATA BASE
141400970526     C*---
141500970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
141600970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
141700970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
141800970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
141900970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
142000970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
142100120913     C     *LIKE         DEFINE    SFLNBR        RigaLetta
142200970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
142300970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
142400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
142500970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
142600970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
142700970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
142800970529     C     *LIKE         DEFINE    D00OP1        WOP1
142900970602     C     *LIKE         DEFINE    D00EPA        DEPA
143000970602     C     *LIKE         DEFINE    D00ETP        DETP
143100970602     C     *LIKE         DEFINE    D00ETA        DETA
143200970602     C     *LIKE         DEFINE    D00EDD        DEDD
143300970526     C*---
143400970526     C* VARIABILI DI CONTROLLO
143500970526     C*---
143600970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
143700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
143800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
143900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
144000970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
144100970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
144200970526     C*
144300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
144400970528     C                   MOVEL     'TISI17R'     DSMSPG
144500970528     C                   MOVEL     'TISI17R'     MSGQUE
144600970526     C                   MOVEL     *BLANKS       MSGKEY
144700970526     C                   MOVEL     *BLANKS       DSMSMS
144800970526     C                   MOVEL     *BLANKS       DSMSKE
144900970526     C                   Z-ADD     *ZEROS        DSMSER
145000970526     C*---
145100970526     C* VARIABILI RIFERITE AL DATA BASE
145200970526     C*---
145300970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
145400970618     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         *AZCAE00F
145500970618     C     *LIKE         DEFINE    CAETFP        KCETFP
145600970618     C     *LIKE         DEFINE    CAETFA        KCETFA
145700980107     C     *LIKE         DEFINE    CAETFE        KCETFE
145800970618     C     *LIKE         DEFINE    CAEEPA        DEPEPA                         *DEPOSITI
145900970618     C     *LIKE         DEFINE    CAETFP        DEPTFP
146000970618     C     *LIKE         DEFINE    CAETFA        DEPTFA
146100980107     C     *LIKE         DEFINE    CAETFE        DEPTFE
146200970618     C     *LIKE         DEFINE    CAEDDE        DEPDDE
146300970526     C*---
146400970526     C* CHIAVI DI LETTURA
146500970526     C*---
146600970528     C* LETTURA AZORG01L - COMPLETA
146700970528     C     KEYORG        KLIST
146800970528     C                   KFLD                    KORFIL                         *FILIALE
146900970618     C* LETTURA AZCAE01L - PARZIALE
147000970618     C     KEYCAE        KLIST
147100970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
147200970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
147300980108     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
147400120913     C* LETTURA AZCAE01L - TOTALE
147500120913     C     K04CAE01      KLIST
147600120913     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
147700120913     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
147800120913     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
147900120913     C                   KFLD                    CAEDDE                         *DECORRENZA
148000120913     C* LETTURA AZCAE01L - Parziale
148100120913     C     K03CAE01      KLIST
148200120913     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
148300120913     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
148400120913     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
148500970618     C* LETTURA AZCAE02L - PARZIALE
148600970618     C     KE2CAE        KLIST
148700970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
148800970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
148900980108     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
149000980107     C* LETTURA AZCAE05L - PARZIALE
149100980107     C     KE5CAE        KLIST
149200980107     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
149300980107     C                   KFLD                    KCETFE                         *TERMINAL ECCEZIONE
149400980108     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
149500980108     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
149600120921     C* LETTURA AZCAE05L - TOTALE
149700120921     C     K05CAE05      KLIST
149800120921     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
149900120921     C                   KFLD                    CAETFE                         *TERMINAL ECCEZIONE
150000120921     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
150100120921     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
150200120921     C                   KFLD                    CAEDDE                         *DECORRENZA
150300120921     C* LETTURA AZCAE05L - PARZIALE
150400120921     C     K04CAE05      KLIST
150500120921     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
150600120921     C                   KFLD                    CAETFE                         *TERMINAL ECCEZIONE
150700120921     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
150800120921     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
150900970527     C*
151000970527     C* CARICA LE TABELLE OCCORRENTI
151100970527     C                   EXSR      CARTAB
151200970526     C*
151300970526     C* DEPOSITA LE OPZIONI DI ENTRATA
151400970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
151500970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
151600970526     C*
151700970526     C* CALCOLA LA DATA CORRENTE
151800970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
151900970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
152000970526     C                   Z-ADD     WN8           G08DAT
152100970526     C                   Z-ADD     *ZEROS        G08INV
152200970526     C                   MOVEL     '0'           G08ERR
152300970526     C                   CALL      'XSRDA8'
152400970526     C                   PARM                    WLBDA8
152500970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
152600970526     C*
152700970526     C* IMPOSTA I PARAMETRI DI INGRESSO
152800970528     C                   MOVEL     D00EPA        VIDPPA                         *PARTENZA/ARRIVO
152900970528     C                   Z-ADD     D00ETP        VIDPTP                         *TERMINAL PARTENZA
153000970528     C                   Z-ADD     D00ETA        VIDPTA                         *TERMINAL ARRIVO
153100980107     C                   Z-ADD     *ZEROS        VIDPTE                         *TERMINAL ECCEZIONE
153200970528     C                   Z-ADD     *ZEROS        VIDPDD                         *DATA DECORRENZA
153300970528IF  1C     D00EDD        IFGT      *ZEROS
153400970528     C                   Z-ADD     D00EDD        G08INV
153500970528     C                   Z-ADD     *ZEROS        G08DAT
153600970528     C                   MOVEL     '3'           G08ERR
153700970528     C                   CALL      'XSRDA8'
153800970528     C                   PARM                    WLBDA8
153900970528     C                   Z-ADD     G08DAT        VIDPDD
154000120903     C* se non è stata passata la data decorrenza, la imposto a video = oggi
154100120903E   1C                   ELSE
154200120903     C                   EVAL      VidPDD = %dec(%date():*eur)
154300120903E   1C                   ENDIF
154400970526     C                   EXSR      CHKPAR
154500970526IF  1C     ERRL00        IFEQ      '1'
154600970526     C                   MOVEL     '1'           D00ERR
154700970526     C                   MOVEL     DSMSMS        D00MSG
154800970526X   1C                   ELSE
154900970528     C                   MOVEL     VIDPPA        DEPEPA                         *DEPOSITI
155000970528     C                   Z-ADD     VIDPTP        DEPTFP
155100970528     C                   Z-ADD     VIDPTA        DEPTFA
155200980107     C                   Z-ADD     *ZEROS        DEPTFE
155300970528     C                   Z-ADD     VIDPDD        DEPDDE
155400970526E   1C                   ENDIF
155500970526     C*
155600970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
155700970526     C                   EXSR      CHKFOP
155800970526     C*
155900120903     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
156000970530IF  1C     DEPOP0        IFEQ      'P01'
156100970530     C                   MOVEL     DEC(3)        VIDING
156200970530E   1C                   ENDIF
156300970530IF  1C     DEPOP0        IFEQ      'P02'
156400970526     C                   MOVEL     DEC(1)        VIDING
156500970530E   1C                   ENDIF
156600970530IF  1C     DEPOP0        IFEQ      'P05'
156700970526     C                   MOVEL     DEC(2)        VIDING
156800970526E   1C                   ENDIF
156900970526     C*
157000970526     C                   ENDSR
157100970526     C*------------------------------------------------------------------------*
157200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
157300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
157400970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
157500970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
157600970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
157700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
157800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
157900970526     C*------------------------------------------------------------------------*
158000970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
158100970526     C*------------------------------------------------------------------------*
158200970526     C     POSSFL        BEGSR
158300970526     C*
158400970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
158500970526     C                   EXSR      CLRSFL
158600970526     C*
158700970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
158800970526     C                   EXSR      SETFIL
158900970526     C*
159000970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
159100970526     C     SFLEOF        IFEQ      0
159200970526     C                   EXSR      CARSFL
159300970526     C                   END
159400970526     C*
159500970526     C                   ENDSR
159600970526     C*------------------------------------------------------------------------*
159700970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
159800970526     C*------------------------------------------------------------------------*
159900970526     C     ENTATT        BEGSR
160000970526     C*
160100970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
160200970526     C                   Z-ADD     0             OPZOK             1 0
160300970526     C*
160400970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
160500970526     C                   Z-ADD     0             SFLVAR            5 0
160600970526     C*
160700970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
160800970526     C                   Z-ADD     0             SFLERR
160900970526     C*
161000970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
161100970526     C                   Z-ADD     SFLULT        WFLULT
161200970526     C*
161300970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
161400970528     C                   READC     SI17D05                                98
161500970526     C     *IN98         DOWEQ     '0'
161600970526     C*
161700970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
161800970526     C                   ADD       1             SFLVAR
161900970526     C*
162000970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
162100970526     C     OPZOK         IFEQ      0
162200970526     C*
162300970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
162400970526     C     OPZKEY        IFNE      *BLANKS
162500970526     C                   EXSR      OPZCON
162600970526     C                   END
162700970526     C*
162800970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
162900970526     C*            2. PULIZIA SCELTA IN SFL
163000970526     C     OPZOK         IFEQ      0
163100970526     C                   MOVEL     '0'           *IN41
163200970526     C                   MOVEL     *BLANKS       OPZKEY
163300970526     C                   ELSE
163400970526     C*
163500970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
163600970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
163700970526     C                   MOVEL     '1'           *IN41
163800970526     C                   Z-ADD     SFLULT        SFLERR
163900970526     C                   END
164000970526     C                   ELSE
164100970526     C*
164200970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
164300970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
164400970526     C                   MOVEL     '1'           *IN41
164500970526     C                   END                                                    OPZOK=0
164600970526     C*
164700970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
164800970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
164900970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
165000970526     C     OPZOK         IFNE      0
165100970526     C                   Z-ADD     SFLERR        SFLNBR
165200970526     C                   ELSE
165300970526     C                   Z-ADD     SFLULT        SFLNBR
165400970526     C                   END
165500970526     C*
165600970526     C* AGGIORNO RECORD SFL
165700970528     C                   UPDATE    SI17D05
165800970526     C*
165900970528     C                   READC     SI17D05                                98
166000970526     C                   END                                                    DO WHILE
166100970526     C*
166200970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
166300970526     C                   Z-ADD     WFLULT        SFLULT
166400970526     C*
166500970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
166600970526     C     SFLVAR        IFEQ      0
166700970526     C*---                 MOVEL'1'       FINL00
166800970526     C                   END
166900970526     C*
167000970526     C                   ENDSR
167100970526     C*------------------------------------------------------------------------*
167200970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
167300970526     C*------------------------------------------------------------------------*
167400970526     C     CLRSFL        BEGSR
167500970526     C*
167600970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
167700970526     C* (0=NON FINE FILE  1=FINE FILE)
167800970526     C                   Z-ADD     0             SFLEOF
167900970526     C*
168000970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
168100970526     C                   Z-ADD     0             SFLULT
168200970526     C*
168300970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
168400970526     C                   MOVEL     '1'           *IN40
168500970528     C                   WRITE     SI17D06
168600970526     C                   MOVEL     '0'           *IN40
168700970526     C*
168800970526     C                   ENDSR
168900970526     C*------------------------------------------------------------------------*
169000970526     C* CARSFL - CARICAMENTO SFL
169100970526     C*------------------------------------------------------------------------*
169200970526     C     CARSFL        BEGSR
169300970526     C*
169400970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
169500970526     C     SFLULT        IFEQ      0
169600970526     C                   EXSR      SETFIL
169700970526     C                   END
169800970526     C*
169900970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
170000970526     C                   Z-ADD     SFLULT        WFLULT
170100970526     C*
170200970526     C* CICLO:
170300970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
170400970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
170500970526     C                   Z-ADD     1             SFLRIG
170600970526     C     SFLRIG        DOWLE     SFLPAG
170700970526     C     SFLEOF        ANDEQ     0
170800970526     C*
170900970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
171000970526     C                   MOVEL     *BLANKS       OPZKEY
171100970526     C*
171200970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
171300970526     C                   EXSR      IRESFL
171400970526     C*
171500970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
171600970526     C                   ADD       1             SFLULT
171700970526     C*
171800970526     C* SCRITTURA RECORD SFL
171900970528     C                   WRITE     SI17D05
172000970526     C                   ADD       1             SFLRIG
172100970526     C*
172200970526     C* LETTURA FILE DA CARICARE
172300970526     C                   EXSR      LETFIL
172400060706     c* se cambiato tipo, lascio una riga vuota se non ho finito la
172500060706     c* pagina
172600060706     c                   if        caeepa<>videpa     and sfleof=0 and
172700060706     C                             sflrig<=sflpag
172800060706     c                   seton                                        01
172900060706     C                   MOVEL     *BLANKS       OPZKEY
173000060706     c                   clear                   vidatb
173100060706     c                   clear                   videpa
173200060706     c                   clear                   vidtfp
173300060706     c                   clear                   viddtp
173400060706     c                   clear                   vidtfa
173500060706     c                   clear                   viddta
173600060706     c                   clear                   vidtfe
173700060706     c                   clear                   viddte
173800060706     c                   clear                   viddde
173900060706     c                   clear                   viddsc
174000060706     C* SCRITTURA RECORD SFL
174100060706     C                   ADD       1             SFLULT
174200060706     C                   WRITE     SI17D05
174300060706     C                   ADD       1             SFLRIG
174400060706     c                   setoff                                       01
174500060706     C                   ENDif                                                  DO WHILE
174600060706     C                   END                                                    DO WHILE
174700970526     C*
174800970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
174900970526     C     WFLULT        IFNE      SFLULT
175000970526     C                   Z-ADD     WFLULT        SFLNBR
175100970526     C                   ADD       1             SFLNBR
175200970526     C                   END
175300970526     C*
175400970526     C                   ENDSR
175500970526     C*------------------------------------------------------------------------*
175600970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
175700970526     C*------------------------------------------------------------------------*
175800970526     C     OUTSFL        BEGSR
175900970526     C*
176000970526     C* WRITE RECORD TESTATA VIDEO
176100970528     C                   WRITE     SI17D01
176200970526     C*
176300970526     C* VISUALIZZAZIONE MESSAGGIO
176400970526     C                   EXSR      SNDMSG
176500970526     C*
176600970526     C* WRITE RECORD OPZIONI VIDEO
176700970528     C                   WRITE     SI17D02
176800970526     C*
176900970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
177000970528     C                   WRITE     SI17D03
177100970526     C*
177200970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
177300970528     C                   WRITE     SI17D04
177400970526     C*
177500970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
177600970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
177700970526     C     SFLEOF        IFEQ      0
177800970526     C                   MOVEL     '0'           *IN42
177900970526     C                   ELSE
178000970526     C*
178100970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
178200970526     C                   MOVEL     '1'           *IN42
178300970526     C                   END
178400970526     C*
178500970526     C* WRITE RECORD TASTI FUNZIONALI
178600970528     C                   WRITE     SI17D09
178700970526     C*
178800970526     C* CASO DI SFL VUOTO
178900970526     C     SFLULT        IFLE      0
179000970526     C*
179100970526     C*    - WRITE RECORD VUOTO
179200970528     C                   WRITE     SI17D0A
179300970528     C                   READ      SI17D0A                                99
179400970526     C*
179500970526     C                   ELSE
179600970526     C*
179700970526     C* CASO DI SFL CON RECORDS
179800970526     C*
179900970526     C*    - WRITE RECORD CONTROLLO SFL
180000970528     C                   WRITE     SI17D06
180100970528     C                   READ      SI17D06                                99
180200970526     C                   END
180300970526     C*
180400970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
180500970528     C                   READ      SI17D03                                99
180600970526     C*
180700970526     C                   ENDSR
180800970526     C*------------------------------------------------------------------------*
180900970526     C* SNDMSG - MANDA MESSAGGIO
181000970526     C*------------------------------------------------------------------------*
181100970526     C     SNDMSG        BEGSR
181200970526     C*
181300970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
181400970526     C                   MOVEL     '1'           *IN40
181500970528     C                   WRITE     SI17DMC
181600970526     C                   MOVEL     '0'           *IN40
181700970526     C*
181800970526     C* VISUALIZZAZIONE MESSAGGIO
181900970526     C                   Z-ADD     0             MSGULT
182000970526     C                   CALL      'XMSG'        XMSGPA
182100970526     C     DSMSER        IFEQ      0
182200970526     C                   MOVEL     DSMSKE        MSGKEY
182300970526     C                   END
182400970526     C                   Z-ADD     1             MSGULT
182500970528     C                   WRITE     SI17DMS                                      SFL MESS
182600970528     C                   WRITE     SI17DMC                                      CONTR MESS
182700970526     C*
182800970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
182900970526     C                   MOVEL     *BLANKS       DSMSMS
183000970526     C*
183100970526     C                   ENDSR
183200970526     C*------------------------------------------------------------------------*
183300970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
183400970526     C*------------------------------------------------------------------------*
183500970526     C     FUNCON        BEGSR
183600970526     C*
183700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
183800970526     C                   EXSR      FUNVAR
183900970526     C*
184000970526     C* PULISCO CAMPI DI LAVORO
184100970526     C                   Z-ADD     0             I                 5 0
184200970526     C                   Z-ADD     0             W                 5 0
184300970526     C*
184400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
184500970526     C     FUNKEY        IFNE      *BLANKS
184600970526     C     1             DO        24            I
184700970526     C                   MOVEL     FUD(I)        FUDRIG
184800970526     C*
184900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
185000970526     C     FUNKEY        IFEQ      FUDFUN
185100970526     C     FUDATT        ANDEQ     'S'
185200970526     C                   Z-ADD     I             W
185300970526     C                   END
185400970526     C                   END                                                    DO WHILE
185500970526     C*
185600970526     C* FUNZIONE NON ATTIVA (ERRORE)
185700970526     C     W             IFEQ      0
185800970526     C*
185900970526     C* EMISSIONE MESSAGGIO DI ERRORE
186000970526     C                   MOVEL     MSG(2)        DSMSMS
186100970526     C                   END                                                    W=0
186200970526     C                   END                                                    FUNKEY NE *BLANKS
186300970526     C*
186400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
186500970526     C     W             IFNE      0
186600970526     C     FUNKEY        OREQ      *BLANKS
186700970526     C*
186800970526     C     FUNKEY        IFEQ      *BLANKS
186900970526     C                   EXSR      ENTGES
187000970526     C                   END
187100970526     C     FUNKEY        IFEQ      '03'
187200970526     C                   EXSR      F03GES
187300970526     C                   END
187400970526     C     FUNKEY        IFEQ      '05'
187500970526     C                   EXSR      F05GES
187600970526     C                   END
187700970526     C     FUNKEY        IFEQ      '06'
187800970526     C                   EXSR      F06GES
187900970526     C                   END
188000970526     C     FUNKEY        IFEQ      '12'
188100970526     C                   EXSR      F12GES
188200970526     C                   END
188300970528     C     FUNKEY        IFEQ      '21'
188400970528     C                   EXSR      F21GES
188500970528     C                   END
188600970528     C     FUNKEY        IFEQ      '22'
188700970528     C                   EXSR      F22GES
188800970528     C                   END
188900970526     C     FUNKEY        IFEQ      '23'
189000970526     C                   EXSR      F23GES
189100970526     C                   END
189200970526     C     FUNKEY        IFEQ      '24'
189300970526     C                   EXSR      F24GES
189400970526     C                   END
189500970526     C     FUNKEY        IFEQ      'RU'
189600970526     C                   EXSR      RUPGES
189700970526     C                   END
189800970526     C*
189900970526     C                   END
190000970526     C*
190100970526     C                   ENDSR
190200970526     C*------------------------------------------------------------------------*
190300970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
190400970526     C*------------------------------------------------------------------------*
190500970526     C     FUNVAR        BEGSR
190600970526     C*
190700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
190800970526     C     KEY           IFEQ      ENTER
190900970526     C                   MOVEL     *BLANKS       FUNKEY
191000970526     C                   END
191100970526     C     KEY           IFEQ      F1
191200970526     C                   MOVEL     '01'          FUNKEY
191300970526     C                   END
191400970526     C     KEY           IFEQ      F2
191500970526     C                   MOVEL     '02'          FUNKEY
191600970526     C                   END
191700970526     C     KEY           IFEQ      F3
191800970526     C                   MOVEL     '03'          FUNKEY
191900970526     C                   END
192000970526     C     KEY           IFEQ      F4
192100970526     C                   MOVEL     '04'          FUNKEY
192200970526     C                   END
192300970526     C     KEY           IFEQ      F5
192400970526     C                   MOVEL     '05'          FUNKEY
192500970526     C                   END
192600970526     C     KEY           IFEQ      F6
192700970526     C                   MOVEL     '06'          FUNKEY
192800970526     C                   END
192900970526     C     KEY           IFEQ      F7
193000970526     C                   MOVEL     '07'          FUNKEY
193100970526     C                   END
193200970526     C     KEY           IFEQ      F8
193300970526     C                   MOVEL     '08'          FUNKEY
193400970526     C                   END
193500970526     C     KEY           IFEQ      F9
193600970526     C                   MOVEL     '09'          FUNKEY
193700970526     C                   END
193800970526     C     KEY           IFEQ      F10
193900970526     C                   MOVEL     '10'          FUNKEY
194000970526     C                   END
194100970526     C     KEY           IFEQ      F11
194200970526     C                   MOVEL     '11'          FUNKEY
194300970526     C                   END
194400970526     C     KEY           IFEQ      F12
194500970526     C                   MOVEL     '12'          FUNKEY
194600970526     C                   END
194700970526     C     KEY           IFEQ      F13
194800970526     C                   MOVEL     '13'          FUNKEY
194900970526     C                   END
195000970526     C     KEY           IFEQ      F14
195100970526     C                   MOVEL     '14'          FUNKEY
195200970526     C                   END
195300970526     C     KEY           IFEQ      F15
195400970526     C                   MOVEL     '15'          FUNKEY
195500970526     C                   END
195600970526     C     KEY           IFEQ      F16
195700970526     C                   MOVEL     '16'          FUNKEY
195800970526     C                   END
195900970526     C     KEY           IFEQ      F17
196000970526     C                   MOVEL     '17'          FUNKEY
196100970526     C                   END
196200970526     C     KEY           IFEQ      F18
196300970526     C                   MOVEL     '18'          FUNKEY
196400970526     C                   END
196500970526     C     KEY           IFEQ      F19
196600970526     C                   MOVEL     '19'          FUNKEY
196700970526     C                   END
196800970526     C     KEY           IFEQ      F20
196900970526     C                   MOVEL     '20'          FUNKEY
197000970526     C                   END
197100970526     C     KEY           IFEQ      F21
197200970526     C                   MOVEL     '21'          FUNKEY
197300970526     C                   END
197400970526     C     KEY           IFEQ      F22
197500970526     C                   MOVEL     '22'          FUNKEY
197600970526     C                   END
197700970526     C     KEY           IFEQ      F23
197800970526     C                   MOVEL     '23'          FUNKEY
197900970526     C                   END
198000970526     C     KEY           IFEQ      F24
198100970526     C                   MOVEL     '24'          FUNKEY
198200970526     C                   END
198300970526     C     KEY           IFEQ      ROLLUP
198400970526     C                   MOVEL     'RU'          FUNKEY
198500970526     C                   END
198600970526     C     KEY           IFEQ      HELP
198700970526     C                   MOVEL     'HP'          FUNKEY
198800970526     C                   END
198900970526     C*
199000970526     C                   ENDSR
199100970526     C*------------------------------------------------------------------------*
199200970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
199300970526     C*------------------------------------------------------------------------*
199400970526     C     FUNGES        BEGSR
199500970526     C*
199600970526     C* PULISCO CAMPI DI LAVORO
199700970526     C                   Z-ADD     0             W                 5 0
199800970526     C*
199900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
200000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
200100970526     C     1             DO        24            I
200200970526     C                   MOVEL     FUD(I)        FUDRIG
200300970526     C*
200400970526     C* PULIZIA FUNZIONI PRECEDENTI
200500970526     C                   MOVEL     *BLANKS       FUV(I)
200600970526     C*
200700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
200800970526     C* (SALTA F23 E F24)
200900970526     C     FUDATT        IFEQ      'S'
201000970526     C     FUDFUN        ANDNE     '23'
201100970526     C     FUDFUN        ANDNE     '24'
201200970526     C                   ADD       1             W
201300970526     C                   MOVEL     FUDDES        FUV(W)
201400970526     C                   END
201500970526     C                   END                                                    DO WHILE
201600970526     C*
201700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
201800970526     C* ALTRIMENTI DISATTIVO F24
201900970526     C     1             DO        24            I
202000970526     C                   MOVEL     FUD(I)        FUDRIG
202100970526     C*
202200970526     C     FUDFUN        IFEQ      '24'
202300970526     C     W             IFGT      6
202400970526     C                   MOVEL     'S'           FUDATT
202500970526     C                   ELSE
202600970526     C                   MOVEL     *BLANKS       FUDATT
202700970526     C                   END                                                    W GT 8
202800970526     C                   MOVEL     FUDRIG        FUD(I)
202900970526     C                   END
203000970526     C                   END                                                    DO WHILE
203100970526     C*
203200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
203300970526     C     W             DIV       6             I
203400970526     C                   MVR                     Z
203500970526     C*
203600970526     C     Z             IFNE      0
203700970526     C     I             ADD       1             Z
203800970526     C                   ELSE
203900970526     C                   Z-ADD     I             Z
204000970526     C                   END
204100970526     C*
204200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
204300970526     C                   ADD       1             FUNGRU
204400970526     C     FUNGRU        IFGT      Z
204500970526     C     FUNGRU        ORLT      1
204600970526     C                   Z-ADD     1             FUNGRU
204700970526     C                   END
204800970526     C*
204900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
205000970526     C     FUNGRU        IFEQ      1
205100970526     C                   MOVEL     FUV(1)        FUN001
205200970526     C                   MOVEL     FUV(2)        FUN002
205300970526     C                   MOVEL     FUV(3)        FUN003
205400970526     C                   MOVEL     FUV(4)        FUN004
205500970526     C                   MOVEL     FUV(5)        FUN005
205600970526     C                   MOVEL     FUV(6)        FUN006
205700970526     C                   END
205800970526     C*
205900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
206000970526     C     FUNGRU        IFEQ      2
206100970526     C                   MOVEL     FUV(7)        FUN001
206200970526     C                   MOVEL     FUV(8)        FUN002
206300970526     C                   MOVEL     FUV(9)        FUN003
206400970526     C                   MOVEL     FUV(10)       FUN004
206500970526     C                   MOVEL     FUV(11)       FUN005
206600970526     C                   MOVEL     FUV(12)       FUN006
206700970526     C                   END
206800970526     C*
206900970526     C* TERZO GRUPPO DI 6 FUNZIONI
207000970526     C     FUNGRU        IFEQ      3
207100970526     C                   MOVEL     FUV(13)       FUN001
207200970526     C                   MOVEL     FUV(14)       FUN002
207300970526     C                   MOVEL     FUV(15)       FUN003
207400970526     C                   MOVEL     FUV(16)       FUN004
207500970526     C                   MOVEL     FUV(17)       FUN005
207600970526     C                   MOVEL     FUV(18)       FUN006
207700970526     C                   END
207800970526     C*
207900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
208000970526     C     FUNGRU        IFEQ      4
208100970526     C                   MOVEL     FUV(19)       FUN001
208200970526     C                   MOVEL     FUV(20)       FUN002
208300970526     C                   MOVEL     FUV(21)       FUN003
208400970526     C                   MOVEL     FUV(22)       FUN004
208500970526     C                   MOVEL     *BLANKS       FUN005
208600970526     C                   MOVEL     *BLANKS       FUN006
208700970526     C                   END
208800970526     C*
208900970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
209000970526     C                   MOVEL     *BLANKS       FUN007
209100970526     C                   MOVEL     *BLANKS       FUN008
209200970526     C*
209300970526     C     1             DO        24            I
209400970526     C                   MOVEL     FUD(I)        FUDRIG
209500970526     C     FUDFUN        IFEQ      '23'
209600970526     C     FUDATT        ANDEQ     'S'
209700970526     C                   MOVEL     FUDDES        FUN007
209800970526     C                   END
209900970526     C     FUDFUN        IFEQ      '24'
210000970526     C     FUDATT        ANDEQ     'S'
210100970526     C                   MOVEL     FUDDES        FUN008
210200970526     C                   END
210300970526     C                   END                                                    DO
210400970526     C*
210500970526     C                   ENDSR
210600970526     C*------------------------------------------------------------------------*
210700970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
210800970526     C*------------------------------------------------------------------------*
210900970526     C     OPZGES        BEGSR
211000970526     C*
211100970526     C* PULISCO CAMPI DI LAVORO
211200970526     C                   Z-ADD     0             W                 5 0
211300970526     C*
211400970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
211500970526     C* E CARICO TABELLA OPZIONI ATTIVE
211600970526     C     1             DO        24            I
211700970526     C                   MOVEL     OPD(I)        OPDRIG
211800970526     C*
211900970526     C* PULIZIA OPZIONI PRECEDENTI
212000970526     C                   MOVEL     *BLANKS       OPV(I)
212100970526     C*
212200970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
212300970526     C     OPDATT        IFEQ      'S'
212400970526     C                   ADD       1             W
212500970526     C                   MOVEL     OPDDES        OPV(W)
212600970526     C                   END
212700970526     C                   END                                                    DO WHILE
212800970526     C*
212900970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
213000970526     C* ALTRIMENTI DISATTIVO F23
213100970526     C     1             DO        24            I
213200970526     C                   MOVEL     FUD(I)        FUDRIG
213300970526     C*
213400970526     C     FUDFUN        IFEQ      '23'
213500970526     C     W             IFGT      8
213600970526     C                   MOVEL     'S'           FUDATT
213700970526     C                   ELSE
213800970526     C                   MOVEL     *BLANKS       FUDATT
213900970526     C                   END                                                    W GT 8
214000970526     C                   MOVEL     FUDRIG        FUD(I)
214100970526     C                   END
214200970526     C                   END                                                    DO WHILE
214300970526     C*
214400970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
214500970526     C     W             DIV       8             I
214600970526     C                   MVR                     Z                 5 0
214700970526     C*
214800970526     C     Z             IFNE      0
214900970526     C     I             ADD       1             Z
215000970526     C                   ELSE
215100970526     C                   Z-ADD     I             Z
215200970526     C                   END
215300970526     C*
215400970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
215500970526     C                   ADD       1             OPZGRU
215600970526     C     OPZGRU        IFGT      Z
215700970526     C     OPZGRU        ORLT      1
215800970526     C                   Z-ADD     1             OPZGRU
215900970526     C                   END
216000970526     C*
216100970526     C* PRIMO GRUPPO DI 8 OPZIONI
216200970526     C     OPZGRU        IFEQ      1
216300970526     C                   MOVEL     OPV(1)        OPZ001
216400970526     C                   MOVEL     OPV(2)        OPZ002
216500970526     C                   MOVEL     OPV(3)        OPZ003
216600970526     C                   MOVEL     OPV(4)        OPZ004
216700970526     C                   MOVEL     OPV(5)        OPZ005
216800970526     C                   MOVEL     OPV(6)        OPZ006
216900970526     C                   MOVEL     OPV(7)        OPZ007
217000970526     C                   MOVEL     OPV(8)        OPZ008
217100970526     C                   END
217200970526     C*
217300970526     C* SECONDO GRUPPO DI 8 OPZIONI
217400970526     C     OPZGRU        IFEQ      2
217500970526     C                   MOVEL     OPV(9)        OPZ001
217600970526     C                   MOVEL     OPV(10)       OPZ002
217700970526     C                   MOVEL     OPV(11)       OPZ003
217800970526     C                   MOVEL     OPV(12)       OPZ004
217900970526     C                   MOVEL     OPV(13)       OPZ005
218000970526     C                   MOVEL     OPV(14)       OPZ006
218100970526     C                   MOVEL     OPV(15)       OPZ007
218200970526     C                   MOVEL     OPV(16)       OPZ008
218300970526     C                   END
218400970526     C*
218500970526     C* TERZO GRUPPO DI 8 OPZIONI
218600970526     C     OPZGRU        IFEQ      3
218700970526     C                   MOVEL     OPV(17)       OPZ001
218800970526     C                   MOVEL     OPV(18)       OPZ002
218900970526     C                   MOVEL     OPV(19)       OPZ003
219000970526     C                   MOVEL     OPV(20)       OPZ004
219100970526     C                   MOVEL     OPV(21)       OPZ005
219200970526     C                   MOVEL     OPV(22)       OPZ006
219300970526     C                   MOVEL     OPV(23)       OPZ007
219400970526     C                   MOVEL     OPV(24)       OPZ008
219500970526     C                   END
219600970526     C*
219700970526     C                   ENDSR
219800970526     C*------------------------------------------------------------------------*
219900970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
220000970526     C*------------------------------------------------------------------------*
220100970526     C     OPZCON        BEGSR
220200970526     C*
220300970526     C* PULISCO CAMPI DI LAVORO
220400970526     C                   Z-ADD     0             I                 5 0
220500970526     C                   Z-ADD     0             W                 5 0
220600970526     C*
220700970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
220800970526     C     1             DO        24            I
220900970526     C                   MOVEL     OPD(I)        OPDRIG
221000970526     C*
221100970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
221200970526     C     OPZKEY        IFEQ      OPDOPZ
221300970526     C     OPZKEY        OREQ      OPDOP1
221400970526     C*
221500970526     C     OPDATT        IFEQ      'S'
221600970526     C                   MOVEL     OPDOPZ        OPZKEY
221700970526     C                   Z-ADD     I             W
221800970526     C                   END
221900970526     C                   END
222000970526     C*
222100970526     C                   END                                                    DO WHILE
222200970526     C*
222300970526     C* OPZIONE NON ATTIVA (ERRORE)
222400970526     C     W             IFEQ      0
222500970526     C*
222600970526     C* OPZIONE NON OK
222700970526     C                   Z-ADD     1             OPZOK
222800970526     C*
222900970526     C* EMISSIONE MESSAGGIO DI ERRORE
223000970526     C                   MOVEL     MSG(1)        DSMSMS
223100970526     C                   ELSE
223200970526     C*
223300970526     C* OPZIONE ATTIVA (PROCEDI ...)
223400970526     C*
223500970526     C     OPZKEY        IFEQ      ' 1'
223600970526     C                   EXSR      O01GES
223700970526     C                   END
223800970526     C     OPZKEY        IFEQ      ' 2'
223900970526     C                   EXSR      O02GES
224000970526     C                   END
224100970526     C     OPZKEY        IFEQ      ' 4'
224200970526     C                   EXSR      O04GES
224300970526     C                   END
224400970526     C     OPZKEY        IFEQ      ' 5'
224500970526     C                   EXSR      O05GES
224600970526     C                   END
224700970526     C     OPZKEY        IFEQ      ' 7'
224800970526     C                   EXSR      O07GES
224900970526     C                   END
225000060804     C     OPZKEY        IFEQ      'SC'
225100060804     C                   EXSR      OSCGES
225200060804     C                   END
225300131009     C     OPZKEY        IFEQ      '+1'
225400131015     C                   EXSR      Op1GES
225500131009     C                   END
225600970526     C                   END                                                    OPZ.NON ATTIVA
225700970526     C*
225800970526     C                   ENDSR
225900970526     C*------------------------------------------------------------------------*
226000970526     C* NOTE COSTRUZIONE SCHIERE
226100970526     C*------------------------------------------------------------------------*
226200970526     C*
226300970526     C* -MSG-MESSAGGI
226400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
226500970526     C* (NON TOCCARE MSG,1 E MSG,2)
226600970526     C*
226700970526     C* -CMD-COMANDI
226800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
226900970526     C*
227000970526     C* -OPD-OPZIONI
227100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
227200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
227300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
227400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
227500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
227600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
227700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
227800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
227900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
228000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
228100970526     C*
228200970526     C* -FUD-FUNZIONI
228300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
228400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
228500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
228600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
228700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
228800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
228900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
229000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
229100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
229200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
229300970526     O*------------------------------------------------------------------------*
229400970526** DEC - DECODIFICHE
229500970526    GESTIONE                                                     1
229600970526INTERROGAZIONE                                                   2
229700970530     SCELTA                                                      3
229800060929ScadenzaTerminal
2299009705265
230000970526** MSG - MESSAGGI
230100970528TISI17R- Opzione non gestita                                                   1
230200970528TISI17R- Tasto funzionale non gestito                                          2
230300970528TISI17R- Non abilitato alla gestione di questo CAP                             3
230400970528TISI17R- Record annullato. Consentito solo il ripristino                       4
230500970528TISI17R- Record non annullato. Impossibile ripristinare                        5
230600970528TISI17R- Accettato solo un parametro di inizio lista                           6
230700131009Opz. "SC=ScadenzaT" o "+1=PosticipaD": i record devono avere tipo A o P        7
230800131009Opz. "SC=ScadenzaTerminal": si deve selezionare per Data Decorrenza
230900120912La data scadenza nuovo terminal deve essere > scadenza terminal precedente     9
231000060929TISI17R- Terminal / linea errati                                              10
231100120913Opz. SC=Scadenza terminal: i record devono avere lo stesso tipo               11
231200970528TISI17R- Data errata                                                          12
231300131009Opz. "SC=ScadenzaT" o "+1=PosticipaD": i record non devono essere scaduti     13
231400120921Nuova data scadenza non può essere > 31 gg dalla precedente                   14
231500131009Opz."SC=ScadenzaT" o "+1=PosticipaD":i record devono essere gli ultimi decorr 15
231600120913La nuova scadenza deve essere < o = a tutte le scadenze selezionate           16
231700120921Data scadenza < oggi. ENTER forza                                             17
231800130913Aggiornamento non effettuabile per le filiali:
231900970528TISI17R- 19                                                                   19
232000970528TISI17R- 20                                                                   20
232100970528** CMD - COMANDI CL
232200970528WRKSPLF
232300970528
232400970528
232500970528
232600970528
232700970526** OPD - OPZIONI
232800970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
232900970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
233000970526                   -3 -     (S/N)=N                        opz. 03¬
233100970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
233200970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
233300970526                   -6 -     (S/N)=N                        opz. 06¬
233400970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
233500970527                   -8 -     (S/N)=N                        opz. 08¬
233600970527                   -9 -     (S/N)=N                        opz. 09¬
233700060929SC=Scadenza termin -10-     (S/N)=N                        opz. 10¬
233800131008+1=PosticipaDecorr -11-     (S/N)=N                        opz. 11¬
233900970526                   -  -     (S/N)=N                        opz. 12¬
234000970526                   -  -     (S/N)=N                        opz. 13¬
234100970526                   -  -     (S/N)=N                        opz. 14¬
234200970526                   -  -     (S/N)=N                        opz. 15¬
234300970526                   -  -     (S/N)=N                        opz. 16¬
234400970526                   -  -     (S/N)=N                        opz. 17¬
234500970526                   -  -     (S/N)=N                        opz. 18¬
234600970526                   -  -     (S/N)=N                        opz. 19¬
234700970526                   -  -     (S/N)=N                        opz. 20¬
234800970526                   -  -     (S/N)=N                        opz. 21¬
234900970526                   -  -     (S/N)=N                        opz. 22¬
235000970526                   -  -     (S/N)=N                        opz. 23¬
235100970526                   -  -     (S/N)=N                        opz. 24¬
235200970526** FUD - FUNZIONI
235300970526                   -        (S/N)=N                        funz.01¬
235400970526                   -        (S/N)=N                        funz.02¬
235500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
235600970526                   -        (S/N)=N                        funz.04¬
235700970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
235800020506F06=Inserimento    -        (S/N)=S                        funz.06¬
235900970526                   -        (S/N)=N                        funz.07¬
236000970527                   -        (S/N)=N                        funz.08¬
236100970527                   -        (S/N)=N                        funz.09¬
236200970527                   -        (S/N)=N                        funz.10¬
236300970526                   -        (S/N)=N                        funz.11¬
236400970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
236500970526                   -        (S/N)=N                        funz.13¬
236600970526                   -        (S/N)=N                        funz.14¬
236700970526                   -        (S/N)=N                        funz.15¬
236800970526                   -        (S/N)=N                        funz.16¬
236900970526                   -        (S/N)=N                        funz.17¬
237000970526                   -        (S/N)=N                        funz.18¬
237100970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
237200970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
237300980512F21=Stampa elenco  -        (S/N)=S                        funz.21¬
237400970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
237500970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
237600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
