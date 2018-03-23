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
011000060929     D SAVING          S                   LIKE(VIDING)
011100060929     D SAVult          S                   LIKE(sflult)
011200060929     D SAVepa          S                   LIKE(videpa)
011300970526     C*------------------------------------------------------------------------*
011400970526     C* MAIN LINES
011500970526     C*------------------------------------------------------------------------*
011600970526     C*
011700970526     C* SE NON CI SONO ERRORI, PROSEGUE
011800970526IF  1C     ERRL00        IFNE      '1'
011900970526     C*
012000970526     C* CARICA IL SFILE
012100970526     C                   EXSR      POSSFL
012200970526     C*
012300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012700970526     C*
012800970526     C* REIMPOSTA ERRORE DI PROCERURA
012900970526     C                   MOVEL     '0'           ERRL00
013000970526     C*
013100970526     C* EMETTE IL SFILE
013200970526     C                   EXSR      OUTSFL
013300970526     C*
013400970526     C* GESTIONE TASTI FUNZIONALI
013500970526     C                   EXSR      FUNCON
013600970526     C*
013700970526E   2C                   ENDDO
013800970527     C*
013900970527     C* CI SONO ERRORI IN ENTRATA
014000970527X   1C                   ELSE
014100970527     C                   MOVEL     '1'           D00ERR
014200970527     C                   MOVEL     DSMSMS        D00MSG
014300970526E   1C                   ENDIF
014400970526     C* FINE PGM
014500970526     C                   SETON                                        LR
014600970526     C*------------------------------------------------------------------------*
014700970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
014800970526     C*------------------------------------------------------------------------*
014900970526     C     F03GES        BEGSR
015000970526     C*
015100970526     C* IMPOSTA "PREMUTO F03"
015200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015300970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
015400970526     C*
015500970526     C                   ENDSR
015600970526     C*------------------------------------------------------------------------*
015700970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
015800970526     C*------------------------------------------------------------------------*
015900970526     C     F05GES        BEGSR
016000970526     C*
016100970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
016200970526     C*
016300970526     C                   ENDSR
016400970526     C*------------------------------------------------------------------------*
016500970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
016600970526     C*------------------------------------------------------------------------*
016700970526     C     F06GES        BEGSR
016800970526     C*
016900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
017000970602     C                   MOVEL     *BLANKS       DEPA
017100970602     C                   Z-ADD     *ZEROS        DETP
017200970602     C                   Z-ADD     *ZEROS        DETA
017300970602     C                   Z-ADD     *ZEROS        DEDD
017400970602     C                   MOVEL     DEPOP0        WOP0
017500970529     C                   MOVEL     'F06'         WOP1
017600970528     C                   MOVEL     'TISI18R'     WPGM              8
017700970526     C                   EXSR      LANPGM                                       *LANCIA PGM
017800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017900970526E   1C                   ENDIF
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300970526     C* F12GES - GESTIONE F12 -> RITORNO
018400970526     C*------------------------------------------------------------------------*
018500970526     C     F12GES        BEGSR
018600970526     C*
018700970526     C* IMPOSTA "PREMUTO F12"
018800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
018900970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
019000970526     C*
019100970526     C                   ENDSR
019200970528     C*------------------------------------------------------------------------*
019300970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
019400970528     C*------------------------------------------------------------------------*
019500970528     C     F21GES        BEGSR
019600970612     C*
019700980206     C                   MOVEL     *BLANKS       DEPA
019800980206     C                   Z-ADD     *ZEROS        DETP
019900980206     C                   Z-ADD     *ZEROS        DETA
020000980206     C                   Z-ADD     *ZEROS        DEDD
020100970612     C                   MOVEL     DEPOP0        WOP0
020200970612     C                   MOVE      '03'          WOP0
020300970612     C                   MOVEL     'F21'         WOP1
020400970612     C                   MOVEL     'TISI19R'     WPGM              8
020500970612     C                   EXSR      LANPGM
020600970528     C*
020700970528     C                   ENDSR
020800970528     C*------------------------------------------------------------------------*
020900970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
021000970528     C*------------------------------------------------------------------------*
021100970528     C     F22GES        BEGSR
021200970528     C*
021300970528     C                   Z-ADD     80            LENGH            15 5
021400970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
021500970528     C                   PARM                    CMD(1)
021600970528     C                   PARM                    LENGH
021700970528     C*
021800970528     C                   ENDSR
021900970526     C*------------------------------------------------------------------------*
022000970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
022100970526     C*------------------------------------------------------------------------*
022200970526     C     F23GES        BEGSR
022300970526     C*
022400970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022500970526     C*
022600970526     C                   ENDSR
022700970526     C*------------------------------------------------------------------------*
022800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022900970526     C*------------------------------------------------------------------------*
023000970526     C     F24GES        BEGSR
023100970526     C*
023200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023300970526     C*
023400970526     C                   ENDSR
023500970526     C*------------------------------------------------------------------------*
023600970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023700970526     C*------------------------------------------------------------------------*
023800970526     C     RUPGES        BEGSR
023900970526     C*
024000970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
024100970526     C*
024200970526     C                   ENDSR
024300970526     C*------------------------------------------------------------------------*
024400970526     C* ENTGES - GESTIONE ENTER -> INVIO
024500970526     C*------------------------------------------------------------------------*
024600970526     C     ENTGES        BEGSR
024700970526     C*
024800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
024900970526     C                   EXSR      NEWPOS
025000970526     C*
025100970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025300970526IF  2C     SWPOSI        IFNE      0
025400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
025500970526X   2C                   ELSE
025600970526IF  3C     SFLULT        IFNE      0
025700970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025800970526     C*
025900970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026000970526IF  4C     SWMODI        IFNE      0
026100970526     C                   EXSR      POSSFL
026200970526E   4C                   ENDIF
026300970526E   3C                   ENDIF
026400970526E   2C                   ENDIF
026500970526E   1C                   ENDIF
026600970526     C*
026700970526     C                   ENDSR
026800970526     C*------------------------------------------------------------------------*
026900970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
027000970526     C*------------------------------------------------------------------------*
027100970526     C     O01GES        BEGSR
027200970526     C*
027300970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
027400970528     C                   MOVEL     VIDEPA        D00EPA
027500970602     C                   Z-ADD     VIDTFP        D00ETP
027600970528     C                   Z-ADD     VIDTFA        D00ETA
027700970528     C                   Z-ADD     VIHDDE        D00EDD
027800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
027900970526     C*
028000970526     C                   ENDSR
028100970526     C*------------------------------------------------------------------------*
028200970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
028300970526     C*------------------------------------------------------------------------*
028400970526     C     O02GES        BEGSR
028500970526     C*
028600970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
028700970526IF  1C     VIDATB        IFEQ      'A'
028800970526     C                   MOVEL     '1'           ERRL00
028900970526     C                   MOVEL     MSG(4)        DSMSMS
029000970526E   1C                   ENDIF
029100970527     C* MODIFICA
029200970526IF  1C     ERRL00        IFEQ      '0'
029300970602     C                   MOVEL     VIDEPA        DEPA
029400970602     C                   Z-ADD     VIDTFP        DETP
029500970602     C                   Z-ADD     VIDTFA        DETA
029600970602     C                   Z-ADD     VIHDDE        DEDD
029700970602     C                   MOVEL     DEPOP0        WOP0
029800970526     C                   MOVEL     'O02'         WOP1
029900970528     C                   MOVEL     'TISI18R'     WPGM
030000970526     C                   EXSR      LANPGM
030100970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
030200970526E   1C                   ENDIF
030300970526     C*
030400970526     C                   ENDSR
030500970526     C*------------------------------------------------------------------------*
030600970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
030700970526     C*------------------------------------------------------------------------*
030800970526     C     O04GES        BEGSR
030900970526     C*
031000970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
031100970526IF  1C     VIDATB        IFEQ      'A'
031200970526     C                   MOVEL     '1'           ERRL00
031300970526     C                   MOVEL     MSG(4)        DSMSMS
031400970526E   1C                   ENDIF
031500970527     C* ANNULLAMENTO
031600970526IF  1C     ERRL00        IFEQ      '0'
031700970602     C                   MOVEL     VIDEPA        DEPA
031800970602     C                   Z-ADD     VIDTFP        DETP
031900970602     C                   Z-ADD     VIDTFA        DETA
032000970602     C                   Z-ADD     VIHDDE        DEDD
032100970602     C                   MOVEL     DEPOP0        WOP0
032200970526     C                   MOVEL     'O04'         WOP1
032300970528     C                   MOVEL     'TISI18R'     WPGM
032400970526     C                   EXSR      LANPGM
032500970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
032600970526E   1C                   ENDIF
032700970526     C*
032800970526     C                   ENDSR
032900970526     C*------------------------------------------------------------------------*
033000970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
033100970526     C*------------------------------------------------------------------------*
033200970526     C     O05GES        BEGSR
033300970526     C*
033400970602     C                   MOVEL     VIDEPA        DEPA
033500970602     C                   Z-ADD     VIDTFP        DETP
033600970602     C                   Z-ADD     VIDTFA        DETA
033700970602     C                   Z-ADD     VIHDDE        DEDD
033800970602     C                   MOVEL     DEPOP0        WOP0
033900970526     C                   MOVEL     'O05'         WOP1
034000970528     C                   MOVEL     'TISI18R'     WPGM
034100970526     C                   EXSR      LANPGM
034200970526     C*
034300970526     C                   ENDSR
034400970526     C*------------------------------------------------------------------------*
034500970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
034600970526     C*------------------------------------------------------------------------*
034700970526     C     O07GES        BEGSR
034800970526     C*
034900970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
035000970526IF  1C     VIDATB        IFNE      'A'
035100970526     C                   MOVEL     '1'           ERRL00
035200970526     C                   MOVEL     MSG(5)        DSMSMS
035300970526E   1C                   ENDIF
035400970526     C* RIPRISITINO
035500970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035600970602     C                   MOVEL     VIDEPA        DEPA
035700970602     C                   Z-ADD     VIDTFP        DETP
035800970602     C                   Z-ADD     VIDTFA        DETA
035900970602     C                   Z-ADD     VIHDDE        DEDD
036000970602     C                   MOVEL     DEPOP0        WOP0
036100970526     C                   MOVEL     'O07'         WOP1
036200970528     C                   MOVEL     'TISI18R'     WPGM
036300970526     C                   EXSR      LANPGM
036400970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
036500970526E   1C                   ENDIF
036600970526     C*
036700970526     C                   ENDSR
036800060804     C*------------------------------------------------------------------------*
036900060804     C* OSCGES - GESTIONE OPZIONE SC= scadenza terminal
037000060804     C*------------------------------------------------------------------------*
037100060804     C     OSCGES        BEGSR
037200060929     c* Per impostyare le scadenza terminal selezione tipo terminal
037300060929     c*  e data
037400060929     c                   if        vidppa=' ' or  vidpdd=0
037500060929     C                   MOVEL     '1'           ERRL00
037600060929     C                   MOVEL     MSG(8)        DSMSMS
037700060929     c                   goto      endscade
037800060929     c                   endif
037900060929     c
038000060928     c* Leggo dal sfl tutti i record che hanno "SC" e devono avere
038100060928     c*  lo stesso tipo terminal
038200060929     c                   eval      savING=viding
038300060929     c                   movel     dec(4)        viding
038400060929     c
038500060928     c                   z-add     1             sflult
038600060929     c                   clear                   nrr               5 0
038700060929     c                   clear                   savepa
038800060929     c* pulisco il sfl di scadenza terminal
038900060928     c                   seton                                        70
039000060929     c                   write     si17c11
039100060928     c                   setoff                                       70
039200060929     c
039300060929     c                   do        wflult        sflult
039400060928     c     sflult        chain     si17d05
039500060928     c                   if        opzkey='SC'
039600060928     c* La prima volta salvo il record, poi se divero il campo EPA
039700060928     c*  errore
039800060928     c                   if        savepa=*blanks
039900060928     c                   eval      savepa=videpa
040000060928     c                   eval      v11epa=videpa
040100060928     c                   else
040200060928     c*  Errore
040300060929     c                   if        savepa<>videpa
040400060928     C                   MOVEL     '1'           ERRL00
040500060928     C                   MOVEL     MSG(7)        DSMSMS
040600060929     c                   leave
040700060928E   1C                   ENDIF
040800060928     c                   endif
040900060929     c
041000060928     c                   movel     vidtfp        v11tfp
041100060929     c                   movel     viddtp        v11dtp
041200060929     c                   movel     vidtfa        v11tfa
041300060929     c                   movel     viddta        v11dta
041400060929     c                   movel     vidtfe        v11tfe
041500060929     c                   movel     viddte        v11dte
041600060929     c                   add       1             nrr
041700060929     c                   write     si17s11
041800060929     c                   endif
041900060929     c                   enddo
042000060928     c*
042100060929     c                   z-add     1             rec1
042200060929     c                   eval      v11nwrip='SI'
042300060929     c                   clear                   v11dsc
042400060929     c                   clear                   v11nwdsc
042500060929     c                   clear                   v11nwtfe
042600060929     c                   if        errl00='0'
042700060929     c                   write     si17d01
042800060929     c                   write     si17z11
042900060929     c
043000060929     c     for11         tag
043100060929     c                   exfmt     si17c11
043200060929     c                   setoff                                       2890
043300060929     c                   setoff                                       646566
043400060929     C                   MOVEL     '0'           ERRL00
043500060929     c* F12 - ritorno
043600060929     c                   if        *inkl
043700060929     c                   goto      endscade
043800060929     c                   endif
043900060929     c* controllo le selezioni fatte
044000060929     c                   exsr      CTRSCADE
044100060929     c
044200060929     c                   if        errl00='1' or not *inkf
044300060929     c                   goto      for11
044400060929     c                   endif
044500060929     c                   endif
044600060928     c
044700060929     c     endscade      tag
044800060929     c
044900060929     c                   if        dsmsms<>msg(8)
045000060929     c                   movel     saving        viding
045100060929     c                   z-add     savult        sflult
045200060929     c                   movel     '1'           swmodi
045300060929     c                   else
045400060929     c                   movel     '1'           opzok
045500060929     c                   endif
045600060929     C                   ENDSR
045700060929     C*------------------------------------------------------------------------*
045800060929     C* CONTROLLO DATI PER SCADENZA TERMINAL
045900060929     C*------------------------------------------------------------------------*
046000060929     C     CTRSCADE      BEGSR
046100060929     c* Data scadenza
046200060929     C     *LIKE         DEFINE    VIDDSC        DSCVID
046300060929IF  1C     ERRL00        IFEQ      '0'
046400060929     C                   Z-ADD     VIDDSC        WDA8
046500060929     C                   Z-ADD     *ZEROS        DA8W
046600060929     C                   EXSR      CHKDA8
046700060929     C                   Z-ADD     WDA8          VIDDSC
046800060929     C                   Z-ADD     DA8W          DSCVID
046900060929IF  2C     ERRL00        IFEQ      '1'
047000060929     C                   SETON                                        6428
047100060929     C                   MOVEL     MSG(12)       vidmsg
047200060929E   2C                   ENDIF
047300060929E   1C                   ENDIF
047400060929     c
047500060929     c* Nuova Data scadenza
047600060929     C     *LIKE         DEFINE    V11nwDSC      DSC11VID
047700060929IF  1C     ERRL00        IFEQ      '0'
047800060929     C                   Z-ADD     V11nwDSC      WDA8
047900060929     C                   Z-ADD     *ZEROS        DA8W
048000060929     C                   EXSR      CHKDA8
048100060929     C                   Z-ADD     WDA8          V11nwDSC
048200060929     C                   Z-ADD     DA8W          DSC11VID
048300060929IF  2C     ERRL00        IFEQ      '1'
048400060929     C                   SETON                                        6528
048500060929     C                   MOVEL     MSG(12)       vidmsg
048600060929E   2C                   ENDIF
048700060929E   1C                   ENDIF
048800060929     c
048900060929     c* La nuova data scadenza deve essere > data scadenza
049000060929    1c                   if        dsc11vid<=dscvid
049100060929     C                   SETON                                        6428
049200060929     C                   MOVEL     MSG(9)        vidmsg
049300060929E   1C                   ENDIF
049400060929     C*--------------------
049500060929     C* TERMINAL / LINEA ECCEZIONE
049600060929     C*--------------------
049700060929     c                   clear                   v11nwdte
049800060929IF  1C     ERRL00        IFEQ      '0'
049900060929     C                   MOVEL     V11nwTFE      WLIN              3
050000060929     C                   MOVEL     *BLANKS       WDLIN
050100060929     C                   EXSR      CHKLIN
050200060929     C                   MOVEL     WLIN          V11nwTFE
050300060929     C                   MOVEL     WDLIN         V11nwdtE
050400060929IF  2C     ERRL00        IFEQ      '1'
050500060929     C                   SETON                                        6628
050600060929     C                   MOVEL     MSG(10)       vidmsg
050700060929E   2C                   ENDIF
050800060929E   1C                   ENDIF
050900060929     c
051000060929     C                   ENDSR
051100060929     C*------------------------------------------------------------------------*
051200060929     C* CHKDA8 - CONTROLLA LA DATA (8)
051300060929     C*------------------------------------------------------------------------*
051400060929     C     CHKDA8        BEGSR
051500060929     C*
051600060929     C     *LIKE         DEFINE    VIDDDE        WDA8
051700060929     C     *LIKE         DEFINE    VIDDDE        DA8W
051800060929     C*
051900060929     C                   Z-ADD     WDA8          G08DAT
052000060929     C                   Z-ADD     *ZEROS        G08INV
052100060929     C                   MOVEL     '0'           G08ERR
052200060929     C                   CALL      'XSRDA8'
052300060929     C                   PARM                    WLBDA8
052400060929     C                   Z-ADD     G08DAT        WDA8
052500060929     C                   Z-ADD     G08INV        DA8W
052600060929IF  1C     G08ERR        IFEQ      '1'
052700060929     C                   MOVEL     '1'           ERRL00
052800060929E   1C                   ENDIF
052900060929     C*
053000060929     C                   ENDSR
053100060929     C*------------------------------------------------------------------------*
053200060929     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
053300060929     C*------------------------------------------------------------------------*
053400060929     C     CHKLIN        BEGSR
053500060929     C*
053600060929     C     *LIKE         DEFINE    Viddta        WDLIN
053700060929     C*
053800060929     C* RICHIESTO ELENCO
053900060929     C                   SETOFF                                           28
054000060929     C     '?'           SCAN      WLIN                                   28
054100060929IF  1C     *IN28         IFEQ      *ON
054200060929     C                   MOVEL     *BLANKS       §COD1             3
054300060929     C                   MOVEL     *BLANKS       §TIP              1
054400060929     C                   MOVEL     *BLANKS       §DES1            25
054500060929     C                   MOVEL     *BLANKS       §PRA              3
054600060929     C                   CALL      'TNSD24R'
054700060929     C                   PARM                    §COD1
054800060929     C                   PARM                    §TIP
054900060929     C                   PARM                    §DES1
055000060929     C                   PARM                    §PRA
055100060929     C                   MOVEL     §COD1         WLIN
055200060929     C                   MOVEL     §DES1         WDLIN
055300060929E   1C                   ENDIF
055400060929     C*
055500060929     C* SOLO CARATTERI NUMERICI
055600060929     C                   SETOFF                                       28
055700060929     C                   TESTN                   WLIN                 28
055800060929IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
055900060929     C                   MOVEL     '1'           ERRL00
056000060929X   1C                   ELSE                                                   *E'NUMERO DA TESTN
056100060929     C                   MOVE      WLIN          W001              1
056200060929IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
056300060929     C                   MOVEL     '1'           ERRL00
056400060929X   2C                   ELSE                                                   *NUMERO
056500060929     C*
056600060929     C* CONTROLLA ESISTENZA CODICE
056700060929     C                   MOVEL     WLIN          KORFIL
056800060929     C     KEYORG        CHAIN     AZORG01L                           99
056900060929IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
057000060929     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
057100060929     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
057200060929     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
057300060929X   3C                   ELSE
057400060929     C                   MOVEL     '1'           ERRL00
057500060929E   3C                   ENDIF
057600060929E   2C                   ENDIF
057700060929E   1C                   ENDIF
057800060929     C*
057900060929     C                   ENDSR
058000970526     C*------------------------------------------------------------------------*
058100970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
058200970526     C*------------------------------------------------------------------------*
058300970526     C     NEWPOS        BEGSR
058400970526     C*
058500970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
058600970526     C*         1= RICHIESTO POSIZIONAMENTO
058700970526     C                   Z-ADD     0             SWPOSI            7 0
058800970526     C*
058900970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
059000970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
059100970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
059200970526     C                   Z-ADD     0             SWMODI            1 0
059300970530     C*
059400970530     C* REIMPOSTA INDICATORI DI ERRORE
059500970530     C                   SETOFF                                       808182
059600970530     C                   SETOFF                                       83
059700970526     C*
059800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
059900970528IF  1C     VIDPPA        IFNE      DEPEPA
060000970528     C     VIDPTP        ORNE      DEPTFP
060100970528     C     VIDPTA        ORNE      DEPTFA
060200980107     C     VIDPTE        ORNE      DEPTFE
060300970528     C     VIDPDD        ORNE      DEPDDE
060400970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
060500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
060600970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
060700970528     C                   MOVEL     VIDPPA        DEPEPA                         *DEPOSITI
060800970528     C                   Z-ADD     VIDPTP        DEPTFP
060900970528     C                   Z-ADD     VIDPTA        DEPTFA
061000980107     C                   Z-ADD     VIDPTE        DEPTFE
061100970528     C                   Z-ADD     VIDPDD        DEPDDE
061200970526E   2C                   ENDIF
061300970526E   1C                   ENDIF
061400970526     C*
061500970526     C                   ENDSR
061600970526     C*------------------------------------------------------------------------*
061700970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
061800970526     C*------------------------------------------------------------------------*
061900970528     C     SETFIL        BEGSR
062000970526     C*
062100970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
062200970528IF  1C     WTIPRI        IFEQ      'PAR'                                        *PER PARTENZA
062300970618     C                   MOVEL     VIDPPA        KCEEPA
062400970618     C                   Z-ADD     VIDPTP        KCETFP
062500980108     C                   Z-ADD     VIDPTA        KCETFA
062600970618     C     KEYCAE        SETLL     AZCAE01L                           99
062700970528E   1C                   ENDIF
062800970528     C*
062900970528IF  1C     WTIPRI        IFEQ      'ARR'                                        *PER ARRIVO
063000970618     C                   MOVEL     VIDPPA        KCEEPA
063100970618     C                   Z-ADD     VIDPTA        KCETFA
063200980108     C                   Z-ADD     VIDPTP        KCETFP
063300970618     C     KE2CAE        SETLL     AZCAE02L                           99
063400970528E   1C                   ENDIF
063500980107     C*
063600980107IF  1C     WTIPRI        IFEQ      'ECC'                                        *PER ARRIVO
063700980107     C                   MOVEL     VIDPPA        KCEEPA
063800980107     C                   Z-ADD     VIDPTE        KCETFE
063900980108     C                   Z-ADD     VIDPTA        KCETFA
064000980108     C                   Z-ADD     VIDPTP        KCETFP
064100980107     C     KE5CAE        SETLL     AZCAE05L                           99
064200980107E   1C                   ENDIF
064300970526     C* LETTURA RECORD
064400970526IF  1C     *IN99         IFEQ      *OFF
064500970526     C                   EXSR      LETFIL
064600970526E   1C                   ENDIF
064700970526     C*
064800970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
064900970526IF  1C     *IN99         IFEQ      *ON
065000970526     C                   Z-ADD     1             SFLEOF
065100970526E   1C                   ENDIF
065200970526     C*
065300970526     C                   ENDSR
065400970526     C*------------------------------------------------------------------------*
065500970526     C* LETFIL - LETTURA RECORD
065600970526     C*------------------------------------------------------------------------*
065700970526     C     LETFIL        BEGSR
065800970526     C*
065900970526     C* LEGGE FINO A:
066000970526     C*     - FINE FILE
066100970526     C*     - TROVATO RECORD VALIDO
066200970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
066300970526DO  1C     *IN99         DOWEQ     *OFF
066400970526     C     WRECOK        ANDEQ     'N'
066500970526     C*
066600970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
066700970528IF  2C     WTIPRI        IFEQ      'PAR'                                        *PER PARTENZA
066800970618     C                   READ      AZCAE01L                               99
066900970528E   2C                   ENDIF
067000970526     C*
067100970528IF  2C     WTIPRI        IFEQ      'ARR'                                        *PER ARRIVO
067200970618     C                   READ      AZCAE02L                               99
067300970528E   2C                   ENDIF
067400980107     C*
067500980107IF  2C     WTIPRI        IFEQ      'ECC'                                        *PER ECCEZIONE
067600980107     C                   READ      AZCAE05L                               99
067700980107E   2C                   ENDIF
067800970528     C*
067900970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
068000970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
068100970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
068200970530X   2C                   ELSE
068300970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
068400970530E   2C                   ENDIF
068500970526E   1C                   ENDDO
068600970526     C*
068700970526     C                   ENDSR
068800970526     C*------------------------------------------------------------------------*
068900970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
069000970526     C*------------------------------------------------------------------------*
069100970526     C     CHKREC        BEGSR
069200970526     C*
069300970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
069400970530     C*
069500970530     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
069600970530IF  1C     VIDPDD        IFGT      *ZEROS
069700970618IF  2C     PDDVID        IFGE      CAEDDE                                       *IN DECORRENZA
069800970618     C     PDDVID        ANDLE     CAEDSC                                       *ALLA DATA
069900970530X   2C                   ELSE
070000970530     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
070100970530E   2C                   ENDIF
070200970530E   1C                   ENDIF
070300970526     C*
070400970526     C                   ENDSR
070500970526     C*------------------------------------------------------------------------*
070600970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
070700970526     C*------------------------------------------------------------------------*
070800970526     C     IRESFL        BEGSR
070900970526     C*
071000970526     C                   MOVEL     *BLANKS       OPZKEY
071100970618     C                   Z-ADD     CAEDDE        VIHDDE
071200970618     C                   MOVEL     CAEATB        VIDATB
071300970618     C                   MOVEL     CAEEPA        VIDEPA
071400970618     C                   Z-ADD     CAETFP        VIDTFP
071500970618     C                   Z-ADD     CAETFA        VIDTFA
071600970618     C                   Z-ADD     CAETFE        VIDTFE
071700970618     C                   Z-ADD     CAEDDE        G08INV
071800970618     C                   Z-ADD     *ZEROS        G08DAT
071900970618     C                   MOVEL     '3'           G08ERR
072000970618     C                   CALL      'XSRDA8'
072100970618     C                   PARM                    WLBDA8
072200970618     C                   Z-ADD     G08DAT        VIDDDE
072300970618     C                   Z-ADD     CAEDSC        G08INV
072400970618     C                   Z-ADD     *ZEROS        G08DAT
072500970618     C                   MOVEL     '3'           G08ERR
072600970618     C                   CALL      'XSRDA8'
072700970618     C                   PARM                    WLBDA8
072800970618     C                   Z-ADD     G08DAT        VIDDSC
072900970528     C* DECODIFICHE
073000970618     C                   MOVEL     *BLANKS       VIDDTP
073100970618IF  1C     CAETFP        IFGT      *ZEROS
073200060929     C                   SETOFF                                           30
073300970528     C                   Z-ADD     1             I
073400060929     C     CAETFP        LOOKUP    ORG(I)                                 30
073500060929IF  2C     *IN30         IFEQ      *ON                                          *TROVATO
073600970528     C                   MOVEL     DORG(I)       VIDDTP
073700970618E   2C                   ENDIF
073800970618E   1C                   ENDIF
073900970526     C*
074000970618     C                   MOVEL     *BLANKS       VIDDTA
074100970618IF  1C     CAETFA        IFGT      *ZEROS
074200060929     C                   SETOFF                                           30
074300970528     C                   Z-ADD     1             I
074400060929     C     CAETFA        LOOKUP    ORG(I)                                 30
074500060929IF  2C     *IN30         IFEQ      *ON                                          *TROVATO
074600970528     C                   MOVEL     DORG(I)       VIDDTA
074700970618E   2C                   ENDIF
074800970618E   1C                   ENDIF
074900970528     C*
075000970618     C                   MOVEL     *BLANKS       VIDDTE
075100060929     C                   SETOFF                                           30
075200970528     C                   Z-ADD     1             I
075300060929     C     CAETFE        LOOKUP    ORG(I)                                 30
075400060929IF  1C     *IN30         IFEQ      *ON                                          *TROVATO
075500970528     C                   MOVEL     DORG(I)       VIDDTE
075600970528E   1C                   ENDIF
075700970528     C*
075800970526     C                   ENDSR
075900970526     C*------------------------------------------------------------------------*
076000970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
076100970526     C*------------------------------------------------------------------------*
076200970526     C     CHKPAR        BEGSR
076300970528     C*
076400970528     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER PARTENZA
076500970529IF  1C     VIDPPA        IFEQ      *BLANKS
076600970529     C     VIDPTP        ANDEQ     *ZEROS
076700970528     C     VIDPTA        ANDEQ     *ZEROS
076800980107     C     VIDPTE        ANDEQ     *ZEROS
076900970528     C                   MOVEL     'PAR'         WTIPRI            3
077000970528E   1C                   ENDIF                                                  *ORDINAMENTO RICHIES
077100970528     C*
077200970528     C* CONTROLLA LA DATA DECORRENZA
077300970528     C     *LIKE         DEFINE    VIDPDD        PDDVID                         *DATA IN A/M/G
077400970528     C                   Z-ADD     *ZEROS        PDDVID
077500970528IF  1C     ERRL00        IFEQ      '0'
077600970528IF  2C     VIDPDD        IFGT      *ZEROS
077700970528     C                   Z-ADD     VIDPDD        G08DAT
077800970528     C                   Z-ADD     *ZEROS        G08INV
077900970528     C                   MOVEL     '0'           G08ERR
078000970528     C                   CALL      'XSRDA8'
078100970528     C                   PARM                    WLBDA8
078200970528     C                   Z-ADD     G08DAT        VIDPDD
078300970528     C                   Z-ADD     G08INV        PDDVID
078400970528IF  3C     G08ERR        IFEQ      '1'
078500970528     C                   MOVEL     '1'           ERRL00
078600970528     C                   MOVEL     MSG(12)       DSMSMS
078700970528     C                   SETON                                        83
078800970528E   3C                   ENDIF
078900970528E   2C                   ENDIF
079000970528E   1C                   ENDIF
079100970528     C*
079200970528     C* CONTROLLA IL TIPO RICERCA
079300970528IF  1C     ERRL00        IFEQ      '0'
079400970528IF  2C     VIDPTP        IFNE      *ZEROS
079500980107     C                   MOVEL     'PAR'         WTIPRI
079600970528E   2C                   ENDIF
079700970528E   1C                   ENDIF
079800980107     C*
079900980107IF  1C     ERRL00        IFEQ      '0'
080000980107IF  2C     VIDPTA        IFNE      *ZEROS
080100970528     C                   MOVEL     'ARR'         WTIPRI
080200980107E   2C                   ENDIF
080300980107E   1C                   ENDIF
080400980107     C*
080500980107IF  1C     ERRL00        IFEQ      '0'
080600980107IF  2C     VIDPTE        IFNE      *ZEROS
080700980107     C                   MOVEL     'ECC'         WTIPRI
080800980107E   2C                   ENDIF
080900980107E   1C                   ENDIF
081000970526     C*
081100970526     C                   ENDSR
081200970526     C*------------------------------------------------------------------------*
081300970526     C* LANPGM - LANCIA UN PROGRAMMA
081400970526     C*------------------------------------------------------------------------*
081500970526     C     LANPGM        BEGSR
081600970526     C*
081700970526     C* IMPOSTA LA DS DI PROCEDURA
081800970602     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
081900970602     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
082000970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
082100970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
082200970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
082300970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
082400970602     C                   MOVEL     DEPA          D00EPA                         *PARTENZA/ARRIVO
082500970602     C                   Z-ADD     DETP          D00ETP                         *TERMINAL PARTENZA
082600970602     C                   Z-ADD     DETA          D00ETA                         *TERMINAL ARRIVO
082700970602     C                   Z-ADD     DEDD          D00EDD                         *DECORRENZA
082800970602     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI DATI
082900970530     C                   MOVEL     *BLANKS       D00TSP
083000970530     C                   Z-ADD     *ZEROS        D00TFP
083100970530     C                   MOVEL     *BLANKS       D00LOC
083200970603     C                   MOVEL     *BLANKS       D00SIN
083300970530     C                   MOVEL     *BLANKS       D00SPT
083400970530     C                   Z-ADD     *ZEROS        D00SLP
083500970530     C                   Z-ADD     *ZEROS        D00SLA
083600970530     C                   MOVEL     *BLANKS       D00STS
083700970530     C                   Z-ADD     *ZEROS        D00SDD
083800970526     C*
083900970526     C                   CALL      WPGM
084000970612     C                   PARM                    KPJBA
084100970612     C                   PARM                    DSSI00
084200970526     C*
084300970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
084400970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
084500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
084600970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
084700970526X   1C                   ELSE                                                   *NON PREMUTO F03
084800970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
084900970526E   1C                   ENDIF
085000970526     C*
085100970526     C                   ENDSR
085200970527     C*------------------------------------------------------------------------*
085300970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
085400970527     C*------------------------------------------------------------------------*
085500970527     C     CARTAB        BEGSR
085600970528     C*---
085700970528     C* ORGANIGRAMMA
085800970528     C*---
085900970528     C                   Z-ADD     *ZEROS        I                 5 0
086000970528     C                   Z-ADD     *ZEROS        ORG
086100970528     C                   MOVEL     *BLANKS       DORG
086200970528     C     *LOVAL        SETLL     AZORG01L
086300970528     C                   READ      AZORG01L                               99
086400970528DO  1C     *IN99         DOWEQ     *OFF
086500970528IF  2C     ORGFVA        IFEQ      *BLANKS
086600970528     C     ORGFAG        ANDNE     'V'
086700970528     C                   ADD       1             I
086800970528     C                   Z-ADD     ORGFIL        ORG(I)
086900970528     C                   MOVEL     ORGDES        DORG(I)
087000970528E   2C                   ENDIF
087100970528     C                   READ      AZORG01L                               99
087200970528E   1C                   ENDDO                                                  *FINE ORGANIGRAMMA
087300970527     C*
087400970527     C                   ENDSR
087500970526     C*------------------------------------------------------------------------*
087600970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
087700970526     C*------------------------------------------------------------------------*
087800970526     C     CHKFOP        BEGSR
087900970626     C*
088000970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
088100970626IF  1C     DEPOP0        IFEQ      'P01'
088200970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
088300970626     C                   MOVEL     'S'           OPDATT
088400970626     C                   MOVEL     OPDRIG        OPD(1)
088500970626X   1C                   ELSE
088600970626     C* OPZIONI
088700970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
088800970626     C                   MOVEL     'S'           OPDATT
088900970626     C                   MOVEL     OPDRIG        OPD(5)
089000970626     C* FUNZIONI
089100970626E   1C                   ENDIF
089200970626     C*
089300970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
089400970626IF  1C     DEPOP0        IFEQ      'P02'
089500970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
089600970626     C                   MOVEL     'S'           OPDATT
089700970626     C                   MOVEL     OPDRIG        OPD(2)
089800970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
089900970626     C                   MOVEL     'S'           OPDATT
090000970626     C                   MOVEL     OPDRIG        OPD(4)
090100970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
090200970626     C                   MOVEL     'S'           OPDATT
090300970626     C                   MOVEL     OPDRIG        OPD(7)
090400060929     C                   MOVEL     OPD(10)       OPDRIG                         *SCELTA
090500060929     C                   MOVEL     'S'           OPDATT
090600060929     C                   MOVEL     OPDRIG        OPD(10)
090700970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
090800970626     C                   MOVEL     'S'           FUDATT
090900970626     C                   MOVEL     FUDRIG        FUD(6)
091000970626E   1C                   ENDIF
091100970526     C*
091200970526     C* GESTIONE OPZIONI A VIDEO
091300970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
091400970526     C                   EXSR      OPZGES
091500970526     C*
091600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
091700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
091800970526     C                   EXSR      FUNGES
091900970526     C*
092000970526     C                   ENDSR
092100970526     C*------------------------------------------------------------------------*
092200970526     C* *INZSR - OPERAZIONI INIZIALI
092300970526     C*------------------------------------------------------------------------*
092400970526     C     *INZSR        BEGSR
092500970526     C*---
092600970526     C* RICEVIMENTO PARAMETRI
092700970526     C*---
092800970526     C     *ENTRY        PLIST
092900970612     C                   PARM                    KPJBA
093000970612     C                   PARM                    DSSI00
093100970526     C*
093200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
093300970526     C     XMSGPA        PLIST
093400970526     C                   PARM                    DSMSMS
093500970526     C                   PARM                    DSMSPG
093600970526     C                   PARM                    DSMSKE
093700970526     C                   PARM                    DSMSER
093800970526     C*
093900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
094000970526     C                   CALL      'XKEY'
094100970526     C                   PARM                    DSKEY
094200970526     C*---
094300970526     C* VARIABILI NON RIFERITE AL DATA BASE
094400970526     C*---
094500970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
094600970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
094700970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
094800970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
094900970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
095000970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
095100970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
095200970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
095300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
095400970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
095500970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
095600970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
095700970529     C     *LIKE         DEFINE    D00OP1        WOP1
095800970602     C     *LIKE         DEFINE    D00EPA        DEPA
095900970602     C     *LIKE         DEFINE    D00ETP        DETP
096000970602     C     *LIKE         DEFINE    D00ETA        DETA
096100970602     C     *LIKE         DEFINE    D00EDD        DEDD
096200970526     C*---
096300970526     C* VARIABILI DI CONTROLLO
096400970526     C*---
096500970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
096600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
096700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
096800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
096900970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
097000970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
097100970526     C*
097200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
097300970528     C                   MOVEL     'TISI17R'     DSMSPG
097400970528     C                   MOVEL     'TISI17R'     MSGQUE
097500970526     C                   MOVEL     *BLANKS       MSGKEY
097600970526     C                   MOVEL     *BLANKS       DSMSMS
097700970526     C                   MOVEL     *BLANKS       DSMSKE
097800970526     C                   Z-ADD     *ZEROS        DSMSER
097900970526     C*---
098000970526     C* VARIABILI RIFERITE AL DATA BASE
098100970526     C*---
098200970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
098300970618     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         *AZCAE00F
098400970618     C     *LIKE         DEFINE    CAETFP        KCETFP
098500970618     C     *LIKE         DEFINE    CAETFA        KCETFA
098600980107     C     *LIKE         DEFINE    CAETFE        KCETFE
098700970618     C     *LIKE         DEFINE    CAEEPA        DEPEPA                         *DEPOSITI
098800970618     C     *LIKE         DEFINE    CAETFP        DEPTFP
098900970618     C     *LIKE         DEFINE    CAETFA        DEPTFA
099000980107     C     *LIKE         DEFINE    CAETFE        DEPTFE
099100970618     C     *LIKE         DEFINE    CAEDDE        DEPDDE
099200970526     C*---
099300970526     C* CHIAVI DI LETTURA
099400970526     C*---
099500970528     C* LETTURA AZORG01L - COMPLETA
099600970528     C     KEYORG        KLIST
099700970528     C                   KFLD                    KORFIL                         *FILIALE
099800970618     C* LETTURA AZCAE01L - PARZIALE
099900970618     C     KEYCAE        KLIST
100000970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
100100970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
100200980108     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
100300970618     C* LETTURA AZCAE02L - PARZIALE
100400970618     C     KE2CAE        KLIST
100500970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
100600970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
100700980108     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
100800980107     C* LETTURA AZCAE05L - PARZIALE
100900980107     C     KE5CAE        KLIST
101000980107     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
101100980107     C                   KFLD                    KCETFE                         *TERMINAL ECCEZIONE
101200980108     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
101300980108     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
101400970527     C*
101500970527     C* CARICA LE TABELLE OCCORRENTI
101600970527     C                   EXSR      CARTAB
101700970526     C*
101800970526     C* DEPOSITA LE OPZIONI DI ENTRATA
101900970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
102000970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
102100970526     C*
102200970526     C* CALCOLA LA DATA CORRENTE
102300970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
102400970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
102500970526     C                   Z-ADD     WN8           G08DAT
102600970526     C                   Z-ADD     *ZEROS        G08INV
102700970526     C                   MOVEL     '0'           G08ERR
102800970526     C                   CALL      'XSRDA8'
102900970526     C                   PARM                    WLBDA8
103000970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
103100970526     C*
103200970526     C* IMPOSTA I PARAMETRI DI INGRESSO
103300970528     C                   MOVEL     D00EPA        VIDPPA                         *PARTENZA/ARRIVO
103400970528     C                   Z-ADD     D00ETP        VIDPTP                         *TERMINAL PARTENZA
103500970528     C                   Z-ADD     D00ETA        VIDPTA                         *TERMINAL ARRIVO
103600980107     C                   Z-ADD     *ZEROS        VIDPTE                         *TERMINAL ECCEZIONE
103700970528     C                   Z-ADD     *ZEROS        VIDPDD                         *DATA DECORRENZA
103800970528IF  1C     D00EDD        IFGT      *ZEROS
103900970528     C                   Z-ADD     D00EDD        G08INV
104000970528     C                   Z-ADD     *ZEROS        G08DAT
104100970528     C                   MOVEL     '3'           G08ERR
104200970528     C                   CALL      'XSRDA8'
104300970528     C                   PARM                    WLBDA8
104400970528     C                   Z-ADD     G08DAT        VIDPDD
104500970528E   1C                   ENDIF
104600970526     C                   EXSR      CHKPAR
104700970526IF  1C     ERRL00        IFEQ      '1'
104800970526     C                   MOVEL     '1'           D00ERR
104900970526     C                   MOVEL     DSMSMS        D00MSG
105000970526X   1C                   ELSE
105100970528     C                   MOVEL     VIDPPA        DEPEPA                         *DEPOSITI
105200970528     C                   Z-ADD     VIDPTP        DEPTFP
105300970528     C                   Z-ADD     VIDPTA        DEPTFA
105400980107     C                   Z-ADD     *ZEROS        DEPTFE
105500970528     C                   Z-ADD     VIDPDD        DEPDDE
105600970526E   1C                   ENDIF
105700970526     C*
105800970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
105900970526     C                   EXSR      CHKFOP
106000970526     C*
106100970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
106200970530IF  1C     DEPOP0        IFEQ      'P01'
106300970530     C                   MOVEL     DEC(3)        VIDING
106400970530E   1C                   ENDIF
106500970530IF  1C     DEPOP0        IFEQ      'P02'
106600970526     C                   MOVEL     DEC(1)        VIDING
106700970530E   1C                   ENDIF
106800970530IF  1C     DEPOP0        IFEQ      'P05'
106900970526     C                   MOVEL     DEC(2)        VIDING
107000970526E   1C                   ENDIF
107100970526     C*
107200970526     C                   ENDSR
107300970526     C*------------------------------------------------------------------------*
107400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
107500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
107600970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
107700970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
107800970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
107900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108100970526     C*------------------------------------------------------------------------*
108200970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
108300970526     C*------------------------------------------------------------------------*
108400970526     C     POSSFL        BEGSR
108500970526     C*
108600970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
108700970526     C                   EXSR      CLRSFL
108800970526     C*
108900970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
109000970526     C                   EXSR      SETFIL
109100970526     C*
109200970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
109300970526     C     SFLEOF        IFEQ      0
109400970526     C                   EXSR      CARSFL
109500970526     C                   END
109600970526     C*
109700970526     C                   ENDSR
109800970526     C*------------------------------------------------------------------------*
109900970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
110000970526     C*------------------------------------------------------------------------*
110100970526     C     ENTATT        BEGSR
110200970526     C*
110300970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
110400970526     C                   Z-ADD     0             OPZOK             1 0
110500970526     C*
110600970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
110700970526     C                   Z-ADD     0             SFLVAR            5 0
110800970526     C*
110900970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
111000970526     C                   Z-ADD     0             SFLERR
111100970526     C*
111200970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
111300970526     C                   Z-ADD     SFLULT        WFLULT
111400970526     C*
111500970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
111600970528     C                   READC     SI17D05                                98
111700970526     C     *IN98         DOWEQ     '0'
111800970526     C*
111900970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
112000970526     C                   ADD       1             SFLVAR
112100970526     C*
112200970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
112300970526     C     OPZOK         IFEQ      0
112400970526     C*
112500970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
112600970526     C     OPZKEY        IFNE      *BLANKS
112700970526     C                   EXSR      OPZCON
112800970526     C                   END
112900970526     C*
113000970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
113100970526     C*            2. PULIZIA SCELTA IN SFL
113200970526     C     OPZOK         IFEQ      0
113300970526     C                   MOVEL     '0'           *IN41
113400970526     C                   MOVEL     *BLANKS       OPZKEY
113500970526     C                   ELSE
113600970526     C*
113700970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
113800970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
113900970526     C                   MOVEL     '1'           *IN41
114000970526     C                   Z-ADD     SFLULT        SFLERR
114100970526     C                   END
114200970526     C                   ELSE
114300970526     C*
114400970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
114500970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
114600970526     C                   MOVEL     '1'           *IN41
114700970526     C                   END                                                    OPZOK=0
114800970526     C*
114900970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
115000970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
115100970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
115200970526     C     OPZOK         IFNE      0
115300970526     C                   Z-ADD     SFLERR        SFLNBR
115400970526     C                   ELSE
115500970526     C                   Z-ADD     SFLULT        SFLNBR
115600970526     C                   END
115700970526     C*
115800970526     C* AGGIORNO RECORD SFL
115900970528     C                   UPDATE    SI17D05
116000970526     C*
116100970528     C                   READC     SI17D05                                98
116200970526     C                   END                                                    DO WHILE
116300970526     C*
116400970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
116500970526     C                   Z-ADD     WFLULT        SFLULT
116600970526     C*
116700970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
116800970526     C     SFLVAR        IFEQ      0
116900970526     C*---                 MOVEL'1'       FINL00
117000970526     C                   END
117100970526     C*
117200970526     C                   ENDSR
117300970526     C*------------------------------------------------------------------------*
117400970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
117500970526     C*------------------------------------------------------------------------*
117600970526     C     CLRSFL        BEGSR
117700970526     C*
117800970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
117900970526     C* (0=NON FINE FILE  1=FINE FILE)
118000970526     C                   Z-ADD     0             SFLEOF
118100970526     C*
118200970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
118300970526     C                   Z-ADD     0             SFLULT
118400970526     C*
118500970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
118600970526     C                   MOVEL     '1'           *IN40
118700970528     C                   WRITE     SI17D06
118800970526     C                   MOVEL     '0'           *IN40
118900970526     C*
119000970526     C                   ENDSR
119100970526     C*------------------------------------------------------------------------*
119200970526     C* CARSFL - CARICAMENTO SFL
119300970526     C*------------------------------------------------------------------------*
119400970526     C     CARSFL        BEGSR
119500970526     C*
119600970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
119700970526     C     SFLULT        IFEQ      0
119800970526     C                   EXSR      SETFIL
119900970526     C                   END
120000970526     C*
120100970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
120200970526     C                   Z-ADD     SFLULT        WFLULT
120300970526     C*
120400970526     C* CICLO:
120500970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
120600970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
120700970526     C                   Z-ADD     1             SFLRIG
120800970526     C     SFLRIG        DOWLE     SFLPAG
120900970526     C     SFLEOF        ANDEQ     0
121000970526     C*
121100970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
121200970526     C                   MOVEL     *BLANKS       OPZKEY
121300970526     C*
121400970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
121500970526     C                   EXSR      IRESFL
121600970526     C*
121700970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
121800970526     C                   ADD       1             SFLULT
121900970526     C*
122000970526     C* SCRITTURA RECORD SFL
122100970528     C                   WRITE     SI17D05
122200970526     C                   ADD       1             SFLRIG
122300970526     C*
122400970526     C* LETTURA FILE DA CARICARE
122500970526     C                   EXSR      LETFIL
122600060706     c* se cambiato tipo, lascio una riga vuota se non ho finito la
122700060706     c* pagina
122800060706     c                   if        caeepa<>videpa     and sfleof=0 and
122900060706     C                             sflrig<=sflpag
123000060706     c                   seton                                        01
123100060706     C                   MOVEL     *BLANKS       OPZKEY
123200060706     c                   clear                   vidatb
123300060706     c                   clear                   videpa
123400060706     c                   clear                   vidtfp
123500060706     c                   clear                   viddtp
123600060706     c                   clear                   vidtfa
123700060706     c                   clear                   viddta
123800060706     c                   clear                   vidtfe
123900060706     c                   clear                   viddte
124000060706     c                   clear                   viddde
124100060706     c                   clear                   viddsc
124200060706     C* SCRITTURA RECORD SFL
124300060706     C                   ADD       1             SFLULT
124400060706     C                   WRITE     SI17D05
124500060706     C                   ADD       1             SFLRIG
124600060706     c                   setoff                                       01
124700060706     C                   ENDif                                                  DO WHILE
124800060706     C                   END                                                    DO WHILE
124900970526     C*
125000970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
125100970526     C     WFLULT        IFNE      SFLULT
125200970526     C                   Z-ADD     WFLULT        SFLNBR
125300970526     C                   ADD       1             SFLNBR
125400970526     C                   END
125500970526     C*
125600970526     C                   ENDSR
125700970526     C*------------------------------------------------------------------------*
125800970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
125900970526     C*------------------------------------------------------------------------*
126000970526     C     OUTSFL        BEGSR
126100970526     C*
126200970526     C* WRITE RECORD TESTATA VIDEO
126300970528     C                   WRITE     SI17D01
126400970526     C*
126500970526     C* VISUALIZZAZIONE MESSAGGIO
126600970526     C                   EXSR      SNDMSG
126700970526     C*
126800970526     C* WRITE RECORD OPZIONI VIDEO
126900970528     C                   WRITE     SI17D02
127000970526     C*
127100970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
127200970528     C                   WRITE     SI17D03
127300970526     C*
127400970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
127500970528     C                   WRITE     SI17D04
127600970526     C*
127700970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
127800970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
127900970526     C     SFLEOF        IFEQ      0
128000970526     C                   MOVEL     '0'           *IN42
128100970526     C                   ELSE
128200970526     C*
128300970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
128400970526     C                   MOVEL     '1'           *IN42
128500970526     C                   END
128600970526     C*
128700970526     C* WRITE RECORD TASTI FUNZIONALI
128800970528     C                   WRITE     SI17D09
128900970526     C*
129000970526     C* CASO DI SFL VUOTO
129100970526     C     SFLULT        IFLE      0
129200970526     C*
129300970526     C*    - WRITE RECORD VUOTO
129400970528     C                   WRITE     SI17D0A
129500970528     C                   READ      SI17D0A                                99
129600970526     C*
129700970526     C                   ELSE
129800970526     C*
129900970526     C* CASO DI SFL CON RECORDS
130000970526     C*
130100970526     C*    - WRITE RECORD CONTROLLO SFL
130200970528     C                   WRITE     SI17D06
130300970528     C                   READ      SI17D06                                99
130400970526     C                   END
130500970526     C*
130600970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
130700970528     C                   READ      SI17D03                                99
130800970526     C*
130900970526     C                   ENDSR
131000970526     C*------------------------------------------------------------------------*
131100970526     C* SNDMSG - MANDA MESSAGGIO
131200970526     C*------------------------------------------------------------------------*
131300970526     C     SNDMSG        BEGSR
131400970526     C*
131500970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
131600970526     C                   MOVEL     '1'           *IN40
131700970528     C                   WRITE     SI17DMC
131800970526     C                   MOVEL     '0'           *IN40
131900970526     C*
132000970526     C* VISUALIZZAZIONE MESSAGGIO
132100970526     C                   Z-ADD     0             MSGULT
132200970526     C                   CALL      'XMSG'        XMSGPA
132300970526     C     DSMSER        IFEQ      0
132400970526     C                   MOVEL     DSMSKE        MSGKEY
132500970526     C                   END
132600970526     C                   Z-ADD     1             MSGULT
132700970528     C                   WRITE     SI17DMS                                      SFL MESS
132800970528     C                   WRITE     SI17DMC                                      CONTR MESS
132900970526     C*
133000970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
133100970526     C                   MOVEL     *BLANKS       DSMSMS
133200970526     C*
133300970526     C                   ENDSR
133400970526     C*------------------------------------------------------------------------*
133500970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
133600970526     C*------------------------------------------------------------------------*
133700970526     C     FUNCON        BEGSR
133800970526     C*
133900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
134000970526     C                   EXSR      FUNVAR
134100970526     C*
134200970526     C* PULISCO CAMPI DI LAVORO
134300970526     C                   Z-ADD     0             I                 5 0
134400970526     C                   Z-ADD     0             W                 5 0
134500970526     C*
134600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
134700970526     C     FUNKEY        IFNE      *BLANKS
134800970526     C     1             DO        24            I
134900970526     C                   MOVEL     FUD(I)        FUDRIG
135000970526     C*
135100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
135200970526     C     FUNKEY        IFEQ      FUDFUN
135300970526     C     FUDATT        ANDEQ     'S'
135400970526     C                   Z-ADD     I             W
135500970526     C                   END
135600970526     C                   END                                                    DO WHILE
135700970526     C*
135800970526     C* FUNZIONE NON ATTIVA (ERRORE)
135900970526     C     W             IFEQ      0
136000970526     C*
136100970526     C* EMISSIONE MESSAGGIO DI ERRORE
136200970526     C                   MOVEL     MSG(2)        DSMSMS
136300970526     C                   END                                                    W=0
136400970526     C                   END                                                    FUNKEY NE *BLANKS
136500970526     C*
136600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
136700970526     C     W             IFNE      0
136800970526     C     FUNKEY        OREQ      *BLANKS
136900970526     C*
137000970526     C     FUNKEY        IFEQ      *BLANKS
137100970526     C                   EXSR      ENTGES
137200970526     C                   END
137300970526     C     FUNKEY        IFEQ      '03'
137400970526     C                   EXSR      F03GES
137500970526     C                   END
137600970526     C     FUNKEY        IFEQ      '05'
137700970526     C                   EXSR      F05GES
137800970526     C                   END
137900970526     C     FUNKEY        IFEQ      '06'
138000970526     C                   EXSR      F06GES
138100970526     C                   END
138200970526     C     FUNKEY        IFEQ      '12'
138300970526     C                   EXSR      F12GES
138400970526     C                   END
138500970528     C     FUNKEY        IFEQ      '21'
138600970528     C                   EXSR      F21GES
138700970528     C                   END
138800970528     C     FUNKEY        IFEQ      '22'
138900970528     C                   EXSR      F22GES
139000970528     C                   END
139100970526     C     FUNKEY        IFEQ      '23'
139200970526     C                   EXSR      F23GES
139300970526     C                   END
139400970526     C     FUNKEY        IFEQ      '24'
139500970526     C                   EXSR      F24GES
139600970526     C                   END
139700970526     C     FUNKEY        IFEQ      'RU'
139800970526     C                   EXSR      RUPGES
139900970526     C                   END
140000970526     C*
140100970526     C                   END
140200970526     C*
140300970526     C                   ENDSR
140400970526     C*------------------------------------------------------------------------*
140500970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
140600970526     C*------------------------------------------------------------------------*
140700970526     C     FUNVAR        BEGSR
140800970526     C*
140900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
141000970526     C     KEY           IFEQ      ENTER
141100970526     C                   MOVEL     *BLANKS       FUNKEY
141200970526     C                   END
141300970526     C     KEY           IFEQ      F1
141400970526     C                   MOVEL     '01'          FUNKEY
141500970526     C                   END
141600970526     C     KEY           IFEQ      F2
141700970526     C                   MOVEL     '02'          FUNKEY
141800970526     C                   END
141900970526     C     KEY           IFEQ      F3
142000970526     C                   MOVEL     '03'          FUNKEY
142100970526     C                   END
142200970526     C     KEY           IFEQ      F4
142300970526     C                   MOVEL     '04'          FUNKEY
142400970526     C                   END
142500970526     C     KEY           IFEQ      F5
142600970526     C                   MOVEL     '05'          FUNKEY
142700970526     C                   END
142800970526     C     KEY           IFEQ      F6
142900970526     C                   MOVEL     '06'          FUNKEY
143000970526     C                   END
143100970526     C     KEY           IFEQ      F7
143200970526     C                   MOVEL     '07'          FUNKEY
143300970526     C                   END
143400970526     C     KEY           IFEQ      F8
143500970526     C                   MOVEL     '08'          FUNKEY
143600970526     C                   END
143700970526     C     KEY           IFEQ      F9
143800970526     C                   MOVEL     '09'          FUNKEY
143900970526     C                   END
144000970526     C     KEY           IFEQ      F10
144100970526     C                   MOVEL     '10'          FUNKEY
144200970526     C                   END
144300970526     C     KEY           IFEQ      F11
144400970526     C                   MOVEL     '11'          FUNKEY
144500970526     C                   END
144600970526     C     KEY           IFEQ      F12
144700970526     C                   MOVEL     '12'          FUNKEY
144800970526     C                   END
144900970526     C     KEY           IFEQ      F13
145000970526     C                   MOVEL     '13'          FUNKEY
145100970526     C                   END
145200970526     C     KEY           IFEQ      F14
145300970526     C                   MOVEL     '14'          FUNKEY
145400970526     C                   END
145500970526     C     KEY           IFEQ      F15
145600970526     C                   MOVEL     '15'          FUNKEY
145700970526     C                   END
145800970526     C     KEY           IFEQ      F16
145900970526     C                   MOVEL     '16'          FUNKEY
146000970526     C                   END
146100970526     C     KEY           IFEQ      F17
146200970526     C                   MOVEL     '17'          FUNKEY
146300970526     C                   END
146400970526     C     KEY           IFEQ      F18
146500970526     C                   MOVEL     '18'          FUNKEY
146600970526     C                   END
146700970526     C     KEY           IFEQ      F19
146800970526     C                   MOVEL     '19'          FUNKEY
146900970526     C                   END
147000970526     C     KEY           IFEQ      F20
147100970526     C                   MOVEL     '20'          FUNKEY
147200970526     C                   END
147300970526     C     KEY           IFEQ      F21
147400970526     C                   MOVEL     '21'          FUNKEY
147500970526     C                   END
147600970526     C     KEY           IFEQ      F22
147700970526     C                   MOVEL     '22'          FUNKEY
147800970526     C                   END
147900970526     C     KEY           IFEQ      F23
148000970526     C                   MOVEL     '23'          FUNKEY
148100970526     C                   END
148200970526     C     KEY           IFEQ      F24
148300970526     C                   MOVEL     '24'          FUNKEY
148400970526     C                   END
148500970526     C     KEY           IFEQ      ROLLUP
148600970526     C                   MOVEL     'RU'          FUNKEY
148700970526     C                   END
148800970526     C     KEY           IFEQ      HELP
148900970526     C                   MOVEL     'HP'          FUNKEY
149000970526     C                   END
149100970526     C*
149200970526     C                   ENDSR
149300970526     C*------------------------------------------------------------------------*
149400970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
149500970526     C*------------------------------------------------------------------------*
149600970526     C     FUNGES        BEGSR
149700970526     C*
149800970526     C* PULISCO CAMPI DI LAVORO
149900970526     C                   Z-ADD     0             W                 5 0
150000970526     C*
150100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
150200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
150300970526     C     1             DO        24            I
150400970526     C                   MOVEL     FUD(I)        FUDRIG
150500970526     C*
150600970526     C* PULIZIA FUNZIONI PRECEDENTI
150700970526     C                   MOVEL     *BLANKS       FUV(I)
150800970526     C*
150900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
151000970526     C* (SALTA F23 E F24)
151100970526     C     FUDATT        IFEQ      'S'
151200970526     C     FUDFUN        ANDNE     '23'
151300970526     C     FUDFUN        ANDNE     '24'
151400970526     C                   ADD       1             W
151500970526     C                   MOVEL     FUDDES        FUV(W)
151600970526     C                   END
151700970526     C                   END                                                    DO WHILE
151800970526     C*
151900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
152000970526     C* ALTRIMENTI DISATTIVO F24
152100970526     C     1             DO        24            I
152200970526     C                   MOVEL     FUD(I)        FUDRIG
152300970526     C*
152400970526     C     FUDFUN        IFEQ      '24'
152500970526     C     W             IFGT      6
152600970526     C                   MOVEL     'S'           FUDATT
152700970526     C                   ELSE
152800970526     C                   MOVEL     *BLANKS       FUDATT
152900970526     C                   END                                                    W GT 8
153000970526     C                   MOVEL     FUDRIG        FUD(I)
153100970526     C                   END
153200970526     C                   END                                                    DO WHILE
153300970526     C*
153400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
153500970526     C     W             DIV       6             I
153600970526     C                   MVR                     Z
153700970526     C*
153800970526     C     Z             IFNE      0
153900970526     C     I             ADD       1             Z
154000970526     C                   ELSE
154100970526     C                   Z-ADD     I             Z
154200970526     C                   END
154300970526     C*
154400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
154500970526     C                   ADD       1             FUNGRU
154600970526     C     FUNGRU        IFGT      Z
154700970526     C     FUNGRU        ORLT      1
154800970526     C                   Z-ADD     1             FUNGRU
154900970526     C                   END
155000970526     C*
155100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
155200970526     C     FUNGRU        IFEQ      1
155300970526     C                   MOVEL     FUV(1)        FUN001
155400970526     C                   MOVEL     FUV(2)        FUN002
155500970526     C                   MOVEL     FUV(3)        FUN003
155600970526     C                   MOVEL     FUV(4)        FUN004
155700970526     C                   MOVEL     FUV(5)        FUN005
155800970526     C                   MOVEL     FUV(6)        FUN006
155900970526     C                   END
156000970526     C*
156100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
156200970526     C     FUNGRU        IFEQ      2
156300970526     C                   MOVEL     FUV(7)        FUN001
156400970526     C                   MOVEL     FUV(8)        FUN002
156500970526     C                   MOVEL     FUV(9)        FUN003
156600970526     C                   MOVEL     FUV(10)       FUN004
156700970526     C                   MOVEL     FUV(11)       FUN005
156800970526     C                   MOVEL     FUV(12)       FUN006
156900970526     C                   END
157000970526     C*
157100970526     C* TERZO GRUPPO DI 6 FUNZIONI
157200970526     C     FUNGRU        IFEQ      3
157300970526     C                   MOVEL     FUV(13)       FUN001
157400970526     C                   MOVEL     FUV(14)       FUN002
157500970526     C                   MOVEL     FUV(15)       FUN003
157600970526     C                   MOVEL     FUV(16)       FUN004
157700970526     C                   MOVEL     FUV(17)       FUN005
157800970526     C                   MOVEL     FUV(18)       FUN006
157900970526     C                   END
158000970526     C*
158100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
158200970526     C     FUNGRU        IFEQ      4
158300970526     C                   MOVEL     FUV(19)       FUN001
158400970526     C                   MOVEL     FUV(20)       FUN002
158500970526     C                   MOVEL     FUV(21)       FUN003
158600970526     C                   MOVEL     FUV(22)       FUN004
158700970526     C                   MOVEL     *BLANKS       FUN005
158800970526     C                   MOVEL     *BLANKS       FUN006
158900970526     C                   END
159000970526     C*
159100970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
159200970526     C                   MOVEL     *BLANKS       FUN007
159300970526     C                   MOVEL     *BLANKS       FUN008
159400970526     C*
159500970526     C     1             DO        24            I
159600970526     C                   MOVEL     FUD(I)        FUDRIG
159700970526     C     FUDFUN        IFEQ      '23'
159800970526     C     FUDATT        ANDEQ     'S'
159900970526     C                   MOVEL     FUDDES        FUN007
160000970526     C                   END
160100970526     C     FUDFUN        IFEQ      '24'
160200970526     C     FUDATT        ANDEQ     'S'
160300970526     C                   MOVEL     FUDDES        FUN008
160400970526     C                   END
160500970526     C                   END                                                    DO
160600970526     C*
160700970526     C                   ENDSR
160800970526     C*------------------------------------------------------------------------*
160900970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
161000970526     C*------------------------------------------------------------------------*
161100970526     C     OPZGES        BEGSR
161200970526     C*
161300970526     C* PULISCO CAMPI DI LAVORO
161400970526     C                   Z-ADD     0             W                 5 0
161500970526     C*
161600970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
161700970526     C* E CARICO TABELLA OPZIONI ATTIVE
161800970526     C     1             DO        24            I
161900970526     C                   MOVEL     OPD(I)        OPDRIG
162000970526     C*
162100970526     C* PULIZIA OPZIONI PRECEDENTI
162200970526     C                   MOVEL     *BLANKS       OPV(I)
162300970526     C*
162400970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
162500970526     C     OPDATT        IFEQ      'S'
162600970526     C                   ADD       1             W
162700970526     C                   MOVEL     OPDDES        OPV(W)
162800970526     C                   END
162900970526     C                   END                                                    DO WHILE
163000970526     C*
163100970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
163200970526     C* ALTRIMENTI DISATTIVO F23
163300970526     C     1             DO        24            I
163400970526     C                   MOVEL     FUD(I)        FUDRIG
163500970526     C*
163600970526     C     FUDFUN        IFEQ      '23'
163700970526     C     W             IFGT      8
163800970526     C                   MOVEL     'S'           FUDATT
163900970526     C                   ELSE
164000970526     C                   MOVEL     *BLANKS       FUDATT
164100970526     C                   END                                                    W GT 8
164200970526     C                   MOVEL     FUDRIG        FUD(I)
164300970526     C                   END
164400970526     C                   END                                                    DO WHILE
164500970526     C*
164600970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
164700970526     C     W             DIV       8             I
164800970526     C                   MVR                     Z                 5 0
164900970526     C*
165000970526     C     Z             IFNE      0
165100970526     C     I             ADD       1             Z
165200970526     C                   ELSE
165300970526     C                   Z-ADD     I             Z
165400970526     C                   END
165500970526     C*
165600970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
165700970526     C                   ADD       1             OPZGRU
165800970526     C     OPZGRU        IFGT      Z
165900970526     C     OPZGRU        ORLT      1
166000970526     C                   Z-ADD     1             OPZGRU
166100970526     C                   END
166200970526     C*
166300970526     C* PRIMO GRUPPO DI 8 OPZIONI
166400970526     C     OPZGRU        IFEQ      1
166500970526     C                   MOVEL     OPV(1)        OPZ001
166600970526     C                   MOVEL     OPV(2)        OPZ002
166700970526     C                   MOVEL     OPV(3)        OPZ003
166800970526     C                   MOVEL     OPV(4)        OPZ004
166900970526     C                   MOVEL     OPV(5)        OPZ005
167000970526     C                   MOVEL     OPV(6)        OPZ006
167100970526     C                   MOVEL     OPV(7)        OPZ007
167200970526     C                   MOVEL     OPV(8)        OPZ008
167300970526     C                   END
167400970526     C*
167500970526     C* SECONDO GRUPPO DI 8 OPZIONI
167600970526     C     OPZGRU        IFEQ      2
167700970526     C                   MOVEL     OPV(9)        OPZ001
167800970526     C                   MOVEL     OPV(10)       OPZ002
167900970526     C                   MOVEL     OPV(11)       OPZ003
168000970526     C                   MOVEL     OPV(12)       OPZ004
168100970526     C                   MOVEL     OPV(13)       OPZ005
168200970526     C                   MOVEL     OPV(14)       OPZ006
168300970526     C                   MOVEL     OPV(15)       OPZ007
168400970526     C                   MOVEL     OPV(16)       OPZ008
168500970526     C                   END
168600970526     C*
168700970526     C* TERZO GRUPPO DI 8 OPZIONI
168800970526     C     OPZGRU        IFEQ      3
168900970526     C                   MOVEL     OPV(17)       OPZ001
169000970526     C                   MOVEL     OPV(18)       OPZ002
169100970526     C                   MOVEL     OPV(19)       OPZ003
169200970526     C                   MOVEL     OPV(20)       OPZ004
169300970526     C                   MOVEL     OPV(21)       OPZ005
169400970526     C                   MOVEL     OPV(22)       OPZ006
169500970526     C                   MOVEL     OPV(23)       OPZ007
169600970526     C                   MOVEL     OPV(24)       OPZ008
169700970526     C                   END
169800970526     C*
169900970526     C                   ENDSR
170000970526     C*------------------------------------------------------------------------*
170100970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
170200970526     C*------------------------------------------------------------------------*
170300970526     C     OPZCON        BEGSR
170400970526     C*
170500970526     C* PULISCO CAMPI DI LAVORO
170600970526     C                   Z-ADD     0             I                 5 0
170700970526     C                   Z-ADD     0             W                 5 0
170800970526     C*
170900970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
171000970526     C     1             DO        24            I
171100970526     C                   MOVEL     OPD(I)        OPDRIG
171200970526     C*
171300970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
171400970526     C     OPZKEY        IFEQ      OPDOPZ
171500970526     C     OPZKEY        OREQ      OPDOP1
171600970526     C*
171700970526     C     OPDATT        IFEQ      'S'
171800970526     C                   MOVEL     OPDOPZ        OPZKEY
171900970526     C                   Z-ADD     I             W
172000970526     C                   END
172100970526     C                   END
172200970526     C*
172300970526     C                   END                                                    DO WHILE
172400970526     C*
172500970526     C* OPZIONE NON ATTIVA (ERRORE)
172600970526     C     W             IFEQ      0
172700970526     C*
172800970526     C* OPZIONE NON OK
172900970526     C                   Z-ADD     1             OPZOK
173000970526     C*
173100970526     C* EMISSIONE MESSAGGIO DI ERRORE
173200970526     C                   MOVEL     MSG(1)        DSMSMS
173300970526     C                   ELSE
173400970526     C*
173500970526     C* OPZIONE ATTIVA (PROCEDI ...)
173600970526     C*
173700970526     C     OPZKEY        IFEQ      ' 1'
173800970526     C                   EXSR      O01GES
173900970526     C                   END
174000970526     C     OPZKEY        IFEQ      ' 2'
174100970526     C                   EXSR      O02GES
174200970526     C                   END
174300970526     C     OPZKEY        IFEQ      ' 4'
174400970526     C                   EXSR      O04GES
174500970526     C                   END
174600970526     C     OPZKEY        IFEQ      ' 5'
174700970526     C                   EXSR      O05GES
174800970526     C                   END
174900970526     C     OPZKEY        IFEQ      ' 7'
175000970526     C                   EXSR      O07GES
175100970526     C                   END
175200060804     C     OPZKEY        IFEQ      'SC'
175300060804     C                   EXSR      OSCGES
175400060804     C                   END
175500970526     C                   END                                                    OPZ.NON ATTIVA
175600970526     C*
175700970526     C                   ENDSR
175800970526     C*------------------------------------------------------------------------*
175900970526     C* NOTE COSTRUZIONE SCHIERE
176000970526     C*------------------------------------------------------------------------*
176100970526     C*
176200970526     C* -MSG-MESSAGGI
176300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
176400970526     C* (NON TOCCARE MSG,1 E MSG,2)
176500970526     C*
176600970526     C* -CMD-COMANDI
176700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
176800970526     C*
176900970526     C* -OPD-OPZIONI
177000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
177100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
177200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
177300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
177400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
177500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
177600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
177700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
177800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
177900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
178000970526     C*
178100970526     C* -FUD-FUNZIONI
178200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
178300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
178400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
178500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
178600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
178700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
178800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
178900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
179000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
179100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
179200970526     O*------------------------------------------------------------------------*
179300970526** DEC - DECODIFICHE
179400970526    GESTIONE                                                     1
179500970526INTERROGAZIONE                                                   2
179600970530     SCELTA                                                      3
179700060929ScadenzaTerminal
1798009705265
179900970526** MSG - MESSAGGI
180000970528TISI17R- Opzione non gestita                                                   1
180100970528TISI17R- Tasto funzionale non gestito                                          2
180200970528TISI17R- Non abilitato alla gestione di questo CAP                             3
180300970528TISI17R- Record annullato. Consentito solo il ripristino                       4
180400970528TISI17R- Record non annullato. Impossibile ripristinare                        5
180500970528TISI17R- Accettato solo un parametro di inizio lista                           6
180600060929X l'opzione "SC-Scadenza terminal" i record scelti non hanno lo stesso TIPO    7
180700060929X l'opzione"SC-Scadenza terminal"selezionare TipoTerminal e Data Decorrenza   8
180800060929TISI17R- La nuova data decorrenza deve essere maggiore della prima            9
180900060929TISI17R- Terminal / linea errati                                              10
181000970530TISI17R- 11
181100970528TISI17R- Data errata                                                          12
181200970528TISI17R- 13                                                                   13
181300970528TISI17R- 14                                                                   14
181400970528TISI17R- 15                                                                   15
181500970528TISI17R- 16                                                                   16
181600970528TISI17R- 17                                                                   17
181700970528TISI17R- 18                                                                   18
181800970528TISI17R- 19                                                                   19
181900970528TISI17R- 20                                                                   20
182000970528** CMD - COMANDI CL
182100970528WRKSPLF
182200970528
182300970528
182400970528
182500970528
182600970526** OPD - OPZIONI
182700970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
182800970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
182900970526                   -3 -     (S/N)=N                        opz. 03¬
183000970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
183100970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
183200970526                   -6 -     (S/N)=N                        opz. 06¬
183300970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
183400970527                   -8 -     (S/N)=N                        opz. 08¬
183500970527                   -9 -     (S/N)=N                        opz. 09¬
183600060929SC=Scadenza termin -10-     (S/N)=N                        opz. 10¬
183700970526                   -  -     (S/N)=N                        opz. 11¬
183800970526                   -  -     (S/N)=N                        opz. 12¬
183900970526                   -  -     (S/N)=N                        opz. 13¬
184000970526                   -  -     (S/N)=N                        opz. 14¬
184100970526                   -  -     (S/N)=N                        opz. 15¬
184200970526                   -  -     (S/N)=N                        opz. 16¬
184300970526                   -  -     (S/N)=N                        opz. 17¬
184400970526                   -  -     (S/N)=N                        opz. 18¬
184500970526                   -  -     (S/N)=N                        opz. 19¬
184600970526                   -  -     (S/N)=N                        opz. 20¬
184700970526                   -  -     (S/N)=N                        opz. 21¬
184800970526                   -  -     (S/N)=N                        opz. 22¬
184900970526                   -  -     (S/N)=N                        opz. 23¬
185000970526                   -  -     (S/N)=N                        opz. 24¬
185100970526** FUD - FUNZIONI
185200970526                   -        (S/N)=N                        funz.01¬
185300970526                   -        (S/N)=N                        funz.02¬
185400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
185500970526                   -        (S/N)=N                        funz.04¬
185600970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
185700020506F06=Inserimento    -        (S/N)=S                        funz.06¬
185800970526                   -        (S/N)=N                        funz.07¬
185900970527                   -        (S/N)=N                        funz.08¬
186000970527                   -        (S/N)=N                        funz.09¬
186100970527                   -        (S/N)=N                        funz.10¬
186200970526                   -        (S/N)=N                        funz.11¬
186300970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
186400970526                   -        (S/N)=N                        funz.13¬
186500970526                   -        (S/N)=N                        funz.14¬
186600970526                   -        (S/N)=N                        funz.15¬
186700970526                   -        (S/N)=N                        funz.16¬
186800970526                   -        (S/N)=N                        funz.17¬
186900970526                   -        (S/N)=N                        funz.18¬
187000970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
187100970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
187200980512F21=Stampa elenco  -        (S/N)=S                        funz.21¬
187300970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
187400970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
187500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
