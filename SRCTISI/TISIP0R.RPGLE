000100970526     H*------------------------------------------------------------------------*
000200060314     H* Calendario eventi particolari - SCELTA
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800060313     FTISIP0D   CF   E             WORKSTN INFDS(DEVDS1)
000900060313     F                                     SFILE(SIP0D05:SFLULT)
001000060313     F                                     SFILE(SIP0DMS:MSGULT)
001100060329     FAZcep01L  IF   E           K DISK
001200060330     FAZcep02L  IF   E           K DISK    rename(azcep000:azcep002)
001300060315     FAZorg01l  IF   E           K DISK
001400060329     Ftabel00f  IF   E           K DISK
001500970526     D*------------------------------------------------------------------------*
001600970526     D* SCHIERE
001700970526     D*------------------------------------------------------------------------*
001800970526     D* DECODIFICHE
001900970526     D*------------
002000970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* MESSAGGI
002300970526     D*------------
002400970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002500970526     D*------------
002600970526     D* DESCRIZIONI OPZIONI
002700970526     D*------------
002800970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
002900970526     D*------------
003000970526     D* DESCRIZIONI TASTI FUNZIONALI
003100970526     D*------------
003200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003300970526     D*------------
003400970526     D* OPZIONI DA CARICARE A VIDEO
003500970526     D*------------
003600970526     D OPV             S             19    DIM(24)
003700970526     D*------------
003800970526     D* FUNZIONI DA CARICARE A VIDEO
003900970526     D*------------
004000970527     D FUV             S             19    DIM(24)
004100970526     D*------------------------------------------------------------------------*
004200970526     D* INPUT
004300970526     D*------------------------------------------------------------------------*
004400970526     D* RIDEFINIZIONE OPZIONI VIDEO
004500970526     D OPDDS           DS
004600970526     D  OPDOPZ                 1      2
004700970526     D  OPDDE1                 3     19
004800970526     D  OPDOP1                21     22
004900970526     D  OPDFI0                23     34
005000970526     D  OPDATT                35     35
005100970526     D  OPDFI1                36     80
005200970526     D  OPDDES                 1     19
005300970526     D  OPDRIG                 1     80
005400970526     D*------------
005500970526     D* RIDEFINIZIONE FUNZIONI VIDEO
005600970526     D FUDDS           DS
005700970526     D  FUDDE1                 1      1
005800970526     D  FUDFUN                 2      3
005900970526     D  FUDDE2                 4     19
006000970526     D  FUDFI0                20     34
006100970526     D  FUDATT                35     35
006200970526     D  FUDFI1                36     80
006300970526     D  FUDDES                 1     19
006400970526     D  FUDRIG                 1     80
006500970526     D*------------
006600970526     D* DS GESTIONE TASTI FUNZIONALI
006700970526     D DSKEY         E DS
006800970526     D DEVDS1          DS
006900970526     D  KEY                  369    369
007000970526     D*------------
007100970526     D* DS "XMSG" - GESTIONE MESSAGGI
007200970526     D*------------
007300970526     D DSMSG         E DS
007400970526     D*------------
007500020919     D TIBS34DS      E DS                                                       *Profili utente
007600020919     D DDATIUTE      E DS                                                       *Dati utente
007700020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007800970527     D*------------------
007900970527     D WLBDA8          DS                  INZ
008000970527     D  G08DAT                 1      8  0
008100970527     D  G08INV                 9     16  0
008200970527     D  G08ERR                17     17
008300970527     D  G08TGI                18     22  0
008400970526     D*------------
008500970526     D* DS DI PROCEDURA
008600970526     D*------------
008700060313     D tisip0ds      E DS
008800060314     D tisi00ds      E DS
008900970526     D*------------
009000970526     D KPJBA         E DS
009100060404     D ds2z          E DS
009200060313     d
009300060313     d Dataiso         s               d   datfmt(*iso)
009400060313     d Dataeur         s               d   datfmt(*eur)
009500060329     d Datadmy         s               d   datfmt(*dmy)
009600060313     d kfil            s                   Like(cepfil)
009700060313     d kdev            s                   Like(cepdev)
009800060313     d knar            s                   Like(cepnar)
009900060313     d kcap            s                   Like(cepcap)
010000060314     d ddev            s                   Like(cepdev)
010100060315     d dcev            s                   Like(cepcev)
010200060315     d dfil            s                   Like(cepfil)
010300060327     d dprv            s                   Like(cepprv)
010400060331     d dTAF            s                   Like(ceptaf)
010500060404     d ddup            s                   Like(cepdup)
010600060329     d kcod            s                   Like(tblCod)
010700060329     d kkey            s                   Like(TblKey)
010800060330     d depord          s                   Like(vidord)
010900060404     d opzione         s              2
011000970526     C*------------------------------------------------------------------------*
011100970526     C* MAIN LINES
011200970526     C*------------------------------------------------------------------------*
011300970526     C*
011400970526     C* SE NON CI SONO ERRORI, PROSEGUE
011500970526IF  1C     ERRL00        IFNE      '1'
011600970526     C*
011700970526     C* CARICA IL SFILE
011800970526     C                   EXSR      POSSFL
011900970526     C*
012000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012400970526     C*
012500970526     C* REIMPOSTA ERRORE DI PROCERURA
012600970526     C                   MOVEL     '0'           ERRL00
012700970526     C*
012800970526     C* EMETTE IL SFILE
012900970526     C                   EXSR      OUTSFL
013000970526     C*
013100970526     C* GESTIONE TASTI FUNZIONALI
013200970526     C                   EXSR      FUNCON
013300970526     C*
013400970526E   2C                   ENDDO
013500970527     C*
013600970527     C* CI SONO ERRORI IN ENTRATA
013700970527X   1C                   ELSE
013800060313     C                   MOVEL     '1'           dp0ERR
013900060313     C                   MOVEL     DSMSMS        dp0MSG
014000970526E   1C                   ENDIF
014100970526     C* FINE PGM
014200970526     C                   SETON                                        LR
014300970526     C*------------------------------------------------------------------------*
014400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
014500970526     C*------------------------------------------------------------------------*
014600970526     C     F03GES        BEGSR
014700970526     C*
014800970526     C* IMPOSTA "PREMUTO F03"
014900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015000060313     C                   MOVEL     '1'           dp0F03                         *DI PROCEDURA
015100970526     C*
015200970526     C                   ENDSR
015300970526     C*------------------------------------------------------------------------*
015400970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
015500970526     C*------------------------------------------------------------------------*
015600970526     C     F05GES        BEGSR
015700970526     C*
015800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
015900970526     C*
016000970526     C                   ENDSR
016100970526     C*------------------------------------------------------------------------*
016200970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
016300970526     C*------------------------------------------------------------------------*
016400970526     C     F06GES        BEGSR
016500970526     C*
016600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
016700970603     C                   MOVEL     *BLANKS       DNAR
016800970602     C                   MOVEL     *BLANKS       DCAP
016900060328     c                   clear                   dfil
017000060328     c                   clear                   ddev
017100160427     c                   movel     datcor        ddev
017200060328     c                   clear                   dcev
017300060331     c                   clear                   dtaf
017400970602     C                   MOVEL     *BLANKS       DLOC
017500970529     C                   MOVEL     DEPOP0        WOP0
017600970529     C                   MOVEL     'F06'         WOP1
017700060315     C                   MOVEL     'TISIP1R'     WPGM              8
017800060314     C                   EXSR      LANPGMp                                      *LANCIA PGM
017900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018000970526E   1C                   ENDIF
018100970526     C*
018200970526     C                   ENDSR
018300970526     C*------------------------------------------------------------------------*
018400970526     C* F12GES - GESTIONE F12 -> RITORNO
018500970526     C*------------------------------------------------------------------------*
018600970526     C     F12GES        BEGSR
018700970526     C*
018800970526     C* IMPOSTA "PREMUTO F12"
018900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019000060313     C                   MOVEL     '1'           dp0F12                         *DI PROCEDURA
019100970526     C*
019200970526     C                   ENDSR
019300970529     C*------------------------------------------------------------------------*
019400970529     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
019500970529     C*------------------------------------------------------------------------*
019600970529     C     F13GES        BEGSR
019700970529     C*
019800970602     C                   MOVEL     *BLANKS       DNAR
019900970602     C                   MOVEL     *BLANKS       DCAP
020000970602     C                   MOVEL     *BLANKS       DLOC
020100060314     c                   movel     datcor        ddev
020200060331     C                   MOVEL     'S05'         WOP0
020300970529     C                   MOVEL     'F13'         WOP1
020400970529     C                   MOVEL     'TISI35R'     WPGM              8
020500060314     C                   EXSR      LANPGM0
020600970529     C*
020700970529     C                   ENDSR
020800970526     C*------------------------------------------------------------------------*
020900970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
021000970526     C*------------------------------------------------------------------------*
021100970526     C     F23GES        BEGSR
021200970526     C*
021300970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
021400970526     C*
021500970526     C                   ENDSR
021600970526     C*------------------------------------------------------------------------*
021700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021800970526     C*------------------------------------------------------------------------*
021900970526     C     F24GES        BEGSR
022000970526     C*
022100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022200970526     C*
022300970526     C                   ENDSR
022400970526     C*------------------------------------------------------------------------*
022500970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
022600970526     C*------------------------------------------------------------------------*
022700970526     C     RUPGES        BEGSR
022800970526     C*
022900970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
023000970526     C*
023100970526     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300970526     C* ENTGES - GESTIONE ENTER -> INVIO
023400970526     C*------------------------------------------------------------------------*
023500970526     C     ENTGES        BEGSR
023600970526     C*
023700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
023800970526     C                   EXSR      NEWPOS
023900970526     C*
024000970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
024100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024200970526IF  2C     SWPOSI        IFNE      0
024300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
024400970526X   2C                   ELSE
024500970526IF  3C     SFLULT        IFNE      0
024600970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
024700970526     C*
024800970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
024900970526IF  4C     SWMODI        IFNE      0
025000970526     C                   EXSR      POSSFL
025100970526E   4C                   ENDIF
025200970526E   3C                   ENDIF
025300970526E   2C                   ENDIF
025400970526E   1C                   ENDIF
025500970526     C*
025600970526     C                   ENDSR
025700970526     C*------------------------------------------------------------------------*
025800970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
025900970526     C*------------------------------------------------------------------------*
026000970526     C     O01GES        BEGSR
026100970526     C*
026200970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
026300060331     C                   MOVEL     VIDTAF        dp0TAF
026400060331     C                   MOVEL     VIDNAR        dp0NAR
026500060313     C                   MOVEL     VIDCAP        dp0CAP
026600060313     C                   MOVEL     VIDLOC        dp0LOC
026700060313     C                   MOVEL     VIDprv        dp0PRV
026800060313     C                   MOVEL     VIDfil        dp0fil
026900060327     C                   MOVEL     VIDcev        Dcev
027000060327     C                   MOVEL     amgdev        Ddev
027100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
027200970526     C*
027300970526     C                   ENDSR
027400970526     C*------------------------------------------------------------------------*
027500970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
027600970526     C*------------------------------------------------------------------------*
027700970526     C     O02GES        BEGSR
027800970526     C*
027900970527     C* MODIFICA
028000970704IF  1C     ERRL00        IFEQ      '0'
028100060327     C                   MOVEL     VIDprv        Dprv
028200060327     C                   MOVEL     VIDNAR        DNAR
028300060331     C                   MOVEL     VIDTAF        DTAF
028400970602     C                   MOVEL     VIDCAP        DCAP
028500970602     C                   MOVEL     VIDLOC        DLOC
028600060315     C                   MOVEL     VIDfil        Dfil
028700060315     C                   MOVEL     VIDcev        Dcev
028800060404     C                   MOVEL     VIDdup        Ddup
028900060315     C                   MOVEL     amgdev        Ddev
029000970529     C                   MOVEL     DEPOP0        WOP0
029100970526     C                   MOVEL     'O02'         WOP1
029200060315     C                   MOVEL     'TISIP1R'     WPGM
029300060315     C                   EXSR      LANPGMP
029400060328     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
029500970528E   1C                   ENDIF
029600970526     C*
029700970526     C                   ENDSR
029800970611     C*------------------------------------------------------------------------*
029900970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
030000970611     C*------------------------------------------------------------------------*
030100970611     C     O03GES        BEGSR
030200970611     C*
030300970611     C* DUPLICA
030400970611IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030500060329     C                   MOVEL     VIDprv        Dprv
030600060329     C                   MOVEL     VIDNAR        DNAR
030700060331     C                   MOVEL     VIDTAF        DTAF
030800060329     C                   MOVEL     VIDCAP        DCAP
030900060329     C                   MOVEL     VIDLOC        DLOC
031000060329     C                   MOVEL     VIDfil        Dfil
031100060329     C                   MOVEL     VIDcev        Dcev
031200060404     C                   clear                   Ddup
031300060329     C                   MOVEL     amgdev        Ddev
031400060329     C                   MOVEL     DEPOP0        WOP0
031500060329     C                   MOVEL     'O03'         WOP1
031600060329     C                   MOVEL     'TISIP1R'     WPGM
031700060329     C                   EXSR      LANPGMP
031800970611     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
031900970611E   1C                   ENDIF
032000970611     C*
032100970611     C                   ENDSR
032200970526     C*------------------------------------------------------------------------*
032300970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
032400970526     C*------------------------------------------------------------------------*
032500970526     C     O04GES        BEGSR
032600970526     C*
032700060327IF  1C     ERRL00        IFEQ      '0'
032800060327     C                   MOVEL     VIDprv        Dprv
032900060327     C                   MOVEL     VIDNAR        DNAR
033000060331     C                   MOVEL     VIDTAF        DTAF
033100060327     C                   MOVEL     VIDCAP        DCAP
033200060327     C                   MOVEL     VIDLOC        DLOC
033300060327     C                   MOVEL     VIDfil        Dfil
033400060327     C                   MOVEL     VIDcev        Dcev
033500060404     C                   MOVEL     VIDdup        Ddup
033600060327     C                   MOVEL     amgdev        Ddev
033700060327     C                   MOVEL     DEPOP0        WOP0
033800060327     C                   MOVEL     'O04'         WOP1
033900060327     C                   MOVEL     'TISIP1R'     WPGM
034000060327     C                   EXSR      LANPGMP
034100060327E   1C                   ENDIF
034200970526     C*
034300970526     C                   ENDSR
034400970526     C*------------------------------------------------------------------------*
034500970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
034600970526     C*------------------------------------------------------------------------*
034700970526     C     O05GES        BEGSR
034800970526     C*
034900060327     C                   MOVEL     VIDprv        Dprv
035000060327     C                   MOVEL     VIDNAR        DNAR
035100060331     C                   MOVEL     VIDTAF        DTAF
035200060327     C                   MOVEL     VIDCAP        DCAP
035300060327     C                   MOVEL     VIDLOC        DLOC
035400060327     C                   MOVEL     VIDfil        Dfil
035500060327     C                   MOVEL     VIDcev        Dcev
035600060404     C                   MOVEL     VIDdup        Ddup
035700060327     C                   MOVEL     amgdev        Ddev
035800060327     C                   MOVEL     DEPOP0        WOP0
035900060327     C                   MOVEL     'O05'         WOP1
036000060327     C                   MOVEL     'TISIP1R'     WPGM
036100060327     C                   EXSR      LANPGMP
036200970526     C*
036300970526     C                   ENDSR
036400970529     C*------------------------------------------------------------------------*
036500970529     C* O13GES - GESTIONE OPZIONE 13=SINONIMI
036600970529     C*------------------------------------------------------------------------*
036700970529     C     O13GES        BEGSR
036800970529     C*
036900060329     c* non richiedibile se c'e' il p.o.
037000060329     c                   if        vidfil>*zeros
037100060329     C                   MOVEL     MSG(4)        DSMSMS
037200060329     C                   MOVEL     '1'           ERRL00
037300060329     c                   else
037400970602     C                   MOVEL     VIDNAR        DNAR
037500970602     C                   MOVEL     VIDCAP        DCAP
037600970602     C                   MOVEL     VIDLOC        DLOC
037700060314     C                   MOVEL     amgdev        Ddev
037800970529     C                   MOVEL     DEPOP0        WOP0
037900970529     C                   MOVEL     'S'           WOP0
038000970529     C                   MOVEL     'O13'         WOP1
038100970529     C                   MOVEL     'TISI35R'     WPGM
038200060314     C                   EXSR      LANPGM0
038300060329     c                   endif
038400970529     C*
038500970529     C                   ENDSR
038600970526     C*------------------------------------------------------------------------*
038700970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
038800970526     C*------------------------------------------------------------------------*
038900970526     C     NEWPOS        BEGSR
039000970526     C*
039100970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
039200970526     C*         1= RICHIESTO POSIZIONAMENTO
039300970526     C                   Z-ADD     0             SWPOSI            7 0
039400970526     C*
039500970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
039600970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
039700970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
039800970526     C                   Z-ADD     0             SWMODI            1 0
039900970530     C*
040000970530     C* REIMPOSTA INDICATORI DI ERRORE
040100060313     C                   SETOFF                                       808281
040200970526     C*
040300060403     c* Se immesso p.o. o cap. ordinamento deve essere per po/cap
040400060403     c                   if        vidpna<>*blanks or vidpca<>*blanks or
040500060403     c                             vidppo>0
040600060403     c                   eval      vidord='C'
040700060403     c                   endif
040800060403     c
040900060403     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
041000970527IF  1C     VIDPNA        IFNE      DEPNAR
041100970527     C     VIDPCA        ORNE      DEPCAP
041200060313     C     VIDPpo        ORNE      DEPppo
041300060313     C     VIDPde        ORNE      DEPpde
041400060330     C     VIDord        ORNE      DEPord
041500970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
041600970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
041700970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
041800970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
041900970527     C                   MOVEL     VIDPCA        DEPCAP
042000060313     C                   MOVEL     VIDppo        DEPppo
042100060313     C                   MOVEL     VIDpde        DEPpde
042200060330     C                   MOVEL     VIDord        DEPord
042300970526E   2C                   ENDIF
042400970526E   1C                   ENDIF
042500970526     C*
042600970526     C                   ENDSR
042700970526     C*------------------------------------------------------------------------*
042800970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
042900970526     C*------------------------------------------------------------------------*
043000970526     C     SETFIL        BEGSR
043100060330     c                   clear                   knar
043200060330     c                   clear                   kcap
043300060330     c                   clear                   kfil
043400060330     c
043500060330     c                   if        vidord='C'
043600060330     c                   seton                                        02
043700060330     c                   setoff                                       01
043800060330     c                   else
043900060330     c                   seton                                        01
044000060330     c                   setoff                                       02
044100060330     c                   endif
044200970529     C*
044300970529     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
044400060313     c                   select
044500060313IF  1C     WTIPRI        wheneq    'CAP'                                        *PER CAP
044600060313     c                   if        kdev=0
044700060313     C                   MOVEL     datcor        Kdev
044800060313     c                   endif
044900060313     C                   MOVEL     VIDPNA        KNAR
045000060313     C                   MOVEL     VIDPCA        KCAP
045100060330     c* Ordinamento per CAP
045200060330     c                   if        vidord='C'
045300060330     C     kcep11        SETLL     AZCep01L                           99
045400060330     c                   else
045500060330     c* Ordinamento per Data
045600060330     C     kcep21        SETLL     AZCep02L                           99
045700060330     c                   endif
045800060313E   1C
045900060313IF  1C     WTIPRI        wheneq    'FIL'                                        *PER LOCALITA'
046000060313     c                   if        kdev=0
046100060313     C                   MOVEL     datcor        Kdev
046200060313     c                   endif
046300060313     c                   movel     vidppo        kfil
046400060330     c* Ordinamento per p.o.
046500060330     c                   if        vidord='C'
046600060330     C     Kcep12        SETLL     AZCep01L                           99
046700060330     c                   else
046800060330     c* Ordinamento per Data
046900060330     C     Kcep21        SETLL     AZCep02L                           99
047000060330     c                   endif
047100060330     c
047200060313E   1C                   ENDsl
047300970526     C* LETTURA RECORD
047400970526IF  1C     *IN99         IFEQ      *OFF
047500970526     C                   EXSR      LETFIL
047600970526E   1C                   ENDIF
047700970526     C*
047800970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
047900970526IF  1C     *IN99         IFEQ      *ON
048000970526     C                   Z-ADD     1             SFLEOF
048100970526E   1C                   ENDIF
048200970526     C*
048300970526     C                   ENDSR
048400970526     C*------------------------------------------------------------------------*
048500970526     C* LETFIL - LETTURA RECORD
048600970526     C*------------------------------------------------------------------------*
048700970526     C     LETFIL        BEGSR
048800970526     C*
048900970526     C* LEGGE FINO A:
049000970526     C*     - FINE FILE
049100970526     C*     - TROVATO RECORD VALIDO
049200970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
049300970526DO  1C     *IN99         DOWEQ     *OFF
049400970526     C     WRECOK        ANDEQ     'N'
049500970529     C*
049600970529     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
049700060330     c                   if        vidord='C'
049800060404     C                   READ      AZcep01L                               99
049900060330     c                   else
050000060404     C                   READ      AZcep02L                               99
050100060330     c                   endif
050200970526     C*
050300970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
050400970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
050500970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
050600970530X   2C                   ELSE
050700970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
050800970530E   2C                   ENDIF
050900970526E   1C                   ENDDO
051000970526     C*
051100970526     C                   ENDSR
051200970526     C*------------------------------------------------------------------------*
051300970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
051400970526     C*------------------------------------------------------------------------*
051500970526     C     CHKREC        BEGSR
051600970526     C*
051700060329     c                   if        cepdev>=kdev
051800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
051900060329     c                   endif
052000060329     c
052100970526     C                   ENDSR
052200970526     C*------------------------------------------------------------------------*
052300970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
052400970526     C*------------------------------------------------------------------------*
052500970526     C     IRESFL        BEGSR
052600970529     C*
052700970529     C                   MOVEL     *BLANKS       OPZKEY
052800060329     c                   clear                   viduni
052900060315     c* Se presente p.o. decodifico
053000060314     c                   if        cepfil>0
053100060314     C                   MOVEL     Cepfil        vIDfil
053200060315     c     cepfil        chain     azorg01l
053300060329     c                   if        not %found(azorg01l)
053400060329     c                   eval      orgdes=*all'?'
053500060315     c                   endif
053600060329     c                   eval      viduni=vidfil +'-'+ orgdes
053700060315     c
053800060315     c                   else
053900060314     c                   clear                   vidfil
054000060404     c                   eval      viduni=cepnar+' '+cepcap+' '+
054100060404     c                             %subst(ceploc:1:33)+' '+cepprv
054200060314     c                   endif
054300060331     C                   MOVEL     CepTAF        VIDTAF
054400060331     C                   MOVEL     CepNAR        VIDNAR
054500060313     C                   MOVEL     CepCAP        VIDCAP
054600060313     C                   MOVEL     CepLOC        VIDLOC
054700060313     C                   MOVEL     Cepprv        VIDprv
054800060313     c
054900060314     C                   movel     Cepcev        VIDcev
055000060404     C                   movel     Cepdup        VIDdup
055100060404     c
055200060404     c* Se diverso dallo STD evidenzio
055300060404     c                   setoff                                       03
055400060404     c                   eval      kcod='2Z'
055500060404     c                   movel(p)  vidcev        kkey
055600060404     c     ktab          chain     tabel00f
055700060404     c                   if        %found(tabel00f)
055800060404     c                   movel     tbluni        ds2z
055900060404     c                   if        §2zdep<>cepdup
056000060404     c                   seton                                        03
056100060404     c                   endif
056200060404     c                   endif
056300060404     c                   movel     *in03         in03
056400060404     c
056500060329     c* Decodifico evento
056600060329     c                   eval      kcod='2A'
056700060329     c                   movel(p)  vidcev        kkey
056800060329     c     ktab          chain     tabel00f
056900060329     c                   if        %found(tabel00f)
057000060329     c                   movel     tbluni        deccev
057100060329     c                   else
057200060329     c                   movel     *all'?'       deccev
057300060329     c                   endif
057400060314     C                   movel     Cepdev        amgdev
057500060313     c     *iso          movel     cepdev        dataiso
057600060329     c                   movel     dataiso       datadmy
057700060329     c                   movel     datadmy       viddev
057800060313     c
057900970526     C                   ENDSR
058000970526     C*------------------------------------------------------------------------*
058100970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
058200970526     C*------------------------------------------------------------------------*
058300970526     C     CHKPAR        BEGSR
058400060313     c* controllo data di riferimento se errata
058500060313     c                   clear                   kdev
058600060313     c                   if        vidpde>0
058700060313     C                   Z-ADD     vidpde        G08DAT
058800060313     C                   Z-ADD     *ZEROS        G08INV
058900060313     C                   MOVEL     '0'           G08ERR
059000060313     C                   CALL      'XSRDA8'
059100060313     C                   PARM                    WLBDA8
059200060313     c                   if        g08err='1'
059300060313     C                   MOVEL     MSG(3)        DSMSMS
059400060313     C                   MOVEL     '1'           ERRL00
059500060313     C                   SETON                                        8182
059600060313E   2C                   else
059700060314     c                   z-add     g08dat        vidpde
059800060313     C                   Z-ADD     G08INV        kdev              8 0          *DATA CORRENTE AA/M/
059900060313     c                   endif
060000060313     c                   endif
060100970529     C*
060200970529     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
060300970529IF  1C     VIDPCA        IFEQ      *BLANKS
060400060313     C     VIDPpo        ANDEQ     0
060500060313     C                   MOVEL     'FIL'         WTIPRI            3
060600970529E   1C                   ENDIF
060700970529     C*
060800970529     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
060900970529IF  1C     ERRL00        IFEQ      '0'
061000970529IF  2C     VIDPCA        IFNE      *BLANKS
061100060313     C     VIDPpo        ANDNE     0
061200970529     C                   MOVEL     MSG(6)        DSMSMS
061300970529     C                   MOVEL     '1'           ERRL00
061400060313     C                   SETON                                        8082
061500970529E   2C                   ENDIF
061600970529E   1C                   ENDIF
061700970529     C*
061800970529     C* CONTROLLA IL TIPO RICERCA
061900970529IF  1C     ERRL00        IFEQ      '0'
062000970529IF  2C     VIDPCA        IFNE      *BLANKS
062100970529     C                   MOVEL     'CAP'         WTIPRI
062200970529E   2C                   ENDIF
062300060313IF  2C     VIDPpo        IFNE      0
062400060313     C                   MOVEL     'FIL'         WTIPRI
062500970529E   2C                   ENDIF
062600970529E   1C                   ENDIF
062700970526     C*
062800970526     C                   ENDSR
062900970526     C*------------------------------------------------------------------------*
063000970526     C* LANPGM - LANCIA UN PROGRAMMA
063100970526     C*------------------------------------------------------------------------*
063200060314     C     LANPGM0       BEGSR
063300970526     C*
063400970526     C* IMPOSTA LA DS DI PROCEDURA
063500060314     c                   clear                   tisi00ds
063600060314     c                   movel     ddev          d00dri
063700060314     C                   MOVEL     WOP0          d00OP0                         *LIVELLO PROCEDURA
063800060314     C                   MOVEL     WOP1          d00OP1                         *LIVELLO PGM
063900060314     C                   MOVEL     '0'           d00F03                         *PREMUTO F03=FINE
064000060314     C                   MOVEL     '0'           d00F12                         *PREMUTO F12=RITORNO
064100060314     C                   MOVEL     '0'           d00ERR                         *'1'=ERRORE
064200060314     C                   MOVEL     *BLANKS       d00MSG                         *MESSAGGIO DI RITORN
064300060314     C                   MOVEL     DNAR          d00NAR                         *NAZIONE
064400060314     C                   MOVEL     DCAP          d00CAP                         *CAP
064500060314     C                   MOVEL     DLOC          d00LOC                         *LOCALITA'
064600970526     C*
064700970526     C                   CALL      WPGM
064800970612     C                   PARM                    KPJBA
064900060314     C                   PARM                    tisi00ds
065000970526     C*
065100970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
065200060314IF  1C     d00F03        IFEQ      '1'                                          *PREMUTO F03
065300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
065400970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
065500970526X   1C                   ELSE                                                   *NON PREMUTO F03
065600060314     C                   MOVEL     d00MSG        DSMSMS                         *MESSAGGIO DI RITORN
065700970526E   1C                   ENDIF
065800970526     C*
065900970526     C                   ENDSR
066000060314     C*------------------------------------------------------------------------*
066100060314     C* LANPGM - LANCIA UN PROGRAMMA
066200060314     C*------------------------------------------------------------------------*
066300060314     C     LANPGMp       BEGSR
066400060314     C*
066500060314     C* IMPOSTA LA DS DI PROCEDURA
066600060314     c                   clear                   tisip0ds
066700060314     C                   MOVEL     WOP0          dp0OP0                         *LIVELLO PROCEDURA
066800060314     C                   MOVEL     WOP1          dp0OP1                         *LIVELLO PGM
066900060314     C                   MOVEL     '0'           dp0F03                         *PREMUTO F03=FINE
067000060314     C                   MOVEL     '0'           dp0F12                         *PREMUTO F12=RITORNO
067100060314     C                   MOVEL     '0'           dp0ERR                         *'1'=ERRORE
067200060314     C                   MOVEL     *BLANKS       dp0MSG                         *MESSAGGIO DI RITORN
067300060314     C                   MOVEL     DNAR          dp0NAR                         *NAZIONE
067400060314     C                   MOVEL     DCAP          dp0CAP                         *CAP
067500060327     C                   MOVEL     Dprv          dp0prv                         *LOCALITA'
067600060404     C                   MOVEL     Ddup          dp0dup                         *LOCALITA'
067700060327     C                   MOVEL     DLOC          dp0LOC                         *LOCALITA'
067800060327     C                   MOVEL     Dfil          dp0fil                         *LOCALITA'
067900060315     C                   MOVEL     Ddev          dp0dri                         *LOCALITA'
068000060315     C                   MOVEL     Dcev          dp0cev                         *LOCALITA'
068100060331     C                   MOVEL     Dtaf          dp0taf                         *LOCALITA'
068200060314     C*
068300060314     C                   CALL      WPGM
068400060314     C                   PARM                    KPJBA
068500060314     C                   PARM                    tisip0ds
068600060314     C*
068700060314     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
068800060314IF  1C     dp0F03        IFEQ      '1'                                          *PREMUTO F03
068900060314     C                   MOVEL     '1'           ERRL00                         *ERRORE
069000060314     C                   MOVEL     '1'           F03L00                         *FINE PGM
069100060314X   1C                   ELSE                                                   *NON PREMUTO F03
069200060314     C                   MOVEL     dp0MSG        DSMSMS                         *MESSAGGIO DI RITORN
069300060314E   1C                   ENDIF
069400060314     C*
069500060314     C                   ENDSR
069600970527     C*------------------------------------------------------------------------*
069700970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
069800970527     C*------------------------------------------------------------------------*
069900970527     C     CARTAB        BEGSR
070000970527     C*
070100970527     C                   ENDSR
070200970526     C*------------------------------------------------------------------------*
070300970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
070400970526     C*------------------------------------------------------------------------*
070500970526     C     CHKFOP        BEGSR
070600970626     C*
070700970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
070800060404IF  1C     opzione       IFEQ      '01'
070900970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
071000970626     C                   MOVEL     'S'           OPDATT
071100970626     C                   MOVEL     OPDRIG        OPD(1)
071200970626X   1C                   ELSE
071300970626     C* OPZIONI
071400970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
071500970626     C                   MOVEL     'S'           OPDATT
071600970626     C                   MOVEL     OPDRIG        OPD(5)
071700970626     C                   MOVEL     OPD(13)       OPDRIG                         *SINONIMI
071800970626     C                   MOVEL     'S'           OPDATT
071900970626     C                   MOVEL     OPDRIG        OPD(13)
072000970626     C* FUNZIONI
072100970626     C                   MOVEL     FUD(13)       FUDRIG                         *SINONIMI
072200970626     C                   MOVEL     'S'           FUDATT
072300970626     C                   MOVEL     FUDRIG        FUD(13)
072400970626E   1C                   ENDIF
072500970626     C*
072600970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
072700060404IF  1C     opzione       IFEQ      '02'
072800970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
072900970626     C                   MOVEL     'S'           OPDATT
073000970626     C                   MOVEL     OPDRIG        OPD(2)
073100970627     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
073200970627     C                   MOVEL     'S'           OPDATT
073300970627     C                   MOVEL     OPDRIG        OPD(3)
073400970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
073500970626     C                   MOVEL     'S'           OPDATT
073600970626     C                   MOVEL     OPDRIG        OPD(4)
073700970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
073800970626     C                   MOVEL     'S'           FUDATT
073900970626     C                   MOVEL     FUDRIG        FUD(6)
074000060327    1C                   ENDIF
074100060331     c
074200060404IF  1C     opzione       IFEQ      '05'
074300060331     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
074400060331     C                   MOVEL     'N'           OPDATT
074500060331     C                   MOVEL     OPDRIG        OPD(2)
074600060331     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
074700060331     C                   MOVEL     'N'           OPDATT
074800060331     C                   MOVEL     OPDRIG        OPD(3)
074900060331     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
075000060331     C                   MOVEL     'N'           OPDATT
075100060331     C                   MOVEL     OPDRIG        OPD(4)
075200060331     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
075300060331     C                   MOVEL     'N'           FUDATT
075400060331     C                   MOVEL     FUDRIG        FUD(6)
075500060331    1C                   ENDIF
075600970526     C*
075700970526     C* GESTIONE OPZIONI A VIDEO
075800970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
075900970526     C                   EXSR      OPZGES
076000970526     C*
076100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
076200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
076300970526     C                   EXSR      FUNGES
076400970526     C*
076500970526     C                   ENDSR
076600020919     C*--------------------------------------------------------------------------------------------*
076700020919     C* REPERISCE I DATI UTENTE
076800020919     C*--------------------------------------------------------------------------------------------*
076900020919     C     REPDATIUTE    BEGSR
077000020919     C*
077100020919     C* INIZIALIZZA VARIABILI DI WRK
077200020919     C                   CLEAR                   TIBS34DS
077300020919     C                   CLEAR                   AZUTEDS
077400020919     C                   CLEAR                   DDATIUTE
077500020919     C*
077600020919     C     *DTAARA       DEFINE    §azute        azuteds
077700020919     C     *DTAARA       DEFINE    §datiute      ddatiute
077800020919     C                   IN(E)     *DTAARA
077900020919if  1C                   IF        %Error
078000020919     c                   EVAL      I34Tla = 'L'
078100020919     C                   CALL      'TIBS34R'
078200020919     C                   PARM                    Tibs34Ds
078300020919     C                   IN        *DTAARA
078400020919e   1C                   ENDIF
078500020919     C*
078600020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
078700020919     C                   MOVEL(P)  RSUT          DSFIRS           20
078800020919     C*
078900020919     C                   ENDSR
079000970526     C*------------------------------------------------------------------------*
079100970526     C* *INZSR - OPERAZIONI INIZIALI
079200970526     C*------------------------------------------------------------------------*
079300970526     C     *INZSR        BEGSR
079400970526     C*---
079500970526     C* RICEVIMENTO PARAMETRI
079600970526     C*---
079700970526     C     *ENTRY        PLIST
079800970612     C                   PARM                    KPJBA
079900060313     C                   PARM                    tisip0ds
080000970526     C*
080100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
080200970526     C     XMSGPA        PLIST
080300970526     C                   PARM                    DSMSMS
080400970526     C                   PARM                    DSMSPG
080500970526     C                   PARM                    DSMSKE
080600970526     C                   PARM                    DSMSER
080700970526     C*
080800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
080900970526     C                   CALL      'XKEY'
081000970526     C                   PARM                    DSKEY
081100970526     C*---
081200970526     C* VARIABILI NON RIFERITE AL DATA BASE
081300970526     C*---
081400970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
081500970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
081600970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
081700970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
081800970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
081900970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
082000970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
082100970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
082200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
082300060313     C     *LIKE         DEFINE    Dp0OP0        DEPOP0                         *DI DEPOSITO
082400060313     C     *LIKE         DEFINE    Dp0OP0        WOP0                           *DI LAVORO
082500060313     C     *LIKE         DEFINE    Dp0OP1        WOP1
082600060313     C     *LIKE         DEFINE    Dp0NAR        DNAR
082700060313     C     *LIKE         DEFINE    Dp0CAP        DCAP
082800060313     C     *LIKE         DEFINE    Dp0LOC        DLOC
082900970526     C*---
083000970526     C* VARIABILI DI CONTROLLO
083100970526     C*---
083200970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
083300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
083400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
083500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
083600970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
083700970530     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
083800970526     C*
083900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
084000060313     C                   MOVEL     'TISIP0R'     DSMSPG
084100060313     C                   MOVEL     'TISIP0R'     MSGQUE
084200970526     C                   MOVEL     *BLANKS       MSGKEY
084300970526     C                   MOVEL     *BLANKS       DSMSMS
084400970526     C                   MOVEL     *BLANKS       DSMSKE
084500970526     C                   Z-ADD     *ZEROS        DSMSER
084600060330     c
084700060313     C     *LIKE         DEFINE    Cepdev        DEPpde                         *DEPOSITI
084800060313     C     *LIKE         DEFINE    CepNAR        DEPNAR                         *DEPOSITI
084900060313     C     *LIKE         DEFINE    CepCAP        DEPCAP
085000060313     C     *LIKE         DEFINE    Cepfil        DEPppo
085100970526     C*---
085200970526     C* CHIAVI DI LETTURA
085300970526     C*---
085400060329     C* LETTURA AZcep01L
085500060330     C     kcep11        KLIST
085600060313     C                   KFLD                    Kfil                           *CAP
085700060313     C                   KFLD                    Knar                           *CAP
085800060313     C                   KFLD                    Kcap                           *CAP
085900060329     C* LETTURA AZCep01L
086000060330     C     Kcep12        KLIST
086100060313     C                   KFLD                    kfil                           *NAZIONE
086200060330     C* LETTURA AZCep02L
086300060330     C     Kcep21        KLIST
086400060330     C                   KFLD                    kdev                           *NAZIONE
086500060330     C                   KFLD                    kfil                           *NAZIONE
086600060330     C                   KFLD                    knar                           *NAZIONE
086700060330     C                   KFLD                    kcap                           *NAZIONE
086800060329     c     Ktab          Klist
086900060329     c                   Kfld                    codut             1 0
087000060329     c                   Kfld                    kCod
087100060329     c                   Kfld                    kKey
087200060329     c                   z-add     1             codut
087300970527     C* REPERIMENTO DATI UTENTE
087400020919     C                   EXSR      REPDATIUTE
087500970527     C*
087600970527     C* CARICA LE TABELLE OCCORRENTI
087700970527     C                   EXSR      CARTAB
087800970526     C*
087900970526     C* DEPOSITA LE OPZIONI DI ENTRATA
088000060313     C                   MOVEL     dp0OP0        DEPOP0                         *LIVELLO PROCEDURA
088100970526     C*
088200970526     C* CALCOLA LA DATA CORRENTE
088300970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
088400970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
088500970526     C                   Z-ADD     WN8           G08DAT
088600970526     C                   Z-ADD     *ZEROS        G08INV
088700970526     C                   MOVEL     '0'           G08ERR
088800970526     C                   CALL      'XSRDA8'
088900970526     C                   PARM                    WLBDA8
089000970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
089100060313     c
089200060313     c* Se non passata la data di ingresso, uso data corrente
089300060313     c                   if        dP0dri=0
089400060313     c                   eval      kdev=datcor
089500060313     c                   eval      vidpde=wn8
089600060313     c                   else
089700060313     c                   eval      kdev=dp0dri
089800060313     c* La imposto a video
089900060313     C                   Z-ADD     Dp0DRI        G08INV
090000060313     C                   Z-ADD     *ZEROS        G08DAT
090100060313     C                   MOVEL     '3'           G08ERR
090200060313     C                   CALL      'XSRDA8'
090300060313     C                   PARM                    WLBDA8
090400060313     C                   Z-ADD     G08DAT        VIDpde                         *DATA RIFERIMENTO
090500060313     c                   endif
090600970526     C*
090700970526     C* IMPOSTA I PARAMETRI DI INGRESSO
090800060313     C                   MOVEL     Dp0NAR        VIDPNA                         *NAZIONE
090900060313     C                   MOVEL     Dp0CAP        VIDPCA                         *CAP
091000060313     C                   MOVEL     Dp0fil        VIDPpo                         *CAP
091100060403     c                   eval      vidord='D'
091200060403     c                   if        dp0nar<>*blanks or dp0cap<>*blanks or
091300060403     c                             dp0fil>0
091400060403     c                   eval      vidord='C'
091500060403     c                   endif
091600060313     c
091700970526     C                   EXSR      CHKPAR
091800970526IF  1C     ERRL00        IFEQ      '1'
091900060313     C                   MOVEL     '1'           Dp0ERR
092000060313     C                   MOVEL     DSMSMS        Dp0MSG
092100970526X   1C                   ELSE
092200970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
092300970527     C                   MOVEL     VIDPCA        DEPCAP
092400060313     C                   MOVEL     VIDppo        DEPppo
092500060313     C                   MOVEL     VIDpde        DEPpde
092600060331     C                   MOVEL     VIDord        DEPord
092700970526E   1C                   ENDIF
092800970526     C*
092900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
093000060404     c                   eval      opzione=%subst(depop0:2:2)
093100970526     C                   EXSR      CHKFOP
093200970526     C*
093300970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
093400060404IF  1C     opzione       IFEQ      '01'
093500970530     C                   MOVEL     DEC(3)        VIDING
093600970530E   1C                   ENDIF
093700060404IF  1C     opzione       IFEQ      '02'
093800970530     C                   MOVEL     DEC(1)        VIDING
093900970530E   1C                   ENDIF
094000060404IF  1C     opzione       IFEQ      '05'
094100970530     C                   MOVEL     DEC(2)        VIDING
094200970530E   1C                   ENDIF
094300970526     C*
094400970526     C                   ENDSR
094500970526     C*------------------------------------------------------------------------*
094600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
094700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
094800970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
094900970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
095000970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
095100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
095200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
095300970526     C*------------------------------------------------------------------------*
095400970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
095500970526     C*------------------------------------------------------------------------*
095600970526     C     POSSFL        BEGSR
095700970526     C*
095800970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
095900970526     C                   EXSR      CLRSFL
096000970526     C*
096100970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
096200970526     C                   EXSR      SETFIL
096300970526     C*
096400970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
096500970526     C     SFLEOF        IFEQ      0
096600970526     C                   EXSR      CARSFL
096700970526     C                   END
096800970526     C*
096900970526     C                   ENDSR
097000970526     C*------------------------------------------------------------------------*
097100970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
097200970526     C*------------------------------------------------------------------------*
097300970526     C     ENTATT        BEGSR
097400970526     C*
097500970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
097600970526     C                   Z-ADD     0             OPZOK             1 0
097700970526     C*
097800970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
097900970526     C                   Z-ADD     0             SFLVAR            5 0
098000970526     C*
098100970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
098200970526     C                   Z-ADD     0             SFLERR
098300970526     C*
098400970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
098500970526     C                   Z-ADD     SFLULT        WFLULT
098600970526     C*
098700970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
098800060313     C                   READC     SIP0D05                                98
098900970526     C     *IN98         DOWEQ     '0'
099000970526     C*
099100970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
099200970526     C                   ADD       1             SFLVAR
099300970526     C*
099400970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
099500970526     C     OPZOK         IFEQ      0
099600970526     C*
099700970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
099800970526     C     OPZKEY        IFNE      *BLANKS
099900970526     C                   EXSR      OPZCON
100000970526     C                   END
100100970526     C*
100200970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
100300970526     C*            2. PULIZIA SCELTA IN SFL
100400970526     C     OPZOK         IFEQ      0
100500970526     C                   MOVEL     '0'           *IN41
100600970526     C                   MOVEL     *BLANKS       OPZKEY
100700970526     C                   ELSE
100800970526     C*
100900970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
101000970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
101100970526     C                   MOVEL     '1'           *IN41
101200970526     C                   Z-ADD     SFLULT        SFLERR
101300970526     C                   END
101400970526     C                   ELSE
101500970526     C*
101600970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
101700970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
101800970526     C                   MOVEL     '1'           *IN41
101900970526     C                   END                                                    OPZOK=0
102000970526     C*
102100970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
102200970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
102300970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
102400970526     C     OPZOK         IFNE      0
102500970526     C                   Z-ADD     SFLERR        SFLNBR
102600970526     C                   ELSE
102700970526     C                   Z-ADD     SFLULT        SFLNBR
102800970526     C                   END
102900970526     C*
103000970526     C* AGGIORNO RECORD SFL
103100060404     c                   movel     in03          *in03
103200060313     C                   UPDATE    SIP0D05
103300970526     C*
103400060313     C                   READC     SIP0D05                                98
103500970526     C                   END                                                    DO WHILE
103600970526     C*
103700970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
103800970526     C                   Z-ADD     WFLULT        SFLULT
103900970526     C*
104000970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
104100970526     C     SFLVAR        IFEQ      0
104200970526     C*---                 MOVEL'1'       FINL00
104300970526     C                   END
104400970526     C*
104500970526     C                   ENDSR
104600970526     C*------------------------------------------------------------------------*
104700970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
104800970526     C*------------------------------------------------------------------------*
104900970526     C     CLRSFL        BEGSR
105000970526     C*
105100970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
105200970526     C* (0=NON FINE FILE  1=FINE FILE)
105300970526     C                   Z-ADD     0             SFLEOF
105400970526     C*
105500970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
105600970526     C                   Z-ADD     0             SFLULT
105700970526     C*
105800970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
105900970526     C                   MOVEL     '1'           *IN40
106000060313     C                   WRITE     SIP0D06
106100970526     C                   MOVEL     '0'           *IN40
106200970526     C*
106300970526     C                   ENDSR
106400970526     C*------------------------------------------------------------------------*
106500970526     C* CARSFL - CARICAMENTO SFL
106600970526     C*------------------------------------------------------------------------*
106700970526     C     CARSFL        BEGSR
106800970526     C*
106900970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
107000970526     C     SFLULT        IFEQ      0
107100970526     C                   EXSR      SETFIL
107200970526     C                   END
107300970526     C*
107400970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
107500970526     C                   Z-ADD     SFLULT        WFLULT
107600970526     C*
107700970526     C* CICLO:
107800970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
107900970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
108000970526     C                   Z-ADD     1             SFLRIG
108100970526     C     SFLRIG        DOWLE     SFLPAG
108200970526     C     SFLEOF        ANDEQ     0
108300970526     C*
108400970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
108500970526     C                   MOVEL     *BLANKS       OPZKEY
108600970526     C*
108700970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
108800970526     C                   EXSR      IRESFL
108900970526     C*
109000970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
109100970526     C                   ADD       1             SFLULT
109200970526     C*
109300970526     C* SCRITTURA RECORD SFL
109400060313     C                   WRITE     SIP0D05
109500970526     C                   ADD       1             SFLRIG
109600970526     C*
109700970526     C* LETTURA FILE DA CARICARE
109800970526     C                   EXSR      LETFIL
109900970526     C                   END                                                    DO WHILE
110000970526     C*
110100970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
110200970526     C     WFLULT        IFNE      SFLULT
110300970526     C                   Z-ADD     WFLULT        SFLNBR
110400970526     C                   ADD       1             SFLNBR
110500970526     C                   END
110600970526     C*
110700970526     C                   ENDSR
110800970526     C*------------------------------------------------------------------------*
110900970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
111000970526     C*------------------------------------------------------------------------*
111100970526     C     OUTSFL        BEGSR
111200970526     C*
111300970526     C* WRITE RECORD TESTATA VIDEO
111400060313     C                   WRITE     SIP0D01
111500970526     C*
111600970526     C* VISUALIZZAZIONE MESSAGGIO
111700970526     C                   EXSR      SNDMSG
111800970526     C*
111900970526     C* WRITE RECORD OPZIONI VIDEO
112000060313     C                   WRITE     SIP0D02
112100970526     C*
112200970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
112300060313     C                   WRITE     SIP0D03
112400970526     C*
112500970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
112600060313     C                   WRITE     SIP0D04
112700970526     C*
112800970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
112900970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
113000970526     C     SFLEOF        IFEQ      0
113100970526     C                   MOVEL     '0'           *IN42
113200970526     C                   ELSE
113300970526     C*
113400970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
113500970526     C                   MOVEL     '1'           *IN42
113600970526     C                   END
113700970526     C*
113800970526     C* WRITE RECORD TASTI FUNZIONALI
113900060313     C                   WRITE     SIP0D09
114000970526     C*
114100970526     C* CASO DI SFL VUOTO
114200970526     C     SFLULT        IFLE      0
114300970526     C*
114400970526     C*    - WRITE RECORD VUOTO
114500060313     C                   WRITE     SIP0D0A
114600060313     C                   READ      SIP0D0A                                99
114700970526     C*
114800970526     C                   ELSE
114900970526     C*
115000970526     C* CASO DI SFL CON RECORDS
115100970526     C*
115200970526     C*    - WRITE RECORD CONTROLLO SFL
115300060313     C                   WRITE     SIP0D06
115400060313     C                   READ      SIP0D06                                99
115500970526     C                   END
115600970526     C*
115700970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
115800060313     C                   READ      SIP0D03                                99
115900970526     C*
116000970526     C                   ENDSR
116100970526     C*------------------------------------------------------------------------*
116200970526     C* SNDMSG - MANDA MESSAGGIO
116300970526     C*------------------------------------------------------------------------*
116400970526     C     SNDMSG        BEGSR
116500970526     C*
116600970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
116700970526     C                   MOVEL     '1'           *IN40
116800060313     C                   WRITE     SIP0DMC
116900970526     C                   MOVEL     '0'           *IN40
117000970526     C*
117100970526     C* VISUALIZZAZIONE MESSAGGIO
117200970526     C                   Z-ADD     0             MSGULT
117300970526     C                   CALL      'XMSG'        XMSGPA
117400970526     C     DSMSER        IFEQ      0
117500970526     C                   MOVEL     DSMSKE        MSGKEY
117600970526     C                   END
117700970526     C                   Z-ADD     1             MSGULT
117800060313     C                   WRITE     SIP0DMS                                      SFL MESS
117900060313     C                   WRITE     SIP0DMC                                      CONTR MESS
118000970526     C*
118100970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
118200970526     C                   MOVEL     *BLANKS       DSMSMS
118300970526     C*
118400970526     C                   ENDSR
118500970526     C*------------------------------------------------------------------------*
118600970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
118700970526     C*------------------------------------------------------------------------*
118800970526     C     FUNCON        BEGSR
118900970526     C*
119000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
119100970526     C                   EXSR      FUNVAR
119200970526     C*
119300970526     C* PULISCO CAMPI DI LAVORO
119400970526     C                   Z-ADD     0             I                 5 0
119500970526     C                   Z-ADD     0             W                 5 0
119600970526     C*
119700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
119800970526     C     FUNKEY        IFNE      *BLANKS
119900970526     C     1             DO        24            I
120000970526     C                   MOVEL     FUD(I)        FUDRIG
120100970526     C*
120200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
120300970526     C     FUNKEY        IFEQ      FUDFUN
120400970526     C     FUDATT        ANDEQ     'S'
120500970526     C                   Z-ADD     I             W
120600970526     C                   END
120700970526     C                   END                                                    DO WHILE
120800970526     C*
120900970526     C* FUNZIONE NON ATTIVA (ERRORE)
121000970526     C     W             IFEQ      0
121100970526     C*
121200970526     C* EMISSIONE MESSAGGIO DI ERRORE
121300970526     C                   MOVEL     MSG(2)        DSMSMS
121400970526     C                   END                                                    W=0
121500970526     C                   END                                                    FUNKEY NE *BLANKS
121600970526     C*
121700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
121800970526     C     W             IFNE      0
121900970526     C     FUNKEY        OREQ      *BLANKS
122000970526     C*
122100970526     C     FUNKEY        IFEQ      *BLANKS
122200970526     C                   EXSR      ENTGES
122300970526     C                   END
122400970526     C     FUNKEY        IFEQ      '03'
122500970526     C                   EXSR      F03GES
122600970526     C                   END
122700970526     C     FUNKEY        IFEQ      '05'
122800970526     C                   EXSR      F05GES
122900970526     C                   END
123000970526     C     FUNKEY        IFEQ      '06'
123100970526     C                   EXSR      F06GES
123200970526     C                   END
123300970526     C     FUNKEY        IFEQ      '12'
123400970526     C                   EXSR      F12GES
123500970526     C                   END
123600970529     C     FUNKEY        IFEQ      '13'
123700970529     C                   EXSR      F13GES
123800970529     C                   END
123900970526     C     FUNKEY        IFEQ      '23'
124000970526     C                   EXSR      F23GES
124100970526     C                   END
124200970526     C     FUNKEY        IFEQ      '24'
124300970526     C                   EXSR      F24GES
124400970526     C                   END
124500970526     C     FUNKEY        IFEQ      'RU'
124600970526     C                   EXSR      RUPGES
124700970526     C                   END
124800970526     C*
124900970526     C                   END
125000970526     C*
125100970526     C                   ENDSR
125200970526     C*------------------------------------------------------------------------*
125300970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
125400970526     C*------------------------------------------------------------------------*
125500970526     C     FUNVAR        BEGSR
125600970526     C*
125700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
125800970526     C     KEY           IFEQ      ENTER
125900970526     C                   MOVEL     *BLANKS       FUNKEY
126000970526     C                   END
126100970526     C     KEY           IFEQ      F1
126200970526     C                   MOVEL     '01'          FUNKEY
126300970526     C                   END
126400970526     C     KEY           IFEQ      F2
126500970526     C                   MOVEL     '02'          FUNKEY
126600970526     C                   END
126700970526     C     KEY           IFEQ      F3
126800970526     C                   MOVEL     '03'          FUNKEY
126900970526     C                   END
127000970526     C     KEY           IFEQ      F4
127100970526     C                   MOVEL     '04'          FUNKEY
127200970526     C                   END
127300970526     C     KEY           IFEQ      F5
127400970526     C                   MOVEL     '05'          FUNKEY
127500970526     C                   END
127600970526     C     KEY           IFEQ      F6
127700970526     C                   MOVEL     '06'          FUNKEY
127800970526     C                   END
127900970526     C     KEY           IFEQ      F7
128000970526     C                   MOVEL     '07'          FUNKEY
128100970526     C                   END
128200970526     C     KEY           IFEQ      F8
128300970526     C                   MOVEL     '08'          FUNKEY
128400970526     C                   END
128500970526     C     KEY           IFEQ      F9
128600970526     C                   MOVEL     '09'          FUNKEY
128700970526     C                   END
128800970526     C     KEY           IFEQ      F10
128900970526     C                   MOVEL     '10'          FUNKEY
129000970526     C                   END
129100970526     C     KEY           IFEQ      F11
129200970526     C                   MOVEL     '11'          FUNKEY
129300970526     C                   END
129400970526     C     KEY           IFEQ      F12
129500970526     C                   MOVEL     '12'          FUNKEY
129600970526     C                   END
129700970526     C     KEY           IFEQ      F13
129800970526     C                   MOVEL     '13'          FUNKEY
129900970526     C                   END
130000970526     C     KEY           IFEQ      F14
130100970526     C                   MOVEL     '14'          FUNKEY
130200970526     C                   END
130300970526     C     KEY           IFEQ      F15
130400970526     C                   MOVEL     '15'          FUNKEY
130500970526     C                   END
130600970526     C     KEY           IFEQ      F16
130700970526     C                   MOVEL     '16'          FUNKEY
130800970526     C                   END
130900970526     C     KEY           IFEQ      F17
131000970526     C                   MOVEL     '17'          FUNKEY
131100970526     C                   END
131200970526     C     KEY           IFEQ      F18
131300970526     C                   MOVEL     '18'          FUNKEY
131400970526     C                   END
131500970526     C     KEY           IFEQ      F19
131600970526     C                   MOVEL     '19'          FUNKEY
131700970526     C                   END
131800970526     C     KEY           IFEQ      F20
131900970526     C                   MOVEL     '20'          FUNKEY
132000970526     C                   END
132100970526     C     KEY           IFEQ      F21
132200970526     C                   MOVEL     '21'          FUNKEY
132300970526     C                   END
132400970526     C     KEY           IFEQ      F22
132500970526     C                   MOVEL     '22'          FUNKEY
132600970526     C                   END
132700970526     C     KEY           IFEQ      F23
132800970526     C                   MOVEL     '23'          FUNKEY
132900970526     C                   END
133000970526     C     KEY           IFEQ      F24
133100970526     C                   MOVEL     '24'          FUNKEY
133200970526     C                   END
133300970526     C     KEY           IFEQ      ROLLUP
133400970526     C                   MOVEL     'RU'          FUNKEY
133500970526     C                   END
133600970526     C     KEY           IFEQ      HELP
133700970526     C                   MOVEL     'HP'          FUNKEY
133800970526     C                   END
133900970526     C*
134000970526     C                   ENDSR
134100970526     C*------------------------------------------------------------------------*
134200970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
134300970526     C*------------------------------------------------------------------------*
134400970526     C     FUNGES        BEGSR
134500970526     C*
134600970526     C* PULISCO CAMPI DI LAVORO
134700970526     C                   Z-ADD     0             W                 5 0
134800970526     C*
134900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
135000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
135100970526     C     1             DO        24            I
135200970526     C                   MOVEL     FUD(I)        FUDRIG
135300970526     C*
135400970526     C* PULIZIA FUNZIONI PRECEDENTI
135500970526     C                   MOVEL     *BLANKS       FUV(I)
135600970526     C*
135700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
135800970526     C* (SALTA F23 E F24)
135900970526     C     FUDATT        IFEQ      'S'
136000970526     C     FUDFUN        ANDNE     '23'
136100970526     C     FUDFUN        ANDNE     '24'
136200970526     C                   ADD       1             W
136300970526     C                   MOVEL     FUDDES        FUV(W)
136400970526     C                   END
136500970526     C                   END                                                    DO WHILE
136600970526     C*
136700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
136800970526     C* ALTRIMENTI DISATTIVO F24
136900970526     C     1             DO        24            I
137000970526     C                   MOVEL     FUD(I)        FUDRIG
137100970526     C*
137200970526     C     FUDFUN        IFEQ      '24'
137300970526     C     W             IFGT      6
137400970526     C                   MOVEL     'S'           FUDATT
137500970526     C                   ELSE
137600970526     C                   MOVEL     *BLANKS       FUDATT
137700970526     C                   END                                                    W GT 8
137800970526     C                   MOVEL     FUDRIG        FUD(I)
137900970526     C                   END
138000970526     C                   END                                                    DO WHILE
138100970526     C*
138200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
138300970526     C     W             DIV       6             I
138400970526     C                   MVR                     Z
138500970526     C*
138600970526     C     Z             IFNE      0
138700970526     C     I             ADD       1             Z
138800970526     C                   ELSE
138900970526     C                   Z-ADD     I             Z
139000970526     C                   END
139100970526     C*
139200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
139300970526     C                   ADD       1             FUNGRU
139400970526     C     FUNGRU        IFGT      Z
139500970526     C     FUNGRU        ORLT      1
139600970526     C                   Z-ADD     1             FUNGRU
139700970526     C                   END
139800970526     C*
139900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
140000970526     C     FUNGRU        IFEQ      1
140100970526     C                   MOVEL     FUV(1)        FUN001
140200970526     C                   MOVEL     FUV(2)        FUN002
140300970526     C                   MOVEL     FUV(3)        FUN003
140400970526     C                   MOVEL     FUV(4)        FUN004
140500970526     C                   MOVEL     FUV(5)        FUN005
140600970526     C                   MOVEL     FUV(6)        FUN006
140700970526     C                   END
140800970526     C*
140900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
141000970526     C     FUNGRU        IFEQ      2
141100970526     C                   MOVEL     FUV(7)        FUN001
141200970526     C                   MOVEL     FUV(8)        FUN002
141300970526     C                   MOVEL     FUV(9)        FUN003
141400970526     C                   MOVEL     FUV(10)       FUN004
141500970526     C                   MOVEL     FUV(11)       FUN005
141600970526     C                   MOVEL     FUV(12)       FUN006
141700970526     C                   END
141800970526     C*
141900970526     C* TERZO GRUPPO DI 6 FUNZIONI
142000970526     C     FUNGRU        IFEQ      3
142100970526     C                   MOVEL     FUV(13)       FUN001
142200970526     C                   MOVEL     FUV(14)       FUN002
142300970526     C                   MOVEL     FUV(15)       FUN003
142400970526     C                   MOVEL     FUV(16)       FUN004
142500970526     C                   MOVEL     FUV(17)       FUN005
142600970526     C                   MOVEL     FUV(18)       FUN006
142700970526     C                   END
142800970526     C*
142900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
143000970526     C     FUNGRU        IFEQ      4
143100970526     C                   MOVEL     FUV(19)       FUN001
143200970526     C                   MOVEL     FUV(20)       FUN002
143300970526     C                   MOVEL     FUV(21)       FUN003
143400970526     C                   MOVEL     FUV(22)       FUN004
143500970526     C                   MOVEL     *BLANKS       FUN005
143600970526     C                   MOVEL     *BLANKS       FUN006
143700970526     C                   END
143800970526     C*
143900970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
144000970526     C                   MOVEL     *BLANKS       FUN007
144100970526     C                   MOVEL     *BLANKS       FUN008
144200970526     C*
144300970526     C     1             DO        24            I
144400970526     C                   MOVEL     FUD(I)        FUDRIG
144500970526     C     FUDFUN        IFEQ      '23'
144600970526     C     FUDATT        ANDEQ     'S'
144700970526     C                   MOVEL     FUDDES        FUN007
144800970526     C                   END
144900970526     C     FUDFUN        IFEQ      '24'
145000970526     C     FUDATT        ANDEQ     'S'
145100970526     C                   MOVEL     FUDDES        FUN008
145200970526     C                   END
145300970526     C                   END                                                    DO
145400970526     C*
145500970526     C                   ENDSR
145600970526     C*------------------------------------------------------------------------*
145700970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
145800970526     C*------------------------------------------------------------------------*
145900970526     C     OPZGES        BEGSR
146000970526     C*
146100970526     C* PULISCO CAMPI DI LAVORO
146200970526     C                   Z-ADD     0             W                 5 0
146300970526     C*
146400970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
146500970526     C* E CARICO TABELLA OPZIONI ATTIVE
146600970526     C     1             DO        24            I
146700970526     C                   MOVEL     OPD(I)        OPDRIG
146800970526     C*
146900970526     C* PULIZIA OPZIONI PRECEDENTI
147000970526     C                   MOVEL     *BLANKS       OPV(I)
147100970526     C*
147200970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
147300970526     C     OPDATT        IFEQ      'S'
147400970526     C                   ADD       1             W
147500970526     C                   MOVEL     OPDDES        OPV(W)
147600970526     C                   END
147700970526     C                   END                                                    DO WHILE
147800970526     C*
147900970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
148000970526     C* ALTRIMENTI DISATTIVO F23
148100970526     C     1             DO        24            I
148200970526     C                   MOVEL     FUD(I)        FUDRIG
148300970526     C*
148400970526     C     FUDFUN        IFEQ      '23'
148500970526     C     W             IFGT      8
148600970526     C                   MOVEL     'S'           FUDATT
148700970526     C                   ELSE
148800970526     C                   MOVEL     *BLANKS       FUDATT
148900970526     C                   END                                                    W GT 8
149000970526     C                   MOVEL     FUDRIG        FUD(I)
149100970526     C                   END
149200970526     C                   END                                                    DO WHILE
149300970526     C*
149400970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
149500970526     C     W             DIV       8             I
149600970526     C                   MVR                     Z                 5 0
149700970526     C*
149800970526     C     Z             IFNE      0
149900970526     C     I             ADD       1             Z
150000970526     C                   ELSE
150100970526     C                   Z-ADD     I             Z
150200970526     C                   END
150300970526     C*
150400970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
150500970526     C                   ADD       1             OPZGRU
150600970526     C     OPZGRU        IFGT      Z
150700970526     C     OPZGRU        ORLT      1
150800970526     C                   Z-ADD     1             OPZGRU
150900970526     C                   END
151000970526     C*
151100970526     C* PRIMO GRUPPO DI 8 OPZIONI
151200970526     C     OPZGRU        IFEQ      1
151300970526     C                   MOVEL     OPV(1)        OPZ001
151400970526     C                   MOVEL     OPV(2)        OPZ002
151500970526     C                   MOVEL     OPV(3)        OPZ003
151600970526     C                   MOVEL     OPV(4)        OPZ004
151700970526     C                   MOVEL     OPV(5)        OPZ005
151800970526     C                   MOVEL     OPV(6)        OPZ006
151900970526     C                   MOVEL     OPV(7)        OPZ007
152000970526     C                   MOVEL     OPV(8)        OPZ008
152100970526     C                   END
152200970526     C*
152300970526     C* SECONDO GRUPPO DI 8 OPZIONI
152400970526     C     OPZGRU        IFEQ      2
152500970526     C                   MOVEL     OPV(9)        OPZ001
152600970526     C                   MOVEL     OPV(10)       OPZ002
152700970526     C                   MOVEL     OPV(11)       OPZ003
152800970526     C                   MOVEL     OPV(12)       OPZ004
152900970526     C                   MOVEL     OPV(13)       OPZ005
153000970526     C                   MOVEL     OPV(14)       OPZ006
153100970526     C                   MOVEL     OPV(15)       OPZ007
153200970526     C                   MOVEL     OPV(16)       OPZ008
153300970526     C                   END
153400970526     C*
153500970526     C* TERZO GRUPPO DI 8 OPZIONI
153600970526     C     OPZGRU        IFEQ      3
153700970526     C                   MOVEL     OPV(17)       OPZ001
153800970526     C                   MOVEL     OPV(18)       OPZ002
153900970526     C                   MOVEL     OPV(19)       OPZ003
154000970526     C                   MOVEL     OPV(20)       OPZ004
154100970526     C                   MOVEL     OPV(21)       OPZ005
154200970526     C                   MOVEL     OPV(22)       OPZ006
154300970526     C                   MOVEL     OPV(23)       OPZ007
154400970526     C                   MOVEL     OPV(24)       OPZ008
154500970526     C                   END
154600970526     C*
154700970526     C                   ENDSR
154800970526     C*------------------------------------------------------------------------*
154900970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
155000970526     C*------------------------------------------------------------------------*
155100970526     C     OPZCON        BEGSR
155200970526     C*
155300970526     C* PULISCO CAMPI DI LAVORO
155400970526     C                   Z-ADD     0             I                 5 0
155500970526     C                   Z-ADD     0             W                 5 0
155600970526     C*
155700970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
155800970526     C     1             DO        24            I
155900970526     C                   MOVEL     OPD(I)        OPDRIG
156000970526     C*
156100970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
156200970526     C     OPZKEY        IFEQ      OPDOPZ
156300970526     C     OPZKEY        OREQ      OPDOP1
156400970526     C*
156500970526     C     OPDATT        IFEQ      'S'
156600970526     C                   MOVEL     OPDOPZ        OPZKEY
156700970526     C                   Z-ADD     I             W
156800970526     C                   END
156900970526     C                   END
157000970526     C*
157100970526     C                   END                                                    DO WHILE
157200970526     C*
157300970526     C* OPZIONE NON ATTIVA (ERRORE)
157400970526     C     W             IFEQ      0
157500970526     C*
157600970526     C* OPZIONE NON OK
157700970526     C                   Z-ADD     1             OPZOK
157800970526     C*
157900970526     C* EMISSIONE MESSAGGIO DI ERRORE
158000970526     C                   MOVEL     MSG(1)        DSMSMS
158100970526     C                   ELSE
158200970526     C*
158300970526     C* OPZIONE ATTIVA (PROCEDI ...)
158400970526     C*
158500970526     C     OPZKEY        IFEQ      ' 1'
158600970526     C                   EXSR      O01GES
158700970526     C                   END
158800970526     C     OPZKEY        IFEQ      ' 2'
158900060328     C                   EXSR      O02GES
159000970526     C                   END
159100970611     C     OPZKEY        IFEQ      ' 3'
159200970611     C                   EXSR      O03GES
159300970611     C                   END
159400970526     C     OPZKEY        IFEQ      ' 4'
159500970526     C                   EXSR      O04GES
159600970526     C                   END
159700970526     C     OPZKEY        IFEQ      ' 5'
159800970526     C                   EXSR      O05GES
159900970526     C                   END
160000970529     C     OPZKEY        IFEQ      '13'
160100970529     C                   EXSR      O13GES
160200970529     C                   END
160300970526     C                   END                                                    OPZ.NON ATTIVA
160400970526     C*
160500970526     C                   ENDSR
160600970526     C*------------------------------------------------------------------------*
160700970526     C* NOTE COSTRUZIONE SCHIERE
160800970526     C*------------------------------------------------------------------------*
160900970526     C*
161000970526     C* -MSG-MESSAGGI
161100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
161200970526     C* (NON TOCCARE MSG,1 E MSG,2)
161300970526     C*
161400970526     C* -CMD-COMANDI
161500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
161600970526     C*
161700970526     C* -OPD-OPZIONI
161800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
161900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
162000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
162100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
162200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
162300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
162400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
162500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
162600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
162700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
162800970526     C*
162900970526     C* -FUD-FUNZIONI
163000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
163100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
163200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
163300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
163400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
163500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
163600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
163700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
163800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
163900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
164000970526     O*------------------------------------------------------------------------*
164100970526** DEC - DECODIFICHE
164200970526    GESTIONE                                                     1
164300970530INTERROGAZIONE                                                   2
164400970530     SCELTA                                                      3
1645009705264
1646009705265
164700970526** MSG - MESSAGGI
164800060313TISIP0R- Opzione non gestita                                                   1
164900060313TISIP0R- Tasto funzionale non gestito                                          2
165000060313TISIP0R- Data di riferimento errata
165100060329TISIP0R- Impossibile richiedere Sinonimi per un P.Operativo                    4
165200060313TISIP0R-                                                                       5
165300060403TISIP0R- Accettato solo un parametro di inizio lista (P.O. o Naz/CAP)          6
165400060313TISIP0R- 7                                                                     7
165500060313TISIP0R- 8                                                                     8
165600060313TISIP0R- 9                                                                     9
165700060313TISIP0R- 10                                                                   10
165800060313TISIP0R- 11                                                                   11
165900060313TISIP0R- 12                                                                   12
166000060313TISIP0R- 13                                                                   13
166100060313TISIP0R- 14                                                                   14
166200060313TISIP0R- 15                                                                   15
166300060313TISIP0R- 16                                                                   16
166400060313TISIP0R- 17                                                                   17
166500060313TISIP0R- 18                                                                   18
166600060313TISIP0R- 19                                                                   19
166700060313TISIP0R- 20                                                                   20
166800970526** OPD - OPZIONI
166900970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
167000060315 2=Modifica        -2 -     (S/N)=S                        opz. 02¬
167100060315 3=Duplica         -3 -     (S/N)=S                        opz. 03¬
167200060315 4=Annullamento    -4 -     (S/N)=S                        opz. 04¬
167300060315 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
167400970526                   -6 -     (S/N)=N                        opz. 06¬
167500060315                   -7 -     (S/N)=N                        opz. 07¬
167600970527                   -8 -     (S/N)=N                        opz. 08¬
167700970527                   -9 -     (S/N)=N                        opz. 09¬
167800970527                   -10-     (S/N)=N                        opz. 10¬
167900970529                   -11-     (S/N)=N                        opz. 11¬
168000970529                   -12-     (S/N)=N                        opz. 12¬
16810006031513=Sinonimi        -13-     (S/N)=S                        opz. 13¬
168200970526                   -  -     (S/N)=N                        opz. 14¬
168300970526                   -  -     (S/N)=N                        opz. 15¬
168400970526                   -  -     (S/N)=N                        opz. 16¬
168500970526                   -  -     (S/N)=N                        opz. 17¬
168600970526                   -  -     (S/N)=N                        opz. 18¬
168700970526                   -  -     (S/N)=N                        opz. 19¬
168800970526                   -  -     (S/N)=N                        opz. 20¬
168900970526                   -  -     (S/N)=N                        opz. 21¬
169000970526                   -  -     (S/N)=N                        opz. 22¬
169100970526                   -  -     (S/N)=N                        opz. 23¬
169200970526                   -  -     (S/N)=N                        opz. 24¬
169300970526** FUD - FUNZIONI
169400970526                   -        (S/N)=N                        funz.01¬
169500970526                   -        (S/N)=N                        funz.02¬
169600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
169700970526                   -        (S/N)=N                        funz.04¬
169800970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
169900060315F06=Inserimento    -        (S/N)=S                        funz.06¬
170000970526                   -        (S/N)=N                        funz.07¬
170100970527                   -        (S/N)=N                        funz.08¬
170200970527                   -        (S/N)=N                        funz.09¬
170300970527                   -        (S/N)=N                        funz.10¬
170400970526                   -        (S/N)=N                        funz.11¬
170500970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
170600060315F13=Sinonimi       -        (S/N)=S                        funz.13¬
170700970526                   -        (S/N)=N                        funz.14¬
170800970526                   -        (S/N)=N                        funz.15¬
170900970526                   -        (S/N)=N                        funz.16¬
171000970526                   -        (S/N)=N                        funz.17¬
171100970526                   -        (S/N)=N                        funz.18¬
171200970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
171300970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
171400970527                   -        (S/N)=N                        funz.21¬
171500970527                   -        (S/N)=N                        funz.22¬
171600970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
171700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
