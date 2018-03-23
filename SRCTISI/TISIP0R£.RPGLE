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
017100060328     c                   clear                   dcev
017200060331     c                   clear                   dtaf
017300970602     C                   MOVEL     *BLANKS       DLOC
017400970529     C                   MOVEL     DEPOP0        WOP0
017500970529     C                   MOVEL     'F06'         WOP1
017600060315     C                   MOVEL     'TISIP1R'     WPGM              8
017700060314     C                   EXSR      LANPGMp                                      *LANCIA PGM
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
018900060313     C                   MOVEL     '1'           dp0F12                         *DI PROCEDURA
019000970526     C*
019100970526     C                   ENDSR
019200970529     C*------------------------------------------------------------------------*
019300970529     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
019400970529     C*------------------------------------------------------------------------*
019500970529     C     F13GES        BEGSR
019600970529     C*
019700970602     C                   MOVEL     *BLANKS       DNAR
019800970602     C                   MOVEL     *BLANKS       DCAP
019900970602     C                   MOVEL     *BLANKS       DLOC
020000060314     c                   movel     datcor        ddev
020100060331     C                   MOVEL     'S05'         WOP0
020200970529     C                   MOVEL     'F13'         WOP1
020300970529     C                   MOVEL     'TISI35R'     WPGM              8
020400060314     C                   EXSR      LANPGM0
020500970529     C*
020600970529     C                   ENDSR
020700970526     C*------------------------------------------------------------------------*
020800970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
020900970526     C*------------------------------------------------------------------------*
021000970526     C     F23GES        BEGSR
021100970526     C*
021200970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
021300970526     C*
021400970526     C                   ENDSR
021500970526     C*------------------------------------------------------------------------*
021600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021700970526     C*------------------------------------------------------------------------*
021800970526     C     F24GES        BEGSR
021900970526     C*
022000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022100970526     C*
022200970526     C                   ENDSR
022300970526     C*------------------------------------------------------------------------*
022400970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
022500970526     C*------------------------------------------------------------------------*
022600970526     C     RUPGES        BEGSR
022700970526     C*
022800970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
022900970526     C*
023000970526     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200970526     C* ENTGES - GESTIONE ENTER -> INVIO
023300970526     C*------------------------------------------------------------------------*
023400970526     C     ENTGES        BEGSR
023500970526     C*
023600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
023700970526     C                   EXSR      NEWPOS
023800970526     C*
023900970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
024000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024100970526IF  2C     SWPOSI        IFNE      0
024200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
024300970526X   2C                   ELSE
024400970526IF  3C     SFLULT        IFNE      0
024500970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
024600970526     C*
024700970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
024800970526IF  4C     SWMODI        IFNE      0
024900970526     C                   EXSR      POSSFL
025000970526E   4C                   ENDIF
025100970526E   3C                   ENDIF
025200970526E   2C                   ENDIF
025300970526E   1C                   ENDIF
025400970526     C*
025500970526     C                   ENDSR
025600970526     C*------------------------------------------------------------------------*
025700970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
025800970526     C*------------------------------------------------------------------------*
025900970526     C     O01GES        BEGSR
026000970526     C*
026100970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
026200060331     C                   MOVEL     VIDTAF        dp0TAF
026300060331     C                   MOVEL     VIDNAR        dp0NAR
026400060313     C                   MOVEL     VIDCAP        dp0CAP
026500060313     C                   MOVEL     VIDLOC        dp0LOC
026600060313     C                   MOVEL     VIDprv        dp0PRV
026700060313     C                   MOVEL     VIDfil        dp0fil
026800060327     C                   MOVEL     VIDcev        Dcev
026900060327     C                   MOVEL     amgdev        Ddev
027000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
027100970526     C*
027200970526     C                   ENDSR
027300970526     C*------------------------------------------------------------------------*
027400970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
027500970526     C*------------------------------------------------------------------------*
027600970526     C     O02GES        BEGSR
027700970526     C*
027800970527     C* MODIFICA
027900970704IF  1C     ERRL00        IFEQ      '0'
028000060327     C                   MOVEL     VIDprv        Dprv
028100060327     C                   MOVEL     VIDNAR        DNAR
028200060331     C                   MOVEL     VIDTAF        DTAF
028300970602     C                   MOVEL     VIDCAP        DCAP
028400970602     C                   MOVEL     VIDLOC        DLOC
028500060315     C                   MOVEL     VIDfil        Dfil
028600060315     C                   MOVEL     VIDcev        Dcev
028700060404     C                   MOVEL     VIDdup        Ddup
028800060315     C                   MOVEL     amgdev        Ddev
028900970529     C                   MOVEL     DEPOP0        WOP0
029000970526     C                   MOVEL     'O02'         WOP1
029100060315     C                   MOVEL     'TISIP1R'     WPGM
029200060315     C                   EXSR      LANPGMP
029300060328     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
029400970528E   1C                   ENDIF
029500970526     C*
029600970526     C                   ENDSR
029700970611     C*------------------------------------------------------------------------*
029800970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
029900970611     C*------------------------------------------------------------------------*
030000970611     C     O03GES        BEGSR
030100970611     C*
030200970611     C* DUPLICA
030300970611IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030400060329     C                   MOVEL     VIDprv        Dprv
030500060329     C                   MOVEL     VIDNAR        DNAR
030600060331     C                   MOVEL     VIDTAF        DTAF
030700060329     C                   MOVEL     VIDCAP        DCAP
030800060329     C                   MOVEL     VIDLOC        DLOC
030900060329     C                   MOVEL     VIDfil        Dfil
031000060329     C                   MOVEL     VIDcev        Dcev
031100060404     C                   clear                   Ddup
031200060329     C                   MOVEL     amgdev        Ddev
031300060329     C                   MOVEL     DEPOP0        WOP0
031400060329     C                   MOVEL     'O03'         WOP1
031500060329     C                   MOVEL     'TISIP1R'     WPGM
031600060329     C                   EXSR      LANPGMP
031700970611     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
031800970611E   1C                   ENDIF
031900970611     C*
032000970611     C                   ENDSR
032100970526     C*------------------------------------------------------------------------*
032200970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
032300970526     C*------------------------------------------------------------------------*
032400970526     C     O04GES        BEGSR
032500970526     C*
032600060327IF  1C     ERRL00        IFEQ      '0'
032700060327     C                   MOVEL     VIDprv        Dprv
032800060327     C                   MOVEL     VIDNAR        DNAR
032900060331     C                   MOVEL     VIDTAF        DTAF
033000060327     C                   MOVEL     VIDCAP        DCAP
033100060327     C                   MOVEL     VIDLOC        DLOC
033200060327     C                   MOVEL     VIDfil        Dfil
033300060327     C                   MOVEL     VIDcev        Dcev
033400060404     C                   MOVEL     VIDdup        Ddup
033500060327     C                   MOVEL     amgdev        Ddev
033600060327     C                   MOVEL     DEPOP0        WOP0
033700060327     C                   MOVEL     'O04'         WOP1
033800060327     C                   MOVEL     'TISIP1R'     WPGM
033900060327     C                   EXSR      LANPGMP
034000060327E   1C                   ENDIF
034100970526     C*
034200970526     C                   ENDSR
034300970526     C*------------------------------------------------------------------------*
034400970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
034500970526     C*------------------------------------------------------------------------*
034600970526     C     O05GES        BEGSR
034700970526     C*
034800060327     C                   MOVEL     VIDprv        Dprv
034900060327     C                   MOVEL     VIDNAR        DNAR
035000060331     C                   MOVEL     VIDTAF        DTAF
035100060327     C                   MOVEL     VIDCAP        DCAP
035200060327     C                   MOVEL     VIDLOC        DLOC
035300060327     C                   MOVEL     VIDfil        Dfil
035400060327     C                   MOVEL     VIDcev        Dcev
035500060404     C                   MOVEL     VIDdup        Ddup
035600060327     C                   MOVEL     amgdev        Ddev
035700060327     C                   MOVEL     DEPOP0        WOP0
035800060327     C                   MOVEL     'O05'         WOP1
035900060327     C                   MOVEL     'TISIP1R'     WPGM
036000060327     C                   EXSR      LANPGMP
036100970526     C*
036200970526     C                   ENDSR
036300970529     C*------------------------------------------------------------------------*
036400970529     C* O13GES - GESTIONE OPZIONE 13=SINONIMI
036500970529     C*------------------------------------------------------------------------*
036600970529     C     O13GES        BEGSR
036700970529     C*
036800060329     c* non richiedibile se c'e' il p.o.
036900060329     c                   if        vidfil>*zeros
037000060329     C                   MOVEL     MSG(4)        DSMSMS
037100060329     C                   MOVEL     '1'           ERRL00
037200060329     c                   else
037300970602     C                   MOVEL     VIDNAR        DNAR
037400970602     C                   MOVEL     VIDCAP        DCAP
037500970602     C                   MOVEL     VIDLOC        DLOC
037600060314     C                   MOVEL     amgdev        Ddev
037700970529     C                   MOVEL     DEPOP0        WOP0
037800970529     C                   MOVEL     'S'           WOP0
037900970529     C                   MOVEL     'O13'         WOP1
038000970529     C                   MOVEL     'TISI35R'     WPGM
038100060314     C                   EXSR      LANPGM0
038200060329     c                   endif
038300970529     C*
038400970529     C                   ENDSR
038500970526     C*------------------------------------------------------------------------*
038600970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
038700970526     C*------------------------------------------------------------------------*
038800970526     C     NEWPOS        BEGSR
038900970526     C*
039000970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
039100970526     C*         1= RICHIESTO POSIZIONAMENTO
039200970526     C                   Z-ADD     0             SWPOSI            7 0
039300970526     C*
039400970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
039500970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
039600970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
039700970526     C                   Z-ADD     0             SWMODI            1 0
039800970530     C*
039900970530     C* REIMPOSTA INDICATORI DI ERRORE
040000060313     C                   SETOFF                                       808281
040100970526     C*
040200060403     c* Se immesso p.o. o cap. ordinamento deve essere per po/cap
040300060403     c                   if        vidpna<>*blanks or vidpca<>*blanks or
040400060403     c                             vidppo>0
040500060403     c                   eval      vidord='C'
040600060403     c                   endif
040700060403     c
040800060403     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
040900970527IF  1C     VIDPNA        IFNE      DEPNAR
041000970527     C     VIDPCA        ORNE      DEPCAP
041100060313     C     VIDPpo        ORNE      DEPppo
041200060313     C     VIDPde        ORNE      DEPpde
041300060330     C     VIDord        ORNE      DEPord
041400970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
041500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
041600970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
041700970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
041800970527     C                   MOVEL     VIDPCA        DEPCAP
041900060313     C                   MOVEL     VIDppo        DEPppo
042000060313     C                   MOVEL     VIDpde        DEPpde
042100060330     C                   MOVEL     VIDord        DEPord
042200970526E   2C                   ENDIF
042300970526E   1C                   ENDIF
042400970526     C*
042500970526     C                   ENDSR
042600970526     C*------------------------------------------------------------------------*
042700970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
042800970526     C*------------------------------------------------------------------------*
042900970526     C     SETFIL        BEGSR
043000060330     c                   clear                   knar
043100060330     c                   clear                   kcap
043200060330     c                   clear                   kfil
043300060330     c
043400060330     c                   if        vidord='C'
043500060330     c                   seton                                        02
043600060330     c                   setoff                                       01
043700060330     c                   else
043800060330     c                   seton                                        01
043900060330     c                   setoff                                       02
044000060330     c                   endif
044100970529     C*
044200970529     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
044300060313     c                   select
044400060313IF  1C     WTIPRI        wheneq    'CAP'                                        *PER CAP
044500060313     c                   if        kdev=0
044600060313     C                   MOVEL     datcor        Kdev
044700060313     c                   endif
044800060313     C                   MOVEL     VIDPNA        KNAR
044900060313     C                   MOVEL     VIDPCA        KCAP
045000060330     c* Ordinamento per CAP
045100060330     c                   if        vidord='C'
045200060330     C     kcep11        SETLL     AZCep01L                           99
045300060330     c                   else
045400060330     c* Ordinamento per Data
045500060330     C     kcep21        SETLL     AZCep02L                           99
045600060330     c                   endif
045700060313E   1C
045800060313IF  1C     WTIPRI        wheneq    'FIL'                                        *PER LOCALITA'
045900060313     c                   if        kdev=0
046000060313     C                   MOVEL     datcor        Kdev
046100060313     c                   endif
046200060313     c                   movel     vidppo        kfil
046300060330     c* Ordinamento per p.o.
046400060330     c                   if        vidord='C'
046500060330     C     Kcep12        SETLL     AZCep01L                           99
046600060330     c                   else
046700060330     c* Ordinamento per Data
046800060330     C     Kcep21        SETLL     AZCep02L                           99
046900060330     c                   endif
047000060330     c
047100060313E   1C                   ENDsl
047200970526     C* LETTURA RECORD
047300970526IF  1C     *IN99         IFEQ      *OFF
047400970526     C                   EXSR      LETFIL
047500970526E   1C                   ENDIF
047600970526     C*
047700970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
047800970526IF  1C     *IN99         IFEQ      *ON
047900970526     C                   Z-ADD     1             SFLEOF
048000970526E   1C                   ENDIF
048100970526     C*
048200970526     C                   ENDSR
048300970526     C*------------------------------------------------------------------------*
048400970526     C* LETFIL - LETTURA RECORD
048500970526     C*------------------------------------------------------------------------*
048600970526     C     LETFIL        BEGSR
048700970526     C*
048800970526     C* LEGGE FINO A:
048900970526     C*     - FINE FILE
049000970526     C*     - TROVATO RECORD VALIDO
049100970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
049200970526DO  1C     *IN99         DOWEQ     *OFF
049300970526     C     WRECOK        ANDEQ     'N'
049400970529     C*
049500970529     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
049600060330     c                   if        vidord='C'
049700060404     C                   READ      AZcep01L                               99
049800060330     c                   else
049900060404     C                   READ      AZcep02L                               99
050000060330     c                   endif
050100970526     C*
050200970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
050300970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
050400970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
050500970530X   2C                   ELSE
050600970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
050700970530E   2C                   ENDIF
050800970526E   1C                   ENDDO
050900970526     C*
051000970526     C                   ENDSR
051100970526     C*------------------------------------------------------------------------*
051200970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
051300970526     C*------------------------------------------------------------------------*
051400970526     C     CHKREC        BEGSR
051500970526     C*
051600060329     c                   if        cepdev>=kdev
051700970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
051800060329     c                   endif
051900060329     c
052000970526     C                   ENDSR
052100970526     C*------------------------------------------------------------------------*
052200970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
052300970526     C*------------------------------------------------------------------------*
052400970526     C     IRESFL        BEGSR
052500970529     C*
052600970529     C                   MOVEL     *BLANKS       OPZKEY
052700060329     c                   clear                   viduni
052800060315     c* Se presente p.o. decodifico
052900060314     c                   if        cepfil>0
053000060314     C                   MOVEL     Cepfil        vIDfil
053100060315     c     cepfil        chain     azorg01l
053200060329     c                   if        not %found(azorg01l)
053300060329     c                   eval      orgdes=*all'?'
053400060315     c                   endif
053500060329     c                   eval      viduni=vidfil +'-'+ orgdes
053600060315     c
053700060315     c                   else
053800060314     c                   clear                   vidfil
053900060404     c                   eval      viduni=cepnar+' '+cepcap+' '+
054000060404     c                             %subst(ceploc:1:33)+' '+cepprv
054100060314     c                   endif
054200060331     C                   MOVEL     CepTAF        VIDTAF
054300060331     C                   MOVEL     CepNAR        VIDNAR
054400060313     C                   MOVEL     CepCAP        VIDCAP
054500060313     C                   MOVEL     CepLOC        VIDLOC
054600060313     C                   MOVEL     Cepprv        VIDprv
054700060313     c
054800060314     C                   movel     Cepcev        VIDcev
054900060404     C                   movel     Cepdup        VIDdup
055000060404     c
055100060404     c* Se diverso dallo STD evidenzio
055200060404     c                   setoff                                       03
055300060404     c                   eval      kcod='2Z'
055400060404     c                   movel(p)  vidcev        kkey
055500060404     c     ktab          chain     tabel00f
055600060404     c                   if        %found(tabel00f)
055700060404     c                   movel     tbluni        ds2z
055800060404     c                   if        §2zdep<>cepdup
055900060404     c                   seton                                        03
056000060404     c                   endif
056100060404     c                   endif
056200060404     c                   movel     *in03         in03
056300060404     c
056400060329     c* Decodifico evento
056500060329     c                   eval      kcod='2A'
056600060329     c                   movel(p)  vidcev        kkey
056700060329     c     ktab          chain     tabel00f
056800060329     c                   if        %found(tabel00f)
056900060329     c                   movel     tbluni        deccev
057000060329     c                   else
057100060329     c                   movel     *all'?'       deccev
057200060329     c                   endif
057300060314     C                   movel     Cepdev        amgdev
057400060313     c     *iso          movel     cepdev        dataiso
057500060329     c                   movel     dataiso       datadmy
057600060329     c                   movel     datadmy       viddev
057700060313     c
057800970526     C                   ENDSR
057900970526     C*------------------------------------------------------------------------*
058000970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
058100970526     C*------------------------------------------------------------------------*
058200970526     C     CHKPAR        BEGSR
058300060313     c* controllo data di riferimento se errata
058400060313     c                   clear                   kdev
058500060313     c                   if        vidpde>0
058600060313     C                   Z-ADD     vidpde        G08DAT
058700060313     C                   Z-ADD     *ZEROS        G08INV
058800060313     C                   MOVEL     '0'           G08ERR
058900060313     C                   CALL      'XSRDA8'
059000060313     C                   PARM                    WLBDA8
059100060313     c                   if        g08err='1'
059200060313     C                   MOVEL     MSG(3)        DSMSMS
059300060313     C                   MOVEL     '1'           ERRL00
059400060313     C                   SETON                                        8182
059500060313E   2C                   else
059600060314     c                   z-add     g08dat        vidpde
059700060313     C                   Z-ADD     G08INV        kdev              8 0          *DATA CORRENTE AA/M/
059800060313     c                   endif
059900060313     c                   endif
060000970529     C*
060100970529     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
060200970529IF  1C     VIDPCA        IFEQ      *BLANKS
060300060313     C     VIDPpo        ANDEQ     0
060400060313     C                   MOVEL     'FIL'         WTIPRI            3
060500970529E   1C                   ENDIF
060600970529     C*
060700970529     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
060800970529IF  1C     ERRL00        IFEQ      '0'
060900970529IF  2C     VIDPCA        IFNE      *BLANKS
061000060313     C     VIDPpo        ANDNE     0
061100970529     C                   MOVEL     MSG(6)        DSMSMS
061200970529     C                   MOVEL     '1'           ERRL00
061300060313     C                   SETON                                        8082
061400970529E   2C                   ENDIF
061500970529E   1C                   ENDIF
061600970529     C*
061700970529     C* CONTROLLA IL TIPO RICERCA
061800970529IF  1C     ERRL00        IFEQ      '0'
061900970529IF  2C     VIDPCA        IFNE      *BLANKS
062000970529     C                   MOVEL     'CAP'         WTIPRI
062100970529E   2C                   ENDIF
062200060313IF  2C     VIDPpo        IFNE      0
062300060313     C                   MOVEL     'FIL'         WTIPRI
062400970529E   2C                   ENDIF
062500970529E   1C                   ENDIF
062600970526     C*
062700970526     C                   ENDSR
062800970526     C*------------------------------------------------------------------------*
062900970526     C* LANPGM - LANCIA UN PROGRAMMA
063000970526     C*------------------------------------------------------------------------*
063100060314     C     LANPGM0       BEGSR
063200970526     C*
063300970526     C* IMPOSTA LA DS DI PROCEDURA
063400060314     c                   clear                   tisi00ds
063500060314     c                   movel     ddev          d00dri
063600060314     C                   MOVEL     WOP0          d00OP0                         *LIVELLO PROCEDURA
063700060314     C                   MOVEL     WOP1          d00OP1                         *LIVELLO PGM
063800060314     C                   MOVEL     '0'           d00F03                         *PREMUTO F03=FINE
063900060314     C                   MOVEL     '0'           d00F12                         *PREMUTO F12=RITORNO
064000060314     C                   MOVEL     '0'           d00ERR                         *'1'=ERRORE
064100060314     C                   MOVEL     *BLANKS       d00MSG                         *MESSAGGIO DI RITORN
064200060314     C                   MOVEL     DNAR          d00NAR                         *NAZIONE
064300060314     C                   MOVEL     DCAP          d00CAP                         *CAP
064400060314     C                   MOVEL     DLOC          d00LOC                         *LOCALITA'
064500970526     C*
064600970526     C                   CALL      WPGM
064700970612     C                   PARM                    KPJBA
064800060314     C                   PARM                    tisi00ds
064900970526     C*
065000970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
065100060314IF  1C     d00F03        IFEQ      '1'                                          *PREMUTO F03
065200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
065300970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
065400970526X   1C                   ELSE                                                   *NON PREMUTO F03
065500060314     C                   MOVEL     d00MSG        DSMSMS                         *MESSAGGIO DI RITORN
065600970526E   1C                   ENDIF
065700970526     C*
065800970526     C                   ENDSR
065900060314     C*------------------------------------------------------------------------*
066000060314     C* LANPGM - LANCIA UN PROGRAMMA
066100060314     C*------------------------------------------------------------------------*
066200060314     C     LANPGMp       BEGSR
066300060314     C*
066400060314     C* IMPOSTA LA DS DI PROCEDURA
066500060314     c                   clear                   tisip0ds
066600060314     C                   MOVEL     WOP0          dp0OP0                         *LIVELLO PROCEDURA
066700060314     C                   MOVEL     WOP1          dp0OP1                         *LIVELLO PGM
066800060314     C                   MOVEL     '0'           dp0F03                         *PREMUTO F03=FINE
066900060314     C                   MOVEL     '0'           dp0F12                         *PREMUTO F12=RITORNO
067000060314     C                   MOVEL     '0'           dp0ERR                         *'1'=ERRORE
067100060314     C                   MOVEL     *BLANKS       dp0MSG                         *MESSAGGIO DI RITORN
067200060314     C                   MOVEL     DNAR          dp0NAR                         *NAZIONE
067300060314     C                   MOVEL     DCAP          dp0CAP                         *CAP
067400060327     C                   MOVEL     Dprv          dp0prv                         *LOCALITA'
067500060404     C                   MOVEL     Ddup          dp0dup                         *LOCALITA'
067600060327     C                   MOVEL     DLOC          dp0LOC                         *LOCALITA'
067700060327     C                   MOVEL     Dfil          dp0fil                         *LOCALITA'
067800060315     C                   MOVEL     Ddev          dp0dri                         *LOCALITA'
067900060315     C                   MOVEL     Dcev          dp0cev                         *LOCALITA'
068000060331     C                   MOVEL     Dtaf          dp0taf                         *LOCALITA'
068100060314     C*
068200060314     C                   CALL      WPGM
068300060314     C                   PARM                    KPJBA
068400060314     C                   PARM                    tisip0ds
068500060314     C*
068600060314     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
068700060314IF  1C     dp0F03        IFEQ      '1'                                          *PREMUTO F03
068800060314     C                   MOVEL     '1'           ERRL00                         *ERRORE
068900060314     C                   MOVEL     '1'           F03L00                         *FINE PGM
069000060314X   1C                   ELSE                                                   *NON PREMUTO F03
069100060314     C                   MOVEL     dp0MSG        DSMSMS                         *MESSAGGIO DI RITORN
069200060314E   1C                   ENDIF
069300060314     C*
069400060314     C                   ENDSR
069500970527     C*------------------------------------------------------------------------*
069600970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
069700970527     C*------------------------------------------------------------------------*
069800970527     C     CARTAB        BEGSR
069900970527     C*
070000970527     C                   ENDSR
070100970526     C*------------------------------------------------------------------------*
070200970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
070300970526     C*------------------------------------------------------------------------*
070400970526     C     CHKFOP        BEGSR
070500970626     C*
070600970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
070700060404IF  1C     opzione       IFEQ      '01'
070800970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
070900970626     C                   MOVEL     'S'           OPDATT
071000970626     C                   MOVEL     OPDRIG        OPD(1)
071100970626X   1C                   ELSE
071200970626     C* OPZIONI
071300970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
071400970626     C                   MOVEL     'S'           OPDATT
071500970626     C                   MOVEL     OPDRIG        OPD(5)
071600970626     C                   MOVEL     OPD(13)       OPDRIG                         *SINONIMI
071700970626     C                   MOVEL     'S'           OPDATT
071800970626     C                   MOVEL     OPDRIG        OPD(13)
071900970626     C* FUNZIONI
072000970626     C                   MOVEL     FUD(13)       FUDRIG                         *SINONIMI
072100970626     C                   MOVEL     'S'           FUDATT
072200970626     C                   MOVEL     FUDRIG        FUD(13)
072300970626E   1C                   ENDIF
072400970626     C*
072500970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
072600060404IF  1C     opzione       IFEQ      '02'
072700970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
072800970626     C                   MOVEL     'S'           OPDATT
072900970626     C                   MOVEL     OPDRIG        OPD(2)
073000970627     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
073100970627     C                   MOVEL     'S'           OPDATT
073200970627     C                   MOVEL     OPDRIG        OPD(3)
073300970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
073400970626     C                   MOVEL     'S'           OPDATT
073500970626     C                   MOVEL     OPDRIG        OPD(4)
073600970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
073700970626     C                   MOVEL     'S'           FUDATT
073800970626     C                   MOVEL     FUDRIG        FUD(6)
073900060327    1C                   ENDIF
074000060331     c
074100060404IF  1C     opzione       IFEQ      '05'
074200060331     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
074300060331     C                   MOVEL     'N'           OPDATT
074400060331     C                   MOVEL     OPDRIG        OPD(2)
074500060331     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
074600060331     C                   MOVEL     'N'           OPDATT
074700060331     C                   MOVEL     OPDRIG        OPD(3)
074800060331     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
074900060331     C                   MOVEL     'N'           OPDATT
075000060331     C                   MOVEL     OPDRIG        OPD(4)
075100060331     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
075200060331     C                   MOVEL     'N'           FUDATT
075300060331     C                   MOVEL     FUDRIG        FUD(6)
075400060331    1C                   ENDIF
075500970526     C*
075600970526     C* GESTIONE OPZIONI A VIDEO
075700970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
075800970526     C                   EXSR      OPZGES
075900970526     C*
076000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
076100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
076200970526     C                   EXSR      FUNGES
076300970526     C*
076400970526     C                   ENDSR
076500020919     C*--------------------------------------------------------------------------------------------*
076600020919     C* REPERISCE I DATI UTENTE
076700020919     C*--------------------------------------------------------------------------------------------*
076800020919     C     REPDATIUTE    BEGSR
076900020919     C*
077000020919     C* INIZIALIZZA VARIABILI DI WRK
077100020919     C                   CLEAR                   TIBS34DS
077200020919     C                   CLEAR                   AZUTEDS
077300020919     C                   CLEAR                   DDATIUTE
077400020919     C*
077500020919     C     *DTAARA       DEFINE    §azute        azuteds
077600020919     C     *DTAARA       DEFINE    §datiute      ddatiute
077700020919     C                   IN(E)     *DTAARA
077800020919if  1C                   IF        %Error
077900020919     c                   EVAL      I34Tla = 'L'
078000020919     C                   CALL      'TIBS34R'
078100020919     C                   PARM                    Tibs34Ds
078200020919     C                   IN        *DTAARA
078300020919e   1C                   ENDIF
078400020919     C*
078500020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
078600020919     C                   MOVEL(P)  RSUT          DSFIRS           20
078700020919     C*
078800020919     C                   ENDSR
078900970526     C*------------------------------------------------------------------------*
079000970526     C* *INZSR - OPERAZIONI INIZIALI
079100970526     C*------------------------------------------------------------------------*
079200970526     C     *INZSR        BEGSR
079300970526     C*---
079400970526     C* RICEVIMENTO PARAMETRI
079500970526     C*---
079600970526     C     *ENTRY        PLIST
079700970612     C                   PARM                    KPJBA
079800060313     C                   PARM                    tisip0ds
079900970526     C*
080000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
080100970526     C     XMSGPA        PLIST
080200970526     C                   PARM                    DSMSMS
080300970526     C                   PARM                    DSMSPG
080400970526     C                   PARM                    DSMSKE
080500970526     C                   PARM                    DSMSER
080600970526     C*
080700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
080800970526     C                   CALL      'XKEY'
080900970526     C                   PARM                    DSKEY
081000970526     C*---
081100970526     C* VARIABILI NON RIFERITE AL DATA BASE
081200970526     C*---
081300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
081400970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
081500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
081600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
081700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
081800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
081900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
082000970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
082100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
082200060313     C     *LIKE         DEFINE    Dp0OP0        DEPOP0                         *DI DEPOSITO
082300060313     C     *LIKE         DEFINE    Dp0OP0        WOP0                           *DI LAVORO
082400060313     C     *LIKE         DEFINE    Dp0OP1        WOP1
082500060313     C     *LIKE         DEFINE    Dp0NAR        DNAR
082600060313     C     *LIKE         DEFINE    Dp0CAP        DCAP
082700060313     C     *LIKE         DEFINE    Dp0LOC        DLOC
082800970526     C*---
082900970526     C* VARIABILI DI CONTROLLO
083000970526     C*---
083100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
083200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
083300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
083400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
083500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
083600970530     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
083700970526     C*
083800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
083900060313     C                   MOVEL     'TISIP0R'     DSMSPG
084000060313     C                   MOVEL     'TISIP0R'     MSGQUE
084100970526     C                   MOVEL     *BLANKS       MSGKEY
084200970526     C                   MOVEL     *BLANKS       DSMSMS
084300970526     C                   MOVEL     *BLANKS       DSMSKE
084400970526     C                   Z-ADD     *ZEROS        DSMSER
084500060330     c
084600060313     C     *LIKE         DEFINE    Cepdev        DEPpde                         *DEPOSITI
084700060313     C     *LIKE         DEFINE    CepNAR        DEPNAR                         *DEPOSITI
084800060313     C     *LIKE         DEFINE    CepCAP        DEPCAP
084900060313     C     *LIKE         DEFINE    Cepfil        DEPppo
085000970526     C*---
085100970526     C* CHIAVI DI LETTURA
085200970526     C*---
085300060329     C* LETTURA AZcep01L
085400060330     C     kcep11        KLIST
085500060313     C                   KFLD                    Kfil                           *CAP
085600060313     C                   KFLD                    Knar                           *CAP
085700060313     C                   KFLD                    Kcap                           *CAP
085800060329     C* LETTURA AZCep01L
085900060330     C     Kcep12        KLIST
086000060313     C                   KFLD                    kfil                           *NAZIONE
086100060330     C* LETTURA AZCep02L
086200060330     C     Kcep21        KLIST
086300060330     C                   KFLD                    kdev                           *NAZIONE
086400060330     C                   KFLD                    kfil                           *NAZIONE
086500060330     C                   KFLD                    knar                           *NAZIONE
086600060330     C                   KFLD                    kcap                           *NAZIONE
086700060329     c     Ktab          Klist
086800060329     c                   Kfld                    codut             1 0
086900060329     c                   Kfld                    kCod
087000060329     c                   Kfld                    kKey
087100060329     c                   z-add     1             codut
087200970527     C* REPERIMENTO DATI UTENTE
087300020919     C                   EXSR      REPDATIUTE
087400970527     C*
087500970527     C* CARICA LE TABELLE OCCORRENTI
087600970527     C                   EXSR      CARTAB
087700970526     C*
087800970526     C* DEPOSITA LE OPZIONI DI ENTRATA
087900060313     C                   MOVEL     dp0OP0        DEPOP0                         *LIVELLO PROCEDURA
088000970526     C*
088100970526     C* CALCOLA LA DATA CORRENTE
088200970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
088300970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
088400970526     C                   Z-ADD     WN8           G08DAT
088500970526     C                   Z-ADD     *ZEROS        G08INV
088600970526     C                   MOVEL     '0'           G08ERR
088700970526     C                   CALL      'XSRDA8'
088800970526     C                   PARM                    WLBDA8
088900970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
089000060313     c
089100060313     c* Se non passata la data di ingresso, uso data corrente
089200060313     c                   if        dP0dri=0
089300060313     c                   eval      kdev=datcor
089400060313     c                   eval      vidpde=wn8
089500060313     c                   else
089600060313     c                   eval      kdev=dp0dri
089700060313     c* La imposto a video
089800060313     C                   Z-ADD     Dp0DRI        G08INV
089900060313     C                   Z-ADD     *ZEROS        G08DAT
090000060313     C                   MOVEL     '3'           G08ERR
090100060313     C                   CALL      'XSRDA8'
090200060313     C                   PARM                    WLBDA8
090300060313     C                   Z-ADD     G08DAT        VIDpde                         *DATA RIFERIMENTO
090400060313     c                   endif
090500970526     C*
090600970526     C* IMPOSTA I PARAMETRI DI INGRESSO
090700060313     C                   MOVEL     Dp0NAR        VIDPNA                         *NAZIONE
090800060313     C                   MOVEL     Dp0CAP        VIDPCA                         *CAP
090900060313     C                   MOVEL     Dp0fil        VIDPpo                         *CAP
091000060403     c                   eval      vidord='D'
091100060403     c                   if        dp0nar<>*blanks or dp0cap<>*blanks or
091200060403     c                             dp0fil>0
091300060403     c                   eval      vidord='C'
091400060403     c                   endif
091500060313     c
091600970526     C                   EXSR      CHKPAR
091700970526IF  1C     ERRL00        IFEQ      '1'
091800060313     C                   MOVEL     '1'           Dp0ERR
091900060313     C                   MOVEL     DSMSMS        Dp0MSG
092000970526X   1C                   ELSE
092100970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
092200970527     C                   MOVEL     VIDPCA        DEPCAP
092300060313     C                   MOVEL     VIDppo        DEPppo
092400060313     C                   MOVEL     VIDpde        DEPpde
092500060331     C                   MOVEL     VIDord        DEPord
092600970526E   1C                   ENDIF
092700970526     C*
092800970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
092900060404     c                   eval      opzione=%subst(depop0:2:2)
093000970526     C                   EXSR      CHKFOP
093100970526     C*
093200970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
093300060404IF  1C     opzione       IFEQ      '01'
093400970530     C                   MOVEL     DEC(3)        VIDING
093500970530E   1C                   ENDIF
093600060404IF  1C     opzione       IFEQ      '02'
093700970530     C                   MOVEL     DEC(1)        VIDING
093800970530E   1C                   ENDIF
093900060404IF  1C     opzione       IFEQ      '05'
094000970530     C                   MOVEL     DEC(2)        VIDING
094100970530E   1C                   ENDIF
094200970526     C*
094300970526     C                   ENDSR
094400970526     C*------------------------------------------------------------------------*
094500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
094600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
094700970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
094800970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
094900970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
095000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
095100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
095200970526     C*------------------------------------------------------------------------*
095300970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
095400970526     C*------------------------------------------------------------------------*
095500970526     C     POSSFL        BEGSR
095600970526     C*
095700970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
095800970526     C                   EXSR      CLRSFL
095900970526     C*
096000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
096100970526     C                   EXSR      SETFIL
096200970526     C*
096300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
096400970526     C     SFLEOF        IFEQ      0
096500970526     C                   EXSR      CARSFL
096600970526     C                   END
096700970526     C*
096800970526     C                   ENDSR
096900970526     C*------------------------------------------------------------------------*
097000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
097100970526     C*------------------------------------------------------------------------*
097200970526     C     ENTATT        BEGSR
097300970526     C*
097400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
097500970526     C                   Z-ADD     0             OPZOK             1 0
097600970526     C*
097700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
097800970526     C                   Z-ADD     0             SFLVAR            5 0
097900970526     C*
098000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
098100970526     C                   Z-ADD     0             SFLERR
098200970526     C*
098300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
098400970526     C                   Z-ADD     SFLULT        WFLULT
098500970526     C*
098600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
098700060313     C                   READC     SIP0D05                                98
098800970526     C     *IN98         DOWEQ     '0'
098900970526     C*
099000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
099100970526     C                   ADD       1             SFLVAR
099200970526     C*
099300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
099400970526     C     OPZOK         IFEQ      0
099500970526     C*
099600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
099700970526     C     OPZKEY        IFNE      *BLANKS
099800970526     C                   EXSR      OPZCON
099900970526     C                   END
100000970526     C*
100100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
100200970526     C*            2. PULIZIA SCELTA IN SFL
100300970526     C     OPZOK         IFEQ      0
100400970526     C                   MOVEL     '0'           *IN41
100500970526     C                   MOVEL     *BLANKS       OPZKEY
100600970526     C                   ELSE
100700970526     C*
100800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
100900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
101000970526     C                   MOVEL     '1'           *IN41
101100970526     C                   Z-ADD     SFLULT        SFLERR
101200970526     C                   END
101300970526     C                   ELSE
101400970526     C*
101500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
101600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
101700970526     C                   MOVEL     '1'           *IN41
101800970526     C                   END                                                    OPZOK=0
101900970526     C*
102000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
102100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
102200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
102300970526     C     OPZOK         IFNE      0
102400970526     C                   Z-ADD     SFLERR        SFLNBR
102500970526     C                   ELSE
102600970526     C                   Z-ADD     SFLULT        SFLNBR
102700970526     C                   END
102800970526     C*
102900970526     C* AGGIORNO RECORD SFL
103000060404     c                   movel     in03          *in03
103100060313     C                   UPDATE    SIP0D05
103200970526     C*
103300060313     C                   READC     SIP0D05                                98
103400970526     C                   END                                                    DO WHILE
103500970526     C*
103600970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
103700970526     C                   Z-ADD     WFLULT        SFLULT
103800970526     C*
103900970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
104000970526     C     SFLVAR        IFEQ      0
104100970526     C*---                 MOVEL'1'       FINL00
104200970526     C                   END
104300970526     C*
104400970526     C                   ENDSR
104500970526     C*------------------------------------------------------------------------*
104600970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
104700970526     C*------------------------------------------------------------------------*
104800970526     C     CLRSFL        BEGSR
104900970526     C*
105000970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
105100970526     C* (0=NON FINE FILE  1=FINE FILE)
105200970526     C                   Z-ADD     0             SFLEOF
105300970526     C*
105400970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
105500970526     C                   Z-ADD     0             SFLULT
105600970526     C*
105700970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
105800970526     C                   MOVEL     '1'           *IN40
105900060313     C                   WRITE     SIP0D06
106000970526     C                   MOVEL     '0'           *IN40
106100970526     C*
106200970526     C                   ENDSR
106300970526     C*------------------------------------------------------------------------*
106400970526     C* CARSFL - CARICAMENTO SFL
106500970526     C*------------------------------------------------------------------------*
106600970526     C     CARSFL        BEGSR
106700970526     C*
106800970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
106900970526     C     SFLULT        IFEQ      0
107000970526     C                   EXSR      SETFIL
107100970526     C                   END
107200970526     C*
107300970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
107400970526     C                   Z-ADD     SFLULT        WFLULT
107500970526     C*
107600970526     C* CICLO:
107700970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
107800970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
107900970526     C                   Z-ADD     1             SFLRIG
108000970526     C     SFLRIG        DOWLE     SFLPAG
108100970526     C     SFLEOF        ANDEQ     0
108200970526     C*
108300970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
108400970526     C                   MOVEL     *BLANKS       OPZKEY
108500970526     C*
108600970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
108700970526     C                   EXSR      IRESFL
108800970526     C*
108900970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
109000970526     C                   ADD       1             SFLULT
109100970526     C*
109200970526     C* SCRITTURA RECORD SFL
109300060313     C                   WRITE     SIP0D05
109400970526     C                   ADD       1             SFLRIG
109500970526     C*
109600970526     C* LETTURA FILE DA CARICARE
109700970526     C                   EXSR      LETFIL
109800970526     C                   END                                                    DO WHILE
109900970526     C*
110000970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
110100970526     C     WFLULT        IFNE      SFLULT
110200970526     C                   Z-ADD     WFLULT        SFLNBR
110300970526     C                   ADD       1             SFLNBR
110400970526     C                   END
110500970526     C*
110600970526     C                   ENDSR
110700970526     C*------------------------------------------------------------------------*
110800970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
110900970526     C*------------------------------------------------------------------------*
111000970526     C     OUTSFL        BEGSR
111100970526     C*
111200970526     C* WRITE RECORD TESTATA VIDEO
111300060313     C                   WRITE     SIP0D01
111400970526     C*
111500970526     C* VISUALIZZAZIONE MESSAGGIO
111600970526     C                   EXSR      SNDMSG
111700970526     C*
111800970526     C* WRITE RECORD OPZIONI VIDEO
111900060313     C                   WRITE     SIP0D02
112000970526     C*
112100970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
112200060313     C                   WRITE     SIP0D03
112300970526     C*
112400970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
112500060313     C                   WRITE     SIP0D04
112600970526     C*
112700970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
112800970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
112900970526     C     SFLEOF        IFEQ      0
113000970526     C                   MOVEL     '0'           *IN42
113100970526     C                   ELSE
113200970526     C*
113300970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
113400970526     C                   MOVEL     '1'           *IN42
113500970526     C                   END
113600970526     C*
113700970526     C* WRITE RECORD TASTI FUNZIONALI
113800060313     C                   WRITE     SIP0D09
113900970526     C*
114000970526     C* CASO DI SFL VUOTO
114100970526     C     SFLULT        IFLE      0
114200970526     C*
114300970526     C*    - WRITE RECORD VUOTO
114400060313     C                   WRITE     SIP0D0A
114500060313     C                   READ      SIP0D0A                                99
114600970526     C*
114700970526     C                   ELSE
114800970526     C*
114900970526     C* CASO DI SFL CON RECORDS
115000970526     C*
115100970526     C*    - WRITE RECORD CONTROLLO SFL
115200060313     C                   WRITE     SIP0D06
115300060313     C                   READ      SIP0D06                                99
115400970526     C                   END
115500970526     C*
115600970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
115700060313     C                   READ      SIP0D03                                99
115800970526     C*
115900970526     C                   ENDSR
116000970526     C*------------------------------------------------------------------------*
116100970526     C* SNDMSG - MANDA MESSAGGIO
116200970526     C*------------------------------------------------------------------------*
116300970526     C     SNDMSG        BEGSR
116400970526     C*
116500970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
116600970526     C                   MOVEL     '1'           *IN40
116700060313     C                   WRITE     SIP0DMC
116800970526     C                   MOVEL     '0'           *IN40
116900970526     C*
117000970526     C* VISUALIZZAZIONE MESSAGGIO
117100970526     C                   Z-ADD     0             MSGULT
117200970526     C                   CALL      'XMSG'        XMSGPA
117300970526     C     DSMSER        IFEQ      0
117400970526     C                   MOVEL     DSMSKE        MSGKEY
117500970526     C                   END
117600970526     C                   Z-ADD     1             MSGULT
117700060313     C                   WRITE     SIP0DMS                                      SFL MESS
117800060313     C                   WRITE     SIP0DMC                                      CONTR MESS
117900970526     C*
118000970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
118100970526     C                   MOVEL     *BLANKS       DSMSMS
118200970526     C*
118300970526     C                   ENDSR
118400970526     C*------------------------------------------------------------------------*
118500970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
118600970526     C*------------------------------------------------------------------------*
118700970526     C     FUNCON        BEGSR
118800970526     C*
118900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
119000970526     C                   EXSR      FUNVAR
119100970526     C*
119200970526     C* PULISCO CAMPI DI LAVORO
119300970526     C                   Z-ADD     0             I                 5 0
119400970526     C                   Z-ADD     0             W                 5 0
119500970526     C*
119600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
119700970526     C     FUNKEY        IFNE      *BLANKS
119800970526     C     1             DO        24            I
119900970526     C                   MOVEL     FUD(I)        FUDRIG
120000970526     C*
120100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
120200970526     C     FUNKEY        IFEQ      FUDFUN
120300970526     C     FUDATT        ANDEQ     'S'
120400970526     C                   Z-ADD     I             W
120500970526     C                   END
120600970526     C                   END                                                    DO WHILE
120700970526     C*
120800970526     C* FUNZIONE NON ATTIVA (ERRORE)
120900970526     C     W             IFEQ      0
121000970526     C*
121100970526     C* EMISSIONE MESSAGGIO DI ERRORE
121200970526     C                   MOVEL     MSG(2)        DSMSMS
121300970526     C                   END                                                    W=0
121400970526     C                   END                                                    FUNKEY NE *BLANKS
121500970526     C*
121600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
121700970526     C     W             IFNE      0
121800970526     C     FUNKEY        OREQ      *BLANKS
121900970526     C*
122000970526     C     FUNKEY        IFEQ      *BLANKS
122100970526     C                   EXSR      ENTGES
122200970526     C                   END
122300970526     C     FUNKEY        IFEQ      '03'
122400970526     C                   EXSR      F03GES
122500970526     C                   END
122600970526     C     FUNKEY        IFEQ      '05'
122700970526     C                   EXSR      F05GES
122800970526     C                   END
122900970526     C     FUNKEY        IFEQ      '06'
123000970526     C                   EXSR      F06GES
123100970526     C                   END
123200970526     C     FUNKEY        IFEQ      '12'
123300970526     C                   EXSR      F12GES
123400970526     C                   END
123500970529     C     FUNKEY        IFEQ      '13'
123600970529     C                   EXSR      F13GES
123700970529     C                   END
123800970526     C     FUNKEY        IFEQ      '23'
123900970526     C                   EXSR      F23GES
124000970526     C                   END
124100970526     C     FUNKEY        IFEQ      '24'
124200970526     C                   EXSR      F24GES
124300970526     C                   END
124400970526     C     FUNKEY        IFEQ      'RU'
124500970526     C                   EXSR      RUPGES
124600970526     C                   END
124700970526     C*
124800970526     C                   END
124900970526     C*
125000970526     C                   ENDSR
125100970526     C*------------------------------------------------------------------------*
125200970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
125300970526     C*------------------------------------------------------------------------*
125400970526     C     FUNVAR        BEGSR
125500970526     C*
125600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
125700970526     C     KEY           IFEQ      ENTER
125800970526     C                   MOVEL     *BLANKS       FUNKEY
125900970526     C                   END
126000970526     C     KEY           IFEQ      F1
126100970526     C                   MOVEL     '01'          FUNKEY
126200970526     C                   END
126300970526     C     KEY           IFEQ      F2
126400970526     C                   MOVEL     '02'          FUNKEY
126500970526     C                   END
126600970526     C     KEY           IFEQ      F3
126700970526     C                   MOVEL     '03'          FUNKEY
126800970526     C                   END
126900970526     C     KEY           IFEQ      F4
127000970526     C                   MOVEL     '04'          FUNKEY
127100970526     C                   END
127200970526     C     KEY           IFEQ      F5
127300970526     C                   MOVEL     '05'          FUNKEY
127400970526     C                   END
127500970526     C     KEY           IFEQ      F6
127600970526     C                   MOVEL     '06'          FUNKEY
127700970526     C                   END
127800970526     C     KEY           IFEQ      F7
127900970526     C                   MOVEL     '07'          FUNKEY
128000970526     C                   END
128100970526     C     KEY           IFEQ      F8
128200970526     C                   MOVEL     '08'          FUNKEY
128300970526     C                   END
128400970526     C     KEY           IFEQ      F9
128500970526     C                   MOVEL     '09'          FUNKEY
128600970526     C                   END
128700970526     C     KEY           IFEQ      F10
128800970526     C                   MOVEL     '10'          FUNKEY
128900970526     C                   END
129000970526     C     KEY           IFEQ      F11
129100970526     C                   MOVEL     '11'          FUNKEY
129200970526     C                   END
129300970526     C     KEY           IFEQ      F12
129400970526     C                   MOVEL     '12'          FUNKEY
129500970526     C                   END
129600970526     C     KEY           IFEQ      F13
129700970526     C                   MOVEL     '13'          FUNKEY
129800970526     C                   END
129900970526     C     KEY           IFEQ      F14
130000970526     C                   MOVEL     '14'          FUNKEY
130100970526     C                   END
130200970526     C     KEY           IFEQ      F15
130300970526     C                   MOVEL     '15'          FUNKEY
130400970526     C                   END
130500970526     C     KEY           IFEQ      F16
130600970526     C                   MOVEL     '16'          FUNKEY
130700970526     C                   END
130800970526     C     KEY           IFEQ      F17
130900970526     C                   MOVEL     '17'          FUNKEY
131000970526     C                   END
131100970526     C     KEY           IFEQ      F18
131200970526     C                   MOVEL     '18'          FUNKEY
131300970526     C                   END
131400970526     C     KEY           IFEQ      F19
131500970526     C                   MOVEL     '19'          FUNKEY
131600970526     C                   END
131700970526     C     KEY           IFEQ      F20
131800970526     C                   MOVEL     '20'          FUNKEY
131900970526     C                   END
132000970526     C     KEY           IFEQ      F21
132100970526     C                   MOVEL     '21'          FUNKEY
132200970526     C                   END
132300970526     C     KEY           IFEQ      F22
132400970526     C                   MOVEL     '22'          FUNKEY
132500970526     C                   END
132600970526     C     KEY           IFEQ      F23
132700970526     C                   MOVEL     '23'          FUNKEY
132800970526     C                   END
132900970526     C     KEY           IFEQ      F24
133000970526     C                   MOVEL     '24'          FUNKEY
133100970526     C                   END
133200970526     C     KEY           IFEQ      ROLLUP
133300970526     C                   MOVEL     'RU'          FUNKEY
133400970526     C                   END
133500970526     C     KEY           IFEQ      HELP
133600970526     C                   MOVEL     'HP'          FUNKEY
133700970526     C                   END
133800970526     C*
133900970526     C                   ENDSR
134000970526     C*------------------------------------------------------------------------*
134100970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
134200970526     C*------------------------------------------------------------------------*
134300970526     C     FUNGES        BEGSR
134400970526     C*
134500970526     C* PULISCO CAMPI DI LAVORO
134600970526     C                   Z-ADD     0             W                 5 0
134700970526     C*
134800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
134900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
135000970526     C     1             DO        24            I
135100970526     C                   MOVEL     FUD(I)        FUDRIG
135200970526     C*
135300970526     C* PULIZIA FUNZIONI PRECEDENTI
135400970526     C                   MOVEL     *BLANKS       FUV(I)
135500970526     C*
135600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
135700970526     C* (SALTA F23 E F24)
135800970526     C     FUDATT        IFEQ      'S'
135900970526     C     FUDFUN        ANDNE     '23'
136000970526     C     FUDFUN        ANDNE     '24'
136100970526     C                   ADD       1             W
136200970526     C                   MOVEL     FUDDES        FUV(W)
136300970526     C                   END
136400970526     C                   END                                                    DO WHILE
136500970526     C*
136600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
136700970526     C* ALTRIMENTI DISATTIVO F24
136800970526     C     1             DO        24            I
136900970526     C                   MOVEL     FUD(I)        FUDRIG
137000970526     C*
137100970526     C     FUDFUN        IFEQ      '24'
137200970526     C     W             IFGT      6
137300970526     C                   MOVEL     'S'           FUDATT
137400970526     C                   ELSE
137500970526     C                   MOVEL     *BLANKS       FUDATT
137600970526     C                   END                                                    W GT 8
137700970526     C                   MOVEL     FUDRIG        FUD(I)
137800970526     C                   END
137900970526     C                   END                                                    DO WHILE
138000970526     C*
138100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
138200970526     C     W             DIV       6             I
138300970526     C                   MVR                     Z
138400970526     C*
138500970526     C     Z             IFNE      0
138600970526     C     I             ADD       1             Z
138700970526     C                   ELSE
138800970526     C                   Z-ADD     I             Z
138900970526     C                   END
139000970526     C*
139100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
139200970526     C                   ADD       1             FUNGRU
139300970526     C     FUNGRU        IFGT      Z
139400970526     C     FUNGRU        ORLT      1
139500970526     C                   Z-ADD     1             FUNGRU
139600970526     C                   END
139700970526     C*
139800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
139900970526     C     FUNGRU        IFEQ      1
140000970526     C                   MOVEL     FUV(1)        FUN001
140100970526     C                   MOVEL     FUV(2)        FUN002
140200970526     C                   MOVEL     FUV(3)        FUN003
140300970526     C                   MOVEL     FUV(4)        FUN004
140400970526     C                   MOVEL     FUV(5)        FUN005
140500970526     C                   MOVEL     FUV(6)        FUN006
140600970526     C                   END
140700970526     C*
140800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
140900970526     C     FUNGRU        IFEQ      2
141000970526     C                   MOVEL     FUV(7)        FUN001
141100970526     C                   MOVEL     FUV(8)        FUN002
141200970526     C                   MOVEL     FUV(9)        FUN003
141300970526     C                   MOVEL     FUV(10)       FUN004
141400970526     C                   MOVEL     FUV(11)       FUN005
141500970526     C                   MOVEL     FUV(12)       FUN006
141600970526     C                   END
141700970526     C*
141800970526     C* TERZO GRUPPO DI 6 FUNZIONI
141900970526     C     FUNGRU        IFEQ      3
142000970526     C                   MOVEL     FUV(13)       FUN001
142100970526     C                   MOVEL     FUV(14)       FUN002
142200970526     C                   MOVEL     FUV(15)       FUN003
142300970526     C                   MOVEL     FUV(16)       FUN004
142400970526     C                   MOVEL     FUV(17)       FUN005
142500970526     C                   MOVEL     FUV(18)       FUN006
142600970526     C                   END
142700970526     C*
142800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
142900970526     C     FUNGRU        IFEQ      4
143000970526     C                   MOVEL     FUV(19)       FUN001
143100970526     C                   MOVEL     FUV(20)       FUN002
143200970526     C                   MOVEL     FUV(21)       FUN003
143300970526     C                   MOVEL     FUV(22)       FUN004
143400970526     C                   MOVEL     *BLANKS       FUN005
143500970526     C                   MOVEL     *BLANKS       FUN006
143600970526     C                   END
143700970526     C*
143800970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
143900970526     C                   MOVEL     *BLANKS       FUN007
144000970526     C                   MOVEL     *BLANKS       FUN008
144100970526     C*
144200970526     C     1             DO        24            I
144300970526     C                   MOVEL     FUD(I)        FUDRIG
144400970526     C     FUDFUN        IFEQ      '23'
144500970526     C     FUDATT        ANDEQ     'S'
144600970526     C                   MOVEL     FUDDES        FUN007
144700970526     C                   END
144800970526     C     FUDFUN        IFEQ      '24'
144900970526     C     FUDATT        ANDEQ     'S'
145000970526     C                   MOVEL     FUDDES        FUN008
145100970526     C                   END
145200970526     C                   END                                                    DO
145300970526     C*
145400970526     C                   ENDSR
145500970526     C*------------------------------------------------------------------------*
145600970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
145700970526     C*------------------------------------------------------------------------*
145800970526     C     OPZGES        BEGSR
145900970526     C*
146000970526     C* PULISCO CAMPI DI LAVORO
146100970526     C                   Z-ADD     0             W                 5 0
146200970526     C*
146300970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
146400970526     C* E CARICO TABELLA OPZIONI ATTIVE
146500970526     C     1             DO        24            I
146600970526     C                   MOVEL     OPD(I)        OPDRIG
146700970526     C*
146800970526     C* PULIZIA OPZIONI PRECEDENTI
146900970526     C                   MOVEL     *BLANKS       OPV(I)
147000970526     C*
147100970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
147200970526     C     OPDATT        IFEQ      'S'
147300970526     C                   ADD       1             W
147400970526     C                   MOVEL     OPDDES        OPV(W)
147500970526     C                   END
147600970526     C                   END                                                    DO WHILE
147700970526     C*
147800970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
147900970526     C* ALTRIMENTI DISATTIVO F23
148000970526     C     1             DO        24            I
148100970526     C                   MOVEL     FUD(I)        FUDRIG
148200970526     C*
148300970526     C     FUDFUN        IFEQ      '23'
148400970526     C     W             IFGT      8
148500970526     C                   MOVEL     'S'           FUDATT
148600970526     C                   ELSE
148700970526     C                   MOVEL     *BLANKS       FUDATT
148800970526     C                   END                                                    W GT 8
148900970526     C                   MOVEL     FUDRIG        FUD(I)
149000970526     C                   END
149100970526     C                   END                                                    DO WHILE
149200970526     C*
149300970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
149400970526     C     W             DIV       8             I
149500970526     C                   MVR                     Z                 5 0
149600970526     C*
149700970526     C     Z             IFNE      0
149800970526     C     I             ADD       1             Z
149900970526     C                   ELSE
150000970526     C                   Z-ADD     I             Z
150100970526     C                   END
150200970526     C*
150300970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
150400970526     C                   ADD       1             OPZGRU
150500970526     C     OPZGRU        IFGT      Z
150600970526     C     OPZGRU        ORLT      1
150700970526     C                   Z-ADD     1             OPZGRU
150800970526     C                   END
150900970526     C*
151000970526     C* PRIMO GRUPPO DI 8 OPZIONI
151100970526     C     OPZGRU        IFEQ      1
151200970526     C                   MOVEL     OPV(1)        OPZ001
151300970526     C                   MOVEL     OPV(2)        OPZ002
151400970526     C                   MOVEL     OPV(3)        OPZ003
151500970526     C                   MOVEL     OPV(4)        OPZ004
151600970526     C                   MOVEL     OPV(5)        OPZ005
151700970526     C                   MOVEL     OPV(6)        OPZ006
151800970526     C                   MOVEL     OPV(7)        OPZ007
151900970526     C                   MOVEL     OPV(8)        OPZ008
152000970526     C                   END
152100970526     C*
152200970526     C* SECONDO GRUPPO DI 8 OPZIONI
152300970526     C     OPZGRU        IFEQ      2
152400970526     C                   MOVEL     OPV(9)        OPZ001
152500970526     C                   MOVEL     OPV(10)       OPZ002
152600970526     C                   MOVEL     OPV(11)       OPZ003
152700970526     C                   MOVEL     OPV(12)       OPZ004
152800970526     C                   MOVEL     OPV(13)       OPZ005
152900970526     C                   MOVEL     OPV(14)       OPZ006
153000970526     C                   MOVEL     OPV(15)       OPZ007
153100970526     C                   MOVEL     OPV(16)       OPZ008
153200970526     C                   END
153300970526     C*
153400970526     C* TERZO GRUPPO DI 8 OPZIONI
153500970526     C     OPZGRU        IFEQ      3
153600970526     C                   MOVEL     OPV(17)       OPZ001
153700970526     C                   MOVEL     OPV(18)       OPZ002
153800970526     C                   MOVEL     OPV(19)       OPZ003
153900970526     C                   MOVEL     OPV(20)       OPZ004
154000970526     C                   MOVEL     OPV(21)       OPZ005
154100970526     C                   MOVEL     OPV(22)       OPZ006
154200970526     C                   MOVEL     OPV(23)       OPZ007
154300970526     C                   MOVEL     OPV(24)       OPZ008
154400970526     C                   END
154500970526     C*
154600970526     C                   ENDSR
154700970526     C*------------------------------------------------------------------------*
154800970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
154900970526     C*------------------------------------------------------------------------*
155000970526     C     OPZCON        BEGSR
155100970526     C*
155200970526     C* PULISCO CAMPI DI LAVORO
155300970526     C                   Z-ADD     0             I                 5 0
155400970526     C                   Z-ADD     0             W                 5 0
155500970526     C*
155600970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
155700970526     C     1             DO        24            I
155800970526     C                   MOVEL     OPD(I)        OPDRIG
155900970526     C*
156000970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
156100970526     C     OPZKEY        IFEQ      OPDOPZ
156200970526     C     OPZKEY        OREQ      OPDOP1
156300970526     C*
156400970526     C     OPDATT        IFEQ      'S'
156500970526     C                   MOVEL     OPDOPZ        OPZKEY
156600970526     C                   Z-ADD     I             W
156700970526     C                   END
156800970526     C                   END
156900970526     C*
157000970526     C                   END                                                    DO WHILE
157100970526     C*
157200970526     C* OPZIONE NON ATTIVA (ERRORE)
157300970526     C     W             IFEQ      0
157400970526     C*
157500970526     C* OPZIONE NON OK
157600970526     C                   Z-ADD     1             OPZOK
157700970526     C*
157800970526     C* EMISSIONE MESSAGGIO DI ERRORE
157900970526     C                   MOVEL     MSG(1)        DSMSMS
158000970526     C                   ELSE
158100970526     C*
158200970526     C* OPZIONE ATTIVA (PROCEDI ...)
158300970526     C*
158400970526     C     OPZKEY        IFEQ      ' 1'
158500970526     C                   EXSR      O01GES
158600970526     C                   END
158700970526     C     OPZKEY        IFEQ      ' 2'
158800060328     C                   EXSR      O02GES
158900970526     C                   END
159000970611     C     OPZKEY        IFEQ      ' 3'
159100970611     C                   EXSR      O03GES
159200970611     C                   END
159300970526     C     OPZKEY        IFEQ      ' 4'
159400970526     C                   EXSR      O04GES
159500970526     C                   END
159600970526     C     OPZKEY        IFEQ      ' 5'
159700970526     C                   EXSR      O05GES
159800970526     C                   END
159900970529     C     OPZKEY        IFEQ      '13'
160000970529     C                   EXSR      O13GES
160100970529     C                   END
160200970526     C                   END                                                    OPZ.NON ATTIVA
160300970526     C*
160400970526     C                   ENDSR
160500970526     C*------------------------------------------------------------------------*
160600970526     C* NOTE COSTRUZIONE SCHIERE
160700970526     C*------------------------------------------------------------------------*
160800970526     C*
160900970526     C* -MSG-MESSAGGI
161000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
161100970526     C* (NON TOCCARE MSG,1 E MSG,2)
161200970526     C*
161300970526     C* -CMD-COMANDI
161400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
161500970526     C*
161600970526     C* -OPD-OPZIONI
161700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
161800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
161900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
162000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
162100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
162200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
162300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
162400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
162500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
162600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
162700970526     C*
162800970526     C* -FUD-FUNZIONI
162900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
163000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
163100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
163200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
163300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
163400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
163500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
163600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
163700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
163800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
163900970526     O*------------------------------------------------------------------------*
164000970526** DEC - DECODIFICHE
164100970526    GESTIONE                                                     1
164200970530INTERROGAZIONE                                                   2
164300970530     SCELTA                                                      3
1644009705264
1645009705265
164600970526** MSG - MESSAGGI
164700060313TISIP0R- Opzione non gestita                                                   1
164800060313TISIP0R- Tasto funzionale non gestito                                          2
164900060313TISIP0R- Data di riferimento errata
165000060329TISIP0R- Impossibile richiedere Sinonimi per un P.Operativo                    4
165100060313TISIP0R-                                                                       5
165200060403TISIP0R- Accettato solo un parametro di inizio lista (P.O. o Naz/CAP)          6
165300060313TISIP0R- 7                                                                     7
165400060313TISIP0R- 8                                                                     8
165500060313TISIP0R- 9                                                                     9
165600060313TISIP0R- 10                                                                   10
165700060313TISIP0R- 11                                                                   11
165800060313TISIP0R- 12                                                                   12
165900060313TISIP0R- 13                                                                   13
166000060313TISIP0R- 14                                                                   14
166100060313TISIP0R- 15                                                                   15
166200060313TISIP0R- 16                                                                   16
166300060313TISIP0R- 17                                                                   17
166400060313TISIP0R- 18                                                                   18
166500060313TISIP0R- 19                                                                   19
166600060313TISIP0R- 20                                                                   20
166700970526** OPD - OPZIONI
166800970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
166900060315 2=Modifica        -2 -     (S/N)=S                        opz. 02¬
167000060315 3=Duplica         -3 -     (S/N)=S                        opz. 03¬
167100060315 4=Annullamento    -4 -     (S/N)=S                        opz. 04¬
167200060315 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
167300970526                   -6 -     (S/N)=N                        opz. 06¬
167400060315                   -7 -     (S/N)=N                        opz. 07¬
167500970527                   -8 -     (S/N)=N                        opz. 08¬
167600970527                   -9 -     (S/N)=N                        opz. 09¬
167700970527                   -10-     (S/N)=N                        opz. 10¬
167800970529                   -11-     (S/N)=N                        opz. 11¬
167900970529                   -12-     (S/N)=N                        opz. 12¬
16800006031513=Sinonimi        -13-     (S/N)=S                        opz. 13¬
168100970526                   -  -     (S/N)=N                        opz. 14¬
168200970526                   -  -     (S/N)=N                        opz. 15¬
168300970526                   -  -     (S/N)=N                        opz. 16¬
168400970526                   -  -     (S/N)=N                        opz. 17¬
168500970526                   -  -     (S/N)=N                        opz. 18¬
168600970526                   -  -     (S/N)=N                        opz. 19¬
168700970526                   -  -     (S/N)=N                        opz. 20¬
168800970526                   -  -     (S/N)=N                        opz. 21¬
168900970526                   -  -     (S/N)=N                        opz. 22¬
169000970526                   -  -     (S/N)=N                        opz. 23¬
169100970526                   -  -     (S/N)=N                        opz. 24¬
169200970526** FUD - FUNZIONI
169300970526                   -        (S/N)=N                        funz.01¬
169400970526                   -        (S/N)=N                        funz.02¬
169500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
169600970526                   -        (S/N)=N                        funz.04¬
169700970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
169800060315F06=Inserimento    -        (S/N)=S                        funz.06¬
169900970526                   -        (S/N)=N                        funz.07¬
170000970527                   -        (S/N)=N                        funz.08¬
170100970527                   -        (S/N)=N                        funz.09¬
170200970527                   -        (S/N)=N                        funz.10¬
170300970526                   -        (S/N)=N                        funz.11¬
170400970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
170500060315F13=Sinonimi       -        (S/N)=S                        funz.13¬
170600970526                   -        (S/N)=N                        funz.14¬
170700970526                   -        (S/N)=N                        funz.15¬
170800970526                   -        (S/N)=N                        funz.16¬
170900970526                   -        (S/N)=N                        funz.17¬
171000970526                   -        (S/N)=N                        funz.18¬
171100970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
171200970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
171300970527                   -        (S/N)=N                        funz.21¬
171400970527                   -        (S/N)=N                        funz.22¬
171500970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
171600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
