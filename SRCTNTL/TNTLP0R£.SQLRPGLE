000100970526     H*------------------------------------------------------------------------*
000200110428     H* Anagrafica punti di scambio traini
000300110505     h*  al momeno NON utilizziamo la data decorrenza: quinddi il codice DEVE
000400110505     h*  esssere univoco!!
000500970526     H*------------------------------------------------------------------------*
000600970526     H DECEDIT('0,') DATEDIT(*DMY.)
000700970526     F*------------------------------------------------------------------------*
000800970526     F* DATA BASE
000900970526     F*------------------------------------------------------------------------*
001000110502     FTNTLP0D   CF   E             WORKSTN INFDS(DEVDS1)
001100110428     F                                     SFILE(TLP0D05:SFLULT)
001200110428     F                                     SFILE(TLP0DMS:MSGULT)
001300110428     FTNPSC01L  IF   E           K DISK
001400060315     FAZorg01l  IF   E           K DISK
001500060329     Ftabel00f  IF   E           K DISK
001600970526     D*------------------------------------------------------------------------*
001700970526     D* SCHIERE
001800970526     D*------------------------------------------------------------------------*
001900970526     D* DECODIFICHE
002000970526     D*------------
002100970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* MESSAGGI
002400970526     D*------------
002500970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* DESCRIZIONI OPZIONI
002800970526     D*------------
002900970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* DESCRIZIONI TASTI FUNZIONALI
003200970526     D*------------
003300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003400970526     D*------------
003500970526     D* OPZIONI DA CARICARE A VIDEO
003600970526     D*------------
003700970526     D OPV             S             19    DIM(24)
003800970526     D*------------
003900970526     D* FUNZIONI DA CARICARE A VIDEO
004000970526     D*------------
004100970527     D FUV             S             19    DIM(24)
004200970526     D*------------------------------------------------------------------------*
004300970526     D* INPUT
004400970526     D*------------------------------------------------------------------------*
004500970526     D* RIDEFINIZIONE OPZIONI VIDEO
004600970526     D OPDDS           DS
004700970526     D  OPDOPZ                 1      2
004800970526     D  OPDDE1                 3     19
004900970526     D  OPDOP1                21     22
005000970526     D  OPDFI0                23     34
005100970526     D  OPDATT                35     35
005200970526     D  OPDFI1                36     80
005300970526     D  OPDDES                 1     19
005400970526     D  OPDRIG                 1     80
005500970526     D*------------
005600970526     D* RIDEFINIZIONE FUNZIONI VIDEO
005700970526     D FUDDS           DS
005800970526     D  FUDDE1                 1      1
005900970526     D  FUDFUN                 2      3
006000970526     D  FUDDE2                 4     19
006100970526     D  FUDFI0                20     34
006200970526     D  FUDATT                35     35
006300970526     D  FUDFI1                36     80
006400970526     D  FUDDES                 1     19
006500970526     D  FUDRIG                 1     80
006600970526     D*------------
006700970526     D* DS GESTIONE TASTI FUNZIONALI
006800970526     D DSKEY         E DS
006900970526     D DEVDS1          DS
007000970526     D  KEY                  369    369
007100970526     D*------------
007200970526     D* DS "XMSG" - GESTIONE MESSAGGI
007300970526     D*------------
007400970526     D DSMSG         E DS
007500970526     D*------------
007600020919     D TIBS34DS      E DS                                                       *Profili utente
007700020919     D DDATIUTE      E DS                                                       *Dati utente
007800020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007900970527     D*------------------
008000970527     D WLBDA8          DS                  INZ
008100970527     D  G08DAT                 1      8  0
008200970527     D  G08INV                 9     16  0
008300970527     D  G08ERR                17     17
008400970527     D  G08TGI                18     22  0
008500970526     D*------------
008600970526     D* DS DI PROCEDURA
008700970526     D*------------
008800110428     D TNTLP0ds      E DS
008900060314     D tisi00ds      E DS
009000970526     D*------------
009100970526     D KPJBA         E DS
009200060404     D ds2z          E DS
009300060313     d
009400060313     d Dataiso         s               d   datfmt(*iso)
009500060313     d Dataeur         s               d   datfmt(*eur)
009600060329     d Datadmy         s               d   datfmt(*dmy)
009700110503     d kpsc            s                   Like(pscpsc)
009800110503     d depprv          s                   Like(pscprv)
009900110429     d depcap          s                   Like(psccap)
010000110429     d deppsc          s                   Like(pscpsc)
010100110503     d depdde          s                   Like(pscdde)
010200110503     d depnar          s                   Like(pscnar)
010300110429     d knar            s                   Like(pscnar)
010400110429     d kcap            s                   Like(psccap)
010500110429     d dprv            s                   Like(pscprv)
010600110429     d dind            s                   Like(pscind)
010700060329     d kcod            s                   Like(tblCod)
010800060329     d kkey            s                   Like(TblKey)
010900110502     d dpsc            s                   Like(pscpsc)
011000110503     d dnot            s                   Like(pscnot)
011100110502     d dnar            s                   Like(pscnar)
011200110502     d dflo            s                   Like(pscflo)
011300110502     d dcap            s                   Like(psccap)
011400110502     d dloc            s                   Like(pscloc)
011500110502     d ddri            s                   Like(pscdde)
011600110503     d ddsc            s                   Like(pscdsc)
011700060404     d opzione         s              2
011800110505     d
011900110505     D tnpscds       E DS                  EXTNAME(TNPSC00F)
012000110505     d
012100110505     d wrkgetlista     s           4096    varying
012200970526     C*------------------------------------------------------------------------*
012300970526     C* MAIN LINES
012400970526     C*------------------------------------------------------------------------*
012500970526     C*
012600970526     C* SE NON CI SONO ERRORI, PROSEGUE
012700970526IF  1C     ERRL00        IFNE      '1'
012800970526     C*
012900970526     C* CARICA IL SFILE
013000970526     C                   EXSR      POSSFL
013100970526     C*
013200970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013300970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013600970526     C*
013700970526     C* REIMPOSTA ERRORE DI PROCERURA
013800970526     C                   MOVEL     '0'           ERRL00
013900970526     C*
014000970526     C* EMETTE IL SFILE
014100970526     C                   EXSR      OUTSFL
014200970526     C*
014300970526     C* GESTIONE TASTI FUNZIONALI
014400970526     C                   EXSR      FUNCON
014500970526     C*
014600970526E   2C                   ENDDO
014700970527     C*
014800970527     C* CI SONO ERRORI IN ENTRATA
014900970527X   1C                   ELSE
015000060313     C                   MOVEL     '1'           dp0ERR
015100060313     C                   MOVEL     DSMSMS        dp0MSG
015200970526E   1C                   ENDIF
015300970526     C* FINE PGM
015400970526     C                   SETON                                        LR
015500970526     C*------------------------------------------------------------------------*
015600970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
015700970526     C*------------------------------------------------------------------------*
015800970526     C     F03GES        BEGSR
015900970526     C*
016000970526     C* IMPOSTA "PREMUTO F03"
016100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016200060313     C                   MOVEL     '1'           dp0F03                         *DI PROCEDURA
016300970526     C*
016400970526     C                   ENDSR
016500970526     C*------------------------------------------------------------------------*
016600970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
016700970526     C*------------------------------------------------------------------------*
016800970526     C     F05GES        BEGSR
016900970526     C*
017000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F06GES        BEGSR
017700970526     C*
017800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
017900110512     C                   MOVEL     *BLANKS       DNOT
018000110512     C                   MOVEL     *BLANKS       Dpsc
018100110502     C                   MOVEL     *BLANKS       DNAR
018200970602     C                   MOVEL     *BLANKS       DCAP
018300110429     c                   clear                   dind
018400970602     C                   MOVEL     *BLANKS       DLOC
018500110429     C                   MOVEL     *BLANKS       Dprv
018600110502     C                   clear                   Ddri
018700110503     C                   clear                   Ddsc
018800110502     C                   MOVEL     *BLANKS       Dflo
018900970529     C                   MOVEL     DEPOP0        WOP0
019000970529     C                   MOVEL     'F06'         WOP1
019100110428     C                   MOVEL     'TNTLP1R'     WPGM              8
019200060314     C                   EXSR      LANPGMp                                      *LANCIA PGM
019300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019400970526E   1C                   ENDIF
019500970526     C*
019600970526     C                   ENDSR
019700970526     C*------------------------------------------------------------------------*
019800970526     C* F12GES - GESTIONE F12 -> RITORNO
019900970526     C*------------------------------------------------------------------------*
020000970526     C     F12GES        BEGSR
020100970526     C*
020200970526     C* IMPOSTA "PREMUTO F12"
020300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020400060313     C                   MOVEL     '1'           dp0F12                         *DI PROCEDURA
020500970526     C*
020600970526     C                   ENDSR
020700970529     C*------------------------------------------------------------------------*
020800970529     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
020900970529     C*------------------------------------------------------------------------*
021000970529     C     F13GES        BEGSR
021100970529     C*
021200970529     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
021500970526     C*------------------------------------------------------------------------*
021600970526     C     F23GES        BEGSR
021700970526     C*
021800970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
021900970526     C*
022000970526     C                   ENDSR
022100970526     C*------------------------------------------------------------------------*
022200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022300970526     C*------------------------------------------------------------------------*
022400970526     C     F24GES        BEGSR
022500970526     C*
022600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022700970526     C*
022800970526     C                   ENDSR
022900970526     C*------------------------------------------------------------------------*
023000970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023100970526     C*------------------------------------------------------------------------*
023200970526     C     RUPGES        BEGSR
023300970526     C*
023400970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
023500970526     C*
023600970526     C                   ENDSR
023700970526     C*------------------------------------------------------------------------*
023800970526     C* ENTGES - GESTIONE ENTER -> INVIO
023900970526     C*------------------------------------------------------------------------*
024000970526     C     ENTGES        BEGSR
024100970526     C*
024200970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
024300970526     C                   EXSR      NEWPOS
024400970526     C*
024500970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
024600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024700970526IF  2C     SWPOSI        IFNE      0
024800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
024900970526X   2C                   ELSE
025000970526IF  3C     SFLULT        IFNE      0
025100970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025200970526     C*
025300970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
025400970526IF  4C     SWMODI        IFNE      0
025500970526     C                   EXSR      POSSFL
025600970526E   4C                   ENDIF
025700970526E   3C                   ENDIF
025800970526E   2C                   ENDIF
025900970526E   1C                   ENDIF
026000970526     C*
026100970526     C                   ENDSR
026200970526     C*------------------------------------------------------------------------*
026300970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
026400970526     C*------------------------------------------------------------------------*
026500970526     C     O01GES        BEGSR
026600970526     C*
026700970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
026800110502     C                   MOVEL     VIDpsc        dp0psc
026900110503     C                   MOVEL     VIDnot        dp0not
027000110503     C                   MOVEL     VIhNAR        dp0NAR
027100060313     C                   MOVEL     VIDCAP        dp0CAP
027200060313     C                   MOVEL     VIDLOC        dp0LOC
027300110503     c                   if        vihnar<>*blanks
027400110503     c                   clear                   dp0prv
027500110503     c                   else
027600060313     C                   MOVEL     VIDprv        dp0PRV
027700110503     c                   endif
027800110429     C                   MOVEL     VIDind        dp0ind
027900110505     C***                MOVEL     amgdde        dp0dri
028000110505     C                   MOVEL     kpde          dp0dri
028100110502     C                   MOVEL     vihflo        dp0flo
028200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
028300970526     C*
028400970526     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
028700970526     C*------------------------------------------------------------------------*
028800970526     C     O02GES        BEGSR
028900970526     C*
029000970527     C* MODIFICA
029100970704IF  1C     ERRL00        IFEQ      '0'
029200110503     c                   if        vihnar<>*blanks
029300110503     c                   clear                   dprv
029400110503     c                   else
029500060327     C                   MOVEL     VIDprv        Dprv
029600110503     c                   endif
029700110502     C                   MOVEL     VIDpsc        Dpsc
029800110503     C                   MOVEL     VIDnot        Dnot
029900110503     C                   MOVEL     VIhNAR        DNAR
030000970602     C                   MOVEL     VIDCAP        DCAP
030100970602     C                   MOVEL     VIDLOC        DLOC
030200110505     C****               MOVEL     amgdde        Ddri
030300110505     C                   MOVEL     kpde          ddri
030400110503     C                   MOVEL     amgdsc        Ddsc
030500110502     C                   MOVEL     VIDind        Dind
030600110502     C                   MOVEL     VIhflo        Dflo
030700970529     C                   MOVEL     DEPOP0        WOP0
030800970526     C                   MOVEL     'O02'         WOP1
030900110428     C                   MOVEL     'TNTLP1R'     WPGM
031000060315     C                   EXSR      LANPGMP
031100060328     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
031200970528E   1C                   ENDIF
031300970526     C*
031400970526     C                   ENDSR
031500970611     C*------------------------------------------------------------------------*
031600970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
031700970611     C*------------------------------------------------------------------------*
031800970611     C     O03GES        BEGSR
031900970611     C*
032000970611     C* DUPLICA
032100970611IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032200110502     C                   MOVEL     VIDpsc        Dpsc
032300110503     c                   if        vihnar<>*blanks
032400110503     c                   clear                   dprv
032500110503     c                   else
032600110502     C                   MOVEL     VIDprv        Dprv
032700110503     c                   endif
032800110503     C                   MOVEL     VIhNAR        DNAR
032900110502     C                   MOVEL     VIDCAP        DCAP
033000110502     C                   MOVEL     VIDLOC        DLOC
033100110505     C***                MOVEL     amgdde        Ddri
033200110505     C                   MOVEL     kpde          ddri
033300110503     C                   MOVEL     amgdsc        Ddsc
033400110502     C                   MOVEL     VIDind        Dind
033500110502     C                   MOVEL     VIhflo        Dflo
033600060329     C                   MOVEL     DEPOP0        WOP0
033700060329     C                   MOVEL     'O03'         WOP1
033800110428     C                   MOVEL     'TNTLP1R'     WPGM
033900060329     C                   EXSR      LANPGMP
034000970611     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
034100970611E   1C                   ENDIF
034200970611     C*
034300970611     C                   ENDSR
034400970526     C*------------------------------------------------------------------------*
034500970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
034600970526     C*------------------------------------------------------------------------*
034700970526     C     O04GES        BEGSR
034800970526     C*
034900060327IF  1C     ERRL00        IFEQ      '0'
035000110502     C                   MOVEL     VIDpsc        Dpsc
035100110503     C                   MOVEL     VIDnot        Dnot
035200110503     c                   if        vihnar<>*blanks
035300110503     c                   clear                   dprv
035400110503     c                   else
035500110502     C                   MOVEL     VIDprv        Dprv
035600110503     c                   endif
035700110503     C                   MOVEL     VIhNAR        DNAR
035800110502     C                   MOVEL     VIDCAP        DCAP
035900110502     C                   MOVEL     VIDLOC        DLOC
036000110505     C***                MOVEL     amgdde        Ddri
036100110505     C                   MOVEL     kpde          ddri
036200110503     C                   MOVEL     amgdsc        Ddsc
036300110502     C                   MOVEL     VIDind        Dind
036400110502     C                   MOVEL     VIhflo        Dflo
036500060327     C                   MOVEL     DEPOP0        WOP0
036600060327     C                   MOVEL     'O04'         WOP1
036700110428     C                   MOVEL     'TNTLP1R'     WPGM
036800060327     C                   EXSR      LANPGMP
036900060327E   1C                   ENDIF
037000970526     C*
037100970526     C                   ENDSR
037200970526     C*------------------------------------------------------------------------*
037300970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
037400970526     C*------------------------------------------------------------------------*
037500970526     C     O05GES        BEGSR
037600970526     C*
037700110502     C                   MOVEL     VIDpsc        Dpsc
037800110503     C                   MOVEL     VIDnot        Dnot
037900110503     c                   if        vihnar<>*blanks
038000110503     c                   clear                   dprv
038100110503     c                   else
038200110502     C                   MOVEL     VIDprv        Dprv
038300110503     c                   endif
038400110503     C                   MOVEL     VIhNAR        DNAR
038500110502     C                   MOVEL     VIDCAP        DCAP
038600110502     C                   MOVEL     VIDLOC        DLOC
038700110505     C***                MOVEL     amgdde        Ddri
038800110505     C                   MOVEL     kpde          ddri
038900110503     C                   MOVEL     amgdsc        Ddsc
039000110502     C                   MOVEL     VIDind        Dind
039100110502     C                   MOVEL     VIhflo        Dflo
039200060327     C                   MOVEL     DEPOP0        WOP0
039300060327     C                   MOVEL     'O05'         WOP1
039400110428     C                   MOVEL     'TNTLP1R'     WPGM
039500060327     C                   EXSR      LANPGMP
039600970526     C*
039700970526     C                   ENDSR
039800970529     C*------------------------------------------------------------------------*
039900970529     C* O13GES - GESTIONE OPZIONE 13=SINONIMI
040000970529     C*------------------------------------------------------------------------*
040100970529     C     O13GES        BEGSR
040200970529     C*
040300970529     C                   ENDSR
040400970526     C*------------------------------------------------------------------------*
040500970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
040600970526     C*------------------------------------------------------------------------*
040700970526     C     NEWPOS        BEGSR
040800970526     C*
040900970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
041000970526     C*         1= RICHIESTO POSIZIONAMENTO
041100970526     C                   Z-ADD     0             SWPOSI            7 0
041200970526     C*
041300970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
041400970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
041500970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
041600970526     C                   Z-ADD     0             SWMODI            1 0
041700970530     C*
041800970530     C* REIMPOSTA INDICATORI DI ERRORE
041900110505     C                   SETOFF                                       838281
042000970526     C*
042100060403     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
042200110429IF  1C     VIDPpr        IFNE      DEPprv
042300970527     C     VIDPCA        ORNE      DEPCAP
042400110429     C     VIDPps        ORNE      DEPpsc
042500110429     C     VIDPde        ORNE      DEPdde
042600970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
042700970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
042800970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
042900970527     C                   MOVEL     VIDPCA        DEPCAP
043000110502     C                   MOVEL     VIDpps        DEPpsc
043100110502     C                   MOVEL     VIDpde        DEPdde
043200110504     C                   MOVEL     VIDppr        DEPprv
043300970526E   2C                   ENDIF
043400970526E   1C                   ENDIF
043500970526     C*
043600970526     C                   ENDSR
043700970526     C*------------------------------------------------------------------------*
043800970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
043900970526     C*------------------------------------------------------------------------*
044000970526     C     SETFIL        BEGSR
044100970529     C*
044200970529     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
044300110503IF  1C                   if        wtipri= 'PSC'                                        *PER CAP
044400110502     c                   eval      kpsc=vidpps
044500110502     c
044600110502     C     Kpsc          SETLL     tnpsc01l
044700110505     c                   else
044800110505     c*
044900110505     c* Per gli altri caricamenti --> SQL
045000110505     c                   exsr      CarSQL
045100110505     c                   endif
045200060330     c
045300970526     C* LETTURA RECORD
045400970526     C                   EXSR      LETFIL
045500110505     c
045600970526     C*
045700970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
045800970526IF  1C     *IN99         IFEQ      *ON
045900970526     C                   Z-ADD     1             SFLEOF
046000110505     c
046100970526E   1C                   ENDIF
046200110505     c
046300970526     C*
046400970526     C                   ENDSR
046500970526     C*------------------------------------------------------------------------*
046600970526     C* LETFIL - LETTURA RECORD
046700970526     C*------------------------------------------------------------------------*
046800970526     C     LETFIL        BEGSR
046900970526     C*
047000970526     C* LEGGE FINO A:
047100970526     C*     - FINE FILE
047200970526     C*     - TROVATO RECORD VALIDO
047300970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
047400110502     c
047500970526DO  1C     *IN99         DOWEQ     *OFF
047600970526     C     WRECOK        ANDEQ     'N'
047700970529     C*
047800110505     c                   if        wtipri='PSC'
047900110502     C                   READ      tnpsc01l                               99
048000110505     c                   else
048100110505     c                   exsr      LeggiSQL
048200110505     c                   endif
048300970526     C*
048400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
048500970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
048600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
048700110505X   2C                   ELSE
048800110505     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
048900970530E   2C                   ENDIF
049000970526E   1C                   ENDDO
049100970526     C*
049200970526     C                   ENDSR
049300110505     C*------------------------------------------------------------------------*
049400110505     c* Preparazione del SQL
049500110505     C*------------------------------------------------------------------------*
049600110505     C     CARSQL        BEGSR
049700110505     c                   eval      wrkgetlista =
049800110505     c                             'select * from tnpsc00f where  -
049900110505     c                                pscatb='' ''  and  '
050000110505     c*
050100110505     c                   if        wtipri='PRV'
050200110505     c                   eval      wrkgetlista=%trim(wrkgetlista)+ '  -
050300110505     c                             pscprv>=''' + vidppr + ''' +
050400110505     c                             order by PSCPRV, PSCCAP'
050500110505     c                   else
050600110505     c                   eval      wrkgetlista=%trim(wrkgetlista)+ '  -
050700110505     c                             psccap>=''' + vidpca + ''' +
050800110505     c                             order by  PSCCAP'
050900110505     c                   endif
051000110505
051100110505     C/EXEC SQL
051200110505     C+ PREPARE s1 FROM :wrkgetlista
051300110505     C/END-EXEC
051400110505     C
051500110505     C/EXEC SQL
051600110505     C+ DECLARE A1 CURSOR FOR S1
051700110505     C/END-EXEC
051800110505     C
051900110505     C/EXEC SQL
052000110505     C+ OPEN a1
052100110505     C/END-EXEC
052200110505     c                   ENDSR
052300110505     C*------------------------------------------------------------------------*
052400110505     c* Lettura del file con SQL
052500110505     C*------------------------------------------------------------------------*
052600110505     C     LeggiSQL      BEGSR
052700110505     c                   setoff                                       99
052800110505
052900110505      *?Leggo il file
053000110505     C/EXEC SQL
053100110505     C+ FETCH NEXT FROM a1 INTO : tnpSCds
053200110505     C/END-EXEC
053300110505
053400110505     c                   select
053500110505
053600110505     c                   when      sqlcod = 100
053700110505     c                   seton                                        99
053800110505     c                   when      sqlcod < 0
053900110505     c                   seton                                        H199
054000110505     c                   other
054100110505     c
054200110505     c*****              exsr      chkrec
054300110505     c
054400110505     c
054500110505     c                   endsl
054600110505     c
054700110505     c* se hoo finito chiudo il cursore
054800110505     c                   if        *in99
054900110505     C/EXEC SQL
055000110505     C+ CLOSE a1
055100110505     C/END-EXEC
055200110505
055300110505     c                   endif
055400110505
055500110505     c                   ENDSR
055600970526     C*------------------------------------------------------------------------*
055700970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
055800970526     C*------------------------------------------------------------------------*
055900970526     C     CHKREC        BEGSR
056000110503     C*  se c'e' la data decorrenza controllo
056100110505     c                   if        vidpde> 0  and pscdsc>0
056200110503     c                   if        pscdde<=kpde and pscdsc>=kpde
056300110503     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
056400110503     c                   endif
056500110503     c
056600110503     c                   else
056700970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
056800060329     c                   endif
056900060329     c
057000970526     C                   ENDSR
057100970526     C*------------------------------------------------------------------------*
057200970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
057300970526     C*------------------------------------------------------------------------*
057400970526     C     IRESFL        BEGSR
057500970529     C*
057600970529     C                   MOVEL     *BLANKS       OPZKEY
057700110503     c* Codice scambio
057800110503     c                   eval      vidpsc=pscpsc
057900110503     c                   eval      vidnot=pscnot
058000110503     c* indirizzo /loc/prv/nar
058100110503     c                   eval      vidind=pscind
058200110503     c                   eval      vidloc=pscloc
058300110503     c                   if        pscnar<>*blanks
058400110503     c                   eval      vidprv=pscnar
058500110503     c                   else
058600110503     c                   eval      vidprv=pscprv
058700110503     c                   endif
058800110503     c                   eval      vidcap=psccap
058900110503     c                   eval      vihnar=pscnar
059000110503     c
059100110503     c                   eval      vihflo=pscflo
059200110503     c* data decorr/scadenza
059300110505     C***                movel     pscdde        amgdde
059400110505     c***  *iso          movel     pscdde        dataiso
059500110505     c***                movel     dataiso       dataeur
059600110505     c***                movel     dataeur       viddde
059700110503     c
059800110505     c                   clear                   viddsc
059900110505     c                   if        pscdsc>0
060000110503     C                   movel     pscdsc        amgdsc
060100110503     c     *iso          movel     pscdsc        dataiso
060200110504     c                   movel     dataiso       dataeur
060300110503     c                   movel     dataeur       viddsc
060400110505     c                   endif
060500110503     c*
060600060313     c
060700970526     C                   ENDSR
060800970526     C*------------------------------------------------------------------------*
060900970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
061000970526     C*------------------------------------------------------------------------*
061100970526     C     CHKPAR        BEGSR
061200060313     c* controllo data di riferimento se errata
061300110503     c                   clear                   kpde
061400060313     c                   if        vidpde>0
061500060313     C                   Z-ADD     vidpde        G08DAT
061600060313     C                   Z-ADD     *ZEROS        G08INV
061700060313     C                   MOVEL     '0'           G08ERR
061800060313     C                   CALL      'XSRDA8'
061900060313     C                   PARM                    WLBDA8
062000060313     c                   if        g08err='1'
062100060313     C                   MOVEL     MSG(3)        DSMSMS
062200060313     C                   MOVEL     '1'           ERRL00
062300110505     C                   SETON                                        8182
062400110505     c                   leavesr
062500060313E   2C                   else
062600060314     c                   z-add     g08dat        vidpde
062700110502     C                   Z-ADD     G08INV        kpde              8 0          *DATA decorrenza
062800060313     c                   endif
062900060313     c                   endif
063000970529     C*
063100110505     c* Immettere uno dei 3 campi
063200110505     c                   if        (vidpps<>*blanks and vidppr<>*blanks) or
063300110505     c                             (vidpps<>*blanks and vidpca<>*blanks) or
063400110505     c                             (vidpca<>*blanks and vidppr<>*blanks)
063500110505     c                   seton                                        8382
063600110505     C                   MOVEL     MSG(6)        DSMSMS
063700110505     C                   MOVEL     '1'           ERRL00
063800110505     c                   leavesr
063900110505     c                   endif
064000110505     c
064100110505     c                   setoff                                       020304
064200970529     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
064300970529IF  1C     VIDPCA        IFEQ      *BLANKS
064400110503     C     VIDPpr        ANDEQ     *blanks
064500110502     C     VIDPps        ANDEQ     *blanks
064600110502     C                   MOVEL     'PSC'         WTIPRI            3
064700110503     c                   seton                                        02
064800970529E   1C                   ENDIF
064900970529     C*
065000110503IF  2C     VIDPps        IFNE      *blanks
065100110502     C                   MOVEL     'PSC'         WTIPRI
065200110503     c                   seton                                        02
065300970529E   2C                   ENDIF
065400110505     c
065500110505     c* Se immessa la provincia, mi posiziono da lì in poi
065600110505     c                   if        vidppr<>*blanks
065700110505     C                   MOVEL     'PRV'         WTIPRI
065800110505     c                   seton                                        03
065900110505E   2C                   ENDIF
066000110505     c                   if        vidpca<>*blanks
066100110505     C                   MOVEL     'CAP'         WTIPRI
066200110505     c                   seton                                        04
066300110505E   2C                   ENDIF
066400970526     C*
066500970526     C                   ENDSR
066600060314     C*------------------------------------------------------------------------*
066700060314     C* LANPGM - LANCIA UN PROGRAMMA
066800060314     C*------------------------------------------------------------------------*
066900060314     C     LANPGMp       BEGSR
067000060314     C*
067100060314     C* IMPOSTA LA DS DI PROCEDURA
067200110502     c                   clear                   tnTLP0ds
067300060314     C                   MOVEL     WOP0          dp0OP0                         *LIVELLO PROCEDURA
067400060314     C                   MOVEL     WOP1          dp0OP1                         *LIVELLO PGM
067500060314     C                   MOVEL     '0'           dp0F03                         *PREMUTO F03=FINE
067600060314     C                   MOVEL     '0'           dp0F12                         *PREMUTO F12=RITORNO
067700060314     C                   MOVEL     '0'           dp0ERR                         *'1'=ERRORE
067800060314     C                   MOVEL     *BLANKS       dp0MSG                         *MESSAGGIO DI RITORN
067900060314     C                   MOVEL     DNAR          dp0NAR                         *NAZIONE
068000060314     C                   MOVEL     DCAP          dp0CAP                         *CAP
068100110502     C                   MOVEL     Dprv          dp0prv
068200110502     C                   MOVEL     Dind          dp0ind
068300110502     C                   MOVEL     DLOC          dp0LOC
068400110502     C                   MOVEL     Dflo          dp0flo
068500110502     C                   MOVEL     Ddri          dp0dri
068600110503     C                   MOVEL     Ddsc          dp0dsc
068700110502     C                   MOVEL     Dpsc          dp0psc
068800110503     C                   MOVEL     Dnot          dp0not
068900060314     C*
069000060314     C                   CALL      WPGM
069100060314     C                   PARM                    KPJBA
069200110502     C                   PARM                    tNTLP0ds
069300060314     C*
069400060314     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
069500060314IF  1C     dp0F03        IFEQ      '1'                                          *PREMUTO F03
069600060314     C                   MOVEL     '1'           ERRL00                         *ERRORE
069700060314     C                   MOVEL     '1'           F03L00                         *FINE PGM
069800060314X   1C                   ELSE                                                   *NON PREMUTO F03
069900060314     C                   MOVEL     dp0MSG        DSMSMS                         *MESSAGGIO DI RITORN
070000060314E   1C                   ENDIF
070100060314     C*
070200060314     C                   ENDSR
070300970527     C*------------------------------------------------------------------------*
070400970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
070500970527     C*------------------------------------------------------------------------*
070600970527     C     CARTAB        BEGSR
070700970527     C*
070800970527     C                   ENDSR
070900970526     C*------------------------------------------------------------------------*
071000970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
071100970526     C*------------------------------------------------------------------------*
071200970526     C     CHKFOP        BEGSR
071300970626     C*
071400970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
071500060404IF  1C     opzione       IFEQ      '01'
071600970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
071700970626     C                   MOVEL     'S'           OPDATT
071800970626     C                   MOVEL     OPDRIG        OPD(1)
071900970626X   1C                   ELSE
072000970626     C* OPZIONI
072100970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
072200970626     C                   MOVEL     'S'           OPDATT
072300970626     C                   MOVEL     OPDRIG        OPD(5)
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
074300110510IF  1C     opzione       oreq      '01'
074400060331     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
074500060331     C                   MOVEL     'N'           OPDATT
074600060331     C                   MOVEL     OPDRIG        OPD(2)
074700060331     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
074800060331     C                   MOVEL     'N'           OPDATT
074900060331     C                   MOVEL     OPDRIG        OPD(3)
075000060331     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
075100060331     C                   MOVEL     'N'           OPDATT
075200060331     C                   MOVEL     OPDRIG        OPD(4)
075300060331     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
075400060331     C                   MOVEL     'N'           FUDATT
075500060331     C                   MOVEL     FUDRIG        FUD(6)
075600060331    1C                   ENDIF
075700970526     C*
075800970526     C* GESTIONE OPZIONI A VIDEO
075900970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
076000970526     C                   EXSR      OPZGES
076100970526     C*
076200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
076300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
076400970526     C                   EXSR      FUNGES
076500970526     C*
076600970526     C                   ENDSR
076700020919     C*--------------------------------------------------------------------------------------------*
076800020919     C* REPERISCE I DATI UTENTE
076900020919     C*--------------------------------------------------------------------------------------------*
077000020919     C     REPDATIUTE    BEGSR
077100020919     C*
077200020919     C* INIZIALIZZA VARIABILI DI WRK
077300020919     C                   CLEAR                   TIBS34DS
077400020919     C                   CLEAR                   AZUTEDS
077500020919     C                   CLEAR                   DDATIUTE
077600020919     C*
077700020919     C     *DTAARA       DEFINE    §azute        azuteds
077800020919     C     *DTAARA       DEFINE    §datiute      ddatiute
077900020919     C                   IN(E)     *DTAARA
078000020919if  1C                   IF        %Error
078100020919     c                   EVAL      I34Tla = 'L'
078200020919     C                   CALL      'TIBS34R'
078300020919     C                   PARM                    Tibs34Ds
078400020919     C                   IN        *DTAARA
078500020919e   1C                   ENDIF
078600020919     C*
078700020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
078800020919     C                   MOVEL(P)  RSUT          DSFIRS           20
078900020919     C*
079000020919     C                   ENDSR
079100970526     C*------------------------------------------------------------------------*
079200970526     C* *INZSR - OPERAZIONI INIZIALI
079300970526     C*------------------------------------------------------------------------*
079400970526     C     *INZSR        BEGSR
079500970526     C*---
079600970526     C* RICEVIMENTO PARAMETRI
079700970526     C*---
079800970526     C     *ENTRY        PLIST
079900970612     C                   PARM                    KPJBA
080000110502     C                   PARM                    tnTLP0ds
080100970526     C*
080200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
080300970526     C     XMSGPA        PLIST
080400970526     C                   PARM                    DSMSMS
080500970526     C                   PARM                    DSMSPG
080600970526     C                   PARM                    DSMSKE
080700970526     C                   PARM                    DSMSER
080800970526     C*
080900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
081000970526     C                   CALL      'XKEY'
081100970526     C                   PARM                    DSKEY
081200970526     C*---
081300970526     C* VARIABILI NON RIFERITE AL DATA BASE
081400970526     C*---
081500970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
081600970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
081700970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
081800970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
081900970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
082000970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
082100970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
082200970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
082300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
082400060313     C     *LIKE         DEFINE    Dp0OP0        DEPOP0                         *DI DEPOSITO
082500060313     C     *LIKE         DEFINE    Dp0OP0        WOP0                           *DI LAVORO
082600060313     C     *LIKE         DEFINE    Dp0OP1        WOP1
082700970526     C*---
082800970526     C* VARIABILI DI CONTROLLO
082900970526     C*---
083000970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
083100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
083200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
083300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
083400970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
083500110527     C                   Z-ADD     16            SFLPAG                         *N°RIGHE X PAG
083600970526     C*
083700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
083800110502     C                   MOVEL     'TNTLP0R'     DSMSPG
083900110502     C                   MOVEL     'TNTLP0R'     MSGQUE
084000970526     C                   MOVEL     *BLANKS       MSGKEY
084100970526     C                   MOVEL     *BLANKS       DSMSMS
084200970526     C                   MOVEL     *BLANKS       DSMSKE
084300970526     C                   Z-ADD     *ZEROS        DSMSER
084400060330     c
084500970526     C*---
084600970526     C* CHIAVI DI LETTURA
084700970526     C*---
084800060329     c     Ktab          Klist
084900060329     c                   Kfld                    codut             1 0
085000060329     c                   Kfld                    kCod
085100060329     c                   Kfld                    kKey
085200060329     c                   z-add     1             codut
085300970527     C* REPERIMENTO DATI UTENTE
085400020919     C                   EXSR      REPDATIUTE
085500970527     C*
085600970527     C* CARICA LE TABELLE OCCORRENTI
085700970527     C                   EXSR      CARTAB
085800970526     C*
085900970526     C* DEPOSITA LE OPZIONI DI ENTRATA
086000060313     C                   MOVEL     dp0OP0        DEPOP0                         *LIVELLO PROCEDURA
086100970526     C*
086200970526     C* CALCOLA LA DATA CORRENTE
086300970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
086400970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
086500970526     C                   Z-ADD     WN8           G08DAT
086600970526     C                   Z-ADD     *ZEROS        G08INV
086700970526     C                   MOVEL     '0'           G08ERR
086800970526     C                   CALL      'XSRDA8'
086900970526     C                   PARM                    WLBDA8
087000970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
087100060313     c
087200060313     c* Se non passata la data di ingresso, uso data corrente
087300060313     c                   if        dP0dri=0
087400110503     c                   eval      kpde=datcor
087500060313     c                   eval      vidpde=wn8
087600060313     c                   else
087700110503     c                   eval      kpde=dp0dri
087800060313     c* La imposto a video
087900060313     C                   Z-ADD     Dp0DRI        G08INV
088000060313     C                   Z-ADD     *ZEROS        G08DAT
088100060313     C                   MOVEL     '3'           G08ERR
088200060313     C                   CALL      'XSRDA8'
088300060313     C                   PARM                    WLBDA8
088400060313     C                   Z-ADD     G08DAT        VIDpde                         *DATA RIFERIMENTO
088500060313     c                   endif
088600970526     C*
088700970526     C* IMPOSTA I PARAMETRI DI INGRESSO
088800060313     c
088900970526     C                   EXSR      CHKPAR
089000970526IF  1C     ERRL00        IFEQ      '1'
089100060313     C                   MOVEL     '1'           Dp0ERR
089200060313     C                   MOVEL     DSMSMS        Dp0MSG
089300970526X   1C                   ELSE
089400110503     C                   MOVEL     VIDpde        DEPdde                         *DEPOSITI
089500970526E   1C                   ENDIF
089600970526     C*
089700970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
089800060404     c                   eval      opzione=%subst(depop0:2:2)
089900970526     C                   EXSR      CHKFOP
090000970526     C*
090100970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
090200060404IF  1C     opzione       IFEQ      '01'
090300970530     C                   MOVEL     DEC(3)        VIDING
090400970530E   1C                   ENDIF
090500060404IF  1C     opzione       IFEQ      '02'
090600970530     C                   MOVEL     DEC(1)        VIDING
090700970530E   1C                   ENDIF
090800060404IF  1C     opzione       IFEQ      '05'
090900970530     C                   MOVEL     DEC(2)        VIDING
091000970530E   1C                   ENDIF
091100970526     C*
091200970526     C                   ENDSR
091300970526     C*------------------------------------------------------------------------*
091400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091600970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
091700970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
091800970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
091900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
092000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
092100970526     C*------------------------------------------------------------------------*
092200970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
092300970526     C*------------------------------------------------------------------------*
092400970526     C     POSSFL        BEGSR
092500970526     C*
092600970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
092700970526     C                   EXSR      CLRSFL
092800970526     C*
092900970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
093000110505     C***                EXSR      SETFIL
093100110505     c                   clear                   sfleof
093200970526     C*
093300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
093400970526     C     SFLEOF        IFEQ      0
093500970526     C                   EXSR      CARSFL
093600970526     C                   END
093700970526     C*
093800970526     C                   ENDSR
093900970526     C*------------------------------------------------------------------------*
094000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
094100970526     C*------------------------------------------------------------------------*
094200970526     C     ENTATT        BEGSR
094300970526     C*
094400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
094500970526     C                   Z-ADD     0             OPZOK             1 0
094600970526     C*
094700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
094800970526     C                   Z-ADD     0             SFLVAR            5 0
094900970526     C*
095000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
095100970526     C                   Z-ADD     0             SFLERR
095200970526     C*
095300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
095400970526     C                   Z-ADD     SFLULT        WFLULT
095500970526     C*
095600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
095700110428     C                   READC     TLP0D05                                98
095800970526     C     *IN98         DOWEQ     '0'
095900970526     C*
096000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
096100970526     C                   ADD       1             SFLVAR
096200970526     C*
096300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
096400970526     C     OPZOK         IFEQ      0
096500970526     C*
096600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
096700970526     C     OPZKEY        IFNE      *BLANKS
096800970526     C                   EXSR      OPZCON
096900970526     C                   END
097000970526     C*
097100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
097200970526     C*            2. PULIZIA SCELTA IN SFL
097300970526     C     OPZOK         IFEQ      0
097400970526     C                   MOVEL     '0'           *IN41
097500970526     C                   MOVEL     *BLANKS       OPZKEY
097600970526     C                   ELSE
097700970526     C*
097800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
097900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
098000970526     C                   MOVEL     '1'           *IN41
098100970526     C                   Z-ADD     SFLULT        SFLERR
098200970526     C                   END
098300970526     C                   ELSE
098400970526     C*
098500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
098600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
098700970526     C                   MOVEL     '1'           *IN41
098800970526     C                   END                                                    OPZOK=0
098900970526     C*
099000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
099100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
099200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
099300970526     C     OPZOK         IFNE      0
099400970526     C                   Z-ADD     SFLERR        SFLNBR
099500970526     C                   ELSE
099600970526     C                   Z-ADD     SFLULT        SFLNBR
099700970526     C                   END
099800970526     C*
099900970526     C* AGGIORNO RECORD SFL
100000110428     C                   UPDATE    TLP0D05
100100970526     C*
100200110428     C                   READC     TLP0D05                                98
100300970526     C                   END                                                    DO WHILE
100400970526     C*
100500970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
100600970526     C                   Z-ADD     WFLULT        SFLULT
100700970526     C*
100800970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
100900970526     C     SFLVAR        IFEQ      0
101000970526     C*---                 MOVEL'1'       FINL00
101100970526     C                   END
101200970526     C*
101300970526     C                   ENDSR
101400970526     C*------------------------------------------------------------------------*
101500970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
101600970526     C*------------------------------------------------------------------------*
101700970526     C     CLRSFL        BEGSR
101800970526     C*
101900970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
102000970526     C* (0=NON FINE FILE  1=FINE FILE)
102100970526     C                   Z-ADD     0             SFLEOF
102200970526     C*
102300970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
102400970526     C                   Z-ADD     0             SFLULT
102500970526     C*
102600970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
102700970526     C                   MOVEL     '1'           *IN40
102800110428     C                   WRITE     TLP0D06
102900970526     C                   MOVEL     '0'           *IN40
103000970526     C*
103100970526     C                   ENDSR
103200970526     C*------------------------------------------------------------------------*
103300970526     C* CARSFL - CARICAMENTO SFL
103400970526     C*------------------------------------------------------------------------*
103500970526     C     CARSFL        BEGSR
103600970526     C*
103700970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
103800970526     C     SFLULT        IFEQ      0
103900970526     C                   EXSR      SETFIL
104000970526     C                   END
104100970526     C*
104200970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
104300970526     C                   Z-ADD     SFLULT        WFLULT
104400970526     C*
104500970526     C* CICLO:
104600970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
104700970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
104800970526     C                   Z-ADD     1             SFLRIG
104900970526     C     SFLRIG        DOWLE     SFLPAG
105000970526     C     SFLEOF        ANDEQ     0
105100970526     C*
105200970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
105300970526     C                   MOVEL     *BLANKS       OPZKEY
105400970526     C*
105500970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
105600970526     C                   EXSR      IRESFL
105700970526     C*
105800970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
105900970526     C                   ADD       1             SFLULT
106000970526     C*
106100970526     C* SCRITTURA RECORD SFL
106200110428     C                   WRITE     TLP0D05
106300970526     C                   ADD       1             SFLRIG
106400970526     C*
106500970526     C* LETTURA FILE DA CARICARE
106600970526     C                   EXSR      LETFIL
106700970526     C                   END                                                    DO WHILE
106800970526     C*
106900970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
107000970526     C     WFLULT        IFNE      SFLULT
107100970526     C                   Z-ADD     WFLULT        SFLNBR
107200970526     C                   ADD       1             SFLNBR
107300970526     C                   END
107400970526     C*
107500970526     C                   ENDSR
107600970526     C*------------------------------------------------------------------------*
107700970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
107800970526     C*------------------------------------------------------------------------*
107900970526     C     OUTSFL        BEGSR
108000970526     C*
108100970526     C* WRITE RECORD TESTATA VIDEO
108200110428     C                   WRITE     TLP0D01
108300970526     C*
108400970526     C* VISUALIZZAZIONE MESSAGGIO
108500970526     C                   EXSR      SNDMSG
108600970526     C*
108700970526     C* WRITE RECORD OPZIONI VIDEO
108800110428     C                   WRITE     TLP0D02
108900970526     C*
109000970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
109100110428     C                   WRITE     TLP0D03
109200970526     C*
109300970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
109400110428     C                   WRITE     TLP0D04
109500970526     C*
109600970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
109700970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
109800970526     C     SFLEOF        IFEQ      0
109900970526     C                   MOVEL     '0'           *IN42
110000970526     C                   ELSE
110100970526     C*
110200970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
110300970526     C                   MOVEL     '1'           *IN42
110400970526     C                   END
110500970526     C*
110600970526     C* WRITE RECORD TASTI FUNZIONALI
110700110428     C                   WRITE     TLP0D09
110800970526     C*
110900970526     C* CASO DI SFL VUOTO
111000970526     C     SFLULT        IFLE      0
111100970526     C*
111200970526     C*    - WRITE RECORD VUOTO
111300110428     C                   WRITE     TLP0D0A
111400110428     C                   READ      TLP0D0A                                99
111500970526     C*
111600970526     C                   ELSE
111700970526     C*
111800970526     C* CASO DI SFL CON RECORDS
111900970526     C*
112000970526     C*    - WRITE RECORD CONTROLLO SFL
112100110428     C                   WRITE     TLP0D06
112200110428     C                   READ      TLP0D06                                99
112300970526     C                   END
112400970526     C*
112500970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
112600110428     C                   READ      TLP0D03                                99
112700970526     C*
112800970526     C                   ENDSR
112900970526     C*------------------------------------------------------------------------*
113000970526     C* SNDMSG - MANDA MESSAGGIO
113100970526     C*------------------------------------------------------------------------*
113200970526     C     SNDMSG        BEGSR
113300970526     C*
113400970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
113500970526     C                   MOVEL     '1'           *IN40
113600110428     C                   WRITE     TLP0DMC
113700970526     C                   MOVEL     '0'           *IN40
113800970526     C*
113900970526     C* VISUALIZZAZIONE MESSAGGIO
114000970526     C                   Z-ADD     0             MSGULT
114100970526     C                   CALL      'XMSG'        XMSGPA
114200970526     C     DSMSER        IFEQ      0
114300970526     C                   MOVEL     DSMSKE        MSGKEY
114400970526     C                   END
114500970526     C                   Z-ADD     1             MSGULT
114600110428     C                   WRITE     TLP0DMS                                      SFL MESS
114700110428     C                   WRITE     TLP0DMC                                      CONTR MESS
114800970526     C*
114900970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
115000970526     C                   MOVEL     *BLANKS       DSMSMS
115100970526     C*
115200970526     C                   ENDSR
115300970526     C*------------------------------------------------------------------------*
115400970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
115500970526     C*------------------------------------------------------------------------*
115600970526     C     FUNCON        BEGSR
115700970526     C*
115800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
115900970526     C                   EXSR      FUNVAR
116000970526     C*
116100970526     C* PULISCO CAMPI DI LAVORO
116200970526     C                   Z-ADD     0             I                 5 0
116300970526     C                   Z-ADD     0             W                 5 0
116400970526     C*
116500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
116600970526     C     FUNKEY        IFNE      *BLANKS
116700970526     C     1             DO        24            I
116800970526     C                   MOVEL     FUD(I)        FUDRIG
116900970526     C*
117000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
117100970526     C     FUNKEY        IFEQ      FUDFUN
117200970526     C     FUDATT        ANDEQ     'S'
117300970526     C                   Z-ADD     I             W
117400970526     C                   END
117500970526     C                   END                                                    DO WHILE
117600970526     C*
117700970526     C* FUNZIONE NON ATTIVA (ERRORE)
117800970526     C     W             IFEQ      0
117900970526     C*
118000970526     C* EMISSIONE MESSAGGIO DI ERRORE
118100970526     C                   MOVEL     MSG(2)        DSMSMS
118200970526     C                   END                                                    W=0
118300970526     C                   END                                                    FUNKEY NE *BLANKS
118400970526     C*
118500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
118600970526     C     W             IFNE      0
118700970526     C     FUNKEY        OREQ      *BLANKS
118800970526     C*
118900970526     C     FUNKEY        IFEQ      *BLANKS
119000970526     C                   EXSR      ENTGES
119100970526     C                   END
119200970526     C     FUNKEY        IFEQ      '03'
119300970526     C                   EXSR      F03GES
119400970526     C                   END
119500970526     C     FUNKEY        IFEQ      '05'
119600970526     C                   EXSR      F05GES
119700970526     C                   END
119800970526     C     FUNKEY        IFEQ      '06'
119900970526     C                   EXSR      F06GES
120000970526     C                   END
120100970526     C     FUNKEY        IFEQ      '12'
120200970526     C                   EXSR      F12GES
120300970526     C                   END
120400970529     C     FUNKEY        IFEQ      '13'
120500970529     C                   EXSR      F13GES
120600970529     C                   END
120700970526     C     FUNKEY        IFEQ      '23'
120800970526     C                   EXSR      F23GES
120900970526     C                   END
121000970526     C     FUNKEY        IFEQ      '24'
121100970526     C                   EXSR      F24GES
121200970526     C                   END
121300970526     C     FUNKEY        IFEQ      'RU'
121400970526     C                   EXSR      RUPGES
121500970526     C                   END
121600970526     C*
121700970526     C                   END
121800970526     C*
121900970526     C                   ENDSR
122000970526     C*------------------------------------------------------------------------*
122100970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
122200970526     C*------------------------------------------------------------------------*
122300970526     C     FUNVAR        BEGSR
122400970526     C*
122500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
122600970526     C     KEY           IFEQ      ENTER
122700970526     C                   MOVEL     *BLANKS       FUNKEY
122800970526     C                   END
122900970526     C     KEY           IFEQ      F1
123000970526     C                   MOVEL     '01'          FUNKEY
123100970526     C                   END
123200970526     C     KEY           IFEQ      F2
123300970526     C                   MOVEL     '02'          FUNKEY
123400970526     C                   END
123500970526     C     KEY           IFEQ      F3
123600970526     C                   MOVEL     '03'          FUNKEY
123700970526     C                   END
123800970526     C     KEY           IFEQ      F4
123900970526     C                   MOVEL     '04'          FUNKEY
124000970526     C                   END
124100970526     C     KEY           IFEQ      F5
124200970526     C                   MOVEL     '05'          FUNKEY
124300970526     C                   END
124400970526     C     KEY           IFEQ      F6
124500970526     C                   MOVEL     '06'          FUNKEY
124600970526     C                   END
124700970526     C     KEY           IFEQ      F7
124800970526     C                   MOVEL     '07'          FUNKEY
124900970526     C                   END
125000970526     C     KEY           IFEQ      F8
125100970526     C                   MOVEL     '08'          FUNKEY
125200970526     C                   END
125300970526     C     KEY           IFEQ      F9
125400970526     C                   MOVEL     '09'          FUNKEY
125500970526     C                   END
125600970526     C     KEY           IFEQ      F10
125700970526     C                   MOVEL     '10'          FUNKEY
125800970526     C                   END
125900970526     C     KEY           IFEQ      F11
126000970526     C                   MOVEL     '11'          FUNKEY
126100970526     C                   END
126200970526     C     KEY           IFEQ      F12
126300970526     C                   MOVEL     '12'          FUNKEY
126400970526     C                   END
126500970526     C     KEY           IFEQ      F13
126600970526     C                   MOVEL     '13'          FUNKEY
126700970526     C                   END
126800970526     C     KEY           IFEQ      F14
126900970526     C                   MOVEL     '14'          FUNKEY
127000970526     C                   END
127100970526     C     KEY           IFEQ      F15
127200970526     C                   MOVEL     '15'          FUNKEY
127300970526     C                   END
127400970526     C     KEY           IFEQ      F16
127500970526     C                   MOVEL     '16'          FUNKEY
127600970526     C                   END
127700970526     C     KEY           IFEQ      F17
127800970526     C                   MOVEL     '17'          FUNKEY
127900970526     C                   END
128000970526     C     KEY           IFEQ      F18
128100970526     C                   MOVEL     '18'          FUNKEY
128200970526     C                   END
128300970526     C     KEY           IFEQ      F19
128400970526     C                   MOVEL     '19'          FUNKEY
128500970526     C                   END
128600970526     C     KEY           IFEQ      F20
128700970526     C                   MOVEL     '20'          FUNKEY
128800970526     C                   END
128900970526     C     KEY           IFEQ      F21
129000970526     C                   MOVEL     '21'          FUNKEY
129100970526     C                   END
129200970526     C     KEY           IFEQ      F22
129300970526     C                   MOVEL     '22'          FUNKEY
129400970526     C                   END
129500970526     C     KEY           IFEQ      F23
129600970526     C                   MOVEL     '23'          FUNKEY
129700970526     C                   END
129800970526     C     KEY           IFEQ      F24
129900970526     C                   MOVEL     '24'          FUNKEY
130000970526     C                   END
130100970526     C     KEY           IFEQ      ROLLUP
130200970526     C                   MOVEL     'RU'          FUNKEY
130300970526     C                   END
130400970526     C     KEY           IFEQ      HELP
130500970526     C                   MOVEL     'HP'          FUNKEY
130600970526     C                   END
130700970526     C*
130800970526     C                   ENDSR
130900970526     C*------------------------------------------------------------------------*
131000970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
131100970526     C*------------------------------------------------------------------------*
131200970526     C     FUNGES        BEGSR
131300970526     C*
131400970526     C* PULISCO CAMPI DI LAVORO
131500970526     C                   Z-ADD     0             W                 5 0
131600970526     C*
131700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
131800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
131900970526     C     1             DO        24            I
132000970526     C                   MOVEL     FUD(I)        FUDRIG
132100970526     C*
132200970526     C* PULIZIA FUNZIONI PRECEDENTI
132300970526     C                   MOVEL     *BLANKS       FUV(I)
132400970526     C*
132500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
132600970526     C* (SALTA F23 E F24)
132700970526     C     FUDATT        IFEQ      'S'
132800970526     C     FUDFUN        ANDNE     '23'
132900970526     C     FUDFUN        ANDNE     '24'
133000970526     C                   ADD       1             W
133100970526     C                   MOVEL     FUDDES        FUV(W)
133200970526     C                   END
133300970526     C                   END                                                    DO WHILE
133400970526     C*
133500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
133600970526     C* ALTRIMENTI DISATTIVO F24
133700970526     C     1             DO        24            I
133800970526     C                   MOVEL     FUD(I)        FUDRIG
133900970526     C*
134000970526     C     FUDFUN        IFEQ      '24'
134100970526     C     W             IFGT      6
134200970526     C                   MOVEL     'S'           FUDATT
134300970526     C                   ELSE
134400970526     C                   MOVEL     *BLANKS       FUDATT
134500970526     C                   END                                                    W GT 8
134600970526     C                   MOVEL     FUDRIG        FUD(I)
134700970526     C                   END
134800970526     C                   END                                                    DO WHILE
134900970526     C*
135000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
135100970526     C     W             DIV       6             I
135200970526     C                   MVR                     Z
135300970526     C*
135400970526     C     Z             IFNE      0
135500970526     C     I             ADD       1             Z
135600970526     C                   ELSE
135700970526     C                   Z-ADD     I             Z
135800970526     C                   END
135900970526     C*
136000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
136100970526     C                   ADD       1             FUNGRU
136200970526     C     FUNGRU        IFGT      Z
136300970526     C     FUNGRU        ORLT      1
136400970526     C                   Z-ADD     1             FUNGRU
136500970526     C                   END
136600970526     C*
136700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
136800970526     C     FUNGRU        IFEQ      1
136900970526     C                   MOVEL     FUV(1)        FUN001
137000970526     C                   MOVEL     FUV(2)        FUN002
137100970526     C                   MOVEL     FUV(3)        FUN003
137200970526     C                   MOVEL     FUV(4)        FUN004
137300970526     C                   MOVEL     FUV(5)        FUN005
137400970526     C                   MOVEL     FUV(6)        FUN006
137500970526     C                   END
137600970526     C*
137700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
137800970526     C     FUNGRU        IFEQ      2
137900970526     C                   MOVEL     FUV(7)        FUN001
138000970526     C                   MOVEL     FUV(8)        FUN002
138100970526     C                   MOVEL     FUV(9)        FUN003
138200970526     C                   MOVEL     FUV(10)       FUN004
138300970526     C                   MOVEL     FUV(11)       FUN005
138400970526     C                   MOVEL     FUV(12)       FUN006
138500970526     C                   END
138600970526     C*
138700970526     C* TERZO GRUPPO DI 6 FUNZIONI
138800970526     C     FUNGRU        IFEQ      3
138900970526     C                   MOVEL     FUV(13)       FUN001
139000970526     C                   MOVEL     FUV(14)       FUN002
139100970526     C                   MOVEL     FUV(15)       FUN003
139200970526     C                   MOVEL     FUV(16)       FUN004
139300970526     C                   MOVEL     FUV(17)       FUN005
139400970526     C                   MOVEL     FUV(18)       FUN006
139500970526     C                   END
139600970526     C*
139700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
139800970526     C     FUNGRU        IFEQ      4
139900970526     C                   MOVEL     FUV(19)       FUN001
140000970526     C                   MOVEL     FUV(20)       FUN002
140100970526     C                   MOVEL     FUV(21)       FUN003
140200970526     C                   MOVEL     FUV(22)       FUN004
140300970526     C                   MOVEL     *BLANKS       FUN005
140400970526     C                   MOVEL     *BLANKS       FUN006
140500970526     C                   END
140600970526     C*
140700970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
140800970526     C                   MOVEL     *BLANKS       FUN007
140900970526     C                   MOVEL     *BLANKS       FUN008
141000970526     C*
141100970526     C     1             DO        24            I
141200970526     C                   MOVEL     FUD(I)        FUDRIG
141300970526     C     FUDFUN        IFEQ      '23'
141400970526     C     FUDATT        ANDEQ     'S'
141500970526     C                   MOVEL     FUDDES        FUN007
141600970526     C                   END
141700970526     C     FUDFUN        IFEQ      '24'
141800970526     C     FUDATT        ANDEQ     'S'
141900970526     C                   MOVEL     FUDDES        FUN008
142000970526     C                   END
142100970526     C                   END                                                    DO
142200970526     C*
142300970526     C                   ENDSR
142400970526     C*------------------------------------------------------------------------*
142500970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
142600970526     C*------------------------------------------------------------------------*
142700970526     C     OPZGES        BEGSR
142800970526     C*
142900970526     C* PULISCO CAMPI DI LAVORO
143000970526     C                   Z-ADD     0             W                 5 0
143100970526     C*
143200970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
143300970526     C* E CARICO TABELLA OPZIONI ATTIVE
143400970526     C     1             DO        24            I
143500970526     C                   MOVEL     OPD(I)        OPDRIG
143600970526     C*
143700970526     C* PULIZIA OPZIONI PRECEDENTI
143800970526     C                   MOVEL     *BLANKS       OPV(I)
143900970526     C*
144000970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
144100970526     C     OPDATT        IFEQ      'S'
144200970526     C                   ADD       1             W
144300970526     C                   MOVEL     OPDDES        OPV(W)
144400970526     C                   END
144500970526     C                   END                                                    DO WHILE
144600970526     C*
144700970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
144800970526     C* ALTRIMENTI DISATTIVO F23
144900970526     C     1             DO        24            I
145000970526     C                   MOVEL     FUD(I)        FUDRIG
145100970526     C*
145200970526     C     FUDFUN        IFEQ      '23'
145300970526     C     W             IFGT      8
145400970526     C                   MOVEL     'S'           FUDATT
145500970526     C                   ELSE
145600970526     C                   MOVEL     *BLANKS       FUDATT
145700970526     C                   END                                                    W GT 8
145800970526     C                   MOVEL     FUDRIG        FUD(I)
145900970526     C                   END
146000970526     C                   END                                                    DO WHILE
146100970526     C*
146200970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
146300970526     C     W             DIV       8             I
146400970526     C                   MVR                     Z                 5 0
146500970526     C*
146600970526     C     Z             IFNE      0
146700970526     C     I             ADD       1             Z
146800970526     C                   ELSE
146900970526     C                   Z-ADD     I             Z
147000970526     C                   END
147100970526     C*
147200970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
147300970526     C                   ADD       1             OPZGRU
147400970526     C     OPZGRU        IFGT      Z
147500970526     C     OPZGRU        ORLT      1
147600970526     C                   Z-ADD     1             OPZGRU
147700970526     C                   END
147800970526     C*
147900970526     C* PRIMO GRUPPO DI 8 OPZIONI
148000970526     C     OPZGRU        IFEQ      1
148100970526     C                   MOVEL     OPV(1)        OPZ001
148200970526     C                   MOVEL     OPV(2)        OPZ002
148300970526     C                   MOVEL     OPV(3)        OPZ003
148400970526     C                   MOVEL     OPV(4)        OPZ004
148500970526     C                   MOVEL     OPV(5)        OPZ005
148600970526     C                   MOVEL     OPV(6)        OPZ006
148700970526     C                   MOVEL     OPV(7)        OPZ007
148800970526     C                   MOVEL     OPV(8)        OPZ008
148900970526     C                   END
149000970526     C*
149100970526     C* SECONDO GRUPPO DI 8 OPZIONI
149200970526     C     OPZGRU        IFEQ      2
149300970526     C                   MOVEL     OPV(9)        OPZ001
149400970526     C                   MOVEL     OPV(10)       OPZ002
149500970526     C                   MOVEL     OPV(11)       OPZ003
149600970526     C                   MOVEL     OPV(12)       OPZ004
149700970526     C                   MOVEL     OPV(13)       OPZ005
149800970526     C                   MOVEL     OPV(14)       OPZ006
149900970526     C                   MOVEL     OPV(15)       OPZ007
150000970526     C                   MOVEL     OPV(16)       OPZ008
150100970526     C                   END
150200970526     C*
150300970526     C* TERZO GRUPPO DI 8 OPZIONI
150400970526     C     OPZGRU        IFEQ      3
150500970526     C                   MOVEL     OPV(17)       OPZ001
150600970526     C                   MOVEL     OPV(18)       OPZ002
150700970526     C                   MOVEL     OPV(19)       OPZ003
150800970526     C                   MOVEL     OPV(20)       OPZ004
150900970526     C                   MOVEL     OPV(21)       OPZ005
151000970526     C                   MOVEL     OPV(22)       OPZ006
151100970526     C                   MOVEL     OPV(23)       OPZ007
151200970526     C                   MOVEL     OPV(24)       OPZ008
151300970526     C                   END
151400970526     C*
151500970526     C                   ENDSR
151600970526     C*------------------------------------------------------------------------*
151700970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
151800970526     C*------------------------------------------------------------------------*
151900970526     C     OPZCON        BEGSR
152000970526     C*
152100970526     C* PULISCO CAMPI DI LAVORO
152200970526     C                   Z-ADD     0             I                 5 0
152300970526     C                   Z-ADD     0             W                 5 0
152400970526     C*
152500970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
152600970526     C     1             DO        24            I
152700970526     C                   MOVEL     OPD(I)        OPDRIG
152800970526     C*
152900970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
153000970526     C     OPZKEY        IFEQ      OPDOPZ
153100970526     C     OPZKEY        OREQ      OPDOP1
153200970526     C*
153300970526     C     OPDATT        IFEQ      'S'
153400970526     C                   MOVEL     OPDOPZ        OPZKEY
153500970526     C                   Z-ADD     I             W
153600970526     C                   END
153700970526     C                   END
153800970526     C*
153900970526     C                   END                                                    DO WHILE
154000970526     C*
154100970526     C* OPZIONE NON ATTIVA (ERRORE)
154200970526     C     W             IFEQ      0
154300970526     C*
154400970526     C* OPZIONE NON OK
154500970526     C                   Z-ADD     1             OPZOK
154600970526     C*
154700970526     C* EMISSIONE MESSAGGIO DI ERRORE
154800970526     C                   MOVEL     MSG(1)        DSMSMS
154900970526     C                   ELSE
155000970526     C*
155100970526     C* OPZIONE ATTIVA (PROCEDI ...)
155200970526     C*
155300970526     C     OPZKEY        IFEQ      ' 1'
155400970526     C                   EXSR      O01GES
155500970526     C                   END
155600970526     C     OPZKEY        IFEQ      ' 2'
155700060328     C                   EXSR      O02GES
155800970526     C                   END
155900970611     C     OPZKEY        IFEQ      ' 3'
156000970611     C                   EXSR      O03GES
156100970611     C                   END
156200970526     C     OPZKEY        IFEQ      ' 4'
156300970526     C                   EXSR      O04GES
156400970526     C                   END
156500970526     C     OPZKEY        IFEQ      ' 5'
156600970526     C                   EXSR      O05GES
156700970526     C                   END
156800970529     C     OPZKEY        IFEQ      '13'
156900970529     C                   EXSR      O13GES
157000970529     C                   END
157100970526     C                   END                                                    OPZ.NON ATTIVA
157200970526     C*
157300970526     C                   ENDSR
157400970526     C*------------------------------------------------------------------------*
157500970526     C* NOTE COSTRUZIONE SCHIERE
157600970526     C*------------------------------------------------------------------------*
157700970526     C*
157800970526     C* -MSG-MESSAGGI
157900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
158000970526     C* (NON TOCCARE MSG,1 E MSG,2)
158100970526     C*
158200970526     C* -CMD-COMANDI
158300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
158400970526     C*
158500970526     C* -OPD-OPZIONI
158600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
158700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
158800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
158900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
159000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
159100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
159200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
159300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
159400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
159500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
159600970526     C*
159700970526     C* -FUD-FUNZIONI
159800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
159900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
160000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
160100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
160200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
160300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
160400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
160500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
160600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
160700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
160800970526     O*------------------------------------------------------------------------*
160900970526** DEC - DECODIFICHE
161000970526    GESTIONE                                                     1
161100970530INTERROGAZIONE                                                   2
161200970530     SCELTA                                                      3
1613009705264
1614009705265
161500970526** MSG - MESSAGGI
161600110502TNTLP0R- Opzione non gestita                                                   1
161700110502TNTLP0R- Tasto funzionale non gestito                                          2
161800110502TNTLP0R- Data di riferimento errata
161900110505TNTLP0R-
162000110502TNTLP0R-                                                                       5
162100110505TNTLP0R- Accettato solo un parametro di inizio lista                           6
162200110502TNTLP0R- 7                                                                     7
162300110502TNTLP0R- 8                                                                     8
162400110502TNTLP0R- 9                                                                     9
162500110502TNTLP0R- 10                                                                   10
162600110502TNTLP0R- 11                                                                   11
162700110502TNTLP0R- 12                                                                   12
162800110502TNTLP0R- 13                                                                   13
162900110502TNTLP0R- 14                                                                   14
163000110502TNTLP0R- 15                                                                   15
163100110502TNTLP0R- 16                                                                   16
163200110502TNTLP0R- 17                                                                   17
163300110502TNTLP0R- 18                                                                   18
163400110502TNTLP0R- 19                                                                   19
163500110502TNTLP0R- 20                                                                   20
163600970526** OPD - OPZIONI
163700970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
163800060315 2=Modifica        -2 -     (S/N)=S                        opz. 02¬
163900060315 3=Duplica         -3 -     (S/N)=S                        opz. 03¬
164000060315 4=Annullamento    -4 -     (S/N)=S                        opz. 04¬
164100060315 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
164200970526                   -6 -     (S/N)=N                        opz. 06¬
164300060315                   -7 -     (S/N)=N                        opz. 07¬
164400970527                   -8 -     (S/N)=N                        opz. 08¬
164500970527                   -9 -     (S/N)=N                        opz. 09¬
164600970527                   -10-     (S/N)=N                        opz. 10¬
164700970529                   -11-     (S/N)=N                        opz. 11¬
164800970529                   -12-     (S/N)=N                        opz. 12¬
164900110503                   -13-     (S/N)=N                        opz. 13¬
165000970526                   -  -     (S/N)=N                        opz. 14¬
165100970526                   -  -     (S/N)=N                        opz. 15¬
165200970526                   -  -     (S/N)=N                        opz. 16¬
165300970526                   -  -     (S/N)=N                        opz. 17¬
165400970526                   -  -     (S/N)=N                        opz. 18¬
165500970526                   -  -     (S/N)=N                        opz. 19¬
165600970526                   -  -     (S/N)=N                        opz. 20¬
165700970526                   -  -     (S/N)=N                        opz. 21¬
165800970526                   -  -     (S/N)=N                        opz. 22¬
165900970526                   -  -     (S/N)=N                        opz. 23¬
166000970526                   -  -     (S/N)=N                        opz. 24¬
166100970526** FUD - FUNZIONI
166200970526                   -        (S/N)=N                        funz.01¬
166300970526                   -        (S/N)=N                        funz.02¬
166400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
166500970526                   -        (S/N)=N                        funz.04¬
166600970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
166700060315F06=Inserimento    -        (S/N)=S                        funz.06¬
166800970526                   -        (S/N)=N                        funz.07¬
166900970527                   -        (S/N)=N                        funz.08¬
167000110503F09=Altri Dati     -        (S/N)=S                        funz.09¬
167100970527                   -        (S/N)=N                        funz.10¬
167200970526                   -        (S/N)=N                        funz.11¬
167300970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
167400110503                   -        (S/N)=N                        funz.13¬
167500970526                   -        (S/N)=N                        funz.14¬
167600970526                   -        (S/N)=N                        funz.15¬
167700970526                   -        (S/N)=N                        funz.16¬
167800970526                   -        (S/N)=N                        funz.17¬
167900970526                   -        (S/N)=N                        funz.18¬
168000970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
168100970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
168200970527                   -        (S/N)=N                        funz.21¬
168300970527                   -        (S/N)=N                        funz.22¬
168400970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
168500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
