000100970526     H*------------------------------------------------------------------------*
000200110428     H* Anagrafica punti di scambio traini
000300110505     h*  al momeno NON utilizziamo la data decorrenza: quinddi il codice DEVE
000400110505     h*  esssere univoco!!
000500970526     H*------------------------------------------------------------------------*
000600970526     H DECEDIT('0,') DATEDIT(*DMY.)
000700130329     h dftactgrp(*no) actgrp(*caller)
000800970526     F*------------------------------------------------------------------------*
000900970526     F* DATA BASE
001000970526     F*------------------------------------------------------------------------*
001100110502     FTNTLP0D   CF   E             WORKSTN INFDS(DEVDS1)
001200110428     F                                     SFILE(TLP0D05:SFLULT)
001300110428     F                                     SFILE(TLP0DMS:MSGULT)
001400130329     f                                     usropn
001500130329     FTNTLP0D1  CF   E             WORKSTN INFDS(DEVDS11)
001600130329     F                                     SFILE(TLP0D051:SFLULT)
001700130329     F                                     SFILE(TLP0DMS1:MSGULT)
001800130329     f                                     usropn
001900110428     FTNPSC01L  IF   E           K DISK
002000060315     FAZorg01l  IF   E           K DISK
002100060329     Ftabel00f  IF   E           K DISK
002200970526     D*------------------------------------------------------------------------*
002300970526     D* SCHIERE
002400970526     D*------------------------------------------------------------------------*
002500970526     D* DECODIFICHE
002600970526     D*------------
002700970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* MESSAGGI
003000970526     D*------------
003100970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* DESCRIZIONI OPZIONI
003400970526     D*------------
003500970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* DESCRIZIONI TASTI FUNZIONALI
003800970526     D*------------
003900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004000970526     D*------------
004100970526     D* OPZIONI DA CARICARE A VIDEO
004200970526     D*------------
004300970526     D OPV             S             19    DIM(24)
004400970526     D*------------
004500970526     D* FUNZIONI DA CARICARE A VIDEO
004600970526     D*------------
004700970527     D FUV             S             19    DIM(24)
004800970526     D*------------------------------------------------------------------------*
004900970526     D* INPUT
005000970526     D*------------------------------------------------------------------------*
005100970526     D* RIDEFINIZIONE OPZIONI VIDEO
005200970526     D OPDDS           DS
005300970526     D  OPDOPZ                 1      2
005400970526     D  OPDDE1                 3     19
005500970526     D  OPDOP1                21     22
005600970526     D  OPDFI0                23     34
005700970526     D  OPDATT                35     35
005800970526     D  OPDFI1                36     80
005900970526     D  OPDDES                 1     19
006000970526     D  OPDRIG                 1     80
006100970526     D*------------
006200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006300970526     D FUDDS           DS
006400970526     D  FUDDE1                 1      1
006500970526     D  FUDFUN                 2      3
006600970526     D  FUDDE2                 4     19
006700970526     D  FUDFI0                20     34
006800970526     D  FUDATT                35     35
006900970526     D  FUDFI1                36     80
007000970526     D  FUDDES                 1     19
007100970526     D  FUDRIG                 1     80
007200970526     D*------------
007300970526     D* DS GESTIONE TASTI FUNZIONALI
007400970526     D DSKEY         E DS
007500970526     D DEVDS1          DS
007600970526     D  KEY                  369    369
007700130329     D DEVDS11         DS
007800130329     D  KEY1                 369    369
007900970526     D*------------
008000970526     D* DS "XMSG" - GESTIONE MESSAGGI
008100970526     D*------------
008200970526     D DSMSG         E DS
008300970526     D*------------
008400020919     D TIBS34DS      E DS                                                       *Profili utente
008500020919     D DDATIUTE      E DS                                                       *Dati utente
008600020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008700970527     D*------------------
008800970527     D WLBDA8          DS                  INZ
008900970527     D  G08DAT                 1      8  0
009000970527     D  G08INV                 9     16  0
009100970527     D  G08ERR                17     17
009200970527     D  G08TGI                18     22  0
009300970526     D*------------
009400970526     D* DS DI PROCEDURA
009500970526     D*------------
009600110428     D TNTLP0ds      E DS
009700060314     D tisi00ds      E DS
009800970526     D*------------
009900970526     D KPJBA         E DS
010000060404     D ds2z          E DS
010100060313     d
010200060313     d Dataiso         s               d   datfmt(*iso)
010300060313     d Dataeur         s               d   datfmt(*eur)
010400060329     d Datadmy         s               d   datfmt(*dmy)
010500110503     d kpsc            s                   Like(pscpsc)
010600110503     d depprv          s                   Like(pscprv)
010700110429     d depcap          s                   Like(psccap)
010800110429     d deppsc          s                   Like(pscpsc)
010900110503     d depdde          s                   Like(pscdde)
011000110503     d depnar          s                   Like(pscnar)
011100110429     d knar            s                   Like(pscnar)
011200110429     d kcap            s                   Like(psccap)
011300110429     d dprv            s                   Like(pscprv)
011400110429     d dind            s                   Like(pscind)
011500060329     d kcod            s                   Like(tblCod)
011600060329     d kkey            s                   Like(TblKey)
011700110502     d dpsc            s                   Like(pscpsc)
011800110503     d dnot            s                   Like(pscnot)
011900110502     d dnar            s                   Like(pscnar)
012000110502     d dflo            s                   Like(pscflo)
012100110502     d dcap            s                   Like(psccap)
012200110502     d dloc            s                   Like(pscloc)
012300110502     d ddri            s                   Like(pscdde)
012400110503     d ddsc            s                   Like(pscdsc)
012500060404     d opzione         s              2
012600110505     d
012700110505     D tnpscds       E DS                  EXTNAME(TNPSC00F)
012800110505     d
012900110505     d wrkgetlista     s           4096    varying
013000130329     d dsp_mod         s             10I 0
013100130329
013200130329     D* PROTOTIPI
013300130329      /copy gaitrasrc/srcprotopr,QsnQryModS
013400970526     C*------------------------------------------------------------------------*
013500970526     C* MAIN LINES
013600970526     C*------------------------------------------------------------------------*
013700970526     C*
013800970526     C* SE NON CI SONO ERRORI, PROSEGUE
013900970526IF  1C     ERRL00        IFNE      '1'
014000970526     C*
014100970526     C* CARICA IL SFILE
014200970526     C                   EXSR      POSSFL
014300970526     C*
014400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014500970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014800970526     C*
014900970526     C* REIMPOSTA ERRORE DI PROCERURA
015000970526     C                   MOVEL     '0'           ERRL00
015100970526     C*
015200970526     C* EMETTE IL SFILE
015300970526     C                   EXSR      OUTSFL
015400970526     C*
015500970526     C* GESTIONE TASTI FUNZIONALI
015600970526     C                   EXSR      FUNCON
015700970526     C*
015800970526E   2C                   ENDDO
015900970527     C*
016000970527     C* CI SONO ERRORI IN ENTRATA
016100970527X   1C                   ELSE
016200060313     C                   MOVEL     '1'           dp0ERR
016300060313     C                   MOVEL     DSMSMS        dp0MSG
016400970526E   1C                   ENDIF
016500970526     C* FINE PGM
016600970526     C                   SETON                                        LR
016700970526     C*------------------------------------------------------------------------*
016800970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
016900970526     C*------------------------------------------------------------------------*
017000970526     C     F03GES        BEGSR
017100970526     C*
017200970526     C* IMPOSTA "PREMUTO F03"
017300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017400060313     C                   MOVEL     '1'           dp0F03                         *DI PROCEDURA
017500970526     C*
017600970526     C                   ENDSR
017700970526     C*------------------------------------------------------------------------*
017800970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
017900970526     C*------------------------------------------------------------------------*
018000970526     C     F05GES        BEGSR
018100970526     C*
018200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018300970526     C*
018400970526     C                   ENDSR
018500970526     C*------------------------------------------------------------------------*
018600970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
018700970526     C*------------------------------------------------------------------------*
018800970526     C     F06GES        BEGSR
018900970526     C*
019000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019100110512     C                   MOVEL     *BLANKS       DNOT
019200110512     C                   MOVEL     *BLANKS       Dpsc
019300110502     C                   MOVEL     *BLANKS       DNAR
019400970602     C                   MOVEL     *BLANKS       DCAP
019500110429     c                   clear                   dind
019600970602     C                   MOVEL     *BLANKS       DLOC
019700110429     C                   MOVEL     *BLANKS       Dprv
019800110502     C                   clear                   Ddri
019900110503     C                   clear                   Ddsc
020000110502     C                   MOVEL     *BLANKS       Dflo
020100970529     C                   MOVEL     DEPOP0        WOP0
020200970529     C                   MOVEL     'F06'         WOP1
020300110428     C                   MOVEL     'TNTLP1R'     WPGM              8
020400060314     C                   EXSR      LANPGMp                                      *LANCIA PGM
020500970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
020600970526E   1C                   ENDIF
020700970526     C*
020800970526     C                   ENDSR
020900970526     C*------------------------------------------------------------------------*
021000970526     C* F12GES - GESTIONE F12 -> RITORNO
021100970526     C*------------------------------------------------------------------------*
021200970526     C     F12GES        BEGSR
021300970526     C*
021400970526     C* IMPOSTA "PREMUTO F12"
021500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021600060313     C                   MOVEL     '1'           dp0F12                         *DI PROCEDURA
021700970526     C*
021800970526     C                   ENDSR
021900970529     C*------------------------------------------------------------------------*
022000970529     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
022100970529     C*------------------------------------------------------------------------*
022200970529     C     F13GES        BEGSR
022300970529     C*
022400970529     C                   ENDSR
022500970526     C*------------------------------------------------------------------------*
022600970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
022700970526     C*------------------------------------------------------------------------*
022800970526     C     F23GES        BEGSR
022900970526     C*
023000970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
023100970526     C*
023200970526     C                   ENDSR
023300970526     C*------------------------------------------------------------------------*
023400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023500970526     C*------------------------------------------------------------------------*
023600970526     C     F24GES        BEGSR
023700970526     C*
023800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023900970526     C*
024000970526     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
024300970526     C*------------------------------------------------------------------------*
024400970526     C     RUPGES        BEGSR
024500970526     C*
024600970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
024700970526     C*
024800970526     C                   ENDSR
024900970526     C*------------------------------------------------------------------------*
025000970526     C* ENTGES - GESTIONE ENTER -> INVIO
025100970526     C*------------------------------------------------------------------------*
025200970526     C     ENTGES        BEGSR
025300970526     C*
025400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025500970526     C                   EXSR      NEWPOS
025600970526     C*
025700970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025900970526IF  2C     SWPOSI        IFNE      0
026000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
026100970526X   2C                   ELSE
026200970526IF  3C     SFLULT        IFNE      0
026300970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026400970526     C*
026500970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026600970526IF  4C     SWMODI        IFNE      0
026700970526     C                   EXSR      POSSFL
026800970526E   4C                   ENDIF
026900970526E   3C                   ENDIF
027000970526E   2C                   ENDIF
027100970526E   1C                   ENDIF
027200970526     C*
027300970526     C                   ENDSR
027400970526     C*------------------------------------------------------------------------*
027500970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
027600970526     C*------------------------------------------------------------------------*
027700970526     C     O01GES        BEGSR
027800970526     C*
027900970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
028000110502     C                   MOVEL     VIDpsc        dp0psc
028100110503     C                   MOVEL     VIDnot        dp0not
028200110503     C                   MOVEL     VIhNAR        dp0NAR
028300060313     C                   MOVEL     VIDCAP        dp0CAP
028400060313     C                   MOVEL     VIDLOC        dp0LOC
028500110503     c                   if        vihnar<>*blanks
028600110503     c                   clear                   dp0prv
028700110503     c                   else
028800060313     C                   MOVEL     VIDprv        dp0PRV
028900110503     c                   endif
029000110429     C                   MOVEL     VIDind        dp0ind
029100110505     C***                MOVEL     amgdde        dp0dri
029200110505     C                   MOVEL     kpde          dp0dri
029300110502     C                   MOVEL     vihflo        dp0flo
029400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
029500970526     C*
029600970526     C                   ENDSR
029700970526     C*------------------------------------------------------------------------*
029800970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
029900970526     C*------------------------------------------------------------------------*
030000970526     C     O02GES        BEGSR
030100970526     C*
030200970527     C* MODIFICA
030300970704IF  1C     ERRL00        IFEQ      '0'
030400110503     c                   if        vihnar<>*blanks
030500110503     c                   clear                   dprv
030600110503     c                   else
030700060327     C                   MOVEL     VIDprv        Dprv
030800110503     c                   endif
030900110502     C                   MOVEL     VIDpsc        Dpsc
031000110503     C                   MOVEL     VIDnot        Dnot
031100110503     C                   MOVEL     VIhNAR        DNAR
031200970602     C                   MOVEL     VIDCAP        DCAP
031300970602     C                   MOVEL     VIDLOC        DLOC
031400110505     C****               MOVEL     amgdde        Ddri
031500110505     C                   MOVEL     kpde          ddri
031600110503     C                   MOVEL     amgdsc        Ddsc
031700110502     C                   MOVEL     VIDind        Dind
031800110502     C                   MOVEL     VIhflo        Dflo
031900970529     C                   MOVEL     DEPOP0        WOP0
032000970526     C                   MOVEL     'O02'         WOP1
032100110428     C                   MOVEL     'TNTLP1R'     WPGM
032200060315     C                   EXSR      LANPGMP
032300060328     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
032400970528E   1C                   ENDIF
032500970526     C*
032600970526     C                   ENDSR
032700970611     C*------------------------------------------------------------------------*
032800970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
032900970611     C*------------------------------------------------------------------------*
033000970611     C     O03GES        BEGSR
033100970611     C*
033200970611     C* DUPLICA
033300970611IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033400110502     C                   MOVEL     VIDpsc        Dpsc
033500110503     c                   if        vihnar<>*blanks
033600110503     c                   clear                   dprv
033700110503     c                   else
033800110502     C                   MOVEL     VIDprv        Dprv
033900110503     c                   endif
034000110503     C                   MOVEL     VIhNAR        DNAR
034100110502     C                   MOVEL     VIDCAP        DCAP
034200110502     C                   MOVEL     VIDLOC        DLOC
034300110505     C***                MOVEL     amgdde        Ddri
034400110505     C                   MOVEL     kpde          ddri
034500110503     C                   MOVEL     amgdsc        Ddsc
034600110502     C                   MOVEL     VIDind        Dind
034700110502     C                   MOVEL     VIhflo        Dflo
034800060329     C                   MOVEL     DEPOP0        WOP0
034900060329     C                   MOVEL     'O03'         WOP1
035000110428     C                   MOVEL     'TNTLP1R'     WPGM
035100060329     C                   EXSR      LANPGMP
035200970611     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
035300970611E   1C                   ENDIF
035400970611     C*
035500970611     C                   ENDSR
035600970526     C*------------------------------------------------------------------------*
035700970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
035800970526     C*------------------------------------------------------------------------*
035900970526     C     O04GES        BEGSR
036000970526     C*
036100060327IF  1C     ERRL00        IFEQ      '0'
036200110502     C                   MOVEL     VIDpsc        Dpsc
036300110503     C                   MOVEL     VIDnot        Dnot
036400110503     c                   if        vihnar<>*blanks
036500110503     c                   clear                   dprv
036600110503     c                   else
036700110502     C                   MOVEL     VIDprv        Dprv
036800110503     c                   endif
036900110503     C                   MOVEL     VIhNAR        DNAR
037000110502     C                   MOVEL     VIDCAP        DCAP
037100110502     C                   MOVEL     VIDLOC        DLOC
037200110505     C***                MOVEL     amgdde        Ddri
037300110505     C                   MOVEL     kpde          ddri
037400110503     C                   MOVEL     amgdsc        Ddsc
037500110502     C                   MOVEL     VIDind        Dind
037600110502     C                   MOVEL     VIhflo        Dflo
037700060327     C                   MOVEL     DEPOP0        WOP0
037800060327     C                   MOVEL     'O04'         WOP1
037900110428     C                   MOVEL     'TNTLP1R'     WPGM
038000060327     C                   EXSR      LANPGMP
038100130404     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
038200060327E   1C                   ENDIF
038300970526     C*
038400970526     C                   ENDSR
038500970526     C*------------------------------------------------------------------------*
038600970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
038700970526     C*------------------------------------------------------------------------*
038800970526     C     O05GES        BEGSR
038900970526     C*
039000110502     C                   MOVEL     VIDpsc        Dpsc
039100110503     C                   MOVEL     VIDnot        Dnot
039200110503     c                   if        vihnar<>*blanks
039300110503     c                   clear                   dprv
039400110503     c                   else
039500110502     C                   MOVEL     VIDprv        Dprv
039600110503     c                   endif
039700110503     C                   MOVEL     VIhNAR        DNAR
039800110502     C                   MOVEL     VIDCAP        DCAP
039900110502     C                   MOVEL     VIDLOC        DLOC
040000110505     C***                MOVEL     amgdde        Ddri
040100110505     C                   MOVEL     kpde          ddri
040200110503     C                   MOVEL     amgdsc        Ddsc
040300110502     C                   MOVEL     VIDind        Dind
040400110502     C                   MOVEL     VIhflo        Dflo
040500060327     C                   MOVEL     DEPOP0        WOP0
040600060327     C                   MOVEL     'O05'         WOP1
040700110428     C                   MOVEL     'TNTLP1R'     WPGM
040800060327     C                   EXSR      LANPGMP
040900970526     C*
041000970526     C                   ENDSR
041100970529     C*------------------------------------------------------------------------*
041200970529     C* O13GES - GESTIONE OPZIONE 13=SINONIMI
041300970529     C*------------------------------------------------------------------------*
041400970529     C     O13GES        BEGSR
041500970529     C*
041600970529     C                   ENDSR
041700970526     C*------------------------------------------------------------------------*
041800970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
041900970526     C*------------------------------------------------------------------------*
042000970526     C     NEWPOS        BEGSR
042100970526     C*
042200970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
042300970526     C*         1= RICHIESTO POSIZIONAMENTO
042400970526     C                   Z-ADD     0             SWPOSI            7 0
042500970526     C*
042600970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
042700970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
042800970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
042900970526     C                   Z-ADD     0             SWMODI            1 0
043000970530     C*
043100970530     C* REIMPOSTA INDICATORI DI ERRORE
043200110505     C                   SETOFF                                       838281
043300970526     C*
043400060403     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
043500110429IF  1C     VIDPpr        IFNE      DEPprv
043600970527     C     VIDPCA        ORNE      DEPCAP
043700110429     C     VIDPps        ORNE      DEPpsc
043800110429     C     VIDPde        ORNE      DEPdde
043900970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
044000970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
044100970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
044200970527     C                   MOVEL     VIDPCA        DEPCAP
044300110502     C                   MOVEL     VIDpps        DEPpsc
044400110502     C                   MOVEL     VIDpde        DEPdde
044500110504     C                   MOVEL     VIDppr        DEPprv
044600970526E   2C                   ENDIF
044700970526E   1C                   ENDIF
044800970526     C*
044900970526     C                   ENDSR
045000970526     C*------------------------------------------------------------------------*
045100970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
045200970526     C*------------------------------------------------------------------------*
045300970526     C     SETFIL        BEGSR
045400970529     C*
045500970529     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
045600110503IF  1C                   if        wtipri= 'PSC'                                        *PER CAP
045700110502     c                   eval      kpsc=vidpps
045800110502     c
045900110502     C     Kpsc          SETLL     tnpsc01l
046000110505     c                   else
046100110505     c*
046200110505     c* Per gli altri caricamenti --> SQL
046300110505     c                   exsr      CarSQL
046400110505     c                   endif
046500060330     c
046600970526     C* LETTURA RECORD
046700970526     C                   EXSR      LETFIL
046800110505     c
046900970526     C*
047000970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
047100970526IF  1C     *IN99         IFEQ      *ON
047200970526     C                   Z-ADD     1             SFLEOF
047300110505     c
047400970526E   1C                   ENDIF
047500110505     c
047600970526     C*
047700970526     C                   ENDSR
047800970526     C*------------------------------------------------------------------------*
047900970526     C* LETFIL - LETTURA RECORD
048000970526     C*------------------------------------------------------------------------*
048100970526     C     LETFIL        BEGSR
048200970526     C*
048300970526     C* LEGGE FINO A:
048400970526     C*     - FINE FILE
048500970526     C*     - TROVATO RECORD VALIDO
048600970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
048700110502     c
048800970526DO  1C     *IN99         DOWEQ     *OFF
048900970526     C     WRECOK        ANDEQ     'N'
049000970529     C*
049100110505     c                   if        wtipri='PSC'
049200110502     C                   READ      tnpsc01l                               99
049300110505     c                   else
049400110505     c                   exsr      LeggiSQL
049500110505     c                   endif
049600970526     C*
049700970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
049800970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
049900970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
050000110505X   2C                   ELSE
050100110505     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
050200970530E   2C                   ENDIF
050300970526E   1C                   ENDDO
050400970526     C*
050500970526     C                   ENDSR
050600110505     C*------------------------------------------------------------------------*
050700110505     c* Preparazione del SQL
050800110505     C*------------------------------------------------------------------------*
050900110505     C     CARSQL        BEGSR
051000110505     c                   eval      wrkgetlista =
051100110505     c                             'select * from tnpsc00f where  -
051200110505     c                                pscatb='' ''  and  '
051300110505     c*
051400110505     c                   if        wtipri='PRV'
051500110505     c                   eval      wrkgetlista=%trim(wrkgetlista)+ '  -
051600110505     c                             pscprv>=''' + vidppr + ''' +
051700110505     c                             order by PSCPRV, PSCCAP'
051800110505     c                   else
051900110505     c                   eval      wrkgetlista=%trim(wrkgetlista)+ '  -
052000110505     c                             psccap>=''' + vidpca + ''' +
052100110505     c                             order by  PSCCAP'
052200110505     c                   endif
052300110505
052400110505     C/EXEC SQL
052500110505     C+ PREPARE s1 FROM :wrkgetlista
052600110505     C/END-EXEC
052700110505     C
052800110505     C/EXEC SQL
052900110505     C+ DECLARE A1 CURSOR FOR S1
053000110505     C/END-EXEC
053100110505     C
053200110505     C/EXEC SQL
053300110505     C+ OPEN a1
053400110505     C/END-EXEC
053500110505     c                   ENDSR
053600110505     C*------------------------------------------------------------------------*
053700110505     c* Lettura del file con SQL
053800110505     C*------------------------------------------------------------------------*
053900110505     C     LeggiSQL      BEGSR
054000110505     c                   setoff                                       99
054100110505
054200110505      *?Leggo il file
054300110505     C/EXEC SQL
054400110505     C+ FETCH NEXT FROM a1 INTO : tnpSCds
054500110505     C/END-EXEC
054600110505
054700110505     c                   select
054800110505
054900110505     c                   when      sqlcod = 100
055000110505     c                   seton                                        99
055100110505     c                   when      sqlcod < 0
055200110505     c                   seton                                        H199
055300110505     c                   other
055400110505     c
055500110505     c*****              exsr      chkrec
055600110505     c
055700110505     c
055800110505     c                   endsl
055900110505     c
056000110505     c* se hoo finito chiudo il cursore
056100110505     c                   if        *in99
056200110505     C/EXEC SQL
056300110505     C+ CLOSE a1
056400110505     C/END-EXEC
056500110505
056600110505     c                   endif
056700110505
056800110505     c                   ENDSR
056900970526     C*------------------------------------------------------------------------*
057000970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
057100970526     C*------------------------------------------------------------------------*
057200970526     C     CHKREC        BEGSR
057300110503     C*  se c'e' la data decorrenza controllo
057400110505     c                   if        vidpde> 0  and pscdsc>0
057500110503     c                   if        pscdde<=kpde and pscdsc>=kpde
057600110503     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
057700110503     c                   endif
057800110503     c
057900110503     c                   else
058000970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
058100060329     c                   endif
058200060329     c
058300970526     C                   ENDSR
058400970526     C*------------------------------------------------------------------------*
058500970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
058600970526     C*------------------------------------------------------------------------*
058700970526     C     IRESFL        BEGSR
058800970529     C*
058900970529     C                   MOVEL     *BLANKS       OPZKEY
059000110503     c* Codice scambio
059100110503     c                   eval      vidpsc=pscpsc
059200110503     c                   eval      vidnot=pscnot
059300110503     c* indirizzo /loc/prv/nar
059400130329     c                   eval      vidind20=pscind
059500130329     c                   eval      vidind=pscind
059600130329     c                   eval      vidloc30=pscloc
059700130329     c                   eval      vidloc=pscloc
059800110503     c                   if        pscnar<>*blanks
059900110503     c                   eval      vidprv=pscnar
060000110503     c                   else
060100110503     c                   eval      vidprv=pscprv
060200110503     c                   endif
060300110503     c                   eval      vidcap=psccap
060400110503     c                   eval      vihnar=pscnar
060500110503     c
060600110503     c                   eval      vihflo=pscflo
060700110503     c* data decorr/scadenza
060800110505     C***                movel     pscdde        amgdde
060900110505     c***  *iso          movel     pscdde        dataiso
061000110505     c***                movel     dataiso       dataeur
061100110505     c***                movel     dataeur       viddde
061200110503     c
061300110505     c                   clear                   viddsc
061400130404     c                   clear                   amgdsc
061500110505     c                   if        pscdsc>0
061600110503     C                   movel     pscdsc        amgdsc
061700110503     c     *iso          movel     pscdsc        dataiso
061800110504     c                   movel     dataiso       dataeur
061900110503     c                   movel     dataeur       viddsc
062000110505     c                   endif
062100110503     c*
062200970526     C                   ENDSR
062300970526     C*------------------------------------------------------------------------*
062400970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
062500970526     C*------------------------------------------------------------------------*
062600970526     C     CHKPAR        BEGSR
062700060313     c* controllo data di riferimento se errata
062800110503     c                   clear                   kpde
062900060313     c                   if        vidpde>0
063000060313     C                   Z-ADD     vidpde        G08DAT
063100060313     C                   Z-ADD     *ZEROS        G08INV
063200060313     C                   MOVEL     '0'           G08ERR
063300060313     C                   CALL      'XSRDA8'
063400060313     C                   PARM                    WLBDA8
063500060313     c                   if        g08err='1'
063600060313     C                   MOVEL     MSG(3)        DSMSMS
063700060313     C                   MOVEL     '1'           ERRL00
063800110505     C                   SETON                                        8182
063900110505     c                   leavesr
064000060313E   2C                   else
064100060314     c                   z-add     g08dat        vidpde
064200110502     C                   Z-ADD     G08INV        kpde              8 0          *DATA decorrenza
064300060313     c                   endif
064400060313     c                   endif
064500970529     C*
064600110505     c* Immettere uno dei 3 campi
064700110505     c                   if        (vidpps<>*blanks and vidppr<>*blanks) or
064800110505     c                             (vidpps<>*blanks and vidpca<>*blanks) or
064900110505     c                             (vidpca<>*blanks and vidppr<>*blanks)
065000110505     c                   seton                                        8382
065100110505     C                   MOVEL     MSG(6)        DSMSMS
065200110505     C                   MOVEL     '1'           ERRL00
065300110505     c                   leavesr
065400110505     c                   endif
065500110505     c
065600110505     c                   setoff                                       020304
065700970529     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
065800970529IF  1C     VIDPCA        IFEQ      *BLANKS
065900110503     C     VIDPpr        ANDEQ     *blanks
066000110502     C     VIDPps        ANDEQ     *blanks
066100110502     C                   MOVEL     'PSC'         WTIPRI            3
066200110503     c                   seton                                        02
066300970529E   1C                   ENDIF
066400970529     C*
066500110503IF  2C     VIDPps        IFNE      *blanks
066600110502     C                   MOVEL     'PSC'         WTIPRI
066700110503     c                   seton                                        02
066800970529E   2C                   ENDIF
066900110505     c
067000110505     c* Se immessa la provincia, mi posiziono da lì in poi
067100110505     c                   if        vidppr<>*blanks
067200110505     C                   MOVEL     'PRV'         WTIPRI
067300110505     c                   seton                                        03
067400110505E   2C                   ENDIF
067500110505     c                   if        vidpca<>*blanks
067600110505     C                   MOVEL     'CAP'         WTIPRI
067700110505     c                   seton                                        04
067800110505E   2C                   ENDIF
067900970526     C*
068000970526     C                   ENDSR
068100060314     C*------------------------------------------------------------------------*
068200060314     C* LANPGM - LANCIA UN PROGRAMMA
068300060314     C*------------------------------------------------------------------------*
068400060314     C     LANPGMp       BEGSR
068500060314     C*
068600060314     C* IMPOSTA LA DS DI PROCEDURA
068700110502     c                   clear                   tnTLP0ds
068800060314     C                   MOVEL     WOP0          dp0OP0                         *LIVELLO PROCEDURA
068900060314     C                   MOVEL     WOP1          dp0OP1                         *LIVELLO PGM
069000060314     C                   MOVEL     '0'           dp0F03                         *PREMUTO F03=FINE
069100060314     C                   MOVEL     '0'           dp0F12                         *PREMUTO F12=RITORNO
069200060314     C                   MOVEL     '0'           dp0ERR                         *'1'=ERRORE
069300060314     C                   MOVEL     *BLANKS       dp0MSG                         *MESSAGGIO DI RITORN
069400060314     C                   MOVEL     DNAR          dp0NAR                         *NAZIONE
069500060314     C                   MOVEL     DCAP          dp0CAP                         *CAP
069600110502     C                   MOVEL     Dprv          dp0prv
069700110502     C                   MOVEL     Dind          dp0ind
069800110502     C                   MOVEL     DLOC          dp0LOC
069900110502     C                   MOVEL     Dflo          dp0flo
070000110502     C                   MOVEL     Ddri          dp0dri
070100110503     C                   MOVEL     Ddsc          dp0dsc
070200110502     C                   MOVEL     Dpsc          dp0psc
070300110503     C                   MOVEL     Dnot          dp0not
070400060314     C*
070500060314     C                   CALL      WPGM
070600060314     C                   PARM                    KPJBA
070700110502     C                   PARM                    tNTLP0ds
070800060314     C*
070900060314     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
071000060314IF  1C     dp0F03        IFEQ      '1'                                          *PREMUTO F03
071100060314     C                   MOVEL     '1'           ERRL00                         *ERRORE
071200060314     C                   MOVEL     '1'           F03L00                         *FINE PGM
071300060314X   1C                   ELSE                                                   *NON PREMUTO F03
071400060314     C                   MOVEL     dp0MSG        DSMSMS                         *MESSAGGIO DI RITORN
071500060314E   1C                   ENDIF
071600060314     C*
071700060314     C                   ENDSR
071800970527     C*------------------------------------------------------------------------*
071900970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
072000970527     C*------------------------------------------------------------------------*
072100970527     C     CARTAB        BEGSR
072200970527     C*
072300970527     C                   ENDSR
072400970526     C*------------------------------------------------------------------------*
072500970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
072600970526     C*------------------------------------------------------------------------*
072700970526     C     CHKFOP        BEGSR
072800970626     C*
072900970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
073000060404IF  1C     opzione       IFEQ      '01'
073100970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
073200970626     C                   MOVEL     'S'           OPDATT
073300970626     C                   MOVEL     OPDRIG        OPD(1)
073400970626X   1C                   ELSE
073500970626     C* OPZIONI
073600970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
073700970626     C                   MOVEL     'S'           OPDATT
073800970626     C                   MOVEL     OPDRIG        OPD(5)
073900970626E   1C                   ENDIF
074000970626     C*
074100970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
074200060404IF  1C     opzione       IFEQ      '02'
074300970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
074400970626     C                   MOVEL     'S'           OPDATT
074500970626     C                   MOVEL     OPDRIG        OPD(2)
074600970627     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
074700970627     C                   MOVEL     'S'           OPDATT
074800970627     C                   MOVEL     OPDRIG        OPD(3)
074900970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
075000970626     C                   MOVEL     'S'           OPDATT
075100970626     C                   MOVEL     OPDRIG        OPD(4)
075200970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
075300970626     C                   MOVEL     'S'           FUDATT
075400970626     C                   MOVEL     FUDRIG        FUD(6)
075500060327    1C                   ENDIF
075600060331     c
075700060404IF  1C     opzione       IFEQ      '05'
075800110510IF  1C     opzione       oreq      '01'
075900060331     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
076000060331     C                   MOVEL     'N'           OPDATT
076100060331     C                   MOVEL     OPDRIG        OPD(2)
076200060331     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
076300060331     C                   MOVEL     'N'           OPDATT
076400060331     C                   MOVEL     OPDRIG        OPD(3)
076500060331     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
076600060331     C                   MOVEL     'N'           OPDATT
076700060331     C                   MOVEL     OPDRIG        OPD(4)
076800060331     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
076900060331     C                   MOVEL     'N'           FUDATT
077000060331     C                   MOVEL     FUDRIG        FUD(6)
077100060331    1C                   ENDIF
077200970526     C*
077300970526     C* GESTIONE OPZIONI A VIDEO
077400970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
077500970526     C                   EXSR      OPZGES
077600970526     C*
077700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
077800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
077900970526     C                   EXSR      FUNGES
078000970526     C*
078100970526     C                   ENDSR
078200020919     C*--------------------------------------------------------------------------------------------*
078300020919     C* REPERISCE I DATI UTENTE
078400020919     C*--------------------------------------------------------------------------------------------*
078500020919     C     REPDATIUTE    BEGSR
078600020919     C*
078700020919     C* INIZIALIZZA VARIABILI DI WRK
078800020919     C                   CLEAR                   TIBS34DS
078900020919     C                   CLEAR                   AZUTEDS
079000020919     C                   CLEAR                   DDATIUTE
079100020919     C*
079200020919     C     *DTAARA       DEFINE    §azute        azuteds
079300020919     C     *DTAARA       DEFINE    §datiute      ddatiute
079400020919     C                   IN(E)     *DTAARA
079500020919if  1C                   IF        %Error
079600020919     c                   EVAL      I34Tla = 'L'
079700020919     C                   CALL      'TIBS34R'
079800020919     C                   PARM                    Tibs34Ds
079900020919     C                   IN        *DTAARA
080000020919e   1C                   ENDIF
080100020919     C*
080200020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
080300020919     C                   MOVEL(P)  RSUT          DSFIRS           20
080400020919     C*
080500020919     C                   ENDSR
080600970526     C*------------------------------------------------------------------------*
080700970526     C* *INZSR - OPERAZIONI INIZIALI
080800970526     C*------------------------------------------------------------------------*
080900970526     C     *INZSR        BEGSR
081000970526     C*---
081100970526     C* RICEVIMENTO PARAMETRI
081200970526     C*---
081300970526     C     *ENTRY        PLIST
081400970612     C                   PARM                    KPJBA
081500110502     C                   PARM                    tnTLP0ds
081600970526     C*
081700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
081800970526     C     XMSGPA        PLIST
081900970526     C                   PARM                    DSMSMS
082000970526     C                   PARM                    DSMSPG
082100970526     C                   PARM                    DSMSKE
082200970526     C                   PARM                    DSMSER
082300970526     C*
082400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
082500970526     C                   CALL      'XKEY'
082600970526     C                   PARM                    DSKEY
082700970526     C*---
082800970526     C* VARIABILI NON RIFERITE AL DATA BASE
082900970526     C*---
083000970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
083100970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
083200970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
083300970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
083400970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
083500970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
083600970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
083700970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
083800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
083900060313     C     *LIKE         DEFINE    Dp0OP0        DEPOP0                         *DI DEPOSITO
084000060313     C     *LIKE         DEFINE    Dp0OP0        WOP0                           *DI LAVORO
084100060313     C     *LIKE         DEFINE    Dp0OP1        WOP1
084200970526     C*---
084300970526     C* VARIABILI DI CONTROLLO
084400970526     C*---
084500970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
084600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
084700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
084800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
084900970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
085000970526     C*
085100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
085200110502     C                   MOVEL     'TNTLP0R'     DSMSPG
085300110502     C                   MOVEL     'TNTLP0R'     MSGQUE
085400970526     C                   MOVEL     *BLANKS       MSGKEY
085500970526     C                   MOVEL     *BLANKS       DSMSMS
085600970526     C                   MOVEL     *BLANKS       DSMSKE
085700970526     C                   Z-ADD     *ZEROS        DSMSER
085800060330     c
085900970526     C*---
086000970526     C* CHIAVI DI LETTURA
086100970526     C*---
086200060329     c     Ktab          Klist
086300060329     c                   Kfld                    codut             1 0
086400060329     c                   Kfld                    kCod
086500060329     c                   Kfld                    kKey
086600060329     c                   z-add     1             codut
086700970527     C* REPERIMENTO DATI UTENTE
086800020919     C                   EXSR      REPDATIUTE
086900970527     C*
087000970527     C* CARICA LE TABELLE OCCORRENTI
087100970527     C                   EXSR      CARTAB
087200970526     C*
087300970526     C* DEPOSITA LE OPZIONI DI ENTRATA
087400060313     C                   MOVEL     dp0OP0        DEPOP0                         *LIVELLO PROCEDURA
087500970526     C*
087600970526     C* CALCOLA LA DATA CORRENTE
087700970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
087800970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
087900970526     C                   Z-ADD     WN8           G08DAT
088000970526     C                   Z-ADD     *ZEROS        G08INV
088100970526     C                   MOVEL     '0'           G08ERR
088200970526     C                   CALL      'XSRDA8'
088300970526     C                   PARM                    WLBDA8
088400970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
088500130329     C* definisco se lo schermo dell'utente supporta o meno il 27X132
088600130329     C                   IF        not %open(TNTLP0D) and
088700130329     C                             not %open(TNTLP0D1)
088800130329     C                   EVAL      dsp_mod = QueryDisplayModeSupport
088900130329     C                              ('4':*omit :*omit :*omit)
089000130329     C                   IF        dsp_mod=0
089100130329     C                   OPEN      TNTLP0D1
089200130404     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
089300130329     C                   ELSE
089400130329     C                   OPEN      TNTLP0D
089500130404     C                   Z-ADD     16            SFLPAG                         *N°RIGHE X PAG
089600130329     C                   ENDIF
089700130329     C                   ENDIF
089800130329     c
089900060313     c
090000060313     c* Se non passata la data di ingresso, uso data corrente
090100060313     c                   if        dP0dri=0
090200110503     c                   eval      kpde=datcor
090300060313     c                   eval      vidpde=wn8
090400060313     c                   else
090500110503     c                   eval      kpde=dp0dri
090600060313     c* La imposto a video
090700060313     C                   Z-ADD     Dp0DRI        G08INV
090800060313     C                   Z-ADD     *ZEROS        G08DAT
090900060313     C                   MOVEL     '3'           G08ERR
091000060313     C                   CALL      'XSRDA8'
091100060313     C                   PARM                    WLBDA8
091200060313     C                   Z-ADD     G08DAT        VIDpde                         *DATA RIFERIMENTO
091300060313     c                   endif
091400970526     C*
091500970526     C* IMPOSTA I PARAMETRI DI INGRESSO
091600060313     c
091700970526     C                   EXSR      CHKPAR
091800970526IF  1C     ERRL00        IFEQ      '1'
091900060313     C                   MOVEL     '1'           Dp0ERR
092000060313     C                   MOVEL     DSMSMS        Dp0MSG
092100970526X   1C                   ELSE
092200110503     C                   MOVEL     VIDpde        DEPdde                         *DEPOSITI
092300970526E   1C                   ENDIF
092400970526     C*
092500970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
092600060404     c                   eval      opzione=%subst(depop0:2:2)
092700970526     C                   EXSR      CHKFOP
092800970526     C*
092900970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
093000060404IF  1C     opzione       IFEQ      '01'
093100970530     C                   MOVEL     DEC(3)        VIDING
093200970530E   1C                   ENDIF
093300060404IF  1C     opzione       IFEQ      '02'
093400970530     C                   MOVEL     DEC(1)        VIDING
093500970530E   1C                   ENDIF
093600060404IF  1C     opzione       IFEQ      '05'
093700970530     C                   MOVEL     DEC(2)        VIDING
093800970530E   1C                   ENDIF
093900970526     C*
094000970526     C                   ENDSR
094100970526     C*------------------------------------------------------------------------*
094200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
094300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
094400970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
094500970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
094600970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
094700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
094800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
094900970526     C*------------------------------------------------------------------------*
095000970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
095100970526     C*------------------------------------------------------------------------*
095200970526     C     POSSFL        BEGSR
095300970526     C*
095400970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
095500970526     C                   EXSR      CLRSFL
095600970526     C*
095700970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
095800110505     C***                EXSR      SETFIL
095900110505     c                   clear                   sfleof
096000970526     C*
096100970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
096200970526     C     SFLEOF        IFEQ      0
096300970526     C                   EXSR      CARSFL
096400970526     C                   END
096500970526     C*
096600970526     C                   ENDSR
096700970526     C*------------------------------------------------------------------------*
096800970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
096900970526     C*------------------------------------------------------------------------*
097000970526     C     ENTATT        BEGSR
097100970526     C*
097200970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
097300970526     C                   Z-ADD     0             OPZOK             1 0
097400970526     C*
097500970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
097600970526     C                   Z-ADD     0             SFLVAR            5 0
097700970526     C*
097800970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
097900970526     C                   Z-ADD     0             SFLERR
098000970526     C*
098100970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
098200970526     C                   Z-ADD     SFLULT        WFLULT
098300970526     C*
098400970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
098500130329     C                   IF        dsp_mod=1
098600110428     C                   READC     TLP0D05                                98
098700130329     c                   else
098800130329     C                   READC     TLP0D051                               98
098900130329     c                   endif
099000970526     C     *IN98         DOWEQ     '0'
099100970526     C*
099200970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
099300970526     C                   ADD       1             SFLVAR
099400970526     C*
099500970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
099600970526     C     OPZOK         IFEQ      0
099700970526     C*
099800970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
099900970526     C     OPZKEY        IFNE      *BLANKS
100000970526     C                   EXSR      OPZCON
100100970526     C                   END
100200970526     C*
100300970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
100400970526     C*            2. PULIZIA SCELTA IN SFL
100500970526     C     OPZOK         IFEQ      0
100600970526     C                   MOVEL     '0'           *IN41
100700970526     C                   MOVEL     *BLANKS       OPZKEY
100800970526     C                   ELSE
100900970526     C*
101000970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
101100970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
101200970526     C                   MOVEL     '1'           *IN41
101300970526     C                   Z-ADD     SFLULT        SFLERR
101400970526     C                   END
101500970526     C                   ELSE
101600970526     C*
101700970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
101800970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
101900970526     C                   MOVEL     '1'           *IN41
102000970526     C                   END                                                    OPZOK=0
102100970526     C*
102200970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
102300970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
102400970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
102500970526     C     OPZOK         IFNE      0
102600970526     C                   Z-ADD     SFLERR        SFLNBR
102700970526     C                   ELSE
102800970526     C                   Z-ADD     SFLULT        SFLNBR
102900970526     C                   END
103000970526     C*
103100970526     C* AGGIORNO RECORD SFL
103200130329     C                   IF        dsp_mod=1
103300110428     C                   UPDATE    TLP0D05
103400130329     c                   else
103500130329     C                   update    TLP0D051
103600130329     c                   endif
103700970526     C*
103800130329     C                   IF        dsp_mod=1
103900110428     C                   READC     TLP0D05                                98
104000130329     c                   else
104100130329     C                   READC     TLP0D051                               98
104200130329     c                   endif
104300970526     C                   END                                                    DO WHILE
104400970526     C*
104500970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
104600970526     C                   Z-ADD     WFLULT        SFLULT
104700970526     C*
104800970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
104900970526     C     SFLVAR        IFEQ      0
105000970526     C*---                 MOVEL'1'       FINL00
105100970526     C                   END
105200970526     C*
105300970526     C                   ENDSR
105400970526     C*------------------------------------------------------------------------*
105500970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
105600970526     C*------------------------------------------------------------------------*
105700970526     C     CLRSFL        BEGSR
105800970526     C*
105900970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
106000970526     C* (0=NON FINE FILE  1=FINE FILE)
106100970526     C                   Z-ADD     0             SFLEOF
106200970526     C*
106300970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
106400970526     C                   Z-ADD     0             SFLULT
106500970526     C*
106600970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
106700970526     C                   MOVEL     '1'           *IN40
106800130329     C                   IF        dsp_mod = 1
106900110428     C                   WRITE     TLP0D06
107000130329     c                   else
107100130329     C                   WRITE     TLP0D061
107200130329     c                   endif
107300970526     C                   MOVEL     '0'           *IN40
107400970526     C*
107500970526     C                   ENDSR
107600970526     C*------------------------------------------------------------------------*
107700970526     C* CARSFL - CARICAMENTO SFL
107800970526     C*------------------------------------------------------------------------*
107900970526     C     CARSFL        BEGSR
108000970526     C*
108100970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
108200970526     C     SFLULT        IFEQ      0
108300970526     C                   EXSR      SETFIL
108400970526     C                   END
108500970526     C*
108600970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
108700970526     C                   Z-ADD     SFLULT        WFLULT
108800970526     C*
108900970526     C* CICLO:
109000970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
109100970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
109200970526     C                   Z-ADD     1             SFLRIG
109300970526     C     SFLRIG        DOWLE     SFLPAG
109400970526     C     SFLEOF        ANDEQ     0
109500970526     C*
109600970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
109700970526     C                   MOVEL     *BLANKS       OPZKEY
109800970526     C*
109900970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
110000970526     C                   EXSR      IRESFL
110100970526     C*
110200970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
110300970526     C                   ADD       1             SFLULT
110400970526     C*
110500970526     C* SCRITTURA RECORD SFL
110600130329     C                   IF        dsp_mod = 1
110700110428     C                   WRITE     TLP0D05
110800130329     c                   else
110900130329     C                   WRITE     TLP0D051
111000130329     c                   endif
111100970526     C                   ADD       1             SFLRIG
111200970526     C*
111300970526     C* LETTURA FILE DA CARICARE
111400970526     C                   EXSR      LETFIL
111500970526     C                   END                                                    DO WHILE
111600970526     C*
111700970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
111800970526     C     WFLULT        IFNE      SFLULT
111900970526     C                   Z-ADD     WFLULT        SFLNBR
112000970526     C                   ADD       1             SFLNBR
112100970526     C                   END
112200970526     C*
112300970526     C                   ENDSR
112400970526     C*------------------------------------------------------------------------*
112500970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
112600970526     C*------------------------------------------------------------------------*
112700970526     C     OUTSFL        BEGSR
112800970526     C*
112900970526     C* WRITE RECORD TESTATA VIDEO
113000130329     C                   IF        dsp_mod = 1
113100110428     C                   WRITE     TLP0D01
113200130329     c                   else
113300130329     C                   WRITE     TLP0D011
113400130329     c                   endif
113500970526     C*
113600970526     C* VISUALIZZAZIONE MESSAGGIO
113700970526     C                   EXSR      SNDMSG
113800970526     C*
113900970526     C* WRITE RECORD OPZIONI VIDEO
114000130329     C                   IF        dsp_mod = 1
114100110428     C                   WRITE     TLP0D02
114200970526     C*
114300970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
114400110428     C                   WRITE     TLP0D03
114500970526     C*
114600970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
114700110428     C                   WRITE     TLP0D04
114800130329     c                   else
114900130329     C                   WRITE     TLP0D021
115000130329     C                   WRITE     TLP0D031
115100130329     C                   WRITE     TLP0D041
115200130329     c                   endif
115300970526     C*
115400970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
115500970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
115600970526     C     SFLEOF        IFEQ      0
115700970526     C                   MOVEL     '0'           *IN42
115800970526     C                   ELSE
115900970526     C*
116000970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
116100970526     C                   MOVEL     '1'           *IN42
116200970526     C                   END
116300970526     C*
116400970526     C* WRITE RECORD TASTI FUNZIONALI
116500130329     C                   IF        dsp_mod = 1
116600110428     C                   WRITE     TLP0D09
116700130329     c                   else
116800130329     C                   WRITE     TLP0D091
116900130329     c                   endif
117000970526     C*
117100970526     C* CASO DI SFL VUOTO
117200970526     C     SFLULT        IFLE      0
117300970526     C*
117400970526     C*    - WRITE RECORD VUOTO
117500130329     C                   IF        dsp_mod = 1
117600110428     C                   WRITE     TLP0D0A
117700110428     C                   READ      TLP0D0A                                99
117800130329     c                   else
117900130329     C                   WRITE     TLP0D0A1
118000130329     C                   READ      TLP0D0A1                               99
118100130329     c                   endif
118200970526     C*
118300970526     C                   ELSE
118400970526     C*
118500970526     C* CASO DI SFL CON RECORDS
118600970526     C*
118700970526     C*    - WRITE RECORD CONTROLLO SFL
118800130329     C                   IF        dsp_mod = 1
118900110428     C                   WRITE     TLP0D06
119000110428     C                   READ      TLP0D06                                99
119100130329     c                   else
119200130329     C                   WRITE     TLP0D061
119300130329     C                   READ      TLP0D061                               99
119400130329     c                   endif
119500970526     C                   END
119600970526     C*
119700970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
119800130329     C                   IF        dsp_mod = 1
119900130329     C                   READ      TLP0D03                                99
120000130329     c                   else
120100130329     C                   READ      TLP0D031                               99
120200130329     c                   endif
120300970526     C*
120400970526     C                   ENDSR
120500970526     C*------------------------------------------------------------------------*
120600970526     C* SNDMSG - MANDA MESSAGGIO
120700970526     C*------------------------------------------------------------------------*
120800970526     C     SNDMSG        BEGSR
120900970526     C*
121000970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
121100970526     C                   MOVEL     '1'           *IN40
121200130329     C                   IF        dsp_mod = 1
121300110428     C                   WRITE     TLP0DMC
121400130329     c                   else
121500130329     C                   WRITE     TLP0DMC1
121600130329     c                   endif
121700970526     C                   MOVEL     '0'           *IN40
121800970526     C*
121900970526     C* VISUALIZZAZIONE MESSAGGIO
122000970526     C                   Z-ADD     0             MSGULT
122100970526     C                   CALL      'XMSG'        XMSGPA
122200970526     C     DSMSER        IFEQ      0
122300970526     C                   MOVEL     DSMSKE        MSGKEY
122400970526     C                   END
122500970526     C                   Z-ADD     1             MSGULT
122600130329     C                   IF        dsp_mod = 1
122700110428     C                   WRITE     TLP0DMS                                      SFL MESS
122800110428     C                   WRITE     TLP0DMC                                      CONTR MESS
122900130329     c                   else
123000130329     C                   WRITE     TLP0DMS1                                     SFL MESS
123100130329     C                   WRITE     TLP0DMC1                                     CONTR MESS
123200130329     c                   endif
123300970526     C*
123400970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
123500970526     C                   MOVEL     *BLANKS       DSMSMS
123600970526     C*
123700970526     C                   ENDSR
123800970526     C*------------------------------------------------------------------------*
123900970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
124000970526     C*------------------------------------------------------------------------*
124100970526     C     FUNCON        BEGSR
124200970526     C*
124300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
124400970526     C                   EXSR      FUNVAR
124500970526     C*
124600970526     C* PULISCO CAMPI DI LAVORO
124700970526     C                   Z-ADD     0             I                 5 0
124800970526     C                   Z-ADD     0             W                 5 0
124900970526     C*
125000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
125100970526     C     FUNKEY        IFNE      *BLANKS
125200970526     C     1             DO        24            I
125300970526     C                   MOVEL     FUD(I)        FUDRIG
125400970526     C*
125500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
125600970526     C     FUNKEY        IFEQ      FUDFUN
125700970526     C     FUDATT        ANDEQ     'S'
125800970526     C                   Z-ADD     I             W
125900970526     C                   END
126000970526     C                   END                                                    DO WHILE
126100970526     C*
126200970526     C* FUNZIONE NON ATTIVA (ERRORE)
126300970526     C     W             IFEQ      0
126400970526     C*
126500970526     C* EMISSIONE MESSAGGIO DI ERRORE
126600970526     C                   MOVEL     MSG(2)        DSMSMS
126700970526     C                   END                                                    W=0
126800970526     C                   END                                                    FUNKEY NE *BLANKS
126900970526     C*
127000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
127100970526     C     W             IFNE      0
127200970526     C     FUNKEY        OREQ      *BLANKS
127300970526     C*
127400970526     C     FUNKEY        IFEQ      *BLANKS
127500970526     C                   EXSR      ENTGES
127600970526     C                   END
127700970526     C     FUNKEY        IFEQ      '03'
127800970526     C                   EXSR      F03GES
127900970526     C                   END
128000970526     C     FUNKEY        IFEQ      '05'
128100970526     C                   EXSR      F05GES
128200970526     C                   END
128300970526     C     FUNKEY        IFEQ      '06'
128400970526     C                   EXSR      F06GES
128500970526     C                   END
128600970526     C     FUNKEY        IFEQ      '12'
128700970526     C                   EXSR      F12GES
128800970526     C                   END
128900970529     C     FUNKEY        IFEQ      '13'
129000970529     C                   EXSR      F13GES
129100970529     C                   END
129200970526     C     FUNKEY        IFEQ      '23'
129300970526     C                   EXSR      F23GES
129400970526     C                   END
129500970526     C     FUNKEY        IFEQ      '24'
129600970526     C                   EXSR      F24GES
129700970526     C                   END
129800970526     C     FUNKEY        IFEQ      'RU'
129900970526     C                   EXSR      RUPGES
130000970526     C                   END
130100970526     C*
130200970526     C                   END
130300970526     C*
130400970526     C                   ENDSR
130500970526     C*------------------------------------------------------------------------*
130600970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
130700970526     C*------------------------------------------------------------------------*
130800970526     C     FUNVAR        BEGSR
130900970526     C*
131000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
131100130329     c                   if        dsp_mod=1
131200970526     C     KEY           IFEQ      ENTER
131300970526     C                   MOVEL     *BLANKS       FUNKEY
131400970526     C                   END
131500970526     C     KEY           IFEQ      F1
131600970526     C                   MOVEL     '01'          FUNKEY
131700970526     C                   END
131800970526     C     KEY           IFEQ      F2
131900970526     C                   MOVEL     '02'          FUNKEY
132000970526     C                   END
132100970526     C     KEY           IFEQ      F3
132200970526     C                   MOVEL     '03'          FUNKEY
132300970526     C                   END
132400970526     C     KEY           IFEQ      F4
132500970526     C                   MOVEL     '04'          FUNKEY
132600970526     C                   END
132700970526     C     KEY           IFEQ      F5
132800970526     C                   MOVEL     '05'          FUNKEY
132900970526     C                   END
133000970526     C     KEY           IFEQ      F6
133100970526     C                   MOVEL     '06'          FUNKEY
133200970526     C                   END
133300970526     C     KEY           IFEQ      F7
133400970526     C                   MOVEL     '07'          FUNKEY
133500970526     C                   END
133600970526     C     KEY           IFEQ      F8
133700970526     C                   MOVEL     '08'          FUNKEY
133800970526     C                   END
133900970526     C     KEY           IFEQ      F9
134000970526     C                   MOVEL     '09'          FUNKEY
134100970526     C                   END
134200970526     C     KEY           IFEQ      F10
134300970526     C                   MOVEL     '10'          FUNKEY
134400970526     C                   END
134500970526     C     KEY           IFEQ      F11
134600970526     C                   MOVEL     '11'          FUNKEY
134700970526     C                   END
134800970526     C     KEY           IFEQ      F12
134900970526     C                   MOVEL     '12'          FUNKEY
135000970526     C                   END
135100970526     C     KEY           IFEQ      F13
135200970526     C                   MOVEL     '13'          FUNKEY
135300970526     C                   END
135400970526     C     KEY           IFEQ      F14
135500970526     C                   MOVEL     '14'          FUNKEY
135600970526     C                   END
135700970526     C     KEY           IFEQ      F15
135800970526     C                   MOVEL     '15'          FUNKEY
135900970526     C                   END
136000970526     C     KEY           IFEQ      F16
136100970526     C                   MOVEL     '16'          FUNKEY
136200970526     C                   END
136300970526     C     KEY           IFEQ      F17
136400970526     C                   MOVEL     '17'          FUNKEY
136500970526     C                   END
136600970526     C     KEY           IFEQ      F18
136700970526     C                   MOVEL     '18'          FUNKEY
136800970526     C                   END
136900970526     C     KEY           IFEQ      F19
137000970526     C                   MOVEL     '19'          FUNKEY
137100970526     C                   END
137200970526     C     KEY           IFEQ      F20
137300970526     C                   MOVEL     '20'          FUNKEY
137400970526     C                   END
137500970526     C     KEY           IFEQ      F21
137600970526     C                   MOVEL     '21'          FUNKEY
137700970526     C                   END
137800970526     C     KEY           IFEQ      F22
137900970526     C                   MOVEL     '22'          FUNKEY
138000970526     C                   END
138100970526     C     KEY           IFEQ      F23
138200970526     C                   MOVEL     '23'          FUNKEY
138300970526     C                   END
138400970526     C     KEY           IFEQ      F24
138500970526     C                   MOVEL     '24'          FUNKEY
138600970526     C                   END
138700970526     C     KEY           IFEQ      ROLLUP
138800970526     C                   MOVEL     'RU'          FUNKEY
138900970526     C                   END
139000970526     C     KEY           IFEQ      HELP
139100970526     C                   MOVEL     'HP'          FUNKEY
139200970526     C                   END
139300130329     c
139400130329     c                   else
139500130329     C     KEY1          IFEQ      ENTER
139600130329     C                   MOVEL     *BLANKS       FUNKEY
139700130329     C                   END
139800130329     C     KEY1          IFEQ      F1
139900130329     C                   MOVEL     '01'          FUNKEY
140000130329     C                   END
140100130329     C     KEY1          IFEQ      F2
140200130329     C                   MOVEL     '02'          FUNKEY
140300130329     C                   END
140400130329     C     KEY1          IFEQ      F3
140500130329     C                   MOVEL     '03'          FUNKEY
140600130329     C                   END
140700130329     C     KEY1          IFEQ      F4
140800130329     C                   MOVEL     '04'          FUNKEY
140900130329     C                   END
141000130329     C     KEY1          IFEQ      F5
141100130329     C                   MOVEL     '05'          FUNKEY
141200130329     C                   END
141300130329     C     KEY1          IFEQ      F6
141400130329     C                   MOVEL     '06'          FUNKEY
141500130329     C                   END
141600130329     C     KEY1          IFEQ      F7
141700130329     C                   MOVEL     '07'          FUNKEY
141800130329     C                   END
141900130329     C     KEY1          IFEQ      F8
142000130329     C                   MOVEL     '08'          FUNKEY
142100130329     C                   END
142200130329     C     KEY1          IFEQ      F9
142300130329     C                   MOVEL     '09'          FUNKEY
142400130329     C                   END
142500130329     C     KEY1          IFEQ      F10
142600130329     C                   MOVEL     '10'          FUNKEY
142700130329     C                   END
142800130329     C     KEY1          IFEQ      F11
142900130329     C                   MOVEL     '11'          FUNKEY
143000130329     C                   END
143100130329     C     KEY1          IFEQ      F12
143200130329     C                   MOVEL     '12'          FUNKEY
143300130329     C                   END
143400130329     C     KEY1          IFEQ      F13
143500130329     C                   MOVEL     '13'          FUNKEY
143600130329     C                   END
143700130329     C     KEY1          IFEQ      F14
143800130329     C                   MOVEL     '14'          FUNKEY
143900130329     C                   END
144000130329     C     KEY1          IFEQ      F15
144100130329     C                   MOVEL     '15'          FUNKEY
144200130329     C                   END
144300130329     C     KEY1          IFEQ      F16
144400130329     C                   MOVEL     '16'          FUNKEY
144500130329     C                   END
144600130329     C     KEY1          IFEQ      F17
144700130329     C                   MOVEL     '17'          FUNKEY
144800130329     C                   END
144900130329     C     KEY1          IFEQ      F18
145000130329     C                   MOVEL     '18'          FUNKEY
145100130329     C                   END
145200130329     C     KEY1          IFEQ      F19
145300130329     C                   MOVEL     '19'          FUNKEY
145400130329     C                   END
145500130329     C     KEY1          IFEQ      F20
145600130329     C                   MOVEL     '20'          FUNKEY
145700130329     C                   END
145800130329     C     KEY1          IFEQ      F21
145900130329     C                   MOVEL     '21'          FUNKEY
146000130329     C                   END
146100130329     C     KEY1          IFEQ      F22
146200130329     C                   MOVEL     '22'          FUNKEY
146300130329     C                   END
146400130329     C     KEY1          IFEQ      F23
146500130329     C                   MOVEL     '23'          FUNKEY
146600130329     C                   END
146700130329     C     KEY1          IFEQ      F24
146800130329     C                   MOVEL     '24'          FUNKEY
146900130329     C                   END
147000130329     C     KEY1          IFEQ      ROLLUP
147100130329     C                   MOVEL     'RU'          FUNKEY
147200130329     C                   END
147300130329     C     KEY1          IFEQ      HELP
147400130329     C                   MOVEL     'HP'          FUNKEY
147500130329     C                   END
147600130329     c                   endif
147700970526     C*
147800970526     C                   ENDSR
147900970526     C*------------------------------------------------------------------------*
148000970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
148100970526     C*------------------------------------------------------------------------*
148200970526     C     FUNGES        BEGSR
148300970526     C*
148400970526     C* PULISCO CAMPI DI LAVORO
148500970526     C                   Z-ADD     0             W                 5 0
148600970526     C*
148700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
148800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
148900970526     C     1             DO        24            I
149000970526     C                   MOVEL     FUD(I)        FUDRIG
149100970526     C*
149200970526     C* PULIZIA FUNZIONI PRECEDENTI
149300970526     C                   MOVEL     *BLANKS       FUV(I)
149400970526     C*
149500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
149600970526     C* (SALTA F23 E F24)
149700970526     C     FUDATT        IFEQ      'S'
149800970526     C     FUDFUN        ANDNE     '23'
149900970526     C     FUDFUN        ANDNE     '24'
150000970526     C                   ADD       1             W
150100970526     C                   MOVEL     FUDDES        FUV(W)
150200970526     C                   END
150300970526     C                   END                                                    DO WHILE
150400970526     C*
150500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
150600970526     C* ALTRIMENTI DISATTIVO F24
150700970526     C     1             DO        24            I
150800970526     C                   MOVEL     FUD(I)        FUDRIG
150900970526     C*
151000970526     C     FUDFUN        IFEQ      '24'
151100970526     C     W             IFGT      6
151200970526     C                   MOVEL     'S'           FUDATT
151300970526     C                   ELSE
151400970526     C                   MOVEL     *BLANKS       FUDATT
151500970526     C                   END                                                    W GT 8
151600970526     C                   MOVEL     FUDRIG        FUD(I)
151700970526     C                   END
151800970526     C                   END                                                    DO WHILE
151900970526     C*
152000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
152100970526     C     W             DIV       6             I
152200970526     C                   MVR                     Z
152300970526     C*
152400970526     C     Z             IFNE      0
152500970526     C     I             ADD       1             Z
152600970526     C                   ELSE
152700970526     C                   Z-ADD     I             Z
152800970526     C                   END
152900970526     C*
153000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
153100970526     C                   ADD       1             FUNGRU
153200970526     C     FUNGRU        IFGT      Z
153300970526     C     FUNGRU        ORLT      1
153400970526     C                   Z-ADD     1             FUNGRU
153500970526     C                   END
153600970526     C*
153700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
153800970526     C     FUNGRU        IFEQ      1
153900970526     C                   MOVEL     FUV(1)        FUN001
154000970526     C                   MOVEL     FUV(2)        FUN002
154100970526     C                   MOVEL     FUV(3)        FUN003
154200970526     C                   MOVEL     FUV(4)        FUN004
154300970526     C                   MOVEL     FUV(5)        FUN005
154400970526     C                   MOVEL     FUV(6)        FUN006
154500970526     C                   END
154600970526     C*
154700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
154800970526     C     FUNGRU        IFEQ      2
154900970526     C                   MOVEL     FUV(7)        FUN001
155000970526     C                   MOVEL     FUV(8)        FUN002
155100970526     C                   MOVEL     FUV(9)        FUN003
155200970526     C                   MOVEL     FUV(10)       FUN004
155300970526     C                   MOVEL     FUV(11)       FUN005
155400970526     C                   MOVEL     FUV(12)       FUN006
155500970526     C                   END
155600970526     C*
155700970526     C* TERZO GRUPPO DI 6 FUNZIONI
155800970526     C     FUNGRU        IFEQ      3
155900970526     C                   MOVEL     FUV(13)       FUN001
156000970526     C                   MOVEL     FUV(14)       FUN002
156100970526     C                   MOVEL     FUV(15)       FUN003
156200970526     C                   MOVEL     FUV(16)       FUN004
156300970526     C                   MOVEL     FUV(17)       FUN005
156400970526     C                   MOVEL     FUV(18)       FUN006
156500970526     C                   END
156600970526     C*
156700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
156800970526     C     FUNGRU        IFEQ      4
156900970526     C                   MOVEL     FUV(19)       FUN001
157000970526     C                   MOVEL     FUV(20)       FUN002
157100970526     C                   MOVEL     FUV(21)       FUN003
157200970526     C                   MOVEL     FUV(22)       FUN004
157300970526     C                   MOVEL     *BLANKS       FUN005
157400970526     C                   MOVEL     *BLANKS       FUN006
157500970526     C                   END
157600970526     C*
157700970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
157800970526     C                   MOVEL     *BLANKS       FUN007
157900970526     C                   MOVEL     *BLANKS       FUN008
158000970526     C*
158100970526     C     1             DO        24            I
158200970526     C                   MOVEL     FUD(I)        FUDRIG
158300970526     C     FUDFUN        IFEQ      '23'
158400970526     C     FUDATT        ANDEQ     'S'
158500970526     C                   MOVEL     FUDDES        FUN007
158600970526     C                   END
158700970526     C     FUDFUN        IFEQ      '24'
158800970526     C     FUDATT        ANDEQ     'S'
158900970526     C                   MOVEL     FUDDES        FUN008
159000970526     C                   END
159100970526     C                   END                                                    DO
159200970526     C*
159300970526     C                   ENDSR
159400970526     C*------------------------------------------------------------------------*
159500970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
159600970526     C*------------------------------------------------------------------------*
159700970526     C     OPZGES        BEGSR
159800970526     C*
159900970526     C* PULISCO CAMPI DI LAVORO
160000970526     C                   Z-ADD     0             W                 5 0
160100970526     C*
160200970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
160300970526     C* E CARICO TABELLA OPZIONI ATTIVE
160400970526     C     1             DO        24            I
160500970526     C                   MOVEL     OPD(I)        OPDRIG
160600970526     C*
160700970526     C* PULIZIA OPZIONI PRECEDENTI
160800970526     C                   MOVEL     *BLANKS       OPV(I)
160900970526     C*
161000970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
161100970526     C     OPDATT        IFEQ      'S'
161200970526     C                   ADD       1             W
161300970526     C                   MOVEL     OPDDES        OPV(W)
161400970526     C                   END
161500970526     C                   END                                                    DO WHILE
161600970526     C*
161700970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
161800970526     C* ALTRIMENTI DISATTIVO F23
161900970526     C     1             DO        24            I
162000970526     C                   MOVEL     FUD(I)        FUDRIG
162100970526     C*
162200970526     C     FUDFUN        IFEQ      '23'
162300970526     C     W             IFGT      8
162400970526     C                   MOVEL     'S'           FUDATT
162500970526     C                   ELSE
162600970526     C                   MOVEL     *BLANKS       FUDATT
162700970526     C                   END                                                    W GT 8
162800970526     C                   MOVEL     FUDRIG        FUD(I)
162900970526     C                   END
163000970526     C                   END                                                    DO WHILE
163100970526     C*
163200970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
163300970526     C     W             DIV       8             I
163400970526     C                   MVR                     Z                 5 0
163500970526     C*
163600970526     C     Z             IFNE      0
163700970526     C     I             ADD       1             Z
163800970526     C                   ELSE
163900970526     C                   Z-ADD     I             Z
164000970526     C                   END
164100970526     C*
164200970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
164300970526     C                   ADD       1             OPZGRU
164400970526     C     OPZGRU        IFGT      Z
164500970526     C     OPZGRU        ORLT      1
164600970526     C                   Z-ADD     1             OPZGRU
164700970526     C                   END
164800970526     C*
164900970526     C* PRIMO GRUPPO DI 8 OPZIONI
165000970526     C     OPZGRU        IFEQ      1
165100970526     C                   MOVEL     OPV(1)        OPZ001
165200970526     C                   MOVEL     OPV(2)        OPZ002
165300970526     C                   MOVEL     OPV(3)        OPZ003
165400970526     C                   MOVEL     OPV(4)        OPZ004
165500970526     C                   MOVEL     OPV(5)        OPZ005
165600970526     C                   MOVEL     OPV(6)        OPZ006
165700970526     C                   MOVEL     OPV(7)        OPZ007
165800970526     C                   MOVEL     OPV(8)        OPZ008
165900970526     C                   END
166000970526     C*
166100970526     C* SECONDO GRUPPO DI 8 OPZIONI
166200970526     C     OPZGRU        IFEQ      2
166300970526     C                   MOVEL     OPV(9)        OPZ001
166400970526     C                   MOVEL     OPV(10)       OPZ002
166500970526     C                   MOVEL     OPV(11)       OPZ003
166600970526     C                   MOVEL     OPV(12)       OPZ004
166700970526     C                   MOVEL     OPV(13)       OPZ005
166800970526     C                   MOVEL     OPV(14)       OPZ006
166900970526     C                   MOVEL     OPV(15)       OPZ007
167000970526     C                   MOVEL     OPV(16)       OPZ008
167100970526     C                   END
167200970526     C*
167300970526     C* TERZO GRUPPO DI 8 OPZIONI
167400970526     C     OPZGRU        IFEQ      3
167500970526     C                   MOVEL     OPV(17)       OPZ001
167600970526     C                   MOVEL     OPV(18)       OPZ002
167700970526     C                   MOVEL     OPV(19)       OPZ003
167800970526     C                   MOVEL     OPV(20)       OPZ004
167900970526     C                   MOVEL     OPV(21)       OPZ005
168000970526     C                   MOVEL     OPV(22)       OPZ006
168100970526     C                   MOVEL     OPV(23)       OPZ007
168200970526     C                   MOVEL     OPV(24)       OPZ008
168300970526     C                   END
168400970526     C*
168500970526     C                   ENDSR
168600970526     C*------------------------------------------------------------------------*
168700970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
168800970526     C*------------------------------------------------------------------------*
168900970526     C     OPZCON        BEGSR
169000970526     C*
169100970526     C* PULISCO CAMPI DI LAVORO
169200970526     C                   Z-ADD     0             I                 5 0
169300970526     C                   Z-ADD     0             W                 5 0
169400970526     C*
169500970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
169600970526     C     1             DO        24            I
169700970526     C                   MOVEL     OPD(I)        OPDRIG
169800970526     C*
169900970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
170000970526     C     OPZKEY        IFEQ      OPDOPZ
170100970526     C     OPZKEY        OREQ      OPDOP1
170200970526     C*
170300970526     C     OPDATT        IFEQ      'S'
170400970526     C                   MOVEL     OPDOPZ        OPZKEY
170500970526     C                   Z-ADD     I             W
170600970526     C                   END
170700970526     C                   END
170800970526     C*
170900970526     C                   END                                                    DO WHILE
171000970526     C*
171100970526     C* OPZIONE NON ATTIVA (ERRORE)
171200970526     C     W             IFEQ      0
171300970526     C*
171400970526     C* OPZIONE NON OK
171500970526     C                   Z-ADD     1             OPZOK
171600970526     C*
171700970526     C* EMISSIONE MESSAGGIO DI ERRORE
171800970526     C                   MOVEL     MSG(1)        DSMSMS
171900970526     C                   ELSE
172000970526     C*
172100970526     C* OPZIONE ATTIVA (PROCEDI ...)
172200970526     C*
172300970526     C     OPZKEY        IFEQ      ' 1'
172400970526     C                   EXSR      O01GES
172500970526     C                   END
172600970526     C     OPZKEY        IFEQ      ' 2'
172700060328     C                   EXSR      O02GES
172800970526     C                   END
172900970611     C     OPZKEY        IFEQ      ' 3'
173000970611     C                   EXSR      O03GES
173100970611     C                   END
173200970526     C     OPZKEY        IFEQ      ' 4'
173300970526     C                   EXSR      O04GES
173400970526     C                   END
173500970526     C     OPZKEY        IFEQ      ' 5'
173600970526     C                   EXSR      O05GES
173700970526     C                   END
173800970529     C     OPZKEY        IFEQ      '13'
173900970529     C                   EXSR      O13GES
174000970529     C                   END
174100970526     C                   END                                                    OPZ.NON ATTIVA
174200970526     C*
174300970526     C                   ENDSR
174400970526     C*------------------------------------------------------------------------*
174500970526     C* NOTE COSTRUZIONE SCHIERE
174600970526     C*------------------------------------------------------------------------*
174700970526     C*
174800970526     C* -MSG-MESSAGGI
174900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
175000970526     C* (NON TOCCARE MSG,1 E MSG,2)
175100970526     C*
175200970526     C* -CMD-COMANDI
175300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
175400970526     C*
175500970526     C* -OPD-OPZIONI
175600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
175700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
175800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
175900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
176000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
176100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
176200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
176300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
176400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
176500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
176600970526     C*
176700970526     C* -FUD-FUNZIONI
176800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
176900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
177000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
177100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
177200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
177300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
177400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
177500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
177600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
177700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
177800970526     O*------------------------------------------------------------------------*
177900970526** DEC - DECODIFICHE
178000970526    GESTIONE                                                     1
178100970530INTERROGAZIONE                                                   2
178200970530     SCELTA                                                      3
1783009705264
1784009705265
178500970526** MSG - MESSAGGI
178600110502TNTLP0R- Opzione non gestita                                                   1
178700110502TNTLP0R- Tasto funzionale non gestito                                          2
178800110502TNTLP0R- Data di riferimento errata
178900110505TNTLP0R-
179000110502TNTLP0R-                                                                       5
179100110505TNTLP0R- Accettato solo un parametro di inizio lista                           6
179200110502TNTLP0R- 7                                                                     7
179300110502TNTLP0R- 8                                                                     8
179400110502TNTLP0R- 9                                                                     9
179500110502TNTLP0R- 10                                                                   10
179600110502TNTLP0R- 11                                                                   11
179700110502TNTLP0R- 12                                                                   12
179800110502TNTLP0R- 13                                                                   13
179900110502TNTLP0R- 14                                                                   14
180000110502TNTLP0R- 15                                                                   15
180100110502TNTLP0R- 16                                                                   16
180200110502TNTLP0R- 17                                                                   17
180300110502TNTLP0R- 18                                                                   18
180400110502TNTLP0R- 19                                                                   19
180500110502TNTLP0R- 20                                                                   20
180600970526** OPD - OPZIONI
180700970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
180800060315 2=Modifica        -2 -     (S/N)=S                        opz. 02¬
180900060315 3=Duplica         -3 -     (S/N)=S                        opz. 03¬
181000060315 4=Annullamento    -4 -     (S/N)=S                        opz. 04¬
181100060315 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
181200970526                   -6 -     (S/N)=N                        opz. 06¬
181300060315                   -7 -     (S/N)=N                        opz. 07¬
181400970527                   -8 -     (S/N)=N                        opz. 08¬
181500970527                   -9 -     (S/N)=N                        opz. 09¬
181600970527                   -10-     (S/N)=N                        opz. 10¬
181700970529                   -11-     (S/N)=N                        opz. 11¬
181800970529                   -12-     (S/N)=N                        opz. 12¬
181900110503                   -13-     (S/N)=N                        opz. 13¬
182000970526                   -  -     (S/N)=N                        opz. 14¬
182100970526                   -  -     (S/N)=N                        opz. 15¬
182200970526                   -  -     (S/N)=N                        opz. 16¬
182300970526                   -  -     (S/N)=N                        opz. 17¬
182400970526                   -  -     (S/N)=N                        opz. 18¬
182500970526                   -  -     (S/N)=N                        opz. 19¬
182600970526                   -  -     (S/N)=N                        opz. 20¬
182700970526                   -  -     (S/N)=N                        opz. 21¬
182800970526                   -  -     (S/N)=N                        opz. 22¬
182900970526                   -  -     (S/N)=N                        opz. 23¬
183000970526                   -  -     (S/N)=N                        opz. 24¬
183100970526** FUD - FUNZIONI
183200970526                   -        (S/N)=N                        funz.01¬
183300970526                   -        (S/N)=N                        funz.02¬
183400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
183500970526                   -        (S/N)=N                        funz.04¬
183600970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
183700060315F06=Inserimento    -        (S/N)=S                        funz.06¬
183800970526                   -        (S/N)=N                        funz.07¬
183900970527                   -        (S/N)=N                        funz.08¬
184000110503F09=Altri Dati     -        (S/N)=S                        funz.09¬
184100970527                   -        (S/N)=N                        funz.10¬
184200970526                   -        (S/N)=N                        funz.11¬
184300970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
184400110503                   -        (S/N)=N                        funz.13¬
184500970526                   -        (S/N)=N                        funz.14¬
184600970526                   -        (S/N)=N                        funz.15¬
184700970526                   -        (S/N)=N                        funz.16¬
184800970526                   -        (S/N)=N                        funz.17¬
184900970526                   -        (S/N)=N                        funz.18¬
185000970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
185100970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
185200970527                   -        (S/N)=N                        funz.21¬
185300970527                   -        (S/N)=N                        funz.22¬
185400970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
185500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
