000100970526     H*------------------------------------------------------------------------*
000200060907     H* GESTIONE file di log tellus - upload
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800060907     FTIS762D   CF   E             WORKSTN INFDS(DEVDS1)
000900060907     F                                     SFILE(S762D05:SFLULT)
001000060907     F                                     SFILE(S762DMS:MSGULT)
001100060911     Ffilte01l  uF   E           K DISK
001200060907     Ffilte02l  IF   E           K DISK    rename(filte000:filte002)
001300060315     FAZorg01l  IF   E           K DISK
001400970526     D*------------------------------------------------------------------------*
001500970526     D* SCHIERE
001600970526     D*------------------------------------------------------------------------*
001700970526     D* DECODIFICHE
001800970526     D*------------
001900970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* MESSAGGI
002200970526     D*------------
002300970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* DESCRIZIONI OPZIONI
002600970526     D*------------
002700970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI TASTI FUNZIONALI
003000970526     D*------------
003100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* OPZIONI DA CARICARE A VIDEO
003400970526     D*------------
003500970526     D OPV             S             19    DIM(24)
003600970526     D*------------
003700970526     D* FUNZIONI DA CARICARE A VIDEO
003800970526     D*------------
003900970527     D FUV             S             19    DIM(24)
004000970526     D*------------------------------------------------------------------------*
004100970526     D* INPUT
004200970526     D*------------------------------------------------------------------------*
004300970526     D* RIDEFINIZIONE OPZIONI VIDEO
004400970526     D OPDDS           DS
004500970526     D  OPDOPZ                 1      2
004600970526     D  OPDDE1                 3     19
004700970526     D  OPDOP1                21     22
004800970526     D  OPDFI0                23     34
004900970526     D  OPDATT                35     35
005000970526     D  OPDFI1                36     80
005100970526     D  OPDDES                 1     19
005200970526     D  OPDRIG                 1     80
005300970526     D*------------
005400970526     D* RIDEFINIZIONE FUNZIONI VIDEO
005500970526     D FUDDS           DS
005600970526     D  FUDDE1                 1      1
005700970526     D  FUDFUN                 2      3
005800970526     D  FUDDE2                 4     19
005900970526     D  FUDFI0                20     34
006000970526     D  FUDATT                35     35
006100970526     D  FUDFI1                36     80
006200970526     D  FUDDES                 1     19
006300970526     D  FUDRIG                 1     80
006400970526     D*------------
006500970526     D* DS GESTIONE TASTI FUNZIONALI
006600970526     D DSKEY         E DS
006700970526     D DEVDS1          DS
006800970526     D  KEY                  369    369
006900970526     D*------------
007000970526     D* DS "XMSG" - GESTIONE MESSAGGI
007100970526     D*------------
007200970526     D DSMSG         E DS
007300970526     D*------------
007400020919     D TIBS34DS      E DS                                                       *Profili utente
007500020919     D DDATIUTE      E DS                                                       *Dati utente
007600020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007700970527     D*------------------
007800970527     D WLBDA8          DS                  INZ
007900970527     D  G08DAT                 1      8  0
008000970527     D  G08INV                 9     16  0
008100970527     D  G08ERR                17     17
008200970527     D  G08TGI                18     22  0
008300970526     D*------------
008400970526     D KPJBA         E DS
008500070207     D DLTEFLO       E DS
008600060313     d
008700060313     d Dataiso         s               d   datfmt(*iso)
008800060313     d Dataeur         s               d   datfmt(*eur)
008900060329     d Datadmy         s               d   datfmt(*dmy)
009000060908     d kdat            s                   Like(ltedat)
009100060330     d depord          s                   Like(vidord)
009200060908     d saving          s                   Like(viding)
009300070424     d w0050           s              5  0
009400060404     d opzione         s              2
009500060908     D Cinelabora      C                   CONST('    In   Elaborazione')
009600970526     C*------------------------------------------------------------------------*
009700970526     C* MAIN LINES
009800970526     C*------------------------------------------------------------------------*
009900970526     C*
010000970526     C* SE NON CI SONO ERRORI, PROSEGUE
010100970526IF  1C     ERRL00        IFNE      '1'
010200970526     C*
010300970526     C* CARICA IL SFILE
010400970526     C                   EXSR      POSSFL
010500970526     C*
010600970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010700970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011000970526     C*
011100970526     C* REIMPOSTA ERRORE DI PROCERURA
011200970526     C                   MOVEL     '0'           ERRL00
011300970526     C*
011400970526     C* EMETTE IL SFILE
011500970526     C                   EXSR      OUTSFL
011600970526     C*
011700970526     C* GESTIONE TASTI FUNZIONALI
011800970526     C                   EXSR      FUNCON
011900970526     C*
012000970526E   2C                   ENDDO
012100970527     C*
012200970527     C* CI SONO ERRORI IN ENTRATA
012300970527X   1C                   ELSE
012400060313     C                   MOVEL     '1'           dp0ERR
012500060313     C                   MOVEL     DSMSMS        dp0MSG
012600970526E   1C                   ENDIF
012700970526     C* FINE PGM
012800970526     C                   SETON                                        LR
012900970526     C*------------------------------------------------------------------------*
013000970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
013100970526     C*------------------------------------------------------------------------*
013200970526     C     F03GES        BEGSR
013300970526     C*
013400970526     C* IMPOSTA "PREMUTO F03"
013500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
013600970526     C*
013700970526     C                   ENDSR
013800970526     C*------------------------------------------------------------------------*
013900970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
014000970526     C*------------------------------------------------------------------------*
014100970526     C     F05GES        BEGSR
014200970526     C*
014300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
014400970526     C*
014500970526     C                   ENDSR
014600970526     C*------------------------------------------------------------------------*
014700970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
014800970526     C*------------------------------------------------------------------------*
014900970526     C     F06GES        BEGSR
015000970529     C*
015100970529     C                   ENDSR
015200060908     C*------------------------------------------------------------------------*
015300060908     C* F12GES - GESTIONE F12 -> RITORNO
015400060908     C*------------------------------------------------------------------------*
015500060908     C     F12GES        BEGSR
015600060908     C*
015700060908     C* IMPOSTA "PREMUTO F12"
015800060908     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
015900060908     C*
016000060908     C                   ENDSR
016100970526     C*------------------------------------------------------------------------*
016200970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
016300970526     C*------------------------------------------------------------------------*
016400970526     C     F23GES        BEGSR
016500970526     C*
016600970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
016700970526     C*
016800970526     C                   ENDSR
016900970526     C*------------------------------------------------------------------------*
017000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
017100970526     C*------------------------------------------------------------------------*
017200970526     C     F24GES        BEGSR
017300970526     C*
017400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
017500970526     C*
017600970526     C                   ENDSR
017700970526     C*------------------------------------------------------------------------*
017800970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
017900970526     C*------------------------------------------------------------------------*
018000970526     C     RUPGES        BEGSR
018100970526     C*
018200970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
018300970526     C*
018400970526     C                   ENDSR
018500970526     C*------------------------------------------------------------------------*
018600970526     C* ENTGES - GESTIONE ENTER -> INVIO
018700970526     C*------------------------------------------------------------------------*
018800970526     C     ENTGES        BEGSR
018900970526     C*
019000970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
019100970526     C                   EXSR      NEWPOS
019200970526     C*
019300970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
019400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019500970526IF  2C     SWPOSI        IFNE      0
019600970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019700970526X   2C                   ELSE
019800970526IF  3C     SFLULT        IFNE      0
019900970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
020000970526     C*
020100970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
020200970526IF  4C     SWMODI        IFNE      0
020300970526     C                   EXSR      POSSFL
020400970526E   4C                   ENDIF
020500970526E   3C                   ENDIF
020600970526E   2C                   ENDIF
020700970526E   1C                   ENDIF
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
021200970526     C*------------------------------------------------------------------------*
021300970526     C     O02GES        BEGSR
021400970526     C*
021500970526     C                   ENDSR
021600970611     C*------------------------------------------------------------------------*
021700970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
021800970611     C*------------------------------------------------------------------------*
021900970611     C     O03GES        BEGSR
022000970611     C                   ENDSR
022100970526     C*------------------------------------------------------------------------*
022200060911     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO  log
022300970526     C*------------------------------------------------------------------------*
022400970526     C     O04GES        BEGSR
022500060911     c                   eval      saving=viding
022600060911     c
022700060911     c* Se record chiuso non posso annullare
022800060911     c                   if        v5hdatu>0
022900060911     C                   MOVEL     MSG(5)        DSMSMS
023000060911     C                   MOVEL     '1'           ERRL00
023100060911     C                   SETON                                        8082
023200060911     c                   goto      endo04ges
023300060911     c                   endif
023400060911     c
023500060911     c                   seton                                        04
023600060911     c* Carico i dati nella videata
023700060911     c                   exsr      CARVID10
023800060911     c* salvo la dicitura
023900060911     c                   eval      viding=dec(4)
024000060911     c
024100060911     c     for04         tag
024200060911     c                   write     s762d01
024300060911     c                   exfmt     s762d10
024400060911     c* Per F3 - fine
024500060911     c   kc              exsr      F03GES
024600060911     c   kc              goto      endo04ges
024700060911     c* Per F12- ritorno
024800060911     c   kl              goto      endo04ges
024900060911     c
025000060911     c* ENTER riemetto videata
025100060911     c  nkf              goto      For04
025200060911     c
025300060911     c* F6 - annulla log
025400060911     c                   if        *inkf
025500060911     c     v10cidb       chain     filte01l
025600060911     c                   if        %found(filte01l)
025700060911     c                   eval      ltehdl='ANNULLATO '
025800060911     C                   EVAL      LTEDATU=DATCOR
025900060911     C                   time                    lteorau
026000060911     c                   update    filte000
026100060911     c*
026200060911     c* ricarico sfl
026300060911     C                   EVAL      swmodi=1
026400060911     c                   endif
026500060911     c                   endif
026600060911     C*
026700060911     C     ENDO04ges     tag
026800060911     c
026900060911     c                   eval      viding=saving
027000060911     c                   setoff                                       04
027100970526     C                   ENDSR
027200970526     C*------------------------------------------------------------------------*
027300970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
027400970526     C*------------------------------------------------------------------------*
027500970526     C     O05GES        BEGSR
027600060908     c* Carico i dati nella videata
027700060908     c                   exsr      CARVID10
027800060908     c* salvo la dicitura
027900060908     c                   eval      saving=viding
028000060908     c                   eval      viding=dec(2)
028100060908     c
028200060908     c                   write     s762d01
028300060908     c                   exfmt     s762d10
028400060908     c* Per F3 - fine
028500060908     c   kc              exsr      F03GES
028600060908     c   kc              goto      endo05ges
028700060908     c* a qualsiasi tasto torno al sfl
028800970526     C*
028900060908     C     ENDO05ges     tag
029000060908     c
029100060908     c                   eval      viding=saving
029200060908     c                   endsr
029300060908     C*------------------------------------------------------------------------*
029400060911     C* O07GES - GESTIONE F07 -> RIpristino  LOG
029500060908     C*------------------------------------------------------------------------*
029600060908     C     O07GES        BEGSR
029700060911     c                   eval      saving=viding
029800060911     c
029900060911     c                   if        v5hdatu=0  or
030000060911     c                             v5hhdl<>'ANNULLATO '
030100060911     C                   MOVEL     MSG(6)        DSMSMS
030200060911     C                   MOVEL     '1'           ERRL00
030300060911     C                   SETON                                        8082
030400060911     c                   goto      endo07ges
030500060911     c                   endif
030600060911     c
030700060911     c                   seton                                        04
030800060911     c* Carico i dati nella videata
030900060911     c                   exsr      CARVID10
031000060911     c* salvo la dicitura
031100060911     c                   eval      viding=dec(5)
031200060911     c
031300060911     c     for07         tag
031400060911     c                   write     s762d01
031500060911     c                   exfmt     s762d10
031600060911     c* Per F3 - fine
031700060911     c   kc              exsr      F03GES
031800060911     c   kc              goto      endo07ges
031900060911     c* Per F12- ritorno
032000060911     c   kl              goto      endo07ges
032100060911     c
032200060911     c* ENTER riemetto videata
032300060911     c  nkf              goto      For07
032400060911     c
032500060911     c* F6 - annulla log
032600060911     c                   if        *inkf
032700060911     c     v10cidb       chain     filte01l
032800060911     c                   if        %found(filte01l)
032900060911     c                   eval      ltehdl='          '
033000060911     C                   clear                   ltedatu
033100060911     C                   clear                   lteorau
033200060911     c                   update    filte000
033300060911     c* ricarico sfl
033400060911     C                   EVAL      swmodi=1
033500060911     c                   endif
033600060911     c                   endif
033700060911     C*
033800060911     C     ENDO07ges     tag
033900060911     c
034000060911     c                   eval      viding=saving
034100060911     c                   setoff                                       04
034200060908     C                   ENDSR
034300060908     C*------------------------------------------------------------------------*
034400060908     C* CARVID10 - carico i dati nella videata dettaglio di filte
034500060908     C*------------------------------------------------------------------------*
034600060908     C     CARVID10      BEGSR
034700060911     C                   SETOFF                                       03
034800060908     c                   eval      v10cpoe=v5cpoe
034900060908     c     v10cpoe       chain     azorg01l
035000060908     c                   if        %found(azorg01l)
035100060908     c                   movel     orgdes        v10dpoe
035200060908     c                   else
035300060908     c                   movel     *all'?'       v10dpoe
035400060908     c                   endif
035500060908     c                   eval      v10cidb =v5cidb
035600060908     c                   eval      v10cprg =v5hprg
035700060908     c                   eval      v10chdl =v5hhdl
035800060908     c                   if        v5cdat>0
035900060908     c     *dmy          movel     v5cdat        dataeur
036000060908     c                   movel     dataeur       v10cdat
036100060908     c                   else
036200060908     c                   clear                   v10cdat
036300060908     c                   endif
036400060908     c                   eval      v10cora =v5cora
036500060908     c
036600060908     c                   eval      v10corau=v5Horau
036700060908     c                   if        v5hdatu>0
036800060908     c                   setoff                                       01
036900060908     c     *dmy          movel     v5Hdatu       dataeur
037000060908     c                   movel     dataeur       v10cdatu
037100060908     c                   else
037200060908     c                   clear                   v10cdatu
037300060908     c                   seton                                        01
037400060908     c                   endif
037500060908     c
037600060908     c                   eval      v10cnsps=v5cnsps
037700060908     c                   eval      v10cnspr2=v5Hnspr
037800060908     c                   eval      v10cnspr =v5hnspr
037900060911     c
038000060911     C* SE ANNULLATO  ACCENDO LO 03
038100060911     C                   if        v10chdl='ANNULLATO '
038200060911     C                   SETON                                        03
038300060911     C                   ENDIF
038400060911     c
038500060911     c* solo se dati ricevuti e recod non annullato
038600060911     c                   if        v5Hdatu<>0  AND NOT *IN03
038700060908     c                   eval      v10cdiffe=v5cnsps-v5hnspr
038800060908     c                   else
038900060908     c                   clear                   v10cdiffe
039000060908     c                   endif
039100060908     c                   eval      v10cnspeb=v5hnspeb
039200060908     c                   eval      v10cnspet=v5hnspet
039300060911     c                   eval      v10cnspew=v5hnspew
039400060908     c                   eval      v10cnspg =v5hnspg
039500060911     c                   eval      v10cnspok=v5hnspok
039600060908     c*
039700060908     c                   ENDSR
039800970526     C*------------------------------------------------------------------------*
039900970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
040000970526     C*------------------------------------------------------------------------*
040100970526     C     NEWPOS        BEGSR
040200970526     C*
040300970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
040400970526     C*         1= RICHIESTO POSIZIONAMENTO
040500970526     C                   Z-ADD     0             SWPOSI            7 0
040600970526     C*
040700970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
040800970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
040900970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
041000970526     C                   Z-ADD     0             SWMODI            1 0
041100970530     C*
041200970530     C* REIMPOSTA INDICATORI DI ERRORE
041300060313     C                   SETOFF                                       808281
041400970526     C*
041500060403     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
041600060907     C     VIDsel        ifNE      DEPsel
041700060313     C     VIDPpo        ORNE      DEPppo
041800060907     C     viddat        ORNE      depdat
041900060330     C     VIDord        ORNE      DEPord
042000970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
042100970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
042200970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
042300060907     C                   MOVEL     VIDsel        DEPsel
042400060313     C                   MOVEL     VIDppo        DEPppo
042500060907     C                   MOVEL     viddat        depdat
042600060330     C                   MOVEL     VIDord        DEPord
042700970526E   2C                   ENDIF
042800970526E   1C                   ENDIF
042900970526     C*
043000970526     C                   ENDSR
043100970526     C*------------------------------------------------------------------------*
043200970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
043300970526     C*------------------------------------------------------------------------*
043400970526     C     SETFIL        BEGSR
043500060908     c                   setoff                                       99
043600970529     C*
043700060908     c* Ordinamento per P.O.
043800060908     c                   if        vidord='P'
043900060908     C     vidppo        SETLL     filte02l
044000060330     c                   else
044100060908     c* Ordinamento per I.D.
044200060908     C     *hival        SETLL     filte01l
044300060330     c                   endif
044400060313E   1C
044500970526     C* LETTURA RECORD
044600970526     C                   EXSR      LETFIL
044700970526     C*
044800970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
044900970526IF  1C     *IN99         IFEQ      *ON
045000970526     C                   Z-ADD     1             SFLEOF
045100970526E   1C                   ENDIF
045200970526     C*
045300970526     C                   ENDSR
045400970526     C*------------------------------------------------------------------------*
045500970526     C* LETFIL - LETTURA RECORD
045600970526     C*------------------------------------------------------------------------*
045700970526     C     LETFIL        BEGSR
045800970526     C*
045900970526     C* LEGGE FINO A:
046000970526     C*     - FINE FILE
046100970526     C*     - TROVATO RECORD VALIDO
046200970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
046300970526DO  1C     *IN99         DOWEQ     *OFF
046400970526     C     WRECOK        ANDEQ     'N'
046500970529     C*
046600060908     C* Ordinamento per p.o.
046700060908     c                   if        vidord='P'
046800060908     c                   if        vidppo>0
046900060908     C     vidppo        READe     filte02l                               99
047000060908     c                   else
047100060908     C                   READ      filte02l                               99
047200060908     c                   endif
047300060908     c
047400060330     c                   else
047500060908     c* ordinamento per IDB
047600060911     C                   READ(N)   filte01l                               99
047700060330     c                   endif
047800970526     C*
047900970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
048000970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
048100970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
048200970530X   2C                   ELSE
048300970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
048400970530E   2C                   ENDIF
048500970526E   1C                   ENDDO
048600970526     C*
048700970526     C                   ENDSR
048800970526     C*------------------------------------------------------------------------*
048900970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
049000970526     C*------------------------------------------------------------------------*
049100970526     C     CHKREC        BEGSR
049200060908     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
049300060908     C* Se impostata data controllo
049400060908     c                   if        kdat>0 and ltedat<>kdat
049500060908     C                   MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
049600060329     c                   endif
049700060908     c*
049800060908     c* Se impostato p.o. controllo per ordinamento per IDB
049900060908     c                   if        vidord='I' and vidppo>0   and
050000060908     c                             vidppo<>ltepoe
050100060908     C                   MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
050200060908     c                   endif
050300060911     c
050400060908     c* Tipo dati da ricevere ricevuti o tutti
050500060908     c                   if        vidsel='1' and  ltenspr>0
050600060908     C                   MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
050700060908     c                   endif
050800060908     c                   if        vidsel='2' and  ltenspr=0
050900060908     C                   MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
051000060908     c                   endif
051100070207     c
051200070207     c* Se record x log d test faccio vedere solo se utente EDP
051300070207     c                   eval      DLTEFLO = lteflo
051400070207     c                   if        §FLOTST = 'S'
051500070207     c                   if        %subst(KNMUS:1:3) <> 'EDP'
051600070207     C                   MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
051700070207     c                   endif
051800070207     c                   endif
051900060908     c*
052000970526     C                   ENDSR
052100970526     C*------------------------------------------------------------------------*
052200970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
052300970526     C*------------------------------------------------------------------------*
052400970526     C     IRESFL        BEGSR
052500970529     C*
052600970529     C                   MOVEL     *BLANKS       OPZKEY
052700060908     c
052800060908     c                   eval      v5cpoe=ltepoe
052900060908     c                   eval      v5cidb=lteidb
053000060908     c* download
053100060908     c                   if        ltedat>0
053200060908     c     *iso          movel     ltedat        dataiso
053300060329     c                   movel     dataiso       datadmy
053400060908     c                   movel     datadmy       v5cdat
053500060908     c                   else
053600060908     c                   clear                   v5cdat
053700060908     c                   endif
053800060908     c
053900060908     c                   eval      v5cora=lteora
054000060908     c                   eval      v5hprg=lteprg
054100060908     c                   eval      v5cnsps=ltensps
054200060908     c* upload
054300060908     c                   if        ltedatu>0
054400060908     c                   setoff                                       02
054500060908     c     *iso          movel     ltedatu       dataiso
054600060908     c                   movel     dataiso       datadmy
054700060908     c                   movel     datadmy       v5hdatu
054800060908     c                   else
054900060908     c                   clear                   v5hdatu
055000060908     c                   seton                                        02
055100060908     c                   endif
055200060908     c                   movel     *in02         in02
055300060908     c
055400060908     c                   eval      v5horau=lteorau
055500060908     c                   eval      v5hhdl=ltehdl
055600060313     c
055700060908     c                   eval      v5hnspr=ltenspr
055800060908     c
055900060908     c                   if        v5hdatu>0
056000060908     c                   eval      v5cupload=%editw(v5hdatu: '  /  /  ')+ ' ' +
056100070620     c                             %editw(v5horau: '0 :  :  ') + ' ' +
056200060908     c                             %editc(v5hnspr: '2')
056300060908     c                   else
056400060908     c                   eval      v5cupload=Cinelabora
056500060908     c                   endif
056600060908     c* dati spedizioni
056700060911     c                   eval      v5hnspew=ltenspew
056800060911     c                   eval      v5hnspok=(ltenspr-ltenspeb-ltenspet)
056900060908     c                   eval      v5hnspeb=ltenspeb
057000060908     c                   eval      v5hnspet=ltenspet
057100060908     c                   eval      v5hnspg =ltenspg
057200060911     c* Se log annullato lo scrivo
057300060911     c                   if        ltehdl='ANNULLATO '
057400060911     C                   EVAL      V5CSPEDIZ='ANNULLATO LOG'
057500060911     c                   seton                                        03
057600060911     C                   ELSE
057700070424     c**                 eval      v5cspediz=' '+%editc(v5hnspok: 'Z')+ '  '+
057800070424     c**                           %editc(v5hnspew: 'Z')
057900070424     c* Al posto delle spedizioni ricevute con o senza errore, metto
058000070424     c*  le spedizioni con e senza giro (quelle senza giro somprendono
058100070424     c*  eventuali errori
058200070424     c                   eval      w0050=v5hnspr-v5hnspg
058300070424     c                   eval      v5cspediz=' '+%editc(v5hnspg: 'Z')+ '  ' +
058400070424     c                             %editc(w0050: 'Z')
058500060911     c                   setoff                                       03
058600060911     C                   endif
058700060911     c
058800060911     c                   movel     *in03         in03
058900060908     c
059000970526     C                   ENDSR
059100970526     C*------------------------------------------------------------------------*
059200970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
059300970526     C*------------------------------------------------------------------------*
059400970526     C     CHKPAR        BEGSR
059500060907     c* controllo data
059600060907     c                   clear                   kdat
059700060907     c                   if        viddat>0
059800060907     C                   Z-ADD     viddat        G08DAT
059900060313     C                   Z-ADD     *ZEROS        G08INV
060000060313     C                   MOVEL     '0'           G08ERR
060100060313     C                   CALL      'XSRDA8'
060200060313     C                   PARM                    WLBDA8
060300060313     c                   if        g08err='1'
060400060313     C                   MOVEL     MSG(3)        DSMSMS
060500060313     C                   MOVEL     '1'           ERRL00
060600060313     C                   SETON                                        8182
060700060313E   2C                   else
060800060907     c                   z-add     g08dat        viddat
060900060907     C                   Z-ADD     G08INV        kdat              8 0          *DATA CORRENTE AA/M/
061000060313     c                   endif
061100060313     c                   endif
061200060907     c* controllo p.o.
061300060907     c                   if        vidppo>0 and errl00='0'
061400060907     c     vidppo        chain     azorg01l
061500060907     c                   if        not %found(azorg01l) or
061600060907     c                             orgfva<>' '  or
061700060907     c                             (orgfag<>'A' and orgfag<>'F')
061800060907     C                   MOVEL     MSG(4)        DSMSMS
061900060907     C                   MOVEL     '1'           ERRL00
062000060907     C                   SETON                                        8082
062100060907     c                   endif
062200060907     c                   endif
062300970526     C*
062400970526     C                   ENDSR
062500970527     C*------------------------------------------------------------------------*
062600970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
062700970527     C*------------------------------------------------------------------------*
062800970527     C     CARTAB        BEGSR
062900970527     C*
063000970527     C                   ENDSR
063100970526     C*------------------------------------------------------------------------*
063200970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
063300970526     C*------------------------------------------------------------------------*
063400970526     C     CHKFOP        BEGSR
063500970626     C*
063600970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
063700060404IF  1C     opzione       IFEQ      '01'
063800970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
063900970626     C                   MOVEL     'S'           OPDATT
064000970626     C                   MOVEL     OPDRIG        OPD(1)
064100970626X   1C                   ELSE
064200970626     C* FUNZIONI
064300970626E   1C                   ENDIF
064400970626     C*
064500060907     C* SE L'UTENTE E' IN GESTIONE, ABILITA solo per EDP               I MODIFICA
064600060404IF  1C     opzione       IFEQ      '02'
064700060907     c
064800060907     c* Se utente EDP abilito annullamento e ripristino
064900060907     c                   if        %subst(knmus:1:3)='EDP'
065000060907     C                   MOVEL     OPD(4)        OPDRIG                         *INTERROGAZIONE
065100060907     C                   MOVEL     'S'           OPDATT
065200060907     C                   MOVEL     OPDRIG        OPD(4)
065300060907     C                   MOVEL     OPD(7)        OPDRIG                         *INTERROGAZIONE
065400060907     C                   MOVEL     'S'           OPDATT
065500060907     C                   MOVEL     OPDRIG        OPD(7)
065600070424     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
065700070424     C                   MOVEL     'S'           OPDATT
065800070424     C                   MOVEL     OPDRIG        OPD(5)
065900060907     c                   else
066000060907     C                   MOVEL     OPD(4)        OPDRIG                         *INTERROGAZIONE
066100060907     C                   MOVEL     'N'           OPDATT
066200060907     C                   MOVEL     OPDRIG        OPD(4)
066300060907     C                   MOVEL     OPD(7)        OPDRIG                         *INTERROGAZIONE
066400060907     C                   MOVEL     'N'           OPDATT
066500060907     C                   MOVEL     OPDRIG        OPD(7)
066600070424     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
066700070424     C                   MOVEL     'N'           OPDATT
066800070424     C                   MOVEL     OPDRIG        OPD(5)
066900060907     c                   endif
067000060327    1C                   ENDIF
067100060331     c
067200060404IF  1C     opzione       IFEQ      '05'
067300060907     C                   MOVEL     OPD(4)        OPDRIG                         *MODIFICA
067400060331     C                   MOVEL     'N'           OPDATT
067500060907     C                   MOVEL     OPDRIG        OPD(4)
067600060907     C                   MOVEL     OPD(7)        OPDRIG                         *DUPLICA
067700060331     C                   MOVEL     'N'           OPDATT
067800060907     C                   MOVEL     OPDRIG        OPD(7)
067900070424     c                   if        %subst(knmus:1:3)='EDP'
068000070424     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
068100070424     C                   MOVEL     'S'           OPDATT
068200070424     C                   MOVEL     OPDRIG        OPD(5)
068300070424     c                   else
068400070424     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
068500070424     C                   MOVEL     'N'           OPDATT
068600070424     C                   MOVEL     OPDRIG        OPD(5)
068700060331    1C                   ENDIF
068800070424    1C                   ENDIF
068900970526     C*
069000970526     C* GESTIONE OPZIONI A VIDEO
069100970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
069200970526     C                   EXSR      OPZGES
069300970526     C*
069400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
069500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
069600970526     C                   EXSR      FUNGES
069700970526     C*
069800970526     C                   ENDSR
069900020919     C*--------------------------------------------------------------------------------------------*
070000020919     C* REPERISCE I DATI UTENTE
070100020919     C*--------------------------------------------------------------------------------------------*
070200020919     C     REPDATIUTE    BEGSR
070300020919     C*
070400020919     C* INIZIALIZZA VARIABILI DI WRK
070500020919     C                   CLEAR                   TIBS34DS
070600020919     C                   CLEAR                   AZUTEDS
070700020919     C                   CLEAR                   DDATIUTE
070800020919     C*
070900020919     C     *DTAARA       DEFINE    §azute        azuteds
071000020919     C     *DTAARA       DEFINE    §datiute      ddatiute
071100020919     C                   IN(E)     *DTAARA
071200020919if  1C                   IF        %Error
071300020919     c                   EVAL      I34Tla = 'L'
071400020919     C                   CALL      'TIBS34R'
071500020919     C                   PARM                    Tibs34Ds
071600020919     C                   IN        *DTAARA
071700020919e   1C                   ENDIF
071800020919     C*
071900020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
072000020919     C                   MOVEL(P)  RSUT          DSFIRS           20
072100020919     C*
072200020919     C                   ENDSR
072300970526     C*------------------------------------------------------------------------*
072400970526     C* *INZSR - OPERAZIONI INIZIALI
072500970526     C*------------------------------------------------------------------------*
072600970526     C     *INZSR        BEGSR
072700970526     C*---
072800970526     C* RICEVIMENTO PARAMETRI
072900970526     C*---
073000970526     C     *ENTRY        PLIST
073100970612     C                   PARM                    KPJBA
073200970526     C*
073300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
073400970526     C     XMSGPA        PLIST
073500970526     C                   PARM                    DSMSMS
073600970526     C                   PARM                    DSMSPG
073700970526     C                   PARM                    DSMSKE
073800970526     C                   PARM                    DSMSER
073900970526     C*
074000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
074100970526     C                   CALL      'XKEY'
074200970526     C                   PARM                    DSKEY
074300060907     C*---
074400970526     C* VARIABILI NON RIFERITE AL DATA BASE
074500970526     C*---
074600970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
074700970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
074800970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
074900970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
075000970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
075100970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
075200970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
075300970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
075400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
075500970526     C*---
075600970526     C* VARIABILI DI CONTROLLO
075700970526     C*---
075800970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
075900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
076000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
076100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
076200970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
076300060908     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
076400970526     C*
076500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
076600060907     C                   MOVEL     'TIS762R'     DSMSPG
076700060907     C                   MOVEL     'TIS762R'     MSGQUE
076800970526     C                   MOVEL     *BLANKS       MSGKEY
076900970526     C                   MOVEL     *BLANKS       DSMSMS
077000970526     C                   MOVEL     *BLANKS       DSMSKE
077100970526     C                   Z-ADD     *ZEROS        DSMSER
077200060330     c
077300060907     C     *LIKE         DEFINE    ltedat        depdat                         *DEPOSITI
077400060907     C     *LIKE         DEFINE    vidsel        DEPsel
077500060907     C     *LIKE         DEFINE    ltepoe        DEPppo
077600970526     C*---
077700970526     C* CHIAVI DI LETTURA
077800970526     C*---
077900970527     C* REPERIMENTO DATI UTENTE
078000020919     C                   EXSR      REPDATIUTE
078100970527     C*
078200970527     C* CARICA LE TABELLE OCCORRENTI
078300970527     C                   EXSR      CARTAB
078400970526     C*
078500970526     C* CALCOLA LA DATA CORRENTE
078600970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
078700970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
078800970526     C                   Z-ADD     WN8           G08DAT
078900970526     C                   Z-ADD     *ZEROS        G08INV
079000970526     C                   MOVEL     '0'           G08ERR
079100970526     C                   CALL      'XSRDA8'
079200970526     C                   PARM                    WLBDA8
079300970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
079400970526     C*
079500970526     C* IMPOSTA I PARAMETRI DI INGRESSO
079600070202     c                   eval      vidord='I'
079700060921     c                   if        dutpou=46
079800060921     c                   clear                   vidppo
079900060921     c                   else
080000060908     c                   eval      vidppo=dutpou
080100060921     c                   endif
080200060313     c
080300970526     C                   EXSR      CHKPAR
080400060907     c
080500970526IF  1C     ERRL00        IFEQ      '1'
080600060908     C                   MOVEL     '1'           Dp0ERR            1
080700060908     C                   MOVEL     DSMSMS        Dp0MSG           78
080800970526X   1C                   ELSE
080900060907     C                   MOVEL     VIDsel        DEPsel
081000060313     C                   MOVEL     VIDppo        DEPppo
081100060907     C                   MOVEL     viddat        depdat
081200060331     C                   MOVEL     VIDord        DEPord
081300970526E   1C                   ENDIF
081400970526     C*
081500970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
081600060908     c                   eval      opzione='02'
081700970526     C                   EXSR      CHKFOP
081800970526     C*
081900970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
082000060404IF  1C     opzione       IFEQ      '01'
082100970530     C                   MOVEL     DEC(3)        VIDING
082200970530E   1C                   ENDIF
082300060404IF  1C     opzione       IFEQ      '02'
082400970530     C                   MOVEL     DEC(1)        VIDING
082500970530E   1C                   ENDIF
082600060404IF  1C     opzione       IFEQ      '05'
082700970530     C                   MOVEL     DEC(2)        VIDING
082800970530E   1C                   ENDIF
082900970526     C*
083000970526     C                   ENDSR
083100970526     C*------------------------------------------------------------------------*
083200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
083300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
083400970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
083500970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
083600970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
083700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
083800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
083900970526     C*------------------------------------------------------------------------*
084000970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
084100970526     C*------------------------------------------------------------------------*
084200970526     C     POSSFL        BEGSR
084300970526     C*
084400970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
084500970526     C                   EXSR      CLRSFL
084600970526     C*
084700970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
084800970526     C                   EXSR      SETFIL
084900970526     C*
085000970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
085100970526     C     SFLEOF        IFEQ      0
085200970526     C                   EXSR      CARSFL
085300970526     C                   END
085400970526     C*
085500970526     C                   ENDSR
085600970526     C*------------------------------------------------------------------------*
085700970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
085800970526     C*------------------------------------------------------------------------*
085900970526     C     ENTATT        BEGSR
086000970526     C*
086100970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
086200970526     C                   Z-ADD     0             OPZOK             1 0
086300970526     C*
086400970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
086500970526     C                   Z-ADD     0             SFLVAR            5 0
086600970526     C*
086700970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
086800970526     C                   Z-ADD     0             SFLERR
086900970526     C*
087000970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
087100970526     C                   Z-ADD     SFLULT        WFLULT
087200970526     C*
087300970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
087400060907     C                   READC     s762D05                                98
087500970526     C     *IN98         DOWEQ     '0'
087600970526     C*
087700970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
087800970526     C                   ADD       1             SFLVAR
087900970526     C*
088000970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
088100970526     C     OPZOK         IFEQ      0
088200970526     C*
088300970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
088400970526     C     OPZKEY        IFNE      *BLANKS
088500970526     C                   EXSR      OPZCON
088600970526     C                   END
088700970526     C*
088800970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
088900970526     C*            2. PULIZIA SCELTA IN SFL
089000970526     C     OPZOK         IFEQ      0
089100970526     C                   MOVEL     '0'           *IN41
089200970526     C                   MOVEL     *BLANKS       OPZKEY
089300970526     C                   ELSE
089400970526     C*
089500970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
089600970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
089700970526     C                   MOVEL     '1'           *IN41
089800970526     C                   Z-ADD     SFLULT        SFLERR
089900970526     C                   END
090000970526     C                   ELSE
090100970526     C*
090200970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
090300970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
090400970526     C                   MOVEL     '1'           *IN41
090500970526     C                   END                                                    OPZOK=0
090600970526     C*
090700970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
090800970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
090900970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
091000970526     C     OPZOK         IFNE      0
091100970526     C                   Z-ADD     SFLERR        SFLNBR
091200970526     C                   ELSE
091300970526     C                   Z-ADD     SFLULT        SFLNBR
091400970526     C                   END
091500970526     C*
091600970526     C* AGGIORNO RECORD SFL
091700060908     c                   movel     in02          *in02
091800060911     c                   movel     in03          *in03
091900060907     C                   UPDATE    s762D05
092000970526     C*
092100060907     C                   READC     s762D05                                98
092200970526     C                   END                                                    DO WHILE
092300970526     C*
092400970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
092500970526     C                   Z-ADD     WFLULT        SFLULT
092600970526     C*
092700970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
092800970526     C     SFLVAR        IFEQ      0
092900970526     C*---                 MOVEL'1'       FINL00
093000970526     C                   END
093100970526     C*
093200970526     C                   ENDSR
093300970526     C*------------------------------------------------------------------------*
093400970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
093500970526     C*------------------------------------------------------------------------*
093600970526     C     CLRSFL        BEGSR
093700970526     C*
093800970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
093900970526     C* (0=NON FINE FILE  1=FINE FILE)
094000970526     C                   Z-ADD     0             SFLEOF
094100970526     C*
094200970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
094300970526     C                   Z-ADD     0             SFLULT
094400970526     C*
094500970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
094600970526     C                   MOVEL     '1'           *IN40
094700060907     C                   WRITE     s762D06
094800970526     C                   MOVEL     '0'           *IN40
094900970526     C*
095000970526     C                   ENDSR
095100970526     C*------------------------------------------------------------------------*
095200970526     C* CARSFL - CARICAMENTO SFL
095300970526     C*------------------------------------------------------------------------*
095400970526     C     CARSFL        BEGSR
095500970526     C*
095600970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
095700970526     C     SFLULT        IFEQ      0
095800970526     C                   EXSR      SETFIL
095900970526     C                   END
096000970526     C*
096100970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
096200970526     C                   Z-ADD     SFLULT        WFLULT
096300970526     C*
096400970526     C* CICLO:
096500970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
096600970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
096700970526     C                   Z-ADD     1             SFLRIG
096800970526     C     SFLRIG        DOWLE     SFLPAG
096900970526     C     SFLEOF        ANDEQ     0
097000970526     C*
097100970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
097200970526     C                   MOVEL     *BLANKS       OPZKEY
097300970526     C*
097400970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
097500970526     C                   EXSR      IRESFL
097600970526     C*
097700970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
097800970526     C                   ADD       1             SFLULT
097900970526     C*
098000970526     C* SCRITTURA RECORD SFL
098100060907     C                   WRITE     s762D05
098200970526     C                   ADD       1             SFLRIG
098300970526     C*
098400970526     C* LETTURA FILE DA CARICARE
098500970526     C                   EXSR      LETFIL
098600970526     C                   END                                                    DO WHILE
098700970526     C*
098800970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
098900970526     C     WFLULT        IFNE      SFLULT
099000970526     C                   Z-ADD     WFLULT        SFLNBR
099100970526     C                   ADD       1             SFLNBR
099200970526     C                   END
099300970526     C*
099400970526     C                   ENDSR
099500970526     C*------------------------------------------------------------------------*
099600970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
099700970526     C*------------------------------------------------------------------------*
099800970526     C     OUTSFL        BEGSR
099900970526     C*
100000970526     C* WRITE RECORD TESTATA VIDEO
100100060907     C                   WRITE     s762D01
100200970526     C*
100300970526     C* VISUALIZZAZIONE MESSAGGIO
100400970526     C                   EXSR      SNDMSG
100500970526     C*
100600970526     C* WRITE RECORD OPZIONI VIDEO
100700060907     C                   WRITE     s762D02
100800970526     C*
100900970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
101000060907     C                   WRITE     s762D03
101100970526     C*
101200970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
101300060907     C                   WRITE     s762D04
101400970526     C*
101500970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
101600970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
101700970526     C     SFLEOF        IFEQ      0
101800970526     C                   MOVEL     '0'           *IN42
101900970526     C                   ELSE
102000970526     C*
102100970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
102200970526     C                   MOVEL     '1'           *IN42
102300970526     C                   END
102400970526     C*
102500970526     C* WRITE RECORD TASTI FUNZIONALI
102600060907     C                   WRITE     s762D09
102700970526     C*
102800970526     C* CASO DI SFL VUOTO
102900970526     C     SFLULT        IFLE      0
103000970526     C*
103100970526     C*    - WRITE RECORD VUOTO
103200060907     C                   WRITE     s762D0A
103300060907     C                   READ      s762D0A                                99
103400970526     C*
103500970526     C                   ELSE
103600970526     C*
103700970526     C* CASO DI SFL CON RECORDS
103800970526     C*
103900970526     C*    - WRITE RECORD CONTROLLO SFL
104000060907     C                   WRITE     s762D06
104100060907     C                   READ      s762D06                                99
104200970526     C                   END
104300970526     C*
104400970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
104500060907     C                   READ      s762D03                                99
104600970526     C*
104700970526     C                   ENDSR
104800970526     C*------------------------------------------------------------------------*
104900970526     C* SNDMSG - MANDA MESSAGGIO
105000970526     C*------------------------------------------------------------------------*
105100970526     C     SNDMSG        BEGSR
105200970526     C*
105300970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
105400970526     C                   MOVEL     '1'           *IN40
105500060907     C                   WRITE     s762DMC
105600970526     C                   MOVEL     '0'           *IN40
105700970526     C*
105800970526     C* VISUALIZZAZIONE MESSAGGIO
105900970526     C                   Z-ADD     0             MSGULT
106000970526     C                   CALL      'XMSG'        XMSGPA
106100970526     C     DSMSER        IFEQ      0
106200970526     C                   MOVEL     DSMSKE        MSGKEY
106300970526     C                   END
106400970526     C                   Z-ADD     1             MSGULT
106500060907     C                   WRITE     s762DMS                                      SFL MESS
106600060907     C                   WRITE     s762DMC                                      CONTR MESS
106700970526     C*
106800970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
106900970526     C                   MOVEL     *BLANKS       DSMSMS
107000970526     C*
107100970526     C                   ENDSR
107200970526     C*------------------------------------------------------------------------*
107300970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
107400970526     C*------------------------------------------------------------------------*
107500970526     C     FUNCON        BEGSR
107600970526     C*
107700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
107800970526     C                   EXSR      FUNVAR
107900970526     C*
108000970526     C* PULISCO CAMPI DI LAVORO
108100970526     C                   Z-ADD     0             I                 5 0
108200970526     C                   Z-ADD     0             W                 5 0
108300970526     C*
108400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
108500970526     C     FUNKEY        IFNE      *BLANKS
108600970526     C     1             DO        24            I
108700970526     C                   MOVEL     FUD(I)        FUDRIG
108800970526     C*
108900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
109000970526     C     FUNKEY        IFEQ      FUDFUN
109100970526     C     FUDATT        ANDEQ     'S'
109200970526     C                   Z-ADD     I             W
109300970526     C                   END
109400970526     C                   END                                                    DO WHILE
109500970526     C*
109600970526     C* FUNZIONE NON ATTIVA (ERRORE)
109700970526     C     W             IFEQ      0
109800970526     C*
109900970526     C* EMISSIONE MESSAGGIO DI ERRORE
110000970526     C                   MOVEL     MSG(2)        DSMSMS
110100970526     C                   END                                                    W=0
110200970526     C                   END                                                    FUNKEY NE *BLANKS
110300970526     C*
110400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
110500970526     C     W             IFNE      0
110600970526     C     FUNKEY        OREQ      *BLANKS
110700970526     C*
110800970526     C     FUNKEY        IFEQ      *BLANKS
110900970526     C                   EXSR      ENTGES
111000970526     C                   END
111100970526     C     FUNKEY        IFEQ      '03'
111200970526     C                   EXSR      F03GES
111300970526     C                   END
111400970526     C     FUNKEY        IFEQ      '05'
111500970526     C                   EXSR      F05GES
111600970526     C                   END
111700970526     C     FUNKEY        IFEQ      '06'
111800970526     C                   EXSR      F06GES
111900970526     C                   END
112000970526     C     FUNKEY        IFEQ      '12'
112100970526     C                   EXSR      F12GES
112200970526     C                   END
112300970526     C     FUNKEY        IFEQ      '23'
112400970526     C                   EXSR      F23GES
112500970526     C                   END
112600970526     C     FUNKEY        IFEQ      '24'
112700970526     C                   EXSR      F24GES
112800970526     C                   END
112900970526     C     FUNKEY        IFEQ      'RU'
113000970526     C                   EXSR      RUPGES
113100970526     C                   END
113200970526     C*
113300970526     C                   END
113400970526     C*
113500970526     C                   ENDSR
113600970526     C*------------------------------------------------------------------------*
113700970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
113800970526     C*------------------------------------------------------------------------*
113900970526     C     FUNVAR        BEGSR
114000970526     C*
114100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
114200970526     C     KEY           IFEQ      ENTER
114300970526     C                   MOVEL     *BLANKS       FUNKEY
114400970526     C                   END
114500970526     C     KEY           IFEQ      F1
114600970526     C                   MOVEL     '01'          FUNKEY
114700970526     C                   END
114800970526     C     KEY           IFEQ      F2
114900970526     C                   MOVEL     '02'          FUNKEY
115000970526     C                   END
115100970526     C     KEY           IFEQ      F3
115200970526     C                   MOVEL     '03'          FUNKEY
115300970526     C                   END
115400970526     C     KEY           IFEQ      F4
115500970526     C                   MOVEL     '04'          FUNKEY
115600970526     C                   END
115700970526     C     KEY           IFEQ      F5
115800970526     C                   MOVEL     '05'          FUNKEY
115900970526     C                   END
116000970526     C     KEY           IFEQ      F6
116100970526     C                   MOVEL     '06'          FUNKEY
116200970526     C                   END
116300970526     C     KEY           IFEQ      F7
116400970526     C                   MOVEL     '07'          FUNKEY
116500970526     C                   END
116600970526     C     KEY           IFEQ      F8
116700970526     C                   MOVEL     '08'          FUNKEY
116800970526     C                   END
116900970526     C     KEY           IFEQ      F9
117000970526     C                   MOVEL     '09'          FUNKEY
117100970526     C                   END
117200970526     C     KEY           IFEQ      F10
117300970526     C                   MOVEL     '10'          FUNKEY
117400970526     C                   END
117500970526     C     KEY           IFEQ      F11
117600970526     C                   MOVEL     '11'          FUNKEY
117700970526     C                   END
117800970526     C     KEY           IFEQ      F12
117900970526     C                   MOVEL     '12'          FUNKEY
118000970526     C                   END
118100970526     C     KEY           IFEQ      F13
118200970526     C                   MOVEL     '13'          FUNKEY
118300970526     C                   END
118400970526     C     KEY           IFEQ      F14
118500970526     C                   MOVEL     '14'          FUNKEY
118600970526     C                   END
118700970526     C     KEY           IFEQ      F15
118800970526     C                   MOVEL     '15'          FUNKEY
118900970526     C                   END
119000970526     C     KEY           IFEQ      F16
119100970526     C                   MOVEL     '16'          FUNKEY
119200970526     C                   END
119300970526     C     KEY           IFEQ      F17
119400970526     C                   MOVEL     '17'          FUNKEY
119500970526     C                   END
119600970526     C     KEY           IFEQ      F18
119700970526     C                   MOVEL     '18'          FUNKEY
119800970526     C                   END
119900970526     C     KEY           IFEQ      F19
120000970526     C                   MOVEL     '19'          FUNKEY
120100970526     C                   END
120200970526     C     KEY           IFEQ      F20
120300970526     C                   MOVEL     '20'          FUNKEY
120400970526     C                   END
120500970526     C     KEY           IFEQ      F21
120600970526     C                   MOVEL     '21'          FUNKEY
120700970526     C                   END
120800970526     C     KEY           IFEQ      F22
120900970526     C                   MOVEL     '22'          FUNKEY
121000970526     C                   END
121100970526     C     KEY           IFEQ      F23
121200970526     C                   MOVEL     '23'          FUNKEY
121300970526     C                   END
121400970526     C     KEY           IFEQ      F24
121500970526     C                   MOVEL     '24'          FUNKEY
121600970526     C                   END
121700970526     C     KEY           IFEQ      ROLLUP
121800970526     C                   MOVEL     'RU'          FUNKEY
121900970526     C                   END
122000970526     C     KEY           IFEQ      HELP
122100970526     C                   MOVEL     'HP'          FUNKEY
122200970526     C                   END
122300970526     C*
122400970526     C                   ENDSR
122500970526     C*------------------------------------------------------------------------*
122600970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
122700970526     C*------------------------------------------------------------------------*
122800970526     C     FUNGES        BEGSR
122900970526     C*
123000970526     C* PULISCO CAMPI DI LAVORO
123100970526     C                   Z-ADD     0             W                 5 0
123200970526     C*
123300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
123400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
123500970526     C     1             DO        24            I
123600970526     C                   MOVEL     FUD(I)        FUDRIG
123700970526     C*
123800970526     C* PULIZIA FUNZIONI PRECEDENTI
123900970526     C                   MOVEL     *BLANKS       FUV(I)
124000970526     C*
124100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
124200970526     C* (SALTA F23 E F24)
124300970526     C     FUDATT        IFEQ      'S'
124400970526     C     FUDFUN        ANDNE     '23'
124500970526     C     FUDFUN        ANDNE     '24'
124600970526     C                   ADD       1             W
124700970526     C                   MOVEL     FUDDES        FUV(W)
124800970526     C                   END
124900970526     C                   END                                                    DO WHILE
125000970526     C*
125100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
125200970526     C* ALTRIMENTI DISATTIVO F24
125300970526     C     1             DO        24            I
125400970526     C                   MOVEL     FUD(I)        FUDRIG
125500970526     C*
125600970526     C     FUDFUN        IFEQ      '24'
125700970526     C     W             IFGT      6
125800970526     C                   MOVEL     'S'           FUDATT
125900970526     C                   ELSE
126000970526     C                   MOVEL     *BLANKS       FUDATT
126100970526     C                   END                                                    W GT 8
126200970526     C                   MOVEL     FUDRIG        FUD(I)
126300970526     C                   END
126400970526     C                   END                                                    DO WHILE
126500970526     C*
126600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
126700970526     C     W             DIV       6             I
126800970526     C                   MVR                     Z
126900970526     C*
127000970526     C     Z             IFNE      0
127100970526     C     I             ADD       1             Z
127200970526     C                   ELSE
127300970526     C                   Z-ADD     I             Z
127400970526     C                   END
127500970526     C*
127600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
127700970526     C                   ADD       1             FUNGRU
127800970526     C     FUNGRU        IFGT      Z
127900970526     C     FUNGRU        ORLT      1
128000970526     C                   Z-ADD     1             FUNGRU
128100970526     C                   END
128200970526     C*
128300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
128400970526     C     FUNGRU        IFEQ      1
128500970526     C                   MOVEL     FUV(1)        FUN001
128600970526     C                   MOVEL     FUV(2)        FUN002
128700970526     C                   MOVEL     FUV(3)        FUN003
128800970526     C                   MOVEL     FUV(4)        FUN004
128900970526     C                   MOVEL     FUV(5)        FUN005
129000970526     C                   MOVEL     FUV(6)        FUN006
129100970526     C                   END
129200970526     C*
129300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
129400970526     C     FUNGRU        IFEQ      2
129500970526     C                   MOVEL     FUV(7)        FUN001
129600970526     C                   MOVEL     FUV(8)        FUN002
129700970526     C                   MOVEL     FUV(9)        FUN003
129800970526     C                   MOVEL     FUV(10)       FUN004
129900970526     C                   MOVEL     FUV(11)       FUN005
130000970526     C                   MOVEL     FUV(12)       FUN006
130100970526     C                   END
130200970526     C*
130300970526     C* TERZO GRUPPO DI 6 FUNZIONI
130400970526     C     FUNGRU        IFEQ      3
130500970526     C                   MOVEL     FUV(13)       FUN001
130600970526     C                   MOVEL     FUV(14)       FUN002
130700970526     C                   MOVEL     FUV(15)       FUN003
130800970526     C                   MOVEL     FUV(16)       FUN004
130900970526     C                   MOVEL     FUV(17)       FUN005
131000970526     C                   MOVEL     FUV(18)       FUN006
131100970526     C                   END
131200970526     C*
131300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
131400970526     C     FUNGRU        IFEQ      4
131500970526     C                   MOVEL     FUV(19)       FUN001
131600970526     C                   MOVEL     FUV(20)       FUN002
131700970526     C                   MOVEL     FUV(21)       FUN003
131800970526     C                   MOVEL     FUV(22)       FUN004
131900970526     C                   MOVEL     *BLANKS       FUN005
132000970526     C                   MOVEL     *BLANKS       FUN006
132100970526     C                   END
132200970526     C*
132300970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
132400970526     C                   MOVEL     *BLANKS       FUN007
132500970526     C                   MOVEL     *BLANKS       FUN008
132600970526     C*
132700970526     C     1             DO        24            I
132800970526     C                   MOVEL     FUD(I)        FUDRIG
132900970526     C     FUDFUN        IFEQ      '23'
133000970526     C     FUDATT        ANDEQ     'S'
133100970526     C                   MOVEL     FUDDES        FUN007
133200970526     C                   END
133300970526     C     FUDFUN        IFEQ      '24'
133400970526     C     FUDATT        ANDEQ     'S'
133500970526     C                   MOVEL     FUDDES        FUN008
133600970526     C                   END
133700970526     C                   END                                                    DO
133800970526     C*
133900970526     C                   ENDSR
134000970526     C*------------------------------------------------------------------------*
134100970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
134200970526     C*------------------------------------------------------------------------*
134300970526     C     OPZGES        BEGSR
134400970526     C*
134500970526     C* PULISCO CAMPI DI LAVORO
134600970526     C                   Z-ADD     0             W                 5 0
134700970526     C*
134800970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
134900970526     C* E CARICO TABELLA OPZIONI ATTIVE
135000970526     C     1             DO        24            I
135100970526     C                   MOVEL     OPD(I)        OPDRIG
135200970526     C*
135300970526     C* PULIZIA OPZIONI PRECEDENTI
135400970526     C                   MOVEL     *BLANKS       OPV(I)
135500970526     C*
135600970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
135700970526     C     OPDATT        IFEQ      'S'
135800970526     C                   ADD       1             W
135900970526     C                   MOVEL     OPDDES        OPV(W)
136000970526     C                   END
136100970526     C                   END                                                    DO WHILE
136200970526     C*
136300970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
136400970526     C* ALTRIMENTI DISATTIVO F23
136500970526     C     1             DO        24            I
136600970526     C                   MOVEL     FUD(I)        FUDRIG
136700970526     C*
136800970526     C     FUDFUN        IFEQ      '23'
136900970526     C     W             IFGT      8
137000970526     C                   MOVEL     'S'           FUDATT
137100970526     C                   ELSE
137200970526     C                   MOVEL     *BLANKS       FUDATT
137300970526     C                   END                                                    W GT 8
137400970526     C                   MOVEL     FUDRIG        FUD(I)
137500970526     C                   END
137600970526     C                   END                                                    DO WHILE
137700970526     C*
137800970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
137900970526     C     W             DIV       8             I
138000970526     C                   MVR                     Z                 5 0
138100970526     C*
138200970526     C     Z             IFNE      0
138300970526     C     I             ADD       1             Z
138400970526     C                   ELSE
138500970526     C                   Z-ADD     I             Z
138600970526     C                   END
138700970526     C*
138800970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
138900970526     C                   ADD       1             OPZGRU
139000970526     C     OPZGRU        IFGT      Z
139100970526     C     OPZGRU        ORLT      1
139200970526     C                   Z-ADD     1             OPZGRU
139300970526     C                   END
139400970526     C*
139500970526     C* PRIMO GRUPPO DI 8 OPZIONI
139600970526     C     OPZGRU        IFEQ      1
139700970526     C                   MOVEL     OPV(1)        OPZ001
139800970526     C                   MOVEL     OPV(2)        OPZ002
139900970526     C                   MOVEL     OPV(3)        OPZ003
140000970526     C                   MOVEL     OPV(4)        OPZ004
140100970526     C                   MOVEL     OPV(5)        OPZ005
140200970526     C                   MOVEL     OPV(6)        OPZ006
140300970526     C                   MOVEL     OPV(7)        OPZ007
140400970526     C                   MOVEL     OPV(8)        OPZ008
140500970526     C                   END
140600970526     C*
140700970526     C* SECONDO GRUPPO DI 8 OPZIONI
140800970526     C     OPZGRU        IFEQ      2
140900970526     C                   MOVEL     OPV(9)        OPZ001
141000970526     C                   MOVEL     OPV(10)       OPZ002
141100970526     C                   MOVEL     OPV(11)       OPZ003
141200970526     C                   MOVEL     OPV(12)       OPZ004
141300970526     C                   MOVEL     OPV(13)       OPZ005
141400970526     C                   MOVEL     OPV(14)       OPZ006
141500970526     C                   MOVEL     OPV(15)       OPZ007
141600970526     C                   MOVEL     OPV(16)       OPZ008
141700970526     C                   END
141800970526     C*
141900970526     C* TERZO GRUPPO DI 8 OPZIONI
142000970526     C     OPZGRU        IFEQ      3
142100970526     C                   MOVEL     OPV(17)       OPZ001
142200970526     C                   MOVEL     OPV(18)       OPZ002
142300970526     C                   MOVEL     OPV(19)       OPZ003
142400970526     C                   MOVEL     OPV(20)       OPZ004
142500970526     C                   MOVEL     OPV(21)       OPZ005
142600970526     C                   MOVEL     OPV(22)       OPZ006
142700970526     C                   MOVEL     OPV(23)       OPZ007
142800970526     C                   MOVEL     OPV(24)       OPZ008
142900970526     C                   END
143000970526     C*
143100970526     C                   ENDSR
143200970526     C*------------------------------------------------------------------------*
143300970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
143400970526     C*------------------------------------------------------------------------*
143500970526     C     OPZCON        BEGSR
143600970526     C*
143700970526     C* PULISCO CAMPI DI LAVORO
143800970526     C                   Z-ADD     0             I                 5 0
143900970526     C                   Z-ADD     0             W                 5 0
144000970526     C*
144100970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
144200970526     C     1             DO        24            I
144300970526     C                   MOVEL     OPD(I)        OPDRIG
144400970526     C*
144500970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
144600970526     C     OPZKEY        IFEQ      OPDOPZ
144700970526     C     OPZKEY        OREQ      OPDOP1
144800970526     C*
144900970526     C     OPDATT        IFEQ      'S'
145000970526     C                   MOVEL     OPDOPZ        OPZKEY
145100970526     C                   Z-ADD     I             W
145200970526     C                   END
145300970526     C                   END
145400970526     C*
145500970526     C                   END                                                    DO WHILE
145600970526     C*
145700970526     C* OPZIONE NON ATTIVA (ERRORE)
145800970526     C     W             IFEQ      0
145900970526     C*
146000970526     C* OPZIONE NON OK
146100970526     C                   Z-ADD     1             OPZOK
146200970526     C*
146300970526     C* EMISSIONE MESSAGGIO DI ERRORE
146400970526     C                   MOVEL     MSG(1)        DSMSMS
146500970526     C                   ELSE
146600970526     C*
146700970526     C* OPZIONE ATTIVA (PROCEDI ...)
146800970526     C*
146900970526     C     OPZKEY        IFEQ      ' 2'
147000060328     C                   EXSR      O02GES
147100970526     C                   END
147200970611     C     OPZKEY        IFEQ      ' 3'
147300970611     C                   EXSR      O03GES
147400970611     C                   END
147500970526     C     OPZKEY        IFEQ      ' 4'
147600970526     C                   EXSR      O04GES
147700970526     C                   END
147800970526     C     OPZKEY        IFEQ      ' 5'
147900970526     C                   EXSR      O05GES
148000970526     C                   END
148100060908     C     OPZKEY        IFEQ      ' 7'
148200060908     C                   EXSR      O07GES
148300060908     C                   END
148400970526     C                   END                                                    OPZ.NON ATTIVA
148500970526     C*
148600970526     C                   ENDSR
148700970526     C*------------------------------------------------------------------------*
148800970526     C* NOTE COSTRUZIONE SCHIERE
148900970526     C*------------------------------------------------------------------------*
149000970526     C*
149100970526     C* -MSG-MESSAGGI
149200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
149300970526     C* (NON TOCCARE MSG,1 E MSG,2)
149400970526     C*
149500970526     C* -CMD-COMANDI
149600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
149700970526     C*
149800970526     C* -OPD-OPZIONI
149900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
150000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
150100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
150200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
150300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
150400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
150500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
150600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
150700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
150800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
150900970526     C*
151000970526     C* -FUD-FUNZIONI
151100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
151200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
151300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
151400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
151500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
151600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
151700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
151800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
151900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
152000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
152100970526     O*------------------------------------------------------------------------*
152200970526** DEC - DECODIFICHE
152300970526    GESTIONE                                                     1
152400970530INTERROGAZIONE                                                   2
152500970530     SCELTA                                                      3
152600060911 ANNULLAMENTO
152700060911  RIPRISTINO
152800970526** MSG - MESSAGGI
152900060907TIS762R- Opzione non gestita                                                   1
153000060907TIS762R- Tasto funzionale non gestito                                          2
153100060907TIS762R- Data di riferimento errata
153200060907TIS762R- P.O. di selezione errato                                              4
153300060911TIS762R- LOG non annullabile                                                   5
153400060911TIS762R- LOG non ripristinabile                                                6
153500060907TIS762R- 7                                                                     7
153600060907TIS762R- 8                                                                     8
153700060907TIS762R- 9                                                                     9
153800060907TIS762R- 10                                                                   10
153900060907TIS762R- 11                                                                   11
154000060907TIS762R- 12                                                                   12
154100060907TIS762R- 13                                                                   13
154200060907TIS762R- 14                                                                   14
154300060907TIS762R- 15                                                                   15
154400060907TIS762R- 16                                                                   16
154500060907TIS762R- 17                                                                   17
154600060907TIS762R- 18                                                                   18
154700060907TIS762R- 19                                                                   19
154800060907TIS762R- 20                                                                   20
154900970526** OPD - OPZIONI
155000970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
155100060907                   -2 -     (S/N)=N                        opz. 02¬
155200060907                   -3 -     (S/N)=N                        opz. 03¬
155300060315 4=Annullamento    -4 -     (S/N)=S                        opz. 04¬
155400060315 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
155500970526                   -6 -     (S/N)=N                        opz. 06¬
155600060907 7=Ripristino      -7 -     (S/N)=S                        opz. 07¬
155700970527                   -8 -     (S/N)=N                        opz. 08¬
155800970527                   -9 -     (S/N)=N                        opz. 09¬
155900970527                   -10-     (S/N)=N                        opz. 10¬
156000970529                   -11-     (S/N)=N                        opz. 11¬
156100970529                   -12-     (S/N)=N                        opz. 12¬
156200060907                   -13-     (S/N)=N                        opz. 13¬
156300970526                   -  -     (S/N)=N                        opz. 14¬
156400970526                   -  -     (S/N)=N                        opz. 15¬
156500970526                   -  -     (S/N)=N                        opz. 16¬
156600970526                   -  -     (S/N)=N                        opz. 17¬
156700970526                   -  -     (S/N)=N                        opz. 18¬
156800970526                   -  -     (S/N)=N                        opz. 19¬
156900970526                   -  -     (S/N)=N                        opz. 20¬
157000970526                   -  -     (S/N)=N                        opz. 21¬
157100970526                   -  -     (S/N)=N                        opz. 22¬
157200970526                   -  -     (S/N)=N                        opz. 23¬
157300970526                   -  -     (S/N)=N                        opz. 24¬
157400970526** FUD - FUNZIONI
157500970526                   -        (S/N)=N                        funz.01¬
157600970526                   -        (S/N)=N                        funz.02¬
157700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
157800970526                   -        (S/N)=N                        funz.04¬
157900970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
158000060907                   -        (S/N)=N                        funz.06¬
158100970526                   -        (S/N)=N                        funz.07¬
158200970527                   -        (S/N)=N                        funz.08¬
158300970527                   -        (S/N)=N                        funz.09¬
158400970527                   -        (S/N)=N                        funz.10¬
158500970526                   -        (S/N)=N                        funz.11¬
158600970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
158700060907                   -        (S/N)=N                        funz.13¬
158800970526                   -        (S/N)=N                        funz.14¬
158900970526                   -        (S/N)=N                        funz.15¬
159000970526                   -        (S/N)=N                        funz.16¬
159100970526                   -        (S/N)=N                        funz.17¬
159200970526                   -        (S/N)=N                        funz.18¬
159300970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
159400970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
159500970527                   -        (S/N)=N                        funz.21¬
159600970527                   -        (S/N)=N                        funz.22¬
159700970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
159800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
