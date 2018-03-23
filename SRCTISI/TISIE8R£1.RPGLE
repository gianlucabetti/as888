000100970526     H*------------------------------------------------------------------------*
000200060922     H* Cappario Utilizzi particolari  SCELTA
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800060922     FTISIQ0D   CF   E             WORKSTN INFDS(DEVDS1)
000900060922     F                                     SFILE(SIQ0D05:SFLULT)
001000060922     F                                     SFILE(SIQ0DMS:MSGULT)
001100060922     FAZcpp01L  IF   E           K DISK
001200060315     FAZorg01l  IF   E           K DISK
001300060922     Ftabel00f  IF   E           K DISK
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
008400970526     D* DS DI PROCEDURA
008500970526     D*------------
008600060922     D tiSIQ0ds      E DS
008700060314     D tisi00ds      E DS
008800060922     D tibs02ds      E DS
008900970526     D*------------
009000970526     D KPJBA         E DS
009100060313     d
009200060313     d Dataiso         s               d   datfmt(*iso)
009300060313     d Dataeur         s               d   datfmt(*eur)
009400060329     d Datadmy         s               d   datfmt(*dmy)
009500060922     d knar            s                   Like(cppnar)
009600060922     d kcap            s                   Like(cppcap)
009700060329     d kcod            s                   Like(tblCod)
009800060329     d kkey            s                   Like(TblKey)
009900060404     d opzione         s              2
010000970526     C*------------------------------------------------------------------------*
010100970526     C* MAIN LINES
010200970526     C*------------------------------------------------------------------------*
010300970526     C*
010400970526     C* SE NON CI SONO ERRORI, PROSEGUE
010500970526IF  1C     ERRL00        IFNE      '1'
010600970526     C*
010700970526     C* CARICA IL SFILE
010800970526     C                   EXSR      POSSFL
010900970526     C*
011000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011400970526     C*
011500970526     C* REIMPOSTA ERRORE DI PROCERURA
011600970526     C                   MOVEL     '0'           ERRL00
011700970526     C*
011800970526     C* EMETTE IL SFILE
011900970526     C                   EXSR      OUTSFL
012000970526     C*
012100970526     C* GESTIONE TASTI FUNZIONALI
012200970526     C                   EXSR      FUNCON
012300970526     C*
012400970526E   2C                   ENDDO
012500970527     C*
012600970527     C* CI SONO ERRORI IN ENTRATA
012700970527X   1C                   ELSE
012800060922     C                   MOVEL     '1'           dq0ERR
012900060922     C                   MOVEL     DSMSMS        dq0MSG
013000970526E   1C                   ENDIF
013100970526     C* FINE PGM
013200970526     C                   SETON                                        LR
013300970526     C*------------------------------------------------------------------------*
013400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
013500970526     C*------------------------------------------------------------------------*
013600970526     C     F03GES        BEGSR
013700970526     C*
013800970526     C* IMPOSTA "PREMUTO F03"
013900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014000060922     C                   MOVEL     '1'           dq0F03                         *DI PROCEDURA
014100970526     C*
014200970526     C                   ENDSR
014300970526     C*------------------------------------------------------------------------*
014400970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
014500970526     C*------------------------------------------------------------------------*
014600970526     C     F05GES        BEGSR
014700970526     C*
014800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
014900970526     C*
015000970526     C                   ENDSR
015100970526     C*------------------------------------------------------------------------*
015200970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
015300970526     C*------------------------------------------------------------------------*
015400970526     C     F06GES        BEGSR
015500970526     C*
015600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
015700970603     C                   MOVEL     *BLANKS       DNAR
015800970602     C                   MOVEL     *BLANKS       DCAP
015900060922     c                   clear                   dntw
016000060922     c                   clear                   ddde
016100060922     c                   clear                   ddsc
016200060922     c                   clear                   duti
016300060922     C                   MOVEL     *BLANKS       Dflo
016400970529     C                   MOVEL     DEPOP0        WOP0
016500970529     C                   MOVEL     'F06'         WOP1
016600060922     C                   MOVEL     'TISIQ1R'     WPGM              8
016700060314     C                   EXSR      LANPGMp                                      *LANCIA PGM
016800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
016900970526E   1C                   ENDIF
017000970526     C*
017100970526     C                   ENDSR
017200060926     C*------------------------------------------------------------------------*
017300060926     C* F07GES - GESTIONE F07 -> int cappario
017400060926     C*------------------------------------------------------------------------*
017500060926     C     F07GES        BEGSR
017600060926     C* IMPOSTA LA DS DI PROCEDURA
017700060926     c                   clear                   tisi00ds
017800060926     c                   if        amgpde>0
017900060926     c                   movel     amgpde        d00dri
018000060926     c                   else
018100060926     c                   movel     datcor        d00dri
018200060926     c                   endif
018300060927     C                   MOVEL     'C05'         d00OP0                         *LIVELLO PROCEDURA
018400060926     c
018500060926     C                   clear                   d00OP1                         *LIVELLO PGM
018600060926     C                   MOVEL     '0'           d00F03                         *PREMUTO F03=FINE
018700060926     C                   MOVEL     '0'           d00F12                         *PREMUTO F12=RITORNO
018800060926     C                   MOVEL     '0'           d00ERR                         *'1'=ERRORE
018900060926     C                   MOVEL     *BLANKS       d00MSG                         *MESSAGGIO DI RITORN
019000060926     C                   clear                   d00NAR                         *NAZIONE
019100060926     C                   clear                   d00CAP                         *CAP
019200060926     C                   clear                   d00LOC                         *LOCALITA'
019300060926     C*
019400060926     C                   CALL      'TISI02R'
019500060926     C                   PARM                    KPJBA
019600060926     C                   PARM                    tisi00ds
019700060926     C                   ENDSR
019800970526     C*------------------------------------------------------------------------*
019900970526     C* F12GES - GESTIONE F12 -> RITORNO
020000970526     C*------------------------------------------------------------------------*
020100970526     C     F12GES        BEGSR
020200970526     C*
020300970526     C* IMPOSTA "PREMUTO F12"
020400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020500060922     C                   MOVEL     '1'           dq0F12                         *DI PROCEDURA
020600970526     C*
020700970526     C                   ENDSR
020800970529     C*------------------------------------------------------------------------*
020900060922     C* F13GES - GESTIONE F13 ->
021000970529     C*------------------------------------------------------------------------*
021100970529     C     F13GES        BEGSR
021200970529     C*
021300970529     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
021600970526     C*------------------------------------------------------------------------*
021700970526     C     F23GES        BEGSR
021800970526     C*
021900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022000970526     C*
022100970526     C                   ENDSR
022200970526     C*------------------------------------------------------------------------*
022300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022400970526     C*------------------------------------------------------------------------*
022500970526     C     F24GES        BEGSR
022600970526     C*
022700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022800970526     C*
022900970526     C                   ENDSR
023000970526     C*------------------------------------------------------------------------*
023100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023200970526     C*------------------------------------------------------------------------*
023300970526     C     RUPGES        BEGSR
023400970526     C*
023500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
023600970526     C*
023700970526     C                   ENDSR
023800970526     C*------------------------------------------------------------------------*
023900970526     C* ENTGES - GESTIONE ENTER -> INVIO
024000970526     C*------------------------------------------------------------------------*
024100970526     C     ENTGES        BEGSR
024200970526     C*
024300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
024400970526     C                   EXSR      NEWPOS
024500970526     C*
024600970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
024700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024800970526IF  2C     SWPOSI        IFNE      0
024900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
025000970526X   2C                   ELSE
025100970526IF  3C     SFLULT        IFNE      0
025200970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025300970526     C*
025400970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
025500970526IF  4C     SWMODI        IFNE      0
025600970526     C                   EXSR      POSSFL
025700970526E   4C                   ENDIF
025800970526E   3C                   ENDIF
025900970526E   2C                   ENDIF
026000970526E   1C                   ENDIF
026100970526     C*
026200970526     C                   ENDSR
026300970526     C*------------------------------------------------------------------------*
026400970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
026500970526     C*------------------------------------------------------------------------*
026600970526     C     O01GES        BEGSR
026700970526     C*
026800970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
026900060922     C                   MOVEL     V5cNAR        dq0NAR
027000060922     C                   MOVEL     V5cCAP        dq0CAP
027100060922     C                   MOVEL     V5cntw        dq0ntw
027200060922     C                   MOVEL     V5hdde        dq0dde
027300060922     C                   MOVEL     V5hdsc        dq0dsc
027400060922     C                   MOVEL     V5cuti        dq0uti
027500060922     C                   MOVEL     v5hflo        Dq0flo
027600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
027700970526     C*
027800970526     C                   ENDSR
027900970526     C*------------------------------------------------------------------------*
028000970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
028100970526     C*------------------------------------------------------------------------*
028200970526     C     O02GES        BEGSR
028300970526     C*
028400970527     C* MODIFICA
028500970704IF  1C     ERRL00        IFEQ      '0'
028600060922     C                   MOVEL     V5cNAR        DNAR
028700060922     C                   MOVEL     V5cCAP        DCAP
028800060922     C                   MOVEL     V5cntw        Dntw
028900060922     C                   MOVEL     V5cuti        Duti
029000060922     C                   MOVEL     V5hflo        Dflo
029100060922     C                   MOVEL     V5hdde        Ddde
029200060922     C                   MOVEL     V5hdsc        Ddsc
029300970529     C                   MOVEL     DEPOP0        WOP0
029400970526     C                   MOVEL     'O02'         WOP1
029500060925     C                   MOVEL     'TISIQ1R'     WPGM
029600060315     C                   EXSR      LANPGMP
029700060328     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
029800970528E   1C                   ENDIF
029900970526     C*
030000970526     C                   ENDSR
030100970611     C*------------------------------------------------------------------------*
030200970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
030300970611     C*------------------------------------------------------------------------*
030400970611     C     O03GES        BEGSR
030500970611     C*
030600970611     C* DUPLICA
030700970611IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030800060922     C                   MOVEL     V5cNAR        DNAR
030900060922     C                   MOVEL     V5cCAP        DCAP
031000060922     C                   MOVEL     V5cntw        Dntw
031100060922     C                   MOVEL     V5cuti        Duti
031200060922     C                   MOVEL     V5hflo        Dflo
031300060922     C                   MOVEL     V5hdde        Ddde
031400060922     C                   MOVEL     V5hdsc        Ddsc
031500060329     C                   MOVEL     DEPOP0        WOP0
031600060329     C                   MOVEL     'O03'         WOP1
031700060925     C                   MOVEL     'TISIQ1R'     WPGM
031800060329     C                   EXSR      LANPGMP
031900970611     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
032000970611E   1C                   ENDIF
032100970611     C*
032200970611     C                   ENDSR
032300970526     C*------------------------------------------------------------------------*
032400970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
032500970526     C*------------------------------------------------------------------------*
032600970526     C     O04GES        BEGSR
032700970526     C*
032800060327IF  1C     ERRL00        IFEQ      '0'
032900060922     C                   MOVEL     V5cNAR        DNAR
033000060922     C                   MOVEL     V5cCAP        DCAP
033100060922     C                   MOVEL     V5cntw        Dntw
033200060922     C                   MOVEL     V5cuti        Duti
033300060922     C                   MOVEL     V5hflo        Dflo
033400060922     C                   MOVEL     V5hdde        Ddde
033500060922     C                   MOVEL     V5hdsc        Ddsc
033600060327     C                   MOVEL     DEPOP0        WOP0
033700060327     C                   MOVEL     'O04'         WOP1
033800060925     C                   MOVEL     'TISIQ1R'     WPGM
033900060327     C                   EXSR      LANPGMP
034000060925     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
034100060327E   1C                   ENDIF
034200970526     C*
034300970526     C                   ENDSR
034400970526     C*------------------------------------------------------------------------*
034500970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
034600970526     C*------------------------------------------------------------------------*
034700970526     C     O05GES        BEGSR
034800970526     C*
034900060327     C                   MOVEL     DEPOP0        WOP0
035000060922     C                   MOVEL     V5cNAR        DNAR
035100060922     C                   MOVEL     V5cCAP        DCAP
035200060922     C                   MOVEL     V5cntw        Dntw
035300060922     C                   MOVEL     V5cuti        Duti
035400060922     C                   MOVEL     V5hflo        Dflo
035500060922     C                   MOVEL     V5hdde        Ddde
035600060922     C                   MOVEL     V5hdsc        Ddsc
035700060327     C                   MOVEL     'O05'         WOP1
035800060925     C                   MOVEL     'TISIQ1R'     WPGM
035900060327     C                   EXSR      LANPGMP
036000970529     C*
036100970529     C                   ENDSR
036200970526     C*------------------------------------------------------------------------*
036300970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
036400970526     C*------------------------------------------------------------------------*
036500970526     C     NEWPOS        BEGSR
036600970526     C*
036700970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
036800970526     C*         1= RICHIESTO POSIZIONAMENTO
036900970526     C                   Z-ADD     0             SWPOSI            7 0
037000970526     C*
037100970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
037200970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
037300970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
037400970526     C                   Z-ADD     0             SWMODI            1 0
037500970530     C*
037600970530     C* REIMPOSTA INDICATORI DI ERRORE
037700060313     C                   SETOFF                                       808281
037800970526     C*
037900060403     c
038000060403     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
038100970527IF  1C     VIDPNA        IFNE      DEPNAR
038200970527     C     VIDPCA        ORNE      DEPCAP
038300060922     C     VIDPnt        ORNE      DEPntw
038400060313     C     VIDPde        ORNE      DEPpde
038500970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
038600970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
038700970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
038800970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
038900970527     C                   MOVEL     VIDPCA        DEPCAP
039000060922     C                   MOVEL     VIDpnt        DEPntw
039100060313     C                   MOVEL     VIDpde        DEPpde
039200970526E   2C                   ENDIF
039300970526E   1C                   ENDIF
039400970526     C*
039500970526     C                   ENDSR
039600970526     C*------------------------------------------------------------------------*
039700970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
039800970526     C*------------------------------------------------------------------------*
039900970526     C     SETFIL        BEGSR
040000060330     c                   clear                   knar
040100060330     c                   clear                   kcap
040200060922     c                   eval      knar=vidpna
040300060922     c                   eval      kcap=vidpca
040400060330     c
040500970529     C*
040600060922     C     Kcpp          SETLL     AZCpp01L                           99
040700060330     c
040800970526     C* LETTURA RECORD
040900970526IF  1C     *IN99         IFEQ      *OFF
041000970526     C                   EXSR      LETFIL
041100970526E   1C                   ENDIF
041200970526     C*
041300970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
041400970526IF  1C     *IN99         IFEQ      *ON
041500970526     C                   Z-ADD     1             SFLEOF
041600970526E   1C                   ENDIF
041700970526     C*
041800970526     C                   ENDSR
041900970526     C*------------------------------------------------------------------------*
042000970526     C* LETFIL - LETTURA RECORD
042100970526     C*------------------------------------------------------------------------*
042200970526     C     LETFIL        BEGSR
042300970526     C*
042400970526     C* LEGGE FINO A:
042500970526     C*     - FINE FILE
042600970526     C*     - TROVATO RECORD VALIDO
042700970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
042800970526DO  1C     *IN99         DOWEQ     *OFF
042900970526     C     WRECOK        ANDEQ     'N'
043000970529     C*
043100970529     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
043200060922     C                   READ      AZcpp01L                               99
043300970526     C*
043400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
043500970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
043600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
043700970530X   2C                   ELSE
043800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
043900970530E   2C                   ENDIF
044000970526E   1C                   ENDDO
044100970526     C*
044200970526     C                   ENDSR
044300970526     C*------------------------------------------------------------------------*
044400970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
044500970526     C*------------------------------------------------------------------------*
044600970526     C     CHKREC        BEGSR
044700060922     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
044800060922     C* controllo data
044900060922     c                   if        amgpde>0 and (amgpde<cppdde or
045000060922     c                             amgpde>cppdsc)
045100060922     C                   MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
045200060329     c                   endif
045300060922     c* Network
045400060922     c                   if        vidpnt<>*blanks and
045500060922     c                             vidpnt<>cppntw
045600060922     C                   MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
045700060922     c                   endif
045800060329     c
045900970526     C                   ENDSR
046000970526     C*------------------------------------------------------------------------*
046100970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
046200970526     C*------------------------------------------------------------------------*
046300970526     C     IRESFL        BEGSR
046400970529     C*
046500970529     C                   MOVEL     *BLANKS       OPZKEY
046600060922     c* Nazione e decodifica
046700060922     c                   eval      v5cnar=cppnar
046800060922     c                   eval      kcod='15'
046900060922     c                   movel(p)  v5cnar        kkey
047000060922     c     ktab          chain     tabel00f
047100060922     c                   if        %found(tabel00f)
047200060922     c                   movel     tbluni        v5dnar
047300060922     c                   else
047400060922     c                   movel     *all'?'       v5dnar
047500060922     c                   endif
047600060922     c
047700060922     c* CAP
047800060922     c                   eval      v5ccap=cppcap
047900060922     c* Decorrenza
048000060922     c                   movel     cppdde        v5hdde
048100060922     c     *iso          movel     cppdde        dataiso
048200060922     c                   movel     dataiso       datadmy
048300060922     c                   movel     datadmy       v5cdde
048400060922     c* scadenza
048500060922     c                   movel     cppdsc        v5hdsc
048600060922     c     *iso          movel     cppdsc        dataiso
048700060922     c                   movel     dataiso       datadmy
048800060922     c                   movel     datadmy       v5cdsc
048900060922     c* network e decodifica
049000060922     C                   MOVEL     Cppntw        V5cntw
049100060922     C                   CLEAR                   tibs02ds
049200060922     C                   MOVEL     'C'           T02MOD
049300060922     C                   MOVEL     KNSIF         T02SIF
049400060922     C                   MOVEL     'NTW'         T02COD
049500060922     C                   MOVEL(P)  cppntw        T02KE1
049600060922     C                   CALL      'TIBS02R'
049700060922     C                   PARM                    KPJBA
049800060922     C                   PARM                    tibs02ds
049900060922      *
050000060922     C     T02ERR        IFNE      *BLANKS
050100060922     C                   MOVEL     *all'?'       v5dntw
050200060922     c                   eval      %subst(v5dntw:1:2)='- '
050300060922     C                   ELSE
050400060922     c                   eval      %subst(v5dntw:1:2)='- '
050500060922     c                   eval      %subst(v5dntw:3:11)=t02uni
050600060922     C                   ENDIF
050700060922     c
050800060922     c* utilizzo
050900060922     C                   if        Cpputi='S'
051000060922     c                   eval      v5cuti='SI'
051100060922     c                   else
051200060922     c                   eval      v5cuti='NO'
051300060922     c                   endif
051400060922     c
051500060922     c                   eval      v5hflo=cppflo
051600060313     c
051700970526     C                   ENDSR
051800970526     C*------------------------------------------------------------------------*
051900970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
052000970526     C*------------------------------------------------------------------------*
052100970526     C     CHKPAR        BEGSR
052200060313     c* controllo data di riferimento se errata
052300060922     c                   clear                   amgpde
052400060313     c                   if        vidpde>0
052500060313     C                   Z-ADD     vidpde        G08DAT
052600060313     C                   Z-ADD     *ZEROS        G08INV
052700060313     C                   MOVEL     '0'           G08ERR
052800060313     C                   CALL      'XSRDA8'
052900060313     C                   PARM                    WLBDA8
053000060313     c                   if        g08err='1'
053100060313     C                   MOVEL     MSG(3)        DSMSMS
053200060313     C                   MOVEL     '1'           ERRL00
053300060922     C                   SETON                                        81
053400060313E   2C                   else
053500060314     c                   z-add     g08dat        vidpde
053600060922     C                   Z-ADD     G08INV        amgpde            8 0          *DATA CORRENTE AA/M/
053700060313     c                   endif
053800060313     c                   endif
053900060922     c* Network
054000060922     c                   if        vidpnt<>*blanks
054100060922     C                   CLEAR                   tibs02ds
054200060922     C                   MOVEL     'C'           T02MOD
054300060922     C                   MOVEL     KNSIF         T02SIF
054400060922     C                   MOVEL     'NTW'         T02COD
054500060922     C                   MOVEL     *BLANKS       T02KE1
054600060922     C                   MOVEL     vidpnt        T02KE1
054700060922     C                   CALL      'TIBS02R'
054800060922     C                   PARM                    KPJBA
054900060922     C                   PARM                    tibs02ds
055000060922      *
055100060922     C     T02ERR        IFNE      *BLANKS
055200060922     C                   MOVEL     MSG(4)        DSMSMS
055300060922     C                   MOVEL     '1'           ERRL00
055400060922     C                   SETON                                        82
055500060922     C                   ENDIF
055600060922     c                   endif
055700970526     C*
055800970526     C                   ENDSR
055900060314     C*------------------------------------------------------------------------*
056000060314     C* LANPGM - LANCIA UN PROGRAMMA
056100060314     C*------------------------------------------------------------------------*
056200060314     C     LANPGMp       BEGSR
056300060314     C*
056400060314     C* IMPOSTA LA DS DI PROCEDURA
056500060922     c                   clear                   tiSIQ0ds
056600060922     C                   MOVEL     WOP0          dq0OP0                         *LIVELLO PROCEDURA
056700060922     C                   MOVEL     WOP1          dq0OP1                         *LIVELLO PGM
056800060922     C                   MOVEL     '0'           dq0F03                         *PREMUTO F03=FINE
056900060922     C                   MOVEL     '0'           dq0F12                         *PREMUTO F12=RITORNO
057000060922     C                   MOVEL     '0'           dq0ERR                         *'1'=ERRORE
057100060922     C                   MOVEL     *BLANKS       dq0MSG                         *MESSAGGIO DI RITORN
057200060922     C                   MOVEL     DNAR          dq0NAR                         *NAZIONE
057300060922     C                   MOVEL     DCAP          dq0CAP                         *CAP
057400060922     C                   MOVEL     Dntw          dq0ntw                         *LOCALITA'
057500060922     C                   MOVEL     Ddde          dq0dde                         *LOCALITA'
057600060922     C                   MOVEL     Ddsc          dq0dsc                         *LOCALITA'
057700060922     C                   MOVEL     Duti          dq0uti                         *LOCALITA'
057800060922     C                   MOVEL     Dflo          dq0flo                         *LOCALITA'
057900060314     C*
058000060314     C                   CALL      WPGM
058100060314     C                   PARM                    KPJBA
058200060922     C                   PARM                    tiSIQ0ds
058300060314     C*
058400060314     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
058500060922IF  1C     DQ0F03        IFEQ      '1'                                          *PREMUTO F03
058600060314     C                   MOVEL     '1'           ERRL00                         *ERRORE
058700060314     C                   MOVEL     '1'           F03L00                         *FINE PGM
058800060314X   1C                   ELSE                                                   *NON PREMUTO F03
058900060922     C                   MOVEL     DQ0MSG        DSMSMS                         *MESSAGGIO DI RITORN
059000060314E   1C                   ENDIF
059100060314     C*
059200060314     C                   ENDSR
059300970527     C*------------------------------------------------------------------------*
059400970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
059500970527     C*------------------------------------------------------------------------*
059600970527     C     CARTAB        BEGSR
059700970527     C*
059800970527     C                   ENDSR
059900970526     C*------------------------------------------------------------------------*
060000970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
060100970526     C*------------------------------------------------------------------------*
060200970526     C     CHKFOP        BEGSR
060300970626     C*
060400970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
060500060404IF  1C     opzione       IFEQ      '01'
060600970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
060700970626     C                   MOVEL     'S'           OPDATT
060800970626     C                   MOVEL     OPDRIG        OPD(1)
060900970626X   1C                   ELSE
061000970626     C* OPZIONI
061100970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
061200970626     C                   MOVEL     'S'           OPDATT
061300970626     C                   MOVEL     OPDRIG        OPD(5)
061400970626E   1C                   ENDIF
061500970626     C*
061600970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
061700060404IF  1C     opzione       IFEQ      '02'
061800970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
061900970626     C                   MOVEL     'S'           OPDATT
062000970626     C                   MOVEL     OPDRIG        OPD(2)
062100970627     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
062200970627     C                   MOVEL     'S'           OPDATT
062300970627     C                   MOVEL     OPDRIG        OPD(3)
062400970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
062500970626     C                   MOVEL     'S'           OPDATT
062600970626     C                   MOVEL     OPDRIG        OPD(4)
062700970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
062800970626     C                   MOVEL     'S'           FUDATT
062900970626     C                   MOVEL     FUDRIG        FUD(6)
063000060327    1C                   ENDIF
063100060331     c
063200060404IF  1C     opzione       IFEQ      '05'
063300060331     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
063400060331     C                   MOVEL     'N'           OPDATT
063500060331     C                   MOVEL     OPDRIG        OPD(2)
063600060331     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
063700060331     C                   MOVEL     'N'           OPDATT
063800060331     C                   MOVEL     OPDRIG        OPD(3)
063900060331     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
064000060331     C                   MOVEL     'N'           OPDATT
064100060331     C                   MOVEL     OPDRIG        OPD(4)
064200060331     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
064300060331     C                   MOVEL     'N'           FUDATT
064400060331     C                   MOVEL     FUDRIG        FUD(6)
064500060331    1C                   ENDIF
064600060927     c
064700060927     c* L'opzione F7 solo se non richiamato da cappario
064800060927     c                   if        *in10
064900060927     C                   MOVEL     FUD(7)        FUDRIG                         *INSERIMENTO
065000060927     C                   MOVEL     'N'           FUDATT
065100060927     C                   MOVEL     FUDRIG        FUD(7)
065200060927     c                   endif
065300970526     C*
065400970526     C* GESTIONE OPZIONI A VIDEO
065500970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
065600970526     C                   EXSR      OPZGES
065700970526     C*
065800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
065900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
066000970526     C                   EXSR      FUNGES
066100970526     C*
066200970526     C                   ENDSR
066300020919     C*--------------------------------------------------------------------------------------------*
066400020919     C* REPERISCE I DATI UTENTE
066500020919     C*--------------------------------------------------------------------------------------------*
066600020919     C     REPDATIUTE    BEGSR
066700020919     C*
066800020919     C* INIZIALIZZA VARIABILI DI WRK
066900020919     C                   CLEAR                   TIBS34DS
067000020919     C                   CLEAR                   AZUTEDS
067100020919     C                   CLEAR                   DDATIUTE
067200020919     C*
067300020919     C     *DTAARA       DEFINE    §azute        azuteds
067400020919     C     *DTAARA       DEFINE    §datiute      ddatiute
067500020919     C                   IN(E)     *DTAARA
067600020919if  1C                   IF        %Error
067700020919     c                   EVAL      I34Tla = 'L'
067800020919     C                   CALL      'TIBS34R'
067900020919     C                   PARM                    Tibs34Ds
068000020919     C                   IN        *DTAARA
068100020919e   1C                   ENDIF
068200020919     C*
068300020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
068400020919     C                   MOVEL(P)  RSUT          DSFIRS           20
068500020919     C*
068600020919     C                   ENDSR
068700970526     C*------------------------------------------------------------------------*
068800970526     C* *INZSR - OPERAZIONI INIZIALI
068900970526     C*------------------------------------------------------------------------*
069000970526     C     *INZSR        BEGSR
069100970526     C*---
069200970526     C* RICEVIMENTO PARAMETRI
069300970526     C*---
069400970526     C     *ENTRY        PLIST
069500970612     C                   PARM                    KPJBA
069600060922     C                   PARM                    tiSIQ0ds
069700970526     C*
069800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
069900970526     C     XMSGPA        PLIST
070000970526     C                   PARM                    DSMSMS
070100970526     C                   PARM                    DSMSPG
070200970526     C                   PARM                    DSMSKE
070300970526     C                   PARM                    DSMSER
070400970526     C*
070500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
070600970526     C                   CALL      'XKEY'
070700970526     C                   PARM                    DSKEY
070800970526     C*---
070900970526     C* VARIABILI NON RIFERITE AL DATA BASE
071000970526     C*---
071100970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
071200970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
071300970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
071400970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
071500970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
071600970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
071700970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
071800970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
071900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
072000060922     C     *LIKE         DEFINE    DQ0OP0        DEPOP0                         *DI DEPOSITO
072100060922     C     *LIKE         DEFINE    DQ0OP0        WOP0                           *DI LAVORO
072200060922     C     *LIKE         DEFINE    DQ0OP1        WOP1
072300060922     C     *LIKE         DEFINE    Dq0NAR        DNAR
072400060922     C     *LIKE         DEFINE    Dq0CAP        DCAP
072500060922     C     *LIKE         DEFINE    Dq0ntw        Dntw
072600060922     C     *LIKE         DEFINE    Dq0dde        Ddde
072700060922     C     *LIKE         DEFINE    Dq0dsc        Ddsc
072800060922     C     *LIKE         DEFINE    Dq0uti        Duti
072900060922     C     *LIKE         DEFINE    Dq0flo        Dflo
073000970526     C*---
073100970526     C* VARIABILI DI CONTROLLO
073200970526     C*---
073300970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
073400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
073500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
073600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
073700970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
073800970530     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
073900970526     C*
074000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
074100060922     C                   MOVEL     'TISIQ0R'     DSMSPG
074200060922     C                   MOVEL     'TISIQ0R'     MSGQUE
074300970526     C                   MOVEL     *BLANKS       MSGKEY
074400970526     C                   MOVEL     *BLANKS       DSMSMS
074500970526     C                   MOVEL     *BLANKS       DSMSKE
074600970526     C                   Z-ADD     *ZEROS        DSMSER
074700060330     c
074800060922     C     *LIKE         DEFINE    Cppdde        DEPpde                         *DEPOSITI
074900060922     C     *LIKE         DEFINE    cppNAR        DEPNAR                         *DEPOSITI
075000060922     C     *LIKE         DEFINE    CppCAP        DEPCAP
075100060922     C     *LIKE         DEFINE    Cppntw        DEPntw
075200970526     C*---
075300970526     C* CHIAVI DI LETTURA
075400970526     C*---
075500060922     C     kcpp          KLIST
075600060313     C                   KFLD                    Knar                           *CAP
075700060922     C                   KFLD                    Kcap                           *CAP
075800060922     c     Ktab          Klist
075900060922     c                   Kfld                    codut             1 0
076000060922     c                   Kfld                    kCod
076100060922     c                   Kfld                    kKey
076200060922     c                   z-add     1             codut
076300060922     c
076400970527     C* REPERIMENTO DATI UTENTE
076500020919     C                   EXSR      REPDATIUTE
076600970527     C*
076700970527     C* CARICA LE TABELLE OCCORRENTI
076800970527     C                   EXSR      CARTAB
076900970526     C*
077000970526     C* DEPOSITA LE OPZIONI DI ENTRATA
077100060922     C                   MOVEL     DQ0OP0        DEPOP0                         *LIVELLO PROCEDURA
077200060927     c* se richiamato da cappario disabilito tasto funzionale F7-Cappario
077300060927     c                   movel     dq0op0        wchiama           1
077400060927     c                   if        wchiama='C'
077500060927     c                   seton                                        10
077600060927     c                   else
077700060927     c                   setoff                                       10
077800060927     c                   endif
077900970526     C*
078000970526     C* CALCOLA LA DATA CORRENTE
078100970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
078200970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
078300970526     C                   Z-ADD     WN8           G08DAT
078400970526     C                   Z-ADD     *ZEROS        G08INV
078500970526     C                   MOVEL     '0'           G08ERR
078600970526     C                   CALL      'XSRDA8'
078700970526     C                   PARM                    WLBDA8
078800970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
078900060313     c
079000060313     c* Se non passata la data di ingresso, uso data corrente
079100060922     c                   if        DQ0dri=0
079200060922     c                   eval      amgpde=datcor
079300060313     c                   eval      vidpde=wn8
079400060313     c                   else
079500060922     c                   eval      amgpde=DQ0dri
079600060313     c* La imposto a video
079700060922     C                   Z-ADD     DQ0DRI        G08INV
079800060313     C                   Z-ADD     *ZEROS        G08DAT
079900060313     C                   MOVEL     '3'           G08ERR
080000060313     C                   CALL      'XSRDA8'
080100060313     C                   PARM                    WLBDA8
080200060313     C                   Z-ADD     G08DAT        VIDpde                         *DATA RIFERIMENTO
080300060313     c                   endif
080400970526     C*
080500970526     C* IMPOSTA I PARAMETRI DI INGRESSO
080600060922     C                   MOVEL     DQ0NAR        VIDPNA                         *NAZIONE
080700060922     C                   MOVEL     DQ0CAP        VIDPCA                         *CAP
080800060922     C                   MOVEL     DQ0ntw        VIDpnt                         *CAP
080900060313     c
081000970526     C                   EXSR      CHKPAR
081100060922     c
081200970526IF  1C     ERRL00        IFEQ      '1'
081300060922     C                   MOVEL     '1'           DQ0ERR
081400060922     C                   MOVEL     DSMSMS        DQ0MSG
081500970526X   1C                   ELSE
081600970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
081700970527     C                   MOVEL     VIDPCA        DEPCAP
081800060922     C                   MOVEL     VIDpnt        DEPntw
081900060313     C                   MOVEL     VIDpde        DEPpde
082000970526E   1C                   ENDIF
082100970526     C*
082200970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
082300060404     c                   eval      opzione=%subst(depop0:2:2)
082400970526     C                   EXSR      CHKFOP
082500970526     C*
082600970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
082700060404IF  1C     opzione       IFEQ      '01'
082800970530     C                   MOVEL     DEC(3)        VIDING
082900970530E   1C                   ENDIF
083000060404IF  1C     opzione       IFEQ      '02'
083100970530     C                   MOVEL     DEC(1)        VIDING
083200970530E   1C                   ENDIF
083300060404IF  1C     opzione       IFEQ      '05'
083400970530     C                   MOVEL     DEC(2)        VIDING
083500970530E   1C                   ENDIF
083600970526     C*
083700970526     C                   ENDSR
083800970526     C*------------------------------------------------------------------------*
083900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
084000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
084100970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
084200970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
084300970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
084400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
084500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
084600970526     C*------------------------------------------------------------------------*
084700970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
084800970526     C*------------------------------------------------------------------------*
084900970526     C     POSSFL        BEGSR
085000970526     C*
085100970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
085200970526     C                   EXSR      CLRSFL
085300970526     C*
085400970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
085500970526     C                   EXSR      SETFIL
085600970526     C*
085700970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
085800970526     C     SFLEOF        IFEQ      0
085900970526     C                   EXSR      CARSFL
086000970526     C                   END
086100970526     C*
086200970526     C                   ENDSR
086300970526     C*------------------------------------------------------------------------*
086400970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
086500970526     C*------------------------------------------------------------------------*
086600970526     C     ENTATT        BEGSR
086700970526     C*
086800970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
086900970526     C                   Z-ADD     0             OPZOK             1 0
087000970526     C*
087100970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
087200970526     C                   Z-ADD     0             SFLVAR            5 0
087300970526     C*
087400970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
087500970526     C                   Z-ADD     0             SFLERR
087600970526     C*
087700970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
087800970526     C                   Z-ADD     SFLULT        WFLULT
087900970526     C*
088000970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
088100060922     C                   READC     SIQ0D05                                98
088200970526     C     *IN98         DOWEQ     '0'
088300970526     C*
088400970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
088500970526     C                   ADD       1             SFLVAR
088600970526     C*
088700970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
088800970526     C     OPZOK         IFEQ      0
088900970526     C*
089000970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
089100970526     C     OPZKEY        IFNE      *BLANKS
089200970526     C                   EXSR      OPZCON
089300970526     C                   END
089400970526     C*
089500970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
089600970526     C*            2. PULIZIA SCELTA IN SFL
089700970526     C     OPZOK         IFEQ      0
089800970526     C                   MOVEL     '0'           *IN41
089900970526     C                   MOVEL     *BLANKS       OPZKEY
090000970526     C                   ELSE
090100970526     C*
090200970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
090300970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
090400970526     C                   MOVEL     '1'           *IN41
090500970526     C                   Z-ADD     SFLULT        SFLERR
090600970526     C                   END
090700970526     C                   ELSE
090800970526     C*
090900970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
091000970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
091100970526     C                   MOVEL     '1'           *IN41
091200970526     C                   END                                                    OPZOK=0
091300970526     C*
091400970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
091500970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
091600970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
091700970526     C     OPZOK         IFNE      0
091800970526     C                   Z-ADD     SFLERR        SFLNBR
091900970526     C                   ELSE
092000970526     C                   Z-ADD     SFLULT        SFLNBR
092100970526     C                   END
092200970526     C*
092300970526     C* AGGIORNO RECORD SFL
092400060922     C                   UPDATE    SIQ0D05
092500970526     C*
092600060922     C                   READC     SIQ0D05                                98
092700970526     C                   END                                                    DO WHILE
092800970526     C*
092900970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
093000970526     C                   Z-ADD     WFLULT        SFLULT
093100970526     C*
093200970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
093300970526     C     SFLVAR        IFEQ      0
093400970526     C*---                 MOVEL'1'       FINL00
093500970526     C                   END
093600970526     C*
093700970526     C                   ENDSR
093800970526     C*------------------------------------------------------------------------*
093900970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
094000970526     C*------------------------------------------------------------------------*
094100970526     C     CLRSFL        BEGSR
094200970526     C*
094300970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
094400970526     C* (0=NON FINE FILE  1=FINE FILE)
094500970526     C                   Z-ADD     0             SFLEOF
094600970526     C*
094700970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
094800970526     C                   Z-ADD     0             SFLULT
094900970526     C*
095000970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
095100970526     C                   MOVEL     '1'           *IN40
095200060922     C                   WRITE     SIQ0D06
095300970526     C                   MOVEL     '0'           *IN40
095400970526     C*
095500970526     C                   ENDSR
095600970526     C*------------------------------------------------------------------------*
095700970526     C* CARSFL - CARICAMENTO SFL
095800970526     C*------------------------------------------------------------------------*
095900970526     C     CARSFL        BEGSR
096000970526     C*
096100970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
096200970526     C     SFLULT        IFEQ      0
096300970526     C                   EXSR      SETFIL
096400970526     C                   END
096500970526     C*
096600970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
096700970526     C                   Z-ADD     SFLULT        WFLULT
096800970526     C*
096900970526     C* CICLO:
097000970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
097100970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
097200970526     C                   Z-ADD     1             SFLRIG
097300970526     C     SFLRIG        DOWLE     SFLPAG
097400970526     C     SFLEOF        ANDEQ     0
097500970526     C*
097600970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
097700970526     C                   MOVEL     *BLANKS       OPZKEY
097800970526     C*
097900970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
098000970526     C                   EXSR      IRESFL
098100970526     C*
098200970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
098300970526     C                   ADD       1             SFLULT
098400970526     C*
098500970526     C* SCRITTURA RECORD SFL
098600060922     C                   WRITE     SIQ0D05
098700970526     C                   ADD       1             SFLRIG
098800970526     C*
098900970526     C* LETTURA FILE DA CARICARE
099000970526     C                   EXSR      LETFIL
099100970526     C                   END                                                    DO WHILE
099200970526     C*
099300970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
099400970526     C     WFLULT        IFNE      SFLULT
099500970526     C                   Z-ADD     WFLULT        SFLNBR
099600970526     C                   ADD       1             SFLNBR
099700970526     C                   END
099800970526     C*
099900970526     C                   ENDSR
100000970526     C*------------------------------------------------------------------------*
100100970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
100200970526     C*------------------------------------------------------------------------*
100300970526     C     OUTSFL        BEGSR
100400970526     C*
100500970526     C* WRITE RECORD TESTATA VIDEO
100600060922     C                   WRITE     SIQ0D01
100700970526     C*
100800970526     C* VISUALIZZAZIONE MESSAGGIO
100900970526     C                   EXSR      SNDMSG
101000970526     C*
101100970526     C* WRITE RECORD OPZIONI VIDEO
101200060922     C                   WRITE     SIQ0D02
101300970526     C*
101400970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
101500060922     C                   WRITE     SIQ0D03
101600970526     C*
101700970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
101800060922     C                   WRITE     SIQ0D04
101900970526     C*
102000970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
102100970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
102200970526     C     SFLEOF        IFEQ      0
102300970526     C                   MOVEL     '0'           *IN42
102400970526     C                   ELSE
102500970526     C*
102600970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
102700970526     C                   MOVEL     '1'           *IN42
102800970526     C                   END
102900970526     C*
103000970526     C* WRITE RECORD TASTI FUNZIONALI
103100060922     C                   WRITE     SIQ0D09
103200970526     C*
103300970526     C* CASO DI SFL VUOTO
103400970526     C     SFLULT        IFLE      0
103500970526     C*
103600970526     C*    - WRITE RECORD VUOTO
103700060922     C                   WRITE     SIQ0D0A
103800060922     C                   READ      SIQ0D0A                                99
103900970526     C*
104000970526     C                   ELSE
104100970526     C*
104200970526     C* CASO DI SFL CON RECORDS
104300970526     C*
104400970526     C*    - WRITE RECORD CONTROLLO SFL
104500060922     C                   WRITE     SIQ0D06
104600060922     C                   READ      SIQ0D06                                99
104700970526     C                   END
104800970526     C*
104900970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
105000060922     C                   READ      SIQ0D03                                99
105100970526     C*
105200970526     C                   ENDSR
105300970526     C*------------------------------------------------------------------------*
105400970526     C* SNDMSG - MANDA MESSAGGIO
105500970526     C*------------------------------------------------------------------------*
105600970526     C     SNDMSG        BEGSR
105700970526     C*
105800970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
105900970526     C                   MOVEL     '1'           *IN40
106000060922     C                   WRITE     SIQ0DMC
106100970526     C                   MOVEL     '0'           *IN40
106200970526     C*
106300970526     C* VISUALIZZAZIONE MESSAGGIO
106400970526     C                   Z-ADD     0             MSGULT
106500970526     C                   CALL      'XMSG'        XMSGPA
106600970526     C     DSMSER        IFEQ      0
106700970526     C                   MOVEL     DSMSKE        MSGKEY
106800970526     C                   END
106900970526     C                   Z-ADD     1             MSGULT
107000060922     C                   WRITE     SIQ0DMS                                      SFL MESS
107100060922     C                   WRITE     SIQ0DMC                                      CONTR MESS
107200970526     C*
107300970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
107400970526     C                   MOVEL     *BLANKS       DSMSMS
107500970526     C*
107600970526     C                   ENDSR
107700970526     C*------------------------------------------------------------------------*
107800970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
107900970526     C*------------------------------------------------------------------------*
108000970526     C     FUNCON        BEGSR
108100970526     C*
108200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
108300970526     C                   EXSR      FUNVAR
108400970526     C*
108500970526     C* PULISCO CAMPI DI LAVORO
108600970526     C                   Z-ADD     0             I                 5 0
108700970526     C                   Z-ADD     0             W                 5 0
108800970526     C*
108900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
109000970526     C     FUNKEY        IFNE      *BLANKS
109100970526     C     1             DO        24            I
109200970526     C                   MOVEL     FUD(I)        FUDRIG
109300970526     C*
109400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
109500970526     C     FUNKEY        IFEQ      FUDFUN
109600970526     C     FUDATT        ANDEQ     'S'
109700970526     C                   Z-ADD     I             W
109800970526     C                   END
109900970526     C                   END                                                    DO WHILE
110000970526     C*
110100970526     C* FUNZIONE NON ATTIVA (ERRORE)
110200970526     C     W             IFEQ      0
110300970526     C*
110400970526     C* EMISSIONE MESSAGGIO DI ERRORE
110500970526     C                   MOVEL     MSG(2)        DSMSMS
110600970526     C                   END                                                    W=0
110700970526     C                   END                                                    FUNKEY NE *BLANKS
110800970526     C*
110900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
111000970526     C     W             IFNE      0
111100970526     C     FUNKEY        OREQ      *BLANKS
111200970526     C*
111300970526     C     FUNKEY        IFEQ      *BLANKS
111400970526     C                   EXSR      ENTGES
111500970526     C                   END
111600970526     C     FUNKEY        IFEQ      '03'
111700970526     C                   EXSR      F03GES
111800970526     C                   END
111900970526     C     FUNKEY        IFEQ      '05'
112000970526     C                   EXSR      F05GES
112100970526     C                   END
112200970526     C     FUNKEY        IFEQ      '06'
112300970526     C                   EXSR      F06GES
112400970526     C                   END
112500060926     C     FUNKEY        IFEQ      '07'
112600060926     C                   EXSR      F07GES
112700060926     C                   END
112800970526     C     FUNKEY        IFEQ      '12'
112900970526     C                   EXSR      F12GES
113000970526     C                   END
113100970529     C     FUNKEY        IFEQ      '13'
113200970529     C                   EXSR      F13GES
113300970529     C                   END
113400970526     C     FUNKEY        IFEQ      '23'
113500970526     C                   EXSR      F23GES
113600970526     C                   END
113700970526     C     FUNKEY        IFEQ      '24'
113800970526     C                   EXSR      F24GES
113900970526     C                   END
114000970526     C     FUNKEY        IFEQ      'RU'
114100970526     C                   EXSR      RUPGES
114200970526     C                   END
114300970526     C*
114400970526     C                   END
114500970526     C*
114600970526     C                   ENDSR
114700970526     C*------------------------------------------------------------------------*
114800970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
114900970526     C*------------------------------------------------------------------------*
115000970526     C     FUNVAR        BEGSR
115100970526     C*
115200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
115300970526     C     KEY           IFEQ      ENTER
115400970526     C                   MOVEL     *BLANKS       FUNKEY
115500970526     C                   END
115600970526     C     KEY           IFEQ      F1
115700970526     C                   MOVEL     '01'          FUNKEY
115800970526     C                   END
115900970526     C     KEY           IFEQ      F2
116000970526     C                   MOVEL     '02'          FUNKEY
116100970526     C                   END
116200970526     C     KEY           IFEQ      F3
116300970526     C                   MOVEL     '03'          FUNKEY
116400970526     C                   END
116500970526     C     KEY           IFEQ      F4
116600970526     C                   MOVEL     '04'          FUNKEY
116700970526     C                   END
116800970526     C     KEY           IFEQ      F5
116900970526     C                   MOVEL     '05'          FUNKEY
117000970526     C                   END
117100970526     C     KEY           IFEQ      F6
117200970526     C                   MOVEL     '06'          FUNKEY
117300970526     C                   END
117400970526     C     KEY           IFEQ      F7
117500970526     C                   MOVEL     '07'          FUNKEY
117600970526     C                   END
117700970526     C     KEY           IFEQ      F8
117800970526     C                   MOVEL     '08'          FUNKEY
117900970526     C                   END
118000970526     C     KEY           IFEQ      F9
118100970526     C                   MOVEL     '09'          FUNKEY
118200970526     C                   END
118300970526     C     KEY           IFEQ      F10
118400970526     C                   MOVEL     '10'          FUNKEY
118500970526     C                   END
118600970526     C     KEY           IFEQ      F11
118700970526     C                   MOVEL     '11'          FUNKEY
118800970526     C                   END
118900970526     C     KEY           IFEQ      F12
119000970526     C                   MOVEL     '12'          FUNKEY
119100970526     C                   END
119200970526     C     KEY           IFEQ      F13
119300970526     C                   MOVEL     '13'          FUNKEY
119400970526     C                   END
119500970526     C     KEY           IFEQ      F14
119600970526     C                   MOVEL     '14'          FUNKEY
119700970526     C                   END
119800970526     C     KEY           IFEQ      F15
119900970526     C                   MOVEL     '15'          FUNKEY
120000970526     C                   END
120100970526     C     KEY           IFEQ      F16
120200970526     C                   MOVEL     '16'          FUNKEY
120300970526     C                   END
120400970526     C     KEY           IFEQ      F17
120500970526     C                   MOVEL     '17'          FUNKEY
120600970526     C                   END
120700970526     C     KEY           IFEQ      F18
120800970526     C                   MOVEL     '18'          FUNKEY
120900970526     C                   END
121000970526     C     KEY           IFEQ      F19
121100970526     C                   MOVEL     '19'          FUNKEY
121200970526     C                   END
121300970526     C     KEY           IFEQ      F20
121400970526     C                   MOVEL     '20'          FUNKEY
121500970526     C                   END
121600970526     C     KEY           IFEQ      F21
121700970526     C                   MOVEL     '21'          FUNKEY
121800970526     C                   END
121900970526     C     KEY           IFEQ      F22
122000970526     C                   MOVEL     '22'          FUNKEY
122100970526     C                   END
122200970526     C     KEY           IFEQ      F23
122300970526     C                   MOVEL     '23'          FUNKEY
122400970526     C                   END
122500970526     C     KEY           IFEQ      F24
122600970526     C                   MOVEL     '24'          FUNKEY
122700970526     C                   END
122800970526     C     KEY           IFEQ      ROLLUP
122900970526     C                   MOVEL     'RU'          FUNKEY
123000970526     C                   END
123100970526     C     KEY           IFEQ      HELP
123200970526     C                   MOVEL     'HP'          FUNKEY
123300970526     C                   END
123400970526     C*
123500970526     C                   ENDSR
123600970526     C*------------------------------------------------------------------------*
123700970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
123800970526     C*------------------------------------------------------------------------*
123900970526     C     FUNGES        BEGSR
124000970526     C*
124100970526     C* PULISCO CAMPI DI LAVORO
124200970526     C                   Z-ADD     0             W                 5 0
124300970526     C*
124400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
124500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
124600970526     C     1             DO        24            I
124700970526     C                   MOVEL     FUD(I)        FUDRIG
124800970526     C*
124900970526     C* PULIZIA FUNZIONI PRECEDENTI
125000970526     C                   MOVEL     *BLANKS       FUV(I)
125100970526     C*
125200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
125300970526     C* (SALTA F23 E F24)
125400970526     C     FUDATT        IFEQ      'S'
125500970526     C     FUDFUN        ANDNE     '23'
125600970526     C     FUDFUN        ANDNE     '24'
125700970526     C                   ADD       1             W
125800970526     C                   MOVEL     FUDDES        FUV(W)
125900970526     C                   END
126000970526     C                   END                                                    DO WHILE
126100970526     C*
126200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
126300970526     C* ALTRIMENTI DISATTIVO F24
126400970526     C     1             DO        24            I
126500970526     C                   MOVEL     FUD(I)        FUDRIG
126600970526     C*
126700970526     C     FUDFUN        IFEQ      '24'
126800970526     C     W             IFGT      6
126900970526     C                   MOVEL     'S'           FUDATT
127000970526     C                   ELSE
127100970526     C                   MOVEL     *BLANKS       FUDATT
127200970526     C                   END                                                    W GT 8
127300970526     C                   MOVEL     FUDRIG        FUD(I)
127400970526     C                   END
127500970526     C                   END                                                    DO WHILE
127600970526     C*
127700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
127800970526     C     W             DIV       6             I
127900970526     C                   MVR                     Z
128000970526     C*
128100970526     C     Z             IFNE      0
128200970526     C     I             ADD       1             Z
128300970526     C                   ELSE
128400970526     C                   Z-ADD     I             Z
128500970526     C                   END
128600970526     C*
128700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
128800970526     C                   ADD       1             FUNGRU
128900970526     C     FUNGRU        IFGT      Z
129000970526     C     FUNGRU        ORLT      1
129100970526     C                   Z-ADD     1             FUNGRU
129200970526     C                   END
129300970526     C*
129400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
129500970526     C     FUNGRU        IFEQ      1
129600970526     C                   MOVEL     FUV(1)        FUN001
129700970526     C                   MOVEL     FUV(2)        FUN002
129800970526     C                   MOVEL     FUV(3)        FUN003
129900970526     C                   MOVEL     FUV(4)        FUN004
130000970526     C                   MOVEL     FUV(5)        FUN005
130100970526     C                   MOVEL     FUV(6)        FUN006
130200970526     C                   END
130300970526     C*
130400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
130500970526     C     FUNGRU        IFEQ      2
130600970526     C                   MOVEL     FUV(7)        FUN001
130700970526     C                   MOVEL     FUV(8)        FUN002
130800970526     C                   MOVEL     FUV(9)        FUN003
130900970526     C                   MOVEL     FUV(10)       FUN004
131000970526     C                   MOVEL     FUV(11)       FUN005
131100970526     C                   MOVEL     FUV(12)       FUN006
131200970526     C                   END
131300970526     C*
131400970526     C* TERZO GRUPPO DI 6 FUNZIONI
131500970526     C     FUNGRU        IFEQ      3
131600970526     C                   MOVEL     FUV(13)       FUN001
131700970526     C                   MOVEL     FUV(14)       FUN002
131800970526     C                   MOVEL     FUV(15)       FUN003
131900970526     C                   MOVEL     FUV(16)       FUN004
132000970526     C                   MOVEL     FUV(17)       FUN005
132100970526     C                   MOVEL     FUV(18)       FUN006
132200970526     C                   END
132300970526     C*
132400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
132500970526     C     FUNGRU        IFEQ      4
132600970526     C                   MOVEL     FUV(19)       FUN001
132700970526     C                   MOVEL     FUV(20)       FUN002
132800970526     C                   MOVEL     FUV(21)       FUN003
132900970526     C                   MOVEL     FUV(22)       FUN004
133000970526     C                   MOVEL     *BLANKS       FUN005
133100970526     C                   MOVEL     *BLANKS       FUN006
133200970526     C                   END
133300970526     C*
133400970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
133500970526     C                   MOVEL     *BLANKS       FUN007
133600970526     C                   MOVEL     *BLANKS       FUN008
133700970526     C*
133800970526     C     1             DO        24            I
133900970526     C                   MOVEL     FUD(I)        FUDRIG
134000970526     C     FUDFUN        IFEQ      '23'
134100970526     C     FUDATT        ANDEQ     'S'
134200970526     C                   MOVEL     FUDDES        FUN007
134300970526     C                   END
134400970526     C     FUDFUN        IFEQ      '24'
134500970526     C     FUDATT        ANDEQ     'S'
134600970526     C                   MOVEL     FUDDES        FUN008
134700970526     C                   END
134800970526     C                   END                                                    DO
134900970526     C*
135000970526     C                   ENDSR
135100970526     C*------------------------------------------------------------------------*
135200970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
135300970526     C*------------------------------------------------------------------------*
135400970526     C     OPZGES        BEGSR
135500970526     C*
135600970526     C* PULISCO CAMPI DI LAVORO
135700970526     C                   Z-ADD     0             W                 5 0
135800970526     C*
135900970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
136000970526     C* E CARICO TABELLA OPZIONI ATTIVE
136100970526     C     1             DO        24            I
136200970526     C                   MOVEL     OPD(I)        OPDRIG
136300970526     C*
136400970526     C* PULIZIA OPZIONI PRECEDENTI
136500970526     C                   MOVEL     *BLANKS       OPV(I)
136600970526     C*
136700970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
136800970526     C     OPDATT        IFEQ      'S'
136900970526     C                   ADD       1             W
137000970526     C                   MOVEL     OPDDES        OPV(W)
137100970526     C                   END
137200970526     C                   END                                                    DO WHILE
137300970526     C*
137400970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
137500970526     C* ALTRIMENTI DISATTIVO F23
137600970526     C     1             DO        24            I
137700970526     C                   MOVEL     FUD(I)        FUDRIG
137800970526     C*
137900970526     C     FUDFUN        IFEQ      '23'
138000970526     C     W             IFGT      8
138100970526     C                   MOVEL     'S'           FUDATT
138200970526     C                   ELSE
138300970526     C                   MOVEL     *BLANKS       FUDATT
138400970526     C                   END                                                    W GT 8
138500970526     C                   MOVEL     FUDRIG        FUD(I)
138600970526     C                   END
138700970526     C                   END                                                    DO WHILE
138800970526     C*
138900970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
139000970526     C     W             DIV       8             I
139100970526     C                   MVR                     Z                 5 0
139200970526     C*
139300970526     C     Z             IFNE      0
139400970526     C     I             ADD       1             Z
139500970526     C                   ELSE
139600970526     C                   Z-ADD     I             Z
139700970526     C                   END
139800970526     C*
139900970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
140000970526     C                   ADD       1             OPZGRU
140100970526     C     OPZGRU        IFGT      Z
140200970526     C     OPZGRU        ORLT      1
140300970526     C                   Z-ADD     1             OPZGRU
140400970526     C                   END
140500970526     C*
140600970526     C* PRIMO GRUPPO DI 8 OPZIONI
140700970526     C     OPZGRU        IFEQ      1
140800970526     C                   MOVEL     OPV(1)        OPZ001
140900970526     C                   MOVEL     OPV(2)        OPZ002
141000970526     C                   MOVEL     OPV(3)        OPZ003
141100970526     C                   MOVEL     OPV(4)        OPZ004
141200970526     C                   MOVEL     OPV(5)        OPZ005
141300970526     C                   MOVEL     OPV(6)        OPZ006
141400970526     C                   MOVEL     OPV(7)        OPZ007
141500970526     C                   MOVEL     OPV(8)        OPZ008
141600970526     C                   END
141700970526     C*
141800970526     C* SECONDO GRUPPO DI 8 OPZIONI
141900970526     C     OPZGRU        IFEQ      2
142000970526     C                   MOVEL     OPV(9)        OPZ001
142100970526     C                   MOVEL     OPV(10)       OPZ002
142200970526     C                   MOVEL     OPV(11)       OPZ003
142300970526     C                   MOVEL     OPV(12)       OPZ004
142400970526     C                   MOVEL     OPV(13)       OPZ005
142500970526     C                   MOVEL     OPV(14)       OPZ006
142600970526     C                   MOVEL     OPV(15)       OPZ007
142700970526     C                   MOVEL     OPV(16)       OPZ008
142800970526     C                   END
142900970526     C*
143000970526     C* TERZO GRUPPO DI 8 OPZIONI
143100970526     C     OPZGRU        IFEQ      3
143200970526     C                   MOVEL     OPV(17)       OPZ001
143300970526     C                   MOVEL     OPV(18)       OPZ002
143400970526     C                   MOVEL     OPV(19)       OPZ003
143500970526     C                   MOVEL     OPV(20)       OPZ004
143600970526     C                   MOVEL     OPV(21)       OPZ005
143700970526     C                   MOVEL     OPV(22)       OPZ006
143800970526     C                   MOVEL     OPV(23)       OPZ007
143900970526     C                   MOVEL     OPV(24)       OPZ008
144000970526     C                   END
144100970526     C*
144200970526     C                   ENDSR
144300970526     C*------------------------------------------------------------------------*
144400970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
144500970526     C*------------------------------------------------------------------------*
144600970526     C     OPZCON        BEGSR
144700970526     C*
144800970526     C* PULISCO CAMPI DI LAVORO
144900970526     C                   Z-ADD     0             I                 5 0
145000970526     C                   Z-ADD     0             W                 5 0
145100970526     C*
145200970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
145300970526     C     1             DO        24            I
145400970526     C                   MOVEL     OPD(I)        OPDRIG
145500970526     C*
145600970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
145700970526     C     OPZKEY        IFEQ      OPDOPZ
145800970526     C     OPZKEY        OREQ      OPDOP1
145900970526     C*
146000970526     C     OPDATT        IFEQ      'S'
146100970526     C                   MOVEL     OPDOPZ        OPZKEY
146200970526     C                   Z-ADD     I             W
146300970526     C                   END
146400970526     C                   END
146500970526     C*
146600970526     C                   END                                                    DO WHILE
146700970526     C*
146800970526     C* OPZIONE NON ATTIVA (ERRORE)
146900970526     C     W             IFEQ      0
147000970526     C*
147100970526     C* OPZIONE NON OK
147200970526     C                   Z-ADD     1             OPZOK
147300970526     C*
147400970526     C* EMISSIONE MESSAGGIO DI ERRORE
147500970526     C                   MOVEL     MSG(1)        DSMSMS
147600970526     C                   ELSE
147700970526     C*
147800970526     C* OPZIONE ATTIVA (PROCEDI ...)
147900970526     C*
148000970526     C     OPZKEY        IFEQ      ' 1'
148100970526     C                   EXSR      O01GES
148200970526     C                   END
148300970526     C     OPZKEY        IFEQ      ' 2'
148400060328     C                   EXSR      O02GES
148500970526     C                   END
148600970611     C     OPZKEY        IFEQ      ' 3'
148700970611     C                   EXSR      O03GES
148800970611     C                   END
148900970526     C     OPZKEY        IFEQ      ' 4'
149000970526     C                   EXSR      O04GES
149100970526     C                   END
149200970526     C     OPZKEY        IFEQ      ' 5'
149300970526     C                   EXSR      O05GES
149400970526     C                   END
149500970526     C                   END                                                    OPZ.NON ATTIVA
149600970526     C*
149700970526     C                   ENDSR
149800970526     C*------------------------------------------------------------------------*
149900970526     C* NOTE COSTRUZIONE SCHIERE
150000970526     C*------------------------------------------------------------------------*
150100970526     C*
150200970526     C* -MSG-MESSAGGI
150300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
150400970526     C* (NON TOCCARE MSG,1 E MSG,2)
150500970526     C*
150600970526     C* -CMD-COMANDI
150700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
150800970526     C*
150900970526     C* -OPD-OPZIONI
151000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
151100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
151200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
151300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
151400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
151500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
151600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
151700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
151800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
151900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
152000970526     C*
152100970526     C* -FUD-FUNZIONI
152200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
152300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
152400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
152500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
152600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
152700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
152800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
152900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
153000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
153100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
153200970526     O*------------------------------------------------------------------------*
153300970526** DEC - DECODIFICHE
153400970526    GESTIONE                                                     1
153500970530INTERROGAZIONE                                                   2
153600970530     SCELTA                                                      3
1537009705264
1538009705265
153900970526** MSG - MESSAGGI
154000060922TISIQ0R- Opzione non gestita                                                   1
154100060922TISIQ0R- Tasto funzionale non gestito                                          2
154200060922TISIQ0R- Data di riferimento errata
154300060922TISIQ0R- Network inesistente                                                   4
154400060922TISIQ0R-                                                                       5
154500060922TISIQ0R- Accettato solo un parametro di inizio lista (P.O. o Naz/CAP)          6
154600060922TISIQ0R- 7                                                                     7
154700060922TISIQ0R- 8                                                                     8
154800060922TISIQ0R- 9                                                                     9
154900060922TISIQ0R- 10                                                                   10
155000060922TISIQ0R- 11                                                                   11
155100060922TISIQ0R- 12                                                                   12
155200060922TISIQ0R- 13                                                                   13
155300060922TISIQ0R- 14                                                                   14
155400060922TISIQ0R- 15                                                                   15
155500060922TISIQ0R- 16                                                                   16
155600060922TISIQ0R- 17                                                                   17
155700060922TISIQ0R- 18                                                                   18
155800060922TISIQ0R- 19                                                                   19
155900060922TISIQ0R- 20                                                                   20
156000970526** OPD - OPZIONI
156100970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
156200060315 2=Modifica        -2 -     (S/N)=S                        opz. 02¬
156300060315 3=Duplica         -3 -     (S/N)=S                        opz. 03¬
156400060315 4=Annullamento    -4 -     (S/N)=S                        opz. 04¬
156500060315 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
156600970526                   -6 -     (S/N)=N                        opz. 06¬
156700060315                   -7 -     (S/N)=N                        opz. 07¬
156800970527                   -8 -     (S/N)=N                        opz. 08¬
156900970527                   -9 -     (S/N)=N                        opz. 09¬
157000970527                   -10-     (S/N)=N                        opz. 10¬
157100970529                   -11-     (S/N)=N                        opz. 11¬
157200970529                   -12-     (S/N)=N                        opz. 12¬
157300060922                   -13-     (S/N)=N                        opz. 13¬
157400970526                   -  -     (S/N)=N                        opz. 14¬
157500970526                   -  -     (S/N)=N                        opz. 15¬
157600970526                   -  -     (S/N)=N                        opz. 16¬
157700970526                   -  -     (S/N)=N                        opz. 17¬
157800970526                   -  -     (S/N)=N                        opz. 18¬
157900970526                   -  -     (S/N)=N                        opz. 19¬
158000970526                   -  -     (S/N)=N                        opz. 20¬
158100970526                   -  -     (S/N)=N                        opz. 21¬
158200970526                   -  -     (S/N)=N                        opz. 22¬
158300970526                   -  -     (S/N)=N                        opz. 23¬
158400970526                   -  -     (S/N)=N                        opz. 24¬
158500970526** FUD - FUNZIONI
158600970526                   -        (S/N)=N                        funz.01¬
158700970526                   -        (S/N)=N                        funz.02¬
158800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
158900970526                   -        (S/N)=N                        funz.04¬
159000970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
159100060315F06=Inserimento    -        (S/N)=S                        funz.06¬
159200060926F07=Cappario       -        (S/N)=S                        funz.07¬
159300970527                   -        (S/N)=N                        funz.08¬
159400970527                   -        (S/N)=N                        funz.09¬
159500970527                   -        (S/N)=N                        funz.10¬
159600970526                   -        (S/N)=N                        funz.11¬
159700970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
159800060922                   -        (S/N)=N                        funz.13¬
159900970526                   -        (S/N)=N                        funz.14¬
160000970526                   -        (S/N)=N                        funz.15¬
160100970526                   -        (S/N)=N                        funz.16¬
160200970526                   -        (S/N)=N                        funz.17¬
160300970526                   -        (S/N)=N                        funz.18¬
160400970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
160500970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
160600970527                   -        (S/N)=N                        funz.21¬
160700970527                   -        (S/N)=N                        funz.22¬
160800970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
160900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
