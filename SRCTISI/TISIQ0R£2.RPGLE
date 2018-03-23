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
008900061030     D dcppflo       E DS
009000970526     D*------------
009100970526     D KPJBA         E DS
009200060313     d
009300060313     d Dataiso         s               d   datfmt(*iso)
009400060313     d Dataeur         s               d   datfmt(*eur)
009500060329     d Datadmy         s               d   datfmt(*dmy)
009600060922     d knar            s                   Like(cppnar)
009700060922     d kcap            s                   Like(cppcap)
009800060329     d kcod            s                   Like(tblCod)
009900060329     d kkey            s                   Like(TblKey)
010000060404     d opzione         s              2
010100970526     C*------------------------------------------------------------------------*
010200970526     C* MAIN LINES
010300970526     C*------------------------------------------------------------------------*
010400970526     C*
010500970526     C* SE NON CI SONO ERRORI, PROSEGUE
010600970526IF  1C     ERRL00        IFNE      '1'
010700970526     C*
010800970526     C* CARICA IL SFILE
010900970526     C                   EXSR      POSSFL
011000970526     C*
011100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011200970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011500970526     C*
011600970526     C* REIMPOSTA ERRORE DI PROCERURA
011700970526     C                   MOVEL     '0'           ERRL00
011800970526     C*
011900970526     C* EMETTE IL SFILE
012000970526     C                   EXSR      OUTSFL
012100970526     C*
012200970526     C* GESTIONE TASTI FUNZIONALI
012300970526     C                   EXSR      FUNCON
012400970526     C*
012500970526E   2C                   ENDDO
012600970527     C*
012700970527     C* CI SONO ERRORI IN ENTRATA
012800970527X   1C                   ELSE
012900060922     C                   MOVEL     '1'           dq0ERR
013000060922     C                   MOVEL     DSMSMS        dq0MSG
013100970526E   1C                   ENDIF
013200970526     C* FINE PGM
013300970526     C                   SETON                                        LR
013400970526     C*------------------------------------------------------------------------*
013500970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
013600970526     C*------------------------------------------------------------------------*
013700970526     C     F03GES        BEGSR
013800970526     C*
013900970526     C* IMPOSTA "PREMUTO F03"
014000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014100060922     C                   MOVEL     '1'           dq0F03                         *DI PROCEDURA
014200970526     C*
014300970526     C                   ENDSR
014400970526     C*------------------------------------------------------------------------*
014500970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
014600970526     C*------------------------------------------------------------------------*
014700970526     C     F05GES        BEGSR
014800970526     C*
014900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
015000970526     C*
015100970526     C                   ENDSR
015200970526     C*------------------------------------------------------------------------*
015300970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
015400970526     C*------------------------------------------------------------------------*
015500970526     C     F06GES        BEGSR
015600970526     C*
015700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
015800970603     C                   MOVEL     *BLANKS       DNAR
015900970602     C                   MOVEL     *BLANKS       DCAP
016000060922     c                   clear                   dntw
016100060922     c                   clear                   ddde
016200060922     c                   clear                   ddsc
016300060922     c                   clear                   duti
016400060922     C                   MOVEL     *BLANKS       Dflo
016500970529     C                   MOVEL     DEPOP0        WOP0
016600970529     C                   MOVEL     'F06'         WOP1
016700060922     C                   MOVEL     'TISIQ1R'     WPGM              8
016800060314     C                   EXSR      LANPGMp                                      *LANCIA PGM
016900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017000970526E   1C                   ENDIF
017100970526     C*
017200970526     C                   ENDSR
017300060926     C*------------------------------------------------------------------------*
017400060926     C* F07GES - GESTIONE F07 -> int cappario
017500060926     C*------------------------------------------------------------------------*
017600060926     C     F07GES        BEGSR
017700060926     C* IMPOSTA LA DS DI PROCEDURA
017800060926     c                   clear                   tisi00ds
017900060926     c                   if        amgpde>0
018000060926     c                   movel     amgpde        d00dri
018100060926     c                   else
018200060926     c                   movel     datcor        d00dri
018300060926     c                   endif
018400060927     C                   MOVEL     'C05'         d00OP0                         *LIVELLO PROCEDURA
018500060926     c
018600060926     C                   clear                   d00OP1                         *LIVELLO PGM
018700060926     C                   MOVEL     '0'           d00F03                         *PREMUTO F03=FINE
018800060926     C                   MOVEL     '0'           d00F12                         *PREMUTO F12=RITORNO
018900060926     C                   MOVEL     '0'           d00ERR                         *'1'=ERRORE
019000060926     C                   MOVEL     *BLANKS       d00MSG                         *MESSAGGIO DI RITORN
019100060926     C                   clear                   d00NAR                         *NAZIONE
019200060926     C                   clear                   d00CAP                         *CAP
019300060926     C                   clear                   d00LOC                         *LOCALITA'
019400060926     C*
019500060926     C                   CALL      'TISI02R'
019600060926     C                   PARM                    KPJBA
019700060926     C                   PARM                    tisi00ds
019800060926     C                   ENDSR
019900970526     C*------------------------------------------------------------------------*
020000970526     C* F12GES - GESTIONE F12 -> RITORNO
020100970526     C*------------------------------------------------------------------------*
020200970526     C     F12GES        BEGSR
020300970526     C*
020400970526     C* IMPOSTA "PREMUTO F12"
020500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020600060922     C                   MOVEL     '1'           dq0F12                         *DI PROCEDURA
020700970526     C*
020800970526     C                   ENDSR
020900970529     C*------------------------------------------------------------------------*
021000060922     C* F13GES - GESTIONE F13 ->
021100970529     C*------------------------------------------------------------------------*
021200970529     C     F13GES        BEGSR
021300970529     C*
021400970529     C                   ENDSR
021500970526     C*------------------------------------------------------------------------*
021600970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
021700970526     C*------------------------------------------------------------------------*
021800970526     C     F23GES        BEGSR
021900970526     C*
022000970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022100970526     C*
022200970526     C                   ENDSR
022300970526     C*------------------------------------------------------------------------*
022400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022500970526     C*------------------------------------------------------------------------*
022600970526     C     F24GES        BEGSR
022700970526     C*
022800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022900970526     C*
023000970526     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023300970526     C*------------------------------------------------------------------------*
023400970526     C     RUPGES        BEGSR
023500970526     C*
023600970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
023700970526     C*
023800970526     C                   ENDSR
023900970526     C*------------------------------------------------------------------------*
024000970526     C* ENTGES - GESTIONE ENTER -> INVIO
024100970526     C*------------------------------------------------------------------------*
024200970526     C     ENTGES        BEGSR
024300970526     C*
024400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
024500970526     C                   EXSR      NEWPOS
024600970526     C*
024700970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
024800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024900970526IF  2C     SWPOSI        IFNE      0
025000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
025100970526X   2C                   ELSE
025200970526IF  3C     SFLULT        IFNE      0
025300970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025400970526     C*
025500970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
025600970526IF  4C     SWMODI        IFNE      0
025700970526     C                   EXSR      POSSFL
025800970526E   4C                   ENDIF
025900970526E   3C                   ENDIF
026000970526E   2C                   ENDIF
026100970526E   1C                   ENDIF
026200970526     C*
026300970526     C                   ENDSR
026400970526     C*------------------------------------------------------------------------*
026500970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
026600970526     C*------------------------------------------------------------------------*
026700970526     C     O01GES        BEGSR
026800970526     C*
026900970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
027000060922     C                   MOVEL     V5cNAR        dq0NAR
027100060922     C                   MOVEL     V5cCAP        dq0CAP
027200060922     C                   MOVEL     V5cntw        dq0ntw
027300060922     C                   MOVEL     V5hdde        dq0dde
027400060922     C                   MOVEL     V5hdsc        dq0dsc
027500060922     C                   MOVEL     V5cuti        dq0uti
027600060922     C                   MOVEL     v5hflo        Dq0flo
027700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
027800970526     C*
027900970526     C                   ENDSR
028000970526     C*------------------------------------------------------------------------*
028100970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
028200970526     C*------------------------------------------------------------------------*
028300970526     C     O02GES        BEGSR
028400970526     C*
028500970527     C* MODIFICA
028600970704IF  1C     ERRL00        IFEQ      '0'
028700060922     C                   MOVEL     V5cNAR        DNAR
028800060922     C                   MOVEL     V5cCAP        DCAP
028900060922     C                   MOVEL     V5cntw        Dntw
029000060922     C                   MOVEL     V5cuti        Duti
029100060922     C                   MOVEL     V5hflo        Dflo
029200060922     C                   MOVEL     V5hdde        Ddde
029300060922     C                   MOVEL     V5hdsc        Ddsc
029400970529     C                   MOVEL     DEPOP0        WOP0
029500970526     C                   MOVEL     'O02'         WOP1
029600060925     C                   MOVEL     'TISIQ1R'     WPGM
029700060315     C                   EXSR      LANPGMP
029800060328     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
029900970528E   1C                   ENDIF
030000970526     C*
030100970526     C                   ENDSR
030200970611     C*------------------------------------------------------------------------*
030300970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
030400970611     C*------------------------------------------------------------------------*
030500970611     C     O03GES        BEGSR
030600970611     C*
030700970611     C* DUPLICA
030800970611IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030900060922     C                   MOVEL     V5cNAR        DNAR
031000060922     C                   MOVEL     V5cCAP        DCAP
031100060922     C                   MOVEL     V5cntw        Dntw
031200060922     C                   MOVEL     V5cuti        Duti
031300060922     C                   MOVEL     V5hflo        Dflo
031400060922     C                   MOVEL     V5hdde        Ddde
031500060922     C                   MOVEL     V5hdsc        Ddsc
031600060329     C                   MOVEL     DEPOP0        WOP0
031700060329     C                   MOVEL     'O03'         WOP1
031800060925     C                   MOVEL     'TISIQ1R'     WPGM
031900060329     C                   EXSR      LANPGMP
032000970611     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
032100970611E   1C                   ENDIF
032200970611     C*
032300970611     C                   ENDSR
032400970526     C*------------------------------------------------------------------------*
032500970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
032600970526     C*------------------------------------------------------------------------*
032700970526     C     O04GES        BEGSR
032800970526     C*
032900060327IF  1C     ERRL00        IFEQ      '0'
033000060922     C                   MOVEL     V5cNAR        DNAR
033100060922     C                   MOVEL     V5cCAP        DCAP
033200060922     C                   MOVEL     V5cntw        Dntw
033300060922     C                   MOVEL     V5cuti        Duti
033400060922     C                   MOVEL     V5hflo        Dflo
033500060922     C                   MOVEL     V5hdde        Ddde
033600060922     C                   MOVEL     V5hdsc        Ddsc
033700060327     C                   MOVEL     DEPOP0        WOP0
033800060327     C                   MOVEL     'O04'         WOP1
033900060925     C                   MOVEL     'TISIQ1R'     WPGM
034000060327     C                   EXSR      LANPGMP
034100060925     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
034200060327E   1C                   ENDIF
034300970526     C*
034400970526     C                   ENDSR
034500970526     C*------------------------------------------------------------------------*
034600970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
034700970526     C*------------------------------------------------------------------------*
034800970526     C     O05GES        BEGSR
034900970526     C*
035000060327     C                   MOVEL     DEPOP0        WOP0
035100060922     C                   MOVEL     V5cNAR        DNAR
035200060922     C                   MOVEL     V5cCAP        DCAP
035300060922     C                   MOVEL     V5cntw        Dntw
035400060922     C                   MOVEL     V5cuti        Duti
035500060922     C                   MOVEL     V5hflo        Dflo
035600060922     C                   MOVEL     V5hdde        Ddde
035700060922     C                   MOVEL     V5hdsc        Ddsc
035800060327     C                   MOVEL     'O05'         WOP1
035900060925     C                   MOVEL     'TISIQ1R'     WPGM
036000060327     C                   EXSR      LANPGMP
036100970529     C*
036200970529     C                   ENDSR
036300970526     C*------------------------------------------------------------------------*
036400970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
036500970526     C*------------------------------------------------------------------------*
036600970526     C     NEWPOS        BEGSR
036700970526     C*
036800970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
036900970526     C*         1= RICHIESTO POSIZIONAMENTO
037000970526     C                   Z-ADD     0             SWPOSI            7 0
037100970526     C*
037200970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
037300970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
037400970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
037500970526     C                   Z-ADD     0             SWMODI            1 0
037600970530     C*
037700970530     C* REIMPOSTA INDICATORI DI ERRORE
037800060313     C                   SETOFF                                       808281
037900970526     C*
038000060403     c
038100060403     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
038200970527IF  1C     VIDPNA        IFNE      DEPNAR
038300970527     C     VIDPCA        ORNE      DEPCAP
038400060922     C     VIDPnt        ORNE      DEPntw
038500060313     C     VIDPde        ORNE      DEPpde
038600970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
038700970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
038800970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
038900970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
039000970527     C                   MOVEL     VIDPCA        DEPCAP
039100060922     C                   MOVEL     VIDpnt        DEPntw
039200060313     C                   MOVEL     VIDpde        DEPpde
039300970526E   2C                   ENDIF
039400970526E   1C                   ENDIF
039500970526     C*
039600970526     C                   ENDSR
039700970526     C*------------------------------------------------------------------------*
039800970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
039900970526     C*------------------------------------------------------------------------*
040000970526     C     SETFIL        BEGSR
040100060330     c                   clear                   knar
040200060330     c                   clear                   kcap
040300060922     c                   eval      knar=vidpna
040400060922     c                   eval      kcap=vidpca
040500060330     c
040600970529     C*
040700060922     C     Kcpp          SETLL     AZCpp01L                           99
040800060330     c
040900970526     C* LETTURA RECORD
041000970526IF  1C     *IN99         IFEQ      *OFF
041100970526     C                   EXSR      LETFIL
041200970526E   1C                   ENDIF
041300970526     C*
041400970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
041500970526IF  1C     *IN99         IFEQ      *ON
041600970526     C                   Z-ADD     1             SFLEOF
041700970526E   1C                   ENDIF
041800970526     C*
041900970526     C                   ENDSR
042000970526     C*------------------------------------------------------------------------*
042100970526     C* LETFIL - LETTURA RECORD
042200970526     C*------------------------------------------------------------------------*
042300970526     C     LETFIL        BEGSR
042400970526     C*
042500970526     C* LEGGE FINO A:
042600970526     C*     - FINE FILE
042700970526     C*     - TROVATO RECORD VALIDO
042800970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
042900970526DO  1C     *IN99         DOWEQ     *OFF
043000970526     C     WRECOK        ANDEQ     'N'
043100970529     C*
043200970529     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
043300060922     C                   READ      AZcpp01L                               99
043400970526     C*
043500970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
043600970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
043700970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
043800970530X   2C                   ELSE
043900970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
044000970530E   2C                   ENDIF
044100970526E   1C                   ENDDO
044200970526     C*
044300970526     C                   ENDSR
044400970526     C*------------------------------------------------------------------------*
044500970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
044600970526     C*------------------------------------------------------------------------*
044700970526     C     CHKREC        BEGSR
044800060922     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
044900060922     C* controllo data
045000060922     c                   if        amgpde>0 and (amgpde<cppdde or
045100060922     c                             amgpde>cppdsc)
045200060922     C                   MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
045300060329     c                   endif
045400060922     c* Network
045500060922     c                   if        vidpnt<>*blanks and
045600060922     c                             vidpnt<>cppntw
045700060922     C                   MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
045800060922     c                   endif
045900060329     c
046000970526     C                   ENDSR
046100970526     C*------------------------------------------------------------------------*
046200970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
046300970526     C*------------------------------------------------------------------------*
046400970526     C     IRESFL        BEGSR
046500970529     C*
046600970529     C                   MOVEL     *BLANKS       OPZKEY
046700060922     c* Nazione e decodifica
046800060922     c                   eval      v5cnar=cppnar
046900060922     c                   eval      kcod='15'
047000060922     c                   movel(p)  v5cnar        kkey
047100060922     c     ktab          chain     tabel00f
047200060922     c                   if        %found(tabel00f)
047300060922     c                   movel     tbluni        v5dnar
047400060922     c                   else
047500060922     c                   movel     *all'?'       v5dnar
047600060922     c                   endif
047700060922     c
047800060922     c* CAP
047900060922     c                   eval      v5ccap=cppcap
048000060922     c* Decorrenza
048100060922     c                   movel     cppdde        v5hdde
048200060922     c     *iso          movel     cppdde        dataiso
048300060922     c                   movel     dataiso       datadmy
048400060922     c                   movel     datadmy       v5cdde
048500060922     c* scadenza
048600060922     c                   movel     cppdsc        v5hdsc
048700060922     c     *iso          movel     cppdsc        dataiso
048800060922     c                   movel     dataiso       datadmy
048900060922     c                   movel     datadmy       v5cdsc
049000060922     c* network e decodifica
049100060922     C                   MOVEL     Cppntw        V5cntw
049200060922     C                   CLEAR                   tibs02ds
049300060922     C                   MOVEL     'C'           T02MOD
049400060922     C                   MOVEL     KNSIF         T02SIF
049500060922     C                   MOVEL     'NTW'         T02COD
049600060922     C                   MOVEL(P)  cppntw        T02KE1
049700060922     C                   CALL      'TIBS02R'
049800060922     C                   PARM                    KPJBA
049900060922     C                   PARM                    tibs02ds
050000060922      *
050100060922     C     T02ERR        IFNE      *BLANKS
050200060922     C                   MOVEL     *all'?'       v5dntw
050300060922     c                   eval      %subst(v5dntw:1:2)='- '
050400060922     C                   ELSE
050500060922     c                   eval      %subst(v5dntw:1:2)='- '
050600060922     c                   eval      %subst(v5dntw:3:11)=t02uni
050700060922     C                   ENDIF
050800060922     c
050900060922     c* utilizzo
051000060922     C                   if        Cpputi='S'
051100060922     c                   eval      v5cuti='SI'
051200060922     c                   else
051300060922     c                   eval      v5cuti='NO'
051400060922     c                   endif
051500061030     c* Bolle
051600061030     c                   clear                   v5cbol
051700061030     c                   if        cppflo=*blanks
051800061030     c                   eval      v5cbol='Tutte '
051900061030     c                   else
052000061030     c                   movel     cppflo        dcppflo
052100061030     c                   if        §cppcas='S'
052200061030     c                   eval      v5cbol='C/Ass '
052300061030     c                   endif
052400061030     c                   endif
052500060922     c
052600060922     c                   eval      v5hflo=cppflo
052700060313     c
052800970526     C                   ENDSR
052900970526     C*------------------------------------------------------------------------*
053000970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
053100970526     C*------------------------------------------------------------------------*
053200970526     C     CHKPAR        BEGSR
053300060313     c* controllo data di riferimento se errata
053400060922     c                   clear                   amgpde
053500060313     c                   if        vidpde>0
053600060313     C                   Z-ADD     vidpde        G08DAT
053700060313     C                   Z-ADD     *ZEROS        G08INV
053800060313     C                   MOVEL     '0'           G08ERR
053900060313     C                   CALL      'XSRDA8'
054000060313     C                   PARM                    WLBDA8
054100060313     c                   if        g08err='1'
054200060313     C                   MOVEL     MSG(3)        DSMSMS
054300060313     C                   MOVEL     '1'           ERRL00
054400060922     C                   SETON                                        81
054500060313E   2C                   else
054600060314     c                   z-add     g08dat        vidpde
054700060922     C                   Z-ADD     G08INV        amgpde            8 0          *DATA CORRENTE AA/M/
054800060313     c                   endif
054900060313     c                   endif
055000060922     c* Network
055100060922     c                   if        vidpnt<>*blanks
055200060922     C                   CLEAR                   tibs02ds
055300060922     C                   MOVEL     'C'           T02MOD
055400060922     C                   MOVEL     KNSIF         T02SIF
055500060922     C                   MOVEL     'NTW'         T02COD
055600060922     C                   MOVEL     *BLANKS       T02KE1
055700060922     C                   MOVEL     vidpnt        T02KE1
055800060922     C                   CALL      'TIBS02R'
055900060922     C                   PARM                    KPJBA
056000060922     C                   PARM                    tibs02ds
056100060922      *
056200060922     C     T02ERR        IFNE      *BLANKS
056300060922     C                   MOVEL     MSG(4)        DSMSMS
056400060922     C                   MOVEL     '1'           ERRL00
056500060922     C                   SETON                                        82
056600060922     C                   ENDIF
056700060922     c                   endif
056800970526     C*
056900970526     C                   ENDSR
057000060314     C*------------------------------------------------------------------------*
057100060314     C* LANPGM - LANCIA UN PROGRAMMA
057200060314     C*------------------------------------------------------------------------*
057300060314     C     LANPGMp       BEGSR
057400060314     C*
057500060314     C* IMPOSTA LA DS DI PROCEDURA
057600060922     c                   clear                   tiSIQ0ds
057700060922     C                   MOVEL     WOP0          dq0OP0                         *LIVELLO PROCEDURA
057800060922     C                   MOVEL     WOP1          dq0OP1                         *LIVELLO PGM
057900060922     C                   MOVEL     '0'           dq0F03                         *PREMUTO F03=FINE
058000060922     C                   MOVEL     '0'           dq0F12                         *PREMUTO F12=RITORNO
058100060922     C                   MOVEL     '0'           dq0ERR                         *'1'=ERRORE
058200060922     C                   MOVEL     *BLANKS       dq0MSG                         *MESSAGGIO DI RITORN
058300060922     C                   MOVEL     DNAR          dq0NAR                         *NAZIONE
058400060922     C                   MOVEL     DCAP          dq0CAP                         *CAP
058500060922     C                   MOVEL     Dntw          dq0ntw                         *LOCALITA'
058600060922     C                   MOVEL     Ddde          dq0dde                         *LOCALITA'
058700060922     C                   MOVEL     Ddsc          dq0dsc                         *LOCALITA'
058800060922     C                   MOVEL     Duti          dq0uti                         *LOCALITA'
058900060922     C                   MOVEL     Dflo          dq0flo                         *LOCALITA'
059000060314     C*
059100060314     C                   CALL      WPGM
059200060314     C                   PARM                    KPJBA
059300060922     C                   PARM                    tiSIQ0ds
059400060314     C*
059500060314     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
059600060922IF  1C     DQ0F03        IFEQ      '1'                                          *PREMUTO F03
059700060314     C                   MOVEL     '1'           ERRL00                         *ERRORE
059800060314     C                   MOVEL     '1'           F03L00                         *FINE PGM
059900060314X   1C                   ELSE                                                   *NON PREMUTO F03
060000060922     C                   MOVEL     DQ0MSG        DSMSMS                         *MESSAGGIO DI RITORN
060100060314E   1C                   ENDIF
060200060314     C*
060300060314     C                   ENDSR
060400970527     C*------------------------------------------------------------------------*
060500970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
060600970527     C*------------------------------------------------------------------------*
060700970527     C     CARTAB        BEGSR
060800970527     C*
060900970527     C                   ENDSR
061000970526     C*------------------------------------------------------------------------*
061100970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
061200970526     C*------------------------------------------------------------------------*
061300970526     C     CHKFOP        BEGSR
061400970626     C*
061500970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
061600060404IF  1C     opzione       IFEQ      '01'
061700970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
061800970626     C                   MOVEL     'S'           OPDATT
061900970626     C                   MOVEL     OPDRIG        OPD(1)
062000970626X   1C                   ELSE
062100970626     C* OPZIONI
062200970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
062300970626     C                   MOVEL     'S'           OPDATT
062400970626     C                   MOVEL     OPDRIG        OPD(5)
062500970626E   1C                   ENDIF
062600970626     C*
062700970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
062800060404IF  1C     opzione       IFEQ      '02'
062900970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
063000970626     C                   MOVEL     'S'           OPDATT
063100970626     C                   MOVEL     OPDRIG        OPD(2)
063200970627     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
063300970627     C                   MOVEL     'S'           OPDATT
063400970627     C                   MOVEL     OPDRIG        OPD(3)
063500970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
063600970626     C                   MOVEL     'S'           OPDATT
063700970626     C                   MOVEL     OPDRIG        OPD(4)
063800970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
063900970626     C                   MOVEL     'S'           FUDATT
064000970626     C                   MOVEL     FUDRIG        FUD(6)
064100060327    1C                   ENDIF
064200060331     c
064300060404IF  1C     opzione       IFEQ      '05'
064400060331     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
064500060331     C                   MOVEL     'N'           OPDATT
064600060331     C                   MOVEL     OPDRIG        OPD(2)
064700060331     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
064800060331     C                   MOVEL     'N'           OPDATT
064900060331     C                   MOVEL     OPDRIG        OPD(3)
065000060331     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
065100060331     C                   MOVEL     'N'           OPDATT
065200060331     C                   MOVEL     OPDRIG        OPD(4)
065300060331     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
065400060331     C                   MOVEL     'N'           FUDATT
065500060331     C                   MOVEL     FUDRIG        FUD(6)
065600060331    1C                   ENDIF
065700060927     c
065800060927     c* L'opzione F7 solo se non richiamato da cappario
065900060927     c                   if        *in10
066000060927     C                   MOVEL     FUD(7)        FUDRIG                         *INSERIMENTO
066100060927     C                   MOVEL     'N'           FUDATT
066200060927     C                   MOVEL     FUDRIG        FUD(7)
066300060927     c                   endif
066400970526     C*
066500970526     C* GESTIONE OPZIONI A VIDEO
066600970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
066700970526     C                   EXSR      OPZGES
066800970526     C*
066900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
067000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
067100970526     C                   EXSR      FUNGES
067200970526     C*
067300970526     C                   ENDSR
067400020919     C*--------------------------------------------------------------------------------------------*
067500020919     C* REPERISCE I DATI UTENTE
067600020919     C*--------------------------------------------------------------------------------------------*
067700020919     C     REPDATIUTE    BEGSR
067800020919     C*
067900020919     C* INIZIALIZZA VARIABILI DI WRK
068000020919     C                   CLEAR                   TIBS34DS
068100020919     C                   CLEAR                   AZUTEDS
068200020919     C                   CLEAR                   DDATIUTE
068300020919     C*
068400020919     C     *DTAARA       DEFINE    §azute        azuteds
068500020919     C     *DTAARA       DEFINE    §datiute      ddatiute
068600020919     C                   IN(E)     *DTAARA
068700020919if  1C                   IF        %Error
068800020919     c                   EVAL      I34Tla = 'L'
068900020919     C                   CALL      'TIBS34R'
069000020919     C                   PARM                    Tibs34Ds
069100020919     C                   IN        *DTAARA
069200020919e   1C                   ENDIF
069300020919     C*
069400020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
069500020919     C                   MOVEL(P)  RSUT          DSFIRS           20
069600020919     C*
069700020919     C                   ENDSR
069800970526     C*------------------------------------------------------------------------*
069900970526     C* *INZSR - OPERAZIONI INIZIALI
070000970526     C*------------------------------------------------------------------------*
070100970526     C     *INZSR        BEGSR
070200970526     C*---
070300970526     C* RICEVIMENTO PARAMETRI
070400970526     C*---
070500970526     C     *ENTRY        PLIST
070600970612     C                   PARM                    KPJBA
070700060922     C                   PARM                    tiSIQ0ds
070800970526     C*
070900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
071000970526     C     XMSGPA        PLIST
071100970526     C                   PARM                    DSMSMS
071200970526     C                   PARM                    DSMSPG
071300970526     C                   PARM                    DSMSKE
071400970526     C                   PARM                    DSMSER
071500970526     C*
071600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
071700970526     C                   CALL      'XKEY'
071800970526     C                   PARM                    DSKEY
071900970526     C*---
072000970526     C* VARIABILI NON RIFERITE AL DATA BASE
072100970526     C*---
072200970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
072300970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
072400970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
072500970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
072600970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
072700970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
072800970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
072900970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
073000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
073100060922     C     *LIKE         DEFINE    DQ0OP0        DEPOP0                         *DI DEPOSITO
073200060922     C     *LIKE         DEFINE    DQ0OP0        WOP0                           *DI LAVORO
073300060922     C     *LIKE         DEFINE    DQ0OP1        WOP1
073400060922     C     *LIKE         DEFINE    Dq0NAR        DNAR
073500060922     C     *LIKE         DEFINE    Dq0CAP        DCAP
073600060922     C     *LIKE         DEFINE    Dq0ntw        Dntw
073700060922     C     *LIKE         DEFINE    Dq0dde        Ddde
073800060922     C     *LIKE         DEFINE    Dq0dsc        Ddsc
073900060922     C     *LIKE         DEFINE    Dq0uti        Duti
074000060922     C     *LIKE         DEFINE    Dq0flo        Dflo
074100970526     C*---
074200970526     C* VARIABILI DI CONTROLLO
074300970526     C*---
074400970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
074500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
074600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
074700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
074800970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
074900970530     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
075000970526     C*
075100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
075200060922     C                   MOVEL     'TISIQ0R'     DSMSPG
075300060922     C                   MOVEL     'TISIQ0R'     MSGQUE
075400970526     C                   MOVEL     *BLANKS       MSGKEY
075500970526     C                   MOVEL     *BLANKS       DSMSMS
075600970526     C                   MOVEL     *BLANKS       DSMSKE
075700970526     C                   Z-ADD     *ZEROS        DSMSER
075800060330     c
075900060922     C     *LIKE         DEFINE    Cppdde        DEPpde                         *DEPOSITI
076000060922     C     *LIKE         DEFINE    cppNAR        DEPNAR                         *DEPOSITI
076100060922     C     *LIKE         DEFINE    CppCAP        DEPCAP
076200060922     C     *LIKE         DEFINE    Cppntw        DEPntw
076300970526     C*---
076400970526     C* CHIAVI DI LETTURA
076500970526     C*---
076600060922     C     kcpp          KLIST
076700060313     C                   KFLD                    Knar                           *CAP
076800060922     C                   KFLD                    Kcap                           *CAP
076900060922     c     Ktab          Klist
077000060922     c                   Kfld                    codut             1 0
077100060922     c                   Kfld                    kCod
077200060922     c                   Kfld                    kKey
077300060922     c                   z-add     1             codut
077400060922     c
077500970527     C* REPERIMENTO DATI UTENTE
077600020919     C                   EXSR      REPDATIUTE
077700970527     C*
077800970527     C* CARICA LE TABELLE OCCORRENTI
077900970527     C                   EXSR      CARTAB
078000970526     C*
078100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
078200060922     C                   MOVEL     DQ0OP0        DEPOP0                         *LIVELLO PROCEDURA
078300060927     c* se richiamato da cappario disabilito tasto funzionale F7-Cappario
078400060927     c                   movel     dq0op0        wchiama           1
078500060927     c                   if        wchiama='C'
078600060927     c                   seton                                        10
078700060927     c                   else
078800060927     c                   setoff                                       10
078900060927     c                   endif
079000970526     C*
079100970526     C* CALCOLA LA DATA CORRENTE
079200970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
079300970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
079400970526     C                   Z-ADD     WN8           G08DAT
079500970526     C                   Z-ADD     *ZEROS        G08INV
079600970526     C                   MOVEL     '0'           G08ERR
079700970526     C                   CALL      'XSRDA8'
079800970526     C                   PARM                    WLBDA8
079900970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
080000060313     c
080100060313     c* Se non passata la data di ingresso, uso data corrente
080200060922     c                   if        DQ0dri=0
080300060922     c                   eval      amgpde=datcor
080400060313     c                   eval      vidpde=wn8
080500060313     c                   else
080600060922     c                   eval      amgpde=DQ0dri
080700060313     c* La imposto a video
080800060922     C                   Z-ADD     DQ0DRI        G08INV
080900060313     C                   Z-ADD     *ZEROS        G08DAT
081000060313     C                   MOVEL     '3'           G08ERR
081100060313     C                   CALL      'XSRDA8'
081200060313     C                   PARM                    WLBDA8
081300060313     C                   Z-ADD     G08DAT        VIDpde                         *DATA RIFERIMENTO
081400060313     c                   endif
081500970526     C*
081600970526     C* IMPOSTA I PARAMETRI DI INGRESSO
081700060922     C                   MOVEL     DQ0NAR        VIDPNA                         *NAZIONE
081800060922     C                   MOVEL     DQ0CAP        VIDPCA                         *CAP
081900060922     C                   MOVEL     DQ0ntw        VIDpnt                         *CAP
082000060313     c
082100970526     C                   EXSR      CHKPAR
082200060922     c
082300970526IF  1C     ERRL00        IFEQ      '1'
082400060922     C                   MOVEL     '1'           DQ0ERR
082500060922     C                   MOVEL     DSMSMS        DQ0MSG
082600970526X   1C                   ELSE
082700970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
082800970527     C                   MOVEL     VIDPCA        DEPCAP
082900060922     C                   MOVEL     VIDpnt        DEPntw
083000060313     C                   MOVEL     VIDpde        DEPpde
083100970526E   1C                   ENDIF
083200970526     C*
083300970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
083400060404     c                   eval      opzione=%subst(depop0:2:2)
083500970526     C                   EXSR      CHKFOP
083600970526     C*
083700970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
083800060404IF  1C     opzione       IFEQ      '01'
083900970530     C                   MOVEL     DEC(3)        VIDING
084000970530E   1C                   ENDIF
084100060404IF  1C     opzione       IFEQ      '02'
084200970530     C                   MOVEL     DEC(1)        VIDING
084300970530E   1C                   ENDIF
084400060404IF  1C     opzione       IFEQ      '05'
084500970530     C                   MOVEL     DEC(2)        VIDING
084600970530E   1C                   ENDIF
084700970526     C*
084800970526     C                   ENDSR
084900970526     C*------------------------------------------------------------------------*
085000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
085100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
085200970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
085300970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
085400970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
085500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
085600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
085700970526     C*------------------------------------------------------------------------*
085800970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
085900970526     C*------------------------------------------------------------------------*
086000970526     C     POSSFL        BEGSR
086100970526     C*
086200970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
086300970526     C                   EXSR      CLRSFL
086400970526     C*
086500970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
086600970526     C                   EXSR      SETFIL
086700970526     C*
086800970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
086900970526     C     SFLEOF        IFEQ      0
087000970526     C                   EXSR      CARSFL
087100970526     C                   END
087200970526     C*
087300970526     C                   ENDSR
087400970526     C*------------------------------------------------------------------------*
087500970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
087600970526     C*------------------------------------------------------------------------*
087700970526     C     ENTATT        BEGSR
087800970526     C*
087900970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
088000970526     C                   Z-ADD     0             OPZOK             1 0
088100970526     C*
088200970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
088300970526     C                   Z-ADD     0             SFLVAR            5 0
088400970526     C*
088500970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
088600970526     C                   Z-ADD     0             SFLERR
088700970526     C*
088800970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
088900970526     C                   Z-ADD     SFLULT        WFLULT
089000970526     C*
089100970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
089200060922     C                   READC     SIQ0D05                                98
089300970526     C     *IN98         DOWEQ     '0'
089400970526     C*
089500970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
089600970526     C                   ADD       1             SFLVAR
089700970526     C*
089800970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
089900970526     C     OPZOK         IFEQ      0
090000970526     C*
090100970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
090200970526     C     OPZKEY        IFNE      *BLANKS
090300970526     C                   EXSR      OPZCON
090400970526     C                   END
090500970526     C*
090600970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
090700970526     C*            2. PULIZIA SCELTA IN SFL
090800970526     C     OPZOK         IFEQ      0
090900970526     C                   MOVEL     '0'           *IN41
091000970526     C                   MOVEL     *BLANKS       OPZKEY
091100970526     C                   ELSE
091200970526     C*
091300970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
091400970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
091500970526     C                   MOVEL     '1'           *IN41
091600970526     C                   Z-ADD     SFLULT        SFLERR
091700970526     C                   END
091800970526     C                   ELSE
091900970526     C*
092000970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
092100970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
092200970526     C                   MOVEL     '1'           *IN41
092300970526     C                   END                                                    OPZOK=0
092400970526     C*
092500970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
092600970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
092700970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
092800970526     C     OPZOK         IFNE      0
092900970526     C                   Z-ADD     SFLERR        SFLNBR
093000970526     C                   ELSE
093100970526     C                   Z-ADD     SFLULT        SFLNBR
093200970526     C                   END
093300970526     C*
093400970526     C* AGGIORNO RECORD SFL
093500060922     C                   UPDATE    SIQ0D05
093600970526     C*
093700060922     C                   READC     SIQ0D05                                98
093800970526     C                   END                                                    DO WHILE
093900970526     C*
094000970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
094100970526     C                   Z-ADD     WFLULT        SFLULT
094200970526     C*
094300970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
094400970526     C     SFLVAR        IFEQ      0
094500970526     C*---                 MOVEL'1'       FINL00
094600970526     C                   END
094700970526     C*
094800970526     C                   ENDSR
094900970526     C*------------------------------------------------------------------------*
095000970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
095100970526     C*------------------------------------------------------------------------*
095200970526     C     CLRSFL        BEGSR
095300970526     C*
095400970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
095500970526     C* (0=NON FINE FILE  1=FINE FILE)
095600970526     C                   Z-ADD     0             SFLEOF
095700970526     C*
095800970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
095900970526     C                   Z-ADD     0             SFLULT
096000970526     C*
096100970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
096200970526     C                   MOVEL     '1'           *IN40
096300060922     C                   WRITE     SIQ0D06
096400970526     C                   MOVEL     '0'           *IN40
096500970526     C*
096600970526     C                   ENDSR
096700970526     C*------------------------------------------------------------------------*
096800970526     C* CARSFL - CARICAMENTO SFL
096900970526     C*------------------------------------------------------------------------*
097000970526     C     CARSFL        BEGSR
097100970526     C*
097200970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
097300970526     C     SFLULT        IFEQ      0
097400970526     C                   EXSR      SETFIL
097500970526     C                   END
097600970526     C*
097700970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
097800970526     C                   Z-ADD     SFLULT        WFLULT
097900970526     C*
098000970526     C* CICLO:
098100970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
098200970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
098300970526     C                   Z-ADD     1             SFLRIG
098400970526     C     SFLRIG        DOWLE     SFLPAG
098500970526     C     SFLEOF        ANDEQ     0
098600970526     C*
098700970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
098800970526     C                   MOVEL     *BLANKS       OPZKEY
098900970526     C*
099000970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
099100970526     C                   EXSR      IRESFL
099200970526     C*
099300970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
099400970526     C                   ADD       1             SFLULT
099500970526     C*
099600970526     C* SCRITTURA RECORD SFL
099700060922     C                   WRITE     SIQ0D05
099800970526     C                   ADD       1             SFLRIG
099900970526     C*
100000970526     C* LETTURA FILE DA CARICARE
100100970526     C                   EXSR      LETFIL
100200970526     C                   END                                                    DO WHILE
100300970526     C*
100400970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
100500970526     C     WFLULT        IFNE      SFLULT
100600970526     C                   Z-ADD     WFLULT        SFLNBR
100700970526     C                   ADD       1             SFLNBR
100800970526     C                   END
100900970526     C*
101000970526     C                   ENDSR
101100970526     C*------------------------------------------------------------------------*
101200970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
101300970526     C*------------------------------------------------------------------------*
101400970526     C     OUTSFL        BEGSR
101500970526     C*
101600970526     C* WRITE RECORD TESTATA VIDEO
101700060922     C                   WRITE     SIQ0D01
101800970526     C*
101900970526     C* VISUALIZZAZIONE MESSAGGIO
102000970526     C                   EXSR      SNDMSG
102100970526     C*
102200970526     C* WRITE RECORD OPZIONI VIDEO
102300060922     C                   WRITE     SIQ0D02
102400970526     C*
102500970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
102600060922     C                   WRITE     SIQ0D03
102700970526     C*
102800970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
102900060922     C                   WRITE     SIQ0D04
103000970526     C*
103100970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
103200970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
103300970526     C     SFLEOF        IFEQ      0
103400970526     C                   MOVEL     '0'           *IN42
103500970526     C                   ELSE
103600970526     C*
103700970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
103800970526     C                   MOVEL     '1'           *IN42
103900970526     C                   END
104000970526     C*
104100970526     C* WRITE RECORD TASTI FUNZIONALI
104200060922     C                   WRITE     SIQ0D09
104300970526     C*
104400970526     C* CASO DI SFL VUOTO
104500970526     C     SFLULT        IFLE      0
104600970526     C*
104700970526     C*    - WRITE RECORD VUOTO
104800060922     C                   WRITE     SIQ0D0A
104900060922     C                   READ      SIQ0D0A                                99
105000970526     C*
105100970526     C                   ELSE
105200970526     C*
105300970526     C* CASO DI SFL CON RECORDS
105400970526     C*
105500970526     C*    - WRITE RECORD CONTROLLO SFL
105600060922     C                   WRITE     SIQ0D06
105700060922     C                   READ      SIQ0D06                                99
105800970526     C                   END
105900970526     C*
106000970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
106100060922     C                   READ      SIQ0D03                                99
106200970526     C*
106300970526     C                   ENDSR
106400970526     C*------------------------------------------------------------------------*
106500970526     C* SNDMSG - MANDA MESSAGGIO
106600970526     C*------------------------------------------------------------------------*
106700970526     C     SNDMSG        BEGSR
106800970526     C*
106900970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
107000970526     C                   MOVEL     '1'           *IN40
107100060922     C                   WRITE     SIQ0DMC
107200970526     C                   MOVEL     '0'           *IN40
107300970526     C*
107400970526     C* VISUALIZZAZIONE MESSAGGIO
107500970526     C                   Z-ADD     0             MSGULT
107600970526     C                   CALL      'XMSG'        XMSGPA
107700970526     C     DSMSER        IFEQ      0
107800970526     C                   MOVEL     DSMSKE        MSGKEY
107900970526     C                   END
108000970526     C                   Z-ADD     1             MSGULT
108100060922     C                   WRITE     SIQ0DMS                                      SFL MESS
108200060922     C                   WRITE     SIQ0DMC                                      CONTR MESS
108300970526     C*
108400970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
108500970526     C                   MOVEL     *BLANKS       DSMSMS
108600970526     C*
108700970526     C                   ENDSR
108800970526     C*------------------------------------------------------------------------*
108900970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
109000970526     C*------------------------------------------------------------------------*
109100970526     C     FUNCON        BEGSR
109200970526     C*
109300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
109400970526     C                   EXSR      FUNVAR
109500970526     C*
109600970526     C* PULISCO CAMPI DI LAVORO
109700970526     C                   Z-ADD     0             I                 5 0
109800970526     C                   Z-ADD     0             W                 5 0
109900970526     C*
110000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
110100970526     C     FUNKEY        IFNE      *BLANKS
110200970526     C     1             DO        24            I
110300970526     C                   MOVEL     FUD(I)        FUDRIG
110400970526     C*
110500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
110600970526     C     FUNKEY        IFEQ      FUDFUN
110700970526     C     FUDATT        ANDEQ     'S'
110800970526     C                   Z-ADD     I             W
110900970526     C                   END
111000970526     C                   END                                                    DO WHILE
111100970526     C*
111200970526     C* FUNZIONE NON ATTIVA (ERRORE)
111300970526     C     W             IFEQ      0
111400970526     C*
111500970526     C* EMISSIONE MESSAGGIO DI ERRORE
111600970526     C                   MOVEL     MSG(2)        DSMSMS
111700970526     C                   END                                                    W=0
111800970526     C                   END                                                    FUNKEY NE *BLANKS
111900970526     C*
112000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
112100970526     C     W             IFNE      0
112200970526     C     FUNKEY        OREQ      *BLANKS
112300970526     C*
112400970526     C     FUNKEY        IFEQ      *BLANKS
112500970526     C                   EXSR      ENTGES
112600970526     C                   END
112700970526     C     FUNKEY        IFEQ      '03'
112800970526     C                   EXSR      F03GES
112900970526     C                   END
113000970526     C     FUNKEY        IFEQ      '05'
113100970526     C                   EXSR      F05GES
113200970526     C                   END
113300970526     C     FUNKEY        IFEQ      '06'
113400970526     C                   EXSR      F06GES
113500970526     C                   END
113600060926     C     FUNKEY        IFEQ      '07'
113700060926     C                   EXSR      F07GES
113800060926     C                   END
113900970526     C     FUNKEY        IFEQ      '12'
114000970526     C                   EXSR      F12GES
114100970526     C                   END
114200970529     C     FUNKEY        IFEQ      '13'
114300970529     C                   EXSR      F13GES
114400970529     C                   END
114500970526     C     FUNKEY        IFEQ      '23'
114600970526     C                   EXSR      F23GES
114700970526     C                   END
114800970526     C     FUNKEY        IFEQ      '24'
114900970526     C                   EXSR      F24GES
115000970526     C                   END
115100970526     C     FUNKEY        IFEQ      'RU'
115200970526     C                   EXSR      RUPGES
115300970526     C                   END
115400970526     C*
115500970526     C                   END
115600970526     C*
115700970526     C                   ENDSR
115800970526     C*------------------------------------------------------------------------*
115900970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
116000970526     C*------------------------------------------------------------------------*
116100970526     C     FUNVAR        BEGSR
116200970526     C*
116300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
116400970526     C     KEY           IFEQ      ENTER
116500970526     C                   MOVEL     *BLANKS       FUNKEY
116600970526     C                   END
116700970526     C     KEY           IFEQ      F1
116800970526     C                   MOVEL     '01'          FUNKEY
116900970526     C                   END
117000970526     C     KEY           IFEQ      F2
117100970526     C                   MOVEL     '02'          FUNKEY
117200970526     C                   END
117300970526     C     KEY           IFEQ      F3
117400970526     C                   MOVEL     '03'          FUNKEY
117500970526     C                   END
117600970526     C     KEY           IFEQ      F4
117700970526     C                   MOVEL     '04'          FUNKEY
117800970526     C                   END
117900970526     C     KEY           IFEQ      F5
118000970526     C                   MOVEL     '05'          FUNKEY
118100970526     C                   END
118200970526     C     KEY           IFEQ      F6
118300970526     C                   MOVEL     '06'          FUNKEY
118400970526     C                   END
118500970526     C     KEY           IFEQ      F7
118600970526     C                   MOVEL     '07'          FUNKEY
118700970526     C                   END
118800970526     C     KEY           IFEQ      F8
118900970526     C                   MOVEL     '08'          FUNKEY
119000970526     C                   END
119100970526     C     KEY           IFEQ      F9
119200970526     C                   MOVEL     '09'          FUNKEY
119300970526     C                   END
119400970526     C     KEY           IFEQ      F10
119500970526     C                   MOVEL     '10'          FUNKEY
119600970526     C                   END
119700970526     C     KEY           IFEQ      F11
119800970526     C                   MOVEL     '11'          FUNKEY
119900970526     C                   END
120000970526     C     KEY           IFEQ      F12
120100970526     C                   MOVEL     '12'          FUNKEY
120200970526     C                   END
120300970526     C     KEY           IFEQ      F13
120400970526     C                   MOVEL     '13'          FUNKEY
120500970526     C                   END
120600970526     C     KEY           IFEQ      F14
120700970526     C                   MOVEL     '14'          FUNKEY
120800970526     C                   END
120900970526     C     KEY           IFEQ      F15
121000970526     C                   MOVEL     '15'          FUNKEY
121100970526     C                   END
121200970526     C     KEY           IFEQ      F16
121300970526     C                   MOVEL     '16'          FUNKEY
121400970526     C                   END
121500970526     C     KEY           IFEQ      F17
121600970526     C                   MOVEL     '17'          FUNKEY
121700970526     C                   END
121800970526     C     KEY           IFEQ      F18
121900970526     C                   MOVEL     '18'          FUNKEY
122000970526     C                   END
122100970526     C     KEY           IFEQ      F19
122200970526     C                   MOVEL     '19'          FUNKEY
122300970526     C                   END
122400970526     C     KEY           IFEQ      F20
122500970526     C                   MOVEL     '20'          FUNKEY
122600970526     C                   END
122700970526     C     KEY           IFEQ      F21
122800970526     C                   MOVEL     '21'          FUNKEY
122900970526     C                   END
123000970526     C     KEY           IFEQ      F22
123100970526     C                   MOVEL     '22'          FUNKEY
123200970526     C                   END
123300970526     C     KEY           IFEQ      F23
123400970526     C                   MOVEL     '23'          FUNKEY
123500970526     C                   END
123600970526     C     KEY           IFEQ      F24
123700970526     C                   MOVEL     '24'          FUNKEY
123800970526     C                   END
123900970526     C     KEY           IFEQ      ROLLUP
124000970526     C                   MOVEL     'RU'          FUNKEY
124100970526     C                   END
124200970526     C     KEY           IFEQ      HELP
124300970526     C                   MOVEL     'HP'          FUNKEY
124400970526     C                   END
124500970526     C*
124600970526     C                   ENDSR
124700970526     C*------------------------------------------------------------------------*
124800970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
124900970526     C*------------------------------------------------------------------------*
125000970526     C     FUNGES        BEGSR
125100970526     C*
125200970526     C* PULISCO CAMPI DI LAVORO
125300970526     C                   Z-ADD     0             W                 5 0
125400970526     C*
125500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
125600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
125700970526     C     1             DO        24            I
125800970526     C                   MOVEL     FUD(I)        FUDRIG
125900970526     C*
126000970526     C* PULIZIA FUNZIONI PRECEDENTI
126100970526     C                   MOVEL     *BLANKS       FUV(I)
126200970526     C*
126300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
126400970526     C* (SALTA F23 E F24)
126500970526     C     FUDATT        IFEQ      'S'
126600970526     C     FUDFUN        ANDNE     '23'
126700970526     C     FUDFUN        ANDNE     '24'
126800970526     C                   ADD       1             W
126900970526     C                   MOVEL     FUDDES        FUV(W)
127000970526     C                   END
127100970526     C                   END                                                    DO WHILE
127200970526     C*
127300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
127400970526     C* ALTRIMENTI DISATTIVO F24
127500970526     C     1             DO        24            I
127600970526     C                   MOVEL     FUD(I)        FUDRIG
127700970526     C*
127800970526     C     FUDFUN        IFEQ      '24'
127900970526     C     W             IFGT      6
128000970526     C                   MOVEL     'S'           FUDATT
128100970526     C                   ELSE
128200970526     C                   MOVEL     *BLANKS       FUDATT
128300970526     C                   END                                                    W GT 8
128400970526     C                   MOVEL     FUDRIG        FUD(I)
128500970526     C                   END
128600970526     C                   END                                                    DO WHILE
128700970526     C*
128800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
128900970526     C     W             DIV       6             I
129000970526     C                   MVR                     Z
129100970526     C*
129200970526     C     Z             IFNE      0
129300970526     C     I             ADD       1             Z
129400970526     C                   ELSE
129500970526     C                   Z-ADD     I             Z
129600970526     C                   END
129700970526     C*
129800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
129900970526     C                   ADD       1             FUNGRU
130000970526     C     FUNGRU        IFGT      Z
130100970526     C     FUNGRU        ORLT      1
130200970526     C                   Z-ADD     1             FUNGRU
130300970526     C                   END
130400970526     C*
130500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
130600970526     C     FUNGRU        IFEQ      1
130700970526     C                   MOVEL     FUV(1)        FUN001
130800970526     C                   MOVEL     FUV(2)        FUN002
130900970526     C                   MOVEL     FUV(3)        FUN003
131000970526     C                   MOVEL     FUV(4)        FUN004
131100970526     C                   MOVEL     FUV(5)        FUN005
131200970526     C                   MOVEL     FUV(6)        FUN006
131300970526     C                   END
131400970526     C*
131500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
131600970526     C     FUNGRU        IFEQ      2
131700970526     C                   MOVEL     FUV(7)        FUN001
131800970526     C                   MOVEL     FUV(8)        FUN002
131900970526     C                   MOVEL     FUV(9)        FUN003
132000970526     C                   MOVEL     FUV(10)       FUN004
132100970526     C                   MOVEL     FUV(11)       FUN005
132200970526     C                   MOVEL     FUV(12)       FUN006
132300970526     C                   END
132400970526     C*
132500970526     C* TERZO GRUPPO DI 6 FUNZIONI
132600970526     C     FUNGRU        IFEQ      3
132700970526     C                   MOVEL     FUV(13)       FUN001
132800970526     C                   MOVEL     FUV(14)       FUN002
132900970526     C                   MOVEL     FUV(15)       FUN003
133000970526     C                   MOVEL     FUV(16)       FUN004
133100970526     C                   MOVEL     FUV(17)       FUN005
133200970526     C                   MOVEL     FUV(18)       FUN006
133300970526     C                   END
133400970526     C*
133500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
133600970526     C     FUNGRU        IFEQ      4
133700970526     C                   MOVEL     FUV(19)       FUN001
133800970526     C                   MOVEL     FUV(20)       FUN002
133900970526     C                   MOVEL     FUV(21)       FUN003
134000970526     C                   MOVEL     FUV(22)       FUN004
134100970526     C                   MOVEL     *BLANKS       FUN005
134200970526     C                   MOVEL     *BLANKS       FUN006
134300970526     C                   END
134400970526     C*
134500970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
134600970526     C                   MOVEL     *BLANKS       FUN007
134700970526     C                   MOVEL     *BLANKS       FUN008
134800970526     C*
134900970526     C     1             DO        24            I
135000970526     C                   MOVEL     FUD(I)        FUDRIG
135100970526     C     FUDFUN        IFEQ      '23'
135200970526     C     FUDATT        ANDEQ     'S'
135300970526     C                   MOVEL     FUDDES        FUN007
135400970526     C                   END
135500970526     C     FUDFUN        IFEQ      '24'
135600970526     C     FUDATT        ANDEQ     'S'
135700970526     C                   MOVEL     FUDDES        FUN008
135800970526     C                   END
135900970526     C                   END                                                    DO
136000970526     C*
136100970526     C                   ENDSR
136200970526     C*------------------------------------------------------------------------*
136300970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
136400970526     C*------------------------------------------------------------------------*
136500970526     C     OPZGES        BEGSR
136600970526     C*
136700970526     C* PULISCO CAMPI DI LAVORO
136800970526     C                   Z-ADD     0             W                 5 0
136900970526     C*
137000970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
137100970526     C* E CARICO TABELLA OPZIONI ATTIVE
137200970526     C     1             DO        24            I
137300970526     C                   MOVEL     OPD(I)        OPDRIG
137400970526     C*
137500970526     C* PULIZIA OPZIONI PRECEDENTI
137600970526     C                   MOVEL     *BLANKS       OPV(I)
137700970526     C*
137800970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
137900970526     C     OPDATT        IFEQ      'S'
138000970526     C                   ADD       1             W
138100970526     C                   MOVEL     OPDDES        OPV(W)
138200970526     C                   END
138300970526     C                   END                                                    DO WHILE
138400970526     C*
138500970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
138600970526     C* ALTRIMENTI DISATTIVO F23
138700970526     C     1             DO        24            I
138800970526     C                   MOVEL     FUD(I)        FUDRIG
138900970526     C*
139000970526     C     FUDFUN        IFEQ      '23'
139100970526     C     W             IFGT      8
139200970526     C                   MOVEL     'S'           FUDATT
139300970526     C                   ELSE
139400970526     C                   MOVEL     *BLANKS       FUDATT
139500970526     C                   END                                                    W GT 8
139600970526     C                   MOVEL     FUDRIG        FUD(I)
139700970526     C                   END
139800970526     C                   END                                                    DO WHILE
139900970526     C*
140000970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
140100970526     C     W             DIV       8             I
140200970526     C                   MVR                     Z                 5 0
140300970526     C*
140400970526     C     Z             IFNE      0
140500970526     C     I             ADD       1             Z
140600970526     C                   ELSE
140700970526     C                   Z-ADD     I             Z
140800970526     C                   END
140900970526     C*
141000970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
141100970526     C                   ADD       1             OPZGRU
141200970526     C     OPZGRU        IFGT      Z
141300970526     C     OPZGRU        ORLT      1
141400970526     C                   Z-ADD     1             OPZGRU
141500970526     C                   END
141600970526     C*
141700970526     C* PRIMO GRUPPO DI 8 OPZIONI
141800970526     C     OPZGRU        IFEQ      1
141900970526     C                   MOVEL     OPV(1)        OPZ001
142000970526     C                   MOVEL     OPV(2)        OPZ002
142100970526     C                   MOVEL     OPV(3)        OPZ003
142200970526     C                   MOVEL     OPV(4)        OPZ004
142300970526     C                   MOVEL     OPV(5)        OPZ005
142400970526     C                   MOVEL     OPV(6)        OPZ006
142500970526     C                   MOVEL     OPV(7)        OPZ007
142600970526     C                   MOVEL     OPV(8)        OPZ008
142700970526     C                   END
142800970526     C*
142900970526     C* SECONDO GRUPPO DI 8 OPZIONI
143000970526     C     OPZGRU        IFEQ      2
143100970526     C                   MOVEL     OPV(9)        OPZ001
143200970526     C                   MOVEL     OPV(10)       OPZ002
143300970526     C                   MOVEL     OPV(11)       OPZ003
143400970526     C                   MOVEL     OPV(12)       OPZ004
143500970526     C                   MOVEL     OPV(13)       OPZ005
143600970526     C                   MOVEL     OPV(14)       OPZ006
143700970526     C                   MOVEL     OPV(15)       OPZ007
143800970526     C                   MOVEL     OPV(16)       OPZ008
143900970526     C                   END
144000970526     C*
144100970526     C* TERZO GRUPPO DI 8 OPZIONI
144200970526     C     OPZGRU        IFEQ      3
144300970526     C                   MOVEL     OPV(17)       OPZ001
144400970526     C                   MOVEL     OPV(18)       OPZ002
144500970526     C                   MOVEL     OPV(19)       OPZ003
144600970526     C                   MOVEL     OPV(20)       OPZ004
144700970526     C                   MOVEL     OPV(21)       OPZ005
144800970526     C                   MOVEL     OPV(22)       OPZ006
144900970526     C                   MOVEL     OPV(23)       OPZ007
145000970526     C                   MOVEL     OPV(24)       OPZ008
145100970526     C                   END
145200970526     C*
145300970526     C                   ENDSR
145400970526     C*------------------------------------------------------------------------*
145500970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
145600970526     C*------------------------------------------------------------------------*
145700970526     C     OPZCON        BEGSR
145800970526     C*
145900970526     C* PULISCO CAMPI DI LAVORO
146000970526     C                   Z-ADD     0             I                 5 0
146100970526     C                   Z-ADD     0             W                 5 0
146200970526     C*
146300970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
146400970526     C     1             DO        24            I
146500970526     C                   MOVEL     OPD(I)        OPDRIG
146600970526     C*
146700970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
146800970526     C     OPZKEY        IFEQ      OPDOPZ
146900970526     C     OPZKEY        OREQ      OPDOP1
147000970526     C*
147100970526     C     OPDATT        IFEQ      'S'
147200970526     C                   MOVEL     OPDOPZ        OPZKEY
147300970526     C                   Z-ADD     I             W
147400970526     C                   END
147500970526     C                   END
147600970526     C*
147700970526     C                   END                                                    DO WHILE
147800970526     C*
147900970526     C* OPZIONE NON ATTIVA (ERRORE)
148000970526     C     W             IFEQ      0
148100970526     C*
148200970526     C* OPZIONE NON OK
148300970526     C                   Z-ADD     1             OPZOK
148400970526     C*
148500970526     C* EMISSIONE MESSAGGIO DI ERRORE
148600970526     C                   MOVEL     MSG(1)        DSMSMS
148700970526     C                   ELSE
148800970526     C*
148900970526     C* OPZIONE ATTIVA (PROCEDI ...)
149000970526     C*
149100970526     C     OPZKEY        IFEQ      ' 1'
149200970526     C                   EXSR      O01GES
149300970526     C                   END
149400970526     C     OPZKEY        IFEQ      ' 2'
149500060328     C                   EXSR      O02GES
149600970526     C                   END
149700970611     C     OPZKEY        IFEQ      ' 3'
149800970611     C                   EXSR      O03GES
149900970611     C                   END
150000970526     C     OPZKEY        IFEQ      ' 4'
150100970526     C                   EXSR      O04GES
150200970526     C                   END
150300970526     C     OPZKEY        IFEQ      ' 5'
150400970526     C                   EXSR      O05GES
150500970526     C                   END
150600970526     C                   END                                                    OPZ.NON ATTIVA
150700970526     C*
150800970526     C                   ENDSR
150900970526     C*------------------------------------------------------------------------*
151000970526     C* NOTE COSTRUZIONE SCHIERE
151100970526     C*------------------------------------------------------------------------*
151200970526     C*
151300970526     C* -MSG-MESSAGGI
151400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
151500970526     C* (NON TOCCARE MSG,1 E MSG,2)
151600970526     C*
151700970526     C* -CMD-COMANDI
151800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
151900970526     C*
152000970526     C* -OPD-OPZIONI
152100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
152200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
152300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
152400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
152500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
152600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
152700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
152800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
152900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
153000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
153100970526     C*
153200970526     C* -FUD-FUNZIONI
153300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
153400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
153500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
153600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
153700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
153800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
153900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
154000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
154100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
154200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
154300970526     O*------------------------------------------------------------------------*
154400970526** DEC - DECODIFICHE
154500970526    GESTIONE                                                     1
154600970530INTERROGAZIONE                                                   2
154700970530     SCELTA                                                      3
1548009705264
1549009705265
155000970526** MSG - MESSAGGI
155100060922TISIQ0R- Opzione non gestita                                                   1
155200060922TISIQ0R- Tasto funzionale non gestito                                          2
155300060922TISIQ0R- Data di riferimento errata
155400060922TISIQ0R- Network inesistente                                                   4
155500060922TISIQ0R-                                                                       5
155600060922TISIQ0R- Accettato solo un parametro di inizio lista (P.O. o Naz/CAP)          6
155700060922TISIQ0R- 7                                                                     7
155800060922TISIQ0R- 8                                                                     8
155900060922TISIQ0R- 9                                                                     9
156000060922TISIQ0R- 10                                                                   10
156100060922TISIQ0R- 11                                                                   11
156200060922TISIQ0R- 12                                                                   12
156300060922TISIQ0R- 13                                                                   13
156400060922TISIQ0R- 14                                                                   14
156500060922TISIQ0R- 15                                                                   15
156600060922TISIQ0R- 16                                                                   16
156700060922TISIQ0R- 17                                                                   17
156800060922TISIQ0R- 18                                                                   18
156900060922TISIQ0R- 19                                                                   19
157000060922TISIQ0R- 20                                                                   20
157100970526** OPD - OPZIONI
157200970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
157300060315 2=Modifica        -2 -     (S/N)=S                        opz. 02¬
157400060315 3=Duplica         -3 -     (S/N)=S                        opz. 03¬
157500060315 4=Annullamento    -4 -     (S/N)=S                        opz. 04¬
157600060315 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
157700970526                   -6 -     (S/N)=N                        opz. 06¬
157800060315                   -7 -     (S/N)=N                        opz. 07¬
157900970527                   -8 -     (S/N)=N                        opz. 08¬
158000970527                   -9 -     (S/N)=N                        opz. 09¬
158100970527                   -10-     (S/N)=N                        opz. 10¬
158200970529                   -11-     (S/N)=N                        opz. 11¬
158300970529                   -12-     (S/N)=N                        opz. 12¬
158400060922                   -13-     (S/N)=N                        opz. 13¬
158500970526                   -  -     (S/N)=N                        opz. 14¬
158600970526                   -  -     (S/N)=N                        opz. 15¬
158700970526                   -  -     (S/N)=N                        opz. 16¬
158800970526                   -  -     (S/N)=N                        opz. 17¬
158900970526                   -  -     (S/N)=N                        opz. 18¬
159000970526                   -  -     (S/N)=N                        opz. 19¬
159100970526                   -  -     (S/N)=N                        opz. 20¬
159200970526                   -  -     (S/N)=N                        opz. 21¬
159300970526                   -  -     (S/N)=N                        opz. 22¬
159400970526                   -  -     (S/N)=N                        opz. 23¬
159500970526                   -  -     (S/N)=N                        opz. 24¬
159600970526** FUD - FUNZIONI
159700970526                   -        (S/N)=N                        funz.01¬
159800970526                   -        (S/N)=N                        funz.02¬
159900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
160000970526                   -        (S/N)=N                        funz.04¬
160100970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
160200060315F06=Inserimento    -        (S/N)=S                        funz.06¬
160300060926F07=Cappario       -        (S/N)=S                        funz.07¬
160400970527                   -        (S/N)=N                        funz.08¬
160500970527                   -        (S/N)=N                        funz.09¬
160600970527                   -        (S/N)=N                        funz.10¬
160700970526                   -        (S/N)=N                        funz.11¬
160800970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
160900060922                   -        (S/N)=N                        funz.13¬
161000970526                   -        (S/N)=N                        funz.14¬
161100970526                   -        (S/N)=N                        funz.15¬
161200970526                   -        (S/N)=N                        funz.16¬
161300970526                   -        (S/N)=N                        funz.17¬
161400970526                   -        (S/N)=N                        funz.18¬
161500970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
161600970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
161700970527                   -        (S/N)=N                        funz.21¬
161800970527                   -        (S/N)=N                        funz.22¬
161900970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
162000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
