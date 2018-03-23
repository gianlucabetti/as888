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
001100080522     FAZcpp02L  IF   E           K DISK    infds(azcppinf)
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
008600080522     D tiSIQ0ds      E DS
008700060314     D tisi00ds      E DS
008800060922     D tibs02ds      E DS
008900061030     D dcppflo       E DS
009000970526     D*------------
009100970526     D KPJBA         E DS
009200080522
009300080522     D azcppinf        ds
009400080522     D  azcppnrri            397    400i 0
009500060313     d
009600060313     d Dataiso         s               d   datfmt(*iso)
009700060313     d Dataeur         s               d   datfmt(*eur)
009800060329     d Datadmy         s               d   datfmt(*dmy)
009900060922     d knar            s                   Like(cppnar)
010000080519     d*kcap            s                   Like(cppcap)
010100080519     d kntw            s                   Like(cppntw)
010200060329     d kcod            s                   Like(tblCod)
010300060329     d kkey            s                   Like(TblKey)
010400060404     d opzione         s              2
010500970526     C*------------------------------------------------------------------------*
010600970526     C* MAIN LINES
010700970526     C*------------------------------------------------------------------------*
010800970526     C*
010900970526     C* SE NON CI SONO ERRORI, PROSEGUE
011000970526IF  1C     ERRL00        IFNE      '1'
011100970526     C*
011200970526     C* CARICA IL SFILE
011300970526     C                   EXSR      POSSFL
011400970526     C*
011500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011600970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011900970526     C*
012000970526     C* REIMPOSTA ERRORE DI PROCERURA
012100970526     C                   MOVEL     '0'           ERRL00
012200970526     C*
012300970526     C* EMETTE IL SFILE
012400970526     C                   EXSR      OUTSFL
012500970526     C*
012600970526     C* GESTIONE TASTI FUNZIONALI
012700970526     C                   EXSR      FUNCON
012800970526     C*
012900970526E   2C                   ENDDO
013000970527     C*
013100970527     C* CI SONO ERRORI IN ENTRATA
013200970527X   1C                   ELSE
013300060922     C                   MOVEL     '1'           dq0ERR
013400060922     C                   MOVEL     DSMSMS        dq0MSG
013500970526E   1C                   ENDIF
013600970526     C* FINE PGM
013700970526     C                   SETON                                        LR
013800970526     C*------------------------------------------------------------------------*
013900970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
014000970526     C*------------------------------------------------------------------------*
014100970526     C     F03GES        BEGSR
014200970526     C*
014300970526     C* IMPOSTA "PREMUTO F03"
014400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014500060922     C                   MOVEL     '1'           dq0F03                         *DI PROCEDURA
014600970526     C*
014700970526     C                   ENDSR
014800970526     C*------------------------------------------------------------------------*
014900970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
015000970526     C*------------------------------------------------------------------------*
015100970526     C     F05GES        BEGSR
015200970526     C*
015300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
015400970526     C*
015500970526     C                   ENDSR
015600970526     C*------------------------------------------------------------------------*
015700970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
015800970526     C*------------------------------------------------------------------------*
015900970526     C     F06GES        BEGSR
016000970526     C*
016100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
016200970603     C                   MOVEL     *BLANKS       DNAR
016300970602     C                   MOVEL     *BLANKS       DCAP
016400060922     c                   clear                   dntw
016500060922     c                   clear                   ddde
016600060922     c                   clear                   ddsc
016700080522     c                   clear                   dnrr
016800080422     c                   movel     'NO'          duti
016900060922     C                   MOVEL     *BLANKS       Dflo
017000970529     C                   MOVEL     DEPOP0        WOP0
017100970529     C                   MOVEL     'F06'         WOP1
017200060922     C                   MOVEL     'TISIQ1R'     WPGM              8
017300060314     C                   EXSR      LANPGMp                                      *LANCIA PGM
017400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017500970526E   1C                   ENDIF
017600970526     C*
017700970526     C                   ENDSR
017800060926     C*------------------------------------------------------------------------*
017900060926     C* F07GES - GESTIONE F07 -> int cappario
018000060926     C*------------------------------------------------------------------------*
018100060926     C     F07GES        BEGSR
018200060926     C* IMPOSTA LA DS DI PROCEDURA
018300060926     c                   clear                   tisi00ds
018400060926     c                   if        amgpde>0
018500060926     c                   movel     amgpde        d00dri
018600060926     c                   else
018700060926     c                   movel     datcor        d00dri
018800060926     c                   endif
018900060927     C                   MOVEL     'C05'         d00OP0                         *LIVELLO PROCEDURA
019000060926     c
019100060926     C                   clear                   d00OP1                         *LIVELLO PGM
019200060926     C                   MOVEL     '0'           d00F03                         *PREMUTO F03=FINE
019300060926     C                   MOVEL     '0'           d00F12                         *PREMUTO F12=RITORNO
019400060926     C                   MOVEL     '0'           d00ERR                         *'1'=ERRORE
019500060926     C                   MOVEL     *BLANKS       d00MSG                         *MESSAGGIO DI RITORN
019600060926     C                   clear                   d00NAR                         *NAZIONE
019700060926     C                   clear                   d00CAP                         *CAP
019800060926     C                   clear                   d00LOC                         *LOCALITA'
019900060926     C*
020000060926     C                   CALL      'TISI02R'
020100060926     C                   PARM                    KPJBA
020200060926     C                   PARM                    tisi00ds
020300060926     C                   ENDSR
020301080828     C*------------------------------------------------------------------------*
020302080828     C* F08GES - GESTIONE F08 -> Controlla cap/località
020303080828     C*------------------------------------------------------------------------*
020304080828     C     F08GES        BEGSR
020322080828     C*
020323080828     C                   CALL      'TISI94R'
020324080828     C                   PARM                    KPJBA
020325080828     c*
020326080828     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500970526     C* F12GES - GESTIONE F12 -> RITORNO
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F12GES        BEGSR
020800970526     C*
020900970526     C* IMPOSTA "PREMUTO F12"
021000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021100060922     C                   MOVEL     '1'           dq0F12                         *DI PROCEDURA
021200970526     C*
021300970526     C                   ENDSR
021400970529     C*------------------------------------------------------------------------*
021500060922     C* F13GES - GESTIONE F13 ->
021600970529     C*------------------------------------------------------------------------*
021700970529     C     F13GES        BEGSR
021800970529     C*
021900970529     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
022200970526     C*------------------------------------------------------------------------*
022300970526     C     F23GES        BEGSR
022400970526     C*
022500970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022600970526     C*
022700970526     C                   ENDSR
022800970526     C*------------------------------------------------------------------------*
022900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023000970526     C*------------------------------------------------------------------------*
023100970526     C     F24GES        BEGSR
023200970526     C*
023300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023400970526     C*
023500970526     C                   ENDSR
023600970526     C*------------------------------------------------------------------------*
023700970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023800970526     C*------------------------------------------------------------------------*
023900970526     C     RUPGES        BEGSR
024000970526     C*
024100970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
024200970526     C*
024300970526     C                   ENDSR
024400970526     C*------------------------------------------------------------------------*
024500970526     C* ENTGES - GESTIONE ENTER -> INVIO
024600970526     C*------------------------------------------------------------------------*
024700970526     C     ENTGES        BEGSR
024800970526     C*
024900970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025000970526     C                   EXSR      NEWPOS
025100970526     C*
025200970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025400970526IF  2C     SWPOSI        IFNE      0
025500970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
025600970526X   2C                   ELSE
025700970526IF  3C     SFLULT        IFNE      0
025800970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025900970526     C*
026000970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026100970526IF  4C     SWMODI        IFNE      0
026200970526     C                   EXSR      POSSFL
026300970526E   4C                   ENDIF
026400970526E   3C                   ENDIF
026500970526E   2C                   ENDIF
026600970526E   1C                   ENDIF
026700970526     C*
026800970526     C                   ENDSR
026900970526     C*------------------------------------------------------------------------*
027000970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
027100970526     C*------------------------------------------------------------------------*
027200970526     C     O01GES        BEGSR
027300970526     C*
027400970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
027500060922     C                   MOVEL     V5cNAR        dq0NAR
027600060922     C                   MOVEL     V5cCAP        dq0CAP
027700060922     C                   MOVEL     V5cntw        dq0ntw
027800060922     C                   MOVEL     V5hdde        dq0dde
027900060922     C                   MOVEL     V5hdsc        dq0dsc
028000080520     C                   MOVEL     V5huti        dq0uti
028100060922     C                   MOVEL     v5hflo        Dq0flo
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
029200060922     C                   MOVEL     V5cNAR        DNAR
029300060922     C                   MOVEL     V5cCAP        DCAP
029400060922     C                   MOVEL     V5cntw        Dntw
029500080520     C                   MOVEL     V5huti        Duti
029600060922     C                   MOVEL     V5hflo        Dflo
029700060922     C                   MOVEL     V5hdde        Ddde
029800060922     C                   MOVEL     V5hdsc        Ddsc
029900080522     C                   MOVEL     V5hnrr        Dnrr
030000970529     C                   MOVEL     DEPOP0        WOP0
030100970526     C                   MOVEL     'O02'         WOP1
030200060925     C                   MOVEL     'TISIQ1R'     WPGM
030300060315     C                   EXSR      LANPGMP
030400060328     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
030500970528E   1C                   ENDIF
030600970526     C*
030700970526     C                   ENDSR
030800970611     C*------------------------------------------------------------------------*
030900970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
031000970611     C*------------------------------------------------------------------------*
031100970611     C     O03GES        BEGSR
031200970611     C*
031300970611     C* DUPLICA
031400970611IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031500060922     C                   MOVEL     V5cNAR        DNAR
031600060922     C                   MOVEL     V5cCAP        DCAP
031700060922     C                   MOVEL     V5cntw        Dntw
031800080520     C                   MOVEL     V5huti        Duti
031900060922     C                   MOVEL     V5hflo        Dflo
032000060922     C                   MOVEL     V5hdde        Ddde
032100060922     C                   MOVEL     V5hdsc        Ddsc
032200080522     c                   clear                   dnrr
032300060329     C                   MOVEL     DEPOP0        WOP0
032400060329     C                   MOVEL     'O03'         WOP1
032500060925     C                   MOVEL     'TISIQ1R'     WPGM
032600060329     C                   EXSR      LANPGMP
032700970611     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
032800970611E   1C                   ENDIF
032900970611     C*
033000970611     C                   ENDSR
033100970526     C*------------------------------------------------------------------------*
033200970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
033300970526     C*------------------------------------------------------------------------*
033400970526     C     O04GES        BEGSR
033500970526     C*
033600060327IF  1C     ERRL00        IFEQ      '0'
033700060922     C                   MOVEL     V5cNAR        DNAR
033800060922     C                   MOVEL     V5cCAP        DCAP
033900060922     C                   MOVEL     V5cntw        Dntw
034000080520     C                   MOVEL     V5huti        Duti
034100060922     C                   MOVEL     V5hflo        Dflo
034200060922     C                   MOVEL     V5hdde        Ddde
034300060922     C                   MOVEL     V5hdsc        Ddsc
034400080522     C                   z-add     V5hnrr        Dnrr
034500060327     C                   MOVEL     DEPOP0        WOP0
034600060327     C                   MOVEL     'O04'         WOP1
034700060925     C                   MOVEL     'TISIQ1R'     WPGM
034800060327     C                   EXSR      LANPGMP
034900060925     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
035000060327E   1C                   ENDIF
035100970526     C*
035200970526     C                   ENDSR
035300970526     C*------------------------------------------------------------------------*
035400970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
035500970526     C*------------------------------------------------------------------------*
035600970526     C     O05GES        BEGSR
035700970526     C*
035800060327     C                   MOVEL     DEPOP0        WOP0
035900060922     C                   MOVEL     V5cNAR        DNAR
036000060922     C                   MOVEL     V5cCAP        DCAP
036100060922     C                   MOVEL     V5cntw        Dntw
036200080520     C                   MOVEL     V5huti        Duti
036300060922     C                   MOVEL     V5hflo        Dflo
036400060922     C                   MOVEL     V5hdde        Ddde
036500060922     C                   MOVEL     V5hdsc        Ddsc
036600080522     C                   clear                   Dnrr
036700060327     C                   MOVEL     'O05'         WOP1
036800060925     C                   MOVEL     'TISIQ1R'     WPGM
036900060327     C                   EXSR      LANPGMP
037000970529     C*
037100970529     C                   ENDSR
037200970526     C*------------------------------------------------------------------------*
037300970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
037400970526     C*------------------------------------------------------------------------*
037500970526     C     NEWPOS        BEGSR
037600970526     C*
037700970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
037800970526     C*         1= RICHIESTO POSIZIONAMENTO
037900970526     C                   Z-ADD     0             SWPOSI            7 0
038000970526     C*
038100970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
038200970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
038300970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
038400970526     C                   Z-ADD     0             SWMODI            1 0
038500970530     C*
038600970530     C* REIMPOSTA INDICATORI DI ERRORE
038700060313     C                   SETOFF                                       808281
038800080520     C                   SETOFF                                       83
038900970526     C*
039000060403     c
039100060403     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
039200080519IF  1C*    VIDPNA        IFNE      DEPNAR
039300080519     C*    VIDPCA        ORNE      DEPCAP
039400080519     C*    VIDPnt        ORNE      DEPntw
039500080519     C*    VIDPde        ORNE      DEPpde
039600080519     c     vidint        ifne      depint
039700080519     c     vidinz        orne      depinz
039800080519     C     VIDPde        ORNE      DEPpde
039900080519     C     VIDPnz        ORNE      DEPpnz
040000080519     C     VIDPcp        ORNE      DEPpcp
040100970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
040200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
040300970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
040400080519     C**                 MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
040500080519     C**                 MOVEL     VIDPCA        DEPCAP
040600080519     C**                 MOVEL     VIDpnt        DEPntw
040700080519     C**                 MOVEL     VIDpde        DEPpde
040800080519     C                   MOVEL     VIDint        DEPint
040900080519     C                   MOVEL     VIDinz        DEPinz
041000080519     C                   MOVEL     VIDpde        DEPpde
041100080519     C                   MOVEL     VIDpnz        DEPpnz
041200080519     C                   MOVEL     VIDpcp        DEPpcp
041300970526E   2C                   ENDIF
041400970526E   1C                   ENDIF
041500970526     C*
041600970526     C                   ENDSR
041700970526     C*------------------------------------------------------------------------*
041800970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
041900970526     C*------------------------------------------------------------------------*
042000970526     C     SETFIL        BEGSR
042100060330     c                   clear                   knar
042200080519     c                   clear                   kntw
042300080519     c**                 clear                   kcap
042400080519     c                   eval      knar=vidinz
042500080519     c                   eval      kntw=vidint
042600080519     c**                 eval      kcap=vidpca
042700060330     c
042800970529     C*
042900080520     C     Kcpp          SETLL     AZCpp02L                           99
043000060330     c
043100970526     C* LETTURA RECORD
043200970526IF  1C     *IN99         IFEQ      *OFF
043300970526     C                   EXSR      LETFIL
043400970526E   1C                   ENDIF
043500970526     C*
043600970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
043700970526IF  1C     *IN99         IFEQ      *ON
043800970526     C                   Z-ADD     1             SFLEOF
043900970526E   1C                   ENDIF
044000970526     C*
044100970526     C                   ENDSR
044200970526     C*------------------------------------------------------------------------*
044300970526     C* LETFIL - LETTURA RECORD
044400970526     C*------------------------------------------------------------------------*
044500970526     C     LETFIL        BEGSR
044600970526     C*
044700970526     C* LEGGE FINO A:
044800970526     C*     - FINE FILE
044900970526     C*     - TROVATO RECORD VALIDO
045000970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
045100970526DO  1C     *IN99         DOWEQ     *OFF
045200970526     C     WRECOK        ANDEQ     'N'
045300970529     C*
045400970529     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
045500080519     C                   READ      AZcpp02L                               99
045600970526     C*
045700970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
045800970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
045900970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
046000970530X   2C                   ELSE
046100970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
046200970530E   2C                   ENDIF
046300970526E   1C                   ENDDO
046400970526     C*
046500970526     C                   ENDSR
046600970526     C*------------------------------------------------------------------------*
046700970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
046800970526     C*------------------------------------------------------------------------*
046900970526     C     CHKREC        BEGSR
047000060922     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
047100060922     C* controllo data
047200060922     c                   if        amgpde>0 and (amgpde<cppdde or
047300060922     c                             amgpde>cppdsc)
047400060922     C                   MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
047500060329     c                   endif
047600060922     c* Network
047700080519     c***                if        vidpnt<>*blanks and
047800080519     c***                          vidpnt<>cppntw
047900080519     C***                MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
048000080519     c***                endif
048100080519     c* Nazione
048200080519     c                   if        vidpnz<>*blanks and
048300080519     c                             vidpnz<>cppnar
048400080519     C                   MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
048500080519     c                   endif
048600080519     c* cap
048700080519     c                   if        vidpcp<>*blanks and
048800080519     c                             vidpcp<>cppcap
048900080519     C                   MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
049000080519     c                   endif
049100060329     c
049200970526     C                   ENDSR
049300970526     C*------------------------------------------------------------------------*
049400970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
049500970526     C*------------------------------------------------------------------------*
049600970526     C     IRESFL        BEGSR
049700970529     C*
049800970529     C                   MOVEL     *BLANKS       OPZKEY
049900060922     c* Nazione e decodifica
050000060922     c                   eval      v5cnar=cppnar
050100060922     c                   eval      kcod='15'
050200060922     c                   movel(p)  v5cnar        kkey
050300060922     c     ktab          chain     tabel00f
050400060922     c                   if        %found(tabel00f)
050500060922     c                   movel     tbluni        v5dnar
050600060922     c                   else
050700080411     c                   if        v5cnar<>'*'
050800060922     c                   movel     *all'?'       v5dnar
050900080411     c                   else
051000080411     c                   clear                   v5dnar
051100060922     c                   endif
051200080411     c                   endif
051300060922     c
051400060922     c* CAP
051500060922     c                   eval      v5ccap=cppcap
051600060922     c* Decorrenza
051700060922     c                   movel     cppdde        v5hdde
051800060922     c     *iso          movel     cppdde        dataiso
051900060922     c                   movel     dataiso       datadmy
052000060922     c                   movel     datadmy       v5cdde
052100060922     c* scadenza
052200060922     c                   movel     cppdsc        v5hdsc
052300060922     c     *iso          movel     cppdsc        dataiso
052400060922     c                   movel     dataiso       datadmy
052500060922     c                   movel     datadmy       v5cdsc
052600060922     c* network e decodifica
052700060922     C                   MOVEL     Cppntw        V5cntw
052800080519     C*                  CLEAR                   tibs02ds
052900080519     C*                  MOVEL     'C'           T02MOD
053000080519     C*                  MOVEL     KNSIF         T02SIF
053100080519     C*                  MOVEL     'NTW'         T02COD
053200080519     C*                  MOVEL(P)  cppntw        T02KE1
053300080519     C*                  CALL      'TIBS02R'
053400080519     C*                  PARM                    KPJBA
053500080519     C*                  PARM                    tibs02ds
053600060922      *
053700080519     C*    T02ERR        IFNE      *BLANKS
053800080519     C*                  MOVEL     *all'?'       v5dntw
053900080519     c*                  eval      %subst(v5dntw:1:2)='- '
054000080519     C*                  ELSE
054100080519     c*                  eval      %subst(v5dntw:1:2)='- '
054200080519     c*                  eval      %subst(v5dntw:3:11)=t02uni
054300080519     C*                  ENDIF
054400060922     c
054500060922     c* utilizzo
054600080520     C                   if        Cpputi='S'
054700080520     c                   eval      v5Huti='SI'
054800080520     c                   else
054900080520     c                   eval      v5Huti='NO'
055000080520     c                   endif
055100080520     c                   movel     cppflo        dcppflo
055200080519     c* ritiri
055300080519     c                   clear                   v5crit
055400080520     c                   if        §cpprit='S'
055500080520     c                   eval      v5crit='SI'
055600080520     c                   endif
055700080519     c* Spedizioni
055800061030     c                   clear                   v5cbol
055900080520     c                   if        §cppall='S'
056000080520     c                   eval      v5cbol='Tutte        '
056100080520     c                   else
056200061030     c                   if        §cppcas='S'
056300080520     c                   eval      v5cbol='C/Assegno    '
056400061030     c                   endif
056500080409     c                   if        §cpppas<>' '
056600080409     c                   if        §cppcas='S'
056700080520     c                   eval      v5cbol='C/A-Assegnato'
056800080409     c                   else
056900080520     c                   eval      v5cbol='Assegnato    '
057000080409     c                   endif
057100080409     c                   endif
057200080520     c                   endif
057300060922     c
057400060922     c                   eval      v5hflo=cppflo
057500080522     c                   z-add     azcppnrri     v5hnrr
057600060313     c
057700970526     C                   ENDSR
057800970526     C*------------------------------------------------------------------------*
057900970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
058000970526     C*------------------------------------------------------------------------*
058100970526     C     CHKPAR        BEGSR
058200060313     c* controllo data di riferimento se errata
058300060922     c                   clear                   amgpde
058400060313     c                   if        vidpde>0
058500060313     C                   Z-ADD     vidpde        G08DAT
058600060313     C                   Z-ADD     *ZEROS        G08INV
058700060313     C                   MOVEL     '0'           G08ERR
058800060313     C                   CALL      'XSRDA8'
058900060313     C                   PARM                    WLBDA8
059000060313     c                   if        g08err='1'
059100060313     C                   MOVEL     MSG(3)        DSMSMS
059200060313     C                   MOVEL     '1'           ERRL00
059300060922     C                   SETON                                        81
059400060313E   2C                   else
059500060314     c                   z-add     g08dat        vidpde
059600060922     C                   Z-ADD     G08INV        amgpde            8 0          *DATA CORRENTE AA/M/
059700060313     c                   endif
059800060313     c                   endif
059900060922     c* Network
060000080520     c*                  if        vidpnt<>*blanks
060100080520     C*                  CLEAR                   tibs02ds
060200080520     C*                  MOVEL     'C'           T02MOD
060300080520     C*                  MOVEL     KNSIF         T02SIF
060400080520     C*                  MOVEL     'NTW'         T02COD
060500080520     C*                  MOVEL     *BLANKS       T02KE1
060600080520     C*                  MOVEL     vidpnt        T02KE1
060700080520     C*                  CALL      'TIBS02R'
060800080520     C*                  PARM                    KPJBA
060900080520     C*                  PARM                    tibs02ds
061000060922      *
061100080520     C*    T02ERR        IFNE      *BLANKS
061200080520     C*                  MOVEL     MSG(4)        DSMSMS
061300080520     C*                  MOVEL     '1'           ERRL00
061400080520     C*                  SETON                                        82
061500080520     C*                  ENDIF
061600080520     c*                  endif
061700080520     c* Nazione
061800080520     c                   if        vidpnz<>*blanks
061900080520     c                   if        vidpnz<>'*  '
062000080520     c                   eval      kcod='15'
062100080520     c                   movel(p)  vidpnz        kkey
062200080520     c     ktab          chain     tabel
062300080520     c                   if        not %found(tabel00f) or
062400080520     c                             tblflg<>' '
062500080520     c                   seton                                        82
062600080520     C                   MOVEL     MSG(5)        dsmsms
062700080520     C                   MOVEL     '1'           ERRL00
062800080520     c                   endif
062900080520     c                   else
063000080520     c* se nazione = '*' allora anche cap deve essere = '*'
063100080520     c                   if        vidpcp<>'*        ' and vidpcp<>*blanks
063200080520     c                   seton                                        83
063300080520     C                   MOVEL     MSG(7)        dsmsms
063400080520     C                   MOVEL     '1'           ERRL00
063500080520     c                   endif
063600080520     c                   endif
063700080520     c                   endif
063800970526     C*
063900970526     C                   ENDSR
064000060314     C*------------------------------------------------------------------------*
064100060314     C* LANPGM - LANCIA UN PROGRAMMA
064200060314     C*------------------------------------------------------------------------*
064300060314     C     LANPGMp       BEGSR
064400060314     C*
064500060314     C* IMPOSTA LA DS DI PROCEDURA
064600060922     c                   clear                   tiSIQ0ds
064700060922     C                   MOVEL     WOP0          dq0OP0                         *LIVELLO PROCEDURA
064800060922     C                   MOVEL     WOP1          dq0OP1                         *LIVELLO PGM
064900060922     C                   MOVEL     '0'           dq0F03                         *PREMUTO F03=FINE
065000060922     C                   MOVEL     '0'           dq0F12                         *PREMUTO F12=RITORNO
065100060922     C                   MOVEL     '0'           dq0ERR                         *'1'=ERRORE
065200060922     C                   MOVEL     *BLANKS       dq0MSG                         *MESSAGGIO DI RITORN
065300060922     C                   MOVEL     DNAR          dq0NAR                         *NAZIONE
065400060922     C                   MOVEL     DCAP          dq0CAP                         *CAP
065500060922     C                   MOVEL     Dntw          dq0ntw                         *LOCALITA'
065600060922     C                   MOVEL     Ddde          dq0dde                         *LOCALITA'
065700060922     C                   MOVEL     Ddsc          dq0dsc                         *LOCALITA'
065800060922     C                   MOVEL     Duti          dq0uti                         *LOCALITA'
065900060922     C                   MOVEL     Dflo          dq0flo                         *LOCALITA'
066000080522     C                   z-add     Dnrr          dq0nrr                         *LOCALITA'
066100060314     C*
066200060314     C                   CALL      WPGM
066300060314     C                   PARM                    KPJBA
066400060922     C                   PARM                    tiSIQ0ds
066500060314     C*
066600060314     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
066700060922IF  1C     DQ0F03        IFEQ      '1'                                          *PREMUTO F03
066800060314     C                   MOVEL     '1'           ERRL00                         *ERRORE
066900060314     C                   MOVEL     '1'           F03L00                         *FINE PGM
067000060314X   1C                   ELSE                                                   *NON PREMUTO F03
067100060922     C                   MOVEL     DQ0MSG        DSMSMS                         *MESSAGGIO DI RITORN
067200060314E   1C                   ENDIF
067300060314     C*
067400060314     C                   ENDSR
067500970527     C*------------------------------------------------------------------------*
067600970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
067700970527     C*------------------------------------------------------------------------*
067800970527     C     CARTAB        BEGSR
067900970527     C*
068000970527     C                   ENDSR
068100970526     C*------------------------------------------------------------------------*
068200970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
068300970526     C*------------------------------------------------------------------------*
068400970526     C     CHKFOP        BEGSR
068500970626     C*
068600970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
068700060404IF  1C     opzione       IFEQ      '01'
068800970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
068900970626     C                   MOVEL     'S'           OPDATT
069000970626     C                   MOVEL     OPDRIG        OPD(1)
069100970626X   1C                   ELSE
069200970626     C* OPZIONI
069300970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
069400970626     C                   MOVEL     'S'           OPDATT
069500970626     C                   MOVEL     OPDRIG        OPD(5)
069600970626E   1C                   ENDIF
069700970626     C*
069800970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
069900060404IF  1C     opzione       IFEQ      '02'
070000970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
070100970626     C                   MOVEL     'S'           OPDATT
070200970626     C                   MOVEL     OPDRIG        OPD(2)
070300970627     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
070400970627     C                   MOVEL     'S'           OPDATT
070500970627     C                   MOVEL     OPDRIG        OPD(3)
070600970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
070700970626     C                   MOVEL     'S'           OPDATT
070800970626     C                   MOVEL     OPDRIG        OPD(4)
070900970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
071000970626     C                   MOVEL     'S'           FUDATT
071100970626     C                   MOVEL     FUDRIG        FUD(6)
071200060327    1C                   ENDIF
071300060331     c
071400060404IF  1C     opzione       IFEQ      '05'
071500060331     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
071600060331     C                   MOVEL     'N'           OPDATT
071700060331     C                   MOVEL     OPDRIG        OPD(2)
071800060331     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
071900060331     C                   MOVEL     'N'           OPDATT
072000060331     C                   MOVEL     OPDRIG        OPD(3)
072100060331     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
072200060331     C                   MOVEL     'N'           OPDATT
072300060331     C                   MOVEL     OPDRIG        OPD(4)
072400060331     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
072500060331     C                   MOVEL     'N'           FUDATT
072600060331     C                   MOVEL     FUDRIG        FUD(6)
072700060331    1C                   ENDIF
072800060927     c
072900060927     c* L'opzione F7 solo se non richiamato da cappario
073000060927     c                   if        *in10
073100060927     C                   MOVEL     FUD(7)        FUDRIG                         *INSERIMENTO
073200060927     C                   MOVEL     'N'           FUDATT
073300060927     C                   MOVEL     FUDRIG        FUD(7)
073400060927     c                   endif
073401080828     c* L'opzione F8 solo se utente EDP
073402080828     c                   if        %subst(knmus:1:3)<>'EDP'
073403080828     C                   MOVEL     FUD(8)        FUDRIG                         *INSERIMENTO
073404080828     C                   MOVEL     'N'           FUDATT
073405080828     C                   MOVEL     FUDRIG        FUD(8)
073406080828     c                   endif
073500970526     C*
073600970526     C* GESTIONE OPZIONI A VIDEO
073700970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
073800970526     C                   EXSR      OPZGES
073900970526     C*
074000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
074100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
074200970526     C                   EXSR      FUNGES
074300970526     C*
074400970526     C                   ENDSR
074500020919     C*--------------------------------------------------------------------------------------------*
074600020919     C* REPERISCE I DATI UTENTE
074700020919     C*--------------------------------------------------------------------------------------------*
074800020919     C     REPDATIUTE    BEGSR
074900020919     C*
075000020919     C* INIZIALIZZA VARIABILI DI WRK
075100020919     C                   CLEAR                   TIBS34DS
075200020919     C                   CLEAR                   AZUTEDS
075300020919     C                   CLEAR                   DDATIUTE
075400020919     C*
075500020919     C     *DTAARA       DEFINE    §azute        azuteds
075600020919     C     *DTAARA       DEFINE    §datiute      ddatiute
075700020919     C                   IN(E)     *DTAARA
075800020919if  1C                   IF        %Error
075900020919     c                   EVAL      I34Tla = 'L'
076000020919     C                   CALL      'TIBS34R'
076100020919     C                   PARM                    Tibs34Ds
076200020919     C                   IN        *DTAARA
076300020919e   1C                   ENDIF
076400020919     C*
076500020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
076600020919     C                   MOVEL(P)  RSUT          DSFIRS           20
076700020919     C*
076800020919     C                   ENDSR
076900970526     C*------------------------------------------------------------------------*
077000970526     C* *INZSR - OPERAZIONI INIZIALI
077100970526     C*------------------------------------------------------------------------*
077200970526     C     *INZSR        BEGSR
077300970526     C*---
077400970526     C* RICEVIMENTO PARAMETRI
077500970526     C*---
077600970526     C     *ENTRY        PLIST
077700970612     C                   PARM                    KPJBA
077800060922     C                   PARM                    tiSIQ0ds
077900970526     C*
078000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
078100970526     C     XMSGPA        PLIST
078200970526     C                   PARM                    DSMSMS
078300970526     C                   PARM                    DSMSPG
078400970526     C                   PARM                    DSMSKE
078500970526     C                   PARM                    DSMSER
078600970526     C*
078700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
078800970526     C                   CALL      'XKEY'
078900970526     C                   PARM                    DSKEY
079000970526     C*---
079100970526     C* VARIABILI NON RIFERITE AL DATA BASE
079200970526     C*---
079300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
079400970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
079500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
079600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
079700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
079800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
079900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
080000970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
080100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
080200060922     C     *LIKE         DEFINE    DQ0OP0        DEPOP0                         *DI DEPOSITO
080300060922     C     *LIKE         DEFINE    DQ0OP0        WOP0                           *DI LAVORO
080400060922     C     *LIKE         DEFINE    DQ0OP1        WOP1
080500060922     C     *LIKE         DEFINE    Dq0NAR        DNAR
080600060922     C     *LIKE         DEFINE    Dq0CAP        DCAP
080700060922     C     *LIKE         DEFINE    Dq0ntw        Dntw
080800060922     C     *LIKE         DEFINE    Dq0dde        Ddde
080900060922     C     *LIKE         DEFINE    Dq0dsc        Ddsc
081000060922     C     *LIKE         DEFINE    Dq0uti        Duti
081100060922     C     *LIKE         DEFINE    Dq0flo        Dflo
081200080522     C     *LIKE         DEFINE    dq0nrr        Dnrr
081300970526     C*---
081400970526     C* VARIABILI DI CONTROLLO
081500970526     C*---
081600970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
081700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
081800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
081900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
082000970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
082100970530     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
082200970526     C*
082300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
082400060922     C                   MOVEL     'TISIQ0R'     DSMSPG
082500060922     C                   MOVEL     'TISIQ0R'     MSGQUE
082600970526     C                   MOVEL     *BLANKS       MSGKEY
082700970526     C                   MOVEL     *BLANKS       DSMSMS
082800970526     C                   MOVEL     *BLANKS       DSMSKE
082900970526     C                   Z-ADD     *ZEROS        DSMSER
083000060330     c
083100060922     C     *LIKE         DEFINE    Cppdde        DEPpde                         *DEPOSITI
083200080520     C     *LIKE         DEFINE    cppNAR        DEPpnz                         *DEPOSITI
083300080520     C     *LIKE         DEFINE    cppNAR        DEPinz                         *DEPOSITI
083400080520     C     *LIKE         DEFINE    CppCAP        DEPpcp
083500080520     C     *LIKE         DEFINE    Cppntw        DEPint
083600970526     C*---
083700970526     C* CHIAVI DI LETTURA
083800970526     C*---
083900060922     C     kcpp          KLIST
084000080519     C                   KFLD                    Kntw                           *CAP
084100080519     C                   KFLD                    Knar                           *CAP
084200060922     c     Ktab          Klist
084300060922     c                   Kfld                    codut             1 0
084400060922     c                   Kfld                    kCod
084500060922     c                   Kfld                    kKey
084600060922     c                   z-add     1             codut
084700060922     c
084800970527     C* REPERIMENTO DATI UTENTE
084900020919     C                   EXSR      REPDATIUTE
085000970527     C*
085100970527     C* CARICA LE TABELLE OCCORRENTI
085200970527     C                   EXSR      CARTAB
085300970526     C*
085400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
085500060922     C                   MOVEL     DQ0OP0        DEPOP0                         *LIVELLO PROCEDURA
085600060927     c* se richiamato da cappario disabilito tasto funzionale F7-Cappario
085700060927     c                   movel     dq0op0        wchiama           1
085800060927     c                   if        wchiama='C'
085900060927     c                   seton                                        10
086000060927     c                   else
086100060927     c                   setoff                                       10
086200060927     c                   endif
086300970526     C*
086400970526     C* CALCOLA LA DATA CORRENTE
086500970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
086600970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
086700970526     C                   Z-ADD     WN8           G08DAT
086800970526     C                   Z-ADD     *ZEROS        G08INV
086900970526     C                   MOVEL     '0'           G08ERR
087000970526     C                   CALL      'XSRDA8'
087100970526     C                   PARM                    WLBDA8
087200970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
087300060313     c
087400060313     c* Se non passata la data di ingresso, uso data corrente
087500060922     c                   if        DQ0dri=0
087600060922     c                   eval      amgpde=datcor
087700060313     c                   eval      vidpde=wn8
087800060313     c                   else
087900060922     c                   eval      amgpde=DQ0dri
088000060313     c* La imposto a video
088100060922     C                   Z-ADD     DQ0DRI        G08INV
088200060313     C                   Z-ADD     *ZEROS        G08DAT
088300060313     C                   MOVEL     '3'           G08ERR
088400060313     C                   CALL      'XSRDA8'
088500060313     C                   PARM                    WLBDA8
088600060313     C                   Z-ADD     G08DAT        VIDpde                         *DATA RIFERIMENTO
088700060313     c                   endif
088800970526     C*
088900970526     C* IMPOSTA I PARAMETRI DI INGRESSO
089000080520     C                   MOVEL     DQ0NAR        VIDPNZ                         *NAZIONE
089100080520     C                   MOVEL     DQ0NAR        VIDINZ                         *NAZIONE
089200080520     C                   MOVEL     DQ0CAP        VIDPCP                         *CAP
089300080520     C                   MOVEL     DQ0ntw        VIDint                         *CAP
089400060313     c
089500970526     C                   EXSR      CHKPAR
089600060922     c
089700970526IF  1C     ERRL00        IFEQ      '1'
089800060922     C                   MOVEL     '1'           DQ0ERR
089900060922     C                   MOVEL     DSMSMS        DQ0MSG
090000970526X   1C                   ELSE
090100080520     C                   MOVEL     VIDint        DEPint
090200080520     C                   MOVEL     VIDinz        DEPinz                         *DEPOSITI
090300080520     C                   MOVEL     VIDpde        DEPpde
090400080520     C                   MOVEL     VIDPCp        DEPpcp
090500080520     C                   MOVEL     VIDinz        DEPinz
090600970526E   1C                   ENDIF
090700970526     C*
090800970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
090900060404     c                   eval      opzione=%subst(depop0:2:2)
091000970526     C                   EXSR      CHKFOP
091100970526     C*
091200970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
091300060404IF  1C     opzione       IFEQ      '01'
091400970530     C                   MOVEL     DEC(3)        VIDING
091500970530E   1C                   ENDIF
091600060404IF  1C     opzione       IFEQ      '02'
091700970530     C                   MOVEL     DEC(1)        VIDING
091800970530E   1C                   ENDIF
091900060404IF  1C     opzione       IFEQ      '05'
092000970530     C                   MOVEL     DEC(2)        VIDING
092100970530E   1C                   ENDIF
092200970526     C*
092300970526     C                   ENDSR
092400970526     C*------------------------------------------------------------------------*
092500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
092600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
092700970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
092800970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
092900970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
093000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
093100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
093200970526     C*------------------------------------------------------------------------*
093300970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
093400970526     C*------------------------------------------------------------------------*
093500970526     C     POSSFL        BEGSR
093600970526     C*
093700970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
093800970526     C                   EXSR      CLRSFL
093900970526     C*
094000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
094100970526     C                   EXSR      SETFIL
094200970526     C*
094300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
094400970526     C     SFLEOF        IFEQ      0
094500970526     C                   EXSR      CARSFL
094600970526     C                   END
094700970526     C*
094800970526     C                   ENDSR
094900970526     C*------------------------------------------------------------------------*
095000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
095100970526     C*------------------------------------------------------------------------*
095200970526     C     ENTATT        BEGSR
095300970526     C*
095400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
095500970526     C                   Z-ADD     0             OPZOK             1 0
095600970526     C*
095700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
095800970526     C                   Z-ADD     0             SFLVAR            5 0
095900970526     C*
096000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
096100970526     C                   Z-ADD     0             SFLERR
096200970526     C*
096300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
096400970526     C                   Z-ADD     SFLULT        WFLULT
096500970526     C*
096600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
096700060922     C                   READC     SIQ0D05                                98
096800970526     C     *IN98         DOWEQ     '0'
096900970526     C*
097000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
097100970526     C                   ADD       1             SFLVAR
097200970526     C*
097300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
097400970526     C     OPZOK         IFEQ      0
097500970526     C*
097600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
097700970526     C     OPZKEY        IFNE      *BLANKS
097800970526     C                   EXSR      OPZCON
097900970526     C                   END
098000970526     C*
098100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
098200970526     C*            2. PULIZIA SCELTA IN SFL
098300970526     C     OPZOK         IFEQ      0
098400970526     C                   MOVEL     '0'           *IN41
098500970526     C                   MOVEL     *BLANKS       OPZKEY
098600970526     C                   ELSE
098700970526     C*
098800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
098900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
099000970526     C                   MOVEL     '1'           *IN41
099100970526     C                   Z-ADD     SFLULT        SFLERR
099200970526     C                   END
099300970526     C                   ELSE
099400970526     C*
099500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
099600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
099700970526     C                   MOVEL     '1'           *IN41
099800970526     C                   END                                                    OPZOK=0
099900970526     C*
100000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
100100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
100200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
100300970526     C     OPZOK         IFNE      0
100400970526     C                   Z-ADD     SFLERR        SFLNBR
100500970526     C                   ELSE
100600970526     C                   Z-ADD     SFLULT        SFLNBR
100700970526     C                   END
100800970526     C*
100900970526     C* AGGIORNO RECORD SFL
101000060922     C                   UPDATE    SIQ0D05
101100970526     C*
101200060922     C                   READC     SIQ0D05                                98
101300970526     C                   END                                                    DO WHILE
101400970526     C*
101500970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
101600970526     C                   Z-ADD     WFLULT        SFLULT
101700970526     C*
101800970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
101900970526     C     SFLVAR        IFEQ      0
102000970526     C*---                 MOVEL'1'       FINL00
102100970526     C                   END
102200970526     C*
102300970526     C                   ENDSR
102400970526     C*------------------------------------------------------------------------*
102500970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
102600970526     C*------------------------------------------------------------------------*
102700970526     C     CLRSFL        BEGSR
102800970526     C*
102900970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
103000970526     C* (0=NON FINE FILE  1=FINE FILE)
103100970526     C                   Z-ADD     0             SFLEOF
103200970526     C*
103300970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
103400970526     C                   Z-ADD     0             SFLULT
103500970526     C*
103600970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
103700970526     C                   MOVEL     '1'           *IN40
103800060922     C                   WRITE     SIQ0D06
103900970526     C                   MOVEL     '0'           *IN40
104000970526     C*
104100970526     C                   ENDSR
104200970526     C*------------------------------------------------------------------------*
104300970526     C* CARSFL - CARICAMENTO SFL
104400970526     C*------------------------------------------------------------------------*
104500970526     C     CARSFL        BEGSR
104600970526     C*
104700970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
104800970526     C     SFLULT        IFEQ      0
104900970526     C                   EXSR      SETFIL
105000970526     C                   END
105100970526     C*
105200970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
105300970526     C                   Z-ADD     SFLULT        WFLULT
105400970526     C*
105500970526     C* CICLO:
105600970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
105700970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
105800970526     C                   Z-ADD     1             SFLRIG
105900970526     C     SFLRIG        DOWLE     SFLPAG
106000970526     C     SFLEOF        ANDEQ     0
106100970526     C*
106200970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
106300970526     C                   MOVEL     *BLANKS       OPZKEY
106400970526     C*
106500970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
106600970526     C                   EXSR      IRESFL
106700970526     C*
106800970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
106900970526     C                   ADD       1             SFLULT
107000970526     C*
107100970526     C* SCRITTURA RECORD SFL
107200060922     C                   WRITE     SIQ0D05
107300970526     C                   ADD       1             SFLRIG
107400970526     C*
107500970526     C* LETTURA FILE DA CARICARE
107600970526     C                   EXSR      LETFIL
107700970526     C                   END                                                    DO WHILE
107800970526     C*
107900970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
108000970526     C     WFLULT        IFNE      SFLULT
108100970526     C                   Z-ADD     WFLULT        SFLNBR
108200970526     C                   ADD       1             SFLNBR
108300970526     C                   END
108400970526     C*
108500970526     C                   ENDSR
108600970526     C*------------------------------------------------------------------------*
108700970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
108800970526     C*------------------------------------------------------------------------*
108900970526     C     OUTSFL        BEGSR
109000970526     C*
109100970526     C* WRITE RECORD TESTATA VIDEO
109200060922     C                   WRITE     SIQ0D01
109300970526     C*
109400970526     C* VISUALIZZAZIONE MESSAGGIO
109500970526     C                   EXSR      SNDMSG
109600970526     C*
109700970526     C* WRITE RECORD OPZIONI VIDEO
109800060922     C                   WRITE     SIQ0D02
109900970526     C*
110000970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
110100060922     C                   WRITE     SIQ0D03
110200970526     C*
110300970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
110400060922     C                   WRITE     SIQ0D04
110500970526     C*
110600970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
110700970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
110800970526     C     SFLEOF        IFEQ      0
110900970526     C                   MOVEL     '0'           *IN42
111000970526     C                   ELSE
111100970526     C*
111200970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
111300970526     C                   MOVEL     '1'           *IN42
111400970526     C                   END
111500970526     C*
111600970526     C* WRITE RECORD TASTI FUNZIONALI
111700060922     C                   WRITE     SIQ0D09
111800970526     C*
111900970526     C* CASO DI SFL VUOTO
112000970526     C     SFLULT        IFLE      0
112100970526     C*
112200970526     C*    - WRITE RECORD VUOTO
112300060922     C                   WRITE     SIQ0D0A
112400060922     C                   READ      SIQ0D0A                                99
112500970526     C*
112600970526     C                   ELSE
112700970526     C*
112800970526     C* CASO DI SFL CON RECORDS
112900970526     C*
113000970526     C*    - WRITE RECORD CONTROLLO SFL
113100060922     C                   WRITE     SIQ0D06
113200060922     C                   READ      SIQ0D06                                99
113300970526     C                   END
113400970526     C*
113500970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
113600060922     C                   READ      SIQ0D03                                99
113700970526     C*
113800970526     C                   ENDSR
113900970526     C*------------------------------------------------------------------------*
114000970526     C* SNDMSG - MANDA MESSAGGIO
114100970526     C*------------------------------------------------------------------------*
114200970526     C     SNDMSG        BEGSR
114300970526     C*
114400970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
114500970526     C                   MOVEL     '1'           *IN40
114600060922     C                   WRITE     SIQ0DMC
114700970526     C                   MOVEL     '0'           *IN40
114800970526     C*
114900970526     C* VISUALIZZAZIONE MESSAGGIO
115000970526     C                   Z-ADD     0             MSGULT
115100970526     C                   CALL      'XMSG'        XMSGPA
115200970526     C     DSMSER        IFEQ      0
115300970526     C                   MOVEL     DSMSKE        MSGKEY
115400970526     C                   END
115500970526     C                   Z-ADD     1             MSGULT
115600060922     C                   WRITE     SIQ0DMS                                      SFL MESS
115700060922     C                   WRITE     SIQ0DMC                                      CONTR MESS
115800970526     C*
115900970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
116000970526     C                   MOVEL     *BLANKS       DSMSMS
116100970526     C*
116200970526     C                   ENDSR
116300970526     C*------------------------------------------------------------------------*
116400970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
116500970526     C*------------------------------------------------------------------------*
116600970526     C     FUNCON        BEGSR
116700970526     C*
116800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
116900970526     C                   EXSR      FUNVAR
117000970526     C*
117100970526     C* PULISCO CAMPI DI LAVORO
117200970526     C                   Z-ADD     0             I                 5 0
117300970526     C                   Z-ADD     0             W                 5 0
117400970526     C*
117500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
117600970526     C     FUNKEY        IFNE      *BLANKS
117700970526     C     1             DO        24            I
117800970526     C                   MOVEL     FUD(I)        FUDRIG
117900970526     C*
118000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
118100970526     C     FUNKEY        IFEQ      FUDFUN
118200970526     C     FUDATT        ANDEQ     'S'
118300970526     C                   Z-ADD     I             W
118400970526     C                   END
118500970526     C                   END                                                    DO WHILE
118600970526     C*
118700970526     C* FUNZIONE NON ATTIVA (ERRORE)
118800970526     C     W             IFEQ      0
118900970526     C*
119000970526     C* EMISSIONE MESSAGGIO DI ERRORE
119100970526     C                   MOVEL     MSG(2)        DSMSMS
119200970526     C                   END                                                    W=0
119300970526     C                   END                                                    FUNKEY NE *BLANKS
119400970526     C*
119500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
119600970526     C     W             IFNE      0
119700970526     C     FUNKEY        OREQ      *BLANKS
119800970526     C*
119900970526     C     FUNKEY        IFEQ      *BLANKS
120000970526     C                   EXSR      ENTGES
120100970526     C                   END
120200970526     C     FUNKEY        IFEQ      '03'
120300970526     C                   EXSR      F03GES
120400970526     C                   END
120500970526     C     FUNKEY        IFEQ      '05'
120600970526     C                   EXSR      F05GES
120700970526     C                   END
120800970526     C     FUNKEY        IFEQ      '06'
120900970526     C                   EXSR      F06GES
121000970526     C                   END
121100060926     C     FUNKEY        IFEQ      '07'
121200060926     C                   EXSR      F07GES
121300060926     C                   END
121301080828     C     FUNKEY        IFEQ      '08'
121302080828     C                   EXSR      F08GES
121303080828     C                   END
121400970526     C     FUNKEY        IFEQ      '12'
121500970526     C                   EXSR      F12GES
121600970526     C                   END
121700970529     C     FUNKEY        IFEQ      '13'
121800970529     C                   EXSR      F13GES
121900970529     C                   END
122000970526     C     FUNKEY        IFEQ      '23'
122100970526     C                   EXSR      F23GES
122200970526     C                   END
122300970526     C     FUNKEY        IFEQ      '24'
122400970526     C                   EXSR      F24GES
122500970526     C                   END
122600970526     C     FUNKEY        IFEQ      'RU'
122700970526     C                   EXSR      RUPGES
122800970526     C                   END
122900970526     C*
123000970526     C                   END
123100970526     C*
123200970526     C                   ENDSR
123300970526     C*------------------------------------------------------------------------*
123400970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
123500970526     C*------------------------------------------------------------------------*
123600970526     C     FUNVAR        BEGSR
123700970526     C*
123800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
123900970526     C     KEY           IFEQ      ENTER
124000970526     C                   MOVEL     *BLANKS       FUNKEY
124100970526     C                   END
124200970526     C     KEY           IFEQ      F1
124300970526     C                   MOVEL     '01'          FUNKEY
124400970526     C                   END
124500970526     C     KEY           IFEQ      F2
124600970526     C                   MOVEL     '02'          FUNKEY
124700970526     C                   END
124800970526     C     KEY           IFEQ      F3
124900970526     C                   MOVEL     '03'          FUNKEY
125000970526     C                   END
125100970526     C     KEY           IFEQ      F4
125200970526     C                   MOVEL     '04'          FUNKEY
125300970526     C                   END
125400970526     C     KEY           IFEQ      F5
125500970526     C                   MOVEL     '05'          FUNKEY
125600970526     C                   END
125700970526     C     KEY           IFEQ      F6
125800970526     C                   MOVEL     '06'          FUNKEY
125900970526     C                   END
126000970526     C     KEY           IFEQ      F7
126100970526     C                   MOVEL     '07'          FUNKEY
126200970526     C                   END
126300970526     C     KEY           IFEQ      F8
126400970526     C                   MOVEL     '08'          FUNKEY
126500970526     C                   END
126600970526     C     KEY           IFEQ      F9
126700970526     C                   MOVEL     '09'          FUNKEY
126800970526     C                   END
126900970526     C     KEY           IFEQ      F10
127000970526     C                   MOVEL     '10'          FUNKEY
127100970526     C                   END
127200970526     C     KEY           IFEQ      F11
127300970526     C                   MOVEL     '11'          FUNKEY
127400970526     C                   END
127500970526     C     KEY           IFEQ      F12
127600970526     C                   MOVEL     '12'          FUNKEY
127700970526     C                   END
127800970526     C     KEY           IFEQ      F13
127900970526     C                   MOVEL     '13'          FUNKEY
128000970526     C                   END
128100970526     C     KEY           IFEQ      F14
128200970526     C                   MOVEL     '14'          FUNKEY
128300970526     C                   END
128400970526     C     KEY           IFEQ      F15
128500970526     C                   MOVEL     '15'          FUNKEY
128600970526     C                   END
128700970526     C     KEY           IFEQ      F16
128800970526     C                   MOVEL     '16'          FUNKEY
128900970526     C                   END
129000970526     C     KEY           IFEQ      F17
129100970526     C                   MOVEL     '17'          FUNKEY
129200970526     C                   END
129300970526     C     KEY           IFEQ      F18
129400970526     C                   MOVEL     '18'          FUNKEY
129500970526     C                   END
129600970526     C     KEY           IFEQ      F19
129700970526     C                   MOVEL     '19'          FUNKEY
129800970526     C                   END
129900970526     C     KEY           IFEQ      F20
130000970526     C                   MOVEL     '20'          FUNKEY
130100970526     C                   END
130200970526     C     KEY           IFEQ      F21
130300970526     C                   MOVEL     '21'          FUNKEY
130400970526     C                   END
130500970526     C     KEY           IFEQ      F22
130600970526     C                   MOVEL     '22'          FUNKEY
130700970526     C                   END
130800970526     C     KEY           IFEQ      F23
130900970526     C                   MOVEL     '23'          FUNKEY
131000970526     C                   END
131100970526     C     KEY           IFEQ      F24
131200970526     C                   MOVEL     '24'          FUNKEY
131300970526     C                   END
131400970526     C     KEY           IFEQ      ROLLUP
131500970526     C                   MOVEL     'RU'          FUNKEY
131600970526     C                   END
131700970526     C     KEY           IFEQ      HELP
131800970526     C                   MOVEL     'HP'          FUNKEY
131900970526     C                   END
132000970526     C*
132100970526     C                   ENDSR
132200970526     C*------------------------------------------------------------------------*
132300970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
132400970526     C*------------------------------------------------------------------------*
132500970526     C     FUNGES        BEGSR
132600970526     C*
132700970526     C* PULISCO CAMPI DI LAVORO
132800970526     C                   Z-ADD     0             W                 5 0
132900970526     C*
133000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
133100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
133200970526     C     1             DO        24            I
133300970526     C                   MOVEL     FUD(I)        FUDRIG
133400970526     C*
133500970526     C* PULIZIA FUNZIONI PRECEDENTI
133600970526     C                   MOVEL     *BLANKS       FUV(I)
133700970526     C*
133800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
133900970526     C* (SALTA F23 E F24)
134000970526     C     FUDATT        IFEQ      'S'
134100970526     C     FUDFUN        ANDNE     '23'
134200970526     C     FUDFUN        ANDNE     '24'
134300970526     C                   ADD       1             W
134400970526     C                   MOVEL     FUDDES        FUV(W)
134500970526     C                   END
134600970526     C                   END                                                    DO WHILE
134700970526     C*
134800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
134900970526     C* ALTRIMENTI DISATTIVO F24
135000970526     C     1             DO        24            I
135100970526     C                   MOVEL     FUD(I)        FUDRIG
135200970526     C*
135300970526     C     FUDFUN        IFEQ      '24'
135400970526     C     W             IFGT      6
135500970526     C                   MOVEL     'S'           FUDATT
135600970526     C                   ELSE
135700970526     C                   MOVEL     *BLANKS       FUDATT
135800970526     C                   END                                                    W GT 8
135900970526     C                   MOVEL     FUDRIG        FUD(I)
136000970526     C                   END
136100970526     C                   END                                                    DO WHILE
136200970526     C*
136300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
136400970526     C     W             DIV       6             I
136500970526     C                   MVR                     Z
136600970526     C*
136700970526     C     Z             IFNE      0
136800970526     C     I             ADD       1             Z
136900970526     C                   ELSE
137000970526     C                   Z-ADD     I             Z
137100970526     C                   END
137200970526     C*
137300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
137400970526     C                   ADD       1             FUNGRU
137500970526     C     FUNGRU        IFGT      Z
137600970526     C     FUNGRU        ORLT      1
137700970526     C                   Z-ADD     1             FUNGRU
137800970526     C                   END
137900970526     C*
138000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
138100970526     C     FUNGRU        IFEQ      1
138200970526     C                   MOVEL     FUV(1)        FUN001
138300970526     C                   MOVEL     FUV(2)        FUN002
138400970526     C                   MOVEL     FUV(3)        FUN003
138500970526     C                   MOVEL     FUV(4)        FUN004
138600970526     C                   MOVEL     FUV(5)        FUN005
138700970526     C                   MOVEL     FUV(6)        FUN006
138800970526     C                   END
138900970526     C*
139000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
139100970526     C     FUNGRU        IFEQ      2
139200970526     C                   MOVEL     FUV(7)        FUN001
139300970526     C                   MOVEL     FUV(8)        FUN002
139400970526     C                   MOVEL     FUV(9)        FUN003
139500970526     C                   MOVEL     FUV(10)       FUN004
139600970526     C                   MOVEL     FUV(11)       FUN005
139700970526     C                   MOVEL     FUV(12)       FUN006
139800970526     C                   END
139900970526     C*
140000970526     C* TERZO GRUPPO DI 6 FUNZIONI
140100970526     C     FUNGRU        IFEQ      3
140200970526     C                   MOVEL     FUV(13)       FUN001
140300970526     C                   MOVEL     FUV(14)       FUN002
140400970526     C                   MOVEL     FUV(15)       FUN003
140500970526     C                   MOVEL     FUV(16)       FUN004
140600970526     C                   MOVEL     FUV(17)       FUN005
140700970526     C                   MOVEL     FUV(18)       FUN006
140800970526     C                   END
140900970526     C*
141000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
141100970526     C     FUNGRU        IFEQ      4
141200970526     C                   MOVEL     FUV(19)       FUN001
141300970526     C                   MOVEL     FUV(20)       FUN002
141400970526     C                   MOVEL     FUV(21)       FUN003
141500970526     C                   MOVEL     FUV(22)       FUN004
141600970526     C                   MOVEL     *BLANKS       FUN005
141700970526     C                   MOVEL     *BLANKS       FUN006
141800970526     C                   END
141900970526     C*
142000970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
142100970526     C                   MOVEL     *BLANKS       FUN007
142200970526     C                   MOVEL     *BLANKS       FUN008
142300970526     C*
142400970526     C     1             DO        24            I
142500970526     C                   MOVEL     FUD(I)        FUDRIG
142600970526     C     FUDFUN        IFEQ      '23'
142700970526     C     FUDATT        ANDEQ     'S'
142800970526     C                   MOVEL     FUDDES        FUN007
142900970526     C                   END
143000970526     C     FUDFUN        IFEQ      '24'
143100970526     C     FUDATT        ANDEQ     'S'
143200970526     C                   MOVEL     FUDDES        FUN008
143300970526     C                   END
143400970526     C                   END                                                    DO
143500970526     C*
143600970526     C                   ENDSR
143700970526     C*------------------------------------------------------------------------*
143800970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
143900970526     C*------------------------------------------------------------------------*
144000970526     C     OPZGES        BEGSR
144100970526     C*
144200970526     C* PULISCO CAMPI DI LAVORO
144300970526     C                   Z-ADD     0             W                 5 0
144400970526     C*
144500970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
144600970526     C* E CARICO TABELLA OPZIONI ATTIVE
144700970526     C     1             DO        24            I
144800970526     C                   MOVEL     OPD(I)        OPDRIG
144900970526     C*
145000970526     C* PULIZIA OPZIONI PRECEDENTI
145100970526     C                   MOVEL     *BLANKS       OPV(I)
145200970526     C*
145300970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
145400970526     C     OPDATT        IFEQ      'S'
145500970526     C                   ADD       1             W
145600970526     C                   MOVEL     OPDDES        OPV(W)
145700970526     C                   END
145800970526     C                   END                                                    DO WHILE
145900970526     C*
146000970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
146100970526     C* ALTRIMENTI DISATTIVO F23
146200970526     C     1             DO        24            I
146300970526     C                   MOVEL     FUD(I)        FUDRIG
146400970526     C*
146500970526     C     FUDFUN        IFEQ      '23'
146600970526     C     W             IFGT      8
146700970526     C                   MOVEL     'S'           FUDATT
146800970526     C                   ELSE
146900970526     C                   MOVEL     *BLANKS       FUDATT
147000970526     C                   END                                                    W GT 8
147100970526     C                   MOVEL     FUDRIG        FUD(I)
147200970526     C                   END
147300970526     C                   END                                                    DO WHILE
147400970526     C*
147500970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
147600970526     C     W             DIV       8             I
147700970526     C                   MVR                     Z                 5 0
147800970526     C*
147900970526     C     Z             IFNE      0
148000970526     C     I             ADD       1             Z
148100970526     C                   ELSE
148200970526     C                   Z-ADD     I             Z
148300970526     C                   END
148400970526     C*
148500970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
148600970526     C                   ADD       1             OPZGRU
148700970526     C     OPZGRU        IFGT      Z
148800970526     C     OPZGRU        ORLT      1
148900970526     C                   Z-ADD     1             OPZGRU
149000970526     C                   END
149100970526     C*
149200970526     C* PRIMO GRUPPO DI 8 OPZIONI
149300970526     C     OPZGRU        IFEQ      1
149400970526     C                   MOVEL     OPV(1)        OPZ001
149500970526     C                   MOVEL     OPV(2)        OPZ002
149600970526     C                   MOVEL     OPV(3)        OPZ003
149700970526     C                   MOVEL     OPV(4)        OPZ004
149800970526     C                   MOVEL     OPV(5)        OPZ005
149900970526     C                   MOVEL     OPV(6)        OPZ006
150000970526     C                   MOVEL     OPV(7)        OPZ007
150100970526     C                   MOVEL     OPV(8)        OPZ008
150200970526     C                   END
150300970526     C*
150400970526     C* SECONDO GRUPPO DI 8 OPZIONI
150500970526     C     OPZGRU        IFEQ      2
150600970526     C                   MOVEL     OPV(9)        OPZ001
150700970526     C                   MOVEL     OPV(10)       OPZ002
150800970526     C                   MOVEL     OPV(11)       OPZ003
150900970526     C                   MOVEL     OPV(12)       OPZ004
151000970526     C                   MOVEL     OPV(13)       OPZ005
151100970526     C                   MOVEL     OPV(14)       OPZ006
151200970526     C                   MOVEL     OPV(15)       OPZ007
151300970526     C                   MOVEL     OPV(16)       OPZ008
151400970526     C                   END
151500970526     C*
151600970526     C* TERZO GRUPPO DI 8 OPZIONI
151700970526     C     OPZGRU        IFEQ      3
151800970526     C                   MOVEL     OPV(17)       OPZ001
151900970526     C                   MOVEL     OPV(18)       OPZ002
152000970526     C                   MOVEL     OPV(19)       OPZ003
152100970526     C                   MOVEL     OPV(20)       OPZ004
152200970526     C                   MOVEL     OPV(21)       OPZ005
152300970526     C                   MOVEL     OPV(22)       OPZ006
152400970526     C                   MOVEL     OPV(23)       OPZ007
152500970526     C                   MOVEL     OPV(24)       OPZ008
152600970526     C                   END
152700970526     C*
152800970526     C                   ENDSR
152900970526     C*------------------------------------------------------------------------*
153000970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
153100970526     C*------------------------------------------------------------------------*
153200970526     C     OPZCON        BEGSR
153300970526     C*
153400970526     C* PULISCO CAMPI DI LAVORO
153500970526     C                   Z-ADD     0             I                 5 0
153600970526     C                   Z-ADD     0             W                 5 0
153700970526     C*
153800970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
153900970526     C     1             DO        24            I
154000970526     C                   MOVEL     OPD(I)        OPDRIG
154100970526     C*
154200970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
154300970526     C     OPZKEY        IFEQ      OPDOPZ
154400970526     C     OPZKEY        OREQ      OPDOP1
154500970526     C*
154600970526     C     OPDATT        IFEQ      'S'
154700970526     C                   MOVEL     OPDOPZ        OPZKEY
154800970526     C                   Z-ADD     I             W
154900970526     C                   END
155000970526     C                   END
155100970526     C*
155200970526     C                   END                                                    DO WHILE
155300970526     C*
155400970526     C* OPZIONE NON ATTIVA (ERRORE)
155500970526     C     W             IFEQ      0
155600970526     C*
155700970526     C* OPZIONE NON OK
155800970526     C                   Z-ADD     1             OPZOK
155900970526     C*
156000970526     C* EMISSIONE MESSAGGIO DI ERRORE
156100970526     C                   MOVEL     MSG(1)        DSMSMS
156200970526     C                   ELSE
156300970526     C*
156400970526     C* OPZIONE ATTIVA (PROCEDI ...)
156500970526     C*
156600970526     C     OPZKEY        IFEQ      ' 1'
156700970526     C                   EXSR      O01GES
156800970526     C                   END
156900970526     C     OPZKEY        IFEQ      ' 2'
157000060328     C                   EXSR      O02GES
157100970526     C                   END
157200970611     C     OPZKEY        IFEQ      ' 3'
157300970611     C                   EXSR      O03GES
157400970611     C                   END
157500970526     C     OPZKEY        IFEQ      ' 4'
157600970526     C                   EXSR      O04GES
157700970526     C                   END
157800970526     C     OPZKEY        IFEQ      ' 5'
157900970526     C                   EXSR      O05GES
158000970526     C                   END
158100970526     C                   END                                                    OPZ.NON ATTIVA
158200970526     C*
158300970526     C                   ENDSR
158400970526     C*------------------------------------------------------------------------*
158500970526     C* NOTE COSTRUZIONE SCHIERE
158600970526     C*------------------------------------------------------------------------*
158700970526     C*
158800970526     C* -MSG-MESSAGGI
158900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
159000970526     C* (NON TOCCARE MSG,1 E MSG,2)
159100970526     C*
159200970526     C* -CMD-COMANDI
159300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
159400970526     C*
159500970526     C* -OPD-OPZIONI
159600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
159700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
159800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
159900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
160000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
160100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
160200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
160300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
160400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
160500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
160600970526     C*
160700970526     C* -FUD-FUNZIONI
160800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
160900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
161000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
161100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
161200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
161300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
161400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
161500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
161600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
161700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
161800970526     O*------------------------------------------------------------------------*
161900970526** DEC - DECODIFICHE
162000970526    GESTIONE                                                     1
162100970530INTERROGAZIONE                                                   2
162200970530     SCELTA                                                      3
1623009705264
1624009705265
162500970526** MSG - MESSAGGI
162600060922TISIQ0R- Opzione non gestita                                                   1
162700060922TISIQ0R- Tasto funzionale non gestito                                          2
162800060922TISIQ0R- Data di riferimento errata
162900060922TISIQ0R- Network inesistente                                                   4
163000080520TISIQ0R- Nazione inesistente                                                   5
163100060922TISIQ0R- Accettato solo un parametro di inizio lista (P.O. o Naz/CAP)          6
163200080520TISIQ0R- Cap non valido per selezione con Nazione "*"                          7
163300060922TISIQ0R- 8                                                                     8
163400060922TISIQ0R- 9                                                                     9
163500060922TISIQ0R- 10                                                                   10
163600060922TISIQ0R- 11                                                                   11
163700060922TISIQ0R- 12                                                                   12
163800060922TISIQ0R- 13                                                                   13
163900060922TISIQ0R- 14                                                                   14
164000060922TISIQ0R- 15                                                                   15
164100060922TISIQ0R- 16                                                                   16
164200060922TISIQ0R- 17                                                                   17
164300060922TISIQ0R- 18                                                                   18
164400060922TISIQ0R- 19                                                                   19
164500060922TISIQ0R- 20                                                                   20
164600970526** OPD - OPZIONI
164700970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
164800060315 2=Modifica        -2 -     (S/N)=S                        opz. 02¬
164900060315 3=Duplica         -3 -     (S/N)=S                        opz. 03¬
165000060315 4=Annullamento    -4 -     (S/N)=S                        opz. 04¬
165100060315 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
165200970526                   -6 -     (S/N)=N                        opz. 06¬
165300060315                   -7 -     (S/N)=N                        opz. 07¬
165400970527                   -8 -     (S/N)=N                        opz. 08¬
165500970527                   -9 -     (S/N)=N                        opz. 09¬
165600970527                   -10-     (S/N)=N                        opz. 10¬
165700970529                   -11-     (S/N)=N                        opz. 11¬
165800970529                   -12-     (S/N)=N                        opz. 12¬
165900060922                   -13-     (S/N)=N                        opz. 13¬
166000970526                   -  -     (S/N)=N                        opz. 14¬
166100970526                   -  -     (S/N)=N                        opz. 15¬
166200970526                   -  -     (S/N)=N                        opz. 16¬
166300970526                   -  -     (S/N)=N                        opz. 17¬
166400970526                   -  -     (S/N)=N                        opz. 18¬
166500970526                   -  -     (S/N)=N                        opz. 19¬
166600970526                   -  -     (S/N)=N                        opz. 20¬
166700970526                   -  -     (S/N)=N                        opz. 21¬
166800970526                   -  -     (S/N)=N                        opz. 22¬
166900970526                   -  -     (S/N)=N                        opz. 23¬
167000970526                   -  -     (S/N)=N                        opz. 24¬
167100970526** FUD - FUNZIONI
167200970526                   -        (S/N)=N                        funz.01¬
167300970526                   -        (S/N)=N                        funz.02¬
167400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
167500970526                   -        (S/N)=N                        funz.04¬
167600970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
167700060315F06=Inserimento    -        (S/N)=S                        funz.06¬
167800060926F07=Cappario       -        (S/N)=S                        funz.07¬
167900080828F08=Contr.CAP/LOC  -        (S/N)=S                        funz.08¬
168000970527                   -        (S/N)=N                        funz.09¬
168100970527                   -        (S/N)=N                        funz.10¬
168200970526                   -        (S/N)=N                        funz.11¬
168300970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
168400060922                   -        (S/N)=N                        funz.13¬
168500970526                   -        (S/N)=N                        funz.14¬
168600970526                   -        (S/N)=N                        funz.15¬
168700970526                   -        (S/N)=N                        funz.16¬
168800970526                   -        (S/N)=N                        funz.17¬
168900970526                   -        (S/N)=N                        funz.18¬
169000970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
169100970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
169200970527                   -        (S/N)=N                        funz.21¬
169300970527                   -        (S/N)=N                        funz.22¬
169400970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
169500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
