000100911028     H DECEDIT('0,') DATEDIT(*DMY.)
000200060213     H* FNLG10R *----------------------------------------------------*
000300900418     H*                                                              *
000400900420     H*             - IMMISSIONE DISPOSIZIONI MITTENTE               *
000500940224     H*--------------------------------------------------------------*
000600940224     H*  05 RIPARTENZA                                               *
000601170505     H*  09 richiamato da FIDNA1R  Gestione Richieste Assistenza     *
000700900418     H*  19 IND. ERRORE GENERICO VIDEO                               *
000800900418     H*  21 IND. COMODO                                              *
000900940224     H*  22 CHAIN SU FLGNP/FLGNA                                     *
001000900418     H*  40 TEST SE GIACENZA GIA CARICATA O NO                       *
001100900419     H*  42 TEST SE GIACENZA RIAPERTA                                *
001200900419     H*  43 CHAIN FLBLP                                              *
001300940224     H*  50...73   ERRORI VIDEO                                      *
001400940224     H*  74 GIACENZA TRASMESSA                                       *
001500940224     H*  87 ABILITAZIONE CMD4 (OFF) --> STORICO                      *
001600940224     H*  90 ESECUZIONE DISPOSIZIONI                                  *
001700941005     H*  91 INDICATORE DI COMODO                                     *
001800960208     H*  94 ABILITAZIONE CMD14: CI SONO NOTE                         *
001900940224     H*  98 ABILITAZIONE RICHIESTA CONFERMA ANNULLAMENTO C/A         *
002000940224     H*  99 RICHIESTO ANNULLAMENTO C/A                               *
002100940224     H*                                                              *
002200900418     H*--------------------------------------------------------------*
002300940927     FFNLG10D   CF   E             WORKSTN
002400940927     F                                     SFILE(LG10S02:NRR)
002500941010     FFNBLP01L  IF   E           K DISK    USROPN
002600131014     FFIARBF2C  IF   E           K DISK    USROPN
002700950228     F                                     IGNORE(FNARBD00)
002800990929     F                                     IGNORE(FIARBT00)
002900950228     F                                     IGNORE(FNARBG00)
003000950228     F                                     IGNORE(FNARBM00)
003100950228     F                                     IGNORE(FNARBV00)
003200990917     F                                     IGNORE(FNARBP00)
003300131014     F                                     IGNORE(FNARBN00)
003400050316     Ftigcp02L  UF   E           K DISK    commit infds(gcpinf)
003500050215     Ftigap01L  IF   E           K DISK
003600050316     FtiGNP01L  UF A E           K DISK    commit
003700941003     FFNARB01L  IF   E           K DISK    USROPN
003800060213     FFiAR401L  IF   E           K DISK
003900990929     FFIAR601L  IF   E           K DISK    USROPN
004000051115     FFIAR901L  IF   E           K DISK    USROPN
004100900418     FTABEL00F  IF   E           K DISK
004200991027     FTNTBE01L  IF   E           K DISK
004300941003     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
004400900418     FCNACO00F  IF   E           K DISK
004500900418     FCNIND00F  IF   E           K DISK
004600040126     FFiar501L  IF   E           K DISK    USROPN
004700941003     D*--------------------------------------------------------------*
004800050316     d trul82ds      e ds
004900050316     d gcpinf          ds
005000050316     d  gcpnrr               397    400i 0
005100020225     d* STATO GIACENZA
005200941005     D STA             S             25    DIM(3) CTDATA PERRCD(1)
005300090520     d* tipi servizio
005400090520     D C5e             S              1    DIM(10)
005500090520     D d5e             S             15    DIM(10)
005600020225     d* CODICI PART.GIAC.
005700000809     D C7Q             S              2    DIM(100)
005800020225     d* DESCR. PART.GIAC
005900941003     D D7Q             S             89    DIM(100)
006000020225     d* CODICI DIVISA
006100941006     D CCV             S              3    DIM(100)
006200020225     d* FLAG X ACCETT. DEC.
006300941006     D FDC             S              1    DIM(100)
006400020225     d* FLAG X ACCETT. DEC.
006500991120     D DEC             S              2  0 DIM(100)
006600020225     d* MESSAGGI DI ERRORE
006700170511     D ERR             S             70    DIM(31) CTDATA PERRCD(1)
006800900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
006900900418     D  TCU                  398    697
007000900418     D                                     DIM(50)
007100900418     D  KCU                  698    847P 0
007200900418     D                                     DIM(50)
007300900418     D                                     PACKEVEN
007400900418     D TCUDS           DS
007500900418     D  F3                     3      3
007600900418     D  F4                     4      4
007700900418     D  F56                    5      6
007800941003     D WLBDA8          DS
007900941003     D  G02DAT                 1      8  0
008000941003     D  G02INV                 9     16  0
008100941003     D  G02ERR                17     17
008200941003     D  G02TGI                18     22  0
008300950213     D                 DS
008400950213     D  GGMAX                 38     39  0
008500950213     D  MESSAG                 1     70
008501170414      * - DS Parametri per ricerca Programmi chiamanti
008502170414     D DSStack         ds
008503170414     D  £Stack                       10    Dim(100)
008504170414
008600991027     D YEURDS        E DS                  EXTNAME(YEURCODS)
008700991027     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
008800960123     D FNLG82        E DS                  EXTNAME(FNLG82DS)
008900941003     D DSTB69        E DS                  EXTNAME(DS7PQRS)
009000941006     D DSCV          E DS
009100020729     D OG143         E DS
009200991027     D DGED          E DS
009300010907     D DGEI          E DS
009400941006     D DS7Q          E DS
009500900508     D DS3A          E DS
009600911017     D DS2D          E DS
009700911017     D DS2E          E DS
009800950213     D DS2G          E DS
009900011217     D TRUL21        E DS                  EXTNAME(TRUL21DS)
010000040126     D dar5gen       E DS
010100900418     D CNCR80        E DS
010200051007     D dgcpflr       E DS
010300060504     d fnlg12ds      e ds
010400060504     d savkpjbu        s                   like(kpjbu)
010500071010     d §kpjbu          s                   like(kpjbu)
010600060504     d precedenti      s              1    inz
010700900418     D KPJBA         E DS
010800071010     D* DATI X RICHIAMO PGM INTERR. BOLLE PARTENZE
010900071010     D PARAM0          DS
011000071010     D  PA0AAS                 1      4  0
011100071010     D  PA0LNP                 5      7  0
011200071010     D  PA0NRS                 8      9  0
011300071010     D  PA0NSP                10     16  0
011400071010     D  PA0FLG                18     18
011500071010     D  PA0SIM                44     46  0
011600071010     D  PA0F03                48     48
011700071010     D PARAM1          DS
011800071010     D  PA1AAS                14     17  0
011900071010     D  PA1LNP                18     20  0
012000071010     D  PA1NRS                21     22  0
012100071010     D  PA1NSP                23     29  0
012200071010     D  PA1F03                30     30
012300071010     D  PA1FLG                31     31
012400941003     I*--------------------------------------------------------------*
012500941003     IAZORG01L  AA
012600941003     I                             P    4    5 0ORGFIL
012700941003     I                                  6    6  ORGFVA
012800971112     I                                 11   11  ORGFL1
012900941003     I                                 14   33  ORGDES
013000020729     I                               3776 3800  ORGDE3
013100941003     C*--------------------------------------------------------------*
013200941003     C                   SETOFF                                       509028
013300960206     C                   SETOFF                                       04
013400900418     C                   EXSR      PULVID
013500900619     C*
013600050316     C     KGIA          CHAIN(e)  tigcp02L                           40
013700050316     c                   if        %error
013800070521     C     KGIA          CHAIN(n)  tigcp02L
013900050316     c                   exsr      geslck
014000050318     C     KGIA          CHAIN(e)  tigcp02L                           40
014100050316     c                   endif
014200900420     C* SE  NON E ANNULLATA
014300900419     C* CONTROLLO SE SI PUO MODIFICARE O DECODIFICO STATO
014400900418     C     *IN40         IFEQ      '0'
014500050808     C* disposizioni forzate Bypasso controllo manutenzione
014600050808     c                   if        kcdaz <>'XG83'
014700020225     c* partenza
014800941003     C     D82GES        IFEQ      'P'
014900060403     C     D82fspe       IFEQ      *blank
015000941003     C     GCPFAS        IFLT      20
015100050222     C     GCPFAS        ORGe      35
015200911108     C*POSSO METTERE DISP. + VOLTTE
015300911023     C                   SETON                                        5090
015400900613     C                   END
015500060403     C                   endif
015600911014     C                   ELSE
015700020225     c* arrivo
015800050318     C     GCPFAS        IFlt      20
015900050318     C     GCPFAS        orgt      31
016000060808     C     GCPasg        orne      *blank
016100050318     C*    GCPFAS        IFNE      20
016200050318     C*    GCPFAS        ORne      31
016300020225     c* messaggi + protezione campi
016400911023     C                   SETON                                        5090
016500911014     C                   END
016600911014     C                   END
016700050825      *se sono edp e sto forzando la fase non può essere diversa da 26 o 36
016800050810     c                   else
016900050825     c                   if        gcpfas <> 26 and gcpfas <> 36
017000050810     c                   seton                                        5090
017100050810     c                   endif
017200050808      * XG83
017300050808     c                   endif
017400941003     C* 50 ON GIAC. NON MANUTENZIONABILE
017500941003     C     *IN50         IFEQ      '1'
017600941003     C                   SETON                                        87
017700941003     C                   MOVEL     STA(3)        VMSG
017800941003     C                   END
017900900420     C*
018000050222     C     gcpriap       COMP      'R'                                    42
018100911014     C** NON TROVATA
018200941007     C                   ELSE
018300920110     C                   SETON                                        50
018400900418     C                   END
018500911014     C*
018600900418     C* PRENDO DATI DELLA BOLLA
018700020225     c* partenza
018800941003     C     D82GES        IFEQ      'P'
018900941003     C     KBOL          CHAIN     FNBLP01L                           43
019000941003     C     BLPATB        IFEQ      '*'
019100941003     C     *IN43         OREQ      '1'
019200941003     C                   SETON                                        285090
019300941003     C                   MOVEL     ERR(1)        $MSG
019400900418     C                   ELSE
019500900418     C                   EXSR      BLPVID
019600900418     C                   END
019700911014     C                   ELSE
019800050215     C     Karb          CHAIN     FNARB01L                           43
019900941003     C     ARBATB        IFEQ      '*'
020000941003     C     *IN43         OREQ      '1'
020100941003     C                   SETON                                        285090
020200941003     C                   MOVEL     ERR(1)        $MSG
020300911014     C                   ELSE
020400941104     C     ARBDCM        IFNE      0
020500941003     C                   SETON                                        285090
020600941003     C                   MOVEL     ERR(2)        $MSG
020700941003     C                   ELSE
020800911014     C                   EXSR      ARBVID
020900911014     C                   END
021000941003     C                   END
021100911014     C                   END
021200941003     C*
021300050613      * l'errore altrimenti forzo il comando di conferma per proseguire
021400050613     c                   if        d82nodsp <> *blank
021500050613     c   50              clear                   d82nodsp
021600050613     c                   endif
021700941003     C   50              EXCEPT    RILGCP
021800900418     C* SE GIACENZA GIA CARICATA CARICO I DATI DELLA GIAC. A VIDEO
021900900418     C     *IN40         IFEQ      '0'
022000900418     C                   EXSR      GCPVID
022100900418     C                   END
022200941005     C                   SETOFF                                       99
022300900418     C*
022400900418     C     EMFG02        TAG
022500040126      * rekord GEN riferimenti ubicazione in magazzino giacenze
022600040126     C                   clear                   dar5gen
022700040126     C                   move      'GEN'         trcar5
022800040126     c     kar5          chain     fiar501l
022900040126     c                   if        %found
023000040126     C                   MOVEL     ar5uni        dar5gen
023100040126     c                   endif
023200040126     c                   movel     §ar5zng       vubima
023300040126
023400050513      *emetto il video se il parametro della ds lo permette
023500050601     c                   if        d82nodsp = *blank or *in04
023600941003     C                   EXFMT     LG10D01
023700050601     c                   endif
023800071010      *interrogazione bolle
023900071010     C     *INKG         IFEQ      '1'
023901170414      *
023902170414      * verifico se programma richiamato dal PGM interrogazione  Bolle
023904170414     c                   setoff                                       30
023905170414     c     'FNLR36R   '  lookup    £Stack                                 30
023906170414     c  n30'FNLS05R   '  lookup    £Stack                                 30
023907170414     c  n30'FNLRU6R1  '  lookup    £Stack                                 30
023908170414
023909170414      * se trovato non richiamo il programma di interrogazione bolle
023910170511     c                   if        *in30
023911170511     c                   seton                                        28
023912170511     c                   movel     Err(31)       $msg
023913170512     C                   GOTO      EMFG02
023914170511     c                   else
024000071010     c                   movel     kpjbu         §kpjbu
024100071010      * partenza
024200071010     c                   if        d82ges = 'P'
024300071010     C                   CLEAR                   PARAM0
024400071010     c                   Z-ADD     BLPAAS        PA0AAS
024500071010     c                   Z-ADD     BLPLNP        PA0LNP
024600071010     c                   Z-ADD     BLPNRS        PA0NRS
024700071010     c                   Z-ADD     BLPNSP        PA0NSP
024800071010     c
024900071010     C                   MOVEL     PARAM0        KPJBU
025000071010     C                   CALL      'FNLS05R'
025100071010     C                   PARM                    KPJBA
025200071010     C                   MOVEL     KPJBU         PARAM0
025300071010      *arrivo
025400071010     c                   else
025500071010     c                   z-add     arbaas        pa1aas
025600071010     c                   z-add     arblnp        pa1lnp
025700071010     c                   z-add     arbnrs        pa1nrs
025800071010     c                   z-add     arbnsp        pa1nsp
025900071010     c                   movel     ' '           pa1f03
026000071010     c                   movel     '1'           pa1flg
026100071010     c                   movel     param1        kpjbu
026200071010     c                   call      'FNLR36R'
026300071010     c                   parm                    kpjba
026400071010     c                   movel     kpjbu         param1
026500071010     C                   endif
026600071010      *
026700071010     c                   movel     §kpjbu        kpjbu
026800071010     C                   END
026801170414     C                   Endif
026900071010     C*
027000000809      *CONTROLLO SE SUPERATO IL LIMITE DI APERTURE EMETTO VIDEO
027100000809      *SEGNALAZIONE
027200000809     C     D82GES        IFEQ      'P'
027300000809     C     EXGIAC        ANDNE     GCPNGC
027400000809     C                   Z-ADD     GCPNGC        EXGIAC            7 0
027500050215     C     KGIA2         SETGT     tigcp02L
027600050316     C     KGIA2         READPE(e) tigcp02L                               97
027700050316     c                   if        %error
027800070521     C     KGIA2         READPE(n) tigcp02L
027900050316     c                   exsr      geslck
028000050318     C     KGIA2         READPE(e) tigcp02L                               97
028100050316     c                   endif
028200000809     C     *IN97         IFEQ      '0'
028300031007     C     GCPFRG        ANDGE     15
028400000809     C                   EXFMT     LG10D07
028500050722     C     *INKL         CABEQ     '1'           FINE
028600050722     C                   ENDIF
028700000905     C                   Z-ADD     0             §FRG
028800050316     C     KGIA          CHAIN(e)  tigcp02L                           97
028900050316     c                   if        %error
029000070521     C     KGIA          CHAIN(n)  tigcp02L
029100050316     c                   exsr      geslck
029200050318     C     KGIA          CHAIN(e)  tigcp02L                           97
029300050316     c                   endif
029400000809     C                   END
029500900418     C*                    ------------
029600960208     C                   SETOFF                                       04
029700941003     C     $MSG          IFNE      ERR(1)
029800941003     C     $MSG          ANDNE     ERR(2)
029900941003     C                   SETOFF                                       28
030000941003     C                   END
030100941006     C                   SETOFF                                       515266
030200960206     C                   SETOFF                                       744748
030300970625     C                   SETOFF                                       44
030400050722     c* se si sono immesse delle note e non si conferma la giacenza rollback
030500050726     c                   if        *inkl and f6note = '1' and gescomm = 'S'
030600050722     c                   seton                                        28
030700050725     c                   eval      $msg = 'Att.ne le note immesse verranno per-
030800050725     c                             se'
030900050722     c                   clear                   f6note
031000050722     C                   GOTO      EMFG02
031100050722     c                   end
031200050722     c*
031300050722     c                   if        *inkl
031400050726     c                   if        gescomm = 'S'
031500050722     c                   rolbk
031600050726     c                   end
031601170413     c                   eval      D82f12 = '1'
031602170413     c                   movel     fnlg82        kpjbu
031700050722     C                   goto      fine
031800050722     C                   ENDIF
031900911108     C* SE NON MANUTENZIONABILE ERRORE
032000911108     C   50              GOTO      EMFG02
032100960123     C*  VISUALIZZA DESTINATARIO ORIGINALE
032200960123     C     *INKM         IFEQ      '1'
032300960123     C                   MOVEL     'N'           WRITOR            1
032400960123     C     WRITOR        DOWEQ     'N'
032500960123     C                   EXSR      GESD06
032600960123     C                   END
032700960123     C                   END
032800060504      * immissione disposizioni emetto videata aperture precedenti
032900060504
033000060504     c                   if        d82nodsp = *blank and precedenti = *blank
033100060504     c                   movel     kpjbu         savkpjbu
033200060504     c                   clear                   fnlg12ds
033300060504     c                   z-add     gcpagc        i12agc
033400060504     c                   z-add     gcpngc        i12ngc
033500060504     c                   z-add     gcpfgc        i12fgc
033600060504     c                   move      d82motr       i12motr
033700060504     c                   movel(p)  fnlg12ds      kpjbu
033800060504     c                   call      'FNLG12R'
033900060504     c                   parm                    kpjba
034000060504     c                   movel     kpjbu         fnlg12ds
034100060802      * se esistono incongruenze e il controllo è stato avviato tramite F6
034200060802      * accendo indicatore di errore per riemettere il video
034300060802     c                   if        o12ok <> '0' and *inkf
034400060802     c                   seton                                        19
034500060802     c                   endif
034600060504     c                   movel     savkpjbu      kpjbu
034700060504     c                   endif
034800060504     c                   move      'X'           precedenti
034900060504      *
035000900418     C* CONTROLLO VIDEO
035100941007     C     *INKN         IFEQ      '0'
035200941007     C                   EXSR      CTRVID
035300990913     C                   MOVEL     VASG          WSVAS1            1
035400990913     C                   MOVEL     VPSG          WSVPS1            1
035500050513      * se non ho la gestione del video e c'è un errore forzo la pulizia
035600050513      * del flag di controllo in modo da emettere il video per segnalare
035700050513      * l'errore altrimenti forzo il comando di conferma per proseguire
035800050513     c                   if        d82nodsp <> *blank
035900050513     c   19              clear                   d82nodsp
036000050513     c  n19              move      *on           *inkf
036100050513     c                   endif
036200050513
036300941007     C   19              GOTO      EMFG02
036400911017     C* CMD14 X RICHIAMO  NOTE
036500941007     C                   ELSE
036600941006     C                   EXSR      GESNOT
036700941006     C                   GOTO      EMFG02
036800941006     C                   END
036900960206     C* CONTROLLO SE DEVO RICHIEDERE FORZATURA
037000960206     C     WMODSP        IFEQ      'S'
037100960206     C     D82GES        ANDEQ     'P'
037200960206     C     §7Q1SF        IFNE      *BLANKS
037300960206     C     EXPORT        ANDEQ     'F'
037400960206     C     §7Q1SA        ORNE      *BLANKS
037500960206     C     EXPORT        ANDEQ     'A'
037600960206     C                   SETON                                        04
037700960206     C                   END
037800960206     C                   END
037900960208     C*
038000960226     C  NKF
038100960226     CANNKP
038200960226     CANNKA              GOTO      EMFG02
038300960226     C   28
038400960226     CANNKA              GOTO      EMFG02
038500960123     C*
038600911023     C* AGGIORNA ARCHIVI   SE HO CONFERMATO CMD06
038700911023     C*                    O SE IN ARRIVO VOGLIO ESEGUIRE DISPOS.
038800960226     C     *INKF         IFEQ      '1'
038900960207     C     *INKP         OREQ      '1'
039000960208     C     D82GES        ANDEQ     'A'
039100960208     C     *INKA         OREQ      '1'
039200941005     C* Controllo se devo gestire forzatura da apertura
039300941005     C     WFORZA        IFEQ      'S'
039400941010     C                   MOVE      *IN90         WIND90            1
039500941010     C                   SETON                                        0490
039600941010     C                   EXFMT     LG10D01
039700941010     C                   MOVE      WIND90        *IN90
039800941005     C     *INKA         IFEQ      '0'
039900941005     C                   GOTO      EMFG02
040000941005     C                   END
040100941005     C                   END
040200941005     C*
040300911021     C                   EXSR      GCPWTR
040400900418     C*
040500911018     C* SE SONO IN PARTENZA E CLIENTE è 8888 RICH. PGM X BOLLE DI RECUPERO
040600911123     C* E NON è MANUALE
040700911212     C* OPPURE SE DISP. DI RIENTRO CALCOLO SPESE GIAC. SENZA FARE BOLLA
040800911212     C* VERRANNO CONSIDERATE IN ARRIVO QUANDO FACCIO BOLLA DI RIENTRO
040900920311     C* OPPURE NUOVO PORTO è FRANCO RICHIAMO SEMPRE PERCHè NON SO COME
041000920311     C* E' IL MITTENTE :POSSO QUINDI ADDEBITARE ADESSO SE IL MITT. è CODIF.
041100921104     C* MITTENTE
041200941003     C     D82GES        IFEQ      'P'
041300941003     C     BLPCCM        IFNE      *ZEROS
041400921105     C                   MOVE      BLPCCM        §8899             4 0
041500921104     C                   ELSE
041600921105     C                   MOVE      BLPKSC        §8899
041700921104     C                   END
041800941003     C*
041900060403     C                   IF        d82fspe <> 'S'
042000941003     C     §8899         IFEQ      8888
042100921104     C     §8899         OREQ      9999
042200921104     C     GCPPSG        ANDEQ     'M'
042300911212     C     VFRI          OREQ      'R'
042400920311     C     VNPC          OREQ      'F'
042500941005     C                   MOVEL     'D'           D82TRI
042600050316     c                   move      'S'           d82comm
042700941003     C                   MOVEL     FNLG82        KPJBU
042800941005     C                   CALL      'FNLG14R'
042900911109     C                   PARM                    KPJBA
043000911018     C                   END
043100060403     c                   endif
043200990913     C* SE SONO USCITA CON F12 RIPRISTINO I DATI PRECEDENTI
043300990913     C                   MOVEL     KPJBU         FNLG82
043400990913     C     D82F12        IFEQ      '1'
043500990913     C                   EXSR      RIPGIA
043600990913     C                   END
043700941003     C*
043800941003     C                   ELSE
043900911023     C* IN ARRIVO SE VOGLIO ESEG. DISPOS.
044000941003     C   KP              DO
044100050316     c                   move      'S'           d82comm
044200941005     C                   MOVEL     FNLG82        KPJBU
044300941005     C                   CALL      'FNLG20R'
044400911109     C                   PARM                    KPJBA
044500911021     C                   END
044600911023     C                   END
044700941003     C                   END
044800900418     C*
044900900418     C     FINE          TAG
045000900418     C                   SETON                                        LR
045100960123     C*-----------------------------------------------------*
045200960123     C*  Gestione della sesta videata
045300960123     C*-----------------------------------------------------*
045400960123     C     GESD06        BEGSR
045500960123     C*
045600960123     C                   EXFMT     LG10D06
045700960123     C*                    ------------
045800960123     C*  Ritorno
045900960123     C     *INKL         IFEQ      '1'
046000960123     C                   MOVEL     'S'           WRITOR
046100960123     C                   END
046200960123     C*
046300960123     C                   ENDSR
046400900418     C/EJECT
046500900418     C*---------------------------------------------------------------*
046600941003     C*   OPERAZIONI INIZIALI                                         *
046700900418     C*---------------------------------------------------------------*
046800941003     C     *INZSR        BEGSR
046900941003     C*
047000900418     C     *ENTRY        PLIST
047100900418     C                   PARM                    KPJBA
047200941003     C                   MOVEL     KPJBU         FNLG82
047300050316     c                   move      d82comm       gescomm           1
047400941006     C                   CLEAR                   DSTB69
047500941003     C                   MOVE      D82AGC        VAGC
047600941003     C                   MOVE      D82FGC        VFGC
047700941003     C                   MOVE      D82NGC        VNGC
047800960123     C                   MOVE      D82AGC        V6CAGC
047900960123     C                   MOVE      D82FGC        V6CFGC
048000960123     C                   MOVE      D82NGC        V6CNGC
048100060403     c                   if        d82fspe = 'S'
048200060403     c                   seton                                        34
048300060403     c                   endif
048301170414      * reperisco i programmi che hanno richiamato questo pgm
048302170414     c                   Call      'XRTVSTKR'
048303170414     C                   Parm                    PStack         1000
048304170414     C                   Parm                    PStartPoint       3 0
048305170414     C                   Parm                    PEntries          3 0
048306170414     c                   Movel     Pstack        DSstack
048400941003     C*
048500900418     C                   Z-ADD     1             CODUT
048600900418     C                   CALL      'X§PARUT'
048700900418     C                   PARM                    UTEDSE
048800900418     C     CODUT         CABEQ     -1            FINE
048900900418     C                   MOVEL     RAGUT         RSUT             20
049000900418     C                   MOVEL     RAGUT         RSUT2            25
049100941003     C                   MOVEL     '* FNLG10'    VPROGR
049200900423     C                   MOVE      'R *'         VPROGR
049300900524     C*-                                                   *
049400900524     C                   MOVEL     REC80         CNCR80
049500900418     C*--- RICERCA CAPOCONTI------------------------------------------*
049600941006     C                   DO        50            X
049700900418     C                   MOVE      TCU(X)        TCUDS
049800941003     C     F56           IFEQ      'CG'
049900941003     C     F4            ANDEQ     '1'
050000941003     C     F3            ANDEQ     '0'
050100941003     C                   Z-ADD     KCU(X)        KCI               4 0
050200941003     C                   END
050300900418     C                   END
050400900418     C*---------------------------------------------------------------*
050500900418     C*  KLIST
050600900418     C     KGIA          KLIST
050700941005     C                   KFLD                    VAGC              4 0
050800900420     C                   KFLD                    VFGC
050900900420     C                   KFLD                    VNGC
051000911014     C                   KFLD                    §FRG
051100000809      *
051200000809     C     KGIA2         KLIST
051300000809     C                   KFLD                    VAGC              4 0
051400000809     C                   KFLD                    VFGC
051500000809     C                   KFLD                    VNGC
051600900418     C*
051700911017     C     KNOT          KLIST
051800911017     C                   KFLD                    VAGC
051900911017     C                   KFLD                    VFGC
052000911017     C                   KFLD                    VNGC
052100911017     C                   KFLD                    §FRG
052200911017     C                   KFLD                    §FAS
052300911017     C*
052400911017     C     KNOR          KLIST
052500911017     C                   KFLD                    VAGC
052600911017     C                   KFLD                    VFGC
052700911017     C                   KFLD                    VNGC
052800911017     C                   KFLD                    §FRG
052900911017     C                   KFLD                    §FAS
053000911017     C                   KFLD                    VNPR
053100911017     C*
053200900420     C     KBOL          KLIST
053300900420     C                   KFLD                    GCPAAS
053400900420     C                   KFLD                    GCPLNP
053500900420     C                   KFLD                    GCPNRS
053600900420     C                   KFLD                    GCPNSP
053700050215     C*
053800050215     C     Karb          KLIST
053900050215     C                   KFLD                    GCPAASa
054000050215     C                   KFLD                    GCPLNPa
054100050215     C                   KFLD                    GCPNRSa
054200050215     C                   KFLD                    GCPNSPa
054300990929     C     KAR6          KLIST
054400990929     C                   KFLD                    GCPAAS
054500990929     C                   KFLD                    GCPLNP
054600990929     C                   KFLD                    GCPNRS
054700990929     C                   KFLD                    GCPNSP
054800990929     C                   KFLD                    KTRC
054900050215     C     KAR6a         KLIST
055000050215     C                   KFLD                    GCPAASa
055100050215     C                   KFLD                    GCPLNPa
055200050215     C                   KFLD                    GCPNRSa
055300050215     C                   KFLD                    GCPNSPa
055400050215     C                   KFLD                    KTRC
055500941003     C*
055600941003     C     KBOL4         KLIST
055700941003     C                   KFLD                    GCPAAS
055800941003     C                   KFLD                    GCPLNP
055900941003     C                   KFLD                    GCPNRS
056000941003     C                   KFLD                    GCPNSP
056100060213     C                   KFLD                    ar4TRC
056200050215     C*
056300050215     C     KBOL4a        KLIST
056400050215     C                   KFLD                    GCPAASa
056500050215     C                   KFLD                    GCPLNPa
056600050215     C                   KFLD                    GCPNRSa
056700050215     C                   KFLD                    GCPNSPa
056800060213     C                   KFLD                    ar4TRC
056900991027     C*
057000991027     C     KTBE          KLIST
057100991027     C                   KFLD                    TBECOD
057200991027     C                   KFLD                    TBEKE1
057300991027     C                   KFLD                    TBEKE2
057400900420     C*
057500900418     C     KTAB          KLIST
057600900418     C                   KFLD                    CODUT
057700900418     C                   KFLD                    TBLCOD
057800900418     C                   KFLD                    TBLKEY
057900941003     C*
058000941003     C     KTBL2         KLIST
058100941003     C                   KFLD                    KKUT
058200941003     C                   KFLD                    KCOD
058300941003     C                   Z-ADD     1             KKUT
058400900418     C*
058500900418     C     KACO          KLIST
058600900418     C                   KFLD                    CODUT
058700900418     C                   KFLD                    KCI
058800900419     C                   KFLD                    VSCM
058900040126     C*
059000040126     C     Kar5          KLIST
059100040126     C                   KFLD                    GCPAAS
059200040126     C                   KFLD                    GCPLNP
059300040126     C                   KFLD                    GCPNRS
059400040126     C                   KFLD                    GCPNSP
059500040126     C                   KFLD                    trcar5
059600900418     C*
059700040126     C     *LIKE         DEFINE    ar5trd        trcar5
059800011217     C     *LIKE         DEFINE    GCPCAS        WEXVNC
059900900420     C     *LIKE         DEFINE    GCPDDM        AMGDDM
060000900427     C     *LIKE         DEFINE    GCPDDM        AMGDTG
060100900427     C     *LIKE         DEFINE    GCPDDM        AMGUDT
060200990913     C     *LIKE         DEFINE    GCPDIS        WSVDIS
060300990913     C     *LIKE         DEFINE    GCPPSG        WSVPSG
060400990913     C     *LIKE         DEFINE    GCPASG        WSVASG
060500990913     C     *LIKE         DEFINE    GCPNPC        WSVNPC
060600060331     C     *LIKE         DEFINE    GCPchiaut     WSV1cchim
060700990913     C     *LIKE         DEFINE    GCPVCS        WSVVCS
060800990913     C     *LIKE         DEFINE    GCPCAS        WSVCAS
060900990913     C     *LIKE         DEFINE    GCPVCA        WSVVCA
061000990913     C     *LIKE         DEFINE    GCPPPC        WSVPPC
061100990913     C     *LIKE         DEFINE    GCPAPC        WSVAPC
061200990913     C     *LIKE         DEFINE    GCPDDM        WSVDDM
061300990913     C     *LIKE         DEFINE    GCPDED        WSVDED
061400050216     C     *LIKE         DEFINE    GCPDEDa       WSVDEDa
061500990913     C     *LIKE         DEFINE    GCPTFT        WSVTFT
061600990913     C     *LIKE         DEFINE    GCPSGS        WSVSGS
061700990913     C     *LIKE         DEFINE    GCPSGR        WSVSGR
061800990913     C     *LIKE         DEFINE    GCPSGD        WSVSGD
061900990913     C     *LIKE         DEFINE    GCPSGA        WSVSGA
062000990913     C     *LIKE         DEFINE    GCPDIV        WSVDIV
062100990913     C     *LIKE         DEFINE    GCPDLD        WSVDLD
062200990913     C     *LIKE         DEFINE    GCPFAS        WSVFAS
062300170307     C     *LIKE         DEFINE    GCPFlr        WSVFlr
062400990913     C     *LIKE         DEFINE    GCPFRG        §FRG
062500911018     C     *LIKE         DEFINE    GCPFAS        §FAS
062600990929     C     *LIKE         DEFINE    AR6TRC        KTRC
062700911017     C     *LIKE         DEFINE    TBLCOD        §COD
062800911017     C     *LIKE         DEFINE    TBLKEY        §KEY
062900941003     C     *LIKE         DEFINE    TBLKUT        KKUT
063000941003     C     *LIKE         DEFINE    TBLCOD        KCOD
063100941010     C*
063200911017     C                   MOVEL     *BLANKS       §DES             60
063300911014     C*
063400911014     C                   MOVE      *ZEROS        §FRG
063500911017     C                   MOVE      20            §FAS
063600900427     C*
063700941005     C                   TIME                    WHHDAT           14 0
063800941003     C                   MOVE      WHHDAT        G02DAT
063900941003     C                   Z-ADD     0             G02INV
064000941003     C                   MOVE      ' '           G02ERR
064100941003     C                   CALL      'XSRDA8'
064200941003     C                   PARM                    WLBDA8
064300941003     C                   MOVE      G02INV        AMGUDT
064400911014     C*
064500911014     C* VERIFICO SE SIAMO IN PARTENZA O ARRIVO
064600941003     C     D82GES        IFEQ      'A'
064700911014     C* APRO ARCHIVI BOLLE E GIACENZE IN PARTENZA O ARRIVO
064800941003     C                   OPEN      FNARB01L
064900990929     C                   OPEN      FIAR601L
065000040126     C                   OPEN      FIAR501L
065100051115     C                   OPEN      FIAR901L
065200050531     C                   SETON                                        80
065300941003     C                   MOVEL     ' IN ARR'     V1CGES
065400941003     C                   MOVE      'IVO '        V1CGES
065500941003     C                   ELSE
065600050531     C                   SETOFF                                       80
065700941003     C                   OPEN      FNBLP01L
065800131014     C                   OPEN      FIARBF2C
065900990929     C                   OPEN      FIAR601L
066000040126     C                   OPEN      FIAR501L
066100051115     C                   OPEN      Fiar901L
066200941003     C                   MOVEL     'IN PART'     V1CGES
066300941003     C                   MOVE      'ENZA'        V1CGES
066400941003     C                   END
066401170505      * verifico se richiamato dalla gestione RA
066402170505     c     'FIDNA1R   '  lookup    £Stack                                 09
066500991027     C*
066600991027     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
066700991027     C                   MOVEL     'GED'         TBECOD
066800991027     C                   MOVEL     *BLANKS       TBEKE1
066900991027     C                   MOVEL     *BLANKS       TBEKE2
067000991027     C                   MOVEL     '1'           TBEKE1
067100991027     C     KTBE          CHAIN     TNTBE01L                           21
067200991027     C  N21              MOVEL     TBEUNI        DGED
067300941003     C* Carico tabella 7Q
067400941006     C                   Z-ADD     0             X
067500941003     C                   MOVEL     '7Q'          KCOD
067600941003     C     KTBL2         CHAIN     TABEL                              31
067700941003     C     *IN31         DOWEQ     '0'
067800941003     C     X             ANDLT     50
067900941003     C     TBLFLG        IFEQ      ' '
068000941003     C                   ADD       1             X
068100941003     C                   MOVEL     TBLKEY        C7Q(X)
068200950102     C                   MOVEL     TBLUNI        D7Q(X)
068300941003     C                   END
068400941003     C     KTBL2         READE     TABEL                                  31
068500941003     C                   END
068600090520     C* Carico tabella 5e
068700090520     C                   Z-ADD     0             X
068800090520     C                   MOVEL     '5E'          KCOD
068900090520     C     KTBL2         CHAIN     TABEL                              31
069000090520     C     *IN31         DOWEQ     '0'
069100090520     C     X             ANDLT     10
069200090520     C     TBLFLG        IFEQ      ' '
069300090520     C                   ADD       1             X
069400090520     C                   MOVEL     TBLKEY        c5e(X)
069500090520     C                   MOVEL     TBLUNI        d5e(X)
069600090520     C                   END
069700090520     C     KTBL2         READE     TABEL                                  31
069800090520     C                   END
069900941006     C* Carico tabella cambi/divise
070000941006     C                   Z-ADD     0             X                 3 0
070100941006     C                   MOVEL     'CV'          KCOD
070200941006     C     KTBL2         CHAIN     TABEL                              31
070300941006     C     *IN31         DOWEQ     '0'
070400941028     C     X             ANDLT     100
070500941006     C     TBLFLG        IFEQ      ' '
070600941006     C                   ADD       1             X
070700941006     C                   MOVEL     TBLKEY        CCV(X)
070800941006     C                   MOVEL     TBLUNI        DSCV
070900941006     C                   MOVEL     §CVFDC        FDC(X)
071000991120     C                   Z-ADD     §CVDEC        DEC(X)
071100991027     C                   END
071200941006     C     KTBL2         READE     TABEL                                  31
071300941006     C                   END
071400950213     C* Carico tabella 2G
071500950213     C                   Z-ADD     0             X
071600950213     C                   MOVEL     '2G'          TBLCOD
071700950213     C                   MOVEL     *BLANKS       TBLKEY
071800950213     C                   MOVEL     '1'           TBLKEY
071900950213     C                   CLEAR                   DS2G
072000950213     C     KTAB          CHAIN     TABEL                              31
072100950213     C     *IN31         IFEQ      '0'
072200950213     C     TBLFLG        ANDEQ     ' '
072300950213     C                   MOVEL     TBLUNI        DS2G
072400950213     C                   END
072500941003     C*
072600900418     C                   ENDSR
072700900418     C*---------------------------------------------------------------*
072800900418     C*   P U L V I D                                                 *
072900900418     C*---------------------------------------------------------------*
073000900418     C     PULVID        BEGSR
073100960123     C                   MOVEL     *ZEROS        V6CAAS
073200960123     C                   MOVEL     *ZEROS        V6CLNP
073300960123     C                   MOVEL     *ZEROS        V6CNRS
073400960123     C                   MOVEL     *ZEROS        V6CNSP
073500960123     C                   MOVEL     *BLANK        V6CRSD
073600960123     C                   MOVEL     *BLANK        V6CDE2
073700960123     C                   MOVEL     *BLANK        V6CIND
073800960123     C                   MOVEL     *BLANK        V6CLOD
073900960123     C                   MOVEL     *BLANK        V6CPRD
074000960123     C                   MOVEL     *ZEROS        V6CCAD
074100960123     C                   MOVEL     *BLANKS       V6CNZD
074200900419     C                   MOVEL     *ZEROS        VNRS
074300900419     C                   MOVEL     *ZEROS        VNSP
074400911023     C                   MOVEL     *BLANK        VMSG
074500900418     C                   MOVEL     *ZEROS        VDSP
074600900418     C                   MOVEL     *BLANK        VTBL
074700900418     C                   MOVEL     *BLANK        DTBL
074800900508     C                   MOVEL     *ZEROS        VLNP
074900900418     C                   MOVEL     *ZEROS        VLNA
075000900418     C                   MOVEL     *BLANK        DLNA
075100900418     C                   MOVEL     *BLANK        VNAS
075200900418     C                   Z-ADD     *ZEROS        VIFT
075300991119     C                   MOVEL     *BLANKS       V1CDFA
075400900418     C                   Z-ADD     *ZEROS        VNCL
075500900418     C                   Z-ADD     *ZEROS        VPKG
075600900418     C                   Z-ADD     *ZEROS        VASS
075700911017     C                   Z-ADD     *ZEROS        VVOL
075800900418     C                   MOVEL     *BLANK        VRSD
075900900418     C                   MOVEL     *BLANK        VIND
076000900418     C                   MOVEL     *BLANK        VLOD
076100900418     C                   MOVEL     *BLANK        VPRD
076200941003     C                   MOVEL     *BLANKS       VCAD
076300900419     C                   MOVEL     *ZEROS        VSCM
076400900418     C                   MOVEL     *BLANK        VRSM
076500900418     C                   MOVEL     *BLANK        VINM
076600900418     C                   MOVEL     *BLANK        VLOM
076700900418     C                   MOVEL     *BLANK        VPRM
076800941003     C                   MOVEL     *BLANKS       VCAM
076900900418     C                   MOVEL     *ZEROS        VDTG
077000900419     C                   MOVEL     *ZEROS        VDIG
077100900419     C                   MOVEL     *BLANKS       DFGC
077200971112     C                   MOVEL     *BLANKS       GIAEST
077300911017     C                   MOVEL     *ZEROS        VCMC
077400911017     C                   MOVEL     *BLANKS       DCMC
077500911017     C                   MOVEL     *BLANKS       VDMC
077600911219     C                   MOVEL     *BLANKS       VNPC
077700911219     C                   MOVEL     *BLANKS       DNPC
077800060331     C                   MOVEL     *BLANKS       V1cchim
077900941003     C                   MOVEL     *BLANKS       V1CNDV
078000941003     C                   MOVEL     *BLANKS       V1CDIV
078100941003     C                   MOVEL     *BLANKS       V1CRD2
078200941005     C                   MOVEL     *BLANKS       V1CNZD
078300941005     C                   MOVEL     *BLANKS       V1CNZM
078400941003     C                   MOVEL     *BLANKS       V1CRMA
078500941003     C                   MOVEL     *BLANKS       V1CMSD
078600941003     C                   MOVEL     *BLANKS       V1CMSP
078700941003     C                   MOVEL     *BLANKS       V1DMSP
078800941003     C                   MOVEL     *BLANKS       V1CGGA
078900941003     C                   MOVEL     *BLANKS       V1CRPG
079000911017     C                   MOVEL     *ZEROS        VDDM
079100941003     C                   MOVEL     *ZEROS        V1CDED
079200911030     C* SE PASSATO RIENTRO LO PROPONGO A VIDEO
079300941005     C     D82DIS        IFEQ      'R'
079400941005     C                   MOVEL     D82DIS        VFRI
079500911030     C                   ELSE
079600900508     C                   MOVEL     *BLANKS       VFRI
079700911030     C                   END
079800900418     C                   ENDSR
079900900418     C*---------------------------------------------------------------*
080000900418     C*   B L P V I D                                                 *
080100900418     C*---------------------------------------------------------------*
080200900418     C     BLPVID        BEGSR
080300941003     C                   MOVE      BLPMGS        G02INV
080400941003     C                   MOVEL     BLPAAS        G02INV
080500941003     C                   Z-ADD     0             G02DAT
080600941003     C                   MOVEL     '3'           G02ERR
080700941003     C                   CALL      'XSRDA8'
080800941003     C                   PARM                    WLBDA8
080900941003     C                   MOVEL     G02DAT        VDSP
081000900524     C* TIPO SPEDIZIONE
081100900524     C                   MOVEL     *BLANKS       DTSP
081200090520     C                   Z-ADD     1             X
081300090520     C     BLPtsp        LOOKUP    C5e(X)                                 32
081400090520     C     *IN32         IFEQ      '1'
081500090520     C                   MOVEl     D5e(X)        Dtsp
081600900524     C                   END
081700900418     C* TIPO BOLLA
081800941003     C                   MOVEL     BLPCBO        VTBL
081900900418     C                   MOVEL     '3A'          TBLCOD
082000900418     C                   MOVEL     *BLANK        TBLKEY
082100900418     C                   MOVEL     VTBL          TBLKEY
082200900418     C     KTAB          CHAIN     TABEL                              21
082300900418     C   21              MOVEL     *ALL'?'       DTBL
082400900508     C  N21              MOVEL     TBLUNI        DS3A
082500020225     c* porto della bolla
082600960206     C  N21              MOVEL     §3ATB1        EXPORT            1
082700900509     C  N21              MOVEL     §3ADES        DTBL
082800900418     C* LINEA ARRIVO
082900900418     C                   MOVEL     BLPLNA        VLNA
083000941003     C                   MOVEL     BLPLNA        ORGFIL
083100941003     C     ORGFIL        CHAIN     AZORG01L                           21
083200900418     C  N21ORGFVA        COMP      '*'                                    21
083300900418     C   21              MOVEL     *ALL'?'       DLNA
083400900418     C  N21              MOVEL     ORGDES        DLNA
083500900418     C*
083600900418     C                   MOVEL     BLPNAS        VNAS
083700900418     C                   Z-ADD     BLPNCL        VNCL
083800941003     C                   Z-ADD     BLPPKF        VPKG
083900941003     C                   Z-ADD     BLPVLF        VVOL
084000941003     C* Reperisco importi fatture e C/Assegno
084100990929     C     EXPORT        IFEQ      'F'
084200990929     C                   MOVEL     '1'           KTRC
084300990929     C                   CLEAR                   AR6IFT
084400990929     C     KAR6          CHAIN     FIAR601L                           31
084500990929     C                   Z-ADD     AR6IFT        VIFT
084600991119     C                   MOVEL     AR6DIV        V1CDFA
084700990929     C                   END
084800051115     C     KBOL          CHAIN     Fiar901L                           31
084900060313      * tenta chain con bolla in arrivo
085000060313      * Se la bolla era dirottata non sentiva più il contrassegno in fase di
085100060313      * inserimento delle disposizioni in partenza
085200060313     c                   if        *in31
085300060313     C     Karb          CHAIN     Fiar901L                           31
085400060313     c                   endif
085500051115     C                   Z-ADD     ar9CAS        VASS
085600051115     C                   MOVEL     ar9VCA        V1CDIV
085700960123     C* Controllo se esiste estensione descrizione destinatario
085800060213     C                   MOVEL     'D'           ar4TRC
085900060213     C                   MOVEL     *BLANKS       ar4NOT
086000060213     C     KBOL4         CHAIN     Fiar401L                           31
086100960123     C* Controllo se esiste destinatario finale
086200960123     C                   SETOFF                                       13
086300960123     C                   CLEAR                   FNLSN0
086400960123     C                   Z-ADD     BLPAAS        DN0AAS
086500960123     C                   Z-ADD     BLPLNP        DN0LNP
086600960123     C                   Z-ADD     BLPNRS        DN0NRS
086700960123     C                   Z-ADD     BLPNSP        DN0NSP
086800960123     C                   MOVEL     *BLANKS       DN0TRC
086900960123     C                   MOVEL     KPJBU         WSVKPJ          256
087000960123     C                   MOVEL     FNLSN0        KPJBU
087100960123     C                   CALL      'FNLSN0R'
087200960123     C                   PARM                    KPJBA
087300960123     C                   MOVEL     KPJBU         FNLSN0
087400960123     C                   MOVEL     WSVKPJ        KPJBU
087500960123     C* Destinatario
087600960123     C     DN0TRV        IFEQ      'S'
087700960123     C                   SETON                                        13
087800960123     C                   MOVEL     DN0RSD        VRSD
087900960123     C                   MOVEL     DN0RS2        V1CRD2
088000960123     C                   MOVEL     DN0IND        VIND
088100960123     C                   MOVEL     DN0LOD        VLOD
088200960123     C                   MOVEL     DN0PRD        VPRD
088300960123     C                   MOVEL     DN0CAD        VCAD
088400960123     C                   MOVEL     DN0NZD        V1CNZD
088500960123     C                   MOVEL     BLPRSD        V6CRSD
088600060213     C                   MOVEL     ar4NOT        V6CDE2
088700960123     C                   MOVEL     BLPIND        V6CIND
088800960123     C                   MOVEL     BLPLOD        V6CLOD
088900960123     C                   MOVEL     BLPPRD        V6CPRD
089000960123     C                   MOVEL     BLPCAD        V6CCAD
089100960123     C                   MOVEL     BLPNZD        V6CNZD
089200960123     C                   ELSE
089300900418     C* DESTINATARIO
089400900418     C                   MOVEL     BLPRSD        VRSD
089500060213     C                   MOVEL     ar4NOT        V1CRD2
089600900418     C                   MOVEL     BLPIND        VIND
089700900418     C                   MOVEL     BLPLOD        VLOD
089800900418     C                   MOVEL     BLPPRD        VPRD
089900900418     C                   MOVEL     BLPCAD        VCAD
090000941003     C                   MOVEL     BLPNZD        V1CNZD
090100960123     C                   END
090200900418     C* MITTENTE
090300900419     C                   MOVEL     BLPCCM        VSCM
090400900418     C                   MOVEL     BLPRSM        VRSM
090500900418     C                   MOVEL     BLPINM        VINM
090600900418     C                   MOVEL     BLPLOM        VLOM
090700900418     C                   MOVEL     BLPPRM        VPRM
090800900418     C                   MOVEL     BLPCAM        VCAM
090900941003     C                   MOVEL     BLPRMN        VRIF
091000941005     C                   MOVEL     BLPRMA        V1CRMA
091100941003     C                   MOVEL     BLPNZM        V1CNZM
091200031022     C                   if        BLPGGA = *blank
091300031022     c                   eval      BLPGGA = 'GR'
091400031022     c                   end
091500941003     C                   MOVEL     BLPGGA        V1CGGA
091600941003     C* Carico messaggio particolarità
091700960207     C                   CLEAR                   DS7Q
091800941003     C                   MOVEL     'N'           WFORZA            1
091900941006     C                   SETOFF                                       0506
092000941003     C     BLPGGA        IFNE      *BLANKS
092100941003     C                   Z-ADD     1             X
092200941003     C     BLPGGA        LOOKUP    C7Q(X)                                 32
092300941003     C     *IN32         IFEQ      '1'
092400941003     C                   MOVE      D7Q(X)        DS7Q
092500941003     C     §7Q3MF        IFEQ      'D'
092600941003     C     §7Q3MS        ANDNE     *BLANKS
092700941003     C     §7Q3MQ        IFEQ      'P'
092800941003     C                   SETON                                        05
092900941003     C                   MOVEL     STA(1)        V1DMSP
093000950118     C                   MOVEL     §7Q3MS        V1CMSP
093100941003     C                   ELSE
093200941003     C                   SETON                                        06
093300941003     C                   MOVEL     STA(2)        V1DMSP
093400950118     C                   MOVEL     §7Q3MS        V1CMSD
093500941003     C                   END
093600941003     C                   END
093700941003     C                   END
093800941003     C                   END
093900941003     C*
094000900418     C                   ENDSR
094100911014     C*---------------------------------------------------------------*
094200911014     C*   A R B V I D                                                 *
094300911014     C*---------------------------------------------------------------*
094400911014     C     ARBVID        BEGSR
094500941003     C                   MOVE      ARBMGS        G02INV
094600941003     C                   MOVEL     ARBAAS        G02INV
094700941003     C                   Z-ADD     0             G02DAT
094800941003     C                   MOVEL     '3'           G02ERR
094900941003     C                   CALL      'XSRDA8'
095000941003     C                   PARM                    WLBDA8
095100941003     C                   MOVEL     G02DAT        VDSP
095200941003     C* TIPO SPEDIZIONE
095300941003     C                   MOVEL     *BLANKS       DTSP
095400090520     C                   Z-ADD     1             X
095500090520     C     arbtsp        LOOKUP    C5e(X)                                 32
095600090520     C     *IN32         IFEQ      '1'
095700090520     C                   MOVEl     D5e(X)        Dtsp
095800090520     C                   END
095900911014     C* TIPO BOLLA
096000941003     C                   MOVEL     ARBCBO        VTBL
096100911014     C                   MOVEL     '3A'          TBLCOD
096200911014     C                   MOVEL     *BLANK        TBLKEY
096300911014     C                   MOVEL     VTBL          TBLKEY
096400911014     C     KTAB          CHAIN     TABEL                              21
096500911014     C   21              MOVEL     *ALL'?'       DTBL
096600911014     C  N21              MOVEL     TBLUNI        DS3A
096700020225     c* porto della bolla
096800960208     C  N21              MOVEL     §3ATB1        EXPORT            1
096900911014     C  N21              MOVEL     §3ADES        DTBL
097000911014     C* LINEA ARRIVO
097100911014     C                   MOVEL     ARBLNA        VLNA
097200911014     C     VLNA          CHAIN     AZORG01L                           21
097300911014     C  N21ORGFVA        COMP      '*'                                    21
097400911014     C   21              MOVEL     *ALL'?'       DLNA
097500911014     C  N21              MOVEL     ORGDES        DLNA
097600911014     C*
097700911014     C                   MOVEL     ARBNAS        VNAS
097800911014     C                   Z-ADD     ARBNCL        VNCL
097900941003     C                   Z-ADD     ARBVLF        VVOL
098000941005     C                   Z-ADD     ARBPKF        VPKG
098100941003     C* Reperisco importi fatture e C/Assegno
098200990929     C     EXPORT        IFEQ      'A'
098300990929     C                   MOVEL     '1'           KTRC
098400990929     C                   CLEAR                   AR6IFT
098500050215     C     KAR6a         CHAIN     FIAR601L                           31
098600990929     C                   Z-ADD     AR6IFT        VIFT
098700991119     C                   MOVEL     AR6DIV        V1CDFA
098800990929     C                   END
098900051115     C     Karb          CHAIN     FIAR901L                           31
099000941003     C                   Z-ADD     AR9CAS        VASS
099100941003     C                   MOVEL     AR9VCA        V1CDIV
099200911014     C* DESTINATARIO
099300911014     C                   MOVEL     ARBRSD        VRSD
099400941003     C* Controllo se esiste estensione descrizione destinatario
099500060213     C                   MOVEL     'D'           ar4TRC
099600941003     C                   MOVEL     *BLANKS       AR4NOT
099700060213     C     KBOL4a        CHAIN     FiAR401L                           31
099800941003     C                   MOVEL     AR4NOT        V1CRD2
099900911014     C                   MOVEL     ARBIND        VIND
100000911014     C                   MOVEL     ARBLOD        VLOD
100100911014     C                   MOVEL     ARBPRD        VPRD
100200911014     C                   MOVEL     ARBCAD        VCAD
100300941003     C                   MOVEL     ARBNZD        V1CNZD
100400911014     C* MITTENTE
100500911014     C     ARBCCM        IFNE      *ZEROS
100600911014     C                   MOVEL     ARBCCM        VSCM
100700911014     C                   ELSE
100800911014     C                   MOVEL     ARBKSC        VSCM
100900911014     C                   END
101000941003     C*                    MOVELARBRMA    VRMA
101100911014     C                   MOVEL     ARBRSM        VRSM
101200911014     C                   MOVEL     ARBINM        VINM
101300911014     C                   MOVEL     ARBLOM        VLOM
101400911014     C                   MOVEL     ARBPRM        VPRM
101500911014     C                   MOVEL     ARBCAM        VCAM
101600941003     C                   MOVEL     ARBNZM        V1CNZM
101700941003     C                   MOVEL     ARBRMN        VRIF
101800031022     C                   if        ARBGGA = *blank
101900031022     c                   eval      ARBGGA = 'GR'
102000031022     c                   end
102100941003     C                   MOVEL     ARBGGA        V1CGGA
102200941003     C* Carico messaggio particolarità
102300960207     C                   CLEAR                   DS7Q
102400941003     C                   MOVEL     'N'           WFORZA            1
102500941010     C                   SETOFF                                       0506
102600941003     C     ARBGGA        IFNE      *BLANKS
102700941003     C                   Z-ADD     1             X
102800941003     C     ARBGGA        LOOKUP    C7Q(X)                                 32
102900941003     C     *IN32         IFEQ      '1'
103000941003     C                   MOVE      D7Q(X)        DS7Q
103100941003     C     §7Q3MF        IFEQ      'D'
103200941003     C     §7Q3MS        ANDNE     *BLANKS
103300941003     C     §7Q3MQ        IFEQ      'P'
103400941003     C                   SETON                                        05
103500941003     C                   MOVEL     STA(1)        V1DMSP
103600941003     C                   MOVEL     §7Q2MQ        V1CMSP
103700941003     C                   MOVEL     §7Q2MC        WFORZA
103800941003     C                   ELSE
103900941003     C                   SETON                                        06
104000941003     C                   MOVEL     STA(2)        V1DMSP
104100941003     C                   MOVEL     §7Q2MQ        V1CMSD
104200941003     C                   END
104300941003     C                   END
104400941003     C                   END
104500941003     C                   END
104600941003     C*
104700911014     C                   ENDSR
104800900418     C*---------------------------------------------------------------*
104900900418     C*   G C P V I D                                                 *
105000900418     C*---------------------------------------------------------------*
105100900418     C     GCPVID        BEGSR
105200050216     c                   if        d82ges = 'P'
105300960123     C                   MOVE      GCPLNP        V6CLNP
105400960123     C                   MOVE      GCPAAS        V6CAAS
105500960123     C                   MOVE      GCPNRS        V6CNRS
105600960123     C                   MOVE      GCPNSP        V6CNSP
105700900524     C                   MOVE      GCPLNP        VLNP
105800900524     C                   MOVE      GCPAAS        VAAS
105900900524     C                   MOVE      GCPNRS        VNRS
106000900524     C                   MOVE      GCPNSP        VNSP
106100050216     c                   else
106200050216     C                   MOVE      GCPLNPa       V6CLNP
106300050216     C                   MOVE      GCPAASa       V6CAAS
106400050216     C                   MOVE      GCPNRSa       V6CNRS
106500050216     C                   MOVE      GCPNSPa       V6CNSP
106600050216     C                   MOVE      GCPLNPa       VLNP
106700050216     C                   MOVE      GCPAASa       VAAS
106800050216     C                   MOVE      GCPNRSa       VNRS
106900050216     C                   MOVE      GCPNSPa       VNSP
107000050216     c                   endif
107100050216
107200051007     C                   MOVEL     GCPflr        dgcpflr
107300051007     c                   movel     D§GCPAPP      v1capp
107400051007     C                   MOVE      GCPSCM        VSCM
107500900504     C                   MOVE      VSCM          §QUATR            4
107600900504     C     §QUATR        IFEQ      *ALL'8'
107700900504     C     §QUATR        OREQ      *ALL'9'
107800900418     C* MITTENTE DA APPOGGIO
107900050216     C     KBOL          CHAIN     tiGAP01L                           21
108000900418     C     *IN21         IFEQ      '0'
108100900418     C                   MOVEL     GAPRSM        VRSM
108200900418     C                   MOVEL     GAPINM        VINM
108300900418     C                   MOVEL     GAPLOM        VLOM
108400900418     C                   MOVEL     GAPPRM        VPRM
108500900418     C                   MOVEL     GAPCAM        VCAM
108600941003     C                   MOVEL     GAPNZM        V1CNZM
108700900418     C                   END
108800900419     C                   ELSE
108900911014     C* SOLO SE SONO IN PARTENZA
109000941005     C     D82GES        IFEQ      'P'
109100900419     C     KACO          CHAIN     CNACO000                           21
109200900419     C  N21ACOFLG        COMP      '*'                                    21
109300900419     C  N21ACOABL        COMP      '*'                                    21
109400900419     C  N21              MOVEL     ACORAG        VRSM
109500900419     C   21              MOVE      *ALL'?'       VRSM
109600900419     C     KACO          CHAIN     CNIND000                           21
109700900419     C   21              MOVE      *ALL'?'       VINM
109800900419     C   21              MOVE      *ALL'?'       VLOM
109900900419     C   21              MOVE      *ALL'?'       VPRM
110000900419     C  N21              MOVEL     INDVIA        VINM
110100900419     C  N21              MOVEL     INDCIT        VLOM
110200900419     C  N21              MOVEL     INDPRV        VPRM
110300050223     C  N21              MOVEL     INDCAe        VCAM
110400941003     C  N21              MOVEL     INDSTA        V1CNZM
110500900419     C                   END
110600911014     C                   END
110700900418     C* CODICE MOTIVO GIACENZA
110800900418     C                   MOVE      GCPCMC        VCMC
110900900418     C                   MOVEL     '2A'          TBLCOD
111000900418     C                   MOVEL     *BLANK        TBLKEY
111100900418     C                   MOVEL     VCMC          TBLKEY
111200900418     C     KTAB          CHAIN     TABEL                              21
111300900418     C   21              MOVEL     *ALL'?'       DCMC
111400900418     C  N21              MOVEL     TBLUNI        DCMC
111500911219     C* ULTERIORE MOTIVO
111600911219     C                   Z-ADD     10            §FAS
111700050215     C     KNOT          CHAIN     tignp01L                           21
111800911219     C  N21              MOVEL     GNPDMC        VDMC
111900911219     C   21              MOVEL     *BLANKS       VDMC
112000911219     C                   Z-ADD     20            §FAS
112100900419     C* DATA APERTURA GIACENZA
112200941005     C                   MOVE      GCPMGC        G02INV
112300941003     C                   MOVEL     GCPAAS        G02INV
112400941003     C                   Z-ADD     0             G02DAT
112500941003     C                   MOVEL     '3'           G02ERR
112600941003     C                   CALL      'XSRDA8'
112700941003     C                   PARM                    WLBDA8
112800941010     C                   MOVE      G02INV        AMGDTG
112900941010     C                   MOVEL     G02DAT        VDTG
113000900419     C                   MOVE      GCPNGC        VNGC
113100990628     C                   MOVE      GCPNGC        V6CNGC
113200900419     C* FIL. CHE HA APERTO LA GIACENZA
113300900419     C     GCPFGC        IFNE      *ZEROS
113400900419     C                   MOVE      GCPFGC        VFGC
113500960123     C                   MOVE      GCPFGC        V6CFGC
113600900419     C     VFGC          CHAIN     AZORG01L                           21
113700900419     C  N21ORGFVA        COMP      '*'                                    21
113800900419     C   21              MOVEL     *ALL'?'       DFGC
113900971112     C   21              MOVEL     *BLANKS       GIAEST
114000900419     C  N21              MOVEL     ORGDES        DFGC
114100020729     C                   clear                   OG143
114200020729     C  n21              MOVEL     ORGDE3        OG143
114300020729     C                   MOVEL     *blank        GIAEST            1
114400020729     C  N21§OgNTW        ifeq      'FED'
114500020729     C     §OgNTW        oreq      'EEX'
114600020729     C     §OgNTW        oreq      'EUP'
114700020729     C                   MOVEL     'E'           GIAEST
114800020729     c                   end
114900900419     C                   END
115000900419     C*
115100900419     C* DATA ULTIMA RIAPERTURA
115200050222     c                   if        gcpriap = 'R'
115300941003     C                   Z-ADD     GCPDUR        G02INV
115400941003     C                   Z-ADD     0             G02DAT
115500941003     C                   MOVEL     '3'           G02ERR
115600941003     C                   CALL      'XSRDA8'
115700941003     C                   PARM                    WLBDA8
115800941003     C                   MOVEL     G02DAT        VDIG
115900950530     C                   MOVE      G02INV        AMGDUR            8 0
116000050222     c                   else
116100050222     C                   z-add     0             VDIG
116200050222     c                   end
116300900420     C* DATA DISPOSIZIONI MITTENTE
116400941006     C     GCPDDM        IFNE      0
116500941006     C                   Z-ADD     GCPDDM        G02INV
116600941003     C                   Z-ADD     0             G02DAT
116700941003     C                   MOVEL     '3'           G02ERR
116800941003     C                   CALL      'XSRDA8'
116900941003     C                   PARM                    WLBDA8
117000941003     C                   MOVEL     G02DAT        VDDM
117100941003     C                   ELSE
117200941003     C                   MOVE      WHHDAT        VDDM
117300900525     C                   END
117400911030     C* DATA COMUNICAZIONE INTERNA = DATA DIST. PAD. SE IN ARRIVO
117500911030     C     GCPDXD        IFGT      *ZEROS
117600941003     C                   Z-ADD     GCPDXD        G02INV
117700941003     C                   Z-ADD     0             G02DAT
117800941003     C                   MOVEL     '3'           G02ERR
117900941003     C                   CALL      'XSRDA8'
118000941003     C                   PARM                    WLBDA8
118100941003     C                   MOVEL     G02DAT        VDXD
118200911030     C                   END
118300050410     C* DATA ESEguibilità DISPOSIZIONI
118400941003     C     GCPDED        IFGT      *ZEROS
118500941003     C                   Z-ADD     GCPDED        G02INV
118600941003     C                   Z-ADD     0             G02DAT
118700941003     C                   MOVEL     '3'           G02ERR
118800941003     C                   CALL      'XSRDA8'
118900941003     C                   PARM                    WLBDA8
119000941003     C                   MOVEL     G02DAT        V1CDED
119100941003     C                   END
119200911030     C* NR. E DATA COMUNICAZ.
119300911030     C                   MOVE      GCPNRC        VNRC
119400911030     C* LUOGO DEPOSITO MERCE
119500911030     C                   MOVE      GCPCCR        VCCR
119600911030     C                   MOVEL     *BLANKS       DCCR
119700911030     C     GCPCCR        IFNE      *ZEROS
119800911030     C                   MOVEL     '2B'          TBLCOD
119900911030     C                   MOVEL     *BLANK        TBLKEY
120000911030     C                   MOVEL     VCCR          TBLKEY
120100911030     C     KTAB          CHAIN     TABEL                              21
120200911030     C   21              MOVEL     *ALL'?'       DCCR
120300911030     C  N21              MOVEL     TBLUNI        DCCR
120400911030     C* SE NON LO TROVO NELLA TABELLA  LO CERCO IN ORGANIGARMMA
120500911030     C     *IN21         IFEQ      '1'
120600941003     C                   MOVE      VCCR          ORGFIL
120700941003     C     ORGFIL        CHAIN     AZORG01L                           21
120800911030     C  N21ORGFVA        COMP      '*'                                    21
120900911030     C   21              MOVEL     *ALL'?'       DCCR
121000911030     C  N21              MOVEL     *BLANK        DCCR
121100911030     C  N21              MOVEL     ORGDES        DCCR
121200911030     C                   END
121300911030     C                   END
121400911121      *
121500941010      *  Controllo se è obbligatorio eseguire una determinata disposiz.
121600941010     C                   EXSR      CTRGGA
121700941005     C     GCPDIS        IFNE      *BLANK
121800941005     C                   MOVEL     GCPDIS        VFRI
121900911219     C                   MOVEL     '2D'          TBLCOD
122000911219     C                   MOVEL     *BLANK        TBLKEY
122100911219     C                   MOVEL     VFRI          TBLKEY
122200911219     C     KTAB          CHAIN     TABEL                              21
122300911219     C   21              MOVEL     *ALL'?'       DFRI
122400911219     C  N21              MOVEL     TBLUNI        DS2D
122500911219     C  N21              MOVEL     §2DDES        DFRI
122600911121     C                   ELSE
122700941005     C     D82DIS        IFEQ      'R'
122800941005     C                   MOVEL     D82DIS        VFRI
122900911219     C                   MOVEL     '2D'          TBLCOD
123000911219     C                   MOVEL     *BLANK        TBLKEY
123100911219     C                   MOVEL     VFRI          TBLKEY
123200911219     C     KTAB          CHAIN     TABEL                              21
123300911219     C   21              MOVEL     *ALL'?'       DFRI
123400911219     C  N21              MOVEL     TBLUNI        DS2D
123500911219     C  N21              MOVEL     §2DDES        DFRI
123600911129     C                   ELSE
123700911121     C                   MOVEL     *BLANK        VFRI
123800911129     C                   END
123900911121     C                   END
124000911121     C     GCPASG        IFNE      *BLANK
124100911121     C                   MOVEL     GCPASG        VASG
124200911121     C                   ELSE
124300960209     C* PROPONGO LA PARTICOLARITA NO SPESE
124400960206     C     §7Q1SF        IFEQ      'N'
124500960206     C     EXPORT        ANDEQ     'F'
124600960206     C     §7Q1SA        OREQ      'N'
124700960206     C     EXPORT        ANDEQ     'A'
124800960206     C                   MOVEL     'N'           VASG
124900960207     C                   ELSE
125000960209     C* ALTRIMENTI SEMPRE SI SPESE
125100960206     C                   MOVEL     'S'           VASG
125200911121     C                   END
125300960207     C                   END
125400911121     C     GCPPSG        IFNE      *BLANK
125500911121     C                   MOVEL     GCPPSG        VPSG
125600911219     C                   MOVEL     '2E'          TBLCOD
125700911219     C                   MOVEL     *BLANK        TBLKEY
125800911219     C                   MOVEL     VPSG          TBLKEY
125900911219     C     KTAB          CHAIN     TABEL                              21
126000911219     C   21              MOVEL     *ALL'?'       DPSG
126100911219     C  N21              MOVEL     TBLUNI        DS2E
126200911219     C  N21              MOVEL     §2EDES        DPSG
126300911121     C                   ELSE
126400911121     C                   MOVEL     *BLANKS       VPSG
126500960209     C*  SE SI SPESE GIACENZA :
126600960209     C*  PROPONGO LO STANDARD  FRANCHI  -> MITT.
126700960209     C*                        ASSEGNATI-> DEST.
126800960209     C*                        RIENTRI  -> MITT.
126900960209     C*  O QUANTO PREVISTO DALLA PARTICOLARITA
127000960209     C     VASG          IFEQ      'S'
127100020225      * franco
127200960209     C     EXPORT        IFEQ      'F'
127300020225      * internaz.
127400971112     C     GIAEST        OREQ      'E'
127500020225      * standard
127600960209     C                   MOVEL     'M'           VPSG
127700020225      * dest.
127800960209     C     §7Q1SF        IFEQ      'D'
127900020225      * rientri
128000960209     C     VFRI          ANDNE     'R'
128100960208     C                   MOVEL     'D'           VPSG
128200960209     C                   END
128300911121     C                   END
128400020225      * assegnato
128500960209     C     EXPORT        IFEQ      'A'
128600020225      * internaz.
128700971112     C     GIAEST        ANDNE     'E'
128800020225      * standard
128900960209     C                   MOVEL     'D'           VPSG
129000020225      * mittente
129100960209     C     §7Q1SA        IFEQ      'M'
129200020225      * rientri
129300960209     C     VFRI          OREQ      'R'
129400960209     C                   MOVEL     'M'           VPSG
129500960208     C                   END
129600960208     C                   END
129700960209     C                   ENDIF
129800960209     C                   END
129900911121     C     GCPNPC        IFNE      *BLANK
130000911121     C                   MOVEL     GCPNPC        VNPC
130100911219     C     GCPNPC        COMP      'A'                                    21
130200911219     C   21              MOVEL     'ASSEGNAT'    DNPC
130300911219     C   21              MOVE      'O '          DNPC
130400911219     C  N21              MOVEL     'FRANCO  '    DNPC
130500911219     C  N21              MOVE      '  '          DNPC
130600911121     C                   ELSE
130700911121     C                   MOVEL     *BLANKS       VNPC
130800911219     C                   MOVE      *BLANKS       DNPC
130900911121     C                   END
131000911121     C     GCPVCS        IFNE      *BLANK
131100911121     C                   MOVEL     GCPVCS        VVCS
131200911121     C                   ELSE
131300911121     C                   MOVEL     'N'           VVCS
131400911121     C                   END
131500941003     C     GCPCAS        IFNE      *ZEROS
131600941003     C                   MOVEL     GCPCAS        VNCA
131700050216     C                   MOVEL     GCPVCA        v1cndv
131800911121     C                   ELSE
131900911121     C                   MOVEL     *ZEROS        VNCA
132000941003     C                   MOVEL     *BLANKS       V1CNDV
132100911121     C                   END
132200911121     C     GCPAPC        IFNE      *BLANK
132300911121     C                   MOVEL     GCPAPC        VAPC
132400911121     C                   ELSE
132500911121     C                   MOVEL     'N'           VAPC
132600911121     C                   END
132700911121     C     GCPPPC        IFNE      *BLANK
132800911121     C                   MOVEL     GCPPPC        VPPC
132900911219     C                   MOVEL     '2E'          TBLCOD
133000911219     C                   MOVEL     *BLANK        TBLKEY
133100911219     C                   MOVEL     VPPC          TBLKEY
133200911219     C     KTAB          CHAIN     TABEL                              21
133300911219     C   21              MOVEL     *ALL'?'       DPPC
133400911219     C  N21              MOVEL     TBLUNI        DS2E
133500911219     C  N21              MOVEL     §2EDES        DPPC
133600911121     C                   ELSE
133700911121     C                   MOVEL     *BLANK        VPPC
133800911121     C                   END
133900911109     C* BLINK CMD14 SE CI SONO NOTE
134000911109     C*
134100050215     C     KNOT          SETLL     tignp01L                               21
134200960208     C   21              SETON                                        94
134300990913     C* ESEGUO SALVATAGGIO VECCHI DATI GIACENZA X EVENTUALE
134400990913     C* RIPRISTINO SE NON CONFERMO SPESE
134500990913     C                   MOVEL     GCPDIS        WSVDIS
134600990913     C                   MOVEL     GCPPSG        WSVPSG
134700990913     C                   MOVEL     GCPASG        WSVASG
134800990913     C                   MOVEL     GCPNPC        WSVNPC
134900060331     C                   MOVEL     GCPchiaut     WSv1cchim
135000990913     C                   MOVEL     GCPVCS        WSVVCS
135100990913     C                   MOVEL     GCPCAS        WSVCAS
135200990913     C                   MOVEL     GCPVCA        WSVVCA
135300990913     C                   MOVEL     GCPPPC        WSVPPC
135400990913     C                   MOVEL     GCPAPC        WSVAPC
135500990913     C                   MOVE      GCPDDM        WSVDDM
135600990913     C                   MOVE      GCPDED        WSVDED
135700990913     C                   Z-ADD     GCPTFT        WSVTFT
135800990913     C                   Z-ADD     GCPSGS        WSVSGS
135900990913     C                   Z-ADD     GCPSGR        WSVSGR
136000990913     C                   Z-ADD     GCPSGD        WSVSGD
136100990913     C                   Z-ADD     GCPSGA        WSVSGA
136200990913     C                   MOVEL     GCPDIV        WSVDIV
136300990913     C                   MOVE      GCPDLD        WSVDLD
136400990913     C                   Z-ADD     GCPFAS        WSVFAS
136500170307     C                   move      GCPflr        WSVFlr
136600990913     C*
136700900418     C                   ENDSR
136800941010     C*---------------------------------------------------------------*
136900941010     C*   C T R G G A                                                 *
137000941010     C*---------------------------------------------------------------*
137100941010     C     CTRGGA        BEGSR
137200941010     C*
137300941010     C     V1CGGA        IFNE      *BLANKS
137400941010     C                   Z-ADD     1             X
137500941010     C     V1CGGA        LOOKUP    C7Q(X)                                 31
137600941010     C     *IN31         IFEQ      '1'
137700960206     C                   MOVEL     D7Q(X)        DS7Q
137800960206     C     GCPFAS        IFEQ      36
137900960206     C     GCPFAS        OREQ      26
138000941010     C                   MOVEL     §7Q5DI        GCPDIS
138100050808      * se forzatura con azione prof. edp rendo manutenzionabile la disposizione
138200060403     c                   if        d82fspe <> 'S'
138300050808     c                   if        kcdaz <> 'XG83'
138400941010     C                   SETON                                        02
138500050808     c                   else
138600050808     c                   z-add     20            gcpfas
138700050809     c                   movel     'C'           gcpdis
138800050808     c                   endif
138900060403     c                   endif
139000050808
139100941010     C                   END
139200941010     C                   END
139300960206     C                   END
139400941010     C*
139500941010     C                   ENDSR
139600900418     C*---------------------------------------------------------------*
139700900418     C*   C T R V I D                                                 *
139800900418     C*---------------------------------------------------------------*
139900900418     C     CTRVID        BEGSR
140000900418     C                   SETOFF                                       19
140100960206     C                   MOVEL     'N'           WMODSP            1
140200941003     C* Cod.particolarità giacenza
140300960207     C     '?'           SCAN      V1CRPG                                 33
140400960207     C     *IN33         IFEQ      '1'
140500941003     C                   MOVEL     'S'           DS7RIC
140600941003     C                   MOVEL     V1CGGA        DS7COD
140700941003     C                   MOVEL     'V'           DS7GES
140800941003     C                   MOVEL     DSTB69        KPJBU
140900941003     C                   CALL      'TRTB69R'
141000941003     C                   PARM                    KPJBA
141100941003     C                   SETON                                        19
141200941003     C                   MOVEL     ' '           V1CRPG
141300941003     C                   GOTO      ENDCTR
141400941003     C                   END
141500911017     C* RICERCA     TIPO DISPOSIZIONI
141600941003     C     VFRI          IFEQ      '?'
141700911017     C*§  RICERCA IN TABEL00F
141800911017     C                   MOVEL     '2D'          §COD
141900911017     C                   MOVEL     *BLANK        §KEY
142000911017     C                   MOVEL     *BLANK        §DES
142100911017     C                   MOVE      *BLANK        VFRI
142200911017     C                   CALL      'X§TABER'
142300911017     C                   PARM                    CODUT
142400911017     C                   PARM                    §COD
142500911017     C                   PARM                    §KEY
142600911017     C                   PARM                    §DES
142700911017     C                   MOVEL     §KEY          VFRI
142800911017     C                   MOVEL     §DES          DFRI
142900911017     C                   SETON                                        19
143000911017     C   19              GOTO      ENDCTR
143100911017     C                   END
143200051007     c* solo in partenza e in consegna accetto il campo dell'appuntamento
143300051011     c                   if        d82ges ='P'
143400051011     c                   if        v1capp= 'A'
143500051007     c                   if        vfri <>'C'
143600051007     C                   SETON                                        1928
143700051007     C                   eval      $MSG='Appuntamento ammesso solo se CONSEGNA'
143800051007     C                   GOTO      ENDCTR
143900051007     c                   end
144000051007     c* se data eseguibilità è manutenzionabile e ho messo l'appuntamento
144100051007     c* non posso inserirla
144200051007     c                   if        v1cded <> 0 and gcpded = 0
144300051007     C                   SETON                                        1928
144400051007     C                   eval      $MSG='Appuntamento ammesso solo se DATA ESE-
144500051007     C                             GUIBILITà non presente'
144600051007     C                   GOTO      ENDCTR
144700051007     c                   end
144800051011     c* se data eseguibilità è manutenzionabile e c'è l'appuntamento
144900051011     c* con la data eseguibilità immessa dall'arrivo non posso toglierla
145000051011     c                   if        v1cded = 0 and gcpded <>0
145100051011     C                   SETON                                        1928
145200051011     C                   eval      $MSG='Non è possibili eliminare la DATA ESE-
145300051011     C                             GUIBILITà immessa precedentemente'
145400051011     C                   GOTO      ENDCTR
145500051011     c                   end
145600051018     c                   if        v1cded<>0 and gcpded<>0 and d§gcpapp=' '
145700051018     C                   SETON                                        1928
145800051018     C                   eval      $MSG='Non è possibili immettere il flag APP-
145900051018     C                             UNTAMENTO se esiste già la DATA ESEGUIBILITà'
146000051018     C                   GOTO      ENDCTR
146100051018     c                   end
146200051011     c                   else
146300051011     c* se esiste il flag d'appuntamento non è possibile toglierlo
146400051011     c                   if        D§GCPAPP = 'A' and gcpded <> 0
146500051011     C                   SETON                                        1928
146600051011     C                   eval      $MSG='Non è possibili eliminare il flag APP-
146700051011     C                             UNTAMENTO immesso precedentemente'
146800051011     C                   GOTO      ENDCTR
146900051011     c                   end
147000051011     c                   end
147100051007     c                   end
147200911017     C* RICERCA       CHI ADDEBIT. SPESE GIACENZA
147300911017     C     VPSG          IFEQ      '?'
147400911017     C*§  RICERCA IN TABEL00F
147500911017     C                   MOVEL     '2E'          §COD
147600911017     C                   MOVEL     *BLANK        §KEY
147700911017     C                   MOVEL     *BLANK        §DES
147800061107     C*mm                MOVE      *BLANK        VFRI
147900911017     C                   CALL      'X§TABER'
148000911017     C                   PARM                    CODUT
148100911017     C                   PARM                    §COD
148200911017     C                   PARM                    §KEY
148300911017     C                   PARM                    §DES
148400911017     C                   MOVEL     §KEY          VPSG
148500911017     C                   MOVEL     §DES          DPSG
148600911017     C                   SETON                                        19
148700911017     C   19              GOTO      ENDCTR
148800911017     C                   END
148900911017     C* RICERCA       CHI ADDEBIT. PROVV.C/ASSEGNO
149000911017     C     VPPC          IFEQ      '?'
149100911017     C*§  RICERCA IN TABEL00F
149200911017     C                   MOVEL     '2E'          §COD
149300911017     C                   MOVEL     *BLANK        §KEY
149400911017     C                   MOVEL     *BLANK        §DES
149500911017     C                   MOVE      *BLANK        VPPC
149600911017     C                   CALL      'X§TABER'
149700911017     C                   PARM                    CODUT
149800911017     C                   PARM                    §COD
149900911017     C                   PARM                    §KEY
150000911017     C                   PARM                    §DES
150100911017     C                   MOVEL     §KEY          VPPC
150200911017     C                   MOVEL     §DES          DPPC
150300911017     C                   SETON                                        19
150400911017     C   19              GOTO      ENDCTR
150500911017     C                   END
150600900420     C* DATA DISPOSIZIONI
150700911017     C* -----------------
150800941003     C                   MOVE      VDDM          G02DAT
150900941003     C                   MOVEL     ' '           G02ERR
151000941003     C                   CALL      'XSRDA8'
151100941003     C                   PARM                    WLBDA8
151200941003     C     G02ERR        IFEQ      '1'
151300941003     C                   SETON                                        511928
151400941003     C                   MOVEL     ERR(3)        $MSG
151500941003     C                   GOTO      ENDCTR
151600941003     C                   END
151700941003     C                   MOVE      G02INV        AMGDDM
151800950213     C                   Z-ADD     G02TGI        WGGDDM            5 0
151900941003     C     AMGDDM        IFLT      AMGDTG
152000941003     C                   SETON                                        511928
152100941003     C                   MOVEL     ERR(4)        $MSG
152200941003     C                   GOTO      ENDCTR
152300941003     C                   END
152400950530     C     AMGDDM        IFLT      AMGDUR
152500950530     C                   SETON                                        511928
152600950530     C                   MOVEL     ERR(12)       $MSG
152700950530     C                   GOTO      ENDCTR
152800950530     C                   END
152900941003     C     AMGDDM        IFGT      AMGUDT
153000941003     C                   SETON                                        511928
153100941003     C                   MOVEL     ERR(5)        $MSG
153200941003     C                   GOTO      ENDCTR
153300941003     C                   END
153400050415     C* DATA  ESEguibilita DISPOSIZIONI
153500941003     C* ------------------------------
153600950228     C                   Z-ADD     0             AMGDED
153700941006     C     V1CDED        IFNE      0
153800941003     C                   MOVE      V1CDED        G02DAT
153900941003     C                   MOVEL     ' '           G02ERR
154000941003     C                   CALL      'XSRDA8'
154100941003     C                   PARM                    WLBDA8
154200941003     C     G02ERR        IFEQ      '1'
154300941003     C                   MOVEL     ERR(3)        $MSG
154400941003     C                   SETON                                        521928
154500941003     C                   GOTO      ENDCTR
154600941003     C                   END
154700941005     C                   MOVE      G02INV        AMGDED            8 0
154800941006     C                   MOVE      G02DAT        V1CDED
154900950213     C                   Z-ADD     G02TGI        WGGDED            5 0
155000941003     C     AMGDED        IFLT      AMGDTG
155100941003     C                   SETON                                        521928
155200941003     C                   MOVEL     ERR(6)        $MSG
155300941003     C                   GOTO      ENDCTR
155400941003     C                   END
155500941003     C     AMGDED        IFLT      AMGDDM
155600941003     C                   MOVEL     ERR(7)        $MSG
155700941003     C                   SETON                                        525119
155800941003     C                   SETON                                        28
155900941003     C                   GOTO      ENDCTR
156000941003     C                   END
156100950213     C     WGGDED        SUB       WGGDDM        WGGDIF            5 0
156200950213     C     WGGDIF        IFGT      §2GESE
156300950213     C                   MOVEL     ERR(11)       MESSAG
156400950213     C                   Z-ADD     §2GESE        GGMAX
156500950213     C                   MOVEL     MESSAG        $MSG
156600950213     C                   SETON                                        521928
156700950213     C                   GOTO      ENDCTR
156800950213     C                   END
156900941006     C                   END
157000911017     C*
157100911017     C* CONTROLLO TIPO DISPOSIZIONI
157200911017     C* ---------------------------
157300060403      * 34ON spese forzate bypass controlli
157400060403     c                   if        not *in34
157500911017     C                   MOVEL     '2D'          TBLCOD
157600911017     C                   MOVEL     *BLANK        TBLKEY
157700911017     C                   MOVEL     VFRI          TBLKEY
157800911017     C     KTAB          CHAIN     TABEL                              21
157900911017     C   21              MOVEL     *ALL'?'       DFRI
158000911017     C  N21              MOVEL     TBLUNI        DS2D
158100911017     C  N21              MOVEL     §2DDES        DFRI
158200911017     C   21              SETON                                        6019
158300911017     C   60              GOTO      ENDCTR
158400060403     c                   endif
158500960207     C* VARIAZIONI  C/ASSEGNO
158600960207     C* ---------------------
158700960207     C     VVCS          IFEQ      'N'
158800960207     C     VNCA          IFNE      *ZEROS
158900960207     C                   SETON                                        661928
159000960207     C                   MOVEL     ERR(8)        $MSG
159100960207     C                   END
159200960207     C                   END
159300991120     C     V1CNDV        IFEQ      *BLANKS
159400991129     C     VNCA          ANDNE     *ZEROS
159500991120     C                   SETON                                        741928
159600991120     C                   MOVEL     ERR(24)       $MSG
159700991120     C                   END
159800991221     C     V1CNDV        IFNE      *BLANKS
159900991221     C     VNCA          ANDEQ     *ZEROS
160000991221     C                   SETON                                        741928
160100991221     C                   MOVEL     ERR(25)       $MSG
160200991221     C                   END
160300960207     C*
160400960207     C                   Z-ADD     1             XX                3 0
160500960207     C     V1CNDV        LOOKUP    CCV(XX)                                31
160600960207     C     *IN31         IFEQ      '0'
160700960207     C                   SETON                                        741928
160800960207     C                   MOVEL     ERR(9)        $MSG
160900020227     C                   else
161000991120     C* Controllo congruità decimali imnporto/divisa
161100020227     c                   select
161200020227     C     DEC(XX)       wheneq    2
161300991120     C                   MOVE      VNCA          WCOMO1            1 0
161400991120     C     WCOMO1        IFGT      0
161500991120     C                   SETON                                        661928
161600991120     C                   SETON                                        74
161700991120     C                   MOVEL     ERR(23)       $MSG
161800991120     C                   END
161900020227     C     DEC(XX)       wheneq    1
162000991120     C                   MOVE      VNCA          WCOMO2            2 0
162100991120     C     WCOMO2        IFGT      0
162200991120     C                   SETON                                        661928
162300991120     C                   SETON                                        74
162400991120     C                   MOVEL     ERR(23)       $MSG
162500991120     C                   END
162600020227     C     DEC(XX)       wheneq    0
162700991120     C                   MOVE      VNCA          WCOMO3            3 0
162800991120     C     WCOMO3        IFGT      0
162900991120     C                   SETON                                        661928
163000991120     C                   SETON                                        74
163100991120     C                   MOVEL     ERR(23)       $MSG
163200991120     C                   END
163300020227     C                   endsl
163400020227     C                   end
163500960207     C*
163600020227     C  N66VNCA          IFNE      *ZEROS
163700960207     C                   MOVE      VNCA          COMO3             3 0
163800960207     C   31COMO3         IFNE      *ZEROS
163900960207     C     FDC(XX)       ANDNE     'S'
164000960207     C     COMO3         ORNE      *ZEROS
164100961018     C     V1CNDV        ANDEQ     *BLANKS
164200960207     C                   SETON                                        661928
164300960207     C                   SETON                                        74
164400960207     C                   MOVEL     ERR(10)       $MSG
164500960207     C                   END
164600991027     C* IMPOSTO DIVISA X CONTROLLO LIMITE C/ASS.
164700050302     C* IMPOSTO PARAMETRI CHIAMATA TRUL21 PER CONTROLLO LIMITI C/A
164800020226     C                   CLEAR                   TRUL21
164900020226     C                   MOVEl     knmus         I21ute
165000020226     C                   MOVEl     'FNLG10R'     I21pgm
165100020226     C     V1CNDV        IFEQ      *BLANKS
165200020226     C                   MOVEL     V1CDIV        I21DIV
165300020226     C                   ELSE
165400020226     C                   MOVEL     V1CNDV        I21DIV
165500020226     C                   END
165600020226     C                   MOVE      'L'           I21TLA
165700020226     C                   Z-ADD     VNCA          I21IMP
165800020226     c* arrivo
165900130902     c                   if        vnpc = *blank
166000130902     c                   move      export        wnpc              1
166100130902     c                   else
166200130902     c                   move      vnpc          wnpc
166300130902     c                   endif
166400020226     C     D82GES        IFEQ      'A'
166500130902     c                   if        wnpc = 'F'
166600130902     c                   z-add     arbksc        i21ksc
166700130902     c                   else
166800130902     c                   z-add     arbccm        i21ksc
166900130902     c                   if        i21ksc=0
167000130902     c                   z-add     8888          i21ksc
167100130902     c                   movel     arblnp        i21ksc
167200130902     c                   endif
167300130902     c                   endif
167400130902     C                   MOVEl     ARBcbo        I21cbo
167500130902     C                   MOVEl     ARBtsp        I21tsp
167600130902     C                   MOVE      ARBlnp        I21lnp
167700130902     C                   MOVEl     ARBnzm        I21nzm
167800130902     C                   MOVE      ARBlna        I21lna
167900130902     C                   MOVE      ARBctr        I21ctr
168000130902     C                   MOVE      ARBNZD        I21NZD
168100130902     C                   MOVE      AR9TIC        I21TIC
168200130902     c                   else
168300130902     c                   if        wnpc = 'F'
168400130902     c                   z-add     blpksc        i21ksc
168500130902     c                   else
168600130902     c                   z-add     blpccm        i21ksc
168700130902     c                   if        i21ksc=0
168800130902     c                   z-add     8888          i21ksc
168900130902     c                   movel     blplnp        i21ksc
169000130902     c                   endif
169100130902     c                   endif
169200020226     C                   MOVEl     blpcbo        I21cbo
169300020226     C                   MOVEl     blptsp        I21tsp
169400020226     C                   MOVE      blplnp        I21lnp
169500020226     C                   MOVEl     blpnzm        I21nzm
169600020226     C                   MOVE      blplna        I21lna
169700020226     C                   MOVE      blpctr        I21ctr
169800020226     C                   MOVE      blpNZD        I21NZD
169900051115     C                   MOVE      ar9TIC        I21TIC
170000020226     c                   end
170100011217     C                   CALL      'TRUL21R'
170200011217     C                   PARM                    TRUL21
170300050302     C* CONTROLLO CHE NON SIA < AL LIMITE
170400011218     C     O21FMN        IFNE      *BLANK
170500011218     C                   SETON                                        661928
170600011218     C                   MOVEL     ERR(26)       $MSG
170700011218     C                   END
170800050302     C* CONTROLLO CHE NON SIA > AL LIMITE
170900011218     C  N66O21FX2        IFNE      *BLANK
171000011218     C                   SETON                                        661928
171100011218     C                   MOVEL     ERR(27)       $MSG
171200011218     C                   END
171300050302     C* CONTROLLO CHE NON SIA > AL LIMITE X BANCARI BARTOLINI
171400050302     C* NO ABILITATI DA SEDE E NON FORZABILI
171500050302     C  N66O21FB         IFNE      *BLANK
171600050302     C                   SETON                                        661928
171700050302     C                   MOVEL     ERR(29)       $MSG
171800050302     C                   END
171900011217      *SEGNALAZIONE
172000011217     C  N66O21FX1        IFNE      *BLANK
172100011217     C     VNCA          ANDNE     WEXVNC
172200011217     C                   SETON                                            28
172300011217     C                   Z-ADD     VNCA          WEXVNC
172400011217     C                   MOVEL     ERR(28)       $MSG
172500011217     C                   END
172600000121     C                   END
172700960207     C* VAR. C/A=SI O CALC.PROVV.C/A=SI CI DEVE ESSERE IMPORTO C/A
172800960207     C* ----------------------------------------------------------
172900960207     C                   MOVEL     'N'           WANNUL            1
173000960207     C     D82GES        IFEQ      'P'
173100960207     C     VFGC          ANDNE     VLNA
173200960207     C     VNCA          IFEQ      *ZEROS
173300960207     C     VASS          ANDEQ     *ZEROS
173400960207     C* Controllo se la bolla aveva C/Assegno
173500131014     C     Kbol          SETGT     FIARBF2C
173600050216     C     Kbol          READPE    FNARBK00                               08
173700960207     C     *IN08         DOWEQ     '0'
173800960207     C     WANNUL        ANDNE     'S'
173900960207     C     ARBCAS        IFNE      0
174000960207     C                   MOVEL     'S'           WANNUL
174100960207     C                   ELSE
174200050216     C     Kbol          READPE    FNARBK00                               08
174300960207     C                   END
174400960207     C                   END
174500960207     C*
174600960207     C                   END
174700960207     C                   END
174800960207     C*
174900960207     C     VVCS          IFEQ      'S'
175000960207     C     WANNUL        IFEQ      'N'
175100960207     C     VNCA          ORNE      0
175200960207     C* SE IMPORTO C/A E UGUALE 0 NON PUò VVCS = S
175300960207     C     VNCA          IFEQ      *ZEROS
175400960207     C     VASS          ANDEQ     *ZEROS
175500960207     C* SE IMPORTO C/A E UGUALE NON PUò VVCS = S
175600960207     C     VNCA          OREQ      VASS
175700960207     C     V1CNDV        ANDEQ     V1CDIV
175800960207     C                   SETON                                        5919
175900960207     C                   END
176000960207     C                   END
176100960207     C                   END
176200960207     C     VAPC          IFEQ      'S'
176300960207     C     VNCA          ANDEQ     *ZEROS
176400960207     C     VASS          ANDEQ     *ZEROS
176500960207     C                   SETON                                        7019
176600960207     C                   END
176700960207     C* ADDEBITO PROVV. C/A
176800960207     C* ---------------------
176900960207     C     VAPC          IFEQ      'S'
177000960207     C                   MOVEL     '2E'          TBLCOD
177100960207     C                   MOVEL     *BLANK        TBLKEY
177200960207     C                   MOVEL     VPPC          TBLKEY
177300960207     C     KTAB          CHAIN     TABEL                              21
177400960207     C   21              MOVEL     *ALL'?'       DPSG
177500960207     C  N21              MOVEL     TBLUNI        DS2E
177600960207     C  N21              MOVEL     §2EDES        DPPC
177700960207     C   21              SETON                                        6719
177800960207     C   21              GOTO      ENDCTR
177900960207     C* SONO IN PARTENZA
178000960207     C     D82GES        IFEQ      'P'
178100020225     c* mai
178200960207 B1  C     §2EPCP        IFEQ      'N'
178300960207     C                   SETON                                            67
178400960207     C                   ELSE
178500020225     c* no 88 e 99
178600960207 B2  C     §2EPCP        IFEQ      'V'
178700960207     C     §8899         COMP      8888                                   67
178800960207     C  N67§8899         COMP      9999                                   67
178900960207     C                   ELSE
179000020225     c* no 88
179100960207 B3  C     §2EPCP        IFEQ      '8'
179200960207     C     §8899         COMP      8888                                   67
179300960207     C                   ELSE
179400020225     c* no 99
179500960207 B4  C     §2EPCP        IFEQ      '9'
179600960207     C     §8899         COMP      9999                                   67
179700960207     C                   END
179800960207     C                   END
179900960207     C                   END
180000960207     C                   END
180100960207     C                   ELSE
180200960207     C* SONO IN ARRIVO     ----
180300020225     c* mai
180400960207 B1  C     §2EPCA        IFEQ      'N'
180500960207     C                   SETON                                            67
180600960207     C                   ELSE
180700020225     c* no 88 e 99
180800960207 B2  C     §2EPCA        IFEQ      'V'
180900960207     C     §8899         COMP      8888                                   67
181000960207     C  N67§8899         COMP      9999                                   67
181100960207     C                   ELSE
181200020225     c* no 88
181300960207 B3  C     §2EPCA        IFEQ      '8'
181400960207     C     §8899         COMP      8888                                   67
181500960207     C                   ELSE
181600020225     c* no 99
181700960207 B4  C     §2EPCA        IFEQ      '9'
181800960207     C     §8899         COMP      9999                                   67
181900960207     C                   END
182000960207     C                   END
182100960207     C                   END
182200960207     C                   END
182300960207     C   67              SETON                                        19
182400960207     C                   END
182500960207     C                   ELSE
182600960207     C* SE NO ADDEBITO PROVV.C/A E CHI PAGA è CAMBIATO ERRORE
182700960207     C     VPPC          IFNE      *BLANK
182800060808     C     d82fspe       andeq     *BLANK
182900960207     C                   SETON                                        7119
183000960207     C                   END
183100960207     C                   END
183200020225     C* ho variato c/a
183300960207     C     VVCS          IFEQ      'S'
183400020225     C* e importo > 0
183500960207     C     VNCA          ANDNE     0
183600960207     C     VAPC          ANDNE     'S'
183700060808     C     d82fspe       andeq     *BLANK
183800960207     C                   SETON                                        7119
183900960207     C                   END
184000020225     C* non ho variato c/a
184100960207     C     VVCS          IFEQ      'N'
184200020225     C* e importo > 0
184300960207     C     VASS          ANDNE     0
184400960207     C     VAPC          ANDNE     'S'
184500960207     C     VFRI          ANDNE     'R'
184600060808     C     d82fspe       andeq     *BLANK
184700960207     C                   SETON                                        7119
184800960207     C                   END
184900020225     c* NON AMMESSO DIROTTAMENTO PER GIACENZE ESTERE
185000020225     c* internaz.
185100971112     C     GIAEST        IFEQ      'E'
185200961202     C     VFRI          ANDEQ     'D'
185300961202     C                   SETON                                        1928
185400961202     C                   MOVEL     ERR(20)       $MSG
185500961202     C                   END
185600960207     C* SE HO ANNULLATO C/A NO ADDEBITO PROVVIGIONI
185700020225     c* ho annullato c/a
185800960207     C     VNCA          IFEQ      0
185900020225     c* no calcolo provvigioni
186000960207     C     VVCS          ANDEQ     'S'
186100960207     C     VAPC          ANDEQ     'S'
186200960207     C                   SETON                                        7019
186300960207     C                   END
186400960207     C* IN CASO DI RIENTRO IL C/A VIENE AUTOMATIC. ANNULLATO
186500960207     C* NON DEVO INDICARE VARIAZ. SU C/A
186600960207     C     VFRI          IFEQ      'R'
186700960207     C     VVCS          IFEQ      'S'
186800960207     C                   SETON                                        6919
186900960207     C                   END
187000960207     C     VAPC          IFEQ      'S'
187100060808     C     d82fspe       andeq     *BLANK
187200960207     C                   SETON                                        7119
187300960207     C                   END
187400960207     C                   END
187500960208     C* CONTR. NUOVO PORTO DI CONSEGNA
187600960208     C* ------------------------------
187700960208     C                   MOVE      EXPORT        PORTO
187800960208     C     VNPC          IFNE      *BLANKS
187900020225     c* stesso porto
188000960208     C     EXPORT        IFEQ      VNPC
188100960208     C                   SETON                                        6419
188200960208     C                   END
188300960208     C* SE  NUOVO PORTO DI CONSEGNA =F SOLO IN PARTENZA
188400960208     C* ------------------------------
188500960208     C     D82GES        IFEQ      'A'
188600960208     C     VNPC          ANDEQ     'F'
188700960208     C                   SETON                                        6519
188800960208     C                   END
188900960208     C*
189000960208     C                   MOVE      VNPC          PORTO             1
189100960208     C                   END
189200960208     C* SE HO ANNULLATO C/A RICHIEDO CMD  8 DI CONFERMA
189300960208     C   KH              SETON                                        99
189400960208     C   KH              SETOFF                                       9819
189500020225     c* ho annullato c/a
189600020227     C  N99VNCA          IFEQ      0
189700960208     C     VVCS          ANDEQ     'S'
189800960208     C                   SETON                                        9819
189900960208     C                   END
190000960207     C* ESEGUO ROUTINE X CONTROLLO ADDEBITO SPESE/TIPO ADDEBITO
190100960208     C  N19              EXSR      CTRDSP
190200960206     C*
190300960208     C     ENDCTR        ENDSR
190400960207     C*---------------------------------------------------------------*
190500960207     C*   C T R D S P                                                 *
190600960207     C*---------------------------------------------------------------*
190700960207     C     CTRDSP        BEGSR
190800960207     C*---------------------------------------------------*
190900960207     C* TIPO DISPOSIZIONI
191000960207     C*---------------------------------------------------*
191100020225     c* in partenza
191200960207     C     D82GES        IFEQ      'P'
191300960207     C*---------------------------------------------------*
191400960207     C                   MOVE      BLPKSC        §8899
191500020225     c* mai
191600960207     C     §2DUTP        IFEQ      'N'
191700020225     c* no 88 e99
191800960207     C     §2DUTP        OREQ      'V'
191900020225     c* e cli.8888
192000960207     C     §8899         ANDEQ     8888
192100020225     c* no 88 e99
192200960207     C     §2DUTP        OREQ      'V'
192300020225     c* e cli.9999
192400960207     C     §8899         ANDEQ     9999
192500020225     c* no 88
192600960207     C     §2DUTP        OREQ      '8'
192700020225     c* e cli.8888
192800960207     C     §8899         ANDEQ     8888
192900020225     c* no 99
193000960207     C     §2DUTP        OREQ      '9'
193100020225     c* e cli.9999
193200960207     C     §8899         ANDEQ     9999
193300960207     C                   SETON                                        60
193400960207     C                   END
193500960207     C*---------------------------------------------------*
193600960207     C                   ELSE
193700960207     C*---------------------------------------------------*
193800020225     c* in arrivo
193900960207     C                   MOVE      ARBKSC        §8899
194000020225     c* mai
194100960207     C     §2DUTA        IFEQ      'N'
194200020225     c* no 88 e 99
194300960207     C     §2DUTA        OREQ      'V'
194400020225     c* e cli.8888
194500960207     C     §8899         ANDEQ     8888
194600020225     c* no 88 e 99
194700960207     C     §2DUTA        OREQ      'V'
194800020225     c* e cli.9999
194900960207     C     §8899         ANDEQ     9999
195000020225     c* no 88
195100960207     C     §2DUTA        OREQ      '8'
195200020225     c* e cli.8888
195300960207     C     §8899         ANDEQ     8888
195400020225     c* no 99
195500960207     C     §2DUTA        OREQ      '9'
195600020225     c* e cli.9999
195700960207     C     §8899         ANDEQ     9999
195800960207     C                   SETON                                        60
195900960207     C                   END
196000960207     C*----------------------------------------------------*
196100960207     C* I dirottamenti in porto assegnato non sono utilizz.*
196200960207     C* in arrivo a meno che non ci sia la particolatità   *
196300960209     C  N60§7Q1SA        IFEQ      ' '
196400960207     C     EXPORT        ANDEQ     'A'
196500960207     C     VFRI          ANDEQ     'D'
196600971002     C* OGGI 2/10: se sto lavorando con un porto assegnato
196700971002     C*            codificato lascio immmettere dirottam.
196800971002     C*            in arrivo
196900020225     c* no dirott.
197000971002     C     §8899         IFEQ      8888
197100020225     c* p.assegnati
197200971002     C     §8899         OREQ      9999
197300020225     c* se no codif.
197400971002     C                   SETON                                        60
197500960207     C                   END
197600971002     C                   END
197700960207     C*----------------------------------------------------*
197800960207     C                   END
197900960207     C*----------------------------------------------------*
198000960207     C     *IN60         CABEQ     '1'           ENDDSP                   19
198100960207     C*---------------------------------------------------*
198200960207     C* TIPO DISPOSIZIONI
198300960207     C*---------------------------------------------------*
198400960207     C* Repemmesso no spese reperisco valori in tabella
198500960207     C                   MOVEL     '2E'          TBLCOD
198600960207     C                   MOVEL     *BLANK        TBLKEY
198700960207     C*---------------------------------------------------*
198800960207     C     VASG          IFEQ      'N'
198900960207     C* x no spese giac. non immettere chi paga
199000960207     C     VPSG          IFNE      ' '
199100960207     C                   SETON                                        194728
199200960207     C                   MOVEL     ERR(16)       $MSG
199300960207     C                   GOTO      ENDDSP
199400960207     C                   END
199500960207     C*---------------------------------------------------*
199600960207     C                   MOVEL     VASG          TBLKEY
199700960207     C     KTAB          CHAIN     TABEL                              21
199800960207     C                   MOVEL     TBLUNI        DS2E
199900960207     C*---------------------------------------------------*
200000960207     C* X si spese controllo se chi paga valido
200100960207     C                   ELSE
200200960207     C                   MOVEL     VPSG          TBLKEY
200300960207     C     KTAB          CHAIN     TABEL                              21
200400960207     C  N21              MOVEL     TBLUNI        DS2E
200500960208     C     *IN21         IFEQ      '1'
200600960208     C     §2EPSG        ORNE      ' '
200700960207     C                   SETON                                        281947
200800960207     C                   MOVEL     ERR(15)       $MSG
200900960207     C                   MOVEL     *ALL'?'       DPSG
201000960207     C                   GOTO      ENDDSP
201100960207     C                   ELSE
201200960207     C                   MOVEL     §2EDES        DPSG
201300960207     C                   END
201400960207     C                   END
201500061116     C* Se dirottamento di un assegnato la disposizione va inserita
201600061116     C* obbligatoriamente in partenza se devono essere recuperate le spese
201700061116     C     VFRI          IFeq      'D'
201800061116     C     export        ANDEQ     'A'
201900061116     C     d82ges        ANDEQ     'A'
202000061116     C                   SETON                                        281947
202100061116     C                   MOVEL     ERR(30)       $MSG
202200061116     C                   GOTO      ENDDSP
202300061116     C                   END
202400061116     C*
202500960208     C* Se dirottamento o reso obbligo a mettere Mittente/No spese
202600960208     C     VFRI          IFNE      'C'
202700060801     C     VFRI          andne     *blank
202800960208     C     VPSG          ANDEQ     'D'
202900960208     C                   SETON                                        281947
203000960208     C                   MOVEL     ERR(18)       $MSG
203100960208     C                   GOTO      ENDDSP
203200960208     C                   END
203300960207     C*---------------------------------------------------*
203400960208     C                   MOVEL     ' '           WFORZ1            1
203500020225     c* no particol.
203600960207     C     §7Q1SF        IFEQ      ' '
203700960208     C     EXPORT        ANDEQ     'F'
203800960208     C     §7Q1SA        OREQ      ' '
203900960208     C     EXPORT        ANDEQ     'A'
204000960207     C* Se non c'è particolarità contr. congruità tab.2E
204100960207     C*---------------------------------------------------*
204200020225     c* arrivo !!
204300960207     C     D82GES        IFEQ      'A'
204400960208     C* Se dirottamento o reso obbligo a mettere Mittente/No spese
204500960208     C     VFRI          IFEQ      'D'
204600960208     C     VASG          ANDEQ     'N'
204700960208     C                   SETON                                        281948
204800960208     C                   MOVEL     ERR(19)       $MSG
204900960208     C                   GOTO      ENDDSP
205000960208     C                   END
205100960209     C* Anche se particol. vincola spese dest. x franco in arrivo
205200960208     C     §7Q1SF        IFEQ      'D'
205300960208     C     VPSG          ANDNE     'D'
205400960208     C                   SETON                                        281948
205500960208     C                   MOVEL     ERR(14)       $MSG
205600960208     C                   GOTO      ENDDSP
205700960208     C                   END
205800960207     C* Imposto varibili x test congruità
205900020225     c* porto franco
206000960207     C     EXPORT        IFEQ      'F'
206100960207     C                   MOVEL     §2ESAF        WTPADD            1
206200960207     C                   MOVEL     §2EFAF        WFORZ1            1
206300020225     c* porto assegnato
206400960207     C                   ELSE
206500960207     C                   MOVEL     §2ESAA        WTPADD
206600960207     C                   MOVEL     §2EFAA        WFORZ1
206700960207     C                   END
206800960207     C                   ELSE
206900020225     c* partenza
207000960207     C* Imposto varibili x test congruità
207100020225     c* porto franco
207200960208     C     EXPORT        IFEQ      'F'
207300960207     C                   MOVEL     §2ESPF        WTPADD            1
207400960207     C                   MOVEL     §2EFPF        WFORZ1            1
207500960207     C                   ELSE
207600020225     c* porto assegnato
207700960207     C                   MOVEL     §2ESPA        WTPADD
207800960207     C                   MOVEL     §2EFPA        WFORZ1
207900960207     C                   END
208000960207     C                   END
208100971105     C*---------------------------------------------------*
208200971105     C* FORZATURA
208300971105     C     WFORZ1        IFEQ      'S'
208400020225     c* internaz.
208500971112     C     GIAEST        ANDEQ     'E'
208600971105     C     VPSG          ANDEQ     'D'
208700971105     C                   MOVEL     ' '           WFORZ1
208800971105     C                   END
208900960207     C*---------------------------------------------------*
209000960207     C* Test congruità
209100960208     C     VFRI          IFEQ      'C'
209200020225     c* mai
209300960208     C     WTPADD        IFEQ      'N'
209400020225     c* no vari
209500960207     C     WTPADD        OREQ      'V'
209600020225     c* cli. 8888
209700960207     C     §8899         ANDEQ     8888
209800020225     c* no vari
209900960207     C     WTPADD        OREQ      'V'
210000020225     c* cli. 9999
210100960207     C     §8899         ANDEQ     9999
210200020225     c* no 8888
210300960207     C     WTPADD        OREQ      '8'
210400020225     c* cli. 8888
210500960207     C     §8899         ANDEQ     8888
210600020225     c* no 9999
210700960207     C     WTPADD        OREQ      '9'
210800020225     c* cli. 9999
210900960207     C     §8899         ANDEQ     9999
211000971002     C* Se sto lavorando su una bolla export non do errore se le spese di
211100971002     C* giacenza non sono a carico del destinatario
211200971002     C     VPSG          IFNE      'D'
211300020225     c* internaz.
211400971112     C     GIAEST        ANDEQ     'E'
211500971002     C* Se sto lavorando su una bolla export non do errore se le provvig.
211600971002     C* C/assegno non sono a carico del destinatario tranne per pomme
211700971002     C                   ELSE
211800960208     C                   SETON                                          4728
211900960207     C                   MOVEL     ERR(17)       $MSG
212000960207     C*  Controllo se forzabile
212100960208     C     WFORZ1        IFEQ      'S'
212200990913     C     WSVPS1        IFEQ      VPSG
212300990913     C     WSVAS1        ANDEQ     VASG
212400960208     C     *INKA         ANDEQ     '1'
212500960208     C                   SETON                                        2847
212600960208     C                   ELSE
212700960208     C                   SETON                                        04
212800960208     C                   SETOFF                                       19
212900960208     C                   END
213000960209     C                   ELSE
213100960209     C                   SETOFF                                       19
213200960209     C                   END
213300960208     C*
213400960207     C                   END
213500970625     C                   END
213600960208     C                   END
213700970625     C* Per export no addebito spese giacenza al destinatario
213800971112     C     GIAEST        IFEQ      'E'
213900970625     C     VPSG          IFEQ      'D'
214000970625     C                   SETON                                          4728
214100970625     C                   MOVEL     ERR(21)       $MSG
214200970625     C                   END
214300971002     C* Per bolle export se provvigioni C/Assegno a carico destinatario
214400971002     C* lo accetto solo per spedizioni in porto assegnato
214500970625     C     VPPC          IFEQ      'D'
214600020305     c*porto franco
214700971203     C     EXPORT        ANDEQ     'F'
214800970625     C                   SETON                                          4428
214900970625     C                   MOVEL     ERR(21)       $MSG
215000970625     C                   END
215100970625     C                   END
215200960207     C*
215300960207     C                   ELSE
215400960208     C*-------------------------------------------------------------*
215500960208     C*  Arrivo: Controllo se immesso cio' che prevede la partic.
215600960208     C     D82GES        IFEQ      'A'
215700960209     C* Se dirottamento o reso obbligo a mettere Mittente/No spese
215800960209     C     VFRI          IFEQ      'D'
215900960209     C     VASG          ANDEQ     'N'
216000960209     C                   SETON                                        281948
216100960209     C                   MOVEL     ERR(19)       $MSG
216200960209     C                   GOTO      ENDDSP
216300960209     C                   END
216400960208     C     VFRI          IFEQ      'C'
216500960208     C     EXPORT        IFEQ      'F'
216600960208     C     §7Q1SF        ANDEQ     'D'
216700960208     C     VPSG          ANDNE     'D'
216800960208     C     EXPORT        OREQ      'A'
216900960208     C     §7Q1SA        ANDEQ     'M'
217000960208     C     VPSG          ANDNE     'M'
217100960208     C                   SETON                                        194728
217200960208     C                   MOVEL     ERR(14)       $MSG
217300960208     C                   END
217400960208     C                   END
217500960208     C     EXPORT        IFEQ      'F'
217600960208     C     §7Q1SF        ANDEQ     'N'
217700960208     C     VASG          ANDNE     'N'
217800960208     C     EXPORT        OREQ      'A'
217900960208     C     §7Q1SA        ANDEQ     'N'
218000960208     C     VASG          ANDNE     'N'
218100960208     C                   SETON                                        194828
218200960208     C                   MOVEL     ERR(13)       $MSG
218300960208     C                   END
218400960208     C*
218500960208     C                   ELSE
218600960208     C     VFRI          IFEQ      'C'
218700960208     C     EXPORT        IFEQ      'F'
218800960208     C     §7Q1SF        ANDEQ     'D'
218900960208     C     VPSG          ANDEQ     'M'
219000960208     C     EXPORT        OREQ      'A'
219100960208     C     §7Q1SA        ANDEQ     'M'
219200960208     C     VPSG          ANDEQ     'D'
219300960208     C                   SETON                                        472804
219400960208     C                   MOVEL     ERR(14)       $MSG
219500960208     C                   MOVEL     'S'           WFORZ1
219600960208     C                   END
219700960208     C                   END
219800960208     C     EXPORT        IFEQ      'F'
219900960208     C     §7Q1SF        ANDEQ     'N'
220000960208     C     VASG          ANDNE     'N'
220100960208     C     EXPORT        OREQ      'A'
220200960208     C     §7Q1SA        ANDEQ     'N'
220300960208     C     VASG          ANDNE     'N'
220400960208     C                   SETON                                        482804
220500960208     C                   MOVEL     ERR(13)       $MSG
220600960208     C                   MOVEL     'S'           WFORZ1
220700960208     C                   END
220800960208     C                   END
220900971017     C* Per export no addebito spese giacenza al destinatario
221000971112     C     GIAEST        IFEQ      'E'
221100971017     C     VPSG          IFEQ      'D'
221200971017     C                   SETON                                          4728
221300971017     C                   MOVEL     ERR(21)       $MSG
221400971017     C                   END
221500971017     C* Per bolle export se provvigioni C/Assegno a carico destinatario
221600971017     C* lo accetto solo per spedizioni in porto assegnato
221700971017     C     VPPC          IFEQ      'D'
221800971017     C     EXPORT        ANDNE     'F'
221900971017     C                   SETON                                          4428
222000971017     C                   MOVEL     ERR(21)       $MSG
222100971017     C                   END
222200971017     C                   END
222300960207     C*-------------------------------------------------------------*
222400960208     C                   END
222500960208     C*-------------------------------------------------------------*
222600960208     C     ENDDSP        ENDSR
222700911017     C*---------------------------------------------------------------*
222800911017     C*   G E S N O T                                                 *
222900911017     C*---------------------------------------------------------------*
223000911017     C     GESNOT        BEGSR
223100050722     c* imposto un campo di comodo per sapere se inserite note perchè se
223200050722     c* non si conferma la giacenza emetto un messaggio per avvisare che le
223300050722     c* note verranno perse (rollback)
223400050722     c                   clear                   f6note            1
223500030218     c     ricar         tag
223600911017     C                   EXSR      PULIZ
223700050216     C     KNOT          SETLL     tignp01L
223800911017     C     *IN21         DOUEQ     '1'
223900050216     C     KNOT          READE     tignp01L                               21
224000911030     C     *IN21         IFEQ      '0'
224100911125     C                   MOVEL     GNPDMC        VDMC1
224200911030     C                   MOVEL     GNPTPN        VTPN
224300911030     C                   MOVEL     GNPNPR        VNPR
224400030218     C                   z-add     GNPNPR        newprog           3 0
224500911030     C                   ADD       1             NRR               3 0
224600960208     C     VTPN          COMP      'R'                                    11
224700941005     C                   WRITE     LG10S02
224800911030     C                   ELSE
224900960208     C     NRR           DIV       10            WRESTO            2 0
225000960208     C                   MVR                     §NR               2 0
225100960208     C     10            SUB       §NR           §NR
225200911018     C                   DO        §NR
225300960227     C                   SETOFF                                       11
225400911125     C                   MOVEL     *BLANK        VDMC1
225500911030     C                   MOVEL     *BLANK        VTPN
225600911030     C                   ADD       1             VNPR
225700911030     C                   ADD       1             NRR
225800941005     C                   WRITE     LG10S02
225900911030     C                   END
226000911030     C                   END
226100911017     C                   END
226200960208     C                   Z-ADD     1             NR2KEY
226300960208     C*
226400960208     C     NRR           IFNE      0
226500960208     C                   Z-ADD     VNPR          WULTPR            5 0
226600960208     C                   Z-ADD     NRR           WSVNRR            4 0
226700960208     C                   END
226800911017     C*
226900911017     C     STRNOT        TAG
227000941005     C                   WRITE     LG10Z02
227100941005     C                   EXFMT     LG10C02
227200960208     C                   Z-ADD     1             NR2KEY
227300941006     C   KL              GOTO      ENDNOT
227400960208     C*  ROLLUP: CARICAMENTO NUOVA PAGINA
227500960208     C     *IN25         IFEQ      '1'
227600960208     C                   Z-ADD     WULTPR        VNPR
227700960208     C                   Z-ADD     WSVNRR        NRR
227800960208     C                   DO        10
227900960208     C                   MOVEL     *BLANK        VDMC1
228000960208     C                   MOVEL     *BLANK        VTPN
228100960208     C                   ADD       1             VNPR
228200960208     C                   ADD       1             NRR
228300960208     C                   WRITE     LG10S02
228400960208     C                   END
228500960208     C                   Z-ADD     NRR           NR2KEY
228600960208     C                   Z-ADD     VNPR          WULTPR            5 0
228700960208     C                   Z-ADD     NRR           WSVNRR            4 0
228800960208     C                   END
228900030218      *se comando 8 duplico dal file note le righe in fase 20 e tipo nota
229000030218      * = a 'R' escludendo le righe tecniche
229100030218     c                   if        *inkh
229200050722     c                   eval      f6note = '1'
229300030218     c                   exsr      dup_note
229400030218     c                   goto      ricar
229500030218     c                   endif
229600960208     C  NKF              GOTO      STRNOT
229700911017     C*  06 AGGIORNO
229800941003     C   KF              DO        *HIVAL        NRR
229900941005     C     NRR           CHAIN     LG10S02                            21
230000911017     C   21              GOTO      ENDNOT
230100050722     c                   eval      f6note = '1'
230200050215     C     KNOR          CHAIN     tignp01L                           22
230300911017     C     *IN22         IFEQ      '1'
230400911125     C     VDMC1         ANDNE     *BLANK
230500911017     C                   MOVE      VAGC          GNPAGC
230600911017     C                   MOVE      VFGC          GNPFGC
230700911017     C                   MOVE      VNGC          GNPNGC
230800911017     C                   MOVE      §FRG          GNPFRG
230900911125     C                   MOVEL     VDMC1         GNPDMC
231000911017     C                   MOVEL     VTPN          GNPTPN
231100911017     C                   MOVE      VNPR          GNPNPR
231200911017     C                   MOVEL     §FAS          GNPFAS
231300990628     C                   Z-ADD     0             GNPDIF
231400990628     C                   Z-ADD     0             GNPNRI
231500050215     C                   WRITE     tignp000
231600911109     C                   END
231700911109     C     *IN22         IFEQ      '0'
231800911125     C                   MOVEL     VDMC1         GNPDMC
231900911017     C                   MOVEL     VTPN          GNPTPN
232000911017     C                   MOVE      VNPR          GNPNPR
232100911017     C                   MOVEL     §FAS          GNPFAS
232200990628     C                   Z-ADD     0             GNPDIF
232300990628     C                   Z-ADD     0             GNPNRI
232400050215     C                   UPDATE    tignp000
232500911017     C                   END
232600911017     C                   END
232700911017     C     ENDNOT        ENDSR
232800911017     C* PULIZIA SUBFILE
232900911017     C     PULIZ         BEGSR
233000911017     C                   SETON                                        70
233100941005     C                   WRITE     LG10C02
233200911017     C                   Z-ADD     0             NRR
233300911017     C                   SETOFF                                       70
233400911017     C                   ENDSR
233500030218     C*---------------------------------------------------------------*
233600030218     C*   dup_note                                                    *
233700030218     C*---------------------------------------------------------------*
233800030218     C     dup_note      begsr
233900030218     c                   movel     *blank        costtec          20
234000030218     c                   eval      costtec = 'DATA DISPOS.CLIENTE:'
234100050215     C     KNOt          setll     tignp01L
234200030218     C                   do        *hival
234300050215     C     KNOt          reade     tignp01L                               22
234400030218     C   22              leave
234500030218     C                   if        gnptpn <> 'R'
234600030218     c                   iter
234700030218     c                   endif
234800030218     c                   movel     gnpdmc        tecnica          20
234900030218     c                   if        tecnica = costtec
235000030218     c                   iter
235100030218     c                   endif
235200030218
235300030218     C                   MOVE      *blank        GNPTPN
235400030218     c                   add       1             newprog
235500030218     C                   z-add     newprog       GNPNPR
235600030218     C                   MOVEL     §FAS          GNPFAS
235700030218     C                   Z-ADD     0             GNPDIF
235800030218     C                   Z-ADD     0             GNPNRI
235900050215     C                   WRITE     tignp000
236000030218     C                   ENDdo
236100030218     C                   endsr
236200900418     C*---------------------------------------------------------------*
236300900418     C*   G C P W T R                                                 *
236400900418     C*---------------------------------------------------------------*
236500900418     C     GCPWTR        BEGSR
236600900418     C*
236700051013     c* se ci sono le disposizioni già immesse in automatico e cambio
236800051013     c* la disposizione, la data eseguibilità o aggiungo note, elimino
236900051013     c* il flag di disposizioni automatiche, cioè non valgono + le disposiz.
237000051013     c* ricevute e fnlg20r non chiamerà + variazione bolla
237100051013     c                   movel     gcpflr        dgcpflr
237200051013     c                   if        d§gcpdisa = 'S' and
237300051013     c                             (gcpdis <> vfri or gcpded <> amgded or
237400051013     c                              f6note = '1')
237500051013     c                   clear                   d§gcpdisa
237600051013     c                   end
237700051013     c*
237800060403      * se richiamo normale aggiorna per inserimento disposizioni
237900060403      * ALTRIMENTI
238000060403      * se richiamo per manutenzione forzatura spese in conferma apertura
238100060403      * aggiorna solo i campi relativi alle spese/ a chi paga /al porto
238200060403     c                   if        d82fspe <> 'S'
238300060403
238400051007     c                   movel     v1capp        D§GCPAPP
238500051007     C                   MOVEL     VFRI          GCPDIS
238600911018     C                   MOVEL     VPSG          GCPPSG
238700911018     C                   MOVEL     VASG          GCPASG
238800911018     C                   MOVEL     VNPC          GCPNPC
238900911018     C                   MOVEL     VVCS          GCPVCS
239000950316     C                   MOVEL     VNCA          GCPCAS
239100941003     C                   MOVEL     V1CNDV        GCPVCA
239200911018     C                   MOVEL     VPPC          GCPPPC
239300911018     C                   MOVEL     VAPC          GCPAPC
239400900420     C                   MOVE      AMGDDM        GCPDDM
239500051013     C                   MOVE      AMGDED        GCPDED
239600060331     c                   if        v1cchim <> *blank
239700060331     c                   move      'N'           gcpchiaut
239800060331     c                   endif
239900050825     c* FASE:se azione di forzatura forza la fase 36 sempre
240000050809     c                   if        kcdaz = 'XG83'
240100050825     c                   z-add     30            gcpfas
240200060804     c                   move      'N'           gcptcm
240300050809     c                   else
240400050222     c                   if        gcpfas = 20 or gcpfas = 25
240500050222     c                   eval      gcpfas = 30
240600050222     c                   end
240700050222     c                   if        gcpfas = 26
240800050222     c                   eval      gcpfas = 36
240900050222     c                   end
241000050809     c                   endif
241100940120     C* SE SONO IN PARTENZA AZZERO CAMPI
241200941005     C     D82GES        IFEQ      'P'
241300950327     C     GCPNBW        IFEQ      0
241400950327     C     GCPNBW        OREQ      *ALL'9'
241500950413     C                   Z-ADD     0             GCPTFT
241600940120     C                   Z-ADD     0             GCPSGS
241700940120     C                   Z-ADD     0             GCPSGR
241800940120     C                   Z-ADD     0             GCPSGD
241900940120     C                   Z-ADD     0             GCPSGA
242000990628     C                   MOVEL     *BLANKS       GCPDIV
242100950327     C                   END
242200990628     C* AZZERO DATI X TRASMISSIONE CLIENTE
242300990628     C                   Z-ADD     0             GNPNRI
242400990628     C                   Z-ADD     0             GNPDIF
242500050405     C                   MOVE      AMGUDT        GCPDLD
242600170307     c                   move      'M'           D§GCPUTEW
242700170307     c                   movel     dgcpflr       gcpflr
242800050215     C  N40              UPDATE    tigcp000
242900940120     C                   ELSE
243000911120     C* SE IMMESSE DISPOSIZ. IN ARRIVO  FASE =31
243100940120     C                   MOVE      031           GCPFAS
243200170307     C                   MOVE      AMGUDT        GCPDLD
243300170307     c                   move      'D'           D§GCPUTEW
243400170307     c                   movel     dgcpflr       gcpflr
243500050215     C  N40              UPDATE    tigcp000
243600940120     C                   END
243700060403      * forzatura  spese
243800060403     c                   else
243900060403     C                   movel     vasg          GCPasg
244000060403     C                   movel     vpsg          GCPpsg
244100060403     C                   movel     vnpc          GCPnpc
244200060403     C  N40              UPDATE    tigcp000
244300060403      * fine forza spese
244400060403     c                   endif
244500050516      * inserisce note resi forzate se non è gestito il video
244600050722     c                   if        d82ntaut <> *blank and gcpdis = 'R'
244700050516     c                   exsr      not_resi
244800050516     c                   endif
244900050516
245000050808     c                   if        kcdaz = 'XG83'
245100050808     c                   exsr      not_forza
245200050808     c                   endif
245300050808
245400050316     c                   if        gescomm = 'S'
245500050316     c                   commit
245600050316     c                   endif
245700911120     C*
245800900418     C                   ENDSR
245900990913     C*---------------------------------------------------------------*
246000990913     C*   R I P G I A                                                 *
246100990913     C*---------------------------------------------------------------*
246200990913     C     RIPGIA        BEGSR
246300990913     C*
246400990913     C* RIAGGANCIO GIACENZA
246500050316     C     KGIA          CHAIN(e)  tigcp02L                           40
246600050316     c                   if        %error
246700070521     C     KGIA          CHAIN(n)  tigcp02L
246800050316     c                   exsr      geslck
246900050318     C     KGIA          CHAIN(e)  tigcp02L                           40
247000050316     c                   endif
247100990913     C* ESEGUO SALVATAGGIO VECCHI DATI GIACENZA X EVENTUALE
247200990913     C* RIPRISTINO SE NON CONFERMO SPESE
247300990913     C                   MOVEL     WSVDIS        GCPDIS
247400990913     C                   MOVEL     WSVPSG        GCPPSG
247500990913     C                   MOVEL     WSVASG        GCPASG
247600990913     C                   MOVEL     WSVNPC        GCPNPC
247700060331     C                   MOVEL     WSV1cchim     GCPchiaut
247800990913     C                   MOVEL     WSVVCS        GCPVCS
247900990913     C                   MOVEL     WSVCAS        GCPCAS
248000990913     C                   MOVEL     WSVVCA        GCPVCA
248100990913     C                   MOVEL     WSVPPC        GCPPPC
248200990913     C                   MOVEL     WSVAPC        GCPAPC
248300990913     C                   MOVE      WSVDDM        GCPDDM
248400990913     C                   MOVE      WSVDED        GCPDED
248500050216     C                   MOVE      WSVDEDa       GCPDEDa
248600990913     C                   Z-ADD     WSVTFT        GCPTFT
248700990913     C                   Z-ADD     WSVSGS        GCPSGS
248800990913     C                   Z-ADD     WSVSGR        GCPSGR
248900990913     C                   Z-ADD     WSVSGD        GCPSGD
249000990913     C                   Z-ADD     WSVSGA        GCPSGA
249100990913     C                   MOVEL     WSVDIV        GCPDIV
249200990913     C                   MOVE      WSVDLD        GCPDLD
249300990913     C                   Z-ADD     WSVFAS        GCPFAS
249400170307     C                   move      WSVFlr        GCPFlr
249500050215     C                   UPDATE    tigcp000
249600990913     C*
249700050316     c                   if        gescomm = 'S'
249800050316     c                   commit
249900050316     c                   endif
250000990913     C                   ENDSR
250100050316     C*----------------------------------------------------*
250200050316     C     geslck        begsr
250300050316     C*----------------------------------------------------*
250400050316     c                   Eval      ul82§rrn = gcpnrr
250500050316     c                   Eval      UL82§FIL = 'TIGCP00F'
250600050316     c                   Eval      UL82§WIN = 'S'
250700050316     c                   Eval      UL82§F7  = 'S'
250800050316     c                   Eval      UL82§num = 3
250900050316     c                   Eval      UL82§att = 3
251000050404     c                   Eval      UL82§mss = 'Si sta bloccando la giacenza Nr.'
251100050404     c                             + %editc(vagc:'Z') +  '  '+ %editc(vfgc:'Z')
251200050404     c                             + '  ' +  %editc(vngc:'Z') + ' Terminare il -
251300050404     c                             lavoro'
251400050404     c                   Eval      UL82§msw = 'La giacenza Nr. '
251500050404     c                             + %editc(vagc:'Z') +  '  '+ %editc(vfgc:'Z')
251600050404     c                             + '  ' +  %editc(vngc:'Z') +
251700050404     c                             ' non è manutenzionabile'
251800050404
251900050316      * Effettuo la chiamata al *PGM d utilità
252000050316     c                   Call(e)   'TRUL82R'
252100050316     c                   Parm                    Trul82ds
252200050316     c                   if        ul82§sts = 'A'
252300050316     c                   goto      fine
252400050316     c                   endif
252500050316
252600050316     C                   ENDSR
252700050516     C*----------------------------------------------------*
252800050516     C     not_resi      begsr
252900050516     C*----------------------------------------------------*
253000050516     c                   clear                   costrien         26
253100050516     c                   if        gcpfas = 36
253200050516     c                   eval      costrien= 'Riapertura oltre il limite'
253300050516     c                   else
253400050722     c                   eval      costrien= 'Decorsi Termini'
253500050516     c                   endif
253600050516     C     KNOt          setgt     tignp01L
253700050516     C     KNOt          readpe    tignp01L                               22
253800050516     c                   eval      gnpdmc = costrien
253900050516
254000050516     c                   eval      gnpagc = gcpagc
254100050516     c                   eval      gnpfgc = gcpfgc
254200050516     c                   eval      gnpngc = gcpngc
254300050516     c                   eval      gnpfrg = gcpfrg
254400050516
254500050516     C                   MOVE      'I'           GNPTPN
254600050516     c   22              clear                   newprog
254700050516     c  n22              z-add     gnpnpr        newprog
254800050516     C                   eval      GNPNPR = newprog + 1
254900050516     C                   z-add     20            GNPFAS
255000050516     C                   Z-ADD     0             GNPDIF
255100050516     C                   Z-ADD     0             GNPNRI
255200050516     C                   WRITE     tignp000
255300050516
255400050516     C                   endsr
255500050808     C*----------------------------------------------------*
255600050808     C     not_forza     begsr
255700050808     C*----------------------------------------------------*
255800050808     C     KNOt          setgt     tignp01L
255900050808     C     KNOt          readpe    tignp01L                               22
256000060804     c                   eval      gnpdmc = 'Disposizioni forzate da ' + knmus
256100050808     c                   eval      gnpagc = gcpagc
256200050808     c                   eval      gnpfgc = gcpfgc
256300050808     c                   eval      gnpngc = gcpngc
256400050808     c                   eval      gnpfrg = gcpfrg
256500050808
256600050912     C                   MOVE      'R'           GNPTPN
256700050808     c   22              clear                   newprog
256800050808     c  n22              z-add     gnpnpr        newprog
256900050808     C                   eval      GNPNPR = newprog + 1
257000050808     C                   z-add     20            GNPFAS
257100050808     C                   Z-ADD     0             GNPDIF
257200050808     C                   Z-ADD     0             GNPNRI
257300050808     C                   WRITE     tignp000
257400050808
257500050808     C                   endsr
257600050316      *----------------------------------------------------------
257700050216     Otigcp000  E            RILGCP
257800011217** STA
257900941003PRIMA IMM.DISPOS.:
258000941003DOPO  IMM.DISPOS.:
258100911023 NON MANUTENZIONABILE !!
258200941003** ERR
258300941003Bolla inesistente                                                     01
258400941003Bolla già consegnata                                                  02
258500941003Data errata                                                           03
258600941003Data disposizioni minore data apertura giacenza                       04
258700941003Data disposizioni maggiore data del giorno                            05
258800941003Data esecuzione disposizioni minore data apertura giacenza            06
258900941003Data esecuzione disposizioni minore data immissione                   07
259000941006Immettere importo solo se "varia importo C/A" = S                     08
259100941006Codice divisa inesistente                                             09
259200941006Per la divisa indicata non sono ammessi importi con decimali          10
259300950213Data eseguibilità superiore di oltre xx giorni data imm. disposizioni 11
259400950530Data immissione disposizioni minore ultima data riapertura            12
259500960206Addebito spese di giacenza non congruente con particolarità           13
259600960206Chi paga spese di giacenza non congruente con particolarità           14
259700960207Chi paga spese giacenza errato                                        15
259800960207Se "NO" spese giacenza non immettere chi paga spese                   16
259900960209Addebito non congruente con le regole standard !!                     17
260000960207Rientro/Dirottamento no addebito al destinatario                      18
260100960208"NO" paga spese non ammesso in arrivo per dirottamenti                19
260200961202Impossibile eseguire dirottamento per bolle export                    20
260300970625Impossibile immettere addebito spese al destinatario per bolle export 21
260400991027Importo C/Assegno superiore al limite massimo previsto per l'incasso  22
260500991120Decimali importo C/Ass. superiori ai decimali previsti per la divisa  23
260600991120Divisa C/Assegno obbligatoria se immesso l'importo                    24
260700991221Se importo nuovo C/Assegno e a zero non bisogna immettere la divisa   25
260800011217Importo C/Assegno inferiore al limite minimo  previsto per l'incasso  26
260900011217Importo C/Assegno superiore al limite massimo previsto per l'incasso  27
261000011217Importo C/Assegno superiore al limite ENTER per forzare               28
261100061116Importo C/Assegno superiore al limite non ammesso per banc. Bartolini 29
261200061116DIROTTAMENTO ASSEGNATO le disposizioni vanno inserite in partenza     30
261400170516Funzione non eseguibile Programma richiamato in precedenza            31
