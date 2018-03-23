000100070521     H DECEDIT('0,') DATEDIT(*DMY.)
000200911018     H* FLFG14R *----------------------------------------------------*
000300900418     H*                                                              *
000400911018     H*             - CREAZIONE BOLLE DI RECUPERO                    *
000500941005     H*                                                              *
000600900418     H*  19 IND. ERRORE GENERICO VIDEO                               *
000700900418     H*  21 IND. COMODO                                              *
000800910314     H*  30 31 DI COMODO                                             *
000900900418     H*  40 TEST SE GIACENZA GIA CARICATA O NO                       *
001000900419     H*  43 CHAIN FLBLP                                              *
001100900510     H*  50.....57 ERRORI VIDEO                                      *
001200900524     H*  58 SE CAMBIATI I GG SOSTA A VIDEO LO EVIDENZ. CON RI        *
001300900508     H*  59 ERRORE IN FASE ADDEBITO SPESE : IN SCRITTURA FLBLP OPPU -*
001400900508     H*     RE MANCA TABELLA DEFAULT (2G) OPPURE NON ESISTE O E'ALLO-*
001500900508     H*     CATO IL RECORD CON NUMERO DI SPEDIZIONE                  *
001600910319     H*  70 ERRORE                                                   *
001700900418     H*--------------------------------------------------------------*
001800051107     FFNLG10D   CF   E             WORKSTN usropn
001900000201     FFNBLP01L  IF   E           K DISK
002000060213     FFiar401L  IF   E           K DISK
002100051115     FFiar901L  IF   E           K DISK
002200990930     FFIAR601L  IF   E           K DISK
002300050316     FTIGCP02L  UF   E           K DISK    commit infds(gcpinf)
002400131014     FFIARBF2C  IF   E           K DISK
002500941005     F                                     IGNORE(FNARBD00)
002600990930     F                                     IGNORE(FIARBT00)
002700941005     F                                     IGNORE(FNARBG00)
002800990917     F                                     IGNORE(FNARBP00)
002900131014     F                                     IGNORE(FNARBN00)
003000900418     FTABEL00F  IF   E           K DISK
003100991027     FTNTBE01L  IF   E           K DISK
003200941005     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003300930527     FCNACO00F  IF   E           K DISK
003400941006     FFNCLS01L  IF   E           K DISK
003500930527     FCNIND00F  IF   E           K DISK
003600990730     FTITGC01L  IF   E           K DISK
003700030206     fFiar501l  if   e           k Disk
003800030821     FFiar500f  o    E             DISK    rename(fiar5000:fiar50)
003900941223     FFNLV22P   O    F  132        PRINTER USROPN
004000050316     D*--------------------------------------------------------------*
004100050316     d trul82ds      e ds
004200050316     d gcpinf          ds
004300050316     d  gcpnrr               397    400i 0
004400941005     D*--------------------------------------------------------------*
004500030821     D ar5             S                   DIM(30) like(ar5trd)
004600900424     D TGS             S              3  0 DIM(3)
004700990730     D TTS             S             11  3 DIM(3)
004800900424     D SGG             S              3  0 DIM(3)
004900990730     D STU             S             11  3 DIM(3)
005000990730     D STT             S             11  3 DIM(3)
005100950731     D FGB             S              7    DIM(40)
005200941005     D CDB             S              2    DIM(40)
005300950731     D TB1             S              2    DIM(40)
005400941006     D C2H             S              1    DIM(50)
005500960201     D D2H             S             89    DIM(50)
005600941006     D CCV             S              3    DIM(100)
005700011219     D*                   CMB       100  9 3             CAMBIO DIVISA
005800990716     D FDC             S              1    DIM(100)
005900011217     D CNZ             S              3    DIM(500)
006000011217     D ITA             S              1    DIM(500)
006100960130     D C1A             S              2    DIM(100)
006200960130     D T1A             S              1    DIM(100)
006300150114     D CCC             S              1    DIM(100)
006400150114     D IPB             S              1    DIM(100)
006500040122     D*
006600941006     D*--------------------------------------------------------------*
006700950123     D CCO             S              1    DIM(80) CTDATA PERRCD(80)
006800991206     D ODP             S              1    DIM(80) CTDATA PERRCD(80)
006900030304     D Ofe             S              1    DIM(80) CTDATA PERRCD(80)
007000950123     D CME             S              1    DIM(80) CTDATA PERRCD(80)
007100941006     D DOV             S              1    DIM(70) CTDATA PERRCD(70)
007200030717     D CM4             S              1    DIM(80) CTDATA PERRCD(80)
007300030717     D CM5             S              1    DIM(80) CTDATA PERRCD(80)
007400000925     D ERR             S             70    DIM(12) CTDATA PERRCD(1)
007500900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
007600900418     D  TCU                  398    697
007700900418     D                                     DIM(50)
007800900418     D  KCU                  698    847P 0
007900900418     D                                     DIM(50)
008000900418     D                                     PACKEVEN
008100901219     D  DTU                  848    895P 0
008200941005     D                                     DIM(12)
008300941005     D                                     PACKEVEN
008400900418     D TCUDS           DS
008500900418     D  F3                     3      3
008600900418     D  F4                     4      4
008700900418     D  F56                    5      6
008800941005     D WLBDA8          DS
008900941005     D  G02DAT                 1      8  0
009000941005     D  G02INV                 9     16  0
009100941005     D  G02ERR                17     17
009200941005     D  G02TGI                18     22  0
009300911018     D* PARAMETRI PASSATI DA PGM CHIAMANTE
009400941005     D FNLG82        E DS                  EXTNAME(FNLG82DS)
009500920212     D                 DS
009600920212     D  TESTO1                 1      5
009700920212     D  GCAAGC                 6      7  0
009800920212     D  GCAFGC                 9     11  0
009900920212     D  GCANGC                13     19  0
010000920212     D  TESTO2                21     25
010100920212     D  VARIO                 27     33
010200920212     D  RIFGIA                 1     33
010300900418     D CNCR80        E DS
010400990930     D YEURDS        E DS                  EXTNAME(YEURCODS)
010500991027     D DGED          E DS
010600990930     D DBLP          E DS
010700000202     D OG143         E DS
010800000202     D DTAS          E DS
010900060927     D DTAS01        E DS
011000990930     D DTASV         E DS
011100960301     D  NSV                    1     20
011200960301     D                                     DIM(20)
011300990930     D  NVA                   21    140P 3
011400990930     D                                     DIM(20)
011500960130     D DS1A          E DS
011600910314     D DSPR          E DS
011700910314     D DS2C          E DS
011800910314     D DS2G          E DS
011900950210     D DS$2          E DS
012000910314     D DS3A          E DS
012100941006     D DSCV          E DS
012200960201     D DS2H          E DS
012300941005     D DSQT1         E DS
012400941222     D DS15          E DS
012500040112     D DS7w          E DS
012600040122     D DScc          E DS
012700100517     D DS7r          E DS
012800031027      *------
012900050125     D DS2z          E DS
013000000922     D DS2F          E DS
013100031027     d* codici com.giac.
013200031027     D C2F             S              1    DIM(50)
013300031027     d* tipo comun.
013400031027     D T2F             S              1    DIM(50)
013500031027     d* invio comun.
013600031027     D F2F             S              1    DIM(50)
013700031027     d* descrz. comun.
013800031027     D D2F             S             20    DIM(50)
013900031027     d* Riservato
014000031027     D R2F             S             20    DIM(50)
014100050125     d* codici com.giac.
014200080903     D C2z             S              3    DIM(200)
014300050125     d* codici com.giac.
014400080903     D nsr             S              1    DIM(200)
014500031027      *------
014600030717     D TRUL90DS      E DS
014700960201     D TRUL42        E DS                  EXTNAME(TRUL42DS)
014800960314     D TRUL43        E DS                  EXTNAME(TRUL43DS)
014900150415     D fnLV59ds      E DS
015000160316     d  KS_CT                 74    118    dim(15)
015100160316     d  KS_pr                119    163    dim(15)
015200160316     d  KS_ie                164    178    dim(15)
015300160316     D trulc7ds      E DS
015400160316     D TR27DS        E DS                  EXTNAME(TRUL27DS1)
015500970709     D* DS  per reperimento dati cap
015600970709     D TISI95        E DS                  EXTNAME(TISI95DS)
015700030206
015800030206     d dAr5Ban       e ds
015900030805     d dAr5BNB       e ds
016000030821     d dAr5          e ds
016100051206     d fnlg11ds      e ds
016200060809     d*fnlg12ds      e ds
016300060504     d fnlg18ds      e ds
016400091103     d dAr5gen       e ds
016500091103     d dcli          e ds
016600091103     d tibs02ds      e ds
016601171002      *ds per richiamo monitor variazioni
016602171002     d tigcp         e ds                  extname(tigcp00f)
016603171002     d fnlg33ds      e ds                  inz
016604171002     d tigcpa        e ds                  extname(tigcp00f)
016605171002     d                                     prefix(a)
016606171002     d tigcpb        e ds                  extname(tigcp00f)
016607171002     d                                     prefix(b)
016700030206
016800930527     D NWFGB           DS
016900930527     D  NWFCA                  5      5
017000930527     D  NWFAN                  6      6
017100930527     D  NWFTD                  7      7
017200900418     D KPJBA         E DS
017300030206
017400030206     d kAr5Trd         s                   Like(Ar5Trd)
017500030821     d compar          s                   like(blpgva)
017600060329     d savkpjbu        s                   like(kpjbu)
017700941006     I*--------------------------------------------------------------*
017800941005     IAZORG01L  AA
017900941005     I                             P    4    5 0ORGFIL
018000941005     I                                  6    6  ORGFVA
018100971112     I                                 11   11  ORGFL1
018200941005     I                                 14   33  ORGDES
018300000202     I                               3776 3800  ORGDE3
018400990730     ITITGC000
018500900424     I              TGCSG1                      TGS(1)
018600900424     I              TGCSG2                      TGS(2)
018700900424     I              TGCSG3                      TGS(3)
018800900424     I              TGCST1                      TTS(1)
018900900424     I              TGCST2                      TTS(2)
019000900424     I              TGCST3                      TTS(3)
019100941005     C*--------------------------------------------------------------------*
019200941005     C*  INIZIO PGM.
019300941005     C*--------------------------------------------------------------------*
019400941005     C*  Carico i dati della giac. a video
019500900418     C                   EXSR      GCPVID
019600941005     C* Addebito giacenza
019700900423     C* -------------------
019800941005     C     EMFG05        TAG
019900990910     C                   SETOFF                                       65
020000050513      *emetto le videate se il parametro della ds lo permette
020100050513     c                   if        d82nodsp = *blank
020200941005     C  N19              WRITE     LG10Z03
020300941005     C                   EXFMT     LG10D03
020400050513     c                   endif
020500900423     C*                    ------------
020600990908     C                   SETOFF                                       626364
020700900423     C*
020800941005     C     *INKL         IFEQ      '1'
020900941005     C                   MOVEL     '1'           D82F12
021000941005     C                   MOVEL     FNLG82        KPJBU
021100941005     C                   GOTO      FINE
021200911217     C                   END
021300900423     C* CONTROLLO VIDEO ESITAZIONE
021400941005     C                   EXSR      CTRVI5
021500050513      * se non ho la gestione del video e c'è un errore forzo la pulizia
021600050513      * del flag di controllo in modo da emettere il video per segnalare
021700050513      * l'errore altrimenti forzo il comando di conferma per proseguire
021800050513     c                   if        d82nodsp <> *blank
021900050922     c                   if        KTPAZ = ' '
022000050513     c   18
022100050513     cor 19              clear                   d82nodsp
022200050922     c                   else
022300050928     c                   if        *in18 or *in19
022400050922     c                   eval      d82f12 = '1'
022500050922     c                   goto      fine
022600050513     c                   endif
022700050928     c                   endif
022800050922     c                   endif
022900050513
023000941005     C     *IN18         IFEQ      '1'
023100941005     C     *IN19         OREQ      '1'
023200941005     C                   GOTO      EMFG05
023300941005     C                   END
023400030618     c* se c'è stato l'annullamento del porto assegnat e il cliente è codif.
023500030618     c* emetto il formato per chiedere di chi è la responsabilità e se del
023600030618     c* cliente addebito spese varie (&)
023700030618     c                   clear                   viddcli
023800030618     C     VNBW          IFEQ      0
023900030618     C     VTFT          andeq     0
024000030618     C     GCPNPC        andeq     'F'
024100030620     C     GCPdis        andeq     'C'
024200030618     C     §QUATR        andne     *ALL'8'
024300030618     C     §QUATR        ANDNE     *ALL'9'
024400030618     c* imposto come default l'addebito delle spese x il cambio di
024500030618     c* porto al cliente
024600030618     c                   move      'C'           viddcli
024700030620     c                   eval      viddtit = 'CAMBIO DI PORTO'
024800030618     c* emetto il video per chiedere a chi fare l'addebito delle spese
024900030618     c                   do        *hival
025000030618     C                   EXFMT     LG10w01
025100060810     c                   clear                   d82nodsp
025200030618     c   kf              leave
025300030618     c                   end
025400030618     c                   end
025500900423     C* ADDEBITO SPESE E CREAZIONE BOLLA
025600900423     C* --------------------------------
025700990305     C                   SETOFF                                       72
025800990305     C     GCPCFG        COMP      '015'                                  75
025900990305     C   75VTFT          COMP      9                                      72
026000900619     C                   SETOFF                                       5861
026100990716     C                   SETOFF                                       284146
026200941228     C                   SETON                                        90
026300050513      *emetto le videate se il parametro della ds lo permette
026400060809     c*                  if        d82tri <> 'D'
026500060809     c*                  movel     kpjbu         savkpjbu
026600060809     C*                  WRITE     LG10D03
026700060809     c*                  clear                   fnlg12ds
026800060809     c*                  z-add     gcpagc        i12agc
026900060809     c*                  z-add     gcpngc        i12ngc
027000060809     c*                  z-add     gcpfgc        i12fgc
027100060809     c*                  move      'S'           i12nodsp
027200060809     c*                  move      d82motr       i12motr
027300060809     c*                  movel(p)  fnlg12ds      kpjbu
027400060809     c*                  call      'FNLG12R'
027500060809     c*                  parm                    kpjba
027600060809     c*                  movel     kpjbu         fnlg12ds
027700060809     c*                  movel     savkpjbu      kpjbu
027800060809     c*                  if        o12ok <> *blank
027900060809     c*                  clear                   d82nodsp
028000060809     c*                  seton                                        19
028100060809     c*                  endif
028200060809     c*                  endif
028300060329     c
028400050513     c                   if        d82nodsp = *blank
028500941228     C                   WRITE     LG10D03
028600941228     C                   SETOFF                                       90
028700011220     C     EMFG06        TAG
028800941006     C                   EXFMT     LG10D04
028900050513     c                   endif
029000900423     C*                    ------------
029100990716     C                   SETOFF                                       284146
029200900423     C*
029300941005     C   KL              GOTO      EMFG05
029400060504      *
029500060504     c                   if        *inkh
029600060504     c                   clear                   fnlg18ds
029700060504     c                   z-add     gcpaas        I18AAS
029800060504     c                   z-add     gcplnp        I18LNP
029900060504     c                   z-add     gcpnrs        I18NRS
030000060504     c                   z-add     gcpnsp        I18NSP
030100060823     c                   movel     blpaas        I18DISP
030200060823     c                   movel     blpmgs        I18DISP
030300060823     c                   z-add     §nrg          I18ric
030400060823     c                   z-add     §nrgdoss      I18ape
030500060823     c                   z-add     §ggsav        I18gior
030600060906     c                   movel     kpjbu         savkpjbu
030700060906     c                   movel(p)  fnlg18ds      kpjbu
030800060504     c                   call      'FNLG18R'
030900060906     c                   parm                    kpjba
031000060906     c                   movel     kpjbu         fnlg18ds
031100060906     c                   movel     savkpjbu      kpjbu
031200060504     c                   endif
031300900423     C* CONTROLLO VIDEO
031400911213     C                   EXSR      CTRVI6
031500050513      * se non ho la gestione del video e c'è un errore forzo la pulizia
031600050513      * del flag di controllo in modo da emettere il video per segnalare
031700050513      * l'errore altrimenti forzo il comando di conferma per proseguire
031800050513     c                   if        d82nodsp <> *blank
031900050928     c  n19              move      *on           *inkf
032000050922     c                   if        KTPAZ = ' '
032100050513     c   19              clear                   d82nodsp
032200050922     c                   else
032300050928     c                   if        *in19
032400050922     c                   eval      d82f12 = '1'
032500050922     c                   goto      fine
032600050922     c                   endif
032700050928     c                   endif
032800050513     c                   endif
032900050513
033000941005     C  NKF
033100900427     COR 19              GOTO      EMFG06
033200900423     C*
033300900426     C* AGGIORNA ARCHIVI - CMD 6  CONFERMA
033400990908     C     VNBW          IFEQ      0
033500990908     C     VTFT          IFEQ      0
033600990305     C     VRCA          ORGT      0
033700990305     C     *IN75         ANDEQ     '1'
033800990305     C     VRTR          ORGT      0
033900990305     C     *IN75         ANDEQ     '1'
034000950327     C     VTSGS         IFNE      0
034100950327     C     VSGR          ORNE      0
034200950327     C     VSGD          ORNE      0
034300950327     C     VSGA          ORNE      0
034400951212     C*  Se non c'è addebito spese giacenza ma ho eseguito cambio
034500951212     C*  di porto richiamo pgm x creazione bolla di recupero x trasp.
034600951212     C     GCPNPC        OREQ      'F'
034700951212     C     GCPNPC        OREQ      ' '
034800951212     C     OLDTB1        ANDEQ     'FS'
034900960130     C*  Se devo addebitare delle spese di trasporto x dirottam.
035000960130     C*  richiamo pgm.
035100960130     C     VRTR          ORGT      0
035200960130     C*  Se ho clacolato delle provvigioni C/Assegno richiamo pgm
035300960130     C*  per addebtito varia G
035400960130     C     VRCA          ORGT      0
035500941005     C                   EXSR      BLPWTR
035600910207     C                   END
035700950327     C                   END
035800990908     C                   END
035900941005     C                   EXSR      GCPWTR
036000900418     C*
036100900418     C     FINE          TAG
036200941005     C                   MOVEL     FNLG82        KPJBU
036300020222      *RICHIAMO PROGRAMMA PER CHIUSURA FILE
036400020222     C                   CLEAR                   TR27DS
036500020222     C                   MOVE      'C'           I27TLA
036600160316     C                   CALL      'TRUL27R1'
036700020222     C                   PARM                    TR27DS
036800160316     c*
036900160316     C                   CLEAR                   trulc7ds
037000160316     C                   MOVEL     'C'           Ic7TLA
037100160316     C                   CALL      'TRULC7R'
037200160316     C                   PARM                    TRULc7DS
037300160316     c*
037400160316     C                   CLEAR                   Fnlv59DS
037500160316     C                   MOVEL     'C'           ilv59tla
037600160316     C                   CALL      'FNLV59R'
037700160316     C                   PARM                    Fnlv59DS
037800160316     c
037900900418     C                   SETON                                        LR
038000900418     C/EJECT
038100900418     C*---------------------------------------------------------------*
038200941005     C*   OPERAZIONI INIZIALI                                         *
038300900418     C*---------------------------------------------------------------*
038400941005     C     *INZSR        BEGSR
038500941005     C*
038600900418     C     *ENTRY        PLIST
038700900418     C                   PARM                    KPJBA
038800941005     C* Pass. parametri da pgm chiamante
038900941005     C                   MOVEL     KPJBU         FNLG82
039000030717     C* MI COMPONGO TRUL90DS X STAMPA BOLLE
039100050922     c                   if        KTPAZ = ' '
039200051107     c                   open      fnlg10d
039300030717     C                   EXSR      SRTRUL90
039400050922     c                   end
039500941005     C                   MOVEL     *BLANK        D82F12
039600941005     C                   MOVE      D82AGC        KAGC
039700941005     C                   MOVE      D82FGC        KFGC
039800941005     C                   MOVE      D82NGC        KNGC
039900050316     c                   move      d82comm       gescomm           1
040000941005     C* Richiamato da pgm.disposizioni non visualizzo cod.fine giacenza
040100941005     C     D82TRI        COMP      'D'                                7676
040200941005     C                   SETON                                        92
040300941005     C*
040400900418     C                   Z-ADD     1             CODUT
040500900418     C                   CALL      'X§PARUT'
040600900418     C                   PARM                    UTEDSE
040700900418     C     CODUT         CABEQ     -1            FINE
040800900418     C                   MOVEL     RAGUT         RSUT             20
040900900418     C                   MOVEL     RAGUT         RSUT2            25
041000941010     C                   MOVEL     '* FNLG14'    VPROGR
041100900423     C                   MOVE      'R *'         VPROGR
041200900427     C                   MOVEL     RAGUT         PARDUT           30
041300900418     C*--- RICERCA CAPOCONTI------------------------------------------*
041400900418     C                   DO        50            X                 2 0
041500900418     C                   MOVE      TCU(X)        TCUDS
041600941005     C     F56           IFEQ      'CG'
041700941005     C     F4            ANDEQ     '1'
041800941005     C     F3            ANDEQ     '0'
041900941005     C                   Z-ADD     KCU(X)        KCI               4 0
042000941005     C                   END
042100900418     C                   END
042200900418     C*---------------------------------------------------------------*
042300941006     C                   MOVEL     'IN PART'     V1CGES
042400941006     C                   MOVE      'ENZA'        V1CGES
042500900418     C                   MOVEL     REC80         CNCR80
042600020506     C     SIMTPP        IFEQ      '2'
042700020506     C     SIMTPP        OREQ      *BLANKS
042800020506     C                   MOVE      SIMPOU        FILIA             3 0
042900921201     C                   ELSE
043000921201     C                   MOVEL     SIMFEL        FILIA
043100921201     C                   END
043200900419     C*                                                    *
043300901219     C* GIRO UDATE
043400941005     C                   TIME                    WHHDAT           14 0
043500941005     C                   MOVE      WHHDAT        G02DAT
043600941005     C                   MOVE      *ZEROS        G02INV
043700941005     C                   MOVE      ' '           G02ERR
043800941005     C                   CALL      'XSRDA8'
043900941005     C                   PARM                    WLBDA8
044000941005     C                   MOVE      G02INV        AMGUDT
044100900418     C*  KLIST
044200900418     C     KGIA          KLIST
044300941005     C                   KFLD                    KAGC
044400941005     C                   KFLD                    KFGC
044500941005     C                   KFLD                    KNGC
044600911022     C                   KFLD                    §FRG
044700911122     C                   Z-ADD     0             §FRG
044800911022     C*
044900911022     C     KGIP          KLIST
045000941005     C                   KFLD                    KAGC
045100941005     C                   KFLD                    KFGC
045200941005     C                   KFLD                    KNGC
045300900418     C*
045400000201     C     KBOLW         KLIST
045500000201     C                   KFLD                    GCPASW
045600000201     C                   KFLD                    GCPLPW
045700000201     C                   KFLD                    GCPNSW
045800000201     C                   KFLD                    GCPNBW
045900000201     C     KBOL          KLIST
046000000201     C                   KFLD                    GCPAAS
046100000201     C                   KFLD                    GCPLNP
046200000201     C                   KFLD                    GCPNRS
046300000201     C                   KFLD                    GCPNSP
046400990908     C     KBLP          KLIST
046500990908     C                   KFLD                    KAAS
046600990908     C                   KFLD                    KLNP
046700990908     C                   KFLD                    KNRS
046800990908     C                   KFLD                    KNSP
046900990930     C     KAR6          KLIST
047000990908     C                   KFLD                    KAAS
047100990908     C                   KFLD                    KLNP
047200990908     C                   KFLD                    KNRS
047300990908     C                   KFLD                    KNSP
047400990930     C                   KFLD                    KTRC
047500941006     C*
047600941006     C     KBOL4         KLIST
047700941006     C                   KFLD                    GCPAAS
047800941006     C                   KFLD                    GCPLNP
047900941006     C                   KFLD                    GCPNRS
048000941006     C                   KFLD                    GCPNSP
048100941006     C                   KFLD                    KTRC              1
048200040325     C                   MOVEL     'D'           KTRC
048300900420     C*
048400900418     C     KTAB          KLIST
048500900418     C                   KFLD                    CODUT
048600900418     C                   KFLD                    TBLCOD
048700900418     C                   KFLD                    TBLKEY
048800991027     C     KTBE          KLIST
048900991027     C                   KFLD                    TBECOD
049000991027     C                   KFLD                    TBEKE1
049100991027     C                   KFLD                    TBEKE2
049200900418     C*
049300930528     C*
049400930528     C     KTABK         KLIST
049500930528     C                   KFLD                    CODUT
049600930528     C                   KFLD                    TBLCOD
049700900418     C*
049800900418     C     KACO          KLIST
049900900418     C                   KFLD                    CODUT
050000900418     C                   KFLD                    KCI
050100900427     C                   KFLD                    §KSC
050200941006     C*
050300941006     C     KTGC          KLIST
050400020221     C                   KFLD                    KKSC              7 0
050500020221     C                   KFLD                    CODCTR            3 0
050600941006     C                   KFLD                    KPRG
050700030206
050800030206     c     kFiar5        Klist
050900030822     c                   Kfld                    BLPAAS
051000030822     c                   Kfld                    BLPLNP
051100030822     c                   Kfld                    BLPNRS
051200030822     c                   Kfld                    BLPNSP
051300030206     c                   Kfld                    kAr5Trd
051400911021     C*
051500900427     C     *LIKE         DEFINE    ACOKSC        §KSC
051600950120     C     *LIKE         DEFINE    VSGF          SAVSGF
051700950120     C     *LIKE         DEFINE    VGGS          SAVGGS
051800941005     C     *LIKE         DEFINE    VKSC          SAVKSC
051900941005     C     *LIKE         DEFINE    VKSC          SA2KSC
052000900524     C     *LIKE         DEFINE    VRSDB         SAVRSB
052100900524     C     *LIKE         DEFINE    VINDB         SAVINB
052200941006     C     *LIKE         DEFINE    VLODB         SAVLOB
052300941006     C     *LIKE         DEFINE    V3CNZM        SAVNZM
052400900524     C     *LIKE         DEFINE    VPRDB         SAVPRB
052500900524     C     *LIKE         DEFINE    VCADB         SAVCAB
052600911022     C     *LIKE         DEFINE    GCPFRG        §FRG
052700900424     C     *LIKE         DEFINE    GCPDDM        AMGDCG
052800900424     C     *LIKE         DEFINE    GCPDDM        AMGDSD
052900900424     C     *LIKE         DEFINE    GCPDDM        AMGDSA
053000900424     C     *LIKE         DEFINE    G02TGI        §GGDA
053100900424     C     *LIKE         DEFINE    G02TGI        §GGA
053200911018     C     *LIKE         DEFINE    GCPKSC        KEYTGC
053300900424     C     *LIKE         DEFINE    TGCSGP        §SGR
053400100708     C     *LIKE         DEFINE    TGCSGv        §SGv
053500900427     C     *LIKE         DEFINE    GCPGGS        §GGPAG
053600901006     C     *LIKE         DEFINE    GCPGGS        §GGPAR
053700900426     C     *LIKE         DEFINE    GCPGGS        §GGSOS
053800900510     C     *LIKE         DEFINE    GCPGGS        §GGSAV
053900900426     C     *LIKE         DEFINE    GCPFRG        §NRG
054000031027     C     *LIKE         DEFINE    GCPFRG        §NRGDoss
054100941005     C     *LIKE         DEFINE    GCPCAS        §ASS
054200941005     C     *LIKE         DEFINE    GCPCAS        VECASS
054300941005     C     *LIKE         DEFINE    GCPCAS        NEWASS
054400941006     C     *LIKE         DEFINE    VRCA          EXRCA
054500900426     C     *LIKE         DEFINE    TBLCOD        §COD
054600900426     C     *LIKE         DEFINE    TBLKEY        §KEY
054700941005     C     *LIKE         DEFINE    GCPDDM        AMGUDT
054800941005     C     *LIKE         DEFINE    GCPAGC        KAGC
054900941005     C     *LIKE         DEFINE    GCPFGC        KFGC
055000941005     C     *LIKE         DEFINE    GCPNGC        KNGC
055100941006     C     *LIKE         DEFINE    TGCPRG        KPRG
055200990908     C     *LIKE         DEFINE    BLPAAS        KAAS
055300990908     C     *LIKE         DEFINE    BLPLNP        KLNP
055400990908     C     *LIKE         DEFINE    BLPNRS        KNRS
055500990908     C     *LIKE         DEFINE    BLPNSP        KNSP
055600990908     C                   MOVEL     *BLANKS       §DES             60
055700911123     C                   MOVE      *ZEROS        §GG               2 0
055800991027     C*
055900991027     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
056000991027     C                   MOVEL     'GED'         TBECOD
056100991027     C                   MOVEL     *BLANKS       TBEKE1
056200991027     C                   MOVEL     *BLANKS       TBEKE2
056300991027     C                   MOVEL     '1'           TBEKE1
056400991027     C     KTBE          CHAIN     TNTBE01L                           21
056500991027     C  N21              MOVEL     TBEUNI        DGED
056600030821     C* ----------------------------
056700030821     C* IMPOSTA LA TABELLA DEI tipi record di ar5 da duplicare
056800030821     C* ----------------------------
056900030821     C                   MOVEL     'AR5'         TBECOD
057000030821     C                   Z-ADD     0             bb                3 0
057100030821     c                   clear                   ar5
057200030821     C     TBECOD        setll     TNTBE01L
057300030821     C                   DO        *hival
057400030821     C     TBECOD        READE     TNTBE01L
057500030821     c                   if        %eof(tntbe01l)
057600030821     c                   leave
057700030821     c                   end
057800030821     C     TBEATB        IFEQ      *BLANK
057900030821     C                   MOVEL     TBEUNI        DAR5
058000030821     c* aggiungo nella schiera AR5 i tipi record da duplicare per le nuove
058100030821     c* bolle di recupero
058200030821     c                   if        §ar5drec = 'S'
058300030821     C                   ADD       1             bb
058400030821     C                   MOVEL     tbeke1        ar5(bb)
058500030821     C                   END
058600030821     C                   END
058700030821     C*
058800030821     C                   ENDDO
058900031027     C*
059000031027     C* CARICO Tabella 2F Tipo comunicazione Giacenza
059100031027     C                   clear                   x2f               3 0
059200031027     C                   MOVEL     '2F'          TBLCOD
059300031027     C                   MOVEL     *BLANK        TBLKEY
059400031027     C     KTABK         CHAIN     TABEL                              21
059500031027     C     *IN21         DOWEQ     *Off
059600031027     C     x2f           ANDLT     50
059700031027     C     TBLFLG        IFEQ      *BLANK
059800031027     C                   MOVEL     TBLUNI        ds2F
059900031027     C                   ADD       1             x2f
060000031027     C                   MOVEL     TBLKEY        C2F(x2f)
060100031027     C                   MOVEL     §2FDES        D2F(x2f)
060200031027     C                   MOVEL     §2FFTM        F2F(x2f)
060300031027     C                   MOVEL     §2FFTP        T2F(x2f)
060400031027     C                   MOVEL     §2Fris        R2F(x2f)
060500031027     C                   END
060600031027     C     KTABK         READE     TABEL                                  21
060700031027     C                   END
060800991027     C*
060900050125     C* CARICO Tabella 2z estensione 2a causali eventi
061000050125     C                   clear                   x2z               3 0
061100050125     C                   MOVEL     '2Z'          TBLCOD
061200050125     C                   MOVEL     *BLANK        TBLKEY
061300050125     C     KTABK         CHAIN     TABEL                              21
061400050125     C     *IN21         DOWEQ     *Off
061500080903     C     x2Z           ANDLT     200
061600050125     C     TBLFLG        IFEQ      *BLANK
061700050125     C                   MOVEL     TBLUNI        ds2z
061800050125     C                   ADD       1             x2z
061900050125     C                   MOVEL     TBLKEY        C2z(x2z)
062000050125     C                   MOVEL     §2znsr        nsr(x2z)
062100050125     C                   END
062200050125     C     KTABK         READE     TABEL                                  21
062300050125     C                   END
062400991027     C* CARICO TIPI BOLLA
062500991027     C                   MOVEL     '3A'          TBLCOD
062600991027     C                   MOVEL     *BLANK        TBLKEY
062700991027     C     KTABK         CHAIN     TABEL                              21
062800900427     C*
062900930527     C* CARICO TIPI BOLLA
063000930527     C                   MOVEL     '3A'          TBLCOD
063100930527     C                   MOVEL     *BLANK        TBLKEY
063200941005     C     KTABK         CHAIN     TABEL                              21
063300941005     C     *IN21         DOWEQ     '0'
063400941005     C     TBLFLG        IFEQ      *BLANK
063500930527     C                   MOVEL     TBLUNI        DS3A
063600930527     C                   MOVEL     §3AFCA        NWFCA
063700930527     C                   MOVEL     §3AFAN        NWFAN
063800930527     C                   MOVEL     §3AFTD        NWFTD
063900930527     C                   ADD       1             §F
064000930527     C                   MOVEL     NWFGB         FGB(§F)
064100950731     C                   MOVEL     §3ATB1        TB1(§F)
064200930527     C                   MOVEL     TBLKEY        CDB(§F)
064300930527     C                   END
064400941005     C     KTABK         READE     TABEL                                  21
064500930527     C                   END
064600941006     C*
064700941006     C* Carico tipi comunicazione fine gaicenza
064800941006     C     D82TRI        IFNE      'D'
064900941006     C                   Z-ADD     0             §F
065000941006     C                   MOVEL     '2H'          TBLCOD
065100941006     C                   MOVEL     *BLANK        TBLKEY
065200941006     C     KTABK         CHAIN     TABEL                              21
065300941006     C     *IN21         DOWEQ     '0'
065400941006     C     §F            ANDLE     50
065500941006     C     TBLFLG        IFEQ      *BLANK
065600941006     C                   ADD       1             §F
065700941006     C                   MOVEL     TBLUNI        D2H(§F)
065800941006     C                   MOVEL     TBLKEY        C2H(§F)
065900941006     C                   END
066000941006     C     KTABK         READE     TABEL                                  21
066100941006     C                   END
066200941006     C                   END
066300941006     C*
066400941006     C* Carico CAMBI/DIVISE
066500941006     C                   Z-ADD     0             §F
066600941006     C                   MOVEL     'CV'          TBLCOD
066700941006     C                   MOVEL     *BLANK        TBLKEY
066800941006     C     KTABK         CHAIN     TABEL                              21
066900941006     C     *IN21         DOWEQ     '0'
067000941006     C     §F            ANDLE     100
067100941006     C     TBLFLG        IFEQ      *BLANK
067200941006     C                   ADD       1             §F
067300941006     C                   MOVEL     TBLUNI        DSCV
067400941006     C                   MOVEL     TBLKEY        CCV(§F)
067500990716     C                   MOVEL     §CVFDC        FDC(§F)
067600941006     C                   END
067700941006     C     KTABK         READE     TABEL                                  21
067800941006     C                   END
067900941222     C*
068000941222     C* Carico CAMBI/DIVISE
068100941222     C                   Z-ADD     0             §F
068200941222     C                   MOVEL     '15'          TBLCOD
068300941222     C                   MOVEL     *BLANK        TBLKEY
068400941222     C     KTABK         CHAIN     TABEL                              21
068500941222     C     *IN21         DOWEQ     '0'
068600011220     C     §F            ANDLE     500
068700941222     C     TBLFLG        IFEQ      *BLANK
068800941222     C                   ADD       1             §F
068900941222     C                   MOVEL     TBLUNI        DS15
069000941222     C                   MOVEL     TBLKEY        CNZ(§F)
069100941222     C                   MOVEL     §15ITA        ITA(§F)
069200941222     C                   END
069300941222     C     KTABK         READE     TABEL                                  21
069400941222     C                   END
069500960130     C*
069600960130     C* Carico TIPI INCASSO
069700960130     C                   Z-ADD     0             §F
069800960130     C                   MOVEL     '1A'          TBLCOD
069900960130     C                   MOVEL     *BLANK        TBLKEY
070000960130     C     KTABK         CHAIN     TABEL                              21
070100960130     C     *IN21         DOWEQ     '0'
070200960130     C     §F            ANDLE     100
070300960130     C     TBLFLG        IFEQ      *BLANK
070400960130     C                   ADD       1             §F
070500960130     C                   MOVEL     TBLUNI        DS1A
070600960130     C                   MOVEL     TBLKEY        C1A(§F)
070700960130     C                   MOVEL     §1AFMB        T1A(§F)
070800960130     C                   END
070900960130     C     KTABK         READE     TABEL                                  21
071000960130     C                   END
071100040122     C* CARICO DATI DELLE VARIE CON FLG SOGGETTE O NO TOTALE IMP.
071200040122     C                   MOVEL     'CC'          TBLCOD
071300040122     C                   MOVEL     *BLANK        TBLKEY
071400040122     C                   CLEAR                   IPB
071500040122     C                   MOVEA     *BLANKS       CCC
071600040122     C     KTABk         CHAIN     TABEL00F                           21
071700040122     C                   Z-ADD     1             XX                3 0
071800040122     C     *IN21         DOWEQ     '0'
071900040122     C     XX            ANDLE     100
072000040122     C* Controllo se sto esaminando un rcd di una varia
072100040122     C                   MOVEL     TBLKEY        WCOMO5            5
072200040122     C     WCOMO5        IFEQ      'VARIE'
072300040122     C                   MOVEL     TBLUNI        DSCC
072400040122     C                   MOVE      §CCFL6        IPB(XX)
072500040122     C                   MOVE      TBLKEY        CCC(XX)
072600040122     C                   ADD       1             XX
072700040122     C                   END
072800040123     C     KTABk         READE     TABEL00F                               21
072900040122     C                   END
073000941005     C* PRENDO I PARAMETRI DI DEFAULT DA TABELLA 2G
073100941005     C                   MOVEL     '2G'          TBLCOD
073200941005     C                   MOVEL     *BLANK        TBLKEY
073300941005     C                   MOVEL     '1'           TBLKEY
073400941005     C                   CLEAR                   DS2G
073500941005     C     KTAB          CHAIN     TABEL                              21
073600941005     C  N21              MOVEL     TBLUNI        DS2G
073700950210     C* PRENDO I PARAMETRI DI DEFAULT DA TABELLA $2
073800950210     C                   MOVEL     '$2'          TBLCOD
073900950210     C                   MOVEL     *BLANK        TBLKEY
074000950210     C                   MOVEL     '1'           TBLKEY
074100950210     C                   CLEAR                   DS$2
074200950210     C     KTAB          CHAIN     TABEL                              21
074300950210     C  N21              MOVEL     TBLUNI        DS$2
074400941005     C* Controllo se devo eseguire pgm.
074500050316     C     KGIA          CHAIN(e)  TIGCP02L                           40
074600050316     c                   if        %error
074700070521     C     KGIA          CHAIN(n)  TIGCP02L
074800050316     c                   exsr      geslck
074900050318     C     KGIA          CHAIN(e)  TIGCP02L                           40
075000050316     c                   endif
075100941005     C   40              GOTO      FINE
075200941005     C*
075300941005     C* Prendo dati della bolla
075400060213     C     KBOL4         CHAIN     Fiar401L                           43
075500060213     C                   MOVEL     ar4NOT        RD2              35
075600990930     C     KBOL          CHAIN     FNBLP01L                           43
075700990908     C                   MOVE      BLPRMN        VRIF             15 0
075800941005     C     *IN43         IFEQ      '1'
075900941005     C     BLPATB        OREQ      '*'
076000941005     C                   GOTO      FINE
076100941005     C                   END
076200990930     C* Controllo se la bolla è in porto franco
076300990930     C                   Z-ADD     1             Y
076400990930     C     BLPCBO        LOOKUP    CDB(Y)                                 32
076500990930     C     *IN32         IFEQ      '1'
076600990930     C                   MOVEL     TB1(Y)        WPORTO            1
076700990930     C     WPORTO        IFEQ      'F'
076800990930     C                   MOVEL     '1'           KTRC
076900990930     C     KAR6          CHAIN     FIAR601L                           43
077000990930     C                   END
077100990930     C                   END
077200941005     C*
077300941005     C* Carico dati tariffe
077400941006     C                   MOVEL     *BLANKS       TBLKEY
077500941005     C                   CLEAR                   DSQT1
077600941006     C                   MOVEL     '1'           TBLKEY
077700941006     C                   MOVEL     'QT'          TBLCOD
077800941005     C     KTAB          CHAIN     TABEL                              15
077900941005     C  N15              MOVEL     TBLUNI        DSQT1
078000941006     C*
078100941006     C* FINTO OPEN X RENDERE SHR YES  I FILE
078200941223     C                   OPEN      FNLV22P
078300941006     C   LR
078400941006     CANNLR              EXCEPT    FINTO
078500941006     C*
078600900418     C                   ENDSR
078700900426     C/SPACE 3
078800900418     C*---------------------------------------------------------------*
078900900418     C*   G C P V I D                                                 *
079000900418     C*---------------------------------------------------------------*
079100900418     C     GCPVID        BEGSR
079200911028     C*
079300990930     C* Azzero campo divisa tassazione
079400990930     C                   MOVEL     *BLANKS       TASDIV
079500050221     C     GCPriap       COMP      'R'                                    42
079600911028     C* SE GIA FATTI ADDEBITI SU QUELLA GIACENZA ULTERIORI
079700911028     C* ADDEBITI DEVONO ESSERE FATTI A MANO
079800911028     C     GCPNBW        IFNE      *ZEROS
079900950221     C     GCPNBW        ANDNE     *ALL'9'
080000911028     C                   SETON                                        77
080100911028     C                   END
080200911028     C*
080300990908     C                   Z-ADD     0             WTOBOL           13 3
080400990908     C                   Z-ADD     0             WSVTOT           13 3
080500900419     C* DATA APERTURA GIACENZA
080600941005     C                   MOVE      GCPMGC        G02INV
080700941005     C                   MOVEL     GCPAGC        G02INV
080800941005     C                   Z-ADD     0             G02DAT
080900941005     C                   MOVEL     '3'           G02ERR
081000941005     C                   CALL      'XSRDA8'
081100941005     C                   PARM                    WLBDA8
081200941005     C                   MOVE      G02DAT        VDTG
081300900419     C                   MOVE      GCPNGC        VNGC
081400911109     C                   MOVE      GCPFGC        VFGC
081500971112     C* AGGANCIO AZORG LNP X SAPERE SE FILIALE ESTERA
081600971112     C                   Z-ADD     GCPLNP        ORGFIL
081700971112     C                   MOVEL     *BLANKS       GIAEST            1
081800020221     C                   MOVE      *BLANK        NETWRK            3
081900971112     C     ORGFIL        CHAIN     AZORG01L                           21
082000971112     C  N21ORGFVA        COMP      '*'                                    21
082100020221     C  N21              MOVEL     ORGDE3        OG143
082200020221     C  N21              MOVE      §OGNTW        NETWRK
082300020717      * sostituito con il campo network il vecchio flag estero
082400020717     C  N21netwrk        ifeq      'FED'
082500020717     C     netwrk        oreq      'EEX'
082600020717     C     netwrk        oreq      'EUP'
082700020717     C                   MOVEL     'E'           GIAEST            1
082800020717     c                   end
082900020717      *
083000900419     C* FIL. CHE HA APERTO LA GIACENZA
083100900509     C     VFGC          IFNE      *ZEROS
083200941005     C                   Z-ADD     VFGC          ORGFIL
083300941005     C                   MOVEL     *ALL'?'       DFGC
083400941005     C     ORGFIL        CHAIN     AZORG01L                           21
083500941005     C  N21ORGFVA        COMP      '*'                                    21
083600900419     C  N21              MOVEL     ORGDES        DFGC
083700900419     C                   END
083800900419     C*
083900900419     C* DATA ULTIMA RIAPERTURA
084000050221     c                   if        gcpriap = 'R'
084100941005     C                   MOVE      GCPDUR        G02INV
084200941005     C                   Z-ADD     0             G02DAT
084300941005     C                   MOVEL     '3'           G02ERR
084400941005     C                   CALL      'XSRDA8'
084500941005     C                   PARM                    WLBDA8
084600941006     C                   MOVE      G02DAT        VDIG
084700050221     c                   else
084800050221     C                   z-add     0             VDIG
084900050221     c                   end
085000941006     C* DATA DISPOSIZIONI
085100941006     C                   MOVE      GCPDDM        G02INV
085200941005     C                   Z-ADD     0             G02DAT
085300941005     C                   MOVEL     '3'           G02ERR
085400941005     C                   CALL      'XSRDA8'
085500941005     C                   PARM                    WLBDA8
085600941006     C                   MOVE      G02DAT        VDDM
085700941006     C* DATA ESECUZIONE DISPOSIZIONI
085800941006     C                   MOVE      GCPDED        G02INV
085900941006     C                   Z-ADD     0             G02DAT
086000941006     C                   MOVEL     '3'           G02ERR
086100941006     C                   CALL      'XSRDA8'
086200941006     C                   PARM                    WLBDA8
086300941006     C                   MOVE      G02DAT        VDED              8 0
086400911018     C* CAMPI X BOLLA DI ADDEBITO
086500900423     C* ---------------------------
086600911022     C* CLIENTE A CUI FATTURARE  (DALLA BOLLA)   SE NON HO GIA FATTO ADDEBITI
086700911022     C     *IN77         IFEQ      '0'
086800911018     C     BLPCCM        IFNE      *ZEROS
086900911018     C                   MOVE      BLPCCM        VKSC
087000911018     C                   ELSE
087100911018     C                   MOVE      BLPKSC        VKSC
087200911018     C                   END
087300091103      * forza codice cliente giacenza se spedizione triangolo
087400091103     c                   exsr      forzacli
087500911021     C                   ELSE
087600911212     C     GCPKSC        IFNE      *ZEROS
087700911021     C                   MOVE      GCPKSC        VKSC
087800911212     C                   END
087900911021     C                   END
088000900423     C                   MOVE      VKSC          §QUATR            4
088100960314     C                   MOVE      GCPSCM        WSCM              7 0
088200900619     C*
088300900619     C*
088400900423     C     §QUATR        IFNE      *ALL'8'
088500900524     C     §QUATR        ANDNE     *ALL'9'
088600911213     C     VKSC          ANDNE     *ZEROS
088700900427     C                   MOVE      VKSC          §KSC
088800900423     C     KACO          CHAIN     CNACO000                           21
088900941006     C  N21              MOVEL     ACORAG        VRSDB
089000941006     C   21              MOVEL     *ALL'?'       VRSDB
089100900423     C  N21ACOFLG        COMP      '*'                                    21
089200900423     C  N21ACOABL        COMP      '*'                                    21
089300941005     C  N21KACO          CHAIN     CNIND000                           21
089400941006     C   21              MOVEL     *ALL'?'       VINDB
089500941006     C   21              MOVEL     *ALL'?'       VLODB
089600941005     C     *IN21         IFEQ      '0'
089700941006     C                   MOVEL     *BLANKS       VINDB
089800941006     C                   MOVEL     INDVIA        VINDB
089900941006     C                   MOVEL     *BLANKS       VLODB
090000941006     C                   MOVEL     INDCIT        VLODB
090100941005     C                   MOVEL     INDPRV        VPRDB
090200050223     C                   MOVEL     INDCAe        VCADB
090300941005     C                   MOVEL     INDSTA        V3CNZM
090400941005     C                   END
090500900423     C                   ELSE
090600900523     C*IMPONGO 8888  IN VKSC
090700900523     C                   MOVEL     *ALL'8'       §QUATR
090800900523     C                   MOVE      §QUATR        VKSC
090900911018     C                   MOVEL     BLPRSM        VRSDB
091000911018     C                   MOVEL     BLPINM        VINDB
091100911018     C                   MOVEL     BLPLOM        VLODB
091200911018     C                   MOVEL     BLPPRM        VPRDB
091300911018     C                   MOVEL     BLPCAM        VCADB
091400941005     C                   MOVEL     BLPNZM        V3CNZM
091500900423     C                   END
091600911213     C* SE PAGA DESTIN.
091700970724     C                   MOVEL     BLPAAS        BLPDSP            8 0
091800911022     C                   MOVE      BLPMGS        BLPDSP
091900941018     C                   MOVEL     BLPAAS        DATDSP            8 0
092000941228     C                   MOVE      BLPMGS        DATDSP
092100910319     C* SALVO CAMPI CODICE CLIENTE
092200910319     C                   MOVE      VKSC          SAVKSC
092300910319     C                   MOVEL     VRSDB         SAVRSB
092400910319     C                   MOVEL     VINDB         SAVINB
092500910319     C                   MOVEL     VLODB         SAVLOB
092600910319     C                   MOVEL     VPRDB         SAVPRB
092700910319     C                   MOVEL     VCADB         SAVCAB
092800941005     C                   MOVEL     V3CNZM        SAVNZM
092900941222     C                   Z-ADD     1             Y                 3 0
093000950228     C     BLPNZD        LOOKUP    CNZ(Y)                                 32
093100900423     C* CODICE FINE  GIACENZA
093200900423     C     GCPCFG        IFNE      *BLANK
093300900423     C                   MOVEL     GCPCFG        VCFG
093400900423     C                   MOVEL     '2C'          TBLCOD
093500900423     C                   MOVEL     *BLANK        TBLKEY
093600900423     C                   MOVEL     VCFG          TBLKEY
093700900423     C     KTAB          CHAIN     TABEL                              21
093800900423     C   21              MOVEL     *ALL'?'       DCFG
093900060407     C   21              clear                   DS2C
094000900511     C  N21              MOVEL     TBLUNI        DS2C
094100900511     C  N21              MOVEL     §2CDES        DCFG
094200900423     C                   END
094300900423     C* DATA FINE GIACENZA
094400900423     C     GCPDCG        IFNE      *ZEROS
094500941005     C                   MOVE      GCPDCG        G02INV
094600941005     C                   Z-ADD     0             G02DAT
094700941005     C                   MOVEL     '3'           G02ERR
094800941005     C                   CALL      'XSRDA8'
094900941005     C                   PARM                    WLBDA8
095000941005     C                   MOVE      G02DAT        VDCG
095100900423     C                   END
095200911021     C* DATA SOSTA DAL
095300911021     C     GCPDSD        IFNE      *ZEROS
095400950203     C                   MOVE      GCPDSD        G02INV
095500950203     C                   Z-ADD     0             G02DAT
095600941005     C                   MOVEL     '3'           G02ERR
095700941005     C                   CALL      'XSRDA8'
095800941005     C                   PARM                    WLBDA8
095900941005     C                   MOVE      G02DAT        VDSD
096000911021     C                   END
096100911021     C* DATA SOSTA  AL
096200911021     C     GCPDSA        IFNE      *ZEROS
096300941006     C                   MOVE      GCPDSA        G02INV
096400941005     C                   Z-ADD     0             G02DAT
096500941005     C                   MOVEL     '3'           G02ERR
096600941005     C                   CALL      'XSRDA8'
096700941005     C                   PARM                    WLBDA8
096800941005     C                   MOVE      G02DAT        VDSA
096900911021     C                   END
097000900423     C*
097100911021     C                   MOVE      GCPGGS        VGGS
097200950120     C                   MOVE      GCPGGS        §GGSAV
097300911021     C                   MOVE      GCPSGF        VSGF
097400911022     C                   MOVE      GCPSGS        VTSGS
097500911021     C                   MOVE      GCPSGR        VSGR
097600911021     C                   MOVE      GCPSGD        VSGD
097700911021     C                   MOVE      GCPSGA        VSGA
097800911021     C                   MOVE      GCPTFT        VTFT
097900990628     C                   MOVE      GCPDIV        V4CDIV
098000990628     C* SE C' E' BLANKS IMPOSTO LIRE A VIDEO
098100990628     C     GCPDIV        IFEQ      *BLANKS
098200991027     C                   MOVEL     §GEDCN        V4CDIV
098300990628     C                   END
098400911021     C*
098500911021     C                   MOVE      GCPASW        VASW
098600990908     C     GCPASW        IFEQ      0
098700990908     C                   MOVE      WHHDAT        VASW
098800990908     C                   END
098900911021     C                   MOVE      GCPNSW        VNSW
099000950221     C     GCPNBW        IFNE      *ALL'9'
099100911021     C                   MOVE      GCPNBW        VNBW
099200950221     C                   END
099300911022     C                   MOVE      GCPLPW        VLNW
099400000201     C* REPERISCO LA DATA DELLA BOLLA CHAINANDO LA SPEDIZIONE
099500000201     C                   Z-ADD     0             VDBG
099600000201     C     GCPNBW        IFNE      0
099700000201     C     KBOLW         CHAIN     FNBLP01L                           31
099800000201     C     *IN31         IFEQ      '0'
099900000201     C                   MOVEL     BLPAAS        G02INV
100000000201     C                   MOVE      BLPMGS        G02INV
100100941005     C                   Z-ADD     0             G02DAT
100200941005     C                   MOVEL     '3'           G02ERR
100300941005     C                   CALL      'XSRDA8'
100400941005     C                   PARM                    WLBDA8
100500941005     C                   MOVE      G02DAT        VDBG
100600000201     C* RIAGGANCIO LA BOLLA DELLA GIACENZA
100700000201     C     KBOL          CHAIN     FNBLP01L                           31
100800000201     C                   END
100900000201     C                   END
101000911022     C                   MOVE      GCPFIV        VFIV
101100911022     C                   MOVE      GCPNFT        VNFT
101200990908     C                   Z-ADD     0             AMGDFT            8 0
101300911022     C     GCPDFT        IFNE      *ZEROS
101400990908     C                   MOVE      GCPDFT        AMGDFT
101500990908     C                   MOVE      GCPDFT        G02INV
101600941005     C                   Z-ADD     0             G02DAT
101700941005     C                   MOVEL     '3'           G02ERR
101800941005     C                   CALL      'XSRDA8'
101900941005     C                   PARM                    WLBDA8
102000941005     C                   Z-ADD     G02DAT        VDFT
102100911022     C                   END
102200911024     C* GESTIONE RECUPERO TRASPORTO  SE CAMBIO PORTO = F
102300911024     C*                   O SE DIROTT. DI FRANCO SENZA CAMBIO PORTO
102400911021     C* ---------------------------
102500950731     C                   Z-ADD     1             §F
102600950731     C     BLPCBO        LOOKUP    CDB(§F)                                21
102700950731     C   21              MOVEL     TB1(§F)       EXPORT            1
102800951212     C   21              MOVEL     TB1(§F)       OLDTB1            2
102900960201     C     GCPDIS        IFEQ      'D'
103000970415     C     GCPDIS        OREQ      'C'
103100970415     C     GCPNPC        ANDEQ     'F'
103200960201     C     GCPNPC        IFEQ      ' '
103300960201     C     EXPORT        ANDEQ     'F'
103400960201     C     GCPNPC        OREQ      'F'
103500960130     C                   SETON                                        69
103600060811     c                   clear                   d82nodsp
103700911021     C                   END
103800960201     C                   END
103900050222     C*  SPESE TRASPORTO RECUPERATE IN AUTOMATICO solo x codificato
104000050222     C     §QUATR        IFNE      *ALL'8'
104100050222     C     §QUATR        ANDNE     *ALL'9'
104200960130     C     GCPDIS        IFEQ      'C'
104300960130     C     GCPNPC        IFEQ      'F'
104400960130     C     GCPNPC        OREQ      ' '
104500960130     C     TB1(§F)       ANDEQ     'FS'
104600960130     C                   SETON                                        73
104700960130     C                   END
104800960130     C                   END
104900050222     C                   END
105000911021     C* GESTIONE C/ASSEGNO
105100911021     C* ------------------
105200930527     C* CALCOLO PROV. C/A  SE LE PAGA IL MITTENTE
105300930527     C*                         ------------------
105400970415     C                   Z-ADD     0             NEWASS
105500920204     C                   MOVE      *ZEROS        VRCA
105600970415     C                   Z-ADD     0             VEXCAS
105700970415     C                   MOVEL     *BLANKS       VEXTIC
105800970415     C                   MOVEL     *BLANKS       VEXDIV
105900960130     C                   MOVE      *ZEROS        VRTR
106000051206      * prima di procedere al calcolo delle provvigioni verifico se la sped.
106100051206      * è già stata fatturata nel caso procedo con il calcolo per recuperarle
106200051206     c                   movel     kpjbu         wsvkpj
106300051206     c                   clear                   fnlg11ds
106400051206     c                   z-add     blpaas        I11AAS
106500051206     c                   z-add     blplnp        I11LNP
106600051206     c                   z-add     blpnrs        I11NRS
106700051206     c                   z-add     blpnsp        I11NSP
106800051206     c                   move      oldtb1        I11TBL
106900051206     c                   movel     fnlg11ds      kpjbu
107000051206     c                   call      'FNLG11R'
107100051206     C                   PARM                    KPJBA
107200051206     c                   movel     kpjbu         fnlg11ds
107300051206     c                   movel     wsvkpj        kpjbu
107400051206      * calcolo le provvigioni solo se già fatturato altrimenti verranno
107500051206      * calcolate al momento della fatturazione
107600051206 1   c                   if        o11fat = 'F'
107700051206     c
107800051206 2   C     GCPPPC        IFEQ      'M'
107900970415     C*
108000970415     C* GUARDO SE C'E' IL CONTRASSEGNO
108100970415     C                   Z-ADD     1             §F                3 0
108200970415     C     BLPCBO        LOOKUP    CDB(§F)                                21
108300970415     C   21              MOVEL     FGB(§F)       NWFGB
108400051206 3   C     NWFCA         IFNE      '0'
108500051115     C                   Z-ADD     0             ar9CAS
108600051115     C                   Z-ADD     0             ar9CMB
108700051115     C                   MOVEL     *BLANKS       ar9VCA
108800051115     C     KBOL          CHAIN     Fiar901L                           21
108900051206 e3  C                   END
109000970415     C*---------
109100970415     C* Se sto addebitando le provvigioni del C/Assegno a seguito di
109200970415     C* un dirottamento devo verificare in fase di addebito spese se
109300970415     C* sono state recuperate le provvigioni sul C/Assegno sulla
109400970415     C* vecchia bolla.
109500051206 3   C     GCPDIS        IFEQ      'D'
109600970415     C                   EXSR      RCADIR
109700051206e3   C                   END
109800970415     C*---------
109900051206 3   C     NEWASS        IFEQ      0
110000941006     C* GUARDO SE C'E' STATA VARIAZ. BOLLA X IL CONTRASSEGNO
110100051206     C     KBOL          CHAIN     FNARBK00                           21
110200051206 4   C     *IN21         IFEQ      '0'
110300950327     C                   CLEAR                   NWFGB
110400930527     C* MEMORIZZO IMPORTO C/ASS COME ERA E CHI PAGAVA
110500051206 5   C     ARBVCA        IFEQ      *BLANKS
110600000229     C     ARBVCA        OREQ      §GEDCN
110700941005     C                   Z-ADD     ARBCAS        VECASS
110800051206x5   C                   ELSE
110900011220     C* converto IMPORTO C/ASSEGNO COME ERA PRIMA DELLA VARIAZ.
111000010907     C                   CLEAR                   YEURDS
111100010907     C                   MOVEL     ARBVCA        YECDVI
111200010907     C                   Z-ADD     ARBCAS        YECIMI
111300010907     C                   MOVEL     §GEDCN        YECDVO
111400010907     C                   MOVEL     'H'           YECTAR
111500010907     C                   CALL      'YEURCO'
111600010907     C                   PARM                    YEURDS
111700010907     C                   Z-ADD     YECIMO        VECASS
111800010907      *SOSTITUITO CON CALCOLO TRAMITE YEURCO SOPRA
111900051206e5   C                   END
112000051206 5   C     ar9VCA        IFEQ      *BLANKS
112100051115     C     ar9VCA        OREQ      §GEDCN
112200051115     C                   Z-ADD     ar9CAS        NEWASS
112300051206x5   C                   ELSE
112400011220     C* converto IMPORTO C/ASSEGNO COME È ATTUALMENTE
112500010907     C                   CLEAR                   YEURDS
112600051115     C                   MOVEL     ar9VCA        YECDVI
112700051115     C                   Z-ADD     ar9CAS        YECIMI
112800010907     C                   MOVEL     §GEDCN        YECDVO
112900010907     C                   MOVEL     'H'           YECTAR
113000010907     C                   CALL      'YEURCO'
113100010907     C                   PARM                    YEURDS
113200010907     C                   Z-ADD     YECIMO        NEWASS
113300010907      *SOSTITUITO CON CALCOLO TRAMITE YEURCO SOPRA
113400051206e5   C                   END
113500980515     C                   Z-ADD     1             §F
113600941104     C     ARBCBO        LOOKUP    CDB(§F)                                21
113700930527     C   21              MOVEL     FGB(§F)       NWFGB
113800051206x4   C                   ELSE
113900930527     C* NON C'E' VARIAZ. BOLLA  IN PARTENZA
114000930527     C* MEMORIZZO IMPORTO C/ASS COME ERA E CHI PAGAVA  DA FLBLP
114100051206 5   C     GCPVCA        IFEQ      *BLANKS
114200000229     C     GCPVCA        OREQ      §GEDCN
114300941006     C                   Z-ADD     GCPCAS        NEWASS
114400051206x5   C                   ELSE
114500051206 6   C     GCPVCA        IFEQ      ar9VCA
114600011220     C* converto IMPORTO C/ASSEGNO MEMORIZZ. SULLA GIACENZA
114700010907     C                   CLEAR                   YEURDS
114800010907     C                   MOVEL     GCPVCA        YECDVI
114900010907     C                   Z-ADD     GCPCAS        YECIMI
115000010907     C                   MOVEL     §GEDCN        YECDVO
115100010907     C                   MOVEL     'H'           YECTAR
115200010907     C                   CALL      'YEURCO'
115300010907     C                   PARM                    YEURDS
115400010907     C                   Z-ADD     YECIMO        NEWASS
115500010907      *SOSTITUITO CON CALCOLO TRAMITE YEURCO SOPRA
115600051206x6   C                   ELSE
115700941006     C                   Z-ADD     1             §F
115800941006     C     GCPVCA        LOOKUP    CCV(§F)                                31
115900051206 7   C     *IN31         IFEQ      '1'
116000010907     C                   CLEAR                   YEURDS
116100010907     C                   MOVEL     GCPVCA        YECDVI
116200010907     C                   Z-ADD     GCPCAS        YECIMI
116300010907     C                   MOVEL     §GEDCN        YECDVO
116400010907     C                   MOVEL     'H'           YECTAR
116500010907     C                   CALL      'YEURCO'
116600010907     C                   PARM                    YEURDS
116700010907     C                   Z-ADD     YECIMO        NEWASS
116800010907      *SOSTITUITO CON CALCOLO TRAMITE YEURCO SOPRA
116900051206e7   C                   END
117000051206e6   C                   END
117100051206e5   C                   END
117200051206 5   C     ar9VCA        IFEQ      *BLANKS
117300051115     C     ar9VCA        OREQ      §GEDCN
117400051115     C                   Z-ADD     ar9CAS        VECASS
117500051206x5   C                   ELSE
117600010907     C                   CLEAR                   YEURDS
117700051115     C                   MOVEL     ar9VCA        YECDVI
117800051115     C                   Z-ADD     ar9CAS        YECIMI
117900010907     C                   MOVEL     §GEDCN        YECDVO
118000010907     C                   MOVEL     'H'           YECTAR
118100010907     C                   CALL      'YEURCO'
118200010907     C                   PARM                    YEURDS
118300010907     C                   Z-ADD     YECIMO        VECASS
118400010907      *SOSTITUITO CON CALCOLO TRAMITE YEURCO SOPRA
118500051206e5   C                   END
118600051206e4   C                   END
118700970415     C*---------
118800970415     C* Altrimenti calcolo la differenza fra nuovo e vecchio C/Ass
118900051206 4   C     NWFCA         IFEQ      '0'
119000930528     C     NWFCA         OREQ      '2'
119100960603     C     NWFCA         OREQ      '1'
119200960603     C     EXPORT        ANDEQ     'A'
119300930527     C                   Z-ADD     NEWASS        §ASS
119400051206 5   C     NEWASS        IFEQ      0
119500960603     C                   Z-ADD     VECASS        §ASS
119600051206e5   C                   END
119700930527     C                   EXSR      CPROVV
119800051206x4   C                   ELSE
119900051206 5   C     NWFCA         IFEQ      '1'
120000930527     C                   MOVE      *ZEROS        EXRCA
120100930527     C                   Z-ADD     VECASS        §ASS
120200930527     C                   EXSR      CPROVV
120300930527     C                   MOVE      VRCA          EXRCA
120400930527     C                   Z-ADD     NEWASS        §ASS
120500930527     C                   EXSR      CPROVV
120600930527     C     VRCA          SUB       EXRCA         VRCA
120700051206e5   C                   END
120800051206e4   C                   END
120900051206x3   C                   ELSE
1210000512064    C     NEWASS        IFNE      0
121100970415     C                   SETON                                        79
121200970416     C                   Z-ADD     NEWASS        VEXCAS
121300970415     C                   MOVEL     ARBTIC        VEXTIC
121400970416     C                   MOVEL     WVCA          VEXDIV
121500051206e4   C                   END
121600051206e3   C                   END
121700051206e2   C                   END
121800051206      * fine calcolo provvigioni se fatturato
121900051206e1   c                   endif
122000930527     C*---------
122100970415     C  N79VRCA          IFGT      0
122200920204     C                   SETON                                        78
122300920204     C                   END
122400970415     C*
122500040112     c                   clear                   ds7w
122600040112     c                   if        blpgga <>*blank
122700040112     c                   move      '7W'          tblcod
122800040112     c                   movel     *blank        tblkey
122900040112     c                   movel     blpgga        tblkey
123000040112     c     ktab          chain     tabel00f
123100040112     c                   if        %found(tabel00f)
123200040112     c                   movel     tbluni        ds7w
123300040112     c                   endif
123400040112     c                   endif
123500960130     C     GCPDIS        COMP      'R'                                    74
123600960130     C     GCPDIS        IFEQ      'R'
123700040112     C     gcpdis        andne     §7w7t1
123800920211     C     GCPPSG        ORNE      'M'
123900960430     C     *IN77         OREQ      '1'
124000990305     C                   MOVE      '9'           VTFT
124100920211     C                   END
124200990305     C* SE DEVO CHIUDERE PER APERTURA C.A. NON DEVO CALCOLARE LE SPESE
124300990305     C* DI GIACENZA E IMPOSTO '9' IN VTFT
124400990305     C     GCPCFG        IFEQ      '015'
124500990305     C                   MOVE      '9'           VTFT
124600990305     C                   SETON                                        75
124700990305     C                   END
124800000922     C* SE TROVO FLAG IMPOSTATI IN TABELLA 2F PER FORZARE NON SPESE GIAC.
124900000922     C*  IMPOSTO '9' IN VTFT
125000000922     C                   CLEAR                   DS2F
125100000922     C                   MOVE      *BLANKS       TBLKEY
125200000922     C                   MOVEL     GCPTCM        TBLKEY
125300000922     C                   MOVEL     '2F'          TBLCOD
125400000922     C     KTAB          CHAIN     TABEL00F                           99
125500000922     C     *IN99         IFEQ      '0'
125600000922     C                   MOVEL     TBLUNI        DS2F
125700000922     C     §2FFTM        IFEQ      'N'
125800000922     C     §2FVAG        ANDEQ     'N'
125900040120     C     §2FRIS        ANDNE     'N'
126000000922     C                   MOVE      '9'           VTFT
126100000922     C                   SETON                                        75
126200000922     C                   END
126300000922     C                   END
126400960308     C* SE DEVO RECUPERARE TRASPORTO O PROVV. C/A (NON PER I RESI!)
126500990305     C     *IN74         IFEQ      '0'
126600990305     C     *IN75         ANDEQ     '0'
126700990305     C     *IN78         IFEQ      '1'
126800990305     C     *IN79         OREQ      '1'
126900990305     C     *IN69         OREQ      '1'
127000990305     C                   MOVEL     '0'           VTFT
127100990305     C                   END
127200990305     C                   END
127300990305     C*
127400900426     C                   ENDSR
127500091103     C*---------------------------------------------------------------*
127600091103     C* verifica se cod.cliente da forzare
127700091103     C*---------------------------------------------------------------*
127800091103     C     forzacli      BEGSR
127900091103     C                   move      'GEN'         KAR5TRD
128000091103     C     kfiar5        chain     fiar501l
128100091103     c                   if        %found(fiar501l)
128200091103     c                   movel     ar5uni        dar5gen
128300091103     c                   if        §ar5ass = 'T'
128400091103     c                   clear                   tibs02ds
128500091103     c                   movel     'C'           t02mod
128600091103     c                   movel     knsif         t02sif
128700091103     c                   movel     'CLI'         t02cod
128800091103     c                   movel     gcpscm        t02ke1
128900091103     c                   clear                   t02ke2
129000091103     c                   call      'TIBS02R'
129100091103     c                   parm                    KPJBA
129200091103     c                   parm                    TIBS02DS
129300091103     c                   if        t02err = *blanks
129400091103     c                   movel     t02uni        dcli
129500091103     c                   if        §cligcp = 'S'
129600091103     c                   movel     gcpscm        vksc
129700091103     c                   endif
129800091103     c                   endif
129900091103     c                   endif
130000091103     c                   endif
130100091103     c
130200091103     c                   endsr
130300970415     C*---------------------------------------------------------------*
130400970415     C* Calcolo provvigioni c/assegno al mittente in caso di dirottam.*
130500970415     C*---------------------------------------------------------------*
130600970415     C     RCADIR        BEGSR
130700970415     C*
130800970415     C* In fase di esecuzione disposizioni di dirottamento il C/Assegno
130900970415     C* viene normalmente spostato dalla bolla originale alla bolla
131000970415     C* figlia. Se sono in fase di conferma chiusura giacenza in partenza,
131100970416     C* e sulla bolla in partenza l'importo del C/Assegno è a zero e non
131200970416     C* è stata effettuata una variazione di importo da giacenza, devo
131300970415     C* cercare di risalire all'importo del contrassegno con cui sono
131400970415     C* andato in consegna (è sulla bolla figlia):
131500970415     C*   (Annullamento)
131600051115     C     ar9CAS        IFEQ      0
131700970415     C     GCPFAS        ANDEQ     40
131800970416     C*
131900970416     C     GCPVCS        IFNE      'S'
132000970416     C                   MOVEL     ARBVCA        WVCA
132100970415     C*   mi posiziono sull'ultima variazione del C/Assegno e
132200970415     C*   considero come importo finale quello ivi indicato
132300970415     C     KBOL          SETGT     FNARBK00
132400970415     C     KBOL          READPE    FNARBK00                               21
132500970415     C     *IN21         IFEQ      '0'
132600970415     C                   CLEAR                   NWFGB
132700970415     C     ARBCBO        LOOKUP    CDB(§F)                                21
132800970415     C   21              MOVEL     FGB(§F)       NWFGB
132900970415     C     ARBVCA        IFEQ      *BLANKS
133000000229     C     ARBVCA        OREQ      §GEDCN
133100970415     C                   Z-ADD     ARBCAS        NEWASS
133200970415     C                   ELSE
133300010907     C                   CLEAR                   YEURDS
133400010907     C                   MOVEL     ARBVCA        YECDVI
133500010907     C                   Z-ADD     ARBCAS        YECIMI
133600010907     C                   MOVEL     §GEDCN        YECDVO
133700010907     C                   MOVEL     'H'           YECTAR
133800010907     C                   CALL      'YEURCO'
133900010907     C                   PARM                    YEURDS
134000010907     C                   Z-ADD     YECIMO        NEWASS
134100010907      *SOSTITUITO CON CALCOLO TRAMITE YEURCO SOPRA
134200970415     C                   END
134300970415     C                   END
134400970416     C* Controllo se è stata effettuata una variazione importo C/Ass
134500970416     C* da giacenza
134600970416     C                   ELSE
134700970416     C                   MOVEL     GCPVCA        WVCA              3
134800970416     C     GCPVCA        IFEQ      *BLANKS
134900000229     C     GCPVCA        OREQ      §GEDCN
135000970416     C                   Z-ADD     GCPCAS        NEWASS
135100970416     C                   ELSE
135200970416     C     GCPVCA        IFEQ      ARBVCA
135300010907     C                   CLEAR                   YEURDS
135400010907     C                   MOVEL     GCPVCA        YECDVI
135500010907     C                   Z-ADD     GCPCAS        YECIMI
135600010907     C                   MOVEL     §GEDCN        YECDVO
135700010907     C                   MOVEL     'H'           YECTAR
135800010907     C                   CALL      'YEURCO'
135900010907     C                   PARM                    YEURDS
136000010907     C                   Z-ADD     YECIMO        NEWASS
136100010907      *SOSTITUITO CON CALCOLO TRAMITE YEURCO SOPRA
136200970416     C                   ELSE
136300970416     C                   Z-ADD     1             §F
136400970416     C     GCPVCA        LOOKUP    CCV(§F)                                31
136500970416     C     *IN31         IFEQ      '1'
136600010907     C                   CLEAR                   YEURDS
136700010907     C                   MOVEL     GCPVCA        YECDVI
136800010907     C                   Z-ADD     GCPCAS        YECIMI
136900010907     C                   MOVEL     §GEDCN        YECDVO
137000010907     C                   MOVEL     'H'           YECTAR
137100010907     C                   CALL      'YEURCO'
137200010907     C                   PARM                    YEURDS
137300010907     C                   Z-ADD     YECIMO        NEWASS
137400010907      *SOSTITUITO CON CALCOLO TRAMITE YEURCO SOPRA
137500970416     C                   END
137600970416     C                   END
137700970416     C                   END
137800970416     C*
137900970416     C                   END
138000970415     C*-------
138100970415     C* Imposto 0 nel vecchio importo del C/Assegno in quanto non so
138200970415     C* se le provvigioni sul vecchio importo sono state addebitate
138300970415     C* al mittente oppure no (se l'annullamento del C/Assegno sulla
138400970415     C* bolla mamma è stato effettuato prima della fatturazione le
138500970415     C* provvigioni non sono state addebitate, se era già stata
138600970415     C* effettuata la contalizzazione le provvigioni erano state
138700970415     C* già recuperate)
138800970415     C                   Z-ADD     0             VECASS
138900970415     C                   END
139000970415     C*
139100970415     C                   ENDSR
139200970415     C*---------------------------------------------------------------*
139300941006     C*    CALCOLO PROVVIGIONI DEL MITTENTE                           *
139400941006     C*---------------------------------------------------------------*
139500911021     C     CPROVV        BEGSR
139600941006     C*
139700990930     C                   CLEAR                   DTAS
139800990930     C                   CLEAR                   DTASV
139900980515     C                   MOVEL     BLPCTR        TASCTR
140000980515     C                   MOVE      VKSC          TASKSC
140100980514     C*
140200980514     C                   MOVEL     BLPCTS        TASCTS
140300980514     C                   MOVEL     BLPCAD        TASCAD
140400980514     C                   MOVEL     BLPFIN        TASFIN
140500980514     C                   MOVEL     BLPCAM        TASCAM
140600990930     C                   MOVEL     BLPNZM        TASNZM
140700980514     C                   MOVEL     BLPFAP        TASFAP
140800980514     C                   MOVEL     BLPTSP        TASTSP
140900980514     C                   MOVEL     BLPTC1        TASTC1
141000980514     C                   MOVEL     BLPTC2        TASTC2
141100980514     C                   MOVEL     BLPFDN        TASFDN
141200980515     C                   MOVEL     BLPVAS        TASVAS
141300980514     C                   Z-ADD     BLPIAS        TASIAS
141400980514     C                   Z-ADD     BLPQFT        TASQFT
141500980514     C                   Z-ADD     BLPNCL        TASNCL
141600160530     C                   Z-ADD     BLPNCL        TASNCLb
141700980514     C                   Z-ADD     DATDSP        TASDSP
141800980514     C                   Z-ADD     TASDSP        TASDCT
141900980514     C                   Z-ADD     BLPLNP        TASLNP
142000980514     C                   Z-ADD     BLPLNA        TASLNA
142100980514     C                   Z-ADD     BLPVLF        TASVLF
142200980514     C                   Z-ADD     BLPPKF        TASPKF
142300160530     C                   Z-ADD     BLPvlc        TASvlc
142400160530     C                   Z-ADD     BLPncr        TASncr
142500160530     C                   Z-ADD     BLPpkb        TASPKb
142600160530     C                   move      BLPfvf        TASfvf
142700980514     C* Reperisco codice tassazione
142800980514     C                   CLEAR                   TISI95
142900980514     C                   MOVEL     '3'           I95TCN
143000980514     C                   MOVEL     BLPCAM        I95CAP
143100980514     C                   MOVEL     BLPNZM        I95NAR
143200980514     C                   MOVEL     BLPPRM        I95PRV
143300980514     C                   MOVEL     BLPLOM        I95LOC
143400980514     C                   MOVEL     BLPINM        I95IND
143500980514     C                   MOVEL     BLPTFP        I95TFP
143600980602     C                   MOVEL     BLPTSP        I95TSP
143700980602     C                   MOVEL     'S'           I95FRE
143800980515     C                   MOVEL     BLPNZM        I95NAR
143900980515     C                   MOVEL     DATDSP        I95DAT
144000020221      *SOLO PER FEDEX IMPOSTA IL FLAG = S
144100020221     C     NETWRK        IFEQ      'FED'
144200020221     C                   MOVE      'S'           I95FI1
144300020221     C                   ELSE
144400020221     C                   MOVE      *BLANK        I95FI1
144500020221     C                   END
144600980514     C                   CALL      'TISI95R'
144700980514     C                   PARM                    TISI95
144800980514     C                   MOVEL     O95CTS        TASMCT
144900980514     C*
145000051130     C                   MOVEL     §GEDCN        TASVCA
145100060927     C                   MOVEL     blpfbr        §asfbr
145200060927     C                   MOVEL     blpcca        §ascca
145300060927     C                   MOVEL     dtas01        TASFLO
145400980514     C                   Z-ADD     §ASS          TASCAS
145500051115     C                   MOVEL     ar9TIC        TASTIC
145600020930     c* imposto peso vdl e colli rilevati
145700030206     c                   z-add     blpncp        Tasncp
145800030206     c                   z-add     blppkc        Taspkc
145900030206      * Bancali
146000030206     c                   If        %Subst(BlpGva:1:1) = 'B'
146100030206     c                   Clear                   dAr5Ban
146200030805     C                   move      'BAN'         KAR5TRD
146300030206     c     kFiar5        Chain     Fiar501l
146400030206     c                   If        %Found(Fiar501l)
146500030206     c                   Movel     Ar5Uni        dAr5Ban
146600030206     c                   EndIf
146700030206     c                   Eval      TasBan = §Ar5Nba + §ar5Nb2
146800030206     c                   EndIf
146900030805      * Bancali Bartolini
147000030805     c                   If        %Subst(blpgva:1:1) = 'O'
147100030805     c                   Clear                   dAr5BNB
147200030805     c                   Clear                   tasban
147300030805     C                   move      'BNB'         KAR5TRD
147400030805     c     kFiar5        Chain     Fiar501l
147500030805     c                   If        %Found(Fiar501l)
147600030805     c                   Movel     Ar5Uni        dAr5BNB
147700030805     c                   EndIf
147800030805     c                   Eval      TasNCL = §Ar5Bcor
147900030805     c                   EndIf
148000980514     C* FLAG SI NO DDT
148100980514     C     BLPFST        IFEQ      'S'
148200980514     C     BLPFST        OREQ      'C'
148300980514     C                   MOVEL     'S'           TASDDT
148400980514     C                   ENDIF
148500980514     C*
148600980514     C                   MOVEL     'G'           TASSVA
148700980514     C*
148800980515     C                   MOVEL     KPJBU         WSVKPJ
148900980514     C                   CALL      'TNSF20R'
149000980514     C                   PARM                    KPJBA
149100990930     C                   PARM                    DTAS
149200990930     C                   PARM                    DTASV
149300980514     C                   MOVEL     WSVKPJ        KPJBU
149400980514     C*
149500980514     C     TASERR        IFEQ      *BLANKS
149600980514     C                   Z-ADD     1             §F
149700980515     C     TASSVA        LOOKUP    NSV(§F)                                30
149800980515     C   30              Z-ADD     NVA(§F)       VRCA
149900980514     C                   ELSE
150000980514     C                   SETON                                        1968
150100980514     C                   END
150200911021     C     ENDCPR        ENDSR
150300900426     C/SPACE 3
150400900423     C*---------------------------------------------------------------*
150500900423     C*   C T R V I 5      ESITAZIONE                                 *
150600900423     C*---------------------------------------------------------------*
150700900423     C     CTRVI5        BEGSR
150800030618     C                   SETOFF                                       1918
150900911022     C* SE HO GIA FATTO ADDEBITI VISUALIZZO SOLO E NON CONTROLLO
151000911022     C* ECCETTO IL CODICE FINE GIACENZA
151100990930     C     *IN77         IFEQ      '0'
151200900427     C*  CLIENTE A CUI FATTURARE   RICERCA
151300941005     C     '?'           SCAN      VKSC                                   21
151400990930     C   21              DO
151500900427     C                   Z-ADD     KCI           ACOKCC
151600900502     C                   MOVEL     *BLANKS       ACORAG
151700900502     C                   MOVEL     VRSDB         ACORAG
151800900427     C                   Z-ADD     0             ACOKSC
151900900427     C                   Z-ADD     0             PARSTA            1 0
152000900427     C                   CALL      'XALFA1'
152100900427     C                   PARM                    PARDUT
152200900427     C                   PARM                    CODUT
152300900427     C                   PARM                    ACORAG
152400900427     C                   PARM                    ACOKCC
152500900427     C                   PARM                    ACOKSC
152600900427     C                   PARM                    PARSTA
152700961016     C                   PARM                    PARFLR           90
152800900427     C     PARSTA        IFNE      -1
152900900427     C                   MOVE      ACOKSC        VKSC
153000900502     C                   ELSE
153100900502     C                   MOVE      *ZEROS        VKSC
153200900427     C                   END
153300960403     C                   SETON                                        18
153400910319     C                   GOTO      ENDCT5
153500990930     C                   END
153600910527     C* CONTROLLO SE MINORE DI ZERO
153700910527     C                   TESTN                   VKSC                 21
153800910527     C  N21              SETON                                        5519
153900910527     C  N21              GOTO      ENDCT5
154000900427     C*
154100990930     C     VKSC          IFGT      *ZEROS
154200900423     C                   MOVE      VKSC          §QUATR
154300990930     C     §QUATR        IFNE      *ALL'8'
154400900423     C     §QUATR        ANDNE     *ALL'9'
154500900427     C                   MOVE      VKSC          §KSC
154600900423     C     KACO          CHAIN     CNACO000                           21
154700900423     C  N21ACOFLG        COMP      '*'                                    21
154800900423     C  N21ACOABL        COMP      '*'                                    21
154900910319     C   21              SETON                                        5519
155000910319     C   21              GOTO      ENDCT5
155100910319     C* SE BLOCCATO SEGNALO CON FORZATURA
155200910319     C     VKSC          IFNE      SA2KSC
155300910319     C                   MOVEL     VKSC          SA2KSC
155400130320     C     ACOABL        IFne      *blank
155500910319     C                   SETON                                        7019
155600910319     C                   GOTO      ENDCT5
155700910319     C                   END
155800910319     C                   END
155900910319     C*
156000910319     C                   MOVEL     ACORAG        VRSDB
156100910319     C     KACO          CHAIN     CNIND000                           21
156200900502     C  N21              MOVEL     INDVIA        VINDB
156300900502     C  N21              MOVEL     INDCIT        VLODB
156400900502     C  N21              MOVEL     INDPRV        VPRDB
156500050223     C  N21              MOVEL     INDCAe        VCADB
156600941006     C  N21              MOVEL     INDSTA        V3CNZM
156700900502     C*
156800990930     C                   ELSE
156900910319     C*
157000900525     C     §QUATR        IFEQ      *ALL'9'
157100900525     C                   SETON                                        6019
157200910319     C                   GOTO      ENDCT5
157300900525     C                   END
157400900502     C     VRSDB         IFEQ      *BLANK
157500900502     C     VINDB         OREQ      *BLANK
157600900502     C     VPRDB         OREQ      *BLANK
157700970909     C     V3CNZM        ANDEQ     *BLANK
157800900502     C     VLODB         OREQ      *BLANK
157900900502     C                   SETON                                        5619
158000910319     C                   GOTO      ENDCT5
158100900502     C                   END
158200990930     C                   END
158300910527     C*
158400990930     C                   ELSE
158500910527     C*
158600910527     C                   SETON                                        5519
158700910527     C                   GOTO      ENDCT5
158800990930     C                   END
158900900427     C* MODIF. CODICE RIVISUALIZZO
159000990930     C     VKSC          IFNE      SAVKSC
159100900524     C     VRSDB         ORNE      SAVRSB
159200900524     C     VINDB         ORNE      SAVINB
159300900524     C     VLODB         ORNE      SAVLOB
159400900524     C     VPRDB         ORNE      SAVPRB
159500900524     C     VCADB         ORNE      SAVCAB
159600941006     C     V3CNZM        ORNE      SAVNZM
159700910319     C                   SETON                                        7119
159800900427     C                   MOVE      VKSC          SAVKSC
159900900524     C                   MOVEL     VRSDB         SAVRSB
160000900524     C                   MOVEL     VINDB         SAVINB
160100900524     C                   MOVEL     VLODB         SAVLOB
160200900524     C                   MOVEL     VPRDB         SAVPRB
160300900524     C                   MOVEL     VCADB         SAVCAB
160400941005     C                   MOVEL     V3CNZM        SAVNZM
160500910319     C                   GOTO      ENDCT5
160600990930     C                   END
160700900502     C* CONTROLLO CHE LA FIL. SIA UGUALE A FIL. PARTENZA BOLLA
160800900502     C   19              GOTO      ENDCT5
160900990930     C                   END
161000911022     C*
161100900423     C* CODICE FINE GIACENZA
161200911022     C* --------------------
161300911109     C* SE RICH DA DISPOSIZIONI NON CONTROLLO : NON VIENE VISUALIZZ.
161400990930     C     D82TRI        IFNE      'D'
161500941005     C     '?'           SCAN      VCFG                                   21
161600990930     C   21              DO
161700900426     C*§  RICERCA IN TABEL00F
161800900426     C                   MOVEL     '2C'          §COD
161900900426     C                   MOVEL     *BLANK        §KEY
162000900426     C                   MOVEL     *BLANK        §DES
162100900426     C                   MOVE      *BLANK        VCFG
162200900620     C                   CALL      'X§TB60R'
162300900426     C                   PARM                    CODUT
162400900426     C                   PARM                    §COD
162500900426     C                   PARM                    §KEY
162600900426     C                   PARM                    §DES
162700900426     C                   MOVEL     §KEY          VCFG
162800900426     C                   MOVEL     §DES          DCFG
162900990930     C                   END
163000900423     C                   MOVEL     '2C'          TBLCOD
163100900423     C                   MOVEL     *BLANK        TBLKEY
163200900423     C                   MOVEL     VCFG          TBLKEY
163300900423     C     KTAB          CHAIN     TABEL                              21
163400900423     C   21              MOVEL     *ALL'?'       DCFG
163500900423     C  N21              MOVEL     TBLUNI        DCFG
163600951109     C   21              SETON                                        4919
163700900423     C* DATA FINE GIACENZA
163800941006     C                   MOVE      VDCG          G02DAT
163900941006     C                   MOVEL     ' '           G02ERR
164000941006     C                   CALL      'XSRDA8'
164100941006     C                   PARM                    WLBDA8
164200990930     C     G02ERR        IFEQ      '1'
164300941006     C                   SETON                                        21
164400941006     C                   ELSE
164500941006     C                   MOVE      G02INV        AMGDCG
164600941006     C                   MOVE      G02DAT        VDCG
164700941010     C     GCPDED        IFNE      0
164800941006     C     AMGDCG        COMP      GCPDED                               21
164900900423     C   21              SETON                                        5119
165000941010     C                   ELSE
165100941010     C     AMGDCG        COMP      GCPDED                               21
165200941010     C   21              SETON                                        5119
165300941010     C                   END
165400900427     C  N21AMGDCG        COMP      AMGUDT                             21
165500900427     C   21              SETON                                        5419
165600990930     C                   END
165700990930     C                   END
165800900427     C*
165900990930     C     *IN77         IFEQ      '0'
166000900423     C*
166100901219     C* CONTROLLI NR/DATA FATTURA
166200901220     C     VTFT          IFEQ      9
166300990908     C* SE NON DEVO FATTURARE NON IMPOSTO LA SPEDIZIONE DI RECUPERO
166400990930     C     VLNW          IFNE      *ZEROS
166500990908     C     VNSW          ORNE      *ZEROS
166600990908     C     VNBW          ORNE      *ZEROS
166700990908     C                   SETON                                        626364
166800990908     C                   SETON                                        1928
166900990908     C                   MOVEL     ERR(4)        $MSG
167000901219     C                   GOTO      ENDCT5
167100990930     C                   END
167200901219     C                   ELSE
167300901219     C*                    ----
167400990908     C* Controllo se prepagato
167500990908     C                   MOVE      VKSC          WAPPOG            4 0
167600990908     C     WAPPOG        IFEQ      8888
167700990908     C     WAPPOG        OREQ      9999
167800990908     C     VLNW          IFEQ      0
167900990908     C     VNBW          ANDEQ     0
168000990908     C                   SETON                                        626364
168100990908     C                   SETON                                        1928
168200990908     C                   MOVEL     ERR(12)       $MSG
168300990908     C                   GOTO      ENDCT5
168400990908     C                   END
168500990908     C                   END
168600990908     C* SE IMMESSO PUNTO OPERATIVO IMMETTERE ANCHE NR.SPEDIZIONE
168700990908     C     VLNW          IFNE      0
168800990908     C     VASW          IFEQ      0
168900990908     C     VNBW          OREQ      0
169000990908     C                   SETON                                        626364
169100990908     C                   SETON                                        1928
169200990908     C                   MOVEL     ERR(5)        $MSG
169300901219     C                   GOTO      ENDCT5
169400901219     C                   END
169500901220     C                   END
169600990908     C* SE IMMESSO NR. SPEDIZIONE DEVE ESISTERE ANCHE ANNO/P.OPER
169700990908     C     VNBW          IFNE      0
169800990908     C     VNSW          ORNE      0
169900990908     C     VASW          IFEQ      0
170000990908     C     VLNW          OREQ      0
170100990908     C                   SETON                                        626364
170200990908     C                   SETON                                        1928
170300990908     C                   MOVEL     ERR(5)        $MSG
170400901219     C                   GOTO      ENDCT5
170500901219     C                   END
170600901220     C                   END
170700990908     C* Controllo che esista il punto operativo
170800990930     C     VNBW          IFNE      0
170900990908     C                   Z-ADD     VLNW          ORGFIL
171000990908     C                   MOVEL     *BLANKS       GIAEST            1
171100990908     C     ORGFIL        CHAIN     AZORG01L                           21
171200990908     C     *IN21         IFEQ      '1'
171300990908     C     ORGFVA        OREQ      '*'
171400990908     C                   MOVEL     ERR(6)        $MSG
171500990908     C                   SETON                                        621928
171600990908     C                   GOTO      ENDCT5
171700990908     C                   END
171800990908     C* Controllo la validità dei dati spedizione immessi
171900990908     C                   Z-ADD     VLNW          KLNP
172000990908     C                   Z-ADD     VASW          KAAS
172100990908     C                   Z-ADD     VNSW          KNRS
172200990908     C                   Z-ADD     VNBW          KNSP
172300990908     C     KBLP          CHAIN     FNBLP01L                           43
172400990908     C                   Z-ADD     BLPMGS        G02INV
172500990908     C                   MOVEL     BLPAAS        G02INV
172600990908     C                   Z-ADD     0             G02DAT
172700990908     C                   MOVEL     '3'           G02ERR
172800990908     C                   CALL      'XSRDA8'
172900990908     C                   PARM                    WLBDA8
173000990908     C                   Z-ADD     G02DAT        VDBG
173100990910     C                   WRITE     LG10D03
173200990908     C* Se non esiste la bolla do errore
173300990908     C     *IN43         IFEQ      '1'
173400990908     C                   MOVEL     ERR(7)        $MSG
173500990908     C                   SETON                                        626364
173600990908     C                   SETON                                        1928
173700990908     C                   GOTO      ENDCT5
173800990908     C                   END
173900990908     C* Se esiste ma non è stata ritirata segnalo bolla errata
174000990908     C     BLPPDR        IFEQ      0
174100990908     C                   MOVEL     ERR(8)        $MSG
174200990908     C                   SETON                                        626364
174300990908     C                   SETON                                        1928
174400990908     C                   GOTO      ENDCT5
174500990908     C                   END
174600990908     C* Se il riferim. mittente numerico è diverso do errore
174700990908     C     VRIF          IFNE      BLPRMN
174800990908     C                   MOVEL     ERR(10)       $MSG
174900990908     C                   SETON                                        626364
175000990908     C                   SETON                                        1928
175100990908     C                   GOTO      ENDCT5
175200990908     C                   END
175300990908     C* Contollo se la bolla stata tassata
175400990930     C                   MOVEL     '1'           KTRC
175500990930     C     KAR6          CHAIN     FIAR601L                           43
175600990930     C     *IN43         IFEQ      '1'
175700990908     C* Se non è stata tassata do errore
175800990930     C     AR6DFT        OREQ      0
175900990908     C                   MOVEL     ERR(9)        $MSG
176000990908     C                   SETON                                        626364
176100990908     C                   SETON                                        1928
176200990908     C                   GOTO      ENDCT5
176300990908     C                   ELSE
176400990910     C                   SETON                                        65
176500990908     C* .. altrimenti memorizzo i dati della fattura a video x
176600990908     C* riscriverli nel gcp
176700990930     C                   MOVE      AR6DFT        AMGDFT
176800990930     C                   MOVE      AR6DFT        G02INV
176900990908     C                   Z-ADD     0             G02DAT
177000990908     C                   MOVEL     '3'           G02ERR
177100990908     C                   CALL      'XSRDA8'
177200990908     C                   PARM                    WLBDA8
177300990908     C                   Z-ADD     G02DAT        VDFT
177400990930     C                   Z-ADD     AR6NFT        VNFT
177500990930     C                   Z-ADD     AR6FIV        VFIV
177600990930     C                   END
177700901219     C*
177800990930     C                   END
177900990930     C                   END
178000990930     C                   END
178100941005     C* PRENDO I DATI DELLA TARIFFA
178200941005     C* 77 HO FATTO ADDEBITI SULLA GIACENZA VISULIZZO SOLO GLI IMPORTI ADDEB-
178300941005     C* CALCOLO SPESE SOLO SE PAGA MITTENTE
178400990930     C     *IN77         IFEQ      '0'
178500941006     C*
178600941006     C     VDIG          IFEQ      *ZEROS
178700941006     C                   Z-ADD     VDTG          VDSD
178800941006     C                   ELSE
178900941006     C                   Z-ADD     VDIG          VDSD
179000941006     C                   END
179100941006     C*
179200941006     C     VDED          IFNE      0
179300941006     C                   Z-ADD     VDED          VDSA
179400941006     C                   ELSE
179500941006     C     VDDM          IFNE      0
179600941006     C                   Z-ADD     VDDM          VDSA
179700941006     C                   ELSE
179800941006     C                   Z-ADD     VDCG          VDSA
179900941006     C                   END
180000941006     C                   END
180100941006     C*
180200060407     c* verifica se richiamare routine per calcolo spese sui dati del prg 0
180300060407     c* se no spese oppure se motivo con azzeramento spese non esegue
180400060407     c                   if        gcpasg <>'N' and §2cnosp = *blank
180500950301     C                   SETOFF                                       16
180600941006     C     GCPPSG        IFEQ      'M'
180700941006     C     VTFT          OREQ      0
180800941006     C     GCPPSG        ANDNE     'D'
180900941006     C                   EXSR      TGCVID
181000950308     C                   END
181100060407     c                   endif
181200960226     C*
181300960226     C     GCPDIS        IFEQ      'C'
181400960226     C     VDFT          ANDNE     0
181500960229     C     GCPNPC        IFEQ      ' '
181600960229     C     OLDTB1        ANDEQ     'FS'
181700960229     C     GCPNPC        OREQ      'F'
181800960229     C*  Richiamo pgm per la tassazione del trasporto
181900960229     C                   EXSR      TASTRA
182000960226     C                   SETON                                        69
182100960226     C                   SETOFF                                       73
182200960229     C                   END
182300960226     C                   END
182400960226     C*
182500990930     C                   END
182600950227     C*  Se non sono in immissione dispozizioni reperisco tipo comunic.
182700950227     C*  fine giacenza
182800950227     C     D82TRI        IFNE      'D'
182900950227     C                   EXSR      REPTPC
183000950227     C                   END
183100941005     C*
183200900423     C     ENDCT5        ENDSR
183300900426     C/SPACE 3
183400960229     C*---------------------------------------------------------------*
183500960229     C*   T A S T R A    Richiamo pgm per la tassazione del trasp.    *
183600960229     C*---------------------------------------------------------------*
183700960229     C     TASTRA        BEGSR
183800960229     C*
183900960229     C*  Imposto campi per tassazione
184000990930     C                   CLEAR                   DTAS
184100990930     C                   CLEAR                   DTASV
184200960229     C                   MOVEL     *BLANKS       TASTLA
184300960229     C                   Z-ADD     8888          TASKSC
184400960229     C                   MOVEL     BLPLNP        TASKSC
184500960229     C                   MOVEL     BLPCTS        TASCTS
184600960229     C                   MOVEL     BLPCTR        TASCTR
184700960229     C                   MOVEL     BLPCAD        TASCAD
184800960229     C                   MOVEL     BLPFIN        TASFIN
184900960229     C                   MOVEL     BLPCAM        TASCAM
185000960229     C                   MOVEL     BLPFAP        TASFAP
185100960229     C                   MOVEL     'C'           TASTSP
185200960229     C                   MOVEL     BLPTC1        TASTC1
185300960229     C                   MOVEL     BLPTC2        TASTC2
185400960229     C                   MOVEL     'S'           TASFDN
185500960229     C                   MOVEL     BLPVAS        TASVAS
185600960229     C                   Z-ADD     BLPIAS        TASIAS
185700960229     C                   Z-ADD     BLPQFT        TASQFT
185800960229     C                   Z-ADD     BLPNCL        TASNCL
185900160530     C                   Z-ADD     BLPNCL        TASNCLb
186000960229     C                   Z-ADD     AMGUDT        TASDSP
186100970414     C                   Z-ADD     TASDSP        TASDCT
186200960229     C                   Z-ADD     BLPLNP        TASLNP
186300960229     C                   Z-ADD     BLPLNA        TASLNA
186400960229     C                   Z-ADD     BLPVLF        TASVLF
186500960229     C                   Z-ADD     BLPPKF        TASPKF
186600160530     C                   Z-ADD     BLPvlc        TASvlc
186700160530     C                   Z-ADD     BLPncr        TASncr
186800160530     C                   Z-ADD     BLPpkb        TASPKb
186900160530     C                   move      BLPfvf        TASfvf
187000960229     C                   Z-ADD     1             §F
187100960229     C     'B '          LOOKUP    CDB(§F)                                32
187200960229     C                   MOVEL     TB1(§F)       TASTBL
187300990930     C* Gestione nuovi campi ds tassazione
187400990930     C                   MOVEL     BLPNZD        TASNZD
187500990930     C                   MOVEL     BLPNZM        TASNZM
187600060927     C                   MOVEL     blpfbr        §asfbr
187700060927     C                   MOVEL     blpcca        §ascca
187800060927     C                   MOVEL     dtas01        TASFLO
187900970709     C* Reperisco codice tassazione
188000970709     C                   CLEAR                   TISI95
188100970709     C                   MOVEL     '3'           I95TCN
188200970709     C                   MOVEL     BLPCAM        I95CAP
188300970709     C                   MOVEL     BLPNZM        I95NAR
188400970709     C                   MOVEL     BLPPRM        I95PRV
188500970709     C                   MOVEL     BLPLOM        I95LOC
188600970709     C                   MOVEL     BLPINM        I95IND
188700970709     C                   MOVEL     BLPTFP        I95TFP
188800970709     C                   MOVEL     BLPNZM        I95NAR
188900020221      *SOLO PER FEDEX IMPOSTA IL FLAG = S
189000020221     C     NETWRK        IFEQ      'FED'
189100020221     C                   MOVE      'S'           I95FI1
189200020221     C                   ELSE
189300020221     C                   MOVE      *BLANK        I95FI1
189400020221     C                   END
189500970709     C                   CALL      'TISI95R'
189600970709     C                   PARM                    TISI95
189700970709     C                   MOVEL     O95CTS        TASMCT
189800970709     C*
189900051115     C                   MOVEL     ar9VCA        TASVCA
190000051115     C                   Z-ADD     ar9CAS        TASCAS
190100960229     C     GCPCAS        IFNE      0
190200960229     C                   MOVEL     GCPVCA        TASVCA
190300960229     C                   Z-ADD     GCPCAS        TASCAS
190400960229     C                   END
190500970411     C* FLAG SI NO DDT
190600970411     C     BLPFST        IFEQ      'S'
190700970411     C     BLPFST        OREQ      'C'
190800970411     C                   MOVEL     'S'           TASDDT
190900970411     C                   ENDIF
191000020930     c* imposto peso vdl e colli rilevati
191100030206     c                   z-add     blpncp        Tasncp
191200030206     c                   z-add     blppkc        Taspkc
191300030206      * Bancali
191400030206     c                   If        %Subst(BlpGva:1:1) = 'B'
191500030206     c                   Clear                   dAr5Ban
191600030805     C                   move      'BAN'         KAR5TRD
191700030206     c     kFiar5        Chain     Fiar501l
191800030206     c                   If        %Found(Fiar501l)
191900030206     c                   Movel     Ar5Uni        dAr5Ban
192000030206     c                   EndIf
192100030206     c                   Eval      TasBan = §Ar5Nba + §ar5Nb2
192200030206     c                   EndIf
192300030805      * Bancali Bartolini
192400030805     c                   If        %Subst(blpgva:1:1) = 'O'
192500030805     c                   Clear                   dAr5BNB
192600030805     c                   Clear                   tasban
192700030805     C                   move      'BNB'         KAR5TRD
192800030805     c     kFiar5        Chain     Fiar501l
192900030805     c                   If        %Found(Fiar501l)
193000030805     c                   Movel     Ar5Uni        dAr5BNB
193100030805     c                   EndIf
193200030805     c                   Eval      TasNCL = §Ar5Bcor
193300030805     c                   EndIf
193400960229     C*
193500980202     C                   MOVEL     KPJBU         WSVKPJ          256
193600960229     C                   CALL      'TNSF20R'
193700960229     C                   PARM                    KPJBA
193800990930     C                   PARM                    DTAS
193900990930     C                   PARM                    DTASV
194000980202     C                   MOVEL     WSVKPJ        KPJBU
194100960229     C*
194200960229     C     TASERR        IFEQ      *BLANKS
194300960229     C                   Z-ADD     TASPOR        VRTR
194400960301     C                   DO        20            §F
194500960301     C     NSV(§F)       IFNE      *BLANKS
194600960301     C     NSV(§F)       ANDNE     'I'
194700960301     C     NSV(§F)       ANDNE     'G'
194800960301     C                   ADD       NVA(§F)       VRTR
194900960229     C                   END
195000960301     C                   END
195100960301     C                   END
195200960229     C*
195300960229     C                   ENDSR
195400941006     C*---------------------------------------------------------------*
195500941006     C*   R E P T P C    Reperisco codice fine giacenza               *
195600031027     C*---------------------------------------------------------------*
195700941006     C     REPTPC        BEGSR
195800941006     C*
195900941006     C                   MOVEL     'R'           V4CTFG
196000980515     C                   MOVEL     ' '           WTROVA            1
196100941006     C                   MOVE      VKSC          KKSC
196200941222     C                   MOVE      KKSC          §QUATR
196300020221     C*
196400020221      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
196500020221     C                   CLEAR                   TR27DS
196600160316     C                   CLEAR                   fnlv59ds
196700021118     C                   MOVE      BLPctr        I27ctb
196800020221     C                   MOVE      BLPTSP        I27TSP
196900020221     C                   MOVE      BLPLNA        I27LNA
197000020221     C                   MOVE      BLPLNP        I27LNP
197100020221     C                   MOVE      KKSC          I27CLI
197200160316     C**                 MOVEL     BLPAAS        I27DTA
197300160316     C**                 MOVE      BLPMGS        I27DTA
197400160316     C                   CALL      'TRUL27R1'
197500020221     C                   PARM                    TR27DS
197600990930     C     §QUATR        IFNE      *ALL'8'
197700941006     C     §QUATR        ANDNE     *ALL'9'
197800941222     C     §QUATR        ANDNE     *ZEROS
197900020221     C                   MOVE      BLPCTR        CODCTR
198000150415     c* e' preferibile caricare tutte le tariffe in modo che il controllo
198100150415     c*  sia il più  completo possibile ai fini della corretta ricerca del
198200150415     c*  codice tariffa e conseguente tariffa giacenza
198300150415     C                   MOVEL     ' '           ILV59TBO
198400150415     C                   Z-ADD     DATDSP        ILV59DSP
198500150415     C                   MOVE      CODCTR        ILV59CTR
198600150415     C                   MOVE      KKSC          ILV59KSC
198700150415     C                   MOVE      KCI           ILV59KCI
198800000202     C                   MOVEL     KKSC          W0030             3 0
198900150415     C                   MOVEL     O27FIE        ILV59FIE
199000150415     C                   MOVEL     BLPTSP        ILV59TSP
199100150415     C                   MOVEL     'S'           ILV59CON
199200150415     c                   eval      ilv59ta2='B'
199300150415     c                   eval      ilv59blo='S'
199400150415     C                   CALL      'FNLV59R'
199500150415     C                   PARM                    fnlv59ds
199600941222     C*
199700150415     C     olv59err      IFEQ      *BLANKS
199800150415     C     olv59tks      ANDEQ     'S'
199900941006     C*  Se ho trovato tariffa in vigore mi posiziono sul file
200000941006     C*  delle tariffe giacenze con il proggressivo giusto
200100150415     C                   MOVE      olv59prg      KPRG
200200150415     C                   MOVE      olv59ctr      CODCTR
200300150415     C                   MOVE      ilv59ksc      KKSC
200400990730     C     KTGC          CHAIN     TITGC01L                           31
200500941006     C     *IN31         IFEQ      '0'
200600991119     C     TGCATB        ANDEQ     *BLANKS
200700991119     C     TGCTFG        ANDNE     *BLANKS
200800941006     C                   MOVE      TGCTFG        V4CTFG
200900941006     C                   MOVEL     'S'           WTROVA
201000941006     C                   END
201100941006     C                   END
201200990930     C                   END
201300941006     C*  Se non ho trovato tipo comunicazione mitt. su tariffe
201400941006     C*  lo cerco sull'anagrafica cliente
201500941006     C     WTROVA        IFNE      'S'
201600941006     C                   MOVEL     *BLANKS       CLSFLO
201700941006     C     KKSC          CHAIN     FNCLS01L                           31
201800941006     C     CLSFLO        IFNE      *BLANKS
201900941006     C                   MOVEL     CLSFLO        V4CTFG
202000941006     C                   END
202100941006     C                   END
202200040120      * in definitiva se non deve comunicare nulla al cliente
202300040120      * e non è riservato.
202400000922     C     §2FFTM        IFEQ      'N'
202500000922     C     §2FVAG        ANDEQ     'N'
202600040120     C     §2FRIS        andne     'N'
202700040121     C*********          MOVE      GCPTCM        V4CTFG
202800000922     C                   SETON                                        99
202900000922     C                   END
203000941006     C*
203100040121     C* Se la giacenza era stata aperta senza comunicazione al mittente
203200040121     C*  tipo : "Z"
203300040121     C* così anche la chiusura forzatamente non dovrà avere comunicazioni
203400040121     C*  al mittente.
203500040121     C     GCPTCM        IFEQ      'Z'
203600040121     C                   MOVE      GCPTCM        V4CTFG
203700040121     C                   END
203800040121     C*
203900941006     C                   ENDSR
204000900423     C*---------------------------------------------------------------*
204100900424     C*   T G C V I D    DATI TARIFFA GIACENZA                        *
204200900423     C*---------------------------------------------------------------*
204300900423     C     TGCVID        BEGSR
204400020221      *
204500020221     C                   MOVEL     ' '           WTROVA
204600020221     C                   MOVE      *ZEROS        CODCTR
204700020221     C                   MOVE      VKSC          KKSC
204800020221     C                   MOVE      KKSC          §QUATR
204900941222     C*
205000020221      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
205100020221     C                   CLEAR                   TR27DS
205200160316     C                   CLEAR                   fnlv59ds
205300021118     C                   MOVE      BLPctr        I27ctb
205400020221     C                   MOVE      BLPTSP        I27TSP
205500020221     C                   MOVE      BLPLNA        I27LNA
205600020221     C                   MOVE      BLPLNP        I27LNP
205700020221     C                   MOVE      KKSC          I27CLI
205800160316     C**                 MOVEL     BLPAAS        I27DTA
205900160316     C**                 MOVE      BLPMGS        I27DTA
206000160316     C                   CALL      'TRUL27R1'
206100020221     C                   PARM                    TR27DS
206200941222     C* Se cliente cui fatturare è 8888 prendo tarfiffa Std.
206300990930     C     §QUATR        IFNE      *ALL'8'
206400941222     C     §QUATR        ANDNE     *ALL'9'
206500941222     C     §QUATR        ANDNE     *ZEROS
206600941222     C                   MOVE      BLPCTR        CODCTR
206700150415     C                   MOVE      BLPCTR        ilv59ctr
206800150415     C                   MOVEL     ' '           ilv59tbo
206900150415     C                   Z-ADD     DATDSP        ilv59dsp
207000150415     C                   MOVE      KKSC          ilv59ksc
207100150415     C                   MOVE      KCI           ilv59kci
207200150415     C                   MOVE      O27FIE        ilv59fie
207300150415     C                   MOVEL     BLPTSP        ilv59tsp
207400150415     C                   MOVEL     'S'           ilv59con
207500150415     c                   eval      ilv59ta2='B'
207600150415     c                   eval      ilv59blo='S'
207700150415     C                   CALL      'FNLV59R'
207800150415     C                   PARM                    fnlv59ds
207900941222     C*
208000150415     C     olv59err      IFEQ      *BLANKS
208100150415     C     olv59tks      ANDEQ     'S'
208200941222     C*  Se ho trovato tariffa in vigore mi posiziono sul file
208300941222     C*  delle tariffe giacenze con il proggressivo giusto
208400150415     C                   MOVE      olv59prg      KPRG
208500150415     C                   MOVE      olv59ctr      CODCTR
208600150415     C                   MOVE      ilv59ksc      KKSC
208700990730     C     KTGC          CHAIN     TITGC01L                           31
208800941222     C     *IN31         IFEQ      '0'
208900991119     C     TGCATB        ANDEQ     *BLANKS
209000941222     C                   MOVEL     'S'           WTROVA
209100941222     C                   END
209200990930     C                   END
209300990930     C                   END
209400941222     C*  Se non ho trovato tariffa cliente cerco su tar. cartello
209500990930     C     WTROVA        IFNE      'S'
209600950301     C     §QUATR        IFNE      *ALL'8'
209700950301     C     §QUATR        ANDNE     *ALL'9'
209800950301     C     §QUATR        ANDNE     *ZEROS
209900950301     C                   SETON                                        16
210000950301     C                   END
210100160316     c* Carico la cartello :
210200160316     c*  - per 8888 col fie della bolla
210300160316     c*  - per tariffe cliente esistente col fie della tariffa
210400160316     c                   clear                   trulc7ds
210500160316    2C     olv59ERR      ifeq      *blanks
210600160316     C     olv59tks      andeq     'S'
210700160316     c                   z-add     1             indx              3 0
210800160316     c                   eval      indx=%lookup(ilv59ctr:ks_ct)
210900160316    3c                   if        indx>0 and %editc(olv59prg:'X')=
211000160316     c                             ks_pr(indx)
211100160316     c                   eval      ic7tfie=ks_ie(indx)
211200160316   x3c                   else
211300160316     c                   eval      ic7tntw=o27ntw
211400160316    3c                   endif
211500160316     c
211600160316   x2c                   else
211700160316     c                   eval      ic7tntw=o27ntw
211800160316    2c                   endif
211900160316     c
212000160316     C                   moveL     blpaas        Ic7dta
212100160316     C                   move      blpmgs        Ic7dta
212200160316     C                   move      blpctr        Ic7ctb
212300160316     c                   call      'TRULC7R'
212400160316     c                   parm                    trulc7ds
212500160316     c
212600941222     C*  Se ho trovato tariffa in vigore mi posiziono sul file
212700941222     C*  delle tariffe giacenze con il proggressivo giusto
212800160316     C                   MOVE      Oc7PRGc       KPRG
212900160316     C                   MOVE      Oc7CTRc       CODCTR
213000160316     C                   z-add     Oc7KSCc       KKSC
213100990730     C     KTGC          CHAIN     TITGC01L                           31
213200991119     C     TGCATB        IFEQ      *BLANKS
213300941222     C  N31              MOVEL     'S'           WTROVA
213400941010     C                   END
213500990930     C                   END
213600941006     C*
213700941010     C     WTROVA        IFNE      'S'
213800941006     C* calcolo
213900941010     C                   SETON                                        1957
214000941006     C                   GOTO      ENDTGC
214100941006     C                   END
214200941006     C*
214300941006     C* Moltiplico spese riconsegna x nr. riaperture
214400920210     C                   MOVE      *ZEROS        §NRG
214500031027     C                   MOVE      *ZEROS        §NRGDoss
214600920210     C                   MOVE      *ZEROS        §GGRIA            3 0
214700050316     C     KGIP          CHAIN(e)  TIGCP000                           21
214800050316     c                   if        %error
214900070521     C     KGIP          CHAIN(n)  TIGCP000
215000050316     c                   exsr      geslck
215100050318     C     KGIP          CHAIN(e)  TIGCP000                           21
215200050316     c                   endif
215300990930     C     *IN21         DOWEQ     '0'
215400060407      * entro nel calcolo solo se le spese per il progressivo che sto
215500060407      * trattando devono essere imputate
215600060407     c                   if        gcpasg <> 'N'
215700941006     C                   ADD       1             §NRG
215800031027     C                   ADD       1             §NRGDoss
215900050125      * Spese di riconsegna verifico se il motivo ha le spese di riconsegna
216000050125      * da addebitare oppure no (flag in tabella 2Z ='N')
216100050125     C                   Z-ADD     1             x2z
216200050125     C     gcpcmc        LOOKUP    C2z(x2z)                               32
216300050125     C   32nsr(x2z)      ifeq      'N'
216400060808     c     §2nospr       orne      *blank
216500050125     C                   sub       1             §NRG
216600050125     c                   end
216700031027      *
216800031027      * decodifica il tipo comunicazione e se riservata al CED
216900031027      * non deve essere conteggiata e quindi sottrae al contatore
217000031027      * del Dossier quello che aveva appena aggiunto
217100031027     C                   Z-ADD     1             x2f
217200031027     C     gcpTCM        LOOKUP    C2F(x2f)                               32
217300031027     C   32r2f(x2f)      ifne      *blank
217400031027     C                   sub       1             §NRGDoss
217500031027     c                   end
217600031027      *
217700920211     C* SE NON HO GIA FATTO ADDEBITI
217800990930     C     GCPFRG        IFGT      0
217900920211     C     GCPNBW        ANDEQ     0
218000920211     C                   MOVE      *ZEROS        G02DAT
218100920211     C                   MOVE      *ZEROS        G02INV
218200920210     C* CALCOLO GG SOSTA PER OGNI RIAPERTURA
218300941006     C     GCPDUR        IFNE      0
218400941006     C                   Z-ADD     GCPDUR        G02INV
218500941006     C                   ELSE
218600941006     C                   MOVEL     GCPAGC        G02INV
218700941006     C                   MOVE      GCPMGC        G02INV
218800941006     C                   END
218900941006     C                   MOVEL     '3'           G02ERR
219000941006     C                   CALL      'XSRDA8'
219100941006     C                   PARM                    WLBDA8
219200920210     C                   Z-ADD     G02TGI        §GGDA
219300920210     C*
219400941006     C     GCPDED        IFNE      0
219500941006     C                   Z-ADD     GCPDED        G02INV
219600941006     C                   ELSE
219700941006     C                   Z-ADD     GCPDDM        G02INV
219800941006     C                   END
219900941228     C                   MOVEL     '3'           G02ERR
220000941006     C                   CALL      'XSRDA8'
220100941006     C                   PARM                    WLBDA8
220200920210     C                   Z-ADD     G02TGI        §GGA
220300920210     C*
220400920211     C     §GGA          SUB       §GGDA         §GGXX             3 0
220500920211     C                   ADD       1             §GGXX
220600920211     C                   ADD       §GGXX         §GGRIA
220700990930     C                   END
220800060407      *gcpasg ='N'
220900060407     c                   endif
221000050316     C     KGIP          READE(e)  TIGCP000                               21
221100050316     c                   if        %error
221200070521     C     KGIP          READE(n)  TIGCP000
221300050316     c                   exsr      geslck
221400050318     C     KGIP          READE(e)  TIGCP000                               21
221500050316     c                   endif
221600990930     C                   END
221700900502     C*
221800990930     C* Mi salvo divisa di tassazione spese di giacenza in un
221900990930     C* altro campo
222000150415     C                   MOVEL     olv59div      WGCDIV            3
222100990930     C* Verifico quale sarebbe la divisa della tariffa in vigore
222200990930     C* oggi a meno che non l'abbia già reperita dal a
222300990930     C     TASDIV        IFEQ      *BLANKS
222400020221     C                   MOVEL     WGCDIV        LSTDIV
222500990930     C                   ELSE
222600990930     C                   MOVEL     TASDIV        LSTDIV            3
222700990930     C                   END
222800900502     C* CALCOLO GLI IMPORTI DA ADDEBITARE DA TARIFFA  X SOSTA
222900900504     C* SE SI FATTURAZIONE
223000050316     C     KGIA          CHAIN(e)  TIGCP02L                           40
223100050316     c                   if        %error
223200070521     C     KGIA          CHAIN(n)  TIGCP02L
223300050316     c                   exsr      geslck
223400050318     C     KGIA          CHAIN(e)  TIGCP02L                           40
223500050316     c                   endif
223600900504     C                   Z-ADD     TGCSGD        VSGD
223700900504     C                   Z-ADD     TGCSGF        VSGF
223800900502     C                   EXSR      TGCCAL
223900900502     C*                    -----------
224000100708     c                   clear                   §sgr
224100100708     C                   Z-ADD     tgcsgv        §sgv
224200941006     C* Se flag x franchigia uguale a 2 e sono dentro i giorni di franchigia
224300941006     C* non devo calcolare le spese di riconsegna e dossier
224400950120     C                   MOVE      VSGF          SAVSGF
224500950120     C                   MOVE      VGGS          SAVGGS
224600060110 1   C     TGCFAF        IFEQ      '2'
224700941006     C     VGGS          ANDLE     VSGF
224800060111     C                   Z-ADD     0             Vtsgs
224900100708     C                   Z-ADD     0             §SGR
225000100708     C                   Z-ADD     0             §SGv
225100941006     C                   Z-ADD     0             VSGD
225200060111     C                   Z-ADD     0             VSGa
225300060110x1   C                   ELSE
225400060110 2   C     TGCFAF        IFEQ      '3'
225500060110     C     VGGS          ANDLE     VSGF
225600060110     C                   Z-ADD     0             Vtsgs
225700060110     C                   MOVE      *ZEROS        VSGD
225800060110     C                   MOVE      *ZEROS        VSGA
225900100708     C                   Z-ADD     0             §SGv
226000100708     C* SOLO SE NON RESO IMPOSTO SPESE RICONSEGNA
226100100708 4   C     GCPDIS        IFne      'R'
226200060110     C     BLPFIN        IFNE      'C'
226300110623     C     BLPFIN        andne     'T'
226400100708     C                   Z-ADD     TGCSGP        §SGR
226500060110     C                   ELSE
226600100708     C                   Z-ADD     TGCSGR        §SGR
226700060110     C                   END
226800100708     C                   END
226900060110x2   C                   ELSE
227000060110 3   C     TGCFAF        IFEQ      '1'
227100060110     C     VGGS          ANDLE     VSGF
227200060110     C                   Z-ADD     0             Vtsgs
227300100708     C* SOLO SE NON RESO IMPOSTO SPESE RICONSEGNA
227400100708 4   C     GCPDIS        IFne      'R'
227500060111     C     BLPFIN        IFNE      'C'
227600110623     C     BLPFIN        andne     'T'
227700100708     C                   Z-ADD     TGCSGP        §SGR
227800060111     C                   ELSE
227900100708     C                   Z-ADD     TGCSGR        §SGR
228000060111     C                   END
228100100708     C                   END
228200060110x3   C                   ELSE
228300951212     C* Se ho dato disposizioni di reso non devo calcolare
228400951212     C* le spese di riconsegna
228500100708 4   C     GCPDIS        IFne      'R'
228600951212     C* SPESE RICONSEGNA
228700060110 5   C     BLPFIN        IFNE      'C'
228800110623     C     BLPFIN        andne     'T'
228900060110     C                   Z-ADD     TGCSGP        §SGR
229000060110x5   C                   ELSE
229100060110     C                   Z-ADD     TGCSGR        §SGR
229200060110e5   C                   END
229300060222e4   C                   END
229400060222e3   C                   END
229500060222e2   C                   END
229600060222e1   C                   END
229700041110
229800900502     C     §SGR          MULT      §NRG          §SGR
229900900502     C* SPESE RICONS.AL QUINTALE + SPESE VARIE
230000900502     C     §SGR          MULT      §PESCA        §SGR
230100100708     C     §SGR          ADD       §sgv          VSGR
230200911022     C* MOLTIPLICO SPESE DOSSIER    X NR. RIAPERTURE
230300031027     C     VSGD          MULT      §NRGDoss      VSGD
230400950308     C*
230500991020     C     LSTDIV        IFNE      *BLANKS
230600990930     C                   MOVE      LSTDIV        V4CDIV
230700991020     C                   END
230800950308     C     GCPASG        IFEQ      'N'
230900950308     C                   Z-ADD     0             VTSGS
231000950308     C                   Z-ADD     0             VSGR
231100950308     C                   Z-ADD     0             VSGD
231200950308     C                   Z-ADD     0             VSGA
231300060110     c* .. se devo procedere al recupero e la tassazione è stata
231400990930     C*    effettuata in una divisa diversa da quella corrente
231500990930     C*    converto gli importi
231600990930     C                   ELSE
231700990930     C     LSTDIV        IFNE      WGCDIV
231800990930     C                   EXSR      CNVSPG
231900950308     C                   END
232000990930     C                   END
232100941006     C*
232200900424     C     ENDTGC        ENDSR
232300900426     C/SPACE 3
232400900424     C*---------------------------------------------------------------*
232500900502     C*   T G C C A L    CALCOLO IMPORTI DA ADDEBITARE X SOSTA        *
232600900424     C*---------------------------------------------------------------*
232700900424     C     TGCCAL        BEGSR
232800920217     C                   MOVE      *ZEROS        G02TGI
232900900424     C     VGGS          IFEQ      *ZEROS
233000900424     C* CALCOLO GIORNI DI SOSTA
233100900502     C                   MOVE      VDSD          G02DAT
233200900427     C                   MOVEL     *BLANK        G02ERR
233300941006     C                   CALL      'XSRDA8'
233400941006     C                   PARM                    WLBDA8
233500900424     C                   Z-ADD     G02TGI        §GGDA
233600900424     C*
233700900502     C                   MOVE      VDSA          G02DAT
233800900427     C                   MOVEL     *BLANK        G02ERR
233900941006     C                   CALL      'XSRDA8'
234000941006     C                   PARM                    WLBDA8
234100900424     C                   Z-ADD     G02TGI        §GGA
234200900424     C*
234300900424     C     §GGA          SUB       §GGDA         §GGSOS
234400900424     C                   ADD       1             §GGSOS
234500920211     C* SOMMO GG RIAPERTURE
234600920210     C                   ADD       §GGRIA        §GGSOS
234700900424     C     §GGSOS        IFGT      VSGF
234800900424     C                   Z-ADD     §GGSOS        §GGPAG
234900900424     C                   ELSE
235000900427     C                   Z-ADD     *ZEROS        §GGPAG
235100900424     C                   END
235200900426     C                   Z-ADD     §GGPAG        VGGS
235300900524     C* MI SALVO I GG DA PAGARE CALCOLATI X SEGNALARE SE A VIDEO LI HANNO
235400900524     C* MODIFICATI
235500900524     C                   Z-ADD     §GGPAG        §GGSAV
235600900424     C                   ELSE
235700900427     C*                    ----
235800900427     C     VGGS          IFGT      VSGF
235900900427     C                   Z-ADD     VGGS          §GGPAG
236000900427     C                   ELSE
236100900427     C                   Z-ADD     *ZEROS        §GGPAG
236200900427     C                   END
236300900424     C                   END
236400941006     C*
236500901006     C                   Z-ADD     0             XP                1 0
236600900424     C                   MOVE      *ZEROS        SGG
236700900424     C                   MOVE      *ZEROS        STU
236800900424     C                   MOVE      *ZEROS        STT
236900900424     C*
237000901006     C                   Z-ADD     §GGPAG        §GGPAR
237100901006     C                   DO        3             X
237200900424     C     §GGPAG        IFGT      TGS(X)
237300901006     C     §GGPAG        SUB       TGS(X)        §GGPAR
237400901006     C     X             IFEQ      1
237500900424     C                   Z-ADD     TGS(X)        SGG(X)
237600901006     C                   ELSE
237700901006     C     TGS(X)        SUB       TGS(XP)       SGG(X)
237800901006     C                   END
237900900424     C                   Z-ADD     TTS(X)        STU(X)
238000900424     C                   ELSE
238100901006     C                   Z-ADD     §GGPAR        SGG(X)
238200900424     C                   Z-ADD     TTS(X)        STU(X)
238300900424     C                   Z-ADD     0             §GGPAG
238400901006     C                   Z-ADD     0             §GGPAR
238500900424     C                   END
238600901006     C                   ADD       1             XP
238700901006     C                   END
238800900424     C* CALCOLO PESO TASSABILE
238900950203     C     TGCRPV        MULT      BLPVLF        §PESVO           13 3
239000950203     C     §PESVO        MULT      100           §PESVO
239100941006     C     §PESVO        IFGT      BLPPKF
239200950203     C     §PESVO        DIV       100           §PES§            13 3
239300900424     C                   ELSE
239400941006     C     BLPPKF        DIV       100           §PES§
239500900424     C                   END
239600941228     C     §PES§         ADD       0,99          §PESCA            6 0
239700900424     C* CALCOLO TOTALE SPESE SOSTA
239800900424     C     SGG           MULT      STU           STT
239900900424     C     STT           MULT      §PESCA        STT
240000900426     C                   XFOOT     STT           VTSGS
240100900508     C* MINIMO TASSABILE SPESE SOSTA  SE SUPERO  GG DI FRANCHIGIA
240200900426     C     VTSGS         IFLT      TGCSTM
240300900504     C     VGGS          ANDGT     VSGF
240400900426     C                   Z-ADD     TGCSTM        VTSGS
240500900424     C                   END
240600900424     C     ENDCAL        ENDSR
240700900426     C/SPACE 3
240800990930     C*---------------------------------------------------------------*
240900990930     C* CNVSPG: Procedo alla conversione delle spese di giacenza      *
241000990930     C*---------------------------------------------------------------*
241100990930     C     CNVSPG        BEGSR
241200990930     C*
241300990930     C* converto le spese di sosta
241400990930     C                   CLEAR                   YEURDS
241500990930     C                   MOVEL     WGCDIV        YECDVI
241600990930     C                   Z-ADD     VTSGS         YECIMI
241700991027     C                   MOVEL     LSTDIV        YECDVO
241800011220     C* SE CONVERTO IN LIRE FORZO IL NUMERO DEI DECIMALI = 0
241900011220     C     YECDVO        IFEQ      'ITL'
242000011220     C                   MOVEL     '0'           YECDCO
242100011220     C                   ELSE
242200990930     C                   MOVEL     '3'           YECDCO
242300011220     C                   END
242400990930     C                   MOVEL     'H'           YECTAR
242500990930     C                   CALL      'YEURCO'
242600990930     C                   PARM                    YEURDS
242700991001     C                   Z-ADD     YECIMO        VTSGS
242800990930     C* converto le spese di riconsegna
242900990930     C                   CLEAR                   YEURDS
243000990930     C                   MOVEL     WGCDIV        YECDVI
243100990930     C                   Z-ADD     VSGR          YECIMI
243200991027     C                   MOVEL     LSTDIV        YECDVO
243300011220     C* SE CONVERTO IN LIRE FORZO IL NUMERO DEI DECIMALI = 0
243400011220     C     YECDVO        IFEQ      'ITL'
243500011220     C                   MOVEL     '0'           YECDCO
243600011220     C                   ELSE
243700011220     C                   MOVEL     '3'           YECDCO
243800011220     C                   END
243900990930     C                   MOVEL     'H'           YECTAR
244000990930     C                   CALL      'YEURCO'
244100990930     C                   PARM                    YEURDS
244200991001     C                   Z-ADD     YECIMO        VSGR
244300990930     C* converto le spese di dossier
244400990930     C                   CLEAR                   YEURDS
244500990930     C                   MOVEL     WGCDIV        YECDVI
244600990930     C                   Z-ADD     VSGD          YECIMI
244700991027     C                   MOVEL     LSTDIV        YECDVO
244800011220     C* SE CONVERTO IN LIRE FORZO IL NUMERO DEI DECIMALI = 0
244900011220     C     YECDVO        IFEQ      'ITL'
245000011220     C                   MOVEL     '0'           YECDCO
245100011220     C                   ELSE
245200011220     C                   MOVEL     '3'           YECDCO
245300011220     C                   END
245400990930     C                   MOVEL     'H'           YECTAR
245500990930     C                   CALL      'YEURCO'
245600990930     C                   PARM                    YEURDS
245700991001     C                   Z-ADD     YECIMO        VSGD
245800990930     C* converto le spese aggiuntive
245900990930     C                   CLEAR                   YEURDS
246000990930     C                   MOVEL     WGCDIV        YECDVI
246100990930     C                   Z-ADD     VSGA          YECIMI
246200991027     C                   MOVEL     LSTDIV        YECDVO
246300011220     C* SE CONVERTO IN LIRE FORZO IL NUMERO DEI DECIMALI = 0
246400011220     C     YECDVO        IFEQ      'ITL'
246500011220     C                   MOVEL     '0'           YECDCO
246600011220     C                   ELSE
246700011220     C                   MOVEL     '3'           YECDCO
246800011220     C                   END
246900990930     C                   MOVEL     'H'           YECTAR
247000990930     C                   CALL      'YEURCO'
247100990930     C                   PARM                    YEURDS
247200991001     C                   Z-ADD     YECIMO        VSGA
247300990930     C* Ricalcolo totale
247400990930     C     VTSGS         ADD       VSGR          §TOTSP
247500990930     C     VSGD          ADD       §TOTSP        §TOTSP
247600990930     C     VSGA          ADD       §TOTSP        §TOTSP
247700990930     C*
247800990930     C                   ENDSR
247900900426     C*---------------------------------------------------------------*
248000900426     C*   C T R V I 6                                                 *
248100900426     C*---------------------------------------------------------------*
248200900426     C     CTRVI6        BEGSR
248300900427     C                   SETOFF                                       19
248400900427     C* CONTROLLO FORMALE   DATA   - DA
248500941006     C                   MOVE      VDSD          G02DAT
248600941006     C                   MOVEL     ' '           G02ERR
248700941006     C                   CALL      'XSRDA8'
248800941006     C                   PARM                    WLBDA8
248900941006     C     G02ERR        IFEQ      '1'
249000941006     C                   SETON                                        21
249100941006     C                   ELSE
249200941006     C                   MOVE      G02INV        AMGDSD
249300941006     C                   MOVE      G02DAT        VDSD
249400941006     C     AMGDSD        COMP      BLPDSP                               21
249500900427     C  N21AMGDSD        COMP      AMGUDT                             21
249600941006     C                   END
249700941006     C   21              SETON                                        5219
249800900427     C* CONTROLLO FORMALE   DATA   -  A
249900941006     C                   MOVE      VDSA          G02DAT
250000941006     C                   MOVEL     ' '           G02ERR
250100941006     C                   CALL      'XSRDA8'
250200941006     C                   PARM                    WLBDA8
250300941006     C     G02ERR        IFEQ      '1'
250400941006     C                   SETON                                        21
250500941006     C                   ELSE
250600941006     C                   MOVE      G02INV        AMGDSA
250700941006     C                   MOVE      G02DAT        VDSA
250800941006     C     AMGDSA        COMP      BLPDSP                               21
250900960308      *
251000960308     C  N21GCPDCG        IFNE      *ZEROS
251100960308     C     AMGDSA        COMP      GCPDCG                             21
251200960308     C                   ELSE
251300960308     C     GCPDED        IFNE      *ZEROS
251400960308     C     AMGDSA        COMP      GCPDED                             21
251500960308     C                   ELSE
251600960308     C     AMGDSA        COMP      AMGUDT                             21
251700960308     C                   ENDIF
251800960308     C                   ENDIF
251900960308      *
252000941006     C                   END
252100900427     C   21              SETON                                        5319
252200900426     C* CONTROLLO DA DATA A DATA CONGRUENZA
252300900427     C  N21AMGDSD        COMP      AMGDSA                             21
252400900427     C   21
252500900427     CANN19              SETON                                        5419
252600920217     C* SE NON ADDEBITO SPESE NON SEGNALO ERRORI SU DATE SOSTA
252700990930     C     VTFT          IFEQ      9
252800920217     C                   SETOFF                                       19
252900920217     C                   SETOFF                                       525354
253000920217     C                   END
253100911217     C   77
253200950308     COR 19              GOTO      ENDCT6
253300990628     C                   MOVE      *ZEROS        §TOTSP           13 3
253400950120     C*
253500950120     C     VSGF          IFNE      SAVSGF
253600950120     C     VGGS          ORNE      SAVGGS
253700950309     C                   MOVE      *ZEROS        VTSGS
253800950309     C                   EXSR      TGCCAL
253900990930     C* converto le spese di sosta
254000990930     C     LSTDIV        IFNE      WGCDIV
254100990930     C                   CLEAR                   YEURDS
254200990930     C                   MOVEL     WGCDIV        YECDVI
254300990930     C                   Z-ADD     VTSGS         YECIMI
254400990930     C                   MOVEL     TASDIV        YECDVO
254500011220     C* SE CONVERTO IN LIRE FORZO IL NUMERO DEI DECIMALI = 0
254600011220     C     YECDVO        IFEQ      'ITL'
254700011220     C                   MOVEL     '0'           YECDCO
254800011220     C                   ELSE
254900011220     C                   MOVEL     '3'           YECDCO
255000011220     C                   END
255100990930     C                   MOVEL     'H'           YECTAR
255200990930     C                   CALL      'YEURCO'
255300990930     C                   PARM                    YEURDS
255400991001     C                   Z-ADD     YECIMO        VTSGS
255500990930     C                   END
255600950120     C                   MOVE      VGGS          SAVGGS
255700950120     C                   MOVE      VSGF          SAVSGF
255800100708     c*
255900100708     c                   clear                   §sgr
256000100708     C                   Z-ADD     tgcsgv        §sgv
256100060110 1   C     TGCFAF        IFEQ      '2'
256200060110     C     VGGS          ANDLE     VSGF
256300100708     C                   Z-ADD     0             §SGR
256400100708     C                   Z-ADD     0             §SGv
256500060110     C                   Z-ADD     0             VSGD
256600060110     C                   MOVE      *ZEROS        VSGA
256700060110     C                   Z-ADD     0             Vtsgs
256800060110x1   C                   ELSE
256900060110 2   C     TGCFAF        IFEQ      '3'
257000060110     C     VGGS          ANDLE     VSGF
257100060110     C                   Z-ADD     0             Vtsgs
257200060110     C                   MOVE      *ZEROS        VSGD
257300060110     C                   MOVE      *ZEROS        VSGA
257400100708     C                   Z-ADD     0             §SGv
257500100708     C* SOLO SE NON RESO IMPOSTO SPESE RICONSEGNA
257600100708 4   C     GCPDIS        IFne      'R'
257700060110     C     BLPFIN        IFNE      'C'
257800110623     C     BLPFIN        andne     'T'
257900100708     C                   Z-ADD     TGCSGP        §SGR
258000060110     C                   ELSE
258100100708     C                   Z-ADD     TGCSGR        §SGR
258200060110     C                   END
258300100708     C                   END
258400060110x2   C                   ELSE
258500060110 3   C     TGCFAF        IFEQ      '1'
258600060110     C     VGGS          ANDLE     VSGF
258700100708     C* SOLO SE NON RESO IMPOSTO SPESE RICONSEGNA
258800100708 4   C     GCPDIS        IFne      'R'
258900060111     C     BLPFIN        IFNE      'C'
259000110623     C     BLPFIN        andne     'T'
259100100708     C                   Z-ADD     TGCSGP        §SGR
259200060111     C                   ELSE
259300100708     C                   Z-ADD     TGCSGR        §SGR
259400060111     C                   END
259500100708     C                   END
259600060110     C                   Z-ADD     0             Vtsgs
259700060110x3   C                   ELSE
259800100708     C                   Z-ADD     TGCSGD        VSGD
259900100708     C* Se ho dato disposizioni di reso non devo calcolare
260000100708     C* le spese di riconsegna
260100100708 4   C     GCPDIS        IFne      'R'
260200950120     C* SPESE RICONSEGNA
260300060110 5   C     BLPFIN        IFNE      'C'
260400110623 5   C     BLPFIN        andne     'T'
260500060110     C                   Z-ADD     TGCSGP        §SGR
260600060110x5   C                   ELSE
260700060110     C                   Z-ADD     TGCSGR        §SGR
260800060110e5   C                   END
260900100708e5   C                   END
261000100708e2   C                   END
261100100708e1   C                   END
261200100708     C                   END
261300100708     C*
261400950120     C     §SGR          MULT      §NRG          §SGR
261500950120     C* SPESE RICONS.AL QUINTALE + SPESE VARIE
261600950120     C     §SGR          MULT      §PESCA        §SGR
261700100708     C     §SGR          ADD       §sgv          VSGR
261800950120     C* MOLTIPLICO SPESE DOSSIER    X NR. RIAPERTURE
261900031027     C     VSGD          MULT      §NRGDoss      VSGD
262000990930     C* converto le spese giacenza
262100060109 5   C     LSTDIV        IFNE      WGCDIV
262200990930     C* converto le spese di riconsegna
262300990930     C                   CLEAR                   YEURDS
262400990930     C                   MOVEL     WGCDIV        YECDVI
262500990930     C                   Z-ADD     VSGR          YECIMI
262600990930     C                   MOVEL     TASDIV        YECDVO
262700011220     C* SE CONVERTO IN LIRE FORZO IL NUMERO DEI DECIMALI = 0
262800060109 6   C     YECDVO        IFEQ      'ITL'
262900011220     C                   MOVEL     '0'           YECDCO
263000060109x6   C                   ELSE
263100011220     C                   MOVEL     '3'           YECDCO
263200060109e6   C                   END
263300990930     C                   MOVEL     'H'           YECTAR
263400990930     C                   CALL      'YEURCO'
263500990930     C                   PARM                    YEURDS
263600991001     C                   Z-ADD     YECIMO        VSGR
263700990930     C* converto le spese di dossier
263800990930     C                   CLEAR                   YEURDS
263900990930     C                   MOVEL     WGCDIV        YECDVI
264000990930     C                   Z-ADD     VSGD          YECIMI
264100990930     C                   MOVEL     TASDIV        YECDVO
264200011220     C* SE CONVERTO IN LIRE FORZO IL NUMERO DEI DECIMALI = 0
2643000601096    C     YECDVO        IFEQ      'ITL'
264400011220     C                   MOVEL     '0'           YECDCO
264500060109x6   C                   ELSE
264600011220     C                   MOVEL     '3'           YECDCO
264700060109e6   C                   END
264800990930     C                   MOVEL     'H'           YECTAR
264900990930     C                   CALL      'YEURCO'
265000990930     C                   PARM                    YEURDS
265100991001     C                   Z-ADD     YECIMO        VSGD
265200060109e5   C                   END
265300060109e4   C                   END
265400920522     C*
265500900502     C     VTSGS         ADD       VSGR          §TOTSP
265600900502     C     VSGD          ADD       §TOTSP        §TOTSP
265700911022     C     VSGA          ADD       §TOTSP        §TOTSP
265800900510     C* METTO IN RI I GG SOSTA SE LI HANNO CAMBIATI
265900900510     C* SE CONFERMANO LA SECONDA VOLTA NON CONTROLLO
266000941006     C     *INKF         IFEQ      '1'
266100900619     C   61§TOTSP        IFEQ      *ZEROS
266200970919      * SE SPESE GIAC.= ZERO E DEVO RECUPERARE IL TRASPORTO FORZO 0
266300970919      * E SALTO PERCHè è LA 2° VOLTA (61 ON) E L'HO GIà SEGNALATO
266400970919     C     GCPNPC        IFEQ      ' '
266500970919     C     OLDTB1        ANDEQ     'FS'
266600970919     C     GCPNPC        OREQ      'F'
266700970919     C                   Z-ADD     0             VTFT
266800970919     C                   GOTO      ENDCT6
266900970919     C                   ELSE
267000970919      * SE SPESE GIAC.= ZERO E NON DEVO RECUPERARE IL TRASPORTO FORZO 9
267100970919     C                   Z-ADD     9             VTFT
267200900619     C                   GOTO      ENDCT6
267300970919     C                   ENDIF
267400900619     C                   END
267500900619     C     *IN58         CABEQ     '1'           SPESE
267600900510     C                   END
267700900510     C  N19VGGS          IFNE      §GGSAV
267800900510     C                   SETON                                        5819
267900900510     C                   END
268000900504     C*
268100900619     C     SPESE         TAG
268200060906     c                   if        d82nodsp <> *blank and
268300060906     c                             vtft = 0 and §totsp = 0
268400060906     c                   z-add     9             vtft
268500060906     c                   endif
268600900619     C* SPESE = 0 E  GIACENZA DA FATTURARE  LO SEGNALO
268700900619     C  N19VTFT          IFEQ      *ZEROS
268800950308     C     GCPASG        ANDNE     'N'
268900060330     C     d82nodsp      ANDeq     *blank
269000960226     C     §TOTSP        COMP      *ZEROS                               2121
269100960226     C   21VRTR          COMP      *ZEROS                               2121
269200960603     C   21VRCA          COMP      *ZEROS                               2121
269300960226     C   21              SETON                                        501961
269400900619     C                   END
269500951113     C* CONTROLLO TIPO COMUNIC. GIACENZA
269600951113     C     *IN19         IFEQ      '0'
269700960618     C                   MOVE      WSCM          W8899             4 0
269800960201     C                   Z-ADD     1             §F
269900960201     C     V4CTFG        LOOKUP    C2H(§F)                                39
270000960201     C     *IN39         IFEQ      '1'
270100960201     C                   MOVEL     D2H(§F)       DS2H
270200960201     C     §2HFAX        IFEQ      'F'
270300960618     C     W8899         IFEQ      *ALL'8'
270400960618     C     W8899         OREQ      *ALL'9'
270500960201     C                   SETON                                        2841
270600960201     C                   MOVEL     ERR(2)        $MSG
270700960201     C                   GOTO      ENDCT6
270800960201     C                   ELSE
270900960314     C                   CLEAR                   TRUL43
271000960314     C                   MOVE      WSCM          D43KSC
271100960314     C                   CALL      'TRUL43R'
271200960314     C                   PARM                    TRUL43
271300960201     C                   CLEAR                   TRUL42
271400960201     C                   MOVEL     INDTLF        D42FAX
271500960314     C     D43TRV        IFEQ      'S'
271600960314     C                   MOVEL     D43FAX        D42FAX
271700960314     C                   END
271800960201     C                   CALL      'TRUL42R'
271900960201     C                   PARM                    TRUL42
272000960201     C     D42ERR        IFEQ      '1'
272100960201     C                   SETON                                        2841
272200960201     C                   MOVEL     ERR(3)        $MSG
272300960201     C                   GOTO      ENDCT6
272400960201     C                   END
272500960201     C                   END
272600960201     C                   END
272700960201     C                   ELSE
272800960201     C                   SETON                                        284119
272900960201     C                   MOVEL     ERR(1)        $MSG
273000951113     C                   END
273100960201     C                   END
273200990716     C*
273300990716     C* SE LA DIVISA NON AMMETTE DECIMALI E SONO STATI IMMESSI SEGNALO
273400990716     C* ERRORE
273500990716     C                   Z-ADD     1             §F
273600990716     C     V4CDIV        LOOKUP    CCV(§F)                                31
273700990716     C     FDC(§F)       IFNE      'S'
273800990716     C                   MOVE      VTSGS         WDEC3             3 0
273900990716     C     WDEC3         IFGT      0
274000990716     C                   SETON                                        4619
274100990716     C                   GOTO      ENDCT6
274200990716     C                   END
274300990716     C                   MOVE      VSGR          WDEC3
274400990716     C     WDEC3         IFGT      0
274500990716     C                   SETON                                        4619
274600990716     C                   GOTO      ENDCT6
274700990716     C                   END
274800990716     C                   MOVE      VSGD          WDEC3
274900990716     C     WDEC3         IFGT      0
275000990716     C                   SETON                                        4619
275100990716     C                   GOTO      ENDCT6
275200990716     C                   END
275300990716     C                   MOVE      VSGA          WDEC3
275400990716     C     WDEC3         IFGT      0
275500990716     C                   SETON                                        4619
275600990716     C                   GOTO      ENDCT6
275700990716     C                   END
275800990716     C                   MOVE      VRCA          WDEC3
275900990716     C     WDEC3         IFGT      0
276000990716     C                   SETON                                        4619
276100990716     C                   GOTO      ENDCT6
276200990716     C                   END
276300990716     C                   MOVE      VRTR          WDEC3
276400990716     C     WDEC3         IFGT      0
276500990716     C                   SETON                                        4619
276600990716     C                   GOTO      ENDCT6
276700990716     C                   END
276800990716     C                   END
276900990908     C* Se la fattura ha un totale imponibile inferiore a quello
277000990908     C* che risulterebbe dalle giacenze lo segnalo
277100990908     C     VTFT          IFEQ      0
277200990908     C     VNBW          ANDNE     0
277300990908     C     VNBW          ANDNE     *ALL'9'
277400990908     C     VDFT          ANDNE     0
277500990908     C* Calcolo il totale imponibile in base all'addebito spese
277600990908     C* giacenze
277700990908     C                   Z-ADD     VRTR          WTOBOL
277800990908     C                   ADD       VRCA          WTOBOL
277900990908     C                   ADD       §TOTSP        WTOBOL
278000990908     C     WTOBOL        IFNE      WSVTOT
278100990930     C     WTOBOL        ANDGT     AR6IMV
278200990930     C* verico differenza, se nell'ordine di 2 o 3
278300990930     C* decimali può essere dovuto ad arrotondamento quindi non
278400990930     C* do errore
278500990930     C     WTOBOL        SUB       AR6IMV        WDIFF            11 3
278600990930     C     WDIFF         IFGT      0,019
278700990908     C                   MOVEL     ERR(11)       $MSG
278800990908     C                   SETON                                        28
278900990908     C                   END
279000990930     C                   END
279100990908     C                   END
279200990910     C                   Z-ADD     WTOBOL        WSVTOT           13 3
279300990908     C*
279400900426     C     ENDCT6        ENDSR
279500900426     C/SPACE 3
279600900418     C*---------------------------------------------------------------*
279700900504     C*   B L P W T R                                                 *
279800900418     C*---------------------------------------------------------------*
279900900504     C     BLPWTR        BEGSR
280000941005     C*
280100990930     C                   CLEAR                   DBLP
280200990930     C                   CLEAR                   DTASV
280300941228     C                   MOVEL     'C'           BLPTSP
280400941228     C                   MOVE      AMGUDT        §LPMGS
280500941228     C                   MOVEL     AMGUDT        §LPAAS
280600941228     C                   Z-ADD     AMGUDT        §LPDRT
280700950102     C                   MOVEL     'P'           §LPFPR
280800941005     C                   MOVE      VKSC          §LPCCM
280900090609     C                   MOVE      VKSC          §LPksca
281000941005     C                   MOVE      VKSC          §LPCCN
281100950102     C                   MOVE      BLPLNP        §LPLNP
281200941005     C                   MOVEL     §2GCBO        §LPCBO
281300941005     C                   MOVEL     '$'           §LPSOP
281400950102     C                   MOVE      BLPLNA        §LPLNA
281500950615     C                   MOVE      BLPQFT        §LPQFT
281600941005     C                   MOVEL     VRSDB         §LPRSM
281700941005     C                   MOVEL     VINDB         §LPINM
281800941005     C                   MOVEL     VCADB         §LPCAM
281900941005     C                   MOVEL     VLODB         §LPLOM
282000941005     C                   MOVEL     VPRDB         §LPPRM
282100941005     C                   MOVEL     V3CNZM        §LPNZM
282200941005     C                   MOVEL     BLPCPI        §LPCPI
282300941005     C                   MOVEL     BLPRSD        §LPRSD
282400941005     C                   MOVEL     RD2           §LPRD2
282500941005     C                   MOVEL     BLPIND        §LPIND
282600941005     C                   MOVEL     BLPCAD        §LPCAD
282700941005     C                   MOVEL     BLPLOD        §LPLOD
282800941005     C                   MOVEL     BLPPRD        §LPPRD
282900941005     C                   MOVEL     BLPNZD        §LPNZD
283000941005     C                   MOVE      BLPGC1        §LPGC1
283100941005     C                   MOVE      BLPGC2        §LPGC2
283200941005     C                   MOVE      BLPCTR        §LPCTR
283300090609     C                   MOVE      BLPCTR        §LPCTRa
283400941005     C                   MOVE      BLPCTS        §LPCTS
283500941005     C                   MOVEL     BLPNAS        §LPNAS
283600941005     C                   MOVE      BLPNCL        §LPNCL
283700941005     C                   MOVE      BLPPKB        §LPPKB
283800960229     C                   MOVE      BLPVLF        §LPVLB
283900960229     C                   MOVE      BLPFVF        §LPFVB
284000960229     C*--------------------------------------------------------*
284100960229     C*  Se il flag del volume bollettato è  'C' o 'K' oppure
284200960229     C*  è  'P' ed il cliente è codificato imposto 'R' in modo
284300960229     C*  che non venga più ricalcolato
284400960229     C                   MOVE      VKSC          §8899             4 0
284500960229     C     BLPFVF        IFEQ      'C'
284600960229     C     BLPFVF        OREQ      'K'
284700960229     C     BLPFVF        OREQ      'P'
284800960229     C     §8899         ANDEQ     8888
284900960229     C     BLPFVF        OREQ      'P'
285000960229     C     §8899         ANDEQ     9999
285100960229     C                   MOVEL     'R'           §LPFVB
285200960229     C                   END
285300960229     C*--------------------------------------------------------*
285400941005     C                   MOVE      BLPVLC        §LPVLC
285500941005     C                   MOVE      BLPNCR        §LPNCR
285600950119     C                   MOVE      BLPRMA        §LPRMA
285700950119     C                   MOVE      BLPRMN        §LPRMN
285800941005     C                   MOVEL     BLPRMO        §LPRMO
285900941005     C                   MOVEL     '4  '         §LPCTM
286000941005     C                   MOVE      'P'           §LPFPP
286100941228     C                   MOVEL     §LPLNP        §LPPDR
286200941005     C                   MOVE      999           §LPPDR
286300941005     C                   MOVEL     VFIV          §LPFIV
286400941005     C                   MOVEL     VNFT          §LPNFT
286500941005     C                   MOVE      AMGDFT        §LPDFT
286600990305     C     VRCA          IFGT      0
286700990305     C     *IN75         ANDEQ     '1'
286800990305     C                   MOVEL     §$2VRG        TASSV1
286900990305     C                   Z-ADD     VRCA          TASVA1
287000990305     C                   ELSE
287100990305     C     VRTR          IFGT      0
287200990305     C     *IN75         ANDEQ     '1'
287300990305     C                   ELSE
287400990305     C                   MOVEL     §$2VRI        TASSV1
287500941221     C     VTSGS         ADD       VSGR          TASVA1
287600941221     C                   ADD       VSGD          TASVA1
287700941221     C                   ADD       VSGA          TASVA1
287800960130     C*  Controllo se immesse provv. C/Assegno
287900960227     C     VRCA          IFGT      0
288000960130     C                   MOVEL     §$2VRG        TASSV2
288100960130     C                   Z-ADD     VRCA          TASVA2
288200960130     C                   END
288300990305     C*
288400990305     C                   END
288500990305     C                   END
288600030618     c* aggiungo la varia &
288700030618     C     GCPDIS        IFEQ      'C'
288800030618     C     GCPNPC        ANDEQ     'F'
288900030618     C     VDFT          ANDEQ     0
289000030618     C     Viddcli       ANDEQ     'C'
289100030618     C                   Z-ADD     1             §F
289200030618     C     §$2vpa        LOOKUP    NSV                                    30
289300030618     C     *IN30         IFEQ      *OFF
289400030618     C                   Z-ADD     1             §f
289500030618     C     ' '           LOOKUP    nSV(§f)                                30
289600030618     C                   END
289700030618     C   30              movel     §$2vpa        nsv(§f)
289800030623     C   30              z-add     88888888      nva(§f)
289900030618     c                   end
290000960130     C*  Controllo se immesse spese trasporto
290100960130     C                   Z-ADD     VRTR          §LPPOR
290200951212     C* Se ho eseguito il cambio di porto non imposto il
290300951212     C* totale imponibile x calcolare trasporto in sede
290400960229     C     GCPDIS        IFEQ      'C'
290500960229     C     GCPNPC        ANDEQ     'F'
290600960226     C     VDFT          ANDEQ     0
290700951212     C     GCPNPC        OREQ      ' '
290800960130     C     GCPDIS        ANDEQ     'C'
290900951212     C     OLDTB1        ANDEQ     'FS'
291000960226     C     VDFT          ANDEQ     0
291100040211     c     §lppor        orgt      0
291200951212     C                   Z-ADD     0             §LPIMV
291300951212     C                   ELSE
291400951212     C                   Z-ADD     TASVA1        §LPIMV
291500960130     C                   ADD       TASVA2        §LPIMV
291600960130     C                   ADD       §LPPOR        §LPIMV
291700951212     C                   END
291800920212     C                   MOVEL     'GIAC.'       TESTO1
291900920212     C                   MOVE      GCPAGC        GCAAGC
292000920212     C                   MOVE      GCPFGC        GCAFGC
292100920212     C                   MOVE      GCPNGC        GCANGC
292200920212     C                   MOVEL     'LNP. '       TESTO2
292300920212     C                   MOVE      GCPLNP        VARIO
292400941006     C                   MOVEL     RIFGIA        §LPNOT
292500941006     C* RICH. IMMISS. BOLLE
292600030717     C* OVRPRTF  BOLLE OLD
292700950123     C                   Z-ADD     80            LUNCMD           15 5
292800941006     C                   MOVEA     D82PRB        CCO(28)
292900950123     C                   MOVEA     D82MOB        CCO(61)
293000941006     C                   CALL      'QCMDEXC'
293100941006     C                   PARM                    CCO
293200941006     C                   PARM                    LUNCMD
293300030717     C* OVRPRTF  BOLLE A4
293400030717     C                   MOVEA     D82PRB4       CM4(30)
293500030717     C                   MOVEA     D82MDB4       CM4(63)
293600030717     C                   CALL      'QCMDEXC'
293700030717     C                   PARM                    CM4
293800030717     C                   PARM                    LUNCMD
293900030717     C* OVRPRTF  BOLLE A5
294000030717     C                   MOVEA     D82PRB5       CM5(30)
294100030717     C                   MOVEA     D82MDB5       CM5(63)
294200030717     C                   CALL      'QCMDEXC'
294300030717     C                   PARM                    CM5
294400030717     C                   PARM                    LUNCMD
294500991206     C* OVRPRTF  BOLLE  DPD
294600991206     C                   MOVEA     D82PRB        ODP(29)
294700991206     C                   CALL      'QCMDEXC'
294800991206     C                   PARM                    ODP
294900991206     C                   PARM                    LUNCMD
295000030304     C* OVRPRTF  BOLLE  FedEx
295100030307     c                   Movel     *Blanks       comman           90
295200030307     C                   Z-ADD     90            LUNCMD           15 5
295300030304     C                   MOVEA     D82PRB        Ofe(29)
295400030307     c                   Movea     Ofe           Comman
295500030307     c                   Eval      %Subst(Comman:74:9) = 'COPIES(2)'
295600030304     C                   CALL      'QCMDEXC'
295700030307     c                   PARM                    Comman
295800030304     C                   PARM                    LUNCMD
295900941006     C* OVRPRTF  ETICHETTE
296000950123     C                   Z-ADD     80            LUNCMD           15 5
296100941006     C                   MOVEA     D82PRE        CME(28)
296200950123     C                   MOVEA     D82MOE        CME(61)
296300941006     C                   CALL      'QCMDEXC'
296400941006     C                   PARM                    CME
296500941006     C                   PARM                    LUNCMD
296600970327     C* Reperisco flag x stampare o meno bolla in partenza
296700970327     C                   MOVEL     BLPFST        §LPBAM
296800970407     C* PASSO FLAG CHE E' GIACENZA
296900970407     C                   MOVEL     'G'           §LPRIC
297000990930     C* Nuovi campi dblp
297100990930     C                   MOVEL     *BLANKS       §LPFLG
297200990930     C                   MOVEL     *BLANKS       §LPLPA
297300990930     C                   MOVEL     *BLANKS       §LPFLO
297400991129     C     TASVA1        IFGT      0
297500991201     C     TASVA2        ORGT      0
297600991129     C     §LPPOR        ORGT      0
297700991020     C                   MOVEL     V4CDIV        §LPDIV
297800991129     C                   END
297900030821     c* in caso di particolarità consegna "RC"  devo verificare
298000030821     c* se passare la particolarità x non fare scrivere il fiar500f nel
298100030821     c* pgm fnls01r inquanto lo scriverò in questo pgm.
298200100517     c                   if        blpgma <> *blank
298300100517     C                   MOVEL     '7R'          TBLCOD
298400100517     C                   MOVEL     *BLANK        TBLKEY
298500100517     C                   MOVEL     blpGMA        TBLKEY
298600100517     C     KTAB          CHAIN     TABEL                              21
298700100517     C     *IN21         IFEQ      '0'
298800100517     C                   MOVEL     TBLUNI        DS7R
298900100517     C                   if        §7Rrc ='S'
299000030821     c                   eval      compar = blpgma
299100030821     c                   exsr      srpar
299200030821     c                   eval      §lpgma = compar
299300030821     c                   end
299400100517     c                   end
299500100517     c                   end
299600030821     c* in caso di particolarità varie "O" devo verificare se passare
299700030821     c* la particolarità x non fare scrivere il fiar500f nel pgm fnls01r
299800030821     c* inquanto lo scriverò in questo pgm.
299900031211     c                   Clear                   dAr5BNB
300000030821     c                   If        %Subst(blpgva:1:1) = 'O'
300100031209     C                   move      'BNB'         KAR5TRD
300200031209     c     kFiar5        Chain     Fiar501l
300300031209     c                   If        %Found(Fiar501l)
300400031209     c                   Movel     Ar5Uni        dAr5BNB
300500031209     c                   EndIf
300600031209     c                   endif
300700031209     c                   z-add     §AR5bTBA      §lpTBA
300800031209     c                   z-add     §AR5bNBA      §lpNBA
300900031209     c                   z-add     §AR5bTB2      §lpTB2
301000031209     c                   z-add     §AR5bNB2      §lpNB2
301100031209     c                   Eval      §lpcor = §Ar5Bcor
301200031209     c                   Eval      §lpcsf = §Ar5Bcsf
301300031212     c                   Eval      §lpgva = blpgva
301400040122      *
301500040122     c                   if        §lpimv > 0
301600040122     c                   exsr      ritassa
301700040122     c                   endif
301800941006     C*
301900941005     C                   CALL      'FNLS01R'
302000941005     C                   PARM                    KPJBA
302100990930     C                   PARM                    DBLP
302200990930     C                   PARM                    DTASV
302300030717     C                   PARM                    TRUL90DS
302400030821     c* scrive fiar500f
302500030821     c                   exsr      AGGAR5
302600950103     C*
302700950103     C                   MOVE      §LPNSP        VNBW
302800950103     C                   MOVE      §LPNRS        VNSW
302900950103     C                   MOVE      §LPLNP        VLNW
303000950103     C                   MOVE      §LPAAS        VASW
303100941006     C* MI RITORNA IL NR. BOLLA CREATO
303200941006     C* DLTOVR
303300941006     C                   Z-ADD     70            LUNCMD           15 5
303400941006     C                   CALL      'QCMDEXC'
303500941006     C                   PARM                    DOV
303600941006     C                   PARM                    LUNCMD
303700941005     C*
303800900504     C                   ENDSR
303900030821     C*---------------------------------------------------------------*
304000030821     C*   srpar : Controllo se devo abblencare la particolarità       *
304100030821     C*---------------------------------------------------------------*
304200030821     C     srpar         BEGSR
304300030821     c                   eval      tbecod = 'AR5'
304400030821     c                   clear                   tbeke2
304500030821     c                   eval      tbeke1 = 'RCC'
304600030821     C     ktbe          chain     TNTBE01L
304700030821     c                   if        %found(tntbe01l) and tbeatb = ' '
304800030821     C                   MOVEL     TBEUNI        DAR5
304900030821     c                   if        §ar5drec='N'
305000030821     c                   clear                   compar
305100030821     C                   END
305200030821     C                   END
305300030821     C                   ENDSR
305400030821     C*---------------------------------------------------------------*
305500030821     C* AGGAR5: Controllo se devo duplicare fiar500f                  *
305600030821     C*---------------------------------------------------------------*
305700030821     C     aggar5        BEGSR
305800030821     c                   if        §lpnsp <> 0
305900030821     c                   do        30            bb
306000030821     c                   if        ar5(bb) = *blanks
306100030821     c                   leave
306200030821     c                   end
306300030821     C                   movel     ar5(bb)       kar5trd
306400030821     c     kFiar5        Chain     Fiar501l
306500030821     c                   If        %Found(Fiar501l)
306600030821     c                   clear                   ar5prg
306700030911     c                   eval      ar5ft1 = 'T'
306800030911     c                   eval      ar5dt1 = 99999999
306900030911     c                   eval      ar5ft2 = 'T'
307000030911     c                   eval      ar5dt2 = 99999999
307100030821     c                   clear                   ar5ft3
307200030821     c                   clear                   ar5dt3
307300030821     c                   move      amgudt        ar5dac
307400030821     C                   TIME                    ar5ORc
307500030821     c                   eval      ar5aas = §lpaas
307600030821     c                   eval      ar5lnp = §lplnp
307700030821     c                   eval      ar5nrs = §lpnrs
307800030821     c                   eval      ar5nsp = §lpnsp
307900030821     c                   write     fiar50
308000030821     c                   end
308100030821     c                   enddo
308200030821     c                   end
308300030821     C                   ENDSR
308400040122     C*---------------------------------------------------------------*
308500040122     C*   richiama nuovamente tassatore per varie aggiuntive          *
308600040122     C*---------------------------------------------------------------*
308700040122     C     ritassa       BEGSR
308800040129     C                   CLEAR                   DTAS
308900040129     C                   MOVEL     BLPCTR        TASCTR
309000040129     C                   MOVE      VKSC          TASKSC
309100040129     C*
309200040129     C                   MOVEL     BLPCTS        TASCTS
309300040129     C                   MOVEL     BLPCAD        TASCAD
309400040129     C                   MOVEL     BLPFIN        TASFIN
309500040129     C                   MOVEL     BLPCAM        TASCAM
309600040129     C                   MOVEL     BLPNZM        TASNZM
309700040129     C                   MOVEL     BLPFAP        TASFAP
309800040129     C                   MOVEL     BLPTSP        TASTSP
309900040129     C                   MOVEL     BLPTC1        TASTC1
310000040129     C                   MOVEL     BLPTC2        TASTC2
310100040129     C                   MOVEL     BLPFDN        TASFDN
310200040129     C                   MOVEL     BLPVAS        TASVAS
310300040129     C                   Z-ADD     BLPIAS        TASIAS
310400040129     C                   Z-ADD     BLPQFT        TASQFT
310500040129     C                   Z-ADD     BLPNCL        TASNCL
310600160530     C                   Z-ADD     BLPNCL        TASNCLb
310700040129     C                   Z-ADD     DATDSP        TASDSP
310800040129     C                   Z-ADD     TASDSP        TASDCT
310900040129     C                   Z-ADD     BLPLNP        TASLNP
311000040129     C                   Z-ADD     BLPLNA        TASLNA
311100040129     C                   Z-ADD     BLPVLF        TASVLF
311200040129     C                   Z-ADD     BLPPKF        TASPKF
311300160530     C                   Z-ADD     BLPvlc        TASvlc
311400160530     C                   Z-ADD     BLPncr        TASncr
311500160530     C                   Z-ADD     BLPpkb        TASPKb
311600160530     C                   move      BLPfvf        TASfvf
311700040129     C* Reperisco codice tassazione
311800040129     C                   CLEAR                   TISI95
311900040129     C                   MOVEL     '3'           I95TCN
312000040129     C                   MOVEL     BLPCAM        I95CAP
312100040129     C                   MOVEL     BLPNZM        I95NAR
312200040129     C                   MOVEL     BLPPRM        I95PRV
312300040129     C                   MOVEL     BLPLOM        I95LOC
312400040129     C                   MOVEL     BLPINM        I95IND
312500040129     C                   MOVEL     BLPTFP        I95TFP
312600040129     C                   MOVEL     BLPTSP        I95TSP
312700040129     C                   MOVEL     'S'           I95FRE
312800040129     C                   MOVEL     BLPNZM        I95NAR
312900040129     C                   MOVEL     DATDSP        I95DAT
313000040129      *SOLO PER FEDEX IMPOSTA IL FLAG = S
313100040129     C     NETWRK        IFEQ      'FED'
313200040129     C                   MOVE      'S'           I95FI1
313300040129     C                   ELSE
313400040129     C                   MOVE      *BLANK        I95FI1
313500040129     C                   END
313600040129     C                   CALL      'TISI95R'
313700040129     C                   PARM                    TISI95
313800040129     C                   MOVEL     O95CTS        TASMCT
313900040129     C*
314000051130     C                   MOVEL     §GEDCN        TASVCA
314100060927     C                   MOVEL     blpfbr        §asfbr
314200060927     C                   MOVEL     blpcca        §ascca
314300060927     C                   MOVEL     dtas01        TASFLO
314400040129     C                   Z-ADD     §ASS          TASCAS
314500051115     C                   MOVEL     ar9TIC        TASTIC
314600040129     c* imposto peso vdl e colli rilevati
314700040129     c                   z-add     blpncp        Tasncp
314800040129     c                   z-add     blppkc        Taspkc
314900040122     C*
315000040122     C                   MOVEL     'Z'           TASSVA
315100040122     C*
315200040122     C                   MOVEL     KPJBU         WSVKPJ
315300040122     C                   CALL      'TNSF20R'
315400040122     C                   PARM                    KPJBA
315500040122     C                   PARM                    DTAS
315600040122     C                   PARM                    DTASV
315700040122     C                   MOVEL     WSVKPJ        KPJBU
315800040122     C*
315900040122     C     TASERR        IFEQ      *BLANKS
316000040202     c                   z-add     0             §lpimv
316100040122     C* Scorro le varie e le sommo nel totale imponibile solo se il
316200040122     C* flag tab cc lo prevede
316300040122     C                   DO        20            WW                3 0
316400040122     C                   Z-ADD     1             XW                3 0
316500040122     C     NSV(WW)       LOOKUP    CCC(XW)                                20
316600040122     C     *IN20         IFEQ      '1'
316700040122     C     IPB(XW)       ANDEQ     'S'
316800040122     C                   ADD       NVA(WW)       §LPIMV
316900040122     C                   END
317000040122     C                   END
317100040122     C                   END
317200040122     C                   ENDSR
317300900504     C*---------------------------------------------------------------*
317400900504     C*   G C P W T R                                                 *
317500900504     C*---------------------------------------------------------------*
317600900504     C     GCPWTR        BEGSR
317700050316     C     KGIA          CHAIN(e)  TIGCP000                           40
317800050316     c                   if        %error
317900070521     C     KGIA          CHAIN(n)  TIGCP000
318000050316     c                   exsr      geslck
318100050318     C     KGIA          CHAIN(e)  TIGCP000                           40
318200050316     c                   endif
318201171002     c                   eval      tigcpb = tigcp
318300900423     C* DATA CHIUSURA GIACENZA
318400950103     C     AMGDCG        IFNE      0
318500900423     C                   Z-ADD     AMGDCG        GCPDCG
318600950103     C                   END
318700900504     C                   MOVEL     VCFG          GCPCFG
318800900423     C                   Z-ADD     VTFT          GCPTFT
318900900423     C                   Z-ADD     VSGR          GCPSGR
319000900423     C                   Z-ADD     VSGD          GCPSGD
319100900423     C                   Z-ADD     AMGDSD        GCPDSD
319200900423     C                   Z-ADD     AMGDSA        GCPDSA
319300900423     C                   Z-ADD     VGGS          GCPGGS
319400900423     C                   Z-ADD     VSGF          GCPSGF
319500911212     C                   Z-ADD     VSGA          GCPSGA
319600911212     C                   Z-ADD     VTSGS         GCPSGS
319700991112     C* SE CI SONO DELLE SPESE DI GIACENZA IMPOSTO LA DIVISA
319800991112     C     VSGF          IFGT      0
319900991112     C     VSGA          ORGT      0
320000991112     C     VTSGS         ORGT      0
320100000919     C     VSGR          ORGT      0
320200000919     C     VSGD          ORGT      0
320300000919     C     VSGA          ORGT      0
320400000919     C     VRTR          ORGT      0
320500000919     C     VRCA          ORGT      0
320600991112     C                   MOVEL     V4CDIV        GCPDIV
320700991112     C                   END
320800900619     C     VTFT          IFEQ      0
320900911109     C                   MOVE      VKSC          GCPKSC
321000911213     C                   Z-ADD     VLNW          GCPLPW
321100911213     C                   Z-ADD     VASW          GCPASW
321200911213     C                   Z-ADD     VNSW          GCPNSW
321300911213     C                   Z-ADD     VNBW          GCPNBW
321400000201     C                   MOVE      AMGUDT        GCPDBG
321500911210     C                   MOVE      VFIV          GCPFIV
321600911210     C                   MOVE      VNFT          GCPNFT
321700911210     C                   MOVE      AMGDFT        GCPDFT
321800900619     C                   ELSE
321900941006     C     GCPDIS        IFNE      'R'
322000950413     C     GCPNBW        IFEQ      0
322100950413     C     GCPNBW        OREQ      9999999
322200911109     C                   Z-ADD     0             GCPKSC
322300900620     C                   Z-ADD     0             GCPSGS
322400900620     C                   Z-ADD     0             GCPSGR
322500900620     C                   Z-ADD     0             GCPSGD
322600911109     C                   Z-ADD     0             GCPSGA
322700910207     C                   Z-ADD     0             GCPLPW
322800910207     C                   Z-ADD     0             GCPASW
322900910207     C                   Z-ADD     0             GCPNSW
323000910207     C                   Z-ADD     0             GCPNBW
323100000201     C                   MOVE      AMGUDT        GCPDBG
323200911210     C                   MOVE      *ZEROS        GCPFIV
323300911210     C                   MOVE      *ZEROS        GCPNFT
323400911210     C                   MOVE      *ZEROS        GCPDFT
323500950413     C                   END
323600950210     C                   ELSE
323700950210     C     VSGA          IFNE      *ZEROS
323800950210     C     VSGD          ORNE      *ZEROS
323900950210     C     VSGR          ORNE      *ZEROS
324000950210     C     VTSGS         ORNE      *ZEROS
324100950210     C                   Z-ADD     9999999       GCPNBW
324200900619     C                   END
324300950210     C                   END
324400911212     C                   END
324500900423     C*
324600911022     C* SE RICH. DA DSIPOSIZ. FASE = 20
324700050222     C     D82TRI        IFne      'D'
324800941010     C                   Z-ADD     050           GCPFAS
324900941010     C                   END
325000060329     C                   if        d82tri <> 'D' AND d82nodsp <> *blank
325100060329     C                   move      'S'           GCPchiaut
325200060329     C                   endif
325300941010     C*
325400950103     C                   MOVEL     V4CTFG        GCPTFG
325500050221     C  N40              UPDATE    TIGCP000
325501171002     c                   exsr      RegVariazioni
325600050316     c                   if        gescomm = 'S'
325700050316     c                   commit
325800050316     c                   endif
325900900418     C                   ENDSR
325901171002     C*__________________________________________________________
325902171002     c     RegVariazioni begsr
325903171002     C*__________________________________________________________
325904171002     c                   eval      ILG33CTA = 'M'
325905171002     c                   eval      ILG33PRU = knmus
325906171002     c                   eval      ILG33NOJ = knmeb
325907171002     c                   eval      ILG33PGM = 'FNLG14R'
325908171002     c                   eval      tigcpa = tigcp
325909171002     c                   call      'FNLG33R'
325910171002     c                   parm                    fnlg33ds
325911171002     c                   parm                    tigcpb
325912171002     c                   parm                    tigcpa
325913171002     c                   endsr
326000030717     C*---------------------------------------------------------------*
326100030717     C*   R O U T I N E   INIZIALE  x trul90ds                        *
326200030717     C*---------------------------------------------------------------*
326300030717     C     srtrul90      BEGSR
326400030717     c                   clear                   trul90ds
326500030717     c                   eval      d90mde = d82moe
326600030717     c                   eval      d90mdb = d82mob
326700030717     c                   eval      d90prb = d82prb
326800030717     c                   eval      d90pre = d82pre
326900030717     c                   eval      d90pss = d82pss
327000030717     c                   eval      d90mdb4 = d82mdb4
327100030717     c                   eval      d90mdb5 = d82mdb5
327200030717     c                   eval      d90psl = d82psl
327300030717     c                   eval      d90bsl = d82bsl
327400030717     c                   eval      d90prb4 = d82prb4
327500030717     c                   eval      d90prb5 = d82prb5
327600030717     c                   endsr
327700050316     C*----------------------------------------------------*
327800050316     C     geslck        begsr
327900050316     C*----------------------------------------------------*
328000050316     c                   Eval      ul82§rrn = gcpnrr
328100050316     c                   Eval      UL82§FIL = 'TIGCP00F'
328200050316     c                   Eval      UL82§WIN = 'S'
328300050316     c                   Eval      UL82§F7  = 'S'
328400050316     c                   Eval      UL82§num = 3
328500050316     c                   Eval      UL82§att = 3
328600050404     c                   Eval      UL82§mss = 'Si sta bloccando la giacenza Nr.'
328700050404     c                             + %editc(kagc:'Z') +  ' '+ %editc(kfgc:'Z')
328800050404     c                             + ' ' +  %editc(kngc:'Z') + ' terminare il -
328900050404     c                             lavoro.'
329000050404     c                   Eval      UL82§msw = 'La giacenza Nr. '
329100050404     c                             + %editc(kagc:'Z') +  ' '+ %editc(kfgc:'Z')
329200050404     c                             + ' ' +  %editc(kngc:'Z') +
329300050404     c                             ' non è manutenzionabile'
329400050316      * Effettuo la chiamata al *PGM d utilità
329500050316     c                   Call(e)   'TRUL82R'
329600050316     c                   Parm                    Trul82ds
329700050316     c                   if        ul82§sts = 'A'
329800050316     c                   goto      fine
329900050316     c                   endif
330000050316
330100050316     C                   ENDSR
330200031027     C*---------------------------------------------------------------*
330300941223     OFNLV22P   E            FINTO
330400990908** CBO
330500991206OVRPRTF FILE(FNLSB0P) OUTQ(          ) SAVE(*YES) FORMTYPE('          ')
330600991206** ODP
330700991206OVRPRTF FILE(FNLSB0P1) OUTQ(          ) SAVE(*YES) FORMTYPE('BOLLE-dpd')
330800030304** Ofe
330900030304OVRPRTF FILE(FNLSB0P2) OUTQ(          ) SAVE(*YES) FORMTYPE('BOLLE-Fed')
331000941006** CME
331100950123OVRPRTF FILE(FNLV22P) OUTQ(          ) SAVE(*YES) FORMTYPE('          ')
331200941006** DOV
331300941006DLTOVR  FILE(*ALL)
331400030717** CM4
331500030721OVRPRTF FILE(FNLSB5PA4) OUTQ(          ) SAVE(*YES) FORMTYPE('          ')
331600030717** CM5
331700030721OVRPRTF FILE(FNLSB5PA5) OUTQ(          ) SAVE(*YES) FORMTYPE('          ')
331800951113** ERR
331900951113Tipo comunicazione fine giacenza errato
332000960201Tp.invio comunicazione chiusura non utilizzabile per clienti generici
332100960201Tp.invio comunicazione chiusura non utilizzabile: numero fax errato
332200990908Immettere dati bolla di recupero spese giacenza solo se devo fatturare
332300990908Immettere tutti i dati della bolla di recupero
332400990908Punto operativo inesistente
332500990908Numero di spedizione inesistente
332600990922Spedizione non confermata: immettere codice padroncino ritiro
332700990908Spedizione non fatturata
332800990910Rf.mittente numerico bolla di recupero diverso dalla rf. sped.giacente
332900990910Importo spese calcolato superiore a quello recuperato
333000990908Indicare bolla di recupero per prepagato con addebito spese giac.
