000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200070316     h dftactgrp(*no) actgrp(*caller)
000300070316
000400070416      * Fidg97R *----------------------------------------------------*
000500070403      *     ?   - ASSEGNAZIONE MANUALE spedizioni da consegnare     ?
000600070403      *--------------------------------------------------------------*
000700070403      * Se il p.o. è in parallelo testo la distinta su ARG altrimenti
000800070403      * Se il p.o. è partito testo la distinta su ARB
000900070227      *--------------------------------------------------------------*
001000940224      *
001100940307      *  21           GENERICO OPERAZIONI I/O
001200940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001300070301      *  23           GENERICO Operazioni varie
001400940224      *  30           SFLDSP
001500940224      * N31           SFLCLR
001600940224      *  31           SFLDSPCTL
001700940224      *  32           SFLNXTCHG
001800940224      *  33           SFLEND
001900940224      *  39           OF PRTF
002000070302      *  40 <---> 49  DSPATR SU video
002100940608      *  Specificare l'uso dei singoli indicatori
002200070227      *  50           Emissione della window totale/parziale
002300070227      *  51 <---> 90  ERRORI SU VIDEO
002400940608      *  Specificare l'uso dei singoli indicatori
002500940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002600940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002700940224      *  99           INDIC. GENERALE DI ERRORE
002800940128     F*----------------------------------------------------*
002900070404      *  Output
003000070316     Ffiarg01l  uF   E           K DISK    commit infds(argds)
003100070319     Ffnarb01l  uF   E           K DISK    commit infds(arbds)
003200070316     Ffidst01l  uf   e           k disk    commit infds(dstds)
003300070404      *
003400070404      *  Input
003500070404     Ffnarb70l  iF   E           K DISK    rename(fnarb000:fnarb70)
003600070406     Ffidst08l  if   e           k disk    rename(fidst000:fidst08)
003700070302     Ffnlbl01l  IF   E           K DISK
003800070302     Ftabel00f  IF   E           K DISK
003900070307     Ftntbe01L  IF   E           K DISK
004000070301     Ffiari01l  if   e           k disk
004100070301     Ffiar601l  if   e           k disk
004200070301     Ffiar901l  if   e           k disk
004300070228     Ffiapd01l  if   e           k disk
004400070228     Ffigrg03l  if   e           k disk
004500070228     Fazorg01l  IF   E           K DISK
004600070227      *
004700070416     Ffidg97d   CF   E             WORKSTN
004800070227     F                                     SFILE(FDG0S02:S2NRR)
004900070309     F                                     SFILE(FDG0S03:S3NRR)
005000940201     F                                     INFDS(DSFMT)
005100070305     D*----------------------------------------------------*
005200070326     D Errmsg          S             78    DIM(30) CTDATA PERRCD(1)             MSG DI ERRORE
005300070326     D*----------------------------------------------------*
005400940211     D* Passaggio Parametri
005500940211     D KPJBA         E DS
005600070227     D KPJBUs          s                   like(KPJBU)
005700940211     D*-------------
005800940211     D DSFMT           DS           512
005900940506     D  $TASTO               369    369
006000940211     D  NRG                  370    370
006100940211     D  NCL                  371    371
006200940211     D  SFLNRR               378    379B 0
006300940127     D*-------------
006400940127     D* Reperimento nome PGM
006500940127     D STATUS         SDS           333
006600940127     D  DSPGM            *PROC
006700030113     D*-------------
006800070320     d dstds           ds
006900070320     d  dst_nrr              397    400b 0
007000070320      *
007100070320     d argds           ds
007200070320     d  arg_nrr              397    400b 0
007300070320      *
007400070320     d arbds           ds
007500070320     d  arb_nrr              397    400b 0
007600070320     D*-------------
007700070316      * Variabili appoggio sempre presenti in tutte le anagrafiche
007800070309$003 D S3NRR           S                   Like(C3rcd)
007900070227$003 D S2NRR           S                   Like(C2rcd)
008000070228$003 D S2pag           S                   Like(C2rcd)
008100070309$003 D S3pag           S                   Like(C3rcd)
008200070416$003 D WSfl            S                   Like(C2nrr)
008300070416$003 D Wmax            S                   Like(C2rcd)
008400070416$003 D Wpag            S                   Like(C2rcd)
008500070330$003 D sav_WSfl        S                   Like(WSfl)
008600070330$003 D sav_Wmax        S                   Like(Wmax)
008700070330$003 D sav_Wpag        S                   Like(Wpag)
008800070416$003 D Wpagine         S                   Like(C2rcd)
008900070307$003 D NRR_s2          S                   Like(S2nrr)
009000070309$003 D NRR_s3          S                   Like(S3nrr)
009100070227      *--------------------------
009200070319     D Aggior_Dist     S              1    INZ('N')
009300070319     D Found_Dist      S              1    INZ('N')
009400070319     D Error_Dist      S              1    INZ('N')
009500070319     D Rec_allocato    S              1    INZ('N')
009600070320      *-----
009700070320     D sav_s2cgi       S                   Like(s2cgi)
009800070416     D sav_Sxcgi       S                   Like(s2cgi)
009900070330     D vedi_testo      S              1
010000070320      *--------------------------
010100070228     d wpes            s                   like(dstpkl)
010200070228     d wvol            s                   like(dstvlu)
010300070320      *--------------------------
010400070301     d wNFV            s                   like(dstNFV)
010500070301     d wNPG            s                   like(dstNPG)
010600070316     d wFGS            s                   like(dstFGS)
010700070321     d wCGI            s                   like(argCGI)
010800070321     d wDDC            s                   like(h1cddc)
010900070301     d kTIP            s                   like(ARItip)
011000070301     d kTRC            s                   like(AR6trc)
011100070307     d kCOD1           s                   like(TBEcod)
011200070307     d kKEY1           s                   like(TBEke1)
011300070404     d wfgs_ifp        s                   like(ARBifp)
011400070228      *--------------------------
011500070305      * Per impostare in testata la Distinta
011600070305     D Distintalfa     DS
011700070305     D  num_ndc                1      7
011800070305     D  ndc_a                  1      7    DIM(7)
011900070305      *--------------------------
012000070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
012100070305     D WLBDAT          DS                  INZ
012200070305     D  G02DAT                 1      8  0
012300070227     D  G02INV                 9     16  0
012400070227     D  G02ERR                17     17
012500070227     D  G02TGI                18     22  0
012600070307      *
012700070307     D DATPAR          DS
012800070307     D  GIODAT                 1      8  0
012900070307     D  GIOINV                 9     16  0
013000070307     D  GIOTGI                17     21  0
013100070307      *
013200070307     d Wdata8          DS
013300070307     d  dadata                 1      8  0
013400070307     d  adata                  9     16  0
013500070307     d  GioLav                17     21  0
013600070307      *--------------------------------
013700070307      * DS per richiamo pgm FNLR36R
013800070307     D DSLR36          DS
013900070307     D  P36AAS                14     17  0
014000070307     D  P36LNP                18     20  0
014100070307     D  P36NRS                21     22  0
014200070307     D  P36NSP                23     29  0
014300070307     D  P36F03                30     30
014400070307     D  P36FLG                31     31
014500070320      *--------------------------
014600070227     d AZUTEds       e ds                  extname(AZUTE00F)
014700070227     d DDatiUte      e ds
014800070320     d* Ds per dati organigramma
014900070228     D OG150         E DS                  INZ
015000070228     D OG146         E DS                  INZ
015100070320     D DgrgFLO       E DS                  INZ
015200070320      *------
015300070404     d $FV6            s              6P 0 dim(600)                             F.V.  x FIDG31R
015400070404      *------
015500070307     D DDGD          E DS
015600070301     D DS3A          E DS
015700070301     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
015800070301     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
015900070301     D DS1P          E DS
016000070301     D C1P             S              1    DIM(50)                              CODICI TABELLA 1P
016100070301     D D1P             S             25    DIM(50)                              DESCR. TABELLA 1P
016200070302     D YEURDS        E DS                  EXTNAME(YEURCODS)
016300070320      *--------------------------
016400070320     d* Parametri x Controllo profilo utenti
016500070320     d TIBS34ds      e ds
016600070227      *--------------------------
016700070227     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
016800070227     D DSLV50        E DS                  EXTNAME(FNLV50DS)
016900070417      *--------------------------
017000070417     D* DS PER - per pulire le distinte
017100070417     D fidg20ds      E DS
017200070227      *--------------------------
017300070302     D* DS PER - calcolo pesi volumi colli
017400070302     D fidg27ds      E DS
017500070302      *--------------------------
017600070319     D* DS PER - aggiornamento distinte
017700070319     D Fidg28ds      E DS
017800070320      *--------------------------
017900070320     D* DS PER FIDG04R - ricerca numero distinta
018000070320     D fidg31ds      E DS
018100070320     D*-------------
018200070328     D* DS PER TNSD99R - affidabilità consegna
018300070328     D tnsd99ds      E DS
018400070328     D*-------------
018500070320     d trul82ds      e ds
018600070319      *--------------------------
018700070228     D* DS PER - Bolle valide in consegna
018800070228     D Fidg16ds      E DS
018900070320      *--------------------------
019000070301     D fnarbdsav     e ds                  extname(fnarb00f)
019100070302     d                                     Prefix(sv_)
019200070320      *------
019300070228     D fnarbds       e ds                  extname(fnarb00f)
019400070228     D fiargds       e ds                  extname(fiarg00f)
019500070227      *--------------------------
019600070320      *?  COSTANTI                                      ?
019700070320      *--------------------------
019800940506     D* Tasti di funzione
019900940506     D F01             C                   CONST(X'31')
020000940506     D F02             C                   CONST(X'32')
020100940506     D F03             C                   CONST(X'33')
020200940506     D F04             C                   CONST(X'34')
020300940506     D F05             C                   CONST(X'35')
020400940506     D F06             C                   CONST(X'36')
020500940506     D F07             C                   CONST(X'37')
020600940506     D F08             C                   CONST(X'38')
020700940506     D F09             C                   CONST(X'39')
020800940506     D F10             C                   CONST(X'3A')
020900940506     D F11             C                   CONST(X'3B')
021000070315     D F12             C                   CONST(X'3C')
021100940506     D F13             C                   CONST(X'B1')
021200940506     D F14             C                   CONST(X'B2')
021300940506     D F15             C                   CONST(X'B3')
021400940506     D F16             C                   CONST(X'B4')
021500940506     D F17             C                   CONST(X'B5')
021600940506     D F18             C                   CONST(X'B6')
021700940506     D F19             C                   CONST(X'B7')
021800940506     D F20             C                   CONST(X'B8')
021900940506     D F21             C                   CONST(X'B9')
022000940506     D F22             C                   CONST(X'BA')
022100940506     D F23             C                   CONST(X'BB')
022200940506     D F24             C                   CONST(X'BC')
022300940506     D ENTER           C                   CONST(X'F1')
022400940506     D ROLDWN          C                   CONST(X'F4')
022500940506     D ROLLUP          C                   CONST(X'F5')
022600070301     d digits          c                   '0123456789'
022700070302     d Alla_Dist       c                   'alla dist.:'
022800070326     d OPZ_2           c                   'TOGLIE     '
022900070306     d Alla            c                   'alla '
023000070306     d Dalla           c                   'dalla'
023100070307     d FUN_1           c                   'F1=Selez.tutto'
023200070305     d FUN_3           c                   'F3=Fine'
023300070305     d FUN_12          c                   'F12=Ritorno'
023400070309     d FUN_9_Drop      c                   'F9=Altri Dati'
023500940127     C*----------------------------------------------------*
023600070320      *?  MAIN PROGRAM                                     ?
023700940127     C*----------------------------------------------------*
023800940223     C* inizializzazione variabili
023900940223     C                   EXSR      INZVAR
024000940223     C*
024100940223     C     $FINE         DOWEQ     *OFF
024200070413     C*
024300070413     C* 1° Video
024400070413     C     $GEST         CASEQ     'D1'          GESD1                          Video Parametri
024500070416     C* giro Batch:
024600070417      * ? Questa è la Routine che esegue la pulizia chiamando il FIDG20R
024700070417     C     $GEST         CASEQ     'PU'          GESPUL                         Esegue Pulizia
024800070416     C*
024900070417     C*
025000070417     C*
025100070417     C* giro a video come il DG30
025200070417      * ? Non vengono utilizzate in quanto vanno in dettaglio delle singole bolle
025300070417      * ?----------------------------------------------------------------------------
025400070413     C* Lista di dettaglio delle Bolle x selezione e guida aggiornamento Data Base
025500070413     C     $GEST         CASEQ     'S2'          GESS2                          Lista delle Bolle
025600070413     C* Finestra dei Totali riepilogativa prima dell'aggiornamento del Data Base
025700070413     C     $GEST         CASEQ     'W1'          GESW1                          Riepilogo Totali
025800070413     C* Durante l'aggiornamento del Data Base scrivo eventuali errori Bolle da visualizzare
025900070413     C*  su questo ultimo SFL.
026000070413     C     $GEST         CASEQ     'S3'          GESS3                          Aggiornamento D.B. e
026100070413      *                                                                         errori su bolle
026200070417      * ?----------------------------------------------------------------------------
026300070417     C*
026400070417     C*
026500940117     C                   END
026600070417     C*
026700070417     C                   ENDdo
026800940325     C* fine programma
026900940325     C                   SETON                                            LR
027000070417      *---------------------------------------------------------------*
027100070417      *  ?  OPERAZIONI INIZIALI      ?
027200070417      *---------------------------------------------------------------*
027300070417     C     *INZSR        BEGSR
027400070417     C*
027500070417     C* Reperimento parametri
027600070417     C     *ENTRY        PLIST
027700070417     C                   PARM                    KPJBA
027800070417      *
027900070417      *  Se richiamata da Azione/Menù
028000070417     c                   CLEAR                   se_EDP            1
028100070417     c                   if        %subst(Knmus:1:3) = 'EDP'
028200070417     c                   MOVE      'S'           se_EDP
028300070417     c                   end
028400070417      *
028500070417     c     karb70        klist
028600070417     c                   kfld                    wfgs_ifp
028700070417     c                   kfld                    h1ndc_meno
028800070417      *
028900070417     c     karb70gt      klist
029000070417     c                   kfld                    wfgs_ifp
029100070417     c                   kfld                    arbndc
029200070417     C*
029300070417     C     Kdst          KLIST
029400070417     C                   KFLD                    wnpg
029500070417     C                   KFLD                    Wnfv
029600070417     C                   KFLD                    wfgs
029700070417     C*
029800070417     C     Kdst_01       KLIST
029900070417     C                   KFLD                    wnpg
030000070417     C                   KFLD                    Numero6           6 0
030100070417     C                   KFLD                    wfgs
030200070417     C*
030300070417     C     Kdst8         KLIST
030400070417     C                   KFLD                    wfgs
030500070417     C                   KFLD                    wnpg
030600070417     C                   KFLD                    h1cDDC
030700070417      *
030800070417     C     KTAB1         KLIST
030900070417     C                   KFLD                    tblkut
031000070417     C                   KFLD                    TBLCOD
031100070417      *
031200070417     C     KTBE          KLIST
031300070417     C                   KFLD                    KCOD1
031400070417     C                   KFLD                    KKEY1
031500070417      *
031600070417     C     Karb1         KLIST
031700070417     C                   KFLD                    s2caas
031800070417     C                   KFLD                    s2clnp
031900070417     C                   KFLD                    s2cnrs
032000070417     C                   KFLD                    s2cnsp
032100070417      *
032200070417     C     KLBL          KLIST
032300070417     C                   KFLD                    lblAAP
032400070417     C                   KFLD                    lblLPP
032500070417     C                   KFLD                    lblNRP
032600070417     C                   KFLD                    lblNSP
032700070417     C*
032800070417     C     KAR6          KLIST
032900070417     C                   KFLD                    arbAAS
033000070417     C                   KFLD                    arbLNP
033100070417     C                   KFLD                    arbNRS
033200070417     C                   KFLD                    arbNSP
033300070417     C                   KFLD                    KTRC
033400070417      *
033500070417     C     KARI          KLIST
033600070417     C                   KFLD                    arbAAS
033700070417     C                   KFLD                    arbLNP
033800070417     C                   KFLD                    arbNRS
033900070417     C                   KFLD                    arbNSP
034000070417     C                   KFLD                    KTIP
034100070417     C*
034200070417     C     Kaggiorna     KLIST
034300070417     C                   KFLD                    s2cAAS
034400070417     C                   KFLD                    s2cLNP
034500070417     C                   KFLD                    s2cNRS
034600070417     C                   KFLD                    s2cNSP
034700070417     C*
034800070417     C     Kapd          KLIST
034900070417     C                   KFLD                    tipo_cod          1
035000070417     C                   KFLD                    cod_padr          7 0
035100070417     C*
035200070417     C                   move      'A'           tipo_cod
035300070417      *
035400070417      * Reperisco dati job
035500070417     c                   exsr      DatiJob
035600070417
035700070417     C* Variabili per gestione videate
035800070417     C                   MOVE      *BLANK        test_Parallelo    1
035900070417     C                   MOVE      *BLANK        $GEST             2
036000070417     C                   MOVE      *BLANK        sav$GEST          2
036100070417     C                   MOVE      *BLANK        $FINE             1
036200070417     C                   MOVE      *BLANK        $INZD1            1
036300070417     C                   MOVE      *BLANK        $INZW1            1
036400070417     C                   MOVE      *BLANK        $INZS2            1
036500070417     C                   MOVE      *BLANK        $INZS3            1
036600070417     C                   MOVE      *BLANK        $EFILE            1
036700070417     C                   MOVE      *BLANK        $ESCI             1
036800070417     C                   MOVE      *BLANK        $RCDOK            1
036900070417     C* Indici
037000070417     C                   Z-ADD     0             X                 3 0
037100070417     C                   Z-ADD     0             Y                 3 0
037200070417     C***
037300070417     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
037400070417     C***
037500070417     C                   TIME                    W0140            14 0
037600070417     C                   MOVE      W0140         UDATE8            8 0
037700070417     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
037800070417     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
037900070417     C*
038000070417     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
038100070417     C                   MOVEL     *BLANK        G02ERR
038200070417     C                   CALL      'XSRDA8'
038300070417     C                   PARM                    WLBDAT
038400070417     c                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
038500070417     C*
038600070417     C*  Aggancio tabella 'DGD' x reperire LIMITI CONTROLLO DISTINTA
038700070417     C                   MOVEL     'DGD'         KCOD1
038800070417     C                   MOVEL     *BLANKS       KKEY1
038900070417     C                   MOVEL     '1'           KKEY1
039000070417     C     KTBE          CHAIN     TNTBE01L
039100070417     C                   If        %Found(TNTBE01L)
039200070417     C                   MOVEL     TBEUNI        DDGD
039300070417     C                   END
039400070417     C*
039500070417     C*  CARICO DATI TABELLA 1P
039600070417     C                   MOVEL     '1P'          TBLCOD
039700070417     C                   Z-ADD     0             X
039800070417     C     KTAB1         setll     TABEL00F
039900070417     C     KTAB1         reade     TABEL00F
040000070417     C                   DOW       not %Eof(TABEL00F)  and (x < 50)
040100070417     C                   ADD       1             X
040200070417     C                   MOVEL     TBLUNI        DS1P
040300070417     C                   MOVEL     TBLKEY        C1P(X)
040400070417     C                   MOVEL     §1PDES        D1P(X)
040500070417     C     KTAB1         reade     TABEL00F
040600070417     C                   END
040700070417     C*
040800070417     C*  CARICO DATI TABELLA 3A
040900070417     C                   MOVEL     '3A'          TBLCOD
041000070417     C                   Z-ADD     0             X
041100070417     C     KTAB1         setll     TABEL00F
041200070417     C     KTAB1         reade     TABEL00F
041300070417     C                   DOW       not %Eof(TABEL00F) and (x < 100)
041400070417     C                   ADD       1             X
041500070417     C                   MOVEL     TBLKEY        C3A(X)
041600070417     C                   MOVEL     TBLUNI        D3A(X)
041700070417     C     KTAB1         reade     TABEL00F
041800070417     C                   END
041900070417     C*
042000070417     C                   ENDSR
042100070417      *---------------------------------------------------------------*
042200070417      *?  Reperimento Dati del job (Utente/Operativi)                ?
042300070417      *---------------------------------------------------------------*
042400070417     c     DatiJOB       BEGSR
042500070417      *
042600070417     c     *dtaara       define    §azute        azuteds
042700070417     c     *dtaara       define    §datiute      ddatiute
042800070417      *
042900070417     c                   in(E)     *dtaara
043000070417     c                   IF        %ERROR or RSUT = *blanks
043100070417     c                   clear                   Tibs34Ds
043200070417     c                   call      'TIBS34R'
043300070417     c                   parm                    Tibs34Ds
043400070417     c                   in        *dtaara
043500070417     c                   ENDIF
043600070417      *
043700070417     c                   ENDSR
043800030113     C************************************************************
043900070320      *?  INIZIALIZZAZIONE VARIABILI                       ?
044000030113     C************************************************************
044100030113     C     INZVAR        BEGSR
044200030113     C*
044300030113     C* Variabili per gestione videate
044400030113     C                   MOVE      *OFF          $FINE
044500070227     C                   MOVE      *OFF          $INZW1
044600070227     C                   MOVE      *OFF          $INZS2
044700070309     C                   MOVE      *OFF          $INZS3
044800030113     C                   MOVE      *OFF          $EFILE
044900030113     C                   MOVE      *OFF          $ESCI
045000030113     C                   MOVE      *OFF          $RCDOK
045100070227     C* Emissione primo video
045200070227     C                   MOVE      'D1'          $GEST
045300070227     C                   MOVE      *ON           $INZD1
045400030113     C* Variabili appoggio
045500030114     C                   Z-ADD     1             WPAG
045600030113     c*
045700030113     C                   ENDSR
045800070227     C************************************************************
045900070320      *?  Emissione primo video di SCELTA                        ?
046000070227     C************************************************************
046100070227     C     GESD1         BEGSR
046200070227     C*
046300070227     C* inizializzazione videata
046400070227     C     $INZD1        IFEQ      *ON
046500070227     C                   EXSR      INZD1
046600070227     C                   MOVE      *OFF          $INZD1
046700070227     C                   ENDIF
046800070227     C*
046900070228     c     Video_1       tag
047000070302     C*  Emissione Video
047100070228      * ?            *-----------------------*
047200070227     C                   EXFMT     FDG0D01
047300070228      * ?            *-----------------------*
047400070326     c                   setoff                                       99
047500070305     C                   movel     'D1'          sav$GEST
047600070417      * Selezioni
0477000702271    C                   SELECT
047800070413     C*  Cambio filiale
047900070413     C                   WHEN      $TASTO   =   F18
048000070413     C                   SETON                                        07
048100070227     C* F3=Fine
048200070227     C                   WHEN      $TASTO   =   F03
048300070227     C                   EXSR      F03_exit
048400070319     c                   rolbk
048500070417     c                   goto      end_gesd1
048600070416      *
0487000704161-   C                   ENDSL
048800070227     C* CONTROLLO DATI
048900070227     C                   EXSR      CTRD1
049000070227     C*
049100070228     C* Per errori o decodifiche
049200070228     c   99              goto      Video_1
049300070417      *
049400070417     C* F6=Esegue Batch
049500070417      * ? Questa è la Routine che esegue la pulizia chiamando il FIDG20R
049600070417     C                   if        $TASTO   =   F06
049700070417     c                   eval      $GEST ='PU'
049800070417     c                   end
049900070417      *
050000070417     C* F9=Esegue mirata sulle bolle
050100070417      * ? Al momento disattivata
050200070417     C                   if        $TASTO   =   F09
050300070417     C                   EXSR      Toglie
050400070417     c                   end
050500070228      *
050600070417     C     end_gesd1     ENDSR
050700070306     C/EJECT
050800070227     C************************************************************
050900070416      *?  Esegue in Batch  un ciclo sulle distinte               ?
051000070227     C************************************************************
051100070417     C     GESPUL        BEGSR
051200070227     C*
051300070416      * ? ----------------------------
051400070416     c* se il p.o. è in parallelo testo la distinta su ARG altrimenti
051500070416     c* se il p.o. è partito testo la distinta su ARB e ARG
051600070416     c                   if        test_Parallelo = 'S'
051700070416     C/EXEC SQL
051800070417     C+ DECLARE A1 CURSOR FOR SELECT argndc, argddc FROM fiarg02l JOIN
051900070417     C+ fnarb01l ON arbaas= argaas and arblnp=arglnp and arbnrs= argnrs
052000070417     C+ and arbnsp= argnsp WHERE argpoc = :d1cFGS and argNDC between
052100070417     C+ :h1ndc_meno and :h1ndc_men2 GROUP BY argndc, argddc ORDER BY
052200070417     C+ argndc, argddc
052300070416     C/END-EXEC
052400070416
052500070416     C/EXEC SQL
052600070416     C+ OPEN a1
052700070416     C/END-EXEC
052800070416     c                   else
052900070416     c                   z-add     wfgs          wfgs_ifp
053000070416      * ? ----------------------------
053100070416     c     karb70        setll     fnarb70l
053200070416      * ? ----------------------------
053300070416     c                   end
053400070416      * ? ----------------------------
053500070416     C*
053600070416     C*  Per EoF deve uscire
053700070416     C                   MOVEL     *OFF          $EFILE
053800070416     C*  Per Record Ok deve uscire riportando i dati
053900070416     C                   MOVEL     *OFF          $RCDOK
054000070416     C*
0541000704161    C     $EFILE        DOUEQ     *ON
054200070416     C     $RCDOK        OREQ      *ON
054300070416     C*
054400070416      *  x Togliere
054500070416     c                   if        test_Parallelo = 'S'
054600070416      * ?LETTURE  FILE
054700070416     C/EXEC SQL
054800070417     C+ FETCH NEXT FROM a1 INTO  :argndc, :argddc
054900070416     C/END-EXEC
055000070417     c                   z-add     argndc        Numero            7 0
055100070417     c                   z-add     argddc        Data              8 0
055200070416     c                   else
055300070416      **
055400070416     c     wfgs_ifp      reade     fnarb70l
055500070416     C                   eval      sqlcod = 0
055600070416      *
055700070416      * Fine File
055800070416     c                   if        %Eof(fnarb70l)
055900070416     C                   eval      sqlcod = 100
056000070416     c                   else
056100070416      **
056200070416      * NO Fine File
056300070416     c                   if        arbndc > h1ndc_men2
056400070416     C                   eval      sqlcod = 100
056500070416     c                   else
056600070417      **
056700070417     c                   z-add     arbndc        Numero            7 0
056800070417     c                   z-add     arbddc        Data              8 0
056900070416     c     karb70gt      setgt     fnarb70l
057000070416     c                   end
057100070416      **
057200070416     c                   end
057300070416      **
057400070416     c                   end
057500070416      *-------------
057600070416     C                   select
057700070416      * Fine File
057800070416     C                   when      sqlcod = 100
057900070416     C                   MOVEL     *ON           $EFILE
058000070416     C                   MOVE      $EFILE        *IN33
058100070416      * errori
058200070416     C                   WHEN      SqlCod < 0
058300070416     c                   if        sqlcod <> -913
058400070416     C                   seton                                        H1
058500070416     c                   end
058600070416      *
058700070416     C                   other
058800070417      *
058900070417      * Aggancia la distinta
059000070417     c                   z-add     numero        numero6
059100070417     C     Kdst_01       chain(n)  fiDST01l
059200070416      * Esegue la pulizia
059300070417     c* se devo pulire i riferimenti della distinta chiamo il pgm FIDG20R
059400070417     c                   if        %Found(fiDST01l)
059500070417     c                   clear                   fidg20ds
059600070417     c                   eval      d20cmti  = 'S'
059700070417     c                   eval      d20clrman= 'S'
059800070417     c                   eval      d20fgsi  = Wfgs
059900070417     c                   eval      d20nfvi  = dstnfv
060000070417     c                   eval      d20dfvi  = dstdfv
060100070417     c                   eval      d20fppi  = dstfpp
060200070417     c                   eval      d20tpmi  = dsttpm
060300070417     c                   eval      kpjbus   = kpjbu
060400070417     c                   eval      kpjbu    = fidg20ds
060500070417     c                   call      'FIDG20R'
060600070417     c                   parm                    kpjba
060700070417     c                   eval      fidg20ds = kpjbu
060800070417     c                   eval      kpjbu    = kpjbus
060900070417     c                   if        d20erro<> ' '
061000070417     c                   end
061100070417     c                   end
061200070416      *
061300070416     C                   endsl
061400070416     C*
0615000704161-   C                   ENDDO
061600070417     C*
061700070417     c                   if        test_Parallelo = 'S'
061800070416     C/EXEC SQL
061900070416     C+ CLOSE a1
062000070416     C/END-EXEC
062100070417     c                   end
062200070417     C*
062300070417     C* Emissione primo video
062400070417     C                   MOVE      'D1'          $GEST
062500070417     C                   MOVE      *ON           $INZD1
062600070417     C*
062700070416     C                   ENDSR
062800070416     C/EJECT
062900070416     C************************************************************
063000070416      *?  INIZIALIZZAZIONE Primo video                           ?
063100070416     C************************************************************
063200070416     C     INZD1         BEGSR
063300070416     C*
063400070227     C* Pulizia campi e indicatori
063500070308     c                   exsr      INZ_indERR
063600070227     C* Pulizia formato record
063700070227     c                   clear                   FDG0D01
063800070227     C* campi titolo
063900070302     C                   MOVEL     dsPGM         D1CPGM
064000070227     C                   MOVEL     knsif         D1nsif
064100070227     C                   MOVEL     knmus         D1nmus
064200070227     C                   MOVEL     RSUT          D1crsu
064300070227     C* campi video
064400070227     C* Il p.o.deve essere impostato in base al profilo
064500070413     C     dutlpo        IFEQ      '2'
064600070227     C                   MOVEL     dutpou        D1CFGS                         *FIL.GESTIONE
064700070413     C                   ELSE
064800070413     c                   seton                                        08
064900070413     C                   MOVEL     SIMFEL        D1CFGS
065000070413     C                   END
065100070227     C                   z-add     UDATE8        D1cDDC
065200070316     C* x i test
065300070316     c                   if        se_EDP = 'S'
065400070316     C                   z-add     09022007      D1cDDC
065500070316     c                   end
065600070417     C*
065700070417     c                   if        %subst(Knmus:1:5) = 'EDPAB'
065800070417     c                   seton                                        09
065900070417     c                   end
066000070227     C*
066100070227     C                   ENDSR
066200940127     C************************************************************
066300070320      *? Controlli primo video di SCELTA Operazione da eseguire  ?
066400940127     C************************************************************
066500070227     C     CTRD1         BEGSR
066600030113     C*
066700070227     C*  Spegne indicatori di errore
066800070308     c                   exsr      INZ_indERR
066900070227     c                   setoff                                       99
067000070228     C*  campi di Work
067100070308     c                   clear                   h1cddc
067200070319     c                   clear                   h1ndc_meno
067300070319     c                   clear                   h1fpp_meno
067400070416     c                   clear                   h1ndc_men2
067500070416     c                   clear                   h1fpp_men2
067600070321     c                   clear                   wfgs
067700070321     c                   clear                   wddc
067800070321     c                   clear                   wcgi
067900070301      * Categoria Foglio x controllo nr.distinta
068000070301     C                   z-add     4             wNPG
068100070227     C* --------------
068200070227      * ?- Controlla la filiale in gestione
068300070227     C                   CLEAR                   DSLV50
068400070227     C                   MOVEL     KNMUS         D50PRU
068500070227     C                   z-add     d1cFGS        D50FGS
068600070227     C                   CALL      'FNLV50R'
068700070227     C                   PARM                    DSLV50
068800070227    1C     D50ERR        IFNE      ' '
068900070326     c                   movel     ErrMsg(1)     d1msg
069000070326     C                   SETON                                        51  99
069100070227     C                   goto      ENDCTR1
069200070227    1C                   ENDIF
069300070227      *
069400070227     C*  Controlla Con il P.O. la data abilitazione
069500070227     C     d1cFGS        CHAIN     AZORG01l
069600070227    1C                   IF        %found(azorg01l)
069700070227     C                   MOVEL     ORGdf0        og150
069800070227     C                   MOVEL     ORGde6        og146
069900070227     c                   if             §ogdda = *blank or §ogdda = *all'0'
070000070227     C                   MOVE      *ON           $FINE
070100070227     C                   goto      ENDCTR1
070200070227     c                   else
070300070227      * se c'è una data in organigramma
070400070227     c                   move      §ogdda        data_org          8 0
070500070227      * se siamo in fase di Parallelo prima di essere partiti
070600070227     c                   if        data_org = 20391231
070700070227     C                   eval      test_Parallelo = 'S'
070800070227     c                   end
070900070227      * se data distinte automatiche NON è
071000070227      * né = 31/12/2039 che significa p.o. in fase di test o parallelo
071100070227      * né < oggi che significa p.o. partito
071200070227      * allora fuori
071300070227     c                   if        data_org <> 20391231 and dateu8 < data_org
071400070227     C                   MOVE      *ON           $FINE
071500070227     C                   goto      ENDCTR1
071600070227     c                   end
071700070227     c                   end
071800070227      *-
071900070227   X1C                   ELSE
072000070227      * Non presente in organigramma
072100070326     C                   SETON                                        51  99
072200070326     c                   movel     ErrMsg(1)     d1msg
072300070227     C                   goto      ENDCTR1
072400070227    1C                   ENDIF
072500070227     C*
072600070316      * Salva la filiale su campo di work x tutto il pgm
072700070316     c                   z-add     d1cfgs        wfgs
072800070227     C* --------------
072900070227      * ?- Data Distinte del:
073000070227     C                   Z-ADD     d1cDDC        G02DAT
073100070227     C                   MOVEL     *BLANK        G02ERR
073200070227     C                   CALL      'XSRDA8'
073300070227     C                   PARM                    WLBDAT
073400070227    1C     G02ERR        IFEQ      '1'
073500070326     c                   movel     ErrMsg(2)     d1msg
073600070326     C                   SETON                                        52  99
073700070227     C                   goto      ENDCTR1
073800070227    1C                   ENDIF
073900070307      *
074000070307      *  Per testare ??????????????????????????????????
074100070308     c                   if        se_EDP    = 'S'
074200070307     c                   goto      salta_ctrl
074300070307     c                   end
074400070307      *
074500070307      * Controlli identici al FNLR50R sulla data distinta
074600070307      * prima della data del giorno
074700070307     c                   if        g02inv < dateu8
074800070326     c                   movel     ErrMsg(3)     d1msg
074900070326     c                   SETON                                        52  99
075000070307     C                   goto      ENDCTR1
075100070307     c                   endif
075200070307      *controllo range fra le date di oggi e quella immessa
075300070413      *  non può essere superiore di 3 giorni lavorativi
075400070307     c                   eval      dadata = dateu8
075500070307     c                   eval      adata  = g02inv
075600070307     c                   CALL      'XSRLAV8'
075700070307     c                   PARM                    Wdata8
075800070413     c                   if        giolav > §dgddc or
075900070413     c                             giolav > 3
076000070326     c                   movel     ErrMsg(4)     d1msg
076100070326     c                   SETON                                        52  99
076200070307     C                   goto      ENDCTR1
076300070307     c                   end
076400070307      *
076500070307     C* VERIFICO CHE LA DATA DELLA DISTINTA NON SIA ANTECEDENTE O
076600070307     C* SEGUENTE ALLA UDATE DI OLTRE 15 GIORNI
076700070307     C* PER TUTTO IL MESE DI AGOSTO NON CONTROLLO 1 15 GG
076800070307     C                   MOVE      dateu8        WMGUDT            4 0
076900070307     C                   MOVEL     WMGUDT        WMUDT             2 0
077000070307     C                   Z-ADD     §DGDGG        NRGIO             3 0
077100070307      *
077200070307     C     WMGUDT        IFGE      §DGDEI
077300070307     C     WMGUDT        ANDLE     §DGDEF
077400070307     C                   Z-ADD     §DGDGE        NRGIO
077500070307     C                   END
077600070307      *
077700070307     C     dateu8        IFGE      §DGDII
077800070307     C     dateu8        ANDLE     §DGDIF
077900070307     C                   Z-ADD     §DGDGI        NRGIO
078000070307     C                   END
078100070307      *
078200070307     C     G02TGI        SUB       NRGIO         GIOTGI
078300070307     C                   CALL      'XSRGI8'
078400070307     C                   PARM                    DATPAR
078500070307     C                   Z-ADD     GIOINV        DATM15            8 0
078600070307      *
078700070307     C     DATM15        IFGT      dateu8
078800070326     c                   movel     ErrMsg(5)     d1msg
078900070326     C                   SETON                                        52  99
079000070307     C                   goto      ENDCTR1
079100070307     C                   END
079200070307      *
079300070307     C     G02TGI        ADD       NRGIO         GIOTGI
079400070307     C                   CALL      'XSRGI8'
079500070307     C                   PARM                    DATPAR
079600070307     C                   Z-ADD     GIOINV        DATP15            8 0
079700070307      *
079800070307     C     DATP15        IFLT      dateu8
079900070326     c                   movel     ErrMsg(5)     d1msg
080000070326     C                   SETON                                        52  99
080100070307     C                   goto      ENDCTR1
080200070307     C                   END
080300070307      *
080400070307     c     salta_ctrl    tag
080500070307      *
080600070227     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
080700070301     C                   Z-ADD     G02INV        h1cDDC
080800070321     c                   z-add     G02INV        wddc
080900070301     C                   Z-ADD     G02DAT        D1cDDC
081000070301     C*---------------
081100070307      *
081200070307      *  Per testare ??????????????????????????????????
081300070308     c                   if        se_EDP    = 'S'
081400070307     c                   goto      salta_ctrl1
081500070307     c                   end
081600070307      *
081700070307     C*   controlla se nella data scelta ci sono distinte aperte
081800070307     c                   move      'S'           tutte_chiuse      1
081900070406     c     kdst8         setll     fidst08l
082000070406     c     kdst8         reade     fidst08l
082100070406     c                   dow       not %Eof(fidst08l)
082200070412     c                   if        dstFCF <> 'S' and dstATB = ' '
082300070307     c                   clear                   tutte_chiuse
082400070307     c                   leave
082500070307     c                   end
082600070406     c     kdst8         reade     fidst08l
082700070307     c                   enddo
082800070307      * nella data tutte non ci sono distinte aperte
082900070307     c                   if        tutte_chiuse = 'S'
083000070326     c                   movel     ErrMsg(6)     d1msg
083100070326     C                   SETON                                        52  99
083200070307     C                   goto      ENDCTR1
083300070307     c                   end
083400070307      *
083500070307     c     salta_ctrl1   tag
083600070301     C*------------------------
083700070416      ** Se richiesto tutto
083800070416     c                   if        (d1ndc2 = *zeros or d1ndc2 = *blanks) and
083900070416     c                                d1ndc3 = *all'9'
084000070416      **
084100070416     c                   if        $TASTO   =   F09
084200070416     c                   movel     ErrMsg(7)     d1msg
084300070416     C                   SETON                                        565899
084400070416     C                   goto      ENDCTR1
084500070416     c                   end
084600070416      **
084700070416     c                   move      d1ndc2        h1ndc_meno
084800070416     c                   move      d1ndc3        h1ndc_men2
084900070416      **
085000070416     c                   else
085100070416      **
085200070416      ** Se non richiesto tutto
085300070416     c                   if        d1ndc2 = *zeros or d1ndc2 = *blanks
085400070313     c                   clear                   d1ndc2
085500070313     c                   end
085600070316     c     '?'           scan      d1ndc2
085700070228     C*   ricerca distinta appartenente al giro oppure tutte
085800070316     c                   if        %Found   or
085900070316     c                             d1ndc2 = *zeros or d1ndc2 = *blanks
086000070326     C******             SETON                                            99
086100070321     c                   exsr      Ricerca_Dist
086200070228     C                   MOVE      *zeros        d1ndc2
086300070313     C                   MOVE      FDGNFV        d1ndc2
086400070326     C******             goto      ENDCTR1
086500070228     c                   end
086600070228      *  controllo validità distinta
086700070301     c     digits        check     d1ndc2
086800070301     c                   if        %Found
086900070326     c                   movel     ErrMsg(9)     d1msg
087000070301     C                   SETON                                        56  99
087100070301     C                   goto      ENDCTR1
087200070301     c                   end
087300070301      *
087400070301     c                   move      d1ndc2        wNFV
087500070319     c                   eval      Aggior_Dist = 'N'
087600070319      *                                     ?  =======  ?
087700070316     c                   exsr      CHAIN_Dist
087800070319     c                   if        Error_Dist = 'S'
087900070319     c                   if        Rec_Allocato = 'S'
088000070326     c                   movel     ErrMsg(11)    d1msg
088100070319     c                   end
088200070326     C                   SETON                                            56  99
088300070319     C                   goto      ENDCTR1
088400070319     c                   end
088500070319      *
088600070316     c                   if        Found_Dist = 'N'
088700070326     c                   movel     ErrMsg(9)     d1msg
088800070301     C                   SETON                                        56  99
088900070301     C                   goto      ENDCTR1
089000070301     c                   else
089100070417      * Distinta non nella data
089200070417     c                   if        dstDFV <> h1cDDC
089300070417     c                   movel     ErrMsg(8)     d1msg
089400070417     C                   SETON                                        56  99
089500070417     C                   goto      ENDCTR1
089600070417     c                   end
089700070301      * Foglio chiuso o Annullato
089800070301    3C     dstFCF        ifEQ      'S'
089900070301    4C     dstATB        orNE      ' '
090000070326     c                   movel     ErrMsg(10)    d1msg
090100070326     C                   SETON                                        56  99
090200070301     C                   goto      ENDCTR1
090300070301     c                   endIF
090400070301     c                   end
090500070301      * se tutto OK
090600070319     c                   z-add     dstPDR        h1pdr_meno
090700070319     c                   move      dstFPP        h1fpp_meno
090800070319     c                   move      d1ndc2        h1ndc_meno
090900070416      *
091000070301     C*---------------
091100070416     c                   if        d1ndc3 = *zeros or d1ndc3 = *blanks
091200070416     c                   clear                   d1ndc3
091300070416     c                   end
091400070416     c     '?'           scan      d1ndc3
091500070228     C*   ricerca distinta appartenente al giro oppure tutte
091600070326     c                   if        %Found
091700070416     c                             or d1ndc3 = *zeros or d1ndc3 = *blanks
091800070326     C**********         SETON                                            99
091900070321     c                   exsr      Ricerca_Dist
092000070416     C                   MOVE      *zeros        d1ndc3
092100070416     C                   MOVE      FDGNFV        d1ndc3
092200070326     C********           goto      ENDCTR1
092300070228     c                   end
092400070301      *  controllo validità distinta
092500070416     c     digits        check     d1ndc3
092600070301     c                   if        %Found
092700070326     c                   movel     ErrMsg(9)     d1msg
092800070301     C                   SETON                                        58  99
092900070301     C                   goto      ENDCTR1
093000070301     c                   end
093100070227     C*
093200070416     c                   move      d1ndc3        wNFV
093300070319     c                   eval      Aggior_Dist = 'N'
093400070319      *                                     ?  =======  ?
093500070316     c                   exsr      CHAIN_Dist
093600070319     c                   if        Error_Dist = 'S'
093700070319     c                   if        Rec_Allocato = 'S'
093800070326     c                   movel     ErrMsg(11)    d1msg
093900070319     c                   end
094000070326     C                   SETON                                            58  99
094100070319     C                   goto      ENDCTR1
094200070319     c                   end
094300070319      *
094400070316     c                   if        Found_Dist = 'N'
094500070326     c                   movel     ErrMsg(9)     d1msg
094600070301     C                   SETON                                        58  99
094700070301     C                   goto      ENDCTR1
094800070301     c                   else
094900070417      * Distinta non nella data
095000070417     c                   if        dstDFV <> h1cDDC
095100070417     c                   movel     ErrMsg(8)     d1msg
095200070417     C                   SETON                                        58  99
095300070417     C                   goto      ENDCTR1
095400070417     c                   end
095500070301      * Foglio chiuso o Annullato
095600070301    3C     dstFCF        ifEQ      'S'
095700070301    4C     dstATB        orNE      ' '
095800070326     c                   movel     ErrMsg(10)    d1msg
095900070326     C                   SETON                                        58  99
096000070301     C                   goto      ENDCTR1
096100070301     c                   endIF
096200070301     c                   end
096300070314      *---------------
096400070301      * se tutto OK
096500070416     c                   z-add     dstPDR        h1pdr_men2
096600070416     c                   move      dstFPP        h1fpp_men2
096700070416     c                   move      d1ndc3        h1ndc_men2
096800070301     C*
096900070416     c                   if            d1ndc3 < D1NDC2
097000070416     c                   movel     ErrMsg(7)     d1msg
097100070416     C                   SETON                                        565899
097200070416     C                   goto      ENDCTR1
097300070416     c                   end
097400070416      *
097500070416     c                   end
097600070416      *
097700070227     C     ENDCTR1       ENDSR
097800070301      *---------------------------------------------------------------*
097900070319      *?  Aggancia la Distinta x allocarla ed in seguito x aggiornarla ?
098000070301      *---------------------------------------------------------------*
098100070321     C     Ricerca_Dist  BEGSR
098200070228     C*
098300070321     C                   clear                   FIDG31DS
098400070321     C                   Z-ADD     0             FDGNFV
098500070321     C                   MOVEL     wFGS          FDGFGS
098600070326      *
098700070326     C                   if        $TASTO   <>  F05
098800070321     C                   MOVEL     wCGI          FDGCGI
098900070326     c                   end
099000070326      *
099100070321     C                   Z-ADD     wDDC          FDGGDA
099200070321     C                   Z-ADD     wDDC          FDGGAL
099300070321     C                   move      wNPG          FDGNPG
099400070321     C                   move      'S'           FDGRIC
099500070321     C                   move      '2'           FDGTFV
099600070321     C                   move      'P'           FDGTPV
099700070321     c                   eval      kpjbus = kpjbu
099800070404     c                   clear                   $FV6
099900070404      * impostato a tutti 99999
100000070404     c                   do        600           yy                3 0
100100070404     c                   z-add     999999        $FV6(yy)
100200070404     c                   enddo
100300070321     C                   MOVEL(p)  FIDG31DS      KPJBU
100400070321     C                   CALL      'FIDG31R'
100500070321     C                   PARM                    KPJBA
100600070404     C                   PARM                    $FV6
100700070321     C                   MOVEL     KPJBU         FIDG31DS
100800070321     c                   eval      kpjbu = kpjbus
100900070321     C*
101000070321     C                   ENDSR
101100070321      *---------------------------------------------------------------*
101200070321      *?  Aggancia la Distinta x allocarla ed in seguito x aggiornarla ?
101300070321      *---------------------------------------------------------------*
101400070321     C     Chain_Dist    BEGSR
101500070321     C*
101600070316      * inizializza il trovato/non trovato
101700070316     c                   eval      Found_Dist = 'N'
101800070319     c                   eval      Error_Dist = 'N'
101900070319     c                   eval      rec_allocato = 'N'
102000070316     C*
102100070316     C     Kdst          CHAIN(e)  fidst01l
102200070316     c*
102300070316     c                   if        %Error
102400070319     c                   eval      Error_Dist = 'S'
102500070316      * se il record è allocato mando messaggio info all'utente che alloca il rcd
102600070316     c*
102700070316     c                   clear                   trul82ds
102800070316     c                   eval      ul82§rrn = dst_nrr
102900070316     c                   eval      ul82§fil = 'FIDST00F'
103000070316     c                   eval      ul82§win = 'S'
103100070316     c                   eval      ul82§f7  = 'S'
103200070319     c                   eval      ul82§num = 2
103300070319     c                   eval      ul82§att = 2
103400070326     c                   eval      ul82§mss = ErrMsg(27)
103500070316     c                   Eval      UL82§msw = 'La Distinta '
103600070316     c                             + %editc(Wnfv:'Z') +  ' è già utilizzata da -
103700070319     c                              un altro lavoro e non è manutenzionabile'
103800070316      *
103900070316      * chiamo il pgm che manda il messaggio info all'utente
104000070316     c                   call(e)   'TRUL82R'
104100070316     c                   parm                    trul82ds
104200070316      *
104300070316     c                   if        ul82§sts = 'A'
104400070319     c                   move      'S'           rec_allocato
104500070316     c                   endif
104600070316     C*
104700070316     c                   end
104800070316      * Trovato record
104900070316     c                   if        %Found(fidst01l)
105000070316     c                   eval      Found_Dist = 'S'
105100070319     C*
105200070319      *   Se richiamato x aggiornamento finale dopo ARG e ARB  ?
105300070319      *   quindi non solo per semplice allocazione del record. ?
105400070319     c                   if        Aggior_Dist = 'S'
105500070319      *                                     ?  =======  ?
105600070319     c                   exsr      Campi_Distinta
105700070319     c                   endIF
105800070319     C*
105900070316     c                   update    fidst000
106000070316     c                   end
106100070316     C*
106200070316     C     ENDChain      ENDSR
106300070316      *---------------------------------------------------------------*
106400070319      *  ?  Imposta i campi della distinta da Aggiornare             ?
106500070316      *---------------------------------------------------------------*
106600070319     C     Campi_DistintaBEGSR
106700070316     C*
106800070319     c                   clear                   fidg28ds
106900070319     c                   eval      D28FGSI = Wfgs
107000070319     c                   eval      D28DFVI = h1cddc
107100070321     c                   eval      D28NFVI = wnfv
107200070319     c                   call      'FIDG28R'
107300070319     c                   parm                    fidg28ds
107400070319     c                   if        d28erro = ' '
107500070319     c                   eval      DSTPKLa = D28PKLao
107600070319     c                   eval      DSTVLUa = D28VLUao
107700070319     c                   eval      DSTstpa = D28stpao
107800070319     c                   eval      DSTNBLa = D28NBLao
107900070319     c                   else
108000070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
108100070319     c                   eval      Error_Dist  = 'S'
108200070319     c                   end
108300070319     C*
108400070319     C                   ENDSR
108500070416     C/EJECT
108600070417     C************************************************************
108700070417     C*? Toglie a Distinta
108800070417     C************************************************************
108900070417     C     Toglie        BEGSR
109000070417     C*
109100070417     C*  Emissione SFL con elenco delle bolle da togliere alla distinta
109200070417     c                   eval      $GEST ='S2'
109300070417     C                   MOVE      *ON           $INZS2
109400070417     C                   MOVE      *ON           $INZW1
109500070417     C                   MOVE      *ON           $INZS3
109600070417     C*
109700070417     C                   ENDSR
109800070417     C/EJECT
109900070301      *---------------------------------------------------------------*
110000070316      *  ?  GESTIONE SFL Bolle da selezionare                        ?
110100070301      *---------------------------------------------------------------*
110200070301     C     GESS2         BEGSR
110300070301     C*
110400070301     C* inizializzazione videata
110500070301     C     $INZS2        IFEQ      *ON
110600070301     C                   EXSR      INZS2
110700070301     C                   MOVE      *OFF          $INZS2
110800070301     C                   ENDIF
110900070306     C*
111000070306     C* Inizializza e imposta piede videata
111100070306     C                   EXSR      INZZ2
111200070305     C*
111300070301     C* emissione piede videata
111400070302      * ?            *-----------------------*
111500070301     C                   WRITE     FDG0Z02
111600070302      * ?            *-----------------------*
111700070301     C*
111800070301     C     WMAX          IFEQ      0
111900070301     C* Non ci sono records
112000070302      * ?            *-----------------------*
112100070301     C                   WRITE     FDG0V02
112200070302      * ?            *-----------------------*
112300070301     c                   setoff                                       30
112400070301     C                   Else
112500070301     c                   seton                                        30
112600070301      *
112700070301     C     Wsfl          IFgt      0
112800070301     C                   Z-ADD     wsfl          C2RCD
112900070301     C                   Else
113000070301     C     Wpag          IFgt      0
113100070301     C                   Z-ADD     wpag          C2RCD
113200070301     C                   EndIF
113300070301     C                   EndIF
113400070301     C                   ENDIF
113500070301      *
113600070301      *  Emissione SFL delle BOLLE
113700070301      * ?            *-----------------------*
113800070301     C                   EXFMT     FDG0C02
113900070301      * ?            *-----------------------*
114000070326     c                   setoff                                       99
114100070305     C                   movel     'S2'          sav$GEST
114200070301      *
114300070301     C     C2NRR         IFNE      0
114400070301     C                   Z-ADD     C2NRR         WSFL
114500070301     C                   ENDIF
114600070301     C                   Z-ADD     SFLNRR        C2RCD
114700070306      ***
114800070301     C* Selezioni
1149000703011    C                   SELECT
115000070301     C* F3=Fine
115100070301     C     $TASTO        WHENEQ    F03
115200070301     C                   EXSR      F03_exit
115300070319     c                   rolbk
115400070307      *
115500070307     C* F1=Seleziona Tutti
115600070307     C                   WHEN      $TASTO   =   F01
115700070307     C                   EXSR      F01_Tutti2
115800070301     C* F12=Ritorno
115900070301     C     $TASTO        WHENEQ    F12
116000070308     c                   exsr      INZ_indERR
116100070305     c                   move      'D1'          $GEST
116200070315     C                   MOVE      *ON           $INZD1
116300070316     c                   rolbk
116400070301      *
1165000703011O   C                   OTHER
116600070301     C* CONTROLLO testata
116700070301     C                   EXSR      CTRC2
116800070301     C* CONTROLLO subfile
116900070301     C     *IN99         IFEQ      *OFF
117000070301     C                   EXSR      CTRS2
117100070301     C                   END
117200070301      *
117300070305      *
117400070301      * ?se NON premuto F9 e immessa una distinta,
117500070301      * ? emette la finestra dei totali Distinta
117600070328      * ?  se non ci sono stati errori precdentemente
117700070328      * ?   e se è stato ripulito il messaggi degli errori precedenti
117800070328      * ?  se il sfl è carico.
117900070328     C     *IN99         IFEQ      *OFF
118000070328     c     ErrMsgZ       andEQ     *blank
118100070328     C     WMAX          andGT     0
118200070326      *
118300070416     c     totSPE_s2     IFgt      0
118400070305     C                   MOVE      *ON           $INZW1
118500070302     C                   movel     'W1'          $GEST
118600070302     c                   else
118700070302      * emette messaggio di Avviso x scegliere una Distinta se non c'è
118800070302      * se si era scelta l'opzione di assegnazione e non si era immessa
118900070302      * una distinta
119000070326     c                   movel     ErrMsg(19)    err001
119100070326     c                   movel     ErrMsg(20)    err002
119200070319     c                   clear                   err003
119300070302      * ?            *-----------------------*
119400070305     C                   exfmt     FDG0WER
119500070302      * ?            *-----------------------*
119600070302     c                   end
119700070305      *
119800070305     c                   end
119900070301      *
1200000703011-   C                   ENDSL
120100070301     C*
120200070301     C                   ENDSR
120300070306     C/EJECT
120400070306      *---------------------------------------------------------------*
120500070320      *?   INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL               ?
120600070306      *---------------------------------------------------------------*
120700070306     C     INZZ2         BEGSR
120800070306     C*
120900070306     C* Messaggio o tasti funzionali
121000070306     c                   clear                   z2dmsg
121100070328     c                   clear                   ErrmsgZ          80
121200070307     C*
121300070306      * errore di selezione di + giri x richiesta interr.stesso giro
121400070306     c                   if        *in51 = *on
121500070328     c                   movel     ErrMsg(12)    ErrmsgZ
121600070306     c                   setoff                                       51
121700070306     c                   end
121800070319     c                   if        *in52 or *in54
121900070328     c                   movel     ErrMsg(13)    ErrmsgZ
122000070326     c                   setoff                                       5254
122100070306     c                   end
122200070326     c                   if        *in62
122300070328     c                   movel     ErrMsg(15)    ErrmsgZ
122400070326     c                   setoff                                       62
122500070319     c                   end
122600070319     c                   if        *in53 or *in55
122700070328     c                   movel     ErrMsg(14)    ErrmsgZ
122800070319     c                   setoff                                       5355
122900070306     c                   end
123000070413     c                   if        *in59
123100070413     c                   movel     ErrMsg(29)    ErrmsgZ
123200070413     c                   setoff                                       59
123300070413     c                   clear                   h1ndc_piu
123400070413     c                   end
123500070328      *
123600070328      * imposta eventuali errori
123700070328     c                   eval      z2dmsg = ErrMsgZ
123800070307     C*
123900070307      * tasti funzionali se non è stato emesso alcun messaggio
124000070328     c                   if        ErrMsgZ = *blank
124100070307     c                   setoff                                       99
124200070307     c                   movel     Fun_3         z2dmsg
124300070307     c                   move      Fun_12        z2dmsg
124400070307     c                   eval      %subst(z2dmsg:10:14) = FUN_1
124500070326     c                   end
124600070306     C*
124700070306     C                   ENDSR
124800070301     C/EJECT
124900070301      *---------------------------------------------------------------*
125000070320      *?  INIZIALIZZAZIONE LISTA Bolle da Giri o da Distinta         ?
125100070301      *---------------------------------------------------------------*
125200070301     C     INZS2         BEGSR
125300070301     C*
125400070301     C* reset indicatori DSPATR
125500070308     c                   exsr      INZ_indERR
125600070301     C* pulizia SFL
125700070301     C                   SETOFF                                         3031
125800070301     C                   WRITE     FDG0C02
125900070301     C                   SETON                                          31
126000070301     C*
126100070301     C* Righe x Pagina del 1° SFL
126200070301     c                   z-add     6             s2pag
126300070301      *
126400070301     C* CARICAMENTO SFL totale
126500070301     C                   Z-ADD     0             S2NRR
126600070301     C                   Z-ADD     1             C2RCD
126700070301     C                   Z-ADD     0             WMAX
126800070302     C                   Z-ADD     0             Wsfl
126900070302     C* campi titolo
127000070302     C                   MOVEL     dsPGM         c2CPGM
127100070302     C                   MOVEL     knsif         c2nsif
127200070302     C                   MOVEL     knmus         c2nmus
127300070302     C                   MOVEL     RSUT          c2crsu
127400070306      *
127500070302     c                   eval      C2dsce = OPZ_2
127600070306     c                   eval      C2dess = 'dalla Distinta nr.' +
127700070319     c                             %trim(%editc(h1ndc_meno:'Z'))
127800070301      *
127900070301      *   Carica il SFL
128000070301     c                   EXSR      ROLS2
128100070301     C*
128200070301     C                   Z-ADD     1             WPAG
128300070301     C*
128400070301     C                   ENDSR
128500070301      *---------------------------------------------------------------*
128600070320      *?  CARICAMENTO PAGINA da SFL dei GIRI                         ?
128700070301      *---------------------------------------------------------------*
128800070301     C     ROLS2         BEGSR
128900070301     C*
129000070301     C                   SETOFF                                       32
129100070301     C                   Z-ADD     0             Y
129200070301     C                   Z-ADD     WMAX          S2NRR
129300070301     C*
129400070301      * ?      carica da Numero Distinta
129500070301     c                   exsr      CAR_da_NDC
129600070301     C* Fine Caricamento
129700070301     C                   Z-ADD     S2NRR         WMAX                 30
129800070301     C*
129900070302     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
130000070301     C     S2NRR         DIV       S2pag         PAGINE            4 0
130100070301     C                   MVR                     RESTO             3 0
130200070301     C     PAGINE        MULT      S2pag         C2RCD
1303000703011    C     RESTO         IFGT      0
130400070301     C                   ADD       1             C2RCD
1305000703011E   C                   ELSE
130600070301     C                   SUB       S2pag         C2RCD
130700070301     C                   ADD       1             C2RCD
1308000703011-   C                   ENDIF
130900070301     C*
131000070301     C                   ENDSR
131100070306      *---------------------------------------------------------------*
131200070316      *  ?  Carica da Numero Distinta                                ?
131300070306      *---------------------------------------------------------------*
131400070306     C     CAR_da_NDC    BEGSR
131500070306     C*
131600070306      * ? ----------------------------
131700070404      *
131800070306     c* se il p.o. è in parallelo testo la distinta su ARG altrimenti
131900070306     c* se il p.o. è partito testo la distinta su ARB e ARG
132000070306     c                   if        test_Parallelo = 'S'
132100070306     C/EXEC SQL
132200070306     C+ DECLARE I1 CURSOR FOR SELECT fiarg02l.*, fnarb01l.* FROM fiarg02l
132300070306     C+ JOIN fnarb01l ON arbaas= argaas and arblnp=arglnp and arbnrs=
132400070416     C+ argnrs and arbnsp= argnsp WHERE argpoc = :d1cFGS and argNDC
132500070416     C+ between :h1ndc_meno and :h1ndc_men2
132600070306     C/END-EXEC
132700070306
132800070306     C/EXEC SQL
132900070306     C+ OPEN I1
133000070306     C/END-EXEC
133100070306     c                   else
133200070404      **
133300070404     c                   z-add     wfgs          wfgs_ifp
133400070404      * ? ----------------------------
133500070404     c     karb70        setll     fnarb70l
133600070404      * ? ----------------------------
133700070306     c                   end
133800070306      * ? ----------------------------
133900070306      * ? Carico il SFL
134000070306      * ?     leggendo le bolle valide
134100070306     c                   exsr      LEGGE_Bolle
134200070306     C*
134300070306     C* Leggo dal file per caricare la lista
1344000703061    C     $EFILE        DOWEQ     *OFF
134500070306     C*
134600070306     C                   clear                   FDG0S02
134700070306      *
134800070306     c                   exsr      RIGA_SFL2
134900070306      *
135000070306     C                   ADD       1             S2NRR
135100070306     C                   ADD       1             Y
135200070306     C                   WRITE     FDG0S02
135300070306     C*
135400070306      * Lettura di un record
135500070306     c                   exsr      LEGGE_Bolle
1356000703061-   C                   ENDDO
135700070306      * ?CHIUSURE FILE
135800070306     c                   exsr      CLOF_S2
135900070306     C*
136000070306     C                   ENDSR
136100070302      *---------------------------------------------------------------*
136200070320      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
136300070302      *---------------------------------------------------------------*
136400070302     C     LEGGE_Bolle   BEGSR
136500070302     C*
136600070302     C*  Per EoF deve uscire
136700070302     C                   MOVEL     *OFF          $EFILE
136800070302     C*  Per Record Ok deve uscire riportando i dati
136900070302     C                   MOVEL     *OFF          $RCDOK
137000070302     C*
1371000703021    C     $EFILE        DOUEQ     *ON
137200070302     C     $RCDOK        OREQ      *ON
137300070302     C*
137400070416      *  x Togliere
137500070306     c                   if        test_Parallelo = 'S'
137600070306      * ?LETTURE  FILE
137700070306     C/EXEC SQL
137800070306     C+ FETCH NEXT FROM i1 INTO  :fiargds, :fnarbds
137900070306     C/END-EXEC
138000070306     c                   else
138100070404      **
138200070416     c     wfgs_ifp      reade     fnarb70l
138300070404      * Fine File
138400070404     C                   eval      sqlcod = 0
138500070416      *
138600070404     c                   if        %Eof(fnarb70l)
138700070404     C                   eval      sqlcod = 100
138800070416     c                   else
138900070416     c                   if        arbndc > h1ndc_men2
139000070416     C                   eval      sqlcod = 100
139100070416     c                   end
139200070416     c                   end
139300070404      **
139400070306     c                   end
139500070306      *
139600070302     C                   select
139700070302      * Fine File
139800070404     C                   when      sqlcod = 100
139900070302     C                   MOVEL     *ON           $EFILE
140000070302     C                   MOVE      $EFILE        *IN33
140100070302      * errori
140200070302     C                   WHEN      SqlCod < 0
140300070302     c                   if        sqlcod <> -913
140400070302     C                   seton                                        H1
140500070302     c                   end
140600070302      *
140700070302     C                   other
140800070302      *
140900070306     C                   MOVE      *ON           $RCDOK
141000070306      *
141100070302     C                   endsl
141200070302     C*
1413000703021-   C                   ENDDO
141400070302     C*
141500070302     C                   ENDSR
141600070301      *---------------------------------------------------------------*
141700070320      *?  Imposta Campi del 2°SFL                                    ?
141800070301      *---------------------------------------------------------------*
141900070301     C     RIGA_SFL2     BEGSR
142000070301     C*
142100070301     C* Salva il record di ARB per riposizionamento seguente
142200070301     c                   movel     fnarbds       fnarbdsav
142300070301     C* Dati Bolla
142400070301      *  Salva la chiave anche per riposizionarsi in seguito
142500070301     c                   z-add     ARBaas        S2Caas
142600070301     c                   z-add     ARBlnp        S2Clnp
142700070301     c                   z-add     ARBnrs        S2Cnrs
142800070301     c                   z-add     ARBnsp        S2Cnsp
142900070301      *
143000070416     c                   z-add     ARBndc        S2ndc
143100070416      *
143200070301     c                   movel     ARBcbo        S2Ccbo
143300070301     c                   movel     ARBrsd        S2Crsd
143400070301     c                   movel     ARBind        S2Cind
143500070301     c                   z-add     ARBlna        S2Clna
143600070301     c                   z-add     ARBznc        S2Cznc
143700070301     c                   movel     ARBgc1        S2Cgc1
143800070301     c                   movel     ARBgc2        S2Cgc2
143900070301     c                   movel     ARBcad        S2Ccad
144000070301     c                   movel     ARBlod        S2Clod
144100070301     c                   movel     ARBprd        S2Cprd
144200070301     c                   movel     ARBrsm        S2Crsm
144300070301     c                   movel     ARBtsp        S2Ctsp
144400070301     c                   z-add     ARBhcr        S2Chcr
144500070302      *  Pesi Volumi Colli da consegnare
144600070305     c                   exsr      Cal_PesVol
144700070302      *   i pesi/volumi/Colli Reali da consegnare
144800070302     c                   z-add     D27PKLDCO     S2Cpkf
144900070302     c                   z-add     D27VLUDCO     S2Cvlf
145000070302     c                   z-add     D27NCLDCO     S2Cncl
145100070301      *
145200070301     C                   move      ARBdcr        MMGG              4 0
145300070301     C                   MOVEL     MMGG          MM                2 0
145400070301     C                   MOVE      MMGG          GG                2 0
145500070301     C                   MOVE      MM            S2Cgmc
145600070301     C                   MOVEL     GG            S2Cgmc
145700070301      * Anomalia
145800070301     C     ARBfan        IFEQ      'S'
145900070301     C                   MOVEL     'ANOM.'       S2Dano
146000070301     C                   END
146100070328      * Data Consegna Prevista
146200070328     c                   clear                   tnsd99ds
146300070328     c                   eval      D98TBO  = 'A'
146400070328     c                   eval      D98AAS  = ARBaas
146500070328     c                   eval      D98LNP  = ARBlnp
146600070328     c                   eval      D98NRS  = ARBnrs
146700070328     c                   eval      D98NSP  = ARBnsp
146800070328     c                   call      'TNSD99R'
146900070328     c                   parm                    tnsd99ds
147000070328     c                   if        D98err  = '0'
147100070328     c                   move      d98DCE        s2cdac
147200070403     c                   move      s2cdac        GG                2 0
147300070403     c                   movel     s2cdac        AA                2 0
147400070403     c                   movel     GG            s2cdac
147500070403     c                   move      AA            s2cdac
147600070328     c                   end
147700070301      * Tipo Consegna
147800070301     C                   Z-ADD     1             XY                3 0
147900070301     C     ARBtc1        LOOKUP    C1P(XY)                                23
148000070301     C     *IN23         IFEQ      '1'
148100070301     C                   MOVEL     D1P(XY)       S2Dtc1
148200070301     C                   END
148300070301     C                   Z-ADD     1             XY
148400070301     C     ARBtc2        LOOKUP    C1P(XY)                                23
148500070301     C     *IN23         IFEQ      '1'
148600070301     C                   MOVEL     D1P(XY)       S2Dtc2
148700070301     C                   END
148800070301     C*
148900070301     C*  Controllo il tipo bolla
149000070301     C                   Z-ADD     1             X
149100070301     C                   MOVEL     *BLANKS       WTBOL1            1
149200070302     C                   MOVEL     *BLANKS       WTBOL2            1
149300070301     C                   CLEAR                   DS3A
149400070301     C     arbCBO        LOOKUP    C3A(X)                                 13
149500070301     C     *IN13         IFEQ      '1'
149600070301     C                   MOVEL     D3A(X)        DS3A
149700070301     C                   MOVEL     §3ATB1        WTBOL1
149800070301     C                   MOVEL     §3ATB2        WTBOL2
149900070301     C                   END
150000070301     C*
150100070301     C*  Campi di comodo per Bolla legata precedente:
150200070301     C                   clear                   comASS           13 3
150300070301     C                   clear                   comVCA            3
150400070301     C                   clear                   comTIC            2
150500070301     C*
150600070301      *?Controlla se esiste una bolla precedente legata ?
150700070301     C     Karb1         Chain     FnLBL01L
150800070301      *
150900070301      * > Se ho una bolla "mamma" ed è locale
151000070301     C                   if        %Found(FnLBL01L) and  arbLNA = lblLAP
151100070301      *
151200070301      * prende la bolla precedente sporcando i campi di ARB
151300070301     C     Klbl          CHAIN(n)  FnARB01L
151400070301      *
151500070301     C*  La prende in considerazione solo se non è
151600070301     C*   un Reso Mittente e un'Avaria e non è un solo incasso
151700070301     C                   IF        %Found(FnARB01L) and
151800070301     C                             arbCCA <> '2' and arbCCA <> '6' and
151900070301     C                             (arbICC = ' ' or arbICC = 'R')
152000070301      *  ?Controllo il tipo bolla mamma?
152100070301     C                   Z-ADD     1             X
152200070301     C     arbCBO        LOOKUP    C3A(X)                                 23
152300070301     C     *IN23         IFEQ      '1'
152400070301     C                   MOVEL     D3A(X)        DS3A
152500070301     C*  Se bolla mamma con C/Assegno vado a reperirlo
152600070301     C     §3AFCA        IFNE      0
152700070301     C     Klbl          Chain     FIAR901L                           56
152800070301     C                   If        %Found(FIAR901L)
152900070301     C                   Z-ADD     ar9CAS        comASS
153000070301     C                   MOVEL     ar9VCA        comVCA
153100070301     C                   MOVEL     ar9TIC        comTIC
153200070301     C                   End
153300070301     C                   END
153400070301     C                   END
153500070301     C                   END
153600070301     C*
153700070302      *  ? Reimposta i campi di ARB dell'SQL ?
153800070301     c                   movel     fnarbdsav     fnarbds
153900070301     C                   END
154000070301     C*
154100070302     c                   if        arbICC = ' ' or arbICC = 'R'
154200070302     C     §3AFCA        IfNE      0
154300070302     C                   Z-ADD     0             ar9CAS
154400070302     C                   MOVEL     *BLANKS       ar9VCA
154500070302     C     Karb1         CHAIN     FiAR901L
154600070302     c                   iF        %Found(FiAR901L)
154700070302     C                   Z-ADD     ar9CAS        s2cCAS
154800070302     C                   MOVEL     ar9VCA        s2cVCA
154900070302     C                   MOVE      ar9TIC        s2cTIC
155000070302     C                   EnD
155100070302     C                   End
155200070302      *
155300070302     C     comASS        IFNE      *ZEROS
155400070302     C                   Z-ADD     comASS        s2cCAS
155500070302     C                   MOVEL     comVCA        s2cVCA
155600070302     C                   MOVE      comTIC        s2cTIC
155700070302     C                   EnD
155800070302     C                   End
155900070302     C*
156000070301     C* Verifico se devo gestire un assegnato
156100070301     C     WTbol1        IFNE      *BLANKS
156200070301     C     WTbol2        ORNE      *BLANKS
156300070302      *
156400070301     C* Se devo gestire 2° bolla imposto tipo record '2' x posiz.FIAR6
156500070301     C     WTbol2        IFNE      *BLANKS
156600070301     C                   MOVE      '2'           KTRC
156700070301     C* .... se devo gestire 1° bolla imposto tipo record '1'
156800070301     C                   ELSE
156900070301     C     WTbol1        IFNE      'F'
157000070301     C                   MOVE      '1'           KTRC
157100070301     C                   END
157200070301     C                   END
157300070301      *
157400070301     C* Aggancio FIAR6 x importo assegnato
157500070301     C     KAR6          CHAIN     FIAR601L
157600070302     c                   if        %Found(FIAR601L) and ar6DFT <> 0
157700070301      *
157800070302     C* Se ho effettuato un ritorno all'incasso controllo se
157900070302     C     arbICA        IFEQ      'R'
158000070302     C* ... si riferisce ad un incasso parziale
158100070302     C                   CLEAR                   ariIPP
158200070302     C                   MOVEL     ar6DIV        s2cVAS
158300070302     C                   Z-ADD     ar6IFT        s2cIMA
158400070301      *
158500070302     C                   movel     'A'           KTIP
158600070302     C     Kari          setll     FiARI01L
158700070302     C     Kari          reade     FiARI01L
158800070302     C* ... se si calcolo importo da incassare x differenza
158900070302     C                   DOW       not %EoF(FiARI01L)
159000070302      *
159100070302     C     ar6DIV        ifEQ      ariDIV
159200070302     C                   SUB       ariIPP        s2cIMA
159300070302     C                   ElsE
159400070302     C* Se la divisa di incasso è diversa converto importo già
159500070302     C* incassato nella divisa della bolla
159600070302     C                   CLEAR                   YEURDS
159700070302     C                   MOVEL     ariDIV        YECDVI
159800070302     C                   Z-ADD     ariIPP        YECIMI
159900070302     C                   MOVEL     ar6DIV        YECDVO
160000070302     C                   MOVEL     'H'           YECTAR
160100070302     C                   CALL      'YEURCO'
160200070302     C                   PARM                    YEURDS
160300070302     C                   SUB       YECIMO        s2cIMA
160400070302     C                   End
160500070302      *
160600070302     C     Kari          reade     FiARI01L
160700070302     C                   END
160800070302      *
160900070302     C                   ELSE
161000070302      *
161100070302     C* ... altrimenti imposto totale fattura
161200070302     C     arbICA        ifEQ      ' '
161300070302     C     arbICA        orEQ      'R'
161400070302     C                   Z-ADD     ar6IFT        s2cIMA
161500070302     C                   MOVEL     ar6DIV        s2cVAS
161600070302     C                   End
161700070302      *
161800070302     C                   END
161900070302     C                   END
162000070301      *
162100070302     C                   ENDif
162200070301     C*
162300070302     C* Accendo indicatori x assegnato (ND)
162400070302     C     s2cIMA        COMP      0                                      41
162500070302     C     s2cCAS        COMP      0                                      42
162600070301     C*
162700070301     C                   ENDSR
162800070307      *---------------------------------------------------------------*
162900070316      *  ?  Seleziona Tutti i record del SFL                         ?
163000070307      *---------------------------------------------------------------*
163100070307     C     F01_Tutti2    BEGSR
163200070307     C*
163300070314     C     WMAX          IFgt      0
163400070307     C*  Seleziona tutte le righe del SFL accendendo il SFLNXTCHG
163500070307     c                   z-add     1             nrr_s2
163600070307     c     nrr_s2        chain     FDG0S02                            21
163700070307     c                   dow       *in21 = *Off
163800070307     c                   move      '1'           s2sce
163900070307     C*
164000070307      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
164100070307     C                   MOVE      *ON           *IN32
164200070307     C                   UPDATE    FDG0S02
164300070307     C*
164400070307     c                   add       1             nrr_s2
164500070307     c     nrr_s2        chain     FDG0S02                            21
164600070307     c                   endDO
164700070314     c                   end
164800070307     C*
164900070307     C                   ENDSR
165000070301      *---------------------------------------------------------------*
165100070320      *?  CONTROLLO TESTATA LISTA                                    ?
165200070301      *---------------------------------------------------------------*
165300070301     C     CTRC2         BEGSR
165400070301     C                   MOVE      *OFF          *IN99
165500070301     C                   ENDSR
165600070301      *---------------------------------------------------------------*
165700070320      *?  CONTROLLO OPZIONI LISTA                                    ?
165800070301      *---------------------------------------------------------------*
165900070301     C     CTRS2         BEGSR
166000070301     C*
166100070301     C*  Pulizia campi Totali
166200070306     c                   clear                   SAV_S2cgi
166300070413     c                   clear                   primoGiro_Blk     1
166400070301     C*
166500070301     C                   MOVEL     *OFF          $ESCI
166600070301     C                   SETOFF                                       99
166700070301     C*
166800070301     C* Leggo il sfl solo se ci sono rcd
1669000703011    C     WMAX          IFGT      0
167000070302     C                   READC     FDG0S02                                21
167100070301     C*
167200070301     C* esce se fine sfl o errore che richiede l'uscita
1673000703012    C     *IN21         DOWEQ     *OFF
167400070301     C     $ESCI         ANDEQ     *OFF
167500070302     C                   Z-ADD     S2NRR         C2RCD
167600070301     C* ctrl su riga
167700070302     C                   EXSR      RECS2
167800070301      *
167900070301     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
1680000703013    C     *IN99         IFEQ      *ON
168100070301     C     $ESCI         OREQ      *ON
168200070301     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
168300070301     C* la ripristinerò a conclusione del ciclo di READC
168400070302     C                   MOVE      *OFF          $INZS2
1685000703013-   C                   ENDIF
168600070301     C*
168700070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
168800070308     c                   if        s2sce = '1'  or s2sce = '5'
168900070301     C                   MOVE      *ON           *IN32
169000070307     c                   end
169100070302     C                   UPDATE    FDG0S02
169200070301     C*
169300070301     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1694000703013    C     $ESCI         IFEQ      *OFF
169500070302     C                   READC     FDG0S02                                21
169600070301      *
169700070301     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1698000703014    C     *IN21         IFEQ      *ON
169900070301     C* calcolo pagina a cui deve posizionarsi
170000070302     c                   z-add     s2pag         Wpagine
170100070301     C                   EXSR      CLCPAG
1702000703014-   C                   ENDIF
1703000703013-   C                   ENDIF
170400070301     C*
1705000703012-   C                   ENDDO
170600070301     C*
1707000703011-   C                   ENDIF
170800070301     C*
170900070301     C                   ENDSR
171000070301     C/EJECT
171100070301      *---------------------------------------------------------------*
171200070320      *?  CONTROLLO CAMPI I/O RIGA LISTA                             ?
171300070301      *---------------------------------------------------------------*
171400070302     C     RECS2         BEGSR
171500070301     C*
171600070301     C* reset indicatori DSPATR
171700070308     c                   exsr      INZ_indERR
171800070327     C*
171900070327     C* gestione opzioni
1720000703273    C     S2sce         IFNE      *blank
172100070327     C     *IN99         ANDEQ     *OFF
172200070327     C                   EXSR      SCE_S2
1723000703273-   C                   ENDIF
172400070301     C*
172500070301     C                   ENDSR
172600070301      *---------------------------------------------------------------*
172700070320      *?  GESTIONE OPZIONI LISTA                                     ?
172800070301      *---------------------------------------------------------------*
172900070309     C     SCE_S2        BEGSR
173000070307     C*
173100070307     C*  Interrogazione bolla
173200070307     c                   If        s2SCE = '5'
173300070307     c                   eval      kpjbus = kpjbu
173400070307     C                   clear                   dslr36
173500070307     C                   move      s2cAAS        P36AAS
173600070307     C                   move      s2cLNP        P36LNP
173700070307     C                   move      s2cNRS        P36NRS
173800070307     C                   move      s2cNSP        P36NSP
173900070307     C                   MOVEL     '1'           P36FLG
174000070307     C                   MOVEL     ' '           P36F03
174100070307     C                   movel(p)  dslr36        KPJBU
174200070307     C                   call      'FNLR36R'                            99
174300070307     C                   parm                    KPJBA
174400070307     c                   movel     kpjbus        kpjbu
174500070307     c                   clear                   s2SCE
174600070307     c                   seton                                        99
174700070307     c                   endIf
174800070301     C*
174900070301     C*  Totalizza le Spedizioni/Pesi/Volumi
175000070302     c                   If        s2SCE = '1'
175100070313     C*
175200070416     c                   add       1             totSPE_s2         5 0
175300070313     c                   move      s2cgi         sav_sxcgi
175400070305     C*
175500070305     C*   Conta quanti giri sono stati scelti nel sfl
175600070305     C*  per permettere di interrogare con l'F4 con stesso giro
175700070413     c                   if        s2cgi <> SAV_s2cgi  or
175800070413     c                             primoGiro_Blk = *Blank and s2cgi = *Blank
175900070413     C*
176000070413      *  e se il giro è Blank non deve ripetere un eventuale conto di altri con giro Blank
176100070413     c                   eval      primoGiro_Blk = 'S'
176200070413     C*
176300070305     c                   move      s2cgi         sav_s2cgi
176400070305     c                   end
176500070305     C*
176600070301     c                   endIf
176700070301     C*
176800070301     C                   ENDSR
176900070301     C/EJECT
177000070302      *---------------------------------------------------------------*
177100070320      *?  Chiusura Files 2°SFL                                       ?
177200070302      *---------------------------------------------------------------*
177300070302     C     CLOF_S2       BEGSR
177400070306      *
177500070416     c                   if        test_Parallelo = 'S'
177600070306     C/EXEC SQL
177700070306     C+ CLOSE i1
177800070306     C/END-EXEC
177900070306     c                   end
178000070306      *
178100070302     c                   ENDSR
178200070302     C/EJECT
178300070301      *---------------------------------------------------------------*
178400070316      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
178500070301      *---------------------------------------------------------------*
178600070228     C     CLCPAG        BEGSR
178700940224     C* Input :
178800940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
178900940224     C*          cursore
179000070301     C* - Wpagine= numero rcd per pagina sfl
179100940224     C* Output :
179200940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
179300940224     C*
179400070301     C     WSFL          DIV       Wpagine       PAGINE            4 0
179500940224     C                   MVR                     RESTO             3 0
179600940224     C     RESTO         IFGT      0
179700940224     C                   ADD       1             PAGINE
179800940224     C                   ENDIF
179900070301     C     PAGINE        MULT      Wpagine       WPAG
180000940224     C*
180100940224     C                   ENDSR
180200070302     C************************************************************
180300070320      *?  Emissione ultima finestra di riepilogo                     ?
180400070302     C************************************************************
180500070302     C     GESW1         BEGSR
180600070302     C*
180700070302     C* inizializzazione videata
180800070302     C     $INZW1        IFEQ      *ON
180900070302     C                   EXSR      INZW1
181000070302     C                   MOVE      *OFF          $INZW1
181100070302     C                   ENDIF
181200070302     C*
181300070302     c     Window_1      tag
181400070302     C*  Emissione Video
181500070302      * ?            *-----------------------*
181600070302     C                   EXFMT     FDG0W01
181700070302      * ?            *-----------------------*
181800070326     c                   setoff                                       99
181900070302     C* Selezioni
1820000703021    C                   SELECT
182100070302     C* F3=Fine
182200070302     C                   WHEN      $TASTO   =   F03
182300070302     C                   EXSR      F03_exit
182400070319     c                   rolbk
182500070302     C* F12=Ritorno
182600070302     C                   WHEN      $TASTO   =   F12
182700070308     c                   exsr      INZ_indERR
182800070305     c                   move      'S2'          $GEST
182900070302      *
1830000703021O   C                   OTHER
183100070302     C* CONTROLLO DATI
183200070302     C                   EXSR      CTRW1
1833000703021-   C                   ENDSL
183400070302     C*
183500070302     C* Per errori o decodifiche
183600070302     c   99              goto      Window_1
183700070302     C*
183800070302     C*    Aggiorna il Data Base
183900070302     C                   if        $TASTO   =   F06
184000070302     C                   EXSR      F06_Aggiorna
184100070302     c                   end
184200070302      *
184300070302     C                   ENDSR
184400070302     C/EJECT
184500070302     C************************************************************
184600070320      *?  INIZIALIZZA indicatori di errore                           ?
184700070302     C************************************************************
184800070308     C     INZ_indERR    BEGSR
184900070308      *
185000070308     C                   move      *ALL'0'       IN5190           40
185100070308     C                   moveA     IN5190        *IN(51)
185200070308      *
185300070308     C                   ENDSR
185400070308     C/EJECT
185500070308     C************************************************************
185600070316      * ?  INIZIALIZZAZIONE Window Finale prima dell'aggiornamento   ?
185700070308     C************************************************************
185800070308     C     INZW1         BEGSR
185900070308     C*
186000070302     C* Pulizia campi e indicatori
186100070308     c                   exsr      INZ_indERR
186200070302     c                   setoff                                       99  50
186300070306     c                   setoff                                       515253
186400070306     c                   setoff                                       545556
186500070302     C* Pulizia formato record
186600070302     c                   clear                   FDG0W01
186700070306      *
186800070306     C*  Togliere
186900070319     c                   z-add     h1ndc_meno    w1ndc1
187000070306     c                   move      Dalla         w1ada
187100070326     c                   eval      %subst(W1dSCE:7:11) = OPZ_2
187200070416     c                   seton                                        50
187300070416      *
187400070416     c                   z-add     h1ndc_men2    w1ndc2
187500070306      *
187600070305      *  Legge il foglio
187700070305     c                   move      w1ndc1        wNFV
187800070319     c                   eval      Aggior_Dist = 'N'
187900070319      *                                     ?  =======  ?
188000070316     c                   exsr      CHAIN_Dist
188100070319      *
188200070316     c                   if        Found_Dist = 'N'
188300070305     c                   movel     *all'?'       w1pdr1
188400070305     c                   else
188500070305     C                   move      'A'           tipo_cod
188600070305     C                   move      dstpdr        cod_padr
188700070305     C     Kapd          CHAIN     fiapd01l
188800070305     c                   if        %Found(fiapd01l)
188900070305     c                   movel     APDRSC        w1pdr1
189000070305     c                   end
189100070305      *
189200070305     c                   z-add     DSTPKL        W1DMP1
189300070305     c                   z-add     DSTVLU        W1DMV1
189400070305     c                   z-add     DSTNBL        W1DMN1
189500070305      *
189600070305     c                   z-add     DSTPKLA       W1DAP1
189700070305     c                   z-add     DSTVLUA       W1DAV1
189800070305     c                   z-add     DSTNBLA       W1DAN1
189900070305     c                   end
190000070305     C*
190100070306      *  Legge il foglio
190200070316     c                   move      w1ndc2        wNFV
190300070319     c                   eval      Aggior_Dist = 'N'
190400070319      *                                     ?  =======  ?
190500070316     c                   exsr      CHAIN_Dist
190600070319      *
190700070316     c                   if        Found_Dist = 'N'
190800070306     c                   movel     *all'?'       w1pdr2
190900070306     c                   else
191000070306     C                   move      'A'           tipo_cod
191100070306     C                   move      dstpdr        cod_padr
191200070306     C     Kapd          CHAIN     fiapd01l
191300070306     c                   if        %Found(fiapd01l)
191400070306     c                   movel     APDRSC        w1pdr2
191500070306     c                   end
191600070306      *
191700070306     c                   z-add     DSTPKL        W1DMP2
191800070306     c                   z-add     DSTVLU        W1DMV2
191900070306     c                   z-add     DSTNBL        W1DMN2
192000070306      *
192100070306     c                   z-add     DSTPKLA       W1DAP2
192200070306     c                   z-add     DSTVLUA       W1DAV2
192300070306     c                   z-add     DSTNBLA       W1DAN2
192400070306     c                   end
192500070306      *
192600070306     c     w1tsn1        comp      W1DMN1                             51
192700070306     c     w1tsp1        comp      W1DMP1                             52
192800070306     c     w1tsv1        comp      W1DMV1                             53
192900070306      *
193000070306     c     w1tsn2        comp      W1DMN2                             54
193100070306     c     w1tsp2        comp      W1DMP2                             55
193200070306     c     w1tsv2        comp      W1DMV2                             56
193300070306      *
193400070302     C                   ENDSR
193500070302     C************************************************************
193600070320      *?  Controlli primo video di SCELTA Operazione da eseguire     ?
193700070302     C************************************************************
193800070302     C     CTRW1         BEGSR
193900070302     C*
194000070302     C*  Spegne indicatori di errore
194100070308     c                   exsr      INZ_indERR
194200070302     c                   setoff                                       99
194300070306     C*  se superano i MAX.previsti in distinta
194400070306     c     w1tsn2        comp      W1DMN2                             54
194500070306     c     w1tsp2        comp      W1DMP2                             55
194600070306     c     w1tsv2        comp      W1DMV2                             56
194700070306     c     w1tsn1        comp      W1DMN1                             51
194800070306     c     w1tsp1        comp      W1DMP1                             52
194900070306     c     w1tsv1        comp      W1DMV1                             53
195000070302     C*
195100070302     C     ENDCTRW1      ENDSR
195200070301      *---------------------------------------------------------------*
195300070320      *?  GESTIONE F03 x uscire LR                                   ?
195400070301      *---------------------------------------------------------------*
195500070227     C     F03_exit      BEGSR
195600070227     C* fine programma
195700940309     C                   MOVE      *ON           $FINE
195800070302      *
195900940309     C                   ENDSR
196000070302     C/EJECT
196100070302      *---------------------------------------------------------------*
196200070320      *?  GESTIONE F06 x aggiornare il Data Base                     ?
196300070302      *---------------------------------------------------------------*
196400070302     C     F06_Aggiorna  BEGSR
196500070306      **
196600070306      **  Leggendo sempre le bolle selezionate dal 2°SFL
196700070309      **   esegue aggiornamento degli archivi e se ci sono problemi
196800070309      **  durante l'aggiornamento delle singole bolle emette 3°SFL con la
196900070309      **   lista delle bolle non aggiornate avvertendo del mancato aggiornamento.
197000070309      **
197100070319      ** L'utente deve quindi rieseguire l'operazione poichè viene annullata tutta
197200070319      ** la transazione con il Rolback.
197300070309      **
197400070309     C*  Esegue l'aggiornamento ed il controllo sul 3° SFL
197500070309     c                   move      'S3'          $GEST
197600070309     c                   move      *on           $INZS3
197700070305     C*
197800070302     C                   ENDSR
197900070301     C/EJECT
198000070301     C*---------------------------------------------------------------*
198100070320      *?  Controlla validità della spedizione                        ?
198200070301     C*---------------------------------------------------------------*
198300070301     C     Check_Sped    BEGSR
198400070301     C*
198500070301      * controlla che il record sia giusto da prendere
198600070301     c* chiamo pgm per vedere se spedizione da mettere in distinta
198700070301     c                   clear                   fidg16ds
198800070301     c                   eval      d16aasi = arbaas
198900070301     c                   eval      d16lnpi = arblnp
199000070301     c                   eval      d16nrsi = arbnrs
199100070301     c                   eval      d16nspi = arbnsp
199200070301     c                   eval      d16consi= 'S'
199300070308      * deve essere impostata la data
199400070416     c                   eval      d16dfvi = h1cDDC
199500070416     c                   if        test_Parallelo = 'S'
199600070416     c                   eval      d16ndci = 'G'
199700070416     c                   end
199800070301     c                   eval      kpjbus = kpjbu
199900070301     c                   eval      kpjbu = fidg16ds
200000070301     c                   call      'FIDG16R'
200100070301     c                   parm                    kpjba
200200070301     c                   parm                    fnarbds
200300070301     c                   parm                    fiargds
200400070301     c                   eval      fidg16ds = kpjbu
200500070301     c                   eval      kpjbu = kpjbus
200600070301     C*
200700070301     C                   ENDSR
200800070305     C*---------------------------------------------------------------*
200900070320      *?  Calcola Pesi Volumi e Colli da consegnare                  ?
201000070305     C*---------------------------------------------------------------*
201100070305     C     Cal_PesVol    BEGSR
201200070305     C*
201300070305     c                   eval      kpjbus = kpjbu
201400070323     c                   clear                   fidg27ds
201500070305     c                   movel     fnarbds       fnarbdsav
201600070323     c                   z-add     wddc          D27DFVI
201700070323     c                   z-add     arbAAS        d27aasI
201800070305     c                   z-add     arbLNP        d27lnpI
201900070305     c                   z-add     arbNRS        d27nrsI
202000070305     c                   z-add     arbNSP        d27nspI
202100070305     c                   eval      kpjbu = fidg27ds
202200070305     c                   call      'FIDG27R'
202300070305     c                   parm                    kpjba
202400070305     c                   parm                    fnarbdsav
202500070305     c                   eval      fidg27ds = kpjbu
202600070305     c                   eval      kpjbu = kpjbus
202700070305     C*
202800070305     C                   ENDSR
202900070309      *---------------------------------------------------------------*
203000070319      *     GESTIONE SFL di AGGIORNAMENTO e segnalazione problemi    ?
203100070309      *---------------------------------------------------------------*
203200070309     C     GESS3         BEGSR
203300070309     C*
203400070309     C* inizializzazione videata
203500070309     C     $INZS3        IFEQ      *ON
203600070309     C                   EXSR      INZS3
203700070309     C                   MOVE      *OFF          $INZS3
203800070309     C                   ENDIF
203900070309     C*
204000070319      *     FASE di COMMIT o ROLLBACK    ?
204100070319     c                   if        exec_RB = 'S'
204200070320      *  Esegue il ROLLBACK x un precedente ?
204300070320      *  problema su uno degli aggiornamenti  ?
204400070319     c                   ROLBK
204500070319     C                   else
204600070320      *   Esegue il COMMIT  --> TUTTO è OK. ?
204700070319     c                   COMMIT
204800070319     C                   end
204900070319     C*
205000070309     C* Inizializza e imposta piede videata
205100070309     C                   EXSR      INZZ3
205200070309     C*
205300070309     C* emissione piede videata
205400070309      * ?            *-----------------------*
205500070309     C                   WRITE     FDG0Z03
205600070309      * ?            *-----------------------*
205700070309     C*
205800070309     C     WMAX          IFEQ      0
205900070319     c                   if        exec_RB = 'N'
206000070319     C* Se è stato aggiornato tutto correttamente deve Emettere
206100070319     C*  il messaggio di TUTTO OK.
206200070330     c                   eval      vedi_testo = 'N'
206300070326     c                   eval      V03MSG = ErrMsg(25)
206400070319     c                   else
206500070319     C*  Qualcosa è andato storto
206600070330     c                   eval      vedi_testo = 'S'
206700070326     c                   eval      V03MSG = ErrMsg(26)
206800070319     c                   end
206900070330      **
207000070309      * ?            *-----------------------*
207100070309     C                   WRITE     FDG0V03
207200070309      * ?            *-----------------------*
207300070309     c                   setoff                                       30
207400070309     C                   Else
207500070309     c                   seton                                        30
207600070309      *
207700070309     C     Wsfl          IFgt      0
207800070309     C                   Z-ADD     wsfl          C3RCD
207900070309     C                   Else
208000070309     C     Wpag          IFgt      0
208100070309     C                   Z-ADD     wpag          C3RCD
208200070309     C                   EndIF
208300070309     C                   EndIF
208400070309     C                   ENDIF
208500070309      *
208600070309      *  Emissione SFL dopo Aggiornamento Data Base
208700070319      *    con errori quindi tutta la transazione deve essere rifatta.
208800070330     c                   if        vedi_testo = 'S'
208900070309      * ?            *-----------------------*
209000070309     C                   EXFMT     FDG0C03
209100070309      * ?            *-----------------------*
209200070330     c                   end
209300070326     c                   setoff                                       99
209400070309      *
209500070309     C     C3NRR         IFNE      0
209600070309     C                   Z-ADD     C3NRR         WSFL
209700070309     C                   ENDIF
209800070309     C                   Z-ADD     SFLNRR        C3RCD
209900070309     C* Selezioni
2100000703091    C                   SELECT
210100070309     C* F3=Fine
210200070309     C     $TASTO        WHENEQ    F03
210300070309     C                   EXSR      F03_exit
210400070309      *
2105000703091O   C                   OTHER
210600070309     C* CONTROLLO testata
210700070309     C                   EXSR      CTRC3
210800070309     C* CONTROLLO subfile
210900070309     C     *IN99         IFEQ      *OFF
211000070309     C                   EXSR      CTRS3
211100070309     C                   END
211200070309      *
211300070309      * Riemette il primo video
211400070309     C                   movel     'D1'          $GEST
211500070309      *   non reinizializzato se ci sono stati problemi
211600070309     C     WMAX          ifgt      0
211700070309     C                   MOVE      *OFF          $INZD1
211800070316     c                   rolbk
211900070316      *
212000070309     c                   else
212100070309      *   reinizializzato se non ci sono stati problemi
212200070309     C                   MOVE      *ON           $INZD1
212300070309     c                   end
212400070330      *?  Attenzione?
212500070330      *?    se si era scelto di Assegnare si deve ritornare sul SFL dei GIRI
212600070330      *?    dove era impostata la scelta.
212700070330      *
2128000703091-   C                   ENDSL
212900070309     C*
213000070309     C                   ENDSR
213100070309     C/EJECT
213200070309      *---------------------------------------------------------------*
213300070320      *?  INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                ?
213400070309      *---------------------------------------------------------------*
213500070309     C     INZZ3         BEGSR
213600070309     C*
213700070309     C* Messaggio o tasti funzionali
213800070309     c                   clear                   z3dmsg
213900070309     c                   clear                   z3der1
214000070309     c                   clear                   z3der2
214100070309     C*
214200070309     C* se ci sono dei records allora ci sono stati dei problemi
214300070319     C*  durante l'aggiornamento bolla x bolla
214400070309     C     WMAX          IFGT      0
214500070309     C*
214600070309     c                   seton                                        51
214700070326     c                   movel     ErrMsg(21)    z3der1
214800070326     c                   movel     ErrMsg(22)    z3der2
214900070309     C*
215000070319     c                   ELSE
215100070319     C* altrimenti
215200070319     C*  Se ci sono stati dei problemi durante gli aggiornamenti
215300070319     C*   o su ARB o su DST tanto da eseguire comunque il ROLLBACK
215400070319     C*    imposta un altro tipo di messaggio.
215500070319     c                   if        exec_RB = 'S'
215600070319     c                   seton                                        51
215700070326     c                   movel     ErrMsg(23)    z3der1
215800070326     c                   movel     ErrMsg(24)    z3der2
215900070319     c                   end
216000070319     C*
216100070309     c                   END
216200070309     C*
216300070309      * tasti funzionali se non è stato emesso alcun messaggio
216400070309     c                   if        z3dmsg = *blank
216500070309     c                   setoff                                       99
216600070309     c                   movel     Fun_3         z3dmsg
216700070309     c                   eval      %subst(z3dmsg:40:13) = FUN_9_Drop
216800070309     c                   end
216900070309     C*
217000070309     C                   ENDSR
217100070309     C/EJECT
217200070309      *---------------------------------------------------------------*
217300070320      *?  INIZIALIZZAZIONE LISTA Bolle da Giri o da Distinta         ?
217400070309      *---------------------------------------------------------------*
217500070309     C     INZS3         BEGSR
217600070309     C*
217700070309     C* reset indicatori DSPATR
217800070309     c                   exsr      INZ_indERR
217900070309     C* pulizia SFL
218000070309     C                   SETOFF                                         3031
218100070309     C                   WRITE     FDG0C03
218200070309     C                   SETON                                          31
218300070309     C*
218400070309     C* Righe x Pagina del 1° SFL
218500070309     c                   z-add     8             s3pag
218600070309      *
218700070309     C* CARICAMENTO SFL totale
218800070309     C                   Z-ADD     0             S3NRR
218900070309     C                   Z-ADD     1             C3RCD
219000070309     C                   Z-ADD     0             WMAX
219100070309     C                   Z-ADD     0             Wsfl
219200070309     C* campi titolo
219300070309     C                   MOVEL     dsPGM         c3CPGM
219400070309     C                   MOVEL     knsif         c3nsif
219500070309     C                   MOVEL     knmus         c3nmus
219600070309     C                   MOVEL     RSUT          c3crsu
219700070309      * TIPO FUNZIONE
219800070309     c                   eval      C3dsce = OPZ_2
219900070309     c                   eval      C3dess = 'dalla Distinta nr.' +
220000070319     c                             %trim(%editc(h1ndc_meno:'Z'))
220100070309      *   Carica il SFL
220200070309     c                   EXSR      ROLS3
220300070309     C*
220400070309     C                   Z-ADD     1             WPAG
220500070309     C*
220600070309     C                   ENDSR
220700070309      *---------------------------------------------------------------*
220800070320      *? Esegue aggiornamento del Data Base caricando il SFL3 x problemi ?
220900070309      *---------------------------------------------------------------*
221000070309     C     ROLS3         BEGSR
221100070309     C*
221200070309     C                   SETOFF                                       32
221300070309     C                   Z-ADD     0             Y
221400070309     C                   Z-ADD     WMAX          S3NRR
221500070309     C*
221600070319      *? L'aggiornamento è pilotato dalle scelte effettuate sul SFL2 ?
221700070319      *? delle bolle e il caricamento del SFL3 serve per elencare    ?
221800070319      *? le bolle che non sono state aggiornate x qualsiasi motivo   ?
221900070319      *? classico: ALLOCAZIONE DEL RECORD.                           ?
222000070319     C*
222100070319     c                   exsr      AGG_da_SFL2
222200070319      *
222300070319      *  Dopo aver aggiornato i files x singola bolla, aggiorna ?
222400070319      *   a livello di distinta. Sempre che già non sia da      ?
222500070319      *   eseguire il ROLL_BACK x precedenti problemi.          ?
222600070319     C*
222700070319     c                   if        Exec_RB <> 'S'
222800070319     c                   exsr      Agg_x_Distinta
222900070319     c                   end
223000070319      *
223100070309     C* Fine Caricamento
223200070309     C                   Z-ADD     S3NRR         WMAX                 30
223300070309     C*
223400070309     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
223500070309     C     S3NRR         DIV       S3pag         PAGINE            4 0
223600070309     C                   MVR                     RESTO             3 0
223700070309     C     PAGINE        MULT      S3pag         C3RCD
2238000703091    C     RESTO         IFGT      0
223900070309     C                   ADD       1             C3RCD
2240000703091E   C                   ELSE
224100070309     C                   SUB       S3pag         C3RCD
224200070309     C                   ADD       1             C3RCD
2243000703091-   C                   ENDIF
224400070309     C*
224500070309     C                   ENDSR
224600070309      *---------------------------------------------------------------*
224700070320      *? Carica il SFL delle bolle non aggiornate se ci sono stati problemi?
224800070309      *---------------------------------------------------------------*
224900070319     C     AGG_da_SFL2   BEGSR
225000070309     C*
225100070319     C*  Si ricorda di eseguire il ROLL_BACK se si sono verificati dei
225200070319     C*   problemi in qualsiasi parte dell'aggiornamento del D.B.
225300070320     c                   move      'N'           Exec_RB           1
225400070319     C*
225500070309     C*   Legge il SFL dei Giri dove selezionato con (1) le righe
225600070309     C*    dei giri
225700070309     c                   Readc     FDG0S02                                21
2258000703092    C     *IN21         DOWEQ     *OFF
225900070309     C*
226000070309     C* Solo i selezionati
226100070309     c                   if        s2SCE = '1'
226200070309      * ? ----------------------------
226300070309      * ? Carico il SFL solo se ci sono dei problemi in aggiornamento delle bolle
226400070309      * ?  per allocazione dei records o altro .
226500070309      * ? ----------------------------
226600070309     c                   clear                   problemi          1
226700070309     C*
226800070309      * ?             *--------------------------*
226900070319     c                   exsr      AGG_Riga_DB
227000070309      * ?             *--------------------------*
227100070309     C*
227200070309     C* x Problemi sui records in aggiornamento
227300070309      * ? ----------------------------
227400070309     c                   if        Problemi = 'S'
227500070319     c                   move      'S'           Exec_RB           1
227600070319      *
227700070309     C                   clear                   FDG0S03
227800070309     c                   exsr      RIGA_SFL3
227900070309      *
228000070309      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
228100070309     c                   seton                                        32
228200070309     C                   ADD       1             S3NRR
228300070309     C                   ADD       1             Y
228400070309     C                   WRITE     FDG0S03
228500070309     c                   End
228600070309     C*
228700070309     c                   End
228800070309     C*
228900070309     c                   Readc     FDG0S02                                21
229000070309     c                   EndDo
229100070309     C*
229200070309     C                   ENDSR
229300070309      *---------------------------------------------------------------*
229400070320      *?  Imposta Campi del 3°SFL                                    ?
229500070309      *---------------------------------------------------------------*
229600070309     C     RIGA_SFL3     BEGSR
229700070309     C*
229800070309     C* Deve semplicemente impostare dal 2° al 3° SFL i campi
229900070309     c                   z-add     S2Caas        S3Caas
230000070309     c                   z-add     S2Clnp        S3Clnp
230100070309     c                   z-add     S2Cnrs        S3Cnrs
230200070309     c                   z-add     S2Cnsp        S3Cnsp
230300070309     c                   movel     S2Ccbo        S3Ccbo
230400070309     C                   z-add     S2Cgmc        S3Cgmc
230500070309     c                   z-add     S2Clna        S3Clna
230600070309     c                   z-add     S2Cznc        S3Cznc
230700070309     c                   z-add     S2Cpkf        S3Cpkf
230800070309     c                   z-add     S2Cvlf        S3Cvlf
230900070309     c                   z-add     S2Cncl        S3Cncl
231000070309     C                   MOVEL     S2Dano        S3Dano
231100070309     c                   movel     S2Crsm        S3Crsm
231200070309     c                   movel     s2CGI         s3CGI
231300070309     c                   movel     S2Crsd        S3Crsd
231400070309     c                   movel     S2Cind        S3Cind
231500070309     c                   movel     S2Ccad        S3Ccad
231600070309     c                   movel     S2Clod        S3Clod
231700070309     c                   movel     S2Cprd        S3Cprd
231800070309     C*
231900070309     C                   ENDSR
232000070309      *---------------------------------------------------------------*
232100070320      *?  CONTROLLO TESTATA LISTA                                    ?
232200070309      *---------------------------------------------------------------*
232300070309     C     CTRC3         BEGSR
232400070309     C                   MOVE      *OFF          *IN99
232500070309     C                   ENDSR
232600070309      *---------------------------------------------------------------*
232700070320      *?  CONTROLLO OPZIONI LISTA                                    ?
232800070309      *---------------------------------------------------------------*
232900070309     C     CTRS3         BEGSR
233000070309     C                   SETOFF                                       99
233100070309     C* reset indicatori DSPATR
233200070309     c                   exsr      INZ_indERR
233300070309     C                   ENDSR
233400070309     C/EJECT
233500070309      *---------------------------------------------------------------*
233600070319      * ??  Aggiornamento dei Files a livello BOLLA      (COMMIT)    ?
233700070309      *---------------------------------------------------------------*
233800070319     C     AGG_Riga_DB   BEGSR
233900070309     c*
234000070319     c*  Bolla x Bolla ossia riga x riga
234100070319     c*
234200070320      *   Aggiorna ARG x Bolla    ?
234300070316     c     kaggiorna     chain(e)  fiarg01l
234400070316     c*
234500070319      *  ?  Se ci sono problemi di allocazione record    ?
234600070316     c                   if        %Error
234700070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
234800070316     c                   eval      problemi = 'S'
234900070316     c                   clear                   trul82ds
235000070316     c                   eval      ul82§rrn = arg_nrr
235100070319     c                   eval      ul82§fil = 'FIARG01L'
235200070316     c                   eval      ul82§win = 'S'
235300070316     c                   eval      ul82§f7  = 'S'
235400070319     c                   eval      ul82§num = 2
235500070319     c                   eval      ul82§att = 2
235600070326     c                   eval      ul82§mss = ErrMsg(27)
235700070319     c                   Eval      UL82§msw = 'La Distinta '
235800070319     c                             + %editc(w1ndc1:'Z') +  ' '
235900070319     c                             + ' non è manutenzionabile.'
236000070316      * chiamo il pgm che manda il messaggio info all'utente
236100070316     c                   call(e)   'TRUL82R'
236200070316     c                   parm                    trul82ds
236300070319      * ancora allocata
236400070316     c                   if        ul82§sts = 'A'
236500070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
236600070319     c                   eval      problemi = 'S'
236700070316     c                   endif
236800070316     c*
236900070316     c                   else
237000070316     c*
237100070316     c                   if        %Found(fiarg01l)
237200070319      * ? Imposta i campi da aggiornare ?
237300070319     c                   clear                   ARGPDC
237400070319     c                   clear                   ARGFGS
237500070319     c                   clear                   ARGNDC
237600070319     c                   clear                   ARGDDC
237700070319     c                   clear                   ARGSTP
237800070319     c                   clear                   ARGNFTL
237900070319     c                   clear                   ARGSLB
238000070319     c                   clear                   ARGDTVDIS
238100070319     c                   clear                   ARGHVDIS
238200070319     c*
238300070316     c                   update    fiarg000
238400070406     c                   end
238500070316     c*
238600070316     c                   end
238700070316     c*-------
238800070320      *   Deve aggiornare  anche  ARB solo se  siamo in produzione  ?
238900070319     c                   if        test_Parallelo <> 'S'  and
239000070319     c                                   problemi <> 'S'
239100070319      * solo
239200070319      * se deve togliere
239300070416     c                   if        h1ndc_meno <> 0
239400070319      *
239500070319     c     kaggiorna     chain(e)  fnarb01l
239600070319     c*
239700070319      *  ?  Se ci sono problemi di allocazione record    ?
239800070319     c                   if        %Error
239900070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
240000070319     c                   eval      problemi = 'S'
240100070319     c*
240200070319     c                   clear                   trul82ds
240300070319     c                   eval      ul82§rrn = arb_nrr
240400070319     c                   eval      ul82§fil = 'FNARB01L'
240500070319     c                   eval      ul82§win = 'S'
240600070319     c                   eval      ul82§f7  = 'S'
240700070319     c                   eval      ul82§num = 2
240800070319     c                   eval      ul82§att = 2
240900070326     c                   eval      ul82§mss = ErrMsg(27)
241000070319     c                   Eval      UL82§msw = 'La Distinta '
241100070319     c                             + %editc(w1ndc1:'Z') +  ' '
241200070319     c                             + ' non è manutenzionabile.'
241300070319      * chiamo il pgm che manda il messaggio info all'utente
241400070319     c                   call(e)   'TRUL82R'
241500070319     c                   parm                    trul82ds
241600070319      * ancora allocata
241700070319     c                   if        ul82§sts = 'A'
241800070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
241900070319     c                   eval      problemi = 'S'
242000070319     c                   endif
242100070319     c*
242200070319     c                   else
242300070319     c*
242400070319     c                   if        %Found(fnarb01l)
242500070319     c                   eval      arbngd = 0
242600070319     c                   eval      arbndc = 0
242700070319     c                   eval      arbpdc = 0
242800070319     c                   eval      arbifp = 0
242900070319     c                   eval      arbddc = 0
243000070319     c                   eval      arbstp = 0
243100070319     c                   eval      arbngd = 0
243200070319     c                   eval      arbnss = 0
243300070319     c                   update    fnarb000
243400070319     c                   end
243500070319     c*
243600070319     c                   end
243700070319      *
243800070319     c                   end
243900070319     c                   end
244000070319     c*
244100070319     C                   ENDSR
244200070319     C/EJECT
244300070319      *---------------------------------------------------------------*
244400070319      * ??  Aggiornamento dei Files   a livello Distinta (COMMIT)    ?
244500070319      *---------------------------------------------------------------*
244600070319     C     AGG_x_DistintaBEGSR
244700070319     c*
244800070320      *   Deve aggiornare la o le Distinte   ?
244900070319     c                   IF        Exec_RB  <> 'S'
245000070319      *
245100070319      *   richiamo in aggiornamento D.B.     ?
245200070319     c                   move      w1ndc1        wNFV
245300070319      *                         ?  =================  ?
245400070319     c                   eval      Aggior_Dist = 'S'
245500070319      *                         ?  =================  ?
245600070319     c                   exsr      CHAIN_Dist
245700070319      *
245800070319     c                   if        Error_Dist   = 'S'  or
245900070319     c                             Rec_Allocato = 'S'  or
246000070319     c                             Found_Dist   = 'N'
246100070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
246200070319     c                   eval      Exec_RB  = 'S'
246300070319     c                   end
246400070319      *
246500070319     c                   endIF
246600070319     c*
246700070319     c* se si tratta di un trasferimento c'è il 2°aggancio alla distinta
246800070319     c                   IF        w1ndc2  > 0  and Exec_RB  <> 'S'
246900070319     c*
247000070319      *   richiamo in aggiornamento D.B.     ?
247100070319     c                   move      w1ndc2        wNFV
247200070319      *                         ?  =================  ?
247300070319     c                   eval      Aggior_Dist = 'S'
247400070319      *                         ?  =================  ?
247500070319     c                   exsr      CHAIN_Dist
247600070319      *
247700070319     c                   if        Error_Dist   = 'S'  or
247800070319     c                             Rec_Allocato = 'S'  or
247900070319     c                             Found_Dist   = 'N'
248000070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
248100070319     c                   eval      Exec_RB  = 'S'
248200070319     c                   end
248300070319      *
248400070319     c                   endIF
248500070319     c*
248600070309     C                   ENDSR
248700070309     C/EJECT
248800070309     C*---------------------------------------------------------------*
248900070326** ErrMsg
249000070326La Filiale non è in gestione                                                   01
249100070326Data errata                                                                    02
249200070326Data antecedente alla data odierna                                             03
249300070326Data distinta superiore alla data odierna dei gg.ammessi                       04
249400070326Data distinta anteriore o superiore alla data odierna dei gg.ammessi           05
249500070326Non esistono distinte aperte nella data scelta                                 06
249600070416Range Distinte non corretto o non ammesso                                      07
249700070417Distinta non appartenente alla data selezionata                                08
249800070326Numero Distinta non valido o inesistente                                       09
249900070326Distinta chiusa o annullata                                                    10
250000070413Distinta ancora in uso da un altro lavoro.                                     11
250100070416                                                                               12
250200070326La Distinta è inesistente o non è stata selezionata                            13
250300070326La Distinta è chiusa o annullata                                               14
250400070326La Distinta è ancora allocata. Non è quindi possibile selezionarla.            15
250500070416                                                                               16
250600070326 Non è stata scelta la DISTINTA per assegnare le Spedizioni                    17
250700070326   Effettuare una scelta mediante le funzioni di ricerca.                      18
250800070326 Occorre selezionare almeno una riga per poter proseguire                      19
250900070326  l'elaborazione.                                                              20
251000070326 Le bolla sopra elencate NON sono state aggiornate perchè ALLOCATE da altro    21
251100070326 Lavoro.  CONTROLLARE e RIESEGUIRE l'operazione                                22
251200070326 Durante l'aggiornamento archivi ci sono stati dei problemi. Non eseguito il   23
251300070326 Lavoro.  CONTROLLARE e RIESEGUIRE l'operazione                                24
251400070326Aggiornamento eseguito correttamente                                           25
251500070326    Aggiornamento  N O N  eseguito                                             26
251600070326Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
251700070326USCIRE SUBITO dal lavoro!                                                      28
251800070413Non si può trasferire sulla stessa Distinta                                    29
251900070326                                                                               30
