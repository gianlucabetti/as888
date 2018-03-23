000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200070316     h dftactgrp(*no) actgrp(*caller)
000300070316
000400070227      * Fidg30R *----------------------------------------------------*
000500070403      *     ?   - ASSEGNAZIONE MANUALE spedizioni da consegnare     ?
000600070403      *--------------------------------------------------------------*
000700070403      * Se il p.o. è in parallelo testo la distinta su ARG altrimenti
000800070403      * Se il p.o. è partito testo la distinta su ARB
000900070227      *--------------------------------------------------------------*
001000100505      *  14           Assegnazione
001100100505      *  15           gestita con PDA
001200100505      *  16           pgm richiamato come MONITOR
001300100505      *  18           Seleziona x Forzare con (F)
001400940224      *
001500940307      *  21           GENERICO OPERAZIONI I/O
001600940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001700070301      *  23           GENERICO Operazioni varie
001800940224      *  30           SFLDSP
001900940224      * N31           SFLCLR
002000940224      *  31           SFLDSPCTL
002100940224      *  32           SFLNXTCHG
002200940224      *  33           SFLEND
002300940224      *  39           OF PRTF
002400070302      *  40 <---> 49  DSPATR SU video
002500940608      *  Specificare l'uso dei singoli indicatori
002600070227      *  50           Emissione della window totale/parziale
002700070227      *  51 <---> 90  ERRORI SU VIDEO
002800150930      *  65 / 67      Errori sulla opzione di scelta 2°SFL
002900940608      *  Specificare l'uso dei singoli indicatori
003000940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003100070829      *  98           ERRORE SPECIALE :
003200940224      *  99           INDIC. GENERALE DI ERRORE
003300940128     F*----------------------------------------------------*
003400070404      *  Output
003500070316     Ffiarg01l  uF   E           K DISK    commit infds(argds)
003600070319     Ffnarb01l  uF   E           K DISK    commit infds(arbds)
003700080609      *
003800070517     Ffidst01l  if   e           k disk    rename(fidst000:fidst01)
003900070517     F                                     infds(dstds)
004000070517      *   solo x allocare il record con il NrelRec  ?
004100070517     Ffidst00F  uf   e             disk    commit
004200070404      *
004300070404      *  Input
004400070404     Ffiarg52j  iF   E           K DISK    rename(fiarg000:fiarg52)
004500070404     Ffnarb70l  iF   E           K DISK    rename(fnarb000:fnarb70)
004600070406     Ffidst08l  if   e           k disk    rename(fidst000:fidst08)
004700070302     Ffnlbl01l  IF   E           K DISK
004800070302     Ftabel00f  IF   E           K DISK
004900070307     Ftntbe01L  IF   E           K DISK
005000070301     Ffiari01l  if   e           k disk
005100150930     FFiar501L  IF   E           K DISK
005200070301     Ffiar601l  if   e           k disk
005300070301     Ffiar901l  if   e           k disk
005400070228     Ffiapd01l  if   e           k disk
005500070228     Ffigrg03l  if   e           k disk
005600070228     Fazorg01l  IF   E           K DISK
005700070829     FfiPRM01l  IF   E           K DISK    usropn
005800070227      *
005900070227     Ffidg30d   CF   E             WORKSTN
006000070227     F                                     SFILE(FDG0S01:S1NRR)
006100070227     F                                     SFILE(FDG0S02:S2NRR)
006200070309     F                                     SFILE(FDG0S03:S3NRR)
006300940201     F                                     INFDS(DSFMT)
006400070305     D*----------------------------------------------------*
006500070829     D Errmsg          S             78    DIM(40) CTDATA PERRCD(1)             MSG DI ERRORE
006600070326     D*----------------------------------------------------*
006700940211     D* Passaggio Parametri
006800940211     D KPJBA         E DS
006900070227     D KPJBUs          s                   like(KPJBU)
007000940211     D*-------------
007100940211     D DSFMT           DS           512
007200940506     D  $TASTO               369    369
007300940211     D  NRG                  370    370
007400940211     D  NCL                  371    371
007500940211     D  SFLNRR               378    379B 0
007600940127     D*-------------
007700940127     D* Reperimento nome PGM
007800940127     D STATUS         SDS           333
007900940127     D  DSPGM            *PROC
008000030113     D*-------------
008100070320     d dstds           ds
008200070320     d  dst_nrr              397    400b 0
008300070320      *
008400070320     d argds           ds
008500070320     d  arg_nrr              397    400b 0
008600070320      *
008700070320     d arbds           ds
008800070320     d  arb_nrr              397    400b 0
008900080606      *
009000070320     D*-------------
009100070316      * Variabili appoggio sempre presenti in tutte le anagrafiche
009200070309$003 D S3NRR           S                   Like(C3rcd)
009300070227$003 D S2NRR           S                   Like(C2rcd)
009400070227$003 D S1NRR           S                   Like(C1rcd)
009500070228$003 D S1pag           S                   Like(C1rcd)
009600070228$003 D S2pag           S                   Like(C2rcd)
009700070309$003 D S3pag           S                   Like(C3rcd)
009800070302$003 D WSfl            S                   Like(C1nrr)
009900030113$003 D Wmax            S                   Like(C1rcd)
010000030113$003 D Wpag            S                   Like(C1rcd)
010100070330$003 D sav_WSfl        S                   Like(WSfl)
010200070330$003 D sav_Wmax        S                   Like(Wmax)
010300070330$003 D sav_Wpag        S                   Like(Wpag)
010400070301$003 D Wpagine         S                   Like(C1rcd)
010500070503$003 D Wrigsfl2        S              3  0 inz(16)
010600070305$003 D NRR_s1          S                   Like(S1nrr)
010700070307$003 D NRR_s2          S                   Like(S2nrr)
010800070309$003 D NRR_s3          S                   Like(S3nrr)
010900070427     D RigPag_SFl1     S              3  0 INZ(18)
011000070427     D RigPag_SFl2     S              3  0 INZ(8)
011100070427     D RigPag_SFl3     S              3  0 INZ(8)
011200070227      *--------------------------
011300070319     D Aggior_Dist     S              1    INZ('N')
011400070319     D Found_Dist      S              1    INZ('N')
011500070319     D Error_Dist      S              1    INZ('N')
011600070319     D Rec_allocato    S              1    INZ('N')
011700070320      *-----
011800070228     D Con_Giro        S              1    INZ(' ')
011900070228     D savARGcgi       S                   Like(argCGI)
012000070320     D Giri_scelti     S              3  0 INZ(0)
012100070320     D sav_s2cgi       S                   Like(s2cgi)
012200070320     D sav_Sxcgi       S                   Like(d1ccgi)
012300070418     D sav_d1cfgs      S                   Like(d1cfgs) inz(0)
012400070418     D sav_d1cddc      S                   Like(d1cddc) inz(0)
012500070831     D sav_d1dtai      S                   Like(d1dtai) inz(0)
012600070831     D sav_d1orai      S                   Like(d1orai) inz(0)
012700071210     D sav_d1dcsr      S                   Like(d1dcsr) inz(0)
012800070326$003 D sav_S1NRR       S                   Like(s1nrr)
012900160506$003 D sav_d1tur1      S                   Like(d1tur1)
013000160506$003 D sav_d1tur2      S                   Like(d1tur2)
013100070427     D sav_pos1        S                   Like(c2pos1)
013200070427     D sav_pos2        S                   Like(c2pos2)
013300070427     D sav_pos3        S                   Like(c2pos3)
013400070427     D sav_pos4        S                   Like(c2pos4)
013500070427     D sav_pos5        S                   Like(c2pos5)
013600070427     D sav_pos6        S                   Like(c2pos6)
013700070514     D C2_pos1         S                   Like(c2pos1)
013800070514     D C2_pos2         S                   Like(c2pos2)
013900070514     D C2_pos3         S                   Like(c2pos3)
014000070514     D C2_pos4         S                   Like(c2pos4)
014100070514     D C2_pos5         S                   Like(c2pos5)
014200070514     D C2_pos6         S                   Like(c2pos6)
014300071029     D savPOSnf        S                   Like(fdgNFpos)
014400070607     D vedi_video      S              1
014500070607     D salta_video1    S              1    inz('N')
014600070607     D salva_d1sce     S                   like(d1sce)
014700070427     D posizionamento  S              1
014800070515     D posizionato     S              1
014900070511     D cerca_stringa   S              1
015000070514     D Wlen            S              3i 0
015100070514     D Wpos            S              3i 0
015200070515$003 D C2RCD_pos       S                   Like(C2rcd)
015300101129      *--------------------------
015400101129     d WDcodInfo       s             20a
015500101129     d dataiso         s               d   datfmt(*iso)
015600101129     d dataeur         s               d   datfmt(*eur)
015700101129     d Wdatabol        s              8s 0
015800070320      *--------------------------
015900070228     d wpes            s                   like(dstpkl)
016000070228     d wvol            s                   like(dstvlu)
016100070228     D totSPE_cgi      S                   Like(s1tNSP)
016200070228     D totPKL_cgi      S                   Like(s1tPKL)
016300070228     D totVLU_cgi      S                   Like(s1tVLU)
016400070228     D totCOL_cgi      S                   Like(s1tCOL)
016500070228     D totSPE_s1       S                   Like(s1tNSP)
016600070228     D totPKL_s1       S                   Like(s1tPKL)
016700070228     D totVLU_s1       S                   Like(s1tVLU)
016800070228     D totCOL_s1       S                   Like(s1tCOL)
016900070302     D totSPE_s2       S                   Like(s1tNSP)
017000070302     D totPKL_s2       S                   Like(s1tPKL)
017100070302     D totVLU_s2       S                   Like(s1tVLU)
017200070302     D totCOL_s2       S                   Like(s1tCOL)
017300070320      *--------------------------
017400070301     d wNFV            s                   like(dstNFV)
017500070301     d wNPG            s                   like(dstNPG)
017600070316     d wFGS            s                   like(dstFGS)
017700070321     d wCGI            s                   like(argCGI)
017800070321     d wDDC            s                   like(h1cddc)
017900070301     d kTIP            s                   like(ARItip)
018000070301     d kTRC            s                   like(AR6trc)
018100070307     d kCOD1           s                   like(TBEcod)
018200070307     d kKEY1           s                   like(TBEke1)
018300070404     d wfgs_ifp        s                   like(ARBifp)
018400070606     d w1sce           s                   like(d1sce)
018500070606     d w1NO_dis        s              6s 0
018600070829     d h1dtaI          s                   like(d1dtaI)
018700071210     d h1dcsr          s                   like(d1dcsr)
018800080606     d PDAconsegne     s              1    inz(' ')
018900080606     d PGM_Chiamante   s             10
019000101130     d exec_opzINFO    s              1    inz(' ')
019100100505     d in_attesa       s              1    inz(' ')
019200100505     D Premuto_F02     s              1
019300160317     D Sped_DPD        s              1
019400160317     D GIRO_vd_Pacco   s             10
019500160318     D GIRO_vedi_PACCO_si...
019600160318     d                 s              1    inz(' ')
019700150930     D inviato_contro_alert...
019800150930     d                 s              1    inz('N')
019900070607      *
020000070611      *?  ABILITATO o Non ABILITATO
020100070611     d Sel_xGiro       s              1a   inz('S')
020200070228      *--------------------------
020300070305      * Per impostare in testata la Distinta
020400070305     D Distintalfa     DS
020500070305     D  num_ndc                1      7
020600070305     D  ndc_a                  1      7    DIM(7)
020700070305      *--------------------------
020800070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
020900070305     D WLBDAT          DS                  INZ
021000070305     D  G02DAT                 1      8  0
021100070227     D  G02INV                 9     16  0
021200070227     D  G02ERR                17     17
021300070227     D  G02TGI                18     22  0
021400070307      *
021500070307     D DATPAR          DS
021600070307     D  GIODAT                 1      8  0
021700070307     D  GIOINV                 9     16  0
021800070307     D  GIOTGI                17     21  0
021900070307      *
022000070307     d Wdata8          DS
022100070307     d  dadata                 1      8  0
022200070307     d  adata                  9     16  0
022300070307     d  GioLav                17     21  0
022400070924      *
022500070307      *--------------------------------
022600070307      * DS per richiamo pgm FNLR36R
022700070307     D DSLR36          DS
022800070307     D  P36AAS                14     17  0
022900070307     D  P36LNP                18     20  0
023000070307     D  P36NRS                21     22  0
023100070307     D  P36NSP                23     29  0
023200070307     D  P36F03                30     30
023300070307     D  P36FLG                31     31
023400070320      *--------------------------
023500150930     D Dar5emd       E DS
023600070227     d AZUTEds       e ds                  extname(AZUTE00F)
023700070227     d DDatiUte      e ds
023800070320     d* Ds per dati organigramma
023900070228     D OG150         E DS                  INZ
024000070228     D OG146         E DS                  INZ
024100070829     D OG147         E DS                  INZ
024200080606     D OG148         E DS                  INZ
024300070320     D DgrgFLO       E DS                  INZ
024400080606     D DdstFLR       E DS                  INZ
024500080627     d fidg45ds      e ds
024600080606     d fidg44ds      e ds
024700080609     d fidg43ds      e ds
024800080606     d fidg30ds      e ds
024900141029     d fnlry09ds2    e ds
025000141113     d fnlr98ds2     e ds
025100150515     d TRULVPODS     e ds
025200150515     d   sk_PDA               16    765    inz   dim(250)
025300140714      *--------------------------
025400140711      * per gestire evento NIC su Togli
025500140711     d fnlrq8ds      e ds
025600140714     d sav_arbndc      s                   like(ARBndc)
025700140714     d sav_arbddc      s                   like(ARBddc)
025800140714     d sav_arbpdc      s                   like(ARBpdc)
025900140714      *--------------------------
026000070320      *------
026100070404     d $FV6            s              6P 0 dim(600)                             F.V.  x FIDG31R
026200070404      *------
026300160317     D DNSD          E DS
026400160317     D DDGD          E DS
026500070301     D DS3A          E DS
026600100505     D dARGFLO       E DS
026700100505     d DTAISO          s               D
026800100505     D DS7r          E DS
026900160429     D DS03          E DS
027000100505     D s7r             S              2    DIM(100)                             CODICE TABELLA 7R
027100100518     D s7rRC           S              2    DIM(100)                             CODICE TABELLA 7R
027200070301     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
027300070301     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
027400070301     D DS1P          E DS
027500070301     D C1P             S              1    DIM(50)                              CODICI TABELLA 1P
027600070301     D D1P             S             25    DIM(50)                              DESCR. TABELLA 1P
027700070302     D YEURDS        E DS                  EXTNAME(YEURCODS)
027800070320      *--------------------------
027900070320     d* Parametri x Controllo profilo utenti
028000070320     d TIBS34ds      e ds
028100070227      *--------------------------
028200070227     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
028300070227     D DSLV50        E DS                  EXTNAME(FNLV50DS)
028400070227      *--------------------------
028500070227     D* DS PER FNLV24R - ricerca autotrasp.
028600070227     D fnlv24ds      E DS
028700070227      *--------------------------
028800070227     D* DS PER - ricerca giri
028900070227     D fidg09ds      E DS
029000070829      *--------------------------
029100070829     D* DS PER -
029200070829     D fidg17ds      E DS
029300070227      *--------------------------
029400070302     D* DS PER - calcolo pesi volumi colli
029500070302     D fidg27ds      E DS
029600070302      *--------------------------
029700070319     D* DS PER - aggiornamento di ARB
029800070319     D fidg23ds      E DS
029900070320      *--------------------------
030000070319     D* DS PER - aggiornamento distinte
030100070517     D******Fidg28ds      E DS
030200070320      *--------------------------
030300070517     D* DS PER - aggiornamento distinte
030400070517     D Fidg37ds      E DS
030500070517      *--------------------------
030600070320     D* DS PER FIDG04R - ricerca numero distinta
030700070320     D fidg31ds      E DS
030800070320     D*-------------
030900070328     D* DS PER TNSD99R - affidabilità consegna
031000070328     D tnsd99ds      E DS
031100070328     D*-------------
031200070320     d trul82ds      e ds
031300070319      *--------------------------
031400070228     D* DS PER - Bolle valide in consegna
031500070228     D Fidg16ds      E DS
031600071008      *--------------------------
031700071008     D* DS PER - Orm x particolarità RC
031800071008     D Fnlr99ds      E DS
031900070320      *--------------------------
032000070301     D fnarbdsav     e ds                  extname(fnarb00f)
032100070302     d                                     Prefix(sv_)
032200070320      *------
032300070228     D fnarbds       e ds                  extname(fnarb00f)
032400070228     D fiargds       e ds                  extname(fiarg00f)
032500070227      *--------------------------
032600070320      *?  COSTANTI                                      ?
032700070320      *--------------------------
032800940506     D* Tasti di funzione
032900940506     D F01             C                   CONST(X'31')
033000940506     D F02             C                   CONST(X'32')
033100940506     D F03             C                   CONST(X'33')
033200940506     D F04             C                   CONST(X'34')
033300940506     D F05             C                   CONST(X'35')
033400940506     D F06             C                   CONST(X'36')
033500940506     D F07             C                   CONST(X'37')
033600940506     D F08             C                   CONST(X'38')
033700940506     D F09             C                   CONST(X'39')
033800940506     D F10             C                   CONST(X'3A')
033900940506     D F11             C                   CONST(X'3B')
034000070315     D F12             C                   CONST(X'3C')
034100940506     D F13             C                   CONST(X'B1')
034200940506     D F14             C                   CONST(X'B2')
034300940506     D F15             C                   CONST(X'B3')
034400940506     D F16             C                   CONST(X'B4')
034500940506     D F17             C                   CONST(X'B5')
034600940506     D F18             C                   CONST(X'B6')
034700940506     D F19             C                   CONST(X'B7')
034800940506     D F20             C                   CONST(X'B8')
034900940506     D F21             C                   CONST(X'B9')
035000940506     D F22             C                   CONST(X'BA')
035100940506     D F23             C                   CONST(X'BB')
035200940506     D F24             C                   CONST(X'BC')
035300940506     D ENTER           C                   CONST(X'F1')
035400940506     D ROLDWN          C                   CONST(X'F4')
035500940506     D ROLLUP          C                   CONST(X'F5')
035600070301     d digits          c                   '0123456789'
035700070302     d Alla_Dist       c                   'alla dist.:'
035800070326     d OPZ_1           c                   'ASSEGNA    '
035900070326     d OPZ_2           c                   'TOGLIE     '
036000070326     d OPZ_3           c                   'TRASFERISCE'
036100070306     d Alla            c                   'alla '
036200070306     d Dalla           c                   'dalla'
036300070426     d FUN_1           c                   'F1=Sel.tutto'
036400070305     d FUN_3           c                   'F3=Fine'
036500070419     d FUN_6           c                   'F6=Conferma Giro NO DETT. Bolle'
036600070419     d FUN_Enter       c                   'Enter=Con Dettaglio Bolle'
036700070309     d FUN_9_Drop      c                   'F9=Altri Dati'
036800070419
036900070606     D Scegliere       C                   '1'
037000070606      *
037100070531     D Assegnare       C                   '1'
037200070531     D Togliere        C                   '2'
037300070531     D Trasferire      C                   '3'
037400070419      **********************************************************************
037500070419      * Constants
037600070419      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
037700070419      **********************************************************************
037800070427$xxx D Ord_GIRODEST    C                   1
037900070427$xxx D Ord_DESTINAT    C                   2
038000070427$xxx D Ord_INDIRIZZO   C                   3
038100070427$xxx D Ord_LOCALITA    C                   4
038200070427$xxx D Ord_VOLUME      C                   5
038300070427$xxx D Ord_PESO        C                   6
038400070427      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
038500070427     D MaxKey          C                   9
038600070419     D Ascendente      C                   1
038700070419     D Discendente     C                   2
038800070419     D Carattere       C                   6
038900070419     D Put             C                   1
039000070419     D EndPut          C                   2
039100070419     D Get             C                   3
039200070419     D Numerico        C                   8
039300070419      **********************************************************************
039400070419      * Campi utili:
039500070419      *     RRN1       - Numero relativo di record del Subfile
039600070419      *     SizeList   - Dimensione della lista
039700070419      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
039800070419      **********************************************************************
039900070419     D Rrn1            S              5I 0
040000070419     D NotUsed         S             16A
040100070419     D ReturnSize      S              9B 0
040200070419     D SizeList        S              9B 0
040300070419     D RrnLast         S              5I 0
040400070427     D WrkSort         S              1  0 inz(0)
040500070427     D WindSort        S              3I 0 inz(0)
040600070419      **********************************************************************
040700070419      * Data Structures
040800070419      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
040900070419      *     QLGSCB     - The sort request block for the QLGSORT API
041000070419      *     QLGSCB00   - The sort request block for the QLGSRTIO API
041100070419      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
041200070419      *     QUSEC      - Error structure for the QLGSORT API
041300070419      **********************************************************************
041400070419     D SflRcd          DS
041500070419     D  S2CGI
041600070420     D  S2CRSD
041700070420     D  S2CIND
041800070420     D  S2CLOD
041900070420     D  S2CPKF
042000070420     D  S2CVLF
042100070420     D  S2CNCL
042200070511     d  H2SUBST
042300080610     D  s2cCOK
042400070419     D  S2CAAS
042500070419     D  S2CLNP
042600070419     D  S2CNRS
042700070419     D  S2CNSP
042800070419     D  S2SCE
042900070427     D  S2CPRD
043000070427     D  S2CCAD
043100070419     D  S2CLNA
043200070419     D  S2CGMC
043300070419     D  S2CGC1
043400070419     D  S2CDAC
043500070419     D  S2CCBO
043600070419     D  S2CTIC
043700070419     D  S2CVCA
043800070419     D  S2CCAS
043900070419     D  S2CHCR
044000070419     D  S2CGC2
044100070419     D  S2DTC2
044200070419     D  S2CVAS
044300070419     D  S2CIMA
044400070419     D  S2DANO
044500070419     D  S2DTC1
044600070419     D  S2CRSM
044700070419     D  S2CTSP
044800070419     D  S2CZNC
044900070427     D  S2PKF
045000100505     D  S2Patt
045100100505     D  S2Forza
045200100505     D  h2dcr
045300100505     D  h2gma
045400101126     D  h2notASS
045500101129     D  h2MGS
045600101130     D  h2in45
045700110804     D  h2xco
045800130424     D  h2att
045900150930     D  h2Calert
046000160429     D  h2forzaznc
046100160429     D  s2turno
046200070419     D  Selected                      1A
046300070419
046400070419      /COPY QSYSINC/QRPGLESRC,QLGSORT
046500070419     D QLGKL                         16    DIM(MaxKey)
046600070419     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
046700070419     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
046800070419     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
046900070419     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
047000070419
047100070419      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
047200070419      /COPY QSYSINC/QRPGLESRC,QUSEC
047300940127     C*----------------------------------------------------*
047400070419      *?     MAIN LINE PROGRAM                             ?
047500940127     C*----------------------------------------------------*
047600940223     C* inizializzazione variabili
047700940223     C                   EXSR      INZVAR
047800940223     C*
047900940223     C     $FINE         DOWEQ     *OFF
048000070413     C*
048100070413     C* 1° Video
048200070413     C     $GEST         CASEQ     'D1'          GESD1                          Video Parametri
048300070413     C*
048400070413     C* Lista GIRI se scelta Assegnazione e non impostato il GIRO fra i parametri d'ingresso
048500070413     C     $GEST         CASEQ     'S1'          GESS1                          Lista dei GIRI
048600070413     C*
048700070413     C* Lista di dettaglio delle Bolle x selezione e guida aggiornamento Data Base
048800070413     C     $GEST         CASEQ     'S2'          GESS2                          Lista delle Bolle
048900070413     C*
049000070413     C* Finestra dei Totali riepilogativa prima dell'aggiornamento del Data Base
049100070413     C     $GEST         CASEQ     'W1'          GESW1                          Riepilogo Totali
049200070413     C*
049300070413     C* Durante l'aggiornamento del Data Base scrivo eventuali errori Bolle da visualizzare
049400070413     C*  su questo ultimo SFL.
049500070413     C     $GEST         CASEQ     'S3'          GESS3                          Aggiornamento D.B. e
049600070413      *                                                                         errori su bolle
049700940117     C                   END
049800940117     C                   END
049900940325     C* fine programma
050000940325     C                   SETON                                            LR
050100030113     C************************************************************
050200070320      *?  INIZIALIZZAZIONE VARIABILI                       ?
050300030113     C************************************************************
050400030113     C     INZVAR        BEGSR
050500030113     C*
050600030113     C* Variabili per gestione videate
050700030113     C                   MOVE      *OFF          $FINE
050800070227     C                   MOVE      *OFF          $INZW1
050900070227     C                   MOVE      *OFF          $INZS1
051000070227     C                   MOVE      *OFF          $INZS2
051100070306     C                   MOVE      *OFF          $VISS2
051200070309     C                   MOVE      *OFF          $INZS3
051300030113     C                   MOVE      *OFF          $EFILE
051400030113     C                   MOVE      *OFF          $ESCI
051500030113     C                   MOVE      *OFF          $RCDOK
051600070227     C* Emissione primo video
051700070227     C                   MOVE      'D1'          $GEST
051800070227     C                   MOVE      *ON           $INZD1
051900030113     C* Variabili appoggio
052000030114     C                   Z-ADD     1             WPAG
052100030113     c*
052200030113     C                   ENDSR
052300070227     C************************************************************
052400080606      *?    se presente in Distinta Fase CUS occorre FORZARE     ?
052500080606     C************************************************************
052600080626     C     CTL_se_FORZAREbegSR
052700080606     C*
052800080630     C* Solo se la filiale è partita con il PDA in Consegna
052900080630     c                   if        §OGPDACON <> *blank
053000080630     C*
053100080606     C*   Controlla presenza Fase "CUS"
053200080630     c                   If        h1ndc_meno >0
053300080626     c                   clear                   torna_a_D1        1
053400080606     c                   clear                   fidg44ds
053500080626     c                   z-add     D1CFGS        CO44FGS
053600120209     c*********          z-add     h1cddc        CO44DDC
053700120209     c                   z-add     h1ddc_meno    CO44DDC
053800080626     c                   z-add     h1ndc_meno    CO44NDC
053900080606     c                   eval      CO44wnd = 'S'
054000080606     c                   eval      kpjbus = kpjbu
054100080606     c                   eval      kpjbu =  fidg44ds
054200080606     c                   call      'FIDG44R'
054300080606     c                   parm                    kpjba
054400080606     c                   eval      fidg44ds = kpjbu
054500080606     c                   eval      kpjbu = kpjbus
054600080606      *
054700080606      * esce dal programma se non si è FORZATO
054800080606     c                   if        CO44FCUS = 'N'
054900080626      * se richiamato da MONITOR deve uscire direttamente dal pgm
055000080626     c                   if        PGM_Chiamante = 'MONITOR'
055100080626     c                   eval      torna_a_D1 = 'S'
055200080606     C                   MOVE      *ON           $FINE
055300080626     c                   rolbk
055400080626     c                   else
055500080626      * deve tornare al video precedente come F12
055600080626     c                   eval      torna_a_D1 = 'S'
055700080701     c                   leaveSR
055800080626     c                   end
055900080606     c                   endif
056000080606     c*
056100080630     c                   end
056200080630     c*
056300080630     c* se trasferisce
056400080630     C*   Controlla presenza Fase "CUS"
056500080630     c                   If        h1ndc_piu >0
056600080630     c                   clear                   torna_a_D1        1
056700080630     c                   clear                   fidg44ds
056800080630     c                   z-add     D1CFGS        CO44FGS
056900120209     c*******            z-add     h1cddc        CO44DDC
057000120209     c                   z-add     h1ddc_piu     CO44DDC
057100080630     c                   z-add     h1ndc_piu     CO44NDC
057200080630     c                   eval      CO44wnd = 'S'
057300080630     c                   eval      kpjbus = kpjbu
057400080630     c                   eval      kpjbu =  fidg44ds
057500080630     c                   call      'FIDG44R'
057600080630     c                   parm                    kpjba
057700080630     c                   eval      fidg44ds = kpjbu
057800080630     c                   eval      kpjbu = kpjbus
057900080630      *
058000080630      * esce dal programma se non si è FORZATO
058100080630     c                   if        CO44FCUS = 'N'
058200080630      * se richiamato da MONITOR deve uscire direttamente dal pgm
058300080630     c                   if        PGM_Chiamante = 'MONITOR'
058400080630     c                   eval      torna_a_D1 = 'S'
058500080630     C                   MOVE      *ON           $FINE
058600080630     c                   rolbk
058700080630     c                   else
058800080630      * deve tornare al video precedente come F12
058900080630     c                   eval      torna_a_D1 = 'S'
059000080701     c                   leaveSR
059100080630     c                   end
059200080630     c                   endif
059300080630     c*
059400080630     c                   end
059500080630     c*
059600080630     c                   end
059700080630     c*
059800080606     C                   endSR
059900080606     C************************************************************
060000070320      *?  Emissione primo video di SCELTA                        ?
060100070227     C************************************************************
060200070227     C     GESD1         BEGSR
060300070227     C*
060400070227     C* inizializzazione videata
060500070227     C     $INZD1        IFEQ      *ON
060600070227     C                   EXSR      INZD1
060700070227     C                   MOVE      *OFF          $INZD1
060800070227     C                   ENDIF
060900070227     C*
061000070228     c     Video_1       tag
061100070302     C*  Emissione Video
061200070607     c                   if        salta_video1 = 'N'
061300100505      *
061400100505      *    ind.10 x campi a VIDEO da condizionare
061500070228      * ?            *-----------------------*
061600070227     C                   EXFMT     FDG0D01
061700070228      * ?            *-----------------------*
061800071029     c                   clear                   savPOSnf
061900070607     C                   Else
062000070607     c                   eval      salta_video1 = 'N'
062100070607     c                   end
062200070326     c                   setoff                                       99
062300070305     C                   movel     'D1'          sav$GEST
062400070227     C* Selezioni
0625000702271    C                   SELECT
062600070413     C*  Cambio filiale
062700070413     C                   WHEN      $TASTO   =   F18
062800070418     C                   SETON                                        07
062900140924     C*  Disposizioni consegna
063000140924     C                   WHEN      $TASTO   =   F19
063100141029     c                   clear                   fnlry09ds2
063200140924     c                   eval      ilry09tch = 'E'
063300141013     c                   exsr      verdispo
063400141029     c                   if        OLRY09ESal = '1'
063500141002     c                   seton                                        97
063600141002     c                   else
063700141002     c                   setoff                                       97
063800141002     c                   endif
063900070227     C* F3=Fine
064000070227     C                   WHEN      $TASTO   =   F03
064100070227     C                   EXSR      F03_exit
064200070319     c                   rolbk
064300070227      *
064400070227     C* F8=Interrogazione Anagr.AUTISTI
064500070227     C                   WHEN      $TASTO   =   F08
064600070227     C                   EXSR      F08_Interr
064700070227      *
0648000702271O   C                   OTHER
064900070227     C* CONTROLLO DATI
065000070227     C                   EXSR      CTRD1
065100080626      *
065200080626     C* CONTROLLA SE LA DISTINTA con PDA ha già la FASE "CUS"
065300080626     c                   EXSR      CTL_se_FORZARE
065400080626     c                   if        torna_a_D1 = 'S'
065500080626     C                   MOVE      *ON           $INZD1
065600080626     c                   goto      Fine_gesd1
065700080626     c                   end
065800080626      *
0659000702271-   C                   ENDSL
066000070227     C*
066100080606      * ? Se richiamato da MONITOR con PDA
066200080606      * ?  e se la ricerca distinta ha chiuso con F3 o F12
066300080606     c                   if        PGM_Chiamante = 'MONITOR' and
066400080606     c                             (fdgTFU ='03' or fdgTFU ='12')
066500080606     C                   EXSR      F03_exit
066600080606     c                   goto      Fine_gesd1
066700080606     c                   end
066800080606     C*
066900070418     C* Per errori o decodifiche o tasto di cambio P.O.
067000070418     C                   if        $TASTO   =   F18 or *in99
067100070418     c                   goto      Video_1
067200070418     c                   end
067300070228     C*
067400070228     C* Se non ci sono errori passa alle operazioni da esguire
067500080826     C*  il 14=Acceso solo se si sta Assegnando
067600080826     c                   setoff                                       14
067700070228     c                   select
067800070228      *
067900070531     c                   when      d1SCE = Assegnare
068000070228      * ? * Emissione Lista con i Giri e i totali spedizioni *
068100070228     c                   exsr      Assegna
068200070228      *
068300070607     c                   when      d1SCE = Togliere   and h1ndc_meno >0
068400070228      * ? * Emissione Lista delle Spedizioni da selezionare  *
068500070228     c                   exsr      Toglie
068600070228      *
068700070607     c                   when      d1SCE = Trasferire and h1ndc_meno >0
068800070228      * ? * Emissione Lista delle Spedizioni da selezionare  *
068900070228     c                   exsr      Trasferisce
069000070228      *
069100070228     C                   EndSL
069200070228     C*
069300080606     C     Fine_GESD1    ENDSR
069400070306     C/EJECT
069500070306     C************************************************************
069600070306     C*? Assegna Distinta
069700070306     C************************************************************
069800070306     C     Assegna       BEGSR
069900070306     C*
070000080826     C* il 14 Acceso identifica Assegnazione per gestire campi su DSPF
070100070306     C*  Emissione SFL con elenco dei Giri da assegnare
070200080826     c                   seton                                        14
070300070306     c                   eval      $GEST ='S1'
070400070306     C                   MOVE      *ON           $INZS1
070500070306     C                   MOVE      *ON           $INZS2
070600070306     C                   MOVE      *ON           $INZW1
070700070309     C                   MOVE      *ON           $INZS3
070800070319     C*
070900070306     C                   ENDSR
071000070306     C************************************************************
071100070306     C*? Toglie a Distinta
071200070306     C************************************************************
071300070306     C     Toglie        BEGSR
071400070306     C*
071500070306     C*  Emissione SFL con elenco delle bolle da togliere alla distinta
071600070306     c                   eval      $GEST ='S2'
071700070306     C                   MOVE      *ON           $INZS2
071800070306     C                   MOVE      *ON           $INZW1
071900070309     C                   MOVE      *ON           $INZS3
072000070319     C*
072100070306     C                   ENDSR
072200070306     C************************************************************
072300070306     C*? Trasferisce da Distinta a Distinta
072400070306     C************************************************************
072500070306     C     Trasferisce   BEGSR
072600070306     C*
072700070306     C*  Emissione SFL con elenco delle bolle da trasferire ad altra distinta
072800070306     c                   eval      $GEST ='S2'
072900070306     C                   MOVE      *ON           $INZS2
073000070306     C                   MOVE      *ON           $INZW1
073100070309     C                   MOVE      *ON           $INZS3
073200070319     C*
073300070306     C                   ENDSR
073400070227     C/EJECT
073500070227     C************************************************************
073600070320      *?  INIZIALIZZAZIONE Primo video                           ?
073700070227     C************************************************************
073800070227     C     INZD1         BEGSR
073900070227     C*
074000070227     C* Pulizia campi e indicatori
074100070308     c                   exsr      INZ_indERR
074200070227     C* Pulizia formato record
074300070227     c                   clear                   FDG0D01
074400070227     C* campi titolo
074500070302     C                   MOVEL     dsPGM         D1CPGM
074600070227     C                   MOVEL     knsif         D1nsif
074700070227     C                   MOVEL     knmus         D1nmus
074800070227     C                   MOVEL     RSUT          D1crsu
074900070227     C* campi video
075000070227     C* Il p.o.deve essere impostato in base al profilo
075100070413     C     dutlpo        IFEQ      '2'
075200070227     C                   MOVEL     dutpou        D1CFGS                         *FIL.GESTIONE
075300070413     C                   ELSE
075400070413     c                   seton                                        08
075500070413     C                   MOVEL     SIMFEL        D1CFGS
075600070413     C                   END
075700080606      *
075800070227     C                   z-add     UDATE8        D1cDDC
075900080606      *
076000150515     C                   clear                   V1Cp88            1
076100150515     c* ricerca P.O. abilitati da tabella VPO per step di test
076200150515     c                   clear                   trulvpods
076300150515     c                   eval      ivpoke1 = 'SQA'
076400150515     c                   call      'TRULVPOR'
076500150515     c                   parm                    trulvpods
076600150515     c                   move      '999'         fgsa              3
076700150515     c     fgsa          lookup    sk_pda                                 37
076800150515     c  n37              move      dutpou        fgsa
076900150515     c  n37fgsa          lookup    sk_pda                                 37
077000150515     c                   if        *in37
077100150515     c                   eval      v1cp88 = 'S'
077200150515     c                   else
077300150515     C                   CLEAR                   V1Cp88
077400150515     c                   end
077500140929     C*
077600140929      * verifica presenza disposizioni web
077700141029     c                   clear                   fnlry09ds2
077800140929     c                   eval      ilry09tch = 'C'
077900141013     c                   exsr      verdispo
078000141029     c                   if        OLRY09ESal = '1' and
078100140929     c                             OLRY09ERR  = *blank
078200140929     c                   seton                                        97
078300140929     c                   endif
078400080606      * Se richiamato dal MONITOR
078500080606     c                   if        PGM_chiamante = 'MONITOR'
078600080606      *
078700080606      *   imposta l'FGS se passata
078800080606     c                   if          G30FGS >0
078900080606     C                   z-add     G30fgs        D1CFGS
079000080606     c                   end
079100080606      *
079200080606      *   imposta la DATA se passata
079300080606     c                   if          G30DFV >0
079400080606     C                   Z-ADD     G30dfv        G02inv
079500080606     C                   MOVEL     '3'           G02ERR
079600080606     C                   CALL      'XSRDA8'
079700080606     C                   PARM                    WLBDAT
079800080606     C                   Z-ADD     G02DAT        D1cDDC
079900080606     c                   end
080000080606      *
080100080606     c                   end
080200070625      *
080300160429     c                   setoff                                       111510
080400160511     C*
080500160511     c                   if        sav_d1cfgs > 0
080600160511     C                   z-add     sav_d1cfgs    D1cfgs
080700160511     c                   end
080800071210      *
080900070625     C*  Controlla Con il P.O. la data abilitazione
081000070625     C     d1cFGS        CHAIN     AZORG01l
081100071210      *
081200071210b    C                   IF        %found(azorg01l)
081300070625     C                   MOVEL     ORGdf0        og150
081400070829     C                   MOVEL     ORGde7        og147
081500080606     c                   movel     orgde8        og148
081600080606      *
081700080606     c* in test o attivo x PDA   ("S", "T")
081800080606     c                   clear                   PDAconsegne
081900080606     c                   if        §OGPDACON <> *blank
082000080606     c                   eval        PDAconsegne = §OGPDACON
082100080606     c                   seton                                        15
082200080606     c                   end
082300071210      *
082400070625     c* p.o. con piking messaggerie
082500070625     c                   if        §ogpkm='S'
082600070625     c                   seton                                        11
082700070625     c                   endif
082800160429     c                   if        §ogturno='S'
082900160429     c                   seton                                        10
083000160429     c                   endif
083100160511      *
083200160511     c* imposto il turno unico di dft
083300160511     c                   if        sav_d1tur1 = ' ' and
083400160511     c                             sav_d1tur2 = ' '
083500160511     c                   if        §ogturno='S'
083600160511     c                   movel     '1'           d1tur1
083700160511     c                   movel     '2'           d1tur2
083800160511     c                   else
083900160511     c                   movel     'U'           d1tur1
084000160511     c                   movel     ' '           d1tur2
084100160511     c                   end
084200160511     c                   else
084300160511     c                   movel     sav_d1tur1    d1tur1
084400160511     c                   movel     sav_d1tur2    d1tur2
084500160511     c                   end
084600080606      *
084700080606      * Se richiamato dal MONITOR
084800080606      *   salta tutte le altre impostazioni della precedente chiamata
084900080606     c                   if        PGM_chiamante = 'MONITOR'
085000080606     c                   move      Togliere      d1SCE
085100080606     c                   GoTo      END_INZ1
085200080606     c                   endif
085300080606      *
085400080606      *
085500071210     c                   open      fiPRM01l
085600071210     c                   z-add     d1cfgs        k_PRMfgs
085700071210     c                   movel(p)  'FIDG17R'     k_PRMpgm
085800071210     c     k_PRM         chain     fiPRM01l
085900071210      *
086000071210     c                   if        %Found(fiPRM01l)
086100071210     c                   movel     prmPAR        fidg17ds
086200071210     c                   endIF
086300071210      *
086400070829     c* Data limite Abbinamento
086500071210a    c                   if        §ogDOada='S'
086600070829     c                   seton                                        12
086700071210      *
0868000712100    c                   if        sav_d1dtaI = 0
086900071210      *
087000071210 1   c                   if        %Found(fiPRM01l)
087100070830     c                   movel     D17FPPI       d1tUSC
087200160511     c                   if        d17tur1 <> *blank
087300160511     c                              or
087400160511     c                             d17tur2 <> *blank
087500160429     c                   movel     d17tur1       d1tur1
087600160429     c                   movel     d17tur2       d1tur2
087700160511     c                   end
087800160506     c                   eval      sav_d1tur1 = d1tur1
087900160506     c                   eval      sav_d1tur2 = d1tur2
088000071210      *
088100071210 2   c                   if        d17DTAI >0
088200070829     C                   Z-ADD     d17DTAi       G02inv
088300070829     C                   MOVEL     '3'           G02ERR
088400070829     C                   CALL      'XSRDA8'
088500070829     C                   PARM                    WLBDAT
088600070829     C                   Z-ADD     G02DAT        d1DTAi
088700070831     C                   Z-ADD     G02DAT        sav_d1DTAi
0888000712102-   c                   end
088900071210      *
089000070829     c                   if        d17ORAi >0
089100070829     C                   Z-ADD     d17ORAi       d1ORAi
089200070829     c                   end
089300071210      *
089400070924      * deve impostare la data dell'ultimo lancio se c'è... altrimenti UDATE
089500070927     C                   Z-ADD     d17dFVI       G02inv
089600070927     C                   MOVEL     '3'           G02ERR
089700070927     C                   CALL      'XSRDA8'
089800070927     C                   PARM                    WLBDAT
089900070927     C                   Z-ADD     G02DAT        d1cDDC
0900000712101-   c                   endIF
090100071210      *
090200071210      *
0903000712100x   c                   else
090400070831     c                   eval      D1dtaI = sav_d1dtaI
090500070831     c                   eval      D1oraI = sav_d1oraI
0906000712100-   c                   endIF
090700071210      *
090800071210a-   c                   endif
090900071210      *
0910000712100    c                   if        sav_d1dcsr = 0
091100071210      *
091200071210     c                   if        %Found(fiPRM01l) and d17LDCRI >0
091300071210     C                   Z-ADD     d17LDCRI      G02inv
091400071210     C                   MOVEL     '3'           G02ERR
091500071210     C                   CALL      'XSRDA8'
091600071210     C                   PARM                    WLBDAT
091700071210     C                   Z-ADD     G02DAT        d1Dcsr
091800071210     C                   Z-ADD     G02DAT        sav_d1Dcsr
091900071210     c                   end
092000071210      *
0921000712100x   c                   else
092200071210     c                   eval      D1dcsr = sav_d1dcsr
0923000712100-   c                   endIF
092400071210      *
092500071210     c                   close     fiPRM01l
092600071210      *
092700071210b-   c                   endif
092800070924      * oppure.....
092900070418     C*  Se precedentemente impostati devono rimanere quelli
093000070418     c                   if        sav_d1cddc > 0
093100070418     C                   z-add     sav_d1cddc    D1cDDC
093200070418     c                   end
093300080606     C*
093400100505     c* Visualizzare Bolle con Particolarità di Attesa
093500100505     C                   MOVEL     'N'           D1pATT
093600160429     C                   MOVEL     'N'           D1stur
093700070418     C*
093800080606     C     END_INZ1      ENDSR
093900940127     C************************************************************
094000070320      *? Controlli primo video di SCELTA Operazione da eseguire  ?
094100940127     C************************************************************
094200070227     C     CTRD1         BEGSR
094300030113     C*
094400070606     C* Ripercorre i controlli
094500070606     c     ini_CTRD1     tag
094600070606     C*
094700070227     C*  Spegne indicatori di errore
094800070308     c                   exsr      INZ_indERR
094900140929     c                   setoff                                       9997
095000070228     C*  campi di Work
095100070228     c                   clear                   con_giro
095200070308     c                   clear                   h1cddc
095300070319     c                   clear                   h1ndc_piu
095400160318     c                   clear                   h1pdd_PIU
095500120209     c                   clear                   h1ddc_piu
095600070319     c                   clear                   h1ndc_meno
095700160318     c                   clear                   h1pdd_meno
095800120209     c                   clear                   h1ddc_meno
095900080630     c                   clear                   h1PDA_piu
096000160506     c                   clear                   h1tur_piu
096100080630     c                   clear                   h1PDA_meno
096200070319     c                   clear                   h1fpp_meno
096300070319     c                   clear                   h1fpp_piu
096400160506     c                   clear                   h1tur_meno
096500070321     c                   clear                   wfgs
096600070321     c                   clear                   wddc
096700070321     c                   clear                   wcgi
096800070829     c                   clear                   h1dtaI
096900071210     c                   clear                   h1dcsr
097000070301      * Categoria Foglio x controllo nr.distinta
097100070301     C                   z-add     4             wNPG
097200070227     C* --------------
097300070227      * ?- Controlla la filiale in gestione
097400070227     C                   CLEAR                   DSLV50
097500070227     C                   MOVEL     KNMUS         D50PRU
097600070227     C                   z-add     d1cFGS        D50FGS
097700070227     C                   CALL      'FNLV50R'
097800070227     C                   PARM                    DSLV50
097900070227    1C     D50ERR        IFNE      ' '
098000070326     c                   movel     ErrMsg(1)     d1msg
098100070326     C                   SETON                                        51  99
098200070227     C                   goto      ENDCTR1
098300070227    1C                   ENDIF
098400140929      * verifica presenza disposizioni web
098500141029     c                   clear                   fnlry09ds2
098600141029     c                   eval      ilry09tch = 'C'
098700141013     c                   exsr      verdispo
098800141029     c                   if        OLRY09ESal = '1' and
098900140929     c                             OLRY09ERR  = *blank
099000140929     c                   seton                                        97
099100140929     c                   endif
099200070227      *
099300160429     c                   setoff                                       111015
099400070227     C*  Controlla Con il P.O. la data abilitazione
099500070227     C     d1cFGS        CHAIN     AZORG01l
099600070227    1C                   IF        %found(azorg01l)
099700070227     C                   MOVEL     ORGdf0        og150
099800080606     c                   movel     orgde8        og148
099900080606      *
100000080606     c* in test o attivo x PDA   ("S", "T")
100100080606     c                   clear                   PDAconsegne
100200080606     c                   if        §OGPDACON <> *blank
100300080606     c                   eval        PDAconsegne = §OGPDACON
100400080606     c                   seton                                        15
100500080606     c                   end
100600080606      *
100700070625     c* p.o. con piking messaggerie
100800070625     c                   if        §ogpkm='S'
100900070625     c                   seton                                        11
101000070625     c                   endif
101100160429     c                   if        §ogturno='S'
101200160429     c                   seton                                        10
101300160429     c                   endif
101400070625      *
101500070227     C                   MOVEL     ORGde6        og146
101600070227     c                   if             §ogdda = *blank or §ogdda = *all'0'
101700070227     C                   MOVE      *ON           $FINE
101800070227     C                   goto      ENDCTR1
101900070227     c                   else
102000070227      * se c'è una data in organigramma
102100070227     c                   move      §ogdda        data_org          8 0
102200070227      * se siamo in fase di Parallelo prima di essere partiti
102300070426     c                   if        data_org = 20391231 or dateu8 < data_org
102400070227     C                   eval      test_Parallelo = 'S'
102500070419     c                   else
102600070419     C                   MOVE      *BLANK        test_Parallelo    1
102700070227     c                   end
102800070227      * se data distinte automatiche NON è
102900070227      * né = 31/12/2039 che significa p.o. in fase di test o parallelo
103000070227      * né < oggi che significa p.o. partito
103100070227      * allora fuori
103200070426     c********           if        data_org <> 20391231 and dateu8 < data_org
103300070426     C********           MOVE      *ON           $FINE
103400070426     C********           goto      ENDCTR1
103500070426     c********           end
103600070227     c                   end
103700070227      *-
103800070227   X1C                   ELSE
103900070227      * Non presente in organigramma
104000070326     C                   SETON                                        51  99
104100070326     c                   movel     ErrMsg(1)     d1msg
104200070227     C                   goto      ENDCTR1
104300070227    1C                   ENDIF
104400070227     C*
104500070316      * Salva la filiale su campo di work x tutto il pgm
104600070316     c                   z-add     d1cfgs        wfgs
104700070418     C                   Z-ADD     D1cFGS        sav_d1cFGS
104800070227     C* --------------
104900070227      * ?- Data Distinte del:
105000070227     C                   Z-ADD     d1cDDC        G02DAT
105100070227     C                   MOVEL     *BLANK        G02ERR
105200070227     C                   CALL      'XSRDA8'
105300070227     C                   PARM                    WLBDAT
105400070227    1C     G02ERR        IFEQ      '1'
105500070326     c                   movel     ErrMsg(2)     d1msg
105600070326     C                   SETON                                        52  99
105700070227     C                   goto      ENDCTR1
105800070227    1C                   ENDIF
105900070307      *
106000070307      *
106100070307      * Controlli identici al FNLR50R sulla data distinta
106200070307      * prima della data del giorno
106300070307     c                   if        g02inv < dateu8
106400070326     c                   movel     ErrMsg(3)     d1msg
106500070326     c                   SETON                                        52  99
106600070307     C                   goto      ENDCTR1
106700070307     c                   endif
106800070307      *controllo range fra le date di oggi e quella immessa
106900070413      *  non può essere superiore di 3 giorni lavorativi
107000070307     c                   eval      dadata = dateu8
107100070307     c                   eval      adata  = g02inv
107200070307     c                   CALL      'XSRLAV8'
107300070307     c                   PARM                    Wdata8
107400070413     c                   if        giolav > §dgddc or
107500070413     c                             giolav > 3
107600070326     c                   movel     ErrMsg(4)     d1msg
107700070326     c                   SETON                                        52  99
107800070307     C                   goto      ENDCTR1
107900070307     c                   end
108000070307      *
108100070307     C* VERIFICO CHE LA DATA DELLA DISTINTA NON SIA ANTECEDENTE O
108200070307     C* SEGUENTE ALLA UDATE DI OLTRE 15 GIORNI
108300070307     C* PER TUTTO IL MESE DI AGOSTO NON CONTROLLO 1 15 GG
108400070307     C                   MOVE      dateu8        WMGUDT            4 0
108500070307     C                   MOVEL     WMGUDT        WMUDT             2 0
108600070307     C                   Z-ADD     §DGDGG        NRGIO             3 0
108700070307      *
108800070307     C     WMGUDT        IFGE      §DGDEI
108900070307     C     WMGUDT        ANDLE     §DGDEF
109000070307     C                   Z-ADD     §DGDGE        NRGIO
109100070307     C                   END
109200070307      *
109300070307     C     dateu8        IFGE      §DGDII
109400070307     C     dateu8        ANDLE     §DGDIF
109500070307     C                   Z-ADD     §DGDGI        NRGIO
109600070307     C                   END
109700070307      *
109800070307     C     G02TGI        SUB       NRGIO         GIOTGI
109900070307     C                   CALL      'XSRGI8'
110000070307     C                   PARM                    DATPAR
110100070307     C                   Z-ADD     GIOINV        DATM15            8 0
110200070307      *
110300070307     C     DATM15        IFGT      dateu8
110400070326     c                   movel     ErrMsg(5)     d1msg
110500070326     C                   SETON                                        52  99
110600070307     C                   goto      ENDCTR1
110700070307     C                   END
110800070307      *
110900070307     C     G02TGI        ADD       NRGIO         GIOTGI
111000070307     C                   CALL      'XSRGI8'
111100070307     C                   PARM                    DATPAR
111200070307     C                   Z-ADD     GIOINV        DATP15            8 0
111300070307      *
111400070307     C     DATP15        IFLT      dateu8
111500070326     c                   movel     ErrMsg(5)     d1msg
111600070326     C                   SETON                                        52  99
111700070307     C                   goto      ENDCTR1
111800070307     C                   END
111900070307      *
112000070307     c     salta_ctrl    tag
112100070307      *
112200070227     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
112300070301     C                   Z-ADD     G02INV        h1cDDC
112400070321     c                   z-add     G02INV        wddc
112500070301     C                   Z-ADD     G02DAT        D1cDDC
112600070418     C                   Z-ADD     D1cDDC        sav_d1cDDC
112700070301     C*---------------
112800070307      *
112900070307     C*   controlla se nella data scelta ci sono distinte aperte
113000070307     c                   move      'S'           tutte_chiuse      1
113100070406     c     kdst8         setll     fidst08l
113200070406     c     kdst8         reade     fidst08l
113300070406     c                   dow       not %Eof(fidst08l)
113400070412     c                   if        dstFCF <> 'S' and dstATB = ' '
113500070307     c                   clear                   tutte_chiuse
113600070307     c                   leave
113700070307     c                   end
113800070406     c     kdst8         reade     fidst08l
113900070307     c                   enddo
114000070307      * nella data tutte non ci sono distinte aperte
114100070307     c                   if        tutte_chiuse = 'S'
114200070326     c                   movel     ErrMsg(6)     d1msg
114300070326     C                   SETON                                        52  99
114400070307     C                   goto      ENDCTR1
114500070307     c                   end
114600070307      *
114700070307     c     salta_ctrl1   tag
114800160502     C* se sceto messaggerie non si deve mettere un turno
114900160509     c                   if        d1tdis = 'M' and §ogturno='S'
115000160502     c                   if        d1tur1 = 'U' or
115100160502     c                             d1tur2 = 'U'
115200160502     c                   else
115300160502     c                   seton                                        9946
115400160502     c                   movel     ErrMsg(33)    d1msg
115500160502     C                   goto      ENDCTR1
115600160502     c                   end
115700160502     c                   end
115800160510      **
115900160510     c                   if        §ogturno='S'
116000160510     c                   if        d1tur1 = 'U' and d1tur2 = ' '  or
116100160510     c                             d1tur2 = 'U' and d1tur1 = ' '
116200160510     c                             or
116300160510     c                             d1tur1 <>'U' and d1tur2 <>'U'
116400160510     c                   else
116500160510     c                   seton                                        9946
116600160510     c                   movel     ErrMsg(33)    d1msg
116700160510     C                   goto      ENDCTR1
116800160510     c                   end
116900160510     c                   end
117000070829      * ?- Data Limite  del:
117100070829     c                   if        d1DTAi > 0
117200070829     C                   Z-ADD     d1DTAi        G02DAT
117300070829     C                   MOVEL     *BLANK        G02ERR
117400070829     C                   CALL      'XSRDA8'
117500070829     C                   PARM                    WLBDAT
117600070903     C                   Z-ADD     G02DAT        d1DTAi
117700070829    1C                   IF        G02err = '1'
117800070829     c                   movel     ErrMsg(2)     d1msg
117900070829     C                   SETON                                        59  99
118000070829     C                   goto      ENDCTR1
118100070829    1C                   Else
118200070829     c                   z-add     g02INV        h1dtaI
118300070829    1C                   ENDIF
118400070829     c                   if        g02INV > h1cDDC
118500070829     c                   movel     ErrMsg(31)    d1msg
118600070829     C                   SETON                                        59  99
118700070829     C                   goto      ENDCTR1
118800070829    1C                   END
118900070829     c                   else
119000070829    1C                   IF        d1ORAi <> *zeros
119100070829     c                   movel     ErrMsg(2)     d1msg
119200070829     C                   SETON                                        59  99
119300070829     C                   goto      ENDCTR1
119400070829    1C                   ENDIF
119500070829     c                   end
119600070829     C* --------------
119700070829      * ?- Ora  Limite     :
119800070829     c                   movel     d1ORAi        oraI              2 0
119900070829     c                   move      d1ORAi        minutiI           2 0
120000070829     c                   if        oraI > 23 or minutiI > 59
120100070829     c                   movel     ErrMsg(32)    d1msg
120200070829     C                   SETON                                        60  99
120300070829     C                   goto      ENDCTR1
120400070829    1C                   END
120500070829      *
120600070831     c                   eval      sav_d1dtaI = D1dtaI
120700070831     c                   eval      sav_d1oraI = D1oraI
120800070307     C*---------------
120900071210      * ?- Data Limite consegna richiesta Prima DEL:
121000071210     c                   if        d1Dcsr > 0
121100071210     C                   Z-ADD     d1Dcsr        G02DAT
121200071210     C                   MOVEL     *BLANK        G02ERR
121300071210     C                   CALL      'XSRDA8'
121400071210     C                   PARM                    WLBDAT
121500071210     C                   Z-ADD     G02DAT        d1Dcsr
121600071210    1C                   IF        G02err = '1'
121700071210     c                   movel     ErrMsg(2)     d1msg
121800071210     C                   SETON                                        61  99
121900071210     C                   goto      ENDCTR1
122000071210    1C                   Else
122100071210     c                   z-add     g02INV        h1dcsr
122200071210    1C                   ENDIF
122300071210     c                   end
122400071210      *
122500071210     c                   eval      sav_d1dcsr = D1dcsr
122600071210     C*---------------
122700070227      * ?- Scelta obbligatoria
122800070227     c                   if        d1sce = ' '
122900070326     c                   movel     ErrMsg(7)     d1msg
123000070227     C                   SETON                                        53  99
123100070227     C                   goto      ENDCTR1
123200070227     c                   end
123300070307     C*------------------------
123400070307      * pulisce gli altri campi non nella selezione
123500070531     c                   if        d1sce = Assegnare
123600070307     c                   clear                   d1ndc2
123700070307     c                   clear                   d1ndc3
123800070307     c                   clear                   d1ndc4
123900070307     c                   end
124000070307      *
124100070531     c                   if        d1sce = Togliere
124200070307     c                   clear                   d1ccgi
124300070307     c                   clear                   d1ndc1
124400070307     c                   clear                   d1ndc3
124500070307     c                   clear                   d1ndc4
124600070307     c                   end
124700070307      *
124800070531     c                   if        d1sce = Trasferire
124900070307     c                   clear                   d1ccgi
125000070307     c                   clear                   d1ndc1
125100070307     c                   clear                   d1ndc2
125200070307     c                   end
125300070301     C*------------------------
125400070227      * ?- Scelta (1) Assegna Giro alla distinta
125500070301     c                   clear                   d1dcgi
125600160318     c                   eval      GIRO_vedi_PACCO_si = *blank
125700160318      *
125800070531     c                   if        d1sce = Assegnare
125900070301     c     '?'           scan      d1cCGI
126000070227     C*   ricerca codice giro
126100070227     c                   if        %Found
126200070326     C******             SETON                                            99
126300070227     c                   eval      kpjbus = kpjbu
126400070227     c                   clear                   fidg09ds
126500070227     c                   move      'P01'         d09iop0
126600070905     c                   move      'C'           d09iTug
126700070227     c                   movel     d1cFGS        d09ifgs
126800070227     c                   z-add     DATEU8        d09idat
126900070228     c                   movel(p)  fidg09ds      kpjbu
127000070227     c                   call      'FIDG09R'
127100070227     c                   parm                    kpjba
127200070227     c                   movel     kpjbu         fidg09ds
127300070320     c                   movel     d09oflo       dGRGflo
127400070320      * se il giro può essere messo in distinta
127500070326      * ?lo deve comunque caricare e controllare in seguito per non
127600070326      * ?renderlo disponibile alla selezione.
127700070326     c*******            if        §grgDST <> 'N'
127800070227     c                   movel     d09ocgi       d1cCGI
127900070326     c*******            end
128000070227     c                   eval      kpjbu = kpjbus
128100070227     c                   end
128200070228      *  controllo validità giro
128300070301     c                   if        d1cCGI <> *blank
128400070301     c                   clear                   fidg09ds
128500070301     c                   eval      D09IOP0 = '001'
128600070301     c                   eval      d09ifgs = d1cFGS
128700070905     c                   eval      d09itug = 'C'
128800070301     c                   eval      d09icgi = d1cCGI
128900070301     c                   move      dateu8        d09idat
129000080606     c                   eval      kpjbus = kpjbu
129100070301     c                   eval      kpjbu = fidg09ds
129200070301     c                   call      'FIDG09R'
129300070301     c                   parm                    kpjba
129400070301     c                   eval      fidg09ds = kpjbu
129500080606     c                   eval      kpjbu = kpjbus
129600070320     c                   movel     d09oflo       dGRGflo
129700070320      * se il giro può essere messo in distinta
129800070320     c                   if        d09oerr = '1' or §grgDST = 'N'
129900070326     c                   movel     ErrMsg(8)     d1msg
130000070301     C                   SETON                                        54  99
130100070301     C                   goto      ENDCTR1
130200070301     c                   else
130300070301     c                   if        d09ocgi <> ' '
130400070301     c                   eval      d1dcgi = d09odes
130500070321     c                   movel     d1cCGI        wcgi
130600070404     c                   movel     d1cCGI        h1cCGI
130700070301     c                   move      'S'           con_giro
130800160318      *  controlla il tipo di giro se Fittizio
130900160318      **   (x bolle DPD generiche da spunte)
131000160318     c                   if        GIRO_vd_Pacco = d1cCGI
131100160318     c                   eval      GIRO_vedi_PACCO_si = 'S'
131200160321     c                   end
131300070301     c                   end
131400070301     c                   end
131500070301      *
131600070301     c                   end
131700070301     C*------------------------
131800070301      * Numero Distinta
131900070313     c                   if        d1ndc1 = *zeros
132000070313     c                   clear                   d1ndc1
132100070313     c                   end
132200070301     c                   if        d1ndc1 <> *blank
132300070301     c     '?'           scan      d1ndc1
132400070227     C*   ricerca distinta appartenente al giro oppure tutte
132500070227     c                   if        %Found
132600070326     C******             SETON                                            99
132700070606     c                   clear                   w1NO_dis
132800070606     c                   move      d1sce         w1sce
132900070321     c                   exsr      Ricerca_Dist
133000070228     C                   MOVE      *zeros        d1ndc1
133100070313     C                   MOVE      FDGNFV        d1ndc1
133200070326     C******             goto      ENDCTR1
133300070227     c                   end
133400070313     c                   if        d1ndc1 = *zeros
133500070313     c                   clear                   d1ndc1
133600070313     c                   end
133700070228      *  controllo validità distinta
133800070301     c     digits        check     d1ndc1
133900070301     c                   if        %Found
134000070326     c                   movel     ErrMsg(9)     d1msg
134100070301     C                   SETON                                        55  99
134200070301     C                   goto      ENDCTR1
134300070301     c                   end
134400070227      *
134500070301     c                   move      d1ndc1        wNFV
134600070319     c                   eval      Aggior_Dist = 'N'
134700070319      *                                     ?  =======  ?
134800070316     c                   exsr      CHAIN_Dist
134900070319     c                   if        Error_Dist = 'S'
135000070319     c                   if        Rec_Allocato = 'S'
135100070326     c                   movel     ErrMsg(11)    d1msg
135200070319     c                   end
135300070326     C                   SETON                                            55  99
135400070319     C                   goto      ENDCTR1
135500070319     c                   end
135600070319      *
135700070316     c                   if        Found_Dist = 'N'
135800070326     c                   movel     ErrMsg(9)     d1msg
135900070301     C                   SETON                                        55  99
136000070301     C                   goto      ENDCTR1
136100070301     c                   else
136200070417      * Distinta non nella data
136300070417     c                   if        dstDFV <> h1cDDC
136400070417     c                   movel     ErrMsg(30)    d1msg
136500070417     C                   SETON                                        55  99
136600070417     C                   goto      ENDCTR1
136700070417     c                   end
136800070301      * Foglio chiuso o Annullato
136900070301    3C     dstFCF        ifEQ      'S'
137000070301    4C     dstATB        orNE      ' '
137100070326     c                   movel     ErrMsg(10)    d1msg
137200070326     C                   SETON                                        55  99
137300070301     C                   goto      ENDCTR1
137400070301     c                   endIF
137500081121      *
137600081121      * Controlla se la distinta è gestita con il PDA
137700081121      *  se in test nel filler
137800081121     c                   eval      ddstFLR = dstFLR
137900160506     c                   if        DSTTPM <> 'M' and §DSTTURNO =' '
138000160506     c                   eval         §DSTTURNO = '1'
138100160506     c                   end
138200160506     c                   eval      h1tur_piu = §DSTTURNO
138300081121     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
138400081121     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
138500081121     c                   eval      h1PDA_piu = 'S'
138600081121     c                   end
138700081121      *
138800070301     c                   end
138900160318      *
139000160318      *  controlla se l'Autista è fittizio quindi possiamo considerare
139100160318      *   la Distinta Fittizia
139200160318     C                   move      'A'           tipo_cod
139300160318     C                   move      dstpdr        cod_padr
139400160318     C     Kapd          CHAIN     fiapd01l
139500160318      *   se Escluso da Fatturazione
139600160318     c                   if        %Found(fiapd01l) and apdPDD = 'S'
139700160318     c                   movel     'S'           H1pdd_PIU
139800160318     c                   end
139900160318      *
140000160318      *  Se è un giro VEDI PACCO (DPD) deve appoggiarsi PER FORZA su una DISTINTA
140100160318      *  di un AUTISTA Escluso da Fatturazione (Fittizio) altrimenti errore
140200160318    3C                   if        GIRO_vedi_PACCO_si = 'S'  and
140300160322    3C                             H1pdd_PIU <> 'S' and s1sce ='1'
140400160322     c                   if             s1sce = Assegnare or
140500160322     c                                  s1sce = Trasferire
140600160318     c                   movel     ErrMsg(37)    d1msg
140700160318     C                   SETON                                        545599
140800160318     C                   goto      ENDCTR1
140900160322     c                   endIF
141000160318     c                   endIF
141100160322      *
141200160318      *
141300070301      * se tutto OK
141400070319     c                   z-add     dstPDR        h1pdr_piu
141500120209     c                   z-add     dstDFV        h1ddc_piu
141600070319     c                   move      dstFPP        h1fpp_piu
141700070319     c                   move      d1ndc1        h1ndc_piu
141800070228     c                   endIF
141900070302     c                   end
142000070227     C*
142100070227      * ?- Scelta (2) Toglie dalla distinta
142200070531     c                   if        d1sce = Togliere
142300070301     C*---------------
142400070227      *   Numero Distinta obbligatorio
142500070316     c********           if            D1NDC2 = *blank
142600070326     c********           movel     ErrMsg(9)     d1msg
142700070316     C********           SETON                                        56  99
142800070316     C********           goto      ENDCTR1
142900070316     c********           else
143000070228      *
143100070313     c                   if        d1ndc2 = *zeros
143200070313     c                   clear                   d1ndc2
143300070313     c                   end
143400070316     c     '?'           scan      d1ndc2
143500070228     C*   ricerca distinta appartenente al giro oppure tutte
143600070316     c                   if        %Found   or
143700070316     c                             d1ndc2 = *zeros or d1ndc2 = *blanks
143800070326     C******             SETON                                            99
143900070606     c                   move      d1sce         w1sce
144000070606     c                   clear                   w1NO_dis
144100070321     c                   exsr      Ricerca_Dist
144200070606      *
144300070606      * se tornando dalla ricerca distinta è stato scelto di fare un trasferimento
144400070606      *  invece di Togliere e basta
144500070606     c                   if        fdgOPZ = Trasferire
144600070606     C                   MOVE      Trasferire    d1sce
144700070606     C                   MOVE      *zeros        d1ndc3
144800070606     C                   MOVE      FDGNFV        d1ndc3
144900070606     c                   goto      ini_CTRD1
145000070606     c                   end
145100070606      *
145200070228     C                   MOVE      *zeros        d1ndc2
145300070313     C                   MOVE      FDGNFV        d1ndc2
145400160509     c                   if        d1ndc2 = *all'0'
145500160509     c                   clear                   d1ndc2
145600160509     c                   end
145700070326     C******             goto      ENDCTR1
145800070228     c                   end
145900070228      *  controllo validità distinta
146000070301     c     digits        check     d1ndc2
146100070301     c                   if        %Found
146200070326     c                   movel     ErrMsg(9)     d1msg
146300070301     C                   SETON                                        56  99
146400070301     C                   goto      ENDCTR1
146500070301     c                   end
146600070301      *
146700070301     c                   move      d1ndc2        wNFV
146800070319     c                   eval      Aggior_Dist = 'N'
146900070319      *                                     ?  =======  ?
147000070316     c                   exsr      CHAIN_Dist
147100070319     c                   if        Error_Dist = 'S'
147200070319     c                   if        Rec_Allocato = 'S'
147300070326     c                   movel     ErrMsg(11)    d1msg
147400070319     c                   end
147500070326     C                   SETON                                            56  99
147600070319     C                   goto      ENDCTR1
147700070319     c                   end
147800070319      *
147900070316     c                   if        Found_Dist = 'N'
148000070326     c                   movel     ErrMsg(9)     d1msg
148100070301     C                   SETON                                        56  99
148200070301     C                   goto      ENDCTR1
148300080701      *
148400070301     c                   else
148500080701      *
148600070417      * Distinta non nella data
148700070417     c                   if        dstDFV <> h1cDDC
148800070417     c                   movel     ErrMsg(30)    d1msg
148900070417     C                   SETON                                        56  99
149000070417     C                   goto      ENDCTR1
149100070417     c                   end
149200070301      * Foglio chiuso o Annullato
149300070301    3C     dstFCF        ifEQ      'S'
149400070301    4C     dstATB        orNE      ' '
149500070326     c                   movel     ErrMsg(10)    d1msg
149600070326     C                   SETON                                        56  99
149700070301     C                   goto      ENDCTR1
149800070301     c                   endIF
149900080701      *
150000080701      * Controlla se la distinta è gestita con il PDA
150100080701      *  se in test nel filler
150200080701     c                   eval      ddstFLR = dstFLR
150300160506     c                   if        DSTTPM <> 'M' and §DSTTURNO =' '
150400160506     c                   eval         §DSTTURNO = '1'
150500160506     c                   end
150600160506     c                   eval      h1tur_meno= §DSTTURNO
150700080701     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
150800080701     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
150900080701     c                   eval      h1PDA_meno = 'S'
151000080701     c                   end
151100080701      *
151200070301     c                   end
151300070227      *
151400160318      *  controlla se l'Autista è fittizio quindi possiamo considerare
151500160318      *   la Distinta Fittizia
151600160318     C                   move      'A'           tipo_cod
151700160318     C                   move      dstpdr        cod_padr
151800160318     C     Kapd          CHAIN     fiapd01l
151900160318      *   se Escluso da Fatturazione
152000160318     c                   if        %Found(fiapd01l) and apdPDD = 'S'
152100160318     c                   movel     'S'           H1pdd_meno
152200160318     c                   end
152300070301      * se tutto OK
152400070319     c                   z-add     dstPDR        h1pdr_meno
152500120209     c                   z-add     dstDFV        h1ddc_meno
152600070319     c                   move      dstFPP        h1fpp_meno
152700160506     c                   move      dstFLR        DdstFLR
152800160506     c                   if        DSTTPM <> 'M' and §DSTTURNO =' '
152900160506     c                   eval         §DSTTURNO = '1'
153000160506     c                   end
153100160506     c                   eval      h1tur_meno= §DSTTURNO
153200070319     c                   move      d1ndc2        h1ndc_meno
153300070227     c                   end
153400070316     c**************     end
153500070227     C*
153600070227      * ?- Scelta (3) Trasferisce dalla distinta alla distinta
153700070531     c                   if        d1sce = Trasferire
153800070301     C*---------------
153900070227     C*   Numero Distinta obbligatorio
154000070316     c*************      if            D1NDC3 = *blank
154100070326     c********           movel     ErrMsg(9)     d1msg
154200070316     C*************      SETON                                        57  99
154300070316     C*************      goto      ENDCTR1
154400070316     c*************      else
154500070228      *
154600070313     c                   if        d1ndc3 = *zeros
154700070313     c                   clear                   d1ndc3
154800070313     c                   end
154900070316     c     '?'           scan      d1ndc3
155000070228     C*   ricerca distinta appartenente al giro oppure tutte
155100070316     c                   if        %Found or
155200070316     c                             d1ndc3 = *zeros or d1ndc3 = *blanks
155300070326     C********           SETON                                            99
155400070606     c                   move      d1sce         w1sce
155500070606     c                   clear                   w1NO_dis
155600070321     c                   exsr      Ricerca_Dist
155700070606      *
155800070606      * se tornando dalla ricerca distinta è stato scelto di fare un togli
155900070606      *  invece di Trasferire e basta
156000070606     c                   if        fdgOPZ = Togliere
156100070606     C                   MOVE      Togliere      d1sce
156200070606     C                   MOVE      *zeros        d1ndc2
156300070606     C                   MOVE      FDGNFV        d1ndc2
156400070606     c                   goto      ini_CTRD1
156500070606     c                   end
156600070606      *
156700070228     C                   MOVE      *zeros        d1ndc3
156800070313     C                   MOVE      FDGNFV        d1ndc3
156900160509     c                   if        d1ndc3 = *all'0'
157000160509     c                   clear                   d1ndc3
157100160509     c                   end
157200070326     C********           goto      ENDCTR1
157300070228     c                   end
157400070301      *  controllo validità distinta
157500070301     c     digits        check     d1ndc3
157600070301     c                   if        %Found
157700070326     c                   movel     ErrMsg(9)     d1msg
157800070301     C                   SETON                                        57  99
157900070301     C                   goto      ENDCTR1
158000070301     c                   end
158100070301      *
158200070301     c                   move      d1ndc3        wNFV
158300070319     c                   eval      Aggior_Dist = 'N'
158400070319      *                                     ?  =======  ?
158500070316     c                   exsr      CHAIN_Dist
158600070319     c                   if        Error_Dist = 'S'
158700070319     c                   if        Rec_Allocato = 'S'
158800070326     c                   movel     ErrMsg(11)    d1msg
158900070319     c                   end
159000070326     C                   SETON                                            57  99
159100070319     C                   goto      ENDCTR1
159200070319     c                   end
159300070319      *
159400070316     c                   if        Found_Dist = 'N'
159500070326     c                   movel     ErrMsg(9)     d1msg
159600070301     C                   SETON                                        57  99
159700070301     C                   goto      ENDCTR1
159800080701      *
159900070301     c                   else
160000080701      *
160100070417      * Distinta non nella data
160200070417     c                   if        dstDFV <> h1cDDC
160300070417     c                   movel     ErrMsg(30)    d1msg
160400070417     C                   SETON                                        57  99
160500070417     C                   goto      ENDCTR1
160600070417     c                   end
160700070301      * Foglio chiuso o Annullato
160800070301    3C     dstFCF        ifEQ      'S'
160900070301    4C     dstATB        orNE      ' '
161000070326     c                   movel     ErrMsg(10)    d1msg
161100070326     C                   SETON                                        57  99
161200070301     C                   goto      ENDCTR1
161300070301     c                   endIF
161400080701     C*
161500080701      * Controlla se la distinta è gestita con il PDA
161600080701      *  se in test nel filler
161700080701     c                   eval      ddstFLR = dstFLR
161800160506     c                   if        DSTTPM <> 'M' and §DSTTURNO =' '
161900160506     c                   eval         §DSTTURNO = '1'
162000160506     c                   end
162100160506     c                   eval      h1tur_meno= §DSTTURNO
162200080701     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
162300080701     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
162400080701     c                   eval      h1PDA_meno = 'S'
162500080701     c                   end
162600080701     C*
162700070301     c                   end
162800160318      *
162900160318      *  controlla se l'Autista è fittizio quindi possiamo considerare
163000160318      *   la Distinta Fittizia
163100160318     C                   move      'A'           tipo_cod
163200160318     C                   move      dstpdr        cod_padr
163300160318     C     Kapd          CHAIN     fiapd01l
163400160318      *   se Escluso da Fatturazione
163500160318     c                   if        %Found(fiapd01l) and apdPDD = 'S'
163600160318     c                   movel     'S'           H1pdd_meno
163700160318     c                   end
163800070301      * se tutto OK
163900070319     c                   z-add     dstPDR        h1pdr_meno
164000120209     c                   z-add     dstDFV        h1ddc_meno
164100070319     c                   move      dstFPP        h1fpp_meno
164200070319     c                   move      d1ndc3        h1ndc_meno
164300070316     c*************      end
164400070301      *---------------
164500070227     C*   Numero Distinta obbligatorio
164600070316     c*************      if            D1NDC4 = *blank
164700070326     c********           movel     ErrMsg(9)     d1msg
164800070316     C*************      SETON                                        58  99
164900070316     C*************      goto      ENDCTR1
165000070316     c*************      else
165100070228      *
165200070326     c                   if        d1ndc4 = *zeros or d1ndc4 = *blanks
165300070313     c                   clear                   d1ndc4
165400070326     c                   else
165500070316     c     '?'           scan      d1ndc4
165600070228     C*   ricerca distinta appartenente al giro oppure tutte
165700070228      * ?- DA implementare ????????????????
165800070326     c                   if        %Found
165900070326     c********                     or d1ndc4 = *zeros or d1ndc4 = *blanks
166000070326     C**********         SETON                                            99
166100070606     c                   move      Scegliere     w1sce
166200070606     c                   move      d1ndc3        w1NO_dis
166300070321     c                   exsr      Ricerca_Dist
166400070606     c                   clear                   w1NO_dis
166500070228     C                   MOVE      *zeros        d1ndc4
166600070313     C                   MOVE      FDGNFV        d1ndc4
166700070326     C********           goto      ENDCTR1
166800070228     c                   end
166900070301      *  controllo validità distinta
167000070301     c     digits        check     d1ndc4
167100070301     c                   if        %Found
167200070326     c                   movel     ErrMsg(9)     d1msg
167300070301     C                   SETON                                        58  99
167400070301     C                   goto      ENDCTR1
167500070301     c                   end
167600070227     C*
167700070301     c                   move      d1ndc4        wNFV
167800070319     c                   eval      Aggior_Dist = 'N'
167900070319      *                                     ?  =======  ?
168000070316     c                   exsr      CHAIN_Dist
168100070319     c                   if        Error_Dist = 'S'
168200070319     c                   if        Rec_Allocato = 'S'
168300070326     c                   movel     ErrMsg(11)    d1msg
168400070319     c                   end
168500070326     C                   SETON                                            58  99
168600070319     C                   goto      ENDCTR1
168700070319     c                   end
168800070319      *
168900070316     c                   if        Found_Dist = 'N'
169000070326     c                   movel     ErrMsg(9)     d1msg
169100070301     C                   SETON                                        58  99
169200070301     C                   goto      ENDCTR1
169300080701      *
169400070301     c                   else
169500080701      *
169600070417      * Distinta non nella data
169700120207      *   Modificato il 7/2/2012 dando la possibilità di inserire su una distinta
169800120207      *    successiva alla data inserita
169900120207      * Ossia posso trasferire su una distinta del giorno dopo
170000120207     c********           if        dstDFV <> h1cDDC
170100120207     c                   if        dstDFV <  h1cDDC
170200070417     c                   movel     ErrMsg(30)    d1msg
170300070417     C                   SETON                                        58  99
170400070417     C                   goto      ENDCTR1
170500070417     c                   end
170600070301      * Foglio chiuso o Annullato
170700070301    3C     dstFCF        ifEQ      'S'
170800070301    4C     dstATB        orNE      ' '
170900070326     c                   movel     ErrMsg(10)    d1msg
171000070326     C                   SETON                                        58  99
171100070301     C                   goto      ENDCTR1
171200070301     c                   endIF
171300070301     c                   end
171400070326      *
171500080701      * Controlla se la distinta è gestita con il PDA
171600080701      *  se in test nel filler
171700080701     c                   eval      ddstFLR = dstFLR
171800160506     c                   if        DSTTPM <> 'M' and §DSTTURNO =' '
171900160506     c                   eval         §DSTTURNO = '1'
172000160506     c                   end
172100160506     c                   eval      h1tur_piu = §DSTTURNO
172200080701     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
172300080701     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
172400080701     c                   eval      h1PDA_piu = 'S'
172500080701     c                   end
172600080701      *
172700070326     c                   end
172800070326      *
172900070314     c                   if            D1NDC3 = D1NDC4
173000070326     c                   movel     ErrMsg(9)     d1msg
173100070314     C                   SETON                                        575899
173200070314     C                   goto      ENDCTR1
173300070314     c                   end
173400070314      *---------------
173500160318      *
173600160318      *  controlla se l'Autista è fittizio quindi possiamo considerare
173700160318      *   la Distinta Fittizia
173800160318     C                   move      'A'           tipo_cod
173900160318     C                   move      dstpdr        cod_padr
174000160318     C     Kapd          CHAIN     fiapd01l
174100160318      *   se Escluso da Fatturazione
174200160318     c                   if        %Found(fiapd01l) and apdPDD = 'S'
174300160318     c                   movel     'S'           H1pdd_PIU
174400160318     c                   end
174500070301      * se tutto OK
174600070319     c                   z-add     dstPDR        h1pdr_piu
174700120209     c                   z-add     dstDFV        h1ddc_piu
174800070319     c                   move      dstFPP        h1fpp_piu
174900070319     c                   move      d1ndc4        h1ndc_piu
175000070316     c*************      end
175100070301     C*
175200070301     c                   end
175300070227     C*
175400070227     C     ENDCTR1       ENDSR
175500141013      *_______________________________________________________________*
175600141013     C     verdispo      BEGSR
175700141013      *_______________________________________________________________*
175800141029     c                   eval      ilry09fgs = d1cfgs
175900141029     c                   eval      ilry09tla = 'L'
176000141029     c                   eval      ilry09tdis= 'E'
176100141029     c                   movel     fnlry09ds2    kpjbu
176200141013     c                   call      'FNLRY09R'
176300141013     c                   parm                    kpjba
176400141029     c                   movel     kpjbu         fnlry09ds2
176500141013     C                   endsr
176600070301      *---------------------------------------------------------------*
176700070319      *?  Aggancia la Distinta x allocarla ed in seguito x aggiornarla ?
176800070301      *---------------------------------------------------------------*
176900070321     C     Ricerca_Dist  BEGSR
177000070228     C*
177100070321     C                   clear                   FIDG31DS
177200071029     c                   move      savPOSnf      fdgNFpos
177300070321     C                   Z-ADD     0             FDGNFV
177400070321     C                   MOVEL     wFGS          FDGFGS
177500070326      *
177600070326     C                   if        $TASTO   <>  F05
177700070321     C                   MOVEL     wCGI          FDGCGI
177800070326     c                   end
177900070326      *
178000070321     C                   Z-ADD     wDDC          FDGGDA
178100070321     C                   Z-ADD     wDDC          FDGGAL
178200070321     C                   move      wNPG          FDGNPG
178300070321     C                   move      'S'           FDGRIC
178400070321     C                   move      '2'           FDGTFV
178500070321     C                   move      'P'           FDGTPV
178600070606     C                   move      w1sce         FDGopz
178700070606     C                   z-add     w1NO_dis      FDGnf1
178800070625     C                   move      d1Tdis        FDGTdis
178900070625     C                   move      d1Tusc        FDGTusc
179000160429     C                   move      d1Tur1        FDGTur1
179100160429     C                   move      d1Tur2        FDGTur2
179200070823     C                   move      'N'           FDGstp
179300080616     C                   move      'E'           FDGieFAS
179400080616     C                   move      'STP'         FDGfas01
179500080616     C                   move      'CUS'         FDGfas02
179600070321     c                   eval      kpjbus = kpjbu
179700070404     c                   clear                   $FV6
179800070404      * impostato a tutti 99999
179900070404     c                   do        600           yy                3 0
180000070404     c                   z-add     999999        $FV6(yy)
180100070404     c                   enddo
180200080715     c                   movel     'ASSMANUALE'  fdgPGM
180300080606      * ?Attenzione:
180400080606      * ? Se richiamato da MONITOR con PDA, occorre modificare
180500080606      * ?  alcuni parametri:
180600080606     c                   if        PGM_chiamante = 'MONITOR'
180700110309     c**************
180800110309      *********  Se richiamato da MONITOR DEVE sempre inviare l'informazione <MONITOR>
180900110309      *********   occorre quindi fare le dovute modifiche sul FIDG31R per i
181000110309      *********   vari casi in funzione del Togliere oppure Mettere poichè
181100110309      *********    serve fare vedere solo la specifica distinta nel caso in cui
181200110309      *********     si deve eseguire solo il TOGLI. (Vedi il FIDG31R correlato)
181300110309     c**************                and d1sce= Togliere
181400110309     c**************
181500080606      *
181600080606     c                   eval       fdgPGM = 'MONITOR'
181700080606     c                   eval       fdgSTP = ' '
181800080616      * dal Monitor si vuole tutto
181900080616     C                   move      'I'           FDGieFAS
182000080616     C                   clear                   FDGfas01
182100080616     C                   clear                   FDGfas02
182200080606     C                   Z-ADD     G30nfv        FDGNFV
182300080606     c                   end
182400080606      *
182500070321     C                   MOVEL(p)  FIDG31DS      KPJBU
182600070321     C                   CALL      'FIDG31R'
182700070321     C                   PARM                    KPJBA
182800070404     C                   PARM                    $FV6
182900070321     C                   MOVEL     KPJBU         FIDG31DS
183000080606      *
183100071029     c                   if        FDGnf1 > 0
183200071029     c                   move      fdgNF1        fdgNFpos
183300071029     C                   end
183400080606      *
183500071029     c                   move      fdgNFpos      savPOSnf
183600070606     C                   clear                   FDGnf1
183700070606     c                   clear                   w1NO_dis
183800070321     c                   eval      kpjbu = kpjbus
183900070531     C*
184000070531     C*  se restituisce un'opzione x eseguire Togliere/Trasferire + giro
184100070321     C*
184200070321     C                   ENDSR
184300070321      *---------------------------------------------------------------*
184400070321      *?  Aggancia la Distinta x allocarla ed in seguito x aggiornarla ?
184500070321      *---------------------------------------------------------------*
184600070321     C     Chain_Dist    BEGSR
184700070321     C*
184800070316      * inizializza il trovato/non trovato
184900070316     c                   eval      Found_Dist = 'N'
185000070319     c                   eval      Error_Dist = 'N'
185100070319     c                   eval      rec_allocato = 'N'
185200070316     C*
185300070517      *   Dalla Vista logica prende il NrelRec  ?
185400070517     C     Kdst          CHAIN     fidst01l
185500071025     C*
185600071025      * ?Se non trovava il record allocava l'ultima distinta che aveva
185700071025      * ?trovato poichè il nrelrec. non si resettava ma era impostato
185800071025      * ?da prima.
185900071025     c                   if        not %Found(fidst01l)
186000071025     c                   eval      dst_nrr = *hival
186100071025     c                   end
186200070517     C*
186300070517      *   x agganciare il Fisico solo x allocare il Record  ?
186400070517     C     dst_nrr       CHAIN(e)  fidst00F
186500070517      * ?è stata usata questa tecnica perchè l'aggiornamento del FIDST
186600070517      * ?è fatto mediante il FIDG37R e,sotto COMIT,xchè tutto funzioni
186700070517      * ?correttamente,il file deve essere condiviso in SHARE altrimenti
186800070517      * ?non funziona. (Si è scelto di utilizzare x tutti il fisico)
186900070316     c*
187000070316     c                   if        %Error
187100070319     c                   eval      Error_Dist = 'S'
187200070316      * se il record è allocato mando messaggio info all'utente che alloca il rcd
187300070316     c*
187400070316     c                   clear                   trul82ds
187500070316     c                   eval      ul82§rrn = dst_nrr
187600070316     c                   eval      ul82§fil = 'FIDST00F'
187700070316     c                   eval      ul82§win = 'S'
187800070316     c                   eval      ul82§f7  = 'S'
187900070319     c                   eval      ul82§num = 2
188000070319     c                   eval      ul82§att = 2
188100070326     c                   eval      ul82§mss = ErrMsg(27)
188200070316     c                   Eval      UL82§msw = 'La Distinta '
188300070316     c                             + %editc(Wnfv:'Z') +  ' è già utilizzata da -
188400070319     c                              un altro lavoro e non è manutenzionabile'
188500070316      *
188600070316      * chiamo il pgm che manda il messaggio info all'utente
188700070316     c                   call(e)   'TRUL82R'
188800070316     c                   parm                    trul82ds
188900070316      *
189000070316     c                   if        ul82§sts = 'A'
189100070319     c                   move      'S'           rec_allocato
189200070316     c                   endif
189300070316     C*
189400070316     c                   end
189500070316      * Trovato record
189600070517     c                   if        %Found(fidst00F)
189700070316     c                   eval      Found_Dist = 'S'
189800070319     C*
189900070319      *   Se richiamato x aggiornamento finale dopo ARG e ARB  ?
190000070319      *   quindi non solo per semplice allocazione del record. ?
190100070319     c                   if        Aggior_Dist = 'S'
190200070319      *                                     ?  =======  ?
190300070319     c                   exsr      Campi_Distinta
190400070517      *                                     ?  =======  ?
190500070517     c                   else
190600070517      * ?  qui l'Update serve esclusivamente per tenere allocato  ?
190700070517      * ?  il record in quanto nel CLLE FIDG30C x il COMMIT è    ?
190800070517      * ?  stato attivato lo STRCMTCTL con *CHG.    ?
190900070316     c                   update    fidst000
191000070517     C*
191100070517     c                   endIF
191200070517     C*
191300070316     c                   end
191400070316     C*
191500070316     C     ENDChain      ENDSR
191600070316      *---------------------------------------------------------------*
191700070319      *  ?  Imposta i campi della distinta da Aggiornare             ?
191800070316      *---------------------------------------------------------------*
191900070319     C     Campi_DistintaBEGSR
192000070517     C*********
192100070517     c*********          clear                   fidg28ds
192200070517     c*********          eval      D28FGSI = Wfgs
192300070517     c*********          eval      D28DFVI = h1cddc
192400070517     c*********          eval      D28NFVI = wnfv
192500070517     c*********          call      'FIDG28R'
192600070517     c*********          parm                    fidg28ds
192700070517     c*********          if        d28erro = ' '
192800070517     c*********          eval      DSTPKLa = D28PKLao
192900070517     c*********          eval      DSTVLUa = D28VLUao
193000070517     c*********          eval      DSTstpa = D28stpao
193100070517     c*********          eval      DSTNBLa = D28NBLao
193200070517     c*********          else
193300070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
193400070517     c*********          eval      Error_Dist  = 'S'
193500070517     c*********          end
193600070319     C*
193700070517      * ?  Oltre a calcolare i valori aggiorna anche il FIDST e FIDST40
193800070517     c                   clear                   fidg37ds
193900070517     c                   eval      D37FGSI = Wfgs
194000070517     c                   eval      D37DFVI = h1cddc
194100070517     c                   eval      D37NFVI = wnfv
194200070517      * ?  FIDG37R non deve eseguire il Commit ma lo deve fare il FIDG30R
194300070517     c                   eval      D37CMTI = 'N'
194400070829     c                   eval      D37dtaI = h1dtaI
194500070829     c                   eval      D37oraI = d1oraI
194600070517     c                   eval      kpjbus = kpjbu
194700070517     c                   eval      kpjbu  = fidg37ds
194800070517     c                   call      'FIDG37R'
194900070517     c                   parm                    kpjba
195000080606      ** mancava questa specifica che reimpostava la DS x caricare i 4 campi
195100080606     c                   eval      fidg37ds = kpjbu
195200070517     c                   eval      kpjbu = kpjbus
195300070517     c                   if        d37erro = ' '
195400070517     c                   eval      DSTPKLa = D37PKLao
195500070517     c                   eval      DSTVLUa = D37VLUao
195600070517     c                   eval      DSTstpa = D37stpao
195700070517     c                   eval      DSTNBLa = D37NBLao
195800070517     c                   else
195900070517      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
196000070517     c                   eval      Error_Dist  = 'S'
196100070517     c                   end
196200070517     C*
196300070319     C                   ENDSR
196400070319      *---------------------------------------------------------------*
196500070319      *  ?  GESTIONE SFL GIRI  da selezionare                        ?
196600070319      *---------------------------------------------------------------*
196700070319     C     GESS1         BEGSR
196800070319     C*
196900940223     C* inizializzazione videata
197000940223     C     $INZS1        IFEQ      *ON
197100940127     C                   EXSR      INZS1
197200940223     C                   MOVE      *OFF          $INZS1
197300940127     C                   ENDIF
197400070327     C*
197500070306     C* Inizializza e imposta piede videata
197600070306     C                   EXSR      INZZ1
197700070306     C*
197800030113     C* emissione piede videata
197900070302      * ?            *-----------------------*
198000070228     C                   WRITE     FDG0Z01
198100070302      * ?            *-----------------------*
198200070228     C*
198300940223     C     WMAX          IFEQ      0
198400070228     C* Non ci sono records
198500070302      * ?            *-----------------------*
198600070228     C                   WRITE     FDG0V01
198700070302      * ?            *-----------------------*
198800070301     c                   setoff                                       30
198900030114     C                   Else
199000070301     c                   seton                                        30
199100070330     c                   eval       sav_WSfl =  WSfl
199200070330     c                   eval       sav_Wpag =  Wpag
199300070301      *
199400030114     C     Wsfl          IFgt      0
199500030114     C                   Z-ADD     wsfl          C1RCD
199600030114     C                   Else
199700030114     C     Wpag          IFgt      0
199800030114     C                   Z-ADD     wpag          C1RCD
199900030114     C                   EndIF
200000030114     C                   EndIF
200100030114     C                   ENDIF
200200070228      *
200300070228      *  Emissione SFL dei GIRI
200400070228      * ?            *-----------------------*
200500070228     C                   EXFMT     FDG0C01
200600070228      * ?            *-----------------------*
200700070326     c                   setoff                                       99
200800070305     C                   movel     'S1'          sav$GEST
200900070228      *
201000940204     C     C1NRR         IFNE      0
201100940204     C                   Z-ADD     C1NRR         WSFL
201200940204     C                   ENDIF
201300940127     C                   Z-ADD     SFLNRR        C1RCD
201400030113     C* Selezioni
2015009401271    C                   SELECT
201600070328     C*
201700070328     C* F3=Fine
201800070328     C     $TASTO        WHENEQ    F03
201900070328     C                   EXSR      F03_exit
202000070328     c                   rolbk
202100070328      *
202200070328     C* F5=Int.tutte le Distinte
202300070328     C     $TASTO        WHENEQ    F05
202400070328     C                   EXSR      F05_interr
202500070328      *
202600070328     C* F12=Ritorno
202700070328     C     $TASTO        WHENEQ    F12
202800070328     c                   move      'D1'          $GEST
202900160506     C                   MOVE      *On           $INZD1
203000070328     c                   rolbk
203100070328      *
203200070328     C* F1=Seleziona Tutti
203300070328     C                   WHEN      $TASTO   =   F01
203400070328     C                   EXSR      F01_Tutti1
203500070328      *
2036009401271O   C                   OTHER
203700070228     C* CONTROLLO testata
203800940131     C                   EXSR      CTRC1
203900070228     C* CONTROLLO subfile
204000940201     C     *IN99         IFEQ      *OFF
204100940131     C                   EXSR      CTRS1
204200940131     C                   END
204300070302      *
204400070302     C* F4=Int.Distinte Stesso Giro
204500070302      *   Ma deve esserne selezionata solo una altrimenti segnala l'errore
204600070302     C     *IN99         IFEQ      *OFF
204700070302     C     $TASTO        andeq     F04
204800070302     c                   if        Conta_Giri = 1
204900070302     C                   EXSR      F04_interr
205000070326      *
205100070326      *  se non ha scelto il foglio deve eliminare la selezione sul sfl dei giri
205200070326     c                   if        fdgnfv = 0
205300070326     c     sav_s1nrr     chain     FDG0S01                            21
205400070326     c                   if        *in21 = *off
205500070326     c                   clear                   s1sce
205600070326     c                   update    FDG0S01
205700070326     c                   end
205800070326     c                   end
205900070326      *
206000070302     c                   else
206100070302      * errore
206200070302     c                   seton                                        51  99
206300070302     c                   end
206400070302     c                   END
206500070327      *
206600070504      * ?se premuto "Enter" è stato richiesto il dettaglio di tutte le
206700070228      * ? spedizioni dei giri selezionati per selezione bolla bolla.
206800070228     C     *IN99         IFEQ      *OFF
206900070419     C     $TASTO        andeq     Enter                                        ex F9
207000070302     C                   MOVE      *ON           $INZS2
207100070301     C                   movel     'S2'          $GEST
207200070306     C                   MOVE      *ON           $VISS2
207300070228     C                   END
207400070228      *
207500070228      * ? emette la finestra dei totali Distinta
207600070328      * ?  se non ci sono stati errori precedentemente
207700070504      * ?se NON premuto Enter bensì F6 e immessa una distinta,
207800070328      * ?   e se è stato ripulito il messaggi degli errori precedenti
207900070328      * ?  se il sfl è carico.
208000070228     C     *IN99         IFEQ      *OFF
208100070419     C     $TASTO        andne     Enter                                        ex F9
208200070504     C     $TASTO        andeq     F06                                          ex F9
208300070328     c     ErrMsgZ       andEQ     *blank
208400070328     C     WMAX          andGT     0
208500070306     C                   MOVE      *ON           $INZS2
208600070306     C                   movel     'S2'          $GEST
208700070306     C                   MOVE      *OFF          $VISS2
208800070314     c                   end
208900070228      *
2090009401271-   C                   ENDSL
209100940127     C*
209200940127     C                   ENDSR
209300070328     C/EJECT
209400070328      *---------------------------------------------------------------*
209500070328      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
209600070328      *---------------------------------------------------------------*
209700070328     C     INZZ1         BEGSR
209800070328     C*
209900070306     C* Messaggio o tasti funzionali
210000070306     c                   clear                   z1dmsg
210100070328     c                   clear                   ErrmsgZ          80
210200070307     C*
210300070306      * errore di selezione di + giri x richiesta interr.stesso giro
210400070306     c                   if        *in51 = *on
210500070328     c                   movel     ErrMsg(12)    ErrmsgZ
210600070306     c                   setoff                                       51
210700070306     c                   end
210800070319     c                   if        *in52 or *in54
210900070328     c                   movel     ErrMsg(13)    ErrmsgZ
211000070319     c                   setoff                                       5254
211100070306     c                   end
211200070326     c                   if        *in62
211300070328     c                   movel     ErrMsg(15)    ErrmsgZ
211400070326     c                   setoff                                       62
211500070319     c                   end
211600070319     c                   if        *in53 or *in55
211700070328     c                   movel     ErrMsg(14)    ErrmsgZ
211800070319     c                   setoff                                       5355
211900070306     c                   end
212000070326     c                   if        *in56 = *on
212100070328     c                   movel     ErrMsg(16)    ErrmsgZ
212200070326     c                   setoff                                       56
212300070326     c                   end
212400070328      *
212500070328      * imposta eventuali errori
212600070328     c                   eval      z1dmsg = ErrmsgZ
212700070307     C* tasti funzionali
212800070328     c                   if        ErrmsgZ = *blank
212900070307     c                   setoff                                       99
213000070307     c                   movel     Fun_1         z1dmsg
213100070419     c                   eval      %subst(z1dmsg:20:31) = FUN_6
213200070419     c                   move      Fun_Enter     z1dmsg
213300070326     c                   end
213400070306     C*
213500070306     C                   ENDSR
213600070306     C/EJECT
213700070306      *---------------------------------------------------------------*
213800070320      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
213900070306      *---------------------------------------------------------------*
214000070306     C     INZS1         BEGSR
214100070306     C*
214200070228     C* reset indicatori DSPATR
214300070308     c                   exsr      INZ_indERR
214400940302     C* pulizia SFL
214500940128     C                   SETOFF                                         3031
214600070228     C                   WRITE     FDG0C01
214700940128     C                   SETON                                          31
214800940128     C*
214900070228     C* Righe x Pagina del 1° SFL
215000070427     c                   z-add     RigPag_SFl1   s1pag
215100070228      *
215200030113     C* CARICAMENTO SFL totale
215300940201     C                   Z-ADD     0             S1NRR
215400030113     C                   Z-ADD     1             C1RCD
215500940128     C                   Z-ADD     0             WMAX
215600070302     C                   Z-ADD     0             Wsfl
215700070302     C* campi titolo
215800070302     C                   MOVEL     dsPGM         c1CPGM
215900070302     C                   MOVEL     knsif         c1nsif
216000070302     C                   MOVEL     knmus         c1nmus
216100070302     C                   MOVEL     RSUT          c1crsu
216200070308     C                   clear                   c1ndc1
216300070302     C*
216400070302     C* Se immessa la Distinta  disabilita Tasti Funzionali di ricerca
216500070315     c                   setoff                                       44
216600070319     c                   if        h1ndc_piu  > 0
216700070302     c                   movel     Alla_Dist     c1ndc1
216800070319     c                   move      h1ndc_piu     distintalfa
216900070305     c                   do        7             xy
217000070305     c                   if        ndc_a(xy) <> '0'
217100070305     c                   leave
217200070305     c                   end
217300070305     c                   move      ' '           ndc_a(xy)
217400070305     c                   enddo
217500070305     c                   move      distintalfa   c1ndc1
217600070302     C* ND tasti di ricerca Distinte
217700070315     c********           seton                                        44
217800070302     c                   end
217900070403     C*
218000070403     c* se il p.o. è in parallelo testo la distinta su ARG altrimenti
218100070403     c* se il p.o. è partito testo la distinta su ARB
218200070302     C*
218300940224     C* Posizionamento su file pilota
218400070228      * ? Se presente il giro
218500070228     c                   IF        con_GIRO = 'S'
218600070403     c     k52j          setll     fiarg52j
218700070403     c                   ELSE
218800070403     c     wfgs          setll     fiarg52j
218900070228     c                   END
219000030113     C* Carico il SFL
219100940127     C                   EXSR      ROLS1
219200030113     C*
219300030114     C                   Z-ADD     1             WPAG
219400070504     c                   eval       sav_Wmax =  Wmax
219500940127     C*
219600940127     C                   ENDSR
219700070305     C/EJECT
219800070305      *---------------------------------------------------------------*
219900070320      *? CARICAMENTO PAGINA LISTA                                    ?
220000070305      *---------------------------------------------------------------*
220100070305     C     ROLS1         BEGSR
220200070305     C*
220300070305     C                   SETOFF                                       32
220400070305     C                   Z-ADD     0             Y
220500070305     C                   Z-ADD     WMAX          S1NRR
220600070305     C*
220700070305     C* Caricamento del Sfl x Giro
220800070305     c                   Exsr      Write_S1
220900070305     C*
221000070305     C                   Z-ADD     S1NRR         WMAX                 30
221100070305     C*
221200070305     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
221300070305     C     S1NRR         DIV       S1pag         PAGINE            4 0
221400070305     C                   MVR                     RESTO             3 0
221500070305     C     PAGINE        MULT      S1pag         C1RCD
2216000703051    C     RESTO         IFGT      0
221700070305     C                   ADD       1             C1RCD
2218000703051E   C                   ELSE
221900070305     C                   SUB       S1pag         C1RCD
222000070305     C                   ADD       1             C1RCD
2221000703051-   C                   ENDIF
222200070305     C*
222300070305     C                   ENDSR
222400070301      *---------------------------------------------------------------*
222500070320      *?  Scrive x Ogni rottura di GIRO                              ?
222600070301      *---------------------------------------------------------------*
222700070305     C     Write_S1      BEGSR
222800070305      *
222900070305     C*  Esegue una lettura di posizionamento GIRO fuori
223000070305      *   per poi poter totalizzare x Giro all'interno della Rout.RIGA_Giro
223100070305      *   richiamata anche in seguito.
223200070305     c                   exsr      Legge_rec
223300940128     C*
2234000702281    C     $EFILE        DowEQ     *OFF
223500070228     C*
223600070228     C*  pulizia totali x Giro
223700070228     c                   movel     argCGI        savARGCGI
223800070228     c                   z-add     0             totSPE_cgi
223900070228     c                   z-add     0             totPKL_cgi
224000070228     c                   z-add     0             totVLU_cgi
224100070228     c                   z-add     0             totCOL_cgi
224200070228      *
224300070228      * Finchè il giro è lo stesso
2244000702281    C     $EFILE        DowEQ     *OFF
2245000702281    C     argCGI        andeq     savARGCGI
224600070228      *
224700070228      *   somma ai totali del Giro
224800070228     c                   add       1             totSPE_cgi
224900100505      *
225000070302      *  Pesi Volumi Colli da consegnare
225100070305     c                   exsr      Cal_PesVol
225200070302      *   i pesi/volumi/Colli Reali da consegnare
225300070302     c                   add       D27PKLDCO     totPKL_cgi
225400070302     c                   add       D27VLUDCO     totVLU_cgi
225500070302     c                   add       D27NCLDCO     totCOL_cgi
225600070228      *
225700070228      *  Esce da qui con un record di Totale x GIRO
225800070228     c                   exsr      Legge_rec
225900070228     c                   EndDO
226000070305      *---------
226100070305      *  Scrive la riga di SFL a rottura di giro
226200070305     C                   clear                   FDG0S01
226300070305     C* codice Giro
226400070305     c                   movel     savARGCGI     s1CGI
226500070320     c                   move      'S'           Write_riga        1
226600070305     C* Descrizione Giro
226700070305     c     kgrg          setll     figrg03l
226800070305     c                   do        *hival
226900070305     c     kgrg          reade     figrg03l
227000070305     c                   if        %eof(figrg03l)
227100070305     c                   leave
227200070305     c                   endif
227300070305     c                   if        dateu8 >= grgdde and dateu8 <= grgdscad
227400070305     c                   movel     grgdes        s1DES
227500070320     c                   movel     grgflo        dgrgFLO
227600070320      * se non deve andare in distinta
227700070326      * ?lo deve comunque caricare e controllare in seguito per non
227800070326      * ?renderlo disponibile alla selezione.
227900070326     c                   if        §grgDST = 'N'
228000070326     c*******            move      'N'           Write_riga        1
228100070326     c                   move      §grgDST       H1inDIS
228200070326     c                   end
228300070305     c                   endif
228400070305     c                   enddo
228500070305     C* Totali spedizioni
228600070305     c                   z-add     totSPE_cgi    s1tNSP
228700070305     c                   z-add     totPKL_cgi    s1tPKL
228800070305     c                   z-add     totVLU_cgi    s1tVLU
228900070305     c                   z-add     totCOL_cgi    s1tCOL
229000070305     c*
229100070305     c* Se selezionato un solo giro viene impostato automaticamente
229200070305     c*  la selezione della riga con il SFLNXTCHG acceso
229300070305     c                   if        d1cCGI <> *blank
229400070305     c                   eval      s1SCE = '1'
229500070305     c                   seton                                        32
229600070305     c                   end
229700070305     C*
229800070320     c                   if        Write_riga = 'S'
229900070305     C                   ADD       1             S1NRR
230000070305     C                   ADD       1             Y
230100070305     C                   WRITE     FDG0S01
230200070320     c                   end
230300070305      *---------
230400070228     c                   EndDO
230500070228     C*
230600070228     C                   ENDSR
230700070301      *---------------------------------------------------------------*
230800070320      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
230900070301      *---------------------------------------------------------------*
231000070228     C     LEGGE_REC     BEGSR
231100070228     C*
231200070228     C*  Per EoF deve uscire
231300940131     C                   MOVEL     *OFF          $EFILE
231400070228     C*  Per Record Ok deve uscire riportando i dati
231500070228     C                   MOVEL     *OFF          $RCDOK
231600070228     C*
2317009401311    C     $EFILE        DOUEQ     *ON
231800940131     C     $RCDOK        OREQ      *ON
231900070403     C*
232000070403      * ? Se presente il giro
232100070403     c                   IF        con_GIRO = 'S'
232200070403     c     k52j          reade     fiarg52j
232300070403     c                   ELSE
232400070403      * ? Se NON presente il giro
232500070403     c     wfgs          reade     fiarg52j
232600070403     c                   END
232700070403      * Fine File
232800070403     C                   IF        %Eof(fiarg52j)
232900070228     C                   MOVEL     *ON           $EFILE
233000070228     C                   MOVE      $EFILE        *IN33
233100070403      *
233200070403     C                   else
233300070403      *
233400070419     c                   if        (test_Parallelo = 'S' and argNDC = 0) or
233500070419     c                             (test_Parallelo <>'S' and arbNDC = 0)
233600070404      *
233700070228      * ?Controlla se la Spedizione è da considerare
233800070228     C                   exsr      Check_Sped
233900070228      * ?     se Non è Esclusa dalla consegna
234000070228     c                   if        d16conso <> 'N'
234100070228     C                   MOVE      *ON           $RCDOK
234200070228     c                   end
234300100505      *
234400130424      * ? Cotrolla inoltre solo in immissione
234500130424      * ?   se richiesto di vedere o meno le bolle con Particolarità di attesa
234600100505      * ?   e consegna richiesta NON raggiunta.
234700130424     c                   clear                   in_attesa
234800100505     c                   if        D1pATT = 'N'
234900100505     c                   exsr      ChK_attesa
235000100505     c                   if           in_attesa = 'S'
235100100505     C                   MOVE      *Off          $RCDOK
235200100505     C                   END
235300100505     c                   end
235400160429      * ? Cotrolla inoltre solo in immissione
235500160429      * ?   se richiesto di vedere o meno le bolle con zona del turno sceltosa
235600160510     c                   if        *in10 and d1sce = Assegnare
235700160510     c                   if          d1stur= 'S' and
235800160510     c                              (d1tur1 ='1' or d1tur2 ='1' or
235900160510     c                               d1tur1 ='2' or d1tur2 ='2')
236000160429     c                   exsr      ChK_turno
236100160510     c* se scelto solo parcel escludo anche messaggerie
236200160510     c                   if        turno = ' '
236300160510     c                   if            d1tdis = 'P'
236400160502     C                   MOVE      *Off          $RCDOK
236500160510     c                   end
236600160510     c                   else
236700160502     C*
236800160510     c                   if        (d1tur1 = turno or turno = d1tur2)
236900160510     c                   else
237000160429     C                   MOVE      *Off          $RCDOK
237100160429     C                   END
237200160429     C                   END
237300160510     c                   end
237400160510     C                   endif
237500160429     c*
237600130424     C                   end
237700070403     C                   END
237800070228     C*
2379009401311-   C                   ENDDO
238000940131     C*
238100940131     C                   ENDSR
238200070301      *---------------------------------------------------------------*
238300070316      *  ?  Seleziona Tutti i record del SFL                         ?
238400070301      *---------------------------------------------------------------*
238500070307     C     F01_Tutti1    BEGSR
238600070305     C*
238700070314     C     WMAX          IFgt      0
238800070305     C*  Seleziona tutte le righe del SFL accendendo il SFLNXTCHG
238900070305     c                   z-add     1             nrr_s1
239000070305     c     nrr_s1        chain     FDG0S01                            21
239100070305     c                   dow       *in21 = *Off
239200070305     c                   move      '1'           s1sce
239300070305     C*
239400070305      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
239500070305     C                   MOVE      *ON           *IN32
239600070305     C                   UPDATE    FDG0S01
239700070305     C*
239800070305     c                   add       1             nrr_s1
239900070305     c     nrr_s1        chain     FDG0S01                            21
240000070305     c                   endDO
240100070314     c                   end
240200070305     C*
240300070305     C                   ENDSR
240400070305      *---------------------------------------------------------------*
240500070320      *?   CONTROLLO TESTATA LISTA                                   ?
240600070305      *---------------------------------------------------------------*
240700070305     C     CTRC1         BEGSR
240800070301     C                   MOVE      *OFF          *IN99
240900070301     C                   ENDSR
241000070301      *---------------------------------------------------------------*
241100070320      *?   CONTROLLO OPZIONI LISTA                                   ?
241200070301      *---------------------------------------------------------------*
241300070301     C     CTRS1         BEGSR
241400070301     C*
241500070301     C*  Pulizia campi Totali
241600070301     c                   z-add     0             totSPE_cgi
241700070301     c                   z-add     0             totPKL_cgi
241800070301     c                   z-add     0             totVLU_cgi
241900070301     c                   z-add     0             totCOL_cgi
242000070302     C*
242100070302     C*  Pulizia campi Totali
242200070302     c                   z-add     0             totSPE_s1
242300070302     c                   z-add     0             totPKL_s1
242400070302     c                   z-add     0             totVLU_s1
242500070302     c                   z-add     0             totCOL_s1
242600070301     C*
242700070302     C                   z-add     0             Conta_Giri        3 0
242800070301     C                   MOVEL     *OFF          $ESCI
242900071129     c                   clear                   ErrMsgZ
243000160322     c                   clear                   un_VEDI_Pacco     1
243100070301     C                   SETOFF                                       99
243200070301     C*
243300070301     C* Leggo il sfl solo se ci sono rcd
2434000703011    C     WMAX          IFGT      0
243500070301     C                   READC     FDG0S01                                21
243600070301     C*
243700070301     C* esce se fine sfl o errore che richiede l'uscita
2438000703012    C     *IN21         DOWEQ     *OFF
243900070301     C     $ESCI         ANDEQ     *OFF
244000070301     C                   Z-ADD     S1NRR         C1RCD
244100070301     C* ctrl su riga
244200070301     C                   EXSR      RECS1
244300070301      *
244400070301     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
2445000703013    C     *IN99         IFEQ      *ON
244600070301     C     $ESCI         OREQ      *ON
244700070301     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
244800070301     C* la ripristinerò a conclusione del ciclo di READC
244900070301     C                   MOVE      *OFF          $INZS1
2450000703013-   C                   ENDIF
245100070301     C*
245200070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
245300070301     C                   MOVE      *ON           *IN32
245400070301     C                   UPDATE    FDG0S01
245500070301     C*
245600070301     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
2457000703013    C     $ESCI         IFEQ      *OFF
245800070301     C                   READC     FDG0S01                                21
245900070301      *
246000070301     C* a fine ciclo ripristino il flag richiesta iniz.sfl
2461000703014    C     *IN21         IFEQ      *ON
246200070301     C* calcolo pagina a cui deve posizionarsi
246300070301     c                   z-add     s1pag         Wpagine
246400070301     C                   EXSR      CLCPAG
2465000703014-   C                   ENDIF
2466000703013-   C                   ENDIF
246700070301     C*
2468000703012-   C                   ENDDO
246900070301     C*
2470000703011-   C                   ENDIF
247100070301     C*
247200070302     C*  Imposta i Totali come se avesse eseguito il 2°SFL
247300070302     c                   z-add     totSPE_s1     totSPE_s2
247400070302     c                   z-add     totPKL_s1     totPKL_s2
247500070302     c                   z-add     totVLU_s1     totVLU_s2
247600070302     c                   z-add     totCOL_s1     totCOL_s2
247700070302     C*
247800070327     C*  Controlli generali sulle scelte fatte
247900070328     c                   if        ErrMsgZ = *blank
248000070327     C*
248100070327     C* Se non è stata scelto  nemmeno un giro... errore
248200070327     c                   clear                   err001
248300070327     c                   clear                   err002
248400070327     c                   clear                   err003
248500070327      *
248600071129     c                   if        Conta_Giri = 0
248700071129     c                   if        not *in99
248800070327     c                   movel     ErrMsg(19)    err001
248900070327     c                   movel     ErrMsg(20)    err002
249000070327     c                   clear                   err003
249100071129     c                   end
249200070327     c                   else
249300070327     C* Se non è stata scelta  nemmeno una distinta... errore
249400070327     c                   if        h1ndc_piu = 0
249500070328     c                   if        $tasto <> F04 and $tasto <> F05 and
249600070419     c                             $tasto <> Enter                              ex F9
249700070327      * emette messaggio di Avviso x scegliere una Distinta se non c'è
249800070327     c                   movel     ErrMsg(17)    err001
249900070327     c                   clear                   err002
250000070327     c                   movel     ErrMsg(18)    err003
250100070327     c                   end
250200070327     c                   end
250300070327     c                   end
250400070327     C*
250500160318     C*  Controlla se il GIRO scelto era Fittizio e c'è invece una Distinta
250600160318     C*    di un Autista NON Fittizio
250700160322     c                   if        h1ndc_piu > 0 and h1pdd_piu = ' '
250800160322     c                             and un_vedi_PACCO ='S'
250900160318      * emette messaggio di Avviso x aver scelto un GIRO fittizio e una Distinta
251000160318     C*  non appartenente ad un AUTISTA Fittizio
251100160318     c                   clear                   err001
251200160318     c                   movel     ErrMsg(37)    err002
251300160318     c                   clear                   err003
251400160318     c                   end
251500160318     C*
251600070327     C*  Messaggio di avviso
251700070327     c                   if        err001 <> *blank or
251800070327     c                             err002 <> *blank or
251900070327     c                             err003 <> *blank
252000070327     c                   seton                                        99
252100070327      * ?            *-----------------------*
252200070327     C                   exfmt     FDG0WER
252300070327      * ?            *-----------------------*
252400070327     c                   end
252500070327     C*
252600070328     c                   end
252700070328     C*
252800070301     C                   ENDSR
252900070301     C/EJECT
253000070301      *---------------------------------------------------------------*
253100070320      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
253200070301      *---------------------------------------------------------------*
253300070301     C     RECS1         BEGSR
253400070301     C*
253500070301     C* reset indicatori DSPATR
253600070308     c                   exsr      INZ_indERR
253700070326     C*
253800070326     c* se è stato scelto ed è un giro che non può essere messo
253900070326     C*  in distinta --> messaggio di errore
254000070326     c                   If        s1SCE = '1' and H1inDIS = 'N'
254100160318     c                               or
254200160322     c                             s1SCE = '1' and  s1CGI = GIRO_vd_Pacco
254300160322     c                               and h1pdd_PIU <>'S' and h1ndc_PIU > 0
254400160322     C*
254500160322     C                   seton                                        9956
254600160322     c                   eval      un_VEDI_Pacco = 'S'
254700160322     C*
254800070326     c                   enD
254900070327     C*
255000070327     C* gestione opzioni
2551000703273    C     S1sce         IFNE      *blank
255200070327     C     *IN99         ANDEQ     *OFF
255300070327     C                   EXSR      SCE_S1
2554000703273-   C                   ENDIF
255500070301     C*
255600070301     C                   ENDSR
255700070301      *---------------------------------------------------------------*
255800070320      *?   GESTIONE OPZIONI LISTA                                    ?
255900070301      *---------------------------------------------------------------*
256000070301     C     SCE_S1        BEGSR
256100070301     C*
256200070301     C*  Totalizza le Spedizioni/Pesi/Volumi
256300070301     c                   If        s1SCE = '1'
256400070302     C                   ADD       1             Conta_Giri
256500070313     c                   move      s1cgi         sav_sxcgi
256600070326     c                   move      s1nrr         sav_s1nrr
256700070301     C*
256800070301     c                   add       s1Tnsp        totSPE_s1
256900070301     c                   add       s1Tpkl        totPKL_s1
257000070301     c                   add       s1Tvlu        totVLU_s1
257100070301     c                   add       s1Tcol        totCOL_s1
257200070301     c                   endIf
257300070301     C*
257400070301     C                   ENDSR
257500070301     C/EJECT
257600070301      *---------------------------------------------------------------*
257700070316      *  ?  GESTIONE SFL Bolle da selezionare                        ?
257800070301      *---------------------------------------------------------------*
257900070301     C     GESS2         BEGSR
258000070301     C*
258100070301     C* inizializzazione videata
258200070301     C     $INZS2        IFEQ      *ON
258300070301     C                   EXSR      INZS2
258400070301     C                   MOVE      *OFF          $INZS2
258500070301     C                   ENDIF
258600070306     C*
258700070326     C* Se non si sta eseguendo l'Assegnazione oppure se siamo in Assegnazione
258800070306     C* ed è stato richiesto di selezionare le Bolle dei Giri selezionati
258900070531     c                   if        $VISS2 = *On or D1sce <> Assegnare
259000070306     C*
259100070306     C* Inizializza e imposta piede videata
259200070306     C                   EXSR      INZZ2
259300070305     C*
259400070301     C* emissione piede videata
259500070302      * ?            *-----------------------*
259600070301     C                   WRITE     FDG0Z02
259700070302      * ?            *-----------------------*
259800070301     C*
259900070301     C     WMAX          IFEQ      0
260000070301     C* Non ci sono records
260100070302      * ?            *-----------------------*
260200070301     C                   WRITE     FDG0V02
260300070302      * ?            *-----------------------*
260400070301     c                   setoff                                       30
260500070301     C                   Else
260600070301     c                   seton                                        30
260700070301      *
260800070515     c                   If        posizionamento <> 'S'
260900070515     c                   If        posizionato =  'S'
261000070515     c                   z-add     C2RCD_pos     c2rcd
261100070515     c                   else
261200070301     C     Wsfl          IFgt      0
261300070503      * essendo il posizionamento *TOP non è + corretto impostare direttamente
261400070503      * il record ma occorre ricavare il primo record della pagina.
261500070503     c                   z-add     0             c2rcd
261600071121      *
261700070503     c                   if        wrigsfl2 > 0
261800070503     c     wsfl          div       wrigsfl2      C2RCD
261900071121     c                   mvr                     resto
262000071121     c                   if        c2rcd > 0
262100070503     c                   mult      wrigsfl2      C2RCD
262200071121     c                   if        resto > 0
262300071121     c                   add       1             C2RCD
262400071121     c                   end
262500071121     c                   else
262600071121     c                   z-add     1             C2RCD
262700071121     c                   end
262800071121     c                   end
262900071121      *
263000071121      * attenzione all'incremento di 1 può far schiantare il sfl
263100071121      * poichè può superare il nr.max delle righe presenti.
263200071121      * esempio se wsfl = 16 o un suo multiplo essendo wrigsfl2 = 16
263300071121      * c2rcd alla fine equivale a wsfl ossia 16 ....e se in seguito
263400071121      * si aggiunge 1 al sflrcdnbr andiamo fuori limite max record
263500071121      * caricati e si schianta in emissione del sfl.
263600071121     c                   if        resto = 0
263700071121     c                   eval      C2RCD = c2rcd - (wrigsfl2 - 1)
263800071121     c                   else
263900071121     c                   if        c2rcd = 0
264000070503     c                   add       1             c2rcd
264100071121     c                   end
264200071121     c                   end
264300071121      ****
264400070301     C                   Else
264500070301     C     Wpag          IFgt      0
264600070301     C                   Z-ADD     wpag          C2RCD
264700070301     C                   EndIF
264800070301     C                   EndIF
264900070301     C                   ENDIF
265000070427     C                   End
265100070515     C                   End
265200100505      *
265300100505      *  spegne il Pro-memoria se premuto il tasto F02
265400100505     c                   eval      Premuto_F02 = '0'
265500070301      *
265600070301      *  Emissione SFL delle BOLLE
265700070301      * ?            *-----------------------*
265800070301     C                   EXFMT     FDG0C02
265900070301      * ?            *-----------------------*
266000070326     c                   setoff                                       99
266100070305     C                   movel     'S2'          sav$GEST
266200070301      *
266300070301     C     C2NRR         IFNE      0
266400070301     C                   Z-ADD     C2NRR         WSFL
266500070301     C                   ENDIF
266600070301     C                   Z-ADD     SFLNRR        C2RCD
266700070515     C                   Z-ADD     SFLNRR        C2RCD_pos
266800070306      ***
266900070306     c                   EndIF
267000070301     C* Selezioni
2671000703011    C                   SELECT
267200070301     C* F3=Fine
267300070301     C     $TASTO        WHENEQ    F03
267400070301     C                   EXSR      F03_exit
267500070319     c                   rolbk
267600070307      *
267700070307     C* F1=Seleziona Tutti
267800070307     C                   WHEN      $TASTO   =   F01
267900070307     C                   EXSR      F01_Tutti2
268000070307      *
268100070301     C* F5=Int.tutte le Distinte
268200070301     C     $TASTO        WHENEQ    F05
268300070301     C                   EXSR      F05_interr
268400070426      *
268500070427     C* F07= Ordinamento x Peso
268600070427     C     $TASTO        WHENEQ    F07
268700070427     c                   z-add     6             WrkSort
268800070515     c                   exsr      Clear_Ordina
268900070427     C                   EXSR      Ordina_SFL2
269000070301      *
269100070427     C* F08= Ordinamento x Volume
269200070427     C     $TASTO        WHENEQ    F08
269300070427     c                   z-add     5             WrkSort
269400070515     c                   exsr      Clear_Ordina
269500070427     C                   EXSR      Ordina_SFL2
269600070427      *
269700070427     C* F10= Ordinamento x Località
269800070427     C     $TASTO        WHENEQ    F10
269900070427     c                   z-add     4             WrkSort
270000070515     c                   exsr      Clear_Ordina
270100070427     C                   EXSR      Ordina_SFL2
270200070427      *
270300070427     C* F11= Ordinamento x Indirizzo
270400070427     C     $TASTO        WHENEQ    F11
270500070427     c                   z-add     3             WrkSort
270600070515     c                   exsr      Clear_Ordina
270700070427     C                   EXSR      Ordina_SFL2
270800070427      *
270900070427     C* F13= Ordinamento x Destinatario
271000070427     C     $TASTO        WHENEQ    F13
271100070427     c                   z-add     2             WrkSort
271200070515     c                   exsr      Clear_Ordina
271300070427     C                   EXSR      Ordina_SFL2
271400070427      *
271500070427     C* F14= Ordinamento x Giro/Destinatario
271600070427     C     $TASTO        WHENEQ    F14
271700070427     c                   z-add     1             WrkSort
271800070515     c                   exsr      Clear_Ordina
271900070427     C                   EXSR      Ordina_SFL2
272000070427      *
272100070301     C* F12=Ritorno
272200070301     C     $TASTO        WHENEQ    F12
272300070308     c                   exsr      INZ_indERR
272400070531     c                   if        d1sce = Assegnare
272500070305     c                   move      'S1'          $GEST
272600070305     c                   z-add     C1RCD         wsfl
272700070305     c                   else
272800070619     c*****              move      'D1'          $GEST
272900070619     C*****              MOVE      *ON           $INZD1
273000070619      *  se togliere o trasferire
273100080610      *   voglio riportare direttamente sull'interrogazione distinte
273200080610      *? ma se richiamato dal Monitor e si deve tornare alla scelta
273300080610      *? sulla distinta.
273400080610     c                   if        PGM_chiamante = 'MONITOR'
273500080610     c                              and d1sce <> Togliere
273600080610     c                   eval      d1sce = Togliere
273700080610     c                   end
273800080610      *
273900070619     c                   eval      salva_d1sce = d1sce
274000070619     c                   eval      salta_video1 = 'S'
274100070619     C                   movel     'D1'          $GEST
274200070619     C                   MOVE      *Off          $INZD1
274300070619     c                   exsr      INZD1
274400070619     c                   eval      d1sce = salva_d1sce
274500070619      *
274600070316     c                   rolbk
274700070305     c                   end
274800070301      *
2749000703011O   C                   OTHER
275000070301     C* CONTROLLO testata
275100070301     C                   EXSR      CTRC2
275200070301     C* CONTROLLO subfile
275300070301     C     *IN99         IFEQ      *OFF
275400070301     C                   EXSR      CTRS2
275500070301     C                   END
275600070301      *
275700070305     C* F4=Int.Distinte Stesso Giro
275800070305     C     *IN99         IFEQ      *OFF
275900070305     C     $TASTO        andeq     F04
276000070305     C                   if        giri_scelti = 1
276100070305     C                   EXSR      F04_interr
276200070305     c                   else
276300070328      * errore x + Giri con F4 (x un solo giro)
276400070305     c                   seton                                        51  99
276500070305     c                   end
276600070305     c                   endIF
276700070305      *
276800070301      * ?se NON premuto F9 e immessa una distinta,
276900070301      * ? emette la finestra dei totali Distinta
277000070328      * ?  se non ci sono stati errori precdentemente
277100070328      * ?   e se è stato ripulito il messaggi degli errori precedenti
277200070328      * ?  se il sfl è carico.
277300080609      * ?   e se non è stata eseguita nessuna opz"8" di chiudi INFO
277400070328     C     *IN99         IFEQ      *OFF
277500070328     c     ErrMsgZ       andEQ     *blank
277600070328     C     WMAX          andGT     0
277700101130     c     exec_opzINFO  andeq     *blank
277800070326      *
277900070319     c     h1ndc_piu     ifgt      0
278000070531     c     D1SCE         andeq     Assegnare
278100070305     c     totSPE_s2     andgt     0
278200070326      *
278300070531     c     D1SCE         oreq      Togliere
278400070306     c     totSPE_s2     andgt     0
278500070326      *
278600070531     c     D1SCE         oreq      Trasferire
278700070326     c     totSPE_s2     andgt     0
278800070326     c     h1ndc_piu     andgt     0
278900070305     C                   MOVE      *ON           $INZW1
279000070302     C                   movel     'W1'          $GEST
279100070302     c                   else
279200070302      * emette messaggio di Avviso x scegliere una Distinta se non c'è
279300070302      * se si era scelta l'opzione di assegnazione e non si era immessa
279400070302      * una distinta
279500070305     c                   if        totSPE_s2 = 0
279600070326     c                   movel     ErrMsg(19)    err001
279700070326     c                   movel     ErrMsg(20)    err002
279800070319     c                   clear                   err003
279900070305     c                   else
280000070326     c                   movel     ErrMsg(17)    err001
280100070319     c                   clear                   err002
280200070326     c                   movel     ErrMsg(18)    err003
280300070305     c                   end
280400070302      * ?            *-----------------------*
280500070305     C                   exfmt     FDG0WER
280600070302      * ?            *-----------------------*
280700070302     c                   end
280800070305      *
280900070305     c                   end
281000070301      *
2811000703011-   C                   ENDSL
281200070301     C*
281300070301     C                   ENDSR
281400070306     C/EJECT
281500070306      *---------------------------------------------------------------*
281600070320      *?   INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL               ?
281700070306      *---------------------------------------------------------------*
281800070306     C     INZZ2         BEGSR
281900070306     C*
282000070306     C* Messaggio o tasti funzionali
282100070306     c                   clear                   z2dmsg
282200070328     c                   clear                   ErrmsgZ          80
282300100505     c                   move      opz_errata    *in65
282400100505     c                   move      da_forzare    *in66
282500100505      *
282600100505      * Se si deve forzare cambia anche le opzioni del SFL in testata
282700100505     c                   setoff                                       18
282800100505     c                   if        da_forzare   = '1'  and
282900100505     c                             d1SCE = Assegnare
283000100505     c                   setoN                                        18
283100100505     C                   End
283200070307     C*
283300070306      * errore di selezione di + giri x richiesta interr.stesso giro
283400070306     c                   if        *in51 = *on
283500070328     c                   movel     ErrMsg(12)    ErrmsgZ
283600070306     c                   setoff                                       51
283700070306     c                   end
283800070319     c                   if        *in52 or *in54
283900070328     c                   movel     ErrMsg(13)    ErrmsgZ
284000070326     c                   setoff                                       5254
284100070306     c                   end
284200070326     c                   if        *in62
284300070328     c                   movel     ErrMsg(15)    ErrmsgZ
284400070326     c                   setoff                                       62
284500070319     c                   end
284600080609     c                   if        *in65
284700160317     c                   if        Sped_DPD = 'S'
284800160317     c                   movel     ErrMsg(36)    ErrmsgZ
284900160317     c                   else
285000080609     c                   movel     ErrMsg(33)    ErrmsgZ
285100160317     c                   end
285200080609     c                   setoff                                       65
285300100505     C                   move      '0'           opz_errata
285400080609     c                   end
285500130424     c                   if        *in66
285600150930     c                   if         in66_msg = 'A'
285700100505     c                   movel     ErrMsg(34)    ErrmsgZ
285800150930     c                   elseif     in66_msg = 'C'
285900150930     c                   movel     ErrMsg(35)    ErrmsgZ
286000160429     c                   elseif     in66_msg = 'Z'
286100160429     c                   movel     ErrMsg(39)    ErrmsgZ
286200160429     c                   end
286300100505     c                   end
286400070319     c                   if        *in53 or *in55
286500070328     c                   movel     ErrMsg(14)    ErrmsgZ
286600070319     c                   setoff                                       5355
286700070306     c                   end
286800070413     c                   if        *in59
286900070413     c                   movel     ErrMsg(29)    ErrmsgZ
287000070413     c                   setoff                                       59
287100070413     c                   clear                   h1ndc_piu
287200160321     c                   clear                   h1pdd_piu
287300070413     c                   end
287400160321     c                   if        *in58
287500160322     c                   movel     ErrMsg(38)    ErrmsgZ
287600160321     c                   setoff                                       58
287700160321     c                   clear                   h1ndc_piu
287800160321     c                   clear                   h1pdd_piu
287900160321     c                   end
288000141113     c                   if        o982esito ='1'
288100141113     c                   movel     o982msgerr    ErrmsgZ
288200141113     c                   end
288300070328      *
288400070328      * imposta eventuali errori
288500070328     c                   eval      z2dmsg = ErrMsgZ
288600070307     C*
288700070307      * tasti funzionali se non è stato emesso alcun messaggio
288800070328     c                   if        ErrMsgZ = *blank
288900070307     c                   setoff                                       99
289000070427     c                   end
289100070426      *
289200070307      * se è stato scelto l'assegnazione (1) e non è stato immessa la distinta
289300070531     c                   if        d1sce = Assegnare or d1sce = Trasferire
289400070427     c                   setoff                                       44
289500070426     c                   else
289600070427     c                   seton                                        44
289700070307     c                   end
289800070306     C*
289900070306     C                   ENDSR
290000070301     C/EJECT
290100070301      *---------------------------------------------------------------*
290200070320      *?  INIZIALIZZAZIONE LISTA Bolle da Giri o da Distinta         ?
290300070301      *---------------------------------------------------------------*
290400070301     C     INZS2         BEGSR
290500070301     C*
290600070301     C* reset indicatori DSPATR
290700070308     c                   exsr      INZ_indERR
290800100505     C                   move      '0'           opz_errata        1
290900100505     C                   move      '0'           da_forzare        1
291000070301     C* pulizia SFL
291100070301     C                   SETOFF                                         3031
291200070301     C                   WRITE     FDG0C02
291300070301     C                   SETON                                          31
291400070301     C*
291500070301     C* Righe x Pagina del 1° SFL
291600070427     c                   z-add     RigPag_SFl2   s2pag
291700070420      *
291800070427     C                   EVAL      WrkSort  = Ord_GIRODEST
291900070427     c                   eval      wINDsort = Ord_GIRODEST
292000070301      *
292100070301     C* CARICAMENTO SFL totale
292200070301     C                   Z-ADD     0             S2NRR
292300070301     C                   Z-ADD     1             C2RCD
292400070301     C                   Z-ADD     0             WMAX
292500070302     C                   Z-ADD     0             Wsfl
292600070302     C* campi titolo
292700070302     C                   MOVEL     dsPGM         c2CPGM
292800070302     C                   MOVEL     knsif         c2nsif
292900070302     C                   MOVEL     knmus         c2nmus
293000070302     C                   MOVEL     RSUT          c2crsu
293100160506     c                   if        §ogturno='S'
293200160506     C                   MOVEL     'T'           c2tur
293300160506     c                   end
293400070427     C                   clear                   c2pos1
293500070427     C                   clear                   c2pos2
293600070427     C                   clear                   c2pos3
293700070427     C                   clear                   c2pos4
293800070427     C                   clear                   c2pos5
293900070427     C                   clear                   c2pos6
294000070427     C                   clear                   sav_pos1
294100070427     C                   clear                   sav_pos2
294200070427     C                   clear                   sav_pos3
294300070427     C                   clear                   sav_pos4
294400070427     C                   clear                   sav_pos5
294500070427     C                   clear                   sav_pos6
294600070302      * TIPO FUNZIONE
294700070302     C                   SELECT
294800070531     C                   when      d1sce = Assegnare
294900070302     c                   eval      C2dsce = OPZ_1
295000070306     c                   eval      C2dess = 'alla Distinta nr.' +
295100070319     c                             %trim(%editc(h1ndc_piu:'Z'))
295200070306      *
295300070531     C                   when      d1sce = Togliere
295400070302     c                   eval      C2dsce = OPZ_2
295500070306     c                   eval      C2dess = 'dalla Distinta nr.' +
295600070319     c                             %trim(%editc(h1ndc_meno:'Z'))
295700070306      *
295800070531     C                   when      d1sce = Trasferire
295900070306     c                   eval      C2dsce = OPZ_3
296000070306     c                   eval      C2dess = 'dalla Distinta nr.' +
296100070319     c                             %trim(%editc(h1ndc_meno:'Z'))
296200070306      *
296300070302     c                   endSL
296400070301      *
296500070301      *   Carica il SFL
296600070301     c                   EXSR      ROLS2
296700070301     C*
296800070301     C                   Z-ADD     1             WPAG
296900070301     C*
297000070301     C                   ENDSR
297100070301      *---------------------------------------------------------------*
297200070320      *?  CARICAMENTO PAGINA da SFL dei GIRI                         ?
297300070301      *---------------------------------------------------------------*
297400070301     C     ROLS2         BEGSR
297500070301     C*
297600070301     C                   SETOFF                                       32
297700070301     C                   Z-ADD     0             Y
297800070301     C                   Z-ADD     WMAX          S2NRR
297900070301     C*
298000070301      * ? Se Scelto Caricamento x Assegnazione
298100070531     c                   if        d1sce = Assegnare
298200070301      * ?      carica da SFL1
298300070301     c                   exsr      CAR_da_SFL1
298400070301     c                   else
298500070301      * ?      carica da Numero Distinta
298600070301     c                   exsr      CAR_da_NDC
298700070301     c                   end
298800070301     C* Fine Caricamento
298900070301     C                   Z-ADD     S2NRR         WMAX                 30
299000070419     C*
299100070419     C* se ci sono almeno 2 records da ordinare fra loro
299200070419     c                   if        wmax > 1
299300070419      * ?   Esegue l'Ordinamento x Giro/Spedizione
299400070515     c                   exsr      Clear_Ordina
299500070419     c                   exsr      Ordina_SFL2
299600070419     c                   end
299700070301     C*
299800070302     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
299900070301     C     S2NRR         DIV       S2pag         PAGINE            4 0
300000070301     C                   MVR                     RESTO             3 0
300100070301     C     PAGINE        MULT      S2pag         C2RCD
3002000703011    C     RESTO         IFGT      0
300300070301     C                   ADD       1             C2RCD
3004000703011E   C                   ELSE
300500070301     C                   SUB       S2pag         C2RCD
300600070301     C                   ADD       1             C2RCD
3007000703011-   C                   ENDIF
300800070301     C*
300900070301     C*
301000070301     C                   ENDSR
301100070301      *---------------------------------------------------------------*
301200070316      *  ?  Carica dal SFL dei Giri                                  ?
301300070301      *---------------------------------------------------------------*
301400070301     C     CAR_da_SFL1   BEGSR
301500070301     C*
301600070301     C*   Legge il SFL dei Giri dove selezionato con (1) le righe
301700070301     C*    dei giri
301800070302     c                   Readc     FDG0S01                                21
3019000703022    C     *IN21         DOWEQ     *OFF
302000070301     C*
302100070301     C* Solo dai Giri selezionati
302200070301     c                   if        s1SCE = '1'
302300070404      *
302400070404      * ? Se ASSEGNA: caricamento da SFL dei giri
302500070404      * ? ----------------------------
302600070404     c     k52j1         setll     fiarg52j
302700070302      * ? ----------------------------
302800070301      * ? Carico il SFL
302900070301      * ?     leggendo le bolle valide
303000070301     c                   exsr      LEGGE_Bolle
303100070301     C*
303200070301     C* Leggo dal file per caricare la lista
3033000703011    C     $EFILE        DOWEQ     *OFF
303400070301     C*
303500070301     C                   clear                   FDG0S02
303600070301     C* codice Giro
303700070404     c                   movel     argCGI        s2CGI
303800070301      *
303900070301     c                   exsr      RIGA_SFL2
304000070306      *
304100070306      * Se si è scelto di non visualizzare le Bolle comunque le carico
304200070306      *  tutte selezionate con SFLNXTCHG x la routine di aggiornamento
304300070306      *   finale.
304400070607     c                   if        $VISS2 = *Off
304500070607     c                             or s2CGI = fdgCGI and Sel_xGiro = 'S' and
304600070607     c                             (fdgopz=Trasferire or fdgopz=Togliere)
304700070306     c                   move      '1'           s2sce
304800070306     c                   seton                                        32
304900070306     c                   end
305000070305      *
305100070301     C                   ADD       1             S2NRR
305200070503     C                   EVAL      RrnLast = S2nrr
305300070301     C                   ADD       1             Y
305400101130     c                   eval      *in45 = H2in45
305500130221     c     h2xco         comp      '0'                                88
305600130424     c                   eval      h2att = in_attesa
305700070301     C                   WRITE     FDG0S02
305800070301     C*
305900070301      * Lettura di un record
306000070301     c                   exsr      LEGGE_Bolle
3061000703011-   C                   ENDDO
306200070301     C*
306300070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL dei GIRI
306400070301     C                   MOVE      *ON           *IN32
306500070301     C                   UPDATE    FDG0S01
306600070302     C*
306700070301     c                   End
306800070302      * ?CHIUSURE FILE
306900070302     c                   exsr      CLOF_S2
307000070302     C*
307100070302     c                   Readc     FDG0S01                                21
307200070301     c                   EndDo
307300070301     C*
307400070301     C                   ENDSR
307500070306      *---------------------------------------------------------------*
307600070316      *  ?  Carica da Numero Distinta                                ?
307700070306      *---------------------------------------------------------------*
307800070306     C     CAR_da_NDC    BEGSR
307900070306     C*
308000070306      * ? ----------------------------
308100080826      * ? Se TOGLIE o TRASFERISCE:  caricamento da Distinta
308200070531     c                   IF        d1sce = Togliere or d1sce = Trasferire
308300070404      *
308400070306     c* se il p.o. è in parallelo testo la distinta su ARG altrimenti
308500070306     c* se il p.o. è partito testo la distinta su ARB e ARG
308600070306     c                   if        test_Parallelo = 'S'
308700070306     C/EXEC SQL
308800070306     C+ DECLARE I1 CURSOR FOR SELECT fiarg02l.*, fnarb01l.* FROM fiarg02l
308900070306     C+ JOIN fnarb01l ON arbaas= argaas and arblnp=arglnp and arbnrs=
309000070306     C+ argnrs and arbnsp= argnsp WHERE argpoc = :d1cFGS and
309100070319     C+ argNDC=:h1ndc_meno
309200070306     C/END-EXEC
309300070306
309400070306     C/EXEC SQL
309500070306     C+ OPEN I1
309600070306     C/END-EXEC
309700070306     c                   else
309800070404      **
309900070404     c                   z-add     wfgs          wfgs_ifp
310000070404      * ? ----------------------------
310100070404     c     karb70        setll     fnarb70l
310200070404      * ? ----------------------------
310300070306     c                   end
310400070306     c                   end
310500070306      * ? ----------------------------
310600070306      * ? Carico il SFL
310700070306      * ?     leggendo le bolle valide
310800070306     c                   exsr      LEGGE_Bolle
310900070306     C*
311000070306     C* Leggo dal file per caricare la lista
3111000703061    C     $EFILE        DOWEQ     *OFF
311200070306     C*
311300070306     C                   clear                   FDG0S02
311400070306     C* codice Giro
311500070404     c                   movel     argCGI        s2CGI
311600070306      *
311700070306     c                   exsr      RIGA_SFL2
311800070607      *
311900070607     c                   if        s2CGI = fdgCGI and Sel_xGiro = 'S' and
312000071024     C                             s2CGI <> *blank and
312100070607     c                             (fdgopz=Trasferire or fdgopz=Togliere)
312200070607     c                   move      '1'           s2sce
312300070607     c                   seton                                        32
312400070607     c                   end
312500070306      *
312600070306     C                   ADD       1             S2NRR
312700070503     C                   EVAL      RrnLast = S2nrr
312800070306     C                   ADD       1             Y
312900101130     c                   eval      *in45 = H2in45
313000130221     c     h2xco         comp      '0'                                88
313100130424     c                   eval      h2att = in_attesa
313200070306     C                   WRITE     FDG0S02
313300070306     C*
313400070306      * Lettura di un record
313500070306     c                   exsr      LEGGE_Bolle
3136000703061-   C                   ENDDO
313700070306      * ?CHIUSURE FILE
313800070306     c                   exsr      CLOF_S2
313900070306     C*
314000070306     C                   ENDSR
314100070302      *---------------------------------------------------------------*
314200070320      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
314300070302      *---------------------------------------------------------------*
314400070302     C     LEGGE_Bolle   BEGSR
314500070302     C*
314600070302     C*  Per EoF deve uscire
314700070302     C                   MOVEL     *OFF          $EFILE
314800070302     C*  Per Record Ok deve uscire riportando i dati
314900070302     C                   MOVEL     *OFF          $RCDOK
315000070302     C*
3151000703021    C     $EFILE        DOUEQ     *ON
315200070302     C     $RCDOK        OREQ      *ON
315300070302     C*
315400070306      *  x Assegnazione
315500070531     c                   if        d1sce = Assegnare
315600070404      * ?LETTURE  FILE
315700070404      * ? ----------------------------
315800070404     c     k52j1         reade     fiarg52j
315900070404     C                   eval      sqlcod = 0
316000070404     c                   if        %Eof(fiarg52j)
316100070404     C                   eval      sqlcod = 100
316200070404     c                   end
316300070404      * ? ----------------------------
316400070306     c                   end
316500070306     C*
316600070404      *  x Togliere  o  Trasferire
316700070531     c                   if        d1sce = Togliere or d1sce = Trasferire
316800070306     c                   if        test_Parallelo = 'S'
316900070306      * ?LETTURE  FILE
317000070306     C/EXEC SQL
317100070306     C+ FETCH NEXT FROM i1 INTO  :fiargds, :fnarbds
317200070306     C/END-EXEC
317300070306     c                   else
317400070404      **
317500070404     c     karb70        reade     fnarb70l
317600070404      * Fine File
317700070404     C                   eval      sqlcod = 0
317800070404     c                   if        %Eof(fnarb70l)
317900070404     C                   eval      sqlcod = 100
318000070417     c                   else
318100070417     c     karg          chain(n)  fiarg01l
318200070417     c                   if        not %Found(fiarg01l)
318300070417     c                   clear                   fiargds
318400070417     c                   end
318500070417     c                   end
318600070404      **
318700070306     c                   end
318800070306     c                   end
318900070306      *
319000070302     C                   select
319100070302      * Fine File
319200070404     C                   when      sqlcod = 100
319300070302     C                   MOVEL     *ON           $EFILE
319400070302     C                   MOVE      $EFILE        *IN33
319500070302      * errori
319600070302     C                   WHEN      SqlCod < 0
319700070302     c                   if        sqlcod <> -913
319800070302     C                   seton                                        H1
319900070302     c                   end
320000070302      *
320100070302     C                   other
320200070302      *
320300070306      *  Per assegnare le bolle deve eseguire il controllo di consegnabilità
320400070531     c                   if        d1sce = Assegnare
320500070404      *
320600070404      *   Leggendo x ARG52J deve scartare le NDC > 0
320700070419     c                   if        (test_Parallelo = 'S' and argndc = 0) or
320800070419     c                             (test_Parallelo <>'S' and arbndc = 0)
320900070404      *
321000070302      * ?Controlla se la Spedizione è da considerare
321100070302     C                   exsr      Check_Sped
321200070302      * ?     se Non è Esclusa dalla consegna
321300070302     c                   if        d16conso <> 'N'
321400070302     C                   MOVE      *ON           $RCDOK
321500070302     c                   end
321600070404      *
321700070404     c                   end
321800070302      *
321900070306      * Per Togliere o Trasfererire
322000070306     c                   else
322100070306     C                   MOVE      *ON           $RCDOK
322200070306     c                   end
322300070306      *
322400070302     C                   endsl
322500100505     C*------
322600100505     C* Solo in Assegnazione:
322700100505     C*  Se non devono essere visualizzate le bolle con particolarità
322800100505     C*   di Attesa  le Filtro
322900130424     c                   clear                   in_attesa
323000130424     c                   if        d1sce = Assegnare and
323100130424     c                             D1pATT = 'N'
323200100505     c                   exsr      ChK_attesa
323300100505     c                   if           in_attesa = 'S'
323400100505     C                   MOVE      *Off          $RCDOK
323500100505     C                   END
323600100505     c                   end
323700160429     C*------
323800160429     C* Solo in Assegnazione:
323900160429     C*  Se non devono essere visualizzate le bolle con zona di un
324000160429     C*   turno diverso
324100160510     c                   if        *in10  and d1sce = Assegnare
324200160510     c                   if          d1stur = 'S'       and
324300160510     c                              (d1tur1 ='1' or d1tur2 ='1' or
324400160510     c                               d1tur1 ='2' or d1tur2 ='2')
324500160429     c                   exsr      ChK_turno
324600160510     c* se scelto solo parcel escludo anche messaggerie
324700160510     c                   if        turno = ' '
324800160510     c                   if            d1tdis = 'P'
324900160502     C                   MOVE      *Off          $RCDOK
325000160502     c                   end
325100160510     c                   else
325200160502     c*
325300160510     c                   if        (d1tur1 = turno or turno = d1tur2)
325400160510     c                   else
325500160429     C                   MOVE      *Off          $RCDOK
325600160429     C                   END
325700160429     c                   end
325800160510     c                   end
325900160510     c                   end
326000100505      *
326100100505     C*------
3262000703021-   C                   ENDDO
326300070302     C*
326400070302     C                   ENDSR
326500100505      *---------------------------------------------------------------*
326600100505      *?  Controlla se la Bolla ha Partic.di Consegna <in Attesa>    ?
326700100505      *---------------------------------------------------------------*
326800100505     C     Chk_attesa    BEGSR
326900100505     c                   if         arbGMA <> *blank and arbTCR = 'P'
327000100505     C*
327100100505     C     ARBdcr        IFGT      DATEU8
327200130429     c                   clear                   tnsd99ds
327300130424     C                   Z-ADD     dateu8        D98DFC
327400130424     C                   Z-ADD     2359          D98OFC
327500130424     C                   MOVEL     ' '           D98TLA
327600130424     C                   MOVEL     'A'           D98TBO
327700130424     C                   Z-ADD     ARBAAS        D98AAS
327800130424     C                   Z-ADD     ARBLNP        D98LNP
327900130424     C                   Z-ADD     ARBNRS        D98NRS
328000130424     C                   Z-ADD     ARBNSP        D98NSP
328100130424     C                   CALL      'TNSD99R'
328200130424     C                   PARM                    TNSD99DS
328300130424     c                   if        d98nci <=-24
328400100505     C     arbgma        lookup    s7r                                    23
328500100505     C                   IF        *in23
328600100505     C                   eval      in_attesa = 'S'
328700100505     C                   END
328800100505     C                   END
328900100505     C                   END
329000100505     C*
329100100505     C                   End
329200100505     C*
329300100505     C                   ENDSR
329400160429      *---------------------------------------------------------------*
329500160429      *?  Controlla se la Bolla ha zona del turno scelto
329600160429      *---------------------------------------------------------------*
329700160429     C     Chk_turno     BEGSR
329800160429     c                   clear                   turno             1
329900160429     c                   clear                   ds03
330000160429     C                   eval      tblcod = '03'
330100160429     C                   movel     D1CFGS        com05             5
330200160429     C                   move      arbznc        com05             5
330300160429     C                   movel(p)  com05         tblkey
330400160429     C     ktab          chain     Tabel00f
330500160429     c                   if        %found(tabel00f)
330600160429     C                   MOVEL     TBLUNI        DS03
330700160429     c                   if        §03TPM = ' '
330800160429     c                   eval      turno = §03turno
330900160429     c                   if        §03Turno = ' '
331000160429     c                   movel     '1'           turno
331100160429     c                   end
331200160429     c                   end
331300160429     c                   end
331400160429     C                   ENDSR
331500070301      *---------------------------------------------------------------*
331600070320      *?  Imposta Campi del 2°SFL                                    ?
331700070301      *---------------------------------------------------------------*
331800070301     C     RIGA_SFL2     BEGSR
331900070301     C*
332000070301     C* Salva il record di ARB per riposizionamento seguente
332100070301     c                   movel     fnarbds       fnarbdsav
332200070301     C* Dati Bolla
332300070301      *  Salva la chiave anche per riposizionarsi in seguito
332400070301     c                   z-add     ARBaas        S2Caas
332500070301     c                   z-add     ARBlnp        S2Clnp
332600070301     c                   z-add     ARBnrs        S2Cnrs
332700070301     c                   z-add     ARBnsp        S2Cnsp
332800101129     c                   z-add     ARBmgs        h2mgs
332900110804     c                   movel     ARBxco        h2xco
333000150930      *
333100150930      * Controlla
333200150930     c                   exsr      contro_alert
333300150930     c                   eval       h2Calert = inviato_contro_alert
333400150930      *
333500100505      * Se è una di quelle bolle che devono rimanere in attesa
333600130424      *   va segnalato sul sfl con "A" di attesa                            e.
333700100505     c                   clear                   s2pATT
333800100505     c                   if         arbGMA <> *blank and arbTCR = 'P'
333900100505     c                   z-add     arbDCR        h2dcr
334000100505     c                   move      arbGMA        h2gma
334100100505     c                   move      'A'           s2pATT
334200100505     c                   end
334300070301      *
334400070301     c                   movel     ARBcbo        S2Ccbo
334500070301     c                   movel     ARBrsd        S2Crsd
334600070301     c                   movel     ARBind        S2Cind
334700070301     c                   z-add     ARBlna        S2Clna
334800070301     c                   z-add     ARBznc        S2Cznc
334900070301     c                   movel     ARBgc1        S2Cgc1
335000070301     c                   movel     ARBgc2        S2Cgc2
335100070301     c                   movel     ARBcad        S2Ccad
335200070301     c                   movel     ARBlod        S2Clod
335300070301     c                   movel     ARBprd        S2Cprd
335400070301     c                   movel     ARBrsm        S2Crsm
335500070301     c                   movel     ARBtsp        S2Ctsp
335600070301     c                   z-add     ARBhcr        S2Chcr
335700070302      *  Pesi Volumi Colli da consegnare
335800070305     c                   exsr      Cal_PesVol
335900070302      *   i pesi/volumi/Colli Reali da consegnare
336000070302     c                   z-add     D27PKLDCO     S2Cpkf
336100070502     c                   move      D27PKLDCO     Decimal_uno       1 0
336200070502     c                   if        Decimal_uno > 0
336300070502     c                   add       1             S2Cpkf
336400070502     c                   end
336500070427     c                   z-add     D27PKLDCO     S2pkf
336600070302     c                   z-add     D27VLUDCO     S2Cvlf
336700070302     c                   z-add     D27NCLDCO     S2Cncl
336800070301      *
336900070301     C                   move      ARBdcr        MMGG              4 0
337000070301     C                   MOVEL     MMGG          MM                2 0
337100070301     C                   MOVE      MMGG          GG                2 0
337200070301     C                   MOVE      MM            S2Cgmc
337300070301     C                   MOVEL     GG            S2Cgmc
337400070301      * Anomalia
337500150515     c                   clear                   s2dano
337600070301     C     ARBfan        IFEQ      'S'
337700070301     C                   MOVEL     'ANOM.'       S2Dano
337800070301     C                   END
337900150515     c* se filiale gestisce richiesta pistola 88
338000150515     c                   if        V1Cp88 ='S'
338100150515     C     ARBfan        IFEQ      'S'
338200150515     C     d27ncl88o     andne     0
338300150515     C                   MOVEL     'AN/88'       S2Dano
338400150515     C                   END
338500150515     C     ARBfan        IFne      'S'
338600150515     C     d27ncl88o     andne     0
338700150515     C                   MOVEL     'Pt.88'       S2Dano
338800150515     C                   END
338900150515     C                   END
339000160429     c* turno della zona
339100160429     c                   clear                   h2forzaznc
339200160429     c                   clear                   s2turno
339300160429     c                   if        *in10
339400160429     c                   exsr      chk_turno
339500160429     c                   movel     turno         s2turno
339600160429     c                   end
339700070301      * Tipo Consegna
339800070301     C                   Z-ADD     1             XY                3 0
339900070301     C     ARBtc1        LOOKUP    C1P(XY)                                23
340000070301     C     *IN23         IFEQ      '1'
340100070301     C                   MOVEL     D1P(XY)       S2Dtc1
340200070301     C                   END
340300070301     C                   Z-ADD     1             XY
340400070301     C     ARBtc2        LOOKUP    C1P(XY)                                23
340500070301     C     *IN23         IFEQ      '1'
340600070301     C                   MOVEL     D1P(XY)       S2Dtc2
340700070301     C                   END
340800070301     C*
340900070301     C*  Controllo il tipo bolla
341000070301     C                   Z-ADD     1             X
341100070301     C                   MOVEL     *BLANKS       WTBOL1            1
341200070302     C                   MOVEL     *BLANKS       WTBOL2            1
341300070301     C                   CLEAR                   DS3A
341400070301     C     arbCBO        LOOKUP    C3A(X)                                 13
341500070301     C     *IN13         IFEQ      '1'
341600070301     C                   MOVEL     D3A(X)        DS3A
341700070301     C                   MOVEL     §3ATB1        WTBOL1
341800070301     C                   MOVEL     §3ATB2        WTBOL2
341900070301     C                   END
342000070301     C*
342100070301     C*  Campi di comodo per Bolla legata precedente:
342200070301     C                   clear                   comASS           13 3
342300070301     C                   clear                   comVCA            3
342400070301     C                   clear                   comTIC            2
342500070301     C*
342600070301      *?Controlla se esiste una bolla precedente legata ?
342700070301     C     Karb1         Chain     FnLBL01L
342800070301      *
342900070301      * > Se ho una bolla "mamma" ed è locale
343000070301     C                   if        %Found(FnLBL01L) and  arbLNA = lblLAP
343100070301      *
343200070301      * prende la bolla precedente sporcando i campi di ARB
343300070301     C     Klbl          CHAIN(n)  FnARB01L
343400070301      *
343500070301     C*  La prende in considerazione solo se non è
343600070301     C*   un Reso Mittente e un'Avaria e non è un solo incasso
343700070301     C                   IF        %Found(FnARB01L) and
343800070301     C                             arbCCA <> '2' and arbCCA <> '6' and
343900070301     C                             (arbICC = ' ' or arbICC = 'R')
344000070301      *  ?Controllo il tipo bolla mamma?
344100070301     C                   Z-ADD     1             X
344200070301     C     arbCBO        LOOKUP    C3A(X)                                 23
344300070301     C     *IN23         IFEQ      '1'
344400070301     C                   MOVEL     D3A(X)        DS3A
344500070301     C*  Se bolla mamma con C/Assegno vado a reperirlo
344600070301     C     §3AFCA        IFNE      0
344700070301     C     Klbl          Chain     FIAR901L                           56
344800070301     C                   If        %Found(FIAR901L)
344900070301     C                   Z-ADD     ar9CAS        comASS
345000070301     C                   MOVEL     ar9VCA        comVCA
345100070301     C                   MOVEL     ar9TIC        comTIC
345200070301     C                   End
345300070301     C                   END
345400070301     C                   END
345500070301     C                   END
345600070301     C*
345700070302      *  ? Reimposta i campi di ARB dell'SQL ?
345800070301     c                   movel     fnarbdsav     fnarbds
345900070301     C                   END
346000070301     C*
346100070302     c                   if        arbICC = ' ' or arbICC = 'R'
346200070302     C     §3AFCA        IfNE      0
346300070302     C                   Z-ADD     0             ar9CAS
346400070302     C                   MOVEL     *BLANKS       ar9VCA
346500070302     C     Karb1         CHAIN     FiAR901L
346600070302     c                   iF        %Found(FiAR901L)
346700070302     C                   Z-ADD     ar9CAS        s2cCAS
346800070302     C                   MOVEL     ar9VCA        s2cVCA
346900070302     C                   MOVE      ar9TIC        s2cTIC
347000070302     C                   EnD
347100070302     C                   End
347200070302      *
347300070302     C     comASS        IFNE      *ZEROS
347400070302     C                   Z-ADD     comASS        s2cCAS
347500070302     C                   MOVEL     comVCA        s2cVCA
347600070302     C                   MOVE      comTIC        s2cTIC
347700070302     C                   EnD
347800070302     C                   End
347900070302     C*
348000070301     C* Verifico se devo gestire un assegnato
348100070301     C     WTbol1        IFNE      *BLANKS
348200070301     C     WTbol2        ORNE      *BLANKS
348300070302      *
348400070301     C* Se devo gestire 2° bolla imposto tipo record '2' x posiz.FIAR6
348500070301     C     WTbol2        IFNE      *BLANKS
348600070301     C                   MOVE      '2'           KTRC
348700070301     C* .... se devo gestire 1° bolla imposto tipo record '1'
348800070301     C                   ELSE
348900070301     C     WTbol1        IFNE      'F'
349000070301     C                   MOVE      '1'           KTRC
349100070301     C                   END
349200070301     C                   END
349300070301      *
349400070301     C* Aggancio FIAR6 x importo assegnato
349500070301     C     KAR6          CHAIN     FIAR601L
349600070302     c                   if        %Found(FIAR601L) and ar6DFT <> 0
349700070301      *
349800070302     C* Se ho effettuato un ritorno all'incasso controllo se
349900070302     C     arbICA        IFEQ      'R'
350000070302     C* ... si riferisce ad un incasso parziale
350100070302     C                   CLEAR                   ariIPP
350200070302     C                   MOVEL     ar6DIV        s2cVAS
350300070302     C                   Z-ADD     ar6IFT        s2cIMA
350400070301      *
350500070302     C                   movel     'A'           KTIP
350600070302     C     Kari          setll     FiARI01L
350700070302     C     Kari          reade     FiARI01L
350800070302     C* ... se si calcolo importo da incassare x differenza
350900070302     C                   DOW       not %EoF(FiARI01L)
351000070302      *
351100070302     C     ar6DIV        ifEQ      ariDIV
351200070302     C                   SUB       ariIPP        s2cIMA
351300070302     C                   ElsE
351400070302     C* Se la divisa di incasso è diversa converto importo già
351500070302     C* incassato nella divisa della bolla
351600070302     C                   CLEAR                   YEURDS
351700070302     C                   MOVEL     ariDIV        YECDVI
351800070302     C                   Z-ADD     ariIPP        YECIMI
351900070302     C                   MOVEL     ar6DIV        YECDVO
352000070302     C                   MOVEL     'H'           YECTAR
352100070302     C                   CALL      'YEURCO'
352200070302     C                   PARM                    YEURDS
352300070302     C                   SUB       YECIMO        s2cIMA
352400070302     C                   End
352500070302      *
352600070302     C     Kari          reade     FiARI01L
352700070302     C                   END
352800070302      *
352900070302     C                   ELSE
353000070302      *
353100070302     C* ... altrimenti imposto totale fattura
353200070302     C     arbICA        ifEQ      ' '
353300070302     C     arbICA        orEQ      'R'
353400070302     C                   Z-ADD     ar6IFT        s2cIMA
353500070302     C                   MOVEL     ar6DIV        s2cVAS
353600070302     C                   End
353700070302      *
353800070302     C                   END
353900070302     C                   END
354000070301      *
354100070302     C                   ENDif
354200070301     C*
354300070302     C* Accendo indicatori x assegnato (ND)
354400070427     C                   if        s2cIMA > 0 or s2cCAS > 0
354500070427     c                   move      'S'           s2cIMP
354600070427     c                   end
354700070301     C*
354800080826      * ? Solo Se TOGLIE o TRASFERISCE:
354900080826     c                   IF        d1sce = Togliere or d1sce = Trasferire
355000080606      * ?Solo se è attiva la gestione del PDA o in test sulla filiale
355100080606     c                   if          PDAconsegne <> *blank
355200080627     C                   exsr      chk_PCT_COK
355300080826     c                   end
355400080606     c                   end
355500080606     C*
355600070301     C                   ENDSR
355700070307      *---------------------------------------------------------------*
355800080606      *  ?  Controlla se presenti delle INFO x PDA da visualizzare   ?
355900080606      *---------------------------------------------------------------*
356000080627     C     chk_PCT_COK   begSR
356100080606      *
356200080627      *  chiama la funzione passando la Bolla
356300101130     c                   eval      h2in45 = '0'
356400080606     c                   clear                   s2cCOK
356500080627     c                   clear                   fidg45ds
356600080627     c                   z-add     s2caas        C45AAS
356700080627     c                   z-add     s2clnp        C45LNP
356800080627     c                   z-add     s2cnrs        C45NRS
356900080627     c                   z-add     s2cnsp        C45NSP
357000080818     c                   z-add     h1pdr_meno    c45codaut
357100080903     c                   z-add     D1CFGS        c45FGS
357200080903     c                   z-add     h1ndc_meno    c45NDC
357300080627     c                   eval      kpjbus = kpjbu
357400080627     c                   eval      kpjbu  = fidg45ds
357500080627     c                   call      'FIDG45R'
357600080627     c                   parm                    kpjba
357700080627      ** mancava questa specifica che reimpostava la DS x caricare i 4 campi
357800080627     c                   eval      fidg45ds = kpjbu
357900080627     c                   eval      kpjbu = kpjbus
358000101126     c                   if        c45err  = ' ' and c45daVISIO <>' '
358100101130     c*****              move      'S'           s2cCOK
358200101130     c                   move      c45davisio    s2cCOK
358300101126     c                   eval      h2notass = c45notASS
358400101130     c                   if        h2notass <> *blank
358500101130     c                   eval      h2in45 = '1'
358600101130     c                   end
358700080627     c                   else
358800080627      * ?  Problemi durante la chiamata
358900080627     c                   end
359000080606      *
359100080606     C                   endSR
359200080627      *---------------------------------------------------------------*
359300080627      *  ?  Aggiorna come visionato la INFO x PDA da visualizzare   ?
359400080627      *---------------------------------------------------------------*
359500080627     C     agg_PCT_COK   begSR
359600080627      *
359700080627      *  chiama la funzione passando la Bolla
359800101130     c                   eval      h2in45 = '0'
359900080627     c                   clear                   fidg45ds
360000080627     c                   z-add     s2caas        C45AAS
360100080627     c                   z-add     s2clnp        C45LNP
360200080627     c                   z-add     s2cnrs        C45NRS
360300080627     c                   z-add     s2cnsp        C45NSP
360400080818     c                   z-add     h1pdr_meno    c45codaut
360500080903     c                   z-add     D1CFGS        c45FGS
360600080903     c                   z-add     h1ndc_meno    c45NDC
360700080627     c                   movel     'S'           C45aggVIS
360800080627     c                   eval      kpjbus = kpjbu
360900080627     c                   eval      kpjbu  = fidg45ds
361000080627     c                   call      'FIDG45R'
361100080627     c                   parm                    kpjba
361200080627     c                   eval      fidg45ds = kpjbu
361300080627     c                   eval      kpjbu = kpjbus
361400101130     c                   if        c45err  = ' ' and c45daVISIO <>' ' and
361500101130     c                             h2notass <> *blank
361600101130     c                   eval      h2in45 = '1'
361700101130     c                   end
361800080627     c                   if        c45err  = ' '
361900080627     c                   clear                   s2cCOK
362000080627     c                   else
362100101130     c********           move      'S'           s2cCOK
362200101130     c                   move      c45davisio    s2cCOK
362300080627      * ?  Problemi durante la chiamata
362400080627     c                   end
362500080627      *
362600080627     C                   endSR
362700080606      *---------------------------------------------------------------*
362800070316      *  ?  Seleziona Tutti i record del SFL                         ?
362900070307      *---------------------------------------------------------------*
363000070307     C     F01_Tutti2    BEGSR
363100070307     C*
363200070314     C     WMAX          IFgt      0
363300070307     C*  Seleziona tutte le righe del SFL accendendo il SFLNXTCHG
363400070307     c                   z-add     1             nrr_s2
363500070307     c     nrr_s2        chain     FDG0S02                            21
363600070307     c                   dow       *in21 = *Off
363700070307     c                   move      '1'           s2sce
363800070307     C*
363900070307      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
364000100505     c                   setoff                                       6665
364100070307     C                   MOVE      *ON           *IN32
364200101130     c                   eval      *in45 = H2in45
364300130221     c     h2xco         comp      '0'                                88
364400070307     C                   UPDATE    FDG0S02
364500070307     C*
364600070307     c                   add       1             nrr_s2
364700070307     c     nrr_s2        chain     FDG0S02                            21
364800070307     c                   endDO
364900070314     c                   end
365000070307     C*
365100070307     C                   ENDSR
365200070301      *---------------------------------------------------------------*
365300070320      *?  CONTROLLO TESTATA LISTA                                    ?
365400070301      *---------------------------------------------------------------*
365500070301     C     CTRC2         BEGSR
365600070301     C                   MOVE      *OFF          *IN99
365700070413      *
365800070413      * Se si tratta di Trasferimento
365900070413      *  non deve essere la stessa distinta da cui trasferisce
366000070531    3C     d1sce         ifEQ      Trasferire
366100070413    3C     h1ndc_piu     andEQ     h1ndc_meno
366200070413     C                   SETON                                        59  99
366300070413     c                   endIF
366400070427      *
366500070427      * ? Controllo campi di posizionamento
366600070514      * ?  o di ricerca stringa (?) in primo Byte
366700070427     c                   eval      posizionamento = 'N'
366800070515     c                   eval      posizionato    = 'N'
366900070511     c                   eval      cerca_stringa  = 'N'
367000070514     c                   clear                   Stringa_20A      20
367100070511      *
367200070427     c                   select
367300070511      * giro
367400070427     c                   when      *in01 and sav_pos1 <> c2pos1
367500070427     c                   eval      sav_pos1 = c2pos1
367600070427     c                   eval      posizionamento = 'S'
367700070511      * Descrizione RagSoc
367800070427     c                   when      *in02 and sav_pos2 <> c2pos2
367900070514     c                   eval      sav_pos2 = c2pos2
368000070515      *  è una ricerca x stringa
368100070511     c                   eval      cerca_stringa  = 'S'
368200070515     c                   eval      Stringa_20A = %subst(c2pos2:1)
368300070515
368400070511      * Indirizzo
368500070427     c                   when      *in03 and sav_pos3 <> c2pos3
368600070514     c                   eval      sav_pos3 = c2pos3
368700070515      *  è una ricerca x stringa
368800070511     c                   eval      cerca_stringa  = 'S'
368900070515     c                   eval      Stringa_20A = %subst(c2pos3:1)
369000070511      * Località
369100070427     c                   when      *in04 and sav_pos4 <> c2pos4
369200070514     c                   eval      sav_pos4 = c2pos4
369300070515      *  è una ricerca x stringa
369400070511     c                   eval      cerca_stringa  = 'S'
369500070515     c                   eval      Stringa_20A = %subst(c2pos4:1)
369600070511      * Volume
369700070427     c                   when      *in05 and sav_pos5 <> c2pos5
369800070427     c                   eval      sav_pos5 = c2pos5
369900070427     c                   eval      posizionamento = 'S'
370000070511      * Peso
370100070427     c                   when      *in06 and sav_pos6 <> c2pos6
370200070427     c                   eval      sav_pos6 = c2pos6
370300070427     c                   eval      posizionamento = 'S'
370400070515      *
370500070515      * Se precedentemente si era posizionato
370600070515     c                   when      c2pos1 <> *blank  or
370700070515     c                             c2pos2 <> *blank  or
370800070515     c                             c2pos3 <> *blank  or
370900070515     c                             c2pos4 <> *blank  or
371000070515     c                             c2pos5 <> *zero   or
371100070515     c                             c2pos6 <> *zero
371200070515     c                   eval      posizionato = 'S'
371300070427      *
371400070427     C                   Endsl
371500070514      *
371600070514      * ? Se si deve posizionare sul SFL on *TOP
371700070514     c                   if        posizionamento = 'S'
371800070514     C                   Exsr      Da_Posizione
371900070514     c                   end
372000070514      *
372100070514      * ? Se si deve cercare una stringa con (*)
372200070514     c                   if        cerca_stringa  = 'S'
372300070514     C                   Exsr      Cerca_Parola
372400070514     c                   end
372500070514      *
372600070301     C                   ENDSR
372700070301      *---------------------------------------------------------------*
372800070514      *?  Imposta il posizionamento al record TOP del SFL2           ?
372900070301      *---------------------------------------------------------------*
373000070514     C     Da_Posizione  BEGSR
373100070514      *
373200070514      * ? Se si deve posizionare sul SFL on *TOP
373300070514     c                   seton                                        99
373400070514     c                   eval      C2RCD =  1
373500070514      *
373600070514      * ? Primo ciclo per togliere le priorità a tutti records
373700070514      * ? del SFL
373800070514     C                   DO        RrnLast       S2nrr
373900070514     C     S2nrr         CHAIN     FDG0S02
374000080609     c                   if        s2sce = '1'  or s2sce = '5' or s2sce = '8'
374100101129     c                             or s2sce = '7'
374200070514     C                   MOVE      *ON           *IN32
374300070515     c                   else
374400070515     C                   SetOFF                                       32
374500070515     c                   end
374600070514     c                   clear                   H2SUBST
374700101130     c                   eval      *in45 = H2in45
374800130221     c     h2xco         comp      '0'                                88
374900070514     C                   UPDATE    FDG0S02
375000070514     c                   end
375100070514      *
375200070514      * ? Ordina il SFL....
375300070514     c                   exsr      Ordina_SFL2
375400070514      *
375500070514      * ? Secondo ciclo x trovare il posizionamento dal record....
375600070514     C                   DO        RrnLast       S2nrr
375700070514     C     S2nrr         CHAIN     FDG0S02
375800070514     c                   if        (*in01 and C2pos1 >  s2CGI)     or
375900070514     c                             (*in02 and C2pos2 >  s2cRSD)    or
376000070514     c                             (*in03 and C2pos3 >  s2cIND)    or
376100070514     c                             (*in04 and C2pos4 >  s2cLOD)    or
376200070514     c                             (*in05 and C2pos5 <  s2cVLF)    or
376300070514     c                             (*in06 and C2pos6 <  s2cPKF)
376400070514     C*
376500070514     c                   eval      C2RCD = S2nrr + 1
376600070514     c                   if        c2RCD > RrnLast
376700070514     c                   eval      C2RCD = RrnLast
376800070514     c                   end
376900070514     C*
377000070514     C* Si tiene in memoria la riga toccata con SFLNXTCHG
377100080609     c                   if        s2sce = '1'  or s2sce = '5' or s2sce = '8'
377200101129     c                             or s2sce = '7'
377300070514     C                   MOVE      *ON           *IN32
377400070515     c                   else
377500070515     C                   SetOFF                                       32
377600070514     c                   end
377700070514     C*
377800101130     c                   eval      *in45 = H2in45
377900130221     c     h2xco         comp      '0'                                88
378000070514     C                   UPDATE    FDG0S02
378100070514     C*
378200070514     c                   else
378300070514     C*
378400070514     c                   iter
378500070514     C*
378600070514     c                   end
378700070514     C                   ENDDO
378800070514      *
378900070514     C                   ENDSR
379000070514      *---------------------------------------------------------------*
379100070514      *?  Se richiesta una ricerca di stringa fra i record del SFL   ?
379200070514      *---------------------------------------------------------------*
379300070514     C     Cerca_Parola  BEGSR
379400070514      *
379500070514      * ? Deve cercare la stringa e visualizzare prioritariamente i records
379600070514      * ? contenenti la stringa
379700070514     c                   seton                                        99
379800070514     c                   eval      C2RCD =  1
379900070514     c                   eval      WLen = %len(%Trim(Stringa_20A))
380000081203      * solo se selezionato qualcosa
380100081203     c                   if        wlen > 0
380200070514      *
380300070514     C                   DO        RrnLast       S2nrr
380400070514     C     S2nrr         CHAIN     FDG0S02
380500070514     c                   clear                   WPos
380600070514     c                   clear                   H2SUBST
380700070514      *
380800070514     c   02              eval      WPos = %scan(%subst(Stringa_20A:1:WLen) :
380900070514     c                              S2cRSD)
381000070514      *
381100070514     c   03              eval      WPos = %scan(%subst(Stringa_20A:1:WLen) :
381200070514     c                              S2cIND)
381300070514      *
381400070514     c   04              eval      WPos = %scan(%subst(Stringa_20A:1:WLen) :
381500070514     c                              S2cLOD)
381600070514
381700070514      *  Se ha trovato la stringa cercata
381800070514     c                   if        Wpos > 0
381900070514     c                   z-add     1             H2SUBST
382000070514     c                   end
382100070514     C*
382200070514     C* Si tiene in memoria la riga toccata con SFLNXTCHG
382300080609     c                   if        s2sce = '1'  or s2sce = '5' or s2sce = '8'
382400101129     c                             or s2sce = '7'
382500070514     C                   MOVE      *ON           *IN32
382600070515     c                   else
382700070515     C                   SetOFF                                       32
382800070514     c                   end
382900070514     C*
383000101130     c                   eval      *in45 = H2in45
383100130221     c     h2xco         comp      '0'                                88
383200070514     C                   UPDATE    FDG0S02
383300070514     C*
383400070514     C                   ENDDO
383500070514      *
383600081203     c                   end
383700081203      *
383800070514      *  Quindi emette il SFL riordinato
383900070514     C                   EXSR      Ordina_SFL2
384000070514      *
384100070514     C                   ENDSR
384200070514      *---------------------------------------------------------------*
384300070514      *?  CONTROLLO OPZIONI LISTA                                    ?
384400070514      *---------------------------------------------------------------*
384500070514     C     CTRS2         BEGSR
384600070514     C*
384700070301     C*  Pulizia campi Totali
384800070302     c                   z-add     0             totSPE_s2
384900070302     c                   z-add     0             totPKL_s2
385000070302     c                   z-add     0             totVLU_s2
385100070302     c                   z-add     0             totCOL_s2
385200070306     c                   z-add     0             giri_scelti
385300070306     c                   clear                   SAV_S2cgi
385400070413     c                   clear                   primoGiro_Blk     1
385500101130     c                   clear                   exec_opzInfo
385600151001     c                   clear                   in66_msg          1
385700070301     C*
385800070301     C                   MOVEL     *OFF          $ESCI
385900070301     C                   SETOFF                                       99
386000100505     C*
386100100505     C*  Se è stato premuto il tasto per forzare lo imposta prima dei controlli
386200100505     C*   sui singoli records
386300100505     C     $TASTO        IFEQ      F02
386400100505     c                   eval      Premuto_F02 = '1'
386500100505     c                   end
386600070301     C*
386700070301     C* Leggo il sfl solo se ci sono rcd
3868000703011    C     WMAX          IFGT      0
386900070302     C                   READC     FDG0S02                                21
387000070301     C*
387100070301     C* esce se fine sfl o errore che richiede l'uscita
3872000703012    C     *IN21         DOWEQ     *OFF
387300070301     C     $ESCI         ANDEQ     *OFF
387400070302     C                   Z-ADD     S2NRR         C2RCD
387500070301     C* ctrl su riga
387600070302     C                   EXSR      RECS2
387700070301      *
387800070301     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
3879000703013    C     *IN99         IFEQ      *ON
388000070301     C     $ESCI         OREQ      *ON
388100100505     C     da_forzare    OREQ      '1'
388200070301     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
388300070301     C* la ripristinerò a conclusione del ciclo di READC
388400070302     C                   MOVE      *OFF          $INZS2
3885000703013-   C                   ENDIF
388600070301     C*
388700070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
388800080609      * ?    aggiunto anche l'aggiornamento dell'INFO da visionare/visionato
388900080609     c                   if        s2sce = '1'  or s2sce = '5' or s2sce = '8'
389000101129     c                             or s2sce = '7'
389100070301     C                   MOVE      *ON           *IN32
389200070515     c                   else
389300070515     C                   SetOFF                                       32
389400070307     c                   end
389500101130     c                   eval      *in45 = H2in45
389600130221     c     h2xco         comp      '0'                                88
389700070302     C                   UPDATE    FDG0S02
389800070301     C*
389900070301     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
3900000703013    C     $ESCI         IFEQ      *OFF
390100070302     C                   READC     FDG0S02                                21
390200070301      *
390300070301     C* a fine ciclo ripristino il flag richiesta iniz.sfl
3904000703014    C     *IN21         IFEQ      *ON
390500070301     C* calcolo pagina a cui deve posizionarsi
390600070302     c                   z-add     s2pag         Wpagine
390700070301     C                   EXSR      CLCPAG
3908000703014-   C                   ENDIF
3909000703013-   C                   ENDIF
391000070301     C*
3911000703012-   C                   ENDDO
391200070301     C*
391300100505     C* Deve bloccarsi come se fosse un errore
391400100505     C*  poichè riga x riga deve forzare le eventuali speidizioni
391500100505      *
391600100505     c                   if        Da_forzare = '1' and
391700160525     c                             (d1SCE = Assegnare or d1SCE = Trasferire)
391800100505     C                   SETON                                        99
391900100505     c                   end
392000100505     C*
3921000703011-   C                   ENDIF
392200070301     C*
392300070301     C                   ENDSR
392400070301     C/EJECT
392500070301      *---------------------------------------------------------------*
392600070320      *?  CONTROLLO CAMPI I/O RIGA LISTA                             ?
392700070301      *---------------------------------------------------------------*
392800070302     C     RECS2         BEGSR
392900070301     C*
393000070301     C* reset indicatori DSPATR
393100070308     c                   exsr      INZ_indERR
393200100505     C*
393300100505     C* x scelta blank resettare il campo di forzatura
3934001005053    C     S2sce         IFeq      *blank
393500100505     c                   eval      s2Forza = ' '
3936001005053-   C                   ENDIF
393700160317     C*
393800141113     c                   clear                   fnlr98ds2
393900141113     c                   if        s2sce = '1' and
394000141113     c                             (d1sce = togliere or
394100141113     c                             d1sce = trasferire)
394200141113     c*check spedizione con disposizioni di dirottamento richiede forzatura se presenti
394300141113     c                   z-add     s2caas        i982aas
394400141113     c                   z-add     s2cLNP        i982lnp
394500141113     c                   z-add     s2cNrs        i982nrs
394600141113     c                   z-add     s2cNsp        i982nsp
394700141113     c                   call      'FNLR98R2'
394800141113     c                   parm                    fnlr98ds2
394900141113     c                   if        o982esito = '1'
395000141113     c                   seton                                        99
395100141113     c                   endif
395200141113     c                   endif
395300160317     C*
395400160317     C*  non possono essere assegnate bolle DPD con VEDI PACCO a GIRI reali
395500160317     C*   ma solo ad autista fittizio quindi a distinte Fittizie
395600160317     c                   if        s2sce = '1' and
395700160317     c                             (d1sce = assegnare or
395800160317     c                              d1sce = trasferire)
395900160321     c                   if         S2CGI = GIRO_vd_PACCO   and
396000160322     c                              H1pdd_piu <> 'S' and h1ndc_piu > 0
396100160322     c                   seton                                        9958
396200160317     c                   endif
396300160317     c                   endif
396400070327     C*
396500070327     C* gestione opzioni
3966000703273    C     S2sce         IFNE      *blank
396700070327     C     *IN99         ANDEQ     *OFF
396800070327     C                   EXSR      SCE_S2
3969000703273-   C                   ENDIF
397000070301     C*
397100070301     C                   ENDSR
397200070301      *---------------------------------------------------------------*
397300070320      *?  GESTIONE OPZIONI LISTA                                     ?
397400070301      *---------------------------------------------------------------*
397500070309     C     SCE_S2        BEGSR
397600070307     C*
397700070307     C*  Interrogazione bolla
397800070307     c                   If        s2SCE = '5'
397900070307     c                   eval      kpjbus = kpjbu
398000070307     C                   clear                   dslr36
398100070307     C                   move      s2cAAS        P36AAS
398200070307     C                   move      s2cLNP        P36LNP
398300070307     C                   move      s2cNRS        P36NRS
398400070307     C                   move      s2cNSP        P36NSP
398500070307     C                   MOVEL     '1'           P36FLG
398600070307     C                   MOVEL     ' '           P36F03
398700070307     C                   movel(p)  dslr36        KPJBU
398800070307     C                   call      'FNLR36R'                            99
398900070307     C                   parm                    KPJBA
399000070307     c                   movel     kpjbus        kpjbu
399100070307     c                   clear                   s2SCE
399200070307     c                   seton                                        99
399300070307     c                   endIf
399400150930      **
399500150930     C*   attenzione: in assegnazione deve controllare se è stato inviato
399600150930     C*   un contro ALert (di NON CONSEGNA) in tal caso deve Forzare l'immissione
399700150930     C*   in distinta della Spedizione con F2.
399800150930      *
399900150930     c                   IF        d1SCE = Assegnare and h2Calert = 'S' and
400000150930     c                             s2SCE = '1' and s2Forza =' '
400100150930     C* F2=Forzatura
400200150930     C                   if        Premuto_F02 = '1'
400300150930     c                   setoff                                       66
400400150930     C                   move      '0'           da_forzare
400500150930     c                   eval      s2Forza = 'F'
400600150930     C                   eval      Premuto_F02 = '0'
400700150930     C*
400800150930      * errore: Occorre forzare la riga se la si vuole proprio selezionare
400900150930      *   si deve forzare con "F2" la riga... deve segnalarlo.
401000150930     c                   else
401100150930     c                   seton                                        66
401200151001     c                   if         in66_msg = *blank
401300150930     c                   eval       in66_msg = 'C'
401400151001     c                   end
401500150930     C                   move      '1'           da_forzare
401600150930     c                   end
401700150930     C*
401800150930      *---- (x1)
401900150930      **
402000100505     C*   attenzione se in assegnazione deve controllare se si tratta di
402100100505     C*    una spedizione  da forzare perchè con particolarità di
402200100505     C*    Particolarità di Attesa
402300100505      *
402400150930     c                   ElseIf    d1SCE = Assegnare and h2att = 'S' and
402500100505     c                             s2SCE = '1' and s2pATT = 'A' and s2Forza =' '
402600100505     C* F2=Forzatura
402700100505     C                   if        Premuto_F02 = '1'
402800100505     c                   setoff                                       66
402900100505     C                   move      '0'           da_forzare
403000100505     c                   eval      s2Forza = 'F'
403100100505     C                   eval      Premuto_F02 = '0'
403200100505     C*
403300100505      * errore: Occorre forzare la riga se la si vuole proprio selezionare
403400100505      *   si deve forzare con "F2" la riga... deve segnalarlo.
403500100505     c                   else
403600100505     c                   seton                                        66
403700151001     c                   if         in66_msg = *blank
403800150930     c                   eval       in66_msg = 'A'
403900151001     c                   end
404000100505     C                   move      '1'           da_forzare
404100100505     c                   end
404200100505     C*
404300160429      *---- (x1)
404400160429      **
404500160429     C*   attenzione se in assegnazione deve controllare se si tratta di
404600160429     C*    una spedizione  da forzare perchè la zona di consegna appartiene
404700160429     C*   al secondo turno
404800160429      *
404900160525     c                   ElseIf    (d1SCE = Assegnare or d1SCE = Trasferire)
405000160525     c                             and s2SCE   = '1' and s2Forza   = ' '
405100160506     c                             and s2turno = '2' and h1tur_piu ='1'
405200160525     c                             and d1tur1 <> 'U' and d1tur2  <> 'U'
405300160525     c                             and h1pdd_piu<>'S'
405400160429     C* F2=Forzatura
405500160429     C                   if        Premuto_F02 = '1'
405600160429     c                   setoff                                       66
405700160429     C                   move      '0'           da_forzare
405800160429     c                   eval      s2Forza = 'F'
405900160429     C                   eval      Premuto_F02 = '0'
406000160429     C*
406100160429      * errore: Occorre forzare la riga se la si vuole proprio selezionare
406200160429      *   si deve forzare con "F2" la riga... deve segnalarlo.
406300160429     c                   else
406400160429     c                   seton                                        66
406500160429     c                   if         in66_msg = *blank
406600160429     c                   eval       in66_msg = 'Z'
406700160429     c                   end
406800160429     C                   move      '1'           da_forzare
406900160429     c                   end
407000160429     C*
407100100505      *---- (x1)
407200100505     c                   Else
407300100505     C*
407400100505      *--------- (2)
407500070301     C*  Totalizza le Spedizioni/Pesi/Volumi
407600070302     c                   If        s2SCE = '1'
407700100505     C*
407800070302     c                   add       1             totSPE_s2
407900070427     c                   add       s2pkf         totPKL_s2
408000070302     c                   add       s2cvlf        totVLU_s2
408100070302     c                   add       s2cncl        totCOL_s2
408200070313     C*
408300070313     c                   move      s2cgi         sav_sxcgi
408400070305     C*
408500070305     C*   Conta quanti giri sono stati scelti nel sfl
408600070305     C*  per permettere di interrogare con l'F4 con stesso giro
408700100505      *--------- (3)
408800070413     c                   if        s2cgi <> SAV_s2cgi  or
408900070413     c                             primoGiro_Blk = *Blank and s2cgi = *Blank
409000070413     C*
409100070413      *  e se il giro è Blank non deve ripetere un eventuale conto di altri con giro Blank
409200070413     c                   eval      primoGiro_Blk = 'S'
409300070413     C*
409400070305     c                   add       1             giri_scelti
409500070305     c                   move      s2cgi         sav_s2cgi
409600070305     c                   move      s2cgi         c2giro
409700070305     c                   end
409800100505      *---- (e3)
409900100505     c                   endIf
410000100505      *---- (e2)
410100070301     c                   endIf
410200100505      *---- (e1)
410300080609      *
410400101129     C*  Interr.INFO e NOTE  una INFO da vedere "cOK"
410500080609      *
410600101130     c                   If        s2SCE = '7' and H2in45 ='1'
410700101130     c                   move      'S'           exec_opzInfo
410800080609      *
410900101129      /free
411000101129         clear NOTINFO;
411100101129           WSPED =  %editc(S2CAAS:'X') + '/' + %editc(S2CLNP:'X') + '/' +
411200101129                    %editc(S2CNRS:'X') + '/' + %editc(S2CNSP:'X');
411300101129
411400101129          wdatabol = S2CAAS * 10000  + H2mgs;
411500101129          dataiso = %date(wdatabol:*iso);
411600101129          dataeur = dataiso;
411700101129          WDEL   = %dec(dataeur);
411800101129
411900101129           WDEST = S2CRSD;
412000101129           tbeCOD = 'ASA';
412100101129           tbeKE1 = S2CCOK;
412200101129           chain (tbecod:tbeke1) tntbe01l;
412300101129           clear WDcodInfo;
412400101129          if  %found(tntbe01l);
412500101129           WDcodInfo = tbeUNI;
412600101129          endif;
412700101129           WCODINFO= %trim(S2CCOK)  + ': ' + WDcodInfo;
412800101129           WNOTASS = H2NOTASS;
412900101129         exfmt NOTINFO;
413000101129      /end-free
413100101129      *
413200101130     c                   clear                   s2SCE
413300101129     c                   endIf
413400101129      *---- (e1)
413500101129      *
413600101129     C*  Chiude INFO se presente una INFO da vedere "cOK"
413700101129      *
413800101129     c                   If        s2SCE = '8'
413900101129      *
414000080613      *   Solo se attivata proc.PDA consegne e RICHIAMATO dal MONITOR
414100080609     c                   if          PDAconsegne <> *blank and
414200080609     c                             PGM_Chiamante = 'MONITOR'
414300080609      *
414400080627     c                   exsr      agg_PCT_COK
414500080627      *
414600080609     c                   clear                   s2SCE
414700101130     c                   move      'S'           exec_opzInfo
414800080609      *
414900080609     c                   else
415000080613      * errore: se non attivata proc.Consegne con PDA o NON richiamato dal
415100080613      *  MONITOR.
415200080818      *   si è scelta opz.(8) sulla riga, deve segnalare l'errore come
415300080613      *    opzione non valida.
415400080609     c                   seton                                        9965
415500100505     C                   move      '1'           opz_errata
415600080609     c                   endIf
415700080609      *
415800080609     c                   endIf
415900070301     C*
416000070301     C                   ENDSR
416100070301     C/EJECT
416200070302      *---------------------------------------------------------------*
416300070320      *?  Chiusura Files 2°SFL                                       ?
416400070302      *---------------------------------------------------------------*
416500070302     C     CLOF_S2       BEGSR
416600070306      *
416700070531     c                   if        d1sce = Togliere or d1sce = Trasferire and
416800070404     c                             test_Parallelo = 'S'
416900070306     C/EXEC SQL
417000070306     C+ CLOSE i1
417100070306     C/END-EXEC
417200070306     c                   end
417300070306      *
417400070302     c                   ENDSR
417500070302     C/EJECT
417600070301      *---------------------------------------------------------------*
417700070316      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
417800070301      *---------------------------------------------------------------*
417900070228     C     CLCPAG        BEGSR
418000940224     C* Input :
418100940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
418200940224     C*          cursore
418300070301     C* - Wpagine= numero rcd per pagina sfl
418400940224     C* Output :
418500940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
418600940224     C*
418700070301     C     WSFL          DIV       Wpagine       PAGINE            4 0
418800940224     C                   MVR                     RESTO             3 0
418900940224     C     RESTO         IFGT      0
419000940224     C                   ADD       1             PAGINE
419100940224     C                   ENDIF
419200070301     C     PAGINE        MULT      Wpagine       WPAG
419300940224     C*
419400940224     C                   ENDSR
419500070302     C************************************************************
419600070320      *?  Emissione ultima finestra di riepilogo                     ?
419700070302     C************************************************************
419800070302     C     GESW1         BEGSR
419900070302     C*
420000070302     C* inizializzazione videata
420100070302     C     $INZW1        IFEQ      *ON
420200070302     C                   EXSR      INZW1
420300070302     C                   MOVE      *OFF          $INZW1
420400070302     C                   ENDIF
420500070302     C*
420600070302     c     Window_1      tag
420700070302     C*  Emissione Video
420800070302      * ?            *-----------------------*
420900070302     C                   EXFMT     FDG0W01
421000070302      * ?            *-----------------------*
421100070326     c                   setoff                                       99
421200070302     C* Selezioni
4213000703021    C                   SELECT
421400070302     C* F3=Fine
421500070302     C                   WHEN      $TASTO   =   F03
421600070302     C                   EXSR      F03_exit
421700070319     c                   rolbk
421800070302     C* F12=Ritorno
421900070302     C                   WHEN      $TASTO   =   F12
422000070308     c                   exsr      INZ_indERR
422100070305     c                   move      'S2'          $GEST
422200070305      *   scelta = 1 se veniva da SFL 1
422300070531     c                   if        d1sce = Assegnare and sav$GEST ='S1'
422400070305     c                   move      'S1'          $GEST
422500070305     c                   end
422600070302      *
4227000703021O   C                   OTHER
422800070302     C* CONTROLLO DATI
422900070302     C                   EXSR      CTRW1
4230000703021-   C                   ENDSL
423100070302     C*
423200070302     C* Per errori o decodifiche
423300070302     c   99              goto      Window_1
423400070302     C*
423500070302     C*    Aggiorna il Data Base
423600070302     C                   if        $TASTO   =   F06
423700070302     C                   EXSR      F06_Aggiorna
423800070302     c                   end
423900070302      *
424000070302     C                   ENDSR
424100070302     C/EJECT
424200070302     C************************************************************
424300070320      *?  INIZIALIZZA indicatori di errore                           ?
424400070302     C************************************************************
424500070308     C     INZ_indERR    BEGSR
424600070308      *
424700070308     C                   move      *ALL'0'       IN5190           40
424800070308     C                   moveA     IN5190        *IN(51)
424900070308      *
425000070308     C                   ENDSR
425100070308     C/EJECT
425200070308     C************************************************************
425300070316      * ?  INIZIALIZZAZIONE Window Finale prima dell'aggiornamento   ?
425400070308     C************************************************************
425500070308     C     INZW1         BEGSR
425600070308     C*
425700070302     C* Pulizia campi e indicatori
425800070308     c                   exsr      INZ_indERR
425900070302     c                   setoff                                       99  50
426000070306     c                   setoff                                       515253
426100070306     c                   setoff                                       545556
426200070302     C* Pulizia formato record
426300070302     c                   clear                   FDG0W01
426400070306     C* imposta campi video
426500070302     c                   z-add     totSPE_s2     w1ssn1
426600070302     c                   z-add     totPKL_s2     w1ssp1
426700070302     c                   z-add     totVLU_s2     w1ssv1
426800070306      *
426900070306     c                   z-add     totSPE_s2     w1ssn2
427000070306     c                   z-add     totPKL_s2     w1ssp2
427100070306     c                   z-add     totVLU_s2     w1ssv2
427200070306      *
427300070306     c                   select
427400070306     C*  Assegnare
427500070531     c                   When      d1sce = Assegnare
427600070319     c                   z-add     h1ndc_piu     w1ndc1
427700070306     c                   move      Alla          w1ada
427800070326     c                   eval      %subst(W1dSCE:6:11) = OPZ_1
427900070306      *
428000070306     C*  Togliere
428100070531     c                   When      d1sce = Togliere
428200070319     c                   z-add     h1ndc_meno    w1ndc1
428300070306     c                   move      Dalla         w1ada
428400070326     c                   eval      %subst(W1dSCE:7:11) = OPZ_2
428500070306      *
428600070306     C*  Trasferire
428700070531     c                   When      d1sce = Trasferire
428800070306     C* Se sta eseguendo il trasferimento occorre aprire la finestra
428900070306     c                   seton                                        50
429000070319     c                   z-add     h1ndc_meno    w1ndc1
429100070319     c                   z-add     h1ndc_piu     w1ndc2
429200070306     c                   move      Dalla         w1ada
429300070326     c                   eval      %subst(W1dSCE:6:11) = OPZ_3
429400070306      *
429500070306     c                   endSL
429600070305      *  Legge il foglio
429700070305     c                   move      w1ndc1        wNFV
429800070319     c                   eval      Aggior_Dist = 'N'
429900070319      *                                     ?  =======  ?
430000070316     c                   exsr      CHAIN_Dist
430100070319      *
430200070316     c                   if        Found_Dist = 'N'
430300070305     c                   movel     *all'?'       w1pdr1
430400070305     c                   else
430500070305     C                   move      'A'           tipo_cod
430600070305     C                   move      dstpdr        cod_padr
430700070305     C     Kapd          CHAIN     fiapd01l
430800070305     c                   if        %Found(fiapd01l)
430900070305     c                   movel     APDRSC        w1pdr1
431000070305     c                   end
431100070305      *
431200070305     c                   z-add     DSTPKL        W1DMP1
431300070305     c                   z-add     DSTVLU        W1DMV1
431400070305     c                   z-add     DSTNBL        W1DMN1
431500070305      *
431600070305     c                   z-add     DSTPKLA       W1DAP1
431700070305     c                   z-add     DSTVLUA       W1DAV1
431800070305     c                   z-add     DSTNBLA       W1DAN1
431900070305     c                   end
432000070305     C*
432100070306      *  Legge il foglio
432200070316     c                   move      w1ndc2        wNFV
432300070319     c                   eval      Aggior_Dist = 'N'
432400070319      *                                     ?  =======  ?
432500070316     c                   exsr      CHAIN_Dist
432600070319      *
432700070316     c                   if        Found_Dist = 'N'
432800070306     c                   movel     *all'?'       w1pdr2
432900070306     c                   else
433000070306     C                   move      'A'           tipo_cod
433100070306     C                   move      dstpdr        cod_padr
433200070306     C     Kapd          CHAIN     fiapd01l
433300070306     c                   if        %Found(fiapd01l)
433400070306     c                   movel     APDRSC        w1pdr2
433500070306     c                   end
433600070306      *
433700070306     c                   z-add     DSTPKL        W1DMP2
433800070306     c                   z-add     DSTVLU        W1DMV2
433900070306     c                   z-add     DSTNBL        W1DMN2
434000070306      *
434100070306     c                   z-add     DSTPKLA       W1DAP2
434200070306     c                   z-add     DSTVLUA       W1DAV2
434300070306     c                   z-add     DSTNBLA       W1DAN2
434400070306     c                   end
434500070306      *
434600070306     c                   select
434700070306     C*  Assegnare
434800070531     c                   When      d1sce = Assegnare
434900070306      *
435000070306     c     W1DAN1        add       totSPE_s2     w1tsn1
435100070306     c     W1DAP1        add       totPKL_s2     w1tsp1
435200070306     c     W1DAV1        add       totVLU_s2     w1tsv1
435300070306     c     w1tsn1        comp      W1DMN1                             51
435400070306     c     w1tsp1        comp      W1DMP1                             52
435500070306     c     w1tsv1        comp      W1DMV1                             53
435600070306     C*
435700070531     c                   When      d1sce = Togliere
435800070306      *
435900070306     c     W1DAN1        sub       totSPE_s2     w1tsn1
436000070306     c     W1DAP1        sub       totPKL_s2     w1tsp1
436100070306     c     W1DAV1        sub       totVLU_s2     w1tsv1
436200070306     c     w1tsn1        comp      W1DMN1                             51
436300070306     c     w1tsp1        comp      W1DMP1                             52
436400070306     c     w1tsv1        comp      W1DMV1                             53
436500070306     C*
436600070531     c                   When      d1sce = Trasferire
436700070302     C*
436800070306     c     W1DAN1        sub       totSPE_s2     w1tsn1
436900070306     c     W1DAP1        sub       totPKL_s2     w1tsp1
437000070306     c     W1DAV1        sub       totVLU_s2     w1tsv1
437100070306     c     w1tsn1        comp      W1DMN1                             51
437200070306     c     w1tsp1        comp      W1DMP1                             52
437300070306     c     w1tsv1        comp      W1DMV1                             53
437400070306      *
437500070306     c     W1DAN2        add       totSPE_s2     w1tsn2
437600070306     c     W1DAP2        add       totPKL_s2     w1tsp2
437700070306     c     W1DAV2        add       totVLU_s2     w1tsv2
437800070306     c     w1tsn2        comp      W1DMN2                             54
437900070306     c     w1tsp2        comp      W1DMP2                             55
438000070306     c     w1tsv2        comp      W1DMV2                             56
438100070306      *
438200070306     c                   endsl
438300070306      *
438400070302     C                   ENDSR
438500070302     C************************************************************
438600070320      *?  Controlli primo video di SCELTA Operazione da eseguire     ?
438700070302     C************************************************************
438800070302     C     CTRW1         BEGSR
438900070302     C*
439000070302     C*  Spegne indicatori di errore
439100070308     c                   exsr      INZ_indERR
439200070302     c                   setoff                                       99
439300070306     C*  se superano i MAX.previsti in distinta
439400070531     c                   if        d1sce = Trasferire
439500070306     c     w1tsn2        comp      W1DMN2                             54
439600070306     c     w1tsp2        comp      W1DMP2                             55
439700070306     c     w1tsv2        comp      W1DMV2                             56
439800070306     c                   else
439900070306     c     w1tsn1        comp      W1DMN1                             51
440000070306     c     w1tsp1        comp      W1DMP1                             52
440100070306     c     w1tsv1        comp      W1DMV1                             53
440200070306     c                   end
440300070302     C*
440400070302     C     ENDCTRW1      ENDSR
440500070301      *---------------------------------------------------------------*
440600070320      *?  GESTIONE F03 x uscire LR                                   ?
440700070301      *---------------------------------------------------------------*
440800070227     C     F03_exit      BEGSR
440900070227     C* fine programma
441000940309     C                   MOVE      *ON           $FINE
441100070302      *
441200940309     C                   ENDSR
441300070227     C/EJECT
441400070301      *---------------------------------------------------------------*
441500070320      *?  GESTIONE F04 x interrogare distinte stesso giro            ?
441600070301      *---------------------------------------------------------------*
441700070227     C     F04_interr    BEGSR
441800070227     C*
441900070313     c                   seton                                        99
442000070321     C                   MOVEL     sav_sxCGI     wCGI
442100070606     c                   move      Scegliere     w1sce
442200070606     c                   clear                   w1NO_dis
442300070606     c                   if        d1sce = Trasferire
442400070606     c                   move      d1ndc3        w1NO_dis
442500070606     c                   end
442600070321     c                   exsr      Ricerca_Dist
442700070302      *
442800070302     C* controlla la distinta selezionata
442900070313     c                   z-add     FDGNFV        wNFV
443000070319     c                   eval      Aggior_Dist = 'N'
443100070319      *                                     ?  =======  ?
443200070316     c                   exsr      CHAIN_Dist
443300070319     c                   if        Error_Dist = 'S'
443400070319     c                   if        Rec_Allocato = 'S'
443500070319     C                   SETON                                            62
443600070319     c                   end
443700070319     C                   SETON                                            99
443800070319     C                   goto      ENDF04
443900070319     c                   end
444000070319      *
444100070316     c                   if        Found_Dist = 'N'
444200070302     C                   SETON                                        52  99
444300070302     C                   goto      ENDF04
444400070302     c                   else
444500070413      *
444600070302      * Foglio chiuso o Annullato
444700070302    3C     dstFCF        ifEQ      'S'
444800070302    4C     dstATB        orNE      ' '
444900070306     C                   SETON                                        53  99
445000070302     C                   goto      ENDF04
445100070302     c                   endIF
445200070413      *
445300070302     c                   end
445400160321      *
445500160321      *  controlla se l'Autista è fittizio quindi possiamo considerare
445600160321      *   la Distinta Fittizia
445700160321     C                   move      'A'           tipo_cod
445800160321     C                   move      dstpdr        cod_padr
445900160321     C     Kapd          CHAIN     fiapd01l
446000160321      *   se Escluso da Fatturazione
446100160323     c                   if        %Found(fiapd01l)
446200160323     c                   if         apdPDD = 'S'
446300160321     c                   movel     'S'           H1pdd_PIU
446400160323     c                   else
446500160323     c                   clear                   H1pdd_PIU
446600160323     c                   end
446700160321     c                   end
446800070302      * se tutto OK
446900070302     C                   MOVE      *zeros        d1ndc1
447000070313     C                   MOVE      FDGNFV        d1ndc1
447100070319     C                   z-add     FDGNFV        h1ndc_piu
447200070319     c                   z-add     dstPDR        h1pdr_piu
447300120209     c                   z-add     dstDFV        h1ddc_piu
447400070319     c                   move      dstFPP        h1fpp_piu
447500080929      *
447600080929      * Controlla se la distinta è gestita con il PDA
447700080929      *  se in test nel filler
447800080929     c                   eval      ddstFLR = dstFLR
447900160506     c                   if        DSTTPM <> 'M' and §DSTTURNO =' '
448000160506     c                   eval         §DSTTURNO = '1'
448100160506     c                   end
448200160506     c                   eval      h1tur_piu = §DSTTURNO
448300080929     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
448400080929     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
448500080929     c                   eval      h1PDA_piu = 'S'
448600080929     c                   else
448700080929     c                   clear                   h1PDA_piu
448800080929     c                   end
448900070413      *
449000070313      * intestazione SF1
449100070531     C                   if        d1sce = Assegnare and h1ndc_piu > 0
449200070313     c                   movel     Alla_Dist     c1ndc1
449300070319     c                   move      h1ndc_piu     distintalfa
449400070313     c                   do        7             xy
449500070313     c                   if        ndc_a(xy) <> '0'
449600070313     c                   leave
449700070313     c                   end
449800070313     c                   move      ' '           ndc_a(xy)
449900070313     c                   enddo
450000070313     c                   move      distintalfa   c1ndc1
450100070313      *
450200070313     c                   eval      C2dsce = OPZ_1
450300070313     c                   eval      C2dess = 'alla Distinta nr.' +
450400070319     c                             %trim(%editc(h1ndc_piu:'Z'))
450500070313     c                   end
450600070227     C*
450700070302     C     EndF04        ENDSR
450800070227     C/EJECT
450900070301      *---------------------------------------------------------------*
451000070320      *?  GESTIONE F05 x interrogare tutte le distinte               ?
451100070301      *---------------------------------------------------------------*
451200070227     C     F05_interr    BEGSR
451300070302     C*
451400070313     c                   seton                                        99
451500070606     c                   move      Scegliere     w1sce
451600070606     c                   clear                   w1NO_dis
451700070606     c                   if        d1sce = Trasferire
451800070606     c                   move      d1ndc3        w1NO_dis
451900070606     c                   end
452000070321     c                   exsr      Ricerca_Dist
452100070321     c                   z-add     FDGNFV        wNFV
452200070302     C* controlla la distinta selezionata
452300070319     c                   eval      Aggior_Dist = 'N'
452400070319      *                                     ?  =======  ?
452500070316     c                   exsr      CHAIN_Dist
452600070319      *
452700070319     c                   if        Error_Dist = 'S'
452800070319     c                   if        Rec_Allocato = 'S'
452900070326     C                   SETON                                            62
453000070319     c                   end
453100070319     C                   SETON                                            99
453200070319     C                   goto      ENDF05
453300070319     c                   end
453400070319      *
453500070316     c                   if        Found_Dist = 'N'
453600070306     C                   SETON                                        54  99
453700070302     C                   goto      ENDF05
453800070302     c                   else
453900070302      * Foglio chiuso o Annullato
454000070302    3C     dstFCF        ifEQ      'S'
454100070302    4C     dstATB        orNE      ' '
454200070306     C                   SETON                                        55  99
454300070302     C                   goto      ENDF05
454400070302     c                   endIF
454500070413      *
454600070302     c                   end
454700160321      *
454800160321      *  controlla se l'Autista è fittizio quindi possiamo considerare
454900160321      *   la Distinta Fittizia
455000160321     C                   move      'A'           tipo_cod
455100160321     C                   move      dstpdr        cod_padr
455200160321     C     Kapd          CHAIN     fiapd01l
455300160321      *   se Escluso da Fatturazione
455400160323     c                   if        %Found(fiapd01l)
455500160323     c                   if           apdPDD = 'S'
455600160321     c                   movel     'S'           H1pdd_PIU
455700160323     c                   else
455800160323     c                   clear                   H1pdd_PIU
455900160323     c                   end
456000160321     c                   end
456100070302      * se tutto OK
456200070302     C                   MOVE      *zeros        d1ndc1
456300070313     C                   MOVE      FDGNFV        d1ndc1
456400070319     C                   z-add     FDGNFV        h1ndc_piu
456500070319     c                   z-add     dstPDR        h1pdr_piu
456600120209     c                   z-add     dstDFV        h1ddc_piu
456700070319     c                   move      dstFPP        h1fpp_piu
456800080929      *
456900080929      * Controlla se la distinta è gestita con il PDA
457000080929      *  se in test nel filler
457100080929     c                   eval      ddstFLR = dstFLR
457200160506     c                   if        DSTTPM <> 'M' and §DSTTURNO =' '
457300160506     c                   eval         §DSTTURNO = '1'
457400160506     c                   end
457500160506     c                   eval      h1tur_piu = §DSTTURNO
457600080929     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
457700080929     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
457800080929     c                   eval      h1PDA_piu = 'S'
457900080929     c                   else
458000080929     c                   clear                   h1PDA_piu
458100080929     c                   end
458200070313      *
458300070313      * intestazione SF1
458400070531     C                   if        d1sce = Assegnare and h1ndc_piu > 0
458500070313     c                   movel     Alla_Dist     c1ndc1
458600070319     c                   move      h1ndc_piu     distintalfa
458700070313     c                   do        7             xy
458800070313     c                   if        ndc_a(xy) <> '0'
458900070313     c                   leave
459000070313     c                   end
459100070313     c                   move      ' '           ndc_a(xy)
459200070313     c                   enddo
459300070313     c                   move      distintalfa   c1ndc1
459400070313      *
459500070313     c                   eval      C2dsce = OPZ_1
459600070313     c                   eval      C2dess = 'alla Distinta nr.' +
459700070319     c                             %trim(%editc(h1ndc_piu:'Z'))
459800070313     c                   end
459900070313     C*
460000070302     C     EndF05        ENDSR
460100070227     C/EJECT
460200070301      *---------------------------------------------------------------*
460300070320      *?  GESTIONE F08 x interrogare Anagrafica Autisti              ?
460400070301      *---------------------------------------------------------------*
460500070227     C     F08_interr    BEGSR
460600070227     C*
460700070227     c                   clear                   fnlv24ds
460800070227     C                   Z-ADD     d1cFGS        d24FIL
460900070227     C                   MOVEL     'R'           d24FLG
461000070227     C                   MOVEL     'A'           d24tip
461100070228     c                   eval      kpjbus = kpjbu
461200070227     C                   MOVEL(p)  fnlv24ds      KPJBU
461300070227     C                   CALL      'FNLV24R'
461400070227     C                   PARM                    KPJBA
461500070227     C                   MOVEL     KPJBU         fnlv24ds
461600070228     c                   eval      kpjbu = kpjbus
461700070227     C*
461800070227     C                   ENDSR
461900070302     C/EJECT
462000070302      *---------------------------------------------------------------*
462100070320      *?  GESTIONE F06 x aggiornare il Data Base                     ?
462200070302      *---------------------------------------------------------------*
462300070302     C     F06_Aggiorna  BEGSR
462400070306      **
462500070306      **  Leggendo sempre le bolle selezionate dal 2°SFL
462600070309      **   esegue aggiornamento degli archivi e se ci sono problemi
462700070309      **  durante l'aggiornamento delle singole bolle emette 3°SFL con la
462800070309      **   lista delle bolle non aggiornate avvertendo del mancato aggiornamento.
462900070309      **
463000070319      ** L'utente deve quindi rieseguire l'operazione poichè viene annullata tutta
463100070319      ** la transazione con il Rolback.
463200070309      **
463300070309     C*  Esegue l'aggiornamento ed il controllo sul 3° SFL
463400070309     c                   move      'S3'          $GEST
463500070309     c                   move      *on           $INZS3
463600070305     C*
463700070302     C                   ENDSR
463800070301     C/EJECT
463900070301      *---------------------------------------------------------------*
464000070316      *  ?  OPERAZIONI INIZIALI      ?
464100070301      *---------------------------------------------------------------*
464200940131     C     *INZSR        BEGSR
464300030113     C*
464400030113     C* Reperimento parametri
464500030113     C     *ENTRY        PLIST
464600030113     C                   PARM                    KPJBA
464700070228      *
464800080606      *  Controllo se il programma è richiamato  dal MONITOR x il PDA
464900080609     c                   setoff                                       16
465000080606     c                   clear                   PGM_Chiamante
465100080606     C                   clear                   FIDG30ds
465200080606      *
465300080606     C                   IF        %subst(kpjbu:1:10) <> *BLANK
465400080606      *
465500080606     C                   movel     KPJBU         FIDG30ds
465600080606      *
465700080606      * se si deve cambiare nome al MONITOR basta modificarlo solo qui
465800080606     c                   if        G30PGM = 'MONITOR'
465900080606     c                   eval      PGM_Chiamante = 'MONITOR'
466000080606     c                   eval      salta_video1 = 'S'
466100080609     c                   seton                                        16
466200080606      *
466300080606     c                   else
466400080606     c                   eval      PGM_Chiamante = G30PGM
466500080606     c                   end
466600080606     C                   END
466700080606      *
466800070307      *  Se richiamata da Azione/Menù
466900070308     c                   CLEAR                   se_EDP            1
467000070308     c                   if        %subst(Knmus:1:3) = 'EDP'
467100070308     c                   MOVE      'S'           se_EDP
467200070307     c                   end
467300070403      *
467400070403     c     k52j          klist
467500070403     c                   kfld                    wfgs
467600070403     c                   kfld                    wcgi
467700070404      *
467800070404     c     k52j1         klist
467900070404     c                   kfld                    wfgs
468000070404     c                   kfld                    s1CGI
468100070404      *
468200070404     c     karb70        klist
468300070404     c                   kfld                    wfgs_ifp
468400070404     c                   kfld                    h1ndc_meno
468500070307      *
468600070228     c     kgrg          klist
468700070316     c                   kfld                    wfgs
468800070228     c                   kfld                    savARGcgi
468900070301     C*
469000070301     C     Kdst          KLIST
469100070316     C                   KFLD                    wnpg
469200070301     C                   KFLD                    Wnfv
469300070316     C                   KFLD                    wfgs
469400070307     C*
469500070406     C     Kdst8         KLIST
469600070406     C                   KFLD                    wfgs
469700070316     C                   KFLD                    wnpg
469800070307     C                   KFLD                    h1cDDC
469900070301      *
470000070301     C     KTAB1         KLIST
470100070301     C                   KFLD                    tblkut
470200070301     C                   KFLD                    TBLCOD
470300100505     C                   z-add     1             tblkut
470400160429     c     ktab          klist
470500160429     c                   kfld                    tblkut
470600160429     c                   kfld                    tblcod
470700160429     c                   kfld                    tblkey
470800070307      *
470900070307     C     KTBE          KLIST
471000070307     C                   KFLD                    KCOD1
471100070307     C                   KFLD                    KKEY1
471200070301      *
471300070301     C     Karb1         KLIST
471400070301     C                   KFLD                    s2caas
471500070301     C                   KFLD                    s2clnp
471600070301     C                   KFLD                    s2cnrs
471700070301     C                   KFLD                    s2cnsp
471800150930      *
471900150930     C     Kfiar5        KLIST
472000150930     C                   KFLD                    s2caas
472100150930     C                   KFLD                    s2clnp
472200150930     C                   KFLD                    s2cnrs
472300150930     C                   KFLD                    s2cnsp
472400150930     C                   KFLD                    TRDar5            3
472500070301      *
472600070301     C     KLBL          KLIST
472700070301     C                   KFLD                    lblAAP
472800070301     C                   KFLD                    lblLPP
472900070301     C                   KFLD                    lblNRP
473000070301     C                   KFLD                    lblNSP
473100070301     C*
473200070301     C     KAR6          KLIST
473300070301     C                   KFLD                    arbAAS
473400070301     C                   KFLD                    arbLNP
473500070301     C                   KFLD                    arbNRS
473600070301     C                   KFLD                    arbNSP
473700070301     C                   KFLD                    KTRC
473800070301      *
473900070301     C     KARI          KLIST
474000070301     C                   KFLD                    arbAAS
474100070301     C                   KFLD                    arbLNP
474200070301     C                   KFLD                    arbNRS
474300070301     C                   KFLD                    arbNSP
474400070301     C                   KFLD                    KTIP
474500070316     C*
474600070316     C     Kaggiorna     KLIST
474700070316     C                   KFLD                    s2cAAS
474800070316     C                   KFLD                    s2cLNP
474900070316     C                   KFLD                    s2cNRS
475000070316     C                   KFLD                    s2cNSP
475100070417     C*
475200070417     C     Karg          KLIST
475300070417     C                   KFLD                    arbAAS
475400070417     C                   KFLD                    arbLNP
475500070417     C                   KFLD                    arbNRS
475600070417     C                   KFLD                    arbNSP
475700070305     C*
475800070305     C     Kapd          KLIST
475900070305     C                   KFLD                    tipo_cod          1
476000070305     C                   KFLD                    cod_padr          7 0
476100070301     C*
476200070305     C                   move      'A'           tipo_cod
476300070829     C*
476400070829     c     k_PRM         klist
476500070829     c                   kfld                    k_PRMfgs          3 0
476600070829     c                   kfld                    k_PRMpgm         10
476700070301      *
476800070227      * Reperisco dati job
476900070227     c                   exsr      DatiJob
477000070227
477100030113     C* Variabili per gestione videate
477200070227     C                   MOVE      *BLANK        test_Parallelo    1
477300030113     C                   MOVE      *BLANK        $GEST             2
477400070305     C                   MOVE      *BLANK        sav$GEST          2
477500030113     C                   MOVE      *BLANK        $FINE             1
477600070227     C                   MOVE      *BLANK        $INZD1            1
477700070227     C                   MOVE      *BLANK        $INZW1            1
477800070227     C                   MOVE      *BLANK        $INZS1            1
477900070227     C                   MOVE      *BLANK        $INZS2            1
478000070309     C                   MOVE      *BLANK        $INZS3            1
478100070306     C                   MOVE      *BLANK        $VISS2            1
478200030113     C                   MOVE      *BLANK        $EFILE            1
478300030113     C                   MOVE      *BLANK        $ESCI             1
478400030113     C                   MOVE      *BLANK        $RCDOK            1
478500030113     C* Indici
478600030113     C                   Z-ADD     0             X                 3 0
478700030113     C                   Z-ADD     0             Y                 3 0
478800070227     C***
478900070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
479000070227     C***
479100070227     C                   TIME                    W0140            14 0
479200070227     C                   MOVE      W0140         UDATE8            8 0
479300070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
479400070319     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
479500070227     C*
479600070227     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
479700070227     C                   MOVEL     *BLANK        G02ERR
479800070227     C                   CALL      'XSRDA8'
479900070227     C                   PARM                    WLBDAT
480000070301     c                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
480100070307     C*
480200070307     C*  Aggancio tabella 'DGD' x reperire LIMITI CONTROLLO DISTINTA
480300070307     C                   MOVEL     'DGD'         KCOD1
480400070307     C                   MOVEL     *BLANKS       KKEY1
480500070307     C                   MOVEL     '1'           KKEY1
480600070307     C     KTBE          CHAIN     TNTBE01L
480700070307     C                   If        %Found(TNTBE01L)
480800070307     C                   MOVEL     TBEUNI        DDGD
480900070307     C                   END
481000160317     C*  VEDI PACCO DPD
481100160317     C                   clear                   GIRO_vd_Pacco
481200160317     C                   MOVEL     'NSD'         KCOD1
481300160317     C                   MOVEL     *BLANKS       KKEY1
481400160317     C                   MOVEL     '1'           KKEY1
481500160317     C     KTBE          CHAIN     TNTBE01L
481600160317     C                   If        %Found(TNTBE01L)
481700160317     C                   MOVEL     TBEUNI        DNSD
481800160317     C                   MOVEL     §nsdCGI       GIRO_vd_Pacco
481900160317     C                   END
482000100505     C*
482100070301     C*  CARICO DATI TABELLA 1P
482200070301     C                   MOVEL     '1P'          TBLCOD
482300070301     C                   Z-ADD     0             X
482400070301     C     KTAB1         setll     TABEL00F
482500070301     C     KTAB1         reade     TABEL00F
482600070302     C                   DOW       not %Eof(TABEL00F)  and (x < 50)
482700070301     C                   ADD       1             X
482800070301     C                   MOVEL     TBLUNI        DS1P
482900070301     C                   MOVEL     TBLKEY        C1P(X)
483000070301     C                   MOVEL     §1PDES        D1P(X)
483100070301     C     KTAB1         reade     TABEL00F
483200070301     C                   END
483300070301     C*
483400070301     C*  CARICO DATI TABELLA 3A
483500070301     C                   MOVEL     '3A'          TBLCOD
483600070301     C                   Z-ADD     0             X
483700070301     C     KTAB1         setll     TABEL00F
483800070301     C     KTAB1         reade     TABEL00F
483900070302     C                   DOW       not %Eof(TABEL00F) and (x < 100)
484000070301     C                   ADD       1             X
484100070301     C                   MOVEL     TBLKEY        C3A(X)
484200070301     C                   MOVEL     TBLUNI        D3A(X)
484300070301     C     KTAB1         reade     TABEL00F
484400070301     C                   END
484500940127     C*
484600100518     C*  CARICO DATI TABELLA 7r part. consegne attesa e Ritiri Contestuali
484700100505     C                   MOVEL     '7R'          TBLCOD
484800100505     C                   Z-ADD     0             X
484900100518     C                   Z-ADD     0             wRC               3 0
485000100505     C     KTAB1         setll     TABEL00F
485100100505     C     KTAB1         reade     TABEL00F
485200100505     C                   DOW       not %Eof(TABEL00F) and (x < 100)
485300100505     c                   movel     tbluni        ds7r
485400100518      *
485500100518      *  carico i codici che hanno la modalità di Attesa
485600100505     c                   if        §7ratt = 'S'
485700100505     C                   ADD       1             X
485800100505     C                   MOVEL     TBLKEY        s7r(X)
485900100505     c                   end
486000100518      *
486100100518      *  carico i codici che hanno il Ritiro Contestuale
486200100518     c                   if        §7rRC  = 'S'
486300100518     C                   ADD       1             wRC
486400100518     C                   MOVEL     TBLKEY        s7rRC(wRC)
486500100518     c                   end
486600100518      *
486700100505     C     KTAB1         READE     TABEL00F
486800100505     C                   END
486900140929     C*
487000940117     C                   ENDSR
487100070227      *---------------------------------------------------------------*
487200070320      *?  Reperimento Dati del job (Utente/Operativi)                ?
487300070227      *---------------------------------------------------------------*
487400070228     c     DatiJOB       BEGSR
487500070227      *
487600070227     c     *dtaara       define    §azute        azuteds
487700070227     c     *dtaara       define    §datiute      ddatiute
487800070227      *
487900070227     c                   in(E)     *dtaara
488000070227     c                   IF        %ERROR or RSUT = *blanks
488100070227     c                   clear                   Tibs34Ds
488200070227     c                   call      'TIBS34R'
488300070227     c                   parm                    Tibs34Ds
488400070227     c                   in        *dtaara
488500070227     c                   ENDIF
488600080606      *
488700070227     c                   ENDSR
488800070301     C*---------------------------------------------------------------*
488900070320      *?  Controlla validità della spedizione                        ?
489000070301     C*---------------------------------------------------------------*
489100070301     C     Check_Sped    BEGSR
489200070301     C*
489300070301      * controlla che il record sia giusto da prendere
489400070301     c* chiamo pgm per vedere se spedizione da mettere in distinta
489500070301     c                   clear                   fidg16ds
489600070301     c                   eval      d16aasi = arbaas
489700070301     c                   eval      d16lnpi = arblnp
489800070301     c                   eval      d16nrsi = arbnrs
489900070301     c                   eval      d16nspi = arbnsp
490000150108     c*                  eval      d16consi= 'S'
490100070308      * deve essere impostata la data
490200070308     c                   eval      d16dfvi = h1cDDC
490300070829     c                   eval      d16dtaI = h1dtaI
490400070829     c                   eval      d16oraI = d1oraI
490500071210     c                   eval      d16LDCRI= h1dcsr
490600130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
490700130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
490800130221     c*******            if        arbxco <> ' '
490900130221     c                   if        arbxco > '0'
491000110727     c                   eval      d16prfoI= 'S'
491100110727     c                   end
491200070416     c                   if        test_Parallelo = 'S'
491300070416     c                   eval      d16ndci = 'G'
491400070416     c                   end
491500150108     c* se sto assegnando una spedizione presente nel giro DIR
491600150108     c* inibisco alcuni controlli di
491700150108     c* assegnabilità in quanto inutili visto che sto dirottando
491800150108     c* es. la sospensione alla consegna, la data consegna richiesta ecc.)
491900150108     c* questo lo faccio abilitando i singoli flag e non
492000150108     c* quello totale
492100150108     c                   if        argcgi = 'DIR'
492200150108     c                   eval      d16dcmi = 'S'
492300150108     c                   eval      d16fbci = 'S'
492400150108     c                   eval      d16cmci = 'S'
492500150108     c                   eval      d16lblmi= 'S'
492600150108     c                   eval      d16colci= 'S'
492700150108     c                   eval      d16parasgi='S'
492800150108     c                   else
492900150108     c                   eval      d16consi= 'S'
493000150108     c                   end
493100070301     c                   eval      kpjbus = kpjbu
493200070301     c                   eval      kpjbu = fidg16ds
493300070301     c                   call      'FIDG16R'
493400070301     c                   parm                    kpjba
493500070301     c                   parm                    fnarbds
493600070301     c                   parm                    fiargds
493700070301     c                   eval      fidg16ds = kpjbu
493800070301     c                   eval      kpjbu = kpjbus
493900070301     C*
494000070301     C                   ENDSR
494100070305     C*---------------------------------------------------------------*
494200070320      *?  Calcola Pesi Volumi e Colli da consegnare                  ?
494300070305     C*---------------------------------------------------------------*
494400070305     C     Cal_PesVol    BEGSR
494500070305     C*
494600070305     c                   eval      kpjbus = kpjbu
494700070323     c                   clear                   fidg27ds
494800070305     c                   movel     fnarbds       fnarbdsav
494900070323     c                   z-add     wddc          D27DFVI
495000070323     c                   z-add     arbAAS        d27aasI
495100070305     c                   z-add     arbLNP        d27lnpI
495200070305     c                   z-add     arbNRS        d27nrsI
495300070305     c                   z-add     arbNSP        d27nspI
495400070829     c                   z-add     h1dtaI        d27dtaI
495500070829     c                   z-add     d1oraI        d27oraI
495600070305     c                   eval      kpjbu = fidg27ds
495700070305     c                   call      'FIDG27R'
495800070305     c                   parm                    kpjba
495900070305     c                   parm                    fnarbdsav
496000070305     c                   eval      fidg27ds = kpjbu
496100070305     c                   eval      kpjbu = kpjbus
496200070305     C*
496300070305     C                   ENDSR
496400070309      *---------------------------------------------------------------*
496500070319      *     GESTIONE SFL di AGGIORNAMENTO e segnalazione problemi    ?
496600070309      *---------------------------------------------------------------*
496700070309     C     GESS3         BEGSR
496800070309     C*
496900070309     C* inizializzazione videata
497000070309     C     $INZS3        IFEQ      *ON
497100070309     C                   EXSR      INZS3
497200070309     C                   MOVE      *OFF          $INZS3
497300070309     C                   ENDIF
497400070309     C*
497500070319      *     FASE di COMMIT o ROLLBACK    ?
497600070319     c                   if        exec_RB = 'S'
497700070320      *  Esegue il ROLLBACK x un precedente ?
497800070320      *  problema su uno degli aggiornamenti  ?
497900070319     c                   ROLBK
498000070319     C                   else
498100070320      *   Esegue il COMMIT  --> TUTTO è OK. ?
498200070319     c                   COMMIT
498300080626      *
498400080626      *  Aggiornamento Archivi x PDA fuori COMMIT
498500080626     c                   exsr      aggiorna_x_PDA
498600080626      *
498700070319     C                   end
498800070319     C*
498900070309     C* Inizializza e imposta piede videata
499000070309     C                   EXSR      INZZ3
499100070309     C*
499200070309     C* emissione piede videata
499300070309      * ?            *-----------------------*
499400070309     C                   WRITE     FDG0Z03
499500070309      * ?            *-----------------------*
499600070309     C*
499700070309     C     WMAX          IFEQ      0
499800070319     c                   if        exec_RB = 'N'
499900070319     C* Se è stato aggiornato tutto correttamente deve Emettere
500000070319     C*  il messaggio di TUTTO OK.
500100070607     c                   eval      vedi_video = 'N'
500200070326     c                   eval      V03MSG = ErrMsg(25)
500300070319     c                   else
500400070319     C*  Qualcosa è andato storto
500500070607     c                   eval      vedi_video = 'S'
500600070326     c                   eval      V03MSG = ErrMsg(26)
500700070319     c                   end
500800070330      **
500900071122     c                   goto      nonVedere
501000070309      * ?            *-----------------------*
501100070309     C                   WRITE     FDG0V03
501200070309      * ?            *-----------------------*
501300071122     c     nonVedere     tag
501400070309     c                   setoff                                       30
501500070309     C                   Else
501600070309     c                   seton                                        30
501700070309      *
501800070309     C     Wsfl          IFgt      0
501900070309     C                   Z-ADD     wsfl          C3RCD
502000070309     C                   Else
502100070309     C     Wpag          IFgt      0
502200070309     C                   Z-ADD     wpag          C3RCD
502300070309     C                   EndIF
502400070309     C                   EndIF
502500070309     C                   ENDIF
502600070309      *
502700070309      *  Emissione SFL dopo Aggiornamento Data Base
502800070319      *    con errori quindi tutta la transazione deve essere rifatta.
502900070607     c                   if        vedi_video = 'S'
503000070309      * ?            *-----------------------*
503100070309     C                   EXFMT     FDG0C03
503200070309      * ?            *-----------------------*
503300070330     c                   end
503400070326     c                   setoff                                       99
503500070309      *
503600070309     C     C3NRR         IFNE      0
503700070309     C                   Z-ADD     C3NRR         WSFL
503800070309     C                   ENDIF
503900070309     C                   Z-ADD     SFLNRR        C3RCD
504000070309     C* Selezioni
5041000703091    C                   SELECT
504200070309     C* F3=Fine
504300070309     C     $TASTO        WHENEQ    F03
504400070309     C                   EXSR      F03_exit
504500070309      *
5046000703091O   C                   OTHER
504700070309     C* CONTROLLO testata
504800070309     C                   EXSR      CTRC3
504900070309     C* CONTROLLO subfile
505000070309     C     *IN99         IFEQ      *OFF
505100070309     C                   EXSR      CTRS3
505200070309     C                   END
505300070309      *
505400070309      * Riemette il primo video
505500070309     C                   movel     'D1'          $GEST
505600070309      *   non reinizializzato se ci sono stati problemi
505700070309     C     WMAX          ifgt      0
505800070309     C                   MOVE      *OFF          $INZD1
505900070316     c                   rolbk
506000070316      *
506100070309     c                   else
506200070309      *   reinizializzato se non ci sono stati problemi
506300070309     C                   MOVE      *ON           $INZD1
506400070309     c                   end
506500070330      *?  Attenzione?
506600070330      *?    se si era scelto di Assegnare si deve ritornare sul SFL dei GIRI
506700070504      *?    dove era impostata la scelta e pulire la distinta x riselezionarla.
506800070531     c                   if        d1sce = Assegnare
506900070330     C                   movel     'S1'          $GEST
507000070413     C                   MOVE      *ON           $INZS1
507100070504     c                   clear                   h1ndc_piu
507200070330     c                   eval      WSfl  =  sav_WSfl
507300070330     c                   eval      Wmax  =  sav_Wmax
507400070330     c                   eval      Wpag  =  sav_Wpag
507500070404      * avendo assegnato l'unico record non posso riemettere il SFL vuoto
507600070404      *   quindi torno al video introduttivo.
507700070404     c                   if        Wmax  = 1
507800070404     C                   movel     'D1'          $GEST
507900070404     C                   MOVE      *ON           $INZD1
508000070404     c                   end
508100070607      *
508200070607     c                   eLSe
508300070607      *  se togliere o trasferire
508400070607      *   voglio riportare direttamente sull'interrogazione deistinte
508500070607     c                   eval      salva_d1sce = d1sce
508600070607     c                   eval      salta_video1 = 'S'
508700070607     C                   movel     'D1'          $GEST
508800070607     C                   MOVE      *Off          $INZD1
508900070607     c                   exsr      INZD1
509000070607     c                   eval      d1sce = salva_d1sce
509100070607      *
509200070607     c                   end
509300070607      *
5094000703091-   C                   ENDSL
509500070309     C*
509600070309     C                   ENDSR
509700070309     C/EJECT
509800070309      *---------------------------------------------------------------*
509900070320      *?  INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                ?
510000070309      *---------------------------------------------------------------*
510100070309     C     INZZ3         BEGSR
510200070309     C*
510300070309     C* Messaggio o tasti funzionali
510400070309     c                   clear                   z3dmsg
510500070309     c                   clear                   z3der1
510600070309     c                   clear                   z3der2
510700070309     C*
510800070309     C* se ci sono dei records allora ci sono stati dei problemi
510900070319     C*  durante l'aggiornamento bolla x bolla
511000070309     C     WMAX          IFGT      0
511100070309     C*
511200070309     c                   seton                                        51
511300070326     c                   movel     ErrMsg(21)    z3der1
511400070326     c                   movel     ErrMsg(22)    z3der2
511500070309     C*
511600070319     c                   ELSE
511700070319     C* altrimenti
511800070319     C*  Se ci sono stati dei problemi durante gli aggiornamenti
511900070319     C*   o su ARB o su DST tanto da eseguire comunque il ROLLBACK
512000070319     C*    imposta un altro tipo di messaggio.
512100070319     c                   if        exec_RB = 'S'
512200070319     c                   seton                                        51
512300070326     c                   movel     ErrMsg(23)    z3der1
512400070326     c                   movel     ErrMsg(24)    z3der2
512500070319     c                   end
512600070319     C*
512700070309     c                   END
512800070309     C*
512900070309      * tasti funzionali se non è stato emesso alcun messaggio
513000070309     c                   if        z3dmsg = *blank
513100070309     c                   setoff                                       99
513200070309     c                   movel     Fun_3         z3dmsg
513300070309     c                   eval      %subst(z3dmsg:40:13) = FUN_9_Drop
513400070309     c                   end
513500070309     C*
513600070309     C                   ENDSR
513700070309     C/EJECT
513800070309      *---------------------------------------------------------------*
513900070320      *?  INIZIALIZZAZIONE LISTA Bolle da Giri o da Distinta         ?
514000070309      *---------------------------------------------------------------*
514100070309     C     INZS3         BEGSR
514200070309     C*
514300070309     C* reset indicatori DSPATR
514400070309     c                   exsr      INZ_indERR
514500070309     C* pulizia SFL
514600070309     C                   SETOFF                                         3031
514700070309     C                   WRITE     FDG0C03
514800070309     C                   SETON                                          31
514900070309     C*
515000070309     C* Righe x Pagina del 1° SFL
515100070427     c                   z-add     RigPag_SFl3   s3pag
515200070309      *
515300070309     C* CARICAMENTO SFL totale
515400070309     C                   Z-ADD     0             S3NRR
515500070309     C                   Z-ADD     1             C3RCD
515600070309     C                   Z-ADD     0             WMAX
515700070309     C                   Z-ADD     0             Wsfl
515800070309     C* campi titolo
515900070309     C                   MOVEL     dsPGM         c3CPGM
516000070309     C                   MOVEL     knsif         c3nsif
516100070309     C                   MOVEL     knmus         c3nmus
516200070309     C                   MOVEL     RSUT          c3crsu
516300070309      * TIPO FUNZIONE
516400070309     C                   SELECT
516500070531     C                   when      d1sce = Assegnare
516600070309     c                   eval      C3dsce = OPZ_1
516700070309     c                   eval      C3dess = 'alla Distinta nr.' +
516800070319     c                             %trim(%editc(h1ndc_piu:'Z'))
516900070309      *
517000070531     C                   when      d1sce = Togliere
517100070309     c                   eval      C3dsce = OPZ_2
517200070309     c                   eval      C3dess = 'dalla Distinta nr.' +
517300070319     c                             %trim(%editc(h1ndc_meno:'Z'))
517400070309      *
517500070531     C                   when      d1sce = Trasferire
517600070309     c                   eval      C3dsce = OPZ_3
517700070309     c                   eval      C3dess = 'dalla Distinta nr.' +
517800070319     c                             %trim(%editc(h1ndc_meno:'Z'))
517900070309      *
518000070309     c                   endSL
518100070309      *
518200070309      *   Carica il SFL
518300070309     c                   EXSR      ROLS3
518400070309     C*
518500070309     C                   Z-ADD     1             WPAG
518600070309     C*
518700070309     C                   ENDSR
518800070309      *---------------------------------------------------------------*
518900070320      *? Esegue aggiornamento del Data Base caricando il SFL3 x problemi ?
519000070309      *---------------------------------------------------------------*
519100070309     C     ROLS3         BEGSR
519200070309     C*
519300070309     C                   SETOFF                                       32
519400070309     C                   Z-ADD     0             Y
519500070309     C                   Z-ADD     WMAX          S3NRR
519600070309     C*
519700070319      *? L'aggiornamento è pilotato dalle scelte effettuate sul SFL2 ?
519800070319      *? delle bolle e il caricamento del SFL3 serve per elencare    ?
519900070319      *? le bolle che non sono state aggiornate x qualsiasi motivo   ?
520000070319      *? classico: ALLOCAZIONE DEL RECORD.                           ?
520100070319     C*
520200070319     c                   exsr      AGG_da_SFL2
520300070319      *
520400070319      *  Dopo aver aggiornato i files x singola bolla, aggiorna ?
520500070319      *   a livello di distinta. Sempre che già non sia da      ?
520600070319      *   eseguire il ROLL_BACK x precedenti problemi.          ?
520700070319     C*
520800070319     c                   if        Exec_RB <> 'S'
520900070319     c                   exsr      Agg_x_Distinta
521000070319     c                   end
521100070319      *
521200070309     C* Fine Caricamento
521300070309     C                   Z-ADD     S3NRR         WMAX                 30
521400070309     C*
521500070309     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
521600070309     C     S3NRR         DIV       S3pag         PAGINE            4 0
521700070309     C                   MVR                     RESTO             3 0
521800070309     C     PAGINE        MULT      S3pag         C3RCD
5219000703091    C     RESTO         IFGT      0
522000070309     C                   ADD       1             C3RCD
5221000703091E   C                   ELSE
522200070309     C                   SUB       S3pag         C3RCD
522300070309     C                   ADD       1             C3RCD
5224000703091-   C                   ENDIF
522500070309     C*
522600070309     C                   ENDSR
522700070309      *---------------------------------------------------------------*
522800070320      *? Carica il SFL delle bolle non aggiornate se ci sono stati problemi?
522900070309      *---------------------------------------------------------------*
523000070319     C     AGG_da_SFL2   BEGSR
523100070309     C*
523200070319     C*  Si ricorda di eseguire il ROLL_BACK se si sono verificati dei
523300070319     C*   problemi in qualsiasi parte dell'aggiornamento del D.B.
523400070320     c                   move      'N'           Exec_RB           1
523500070319     C*
523600080609     C*   Legge il SFL delle Bolle dove selezionato con (1) le righe
523700080609     C*    da aggiornare
523800070531     c                   Readc     FDG0S02                                21
523900070531     C*
5240000703092    C     *IN21         DOWEQ     *OFF
524100070531     C*
524200070531     C* Solo i selezionati
524300070531     c                   if        s2SCE = '1'
524400070531     c                   move      'S'           Riga_a_1          1
524500070309      * ? ----------------------------
524600070309      * ? Carico il SFL solo se ci sono dei problemi in aggiornamento delle bolle
524700070309      * ?  per allocazione dei records o altro .
524800070309      * ? ----------------------------
524900070309     c                   clear                   problemi          1
525000070309     C*
525100070309      * ?             *--------------------------*
525200070319     c                   exsr      AGG_Riga_DB
525300070309      * ?             *--------------------------*
525400070309     C*
525500070309     C* x Problemi sui records in aggiornamento
525600070309      * ? ----------------------------
525700070309     c                   if        Problemi = 'S'
525800070319     c                   move      'S'           Exec_RB           1
525900070319      *
526000070309     C                   clear                   FDG0S03
526100070309     c                   exsr      RIGA_SFL3
526200070309      *
526300070309      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
526400070309     c                   seton                                        32
526500130221     c     h3xco         comp      '0'                                88
526600070309     C                   ADD       1             S3NRR
526700070309     C                   ADD       1             Y
526800070309     C                   WRITE     FDG0S03
526900070309     c                   End
527000070309     C*
527100070309     c                   End
527200070309     C*
527300070531     c                   Readc     FDG0S02                                21
527400070309     c                   EndDo
527500070309     C*
527600070309     C                   ENDSR
527700070309      *---------------------------------------------------------------*
527800070320      *?  Imposta Campi del 3°SFL                                    ?
527900070309      *---------------------------------------------------------------*
528000070309     C     RIGA_SFL3     BEGSR
528100070309     C*
528200070309     C* Deve semplicemente impostare dal 2° al 3° SFL i campi
528300070309     c                   z-add     S2Caas        S3Caas
528400070309     c                   z-add     S2Clnp        S3Clnp
528500070309     c                   z-add     S2Cnrs        S3Cnrs
528600070309     c                   z-add     S2Cnsp        S3Cnsp
528700110804     c                   movel     h2xco         h3xco
528800070309     c                   movel     S2Ccbo        S3Ccbo
528900070309     C                   z-add     S2Cgmc        S3Cgmc
529000070309     c                   z-add     S2Clna        S3Clna
529100070309     c                   z-add     S2Cznc        S3Cznc
529200070427     c                   z-add     S2pkf         S3Cpkf
529300070309     c                   z-add     S2Cvlf        S3Cvlf
529400070309     c                   z-add     S2Cncl        S3Cncl
529500070309     C                   MOVEL     S2Dano        S3Dano
529600070309     c                   movel     S2Crsm        S3Crsm
529700070309     c                   movel     s2CGI         s3CGI
529800070309     c                   movel     S2Crsd        S3Crsd
529900070309     c                   movel     S2Cind        S3Cind
530000070309     c                   movel     S2Ccad        S3Ccad
530100070309     c                   movel     S2Clod        S3Clod
530200070309     c                   movel     S2Cprd        S3Cprd
530300070309     C*
530400070309     C                   ENDSR
530500070309      *---------------------------------------------------------------*
530600070320      *?  CONTROLLO TESTATA LISTA                                    ?
530700070309      *---------------------------------------------------------------*
530800070309     C     CTRC3         BEGSR
530900070309     C                   MOVE      *OFF          *IN99
531000070309     C                   ENDSR
531100070309      *---------------------------------------------------------------*
531200070320      *?  CONTROLLO OPZIONI LISTA                                    ?
531300070309      *---------------------------------------------------------------*
531400070309     C     CTRS3         BEGSR
531500070309     C                   SETOFF                                       99
531600070309     C* reset indicatori DSPATR
531700070309     c                   exsr      INZ_indERR
531800070309     C                   ENDSR
531900070309     C/EJECT
532000070309      *---------------------------------------------------------------*
532100080626      * ??  Aggiornamento dei Files a livello BOLLA                  ?
532200070309      *---------------------------------------------------------------*
532300070319     C     AGG_Riga_DB   BEGSR
532400070531     c*
532500070531     c*  Bolla x Bolla ossia riga x riga
532600070531     c                   exsr      Aggiorna_arG
532700070531      *
532800070531     c                   exsr      Aggiorna_arB
532900070319     c*
533000070319     C                   ENDSR
533100070319     C/EJECT
533200070319      *---------------------------------------------------------------*
533300080626      * ??  Aggiornamento dei Files   a livello Distinta             ?
533400070319      *---------------------------------------------------------------*
533500070531     C     Aggiorna_arG  Begsr
533600070319     c*
533700070531      *   Aggiorna ARG x Bolla    ?
533800070531     c     kaggiorna     chain(e)  fiarg01l
533900070531     c*
534000070531      *  ?  Se ci sono problemi di allocazione record    ?
534100070531     c                   if        %Error
534200070531      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
534300070531     c                   eval      problemi = 'S'
534400070531     c                   clear                   trul82ds
534500070531     c                   eval      ul82§rrn = arg_nrr
534600070531     c                   eval      ul82§fil = 'FIARG01L'
534700070531     c                   eval      ul82§win = 'S'
534800070531     c                   eval      ul82§f7  = 'S'
534900070531     c                   eval      ul82§num = 2
535000070531     c                   eval      ul82§att = 2
535100070531     c                   eval      ul82§mss = ErrMsg(27)
535200070531     c                   Eval      UL82§msw = 'La Distinta '
535300070531     c                             + %editc(w1ndc1:'Z') +  ' '
535400070531     c                             + ' non è manutenzionabile.'
535500070531      * chiamo il pgm che manda il messaggio info all'utente
535600070531     c                   call(e)   'TRUL82R'
535700070531     c                   parm                    trul82ds
535800070531      * ancora allocata
535900070531     c                   if        ul82§sts = 'A'
536000070531      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
536100070531     c                   eval      problemi = 'S'
536200070531     c                   endif
536300070531     c*
536400070531     c                   else
536500070531     c*
536600070531     c                   if        %Found(fiarg01l)
536700070531      * ? Imposta i campi da aggiornare ?
536800070531     c                   select
536900070531     c*
537000070531     c* solo x righe selezionate
537100070531     c                   when      d1sce = Assegnare or d1sce = Trasferire
537200070531     c                   eval      ARGFGS     =  wfgs
537300070531     c                   eval      ARGPDC     =  h1pdr_piu
537400070531     c                   eval      ARGNDC     =  h1ndc_piu
537500120209     c*********          eval      ARGDDC     =  h1cddc
537600120209     c                   eval      argDDC     =  h1ddc_piu
537700070531     c                   eval      ARGSTP     =  0
537800070531     c                   eval      ARGNFTL    =  99999
537900070531     c                   eval      ARGSLB     =  0
538000070531     c                   eval      ARGDTVDIS  = Dateu8
538100070531     c                   eval      ARGHVDIS   = HHMMss
538200070531     c*
538300070531     c* solo x righe selezionate
538400070531     c                   when      d1sce = Togliere
538500070531     c                   clear                   ARGPDC
538600070531     c                   clear                   ARGFGS
538700070531     c                   clear                   ARGNDC
538800070531     c                   clear                   ARGDDC
538900070531     c                   clear                   ARGSTP
539000070531     c                   clear                   ARGNFTL
539100070531     c                   clear                   ARGSLB
539200070531     c                   clear                   ARGDTVDIS
539300070531     c                   clear                   ARGHVDIS
539400100505      *
539500100505      * Comunque aggiorna il flag pulendolo
539600100505      *  anche se si tratta di una bolla con la particolarità di attesa
539700100505      *  poichè in ogni caso è stato preso in cosiderazione e selezionato su questo pgm
539800100505     c                   movel     argflo        dargflo
539900100505      * in attesa
540000100505     c                   clear                   §argattesa
540100100505     c                   movel     dargflo       argflo
540200100505     c*
540300070531     c                   endsl
540400070531     c*
540500070531     c                   update    fiarg000
540600070531     c                   end
540700070531     c*
540800070531     c                   end
540900070531     c*
541000070531     C                   ENDSR
541100070531     C/EJECT
541200070531      *---------------------------------------------------------------*
541300080626      * ??  Aggiornamento dei Files   a livello Distinta             ?
541400070531      *---------------------------------------------------------------*
541500070531     C     Aggiorna_arB  Begsr
541600070531     c*
541700070531      *   Deve aggiornare  anche  ARB solo se  siamo in produzione  ?
541800070531     c                   if        test_Parallelo <> 'S'  and
541900070531     c                                   problemi <> 'S'
542000070531      * solo
542100070531      * se deve togliere
542200070531     c                   if        (d1sce = Togliere or d1sce = Trasferire) and
542300070531     c                             h1ndc_meno <> 0
542400070531      *
542500070531     c     kaggiorna     chain(e)  fnarb01l
542600070531     c*
542700070531      *  ?  Se ci sono problemi di allocazione record    ?
542800070531     c                   if        %Error
542900070531      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
543000070531     c                   eval      problemi = 'S'
543100070531     c*
543200070531     c                   clear                   trul82ds
543300070531     c                   eval      ul82§rrn = arb_nrr
543400070531     c                   eval      ul82§fil = 'FNARB01L'
543500070531     c                   eval      ul82§win = 'S'
543600070531     c                   eval      ul82§f7  = 'S'
543700070531     c                   eval      ul82§num = 2
543800070531     c                   eval      ul82§att = 2
543900070531     c                   eval      ul82§mss = ErrMsg(27)
544000070531     c                   Eval      UL82§msw = 'La Distinta '
544100070531     c                             + %editc(w1ndc1:'Z') +  ' '
544200070531     c                             + ' non è manutenzionabile.'
544300070531      * chiamo il pgm che manda il messaggio info all'utente
544400070531     c                   call(e)   'TRUL82R'
544500070531     c                   parm                    trul82ds
544600070531      * ancora allocata
544700070531     c                   if        ul82§sts = 'A'
544800070531      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
544900070531     c                   eval      problemi = 'S'
545000070531     c                   endif
545100070531     c*
545200070531     c                   else
545300070531     c*
545400070531     c                   if        %Found(fnarb01l)
545500140714      **
545600140714      ** salva la chiave della distinta/autista prima di toglierla da ARB
545700140714     c                   z-add     arbNDC        sav_arbndc
545800140714     c                   z-add     arbDDC        sav_arbddc
545900140714     c                   z-add     arbPDC        sav_arbpdc
546000080609      **
546100080609      * ?Se attivato come CONSEGNE con PDA:
546200080626     c                   if          PDAconsegne <> *blank
546300080609      **
546400080609     c                   endIf
546500080609      **
546600070531     c                   eval      arbngd = 0
546700070531     c                   eval      arbndc = 0
546800070531     c                   eval      arbpdc = 0
546900070531     c                   eval      arbifp = 0
547000070531     c                   eval      arbddc = 0
547100070531     c                   eval      arbstp = 0
547200070531     c                   eval      arbngd = 0
547300070531     c                   eval      arbnss = 0
547400070531     c                   update    fnarb000
547500140711      **
547600140711      **  Imposta l'evento "NIC" se era stato emesso un "MIC" sulla spedizione
547700140714     c                   exsr      Aggio_EVB_NIC
547800071008      **
547900071008     c* ritiro contestuale alla consegna:
548000071008      **  se è stata annullata la consegna deve annullare anche il ritiro
548100071008      **  che era stato ordinato contestualmente alla consegna.
548200071008     c                   exsr      Aggiorna_ORM
548300070531     c                   end
548400070531     c*
548500070531     c                   end
548600070531      *
548700070531     c                   end
548800070531     c                   end
548900070531     c*
549000070531     C                   ENDSR
549100070531     C/EJECT
549200140711      *---------------------------------------------------------------*
549300140711      * ??  imposta l'Evento di NIC sulla spedizione che aveva MIC   ?
549400140711      *---------------------------------------------------------------*
549500140714     C     Aggio_EVB_NIC BEGSR
549600140711      ***
549700140711      ***   richiama la routine che si occupa di impostare il NIC
549800140711     c                   clear                   fnlrq8ds
549900140711     c                   eval      kpjbus = kpjbu
550000140711     c                   eval      LRQ8CMTI =  'S'
550100140711     c                   eval      LRQ8AASI =  arbaas
550200140711     c                   eval      LRQ8LNPI =  arblnp
550300140711     c                   eval      LRQ8NRSI =  arbnrs
550400140711     c                   eval      LRQ8NSPI =  arbnsp
550500140711     c                   eval      LRQ8FGSI =  d1cFGS
550600140714     c                   eval      LRQ8NDCI =  sav_arbndc
550700140714     c                   eval      LRQ8DDCI =  sav_arbddc
550800140714     c                   eval      LRQ8PDCI =  sav_arbpdc
550900140711     c                   eval      LRQ8NPGI =  4
551000140711     c                   eval      LRQ8FPPI =  h1fpp_meno
551100140714     c                   eval      LRQ8MICI =  ' '
551200140711     c                   eval      LRQ8TLAI =  'L'
551300140711     c                   eval      kpjbu = fnlrq8ds
551400140711     c                   call      'FNLRQ8R'
551500140711     c                   parm                    kpjba
551600140711     c                   eval      fnlrq8ds = kpjbu
551700140711     c                   if        LRQ8ESIO <> *blanks
551800140711      * ?  Messaggio di ERRORE                   MSG   ?
551900140711      **         cosa si deve fare ????? se errore
552000140711      **
552100140711     c                   end
552200140711     c                   eval      kpjbu = kpjbus
552300140711     c*
552400140711      ***
552500140711     C                   ENDSR
552600140711     C/EJECT
552700140711      *---------------------------------------------------------------*
552800071008      *---------------------------------------------------------------*
552900071008      * ??  toglie sull'orm x particolarità RC                       ?
553000071008      *---------------------------------------------------------------*
553100071008     C     Aggiorna_ORM  BEGSR
553200071008     c*
553300071008     c*  Se l'autista va a consegnare dal cliente deve anche Ritirare qualcosa
553400071008     c*  se ha il ritiro contestuale alla consegna.
553500100518     c********           if        arbgma = 'RC'
553600100518      ***
553700100518      *****   Adesso deve testare se il tipo consegna particolare
553800100518      *****     è inerente al ritiro contestuale.
553900100518      ***
554000100518     C     arbgma        lookup    s7rRC                                  23
554100100518     C                   IF        *in23
554200071008     c                   clear                   fnlr99ds
554300071008     c                   eval      i99tla = 'L'
554400071008     c                   eval      i99aas = arbaas
554500071008     c                   eval      i99lnp = arblnp
554600071008     c                   eval      i99nrs = arbnrs
554700071008     c                   eval      i99nsp = arbnsp
554800071008     c                   eval      i99fgs = d1cFGS
554900071008     c                   eval      i99ndc = arbndc
555000071008     c                   eval      i99ddc = arbddc
555100071008     c                   eval      i99comit ='1'
555200071008     c                   eval      i99cmt ='N'
555300071008     c                   eval      i99fao = 390
555400071008     c                   call      'FNLR99R'
555500071008     c                   parm                    kpjba
555600071008     c                   parm                    fnlr99ds
555700071008     c                   end
555800071008     c*
555900071008     C                   ENDSR
556000071008     C/EJECT
556100070531      *---------------------------------------------------------------*
556200080626      * ??  Aggiornamento dei Files   a livello Distinta             ?
556300070531      *---------------------------------------------------------------*
556400070531     C     AGG_x_DistintaBEGSR
556500070531     c*
556600070320      *   Deve aggiornare  anche  ARB solo se  siamo in produzione  ?
556700070319      * ? Se deve Assegnare: ?
556800070319      * ? AGGANCIA ARB  mediante il FIDG23R ma a livello di distinta ?
556900070319     c                   IF        test_Parallelo <> 'S'  and Exec_RB <> 'S'
557000070319      * se deve ASSEGNARE
557100070319     c                   if        h1ndc_piu <> 0
557200070319     c                   clear                   fidg23ds
557300070319     c                   eval      d23CMTI = 'N'
557400070319     c                   eval      d23FGSI = wFGS
557500070319     c                   eval      d23NFTL = 99999
557600120209     c******             eval      d23DFVI = h1cddc
557700120209     c                   eval      d23DFVI = h1ddc_piu
557800070319     c                   eval      d23NFDI = h1ndc_piu
557900070319     c                   eval      d23NFAI = h1ndc_piu
558000070319     c                   eval      d23FPPI = h1FPP_piu
558100070319     c                   eval      kpjbus = kpjbu
558200070319     c                   eval      kpjbu = fidg23ds
558300070531     c                   call      'FIDG23C'
558400070319     c                   parm                    kpjba
558500070319     c                   eval      fidg23ds = kpjbu
558600070319     c                   if        d23erro <> *blanks
558700070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
558800070319     c                   eval      Exec_RB  = 'S'
558900070319     c                   end
559000070319     c                   eval      kpjbu = kpjbus
559100070319     c                   end
559200070316     c*
559300070319     c                   endIF
559400070319     c*
559500070320      *   Deve aggiornare la o le Distinte   ?
559600070319     c                   IF        Exec_RB  <> 'S'
559700070319      *
559800070319      *   richiamo in aggiornamento D.B.     ?
559900070319     c                   move      w1ndc1        wNFV
560000070319      *                         ?  =================  ?
560100070319     c                   eval      Aggior_Dist = 'S'
560200070319      *                         ?  =================  ?
560300070319     c                   exsr      CHAIN_Dist
560400070319      *
560500070319     c                   if        Error_Dist   = 'S'  or
560600070319     c                             Rec_Allocato = 'S'  or
560700070319     c                             Found_Dist   = 'N'
560800070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
560900070319     c                   eval      Exec_RB  = 'S'
561000070319     c                   end
561100070319      *
561200070319     c                   endIF
561300070319     c*
561400070319     c* se si tratta di un trasferimento c'è il 2°aggancio alla distinta
561500070319     c                   IF        w1ndc2  > 0  and Exec_RB  <> 'S'
561600070319     c*
561700070319      *   richiamo in aggiornamento D.B.     ?
561800070319     c                   move      w1ndc2        wNFV
561900070319      *                         ?  =================  ?
562000070319     c                   eval      Aggior_Dist = 'S'
562100070319      *                         ?  =================  ?
562200070319     c                   exsr      CHAIN_Dist
562300070319      *
562400070319     c                   if        Error_Dist   = 'S'  or
562500070319     c                             Rec_Allocato = 'S'  or
562600070319     c                             Found_Dist   = 'N'
562700070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
562800070319     c                   eval      Exec_RB  = 'S'
562900080609     c                   elSE
563000080609      **
563100070319     c                   end
563200070319      *
563300070319     c                   endIF
563400070319     c*
563500070309     C                   ENDSR
563600070309     C/EJECT
563700080626      * ?-------------------------------------------------------------*?
563800080626      *? esegue aggiornamenti x PDA eseguendo la Quadratura Distinta  ?
563900080626      * ?-------------------------------------------------------------*?
564000080626     C     Aggiorna_x_PDAbegsr
564100080626     C*
564200080626      **?  Se attivato come CONSEGNE con PDA: ?
564300080626      * ?   Allinea i file di trasmissione al PDA utilizzando  ?
564400080626      ** ?   la funzione di Quadratura fra ARB e PDC (trsm)
564500080704     c                   if          PDAconsegne <> *blank
564600080704      *
564700080704     c                   If          d1SCE <> Assegnare
564800080916      *
564900080916      *  se si tratta di distinta con PDA
565000080916     c                   If          h1PDA_meno = 'S'
565100080626      *   Comunque Toglie
565200080630      *     avendo prima già controllato e forzato
565300080630      *     non si deve più eseguire il controllo
565400080626     c                   clear                   fidg43ds
565500080626     c                   eval      CO43TLA =  'Q'
565600080626     c                   eval      CO43FGS =  wFGS
565700080626     c                   eval      CO43NDC =  h1ndc_meno
565800120209     c********           eval      CO43DDC =  h1cddc
565900120209     c                   eval      CO43DDC =  h1ddc_meno
566000080701     c                   eval      CO43ExlChk =  'S'
566100080626     c                   eval      kpjbus = kpjbu
566200080626     c                   movel     fidg43ds      kpjbu
566300080626     c                   call      'FIDG43R'
566400080626     c                   parm                    kpjba
566500080626     c                   movel     kpjbu         fidg43ds
566600080626     c                   eval      kpjbu = kpjbus
566700080626      *
566800080626      * se c'è stato un errore
566900080626     c                   if        co43err <> ' '
567000080626     c                   endif
567100080916      **
567200080704     c                   end
567300080916     c                   endIf
567400080626      **
567500080916      **
567600080704     c                   if          d1SCE = Trasferire or
567700080704     c                               d1SCE = Assegnare
567800080916      **
567900080916      *  se si tratta di distinta con PDA
568000080916     c                   If          h1PDA_piu  = 'S'
568100080626      *   Comunque Trasferisce
568200080630      *     avendo prima già controllato e forzato
568300080630      *     non si deve più eseguire il controllo
568400080626     c                   clear                   fidg43ds
568500080626     c                   eval      CO43TLA =  'Q'
568600080626     c                   eval      CO43FGS =  wFGS
568700080626     c                   eval      CO43NDC =  h1ndc_piu
568800120209     c*********          eval      CO43DDC =  h1cddc
568900120209     c                   eval      CO43DDC =  h1ddc_piu
569000080701     c                   eval      CO43ExlChk = 'S'
569100080626     c                   eval      kpjbus = kpjbu
569200080626     c                   movel     fidg43ds      kpjbu
569300080626     c                   call      'FIDG43R'
569400080626     c                   parm                    kpjba
569500080626     c                   movel     kpjbu         fidg43ds
569600080626     c                   eval      kpjbu = kpjbus
569700080626      *
569800080626      * se c'è stato un errore
569900080626     c                   if        co43err <> ' '
570000080626     c                   endif
570100080916     c                   end
570200080626     c                   endif
570300080626      **
570400080626      * se richiamato da MONITOR deve uscire direttamente x tornare al MONITOR
570500080626     c                   if        PGM_Chiamante = 'MONITOR'
570600080626     C                   MOVE      *On           $FINE
570700080626     c                   endIf
570800080626     c*
570900080626     c                   endIf
571000080626     c*
571100080626     C                   ENDSR
571200080626     C/EJECT
571300070427      * ?-------------------------------------------------------------*?
571400070427      *? Riordina comunque il SFL                                     ?
571500070427      * ?-------------------------------------------------------------*?
571600070515     C     Clear_Ordina  BEGSR
571700070427     C*
571800070515     c                   eval      posizionamento = 'N'
571900070515     c                   eval      posizionato    = 'N'
572000070515     C                   clear                   c2pos1
572100070515     c                   clear                   c2pos2
572200070515     C                   clear                   c2pos3
572300070515     c                   clear                   c2pos4
572400070515     C                   clear                   c2pos5
572500070515     C                   clear                   c2pos6
572600070515     C                   clear                   sav_pos1
572700070515     C                   clear                   sav_pos2
572800070515     C                   clear                   sav_pos3
572900070515     C                   clear                   sav_pos4
573000070515     C                   clear                   sav_pos5
573100070515     C                   clear                   sav_pos6
573200070515     c*
573300070515     C                   ENDSR
573400070515     C/EJECT
573500070515      * ?-------------------------------------------------------------*?
573600070515      *? Riordina comunque il SFL                                     ?
573700080610      *?  Ma se chiamato dalla Funzione MONITOR x PDA                 ?
573800080610      *?   Deve mostrare prima le spedizioni con INFO da Visionare    ?
573900070515      * ?-------------------------------------------------------------*?
574000070515     C     Ordina_SFL2   BEGSR
574100070514     C*
574200070514     C*  Totale Records Caricati nel 2°SFL
574300070427     c                   eval      wsfl = 1
574400070427     C*
574500070427     C*  se richiesto Ordinamento sul SFL da operatore --> F11
574600070427     c                   setoff                                       010203
574700070427     c                   setoff                                       040506
574800070427     c                   setoff                                       07
574900070427     C*
575000070427      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
575100070427      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
575200070427      * Il campo è usato per selezionare i records dando un ordine a quelli
575300070427      * selezionati davanti ai non selezionati.
575400070427     C                   CLEAR                   QLGSCB
575500070427     C                   CLEAR                   QLGSCB00
575600070427      *
575700070427     c                   select
575800070427      *
575900070427      *?  Ordinamento per GIRO/Destinatario  ?
576000070427     C                   when      WrkSort = Ord_GIRODEST
576100070427     c                   move      Ord_GIRODEST  wINDsort
576200070427      *
576300070427      * x Giro (Illumina il campo) con lo 07 = *on
576400070427     c                   seton                                        07
576500070427      *
576600080610      * ? se MONITOR prima le INFO:?
576700080610     c                   if        PGM_Chiamante = 'MONITOR'
576800080610     C                   EVAL      QLGNBRK    = 3
576900080610      *
577000080610     ** La Priorità se richiesta ricerca di stringa
577100080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
577200080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
577300080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)+ %SIZE(H2Subst)
577400080610     C                   EVAL      QLGSS    = %SIZE(S2cCOK)
577500080610     C                   EVAL      QLGDT    = Carattere
577600080610     C                   EVAL      QLGSO    = Discendente
577700080610     C                   EVAL      QLGKL(1) = QLGSKL
577800080610
577900080610     ** Il GIRO è in posizione (2) 10 Bytes char ascending
578000080610     C                   EVAL      QLGSP      = 1
578100080610     C                   EVAL      QLGSS      = %SIZE(S2CGI)
578200080610     C                   EVAL      QLGDT      = Carattere
578300080610     C                   EVAL      QLGSO      = Ascendente
578400080610     C                   EVAL      QLGKL(2)   = QLGSKL
578500080610
578600080610     ** La DESCRIZIONE ragione sociale  è Char ascending
578700080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
578800080610     C                   EVAL      QLGSS    = %SIZE(S2CRSD)
578900080610     C                   EVAL      QLGDT    = Carattere
579000080610     C                   EVAL      QLGSO    = Ascendente
579100080610     C                   EVAL      QLGKL(3) = QLGSKL
579200080610
579300080610     c                   else
579400080610
579500070427     ** 2 campi chiave.   x GIRO e DESTINTARIO
579600070427     C                   EVAL      QLGNBRK    = 2
579700070427
579800070427     ** Il GIRO è in posizione (1) 10 Bytes char ascending
579900070427     C                   EVAL      QLGSP      = 1
580000070427     C                   EVAL      QLGSS      = %SIZE(S2CGI)
580100070427     C                   EVAL      QLGDT      = Carattere
580200070427     C                   EVAL      QLGSO      = Ascendente
580300070427     C                   EVAL      QLGKL(1)   = QLGSKL
580400070427
580500070705     ** La DESCRIZIONE ragione sociale  è Char ascending
580600070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
580700070427     C                   EVAL      QLGSS    = %SIZE(S2CRSD)
580800070705     C                   EVAL      QLGDT    = Carattere
580900070427     C                   EVAL      QLGSO    = Ascendente
581000070427     C                   EVAL      QLGKL(2) = QLGSKL
581100080610     c                   end
581200070427      *
581300070427      *?  Ordinamento per Destinatario    ?
581400070427     C                   when      WrkSort = Ord_DESTINAT
581500070427     c                   move      Ord_DESTINAT  wINDsort
581600080610      *
581700080610      * ? se MONITOR prima le INFO:?
581800080610     c                   if        PGM_Chiamante = 'MONITOR'
581900080610     C                   EVAL      QLGNBRK    = 3
582000080610
582100080610     ** La Priorità se richiesta ricerca di stringa
582200080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
582300080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
582400080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)+ %SIZE(H2Subst)
582500080610     C                   EVAL      QLGSS    = %SIZE(S2cCOK)
582600080610     C                   EVAL      QLGDT    = Carattere
582700080610     C                   EVAL      QLGSO    = Discendente
582800080610     C                   EVAL      QLGKL(1) = QLGSKL
582900080610
583000080610     ** La Priorità se richiesta ricerca di stringa
583100080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
583200080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
583300080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)
583400080610     C                   EVAL      QLGSS    = %SIZE(H2Subst)
583500080610     C                   EVAL      QLGDT    = Numerico
583600080610     C                   EVAL      QLGSO    = Discendente
583700080610     C                   EVAL      QLGKL(2) = QLGSKL
583800080610
583900080610     ** Il DEST è in posizione (11) 20 Bytes char ascending
584000080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
584100080610     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
584200080610     C                   EVAL      QLGDT    = Carattere
584300080610     C                   EVAL      QLGSO    = Ascendente
584400080610     C                   EVAL      QLGKL(3) = QLGSKL
584500080610
584600080610     c                   else
584700070427      *
584800070511     ** 2 campi chiave.   x SUBSTring e DESTINATARIO
584900070511     C                   EVAL      QLGNBRK    = 2
585000070427
585100070511     ** La Priorità se richiesta ricerca di stringa
585200070511     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
585300070511     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
585400070511     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)
585500070511     C                   EVAL      QLGSS    = %SIZE(H2Subst)
585600070511     C                   EVAL      QLGDT    = Numerico
585700070511     C                   EVAL      QLGSO    = Discendente
585800070427     C                   EVAL      QLGKL(1) = QLGSKL
585900070511
586000070511     ** Il DEST è in posizione (11) 20 Bytes char ascending
586100070511     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
586200070511     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
586300070511     C                   EVAL      QLGDT    = Carattere
586400070511     C                   EVAL      QLGSO    = Ascendente
586500070511     C                   EVAL      QLGKL(2) = QLGSKL
586600080610      *
586700080610     c                   end
586800070427      *
586900070427      *?  Ordinamento per Indirizzo/Destinatario    ?
587000070427     C                   when      WrkSort = Ord_INDIRIZZO
587100070427     c                   move      Ord_INDIRIZZO wINDsort
587200080610      *
587300080610      * ? se MONITOR prima le INFO:?
587400080610     c                   if        PGM_Chiamante = 'MONITOR'
587500080610     C                   EVAL      QLGNBRK    = 4
587600080610
587700080610     ** La Priorità se richiesta ricerca di stringa
587800080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
587900080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
588000080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)+ %SIZE(H2Subst)
588100080610     C                   EVAL      QLGSS    = %SIZE(S2cCOK)
588200080610     C                   EVAL      QLGDT    = Carattere
588300080610     C                   EVAL      QLGSO    = Discendente
588400080610     C                   EVAL      QLGKL(1) = QLGSKL
588500080610
588600080610     ** La Priorità se richiesta ricerca di stringa
588700080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
588800080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
588900080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)
589000080610     C                   EVAL      QLGSS    = %SIZE(H2Subst)
589100080610     C                   EVAL      QLGDT    = Numerico
589200080610     C                   EVAL      QLGSO    = Discendente
589300080610     C                   EVAL      QLGKL(2) = QLGSKL
589400080610
589500080610     ** Il INDIRIZZO è in posizione (xx) 20 Bytes char ascending
589600080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
589700080610     c                                           %SIZE(S2CRSD)
589800080610     C                   EVAL      QLGSS    = %SIZE(S2Cind)
589900080610     C                   EVAL      QLGDT    = Carattere
590000080610     C                   EVAL      QLGSO    = Ascendente
590100080610     C                   EVAL      QLGKL(3) = QLGSKL
590200080610
590300080610     ** Il DEST è in posizione (11) 20 Bytes char ascending
590400080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
590500080610     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
590600080610     C                   EVAL      QLGDT    = Carattere
590700080610     C                   EVAL      QLGSO    = Ascendente
590800080610     C                   EVAL      QLGKL(4) = QLGSKL
590900080610      *
591000080610     c                   else
591100070427      *
591200070511     ** 3 campi chiave.   x INDIRIZZO/DESTINATARIO
591300070511     C                   EVAL      QLGNBRK    = 3
591400070511
591500070511     ** La Priorità se richiesta ricerca di stringa
591600070511     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
591700070511     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
591800070511     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)
591900070511     C                   EVAL      QLGSS    = %SIZE(H2Subst)
592000070511     C                   EVAL      QLGDT    = Numerico
592100070511     C                   EVAL      QLGSO    = Discendente
592200070511     C                   EVAL      QLGKL(1) = QLGSKL
592300070427
592400070427     ** Il INDIRIZZO è in posizione (xx) 20 Bytes char ascending
592500070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
592600070427     c                                           %SIZE(S2CRSD)
592700070427     C                   EVAL      QLGSS    = %SIZE(S2Cind)
592800070427     C                   EVAL      QLGDT    = Carattere
592900070427     C                   EVAL      QLGSO    = Ascendente
593000070511     C                   EVAL      QLGKL(2) = QLGSKL
593100070427
593200070427     ** Il DEST è in posizione (11) 20 Bytes char ascending
593300070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
593400070427     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
593500070427     C                   EVAL      QLGDT    = Carattere
593600070427     C                   EVAL      QLGSO    = Ascendente
593700070511     C                   EVAL      QLGKL(3) = QLGSKL
593800070427
593900080610     c                   end
594000080610      *
594100070427      *?  Ordinamento per Località/Destinatario    ?
594200070427     C                   when      WrkSort = Ord_LOCALITA
594300070427     c                   move      Ord_LOCALITA  wINDsort
594400070427      *
594500080610      * ? se MONITOR prima le INFO:?
594600080610     c                   if        PGM_Chiamante = 'MONITOR'
594700080610     C                   EVAL      QLGNBRK    = 4
594800080610
594900080610     ** La Priorità se richiesta ricerca di stringa
595000080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
595100080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
595200080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)+ %SIZE(H2Subst)
595300080610     C                   EVAL      QLGSS    = %SIZE(S2cCOK)
595400080610     C                   EVAL      QLGDT    = Carattere
595500080610     C                   EVAL      QLGSO    = Discendente
595600080610     C                   EVAL      QLGKL(1) = QLGSKL
595700080610
595800080610     ** La Priorità se richiesta ricerca di stringa
595900080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
596000080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
596100080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)
596200080610     C                   EVAL      QLGSS    = %SIZE(H2Subst)
596300080610     C                   EVAL      QLGDT    = Numerico
596400080610     C                   EVAL      QLGSO    = Discendente
596500080610     C                   EVAL      QLGKL(2) = QLGSKL
596600080610
596700080610     ** La LOCALITA' è in posizione (xx) 20 Bytes char ascending
596800080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
596900080610     c                                           %SIZE(S2CRSD) +
597000080610     c                                           %SIZE(S2CIND)
597100080610     C                   EVAL      QLGSS    = %SIZE(S2Clod)
597200080610     C                   EVAL      QLGDT    = Carattere
597300080610     C                   EVAL      QLGSO    = Ascendente
597400080610     C                   EVAL      QLGKL(3) = QLGSKL
597500080610
597600080610     ** Il DEST è in posizione (11) 20 Bytes char ascending
597700080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
597800080610     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
597900080610     C                   EVAL      QLGDT    = Carattere
598000080610     C                   EVAL      QLGSO    = Ascendente
598100080610     C                   EVAL      QLGKL(4) = QLGSKL
598200080610
598300080610     c                   else
598400080610      *
598500070511     ** 3 campi chiave.   x LOCALITA'/DESTINATARIO
598600070511     C                   EVAL      QLGNBRK    = 3
598700070511
598800070511     ** La Priorità se richiesta ricerca di stringa
598900070511     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
599000070511     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
599100070511     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)
599200070511     C                   EVAL      QLGSS    = %SIZE(H2Subst)
599300070511     C                   EVAL      QLGDT    = Numerico
599400070511     C                   EVAL      QLGSO    = Discendente
599500070511     C                   EVAL      QLGKL(1) = QLGSKL
599600070427
599700070427     ** La LOCALITA' è in posizione (xx) 20 Bytes char ascending
599800070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
599900070427     c                                           %SIZE(S2CRSD) +
600000070427     c                                           %SIZE(S2CIND)
600100070427     C                   EVAL      QLGSS    = %SIZE(S2Clod)
600200070427     C                   EVAL      QLGDT    = Carattere
600300070427     C                   EVAL      QLGSO    = Ascendente
600400070511     C                   EVAL      QLGKL(2) = QLGSKL
600500070427
600600070427     ** Il DEST è in posizione (11) 20 Bytes char ascending
600700070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
600800070427     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
600900070427     C                   EVAL      QLGDT    = Carattere
601000070427     C                   EVAL      QLGSO    = Ascendente
601100070511     C                   EVAL      QLGKL(3) = QLGSKL
601200080610      *
601300080610     c                   end
601400070427      *
601500070427      *?  Ordinamento per Peso/Destinatario    ?
601600070427     C                   when      WrkSort = Ord_PESO
601700070427     c                   move      Ord_PESO      wINDsort
601800070427      *
601900080610      * ? se MONITOR prima le INFO:?
602000080610     c                   if        PGM_Chiamante = 'MONITOR'
602100080610     C                   EVAL      QLGNBRK    = 3
602200080610
602300080610     ** La Priorità se richiesta ricerca di stringa
602400080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
602500080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
602600080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)+ %SIZE(H2Subst)
602700080610     C                   EVAL      QLGSS    = %SIZE(S2cCOK)
602800080610     C                   EVAL      QLGDT    = Carattere
602900080610     C                   EVAL      QLGSO    = Discendente
603000080610     C                   EVAL      QLGKL(1) = QLGSKL
603100080610
603200080610     ** Il PESO è in posizione (xx) 7 Digits Num descending
603300080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
603400080610     c                                           %SIZE(S2CRSD) +
603500080610     c                                           %SIZE(S2CIND) +
603600080610     c                                           %SIZE(S2CLOD)
603700080610     C                   EVAL      QLGSS    = %SIZE(S2Cpkf)
603800080610     C                   EVAL      QLGDT    = Numerico
603900080610     C                   EVAL      QLGSO    = Discendente
604000080610     C                   EVAL      QLGKL(2) = QLGSKL
604100080610
604200080610     ** Il DEST è in posizione (11) 20 Bytes char ascending
604300080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
604400080610     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
604500080610     C                   EVAL      QLGDT    = Carattere
604600080610     C                   EVAL      QLGSO    = Ascendente
604700080610     C                   EVAL      QLGKL(3) = QLGSKL
604800080610
604900080610     c                   else
605000080610      *
605100070427     ** 2 campi chiave.   x PESO/DESTINATARIO
605200070427     C                   EVAL      QLGNBRK    = 2
605300070427
605400070427     ** Il PESO è in posizione (xx) 7 Digits Num descending
605500070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
605600070427     c                                           %SIZE(S2CRSD) +
605700070427     c                                           %SIZE(S2CIND) +
605800070427     c                                           %SIZE(S2CLOD)
605900070427     C                   EVAL      QLGSS    = %SIZE(S2Cpkf)
606000070427     C                   EVAL      QLGDT    = Numerico
606100070427     C                   EVAL      QLGSO    = Discendente
606200070427     C                   EVAL      QLGKL(1) = QLGSKL
606300070427
606400070427     ** Il DEST è in posizione (11) 20 Bytes char ascending
606500070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
606600070427     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
606700070427     C                   EVAL      QLGDT    = Carattere
606800070427     C                   EVAL      QLGSO    = Ascendente
606900070427     C                   EVAL      QLGKL(2) = QLGSKL
607000080610      *
607100080610     c                   end
607200070427      *
607300070427      *?  Ordinamento per Volume/Destinatario    ?
607400070427     C                   when      WrkSort = Ord_VOLUME
607500070427     c                   move      Ord_VOLUME    wINDsort
607600080610      *
607700080610      * ? se MONITOR prima le INFO:?
607800080610     c                   if        PGM_Chiamante = 'MONITOR'
607900080610     C                   EVAL      QLGNBRK    = 3
608000080610
608100080610     ** La Priorità se richiesta ricerca di stringa
608200080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
608300080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
608400080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)+ %SIZE(H2Subst)
608500080610     C                   EVAL      QLGSS    = %SIZE(S2cCOK)
608600080610     C                   EVAL      QLGDT    = Carattere
608700080610     C                   EVAL      QLGSO    = Discendente
608800080610     C                   EVAL      QLGKL(1) = QLGSKL
608900080610
609000080610     ** Il VOLUME è in posizione (xx) 5 Digits Num descending
609100080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
609200080610     c                                           %SIZE(S2CRSD) +
609300080610     c                                           %SIZE(S2CIND) +
609400080610     c                                           %SIZE(S2CLOD) +
609500080610     c                                           %SIZE(S2CPKF)
609600080610     C                   EVAL      QLGSS    = %SIZE(S2Cvlf)
609700080610     C                   EVAL      QLGDT    = Numerico
609800080610     C                   EVAL      QLGSO    = Discendente
609900080610     C                   EVAL      QLGKL(2) = QLGSKL
610000080610
610100080610     ** Il DEST è in posizione (11) 20 Bytes char ascending
610200080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
610300080610     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
610400080610     C                   EVAL      QLGDT    = Carattere
610500080610     C                   EVAL      QLGSO    = Ascendente
610600080610     C                   EVAL      QLGKL(3) = QLGSKL
610700080610      *
610800080610     c                   else
610900070427      *
611000070427     ** 2 campi chiave.   x VOLUME/DESTINATARIO
611100070427     C                   EVAL      QLGNBRK    = 2
611200070427
611300070427     ** Il VOLUME è in posizione (xx) 5 Digits Num descending
611400070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
611500070427     c                                           %SIZE(S2CRSD) +
611600070427     c                                           %SIZE(S2CIND) +
611700070427     c                                           %SIZE(S2CLOD) +
611800070427     c                                           %SIZE(S2CPKF)
611900070427     C                   EVAL      QLGSS    = %SIZE(S2Cvlf)
612000070427     C                   EVAL      QLGDT    = Numerico
612100070427     C                   EVAL      QLGSO    = Discendente
612200070427     C                   EVAL      QLGKL(1) = QLGSKL
612300070427
612400070427     ** Il DEST è in posizione (11) 20 Bytes char ascending
612500070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
612600070427     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
612700070427     C                   EVAL      QLGDT    = Carattere
612800070427     C                   EVAL      QLGSO    = Ascendente
612900070427     C                   EVAL      QLGKL(2) = QLGSKL
613000070427      *
613100080610     c                   end
613200080610      *
613300070427     c                   endSL
613400070427      *------------
613500070427      * Load other sort parameters.
613600070427     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
613700070427     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
613800070427     C                   EVAL      QLGRT     = 8
613900070427     C                   EVAL      QLGOKL    = 80
614000070427     C                   EVAL      QLGLKE    = 16
614100070427     C                   EVAL      QLGLSS    = 290
614200070427
614300070427      * Initialize Sort I/O API fields.
614400070427     C                   EVAL      QLGRL00  = QLGRL
614500070427     C                   EVAL      QLGRC00  = 1
614600070427     C                   CLEAR                   QUSEI
614700070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
614800070427
614900070427      * First step - Initialize the sort routine.
615000070427     C                   CALL      'QLGSORT'
615100070427     C                   PARM                    QLGSCB
615200070427     C                   PARM                    NotUsed
615300070427     C                   PARM                    NotUsed
615400070427     C                   PARM                    SizeList
615500070427     C                   PARM                    ReturnSize
615600070427     C                   PARM                    QUSEC
615700070427
615800070427      * Next step - Write records to I/O routine.
615900070427     C                   EVAL      QLGRT00 = Put
616000070427
616100070503     C                   DO        RrnLast       S2nrr_xx          5 0
616200070503     C     S2nrr_xx      CHAIN     FDG0S02
616300070427
616400070427     ** Solo le righe con Selected = 'Y' sono riordinate,
616500070427     ** quindi per fare un ordinamento di tutte le righe
616600070427     ** metto 'Y' sempre.
616700070427     C                   EVAL      Selected  = 'Y'
616800070427
616900070427     C                   CLEAR                   QUSEI
617000070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
617100070427      *
617200070427     C                   CALL      'QLGSRTIO'
617300070427     C                   PARM                    QLGSCB00
617400070427     C                   PARM                    SflRcd
617500070427     C                   PARM                    NotUsed
617600070427     C                   PARM                    SizeList
617700070427     C                   PARM                    NotUsed
617800070427     C                   PARM                    QUSEC
617900070427
618000070427     C                   ENDDO
618100070427
618200070427      * Next step - Signal end of input, clear subfile for reload.
618300070427     C                   EVAL      QLGRT00 = EndPut
618400070427     C                   CLEAR                   QUSEI
618500070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
618600070427
618700070427     C                   CALL      'QLGSRTIO'
618800070427     C                   PARM                    QLGSCB00
618900070427     C                   PARM                    SflRcd
619000070427     C                   PARM                    NotUsed
619100070427     C                   PARM                    SizeList
619200070427     C                   PARM                    NotUsed
619300070427     C                   PARM                    QUSEC
619400070427      *
  pulizia SFL 2  ?
619500070427     C                   SETOFF                                         3031
619600070427     C                   WRITE     FDG0C02
619700070427     C                   SETON                                          31
619800070427      *
619900070427
620000070427      *
  Campo di Ordinamento in REVERSE ?
620100070427     c                   if        wINDsort  > 0
620200070427     C                   move      *on           *in(wINDsort)
620300070427     c                   end
620400070427
620500070427      *
  Campo di Posizionamento  ?
620600070427     c   01              eval      C2DESp = '     Posiz. x GIRO :'
620700070515     c   02              eval      C2DESp = 'Scans. x DESTINAT. :'
620800070515     c   03              eval      C2DESp = 'Scans. x INDIRIZZO :'
620900070515     c   04              eval      C2DESp = 'Scans. x  LOCALITA :'
621000070427     c   05              eval      C2DESp = '   Posiz. x VOLUME :'
621100070427     c   06              eval      C2DESp = '     Posiz. x PESO :'
621200070427
621300070427      * Final step - Write the records back to the subfile.
621400070427     C                   EVAL      QLGRT00 = Get
621500070427
621600070503     C                   DO        RrnLast       S2nrr_xx
621700070503     c                   eval      S2nrr = S2nrr_xx
621800070427     C                   CLEAR                   QUSEI
621900070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
622000070427     C                   CALL      'QLGSRTIO'
622100070427     C                   PARM                    QLGSCB00
622200070427     C                   PARM                    NotUsed
622300070427     C                   PARM                    SflRcd
622400070427     C                   PARM                    QLGRL00
622500070427     C                   PARM                    NotUsed
622600070427     C                   PARM                    QUSEC
622700070503      * SFLnxtCHG
622800070503     c                   if        s2sce <> *blank
622900070503     c                   seton                                        32
623000070515     c                   else
623100070515     C                   SetOFF                                       32
623200070503     c                   end
623300101130     c                   eval      *in45 = H2in45
623400130221     c     h2xco         comp      '0'                                88
623500070427     C                   WRITE     FDG0S02
623600070427     C                   ENDDO
623700070427      *
623800070503      *  All'uscita di queste chiamate a routine di sistema il SFL record
623900070503      *   si incrementa stranamente quindi lo reimposto correttamente
624000070503      *    prima di lasciare la routine.
624100070503     C                   EVAL      S2nrr = RrnLast
624200070514     C*
624300070427     C                   ENDSR
624400070427     C/EJECT
624500150930     C*-----------------------------------------------------*
624600150930     C* controllo se mandato un CONTRO Alert al CLIENTE
624700150930     C*-----------------------------------------------------*
624800150930     C     contro_alert  BEGSR
624900150930      *
625000150930      * rec.EMD   invio Alert e contro Alert
625100150930     C                   eval      inviato_contro_alert = 'N'
625200150930     C                   clear                   dar5emd
625300150930     C                   move      'EMD'         TRDar5
625400150930     C     Kfiar5        CHAIN     FiAR501L
625500150930     C                   IF        %Found(FiAR501L)
625600150930     C                   MOVEL     ar5uni        dar5emd
625700150930     c                   if        §AR5ICA ='S'
625800150930     c                   if        §AR5DPC <>*blank and §AR5DPC <>*zeros
625900150930     c                   move      §AR5DPC       data8DPC          8 0
626000150930     c                   if        data8DPC = Dateu8
626100150930     C                   eval      inviato_contro_alert = 'S'
626200150930     C                   end
626300150930     C                   end
626400150930     C                   end
626500150930     C                   endif
626600150930      *
626700150930     C                   ENDSR
626800080609      * ?-------------------------------------------------------------*?
626900080609      *
627000070326** ErrMsg
627100070326La Filiale non è in gestione                                                   01
627200070326Data errata                                                                    02
627300070326Data antecedente alla data odierna                                             03
627400070326Data distinta superiore alla data odierna dei gg.ammessi                       04
627500070326Data distinta anteriore o superiore alla data odierna dei gg.ammessi           05
627600070326Non esistono distinte aperte nella data scelta                                 06
627700070326Scelta obbligatoria                                                            07
627800070326Codice Giro non corretto o non assegnabile ad una distinta                     08
627900070326Numero Distinta non valido o inesistente                                       09
628000070326Distinta chiusa o annullata                                                    10
628100070413Distinta ancora in uso da un altro lavoro.                                     11
628200070328Scegliere 1 solo giro x effettuare l'interrogazione                            12
628300070326La Distinta è inesistente o non è stata selezionata                            13
628400070326La Distinta è chiusa o annullata                                               14
628500070326La Distinta è ancora allocata. Non è quindi possibile selezionarla.            15
628600070326Non è possibile utilizzare il Giro su Distinte                                 16
628700070326 Non è stata scelta la DISTINTA per assegnare le Spedizioni                    17
628800070326   Effettuare una scelta mediante le funzioni di ricerca.                      18
628900070326 Occorre selezionare almeno una riga per poter proseguire                      19
629000070326  l'elaborazione.                                                              20
629100070326 Le bolla sopra elencate NON sono state aggiornate perchè ALLOCATE da altro    21
629200070326 Lavoro.  CONTROLLARE e RIESEGUIRE l'operazione                                22
629300070326 Durante l'aggiornamento archivi ci sono stati dei problemi. Non eseguito il   23
629400070326 Lavoro.  CONTROLLARE e RIESEGUIRE l'operazione                                24
629500070326Aggiornamento eseguito correttamente                                           25
629600070326    Aggiornamento  N O N  eseguito                                             26
629700070326Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
629800070326USCIRE SUBITO dal lavoro!                                                      28
629900070413Non si può trasferire sulla stessa Distinta                                    29
630000070417Distinta non appartenente alla Data selezionata                                30
630100070829Data limite superiore alla data Distinta                                       31
630200070829Ora limite errata                                                              32
630300080609Il valore immesso per il campo non è valido.                                   33
630400100505Forzare con (F2) la spedizione con Particolarità di ATTESA x consegnarla !!    34
630500150930F2 x Forzare -> presente ALERT di VARIAZIONE data CONSEGNA.                    35
630600160317Non può essere selezionata. Assegnarla a una distinta DPD Fittizia !!          36
630700160323Attribuire GIRO VEDI PACCO a DISTINTA di AUTISTA FITTIZIO !!                   37
630800160323Spedizioni VEDI PACCO possono essere assegnate solo ad AUTISTA Fittizio!!      38
630900160429F2 x Forzare -> spedizione del secondo turno di consegna                       39
631000070829                                                                               40
