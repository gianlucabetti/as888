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
012900070427     D sav_pos1        S                   Like(c2pos1)
013000070427     D sav_pos2        S                   Like(c2pos2)
013100070427     D sav_pos3        S                   Like(c2pos3)
013200070427     D sav_pos4        S                   Like(c2pos4)
013300070427     D sav_pos5        S                   Like(c2pos5)
013400070427     D sav_pos6        S                   Like(c2pos6)
013500070514     D C2_pos1         S                   Like(c2pos1)
013600070514     D C2_pos2         S                   Like(c2pos2)
013700070514     D C2_pos3         S                   Like(c2pos3)
013800070514     D C2_pos4         S                   Like(c2pos4)
013900070514     D C2_pos5         S                   Like(c2pos5)
014000070514     D C2_pos6         S                   Like(c2pos6)
014100071029     D savPOSnf        S                   Like(fdgNFpos)
014200070607     D vedi_video      S              1
014300070607     D salta_video1    S              1    inz('N')
014400070607     D salva_d1sce     S                   like(d1sce)
014500070427     D posizionamento  S              1
014600070515     D posizionato     S              1
014700070511     D cerca_stringa   S              1
014800070514     D Wlen            S              3i 0
014900070514     D Wpos            S              3i 0
015000070515$003 D C2RCD_pos       S                   Like(C2rcd)
015100101129      *--------------------------
015200101129     d WDcodInfo       s             20a
015300101129     d dataiso         s               d   datfmt(*iso)
015400101129     d dataeur         s               d   datfmt(*eur)
015500101129     d Wdatabol        s              8s 0
015600070320      *--------------------------
015700070228     d wpes            s                   like(dstpkl)
015800070228     d wvol            s                   like(dstvlu)
015900070228     D totSPE_cgi      S                   Like(s1tNSP)
016000070228     D totPKL_cgi      S                   Like(s1tPKL)
016100070228     D totVLU_cgi      S                   Like(s1tVLU)
016200070228     D totCOL_cgi      S                   Like(s1tCOL)
016300070228     D totSPE_s1       S                   Like(s1tNSP)
016400070228     D totPKL_s1       S                   Like(s1tPKL)
016500070228     D totVLU_s1       S                   Like(s1tVLU)
016600070228     D totCOL_s1       S                   Like(s1tCOL)
016700070302     D totSPE_s2       S                   Like(s1tNSP)
016800070302     D totPKL_s2       S                   Like(s1tPKL)
016900070302     D totVLU_s2       S                   Like(s1tVLU)
017000070302     D totCOL_s2       S                   Like(s1tCOL)
017100070320      *--------------------------
017200070301     d wNFV            s                   like(dstNFV)
017300070301     d wNPG            s                   like(dstNPG)
017400070316     d wFGS            s                   like(dstFGS)
017500070321     d wCGI            s                   like(argCGI)
017600070321     d wDDC            s                   like(h1cddc)
017700070301     d kTIP            s                   like(ARItip)
017800070301     d kTRC            s                   like(AR6trc)
017900070307     d kCOD1           s                   like(TBEcod)
018000070307     d kKEY1           s                   like(TBEke1)
018100070404     d wfgs_ifp        s                   like(ARBifp)
018200070606     d w1sce           s                   like(d1sce)
018300070606     d w1NO_dis        s              6s 0
018400070829     d h1dtaI          s                   like(d1dtaI)
018500071210     d h1dcsr          s                   like(d1dcsr)
018600080606     d PDAconsegne     s              1    inz(' ')
018700080606     d PGM_Chiamante   s             10
018800101130     d exec_opzINFO    s              1    inz(' ')
018900100505     d in_attesa       s              1    inz(' ')
019000100505     D Premuto_F02     s              1
019100160317     D Sped_DPD        s              1
019200160317     D GIRO_vd_Pacco   s             10
019300160318     D GIRO_vedi_PACCO_si...
019400160318     d                 s              1    inz(' ')
019500150930     D inviato_contro_alert...
019600150930     d                 s              1    inz('N')
019700070607      *
019800070611      *?  ABILITATO o Non ABILITATO
019900070611     d Sel_xGiro       s              1a   inz('S')
020000070228      *--------------------------
020100070305      * Per impostare in testata la Distinta
020200070305     D Distintalfa     DS
020300070305     D  num_ndc                1      7
020400070305     D  ndc_a                  1      7    DIM(7)
020500070305      *--------------------------
020600070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
020700070305     D WLBDAT          DS                  INZ
020800070305     D  G02DAT                 1      8  0
020900070227     D  G02INV                 9     16  0
021000070227     D  G02ERR                17     17
021100070227     D  G02TGI                18     22  0
021200070307      *
021300070307     D DATPAR          DS
021400070307     D  GIODAT                 1      8  0
021500070307     D  GIOINV                 9     16  0
021600070307     D  GIOTGI                17     21  0
021700070307      *
021800070307     d Wdata8          DS
021900070307     d  dadata                 1      8  0
022000070307     d  adata                  9     16  0
022100070307     d  GioLav                17     21  0
022200070924      *
022300070307      *--------------------------------
022400070307      * DS per richiamo pgm FNLR36R
022500070307     D DSLR36          DS
022600070307     D  P36AAS                14     17  0
022700070307     D  P36LNP                18     20  0
022800070307     D  P36NRS                21     22  0
022900070307     D  P36NSP                23     29  0
023000070307     D  P36F03                30     30
023100070307     D  P36FLG                31     31
023200070320      *--------------------------
023300150930     D Dar5emd       E DS
023400070227     d AZUTEds       e ds                  extname(AZUTE00F)
023500070227     d DDatiUte      e ds
023600070320     d* Ds per dati organigramma
023700070228     D OG150         E DS                  INZ
023800070228     D OG146         E DS                  INZ
023900070829     D OG147         E DS                  INZ
024000080606     D OG148         E DS                  INZ
024100070320     D DgrgFLO       E DS                  INZ
024200080606     D DdstFLR       E DS                  INZ
024300080627     d fidg45ds      e ds
024400080606     d fidg44ds      e ds
024500080609     d fidg43ds      e ds
024600080606     d fidg30ds      e ds
024700141029     d fnlry09ds2    e ds
024800141113     d fnlr98ds2     e ds
024900150515     d TRULVPODS     e ds
025000150515     d   sk_PDA               16    765    inz   dim(250)
025100140714      *--------------------------
025200140711      * per gestire evento NIC su Togli
025300140711     d fnlrq8ds      e ds
025400140714     d sav_arbndc      s                   like(ARBndc)
025500140714     d sav_arbddc      s                   like(ARBddc)
025600140714     d sav_arbpdc      s                   like(ARBpdc)
025700140714      *--------------------------
025800070320      *------
025900070404     d $FV6            s              6P 0 dim(600)                             F.V.  x FIDG31R
026000070404      *------
026100160317     D DNSD          E DS
026200160317     D DDGD          E DS
026300070301     D DS3A          E DS
026400100505     D dARGFLO       E DS
026500100505     d DTAISO          s               D
026600100505     D DS7r          E DS
026700100505     D s7r             S              2    DIM(100)                             CODICE TABELLA 7R
026800100518     D s7rRC           S              2    DIM(100)                             CODICE TABELLA 7R
026900070301     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
027000070301     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
027100070301     D DS1P          E DS
027200070301     D C1P             S              1    DIM(50)                              CODICI TABELLA 1P
027300070301     D D1P             S             25    DIM(50)                              DESCR. TABELLA 1P
027400070302     D YEURDS        E DS                  EXTNAME(YEURCODS)
027500070320      *--------------------------
027600070320     d* Parametri x Controllo profilo utenti
027700070320     d TIBS34ds      e ds
027800070227      *--------------------------
027900070227     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
028000070227     D DSLV50        E DS                  EXTNAME(FNLV50DS)
028100070227      *--------------------------
028200070227     D* DS PER FNLV24R - ricerca autotrasp.
028300070227     D fnlv24ds      E DS
028400070227      *--------------------------
028500070227     D* DS PER - ricerca giri
028600070227     D fidg09ds      E DS
028700070829      *--------------------------
028800070829     D* DS PER -
028900070829     D fidg17ds      E DS
029000070227      *--------------------------
029100070302     D* DS PER - calcolo pesi volumi colli
029200070302     D fidg27ds      E DS
029300070302      *--------------------------
029400070319     D* DS PER - aggiornamento di ARB
029500070319     D fidg23ds      E DS
029600070320      *--------------------------
029700070319     D* DS PER - aggiornamento distinte
029800070517     D******Fidg28ds      E DS
029900070320      *--------------------------
030000070517     D* DS PER - aggiornamento distinte
030100070517     D Fidg37ds      E DS
030200070517      *--------------------------
030300070320     D* DS PER FIDG04R - ricerca numero distinta
030400070320     D fidg31ds      E DS
030500070320     D*-------------
030600070328     D* DS PER TNSD99R - affidabilità consegna
030700070328     D tnsd99ds      E DS
030800070328     D*-------------
030900070320     d trul82ds      e ds
031000070319      *--------------------------
031100070228     D* DS PER - Bolle valide in consegna
031200070228     D Fidg16ds      E DS
031300071008      *--------------------------
031400071008     D* DS PER - Orm x particolarità RC
031500071008     D Fnlr99ds      E DS
031600070320      *--------------------------
031700070301     D fnarbdsav     e ds                  extname(fnarb00f)
031800070302     d                                     Prefix(sv_)
031900070320      *------
032000070228     D fnarbds       e ds                  extname(fnarb00f)
032100070228     D fiargds       e ds                  extname(fiarg00f)
032200070227      *--------------------------
032300070320      *?  COSTANTI                                      ?
032400070320      *--------------------------
032500940506     D* Tasti di funzione
032600940506     D F01             C                   CONST(X'31')
032700940506     D F02             C                   CONST(X'32')
032800940506     D F03             C                   CONST(X'33')
032900940506     D F04             C                   CONST(X'34')
033000940506     D F05             C                   CONST(X'35')
033100940506     D F06             C                   CONST(X'36')
033200940506     D F07             C                   CONST(X'37')
033300940506     D F08             C                   CONST(X'38')
033400940506     D F09             C                   CONST(X'39')
033500940506     D F10             C                   CONST(X'3A')
033600940506     D F11             C                   CONST(X'3B')
033700070315     D F12             C                   CONST(X'3C')
033800940506     D F13             C                   CONST(X'B1')
033900940506     D F14             C                   CONST(X'B2')
034000940506     D F15             C                   CONST(X'B3')
034100940506     D F16             C                   CONST(X'B4')
034200940506     D F17             C                   CONST(X'B5')
034300940506     D F18             C                   CONST(X'B6')
034400940506     D F19             C                   CONST(X'B7')
034500940506     D F20             C                   CONST(X'B8')
034600940506     D F21             C                   CONST(X'B9')
034700940506     D F22             C                   CONST(X'BA')
034800940506     D F23             C                   CONST(X'BB')
034900940506     D F24             C                   CONST(X'BC')
035000940506     D ENTER           C                   CONST(X'F1')
035100940506     D ROLDWN          C                   CONST(X'F4')
035200940506     D ROLLUP          C                   CONST(X'F5')
035300070301     d digits          c                   '0123456789'
035400070302     d Alla_Dist       c                   'alla dist.:'
035500070326     d OPZ_1           c                   'ASSEGNA    '
035600070326     d OPZ_2           c                   'TOGLIE     '
035700070326     d OPZ_3           c                   'TRASFERISCE'
035800070306     d Alla            c                   'alla '
035900070306     d Dalla           c                   'dalla'
036000070426     d FUN_1           c                   'F1=Sel.tutto'
036100070305     d FUN_3           c                   'F3=Fine'
036200070419     d FUN_6           c                   'F6=Conferma Giro NO DETT. Bolle'
036300070419     d FUN_Enter       c                   'Enter=Con Dettaglio Bolle'
036400070309     d FUN_9_Drop      c                   'F9=Altri Dati'
036500070419
036600070606     D Scegliere       C                   '1'
036700070606      *
036800070531     D Assegnare       C                   '1'
036900070531     D Togliere        C                   '2'
037000070531     D Trasferire      C                   '3'
037100070419      **********************************************************************
037200070419      * Constants
037300070419      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
037400070419      **********************************************************************
037500070427$xxx D Ord_GIRODEST    C                   1
037600070427$xxx D Ord_DESTINAT    C                   2
037700070427$xxx D Ord_INDIRIZZO   C                   3
037800070427$xxx D Ord_LOCALITA    C                   4
037900070427$xxx D Ord_VOLUME      C                   5
038000070427$xxx D Ord_PESO        C                   6
038100070427      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
038200070427     D MaxKey          C                   9
038300070419     D Ascendente      C                   1
038400070419     D Discendente     C                   2
038500070419     D Carattere       C                   6
038600070419     D Put             C                   1
038700070419     D EndPut          C                   2
038800070419     D Get             C                   3
038900070419     D Numerico        C                   8
039000070419      **********************************************************************
039100070419      * Campi utili:
039200070419      *     RRN1       - Numero relativo di record del Subfile
039300070419      *     SizeList   - Dimensione della lista
039400070419      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
039500070419      **********************************************************************
039600070419     D Rrn1            S              5I 0
039700070419     D NotUsed         S             16A
039800070419     D ReturnSize      S              9B 0
039900070419     D SizeList        S              9B 0
040000070419     D RrnLast         S              5I 0
040100070427     D WrkSort         S              1  0 inz(0)
040200070427     D WindSort        S              3I 0 inz(0)
040300070419      **********************************************************************
040400070419      * Data Structures
040500070419      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
040600070419      *     QLGSCB     - The sort request block for the QLGSORT API
040700070419      *     QLGSCB00   - The sort request block for the QLGSRTIO API
040800070419      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
040900070419      *     QUSEC      - Error structure for the QLGSORT API
041000070419      **********************************************************************
041100070419     D SflRcd          DS
041200070419     D  S2CGI
041300070420     D  S2CRSD
041400070420     D  S2CIND
041500070420     D  S2CLOD
041600070420     D  S2CPKF
041700070420     D  S2CVLF
041800070420     D  S2CNCL
041900070511     d  H2SUBST
042000080610     D  s2cCOK
042100070419     D  S2CAAS
042200070419     D  S2CLNP
042300070419     D  S2CNRS
042400070419     D  S2CNSP
042500070419     D  S2SCE
042600070427     D  S2CPRD
042700070427     D  S2CCAD
042800070419     D  S2CLNA
042900070419     D  S2CGMC
043000070419     D  S2CGC1
043100070419     D  S2CDAC
043200070419     D  S2CCBO
043300070419     D  S2CTIC
043400070419     D  S2CVCA
043500070419     D  S2CCAS
043600070419     D  S2CHCR
043700070419     D  S2CGC2
043800070419     D  S2DTC2
043900070419     D  S2CVAS
044000070419     D  S2CIMA
044100070419     D  S2DANO
044200070419     D  S2DTC1
044300070419     D  S2CRSM
044400070419     D  S2CTSP
044500070419     D  S2CZNC
044600070427     D  S2PKF
044700100505     D  S2Patt
044800100505     D  S2Forza
044900100505     D  h2dcr
045000100505     D  h2gma
045100101126     D  h2notASS
045200101129     D  h2MGS
045300101130     D  h2in45
045400110804     D  h2xco
045500130424     D  h2att
045600150930     D  h2Calert
045700070419     D  Selected                      1A
045800070419
045900070419      /COPY QSYSINC/QRPGLESRC,QLGSORT
046000070419     D QLGKL                         16    DIM(MaxKey)
046100070419     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
046200070419     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
046300070419     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
046400070419     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
046500070419
046600070419      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
046700070419      /COPY QSYSINC/QRPGLESRC,QUSEC
046800940127     C*----------------------------------------------------*
046900070419      *?     MAIN LINE PROGRAM                             ?
047000940127     C*----------------------------------------------------*
047100940223     C* inizializzazione variabili
047200940223     C                   EXSR      INZVAR
047300940223     C*
047400940223     C     $FINE         DOWEQ     *OFF
047500070413     C*
047600070413     C* 1° Video
047700070413     C     $GEST         CASEQ     'D1'          GESD1                          Video Parametri
047800070413     C*
047900070413     C* Lista GIRI se scelta Assegnazione e non impostato il GIRO fra i parametri d'ingresso
048000070413     C     $GEST         CASEQ     'S1'          GESS1                          Lista dei GIRI
048100070413     C*
048200070413     C* Lista di dettaglio delle Bolle x selezione e guida aggiornamento Data Base
048300070413     C     $GEST         CASEQ     'S2'          GESS2                          Lista delle Bolle
048400070413     C*
048500070413     C* Finestra dei Totali riepilogativa prima dell'aggiornamento del Data Base
048600070413     C     $GEST         CASEQ     'W1'          GESW1                          Riepilogo Totali
048700070413     C*
048800070413     C* Durante l'aggiornamento del Data Base scrivo eventuali errori Bolle da visualizzare
048900070413     C*  su questo ultimo SFL.
049000070413     C     $GEST         CASEQ     'S3'          GESS3                          Aggiornamento D.B. e
049100070413      *                                                                         errori su bolle
049200940117     C                   END
049300940117     C                   END
049400940325     C* fine programma
049500940325     C                   SETON                                            LR
049600030113     C************************************************************
049700070320      *?  INIZIALIZZAZIONE VARIABILI                       ?
049800030113     C************************************************************
049900030113     C     INZVAR        BEGSR
050000030113     C*
050100030113     C* Variabili per gestione videate
050200030113     C                   MOVE      *OFF          $FINE
050300070227     C                   MOVE      *OFF          $INZW1
050400070227     C                   MOVE      *OFF          $INZS1
050500070227     C                   MOVE      *OFF          $INZS2
050600070306     C                   MOVE      *OFF          $VISS2
050700070309     C                   MOVE      *OFF          $INZS3
050800030113     C                   MOVE      *OFF          $EFILE
050900030113     C                   MOVE      *OFF          $ESCI
051000030113     C                   MOVE      *OFF          $RCDOK
051100070227     C* Emissione primo video
051200070227     C                   MOVE      'D1'          $GEST
051300070227     C                   MOVE      *ON           $INZD1
051400030113     C* Variabili appoggio
051500030114     C                   Z-ADD     1             WPAG
051600030113     c*
051700030113     C                   ENDSR
051800070227     C************************************************************
051900080606      *?    se presente in Distinta Fase CUS occorre FORZARE     ?
052000080606     C************************************************************
052100080626     C     CTL_se_FORZAREbegSR
052200080606     C*
052300080630     C* Solo se la filiale è partita con il PDA in Consegna
052400080630     c                   if        §OGPDACON <> *blank
052500080630     C*
052600080606     C*   Controlla presenza Fase "CUS"
052700080630     c                   If        h1ndc_meno >0
052800080626     c                   clear                   torna_a_D1        1
052900080606     c                   clear                   fidg44ds
053000080626     c                   z-add     D1CFGS        CO44FGS
053100120209     c*********          z-add     h1cddc        CO44DDC
053200120209     c                   z-add     h1ddc_meno    CO44DDC
053300080626     c                   z-add     h1ndc_meno    CO44NDC
053400080606     c                   eval      CO44wnd = 'S'
053500080606     c                   eval      kpjbus = kpjbu
053600080606     c                   eval      kpjbu =  fidg44ds
053700080606     c                   call      'FIDG44R'
053800080606     c                   parm                    kpjba
053900080606     c                   eval      fidg44ds = kpjbu
054000080606     c                   eval      kpjbu = kpjbus
054100080606      *
054200080606      * esce dal programma se non si è FORZATO
054300080606     c                   if        CO44FCUS = 'N'
054400080626      * se richiamato da MONITOR deve uscire direttamente dal pgm
054500080626     c                   if        PGM_Chiamante = 'MONITOR'
054600080626     c                   eval      torna_a_D1 = 'S'
054700080606     C                   MOVE      *ON           $FINE
054800080626     c                   rolbk
054900080626     c                   else
055000080626      * deve tornare al video precedente come F12
055100080626     c                   eval      torna_a_D1 = 'S'
055200080701     c                   leaveSR
055300080626     c                   end
055400080606     c                   endif
055500080606     c*
055600080630     c                   end
055700080630     c*
055800080630     c* se trasferisce
055900080630     C*   Controlla presenza Fase "CUS"
056000080630     c                   If        h1ndc_piu >0
056100080630     c                   clear                   torna_a_D1        1
056200080630     c                   clear                   fidg44ds
056300080630     c                   z-add     D1CFGS        CO44FGS
056400120209     c*******            z-add     h1cddc        CO44DDC
056500120209     c                   z-add     h1ddc_piu     CO44DDC
056600080630     c                   z-add     h1ndc_piu     CO44NDC
056700080630     c                   eval      CO44wnd = 'S'
056800080630     c                   eval      kpjbus = kpjbu
056900080630     c                   eval      kpjbu =  fidg44ds
057000080630     c                   call      'FIDG44R'
057100080630     c                   parm                    kpjba
057200080630     c                   eval      fidg44ds = kpjbu
057300080630     c                   eval      kpjbu = kpjbus
057400080630      *
057500080630      * esce dal programma se non si è FORZATO
057600080630     c                   if        CO44FCUS = 'N'
057700080630      * se richiamato da MONITOR deve uscire direttamente dal pgm
057800080630     c                   if        PGM_Chiamante = 'MONITOR'
057900080630     c                   eval      torna_a_D1 = 'S'
058000080630     C                   MOVE      *ON           $FINE
058100080630     c                   rolbk
058200080630     c                   else
058300080630      * deve tornare al video precedente come F12
058400080630     c                   eval      torna_a_D1 = 'S'
058500080701     c                   leaveSR
058600080630     c                   end
058700080630     c                   endif
058800080630     c*
058900080630     c                   end
059000080630     c*
059100080630     c                   end
059200080630     c*
059300080606     C                   endSR
059400080606     C************************************************************
059500070320      *?  Emissione primo video di SCELTA                        ?
059600070227     C************************************************************
059700070227     C     GESD1         BEGSR
059800070227     C*
059900070227     C* inizializzazione videata
060000070227     C     $INZD1        IFEQ      *ON
060100070227     C                   EXSR      INZD1
060200070227     C                   MOVE      *OFF          $INZD1
060300070227     C                   ENDIF
060400070227     C*
060500070228     c     Video_1       tag
060600070302     C*  Emissione Video
060700070607     c                   if        salta_video1 = 'N'
060800100505      *
060900100505      *    ind.10 x campi a VIDEO da condizionare
061000070228      * ?            *-----------------------*
061100070227     C                   EXFMT     FDG0D01
061200070228      * ?            *-----------------------*
061300071029     c                   clear                   savPOSnf
061400070607     C                   Else
061500070607     c                   eval      salta_video1 = 'N'
061600070607     c                   end
061700070326     c                   setoff                                       99
061800070305     C                   movel     'D1'          sav$GEST
061900070227     C* Selezioni
0620000702271    C                   SELECT
062100070413     C*  Cambio filiale
062200070413     C                   WHEN      $TASTO   =   F18
062300070418     C                   SETON                                        07
062400140924     C*  Disposizioni consegna
062500140924     C                   WHEN      $TASTO   =   F19
062600141029     c                   clear                   fnlry09ds2
062700140924     c                   eval      ilry09tch = 'E'
062800141013     c                   exsr      verdispo
062900141029     c                   if        OLRY09ESal = '1'
063000141002     c                   seton                                        97
063100141002     c                   else
063200141002     c                   setoff                                       97
063300141002     c                   endif
063400070227     C* F3=Fine
063500070227     C                   WHEN      $TASTO   =   F03
063600070227     C                   EXSR      F03_exit
063700070319     c                   rolbk
063800070227      *
063900070227     C* F8=Interrogazione Anagr.AUTISTI
064000070227     C                   WHEN      $TASTO   =   F08
064100070227     C                   EXSR      F08_Interr
064200070227      *
0643000702271O   C                   OTHER
064400070227     C* CONTROLLO DATI
064500070227     C                   EXSR      CTRD1
064600080626      *
064700080626     C* CONTROLLA SE LA DISTINTA con PDA ha già la FASE "CUS"
064800080626     c                   EXSR      CTL_se_FORZARE
064900080626     c                   if        torna_a_D1 = 'S'
065000080626     C                   MOVE      *ON           $INZD1
065100080626     c                   goto      Fine_gesd1
065200080626     c                   end
065300080626      *
0654000702271-   C                   ENDSL
065500070227     C*
065600080606      * ? Se richiamato da MONITOR con PDA
065700080606      * ?  e se la ricerca distinta ha chiuso con F3 o F12
065800080606     c                   if        PGM_Chiamante = 'MONITOR' and
065900080606     c                             (fdgTFU ='03' or fdgTFU ='12')
066000080606     C                   EXSR      F03_exit
066100080606     c                   goto      Fine_gesd1
066200080606     c                   end
066300080606     C*
066400070418     C* Per errori o decodifiche o tasto di cambio P.O.
066500070418     C                   if        $TASTO   =   F18 or *in99
066600070418     c                   goto      Video_1
066700070418     c                   end
066800070228     C*
066900070228     C* Se non ci sono errori passa alle operazioni da esguire
067000080826     C*  il 14=Acceso solo se si sta Assegnando
067100080826     c                   setoff                                       14
067200070228     c                   select
067300070228      *
067400070531     c                   when      d1SCE = Assegnare
067500070228      * ? * Emissione Lista con i Giri e i totali spedizioni *
067600070228     c                   exsr      Assegna
067700070228      *
067800070607     c                   when      d1SCE = Togliere   and h1ndc_meno >0
067900070228      * ? * Emissione Lista delle Spedizioni da selezionare  *
068000070228     c                   exsr      Toglie
068100070228      *
068200070607     c                   when      d1SCE = Trasferire and h1ndc_meno >0
068300070228      * ? * Emissione Lista delle Spedizioni da selezionare  *
068400070228     c                   exsr      Trasferisce
068500070228      *
068600070228     C                   EndSL
068700070228     C*
068800080606     C     Fine_GESD1    ENDSR
068900070306     C/EJECT
069000070306     C************************************************************
069100070306     C*? Assegna Distinta
069200070306     C************************************************************
069300070306     C     Assegna       BEGSR
069400070306     C*
069500080826     C* il 14 Acceso identifica Assegnazione per gestire campi su DSPF
069600070306     C*  Emissione SFL con elenco dei Giri da assegnare
069700080826     c                   seton                                        14
069800070306     c                   eval      $GEST ='S1'
069900070306     C                   MOVE      *ON           $INZS1
070000070306     C                   MOVE      *ON           $INZS2
070100070306     C                   MOVE      *ON           $INZW1
070200070309     C                   MOVE      *ON           $INZS3
070300070319     C*
070400070306     C                   ENDSR
070500070306     C************************************************************
070600070306     C*? Toglie a Distinta
070700070306     C************************************************************
070800070306     C     Toglie        BEGSR
070900070306     C*
071000070306     C*  Emissione SFL con elenco delle bolle da togliere alla distinta
071100070306     c                   eval      $GEST ='S2'
071200070306     C                   MOVE      *ON           $INZS2
071300070306     C                   MOVE      *ON           $INZW1
071400070309     C                   MOVE      *ON           $INZS3
071500070319     C*
071600070306     C                   ENDSR
071700070306     C************************************************************
071800070306     C*? Trasferisce da Distinta a Distinta
071900070306     C************************************************************
072000070306     C     Trasferisce   BEGSR
072100070306     C*
072200070306     C*  Emissione SFL con elenco delle bolle da trasferire ad altra distinta
072300070306     c                   eval      $GEST ='S2'
072400070306     C                   MOVE      *ON           $INZS2
072500070306     C                   MOVE      *ON           $INZW1
072600070309     C                   MOVE      *ON           $INZS3
072700070319     C*
072800070306     C                   ENDSR
072900070227     C/EJECT
073000070227     C************************************************************
073100070320      *?  INIZIALIZZAZIONE Primo video                           ?
073200070227     C************************************************************
073300070227     C     INZD1         BEGSR
073400070227     C*
073500070227     C* Pulizia campi e indicatori
073600070308     c                   exsr      INZ_indERR
073700070227     C* Pulizia formato record
073800070227     c                   clear                   FDG0D01
073900070227     C* campi titolo
074000070302     C                   MOVEL     dsPGM         D1CPGM
074100070227     C                   MOVEL     knsif         D1nsif
074200070227     C                   MOVEL     knmus         D1nmus
074300070227     C                   MOVEL     RSUT          D1crsu
074400070227     C* campi video
074500070227     C* Il p.o.deve essere impostato in base al profilo
074600070413     C     dutlpo        IFEQ      '2'
074700070227     C                   MOVEL     dutpou        D1CFGS                         *FIL.GESTIONE
074800070413     C                   ELSE
074900070413     c                   seton                                        08
075000070413     C                   MOVEL     SIMFEL        D1CFGS
075100070413     C                   END
075200080606      *
075300070227     C                   z-add     UDATE8        D1cDDC
075400080606      *
075500150515     C                   clear                   V1Cp88            1
075600150515     c* ricerca P.O. abilitati da tabella VPO per step di test
075700150515     c                   clear                   trulvpods
075800150515     c                   eval      ivpoke1 = 'SQA'
075900150515     c                   call      'TRULVPOR'
076000150515     c                   parm                    trulvpods
076100150515     c                   move      '999'         fgsa              3
076200150515     c     fgsa          lookup    sk_pda                                 37
076300150515     c  n37              move      dutpou        fgsa
076400150515     c  n37fgsa          lookup    sk_pda                                 37
076500150515     c                   if        *in37
076600150515     c                   eval      v1cp88 = 'S'
076700150515     c                   else
076800150515     C                   CLEAR                   V1Cp88
076900150515     c                   end
077000140929     C*
077100140929      * verifica presenza disposizioni web
077200141029     c                   clear                   fnlry09ds2
077300140929     c                   eval      ilry09tch = 'C'
077400141013     c                   exsr      verdispo
077500141029     c                   if        OLRY09ESal = '1' and
077600140929     c                             OLRY09ERR  = *blank
077700140929     c                   seton                                        97
077800140929     c                   endif
077900080606      * Se richiamato dal MONITOR
078000080606     c                   if        PGM_chiamante = 'MONITOR'
078100080606      *
078200080606      *   imposta l'FGS se passata
078300080606     c                   if          G30FGS >0
078400080606     C                   z-add     G30fgs        D1CFGS
078500080606     c                   end
078600080606      *
078700080606      *   imposta la DATA se passata
078800080606     c                   if          G30DFV >0
078900080606     C                   Z-ADD     G30dfv        G02inv
079000080606     C                   MOVEL     '3'           G02ERR
079100080606     C                   CALL      'XSRDA8'
079200080606     C                   PARM                    WLBDAT
079300080606     C                   Z-ADD     G02DAT        D1cDDC
079400080606     c                   end
079500080606      *
079600080606     c                   end
079700070625      *
079800080606     c                   setoff                                       1115
079900071210      *
080000070625     C*  Controlla Con il P.O. la data abilitazione
080100070625     C     d1cFGS        CHAIN     AZORG01l
080200071210      *
080300071210b    C                   IF        %found(azorg01l)
080400070625     C                   MOVEL     ORGdf0        og150
080500070829     C                   MOVEL     ORGde7        og147
080600080606     c                   movel     orgde8        og148
080700080606      *
080800080606     c* in test o attivo x PDA   ("S", "T")
080900080606     c                   clear                   PDAconsegne
081000080606     c                   if        §OGPDACON <> *blank
081100080606     c                   eval        PDAconsegne = §OGPDACON
081200080606     c                   seton                                        15
081300080606     c                   end
081400071210      *
081500070625     c* p.o. con piking messaggerie
081600070625     c                   if        §ogpkm='S'
081700070625     c                   seton                                        11
081800070625     c                   endif
081900071210      *
082000080606      *
082100080606      * Se richiamato dal MONITOR
082200080606      *   salta tutte le altre impostazioni della precedente chiamata
082300080606     c                   if        PGM_chiamante = 'MONITOR'
082400080606     c                   move      Togliere      d1SCE
082500080606     c                   GoTo      END_INZ1
082600080606     c                   endif
082700080606      *
082800080606      *
082900071210     c                   open      fiPRM01l
083000071210     c                   z-add     d1cfgs        k_PRMfgs
083100071210     c                   movel(p)  'FIDG17R'     k_PRMpgm
083200071210     c     k_PRM         chain     fiPRM01l
083300071210      *
083400071210     c                   if        %Found(fiPRM01l)
083500071210     c                   movel     prmPAR        fidg17ds
083600071210     c                   endIF
083700071210      *
083800070829     c* Data limite Abbinamento
083900071210a    c                   if        §ogDOada='S'
084000070829     c                   seton                                        12
084100071210      *
0842000712100    c                   if        sav_d1dtaI = 0
084300071210      *
084400071210 1   c                   if        %Found(fiPRM01l)
084500070830     c                   movel     D17FPPI       d1tUSC
084600071210      *
084700071210 2   c                   if        d17DTAI >0
084800070829     C                   Z-ADD     d17DTAi       G02inv
084900070829     C                   MOVEL     '3'           G02ERR
085000070829     C                   CALL      'XSRDA8'
085100070829     C                   PARM                    WLBDAT
085200070829     C                   Z-ADD     G02DAT        d1DTAi
085300070831     C                   Z-ADD     G02DAT        sav_d1DTAi
0854000712102-   c                   end
085500071210      *
085600070829     c                   if        d17ORAi >0
085700070829     C                   Z-ADD     d17ORAi       d1ORAi
085800070829     c                   end
085900071210      *
086000070924      * deve impostare la data dell'ultimo lancio se c'è... altrimenti UDATE
086100070927     C                   Z-ADD     d17dFVI       G02inv
086200070927     C                   MOVEL     '3'           G02ERR
086300070927     C                   CALL      'XSRDA8'
086400070927     C                   PARM                    WLBDAT
086500070927     C                   Z-ADD     G02DAT        d1cDDC
0866000712101-   c                   endIF
086700071210      *
086800071210      *
0869000712100x   c                   else
087000070831     c                   eval      D1dtaI = sav_d1dtaI
087100070831     c                   eval      D1oraI = sav_d1oraI
0872000712100-   c                   endIF
087300071210      *
087400071210a-   c                   endif
087500071210      *
0876000712100    c                   if        sav_d1dcsr = 0
087700071210      *
087800071210     c                   if        %Found(fiPRM01l) and d17LDCRI >0
087900071210     C                   Z-ADD     d17LDCRI      G02inv
088000071210     C                   MOVEL     '3'           G02ERR
088100071210     C                   CALL      'XSRDA8'
088200071210     C                   PARM                    WLBDAT
088300071210     C                   Z-ADD     G02DAT        d1Dcsr
088400071210     C                   Z-ADD     G02DAT        sav_d1Dcsr
088500071210     c                   end
088600071210      *
0887000712100x   c                   else
088800071210     c                   eval      D1dcsr = sav_d1dcsr
0889000712100-   c                   endIF
089000071210      *
089100071210     c                   close     fiPRM01l
089200071210      *
089300071210b-   c                   endif
089400070924      * oppure.....
089500070418     C*  Se precedentemente impostati devono rimanere quelli
089600070418     c                   if        sav_d1cddc > 0
089700070418     C                   z-add     sav_d1cddc    D1cDDC
089800070418     c                   end
089900070418     C*
090000070418     c                   if        sav_d1cfgs > 0
090100070418     C                   z-add     sav_d1cfgs    D1cfgs
090200070418     c                   end
090300080606     C*
090400100505     c* Visualizzare Bolle con Particolarità di Attesa
090500100505     c*******            if        d1pATT = *blank
090600100505     C                   MOVEL     'N'           D1pATT
090700100505     c*******            end
090800070418     C*
090900080606     C     END_INZ1      ENDSR
091000940127     C************************************************************
091100070320      *? Controlli primo video di SCELTA Operazione da eseguire  ?
091200940127     C************************************************************
091300070227     C     CTRD1         BEGSR
091400030113     C*
091500070606     C* Ripercorre i controlli
091600070606     c     ini_CTRD1     tag
091700070606     C*
091800070227     C*  Spegne indicatori di errore
091900070308     c                   exsr      INZ_indERR
092000140929     c                   setoff                                       9997
092100070228     C*  campi di Work
092200070228     c                   clear                   con_giro
092300070308     c                   clear                   h1cddc
092400070319     c                   clear                   h1ndc_piu
092500160318     c                   clear                   h1pdd_PIU
092600120209     c                   clear                   h1ddc_piu
092700070319     c                   clear                   h1ndc_meno
092800160318     c                   clear                   h1pdd_meno
092900120209     c                   clear                   h1ddc_meno
093000080630     c                   clear                   h1PDA_piu
093100080630     c                   clear                   h1PDA_meno
093200070319     c                   clear                   h1fpp_meno
093300070319     c                   clear                   h1fpp_piu
093400070321     c                   clear                   wfgs
093500070321     c                   clear                   wddc
093600070321     c                   clear                   wcgi
093700070829     c                   clear                   h1dtaI
093800071210     c                   clear                   h1dcsr
093900070301      * Categoria Foglio x controllo nr.distinta
094000070301     C                   z-add     4             wNPG
094100070227     C* --------------
094200070227      * ?- Controlla la filiale in gestione
094300070227     C                   CLEAR                   DSLV50
094400070227     C                   MOVEL     KNMUS         D50PRU
094500070227     C                   z-add     d1cFGS        D50FGS
094600070227     C                   CALL      'FNLV50R'
094700070227     C                   PARM                    DSLV50
094800070227    1C     D50ERR        IFNE      ' '
094900070326     c                   movel     ErrMsg(1)     d1msg
095000070326     C                   SETON                                        51  99
095100070227     C                   goto      ENDCTR1
095200070227    1C                   ENDIF
095300140929      * verifica presenza disposizioni web
095400141029     c                   clear                   fnlry09ds2
095500141029     c                   eval      ilry09tch = 'C'
095600141013     c                   exsr      verdispo
095700141029     c                   if        OLRY09ESal = '1' and
095800140929     c                             OLRY09ERR  = *blank
095900140929     c                   seton                                        97
096000140929     c                   endif
096100070227      *
096200080606     c                   setoff                                       11  15
096300070227     C*  Controlla Con il P.O. la data abilitazione
096400070227     C     d1cFGS        CHAIN     AZORG01l
096500070227    1C                   IF        %found(azorg01l)
096600070227     C                   MOVEL     ORGdf0        og150
096700080606     c                   movel     orgde8        og148
096800080606      *
096900080606     c* in test o attivo x PDA   ("S", "T")
097000080606     c                   clear                   PDAconsegne
097100080606     c                   if        §OGPDACON <> *blank
097200080606     c                   eval        PDAconsegne = §OGPDACON
097300080606     c                   seton                                        15
097400080606     c                   end
097500080606      *
097600070625     c* p.o. con piking messaggerie
097700070625     c                   if        §ogpkm='S'
097800070625     c                   seton                                        11
097900070625     c                   endif
098000070625      *
098100070227     C                   MOVEL     ORGde6        og146
098200070227     c                   if             §ogdda = *blank or §ogdda = *all'0'
098300070227     C                   MOVE      *ON           $FINE
098400070227     C                   goto      ENDCTR1
098500070227     c                   else
098600070227      * se c'è una data in organigramma
098700070227     c                   move      §ogdda        data_org          8 0
098800070227      * se siamo in fase di Parallelo prima di essere partiti
098900070426     c                   if        data_org = 20391231 or dateu8 < data_org
099000070227     C                   eval      test_Parallelo = 'S'
099100070419     c                   else
099200070419     C                   MOVE      *BLANK        test_Parallelo    1
099300070227     c                   end
099400070227      * se data distinte automatiche NON è
099500070227      * né = 31/12/2039 che significa p.o. in fase di test o parallelo
099600070227      * né < oggi che significa p.o. partito
099700070227      * allora fuori
099800070426     c********           if        data_org <> 20391231 and dateu8 < data_org
099900070426     C********           MOVE      *ON           $FINE
100000070426     C********           goto      ENDCTR1
100100070426     c********           end
100200070227     c                   end
100300070227      *-
100400070227   X1C                   ELSE
100500070227      * Non presente in organigramma
100600070326     C                   SETON                                        51  99
100700070326     c                   movel     ErrMsg(1)     d1msg
100800070227     C                   goto      ENDCTR1
100900070227    1C                   ENDIF
101000070227     C*
101100070316      * Salva la filiale su campo di work x tutto il pgm
101200070316     c                   z-add     d1cfgs        wfgs
101300070418     C                   Z-ADD     D1cFGS        sav_d1cFGS
101400070227     C* --------------
101500070227      * ?- Data Distinte del:
101600070227     C                   Z-ADD     d1cDDC        G02DAT
101700070227     C                   MOVEL     *BLANK        G02ERR
101800070227     C                   CALL      'XSRDA8'
101900070227     C                   PARM                    WLBDAT
102000070227    1C     G02ERR        IFEQ      '1'
102100070326     c                   movel     ErrMsg(2)     d1msg
102200070326     C                   SETON                                        52  99
102300070227     C                   goto      ENDCTR1
102400070227    1C                   ENDIF
102500070307      *
102600070307      *
102700070307      * Controlli identici al FNLR50R sulla data distinta
102800070307      * prima della data del giorno
102900070307     c                   if        g02inv < dateu8
103000070326     c                   movel     ErrMsg(3)     d1msg
103100070326     c                   SETON                                        52  99
103200070307     C                   goto      ENDCTR1
103300070307     c                   endif
103400070307      *controllo range fra le date di oggi e quella immessa
103500070413      *  non può essere superiore di 3 giorni lavorativi
103600070307     c                   eval      dadata = dateu8
103700070307     c                   eval      adata  = g02inv
103800070307     c                   CALL      'XSRLAV8'
103900070307     c                   PARM                    Wdata8
104000070413     c                   if        giolav > §dgddc or
104100070413     c                             giolav > 3
104200070326     c                   movel     ErrMsg(4)     d1msg
104300070326     c                   SETON                                        52  99
104400070307     C                   goto      ENDCTR1
104500070307     c                   end
104600070307      *
104700070307     C* VERIFICO CHE LA DATA DELLA DISTINTA NON SIA ANTECEDENTE O
104800070307     C* SEGUENTE ALLA UDATE DI OLTRE 15 GIORNI
104900070307     C* PER TUTTO IL MESE DI AGOSTO NON CONTROLLO 1 15 GG
105000070307     C                   MOVE      dateu8        WMGUDT            4 0
105100070307     C                   MOVEL     WMGUDT        WMUDT             2 0
105200070307     C                   Z-ADD     §DGDGG        NRGIO             3 0
105300070307      *
105400070307     C     WMGUDT        IFGE      §DGDEI
105500070307     C     WMGUDT        ANDLE     §DGDEF
105600070307     C                   Z-ADD     §DGDGE        NRGIO
105700070307     C                   END
105800070307      *
105900070307     C     dateu8        IFGE      §DGDII
106000070307     C     dateu8        ANDLE     §DGDIF
106100070307     C                   Z-ADD     §DGDGI        NRGIO
106200070307     C                   END
106300070307      *
106400070307     C     G02TGI        SUB       NRGIO         GIOTGI
106500070307     C                   CALL      'XSRGI8'
106600070307     C                   PARM                    DATPAR
106700070307     C                   Z-ADD     GIOINV        DATM15            8 0
106800070307      *
106900070307     C     DATM15        IFGT      dateu8
107000070326     c                   movel     ErrMsg(5)     d1msg
107100070326     C                   SETON                                        52  99
107200070307     C                   goto      ENDCTR1
107300070307     C                   END
107400070307      *
107500070307     C     G02TGI        ADD       NRGIO         GIOTGI
107600070307     C                   CALL      'XSRGI8'
107700070307     C                   PARM                    DATPAR
107800070307     C                   Z-ADD     GIOINV        DATP15            8 0
107900070307      *
108000070307     C     DATP15        IFLT      dateu8
108100070326     c                   movel     ErrMsg(5)     d1msg
108200070326     C                   SETON                                        52  99
108300070307     C                   goto      ENDCTR1
108400070307     C                   END
108500070307      *
108600070307     c     salta_ctrl    tag
108700070307      *
108800070227     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
108900070301     C                   Z-ADD     G02INV        h1cDDC
109000070321     c                   z-add     G02INV        wddc
109100070301     C                   Z-ADD     G02DAT        D1cDDC
109200070418     C                   Z-ADD     D1cDDC        sav_d1cDDC
109300070301     C*---------------
109400070307      *
109500070307     C*   controlla se nella data scelta ci sono distinte aperte
109600070307     c                   move      'S'           tutte_chiuse      1
109700070406     c     kdst8         setll     fidst08l
109800070406     c     kdst8         reade     fidst08l
109900070406     c                   dow       not %Eof(fidst08l)
110000070412     c                   if        dstFCF <> 'S' and dstATB = ' '
110100070307     c                   clear                   tutte_chiuse
110200070307     c                   leave
110300070307     c                   end
110400070406     c     kdst8         reade     fidst08l
110500070307     c                   enddo
110600070307      * nella data tutte non ci sono distinte aperte
110700070307     c                   if        tutte_chiuse = 'S'
110800070326     c                   movel     ErrMsg(6)     d1msg
110900070326     C                   SETON                                        52  99
111000070307     C                   goto      ENDCTR1
111100070307     c                   end
111200070307      *
111300070307     c     salta_ctrl1   tag
111400070829     C* --------------
111500070829      * ?- Data Limite  del:
111600070829     c                   if        d1DTAi > 0
111700070829     C                   Z-ADD     d1DTAi        G02DAT
111800070829     C                   MOVEL     *BLANK        G02ERR
111900070829     C                   CALL      'XSRDA8'
112000070829     C                   PARM                    WLBDAT
112100070903     C                   Z-ADD     G02DAT        d1DTAi
112200070829    1C                   IF        G02err = '1'
112300070829     c                   movel     ErrMsg(2)     d1msg
112400070829     C                   SETON                                        59  99
112500070829     C                   goto      ENDCTR1
112600070829    1C                   Else
112700070829     c                   z-add     g02INV        h1dtaI
112800070829    1C                   ENDIF
112900070829     c                   if        g02INV > h1cDDC
113000070829     c                   movel     ErrMsg(31)    d1msg
113100070829     C                   SETON                                        59  99
113200070829     C                   goto      ENDCTR1
113300070829    1C                   END
113400070829     c                   else
113500070829    1C                   IF        d1ORAi <> *zeros
113600070829     c                   movel     ErrMsg(2)     d1msg
113700070829     C                   SETON                                        59  99
113800070829     C                   goto      ENDCTR1
113900070829    1C                   ENDIF
114000070829     c                   end
114100070829     C* --------------
114200070829      * ?- Ora  Limite     :
114300070829     c                   movel     d1ORAi        oraI              2 0
114400070829     c                   move      d1ORAi        minutiI           2 0
114500070829     c                   if        oraI > 23 or minutiI > 59
114600070829     c                   movel     ErrMsg(32)    d1msg
114700070829     C                   SETON                                        60  99
114800070829     C                   goto      ENDCTR1
114900070829    1C                   END
115000070829      *
115100070831     c                   eval      sav_d1dtaI = D1dtaI
115200070831     c                   eval      sav_d1oraI = D1oraI
115300070307     C*---------------
115400071210      * ?- Data Limite consegna richiesta Prima DEL:
115500071210     c                   if        d1Dcsr > 0
115600071210     C                   Z-ADD     d1Dcsr        G02DAT
115700071210     C                   MOVEL     *BLANK        G02ERR
115800071210     C                   CALL      'XSRDA8'
115900071210     C                   PARM                    WLBDAT
116000071210     C                   Z-ADD     G02DAT        d1Dcsr
116100071210    1C                   IF        G02err = '1'
116200071210     c                   movel     ErrMsg(2)     d1msg
116300071210     C                   SETON                                        61  99
116400071210     C                   goto      ENDCTR1
116500071210    1C                   Else
116600071210     c                   z-add     g02INV        h1dcsr
116700071210    1C                   ENDIF
116800071210     c                   end
116900071210      *
117000071210     c                   eval      sav_d1dcsr = D1dcsr
117100071210     C*---------------
117200070227      * ?- Scelta obbligatoria
117300070227     c                   if        d1sce = ' '
117400070326     c                   movel     ErrMsg(7)     d1msg
117500070227     C                   SETON                                        53  99
117600070227     C                   goto      ENDCTR1
117700070227     c                   end
117800070307     C*------------------------
117900070307      * pulisce gli altri campi non nella selezione
118000070531     c                   if        d1sce = Assegnare
118100070307     c                   clear                   d1ndc2
118200070307     c                   clear                   d1ndc3
118300070307     c                   clear                   d1ndc4
118400070307     c                   end
118500070307      *
118600070531     c                   if        d1sce = Togliere
118700070307     c                   clear                   d1ccgi
118800070307     c                   clear                   d1ndc1
118900070307     c                   clear                   d1ndc3
119000070307     c                   clear                   d1ndc4
119100070307     c                   end
119200070307      *
119300070531     c                   if        d1sce = Trasferire
119400070307     c                   clear                   d1ccgi
119500070307     c                   clear                   d1ndc1
119600070307     c                   clear                   d1ndc2
119700070307     c                   end
119800070301     C*------------------------
119900070227      * ?- Scelta (1) Assegna Giro alla distinta
120000070301     c                   clear                   d1dcgi
120100160318     c                   eval      GIRO_vedi_PACCO_si = *blank
120200160318      *
120300070531     c                   if        d1sce = Assegnare
120400070301     c     '?'           scan      d1cCGI
120500070227     C*   ricerca codice giro
120600070227     c                   if        %Found
120700070326     C******             SETON                                            99
120800070227     c                   eval      kpjbus = kpjbu
120900070227     c                   clear                   fidg09ds
121000070227     c                   move      'P01'         d09iop0
121100070905     c                   move      'C'           d09iTug
121200070227     c                   movel     d1cFGS        d09ifgs
121300070227     c                   z-add     DATEU8        d09idat
121400070228     c                   movel(p)  fidg09ds      kpjbu
121500070227     c                   call      'FIDG09R'
121600070227     c                   parm                    kpjba
121700070227     c                   movel     kpjbu         fidg09ds
121800070320     c                   movel     d09oflo       dGRGflo
121900070320      * se il giro può essere messo in distinta
122000070326      * ?lo deve comunque caricare e controllare in seguito per non
122100070326      * ?renderlo disponibile alla selezione.
122200070326     c*******            if        §grgDST <> 'N'
122300070227     c                   movel     d09ocgi       d1cCGI
122400070326     c*******            end
122500070227     c                   eval      kpjbu = kpjbus
122600070227     c                   end
122700070228      *  controllo validità giro
122800070301     c                   if        d1cCGI <> *blank
122900070301     c                   clear                   fidg09ds
123000070301     c                   eval      D09IOP0 = '001'
123100070301     c                   eval      d09ifgs = d1cFGS
123200070905     c                   eval      d09itug = 'C'
123300070301     c                   eval      d09icgi = d1cCGI
123400070301     c                   move      dateu8        d09idat
123500080606     c                   eval      kpjbus = kpjbu
123600070301     c                   eval      kpjbu = fidg09ds
123700070301     c                   call      'FIDG09R'
123800070301     c                   parm                    kpjba
123900070301     c                   eval      fidg09ds = kpjbu
124000080606     c                   eval      kpjbu = kpjbus
124100070320     c                   movel     d09oflo       dGRGflo
124200070320      * se il giro può essere messo in distinta
124300070320     c                   if        d09oerr = '1' or §grgDST = 'N'
124400070326     c                   movel     ErrMsg(8)     d1msg
124500070301     C                   SETON                                        54  99
124600070301     C                   goto      ENDCTR1
124700070301     c                   else
124800070301     c                   if        d09ocgi <> ' '
124900070301     c                   eval      d1dcgi = d09odes
125000070321     c                   movel     d1cCGI        wcgi
125100070404     c                   movel     d1cCGI        h1cCGI
125200070301     c                   move      'S'           con_giro
125300160318      *  controlla il tipo di giro se Fittizio
125400160318      **   (x bolle DPD generiche da spunte)
125500160318     c                   if        GIRO_vd_Pacco = d1cCGI
125600160318     c                   eval      GIRO_vedi_PACCO_si = 'S'
125700160321     c                   end
125800070301     c                   end
125900070301     c                   end
126000070301      *
126100070301     c                   end
126200070301     C*------------------------
126300070301      * Numero Distinta
126400070313     c                   if        d1ndc1 = *zeros
126500070313     c                   clear                   d1ndc1
126600070313     c                   end
126700070301     c                   if        d1ndc1 <> *blank
126800070301     c     '?'           scan      d1ndc1
126900070227     C*   ricerca distinta appartenente al giro oppure tutte
127000070227     c                   if        %Found
127100070326     C******             SETON                                            99
127200070606     c                   clear                   w1NO_dis
127300070606     c                   move      d1sce         w1sce
127400070321     c                   exsr      Ricerca_Dist
127500070228     C                   MOVE      *zeros        d1ndc1
127600070313     C                   MOVE      FDGNFV        d1ndc1
127700070326     C******             goto      ENDCTR1
127800070227     c                   end
127900070313     c                   if        d1ndc1 = *zeros
128000070313     c                   clear                   d1ndc1
128100070313     c                   end
128200070228      *  controllo validità distinta
128300070301     c     digits        check     d1ndc1
128400070301     c                   if        %Found
128500070326     c                   movel     ErrMsg(9)     d1msg
128600070301     C                   SETON                                        55  99
128700070301     C                   goto      ENDCTR1
128800070301     c                   end
128900070227      *
129000070301     c                   move      d1ndc1        wNFV
129100070319     c                   eval      Aggior_Dist = 'N'
129200070319      *                                     ?  =======  ?
129300070316     c                   exsr      CHAIN_Dist
129400070319     c                   if        Error_Dist = 'S'
129500070319     c                   if        Rec_Allocato = 'S'
129600070326     c                   movel     ErrMsg(11)    d1msg
129700070319     c                   end
129800070326     C                   SETON                                            55  99
129900070319     C                   goto      ENDCTR1
130000070319     c                   end
130100070319      *
130200070316     c                   if        Found_Dist = 'N'
130300070326     c                   movel     ErrMsg(9)     d1msg
130400070301     C                   SETON                                        55  99
130500070301     C                   goto      ENDCTR1
130600070301     c                   else
130700070417      * Distinta non nella data
130800070417     c                   if        dstDFV <> h1cDDC
130900070417     c                   movel     ErrMsg(30)    d1msg
131000070417     C                   SETON                                        55  99
131100070417     C                   goto      ENDCTR1
131200070417     c                   end
131300070301      * Foglio chiuso o Annullato
131400070301    3C     dstFCF        ifEQ      'S'
131500070301    4C     dstATB        orNE      ' '
131600070326     c                   movel     ErrMsg(10)    d1msg
131700070326     C                   SETON                                        55  99
131800070301     C                   goto      ENDCTR1
131900070301     c                   endIF
132000081121      *
132100081121      * Controlla se la distinta è gestita con il PDA
132200081121      *  se in test nel filler
132300081121     c                   eval      ddstFLR = dstFLR
132400081121     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
132500081121     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
132600081121     c                   eval      h1PDA_piu = 'S'
132700081121     c                   end
132800081121      *
132900070301     c                   end
133000160318      *
133100160318      *  controlla se l'Autista è fittizio quindi possiamo considerare
133200160318      *   la Distinta Fittizia
133300160318     C                   move      'A'           tipo_cod
133400160318     C                   move      dstpdr        cod_padr
133500160318     C     Kapd          CHAIN     fiapd01l
133600160318      *   se Escluso da Fatturazione
133700160318     c                   if        %Found(fiapd01l) and apdPDD = 'S'
133800160318     c                   movel     'S'           H1pdd_PIU
133900160318     c                   end
134000160318      *
134100160318      *  Se è un giro VEDI PACCO (DPD) deve appoggiarsi PER FORZA su una DISTINTA
134200160318      *  di un AUTISTA Escluso da Fatturazione (Fittizio) altrimenti errore
134300160318    3C                   if        GIRO_vedi_PACCO_si = 'S'  and
134400160322    3C                             H1pdd_PIU <> 'S' and s1sce ='1'
134500160322     c                   if             s1sce = Assegnare or
134600160322     c                                  s1sce = Trasferire
134700160318     c                   movel     ErrMsg(37)    d1msg
134800160318     C                   SETON                                        545599
134900160318     C                   goto      ENDCTR1
135000160322     c                   endIF
135100160318     c                   endIF
135200160322      *
135300160318      *
135400070301      * se tutto OK
135500070319     c                   z-add     dstPDR        h1pdr_piu
135600120209     c                   z-add     dstDFV        h1ddc_piu
135700070319     c                   move      dstFPP        h1fpp_piu
135800070319     c                   move      d1ndc1        h1ndc_piu
135900070228     c                   endIF
136000070302     c                   end
136100070227     C*
136200070227      * ?- Scelta (2) Toglie dalla distinta
136300070531     c                   if        d1sce = Togliere
136400070301     C*---------------
136500070227      *   Numero Distinta obbligatorio
136600070316     c********           if            D1NDC2 = *blank
136700070326     c********           movel     ErrMsg(9)     d1msg
136800070316     C********           SETON                                        56  99
136900070316     C********           goto      ENDCTR1
137000070316     c********           else
137100070228      *
137200070313     c                   if        d1ndc2 = *zeros
137300070313     c                   clear                   d1ndc2
137400070313     c                   end
137500070316     c     '?'           scan      d1ndc2
137600070228     C*   ricerca distinta appartenente al giro oppure tutte
137700070316     c                   if        %Found   or
137800070316     c                             d1ndc2 = *zeros or d1ndc2 = *blanks
137900070326     C******             SETON                                            99
138000070606     c                   move      d1sce         w1sce
138100070606     c                   clear                   w1NO_dis
138200070321     c                   exsr      Ricerca_Dist
138300070606      *
138400070606      * se tornando dalla ricerca distinta è stato scelto di fare un trasferimento
138500070606      *  invece di Togliere e basta
138600070606     c                   if        fdgOPZ = Trasferire
138700070606     C                   MOVE      Trasferire    d1sce
138800070606     C                   MOVE      *zeros        d1ndc3
138900070606     C                   MOVE      FDGNFV        d1ndc3
139000070606     c                   goto      ini_CTRD1
139100070606     c                   end
139200070606      *
139300070228     C                   MOVE      *zeros        d1ndc2
139400070313     C                   MOVE      FDGNFV        d1ndc2
139500070326     C******             goto      ENDCTR1
139600070228     c                   end
139700070228      *  controllo validità distinta
139800070301     c     digits        check     d1ndc2
139900070301     c                   if        %Found
140000070326     c                   movel     ErrMsg(9)     d1msg
140100070301     C                   SETON                                        56  99
140200070301     C                   goto      ENDCTR1
140300070301     c                   end
140400070301      *
140500070301     c                   move      d1ndc2        wNFV
140600070319     c                   eval      Aggior_Dist = 'N'
140700070319      *                                     ?  =======  ?
140800070316     c                   exsr      CHAIN_Dist
140900070319     c                   if        Error_Dist = 'S'
141000070319     c                   if        Rec_Allocato = 'S'
141100070326     c                   movel     ErrMsg(11)    d1msg
141200070319     c                   end
141300070326     C                   SETON                                            56  99
141400070319     C                   goto      ENDCTR1
141500070319     c                   end
141600070319      *
141700070316     c                   if        Found_Dist = 'N'
141800070326     c                   movel     ErrMsg(9)     d1msg
141900070301     C                   SETON                                        56  99
142000070301     C                   goto      ENDCTR1
142100080701      *
142200070301     c                   else
142300080701      *
142400070417      * Distinta non nella data
142500070417     c                   if        dstDFV <> h1cDDC
142600070417     c                   movel     ErrMsg(30)    d1msg
142700070417     C                   SETON                                        56  99
142800070417     C                   goto      ENDCTR1
142900070417     c                   end
143000070301      * Foglio chiuso o Annullato
143100070301    3C     dstFCF        ifEQ      'S'
143200070301    4C     dstATB        orNE      ' '
143300070326     c                   movel     ErrMsg(10)    d1msg
143400070326     C                   SETON                                        56  99
143500070301     C                   goto      ENDCTR1
143600070301     c                   endIF
143700080701      *
143800080701      * Controlla se la distinta è gestita con il PDA
143900080701      *  se in test nel filler
144000080701     c                   eval      ddstFLR = dstFLR
144100080701     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
144200080701     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
144300080701     c                   eval      h1PDA_meno = 'S'
144400080701     c                   end
144500080701      *
144600070301     c                   end
144700070227      *
144800160318      *  controlla se l'Autista è fittizio quindi possiamo considerare
144900160318      *   la Distinta Fittizia
145000160318     C                   move      'A'           tipo_cod
145100160318     C                   move      dstpdr        cod_padr
145200160318     C     Kapd          CHAIN     fiapd01l
145300160318      *   se Escluso da Fatturazione
145400160318     c                   if        %Found(fiapd01l) and apdPDD = 'S'
145500160318     c                   movel     'S'           H1pdd_meno
145600160318     c                   end
145700070301      * se tutto OK
145800070319     c                   z-add     dstPDR        h1pdr_meno
145900120209     c                   z-add     dstDFV        h1ddc_meno
146000070319     c                   move      dstFPP        h1fpp_meno
146100070319     c                   move      d1ndc2        h1ndc_meno
146200070227     c                   end
146300070316     c**************     end
146400070227     C*
146500070227      * ?- Scelta (3) Trasferisce dalla distinta alla distinta
146600070531     c                   if        d1sce = Trasferire
146700070301     C*---------------
146800070227     C*   Numero Distinta obbligatorio
146900070316     c*************      if            D1NDC3 = *blank
147000070326     c********           movel     ErrMsg(9)     d1msg
147100070316     C*************      SETON                                        57  99
147200070316     C*************      goto      ENDCTR1
147300070316     c*************      else
147400070228      *
147500070313     c                   if        d1ndc3 = *zeros
147600070313     c                   clear                   d1ndc3
147700070313     c                   end
147800070316     c     '?'           scan      d1ndc3
147900070228     C*   ricerca distinta appartenente al giro oppure tutte
148000070316     c                   if        %Found or
148100070316     c                             d1ndc3 = *zeros or d1ndc3 = *blanks
148200070326     C********           SETON                                            99
148300070606     c                   move      d1sce         w1sce
148400070606     c                   clear                   w1NO_dis
148500070321     c                   exsr      Ricerca_Dist
148600070606      *
148700070606      * se tornando dalla ricerca distinta è stato scelto di fare un togli
148800070606      *  invece di Trasferire e basta
148900070606     c                   if        fdgOPZ = Togliere
149000070606     C                   MOVE      Togliere      d1sce
149100070606     C                   MOVE      *zeros        d1ndc2
149200070606     C                   MOVE      FDGNFV        d1ndc2
149300070606     c                   goto      ini_CTRD1
149400070606     c                   end
149500070606      *
149600070228     C                   MOVE      *zeros        d1ndc3
149700070313     C                   MOVE      FDGNFV        d1ndc3
149800070326     C********           goto      ENDCTR1
149900070228     c                   end
150000070301      *  controllo validità distinta
150100070301     c     digits        check     d1ndc3
150200070301     c                   if        %Found
150300070326     c                   movel     ErrMsg(9)     d1msg
150400070301     C                   SETON                                        57  99
150500070301     C                   goto      ENDCTR1
150600070301     c                   end
150700070301      *
150800070301     c                   move      d1ndc3        wNFV
150900070319     c                   eval      Aggior_Dist = 'N'
151000070319      *                                     ?  =======  ?
151100070316     c                   exsr      CHAIN_Dist
151200070319     c                   if        Error_Dist = 'S'
151300070319     c                   if        Rec_Allocato = 'S'
151400070326     c                   movel     ErrMsg(11)    d1msg
151500070319     c                   end
151600070326     C                   SETON                                            57  99
151700070319     C                   goto      ENDCTR1
151800070319     c                   end
151900070319      *
152000070316     c                   if        Found_Dist = 'N'
152100070326     c                   movel     ErrMsg(9)     d1msg
152200070301     C                   SETON                                        57  99
152300070301     C                   goto      ENDCTR1
152400080701      *
152500070301     c                   else
152600080701      *
152700070417      * Distinta non nella data
152800070417     c                   if        dstDFV <> h1cDDC
152900070417     c                   movel     ErrMsg(30)    d1msg
153000070417     C                   SETON                                        57  99
153100070417     C                   goto      ENDCTR1
153200070417     c                   end
153300070301      * Foglio chiuso o Annullato
153400070301    3C     dstFCF        ifEQ      'S'
153500070301    4C     dstATB        orNE      ' '
153600070326     c                   movel     ErrMsg(10)    d1msg
153700070326     C                   SETON                                        57  99
153800070301     C                   goto      ENDCTR1
153900070301     c                   endIF
154000080701     C*
154100080701      * Controlla se la distinta è gestita con il PDA
154200080701      *  se in test nel filler
154300080701     c                   eval      ddstFLR = dstFLR
154400080701     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
154500080701     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
154600080701     c                   eval      h1PDA_meno = 'S'
154700080701     c                   end
154800080701     C*
154900070301     c                   end
155000160318      *
155100160318      *  controlla se l'Autista è fittizio quindi possiamo considerare
155200160318      *   la Distinta Fittizia
155300160318     C                   move      'A'           tipo_cod
155400160318     C                   move      dstpdr        cod_padr
155500160318     C     Kapd          CHAIN     fiapd01l
155600160318      *   se Escluso da Fatturazione
155700160318     c                   if        %Found(fiapd01l) and apdPDD = 'S'
155800160318     c                   movel     'S'           H1pdd_meno
155900160318     c                   end
156000070301      * se tutto OK
156100070319     c                   z-add     dstPDR        h1pdr_meno
156200120209     c                   z-add     dstDFV        h1ddc_meno
156300070319     c                   move      dstFPP        h1fpp_meno
156400070319     c                   move      d1ndc3        h1ndc_meno
156500070316     c*************      end
156600070301      *---------------
156700070227     C*   Numero Distinta obbligatorio
156800070316     c*************      if            D1NDC4 = *blank
156900070326     c********           movel     ErrMsg(9)     d1msg
157000070316     C*************      SETON                                        58  99
157100070316     C*************      goto      ENDCTR1
157200070316     c*************      else
157300070228      *
157400070326     c                   if        d1ndc4 = *zeros or d1ndc4 = *blanks
157500070313     c                   clear                   d1ndc4
157600070326     c                   else
157700070316     c     '?'           scan      d1ndc4
157800070228     C*   ricerca distinta appartenente al giro oppure tutte
157900070228      * ?- DA implementare ????????????????
158000070326     c                   if        %Found
158100070326     c********                     or d1ndc4 = *zeros or d1ndc4 = *blanks
158200070326     C**********         SETON                                            99
158300070606     c                   move      Scegliere     w1sce
158400070606     c                   move      d1ndc3        w1NO_dis
158500070321     c                   exsr      Ricerca_Dist
158600070606     c                   clear                   w1NO_dis
158700070228     C                   MOVE      *zeros        d1ndc4
158800070313     C                   MOVE      FDGNFV        d1ndc4
158900070326     C********           goto      ENDCTR1
159000070228     c                   end
159100070301      *  controllo validità distinta
159200070301     c     digits        check     d1ndc4
159300070301     c                   if        %Found
159400070326     c                   movel     ErrMsg(9)     d1msg
159500070301     C                   SETON                                        58  99
159600070301     C                   goto      ENDCTR1
159700070301     c                   end
159800070227     C*
159900070301     c                   move      d1ndc4        wNFV
160000070319     c                   eval      Aggior_Dist = 'N'
160100070319      *                                     ?  =======  ?
160200070316     c                   exsr      CHAIN_Dist
160300070319     c                   if        Error_Dist = 'S'
160400070319     c                   if        Rec_Allocato = 'S'
160500070326     c                   movel     ErrMsg(11)    d1msg
160600070319     c                   end
160700070326     C                   SETON                                            58  99
160800070319     C                   goto      ENDCTR1
160900070319     c                   end
161000070319      *
161100070316     c                   if        Found_Dist = 'N'
161200070326     c                   movel     ErrMsg(9)     d1msg
161300070301     C                   SETON                                        58  99
161400070301     C                   goto      ENDCTR1
161500080701      *
161600070301     c                   else
161700080701      *
161800070417      * Distinta non nella data
161900120207      *   Modificato il 7/2/2012 dando la possibilità di inserire su una distinta
162000120207      *    successiva alla data inserita
162100120207      * Ossia posso trasferire su una distinta del giorno dopo
162200120207     c********           if        dstDFV <> h1cDDC
162300120207     c                   if        dstDFV <  h1cDDC
162400070417     c                   movel     ErrMsg(30)    d1msg
162500070417     C                   SETON                                        58  99
162600070417     C                   goto      ENDCTR1
162700070417     c                   end
162800070301      * Foglio chiuso o Annullato
162900070301    3C     dstFCF        ifEQ      'S'
163000070301    4C     dstATB        orNE      ' '
163100070326     c                   movel     ErrMsg(10)    d1msg
163200070326     C                   SETON                                        58  99
163300070301     C                   goto      ENDCTR1
163400070301     c                   endIF
163500070301     c                   end
163600070326      *
163700080701      * Controlla se la distinta è gestita con il PDA
163800080701      *  se in test nel filler
163900080701     c                   eval      ddstFLR = dstFLR
164000080701     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
164100080701     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
164200080701     c                   eval      h1PDA_piu = 'S'
164300080701     c                   end
164400080701      *
164500070326     c                   end
164600070326      *
164700070314     c                   if            D1NDC3 = D1NDC4
164800070326     c                   movel     ErrMsg(9)     d1msg
164900070314     C                   SETON                                        575899
165000070314     C                   goto      ENDCTR1
165100070314     c                   end
165200070314      *---------------
165300160318      *
165400160318      *  controlla se l'Autista è fittizio quindi possiamo considerare
165500160318      *   la Distinta Fittizia
165600160318     C                   move      'A'           tipo_cod
165700160318     C                   move      dstpdr        cod_padr
165800160318     C     Kapd          CHAIN     fiapd01l
165900160318      *   se Escluso da Fatturazione
166000160318     c                   if        %Found(fiapd01l) and apdPDD = 'S'
166100160318     c                   movel     'S'           H1pdd_PIU
166200160318     c                   end
166300070301      * se tutto OK
166400070319     c                   z-add     dstPDR        h1pdr_piu
166500120209     c                   z-add     dstDFV        h1ddc_piu
166600070319     c                   move      dstFPP        h1fpp_piu
166700070319     c                   move      d1ndc4        h1ndc_piu
166800070316     c*************      end
166900070301     C*
167000070301     c                   end
167100070227     C*
167200070227     C     ENDCTR1       ENDSR
167300141013      *_______________________________________________________________*
167400141013     C     verdispo      BEGSR
167500141013      *_______________________________________________________________*
167600141029     c                   eval      ilry09fgs = d1cfgs
167700141029     c                   eval      ilry09tla = 'L'
167800141029     c                   eval      ilry09tdis= 'E'
167900141029     c                   movel     fnlry09ds2    kpjbu
168000141013     c                   call      'FNLRY09R'
168100141013     c                   parm                    kpjba
168200141029     c                   movel     kpjbu         fnlry09ds2
168300141013     C                   endsr
168400070301      *---------------------------------------------------------------*
168500070319      *?  Aggancia la Distinta x allocarla ed in seguito x aggiornarla ?
168600070301      *---------------------------------------------------------------*
168700070321     C     Ricerca_Dist  BEGSR
168800070228     C*
168900070321     C                   clear                   FIDG31DS
169000071029     c                   move      savPOSnf      fdgNFpos
169100070321     C                   Z-ADD     0             FDGNFV
169200070321     C                   MOVEL     wFGS          FDGFGS
169300070326      *
169400070326     C                   if        $TASTO   <>  F05
169500070321     C                   MOVEL     wCGI          FDGCGI
169600070326     c                   end
169700070326      *
169800070321     C                   Z-ADD     wDDC          FDGGDA
169900070321     C                   Z-ADD     wDDC          FDGGAL
170000070321     C                   move      wNPG          FDGNPG
170100070321     C                   move      'S'           FDGRIC
170200070321     C                   move      '2'           FDGTFV
170300070321     C                   move      'P'           FDGTPV
170400070606     C                   move      w1sce         FDGopz
170500070606     C                   z-add     w1NO_dis      FDGnf1
170600070625     C                   move      d1Tdis        FDGTdis
170700070625     C                   move      d1Tusc        FDGTusc
170800070823     C                   move      'N'           FDGstp
170900080616     C                   move      'E'           FDGieFAS
171000080616     C                   move      'STP'         FDGfas01
171100080616     C                   move      'CUS'         FDGfas02
171200070321     c                   eval      kpjbus = kpjbu
171300070404     c                   clear                   $FV6
171400070404      * impostato a tutti 99999
171500070404     c                   do        600           yy                3 0
171600070404     c                   z-add     999999        $FV6(yy)
171700070404     c                   enddo
171800080715     c                   movel     'ASSMANUALE'  fdgPGM
171900080606      * ?Attenzione:
172000080606      * ? Se richiamato da MONITOR con PDA, occorre modificare
172100080606      * ?  alcuni parametri:
172200080606     c                   if        PGM_chiamante = 'MONITOR'
172300110309     c**************
172400110309      *********  Se richiamato da MONITOR DEVE sempre inviare l'informazione <MONITOR>
172500110309      *********   occorre quindi fare le dovute modifiche sul FIDG31R per i
172600110309      *********   vari casi in funzione del Togliere oppure Mettere poichè
172700110309      *********    serve fare vedere solo la specifica distinta nel caso in cui
172800110309      *********     si deve eseguire solo il TOGLI. (Vedi il FIDG31R correlato)
172900110309     c**************                and d1sce= Togliere
173000110309     c**************
173100080606      *
173200080606     c                   eval       fdgPGM = 'MONITOR'
173300080606     c                   eval       fdgSTP = ' '
173400080616      * dal Monitor si vuole tutto
173500080616     C                   move      'I'           FDGieFAS
173600080616     C                   clear                   FDGfas01
173700080616     C                   clear                   FDGfas02
173800080606     C                   Z-ADD     G30nfv        FDGNFV
173900080606     c                   end
174000080606      *
174100070321     C                   MOVEL(p)  FIDG31DS      KPJBU
174200070321     C                   CALL      'FIDG31R'
174300070321     C                   PARM                    KPJBA
174400070404     C                   PARM                    $FV6
174500070321     C                   MOVEL     KPJBU         FIDG31DS
174600080606      *
174700071029     c                   if        FDGnf1 > 0
174800071029     c                   move      fdgNF1        fdgNFpos
174900071029     C                   end
175000080606      *
175100071029     c                   move      fdgNFpos      savPOSnf
175200070606     C                   clear                   FDGnf1
175300070606     c                   clear                   w1NO_dis
175400070321     c                   eval      kpjbu = kpjbus
175500070531     C*
175600070531     C*  se restituisce un'opzione x eseguire Togliere/Trasferire + giro
175700070321     C*
175800070321     C                   ENDSR
175900070321      *---------------------------------------------------------------*
176000070321      *?  Aggancia la Distinta x allocarla ed in seguito x aggiornarla ?
176100070321      *---------------------------------------------------------------*
176200070321     C     Chain_Dist    BEGSR
176300070321     C*
176400070316      * inizializza il trovato/non trovato
176500070316     c                   eval      Found_Dist = 'N'
176600070319     c                   eval      Error_Dist = 'N'
176700070319     c                   eval      rec_allocato = 'N'
176800070316     C*
176900070517      *   Dalla Vista logica prende il NrelRec  ?
177000070517     C     Kdst          CHAIN     fidst01l
177100071025     C*
177200071025      * ?Se non trovava il record allocava l'ultima distinta che aveva
177300071025      * ?trovato poichè il nrelrec. non si resettava ma era impostato
177400071025      * ?da prima.
177500071025     c                   if        not %Found(fidst01l)
177600071025     c                   eval      dst_nrr = *hival
177700071025     c                   end
177800070517     C*
177900070517      *   x agganciare il Fisico solo x allocare il Record  ?
178000070517     C     dst_nrr       CHAIN(e)  fidst00F
178100070517      * ?è stata usata questa tecnica perchè l'aggiornamento del FIDST
178200070517      * ?è fatto mediante il FIDG37R e,sotto COMIT,xchè tutto funzioni
178300070517      * ?correttamente,il file deve essere condiviso in SHARE altrimenti
178400070517      * ?non funziona. (Si è scelto di utilizzare x tutti il fisico)
178500070316     c*
178600070316     c                   if        %Error
178700070319     c                   eval      Error_Dist = 'S'
178800070316      * se il record è allocato mando messaggio info all'utente che alloca il rcd
178900070316     c*
179000070316     c                   clear                   trul82ds
179100070316     c                   eval      ul82§rrn = dst_nrr
179200070316     c                   eval      ul82§fil = 'FIDST00F'
179300070316     c                   eval      ul82§win = 'S'
179400070316     c                   eval      ul82§f7  = 'S'
179500070319     c                   eval      ul82§num = 2
179600070319     c                   eval      ul82§att = 2
179700070326     c                   eval      ul82§mss = ErrMsg(27)
179800070316     c                   Eval      UL82§msw = 'La Distinta '
179900070316     c                             + %editc(Wnfv:'Z') +  ' è già utilizzata da -
180000070319     c                              un altro lavoro e non è manutenzionabile'
180100070316      *
180200070316      * chiamo il pgm che manda il messaggio info all'utente
180300070316     c                   call(e)   'TRUL82R'
180400070316     c                   parm                    trul82ds
180500070316      *
180600070316     c                   if        ul82§sts = 'A'
180700070319     c                   move      'S'           rec_allocato
180800070316     c                   endif
180900070316     C*
181000070316     c                   end
181100070316      * Trovato record
181200070517     c                   if        %Found(fidst00F)
181300070316     c                   eval      Found_Dist = 'S'
181400070319     C*
181500070319      *   Se richiamato x aggiornamento finale dopo ARG e ARB  ?
181600070319      *   quindi non solo per semplice allocazione del record. ?
181700070319     c                   if        Aggior_Dist = 'S'
181800070319      *                                     ?  =======  ?
181900070319     c                   exsr      Campi_Distinta
182000070517      *                                     ?  =======  ?
182100070517     c                   else
182200070517      * ?  qui l'Update serve esclusivamente per tenere allocato  ?
182300070517      * ?  il record in quanto nel CLLE FIDG30C x il COMMIT è    ?
182400070517      * ?  stato attivato lo STRCMTCTL con *CHG.    ?
182500070316     c                   update    fidst000
182600070517     C*
182700070517     c                   endIF
182800070517     C*
182900070316     c                   end
183000070316     C*
183100070316     C     ENDChain      ENDSR
183200070316      *---------------------------------------------------------------*
183300070319      *  ?  Imposta i campi della distinta da Aggiornare             ?
183400070316      *---------------------------------------------------------------*
183500070319     C     Campi_DistintaBEGSR
183600070517     C*********
183700070517     c*********          clear                   fidg28ds
183800070517     c*********          eval      D28FGSI = Wfgs
183900070517     c*********          eval      D28DFVI = h1cddc
184000070517     c*********          eval      D28NFVI = wnfv
184100070517     c*********          call      'FIDG28R'
184200070517     c*********          parm                    fidg28ds
184300070517     c*********          if        d28erro = ' '
184400070517     c*********          eval      DSTPKLa = D28PKLao
184500070517     c*********          eval      DSTVLUa = D28VLUao
184600070517     c*********          eval      DSTstpa = D28stpao
184700070517     c*********          eval      DSTNBLa = D28NBLao
184800070517     c*********          else
184900070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
185000070517     c*********          eval      Error_Dist  = 'S'
185100070517     c*********          end
185200070319     C*
185300070517      * ?  Oltre a calcolare i valori aggiorna anche il FIDST e FIDST40
185400070517     c                   clear                   fidg37ds
185500070517     c                   eval      D37FGSI = Wfgs
185600070517     c                   eval      D37DFVI = h1cddc
185700070517     c                   eval      D37NFVI = wnfv
185800070517      * ?  FIDG37R non deve eseguire il Commit ma lo deve fare il FIDG30R
185900070517     c                   eval      D37CMTI = 'N'
186000070829     c                   eval      D37dtaI = h1dtaI
186100070829     c                   eval      D37oraI = d1oraI
186200070517     c                   eval      kpjbus = kpjbu
186300070517     c                   eval      kpjbu  = fidg37ds
186400070517     c                   call      'FIDG37R'
186500070517     c                   parm                    kpjba
186600080606      ** mancava questa specifica che reimpostava la DS x caricare i 4 campi
186700080606     c                   eval      fidg37ds = kpjbu
186800070517     c                   eval      kpjbu = kpjbus
186900070517     c                   if        d37erro = ' '
187000070517     c                   eval      DSTPKLa = D37PKLao
187100070517     c                   eval      DSTVLUa = D37VLUao
187200070517     c                   eval      DSTstpa = D37stpao
187300070517     c                   eval      DSTNBLa = D37NBLao
187400070517     c                   else
187500070517      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
187600070517     c                   eval      Error_Dist  = 'S'
187700070517     c                   end
187800070517     C*
187900070319     C                   ENDSR
188000070319      *---------------------------------------------------------------*
188100070319      *  ?  GESTIONE SFL GIRI  da selezionare                        ?
188200070319      *---------------------------------------------------------------*
188300070319     C     GESS1         BEGSR
188400070319     C*
188500940223     C* inizializzazione videata
188600940223     C     $INZS1        IFEQ      *ON
188700940127     C                   EXSR      INZS1
188800940223     C                   MOVE      *OFF          $INZS1
188900940127     C                   ENDIF
189000070327     C*
189100070306     C* Inizializza e imposta piede videata
189200070306     C                   EXSR      INZZ1
189300070306     C*
189400030113     C* emissione piede videata
189500070302      * ?            *-----------------------*
189600070228     C                   WRITE     FDG0Z01
189700070302      * ?            *-----------------------*
189800070228     C*
189900940223     C     WMAX          IFEQ      0
190000070228     C* Non ci sono records
190100070302      * ?            *-----------------------*
190200070228     C                   WRITE     FDG0V01
190300070302      * ?            *-----------------------*
190400070301     c                   setoff                                       30
190500030114     C                   Else
190600070301     c                   seton                                        30
190700070330     c                   eval       sav_WSfl =  WSfl
190800070330     c                   eval       sav_Wpag =  Wpag
190900070301      *
191000030114     C     Wsfl          IFgt      0
191100030114     C                   Z-ADD     wsfl          C1RCD
191200030114     C                   Else
191300030114     C     Wpag          IFgt      0
191400030114     C                   Z-ADD     wpag          C1RCD
191500030114     C                   EndIF
191600030114     C                   EndIF
191700030114     C                   ENDIF
191800070228      *
191900070228      *  Emissione SFL dei GIRI
192000070228      * ?            *-----------------------*
192100070228     C                   EXFMT     FDG0C01
192200070228      * ?            *-----------------------*
192300070326     c                   setoff                                       99
192400070305     C                   movel     'S1'          sav$GEST
192500070228      *
192600940204     C     C1NRR         IFNE      0
192700940204     C                   Z-ADD     C1NRR         WSFL
192800940204     C                   ENDIF
192900940127     C                   Z-ADD     SFLNRR        C1RCD
193000030113     C* Selezioni
1931009401271    C                   SELECT
193200070328     C*
193300070328     C* F3=Fine
193400070328     C     $TASTO        WHENEQ    F03
193500070328     C                   EXSR      F03_exit
193600070328     c                   rolbk
193700070328      *
193800070328     C* F5=Int.tutte le Distinte
193900070328     C     $TASTO        WHENEQ    F05
194000070328     C                   EXSR      F05_interr
194100070328      *
194200070328     C* F12=Ritorno
194300070328     C     $TASTO        WHENEQ    F12
194400070328     c                   move      'D1'          $GEST
194500070328     C                   MOVE      *ON           $INZD1
194600070328     c                   rolbk
194700070328      *
194800070328     C* F1=Seleziona Tutti
194900070328     C                   WHEN      $TASTO   =   F01
195000070328     C                   EXSR      F01_Tutti1
195100070328      *
1952009401271O   C                   OTHER
195300070228     C* CONTROLLO testata
195400940131     C                   EXSR      CTRC1
195500070228     C* CONTROLLO subfile
195600940201     C     *IN99         IFEQ      *OFF
195700940131     C                   EXSR      CTRS1
195800940131     C                   END
195900070302      *
196000070302     C* F4=Int.Distinte Stesso Giro
196100070302      *   Ma deve esserne selezionata solo una altrimenti segnala l'errore
196200070302     C     *IN99         IFEQ      *OFF
196300070302     C     $TASTO        andeq     F04
196400070302     c                   if        Conta_Giri = 1
196500070302     C                   EXSR      F04_interr
196600070326      *
196700070326      *  se non ha scelto il foglio deve eliminare la selezione sul sfl dei giri
196800070326     c                   if        fdgnfv = 0
196900070326     c     sav_s1nrr     chain     FDG0S01                            21
197000070326     c                   if        *in21 = *off
197100070326     c                   clear                   s1sce
197200070326     c                   update    FDG0S01
197300070326     c                   end
197400070326     c                   end
197500070326      *
197600070302     c                   else
197700070302      * errore
197800070302     c                   seton                                        51  99
197900070302     c                   end
198000070302     c                   END
198100070327      *
198200070504      * ?se premuto "Enter" è stato richiesto il dettaglio di tutte le
198300070228      * ? spedizioni dei giri selezionati per selezione bolla bolla.
198400070228     C     *IN99         IFEQ      *OFF
198500070419     C     $TASTO        andeq     Enter                                        ex F9
198600070302     C                   MOVE      *ON           $INZS2
198700070301     C                   movel     'S2'          $GEST
198800070306     C                   MOVE      *ON           $VISS2
198900070228     C                   END
199000070228      *
199100070228      * ? emette la finestra dei totali Distinta
199200070328      * ?  se non ci sono stati errori precedentemente
199300070504      * ?se NON premuto Enter bensì F6 e immessa una distinta,
199400070328      * ?   e se è stato ripulito il messaggi degli errori precedenti
199500070328      * ?  se il sfl è carico.
199600070228     C     *IN99         IFEQ      *OFF
199700070419     C     $TASTO        andne     Enter                                        ex F9
199800070504     C     $TASTO        andeq     F06                                          ex F9
199900070328     c     ErrMsgZ       andEQ     *blank
200000070328     C     WMAX          andGT     0
200100070306     C                   MOVE      *ON           $INZS2
200200070306     C                   movel     'S2'          $GEST
200300070306     C                   MOVE      *OFF          $VISS2
200400070314     c                   end
200500070228      *
2006009401271-   C                   ENDSL
200700940127     C*
200800940127     C                   ENDSR
200900070328     C/EJECT
201000070328      *---------------------------------------------------------------*
201100070328      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
201200070328      *---------------------------------------------------------------*
201300070328     C     INZZ1         BEGSR
201400070328     C*
201500070306     C* Messaggio o tasti funzionali
201600070306     c                   clear                   z1dmsg
201700070328     c                   clear                   ErrmsgZ          80
201800070307     C*
201900070306      * errore di selezione di + giri x richiesta interr.stesso giro
202000070306     c                   if        *in51 = *on
202100070328     c                   movel     ErrMsg(12)    ErrmsgZ
202200070306     c                   setoff                                       51
202300070306     c                   end
202400070319     c                   if        *in52 or *in54
202500070328     c                   movel     ErrMsg(13)    ErrmsgZ
202600070319     c                   setoff                                       5254
202700070306     c                   end
202800070326     c                   if        *in62
202900070328     c                   movel     ErrMsg(15)    ErrmsgZ
203000070326     c                   setoff                                       62
203100070319     c                   end
203200070319     c                   if        *in53 or *in55
203300070328     c                   movel     ErrMsg(14)    ErrmsgZ
203400070319     c                   setoff                                       5355
203500070306     c                   end
203600070326     c                   if        *in56 = *on
203700070328     c                   movel     ErrMsg(16)    ErrmsgZ
203800070326     c                   setoff                                       56
203900070326     c                   end
204000070328      *
204100070328      * imposta eventuali errori
204200070328     c                   eval      z1dmsg = ErrmsgZ
204300070307     C* tasti funzionali
204400070328     c                   if        ErrmsgZ = *blank
204500070307     c                   setoff                                       99
204600070307     c                   movel     Fun_1         z1dmsg
204700070419     c                   eval      %subst(z1dmsg:20:31) = FUN_6
204800070419     c                   move      Fun_Enter     z1dmsg
204900070326     c                   end
205000070306     C*
205100070306     C                   ENDSR
205200070306     C/EJECT
205300070306      *---------------------------------------------------------------*
205400070320      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
205500070306      *---------------------------------------------------------------*
205600070306     C     INZS1         BEGSR
205700070306     C*
205800070228     C* reset indicatori DSPATR
205900070308     c                   exsr      INZ_indERR
206000940302     C* pulizia SFL
206100940128     C                   SETOFF                                         3031
206200070228     C                   WRITE     FDG0C01
206300940128     C                   SETON                                          31
206400940128     C*
206500070228     C* Righe x Pagina del 1° SFL
206600070427     c                   z-add     RigPag_SFl1   s1pag
206700070228      *
206800030113     C* CARICAMENTO SFL totale
206900940201     C                   Z-ADD     0             S1NRR
207000030113     C                   Z-ADD     1             C1RCD
207100940128     C                   Z-ADD     0             WMAX
207200070302     C                   Z-ADD     0             Wsfl
207300070302     C* campi titolo
207400070302     C                   MOVEL     dsPGM         c1CPGM
207500070302     C                   MOVEL     knsif         c1nsif
207600070302     C                   MOVEL     knmus         c1nmus
207700070302     C                   MOVEL     RSUT          c1crsu
207800070308     C                   clear                   c1ndc1
207900070302     C*
208000070302     C* Se immessa la Distinta  disabilita Tasti Funzionali di ricerca
208100070315     c                   setoff                                       44
208200070319     c                   if        h1ndc_piu  > 0
208300070302     c                   movel     Alla_Dist     c1ndc1
208400070319     c                   move      h1ndc_piu     distintalfa
208500070305     c                   do        7             xy
208600070305     c                   if        ndc_a(xy) <> '0'
208700070305     c                   leave
208800070305     c                   end
208900070305     c                   move      ' '           ndc_a(xy)
209000070305     c                   enddo
209100070305     c                   move      distintalfa   c1ndc1
209200070302     C* ND tasti di ricerca Distinte
209300070315     c********           seton                                        44
209400070302     c                   end
209500070403     C*
209600070403     c* se il p.o. è in parallelo testo la distinta su ARG altrimenti
209700070403     c* se il p.o. è partito testo la distinta su ARB
209800070302     C*
209900940224     C* Posizionamento su file pilota
210000070228      * ? Se presente il giro
210100070228     c                   IF        con_GIRO = 'S'
210200070403     c     k52j          setll     fiarg52j
210300070403     c                   ELSE
210400070403     c     wfgs          setll     fiarg52j
210500070228     c                   END
210600030113     C* Carico il SFL
210700940127     C                   EXSR      ROLS1
210800030113     C*
210900030114     C                   Z-ADD     1             WPAG
211000070504     c                   eval       sav_Wmax =  Wmax
211100940127     C*
211200940127     C                   ENDSR
211300070305     C/EJECT
211400070305      *---------------------------------------------------------------*
211500070320      *? CARICAMENTO PAGINA LISTA                                    ?
211600070305      *---------------------------------------------------------------*
211700070305     C     ROLS1         BEGSR
211800070305     C*
211900070305     C                   SETOFF                                       32
212000070305     C                   Z-ADD     0             Y
212100070305     C                   Z-ADD     WMAX          S1NRR
212200070305     C*
212300070305     C* Caricamento del Sfl x Giro
212400070305     c                   Exsr      Write_S1
212500070305     C*
212600070305     C                   Z-ADD     S1NRR         WMAX                 30
212700070305     C*
212800070305     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
212900070305     C     S1NRR         DIV       S1pag         PAGINE            4 0
213000070305     C                   MVR                     RESTO             3 0
213100070305     C     PAGINE        MULT      S1pag         C1RCD
2132000703051    C     RESTO         IFGT      0
213300070305     C                   ADD       1             C1RCD
2134000703051E   C                   ELSE
213500070305     C                   SUB       S1pag         C1RCD
213600070305     C                   ADD       1             C1RCD
2137000703051-   C                   ENDIF
213800070305     C*
213900070305     C                   ENDSR
214000070301      *---------------------------------------------------------------*
214100070320      *?  Scrive x Ogni rottura di GIRO                              ?
214200070301      *---------------------------------------------------------------*
214300070305     C     Write_S1      BEGSR
214400070305      *
214500070305     C*  Esegue una lettura di posizionamento GIRO fuori
214600070305      *   per poi poter totalizzare x Giro all'interno della Rout.RIGA_Giro
214700070305      *   richiamata anche in seguito.
214800070305     c                   exsr      Legge_rec
214900940128     C*
2150000702281    C     $EFILE        DowEQ     *OFF
215100070228     C*
215200070228     C*  pulizia totali x Giro
215300070228     c                   movel     argCGI        savARGCGI
215400070228     c                   z-add     0             totSPE_cgi
215500070228     c                   z-add     0             totPKL_cgi
215600070228     c                   z-add     0             totVLU_cgi
215700070228     c                   z-add     0             totCOL_cgi
215800070228      *
215900070228      * Finchè il giro è lo stesso
2160000702281    C     $EFILE        DowEQ     *OFF
2161000702281    C     argCGI        andeq     savARGCGI
216200070228      *
216300070228      *   somma ai totali del Giro
216400070228     c                   add       1             totSPE_cgi
216500100505      *
216600070302      *  Pesi Volumi Colli da consegnare
216700070305     c                   exsr      Cal_PesVol
216800070302      *   i pesi/volumi/Colli Reali da consegnare
216900070302     c                   add       D27PKLDCO     totPKL_cgi
217000070302     c                   add       D27VLUDCO     totVLU_cgi
217100070302     c                   add       D27NCLDCO     totCOL_cgi
217200070228      *
217300070228      *  Esce da qui con un record di Totale x GIRO
217400070228     c                   exsr      Legge_rec
217500070228     c                   EndDO
217600070305      *---------
217700070305      *  Scrive la riga di SFL a rottura di giro
217800070305     C                   clear                   FDG0S01
217900070305     C* codice Giro
218000070305     c                   movel     savARGCGI     s1CGI
218100070320     c                   move      'S'           Write_riga        1
218200070305     C* Descrizione Giro
218300070305     c     kgrg          setll     figrg03l
218400070305     c                   do        *hival
218500070305     c     kgrg          reade     figrg03l
218600070305     c                   if        %eof(figrg03l)
218700070305     c                   leave
218800070305     c                   endif
218900070305     c                   if        dateu8 >= grgdde and dateu8 <= grgdscad
219000070305     c                   movel     grgdes        s1DES
219100070320     c                   movel     grgflo        dgrgFLO
219200070320      * se non deve andare in distinta
219300070326      * ?lo deve comunque caricare e controllare in seguito per non
219400070326      * ?renderlo disponibile alla selezione.
219500070326     c                   if        §grgDST = 'N'
219600070326     c*******            move      'N'           Write_riga        1
219700070326     c                   move      §grgDST       H1inDIS
219800070326     c                   end
219900070305     c                   endif
220000070305     c                   enddo
220100070305     C* Totali spedizioni
220200070305     c                   z-add     totSPE_cgi    s1tNSP
220300070305     c                   z-add     totPKL_cgi    s1tPKL
220400070305     c                   z-add     totVLU_cgi    s1tVLU
220500070305     c                   z-add     totCOL_cgi    s1tCOL
220600070305     c*
220700070305     c* Se selezionato un solo giro viene impostato automaticamente
220800070305     c*  la selezione della riga con il SFLNXTCHG acceso
220900070305     c                   if        d1cCGI <> *blank
221000070305     c                   eval      s1SCE = '1'
221100070305     c                   seton                                        32
221200070305     c                   end
221300070305     C*
221400070320     c                   if        Write_riga = 'S'
221500070305     C                   ADD       1             S1NRR
221600070305     C                   ADD       1             Y
221700070305     C                   WRITE     FDG0S01
221800070320     c                   end
221900070305      *---------
222000070228     c                   EndDO
222100070228     C*
222200070228     C                   ENDSR
222300070301      *---------------------------------------------------------------*
222400070320      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
222500070301      *---------------------------------------------------------------*
222600070228     C     LEGGE_REC     BEGSR
222700070228     C*
222800070228     C*  Per EoF deve uscire
222900940131     C                   MOVEL     *OFF          $EFILE
223000070228     C*  Per Record Ok deve uscire riportando i dati
223100070228     C                   MOVEL     *OFF          $RCDOK
223200070228     C*
2233009401311    C     $EFILE        DOUEQ     *ON
223400940131     C     $RCDOK        OREQ      *ON
223500070403     C*
223600070403      * ? Se presente il giro
223700070403     c                   IF        con_GIRO = 'S'
223800070403     c     k52j          reade     fiarg52j
223900070403     c                   ELSE
224000070403      * ? Se NON presente il giro
224100070403     c     wfgs          reade     fiarg52j
224200070403     c                   END
224300070403      * Fine File
224400070403     C                   IF        %Eof(fiarg52j)
224500070228     C                   MOVEL     *ON           $EFILE
224600070228     C                   MOVE      $EFILE        *IN33
224700070403      *
224800070403     C                   else
224900070403      *
225000070419     c                   if        (test_Parallelo = 'S' and argNDC = 0) or
225100070419     c                             (test_Parallelo <>'S' and arbNDC = 0)
225200070404      *
225300070228      * ?Controlla se la Spedizione è da considerare
225400070228     C                   exsr      Check_Sped
225500070228      * ?     se Non è Esclusa dalla consegna
225600070228     c                   if        d16conso <> 'N'
225700070228     C                   MOVE      *ON           $RCDOK
225800070228     c                   end
225900100505      *
226000130424      * ? Cotrolla inoltre solo in immissione
226100130424      * ?   se richiesto di vedere o meno le bolle con Particolarità di attesa
226200100505      * ?   e consegna richiesta NON raggiunta.
226300130424     c                   clear                   in_attesa
226400100505     c                   if        D1pATT = 'N'
226500100505     c                   exsr      ChK_attesa
226600100505     c                   if           in_attesa = 'S'
226700100505     C                   MOVE      *Off          $RCDOK
226800100505     C                   END
226900100505     c                   end
227000070403      *
227100130424     C                   end
227200070403     C                   END
227300070228     C*
2274009401311-   C                   ENDDO
227500940131     C*
227600940131     C                   ENDSR
227700070301      *---------------------------------------------------------------*
227800070316      *  ?  Seleziona Tutti i record del SFL                         ?
227900070301      *---------------------------------------------------------------*
228000070307     C     F01_Tutti1    BEGSR
228100070305     C*
228200070314     C     WMAX          IFgt      0
228300070305     C*  Seleziona tutte le righe del SFL accendendo il SFLNXTCHG
228400070305     c                   z-add     1             nrr_s1
228500070305     c     nrr_s1        chain     FDG0S01                            21
228600070305     c                   dow       *in21 = *Off
228700070305     c                   move      '1'           s1sce
228800070305     C*
228900070305      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
229000070305     C                   MOVE      *ON           *IN32
229100070305     C                   UPDATE    FDG0S01
229200070305     C*
229300070305     c                   add       1             nrr_s1
229400070305     c     nrr_s1        chain     FDG0S01                            21
229500070305     c                   endDO
229600070314     c                   end
229700070305     C*
229800070305     C                   ENDSR
229900070305      *---------------------------------------------------------------*
230000070320      *?   CONTROLLO TESTATA LISTA                                   ?
230100070305      *---------------------------------------------------------------*
230200070305     C     CTRC1         BEGSR
230300070301     C                   MOVE      *OFF          *IN99
230400070301     C                   ENDSR
230500070301      *---------------------------------------------------------------*
230600070320      *?   CONTROLLO OPZIONI LISTA                                   ?
230700070301      *---------------------------------------------------------------*
230800070301     C     CTRS1         BEGSR
230900070301     C*
231000070301     C*  Pulizia campi Totali
231100070301     c                   z-add     0             totSPE_cgi
231200070301     c                   z-add     0             totPKL_cgi
231300070301     c                   z-add     0             totVLU_cgi
231400070301     c                   z-add     0             totCOL_cgi
231500070302     C*
231600070302     C*  Pulizia campi Totali
231700070302     c                   z-add     0             totSPE_s1
231800070302     c                   z-add     0             totPKL_s1
231900070302     c                   z-add     0             totVLU_s1
232000070302     c                   z-add     0             totCOL_s1
232100070301     C*
232200070302     C                   z-add     0             Conta_Giri        3 0
232300070301     C                   MOVEL     *OFF          $ESCI
232400071129     c                   clear                   ErrMsgZ
232500160322     c                   clear                   un_VEDI_Pacco     1
232600070301     C                   SETOFF                                       99
232700070301     C*
232800070301     C* Leggo il sfl solo se ci sono rcd
2329000703011    C     WMAX          IFGT      0
233000070301     C                   READC     FDG0S01                                21
233100070301     C*
233200070301     C* esce se fine sfl o errore che richiede l'uscita
2333000703012    C     *IN21         DOWEQ     *OFF
233400070301     C     $ESCI         ANDEQ     *OFF
233500070301     C                   Z-ADD     S1NRR         C1RCD
233600070301     C* ctrl su riga
233700070301     C                   EXSR      RECS1
233800070301      *
233900070301     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
2340000703013    C     *IN99         IFEQ      *ON
234100070301     C     $ESCI         OREQ      *ON
234200070301     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
234300070301     C* la ripristinerò a conclusione del ciclo di READC
234400070301     C                   MOVE      *OFF          $INZS1
2345000703013-   C                   ENDIF
234600070301     C*
234700070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
234800070301     C                   MOVE      *ON           *IN32
234900070301     C                   UPDATE    FDG0S01
235000070301     C*
235100070301     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
2352000703013    C     $ESCI         IFEQ      *OFF
235300070301     C                   READC     FDG0S01                                21
235400070301      *
235500070301     C* a fine ciclo ripristino il flag richiesta iniz.sfl
2356000703014    C     *IN21         IFEQ      *ON
235700070301     C* calcolo pagina a cui deve posizionarsi
235800070301     c                   z-add     s1pag         Wpagine
235900070301     C                   EXSR      CLCPAG
2360000703014-   C                   ENDIF
2361000703013-   C                   ENDIF
236200070301     C*
2363000703012-   C                   ENDDO
236400070301     C*
2365000703011-   C                   ENDIF
236600070301     C*
236700070302     C*  Imposta i Totali come se avesse eseguito il 2°SFL
236800070302     c                   z-add     totSPE_s1     totSPE_s2
236900070302     c                   z-add     totPKL_s1     totPKL_s2
237000070302     c                   z-add     totVLU_s1     totVLU_s2
237100070302     c                   z-add     totCOL_s1     totCOL_s2
237200070302     C*
237300070327     C*  Controlli generali sulle scelte fatte
237400070328     c                   if        ErrMsgZ = *blank
237500070327     C*
237600070327     C* Se non è stata scelto  nemmeno un giro... errore
237700070327     c                   clear                   err001
237800070327     c                   clear                   err002
237900070327     c                   clear                   err003
238000070327      *
238100071129     c                   if        Conta_Giri = 0
238200071129     c                   if        not *in99
238300070327     c                   movel     ErrMsg(19)    err001
238400070327     c                   movel     ErrMsg(20)    err002
238500070327     c                   clear                   err003
238600071129     c                   end
238700070327     c                   else
238800070327     C* Se non è stata scelta  nemmeno una distinta... errore
238900070327     c                   if        h1ndc_piu = 0
239000070328     c                   if        $tasto <> F04 and $tasto <> F05 and
239100070419     c                             $tasto <> Enter                              ex F9
239200070327      * emette messaggio di Avviso x scegliere una Distinta se non c'è
239300070327     c                   movel     ErrMsg(17)    err001
239400070327     c                   clear                   err002
239500070327     c                   movel     ErrMsg(18)    err003
239600070327     c                   end
239700070327     c                   end
239800070327     c                   end
239900070327     C*
240000160318     C*  Controlla se il GIRO scelto era Fittizio e c'è invece una Distinta
240100160318     C*    di un Autista NON Fittizio
240200160322     c                   if        h1ndc_piu > 0 and h1pdd_piu = ' '
240300160322     c                             and un_vedi_PACCO ='S'
240400160318      * emette messaggio di Avviso x aver scelto un GIRO fittizio e una Distinta
240500160318     C*  non appartenente ad un AUTISTA Fittizio
240600160318     c                   clear                   err001
240700160318     c                   movel     ErrMsg(37)    err002
240800160318     c                   clear                   err003
240900160318     c                   end
241000160318     C*
241100070327     C*  Messaggio di avviso
241200070327     c                   if        err001 <> *blank or
241300070327     c                             err002 <> *blank or
241400070327     c                             err003 <> *blank
241500070327     c                   seton                                        99
241600070327      * ?            *-----------------------*
241700070327     C                   exfmt     FDG0WER
241800070327      * ?            *-----------------------*
241900070327     c                   end
242000070327     C*
242100070328     c                   end
242200070328     C*
242300070301     C                   ENDSR
242400070301     C/EJECT
242500070301      *---------------------------------------------------------------*
242600070320      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
242700070301      *---------------------------------------------------------------*
242800070301     C     RECS1         BEGSR
242900070301     C*
243000070301     C* reset indicatori DSPATR
243100070308     c                   exsr      INZ_indERR
243200070326     C*
243300070326     c* se è stato scelto ed è un giro che non può essere messo
243400070326     C*  in distinta --> messaggio di errore
243500070326     c                   If        s1SCE = '1' and H1inDIS = 'N'
243600160318     c                               or
243700160322     c                             s1SCE = '1' and  s1CGI = GIRO_vd_Pacco
243800160322     c                               and h1pdd_PIU <>'S' and h1ndc_PIU > 0
243900160322     C*
244000160322     C                   seton                                        9956
244100160322     c                   eval      un_VEDI_Pacco = 'S'
244200160322     C*
244300070326     c                   enD
244400070327     C*
244500070327     C* gestione opzioni
2446000703273    C     S1sce         IFNE      *blank
244700070327     C     *IN99         ANDEQ     *OFF
244800070327     C                   EXSR      SCE_S1
2449000703273-   C                   ENDIF
245000070301     C*
245100070301     C                   ENDSR
245200070301      *---------------------------------------------------------------*
245300070320      *?   GESTIONE OPZIONI LISTA                                    ?
245400070301      *---------------------------------------------------------------*
245500070301     C     SCE_S1        BEGSR
245600070301     C*
245700070301     C*  Totalizza le Spedizioni/Pesi/Volumi
245800070301     c                   If        s1SCE = '1'
245900070302     C                   ADD       1             Conta_Giri
246000070313     c                   move      s1cgi         sav_sxcgi
246100070326     c                   move      s1nrr         sav_s1nrr
246200070301     C*
246300070301     c                   add       s1Tnsp        totSPE_s1
246400070301     c                   add       s1Tpkl        totPKL_s1
246500070301     c                   add       s1Tvlu        totVLU_s1
246600070301     c                   add       s1Tcol        totCOL_s1
246700070301     c                   endIf
246800070301     C*
246900070301     C                   ENDSR
247000070301     C/EJECT
247100070301      *---------------------------------------------------------------*
247200070316      *  ?  GESTIONE SFL Bolle da selezionare                        ?
247300070301      *---------------------------------------------------------------*
247400070301     C     GESS2         BEGSR
247500070301     C*
247600070301     C* inizializzazione videata
247700070301     C     $INZS2        IFEQ      *ON
247800070301     C                   EXSR      INZS2
247900070301     C                   MOVE      *OFF          $INZS2
248000070301     C                   ENDIF
248100070306     C*
248200070326     C* Se non si sta eseguendo l'Assegnazione oppure se siamo in Assegnazione
248300070306     C* ed è stato richiesto di selezionare le Bolle dei Giri selezionati
248400070531     c                   if        $VISS2 = *On or D1sce <> Assegnare
248500070306     C*
248600070306     C* Inizializza e imposta piede videata
248700070306     C                   EXSR      INZZ2
248800070305     C*
248900070301     C* emissione piede videata
249000070302      * ?            *-----------------------*
249100070301     C                   WRITE     FDG0Z02
249200070302      * ?            *-----------------------*
249300070301     C*
249400070301     C     WMAX          IFEQ      0
249500070301     C* Non ci sono records
249600070302      * ?            *-----------------------*
249700070301     C                   WRITE     FDG0V02
249800070302      * ?            *-----------------------*
249900070301     c                   setoff                                       30
250000070301     C                   Else
250100070301     c                   seton                                        30
250200070301      *
250300070515     c                   If        posizionamento <> 'S'
250400070515     c                   If        posizionato =  'S'
250500070515     c                   z-add     C2RCD_pos     c2rcd
250600070515     c                   else
250700070301     C     Wsfl          IFgt      0
250800070503      * essendo il posizionamento *TOP non è + corretto impostare direttamente
250900070503      * il record ma occorre ricavare il primo record della pagina.
251000070503     c                   z-add     0             c2rcd
251100071121      *
251200070503     c                   if        wrigsfl2 > 0
251300070503     c     wsfl          div       wrigsfl2      C2RCD
251400071121     c                   mvr                     resto
251500071121     c                   if        c2rcd > 0
251600070503     c                   mult      wrigsfl2      C2RCD
251700071121     c                   if        resto > 0
251800071121     c                   add       1             C2RCD
251900071121     c                   end
252000071121     c                   else
252100071121     c                   z-add     1             C2RCD
252200071121     c                   end
252300071121     c                   end
252400071121      *
252500071121      * attenzione all'incremento di 1 può far schiantare il sfl
252600071121      * poichè può superare il nr.max delle righe presenti.
252700071121      * esempio se wsfl = 16 o un suo multiplo essendo wrigsfl2 = 16
252800071121      * c2rcd alla fine equivale a wsfl ossia 16 ....e se in seguito
252900071121      * si aggiunge 1 al sflrcdnbr andiamo fuori limite max record
253000071121      * caricati e si schianta in emissione del sfl.
253100071121     c                   if        resto = 0
253200071121     c                   eval      C2RCD = c2rcd - (wrigsfl2 - 1)
253300071121     c                   else
253400071121     c                   if        c2rcd = 0
253500070503     c                   add       1             c2rcd
253600071121     c                   end
253700071121     c                   end
253800071121      ****
253900070301     C                   Else
254000070301     C     Wpag          IFgt      0
254100070301     C                   Z-ADD     wpag          C2RCD
254200070301     C                   EndIF
254300070301     C                   EndIF
254400070301     C                   ENDIF
254500070427     C                   End
254600070515     C                   End
254700100505      *
254800100505      *  spegne il Pro-memoria se premuto il tasto F02
254900100505     c                   eval      Premuto_F02 = '0'
255000070301      *
255100070301      *  Emissione SFL delle BOLLE
255200070301      * ?            *-----------------------*
255300070301     C                   EXFMT     FDG0C02
255400070301      * ?            *-----------------------*
255500070326     c                   setoff                                       99
255600070305     C                   movel     'S2'          sav$GEST
255700070301      *
255800070301     C     C2NRR         IFNE      0
255900070301     C                   Z-ADD     C2NRR         WSFL
256000070301     C                   ENDIF
256100070301     C                   Z-ADD     SFLNRR        C2RCD
256200070515     C                   Z-ADD     SFLNRR        C2RCD_pos
256300070306      ***
256400070306     c                   EndIF
256500070301     C* Selezioni
2566000703011    C                   SELECT
256700070301     C* F3=Fine
256800070301     C     $TASTO        WHENEQ    F03
256900070301     C                   EXSR      F03_exit
257000070319     c                   rolbk
257100070307      *
257200070307     C* F1=Seleziona Tutti
257300070307     C                   WHEN      $TASTO   =   F01
257400070307     C                   EXSR      F01_Tutti2
257500070307      *
257600070301     C* F5=Int.tutte le Distinte
257700070301     C     $TASTO        WHENEQ    F05
257800070301     C                   EXSR      F05_interr
257900070426      *
258000070427     C* F07= Ordinamento x Peso
258100070427     C     $TASTO        WHENEQ    F07
258200070427     c                   z-add     6             WrkSort
258300070515     c                   exsr      Clear_Ordina
258400070427     C                   EXSR      Ordina_SFL2
258500070301      *
258600070427     C* F08= Ordinamento x Volume
258700070427     C     $TASTO        WHENEQ    F08
258800070427     c                   z-add     5             WrkSort
258900070515     c                   exsr      Clear_Ordina
259000070427     C                   EXSR      Ordina_SFL2
259100070427      *
259200070427     C* F10= Ordinamento x Località
259300070427     C     $TASTO        WHENEQ    F10
259400070427     c                   z-add     4             WrkSort
259500070515     c                   exsr      Clear_Ordina
259600070427     C                   EXSR      Ordina_SFL2
259700070427      *
259800070427     C* F11= Ordinamento x Indirizzo
259900070427     C     $TASTO        WHENEQ    F11
260000070427     c                   z-add     3             WrkSort
260100070515     c                   exsr      Clear_Ordina
260200070427     C                   EXSR      Ordina_SFL2
260300070427      *
260400070427     C* F13= Ordinamento x Destinatario
260500070427     C     $TASTO        WHENEQ    F13
260600070427     c                   z-add     2             WrkSort
260700070515     c                   exsr      Clear_Ordina
260800070427     C                   EXSR      Ordina_SFL2
260900070427      *
261000070427     C* F14= Ordinamento x Giro/Destinatario
261100070427     C     $TASTO        WHENEQ    F14
261200070427     c                   z-add     1             WrkSort
261300070515     c                   exsr      Clear_Ordina
261400070427     C                   EXSR      Ordina_SFL2
261500070427      *
261600070301     C* F12=Ritorno
261700070301     C     $TASTO        WHENEQ    F12
261800070308     c                   exsr      INZ_indERR
261900070531     c                   if        d1sce = Assegnare
262000070305     c                   move      'S1'          $GEST
262100070305     c                   z-add     C1RCD         wsfl
262200070305     c                   else
262300070619     c*****              move      'D1'          $GEST
262400070619     C*****              MOVE      *ON           $INZD1
262500070619      *  se togliere o trasferire
262600080610      *   voglio riportare direttamente sull'interrogazione distinte
262700080610      *? ma se richiamato dal Monitor e si deve tornare alla scelta
262800080610      *? sulla distinta.
262900080610     c                   if        PGM_chiamante = 'MONITOR'
263000080610     c                              and d1sce <> Togliere
263100080610     c                   eval      d1sce = Togliere
263200080610     c                   end
263300080610      *
263400070619     c                   eval      salva_d1sce = d1sce
263500070619     c                   eval      salta_video1 = 'S'
263600070619     C                   movel     'D1'          $GEST
263700070619     C                   MOVE      *Off          $INZD1
263800070619     c                   exsr      INZD1
263900070619     c                   eval      d1sce = salva_d1sce
264000070619      *
264100070316     c                   rolbk
264200070305     c                   end
264300070301      *
2644000703011O   C                   OTHER
264500070301     C* CONTROLLO testata
264600070301     C                   EXSR      CTRC2
264700070301     C* CONTROLLO subfile
264800070301     C     *IN99         IFEQ      *OFF
264900070301     C                   EXSR      CTRS2
265000070301     C                   END
265100070301      *
265200070305     C* F4=Int.Distinte Stesso Giro
265300070305     C     *IN99         IFEQ      *OFF
265400070305     C     $TASTO        andeq     F04
265500070305     C                   if        giri_scelti = 1
265600070305     C                   EXSR      F04_interr
265700070305     c                   else
265800070328      * errore x + Giri con F4 (x un solo giro)
265900070305     c                   seton                                        51  99
266000070305     c                   end
266100070305     c                   endIF
266200070305      *
266300070301      * ?se NON premuto F9 e immessa una distinta,
266400070301      * ? emette la finestra dei totali Distinta
266500070328      * ?  se non ci sono stati errori precdentemente
266600070328      * ?   e se è stato ripulito il messaggi degli errori precedenti
266700070328      * ?  se il sfl è carico.
266800080609      * ?   e se non è stata eseguita nessuna opz"8" di chiudi INFO
266900070328     C     *IN99         IFEQ      *OFF
267000070328     c     ErrMsgZ       andEQ     *blank
267100070328     C     WMAX          andGT     0
267200101130     c     exec_opzINFO  andeq     *blank
267300070326      *
267400070319     c     h1ndc_piu     ifgt      0
267500070531     c     D1SCE         andeq     Assegnare
267600070305     c     totSPE_s2     andgt     0
267700070326      *
267800070531     c     D1SCE         oreq      Togliere
267900070306     c     totSPE_s2     andgt     0
268000070326      *
268100070531     c     D1SCE         oreq      Trasferire
268200070326     c     totSPE_s2     andgt     0
268300070326     c     h1ndc_piu     andgt     0
268400070305     C                   MOVE      *ON           $INZW1
268500070302     C                   movel     'W1'          $GEST
268600070302     c                   else
268700070302      * emette messaggio di Avviso x scegliere una Distinta se non c'è
268800070302      * se si era scelta l'opzione di assegnazione e non si era immessa
268900070302      * una distinta
269000070305     c                   if        totSPE_s2 = 0
269100070326     c                   movel     ErrMsg(19)    err001
269200070326     c                   movel     ErrMsg(20)    err002
269300070319     c                   clear                   err003
269400070305     c                   else
269500070326     c                   movel     ErrMsg(17)    err001
269600070319     c                   clear                   err002
269700070326     c                   movel     ErrMsg(18)    err003
269800070305     c                   end
269900070302      * ?            *-----------------------*
270000070305     C                   exfmt     FDG0WER
270100070302      * ?            *-----------------------*
270200070302     c                   end
270300070305      *
270400070305     c                   end
270500070301      *
2706000703011-   C                   ENDSL
270700070301     C*
270800070301     C                   ENDSR
270900070306     C/EJECT
271000070306      *---------------------------------------------------------------*
271100070320      *?   INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL               ?
271200070306      *---------------------------------------------------------------*
271300070306     C     INZZ2         BEGSR
271400070306     C*
271500070306     C* Messaggio o tasti funzionali
271600070306     c                   clear                   z2dmsg
271700070328     c                   clear                   ErrmsgZ          80
271800100505     c                   move      opz_errata    *in65
271900100505     c                   move      da_forzare    *in66
272000100505      *
272100100505      * Se si deve forzare cambia anche le opzioni del SFL in testata
272200100505     c                   setoff                                       18
272300100505     c                   if        da_forzare   = '1'  and
272400100505     c                             d1SCE = Assegnare
272500100505     c                   setoN                                        18
272600100505     C                   End
272700070307     C*
272800070306      * errore di selezione di + giri x richiesta interr.stesso giro
272900070306     c                   if        *in51 = *on
273000070328     c                   movel     ErrMsg(12)    ErrmsgZ
273100070306     c                   setoff                                       51
273200070306     c                   end
273300070319     c                   if        *in52 or *in54
273400070328     c                   movel     ErrMsg(13)    ErrmsgZ
273500070326     c                   setoff                                       5254
273600070306     c                   end
273700070326     c                   if        *in62
273800070328     c                   movel     ErrMsg(15)    ErrmsgZ
273900070326     c                   setoff                                       62
274000070319     c                   end
274100080609     c                   if        *in65
274200160317     c                   if        Sped_DPD = 'S'
274300160317     c                   movel     ErrMsg(36)    ErrmsgZ
274400160317     c                   else
274500080609     c                   movel     ErrMsg(33)    ErrmsgZ
274600160317     c                   end
274700080609     c                   setoff                                       65
274800100505     C                   move      '0'           opz_errata
274900080609     c                   end
275000130424     c                   if        *in66
275100150930     c                   if         in66_msg = 'A'
275200100505     c                   movel     ErrMsg(34)    ErrmsgZ
275300150930     c                   elseif     in66_msg = 'C'
275400150930     c                   movel     ErrMsg(35)    ErrmsgZ
275500150930     c                   end
275600100505     c                   end
275700070319     c                   if        *in53 or *in55
275800070328     c                   movel     ErrMsg(14)    ErrmsgZ
275900070319     c                   setoff                                       5355
276000070306     c                   end
276100070413     c                   if        *in59
276200070413     c                   movel     ErrMsg(29)    ErrmsgZ
276300070413     c                   setoff                                       59
276400070413     c                   clear                   h1ndc_piu
276500160321     c                   clear                   h1pdd_piu
276600070413     c                   end
276700160321     c                   if        *in58
276800160322     c                   movel     ErrMsg(38)    ErrmsgZ
276900160321     c                   setoff                                       58
277000160321     c                   clear                   h1ndc_piu
277100160321     c                   clear                   h1pdd_piu
277200160321     c                   end
277300141113     c                   if        o982esito ='1'
277400141113     c                   movel     o982msgerr    ErrmsgZ
277500141113     c                   end
277600070328      *
277700070328      * imposta eventuali errori
277800070328     c                   eval      z2dmsg = ErrMsgZ
277900070307     C*
278000070307      * tasti funzionali se non è stato emesso alcun messaggio
278100070328     c                   if        ErrMsgZ = *blank
278200070307     c                   setoff                                       99
278300070427     c                   end
278400070426      *
278500070307      * se è stato scelto l'assegnazione (1) e non è stato immessa la distinta
278600070531     c                   if        d1sce = Assegnare or d1sce = Trasferire
278700070427     c                   setoff                                       44
278800070426     c                   else
278900070427     c                   seton                                        44
279000070307     c                   end
279100070306     C*
279200070306     C                   ENDSR
279300070301     C/EJECT
279400070301      *---------------------------------------------------------------*
279500070320      *?  INIZIALIZZAZIONE LISTA Bolle da Giri o da Distinta         ?
279600070301      *---------------------------------------------------------------*
279700070301     C     INZS2         BEGSR
279800070301     C*
279900070301     C* reset indicatori DSPATR
280000070308     c                   exsr      INZ_indERR
280100100505     C                   move      '0'           opz_errata        1
280200100505     C                   move      '0'           da_forzare        1
280300070301     C* pulizia SFL
280400070301     C                   SETOFF                                         3031
280500070301     C                   WRITE     FDG0C02
280600070301     C                   SETON                                          31
280700070301     C*
280800070301     C* Righe x Pagina del 1° SFL
280900070427     c                   z-add     RigPag_SFl2   s2pag
281000070420      *
281100070427     C                   EVAL      WrkSort  = Ord_GIRODEST
281200070427     c                   eval      wINDsort = Ord_GIRODEST
281300070301      *
281400070301     C* CARICAMENTO SFL totale
281500070301     C                   Z-ADD     0             S2NRR
281600070301     C                   Z-ADD     1             C2RCD
281700070301     C                   Z-ADD     0             WMAX
281800070302     C                   Z-ADD     0             Wsfl
281900070302     C* campi titolo
282000070302     C                   MOVEL     dsPGM         c2CPGM
282100070302     C                   MOVEL     knsif         c2nsif
282200070302     C                   MOVEL     knmus         c2nmus
282300070302     C                   MOVEL     RSUT          c2crsu
282400070427     C                   clear                   c2pos1
282500070427     C                   clear                   c2pos2
282600070427     C                   clear                   c2pos3
282700070427     C                   clear                   c2pos4
282800070427     C                   clear                   c2pos5
282900070427     C                   clear                   c2pos6
283000070427     C                   clear                   sav_pos1
283100070427     C                   clear                   sav_pos2
283200070427     C                   clear                   sav_pos3
283300070427     C                   clear                   sav_pos4
283400070427     C                   clear                   sav_pos5
283500070427     C                   clear                   sav_pos6
283600070302      * TIPO FUNZIONE
283700070302     C                   SELECT
283800070531     C                   when      d1sce = Assegnare
283900070302     c                   eval      C2dsce = OPZ_1
284000070306     c                   eval      C2dess = 'alla Distinta nr.' +
284100070319     c                             %trim(%editc(h1ndc_piu:'Z'))
284200070306      *
284300070531     C                   when      d1sce = Togliere
284400070302     c                   eval      C2dsce = OPZ_2
284500070306     c                   eval      C2dess = 'dalla Distinta nr.' +
284600070319     c                             %trim(%editc(h1ndc_meno:'Z'))
284700070306      *
284800070531     C                   when      d1sce = Trasferire
284900070306     c                   eval      C2dsce = OPZ_3
285000070306     c                   eval      C2dess = 'dalla Distinta nr.' +
285100070319     c                             %trim(%editc(h1ndc_meno:'Z'))
285200070306      *
285300070302     c                   endSL
285400070301      *
285500070301      *   Carica il SFL
285600070301     c                   EXSR      ROLS2
285700070301     C*
285800070301     C                   Z-ADD     1             WPAG
285900070301     C*
286000070301     C                   ENDSR
286100070301      *---------------------------------------------------------------*
286200070320      *?  CARICAMENTO PAGINA da SFL dei GIRI                         ?
286300070301      *---------------------------------------------------------------*
286400070301     C     ROLS2         BEGSR
286500070301     C*
286600070301     C                   SETOFF                                       32
286700070301     C                   Z-ADD     0             Y
286800070301     C                   Z-ADD     WMAX          S2NRR
286900070301     C*
287000070301      * ? Se Scelto Caricamento x Assegnazione
287100070531     c                   if        d1sce = Assegnare
287200070301      * ?      carica da SFL1
287300070301     c                   exsr      CAR_da_SFL1
287400070301     c                   else
287500070301      * ?      carica da Numero Distinta
287600070301     c                   exsr      CAR_da_NDC
287700070301     c                   end
287800070301     C* Fine Caricamento
287900070301     C                   Z-ADD     S2NRR         WMAX                 30
288000070419     C*
288100070419     C* se ci sono almeno 2 records da ordinare fra loro
288200070419     c                   if        wmax > 1
288300070419      * ?   Esegue l'Ordinamento x Giro/Spedizione
288400070515     c                   exsr      Clear_Ordina
288500070419     c                   exsr      Ordina_SFL2
288600070419     c                   end
288700070301     C*
288800070302     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
288900070301     C     S2NRR         DIV       S2pag         PAGINE            4 0
289000070301     C                   MVR                     RESTO             3 0
289100070301     C     PAGINE        MULT      S2pag         C2RCD
2892000703011    C     RESTO         IFGT      0
289300070301     C                   ADD       1             C2RCD
2894000703011E   C                   ELSE
289500070301     C                   SUB       S2pag         C2RCD
289600070301     C                   ADD       1             C2RCD
2897000703011-   C                   ENDIF
289800070301     C*
289900070301     C*
290000070301     C                   ENDSR
290100070301      *---------------------------------------------------------------*
290200070316      *  ?  Carica dal SFL dei Giri                                  ?
290300070301      *---------------------------------------------------------------*
290400070301     C     CAR_da_SFL1   BEGSR
290500070301     C*
290600070301     C*   Legge il SFL dei Giri dove selezionato con (1) le righe
290700070301     C*    dei giri
290800070302     c                   Readc     FDG0S01                                21
2909000703022    C     *IN21         DOWEQ     *OFF
291000070301     C*
291100070301     C* Solo dai Giri selezionati
291200070301     c                   if        s1SCE = '1'
291300070404      *
291400070404      * ? Se ASSEGNA: caricamento da SFL dei giri
291500070404      * ? ----------------------------
291600070404     c     k52j1         setll     fiarg52j
291700070302      * ? ----------------------------
291800070301      * ? Carico il SFL
291900070301      * ?     leggendo le bolle valide
292000070301     c                   exsr      LEGGE_Bolle
292100070301     C*
292200070301     C* Leggo dal file per caricare la lista
2923000703011    C     $EFILE        DOWEQ     *OFF
292400070301     C*
292500070301     C                   clear                   FDG0S02
292600070301     C* codice Giro
292700070404     c                   movel     argCGI        s2CGI
292800070301      *
292900070301     c                   exsr      RIGA_SFL2
293000070306      *
293100070306      * Se si è scelto di non visualizzare le Bolle comunque le carico
293200070306      *  tutte selezionate con SFLNXTCHG x la routine di aggiornamento
293300070306      *   finale.
293400070607     c                   if        $VISS2 = *Off
293500070607     c                             or s2CGI = fdgCGI and Sel_xGiro = 'S' and
293600070607     c                             (fdgopz=Trasferire or fdgopz=Togliere)
293700070306     c                   move      '1'           s2sce
293800070306     c                   seton                                        32
293900070306     c                   end
294000070305      *
294100070301     C                   ADD       1             S2NRR
294200070503     C                   EVAL      RrnLast = S2nrr
294300070301     C                   ADD       1             Y
294400101130     c                   eval      *in45 = H2in45
294500130221     c     h2xco         comp      '0'                                88
294600130424     c                   eval      h2att = in_attesa
294700070301     C                   WRITE     FDG0S02
294800070301     C*
294900070301      * Lettura di un record
295000070301     c                   exsr      LEGGE_Bolle
2951000703011-   C                   ENDDO
295200070301     C*
295300070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL dei GIRI
295400070301     C                   MOVE      *ON           *IN32
295500070301     C                   UPDATE    FDG0S01
295600070302     C*
295700070301     c                   End
295800070302      * ?CHIUSURE FILE
295900070302     c                   exsr      CLOF_S2
296000070302     C*
296100070302     c                   Readc     FDG0S01                                21
296200070301     c                   EndDo
296300070301     C*
296400070301     C                   ENDSR
296500070306      *---------------------------------------------------------------*
296600070316      *  ?  Carica da Numero Distinta                                ?
296700070306      *---------------------------------------------------------------*
296800070306     C     CAR_da_NDC    BEGSR
296900070306     C*
297000070306      * ? ----------------------------
297100080826      * ? Se TOGLIE o TRASFERISCE:  caricamento da Distinta
297200070531     c                   IF        d1sce = Togliere or d1sce = Trasferire
297300070404      *
297400070306     c* se il p.o. è in parallelo testo la distinta su ARG altrimenti
297500070306     c* se il p.o. è partito testo la distinta su ARB e ARG
297600070306     c                   if        test_Parallelo = 'S'
297700070306     C/EXEC SQL
297800070306     C+ DECLARE I1 CURSOR FOR SELECT fiarg02l.*, fnarb01l.* FROM fiarg02l
297900070306     C+ JOIN fnarb01l ON arbaas= argaas and arblnp=arglnp and arbnrs=
298000070306     C+ argnrs and arbnsp= argnsp WHERE argpoc = :d1cFGS and
298100070319     C+ argNDC=:h1ndc_meno
298200070306     C/END-EXEC
298300070306
298400070306     C/EXEC SQL
298500070306     C+ OPEN I1
298600070306     C/END-EXEC
298700070306     c                   else
298800070404      **
298900070404     c                   z-add     wfgs          wfgs_ifp
299000070404      * ? ----------------------------
299100070404     c     karb70        setll     fnarb70l
299200070404      * ? ----------------------------
299300070306     c                   end
299400070306     c                   end
299500070306      * ? ----------------------------
299600070306      * ? Carico il SFL
299700070306      * ?     leggendo le bolle valide
299800070306     c                   exsr      LEGGE_Bolle
299900070306     C*
300000070306     C* Leggo dal file per caricare la lista
3001000703061    C     $EFILE        DOWEQ     *OFF
300200070306     C*
300300070306     C                   clear                   FDG0S02
300400070306     C* codice Giro
300500070404     c                   movel     argCGI        s2CGI
300600070306      *
300700070306     c                   exsr      RIGA_SFL2
300800070607      *
300900070607     c                   if        s2CGI = fdgCGI and Sel_xGiro = 'S' and
301000071024     C                             s2CGI <> *blank and
301100070607     c                             (fdgopz=Trasferire or fdgopz=Togliere)
301200070607     c                   move      '1'           s2sce
301300070607     c                   seton                                        32
301400070607     c                   end
301500070306      *
301600070306     C                   ADD       1             S2NRR
301700070503     C                   EVAL      RrnLast = S2nrr
301800070306     C                   ADD       1             Y
301900101130     c                   eval      *in45 = H2in45
302000130221     c     h2xco         comp      '0'                                88
302100130424     c                   eval      h2att = in_attesa
302200070306     C                   WRITE     FDG0S02
302300070306     C*
302400070306      * Lettura di un record
302500070306     c                   exsr      LEGGE_Bolle
3026000703061-   C                   ENDDO
302700070306      * ?CHIUSURE FILE
302800070306     c                   exsr      CLOF_S2
302900070306     C*
303000070306     C                   ENDSR
303100070302      *---------------------------------------------------------------*
303200070320      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
303300070302      *---------------------------------------------------------------*
303400070302     C     LEGGE_Bolle   BEGSR
303500070302     C*
303600070302     C*  Per EoF deve uscire
303700070302     C                   MOVEL     *OFF          $EFILE
303800070302     C*  Per Record Ok deve uscire riportando i dati
303900070302     C                   MOVEL     *OFF          $RCDOK
304000070302     C*
3041000703021    C     $EFILE        DOUEQ     *ON
304200070302     C     $RCDOK        OREQ      *ON
304300070302     C*
304400070306      *  x Assegnazione
304500070531     c                   if        d1sce = Assegnare
304600070404      * ?LETTURE  FILE
304700070404      * ? ----------------------------
304800070404     c     k52j1         reade     fiarg52j
304900070404     C                   eval      sqlcod = 0
305000070404     c                   if        %Eof(fiarg52j)
305100070404     C                   eval      sqlcod = 100
305200070404     c                   end
305300070404      * ? ----------------------------
305400070306     c                   end
305500070306     C*
305600070404      *  x Togliere  o  Trasferire
305700070531     c                   if        d1sce = Togliere or d1sce = Trasferire
305800070306     c                   if        test_Parallelo = 'S'
305900070306      * ?LETTURE  FILE
306000070306     C/EXEC SQL
306100070306     C+ FETCH NEXT FROM i1 INTO  :fiargds, :fnarbds
306200070306     C/END-EXEC
306300070306     c                   else
306400070404      **
306500070404     c     karb70        reade     fnarb70l
306600070404      * Fine File
306700070404     C                   eval      sqlcod = 0
306800070404     c                   if        %Eof(fnarb70l)
306900070404     C                   eval      sqlcod = 100
307000070417     c                   else
307100070417     c     karg          chain(n)  fiarg01l
307200070417     c                   if        not %Found(fiarg01l)
307300070417     c                   clear                   fiargds
307400070417     c                   end
307500070417     c                   end
307600070404      **
307700070306     c                   end
307800070306     c                   end
307900070306      *
308000070302     C                   select
308100070302      * Fine File
308200070404     C                   when      sqlcod = 100
308300070302     C                   MOVEL     *ON           $EFILE
308400070302     C                   MOVE      $EFILE        *IN33
308500070302      * errori
308600070302     C                   WHEN      SqlCod < 0
308700070302     c                   if        sqlcod <> -913
308800070302     C                   seton                                        H1
308900070302     c                   end
309000070302      *
309100070302     C                   other
309200070302      *
309300070306      *  Per assegnare le bolle deve eseguire il controllo di consegnabilità
309400070531     c                   if        d1sce = Assegnare
309500070404      *
309600070404      *   Leggendo x ARG52J deve scartare le NDC > 0
309700070419     c                   if        (test_Parallelo = 'S' and argndc = 0) or
309800070419     c                             (test_Parallelo <>'S' and arbndc = 0)
309900070404      *
310000070302      * ?Controlla se la Spedizione è da considerare
310100070302     C                   exsr      Check_Sped
310200070302      * ?     se Non è Esclusa dalla consegna
310300070302     c                   if        d16conso <> 'N'
310400070302     C                   MOVE      *ON           $RCDOK
310500070302     c                   end
310600070404      *
310700070404     c                   end
310800070302      *
310900070306      * Per Togliere o Trasfererire
311000070306     c                   else
311100070306     C                   MOVE      *ON           $RCDOK
311200070306     c                   end
311300070306      *
311400070302     C                   endsl
311500100505     C*------
311600100505     C* Solo in Assegnazione:
311700100505     C*  Se non devono essere visualizzate le bolle con particolarità
311800100505     C*   di Attesa  le Filtro
311900130424     c                   clear                   in_attesa
312000130424     c                   if        d1sce = Assegnare and
312100130424     c                             D1pATT = 'N'
312200100505     c                   exsr      ChK_attesa
312300100505     c                   if           in_attesa = 'S'
312400100505     C                   MOVE      *Off          $RCDOK
312500100505     C                   END
312600100505     c                   end
312700100505      *
312800100505     C*------
3129000703021-   C                   ENDDO
313000070302     C*
313100070302     C                   ENDSR
313200100505      *---------------------------------------------------------------*
313300100505      *?  Controlla se la Bolla ha Partic.di Consegna <in Attesa>    ?
313400100505      *---------------------------------------------------------------*
313500100505     C     Chk_attesa    BEGSR
313600100505     c                   if         arbGMA <> *blank and arbTCR = 'P'
313700100505     C*
313800100505     C     ARBdcr        IFGT      DATEU8
313900130429     c                   clear                   tnsd99ds
314000130424     C                   Z-ADD     dateu8        D98DFC
314100130424     C                   Z-ADD     2359          D98OFC
314200130424     C                   MOVEL     ' '           D98TLA
314300130424     C                   MOVEL     'A'           D98TBO
314400130424     C                   Z-ADD     ARBAAS        D98AAS
314500130424     C                   Z-ADD     ARBLNP        D98LNP
314600130424     C                   Z-ADD     ARBNRS        D98NRS
314700130424     C                   Z-ADD     ARBNSP        D98NSP
314800130424     C                   CALL      'TNSD99R'
314900130424     C                   PARM                    TNSD99DS
315000130424     c                   if        d98nci <=-24
315100100505     C     arbgma        lookup    s7r                                    23
315200100505     C                   IF        *in23
315300100505     C                   eval      in_attesa = 'S'
315400100505     C                   END
315500100505     C                   END
315600100505     C                   END
315700100505     C*
315800100505     C                   End
315900100505     C*
316000100505     C                   ENDSR
316100070301      *---------------------------------------------------------------*
316200070320      *?  Imposta Campi del 2°SFL                                    ?
316300070301      *---------------------------------------------------------------*
316400070301     C     RIGA_SFL2     BEGSR
316500070301     C*
316600070301     C* Salva il record di ARB per riposizionamento seguente
316700070301     c                   movel     fnarbds       fnarbdsav
316800070301     C* Dati Bolla
316900070301      *  Salva la chiave anche per riposizionarsi in seguito
317000070301     c                   z-add     ARBaas        S2Caas
317100070301     c                   z-add     ARBlnp        S2Clnp
317200070301     c                   z-add     ARBnrs        S2Cnrs
317300070301     c                   z-add     ARBnsp        S2Cnsp
317400101129     c                   z-add     ARBmgs        h2mgs
317500110804     c                   movel     ARBxco        h2xco
317600150930      *
317700150930      * Controlla
317800150930     c                   exsr      contro_alert
317900150930     c                   eval       h2Calert = inviato_contro_alert
318000150930      *
318100100505      * Se è una di quelle bolle che devono rimanere in attesa
318200130424      *   va segnalato sul sfl con "A" di attesa                            e.
318300100505     c                   clear                   s2pATT
318400100505     c                   if         arbGMA <> *blank and arbTCR = 'P'
318500100505     c                   z-add     arbDCR        h2dcr
318600100505     c                   move      arbGMA        h2gma
318700100505     c                   move      'A'           s2pATT
318800100505     c                   end
318900070301      *
319000070301     c                   movel     ARBcbo        S2Ccbo
319100070301     c                   movel     ARBrsd        S2Crsd
319200070301     c                   movel     ARBind        S2Cind
319300070301     c                   z-add     ARBlna        S2Clna
319400070301     c                   z-add     ARBznc        S2Cznc
319500070301     c                   movel     ARBgc1        S2Cgc1
319600070301     c                   movel     ARBgc2        S2Cgc2
319700070301     c                   movel     ARBcad        S2Ccad
319800070301     c                   movel     ARBlod        S2Clod
319900070301     c                   movel     ARBprd        S2Cprd
320000070301     c                   movel     ARBrsm        S2Crsm
320100070301     c                   movel     ARBtsp        S2Ctsp
320200070301     c                   z-add     ARBhcr        S2Chcr
320300070302      *  Pesi Volumi Colli da consegnare
320400070305     c                   exsr      Cal_PesVol
320500070302      *   i pesi/volumi/Colli Reali da consegnare
320600070302     c                   z-add     D27PKLDCO     S2Cpkf
320700070502     c                   move      D27PKLDCO     Decimal_uno       1 0
320800070502     c                   if        Decimal_uno > 0
320900070502     c                   add       1             S2Cpkf
321000070502     c                   end
321100070427     c                   z-add     D27PKLDCO     S2pkf
321200070302     c                   z-add     D27VLUDCO     S2Cvlf
321300070302     c                   z-add     D27NCLDCO     S2Cncl
321400070301      *
321500070301     C                   move      ARBdcr        MMGG              4 0
321600070301     C                   MOVEL     MMGG          MM                2 0
321700070301     C                   MOVE      MMGG          GG                2 0
321800070301     C                   MOVE      MM            S2Cgmc
321900070301     C                   MOVEL     GG            S2Cgmc
322000070301      * Anomalia
322100150515     c                   clear                   s2dano
322200070301     C     ARBfan        IFEQ      'S'
322300070301     C                   MOVEL     'ANOM.'       S2Dano
322400070301     C                   END
322500150515     c* se filiale gestisce richiesta pistola 88
322600150515     c                   if        V1Cp88 ='S'
322700150515     C     ARBfan        IFEQ      'S'
322800150515     C     d27ncl88o     andne     0
322900150515     C                   MOVEL     'AN/88'       S2Dano
323000150515     C                   END
323100150515     C     ARBfan        IFne      'S'
323200150515     C     d27ncl88o     andne     0
323300150515     C                   MOVEL     'Pt.88'       S2Dano
323400150515     C                   END
323500150515     C                   END
323600070301      * Tipo Consegna
323700070301     C                   Z-ADD     1             XY                3 0
323800070301     C     ARBtc1        LOOKUP    C1P(XY)                                23
323900070301     C     *IN23         IFEQ      '1'
324000070301     C                   MOVEL     D1P(XY)       S2Dtc1
324100070301     C                   END
324200070301     C                   Z-ADD     1             XY
324300070301     C     ARBtc2        LOOKUP    C1P(XY)                                23
324400070301     C     *IN23         IFEQ      '1'
324500070301     C                   MOVEL     D1P(XY)       S2Dtc2
324600070301     C                   END
324700070301     C*
324800070301     C*  Controllo il tipo bolla
324900070301     C                   Z-ADD     1             X
325000070301     C                   MOVEL     *BLANKS       WTBOL1            1
325100070302     C                   MOVEL     *BLANKS       WTBOL2            1
325200070301     C                   CLEAR                   DS3A
325300070301     C     arbCBO        LOOKUP    C3A(X)                                 13
325400070301     C     *IN13         IFEQ      '1'
325500070301     C                   MOVEL     D3A(X)        DS3A
325600070301     C                   MOVEL     §3ATB1        WTBOL1
325700070301     C                   MOVEL     §3ATB2        WTBOL2
325800070301     C                   END
325900070301     C*
326000070301     C*  Campi di comodo per Bolla legata precedente:
326100070301     C                   clear                   comASS           13 3
326200070301     C                   clear                   comVCA            3
326300070301     C                   clear                   comTIC            2
326400070301     C*
326500070301      *?Controlla se esiste una bolla precedente legata ?
326600070301     C     Karb1         Chain     FnLBL01L
326700070301      *
326800070301      * > Se ho una bolla "mamma" ed è locale
326900070301     C                   if        %Found(FnLBL01L) and  arbLNA = lblLAP
327000070301      *
327100070301      * prende la bolla precedente sporcando i campi di ARB
327200070301     C     Klbl          CHAIN(n)  FnARB01L
327300070301      *
327400070301     C*  La prende in considerazione solo se non è
327500070301     C*   un Reso Mittente e un'Avaria e non è un solo incasso
327600070301     C                   IF        %Found(FnARB01L) and
327700070301     C                             arbCCA <> '2' and arbCCA <> '6' and
327800070301     C                             (arbICC = ' ' or arbICC = 'R')
327900070301      *  ?Controllo il tipo bolla mamma?
328000070301     C                   Z-ADD     1             X
328100070301     C     arbCBO        LOOKUP    C3A(X)                                 23
328200070301     C     *IN23         IFEQ      '1'
328300070301     C                   MOVEL     D3A(X)        DS3A
328400070301     C*  Se bolla mamma con C/Assegno vado a reperirlo
328500070301     C     §3AFCA        IFNE      0
328600070301     C     Klbl          Chain     FIAR901L                           56
328700070301     C                   If        %Found(FIAR901L)
328800070301     C                   Z-ADD     ar9CAS        comASS
328900070301     C                   MOVEL     ar9VCA        comVCA
329000070301     C                   MOVEL     ar9TIC        comTIC
329100070301     C                   End
329200070301     C                   END
329300070301     C                   END
329400070301     C                   END
329500070301     C*
329600070302      *  ? Reimposta i campi di ARB dell'SQL ?
329700070301     c                   movel     fnarbdsav     fnarbds
329800070301     C                   END
329900070301     C*
330000070302     c                   if        arbICC = ' ' or arbICC = 'R'
330100070302     C     §3AFCA        IfNE      0
330200070302     C                   Z-ADD     0             ar9CAS
330300070302     C                   MOVEL     *BLANKS       ar9VCA
330400070302     C     Karb1         CHAIN     FiAR901L
330500070302     c                   iF        %Found(FiAR901L)
330600070302     C                   Z-ADD     ar9CAS        s2cCAS
330700070302     C                   MOVEL     ar9VCA        s2cVCA
330800070302     C                   MOVE      ar9TIC        s2cTIC
330900070302     C                   EnD
331000070302     C                   End
331100070302      *
331200070302     C     comASS        IFNE      *ZEROS
331300070302     C                   Z-ADD     comASS        s2cCAS
331400070302     C                   MOVEL     comVCA        s2cVCA
331500070302     C                   MOVE      comTIC        s2cTIC
331600070302     C                   EnD
331700070302     C                   End
331800070302     C*
331900070301     C* Verifico se devo gestire un assegnato
332000070301     C     WTbol1        IFNE      *BLANKS
332100070301     C     WTbol2        ORNE      *BLANKS
332200070302      *
332300070301     C* Se devo gestire 2° bolla imposto tipo record '2' x posiz.FIAR6
332400070301     C     WTbol2        IFNE      *BLANKS
332500070301     C                   MOVE      '2'           KTRC
332600070301     C* .... se devo gestire 1° bolla imposto tipo record '1'
332700070301     C                   ELSE
332800070301     C     WTbol1        IFNE      'F'
332900070301     C                   MOVE      '1'           KTRC
333000070301     C                   END
333100070301     C                   END
333200070301      *
333300070301     C* Aggancio FIAR6 x importo assegnato
333400070301     C     KAR6          CHAIN     FIAR601L
333500070302     c                   if        %Found(FIAR601L) and ar6DFT <> 0
333600070301      *
333700070302     C* Se ho effettuato un ritorno all'incasso controllo se
333800070302     C     arbICA        IFEQ      'R'
333900070302     C* ... si riferisce ad un incasso parziale
334000070302     C                   CLEAR                   ariIPP
334100070302     C                   MOVEL     ar6DIV        s2cVAS
334200070302     C                   Z-ADD     ar6IFT        s2cIMA
334300070301      *
334400070302     C                   movel     'A'           KTIP
334500070302     C     Kari          setll     FiARI01L
334600070302     C     Kari          reade     FiARI01L
334700070302     C* ... se si calcolo importo da incassare x differenza
334800070302     C                   DOW       not %EoF(FiARI01L)
334900070302      *
335000070302     C     ar6DIV        ifEQ      ariDIV
335100070302     C                   SUB       ariIPP        s2cIMA
335200070302     C                   ElsE
335300070302     C* Se la divisa di incasso è diversa converto importo già
335400070302     C* incassato nella divisa della bolla
335500070302     C                   CLEAR                   YEURDS
335600070302     C                   MOVEL     ariDIV        YECDVI
335700070302     C                   Z-ADD     ariIPP        YECIMI
335800070302     C                   MOVEL     ar6DIV        YECDVO
335900070302     C                   MOVEL     'H'           YECTAR
336000070302     C                   CALL      'YEURCO'
336100070302     C                   PARM                    YEURDS
336200070302     C                   SUB       YECIMO        s2cIMA
336300070302     C                   End
336400070302      *
336500070302     C     Kari          reade     FiARI01L
336600070302     C                   END
336700070302      *
336800070302     C                   ELSE
336900070302      *
337000070302     C* ... altrimenti imposto totale fattura
337100070302     C     arbICA        ifEQ      ' '
337200070302     C     arbICA        orEQ      'R'
337300070302     C                   Z-ADD     ar6IFT        s2cIMA
337400070302     C                   MOVEL     ar6DIV        s2cVAS
337500070302     C                   End
337600070302      *
337700070302     C                   END
337800070302     C                   END
337900070301      *
338000070302     C                   ENDif
338100070301     C*
338200070302     C* Accendo indicatori x assegnato (ND)
338300070427     C                   if        s2cIMA > 0 or s2cCAS > 0
338400070427     c                   move      'S'           s2cIMP
338500070427     c                   end
338600070301     C*
338700080826      * ? Solo Se TOGLIE o TRASFERISCE:
338800080826     c                   IF        d1sce = Togliere or d1sce = Trasferire
338900080606      * ?Solo se è attiva la gestione del PDA o in test sulla filiale
339000080606     c                   if          PDAconsegne <> *blank
339100080627     C                   exsr      chk_PCT_COK
339200080826     c                   end
339300080606     c                   end
339400080606     C*
339500070301     C                   ENDSR
339600070307      *---------------------------------------------------------------*
339700080606      *  ?  Controlla se presenti delle INFO x PDA da visualizzare   ?
339800080606      *---------------------------------------------------------------*
339900080627     C     chk_PCT_COK   begSR
340000080606      *
340100080627      *  chiama la funzione passando la Bolla
340200101130     c                   eval      h2in45 = '0'
340300080606     c                   clear                   s2cCOK
340400080627     c                   clear                   fidg45ds
340500080627     c                   z-add     s2caas        C45AAS
340600080627     c                   z-add     s2clnp        C45LNP
340700080627     c                   z-add     s2cnrs        C45NRS
340800080627     c                   z-add     s2cnsp        C45NSP
340900080818     c                   z-add     h1pdr_meno    c45codaut
341000080903     c                   z-add     D1CFGS        c45FGS
341100080903     c                   z-add     h1ndc_meno    c45NDC
341200080627     c                   eval      kpjbus = kpjbu
341300080627     c                   eval      kpjbu  = fidg45ds
341400080627     c                   call      'FIDG45R'
341500080627     c                   parm                    kpjba
341600080627      ** mancava questa specifica che reimpostava la DS x caricare i 4 campi
341700080627     c                   eval      fidg45ds = kpjbu
341800080627     c                   eval      kpjbu = kpjbus
341900101126     c                   if        c45err  = ' ' and c45daVISIO <>' '
342000101130     c*****              move      'S'           s2cCOK
342100101130     c                   move      c45davisio    s2cCOK
342200101126     c                   eval      h2notass = c45notASS
342300101130     c                   if        h2notass <> *blank
342400101130     c                   eval      h2in45 = '1'
342500101130     c                   end
342600080627     c                   else
342700080627      * ?  Problemi durante la chiamata
342800080627     c                   end
342900080606      *
343000080606     C                   endSR
343100080627      *---------------------------------------------------------------*
343200080627      *  ?  Aggiorna come visionato la INFO x PDA da visualizzare   ?
343300080627      *---------------------------------------------------------------*
343400080627     C     agg_PCT_COK   begSR
343500080627      *
343600080627      *  chiama la funzione passando la Bolla
343700101130     c                   eval      h2in45 = '0'
343800080627     c                   clear                   fidg45ds
343900080627     c                   z-add     s2caas        C45AAS
344000080627     c                   z-add     s2clnp        C45LNP
344100080627     c                   z-add     s2cnrs        C45NRS
344200080627     c                   z-add     s2cnsp        C45NSP
344300080818     c                   z-add     h1pdr_meno    c45codaut
344400080903     c                   z-add     D1CFGS        c45FGS
344500080903     c                   z-add     h1ndc_meno    c45NDC
344600080627     c                   movel     'S'           C45aggVIS
344700080627     c                   eval      kpjbus = kpjbu
344800080627     c                   eval      kpjbu  = fidg45ds
344900080627     c                   call      'FIDG45R'
345000080627     c                   parm                    kpjba
345100080627     c                   eval      fidg45ds = kpjbu
345200080627     c                   eval      kpjbu = kpjbus
345300101130     c                   if        c45err  = ' ' and c45daVISIO <>' ' and
345400101130     c                             h2notass <> *blank
345500101130     c                   eval      h2in45 = '1'
345600101130     c                   end
345700080627     c                   if        c45err  = ' '
345800080627     c                   clear                   s2cCOK
345900080627     c                   else
346000101130     c********           move      'S'           s2cCOK
346100101130     c                   move      c45davisio    s2cCOK
346200080627      * ?  Problemi durante la chiamata
346300080627     c                   end
346400080627      *
346500080627     C                   endSR
346600080606      *---------------------------------------------------------------*
346700070316      *  ?  Seleziona Tutti i record del SFL                         ?
346800070307      *---------------------------------------------------------------*
346900070307     C     F01_Tutti2    BEGSR
347000070307     C*
347100070314     C     WMAX          IFgt      0
347200070307     C*  Seleziona tutte le righe del SFL accendendo il SFLNXTCHG
347300070307     c                   z-add     1             nrr_s2
347400070307     c     nrr_s2        chain     FDG0S02                            21
347500070307     c                   dow       *in21 = *Off
347600070307     c                   move      '1'           s2sce
347700070307     C*
347800070307      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
347900100505     c                   setoff                                       6665
348000070307     C                   MOVE      *ON           *IN32
348100101130     c                   eval      *in45 = H2in45
348200130221     c     h2xco         comp      '0'                                88
348300070307     C                   UPDATE    FDG0S02
348400070307     C*
348500070307     c                   add       1             nrr_s2
348600070307     c     nrr_s2        chain     FDG0S02                            21
348700070307     c                   endDO
348800070314     c                   end
348900070307     C*
349000070307     C                   ENDSR
349100070301      *---------------------------------------------------------------*
349200070320      *?  CONTROLLO TESTATA LISTA                                    ?
349300070301      *---------------------------------------------------------------*
349400070301     C     CTRC2         BEGSR
349500070301     C                   MOVE      *OFF          *IN99
349600070413      *
349700070413      * Se si tratta di Trasferimento
349800070413      *  non deve essere la stessa distinta da cui trasferisce
349900070531    3C     d1sce         ifEQ      Trasferire
350000070413    3C     h1ndc_piu     andEQ     h1ndc_meno
350100070413     C                   SETON                                        59  99
350200070413     c                   endIF
350300070427      *
350400070427      * ? Controllo campi di posizionamento
350500070514      * ?  o di ricerca stringa (?) in primo Byte
350600070427     c                   eval      posizionamento = 'N'
350700070515     c                   eval      posizionato    = 'N'
350800070511     c                   eval      cerca_stringa  = 'N'
350900070514     c                   clear                   Stringa_20A      20
351000070511      *
351100070427     c                   select
351200070511      * giro
351300070427     c                   when      *in01 and sav_pos1 <> c2pos1
351400070427     c                   eval      sav_pos1 = c2pos1
351500070427     c                   eval      posizionamento = 'S'
351600070511      * Descrizione RagSoc
351700070427     c                   when      *in02 and sav_pos2 <> c2pos2
351800070514     c                   eval      sav_pos2 = c2pos2
351900070515      *  è una ricerca x stringa
352000070511     c                   eval      cerca_stringa  = 'S'
352100070515     c                   eval      Stringa_20A = %subst(c2pos2:1)
352200070515
352300070511      * Indirizzo
352400070427     c                   when      *in03 and sav_pos3 <> c2pos3
352500070514     c                   eval      sav_pos3 = c2pos3
352600070515      *  è una ricerca x stringa
352700070511     c                   eval      cerca_stringa  = 'S'
352800070515     c                   eval      Stringa_20A = %subst(c2pos3:1)
352900070511      * Località
353000070427     c                   when      *in04 and sav_pos4 <> c2pos4
353100070514     c                   eval      sav_pos4 = c2pos4
353200070515      *  è una ricerca x stringa
353300070511     c                   eval      cerca_stringa  = 'S'
353400070515     c                   eval      Stringa_20A = %subst(c2pos4:1)
353500070511      * Volume
353600070427     c                   when      *in05 and sav_pos5 <> c2pos5
353700070427     c                   eval      sav_pos5 = c2pos5
353800070427     c                   eval      posizionamento = 'S'
353900070511      * Peso
354000070427     c                   when      *in06 and sav_pos6 <> c2pos6
354100070427     c                   eval      sav_pos6 = c2pos6
354200070427     c                   eval      posizionamento = 'S'
354300070515      *
354400070515      * Se precedentemente si era posizionato
354500070515     c                   when      c2pos1 <> *blank  or
354600070515     c                             c2pos2 <> *blank  or
354700070515     c                             c2pos3 <> *blank  or
354800070515     c                             c2pos4 <> *blank  or
354900070515     c                             c2pos5 <> *zero   or
355000070515     c                             c2pos6 <> *zero
355100070515     c                   eval      posizionato = 'S'
355200070427      *
355300070427     C                   Endsl
355400070514      *
355500070514      * ? Se si deve posizionare sul SFL on *TOP
355600070514     c                   if        posizionamento = 'S'
355700070514     C                   Exsr      Da_Posizione
355800070514     c                   end
355900070514      *
356000070514      * ? Se si deve cercare una stringa con (*)
356100070514     c                   if        cerca_stringa  = 'S'
356200070514     C                   Exsr      Cerca_Parola
356300070514     c                   end
356400070514      *
356500070301     C                   ENDSR
356600070301      *---------------------------------------------------------------*
356700070514      *?  Imposta il posizionamento al record TOP del SFL2           ?
356800070301      *---------------------------------------------------------------*
356900070514     C     Da_Posizione  BEGSR
357000070514      *
357100070514      * ? Se si deve posizionare sul SFL on *TOP
357200070514     c                   seton                                        99
357300070514     c                   eval      C2RCD =  1
357400070514      *
357500070514      * ? Primo ciclo per togliere le priorità a tutti records
357600070514      * ? del SFL
357700070514     C                   DO        RrnLast       S2nrr
357800070514     C     S2nrr         CHAIN     FDG0S02
357900080609     c                   if        s2sce = '1'  or s2sce = '5' or s2sce = '8'
358000101129     c                             or s2sce = '7'
358100070514     C                   MOVE      *ON           *IN32
358200070515     c                   else
358300070515     C                   SetOFF                                       32
358400070515     c                   end
358500070514     c                   clear                   H2SUBST
358600101130     c                   eval      *in45 = H2in45
358700130221     c     h2xco         comp      '0'                                88
358800070514     C                   UPDATE    FDG0S02
358900070514     c                   end
359000070514      *
359100070514      * ? Ordina il SFL....
359200070514     c                   exsr      Ordina_SFL2
359300070514      *
359400070514      * ? Secondo ciclo x trovare il posizionamento dal record....
359500070514     C                   DO        RrnLast       S2nrr
359600070514     C     S2nrr         CHAIN     FDG0S02
359700070514     c                   if        (*in01 and C2pos1 >  s2CGI)     or
359800070514     c                             (*in02 and C2pos2 >  s2cRSD)    or
359900070514     c                             (*in03 and C2pos3 >  s2cIND)    or
360000070514     c                             (*in04 and C2pos4 >  s2cLOD)    or
360100070514     c                             (*in05 and C2pos5 <  s2cVLF)    or
360200070514     c                             (*in06 and C2pos6 <  s2cPKF)
360300070514     C*
360400070514     c                   eval      C2RCD = S2nrr + 1
360500070514     c                   if        c2RCD > RrnLast
360600070514     c                   eval      C2RCD = RrnLast
360700070514     c                   end
360800070514     C*
360900070514     C* Si tiene in memoria la riga toccata con SFLNXTCHG
361000080609     c                   if        s2sce = '1'  or s2sce = '5' or s2sce = '8'
361100101129     c                             or s2sce = '7'
361200070514     C                   MOVE      *ON           *IN32
361300070515     c                   else
361400070515     C                   SetOFF                                       32
361500070514     c                   end
361600070514     C*
361700101130     c                   eval      *in45 = H2in45
361800130221     c     h2xco         comp      '0'                                88
361900070514     C                   UPDATE    FDG0S02
362000070514     C*
362100070514     c                   else
362200070514     C*
362300070514     c                   iter
362400070514     C*
362500070514     c                   end
362600070514     C                   ENDDO
362700070514      *
362800070514     C                   ENDSR
362900070514      *---------------------------------------------------------------*
363000070514      *?  Se richiesta una ricerca di stringa fra i record del SFL   ?
363100070514      *---------------------------------------------------------------*
363200070514     C     Cerca_Parola  BEGSR
363300070514      *
363400070514      * ? Deve cercare la stringa e visualizzare prioritariamente i records
363500070514      * ? contenenti la stringa
363600070514     c                   seton                                        99
363700070514     c                   eval      C2RCD =  1
363800070514     c                   eval      WLen = %len(%Trim(Stringa_20A))
363900081203      * solo se selezionato qualcosa
364000081203     c                   if        wlen > 0
364100070514      *
364200070514     C                   DO        RrnLast       S2nrr
364300070514     C     S2nrr         CHAIN     FDG0S02
364400070514     c                   clear                   WPos
364500070514     c                   clear                   H2SUBST
364600070514      *
364700070514     c   02              eval      WPos = %scan(%subst(Stringa_20A:1:WLen) :
364800070514     c                              S2cRSD)
364900070514      *
365000070514     c   03              eval      WPos = %scan(%subst(Stringa_20A:1:WLen) :
365100070514     c                              S2cIND)
365200070514      *
365300070514     c   04              eval      WPos = %scan(%subst(Stringa_20A:1:WLen) :
365400070514     c                              S2cLOD)
365500070514
365600070514      *  Se ha trovato la stringa cercata
365700070514     c                   if        Wpos > 0
365800070514     c                   z-add     1             H2SUBST
365900070514     c                   end
366000070514     C*
366100070514     C* Si tiene in memoria la riga toccata con SFLNXTCHG
366200080609     c                   if        s2sce = '1'  or s2sce = '5' or s2sce = '8'
366300101129     c                             or s2sce = '7'
366400070514     C                   MOVE      *ON           *IN32
366500070515     c                   else
366600070515     C                   SetOFF                                       32
366700070514     c                   end
366800070514     C*
366900101130     c                   eval      *in45 = H2in45
367000130221     c     h2xco         comp      '0'                                88
367100070514     C                   UPDATE    FDG0S02
367200070514     C*
367300070514     C                   ENDDO
367400070514      *
367500081203     c                   end
367600081203      *
367700070514      *  Quindi emette il SFL riordinato
367800070514     C                   EXSR      Ordina_SFL2
367900070514      *
368000070514     C                   ENDSR
368100070514      *---------------------------------------------------------------*
368200070514      *?  CONTROLLO OPZIONI LISTA                                    ?
368300070514      *---------------------------------------------------------------*
368400070514     C     CTRS2         BEGSR
368500070514     C*
368600070301     C*  Pulizia campi Totali
368700070302     c                   z-add     0             totSPE_s2
368800070302     c                   z-add     0             totPKL_s2
368900070302     c                   z-add     0             totVLU_s2
369000070302     c                   z-add     0             totCOL_s2
369100070306     c                   z-add     0             giri_scelti
369200070306     c                   clear                   SAV_S2cgi
369300070413     c                   clear                   primoGiro_Blk     1
369400101130     c                   clear                   exec_opzInfo
369500151001     c                   clear                   in66_msg          1
369600070301     C*
369700070301     C                   MOVEL     *OFF          $ESCI
369800070301     C                   SETOFF                                       99
369900100505     C*
370000100505     C*  Se è stato premuto il tasto per forzare lo imposta prima dei controlli
370100100505     C*   sui singoli records
370200100505     C     $TASTO        IFEQ      F02
370300100505     c                   eval      Premuto_F02 = '1'
370400100505     c                   end
370500070301     C*
370600070301     C* Leggo il sfl solo se ci sono rcd
3707000703011    C     WMAX          IFGT      0
370800070302     C                   READC     FDG0S02                                21
370900070301     C*
371000070301     C* esce se fine sfl o errore che richiede l'uscita
3711000703012    C     *IN21         DOWEQ     *OFF
371200070301     C     $ESCI         ANDEQ     *OFF
371300070302     C                   Z-ADD     S2NRR         C2RCD
371400070301     C* ctrl su riga
371500070302     C                   EXSR      RECS2
371600070301      *
371700070301     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
3718000703013    C     *IN99         IFEQ      *ON
371900070301     C     $ESCI         OREQ      *ON
372000100505     C     da_forzare    OREQ      '1'
372100070301     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
372200070301     C* la ripristinerò a conclusione del ciclo di READC
372300070302     C                   MOVE      *OFF          $INZS2
3724000703013-   C                   ENDIF
372500070301     C*
372600070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
372700080609      * ?    aggiunto anche l'aggiornamento dell'INFO da visionare/visionato
372800080609     c                   if        s2sce = '1'  or s2sce = '5' or s2sce = '8'
372900101129     c                             or s2sce = '7'
373000070301     C                   MOVE      *ON           *IN32
373100070515     c                   else
373200070515     C                   SetOFF                                       32
373300070307     c                   end
373400101130     c                   eval      *in45 = H2in45
373500130221     c     h2xco         comp      '0'                                88
373600070302     C                   UPDATE    FDG0S02
373700070301     C*
373800070301     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
3739000703013    C     $ESCI         IFEQ      *OFF
374000070302     C                   READC     FDG0S02                                21
374100070301      *
374200070301     C* a fine ciclo ripristino il flag richiesta iniz.sfl
3743000703014    C     *IN21         IFEQ      *ON
374400070301     C* calcolo pagina a cui deve posizionarsi
374500070302     c                   z-add     s2pag         Wpagine
374600070301     C                   EXSR      CLCPAG
3747000703014-   C                   ENDIF
3748000703013-   C                   ENDIF
374900070301     C*
3750000703012-   C                   ENDDO
375100070301     C*
375200100505     C* Deve bloccarsi come se fosse un errore
375300100505     C*  poichè riga x riga deve forzare le eventuali speidizioni
375400100505      *
375500100505     c                   if        Da_forzare = '1' and
375600100505     c                             d1SCE = Assegnare
375700100505     C                   SETON                                        99
375800100505     c                   end
375900100505     C*
3760000703011-   C                   ENDIF
376100070301     C*
376200070301     C                   ENDSR
376300070301     C/EJECT
376400070301      *---------------------------------------------------------------*
376500070320      *?  CONTROLLO CAMPI I/O RIGA LISTA                             ?
376600070301      *---------------------------------------------------------------*
376700070302     C     RECS2         BEGSR
376800070301     C*
376900070301     C* reset indicatori DSPATR
377000070308     c                   exsr      INZ_indERR
377100100505     C*
377200100505     C* x scelta blank resettare il campo di forzatura
3773001005053    C     S2sce         IFeq      *blank
377400100505     c                   eval      s2Forza = ' '
3775001005053-   C                   ENDIF
377600160317     C*
377700141113     c                   clear                   fnlr98ds2
377800141113     c                   if        s2sce = '1' and
377900141113     c                             (d1sce = togliere or
378000141113     c                             d1sce = trasferire)
378100141113     c*check spedizione con disposizioni di dirottamento richiede forzatura se presenti
378200141113     c                   z-add     s2caas        i982aas
378300141113     c                   z-add     s2cLNP        i982lnp
378400141113     c                   z-add     s2cNrs        i982nrs
378500141113     c                   z-add     s2cNsp        i982nsp
378600141113     c                   call      'FNLR98R2'
378700141113     c                   parm                    fnlr98ds2
378800141113     c                   if        o982esito = '1'
378900141113     c                   seton                                        99
379000141113     c                   endif
379100141113     c                   endif
379200160317     C*
379300160317     C*  non possono essere assegnate bolle DPD con VEDI PACCO a GIRI reali
379400160317     C*   ma solo ad autista fittizio quindi a distinte Fittizie
379500160317     c                   if        s2sce = '1' and
379600160317     c                             (d1sce = assegnare or
379700160317     c                              d1sce = trasferire)
379800160321     c                   if         S2CGI = GIRO_vd_PACCO   and
379900160322     c                              H1pdd_piu <> 'S' and h1ndc_piu > 0
380000160322     c                   seton                                        9958
380100160317     c                   endif
380200160317     c                   endif
380300070327     C*
380400070327     C* gestione opzioni
3805000703273    C     S2sce         IFNE      *blank
380600070327     C     *IN99         ANDEQ     *OFF
380700070327     C                   EXSR      SCE_S2
3808000703273-   C                   ENDIF
380900070301     C*
381000070301     C                   ENDSR
381100070301      *---------------------------------------------------------------*
381200070320      *?  GESTIONE OPZIONI LISTA                                     ?
381300070301      *---------------------------------------------------------------*
381400070309     C     SCE_S2        BEGSR
381500070307     C*
381600070307     C*  Interrogazione bolla
381700070307     c                   If        s2SCE = '5'
381800070307     c                   eval      kpjbus = kpjbu
381900070307     C                   clear                   dslr36
382000070307     C                   move      s2cAAS        P36AAS
382100070307     C                   move      s2cLNP        P36LNP
382200070307     C                   move      s2cNRS        P36NRS
382300070307     C                   move      s2cNSP        P36NSP
382400070307     C                   MOVEL     '1'           P36FLG
382500070307     C                   MOVEL     ' '           P36F03
382600070307     C                   movel(p)  dslr36        KPJBU
382700070307     C                   call      'FNLR36R'                            99
382800070307     C                   parm                    KPJBA
382900070307     c                   movel     kpjbus        kpjbu
383000070307     c                   clear                   s2SCE
383100070307     c                   seton                                        99
383200070307     c                   endIf
383300150930      **
383400150930     C*   attenzione: in assegnazione deve controllare se è stato inviato
383500150930     C*   un contro ALert (di NON CONSEGNA) in tal caso deve Forzare l'immissione
383600150930     C*   in distinta della Spedizione con F2.
383700150930      *
383800150930     c                   IF        d1SCE = Assegnare and h2Calert = 'S' and
383900150930     c                             s2SCE = '1' and s2Forza =' '
384000150930     C* F2=Forzatura
384100150930     C                   if        Premuto_F02 = '1'
384200150930     c                   setoff                                       66
384300150930     C                   move      '0'           da_forzare
384400150930     c                   eval      s2Forza = 'F'
384500150930     C                   eval      Premuto_F02 = '0'
384600150930     C*
384700150930      * errore: Occorre forzare la riga se la si vuole proprio selezionare
384800150930      *   si deve forzare con "F2" la riga... deve segnalarlo.
384900150930     c                   else
385000150930     c                   seton                                        66
385100151001     c                   if         in66_msg = *blank
385200150930     c                   eval       in66_msg = 'C'
385300151001     c                   end
385400150930     C                   move      '1'           da_forzare
385500150930     c                   end
385600150930     C*
385700150930      *---- (x1)
385800150930      **
385900100505     C*   attenzione se in assegnazione deve controllare se si tratta di
386000100505     C*    una spedizione  da forzare perchè con particolarità di
386100100505     C*    Particolarità di Attesa
386200100505      *
386300150930     c                   ElseIf    d1SCE = Assegnare and h2att = 'S' and
386400100505     c                             s2SCE = '1' and s2pATT = 'A' and s2Forza =' '
386500100505     C* F2=Forzatura
386600100505     C                   if        Premuto_F02 = '1'
386700100505     c                   setoff                                       66
386800100505     C                   move      '0'           da_forzare
386900100505     c                   eval      s2Forza = 'F'
387000100505     C                   eval      Premuto_F02 = '0'
387100100505     C*
387200100505      * errore: Occorre forzare la riga se la si vuole proprio selezionare
387300100505      *   si deve forzare con "F2" la riga... deve segnalarlo.
387400100505     c                   else
387500100505     c                   seton                                        66
387600151001     c                   if         in66_msg = *blank
387700150930     c                   eval       in66_msg = 'A'
387800151001     c                   end
387900100505     C                   move      '1'           da_forzare
388000100505     c                   end
388100100505     C*
388200100505      *---- (x1)
388300100505     c                   Else
388400100505     C*
388500100505      *--------- (2)
388600070301     C*  Totalizza le Spedizioni/Pesi/Volumi
388700070302     c                   If        s2SCE = '1'
388800100505     C*
388900070302     c                   add       1             totSPE_s2
389000070427     c                   add       s2pkf         totPKL_s2
389100070302     c                   add       s2cvlf        totVLU_s2
389200070302     c                   add       s2cncl        totCOL_s2
389300070313     C*
389400070313     c                   move      s2cgi         sav_sxcgi
389500070305     C*
389600070305     C*   Conta quanti giri sono stati scelti nel sfl
389700070305     C*  per permettere di interrogare con l'F4 con stesso giro
389800100505      *--------- (3)
389900070413     c                   if        s2cgi <> SAV_s2cgi  or
390000070413     c                             primoGiro_Blk = *Blank and s2cgi = *Blank
390100070413     C*
390200070413      *  e se il giro è Blank non deve ripetere un eventuale conto di altri con giro Blank
390300070413     c                   eval      primoGiro_Blk = 'S'
390400070413     C*
390500070305     c                   add       1             giri_scelti
390600070305     c                   move      s2cgi         sav_s2cgi
390700070305     c                   move      s2cgi         c2giro
390800070305     c                   end
390900100505      *---- (e3)
391000100505     c                   endIf
391100100505      *---- (e2)
391200070301     c                   endIf
391300100505      *---- (e1)
391400080609      *
391500101129     C*  Interr.INFO e NOTE  una INFO da vedere "cOK"
391600080609      *
391700101130     c                   If        s2SCE = '7' and H2in45 ='1'
391800101130     c                   move      'S'           exec_opzInfo
391900080609      *
392000101129      /free
392100101129         clear NOTINFO;
392200101129           WSPED =  %editc(S2CAAS:'X') + '/' + %editc(S2CLNP:'X') + '/' +
392300101129                    %editc(S2CNRS:'X') + '/' + %editc(S2CNSP:'X');
392400101129
392500101129          wdatabol = S2CAAS * 10000  + H2mgs;
392600101129          dataiso = %date(wdatabol:*iso);
392700101129          dataeur = dataiso;
392800101129          WDEL   = %dec(dataeur);
392900101129
393000101129           WDEST = S2CRSD;
393100101129           tbeCOD = 'ASA';
393200101129           tbeKE1 = S2CCOK;
393300101129           chain (tbecod:tbeke1) tntbe01l;
393400101129           clear WDcodInfo;
393500101129          if  %found(tntbe01l);
393600101129           WDcodInfo = tbeUNI;
393700101129          endif;
393800101129           WCODINFO= %trim(S2CCOK)  + ': ' + WDcodInfo;
393900101129           WNOTASS = H2NOTASS;
394000101129         exfmt NOTINFO;
394100101129      /end-free
394200101129      *
394300101130     c                   clear                   s2SCE
394400101129     c                   endIf
394500101129      *---- (e1)
394600101129      *
394700101129     C*  Chiude INFO se presente una INFO da vedere "cOK"
394800101129      *
394900101129     c                   If        s2SCE = '8'
395000101129      *
395100080613      *   Solo se attivata proc.PDA consegne e RICHIAMATO dal MONITOR
395200080609     c                   if          PDAconsegne <> *blank and
395300080609     c                             PGM_Chiamante = 'MONITOR'
395400080609      *
395500080627     c                   exsr      agg_PCT_COK
395600080627      *
395700080609     c                   clear                   s2SCE
395800101130     c                   move      'S'           exec_opzInfo
395900080609      *
396000080609     c                   else
396100080613      * errore: se non attivata proc.Consegne con PDA o NON richiamato dal
396200080613      *  MONITOR.
396300080818      *   si è scelta opz.(8) sulla riga, deve segnalare l'errore come
396400080613      *    opzione non valida.
396500080609     c                   seton                                        9965
396600100505     C                   move      '1'           opz_errata
396700080609     c                   endIf
396800080609      *
396900080609     c                   endIf
397000070301     C*
397100070301     C                   ENDSR
397200070301     C/EJECT
397300070302      *---------------------------------------------------------------*
397400070320      *?  Chiusura Files 2°SFL                                       ?
397500070302      *---------------------------------------------------------------*
397600070302     C     CLOF_S2       BEGSR
397700070306      *
397800070531     c                   if        d1sce = Togliere or d1sce = Trasferire and
397900070404     c                             test_Parallelo = 'S'
398000070306     C/EXEC SQL
398100070306     C+ CLOSE i1
398200070306     C/END-EXEC
398300070306     c                   end
398400070306      *
398500070302     c                   ENDSR
398600070302     C/EJECT
398700070301      *---------------------------------------------------------------*
398800070316      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
398900070301      *---------------------------------------------------------------*
399000070228     C     CLCPAG        BEGSR
399100940224     C* Input :
399200940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
399300940224     C*          cursore
399400070301     C* - Wpagine= numero rcd per pagina sfl
399500940224     C* Output :
399600940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
399700940224     C*
399800070301     C     WSFL          DIV       Wpagine       PAGINE            4 0
399900940224     C                   MVR                     RESTO             3 0
400000940224     C     RESTO         IFGT      0
400100940224     C                   ADD       1             PAGINE
400200940224     C                   ENDIF
400300070301     C     PAGINE        MULT      Wpagine       WPAG
400400940224     C*
400500940224     C                   ENDSR
400600070302     C************************************************************
400700070320      *?  Emissione ultima finestra di riepilogo                     ?
400800070302     C************************************************************
400900070302     C     GESW1         BEGSR
401000070302     C*
401100070302     C* inizializzazione videata
401200070302     C     $INZW1        IFEQ      *ON
401300070302     C                   EXSR      INZW1
401400070302     C                   MOVE      *OFF          $INZW1
401500070302     C                   ENDIF
401600070302     C*
401700070302     c     Window_1      tag
401800070302     C*  Emissione Video
401900070302      * ?            *-----------------------*
402000070302     C                   EXFMT     FDG0W01
402100070302      * ?            *-----------------------*
402200070326     c                   setoff                                       99
402300070302     C* Selezioni
4024000703021    C                   SELECT
402500070302     C* F3=Fine
402600070302     C                   WHEN      $TASTO   =   F03
402700070302     C                   EXSR      F03_exit
402800070319     c                   rolbk
402900070302     C* F12=Ritorno
403000070302     C                   WHEN      $TASTO   =   F12
403100070308     c                   exsr      INZ_indERR
403200070305     c                   move      'S2'          $GEST
403300070305      *   scelta = 1 se veniva da SFL 1
403400070531     c                   if        d1sce = Assegnare and sav$GEST ='S1'
403500070305     c                   move      'S1'          $GEST
403600070305     c                   end
403700070302      *
4038000703021O   C                   OTHER
403900070302     C* CONTROLLO DATI
404000070302     C                   EXSR      CTRW1
4041000703021-   C                   ENDSL
404200070302     C*
404300070302     C* Per errori o decodifiche
404400070302     c   99              goto      Window_1
404500070302     C*
404600070302     C*    Aggiorna il Data Base
404700070302     C                   if        $TASTO   =   F06
404800070302     C                   EXSR      F06_Aggiorna
404900070302     c                   end
405000070302      *
405100070302     C                   ENDSR
405200070302     C/EJECT
405300070302     C************************************************************
405400070320      *?  INIZIALIZZA indicatori di errore                           ?
405500070302     C************************************************************
405600070308     C     INZ_indERR    BEGSR
405700070308      *
405800070308     C                   move      *ALL'0'       IN5190           40
405900070308     C                   moveA     IN5190        *IN(51)
406000070308      *
406100070308     C                   ENDSR
406200070308     C/EJECT
406300070308     C************************************************************
406400070316      * ?  INIZIALIZZAZIONE Window Finale prima dell'aggiornamento   ?
406500070308     C************************************************************
406600070308     C     INZW1         BEGSR
406700070308     C*
406800070302     C* Pulizia campi e indicatori
406900070308     c                   exsr      INZ_indERR
407000070302     c                   setoff                                       99  50
407100070306     c                   setoff                                       515253
407200070306     c                   setoff                                       545556
407300070302     C* Pulizia formato record
407400070302     c                   clear                   FDG0W01
407500070306     C* imposta campi video
407600070302     c                   z-add     totSPE_s2     w1ssn1
407700070302     c                   z-add     totPKL_s2     w1ssp1
407800070302     c                   z-add     totVLU_s2     w1ssv1
407900070306      *
408000070306     c                   z-add     totSPE_s2     w1ssn2
408100070306     c                   z-add     totPKL_s2     w1ssp2
408200070306     c                   z-add     totVLU_s2     w1ssv2
408300070306      *
408400070306     c                   select
408500070306     C*  Assegnare
408600070531     c                   When      d1sce = Assegnare
408700070319     c                   z-add     h1ndc_piu     w1ndc1
408800070306     c                   move      Alla          w1ada
408900070326     c                   eval      %subst(W1dSCE:6:11) = OPZ_1
409000070306      *
409100070306     C*  Togliere
409200070531     c                   When      d1sce = Togliere
409300070319     c                   z-add     h1ndc_meno    w1ndc1
409400070306     c                   move      Dalla         w1ada
409500070326     c                   eval      %subst(W1dSCE:7:11) = OPZ_2
409600070306      *
409700070306     C*  Trasferire
409800070531     c                   When      d1sce = Trasferire
409900070306     C* Se sta eseguendo il trasferimento occorre aprire la finestra
410000070306     c                   seton                                        50
410100070319     c                   z-add     h1ndc_meno    w1ndc1
410200070319     c                   z-add     h1ndc_piu     w1ndc2
410300070306     c                   move      Dalla         w1ada
410400070326     c                   eval      %subst(W1dSCE:6:11) = OPZ_3
410500070306      *
410600070306     c                   endSL
410700070305      *  Legge il foglio
410800070305     c                   move      w1ndc1        wNFV
410900070319     c                   eval      Aggior_Dist = 'N'
411000070319      *                                     ?  =======  ?
411100070316     c                   exsr      CHAIN_Dist
411200070319      *
411300070316     c                   if        Found_Dist = 'N'
411400070305     c                   movel     *all'?'       w1pdr1
411500070305     c                   else
411600070305     C                   move      'A'           tipo_cod
411700070305     C                   move      dstpdr        cod_padr
411800070305     C     Kapd          CHAIN     fiapd01l
411900070305     c                   if        %Found(fiapd01l)
412000070305     c                   movel     APDRSC        w1pdr1
412100070305     c                   end
412200070305      *
412300070305     c                   z-add     DSTPKL        W1DMP1
412400070305     c                   z-add     DSTVLU        W1DMV1
412500070305     c                   z-add     DSTNBL        W1DMN1
412600070305      *
412700070305     c                   z-add     DSTPKLA       W1DAP1
412800070305     c                   z-add     DSTVLUA       W1DAV1
412900070305     c                   z-add     DSTNBLA       W1DAN1
413000070305     c                   end
413100070305     C*
413200070306      *  Legge il foglio
413300070316     c                   move      w1ndc2        wNFV
413400070319     c                   eval      Aggior_Dist = 'N'
413500070319      *                                     ?  =======  ?
413600070316     c                   exsr      CHAIN_Dist
413700070319      *
413800070316     c                   if        Found_Dist = 'N'
413900070306     c                   movel     *all'?'       w1pdr2
414000070306     c                   else
414100070306     C                   move      'A'           tipo_cod
414200070306     C                   move      dstpdr        cod_padr
414300070306     C     Kapd          CHAIN     fiapd01l
414400070306     c                   if        %Found(fiapd01l)
414500070306     c                   movel     APDRSC        w1pdr2
414600070306     c                   end
414700070306      *
414800070306     c                   z-add     DSTPKL        W1DMP2
414900070306     c                   z-add     DSTVLU        W1DMV2
415000070306     c                   z-add     DSTNBL        W1DMN2
415100070306      *
415200070306     c                   z-add     DSTPKLA       W1DAP2
415300070306     c                   z-add     DSTVLUA       W1DAV2
415400070306     c                   z-add     DSTNBLA       W1DAN2
415500070306     c                   end
415600070306      *
415700070306     c                   select
415800070306     C*  Assegnare
415900070531     c                   When      d1sce = Assegnare
416000070306      *
416100070306     c     W1DAN1        add       totSPE_s2     w1tsn1
416200070306     c     W1DAP1        add       totPKL_s2     w1tsp1
416300070306     c     W1DAV1        add       totVLU_s2     w1tsv1
416400070306     c     w1tsn1        comp      W1DMN1                             51
416500070306     c     w1tsp1        comp      W1DMP1                             52
416600070306     c     w1tsv1        comp      W1DMV1                             53
416700070306     C*
416800070531     c                   When      d1sce = Togliere
416900070306      *
417000070306     c     W1DAN1        sub       totSPE_s2     w1tsn1
417100070306     c     W1DAP1        sub       totPKL_s2     w1tsp1
417200070306     c     W1DAV1        sub       totVLU_s2     w1tsv1
417300070306     c     w1tsn1        comp      W1DMN1                             51
417400070306     c     w1tsp1        comp      W1DMP1                             52
417500070306     c     w1tsv1        comp      W1DMV1                             53
417600070306     C*
417700070531     c                   When      d1sce = Trasferire
417800070302     C*
417900070306     c     W1DAN1        sub       totSPE_s2     w1tsn1
418000070306     c     W1DAP1        sub       totPKL_s2     w1tsp1
418100070306     c     W1DAV1        sub       totVLU_s2     w1tsv1
418200070306     c     w1tsn1        comp      W1DMN1                             51
418300070306     c     w1tsp1        comp      W1DMP1                             52
418400070306     c     w1tsv1        comp      W1DMV1                             53
418500070306      *
418600070306     c     W1DAN2        add       totSPE_s2     w1tsn2
418700070306     c     W1DAP2        add       totPKL_s2     w1tsp2
418800070306     c     W1DAV2        add       totVLU_s2     w1tsv2
418900070306     c     w1tsn2        comp      W1DMN2                             54
419000070306     c     w1tsp2        comp      W1DMP2                             55
419100070306     c     w1tsv2        comp      W1DMV2                             56
419200070306      *
419300070306     c                   endsl
419400070306      *
419500070302     C                   ENDSR
419600070302     C************************************************************
419700070320      *?  Controlli primo video di SCELTA Operazione da eseguire     ?
419800070302     C************************************************************
419900070302     C     CTRW1         BEGSR
420000070302     C*
420100070302     C*  Spegne indicatori di errore
420200070308     c                   exsr      INZ_indERR
420300070302     c                   setoff                                       99
420400070306     C*  se superano i MAX.previsti in distinta
420500070531     c                   if        d1sce = Trasferire
420600070306     c     w1tsn2        comp      W1DMN2                             54
420700070306     c     w1tsp2        comp      W1DMP2                             55
420800070306     c     w1tsv2        comp      W1DMV2                             56
420900070306     c                   else
421000070306     c     w1tsn1        comp      W1DMN1                             51
421100070306     c     w1tsp1        comp      W1DMP1                             52
421200070306     c     w1tsv1        comp      W1DMV1                             53
421300070306     c                   end
421400070302     C*
421500070302     C     ENDCTRW1      ENDSR
421600070301      *---------------------------------------------------------------*
421700070320      *?  GESTIONE F03 x uscire LR                                   ?
421800070301      *---------------------------------------------------------------*
421900070227     C     F03_exit      BEGSR
422000070227     C* fine programma
422100940309     C                   MOVE      *ON           $FINE
422200070302      *
422300940309     C                   ENDSR
422400070227     C/EJECT
422500070301      *---------------------------------------------------------------*
422600070320      *?  GESTIONE F04 x interrogare distinte stesso giro            ?
422700070301      *---------------------------------------------------------------*
422800070227     C     F04_interr    BEGSR
422900070227     C*
423000070313     c                   seton                                        99
423100070321     C                   MOVEL     sav_sxCGI     wCGI
423200070606     c                   move      Scegliere     w1sce
423300070606     c                   clear                   w1NO_dis
423400070606     c                   if        d1sce = Trasferire
423500070606     c                   move      d1ndc3        w1NO_dis
423600070606     c                   end
423700070321     c                   exsr      Ricerca_Dist
423800070302      *
423900070302     C* controlla la distinta selezionata
424000070313     c                   z-add     FDGNFV        wNFV
424100070319     c                   eval      Aggior_Dist = 'N'
424200070319      *                                     ?  =======  ?
424300070316     c                   exsr      CHAIN_Dist
424400070319     c                   if        Error_Dist = 'S'
424500070319     c                   if        Rec_Allocato = 'S'
424600070319     C                   SETON                                            62
424700070319     c                   end
424800070319     C                   SETON                                            99
424900070319     C                   goto      ENDF04
425000070319     c                   end
425100070319      *
425200070316     c                   if        Found_Dist = 'N'
425300070302     C                   SETON                                        52  99
425400070302     C                   goto      ENDF04
425500070302     c                   else
425600070413      *
425700070302      * Foglio chiuso o Annullato
425800070302    3C     dstFCF        ifEQ      'S'
425900070302    4C     dstATB        orNE      ' '
426000070306     C                   SETON                                        53  99
426100070302     C                   goto      ENDF04
426200070302     c                   endIF
426300070413      *
426400070302     c                   end
426500160321      *
426600160321      *  controlla se l'Autista è fittizio quindi possiamo considerare
426700160321      *   la Distinta Fittizia
426800160321     C                   move      'A'           tipo_cod
426900160321     C                   move      dstpdr        cod_padr
427000160321     C     Kapd          CHAIN     fiapd01l
427100160321      *   se Escluso da Fatturazione
427200160323     c                   if        %Found(fiapd01l)
427300160323     c                   if         apdPDD = 'S'
427400160321     c                   movel     'S'           H1pdd_PIU
427500160323     c                   else
427600160323     c                   clear                   H1pdd_PIU
427700160323     c                   end
427800160321     c                   end
427900070302      * se tutto OK
428000070302     C                   MOVE      *zeros        d1ndc1
428100070313     C                   MOVE      FDGNFV        d1ndc1
428200070319     C                   z-add     FDGNFV        h1ndc_piu
428300070319     c                   z-add     dstPDR        h1pdr_piu
428400120209     c                   z-add     dstDFV        h1ddc_piu
428500070319     c                   move      dstFPP        h1fpp_piu
428600080929      *
428700080929      * Controlla se la distinta è gestita con il PDA
428800080929      *  se in test nel filler
428900080929     c                   eval      ddstFLR = dstFLR
429000080929     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
429100080929     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
429200080929     c                   eval      h1PDA_piu = 'S'
429300080929     c                   else
429400080929     c                   clear                   h1PDA_piu
429500080929     c                   end
429600070413      *
429700070313      * intestazione SF1
429800070531     C                   if        d1sce = Assegnare and h1ndc_piu > 0
429900070313     c                   movel     Alla_Dist     c1ndc1
430000070319     c                   move      h1ndc_piu     distintalfa
430100070313     c                   do        7             xy
430200070313     c                   if        ndc_a(xy) <> '0'
430300070313     c                   leave
430400070313     c                   end
430500070313     c                   move      ' '           ndc_a(xy)
430600070313     c                   enddo
430700070313     c                   move      distintalfa   c1ndc1
430800070313      *
430900070313     c                   eval      C2dsce = OPZ_1
431000070313     c                   eval      C2dess = 'alla Distinta nr.' +
431100070319     c                             %trim(%editc(h1ndc_piu:'Z'))
431200070313     c                   end
431300070227     C*
431400070302     C     EndF04        ENDSR
431500070227     C/EJECT
431600070301      *---------------------------------------------------------------*
431700070320      *?  GESTIONE F05 x interrogare tutte le distinte               ?
431800070301      *---------------------------------------------------------------*
431900070227     C     F05_interr    BEGSR
432000070302     C*
432100070313     c                   seton                                        99
432200070606     c                   move      Scegliere     w1sce
432300070606     c                   clear                   w1NO_dis
432400070606     c                   if        d1sce = Trasferire
432500070606     c                   move      d1ndc3        w1NO_dis
432600070606     c                   end
432700070321     c                   exsr      Ricerca_Dist
432800070321     c                   z-add     FDGNFV        wNFV
432900070302     C* controlla la distinta selezionata
433000070319     c                   eval      Aggior_Dist = 'N'
433100070319      *                                     ?  =======  ?
433200070316     c                   exsr      CHAIN_Dist
433300070319      *
433400070319     c                   if        Error_Dist = 'S'
433500070319     c                   if        Rec_Allocato = 'S'
433600070326     C                   SETON                                            62
433700070319     c                   end
433800070319     C                   SETON                                            99
433900070319     C                   goto      ENDF05
434000070319     c                   end
434100070319      *
434200070316     c                   if        Found_Dist = 'N'
434300070306     C                   SETON                                        54  99
434400070302     C                   goto      ENDF05
434500070302     c                   else
434600070302      * Foglio chiuso o Annullato
434700070302    3C     dstFCF        ifEQ      'S'
434800070302    4C     dstATB        orNE      ' '
434900070306     C                   SETON                                        55  99
435000070302     C                   goto      ENDF05
435100070302     c                   endIF
435200070413      *
435300070302     c                   end
435400160321      *
435500160321      *  controlla se l'Autista è fittizio quindi possiamo considerare
435600160321      *   la Distinta Fittizia
435700160321     C                   move      'A'           tipo_cod
435800160321     C                   move      dstpdr        cod_padr
435900160321     C     Kapd          CHAIN     fiapd01l
436000160321      *   se Escluso da Fatturazione
436100160323     c                   if        %Found(fiapd01l)
436200160323     c                   if           apdPDD = 'S'
436300160321     c                   movel     'S'           H1pdd_PIU
436400160323     c                   else
436500160323     c                   clear                   H1pdd_PIU
436600160323     c                   end
436700160321     c                   end
436800070302      * se tutto OK
436900070302     C                   MOVE      *zeros        d1ndc1
437000070313     C                   MOVE      FDGNFV        d1ndc1
437100070319     C                   z-add     FDGNFV        h1ndc_piu
437200070319     c                   z-add     dstPDR        h1pdr_piu
437300120209     c                   z-add     dstDFV        h1ddc_piu
437400070319     c                   move      dstFPP        h1fpp_piu
437500080929      *
437600080929      * Controlla se la distinta è gestita con il PDA
437700080929      *  se in test nel filler
437800080929     c                   eval      ddstFLR = dstFLR
437900080929     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
438000080929     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
438100080929     c                   eval      h1PDA_piu = 'S'
438200080929     c                   else
438300080929     c                   clear                   h1PDA_piu
438400080929     c                   end
438500070313      *
438600070313      * intestazione SF1
438700070531     C                   if        d1sce = Assegnare and h1ndc_piu > 0
438800070313     c                   movel     Alla_Dist     c1ndc1
438900070319     c                   move      h1ndc_piu     distintalfa
439000070313     c                   do        7             xy
439100070313     c                   if        ndc_a(xy) <> '0'
439200070313     c                   leave
439300070313     c                   end
439400070313     c                   move      ' '           ndc_a(xy)
439500070313     c                   enddo
439600070313     c                   move      distintalfa   c1ndc1
439700070313      *
439800070313     c                   eval      C2dsce = OPZ_1
439900070313     c                   eval      C2dess = 'alla Distinta nr.' +
440000070319     c                             %trim(%editc(h1ndc_piu:'Z'))
440100070313     c                   end
440200070313     C*
440300070302     C     EndF05        ENDSR
440400070227     C/EJECT
440500070301      *---------------------------------------------------------------*
440600070320      *?  GESTIONE F08 x interrogare Anagrafica Autisti              ?
440700070301      *---------------------------------------------------------------*
440800070227     C     F08_interr    BEGSR
440900070227     C*
441000070227     c                   clear                   fnlv24ds
441100070227     C                   Z-ADD     d1cFGS        d24FIL
441200070227     C                   MOVEL     'R'           d24FLG
441300070227     C                   MOVEL     'A'           d24tip
441400070228     c                   eval      kpjbus = kpjbu
441500070227     C                   MOVEL(p)  fnlv24ds      KPJBU
441600070227     C                   CALL      'FNLV24R'
441700070227     C                   PARM                    KPJBA
441800070227     C                   MOVEL     KPJBU         fnlv24ds
441900070228     c                   eval      kpjbu = kpjbus
442000070227     C*
442100070227     C                   ENDSR
442200070302     C/EJECT
442300070302      *---------------------------------------------------------------*
442400070320      *?  GESTIONE F06 x aggiornare il Data Base                     ?
442500070302      *---------------------------------------------------------------*
442600070302     C     F06_Aggiorna  BEGSR
442700070306      **
442800070306      **  Leggendo sempre le bolle selezionate dal 2°SFL
442900070309      **   esegue aggiornamento degli archivi e se ci sono problemi
443000070309      **  durante l'aggiornamento delle singole bolle emette 3°SFL con la
443100070309      **   lista delle bolle non aggiornate avvertendo del mancato aggiornamento.
443200070309      **
443300070319      ** L'utente deve quindi rieseguire l'operazione poichè viene annullata tutta
443400070319      ** la transazione con il Rolback.
443500070309      **
443600070309     C*  Esegue l'aggiornamento ed il controllo sul 3° SFL
443700070309     c                   move      'S3'          $GEST
443800070309     c                   move      *on           $INZS3
443900070305     C*
444000070302     C                   ENDSR
444100070301     C/EJECT
444200070301      *---------------------------------------------------------------*
444300070316      *  ?  OPERAZIONI INIZIALI      ?
444400070301      *---------------------------------------------------------------*
444500940131     C     *INZSR        BEGSR
444600030113     C*
444700030113     C* Reperimento parametri
444800030113     C     *ENTRY        PLIST
444900030113     C                   PARM                    KPJBA
445000070228      *
445100080606      *  Controllo se il programma è richiamato  dal MONITOR x il PDA
445200080609     c                   setoff                                       16
445300080606     c                   clear                   PGM_Chiamante
445400080606     C                   clear                   FIDG30ds
445500080606      *
445600080606     C                   IF        %subst(kpjbu:1:10) <> *BLANK
445700080606      *
445800080606     C                   movel     KPJBU         FIDG30ds
445900080606      *
446000080606      * se si deve cambiare nome al MONITOR basta modificarlo solo qui
446100080606     c                   if        G30PGM = 'MONITOR'
446200080606     c                   eval      PGM_Chiamante = 'MONITOR'
446300080606     c                   eval      salta_video1 = 'S'
446400080609     c                   seton                                        16
446500080606      *
446600080606     c                   else
446700080606     c                   eval      PGM_Chiamante = G30PGM
446800080606     c                   end
446900080606     C                   END
447000080606      *
447100070307      *  Se richiamata da Azione/Menù
447200070308     c                   CLEAR                   se_EDP            1
447300070308     c                   if        %subst(Knmus:1:3) = 'EDP'
447400070308     c                   MOVE      'S'           se_EDP
447500070307     c                   end
447600070403      *
447700070403     c     k52j          klist
447800070403     c                   kfld                    wfgs
447900070403     c                   kfld                    wcgi
448000070404      *
448100070404     c     k52j1         klist
448200070404     c                   kfld                    wfgs
448300070404     c                   kfld                    s1CGI
448400070404      *
448500070404     c     karb70        klist
448600070404     c                   kfld                    wfgs_ifp
448700070404     c                   kfld                    h1ndc_meno
448800070307      *
448900070228     c     kgrg          klist
449000070316     c                   kfld                    wfgs
449100070228     c                   kfld                    savARGcgi
449200070301     C*
449300070301     C     Kdst          KLIST
449400070316     C                   KFLD                    wnpg
449500070301     C                   KFLD                    Wnfv
449600070316     C                   KFLD                    wfgs
449700070307     C*
449800070406     C     Kdst8         KLIST
449900070406     C                   KFLD                    wfgs
450000070316     C                   KFLD                    wnpg
450100070307     C                   KFLD                    h1cDDC
450200070301      *
450300070301     C     KTAB1         KLIST
450400070301     C                   KFLD                    tblkut
450500070301     C                   KFLD                    TBLCOD
450600100505     C                   z-add     1             tblkut
450700070307      *
450800070307     C     KTBE          KLIST
450900070307     C                   KFLD                    KCOD1
451000070307     C                   KFLD                    KKEY1
451100070301      *
451200070301     C     Karb1         KLIST
451300070301     C                   KFLD                    s2caas
451400070301     C                   KFLD                    s2clnp
451500070301     C                   KFLD                    s2cnrs
451600070301     C                   KFLD                    s2cnsp
451700150930      *
451800150930     C     Kfiar5        KLIST
451900150930     C                   KFLD                    s2caas
452000150930     C                   KFLD                    s2clnp
452100150930     C                   KFLD                    s2cnrs
452200150930     C                   KFLD                    s2cnsp
452300150930     C                   KFLD                    TRDar5            3
452400070301      *
452500070301     C     KLBL          KLIST
452600070301     C                   KFLD                    lblAAP
452700070301     C                   KFLD                    lblLPP
452800070301     C                   KFLD                    lblNRP
452900070301     C                   KFLD                    lblNSP
453000070301     C*
453100070301     C     KAR6          KLIST
453200070301     C                   KFLD                    arbAAS
453300070301     C                   KFLD                    arbLNP
453400070301     C                   KFLD                    arbNRS
453500070301     C                   KFLD                    arbNSP
453600070301     C                   KFLD                    KTRC
453700070301      *
453800070301     C     KARI          KLIST
453900070301     C                   KFLD                    arbAAS
454000070301     C                   KFLD                    arbLNP
454100070301     C                   KFLD                    arbNRS
454200070301     C                   KFLD                    arbNSP
454300070301     C                   KFLD                    KTIP
454400070316     C*
454500070316     C     Kaggiorna     KLIST
454600070316     C                   KFLD                    s2cAAS
454700070316     C                   KFLD                    s2cLNP
454800070316     C                   KFLD                    s2cNRS
454900070316     C                   KFLD                    s2cNSP
455000070417     C*
455100070417     C     Karg          KLIST
455200070417     C                   KFLD                    arbAAS
455300070417     C                   KFLD                    arbLNP
455400070417     C                   KFLD                    arbNRS
455500070417     C                   KFLD                    arbNSP
455600070305     C*
455700070305     C     Kapd          KLIST
455800070305     C                   KFLD                    tipo_cod          1
455900070305     C                   KFLD                    cod_padr          7 0
456000070301     C*
456100070305     C                   move      'A'           tipo_cod
456200070829     C*
456300070829     c     k_PRM         klist
456400070829     c                   kfld                    k_PRMfgs          3 0
456500070829     c                   kfld                    k_PRMpgm         10
456600070301      *
456700070227      * Reperisco dati job
456800070227     c                   exsr      DatiJob
456900070227
457000030113     C* Variabili per gestione videate
457100070227     C                   MOVE      *BLANK        test_Parallelo    1
457200030113     C                   MOVE      *BLANK        $GEST             2
457300070305     C                   MOVE      *BLANK        sav$GEST          2
457400030113     C                   MOVE      *BLANK        $FINE             1
457500070227     C                   MOVE      *BLANK        $INZD1            1
457600070227     C                   MOVE      *BLANK        $INZW1            1
457700070227     C                   MOVE      *BLANK        $INZS1            1
457800070227     C                   MOVE      *BLANK        $INZS2            1
457900070309     C                   MOVE      *BLANK        $INZS3            1
458000070306     C                   MOVE      *BLANK        $VISS2            1
458100030113     C                   MOVE      *BLANK        $EFILE            1
458200030113     C                   MOVE      *BLANK        $ESCI             1
458300030113     C                   MOVE      *BLANK        $RCDOK            1
458400030113     C* Indici
458500030113     C                   Z-ADD     0             X                 3 0
458600030113     C                   Z-ADD     0             Y                 3 0
458700070227     C***
458800070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
458900070227     C***
459000070227     C                   TIME                    W0140            14 0
459100070227     C                   MOVE      W0140         UDATE8            8 0
459200070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
459300070319     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
459400070227     C*
459500070227     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
459600070227     C                   MOVEL     *BLANK        G02ERR
459700070227     C                   CALL      'XSRDA8'
459800070227     C                   PARM                    WLBDAT
459900070301     c                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
460000070307     C*
460100070307     C*  Aggancio tabella 'DGD' x reperire LIMITI CONTROLLO DISTINTA
460200070307     C                   MOVEL     'DGD'         KCOD1
460300070307     C                   MOVEL     *BLANKS       KKEY1
460400070307     C                   MOVEL     '1'           KKEY1
460500070307     C     KTBE          CHAIN     TNTBE01L
460600070307     C                   If        %Found(TNTBE01L)
460700070307     C                   MOVEL     TBEUNI        DDGD
460800070307     C                   END
460900160317     C*  VEDI PACCO DPD
461000160317     C                   clear                   GIRO_vd_Pacco
461100160317     C                   MOVEL     'NSD'         KCOD1
461200160317     C                   MOVEL     *BLANKS       KKEY1
461300160317     C                   MOVEL     '1'           KKEY1
461400160317     C     KTBE          CHAIN     TNTBE01L
461500160317     C                   If        %Found(TNTBE01L)
461600160317     C                   MOVEL     TBEUNI        DNSD
461700160317     C                   MOVEL     §nsdCGI       GIRO_vd_Pacco
461800160317     C                   END
461900100505     C*
462000070301     C*  CARICO DATI TABELLA 1P
462100070301     C                   MOVEL     '1P'          TBLCOD
462200070301     C                   Z-ADD     0             X
462300070301     C     KTAB1         setll     TABEL00F
462400070301     C     KTAB1         reade     TABEL00F
462500070302     C                   DOW       not %Eof(TABEL00F)  and (x < 50)
462600070301     C                   ADD       1             X
462700070301     C                   MOVEL     TBLUNI        DS1P
462800070301     C                   MOVEL     TBLKEY        C1P(X)
462900070301     C                   MOVEL     §1PDES        D1P(X)
463000070301     C     KTAB1         reade     TABEL00F
463100070301     C                   END
463200070301     C*
463300070301     C*  CARICO DATI TABELLA 3A
463400070301     C                   MOVEL     '3A'          TBLCOD
463500070301     C                   Z-ADD     0             X
463600070301     C     KTAB1         setll     TABEL00F
463700070301     C     KTAB1         reade     TABEL00F
463800070302     C                   DOW       not %Eof(TABEL00F) and (x < 100)
463900070301     C                   ADD       1             X
464000070301     C                   MOVEL     TBLKEY        C3A(X)
464100070301     C                   MOVEL     TBLUNI        D3A(X)
464200070301     C     KTAB1         reade     TABEL00F
464300070301     C                   END
464400940127     C*
464500100518     C*  CARICO DATI TABELLA 7r part. consegne attesa e Ritiri Contestuali
464600100505     C                   MOVEL     '7R'          TBLCOD
464700100505     C                   Z-ADD     0             X
464800100518     C                   Z-ADD     0             wRC               3 0
464900100505     C     KTAB1         setll     TABEL00F
465000100505     C     KTAB1         reade     TABEL00F
465100100505     C                   DOW       not %Eof(TABEL00F) and (x < 100)
465200100505     c                   movel     tbluni        ds7r
465300100518      *
465400100518      *  carico i codici che hanno la modalità di Attesa
465500100505     c                   if        §7ratt = 'S'
465600100505     C                   ADD       1             X
465700100505     C                   MOVEL     TBLKEY        s7r(X)
465800100505     c                   end
465900100518      *
466000100518      *  carico i codici che hanno il Ritiro Contestuale
466100100518     c                   if        §7rRC  = 'S'
466200100518     C                   ADD       1             wRC
466300100518     C                   MOVEL     TBLKEY        s7rRC(wRC)
466400100518     c                   end
466500100518      *
466600100505     C     KTAB1         READE     TABEL00F
466700100505     C                   END
466800140929     C*
466900940117     C                   ENDSR
467000070227      *---------------------------------------------------------------*
467100070320      *?  Reperimento Dati del job (Utente/Operativi)                ?
467200070227      *---------------------------------------------------------------*
467300070228     c     DatiJOB       BEGSR
467400070227      *
467500070227     c     *dtaara       define    §azute        azuteds
467600070227     c     *dtaara       define    §datiute      ddatiute
467700070227      *
467800070227     c                   in(E)     *dtaara
467900070227     c                   IF        %ERROR or RSUT = *blanks
468000070227     c                   clear                   Tibs34Ds
468100070227     c                   call      'TIBS34R'
468200070227     c                   parm                    Tibs34Ds
468300070227     c                   in        *dtaara
468400070227     c                   ENDIF
468500080606      *
468600070227     c                   ENDSR
468700070301     C*---------------------------------------------------------------*
468800070320      *?  Controlla validità della spedizione                        ?
468900070301     C*---------------------------------------------------------------*
469000070301     C     Check_Sped    BEGSR
469100070301     C*
469200070301      * controlla che il record sia giusto da prendere
469300070301     c* chiamo pgm per vedere se spedizione da mettere in distinta
469400070301     c                   clear                   fidg16ds
469500070301     c                   eval      d16aasi = arbaas
469600070301     c                   eval      d16lnpi = arblnp
469700070301     c                   eval      d16nrsi = arbnrs
469800070301     c                   eval      d16nspi = arbnsp
469900150108     c*                  eval      d16consi= 'S'
470000070308      * deve essere impostata la data
470100070308     c                   eval      d16dfvi = h1cDDC
470200070829     c                   eval      d16dtaI = h1dtaI
470300070829     c                   eval      d16oraI = d1oraI
470400071210     c                   eval      d16LDCRI= h1dcsr
470500130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
470600130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
470700130221     c*******            if        arbxco <> ' '
470800130221     c                   if        arbxco > '0'
470900110727     c                   eval      d16prfoI= 'S'
471000110727     c                   end
471100070416     c                   if        test_Parallelo = 'S'
471200070416     c                   eval      d16ndci = 'G'
471300070416     c                   end
471400150108     c* se sto assegnando una spedizione presente nel giro DIR
471500150108     c* inibisco alcuni controlli di
471600150108     c* assegnabilità in quanto inutili visto che sto dirottando
471700150108     c* es. la sospensione alla consegna, la data consegna richiesta ecc.)
471800150108     c* questo lo faccio abilitando i singoli flag e non
471900150108     c* quello totale
472000150108     c                   if        argcgi = 'DIR'
472100150108     c                   eval      d16dcmi = 'S'
472200150108     c                   eval      d16fbci = 'S'
472300150108     c                   eval      d16cmci = 'S'
472400150108     c                   eval      d16lblmi= 'S'
472500150108     c                   eval      d16colci= 'S'
472600150108     c                   eval      d16parasgi='S'
472700150108     c                   else
472800150108     c                   eval      d16consi= 'S'
472900150108     c                   end
473000070301     c                   eval      kpjbus = kpjbu
473100070301     c                   eval      kpjbu = fidg16ds
473200070301     c                   call      'FIDG16R'
473300070301     c                   parm                    kpjba
473400070301     c                   parm                    fnarbds
473500070301     c                   parm                    fiargds
473600070301     c                   eval      fidg16ds = kpjbu
473700070301     c                   eval      kpjbu = kpjbus
473800070301     C*
473900070301     C                   ENDSR
474000070305     C*---------------------------------------------------------------*
474100070320      *?  Calcola Pesi Volumi e Colli da consegnare                  ?
474200070305     C*---------------------------------------------------------------*
474300070305     C     Cal_PesVol    BEGSR
474400070305     C*
474500070305     c                   eval      kpjbus = kpjbu
474600070323     c                   clear                   fidg27ds
474700070305     c                   movel     fnarbds       fnarbdsav
474800070323     c                   z-add     wddc          D27DFVI
474900070323     c                   z-add     arbAAS        d27aasI
475000070305     c                   z-add     arbLNP        d27lnpI
475100070305     c                   z-add     arbNRS        d27nrsI
475200070305     c                   z-add     arbNSP        d27nspI
475300070829     c                   z-add     h1dtaI        d27dtaI
475400070829     c                   z-add     d1oraI        d27oraI
475500070305     c                   eval      kpjbu = fidg27ds
475600070305     c                   call      'FIDG27R'
475700070305     c                   parm                    kpjba
475800070305     c                   parm                    fnarbdsav
475900070305     c                   eval      fidg27ds = kpjbu
476000070305     c                   eval      kpjbu = kpjbus
476100070305     C*
476200070305     C                   ENDSR
476300070309      *---------------------------------------------------------------*
476400070319      *     GESTIONE SFL di AGGIORNAMENTO e segnalazione problemi    ?
476500070309      *---------------------------------------------------------------*
476600070309     C     GESS3         BEGSR
476700070309     C*
476800070309     C* inizializzazione videata
476900070309     C     $INZS3        IFEQ      *ON
477000070309     C                   EXSR      INZS3
477100070309     C                   MOVE      *OFF          $INZS3
477200070309     C                   ENDIF
477300070309     C*
477400070319      *     FASE di COMMIT o ROLLBACK    ?
477500070319     c                   if        exec_RB = 'S'
477600070320      *  Esegue il ROLLBACK x un precedente ?
477700070320      *  problema su uno degli aggiornamenti  ?
477800070319     c                   ROLBK
477900070319     C                   else
478000070320      *   Esegue il COMMIT  --> TUTTO è OK. ?
478100070319     c                   COMMIT
478200080626      *
478300080626      *  Aggiornamento Archivi x PDA fuori COMMIT
478400080626     c                   exsr      aggiorna_x_PDA
478500080626      *
478600070319     C                   end
478700070319     C*
478800070309     C* Inizializza e imposta piede videata
478900070309     C                   EXSR      INZZ3
479000070309     C*
479100070309     C* emissione piede videata
479200070309      * ?            *-----------------------*
479300070309     C                   WRITE     FDG0Z03
479400070309      * ?            *-----------------------*
479500070309     C*
479600070309     C     WMAX          IFEQ      0
479700070319     c                   if        exec_RB = 'N'
479800070319     C* Se è stato aggiornato tutto correttamente deve Emettere
479900070319     C*  il messaggio di TUTTO OK.
480000070607     c                   eval      vedi_video = 'N'
480100070326     c                   eval      V03MSG = ErrMsg(25)
480200070319     c                   else
480300070319     C*  Qualcosa è andato storto
480400070607     c                   eval      vedi_video = 'S'
480500070326     c                   eval      V03MSG = ErrMsg(26)
480600070319     c                   end
480700070330      **
480800071122     c                   goto      nonVedere
480900070309      * ?            *-----------------------*
481000070309     C                   WRITE     FDG0V03
481100070309      * ?            *-----------------------*
481200071122     c     nonVedere     tag
481300070309     c                   setoff                                       30
481400070309     C                   Else
481500070309     c                   seton                                        30
481600070309      *
481700070309     C     Wsfl          IFgt      0
481800070309     C                   Z-ADD     wsfl          C3RCD
481900070309     C                   Else
482000070309     C     Wpag          IFgt      0
482100070309     C                   Z-ADD     wpag          C3RCD
482200070309     C                   EndIF
482300070309     C                   EndIF
482400070309     C                   ENDIF
482500070309      *
482600070309      *  Emissione SFL dopo Aggiornamento Data Base
482700070319      *    con errori quindi tutta la transazione deve essere rifatta.
482800070607     c                   if        vedi_video = 'S'
482900070309      * ?            *-----------------------*
483000070309     C                   EXFMT     FDG0C03
483100070309      * ?            *-----------------------*
483200070330     c                   end
483300070326     c                   setoff                                       99
483400070309      *
483500070309     C     C3NRR         IFNE      0
483600070309     C                   Z-ADD     C3NRR         WSFL
483700070309     C                   ENDIF
483800070309     C                   Z-ADD     SFLNRR        C3RCD
483900070309     C* Selezioni
4840000703091    C                   SELECT
484100070309     C* F3=Fine
484200070309     C     $TASTO        WHENEQ    F03
484300070309     C                   EXSR      F03_exit
484400070309      *
4845000703091O   C                   OTHER
484600070309     C* CONTROLLO testata
484700070309     C                   EXSR      CTRC3
484800070309     C* CONTROLLO subfile
484900070309     C     *IN99         IFEQ      *OFF
485000070309     C                   EXSR      CTRS3
485100070309     C                   END
485200070309      *
485300070309      * Riemette il primo video
485400070309     C                   movel     'D1'          $GEST
485500070309      *   non reinizializzato se ci sono stati problemi
485600070309     C     WMAX          ifgt      0
485700070309     C                   MOVE      *OFF          $INZD1
485800070316     c                   rolbk
485900070316      *
486000070309     c                   else
486100070309      *   reinizializzato se non ci sono stati problemi
486200070309     C                   MOVE      *ON           $INZD1
486300070309     c                   end
486400070330      *?  Attenzione?
486500070330      *?    se si era scelto di Assegnare si deve ritornare sul SFL dei GIRI
486600070504      *?    dove era impostata la scelta e pulire la distinta x riselezionarla.
486700070531     c                   if        d1sce = Assegnare
486800070330     C                   movel     'S1'          $GEST
486900070413     C                   MOVE      *ON           $INZS1
487000070504     c                   clear                   h1ndc_piu
487100070330     c                   eval      WSfl  =  sav_WSfl
487200070330     c                   eval      Wmax  =  sav_Wmax
487300070330     c                   eval      Wpag  =  sav_Wpag
487400070404      * avendo assegnato l'unico record non posso riemettere il SFL vuoto
487500070404      *   quindi torno al video introduttivo.
487600070404     c                   if        Wmax  = 1
487700070404     C                   movel     'D1'          $GEST
487800070404     C                   MOVE      *ON           $INZD1
487900070404     c                   end
488000070607      *
488100070607     c                   eLSe
488200070607      *  se togliere o trasferire
488300070607      *   voglio riportare direttamente sull'interrogazione deistinte
488400070607     c                   eval      salva_d1sce = d1sce
488500070607     c                   eval      salta_video1 = 'S'
488600070607     C                   movel     'D1'          $GEST
488700070607     C                   MOVE      *Off          $INZD1
488800070607     c                   exsr      INZD1
488900070607     c                   eval      d1sce = salva_d1sce
489000070607      *
489100070607     c                   end
489200070607      *
4893000703091-   C                   ENDSL
489400070309     C*
489500070309     C                   ENDSR
489600070309     C/EJECT
489700070309      *---------------------------------------------------------------*
489800070320      *?  INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                ?
489900070309      *---------------------------------------------------------------*
490000070309     C     INZZ3         BEGSR
490100070309     C*
490200070309     C* Messaggio o tasti funzionali
490300070309     c                   clear                   z3dmsg
490400070309     c                   clear                   z3der1
490500070309     c                   clear                   z3der2
490600070309     C*
490700070309     C* se ci sono dei records allora ci sono stati dei problemi
490800070319     C*  durante l'aggiornamento bolla x bolla
490900070309     C     WMAX          IFGT      0
491000070309     C*
491100070309     c                   seton                                        51
491200070326     c                   movel     ErrMsg(21)    z3der1
491300070326     c                   movel     ErrMsg(22)    z3der2
491400070309     C*
491500070319     c                   ELSE
491600070319     C* altrimenti
491700070319     C*  Se ci sono stati dei problemi durante gli aggiornamenti
491800070319     C*   o su ARB o su DST tanto da eseguire comunque il ROLLBACK
491900070319     C*    imposta un altro tipo di messaggio.
492000070319     c                   if        exec_RB = 'S'
492100070319     c                   seton                                        51
492200070326     c                   movel     ErrMsg(23)    z3der1
492300070326     c                   movel     ErrMsg(24)    z3der2
492400070319     c                   end
492500070319     C*
492600070309     c                   END
492700070309     C*
492800070309      * tasti funzionali se non è stato emesso alcun messaggio
492900070309     c                   if        z3dmsg = *blank
493000070309     c                   setoff                                       99
493100070309     c                   movel     Fun_3         z3dmsg
493200070309     c                   eval      %subst(z3dmsg:40:13) = FUN_9_Drop
493300070309     c                   end
493400070309     C*
493500070309     C                   ENDSR
493600070309     C/EJECT
493700070309      *---------------------------------------------------------------*
493800070320      *?  INIZIALIZZAZIONE LISTA Bolle da Giri o da Distinta         ?
493900070309      *---------------------------------------------------------------*
494000070309     C     INZS3         BEGSR
494100070309     C*
494200070309     C* reset indicatori DSPATR
494300070309     c                   exsr      INZ_indERR
494400070309     C* pulizia SFL
494500070309     C                   SETOFF                                         3031
494600070309     C                   WRITE     FDG0C03
494700070309     C                   SETON                                          31
494800070309     C*
494900070309     C* Righe x Pagina del 1° SFL
495000070427     c                   z-add     RigPag_SFl3   s3pag
495100070309      *
495200070309     C* CARICAMENTO SFL totale
495300070309     C                   Z-ADD     0             S3NRR
495400070309     C                   Z-ADD     1             C3RCD
495500070309     C                   Z-ADD     0             WMAX
495600070309     C                   Z-ADD     0             Wsfl
495700070309     C* campi titolo
495800070309     C                   MOVEL     dsPGM         c3CPGM
495900070309     C                   MOVEL     knsif         c3nsif
496000070309     C                   MOVEL     knmus         c3nmus
496100070309     C                   MOVEL     RSUT          c3crsu
496200070309      * TIPO FUNZIONE
496300070309     C                   SELECT
496400070531     C                   when      d1sce = Assegnare
496500070309     c                   eval      C3dsce = OPZ_1
496600070309     c                   eval      C3dess = 'alla Distinta nr.' +
496700070319     c                             %trim(%editc(h1ndc_piu:'Z'))
496800070309      *
496900070531     C                   when      d1sce = Togliere
497000070309     c                   eval      C3dsce = OPZ_2
497100070309     c                   eval      C3dess = 'dalla Distinta nr.' +
497200070319     c                             %trim(%editc(h1ndc_meno:'Z'))
497300070309      *
497400070531     C                   when      d1sce = Trasferire
497500070309     c                   eval      C3dsce = OPZ_3
497600070309     c                   eval      C3dess = 'dalla Distinta nr.' +
497700070319     c                             %trim(%editc(h1ndc_meno:'Z'))
497800070309      *
497900070309     c                   endSL
498000070309      *
498100070309      *   Carica il SFL
498200070309     c                   EXSR      ROLS3
498300070309     C*
498400070309     C                   Z-ADD     1             WPAG
498500070309     C*
498600070309     C                   ENDSR
498700070309      *---------------------------------------------------------------*
498800070320      *? Esegue aggiornamento del Data Base caricando il SFL3 x problemi ?
498900070309      *---------------------------------------------------------------*
499000070309     C     ROLS3         BEGSR
499100070309     C*
499200070309     C                   SETOFF                                       32
499300070309     C                   Z-ADD     0             Y
499400070309     C                   Z-ADD     WMAX          S3NRR
499500070309     C*
499600070319      *? L'aggiornamento è pilotato dalle scelte effettuate sul SFL2 ?
499700070319      *? delle bolle e il caricamento del SFL3 serve per elencare    ?
499800070319      *? le bolle che non sono state aggiornate x qualsiasi motivo   ?
499900070319      *? classico: ALLOCAZIONE DEL RECORD.                           ?
500000070319     C*
500100070319     c                   exsr      AGG_da_SFL2
500200070319      *
500300070319      *  Dopo aver aggiornato i files x singola bolla, aggiorna ?
500400070319      *   a livello di distinta. Sempre che già non sia da      ?
500500070319      *   eseguire il ROLL_BACK x precedenti problemi.          ?
500600070319     C*
500700070319     c                   if        Exec_RB <> 'S'
500800070319     c                   exsr      Agg_x_Distinta
500900070319     c                   end
501000070319      *
501100070309     C* Fine Caricamento
501200070309     C                   Z-ADD     S3NRR         WMAX                 30
501300070309     C*
501400070309     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
501500070309     C     S3NRR         DIV       S3pag         PAGINE            4 0
501600070309     C                   MVR                     RESTO             3 0
501700070309     C     PAGINE        MULT      S3pag         C3RCD
5018000703091    C     RESTO         IFGT      0
501900070309     C                   ADD       1             C3RCD
5020000703091E   C                   ELSE
502100070309     C                   SUB       S3pag         C3RCD
502200070309     C                   ADD       1             C3RCD
5023000703091-   C                   ENDIF
502400070309     C*
502500070309     C                   ENDSR
502600070309      *---------------------------------------------------------------*
502700070320      *? Carica il SFL delle bolle non aggiornate se ci sono stati problemi?
502800070309      *---------------------------------------------------------------*
502900070319     C     AGG_da_SFL2   BEGSR
503000070309     C*
503100070319     C*  Si ricorda di eseguire il ROLL_BACK se si sono verificati dei
503200070319     C*   problemi in qualsiasi parte dell'aggiornamento del D.B.
503300070320     c                   move      'N'           Exec_RB           1
503400070319     C*
503500080609     C*   Legge il SFL delle Bolle dove selezionato con (1) le righe
503600080609     C*    da aggiornare
503700070531     c                   Readc     FDG0S02                                21
503800070531     C*
5039000703092    C     *IN21         DOWEQ     *OFF
504000070531     C*
504100070531     C* Solo i selezionati
504200070531     c                   if        s2SCE = '1'
504300070531     c                   move      'S'           Riga_a_1          1
504400070309      * ? ----------------------------
504500070309      * ? Carico il SFL solo se ci sono dei problemi in aggiornamento delle bolle
504600070309      * ?  per allocazione dei records o altro .
504700070309      * ? ----------------------------
504800070309     c                   clear                   problemi          1
504900070309     C*
505000070309      * ?             *--------------------------*
505100070319     c                   exsr      AGG_Riga_DB
505200070309      * ?             *--------------------------*
505300070309     C*
505400070309     C* x Problemi sui records in aggiornamento
505500070309      * ? ----------------------------
505600070309     c                   if        Problemi = 'S'
505700070319     c                   move      'S'           Exec_RB           1
505800070319      *
505900070309     C                   clear                   FDG0S03
506000070309     c                   exsr      RIGA_SFL3
506100070309      *
506200070309      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
506300070309     c                   seton                                        32
506400130221     c     h3xco         comp      '0'                                88
506500070309     C                   ADD       1             S3NRR
506600070309     C                   ADD       1             Y
506700070309     C                   WRITE     FDG0S03
506800070309     c                   End
506900070309     C*
507000070309     c                   End
507100070309     C*
507200070531     c                   Readc     FDG0S02                                21
507300070309     c                   EndDo
507400070309     C*
507500070309     C                   ENDSR
507600070309      *---------------------------------------------------------------*
507700070320      *?  Imposta Campi del 3°SFL                                    ?
507800070309      *---------------------------------------------------------------*
507900070309     C     RIGA_SFL3     BEGSR
508000070309     C*
508100070309     C* Deve semplicemente impostare dal 2° al 3° SFL i campi
508200070309     c                   z-add     S2Caas        S3Caas
508300070309     c                   z-add     S2Clnp        S3Clnp
508400070309     c                   z-add     S2Cnrs        S3Cnrs
508500070309     c                   z-add     S2Cnsp        S3Cnsp
508600110804     c                   movel     h2xco         h3xco
508700070309     c                   movel     S2Ccbo        S3Ccbo
508800070309     C                   z-add     S2Cgmc        S3Cgmc
508900070309     c                   z-add     S2Clna        S3Clna
509000070309     c                   z-add     S2Cznc        S3Cznc
509100070427     c                   z-add     S2pkf         S3Cpkf
509200070309     c                   z-add     S2Cvlf        S3Cvlf
509300070309     c                   z-add     S2Cncl        S3Cncl
509400070309     C                   MOVEL     S2Dano        S3Dano
509500070309     c                   movel     S2Crsm        S3Crsm
509600070309     c                   movel     s2CGI         s3CGI
509700070309     c                   movel     S2Crsd        S3Crsd
509800070309     c                   movel     S2Cind        S3Cind
509900070309     c                   movel     S2Ccad        S3Ccad
510000070309     c                   movel     S2Clod        S3Clod
510100070309     c                   movel     S2Cprd        S3Cprd
510200070309     C*
510300070309     C                   ENDSR
510400070309      *---------------------------------------------------------------*
510500070320      *?  CONTROLLO TESTATA LISTA                                    ?
510600070309      *---------------------------------------------------------------*
510700070309     C     CTRC3         BEGSR
510800070309     C                   MOVE      *OFF          *IN99
510900070309     C                   ENDSR
511000070309      *---------------------------------------------------------------*
511100070320      *?  CONTROLLO OPZIONI LISTA                                    ?
511200070309      *---------------------------------------------------------------*
511300070309     C     CTRS3         BEGSR
511400070309     C                   SETOFF                                       99
511500070309     C* reset indicatori DSPATR
511600070309     c                   exsr      INZ_indERR
511700070309     C                   ENDSR
511800070309     C/EJECT
511900070309      *---------------------------------------------------------------*
512000080626      * ??  Aggiornamento dei Files a livello BOLLA                  ?
512100070309      *---------------------------------------------------------------*
512200070319     C     AGG_Riga_DB   BEGSR
512300070531     c*
512400070531     c*  Bolla x Bolla ossia riga x riga
512500070531     c                   exsr      Aggiorna_arG
512600070531      *
512700070531     c                   exsr      Aggiorna_arB
512800070319     c*
512900070319     C                   ENDSR
513000070319     C/EJECT
513100070319      *---------------------------------------------------------------*
513200080626      * ??  Aggiornamento dei Files   a livello Distinta             ?
513300070319      *---------------------------------------------------------------*
513400070531     C     Aggiorna_arG  Begsr
513500070319     c*
513600070531      *   Aggiorna ARG x Bolla    ?
513700070531     c     kaggiorna     chain(e)  fiarg01l
513800070531     c*
513900070531      *  ?  Se ci sono problemi di allocazione record    ?
514000070531     c                   if        %Error
514100070531      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
514200070531     c                   eval      problemi = 'S'
514300070531     c                   clear                   trul82ds
514400070531     c                   eval      ul82§rrn = arg_nrr
514500070531     c                   eval      ul82§fil = 'FIARG01L'
514600070531     c                   eval      ul82§win = 'S'
514700070531     c                   eval      ul82§f7  = 'S'
514800070531     c                   eval      ul82§num = 2
514900070531     c                   eval      ul82§att = 2
515000070531     c                   eval      ul82§mss = ErrMsg(27)
515100070531     c                   Eval      UL82§msw = 'La Distinta '
515200070531     c                             + %editc(w1ndc1:'Z') +  ' '
515300070531     c                             + ' non è manutenzionabile.'
515400070531      * chiamo il pgm che manda il messaggio info all'utente
515500070531     c                   call(e)   'TRUL82R'
515600070531     c                   parm                    trul82ds
515700070531      * ancora allocata
515800070531     c                   if        ul82§sts = 'A'
515900070531      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
516000070531     c                   eval      problemi = 'S'
516100070531     c                   endif
516200070531     c*
516300070531     c                   else
516400070531     c*
516500070531     c                   if        %Found(fiarg01l)
516600070531      * ? Imposta i campi da aggiornare ?
516700070531     c                   select
516800070531     c*
516900070531     c* solo x righe selezionate
517000070531     c                   when      d1sce = Assegnare or d1sce = Trasferire
517100070531     c                   eval      ARGFGS     =  wfgs
517200070531     c                   eval      ARGPDC     =  h1pdr_piu
517300070531     c                   eval      ARGNDC     =  h1ndc_piu
517400120209     c*********          eval      ARGDDC     =  h1cddc
517500120209     c                   eval      argDDC     =  h1ddc_piu
517600070531     c                   eval      ARGSTP     =  0
517700070531     c                   eval      ARGNFTL    =  99999
517800070531     c                   eval      ARGSLB     =  0
517900070531     c                   eval      ARGDTVDIS  = Dateu8
518000070531     c                   eval      ARGHVDIS   = HHMMss
518100070531     c*
518200070531     c* solo x righe selezionate
518300070531     c                   when      d1sce = Togliere
518400070531     c                   clear                   ARGPDC
518500070531     c                   clear                   ARGFGS
518600070531     c                   clear                   ARGNDC
518700070531     c                   clear                   ARGDDC
518800070531     c                   clear                   ARGSTP
518900070531     c                   clear                   ARGNFTL
519000070531     c                   clear                   ARGSLB
519100070531     c                   clear                   ARGDTVDIS
519200070531     c                   clear                   ARGHVDIS
519300100505      *
519400100505      * Comunque aggiorna il flag pulendolo
519500100505      *  anche se si tratta di una bolla con la particolarità di attesa
519600100505      *  poichè in ogni caso è stato preso in cosiderazione e selezionato su questo pgm
519700100505     c                   movel     argflo        dargflo
519800100505      * in attesa
519900100505     c                   clear                   §argattesa
520000100505     c                   movel     dargflo       argflo
520100100505     c*
520200070531     c                   endsl
520300070531     c*
520400070531     c                   update    fiarg000
520500070531     c                   end
520600070531     c*
520700070531     c                   end
520800070531     c*
520900070531     C                   ENDSR
521000070531     C/EJECT
521100070531      *---------------------------------------------------------------*
521200080626      * ??  Aggiornamento dei Files   a livello Distinta             ?
521300070531      *---------------------------------------------------------------*
521400070531     C     Aggiorna_arB  Begsr
521500070531     c*
521600070531      *   Deve aggiornare  anche  ARB solo se  siamo in produzione  ?
521700070531     c                   if        test_Parallelo <> 'S'  and
521800070531     c                                   problemi <> 'S'
521900070531      * solo
522000070531      * se deve togliere
522100070531     c                   if        (d1sce = Togliere or d1sce = Trasferire) and
522200070531     c                             h1ndc_meno <> 0
522300070531      *
522400070531     c     kaggiorna     chain(e)  fnarb01l
522500070531     c*
522600070531      *  ?  Se ci sono problemi di allocazione record    ?
522700070531     c                   if        %Error
522800070531      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
522900070531     c                   eval      problemi = 'S'
523000070531     c*
523100070531     c                   clear                   trul82ds
523200070531     c                   eval      ul82§rrn = arb_nrr
523300070531     c                   eval      ul82§fil = 'FNARB01L'
523400070531     c                   eval      ul82§win = 'S'
523500070531     c                   eval      ul82§f7  = 'S'
523600070531     c                   eval      ul82§num = 2
523700070531     c                   eval      ul82§att = 2
523800070531     c                   eval      ul82§mss = ErrMsg(27)
523900070531     c                   Eval      UL82§msw = 'La Distinta '
524000070531     c                             + %editc(w1ndc1:'Z') +  ' '
524100070531     c                             + ' non è manutenzionabile.'
524200070531      * chiamo il pgm che manda il messaggio info all'utente
524300070531     c                   call(e)   'TRUL82R'
524400070531     c                   parm                    trul82ds
524500070531      * ancora allocata
524600070531     c                   if        ul82§sts = 'A'
524700070531      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
524800070531     c                   eval      problemi = 'S'
524900070531     c                   endif
525000070531     c*
525100070531     c                   else
525200070531     c*
525300070531     c                   if        %Found(fnarb01l)
525400140714      **
525500140714      ** salva la chiave della distinta/autista prima di toglierla da ARB
525600140714     c                   z-add     arbNDC        sav_arbndc
525700140714     c                   z-add     arbDDC        sav_arbddc
525800140714     c                   z-add     arbPDC        sav_arbpdc
525900080609      **
526000080609      * ?Se attivato come CONSEGNE con PDA:
526100080626     c                   if          PDAconsegne <> *blank
526200080609      **
526300080609     c                   endIf
526400080609      **
526500070531     c                   eval      arbngd = 0
526600070531     c                   eval      arbndc = 0
526700070531     c                   eval      arbpdc = 0
526800070531     c                   eval      arbifp = 0
526900070531     c                   eval      arbddc = 0
527000070531     c                   eval      arbstp = 0
527100070531     c                   eval      arbngd = 0
527200070531     c                   eval      arbnss = 0
527300070531     c                   update    fnarb000
527400140711      **
527500140711      **  Imposta l'evento "NIC" se era stato emesso un "MIC" sulla spedizione
527600140714     c                   exsr      Aggio_EVB_NIC
527700071008      **
527800071008     c* ritiro contestuale alla consegna:
527900071008      **  se è stata annullata la consegna deve annullare anche il ritiro
528000071008      **  che era stato ordinato contestualmente alla consegna.
528100071008     c                   exsr      Aggiorna_ORM
528200070531     c                   end
528300070531     c*
528400070531     c                   end
528500070531      *
528600070531     c                   end
528700070531     c                   end
528800070531     c*
528900070531     C                   ENDSR
529000070531     C/EJECT
529100140711      *---------------------------------------------------------------*
529200140711      * ??  imposta l'Evento di NIC sulla spedizione che aveva MIC   ?
529300140711      *---------------------------------------------------------------*
529400140714     C     Aggio_EVB_NIC BEGSR
529500140711      ***
529600140711      ***   richiama la routine che si occupa di impostare il NIC
529700140711     c                   clear                   fnlrq8ds
529800140711     c                   eval      kpjbus = kpjbu
529900140711     c                   eval      LRQ8CMTI =  'S'
530000140711     c                   eval      LRQ8AASI =  arbaas
530100140711     c                   eval      LRQ8LNPI =  arblnp
530200140711     c                   eval      LRQ8NRSI =  arbnrs
530300140711     c                   eval      LRQ8NSPI =  arbnsp
530400140711     c                   eval      LRQ8FGSI =  d1cFGS
530500140714     c                   eval      LRQ8NDCI =  sav_arbndc
530600140714     c                   eval      LRQ8DDCI =  sav_arbddc
530700140714     c                   eval      LRQ8PDCI =  sav_arbpdc
530800140711     c                   eval      LRQ8NPGI =  4
530900140711     c                   eval      LRQ8FPPI =  h1fpp_meno
531000140714     c                   eval      LRQ8MICI =  ' '
531100140711     c                   eval      LRQ8TLAI =  'L'
531200140711     c                   eval      kpjbu = fnlrq8ds
531300140711     c                   call      'FNLRQ8R'
531400140711     c                   parm                    kpjba
531500140711     c                   eval      fnlrq8ds = kpjbu
531600140711     c                   if        LRQ8ESIO <> *blanks
531700140711      * ?  Messaggio di ERRORE                   MSG   ?
531800140711      **         cosa si deve fare ????? se errore
531900140711      **
532000140711     c                   end
532100140711     c                   eval      kpjbu = kpjbus
532200140711     c*
532300140711      ***
532400140711     C                   ENDSR
532500140711     C/EJECT
532600140711      *---------------------------------------------------------------*
532700071008      *---------------------------------------------------------------*
532800071008      * ??  toglie sull'orm x particolarità RC                       ?
532900071008      *---------------------------------------------------------------*
533000071008     C     Aggiorna_ORM  BEGSR
533100071008     c*
533200071008     c*  Se l'autista va a consegnare dal cliente deve anche Ritirare qualcosa
533300071008     c*  se ha il ritiro contestuale alla consegna.
533400100518     c********           if        arbgma = 'RC'
533500100518      ***
533600100518      *****   Adesso deve testare se il tipo consegna particolare
533700100518      *****     è inerente al ritiro contestuale.
533800100518      ***
533900100518     C     arbgma        lookup    s7rRC                                  23
534000100518     C                   IF        *in23
534100071008     c                   clear                   fnlr99ds
534200071008     c                   eval      i99tla = 'L'
534300071008     c                   eval      i99aas = arbaas
534400071008     c                   eval      i99lnp = arblnp
534500071008     c                   eval      i99nrs = arbnrs
534600071008     c                   eval      i99nsp = arbnsp
534700071008     c                   eval      i99fgs = d1cFGS
534800071008     c                   eval      i99ndc = arbndc
534900071008     c                   eval      i99ddc = arbddc
535000071008     c                   eval      i99comit ='1'
535100071008     c                   eval      i99cmt ='N'
535200071008     c                   eval      i99fao = 390
535300071008     c                   call      'FNLR99R'
535400071008     c                   parm                    kpjba
535500071008     c                   parm                    fnlr99ds
535600071008     c                   end
535700071008     c*
535800071008     C                   ENDSR
535900071008     C/EJECT
536000070531      *---------------------------------------------------------------*
536100080626      * ??  Aggiornamento dei Files   a livello Distinta             ?
536200070531      *---------------------------------------------------------------*
536300070531     C     AGG_x_DistintaBEGSR
536400070531     c*
536500070320      *   Deve aggiornare  anche  ARB solo se  siamo in produzione  ?
536600070319      * ? Se deve Assegnare: ?
536700070319      * ? AGGANCIA ARB  mediante il FIDG23R ma a livello di distinta ?
536800070319     c                   IF        test_Parallelo <> 'S'  and Exec_RB <> 'S'
536900070319      * se deve ASSEGNARE
537000070319     c                   if        h1ndc_piu <> 0
537100070319     c                   clear                   fidg23ds
537200070319     c                   eval      d23CMTI = 'N'
537300070319     c                   eval      d23FGSI = wFGS
537400070319     c                   eval      d23NFTL = 99999
537500120209     c******             eval      d23DFVI = h1cddc
537600120209     c                   eval      d23DFVI = h1ddc_piu
537700070319     c                   eval      d23NFDI = h1ndc_piu
537800070319     c                   eval      d23NFAI = h1ndc_piu
537900070319     c                   eval      d23FPPI = h1FPP_piu
538000070319     c                   eval      kpjbus = kpjbu
538100070319     c                   eval      kpjbu = fidg23ds
538200070531     c                   call      'FIDG23C'
538300070319     c                   parm                    kpjba
538400070319     c                   eval      fidg23ds = kpjbu
538500070319     c                   if        d23erro <> *blanks
538600070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
538700070319     c                   eval      Exec_RB  = 'S'
538800070319     c                   end
538900070319     c                   eval      kpjbu = kpjbus
539000070319     c                   end
539100070316     c*
539200070319     c                   endIF
539300070319     c*
539400070320      *   Deve aggiornare la o le Distinte   ?
539500070319     c                   IF        Exec_RB  <> 'S'
539600070319      *
539700070319      *   richiamo in aggiornamento D.B.     ?
539800070319     c                   move      w1ndc1        wNFV
539900070319      *                         ?  =================  ?
540000070319     c                   eval      Aggior_Dist = 'S'
540100070319      *                         ?  =================  ?
540200070319     c                   exsr      CHAIN_Dist
540300070319      *
540400070319     c                   if        Error_Dist   = 'S'  or
540500070319     c                             Rec_Allocato = 'S'  or
540600070319     c                             Found_Dist   = 'N'
540700070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
540800070319     c                   eval      Exec_RB  = 'S'
540900070319     c                   end
541000070319      *
541100070319     c                   endIF
541200070319     c*
541300070319     c* se si tratta di un trasferimento c'è il 2°aggancio alla distinta
541400070319     c                   IF        w1ndc2  > 0  and Exec_RB  <> 'S'
541500070319     c*
541600070319      *   richiamo in aggiornamento D.B.     ?
541700070319     c                   move      w1ndc2        wNFV
541800070319      *                         ?  =================  ?
541900070319     c                   eval      Aggior_Dist = 'S'
542000070319      *                         ?  =================  ?
542100070319     c                   exsr      CHAIN_Dist
542200070319      *
542300070319     c                   if        Error_Dist   = 'S'  or
542400070319     c                             Rec_Allocato = 'S'  or
542500070319     c                             Found_Dist   = 'N'
542600070319      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
542700070319     c                   eval      Exec_RB  = 'S'
542800080609     c                   elSE
542900080609      **
543000070319     c                   end
543100070319      *
543200070319     c                   endIF
543300070319     c*
543400070309     C                   ENDSR
543500070309     C/EJECT
543600080626      * ?-------------------------------------------------------------*?
543700080626      *? esegue aggiornamenti x PDA eseguendo la Quadratura Distinta  ?
543800080626      * ?-------------------------------------------------------------*?
543900080626     C     Aggiorna_x_PDAbegsr
544000080626     C*
544100080626      **?  Se attivato come CONSEGNE con PDA: ?
544200080626      * ?   Allinea i file di trasmissione al PDA utilizzando  ?
544300080626      ** ?   la funzione di Quadratura fra ARB e PDC (trsm)
544400080704     c                   if          PDAconsegne <> *blank
544500080704      *
544600080704     c                   If          d1SCE <> Assegnare
544700080916      *
544800080916      *  se si tratta di distinta con PDA
544900080916     c                   If          h1PDA_meno = 'S'
545000080626      *   Comunque Toglie
545100080630      *     avendo prima già controllato e forzato
545200080630      *     non si deve più eseguire il controllo
545300080626     c                   clear                   fidg43ds
545400080626     c                   eval      CO43TLA =  'Q'
545500080626     c                   eval      CO43FGS =  wFGS
545600080626     c                   eval      CO43NDC =  h1ndc_meno
545700120209     c********           eval      CO43DDC =  h1cddc
545800120209     c                   eval      CO43DDC =  h1ddc_meno
545900080701     c                   eval      CO43ExlChk =  'S'
546000080626     c                   eval      kpjbus = kpjbu
546100080626     c                   movel     fidg43ds      kpjbu
546200080626     c                   call      'FIDG43R'
546300080626     c                   parm                    kpjba
546400080626     c                   movel     kpjbu         fidg43ds
546500080626     c                   eval      kpjbu = kpjbus
546600080626      *
546700080626      * se c'è stato un errore
546800080626     c                   if        co43err <> ' '
546900080626     c                   endif
547000080916      **
547100080704     c                   end
547200080916     c                   endIf
547300080626      **
547400080916      **
547500080704     c                   if          d1SCE = Trasferire or
547600080704     c                               d1SCE = Assegnare
547700080916      **
547800080916      *  se si tratta di distinta con PDA
547900080916     c                   If          h1PDA_piu  = 'S'
548000080626      *   Comunque Trasferisce
548100080630      *     avendo prima già controllato e forzato
548200080630      *     non si deve più eseguire il controllo
548300080626     c                   clear                   fidg43ds
548400080626     c                   eval      CO43TLA =  'Q'
548500080626     c                   eval      CO43FGS =  wFGS
548600080626     c                   eval      CO43NDC =  h1ndc_piu
548700120209     c*********          eval      CO43DDC =  h1cddc
548800120209     c                   eval      CO43DDC =  h1ddc_piu
548900080701     c                   eval      CO43ExlChk = 'S'
549000080626     c                   eval      kpjbus = kpjbu
549100080626     c                   movel     fidg43ds      kpjbu
549200080626     c                   call      'FIDG43R'
549300080626     c                   parm                    kpjba
549400080626     c                   movel     kpjbu         fidg43ds
549500080626     c                   eval      kpjbu = kpjbus
549600080626      *
549700080626      * se c'è stato un errore
549800080626     c                   if        co43err <> ' '
549900080626     c                   endif
550000080916     c                   end
550100080626     c                   endif
550200080626      **
550300080626      * se richiamato da MONITOR deve uscire direttamente x tornare al MONITOR
550400080626     c                   if        PGM_Chiamante = 'MONITOR'
550500080626     C                   MOVE      *On           $FINE
550600080626     c                   endIf
550700080626     c*
550800080626     c                   endIf
550900080626     c*
551000080626     C                   ENDSR
551100080626     C/EJECT
551200070427      * ?-------------------------------------------------------------*?
551300070427      *? Riordina comunque il SFL                                     ?
551400070427      * ?-------------------------------------------------------------*?
551500070515     C     Clear_Ordina  BEGSR
551600070427     C*
551700070515     c                   eval      posizionamento = 'N'
551800070515     c                   eval      posizionato    = 'N'
551900070515     C                   clear                   c2pos1
552000070515     c                   clear                   c2pos2
552100070515     C                   clear                   c2pos3
552200070515     c                   clear                   c2pos4
552300070515     C                   clear                   c2pos5
552400070515     C                   clear                   c2pos6
552500070515     C                   clear                   sav_pos1
552600070515     C                   clear                   sav_pos2
552700070515     C                   clear                   sav_pos3
552800070515     C                   clear                   sav_pos4
552900070515     C                   clear                   sav_pos5
553000070515     C                   clear                   sav_pos6
553100070515     c*
553200070515     C                   ENDSR
553300070515     C/EJECT
553400070515      * ?-------------------------------------------------------------*?
553500070515      *? Riordina comunque il SFL                                     ?
553600080610      *?  Ma se chiamato dalla Funzione MONITOR x PDA                 ?
553700080610      *?   Deve mostrare prima le spedizioni con INFO da Visionare    ?
553800070515      * ?-------------------------------------------------------------*?
553900070515     C     Ordina_SFL2   BEGSR
554000070514     C*
554100070514     C*  Totale Records Caricati nel 2°SFL
554200070427     c                   eval      wsfl = 1
554300070427     C*
554400070427     C*  se richiesto Ordinamento sul SFL da operatore --> F11
554500070427     c                   setoff                                       010203
554600070427     c                   setoff                                       040506
554700070427     c                   setoff                                       07
554800070427     C*
554900070427      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
555000070427      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
555100070427      * Il campo è usato per selezionare i records dando un ordine a quelli
555200070427      * selezionati davanti ai non selezionati.
555300070427     C                   CLEAR                   QLGSCB
555400070427     C                   CLEAR                   QLGSCB00
555500070427      *
555600070427     c                   select
555700070427      *
555800070427      *?  Ordinamento per GIRO/Destinatario  ?
555900070427     C                   when      WrkSort = Ord_GIRODEST
556000070427     c                   move      Ord_GIRODEST  wINDsort
556100070427      *
556200070427      * x Giro (Illumina il campo) con lo 07 = *on
556300070427     c                   seton                                        07
556400070427      *
556500080610      * ? se MONITOR prima le INFO:?
556600080610     c                   if        PGM_Chiamante = 'MONITOR'
556700080610     C                   EVAL      QLGNBRK    = 3
556800080610      *
556900080610     ** La Priorità se richiesta ricerca di stringa
557000080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
557100080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
557200080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)+ %SIZE(H2Subst)
557300080610     C                   EVAL      QLGSS    = %SIZE(S2cCOK)
557400080610     C                   EVAL      QLGDT    = Carattere
557500080610     C                   EVAL      QLGSO    = Discendente
557600080610     C                   EVAL      QLGKL(1) = QLGSKL
557700080610
557800080610     ** Il GIRO è in posizione (2) 10 Bytes char ascending
557900080610     C                   EVAL      QLGSP      = 1
558000080610     C                   EVAL      QLGSS      = %SIZE(S2CGI)
558100080610     C                   EVAL      QLGDT      = Carattere
558200080610     C                   EVAL      QLGSO      = Ascendente
558300080610     C                   EVAL      QLGKL(2)   = QLGSKL
558400080610
558500080610     ** La DESCRIZIONE ragione sociale  è Char ascending
558600080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
558700080610     C                   EVAL      QLGSS    = %SIZE(S2CRSD)
558800080610     C                   EVAL      QLGDT    = Carattere
558900080610     C                   EVAL      QLGSO    = Ascendente
559000080610     C                   EVAL      QLGKL(3) = QLGSKL
559100080610
559200080610     c                   else
559300080610
559400070427     ** 2 campi chiave.   x GIRO e DESTINTARIO
559500070427     C                   EVAL      QLGNBRK    = 2
559600070427
559700070427     ** Il GIRO è in posizione (1) 10 Bytes char ascending
559800070427     C                   EVAL      QLGSP      = 1
559900070427     C                   EVAL      QLGSS      = %SIZE(S2CGI)
560000070427     C                   EVAL      QLGDT      = Carattere
560100070427     C                   EVAL      QLGSO      = Ascendente
560200070427     C                   EVAL      QLGKL(1)   = QLGSKL
560300070427
560400070705     ** La DESCRIZIONE ragione sociale  è Char ascending
560500070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
560600070427     C                   EVAL      QLGSS    = %SIZE(S2CRSD)
560700070705     C                   EVAL      QLGDT    = Carattere
560800070427     C                   EVAL      QLGSO    = Ascendente
560900070427     C                   EVAL      QLGKL(2) = QLGSKL
561000080610     c                   end
561100070427      *
561200070427      *?  Ordinamento per Destinatario    ?
561300070427     C                   when      WrkSort = Ord_DESTINAT
561400070427     c                   move      Ord_DESTINAT  wINDsort
561500080610      *
561600080610      * ? se MONITOR prima le INFO:?
561700080610     c                   if        PGM_Chiamante = 'MONITOR'
561800080610     C                   EVAL      QLGNBRK    = 3
561900080610
562000080610     ** La Priorità se richiesta ricerca di stringa
562100080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
562200080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
562300080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)+ %SIZE(H2Subst)
562400080610     C                   EVAL      QLGSS    = %SIZE(S2cCOK)
562500080610     C                   EVAL      QLGDT    = Carattere
562600080610     C                   EVAL      QLGSO    = Discendente
562700080610     C                   EVAL      QLGKL(1) = QLGSKL
562800080610
562900080610     ** La Priorità se richiesta ricerca di stringa
563000080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
563100080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
563200080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)
563300080610     C                   EVAL      QLGSS    = %SIZE(H2Subst)
563400080610     C                   EVAL      QLGDT    = Numerico
563500080610     C                   EVAL      QLGSO    = Discendente
563600080610     C                   EVAL      QLGKL(2) = QLGSKL
563700080610
563800080610     ** Il DEST è in posizione (11) 20 Bytes char ascending
563900080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
564000080610     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
564100080610     C                   EVAL      QLGDT    = Carattere
564200080610     C                   EVAL      QLGSO    = Ascendente
564300080610     C                   EVAL      QLGKL(3) = QLGSKL
564400080610
564500080610     c                   else
564600070427      *
564700070511     ** 2 campi chiave.   x SUBSTring e DESTINATARIO
564800070511     C                   EVAL      QLGNBRK    = 2
564900070427
565000070511     ** La Priorità se richiesta ricerca di stringa
565100070511     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
565200070511     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
565300070511     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)
565400070511     C                   EVAL      QLGSS    = %SIZE(H2Subst)
565500070511     C                   EVAL      QLGDT    = Numerico
565600070511     C                   EVAL      QLGSO    = Discendente
565700070427     C                   EVAL      QLGKL(1) = QLGSKL
565800070511
565900070511     ** Il DEST è in posizione (11) 20 Bytes char ascending
566000070511     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
566100070511     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
566200070511     C                   EVAL      QLGDT    = Carattere
566300070511     C                   EVAL      QLGSO    = Ascendente
566400070511     C                   EVAL      QLGKL(2) = QLGSKL
566500080610      *
566600080610     c                   end
566700070427      *
566800070427      *?  Ordinamento per Indirizzo/Destinatario    ?
566900070427     C                   when      WrkSort = Ord_INDIRIZZO
567000070427     c                   move      Ord_INDIRIZZO wINDsort
567100080610      *
567200080610      * ? se MONITOR prima le INFO:?
567300080610     c                   if        PGM_Chiamante = 'MONITOR'
567400080610     C                   EVAL      QLGNBRK    = 4
567500080610
567600080610     ** La Priorità se richiesta ricerca di stringa
567700080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
567800080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
567900080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)+ %SIZE(H2Subst)
568000080610     C                   EVAL      QLGSS    = %SIZE(S2cCOK)
568100080610     C                   EVAL      QLGDT    = Carattere
568200080610     C                   EVAL      QLGSO    = Discendente
568300080610     C                   EVAL      QLGKL(1) = QLGSKL
568400080610
568500080610     ** La Priorità se richiesta ricerca di stringa
568600080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
568700080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
568800080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)
568900080610     C                   EVAL      QLGSS    = %SIZE(H2Subst)
569000080610     C                   EVAL      QLGDT    = Numerico
569100080610     C                   EVAL      QLGSO    = Discendente
569200080610     C                   EVAL      QLGKL(2) = QLGSKL
569300080610
569400080610     ** Il INDIRIZZO è in posizione (xx) 20 Bytes char ascending
569500080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
569600080610     c                                           %SIZE(S2CRSD)
569700080610     C                   EVAL      QLGSS    = %SIZE(S2Cind)
569800080610     C                   EVAL      QLGDT    = Carattere
569900080610     C                   EVAL      QLGSO    = Ascendente
570000080610     C                   EVAL      QLGKL(3) = QLGSKL
570100080610
570200080610     ** Il DEST è in posizione (11) 20 Bytes char ascending
570300080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
570400080610     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
570500080610     C                   EVAL      QLGDT    = Carattere
570600080610     C                   EVAL      QLGSO    = Ascendente
570700080610     C                   EVAL      QLGKL(4) = QLGSKL
570800080610      *
570900080610     c                   else
571000070427      *
571100070511     ** 3 campi chiave.   x INDIRIZZO/DESTINATARIO
571200070511     C                   EVAL      QLGNBRK    = 3
571300070511
571400070511     ** La Priorità se richiesta ricerca di stringa
571500070511     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
571600070511     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
571700070511     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)
571800070511     C                   EVAL      QLGSS    = %SIZE(H2Subst)
571900070511     C                   EVAL      QLGDT    = Numerico
572000070511     C                   EVAL      QLGSO    = Discendente
572100070511     C                   EVAL      QLGKL(1) = QLGSKL
572200070427
572300070427     ** Il INDIRIZZO è in posizione (xx) 20 Bytes char ascending
572400070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
572500070427     c                                           %SIZE(S2CRSD)
572600070427     C                   EVAL      QLGSS    = %SIZE(S2Cind)
572700070427     C                   EVAL      QLGDT    = Carattere
572800070427     C                   EVAL      QLGSO    = Ascendente
572900070511     C                   EVAL      QLGKL(2) = QLGSKL
573000070427
573100070427     ** Il DEST è in posizione (11) 20 Bytes char ascending
573200070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
573300070427     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
573400070427     C                   EVAL      QLGDT    = Carattere
573500070427     C                   EVAL      QLGSO    = Ascendente
573600070511     C                   EVAL      QLGKL(3) = QLGSKL
573700070427
573800080610     c                   end
573900080610      *
574000070427      *?  Ordinamento per Località/Destinatario    ?
574100070427     C                   when      WrkSort = Ord_LOCALITA
574200070427     c                   move      Ord_LOCALITA  wINDsort
574300070427      *
574400080610      * ? se MONITOR prima le INFO:?
574500080610     c                   if        PGM_Chiamante = 'MONITOR'
574600080610     C                   EVAL      QLGNBRK    = 4
574700080610
574800080610     ** La Priorità se richiesta ricerca di stringa
574900080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
575000080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
575100080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)+ %SIZE(H2Subst)
575200080610     C                   EVAL      QLGSS    = %SIZE(S2cCOK)
575300080610     C                   EVAL      QLGDT    = Carattere
575400080610     C                   EVAL      QLGSO    = Discendente
575500080610     C                   EVAL      QLGKL(1) = QLGSKL
575600080610
575700080610     ** La Priorità se richiesta ricerca di stringa
575800080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
575900080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
576000080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)
576100080610     C                   EVAL      QLGSS    = %SIZE(H2Subst)
576200080610     C                   EVAL      QLGDT    = Numerico
576300080610     C                   EVAL      QLGSO    = Discendente
576400080610     C                   EVAL      QLGKL(2) = QLGSKL
576500080610
576600080610     ** La LOCALITA' è in posizione (xx) 20 Bytes char ascending
576700080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
576800080610     c                                           %SIZE(S2CRSD) +
576900080610     c                                           %SIZE(S2CIND)
577000080610     C                   EVAL      QLGSS    = %SIZE(S2Clod)
577100080610     C                   EVAL      QLGDT    = Carattere
577200080610     C                   EVAL      QLGSO    = Ascendente
577300080610     C                   EVAL      QLGKL(3) = QLGSKL
577400080610
577500080610     ** Il DEST è in posizione (11) 20 Bytes char ascending
577600080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
577700080610     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
577800080610     C                   EVAL      QLGDT    = Carattere
577900080610     C                   EVAL      QLGSO    = Ascendente
578000080610     C                   EVAL      QLGKL(4) = QLGSKL
578100080610
578200080610     c                   else
578300080610      *
578400070511     ** 3 campi chiave.   x LOCALITA'/DESTINATARIO
578500070511     C                   EVAL      QLGNBRK    = 3
578600070511
578700070511     ** La Priorità se richiesta ricerca di stringa
578800070511     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
578900070511     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
579000070511     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)
579100070511     C                   EVAL      QLGSS    = %SIZE(H2Subst)
579200070511     C                   EVAL      QLGDT    = Numerico
579300070511     C                   EVAL      QLGSO    = Discendente
579400070511     C                   EVAL      QLGKL(1) = QLGSKL
579500070427
579600070427     ** La LOCALITA' è in posizione (xx) 20 Bytes char ascending
579700070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
579800070427     c                                           %SIZE(S2CRSD) +
579900070427     c                                           %SIZE(S2CIND)
580000070427     C                   EVAL      QLGSS    = %SIZE(S2Clod)
580100070427     C                   EVAL      QLGDT    = Carattere
580200070427     C                   EVAL      QLGSO    = Ascendente
580300070511     C                   EVAL      QLGKL(2) = QLGSKL
580400070427
580500070427     ** Il DEST è in posizione (11) 20 Bytes char ascending
580600070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
580700070427     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
580800070427     C                   EVAL      QLGDT    = Carattere
580900070427     C                   EVAL      QLGSO    = Ascendente
581000070511     C                   EVAL      QLGKL(3) = QLGSKL
581100080610      *
581200080610     c                   end
581300070427      *
581400070427      *?  Ordinamento per Peso/Destinatario    ?
581500070427     C                   when      WrkSort = Ord_PESO
581600070427     c                   move      Ord_PESO      wINDsort
581700070427      *
581800080610      * ? se MONITOR prima le INFO:?
581900080610     c                   if        PGM_Chiamante = 'MONITOR'
582000080610     C                   EVAL      QLGNBRK    = 3
582100080610
582200080610     ** La Priorità se richiesta ricerca di stringa
582300080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
582400080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
582500080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)+ %SIZE(H2Subst)
582600080610     C                   EVAL      QLGSS    = %SIZE(S2cCOK)
582700080610     C                   EVAL      QLGDT    = Carattere
582800080610     C                   EVAL      QLGSO    = Discendente
582900080610     C                   EVAL      QLGKL(1) = QLGSKL
583000080610
583100080610     ** Il PESO è in posizione (xx) 7 Digits Num descending
583200080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
583300080610     c                                           %SIZE(S2CRSD) +
583400080610     c                                           %SIZE(S2CIND) +
583500080610     c                                           %SIZE(S2CLOD)
583600080610     C                   EVAL      QLGSS    = %SIZE(S2Cpkf)
583700080610     C                   EVAL      QLGDT    = Numerico
583800080610     C                   EVAL      QLGSO    = Discendente
583900080610     C                   EVAL      QLGKL(2) = QLGSKL
584000080610
584100080610     ** Il DEST è in posizione (11) 20 Bytes char ascending
584200080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
584300080610     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
584400080610     C                   EVAL      QLGDT    = Carattere
584500080610     C                   EVAL      QLGSO    = Ascendente
584600080610     C                   EVAL      QLGKL(3) = QLGSKL
584700080610
584800080610     c                   else
584900080610      *
585000070427     ** 2 campi chiave.   x PESO/DESTINATARIO
585100070427     C                   EVAL      QLGNBRK    = 2
585200070427
585300070427     ** Il PESO è in posizione (xx) 7 Digits Num descending
585400070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
585500070427     c                                           %SIZE(S2CRSD) +
585600070427     c                                           %SIZE(S2CIND) +
585700070427     c                                           %SIZE(S2CLOD)
585800070427     C                   EVAL      QLGSS    = %SIZE(S2Cpkf)
585900070427     C                   EVAL      QLGDT    = Numerico
586000070427     C                   EVAL      QLGSO    = Discendente
586100070427     C                   EVAL      QLGKL(1) = QLGSKL
586200070427
586300070427     ** Il DEST è in posizione (11) 20 Bytes char ascending
586400070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
586500070427     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
586600070427     C                   EVAL      QLGDT    = Carattere
586700070427     C                   EVAL      QLGSO    = Ascendente
586800070427     C                   EVAL      QLGKL(2) = QLGSKL
586900080610      *
587000080610     c                   end
587100070427      *
587200070427      *?  Ordinamento per Volume/Destinatario    ?
587300070427     C                   when      WrkSort = Ord_VOLUME
587400070427     c                   move      Ord_VOLUME    wINDsort
587500080610      *
587600080610      * ? se MONITOR prima le INFO:?
587700080610     c                   if        PGM_Chiamante = 'MONITOR'
587800080610     C                   EVAL      QLGNBRK    = 3
587900080610
588000080610     ** La Priorità se richiesta ricerca di stringa
588100080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI) + %SIZE(S2CRSD) +
588200080610     c                             %SIZE(S2CIND)+ %SIZE(S2CLOD)+ %SIZE(S2CPKF)+
588300080610     c                             %SIZE(S2CVLF)+ %SIZE(S2CNCL)+ %SIZE(H2Subst)
588400080610     C                   EVAL      QLGSS    = %SIZE(S2cCOK)
588500080610     C                   EVAL      QLGDT    = Carattere
588600080610     C                   EVAL      QLGSO    = Discendente
588700080610     C                   EVAL      QLGKL(1) = QLGSKL
588800080610
588900080610     ** Il VOLUME è in posizione (xx) 5 Digits Num descending
589000080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
589100080610     c                                           %SIZE(S2CRSD) +
589200080610     c                                           %SIZE(S2CIND) +
589300080610     c                                           %SIZE(S2CLOD) +
589400080610     c                                           %SIZE(S2CPKF)
589500080610     C                   EVAL      QLGSS    = %SIZE(S2Cvlf)
589600080610     C                   EVAL      QLGDT    = Numerico
589700080610     C                   EVAL      QLGSO    = Discendente
589800080610     C                   EVAL      QLGKL(2) = QLGSKL
589900080610
590000080610     ** Il DEST è in posizione (11) 20 Bytes char ascending
590100080610     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
590200080610     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
590300080610     C                   EVAL      QLGDT    = Carattere
590400080610     C                   EVAL      QLGSO    = Ascendente
590500080610     C                   EVAL      QLGKL(3) = QLGSKL
590600080610      *
590700080610     c                   else
590800070427      *
590900070427     ** 2 campi chiave.   x VOLUME/DESTINATARIO
591000070427     C                   EVAL      QLGNBRK    = 2
591100070427
591200070427     ** Il VOLUME è in posizione (xx) 5 Digits Num descending
591300070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)  +
591400070427     c                                           %SIZE(S2CRSD) +
591500070427     c                                           %SIZE(S2CIND) +
591600070427     c                                           %SIZE(S2CLOD) +
591700070427     c                                           %SIZE(S2CPKF)
591800070427     C                   EVAL      QLGSS    = %SIZE(S2Cvlf)
591900070427     C                   EVAL      QLGDT    = Numerico
592000070427     C                   EVAL      QLGSO    = Discendente
592100070427     C                   EVAL      QLGKL(1) = QLGSKL
592200070427
592300070427     ** Il DEST è in posizione (11) 20 Bytes char ascending
592400070427     C                   EVAL      QLGSP   = 1 + %SIZE(S2CGI)
592500070427     C                   EVAL      QLGSS    = %SIZE(S2Crsd)
592600070427     C                   EVAL      QLGDT    = Carattere
592700070427     C                   EVAL      QLGSO    = Ascendente
592800070427     C                   EVAL      QLGKL(2) = QLGSKL
592900070427      *
593000080610     c                   end
593100080610      *
593200070427     c                   endSL
593300070427      *------------
593400070427      * Load other sort parameters.
593500070427     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
593600070427     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
593700070427     C                   EVAL      QLGRT     = 8
593800070427     C                   EVAL      QLGOKL    = 80
593900070427     C                   EVAL      QLGLKE    = 16
594000070427     C                   EVAL      QLGLSS    = 290
594100070427
594200070427      * Initialize Sort I/O API fields.
594300070427     C                   EVAL      QLGRL00  = QLGRL
594400070427     C                   EVAL      QLGRC00  = 1
594500070427     C                   CLEAR                   QUSEI
594600070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
594700070427
594800070427      * First step - Initialize the sort routine.
594900070427     C                   CALL      'QLGSORT'
595000070427     C                   PARM                    QLGSCB
595100070427     C                   PARM                    NotUsed
595200070427     C                   PARM                    NotUsed
595300070427     C                   PARM                    SizeList
595400070427     C                   PARM                    ReturnSize
595500070427     C                   PARM                    QUSEC
595600070427
595700070427      * Next step - Write records to I/O routine.
595800070427     C                   EVAL      QLGRT00 = Put
595900070427
596000070503     C                   DO        RrnLast       S2nrr_xx          5 0
596100070503     C     S2nrr_xx      CHAIN     FDG0S02
596200070427
596300070427     ** Solo le righe con Selected = 'Y' sono riordinate,
596400070427     ** quindi per fare un ordinamento di tutte le righe
596500070427     ** metto 'Y' sempre.
596600070427     C                   EVAL      Selected  = 'Y'
596700070427
596800070427     C                   CLEAR                   QUSEI
596900070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
597000070427      *
597100070427     C                   CALL      'QLGSRTIO'
597200070427     C                   PARM                    QLGSCB00
597300070427     C                   PARM                    SflRcd
597400070427     C                   PARM                    NotUsed
597500070427     C                   PARM                    SizeList
597600070427     C                   PARM                    NotUsed
597700070427     C                   PARM                    QUSEC
597800070427
597900070427     C                   ENDDO
598000070427
598100070427      * Next step - Signal end of input, clear subfile for reload.
598200070427     C                   EVAL      QLGRT00 = EndPut
598300070427     C                   CLEAR                   QUSEI
598400070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
598500070427
598600070427     C                   CALL      'QLGSRTIO'
598700070427     C                   PARM                    QLGSCB00
598800070427     C                   PARM                    SflRcd
598900070427     C                   PARM                    NotUsed
599000070427     C                   PARM                    SizeList
599100070427     C                   PARM                    NotUsed
599200070427     C                   PARM                    QUSEC
599300070427      *
  pulizia SFL 2  ?
599400070427     C                   SETOFF                                         3031
599500070427     C                   WRITE     FDG0C02
599600070427     C                   SETON                                          31
599700070427      *
599800070427
599900070427      *
  Campo di Ordinamento in REVERSE ?
600000070427     c                   if        wINDsort  > 0
600100070427     C                   move      *on           *in(wINDsort)
600200070427     c                   end
600300070427
600400070427      *
  Campo di Posizionamento  ?
600500070427     c   01              eval      C2DESp = '     Posiz. x GIRO :'
600600070515     c   02              eval      C2DESp = 'Scans. x DESTINAT. :'
600700070515     c   03              eval      C2DESp = 'Scans. x INDIRIZZO :'
600800070515     c   04              eval      C2DESp = 'Scans. x  LOCALITA :'
600900070427     c   05              eval      C2DESp = '   Posiz. x VOLUME :'
601000070427     c   06              eval      C2DESp = '     Posiz. x PESO :'
601100070427
601200070427      * Final step - Write the records back to the subfile.
601300070427     C                   EVAL      QLGRT00 = Get
601400070427
601500070503     C                   DO        RrnLast       S2nrr_xx
601600070503     c                   eval      S2nrr = S2nrr_xx
601700070427     C                   CLEAR                   QUSEI
601800070427     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
601900070427     C                   CALL      'QLGSRTIO'
602000070427     C                   PARM                    QLGSCB00
602100070427     C                   PARM                    NotUsed
602200070427     C                   PARM                    SflRcd
602300070427     C                   PARM                    QLGRL00
602400070427     C                   PARM                    NotUsed
602500070427     C                   PARM                    QUSEC
602600070503      * SFLnxtCHG
602700070503     c                   if        s2sce <> *blank
602800070503     c                   seton                                        32
602900070515     c                   else
603000070515     C                   SetOFF                                       32
603100070503     c                   end
603200101130     c                   eval      *in45 = H2in45
603300130221     c     h2xco         comp      '0'                                88
603400070427     C                   WRITE     FDG0S02
603500070427     C                   ENDDO
603600070427      *
603700070503      *  All'uscita di queste chiamate a routine di sistema il SFL record
603800070503      *   si incrementa stranamente quindi lo reimposto correttamente
603900070503      *    prima di lasciare la routine.
604000070503     C                   EVAL      S2nrr = RrnLast
604100070514     C*
604200070427     C                   ENDSR
604300070427     C/EJECT
604400150930     C*-----------------------------------------------------*
604500150930     C* controllo se mandato un CONTRO Alert al CLIENTE
604600150930     C*-----------------------------------------------------*
604700150930     C     contro_alert  BEGSR
604800150930      *
604900150930      * rec.EMD   invio Alert e contro Alert
605000150930     C                   eval      inviato_contro_alert = 'N'
605100150930     C                   clear                   dar5emd
605200150930     C                   move      'EMD'         TRDar5
605300150930     C     Kfiar5        CHAIN     FiAR501L
605400150930     C                   IF        %Found(FiAR501L)
605500150930     C                   MOVEL     ar5uni        dar5emd
605600150930     c                   if        §AR5ICA ='S'
605700150930     c                   if        §AR5DPC <>*blank and §AR5DPC <>*zeros
605800150930     c                   move      §AR5DPC       data8DPC          8 0
605900150930     c                   if        data8DPC = Dateu8
606000150930     C                   eval      inviato_contro_alert = 'S'
606100150930     C                   end
606200150930     C                   end
606300150930     C                   end
606400150930     C                   endif
606500150930      *
606600150930     C                   ENDSR
606700080609      * ?-------------------------------------------------------------*?
606800080609      *
606900070326** ErrMsg
607000070326La Filiale non è in gestione                                                   01
607100070326Data errata                                                                    02
607200070326Data antecedente alla data odierna                                             03
607300070326Data distinta superiore alla data odierna dei gg.ammessi                       04
607400070326Data distinta anteriore o superiore alla data odierna dei gg.ammessi           05
607500070326Non esistono distinte aperte nella data scelta                                 06
607600070326Scelta obbligatoria                                                            07
607700070326Codice Giro non corretto o non assegnabile ad una distinta                     08
607800070326Numero Distinta non valido o inesistente                                       09
607900070326Distinta chiusa o annullata                                                    10
608000070413Distinta ancora in uso da un altro lavoro.                                     11
608100070328Scegliere 1 solo giro x effettuare l'interrogazione                            12
608200070326La Distinta è inesistente o non è stata selezionata                            13
608300070326La Distinta è chiusa o annullata                                               14
608400070326La Distinta è ancora allocata. Non è quindi possibile selezionarla.            15
608500070326Non è possibile utilizzare il Giro su Distinte                                 16
608600070326 Non è stata scelta la DISTINTA per assegnare le Spedizioni                    17
608700070326   Effettuare una scelta mediante le funzioni di ricerca.                      18
608800070326 Occorre selezionare almeno una riga per poter proseguire                      19
608900070326  l'elaborazione.                                                              20
609000070326 Le bolla sopra elencate NON sono state aggiornate perchè ALLOCATE da altro    21
609100070326 Lavoro.  CONTROLLARE e RIESEGUIRE l'operazione                                22
609200070326 Durante l'aggiornamento archivi ci sono stati dei problemi. Non eseguito il   23
609300070326 Lavoro.  CONTROLLARE e RIESEGUIRE l'operazione                                24
609400070326Aggiornamento eseguito correttamente                                           25
609500070326    Aggiornamento  N O N  eseguito                                             26
609600070326Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
609700070326USCIRE SUBITO dal lavoro!                                                      28
609800070413Non si può trasferire sulla stessa Distinta                                    29
609900070417Distinta non appartenente alla Data selezionata                                30
610000070829Data limite superiore alla data Distinta                                       31
610100070829Ora limite errata                                                              32
610200080609Il valore immesso per il campo non è valido.                                   33
610300100505Forzare con (F2) la spedizione con Particolarità di ATTESA x consegnarla !!    34
610400150930F2 x Forzare -> presente ALERT di VARIAZIONE data CONSEGNA.                    35
610500160317Non può essere selezionata. Assegnarla a una distinta DPD Fittizia !!          36
610600160323Attribuire GIRO VEDI PACCO a DISTINTA di AUTISTA FITTIZIO !!                   37
610700160323Spedizioni VEDI PACCO possono essere assegnate solo ad AUTISTA Fittizio!!      38
610800070829                                                                               39
610900070829                                                                               40
