000100980223     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)
000200980923     H*PARMS BNDDIR(PJXBND PJCBND NDC045BND)
000300980223     H*PARMS ACTGRP(PNOTA)
000400000127     H*PARMS CVTOPT(*DATETIME) OPTIMIZE(*NONE)
000500940211     H DECEDIT('0,') DATEDIT(*DMY.)
000600950825      *
000700040308      *            Gestione saldaconto
000800951026      *           ---------------------
000900940224      *
001000940224      *
001100940224      * DOCUMENTI MICROANALISI :
001200940224      *
001300990324      * NOTE TECNICHE :
001400990528C0762 *                 ###  le specifiche indicate con   C0762    ###
001500990526  "   *                 ###  sono relative all'apertura dello      ###
001600990526  "   *                 ###  sconto su partite con divise diverse  ###
001700040308  "   *                 ###  da quella di testata;                 ###
001800040308  "   *                 ###  in precedenza erano siglate 0941 ma   ###
001900990528C0762 *                 ###  non era il task giusto                ###
002000990616      *                 ----------------------------------------------
002100990528C0941 *                 ###  le specifiche indicate con   C0941    ###
002200990526  "   *                 ###  sono relative all'apertura dello      ###
002300990526  "   *                 ###  abbuono su partite con divise diverse ###
002400990528C0941 *                 ###  da quella di testata;                 ###
002500990629      *                 ----------------------------------------------
002600990616C1185 *                 ###  le specifiche indicate con   C1185    ###
002700990616  "   *                 ###  gestiscono gli sconti condizionati    ###
002800990616C1185 *                 ###  (chiamando un pgm apposito)           ###
002900990908      *                 ----------------------------------------------
003000990908C1209 *                 ###  gestione F14=Commenti: se fatto       ###
003100990803  "   *                 ###  sul ctl, vale per il conto;           ###
003200990803  "   *                 ###  se fatto sul sfl, vale per la partita ###
003300990803  "   *                 ###  su cui si è posizionati.              ###
003400990803  "   *                 ###  Non è possibile fare F14 sulle partite###
003500990803  "   *                 ###  immesse dal saldaconto.               ###
003600990803  "   *                 ###  Le partite che hanno già dei commenti ###
003700990908C1209 *                 ###  sono sottolineate.                    ###
003800990908      *                 ----------------------------------------------
003900990910C1226 *                 ###  L'F12 sulla window di parzializzazione###
004000990910  "   *                 ###        aveva l'effetto di un 'Invio'   ###
004100990908  "   *                 ###  ossia andava al saldaconto invece di  ###
004200990908C1226 *                 ###  retrocedere al multicodice            ###
004300991020D0206 *                 ----------------------------------------------
004400991020  "   *                 ###  usava la causale principale di tabella###
004500991020D0206 *                 ###  e non quella impostata in testata     ###
004600000821C1393 *  -------------------------------------------------------------
004700000821  "   *  ## siccome è stata tolta una "bruttura" nel tubero delle  ###
004800000821  "   *  ## partite che permetteva di indicare come importo in     ###
004900000821  "   *  ## divisa di partita un valore diverso da quello in m.c.  ###
005000000821  "   *  ## anche se la divisa di partita era la m.c. , è morto    ###
005100000821  "   *  ## anche l'automatismo di riquadratura dei totali in m.c. ###
005200000821  "   *  ## Col task D0529 veniva emessa una finestra che segnalava###
005300000821  "   *  ## di non poter effettuare automatismo in quanto tutte le ###
005400000821  "   *  ## partite selezionate hanno come divisa la m.c.          ###
005500000821  "   *  ## Con questo task viene automatizzata la scrittura di un ###
005600000821C1393 *  ## movimento di arrotondamento per conto.                 ###
005700010105C1458 *  -------------------------------------------------------------
005800010105  "   *  ## aggiunta la gestione del blocco sulla singola rata;    ###
005900010105  "   *  ## mentre si faceva ciò si sono fatte anche delle procedu=###
006000010105  "   *  ## re che leggono i rcd degli scadenzari, quindi si è     ###
006100010105  "   *  ## tolto l'uso interno del file NDPPS00F;                 ###
006200010105  "   *  ## siccome è rarissimo se non impossibile che i rcd di    ###
006300010105  "   *  ## NDPSC siano allocati anche per una pura lettura (come  ###
006400010105  "   *  ## fa la procedura in questione) non è stato presa in     ###
006500010105  "   *  ## considerazione la possibilità che ciò accada, per cui  ###
006600010105  "   *  ## il rcd della partita potrebbe non mostrare l'eventuale ###
006700010105C1458 *  ## blocco sulla rata e, quindi, essere manutenzionabile   ###
006800940224      *
006900940224      * PERSONALIZZAZIONI
007000940224      *
007100940224      *
007200940224      *
007300940314      *               MAPPA INDICATORI
007400940224      *
007500940307      *  21           GENERICO OPERAZIONI I/O
007600940224      *  22           GENERICO ERRORE OPERAZIONI I/O
007700940224      *  30           SFLDSP
007800940224      * N31           SFLCLR
007900940224      *  31           SFLDSPCTL
008000940224      *  32           SFLNXTCHG
008100940224      *  33           SFLEND
008200940224      *  39           OF PRTF
008300940224      *  40 <---> 49  DSPATR ERRORI SU SFL
008400940224      *  50 <---> 98  ERRORI SU VIDEO
008500950526      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
008600940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
008700940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
008800940224      *  99           INDIC. GENERALE DI ERRORE
008900940128     F*----------------------------------------------------*
009000970225     F*------------
009100040308     FNDPPA03L  IF   E           K DISK
009200070417C2012FNDSDC01W  UF A E           K DISK    USROPN
009300070417C2012F                                     RENAME(NDSDC000:NDSDC01)
009400951026     F*------------
009500010105     x***NDPPS01L  IF   E           K DISK
009600940211     F*------------
009700040308     FNDC045D   CF   E             WORKSTN
009800951026     F                                     RENAME(C045S1:FMTS1)
009900951026     F                                     RENAME(C045C1:FMTC1)
010000951026     F                                     RENAME(C045Z1:FMTZ1)
010100951026     F                                     RENAME(C045T1:FMTT1)
010200951026     F                                     RENAME(C045D1:FMTD1)
010300940131     F                                     SFILE(FMTS1:S1NRR)
010400940201     F                                     INFDS(DSFMT)
010500950704     D*----------------------------------------------------*
010600980923     D*-------------
010700980923     D* PROTOTIPAZIONE DELLE PROCEDURE
010800981103     D/COPY *LIBL/SRCCPY,PJX002PR
010900010105C1458D/COPY *LIBL/SRCCPY,PJC006PR
011000980923     D*-------------
011100950920     D* data *loval
011200950920     D Dt0             C                   const('0001-01-01')
011300951024     D* data *hival
011400951024     D Dt9             C                   const('9999-12-31')
011500950706     D*
011600950821     D* skiera salvataggio indicatori x sflmsgid
011700950821     D In              S              1    DIM(99)
011800950704     D*
011900950704     D $TX             S             50    DIM(50)
012000950704     D $FL             S              1    DIM(50)
012100940909     D* Schiere generali per la gestione dei tasti di funzione
012200940506     D $FM             S             27    DIM(24)
012300940506     D $FX             S             50    DIM(24)
012400940506     D $FC             S              1    DIM(24)
012500940506     D $FR             S              1    DIM(24)
012600960311     D* Indice della schiera tasti funzione
012700960311     D PosFun          S              2  0
012800940506     D* Schiere per la gestione dei tasti di funzione
012900940506     D* Video S1
013000950630     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
013100950630     D $FX1            S             50    DIM(24)
013200950802     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
013300950802     D $FR1            S              1    DIM(24) ALT($FC1)
013400960311     D* Schiera contente i nomi dei programmi da richiamare con i tasti
013500960311     D* funzione personalizzati
013600960311     D $PGF1           S             10    DIM(25)
013700940302     D*
013800940211     D/EJECT
013900940128     D*----------------------------------------------------*
014000940207     D*-------------
014100940211     D* Passaggio Parametri
014200940211     D KPJBA         E DS
014300940211     D*-------------
014400941209     D* Dati di ambiente ottenuti da XSOC
014500040308     D SOC001        E DS                  EXTNAME(XSOC001DS)
014600961009     D    XscDare                     1    overlay(xscFgo:7)                    Scadenz. retroattivo
014700961009     D    XscAvere                    1    overlay(xscFgo:8)                    Scadenz. retroattivo
014800941209     D*-------------
014900941209     D* DS Interna per dati di output di XSOC
015000941209     D XSOCDS          DS          1000
015100960131     D*-------------
015200960201     D* DS per richiamo interrogazione conti
015300960201     D ANA399DS      E DS
015400960201     D*-------------
015500960131     D* Parametri in ricezione
015600040308     D NDC045DS      E DS
015700960311     D*-------------
015800980114     D* Parametri selezione
015900040308     D NDC079DS      E DS                  inz
016000040308     D  DtParDa079   E                     INZ('0001-01-01')
016100040308     D  DtParA079    E                     INZ('0001-01-01')
016200070418C2012D  DtScaDa079   E                     INZ('0001-01-01')
016300070418C2012D  DtScaA079    E                     INZ('9999-12-31')
016400990616C1185D*-------------
016500990616  "  D* Parametri gestione sconti condizionati
016600990616  "  D NDC545DS      E DS
016700990616  "  D  RET545       E                     INZ('0')
016800990616  "  D  OPR545       E                     INZ('0')
016900990616  "  D  ERR545       E                     INZ('0')
017000990616C1185D  DTVAL545     E                     inz(dt0)
017100980114     D*-------------
017200960311     D* campo di salvataggio per la DS d'ingresso durante la chiamata
017300960311     D* alla funzione personalizzata
017400040308     D $SaveDS         S                   like(NDC045DS)
017500951019     D*-------------
017600960131     D* Parametri ricerca partite
017700040308     D NDC060DS      E DS                  inz
017800040308     D  RET060       E                     INZ('0')
017900040308     D  OPR060       E                     INZ('0')
018000040308     D  ERR060       E                     INZ('0')
018100040308     D  DTpar060     E                     INZ('0001-01-01')
018200040308     D  DTCHIU060    E                     INZ('0001-01-01')
018300960131     D*-------------
018400951020     D* queste DS mi servono solo per referenziare alcuni campi che
018500951020     D* altrimenti dovrei dimensionare in modo fisso
018600040308     D NDPAS00F      E DS
018700040308     D NDBHM00F      E DS
018800951019     D*-------------
018900951019     D* Regole contabili
019000040308     D ND002DS       E DS
019100951019     D  DTANN002     E                     inz('0001-01-01')
019200951019     D  DTIMM002     E                     inz('0001-01-01')
019300951019     D  DTULMO002    E                     inz('0001-01-01')
019400950921     D*-------------
019500951019     D* Dati movimento
019600951019     D ND007DS       E DS                   inz
019700951019     D  DTRFCAM007   E                      inz(dt0)
019800951019     D  DTDOC007     E                      inz(dt0)
019900951019     D  DTLIB007     E                      inz(dt0)
020000951019     D*-------------
020100951019     D* Dati tesoreria
020200951019     D ND014DS       E DS                   inz
020300951019     D  DTOPE014     E                      inz(dt0)
020400951019     D  DTVAL014     E                      inz(dt0)
020500951019     D  DTPAR014     E                      inz(dt0)
020600951019     D  DTVALB014    E                      inz(dt0)
020700951017     D*-------------
020800951017     D* Rapporti di tesoreria + descrizioni
020900040308     D ND016DS       E DS
021000951017     D*-------------
021100951019     D* Dati partite
021200951019     D ND018DS       E DS                   inz
021300951019     D  DTPAR018     E                      inz(dt0)
021400951019     D  DTSCA018     E                      inz(dt0)
021500951019     D  DTFLU018     E                      inz(dt0)
021600951019     D*-------------
021700951120     D* Parametri scrittura ritenute d'acconto
021800981026     D NDCR95DS      E DS                   inz
021900951120     D  R95DTPAR     E                      inz(dt0)
022000951120     D  R95DTREG     E                      inz(dt0)
022100981104     D  R95DtRfCam   E                      inz(dt0)
022200951120     D*-------------
022300951120     D* Parametri reperimento ritenute d'acconto
022400981026     D NDCR96DS      E DS                   inz
022500951120     D  R96DTPAR     E                      inz(dt0)
022600951120     D*-------------
022700951120     D* Parametri gestione cessioni
022800951120     D NDC046DS      E DS
022900951120     D  RET046       E                     INZ('0')
023000951120     D  OPR046       E                     INZ('0')
023100951120     D  ERR046       E                     INZ('0')
023200951120     D  CMT046       E                     INZ('1')
023300951120     D  DTSCA046     E                      inz(dt0)
023400951120     D  DTREG046     E                      inz(dt0)
023500951120     D*-------------
023600951018     D* Ricerca operazioni bancarie
023700951023     D ANG0IPDS      E DS
023800951023     D ANG1IPDS      E DS
023900951114     D ANG2IPDS      E DS
024000951018     D*-------------
024100951018     D* Ricerca divise
024200951018     D A015DS        E DS                  EXTNAME(ANA015DS) INZ
024300951018     D OPZ15         E                     EXTFLD(OPZ015)
024400951018     D ERR15         E                     EXTFLD(ERR015)
024500951018     D OPR15         E                     EXTFLD(OPR015)
024600951018     D RET15         E                     EXTFLD(RET015)
024700971128     D*-------------
024800971128     D* Tabella blocco amministrativo
024900971128     D ANG0ABDS      E DS
025000951018     D*-------------
025100951017     D* Scadenziari
025200040308     D X20ScadDS     E DS                   inz
025300951024     D  X20DTPAR     E                      inz(Dt0)
025400951025     D  X20DTREGMD   E                      inz(Dt0)
025500951024     D  X20DTREGMP   E                      inz(Dt9)
025600951025     D  X20DTREGMG   E                      inz(Dt0)
025700951024     D  X20DTSCA     E                      inz(Dt0)
025800951026     D*-------------
025900951026     D* DS parametro esteso se richiesto x20parscd='S'
026000951031     D X20SCADXDS    E DS                  inz
026100951026     D* schiere per parametro esteso
026200951027     D  DTSCAX                       10A   DIM(50) overlay(x20dtscax)
026300951027     D  IMPRMC                       19  3 DIM(50) overlay(x20imprmc)
026400951027     D  IMPRD                        19  3 DIM(50) overlay(x20imprd)
026500951027     D  IMPOMC                       19  3 DIM(50) overlay(x20impomc)
026600951027     D  IMPOD                        19  3 DIM(50) overlay(x20impod)
026700951027     D  CAUS1                         4A   DIM(50) overlay(x20caus1)
026800951027     D  CAUS2                         4A   DIM(50) overlay(x20caus2)
026900010105     D*-------------
027000950706     D* Richiamo a XATB
027100950804     D XATBDS        E DS                  inz
027200940211     D*-------------
027300970227     D* Causali operative
027400970227     D ANOPE         E DS                  EXTNAME(ANOPE00F)
027500970227     D  OPEDTANN     E                      inz(Dt0)
027600970227     D  OPEDTIMM     E                      inz(Dt0)
027700970227     D  OPEDTULMO    E                      inz(Dt0)
027800970227     D*-------------
027900950801     D* Ricerca su tabelle
028000950804     D XTabDS        E DS                  inz
028100950908     D*-------------
028200940211     D DSFMT           DS           512
028300940506     D  $TASTO               369    369
028400940211     D  NRG                  370    370
028500940211     D  NCL                  371    371
028600940211     D  SFLNRR               378    379B 0
028700940211     D*-------------
028800940211     D* posizione cursore
028900940211     D CURSOR          DS
029000940211     D  RIRI                   1      2B 0 INZ
029100940211     D  R$$                    2      2
029200940211     D  COCO                   3      4B 0 INZ
029300940211     D  C$$                    4      4
029400950710     D*-------------
029500950710     D* DS Esterna gestione messaggi
029600950710     D XMSGDS        E DS
029700950710     D  STS          E                     EXTFLD(MSGSTS)
029800950710     D                                     DIM(333)
029900950710     D  JBA          E                     EXTFLD(MSGJBA)
030000950710     D                                     DIM(502)
030100940127     D*-------------
030200950825     D* messaggi opzione per XMsgR
030300950825     D XTxt            DS
030400950912     D   XTxt1                       15
030500950912     D   XTxt2                       15
030600950912     D   XTxt3                       15
030700950912     D   XTxt4                       15
030800950912     D   XTxt5                       15
030900950912     D   XTxt6                       15
031000950912     D   XTxt7                       15
031100950912     D   XTxt8                       15
031200951110     D*-------------
031300951110     D                 DS           512
031400951110     D  C1NDC0018              1      8
031500951110     D  Pack0018               1      8P 3
031600951110     D  C1NDC0019              9     16
031700951110     D  Pack0019               9     16P 3
031800950825     D*-------------
031900950825     D* Reperimento nome PGM
032000940127     D STATUS         SDS           333
032100940127     D  DSPGM            *PROC
032200950411     D  PARMS            *PARMS
032300940207     D*-------------
032400940616     D* Costanti
032500940616     D* nr.di righe del sfl (SFLPAG)
032600960319     D SFLPAG          C                   CONST(7)
032700940616     D*-------------
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
034000940506     D F12             C                   CONST(X'3C')
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
035600990803C1209D*-------------
035700990803  "  D* Parametri del PGM di gestione commenti ANA725R
035800990803  "  D ANA725DS      E DS                  INZ
035900990803  "  D  RET725       E                     INZ('0')
036000990803  "  D  OPR725       E                     INZ('0')
036100990803  "  D  ERR725       E                     INZ('0')
036200990803  "  D*-------------
036300990803  "  D* Parametri del PGM di test esistenza commenti ANA724R
036400990803  "  D ANA724DS      E DS                  INZ
036500990803  "  D  EXI724       E                     INZ('0')
036600040308C1209D  RIS724       E                     INZ('0')
036700010305A0871x* CMT724       E                     INZ('0')
036800990803C1209D*-------------
036900950524     D*-------------
037000950606     D* Variabili per gestione videate
037100950606     D $Gest           S              2A
037200950606     D $Fine           S              1A
037300950721     D $InzS1          S              1A
037400950606     D $LastV          S              2A
037500950606     D $EndFile        S              1A
037600950606     D $Esci           S              1A
037700951018     D $RcdOk          S              1A
037800950801     D $S1L01          S              3  0
037900950801     D $S1L02          S              3  0
038000951128     D*-------------
038100951128     D* DS multipla per chiamata saldaconto da movimenti banca
038200001027D0748X*Sk2Partite      DS                  occurs(100)
038300070517D2164x*$DimSk ex-D0748 C                   Const(300)
038400070517D2164D $DimSk          C                   Const(3000)
038500001027D0748D Sk2Partite      DS                  occurs($DimSK)
038600960529     D  Sk2Kcc                             like(PASKcc)
038700960529     D  Sk2Ksc                             like(PASKsc)
038800951129     D  Sk2ImportD                         like(PASImportD)
038900951129     D  Sk2DtPar                           like(PASDtPar)
039000951129     D  Sk2NrPar                           like(PASNrPar)
039100951129     D  Sk2SerPar                          like(PASSerPar)
039200951201     D  Sk2DtSca                           like(PASDtSca)
039300960529     D  Sk2ImpDivC                         like(PASImportD)
039400960529     D  Sk2ImpAbbA                         like(PASImportD)
039500960529     D  Sk2ImpAbbP                         like(PASImportD)
039600960529     D  Sk2Sconto                          like(PASImportD)
039700950606     D*-------------
039800950704     D* Variabili
039900951018     D WMax            S                   like(C1Rcd)
040000951030     D WImportD        S                   like(Importo045)
040100951030     D WImporto        S                   like(Importo045)
040200951027     D WAbbuonoD       S                   like(Importo045)
040300951027     D WAbbuono        S                   like(Importo045)
040400951027     D WScontoD        S                   like(Importo045)
040500951027     D WSconto         S                   like(Importo045)
040600951027     D WRitenuta       S                   like(Importo045)
040700951027     D WDiffCam        S                   like(Importo045)
040800951026     D TotAvere        S                   like(Importo045)
040900951026     D TotDare         S                   like(Importo045)
041000951018     D Resto           S              3  0
041100951109     D DiffCamAtt      S                   like(Importo045)
041200951109     D DiffCamPas      S                   like(Importo045)
041300951108     D AbbAttivo       S                   like(Importo045)
041400951108     D AbbPassivo      S                   like(Importo045)
041500960115     D AbbAttivoD      S                   like(ImportD045)
041600960115     D AbbPassivD      S                   like(ImportD045)
041700951020     D Regola          S                   like(BHMRgRipVa)
041800951020     D Voce            S                   like(BHMVoce   )
041900951020     D RifInt          S                   like(BHMRifInt )
042000951020     D SubNum          S                   like(BHMSubNum )
042100951020     D SubRig          S                   like(BHMSubRig)
042200951109     D KccMOA          S                   like(C1Kcc)
042300951109     D KscMOA          S                   like(C1Ksc)
042400951030     D ImpDivPar       S                   like(Importo045)
042500951109     D Importo         S                   like(Importo045)
042600951122     D ImpSeg          S                   like(Importo045)
042700951122     D ImpSegD         S                   like(ImportD045)
042800980114     D Residuo         S                   like(Importo045)
042900980114     D ResiduoD        S                   like(ImportD045)
043000980114     D ScontoD         S                   like(ImportD045)
043100980114     D AbbuonoD        S                   like(ImportD045)
043200951031     D NrRigaMAbA      S                   like(PASNrRigaM)
043300951031     D NrRigaMAbD      S                   like(PASNrRigaM)
043400951031     D NrRigaMImp      S                   like(PASNrRigaM)
043500951031     D NrRigaMSco      S                   like(PASNrRigaM)
043600951108     D NrRigaMDCA      S                   like(PASNrRigaM)
043700951108     D NrRigaMDCD      S                   like(PASNrRigaM)
043800951113     D Decimali        S              1  0
043900951113     D ImportoDec      S                   like(Importo045)
044000951116     D DivisaMOA       S                   like(PASDivisa)
044100951201     D ScaTrovata      S              1A
044200960109     D RigaVuota       S                   like(C1Nrr)
044300960318     D ErrSfl          S              1  0
044400000529C1209x*Oggi            S             10A
044500990803C1209D Oggi            S               D
044600970327     D BkAmm           S                   like(PPSBkAmm)
044700010105C1458D BkAmmPSC        S                   like(PSCBkAmm)
044800980114     D WExf            S                   like(Exf079)
044900980115     D AutoImpD        S                   like(ImportD045)
045000980925     D* flag di NO controllo scostamento cambio/importo
045100980925     D NoCtrlScos      S              1A
045200980925     D* appogio per calcoli intermedi
045300980925     D W3015           S             30S15
045400980925     D* appogio per calcoli intermedi
045500980925     D W2004           S             30S15
045600981203     D* appogio per data riferimento da passare alla procedura X0201TpTri
045700981203     D* (questo pgm è CVTOPT(*DATETIME) mentre la procedura necessita di un
045800981203     D*  campo di tipo data)
045900981203     D WDataISO        S               D
046000990528C0762D* divisa per registrazione totale sconto
046100990519  "  D DivSconto       S                   like(S1Divisa)
046200990519  "  D* flag che indica se divise di partita diverse per lo sconto
046300990519  "  D DivDiveSco      S              1
046400990519  "  D* divisa per registrazione totale abbuono
046500990519  "  D DivAbbuono      S                   like(S1Divisa)
046600990519  "  D* flag che indica se divise di partita diverse per l'abbuono
046700990528C0762D DivDiveAbb      S              1
046800000529D0529D WDiffTotDS      DS
046900000529D0529D    WDiffTot                   13p 3
047000000821C1393D* Importo dell'arrotondamento effettuato
047100000821C1393D Arrotondam      S                   like(Importo045)
047200001201D0748C* flag di richiesta richiamo routine AutoSel2 per 2 volte (accade
047300001201  "  C*  per gli abbuoni quando è presente solo l'importo in conto e
047400001201  "  C*  c'è bisogno dell'automatismo che calcola quello concordato)
047500001201D0748D AutoSel2x2      S              1
047600950706     D*-------------
047700950706     D* Definizioni parametri standard chiamate moduli
047800950707     D* Bottoni
047900950707     D Fun             S              1     dim(100)
048000950707     D Ctr             S              1
048100950707     D* Definizioni parametri standard chiamate moduli
048200950707     D Esito           S              1
048300950707     D GesMsg          S              1
048400950707     D LenOut          S              9B 0
048500950905     D StrutturaO      S             10
048600950822     D Output          S           4000
048700970227     D Refresh         S              1
048800950707     D* Definizioni parametri non standard chiamate moduli
048900950823     D Risposta        S              1
049000950707     D LenIn           S              9B 0
049100950707     D Operazione      S              2
049200951019     D NrRiga          S                   like(PASNrRiga)
049300951019     D NrRigaM         S                   like(PASNrRigaM)
049400950704     D*-------------
049500950704     D* Variabili appoggio sempre presenti in tutte le anagrafiche
049600950721     D S1Nrr           S                   like(C1Rcd)
049700950606     D CurR            S              2S 0
049800950606     D CurC            S              2S 0
049900950606     D*-------------
050000950606     D*  Indici
050100070517D2164X*X               S              3S 0
050200070517  "  X*Y               S              3S 0
050300070517  "  X*Z               S              3S 0
050400070517  "  D X               S              5S 0
050500070517  "  D Y               S              5S 0
050600070517D2164D Z               S              5S 0
050700950706     D*-------------
050800950706     D* Definizione DS per recupero msg generati dai moduli
050900950706     D QLenMsg         S              9B 0
051000950706     D QFormat         S              8
051100950706     D QCallMsg        S             10
051200950706     D QCallStack      S              9B 0
051300950706     D QMsgTyp         S             10
051400950706     D QMsgKey         S              4
051500950706     D QWait           S              9B 0
051600950706     D QAction         S             10
051700950706     D QErrCod         DS
051800950706     D  Q1ErrLen                      9B 0 INZ(16)
051900950706     D  Q1LenRet                      9B 0 INZ(16)
052000950706     D  Q1ErrId                       7
052100950706     D QInfo           DS
052200950706     D  Q2BytRet                      9B 0
052300950706     D  Q2BytAva                      9B 0
052400950706     D  Q2Severity                    9B 0
052500950706     D  Q2MsgId                       7
052600950706     D  Q2MsgTyp                      2
052700950706     D  Q2MsgKey                      4
052800950727     D  Q2Reserve                    15
052900950706     D  Q2MsgLen                      9B 0
053000950706     D  Q2MsgLenAv                    9B 0
053100950706     D  Q2MsgTxt                     80
053200950825     D*-------------
053300000529D0529x*MsgID           s             27a   Dim(4) ctdata perrcd(1)
053400000529  "  x* aggiunti i msg per la gestione dell'arrotondamento manuale
053500000529  "  x* sulle partite in m.c. (COS1228 COS1229 COS1230)
053600000821D0529x*MsgID           s             27a   Dim(7) ctdata perrcd(1)
053700010226C1458x*MsgID ex-C1393  s             27a   Dim(8) ctdata perrcd(1)
053800010226  "  D MsgID           s             27a   Dim(11) ctdata perrcd(1)
053900010226  "  x*MsgT            s             70a   Dim(3)
054000010226C1458D MsgT            s             70a   Dim(11)
054100960524     D Sk70Dim         s              1a   Dim(70)
054200960524     D*-------------
054300960524     D RifPartita      ds                  inz
054400981022     D   NrPar                        9p 0
054500981022     D   GGPar                        2a
054600981022     D                                1a   inz('/')
054700981022     D   MMPar                        2a
054800981022     D                                1a   inz('/')
054900981022     D   AAPar                        2a
055000981022     D   SerPar                       4a
055100960524     D*-------------
055200960524     D GGMMAA          ds
055300960524     D  GG                            2s 0
055400960524     D  MM                            2s 0
055500960524     D  AA                            2s 0
055600950606     D*-------------
055700951019     D* campi importati
055800951019     D £NDREG        E DS                  extname(NDREG00F)    import
055900951019     D £NrDec          s              1P 0 import
056000951019     D £NrDecDiv       s              1P 0 import
056100000117C1309D ZDateISO        S               D
056200070418C2012D $ord            s              1s 0 inz(1)
056300070418C2012D $Ries1Da        s              1a   inz
056400940127     C*----------------------------------------------------*
056500940127     C*                MAIN LINE PROGRAM
056600940127     C*----------------------------------------------------*
056700951024     C*
056800951026     C                   MOVEL     Kpjbu         NDC045DS
056900951024     C*
057000951024     C* inizializzazione variabili
057100951026     C                   IF        Inz045 = *on
057200951024     C                   EXSR      INZVAR
057300951024     C                   ELSE
057400951024     C                   EXSR      MiniInzVar
057500951024     C                   ENDIF
057600940223     C*
057700940223     C     $FINE         DOWEQ     *OFF
057800940131     C     $GEST         CASEQ     'S1'          GESS1
057900941027     C                   ENDCS
058000950721     C                   Enddo
058100940325     C* fine programma
058200950721     C                   Exsr      ENDPGM
058300940225     C*
058400940325     C************************************************************
058500940325     C* FINE PROGRAMMA
058600940325     C************************************************************
058700950721     C     ENDPGM        Begsr
058800940325     C*
058900951026     C                   MOVEL     NDC045DS      Kpjbu
059000951020     C*
059100951020     C                   Eval      *inrt=*on
059200950630     C                   return
059300950807     C*
059400950721     C                   Endsr
059500940201     C************************************************************
059600940201     C* RICERCHE
059700940201     C************************************************************
059800950721     C     SEARCH        Begsr
059900940127     C*
060000940127     C* determino Riga/Colonna del cursore
060100950721     C                   Move      NRG           R$$
060200950721     C                   Move      NCL           C$$
060300951027     C                   Z-ADD     RIRI          CURR
060400940127     C                   Z-ADD     COCO          CURC
060500950727     C*
060600950801     C                   SELECT
060700951016     C*
060800951027     C* FMTS1  - Divisa
060900951027     C     H1NMFL        WHENEQ    'S1DIVISA'
061000951222     C*
061100951222     C     C1NRR         CHAIN     FMTS1                              21
061200951222     C*
061300951027     C                   RESET                   A015DS
061400951106     C                   MOVEL     '05'          OPZ15
061500951027     C                   MOVEL     A015DS        KPJBU
061600951027     C                   CALL      'ANA015R'
061700951027     C                   PARM                    KPJBA
061800951027     C                   MOVEL     KPJBU         A015DS
061900960304     C                   move      *on           *in32
062000960304     C* imposto gli attributi del sfl
062100960304     C                   EXSR      AtrS1
062200960304     C                   UPDATE    FMTS1
062300960304     C                   move      *off          *in32
062400951027     C*
062500951027     C*
062600960131     C*
062700960131     C* FMTS1  - Estremi partita
062800960131     C     H1NMFL        WHENEQ    'S1DTPAR'
062900960131     C     H1NMFL        OREQ      'S1NRPAR'
063000960131     C*
063100960131     C     C1NRR         CHAIN     FMTS1                              21
063200960314     C     S1NRPAR       IFNE      0
063300960314     C     S1DTPAR       ANDNE     0
063400960131     C*
063500960314     C                   RESET                   NDC060DS
063600960314     C                   MOVEL     '02'          OPZ060
063700960314     C                   MOVEL     REGCtb        Ctb060
063800960314     C                   MOVEL     C1Kcc         Kcc060
063900960314     C                   MOVEL     C1Ksc         Ksc060
064000960201     C                   CALLB     'XDT4000'
064100960314     C     DtPar060      PARM                    XDTAMG           10
064200960131     C                   PARM      S1DtPar       XDTGMA            6 0
064300960131     C                   PARM      4             XDTSTA            1 0
064400960314     C                   Z-ADD     S1NrPar       NrPar060
064500960314     C                   MOVE      S1SerPar      SerPar060
064600960325     C* passo l'unità di selezione perchè è campo obbligatorio della
064700960325     C* parzializzazione di questo pgm
064800040308     C                   MOVEL     C1UniSel      Unita060
064900040308     C*imposto chiave unique per la lista
065000040308     C                   MOVEL     ctb060        UQ1060
065100040308     C                   MOVEL     kcc060        UQ2060
065200040308     C                   MOVEL     ksc060        UQ3060
065300960314     C                   move      dtpar060      UQ4060
065400960314     C                   move      nrpar060      UQ5060
065500960314     C                   move      serpar060     UQ6060
065600960314     C                   MOVE      *BLANK        KPJBU
065700960314     C                   MOVEL     NDC060DS      KPJBU
065800960314     C                   CALL      'NDC060R'
065900960131     C                   PARM                    KPJBA
066000960314     C                   MOVEL     KPJBU         NDC060DS
066100960304     C                   move      *on           *in32
066200960304     C* imposto gli attributi del sfl
066300960304     C                   EXSR      AtrS1
066400960304     C                   UPDATE    FMTS1
066500960304     C                   move      *off          *in32
066600960314     C                   endif
066700960131     C*
066800950804     C                   Other
066900951016     C* Ricerca non abilitata
067000950804     C                   SETON                                        99
067100950804     C                   Eval      in(98) = *on
067200950731     C*
067300040308     C                   ENDSL
067400950731     C*
067500950612     C* imposto pos. del cursore
067600950612     C                   Z-ADD     CURR          H1RIGA
067700950612     C                   Z-ADD     CURC          H1COLO
067800940127     C*
067900950721     C                   Endsr
068000990803C1209 *
068100990803  "  C************************************************************
068200990803  "  C* F14=Commenti
068300990803  "  C************************************************************
068400990803  "  C     F14Comm       BEGSR
068500990803  "   *
068600990803  "   * determino Riga/Colonna del cursore
068700990803  "  C                   Move      NRG           R$$
068800990803  "  C                   Move      NCL           C$$
068900990803  "  C                   Z-ADD     RIRI          CURR
069000990803  "  C                   Z-ADD     COCO          CURC
069100990803  "   *
069200990803  "  C                   SELECT
069300990803  "   *
069400990803  "   * FMTC1  - Commenti sul conto
069500990803  "  C     H1NMRC        WHENNE    'C045S1'
069600990803  "  C                   RESET                   ANA725DS
069700990803  "  C                   MOVE      '02'          OPZ725
069800990803  "  C                   MOVE      'PPA'         TPA725
069900990803  "  C* inserire chiavi commenti
070000990803  "  C                   MOVEL     REGCtb        KY1725
070100990803  "  C                   SELECT
070200990803  "  C     SNatura002    wheneq    'C'
070300990803  "  C                   MOVEL     '700'         TPA725
070400990803  "  C                   MOVEL     C1Ksc         KY1725
070500990803  "  C                   MOVEL     '000'         KY3725
070600990803  "  C     SNatura002    wheneq    'F'
070700990803  "  C                   MOVEL     '001'         TPA725
070800990803  "  C                   MOVEL     c1KSC         KY1725
070900990803  "  C                   OTHER
071000990803  "  C                   MOVEL     'PDC'         TPA725
071100990803  "  C                   MOVEL     C1Kcc         KY1725
071200990803  "  C                   MOVEL     C1Ksc         KY2725
071300990803  "  C                   EndSl
071400990803  "  C                   MOVE      *BLANK        KPJBU
071500990803  "  C                   MOVEL     ANA725DS      KPJBU
071600990803  "  C                   CALL      'ANA725R'
071700990803  "  C                   PARM                    KPJBA
071800990803  "  C                   MOVEL     KPJBU         ANA725DS
071900990803  "  C* ritorno da PGM commenti
072000990803  "  C                   EXSR      RTN725
072100990803  "   *
072200990803  "  C* test esistenza commenti sul conto
072300990803  "  C                   EXSR      CTR724Kcc
072400990803  "   * Se esiste commento sul conto lo evidenzio
072500990803  "  C                   IF        Exi724 = *On
072600990803  "  C                   SETON                                        20
072700990803  "  C                   ELSE
072800990803  "  C                   SETOFF                                       20
072900990803  "  C                   ENDIF
073000990803  "   *
073100990803  "   * FMTS1  - Estremi partita
073200990803  "  C     H1NMRC        WHENEQ    'C045S1'
073300990803  "  C*    H1NMFL        WHENEQ    'S1DTPAR'
073400990803  "  C*    H1NMFL        OREQ      'S1NRPAR'
073500990803  "  C*    H1NMFL        OREQ      'S1SERPAR'
073600990803  "   *
073700990803  "  C     C1NRR         CHAIN     FMTS1                              21
073800990803  "   *Proseguo solo all'F14, il cursore è posizionato su una
073900990803  "   *riga del sfl e non sul "Segue.."
074000990803  "  C                   If        *In21      =   *Off
074100990803  "   *
074200990803  "  C                   Select
074300990803  "   *Se la partita esiste ed è recuperata da Database
074400990803  "   *gestisco l'F14=Commenti
074500990803  "  C                   When      S1NrPar <> 0   AND
074600990803  "  C                             S1DtPar <> 0   AND
074700990803  "  C                             H1NrAsInt <> 0
074800990803  "   *
074900990803  "  C                   RESET                   ANA725DS
075000990803  "  C                   MOVE      '02'          OPZ725
075100990803  "  C                   MOVE      'PPA'         TPA725
075200990803  "  C* inserire chiavi commenti
075300990803  "  C                   MOVEL     REGCtb        KY1725
075400990803  "  C                   MOVEL     C1Kcc         KY2725
075500990803  "  C                   MOVE      C1Ksc         KY2725
075600990803  "  C                   MOVEL     S1DtPar       KY3725
075700990803  "  C                   MOVEL     S1NrPar       KY4725
075800990803  "  C                   MOVE      S1SerPar      KY4725
075900990803  "  C                   MOVE      *BLANK        KPJBU
076000990803  "  C                   MOVEL     ANA725DS      KPJBU
076100990803  "  C                   CALL      'ANA725R'
076200990803  "  C                   PARM                    KPJBA
076300990803  "  C                   MOVEL     KPJBU         ANA725DS
076400990803  "  C* ritorno da PGM commenti
076500990803  "  C                   EXSR      RTN725
076600990803  "  C* imposto gli attributi del sfl
076700990803  "  C                   Move      *On           *In32
076800990803  "  C                   EXSR      AtrS1
076900990803  "  C                   UPDATE    FMTS1
077000990803  "  C                   Move      *Off          *In32
077100990803  "   *
077200990803  "   *Se la partita non esiste o non è recuperata da Database
077300990803  "   *non gestisco l'F14=Commenti
077400990803  "  C                   When      (S1NrPar = 0 AND S1DtPar = 0) OR
077500990803  "  C                             H1NrAsInt = 0
077600990803  "  C                   SETON                                        99
077700990803  "   * 'Ricerca abilitata solo sulle partite precaricate sul saldac'
077800990803  "  C                   Eval      in(72) = *on
077900990803  "   *
078000990803  "  C                   EndSl
078100990803  "  C*
078200990803  "   * all'F14, il cursore è posizionato sul "Segue.."
078300990803  "  C                   Else
078400990803  "  C* Ricerca non abilitata
078500990803  "  C                   SETON                                        99
078600990803  "  C                   Eval      in(98) = *on
078700990803  "  C                   EndIf
078800990803  "  C*
078900990803  "  C                   OTHER
079000990803  "  C* Ricerca non abilitata
079100990803  "  C                   SETON                                        99
079200990803  "  C                   Eval      in(98) = *on
079300990803  "   *
079400990803  "  C                   ENDSL
079500990803  "   *
079600990803  "   * imposto pos. del cursore
079700990803  "  C                   Z-ADD     CURR          H1RIGA
079800990803  "  C                   Z-ADD     CURC          H1COLO
079900990803  "   *
080000990803  "  C     XF14Comm      ENDSR
080100990803  "  C************************************************************
080200990803  "  C* GESTIONE RITORNO DA PGM DEI COMMENTI
080300990803  "  C************************************************************
080400990803  "  C     RTN725        BEGSR
080500990803  "  C*
080600990803  "  C* modo di ritorno
080700990803  "  C*
080800990803  "  C                   SELECT
080900990803  "  C* questi modi di utilizzo dei valori di ritorno dal
081000990803  "  C* pgm di manutenzione rcd di anagrafica sono delle
081100990803  "  C* proposte, normalmente sempre valide, ma modificabili
081200990803  "  C* per situazioni particolari
081300990803  "  C* 1 = F3
081400990803  "  C     RET725        WHENEQ    '1'
081500990803  "  C                   MOVE      *ON           $ESCI
081600990803  "  C                   MOVE      *ON           $FINE
081700990803  "  C                   MOVE      '1'           RET045
081800990803  "  C* 2 = F12
081900990803  "  C     RET725        WHENEQ    '2'
082000990803  "  C     OPR725        ANDNE     '1'
082100990803  "  C                   MOVE      *ON           $ESCI
082200990803  "  C*
082300990803  "  C                   ENDSL
082400990803  "  C*
082500990803  "  C* operazione eseguite dal pgm chiamato
082600990803  "  C*
082700990803  "  C                   SELECT
082800990803  "  C* 1 = eseguita scrittura o modifica commento
082900990803  "  C     OPR725        WHENEQ    '1'
083000990803  "  C* non reperisco se commento presente
083100990803  "  C* perchè lo faccio già in AtrS1
083200990803  "  C*
083300990803  "  C                   ENDSL
083400990803  "  C*
083500990803  "  C* funzione non eseguibile per errore :
083600990803  "  C*
083700990803  "  C                   SELECT
083800990803  "  C* 1 = funzione richiamata chiusa in errore
083900990803  "  C     ERR725        WHENEQ    '1'
084000990803  "  C                   MOVE      *ON           $ESCI
084100990803  "  C                   SETON                                        9599
084200990803  "  C* 2 = funzione richiamata senza la dovuta autorizzazione
084300990803  "  C     ERR725        WHENEQ    '2'
084400990803  "  C                   MOVE      *ON           $ESCI
084500990803  "  C                   SETON                                        9499
084600040308  "  C*  eventualmente gestire altri codici di errore
084700990803  "  C*
084800990803  "  C                   ENDSL
084900990803  "  C*
085000990803  "  C                   ENDSR
085100990803C1209 *
085200940506     C************************************************************
085300940506     C* RIEMPIMENTO TASTI FUNZIONE
085400940506     C************************************************************
085500950721     C     RIEKEY        Begsr
085600940506     C*
085700940506     C* Input :
085800940506     C* - $FX = schiera con tutti i testi disponibili
085900940506     C* - $FC = schiere con i flag di validità associati ai testi
086000940506     C* - $FR = schiere con i flag di REVERSE IMAGE
086100940506     C* - $ULK = ultimo testo caricato
086200940721     C* - $L01 = Lunghezza prima riga
086300940721     C* - $L02 = Lunghezza seconda riga
086400940506     C* Output :
086500940506     C* - $ULK
086600940609     C* - $KEY1= 1° riga di tasti funzione
086700940609     C* - $KEY2= 2° riga di tasti funzione
086800940506     C*
086900950522     C                   CALLB     'XKEYMSG'
087000940506     C* input
087100940506     C                   PARM                    $FX
087200940506     C                   PARM                    $FC
087300940506     C                   PARM                    $FR
087400940506     C                   PARM                    $ULK              3 0
087500940506     C* output
087600940506     C                   PARM                    $KEY1            79
087700940506     C                   PARM                    $KEY2            79
087800950301     C* input
087900040308     C                   PARM                    $L01              3 0
088000040308     C                   PARM                    $L02              3 0
088100940506     C*
088200060224C1913C                   PARM                    $ALLFUNCT       480
088300060224C1913C*
088400950721     C                   Endsr
088500940224     C************************************************************
088600940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
088700940224     C************************************************************
088800950721     C     GESPIE        Begsr
088900940224     C*
089000950721     C                   Select
089100940506     C* Lista
089200940506     C     $GEST         WHENEQ    'S1'
089300940506     C     $ULKS1        IFEQ      0
089400950721     C                   Exsr      F24S1
089500940506     C                   ELSE
089600950704     C                   WRITE     FMTZ1
089700950721     C                   Endif
089800950721     C                   Endsl
089900940506     C*
090000950721     C                   Endsr
090100960311     C************************************************************
090200960311     C* Chiamata programma con tasto personalizzato
090300960311     C************************************************************
090400960311     C     FPerS1        BEGSR
090500960311     C*
090600960311     C* salvo la DS di ingresso
090700040308     C                   MOVEL     NDC045DS      $SaveDS
090800960311     C*
090900960311     C* passare nei campi appositi della DS STD002DS il valore
091000960311     C* della chiave della VL univoca
091100040308     C*
091200040308     C                   MOVEL     NDC045DS      KPJBU
091300040308     C                   CALL      $PGF1(PosFun)                        22
091400960311     C                   PARM                    KPJBA
091500960311     C                   PARM                    NOMPGM
091600960311     C*
091700960311     C* ripristino la DS di ingresso
091800040308     C                   MOVEL     $saveDS       NDC045DS
091900960311     C*
092000960311     C                   ENDSR
092100940127     C************************************************************
092200940131     C* GESTIONE LISTA
092300940127     C************************************************************
092400950721     C     GESS1         Begsr
092500940223     C* inizializzazione videata
092600950721     C     $InzS1        IFEQ      *ON
092700950721     C                   Exsr      InzS1
092800950721     C                   Endif
092900950728      *
093000950727      * emissione piede videata se proveniente da altra
093100950727      * salvataggio valore formato di provenienza
093200940202     C     $LASTV        IFNE      'S1'
093300950801     C                   WRITE     FMTT1
093400950727     C                   Exsr      GESPIE
093500950721     C                   Move      'S1'          $LASTV
093600950721     C                   Endif
093700950801     C*
093800950721     C                   Eval      *in09=c1mode
093900951027     C*
094000950801     C                   WRITE     FMTD1
094100951027     C* Indicatori sflmsg
094200950804     C                   if        *in99=*on
094300950804     C                   Exsr      SflMsgIndy
094400950804     C                   Endif
094500950801     C*
094600960311     C*
094700960311     C* inizializzazione indice tasti personalizzati
094800960311     C                   Eval      PosFun = 25
094900960311     C*
095000950801     C                   EXFMT     FMTC1
095100951020     C                   IF        SFLNRR <> 0
095200951020     C                   Z-ADD     SFLNRR        C1Rcd
095300951020     C                   ENDIF
095400980114     C* memorizzo l'avvenuta EXFMT per richiamo a pgm di sola finestra
095500980114     C                   EVAL      WExf = *on
095600950804      * Clear skiera salvataggio indicatori per sflmsgid
095700950804     C                   Clear                   In
095800940506     C*
095900940506     C* controllo abilitazione tasti funzione
096000950721     C                   Exsr      CTRKS1
096100941205     C*
096200940506     C     *IN99         IFEQ      *OFF
096300940127     C*
096400950721     C                   Select
096500940127     C* F3=Fine
096600940506     C     $TASTO        WHENEQ    F03
096700950721     C                   Exsr      F03S1
096800950712     C* F4=Fine
096900950712     C     $TASTO        WHENEQ    F04
097000950721     C                   Exsr      Search
097100950704     C* F5=Fine
097200950704     C     $TASTO        WHENEQ    F05
097300950721     C                   Exsr      F05S1
097400950707     C* F6=Conferma
097500950707     C     $TASTO        WHENEQ    F06
097600951120     C     $TASTO        OREQ      F11
097700950721     C                   Exsr      F06S1
097800950801     C* F8=Successivo
097900950801     C     $TASTO        WHENEQ    F08
098000950801     C                   Exsr      F08S1
098100940131     C* F12=Ritorno
098200940506     C     $TASTO        WHENEQ    F12
098300950721     C                   Exsr      F12S1
098400990803C1209 *
098500990803  "  C* F14=Commenti
098600990803  "  C     $TASTO        WHENEQ    F14
098700990803  "  C                   Exsr      F14Comm
098800990803C1209 *
098900960201     C* F15=Interrogazione conto
099000960201     C     $TASTO        WHENEQ    F15
099100960201     C                   EXSR      F15S1
099200960109     C* F19=Righe immissione
099300960109     C     $TASTO        WHENEQ    F19
099400960109     C                   Exsr      F19S1
099500940224     C* F24=Altri tasti
099600940506     C     $TASTO        WHENEQ    F24
099700950721     C                   Exsr      F24S1
099800940127     C* ROLL-UP
099900940506     C     $TASTO        WHENEQ    ROLLUP
100000950801     C                   Exsr      RollS1
100100950801     C                   EVAL      C1Rcd = WMax - SflPag + 1
100200960311     C*
100300960311     C* Personalizzata
100400960311     C     $PGF1(PosFun) WHENNE    *BLANK
100500960311     C                   EXSR      FperS1
100600960311     C*
100700940127     C*
100800941027     C                   OTHER
100900940127     C* CONTROLLO DATI
101000950721     C                   Exsr      CTRC1
101100940201     C     *IN99         IFEQ      *OFF
101200950721     C                   Exsr      CTRS1
101300951018     C                   EndIF
101400990616C1185C*
101500990616  "  C*F20=Sconti condizionati
101600990616  "  C     $TASTO        IfEq      F20
101700990616  "  C                   EXSR      Call545
101800000302C1185C                   EndIf
101900941205     C*
102000950721     C                   Endsl
102100940127     C*
102200950721     C                   Endif
102300941205     C*
102400950721     C                   Endsr
102500940127     C************************************************************
102600940131     C* INIZIALIZZAZIONE LISTA
102700940127     C************************************************************
102800950721     C     InzS1         Begsr
102900940302     C* pulizia SFL
103000940128     C                   SETOFF                                         3031
103100940131     C                   WRITE     FMTC1
103200940128     C                   SETON                                          31
103300950921     C*
103400950731     C                   Clear                   H1Riga
103500950731     C                   Clear                   H1Colo
103600970327     C*
103700951018     C* caricamento dati su sflctl
103800950731     C                   EXSR      RieC1
103900951020     C* controllo dati su sflctl per avere ulteriori dft e le decodifiche
104000951020     C                   EXSR      CtrC1
104100951020     C                   Move      *OFF          $InzS1
104200950731     C*
104300951027     C* pulizia campi lavoro totali importi
104400951030     C                   Clear                   WImportD
104500951030     C                   Clear                   WImporto
104600951027     C                   Clear                   WAbbuonoD
104700951027     C                   Clear                   WAbbuono
104800951027     C                   Clear                   WScontoD
104900951027     C                   Clear                   WSconto
105000951027     C                   Clear                   WRitenuta
105100951027     C                   Clear                   WDiffCam
105200951121     C                   Clear                   FMTD1
105300940128     C*
105400980114     C* richiamo filtro di selezione (se richiesto)
105500940128     C*
105600980114     C                   RESET                   NDC079DS
105700980410     C* riempimento campi di input della DS di passaggio parametri
105800980410     C                   EVAL      SNatura079 = SNatura002
105900980410     C                   EVAL      UnitaS079 = UniSel045
106000980410     C                   EVAL      Kcc079 = C1Kcc
106100980410     C                   EVAL      Ksc079 = C1Ksc
106200980410     C                   EVAL      KccKscD079 = C1KccKscD
106300980114     C                   IF        Filtro045 = XscOn
106400980410     C* forzo il video del NDC079R a rinfrescare lo sfondo
106500980116     C                   EVAL      WExf = *on
106600980114     C                   EXSR      Call079
106700980114     C                   ENDIF
106800980114     C*
106900980114     C* CARICAMENTO SFL A PAGINE
107000980114     C*
107100940201     C                   Z-ADD     0             S1NRR
107200940128     C                   Z-ADD     0             WMAX
107300950705     C* visualizzo il sfl troncato
107400950721     C                   Eval      c1mode=*on
107500980115     C* valorizzo la varibile di proposta automatica importo in conto con
107600980115     C* il totale da saldacontare
107700980115     C                   Eval      AutoImpD    = C1ImportD
107800951128     C*
107900951128     C* se gestione partite selezionate dai movimenti banca
108000980123     C                   if        Opz045 = '01'  and
108100980123     C* e non è stato richiesto il ripristino dati manuali
108200980127     C                             Reset045 = XscOff
108300951128     C* il sfl lo carico direttamente dalla schiera in ingresso
108400951128     C                   Exsr      CarBanS1
108500961121     c                   endif
108600951128     C*
108700961121     C* se gestione partite selezionate dai movimenti banca, ma non ha
108800961121     c* caricato nulla oppure
108900961121     C                   if        Opz045 = '01' and *in30 = *off or
109000961121     C* se partite non selezionate da movimenti banca
109100961121     C                             Opz045 <> '01'
109200961121     C* legge da NDPPA
109300950809     C* Posizionamento su file pilota
109400950809     C                   Exsr      SETANA
109500951026     C*
109600951026     C* Lettura file pilota
109700951128     C                   Exsr      REDANA
109800940131     C*
109900951031     C                   IF        $EndFile = *off
110000951128     C*
110100951128     C* gestione partite a scadenze prevista in tabella
110200010104C0958X***                IF        GPS0IP = '1'       and
110300990216C0958C* e confermata nella chiamata a X20ScadR
110400990216C0958C                   IF        X20ParScd = 'S'    and
110500960703     C* solo se il conto NON è un percipiente
110600960515     C                             ( SNatura002<>'F' or Percipi002<>*on )
110700951027     C                   Exsr      CarScaS1
110800951027     C* gestione partite non a scadenze
110900951128     C                   ELSE
111000951027     C                   Exsr      CarParS1
111100951128     C                   ENDIF
111200950801     C*
111300951031     C                   ENDIF
111400951128     C*
111500961121     C                   ENDIF
111600951031     C*
111700960109     C* salvo il numero della 1a riga d'immissione per la gestione F19
111800960109     C                   EVAL      RigaVuota = WMAX + 1
111900950801     C* carico righe vuote a riempimento pagina
112000951026     C                   Exsr      RollS1
112100950801     C*
112200950801     C                   EVAL      C1Rcd = 1
112300940127     C*
112400950921     C                   ENDSR
112500950921     C************************************************************
112600950921     C* Riempie sflctl
112700950921     C************************************************************
112800950921     C     RieC1         Begsr
112900971128     C*
113000971128     C                   SETOFF                                       37
113100950921     C*
113200951019     C     RegDtReg      IFGT      Dt0
113300960201     C                   CALLB     'XDT4000'
113400951019     C                   PARM      RegDtReg      XDTAMG           10
113500950914     C     C1DtReg       PARM                    XDTGMA            6 0
113600950914     C                   PARM      2             XDTSTA            1 0
113700950801     C                   ELSE
113800950801     C                   MOVE      *zero         C1DtReg
113900950801     C                   ENDIF
114000951019     C                   EVAL      C1NrReg  = RegNrReg
114100951019     C                   EVAL      C1SerReg = RegSerReg
114200970226     C                   EVAL      C1TpRegz = REGTpRegz
114300951019     C     RegDtDoc      IFGT      Dt0
114400960201     C                   CALLB     'XDT4000'
114500951019     C                   PARM      RegDtDoc      XDTAMG           10
114600950914     C     C1DtDoc       PARM                    XDTGMA            6 0
114700950914     C                   PARM      2             XDTSTA            1 0
114800950801     C                   ELSE
114900950801     C                   MOVE      *zero         C1DtDoc
115000950801     C                   ENDIF
115100951019     C                   EVAL      C1NrDoc  = RegNrDoc
115200951019     C                   EVAL      C1SerDoc = RegSerDoc
115300951019     C                   EVAL      C1Divisa = RegDivisa
115400951026     C                   EVAL      C1ImportD = ImportD045
115500951026     C                   EVAL      C1Importo = Importo045
115600951122     C* se gestione a cambio storico
115700951122     C                   IF        CmbSto045 = *on
115800951122     C* imposto il cambio di testata a 0
115900951122     C                   EVAL      C1Cambio  = 0
116000951122     C                   ELSE
116100960110     C                   EVAL      C1Cambio  = Cambio045
116200951122     C                   ENDIF
116300951031     C                   EVAL      C1Kcc     = Kcc045
116400951031     C                   EVAL      C1Ksc     = Ksc045
116500951031     C* Richiamo modulo controllo conto
116600951031     C                   CLEAR                   C1KccKscD
116700951031     C                   IF        C1Kcc > *blank and C1Ksc > *blank
116800951031     C                   RESET                   ND002DS
116900951031     C                   EVAL      LenOut = %Size(ND002DS)
117000951031     C                   CallB     'NDMVC002'
117100951031     C                   PARM                    xscsoc
117200951031     C                   PARM                    C1Kcc
117300951031     C                   PARM                    C1Ksc
117400951031     C                   PARM                    *omit
117500951031     C                   PARM      *OFF          GesMsg
117600951031     C                   PARM      *OFF          Esito
117700951031     C                   PARM      'ND002DS'     StrutturaO
117800951031     C                   PARM                    ND002DS
117900951031     C                   PARM                    LenOut
118000040308     C     Esito         IFEQ      *off
118100971128     C* descrizione conto
118200951031     C                   Movel     DesBrev002    C1KccKscD
118300971128     C* blocco amministrativo su conto
118400971128     C                   EVAL      C1BkAmm = BkAmm002
118500971128     C                   EVAL      C1BkAmmD = *blank
118600971128     C* se esiste un blocco amministrativo per conto
118700980205     C                   IF        C1BkAmm <> *blank
118800971128     C* reperisco descrizione lunga blocco
118900971128     C                   EVAL      XtbRic = '1'
119000971128     C                   EVAL      XtbAzi = XscSoc
119100971128     C                   EVAL      XtbKey = *zero
119200971128     C                   EVAL      XtbCod = '0AB'
119300971128     C                   MOVE      C1BkAmm       XtbKey
119400971128     C                   CALLB     'XATB'
119500971128     C                   PARM                    XAtbDS
119600971128     C                   IF        XtbErr <> '0'
119700971128     C                   EVAL      C1BkAmmD = *all'?'
119800971128     C                   ELSE
119900971128     C                   EVAL      ANG0ABDS = XtbUni
120000971128     C                   EVAL      C1BkAmmD = Des0AB
120100971128     C                   ENDIF
120200971128     C                   ENDIF
120300040308     C                   ENDIF
120400040308     C                   ENDIF
120500960315     C                   EVAL      C1UniSel  = UniSel045
120600951116     C                   Z-ADD     0             C1PercSco1
120700951116     C                   Z-ADD     0             C1PercSco2
120800951116     C                   MOVE      Xscon         C1On1
120900951027     C                   MOVE      Xscoff        C1Off1
121000951106     C                   MOVE      Xscoff        C1ForzaSco
121100951017     C*
121200950731     C                   Endsr
121300950731     C************************************************************
121400980114     C* Richiamo filtro di selezione
121500950731     C************************************************************
121600980114     C     Call079       BEGSR
121700950921     C*
121800980114     C* passo eventuale EXFMT per chiamata a pgm di sola finestra
121900980114     C                   EVAL      Exf079 = WExf
122000980114     C*
122100980115     C                   MOVEL     NDC079DS      KPJBU
122200980115     C*
122300980114     C                   CALLB     'NDC079R'
122400980115     C                   PARM                    KPJBA
122500980115     C                   PARM                    SOC001
122600980115     C*
122700980115     C                   MOVEL     KPJBU         NDC079DS
122800980114     C*
122900980114     C* ritorno pgm
123000980114     C                   EXSR      Rtn079
123100980114     C*
123200980114     C                   ENDSR
123300980114     C************************************************************
123400980114     C* GESTIONE RITORNO DA filtro selezioni
123500980114     C************************************************************
123600980114     C     RTN079        BEGSR
123700980114     C*
123800980114     C* modo di ritorno
123900980114     C*
124000980114     C                   SELECT
124100980114     C* questi modi di utilizzo dei valori di ritorno dal
124200980114     C* pgm di manutenzione rcd di anagrafica sono delle
124300980114     C* proposte, normalmente sempre valide, ma modificabili
124400980114     C* per situazioni particolari
124500980114     C* 1 = F3
124600980114     C     RET079        WHENEQ    '1'
124700980114     C* 2 = F12
124800980114     C     RET079        WHENEQ    '2'
124900990908C1226C                   EVAL      Ret045 = Ret079
125000990908C1226C                   EXSR      EndPgm
125100980114     C* 0 = Nessun tasto di uscita
125200980114     C     RET079        WHENEQ    '0'
125300980415     C* imposto sul sflctl i dati che possono essere variati mediante filtro
125400980415     C                   EVAL      C1UniSel = UnitaS079
125500980114     C                   ENDSL
125600980114     C*
125700980114     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
125800980114     C* di sola finestra
125900980114     C                   EVAL       WEXF = *off
126000980114     C*
126100980114     C                   ENDSR
126200990616C1185 ************************************************************
126300990616  "   * Gestione chiamata sconti condizionati
126400990616  "   ************************************************************
126500990616  "  C     Call545       BEGSR
126600990616  "   *
126700990616  "   * determino Riga/Colonna del cursore
126800990616  "  C                   Move      NRG           R$$
126900990616  "  C                   Move      NCL           C$$
127000990616  "  C                   Z-ADD     RIRI          CURR
127100990616  "  C                   Z-ADD     COCO          CURC
127200990616  "   *
127300990616  "  C                   RESET                   NDC545DS
127400990616  "   *
127500990616  "  C                   Eval      Societa545  = XScSoc
127600990617  "   * NDC546r non riesce a leggere sul sfl in Share i dati in Output
127700990617  "   * che non sono del Sfl
127800990617  "  C                   Eval      Kcc545      = C1Kcc
127900990617  "  C                   Eval      Ksc545      = C1Ksc
128000990617  "  C                   Eval      KccKscD545  = C1KccKscD
128100990617  "  C                   Eval      TotIncD545  = C1ImportD
128200990617  "  C                   Eval      TotSelD545  = D1ImportD
128300990617  "   * NDC546r non ha sul sfl tutti i dati necessari per ripristinare
128400990617  "   * gli attributi del saldaconto
128500990617  "  C                   Eval      X20ParS545  = X20ParScd
128600990616  "  C                   Eval      CmbSto545   = CmbSto045
128700990617  "   * NDC546r deve avere la data dalla quale calcolare il n°gg
128800990617  "   * x reperire la % di sconto condizionato
128900990616  "  C                   Eval      DtVal545    = DtVal045
129000990616  "   * NDC546r deve sapere come inizializzare la eventuale window
129100990616  "   * in base all'emissione di window già fatte dal NDC045R
129200990616  "  C                   Eval      PrimaWi545  = WExf
129300990616  "   *
129400990616  "  C                   CLEAR                   KPJBU
129500990616  "  C                   Movel     NDC545DS      KPJBU
129600990616  "  C                   CALL      'NDC545R'
129700990616  "  C                   PARM                    KPJBA
129800060713C1185C                   PARM                    ANG0IPDS
129900060713D2021C                   Movel     KPJBU         NDC545DS
130000060713C1185 *
130100990616  "   * ritorno da PGM gestione
130200990621  "  C                   EXSR      RTN545
130300990616  "   *
130400990616  "   *
130500990616  "   *Aggiorno la variabile che mi dice come inizializzare
130600990616  "   *l'emissione delle window sucessive
130700990616  "  C                   Eval      WExf = PrimaWi545
130800990616  "   *
130900990616  "   * imposto pos. del cursore
131000990616  "  C                   Z-ADD     CURR          H1RIGA
131100990616  "  C                   Z-ADD     CURC          H1COLO
131200990616  "   *
131300990616  "   *
131400990616  "  C     XCall545      ENDSR
131500990616  "   ************************************************************
131600990616  "   * GESTIONE RITORNO DA PGM DI GESTIONE Sconti condizionati
131700990616  "   ************************************************************
131800990616  "  C     RTN545        BEGSR
131900990616  "   *
132000990616  "  C*
132100990616  "  C* funzione non eseguibile per errore :
132200990616  "  C*
132300990616  "  C                   SELECT
132400990616  "  C* 1 = funzione richiamata chiusa in errore
132500990616  "  C     ERR545        WHENEQ    '1'
132600990616  "  C                   MOVE      *ON           $ESCI
132700990616  "  C                   SETON                                        9399
132800990616  "  C* 2 = funzione richiamata senza la dovuta autorizzazione
132900990616  "  C     ERR545        WHENEQ    '2'
133000990616  "  C                   MOVE      *ON           $ESCI
133100990616  "  C                   SETON                                        9499
133200990616  "   *
133300990616  "  C                   ENDSL
133400990616  "   *
133500040308C1185C     XRtn545       ENDSR
133600980114     C************************************************************
133700980114     C* CARICAMENTO sfl con le partite selezionate dai mov.banca
133800980114     C************************************************************
133900980114     C     CarBanS1      Begsr
134000980114     C*
134100950907     C                   Eval      $EndFile = *off
134200970327     C                   MOVE      *blank        BkAmm
134300010105C1458C                   MOVE      *blank        BkAmmPSC
134400950907     C*
134500070417C2012 * stabilisco ordine del sfl
134600070417  "  C                   exsr      DefOrd
134700070417C2012 *
134800951128     C* Leggo dalla DS multipla per caricare la lista
134900950907     C*
135000001027D0748X****************** DO        100           X
135100001027D0748C                   DO        $DimSK        X
135200951128     C     X             OCCUR     Sk2PArtite
135300951128     C*
135400951128     C* considero solo le partite del conto in esame
135500951128     C                   IF        Sk2Kcc = C1Kcc  and  Sk2Ksc = C1Ksc
135600951027     C*
135700070418C2012X*****specifiche asteriscate ed integrate in "Pre_Ries1DaB"
135800070418  "  X****************** CLEAR                   FMTS1
135900070418  "  X*****per ogni rcd caricato devo attivare la sflnxtchg perchè ho già
136000070418  "  X*****impostato l'importo in conto
136100070418C2012X****************** SETON                                        32
136200070418C2012 * preparo scrittura sfl/work-file
136300070418C2012C                   exsr      Pre_Ries1DaB
136400950822     C*
136500950801     C* riempio la riga da database
136600951128     C                   Exsr      RieS1DaBan
136700950906     C*
136800070418C2012X*****specifiche asteriscate ed integrate in "Post_Ries1DaB"
136900070418  "  X*****scrivo il rcd solo se è considerato valido
137000070418  "  X****************** IF        $RcdOk = *on
137100070418  "  X******************
137200070418  "  X****************** ADD       1             S1NRR
137300070418  "  X*****imposto gli attributi del sfl
137400070418  "  X****************** EXSR      AtrS1
137500070418  "  X******************
137600070418  "  X****************** WRITE     FMTS1
137700070418  "  X******************
137800070418C2012X****************** ENDIF
137900070418C2012 * scrittura sfl/work-file
138000070418C2012C                   exsr      Post_Ries1DaB
138100951128     C*
138200951128     C                   ELSE
138300951128     C* alla 1a riga vuota considero finite le partite
138400951128     C                   IF        Sk2Kcc = *blank
138500951128     C                   LEAVE
138600951128     C                   ENDIF
138700951128     C                   ENDIF
138800951128     C*
138900951128     C                   ENDDO
139000950921     C*
139100070418C2012X*****specifiche asteriscate ed integrate in "Fine_Ries1"
139200070418  "  X****************** SETOFF                                       32
139300070418  "  X******************
139400070418  "  X*****impedisco il rollup con righe vuote
139500070418  "  X****************** MOVE      $EndFile      *in33
139600070418  "  X******************
139700070418  "  X****************** Z-ADD     S1NRR         WMAX
139800070418  "  X*****30=sfldsp ***
139900070418C2012X**** s1nrr ******* comp      *zeros                             30
140000070418C2012 * ultime operazioni scrittura sfl
140100070418  "  C                   eval      $Ries1Da = 'B'
140200070418C2012C                   exsr      Fine_Ries1
140300950707      *
140400070418C2012X****************** Endsr
140500070418C2012C     XCarBanS1     Endsr
140600951128     C************************************************************
140700951128     C* CARICAMENTO sfl per scadenze
140800951128     C************************************************************
140900951128     C     CarScaS1      Begsr
141000951128     C*
141100951128     C                   Eval      $EndFile = *off
141200070417C2012 *
141300070417  "   * stabilisco ordine del sfl
141400070417C2012C                   exsr      DefOrd
141500951128     C*
141600951128     C* Leggo dal file anagrafico per caricare la lista
141700951128     C*
141800951128     C     $EndFile      DOueq     *on
141900951128     C*
142000951128     C* ciclo per le scadenze recuperate
142100951128     C*
142200951128     C                   DO        X20Index      x
142300960402     C*
142400960402     C* se la scadenza è chiusa (in divisa) la salto
142500070417C2012 * oppure se la scadenza non è richiesta, la salto
142600960402     C                   IF        ImpRD(x) = 0
142700070417C2012C                             or  DtScax(x) < DtScaDa079 or
142800070417C2012C                             DtScax(x) > DtScaA079
142900960402     C                   ITER
143000960402     C                   ENDIF
143100951128     C*
143200070418C2012X*****specifiche asteriscate ed integrate in "Pre_Ries1DaS"
143300070418  "  X****************** CLEAR                   FMTS1
143400070418  "  X*****non c'è bisogno di attivare la sflnxtchg per i rcd caricati
143500070418C2012X****************** SETOFF                                       32
143600070418C2012 * preparo scrittura sfl/work-file
143700070418C2012C                   exsr      Pre_Ries1DaS
143800951128     C*
143900951128     C* riempio la riga da database
144000980115     C*  dati specifici scadenze
144100951128     C                   Exsr      RieS1DaSca
144200980115     C*  dati comuni partite
144300980115     C                   Exsr      RieS1DaPar
144400960524     C*
144500070418C2012X*****specifiche asteriscate ed integrate in "Post_Ries1DaS"
144600070418  "  X*****scrivo il rcd solo se è considerato valido
144700070418  "  X****************** IF        $RcdOk = *on
144800070418  "  X******************
144900070418  "  X****************** ADD       1             S1NRR
145000070418  "  X*****se è richiesta la proposta automatica
145100070418  "  X****************** IF        PropAut045 = XscOn and
145200070418  "  X*****e il rcd è stato selezionato
145300070418  "  X******************           S1Selez = 'X'
145400070418  "  X*****attivo SFLNXTCHG
145500070418  "  X****************** SETON                                        32
145600070418  "  X****************** ENDIF
145700070418  "  X*****imposto gli attributi del sfl
145800070418  "  X****************** EXSR      AtrS1
145900070418  "  X******************
146000070418  "  X****************** WRITE     FMTS1
146100070418  "  X******************
146200070418  "  X****************** SETOFF                                       32
146300070418C2012X****************** ENDIF
146400070418C2012 * scrittura sfl/work-file
146500070418C2012C                   exsr      Post_Ries1DaS
146600951128     C*
146700951128     C                   ENDDO
146800951128     C*
146900951128     C* leggo un nuovo rcd da database
147000951128     C                   Exsr      REDANA
147100951128     C*
147200951128     C                   Enddo
147300951128     C*
147400070418C2012X*****specifiche asteriscate ed integrate in "Fine_Ries1"
147500070418  "  X****************** SETOFF                                       32
147600070418  "  X******************
147700070418  "  X*****impedisco il rollup con righe vuote
147800070418  "  X****************** MOVE      $EndFile      *in33
147900070418  "  X******************
148000070418  "  X****************** Z-ADD     S1NRR         WMAX
148100070418  "  X*****30=sfldsp ***
148200070418C2012X**** s1nrr ******* comp      *zeros                             30
148300070418C2012 * ultime operazioni scrittura sfl
148400070418  "  C                   eval      $Ries1Da = 'S'
148500070418C2012C                   exsr      Fine_Ries1
148600951128      *
148700070418C2012X****************** Endsr
148800070418C2012C     XCarScaS1     Endsr
148900070418C2012 ************************************************************
149000070418  "   * Definizione ordine di caricamento del sfl
149100070418  "   ************************************************************
149200070418  "  C     DefOrd        Begsr
149300070418  "   *
149400070418  "   * se non chiesta scadenza, carico direttamente sfl
149500070418  "   * in ordine di partita/scadenza
149600070418  "  C                   eval      $ord = 1
149700070418  "   *
149800070418  "  C                   select
149900070418  "   *
150000070418  "   * se chiesta scadenza, carico il work-file poi dal work-file
150100070418  "   * ordinato per scadenza/partita, carico il sfl
150200070418  "  C                   when      DtScaDa079<>Dt0 or DtScaA079<>Dt9
150300070418  "  C                   eval      $ord = 2
150400070418  "   *
150500070418  "   * preparo il work-file
150600070418  "  C                   if        %open(ndsdc01w)
150700070418  "  C                   close     ndsdc01w
150800070418  "  C                   endif
150900070418  "   *   carico il work-file in qtemp e lo pulisco
151000070418  "  C                   callb     'NDC079C'
151100070418  "  C                   parm                    Kpjba
151200070418  "  C                   if        not %open(ndsdc01w)
151300070418  "  C                   open      ndsdc01w
151400070418  "  C                   endif
151500070418  "   *
151600070418  "  C                   endsl
151700070418  "   *
151800070418  "  C     XDefOrd       Endsr
151900070418  "   ************************************************************
152000070418  "   * preparo scrittura sfl/work-file da movimenti banca
152100070418  "   ************************************************************
152200070418  "  C     Pre_Ries1DaB  Begsr
152300070418  "   *
152400070418  "  C                   select
152500070418  "   *
152600070418  "   * ordinamento per partita/scadenza
152700070418  "  C                   when      $ord = 1
152800070418  "   *
152900070418  "  C                   CLEAR                   FMTS1
153000070418  "  C*   per ogni rcd caricato devo attivare la sflnxtchg
153100070418  "  C*   perchè ho già impostato l'importo in conto
153200070418  "  C                   SETON                                        32
153300070418  "   *
153400070418  "   * ordinamento per scadenza/partita
153500070418  "  C                   when      $ord = 2
153600070418  "  C                   clear                   Ndsdc01
153700070418  "   *
153800070418  "  C                   endsl
153900070418  "   *
154000070418  "  C     XPre_Ries1DaB Endsr
154100070418  "   ************************************************************
154200070418  "   * scrittura sfl/work-file da movimenti banca
154300070418  "   ************************************************************
154400070418  "  C     Post_Ries1DaB Begsr
154500070418  "  C*
154600070418  "  C* scrivo il rcd solo se è considerato valido
154700070418  "  C                   IF        $RcdOk = *on
154800070418  "  C*
154900070418  "  C                   select
155000070418  "   *
155100070418  "   * ordinamento per partita/scadenza
155200070418  "  C                   when      $ord = 1
155300070418  "  C                   ADD       1             S1NRR
155400070418  "  C*   imposto gli attributi del sfl
155500070418  "  C                   EXSR      AtrS1
155600070418  "  C*
155700070418  "  C                   WRITE     FMTS1
155800070418  "   *
155900070418  "   * ordinamento per scadenza/partita
156000070418  "  C                   when      $ord = 2
156100070418  "  C                   write     Ndsdc01
156200070418  "   *
156300070418  "  C                   endsl
156400070418  "  C*
156500070418  "  C                   ENDIF
156600070418  "   *
156700070418  "  C     XPost_Ries1DaBEndsr
156800070418  "   ************************************************************
156900070418  "   * preparo scrittura sfl/work-file con scadenze
157000070418  "   ************************************************************
157100070418  "  C     Pre_Ries1DaS  Begsr
157200070418  "   *
157300070418  "  C                   select
157400070418  "   *
157500070418  "   * ordinamento per partita/scadenza
157600070418  "  C                   when      $ord = 1
157700070418  "  C                   CLEAR                   FMTS1
157800070418  "  C*   non c'è bisogno di attivare la sflnxtchg per i rcd caricati
157900070418  "  C                   SETOFF                                       32
158000070418  "   *
158100070418  "   * ordinamento per scadenza/partita
158200070418  "  C                   when      $ord = 2
158300070418  "  C                   clear                   Ndsdc01
158400070418  "   *
158500070418  "  C                   endsl
158600070418  "   *
158700070418  "  C     XPre_Ries1DaS Endsr
158800070418  "   ************************************************************
158900070418  "   * scrittura sfl/work-file da scadenze
159000070418  "   ************************************************************
159100070418  "  C     Post_Ries1DaS Begsr
159200070418  "   *
159300070418  "  C* scrivo il rcd solo se è considerato valido
159400070418  "  C                   IF        $RcdOk = *on
159500070418  "  C*
159600070418  "  C                   select
159700070418  "   *
159800070418  "   * ordinamento per partita/scadenza
159900070418  "  C                   when      $ord = 1
160000070418  "  C                   ADD       1             S1NRR
160100070418  "  C*   se è richiesta la proposta automatica
160200070418  "  C                   IF        PropAut045 = XscOn and
160300070418  "  C*   e il rcd è stato selezionato
160400070418  "  C                             S1Selez = 'X'
160500070418  "  C*   attivo SFLNXTCHG
160600070418  "  C                   SETON                                        32
160700070418  "  C                   ENDIF
160800070418  "  C*   imposto gli attributi del sfl
160900070418  "  C                   EXSR      AtrS1
161000070418  "  C*
161100070418  "  C                   WRITE     FMTS1
161200070418  "  C*
161300070418  "  C                   SETOFF                                       32
161400070418  "   *
161500070418  "   * ordinamento per scadenza/partita
161600070418  "  C                   when      $ord = 2
161700070418  "  C                   write     Ndsdc01
161800070418  "   *
161900070418  "  C                   endsl
162000070418  "   *
162100070418  "  C                   ENDIF
162200070418  "   *
162300070418  "  C     XPost_Ries1DaSEndsr
162400070418  "   ************************************************************
162500070418  "   * ultime operazioni scrittura sfl
162600070418  "   ************************************************************
162700070418  "  C     Fine_Ries1    Begsr
162800070418  "   *
162900070418  "  C                   select
163000070418  "   *
163100070418  "   * ordinamento per partita/scadenza
163200070418  "  C                   when      $ord = 1
163300070418  "  C                   move      *off          *in32
163400070418  "   *   impedisco il rollup con righe vuote
163500070418  "  C                   MOVE      $EndFile      *in33
163600070418  "  C                   Z-ADD     S1NRR         WMAX
163700070418  "   *   30=sfldsp
163800070418  "  C     s1nrr         comp      *zeros                             30
163900070418  "   *
164000070418  "   * ordinamento per scadenza
164100070418  "  C                   when      $ord = 2
164200070418  "   *   carico sfl da workfile in ordine di scad.
164300070418  "  C                   exsr      RieS1
164400070418  "   *
164500070418  "  C                   endsl
164600070418  "   *
164700070418  "  C     XFine_Ries1   Endsr
164800070418  "   ************************************************************
164900070418  "   * CARICAMENTO sfl da work-file
165000070418  "   ************************************************************
165100070418  "  C     RieS1         Begsr
165200070418  "   *
165300070418  "  C                   clear                   Fmts1
165400070418  "   *
165500070419  "  C     *loval        setll     ndsdc01w
165600070419  "  C                   read      ndsdc01w                               26
165700070418  "   *
165800070419  "  C                   dow       *in26 = *off
165900070418  "   *
166000070418  "   *  -----------------------------------------------
166100070418  "   *  valorizzazione dei campi del sfl avviene automaticamente
166200070418  "   *  in quanto il nome campi di ndsdc00w = nome campi del sfl
166300070418  "   *
166400070418  "   *  -----------------------------------------------
166500070418  "   *  gestione sflnxtchg
166600070418  "   *
166700070418  "  C                   select
166800070418  "   *
166900070418  "   *    caricamento da movimenti banca
167000070418  "  C                   when      $Ries1Da = 'B'
167100070418  "   *      per ogni rcd caricato devo attivare la sflnxtchg
167200070418  "   *      perchè ho già impostato l'importo in conto
167300070418  "  C                   move      *on           *in32
167400070418  "   *
167500070418  "   *    caricamento da scadenze/ndppa
167600070418  "  C                   when      $Ries1Da = 'S'
167700070418  "   *      non c'è bisogno di attivare sflnxtchg per i rcd caricati
167800070418  "  C                   move      *off          *in32
167900070418  "   *      se richiesta la proposta automatica e rcd selezionato
168000070418  "   *      attivo SFLNXTCHG
168100070418  "  C                   if        PropAut045=xscon and S1selez='X'
168200070418  "  C                   move      *on           *in32
168300070418  "  C                   endif
168400070418  "   *
168500070418  "  C                   endsl
168600070418  "   *
168700070418  "   *  -----------------------------------------------
168800070418  "   *  scrittura sfl
168900070418  "  C                   add       1             s1nrr
169000070418  "  C                   exsr      AtrS1
169100070418  "  C                   write     FmtS1
169200070419  "  C                   move      *off          *in32
169300070418  "  C                   clear                   Fmts1
169400070418  "   *
169500070418  "   *
169600070419  "  C                   read      ndsdc01w                               26
169700070418  "  C                   enddo
169800070418  "   *
169900070418  "   * -----------------------------------------------
170000070418  "   * ultime impostazioni sfl
170100070418  "   *   sflnxtchg
170200070418  "  C                   move      *off          *in32
170300070418  "   *   impedisco il rollup con righe vuote
170400070419  "  C                   move      *in26         *in33
170500070418  "  C                   z-add     s1nrr         wmax
170600070418  "   *   30=sfldsp
170700070418  "  C     s1nrr         comp      *zeros                             30
170800070418  "   *
170900070418  "   * -----------------------------------------------
171000070418  "   * chiusura work-file
171100070418  "  C                   if        %open(ndsdc01w)
171200070418  "  C                   close     ndsdc01w
171300070418  "  C                   endif
171400070418  "   *
171500070419  "   * -----------------------------------------------
171600070419  "   * ripristino ordinamento normale per partita
171700070419  "  C                   eval      $ord = 1
171800070418  "   *
171900070418C2012C     XRieS1        Endsr
172000951027     C************************************************************
172100980115     C* CARICAMENTO sfl per partite
172200951027     C************************************************************
172300951027     C     CarParS1      Begsr
172400951027     C*
172500951027     C                   Eval      $EndFile = *off
172600951027     C*
172700951027     C* Leggo dal file anagrafico per caricare la lista
172800951027     C*
172900951027     C     $EndFile      DOueq     *on
173000951027     C*
173100951027     C*
173200951027     C                   CLEAR                   FMTS1
173300951027     C* non c'è bisogno di attivare la sflnxtchg per i rcd caricati
173400951027     C                   SETOFF                                       32
173500951027     C*
173600951027     C* riempio la riga da database
173700980115     C*  dati specifici NON scadenze
173800951030     C                   Exsr      RieS1DaPPA
173900980115     C*  dati comuni partite
174000980115     C                   Exsr      RieS1DaPar
174100960524     C*
174200960524     C* scrivo il rcd solo se è considerato valido
174300960524     C                   IF        $RcdOk = *on
174400960524     C*
174500960524     C                   ADD       1             S1NRR
174600980116     C* se è richiesta la proposta automatica
174700980116     C                   IF        PropAut045 = XscOn and
174800980116     C* e il rcd è stato selezionato
174900980116     C                             S1Selez = 'X'
175000980116     C* attivo SFLNXTCHG
175100980116     C                   SETON                                        32
175200980116     C                   ENDIF
175300960524     C* imposto gli attributi del sfl
175400960524     C                   EXSR      AtrS1
175500960524     C*
175600960524     C                   WRITE     FMTS1
175700960524     C*
175800980116     C                   SETOFF                                       32
175900960524     C                   ENDIF
176000951027     C*
176100951027     C* leggo un nuovo rcd da database
176200951027     C                   Exsr      REDANA
176300951027     C*
176400951027     C                   Enddo
176500951027     C*
176600951027     C                   SETOFF                                       32
176700951027     C*
176800951027     C* impedisco il rollup con righe vuote
176900951027     C                   MOVE      $EndFile      *in33
177000951027      *
177100951027     C                   Z-ADD     S1NRR         WMAX
177200951027      * 30=sfldsp
177300951027     C     s1nrr         comp      *zeros                             30
177400951027      *
177500951027     C                   Endsr
177600950707     C************************************************************
177700950707     C* CARICAMENTO PAGINA vuota
177800950707     C************************************************************
177900950801     C     RollS1        Begsr
178000950707      *
178100950707     C                   SETOFF                                       32
178200950707      *
178300950707     C                   Z-ADD     WMAX          S1NRR
178400950801     C* calcolo quante righe mancano al riempimento della pagina
178500950807     C     S1Nrr         DIV       SflPag        Resto
178600950807     C                   MVR                     Resto
178700950807     C     SflPag        SUB       Resto         Resto
178800950801     C* se non ne manca nessuna significa che devo riempire una nuova pagina
178900950801     C                   IF        Resto = 0
179000950801     C                   Z-ADD     SflPag        Resto
179100950801     C                   ENDIF
179200950731     C                   CLEAR                   FMTS1
179300950707     C*
179400950707     C* Carico una pagina vuota
179500950825     C     Resto         DOWGT     *zeros
179600951026     C     S1Nrr         ANDLT     9999
179700950801     C* imposto alcuni default
179800951106     C                   MOVE      C1Divisa      S1Divisa
179900950801     C* imposto gli attributi del sfl
180000950801     C                   EXSR      AtrS1
180100950825     C*
180200950801     C                   ADD       1             S1NRR
180300950707     C                   WRITE     FMTS1
180400950801     C                   SUB       1             Resto
180500950721     C                   Enddo
180600950825     C*
180700950707     C                   Z-ADD     S1NRR         WMAX
180800950825     C* 30=SflDsp
180900950825     C     S1Nrr         COMP      *zeros                             30
181000950825     C* 33=SflEnd
181100001027D0748X**** S1Nrr         COMP      100                                    33
181200001027D0748C     S1Nrr         COMP      $DimSK                                 33
181300950802     C*
181400950721     C                   Endsr
181500940224     C************************************************************
181600990122     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL                       w
181700940224     C************************************************************
181800950802     C     AtrS1         Begsr
181900950802     C*
182000960315     C* azzero gli indicatori degli attributi dei campi del sfl
182100960315     C                   MOVE      *zero         In0109            9
182200960315     C                   MOVEA     In0109        *in(01)
182300960315     C*
182400960315     C* se il residuo è l'intero importo originale
182500960315     C                   IF        S1ResiduoD = S1ImpOriD  and
182600960315     C* e la riga è recuperata da DataBase
182700960315     C                             H1NrAsInt <> 0
182800960315     C* sottolineo il residuo
182900960315     C                   SETON                                        01
183000960315     C                   ENDIF
183100960315     C*
183200981106     C* azzero gli indicatori che devono proteggere i campi I/O del sfl
183300960315     C                   MOVE      *zero         In1019           10
183400960315     C                   MOVEA     In1019        *in(10)
183500950802     C*
183600980223     C* se la partita/scadenza è aperta in m.c. ma chiusa in divisa
183700980224     C                   IF        S1ResiduoD = 0  and
183800980224     C* e la riga è recuperata da DataBase
183900980224     C                             H1NrAsInt <> 0
184000970327     C* proteggo TUTTO
184100990122     C                   SETON                                        101112
184200971224     C                   SETON                                        131415
184300981106     C                   SETON                                        16
184400980205     C                   ENDIF
184500980223     C*
184600980223     C* se la riga ha un blocco amministrativo
184700980223     C                   IF        S1BkAmm <> *blank
184800980223     C* rendo lampeggiante la data partita per indicare il blocco su partita
184900980223     C                   SETON                                        08
185000980223     C* se il conto non è cliente
185100090504>>>>>C*                  IF        SNatura002 <> 'C'
185200980223     C* proteggo TUTTO
185300980223     C                   SETON                                        101112
185400980223     C                   SETON                                        131415
185500980224     C                   SETON                                        16
185600980223     C                   ENDIF
185700090504>>>>>C*                  ENDIF
185800971128     C*
185900980205     C* se il conto ha un blocco amministrativo da Regole contabili (RCO)
186000971128     C                   IF        C1BkAmm <> *blank and
186100980205     C* e il conto non è ne cliente ne fornitore
186200980205     C                             (SNatura002 <> 'C' and
186300980205     C                              SNatura002 <> 'F' or
186400980205     C* oppure il conto è fornitore
186500980205     C                              SNatura002 = 'F' and
186600980205     C* e il segno è Avere
186700980205     C                              S1Segno = XscAvere)
186800971128     C* proteggo TUTTO
186900971128     C                   SETON                                        101112
187000980122     C                   SETON                                        131415
187100980224     C                   SETON                                        16
187200971128     C                   ENDIF
187300970327     C*
187400970327     C* se la riga è recuperata da DataBase
187500970327     C                   IF        H1NrAsInt > 0
187600970327     C* proteggo gli estremi partita
187700970327     C* proteggo la divisa
187800970327     C* proteggo la data scadenza
187900970327     C* proteggo il segno
188000970327     C                   SETON                                        10
188100970327     C                   ENDIF
188200951027     C*
188300951027     C* se la riga non è recuperata da DataBase
188400951030     C                   IF        H1NrAsInt = 0
188500951027     C* proteggo lo sconto
188600951027     C                   SETON                                        11
188700951027     C                   ENDIF
188800950927     C*
188900951107     C* se la divisa è = da quella di testata
189000951108     C                   IF        S1Divisa = C1Divisa
189100951108     C* proteggo e nascondo :
189200951108     C* il cambio e l'importo concordato, l'importo in divisa di partita
189300951108     C* calcolato, la sopravvenienza
189400951027     C                   SETON                                        12
189500951108     C                   ENDIF
189600971224     C*
189700971224     C* se la divisa di testata <> da m.c.
189800971224     C                   IF        C1Divisa <> XscDiv  and
189900971224     C*  e la divisa di partita =  a  m.c.
190000971224     C                             S1Divisa =  XscDiv
190100971224     C* proteggo:
190200971224     C* la forzatura importo in divisa di partita
190300971224     C                   SETON                                        15
190400971224     C                   ENDIF
190500951114     C*
190600951114     C* se la riga è recuperata da DataBase
190700951114     C                   IF        H1NrAsInt > 0          and
190800951114     C* e gestione partite a scadenze
190900990216C0958C* confermate nella chiamata a X20ScadR
191000990216C0958C                             X20ParScd = 'S'
191100951114     C* predispongo la riga per visualizzare sia la causale che il cambio
191200951114     C                   MOVE      *blank        S1Txt
191300951114     C                   MOVEL     'Causale'     S1Txt
191400951114     C                   SETOFF                                       40
191500951114     C* altrimenti
191600951114     C                   ELSE
191700951114     C* predispongo la riga per visualizzare solo il cambio
191800951114     C                   MOVE      *blank        S1Txt
191900951114     C                   MOVEL     'Cambio'      S1Txt
192000951114     C                   SETON                                        40
192100951114     C                   ENDIF
192200951123     C*
192300951123     C* se la divisa di partita è <> da quella contabile
192400951123     C                   IF        S1Divisa <> C1Divisa   and
192500951123     C* e gestione cambio storico
192600951123     C                             CmbSto045 = *on
192700980122     C* proteggo TUTTO
192800980122     C                   SETON                                        101112
192900980122     C                   SETON                                        131415
193000980224     C                   SETON                                        16
193100951123     C                   ENDIF
193200981106     C*
193300981106     C* se la divise di partita e di contabile hanno tra loro SOLO cambi
193400981106     C* fissi
193500981203     C                   IF        H1Triang = '3' or
193600981203     C                             H1Triang = '4'
193700981204     C* proteggo flag forzatura importo calcolato
193800981204     C* (che deve essere a SI)
193900981204     C                   SETON                                        17
194000981203     C                   ENDIF
194100990803C1209 *
194200990803  "  C* test esistenza commenti sulla partita
194300990803  "  C                   EXSR      CTR724Par
194400990803  "   * Se esiste commento sulla partita la sottolineo
194500990803  "  C                   IF        H1Comm = *On
194600990803  "  C                   SETON                                        18
194700990803  "  C                   ELSE
194800990803  "  C                   SETOFF                                       18
194900990803  "  C                   ENDIF
195000990803C1209 *
195100951027     C*
195200950802     C                   Endsr
195300990803C1209C************************************************************
195400990803  "  C* Controllo esistenza commenti sulla partita
195500990803  "  C************************************************************
195600990803  "  C     CTR724Par     BEGSR
195700990803  "  C*
195800990803  "  C                   RESET                   ANA724DS
195900990803  "  C                   MOVE      *BLANK        EXI724
196000990803  "  C                   MOVEL     '01'          OPZ724
196100990803  "  C                   MOVEL     XSCSOC        SOC724
196200990803  "  C                   MOVEL     *BLANK        UNI724
196300990803  "  C                   MOVEL     'PPA'         TPA724
196400990803  "  C* inserire chiavi commenti
196500990803  "  C                   MOVEL     REGCtb        KY1724
196600990803  "  C                   MOVEL     C1Kcc         KY2724
196700990803  "  C                   MOVE      C1Ksc         KY2724
196800990803  "  C                   MOVEL     S1DtPar       KY3724
196900990803  "  C                   MOVEL     S1NrPar       KY4724
197000990803  "  C                   MOVE      S1SerPar      KY4724
197100990803  "  C                   MOVEL     Oggi          DTS724
197200990803  "  C                   MOVEL     ANA724DS      KPJBU
197300990803  "  C                   CALL      'ANA724R'
197400990803  "  C                   PARM                    KPJBA
197500990803  "  C                   MOVEL     KPJBU         ANA724DS
197600990803  "  C*
197700990803  "  C* memorizzo la presenza o meno di commenti
197800990803  "  C                   MOVE      EXI724        H1Comm
197900990803  "  C*
198000990803C1209C     XCtr724Par    ENDSR
198100950802     C************************************************************
198200950802     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
198300950802     C************************************************************
198400950802     C     SETANA        Begsr
198500950809     C*
198600951019     C                   MOVE      RegSocieta    PPASocieta
198700951019     C                   MOVE      RegCtb        PPACtb
198800951017     C                   MOVE      C1Kcc         PPAKcc
198900951017     C                   MOVE      C1Ksc         PPAKsc
199000950809     C*
199100951017    >C     K04PPA03      SETLL     NDPPA000                               21
199200940224     C*
199300951031     C* predispongo la DS per la chiamata agli scadenziari
199400951031     C   21              EXSR      RieX20DS
199500951031     C*
199600950721     C                   Endsr
199700940128     C************************************************************
199800940131     C* LETTURA RCD ARCHIVIO PILOTA
199900940128     C************************************************************
200000950721     C     REDANA        Begsr
200100951017     C*
200200951017     C                   MOVEL     *OFF          $EndFile
200300951017     C*
200400951017     C     $EndFile      DOUEQ     *ON
200500951017     C     $RCDOK        OREQ      *ON
200600940131     C*
200700040308     C     K04PPA03      Reade     NDPPA000                               21
200800940131     C*
200900950721     C                   Move      *IN21         $EndFile
201000951017     C*
201100951017     C     $EndFile      IFEQ      *OFF
201200951017     C                   EXSR      SELANA
201300951017     C                   ENDIF
201400951017     C*
201500951017     C                   ENDDO
201600940131     C*
201700950721     C                   Endsr
201800950707     C************************************************************
201900951017     C* Convalida o meno l'emissione del rcd letto
202000950707     C************************************************************
202100951017     C     SelAna        Begsr
202200950907     C*
202300951017     C* parto presupponendo il rcd valido
202400951017     C                   MOVEL     *on           $RcdOk
202500970327     C* senza blocco amministrativo
202600970327     C                   MOVEL     *blank        BkAmm
202700010105C1458C                   MOVE      *blank        BkAmmPSC
202800951017     C*
202900951017     C* al primo test negativo, memorizzo che il rcd non è valido e vado a
203000951017     C* fine routine
203100951017     C                   DO
203200951017     C*
203300980114     C* unità <> da selezione
203400980114     C                   IF        UnitaS079 <> *blank           and
203500980114     C                             PPAUnita <> UnitaS079
203600951017     C                   MOVEL     *off          $RcdOk
203700951017     C                   LEAVE
203800951017     C                   ENDIF
203900980114     C*
204000980114     C* divisa <> da selezione
204100980114     C                   IF        Divisa079 <> *blank           and
204200980114     C                             PPADivisa <> Divisa079
204300980114     C                   MOVEL     *off          $RcdOk
204400980114     C                   LEAVE
204500980114     C                   ENDIF
204600980114     C*
204700980114     C* data partita non compresa nel range di selezione
204800980115     C                   IF        DtParA079 <> Dt0              and
204900980114     C                             (PPADtPar < DtParDa079 or
205000980114     C                              PPADtPar > DtParA079)
205100980114     C                   MOVEL     *off          $RcdOk
205200980114     C                   LEAVE
205300980114     C                   ENDIF
205400980114     C*
205500980122     C* se almeno un nr.partita è stato immesso per la selezione
205600980122     C                   IF        NrPar1079 > 0 or
205700980122     C                             NrPar2079 > 0 or
205800980122     C                             NrPar3079 > 0 or
205900980122     C                             NrPar4079 > 0 or
206000980122     C                             NrPar5079 > 0 or
206100980122     C                             NrPar6079 > 0 or
206200980122     C                             NrPar7079 > 0 or
206300980122     C                             NrPar8079 > 0
206400980122     C* se nessun nr.partita selezionato è = a quello reperito
206500980122     C                   IF        PPANrPar <> NrPar1079   and
206600980122     C                             PPANrPar <> NrPar2079   and
206700980122     C                             PPANrPar <> NrPar3079   and
206800980122     C                             PPANrPar <> NrPar4079   and
206900980122     C                             PPANrPar <> NrPar5079   and
207000980122     C                             PPANrPar <> NrPar6079   and
207100980122     C                             PPANrPar <> NrPar7079   and
207200980122     C                             PPANrPar <> NrPar8079
207300980114     C                   MOVEL     *off          $RcdOk
207400980114     C                   LEAVE
207500980114     C                   ENDIF
207600980122     C                   ENDIF
207700980114     C*
207800980122     C* se almeno una serie partita è stata immessa per la selezione
207900980122     C                   IF        SerPar1079 <> *blank or
208000980122     C                             SerPar2079 <> *blank or
208100980122     C                             SerPar3079 <> *blank
208200980122     C* se nessuna serie partita selezionata è = a quella reperita
208300980122     C                   IF        PPASerPar <> SerPar1079    and
208400980122     C                             PPASerPar <> SerPar2079    and
208500980114     C                             PPASerPar <> SerPar3079
208600980114     C                   MOVEL     *off          $RcdOk
208700980114     C                   LEAVE
208800980114     C                   ENDIF
208900980122     C                   ENDIF
209000010104     C*
209100010105C1458C* reperisco i dati di PPS
209200010105  "  C* (se il rcd è stato reperito...)
209300010105  "  C                   IF        C0601RpPPS(PPASys:
209400010105  "  C                                        PPANrAsInt:
209500010105  "  C                                        NDPPS00F:
209600010105  "  C                                        %size(NDPPS00F)) = 0
209700040308C1458C* blocco amministrativo
209800040308C1458x***                MOVEL     PPASys        PPSSys
209900010105  "  x***                MOVEL     PPANrAsInt    PPSNrAsInt
210000010105  "  x***  K02PPS01      CHAIN     NDPPS000                           21
210100010105  "  x* il blocco amministrativo lo memorizzo e invece di scartare la
210200010105  "  x* partita ne inibisco totalmente l'uso
210300010105  "  x***                IF        *in21 = *off
210400010105  "  x***                MOVEL     PPSBkAmm      BkAmm
210500010105  "  x***                ELSE
210600010105  "  x***                MOVEL     *blank        BkAmm
210700040308C1458x***                ENDIF
210800040308C1458C* il blocco amministrativo lo memorizzo e invece di scartare la
210900010105  "  C* partita ne inibisco totalmente l'uso
211000010105C1458C                   MOVEL     PPSBkAmm      BkAmm
211100980114     C*
211200980114     C* categorie anagrafiche non comprese nelle selezioni
211300980114     C                   IF        CtgAna1079 <> *blank and
211400980114     C                             PPSCtgAna1 <> CtgAna1079    or
211500980114     C                             CtgAna2079 <> *blank and
211600980114     C                             PPSCtgAna2 <> CtgAna2079    or
211700980114     C                             CtgAna3079 <> *blank and
211800980114     C                             PPSCtgAna3 <> CtgAna3079
211900980114     C                   MOVEL     *off          $RcdOk
212000980114     C                   LEAVE
212100980114     C                   ENDIF
212200980114     C*
212300980114     C* agente non compreso nelle selezioni
212400980114     C                   IF        KscAge079 <> *blank and
212500980114     C                             PPSKscAge <> KscAge079
212600980114     C                   MOVEL     *off          $RcdOk
212700980114     C                   LEAVE
212800980114     C                   ENDIF
212900980217     C*
213000980217     C* blocco amministrativo non compreso nelle selezioni
213100980217     C                   SELECT
213200980217     C* solo quelle bloccate
213300980220     C                   WHEN      BkAmm079 = '* '
213400980220     C                   IF        PPSBkAmm = *blank
213500980217     C                   MOVEL     *off          $RcdOk
213600980217     C                   LEAVE
213700980220     C                   ENDIF
213800980217     C* solo quelle NON bloccate
213900980220     C                   WHEN      BkAmm079 = '**'
214000980220     C                   IF        PPSBkAmm <> *blank
214100980217     C                   MOVEL     *off          $RcdOk
214200980217     C                   LEAVE
214300980220     C                   ENDIF
214400980217     C* solo quelle aventi il blocco digitato
214500980217     C                   WHEN      BkAmm079 <> *blank and
214600980217     C                             PPSBkAmm <> BkAmm079
214700980217     C                   MOVEL     *off          $RcdOk
214800980217     C                   LEAVE
214900980217     C                   ENDSL
215000980217     C*
215100980217     C* Factor non compreso nelle selezioni
215200980217     C                   IF        KccFact079 <> *blank and
215300980217     C                             KscFact079 <> *blank and
215400980217     C                             (PPSKccFact <> KccFact079 or
215500980217     C                              PPSKscFact <> KscFact079)
215600980217     C                   MOVEL     *off          $RcdOk
215700980217     C                   LEAVE
215800980217     C                   ENDIF
215900951017     C*
216000010105C1458C* il rcd di PPS si dovrebbe sempre trovare
216100010105  "  C                   ENDIF
216200010105C1458C*
216300951017     C* reperisco i saldi mediante la routine degli scadenziari
216400951017     C* e intanto controllo se è ancora aperta
216500951031     C                   MOVE      PPADtPar      X20DtPar
216600951031     C                   MOVE      PPANrPar      X20NrPar
216700951031     C                   MOVE      PPASerPar     X20SerPar
216800951031     C                   RESET                   X20ScadXDS
216900951017     C                   CALL      'X20SCADR'
217000951017     C                   PARM                    X20ScadDS
217100951026     C                   PARM                    X20ScadXDS
217200960202     C                   EVAL      TotDare  = X20TDDD + X20TDDP + X20TDDG
217300960202     C                   EVAL      TotAvere = X20TADD + X20TADP + X20TADG
217400951017     C* se non c'è differenza tra dare e avere vuol dire che la partita
217500951026     C* o la scadenza è chiusa
217600951017     C                   IF        TotDare = TotAvere
217700951017     C                   MOVEL     *off          $RcdOk
217800951017     C                   LEAVE
217900951017     C                   ENDIF
218000951017     C*
218100951017     C                   ENDDO
218200951017     C*
218300951017     C                   ENDSR
218400951017     C************************************************************
218500951017     C* Riempio campi della DS della routine scadenziari
218600951017     C************************************************************
218700951017     C     RieX20DS      Begsr
218800951017     C*
218900951024     C                   RESET                   X20ScadDS
219000951019     C                   MOVE      RegSocieta    X20Societa
219100951128     C                   MOVE      RegCtb        X20Ctb
219200981016     C                   IF        MovDef0IP = *off
219300981016     C                   MOVE      '0'           X20Def
219400981016     C                   ELSE
219500981016     C                   MOVE      '1'           X20Def
219600981016     C                   ENDIF
219700981016     C                   IF        MovProv0IP = *off
219800981016     C                   MOVE      '0'           X20Prov
219900981016     C                   ELSE
220000981016     C                   MOVE      '1'           X20Prov
220100981016     C                   ENDIF
220200981016     C                   IF        MovGest0IP = *off
220300981016     C                   MOVE      '0'           X20Gest
220400981016     C                   ELSE
220500981016     C                   MOVE      '1'           X20Gest
220600981016     C                   ENDIF
220700951017     C                   MOVE      C1Kcc         X20Kcc
220800951017     C                   MOVE      C1Ksc         X20Ksc
220900980821     C                   MOVE      SNatura002    X20SNatura
221000951017     C                   MOVE      'R'           X20ElaSca
221100951017     C                   MOVE      'N'           X20PPA
221200951017     C                   MOVE      'R'           X20LRT
221300960201     C                   CALLB     'XDT4000'
221400951025     C     X20DtRegMD    PARM                    XDTAMG           10
221500000117C1309C                   PARM      ZDate         XDTGMA            6 0
221600951025     C                   PARM      4             XDTSTA            1 0
221700951031     C                   EVAL      X20DtRegMG = XDtAMG
221800951031     C                   EVAL      X20DtSca   = XDtAMG
221900960515     C*
222000990526C0958C* se gestione partite a scadenze prevista in tabella
222100951201     C                   IF        GPS0IP = '1'                or
222200951201     C* oppure è stato richiamato dai movimenti banca
222300951201     C                             Opz045 = '01'
222400990526C0958C* dovrei richiamare l'X20ScadR con gestione scadenze
222500960515     C*
222600960515     C* ma se il conto è un percipiente
222700990215C0958C                   IF        SNatura002 = 'F' and Percipi002 = *on or
222800040308  "  C* oppure il conto stesso non è gestito a scadenze
222900990215C0958C                             Scadenz002 = '0'
223000960515     C* forzo la gestione a partite
223100960515     C                   MOVE      'P'           X20ParSca
223200960515     C                   MOVE      ' '           X20ParScd
223300951107     C                   ELSE
223400960515     C                   MOVE      'S'           X20ParSca
223500960515     C                   MOVE      'S'           X20ParScd
223600951107     C                   ENDIF
223700960515     C*
223800960515     C                   ELSE
223900960515     C                   MOVE      'P'           X20ParSca
224000960515     C                   MOVE      ' '           X20ParScd
224100960515     C                   ENDIF
224200951017     C*
224300951017     C                   Endsr
224400951128     C************************************************************
224500951128     C* Riempio campi del subfile con valori dalla schiera delle
224600951128     C*  partite dei movimenti banca
224700951128     C************************************************************
224800951128     C     RieS1DaBan    Begsr
224900951128     C*
225000951128     C                   MOVEL     *on           $RcdOk
225100010502C1458C* pulisco le variabili di riga
225200010502C1458C                   EVAL      H1RataBk = *off
225300951128     C*
225400951128     C* chiamo gli scadenziari per avere gli altri dati che non ho sulla DS
225500951128     C* e intanto controllo se è ancora aperta
225600951128     C                   EXSR      RieX20DS
225700951128     C                   MOVE      Sk2DtPar      X20DtPar
225800960529     C                   Z-ADD     Sk2NrPar      X20NrPar
225900951128     C                   MOVE      Sk2SerPar     X20SerPar
226000951128     C                   RESET                   X20ScadXDS
226100951128     C                   CALL      'X20SCADR'
226200951128     C                   PARM                    X20ScadDS
226300951128     C                   PARM                    X20ScadXDS
226400960202     C                   EVAL      TotDare  = X20TDDD + X20TDDP + X20TDDG
226500960202     C                   EVAL      TotAvere = X20TADD + X20TADP + X20TADG
226600951128     C* se non c'è differenza tra dare e avere vuol dire che la partita
226700951128     C* o la scadenza è chiusa
226800040308     C                   IF        TotDare = TotAvere
226900951128     C                   MOVEL     *off          $RcdOk
227000951128     C*
227100040308     C                   ELSE
227200951201     C*
227300951201     C* ciclo per le scadenze recuperate finche non trovo quella voluta
227400951201     C* oppure non finiscono le scadenze
227500951201     C*
227600951201     C                   Z-ADD     1             Y
227700951201     C                   EVAL      ScaTrovata = *off
227800951201     C*
227900040308     C     y             DOUGT     X20Index
228000040308     C     DtScaX(y)     IFEQ      Sk2DtSca
228100070417C2012C     DtScax(y)     andge     DtScaDa079
228200070417C2012C     DtScax(y)     andle     DtScaA079
228300040308     C                   EVAL      ScaTrovata = *on
228400040308     C                   LEAVE
228500040308     C                   ENDIF
228600040308     C                   ADD       1             Y
228700040308     C                   ENDDO
228800040308     C*
228900040308     C* se ho trovato la scadenza richiesta uso la DS X20ScadX
229000040308     C                   IF        ScaTrovata = *on
229100040308     C*
229200040308     C     DtScaX(Y)     IFGT      Dt0
229300960201     C                   CALLB     'XDT4000'
229400951201     C                   PARM      DtScaX(Y)     XDTAMG           10
229500951201     C     S1DtSca       PARM                    XDTGMA            6 0
229600951201     C                   PARM      2             XDTSTA            1 0
229700070417C2012C                   move      DtScaX(Y)     H1DtSca
229800040308     C                   ELSE
229900040308     C                   MOVE      *zero         S1DtSca
230000070419C2012C                   move      *loval        WDataIso
230100070419C2012C                   move      WDataIso      H1DtSca
230200040308     C                   ENDIF
230300040308     C*
230400040308     C                   EVAL      S1Residuo = ImpRMC(y)
230500040308     C                   EVAL      S1ResiduoD = ImpRD(y)
230600040308     C                   EVAL      S1Causale = Caus2(y)
230700040308     C* se il conto da saldacontare è un cliente
230800040308     C                   IF        SNatura002 = 'C'
230900040308     C* se l'IVA dare è <> 0
231000040308     C                   IF        X20IVADare <> 0
231100040308     C* l'importo originale è l'IVA dare
231200040308     C                   EVAL      S1ImpOriD = X20IVADare
231300040308     C* altrimenti
231400040308     C                   ELSE
231500960315     C* l'importo originale è l'IVA avere
231600960315     C                   EVAL      S1ImpOriD = X20IVAAver
231700040308     C                   ENDIF
231800040308     C* se il conto da saldacontare è un fornitore
231900040308     C                   ELSE
232000040308     C* se l'IVA avere è <> 0
232100040308     C                   IF        X20IVAAver <> 0
232200040308     C* l'importo originale è l'IVA avere
232300040308     C                   EVAL      S1ImpOriD = X20IVAAver
232400040308     C* altrimenti
232500040308     C                   ELSE
232600040308     C* l'importo originale è l'IVA dare
232700040308     C                   EVAL      S1ImpOriD = X20IVADare
232800040308     C                   ENDIF
232900040308     C                   ENDIF
233000040308     C*
233100040308     C* altrimenti
233200040308     C                   ELSE
233300951201     C* uso i dati di partita
233400951201     C                   MOVE      *zero         S1DtSca
233500070419C2012C                   move      *loval        WDataIso
233600070419C2012C                   move      WDataIso      H1DtSca
233700960202     C                   EVAL      S1ResiduoD = TotDare - TotAvere
233800960202     C                   EVAL      TotDare  = X20TDLD + X20TDLP + X20TDLG
233900960202     C                   EVAL      TotAvere = X20TALD + X20TALP + X20TALG
234000960202     C                   EVAL      S1Residuo = TotDare - TotAvere
234100040308     C                   ENDIF
234200951201     C*
234300951201     C* devo impostare il nr.assoluto a qualcosa perchè, altrimenti,
234400960412     C* considererebbe la riga come immessa
234500040308     C                   Z-ADD     *hival        H1NrAsInt
234600951201     C*
234700960201     C                   CALLB     'XDT4000'
234800951201     C                   PARM      Sk2DtPar      XDTAMG           10
234900951201     C     S1DtPar       PARM                    XDTGMA            6 0
235000951201     C                   PARM      2             XDTSTA            1 0
235100070417C2012C                   move      Sk2DtPar      H1DtPar
235200040308     C                   Z-ADD     Sk2NrPar      S1NrPar
235300040308     C                   MOVE      Sk2SerPar     S1SerPar
235400951201     C*
235500040308     C                   MOVE      X20Divisa     S1Divisa
235600951201     C*
235700951201     C* l'importo in conto è quello della partita provvisoria
235800951201     C                   EVAL      S1ImportD = Sk2ImportD
235900960322     C* se la divisa è = da quella di testata
236000040308     C                   IF        S1Divisa = C1Divisa           or
236100960322     C* oppure se la divisa di partita è <> da quella contabile
236200960322     C                             S1Divisa <> C1Divisa   and
236300960322     C* e gestione cambio storico
236400960322     C                             CmbSto045 = *on
236500960322     C* l'importo concordato deve restare a 0
236600960322     C                   EVAL      S1ImpConc = 0
236700960529     C* posso impostare eventuali abbuoni e/o sconti
236800960529     C                   EVAL      S1ScontoD = Sk2Sconto
236900040308     C                   EVAL      S1AbbuonoD = Sk2ImpAbbA - Sk2ImpAbbP
237000040308     C                   IF        S1AbbuonoD > 0
237100040308     C                   EVAL      S1Segabb = 'A'
237200040308     C                   ELSE
237300040308     C                   EVAL      S1AbbuonoD = S1AbbuonoD * -1
237400040308     C                   EVAL      S1Segabb = 'P'
237500040308     C                   ENDIF
237600040308     C* altrimenti
237700040308     C                   ELSE
237800960322     C* lo valorizzo con l'importo in divisa concordato di partita
237900960322     C                   EVAL      S1ImpConc = Sk2ImpDivC
238000990528C0762C* imposto eventuali sconti
238100990528C0762C                   EVAL      S1ScontoD = Sk2Sconto
238200990528C0941C                   EVAL      S1AbbuonoD = Sk2ImpAbbA - Sk2ImpAbbP
238300990519  "  C                   IF        S1AbbuonoD > 0
238400990519  "  C                   EVAL      S1Segabb = 'A'
238500990519  "  C                   ELSE
238600990519  "  C                   EVAL      S1AbbuonoD = S1AbbuonoD * -1
238700990519  "  C                   EVAL      S1Segabb = 'P'
238800990528C0941C                   ENDIF
238900040308     C                   ENDIF
239000951201     C                   EVAL      S1Selez = 'X'
239100951128     C*
239200951128     C* se il valore di residuo è negativo
239300040308     C                   IF        S1Residuo < 0
239400040308     C* dichiaro che il suo segno è Avere
239500040308     C                   EVAL      S1Segno = XscAvere
239600040308     C* altrimenti
239700040308     C                   ELSE
239800040308     C* dichiaro che il suo segno è Dare
239900040308     C                   EVAL      S1Segno = XscDare
240000040308     C                   ENDIF
240100040308     C* se il valore di residuo è negativo
240200040308     C                   IF        S1Residuo < 0
240300040308     C* lo trasformo in valore assoluto
240400040308     C                   MULT      -1            S1Residuo
240500040308     C                   MULT      -1            S1ResiduoD
240600040308     C                   ENDIF
240700040308     C*
240800040308     C* se la partita è aperta anche per il valore in divisa
240900040308     C                   IF        S1ResiduoD <> 0
241000951128     C* calcolo il cambio del residuo
241100951128     C* il cambio lo ottengo con il rapporto fra i residui di partita
241200980923     C                   RESET                   PJX00202DS
241300980923     C                   MOVEL     S1Divisa      Divisa0202
241400980923     C                   MOVEL     XscDiv        DivMC0202
241500981005     C                   Z-ADD     S1ResiduoD    ImporD0202
241600981005     C                   Z-ADD     S1Residuo     ImporM0202
241700980923     C                   Z-ADD     0             Cambio0202
241800980923     C                   MOVEL     DtRif045      DtRif0202
241900980925     C* non richiedo il controllo scostamento cambio/importo perchè il
242000980925     C* cambio recuperato può essere un valore sballato
242100980925     C                   MOVEL     *on           NoCtrlScos
242200980923     C*
242300980923     C                   CALLP     X0202TestC(PJX00202DS :
242400980923     C                                        %size(PJX00202DS) :
242500980923     C                                        *off   : Esito   :
242600980925     C                                        NoCtrlScos : *omit    :
242700980924     C                                        %size(ANDVS00F)   :
242800980924     C                                        %size(ANDVX00F)   :
242900980924     C                                        ANDVS00F : ANDVX00F )
243000980925     C* se errori (dovrebbe dare come unico errore il fatto che il cambio
243100980925     C*  calcolato è >= 10000000 oppure < 0,000001)
243200040308     C                   IF        Esito <> '0'
243300040308     C* richiamo finestra con messaggio
243400040308     C                   EXSR      MsgEVALBrk
243500040308     C* se NO errori
243600040308     C                   ELSE
243700040308     C* il cambio calcolato è quello della partita/scadenza
243800040308     C                   Z-ADD     Cambio0202    S1Cambio
243900040308     C                   ENDIF
244000040308     C*
244100040308     C* se la partita risulta chiusa in divisa (probabile mancanza delle
244200040308     C*  differenze cambio)
244300040308     C                   ELSE
244400040308     C* imposto il cambio di partita = 0
244500040308     C                   EVAL      S1Cambio = 0
244600040308     C* reperisco i dati divisa di partita
244700040308     C                   IF        X0201RpDVS(S1Divisa:DtRif045:
244800981106     C                                        ANDVS00F: %size(ANDVS00F):
244900981106     C                                        ANDVX00F: %size(ANDVX00F))
245000981106     C                             <> 0
245100981106     C* se errori (l'unico possibile è che la divisa fosse annullata
245200981106     C*  alla data)
245300981106     C* richiamo finestra con messaggio
245400981106     C                   EXSR      MsgEVALBrk
245500040308     C                   ENDIF
245600040308     C                   ENDIF
245700040308     C*
245800040308     C* se il conto è un percipiente
245900040308     C                   IF        SNatura002 = 'F' and Percipi002 = *on
246000040308     C* recupero la ritenuta calcolata
246100040308     C                   EVAL      PPANrPar  =   Sk2NrPar
246200040308     C                   EVAL      PPADtPar  =   Sk2DtPar
246300040308     C                   EVAL      PPASerPar =   Sk2SerPar
246400040308     C                   EXSR      RepRitAcc
246500040308     C* decurto il residuo (sia in divisa che in m.c. visto che se c'è una
246600040308     C* ritenuta stò lavorando con partita in m.c.) della ritenuta
246700040308     C*
246800040308     C* se l'importo della ritenuta è < 0
246900040308     C                   IF        H1RitCalc < 0
247000040308     C* bisogna domandarsi se è una nota di accredito (segno Dare)
247100040308     C                   IF        S1Segno = XscDare
247200971110     C* in questo caso decurto algebricamente (cioè sommo)
247300981022     C                   EVAL      S1ResiduoD = S1ResiduoD + H1RitCalcD
247400971110     C                   EVAL      S1Residuo = S1Residuo + H1RitCalc
247500971110     C* altrimenti è un caso anomalo e non prendo in considerazione la
247600971110     C* ritenuta d'acconto
247700040308     C                   ELSE
247800040308     C                   EVAL      H1RitCalc = 0
247900040308     C                   EVAL      H1RitCalcD = 0
248000040308     C                   ENDIF
248100040308     C*
248200040308     C* se l'importo della ritenuta è >= 0
248300040308     C                   ELSE
248400040308     C* lo decurto semplicemente
248500040308     C                   EVAL      S1ResiduoD = S1ResiduoD - H1RitCalcD
248600040308     C                   EVAL      S1Residuo = S1Residuo - H1RitCalc
248700040308     C                   ENDIF
248800040308     C*
248900040308     C                   ENDIF
249000951128     C*
249100040308     C                   ENDIF
249200040308     C*
249300040308     C                   EVAL      S1FrcAbb = XscOff
249400040308     C* se divisa di partita <> da divisa di testata
249500040308     C* bisogna impostare il flag di forzatura importo in divisa di partita
249600040308     C                   IF        S1Divisa <> C1Divisa
249700040308     C* se divisa di riga = m.c.
249800040308     C                   IF        S1Divisa = XscDiv
249900040308     C* la sopravenienza deve esserci (quindi la forzatura deve essere='N')
250000040308     C                   EVAL      S1FrcImpDP = XscOff
250100040308     C* negli altri casi
250200040308     C                   ELSE
250300040308     C* propongo la forzatura di tabella
250400040308     C                   IF        FrcImDP0IP = *on
250500040308     C                   EVAL      S1FrcImpDP = xscon
250600040308     C                   ELSE
250700980226     C                   EVAL      S1FrcImpDP = xscoff
250800040308     C                   ENDIF
250900040308     C                   ENDIF
251000040308     C                   ENDIF
251100040308     C                   EVAL      S1BkAmm = *blank
251200040308     C*
251300040308     C* se la riga è stata inclusa
251400040308     C                   IF        $RcdOK = *on  and
251500040308     C* e la divisa di partita è divera da quella di testata
251600040308     C                             C1Divisa <> S1Divisa
251700040308     C* controllo che legame c'è tra la divisa della partita e quella di
251800040308     C* testata :
251900040308     C* se tra di loro ci sono solo cambi fissi (una delle due è divisa
252000040308     C*  di triangolazione e l'altra ha un cambio fisso con questa, oppure
252100040308     C*  entrambe triangolano con la stessa divisa), l'importo concordato
252200040308     C* non deve essere digitabile dall'utente ma calcolato dal pgm
252300040308     C                   MOVEL     DtRif045      WDataISO
252400040308     C                   IF        X0201TpTri(H1Triang        :
252500990215     C                                        C1Divisa        :
252600990215     C                                        S1Divisa        :
252700990215     C                                        WDataISO        :
252800990215     C                                        *omit            )
252900990215     C* gli errori possibili sono solo relativi alle divise; in questo caso
253000990215     C* non vi possono essere in quanto vengono già passati i dati delle
253100990215     C* medesime e quindi sono già state testate
253200990215     C                             = 0
253300990215     C*
253400990215     C* se la divise di partita e di contabile hanno tra loro SOLO cambi
253500990215     C* fissi
253600040308     C                   IF        H1Triang = '3' or
253700040308     C                             H1Triang = '4'
253800040308     C* il flag di forzatura importo in divisa di partita DEVE essere SI
253900040308     C* non perchè è obbligatoria una sopravvenienza che invece NON ci
254000040308     C* sarà, ma perchè è l'importo concordato ad essere = a quello
254100040308     C* calcolato
254200040308     C                   EVAL      S1FrcImpDP = XscOn
254300040308     C                   ENDIF
254400040308     C*
254500040308     C                   ENDIF
254600040308     C                   ENDIF
254700951204     C*
254800951128     C                   ENDSR
254900951017     C************************************************************
255000951027     C* Riempio campi del subfile con valori delle scadenze
255100951017     C************************************************************
255200951027     C     RieS1DaSca    Begsr
255300960524     C*
255400960524     C                   MOVEL     *on           $RcdOk
255500951027     C*
255600951027     C     DtScaX(x)     IFGT      Dt0
255700960201     C                   CALLB     'XDT4000'
255800951027     C                   PARM      DtScaX(x)     XDTAMG           10
255900951027     C     S1DtSca       PARM                    XDTGMA            6 0
256000951027     C                   PARM      2             XDTSTA            1 0
256100070417C2012C                   move      DtScaX(x)     H1DtSca
256200951027     C                   ELSE
256300951027     C                   MOVE      *zero         S1DtSca
256400070419C2012C                   move      *loval        WDataIso
256500070419C2012C                   move      WDataIso      H1DtSca
256600951027     C                   ENDIF
256700951027     C                   EVAL      S1Residuo = ImpRMC(x)
256800951027     C                   EVAL      S1ResiduoD = ImpRD(x)
256900951027     C                   EVAL      S1Causale = Caus2(x)
257000010104C1458C*
257100010105  "  C                   EVAL      BkAmmPSC = *blank
257200010104  "  C* se non c'è un blocco amministrativo sull'intera partita
257300010104  "  C                   IF        BkAmm = *blank
257400010104  "  C* controllo sulla singola rata
257500010105  "  C                   IF        C0601RpPSC(PPASys:
257600010105  "  C                                        PPANrAsInt:
257700010226  "  C                                        DtScaX(x):
257800010226  "  C                                        *omit:
257900010226  "  C                                        '1':
258000010226  "  C                                        *omit:
258100010226  "  C                                        *omit:
258200010226  "  C                                        %size(NDPSC00F):
258300010226  "  C                                        NDPSC00F) = '0'
258400010105  "  C                   EVAL      BkAmmPSC = PSCBkAmm
258500010105  "  C                   ENDIF
258600010105C1458C                   ENDIF
258700951204     C*
258800950823     C                   ENDSR
258900980115     C************************************************************
259000980115     C* Riempio campi del subfile con valori di database
259100980115     C************************************************************
259200980115     C     RieS1DaPPA    Begsr
259300980115     C*
259400980115     C                   MOVEL     *on           $RcdOk
259500980115     C*
259600980115     C* calcolo il cambio medio come rapporto tra il saldo in m.c. e quello
259700980115     C* in divisa
259800980115     C                   EVAL      S1ResiduoD = TotDare - TotAvere
259900980115     C                   EVAL      TotDare  = X20TDLD + X20TDLP + X20TDLG
260000980115     C                   EVAL      TotAvere = X20TALD + X20TALP + X20TALG
260100980115     C                   EVAL      S1Residuo = TotDare - TotAvere
260200980115     C*
260300980115     C                   ENDSR
260400980115     C************************************************************
260500980115     C* Riempio campi del subfile con valori di database
260600980115     C************************************************************
260700980115     C     RieS1DaPar    Begsr
260800980119     C*
260900980119     C* parto presupponendo il rcd valido
261000980119     C                   MOVEL     *on           $RcdOk
261100010226C1458C* pulisco le variabili di riga
261200010226C1458C                   EVAL      H1RataBk = *off
261300980119     C*
261400980119     C* al primo test negativo, memorizzo che il rcd non è valido e vado a
261500980119     C* fine routine
261600040308     C                   DO
261700040308     C*
261800040308     C                   Z-ADD     PPANrAsInt    H1NrAsInt
261900040308     C     PPADtPar      IFGT      Dt0
262000040308     C                   CALLB     'XDT4000'
262100040308     C                   PARM      PPADtPar      XDTAMG           10
262200040308     C     S1DtPar       PARM                    XDTGMA            6 0
262300040308     C                   PARM      2             XDTSTA            1 0
262400070417C2012C                   move      PPADtPar      H1DtPar
262500040308     C                   ELSE
262600980115     C                   MOVE      *zero         S1DtPar
262700070419C2012C                   move      *loval        WDataIso
262800070419C2012C                   move      WDataIso      H1DtPar
262900040308     C                   ENDIF
263000040308     C                   MOVE      PPANrPar      S1NrPar
263100040308     C                   MOVE      PPASerPar     S1SerPar
263200040308     C                   MOVE      PPADivisa     S1Divisa
263300040308     C*
263400040308     C* se il valore di residuo è negativo
263500040308     C                   IF        S1Residuo < 0
263600040308     C* lo trasformo in valore assoluto
263700040308     C                   MULT      -1            S1Residuo
263800040308     C                   MULT      -1            S1ResiduoD
263900040308     C* dichiaro che il suo segno è Avere
264000040308     C                   EVAL      S1Segno = XscAvere
264100040308     C* se il valore di residuo è positivo
264200040308     C                   ELSE
264300040308     C* dichiaro che il suo segno è Dare
264400040308     C                   EVAL      S1Segno = XscDare
264500040308     C                   ENDIF
264600980119     C*
264700980119     C* testo se il segno è valido rispetto alle selezioni
264800040308     C                   IF        S1Segno = XscDare  and
264900040308     C                             Segno079 = XscAvere     or
265000040308     C                             S1Segno = XscAvere and
265100040308     C                             Segno079 = XscDare
265200040308     C* il rcd non è valido
265300040308     C                   MOVEL     *off          $RcdOk
265400040308     C                   LEAVE
265500040308     C                   ENDIF
265600040308     C*
265700040308     C* se il conto da saldacontare è un cliente
265800040308     C                   IF        SNatura002 = 'C'
265900040308     C* se l'IVA dare è <> 0
266000040308     C                   IF        X20IVADare <> 0
266100040308     C* l'importo originale è l'IVA dare
266200040308     C                   EVAL      S1ImpOriD = X20IVADare
266300040308     C* altrimenti
266400040308     C                   ELSE
266500980115     C* l'importo originale è l'IVA avere
266600980115     C                   EVAL      S1ImpOriD = X20IVAAver
266700040308     C                   ENDIF
266800040308     C* se il conto da saldacontare è un fornitore
266900040308     C                   ELSE
267000040308     C* se l'IVA avere è <> 0
267100040308     C                   IF        X20IVAAver <> 0
267200040308     C* l'importo originale è l'IVA avere
267300040308     C                   EVAL      S1ImpOriD = X20IVAAver
267400040308     C* altrimenti
267500040308     C                   ELSE
267600040308     C* l'importo originale è l'IVA dare
267700040308     C                   EVAL      S1ImpOriD = X20IVADare
267800040308     C                   ENDIF
267900040308     C                   ENDIF
268000040308     C*
268100040308     C* se la partita è aperta anche per il valore in divisa
268200040308     C                   IF        S1ResiduoD <> 0
268300980115     C* calcolo il cambio del residuo
268400980925     C* il cambio lo ottengo con il rapporto fra i residui di partita
268500980925     C                   RESET                   PJX00202DS
268600980925     C                   MOVEL     S1Divisa      Divisa0202
268700980925     C                   MOVEL     XscDiv        DivMC0202
268800981005     C                   Z-ADD     S1ResiduoD    ImporD0202
268900981005     C                   Z-ADD     S1Residuo     ImporM0202
269000980925     C                   Z-ADD     0             Cambio0202
269100980925     C                   MOVEL     DtRif045      DtRif0202
269200980925     C* non richiedo il controllo scostamento cambio/importo perchè il
269300980925     C* cambio recuperato può essere un valore sballato
269400980925     C                   MOVEL     *on           NoCtrlScos
269500980925     C*
269600980925     C                   CALLP     X0202TestC(PJX00202DS :
269700980925     C                                        %size(PJX00202DS) :
269800980925     C                                        *off   : Esito   :
269900980925     C                                        NoCtrlScos : *omit    :
270000980925     C                                        %size(ANDVS00F)   :
270100980925     C                                        %size(ANDVX00F)   :
270200980925     C                                        ANDVS00F : ANDVX00F )
270300980925     C* se errori (dovrebbe dare come unico errore il fatto che il cambio
270400980925     C*  calcolato è >= 10000000 oppure < 0,000001)
270500040308     C                   IF        Esito <> '0'
270600040308     C* richiamo finestra con messaggio
270700040308     C                   EXSR      MsgEVALBrk
270800040308     C* se la riga è stata esclusa
270900040308     C                   IF        $RcdOK = *off
271000040308     C* salto il resto della routine (per evitare la proposta automatica)
271100040308     C                   LEAVE
271200040308     C                   ENDIF
271300040308     C* se NO errori
271400040308     C                   ELSE
271500040308     C* il cambio calcolato è quello della partita/scadenza
271600040308     C                   Z-ADD     Cambio0202    S1Cambio
271700040308     C                   ENDIF
271800980925     C*
271900980925     C* se la partita risulta chiusa in divisa (probabile mancanza delle
272000980925     C*  differenze cambio)
272100040308     C                   ELSE
272200040308     C* imposto il cambio calcolato = 0
272300040308     C                   EVAL      S1Cambio = 0
272400040308     C* reperisco i dati divisa di partita
272500040308     C                   IF        X0201RpDVS(S1Divisa:DtRif045:
272600040308     C                                        ANDVS00F: %size(ANDVS00F):
272700040308     C                                        ANDVX00F: %size(ANDVX00F))
272800040308     C                             <> 0
272900040308     C* se errori (l'unico possibile è che la divisa fosse annullata
273000040308     C*  alla data)
273100040308     C* richiamo finestra con messaggio
273200040308     C                   EXSR      MsgEVALBrk
273300040308     C                   ENDIF
273400040308     C                   ENDIF
273500040308     C*
273600040308     C* se il conto è un percipiente
273700040308     C                   IF        SNatura002 = 'F' and Percipi002 = *on
273800040308     C* recupero la ritenuta calcolata
273900040308     C                   EXSR      RepRitAcc
274000040308     C* decurto il residuo (sia in divisa che in m.c. visto che se c'è una
274100040308     C* ritenuta stò lavorando con partita in m.c.) della ritenuta
274200040308     C*
274300040308     C* se l'importo della ritenuta è < 0
274400040308     C                   IF        H1RitCalc < 0
274500040308     C* bisogna domandarsi se è una nota di accredito (segno Dare)
274600040308     C                   IF        S1Segno = XscDare
274700040308     C* in questo caso decurto algebricamente (cioè sommo)
274800040308     C                   EVAL      S1ResiduoD = S1ResiduoD + H1RitCalcD
274900040308     C                   EVAL      S1Residuo = S1Residuo + H1RitCalc
275000040308     C* altrimenti è un caso anomalo e non prendo in considerazione la
275100040308     C* ritenuta d'acconto
275200040308     C                   ELSE
275300040308     C                   EVAL      H1RitCalc = 0
275400040308     C                   EVAL      H1RitCalcD = 0
275500040308     C                   ENDIF
275600040308     C*
275700040308     C* se l'importo della ritenuta è >= 0
275800040308     C                   ELSE
275900040308     C* lo decurto semplicemente
276000040308     C                   EVAL      S1ResiduoD = S1ResiduoD - H1RitCalcD
276100040308     C                   EVAL      S1Residuo = S1Residuo - H1RitCalc
276200040308     C                   ENDIF
276300040308     C*
276400040308     C                   ENDIF
276500980115     C*
276600980115     C                   EVAL      S1FrcAbb = XscOff
276700980226     C* se divisa di partita <> da divisa di testata
276800980226     C* bisogna impostare il flag di forzatura importo in divisa di partita
276900040308     C                   IF        S1Divisa <> C1Divisa
277000040308     C* se divisa di riga = m.c.
277100040308     C                   IF        S1Divisa = XscDiv
277200040308     C* la sopravenienza deve esserci (quindi la forzatura deve essere='N')
277300040308     C                   EVAL      S1FrcImpDP = XscOff
277400040308     C* negli altri casi
277500040308     C                   ELSE
277600040308     C* propongo la forzatura di tabella
277700040308     C                   IF        FrcImDP0IP = *on
277800040308     C                   EVAL      S1FrcImpDP = xscon
277900040308     C                   ELSE
278000040308     C                   EVAL      S1FrcImpDP = xscoff
278100040308     C                   ENDIF
278200040308     C                   ENDIF
278300040308     C                   ENDIF
278400980115     C                   EVAL      S1BkAmm = BkAmm
278500010105C1458C* se non c'è blocco sulla partita
278600010105  "  C                   IF        S1BkAmm = *blank
278700010105  "  C* provo con quello di rata
278800010105  "  C                   EVAL      S1BkAmm = BkAmmPSC
278900010105C1458C                   ENDIF
279000980115     C                   EVAL      S1BkAmmD = *blank
279100980115     C* reperisco descrizione breve blocco
279200040308     C                   IF        S1BkAmm <> *blank
279300040308     C                   EVAL      XtbRic = '1'
279400040308     C                   EVAL      XtbAzi = XscSoc
279500040308     C                   EVAL      XtbKey = *zero
279600040308     C                   EVAL      XtbCod = '0AB'
279700040308     C                   MOVE      S1BkAmm       XtbKey
279800040308     C                   CALLB     'XATB'
279900040308     C                   PARM                    XAtbDS
280000040308     C                   IF        XtbErr <> '0'
280100040308     C                   EVAL      S1BkAmmD = *all'?'
280200040308     C                   ELSE
280300040308     C                   EVAL      ANG0ABDS = XtbUni
280400040308     C                   EVAL      S1BkAmmD = De20AB
280500040308     C                   ENDIF
280600040308     C                   ENDIF
280700010226C1458C*
280800010226  "  C* se non esiste nessun blocco di partita
280900010226  "  C                   IF        S1BkAmm = *blank
281000010226  "  C* e la visione delle partite non è per scadenza
281100010226  "  C                             and X20ParScd <> 'S'
281200010226  "  C* memorizzo se esiste un blocco di rata che in questo caso non
281300010226  "  C* è gestibile
281400010226  "  C                   EVAL      H1RataBk = C0602BkPSC
281500010226  "  C                                        (PPASys:
281600010226  "  C                                         PPANrAsInt)
281700010226C1458C                   ENDIF
281800010226     C*
281900980122     C* se la riga è selezionabile, cioè:
282000980122     C*  non ci sono blocchi amministrativi sul conto
282100040308     C                   IF        (C1BkAmm = *blank or
282200980205     C                              SNatura002 = 'C' or
282300980205     C                              SNatura002 = 'F' and
282400980205     C                              S1Segno = XscDare)   and
282500980122     C*  non ci sono blocchi amministrativi sulla partita
282600980205     C                             (S1BkAmm = *blank or
282700980205     C                              SNatura002 = 'C') and
282800980122     C*  e la partita sarebbe elaborabile a cambio storico perchè la sua
282900980122     C*  divisa è la stessa di testata
283000980122     C                             (S1Divisa = C1Divisa or
283100980122     C                              CmbSto045 <> *on)
283200980115     C* se richiesta la proposta automatica dell'importo in conto
283300040308     C                   IF        PropAut045 = XscOn and
283400040308     C* e l'importo ancora da ripartire è > 0
283500040308     C                             AutoImpD    > 0
283600040308     C* eseguo la ripartizione dell'importo da saldacontare sull'importo in
283700040308     C* conto a seconda del residuo di partita
283800040308     C                   EXSR      PropAuto
283900040308     C                   ENDIF
284000040308     C                   ENDIF
284100040308     C*
284200040308     C                   ENDDO
284300040308     C*
284400040308     C* se la riga è stata inclusa
284500040308     C                   IF        $RcdOK = *on  and
284600981203     C* e la divisa di partita è divera da quella di testata
284700981203     C                             C1Divisa <> S1Divisa
284800981203     C* controllo che legame c'è tra la divisa della partita e quella di
284900981203     C* testata :
285000981203     C* se tra di loro ci sono solo cambi fissi (una delle due è divisa
285100981203     C*  di triangolazione e l'altra ha un cambio fisso con questa, oppure
285200981203     C*  entrambe triangolano con la stessa divisa), l'importo concordato
285300981203     C* non deve essere digitabile dall'utente ma calcolato dal pgm
285400981203     C                   MOVEL     DtRif045      WDataISO
285500040308     C                   IF        X0201TpTri(H1Triang        :
285600981203     C                                        C1Divisa        :
285700981203     C                                        S1Divisa        :
285800981203     C                                        WDataISO        :
285900981203     C                                        *omit            )
286000981203     C* gli errori possibili sono solo relativi alle divise; in questo caso
286100981203     C* non vi possono essere in quanto vengono già passati i dati delle
286200981203     C* medesime e quindi sono già state testate
286300981203     C                             = 0
286400981203     C*
286500981203     C* se la divise di partita e di contabile hanno tra loro SOLO cambi
286600040308     C* fissi
286700040308     C                   IF        H1Triang = '3' or
286800040308     C                             H1Triang = '4'
286900040308     C* il flag di forzatura importo in divisa di partita DEVE essere SI
287000040308     C* non perchè è obbligatoria una sopravvenienza che invece NON ci
287100040308     C* sarà, ma perchè è l'importo concordato ad essere = a quello
287200040308     C* calcolato
287300040308     C                   EVAL      S1FrcImpDP = XscOn
287400040308     C                   ENDIF
287500040308     C*
287600040308     C                   ENDIF
287700040308     C                   ENDIF
287800981203     C*
287900980115     C                   ENDSR
288000980115     C************************************************************
288100980115     C* Proposta automatica importo in conto
288200980115     C************************************************************
288300980115     C     PropAuto      BEGSR
288400980115     C*
288500980115     C                   SELECT
288600980115     C*
288700980115     C* se la divisa di riga è la stessa di testata
288800980115     C*
288900040308     C                   WHEN      S1Divisa = C1Divisa
289000980115     C*
289100980115     C* se il segno di chiusura partita è opposto a quello dell'operazione
289200980115     C* principale di saldaconto
289300040308     C                   IF        S1Segno <> Segno045
289400040308     C*
289500040308     C* se residuo partita non eccede il rimanente importo da saldacontare
289600040308     C                   IF        S1ResiduoD <= AutoImpD
289700040308     C* valorizzo l'importo in conto con il residuo in div. della partita
289800040308     C                   EVAL      S1ImportD = S1ResiduoD
289900040308     C* altrimenti
290000040308     C                   ELSE
290100040308     C* valorizzo importo in conto con il rimanente importo da saldacontare
290200040308     C                   EVAL      S1ImportD = AutoImpD
290300040308     C                   ENDIF
290400040308     C* storno dal totale saldacontabile l'importo in conto
290500040308     C                   EVAL      AutoImpD    = AutoImpD    - S1ImportD
290600040308     C*
290700040308     C* se il segno di chiusura partita è uguale a quello dell'operazione
290800040308     C* principale di saldaconto
290900040308     C                   ELSE
291000980115     C*
291100980115     C* valorizzo l'importo in conto con il residuo in div. della partita
291200980115     C                   EVAL      S1ImportD = S1ResiduoD
291300980115     C* aggiungo al totale saldacontabile l'importo in conto
291400980115     C                   EVAL      AutoImpD    = AutoImpD    + S1ImportD
291500040308     C                   ENDIF
291600980115     C*
291700980115     C                   EVAL      S1Selez = 'X'
291800980115     C*
291900981007     C* se la divisa di partita è <> da quella di testata ma è la m.c.
292000980115     C*
292100040308     C                   WHEN      S1Divisa <> C1Divisa and
292200980115     C                             S1Divisa = XscDiv
292300980115     C*
292400990526     C* valorizzo l'importo concordato con il residuo in m.c. della partita
292500990526     C                   EVAL      S1ImpConc = S1Residuo
292600980115     C* calcolo l'importo in conto dall'importo concordato
292700980923     C                   RESET                   PJX00202DS
292800981007     C                   MOVEL     C1Divisa      Divisa0202
292900980923     C                   MOVEL     XscDiv        DivMC0202
293000981007     C                   Z-ADD     0             ImporD0202
293100981007     C                   Z-ADD     S1ImpConc     ImporM0202
293200981007     C* NON esiste gestione a cambio storico per righe con divisa di partita
293300981007     C* <> da contabile
293400981007     C* per cui uso il cambio di testata
293500980923     C                   Z-ADD     C1Cambio      Cambio0202
293600980923     C                   MOVEL     DtRif045      DtRif0202
293700981005     C* non richiedo il controllo scostamento cambio/importo perchè il
293800981005     C* cambio recuperato è un valore storico
293900981005     C                   MOVEL     *on           NoCtrlScos
294000980923     C*
294100980923     C                   CALLP     X0202TestC(PJX00202DS :
294200980923     C                                        %size(PJX00202DS) :
294300980923     C                                        *off   : Esito   :
294400981005     C                                        NoCtrlScos : *omit)
294500981007     C                   EVAL      S1ImportD = ImporD0202
294600980115     C*
294700980115     C* se il segno di chiusura partita è opposto a quello dell'operazione
294800980115     C* principale di saldaconto
294900040308     C                   IF        S1Segno <> Segno045
295000040308     C* se l'importo trovato è <= dell'importo da saldacontare
295100040308     C                   IF        S1ImportD <= AutoImpD
295200040308     C* l'importo in conto è = all'importo trovato
295300040308     C*
295400040308     C* altrimenti
295500040308     C                   ELSE
295600040308     C* l'importo in conto è il rimanente importo da saldacontare
295700040308     C                   EVAL      S1ImportD = AutoImpD
295800040308     C* l'importo concordato lo calcolo rispetto a questo
295900040308     C                   RESET                   PJX00202DS
296000040308     C                   MOVEL     C1Divisa      Divisa0202
296100040308     C                   MOVEL     XscDiv        DivMC0202
296200040308     C                   Z-ADD     S1ImportD     ImporD0202
296300980923     C                   Z-ADD     0             ImporM0202
296400981007     C* NON esiste gestione a cambio storico per righe con divisa di partita
296500981007     C* <> da contabile
296600981007     C* per cui uso il cambio di testata
296700980923     C                   Z-ADD     C1Cambio      Cambio0202
296800980923     C                   MOVEL     DtRif045      DtRif0202
296900981005     C* non richiedo il controllo scostamento cambio/importo perchè il
297000981005     C* cambio recuperato è un valore storico
297100981005     C                   MOVEL     *on           NoCtrlScos
297200980923     C*
297300980923     C                   CALLP     X0202TestC(PJX00202DS :
297400980923     C                                        %size(PJX00202DS) :
297500980923     C                                        *off   : Esito   :
297600981005     C                                        NoCtrlScos : *omit)
297700990615C0941C                   EVAL      S1ImpConc = ImporM0202
297800040308  "  C* sconto e abbuono non vengono presi in considerazione perchè in
297900040308  "  C* caricamento righe questi dati sono a 0 e quasta routine NON
298000990615C0941C* viene usata quando si vuole fare un passaggio P a D
298100990615     C                   ENDIF
298200980115     C* storno dal totale saldacontabile l'importo in conto
298300980115     C                   EVAL      AutoImpD    = AutoImpD    - S1ImportD
298400980115     C*
298500980115     C* se il segno di chiusura partita è uguale a quello dell'operazione
298600980115     C* principale di saldaconto
298700040308     C                   ELSE
298800980115     C* aggiungo al totale saldacontabile l'importo in conto
298900980115     C                   EVAL      AutoImpD    = AutoImpD    + S1ImportD
299000040308     C                   ENDIF
299100980115     C*
299200980115     C                   EVAL      S1Selez = 'X'
299300980115     C*
299400980115     C* in altri casi
299500980115     C*
299600040308     C                   OTHER
299700980115     C*
299800980115     C* la proposta automatica finisce qui
299900980115     C                   EVAL      AutoImpD    = 0
300000980115     C*
300100040308     C                   ENDSL
300200980115     C*
300300980115     C                   ENDSR
300400950706     C************************************************************
300500951120     C* Reperisco la ritenuta d'acconto collegata alla partita
300600950706     C************************************************************
300700951120     C     RepRitAcc     BEGSR
300800950706     C*
300900951120     C                   RESET                   NDCR96DS
301000951120     C                   MOVE      XscSoc        R96Societa
301100951120     C                   MOVE      C1Kcc         R96Kcc
301200951120     C                   MOVE      C1Ksc         R96Ksc
301300951120     C                   MOVE      PPANrPar      R96NrPar
301400951120     C                   MOVE      PPADtPar      R96DtPar
301500951120     C                   MOVE      PPASerPar     R96SerPar
301600951122     C                   Z-ADD     £NrDec        R96NrDec
301700981103     C                   Z-ADD     DVXNrDec      R96NrDecD
301800951122     C                   Z-ADD     0             R96Per
301900981104     C                   EVAL      R96DivMC = XscDiv
302000981104     C                   EVAL      R96DtRfCam = DtRif045
302100981104     C* se la divisa di partita è = a quella di testata
302200981104     C                   IF        S1Divisa = C1Divisa
302300981104     C* passo il cambio di testata
302400981104     C                   EVAL      R96Cambio = C1Cambio
302500981104     C* dichiaro che la modalità è di ricalcolo
302600981104     C                   EVAL      R96Modo = '2'
302700981104     C* altrimenti
302800981104     C                   ELSE
302900981104     C* lo faccio calcolare alla routine
303000981104     C                   EVAL      R96Cambio = 0
303100981104     C* dichiaro che la modalità è storica
303200981104     C                   EVAL      R96Modo = '1'
303300981104     C                   ENDIF
303400951120     C*
303500951120     C                   CALL      'NDCR96R'
303600951120     C                   PARM                    NDCR96DS
303700951120     C*
303800991008D0254C                   EVAL      H1RitAcc0 = '0'
303900951120     C                   IF        R96Err = *off
304000951120     C                   Z-ADD     R96RAc        H1RitCalc
304100981022     C                   Z-ADD     R96RAcD       H1RitCalcD
304200991008D0254C* se la ritenuta reperita è 0
304300991008  "  C                   IF        R96RAc = 0 and
304400991008  "  C                             R96RAcD = 0
304500991008  "  C* siamo nel caso di una ritenuta a % 0 e quindii deve generare
304600991008  "  C* lo stesso una scrittura
304700991008  "  C                   EVAL      H1RitAcc0 = '1'
304800991008  "  C                   ENDIF
304900991008D0254C*
305000951120     C                   ENDIF
305100951120     C*
305200951120     C                   ENDSR
305300951120     C************************************************************
305400951120     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
305500951120     C* INPUT
305600951120     C* $FC1 = Abilitazioni videata S1
305700951120     C* OUTPUT
305800951120     C* *IN97 *IN99 Errori
305900951120     C************************************************************
306000951120     C     CTRKS1        Begsr
306100951120     C*
306200941205     C                   SETOFF                                       99
306300950721     C                   MoveA     $FC1          $FC
306400950721     C                   Exsr      CTRKEY
306500941205     C     *IN99         IFEQ      *ON
306600950804     C                   Eval      In(97) = *on
306700950721     C                   Endif
306800941205     C*
306900950721     C                   Endsr
307000940309     C************************************************************
307100940309     C* GESTIONE F03 VIDEO S1
307200940309     C************************************************************
307300950721     C     F03S1         Begsr
307400940309     C*
307500950721     C                   Move      *ON           $FINE
307600040308     C                   Move      '1'           RET045
307700940325     C* fine programma
307800950721     C                   Exsr      ENDPGM
307900940309     C*
308000950721     C                   Endsr
308100950704     C************************************************************
308200950704     C* GESTIONE F05 VIDEO S1 - ripristino
308300950704     C************************************************************
308400950721     C     F05S1         Begsr
308500950704     C*
308600950721     C                   Move      *on           $InzS1
308700950727     C                   Move      *hival        $LastV
308800950704     C*
308900950721     C                   Endsr
309000950707     C************************************************************
309100950710     C* GESTIONE F06 VIDEO S1 - Conferma
309200950707     C************************************************************
309300950721     C     F06S1         Begsr
309400950707     C*
309500951020     C                   Exsr      CTRC1
309600040308     C     *IN99         IFEQ      *OFF
309700950721     C                   Exsr      CTRS1
309800951018     C* se non ci sono errori
309900991108D0286C                   if        *in99=*off
310000991108  "   *
310100991108  "   * Controllo che sia diverso da 0
310200991108  "   * almeno uno tra i totali importo, abbuono, sconto e ritenuta
310300991108  "   *
310400991108  "  C                   if        D1ImportD = *zeros
310500991108  "  C                             and D1AbbuonoD = *zeros
310600991108  "  C                             and D1ScontoD = *zeros
310700991108  "  C                             and D1Ritenuta = *zeros
310800991108  "  C                   seton                                        7999
310900991108  "  C                   eval      in(79) = *in79
311000991108  "  C                   Endif
311100991108  "  C                   Endif
311200991108D0286C*
311300040308     C                   if        *in99=*off
311400951120     C* se ho premuto F11 e non F6
311500040308     C                   IF        $Tasto = F11
311600040308     C* richiamo la gestione cessioni
311700040308     C                   EXSR      CallCess
311800040308     C                   Endif
311900040308     C* se ancora tutto OK
312000040308     C                   if        *in99=*off
312100040308     C* richiedo l'aggiornamento
312200040308     C                   Exsr      Aggiorna
312300040308     C                   Endif
312400040308     C* Se aggiornamento Ok esco da pgm
312500040308     C                   if        *in99=*off
312600040308     C                   Exsr      PreparaOut
312700040308     C                   Endif
312800040308     C                   Endif
312900040308     C                   Endif
313000040308     C*
313100040308     C                   Endsr
313200950821     C************************************************************
313300950821     C* GESTIONE F08 VIDEO S1
313400950821     C************************************************************
313500950821     C     F08S1         BEGSR
313600950821     C*
313700950801     C                   MOVE      *ON           $FINE
313800951026     C                   MOVE      '0'           RET045
313900950801     C*
314000950801     C                   ENDSR
314100940309     C************************************************************
314200940309     C* GESTIONE F12 VIDEO S1
314300940309     C************************************************************
314400960109     C     F12S1         BEGSR
314500940309     C*
314600950721     C                   Move      *ON           $FINE
314700951026    >C                   Move      '2'           RET045
314800940309     C*
314900960109     C                   ENDSR
315000960201     C************************************************************
315100960201     C* GESTIONE F15 VIDEO S1 - Interrogazione conti
315200960201     C************************************************************
315300960201     C     F15S1         BEGSR
315400960201     C*
315500960201     C                   if        C1kcc > *blanks
315600960201     C                   reset                   ana399ds
315700960201     C                   eval      A39societa = XSCSOC
315800960201     C                   eval      A39unita = REGUnita
315900960201     C                   eval      A39ctb = REGCtb
316000960201     C                   eval      A39kcc = C1Kcc
316100960201     C                   eval      A39ksc = C1Ksc
316200960201     C                   movel     ana399ds      kpjbu
316300960201     C                   CALL      'ANA399R'                            21
316400960201     C                   PARM                    KPJBA
316500040308     C     A39ERRORI     IFEQ      '0'
316600960201     C                   endif
316700960201     C*
316800960201     C                   endif
316900960201     C*
317000960201     C                   ENDSR
317100960109     C************************************************************
317200960109     C* GESTIONE F19 VIDEO S1
317300960109     C************************************************************
317400960109     C     F19S1         BEGSR
317500960109     C*
317600960109     C                   Z-ADD     RigaVuota     C1Rcd
317700960109     C*
317800960109     C                   ENDSR
317900940309     C************************************************************
318000940309     C* GESTIONE F24 VIDEO S1
318100940309     C************************************************************
318200950721     C     F24S1         Begsr
318300940506     C*
318400950721     C                   MoveA     $FX1          $FX
318500950721     C                   MoveA     $FC1          $FC
318600950721     C                   MoveA     $FR1          $FR
318700940506     C                   Z-ADD     $ULKS1        $ULK
318800040308     C                   Z-ADD     $S1L01        $L01
318900040308     C                   Z-ADD     0             $L02
319000950721     C                   Exsr      RIEKEY
319100940506     C                   Z-ADD     $ULK          $ULKS1            3 0
319200950721     C                   MoveA     $FC           $FC1
319300950721     C                   Movel     $KEY1         Z1KE1
319400951030     C***                Movel     $KEY2         Z1KE2
319500060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
319600950704     C                   WRITE     FMTZ1
319700940506     C*
319800950721     C                   Endsr
319900940128     C************************************************************
320000940131     C* CONTROLLO TESTATA LISTA
320100940128     C************************************************************
320200950721     C     CTRC1         Begsr
320300940128     C*
320400950721     C                   Move      *OFF          *IN99
320500940202     C*
320600951027     C* flag forzatura errato
320700951027     C                   IF        C1ForzaSco <> Xscon   and
320800951027     C                             C1ForzaSco <> Xscoff
320900951027     C                   SETON                                        5099
321000951027     C                   ENDIF
321100951027     C*
321200990803C1209 *
321300990803  "  C* test esistenza commenti sul conto
321400990803  "  C                   EXSR      CTR724Kcc
321500990803  "   * Se esiste commento sul conto lo evidenzio
321600990803  "  C                   IF        Exi724 = *On
321700990803  "  C                   SETON                                        20
321800990803  "  C                   ELSE
321900990803  "  C                   SETOFF                                       20
322000990803  "  C                   ENDIF
322100990803C1209 *
322200950721     C                   Endsr
322300990803C1209C************************************************************
322400990803  "  C* Controllo esistenza commenti sul conto
322500990803  "  C************************************************************
322600990803  "  C     CTR724Kcc     BEGSR
322700990803  "  C*
322800990803  "  C                   RESET                   ANA724DS
322900990803  "  C                   MOVE      *BLANK        EXI724
323000990803  "  C                   MOVEL     '01'          OPZ724
323100990803  "  C                   MOVEL     XSCSOC        SOC724
323200990803  "  C                   MOVEL     *BLANK        UNI724
323300990803  "  C                   SELECT
323400990803  "  C     SNatura002    wheneq    'C'
323500990803  "  C                   MOVEL     '700'         TPA724
323600990803  "  C                   MOVEL     C1Ksc         KY1724
323700990803  "  C                   MOVEL     '000'         KY3724
323800990803  "  C     SNatura002    wheneq    'F'
323900990803  "  C                   MOVEL     '001'         TPA724
324000990803  "  C                   MOVEL     c1KSC         KY1724
324100990803  "  C                   OTHER
324200990803  "  C                   MOVEL     'PDC'         TPA724
324300990803  "  C                   MOVEL     C1Kcc         KY1724
324400990803  "  C                   MOVEL     C1Ksc         KY2724
324500990803  "  C                   endsl
324600990803  "  C                   MOVEL     Oggi          DTS724
324700990803  "  C                   MOVEL     ANA724DS      KPJBU
324800990803  "  C                   CALL      'ANA724R'
324900990803  "  C                   PARM                    KPJBA
325000990803  "  C                   MOVEL     KPJBU         ANA724DS
325100990803  "  C*
325200990803  "  C* memorizzo la presenza o meno di commenti
325300990803  "  C                   MOVE      EXI724        H1Comm
325400990803  "  C*
325500990803C1209C     XCtr724Kcc    ENDSR
325600940131     C************************************************************
325700940131     C* CONTROLLO OPZIONI LISTA
325800940131     C************************************************************
325900950721     C     CTRS1         Begsr
326000940131     C*
326100950721     C                   Movel     *OFF          $ESCI
326200940201     C                   SETOFF                                       99
326300960318     C                   EVAL      ErrSfl = 0
326400950825     C*
326500951027     C* pulizia campi lavoro totali importi
326600951030     C                   Clear                   WImportD
326700951030     C                   Clear                   WImporto
326800951027     C                   Clear                   WAbbuonoD
326900951027     C                   Clear                   WAbbuono
327000951027     C                   Clear                   WScontoD
327100951027     C                   Clear                   WSconto
327200951027     C                   Clear                   WRitenuta
327300951027     C                   Clear                   WDiffCam
327400951108     C                   Clear                   AbbAttivo
327500951108     C                   Clear                   AbbPassivo
327600960115     C                   Clear                   AbbAttivoD
327700960115     C                   Clear                   AbbPassivD
327800951109     C                   Clear                   DiffCamAtt
327900951109     C                   Clear                   DiffCamPas
328000990528C0762C                   CLEAR                   DivSconto
328100990519  "  C                   MOVE      *off          DivDiveSco
328200990519  "  C                   CLEAR                   DivAbbuono
328300990528C0762C                   MOVE      *off          DivDiveAbb
328400000821C1393C                   CLEAR                   Arrotondam
328500940131     C*
328600940127     C* Leggo il sfl solo se ci sono rcd
328700950721     C                   ReadC     FMTS1                                  21
328800940127     C*
328900940131     C* esce se fine sfl o errore che richiede l'uscita
329000040308     C     *IN21         DOWEQ     *OFF
329100940131     C     $ESCI         ANDEQ     *OFF
329200940131     C* ctrl su riga
329300950721     C                   Exsr      CheckS1
329400950630     C* attivo sempre sflnxtchg
329500950721     C                   Move      *ON           *IN32
329600950801     C* imposto gli attributi del sfl
329700950801     C                   EXSR      AtrS1
329800940223     C*
329900940127     C                   UPDATE    FMTS1
330000950721     C                   Move      *OFF          *IN32
330100940223     C*
330200940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
330300040308     C     $ESCI         IFEQ      *OFF
330400040308     C                   ReadC     FMTS1                                  21
330500040308     C* a fine ciclo ripristino il flag richiesta iniz.sfl
330600040308     C                   Endif
330700040308     C*
330800040308     C                   Enddo
330900040308     C*
331000040308     C* ctrl sui totali
331100040308     C*
331200040308     C* se il segno con cui devo saldacontare è inverso a quello
331300040308     C* di tabella
331400980116     C                   IF        Segno045 = XscDare and
331500980116     C                             SOP0IP = 'A'            or
331600980116     C                             Segno045 = XscAvere and
331700980116     C                             SOP0IP = 'D'
331800951122     C* inverto anche il segno con cui testare la squadratura
331900951122     C                   EVAL      ImpSegD = C1ImportD * -1
332000951122     C                   EVAL      ImpSeg  = C1Importo * -1
332100951122     C                   ELSE
332200951122     C                   EVAL      ImpSegD = C1ImportD
332300951122     C                   EVAL      ImpSeg  = C1Importo
332400951122     C                   ENDIF
332500951122     C*
332600951113     C* se il totale di testata in divisa è > 0
332700951113     C                   IF        C1ImportD > 0           and
332800951113     C* ed esiste una squadratura totale in divisa
332900951122     C                             WImportD <> ImpSegD
333000951110     C                   SETON                                        7399
333100951110     C                   EVAL      in(73) = *in73
333200951122     C                   EVAL      Pack0019 = ImpSegD - WImportD
333300960122     C     ImpSegD       SUB       WImportD      Pack0019
333400951110     C                   ENDIF
333500951113     C*
333600951113     C* se il totale di testata in m.c. è > 0
333700951113     C                   IF        C1Importo > 0             and
333800951113     C* ed esiste una squadratura totale in m.c.
333900951122     C                             WImporto <> ImpSeg        and
334000951113     C* ma non ci sono ulteriori errori
334100951113     C                             *in99 = *off
334200951113     C*  eseguo una riquadratura automatica
334300951113     C                   EXSR      RiquadraMC
334400951113     C                   ENDIF
334500951023     C*
334600951121     C* imposto i totali parziali a pié pagina
334700951121     C                   Z-ADD     WImportD      D1ImportD
334800951121     C                   Z-ADD     WImporto      D1Importo
334900951121     C                   IF        WImportD <> 0
335000981005     C* se il cambio non è storico prende quello di testata
335100971230     C                   IF        CmbSto045 = *off
335200971230     c                   Z-ADD     c1cambio      d1cambio
335300981005     C* se gestione a cambio storico
335400981005     C                   ELSE
335500981005     C* calcolo il cambio dagli importi
335600981005     C                   RESET                   PJX00202DS
335700981005     C                   MOVEL     C1Divisa      Divisa0202
335800981005     C                   MOVEL     XscDiv        DivMC0202
335900981005     C                   Z-ADD     WImportD      ImporD0202
336000981005     C                   Z-ADD     WImporto      ImporM0202
336100981005     C                   Z-ADD     0             Cambio0202
336200981005     C                   MOVEL     DtRif045      DtRif0202
336300981005     C* non richiedo il controllo scostamento cambio/importo perchè il
336400981005     C* cambio recuperato è un valore storico
336500981005     C                   MOVEL     *on           NoCtrlScos
336600981005     C*
336700981005     C                   CALLP     X0202TestC(PJX00202DS :
336800981005     C                                        %size(PJX00202DS) :
336900981005     C                                        *off   : Esito   :
337000981005     C                                        NoCtrlScos : *omit)
337100981005     C                   EVAL      D1Cambio = Cambio0202
337200971230     C                   ENDIF
337300951121     C                   ELSE
337400951121     C                   EVAL      D1Cambio = 0
337500951121     C                   ENDIF
337600990210     C*
337700990210     C* totali abbuono
337800990210     C*
337900990601     C                   Z-ADD     WAbbuonoD     D1AbbuonoD
338000951121     C                   Z-ADD     WAbbuono      D1Abbuono
338100990531C0941C                   IF        WAbbuono  <> 0
338200011109D1004C                             or WAbbuonoD <> 0
338300990528C0941C* se le divise degli abbuoni sono sempre le stesse
338400990526  "  C                   IF        DivDiveAbb = *off
338500990526  "  C* l'abbuono in divisa è la sommatoria dei valori in riga di sfl
338600990526  "  C                   MOVEL     DivAbbuono    D1DivAbb
338700990526  "  C*
338800990526  "  C                   SELECT
338900990526  "  C*
339000990526  "  C* se gestione a cambio storico
339100990526  "  C                   WHEN      CmbSto045 = *on
339200990526  "  C* uso il cambio degli importi tanto è questo che viene usato
339300990526  "  C* nella scrittura del movimento
339400990526  "  C                   EVAL      D1CamAbb = D1Cambio
339500990526  "  C*
339600990526  "  C* se la divisa degl'abbuoni è diversa da quella di contabile
339700040308C0941C                   WHEN      DivAbbuono <> C1Divisa
339800981005     C* calcolo il cambio dagli importi
339900981005     C                   RESET                   PJX00202DS
340000990531     C                   MOVEL     DivAbbuono    Divisa0202
340100981005     C                   MOVEL     XscDiv        DivMC0202
340200990602C0941C* X0202TestC non accetta valori negativi ma il totale abbuoni può
340300990602  "  C* esserlo
340400990602  "  C                   IF        WAbbuonoD < 0
340500990602  "  C                   Z-SUB     WAbbuonoD     ImporD0202
340600990602  "  C                   Z-SUB     WAbbuono      ImporM0202
340700990602C0941C                   ELSE
340800981005     C                   Z-ADD     WAbbuonoD     ImporD0202
340900981005     C                   Z-ADD     WAbbuono      ImporM0202
341000990602C0941C                   ENDIF
341100981005     C                   Z-ADD     0             Cambio0202
341200981005     C                   MOVEL     DtRif045      DtRif0202
341300981005     C* non richiedo il controllo scostamento cambio/importo perchè il
341400981005     C* cambio recuperato è un valore storico
341500981005     C                   MOVEL     *on           NoCtrlScos
341600981005     C*
341700981005     C                   CALLP     X0202TestC(PJX00202DS :
341800981005     C                                        %size(PJX00202DS) :
341900981005     C                                        *off   : Esito   :
342000981005     C                                        NoCtrlScos : *omit)
342100981005     C                   EVAL      D1CamAbb = Cambio0202
342200990528C0941C*
342300990526  "  C* se non si è a cambio storico e gl'abbuoni sono in divisa di
342400990526  "  C* contabile
342500990526  "  C                   OTHER
342600990526  "  C* prende quello di testata
342700990526  "  c                   Z-ADD     C1Cambio      D1CamAbb
342800990526  "  C                   ENDSL
342900990526  "  C*
343000990526  "  C* se le divise degl'abbuoni NON sono sempre le stesse
343100990526  "  C                   ELSE
343200990526  "  C* lo sconto viene trasformato in m.c.
343300990526  "  C                   Z-ADD     WAbbuono      D1AbbuonoD
343400990526  "  c                   Z-ADD     1             D1CamAbb
343500990526  "  C                   MOVEL     XscDiv        D1DivAbb
343600990526  "  C                   ENDIF
343700040308  "  X****************** ENDIF
343800990528C0941C*
343900951121     C                   ELSE
344000951121     C                   EVAL      D1CamAbb = 0
344100990531C0941C                   MOVEL     *blank        D1DivAbb
344200951121     C                   ENDIF
344300990210     C*
344400990210     C* totali sconto
344500990210     C*
344600990528C0941C                   Z-ADD     WSconto       D1Sconto
344700990601C0941C                   Z-ADD     WScontoD      D1ScontoD
344800040308     C                   IF        WSconto <> 0
344900990528C0762C* se le divise degli sconti sono sempre le stesse
345000990519  "  C                   IF        DivDiveSco = *off
345100990601C0762C* lo sconto in divisa è la sommatoria dei valori in riga di sfl
345200040308C0941X****************** Z-ADD     WScontoD      D1ScontoD
345300990601C0762C                   MOVEL     DivSconto     D1DivSco
345400990519  "  C*
345500990519  "  C                   SELECT
345600990519  "  C*
345700990519  "  C* se gestione a cambio storico
345800990519  "  C                   WHEN      CmbSto045 = *on
345900990519  "  C* uso il cambio degli importi tanto è questo che viene usato
346000990519  "  C* nella scrittura del movimento
346100990519  "  C                   EVAL      D1CamSco = D1Cambio
346200990519  "  C*
346300990519  "  C* se la divisa degli sconti è diversa da quella di contabile
346400990528C0762C                   WHEN      DivSconto <> C1Divisa
346500981005     C* calcolo il cambio dagli importi
346600981005     C                   RESET                   PJX00202DS
346700990528C0762C                   MOVEL     DivSconto     Divisa0202
346800981005     C                   MOVEL     XscDiv        DivMC0202
346900981005     C                   Z-ADD     WScontoD      ImporD0202
347000981005     C                   Z-ADD     WSconto       ImporM0202
347100981005     C                   Z-ADD     0             Cambio0202
347200981005     C                   MOVEL     DtRif045      DtRif0202
347300981005     C* non richiedo il controllo scostamento cambio/importo perchè il
347400981005     C* cambio recuperato è un valore storico
347500981005     C                   MOVEL     *on           NoCtrlScos
347600981005     C*
347700981005     C                   CALLP     X0202TestC(PJX00202DS :
347800981005     C                                        %size(PJX00202DS) :
347900981005     C                                        *off   : Esito   :
348000981005     C                                        NoCtrlScos : *omit)
348100981005     C                   EVAL      D1CamSco = Cambio0202
348200990528C0762C*
348300990526  "  C* se non si è a cambio storico e gli sconti sono in divisa di
348400990526  "  C* contabile
348500990526  "  C                   OTHER
348600990526  "  C* prende quello di testata
348700990526  "  c                   Z-ADD     C1Cambio      D1CamSco
348800990526  "  C                   ENDSL
348900990526  "  C*
349000990526  "  C* se le divise degli sconti NON sono sempre le stesse
349100990526  "  C                   ELSE
349200990526  "  C* lo sconto viene trasformato in m.c.
349300990526  "  C                   Z-ADD     WSconto       D1ScontoD
349400990526  "  c                   Z-ADD     1             D1CamSco
349500990526  "  C                   MOVEL     XscDiv        D1DivSco
349600990528C0762C                   ENDIF
349700990210     C*
349800040308     C                   ELSE
349900990210     C                   EVAL      D1CamSco = 0
350000990531C0941C                   MOVEL     *blank        D1DivSco
350100040308     C                   ENDIF
350200040308     C*
350300040308     C* totale ritenute
350400040308     C*
350500040308     C                   Z-ADD     WRitenuta     D1Ritenuta
350600040308     C*
350700040308     C* totale differenze cambio
350800040308     C*
350900040308     C                   Z-ADD     WDiffcam      D1DiffCam
351000040308     C*
351100040308     C                   Endsr
351200040308     C************************************************************
351300040308     C* CONTROLLO CAMPI I/O RIGA LISTA
351400040308     C************************************************************
351500040308     C     CheckS1       Begsr
351600951017     C*
351700951017     C* reset indicatori errori/DSPATR del sfl
351800951017     C                   Move      *ALL'0'       IN6098           39
351900951017     C                   MoveA     IN6098        *IN(60)
352000951017     C*
352100951124     C* azzero quei campi output che vengono calcolati durante questa
352200951124     C* routine (cosicché, se la riga viene scartata non rimangono
352300951124     C* impostati)
352400951124     C                   Z-ADD     0             S1Ritenuta
352500951124     C                   Z-ADD     0             S1DiffCam
352600951124     C                   Z-ADD     0             S1SopravvD
352700951124     C                   Z-ADD     0             S1ImpDivP
352800990210     C                   CLEAR                   H1Sopravv
352900951124     C*
353000951108     C* SOLO PER LE RIGHE D'IMMISSIONE
353100951018     C*
353200040308     C                   IF        H1NrAsInt = 0
353300040308     C* se è stata valorizzata
353400040308     C                   IF        (S1DtPar > 0             or
353500040308     C                              S1NrPar > 0             or
353600951017     C                              S1SerPar > *blank       or
353700951027     C                              S1DtSca > 0             or
353800951115     C                              S1Divisa <> C1Divisa    or
353900951017     C                              S1ImportD > 0           or
354000980225     C                              S1Cambio > 0            or
354100980225     C                              S1Segno <> *blank       or
354200980225     C                              S1Selez <> *blank)
354300951017     C* eseguo i ctrl in visione di scrivere una nuova partita
354400951017     C*
354500960412     C* se non è stato impostato il segno partita
354600960412     C                   IF        S1Segno = *blank
354700951117     C* per prima cosa do alla nuova partita il segno
354800980116     C                   IF        Segno045 = XscDare
354900961009     C                   EVAL      S1Segno = XscAvere
355000951117     C                   ELSE
355100961009     C                   EVAL      S1Segno = XscDare
355200951117     C                   ENDIF
355300960412     C* altrimenti
355400960412     C                   ELSE
355500960412     C* lo controllo
355600961009     C                   IF        S1Segno <> XscDare and S1Segno<> XscAvere
355700960412     C                   SETON                                        6299
355800960412     C                   Eval      in(62)=*on
355900960412     C                   ENDIF
356000960412     C                   ENDIF
356100951117     C*
356200951117     C* data partita formalmente corretta
356300960201     C                   CALLB     'XDT4000'
356400950914     C                   PARM                    XDTAMG           10
356500951017     C                   PARM      S1DtPar       XDTGMA            6 0
356600950914     C                   PARM      3             XDTSTA            1 0
356700040308     C     XdtSta        IFLT      0
356800040308     C                   seton                                        6099
356900040308     C                   Eval      in(60)=*on
357000040308     C                   Endif
357100040308     C*
357200040308     C* numero partita valorizzato
357300040308     C     S1NrPar       IFLE      0
357400040308     C                   seton                                        6199
357500040308     C                   Eval      in(61)=*on
357600040308     C                   Endif
357700040308     C*
357800040308     C* se partita esistente deve essere della stessa divisa
357900040308     C     *in60         IFEQ      '0'
358000040308     C     *in61         ANDEQ     '0'
358100040308     C                   MOVE      RegSocieta    PPASocieta
358200040308     C                   MOVE      RegCtb        PPACtb
358300040308     C                   MOVE      C1Kcc         PPAKcc
358400040308     C                   MOVE      C1Ksc         PPAKsc
358500040308     C                   MOVE      XDtAMG        PPADtPar
358600040308     C                   MOVE      S1NrPar       PPANrPar
358700040308     C                   MOVE      S1SerPar      PPASerPar
358800040308     C     K07PPA03      CHAIN     NDPPA000                           21
358900040308     C     *in21         IFEQ      *off
359000040308     C     PPADivisa     ANDNE     S1Divisa
359100040308     C                   seton                                        6499
359200040308     C                   Eval      in(64)=*on
359300040308     C                   ENDIF
359400040308     C                   ENDIF
359500040308     C*
359600080111D2321C                   Eval      H1DtSca     = *Loval
359700040308     C* data scadenza formalmente corretta
359800040308     C     S1DtSca       IFNE      0
359900040308     C                   CALLB     'XDT4000'
360000040308     C                   PARM                    XDTAMG           10
360100040308     C                   PARM      S1DtSca       XDTGMA            6 0
360200040308     C                   PARM      3             XDTSTA            1 0
360300040308     C     XdtSta        IFLT      0
360400040308     C                   seton                                        6599
360500040308     C                   Eval      in(65)=*on
360600040308     C                   ENDIF
360700040308     C* se non è impostata uso come dft la data registrazione
360800040308     C                   ELSE
360900040308     C                   Z-ADD     C1DtReg       S1DtSca
361000040308     C                   ENDIF
361100080111D2321C                   CALLB     'XDT4000'
361200080111  "  C     H1DtSca       PARM                    XDTAMG           10
361300080111  "  C                   PARM      S1DtSca       XDTGMA            6 0
361400080111  "  C                   PARM      4             XDTSTA            1 0
361500080111  "  C*
361600080111  "  C                   Eval      H1DtPar     = *Loval
361700080111  "  C                   if        S1DtPar    <> 0
361800080111  "  C                   CALLB     'XDT4000'
361900080111  "  C     H1DtPar       PARM                    XDTAMG           10
362000080111  "  C                   PARM      S1DtPar       XDTGMA            6 0
362100080111  "  C                   PARM      4             XDTSTA            1 0
362200080111D2321C                   endif
362300040308     C*
362400040308     C                   ENDIF
362500040308     C*
362600040308     C                   ENDIF
362700040308     C*
362800040308     C* PER TUTTE LE RIGHE
362900040308     C*
363000040308     C* se la  divisa di partita è diversa sia dalla divisa di contabile che
363100040308     C* dall m.c.
363200040308     C     S1Divisa      IFNE      C1Divisa
363300040308     C     S1Divisa      ANDNE     XscDiv
363400040308     C* se la riga è immessa,per prima cosa recupero i dati della divisa
363500040308     C* (se la riga era presente, i dati li ho già dal caricamento)
363600040308     C* perchè mi serviranno per i ctrl dei decimali e della congruenza
363700040308     C     H1NrAsInt     IFEQ      0
363800040308     C* reperimento dati divisa inserita
363900040308     C                   IF        X0201RpDVS(S1Divisa:DtRif045:
364000040308     C                                        ANDVS00F: %size(ANDVS00F):
364100040308     C                                        ANDVX00F: %size(ANDVX00F))
364200040308     C                             <> 0
364300040308     C* al momento non dovrebbe essere possibile un errore in quanto si
364400040308     C* sarebbe già dovuto fermare sul caricamento; la chiamata alla
364500040308     C* funzione serve solo per reperire i dati della divisa
364600040308     C                   ENDIF
364700040308     C                   ENDIF
364800040308     C*
364900040308     C* se il cambio di partita è 0
365000040308     C                   IF        S1CamConc = 0
365100040308     C* richiamando la procedura con un importo fittizio essa restituisce
365200040308     C* il cambio alla data di riferimento che propongo come dafault per il
365300981014     C* cambio su partita
365400981014     C                   RESET                   PJX00202DS
365500981014     C                   MOVEL     S1Divisa      Divisa0202
365600981014     C                   MOVEL     XscDiv        DivMC0202
365700981014     C                   Z-ADD     1             ImporD0202
365800981014     C                   Z-ADD     0             ImporM0202
365900981014     C                   Z-ADD     0             Cambio0202
366000981014     C                   MOVEL     DtRif045      DtRif0202
366100981014     C* non richiedo il controllo scostamento
366200981014     C                   MOVEL     *on           NoCtrlScos
366300981014     C*
366400981014     C                   CALLP     X0202TestC(PJX00202DS :
366500981014     C                                        %size(PJX00202DS) :
366600981014     C                                        *off   : Esito   :
366700981014     C                                        NoCtrlScos : *omit)
366800981014     C                   EVAL      S1CamConc = Cambio0202
366900040308     C                   ENDIF
367000040308     C*
367100040308     C                   ELSE
367200040308     C* se la divisa di partita non è = alla contabile
367300040308     C     S1Divisa      IFNE      C1Divisa
367400040308     C* ma è = alla m.c.
367500040308     C     S1Divisa      ANDEQ     XscDiv
367600040308     C* se vuoto, imposto il cambio di partita
367700040308     C     S1CamConc     ANDEQ     0
367800040308     C* imposto a 1 il cambio di partita
367900040308     C                   EVAL      S1CamConc = 1
368000040308     C                   ENDIF
368100040308     C                   ENDIF
368200040308     C*
368300040308     C* se gestione a cambio storico
368400040308     C                   IF        CmbSto045 = *on         and
368500951122     C* e la divisa di partita non è = alla contabile
368600951218     C                             S1Divisa <> C1Divisa
368700951122     C* forzo comunque il cambio su divisa = cambio di partita
368800951122     C                   Z-ADD     S1Cambio      S1CamConc
368900040308     C                   ENDIF
369000951027     C*
369100951027     C* azzero i valori da calcolare dinamicamente
369200951027     C                   Z-ADD     0             H1Importo
369300951027     C                   Z-ADD     0             H1Abbuono
369400951027     C                   Z-ADD     0             H1Sconto
369500951027     C                   Z-ADD     0             S1DiffCam
369600951018     C*
369700951030     C* eseguo i ragionamenti sugli importi delle righe se :
369800951006     C*
369900951027     C* il flag di selezione è valorizzato
370000040308     C     S1Selez       IFNE      *blank
370100980224     C*
370200951115     C* e non ho trovato errori nei controlli dei campi di partita
370300980224     C* (gestione righe d'immissione)
370400951027     C     *in99         ANDEQ     *off
370500951025     C*
370600951108     C* GESTIONE SCONTI DI TESTATA
370700951108     C*
370800951108     C* se la riga proviene da database
370900040308C0762X***                IF        H1NrAsInt > 0         and
371000990526  "  C                   IF        H1NrAsInt > 0
371100040308  "  X**e la divisa è la stessa di testata
371200040308C0762X***                          S1Divisa = C1Divisa
371300951027     C*
371400951110     C* calcolo lo sconto in automatico se presente almeno una percentuale
371500040308     C* di testata
371600040308     C                   IF        ( C1PercSco1 > 0   or
371700040308     C                               C1PercSco2 > 0 )          and
371800040308     C* e se l'importo in conto non è valorizzato
371900040308     C                             S1ImportD = 0               and
372000040308     C* e se lo sconto su riga non è valorizzato
372100040308     C*   oppure se richiesta forzatura su tutte le righe
372200040308     C                             ( S1ScontoD = 0    or
372300040308     C                               C1ForzaSco = Xscon )
372400040308     C                   EXSR      CalcSconto
372500040308     C                   ENDIF
372600040308     C                   ENDIF
372700040308     C*
372800040308     C* VALORIZZAZIONE DEGLI IMPORTI
372900040308     C*
373000040308     C* se la divisa di partita è la stessa della contabile
373100040308     C     S1Divisa      IFEQ      C1Divisa
373200040308     C*
373300040308     C* posso impostare l'importo in conto o l'abbuono in automatico
373400040308     C* a seconda della selezione
373500040308     C                   EXSR      AutoSel1
373600040308     C*
373700040308     C* ctrl validità segno abbuono (se abbuono digitato)
373800040308     C                   IF        S1SegAbb <> 'P' and S1SegAbb <> 'A'  and
373900040308     C                             S1AbbuonoD <> 0                       and
374000040308     C                             H1NrAsInt <> 0
374100040308     C                   SETON                                        6799
374200040308     C                   Eval      in(67)=*on
374300040308     C                   ENDIF
374400040308     C*
374500040308     C* ctrl validità forzatura abbuono
374600040308     C                   IF        S1FrcAbb <> XscOn       and
374700040308     C                             S1FrcAbb <> XscOff      and
374800040308     C                             S1AbbuonoD <> 0         and
374900040308     C                             H1NrAsInt <> 0
375000040308     C                   SETON                                        6999
375100040308     C                   Eval      in(69)=*on
375200040308     C                   ENDIF
375300040308     C*
375400040308     C* se la divisa di partita è diversa da quella di contabile
375500040308     C                   ELSE
375600040308     C*
375700040308     C* posso impostare l'importo concordato in automatico
375800040308     C* a seconda della selezione
375900040308     C                   EXSR      AutoSel2
376000040308     C*
376100040308     C* AUTOMATISMI DI CALCOLO IMPORTO DA ALTRO IMPORTO SE RICHIESTI
376200040308     C* ( da importo in conto a importo concordato o viceversa)
376300040308     C                   IF        ClcConc0IP = *on  or
376400040308     C* la richiesta di questi automatismi può avvenire anche da pgm
376500040308     C* qualora le divise abbiano tra loro un rapporto solo di cambi fissi
376600040308     C                             H1Triang = '3' or H1Triang = '4'
376700040308     C*
376800040308     C* se è stato digitato solo l'importo concordato
376900040308     C                   IF        S1ImpConc > 0 and S1ImportD = 0
377000040308     C* calcolo automaticamente il mancante
377100040308     C                   EXSR      RieImportD
377200040308     C                   ENDIF
377300980226     C* se è stato digitato solo l'importo in conto
377400040308     C                   IF        S1ImpConc = 0 and S1ImportD > 0
377500980226     C* calcolo automaticamente il mancante
377600040308     C                   EXSR      RieImpConc
377700001201D0748C*
377800001201  "  C* se a questo punto ho anche l'importo concordato
377900001201  "  C                   IF        S1ImpConc > 0 and
378000001201  "  C* e la selezione prevedeva un abbuono
378100001201  "  C                             S1Selez = 'A' and
378200001201  "  C* e non si era riusciti a calcolarlo per mancanza dell'importo
378300001201  "  C* concordato
378400001201  "  C                             AutoSel2x2 = *on
378500001201  "  C* rieseguo la routine di automatismo
378600001201  "  C                   EXSR      AutoSel2
378700001201  "  C                   ENDIF
378800001201D0748C*
378900040308     C                   ENDIF
379000040308     C*
379100040308     C                   ENDIF
379200040308     C*
379300040308     C* infine testo se manca anche uno solo di questi dati:
379400040308     C*  importo in conto e importo concordato
379500040308     C                   IF        S1ImpConc = 0      or
379600040308     C                             S1ImportD = 0
379700040308     C                   IF        S1ImpConc = 0
379800040308     C                   SETON                                        6699
379900040308     C                   Eval      in(66)=*on
380000040308     C                   ELSE
380100040308     C                   SETON                                        6399
380200040308     C                   Eval      in(63)=*on
380300040308     C                   ENDIF
380400040308     C*
380500040308     C                   ELSE
380600040308     C*
380700040308     C* ctrl validità forzatura importo in divisa di partita
380800040308     C                   IF        S1FrcImpDP <> XscOn       and
380900040308     C                             S1FrcImpDP <> XscOff      and
381000040308     C                             S1ImpConc  <> 0         and
381100040308     C                             H1NrAsInt <> 0
381200040308     C                   SETON                                        7099
381300040308     C                   Eval      in(70)=*on
381400040308     C                   ENDIF
381500040308     C*
381600040308     C* calcolo l'importo in divisa di partita
381700040308     C*
381800040308     C* se non è stata richiesta la forzatura lo calcolo
381900040308     C* trasformando l'importo digitato (in divisa di testata) nel valore
382000040308     C* in moneta di conto
382100040308     C                   IF        S1FrcImpDP <> XscOn
382200040308     C* calcolo l'importo in divisa di partita
382300040308     C                   EXSR      ClcImpDivP
382400040308     C*
382500040308     C                   Z-ADD     ImporD0202    S1ImpDivP
382600040308     C*
382700040308     C* se è stata richiesta la forzatura
382800040308     C                   ELSE
382900040308     C* allora pongo l'importo calcolato = a quello concordato
383000040308     C                   Z-ADD     S1ImpConc     S1ImpDivP
383100040308     C                   ENDIF
383200040308     C*
383300040308     C* determino l'esistenza di sopravvenienza
383400040308     C                   EVAL      S1SopravvD = S1ImpDivP - S1ImpConc
383500040308     C*
383600040308     C                   ENDIF
383700040308     C*
383800040308     C                   ENDIF
383900040308     C*
384000040308     C* solo per le righe provenienti da database
384100040308     C                   IF        H1NrAsInt > 0
384200040308     C*
384300040308     C* calcolo l'importo che influirà sulla partita (in divisa di partita)
384400040308     C* in modo da sapere se la partita è a saldo oppure in acconto
384500990617     C*
384600990617C0941C* se la divisa di partita è la stessa della contabile
384700990617  "  C     S1Divisa      IFEQ      C1Divisa
384800990617  "  C* parto dall'importo in conto
384900990617  "  C                   EVAL      ImpDivPar = S1ImportD
385000990617  "  C* se la divisa di partita è diversa della contabile
385100990617  "  C                   ELSE
385200990617  "  C* parto dall'importo concordato
385300990617  "  C                   EVAL      ImpDivPar = S1ImpConc
385400990617  "  C                   ENDIF
385500990617  "  C* poi sommo lo sconto
385600990617  "  C                   EVAL      ImpDivPar = ImpDivPar + S1ScontoD
385700990617  "  C* se l'abbuono è attivo e il segno partita è dare
385800990617  "  C* (incasso di +  nel caso di saldaconto cliente   oppure
385900990617  "  C*  ricevo indietro di +  nel caso di saldaconto fornitore)
386000990617  "  C                   IF        S1SegAbb = 'A'  and  S1Segno = XscDare or
386100990617  "  C* oppure l'abbuono è passivo e il segno partita è avere
386200990617  "  C* (rendo di +  nel caso di saldaconto cliente   oppure
386300990617  "  C*  pago di +  nel caso di saldaconto fornitore)
386400990617  "  C                             S1SegAbb = 'P'  and  S1Segno = XscAvere
386500990617  "  C*  l'importo è > del (residuo-sconto) per cui l'abbuono va sottratto
386600990617  "  C                   EVAL      AbbuonoD = S1AbbuonoD
386700990617  "  C* viceversa
386800990617  "  C                   ELSE
386900990617  "  C*  l'importo è < del (residuo-sconto) per cui l'abbuono va sommato
387000990617  "  C                   EVAL      AbbuonoD = S1AbbuonoD * -1
387100990617  "  C                   ENDIF
387200990617  "  C*
387300990617  "  C                   EVAL      ImpDivPar = ImpDivPar - AbbuonoD
387400990617  "  C*
387500990617C0941C                   ENDIF
387600951027     C*
387700951113     C* controllo i decimali degli importi in input
387800980923     C                   EXSR      CtrlNrDec
387900951113     C*
388000951113     C* calcolo i vari controvalori
388100951113     C                   EXSR      CalcCV
388200951117     C*
388300951117     C* solo per le righe provenienti da database
388400040308     C                   IF        H1NrAsInt > 0
388500040308     C*
388600040308     C* calcolo la differenza cambi
388700040308     C                   EXSR      CalcDifCam
388800040308     C*
388900040308     C* se la divisa di partita è <> da quella di testata
389000040308     C                   IF        S1Divisa <> C1Divisa
389100040308     C* testo che l'importo concordato + l'eventuale sconto risultino
389200040308     C* accettabili rispetto l'importo in conto
389300040308     C* (per fare ciò controllo che il valore in m.c. dell'importo in conto
389400040308     C*  sia accettabile rispetto l'importo totale in divisa di partita)
389500040308     C* NON è un errore bloccante ma solo una segnalazione
389600040308     C                   EXSR      TstDivDive
389700040308     C                   ENDIF
389800040308     C*
389900040308     C* se la partita è a chiudere
390000040308     C                   IF        ImpDivPar = S1ResiduoD         and
390100040308     C* testo se anche l'importo in m.c. è a chiudere oppure esiste
390200040308     C* una differenza cambio (se la divisa di partita non è la m.c.)
390300040308     C                             S1Divisa <> XscDiv
390400040308     C                   EXSR      TstParChiu
390500040308     C                   ENDIF
390600040308     C*
390700040308     C                   ENDIF
390800040308     C*
390900040308     C* se il conto è un percipiente e ha una ritenuta da gestire
391000040308D0254X***                IF        H1RitCalc <> 0   and
391100991008  "  C                   IF        (H1RitCalc <> 0 or
391200991008D0254C                              H1RitAcc0 = *on) and
391300991008     C                             *in99 = *off     and
391400970819     C* e la registrazione non è provvisoria
391500980223     C                             REGTpRegz <> 'P' and
391600980223     C* e il residuo in divisa è <> 0
391700980223     C                             S1ResiduoD <> 0
391800951120     C* calcolo la ritenuta sull'importo da contabilizzare
391900951120     C                   EXSR      CalcRitAcc
392000951120     C                   ENDIF
392100951024     C*
392200951120     C* VALORIZZO I TOTALI
392300951120     C*
392400990528C0941C* totale importi in conto
392500040308     C                   IF        SOP0IP = 'D' and s1Segno <> XscDare or
392600040308     C                             SOP0IP = 'A' and s1Segno <> XscAvere
392700951030     C                   ADD       S1ImportD     WImportD
392800951030     C                   ADD       H1Importo     WImporto
392900990528C0941C*
393000990528C0941C* totale sconti
393100990528C0762C                   IF        DivDiveSco = *off
393200951031     C                   ADD       S1ScontoD     WScontoD
393300951031     C                   ADD       H1Sconto      WSconto
393400990528C0762C                   ELSE
393500990526  "  C                   ADD       H1Sconto      WSconto
393600990526  "  C                   Z-ADD     0             WScontoD
393700990528C0762C                   ENDIF
393800040308     C                   ELSE
393900951031     C                   SUB       S1ImportD     WImportD
394000951031     C                   SUB       H1Importo     WImporto
394100990528C0762C                   IF        DivDiveSco = *off
394200951031     C                   SUB       S1ScontoD     WScontoD
394300951031     C                   SUB       H1Sconto      WSconto
394400990528C0762C                   ELSE
394500990526  "  C                   SUB       H1Sconto      WSconto
394600990526  "  C                   Z-ADD     0             WScontoD
394700990528C0762C                   ENDIF
394800040308     C                   ENDIF
394900990528C0941C*
395000990601  "  C* totale abbuoni
395100990601  "  C*
395200990601  "  C* se le divise dell'abbuono sono le stesse
395300990528C0941C                   IF        DivDiveAbb = *off
395400040308     C                   IF        S1SegAbb = 'A'
395500951027     C                   ADD       S1AbbuonoD    WAbbuonoD
395600951030     C                   ADD       H1Abbuono     WAbbuono
395700951108     C                   EVAL      AbbAttivo = AbbAttivo + H1Abbuono
395800960115     C                   EVAL      AbbAttivoD = AbbAttivoD + S1AbbuonoD
395900040308     C                   ELSE
396000951031     C                   SUB       S1AbbuonoD    WAbbuonoD
396100951031     C                   SUB       H1Abbuono     WAbbuono
396200951108     C                   EVAL      AbbPassivo = AbbPassivo + H1Abbuono
396300960115     C                   EVAL      AbbPassivD = AbbPassivD + S1AbbuonoD
396400040308     C                   ENDIF
396500990601C0941C* se le divise dell'abbuono sono diverse
396600990601  "  C                   ELSE
396700990526  "  C                   IF        S1SegAbb = 'A'
396800990526  "  C                   ADD       H1Abbuono     WAbbuono
396900990526  "  C                   EVAL      AbbAttivo = AbbAttivo + H1Abbuono
397000990601  "  C* l'abbuono attivo in divisa diventa = a quello in m.c.
397100990601  "  C                   EVAL      AbbAttivoD = AbbAttivo
397200990526  "  C                   ELSE
397300990526  "  C                   SUB       H1Abbuono     WAbbuono
397400990526  "  C                   EVAL      AbbPassivo = AbbPassivo + H1Abbuono
397500990601  "  C* l'abbuono passivo in divisa diventa = a quello in m.c.
397600990601  "  C                   EVAL      AbbPassivD = AbbPassivo
397700990526  "  C                   ENDIF
397800990526  "  C                   Z-ADD     0             WAbbuonoD
397900990528C0941C                   ENDIF
398000951031     C*
398100990528C0941C* totale differenze cambio
398200960308     C                   SELECT
398300961009     C                   WHEN      S1Diffcam > 0 and S1Segno = XscDare
398400960308     C                   ADD       S1Diffcam     WDiffCam
398500960308     C                   EVAL      DiffCamAtt = DiffCamAtt + S1DiffCam
398600961009     C                   WHEN      S1Diffcam < 0 and S1Segno = XscAvere
398700960308     C                   sub       S1Diffcam     WDiffCam
398800960308     C                   EVAL      DiffCamAtt = DiffCamAtt - S1DiffCam
398900961009     C                   WHEN      S1Diffcam > 0 and S1Segno = XscAvere
399000960308     C                   EVAL      DiffCamPas = DiffCamPas + S1DiffCam
399100960308     C                   sub       S1Diffcam     WDiffCam
399200961009     C                   WHEN      S1Diffcam < 0 and S1Segno = XscDare
399300960308     C                   EVAL      DiffCamPas = DiffCamPas - S1DiffCam
399400960308     C                   add       S1Diffcam     WDiffCam
399500960308     C                   ENDSL
399600951031     C*
399700990528C0941C* totale ritenute d'acconto
399800951027     C                   ADD       S1Ritenuta    WRitenuta
399900010226C1458C*
400000010226  "  C* se la riga ha una rata bloccata ma la gestione è per partite
400100010226  "  C                   IF        H1RataBk = *on
400200010226  "  C* devo avvertire di ciò l'utente
400300010226  "  C                   EXSR      MsgRataBk
400400010226C1458C                   ENDIF
400500951018     C*
400600951218     C* se la riga non è da prendere in considerazione
400700040308     C                   ELSE
400800951218     C* cancello tutti gli importi a video
400900951218     C                   Z-ADD     0             S1ImportD
401000951218     C                   Z-ADD     0             S1AbbuonoD
401100951218     C                   Z-ADD     0             S1ScontoD
401200951218     C                   Z-ADD     0             S1Ritenuta
401300951218     C                   Z-ADD     0             S1DiffCam
401400951218     C                   Z-ADD     0             S1CamConc
401500951218     C                   Z-ADD     0             S1ImpConc
401600951218     C                   Z-ADD     0             S1SopravvD
401700951218     C                   Z-ADD     0             S1ImpDivP
401800951218     C                   Z-ADD     0             H1Importo
401900951218     C                   Z-ADD     0             H1Abbuono
402000951218     C                   Z-ADD     0             H1Sconto
402100951218     C                   Z-ADD     0             H1Sopravv
402200040308     C                   ENDIF
402300040308     C*
402400040308     C* se questa è la prima riga in errore
402500040308     C                   IF        ErrSfl = 0 and *in99 = *on
402600040308     C* imposto il sflrcdnbr in modo che il cursore ci si posizioni sopra
402700040308     C                   EVAL      C1Rcd = S1Nrr
402800040308     C                   EVAL      ErrSfl = 1
402900040308     C                   ENDIF
403000040308     C*
403100040308     C* se è attivo almeno un errore nel drop richiedo l'apertura
403200040308     C                   IF        *in69=*on or *in68=*on or *in75=*on or
403300040308     C                             *in67=*on or *in76=*on or *in66=*on or
403400040308     C                             *in77=*on or *in78=*on or *in71=*on
403500040308     C                   EVAL      C1Mode = *off
403600040308     C                   ENDIF
403700040308     C*
403800040308     C                   ENDSR
403900980226     C************************************************************
404000980226     C* Automatismi per partite con divisa di partita = divisa testata
404100980226     C************************************************************
404200980226     C     AutoSel1      BEGSR
404300980226     C*
404400980226     C* posso impostare l'importo in conto o l'abbuono in automatico
404500980226     C* a seconda della selezione
404600040308     C                   SELECT
404700980226     C*
404800980226     C* se l'opzione è 'T' = tutto l'importo rimanente
404900980226     C                   WHEN      S1Selez = 'T'
405000980226     C* se l'importo in conto è vuoto
405100040308     C                   IF        S1ImportD = 0
405200980226     C* lo valorizzo con tutto l'importo di testata - ciò che è già stato
405300980226     C* impostato, controllando che sia possibile calcolare una rimanenza
405400980226     C* non negativa a seconda del segno di partita
405500040308     C                   SELECT
405600980226     C                   WHEN      S1Segno <> Segno045 and
405700980226     C                             C1ImportD >= WImportD
405800980226     C     C1ImportD     SUB       WImportD      S1ImportD
405900980226     C* se la riga è valorizzata, imposto il flag di selezione in X
406000980226     C                   EVAL      S1Selez = 'X'
406100980226     C                   WHEN      S1Segno = Segno045 and
406200980226     C                             C1ImportD < WImportD
406300980226     C     WImportD      SUB       C1ImportD     S1ImportD
406400980226     C* se la riga è valorizzata, imposto il flag di selezione in X
406500980226     C                   EVAL      S1Selez = 'X'
406600040308     C                   ENDSL
406700040308     C                   ENDIF
406800980226     C*
406900980226     C* se l'opzione è 'X' = importo automatico
407000980226     C                   WHEN      S1Selez = 'X'        and
407100980226     C* e la riga non è d'immissione
407200040308     C                             H1NrAsInt > 0        and
407300980226     C* e l'importo in conto è vuoto
407400980226     C                             S1ImportD = 0
407500980226     C* lo valorizzo al (residuo - abbuono - sconto)
407600980226     C* se l'abbuono è attivo e il segno partita è dare
407700990617D0122C* (incasso di +  nel caso di saldaconto cliente   oppure
407800990617D0122C*  ricevo indietro di +  nel caso di saldaconto fornitore)
407900040308     C                   IF        S1SegAbb = 'A'  and  S1Segno = XscDare or
408000980226     C* oppure l'abbuono è passivo e il segno partita è avere
408100990617D0122C* (rendo di +  nel caso di saldaconto cliente   oppure
408200990617D0122C*  pago di +  nel caso di saldaconto fornitore)
408300990617     C                             S1SegAbb = 'P'  and  S1Segno = XscAvere
408400980226     C*  l'importo è > del (residuo-sconto) per cui l'abbuono va sommato
408500980226     C                   EVAL      AbbuonoD = S1AbbuonoD * -1
408600980226     C* viceversa
408700040308     C                   ELSE
408800980226     C*  l'importo è < del (residuo-sconto) per cui l'abbuono va sottrato
408900980226     C                   EVAL      AbbuonoD = S1AbbuonoD
409000040308     C                   ENDIF
409100980226     C*
409200980226     C                   EVAL      S1ImportD = S1ResiduoD - S1ScontoD -
409300980226     C                                         AbbuonoD
409400980226     C*
409500980226     C* se l'opzione è 'A' = abbuono automatico
409600980226     C                   WHEN      S1Selez = 'A'        and
409700980226     C* e la riga non è d'immissione
409800040308     C                             H1NrAsInt > 0        and
409900980226     C* e l'importo in conto è vuoto
410000980226     C                             S1ImportD = 0        and
410100980226     C* e l'abbuono è valorizzato
410200980226     C                             S1AbbuonoD > 0
410300980226     C* valorizzo l'importo in conto al (residuo - abbuono - sconto)
410400980226     C* se l'abbuono è attivo e il segno partita è dare
410500990617D0122C* (incasso di +  nel caso di saldaconto cliente   oppure
410600990617D0122C*  ricevo indietro di +  nel caso di saldaconto fornitore)
410700040308     C                   IF        S1SegAbb = 'A'  and  S1Segno = XscDare or
410800980226     C* oppure l'abbuono è passivo e il segno partita è avere
410900990617D0122C* (rendo di +  nel caso di saldaconto cliente   oppure
411000990617D0122C*  pago di +  nel caso di saldaconto fornitore)
411100990617     C                             S1SegAbb = 'P'  and  S1Segno = XscAvere
411200980226     C*  l'importo è > del (residuo-sconto) per cui l'abbuono va sommato
411300980226     C                   EVAL      AbbuonoD = S1AbbuonoD * -1
411400980226     C* viceversa
411500040308     C                   ELSE
411600980226     C*  l'importo è < del (residuo-sconto) per cui l'abbuono va sottrato
411700980226     C                   EVAL      AbbuonoD = S1AbbuonoD
411800040308     C                   ENDIF
411900980226     C*
412000980226     C                   EVAL      S1ImportD = S1ResiduoD - S1ScontoD -
412100980226     C                                         AbbuonoD
412200980226     C*
412300980226     C* se l'opzione è 'A' = abbuono automatico
412400980226     C                   WHEN      S1Selez = 'A'        and
412500980226     C* e la riga non è d'immissione
412600040308     C                             H1NrAsInt > 0        and
412700980226     C* e l'importo in conto è valorizzato
412800980226     C                             S1ImportD > 0        and
412900980226     C* e l'abbuono è vuoto
413000980226     C                             S1AbbuonoD = 0
413100980226     C* valorizzo l'abbuono (residuo - importo - sconto)
413200980226     C                   EVAL      S1AbbuonoD = S1ResiduoD - S1ScontoD -
413300980226     C                                          S1ImportD
413400980226     C* poi determino il segno dell'abbuono
413500990617D0122C* 1. incasso di + da cliente
413600990617D0122C* 2. ricevo di + da fornitore a fronte di nota
413700040308     C                   IF        S1ImportD > (S1ResiduoD - S1ScontoD) and
413800980226     C                             S1Segno = XscDare           or
413900990617D0122C* 3. rendo di - a cliente a fronte di nota
414000990617D0122C* 4. pago di - a fornitore
414100980226     C                             S1ImportD < (S1ResiduoD - S1ScontoD) and
414200980226     C                             S1Segno = XscAvere
414300040308     C                   EVAL      S1SegAbb = 'A'
414400040308     C                   ELSE
414500040308     C                   EVAL      S1SegAbb = 'P'
414600040308     C                   ENDIF
414700040308     C* lo imposto al valore assoluto
414800040308     C                   IF        S1AbbuonoD < 0
414900040308     C                   EVAL      S1AbbuonoD = S1AbbuonoD * -1
415000040308     C                   ENDIF
415100040308     C*
415200040308     C* se l'opzione è 'A' = abbuono automatico
415300040308     C                   WHEN      S1Selez = 'A'        and
415400040308     C* e l'importo in conto è vuoto
415500040308     C                             S1ImportD = 0        and
415600040308     C* e l'abbuono è vuoto
415700040308     C                             S1AbbuonoD = 0
415800040308     C* do errore campo obbligatorio sull'importo in conto
415900040308     C                   SETON                                        6399
416000040308     C                   Eval      in(63)=*on
416100040308     C*
416200040308     C                   OTHER
416300040308     C* se l'importo in conto non è stato digitato
416400040308     C                   IF        S1ImportD = 0
416500040308     C* do errore
416600040308     C                   SETON                                        6399
416700040308     C                   Eval      in(63)=*on
416800040308     C                   ENDIF
416900040308     C*
417000040308     C                   ENDSL
417100040308     C*
417200040308     C                   ENDSR
417300040308     C************************************************************
417400040308     C* Automatismi per partite con divisa di partita <> divisa testata
417500040308     C************************************************************
417600040308     C     AutoSel2      BEGSR
417700980226     C*
417800001201D0748C* azzero la richiesta di richiamo a questa routine per 2 volte
417900001201  "  C                   EVAL      AutoSel2x2 = *off
418000001201D0748C*
418100980226     C* posso impostare l'importo in conto o l'abbuono in automatico
418200980226     C* a seconda della selezione
418300040308     C                   SELECT
418400980226     C*
418500980226     C* se l'opzione è 'T' = tutto l'importo rimanente
418600980226     C                   WHEN      S1Selez = 'T'
418700980226     C* se l'importo in conto è vuoto
418800040308     C                   IF        S1ImportD = 0
418900980226     C* lo valorizzo con tutto l'importo di testata - ciò che è già stato
419000980226     C* impostato, controllando che sia possibile calcolare una rimanenza
419100980226     C* non negativa a seconda del segno di partita
419200040308     C                   SELECT
419300980226     C                   WHEN      S1Segno <> Segno045 and
419400980226     C                             C1ImportD >= WImportD
419500980226     C     C1ImportD     SUB       WImportD      S1ImportD
419600980226     C* se la riga è valorizzata, imposto il flag di selezione in X
419700980226     C                   EVAL      S1Selez = 'X'
419800980226     C                   WHEN      S1Segno = Segno045 and
419900980226     C                             C1ImportD < WImportD
420000980226     C     WImportD      SUB       C1ImportD     S1ImportD
420100980226     C* se la riga è valorizzata, imposto il flag di selezione in X
420200040308     C                   EVAL      S1Selez = 'X'
420300040308     C                   ENDSL
420400040308     C                   ENDIF
420500980226     C*
420600980226     C* se l'opzione è 'X' = importo automatico
420700980226     C                   WHEN      S1Selez = 'X'        and
420800980226     C* e l'importo in conto è vuoto
420900980226     C                             S1ImportD = 0        and
421000980226     C* e l'importo concordato è vuoto
421100980226     C                             S1ImpConc = 0
421200990528C0762C* lo valorizzo al residuo - abbuono e sconto
421300990528C0762C                   EVAL      S1ImpConc = S1ResiduoD - S1ScontoD
421400990617C0941C* se l'abbuono è attivo e il segno partita è dare
421500990617  "  C* (incasso di +  nel caso di saldaconto cliente   oppure
421600990617  "  C*  ricevo indietro di +  nel caso di saldaconto fornitore)
421700990617  "  C                   IF        S1SegAbb = 'A'  and  S1Segno = XscDare or
421800990617  "  C* oppure l'abbuono è passivo e il segno partita è avere
421900990617  "  C* (rendo di +  nel caso di saldaconto cliente   oppure
422000990617  "  C*  pago di +  nel caso di saldaconto fornitore)
422100990617  "  C                             S1SegAbb = 'P'  and  S1Segno = XscAvere
422200990617  "  C*  l'importo è > del (residuo-sconto) per cui l'abbuono va sommato
422300990617  "  C                   EVAL      S1ImpConc = S1ImpConc + S1AbbuonoD
422400990617  "  C* viceversa
422500990617  "  C                   ELSE
422600990617  "  C*  l'importo è < del (residuo-sconto) per cui l'abbuono va sottrato
422700990617  "  C                   EVAL      S1ImpConc = S1ImpConc - S1AbbuonoD
422800990617  "  C                   ENDIF
422900990602  "  C
423000990602  "  C* se l'opzione è 'A' = abbuono automatico
423100990602  "  C                   WHEN      S1Selez = 'A'        and
423200990602  "  C* e l'importo in conto è vuoto
423300990602  "  C                             S1ImportD = 0        and
423400990602  "  C* e l'importo concordato è vuoto
423500990602  "  C                             S1ImpConc = 0        and
423600990602  "  C* e l'abbuono è valorizzato
423700990602  "  C                             S1AbbuonoD > 0
423800990602  "  C* valorizzo l'importo concordato al (residuo - abbuono - sconto)
423900990602  "  C* se l'abbuono è attivo e il segno partita è dare
424000990617  "  C* (incasso di +  nel caso di saldaconto cliente   oppure
424100990617  "  C*  ricevo indietro di +  nel caso di saldaconto fornitore)
424200990602  "  C                   IF        S1SegAbb = 'A'  and  S1Segno = XscDare or
424300990602  "  C* oppure l'abbuono è passivo e il segno partita è avere
424400990617  "  C* (rendo di +  nel caso di saldaconto cliente   oppure
424500990617  "  C*  pago di +  nel caso di saldaconto fornitore)
424600990617  "  C                             S1SegAbb = 'P'  and  S1Segno = XscAvere
424700990602  "  C*  l'importo è > del (residuo-sconto) per cui l'abbuono va sommato
424800990602  "  C                   EVAL      AbbuonoD = S1AbbuonoD * -1
424900990602  "  C* viceversa
425000990602  "  C                   ELSE
425100990602  "  C*  l'importo è < del (residuo-sconto) per cui l'abbuono va sottrato
425200990602  "  C                   EVAL      AbbuonoD = S1AbbuonoD
425300990602  "  C                   ENDIF
425400990602  "  C*
425500990602  "  C                   EVAL      S1ImpConc = S1ResiduoD - S1ScontoD -
425600990602  "  C                                         AbbuonoD
425700990602  "  C*
425800990602  "  C* se l'opzione è 'A' = abbuono automatico
425900990602  "  C                   WHEN      S1Selez = 'A'        and
426000990602  "  C* e l'importo concordato è valorizzato
426100990602  "  C                             S1ImpConc > 0        and
426200990602  "  C* e l'abbuono è vuoto
426300990602  "  C                             S1AbbuonoD = 0
426400990602  "  C* valorizzo l'abbuono (residuo - importo - sconto)
426500990602  "  C                   EVAL      S1AbbuonoD = S1ResiduoD - S1ScontoD -
426600990602  "  C                                          S1ImpConc
426700990602  "  C* poi determino il segno dell'abbuono
426800990615  "  C                   IF        S1ImpConc > (S1ResiduoD - S1ScontoD) and
426900990602  "  C                             S1Segno = XscDare           or
427000990615  "  C                             S1ImpConc < (S1ResiduoD - S1ScontoD) and
427100990602  "  C                             S1Segno = XscAvere
427200990602  "  C                   EVAL      S1SegAbb = 'A'
427300990602  "  C                   ELSE
427400990602  "  C                   EVAL      S1SegAbb = 'P'
427500990602  "  C                   ENDIF
427600990602  "  C* lo imposto al valore assoluto
427700990602  "  C                   IF        S1AbbuonoD < 0
427800990602  "  C                   EVAL      S1AbbuonoD = S1AbbuonoD * -1
427900990602  "  C                   ENDIF
428000001201C0941C
428100001201D0748C* se l'opzione è 'A' = abbuono automatico
428200001201  "  C                   WHEN      S1Selez = 'A'        and
428300001201  "  C* e l'importo in conto è valorizzato
428400001201  "  C                             S1ImportD > 0        and
428500001201  "  C* e l'importo concordato è vuoto
428600001201  "  C                             S1ImpConc = 0        and
428700001201  "  C* e l'abbuono è vuoto
428800001201  "  C                             S1AbbuonoD = 0
428900001201  "  C* non posso calcolare l'abbuono perchè, essendo in divisa di
429000001201  "  C* partita, ho bisogno che sia valorizzato l'importo concordato;
429100001201  "  C* per cui mi memorizzo di richiamare nuovamente questa routine
429200001201  "  C* qualora esistano le condizioni per un calcolo automatico dell'
429300001201  "  C* importo concordato da quello in conto, calcolo che viene fatto re or
429400001201  "  C* dalla routine successiva
429500001201  "  C                   EVAL      AutoSel2x2 = *on
429600001201D0748C*
429700001201C0941C*
429800990602  "  C* se l'opzione è 'A' = abbuono automatico
429900990602  "  C                   WHEN      S1Selez = 'A'        and
430000990602  "  C* e l'importo in conto è vuoto
430100990602  "  C                             S1ImportD = 0        and
430200990602  "  C* e l'importo concordato è vuoto
430300990602  "  C                             S1ImpConc = 0        and
430400990602  "  C* e l'abbuono è vuoto
430500990602  "  C                             S1AbbuonoD = 0
430600990602  "  C* do errore campo obbligatorio sull'importo in conto
430700990602  "  C                   SETON                                        6399
430800990602C0941C                   Eval      in(63)=*on
430900980226     C*
431000040308     C                   OTHER
431100980226     C*
431200040308     C                   ENDSL
431300980226     C*
431400980226     C                   ENDSR
431500951030     C************************************************************
431600980226     C* Riempimento automatico dell'importo in conto partendo
431700980226     C* dall'importo concordato
431800951030     C************************************************************
431900980226     C     RieImportD    BEGSR
432000980226     C*
432100981007     C* partendo dall'importo concordato, calcolo prima il suo controvalore
432200981007     C* in m.c. mediante il cambio concordato, poi, se la divisa di
432300981007     C* testata è diversa dalla m.c., passo dall'importo calcolato a
432400981007     C* quello in divisa di contabile mediante il cambio di testata
432500980227     C* (per fare questo calcolo uso la routine di controllo congruenza
432600980227     C*  importi-cambio con aggiornamento dell'importo mancante : questo
432700980227     C*  perchè il cambio è un campo i/o e quindi suscettibile di
432800980227     C*  errori)
432900980227     C* questa routine ha senso solo per le partite che hanno divisa diversa
433000980227     C* da quella contabile; queste partite non sono gestite a cambio
433100980227     C* storico
433200980923     C                   RESET                   PJX00202DS
433300980923     C                   MOVEL     S1Divisa      Divisa0202
433400980923     C                   MOVEL     XscDiv        DivMC0202
433500980923     C                   Z-ADD     S1ImpConc     ImporD0202
433600980923     C                   Z-ADD     0             ImporM0202
433700980923     C                   Z-ADD     S1CamConc     Cambio0202
433800980923     C                   MOVEL     DtRif045      DtRif0202
433900980923     C*
434000980923     C                   CALLP     X0202TestC(PJX00202DS :
434100980923     C                                        %size(PJX00202DS) :
434200980923     C                                        *on    : Esito   :
434300980923     C                                        *omit  : *omit)
434400980923     C* se errori
434500040308     C                   IF        Esito <> '0'
434600980923     C* gestisco la loro emissione
434700980925     c                   clear                   C1TextMsg
434800980923     C                   DOU       Q2BytAva = 0
434900980923     C                   EXSR      RestoreMsg
435000980923     C                   IF        Q2BytAva > 0
435100980923     C                   SELECT
435200980923     C                   WHEN      Q2MsgId = 'MVC0067'
435300980923     C                   SETON                                        7799
435400980923     C                   Eval      in(77) = *on
435500980923     C                   WHEN      Q2MsgId = 'MVC0065'
435600980923     C                   SETON                                        7799
435700980923     C                   Eval      in(77) = *on
435800980923     C                   OTHER
435900980923     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
436000980923     C                   MOVE      Q2MsgId       IdMsg
436100980923     C                   CALLB     'XRTVM'
436200980923     C                   PARM                    IDMSG            27
436300980923     C                   PARM                    TXTMSG          100
436400980923     C                   PARM                    ERRMSG            1
436500980923     C                   PARM                    *omit
436600980923     C                   PARM                    *omit
436700980923     C                   PARM      Q2msgtxt      Msgdta          100
436800980923     C                   IF        ErrMsg <> *on
436900980925     C                   CAT       TxtMsg:1      C1TextMsg
437000980925     C                   SETON                                        9699
437100981006     C                   EVAL      in(96)=*on
437200980923     C                   ENDIF
437300980923     C                   ENDSL
437400980923     C                   ENDIF
437500980923     C                   ENDDO
437600951113     C*
437700040308     C                   ELSE
437800980923     C*
437900980226     C* se la divisa di testata è <> dalla m.c.
438000040308     C                   IF        C1Divisa <> XscDiv
438100980923     C* salvo l'importo calcolato precedentemente
438200980923     C                   EVAL      W2004 = ImporM0202
438300980923     C* calcolo l'importo in conto partendo dall'importo in m.c. per
438400980923     C* arrivare alla divisa di testata (il cambio di calcolo è quello di
438500980923     C* testata
438600980923     C                   RESET                   PJX00202DS
438700980923     C                   MOVEL     C1Divisa      Divisa0202
438800980923     C                   MOVEL     XscDiv        DivMC0202
438900980923     C                   Z-ADD     0             ImporD0202
439000980923     C                   Z-ADD     W2004         ImporM0202
439100980923     C                   Z-ADD     C1Cambio      Cambio0202
439200980923     C                   MOVEL     DtRif045      DtRif0202
439300980923     C*
439400980923     C                   CALLP     X0202TestC(PJX00202DS :
439500980923     C                                        %size(PJX00202DS) :
439600980923     C                                        *on    : Esito   :
439700980923     C                                        *omit  : *omit)
439800980923     C*
439900980923     C* se ci sono errori
440000040308     C                   IF        Esito <> '0'
440100980923     C* gestisco la loro emissione
440200980925     c                   clear                   C1TextMsg
440300980923     C                   DOU       Q2BytAva = 0
440400980923     C                   EXSR      RestoreMsg
440500980923     C                   IF        Q2BytAva > 0
440600980923     C                   SELECT
440700980923     C                   OTHER
440800980923     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
440900980923     C                   MOVE      Q2MsgId       IdMsg
441000980923     C                   CALLB     'XRTVM'
441100980923     C                   PARM                    IDMSG            27
441200980923     C                   PARM                    TXTMSG          100
441300980923     C                   PARM                    ERRMSG            1
441400980923     C                   PARM                    *omit
441500980923     C                   PARM                    *omit
441600980923     C                   PARM      Q2msgtxt      Msgdta          100
441700980923     C                   IF        ErrMsg <> *on
441800980925     C                   CAT       TxtMsg:1      C1TextMsg
441900980925     C                   SETON                                        9699
442000981006     C                   EVAL      in(96)=*on
442100980923     C                   ENDIF
442200980923     C                   ENDSL
442300980923     C                   ENDIF
442400980923     C                   ENDDO
442500980923     C* altrimenti l'importo in conto è quello calcolato
442600040308     C                   ELSE
442700981014     C                   EVAL      S1ImportD = ImporD0202
442800040308     C                   ENDIF
442900040308     C*
443000040308     C* altrimenti l'importo in conto è quello calcolato
443100040308     C                   ELSE
443200040308     C                   EVAL      S1ImportD = ImporM0202
443300040308     C                   ENDIF
443400040308     C*
443500040308     C                   ENDIF
443600040308     C*
443700040308     C                   ENDSR
443800040308     C************************************************************
443900040308     C* Riempimento automatico dell'importo concordato partendo
444000040308     C* dall'importo in conto
444100040308     C************************************************************
444200040308     C     RieImpConc    BEGSR
444300040308     C*
444400980226     C* calcolo l'importo in divisa di partita
444500980226     C                   EXSR      ClcImpDivP
444600980226     C*
444700980923     C                   Z-ADD     ImporD0202    S1ImpConc
444800980226     C*
444900980226     C                   ENDSR
445000980226     C************************************************************
445100980226     C* Calcolo importo in divisa di partita
445200980226     C************************************************************
445300980226     C     ClcImpDivP    BEGSR
445400980226     C*
445500980226     C* l'importo in divisa di partita è un dato che ha senso solo per le
445600980226     C* partite che hanno divisa diversa da quella contabile; queste partite
445700980226     C* non sono gestite a cambio storico
445800980226     C*
445900980226     C* se la divisa di testata è <> dalla m.c.
446000040308     C                   IF        C1Divisa <> XscDiv
446100980923     C* calcolo prima il valore in m.c.
446200980923     C                   RESET                   PJX00202DS
446300980923     C                   MOVEL     C1Divisa      Divisa0202
446400980923     C                   MOVEL     XscDiv        DivMC0202
446500980923     C                   Z-ADD     S1ImportD     ImporD0202
446600980923     C                   Z-ADD     0             ImporM0202
446700980923     C* l'importo in m.c. va calcolato passando il cambio divisa partenza
446800980923     C                   Z-ADD     C1Cambio      Cambio0202
446900980923     C                   MOVEL     DtRif045      DtRif0202
447000980923     C*
447100980923     C                   CALLP     X0202TestC(PJX00202DS :
447200980923     C                                        %size(PJX00202DS) :
447300980923     C                                        *on    : Esito   :
447400980923     C                                        *omit  : *omit)
447500980923     C*
447600980923     C* se ci sono errori
447700040308     C                   IF        Esito <> '0'
447800980923     C* gestisco la loro emissione
447900980925     c                   clear                   C1TextMsg
448000980923     C                   DOU       Q2BytAva = 0
448100980923     C                   EXSR      RestoreMsg
448200980923     C                   IF        Q2BytAva > 0
448300980923     C                   SELECT
448400980923     C                   OTHER
448500980923     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
448600980923     C                   MOVE      Q2MsgId       IdMsg
448700980923     C                   CALLB     'XRTVM'
448800980923     C                   PARM                    IDMSG            27
448900980923     C                   PARM                    TXTMSG          100
449000980923     C                   PARM                    ERRMSG            1
449100980923     C                   PARM                    *omit
449200980923     C                   PARM                    *omit
449300980923     C                   PARM      Q2msgtxt      Msgdta          100
449400980923     C                   IF        ErrMsg <> *on
449500980925     C                   CAT       TxtMsg:1      C1TextMsg
449600980925     C                   SETON                                        9699
449700981006     C                   EVAL      in(96)=*on
449800980923     C                   ENDIF
449900980923     C                   ENDSL
450000980923     C                   ENDIF
450100980923     C                   ENDDO
450200980923     C* se non ci sono errori, questo è l'importo
450300040308     C                   ELSE
450400980923     C                   EVAL      Importo = ImporM0202
450500980923     C                   ENDIF
450600980923     C*
450700980226     C* altrimenti gl'importi sono uguali
450800040308     C                   ELSE
450900980226     C                   EVAL      Importo = S1ImportD
451000040308     C                   ENDIF
451100980923     C*
451200980923     C* poi, dividendo per il cambio concordato, lo trasformo in divisa
451300980226     C* di partita
451400980923     C                   RESET                   PJX00202DS
451500980923     C                   MOVEL     S1Divisa      Divisa0202
451600980923     C                   MOVEL     XscDiv        DivMC0202
451700980923     C                   Z-ADD     0             ImporD0202
451800980923     C                   Z-ADD     Importo       ImporM0202
451900980923     C* l'importo in m.c. va calcolato passando il cambio divisa partenza
452000980923     C                   Z-ADD     S1CamConc     Cambio0202
452100980923     C                   MOVEL     DtRif045      DtRif0202
452200980923     C*
452300980923     C                   CALLP     X0202TestC(PJX00202DS :
452400980923     C                                        %size(PJX00202DS) :
452500980923     C                                        *on    : Esito   :
452600980923     C                                        *omit  : *omit)
452700980923     C*
452800980923     C* se ci sono errori
452900040308     C                   IF        Esito <> '0'
453000980923     C* gestisco la loro emissione
453100980925     c                   clear                   C1TextMsg
453200980923     C                   DOU       Q2BytAva = 0
453300980923     C                   EXSR      RestoreMsg
453400980923     C                   IF        Q2BytAva > 0
453500980923     C                   SELECT
453600980923     C                   WHEN      Q2MsgId = 'MVC0067'
453700980923     C                   SETON                                        7799
453800980923     C                   Eval      in(77) = *on
453900980923     C                   OTHER
454000980923     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
454100980923     C                   MOVE      Q2MsgId       IdMsg
454200980923     C                   CALLB     'XRTVM'
454300980923     C                   PARM                    IDMSG            27
454400980923     C                   PARM                    TXTMSG          100
454500980923     C                   PARM                    ERRMSG            1
454600980923     C                   PARM                    *omit
454700980923     C                   PARM                    *omit
454800980923     C                   PARM      Q2msgtxt      Msgdta          100
454900980923     C                   IF        ErrMsg <> *on
455000980925     C                   CAT       TxtMsg:1      C1TextMsg
455100980925     C                   SETON                                        9699
455200981006     C                   EVAL      in(96)=*on
455300980923     C                   ENDIF
455400980923     C                   ENDSL
455500980923     C                   ENDIF
455600980923     C                   ENDDO
455700040308     C                   ENDIF
455800980226     C*
455900980226     C                   ENDSR
456000980226     C************************************************************
456100980226     C* Calcolo lo sconto rispetto alle percentuali in testata
456200980226     C************************************************************
456300980226     C     CalcSconto    BEGSR
456400980226     C*
456500951120     C                   EVAL      ResiduoD = S1ResiduoD
456600951113     C                   EVAL      ScontoD  = 0
456700951030     C*
456800951110     C* valorizzo lo sconto in automatico se presente la 1a percentuale
456900951110     C* di testata
457000040308     C                   IF        C1PercSco1 > 0           and
457100951110     C* se lo sconto su riga non è valorizzato
457200951110     C                             ( S1ScontoD = 0   or
457300951110     C* oppure se richiesta forzatura su tutte le righe
457400951110     C                               C1ForzaSco = Xscon  )
457500980925     C     ResiduoD      DIV       100           W3015
457600980925     C                   MULT      C1PercSco1    W3015
457700951110     C* arrotondo lo sconto calcolato ai decimali della divisa di partita
457800040308     C                   SELECT
457900951110     C* se la divisa non è né quella di testata né la m.c.
458000951110     C     S1Divisa      WHENNE    C1Divisa
458100951110     C     S1Divisa      ANDNE     XscDiv
458200980925     C* i decimali validi sono stati reperiti con la chiamata alla procedura
458300980925     C* X0202TestC e salvati su sfl
458400980925     C                   Z-ADD     DVXNrDec      Decimali
458500960322     C* se la divisa è la m.c.
458600960322     C     S1Divisa      WHENEQ    XscDiv
458700980923     C* i decimali validi sono stati importati
458800980923     C                   Z-ADD     £NrDec        Decimali
458900951110     C* se la divisa è quella di testata
459000951110     C     S1Divisa      WHENEQ    C1Divisa
459100980923     C* i decimali validi sono stati importati
459200980923     C                   Z-ADD     £NrDecDiv     Decimali
459300040308     C                   ENDSL
459400980925     C                   CALLP     X0201Arrot (W3015   : 'H' : Decimali)
459500951110     C* calcolo a quanto vale il residuo senza questo sconto
459600980925     C                   EVAL      ResiduoD = ResiduoD - W3015
459700040308     C                   ENDIF
459800951110     C*
459900951110     C* valorizzo lo sconto in automatico se presente la 2a percentuale
460000951110     C* d1 testata
460100040308     C                   IF        C1PercSco2 > 0           and
460200951110     C* se lo sconto su riga non è valorizzato
460300951110     C                             ( S1ScontoD = 0   or
460400951110     C* oppure se richiesta forzatura su tutte le righe
460500951110     C                               C1ForzaSco = Xscon  )
460600980925     C     ResiduoD      DIV       100           W3015
460700980925     C                   MULT      C1PercSco2    W3015
460800951110     C* arrotondo lo sconto calcolato ai decimali della divisa di partita
460900040308     C                   SELECT
461000951110     C* se la divisa non è né quella di testata né la m.c.
461100951110     C     S1Divisa      WHENNE    C1Divisa
461200951110     C     S1Divisa      ANDNE     XscDiv
461300980925     C* i decimali validi sono stati reperiti con la chiamata alla procedura
461400980925     C* X0202TestC e salvati su sfl
461500980925     C                   Z-ADD     DVXNrDec      Decimali
461600960322     C* se la divisa è la m.c.
461700960322     C     S1Divisa      WHENEQ    XscDiv
461800980923     C* i decimali validi sono stati importati
461900980923     C                   Z-ADD     £NrDec        Decimali
462000951110     C* se la divisa è quella di testata
462100951110     C     S1Divisa      WHENEQ    C1Divisa
462200980923     C* i decimali validi sono stati importati
462300980923     C                   Z-ADD     £NrDecDiv     Decimali
462400040308     C                   ENDSL
462500980925     C                   CALLP     X0201Arrot (W3015   : 'H' : Decimali)
462600951113     C* calcolo a quanto vale il residuo senza questo sconto
462700980925     C                   EVAL      ResiduoD = ResiduoD - W3015
462800040308     C                   ENDIF
462900951113     C*
463000951113     C* lo sconto complessivo è dato dalla somma dello sconto generato
463100951113     C* dalla 1a %, calcolata sul residuo complessivo, con quello generato
463200951113     C* dalla 2a %, calcolata sul residuo decurtato dello sconto precedente
463300951120     C                   EVAL      S1ScontoD = S1ResiduoD - ResiduoD
463400951110     C*
463500951110     C                   ENDSR
463600951110     C************************************************************
463700951113     C* Controllo il numero di decimali digitato per gli importi
463800951110     C************************************************************
463900951113     C     CtrlNrDec     BEGSR
464000951113     C*
464100951113     C* abbuono
464200951113     C*
464300951113     C* sconto
464400951113     C*
464500951113     C* importo concordato
464600951113     C*
464700951113     C                   IF        S1ImpConc <> 0
464800951113     C* siccome l'importo concordato è nella divisa di partita
464900960322     C                   SELECT
465000960322     C* se la divisa di partita non è né quella di testata né la m.c.
465100960322     C     S1Divisa      WHENNE    C1Divisa
465200960322     C     S1Divisa      ANDNE     XscDiv
465300980925     C* i decimali validi sono stati reperiti con la chiamata alla procedura
465400980925     C* X0202TestC e salvati su sfl
465500980925     C                   Z-ADD     DVXNrDec      Decimali
465600960322     C* se la divisa è la m.c.
465700960322     C     S1Divisa      WHENEQ    XscDiv
465800960322     C* i decimali validi sono stati valori importati
465900960322     C                   Z-ADD     £NrDec        Decimali
466000960322     C* se la divisa è quella di testata
466100960322     C     S1Divisa      WHENEQ    C1Divisa
466200960322     C* i decimali validi sono stati valori importati
466300960322     C                   Z-ADD     £NrDecDiv     Decimali
466400040308     C                   ENDSL
466500951113     C*
466600951113     C                   Z-ADD     S1ImpConc     ImportoDec
466700980923     C                   IF        X0201NrDec(ImportoDec : Decimali ) = *on
466800951113     C                   SETON                                        7899
466900951113     C                   Eval      in(78) = *on
467000951113     C                   ENDIF
467100951113     C                   ENDIF
467200951113     C*
467300951113     C                   ENDSR
467400951113     C************************************************************
467500951113     C* Calcolo i controvalori della riga
467600951113     C************************************************************
467700951113     C     CalcCV        BEGSR
467800951113     C*
467900951030     C* importo in conto
468000951030     C*
468100040308     C                   IF        S1ImportD <> 0
468200960424     C*
468300951110     C* se la divisa di partita NON è la m.c.
468400040308     C                   IF        S1Divisa <> XscDiv
468500040308     C                   RESET                   PJX00202DS
468600040308     C* l'importo in conto fa sempre riferimento alla divisa di contabile
468700040308     C                   MOVEL     C1Divisa      Divisa0202
468800040308     C                   MOVEL     XscDiv        DivMC0202
468900040308     C                   Z-ADD     S1ImportD     ImporD0202
469000040308     C                   Z-ADD     0             ImporM0202
469100040308     C* se gestione a cambio storico
469200040308     C                   IF        CmbSto045 = *on
469300040308     C* se la riga è d'immissione (partita nuova)
469400040308     C                   IF        H1NrAsInt = 0
469500040308     C* il cambio sarà quello di riferimento
469600040308     C                   Z-ADD     0             Cambio0202
469700040308     C* altrimenti (partita già esistente)
469800040308     C                   ELSE
469900040308     C* uso il cambio di partita
470000040308     C                   Z-ADD     S1Cambio      Cambio0202
470100040308     C                   ENDIF
470200040308     C* se gestione NON a cambio storico
470300040308     C                   ELSE
470400040308     C* uso il cambio di testata
470500040308     C                   Z-ADD     C1Cambio      Cambio0202
470600040308     C                   ENDIF
470700040308     C                   MOVEL     DtRif045      DtRif0202
470800040308     C*
470900040308     C                   CALLP     X0202TestC(PJX00202DS :
471000040308     C                                        %size(PJX00202DS) :
471100040308     C                                        *on    : Esito   :
471200040308     C                                        *omit  : *omit)
471300040308     C* se errori
471400040308     C                   IF        Esito <> '0'
471500980923     C* gestisco la loro emissione
471600980925     c                   clear                   C1TextMsg
471700980923     C                   DOU       Q2BytAva = 0
471800980923     C                   EXSR      RestoreMsg
471900980923     C                   IF        Q2BytAva > 0
472000980923     C                   SELECT
472100980923     C                   WHEN      Q2MsgId = 'PRO0132'
472200980923     C                   SETON                                        7499
472300980923     C                   Eval      in(74) = *on
472400980923     C                   OTHER
472500980923     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
472600980923     C                   MOVE      Q2MsgId       IdMsg
472700980923     C                   CALLB     'XRTVM'
472800980923     C                   PARM                    IDMSG            27
472900980923     C                   PARM                    TXTMSG          100
473000980923     C                   PARM                    ERRMSG            1
473100980923     C                   PARM                    *omit
473200980923     C                   PARM                    *omit
473300980923     C                   PARM      Q2msgtxt      Msgdta          100
473400980923     C                   IF        ErrMsg <> *on
473500980925     C                   CAT       TxtMsg:1      C1TextMsg
473600980925     C                   SETON                                        9699
473700981006     C                   EVAL      in(96)=*on
473800980923     C                   ENDIF
473900980923     C                   ENDSL
474000980923     C                   ENDIF
474100980923     C                   ENDDO
474200980923     C* se NO errori
474300040308     C                   ELSE
474400980923     C                   EVAL      H1Importo = ImporM0202
474500980923     C                   ENDIF
474600960424     C*
474700960424     C* se la divisa di partita è la m.c.
474800040308     C                   ELSE
474900040308     C* se la divisa di partita non è quella di contabile
475000040308     C                   IF        S1Divisa <> C1Divisa
475100040308     C* l'importo in m.c. DEVE essere = importo di partita
475200040308     C                   EVAL      H1Importo  = S1ImpDivP
475300040308     C* se la divisa di partita è quella di contabile
475400040308     C                   ELSE
475500040308     C* gli importi sono uguali
475600040308     C                   EVAL      H1Importo = S1ImportD
475700040308     C                   ENDIF
475800040308     C                   ENDIF
475900040308     C*
476000040308     C                   ENDIF
476100040308     C*
476200951030     C* abbuono
476300951030     C*
476400951110     C                   IF        S1AbbuonoD <> 0
476500990528C0941C*
476600990526  "  C* memorizzo qual'è la divisa degli abbuoni
476700990526  "  C*  se la divisa dell'abbuono è sempre la stessa
476800990526  "  C                   IF        DivDiveAbb = *off
476900990526  "  C*  se è la prima volta che trovo un abbuono
477000990526  "  C                   IF        DivAbbuono = *blank
477100990526  "  C*  imposto la divisa dell'abbuono
477200990526  "  C                   EVAL      DivAbbuono = S1Divisa
477300990526  "  C*  se NON è la prima volta che trovo un abbuono
477400990526  "  C                   ELSE
477500990526  "  C*  testo se la divisa di questa partita è diversa dalle altre
477600990526  "  C                   IF        DivAbbuono <> S1Divisa
477700990526  "  C*  se sì, debbo indicare che le divise dell'abbuono sono diverse
477800990526  "  C                   EVAL      DivDiveAbb = *on
477900990526  "  C                   EVAL      DivAbbuono = *blank
478000990526  "  C                   ENDIF
478100990526  "  C                   ENDIF
478200990528C0941C                   ENDIF
478300990526     C*
478400981007     C* se la divisa di partita NON è la m.c. (l'abbuono è sempre in divisa
478500990528C0941C*  di testata)
478600980923     C                   IF        S1Divisa <> XscDiv
478700980923     C                   RESET                   PJX00202DS
478800980923     C                   MOVEL     S1Divisa      Divisa0202
478900980923     C                   MOVEL     XscDiv        DivMC0202
479000980923     C                   Z-ADD     S1AbbuonoD    ImporD0202
479100980923     C                   Z-ADD     0             ImporM0202
479200980923     C* se gestione a cambio storico
479300980923     C                   IF        CmbSto045 = *on
479400980923     C* se la riga è d'immissione (partita nuova)
479500040308     C                   IF        H1NrAsInt = 0
479600980923     C* il cambio sarà quello di riferimento
479700980923     C                   Z-ADD     0             Cambio0202
479800980923     C* altrimenti (partita già esistente)
479900980923     C                   ELSE
480000980923     C* uso il cambio di partita
480100980923     C                   Z-ADD     S1Cambio      Cambio0202
480200980923     C                   ENDIF
480300980923     C* altrimenti
480400980923     C                   ELSE
480500990528C0941C* se divisa di testata e quella di partita sono diverse
480600990526  "  C                   IF        S1Divisa <> C1Divisa
480700990526  "  C* uso il cambio di partita
480800990526  "  C                   Z-ADD     S1Cambio      Cambio0202
480900990526  "  C* altrimenti
481000990528C0941C                   ELSE
481100980923     C* uso il cambio di testata
481200980923     C                   Z-ADD     C1Cambio      Cambio0202
481300990528C0941C                   ENDIF
481400980923     C                   ENDIF
481500980923     C                   MOVEL     DtRif045      DtRif0202
481600980923     C*
481700980923     C                   CALLP     X0202TestC(PJX00202DS :
481800980923     C                                        %size(PJX00202DS) :
481900980923     C                                        *on    : Esito   :
482000980923     C                                        *omit  : *omit)
482100980923     C* se errori
482200040308     C                   IF        Esito <> '0'
482300980923     C* gestisco la loro emissione
482400980925     c                   clear                   C1TextMsg
482500980923     C                   DOU       Q2BytAva = 0
482600980923     C                   EXSR      RestoreMsg
482700980923     C                   IF        Q2BytAva > 0
482800980923     C                   SELECT
482900990602C0941C                   WHEN      Q2MsgId = 'PRO0132'
483000990602  "  C                   SETON                                        7599
483100990602C0941C                   Eval      in(75) = *on
483200980923     C                   OTHER
483300980923     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
483400980923     C                   MOVE      Q2MsgId       IdMsg
483500980923     C                   CALLB     'XRTVM'
483600980923     C                   PARM                    IDMSG            27
483700980923     C                   PARM                    TXTMSG          100
483800980923     C                   PARM                    ERRMSG            1
483900980923     C                   PARM                    *omit
484000980923     C                   PARM                    *omit
484100980923     C                   PARM      Q2msgtxt      Msgdta          100
484200980923     C                   IF        ErrMsg <> *on
484300980925     C                   CAT       TxtMsg:1      C1TextMsg
484400980925     C                   SETON                                        9699
484500981006     C                   EVAL      in(96)=*on
484600980923     C                   ENDIF
484700980923     C                   ENDSL
484800980923     C                   ENDIF
484900980923     C                   ENDDO
485000980923     C* se NO errori
485100040308     C                   ELSE
485200980923     C                   EVAL      H1Abbuono = ImporM0202
485300980923     C                   ENDIF
485400980923     C*
485500980923     C* se divisa di partita = m.c., gl'importi sono uguali
485600951110     C                   ELSE
485700951110     C                   EVAL      H1Abbuono = S1AbbuonoD
485800951110     C                   ENDIF
485900980923     C*
486000951204     C* testo che l'abbuono in m.c. non superi l'importo massimo
486100951204     C                   IF        H1Abbuono > MaxAbb0IP    and
486200951204     C                             MaxAbb0IP > 0            and
486300951204     C* se non è stata richiesta la forzatura
486400951204     C                             S1FrcAbb = XscOff
486500951204     C                   SETON                                        6899
486600951204     C                   EVAL      in(68) = *on
486700951204     C                   ENDIF
486800951110     C                   ENDIF
486900951030     C*
487000951030     C* sconto
487100951030     C*
487200990526     C                   IF        S1ScontoD <> 0
487300990528C0762C*
487400990526  "  C* memorizzo qual'è la divisa degli sconti
487500990526  "  C*  se la divisa dello sconto è sempre la stessa
487600990526  "  C                   IF        DivDiveSco = *off
487700990526  "  C*  se è la prima volta che trovo uno sconto
487800990526  "  C                   IF        DivSconto = *blank
487900990526  "  C*  imposto la divisa dello sconto
488000990526  "  C                   EVAL      DivSconto = S1Divisa
488100990526  "  C*  se NON è la prima volta che trovo uno sconto
488200990526  "  C                   ELSE
488300990526  "  C*  testo se la divisa di questa partita è diversa dalle altre
488400990526  "  C                   IF        DivSconto <> S1Divisa
488500990526  "  C*  se sì, debbo indicare che le divise dello sconto sono diverse
488600990526  "  C                   EVAL      DivDiveSco = *on
488700990526  "  C                   EVAL      DivSconto = *blank
488800990526  "  C                   ENDIF
488900990526  "  C                   ENDIF
489000990528C0762C                   ENDIF
489100990210     C*
489200981007     C* se la divisa di partita NON è la m.c. (lo sconto è sempre in divisa
489300990528C0762C*  di partita)
489400951110     C                   IF        S1Divisa <> XscDiv
489500980923     C                   RESET                   PJX00202DS
489600980923     C                   MOVEL     S1Divisa      Divisa0202
489700980923     C                   MOVEL     XscDiv        DivMC0202
489800980923     C                   Z-ADD     S1ScontoD     ImporD0202
489900980923     C                   Z-ADD     0             ImporM0202
490000980923     C* se gestione a cambio storico
490100980923     C                   IF        CmbSto045 = *on
490200980923     C* uso il cambio di partita (lo sconto si può digitare solo per partite
490300980923     C*  esistenti)
490400980923     C                   Z-ADD     S1Cambio      Cambio0202
490500980923     C* altrimenti
490600990528C0762C                   ELSE
490700990526  "  C* se divisa di testata e quella di partita sono diverse
490800990526  "  C                   IF        S1Divisa <> C1Divisa
490900990526  "  C* uso il cambio di partita
491000990526  "  C                   Z-ADD     S1Cambio      Cambio0202
491100990526  "  C* altrimenti
491200990526  "  C                   ELSE
491300990526  "  C* uso il cambio di testata
491400990526  "  C                   Z-ADD     C1Cambio      Cambio0202
491500990528C0762C                   ENDIF
491600990204     C                   ENDIF
491700980923     C                   MOVEL     DtRif045      DtRif0202
491800980923     C*
491900980923     C                   CALLP     X0202TestC(PJX00202DS :
492000980923     C                                        %size(PJX00202DS) :
492100980923     C                                        *on    : Esito   :
492200980923     C                                        *omit  : *omit)
492300980923     C* se errori
492400040308     C                   IF        Esito <> '0'
492500980923     C* gestisco la loro emissione
492600980925     c                   clear                   C1TextMsg
492700980923     C                   DOU       Q2BytAva = 0
492800980923     C                   EXSR      RestoreMsg
492900980923     C                   IF        Q2BytAva > 0
493000980923     C                   SELECT
493100990210     C                   WHEN      Q2MsgId = 'PRO0132'
493200990210     C                   SETON                                        7699
493300990210     C                   Eval      in(76) = *on
493400980923     C                   OTHER
493500980923     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
493600980923     C                   MOVE      Q2MsgId       IdMsg
493700980923     C                   CALLB     'XRTVM'
493800980923     C                   PARM                    IDMSG            27
493900980923     C                   PARM                    TXTMSG          100
494000980923     C                   PARM                    ERRMSG            1
494100980923     C                   PARM                    *omit
494200980923     C                   PARM                    *omit
494300980923     C                   PARM      Q2msgtxt      Msgdta          100
494400980923     C                   IF        ErrMsg <> *on
494500980925     C                   CAT       TxtMsg:1      C1TextMsg
494600980925     C                   SETON                                        9699
494700981006     C                   EVAL      in(96)=*on
494800980923     C                   ENDIF
494900980923     C                   ENDSL
495000980923     C                   ENDIF
495100980923     C                   ENDDO
495200980923     C* se NO errori
495300040308     C                   ELSE
495400980923     C                   EVAL      H1Sconto = ImporM0202
495500980923     C                   ENDIF
495600980923     C*
495700980923     C* se divisa di partita = m.c., gl'importi sono uguali
495800951110     C                   ELSE
495900951113     C                   EVAL      H1Sconto = S1ScontoD
496000951110     C                   ENDIF
496100951110     C                   ENDIF
496200951110     C*
496300951110     C* sopravvenienza
496400951110     C*
496500951110     C                   IF        S1SopravvD <> 0
496600951110     C* la divisa di partita NON può essere = alla contabile altrimenti
496700951110     C* NON avrebbe sopravvenienza ;
496800981007     C* se la divisa di partita è <> dalla m.c. (la sopravvenienza è sempre
496900981007     C*  in divisa di partita)
497000951110     C                   IF        S1Divisa <> XscDiv
497100980923     C                   RESET                   PJX00202DS
497200980923     C                   MOVEL     S1Divisa      Divisa0202
497300980923     C                   MOVEL     XscDiv        DivMC0202
497400980923     C                   Z-ADD     S1SopravvD    ImporD0202
497500981006     C* la sopravvenienza può essere visualizzata negativa ma X0202TestC
497600981006     C* darebbe errore solo per questo, per cui viene passato il suo valore
497700981006     C* assoluto
497800981006     C                   IF        ImporD0202 < 0
497900981006     C                   MULT      -1            ImporD0202
498000981006     C                   ENDIF
498100980923     C                   Z-ADD     0             ImporM0202
498200980923     C                   Z-ADD     S1CamConc     Cambio0202
498300980923     C                   MOVEL     DtRif045      DtRif0202
498400980923     C*
498500980923     C                   CALLP     X0202TestC(PJX00202DS :
498600980923     C                                        %size(PJX00202DS) :
498700980923     C                                        *on    : Esito   :
498800980923     C                                        *omit  : *omit)
498900980923     C* se errori
499000040308     C                   IF        Esito <> '0'
499100980923     C* gestisco la loro emissione
499200980925     c                   clear                   C1TextMsg
499300980923     C                   DOU       Q2BytAva = 0
499400980923     C                   EXSR      RestoreMsg
499500980923     C                   IF        Q2BytAva > 0
499600980923     C                   SELECT
499700980923     C                   OTHER
499800980923     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
499900980923     C                   MOVE      Q2MsgId       IdMsg
500000980923     C                   CALLB     'XRTVM'
500100980923     C                   PARM                    IDMSG            27
500200980923     C                   PARM                    TXTMSG          100
500300980923     C                   PARM                    ERRMSG            1
500400980923     C                   PARM                    *omit
500500980923     C                   PARM                    *omit
500600980923     C                   PARM      Q2msgtxt      Msgdta          100
500700980923     C                   IF        ErrMsg <> *on
500800980925     C                   CAT       TxtMsg:1      C1TextMsg
500900980925     C                   SETON                                        9699
501000981006     C                   EVAL      in(96)=*on
501100980923     C                   ENDIF
501200980923     C                   ENDSL
501300980923     C                   ENDIF
501400980923     C                   ENDDO
501500980923     C* se NO errori
501600040308     C                   ELSE
501700980923     C                   EVAL      H1Sopravv = ImporM0202
501800981006     C* la sopravvenienza può essere visualizzata negativa ma X0202TestC
501900981006     C* darebbe errore solo per questo, per cui reimposto il suo segno
502000981006     C                   IF        S1SopravvD < 0
502100981006     C                   MULT      -1            H1Sopravv
502200981006     C                   ENDIF
502300980923     C                   ENDIF
502400980923     C*
502500980923     C* se divisa di partita = m.c., gl'importi sono uguali
502600951110     C                   ELSE
502700951110     C                   EVAL      H1Sopravv = S1SopravvD
502800951110     C                   ENDIF
502900951110     C                   ENDIF
503000951030     C*
503100951030     C                   ENDSR
503200950927     C************************************************************
503300951030     C* Calcolo differenza cambi
503400950927     C************************************************************
503500951030     C     CalcDifCam    BEGSR
503600950927     C*
503700951030     C* se la divisa è la stessa della contabile
503800951030     C                   IF        S1Divisa = C1Divisa
503900980923     C*
504000951110     C* se la divisa di partita non è la m.c.
504100951113     C                   IF        S1Divisa <> XscDiv
504200980923     C* calcolo l'importo in m.c. al cambio di partita;
504300980923     C                   RESET                   PJX00202DS
504400981007     C                   MOVEL     C1Divisa      Divisa0202
504500980923     C                   MOVEL     XscDiv        DivMC0202
504600980923     C                   Z-ADD     S1ImportD     ImporD0202
504700980923     C                   Z-ADD     0             ImporM0202
504800980923     C                   Z-ADD     S1Cambio      Cambio0202
504900980923     C                   MOVEL     DtRif045      DtRif0202
505000981005     C* non richiedo il controllo scostamento cambio/importo perchè il
505100981005     C* cambio recuperato è un valore storico
505200981005     C                   MOVEL     *on           NoCtrlScos
505300980923     C*
505400980923     C                   CALLP     X0202TestC(PJX00202DS :
505500980923     C                                        %size(PJX00202DS) :
505600981005     C                                        *off   : Esito   :
505700981005     C                                        NoCtrlScos : *omit)
505800951110     C* poi lo sottraggo dall'importo in m.c. al cambio di contabile
505900980923     C                   EVAL      S1DiffCam = H1Importo - ImporM0202
506000980923     C*
506100951110     C* altrimenti non esiste differenza cambi
506200951110     C                   ELSE
506300951110     C                   EVAL      S1DiffCam = 0
506400951110     C                   ENDIF
506500980923     C*
506600980923     C* se la divisa di partita NON è la stessa della contabile
506700951030     C                   ELSE
506800980923     C*
506900951110     C* se la divisa di partita non è la m.c.
507000951113     C                   IF        S1Divisa <> XscDiv
507100951116     C* calcolo il valore in m.c. dell'importo concordato al cambio storico;
507200980923     C                   RESET                   PJX00202DS
507300980923     C                   MOVEL     S1Divisa      Divisa0202
507400980923     C                   MOVEL     XscDiv        DivMC0202
507500980923     C                   Z-ADD     S1ImpConc     ImporD0202
507600980923     C                   Z-ADD     0             ImporM0202
507700980923     C                   Z-ADD     S1Cambio      Cambio0202
507800980923     C                   MOVEL     DtRif045      DtRif0202
507900981005     C* non richiedo il controllo scostamento cambio/importo perchè il
508000981005     C* cambio recuperato è un valore storico
508100981005     C                   MOVEL     *on           NoCtrlScos
508200980923     C*
508300980923     C                   CALLP     X0202TestC(PJX00202DS :
508400980923     C                                        %size(PJX00202DS) :
508500981005     C                                        *off   : Esito   :
508600981005     C                                        NoCtrlScos : *omit)
508700980923     C                   EVAL      Importo = ImporM0202
508800951109     C* calcolo il valore in m.c. dell'importo concordato al cambio
508900951116     C* assegnato alla partita;
509000980923     C                   RESET                   PJX00202DS
509100980923     C                   MOVEL     S1Divisa      Divisa0202
509200980923     C                   MOVEL     XscDiv        DivMC0202
509300980923     C                   Z-ADD     S1ImpConc     ImporD0202
509400980923     C                   Z-ADD     0             ImporM0202
509500980923     C                   Z-ADD     S1CamConc     Cambio0202
509600980923     C                   MOVEL     DtRif045      DtRif0202
509700981005     C* non richiedo il controllo scostamento cambio/importo perchè il
509800981005     C* cambio recuperato è un valore storico
509900981005     C                   MOVEL     *on           NoCtrlScos
510000980923     C*
510100980923     C                   CALLP     X0202TestC(PJX00202DS :
510200980923     C                                        %size(PJX00202DS) :
510300981005     C                                        *off   : Esito   :
510400981005     C                                        NoCtrlScos : *omit)
510500951116     C* la differenza cambi è la differenza tra l'importo concordato in m.c.
510600951116     C* calcolato al cambio storico e questo
510700980923     C                   EVAL      S1DiffCam = Importo - ImporM0202
510800980923     C*
510900980923     C* se la divisa di partita è la m.c. NON esiste differenza cambi
511000951110     C                   ELSE
511100951110     C                   EVAL      S1DiffCam = 0
511200951110     C                   ENDIF
511300951030     C                   ENDIF
511400951030     C*
511500951030     C                   ENDSR
511600951018     C************************************************************
511700990203     C* Testo se c'è coerenza tra l'importo in conto e l'importo
511800990204     C* in divisa di partita (concordato + sconto), quando la
511900990204     C* divisa di partita è diversa da quella di testata
512000951018     C************************************************************
512100990203     C     TstDivDive    BEGSR
512200951018     C*
512300990204     C* per ottenere questo test effettuo il controllo che l'importo in m.c.
512400990204     C* dell'importo in conto e l'importo concordato siano congruenti
512500990204     C                   RESET                   PJX00202DS
512600990204     C                   MOVEL     S1Divisa      Divisa0202
512700990204     C                   MOVEL     XscDiv        DivMC0202
512800990204     C                   Z-ADD     S1ImpConc     ImporD0202
512900990204     C                   Z-ADD     H1Importo     ImporM0202
513000990204     C                   Z-ADD     S1CamConc     Cambio0202
513100990204     C                   MOVEL     DtRif045      DtRif0202
513200990204     C                   MOVEL     *off          NoCtrlScos
513300990204     C*
513400990204     C                   CALLP     X0202TestC(PJX00202DS :
513500990204     C                                        %size(PJX00202DS) :
513600990204     C                                        *off   : Esito   :
513700990204     C                                        NoCtrlScos : *omit    :
513800990204     C                                        %size(ANDVS00F)   :
513900990204     C                                        %size(ANDVX00F)   :
514000990204     C                                        ANDVS00F : ANDVX00F )
514100990204     C* se errore
514200040308     C                   IF        Esito <> '0'
514300990204     C* emetto un avvertimento non bloccante (cioè non accendo il 99)
514400990204     C                   SETON                                        71
514500990204     C                   EVAL      in(71)=*on
514600990204     C                   ENDIF
514700990203     C*
514800990203     C                   ENDSR
514900990203     C************************************************************
515000990203     C* Testo se la partita è a chiudere anche per la m.c. oppure
515100990203     C* devo generare una differenza cambio
515200990203     C************************************************************
515300990203     C     TstParChiu    BEGSR
515400990203     C*
515500951110     C* il test di chiusura partita in m.c. è il medesimo sia per partite
515600951110     C* con divisa = alla contabile, sia per partite con divisa <>
515700040308     C*
515800951110     C* l'importo in conto +/- l'abbuono + sconto deve essere <= residuo
515900951110     C                   EVAL      Importo = H1Importo + H1Sconto
516000951110     C* se l'abbuono è attivo e il segno partita è dare
516100990617D0122C* (incasso di +  nel caso di saldaconto cliente   oppure
516200990617D0122C*  ricevo indietro di +  nel caso di saldaconto fornitore)
516300040308     C                   IF        S1SegAbb = 'A'  and  S1Segno = XscDare or
516400951110     C* oppure l'abbuono è passivo e il segno partita è avere
516500990617D0122C* (rendo di +  nel caso di saldaconto cliente   oppure
516600990617D0122C*  pago di +  nel caso di saldaconto fornitore)
516700040308     C                             S1SegAbb = 'P'  and  S1Segno = XscAvere
516800040308     C*  l'importo è > del (residuo-sconto) per cui l'abbuono va sottratto
516900040308     C                   EVAL      Importo = Importo - H1Abbuono
517000040308     C* viceversa
517100040308     C                   ELSE
517200040308     C*  l'importo è < del (residuo-sconto) per cui l'abbuono va sommato
517300040308     C                   EVAL      Importo = Importo + H1Abbuono
517400040308     C                   ENDIF
517500040308     C*
517600040308     C* se NO storico
517700040308     C                   if        cmbsto045 = *off
517800040308     C*
517900040308     C* vado ad aggiungere la differenza nella differenza cambio
518000040308     C                   EVAL      S1DiffCam = Importo-S1Residuo
518100961126     C                             - H1sopravv
518200971224     C*
518300971224     C* se storico
518400960320     c                   else
518500960320     c                   eval      H1Importo = H1importo -
518600960320     c                             (importo - S1residuo)
518700960320     c                   eval      Importo = s1residuo
518800960320     c                   endif
518900951110     C*
519000951110     C                   ENDSR
519100951120     C************************************************************
519200951120     C* Calcolo la ritenuta d'acconto rispetto all'importo da
519300951120     C* contabilizzare
519400951120     C************************************************************
519500951120     C     CalcRitAcc    BEGSR
519600951120     C*
519700951120     C                   RESET                   NDCR96DS
519800951120     C                   MOVE      XscSoc        R96Societa
519900951120     C                   MOVE      C1Kcc         R96Kcc
520000951120     C                   MOVE      C1Ksc         R96Ksc
520100951120     C                   MOVE      S1NrPar       R96NrPar
520200951120     C     S1DtPar       IFGT      0
520300960201     C                   CALLB     'XDT4000'
520400951120     C     R96DtPar      PARM                    XDTAMG           10
520500951120     C                   PARM      S1DtPar       XDTGMA            6 0
520600951120     C                   PARM      4             XDTSTA            1 0
520700951120     C                   ELSE
520800951120     C                   MOVE      Dt0           R96DtPar
520900951120     C                   ENDIF
521000951120     C                   MOVE      S1SerPar      R96SerPar
521100981106     C* se divisa di partita = a quella di testata (contabile)
521200981106     C                   IF        S1Divisa = C1Divisa
521300951120     C* se l'abbuono è attivo e il segno partita è dare
521400990617D0122C* (incasso di +  nel caso di saldaconto cliente   oppure
521500990617D0122C*  ricevo indietro di +  nel caso di saldaconto fornitore)
521600961009     C                   IF        S1SegAbb = 'A'  and  S1Segno = XscDare or
521700951120     C* oppure l'abbuono è passivo e il segno partita è avere
521800990617D0122C* (rendo di +  nel caso di saldaconto cliente   oppure
521900990617D0122C*  pago di +  nel caso di saldaconto fornitore)
522000990617     C                             S1SegAbb = 'P'  and  S1Segno = XscAvere
522100951120     C*  l'importo è > del (residuo-sconto) per cui l'abbuono va sottratto
522200951120     C                   EVAL      AbbuonoD = S1AbbuonoD * -1
522300951120     C* viceversa
522400951120     C                   ELSE
522500951120     C*  l'importo è < del (residuo-sconto) per cui l'abbuono va sommato
522600951120     C                   EVAL      AbbuonoD = S1AbbuonoD
522700951120     C                   ENDIF
522800980925     C                   EVAL      W3015   = (S1ImportD + S1ScontoD +
522900970310     C                                        AbbuonoD)
523000980925     C                   DIV       S1ResiduoD    W3015
523100990617     C*
523200981106     C* se divisa di partita <> a quella di testata (contabile)
523300981106     C                   ELSE
523400040308C0762X***  S1ImpConc     DIV       S1ResiduoD    W3015
523500990528C0762C     S1ImpConc     ADD       S1ScontoD     W3015
523600990617C0941C* se l'abbuono è attivo e il segno partita è dare
523700990617  "  C* (incasso di +  nel caso di saldaconto cliente   oppure
523800990617  "  C*  ricevo indietro di +  nel caso di saldaconto fornitore)
523900990617  "  C                   IF        S1SegAbb = 'A'  and  S1Segno = XscDare or
524000990617  "  C* oppure l'abbuono è passivo e il segno partita è avere
524100990617  "  C* (rendo di +  nel caso di saldaconto cliente   oppure
524200990617  "  C*  pago di +  nel caso di saldaconto fornitore)
524300990617  "  C                             S1SegAbb = 'P'  and  S1Segno = XscAvere
524400990617  "  C*  l'importo è > del (residuo-sconto) per cui l'abbuono va sottratto
524500990617  "  C                   EVAL      AbbuonoD = S1AbbuonoD * -1
524600990617  "  C* viceversa
524700990617  "  C                   ELSE
524800990617  "  C*  l'importo è < del (residuo-sconto) per cui l'abbuono va sommato
524900990617  "  C                   EVAL      AbbuonoD = S1AbbuonoD
525000011109C0941C                   ENDIF
525100011113D1004X**ex C0941******** EVAL      W3015   = (S1ImportD + S1ScontoD +
525200011109D1004C                   EVAL      W3015   = (S1ImpConc + S1ScontoD +
525300990617C0941C                                        AbbuonoD)
525400990122     C                   DIV       S1ResiduoD    W3015
525500981106     C                   ENDIF
525600980925     C     W3015         MULT      100           R96Per
525700951122     C                   Z-ADD     £NrDec        R96NrDec
525800981103     C                   Z-ADD     DVXNrDec      R96NrDecD
525900981103     C                   EVAL      R96DivMC = XscDiv
526000981103     C                   EVAL      R96DtRfCam = DtRif045
526100981103     C* se la divisa di partita è = a quella di testata
526200981103     C                   IF        S1Divisa = C1Divisa
526300981103     C* passo il cambio di testata
526400981103     C                   EVAL      R96Cambio = C1Cambio
526500981103     C* dichiaro che la modalità è di ricalcolo
526600981103     C                   EVAL      R96Modo = '2'
526700981103     C* altrimenti
526800981103     C                   ELSE
526900981103     C* lo faccio calcolare alla routine
527000981103     C                   EVAL      R96Cambio = 0
527100981103     C* dichiaro che la modalità è storica
527200981103     C                   EVAL      R96Modo = '1'
527300981103     C                   ENDIF
527400951120     C*
527500951120     C                   CALL      'NDCR96R'
527600951120     C                   PARM                    NDCR96DS
527700951120     C*
527800991008D0254C                   EVAL      H1RitAcc0 = '0'
527900951120     C                   IF        R96Err = *off
528000951120     C                   Z-ADD     R96DaVers     S1Ritenuta
528100981022     C                   Z-ADD     R96DaVersD    H1RitenutD
528200960515     C                   Z-ADD     R96Per        H1PercRit
528300991008D0254C* se la ritenuta reperita è 0 come la percentuale
528400991008  "  C                   IF        R96DaVers = 0 and
528500991008  "  C                             R96DaVersD = 0
528600991008  "  C* siamo nel caso di una ritenuta a % 0 e quindi deve generare
528700991008  "  C* lo stesso una scrittura
528800991008  "  C                   EVAL      H1RitAcc0 = '1'
528900991008  "  C                   ENDIF
529000991008D0254C*
529100951120     C                   ENDIF
529200951120     C*
529300951120     C                   ENDSR
529400951110     C************************************************************
529500951110     C* Ripristino indicatori salvati x sflmsg
529600951110     C************************************************************
529700951113     C     SflMsgIndy    BEGSR
529800951110     C*
529900951113     C                   MOVEA     in(60)        in6098
530000951113     C                   MOVEA     in6098        *in(60)
530100951113     C*
530200951113     C                   ENDSR
530300951113     C************************************************************
530400951113     C* Riquadratura al livello di m.c.
530500951113     C************************************************************
530600951113     C     RiquadraMC    Begsr
530700951113     C*
530800951113     C* calcolo la squadratura
530900960308     C                   EVAL      Importo = ImpSeg - WImporto
531000000821C1393x*
531100000821  "  x* imposto il totale importo in conto = al totale di testata
531200000821C1393x****************** EVAL      WImporto = ImpSeg
531300951113     C*
531400951113     C* rileggo il sfl per modificare il 1o rcd con differenza cambio
531500951113     C* aggiungendo la squadratura dall'importo in conto in m.c.
531600951113     C* e alla differenza cambio
531700951113     C*
531800951113     C                   ReadC     FMTS1                                  21
531900951113     C*
532000951113     C* se il totale differenza cambio è = 0
532100951113     C* significa che (probabilmente) nessun rcd ha differenza cambio
532200951113     C* per cui, volendo risparmiare delle READC inutili, carico la
532300951113     C* squadratura nel primo rcd
532400000529D0529C* a meno che non abbia come divisa di partita la m.c.
532500951113     C*
532600000529D0529x* ciclo finché non trovo un rcd con differenza cambio
532700000529D0529C* ciclo finché non trovo una partita selezionata che non abbia
532800000529  "  C* come divisa la m.c. e che abbia già della differenza cambio
532900000529  "  C* oppure sul primo con divisa di partita diversa da m.c.,anche se
533000000529D0529C* senza differenza cambio, se il totale della diff.cambio è 0
533100040308     C     *IN21         DOWEQ     *OFF
533200951113     C*
533300951113     C* al primo rcd con differenza cambio <> 0
533400040308     C     S1DiffCam     IFNE      0
533500000529D0529C* purché non sia in m.c. (dovrebbe essere impossibile)
533600000529D0529C     S1Divisa      ANDNE     xscDiv
533700951113     C* oppure sul primo rcd se il totale differenza cambio = 0
533800960308     C     DIFFCAMATT    OREQ      0
533900960308     C     DIFFCAMPAS    ANDEQ     0
534000000529D0529C* purché non sia in m.c.
534100000529D0529C     S1Divisa      ANDNE     xscDiv
534200970307     C*
534300970307     C* se la partita ha già differenza cambio
534400040308     C     S1DiffCam     IFNE      0
534500040308     C                   SETON                                        21
534600040308     C* scarico la differenza cambio dal totale
534700040308     C                   SELECT
534800040308     C                   WHEN      S1Diffcam > 0 and S1Segno = XscDare
534900040308     C                   sub       S1Diffcam     WDiffCam
535000040308     C                   EVAL      DiffCamAtt = DiffCamAtt - S1DiffCam
535100040308     C                   WHEN      S1Diffcam < 0 and S1Segno = XscAvere
535200040308     C                   add       S1Diffcam     WDiffCam
535300040308     C                   EVAL      DiffCamAtt = DiffCamAtt + S1DiffCam
535400040308     C                   WHEN      S1Diffcam > 0 and S1Segno = XScAvere
535500040308     C                   add       S1Diffcam     WDiffCam
535600040308     C                   EVAL      DiffCamPas = DiffCamPas - S1DiffCam
535700040308     C                   WHEN      S1Diffcam < 0 and S1Segno = XscDare
535800040308     C                   sub       S1Diffcam     WDiffCam
535900040308     C                   EVAL      DiffCamPas = DiffCamPas + S1DiffCam
536000040308     C                   ENDSL
536100040308     C                   ENDIF
536200040308     C*
536300040308     C* agisco sull'importo in m.c. al cambio di testata e sull'eventuale
536400040308     C* differenza cambio
536500040308     C* (siccome la differenza cambio è data da importo in m.c. al cambio
536600040308     C*  di testata - importo in m.c. al cambio di partita, se aumuenta o
536700040308     C*  diminuisce l'importo in m.c. al cambio di testata si ha lo stesso
536800040308     C*  effetto sulla differenza cambio) :
536900040308     C* se il segno della partita è giusto
537000040308     C                   IF        S1Segno <> Segno045
537100040308     C* l'importo deve aumentare della squadratura
537200040308     C                   EVAL      H1Importo = H1Importo + Importo
537300040308     C* la differenza cambio segue l'importo
537400040308     C                   EVAL      S1DiffCam = S1DiffCam + Importo
537500040308     C* se il segno della partita è inverso
537600040308     C                   ELSE
537700040308     C* l'importo deve diminuire della squadratura
537800040308     C                   EVAL      H1Importo = H1Importo - Importo
537900040308     C* la differenza cambio segue l'importo
538000040308     C                   EVAL      S1DiffCam = S1DiffCam - Importo
538100040308     C                   ENDIF
538200040308     C*
538300040308     C* se la partita ha già differenza cambio
538400040308     C     S1DiffCam     IFNE      0
538500040308     c* ricarico la differenza cambio sul totale
538600040308     C                   SELECT
538700040308     C                   WHEN      S1Diffcam > 0 and S1Segno = XscDare
538800040308     C                   add       S1Diffcam     WDiffCam
538900040308     C                   EVAL      DiffCamAtt = DiffCamAtt + S1DiffCam
539000040308     C                   WHEN      S1Diffcam < 0 and S1Segno = XscAvere
539100040308     C                   sub       S1Diffcam     WDiffCam
539200040308     C                   EVAL      DiffCamAtt = DiffCamAtt - S1DiffCam
539300040308     C                   WHEN      S1Diffcam > 0 and S1Segno = XscAvere
539400040308     C                   sub       S1Diffcam     WDiffCam
539500040308     C                   EVAL      DiffCamPas = DiffCamPas + S1DiffCam
539600040308     C                   WHEN      S1Diffcam < 0 and S1Segno = XscDare
539700040308     C                   add       S1Diffcam     WDiffCam
539800040308     C                   EVAL      DiffCamPas = DiffCamPas - S1DiffCam
539900040308     C                   ENDSL
540000040308     C                   ENDIF
540100040308     C*
540200040308     C                   Z-ADD     0             Importo
540300040308     C                   ENDIF
540400040308     C*
540500040308     C* attivo sempre sflnxtchg
540600040308     C                   Move      *ON           *IN32
540700040308     C* imposto gli attributi del sfl
540800040308     C                   EXSR      AtrS1
540900040308     C*
541000040308     C                   UPDATE    FMTS1
541100040308     C                   Move      *OFF          *IN32
541200040308     C*
541300040308     C* leggo prossimo rcd dal sfl se non scaricata la squadratura
541400040308     C     Importo       IFNE      0
541500040308     C                   ReadC     FMTS1                                  21
541600040308     C                   ELSE
541700040308     C                   LEAVE
541800040308     C                   ENDIF
541900040308     C*
542000040308     C                   ENDDO
542100040308     C*
542200040308     C* siccome non ho fatto un ciclo completo di READC, ho il puntatore al
542300040308     C* sfl sposizionato per cui devo eseguire queste operazioni per fare in
542400040308     C* modo che al prossimo ciclo riparta da capo
542500040308     C* - leggo l'ultimo rcd del sfl
542600040308     C     WMax          CHAIN     FMTS1                              21
542700040308     C* - se il rcd è da aggiornare ne faccio l'update e ciò che ne consegue
542800040308     C                   IF        S1Selez <> *blank
542900951113     C                   Move      *ON           *IN32
543000981106     C                   EXSR      AtrS1
543100951113     C                   UPDATE    FMTS1
543200951113     C                   Move      *OFF          *IN32
543300040308     C                   ENDIF
543400951113     C* - poi faccio una lettura che sicuramente andrà a vuoto
543500951113     C                   ReadC     FMTS1                                  21
543600951113     C* così è riposizionato
543700000529     x*
543800000529     x* infine, aggiungo la squadratura al totale differenze cambio
543900000529D0529C*
544000000529  "  C* se l'importo differenza cambio non è stato scaricato
544100000529  "  C                   IF        Importo <> 0
544200000529  "  C* allora può essere accaduto che tutte le righe selezionate
544300000529  "  C* abbiano come divisa di partita la m.c.
544400000529  "  C* In questo caso emetto una finestra che avverte l'utente di ciò
544500000821D0529C                   EXSR      ErrRiquaMC
544600000821C1393x****************** SETON                                        8099
544700000821  "  x****************** EVAL      in(80)=*on
544800000821  "  C* altrimenti
544900000821  "  C                   ELSE
545000000821  "  C* imposto il totale importo in conto = al totale di testata
545100000821C1393C                   EVAL      WImporto = ImpSeg
545200000529D0529C                   ENDIF
545300951113     C*
545400951113     C                   ENDSR
545500000529D0529C************************************************************
545600000529  "  C* Gestione dell'errore nella riquadratura m.c.
545700000529  "  C************************************************************
545800000529D0529C     ErrRiquaMC    BEGSR
545900000821C1393C*
546000000821  "  C                   CLEAR                   XMSGDS
546100000821C1393C                   CLEAR                   XMSGTXT
546200000529     C*
546300000529     C* il primo msg ha dei dati variabili per cui lo tratto separatemante
546400000529     C                   Z-ADD     Importo       WDiffTot
546500000529     C                   MOVEL     WDiffTotDS    Msgdta
546600000529     C                   MOVEL     MsgId(5)      IDMSG
546700000529      *  variabile numerica necessaria al messaggio COSxxxx
546800000529     C                   CALLB     'XRTVM'
546900000529     C                   PARM                    IDMSG            27
547000000529     C                   PARM                    TXTMSG          100
547100000529     C                   PARM                    ERRMSG            1
547200000529     C                   PARM                    *omit
547300000529     C                   PARM                    *omit
547400000529     C                   PARM                    Msgdta          100
547500000529     C*
547600000529     C     ERRMSG        IFNE      *ON
547700000529     C                   MOVEL     TXTMSG        MsgT(1)
547800000529     C                   ELSE
547900000529     C                   MOVEL     *ALL' '       MsgT(1)
548000000529     C                   ENDIF
548100000529     C*
548200000529     C* secondo messaggio
548300000529     C                   MOVEL     MsgId(6)      IDMSG
548400000529     C                   CALLB     'XRTVM'
548500000529     C                   PARM                    IDMSG            27
548600000529     C                   PARM                    TXTMSG          100
548700000529     C                   PARM                    ERRMSG            1
548800000529     C*
548900000529     C     ERRMSG        IFNE      *ON
549000000529     C                   MOVEL     TXTMSG        MsgT(2)
549100000529     C                   ELSE
549200000529     C                   MOVEL     *ALL' '       MsgT(2)
549300000529     C                   ENDIF
549400000529     C*
549500000529     C* terzo messaggio
549600000529     C                   MOVEL     MsgId(7)      IDMSG
549700000529     C                   CALLB     'XRTVM'
549800000529     C                   PARM                    IDMSG            27
549900000529     C                   PARM                    TXTMSG          100
550000000529     C                   PARM                    ERRMSG            1
550100000529     C*
550200000529     C     ERRMSG        IFNE      *ON
550300000529     C                   MOVEL     TXTMSG        MsgT(3)
550400000529     C                   ELSE
550500000529     C                   MOVEL     *ALL' '       MsgT(3)
550600000529     C                   ENDIF
550700000529     C*
550800000529     C                   MOVEL     MSGT(1)       MSGMS1
550900000529     C                   MOVEL     MSGT(2)       MSGMS2
551000000529     C                   MOVEL     MSGT(3)       MSGMS3
551100000821C1393C*
551200000821  "  C                   EVAL      XTXT  = *blank
551300000821  "  C* reperisco i bottoni da passare a XMSGR
551400000821  "  C                   MOVEL     MsgId(8)      IDMSG
551500000821  "  C                   CALLB     'XRTVM'
551600000821  "  C                   PARM                    IDMSG            27
551700000821  "  C                   PARM                    TXTMSG          100
551800000821  "  C                   PARM                    ERRMSG            1
551900000821  "  C*
552000000821  "  C     ERRMSG        IFNE      *ON
552100000821  "  C                   EVAL      XTxt1 = %subst(TXTMSG :1 :15)
552200000821  "  C                   EVAL      XTxt2 = %subst(TXTMSG :16:15)
552300000821  "  C                   EVAL      XTxt3 = %subst(TXTMSG :31:15)
552400000821  "  C                   ENDIF
552500000821C1393C                   EVAL      XMsgTxt = XTxt
552600000529     C*
552700000529     C                   Z-ADD     0             MSGMSG
552800000821C1393x****************** MOVEL     'N'           MSGRSP
552900000821C1393C                   MOVEL     'S'           MSGRSP
553000000821C1393C                   MOVEL     '123'         MSGTPR
553100000529     C                   MOVEL     'S'           MSGEMV
553200000529     C                   Z-ADD     10            MSGRGP
553300000529     C                   MOVEL     'N'           MSGLCK
553400000529     C                   MOVEA     STATUS        STS
553500000529     C                   MOVEL     'N'           MSGCNL
553600000529     C                   MOVEL     'N'           MSGVID
553700000529     C                   MOVEL     'N'           MSGOPE
553800000529     C                   MOVEL     'N'           MSGSTP
553900000529     C                   CALL      'XMSGR'
554000000529     C                   PARM                    XMSGDS
554100000529     C                   PARM                    XMSGTXT         135
554200000821C1393C*
554300000821  "  C                   SELECT
554400000821  "  C* Ritorno alla lista
554500000821  "  C     MsgRtn        WHENEQ    '1'
554600000821  "  C* emetto errore e aspetto che l'utente corregga gli importi
554700000821  "  C* concordati
554800000821  "  C                   SETON                                        8099
554900000821  "  C                   EVAL      in(80)=*on
555000000821  "  C*
555100000821  "  C* proseguo anche se dato F6 ma non faccio nulla per quadrare
555200000821  "  C     MsgRtn        WHENEQ    '2'
555300000821  "  C*
555400000821  "  C* genero arrotondamento automatico
555500000821  "  C     MsgRtn        WHENEQ    '3'
555600000821  "  C* solo nel caso sia stato premuto F06
555700000821  "  C                   IF        $Tasto = F06
555800000821  "  C                   Z-ADD     Importo       Arrotondam
555900000821  "  C                   CALLB     'NDC540R'
556000000821  "  C                   PARM                    Arrotondam
556100000821  "  C                   PARM                    C1Kcc
556200000821  "  C                   PARM                    C1Ksc
556300000821  "  C                   PARM                    DtRif045
556400000821  "  C                   PARM                    SOP0IP
556500000821  "  C                   PARM                    SOC001
556600000821  "  C                   PARM                    ANG2IPDS
556700000821  "  C                   ENDIF
556800000821  "  C*
556900000821C1393C                   ENDSL
557000000529     C*
557100000529D0529C                   ENDSR
557200951018     C************************************************************
557300951120     C* Chiama la gestione cessioni
557400951018     C************************************************************
557500951120     C     CallCess      BEGSR
557600951113     C*
557700951120     C                   RESET                   NDC046DS
557800951120     C*
557900951120     C                   MOVE      '01'          Opz046
558000951120     C                   MOVE      C1Kcc         KccPres046
558100951120     C                   MOVE      C1Ksc         KscPres046
558200951120     C                   MOVE      C1KccKscD     KccDes046
558300951122     C                   MOVE      RegNrAsReg    NrAsReg046
558400951122     C                   MOVE      RegNrReg      NrReg046
558500951120     C                   MOVE      RegDtReg      DtReg046
558600951120     C                   MOVE      RegSerReg     SerReg046
558700951120     C                   MOVE      C1Divisa      Divisa046
558800951120     C                   Z-ADD     D1ImportD     ImpMax046
558900951120     C                   MOVE      CdDist0IP     CdDist046
559000970225     C                   MOVE      RegSys        Sys046
559100951120     C                   MOVEL     NDC046DS      Kpjbu
559200951120     C*
559300951120     C                   CALL      'NDC046R'
559400951120     C                   PARM                    Kpjba
559500951120     C                   MOVEL     Kpjbu         NDC046DS
559600951120     C*
559700951120     C* ritorno da PGM gestione
559800951120     C                   EXSR      Rtn046
559900951120     C*
560000951120     C                   ENDSR
560100951120     C************************************************************
560200951120     C* Ritorno da gestione cessioni
560300951120     C************************************************************
560400951120     C     Rtn046        BEGSR
560500951120     C*
560600951120     C* modo di ritorno
560700951120     C*
560800040308     C                   SELECT
560900951120     C* 1 = F3
561000951120     C     RET046        WHENEQ    '1'
561100951120     C*  non c'è
561200951120     C* 2 = F12
561300951120     C     RET046        WHENEQ    '2'
561400951120     C* blocco l'aggiornamento e ritorno al sfl
561500951120     C                   SETON                                        99
561600951120     C*
561700040308     C                   ENDSL
561800951120     C*
561900951120     C* operazione eseguite dal pgm chiamato
562000951120     C*
562100040308     C                   SELECT
562200951120     C* eseguito aggiornamento
562300951120     C     OPR046        WHENEQ    '1'
562400951120     C* passo al pgm multicodice l'importo delle cessioni per questo conto
562500951120     C                   Z-ADD     SelImp046     ImpCess045
562600951120     C*
562700040308     C                   ENDSL
562800951120     C*
562900951120     C* funzione non eseguibile per errore :
563000951120     C*
563100040308     C                   SELECT
563200951120     C* 1 = funzione richiamata chiusa in qualunque errore
563300951120     C     ERR046        WHENNE    '0'
563400951120     C* blocco l'aggiornamento e ritorno al sfl
563500951120     C                   SETON                                        99
563600951120     C*
563700040308     C                   ENDSL
563800951120     C*
563900951120     C                   ENDSR
564000951120     C************************************************************
564100951120     C* Aggiornamento di tutti i file collegati al c/anticipo
564200951120     C************************************************************
564300951120     C     Aggiorna      Begsr
564400951120     C*
564500951031     C* scrivo per ogni totale un movimento con il conto patrimoniale
564600951031     C                   EXSR      WrtMovPatr
564700951018     C*
564800951018     C* Leggo il sfl solo se ci sono rcd
564900951018     C                   ReadC     FMTS1                                  21
565000951018     C*
565100951018     C* esce se fine sfl
565200040308     C     *IN21         DOWEQ     *OFF
565300951204     C*
565400970321     C* elaboro la riga se :
565500951204     C* il flag di selezione è valorizzato
565600040308     C     S1Selez       IFNE      *blank
565700951018     C*
565800951030     C* importo in conto
565900951018     C*
566000951204     C* scrivo, per ogni partita, i grappoli per l'importo
566100951107     C                   EXSR      WrtGrapImp
566200951030     C*
566300951030     C* sconto
566400951030     C*
566500951030     C* se esiste uno sconto
566600951116     C                   IF        S1ScontoD <> 0
566700951031     C* scrivo per ogni partita i grappoli per lo sconto
566800951107     C                   EXSR      WrtGrapSco
566900951030     C                   ENDIF
567000951030     C*
567100951030     C* abbuono
567200951030     C*
567300951030     C* se esiste un abbuono
567400951030     C                   IF        S1AbbuonoD > 0
567500951031     C* scrivo per ogni partita i grappoli per l'abbuono
567600951108     C                   EXSR      WrtGrapAbb
567700951030     C                   ENDIF
567800951018     C*
567900951019     C* differenza cambi
568000951018     C*
568100951019     C* se esiste una differenza cambi
568200951018     C                   IF        S1DiffCam <> 0
568300951107     C* scrivo per ogni partita i grappoli di differenza cambi
568400951107     C                   EXSR      WrtGrapDCa
568500951018     C                   ENDIF
568600951108     C*
568700951108     C* sopravvenienza
568800951108     C*
568900951108     C* se esiste una sopravvenienza
569000951108     C                   IF        S1SopravvD <> 0
569100951108     C* scrivo per ogni partita il movimento e i suoi grappoli
569200951108     C                   EXSR      WrtMovSop
569300951108     C                   ENDIF
569400951120     C*
569500951120     C* ritenuta d'acconto
569600951120     C*
569700951120     C* se esiste una ritenuta d'acconto
569800951120     C                   IF        S1Ritenuta <> 0
569900991008D0254C* oppure è una ritenuta a % 0
570000991008D0254C                             or H1RitAcc0 = *on
570100951120     C* chiamo la routine che l'aggiorna
570200951120     C                   EXSR      CallWrtRit
570300951120     C                   ENDIF
570400951018     C*
570500951204     C                   ENDIF
570600951204     C*
570700951018     C                   ReadC     FMTS1                                  21
570800951018     C*
570900040308     C                   Enddo
571000951031     C*
571100951031     C* scrivo per ogni totale un movimento con il conto economico
571200951031     C                   EXSR      WrtMovEcon
571300951109     C*
571400951109     C* valorizzo il parametro di ritorno
571500951109     C*
571600951109     C                   EVAL      ImpSalD045 = D1ImportD
571700951109     C                   EVAL      ImpSal045 = D1Importo
571800000821C1393C* bisogna tenere conto anche dell'eventuale arrotondamento
571900000821  "  C* effettuato se c'era squadratura tra i totali in m.c.
572000000821C1393C                   EVAL      ImpSal045 = ImpSal045 + Arrotondam
572100991105D0286 * il parametro NRRMImp045 viene valorizzato soltanto se
572200991105  "   * il totale selezioni è diverso da zero
572300991105  "   *
572400991105D0286C                   if        D1ImportD <> *zeros
572500970321     C                   EVAL      NrRMImp045 = NrRigaMImp
572600991105D0286C                   endif
572700951018     C*
572800951018     C                   Endsr
572900950711     C************************************************************
573000951031     C* Scrivo i movimenti del conto patrimoniale
573100950711     C************************************************************
573200951031     C     WrtMovPatr    BEGSR
573300950711     C*
573400951031     C* scrivo un movimento totale di tutte le partita per l'importo
573500951031     C*
573600960412     C                   IF        D1ImportD <> 0
573700951019     C                   RESET                   ND007DS
573800951019     C*  campi variabili
573900951030     C                   EVAL      NrRigaM = 0
574000951030     C                   EVAL      Kcc007    = C1Kcc
574100951030     C                   EVAL      Ksc007    = C1Ksc
574200960131     C                   EVAL      KccCPU007 = KccCPD045
574300960131     C                   EVAL      KscCPU007 = KscCPD045
574400960703     C* se l'importo totale > 0 , il segno del movimento è = a quello
574500951107     C* principale
574600951107     C                   IF        D1Importo > 0
574700951107     C                   EVAL      ImportD007 = D1ImportD
574800951107     C                   EVAL      Importo007 = D1Importo
574900961009     C                   IF        SOP0IP   = 'D'
575000961009     C                   EVAL      Segno007  = 'D'
575100951030     C                   ELSE
575200961009     C                   EVAL      Segno007  = 'A'
575300951030     C                   ENDIF
575400960703     C* se l'importo totale < 0 , il segno del movimento è opposto a quello
575500951107     C* principale
575600951107     C                   ELSE
575700951107     C                   EVAL      ImportD007 = D1ImportD * -1
575800951107     C                   EVAL      Importo007 = D1Importo * -1
575900961009     C                   IF        SOP0IP   = 'D'
576000961009     C                   EVAL      Segno007  = 'A'
576100951107     C                   ELSE
576200961009     C                   EVAL      Segno007  = 'D'
576300951107     C                   ENDIF
576400951107     C                   ENDIF
576500951107     C*
576600951122     C* se gestione a cambio storico
576700951122     C                   IF        CmbSto045 = *on
576800951122     C* uso il cambio calcolato come rapporto tra la sommatoria degli
576900951122     C* importi in conto nella divisa di testata con la sommatoria  i
577000951122     C* degli importi in conto nella m.c.
577100951122     C                   EVAL      Cambio007 = D1Cambio
577200951122     C* altrimenti
577300951122     C                   ELSE
577400951122     C* uso il cambio di testata
577500951122     C                   EVAL      Cambio007 = C1Cambio
577600951122     C                   ENDIF
577700951019     C                   EVAL      Divisa007 = C1Divisa
577800040308D0206X*****              EVAL      CausRig007  = CAU0IP
577900991019  "  C* la causale delle righe principali (conti patrimoniali di
578000991019  "  C*  incasso o pagamento) non è quella di tabella (che è solo un
578100991019  "  C*  dft) ma quella che è stata scritta come causale di testata
578200991019  "  C*  su NDREG00F
578300991019D0206C                   EVAL      CausRig007  = REGCausTes
578400951018     C                   EXSR      CallWrtMOV
578500951031     C*
578600951031     C* salvo il nr.riga movimento
578700951031     C                   EVAL      NrRigaMImp = NrRigaM
578800970225     C*
578900960111     C* se esiste un rapporto bancario per il conto
579000960111     C                   IF        Banca045 <> *blank  and
579100960111     C                             Linea045 <> *blank
579200960111     C* scrivo anche la tesoreria del movimento
579300960111     C                   RESET                   ND014DS
579400960111     C*  campi variabili
579500960111     C                   Eval      BANCA014  = Banca045
579600960111     C                   Eval      LINEA014  = Linea045
579700960111     C                   Eval      CAUBAN014 = *blank
579800960111     C                   EXSR      CallWrtBMV
579900960111     C                   ENDIF
580000960412     C                   ENDIF
580100951018     C*
580200951031     C* scrivo un movimento totale di tutte le partita per lo sconto
580300951031     C*
580400951116     C                   IF        D1ScontoD <> 0
580500951030     C                   RESET                   ND007DS
580600951030     C*  campi variabili
580700951030     C                   EVAL      NrRigaM = 0
580800951115     C                   EVAL      Kcc007    = C1Kcc
580900951115     C                   EVAL      Ksc007    = C1Ksc
581000960131     C                   EVAL      KccCPU007 = KSC1IP
581100960131     C                   EVAL      KscCPU007 = SSC1IP
581200951107     C* se l'importo totale > 0 , il segno del movimento è opposto a quello
581300951107     C* principale
581400000921D0708X***                IF        D1Sconto > 0
581500000921D0708C                   IF        D1ScontoD > 0
581600951107     C                   EVAL      ImportD007 = D1ScontoD
581700951107     C                   EVAL      Importo007 = D1Sconto
581800961009     C                   IF        SOP0IP   = 'D'
581900961009     C                   EVAL      Segno007  = 'D'
582000951107     C                   ELSE
582100961009     C                   EVAL      Segno007  = 'A'
582200951107     C                   ENDIF
582300951107     C* se l'importo totale < 0 , il segno del movimento è uguale a quello
582400951107     C* principale
582500951107     C                   ELSE
582600951107     C                   EVAL      ImportD007 = D1ScontoD  * -1
582700951107     C                   EVAL      Importo007 = D1Sconto   * -1
582800961009     C                   IF        SOP0IP   = 'D'
582900961009     C                   EVAL      Segno007  = 'A'
583000951107     C                   ELSE
583100961009     C                   EVAL      Segno007  = 'D'
583200951107     C                   ENDIF
583300951107     C                   ENDIF
583400951107     C*
583500040308C0762C                   EVAL      Cambio007 = D1CamSco
583600990528C0762C                   EVAL      Divisa007 = D1DivSco
583700990211     C*
583800951109     C                   EVAL      CausRig007  = CSC1IP
583900951030     C                   EXSR      CallWrtMOV
584000960111     C*
584100960111     C* se esiste un rapporto bancario per il conto
584200960111     C                   IF        Banca045 <> *blank  and
584300960111     C                             Linea045 <> *blank
584400960111     C* scrivo anche la tesoreria del movimento
584500960111     C                   RESET                   ND014DS
584600960111     C*  campi variabili
584700960111     C                   Eval      BANCA014  = Banca045
584800960111     C                   Eval      LINEA014  = Linea045
584900960111     C                   Eval      CAUBAN014 = *blank
585000960111     C                   EXSR      CallWrtBMV
585100960111     C                   ENDIF
585200951031     C*
585300951031     C* salvo il nr.riga movimento
585400951031     C                   EVAL      NrRigaMSco = NrRigaM
585500951116     C                   ENDIF
585600951030     C*
585700951031     C* scrivo un movimento totale di tutte le partite per l'abbuono
585800951108     C* attivo
585900951031     C*
586000951108     C                   IF        AbbAttivo > 0
586100000921D0708C                             or AbbAttivoD > 0
586200951030     C                   RESET                   ND007DS
586300951030     C*  campi variabili
586400951030     C                   EVAL      NrRigaM = 0
586500951115     C                   EVAL      Kcc007    = C1Kcc
586600951115     C                   EVAL      Ksc007    = C1Ksc
586700960131     C                   EVAL      KccCPU007 = KAA1IP
586800960131     C                   EVAL      KscCPU007 = SAA1IP
586900961009     C                   EVAL      Segno007  = 'D'
587000960115     C                   EVAL      ImportD007 = AbbAttivoD
587100951108     C                   EVAL      Importo007 = AbbAttivo
587200040308C0941C                   EVAL      Cambio007 = D1CamAbb
587300990531C0941C                   EVAL      Divisa007 = D1DivAbb
587400951108     C                   EVAL      CausRig007  = CAA1IP
587500951030     C                   EXSR      CallWrtMOV
587600960111     C*
587700960111     C* se esiste un rapporto bancario per il conto
587800960111     C                   IF        Banca045 <> *blank  and
587900960111     C                             Linea045 <> *blank
588000960111     C* scrivo anche la tesoreria del movimento
588100960111     C                   RESET                   ND014DS
588200960111     C*  campi variabili
588300960111     C                   Eval      BANCA014  = Banca045
588400960111     C                   Eval      LINEA014  = Linea045
588500960111     C                   Eval      CAUBAN014 = *blank
588600960111     C                   EXSR      CallWrtBMV
588700960111     C                   ENDIF
588800951031     C*
588900951031     C* salvo il nr.riga movimento
589000951031     C                   EVAL      NrRigaMAbD = NrRigaM
589100951031     C                   ENDIF
589200951031     C*
589300951031     C* scrivo un movimento totale di tutte le partite per l'abbuono
589400951108     C* passivo
589500951031     C*
589600951108     C                   IF        AbbPassivo > 0
589700000921D0708C                             or AbbPassivD > 0
589800951031     C                   RESET                   ND007DS
589900951031     C*  campi variabili
590000951031     C                   EVAL      NrRigaM = 0
590100951115     C                   EVAL      Kcc007    = C1Kcc
590200951115     C                   EVAL      Ksc007    = C1Ksc
590300960131     C                   EVAL      KccCPU007 = KAP1IP
590400960131     C                   EVAL      KscCPU007 = SAP1IP
590500961009     C                   EVAL      Segno007  = 'A'
590600960115     C                   EVAL      ImportD007 = AbbPassivD
590700951108     C                   EVAL      Importo007 = AbbPassivo
590800040308C0941C                   EVAL      Cambio007 = D1CamAbb
590900990531C0941C                   EVAL      Divisa007 = D1DivAbb
591000951108     C                   EVAL      CausRig007  = CAP1IP
591100951031     C                   EXSR      CallWrtMOV
591200960111     C*
591300960111     C* se esiste un rapporto bancario per il conto
591400960111     C                   IF        Banca045 <> *blank  and
591500960111     C                             Linea045 <> *blank
591600960111     C* scrivo anche la tesoreria del movimento
591700960111     C                   RESET                   ND014DS
591800960111     C*  campi variabili
591900960111     C                   Eval      BANCA014  = Banca045
592000960111     C                   Eval      LINEA014  = Linea045
592100960111     C                   Eval      CAUBAN014 = *blank
592200960111     C                   EXSR      CallWrtBMV
592300960111     C                   ENDIF
592400951031     C*
592500951031     C* salvo il nr.riga movimento
592600951031     C                   EVAL      NrRigaMAbA = NrRigaM
592700951031     C                   ENDIF
592800951031     C*
592900951031     C* scrivo un movimento totale di tutte le partite per la differenza
593000951109     C* cambi attive
593100951031     C*
593200951109     C                   IF        DiffCamAtt > 0
593300951031     C                   RESET                   ND007DS
593400951031     C*  campi variabili
593500951031     C                   EVAL      NrRigaM = 0
593600951115     C                   EVAL      Kcc007    = C1Kcc
593700951115     C                   EVAL      Ksc007    = C1Ksc
593800960131     C                   EVAL      KccCPU007 = KDA2IP
593900960131     C                   EVAL      KscCPU007 = SDA2IP
594000961009     C                   EVAL      Segno007  = 'D'
594100951109     C                   EVAL      Importo007 = DiffCamAtt
594200951031     C                   EVAL      ImportD007 = 0
594300951109     C                   EVAL      Divisa007 = XscDiv
594400951031     C                   EVAL      Cambio007 = 0
594500951114     C                   EVAL      CausRig007  = CDA2IP
594600951031     C                   EXSR      CallWrtMOV
594700960111     C*
594800960111     C* se esiste un rapporto bancario per il conto
594900960111     C                   IF        Banca045 <> *blank  and
595000960111     C                             Linea045 <> *blank
595100960111     C* scrivo anche la tesoreria del movimento
595200960111     C                   RESET                   ND014DS
595300960111     C*  campi variabili
595400960111     C                   Eval      BANCA014  = Banca045
595500960111     C                   Eval      LINEA014  = Linea045
595600960111     C                   Eval      CAUBAN014 = *blank
595700960111     C                   EXSR      CallWrtBMV
595800960111     C                   ENDIF
595900951108     C*
596000951108     C* salvo il nr.riga movimento
596100951108     C                   EVAL      NrRigaMDCD = NrRigaM
596200951031     C                   ENDIF
596300951031     C*
596400951031     C* scrivo un movimento totale di tutte le partite per la differenza
596500951109     C* cambi passive
596600951031     C*
596700951109     C                   IF        DiffCamPas > 0
596800951031     C                   RESET                   ND007DS
596900951031     C*  campi variabili
597000951031     C                   EVAL      NrRigaM = 0
597100951115     C                   EVAL      Kcc007    = C1Kcc
597200951115     C                   EVAL      Ksc007    = C1Ksc
597300960131     C                   EVAL      KccCPU007 = KDP2IP
597400960131     C                   EVAL      KscCPU007 = SDP2IP
597500961009     C                   EVAL      Segno007  = 'A'
597600951109     C                   EVAL      Importo007 = DiffCamPas
597700951031     C                   EVAL      ImportD007 = 0
597800951109     C                   EVAL      Divisa007 = XscDiv
597900951031     C                   EVAL      Cambio007 = 0
598000951114     C                   EVAL      CausRig007  = CDP2IP
598100951031     C                   EXSR      CallWrtMOV
598200960111     C*
598300960111     C* se esiste un rapporto bancario per il conto
598400960111     C                   IF        Banca045 <> *blank  and
598500960111     C                             Linea045 <> *blank
598600960111     C* scrivo anche la tesoreria del movimento
598700960111     C                   RESET                   ND014DS
598800960111     C*  campi variabili
598900960111     C                   Eval      BANCA014  = Banca045
599000960111     C                   Eval      LINEA014  = Linea045
599100960111     C                   Eval      CAUBAN014 = *blank
599200960111     C                   EXSR      CallWrtBMV
599300960111     C                   ENDIF
599400951108     C*
599500951108     C* salvo il nr.riga movimento
599600951108     C                   EVAL      NrRigaMDCA = NrRigaM
599700951031     C                   ENDIF
599800951030     C*
599900951030     C                   ENDSR
600000951018     C************************************************************
600100951107     C* Scrivo i grappoli per l'importo di partita
600200951018     C************************************************************
600300951107     C     WrtGrapImp    Begsr
600400951019     C*
600500951108     C* scrivo una partita collegata al movimento globale degli importi
600600951107     C                   RESET                   ND018DS
600700951107     C*  campi variabili
600800951107     C                   Eval      NrRigaM     = NrRigaMImp
600900961009     C                   IF        S1Segno = XscDare
601000961009     C                   EVAL      Segno018  = 'A'
601100951107     C                   ELSE
601200961009     C                   EVAL      Segno018  = 'D'
601300951107     C                   ENDIF
601400951116     C                   Eval      Divisa018   = *blank
601500951109     C                   Eval      Importo018  = H1Importo
601600951107     C                   Eval      ImpDivM018  = S1ImportD
601700951107     C* se la divisa di partita è diversa da quella di testata
601800951107     C                   IF        S1Divisa <> C1Divisa
601900951107     C* bisogna passare anche l'importo concordato, l'importo in divisa
602000951107     C* di partita (da calcolare) e il cambio di partita
602100951107     C                   Eval      ImpDivC018  = S1ImpConc
602200951108     C                   EVAL      ImportD018  = S1ImpDivP
602300951107     C                   Eval      Cambio018   = S1CamConc
602400960424     C                   ENDIF
602500951107     C*
602600951107     C                   EXSR      CallWrtPAS
602700951030     C*
602800951030     C                   ENDSR
602900951107     C************************************************************
603000951107     C* Scrivo i grappoli per lo sconto di partita
603100951107     C************************************************************
603200951107     C     WrtGrapSco    Begsr
603300951107     C*
603400951108     C* scrivo una partita collegata al movimento globale degli sconti
603500951107     C                   RESET                   ND018DS
603600951107     C*  campi variabili
603700951115     C                   Eval      NrRigaM     = NrRigaMSco
603800961009     C                   IF        S1Segno = XscDare
603900961009     C                   EVAL      Segno018  = 'A'
604000951107     C                   ELSE
604100961009     C                   EVAL      Segno018  = 'D'
604200951107     C                   ENDIF
604300990528C0762C* se la divisa di partita è la stessa con cui scriivo il movimento
604400990528C0762C* di sconto
604500990528C0762C                   IF        S1Divisa = DivSconto
604600951116     C                   Eval      Divisa018   = *blank
604700951109     C                   Eval      Importo018  = H1Sconto
604800990528C0762C* l'importo in divisa del movimento è la stessa a video
604900951107     C                   Eval      ImpDivM018  = S1ScontoD
605000990528C0762C* se la divisa di partita è diversa da quella del movimento
605100040308  "  C* di sconto
605200040308  "  C                   ELSE
605300040308  "  C* in questo caso la divisa dello sconto DEVE essere la m.c.!!!
605400040308  "  C                   EVAL      Divisa018   = S1Divisa
605500040308  "  C                   EVAL      Importo018  = H1Sconto
605600040308  "  C* l'importo in divisa del movimento è l'importo in m.c.
605700040308  "  C                   Eval      ImpDivM018  = H1Sconto
605800040308  "  C* l'importo in divisa di partita è l'importo a video
605900040308  "  C                   Eval      ImportD018  = S1ScontoD
606000040308  "  C                   Eval      ImpDivC018  = S1ScontoD
606100990528C0762C                   ENDIF
606200951107     C*
606300951107     C                   EXSR      CallWrtPAS
606400951107     C*
606500951107     C                   ENDSR
606600951030     C************************************************************
606700951107     C* Scrivo i grappoli per l'abbuono di partita
606800951030     C************************************************************
606900951107     C     WrtGrapAbb    Begsr
607000951030     C*
607100951108     C* scrivo una partita collegata al movimento globale degli abbuoni
607200951108     C* (attivi o passivi)
607300951108     C                   RESET                   ND018DS
607400951108     C*  campi variabili
607500951108     C                   IF        S1SegAbb = 'A'
607600961009     C                   EVAL      Segno018  = 'D'
607700951108     C                   Eval      NrRigaM     = NrRigaMAbD
607800951108     C                   ELSE
607900961009     C                   EVAL      Segno018  = 'A'
608000951108     C                   Eval      NrRigaM     = NrRigaMAbA
608100951108     C                   ENDIF
608200990528C0941C* se la divisa di partita è la stessa con cui scriivo il movimento
608300990526  "  C* di abbuono
608400990528C0941C                   IF        S1Divisa = DivAbbuono
608500951116     C                   Eval      Divisa018   = *blank
608600951109     C                   Eval      Importo018  = H1Abbuono
608700990528C0941C* l'importo in divisa del movimento è la stessa a video
608800951108     C                   Eval      ImpDivM018  = S1AbbuonoD
608900990528C0941C* se la divisa di partita è diversa da quella del movimento
609000990519  "  C* di abbuono
609100990519  "  C                   ELSE
609200990519  "  C* in questo caso la divisa dell'abbuono DEVE essere la m.c.!!!
609300990519  "  C                   EVAL      Divisa018   = S1Divisa
609400990519  "  C                   EVAL      Importo018  = H1Abbuono
609500990519  "  C* l'importo in divisa del movimento è l'importo in m.c.
609600990519  "  C                   Eval      ImpDivM018  = H1Abbuono
609700990519  "  C* l'importo in divisa di partita è l'importo a video
609800990519  "  C                   Eval      ImportD018  = S1AbbuonoD
609900990519  "  C                   Eval      ImpDivC018  = S1AbbuonoD
610000990528C0941C                   ENDIF
610100951108     C*
610200951108     C                   EXSR      CallWrtPAS
610300951030     C*
610400951030     C                   ENDSR
610500951108     C************************************************************
610600960314     C* Scrivo i grappoli per la differenza cambio
610700951108     C************************************************************
610800951108     C     WrtGrapDCa    BEGSR
610900951108     C*
611000951108     C* scrivo una partita collegata al movimento globale delle differenze
611100951108     C* cambio (attive o passive)
611200951108     C                   RESET                   ND018DS
611300951108     C*  campi variabili
611400951108     C                   SELECT
611500961009     C                   WHEN      S1DiffCam > 0 and S1Segno = XscDare
611600951116     C                   Eval      Importo018  = S1DiffCam
611700951116     C                   Eval      ImpDivM018  = S1DiffCam
611800961009     C                   EVAL      Segno018  = 'D'
611900951108     C                   Eval      NrRigaM     = NrRigaMDCD
612000961009     C                   WHEN      S1DiffCam < 0 and S1Segno = XscAvere
612100951116     C                   Eval      Importo018  = S1DiffCam * -1
612200951116     C                   Eval      ImpDivM018  = S1DiffCam * -1
612300961009     C                   EVAL      Segno018  = 'D'
612400951108     C                   Eval      NrRigaM     = NrRigaMDCD
612500961009     C                   WHEN      S1DiffCam > 0 and S1Segno = XscAvere
612600951116     C                   Eval      Importo018  = S1DiffCam
612700951116     C                   Eval      ImpDivM018  = S1DiffCam
612800961009     C                   EVAL      Segno018  = 'A'
612900951108     C                   Eval      NrRigaM     = NrRigaMDCA
613000961009     C                   WHEN      S1DiffCam < 0 and S1Segno = XscDare
613100951116     C                   Eval      Importo018  = S1DiffCam * -1
613200951116     C                   Eval      ImpDivM018  = S1DiffCam * -1
613300961009     C                   EVAL      Segno018  = 'A'
613400951108     C                   Eval      NrRigaM     = NrRigaMDCA
613500951108     C                   ENDSL
613600951116     C                   Eval      Divisa018   = *blank
613700990531C0941C* se la divisa di contabile è = a quella di partita
613800990531  "  C                   IF        C1Divisa = S1Divisa
613900990531  "  C* uso il cambio di testata
614000990531  "  C                   EVAL      Cambio018 = C1Cambio
614100990531  "  C* altrimenti
614200990531  "  C                   ELSE
614300990531  "  C* uso il cambio di partita, non l'originale ma l'inputato
614400990531  "  C                   EVAL      Cambio018 = S1CamConc
614500040308C0941C                   ENDIF
614600951108     C*
614700951108     C                   EXSR      CallWrtPAS
614800951108     C*
614900951108     C                   ENDSR
615000951108     C************************************************************
615100951108     C* Operazioni per scrivere il movimento e suoi grappoli
615200951108     C* per la sopravvenienza di partita con divisa <> dalla contabile
615300951108     C************************************************************
615400951108     C     WrtMovSop     BEGSR
615500951108     C*
615600951108     C* scrivo un movimento di conto patrimoniale
615700951108     C* per la sopravvenienza della singola partita
615800951108     C*
615900951108     C                   RESET                   ND007DS
616000951108     C*  campi variabili
616100951108     C                   EVAL      NrRigaM = 0
616200951109     C                   SELECT
616300951109     C* sopravvenienza attiva
616400961009     C                   WHEN      S1SopravvD > 0 and S1Segno = XscDare
616500961009     C                   EVAL      Segno007  = 'D'
616600951109     C                   EVAL      ImportD007 = S1SopravvD
616700951116     C                   EVAL      Importo007 = H1Sopravv
616800960705     C                   EVAL      KccCPU007 = KVA2IP
616900960705     C                   EVAL      KscCPU007 = SVA2IP
617000951114     C                   EVAL      CausRig007 = CVA2IP
617100961009     C                   WHEN      S1SopravvD < 0 and S1Segno = XscAvere
617200961009     C                   EVAL      Segno007  = 'D'
617300951109     C                   EVAL      ImportD007 = S1SopravvD * -1
617400951109     C                   EVAL      Importo007 = H1Sopravv  * -1
617500960705     C                   EVAL      KccCPU007 = KVA2IP
617600960705     C                   EVAL      KscCPU007 = SVA2IP
617700951114     C                   EVAL      CausRig007 = CVA2IP
617800951109     C* sopravvenienza passiva
617900961009     C                   WHEN      S1SopravvD > 0 and S1Segno = XscAvere
618000961009     C                   EVAL      Segno007  = 'A'
618100951109     C                   EVAL      ImportD007 = S1SopravvD
618200951109     C                   EVAL      Importo007 = H1Sopravv
618300960705     C                   EVAL      KccCPU007 = KVP2IP
618400960705     C                   EVAL      KscCPU007 = SVP2IP
618500951114     C                   EVAL      CausRig007 = CVP2IP
618600961009     C                   WHEN      S1SopravvD < 0 and S1Segno = XscDare
618700961009     C                   EVAL      Segno007  = 'A'
618800951109     C                   EVAL      ImportD007 = S1SopravvD * -1
618900951109     C                   EVAL      Importo007 = H1Sopravv  * -1
619000960705     C                   EVAL      KccCPU007 = KVP2IP
619100960705     C                   EVAL      KscCPU007 = SVP2IP
619200951114     C                   EVAL      CausRig007 = CVP2IP
619300951109     C                   ENDSL
619400951108     C*
619500951116     C                   EVAL      Kcc007    = C1Kcc
619600951116     C                   EVAL      Ksc007    = C1Ksc
619700951109     C                   EVAL      Cambio007 = S1CamConc
619800951108     C                   EVAL      Divisa007 = S1Divisa
619900951108     C                   EXSR      CallWrtMOV
620000960111     C*
620100960111     C* se esiste un rapporto bancario per il conto
620200960111     C                   IF        Banca045 <> *blank  and
620300960111     C                             Linea045 <> *blank
620400960111     C* scrivo anche la tesoreria del movimento
620500960111     C                   RESET                   ND014DS
620600960111     C*  campi variabili
620700960111     C                   Eval      BANCA014  = Banca045
620800960111     C                   Eval      LINEA014  = Linea045
620900960111     C                   Eval      CAUBAN014 = *blank
621000960111     C                   EXSR      CallWrtBMV
621100960111     C                   ENDIF
621200951108     C*
621300951108     C* scrivo una partita collegata al movimento della sopravvenienza
621400951108     C* (attiva o passiva)
621500951108     C                   RESET                   ND018DS
621600951108     C*  campi variabili
621700951108     C                   EVAL      Segno018  = Segno007
621800951109     C                   EVAL      Cambio018 = S1CamConc
621900951116     C                   Eval      Divisa018   = *blank
622000951116     C                   Eval      Importo018  = Importo007
622100951116     C                   Eval      ImpDivM018  = ImportD007
622200951108     C*
622300951108     C                   EXSR      CallWrtPAS
622400951108     C*
622500951108     C* scrivo un movimento di conto economico
622600951108     C* per la sopravvenienza della singola partita
622700951108     C*
622800951108     C                   RESET                   ND007DS
622900951108     C*  campi variabili
623000951108     C                   EVAL      NrRigaM = 0
623100951108     C*
623200951108     C                   SELECT
623300951116     C* sopravvenienza attiva
623400961009     C                   WHEN      S1SopravvD > 0  and  S1Segno = XscDare
623500961009     C                   EVAL      Segno007  = 'A'
623600951114     C                   EVAL      Kcc007    = KVA2IP
623700951114     C                   EVAL      Ksc007    = SVA2IP
623800951108     C                   EVAL      ImportD007 = S1SopravvD
623900951109     C                   EVAL      Importo007 = H1Sopravv
624000951114     C                   EVAL      CausRig007 = CVA2IP
624100951108     C* imposto i campi Regola e Voce, che servono per scrivere la riga di
624200951108     C* analitica
624300951114     C                   EVAL      Regola    = RVA2IP
624400951114     C                   EVAL      Voce      = VVA2IP
624500961009     C                   WHEN      S1SopravvD < 0  and  S1Segno = XscAvere
624600961009     C                   EVAL      Segno007  = 'A'
624700951116     C                   EVAL      Kcc007    = KVA2IP
624800951116     C                   EVAL      Ksc007    = SVA2IP
624900951116     C                   EVAL      ImportD007 = S1SopravvD * -1
625000951116     C                   EVAL      Importo007 = H1Sopravv  * -1
625100951116     C                   EVAL      CausRig007 = CVA2IP
625200951116     C* imposto i campi Regola e Voce, che servono per scrivere la riga di
625300951116     C* analitica
625400951116     C                   EVAL      Regola    = RVA2IP
625500951116     C                   EVAL      Voce      = VVA2IP
625600951116     C* sopravvenienza passiva
625700961009     C                   WHEN      S1SopravvD < 0  and  S1Segno = XscDare
625800961009     C                   EVAL      Segno007  = 'D'
625900951114     C                   EVAL      Kcc007    = KVP2IP
626000951114     C                   EVAL      Ksc007    = SVP2IP
626100951108     C                   EVAL      ImportD007 = S1SopravvD * -1
626200951109     C                   EVAL      Importo007 = H1Sopravv  * -1
626300951114     C                   EVAL      CausRig007 = CVP2IP
626400951108     C* imposto i campi Regola e Voce, che servono per scrivere la riga di
626500951108     C* analitica
626600951114     C                   EVAL      Regola    = RVP2IP
626700951114     C                   EVAL      Voce      = VVP2IP
626800961009     C                   WHEN      S1SopravvD > 0  and  S1Segno = XscAvere
626900961009     C                   EVAL      Segno007  = 'D'
627000951114     C                   EVAL      Kcc007    = KVP2IP
627100951114     C                   EVAL      Ksc007    = SVP2IP
627200951116     C                   EVAL      ImportD007 = S1SopravvD
627300951116     C                   EVAL      Importo007 = H1Sopravv
627400951114     C                   EVAL      CausRig007 = CVP2IP
627500951108     C* imposto i campi Regola e Voce, che servono per scrivere la riga di
627600951108     C* analitica
627700951114     C                   EVAL      Regola    = RVP2IP
627800951114     C                   EVAL      Voce      = VVP2IP
627900951108     C                   ENDSL
628000951108     C*
628100960705     C                   EVAL      KccCPU007 = C1Kcc
628200960705     C                   EVAL      KscCPU007 = C1Ksc
628300951109     C                   EVAL      Cambio007 = S1CamConc
628400951108     C                   EVAL      Divisa007 = S1Divisa
628500951108     C                   EXSR      CallWrtMOV
628600951108     C*
628700951108     C* scrivo una riga analitica con la sommatoria degli importi per segno
628800951108     C                   EVAL      KccMOA    = Kcc007
628900951108     C                   EVAL      KscMOA    = Ksc007
629000951116     C                   EVAL      DivisaMOA = S1Divisa
629100951108     C                   EXSR      CallWrtMOA
629200951108     C*
629300951108     C                   ENDSR
629400951031     C************************************************************
629500951120     C* Chiamo la routine che aggiorna la ritenuta d'acconto
629600951031     C************************************************************
629700951120     C     CallWrtRit    BEGSR
629800951031     C*
629900951120     C                   RESET                   NDCR95DS
630000981026     C                   MOVE      XscSoc        R95Societa
630100951120     C                   MOVE      C1Kcc         R95Kcc
630200951120     C                   MOVE      C1Ksc         R95Ksc
630300951120     C                   MOVE      S1NrPar       R95NrPar
630400951120     C     S1DtPar       IFGT      0
630500960201     C                   CALLB     'XDT4000'
630600951120     C     R95DtPar      PARM                    XDTAMG           10
630700951120     C                   PARM      S1DtPar       XDTGMA            6 0
630800951120     C                   PARM      4             XDTSTA            1 0
630900951120     C                   ELSE
631000951120     C                   MOVE      Dt0           R95DtPar
631100951120     C                   ENDIF
631200951122     C     S1DtSca       IFGT      0
631300960201     C                   CALLB     'XDT4000'
631400951122     C     R95DtSca      PARM                    XDTAMG           10
631500951122     C                   PARM      S1DtSca       XDTGMA            6 0
631600951122     C                   PARM      4             XDTSTA            1 0
631700951122     C                   ELSE
631800960610     C                   MOVE      REGDtReg      R95DtSca
631900951122     C                   ENDIF
632000951120     C                   MOVE      S1SerPar      R95SerPar
632100951120     C                   MOVE      RegNrReg      R95NrReg
632200951120     C                   MOVE      RegDtReg      R95DtReg
632300951120     C                   MOVE      RegSerReg     R95SerReg
632400951120     C                   MOVE      RegSys        R95Sys
632500951120     C                   MOVE      RegNrAsReg    R95NrAsReg
632600981026     C                   Z-ADD     S1Ritenuta    R95RAc
632700981022     C                   Z-ADD     H1RitenutD    R95RAcD
632800951120     C* se l'abbuono è attivo e il segno partita è dare
632900990617D0122C* (incasso di +  nel caso di saldaconto cliente   oppure
633000990617D0122C*  ricevo indietro di +  nel caso di saldaconto fornitore)
633100961009     C                   IF        S1SegAbb = 'A'  and  S1Segno = XscDare or
633200951120     C* oppure l'abbuono è passivo e il segno partita è avere
633300990617D0122C* (rendo di +  nel caso di saldaconto cliente   oppure
633400990617D0122C*  pago di +  nel caso di saldaconto fornitore)
633500990617     C                             S1SegAbb = 'P'  and  S1Segno = XscAvere
633600951120     C*  l'importo è > del (residuo-sconto) per cui l'abbuono va sottratto
633700951120     C                   EVAL      AbbuonoD = S1AbbuonoD * -1
633800951120     C* viceversa
633900951120     C                   ELSE
634000951120     C*  l'importo è < del (residuo-sconto) per cui l'abbuono va sommato
634100951120     C                   EVAL      AbbuonoD = S1AbbuonoD
634200951120     C                   ENDIF
634300960515     C                   EVAL      R95Per = H1PercRit
634400981022     C                   MOVEL     S1Divisa      R95Divisa
634500981104     C                   MOVEL     DtRif045      R95DtRfCam
634600030415c1647c                   if        DtVal045 <> Dt0
634700030415  "  C                   MOVE      DtVal045      R95DtFlu
634800030415  "  c                   else
634900030415  "  c* se non valorizzata la data valuta si usa data documento
635000030415  "  C                   MOVE      RegDtDoc      R95DtFlu
635100030415C1647c                   endif
635200981105     C                   Z-ADD     £NrDec        R95NrDec
635300981105     C                   Z-ADD     DVXNrDec      R95NrDecD
635400981105     C* se la divisa di partita è = a quella di testata
635500981105     C                   IF        S1Divisa = C1Divisa
635600981105     C* passo il cambio di testata
635700981105     C                   EVAL      R95Cambio = C1Cambio
635800981105     C* altrimenti
635900981105     C                   ELSE
636000981105     C* lo faccio calcolare alla routine
636100981105     C                   EVAL      R95Cambio = 0
636200981105     C                   ENDIF
636300981105     C*
636400951120     C                   MOVEL     NDCR95DS      Kpjbu
636500951120     C*
636600981105     C                   CALL      'NDCR95R'
636700951120     C                   PARM                    Kpjba
636800951120     C*
636900951120     C                   ENDSR
637000951120     C************************************************************
637100951120     C* Scrivo i movimenti del conto economico
637200951120     C************************************************************
637300951120     C     WrtMovEcon    BEGSR
637400951120     C*
637500990210     C* scrivo un movimento totale di tutte le partita per lo sconto
637600951031     C*
637700990210     C                   IF        D1ScontoD <> 0
637800951031     C                   RESET                   ND007DS
637900951031     C*  campi variabili
638000951031     C                   EVAL      NrRigaM = 0
638100951031     C                   EVAL      Kcc007    = KSC1IP
638200951031     C                   EVAL      Ksc007    = SSC1IP
638300960131     C                   EVAL      KccCPU007 = C1Kcc
638400960131     C                   EVAL      KscCPU007 = C1Ksc
638500951109     C* se l'importo totale > 0 , il segno del movimento è uguale a quello
638600951107     C* principale
638700000921D0708X***                IF        D1Sconto > 0
638800000921D0708C                   IF        D1ScontoD > 0
638900951107     C                   EVAL      ImportD007 = D1ScontoD
639000951107     C                   EVAL      Importo007 = D1Sconto
639100961009     C                   IF        SOP0IP   = 'D'
639200961009     C                   EVAL      Segno007  = 'A'
639300951107     C                   ELSE
639400961009     C                   EVAL      Segno007  = 'D'
639500951107     C                   ENDIF
639600951109     C* se l'importo totale < 0 , il segno del movimento è opposto a quello
639700951107     C* principale
639800951107     C                   ELSE
639900951107     C                   EVAL      ImportD007 = D1ScontoD  * -1
640000951107     C                   EVAL      Importo007 = D1Sconto   * -1
640100961009     C                   IF        SOP0IP   = 'D'
640200961009     C                   EVAL      Segno007  = 'D'
640300951107     C                   ELSE
640400961009     C                   EVAL      Segno007  = 'A'
640500951107     C                   ENDIF
640600951107     C                   ENDIF
640700990528C0762C*
640800990526  "  C* uso il cambio salvato sul piede
640900040308  "  C                   EVAL      Cambio007 = D1CamSco
641000990528C0762C                   EVAL      Divisa007 = D1DivSco
641100990210     C*
641200951109     C                   EVAL      CausRig007 = CSC1IP
641300951031     C                   EXSR      CallWrtMOV
641400951108     C*
641500951108     C* scrivo una riga analitica con la sommatoria degli importi per segno
641600951108     C                   EVAL      KccMOA    = KSC1IP
641700951108     C                   EVAL      KscMOA    = SSC1IP
641800951108     C* imposto i campi Regola e Voce, che servono per scrivere la riga di
641900951108     C* analitica, con i valori giusti per il tipo di differenza cambio
642000951108     C* reperiti da tabella 1IP
642100951108     C                   EVAL      Regola    = RSC1IP
642200951108     C                   EVAL      Voce      = VSC1IP
642300951116     C                   EVAL      DivisaMOA = S1Divisa
642400951108     C                   EXSR      CallWrtMOA
642500951116     C                   ENDIF
642600951031     C*
642700951031     C* scrivo un movimento totale di tutte le partite per l'abbuono
642800951109     C* in attivo
642900951031     C*
643000951108     C                   IF        AbbAttivo > 0
643100000921D0708C                             or AbbAttivoD > 0
643200951031     C                   RESET                   ND007DS
643300951031     C*  campi variabili
643400951031     C                   EVAL      NrRigaM = 0
643500951109     C                   EVAL      Kcc007    = KAA1IP
643600951109     C                   EVAL      Ksc007    = SAA1IP
643700960131     C                   EVAL      KccCPU007 = C1Kcc
643800960131     C                   EVAL      KscCPU007 = C1Ksc
643900961009     C                   EVAL      Segno007  = 'A'
644000960115     C                   EVAL      ImportD007 = AbbAttivoD
644100951108     C                   EVAL      Importo007 = AbbAttivo
644200040308C0941C                   EVAL      Cambio007 = D1CamAbb
644300990531C0941C                   EVAL      Divisa007 = D1DivAbb
644400990531     C                   EVAL      CausRig007 = CAA1IP
644500951031     C                   EXSR      CallWrtMOV
644600951108     C*
644700951108     C* scrivo una riga analitica con la sommatoria degli importi per segno
644800951109     C                   EVAL      KccMOA    = KAA1IP
644900951109     C                   EVAL      KscMOA    = SAA1IP
645000951108     C* imposto i campi Regola e Voce, che servono per scrivere la riga di
645100951108     C* analitica, con i valori giusti per il tipo di differenza cambio
645200951108     C* reperiti da tabella 1IP
645300951109     C                   EVAL      Regola    = RAA1IP
645400951109     C                   EVAL      Voce      = VAA1IP
645500951116     C                   EVAL      DivisaMOA = S1Divisa
645600951108     C                   EXSR      CallWrtMOA
645700951031     C                   ENDIF
645800951031     C*
645900951031     C* scrivo un movimento totale di tutte le partite per l'abbuono
646000951109     C* in passivo
646100951031     C*
646200951108     C                   IF        AbbPassivo > 0
646300000921D0708C                             or AbbPassivD > 0
646400951031     C                   RESET                   ND007DS
646500951031     C*  campi variabili
646600951031     C                   EVAL      NrRigaM = 0
646700951109     C                   EVAL      Kcc007    = KAP1IP
646800951109     C                   EVAL      Ksc007    = SAP1IP
646900960131     C                   EVAL      KccCPU007 = C1Kcc
647000960131     C                   EVAL      KscCPU007 = C1Ksc
647100961009     C                   EVAL      Segno007  = 'D'
647200960115     C                   EVAL      ImportD007 = AbbPassivD
647300951108     C                   EVAL      Importo007 = AbbPassivo
647400040308C0941C                   EVAL      Cambio007 = D1CamAbb
647500990531C0941C                   EVAL      Divisa007 = D1DivAbb
647600951108     C                   EVAL      CausRig007 = CAP1IP
647700951031     C                   EXSR      CallWrtMOV
647800951108     C*
647900951108     C* scrivo una riga analitica con la sommatoria degli importi per segno
648000951109     C                   EVAL      KccMOA    = KAP1IP
648100951109     C                   EVAL      KscMOA    = SAP1IP
648200951108     C* imposto i campi Regola e Voce, che servono per scrivere la riga di
648300951108     C* analitica, con i valori giusti per il tipo di differenza cambio
648400951108     C* reperiti da tabella 1IP
648500951109     C                   EVAL      Regola    = RAP1IP
648600951109     C                   EVAL      Voce      = VAP1IP
648700951116     C                   EVAL      DivisaMOA = S1Divisa
648800951108     C                   EXSR      CallWrtMOA
648900951031     C                   ENDIF
649000951031     C*
649100951031     C* scrivo un movimento totale di tutte le partite per la differenza
649200951109     C* cambi attive
649300951031     C*
649400951109     C                   IF        DiffCamAtt > 0
649500951031     C                   RESET                   ND007DS
649600951031     C*  campi variabili
649700951031     C                   EVAL      NrRigaM = 0
649800951114     C                   EVAL      Kcc007    = KDA2IP
649900951114     C                   EVAL      Ksc007    = SDA2IP
650000960131     C                   EVAL      KccCPU007 = C1Kcc
650100960131     C                   EVAL      KscCPU007 = C1Ksc
650200961009     C                   EVAL      Segno007  = 'A'
650300951109     C                   EVAL      Importo007 = DiffCamAtt
650400951031     C                   EVAL      ImportD007 = 0
650500951109     C                   EVAL      Divisa007 = XscDiv
650600951031     C                   EVAL      Cambio007 = 0
650700951116     C                   EVAL      CausRig007 = CDA2IP
650800951031     C                   EXSR      CallWrtMOV
650900951108     C*
651000951108     C* scrivo una riga analitica con la sommatoria degli importi per segno
651100951114     C                   EVAL      KccMOA    = KDA2IP
651200951114     C                   EVAL      KscMOA    = SDA2IP
651300951108     C* imposto i campi Regola e Voce, che servono per scrivere la riga di
651400951108     C* analitica, con i valori giusti per il tipo di differenza cambio
651500951108     C* reperiti da tabella 1IP
651600951114     C                   EVAL      Regola    = RDA2IP
651700951114     C                   EVAL      Voce      = VDA2IP
651800951116     C                   EVAL      DivisaMOA = XscDiv
651900951108     C                   EXSR      CallWrtMOA
652000951031     C                   ENDIF
652100951031     C*
652200951031     C* scrivo un movimento totale di tutte le partite per la differenza
652300951109     C* cambi passive
652400951031     C*
652500951109     C                   IF        DiffCamPas > 0
652600951031     C                   RESET                   ND007DS
652700951031     C*  campi variabili
652800951031     C                   EVAL      NrRigaM = 0
652900951114     C                   EVAL      Kcc007    = KDP2IP
653000951114     C                   EVAL      Ksc007    = SDP2IP
653100960131     C                   EVAL      KccCPU007 = C1Kcc
653200960131     C                   EVAL      KscCPU007 = C1Ksc
653300961009     C                   EVAL      Segno007  = 'D'
653400951109     C                   EVAL      Importo007 = DiffCamPas
653500951031     C                   EVAL      ImportD007 = 0
653600951109     C                   EVAL      Divisa007 = XscDiv
653700951031     C                   EVAL      Cambio007 = 0
653800951116     C                   EVAL      CausRig007 = CDP2IP
653900951031     C                   EXSR      CallWrtMOV
654000951108     C*
654100951108     C* scrivo una riga analitica con la sommatoria degli importi per segno
654200951114     C                   EVAL      KccMOA    = KDP2IP
654300951114     C                   EVAL      KscMOA    = SDP2IP
654400951108     C* imposto i campi Regola e Voce, che servono per scrivere la riga di
654500951108     C* analitica, con i valori giusti per il tipo di differenza cambio
654600951108     C* reperiti da tabella 1IP
654700951114     C                   EVAL      Regola    = RDP2IP
654800951114     C                   EVAL      Voce      = VDP2IP
654900951116     C                   EVAL      DivisaMOA = XscDiv
655000951108     C                   EXSR      CallWrtMOA
655100951031     C                   ENDIF
655200951031     C*
655300951031     C                   ENDSR
655400951030     C************************************************************
655500951030     C* Chiamata al modulo di scrittura movimento
655600951030     C************************************************************
655700951030     C     CallWrtMOV    Begsr
655800951030     C*
655900951019     C* campi fissi
656000951019     C                   MOVE      RegDtDoc      DtDoc007
656100951019     C                   EVAL      NrDoc007 = RegNrDoc
656200951019     C                   EVAL      SerDoc007 = RegSerDoc
656300951031     C                   EVAL      DtRfCam007 = DtRif045
656400951019     C*                  EVAL      FlgMov007 = ???
656500951019     C                   EVAL      MonCont007 = XscDiv
656600951019     C                   EVAL      PerImp007  = 999
656700951018     C*
656800951018     C                   EVAL      LenIn = %Size(ND007DS)
656900951018     C*
657000951018     C                   CALLB     'NDMVC011'
657100951023     C                   PARM                    NrRigaM
657200951019     C                   PARM      '03'          Operazione
657300951018     C                   PARM                    LenIn
657400951018     C                   PARM                    ND007DS
657500951025     C                   PARM      *off          GesMsg
657600951018     C                   PARM                    Esito
657700951018     C                   PARM                    *omit
657800951018     C                   PARM                    *omit
657900951018     C                   PARM                    *omit
658000951018     C                   PARM                    *omit
658100951018     C                   PARM                    *omit
658200951018     C*
658300951018     C                   ENDSR
658400960111     C************************************************************
658500960111     C* Chiamata al modulo di scrittura tesoreria
658600960111     C************************************************************
658700960111     C     CallWrtBMV    Begsr
658800960111     C*
658900960111     C* campi fissi
659000960111     C                   MOVEL     DtOpe045      DtOpe014
659100960111     C                   MOVEL     DtVal045      DtVal014
659200021030c1574c                   eval      Fungen014=FunGen045
659300051014D1879c                   eval      certif014=Spunta045
659400960111     C*
659500960111     C                   EVAL      LenIn = %Size(ND014DS)
659600960111     C*
659700960111     C                   CALLB     'NDMVC501'
659800040308     C                   PARM      '03'          Operazione
659900960111     C                   PARM                    NrRigaM
660000960111     C                   PARM                    LenIn
660100960111     C                   PARM                    ND014DS
660200960111     C                   PARM      *on           GesMsg
660300960111     C                   PARM                    Esito
660400960111     C                   PARM                    *omit
660500960111     C                   PARM                    *omit
660600960111     C                   PARM                    *omit
660700960111     C*
660800960111     C                   ENDSR
660900951018     C************************************************************
661000951019     C* Chiamata al modulo di scrittura partita
661100951018     C************************************************************
661200951019     C     CallWrtPAS    Begsr
661300951018     C*
661400950801     C                   Eval      LenIn  = %Size(ND018DS)
661500951019     C*
661600951019     C* campi fissi
661700950801     C                   IF        S1DtPar  > 0
661800960201     C                   CALLB     'XDT4000'
661900950914     C     DtPar018      PARM                    XDTAMG           10
662000950914     C                   PARM      S1DtPar       XDTGMA            6 0
662100950914     C                   PARM      4             XDTSTA            1 0
662200950801     C                   ELSE
662300950914     C                   MOVE      Dt0           DtPar018
662400950801     C                   ENDIF
662500950801     C                   Eval      Nrpar018    = S1NrPar
662600950801     C                   Eval      SerPar018   = S1SerPar
662700951115     C                   IF        S1DtSca  > 0
662800960201     C                   CALLB     'XDT4000'
662900951115     C     DtSca018      PARM                    XDTAMG           10
663000951115     C                   PARM      S1DtSca       XDTGMA            6 0
663100951115     C                   PARM      4             XDTSTA            1 0
663200951115     C                   ELSE
663300960201     C                   MOVE      REGDtReg      DtSca018
663400951115     C                   ENDIF
663500030415c1647c                   if        DtVal045 <> Dt0
663600020326C1506C                   MOVEL     DtVal045      DtFlu018
663700030415c1647c                   else
663800030415  "  c* se non valorizzata la data valuta si usa data documento
663900030415  "  C                   MOVEL     Regdtdoc      DtFlu018
664000030415c1647c                   endif
664100950801     C*
664200950801     C                   CALLB     'NDMVC401'
664300040308     C                   PARM      '03'          Operazione
664400951019     C                   PARM                    NrRigaM
664500951019     C                   PARM      0             NrRiga
664600950801     C                   PARM                    LenIn
664700950801     C                   PARM                    ND018ds
664800951025     C                   PARM      *on           GesMsg
664900950801     C                   PARM                    Esito
665000951019     C                   PARM                    *omit
665100951019     C                   PARM                    *omit
665200951019     C                   PARM                    *omit
665300951019     C*
665400951019     C                   ENDSR
665500951019     C************************************************************
665600951019     C* Chiamata al modulo di scrittura analitica
665700951019     C************************************************************
665800951019     C     CallWrtMOA    Begsr
665900951019     C*
666000951020     C                   CALLB     'NDMVC306'
666100040308     C                   PARM      '01'          Operazione
666200951020     C                   PARM                    NrRigaM
666300951020     C                   PARM                    Regola
666400951020     C                   PARM                    Voce
666500951020     C                   PARM      *blank        RifInt
666600951020     C                   PARM      0             SubNum
666700951020     C                   PARM      0             SubRig
666800951020     C                   PARM                    *omit
666900951108     C                   PARM                    KccMOA
667000951108     C                   PARM                    KscMOA
667100951020     C                   PARM                    RegSocieta
667200951020     C                   PARM                    RegDtReg
667300951020     C                   PARM                    XscCut
667400951020     C                   PARM                    Esito
667500951116     C                   PARM                    DivisaMOA
667600951019     C*
667700951019     C                   ENDSR
667800950707     C************************************************************
667900950707     C* Recupero i msg sono stati generati dal modulo chiamato
668000950707     C************************************************************
668100950721     C     RcvMsg        Begsr
668200950707     C*
668300950706     C                   CALL      'QMHRCVPM'
668400950706     C                   PARM                    QInfo
668500951116     C                   PARM      113           QLenMsg
668600950706     C                   PARM      'RCVM0100'    QFormat
668700950706     C                   PARM      '*'           QCallMsg
668800950706     C                   PARM      0             QCallStack
668900950706     C                   PARM      '*DIAG'       QMsgTyp
669000950706     C                   PARM                    QMsgKey
669100950706     C                   PARM      0             QWait
669200950721     C                   PARM      '*REMOVE'     QAction
669300950706     C                   PARM                    QErrCod
669400950706     C*
669500950721     C                   Endsr
669600950706     C************************************************************
669700950706     C* Gestione messaggi video
669800950706     C************************************************************
669900951019     C     GestErr       Begsr
670000950801     C                   Endsr
670100950801     C************************************************************
670200950801     C* Gestione messaggi non riconosciuti
670300950801     C************************************************************
670400950801     C     OtherMsg      Begsr
670500950801     C*
670600950801     C                   Eval      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
670700950801     C                   Move      Q2MsgId       IdMsg
670800950801     C                   CALLB     'XRTVM'
670900950801     C                   PARM                    IDMSG            27
671000950801     C                   PARM                    TXTMSG          100
671100950801     C                   PARM                    ERRMSG            1
671200950905     C                   PARM                    *omit
671300950905     C                   PARM                    *omit
671400950905     C                   PARM      Q2msgtxt      Msgdta          100
671500950801     C                   IF        ErrMsg <> *on
671600980925     C                   CAT       TxtMsg:1      C1TextMsg
671700951018     C                   Eval      In(96) = *on
671800950801     C                   Endif
671900950801     C*
672000950801     C                   Endsr
672100950801     C************************************************************
672200950801     C* Gestione operazioni dopo conferma
672300950801     C************************************************************
672400950801     C     PreparaOut    BEGSR
672500950801     C*
672600950801     C* segnalo al pgm chiamante
672700951018     C* l'avvenuta conferma che equivale e dirgli
672800951018     C* di non aver premuto F3/F12
672900951026     C                   MOVE      '0'           RET045
673000950801     C* e richiedo l'uscita da pgm
673100950801     C                   MOVE      *on           $fine
673200950801     C*
673300950801     C                   Endsr
673400980925     C************************************************************
673500980925     C* Recupero i msg sono stati generati dal modulo chiamato
673600980925     C************************************************************
673700980925     C     RestoreMsg    BEGSR
673800980925     C*
673900980925     C                   CALL      'QMHRCVPM'
674000980925     C                   PARM                    QInfo
674100980925     C                   PARM      113           QLenMsg
674200980925     C                   PARM      'RCVM0100'    QFormat
674300980925     C                   PARM      '*'           QCallMsg
674400980925     C                   PARM      0             QCallStack
674500980925     C                   PARM      '*DIAG'       QMsgTyp
674600980925     C                   PARM                    QMsgKey
674700980925     C                   PARM      0             QWait
674800980925     C                   PARM      '*REMOVE'     QAction
674900980925     C                   PARM                    QErrCod
675000980925     C*
675100980925     C                   ENDSR
675200960524     C******************************************************
675300960524     C* Messaggio EVAL che si rompe
675400960524     C* (cioè Residuo/ResiduoD >> dimensione Cambio)
675500960524     C******************************************************
675600960524     C     MsgEVALBrk    BEGSR
675700000821C1393C*
675800000821  "  C                   CLEAR                   XMSGDS
675900000821C1393C                   CLEAR                   XMSGTXT
676000971021     C*
676100981022     C* il primo msg ha dei dati variabili per cui lo tratto separatemante
676200981022     C                   RESET                   RifPartita
676300981022     C                   Z-ADD     S1NrPar       NrPar
676400981022     C                   MOVEL     S1DtPar       GGMMAA
676500981022     C                   MOVEL     GG            GGPar
676600981022     C                   MOVEL     MM            MMPar
676700981022     C                   MOVEL     AA            AAPar
676800981022     C                   MOVEL     S1SerPar      SerPar
676900981022     C                   MOVEL     MsgId(1)      IDMSG
677000981022     C                   CALLB     'XRTVM'
677100981022     C                   PARM                    IDMSG            27
677200981022     C                   PARM                    TXTMSG          100
677300981022     C                   PARM                    ERRMSG            1
677400981022     C                   PARM                    *omit
677500981022     C                   PARM                    *omit
677600981022     C                   PARM      RifPartita    Msgdta          100
677700981022     C*
677800981022     C     ERRMSG        IFNE      *ON
677900981022     C                   MOVEL     TXTMSG        MsgT(1)
678000981022     C                   ELSE
678100981022     C                   MOVEL     *ALL' '       MsgT(1)
678200981022     C                   ENDIF
678300981022     C*
678400981022     C* gli altri li reperisco ciclando sulla schiera
678500981022     C     2             DO        3             z
678600981007     C                   MOVEL     MsgId(z)      IDMSG
678700971021     C                   CALLB     'XRTVM'
678800971021     C                   PARM                    IDMSG            27
678900971021     C                   PARM                    TXTMSG          100
679000971021     C                   PARM                    ERRMSG            1
679100971021     C*
679200971021     C     ERRMSG        IFNE      *ON
679300981007     C                   MOVEL     TXTMSG        MsgT(z)
679400971021     C                   ELSE
679500981007     C                   MOVEL     *ALL' '       MsgT(z)
679600971021     C                   ENDIF
679700971021     C                   ENDDO
679800971021     C*
679900981022     C                   MOVEL     MSGT(1)       MSGMS1
680000971021     C                   MOVEL     MSGT(2)       MSGMS2
680100971021     C                   MOVEL     MSGT(3)       MSGMS3
680200960524     C*
680300960524     C                   EVAL      XTXT  = *blank
680400971021     C* reperisco i bottoni da passare a XMSGR
680500971021     C                   MOVEL     MsgId(4)      IDMSG
680600971021     C                   CALLB     'XRTVM'
680700971021     C                   PARM                    IDMSG            27
680800971021     C                   PARM                    TXTMSG          100
680900971021     C                   PARM                    ERRMSG            1
681000971021     C*
681100971021     C     ERRMSG        IFNE      *ON
681200971021     C                   EVAL      XTxt1 = %subst(TXTMSG :1 :15)
681300971021     C                   EVAL      XTxt2 = %subst(TXTMSG :16:15)
681400971021     C                   ENDIF
681500960524     C                   EVAL      XMsgTxt = XTxt
681600971021     C*
681700960524     C                   Z-ADD     0             MSGMSG
681800960524     C                   MOVEL     'S'           MSGRSP
681900960524     C                   MOVEL     '12'          MSGTPR
682000960524     C                   MOVEL     'S'           MSGEMV
682100960524     C                   Z-ADD     10            MSGRGP
682200960524     C                   MOVEL     'N'           MSGLCK
682300960524     C                   MOVEA     STATUS        STS
682400960524     C                   MOVEL     'N'           MSGCNL
682500960524     C                   MOVEL     'N'           MSGVID
682600960524     C                   MOVEL     'N'           MSGOPE
682700960524     C                   MOVEL     'N'           MSGSTP
682800960524     C                   CALL      'XMSGR'
682900960524     C                   PARM                    XMSGDS
683000960524     C                   PARM                    XMSGTXT         135
683100960524     C*
683200960524     C                   SELECT
683300960524     C* Interrompe caricamento
683400960524     C     MsgRtn        WHENEQ    '1'
683500960524     C* ritorno al pgm che ha dato il comando
683600960524     C                   EVAL      Ret045 = '2'
683700960524     C                   EXSR      EndPgm
683800960524     C* Carica senza la partita in errore
683900960524     C     MsgRtn        WHENEQ    '2'
684000960524     C* non carico questa partita
684100960524     C                   EVAL      $RcdOk = *off
684200960524     C*
684300960524     C                   ENDSL
684400960524     C*
684500960524     C                   ENDSR
684600010226C1458C******************************************************
684700010226  "  C* Messaggio che esiste una rata bloccata ma che non
684800010226  "  C* si può vedere perchè siamo in gestione partite e non scadenze
684900010226  "  C******************************************************
685000010226C1458C     MsgRataBk     BEGSR
685100010226     C*
685200010226     C                   CLEAR                   XMSGDS
685300010226     C                   CLEAR                   XMSGTXT
685400010226     C*
685500010226     C* il primo msg ha dei dati variabili per cui lo tratto separatemante
685600010226     C                   RESET                   RifPartita
685700010226     C                   Z-ADD     S1NrPar       NrPar
685800010226     C                   MOVEL     S1DtPar       GGMMAA
685900010226     C                   MOVEL     GG            GGPar
686000010226     C                   MOVEL     MM            MMPar
686100010226     C                   MOVEL     AA            AAPar
686200010226     C                   MOVEL     S1SerPar      SerPar
686300010226     C                   MOVEL     MsgId(9)      IDMSG
686400010226     C                   CALLB     'XRTVM'
686500010226     C                   PARM                    IDMSG            27
686600010226     C                   PARM                    TXTMSG          100
686700010226     C                   PARM                    ERRMSG            1
686800010226     C                   PARM                    *omit
686900010226     C                   PARM                    *omit
687000010226     C                   PARM      RifPartita    Msgdta          100
687100010226     C*
687200010226     C     ERRMSG        IFNE      *ON
687300010226     C                   MOVEL     TXTMSG        MsgT(9)
687400010226     C                   ELSE
687500010226     C                   MOVEL     *ALL' '       MsgT(9)
687600010226     C                   ENDIF
687700010305     C*
687800010305     C* gli altri li reperisco ciclando sulla schiera
687900010305     C     10            DO        11            z
688000010305     C                   MOVEL     MsgId(z)      IDMSG
688100010305     C                   CALLB     'XRTVM'
688200010305     C                   PARM                    IDMSG            27
688300010305     C                   PARM                    TXTMSG          100
688400010305     C                   PARM                    ERRMSG            1
688500010305     C*
688600010305     C     ERRMSG        IFNE      *ON
688700010305     C                   MOVEL     TXTMSG        MsgT(z)
688800010305     C                   ELSE
688900010305     C                   MOVEL     *ALL' '       MsgT(z)
689000010305     C                   ENDIF
689100010305     C                   ENDDO
689200010305     C*
689300010305     C                   MOVEL     MSGT(9)       MSGMS1
689400010305     C                   MOVEL     MSGT(10)      MSGMS2
689500010305     C                   MOVEL     MSGT(11)      MSGMS3
689600010226     C*
689700010226     C                   EVAL      XTXT  = *blank
689800010226     C                   EVAL      XMsgTxt = XTxt
689900010226     C*
690000010226     C                   Z-ADD     0             MSGMSG
690100010226     C*                  MOVEL     'S'           MSGRSP
690200010226     C*                  MOVEL     '12'          MSGTPR
690300010226     C                   MOVEL     'S'           MSGEMV
690400010226     C                   Z-ADD     10            MSGRGP
690500010226     C                   MOVEL     'N'           MSGLCK
690600010226     C                   MOVEA     STATUS        STS
690700010226     C                   MOVEL     'N'           MSGCNL
690800010226     C                   MOVEL     'N'           MSGVID
690900010226     C                   MOVEL     'N'           MSGOPE
691000010226     C                   MOVEL     'N'           MSGSTP
691100010226     C                   CALL      'XMSGR'
691200010226     C                   PARM                    XMSGDS
691300010226     C                   PARM                    XMSGTXT         135
691400010226     C*
691500010226C1458C                   ENDSR
691600950706     C************************************************************
691700950706     C* CONTROLLO ABILITAZIONE TASTI
691800950706     C* INPUT
691900950706     C* $TASTO =Tasto funzione premuto
692000950706     C* $FC = Tasti abilitati
692100950706     C* OUTPUT
692200950706     C* IN99 = ERrore
692300950706     C************************************************************
692400950721     C     CTRKEY        Begsr
692500950706     C*
692600950721     C                   Select
692700941205     C     $TASTO        WHENEQ    F01
692800960311     C                   z-add     01            posfun
692900941205     C     $FC(1)        IFEQ      '0'
693000941205     C                   SETON                                        99
693100950721     C                   Endif
693200941205     C     $TASTO        WHENEQ    F02
693300960311     C                   z-add     02            posfun
693400941205     C     $FC(2)        IFEQ      '0'
693500941205     C                   SETON                                        99
693600950721     C                   Endif
693700941205     C     $TASTO        WHENEQ    F03
693800960311     C                   z-add     03            posfun
693900941205     C     $FC(3)        IFEQ      '0'
694000941205     C                   SETON                                        99
694100950721     C                   Endif
694200941205     C     $TASTO        WHENEQ    F04
694300960311     C                   z-add     04            posfun
694400941205     C     $FC(4)        IFEQ      '0'
694500941205     C                   SETON                                        99
694600950721     C                   Endif
694700941205     C     $TASTO        WHENEQ    F05
694800960311     C                   z-add     05            posfun
694900941205     C     $FC(5)        IFEQ      '0'
695000941205     C                   SETON                                        99
695100950721     C                   Endif
695200941205     C     $TASTO        WHENEQ    F06
695300960311     C                   z-add     06            posfun
695400941205     C     $FC(6)        IFEQ      '0'
695500941205     C                   SETON                                        99
695600950721     C                   Endif
695700941205     C     $TASTO        WHENEQ    F07
695800960311     C                   z-add     07            posfun
695900941205     C     $FC(7)        IFEQ      '0'
696000941205     C                   SETON                                        99
696100950721     C                   Endif
696200941205     C     $TASTO        WHENEQ    F08
696300960311     C                   z-add     08            posfun
696400941205     C     $FC(8)        IFEQ      '0'
696500941205     C                   SETON                                        99
696600950721     C                   Endif
696700941205     C     $TASTO        WHENEQ    F09
696800960311     C                   z-add     09            posfun
696900941205     C     $FC(09)       IFEQ      '0'
697000941205     C                   SETON                                        99
697100950721     C                   Endif
697200941205     C     $TASTO        WHENEQ    F10
697300960311     C                   z-add     10            posfun
697400941205     C     $FC(10)       IFEQ      '0'
697500941205     C                   SETON                                        99
697600950721     C                   Endif
697700941205     C     $TASTO        WHENEQ    F11
697800960311     C                   z-add     11            posfun
697900941205     C     $FC(11)       IFEQ      '0'
698000941205     C                   SETON                                        99
698100950721     C                   Endif
698200941205     C     $TASTO        WHENEQ    F12
698300960311     C                   z-add     12            posfun
698400941205     C     $FC(12)       IFEQ      '0'
698500941205     C                   SETON                                        99
698600950721     C                   Endif
698700941205     C     $TASTO        WHENEQ    F13
698800960311     C                   z-add     13            posfun
698900941205     C     $FC(13)       IFEQ      '0'
699000941205     C                   SETON                                        99
699100950721     C                   Endif
699200941205     C     $TASTO        WHENEQ    F14
699300960311     C                   z-add     14            posfun
699400941205     C     $FC(14)       IFEQ      '0'
699500941205     C                   SETON                                        99
699600950721     C                   Endif
699700941205     C     $TASTO        WHENEQ    F15
699800960311     C                   z-add     15            posfun
699900941205     C     $FC(15)       IFEQ      '0'
700000941205     C                   SETON                                        99
700100950721     C                   Endif
700200941205     C     $TASTO        WHENEQ    F16
700300960311     C                   z-add     16            posfun
700400941205     C     $FC(16)       IFEQ      '0'
700500941205     C                   SETON                                        99
700600950721     C                   Endif
700700941205     C     $TASTO        WHENEQ    F17
700800960311     C                   z-add     17            posfun
700900941205     C     $FC(17)       IFEQ      '0'
701000941205     C                   SETON                                        99
701100950721     C                   Endif
701200941205     C     $TASTO        WHENEQ    F18
701300960311     C                   z-add     18            posfun
701400941205     C     $FC(18)       IFEQ      '0'
701500941205     C                   SETON                                        99
701600950721     C                   Endif
701700941205     C     $TASTO        WHENEQ    F19
701800960311     C                   z-add     19            posfun
701900941205     C     $FC(19)       IFEQ      '0'
702000941205     C                   SETON                                        99
702100950721     C                   Endif
702200941205     C     $TASTO        WHENEQ    F20
702300960311     C                   z-add     20            posfun
702400941205     C     $FC(20)       IFEQ      '0'
702500941205     C                   SETON                                        99
702600950721     C                   Endif
702700941205     C     $TASTO        WHENEQ    F21
702800960311     C                   z-add     21            posfun
702900941205     C     $FC(21)       IFEQ      '0'
703000941205     C                   SETON                                        99
703100950721     C                   Endif
703200941205     C     $TASTO        WHENEQ    F22
703300960311     C                   z-add     22            posfun
703400941205     C     $FC(22)       IFEQ      '0'
703500941205     C                   SETON                                        99
703600950721     C                   Endif
703700941205     C     $TASTO        WHENEQ    F23
703800960311     C                   z-add     23            posfun
703900941205     C     $FC(23)       IFEQ      '0'
704000941205     C                   SETON                                        99
704100950721     C                   Endif
704200941205     C     $TASTO        WHENEQ    F24
704300960311     C                   z-add     24            posfun
704400941205     C     $FC(24)       IFEQ      '0'
704500941205     C                   SETON                                        99
704600950721     C                   Endif
704700950721     C                   Endsl
704800941205     C*
704900950721     C                   Endsr
705000940131     C************************************************************
705100940131     C* OPERAZIONI INIZIALI
705200940131     C************************************************************
705300950721     C     *INZSR        Begsr
705400000125     C*
705500000117C1309C     *DMY          Move      Udate         ZDateISO
705600000117C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
705700940131     C*
705800940127     C* Reperimento parametri
705900940127     C*
706000940117     C     *ENTRY        PLIST
706100950807     C                   PARM                    KPJBA
706200951023     C                   PARM                    ANG0IPDS
706300951023     C                   PARM                    ANG1IPDS
706400951116     C                   PARM                    ANG2IPDS
706500951128     C                   PARM                    Sk2Partite
706600950406     C*
706700940506     C* Reperimento tasti di funzione
706800940506     C*
706900950721     C                   Exsr      REPKEY
707000950801     C*
707100950801     C* reperisco le dimensioni delle variabili che conterranno i tasti
707200950801     C                   Eval      $S1L01 = %size(Z1Ke1)
707300951030     C***                Eval      $S1L02 = %size(Z1Ke2)
707400940127     C*
707500960311     C*
707600960311     C* Reperimento dati società
707700960311     C*
707800960311     C                   MOVEL     'SOC001'      TIPXSC
707900960311     C                   MOVEL     *BLANK        SOCXSC
708000960311     C                   EXSR      REPSOC
708100960311     C     RTNXSC        IFNE      '1'
708200960311     C                   MOVEL     XSOCDS        SOC001
708300960311     C                   ELSE
708400960311     C                   RETURN
708500960311     C                   ENDIF
708600960311     C*
708700960311     C*
708800960311     C* Richiamo routine per aggiunta tasti funzione personalizzati
708900960311     C* Video D1/D4/D5
709000960311     C                   CAllb     'X46USREXT'
709100960311     C                   PARM                    *omit
709200960311     C                   PARM                    *omit
709300960311     C                   PARM                    *omit
709400960311     C                   PARM                    *omit
709500960311     C                   PARM                    *omit
709600960311     C                   PARM                    $FX1
709700960311     C                   PARM                    $FC1
709800960311     C                   PARM                    $PGF1
709900960311     C                   PARM                    DsPgm
710000960311     C                   PARM                    xsccut
710100960311     C                   PARM                    xscgrp
710200960311     C*
710300950721     C                   Endsr
710400940223     C************************************************************
710500940223     C* INIZIALIZZAZIONE VARIABILI
710600940223     C************************************************************
710700950721     C     Inzvar        Begsr
710800951124     C*
710900951124     C* Reperimento dati società
711000951124     C*
711100040308     C                   Movel     'SOC001'      TIPXSC
711200951124     C                   Movel     *BLANK        SOCXSC
711300951124     C                   Exsr      REPSOC
711400951124     C     RTNXSC        IFNE      '1'
711500040308     C                   Movel     XSOCDS        SOC001
711600951124     C                   ELSE
711700951124     C                   Move      *ON           $FINE
711800951124     C                   Exsr      ENDPGM
711900951124     C                   Endif
712000951020     C*
712100940223     C* Variabili per gestione videate
712200940223     C*
712300950721     C                   Move      *OFF          $FINE
712400950721     C                   Move      *OFF          $ESCI
712500950809     C                   MOVE      *off          $EndFile
712600940223     C*
712700950728     C                   Exsr      DefOpz
712800950721     C                   Move      *on           $InzS1
712900950727     C                   Move      *hival        $Lastv
713000950728     C                   Z-ADD     0             $ULKS1            3 0
713100951018     C*
713200951018     C* Variabili varie
713300951117     C                   Z-ADD     0             ImpDivPar
713400951117     C                   Z-ADD     0             Importo
713500951117     C                   Z-ADD     0             ResiduoD
713600951117     C                   Z-ADD     0             ScontoD
713700951117     C                   Z-ADD     0             AbbuonoD
713800951117     C                   Z-ADD     0             DiffCamAtt
713900951117     C                   Z-ADD     0             DiffCamPas
714000951117     C                   Z-ADD     0             AbbAttivo
714100951117     C                   Z-ADD     0             AbbPassivo
714200960115     C                   Z-ADD     0             AbbAttivoD
714300960115     C                   Z-ADD     0             AbbPassivD
714400991105D0286C                   Z-ADD     0             NrRMImp045
714500980114     C                   EVAL      WEXF = *off
714600950801     C*
714700990803C1209C* Data odierna
714800990803C1209C     *Eur          Move      *Date         Oggi
714900990803     C*
715000950801     C* Valorizzazione campi univoci testate
715100950801     C*
715200950801     C                   CLEAR                   FMTT1
715300950801     C                   MOVEL     KNSIF         NOMSIF
715400950801     C                   MOVEL     XSCDSI        NOMDIT
715500950801     C                   MOVEL     DSPGM         NOMPGM
715600941104     C*
715700941104     C* definisco qual'è la prima videata
715800950721     C                   Move      'S1'          $GEST
715900951120     C*
716000951120     C* Inizializzazione/Decodifica prima videata
716100951120     C*
716200951120     C                   EXSR      INZS1
716300951120     C                   MOVE      *OFF          $INZS1
716400951120     C*
716500951120     C* ulteriori definizioni dei tasti di funzione
716600951120     C*
716700951120     C                   EXSR      UltOpz
716800951120     C*
716900950721     C                   Endsr
717000951024     C************************************************************
717100951024     C* INIZIALIZZAZIONE VARIABILI
717200951024     C* (effetto mini per ritorno da pgm chiamato)
717300951024     C************************************************************
717400951024     C     MiniInzVar    BEGSR
717500951024     C*
717600951024     C* Variabili per gestione videate
717700951024     C*
717800951024     C                   MOVE      *OFF          $FINE
717900951024     C*
718000951024     C                   ENDSR
718100950706     C************************************************************
718200950706     C* REPERIMENTO DATI SOCIETA'
718300950706     C************************************************************
718400950721     C     REPSOC        Begsr
718500950706     C*
718600950522     C                   CALLB     'XSOC'
718700941209     C                   PARM                    TIPXSC            6
718800941209     C                   PARM                    SOCXSC            3
718900941209     C                   PARM                    CDSXSC            9 0
719000941209     C                   PARM                    MODXSC            3
719100941209     C                   PARM      *BLANKS       RTNXSC            1
719200941209     C                   PARM                    XSOCDS
719300941209     C                   PARM                    KPJBA
719400941209     C*
719500950721     C                   Endsr
719600940131     C************************************************************
719700940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
719800940131     C************************************************************
719900950721     C     DEFOPZ        Begsr
720000940715     C*
720100940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
720200941018     C* con 1 o 0 a seconda della utilizzabilità o meno
720300940715     C* del tasto funzione correlato
720400950802     C*
720500950802     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
720600950802     C* di compilazione
720700950802     C                   RESET                   $FC1
720800950802     C                   RESET                   $FR1
720900950802     C*
721000950802     C                   Endsr
721100951120     C************************************************************
721200951120     C* ULTERIORI DEFINIZIONI OPZIONI UTILIZZABILI A PGM
721300951120     C************************************************************
721400951120     C     UltOpz        Begsr
721500951120     C*
721600951120     C* Se il conto in ingresso risulta un fornitore con gestione
721700951120     C* cessioni = SI e la divisa d'ingresso è la m.c.
721800951120     C                   IF        SNatura002 = 'F'          and
721900951120     C                             Ces0IP = *on              and
722000951120     C                             C1Divisa = XscDiv
722100990617     C* attivo F11=Cessioni+Conferma
722200951120     C                   EVAL      $FC1(11) = '1'
722300951120     C                   ELSE
722400951120     C                   EVAL      $FC1(11) = '0'
722500951120     C                   ENDIF
722600990617     C*
722700990617     C* se il conto in ingresso non è un cliente
722800990617     C                   IF        SNatura002 <> 'C'
722900990617     C* disattivo F20=Sconto condizionato
723000990617     C                   EVAL      $FC1(20) = '0'
723100990617     C                   ELSE
723200990617     C                   EVAL      $FC1(20) = '1'
723300990617     C                   ENDIF
723400951120     C*
723500951120     C                   Endsr
723600940506     C************************************************************
723700940506     C* REPERIMENTO TASTI DI FUNZIONE
723800940506     C************************************************************
723900950721     C     REPKEY        Begsr
724000950721     C                   MoveA     $FM1          $FM
724100950802     C                   MoveA     $FC1          $FC
724200950721     C                   Exsr      REPKKK
724300950721     C                   MoveA     $FX           $FX1
724400950721     C                   Endsr
724500940506     C************************************************************
724600940506     C* REPERIMENTO TASTI DI FUNZIONE
724700940506     C************************************************************
724800950721     C     REPKKK        Begsr
724900940506     C*
725000940506     C                   CLEAR                   $FX
725100940506     C     1             DO        24            X
725200940506     C*
725300940506     C     $FM(X)        IFNE      *BLANK
725400950802     C     $FC(X)        ANDNE     '0'
725500940506     C*
725600950721     C                   Movel     $FM(X)        IDMSG
725700950522     C                   CALLB     'XRTVM'
725800940506     C                   PARM                    IDMSG            27
725900940506     C                   PARM                    TXTMSG          100
726000940506     C                   PARM                    ERRMSG            1
726100940506     C*
726200940506     C     ERRMSG        IFNE      *ON
726300950721     C                   Movel     TXTMSG        $FX(X)
726400940506     C                   ELSE
726500950721     C                   Movel     *ALL' '       $FX(X)
726600950721     C                   Endif
726700940506     C*
726800950721     C                   Endif
726900940506     C*
727000950721     C                   Enddo
727100940506     C*
727200950721     C                   Endsr
727300940128     C************************************************************
727400950606     C* DEFINIZIONE KLIST
727500940128     C************************************************************
727600950721     C     DefKlist      Begsr
727700940128     C*
727800940223     C* klist
727900940223     C*
728000951017     C* ndppa03L
728100951017     C     K04PPA03      KLIST
728200951017     C                   KFLD                    PPASocieta
728300951017     C                   KFLD                    PPACtb
728400951017     C                   KFLD                    PPAKcc
728500951017     C                   KFLD                    PPAKsc
728600951018     C     K07PPA03      KLIST
728700951018     C                   KFLD                    PPASocieta
728800951018     C                   KFLD                    PPACtb
728900951018     C                   KFLD                    PPAKcc
729000951018     C                   KFLD                    PPAKsc
729100951018     C                   KFLD                    PPADtPar
729200951018     C                   KFLD                    PPANrPar
729300951018     C                   KFLD                    PPASerPar
729400010105C1458x*
729500010105  "  x* ndpps01L
729600010105  "  x***  K02PPS01      KLIST
729700010105  "  x***                KFLD                    PPSSys
729800010105C1458x***                KFLD                    PPSNrAsInt
729900940127     C*
730000950721     C                   Endsr
730100950630** TESTI TASTI DI FUNZIONE
730200950721                             01
730300951020PROMSG    *LIBL     KEY0002  02
730400940506PROMSG    *LIBL     KEY0003  03
730500940506PROMSG    *LIBL     KEY0004  04
730600940506PROMSG    *LIBL     KEY0005  05
730700940506PROMSG    *LIBL     KEY0006  06
730800950721                             07
730900950801PROMSG    *LIBL     KEY0008  08
731000950801PROMSG    *LIBL     KEY0009  09
731100940506PROMSG    *LIBL     KEY0010  10
731200951120PROMSG    *LIBL     KEY0117  11
731300940506PROMSG    *LIBL     KEY0012  12
731400950721                             13
731500990803PROMSG    *LIBL     KEY0137  14
731600960201PROMSG    *LIBL     KEY0053  15
731700951026PROMSG    *LIBL     KEY0045  16
731800950721                             17
731900960109                             18
732000960109PROMSG    *LIBL     KEY0122  19
732100990616PROMSG    *LIBL     KEY0187  20
732200950721                             21
732300950721                             22
732400950721                             23
732500940506PROMSG    *LIBL     KEY0024  24
732600950908**  ABILITAZIONE S1
7327009508020N  01
7328009512040N  02
7329009512042N  03
7330009512042N  04
7331009512042N  05
7332009508021N  06
7333009508020N  07
7334009510180N  08
7335009508021N  09
7336009508090N  10
7337009511201N  11
7338009512042N  12
7339009508020N  13
7340009908032N  14
7341009602011N  15
7342009508100N  16
7343009508020N  17
7344009601090N  18
7345009601091N  19
7346009906161N  20
7347009508020N  21
7348009508020N  22
7349009508020N  23   Impostare a '1' se si utilizzano opzioni
7350009508021N  24   Lasciare sempre impostata a '1' il valore della riga 24
735100950825**
735200971021PROMSG    *LIBL     COS1027
735300971021PROMSG    *LIBL     COS1028
735400971021PROMSG    *LIBL     COS1029
735500971021PROMSG    *LIBL     COS1211
735600000529PROMSG    *LIBL     COS1228
735700000529PROMSG    *LIBL     COS1229
735800000529PROMSG    *LIBL     COS1230
735900000821PROMSG    *LIBL     COS1231
736000010226PROMSG    *LIBL     COS1052
736100010226PROMSG    *LIBL     COS1053
736200010226PROMSG    *LIBL     COS1054
