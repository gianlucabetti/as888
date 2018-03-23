000100141016     /*PRM  dbgview(*source)
000200141016     /*END
000300141022      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
000400141022      *        C O N T R O L L O    T A R I F F E / O F F E R T E       *
000500141022      *                    P E R    C L I E N T E                       *
000600141022      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
000700141022
000800141022     H DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000900141022
001000010530      * TESTATA E DETTAGLIO TARIFFE
001100061122     FTNTAM01L  IF   E           K DISK    usropn
001200010530     FTNOFM01L  IF   E           K DISK    USROPN
001300010530     F                                     RENAME(TNTAM000:TNOFM000)
001400061122     FTITAD04L  IF   E           K DISK    usropn
001500061122     ftiofd01L  if   e           k disk    usropn
001600061122     f                                     rename(titad000:tiofd000)
001700010530      *
001800010530      * TESTATA E DETTAGLIO TARIFFE PARTICOLARI
001900061122     FTITPT01L  IF   E           K DISK    usropn
002000010530     FTIOPT01L  IF   E           K DISK    USROPN
002100010530     F                                     RENAME(TITPT000:TIOPT000)
002200061122     FTITPD01L  IF   E           K DISK    usropn
002300010629     FTIOPD01L  IF   E           K DISK    USROPN
002400010629     F                                     RENAME(TITPD000:TIOPD000)
002500010530      *
002600010530      * TARIFFE GIACENZE
002700061122     FTITGC01L  IF   E           K DISK    usropn
002800010621     FTIOGC01L  IF   E           K DISK    USROPN
002900010621     F                                     RENAME(TITGC000:TIOGC000)
003000010530      *
003100010530      * VISITE
003200091022     FTiVIS05L  IF   E           K DISK    usropn
003300010530      *
003400010530      * NOTE TARIFFE
003500010530     FTFNTC01L  IF   E           K DISK
003600010530      *
003700010530      * PIANO DEI CONTI
003800010530     FCNACO00F  IF   E           K DISK
003900010530     FCNIND00F  IF   E           K DISK
004000010530     FCNCLP00F  IF   E           K DISK
004100010530     FTFACO00F  IF   E           K DISK    USROPN
004200010530     F                                     RENAME(CNACO000:TFACO000)
004300010530     FTFIND00F  IF   E           K DISK    USROPN
004400010530     F                                     RENAME(CNIND000:TFIND000)
004500010530     FTFCLP00F  IF   E           K DISK    USROPN
004600010530     F                                     RENAME(CNCLP000:TFCLP000)
004700010530      *
004800010530      * FILE VARI
004900010530     FAZCPC03L  IF   E           K DISK
005000141017     ***fAZCMM01L  if   e           k disk
005100010530     FAZORG01L  IF   E           K DISK
005200010530     FTABEL00F  IF   E           K DISK
005300141017     ***FTNTBE01L  IF   E           K DISK
005400050209     fTivss02l  if   e           k disk
005500060816     ftipmg01l  if   e           k disk
005600141016     ***fCnabi00f  if   e           k disk
005700101015     ftisis01l  if   e           k disk
005800141022
005900141022      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
006000141022      *                 DEFINIZIONE SCHIERE                             *
006100141022      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
006200010530     D* Messaggi di errore
006300050324     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
006400010530     D*  TABELLE PROVINCIE E REGIONI
006500041220     D CCT             S              2    DIM(600)
006600041220     D SCT             S              2    DIM(600)
006700041220     D RCT             S              2    DIM(600)
006800041220     D NAR             S              3    DIM(600)
006900050204     d prvdes          s             20    dim(600)
007000050303     d noctdes         s              1    dim(600)
007100041220     D CRE             S              2    DIM(200)
007200041220     D DRE             S             20    DIM(200)
007300010530     D NAZ             S              3    DIM(20)
007400010601     D* CODIICI YTASSAZIONE DELLA LINEA DEL CLIENTE
007500041220     D CTL             S              2    DIM(600)
007600010530     D* SCHIERE PER TARIFFE PARTICOLARI
007700010530     D FTC             S              1    DIM(50)
007800010530     D DTC             S             25    DIM(50)
007900020108     D* ordinamento in stampa tariffe particolari
008000020108     D OST             S              2    DIM(50)
008100020108     D WOS             S              2    DIM(50)
008200020108     D TST             S              1    DIM(50)
008300020108     D CP              S              1    DIM(50)
008400020108     D* estensione tabella 1P per stampa
008500020108     D EDTC            S             31    DIM(50)
008600020108     D EDAC            S              1    DIM(50)
008700020109     D ESUD            S              1    DIM(50)
008800010601     D* SCHIERE PER LE NOTE
008900010601     D NOTE            S             78    DIM(4)
009000010606     D* --------------  SCHIERE TARIFFE IN DETTAGLIO ------------------------
009100010606     D* SCAGLIONI
009200010606     D SGL             S              7  2 DIM(18)
009300010606     D* INOLTRO
009400010606     D PRO             S              7  3 DIM(18)
009500010606     D* RAPPORTO PESO VOLUME
009600010606     D RPV             S              4  0 DIM(18)
009700010606     D*--------------- SCHIERE PER LA STAMPA -----------------
009800010606     D*"/" di divisione negli scaglioni da a
009900010606     D SGLB            S              1    DIM(18)
010000010606     D* SCAGLIONI senza decimali
010100010606     D SGLD            S              4  0 DIM(18)
010200010606     D SGLA            S              4  0 DIM(18)
010300010606     D* SCAGLIONI con 1 decimale
010400010606     D SG1D            S              3  1 DIM(18)
010500010606     D SG1A            S              3  1 DIM(18)
010600010606     D* SCAGLIONI con 2 decimali
010700010622     D SG2D            S              4  2 DIM(18)
010800010622     D SG2A            S              4  2 DIM(18)
010900080901     D* SCAGLIONI con 3 decimali
011000080901     D SG3D            S              4  3 DIM(18)
011100080901     D SG3A            S              4  3 DIM(18)
011200010606     D* DECODIFICA SOTTO GLI SCAGLIONI
011300010606     D ATF             S              7    DIM(18)
011400010607     D* ------------------- 6 SCAGLIONI PER LA STAMPA --------------------------
011500010607     D SSLB            S              1    DIM(6)
011600010607     D* SCAGLIONI SENZA DECIMALI
011700010607     D SSLD            S              4  0 DIM(6)
011800010607     D SSLA            S              4  0 DIM(6)
011900010607     D* SCAGLIONI CON 1 DECIMALI
012000010607     D SS1D            S              3  1 DIM(6)
012100010607     D SS1A            S              3  1 DIM(6)
012200010607     D* SCAGLIONI CON 2 DECIMALI
012300010622     D SS2D            S              4  2 DIM(6)
012400010622     D SS2A            S              4  2 DIM(6)
012500080901     D* SCAGLIONI CON 3 DECIMALI
012600080901     D SS3D            S              4  3 DIM(6)
012700080901     D SS3A            S              4  3 DIM(6)
012800010612     D* ------------------- 6 APPLICAZIONE TARIFFA FINITA ----------------------
012900010612     D STF             S              7    DIM(6)
013000010614     D* ------------------- 6 SCAGLIONI E DETTAGLI TARIFFARI -------------------
013100010614     D SSL             S              7  2 DIM(6)
013200080916     D SSLP            S              8  3 DIM(6)
013300010614     D TCT             S              7  3 DIM(6)
013400010614     D TMN             S              7  3 DIM(6)
013500010614     D TMX             S              7  3 DIM(6)
013600010614     D TIN             S              7  3 DIM(6)
013700010614     D* ------------------- SCHIERA DI COMODO ----------------------------------
013800010614     D COM             S              7    DIM(6)
013900010615     D CO4             S              4    DIM(6)
014000010614     D*  SKI TARIFFE X SCAGL. DI OGNI PROVINCIA(CTS) DELLA REGIONE
014100050616     D BIG             S             42    DIM(15)
014200050616     D BMN             S             42    DIM(15)
014300050616     D BMX             S             42    DIM(15)
014400050616     D BIN             S             42    DIM(15)
014500050616     D BSI             S              2    DIM(15)
014600050616     D BRG             S              1    DIM(15)
014700050616     d dprv            s             20    dim(15)
014800050616     d nodes           s              1    dim(15)
014900010614     D* SKIERE DI WORK
015000010614     D WIN             S             42    DIM(30)
015100010614     D WMN             S             42    DIM(30)
015200010614     D WMX             S             42    DIM(30)
015300050616     D WCT             S              3    DIM(15)
015400010614     D WTI             S             42    DIM(30)
015500010614     D* SKIERE DI STAMPA
015600141016     D PDE             S             29    DIM(30)
015700010614     D PAM             S              7  3 DIM(6)
015800010614     D PMN             S              7  3 DIM(6)
015900010614     D PMX             S              7  3 DIM(6)
016000010614     D PIN             S              7  3 DIM(6)
016100010614     D*     % PER SCAGLIONI A VALORE
016200010614     D AI2             S              1    DIM(6)
016300010615     D*     SCAGLIONI STAMPA RAPPORTO PESO VOLUME
016400010615     D SPV             S              4  0 DIM(18)
016500010618      *-------------------------------------------------------------------------
016600010618      *         SCHIERE SCAGLIONI DETTAGLIO TARIFFE PARTICOLARI
016700010618      *-------------------------------------------------------------------------
016800010618     D* --------------  SCHIERE TARIFFE IN DETTAGLIO ------------------------
016900010618     D* SCAGLIONI
017000010618     D*
017100080916     D SCP             S              8  3 DIM(18)
017200010619     D* TARIFFA PARTICOLARE DETTAGLIO
017300010621     D TAP             S              7  3 DIM(6)
017400010621     D TPN             S              7  3 DIM(6)
017500010621     D TPX             S              7  3 DIM(6)
017600010621     D AIP             S              1    DIM(6)
017700010620     D*
017800010621     D PER             S              1    DIM(6)
017900010619     D*
018000021204      *          SCHIERE PER STAMPA TARIFFE PARTICOLARI SU + RIGHE
018100021204     d TptA            s             31    Dim(50)
018200021204     d TptB            s             25    Dim(50)
018300021204     d Tptc            s             31    Dim(50)
018400010614     D*
018500031119     D*-------------------------------------------------------------------------
018600101015      * DATE SCATTI ISTAT
018700101015     D DIA             S              8  0 DIM(999)
018800060203
018900080609      * schiera filiali abilitate al lasciato avviso
019000080609     d sklav           s              3  0 dim(999)
019100010601      *-------------------------------------------------------------------------
019200010530     D* DS DEL FILE TABEL00F
019300141017     ***D DSCV          E DS
019400141017     ***D DS1H          E DS
019500010530     D DS1P          E DS
019600020109     D DSSP          E DS
019700010530     D DS2G          E DS
019800010618     D DS3IDP        E DS
019900010530     D DSCT          E DS
020000010530     D DS15          E DS
020100010531     D DSTR          E DS
020200141016     ***D DSFA          E DS
020300010614AI2  D DS4U          E DS
020400031119      *
020500010725     D* DS DEL FILE TNTBE00F
020600141016     ***D DTAA          E DS
020700010531     D*
020800010625     D* VALORI STANDARD AZIENDALI
020900010625     D DGEI          E DS
021000141016     *** * limite c/assegni
021100141016     ***d dlca          e ds
021200010531     D* DS flag operativi del file tariffe
021300010531     D DSTA01        E DS
021400010601     D* DS organigramma per poste
021500010601     D OG143         E DS
021600141017     *** * DS organigramma per codice regione
021700141017     ***d og147         e ds
021800020624      *--------
021900020624      * Ds di riferimento al file esterno AzUte00F
022000020624      *--------
022100020624     d AzuteDs       e ds                  ExtName(AzUte00F)
022200020624      *--------
022300020624      * Ds per dati organigramma
022400020624      *--------
022500020624     d DDatiUte      e ds
022600020624      *--------
022700020624      * Parametri per richiamo al pgm di controllo profilo utenti
022800020624      *--------
022900020624     d Tibs34Ds      e ds                  Inz
023000141017      *--------
023100141017      * Parametri di questo *pgm
023200141017      *--------
023300141017     d TNTA48ds      e ds
023400141017     D*--------
023500141017     D KPJBA         E DS                  inz
023600010531     D*
023700010625     D DSBS02        E DS                  EXTNAME(TIBS02DS)
023800010625     D*
023900141017     ***D fnlv45ds      E DS
024000141017     ***D fnlv41ds      E DS
024100141017     d Tibs10ds      e ds                  inz
024200141017     d dlav          e ds                  inz
024300141017     d tntbeds       e ds                  extname(tntbe00f)  inz
024400010530      *-------------------------------------------------------------------------
024500010601      * ds interne
024600141017      *
024700141017     ***D CDPKEY          DS
024800141017     ***D  CDPG                   1      3
024900141017     ***D  GGPG                   2      3
025000141017     ***D  CDLIN                  4      4
025100010627      *
025200010601      *-------------------------------------------------------------------------
025300010531      *
025400141017     ***D DATA_eur        S               D   DATFMT(*eur)
025500060816     d data_iso1       s               d   datfmt(*iso)
025600060816     d data_iso2       s               d   datfmt(*iso)
025700010531      *
025800141016     ***d wCMMcod         s                   like(CMMcod)   inz
025900010601     d cod             s                   like(tblcod)
026000141016     ***d costpg          s                   like(§fatpg)
026100010601     d ctrata          s                   like(§trata)
026200010531     D ds1ksc          s                   like(tamksc)
026300010531     D ds1ctr          s                   like(tamctr)
026400010531     D ds1prg          s                   like(tamprg)
026500010601     D notapl          s                   like(ntcapl)
026600010621     D notke1          s                   like(ntcnk1)
026700010621     D notke2          s                   like(ntcnk2)
026800010601     D nottnt          s                   like(ntctnt)
026900111103     D kcod            s                   like(tbecod)
027000010601     D key             s                   like(tblkey)
027100010601     D ksc             s                   like(acoksc)
027200010724     D oltre           s                   like(tpdsgl)
027300010531     D parsks          s                   like(tamksc)
027400060307     d savapl          s                   like(tptapl)
027500010618     D savftc          s                   like(tptftc)
027600010621     D savorp          s                   like(tpdorp)
027700010801     D savorp1         s                   like(tpdorp)
027800010621     D savsgl          s                   like(tpdsgl)
027900010806     D recsgl          s                   like(tpdsgl)
028000010604     D rotcap          s                   like(tadcap)
028100010604     D rotcts          s                   like(tadcts)
028200010604     D rotlnp          s                   like(tadlnp)
028300010604     D wrkcap          s                   like(tadcap)
028400010604     D wrkcts          s                   like(tadcts)
028500010626     D wrkftc          s                   like(tpdftc)
028600010604     D wrklnp          s                   like(tadlnp)
028700010626     D wrkorp          s                   like(tpdorp)
028800010626     D wrksgl          s                   like(tpdsgl)
028900010531      *-------------------------------------------------------------------------
029000021204     d atp1            s              2  0
029100021204     d atpc            s              2  0
029200010614     D b               s              2  0
029300021205     D blk10           s             10    INZ(*ALL'.')
029400030318     D blk12           s             13    INZ(*ALL'.')
029500030318     D blk21           s             21    INZ(*ALL'.')
029600030318     D blk25           s             25    INZ(*ALL'.')
029700021205     D blk36           s             36    INZ(*ALL'.')
029800021205     D blk56           s             56    INZ(*ALL'.')
029900010614     D c               s              2  0
030000010604      * se esiste il cap nel 1° scaglione metto 'S'
030100010604     D cap1scaglio     s              1
030200141016     ***d CntList         s              5i 0 Inz(*Zeros)
030300010615     D codorp          s              1
030400010615     D codreg          s              2
030500010615     D codsig          s              2
030600141016     d comSTD          s             31
030700141016     d comPMA          s              5  0
030800010601     D comfil          s              3  0
030900020109     D comftc          s              1
031000010613     D compro          s              1
031100010613     D comreg          s              2
031200010621     D com04           s              4
031300010614     D como29          s             29
031400010614     D como42          s             42
031500010614     D cmno42          s             42
031600010614     D cmxo42          s             42
031700010614     D cino42          s             42
031800141016     ***d conta           s              1  0
031900010608     D ctrcap          s              1
032000141016     ***D datastampa      s             10
032100141016     ***D desctr          s             13
032200050204     d desprv          s             20
032300141016     ***D desreg          s             14
032400010618     D de1tpg          s              9
032500010618     D de3tpg          s             13
032600141016     ***D dissca2         s             63
032700141016     ***D dissca3         s             50
032800141016     ***D dissca4         s             37
032900141016     ***D dissca5         s             24
033000141016     ***D dissca6         s             11
033100010608     D ds3um           s             20
033200010531     D ds6pr2          s              1
033300010621     D errmsg          s             78
033400010614     D h               s              2  0
033500010601     D i96dri          s              8  0
033600020108     D j               s              3  0
033700020108     D jj              s              3  0
033800020108     D jjj             s              3  0
033900010605     D lnpcli          s              2
034000010724     D mintas          s              3  0
034100010619     D num4            s              4  0
034200050303     d nostdes         s              1
034300020114     D oltrenodec      s             10  0
034400010601     D o96ver          s              5  0
034500010614     D p               s              2  0
034600010531     D k               s              3  0
034700010531     D k1              s              3  0
034800010618     D k2              s              3  0
034900010607     D k4              s              3  0
035000010622     D k5              s              3  0
035100010604     D Y               s              3  0
035200141017     ***D X               s              2  0
035300010531     D unoctr          s              1  0
035400010625     D recval          s              3  0
035500010604     D rotpro          s              1
035600010613     D rotreg          s              2
035700141016     ***D staddt          S             10
035800141016     ***D stadst          S             10
035900020624     d stalnp          s             20
036000010606     D stino           S              1
036100141016     ***D stpanno         s              4  0
036200141016     ***D stpmod          s              3  0
036300030318     D stpper          s              7
036400141016     ***D totino          s             15  3
036500141017     ***d totrig          s              5  0
036600010731     D unmis           s              3
036700030318     D valper          s              6  3
036800141016     ***d wagenzia        s             16
036900010615     D waro2           s              4  0
037000010615     D warf2           s              4  0
037100010615     D warl2           s              4  0
037200100506     D wata            s              5  0
037300100506     D wata1           s              6  1
037400100506     D wata2           s              7  2
037500141016     ***d wbanca          s             20
037600060925     d wctr1           s              1
037700051206     d wema84          s              1    Inz('0')
037800050526     d wema85          s              1    Inz('0')
037900050526     d wema87          s              1    Inz('0')
038000050526     d wema88          s              1    Inz('0')
038100090612     d wema89          s              1    Inz('0')
038200060816     d wgio            s              5  0
038300021204     d Witr            s              8  3
038400010618     D wnumsc          s              2  0
038500051006     d wnrscaglio      s              2  0
038600010625     D w_per           s              1
038700050930     d woktar          s              1    Inz('0')
038800021204     d WokTp           s              1    Inz('0')
038900141016     ***d wpos            s              2  0
039000141016     ***d wpos1           s              2  0
039100141016     ***d wpos2           s              2  0
039200041230     d wpsw            s              1    Inz('0')
039300050204     d wragprv         s              1    Inz('0')
039400010615     D wrpv            s             24
039500141016     D wrkdestpg       s             13
039600010604     D wrkreg          s              2
039700010604     D wrkpro          s              1
039800020624     D wrkslp          s             20
039900141016     ***D wrkprtlnp       s             80
040000010613     D wsgl72          S              7  2
040100080916     D wsgl83          S              8  3
040200011001     D w_ellimris      S              1
040300141016     D w_tarpar        S             83
040400141016     D w_tarparest     S             83
040500141016     ***d w_oklnp         s              1
040600141016     ***d w_okstp         s              1
040700010723     D w0010           S              1  0
040800010606     D w0020           S              2  0
040900141017     ***D w003a           S              3
041000010606     D w0030           S              3  0
041100010606     D w0050           S              5  0
041200051006     d xx              s              2  0
041300051006     d yy              s              2  0
041400080117     d xy              s              2  0
041500010606     D z               s              3  0
041600080609     d xxx             s              3  0
041700091112     d $trattativa     s              1n
041800101015     d $EoF            s              1n   inz(*off)
041900110216     d savk            s              3  0
042000021205      *-----------------
042100021205      * PARAMETRI DI PASSAGGIO AL PROGRAMMA DI "GIUSTIFICAZIONE" TESTO
042200021205      *-----------------
042300141017     ***d TxtInput        S            200    varying
042400141017     ***d Lungh           S              5i 0 inz(105)
042500141017     ***d TxtGiust        S            200    varying
042600141017     ***d TxtResto        S            200    varying
042700021205
042800010625     D* DEFINIZIONE COSTANTI
042900060817     D CSPE1           C                   CONST('solo sulle spese di sosta')
043000010625     D CSPE2           C                   CONST('su tutte le spese')
043100060817     D CSPE3           C                   CONST('su spese sosta,dossier, -
043200060817     D                                     varie')
043300060817     D CSPE12          C                   CONST('only on holding costs')
043400060703     D CSPE22          C                   CONST('on all costs')
043500060817     D CSPE32          C                   CONST('on holding, dossier, -
043600060817     D                                     various costs')
043700060817     D CSPE13          C                   CONST('uniquement sur les frais -
043800060817     D                                     d''arrêt')
043900060817     D CSPE23          C                   CONST('sur tous les frais')
044000060817     D CSPE33          C                   CONST('sur les frais d''arrêt, de -
044100060817     D                                     dossier, divers')
044200060817     D CSPE14          C                   CONST('nur auf Kosten für -
044300060817     D                                     Aufbewahrung')
044400060817     D CSPE24          C                   CONST('auf alle Kosten')
044500060817     D CSPE34          C                   CONST('uf Kosten für Aufbewahrung,-
044600060817     D                                     Bearbeitung,Verschiedenes')
044700140527     D Cost_m_gratis   C                   CONST(': GRATIS fino al 31/12/2015-
044800140528     D                                     . Euro 0,20 dal 1/1/2016.')
044900140528     D Cost_m_gratisI  C                   CONST(': FREE until 31/12/2015. Eu-
045000140528     D                                     ro 0,20 as of 1/1/2016.')
045100140528     D Cost_m_gratisF  C                   CONST(': GRATUIT jusqu''au 31/12/20-
045200140528     D                                     15. Euro 0,20 à partir du 1/1/2016.')
045300140528     D Cost_m_gratisD  C                   CONST(': KOSTENLOS bis zum 31.12.20-
045400140528     D                                     15. Euro 0,20 ab 01.01.2016.')
045500140218
045600140218       // -?DS campo TPTflo
045700140218     d dTPT01        e ds
045800091022
045900141017     ***itivis000
046000141017     ***i              visesi                      wisesi
046100091022
046200010530     C****************************************************************
046300010530     C*  RIEPILOGO INDICATORI
046400010530     C***************************************************************
046500010604     C* 02    - TARIFFA EUROEXPRESS
046600010618     C* 04    - TARIFFA PARTICOLARE A ZERO
046700010626     C* 06    - TARIFFA PARTICOLARE ISOLA
046800010618     C* 08    - STAMPA MINIMO TASSABILE PER TARIFFA A Q.LE
046900010801     C* 09    - TIPO PAGAMENTO A VALORE (TARIFFA %) TARIFFE PARTICOLARI
047000010626     C* 10    - LEGGO TARIFFE PARTICOLARI
047100061207      * 12    - STAMPA FUEL (tariffa "f ") A SCAGLIONI)
047200141017     ***C* 18    - STAMPA ARROTONDAMENTI
047300141016     ***C* 19    - STAMPA RAPPORTO PESO VOLUME
047400010626     C* 21    - MANDATO A VALORE VARIABILE
047500010626     C* 22    - ELEVAZIONE LIMITE RISARCIBILE
047600010626     C* 24    - MANDATO FITTIZIO
047700010531     C* 25    - TARIFFA DPD
047800141017     ***C* 26    - CONDIZIONA LA STAMPA DEL "PER" DI FIANCO ALLA VALIDITA' TARIF
047900141017     ***C* 27    - CONDIZIONA LA STAMPA DELL'ADDIZIONALE DI GESTIONE 1° riga
048000141016     ***C* 28    - CONDIZIONA LA STAMPA DELLE SPESE GIACENZA VARIE A SPEDIZIONE
048100141017     ***C* 29    - CONDIZIONA LA STAMPA DELL'ADDIZIONALE DI GESTIONE 2° riga
048200010626     C* 31    - INDICATORE DI COMODO
048300010626     C* 35    - INDICATORE DI COMODO
048400010606     C* 37    - SCAGLIONI CON UN SOLO DECIMALE
048500141017     ***C* 40/46 - CONDIZIONA LA STAMPA DEGLI SCAGLIONI
048600141017     ***C* 48    - STAMPO IL MINIMO
048700141017     ***C* 49    - STAMPO IL MASSIMO
048800141017     ***C* 51/56 - CONDIZIONA LA STAMPA DEGLI SCAGLIONI VUOTI
048900141017     ***C* 57    - STAMPO INOLTRO
049000141016     ***C* 61/66 - CONDIZIONA LA STAMPA DEL DETTAGLIO TARIFFARIO
049100141016     ***C* 72    - CONDIZIONA LA STAMPA DELL'ADDEBITO SPESE DI INCASSO
049200010628     C* 73    - CONDIZIONA LA STAMPA DEL RESPONSABILE TRASPORTI
049300141017     ***C* 75    - STAMPO MINIMO
049400010628     C* 77    - CONDIZIONA LA STAMPA DEL E.P.C.
049500141017     ***C* 78/79 - CONDIZIONA LA STAMPA DELLA TESTATA DEGLI SCAGLIONI
049600010627     C* 80    - OVERFLOW
049700010627     C* 81    - CONDIZIONA LA STAMPA DELLA TESTATA ASSICURAZIONI MERCE
049800091117      * 85    - Trattativa in stampa
049900141017     ***C* 86    - STAMPO MASSIMO
050000010531     C* 87    - TARIFFA A VALORE
050100020114     C* 88    - OLTRE        CON DECIMALI
050200020114     C* 89    - ARRONDAMENTI CON DECIMALI
050300010606     C* 91    - SE ACCESO SCAGLIONI CON DECIMALI
050400010530     C* 92/97 - STAMPA DATI DALLA DS1H
050500010530     C* 98    - ON PROVENGO RA OFFERTA
050600010530     C* 99    - INDICATORE GENERALE DI ERRORE
050700010530     C*****************************************************************
050800010531      *-------------------------------------------------------------------------
050900010531      *    T E S T A T A    S T A M P A    T A R I F F A
051000010531      *-------------------------------------------------------------------------
051100080609
051200080609     c/exec sql
051300080609     c+ set option dynusrprf = *owner, closqlcsr = *endmod
051400080609     c/end-exec
051500080609
051600080609      * carico tabella filiali attive al lasciato avviso
051700080609     c                   exsr      carlav
051800050930
051900050930     c                   eval      woktar = *Off
052000010531      *
052100010531      * RECUPERO IL CLIENTE
052200010601     C                   EXSR      SR_RECCLI
052300010601      *
052400010601      * SE 99 ACCESO ERRORE E VADO A FINE NON TROVATO  CLIENTE IN ANAGRAFICA
052500010601      *
052600010601     C                   IF        *IN99
052700010601     C                   GOTO      FINE
052800010601     C                   ENDIF
052900010601      *
053000010601      * DECODIFICO I DATI DEL CLIENTE
053100010601      *
053200010601     C                   EXSR      SR_DECANA
053300010601      *
053400010601      * RECUPERO NOTE TARIFFA/OFFERTA CLIENTE E RESPONSABILE TRASPORTI
053500010601      *
053600010601     C                   EXSR      SR_RECNOT
053700010601      *
053800010601      * DECODIFICO I DATI DEL PUNTO OPERATIVO
053900010601      *
054000010601     C                   EXSR      SR_DECPO
054100010601      *
054200010601      * aggancio testata tariffa / offerta
054300010621     c                   eval      ds1ksc = parsks
054400141016     c                   eval      ds1ctr = iTA48ctr
054500141016     c                   eval      ds1prg = iTA48prg
054600010601
054700061122     c  n98ds1ktm        chain     tntam000
054800061122     c   98ds1ktm        chain     tnofm000
054900010531
055000010531      * decodifica delle condizioni generali
055100010531
055200010531     c                   exsr      SR_DECTAM
055300020205
055400020205      * carico le tariffe particolari
055500020205     c                   exsr      Sr_Cartarp
055600141016     .*//.......................................................................
055700010531      *
055800010605      * elaboro dettaglio tariffario per recuperare scaglioni/inoltro/rpv
055900010604      *
056000010604     c                   exsr      SR_SCAGLIONI
056100050930      * se si sono vericati errori non continuo
056200050930     c                   if        *in99
056300050930     c                   goto      fine
056400050930     c                   endif
056500010606      *
056600010606      * verifico se devo stampare l'inoltro alla fine della tariffa xchè uguale
056700010606      * per tutti oppure no
056800010606     c                   exsr      SR_STINO
056900020214      * se si sono vericati errori non continuo
057000020214     c                   if        *in99
057100020214     c                   goto      fine
057200020214     c                   endif
057300010607      * elaboro e stampo il dettaglio tariffario
057400010606      *
057500010607     c                   exsr      SR_ELADET
057600010614      * se si sono vericati errori non continuo
057700010614     c                   if        *in99
057800010614     c                   goto      fine
057900010614     c                   endif
058000010628      *
058100010628      * stampo arrotondamenti
058200010628     c                   EXSR      SR_ARROTO
058300010618      * stampo i dati della testata tariffa
058400010618      *
058500141017     .*//.......................................................................
058600050930     c                   eval      woktar = *On
058700010618      * routine gestione tariffe particolari
058800010618     c                   exsr      SR_STATPT
058900021204      * routine stampa sk unica tariffe particolari
059000021204     c                   ExSr      Sr_StaTpt1
059100010625      * routine di gestione tariffe giacenza
059200010625     c                   exsr      SR_STATGC
059300141016     .*//.......................................................................
059400050103      *
059500050526      * Se offerta
059600050526      * Capitolo dei servizi ON LINE + E-MAIL
059700050526     c                   If        *In98
059800050526     c                   ExSr      Sr_Comunica
059900050526     c                   EndIf
060000141016     .*//.......................................................................
060100010614      *
060200010614     c     fine          tag
060300041229
060400010614     c                   seton                                        lr
060500010601     C*------------------------------------------------------------------------
060600010601     C*   SR_RECCLI  RECUPERO CLIENTE
060700010601     C*------------------------------------------------------------------------
060800010601      *
060900010601     C     SR_RECCLI     BEGSR
061000010601      *
061100010601      *
061200010601      * V I S I T A
061300010601      *
061400010601     C                   IF        *IN98
061500010601      *
061600141017     ***c                   if        i45ast = 'T'
061700141017     c                   if        iTA48cto = 'O'
061800141016     C     iTA48ksc      chain     tivis000
061900091022     c                   endif
062000010601      * recupero ANAGRAFICA CLIENTE
062100010601      * se cliente nuovo leggo nel file anagrafici visite
062200010601     C                   z-add     visnrv        ksc
062300010601     c     ds7kac        chain     tfaco000                           99
062400010601     c  n99ds7kac        chain     tfind000                           99
062500010601     c  n99ds7kac        chain     tfclp000                           99
062600010601      * se non trovato cerco nell'anagrafica clienti
062700010601     c                   if        *in99
062800010601     C                   z-add     visksc        ksc
062900010601     c     ds7kac        chain     cnaco000                           99
063000010601     c  n99ds7kac        chain     cnind000                           99
063100010601     c  n99ds7kac        chain     cnclp000                           99
063200010601     c                   end
063300010601      *
063400010601     c                   end
063500010601      *
063600010601      * RECUPERO IL CLIENTE
063700010601      *
063800010601      * T A R I F F A
063900010601      *
064000010601     C                   IF        not *IN98
064100010601     C                   z-add     parsks        ksc
064200010601     c     ds7kac        chain     cnaco000                           99
064300010601     c  n99ds7kac        chain     cnind000                           99
064400010601     c  n99ds7kac        chain     cnclp000                           99
064500010601     c                   endif
064600010601      * se il cliente sia in visita che in tariffa non è stato trovato
064700010601      * viene stampato l'errore
064800010601     c                   if        *in99
064900010601     c                   move      MSG(3)        errmsg
065000010601     c                   exsr      staerr
065100010622      *
065200141016     .*//.......................................................................
065300141017     c                   endif
065400010601      *
065500010601     C                   ENDSR
065600010601     C*------------------------------------------------------------------------
065700010601     C*   SR_DECANA  DECODIFICA DATI CLIENTI PER LA STAMPA
065800010601     C*------------------------------------------------------------------------
065900010601      *
066000010601     C     SR_DECANA     BEGSR
066100141016      *
066200141016     .*//.......................................................................
066300010601      *
066400010601      * --------- decodifico tipo pagamento contrassegno ---------------
066500010601      *
066600010601     c                   eval      cod = '4U'
066700010601     c                   movel(p)  clpfpc        key
066800141017     c     tabkeyl       chain     tabel
066900010601     c                   if        %found(tabel00f)
067000010601     c                   movel     tbluni        ds4u
067100141016     .*//.......................................................................
067200010601      *
067300010601     c                   endif
067400010601      *
067500010601     c                   ENDSR
067600010601     C*------------------------------------------------------------------------
067700010601     C*   SR_RECNOT  RECUPERA NOTE TARIFFE/OFFERTA CLIENTI
067800010601     C*------------------------------------------------------------------------
067900010601      *
068000010601     C     SR_RECNOT     BEGSR
068100010601      *
068200010601      * ---------- chiave note tariffe/offerta cliente ------------------
068300010601      *
068400010601      * visita
068500010601     c   98              eval      notapl = 'V'
068600010601      * tariffa
068700010601     c  N98              eval      notapl = 'C'
068800091112      * trattativa
068900091112     c                   if        $trattativa
069000091112     c                   eval      notapl = 'T'
069100091112     c                   endif
069200010625      *
069300010625     c                   clear                   b
069400010601      *
069500141017     c                   movel     dutKCI        notke1
069600010625     c                   move      parsks        notke1
069700010601      * note legate alla tariffa/offerta
069800010601     c                   movel     '10'          nottnt
069900141016     c                   movel     iTA48ctr      notke2
070000010601      *
070100141017     c     keyntc        setll     tfntc
070200010601     c                   do        *hival
070300141017     c     keyntc        reade     tfntc
070400010601
070500010601     c                   if        %eof(tfntc01l)
070600010601      * imposto la chiave senza il codice tariffa e mi riposiziono
070700010601     c                   clear                   notke2
070800141017     c     keyntc        setll     tfntc
070900010601     c                   leave
071000010601     c                   endif
071100010601
071200010601     c                   if        ntcflt = 'A'
071300010601      * se è annullato leggo il successivo
071400010601     c                   iter
071500010601     c                   else
071600010601
071700010601      * se invece non è annullato ed esiste mi riposiziono per il caricamento
071800010601      * della schiera di comodo delle note
071900141017     c     keyntc        setll     tfntc
072000010601     c                   leave
072100010601     c                   endif
072200010601
072300010601     c                   enddo
072400010601      * carico le note in schiera
072500010601     c                   do        *hival
072600141017     c     keyntc        reade     tfntc
072700010601
072800010601     c                   if        %eof(tfntc01l)
072900010601     c                   leave
073000010601     c                   endif
073100010601      * se annullato oppure è una nota interna leggo la successiva
073200010621     c                   if        ntcflt = 'A' or ntcsns <> 'S'
073300010601     c                   iter
073400010601     c                   endif
073500010601      *
073600010601     c                   add       1             B
073700010601     c                   movel     ntcrnt        note(b)
073800010601      * se b = 4 numero massimo di note da stampare vado a fine
073900010601     c                   if        b = 4
074000010601     c                   leave
074100010601     c                   endif
074200010601
074300010601     c                   enddo
074400010601
074500010601      * ------------ RESPONSABILE TRASPORTI ---------------------------------
074600091112
074700141016     .*//.......................................................................
074800010601
074900010601     c                   endsr
075000010601      *
075100010601     C*------------------------------------------------------------------------
075200010601     C*   SR_DECPO   DECODIFICA I DATI DEL PUNTO OPERATIVO
075300010601     C*------------------------------------------------------------------------
075400010601      *
075500010601     C     SR_DECPO      BEGSR
075600010601      *
075700010601      * Se visita e cliente nuovo recupero la filiale dai primi 3 campi del
075800010601      * codice commerciale
075900010601      *
076000010601     c                   if        *in98 and visksc = 0
076100010601     c                   movel     viscmm        comfil
076200010601     c                   endif
076300010601      * Se visita di un cliente esistente recupero il p.o. appartenenza
076400010601      * cliente
076500010601      *
076600010601     c                   if        *in98 and visksc > 0
076700010601     c                   movel     visksc        comfil
076800010601     c                   endif
076900010601      *
077000010601      * se tariffa recupero dal codice cliente
077100010601      *
077200010601     c                   if        not *in98
077300010601     c                   movel     parsks        comfil
077400010601     c                   endif
077500010601      *
077600010601      * decodifica
077700010601      *
077800141017     c     comfil        chain     azorg
077900010601      *
078000010601     C                   if        %found(azorg01l)
078100010601      * verifico se cliente Posta
078200010601     c                   movel     orgde3        og143
078300141017     *** * cerco il codice regione
078400141017     ***c                   movel     orgde7        og147
078500010601      *
078600010601     c                   else
078700010601     c                   clear                   orgcts
078800141017     ***c                   clear                   og147
078900010601      *
079000010601     c                   endif
079100060606
079200141016     .*//.......................................................................
079300010601      *
079400010601     c                   endsr
079500010601      *
079600010601     C*------------------------------------------------------------------------
079700010531     C*   SR_DECTAM  DECODIFICA DELLA TESTATA TARIFFA
079800010531     C*------------------------------------------------------------------------
079900010531      *
080000010531     C     SR_DECTAM     BEGSR
080100010627      *
080200010627     C                   movel     TAMFLO        DSTA01
080300010627      * se divisa diversa da euro vado a fine PGM
080400010627     c                   if        §tadiv <> 'EUR'
080500010627     c                   goto      fine
080600010627     c                   endif
080700020123      * se tariffa FEDEX  vado a fine PGM
080800020123     c                   if        §tafed =  'S'
080900020123     c                   goto      fine
081000020123     c                   endif
081100010531      *
081200141016     .*//.......................................................................
081300010531
081400010531      ** verifico se è una tariffa DPD
081500010531      *
081600010531     C                   eval      *in25 = (§tadpd = 'S')
081700010604      ** verifico se è una tariffa EUROEXPRESS
081800010604      *
081900010604     C                   eval      *in02 = (tamfie = 'E')
082000060606
082100141016     .*//.......................................................................
082200060606
082300010601      *
082400010601      * carico i codici tassazione del cliente dal cappario versione data
082500010601      * decorrenza tariffa in caso di stampa dei soli codici tassazione cliente
082600010601      *
082700010601     c                   z-add     tamddt        i96dri
082800010601     c                   call      'TISI96R'
082900010601     c                   parm                    i96dri
083000010601     c                   parm                    o96ver
083100010601      * aggancio tabella codici tassazione con quella del p.o. del cliente
083200010601      *
083300010601     c                   eval      cod = 'CT'
083400010601     c                   if        orgcts <> '  '
083500010601     c                   movel(p)  orgcts        key
083600010601     c                   else
083700010601     c                   movel(p)  *all'9'       key
083800010601     c                   endif
083900010601      *
084000141017     c     tabkeyl       chain     tabel
084100010601      *
084200010601     c                   if        %found(tabel00f)
084300010601     c                   movel     tbluni        dsct
084400010604      * recupero dal cappario i codici tassazione
084500010604     c                   z-add     1             y
084600141017     c     kcpc          setll     azcpc000
084700010604      *
084800010604     c                   do        *hival
084900141017     c     kcpc          reade     azcpc000
085000010604     c                   if        %eof(azcpc03l)
085100010604     c                   leave
085200010604     c                   endif
085300010604      *
085400010604     c                   if        cpcatb = ' '
085500010604     c     cpccts        lookup    ctl                                    31
085600010604      * carico
085700010604     c                   if        not *in31
085800010604     c                   movel     cpccts        ctl(y)
085900010604     c                   add       1             y
086000010604     c                   endif
086100010604      *
086200010604     c                   endif
086300010604      *
086400010604     c                   enddo
086500010604      *
086600010604     c                   endif
086700010604      *
086800010604      * decodifico la tariffa cliente
086900010604      *
087000141016     c                   movel     iTA48ctr      unoctr
087100010604     c                   movel(p)  unoctr        key
087200010604     c                   eval      cod = 'TR'
087300010604      *
087400141017     c     tabkeyl       chain     tabel
087500010604      *
087600010604     c                   if        %found(tabel00f)
087700010604     c                   movel     tbluni        dstr
087800010604      * valorizzo i campi descrittivi di stampa
087900010604     c                   movel     §trde3        ds3um
088000141016     .*//.......................................................................
088100010604      * salvo il minimo tassabile
088200010604     c                   eval      ctrata  = §trata
088300010604      *
088400010604     c                   endif
088500010801      * tariffa a valore (per stampare scaglioni senza decimali)
088600010604     c                   eval      *in87 = (unoctr = 4)
088700141016
088800141016     .*//.......................................................................
088900010604
089000010531     C                   ENDSR
089100020205
089200020205      *------------------------------------------------------------------------
089300020205      *   Sr_Cartarp  CARICO LE TARIFFE PARTICOLARI
089400020205      *------------------------------------------------------------------------
089500020205     C     Sr_Cartarp    begsr
089600020205
089700020205      * carico solo quelle che interessano la tariffa che sto stampando
089800020205     c                   eval      cod ='1P'
089900020205     c                   z-add     0             K
090000141017     c     tabk2l        setll     tabel
090100020205 b1  c                   do        *hival
090200141017     c     tabk2l        reade     tabel
090300020205      * Fine file
090400020205     c                   if        %eof(tabel00f)
090500020205     c                   leave
090600020205     c                   endif
090700020205      * escludo le annullate
090800020205     c                   if        tblflg <> ' '
090900020205     c                   iter
091000020205     c                   endif
091100080609      * se tariffa particolare c= lasciato avviso
091200080609      * per le filiali non abilitate, accetto cartello
091300080609      * non stampo
091400080609     c                   If        tblkey = 'c       '
091500051012     c                             and comfil <> 888
091600080609     c                   if        %lookup(comfil:sklav) = *zeros
091700080609     c                             and %lookup(999:sklav) = *zeros
091800051012     c                   iter
091900080609     c                   endif
092000051012     c                   endif
092100020205
092200020205     c                   movel     tbluni        ds1p
092300020205
092400020205      * Tariffa con servizio Poste escludo le tariffe particolari con
092500020205      * flag §1PFPT = 'N'
092600020205     c                   if        tamtsp = 'P' and §1pfpt = 'N'
092700020205     c                   iter
092800020205     c                   endif
092900020205      * Tariffa con Network DPD escludo le tariffe particolare con
093000020205      * flag §1PFDP = 'N'
093100020205     c                   if        §tadpd = 'S' and §1pfdp = 'N'
093200020205     c                   iter
093300020205     c                   endif
093400020205      * Per Tariffa FedEx non faccio controlli xchè non vengono lette
093500020205
093600020205      * Tariffa Italia escludo le tariffe particolare con
093700020205      * flag §1PTCO = 'N' e non deve essere servizio poste
093800020205     c                   if        tamfie = 'I' and §1ptco = 'N'
093900020205     c                             and tamtsp <> 'P'
094000020205     c                   iter
094100020205     c                   endif
094200020205      * Tariffa EuroExpress escludo le tariffe particolare con
094300020205      * flag §1PFIE = 'N'
094400020205     c                   if        tamfie = 'E' and §1pfie = 'N'
094500020205     c                   iter
094600020205     c                   endif
094700020614      * Per tariffe di cartello verifico che non sia tipo servizio "P"
094800020614     C                   if        tamfie = ' ' and §1ptco = 'N'
094900020614     c                             and tamtsp <> 'P'
095000020614     c                   iter
095100020614     c                   endif
095200020205
095300020205     c                   add       1             k
095400020205     c                   movel     tblkey        ftc(k)
095500020205     c                   movel     tbluni        dtc(k)
095600020205      * carico l'ordinamento in stampa
095700020205     c                   movel     §1post        ost(k)
095800020205      * carico il tipo stampa standard oppure no
095900020205     c                   movel     §1ptst        tst(k)
096000020205
096100020205 e1  c                   enddo
096200020205      * carico la schiera delle estensioni consegne particolari
096300020205     C                   eval      cod ='SP'
096400020205     C                   z-add     0             K
096500141017     c     tabk2l        setll     tabel
096600020205
096700020205 b1  C                   DO        *hival
096800020205      *
096900141017     c     tabk2l        reade     tabel
097000020205      *
097100020205     c                   if        %eof(tabel00f)
097200020205     c                   leave
097300020205     c                   endif
097400020205      *
097500020205 b2  C                   IF        tblflg = ' '
097600020205     C                   movel     TBLUNI        DSSP
097700020205      *
097800020205     c                   movel     TBLKEY        comftc
097900020205      *
098000020205      * carico tutte le consegne particolari
098100020205     C                   z-add     1             K
098200020205     c                   setoff                                       69
098300020205     C     comftc        lookup    FTC(K)                                 69
098400020205     c                   if        *in69
098500020205     C                   movel     §spdes        EDTC(K)
098600020205     C                   movel     §spdac        EDAC(K)
098700020205     C                   movel     §spsud        ESUD(K)
098800020205     c                   endif
098900020205      *
099000020205 e2  C                   endif
099100020205      *
099200020205 e1  C                   ENDDO
099300020205      * ordino le tariffe particolari in base all'ordine in stampa
099400020205      *
099500020205     c                   movel     OST           WOS
099600020205     c                   sorta     wos
099700020205 b1  c                   do        50            j
099800020205     c                   z-add     1             jj
099900020205     c                   setoff                                       69
100000020205     c                   if        wos(j) <> '99' and wos(j) <> *blank
100100020205     c     wos(j)        lookup    ost(jj)                                69
100200020205      * carico la schiera delle tariffe particolare in ordine di stampa
100300020205     c                   if        *in69 = *on
100400020205     c                   add       1             jjj
100500020205     c                   move      ftc(jj)       cp(jjj)
100600020205     c                   endif
100700020205     c                   endif
100800020205 e1  c                   enddo
100900020205
101000020205     c                   endsr
101100020205
101200010604     C*------------------------------------------------------------------------
101300010604     C*   SR_SCAGLIONI PREPARAZIONE SCHIERA SCAGLIONI
101400010604     C*------------------------------------------------------------------------
101500010604      *
101600010604     C     SR_SCAGLIONI  BEGSR
101700010604      *
101800010605      * CARICO SCAGLIONI
101900010604      *
102000010604     c                   setoff                                       99
102100010622     c                   z-add     0             k
102200141016     .*//.......................................................................
102300010605      * cerco prima con la linea di partenza del cliente
102400010605      *
102500141017     C  n98DS1KTD        SETLL     TITAD000
102600141017     c   98ds1ktd        setll     tiofd000
102700010605      * se non trovato con la linea di partenza del cliente lo segnalo
102800061122     c                   if        not %equal
102900010605     c                   eval      lnpcli = 'NO'
103000010605      * mi riposiziono con chiave generica
103100010604      *
103200141017     C  n98DS1KTM        SETLL     TITAD000
103300141017     c   98ds1ktm        setll     tiofd000
103400010605      * se non trovato nulla errore manca dettaglio tariffario
103500010604      *
103600061122     c                   if        not %equal
103700010605     c                   seton                                        99
103800010605     c                   move      msg(4)        errmsg
103900010605     c                   exsr      staerr
104000010605     c                   goto      endscaglia
104100010605     c                   endif
104200010605      *
104300010605     c                   endif
104400010605      *
104500010604     C                   DO        *HIVAL
104600010604      *
104700141017     C  n98DS1KTM        READE     TITAD000
104800141017     c   98ds1ktm        reade     tiofd000
104900010604      *
105000061122     C                   if        %eof
105100010606      * se fine lettura e non ho caricato nessuno scaglione (K = 0) e
105200010621      * ho letto solo dettaglio con cap valorizzato (cap1scaglio = 'S')
105300010606      * mi riposiziono con l'ultima chiave per caricare almeno gli scaglioni
105400010621     c                   if        k = 0 and cap1scaglio = 'S'
105500141017     c  n98keytad        setll     titad000
105600141017     c   98keytad        setll     tiofd000
105700010606      * valorizzo il flag del non controllo del cap perchè altrimenti non riesco
105800010606      * a caricare gli scaglioni
105900010606     c                   eval      ctrcap = 'N'
106000010606     c                   iter
106100010606      * altrimenti vado a fine ciclo lettura
106200010606     c                   else
106300010604     c                   leave
106400010604     c                   endif
106500010606      *
106600010606     c                   endif
106700010604      * se annullato rileggo
106800010604     c                   if        tadatb = 'A'
106900010604     c                   iter
107000010604     c                   endif
107100141016     .*//.......................................................................
107200010604      *
107300010604      * salvataggio dei campi per le rotture regione/provincia
107400010604      *
107500010605     c                   if        rotlnp = *zeros
107600010606      * se controllo del cap lo devo fare (ctrcap = ' ')
107700010606      * se cap valorizzato nel 1° scaglione lo segnalo per caricare schiera
107800010606      * inoltro quando non c'è cap  e ritorno a leggere
107900010621     c                   if        tadcap <>  *blanks  and ctrcap = ' '
108000010606     c                   eval      cap1scaglio = 'S'
108100010606     c                   iter
108200010606     c                   endif
108300010604     c                   eval      rotlnp = tadlnp
108400010604     c                   eval      rotcts = tadcts
108500010604     c                   eval      rotcap = tadcap
108600010604      *
108700010604      * controlli di rottura per regione e pulizie schiere
108800010605      *
108900010605     c                   exsr      regdet
109000010605     c   99              goto      endscaglia
109100010605     c                   exsr      prodet
109200010605     c   99              goto      endscaglia
109300010605      *
109400010605     c                   endif
109500010605      *
109600010605      * carico gli scaglioni per lnp cts e cap uguali
109700010605      *
109800010605     c                   if        tadlnp = rotlnp and tadcts = rotcts and
109900010605     c                             tadcap = rotcap
110000010605     c                   add       1             k
110100010605      *
110200010605     c                   if        k > 18
110300050914      * accendo l'indicatore di errore così esce dal pgm
110400050914     c                   seton                                        99
110500010605     c                   move      msg(5)        errmsg
110600010605     c                   exsr      staerr
110700050930     c                   goto      endscaglia
110800010605     c                   endif
110900010605      *
111000010605     c                   z-add     tadsgl        sgl(k)
111100010606      *
111200010801      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
111300010801      * a valore
111400010801     c                   if        not *in87
111500010801      *
111600010606     c                   move      tadsgl        w0030
111700010606     c                   z-add     tadsgl        w0050
111800010606      * se scaglione 99999 non controllo i decimali
111900010606     c                   if        w0050 <> 99999 and w0030 > 0
112000010606     c                   seton                                        91
112100010606     c                   endif
112200010606      * se con decimali verifico se valorizzato dal secondo in poi
112300010606     c                   if        *in91
112400010606     c                   z-add     w0030         w0020
112500010606     c                   if        w0020 > 0
112600010606     c                   seton                                        37
112700010606     c                   endif
112800010606     c                   endif
112900010801      *
113000010801     c                   endif
113100010606      * se non è valorizzato il CAP carico la schiera dell'inoltro
113200010606     c                   if        tadcap = *blanks
113300010606     c                   z-add     tadino        pro(k)
113400010606      * pulisco il flag che mi indica che sto caricando la schiera degli inoltri
113500010606     c                   clear                   cap1scaglio
113600010606     c                   endif
113700010606      * valorizzo la schiera del rapporto peso volume
113800010622     c     TADRPV        MULT      100           rpv(k)
113900010606      *
114000010606     c                   else
114100010606      * rottura di chiave
114200010606     c                   leave
114300010606      *
114400010606     c                   endif
114500010606      *
114600010606     c                   enddo
114700010606      *
114800010606      * se non ho caricato nessuno scaglione do errore di mancanza di dettaglio
114900010606      *
115000010606     c                   if        K = 0
115100010606     c                   seton                                        99
115200010606     c                   move      msg(4)        errmsg
115300010606     c                   exsr      staerr
115400010606     c                   goto      endscaglia
115500010606     c                   endif
115600051006      * memorizzo quanti scaglioni ho trovato
115700051006     c                   Eval      wnrscaglio = k
115800080117      * prima di stampare controllo se gli scaglioni sono stampabili
115900080117      * in stampa il campo è lungo 4
116000080117     c                   eval      xy = 1
116100080117     c                   for       xy by 1 to wnrscaglio
116200080117     c                   z-add     sgl(xy)       w0050
116300080117      * scaglioni senza decimali al massimo 9999
116400080117     c                   if        not *in91 and w0050 > 9999
116500080118     c                             and w0050 <> 99999
116600080117     c                   eval      *in99 = *on
116700080117     c                   eval      errmsg = 'Scaglione ' +
116800080117     c                             %editc(sgl(xy):'4') +
116900080117     c                             ' non stampabile'
117000080117     c                   exsr      staerr
117100080117     c                   leavesr
117200080117     c                   endif
117300080218      * scaglioni con 1 decimale al massimo 999,9
117400080218     c                   if        *in91 and not *in37 and w0050 > 999,9
117500080118     c                             and w0050 <> 99999
117600080117     c                   eval      *in99 = *on
117700080117     c                   eval      errmsg = 'Scaglione ' +
117800080117     c                             %editc(sgl(xy):'4') +
117900080117     c                             ' non stampabile'
118000080117     c                   exsr      staerr
118100080117     c                   leavesr
118200080117     c                   endif
118300080218      * scaglioni con 2 decimale al massimo 99,99
118400080218     c                   if        *in91 and *in37 and w0050 > 99,99
118500080118     c                             and w0050 <> 99999
118600080117     c                   eval      *in99 = *on
118700080117     c                   eval      errmsg = 'Scaglione ' +
118800080117     c                             %editc(sgl(xy):'4') +
118900080117     c                             ' non stampabile'
119000080117     c                   exsr      staerr
119100080117     c                   leavesr
119200080117     c                   endif
119300080117     c                   endfor
119400010606      *
119500010606      * preparo la schiera scaglioni da stampare
119600010606      *
119700010606     c                   exsr      sr_stascaglia
119800010606      *
119900010606     c     endscaglia    endsr
120000010606      *
120100010606     C*-------------------------------------------------------------------------
120200010606     C*  SR_STASCAGLIA PREPARO LA SCHIERA DEGLI SCAGLIONI DA STAMPARE
120300010606     C*-------------------------------------------------------------------------
120400010606     C     SR_STASCAGLIA BEGSR
120500010606      *
120600010606     c                   z-add     1             k
120700010606     c                   clear                   k1
120800010606      *
120900010606     c                   dow       sgl(k) > 0
121000010606      * preparo lo scaglione DAL addizionato di 1 o 0,1 o 0,01 in base ai decima
121100010606      * li
121200010606     c                   z-add     sgl(k)        w0050
121300010606      *
121400010606      * verifico se scaglione 99999
121500010606     c                   if        w0050 <> 99999
121600010606      *
121700010606     c                   if        k1 > 0
121800010606      * se non ha decimali
121900010606     c                   if        not *in91
122000010622     C                   eval      sgld(k) = sgl(k1) + 1
122100010606     c                   endif
122200010606      *
122300010606     c                   if        *in91 and not *in37
122400010606      * se ha 1 decimale
122500010622     C                   eval      sg1d(k) = sgl(k1) + 0,1
122600010606     c                   endif
122700010606      *
122800010606     c                   if        *in91 and *in37
122900010606      * se ha + di 1 decimale
123000010622     C                   eval      sg2d(k) = sgl(k1) + 0,01
123100010606     c                   endif
123200010606      *
123300010606     c                   endif
123400010606      *
123500010606      * costruisco la schiera dell'applicazione tariffa finita che va sotto gli
123600010606      * scaglioni da/a in base al valore della tariffa finita che è in testata
123700010606      * tariffa e in mancanza di quest'ultima prendo dalla tabella TR
123800010606      *
123900010606      * muovo unità di misura della tariffa nella schiera
124000060922     c**!!!              movel     §trde3        atf(k)
124100060922      * devo fare tutto fisso a pgm perchè in tabella il campo è lungo 10
124200060922      * mentre in stampa è lungo 7
124300060922      * quindi devo personalizzarlo per ogni lingua
124400060925     c                   move      unoctr        wctr1
124500060922     c                   exsr      sr_atfk
124600010606      * faccio i vari ragionamenti in base al valore della tariffa finita
124700010606      *
124800010606      * se scaglioni senza decimali
124900011218     c                   if        not *in91
125000010606
125100011218     c                   z-add     tamata        wata
125200010606      * se in tariffa non è definita la recupero dalla tabella TR se maggiore 1
125300011218     c                   if        wata = 0 and ctrata > 1
125400011218     c                   z-add     ctrata        wata
125500011218     c                   endif
125600011218      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
125700011218      * scrivo "A SPED."
125800011218     c                   if        (wata > 0 and sgl(k) <= wata)
125900060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
126000060925     c                   clear                   wctr1
126100060922     c                   exsr      sr_atfk
126200011218     c                   endif
126300010606      *
126400011218     c                   else
126500010606      * scaglioni con decimali
126600011218     c                   z-add     tamata        wata1
126700011218     c                   z-add     tamata        wata2
126800010606      *
126900010606      * se in tariffa non è definita la recupero dalla tabella TR se maggiore 1
127000011218     c                   if        wata1 = 0 and ctrata > 1 and not *in37
127100011218     c                   z-add     ctrata        wata1
127200011218     c                   endif
127300010606      * se in tariffa non è definita la recupero dalla tabella TR se maggiore 1
127400011218     c                   if        wata2 = 0 and ctrata > 1 and *in37
127500011218     c                   z-add     ctrata        wata2
127600011218     c                   endif
127700010606      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
127800010606      * scrivo "A SPED."  1 decimale
127900011218     c                   if        (wata1 > 0 and sgl(k)<= wata1 and not *in37)
128000060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
128100060925     c                   clear                   wctr1
128200060922     c                   exsr      sr_atfk
128300011218     c                   endif
128400010606      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
128500010606      * scrivo "A SPED."  2 decimali
128600011218     c                   if        (wata2 > 0 and sgl(k) <= wata2 and *in37)
128700060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
128800060925     c                   clear                   wctr1
128900060922     c                   exsr      sr_atfk
129000011218     c                   endif
129100010606      *                                                      *in91 on
129200011218     c                   endif
129300011123
129400010606      *
129500010606     c                   move      '/'           sglb(k)
129600010606      * preparo lo scaglione AL
129700010606      *
129800010606      * no decimali
129900010606     c                   if        not *in91
130000010606     c                   z-add     sgl(k)        sgla(k)
130100010606     c                   endif
130200010606      * 1 decimale
130300010606     c                   if        *in91 and not *in37
130400010606     c                   z-add     sgl(k)        sg1a(k)
130500010606     c                   endif
130600010606      * 2 decimali
130700010606     c                   if        *in91 and *in37
130800010606     c                   z-add     sgl(k)        sg2a(k)
130900010606     c                   endif
131000010606      *
131100010606     c                   else
131200010606      * scaglione 99999 è sempre maggiore della tariffa finita
131300060922     c**!!!              eval      atf(k) = §trde3
131400060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
131500060922      * la decodifica del tipo tariffa
131600060925     c                   move      unoctr        wctr1
131700060922     c                   exsr      sr_atfk
131800010606      *
131900010606     c                   endif
132000010606      *
132100010606     c                   add       1             k
132200010606     c                   add       1             k1
132300010606      *
132400010606      * se maggiore di 18 esco
132500010606     c                   if        k > 18
132600010606     c                   leave
132700010606     c                   end
132800010606      *
132900010606     c                   enddo
133000010606      *
133100010606      * se c'è solo uno scaglione non scrivo niente sotto
133200021220      * solo se lo scaglione è 99999
133300010606      *
133400021220     c                   if        k1 <= 1 and sgl(k1) >= 99999
133500010606     c                   clear                   atf(1)
133600010606     c                   endif
133700010606      *
133800010606     C                   ENDSR
133900010606     C*-------------------------------------------------------------------------
134000010606     C*  SR_STINO   CONTROLLO SE DEVO STAMPARE INOLTRO A FINE DETTAGLIO
134100010606     C*-------------------------------------------------------------------------
134200010606     C     SR_STINO      BEGSR
134300020531
134400141016     .*//.......................................................................
134500010606      *
134600010606      * leggo tutto il titad e verifico per ogni scaglione valido se
134700010606      * se l'inoltro è uguale
134800010606      *
134900141017     c  n98ds1ktm        setll     titad000
135000141017     c   98ds1ktm        setll     tiofd000
135100010606      *
135200010606     c                   do        *hival
135300141017     c  n98ds1ktm        reade     titad000
135400141017     c   98ds1ktm        reade     tiofd000
135500010606      *
135600061122     c                   if        %eof
135700010606     c                   leave
135800010606     c                   endif
135900010606      * se annullato rileggo
136000010606     c                   if        tadatb = 'A'
136100010606     c                   iter
136200010606     c                   endif
136300141016     .*//.......................................................................
136400010606      * decodifico la linea di partenza se non ancora fatto
136500010606      *
136600010606     c                   if        stalnp = *blanks
136700141017     c     tadlnp        chain     azorg
136800010606      *
136900020531     c                   if        %found(azorg01l)
137000020531     c                   movel     orgdes        stalnp
137100141016     .*//.......................................................................
137200020531     c                   endif
137300010613      * salvo i campi per eventuali rotture utili in fulcro
137400010613     c                   eval      rotlnp = tadlnp
137500010613     c                   eval      rotcts = tadcts
137600010613     c                   eval      rotcap = tadcap
137700010613     c                   eval      rotpro = %subst(tadorp:5:1)
137800010613     c                   eval      rotreg = %subst(tadorp:3:2)
137900010618      * Salvo gli stessi campi di lavoro per letture in fulcro
138000010618     c                   eval      wrklnp = rotlnp
138100010618     c                   eval      wrkslp = stalnp
138200010618     c                   eval      wrkcts = rotcts
138300010618     c                   eval      wrkcap = rotcap
138400010618     c                   eval      wrkpro = rotpro
138500010618     c                   eval      wrkreg = rotreg
138600020206      * rotture per regione e provincia
138700020214     c                   exsr      regdet
138800020206      * per 99 errori vado a fine
138900020214     c   99              leave
139000020214     c                   exsr      prodet
139100020206      * per 99 errori vado a fine
139200020214     c   99              leave
139300020206      *
139400010606     c                   endif
139500010606      *
139600010606      * se cap valorizzato leggo il record successivo
139700010621     c                   if        tadcap <> *blanks
139800010606     c                   iter
139900010606     c                   endif
140000010606      * verifico se scaglione presente
140100010606      *
140200010606     c                   z-add     1             z
140300010613     c                   z-add     tadsgl        wsgl72
140400010613     c     Wsgl72        lookup    sgl(z)                                 35
140500010606     c                   if        not *in35
140600010606     c                   iter
140700010606     c                   endif
140800010606      * controllo l'inoltro nella schiera
140900010606      *
141000010606     c                   if        tadino <> pro(z)
141100010606     c                   eval      stino = 'Y'
141200010606     c                   leave
141300010606     c                   endif
141400010606      *
141500010606     c                   enddo
141600010606      *
141700010606     c                   endsr
141800010606     C*-------------------------------------------------------------------------
141900010607     C*  SR_ELADET  ELABORO E STAMPO IL DETTAGLIO TARIFFARIO
142000010606     C*-------------------------------------------------------------------------
142100010607     C     SR_ELADET     BEGSR
142200010606      *
142300010607      * elaboro 6 scaglioni alla volta
142400010607     C                   Z-ADD     1             K4
142500010607     C                   DOW       K4 <= 18
142600010607     C                   MOVEA     SGLD(K4)      SSLD
142700010607     C                   MOVEA     SGLB(K4)      SSLB
142800010607     C                   MOVEA     SGLA(K4)      SSLA
142900010607      * 1 DECIMALE
143000010607     C                   IF        *IN91 AND NOT *IN37
143100010607     C                   MOVEA     SG1A(K4)      SS1A
143200010607     C                   MOVEA     SG1D(K4)      SS1D
143300010607     C                   ENDIF
143400010607      * 2 DECIMALI
143500010607     C                   IF        *IN91 AND *IN37
143600010607     C                   MOVEA     SG2A(K4)      SS2A
143700010607     C                   MOVEA     SG2D(K4)      SS2D
143800010607     C                   ENDIF
143900010607      *
144000010607      * CARICO GLI SCAGLIONI PER ELABORARE IL DETTAGLIO
144100010607      *
144200010607     C                   MOVEA     SGL(K4)       SSL
144300141017     ***C*  INTESTAZIONE SCAGLIONI
144400141017     ***C                   Z-ADD     SSL(1)        W0050             5 0
144500141017     ***C     W0050         COMP      99999                                  41
144600141017     ***C     SSL(1)        COMP      *ZEROS                                 51
144700141017     ***C                   Z-ADD     SSL(2)        W0050             5 0
144800141017     ***C     W0050         COMP      99999                                  42
144900141017     ***C     SSL(2)        COMP      *ZEROS                                 52
145000141017     ***C                   Z-ADD     SSL(3)        W0050             5 0
145100141017     ***C     W0050         COMP      99999                                  43
145200141017     ***C     SSL(3)        COMP      *ZEROS                                 53
145300141017     ***C                   Z-ADD     SSL(4)        W0050             5 0
145400141017     ***C     W0050         COMP      99999                                  44
145500141017     ***C     SSL(4)        COMP      *ZEROS                                 54
145600141017     ***C                   Z-ADD     SSL(5)        W0050             5 0
145700141017     ***C     W0050         COMP      99999                                  45
145800141017     ***C     SSL(5)        COMP      *ZEROS                                 55
145900141017     ***C                   Z-ADD     SSL(6)        W0050             5 0
146000141017     ***C     W0050         COMP      99999                                  46
146100141017     ***C     SSL(6)        COMP      *ZEROS                                 56
146200141017     ***C*
146300141017     ***C* SE IL 41 E' ACCESO PERCHÈ IL PRIMO SCAGLIONE E' UGUALE A TUTTI 999999
146400141017     ***C* O IL SECONDO SCAGLIONE E' UGUALE A ZERO
146500141017     ***C* MA SE NON SI TRATTA + DEL 1° SCAGLIONE IN ASSOLUTO DEVO STAMPARE
146600141017     ***C* "OLTRE LO SCAGLIONE PRECEDENTE
146700141017     ***C* QUINDI ACCENDO IL 40 E SPENGO IL 41
146800141017     ***C*
146900141017     ***C                   IF        *IN41 AND K4 <> 1
147000141017     ***C                   SETOFF                                       41
147100141017     ***C                   SETON                                        40
147200141017     ***C                   ENDIF
147300061207      * indicatore e campo di comodo per tariffa Fuel a scaglioni
147400061207      * sto stampando il dettaglio tariffario quindi pulisco il campo e
147500061207      * spengo l'indicatore
147600141016     .*//.......................................................................
147700061207     c                   eval      *in12 = *off
147800010607     C*****
147900010607     C*  STAMPA INTESTAZIONE INIZIALE
148000010607     C*****
148100010608      * preparo la testata del dettaglio tariffario
148200010608     c                   exsr      SR_STATESDET
148300010608      *
148400010607     C*****
148500010613     C*  STAMPA DETTAGLIO TARIFFARIO
148600010613     C*****
148700010613     C                   EXSR      SR_FULCRO
148800010607     C* 99 indicatore di errore risultante dalla routine FULCRO
148900010614     C   99              GOTO      ENDDET
149000010615      * vedo se devo stampare l'inoltro
149100010615     c                   EXSR      SR_INOLTR
149200010615      * Rapporto peso volume SOLO SE NON È DPD
149300010615     c  N25              EXSR      SR_STARAP
149400010618      *
149500010618      * rielaboro altri 6 scaglioni
149600010618      *
149700010618     c                   add       6             k4
149800010618      * se scaglioni minori o uguali a 18 e scaglione valorizzato
149900010618     c                   if        k4 <= 18 and
150000010618     c                             sgl(k4) <> *zeros
150100010618      * pulisco schiere di comodo per la stampa degli scaglioni
150200010618     c                   clear                   ssld
150300010618     c                   clear                   sslb
150400010618     c                   clear                   ssla
150500010618     c                   clear                   ss1d
150600010618     c                   clear                   ss1a
150700010618     c                   clear                   ss2d
150800010618     c                   clear                   ss2a
150900010618     c                   clear                   ssl
151000141017     *** * spengo indicatori che condizionano la stampa
151100141017     ***c                   setoff                                       404142
151200141017     ***c                   setoff                                       434445
151300141017     ***c                   setoff                                       46
151400141017     ***c                   setoff                                       515253
151500141017     ***c                   setoff                                       545556
151600010618      * Recupero i campi di lettura salvati la prima volta
151700010618      *
151800010618     c                   eval      rotlnp = wrklnp
151900010618     c                   eval      stalnp = wrkslp
152000010618     c                   eval      rotcts = wrkcts
152100010618     c                   eval      rotreg = wrkreg
152200010618     c                   eval      rotpro = wrkpro
152300010618     c                   eval      rotcap = wrkcap
152400010618      *
152500010618     c                   if        not *in10
152600010621     c                   eval      tadcts = wrkcts
152700010618     c                   move      rotreg        com04
152800010618     c                   movel     com04         tadorp
152900010618     c                   move      rotpro        tadorp
153000010618     c                   endif
153100010618      *
153200010618     c                   exsr      regdet
153300010618     c   99              goto      enddet
153400010618      *
153500010618     c                   exsr      prodet
153600010618     c   99              goto      enddet
153700010618      *
153800010618     c                   else
153900010618      * non ci sono + scaglioni da stampare
154000010618     c                   eval      k4=19
154100010618     c                   leave
154200010618      *
154300010618     c                   endif
154400010618      *
154500010618     c                   enddo
154600010614      *
154700010614     c     ENDDET        ENDSR
154800010614      *
154900010608     C*-------------------------------------------------------------------------
155000010608     C*  SR_STATESDET PREPARO E STAMPO TESTATA DETTAGLIO TARIFFARIO
155100010608     C*-------------------------------------------------------------------------
155200010608     C     SR_STATESDET  BEGSR
155300010725      *
155400141017     ***c                   setoff                                       79
155500141016     .*//.......................................................................
155600141017     *** * se 41 acceso il 1° scaglione è = 99999
155700141017     ***c                   if        *in41
155800141016     .*//.......................................................................
155900141017     ***c                   seton                                        79
156000141017     ***c                   endif
156100141017     *** * se 42 acceso il 2° scaglione è = 99999
156200141017     ***c                   if        *in42
156300141016     .*//.......................................................................
156400141017     ***c                   endif
156500141017     *** * se 52 acceso il 2° scaglione è = 00000
156600141017     ***c                   if        *in52
156700141016     .*//.......................................................................
156800141017     ***c                   endif
156900141017     *** * se 43 acceso il 3° scaglione è = 99999
157000141017     ***c                   if        *in43
157100141016     .*//.......................................................................
157200141017     ***c                   endif
157300141017     *** * se 53 acceso il 3° scaglione è = 00000
157400141017     ***c                   if        *in53
157500141016     .*//.......................................................................
157600141017     ***c                   endif
157700141017     *** * se 44 acceso il 4° scaglione è = 99999
157800141017     ***c                   if        *in44
157900141016     .*//.......................................................................
158000141017     ***c                   endif
158100141017     *** * se 54 acceso il 4° scaglione è = 00000
158200141017     ***c                   if        *in54
158300141016     .*//.......................................................................
158400141017     ***c                   endif
158500141017     *** * se 45 acceso il 5° scaglione è = 99999
158600141017     ***c                   if        *in45
158700141016     .*//.......................................................................
158800141017     ***c                   endif
158900141017     *** * se 55 acceso il 5° scaglione è = 00000
159000141017     ***c                   if        *in55
159100141016     .*//.......................................................................
159200141017     ***c                   endif
159300141017     *** * se 46 acceso il 6° scaglione è = 99999
159400141017     ***c                   if        *in46
159500141016     .*//.......................................................................
159600141017     ***c                   endif
159700141017     *** * se 56 acceso il 6° scaglione è = 00000
159800141017     ***c                   if        *in56
159900141016     .*//.......................................................................
160000141017     ***c                   endif
160100010608     C*
160200010612      * prepatro la schiera dell'applicazione tariffa finita
160300010612     C                   Z-ADD     K4            B
160400010612     C                   DO        6             A
160500010612     C                   MOVE      ATF(B)        STF(A)
160600010612     C                   ADD       1             B
160700010612     C                   END
160800010612      *
160900010917      *
161000141016     .*//.......................................................................
161100141017     *** * SE TARIFFE PARTICOLARI NON STAMPO DA VOSTRO MAGAZZINO
161200141017     ***C                   IF        *IN10
161300141017     ***C                   SETON                                        78
161400141017     ***C                   ENDIF
161500061207
161600061207      * se tariffa particolare "f " Fuel
161700141017     ***c                   if        *in12
161800141017     *** * forzo l'accensione dell'indicatore 79 per non stampare la seconda
161900141017     *** * riga quando ho più scaglioni
162000141017     ***c                   eval      *in79 = *on
162100141017     ***c                   endif
162200010625      *
162300141016     .*//.......................................................................
162400010612      *
162500010612     c                   ENDSR
162600010613      *
162700010613     C*-------------------------------------------------------------------------
162800010613     C*  SR_FULCRO  STAMPA DETTAGLIO TARIFFARIO
162900010613     C*-------------------------------------------------------------------------
163000010613     C     SR_FULCRO     BEGSR
163100020531
163200141016     .*//.......................................................................
163300010613      *
163400010613      * Leggo il dettaglio tariffario dall'inizio
163500010613      *
163600141017     C  n98DS1KTM        SETLL     TITAD000
163700141017     c   98ds1ktm        setll     tiofd000
163800010613     C                   DO        *HIVAL
163900010613      *
164000141017     C  n98DS1KTM        READE     TITAD000
164100141017     c   98ds1ktm        reade     tiofd000
164200010613      * se fine file
164300061122     c                   if        %eof
164400010613      *
164500010613      * se cap uguale a blank STAMPO totali per provincia
164600010613      *
164700010613     c                   if        rotcap = *blanks
164800010613     c                   exsr      protot
164900010613     c                   else
165000010613      *
165100010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
165200010613      *
165300010613     c                   exsr      captot
165400010613     c                   endif
165500010613      *
165600010613      * calcoli per regione
165700010613      *
165800010613     c                   exsr      calreg
165900010613      *
166000010613      * stampo i totali per regione
166100010613      *
166200010613     c  n99              exsr      stareg
166300010613      *
166400010613     c                   leave
166500010613      * eof
166600010613     c                   endif
166700010613      *
166800010613      * verifico il record letto se coerente con le richieste di stampa
166900010613      *
167000010613      * se annullato rileggo
167100010613     c                   if        tadatb = 'A'
167200010613     c                   iter
167300010613     c                   endif
167400141016     .*//.......................................................................
167500010613      *
167600010613      * verifico se è uno scaglione previsto
167700010613      *
167800010613     c                   z-add     tadsgl        wsgl72
167900010614     c     wsgl72        lookup    ssl                                    35
168000010614      * se non trovato nei 6 scaglioni da stampare verifico se scaglione corrett
168100010614     c                   if        not *in35
168200010613     c     wsgl72        lookup    sgl                                    35
168300010613     c                   if        not *in35
168400010613      * errore scaglione non stampabile
168500050914      * esco dal pgm
168600050914     c                   seton                                        99
168700010613     c                   movel     msg(8)        errmsg
168800010613     c                   exsr      staerr
168900050914     c                   leave
169000010614     c                   endif
169100010613     c                   iter
169200010613     c                   endif
169300010613
169400010613      * preparo i campi per eventuali rotture
169500010613
169600010613     c                   eval      compro = %subst(tadorp:5:1)
169700010613     c                   eval      comreg = %subst(tadorp:3:2)
169800010614      *
169900010614     c                   SELECT
170000010614      *
170100010613      *-------------------------------------------------------------------------
170200010613      *           ROTTURA PER      L I N E A    D I    P A R T E N Z A
170300010613      *-------------------------------------------------------------------------
170400010614     c                   WHEN      tadlnp  <> rotlnp
170500010613     c                   eval      rotlnp = tadlnp
170600010613     c                   eval      rotreg = comreg
170700010613     c                   eval      rotpro = compro
170800010613      * verifico se cap valorizzato
170900010613     c                   if        rotcap = *blanks
171000010613      * se cap uguale a blank STAMPO totali per provincia
171100010613     c                   exsr      protot
171200010613     c                   else
171300010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
171400010613     c                   exsr      captot
171500010613     c                   endif
171600010613      *
171700010613     c                   eval      rotcap = tadcap
171800010613      * calcoli per regione
171900010613     c                   exsr      calreg
172000010613      * se ci sono errori vado a fine
172100010613     c   99              leave
172200010613      * stampo i totali per regione
172300010613     c                   exsr      stareg
172400010613      * decodifico la nuova linea di partenza
172500141017     c     tadlnp        chain     azorg
172600020531     c                   if        %found(azorg01l)
172700020531     c                   movel     orgdes        stalnp
172800141016     .*//.......................................................................
172900020531     c                   endif
173000010613      * stampo la testata scaglioni con la nuova linea di partenza
173100141016     .*//.......................................................................
173200010613      * pulizie e controllo dettaglio per regione
173300010613     c                   exsr      regdet
173400010613      * pulizie e controllo dettaglio per provincia
173500010613     c                   exsr      prodet
173600010613      * se ci sono errori vado a fine
173700010613     c   99              leave
173800010613      *
173900010613      *
174000010613      * sto leggendo per la stessa linea di partenza
174100010613      *
174200010613      *-------------------------------------------------------------------------
174300010613      *           ROTTURA PER      R E G I O N E
174400010613      *-------------------------------------------------------------------------
174500010614     c                   WHEN      comreg <> rotreg
174600010613     c                   eval      rotreg = comreg
174700010613     c                   eval      rotpro = compro
174800010613      * verifico se cap valorizzato
174900010613     c                   if        rotcap = *blanks
175000010613      * se cap uguale a blank STAMPO totali per provincia
175100010613     c                   exsr      protot
175200010613     c                   else
175300010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
175400010613     c                   exsr      captot
175500010613     c                   endif
175600010613      *
175700010613     c                   eval      rotcap = tadcap
175800010613      * calcoli per regione
175900010613     c                   exsr      calreg
176000010613      * se ci sono errori vado a fine
176100010613     c   99              leave
176200010613      * stampo i totali per regione
176300010613     c                   exsr      stareg
176400010613      * pulizie e controllo dettaglio per regione
176500010613     c                   exsr      regdet
176600010613      * pulizie e controllo dettaglio per provincia
176700010613     c                   exsr      prodet
176800010613      * se ci sono errori vado a fine
176900010613     c   99              leave
177000010613      *
177100010613      *
177200010613      * sto leggendo per la stessa linea di partenza / regione
177300010613      *
177400010613      *-------------------------------------------------------------------------
177500010613      *           ROTTURA PER      P R O V I N C I A
177600010613      *-------------------------------------------------------------------------
177700010614     c                   WHEN      compro <> rotpro
177800010613     c                   eval      rotpro = compro
177900010613      * verifico se cap valorizzato
178000010613     c                   if        rotcap = *blanks
178100010613      * se cap uguale a blank STAMPO totali per provincia
178200010613     c                   exsr      protot
178300010613     c                   else
178400010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
178500010613     c                   exsr      captot
178600010613     c                   endif
178700010613      *
178800010613     c                   eval      rotcap = tadcap
178900010613      * pulizie e controllo dettaglio per provincia
179000010613     c                   exsr      prodet
179100010613      * se ci sono errori vado a fine
179200010613     c   99              leave
179300010613      *
179400010613      *
179500010613      * sto leggendo per la stessa linea di partenza / regione / provincia
179600010613      *
179700010613      *-------------------------------------------------------------------------
179800010613      *           ROTTURA PER      P A E S E   D I    A R R I V O   (CAP)
179900010613      *-------------------------------------------------------------------------
180000010614     c                   WHEN      tadcap <> rotcap
180100010613      * verifico se cap valorizzato
180200010613     c                   if        rotcap = *blanks
180300010613      * se cap uguale a blank STAMPO totali per provincia
180400010613     c                   exsr      protot
180500010613     c                   else
180600010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
180700010613     c                   exsr      captot
180800010613     c                   endif
180900010613      * se ci sono errori vado a fine
181000010613     c   99              leave
181100010613      *
181200010613     c                   eval      rotcap = tadcap
181300010613      * pulisco le schiere di stampa
181400010613     c                   clear                   tct
181500010613     c                   clear                   tmn
181600010613     c                   clear                   tmx
181700010613     c                   clear                   tin
181800010613      *
181900010614     c                   ENDSL
182000010613      *
182100010613      *-------------------------------------------------------------------------
182200010613      *      E L A B O R O   L O   S C A G L I O N E
182300010613      *-------------------------------------------------------------------------
182400010613
182500010613      * ricerco lo SCAGLIONE di appartenenza
182600010613     c                   z-add     1             k1
182700010613     c                   z-add     tadsgl        wsgl72
182800010613     c     wsgl72        lookup    ssl(k1)                                35
182900010614      * se non trovato scrivo nel 1° scaglione
183000010614     c  n35              z-add     1             k1
183100010614      * valorizzo le schiere degli importi da stampare
183200010614     c                   z-add     taditr        tct(k1)
183300010614     c                   z-add     tadmin        tmn(k1)
183400010614     c                   z-add     tadmax        tmx(k1)
183500010614      * se devo stampare l'inoltro per ogni dettaglio
183600010614     c                   if        stino = 'Y'
183700010614     c                   z-add     tadino        tin(k1)
183800010614     c                   endif
183900010614      *
184000010614     c                   ENDDO
184100010614      *
184200010614     c                   ENDSR
184300020531
184400141016     .*//.......................................................................
184500020531
184600010614     C*-------------------------------------------------------------------------
184700010614     C*  PROTOT  TOTALI X  P R O V I N C I A
184800010614     C*-------------------------------------------------------------------------
184900010614     C     PROTOT        BEGSR
185000010614      *
185100010614      *  SALVO LA SCHIERA TCT NEL PRIMO BIG LIBERO
185200010614     C                   Z-ADD     1             K
185300010614     C     *BLANKS       LOOKUP    BIG(K)                                 35
185400010614
185500010614      * se non trovo nessun posto libero errore
185600010614
185700010614     C                   if        not *in35
185800010614     C                   seton                                        99
185900010614     C                   move      MSG(9)        errmsg
186000010614     C                   EXSR      STAERR
186100050930      * vado a fine se errore
186200050930     c                   goto      fine
186300010614     C                   else
186400051006      * Controllo se ok tutti gli scaglioni
186500051006     c                   ExSr      Sr_Ctrsca
186600010614      * carico la schiera delle province 6 alla volta
186700010614      * città
186800010614     C                   DO        6             B
186900010614     C                   MOVE      TCT(B)        COM(B)
187000010614     C                   END
187100010614     C                   MOVEA     COM           BIG(K)
187200010614      *
187300010614     C                   MOVE      CODSIG        BSI(K)
187400010614     C                   MOVE      CODORP        BRG(K)
187500050204     c                   move      desprv        dprv(k)
187600050303     c                   eval      nodes(k) = nostdes
187700010614      * minimo
187800010614     C                   DO        6             B
187900010614     C                   MOVE      TMN(B)        COM(B)
188000010614     C                   END
188100010614     C                   MOVEA     COM           BMN(K)
188200010614      * masismo
188300010614     C                   DO        6             B
188400010614     C                   MOVE      TMX(B)        COM(B)
188500010614     C                   END
188600010614     C                   MOVEA     COM           BMX(K)
188700010614      * inoltro
188800010614     C                   DO        6             B
188900010614     C                   MOVE      TIN(B)        COM(B)
189000010614     C                   END
189100010614     C                   MOVEA     COM           BIN(K)
189200010614      **
189300010614     C                   END
189400010614      *
189500010614     C                   ENDSR
189600010614     C*-------------------------------------------------------------------------
189700010614     C*  CAPTOT  TOTALI X  PAESE DI ARRIVO (CAP)
189800010614     C*-------------------------------------------------------------------------
189900010614     C     CAPTOT        BEGSR
190000010614      *  SALVO LA SCHIERA TCT DIRETTAMENTE NEL PDE/WIN
190100010614     C                   Z-ADD     1             P
190200010614     C     *BLANKS       LOOKUP    PDE(P)                                 35
190300010614      * se non trovato do errore
190400010614     c                   if        not *in35
190500010614     C                   seton                                        99
190600010614     C                   move      MSG(12)       errmsg
190700010614     C                   EXSR      STAERR
190800010614      *
190900010614     C                   ELSE
191000051006      * Controllo se ok tutti gli scaglioni
191100051006     c                   ExSr      Sr_Ctrsca
191200010614      *
191300010614     C                   MOVEL     *all'.'       PDE(P)
191400010614     C                   MOVE      rotcap        PDE(P)
191500010614      * città
191600010614     C                   DO        6             B
191700010614     C                   MOVE      TCT(B)        COM(B)
191800010614     C                   END
191900010614     C                   MOVEA     COM           WIN(P)
192000010614      * minimo
192100010614     C                   DO        6             B
192200010614     C                   MOVE      TMN(B)        COM(B)
192300010614     C                   END
192400010614     C                   MOVEA     COM           WMN(P)
192500010614      * massimo
192600010614     C                   DO        6             B
192700010614     C                   MOVE      TMX(B)        COM(B)
192800010614     C                   END
192900010614     C                   MOVEA     COM           WMX(P)
193000010614      * inoltro
193100010614     C                   DO        6             B
193200010614     C                   MOVE      TIN(B)        COM(B)
193300010614     C                   END
193400010614     C                   MOVEA     COM           WTI(P)
193500010614     C                   END
193600010614      *
193700010614     C                   ENDSR
193800051006      *-------------------------------------------------------------------------
193900051006      * Controllo se ok gli scaglioni da stampare
194000051006      *-------------------------------------------------------------------------
194100051006     c     Sr_Ctrsca     BegSr
194200051006
194300051006     c                   Select
194400051006
194500051006     c                   When      wnrscaglio > 12
194600051006      * sto stampando il primo o il secondo giro di scaglioni
194700051006      * vuol dire che devo stampare di sicuro 6 importi
194800051006     c                   If        k4 < 13
194900051006     c                   Eval      yy = 6
195000051006      * sto stampando il terzo giro di scaglioni
195100051006      * cerco quanti importi devo stampare
195200051006     c                   Else
195300060428     c                   Eval      yy = wnrscaglio - 12
195400051006     c                   EndIf
195500051006
195600051006     c                   When      wnrscaglio > 6 and wnrscaglio < 12
195700051006      * sto stampando il primo giro di scaglioni
195800051006      * vuol dire che devo stampare di sicuro 6 importi
195900051006     c                   If        k4 = 1
196000051006     c                   Eval      yy = 6
196100051006      * sto stampando il secondo giro di scaglioni
196200051006      * cerco quanti importi devo stampare
196300051006     c                   Else
196400051006     c                   Eval      yy = wnrscaglio - 6
196500051006     c                   EndIf
196600051006
196700051006     c                   When      wnrscaglio = 6
196800051006     c                   Eval      yy = 6
196900051006
197000051006     c                   When      wnrscaglio < 6
197100051006     c                   Eval      yy = wnrscaglio
197200051006     c                   EndSl
197300051006
197400051006      * controllo se ci sono degli importi a zero
197500051006     c                   Do        yy            xx
197600051006     c                   If        tct(xx) = *Zeros
197700051006     c                   Eval      errmsg = msg(8)
197800051006     c                   ExSr      staerr
197900051006     c                   Goto      fine
198000051006     c                   EndIf
198100051006     c                   EndDo
198200051006
198300051006     c                   EndSr
198400010614     C*-------------------------------------------------------------------------
198500010614     C*  CALREG  CALCOLO SKIERE X STAMPA REGIONE
198600010614     C*-------------------------------------------------------------------------
198700010614     C     CALREG        BEGSR
198800010614      *
198900010614      *                  N  O  T  E
199000010614      * Leggo BIG (schiera per ogni provincia della regione) e carico WIN/WCT
199100010614      * schiere di work)
199200010614      * X ogni provincia letta (quindi per ogni elemento) dopo averlo caricato
199300010614      * controllo se ci sono altre province uguali
199400010614      * se si accodo nella skiera dei COD/TASSAZ. che hanno la stessa tariffa
199500010614      * leggo i soli elementi con BRG>0 (a mano mano che li raggruppo
199600010614      * li abblenco). Mi fermo quando leggo la regione che è sempre
199700010614      * l'ultimo e va da solo : K=1 la descrizione è  CAPOLUOGHI
199800010614      *                         K>1 "    "    "    è  ALTRI
199900010614      *
200000010614      * RICERCO IL 1.O ELEMENTO NON OCCUPATO DA PAESI DI ARRIVO
200100010614     C                   DO        29            P
200200010614     C     PDE(P)        COMP      *blanks                                35
200300010614     C   35              SUB       1             P
200400010614     C  N35              END
200500110216
200600110216     c                   clear                   savk
200700010614      *
200800050616    1C                   DO        15            K
200900010614    2C     BRG(K)        IFNE      '  '
201000010614      * ELEMENTO VALIDO                                    -----------
201100010614      *
201200010614      * 9 = regione
201300010614      *
201400010614    3C     BRG(K)        IFEQ      '9'
201500010614     C                   ADD       1             P
201600110330     c                   eval      savk = p
201700010614      * errore
201800010614    4C     P             IFGT      30
201900010614     C                   SETON                                        99
202000010614     C                   MOVE      MSG(12)       ERRMSG
202100010614     C                   EXSR      STAERR
202200010614     C                   GOTO      ENDCAL
202300010614    4C                   END
202400010614      * città
202500010614     C                   MOVEA     BIG(K)        COMO42
202600010614     C                   MOVEA     COMO42        WIN(P)
202700010614     C                   MOVE      *BLANKS       BIG(K)
202800010614      * minimo
202900010614     C                   MOVEA     BMN(K)        CMNO42
203000010614     C                   MOVEA     CMNO42        WMN(P)
203100010614     C                   MOVE      *BLANKS       BMN(K)
203200010614      * massimo
203300010614     C                   MOVEA     BMX(K)        CMXO42
203400010614     C                   MOVEA     CMXO42        WMX(P)
203500010614     C                   MOVE      *BLANKS       BMX(K)
203600010614      * inoltro
203700010614     C                   MOVEA     BIN(K)        CINO42
203800010614     C                   MOVEA     CINO42        WTI(P)
203900010614     C                   MOVE      *BLANKS       BIN(K)
204000010614      *
204100010614     C                   MOVE      '  '          BRG(K)
204200010614      *
204300010614    4C     K             IFEQ      1
204400010626     C                   EVAL      PDE(P) = '. . . . . . . . . . '
204500010614     C                   ELSE
204600110216     c                   eval      pde(p) = '.........................'
204700010614    4C                   END
204800010614     C                   GOTO      ENDCAL
204900010614      * non si tratta di regione
205000010614
205100010614   X3C                   ELSE
205200010614
205300010614     C                   MOVE      *BLANKS       WCT
205400010614     C                   Z-ADD     0             C
205500050204     c                   Eval      wragprv = *Off
205600010614      * città
205700010614     C                   ADD       1             P
205800010614     C                   MOVEA     BIG(K)        COMO42
205900010614     C                   MOVEA     COMO42        WIN(P)
206000010614     C                   MOVE      *BLANKS       BIG(K)
206100010614      * minimo
206200010614     C                   MOVEA     BMN(K)        CMNO42
206300010614     C                   MOVEA     CMNO42        WMN(P)
206400010614     C                   MOVE      *BLANKS       BMN(K)
206500010614      * massimo
206600010614     C                   MOVEA     BMX(K)        CMXO42
206700010614     C                   MOVEA     CMXO42        WMX(P)
206800010614     C                   MOVE      *BLANKS       BMX(K)
206900010614      * inoltro
207000010614     C                   MOVEA     BIN(K)        CINO42
207100010614     C                   MOVEA     CINO42        WTI(P)
207200010614     C                   MOVE      *BLANKS       BIN(K)
207300010614      * sigla della provincia
207400010614     C                   ADD       1             C
207500010614     C                   MOVE      BSI(K)        WCT(C)
207600010614      *
207700010614     C                   MOVE      '  '          BRG(K)
207800010614      **
207900010614      * DOPO AVER CARICATO LA PROV. LETTA NE CERCO ALTRE DA RAGGRUPPARE
208000010614     C     K             ADD       1             H
208100010614      *
208200050616    4C     H             DO        15            H
208300010614    5C     COMO42        IFEQ      BIG(H)
208400010614     C     CMNO42        ANDEQ     BMN(H)
208500010614     C     CMXO42        ANDEQ     BMX(H)
208600010614     C     CINO42        ANDEQ     BIN(H)
208700010614     C     BRG(H)        ANDNE     '9'
208800010614     C                   ADD       1             C
208900010614     C                   MOVE      BSI(H)        WCT(C)
209000010614     C                   MOVEL     '/'           WCT(C)
209100010614      *
209200010614     C                   MOVE      *BLANKS       BIG(H)
209300010614     C                   MOVE      *BLANKS       BMN(H)
209400010614     C                   MOVE      *BLANKS       BMX(H)
209500010614     C                   MOVE      *BLANKS       BIN(H)
209600010614     C                   MOVE      '  '          BRG(H)
209700050204     c                   Eval      wragprv = *On
209800010614      *
209900010614    5C                   ENDIF
210000010614    4C                   ENDDO
210100010614      *
210200010614     C                   ADD       1             C
210300010614     C                   MOVEA     *ALL'.'       WCT(C)
210400010614     C                   MOVEA     WCT           COMO29
210500010614     C                   MOVEA     COMO29        PDE(P)
210600050204
210700050204      * se tariffa estera e non ho raggruppato più province devo stampare
210800050204      * la descrizione della provincia e non il codice
210900050204     c                   If        wragprv = *Off and (*in02 or *in25)
211000050204     c                   Eval      como29 = *all'.'
211100050303     c                   If        nodes(k) <> 'N'
211200050204     c                   Eval      %subst(como29:1:(%len(%trim(dprv(k))))) =
211300050204     c                             %trim(dprv(k))
211400050303     c                   EndIf
211500050204     c                   Move      como29        pde(p)
211600050204     c                   EndIf
211700010614      *
211800010614    3C                   END
211900010614      *
212000010614    2C                   END
212100010614    1C                   END
212200010614      *
212300010614     C     ENDCAL        ENDSR
212400010614     C*-------------------------------------------------------------------------
212500010615     C*  STAREG  GESTIONE STAMPE REGIONE
212600010614     C*-------------------------------------------------------------------------
212700010614     C     STAREG        BEGSR
212800110216
212900110216     c                   clear                   giro              1 0
213000110216
213100110216      * stampo per prima 'ALTRI' cioè la regione
213200110216     c                   if        savk > 1
213300110216     c                   eval      k = savk
213400110216     c                   else
213500010614      * LOOP EMISSIONE RAGGRUPPAMENTI/REGIONE
213600010614     C                   Z-ADD     1             K
213700110216     c                   endif
213800110216
213900110216     c     rifai         tag
214000010614      *
214100010615    1C     WIN(K)        DOUEQ     *BLANKS
214200110216     c                   if        k = savk  and giro > 0
214300110216     c                   leave
214400110216     c                   endif
214500010614     C                   MOVEA     WIN(K)        COM
214600010614     C                   Z-ADD     1             B
214700010614     C                   DO        6             B
214800010614     C                   MOVE      COM(B)        PAM(B)
214900010614      * PER TARIFFA A VALORE
215000010614     C                   IF        *IN87 AND COM(B) > *ZEROS
215100010614     C                   MOVEL     '%'           AI2(B)
215200010614     C                   END
215300010614      *
215400010614     C                   END
215500010614      * VERIFICO SE C'E' IL MINIMO E IL MASSIMO
215600141017     ***C                   SETOFF                                       4849
215700010614      *
215800010615    2C                   IF        WMN(K) <> *BLANKS AND WMN(K) <> *ZEROS
215900141017     ***C                   SETON                                        48
216000010614      *
216100010614     C                   MOVEA     WMN(K)        COM
216200010614      *
216300010614     C                   DO        6             B
216400010614     C                   MOVE      COM(B)        PMN(B)
216500010614     C                   ENDDO
216600010614      *
216700010615    2C                   ENDIF
216800010614      **
216900010615    2C                   IF        WMX(K) <> *BLANKS AND WMX(K) <> *ZEROS
217000141017     ***C                   SETON                                        49
217100010614      **
217200010614     C                   MOVEA     WMX(K)        COM
217300010614      *
217400010614     C                   DO        6             B
217500010614     C                   MOVE      COM(B)        PMX(B)
217600010614     C                   ENDDO
217700010614      *
217800010615    2C                   ENDIF
217900010614      *
218000010614      * VERIFICO SE DEVO STAMPARE LE SPESE DI INOLTRO
218100141017     ***C                   SETOFF                                       57
218200010614      *
218300010615    2C                   IF        STINO = 'Y'
218400141017     ***C                   SETON                                        57
218500010614     C                   MOVEA     WTI(K)        COM
218600010614     C                   Z-ADD     1             B
218700010614      *
218800010614     C                   DO        6             B
218900010614     C                   MOVE      COM(B)        PIN(B)
219000010614     C                   END
219100010614      *
219200010615    2C                   ENDIF
219300010614      *
219400141016     .*//.......................................................................
219500010614     C*
219600010614     C                   ADD       1             K
219700010615    1C                   END
219800110216
219900110216      * se ho stampato 'Altri' torno a loopare per stampare il resto
220000110216     c                   if        savk > 1 and giro = 0
220100110216     c                   eval      k = 1
220200110216     c                   eval      giro = 1
220300110216     c                   goto      rifai
220400110216     c                   endif
220500010615     C*
220600010614     C                   MOVEA     *BLANKS       AI2
220700010615      *
220800010614     C                   ENDSR
220900010625     C*-------------------------------------------------------------------------
221000010615     C*  PRODET  DETTAGLIO INIZIALE X  P R O V I N C I A
221100010625     C*-------------------------------------------------------------------------
221200010615     C     PRODET        BEGSR
221300010615      *
221400010622     C                   Z-ADD     1             K5
221500010615      * cerco il codice tassazione in schiera per TITAD
221600010622     C  N10TADCTS        LOOKUP    CCT(K5)                                35
221700010615      * cerco il codice tassazione in schiera per TITPD
221800010622     C   10TPDCTS        LOOKUP    CCT(K5)                                35
221900010615      *  se non trovato errore
222000010615     C  N35              SETON                                        99
222100010615     C     *IN99         IFEQ      '1'
222200010615     C                   MOVE      MSG(6)        ERRMSG
222300010615     C                   EXSR      STAERR
222400010615      *
222500010615     C                   ELSE
222600010615      *
222700010615      * TITAD
222800010615     C  N10              MOVE      TADORP        CODORP
222900010615      * TITPD
223000010615     C   10              MOVE      TPDORP        CODORP
223100010615      *
223200010622     C                   MOVE      SCT(K5)       CODSIG
223300050204     C                   MOVE      prvdes(k5)    desprv
223400050303     c                   Eval      nostdes = noctdes(k5)
223500010615     C*  PULIZIA SCHIERE PROVINCIA
223600010615     c                   if        not *in10
223700010615     C                   CLEAR                   TCT
223800010615     C                   CLEAR                   TMN
223900010615     C                   CLEAR                   TMX
224000010615     C                   CLEAR                   TIN
224100010615      *
224200010615     c                   else
224300010615      *
224400010615     C                   MOVEA     *ZEROS        TAP
224500010615     C                   MOVEA     *ZEROS        TPN
224600010615     C                   MOVEA     *ZEROS        TPX
224700010615      *
224800010615     C                   endif
224900010615      *
225000010615     C                   END
225100010615      *
225200010615     C                   ENDSR
225300010615     C*-----------------------------------------
225400010615     C*  REGDET  DETTAGLIO INIZIALE X  R E G I O N E
225500010615     C*-----------------------------------------
225600010615     C     REGDET        BEGSR
225700010615      *
225800010622     C                   Z-ADD     1             K5
225900010615      * cerco il codice tassazione in schiera per TITAD
226000010622     C  N10TADCTS        LOOKUP    CCT(K5)                                35
226100010615      * cerco il codice tassazione in schiera per TITPD
226200010622     C   10TPDCTS        LOOKUP    CCT(K5)                                35
226300010615      *  se non trovato errore
226400010615     C  N35              SETON                                        99
226500010615     C     *IN99         IFEQ      '1'
226600010615     C                   MOVE      MSG(6)        ERRMSG
226700010615     C                   EXSR      STAERR
226800010615      *
226900010615     C                   ELSE
227000010615      *
227100010622     C                   MOVE      RCT(K5)       CODREG
227200010615      * per tariffa DPD e linea partenza cliente non è  DPD regione IT diventa
227300010615      * regione EE
227400020312     c                   if        codreg = 'IT' and *in25 and §ogntw <> 'DPD'
227500010615     C                   MOVEL     'EE'          CODREG
227600010615     C                   ENDIF
227700010615      *
227800010622     C                   Z-ADD     1             K5
227900010622     C     CODREG        LOOKUP    CRE(K5)                                35
228000010615     C  N35              SETON                                        99
228100010615     C     *IN99         IFEQ      '1'
228200010615     C                   MOVE      MSG(7)        ERRMSG
228300010615     C                   EXSR      STAERR
228400010615     C                   ELSE
228500010615      *
228600141016     .*//.......................................................................
228700010615      *  PULIZIA SCHIERE REGIONE
228800010615     C                   MOVE      *BLANKS       BIG
228900010615     C                   MOVE      *BLANKS       BMN
229000010615     C                   MOVE      *BLANKS       BMX
229100010615     C                   MOVE      *BLANKS       BSI
229200010615     C                   MOVE      *BLANKS       BIN
229300010615     C                   CLEAR                   BRG
229400010615     C                   MOVE      *BLANKS       WIN
229500010615     C                   MOVE      *BLANKS       WMN
229600010615     C                   MOVE      *BLANKS       WMX
229700010615     C                   MOVE      *BLANKS       WTI
229800010622      *
229900010615     C                   MOVE      *BLANKS       PDE
230000010615     C                   END
230100010615     C                   END
230200010615      *
230300010615     C                   ENDSR
230400010615      *-------------------------------------------------------------------------
230500010615      *  SR_INOLTR  GESTIONA STAMPE INOLTRO SE PER TUTTI UGUALE
230600010615      *-------------------------------------------------------------------------
230700010615     C     SR_INOLTR     BEGSR
230800010615      *
230900010615      * SE TARIFFE DI INOLTRO UGUALI TRA LORO STAMPO
231000010615    1C     STINO         IFNE      'Y'
231100010615      * SE INOLTRO =0 NON STAMPO NULLA
231200010615     C                   Z-ADD     K4            B
231300010615     C                   DO        6             A
231400010615     C                   MOVE      PRO(B)        COM(A)
231500010615     C                   ADD       1             B
231600010615     C                   END
231700010615     C                   MOVEA     COM           CINO42
231800010615      *
231900010615      * SE INOLTRO > 0
232000010615      *
232100010615    2C                   IF        CINO42 <> *ZEROS
232200010615      *
232300010615     C                   Z-ADD     K4            K
232400010615    3C                   DO        6             A
232500010615     C                   Z-ADD     PRO(K)        PAM(A)
232600010615      * SE TARIFFA A VALORE E IMPORTO VALORIZZATO IMPOSTO %
232700010615     C                   IF        PAM(A) > *ZEROS AND *IN87
232800010615     C                   MOVEL     '%'           AI2(A)
232900010615     C                   END
233000010615     C                   ADD       1             K
233100010615    3C                   END
233200141016     .*//.......................................................................
233300010615      *
233400010615    2C                   endif
233500010615      *
233600010615    1C                   endif
233700010615      *
233800010615     C                   CLEAR                   AI2
233900010615      *
234000010615     C                   ENDSR
234100010615      *-------------------------------------------------------------------------
234200010615      *  SR_ARROTO  GESTIONA STAMPE ARROTONDAMENTI
234300010615      *-------------------------------------------------------------------------
234400010615     C     SR_ARROTO     BEGSR
234500010615      *
234600141016     .*//.......................................................................
234700010615      * verifico gli importi degli arrotondamenti
234800010615      *
234900010621    1c                   if        tamarl > 0
235000010615      * verifico se esistono dei valori con i decimali
235100010615     C                   move      tamarf        w0030
235200010615     C                   eval      *in89 =(w0030 > 0)
235300010615      *
235400010621    2c                   if        not *in89
235500010615     C                   move      tamarl        w0030
235600010615     C                   eval      *in89 =(w0030 > 0)
235700010621    2c                   endif
235800010615      *
235900010621    2c                   if        not *in89
236000010615     C                   move      tamaro        w0030
236100010615     C                   eval      *in89 =(w0030 > 0)
236200010621    2c                   endif
236300010615      *
236400010615      * se 89 spento non esistono importi con decimali sposto in camp di comodo
236500010621    2c                   if        not *in89
236600010615     C                   Z-ADD     TAMARO        WARO2
236700010615     C                   Z-ADD     TAMARF        WARF2
236800010615     C                   Z-ADD     TAMARL        WARL2
236900010621    2c                   endif
237000010615      *
237100010621    1c                   endif
237200010621      *
237300010615     C                   ENDSR
237400010615      *-------------------------------------------------------------------------
237500010615      *  SR_STARAP  GESTIONA STAMPE RAPPORTO PESO VOLUME
237600010615      *-------------------------------------------------------------------------
237700010615     C     SR_STARAP     BEGSR
237800010615      *
237900141016     .*//.......................................................................
238000010615      *
238100010615      * Stampo solo se diverso da zero
238200010615      *
238300010615     C                   Z-ADD     K4            B
238400010615     C                   DO        6             A                 2 0
238500010615     C                   MOVE      RPV(B)        CO4(A)
238600010615     C                   ADD       1             B
238700010615     C                   END
238800010615     C                   MOVEA     CO4           WRPV
238900010615      *
239000010615      * Se rapporto peso volume uguale a zero scrivo Volume compreso in tariffa
239100010615    1C                   if        wrpv = *ZEROS
239200010615      *
239300010615     C                   ELSE
239400010615      * carico la schiera da stampare
239500010615     C                   CLEAR                   SPV
239600010615     C                   Z-ADD     K4            B
239700010615     C                   DO        6             A
239800010615     C                   MOVE      RPV(B)        SPV(A)
239900010615     C                   ADD       1             B
240000010615     C                   END
240100010615    1C                   ENDIF
240200010615      *
240300010615     C                   ENDSR
240400010615     C*
240500141017
240600141016     .*//.......................................................................
240700010625
240800010618      *
240900010618      *-------------------------------------------------------------------------
241000010618      *  SR_STATPT  STAMPA TARIFFE PARTICOLARI
241100010618      *-------------------------------------------------------------------------
241200010618     C     SR_STATPT     BEGSR
241300010622      * accendo indicatore tariffe particolari
241400010622     c                   seton                                        10
241500021204
241600021204      * Pulisco le sk per stampa tariffe particolari su + righe
241700021204     c                   Clear                   Tpta
241800021204     c                   Clear                   Tptb
241900021204     c                   Clear                   Tptc
242000021204     c                   Clear                   Atp1
242100021204     c                   Clear                   Atpc
242200060307      * pulisco campo di comodo
242300060307     c                   clear                   savapl
242400021204
242500020109      * leggo la schiera ordinata delle tariffe particolari
242600020109      *
242700020109     c                   do        50            j
242800020109      * se valorizzata
242900020109     c                   if        cp(j) <> *blank
243000020109      * aggancio la tabella relativa le tariffe particolari
243100020109     c                   setoff                                       99
243200020109     c                   z-add     1             y
243300020109     c     cp(j)         lookup    ftc(y)                                 31
243400020109      * verifico se è una tariffa particolare da stampare  (Posta no Posta)
243500020109     c                   if        not *in31
243600020109     c                   seton                                        99
243700020109     c                   else
243800020109      *
243900020109     c                   endif
244000020109      *
244100020109     c                   SELECT
244200020109      * se tariffa stampabile (99 spento)
244300020109     c                   when      *in99
244400020109      * se è una tariffa particolare che vien stampata in modo standard
244500020109     c                   when      tst(y) = ' '
244600020109     c                   EXSR      SR_TAPARSTD
244700020109      * se è una tariffa particolare che non vien stampata in modo standard
244800020109     c                   when      tst(y) = 'N'
244900020109      * verifico che tariffa particolare è
245000020109      * elavazione limite risarcibile
245100020109     c                   if        ftc(y) = 'R'
245200020109     C                   EXSR      SR_ELLIRIS
245300020109     c                   endif
245400060301      * mandato assicurativo AC PLUS
245500020109     c                   if        ftc(y) = 'C'
245600020109     C                   EXSR      SR_MANDATO
245700020109     c                   endif
245800060301      * mandato assicurativo AC BASE
245900060301     c                   if        ftc(y) = 'd'
246000060301     c                   exsr      sr_acbase
246100060301     c                   endif
246200060816      * fuel surcharge
246300060816     c                   if        ftc(y) = 'f'
246400060816     c                   exsr      sr_fuel
246500060816     c                   endif
246600020109      *
246700020109     c                   ENDSL
246800020109      *
246900020109     c                   endif
247000020109      *
247100020109     c                   enddo
247200020109      *
247300010618     c                   ENDSR
247400021204
247500021204      *-------------------------------------------------------------------------
247600021204      *  Sr_StaTpt1   Stampa tariffe particolari da sk unica
247700021204      *-------------------------------------------------------------------------
247800021204     c     Sr_StaTpt1    BegSr
247900021204
248000141016     .*//.......................................................................
248100021204     c                   Eval      WokTp = *Off
248200021205      ***
248300021204      * Stampo prima le tariffe particolari con solo l'importo
248400021204Do  1c                   Do        50            Atp1
248500021204     c                   If        Tpta(Atp1) = *Blanks
248600021204     c                   Leave
248700021204     c                   EndIf
248800021204     c                   Eval      Woktp = *On
248900141016     .*//.......................................................................
249000021204    1c                   EndDo
249100141016     .*//.......................................................................
249200021204
249300021204     c                   Eval      WokTp = *Off
249400021205      ***
249500141016     .*//.......................................................................
249600021204
249700021204     c                   EndSr
249800010625      *
249900010625      *-------------------------------------------------------------------------
250000010625      *  SR_STATGC  STAMPA TARIFFE GIACENZA
250100010625      *-------------------------------------------------------------------------
250200010625     C     SR_STATGC     BEGSR
250300010625      *
250400010625     c                   if        *in98
250500141017     c     ds1ktm        chain     tiogc000                           99
250600010625      *
250700010625     c                   else
250800010625      *
250900141017     c     ds1ktm        chain     titgc000                           99
251000010625     c                   endif
251100010625      *
251200010625     c                   if        *in99 or tgcatb = 'A'
251300010625     c                   goto      endstatgc
251400010625     c                   endif
251500010928      *
251600141016     .*//.......................................................................
251700141016      *
251800010807      * se giorni di rientro sono a zero li recupero dalla tabella "2G"
251900010807      * in base al tipo tariffa
252000010807     c                   if        tgcggr = *zeros
252100010807      * tariffa italia
252200010807     c                   if        tamfie = 'I'
252300010807     c                   z-add     §2gggl        tgcggr
252400010807      * se DPD
252500010807     c   25              z-add     §2ggdp        tgcggr
252600010807      *
252700010807     c                   else
252800010807      * tariffa estera
252900010807     c                   z-add     §2ggre        tgcggr
253000010807      *
253100010807     c                   endif
253200010807      *
253300010807     c                   endif
253400010807      *
253500141016     .*//.......................................................................
253600010625      *
253700010625     c     endstatgc     endsr
253800010625      *
253900010618      *-------------------------------------------------------------------------
254000020109      *  SR_TAPARSTD STAMPA DATI DELLA TARIFFA PARTICOLARE STANDARD
254100010618      *-------------------------------------------------------------------------
254200020109     C     SR_TAPARSTD   BEGSR
254300010618      *
254400061207      * indicatore e campo di comodo per tariffa Fuel a scaglioni
254500061207      * sto stampando le tariffe particolari srandard ruindi pulisco il campo
254600061207      * e spengo l'indicatore
254700141016     .*//.......................................................................
254800061207     c                   eval      *in12 = *off
254900010618      *
255000020109     C                   movel(P)  ftc(y)        savftc
255100020109     c                   movel(p)  edtc(y)       comstd
255200020109      * se si tratta della tariffa particolare isola accendo
255300020109     c                   if        ftc(y) = 'I'
255400020109     C                   SETON                                        06
255500020109     c                   endif
255600010628      *
255700141017     C  n98KTPT          CHAIN     TITPT000
255800141017     c   98ktpt          chain     tiopt000
255900010618      * se non trovato vado a fine senza stampare
256000061122     c                   if        not %found or tptatb = 'A'
256100020109      * verifico se devo stampare al costo
256200020109     c                   if        edac(y) = 'S'
256300021204
256400021204      * se non devo stampare righe aggiuntive salvo la tariffa particolare
256500021204      * in una sk che stamperò alla fine di tutte le tariffe particolari
256600021204If  3c                   If        Esud(y) <> 'S'
256700021204     c                   Add       1             Atpc
256800021204     c                   Movel     Comstd        Tptc(Atpc)
256900021204
257000021204      * se devo stampare righe aggiuntive stampo anche la tariffa particolare
257100021204   x3c                   Else
257200141016     .*//.......................................................................
257300020109      * vado a stampare righe aggiuntive se richieste
257400020109     c                   goto      sudtpstd
257500021204    3c                   EndIf
257600021204
257700020109     c                   else
257800020109      * altrimenti vado a fine e non stampo nulla
257900020109     c                   goto      endtpstd
258000010618     c                   endif
258100010618      *
258200020109     c                   endif
258300010618      * cerco se ha record validi con tariffa o minimo > 0
258400010618     c                   EXSR      RICREC
258500010618      * per 04 acceso stampo nessun addebito
258600010618     c                   if        *in04
258700020109     c                   goto      endtpstd
258800010618     c                   endif
258900010625      *
259000010625      * per 04 spento verifico quanti record dettaglio tariffario esistono
259100010625      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
259200010625     c                   if        recval = 1
259300010625     C                   exsr      sr_gesunico
259400021204
259500021204      * se la tariffa particolare è di 1 riga con solo l'importo
259600021204      * salvo la tariffa particolare in una sk che stamperò alla fine di
259700021204      * tutte le tariffe particolari prima della sk al costo
259800021204If  2c                   If        Esud(y) <> 'S' and
259900021204     c                             W_Tarparest = *Blanks and
260000021204     c                             oltre = *Zeros and TpdMin = *Zeros and
260100021204     c                             TpdMax = *Zeros and TptTpg <> '0' and
260200021204     c                             TptRpv = *Zeros and TpTArl = *Zeros
260300021204     c                   Add       1             Atp1
260400021204     c                   Z-Add     TpdItr        Witr
260500021204     c                   Eval      Tpta(Atp1) = Comstd
260600021204      * no percentuale
260700021204If  3c                   If        W_Per = ' '
260800021204     c                   Eval      Tptb(Atp1) = ': ' +
260900021204     c                                          Wrkdestpg + ' ' +
261000021204     c                                          %Editc(Witr:'3')
261100021204      * sì percentuale
261200021204   x3c                   Else
261300021204     c                   Eval      Tptb(Atp1) = ': ' +
261400021204     c                                          Wrkdestpg + ' ' +
261500021204     c                                          StpPer + ' % '
261600021204    3c                   EndIf
261700021204
261800021204      * se devo stampare righe aggiuntive o l'estensione della tariffa
261900021204      * particolare stampo anche la tariffa particolare
262000021204   x2c                   Else
262100010723     c                   exsr      findet
262200021204    2c                   EndIf
262300010625     c                   else
262400010618      *
262500010618      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
262600010618      *
262700010625     c                   exsr      sr_gestpd
262800010625      *
262900010625     c                   endif
263000020109      *
263100020109     c     sudtpstd      tag
263200020109      *
263300141016     .*//.......................................................................
263400010618      *
263500020109     c                   setoff                                       06
263600020109      *
263700020109     C     endtpstd      endsr
263800010625      *-------------------------------------------------------------------------
263900010625      *  SR_ELLIRIS STAMPA DATI DELLA TARIFFA PARTICOLARE ELEVAZ.LIMITE RISARC.
264000010625      *-------------------------------------------------------------------------
264100010625     C     SR_ELLIRIS    BEGSR
264200010625      *
264300010625     C                   movel     'R '          savftc
264400010628      *
264500141017     C  n98KTPT          CHAIN     TITPT000
264600141017     c   98ktpt          chain     tiopt000
264700010625      * se non trovato vado a fine e non stampo nulla
264800061122     c                   if        not %found
264900010625     c                   goto      endelliris
265000010625     c                   endif
265100010625      * se trovato ma annullato vado a fine  e non stampo nulla
265200010625     c                   if        tptatb = 'A'
265300010625     c                   goto      endelliris
265400010625     c                   endif
265500010627      * se esiste stampo la scritta assicurazioni merci ed accendo indicatore
265600010627      * che mi segnala di aver stampato questa riga per non farlo dopo
265700010627      *
265800141016     .*//.......................................................................
265900010627     C                   SETON                                        81
266000010625      * recupero i valori standard dalla tabella GEI
266100010625     C                   CLEAR                   DGEI
266200010625     C                   MOVEL     'C'           T02MOD
266300010625     C                   MOVEL     KNSIF         T02SIF
266400010625     C                   MOVEL     'GEI'         T02COD
266500011001      *
266600010625     C                   MOVEL     §TADIV        T02KE1
266700010625      *
266800010625     C                   CALL      'TIBS02R'
266900010625     C                   PARM                    KPJBA
267000010625     C                   PARM                    DSBS02
267100010625      *
267200010625     C     T02ERR        IFEQ      *BLANKS
267300010625     C                   MOVEL     T02UNI        DGEI
267400010625     C                   ENDIF
267500010625     C*
267600010625      * verifico se limite risarcibile > di quello standard
267700010625     c                   if        tptvlm > §gelrp
267800010625     c                   seton                                        22
267900141016     .*//.......................................................................
268000010625      *
268100010625      * decodifico il flag valore merce
268200010625      *
268300010625     C                   movel     'TR'          COD
268400010625     C                   movel(P)  tptfvm        KEY
268500141017     c     tabkeyl       chain     tabel
268600010625      *
268700010625     c                   if        %found(tabel00f)
268800010625     c                   movel     TBLUNI        DSTR
268900010625     c                   else
269000010625     c                   clear                   DSTR
269100010625     c                   endif
269200010625      *
269300010625     c                   endif
269400141016     .*//.......................................................................
269500010625      *
269600010625      * cerco se ha record validi con tariffa o minimo > 0
269700010625     c                   EXSR      RICREC
269800011001      *
269900141016     .*//.......................................................................
270000011001      *
270100010625      * per 04 acceso stampo nessun addebito
270200010625     c                   if        *in04
270300141016     .*//.......................................................................
270400010625     c                   goto      endelliris
270500010625     c                   endif
270600010626      *
270700010626      * per 04 spento verifico quanti record dettaglio tariffario esistono
270800010626      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
270900010626     c                   if        recval = 1
271000010626     C                   exsr      sr_gesunico
271100141016      *
271200141016     .*//.......................................................................
271300010919      *
271400010919      * stampo eventuale estensione scaglione unico
271500010919      *
271600010919     c                   exsr      sr_estes
271700010723      *
271800010723     c                   exsr      findet
271900010723      *
272000010626     c                   else
272100010625      *
272200010625      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
272300010625      *
272400010625     c                   exsr      sr_gestpd
272500010626      *
272600010626     c                   endif
272700141016     .*//.......................................................................
272800010625      *
272900010625     C     endelliris    endsr
273000010625      *
273100010625      *-------------------------------------------------------------------------
273200010625      *  SR_MANDATO STAMPA DATI DELLA TARIFFA PARTICOLARE ASSICURAZIONE X CONTO
273300010625      *-------------------------------------------------------------------------
273400010625     C     SR_MANDATO    BEGSR
273500010625      *
273600010626     c                   setoff                                       2124
273700020109      *
273800020109     C                   movel     'C '          savftc
273900010625      *
274000010628      *
274100141017     C  n98KTPT          CHAIN     TITPT000
274200141017     c   98ktpt          chain     tiopt000
274300010625      * se non trovato vado a fine e non stampo nulla
274400061122     c                   if        not %found
274500010625     c                   goto      endmandato
274600010625     c                   endif
274700010625      * se trovato ma annullato vado a fine  e non stampo nulla
274800010625     c                   if        tptatb = 'A'
274900010625     c                   goto      endmandato
275000010625     c                   endif
275100010627      * se esiste stampo la scritta assicurazioni merci solo se 81 spento
275200010627      * cioè non l'ho già stampata prima
275300010627      *
275400141016     .*//.......................................................................
275500010627     C  n81              SETON                                        81
275600010625      * verifico se mandato fittizio
275700010625     c                   if        tpttma = 'F'
275800010625     c                   seton                                        24
275900010625     c                   endif
276000010625      * verifico se mandato a valore variabile
276100010625     c                   if        tptvlm = 0
276200010625     c                   seton                                        21
276300010625     c                   endif
276400060307      * mi salvo il tptapl x stampa AC BASE
276500060307     c                   eval      savapl = tptapl
276600060515      * se mandatao fittizio pulisco il tipo applicazione
276700060515     c                   if        *in24
276800060515     c                   clear                   savapl
276900060515     c                   endif
277000141016     .*//.......................................................................
277100010625      * se non è mandato a valore variabile e non è fittizio
277200010625     c                   if        not *in21 and not *in24
277300010625      *
277400010625      * decodifico il flag valore merce
277500010625      *
277600010625     C                   movel     'TR'          COD
277700010625     C                   movel(P)  tptfvm        KEY
277800141017     c     tabkeyl       chain     tabel
277900010625      *
278000010625     c                   if        %found(tabel00f)
278100010625     c                   movel     TBLUNI        DSTR
278200010625     c                   else
278300010625     c                   clear                   DSTR
278400010625     c                   endif
278500141016     .*//.......................................................................
278600010625      *
278700010625     c                   endif
278800010625      *
278900010625      * cerco se ha record validi con tariffa o minimo > 0
279000010625     c                   EXSR      RICREC
279100141016     .*//.......................................................................
279200011001      *
279300010625      * per 04 acceso stampo nessun addebito
279400010625     c                   if        *in04
279500141016     .*//.......................................................................
279600010625     c                   goto      endmandato
279700010625     c                   endif
279800010626      *
279900010626      * per 04 spento verifico quanti record dettaglio tariffario esistono
280000010626      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
280100010626     c                   if        recval = 1
280200010626     C                   exsr      sr_gesunico
280300010626      *
280400141016     .*//.......................................................................
280500010723      *
280600010723     c                   exsr      findet
280700010723      *
280800010626      *
280900010626     c                   else
281000010625      *
281100010625      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
281200010625      *
281300010625     c                   exsr      sr_gestpd
281400010626      *
281500010626     c                   endif
281600010625      *
281700141016     .*//.......................................................................
281800010625      *
281900010625     C     endmandato    endsr
282000010628      *
282100060301      *-------------------------------------------------------------------------
282200060301      *  sr_acbase STAMPA DATI DELLA TARIFFA PARTICOLARE d
282300060301      *-------------------------------------------------------------------------
282400060301     c     sr_acbase     begsr
282500060301
282600060301      * la tariffa ac base non può essere fittizia e non può essere a mandato
282700060301      * variabile
282800060301
282900060301     c                   movel     'd '          savftc
283000060301
283100141017     c  n98ktpt          chain     titpt000
283200141017     c   98ktpt          chain     tiopt000
283300060301      * se non trovato vado a fine e non stampo nulla
283400061122     c                   if        not %found
283500060301     c                   goto      endacbase
283600060301     c                   endif
283700060301      * se trovato ma annullato vado a fine  e non stampo nulla
283800060301     c                   if        tptatb = 'A'
283900060301     c                   goto      endacbase
284000060301     c                   endif
284100141016     .*//.......................................................................
284200060301     c  n81              seton                                        81
284300141016     .*//.......................................................................
284400060301      *
284500060301      * decodifico il flag valore merce
284600060301      *
284700060301     c                   movel     'TR'          cod
284800060301     c                   movel(p)  tptfvm        key
284900141017     c     tabkeyl       chain     tabel
285000060301
285100060301     c                   if        %found(tabel00f)
285200060301     c                   movel     tbluni        dstr
285300060301     c                   else
285400060301     c                   clear                   dstr
285500060301     c                   endif
285600060301
285700060301      * cerco se ha record validi con tariffa o minimo > 0
285800060301     c                   EXSR      RICREC
285900141016     .*//.......................................................................
286000060301      * per 04 acceso stampo nessun addebito
286100060301     c                   if        *in04
286200060301      * mandato a valore variabile o con valore espresso (no fittizio)
286300060301     c                   goto      endacbase
286400060301     c                   endif
286500060301      *
286600060301      * per 04 spento verifico quanti record dettaglio tariffario esistono
286700060301      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
286800060301     c                   if        recval = 1
286900060301     C                   exsr      sr_gesunico
287000060301
287100141016     .*//.......................................................................
287200060301
287300060301     c                   exsr      findet
287400060301
287500060301     c                   else
287600060301
287700060301      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
287800060301     c                   exsr      sr_gestpd
287900060301
288000060301     c                   endif
288100060301
288200141016     .*//.......................................................................
288300060301      *
288400060301     C     endacbase     endsr
288500060816      *
288600060816      *-------------------------------------------------------------------------
288700060816      *  sr_fuel   STAMPA DATI DELLA TARIFFA PARTICOLARE f
288800060816      *-------------------------------------------------------------------------
288900060816     c     sr_fuel       begsr
289000060816
289100060816     c                   movel     'f '          savftc
289200060816
289300141017     c  n98ktpt          chain     titpt000
289400141017     c   98ktpt          chain     tiopt000
289500060816      * se non trovato vado a fine e non stampo nulla
289600061122     c                   if        not %found
289700060816     c                   leavesr
289800060816     c                   endif
289900060816      * se trovato ma annullato vado a fine  e non stampo nulla
290000060816     c                   if        tptatb = 'A'
290100060816     c                   leavesr
290200060816     c                   endif
290300140218
290400140218     c                   eval      dTPT01 = TPTflo
290500140218     c                   eval      compma = §TPTpma
290600140218
290700060816      * cerco se ha record validi con tariffa o minimo > 0
290800060816     c                   exsr      ricrec
290900060816      * per 04 acceso tariffa negata
291000140218     c                   if        *in04 and compma = 0
291100061207      * non devo stampare la tariffa particolare
291200061207     c                   leavesr
291300061207     c                   endif
291400141016      *
291500141016     .*//.......................................................................
291600060816      * recupero il prezzo medio del gasolio
291700060816     c                   clear                   pmgpmg
291800141017     c     tptdpb        setll     tipmg000
291900141017     c                   read      tipmg000
292000060816      * devo controllare la data del prezzo medio che ho trovato
292100060816      * se la data del prezzo medio è troppo indietro non devo stampare
292200140224      * quindi stampo un errore, anche se il prezzo è a 0
292300140224      * faccio il controllo con 6 gg
292400061212     c                   if        pmgdtd > *zeros
292500060816     c     *iso          move      pmgdtd        data_iso1
292600061212     c                   else
292700061212     c                   eval      data_iso1 = *loval
292800061212     c                   endif
292900060816     c     *iso          move      tptdpb        data_iso2
293000060816     c     data_iso2     subdur    data_iso1     wgio:*d
293100140224     c                   IF        wgio > 6 or pmgpmg = 0
293200140224     c                   eval      errmsg = msg(1)
293300140224     c                   exsr      staerr
293400140224     c                   goto      fine
293500140224     c                   ENDIF
293600141016     .*//.......................................................................
293700060816      * stampo il prezzo medio del gasolio in base alla data indicata
293800060816      * in tariffa
293900141016     .*//.......................................................................
294000140219      * per 04 acceso vuol dire che la tariffa è tutta negata quindi
294100140219      * non devo stampare niente.
294200140219     c                   IF        *in04
294300140219     c                   ELSE
294400060816      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
294500060816      * verifico quanti record dettaglio tariffario esistono
294600060816      * se 1 scaglione solo devo stampare una dicitura particolare
294700080925     c                   if        recval = 1 and oltre = 0
294800141016     .*//.......................................................................
294900060816      * se maggiori di 1 gestisco la stampa standard delle tariffe particolari
295000060816      * con più scaglioni
295100060816     c                   else
295200141016     .*//.......................................................................
295300061207      * forzo l'accensione dell'indicatore 12 per stampare la dicitura
295400061207      * particolare per la tariffa fuel
295500061207     c                   eval      *in12 = *on
295600061207     c                   exsr      sr_gestpd
295700060816     c                   endif
295800140219     c                   ENDIF
295900141016     .*//.......................................................................
296000060816
296100060816     c                   endsr
296200060301      *
296300010625      *-------------------------------------------------------------------------
296400010625      * SR_GESTPD GESTIONE DEL DETTAGLIO TARIFFARIO UGUALE PER TUTTE LE TAR.PAR.
296500010625      *-------------------------------------------------------------------------
296600010625     c     SR_GESTPD     BEGSR
296700010625      * Decodfico tipo tariffa e scaglioni
296800010625     c                   movel     'TR'          COD
296900010625      *
297000010625     C                   CLEAR                   ATF
297100010625     c                   EXSR      DECTPG
297200010625      * elaboro 6 scaglioni alla volta
297300010625     c                   z-add     1             k4
297400010625      *
297500010625    1c                   dow       k4 <= 18
297600010625     C                   MOVEA     SGLD(K4)      SSLD
297700010625     C                   MOVEA     SGLB(K4)      SSLB
297800010625     C                   MOVEA     SGLA(K4)      SSLA
297900010625      * 1 DECIMALE
298000080901     C                   IF        *IN91 AND NOT *IN37 and not *in38
298100010625     C                   MOVEA     SG1A(K4)      SS1A
298200010625     C                   MOVEA     SG1D(K4)      SS1D
298300010625     C                   ENDIF
298400010625      * 2 DECIMALI
298500080901     C                   IF        *IN91 AND *IN37 and not *in38
298600010625     C                   MOVEA     SG2A(K4)      SS2A
298700010625     C                   MOVEA     SG2D(K4)      SS2D
298800010625     C                   ENDIF
298900080901      * 3 DECIMALI
299000080901     C                   IF        *IN91 AND *IN37 and  *in38
299100080901     C                   MOVEA     SG3A(K4)      SS3A
299200080901     C                   MOVEA     SG3D(K4)      SS3D
299300080901     C                   ENDIF
299400010625      *
299500010625      * CARICO GLI SCAGLIONI PER ELABORARE IL DETTAGLIO
299600010625      *
299700080916     C                   MOVEA     SCP(K4)       SSLP
299800010625     C*  INTESTAZIONE SCAGLIONI
299900141017     ***C                   Z-ADD     SSLP(1)       W0050             5 0
300000141017     ***C     W0050         COMP      99999                                  41
300100141017     ***C     SSLP(1)       COMP      *ZEROS                                 51
300200141017     ***C                   Z-ADD     SSLP(2)       W0050             5 0
300300141017     ***C     W0050         COMP      99999                                  42
300400141017     ***C     SSLP(2)       COMP      *ZEROS                                 52
300500141017     ***C                   Z-ADD     SSLP(3)       W0050             5 0
300600141017     ***C     W0050         COMP      99999                                  43
300700141017     ***C     SSLP(3)       COMP      *ZEROS                                 53
300800141017     ***C                   Z-ADD     SSLP(4)       W0050             5 0
300900141017     ***C     W0050         COMP      99999                                  44
301000141017     ***C     SSLP(4)       COMP      *ZEROS                                 54
301100141017     ***C                   Z-ADD     SSLP(5)       W0050             5 0
301200141017     ***C     W0050         COMP      99999                                  45
301300141017     ***C     SSLP(5)       COMP      *ZEROS                                 55
301400141017     ***C                   Z-ADD     SSLP(6)       W0050             5 0
301500141017     ***C     W0050         COMP      99999                                  46
301600141017     ***C     SSLP(6)       COMP      *ZEROS                                 56
301700010625     C*
301800010625     C* SE IL 41 E' ACCESO PERCHÈ IL PRIMO SCAGLIONE E' UGUALE A TUTTI 999999
301900010627     C* O IL SECONDO SCAGLIONE E' A ZERO
302000010627     C* MA SE NON SI TRATTA + DEL 1° SCAGLIONE IN ASSOLUTO DEVO STAMPARE "OLTRE
302100010625     C* LO SCAGLIONE PRECEDENTE
302200141017     ***C* QUINDI ACCENDO IL 40 E SPENGO IL 41
302300141017     ***C*
302400141017     ***C                   IF        *IN41 AND K4 <> 1
302500141017     ***C                   SETOFF                                       41
302600141017     ***C                   SETON                                        40
302700141017     ***C                   ENDIF
302800010625      *
302900010625     C                   EXSR      DECSCA
303000010625     C* LETTURA DEL DETTAGLIO
303100010625     C                   EXSR      LEGDET
303200010625      *
303300010625      * rielaboro altri 6 scaglioni
303400010625      *
303500010625     c                   add       6             k4
303600010625      * se scaglioni minori o uguali a 18 e scaglione valorizzato
303700010625    2c                   if        k4 <= 18 and
303800010625     c                             scp(k4) <> *zeros
303900010625      * pulisco schiere di comodo per la stampa degli scaglioni
304000010625     c                   clear                   ssld
304100010625     c                   clear                   sslb
304200010625     c                   clear                   ssla
304300010625     c                   clear                   ss1d
304400010625     c                   clear                   ss1a
304500010625     c                   clear                   ss2d
304600010625     c                   clear                   ss2a
304700080901     c                   clear                   ss3d
304800080901     c                   clear                   ss3a
304900080916     c                   clear                   sslp
305000141017     *** * spengo indicatori che condizionano la stampa
305100141017     ***c                   setoff                                       404142
305200141017     ***c                   setoff                                       434445
305300141017     ***c                   setoff                                       46
305400141017     ***c                   setoff                                       515253
305500141017     ***c                   setoff                                       545556
305600010625      *
305700010625     c                   else
305800010625      * non ci sono + scaglioni da stampare
305900010625     c                   eval      k4=19
306000010625     c                   leave
306100010625      *
306200010625    2c                   endif
306300010625      *
306400010625    1c                   enddo
306500010625      *
306600010625     C*
306700010625     C                   EXSR      FINDET
306800010625      *
306900010625     c                   endsr
307000010625      *-------------------------------------------------------------------------
307100010625      * SR_GESUNICO GESTIONE DETTAGLIO TARIFFARIO CON UNA SOLA RIGA
307200010625      *-------------------------------------------------------------------------
307300010625     c     SR_GESUNICO   BEGSR
307400010625      *
307500010625     c                   clear                   w_tarpar
307600010731     c                   clear                   w_tarparest
307700011001     c                   clear                   w_ellimris
307800010626      * Decodfico tipo pagamento
307900010625     c                   movel     'TR'          COD
308000010625     C                   movel(P)  tpttpg        KEY
308100141017     c     tabkeyl       chain     tabel
308200010625      *
308300010625     c                   if        %found(tabel00f)
308400010625     c                   movel     TBLUNI        DSTR
308500010625     c                   else
308600010625     c                   clear                   DSTR
308700010625     c                   endif
308800010625      *
308900010625      * verifico se scaglioni a valore
309000010625     c                   if        §trtap = 'S'
309100010625     c                   eval      W_per = '%'
309200010625     c                   else
309300010625     c                   clear                   W_per
309400010625     c                   endif
309500010626      *
309600010626      * decodifico a mio modo il tipo pagamento
309700010626      *
309800141016     C                   CLEAR                   WRKDESTPG
309900141016      *
310000141016     c                   if        tpttpg = 'T'
310100141016     c                   eval      wrkdestpg = 'SUL TRASPORTO'
310200141016     c                   endif
310300141016
310400141016     c                   if        tpttpg = 'V'
310500141016     c                   eval      wrkdestpg = 'SUL VALORE   '
310600141016     c                   endif
310700141016
310800141016     c                   if        tpttpg = '0'
310900141016     c                   eval      wrkdestpg = 'A Q.LE       '
311000141016     c                   endif
311100141016
311200141016     c                   if        tpttpg = '1'
311300141016     c                   eval      wrkdestpg = 'A COLLO      '
311400141016     c                   endif
311500141016
311600141016     c                   if        tpttpg = '2'
311700141016     c                   eval      wrkdestpg = 'A SPEDIZIONE '
311800141016     c                   endif
311900141016
312000141016     c                   if        tpttpg = '3'
312100141016     c                   eval      wrkdestpg = 'A KG.        '
312200141016     c                   endif
312300141016
312400141016     c                   if        tpttpg = '4'
312500141016     c                   eval      wrkdestpg = 'A VALORE     '
312600141016     c                   endif
312700141016
312800141016     c                   if        tpttpg = '5'
312900141016     c                   eval      wrkdestpg = 'A QUANTITA'' '
313000141016     c                   endif
313100141016      * Tar.particolare Diritto Fatturazione
313200141016     c                   If        TptFtc = '§'
313300141016     c                   Eval      WrkDesTpg = 'A FATTURA   '
313400141016     c                   EndIf
313500141016      * Tar.particolare Gest.bancali a rendere
313600141016     c                   If        TptFtc = '='
313700141016     c                   Eval      WrkDesTpg = 'A BANCALE   '
313800141016     c                   EndIf
313900141016
314000141016     .*//.......................................................................
314100010625      *
314200010625      * CERCO IL RECORD VALIDO
314300010625      *
314400010625      *
314500141017     C  n98ktpt          setll     titpd000
314600141017     c   98ktpt          setll     tiopd000
314700010625      *
314800010625     c                   do        *hival
314900141017     C  n98ktpt          reade     titpd000
315000141017     c   98ktpt          reade     tiopd000
315100010625      *
315200061122     c                   if        %eof
315300010625     c                   leave
315400010625     c                   endif
315500010625      *
315600010625     c                   if        tpdatb = 'A'
315700010625     c                   iter
315800010625     c                   endif
315900010625      *
316000010625      * se tariffa non valorizzata leggo la successiva
316100010625      * importo e minimo =  a zero
316200010625      *
316300010625     C                   IF        tpditr = 0 and tpdmin = 0
316400010625     c                   iter
316500010625     c                   else
316600010723      * se importo con percentuale verifico quanti decimali ha
316700010723     c                   if        W_per = '%'
316800010723     c                   z-add     tpditr        valper
316900010723     C                   eval      stpper = %EDITC(valper : '3')
317000010723      * VERIFICO SE HA DECIMALI VALORIZZATI
317100010723     C                   move      valper        w0030
317200010723     C                   move      w0030         w0020
317300010723     C                   move      w0020         w0010
317400010723      * tolgo gli zeri non significativi dopo la virgola
317500010723     c                   select
317600010723      *
317700010723     C                   when      w0030 = *zeros
317800010723     C                   move      '    '        stpper
317900010723      *
318000010723     c                   when      w0020 = *zeros
318100010723     c                   move      '  '          stpper
318200010723      *
318300010723     c                   when      w0010 = *zero
318400010723     c                   move      ' '           stpper
318500010723      *
318600010723     c                   endsl
318700010723      *
318800010723     c                   endif
318900011001      * se sto stampando l'elevazione limite risarcibile mi devo comportare
319000011009      * diversamente se tariffa a valore  e non è definito OLTRE e c'e
319100011009      * elevazione al limite
319200011001     c                   if        W_per = '%' and savftc = 'R ' and oltre = 0
319300011009     c                             and *in22
319400011001      * non definiscp W_tarpar in quanto gestisco in modo diverso
319500011001      * valorizzo il campo W_ellimris
319600011001     c                   eval      w_ellimris = 'Y'
319700011001      *
319800011001      * se esiste minimo e massimo devo valorizzare l'estensione
319900011001      *
320000011001      * solo minimo
320100011001      *
320200011001     c                   if        tpdmin >  0 and tpdmax = 0
320300021205     c                   eval      W_TARPAREST = blk56
320400011001     C                             + ' minimo........' +
320500011001     c                             %editc(tpdmin:'3')
320600011001     c                   endif
320700011001      *
320800011001      * solo massimo
320900011001      *
321000011001     c                   if        tpdmin =  0 and tpdmax > 0
321100021205     c                   eval      W_TARPAREST = blk56
321200011001     C                             + ' massimo.......' +
321300011001     c                             %editc(tpdmax:'3')
321400011001     c                   endif
321500011001      *
321600011001      * sia mino che massimo
321700011001      *
321800011001     c                   if        tpdmin >  0 and tpdmax > 0
321900030318     c                   eval      W_TARPAREST = blk10 + blk10 + blk12
322000060908     C                             + '...... minimo...' +
322100011001     c                             %editc(tpdmin:'3')
322200011009     C                             + ' massimo..' +
322300011001     c                             %editc(tpdmax:'3')
322400011001     c                   endif
322500011001     c*
322600011001     c                   leave
322700011001     c                   endif
322800010731      *
322900010731      * verifico se devo stampare OLTRE
323000010731      *
323100010731     c                   if        oltre > 0
323200020114      *
323300020114      * verifico se ci sono decimali valorizzati
323400020114      *
323500020114     c                   clear                   *in88
323600020114     c                   clear                   oltrenodec
323700020114     C                   move      oltre         w0030
323800020114     C                   eval      *in88 =(w0030 > 0)
323900020114      * oltre senza decimali
324000020114     c                   if        not *in88
324100020114     c                   z-add     oltre         oltrenodec
324200020114     c                   endif
324300010731      * controllo se tariffa a q.le
324400010731     c                   if        tpttpg = '0'
324500010731     c                   eval      unmis = 'KG.'
324600010731     c                   else
324700010731     c                   clear                   unmis
324800010731     c                   endif
324900010731      *
325000010731      * stampo nella prima riga il valore e nella riga di estensione se ci sono
325100010731      * stampo il minimo e massimo
325200010731      *
325300010731      * no percentuale
325400010731     c                   if        w_per = ' '
325500020114     c   88              eval      W_TARPAR = ': ' + wrkdestpg +
325600010731     c                             'oltre ' + (%editc(oltre:'3'))+
325700030318     c                             ' ' + unmis + blk10 + blk10 + blk12
325800021205     c                                                 + %editc(tpditr:'3')
325900020114      * no decimali
326000020114     c  n88              eval      W_TARPAR = ': ' + wrkdestpg +
326100020114     c                             'oltre ' + (%editc(oltrenodec:'3'))+
326200021205     c                             ' ' + unmis + blk36 + %editc(tpditr:'3')
326300010731     c                   else
326400010731      * si percentuale
326500020114     c   88              eval      W_TARPAR = ': ' +
326600010731     c                             wrkdestpg + ' ' + stpper + ' % ' +
326700141016
326800030318     c                             ' ' + unmis + blk21
326900020114      * no decimali
327000020114     c  n88              eval      W_TARPAR = ': ' +
327100020114     c                             wrkdestpg + ' ' + stpper + ' % ' +
327200020114     c                             'oltre ' + (%editc(oltrenodec:'3'))+
327300030318     c                             ' ' + unmis + blk25
327400010731     c                   endif
327500010731      *
327600010731      * se esiste minimo e massimo devo valorizzare anche l'estensione
327700010731      * tariffe particolari con l'oltre
327800010731      *
327900010731      * solo minimo
328000010731      *
328100010731     c                   if        tpdmin >  0 and tpdmax = 0
328200021205     c                   eval      W_TARPAREST = blk56
328300010731     C                             + ' minimo........' +
328400010731     c                             %editc(tpdmin:'3')
328500010731     c                   endif
328600010731      *
328700010731      * solo massimo
328800010731      *
328900010731     c                   if        tpdmin =  0 and tpdmax > 0
329000021205     c                   eval      W_TARPAREST = blk56
329100010731     C                             + ' massimo.......' +
329200010731     c                             %editc(tpdmax:'3')
329300010731     c                   endif
329400010731      *
329500010731      * sia mino che massimo
329600010731      *
329700010731     c                   if        tpdmin >  0 and tpdmax > 0
329800030318     c                   eval      W_TARPAREST = blk10 + blk10 + blk12
329900060908     C                             + '...... minimo...' +
330000010731     c                             %editc(tpdmin:'3')
330100011009     C                             + ' massimo..' +
330200010731     c                             %editc(tpdmax:'3')
330300010731     c                   endif
330400010731      *
330500010731     c                   else
330600010731      *
330700010731     c
330800010723      * valorizzo il campo che conterrà la percetuale
330900010625      * se non c'è ne minimo ne massimo
331000010625     c                   if        tpdmin = 0 and tpdmax =  0
331100010723      * no percentuale
331200010723     c                   if        w_per = ' '
331300010626     c                   eval      W_TARPAR = ': ' + wrkdestpg +
331400021205     c                             blk56 + %editc(tpditr:'3')
331500010723     c                   else
331600010723      * si percentuale
331700010723     c                   eval      W_TARPAR = ': ' +
331800030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
331900030318     c                             blk10 + blk10 + blk25
332000010723     c                   endif
332100010723      *
332200010625     c                   endif
332300010625      * se c'è minimo
332400010625     c                   if        tpdmin > 0 and tpdmax = 0
332500010723      * no percentuale
332600010723     c                   if        w_per = ' '
332700010626     c                   eval      W_TARPAR = ': ' + wrkdestpg +
332800030318     c                             blk10 + blk10 + blk12
332900060302     c                             + ' ' + %editc(tpditr:'3')
333000010626     C                             + ' minimo...' +
333100010723     c                             %editc(tpdmin:'3')
333200010723     c                   else
333300010723      * si percentuale
333400010723     c                   eval      W_TARPAR = ': ' +
333500030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
333600030318     c                             blk10 + blk25
333700010723     C                             + ' minimo...' +
333800010723     c                             %editc(tpdmin:'3')
333900010723     c                   endif
334000010723      *
334100010625     c                   endif
334200010625      * se c'è solo massimo
334300010625     c                   if        tpdmin = 0 and tpdmax > 0
334400010723      * no percentuale
334500010723     c                   if        w_per = ' '
334600010626     c                   eval      W_TARPAR = ': ' + wrkdestpg +
334700030318     c                             blk10 + blk10 + blk12
334800060302     c                             + ' ' + %editc(tpditr:'3')
334900010626     C                             + ' massimo..' +
335000010626     c                             %editc(tpdmax:'3')
335100010723     c                   else
335200010723      * si percentuale
335300010723     c                   eval      W_TARPAR = ': ' +
335400030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
335500030318     c                             blk10 + blk25
335600010723     C                             + ' massimo..' +
335700010723     c                             %editc(tpdmax:'3')
335800010723     c                   endif
335900010723      *
336000010625     c                   endif
336100010625      * se c'è minimo e massimo
336200010625     c                   if        tpdmin > 0 and tpdmax > 0
336300010723      * no percentuale
336400010723     c                   if        w_per = ' '
336500010626     c                   eval      W_TARPAR = ': ' + wrkdestpg +
336600021205     c                             ' ' + %editc(tpditr:'3') + blk10
336700060908     c                             + ' minimo....' +
336800010626     c                             %editc(tpdmin:'3')
336900010626     C                             + ' massimo..' +
337000010626     c                             %editc(tpdmax:'3')
337100010723     c                   else
337200010723      * si percentuale
337300010723     c                   eval      W_TARPAR = ': ' +
337400030318     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
337500060908     c                             + ' minimo..' +
337600010723     c                             %editc(tpdmin:'3')
337700060908     C                             + ' massimo...' +
337800010723     c                             %editc(tpdmax:'3')
337900010723     c                   endif
338000010723      *
338100010625     c                   endif
338200010731      *
338300010731     c                   endif
338400010625     c                   leave
338500010625     c                   endif
338600010625     C
338700010625      *
338800010625     c                   enddo
338900010625      *
339000010625     c                   ENDSR
339100010625      *
339200010618      *-------------------------------------------------------------------------
339300010618      * RICREC  CONTROLLO SE ESISTE UN RECORD VALIDO E CON TARIFFA+MINIMO > 0
339400010618      *-------------------------------------------------------------------------
339500010618     c     RICREC        BEGSR
339600010618      *
339700010618     c                   setoff                                       04
339800010625     c                   z-add     *zeros        recval
339900010626     c                   clear                   wrkftc
340000010626     c                   clear                   wrkorp
340100010626     c                   clear                   wrksgl
340200010806     c                   clear                   recsgl
340300010724     c                   clear                   oltre
340400010724     c                   clear                   mintas
340500010801     c                   clear                   savorp1
340600010731     c                   clear                   W_TARPAREST
340700010618      *
340800141017     C  n98ktpt          setll     titpd000
340900141017     c   98ktpt          setll     tiopd000
341000010618      *
341100010618     c                   do        *hival
341200141017     C  n98ktpt          reade     titpd000
341300141017     c   98ktpt          reade     tiopd000
341400010618      *
341500061122     c                   if        %eof
341600010801      * se non ho il recval valorizzato accendo lo 04
341700010625     c                   if        recval = 0
341800010618     c                   seton                                        04
341900010625     c                   endif
342000010625      *
342100010618     c                   leave
342200010618     c                   endif
342300010618      *
342400010618     c                   if        tpdatb = 'A'
342500010618     c                   iter
342600010618     c                   endif
342700010618      *
342800010618      * se tariffa non valorizzata leggo la successiva
342900010618      * importo e minimo =  a zero
343000010618      * se valorizzata esco per gestire la stampa del dettaglio tariffario
343100010618      *
343200010618     C                   IF        tpditr = 0 and tpdmin = 0
343300010724      * mi salvo il valore dello scaglione precedente per stampare in un even-
343400010806      * tuale oltre nella riga della tariffa particolare se diverso da 99999
343500010806     c                   z-add     tpdsgl        w0050
343600010806     c                   if        w0050 <> 99999
343700010806      *
343800010724     c                   z-add     tpdsgl        oltre
343900010724      * mi salvo il primo scaglione per stampare il minimo tassabile in caso di
344000010724      * tariffa particolare a qle
344100010724     c                   if        mintas = 0
344200010724     c                   z-add     tpdsgl        mintas
344300010724     c                   endif
344400010806      *
344500010806     c                   endif
344600010801      * mi salvo ordinamento regione e provincia la prima volta
344700010801     c                   if        savorp1 = *blanks
344800010801     c                   eval      savorp1 = tpdorp
344900010806      * memorizzo la chiave che mi serve per riaggancare il primo
345000010806      * record valido
345100010806     c                   eval      wrkorp = tpdorp
345200010806     c                   eval      wrkftc = tpdftc
345300010806     c                   eval      wrksgl = tpdsgl
345400010806      *
345500010806     c                   endif
345600010724      *
345700010618     c                   iter
345800010618     c                   else
345900010625     c                   add       1             recval
346000010806      * memorizzo lo scaglioni perc hè nel caso di unisco scaglione valido
346100010806      * stampo unico scaglione solo se scaglione = a 99999
346200010806     c                   eval      recsgl = tpdsgl
346300010806      * memorizzo la chiave che mi serve per riaggancare il primo
346400010806      * record valido se è = a blanks
346500010806     c                   if        recval = 1 and wrkftc = '  '
346600010806     c                   eval      wrkorp = tpdorp
346700010806     c                   eval      wrkftc = tpdftc
346800010806     c                   eval      wrksgl = tpdsgl
346900010806     c                   endif
347000010626      * se trovati + di 1 record non proseguo in lettura
347100010626      *
347200010626     c                   if        recval > 1
347300010626      * mi riposiziono sul prima record valido trovato
347400141017     c  n98ktpdwrk       chain     titpd000
347500141017     c   98ktpdwrk       chain     tiopd000
347600010626      *
347700010626     c                   leave
347800010626     c                   else
347900010625     c                   iter
348000010626     c                   endif
348100010626      *
348200010618     c                   endif
348300010618     C*
348400010618    2C                   enddo
348500010801      * se al primo record valido verifico che non ha lo stesso ordinamento
348600010801      * regione e provincia del record a zero forzo per la stampa in dettaglio
348700010801      *
348800010801     c                   if        recval = 1 and tpdorp <> savorp1 and
348900010801     c                             savorp1 <> *blanks
349000010801     c                   eval      recval = 9
349100010806      * mi riposiziono sul prima record valido trovato
349200141017     c  n98ktpdwrk       chain     titpd000
349300141017     c   98ktpdwrk       chain     tiopd000
349400010801     c                   clear                   savorp1
349500010801     c                   endif
349600010724      * Se ho trovato un solo record valido verifico lo scaglione trovato
349700010801     c                   if        recval = 1
349800010806     c                   z-add     recsgl        w0050
349900011012     c                   if        w0050 <> 99999 and tpttpg <> '2'
350000010724      * se diverso da 99999 devo gestire il tutto come se avesse + scaglioni
350100011012      * e tipo tariffa non è a spedizione
350200010724     c                   eval      recval = 9
350300010724      * mi riposiziono sul prima record valido trovato
350400141017     c  n98ktpdwrk       chain     titpd000
350500141017     c   98ktpdwrk       chain     tiopd000
350600010724     c                   endif
350700010724     c                   endif
350800010618     C*
350900010618     C                   ENDSR
351000010618      *-------------------------------------------------------------------------
351100010618      *  DECTPG DECODIFICA TIPO TARIFFA E SCAGLIONI
351200010618      *-------------------------------------------------------------------------
351300010618     C     DECTPG        BEGSR
351400010618      *
351500010618     c                   setoff                                       09
351600010618      *
351700010618      * decodifico il tipo tariffa
351800010618      *
351900010618     C                   movel(P)  tpttpg        KEY
352000141017     c     tabkeyl       chain     tabel
352100010618      *
352200010618     c                   if        %found(tabel00f)
352300010618     c                   movel     TBLUNI        DSTR
352400010618     c                   else
352500010618     c                   clear                   DSTR
352600010618     c                   endif
352700010618      *
352800010618     C                   movel(P)  §TRDE1        DE1TPG
352900010618     C                   movel(P)  §TRDE3        DE3TPG
353000140522      * minimo tassabile
353100140522     c                   z-add     §trata        ctrata
353200010618      * verifico se scaglioni a valore
353300010618     c                   eval      *in09 = (§trtap = 'S')
353400010618      **
353500010618      * SCAGLIONI
353600010618      *
353700010618      * memorizzo gli scaglioni del dettaglio tariffa particolare
353800010618      *
353900010618     c                   EXSR      MEMSCA
354000010618      *
354100010618     c                   ENDSR
354200010618      *-------------------------------------------------------------------------
354300010618      *  MEMSCA CARICO SCAGLIONI TARIFFE PARTICOLARI
354400010618      *-------------------------------------------------------------------------
354500010618     C     MEMSCA        BEGSR
354600010618      *
354700010621     C                   SETOFF                                       089137
354800141017     c                   setoff                                       38
354900141017     ***c                   setoff                                       4041
355000010618      *
355100010618     C                   MOVEA     *ZEROS        SCP
355200080916     C                   MOVEA     *ZEROS        SSLP
355300010621     c                   clear                   sgld
355400010621     c                   clear                   sg1d
355500010621     c                   clear                   sg2d
355600080901     c                   clear                   sg3d
355700010621     c                   clear                   sgla
355800010621     c                   clear                   sg1a
355900010621     c                   clear                   sg2a
356000080901     c                   clear                   sg3a
356100010618      *
356200010618     c                   eval      rotcts = tpdcts
356300010618     c                   eval      rotpro = %subst(tpdorp:5:1)
356400010618     c                   eval      rotreg = %subst(tpdorp:3:2)
356500010618      * SALVO PER LETTURE FUTURE
356600010618     c                   eval      wrkcts = rotcts
356700010618     c                   eval      wrkpro = rotpro
356800010618     c                   eval      wrkreg = rotreg
356900010618      *
357000010618      * CERCO IL DETTAGLIO TARIFFARIO
357100010618      *
357200141017     C  n98ktpt          setll     titpd000
357300141017     c   98ktpt          setll     tiopd000
357400010618      *
357500010621    1c                   do        *hival
357600141017     C  n98ktpt          reade     titpd000
357700141017     c   98ktpt          reade     tiopd000
357800010618      *
357900061122    2c                   if        %eof
358000010618     c                   seton                                        04
358100010618     c                   leave
358200010621    2c                   endif
358300010618      *
358400010621    2c                   if        tpdatb = 'A'
358500010618     c                   iter
358600010621    2c                   endif
358700010618      *
358800010618     c                   exsr      regdet
358900010618     c   99              goto      FINE
359000010618     c                   exsr      prodet
359100010618     c   99              goto      FINE
359200010618      *
359300010618      * carico il primo scaglione
359400010618      *
359500010621     C                   z-add     tpdsgl        SCP(1)
359600010621      *
359700010801      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
359800010801      * a valore
359900080901     c                   if        not *in09 or tpdftc = 'f'
360000010801      *
360100010621     c                   move      tpdsgl        w0030
360200010621     c                   z-add     tpdsgl        w0050
360300010621      * se scaglione 99999 non controllo i decimali
360400010621    2c                   if        w0050 <> 99999 and w0030 > 0
360500010621     c                   seton                                        91
360600010621    2c                   endif
360700010621      * se con decimali verifico se valorizzato dal secondo in poi
360800010621    2c                   if        *in91
360900080901     c                   z-add     w0030         w0010
361000010621     c                   z-add     w0030         w0020
361100010621    3c                   if        w0020 > 0
361200010621     c                   seton                                        37
361300010621    3c                   endif
361400080901    3c                   if        w0010 > 0
361500080901     c                   seton                                        38
361600080901    3c                   endif
361700080910      * se si tratta di fuel surcharge faccio sempre 3 decimali
361800080910     c                   If        tpdftc = 'f'
361900080910     c                   seton                                        3837
362000080910    2c                   endif
362100010621    2c                   endif
362200010801      *
362300010801     c                   endif
362400050324
362500050324      * Se scaglione non è 99999
362600050324      * controllo che non sia maggiore di 9999 xchè non ci sta in stampa
362700050324      * in questo caso ERRORE ed esco dal programma
362800050324     c                   z-add     tpdsgl        w0050
362900050324     c                   If        w0050 <> 99999 and tpdsgl > 9999
363000050324     c                   move      msg(16)       errmsg
363100050324     c                   exsr      staerr
363200050324     c                   Goto      FINE
363300050324     c                   EndIf
363400010622      *
363500010622     C                   LEAVE
363600010622      *
363700010622    1C                   ENDDO
363800010622      *
363900010618     C                   z-add     1             K
364000010618      *
364100010618      * LETTURA TPD CON KEY KTPT CON LO STESSO CODICE TASSAZIONE
364200010618      *
364300010621    2C                   DO        *HIVAL
364400010618      *
364500141017     C  n98ktpt          reade     titpd000
364600141017     c   98ktpt          reade     tiopd000
364700010618      * se fine lettura
364800061122     C                   IF        %eof
364900010618     c                   leave
365000010618     c                   endif
365100010618      * se codice tassazione diverso
365200010618     c                   if        tpdcts <> rotcts
365300010618     c                   leave
365400010618     c                   endif
365500010618      * se annullato leggo il successivo
365600010618     c                   if        tpdatb <> ' '
365700010618     c                   iter
365800010618     c                   endif
365900010618      *
366000010618     C                   add       1             K
366100010618      * fino a 18 scaglioni carico
366200010621    3C                   if        k <= 18
366300010618      *
366400010621     C                   z-add     tpdsgl        SCP(K)
366500010621      *
366600010801      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
366700010801      * a valore
366800080901     c                   if        not *in09 or tpdftc = 'f'
366900010801      *
367000010621     c                   move      tpdsgl        w0030
367100010621     c                   z-add     tpdsgl        w0050
367200010621      * se scaglione 99999 non controllo i decimali
367300010621     c                   if        w0050 <> 99999 and w0030 > 0
367400010621     c                   seton                                        91
367500010621     c                   endif
367600010621      * se con decimali verifico se valorizzato dal secondo in poi
367700010621     c                   if        *in91
367800010621     c                   z-add     w0030         w0020
367900080901     c                   z-add     w0030         w0010
368000010621     c                   if        w0020 > 0
368100010621     c                   seton                                        37
368200010621     c                   endif
368300080901    3c                   if        w0010 > 0
368400080901     c                   seton                                        38
368500080901    3c                   endif
368600010621     c                   endif
368700010801      *
368800010801     c                   endif
368900050324
369000050324      * Se scaglione non è 99999
369100050324      * controllo che non sia maggiore di 9999 xchè non ci sta in stampa
369200050324      * in questo caso ERRORE ed esco dal programma
369300050324     c                   z-add     tpdsgl        w0050
369400050324     c                   If        w0050 <> 99999 and tpdsgl > 9999
369500050324     c                   move      msg(16)       errmsg
369600050324     c                   exsr      staerr
369700050324     c                   Goto      FINE
369800050324     c                   EndIf
369900010618      *
370000010621   x3C                   ELSE
370100010618      *
370200010618     C                   move      msg(15)       errmsg
370300010618     C                   exsr      staerr
370400050914      * Per errore esco dal pgm
370500050914     c                   Goto      FINE
370600010621    3C                   endif
370700010618      *
370800010621    2C                   enddo
370900010618      * memorizzo il numero scaglioni caricati
371000010618     C                   z-add     k             wnumsc
371100010618      *
371200010621      * preparo la schiera degli scaglioni da stampare
371300010621      *
371400010621     c                   z-add     1             k
371500010621     c                   clear                   k1
371600010621      *
371700010621    2c                   dow       scp(k) > 0
371800010621      * preparo lo scaglione DAL addizionato di 1 o 0,1 o 0,01 in base ai decima
371900010621      * li
372000010621     c                   z-add     scp(k)        w0050
372100010621      *
372200010621      * verifico se scaglione 99999
372300010621    3c                   if        w0050 <> 99999
372400010621      *
372500010621    4c                   if        k1 > 0
372600010621      * se non ha decimali
372700010621     c                   if        not *in91
372800010622     C                   eval      sgld(k) = scp(k1) + 1
372900010621     c                   endif
373000010621      *
373100080901     c                   if        *in91 and not *in37 and not *in38
373200010621      * se ha 1 decimale
373300010622     C                   eval      sg1d(k) = scp(k1) + 0,1
373400010621     c                   endif
373500010621      *
373600080901     c                   if        *in91 and *in37 and not *in38
373700010621      * se ha + di 1 decimale
373800010622     C                   eval      sg2d(k) = scp(k1) + 0,01
373900010621     c                   endif
374000080901      *
374100080901     c                   if        *in91 and *in37 and  *in38
374200080901      * se ha 3 decimale
374300080901     C                   eval      sg3d(k) = scp(k1) + 0,001
374400080901     c                   endif
374500010622      *
374600010622     c                   else
374700010622    4c                   if        k =  1
374800010622      * se non ha decimali
374900010622     c                   if        not *in91
375000010622     C                   eval      sgld(k) =  1
375100010622     c                   endif
375200010622      *
375300080901     c                   if        *in91 and not *in37 and not *in38
375400010622      * se ha 1 decimale
375500010622     C                   eval      sg1d(k) =  0,1
375600010622     c                   endif
375700010622      *
375800080901     c                   if        *in91 and *in37 and not *in38
375900010622      * se ha + di 1 decimale
376000010622     C                   eval      sg2d(k) =  0,01
376100010622     c                   endif
376200080901      *
376300080901     c                   if        *in91 and *in37 and *in38
376400080901      * se ha 3 decimali
376500080901     C                   eval      sg3d(k) =  0,001
376600080901     c                   endif
376700010621      *
376800010622     c                   endif
376900010622      *
377000010621    4c                   endif
377100010621      *
377200010621      * costruisco la schiera dell'applicazione tariffa finita che va sotto gli
377300010621      * scaglioni da/a
377400010621      * tariffa e in mancanza di quest'ultima prendo dalla tabella TR
377500010621      *
377600010621      * muovo unità di misura della tariffa nella schiera
377700060922     c**!!!              movel     de3tpg        atf(k)
377800060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
377900060925     c                   move      tpttpg        wctr1
378000060922     c                   exsr      sr_atfk
378100140522      * faccio i vari ragionamenti in base al valore della tariffa finita
378200140522      *
378300140522      * se scaglioni senza decimali
378400140522     c                   if        not *in91
378500140522
378600140522     c                   z-add     ctrata        wata
378700140522      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
378800140522      * scrivo "A SPED."
378900140522     c                   if        (wata > 0 and sgl(k) <= wata)
379000140522      * richiamo la routine (dove ho tutto fisso e in un unico posto)
379100140522     c                   clear                   wctr1
379200140522     c                   exsr      sr_atfk
379300140522     c                   endif
379400140522      *
379500140522     c                   else
379600140522      * scaglioni con decimali
379700140522     c                   z-add     ctrata        wata1
379800140522     c                   z-add     ctrata        wata2
379900140522      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
380000140522      * scrivo "A SPED."  1 decimale
380100140522     c                   if        (wata1 > 0 and sgl(k)<= wata1 and not *in37)
380200140522      * richiamo la routine (dove ho tutto fisso e in un unico posto)
380300140522     c                   clear                   wctr1
380400140522     c                   exsr      sr_atfk
380500140522     c                   endif
380600140522      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
380700140522      * scrivo "A SPED."  2 decimali
380800140522     c                   if        (wata2 > 0 and sgl(k) <= wata2 and *in37)
380900140522      * richiamo la routine (dove ho tutto fisso e in un unico posto)
381000140522     c                   clear                   wctr1
381100140522     c                   exsr      sr_atfk
381200140522     c                   endif
381300140522      *                                                      *in91 on
381400140522     c                   endif
381500010621      *
381600010621     c                   move      '/'           sglb(k)
381700010621      * preparo lo scaglione AL
381800010621      *
381900010621      * no decimali
382000010621    4c                   if        not *in91
382100010621     c                   z-add     scp(k)        sgla(k)
382200010621    4c                   endif
382300010621      * 1 decimale
382400080901    4c                   if        *in91 and not *in37  and not *in38
382500010621     c                   z-add     scp(k)        sg1a(k)
382600010621    4c                   endif
382700010621      * 2 decimali
382800080901    4c                   if        *in91 and *in37 and not *in38
382900010621     c                   z-add     scp(k)        sg2a(k)
383000010621    4c                   endif
383100080901      * 3 decimali
383200080901    4c                   if        *in91 and *in37 and  *in38
383300080901     c                   z-add     scp(k)        sg3a(k)
383400080901    4c                   endif
383500010621      *
383600010621   x3c                   else
383700010621      * scaglione 99999 è sempre maggiore della tariffa finita
383800060922     c**!!!              eval      atf(k) = de3tpg
383900060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
384000060925     c                   move      tpttpg        wctr1
384100060922     c                   exsr      sr_atfk
384200010621      *
384300010621    3c                   endif
384400010621      *
384500010621     c                   add       1             k
384600010621     c                   add       1             k1
384700010621      *
384800010621      * se maggiore di 18 esco
384900010621    3c                   if        k > 18
385000010621     c                   leave
385100010621    3c                   end
385200010621      *
385300010621    2c                   enddo
385400010621      *
385500010621      * se c'è solo uno scaglione non scrivo niente sotto
385600021220      * solo se lo scaglione è 99999
385700010621      *
385800021220     c                   if        k1 <= 1 and scp(k1) >= 99999
385900010621     c                   clear                   atf(1)
386000010621     c                   endif
386100010621      *
386200010621     c                   clear                   savsgl
386300010618      *
386400130905     C                   endsr
386500010618     C*
386600010619     C*------------------------------------------------------------------------
386700010619     C*  DECSCA DECODIFICO SCAGLIONI TARIFFE PARTICOLARI
386800010619     C*-------------------------------------------------------------------------
386900010619     C     DECSCA        BEGSR
387000010619      * imposto i campi di rottura salvati
387100010619     C                   MOVEL     WRKREG        ROTREG
387200010619     C                   MOVE      WRKPRO        ROTPRO
387300010619     C                   MOVE      WRKCTS        ROTCTS
387400010619     C                   MOVE      WRKCTS        TPDCTS
387500010619     C                   MOVE      WRKREG        COM04
387600010619     C                   MOVEL     COM04         TPDORP
387700010619     C                   MOVE      WRKPRO        TPDORP
387800010619      * pulisco le schiere
387900010619     C                   EXSR      REGDET
388000010619     C                   EXSR      PRODET
388100010625      *
388200010628      *
388300141016     .*//.......................................................................
388400010625      *
388500010622
388600010622      * stampo la dicitura della tariffa
388700020109      * se tariffa che viene stampata in modo standard recupero la decodifica
388800020109      * dall'estensione della tabella 1P la tabella SP
388900020109      *
389000020109     c                   if        tst(y) = ' '
389100020109     c                   movel(p)  edtc(y)       comstd
389200141016     .*//.......................................................................
389300020109     c                   endif
389400010622      * valorizzo i campi del prtf scaglioni
389500141016     .*//.......................................................................
389600010622     c                   eval      ds3um = de3tpg
389700010622      * stampo la testata
389800010622     c                   EXSR      SR_STATESDET
389900010622      *
390000010619     C                   ENDSR
390100010619     O*---------------------------------------------------------------*
390200010619     O* LEGGO DETTAGLIO DI TPD
390300010619     O*---------------------------------------------------------------*
390400010619     C     LEGDET        BEGSR
390500010619      *
390600141017     C  n98KTPT          SETLL     TITPD000
390700141017     c   98ktpt          setll     tiopd000
390800010619      *
390900010620    1C                   DO        *HIVAL
391000141017     C  n98KTPT          READE     TITPD000
391100141017     c   98ktpt          reade     tiopd000
391200010619      *
391300061122     C                   IF        %EOF
391400010619     C                   LEAVE
391500010619     C                   ENDIF
391600010619      *
391700010619     C                   IF        TPDATB = 'A'
391800010619     C                   ITER
391900010619     C                   ENDIF
392000010619      *
392100010619      * SE SCAGLIONE 9999 VERIFICO SE E' L'ULTIMO
392200010621     C                   Z-ADD     TPDSGL        NUM4
392300010620    2C     NUM4          IFEQ      9999
392400010621     C     TPDSGL        ANDNE     *ALL'9'
392500010619      *
392600010621     C                   MOVEL     TPDORP        SAVORP
392700010621     C                   MOVEL     TPDSGL        SAVSGL
392800010619      *
392900010619     C                   SETOFF                                       35
393000010619      * verifico se esiste un'altro scaglione per lo stesso ORP
393100010620    3C                   DO        *HIVAL
393200141017     C  n98KTPD          READE     TITPD000                               35
393300141017     c   98ktpd          reade     tiopd000                               35
393400010619      * se annullato leggo il successivo
393500010619     C                   IF        NOT *IN35 AND TPDATB = 'A'
393600010619     C                   ITER
393700010619     C                   ENDIF
393800010619      * se non trovato o non trovato riaggancio il record precedente
393900141017     C  n98KTPD2         CHAIN     TITPD000
394000141017     c   98ktpd2         chain     tiopd000
394100010619      *
394200010619     C                   LEAVE
394300010619      *
394400010620    3C                   ENDDO
394500010619     C*
394600010619     C* NON C'E' ALTRO SCAGLIONE
394700010620    2C                   ENDIF
394800010619     C*
394900010619     C                   Z-ADD     1             K2
395000080916     C                   Z-ADD     TPDSGL        wsgl83
395100010619      * cerco lo scaglione nella schiera
395200080916     C     wsgl83        LOOKUP    SCP(1)                                 35
395300010619      * se non trovato cerco lo scaglione nella schiera stampa
395400080916     C   35wsgl83        LOOKUP    SSLP(K2)                               90
395500010619      * se non trovato nella schiera di sola stampa ma trovato nella schiera SCP
395600010619      * torno a leggere
395700010619     c                   if        *in35 and not *in90
395800010619     c                   iter
395900010619     c                   endif
396000010619      *
396100010619      * se non trovato nella schiera scp ERRORE  scaglione non previsto
396200010620    2C                   IF        not *in35
396300010619     C                   MOVEL     MSG(14)       ERRMSG
396400010619     C                   EXSR      STAERR
396500050914      * Per errore esco dal pgm
396600050914     c                   Goto      FINE
396700010619      *
396800010620   X2C                   ELSE
396900010619     c                   eval      compro = %subst(tpdorp:5:1)
397000010619     c                   eval      comreg = %subst(tpdorp:3:2)
397100010619      * CAMBIATA PROVINCIA
397200010622    3C                   IF        compro <>  rotpro or comreg <> rotreg
397300010619     C                   EXSR      PROTPT
397400010619     C                   EXSR      PRODET
397500010619     C   99              GOTO      FINE
397600010619     C                   MOVE      COMPRO        ROTPRO
397700010620    3C                   END
397800010619      * CAMBIATA REGIONE
397900010620    3C                   IF        COMREG <> ROTREG
398000010619     C                   EXSR      REGTPT
398100010619      *
398200010619     C                   EXSR      REGDET
398300010619     C   99              GOTO      FINE
398400010619     C                   MOVEL     COMREG        ROTREG
398500010620    3C                   END
398600010619      *
398700010619     C                   Z-ADD     TPDITR        TAP(K2)
398800010619     C                   Z-ADD     TPDMIN        TPN(K2)
398900010619     C                   Z-ADD     TPDMAX        TPX(K2)
399000010619     C                   MOVEL     TPDAIN        SAVAIP
399100010619    2C                   END
399200010619      *
399300010619    1C                   ENDDO
399400010620      *
399500010619     C                   EXSR      PROTPT
399600010619     C                   EXSR      REGTPT
399700010621      *
399800010619     C                   ENDSR
399900010620     O*---------------------------------------------------------------*
400000010620     O* PROTPT CAMBIO PROVINCIA
400100010620     O*---------------------------------------------------------------*
400200010620     C     PROTPT        BEGSR
400300010620     C                   Z-ADD     1             C
400400010621     C     *BLANKS       LOOKUP    BIG(C)                                 35
400500010620     C     *IN35         IFEQ      '0'
400600010620     C                   MOVE      MSG(10)       ERRMSG
400700010620     C                   EXSR      STAERR
400800050914      * Per errore esco dal pgm
400900050914     c                   Goto      FINE
401000010620     C                   ELSE
401100010621      *
401200010621     c                   do        6             b
401300010621     c                   move      tap(b)        com(b)
401400010621     c                   end
401500010621      *
401600010621     c                   movea     com           big(c)
401700010621      *
401800010621     c                   do        6             b
401900010621     c                   move      tpn(b)        com(b)
402000010621     c                   end
402100010621      *
402200010621     c                   movea     com           bmn(c)
402300010621      *
402400010621     c                   do        6             b
402500010621     c                   move      tpx(b)        com(b)
402600010621     c                   end
402700010621      *
402800010621     c                   movea     com           bmx(c)
402900010621     c
403000010620     C                   MOVE      CODSIG        BSI(C)
403100010620     C                   MOVE      SAVAIP        AIP(C)
403200010620     C                   MOVE      CODORP        BRG(C)
403300050204     c                   move      desprv        dprv(c)
403400050303     c                   Eval      nodes(c) = nostdes
403500010620     C                   END
403600010620     C                   MOVEL     ' '           SAVAIP
403700010620     C                   ENDSR
403800010620      *-------------------------------------------------------------------------
403900010620      * REGTPT CAMBIO REGIONE
404000010620      *-------------------------------------------------------------------------
404100010620     C     REGTPT        BEGSR
404200010620     C                   Z-ADD     0             NUM4
404300010620     C                   Z-ADD     1             K
404400110216
404500110216     c                   clear                   savk
404600110216
404700010620      * preparo le schiere che devono essere stampate
404800050616    1C                   DO        15            K
404900010620    2C     BRG(K)        IFNE      '  '
405000010622    3C     BRG(K)        IFEQ      '9'
405100110216     c                   eval      savk = k
405200010620      *
405300010622     C     K             IFEQ      1
405400010621     C                   MOVEL     *all'.'       PDE(K)
405500010620     C                   ELSE
405600110216     C                   EVAL      PDE(k) = '.............................'
405700010620     C                   END
405800010620     C                   ADD       1             NUM4
405900010620     C*
406000010622    3C                   ELSE
406100010620     C*
406200010620     C                   MOVEA     *BLANKS       WCT
406300010620     C                   Z-ADD     1             C
406400010620     C                   MOVE      BSI(K)        WCT(C)
406500050204     c                   Eval      wragprv = *Off
406600010621      *
406700010621      * città
406800010621     C                   ADD       1             P
406900010621     C                   MOVEA     BIG(K)        COMO42
407000010621     C                   MOVEA     COMO42        WIN(P)
407100010621      * minimo
407200010621     C                   MOVEA     BMN(K)        CMNO42
407300010621     C                   MOVEA     CMNO42        WMN(P)
407400010621      * massimo
407500010621     C                   MOVEA     BMX(K)        CMXO42
407600010621     C                   MOVEA     CMXO42        WMX(P)
407700010620     C                   MOVEL     AIP(K)        SAVAIP            1
407800010620      *
407900010620      * MEMORIZZO LE ALTRE PROVINCIE CHE HANNO LE STESSE TARIFFE
408000010620     C     K             ADD       1             H
408100050616    4C     H             DO        15            H
408200010620      *
408300010620     C     BRG(H)        IFNE      '9'
408400010621     C     como42        ANDEQ     big(H)
408500010621     C     cmno42        ANDEQ     bmn(H)
408600010621     C     cmxo42        ANDEQ     bmX(H)
408700010620     C     SAVAIP        ANDEQ     AIP(H)
408800010625      *
408900010625      * se isola controllo anche che non siano le province sondrio e como
409000010625     c                   IF        (*in06  and bsi(h)<>'CO' and bsi(h)<>'SO')
409100010625     c                             or not *in06
409200010620     C                   ADD       1             C
409300010620     C                   MOVEL     '/'           WCT(C)
409400010620     C                   MOVE      BSI(H)        WCT(C)
409500010621     C                   MOVEL     *BLANKS       big(H)
409600010621     C                   MOVEL     *BLANKS       bmn(H)
409700010621     C                   MOVEL     *BLANKS       bmx(H)
409800010620     C                   MOVEL     *BLANKS       BRG(H)
409900050204     c                   Eval      wragprv = *On
410000010625     c                   endif
410100010625      *
410200010620     C                   ENDIF
410300010620      *
410400010620   e4C                   ENDDO
410500010620     C*
410600010620     C* MEMORIZZO LA SCHIERA DELLE PROVINCIE CON STESSE TARIFFE
410700010620     C                   ADD       1             C
410800010621     C                   MOVEA     *all'.'       WCT(C)
410900010621     c                   movea     wct           como29
411000010621     C                   MOVEA     como29        PDE(K)
411100010625      * se sto stampando le isole recupero la decodifica delle isole
411200010625     c                   if        %subst(como29:2:2) = 'CO' and *in06
411300010625     c                   eval      PDE(k) = 'CAMPIONE D''ITALIA .........'
411400010625     C                   ENDIF
411500010625      *
411600010625     c                   if        %subst(como29:2:2) = 'SO' and *in06
411700010625     c                   eval      PDE(k) = 'LIVIGNO ....................'
411800010625     C                   ENDIF
411900050204
412000050204      * se tariffa estera e non ho raggruppato più province devo stampare
412100050204      * la descrizione della provincia e non il codice
412200050204     c                   If        wragprv = *Off and (*in02 or *in25)
412300050204     c                   Eval      como29 = *all'.'
412400050303     c                   If        nodes(k) <> 'N'
412500050204     c                   Eval      %subst(como29:1:(%len(%trim(dprv(k))))) =
412600050204     c                             %trim(dprv(k))
412700050303     c                   EndIf
412800050204     c                   Move      como29        pde(k)
412900050204     c                   EndIf
413000010620     C*
413100010622   e3C                   END
413200010620   e2C                   END
413300010620   e1C                   END
413400010620      ***
413500010620      *** STAMPA REGIONE
413600010620      ***
413700110216
413800110216     c                   clear                   giro              1 0
413900110216
414000110216      * stampo per prima 'ALTRI' cioè la regione
414100110216     c                   if        savk > 1
414200110216     c                   eval      k = savk
414300110216     c                   else
414400110216     c                   eval      k = 1
414500110216     c                   endif
414600110216
414700110216     c     rifai1        tag
414800010620      *
414900110216 1   c                   dow       brg(k) <> *blanks
415000110216     c                   if        k = savk  and giro > 0
415100110216     c                   leave
415200110216     c                   endif
415300010620      *
415400010621      *
415500010622     c                   movea     BIG(k)        com
415600010621     c                   z-add     1             b
415700110216 2   c                   do        6             b
415800010621     c                   move      com(b)        tap(b)
415900010620      * se tariffa a valore di fianco al valore metto la percentuale
416000110216 3   c                   if        *in09 and tap(b) > 0
416100010622     C                   MOVEL     '%'           PER(B)
416200110216 3   C                   END
416300010620      *
416400110216 2   C                   END
416500010620      * applicazione inoltro (forse non si usa)
416600110216 2   C     AIP(K)        IFEQ      'S'
416700010620     C                   MOVEL     'SI'          STAAIP            2
416800010620     C                   ELSE
416900010620     C                   MOVEL     'NO'          STAAIP
417000110216 2   C                   END
417100010620      * VERIFICO SE C'E' IL MINIMO E IL MASSIMO
417200010620      *
417300141017     ***C                   SETOFF                                       7586
417400010621      *
417500010622    2C                   IF        BMN(K) <> *BLANKS AND BMN(K) <> *ZEROS
417600141017     ***C                   SETON                                        75
417700010621      *
417800010622     C                   MOVEA     BMN(K)        COM
417900010621      *
418000010621     C                   DO        6             B
418100010621     C                   MOVE      COM(B)        TPN(B)
418200010621     C                   ENDDO
418300010621      *
418400110216    2C                   ENDIF
418500010621      **
418600010622    2C                   IF        BMX(K) <> *BLANKS AND BMX(K) <> *ZEROS
418700141017     ***C                   SETON                                        86
418800010621      **
418900010622     C                   MOVEA     BMX(K)        COM
419000010621      *
419100010621     C                   DO        6             B
419200010621     C                   MOVE      COM(B)        TPX(B)
419300010621     C                   ENDDO
419400010621      *
419500010621    2C                   ENDIF
419600010620      *
419700010620      * gestione stampa tariffa particolare
419800010620      *
419900010620     c                   EXSR      SR_STPPAR
420000010621      *
420100010621      *
420200110216     c                   eval      k = k+1
420300010620    1C                   ENDDO
420400110216
420500110216      * se ho stampato 'Altri' torno a loopare per stampare il resto
420600110216     c                   if        savk > 1 and giro = 0
420700110216     c                   eval      k = 1
420800110216     c                   eval      giro = 1
420900110216     c                   goto      rifai1
421000110216     c                   endif
421100010620      *
421200010621     C                   MOVEA     *blanks       big
421300010621     C                   MOVEA     *blanks       bmn
421400010621     C                   MOVEA     *blanks       bmx
421500010621     C                   MOVEA     *blanks       win
421600010621     C                   MOVEA     *blanks       wmn
421700010621     C                   MOVEA     *blanks       wmx
421800010620     C                   MOVEA     *BLANKS       BSI
421900010620     C                   MOVEA     *BLANKS       AIP
422000010620     C                   MOVEA     *BLANKS       PER
422100010620     C                   MOVEA     *ZEROS        TAP
422200010620     C                   MOVEA     *ZEROS        TPN
422300010620     C                   MOVEA     *ZEROS        TPX
422400010620     C                   CLEAR                   BRG
422500010620      *
422600010620     C                   ENDSR
422700010620      *---------------------------------------------------------------*
422800010620      * SR_STPPAR STAMPA TARIFFE PARTICOLARI
422900010620      *---------------------------------------------------------------*
423000010620     C     SR_STPPAR     BEGSR
423100010620      * valorizzo il dettaglio tariffario in stampa
423200010620      *
423300141016     .*//.......................................................................
423400010620      * verifico se sono tutti valorizzati
423500010622     c     tap(1)        comp      0                                  61
423600010622     c     tap(2)        comp      0                                  62
423700080925      * se si tratta della tariffa particolare fuel  se valorizzato il secondo
423800080925      * scaglione valorizzo anche il precedente ed anche la %
423900080925     c                   If        savftc = 'f ' and *in62
424000080925     c                   eval      *in61 = *on
424100080925     c                   eval      per(1)= '%'
424200080925     c                   endif
424300010622     c     tap(3)        comp      0                                  63
424400080925      * se si tratta della tariffa particolare fuel  se valorizzato il terzo
424500080925      * scaglione valorizzo anche i precedenti ed anche le %
424600080925     c                   If        savftc = 'f ' and *in63
424700080925     c                   eval      *in61 = *on
424800080925     c                   eval      *in62 = *on
424900080925     c                   eval      per(1)= '%'
425000080925     c                   eval      per(2)= '%'
425100080925     c                   endif
425200010622     c     tap(4)        comp      0                                  64
425300080925      * se si tratta della tariffa particolare fuel  se valorizzato il quarto
425400080925      * scaglione valorizzo anche i precedenti ed anche le %
425500080925     c                   If        savftc = 'f ' and *in64
425600080925     c                   eval      *in61 = *on
425700080925     c                   eval      *in62 = *on
425800080925     c                   eval      *in63 = *on
425900080925     c                   eval      per(1)= '%'
426000080925     c                   eval      per(2)= '%'
426100080925     c                   eval      per(3)= '%'
426200080925     c                   endif
426300010622     c     tap(5)        comp      0                                  65
426400080925      * se si tratta della tariffa particolare fuel  se valorizzato il quinto
426500080925      * scaglione valorizzo anche i precedenti ed anche le %
426600080925     c                   If        savftc = 'f ' and *in65
426700080925     c                   eval      *in61 = *on
426800080925     c                   eval      *in62 = *on
426900080925     c                   eval      *in63 = *on
427000080925     c                   eval      *in64 = *on
427100080925     c                   eval      per(1)= '%'
427200080925     c                   eval      per(2)= '%'
427300080925     c                   eval      per(3)= '%'
427400080925     c                   eval      per(4)= '%'
427500080925     c                   endif
427600010622     c     tap(6)        comp      0                                  66
427700080925      * se si tratta della tariffa particolare fuel  se valorizzato il sesto
427800080925      * scaglione valorizzo anche i precedenti ed anche le %
427900080925     c                   If        savftc = 'f ' and *in66
428000080925     c                   eval      *in61 = *on
428100080925     c                   eval      *in62 = *on
428200080925     c                   eval      *in63 = *on
428300080925     c                   eval      *in64 = *on
428400080925     c                   eval      *in65 = *on
428500080925     c                   eval      per(1)= '%'
428600080925     c                   eval      per(2)= '%'
428700080925     c                   eval      per(3)= '%'
428800080925     c                   eval      per(4)= '%'
428900080925     c                   eval      per(5)= '%'
429000080925     c                   endif
429100010620      *
429200141016     .*//.......................................................................
429300010621      *
429400010621     C                   ENDSR
429500010622      *-------------------------------------------------------------------------
429600010622      * FINDET STAMPA FINALE SOTTO OGNI SINGOLA TARIFFA PARTICOLARE
429700010622      *-------------------------------------------------------------------------
429800010622     C     FINDET        BEGSR
429900010919      *
430000010919      * stampo eventuale estensione scaglione unico
430100010919      *
430200010919     c                   exsr      sr_estes
430300010919      *
430400141016     .*//.......................................................................
430500010622      * verifico se tariffa a q.le per stampare il minimo tassabile
430600010622     c                   eval      *in08 = (tpttpg = '0')
430700010622      * per 08 controllo il minimo tassabile
430800141016     .*//.......................................................................
430900010622      * verifico rapporto peso / volume
431000141016     .*//.......................................................................
431100010622      * verifico gli importi degli arrotondamenti
431200010622      *
431300010622    1c                   if        tptarl > 0
431400010622      * verifico se esistono dei valori con i decimali
431500010622     C                   move      tptarf        w0030
431600010622     C                   eval      *in89 =(w0030 > 0)
431700010622      *
431800010622    2c                   if        not *in89
431900010622     C                   move      tptarl        w0030
432000010622     C                   eval      *in89 =(w0030 > 0)
432100010622    2c                   endif
432200010622      *
432300010622    2c                   if        not *in89
432400010622     C                   move      tptaro        w0030
432500010622     C                   eval      *in89 =(w0030 > 0)
432600010622    2c                   endif
432700010622      *
432800010622      * se 89 spento non esistono importi con decimali sposto in camp di comodo
432900010622    2c                   if        not *in89
433000010622     C                   Z-ADD     TptARO        WARO2
433100010622     C                   Z-ADD     TptARF        WARF2
433200010622     C                   Z-ADD     TptARL        WARL2
433300010622    2c                   endif
433400010622      *
433500010622      * se arrotondamenti di 1 in 1 non stampo
433600010622      * se arrotondamento fino a diverso da arrotondamento oltre a
433700010622      *
433800141017    2***c                   if        tptarf <> tptaro
433900010622      *
434000141016     .*//.......................................................................
434100141017     ***c                   seton                                        18
434200010622      *
434300141016     .*//.......................................................................
434400010628      *
434500141017     ***C                   ELSE
434600010625      *
434700141016     .*//.......................................................................
434800010625
434900141017     ***c                   seton                                        18
435000010625      *
435100141017    2***C                   endif
435200010622      *
435300010622    1c                   endif
435400010917      *
435500141016     .*//.......................................................................
435600010622      *
435700010622     C                   ENDSR
435800010919      *
435900010919      *---------------------------------------------------------------*
436000010919      * SR_ESTES Gestione stampa estensione scaglione unico
436100010919      *---------------------------------------------------------------*
436200010919     C     SR_ESTES      BEGSR
436300010919      *
436400010919      * se il campo del TARPAREST è diverso da blanks vuol dire che devo
436500010919      * stampare l'estensione della tariffa particolare
436600010919      *
436700010919     c                   if        W_TARPAREST <> *blanks
436800141016     .*//.......................................................................
436900010919      *
437000010919     c                   clear                   W_TARPAREST
437100010919      *
437200010919     c                   endif
437300010627      *
437400010919     C                   ENDSR
437500050526
437600050526      *---------------------------------------------------------------*
437700050526      * SR_COMUNICA controllo e stampa dei servizi on line + e-mail
437800050526      *---------------------------------------------------------------*
437900050526     c     SR_COMUNICA   BegSr
438000050526
438100050526      * Controllo se devo stampare i SERVIZI ON LINE
438200050526     c                   ExSr      Sr_Psw
438300050526      * Controllo se devo stampare la richiesta di almeno 1 mail
438400050526     c                   ExSr      Sr_Ema
438500050526
438600141016     .*//.......................................................................
438700041229
438800050526     c                   EndSr
438900041229      *---------------------------------------------------------------*
439000041229      * SR_PSW stampa servizi ON LINE se offerta
439100041229      *---------------------------------------------------------------*
439200041229     c     SR_PSW        BegSr
439300041229
439400041229     c                   Eval      wpsw = *Off
439500041229
439600041229      * Se cliente nuovo
439700041229      * --> stampo sempre
439800041230if  1c                   If        VisCnw = 'S'
439900041229     c                   Eval      wpsw = *On
440000041229      * Se cliente non nuovo
440100041229      * --> stampo se non abilitato o se non è figlio di papà abilitato
440200041230   x1c                   Else
440300041230     c                   Move      *all'0'       vssksu
440400041230     c                   Move      visksc        vssksu
440500041230     c                   Eval      vssisv = 'TT'
440600141017     c     kTivss        Setll     Tivss000
440700041230if  2c                   If        not %Equal(Tivss02l)
440800041230     c                   Clear                   Tibs10ds
440900041230     c                   Eval      d10tle = 'WW'
441000041230     c                   Eval      d10paf = 'P'
441100041230     c                   Eval      d10cod = visksc
441200041230     c                   Eval      d10drf = *date
441300041230     c                   Call      'TIBS10R'
441400041230     c                   Parm                    Tibs10ds
441500041230if  3c                   If        d10err = *Blanks and d10cop > *Zeros
441600041230     c                   Move      *all'0'       vssksu
441700041230     c                   Move      d10cop        vssksu
441800041230     c                   Eval      vssisv = 'TT'
441900141017     c     kTivss        Setll     Tivss000
442000041230     c                   If        not %Equal(Tivss02l)
442100041230     c                   Eval      wpsw = *On
442200041230     c                   EndIf
442300041230e   3c                   EndIf
442400050112if  3c                   If        d10err <> *Blanks
442500050112     c                   Eval      wpsw = *On
442600050112e   3c                   EndIf
442700041230e   2c                   EndIf
442800041230e   1c                   EndIf
442900041229
443000041229     c                   EndSr
443100050525
443200050525      *---------------------------------------------------------------*
443300050526      * SR_EMA stampa delle e-mail da richiedere se offerta
443400050525      *---------------------------------------------------------------*
443500050526     c     Sr_Ema        BegSr
443600050526
443700050526     c                   Eval      wema85 = *Off
443800050526     c                   Eval      wema88 = *Off
443900050526     c                   Eval      wema87 = *Off
444000051206     c                   Eval      wema84 = *Off
444100090612     c                   Eval      wema89 = *Off
444200091112      * visita
444300050527     c                   eval      notapl = 'V'
444400141017     c                   movel     DUTkci        notke1
444500050527     c                   move      parsks        notke1
444600091112      * trattativa
444700091112     c                   if        $trattativa
444800091112     c                   eval      notapl = 'T'
444900091112     c                   endif
445000050527     c                   Clear                   notke2
445100050525
445200050526      * e-mail bonifico bancario nota 85
445300050526     c                   If        §4uabi = 'S'
445400050527      * nota della visita
445500050526     c                   movel     '85'          nottnt
445600141017     c     keyntc        Chain     tfntc
445700050527     c                   If        %Found(tfntc01l)
445800050527     c                   Goto      nota88
445900050527     c                   EndIf
446000050527      * nota del cliente se non è un cliente nuovo
446100050527     c                   If        visksc > *Zeros
446200050527     c                   eval      notapl = 'C'
446300050527     c                   move      visksc        notke1
446400141017     c     keyntc        Chain     tfntc
446500050527     c                   If        %Found(tfntc01l)
446600050527     c                   Goto      nota88
446700050527     c                   EndIf
446800050527     c                   EndIf
446900050527      * nota non trovata
447000050526     c                   Eval      wema85 = *On
447100050526     c                   EndIf
447200050527
447300050527     c     nota88        Tag
447400091112      * visita
447500050527     c                   eval      notapl = 'V'
447600050527     c                   move      parsks        notke1
447700091112      * trattativa
447800091112     c                   if        $trattativa
447900091112     c                   eval      notapl = 'T'
448000091112     c                   endif
448100050527
448200050527      * e-mail giacenze nota 88
448300050527     c                   movel     '88'          nottnt
448400050527      * nota della visita
448500141017     c     keyntc        Chain     tfntc
448600050527     c                   If        %Found(tfntc01l)
448700050527     c                   Goto      nota87
448800050527     c                   EndIf
448900050527      * nota del cliente se non è un cliente nuovo
449000050527     c                   If        visksc > *Zeros
449100050527     c                   eval      notapl = 'C'
449200050527     c                   move      visksc        notke1
449300141017     c     keyntc        Chain     tfntc
449400050527     c                   If        %Found(tfntc01l)
449500050527     c                   Goto      nota87
449600050527     c                   EndIf
449700050527     c                   EndIf
449800050527      * nota non trovata
449900050527     c                   Eval      wema88 = *On
450000050527
450100050527     c     nota87        Tag
450200091112      * visita
450300050527     c                   eval      notapl = 'V'
450400050527     c                   move      parsks        notke1
450500091112      * trattativa
450600091112     c                   if        $trattativa
450700091112     c                   eval      notapl = 'T'
450800091112     c                   endif
450900050527
451000050526      * e-mail danni nota 87
451100050527     c                   movel     '87'          nottnt
451200050527      * nota della visita
451300141017     c     keyntc        Chain     tfntc
451400050527     c                   If        %found(tfntc01l)
451500090612     c                   Goto      nota84
451600050527     c                   EndIf
451700050527      * nota del cliente se non è un cliente nuovo
451800050527     c                   If        visksc > *Zeros
451900050527     c                   eval      notapl = 'C'
452000050527     c                   move      visksc        notke1
452100141017     c     keyntc        Chain     tfntc
452200050527     c                   If        %Found(tfntc01l)
452300051206     c                   Goto      nota84
452400050527     c                   EndIf
452500050527     c                   EndIf
452600050527      * nota non trovata
452700050527     c                   Eval      wema87 = *On
452800051206
452900051206     c     nota84        Tag
453000091112      * visita
453100051206     c                   eval      notapl = 'V'
453200051206     c                   move      parsks        notke1
453300091112      * trattativa
453400091112     c                   if        $trattativa
453500091112     c                   eval      notapl = 'T'
453600091112     c                   endif
453700091112
453800051206      * e-mail invio fattura nota 84
453900051206     c                   movel     '84'          nottnt
454000051206      * nota della visita
454100141017     c     keyntc        Chain     tfntc
454200051206     c                   If        %found(tfntc01l)
454300090612     c                   Goto      nota89
454400051206     c                   EndIf
454500051206      * nota del cliente se non è un cliente nuovo
454600051206     c                   If        visksc > *Zeros
454700051206     c                   eval      notapl = 'C'
454800051206     c                   move      visksc        notke1
454900141017     c     keyntc        Chain     tfntc
455000051206     c                   If        %Found(tfntc01l)
455100090612     c                   Goto      nota89
455200051206     c                   EndIf
455300051206     c                   EndIf
455400051206      * nota non trovata
455500060203     c                   Eval      wema84 = *On
455600090612
455700090612     c     nota89        Tag
455800091112      * visita
455900090612     c                   eval      notapl = 'V'
456000090612     c                   move      parsks        notke1
456100091112      * trattativa
456200091112     c                   if        $trattativa
456300091112     c                   eval      notapl = 'T'
456400091112     c                   endif
456500090612
456600090612      * e-mail sollecito pagamenti 89
456700090612     c                   movel     '89'          nottnt
456800090612      * nota della visita
456900141017     c     keyntc        Chain     tfntc
457000090612     c                   If        %found(tfntc01l)
457100090612     c                   Leavesr
457200090612     c                   EndIf
457300090612      * nota del cliente se non è un cliente nuovo
457400090612     c                   If        visksc > *Zeros
457500090612     c                   eval      notapl = 'C'
457600090612     c                   move      visksc        notke1
457700141017     c     keyntc        Chain     tfntc
457800090612     c                   If        %Found(tfntc01l)
457900090612     c                   Leavesr
458000090612     c                   EndIf
458100090612     c                   EndIf
458200090612      * nota non trovata
458300090612     c                   Eval      wema89 = *On
458400050525
458500050525     c                   EndSr
458600041229
458700141016     .*//.......................................................................
458800050526
458900010618      *---------------------------------------------------------------*
459000010618      * STAMPA ERRORE
459100010618      *---------------------------------------------------------------*
459200141016     C     STAERR        BEGSR
459300141016      *
459400141016     c                   eval      oTA48err = *on
459500141016     c                   eval      oTA48msg = ErrMSG
459600141016      *
459700141016      * se offerta valorizzo con il numero della visita
459800141016    2c                   IF        *in98
459900141016     c                   eval      oTA48ksc = VISksc
460000141016     c                   eval      oTA48nrv = TAMksc
460100141016     c                   if        $Trattativa
460200141016     c                   eval      oTA48ntc = TAMksc
460300141016     c                   endif
460400141016      * tariffa
460500141016   x2c                   ELSE
460600141016     c                   eval      oTA48ksc = TAMksc
460700141016    2c                   ENDIF
460800141016      *
460900141016     c                   if        $Trattativa
461000141016     c                   eval      oTA48ctr = TAMctr
461100141016     c                   eval      oTA48ptr = TAMprg
461200141016     c                   endif
461300141016      *
461400141016     c                   if        not $Trattativa
461500141016     c                   eval      oTA48ctr = TAMctr
461600141016     c                   endif
461700141016      *
461800010615     C                   ENDSR
461900060922
462000060922      *---------------------------------------------------------------*
462100060922      * Imposto ATF lungo 7 nelle varie lingue
462200060922      *---------------------------------------------------------------*
462300060922     c     sr_atfk       begsr
462400060922
462500060922      * tariffa peso q.li 0
462600060925     c                   if        wctr1 = '0'
462700060922     c                   eval      atf(k) = 'A Q.LE'
462800141017     .*//.......................................................................
462900060922     c                   endif
463000060922
463100060922      * tariffa colli 1
463200060925     c                   if        wctr1 = '1'
463300060922     c                   eval      atf(k) = 'A COLLO'
463400141017     .*//.......................................................................
463500060922     c                   endif
463600060922
463700060922      * tariffa spedizione 2
463800060925     c                   if        wctr1 = '2'
463900060922     c                   eval      atf(k) = 'A SPED.'
464000141017     .*//.......................................................................
464100060922     c                   endif
464200060922
464300060922      * tariffa peso kg 3
464400060925     c                   if        wctr1 = '3'
464500060922     c                   eval      atf(k) = 'A KG.'
464600141017     .*//.......................................................................
464700060922     c                   endif
464800060922
464900060922      * tariffa a valore 4
465000060925     c                   if        wctr1 = '4'
465100060922     c                   eval      atf(k) = 'A VAL.'
465200141017     .*//.......................................................................
465300060922     c                   endif
465400060922
465500060922      * tariffa a quantità 5
465600060925     c                   if        wctr1 = '5'
465700060922     c                   eval      atf(k) = 'A Q.TA'
465800141017     .*//.......................................................................
465900060922     c                   endif
466000060922
466100060922      * tariffa valore specifico V (usato nelle tariffe particolari)
466200060925     c                   if        wctr1 = 'V'
466300060922     c                   eval      atf(k) = 'VAL.SPE'
466400141017     .*//.......................................................................
466500060922     c                   endif
466600060922
466700060922      * questo è un chiodo per impostare 'A SPED.' quando lo richiede
466800060922      * il programma
466900060925     c                   if        wctr1 = ' '
467000060922     c                   eval      atf(k) = 'A SPED.'
467100141017     .*//.......................................................................
467200060922     c                   endif
467300060922
467400060922     c                   endsr
467500060922
467600141016     .*//.......................................................................
467700060922
467800010606     C*-------------------------------------------------------------------------
467900010606     C*  SR_CARTAB  CARICAMENTO INIZIALE TABELLE
468000010606     C*-------------------------------------------------------------------------
468100010606     C     SR_CARTAB     BEGSR
468200010606     C*  carico la schiera di tutti i codici tassazione
468300010606     C                   clear                   K
468400010606     C                   clear                   K1
468500010606     C                   eval      cod = 'CT'
468600141017     c     tabk2l        setll     tabel
468700010531     C                   DO        *hival
468800141017     c     tabk2l        READE     Tabel
468900010531      *
469000010531     C                   If        %eof(tabel00f)
469100010531     C                   leave
469200010531     C                   endif
469300010531      *
469400010531     C                   IF        tblkey <> *blanks
469500010531     C                   add       1             K
469600010531     C                   movel     Tblkey        CCT(K)
469700010531     C                   movel     Tbluni        DSCT
469800010531     C                   move      §CTprv        SCT(K)
469900010531     C                   MOVE      §CTrap        RCT(K)
470000010531     C                   MOVE      §CTnar        NAR(K)
470100010531     C                   MOVE      §CTcor        DS6PR2
470200050204     c                   Move      §ctdes        prvdes(k)
470300050303     c                   Eval      noctdes(k) = §ctsta
470400010531      * se è una regione carico la schiera delle regioni
470500010531     C                   IF        ds6pr2 = '9'
470600010531     C                   add       1             K1
470700010531     C                   movel     Tblkey        CRE(K1)
470800010531     C                   move      §CTdes        DRE(K1)
470900010531     C                   endif
471000010531      *
471100010531     C                   endif
471200010531      *
471300010531     C                   ENDDO
471400010531      *
471500010531      *  carico la schiera delle nazioni con quella ITALIA
471600010531     C                   z-add     1             K
471700010531     C                   eval      cod = '15'
471800141017     C     TABK2l        SETLL     TABEL
471900010531     c                   DO        *HIVAL
472000141017     C     TABK2l        READE     TABEL
472100010531      *
472200010621     C                   IF          %eof(tabel00f)
472300010531     C                   leave
472400010531     c                   ENDIF
472500010531      *
472600010531      * se non è annullato
472700010531     C                   IF        Tblflg = ' '
472800010531      *
472900010531     C                   Movel     tbluni        DS15
473000010531      * carico solo la nazione ITALIA
473100010531     C                   IF        §15ita = 'I'
473200010531     C                   Movel     Tblkey        NAZ(K)
473300010531     C                   add       1             K
473400010531     C                   endif
473500010531      *
473600010531     C                   endif
473700010531      *
473800010531     C                   ENDDO
473900010807      *
474000010807      * aggancio la tabella delle variabili giacenza
474100010807     C                   eval      cod = '2G'
474200010807     C                   clear                   KEY
474300010807     C                   movel     '1'           KEY
474400141017     C     TABKEY        CHAIN     TABEL
474500010807     c                   if        %found(tabel00f)
474600010807     C                   movel     TBLUNI        DS2G
474700010807     c                   else
474800010807     C                   clear                   DS2G
474900010807     c                   endif
475000101015
475100101015      /free
475200101015       //?Carico scatti ISTAT
475300101015         SISsca = 0;
475400101015         clear DIA;
475500141017         setll (SISsca) TISIS000;
475600101015
475700101015         DOW  not $EoF;
475800101015
475900141017           read TISIS000;
476000101015
476100101015           IF  %Eof(TISIS01L);
476200101015             $EoF = *on;
476300101015             leave;
476400101015           ENDIF;
476500101015
476600101015           DIA(SISsca) = SISdata;
476700101015
476800101015         ENDDO;
476900101015
477000101015      /end-free
477100031119
477200021022      *
477300010531     C                   ENDSR
477400080609
477500080609      *------------------------------------------------------------------------*
477600080609      * Carico filiali abilitate al lasciato avviso
477700080609      *------------------------------------------------------------------------*
477800080609     c     carlav        begsr
477900080609
478000080609     c                   clear                   sklav
478100080609     c                   clear                   xxx
478200080609
478300080609     c/exec sql
478400080609     c+ declare lav cursor for select tntbe01l.*
478500080609     c+ from tntbe01l
478600080609     c+ where tbecod='LAV' and tbeatb = ''
478700080609     c/end-exec
478800080609
478900080609     c/exec sql
479000080609     c+ open lav
479100080609     c/end-exec
479200080609
479300080609     c                   do        *hival
479400080609
479500080609     c/exec sql
479600080609     c+ fetch next from lav into :tntbeds
479700080609     c/end-exec
479800080609
479900080609     c                   select
480000080609     c                   when      sqlcod = 100
480100080609     c                   leave
480200080609     c                   when      sqlcod < 0
480300080609     c                   other
480400080609     c                   eval      dlav = tbeuni
480500080609      * carico la schiera della filiali abilitate
480600080616     c                   if        d§lavtar > *date
480700080609     c                   iter
480800080609     c                   endif
480900080609     c                   eval      xxx = xxx + 1
481000080609     c                   eval      sklav(xxx) = %dec(tbeke1:3:0)
481100080609
481200080609     c                   endsl
481300080609
481400080609     c                   enddo
481500080609
481600080609     c/exec sql
481700080609     c+ close lav
481800080609     c/end-exec
481900080609
482000080609     c                   endsr
482100141016
482200010530     C*------------------------------------------------------------------------
482300010530     C* ROUTINE INIZIALE
482400010530     C*------------------------------------------------------------------------
482500010530      *
482600010530     C     *INZSR        BEGSR
482700010530      *
482800010530     C     *ENTRY        PLIST
482900141016     c                   parm                    TNTA48ds
483000141016      *
483100141016      * pulizia dei dati di Output
483200141016     c                   clear                   oTA48ksc
483300141016     c                   clear                   oTA48nrv
483400141016     c                   clear                   oTA48ntc
483500141016     c                   clear                   oTA48ctr
483600141016     c                   clear                   oTA48ptr
483700141016     c                   eval      oTA48err = *off
483800141016     c                   clear                   oTA48msg
483900010628      *
484000020624     C                   z-add     1             codut             1 0
484100020624
484200020624      * reperisco i dati utente
484300020624     c     *dtaara       define    §azute        AzuteDs
484400020624     c     *dtaara       define    §datiute      DDatiUte
484500020624
484600020624     c                   in(E)     *dtaara
484700020624     c                   IF        %Error or RSUT = *blanks
484800020624     c                   call      'TIBS34R'
484900020624     c                   parm                    Tibs34Ds
485000020624     c                   in        *dtaara
485100020624     c                   ENDIF
485200141016     .*//.......................................................................
485300060704
485400060704      * lingua per tabelle
485500060704     c                   clear                   kcodut            1 0
485600060704     c                   eval      kcodut = 1
485700141017     .*//.......................................................................
485800010530      *
485900010531      * carico le tabelle
486000010531     c                   exsr      SR_CARTAB
486100010531      *
486200010531      * TARIFFA O OFFERTA
486300010531      *
486400010531      *............ OFFERTA .....................
486500010531      *
486600141017     ***c                   if        i45ast = 'T'
486700141017     c                   if        iTA48cto = 'O'
486800010531      * accendo il 98
486900010531     c                   seton                                        98
487000091112      * se trattativa imposto flag
487100141017     c                   eval      $trattativa = (iTA48cto = 'O')
487200141017     c                   eval      *in85 = (iTA48cto = 'O')
487300010531      * valorizzo il campo del codice cliente tariffa con il numero visita
487400141017     c                   eval      parsks = iTA48ksc
487500010531      * apro i file visita/offerta
487600010531     c                   open      TFACO00F
487700010531     c                   open      TFIND00F
487800010531     c                   open      TFCLP00F
487900010531     c                   open      TNOFM01L
488000061122     c                   open      tiofd01L
488100010531     c                   open      TIOPT01L
488200010629     c                   open      TIOPD01L
488300010531     c                   open      TIOGC01L
488400141017     ***c                   if        i45ast = 'T'
488500141017     c                   if        iTA48cto = 'O'
488600091022     c                   open      Tivis05l
488700091022     c                   endif
488800041229
488900141017     *** *  i45ast = 'C'
489000141017      *  iTA48cto = 'T'
489100010531     c                   else
489200010531      *
489300010531      *............ TARIFFA .....................
489400061122      * apro i file visita/offerta
489500061122     c                   open      TNtam01L
489600061122     c                   open      Titad04L
489700061122     c                   open      Titpt01L
489800061122     c                   open      TItpD01L
489900061122     c                   open      TItGC01L
490000010531      *
490100010531      * valorizzo il campo del codice cliente tariffa
490200141017     c                   eval      parsks = iTA48ksc
490300010531      *
490400010531     c                   endif
490500010531      *
490600010531      *   DEFINIZIONI DELLE CHIAVI
490700010531      *
490800010531      * accesso per codice su TABEL00F
490900090608     c     TABKEY        klist
491000010531     c                   kfld                    codut
491100010531     c                   kfld                    cod
491200010531     c                   kfld                    key
491300060605
491400060605      * accesso per codice su TABEL00F in lingua
491500060605     c     tabkeyl       klist
491600060605     c                   kfld                    kcodut
491700060605     c                   kfld                    cod
491800060605     c                   kfld                    key
491900060606
492000060606      * accesso per codice su TABEL00F  parziale in lingua
492100060606     c     tabk2l        klist
492200060606     c                   kfld                    kcodut
492300060606     c                   kfld                    cod
492400060606
492500010531      * accesso TNTAM01L/TNOFM01L
492600010531     c     DS1KTM        klist
492700010531     c                   kfld                    ds1ksc
492800010531     c                   kfld                    ds1ctr
492900010531     c                   kfld                    ds1prg
493000010605      * accesso TITAD04L con linea partenza impostata
493100010605     c     DS1KTD        klist
493200010605     c                   kfld                    ds1ksc
493300010605     c                   kfld                    ds1ctr
493400010605     c                   kfld                    ds1prg
493500010605     c                   kfld                    comfil
493600010606      * accesso TITAD04L con chiave piena
493700010606     c     KEYTAD        klist
493800010606     c                   kfld                    tadksc
493900010606     c                   kfld                    tadctr
494000010606     c                   kfld                    tadprg
494100010606     c                   kfld                    tadlnp
494200010606     c                   kfld                    tadorp
494300010606     c                   kfld                    tadnaz
494400010606     c                   kfld                    tadcap
494500010618      ** accesso su TITPT e TITPD
494600010618     C     KTPT          KLIST
494700010618     C                   KFLD                    ds1ksc
494800010618     C                   KFLD                    ds1ctr
494900010618     C                   KFLD                    ds1prg
495000010618     C                   KFLD                    savftc
495100010621      ** accesso su  TITPD
495200010621     C     KTPD          KLIST
495300010621     C                   KFLD                    DS1KSC
495400010621     C                   KFLD                    DS1CTR
495500010621     C                   KFLD                    DS1PRG
495600010621     C                   KFLD                    SAVFTC
495700010621     C                   KFLD                    SAVORP
495800010621     C     KTPD2         KLIST
495900010621     C                   KFLD                    DS1KSC
496000010621     C                   KFLD                    DS1CTR
496100010621     C                   KFLD                    DS1PRG
496200010621     C                   KFLD                    SAVFTC
496300010621     C                   KFLD                    SAVORP
496400010621     C                   KFLD                    SAVSGL
496500010626     C     KTPDWRK       KLIST
496600010626     C                   KFLD                    DS1KSC
496700010626     C                   KFLD                    DS1CTR
496800010626     C                   KFLD                    DS1PRG
496900010626     C                   KFLD                    WRKFTC
497000010626     C                   KFLD                    WRKORP
497100010626     C                   KFLD                    WRKSGL
497200010601      ** accesso CNACO / TFACO
497300010601     C     DS7KAC        klist
497400010601     C                   kfld                    codut
497500141017     C                   kfld                    DUTkci
497600010601     C                   kfld                    ksc
497700010601      ** accesso TFNTC
497800010601     C     KEYNTC        klist
497900010601     c                   kfld                    notapl
498000010601     c                   kfld                    notke1
498100010601     c                   kfld                    notke2
498200010601     c                   kfld                    nottnt
498300010604      * accesso AZCPC03L
498400010621     c     kcpc          klist
498500010604     c                   kfld                    o96ver
498600010604     c                   kfld                    §ctnar
498700010604     c                   kfld                    comfil
498800041230      * accesso TIVSS02L
498900041230     c     kTivss        klist
499000041230     c                   kfld                    vssksu
499100041230     c                   kfld                    vssisv
499200010531      *
499300010531     c                   ENDSR
499400010531      *
499500010530**   MSG
499600140224ATTENZIONE!!! Prezzo base Fuel mai rilevato settimanalmente dal Ministero!!   1
499700010530                                                                              2
499800010530CLIENTE NON CODIFICATO O ANNULLATO                                            3
499900010530NESSUNA RIGA DI DETTAGLIO X QUESTA TARIFFA/OFFERTA                            4
500000050615PIU' DI 18 SCAGLIONI X CODICE DI TASSAZIONE                                   5
500100010530CODICE TASSAZIONE NON TROVATO !!!!!!!!!!!      CUMELA MO' ?                   6
500200010530CODICE REGIONE DEL CODICE DI TASSAZIONE NON ESISTE IN TABELLA                 7
500300010530UN CODICE DI TASSAZIONE DELLA TARIFFA HA UNO SCAGLIONE NON PREVISTO           8
500400050616PIU' DI 15 PROVINCIE X 1 REGIONE? TABELLA ERRATA O  T N T A D  NON ALLINEATO  9
500500050616PIU' DI 15 PROVINCIE X 1 REGIONE? TABELLA ERRATA O  T N T P D  NON ALLINEATO  10
500600050615                                                                              1
500700010530TROPPI PAESI DI ARRIVO/CODICI DI TASSAZIONE PER LA REGIONE                    2
500800140224                                                                              3
500900010530UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON PREVISTO   4
501000050615PIU' DI 18 SCAGLIONI X CODICE DI TASSAZIONE PER LE TARIFFE PARTICOLARI        5
501100050324UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON VALIDO     6
