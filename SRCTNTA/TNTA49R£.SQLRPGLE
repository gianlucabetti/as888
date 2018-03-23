000100041230     H DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000200010530     H*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**
000300010530     H*        S T A M P A       T A R I F F E / O F F E R T E           *
000400010530     H*                  P E R     C L I E N T E                         *
000500010530     H*        98 ON - OFFERTA            98 OFF- TARIFFA                *
000600010530     H*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**
000700010530     H*                                                                  *
000800140512     H* ATTENZIONE!!! E' STATA SCRITTA UNA DOCUMENTAZIONE DEL PROGRAMMA  *
000900140512     H*               E RELATIVE REGOLE DI STAMPA. IN CASO DI MODIFICHE  *
001000140512     H*               LEGATE ALLE REGOLE DI STAMPA AGGIORANERE LA DOCU-  *
001100140512     H*               MENTAZIONE.                                        *
001200140512     H*G:\AS400\Documentazione EDP\PGM utilità\Stampa tariffe Particolari*
001300140512     H*   clienti.doc                                                    *
001400010530     H*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**
001500010530      * TESTATA E DETTAGLIO TARIFFE
001600061122     FTNTAM01L  IF   E           K DISK    usropn
001700010530     FTNOFM01L  IF   E           K DISK    USROPN
001800010530     F                                     RENAME(TNTAM000:TNOFM000)
001900061122     FTITAD04L  IF   E           K DISK    usropn
002000061122     ftiofd01L  if   e           k disk    usropn
002100061122     f                                     rename(titad000:tiofd000)
002200010530      *
002300010530      * TESTATA E DETTAGLIO TARIFFE PARTICOLARI
002400061122     FTITPT01L  IF   E           K DISK    usropn
002500010530     FTIOPT01L  IF   E           K DISK    USROPN
002600010530     F                                     RENAME(TITPT000:TIOPT000)
002700061122     FTITPD01L  IF   E           K DISK    usropn
002800010629     FTIOPD01L  IF   E           K DISK    USROPN
002900010629     F                                     RENAME(TITPD000:TIOPD000)
003000010530      *
003100010530      * TARIFFE GIACENZE
003200061122     FTITGC01L  IF   E           K DISK    usropn
003300010621     FTIOGC01L  IF   E           K DISK    USROPN
003400010621     F                                     RENAME(TITGC000:TIOGC000)
003500010530      *
003600010530      * VISITE
003700091022     FTiVIS05L  IF   E           K DISK    usropn
003800010530      *
003900010530      * NOTE TARIFFE
004000010530     FTFNTC01L  IF   E           K DISK
004100010530      *
004200010530      * PIANO DEI CONTI
004300010530     FCNACO00F  IF   E           K DISK
004400010530     FCNIND00F  IF   E           K DISK
004500010530     FCNCLP00F  IF   E           K DISK
004600010530     FTFACO00F  IF   E           K DISK    USROPN
004700010530     F                                     RENAME(CNACO000:TFACO000)
004800010530     FTFIND00F  IF   E           K DISK    USROPN
004900010530     F                                     RENAME(CNIND000:TFIND000)
005000010530     FTFCLP00F  IF   E           K DISK    USROPN
005100010530     F                                     RENAME(CNCLP000:TFCLP000)
005200010530      *
005300010530      * FILE VARI
005400010530     FAZCPC03L  IF   E           K DISK
005500130905     fAZCMM01L  if   e           k disk
005600010530     FAZORG01L  IF   E           K DISK
005700010530     FTABEL00F  IF   E           K DISK
005800010725     FTNTBE01L  IF   E           K DISK
005900050209     fTivss02l  if   e           k disk
006000060816     ftipmg01l  if   e           k disk
006100080211     fCnabi00f  if   e           k disk
006200101015     ftisis01l  if   e           k disk
006300010627     FTNTA49P   O    E             PRINTER oflind(*in80) INFDS(PRTTAR)
006400010530     F*
006500010530     F*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**
006600010530     D*                 DEFINIZIONE SCHIERE
006700010530     D*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**
006800010530     D* Messaggi di errore
006900050324     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
007000010530     D*  TABELLE PROVINCIE E REGIONI
007100041220     D CCT             S              2    DIM(600)
007200041220     D SCT             S              2    DIM(600)
007300041220     D RCT             S              2    DIM(600)
007400041220     D NAR             S              3    DIM(600)
007500050204     d prvdes          s             20    dim(600)
007600050303     d noctdes         s              1    dim(600)
007700041220     D CRE             S              2    DIM(200)
007800041220     D DRE             S             20    DIM(200)
007900010530     D NAZ             S              3    DIM(20)
008000010601     D* CODIICI YTASSAZIONE DELLA LINEA DEL CLIENTE
008100041220     D CTL             S              2    DIM(600)
008200010530     D* SCHIERE PER TARIFFE PARTICOLARI
008300010530     D FTC             S              1    DIM(50)
008400010530     D DTC             S             25    DIM(50)
008500020108     D* ordinamento in stampa tariffe particolari
008600020108     D OST             S              2    DIM(50)
008700020108     D WOS             S              2    DIM(50)
008800020108     D TST             S              1    DIM(50)
008900020108     D CP              S              1    DIM(50)
009000020108     D* estensione tabella 1P per stampa
009100020108     D EDTC            S             31    DIM(50)
009200020108     D EDAC            S              1    DIM(50)
009300020109     D ESUD            S              1    DIM(50)
009400010601     D* SCHIERE PER LE NOTE
009500010601     D NOTE            S             78    DIM(4)
009600010606     D* --------------  SCHIERE TARIFFE IN DETTAGLIO ------------------------
009700010606     D* SCAGLIONI
009800010606     D SGL             S              7  2 DIM(18)
009900010606     D* INOLTRO
010000010606     D PRO             S              7  3 DIM(18)
010100010606     D* RAPPORTO PESO VOLUME
010200010606     D RPV             S              4  0 DIM(18)
010300010606     D*--------------- SCHIERE PER LA STAMPA -----------------
010400010606     D*"/" di divisione negli scaglioni da a
010500010606     D SGLB            S              1    DIM(18)
010600010606     D* SCAGLIONI senza decimali
010700010606     D SGLD            S              4  0 DIM(18)
010800010606     D SGLA            S              4  0 DIM(18)
010900010606     D* SCAGLIONI con 1 decimale
011000010606     D SG1D            S              3  1 DIM(18)
011100010606     D SG1A            S              3  1 DIM(18)
011200010606     D* SCAGLIONI con 2 decimali
011300010622     D SG2D            S              4  2 DIM(18)
011400010622     D SG2A            S              4  2 DIM(18)
011500080901     D* SCAGLIONI con 3 decimali
011600080901     D SG3D            S              4  3 DIM(18)
011700080901     D SG3A            S              4  3 DIM(18)
011800010606     D* DECODIFICA SOTTO GLI SCAGLIONI
011900010606     D ATF             S              7    DIM(18)
012000010607     D* ------------------- 6 SCAGLIONI PER LA STAMPA --------------------------
012100010607     D SSLB            S              1    DIM(6)
012200010607     D* SCAGLIONI SENZA DECIMALI
012300010607     D SSLD            S              4  0 DIM(6)
012400010607     D SSLA            S              4  0 DIM(6)
012500010607     D* SCAGLIONI CON 1 DECIMALI
012600010607     D SS1D            S              3  1 DIM(6)
012700010607     D SS1A            S              3  1 DIM(6)
012800010607     D* SCAGLIONI CON 2 DECIMALI
012900010622     D SS2D            S              4  2 DIM(6)
013000010622     D SS2A            S              4  2 DIM(6)
013100080901     D* SCAGLIONI CON 3 DECIMALI
013200080901     D SS3D            S              4  3 DIM(6)
013300080901     D SS3A            S              4  3 DIM(6)
013400010612     D* ------------------- 6 APPLICAZIONE TARIFFA FINITA ----------------------
013500010612     D STF             S              7    DIM(6)
013600010614     D* ------------------- 6 SCAGLIONI E DETTAGLI TARIFFARI -------------------
013700010614     D SSL             S              7  2 DIM(6)
013800080916     D SSLP            S              8  3 DIM(6)
013900010614     D TCT             S              7  3 DIM(6)
014000010614     D TMN             S              7  3 DIM(6)
014100010614     D TMX             S              7  3 DIM(6)
014200010614     D TIN             S              7  3 DIM(6)
014300010614     D* ------------------- SCHIERA DI COMODO ----------------------------------
014400010614     D COM             S              7    DIM(6)
014500010615     D CO4             S              4    DIM(6)
014600010614     D*  SKI TARIFFE X SCAGL. DI OGNI PROVINCIA(CTS) DELLA REGIONE
014700050616     D BIG             S             42    DIM(15)
014800050616     D BMN             S             42    DIM(15)
014900050616     D BMX             S             42    DIM(15)
015000050616     D BIN             S             42    DIM(15)
015100050616     D BSI             S              2    DIM(15)
015200050616     D BRG             S              1    DIM(15)
015300050616     d dprv            s             20    dim(15)
015400050616     d nodes           s              1    dim(15)
015500010614     D* SKIERE DI WORK
015600010614     D WIN             S             42    DIM(30)
015700010614     D WMN             S             42    DIM(30)
015800010614     D WMX             S             42    DIM(30)
015900050616     D WCT             S              3    DIM(15)
016000010614     D WTI             S             42    DIM(30)
016100010614     D* SKIERE DI STAMPA
016200010614     D PDE             S             29    DIM(30)
016300010614     D PAM             S              7  3 DIM(6)
016400010614     D PMN             S              7  3 DIM(6)
016500010614     D PMX             S              7  3 DIM(6)
016600010614     D PIN             S              7  3 DIM(6)
016700010614     D*     % PER SCAGLIONI A VALORE
016800010614     D AI2             S              1    DIM(6)
016900010615     D*     SCAGLIONI STAMPA RAPPORTO PESO VOLUME
017000010615     D SPV             S              4  0 DIM(18)
017100010618      *-------------------------------------------------------------------------
017200010618      *         SCHIERE SCAGLIONI DETTAGLIO TARIFFE PARTICOLARI
017300010618      *-------------------------------------------------------------------------
017400010618     D* --------------  SCHIERE TARIFFE IN DETTAGLIO ------------------------
017500010618     D* SCAGLIONI
017600010618     D*
017700080916     D SCP             S              8  3 DIM(18)
017800010619     D* TARIFFA PARTICOLARE DETTAGLIO
017900010621     D TAP             S              7  3 DIM(6)
018000010621     D TPN             S              7  3 DIM(6)
018100010621     D TPX             S              7  3 DIM(6)
018200010621     D AIP             S              1    DIM(6)
018300010620     D*
018400010621     D PER             S              1    DIM(6)
018500010619     D*
018600021204      *          SCHIERE PER STAMPA TARIFFE PARTICOLARI SU + RIGHE
018700021204     d TptA            s             31    Dim(50)
018800021204     d TptB            s             25    Dim(50)
018900021204     d Tptc            s             31    Dim(50)
019000010614     D*
019100031119     D*-------------------------------------------------------------------------
019200101015      * DATE SCATTI ISTAT
019300101015     D DIA             S              8  0 DIM(999)
019400060203
019500080609      * schiera filiali abilitate al lasciato avviso
019600080609     d sklav           s              3  0 dim(999)
019700010601      *-------------------------------------------------------------------------
019800010530     D* DS DEL FILE TABEL00F
019900010621     D DSCV          E DS
020000010530     D DS1H          E DS
020100010530     D DS1P          E DS
020200020109     D DSSP          E DS
020300010530     D DS2G          E DS
020400010618     D DS3IDP        E DS
020500010530     D DSCT          E DS
020600010530     D DS15          E DS
020700010531     D DSTR          E DS
020800010601     D DSFA          E DS
020900010614AI2  D DS4U          E DS
021000031119      *
021100010725     D* DS DEL FILE TNTBE00F
021200010725     D DTAA          E DS
021300010531     D*
021400010625     D* VALORI STANDARD AZIENDALI
021500010625     D DGEI          E DS
021600020625      * limite c/assegni
021700020625     d dlca          e ds
021800010531     D* DS flag operativi del file tariffe
021900010531     D DSTA01        E DS
022000010601     D* DS organigramma per poste
022100010601     D OG143         E DS
022200020530      * DS organigramma per codice regione
022300020530     d og147         e ds
022400020624      *--------
022500020624      * Ds di riferimento al file esterno AzUte00F
022600020624      *--------
022700020624     d AzuteDs       e ds                  ExtName(AzUte00F)
022800020624      *--------
022900020624      * Ds per dati organigramma
023000020624      *--------
023100020624     d DDatiUte      e ds
023200020624      *--------
023300020624      * Parametri per richiamo al pgm di controllo profilo utenti
023400020624      *--------
023500020624     d Tibs34Ds      e ds                  Inz
023600020624
023700010531     D*
023800010530     D KPJBA         E DS
023900010531     D*
024000160623      // - Ricerca/Controllo tabelle
024100160623     d TIBS02DS      e ds                  inz
024200010625     D*
024300020919     D fnlv45ds      E DS
024400021022     D fnlv41ds      E DS
024500041230     d Tibs10ds      e ds
024600080609     d dlav          e ds
024700080609     d tntbeds       e ds                  extname(tntbe00f)
024800160623      // - Tabella limiti Tipo Servizio
024900160623     d dLTS          e ds
025000010530      *-------------------------------------------------------------------------
025100010601      * ds interne
025200010601     D*
025300010601     D CDPKEY          DS
025400010601     D  CDPG                   1      3
025500010601     D  GGPG                   2      3
025600010601     D  CDLIN                  4      4
025700010627      *
025800010627     D PRTTAR          DS
025900010627     D  curline              367    368I 0
026000010627      *
026100010601      *-------------------------------------------------------------------------
026200010531      *
026300010531     D DATA_eur        S               D   DATFMT(*eur)
026400060816     d data_iso1       s               d   datfmt(*iso)
026500060816     d data_iso2       s               d   datfmt(*iso)
026600010531      *
026700130905     d wCMMcod         s                   like(CMMcod)   inz
026800010601     d cod             s                   like(tblcod)
026900010625     d costpg          s                   like(§fatpg)
027000010601     d ctrata          s                   like(§trata)
027100010531     D ds1ksc          s                   like(tamksc)
027200010531     D ds1ctr          s                   like(tamctr)
027300010531     D ds1prg          s                   like(tamprg)
027400010601     D notapl          s                   like(ntcapl)
027500010621     D notke1          s                   like(ntcnk1)
027600010621     D notke2          s                   like(ntcnk2)
027700010601     D nottnt          s                   like(ntctnt)
027800111103     D kcod            s                   like(tbecod)
027900010601     D key             s                   like(tblkey)
028000010601     D ksc             s                   like(acoksc)
028100010724     D oltre           s                   like(tpdsgl)
028200010531     D parsks          s                   like(tamksc)
028300060307     d savapl          s                   like(tptapl)
028400010618     D savftc          s                   like(tptftc)
028500010621     D savorp          s                   like(tpdorp)
028600010801     D savorp1         s                   like(tpdorp)
028700010621     D savsgl          s                   like(tpdsgl)
028800010806     D recsgl          s                   like(tpdsgl)
028900010604     D rotcap          s                   like(tadcap)
029000010604     D rotcts          s                   like(tadcts)
029100010604     D rotlnp          s                   like(tadlnp)
029200010604     D wrkcap          s                   like(tadcap)
029300010604     D wrkcts          s                   like(tadcts)
029400010626     D wrkftc          s                   like(tpdftc)
029500010604     D wrklnp          s                   like(tadlnp)
029600010626     D wrkorp          s                   like(tpdorp)
029700010626     D wrksgl          s                   like(tpdsgl)
029800010531      *-------------------------------------------------------------------------
029900021204     d atp1            s              2  0
030000021204     d atpc            s              2  0
030100010614     D b               s              2  0
030200021205     D blk10           s             10    INZ(*ALL'.')
030300030318     D blk12           s             13    INZ(*ALL'.')
030400030318     D blk21           s             21    INZ(*ALL'.')
030500030318     D blk25           s             25    INZ(*ALL'.')
030600021205     D blk36           s             36    INZ(*ALL'.')
030700021205     D blk56           s             56    INZ(*ALL'.')
030800010614     D c               s              2  0
030900010604      * se esiste il cap nel 1° scaglione metto 'S'
031000010604     D cap1scaglio     s              1
031100021205     d CntList         s              5i 0 Inz(*Zeros)
031200010615     D codorp          s              1
031300010615     D codreg          s              2
031400010615     D codsig          s              2
031500010601     D comfil          s              3  0
031600020109     D comftc          s              1
031700010613     D compro          s              1
031800010613     D comreg          s              2
031900010621     D com04           s              4
032000010614     D como29          s             29
032100010614     D como42          s             42
032200010614     D cmno42          s             42
032300010614     D cmxo42          s             42
032400010614     D cino42          s             42
032500021204     d conta           s              1  0
032600010608     D ctrcap          s              1
032700010601     D datastampa      s             10
032800010723     D desctr          s             13
032900050204     d desprv          s             20
033000010615     D desreg          s             14
033100010618     D de1tpg          s              9
033200010618     D de3tpg          s             13
033300010801     D dissca2         s             63
033400010801     D dissca3         s             50
033500010801     D dissca4         s             37
033600010801     D dissca5         s             24
033700010801     D dissca6         s             11
033800010608     D ds3um           s             20
033900010531     D ds6pr2          s              1
034000010621     D errmsg          s             78
034100010614     D h               s              2  0
034200010601     D i96dri          s              8  0
034300020108     D j               s              3  0
034400020108     D jj              s              3  0
034500020108     D jjj             s              3  0
034600010605     D lnpcli          s              2
034700010724     D mintas          s              3  0
034800010619     D num4            s              4  0
034900050303     d nostdes         s              1
035000020114     D oltrenodec      s             10  0
035100010601     D o96ver          s              5  0
035200010614     D p               s              2  0
035300010531     D k               s              3  0
035400010531     D k1              s              3  0
035500010618     D k2              s              3  0
035600010607     D k4              s              3  0
035700010622     D k5              s              3  0
035800010604     D Y               s              3  0
035900010618     D X               s              2  0
036000010531     D unoctr          s              1  0
036100010625     D recval          s              3  0
036200010604     D rotpro          s              1
036300010613     D rotreg          s              2
036400010531     D staddt          S             10
036500010531     D stadst          S             10
036600020624     d stalnp          s             20
036700010606     D stino           S              1
036800010725     D stpanno         s              4  0
036900010725     D stpmod          s              3  0
037000030318     D stpper          s              7
037100010615     D totino          s             15  3
037200050526     d totrig          s              5  0
037300010731     D unmis           s              3
037400030318     D valper          s              6  3
037500080211     d wagenzia        s             16
037600010615     D waro2           s              4  0
037700010615     D warf2           s              4  0
037800010615     D warl2           s              4  0
037900100506     D wata            s              5  0
038000100506     D wata1           s              6  1
038100100506     D wata2           s              7  2
038200080211     d wbanca          s             20
038300060925     d wctr1           s              1
038400051206     d wema84          s              1    Inz('0')
038500050526     d wema85          s              1    Inz('0')
038600050526     d wema87          s              1    Inz('0')
038700050526     d wema88          s              1    Inz('0')
038800090612     d wema89          s              1    Inz('0')
038900060816     d wgio            s              5  0
039000021204     d Witr            s              8  3
039100010618     D wnumsc          s              2  0
039200051006     d wnrscaglio      s              2  0
039300010625     D w_per           s              1
039400050930     d woktar          s              1    Inz('0')
039500021204     d WokTp           s              1    Inz('0')
039600080211     d wpos            s              2  0
039700080211     d wpos1           s              2  0
039800080211     d wpos2           s              2  0
039900041230     d wpsw            s              1    Inz('0')
040000050204     d wragprv         s              1    Inz('0')
040100010615     D wrpv            s             24
040200010626     D wrkdestpg       s             13
040300010604     D wrkreg          s              2
040400010604     D wrkpro          s              1
040500020624     D wrkslp          s             20
040600020703     D wrkprtlnp       s             80
040700010613     D wsgl72          S              7  2
040800080916     D wsgl83          S              8  3
040900011001     D w_ellimris      S              1
041000021205     D w_tarpar        S             83
041100021205     D w_tarparest     S             83
041200020531     d w_oklnp         s              1
041300020531     d w_okstp         s              1
041400010723     D w0010           S              1  0
041500010606     D w0020           S              2  0
041600010601     D w003a           S              3
041700010606     D w0030           S              3  0
041800010606     D w0050           S              5  0
041900051006     d xx              s              2  0
042000051006     d yy              s              2  0
042100080117     d xy              s              2  0
042200010606     D z               s              3  0
042300080609     d xxx             s              3  0
042400091112     d $trattativa     s              1n
042500101015     d $EoF            s              1n   inz(*off)
042600110216     d savk            s              3  0
042700160623     d Stpvlm          s              6a   inz
042800160623     d Stppkg          s              8a   inz
042900160623     d Decimali        s              3s 0 inz
043000021205      *-----------------
043100021205      * PARAMETRI DI PASSAGGIO AL PROGRAMMA DI "GIUSTIFICAZIONE" TESTO
043200021205      *-----------------
043300021205     d TxtInput        S            200    varying
043400021205     d Lungh           S              5i 0 inz(105)
043500021205     d TxtGiust        S            200    varying
043600021205     d TxtResto        S            200    varying
043700021205
043800010625     D* DEFINIZIONE COSTANTI
043900060817     D CSPE1           C                   CONST('solo sulle spese di sosta')
044000010625     D CSPE2           C                   CONST('su tutte le spese')
044100060817     D CSPE3           C                   CONST('su spese sosta,dossier, -
044200060817     D                                     varie')
044300060817     D CSPE12          C                   CONST('only on holding costs')
044400060703     D CSPE22          C                   CONST('on all costs')
044500060817     D CSPE32          C                   CONST('on holding, dossier, -
044600060817     D                                     various costs')
044700060817     D CSPE13          C                   CONST('uniquement sur les frais -
044800060817     D                                     d''arrêt')
044900060817     D CSPE23          C                   CONST('sur tous les frais')
045000060817     D CSPE33          C                   CONST('sur les frais d''arrêt, de -
045100060817     D                                     dossier, divers')
045200060817     D CSPE14          C                   CONST('nur auf Kosten für -
045300060817     D                                     Aufbewahrung')
045400060817     D CSPE24          C                   CONST('auf alle Kosten')
045500060817     D CSPE34          C                   CONST('uf Kosten für Aufbewahrung,-
045600060817     D                                     Bearbeitung,Verschiedenes')
045700140527     D Cost_m_gratis   C                   CONST(': GRATIS fino al 31/12/2015-
045800140528     D                                     . Euro 0,20 dal 1/1/2016.')
045900140528     D Cost_m_gratisI  C                   CONST(': FREE until 31/12/2015. Eu-
046000140528     D                                     ro 0,20 as of 1/1/2016.')
046100140528     D Cost_m_gratisF  C                   CONST(': GRATUIT jusqu''au 31/12/20-
046200140528     D                                     15. Euro 0,20 à partir du 1/1/2016.')
046300140528     D Cost_m_gratisD  C                   CONST(': KOSTENLOS bis zum 31.12.20-
046400140528     D                                     15. Euro 0,20 ab 01.01.2016.')
046500140218
046600140218       // -?DS campo TPTflo
046700140218     d dTPT01        e ds
046800160623
046900160623      //---------------------------------------------------------------
047000160623      //?Definizione Prototipi.
047100160623      //---------------------------------------------------------------
047200160623      /copy gaitrasrc/srcprotopr,TIBS02R
047300160623
047400160623      //---------------------------------------------------------------
047500091022
047600091022     itivis000
047700091022     i              visesi                      wisesi
047800091022
047900010530     C****************************************************************
048000010530     C*  RIEPILOGO INDICATORI
048100010530     C***************************************************************
048200010604     C* 02    - TARIFFA EUROEXPRESS
048300010618     C* 04    - TARIFFA PARTICOLARE A ZERO
048400010626     C* 06    - TARIFFA PARTICOLARE ISOLA
048500010618     C* 08    - STAMPA MINIMO TASSABILE PER TARIFFA A Q.LE
048600010801     C* 09    - TIPO PAGAMENTO A VALORE (TARIFFA %) TARIFFE PARTICOLARI
048700010626     C* 10    - LEGGO TARIFFE PARTICOLARI
048800061207      * 12    - STAMPA FUEL (tariffa "f ") A SCAGLIONI)
048900010626     C* 18    - STAMPA ARROTONDAMENTI
049000010626     C* 19    - STAMPA RAPPORTO PESO VOLUME
049100010626     C* 21    - MANDATO A VALORE VARIABILE
049200010626     C* 22    - ELEVAZIONE LIMITE RISARCIBILE
049300010626     C* 24    - MANDATO FITTIZIO
049400010531     C* 25    - TARIFFA DPD
049500010626     C* 26    - CONDIZIONA LA STAMPA DEL "PER" DI FIANCO ALLA VALIDITA' TARIFFA
049600041129     C* 27    - CONDIZIONA LA STAMPA DELL'ADDIZIONALE DI GESTIONE 1° riga
049700010928     C* 28    - CONDIZIONA LA STAMPA DELLE SPESE GIACENZA VARIE A SPEDIZIONE
049800041129     C* 29    - CONDIZIONA LA STAMPA DELL'ADDIZIONALE DI GESTIONE 2° riga
049900010626     C* 31    - INDICATORE DI COMODO
050000010626     C* 35    - INDICATORE DI COMODO
050100010606     C* 37    - SCAGLIONI CON UN SOLO DECIMALE
050200010618     C* 40/46 - CONDIZIONA LA STAMPA DEGLI SCAGLIONI
050300010615     C* 48    - STAMPO IL MINIMO
050400010615     C* 49    - STAMPO IL MASSIMO
050500010725     C* 51/56 - CONDIZIONA LA STAMPA DEGLI SCAGLIONI VUOTI
050600010725     C* 57    - STAMPO INOLTRO
050700010615     C* 61/66 - CONDIZIONA LA STAMPA DEL DETTAGLIO TARIFFARIO
050800010725     C* 72    - CONDIZIONA LA STAMPA DELL'ADDEBITO SPESE DI INCASSO
050900010628     C* 73    - CONDIZIONA LA STAMPA DEL RESPONSABILE TRASPORTI
051000010626     C* 75    - STAMPO MINIMO
051100010628     C* 77    - CONDIZIONA LA STAMPA DEL E.P.C.
051200010626     C* 78/79 - CONDIZIONA LA STAMPA DELLA TESTATA DEGLI SCAGLIONI
051300010627     C* 80    - OVERFLOW
051400010627     C* 81    - CONDIZIONA LA STAMPA DELLA TESTATA ASSICURAZIONI MERCE
051500091117      * 85    - Trattativa in stampa
051600010626     C* 86    - STAMPO MASSIMO
051700010531     C* 87    - TARIFFA A VALORE
051800020114     C* 88    - OLTRE        CON DECIMALI
051900020114     C* 89    - ARRONDAMENTI CON DECIMALI
052000010606     C* 91    - SE ACCESO SCAGLIONI CON DECIMALI
052100010530     C* 92/97 - STAMPA DATI DALLA DS1H
052200010530     C* 98    - ON PROVENGO RA OFFERTA
052300010530     C* 99    - INDICATORE GENERALE DI ERRORE
052400010530     C*****************************************************************
052500010531      *-------------------------------------------------------------------------
052600010531      *    T E S T A T A    S T A M P A    T A R I F F A
052700010531      *-------------------------------------------------------------------------
052800021022      *
052900021022      * Per errore di reperimento tetso vado a fine pgm
053000021022     C                   IF        $NoStp   = *on
053100021022     C                   GOTO      FINE
053200021022     C                   ENDIF
053300080609
053400080609     c/exec sql
053500080609     c+ set option dynusrprf = *owner, closqlcsr = *endmod
053600080609     c/end-exec
053700080609
053800080609      * carico tabella filiali attive al lasciato avviso
053900080609     c                   exsr      carlav
054000050930
054100050930     c                   eval      woktar = *Off
054200010531      *
054300010531      * RECUPERO IL CLIENTE
054400010601     C                   EXSR      SR_RECCLI
054500010601      *
054600010601      * SE 99 ACCESO ERRORE E VADO A FINE NON TROVATO  CLIENTE IN ANAGRAFICA
054700010601      *
054800010601     C                   IF        *IN99
054900010601     C                   GOTO      FINE
055000010601     C                   ENDIF
055100010601      *
055200010601      * DECODIFICO I DATI DEL CLIENTE
055300010601      *
055400010601     C                   EXSR      SR_DECANA
055500010601      *
055600010601      * RECUPERO NOTE TARIFFA/OFFERTA CLIENTE E RESPONSABILE TRASPORTI
055700010601      *
055800010601     C                   EXSR      SR_RECNOT
055900010601      *
056000010601      * DECODIFICO I DATI DEL PUNTO OPERATIVO
056100010601      *
056200010601     C                   EXSR      SR_DECPO
056300010601      *
056400010601      * aggancio testata tariffa / offerta
056500010621     c                   eval      ds1ksc = parsks
056600010531     c                   eval      ds1prg = i45prg
056700010531     c                   eval      ds1ctr = i45ctr
056800010601
056900061122     c  n98ds1ktm        chain     tntam000
057000061122     c   98ds1ktm        chain     tnofm000
057100010531
057200010531      * decodifica delle condizioni generali
057300010531
057400010531     c                   exsr      SR_DECTAM
057500020205
057600020205      * carico le tariffe particolari
057700020205     c                   exsr      Sr_Cartarp
057800010604      *
057900010604      * stampo la prima parte della tariffa
058000010604      *
058100010622     c                   write     ta49pPP
058200010604     c                   write     ta49p01
058300010531      *
058400010605      * elaboro dettaglio tariffario per recuperare scaglioni/inoltro/rpv
058500010604      *
058600010604     c                   exsr      SR_SCAGLIONI
058700050930      * se si sono vericati errori non continuo
058800050930     c                   if        *in99
058900050930     c                   goto      fine
059000050930     c                   endif
059100010606      *
059200010606      * verifico se devo stampare l'inoltro alla fine della tariffa xchè uguale
059300010606      * per tutti oppure no
059400010606     c                   exsr      SR_STINO
059500020214      * se si sono vericati errori non continuo
059600020214     c                   if        *in99
059700020214     c                   goto      fine
059800020214     c                   endif
059900010607      * elaboro e stampo il dettaglio tariffario
060000010606      *
060100010607     c                   exsr      SR_ELADET
060200010614      * se si sono vericati errori non continuo
060300010614     c                   if        *in99
060400010614     c                   goto      fine
060500010614     c                   endif
060600010628      *
060700010628      * stampo arrotondamenti
060800010628     c                   EXSR      SR_ARROTO
060900010618      * stampo i dati della testata tariffa
061000010618      *
061100010618     c                   exsr      SR_STATAM
061200050930     c                   eval      woktar = *On
061300010618      * routine gestione tariffe particolari
061400010618     c                   exsr      SR_STATPT
061500021204      * routine stampa sk unica tariffe particolari
061600021204     c                   ExSr      Sr_StaTpt1
061700010625      * routine di gestione tariffe giacenza
061800010625     c                   exsr      SR_STATGC
061900010625      * stampo condizioni di pagamento
062000010625      *
062100010627     c                   exsr      SR_OVER
062200010625      *
062300010625     c                   write     ta49pcon
062400010625      * se pagamento che utilizza la banca stampo anche questa riga
062500091016      * pagamento RIBA = '1' RID = '2'
062600091016     c                   if        costpg = '1' or costpg = '2'
062700010625      *
062800010627     c                   exsr      SR_OVER
062900010625      *
063000010625     c                   write     ta49pcon1
063100010625     c                   endif
063200021204      * Per tutte le tariffe stampo dicitura assicurazione merce
063300150908     c                   Add       4             curline
063400031008     c                   ExSr      Sr_Over
063500021204     c                   Write     Ta49pam
063600060324      * Per tutte le tariffe italia stampo anche la parte relativa all'AC BASE
063700060324     c                   If        Not *In25 and Not *In02
063800150917     c                   Add       2             curline
063900150917     c                   ExSr      Sr_Over
064000060324     c                   Write     Ta49pam1
064100060324     c                   endif
064200040211
064300040211      * Per tutte le tariffe italia stampo paragrafo x telefono destinatario
064400040211     c                   If        Not *In25 and Not *In02
064500040216     c                   Add       6             curline
064600040211     c                   ExSr      Sr_Over
064700040211     c                   Write     Ta49pte
064800040211     c                   EndIf
064900040211      * Per tutte le tariffe EEX stampo paragrafo x telefono destinatario
065000040211     c                   If        *In02
065100040211     c                   Add       3             curline
065200040211     c                   ExSr      Sr_Over
065300040211     c                   Write     Ta49ptee
065400040211     c                   EndIf
065500020624      * Per EuroExpress
065600020624     c                   if        *in02
065700020625      * cerco limite massimo per il c/assegno
065800020625     c                   clear                   dlca
065900160623     c                   clear                   tibs02ds
066000020625     c                   movel     'C'           t02mod
066100020625     c                   movel     knsif         t02sif
066200020625     c                   movel     'LCA'         t02cod
066300020625     c                   movel(p)  '011'         t02ke1
066400020625     c                   call      'TIBS02R'
066500020625     c                   parm                    kpjba
066600160623     c                   parm                    tibs02ds
066700020625     c                   if        t02err = *blanks
066800020625     c                   movel     t02uni        dlca
066900020625     c                   endif
067000020625     c                   eval      stalca = §lcax2
067100020412      * stampa dicitura fissa per porti assegnati export
067200031008     c                   Add       3             curline
067300031008     c                   exsr      SR_OVER
067400020412     c                   write     ta49pee
067500020624      * stampa dicitura fissa per contrassegni export
067600061116     c                   Add       8             curline
067700031008     c                   exsr      SR_OVER
067800020624     c                   write     ta49pee1
067900020412     c                   endif
068000050103      *
068100050526      * Se offerta
068200050526      * Capitolo dei servizi ON LINE + E-MAIL
068300050526     c                   If        *In98
068400050526     c                   ExSr      Sr_Comunica
068500050526     c                   EndIf
068600041229      * stampa condizioni
068700010625      *
068800010627     c                   exsr      SR_OVER
068900010625     c                   write     ta49pe1
069000010625      *
069100041229      * stampa condizioni e firme finali
069200010627     c                   exsr      SR_OVER
069300010625     c                   write     ta49pfi
069400010625      *
069500010627     c                   exsr      SR_OVER
069600010625     c                   write     ta49pe2
069700010627      *
069800010627     c                   exsr      SR_OVER
069900010627     c                   write     ta49pe2B
070000010627      *
070100010627     c                   exsr      SR_OVER
070200010627     c                   write     ta49pe2C
070300010627      *
070400010627     c                   exsr      SR_OVER
070500010627     c                   write     ta49pe2D
070600010627      *
070700010627     c                   exsr      SR_OVER
070800010627     c                   write     ta49pe2E
070900010627      *
071000010627     c                   exsr      SR_OVER
071100010627     c                   write     ta49pe2F
071200110905      *
071300110905     c                   exsr      SR_OVER
071400110905     c                   write     ta49pe2G
071500011009      *
071600011009     c                   exsr      SR_OVER
071700010625     c                   write     ta49pfi
071800010625      *
071900010614      *
072000010614     c     fine          tag
072100041229
072200010614     c                   seton                                        lr
072300010601     C*------------------------------------------------------------------------
072400010601     C*   SR_RECCLI  RECUPERO CLIENTE
072500010601     C*------------------------------------------------------------------------
072600010601      *
072700010601     C     SR_RECCLI     BEGSR
072800010601      *
072900010601      *
073000010601      * V I S I T A
073100010601      *
073200010601     C                   IF        *IN98
073300010601      *
073400091022     c                   if        i45ast = 'T'
073500091022     C     i45nrv        chain     tivis000
073600091022     c                   endif
073700010601      * recupero ANAGRAFICA CLIENTE
073800010601      * se cliente nuovo leggo nel file anagrafici visite
073900010601     C                   z-add     visnrv        ksc
074000010601     c     ds7kac        chain     tfaco000                           99
074100010601     c  n99ds7kac        chain     tfind000                           99
074200010601     c  n99ds7kac        chain     tfclp000                           99
074300010601      * se non trovato cerco nell'anagrafica clienti
074400010601     c                   if        *in99
074500010601     C                   z-add     visksc        ksc
074600010601     c     ds7kac        chain     cnaco000                           99
074700010601     c  n99ds7kac        chain     cnind000                           99
074800010601     c  n99ds7kac        chain     cnclp000                           99
074900010601     c                   end
075000010601      *
075100010601     c                   end
075200010601      *
075300010601      * RECUPERO IL CLIENTE
075400010601      *
075500010601      * T A R I F F A
075600010601      *
075700010601     C                   IF        not *IN98
075800010601     C                   z-add     parsks        ksc
075900010601     c     ds7kac        chain     cnaco000                           99
076000010601     c  n99ds7kac        chain     cnind000                           99
076100010601     c  n99ds7kac        chain     cnclp000                           99
076200010601     c                   endif
076300010601      * se il cliente sia in visita che in tariffa non è stato trovato
076400010601      * viene stampato l'errore
076500010601     c                   if        *in99
076600010601     c                   move      MSG(3)        errmsg
076700010601     c                   exsr      staerr
076800010622      *
076900010622     c                   else
077000010622      * se non c'è errore valorizzo i campi in stampa
077100010622     c                   eval      picrag = acorag
077200010622     c                   eval      picind = indvia
077300010622     c                   eval      piccap = indcae
077400010622     c                   eval      picloc = indcit
077500010622     c                   eval      picpro = indprv
077600010601     c                   endif
077700010601      *
077800010601     C                   ENDSR
077900010601     C*------------------------------------------------------------------------
078000010601     C*   SR_DECANA  DECODIFICA DATI CLIENTI PER LA STAMPA
078100010601     C*------------------------------------------------------------------------
078200010601      *
078300010601     C     SR_DECANA     BEGSR
078400010622      * valorizzo i numeri riferimento
078500010622     c                   eval      picnr1 = i45nr1
078600010622     c                   eval      picnr2 = i45nr2
078700010601      *
078800010601      * ---------- decodfico la nazione se diversa da italia ------------
078900010601      *
079000010601     c                   if        indsta <> *blanks
079100010601     c                   eval      cod ='15'
079200010601     c                   movel     indsta        w003a
079300010601     c                   movel(p)  w003a         key
079400060724     c     tabkeyl       chain     tabel00f
079500010601     c                   if        %found(tabel00f)
079600010621     c                   movel     tbluni        desnar
079700010601     c                   else
079800010621     c                   clear                   desnar
079900010601     c                   endif
080000010601      *
080100010601     c                   endif
080200010601      *
080300010601      * ------------------ decodifico il commerciale --------------------
080400010601      *
080500010601      * visita
080600130905     c                   clear                   wCMMcod
080700010601      *
080800010601     c                   if        *in98
080900010601      * commerciale visita
081000010601     c                   if        viscmm > 0
081100130905     c                   z-add     viscmm        wCMMcod
081200010601     c                   endif
081300010601      * tariffa
081400010601     c                   else
081500010601      * commerciale tariffa
081600010601     c                   if        clpage > 0
081700130905     c                   z-add     clpage        wCMMcod
081800010601     c                   endif
081900010601      *
082000010601     c                   endif
082100010601      *
082200130905     c                   if        wCMMcod <> *zero
082300010601      *
082400130905     c     wCMMcod       chain     AZCMM000
082500130905     c                   if        %found(AZCMM01L)
082600130905     c                   movel     CMMdes        descom
082700010601     c                   else
082800010601     c                   clear                   descom
082900010601     c                   endif
083000010601      *
083100010601     c                   endif
083200010601      *
083300010601      * ------------------- decodifico il pagamento -------------------------
083400010601      *
083500010621     c                   if        indcdp <> *blanks
083600010625     c                   move      indcdp        cdpg
083700010601     c                   eval      cdlin = '1'
083800010601     c                   movel(p)  cdpkey        key
083900010601     c                   eval      cod = 'FA'
084000010601      *
084100060605     c     tabkeyl       chain     tabel00f
084200010601     c                   if        %found(tabel00f)
084300010601     c                   movel     tbluni        dsfa
084400010601     c                   movel     §fades        descdp
084500010601      * mi salvo il tipo pagamento per pilotare la stampa della banca d'appoggio
084600010601      * oppure (es. Rimessa diretta non si stampa la banca d'appoggio)
084700010625     c                   eval      costpg = §fatpg
084800010601     c                   endif
084900010601      *
085000010601     c                   endif
085100010601      * ----- converto spese di incasso in euro
085200010621     c                   if        indsin > *zeros
085300010725      *
085400010725     C                   SETON                                        72
085500010601      *
085600030210
085700030210     c                   Move      IndSin        ValSin
085800030210
085900010601     c                   endif
086000010601      * ------------ banca d'appoggio ---------------------------------
086100080211     c                   clear                   pcabi
086200080211     c                   clear                   pccab
086300080211     c                   clear                   desbna
086400080211    1c                   if        indabi <> 99999 or indcab <> 99999
086500080211     c                   eval      pcabi = indabi
086600080211     c                   eval      pccab = indcab
086700080211     c     kcnabi        chain     cnabi00f
086800080211    2c                   if        %found(cnabi00f) and abiann <> '*'
086900080211     c                   clear                   wagenzia
087000080211     c                   clear                   wbanca
087100080211     c                   Eval      wbanca = abiist
087200080211      * Compongo il nome della banca con località o comune
087300080211    3c                   If        abiage = *Blanks
087400080211     c                   Select
087500080211     c                   When      abiloc <> *Blanks
087600080211     c                   Eval      wagenzia = abiloc
087700080211     c                   When      abicom <> *Blanks
087800080211     c                   Eval      wagenzia = abicom
087900080211     c                   EndSl
088000080211      * Compongo il nome della banca con agenzia + località o comune
088100080211   x3c                   Else
088200080211      * Cerco il primo byte vuoto dell'agenzia
088300080211     c                   Eval      wpos = %checkr(' ':abiage)
088400080211
088500080211     c                   Eval      wagenzia = %subst(abiage:1:wpos)
088600080211     c                   Eval      wpos1 = wpos + 2
088700080211     c                   Eval      wpos2 = 16 - wpos1
088800080211      * Se wpos2 inferiore a zero lo imposto a 0
088900080211     c                   If        wpos2 < *Zeros
089000080211     c                   Clear                   wpos2
089100080211     c                   EndIf
089200080211      * Se posso aggiungere anche solo 1 carattere procedo
089300080211    4c                   If        wpos > 1 and wpos < 16
089400080211      * Cerco di riempire quello che resta con la località o il comune
089500080211     c                   Select
089600080211     c                   When      abiloc <> *Blanks
089700080211     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
089800080211     c                             %subst(abiloc:1:wpos2)
089900080211     c                   When      abicom <> *Blanks
090000080211     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
090100080211     c                             %subst(abicom:1:wpos2)
090200080211     c                   EndSl
090300080211    4c                   EndIf
090400080211
090500080211    3c                   EndIf
090600080211    2c                   EndIf
090700080211     c                   eval      desbna = wbanca + wagenzia
090800080211    1c                   endif
090900010601      *
091000010601      * --------- decodifico tipo pagamento contrassegno ---------------
091100010601      *
091200010601     c                   eval      cod = '4U'
091300010601     c                   movel(p)  clpfpc        key
091400060605     c     tabkeyl       chain     tabel00f
091500010601     c                   if        %found(tabel00f)
091600010601     c                   movel     tbluni        ds4u
091700010621     c                   movel     §4udes        comaas
091800010601      * se è un tipo pagamento che richiede obbligatoriamente il codice ABI CAB
091900010601      * stampo anche la decodifica della banca d'appoggio
092000010601     c                   if        §4uabi = 'S'
092100080118     c                   eval      combab1 = %subst(clpbab:1:4)
092200080118     c                   eval      combab2 = %subst(clpbab:5:4)
092300080118     c                   eval      combab3 = %subst(clpbab:9:4)
092400080118     c                   eval      combab4 = %subst(clpbab:13:4)
092500080118     c                   eval      combab5 = %subst(clpbab:17:4)
092600080118     c                   eval      combab6 = %subst(clpbab:21:4)
092700080118     c                   eval      combab7 = %subst(clpbab:25:4)
092800080211     c                   eval      combab8 = %subst(clpbab:29:4)
092900010601     c                   endif
093000010601      *
093100010601     c                   endif
093200010601      *
093300010601     c                   ENDSR
093400010601     C*------------------------------------------------------------------------
093500010601     C*   SR_RECNOT  RECUPERA NOTE TARIFFE/OFFERTA CLIENTI
093600010601     C*------------------------------------------------------------------------
093700010601      *
093800010601     C     SR_RECNOT     BEGSR
093900010601      *
094000010601      * ---------- chiave note tariffe/offerta cliente ------------------
094100010601      *
094200010601      * visita
094300010601     c   98              eval      notapl = 'V'
094400010601      * tariffa
094500010601     c  N98              eval      notapl = 'C'
094600091112      * trattativa
094700091112     c                   if        $trattativa
094800091112     c                   eval      notapl = 'T'
094900091112     c                   endif
095000010625      *
095100010625     c                   clear                   b
095200010601      *
095300010601     c                   movel     I45KCC        notke1
095400010625     c                   move      parsks        notke1
095500010601      * note legate alla tariffa/offerta
095600010601     c                   movel     '10'          nottnt
095700010601     c                   movel     I45CTR        notke2
095800010601      *
095900010601     c     keyntc        setll     tfntc01l
096000010601     c                   do        *hival
096100010601     c     keyntc        reade     tfntc01l
096200010601
096300010601     c                   if        %eof(tfntc01l)
096400010601      * imposto la chiave senza il codice tariffa e mi riposiziono
096500010601     c                   clear                   notke2
096600010601     c     keyntc        setll     tfntc01l
096700010601     c                   leave
096800010601     c                   endif
096900010601
097000010601     c                   if        ntcflt = 'A'
097100010601      * se è annullato leggo il successivo
097200010601     c                   iter
097300010601     c                   else
097400010601
097500010601      * se invece non è annullato ed esiste mi riposiziono per il caricamento
097600010601      * della schiera di comodo delle note
097700010601     c     keyntc        setll     tfntc01l
097800010601     c                   leave
097900010601     c                   endif
098000010601
098100010601     c                   enddo
098200010601      * carico le note in schiera
098300010601     c                   do        *hival
098400010601     c     keyntc        reade     tfntc01l
098500010601
098600010601     c                   if        %eof(tfntc01l)
098700010601     c                   leave
098800010601     c                   endif
098900010601      * se annullato oppure è una nota interna leggo la successiva
099000010621     c                   if        ntcflt = 'A' or ntcsns <> 'S'
099100010601     c                   iter
099200010601     c                   endif
099300010601      *
099400010601     c                   add       1             B
099500010601     c                   movel     ntcrnt        note(b)
099600010601      * se b = 4 numero massimo di note da stampare vado a fine
099700010601     c                   if        b = 4
099800010601     c                   leave
099900010601     c                   endif
100000010601
100100010601     c                   enddo
100200010601
100300010601      * ------------ RESPONSABILE TRASPORTI ---------------------------------
100400091112
100500100923     c                   clear                   notke2
100600100923
100700091112      * se trattativa su cliente cerco con il cliente
100800091112     c                   if        $trattativa and visksc > 0
100900091112     c                   eval      notapl = 'C'
101000091112     c                   move      visksc        notke1
101100091112     c                   endif
101200010601
101300010601     c                   movel     '05'          nottnt
101400010601     c     keyntc        setll     tfntc01l
101500010601     c                   do        *hival
101600010601     c     keyntc        reade     tfntc01l
101700010601
101800010601     c                   if        %eof(tfntc01l)
101900010601     c                   leave
102000010601     c                   endif
102100010601      * se annullato oppure è una nota interna leggo la successiva
102200010621     c                   if        ntcflt = 'A' or ntcsns <> 'S'
102300010601     c                   iter
102400010601     c                   endif
102500010601      * se alla c.a. è uguale a blank carico la nota
102600010621     c                   if        resptra = *blanks
102700010601     c                   movel     ntcrnt        resptra
102800010628     C                   SETON                                        73
102900010601      *
103000010601     c                   else
103100010601      * se è già pieno scrivo nel nominativo del e.p.c. ed esco
103200010601     c                   movel     ntcrnt        resptra2
103300010628     C                   SETON                                        77
103400010601     c                   leave
103500010601     c                   endif
103600010601
103700010601     c                   enddo
103800010601
103900010601     c                   endsr
104000010601      *
104100010601     C*------------------------------------------------------------------------
104200010601     C*   SR_DECPO   DECODIFICA I DATI DEL PUNTO OPERATIVO
104300010601     C*------------------------------------------------------------------------
104400010601      *
104500010601     C     SR_DECPO      BEGSR
104600010601      *
104700010601      * Se visita e cliente nuovo recupero la filiale dai primi 3 campi del
104800010601      * codice commerciale
104900010601      *
105000010601     c                   if        *in98 and visksc = 0
105100010601     c                   movel     viscmm        comfil
105200010601     c                   endif
105300010601      * Se visita di un cliente esistente recupero il p.o. appartenenza
105400010601      * cliente
105500010601      *
105600010601     c                   if        *in98 and visksc > 0
105700010601     c                   movel     visksc        comfil
105800010601     c                   endif
105900010601      *
106000010601      * se tariffa recupero dal codice cliente
106100010601      *
106200010601     c                   if        not *in98
106300010601     c                   movel     parsks        comfil
106400010601     c                   endif
106500010601      *
106600010601      * decodifica
106700010601      *
106800010601     c     comfil        chain     azorg01l
106900010601      *
107000010601     C                   if        %found(azorg01l)
107100010601     c                   movel     orgdes        td1fil
107200010601     c                   movel     orgind        td1via
107300010601     c                   movel     orgcpf        td1cap
107400010601     c                   movel     orgloc        td1loc
107500010601     c                   movel     orgpro        td1prv
107600010601     c                   movel     orgtel        td2tel
107700010601     c                   movel     orgfax        td2tfx
107800010601      * verifico se cliente Posta
107900010601     c                   movel     orgde3        og143
108000020530      * cerco il codice regione
108100020530     c                   movel     orgde7        og147
108200020530     c
108300010601      *
108400010601     c                   else
108500010601     c                   clear                   orgcts
108600020531     c                   clear                   og147
108700010601      *
108800010601     c                   endif
108900010601      * costruisco la data stampa
109000010601     c                   movel     i45gds        data_eur
109100010601     c     *eur          movel     data_eur      datastampa
109200060606
109300060606      * imposto nelle varie lingue
109400060606     c                   select
109500060606     c                   when      i45lin = 'I'
109600010601     c                   eval      picdal = %trim(orgdes) + ',li ' + datastampa
109700060606     c                   when      i45lin = '2'
109800060606     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
109900060817     c                   when      i45lin = '3'
110000060817     c                   eval      picdal = %trim(orgdes) + ',le ' + datastampa
110100060817     c                   when      i45lin = '4'
110200060817     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
110300060606     c                   endsl
110400010601      *
110500010601     c                   endsr
110600010601      *
110700010601     C*------------------------------------------------------------------------
110800010531     C*   SR_DECTAM  DECODIFICA DELLA TESTATA TARIFFA
110900010531     C*------------------------------------------------------------------------
111000010531      *
111100010531     C     SR_DECTAM     BEGSR
111200010627      *
111300010627     C                   movel     TAMFLO        DSTA01
111400010627      * se divisa diversa da euro vado a fine PGM
111500010627     c                   if        §tadiv <> 'EUR'
111600010627     c                   goto      fine
111700010627     c                   endif
111800020123      * se tariffa FEDEX  vado a fine PGM
111900020123     c                   if        §tafed =  'S'
112000020123     c                   goto      fine
112100020123     c                   endif
112200010531      *
112300010531      *  Data decorrenza in stampa
112400010531     c     *iso          movel     tamddt        data_EUR
112500010531     C     *eur          movel     DATA_EUR      staddt
112600010531      *  Data scadenza in stampa
112700010531     c     *iso          movel     tamdst        data_EUR
112800010531     C     *eur          movel     DATA_EUR      stadst
112900010531
113000010531      ** verifico se è una tariffa DPD
113100010531      *
113200010531     C                   eval      *in25 = (§tadpd = 'S')
113300010604      ** verifico se è una tariffa EUROEXPRESS
113400010604      *
113500010604     C                   eval      *in02 = (tamfie = 'E')
113600060606
113700060606      * imposto l'intestazione della tariffa
113800060606      * tariffa EEX
113900060606     c                   if        *in02
114000060606     c                   select
114100060606     c                   when      i45lin = 'I'
114200060606     c                   eval      destar = 'TARIFFA EUROEXPRESS'
114300060606     c                   when      i45lin = '2'
114400060630     c                   eval      destar = 'EUROEXPRESS TARIFF'
114500060816     c                   when      i45lin = '3'
114600060816     c                   eval      destar = 'TARIF EUROEXPRESS'
114700060816     c                   when      i45lin = '4'
114800060816     c                   eval      destar = 'PREIS EUROEXPRESS'
114900060606     c                   endsl
115000060606     c                   endif
115100060606      * tariffa DPD
115200060606     c                   if        not *in02 and *in25
115300060606     c                   select
115400060606     c                   when      i45lin = 'I'
115500060606     c                   eval      destar = 'TARIFFA MONOCOLLO D.P.D'
115600060606     c                   when      i45lin = '2'
115700060630     c                   eval      destar = 'DPD SINGLE PARCEL TARIFF'
115800060816     c                   when      i45lin = '3'
115900060816     c                   eval      destar = 'TARIF UN SEUL COLIS D.P.D.'
116000060816     c                   when      i45lin = '4'
116100060816     c                   eval      destar = 'PREIS MONOPACKSTUECK D.P.D.'
116200060606     c                   endsl
116300060606     c                   endif
116400060606
116500010531      * recupero la divisa per decodifica in stampa e per eventuali
116600010531      * conversioni di importi ancora in lire in altri archivi
116700010531      * aggancio la tabella CV
116800010531      *
116900010531     C                   eval      cod= 'CV'
117000010531     C                   movel(p)  §TADIV        KEY
117100060605     c     tabkeyl       chain     tabel00f
117200010531      *
117300010531     c                   if        %found (tabel00f)
117400010531     C                   movel     TBLUNI        DSCV
117500010531     C                   movel     §CVDES        DESDIV
117600010531     c                   endif
117700010601      *
117800010601      * carico i codici tassazione del cliente dal cappario versione data
117900010601      * decorrenza tariffa in caso di stampa dei soli codici tassazione cliente
118000010601      *
118100010601     c                   z-add     tamddt        i96dri
118200010601     c                   call      'TISI96R'
118300010601     c                   parm                    i96dri
118400010601     c                   parm                    o96ver
118500010601      * aggancio tabella codici tassazione con quella del p.o. del cliente
118600010601      *
118700010601     c                   eval      cod = 'CT'
118800010601     c                   if        orgcts <> '  '
118900010601     c                   movel(p)  orgcts        key
119000010601     c                   else
119100010601     c                   movel(p)  *all'9'       key
119200010601     c                   endif
119300010601      *
119400060704     c     tabkeyl       chain     tabel00f
119500010601      *
119600010601     c                   if        %found(tabel00f)
119700010601     c                   movel     tbluni        dsct
119800010604      * recupero dal cappario i codici tassazione
119900010604     c                   z-add     1             y
120000010604     c     kcpc          setll     azcpc03l
120100010604      *
120200010604     c                   do        *hival
120300010604     c     kcpc          reade     azcpc03l
120400010604     c                   if        %eof(azcpc03l)
120500010604     c                   leave
120600010604     c                   endif
120700010604      *
120800010604     c                   if        cpcatb = ' '
120900010604     c     cpccts        lookup    ctl                                    31
121000010604      * carico
121100010604     c                   if        not *in31
121200010604     c                   movel     cpccts        ctl(y)
121300010604     c                   add       1             y
121400010604     c                   endif
121500010604      *
121600010604     c                   endif
121700010604      *
121800010604     c                   enddo
121900010604      *
122000010604     c                   endif
122100010604      *
122200010604      * decodifico la tariffa cliente
122300010604      *
122400010604     c                   movel     i45ctr        unoctr
122500010604     c                   movel(p)  unoctr        key
122600010604     c                   eval      cod = 'TR'
122700010604      *
122800060605     c     tabkeyl       chain     tabel00f
122900010604      *
123000010604     c                   if        %found(tabel00f)
123100010604     c                   movel     tbluni        dstr
123200010604      * valorizzo i campi descrittivi di stampa
123300011010     c                   movel     §trdst        desctr
123400010608     c                   movel     §trde1        desum
123500010604     c                   movel     §trde3        ds3um
123600010928      * se a quintale o a kg metto in  desum KG. e in desctr A PESO
123700010626     c                   if        unoctr = 0 or unoctr = 3
123800010626     c                   eval      desum = '       KG.'
123900010626     c                   endif
124000010604      * salvo il minimo tassabile
124100010604     c                   eval      ctrata  = §trata
124200010604      *
124300010604     c                   endif
124400010801      * tariffa a valore (per stampare scaglioni senza decimali)
124500010604     c                   eval      *in87 = (unoctr = 4)
124600010723      * valorizzo il campo note
124700010723     c                   eval      p07des = tamdcv
124800010928      * se il campo note è uguale a blank verifico se tariffa preferenziale
124900010928      * partenza o arrivo
125000011009     c                   if        p07des = *blanks and tambap =  'P'
125100060606      * in lingua
125200060606     c                   select
125300060606     c                   when      i45lin = 'I'
125400010928     c                   eval      p07des = 'Valida solo per traffico in +
125500010928     c                                       Partenza'
125600060606     c                   when      i45lin = '2'
125700060817     c                   eval      p07des = 'Valid only for Departing +
125800060817     c                                       traffic'
125900060817     c                   when      i45lin = '3'
126000060817     c                   eval      p07des = 'Valable uniquement pour le trafic +
126100060817     c                                       au départ'
126200060817     c                   when      i45lin = '4'
126300060817     c                   eval      p07des = 'Gilt nur für Abgehende Sendungen'
126400060606     c                   endsl
126500010928     c                   endif
126600010928      *
126700011009     c                   if        p07des = *blanks and tambap =  'A'
126800060606      * in lingua
126900060606     c                   select
127000060606     c                   when      i45lin = 'I'
127100010928     c                   eval      p07des = 'Valida solo per traffico in +
127200010928     c                                       Arrivo'
127300060606     c                   when      i45lin = '2'
127400060630     c                   eval      p07des = 'Valid only for Arriving traf+
127500060630     c                                       fic'
127600060817     c                   when      i45lin = '3'
127700060817     c                   eval      p07des = 'Valable uniquement pour le trafic +
127800060817     c                                       à l''arrivée'
127900060817     c                   when      i45lin = '4'
128000060817     c                   eval      p07des = 'Gilt nur für Eingehende Sendungen'
128100060606     c                   endsl
128200010928     c                   endif
128300040430      * TESTO PARTICOLARITÀ GIACENZA
128400040430      * se codice = GP metto 5 nel numero di avvisi prima di rendere la merce
128500040430      * altrimenti lascio 2
128600040430     c                   eval      comnav = 2
128700040430     c                   if        tamgga = 'GP'
128800040430     c                   eval      comnav = 5
128900040430     c                   endif
129000060922      * devo impostare anche la parte finale del numero nelle varie lingue
129100060922     c                   exsr      sr_numgp
129200010928      *
129300010725      * recupero in base alla data decorrenza la versione di prestampato condi-
129400010725      * zioni generali dalla tabella TAA
129500111103     c                   eval      kcod = 'TAA'
129600010725     c     kcod          setll     tntbe01l
129700010725     c                   do        *hival
129800010725     c     kcod          reade     tntbe01l
129900010725      * se fine file vado a fine
130000010725     c                   if        %eof(tntbe01l)
130100010725     c                   leave
130200010725     c                   endif
130300010725      * se annullato leggo il successivo
130400010725     c                   if        tbeatb <> ' '
130500010725     c                   iter
130600010725     c                   endif
130700010725      * mi salvo il modello e l'anno
130800010725     c                   movel     tbeuni        dtaa
130900010725      * mi salvo sempre il primo che ho letto
131000010725     c                   if        stpmod = 0
131100010725     c                   movel     tbeke1        stpmod
131200010725     c                   movel     §dccdad       stpanno
131300010725     c                   endif
131400060302      *?ATTENZIONE  ?
131500060302      * dal 01/03/06 i controlli vanno fatti con la data di stampa e non +
131600060302      * con la data decorrenza tariffa
131700060302      * verifico la data di stampa
131800060302      * se data di stampa maggiore della data di validità del modello leggo
131900060302     c                   if        i45ads >  §dccdad
132000010725     c                   movel     tbeke1        stpmod
132100010725     c                   movel     §dccdad       stpanno
132200010725     c                   iter
132300010725     c                   endif
132400060302      * se data di stampa = salvo a vado a fine
132500060302     c                   if        i45ads =  §dccdad
132600010725     c                   movel     tbeke1        stpmod
132700010725     c                   movel     §dccdad       stpanno
132800010725     c                   leave
132900010725     c                   endif
133000060302      * se data di stampa <  della data validita' vado a fine
133100060302     c                   if        i45ads <  §dccdad
133200010725     c                   leave
133300010725     c                   endif
133400010725      *
133500010725     c                   enddo
133600010604
133700010531     C                   ENDSR
133800020205
133900020205      *------------------------------------------------------------------------
134000020205      *   Sr_Cartarp  CARICO LE TARIFFE PARTICOLARI
134100020205      *------------------------------------------------------------------------
134200020205     C     Sr_Cartarp    begsr
134300020205
134400020205      * carico solo quelle che interessano la tariffa che sto stampando
134500020205     c                   eval      cod ='1P'
134600020205     c                   z-add     0             K
134700060606     c     tabk2l        setll     tabel00f
134800020205 b1  c                   do        *hival
134900060606     c     tabk2l        reade     tabel00f
135000020205      * Fine file
135100020205     c                   if        %eof(tabel00f)
135200020205     c                   leave
135300020205     c                   endif
135400020205      * escludo le annullate
135500020205     c                   if        tblflg <> ' '
135600020205     c                   iter
135700020205     c                   endif
135800080609      * se tariffa particolare c= lasciato avviso
135900080609      * per le filiali non abilitate, accetto cartello
136000080609      * non stampo
136100080609     c                   If        tblkey = 'c       '
136200051012     c                             and comfil <> 888
136300080609     c                   if        %lookup(comfil:sklav) = *zeros
136400080609     c                             and %lookup(999:sklav) = *zeros
136500051012     c                   iter
136600080609     c                   endif
136700051012     c                   endif
136800020205
136900020205     c                   movel     tbluni        ds1p
137000020205
137100020205      * Tariffa con servizio Poste escludo le tariffe particolari con
137200020205      * flag §1PFPT = 'N'
137300020205     c                   if        tamtsp = 'P' and §1pfpt = 'N'
137400020205     c                   iter
137500020205     c                   endif
137600020205      * Tariffa con Network DPD escludo le tariffe particolare con
137700020205      * flag §1PFDP = 'N'
137800020205     c                   if        §tadpd = 'S' and §1pfdp = 'N'
137900020205     c                   iter
138000020205     c                   endif
138100020205      * Per Tariffa FedEx non faccio controlli xchè non vengono lette
138200020205
138300020205      * Tariffa Italia escludo le tariffe particolare con
138400020205      * flag §1PTCO = 'N' e non deve essere servizio poste
138500020205     c                   if        tamfie = 'I' and §1ptco = 'N'
138600020205     c                             and tamtsp <> 'P'
138700020205     c                   iter
138800020205     c                   endif
138900020205      * Tariffa EuroExpress escludo le tariffe particolare con
139000020205      * flag §1PFIE = 'N'
139100020205     c                   if        tamfie = 'E' and §1pfie = 'N'
139200020205     c                   iter
139300020205     c                   endif
139400020614      * Per tariffe di cartello verifico che non sia tipo servizio "P"
139500020614     C                   if        tamfie = ' ' and §1ptco = 'N'
139600020614     c                             and tamtsp <> 'P'
139700020614     c                   iter
139800020614     c                   endif
139900020205
140000020205     c                   add       1             k
140100020205     c                   movel     tblkey        ftc(k)
140200020205     c                   movel     tbluni        dtc(k)
140300020205      * carico l'ordinamento in stampa
140400020205     c                   movel     §1post        ost(k)
140500020205      * carico il tipo stampa standard oppure no
140600020205     c                   movel     §1ptst        tst(k)
140700020205
140800020205 e1  c                   enddo
140900020205      * carico la schiera delle estensioni consegne particolari
141000020205     C                   eval      cod ='SP'
141100020205     C                   z-add     0             K
141200060606     c     tabk2l        setll     tabel00f
141300020205
141400020205 b1  C                   DO        *hival
141500020205      *
141600060606     c     tabk2l        reade     tabel00f
141700020205      *
141800020205     c                   if        %eof(tabel00f)
141900020205     c                   leave
142000020205     c                   endif
142100020205      *
142200020205 b2  C                   IF        tblflg = ' '
142300020205     C                   movel     TBLUNI        DSSP
142400020205      *
142500020205     c                   movel     TBLKEY        comftc
142600020205      *
142700020205      * carico tutte le consegne particolari
142800020205     C                   z-add     1             K
142900020205     c                   setoff                                       69
143000020205     C     comftc        lookup    FTC(K)                                 69
143100020205     c                   if        *in69
143200020205     C                   movel     §spdes        EDTC(K)
143300020205     C                   movel     §spdac        EDAC(K)
143400020205     C                   movel     §spsud        ESUD(K)
143500020205     c                   endif
143600020205      *
143700020205 e2  C                   endif
143800020205      *
143900020205 e1  C                   ENDDO
144000020205      * ordino le tariffe particolari in base all'ordine in stampa
144100020205      *
144200020205     c                   movel     OST           WOS
144300020205     c                   sorta     wos
144400020205 b1  c                   do        50            j
144500020205     c                   z-add     1             jj
144600020205     c                   setoff                                       69
144700020205     c                   if        wos(j) <> '99' and wos(j) <> *blank
144800020205     c     wos(j)        lookup    ost(jj)                                69
144900020205      * carico la schiera delle tariffe particolare in ordine di stampa
145000020205     c                   if        *in69 = *on
145100020205     c                   add       1             jjj
145200020205     c                   move      ftc(jj)       cp(jjj)
145300020205     c                   endif
145400020205     c                   endif
145500020205 e1  c                   enddo
145600020205
145700020205     c                   endsr
145800020205
145900010604     C*------------------------------------------------------------------------
146000010604     C*   SR_SCAGLIONI PREPARAZIONE SCHIERA SCAGLIONI
146100010604     C*------------------------------------------------------------------------
146200010604      *
146300010604     C     SR_SCAGLIONI  BEGSR
146400010604      *
146500010605      * CARICO SCAGLIONI
146600010604      *
146700010604     c                   setoff                                       99
146800010622     c                   z-add     0             k
146900020531     c                   eval      w_oklnp = *off
147000010605      * cerco prima con la linea di partenza del cliente
147100010605      *
147200061122     C  n98DS1KTD        SETLL     TITAD04l
147300061122     c   98ds1ktd        setll     tiofd01l
147400010605      * se non trovato con la linea di partenza del cliente lo segnalo
147500061122     c                   if        not %equal
147600010605     c                   eval      lnpcli = 'NO'
147700010605      * mi riposiziono con chiave generica
147800010604      *
147900061122     C  n98DS1KTM        SETLL     TITAD04l
148000061122     c   98ds1ktm        setll     tiofd01l
148100010605      * se non trovato nulla errore manca dettaglio tariffario
148200010604      *
148300061122     c                   if        not %equal
148400010605     c                   seton                                        99
148500010605     c                   move      msg(4)        errmsg
148600010605     c                   exsr      staerr
148700010605     c                   goto      endscaglia
148800010605     c                   endif
148900010605      *
149000010605     c                   endif
149100010605      *
149200010604     C                   DO        *HIVAL
149300010604      *
149400061122     C  n98DS1KTM        READE     TITAD04l
149500061122     c   98ds1ktm        reade     tiofd01l
149600010604      *
149700061122     C                   if        %eof
149800010606      * se fine lettura e non ho caricato nessuno scaglione (K = 0) e
149900010621      * ho letto solo dettaglio con cap valorizzato (cap1scaglio = 'S')
150000010606      * mi riposiziono con l'ultima chiave per caricare almeno gli scaglioni
150100010621     c                   if        k = 0 and cap1scaglio = 'S'
150200061122     c  n98keytad        setll     titad04l
150300061122     c   98keytad        setll     tiofd01l
150400010606      * valorizzo il flag del non controllo del cap perchè altrimenti non riesco
150500010606      * a caricare gli scaglioni
150600010606     c                   eval      ctrcap = 'N'
150700010606     c                   iter
150800010606      * altrimenti vado a fine ciclo lettura
150900010606     c                   else
151000010604     c                   leave
151100010604     c                   endif
151200010606      *
151300010606     c                   endif
151400010604      * se annullato rileggo
151500010604     c                   if        tadatb = 'A'
151600010604     c                   iter
151700010604     c                   endif
151800020531     c                   eval      w_okstp = *off
151900010604      * se I45DET = 'L' devo stampare solo le righe di dettaglio che hanno la
152000010604      *                 LNP uguale alla LNP del codice cliente
152100020531     c                   if        I45DET = 'L'
152200020531      * se ho già stampato una linea di partenza non stampo più niente
152300020531     c                   if        w_oklnp = *on and rotlnp <> 0 and
152400020531     c                             rotlnp <> tadlnp
152500020531     c                   iter
152600020531     c                   endif
152700020531     c                   exsr      sr_chklnp
152800020531     c                   if        w_okstp = *off
152900010604     c                   iter
153000010604     c                   endif
153100020531     c                   eval      w_oklnp = *on
153200020531     c                   endif
153300010604      * se I45DET = 'A' devo stampare solo le righe di dettaglio che hanno la
153400010604      *                 LNP uguale alla LNP del codice cliente o il codice
153500010604      *                 tassazione tra quelli pevisti dalla linea del codice
153600010604      *                 cliente
153700010604     c                   if        I45DET = 'A'
153800020531      * se ho già stampato una linea di partenza non stampo più niente
153900020531     c                   if        w_oklnp = *on and rotlnp <> 0 and
154000020531     c                             rotlnp <> tadlnp
154100020531     c                   iter
154200020531     c                   endif
154300020531     c                   exsr      sr_chklnp
154400010604     c     tadcts        lookup    ctl                                    35
154500020531     c                   if        not *in35 and w_okstp = *off
154600010604     c                   iter
154700010604     c                   endif
154800020531     c                   eval      w_oklnp = *on
154900010604      *
155000010604     c                   endif
155100010604      *
155200010604      * salvataggio dei campi per le rotture regione/provincia
155300010604      *
155400010605     c                   if        rotlnp = *zeros
155500010606      * se controllo del cap lo devo fare (ctrcap = ' ')
155600010606      * se cap valorizzato nel 1° scaglione lo segnalo per caricare schiera
155700010606      * inoltro quando non c'è cap  e ritorno a leggere
155800010621     c                   if        tadcap <>  *blanks  and ctrcap = ' '
155900010606     c                   eval      cap1scaglio = 'S'
156000010606     c                   iter
156100010606     c                   endif
156200010604     c                   eval      rotlnp = tadlnp
156300010604     c                   eval      rotcts = tadcts
156400010604     c                   eval      rotcap = tadcap
156500010604      *
156600010604      * controlli di rottura per regione e pulizie schiere
156700010605      *
156800010605     c                   exsr      regdet
156900010605     c   99              goto      endscaglia
157000010605     c                   exsr      prodet
157100010605     c   99              goto      endscaglia
157200010605      *
157300010605     c                   endif
157400010605      *
157500010605      * carico gli scaglioni per lnp cts e cap uguali
157600010605      *
157700010605     c                   if        tadlnp = rotlnp and tadcts = rotcts and
157800010605     c                             tadcap = rotcap
157900010605     c                   add       1             k
158000010605      *
158100010605     c                   if        k > 18
158200050914      * accendo l'indicatore di errore così esce dal pgm
158300050914     c                   seton                                        99
158400010605     c                   move      msg(5)        errmsg
158500010605     c                   exsr      staerr
158600050930     c                   goto      endscaglia
158700010605     c                   endif
158800010605      *
158900010605     c                   z-add     tadsgl        sgl(k)
159000010606      *
159100010801      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
159200010801      * a valore
159300010801     c                   if        not *in87
159400010801      *
159500010606     c                   move      tadsgl        w0030
159600010606     c                   z-add     tadsgl        w0050
159700010606      * se scaglione 99999 non controllo i decimali
159800010606     c                   if        w0050 <> 99999 and w0030 > 0
159900010606     c                   seton                                        91
160000010606     c                   endif
160100010606      * se con decimali verifico se valorizzato dal secondo in poi
160200010606     c                   if        *in91
160300010606     c                   z-add     w0030         w0020
160400010606     c                   if        w0020 > 0
160500010606     c                   seton                                        37
160600010606     c                   endif
160700010606     c                   endif
160800010801      *
160900010801     c                   endif
161000010606      * se non è valorizzato il CAP carico la schiera dell'inoltro
161100010606     c                   if        tadcap = *blanks
161200010606     c                   z-add     tadino        pro(k)
161300010606      * pulisco il flag che mi indica che sto caricando la schiera degli inoltri
161400010606     c                   clear                   cap1scaglio
161500010606     c                   endif
161600010606      * valorizzo la schiera del rapporto peso volume
161700010622     c     TADRPV        MULT      100           rpv(k)
161800010606      *
161900010606     c                   else
162000010606      * rottura di chiave
162100010606     c                   leave
162200010606      *
162300010606     c                   endif
162400010606      *
162500010606     c                   enddo
162600010606      *
162700010606      * se non ho caricato nessuno scaglione do errore di mancanza di dettaglio
162800010606      *
162900010606     c                   if        K = 0
163000010606     c                   seton                                        99
163100010606     c                   move      msg(4)        errmsg
163200010606     c                   exsr      staerr
163300010606     c                   goto      endscaglia
163400010606     c                   endif
163500051006      * memorizzo quanti scaglioni ho trovato
163600051006     c                   Eval      wnrscaglio = k
163700080117      * prima di stampare controllo se gli scaglioni sono stampabili
163800080117      * in stampa il campo è lungo 4
163900080117     c                   eval      xy = 1
164000080117     c                   for       xy by 1 to wnrscaglio
164100080117     c                   z-add     sgl(xy)       w0050
164200080117      * scaglioni senza decimali al massimo 9999
164300080117     c                   if        not *in91 and w0050 > 9999
164400080118     c                             and w0050 <> 99999
164500080117     c                   eval      *in99 = *on
164600080117     c                   eval      errmsg = 'Scaglione ' +
164700080117     c                             %editc(sgl(xy):'4') +
164800080117     c                             ' non stampabile'
164900080117     c                   exsr      staerr
165000080117     c                   leavesr
165100080117     c                   endif
165200080218      * scaglioni con 1 decimale al massimo 999,9
165300080218     c                   if        *in91 and not *in37 and w0050 > 999,9
165400080118     c                             and w0050 <> 99999
165500080117     c                   eval      *in99 = *on
165600080117     c                   eval      errmsg = 'Scaglione ' +
165700080117     c                             %editc(sgl(xy):'4') +
165800080117     c                             ' non stampabile'
165900080117     c                   exsr      staerr
166000080117     c                   leavesr
166100080117     c                   endif
166200080218      * scaglioni con 2 decimale al massimo 99,99
166300080218     c                   if        *in91 and *in37 and w0050 > 99,99
166400080118     c                             and w0050 <> 99999
166500080117     c                   eval      *in99 = *on
166600080117     c                   eval      errmsg = 'Scaglione ' +
166700080117     c                             %editc(sgl(xy):'4') +
166800080117     c                             ' non stampabile'
166900080117     c                   exsr      staerr
167000080117     c                   leavesr
167100080117     c                   endif
167200080117     c                   endfor
167300010606      *
167400010606      * preparo la schiera scaglioni da stampare
167500010606      *
167600010606     c                   exsr      sr_stascaglia
167700010606      *
167800010606     c     endscaglia    endsr
167900010606      *
168000010606     C*-------------------------------------------------------------------------
168100010606     C*  SR_STASCAGLIA PREPARO LA SCHIERA DEGLI SCAGLIONI DA STAMPARE
168200010606     C*-------------------------------------------------------------------------
168300010606     C     SR_STASCAGLIA BEGSR
168400010606      *
168500010606     c                   z-add     1             k
168600010606     c                   clear                   k1
168700010606      *
168800010606     c                   dow       sgl(k) > 0
168900010606      * preparo lo scaglione DAL addizionato di 1 o 0,1 o 0,01 in base ai decima
169000010606      * li
169100010606     c                   z-add     sgl(k)        w0050
169200010606      *
169300010606      * verifico se scaglione 99999
169400010606     c                   if        w0050 <> 99999
169500010606      *
169600010606     c                   if        k1 > 0
169700010606      * se non ha decimali
169800010606     c                   if        not *in91
169900010622     C                   eval      sgld(k) = sgl(k1) + 1
170000010606     c                   endif
170100010606      *
170200010606     c                   if        *in91 and not *in37
170300010606      * se ha 1 decimale
170400010622     C                   eval      sg1d(k) = sgl(k1) + 0,1
170500010606     c                   endif
170600010606      *
170700010606     c                   if        *in91 and *in37
170800010606      * se ha + di 1 decimale
170900010622     C                   eval      sg2d(k) = sgl(k1) + 0,01
171000010606     c                   endif
171100010606      *
171200010606     c                   endif
171300010606      *
171400010606      * costruisco la schiera dell'applicazione tariffa finita che va sotto gli
171500010606      * scaglioni da/a in base al valore della tariffa finita che è in testata
171600010606      * tariffa e in mancanza di quest'ultima prendo dalla tabella TR
171700010606      *
171800010606      * muovo unità di misura della tariffa nella schiera
171900060922      * devo fare tutto fisso a pgm perchè in tabella il campo è lungo 10
172000060922      * mentre in stampa è lungo 7
172100060922      * quindi devo personalizzarlo per ogni lingua
172200060925     c                   move      unoctr        wctr1
172300060922     c                   exsr      sr_atfk
172400010606      * faccio i vari ragionamenti in base al valore della tariffa finita
172500010606      *
172600010606      * se scaglioni senza decimali
172700011218     c                   if        not *in91
172800010606
172900011218     c                   z-add     tamata        wata
173000010606      * se in tariffa non è definita la recupero dalla tabella TR se maggiore 1
173100011218     c                   if        wata = 0 and ctrata > 1
173200011218     c                   z-add     ctrata        wata
173300011218     c                   endif
173400011218      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
173500011218      * scrivo "A SPED."
173600011218     c                   if        (wata > 0 and sgl(k) <= wata)
173700060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
173800060925     c                   clear                   wctr1
173900060922     c                   exsr      sr_atfk
174000011218     c                   endif
174100010606      *
174200011218     c                   else
174300010606      * scaglioni con decimali
174400011218     c                   z-add     tamata        wata1
174500011218     c                   z-add     tamata        wata2
174600010606      *
174700010606      * se in tariffa non è definita la recupero dalla tabella TR se maggiore 1
174800011218     c                   if        wata1 = 0 and ctrata > 1 and not *in37
174900011218     c                   z-add     ctrata        wata1
175000011218     c                   endif
175100010606      * se in tariffa non è definita la recupero dalla tabella TR se maggiore 1
175200011218     c                   if        wata2 = 0 and ctrata > 1 and *in37
175300011218     c                   z-add     ctrata        wata2
175400011218     c                   endif
175500010606      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
175600010606      * scrivo "A SPED."  1 decimale
175700011218     c                   if        (wata1 > 0 and sgl(k)<= wata1 and not *in37)
175800060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
175900060925     c                   clear                   wctr1
176000060922     c                   exsr      sr_atfk
176100011218     c                   endif
176200010606      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
176300010606      * scrivo "A SPED."  2 decimali
176400011218     c                   if        (wata2 > 0 and sgl(k) <= wata2 and *in37)
176500060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
176600060925     c                   clear                   wctr1
176700060922     c                   exsr      sr_atfk
176800011218     c                   endif
176900010606      *                                                      *in91 on
177000011218     c                   endif
177100011123
177200010606      *
177300010606     c                   move      '/'           sglb(k)
177400010606      * preparo lo scaglione AL
177500010606      *
177600010606      * no decimali
177700010606     c                   if        not *in91
177800010606     c                   z-add     sgl(k)        sgla(k)
177900010606     c                   endif
178000010606      * 1 decimale
178100010606     c                   if        *in91 and not *in37
178200010606     c                   z-add     sgl(k)        sg1a(k)
178300010606     c                   endif
178400010606      * 2 decimali
178500010606     c                   if        *in91 and *in37
178600010606     c                   z-add     sgl(k)        sg2a(k)
178700010606     c                   endif
178800010606      *
178900010606     c                   else
179000010606      * scaglione 99999 è sempre maggiore della tariffa finita
179100060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
179200060922      * la decodifica del tipo tariffa
179300060925     c                   move      unoctr        wctr1
179400060922     c                   exsr      sr_atfk
179500010606      *
179600010606     c                   endif
179700010606      *
179800010606     c                   add       1             k
179900010606     c                   add       1             k1
180000010606      *
180100010606      * se maggiore di 18 esco
180200010606     c                   if        k > 18
180300010606     c                   leave
180400010606     c                   end
180500010606      *
180600010606     c                   enddo
180700010606      *
180800010606      * se c'è solo uno scaglione non scrivo niente sotto
180900021220      * solo se lo scaglione è 99999
181000010606      *
181100021220     c                   if        k1 <= 1 and sgl(k1) >= 99999
181200010606     c                   clear                   atf(1)
181300010606     c                   endif
181400010606      *
181500010606     C                   ENDSR
181600010606     C*-------------------------------------------------------------------------
181700010606     C*  SR_STINO   CONTROLLO SE DEVO STAMPARE INOLTRO A FINE DETTAGLIO
181800010606     C*-------------------------------------------------------------------------
181900010606     C     SR_STINO      BEGSR
182000020531
182100020531     c                   eval      w_oklnp = *off
182200010606      *
182300010606      * leggo tutto il titad e verifico per ogni scaglione valido se
182400010606      * se l'inoltro è uguale
182500010606      *
182600061122     c  n98ds1ktm        setll     titad04l
182700061122     c   98ds1ktm        setll     tiofd01l
182800010606      *
182900010606     c                   do        *hival
183000061122     c  n98ds1ktm        reade     titad04l
183100061122     c   98ds1ktm        reade     tiofd01l
183200010606      *
183300061122     c                   if        %eof
183400010606     c                   leave
183500010606     c                   endif
183600010606      * se annullato rileggo
183700010606     c                   if        tadatb = 'A'
183800010606     c                   iter
183900010606     c                   endif
184000020531     c                   eval      w_okstp = *off
184100010606      * se I45DET = 'L' devo leggere  solo le righe di dettaglio che hanno la
184200010606      *                 LNP uguale alla LNP del codice cliente
184300020531     c                   if        I45DET = 'L'
184400020531      * se ho già stampato una linea di partenza non stampo più niente
184500020531     c                   if        w_oklnp = *on and rotlnp <> 0 and
184600020531     c                             rotlnp <> tadlnp
184700020531     c                   iter
184800020531     c                   endif
184900020531     c                   exsr      sr_chklnp
185000020531     c                   if        w_okstp = *off
185100010606     c                   iter
185200010606     c                   endif
185300020531     c                   eval      w_oklnp = *on
185400020531     c                   endif
185500010606      * se I45DET = 'A' devo leggere  solo le righe di dettaglio che hanno la
185600010606      *                 LNP uguale alla LNP del codice cliente o il codice
185700010606      *                 tassazione tra quelli pevisti dalla linea del codice
185800010606      *                 cliente
185900010606     c                   if        I45DET = 'A'
186000020531      * se ho già stampato una linea di partenza non stampo più niente
186100020531     c                   if        w_oklnp = *on and rotlnp <> 0 and
186200020531     c                             rotlnp <> tadlnp
186300020531     c                   iter
186400020531     c                   endif
186500020531     c                   exsr      sr_chklnp
186600010606     c     tadcts        lookup    ctl                                    35
186700020531     c                   if        not *in35 and w_okstp = *off
186800010606     c                   iter
186900010606     c                   endif
187000020531     c                   eval      w_oklnp = *on
187100010606      *
187200010606     c                   endif
187300010606      * decodifico la linea di partenza se non ancora fatto
187400010606      *
187500010606     c                   if        stalnp = *blanks
187600020531     c     tadlnp        chain     azorg01l
187700010606      *
187800020531     c                   if        %found(azorg01l)
187900020531     c                   movel     orgdes        stalnp
188000020624      * controllo se linea di partenza 9xx
188100020624     c                   if        orgfl2 = 'S'
188200020624      * italia
188300020624     c                   if        tadlnp = 950
188400060606      * in lingua
188500060606     c                   select
188600060606     c                   when      i45lin = 'I'
188700070312     c                   eval      prtlnp =
188800070313     c                             'Merce ritirata da tutte le ns. filiali'
188900020624     c                             + '  ' + stalnp
189000060606     c                   when      i45lin = '2'
189100060630     c                   eval      prtlnp = 'Goods collected from all our +
189200070313     c                              branches' + '  ' + stalnp
189300060817     c                   when      i45lin = '3'
189400070314     c                   eval      prtlnp = 'Marchandise retirée par toutes +
189500070314     c                              nos filiales' + '  ' + stalnp
189600060817     c                   when      i45lin = '4'
189700070313     c                   eval      prtlnp = 'Abholung in allen unseren Filialen'
189800060817     c                             + '  ' + stalnp
189900060606     c                   endsl
190000020624     c                   else
190100020624      * regione
190200060606      * in lingua
190300060606     c                   select
190400060606     c                   when      i45lin = 'I'
190500070312     c                   eval      prtlnp = 'Merce ritirata dalle ns. filiali +
190600070312     c                              della regione' + '  ' + stalnp
190700060606     c                   when      i45lin = '2'
190800070314     c                   eval      prtlnp = 'Goods collected from our branches +
190900060630     c                              in the region' + '  ' + stalnp
191000060817     c                   when      i45lin = '3'
191100070314     c                   eval      prtlnp = 'Marchandise retirée par nos +
191200070314     c                              filiales de la région' + '  ' + stalnp
191300060817     c                   when      i45lin = '4'
191400070314     c                   eval      prtlnp = 'Abholung in unseren Filialen der +
191500060817     c                              Region' + '  ' + stalnp
191600060606     c                   endsl
191700020624     c                   endif
191800020624     c                   else
191900020624      * lnp non è 9xx
192000060606      * in lingua
192100060606     c                   select
192200060606     c                   when      i45lin = 'I'
192300070312     c                   eval      prtlnp =
192400070312     c                             'Merce ritirata dalla ns. filiale di'
192500070312     c                             + '  ' + stalnp
192600060606     c                   when      i45lin = '2'
192700070314     c                   eval      prtlnp = 'Goods collected from our branch +
192800060630     c                              in' + '  ' + stalnp
192900060817     c                   when      i45lin = '3'
193000060817     c                   eval      prtlnp = 'Marchandise retirée par notre +
193100070314     c                              filiale de' + '  ' + stalnp
193200060817     c                   when      i45lin = '4'
193300070314     c                   eval      prtlnp = 'Abholung in unseren Filialen in'
193400060817     c                             + '  ' + stalnp
193500060606     c                   endsl
193600020624     c                   endif
193700020624
193800020531     c                   endif
193900010613      * salvo i campi per eventuali rotture utili in fulcro
194000010613     c                   eval      rotlnp = tadlnp
194100010613     c                   eval      rotcts = tadcts
194200010613     c                   eval      rotcap = tadcap
194300010613     c                   eval      rotpro = %subst(tadorp:5:1)
194400010613     c                   eval      rotreg = %subst(tadorp:3:2)
194500010618      * Salvo gli stessi campi di lavoro per letture in fulcro
194600010618     c                   eval      wrklnp = rotlnp
194700010618     c                   eval      wrkslp = stalnp
194800020703     c                   eval      wrkprtlnp = prtlnp
194900010618     c                   eval      wrkcts = rotcts
195000010618     c                   eval      wrkcap = rotcap
195100010618     c                   eval      wrkpro = rotpro
195200010618     c                   eval      wrkreg = rotreg
195300020206      * rotture per regione e provincia
195400020214     c                   exsr      regdet
195500020206      * per 99 errori vado a fine
195600020214     c   99              leave
195700020214     c                   exsr      prodet
195800020206      * per 99 errori vado a fine
195900020214     c   99              leave
196000020206      *
196100010606     c                   endif
196200010606      *
196300010606      * se cap valorizzato leggo il record successivo
196400010621     c                   if        tadcap <> *blanks
196500010606     c                   iter
196600010606     c                   endif
196700010606      * verifico se scaglione presente
196800010606      *
196900010606     c                   z-add     1             z
197000010613     c                   z-add     tadsgl        wsgl72
197100010613     c     Wsgl72        lookup    sgl(z)                                 35
197200010606     c                   if        not *in35
197300010606     c                   iter
197400010606     c                   endif
197500010606      * controllo l'inoltro nella schiera
197600010606      *
197700010606     c                   if        tadino <> pro(z)
197800010606     c                   eval      stino = 'Y'
197900010606     c                   leave
198000010606     c                   endif
198100010606      *
198200010606     c                   enddo
198300010606      *
198400010606     c                   endsr
198500010606     C*-------------------------------------------------------------------------
198600010607     C*  SR_ELADET  ELABORO E STAMPO IL DETTAGLIO TARIFFARIO
198700010606     C*-------------------------------------------------------------------------
198800010607     C     SR_ELADET     BEGSR
198900010606      *
199000010607      * elaboro 6 scaglioni alla volta
199100010607     C                   Z-ADD     1             K4
199200010607     C                   DOW       K4 <= 18
199300010607     C                   MOVEA     SGLD(K4)      SSLD
199400010607     C                   MOVEA     SGLB(K4)      SSLB
199500010607     C                   MOVEA     SGLA(K4)      SSLA
199600010607      * 1 DECIMALE
199700010607     C                   IF        *IN91 AND NOT *IN37
199800010607     C                   MOVEA     SG1A(K4)      SS1A
199900010607     C                   MOVEA     SG1D(K4)      SS1D
200000010607     C                   ENDIF
200100010607      * 2 DECIMALI
200200010607     C                   IF        *IN91 AND *IN37
200300010607     C                   MOVEA     SG2A(K4)      SS2A
200400010607     C                   MOVEA     SG2D(K4)      SS2D
200500010607     C                   ENDIF
200600010607      *
200700010607      * CARICO GLI SCAGLIONI PER ELABORARE IL DETTAGLIO
200800010607      *
200900010607     C                   MOVEA     SGL(K4)       SSL
201000010607     C*  INTESTAZIONE SCAGLIONI
201100010607     C                   Z-ADD     SSL(1)        W0050             5 0
201200021220     C     W0050         COMP      99999                                  41
201300010725     C     SSL(1)        COMP      *ZEROS                                 51
201400010607     C                   Z-ADD     SSL(2)        W0050             5 0
201500010607     C     W0050         COMP      99999                                  42
201600010725     C     SSL(2)        COMP      *ZEROS                                 52
201700010607     C                   Z-ADD     SSL(3)        W0050             5 0
201800010607     C     W0050         COMP      99999                                  43
201900010725     C     SSL(3)        COMP      *ZEROS                                 53
202000010607     C                   Z-ADD     SSL(4)        W0050             5 0
202100010607     C     W0050         COMP      99999                                  44
202200010725     C     SSL(4)        COMP      *ZEROS                                 54
202300010607     C                   Z-ADD     SSL(5)        W0050             5 0
202400010607     C     W0050         COMP      99999                                  45
202500010725     C     SSL(5)        COMP      *ZEROS                                 55
202600010607     C                   Z-ADD     SSL(6)        W0050             5 0
202700010607     C     W0050         COMP      99999                                  46
202800010725     C     SSL(6)        COMP      *ZEROS                                 56
202900010607     C*
203000010607     C* SE IL 41 E' ACCESO PERCHÈ IL PRIMO SCAGLIONE E' UGUALE A TUTTI 999999
203100010627     C* O IL SECONDO SCAGLIONE E' UGUALE A ZERO
203200010627     C* MA SE NON SI TRATTA + DEL 1° SCAGLIONE IN ASSOLUTO DEVO STAMPARE "OLTRE
203300010607     C* LO SCAGLIONE PRECEDENTE
203400010607     C* QUINDI ACCENDO IL 40 E SPENGO IL 41
203500010607     C*
203600010607     C                   IF        *IN41 AND K4 <> 1
203700010607     C                   SETOFF                                       41
203800010607     C                   SETON                                        40
203900010607     C                   ENDIF
204000061207      * indicatore e campo di comodo per tariffa Fuel a scaglioni
204100061207      * sto stampando il dettaglio tariffario quindi pulisco il campo e
204200061207      * spengo l'indicatore
204300061207     c                   clear                   desumf
204400061207     c                   eval      *in12 = *off
204500010607     C*****
204600010607     C*  STAMPA INTESTAZIONE INIZIALE
204700010607     C*****
204800010608      * preparo la testata del dettaglio tariffario
204900010608     c                   exsr      SR_STATESDET
205000010608      *
205100010607     C*****
205200010613     C*  STAMPA DETTAGLIO TARIFFARIO
205300010613     C*****
205400010613     C                   EXSR      SR_FULCRO
205500010607     C* 99 indicatore di errore risultante dalla routine FULCRO
205600010614     C   99              GOTO      ENDDET
205700010615      * vedo se devo stampare l'inoltro
205800010615     c                   EXSR      SR_INOLTR
205900010615      * Rapporto peso volume SOLO SE NON È DPD
206000010615     c  N25              EXSR      SR_STARAP
206100010618      *
206200010618      * rielaboro altri 6 scaglioni
206300010618      *
206400010618     c                   add       6             k4
206500010618      * se scaglioni minori o uguali a 18 e scaglione valorizzato
206600010618     c                   if        k4 <= 18 and
206700010618     c                             sgl(k4) <> *zeros
206800010618      * pulisco schiere di comodo per la stampa degli scaglioni
206900010618     c                   clear                   ssld
207000010618     c                   clear                   sslb
207100010618     c                   clear                   ssla
207200010618     c                   clear                   ss1d
207300010618     c                   clear                   ss1a
207400010618     c                   clear                   ss2d
207500010618     c                   clear                   ss2a
207600010618     c                   clear                   ssl
207700010618      * spengo indicatori che condizionano la stampa
207800010618     c                   setoff                                       404142
207900010618     c                   setoff                                       434445
208000010618     c                   setoff                                       46
208100010725     c                   setoff                                       515253
208200010725     c                   setoff                                       545556
208300010618      * Recupero i campi di lettura salvati la prima volta
208400010618      *
208500010618     c                   eval      rotlnp = wrklnp
208600010618     c                   eval      stalnp = wrkslp
208700020703     c                   eval      prtlnp = wrkprtlnp
208800010618     c                   eval      rotcts = wrkcts
208900010618     c                   eval      rotreg = wrkreg
209000010618     c                   eval      rotpro = wrkpro
209100010618     c                   eval      rotcap = wrkcap
209200010618      *
209300010618     c                   if        not *in10
209400010621     c                   eval      tadcts = wrkcts
209500010618     c                   move      rotreg        com04
209600010618     c                   movel     com04         tadorp
209700010618     c                   move      rotpro        tadorp
209800010618     c                   endif
209900010618      *
210000010618     c                   exsr      regdet
210100010618     c   99              goto      enddet
210200010618      *
210300010618     c                   exsr      prodet
210400010618     c   99              goto      enddet
210500010618      *
210600010618     c                   else
210700010618      * non ci sono + scaglioni da stampare
210800010618     c                   eval      k4=19
210900010618     c                   leave
211000010618      *
211100010618     c                   endif
211200010618      *
211300010618     c                   enddo
211400010614      *
211500010614     c     ENDDET        ENDSR
211600010614      *
211700010608     C*-------------------------------------------------------------------------
211800010608     C*  SR_STATESDET PREPARO E STAMPO TESTATA DETTAGLIO TARIFFARIO
211900010608     C*-------------------------------------------------------------------------
212000010608     C     SR_STATESDET  BEGSR
212100010725      *
212200010725     c                   setoff                                       79
212300010608      *
212400010608      * Se scaglioni senza decimali
212500010608      *
212600010608     c                   if        not *in91
212700010622     c                   eval      dissca = ' '+%editc(ssld(1):'3') + sslb(1) +
212800010801     c                                      %editc(ssla(1):'Z') + '    ' +
212900010608     c                                      %editc(ssld(2):'3') + sslb(2) +
213000010801     c                                      %editc(ssla(2):'Z') + '    ' +
213100010608     c                                      %editc(ssld(3):'3') + sslb(3) +
213200010801     c                                      %editc(ssla(3):'Z') + '    ' +
213300010608     c                                      %editc(ssld(4):'3') + sslb(4) +
213400010801     c                                      %editc(ssla(4):'Z') + '    ' +
213500010608     c                                      %editc(ssld(5):'3') + sslb(5) +
213600010801     c                                      %editc(ssla(5):'Z') + '    ' +
213700010608     c                                      %editc(ssld(6):'3') + sslb(6) +
213800010608     c                                      %editc(ssla(6):'Z')
213900010608     c                   endif
214000010608      *
214100010608      * Se scaglioni con 1 decimale
214200010608      *
214300080901     c                   if        *in91 and not *in37 and not *in38
214400010626     c                   eval      dissca = '   '+%editc(ss1d(1):'4') +sslb(1)+
214500010801     c                                      %editc(ss1a(1):'4') + '    ' +
214600010608     c                                      %editc(ss1d(2):'4') + sslb(2) +
214700010801     c                                      %editc(ss1a(2):'4') + '    ' +
214800010608     c                                      %editc(ss1d(3):'4') + sslb(3) +
214900010801     c                                      %editc(ss1a(3):'4') + '    ' +
215000010608     c                                      %editc(ss1d(4):'4') + sslb(4) +
215100010801     c                                      %editc(ss1a(4):'4') + '    ' +
215200010608     c                                      %editc(ss1d(5):'4') + sslb(5) +
215300010801     c                                      %editc(ss1a(5):'4') + '    ' +
215400010608     c                                      %editc(ss1d(6):'4') + sslb(6) +
215500010608     c                                      %editc(ss1a(6):'4')
215600010608     c                   endif
215700010608      *
215800010608      * Se scaglioni con 2 decimali
215900010608      *
216000080901     c                   if        *in91  and  *in37 and not *in38
216100010622     c                   eval      dissca = ' '+%editc(ss2d(1):'4') + sslb(1) +
216200010801     c                                      %editc(ss2a(1):'4') + '    ' +
216300010608     c                                      %editc(ss2d(2):'4') + sslb(2) +
216400010801     c                                      %editc(ss2a(2):'4') + '   ' +
216500010608     c                                      %editc(ss2d(3):'4') + sslb(3) +
216600010801     c                                      %editc(ss2a(3):'4') + '   ' +
216700010608     c                                      %editc(ss2d(4):'4') + sslb(4) +
216800010801     c                                      %editc(ss2a(4):'4') + '   ' +
216900010608     c                                      %editc(ss2d(5):'4') + sslb(5) +
217000010801     c                                      %editc(ss2a(5):'4') + '   ' +
217100010608     c                                      %editc(ss2d(6):'4') + sslb(6) +
217200010608     c                                      %editc(ss2a(6):'4')
217300010608     c                   endif
217400080901      *
217500080901      * Se scaglioni con 3 decimali
217600080901      *
217700080901     c                   if        *in91  and  *in37 and  *in38
217800080901     c                   eval      dissca = ' '+%editc(ss3d(1):'4') + sslb(1) +
217900080901     c                                      %editc(ss3a(1):'4') + '   ' +
218000080901     c                                      %editc(ss3d(2):'4') + sslb(2) +
218100080901     c                                      %editc(ss3a(2):'4') + '  ' +
218200080901     c                                      %editc(ss3d(3):'4') + sslb(3) +
218300080901     c                                      %editc(ss3a(3):'4') + '  ' +
218400080901     c                                      %editc(ss3d(4):'4') + sslb(4) +
218500080901     c                                      %editc(ss3a(4):'4') + '  ' +
218600080901     c                                      %editc(ss3d(5):'4') + sslb(5) +
218700080901     c                                      %editc(ss3a(5):'4') + '  ' +
218800080901     c                                      %editc(ss3d(6):'4') + sslb(6) +
218900080901     c                                      %editc(ss3a(6):'4')
219000080901     c                   endif
219100010608      * se 40 acceso il 1° scaglione è = 99999 dopo almeno 6 scaglioni
219200010608     c                   if        *in40
219300060606      * in lingua
219400060606     c                   select
219500060606     c                   when      i45lin = 'I'
219600010608     c                   eval      dissca = '    OLTRE '
219700060606     c                   when      i45lin = '2'
219800060606     c                   eval      dissca = '    OVER  '
219900060816     c                   when      i45lin = '3'
220000060817     c                   eval      dissca = '    PLUS  '
220100060816     c                   when      i45lin = '4'
220200060818     c                   eval      dissca = '  MEHR ALS'
220300060606     c                   endsl
220400010608     c                   endif
220500010608      * se 41 acceso il 1° scaglione è = 99999
220600010608     c                   if        *in41
220700010608     c                   clear                   desum
220800010608     c                   eval      dissca = ds3um
220900010626     c                   seton                                        79
221000010608     c                   endif
221100010608      * se 42 acceso il 2° scaglione è = 99999
221200010608     c                   if        *in42
221300010608     c                   clear                   dissca2
221400060606      * in lingua
221500060606     c                   select
221600060606     c                   when      i45lin = 'I'
221700010622     c                   eval      dissca2 = 'OLTRE  '
221800060606     c                   when      i45lin = '2'
221900060606     c                   eval      dissca2 = 'OVER   '
222000060817     c                   when      i45lin = '3'
222100060817     c                   eval      dissca2 = 'PLUS   '
222200060816     c                   when      i45lin = '4'
222300060817     c                   eval      dissca2 = 'MEHR ALS '
222400060606     c                   endsl
222500010622     c                   move      dissca2       dissca
222600010608     c                   endif
222700010725      * se 52 acceso il 2° scaglione è = 00000
222800010725     c                   if        *in52
222900010725     c                   clear                   dissca2
223000010725     c                   move      dissca2       dissca
223100010725     c                   endif
223200010608      * se 43 acceso il 3° scaglione è = 99999
223300010608     c                   if        *in43
223400010608     c                   clear                   dissca3
223500060606      * in lingua
223600060606     c                   select
223700060606     c                   when      i45lin = 'I'
223800010801     c                   eval      dissca3 = 'OLTRE  '
223900060606     c                   when      i45lin = '2'
224000060606     c                   eval      dissca3 = 'OVER   '
224100060817     c                   when      i45lin = '3'
224200060817     c                   eval      dissca3 = 'PLUS   '
224300060816     c                   when      i45lin = '4'
224400060817     c                   eval      dissca3 = 'MEHR ALS '
224500060606     c                   endsl
224600010622     c                   move      dissca3       dissca
224700010608     c                   endif
224800010725      * se 53 acceso il 3° scaglione è = 00000
224900010725     c                   if        *in53
225000010725     c                   clear                   dissca3
225100010725     c                   move      dissca3       dissca
225200010725     c                   endif
225300010608      * se 44 acceso il 4° scaglione è = 99999
225400010608     c                   if        *in44
225500010608     c                   clear                   dissca4
225600060606      * in lingua
225700060606     c                   select
225800060606     c                   when      i45lin = 'I'
225900010801     c                   eval      dissca4 = 'OLTRE  '
226000060606     c                   when      i45lin = '2'
226100060606     c                   eval      dissca4 = 'OVER   '
226200060817     c                   when      i45lin = '3'
226300060817     c                   eval      dissca4 = 'PLUS   '
226400060816     c                   when      i45lin = '4'
226500060817     c                   eval      dissca4 = 'MEHR ALS '
226600060606     c                   endsl
226700010622     c                   move      dissca4       dissca
226800010608     c                   endif
226900010725      * se 54 acceso il 4° scaglione è = 00000
227000010725     c                   if        *in54
227100010725     c                   clear                   dissca4
227200010725     c                   move      dissca4       dissca
227300010725     c                   endif
227400010608      * se 45 acceso il 5° scaglione è = 99999
227500010608     c                   if        *in45
227600010608     c                   clear                   dissca5
227700060606      * in lingua
227800060606     c                   select
227900060606     c                   when      i45lin = 'I'
228000010801     c                   eval      dissca5 = 'OLTRE  '
228100060606     c                   when      i45lin = '2'
228200060606     c                   eval      dissca5 = 'OVER   '
228300060817     c                   when      i45lin = '3'
228400060817     c                   eval      dissca5 = 'PLUS   '
228500060816     c                   when      i45lin = '4'
228600060817     c                   eval      dissca5 = 'MEHR ALS '
228700060606     c                   endsl
228800010622     c                   move      dissca5       dissca
228900010608     c                   endif
229000010725      * se 55 acceso il 5° scaglione è = 00000
229100010725     c                   if        *in55
229200010725     c                   clear                   dissca5
229300010725     c                   move      dissca5       dissca
229400010725     c                   endif
229500010608      * se 46 acceso il 6° scaglione è = 99999
229600010608     c                   if        *in46
229700010608     c                   clear                   dissca6
229800060606      * in lingua
229900060606     c                   select
230000060606     c                   when      i45lin = 'I'
230100010725     c                   eval      dissca6 = 'OLTRE '
230200060606     c                   when      i45lin = '2'
230300060606     c                   eval      dissca6 = 'OVER   '
230400060817     c                   when      i45lin = '3'
230500060817     c                   eval      dissca6 = 'PLUS   '
230600060816     c                   when      i45lin = '4'
230700060817     c                   eval      dissca6 = 'MEHR ALS '
230800060606     c                   endsl
230900010622     c                   move      dissca6       dissca
231000010608     c                   endif
231100010725      * se 56 acceso il 6° scaglione è = 00000
231200010725     c                   if        *in56
231300010725     c                   clear                   dissca6
231400010725     c                   move      dissca6       dissca
231500010725     c                   endif
231600010608     C*
231700010612      * prepatro la schiera dell'applicazione tariffa finita
231800010612     C                   Z-ADD     K4            B
231900010612     C                   DO        6             A
232000010612     C                   MOVE      ATF(B)        STF(A)
232100010612     C                   ADD       1             B
232200010612     C                   END
232300010612      *
232400010612     c                   clear                   prtatf
232500010612      *
232600010801     c                   eval      PRTATF = '    ' + stf(1) + '      '  +
232700010801     c                                      stf(2) + '      '    +
232800010801     c                                      stf(3) + '      '    +
232900010801     c                                      stf(4) + '      '    +
233000010626     c                                      stf(5) + '      '    +
233100010612     c                                      stf(6)
233200010612     C*
233300010917      *
233400010917     c                   if        *in80 or curline > 56
233500010917     c                   write     ta49pfin
233600010917     c                   write     ta49pPP
233700010917     c                   setoff                                       80
233800010917     c                   endif
233900010917      * se non stampo dettaglio tariffe particolari stampo linea di partenza
234000010625     c  n10              write     ta49p02a
234100010626      * SE TARIFFE PARTICOLARI NON STAMPO DA VOSTRO MAGAZZINO
234200010626     C                   IF        *IN10
234300010626     C                   SETON                                        78
234400010626     C                   ENDIF
234500061207
234600061207      * se tariffa particolare "f " Fuel
234700061207     c                   if        *in12
234800061207      * forzo l'accensione dell'indicatore 79 per non stampare la seconda riga
234900061207      * quando ho più scaglioni
235000061207     c                   eval      *in79 = *on
235100061207      * e pulisco il campo di stampa
235200061207     c                   clear                   prtatf
235300061207     c                   endif
235400010625      *
235500010612     C                   write     ta49p02
235600010612      *
235700010612     c                   ENDSR
235800010613      *
235900010613     C*-------------------------------------------------------------------------
236000010613     C*  SR_FULCRO  STAMPA DETTAGLIO TARIFFARIO
236100010613     C*-------------------------------------------------------------------------
236200010613     C     SR_FULCRO     BEGSR
236300020531
236400020531     c                   eval      w_oklnp = *off
236500010613      *
236600010613      * Leggo il dettaglio tariffario dall'inizio
236700010613      *
236800061122     C  n98DS1KTM        SETLL     TITAD04l
236900061122     c   98ds1ktm        setll     tiofd01l
237000010613     C                   DO        *HIVAL
237100010613      *
237200061122     C  n98DS1KTM        READE     TITAD04l
237300061122     c   98ds1ktm        reade     tiofd01l
237400010613      * se fine file
237500061122     c                   if        %eof
237600010613      *
237700010613      * se cap uguale a blank STAMPO totali per provincia
237800010613      *
237900010613     c                   if        rotcap = *blanks
238000010613     c                   exsr      protot
238100010613     c                   else
238200010613      *
238300010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
238400010613      *
238500010613     c                   exsr      captot
238600010613     c                   endif
238700010613      *
238800010613      * calcoli per regione
238900010613      *
239000010613     c                   exsr      calreg
239100010613      *
239200010613      * stampo i totali per regione
239300010613      *
239400010613     c  n99              exsr      stareg
239500010613      *
239600010613     c                   leave
239700010613      * eof
239800010613     c                   endif
239900010613      *
240000010613      * verifico il record letto se coerente con le richieste di stampa
240100010613      *
240200010613      * se annullato rileggo
240300010613     c                   if        tadatb = 'A'
240400010613     c                   iter
240500010613     c                   endif
240600020531     c                   eval      w_okstp = *off
240700010613      * se I45DET = 'L' devo stampare solo le righe di dettaglio che hanno la
240800010613      *                 LNP uguale alla LNP del codice cliente
240900020531     c                   if        I45DET = 'L'
241000020531      * se ho già stampato una linea di partenza non stampo più niente
241100020531     c                   if        w_oklnp = *on and rotlnp <> 0 and
241200020531     c                             rotlnp <> tadlnp
241300020531     c                   iter
241400020531     c                   endif
241500020531     c                   exsr      sr_chklnp
241600020531     c                   if        w_okstp = *off
241700010613     c                   iter
241800010613     c                   endif
241900020531     c                   eval      w_oklnp = *on
242000020531     c                   endif
242100010613      * se I45DET = 'A' devo stampare solo le righe di dettaglio che hanno la
242200010613      *                 LNP uguale alla LNP del codice cliente o il codice
242300010613      *                 tassazione tra quelli pevisti dalla linea del codice
242400010613      *                 cliente
242500010613     c                   if        I45DET = 'A'
242600020531      * se ho già stampato una linea di partenza non stampo più niente
242700020531     c                   if        w_oklnp = *on and rotlnp <> 0 and
242800020531     c                             rotlnp <> tadlnp
242900020531     c                   iter
243000020531     c                   endif
243100020531     c                   exsr      sr_chklnp
243200010613     c     tadcts        lookup    ctl                                    35
243300020531     c                   if        not *in35 and w_okstp = *off
243400010613     c                   iter
243500010613     c                   endif
243600020531     c                   eval      w_oklnp = *on
243700010613     c                   endif
243800010613      *
243900010613      * verifico se è uno scaglione previsto
244000010613      *
244100010613     c                   z-add     tadsgl        wsgl72
244200010614     c     wsgl72        lookup    ssl                                    35
244300010614      * se non trovato nei 6 scaglioni da stampare verifico se scaglione corrett
244400010614     c                   if        not *in35
244500010613     c     wsgl72        lookup    sgl                                    35
244600010613     c                   if        not *in35
244700010613      * errore scaglione non stampabile
244800050914      * esco dal pgm
244900050914     c                   seton                                        99
245000010613     c                   movel     msg(8)        errmsg
245100010613     c                   exsr      staerr
245200050914     c                   leave
245300010614     c                   endif
245400010613     c                   iter
245500010613     c                   endif
245600010613
245700010613      * preparo i campi per eventuali rotture
245800010613
245900010613     c                   eval      compro = %subst(tadorp:5:1)
246000010613     c                   eval      comreg = %subst(tadorp:3:2)
246100010614      *
246200010614     c                   SELECT
246300010614      *
246400010613      *-------------------------------------------------------------------------
246500010613      *           ROTTURA PER      L I N E A    D I    P A R T E N Z A
246600010613      *-------------------------------------------------------------------------
246700010614     c                   WHEN      tadlnp  <> rotlnp
246800010613     c                   eval      rotlnp = tadlnp
246900010613     c                   eval      rotreg = comreg
247000010613     c                   eval      rotpro = compro
247100010613      * verifico se cap valorizzato
247200010613     c                   if        rotcap = *blanks
247300010613      * se cap uguale a blank STAMPO totali per provincia
247400010613     c                   exsr      protot
247500010613     c                   else
247600010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
247700010613     c                   exsr      captot
247800010613     c                   endif
247900010613      *
248000010613     c                   eval      rotcap = tadcap
248100010613      * calcoli per regione
248200010613     c                   exsr      calreg
248300010613      * se ci sono errori vado a fine
248400010613     c   99              leave
248500010613      * stampo i totali per regione
248600010613     c                   exsr      stareg
248700010613      * decodifico la nuova linea di partenza
248800020531     c     tadlnp        chain     azorg01l
248900020531     c                   if        %found(azorg01l)
249000020531     c                   movel     orgdes        stalnp
249100020624      * controllo se linea di partenza 9xx
249200020624     c                   if        orgfl2 = 'S'
249300020624      * italia
249400020624     c                   if        tadlnp = 950
249500060704      * in lingua
249600060704     c                   select
249700060704     c                   when      i45lin = 'I'
249800070312     c                   eval      prtlnp =
249900070312     c                             'Merce ritirata da tutte le ns. filiali'
250000020624     c                             + '  ' + stalnp
250100060704     c                   when      i45lin = '2'
250200060704     c                   eval      prtlnp = 'Goods collected from all our +
250300070314     c                              branches' + '  ' + stalnp
250400060817     c                   when      i45lin = '3'
250500070314     c                   eval      prtlnp = 'Marchandise retirée par toutes +
250600070314     c                              nos filiales' + '  ' + stalnp
250700060817     c                   when      i45lin = '4'
250800070314     c                   eval      prtlnp = 'Abholung in allen unseren Filialen'
250900060817     c                             + '  ' + stalnp
251000060704     c                   endsl
251100020624     c                   else
251200020624      * regione
251300060704      * in lingua
251400060704     c                   select
251500060704     c                   when      i45lin = 'I'
251600070312     c                   eval      prtlnp = 'Merce ritirata dalle ns. filiali +
251700070312     c                              della regione' + '  ' + stalnp
251800060704     c                   when      i45lin = '2'
251900070314     c                   eval      prtlnp = 'Goods collected from our branches +
252000060704     c                              in the region' + '  ' + stalnp
252100060817     c                   when      i45lin = '3'
252200070314     c                   eval      prtlnp = 'Marchandise retirée par nos +
252300070314     c                              filiales de la région' + '  ' + stalnp
252400060817     c                   when      i45lin = '4'
252500070314     c                   eval      prtlnp = 'Abholung in unseren Filialen der +
252600060817     c                              Region' + '  ' + stalnp
252700060704     c                   endsl
252800020624     c                   endif
252900020624     c                   else
253000020624      * lnp non è 9xx
253100060704      * in lingua
253200060704     c                   select
253300060704     c                   when      i45lin = 'I'
253400070312     c                   eval      prtlnp =
253500070312     c                             'Merce ritirata dalla ns. filiale di'
253600020624     c                             + '  ' + stalnp
253700060704     c                   when      i45lin = '2'
253800070314     c                   eval      prtlnp = 'Goods collected from our branch +
253900060704     c                              in' + '  ' + stalnp
254000060817     c                   when      i45lin = '3'
254100060817     c                   eval      prtlnp = 'Marchandise retirée par notre +
254200070314     c                              filiale de' + '  ' + stalnp
254300060817     c                   when      i45lin = '4'
254400070314     c                   eval      prtlnp = 'Abholung in unseren Filialen in'
254500060817     c                             + '  ' + stalnp
254600060704     c                   endsl
254700020624     c                   endif
254800020624
254900020531     c                   endif
255000010613      * stampo la testata scaglioni con la nuova linea di partenza
255100010613      *
255200010625      *
255300010628     c                   if        *in80 or curline > 56
255400010628     c                   write     ta49pfin
255500010628     c                   write     ta49pPP
255600010628     c                   setoff                                       80
255700010628     c                   endif
255800010625      *
255900010625     c                   write     ta49p02a
256000010625     c                   write     ta49p02
256100010613      * pulizie e controllo dettaglio per regione
256200010613     c                   exsr      regdet
256300010613      * pulizie e controllo dettaglio per provincia
256400010613     c                   exsr      prodet
256500010613      * se ci sono errori vado a fine
256600010613     c   99              leave
256700010613      *
256800010613      *
256900010613      * sto leggendo per la stessa linea di partenza
257000010613      *
257100010613      *-------------------------------------------------------------------------
257200010613      *           ROTTURA PER      R E G I O N E
257300010613      *-------------------------------------------------------------------------
257400010614     c                   WHEN      comreg <> rotreg
257500010613     c                   eval      rotreg = comreg
257600010613     c                   eval      rotpro = compro
257700010613      * verifico se cap valorizzato
257800010613     c                   if        rotcap = *blanks
257900010613      * se cap uguale a blank STAMPO totali per provincia
258000010613     c                   exsr      protot
258100010613     c                   else
258200010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
258300010613     c                   exsr      captot
258400010613     c                   endif
258500010613      *
258600010613     c                   eval      rotcap = tadcap
258700010613      * calcoli per regione
258800010613     c                   exsr      calreg
258900010613      * se ci sono errori vado a fine
259000010613     c   99              leave
259100010613      * stampo i totali per regione
259200010613     c                   exsr      stareg
259300010613      * pulizie e controllo dettaglio per regione
259400010613     c                   exsr      regdet
259500010613      * pulizie e controllo dettaglio per provincia
259600010613     c                   exsr      prodet
259700010613      * se ci sono errori vado a fine
259800010613     c   99              leave
259900010613      *
260000010613      *
260100010613      * sto leggendo per la stessa linea di partenza / regione
260200010613      *
260300010613      *-------------------------------------------------------------------------
260400010613      *           ROTTURA PER      P R O V I N C I A
260500010613      *-------------------------------------------------------------------------
260600010614     c                   WHEN      compro <> rotpro
260700010613     c                   eval      rotpro = compro
260800010613      * verifico se cap valorizzato
260900010613     c                   if        rotcap = *blanks
261000010613      * se cap uguale a blank STAMPO totali per provincia
261100010613     c                   exsr      protot
261200010613     c                   else
261300010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
261400010613     c                   exsr      captot
261500010613     c                   endif
261600010613      *
261700010613     c                   eval      rotcap = tadcap
261800010613      * pulizie e controllo dettaglio per provincia
261900010613     c                   exsr      prodet
262000010613      * se ci sono errori vado a fine
262100010613     c   99              leave
262200010613      *
262300010613      *
262400010613      * sto leggendo per la stessa linea di partenza / regione / provincia
262500010613      *
262600010613      *-------------------------------------------------------------------------
262700010613      *           ROTTURA PER      P A E S E   D I    A R R I V O   (CAP)
262800010613      *-------------------------------------------------------------------------
262900010614     c                   WHEN      tadcap <> rotcap
263000010613      * verifico se cap valorizzato
263100010613     c                   if        rotcap = *blanks
263200010613      * se cap uguale a blank STAMPO totali per provincia
263300010613     c                   exsr      protot
263400010613     c                   else
263500010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
263600010613     c                   exsr      captot
263700010613     c                   endif
263800010613      * se ci sono errori vado a fine
263900010613     c   99              leave
264000010613      *
264100010613     c                   eval      rotcap = tadcap
264200010613      * pulisco le schiere di stampa
264300010613     c                   clear                   tct
264400010613     c                   clear                   tmn
264500010613     c                   clear                   tmx
264600010613     c                   clear                   tin
264700010613      *
264800010614     c                   ENDSL
264900010613      *
265000010613      *-------------------------------------------------------------------------
265100010613      *      E L A B O R O   L O   S C A G L I O N E
265200010613      *-------------------------------------------------------------------------
265300010613
265400010613      * ricerco lo SCAGLIONE di appartenenza
265500010613     c                   z-add     1             k1
265600010613     c                   z-add     tadsgl        wsgl72
265700010613     c     wsgl72        lookup    ssl(k1)                                35
265800010614      * se non trovato scrivo nel 1° scaglione
265900010614     c  n35              z-add     1             k1
266000010614      * valorizzo le schiere degli importi da stampare
266100010614     c                   z-add     taditr        tct(k1)
266200010614     c                   z-add     tadmin        tmn(k1)
266300010614     c                   z-add     tadmax        tmx(k1)
266400010614      * se devo stampare l'inoltro per ogni dettaglio
266500010614     c                   if        stino = 'Y'
266600010614     c                   z-add     tadino        tin(k1)
266700010614     c                   endif
266800010614      *
266900010614     c                   ENDDO
267000010614      *
267100010614     c                   ENDSR
267200020531
267300020531      *-------------------------------------------------------------------------
267400020531      *  SR_CHKLNP  controllo se LNP ok per la stampa
267500020531      *-------------------------------------------------------------------------
267600020531     c     sr_chklnp     begsr
267700020531
267800020531     c                   if        w_okstp = *off
267900020531     c                   select
268000020531     c                   when      tadlnp = comfil
268100020531     c                   eval      w_okstp = *on
268200020531     c                   when      tadlnp = §ogret
268300020531     c                   eval      w_okstp = *on
268400020531     c                   when      tadlnp = 950
268500020531     c                   eval      w_okstp = *on
268600020531     c                   endsl
268700020531     c                   endif
268800020531
268900020531     c                   endsr
269000020531
269100010614     C*-------------------------------------------------------------------------
269200010614     C*  PROTOT  TOTALI X  P R O V I N C I A
269300010614     C*-------------------------------------------------------------------------
269400010614     C     PROTOT        BEGSR
269500010614      *
269600010614      *  SALVO LA SCHIERA TCT NEL PRIMO BIG LIBERO
269700010614     C                   Z-ADD     1             K
269800010614     C     *BLANKS       LOOKUP    BIG(K)                                 35
269900010614
270000010614      * se non trovo nessun posto libero errore
270100010614
270200010614     C                   if        not *in35
270300010614     C                   seton                                        99
270400010614     C                   move      MSG(9)        errmsg
270500010614     C                   EXSR      STAERR
270600050930      * vado a fine se errore
270700050930     c                   goto      fine
270800010614     C                   else
270900051006      * Controllo se ok tutti gli scaglioni
271000051006     c                   ExSr      Sr_Ctrsca
271100010614      * carico la schiera delle province 6 alla volta
271200010614      * città
271300010614     C                   DO        6             B
271400010614     C                   MOVE      TCT(B)        COM(B)
271500010614     C                   END
271600010614     C                   MOVEA     COM           BIG(K)
271700010614      *
271800010614     C                   MOVE      CODSIG        BSI(K)
271900010614     C                   MOVE      CODORP        BRG(K)
272000050204     c                   move      desprv        dprv(k)
272100050303     c                   eval      nodes(k) = nostdes
272200010614      * minimo
272300010614     C                   DO        6             B
272400010614     C                   MOVE      TMN(B)        COM(B)
272500010614     C                   END
272600010614     C                   MOVEA     COM           BMN(K)
272700010614      * masismo
272800010614     C                   DO        6             B
272900010614     C                   MOVE      TMX(B)        COM(B)
273000010614     C                   END
273100010614     C                   MOVEA     COM           BMX(K)
273200010614      * inoltro
273300010614     C                   DO        6             B
273400010614     C                   MOVE      TIN(B)        COM(B)
273500010614     C                   END
273600010614     C                   MOVEA     COM           BIN(K)
273700010614      **
273800010614     C                   END
273900010614      *
274000010614     C                   ENDSR
274100010614     C*-------------------------------------------------------------------------
274200010614     C*  CAPTOT  TOTALI X  PAESE DI ARRIVO (CAP)
274300010614     C*-------------------------------------------------------------------------
274400010614     C     CAPTOT        BEGSR
274500010614      *  SALVO LA SCHIERA TCT DIRETTAMENTE NEL PDE/WIN
274600010614     C                   Z-ADD     1             P
274700010614     C     *BLANKS       LOOKUP    PDE(P)                                 35
274800010614      * se non trovato do errore
274900010614     c                   if        not *in35
275000010614     C                   seton                                        99
275100010614     C                   move      MSG(12)       errmsg
275200010614     C                   EXSR      STAERR
275300010614      *
275400010614     C                   ELSE
275500051006      * Controllo se ok tutti gli scaglioni
275600051006     c                   ExSr      Sr_Ctrsca
275700010614      *
275800010614     C                   MOVEL     *all'.'       PDE(P)
275900010614     C                   MOVE      rotcap        PDE(P)
276000010614      * città
276100010614     C                   DO        6             B
276200010614     C                   MOVE      TCT(B)        COM(B)
276300010614     C                   END
276400010614     C                   MOVEA     COM           WIN(P)
276500010614      * minimo
276600010614     C                   DO        6             B
276700010614     C                   MOVE      TMN(B)        COM(B)
276800010614     C                   END
276900010614     C                   MOVEA     COM           WMN(P)
277000010614      * massimo
277100010614     C                   DO        6             B
277200010614     C                   MOVE      TMX(B)        COM(B)
277300010614     C                   END
277400010614     C                   MOVEA     COM           WMX(P)
277500010614      * inoltro
277600010614     C                   DO        6             B
277700010614     C                   MOVE      TIN(B)        COM(B)
277800010614     C                   END
277900010614     C                   MOVEA     COM           WTI(P)
278000010614     C                   END
278100010614      *
278200010614     C                   ENDSR
278300051006      *-------------------------------------------------------------------------
278400051006      * Controllo se ok gli scaglioni da stampare
278500051006      *-------------------------------------------------------------------------
278600051006     c     Sr_Ctrsca     BegSr
278700051006
278800051006     c                   Select
278900051006
279000051006     c                   When      wnrscaglio > 12
279100051006      * sto stampando il primo o il secondo giro di scaglioni
279200051006      * vuol dire che devo stampare di sicuro 6 importi
279300051006     c                   If        k4 < 13
279400051006     c                   Eval      yy = 6
279500051006      * sto stampando il terzo giro di scaglioni
279600051006      * cerco quanti importi devo stampare
279700051006     c                   Else
279800060428     c                   Eval      yy = wnrscaglio - 12
279900051006     c                   EndIf
280000051006
280100051006     c                   When      wnrscaglio > 6 and wnrscaglio < 12
280200051006      * sto stampando il primo giro di scaglioni
280300051006      * vuol dire che devo stampare di sicuro 6 importi
280400051006     c                   If        k4 = 1
280500051006     c                   Eval      yy = 6
280600051006      * sto stampando il secondo giro di scaglioni
280700051006      * cerco quanti importi devo stampare
280800051006     c                   Else
280900051006     c                   Eval      yy = wnrscaglio - 6
281000051006     c                   EndIf
281100051006
281200051006     c                   When      wnrscaglio = 6
281300051006     c                   Eval      yy = 6
281400051006
281500051006     c                   When      wnrscaglio < 6
281600051006     c                   Eval      yy = wnrscaglio
281700051006     c                   EndSl
281800051006
281900051006      * controllo se ci sono degli importi a zero
282000051006     c                   Do        yy            xx
282100051006     c                   If        tct(xx) = *Zeros
282200051006     c                   Eval      errmsg = msg(8)
282300051006     c                   ExSr      staerr
282400051006     c                   Goto      fine
282500051006     c                   EndIf
282600051006     c                   EndDo
282700051006
282800051006     c                   EndSr
282900010614     C*-------------------------------------------------------------------------
283000010614     C*  CALREG  CALCOLO SKIERE X STAMPA REGIONE
283100010614     C*-------------------------------------------------------------------------
283200010614     C     CALREG        BEGSR
283300010614      *
283400010614      *                  N  O  T  E
283500010614      * Leggo BIG (schiera per ogni provincia della regione) e carico WIN/WCT
283600010614      * schiere di work)
283700010614      * X ogni provincia letta (quindi per ogni elemento) dopo averlo caricato
283800010614      * controllo se ci sono altre province uguali
283900010614      * se si accodo nella skiera dei COD/TASSAZ. che hanno la stessa tariffa
284000010614      * leggo i soli elementi con BRG>0 (a mano mano che li raggruppo
284100010614      * li abblenco). Mi fermo quando leggo la regione che è sempre
284200010614      * l'ultimo e va da solo : K=1 la descrizione è  CAPOLUOGHI
284300010614      *                         K>1 "    "    "    è  ALTRI
284400010614      *
284500010614      * RICERCO IL 1.O ELEMENTO NON OCCUPATO DA PAESI DI ARRIVO
284600010614     C                   DO        29            P
284700010614     C     PDE(P)        COMP      *blanks                                35
284800010614     C   35              SUB       1             P
284900010614     C  N35              END
285000110216
285100110216     c                   clear                   savk
285200010614      *
285300050616    1C                   DO        15            K
285400010614    2C     BRG(K)        IFNE      '  '
285500010614      * ELEMENTO VALIDO                                    -----------
285600010614      *
285700010614      * 9 = regione
285800010614      *
285900010614    3C     BRG(K)        IFEQ      '9'
286000010614     C                   ADD       1             P
286100110330     c                   eval      savk = p
286200010614      * errore
286300010614    4C     P             IFGT      30
286400010614     C                   SETON                                        99
286500010614     C                   MOVE      MSG(12)       ERRMSG
286600010614     C                   EXSR      STAERR
286700010614     C                   GOTO      ENDCAL
286800010614    4C                   END
286900010614      * città
287000010614     C                   MOVEA     BIG(K)        COMO42
287100010614     C                   MOVEA     COMO42        WIN(P)
287200010614     C                   MOVE      *BLANKS       BIG(K)
287300010614      * minimo
287400010614     C                   MOVEA     BMN(K)        CMNO42
287500010614     C                   MOVEA     CMNO42        WMN(P)
287600010614     C                   MOVE      *BLANKS       BMN(K)
287700010614      * massimo
287800010614     C                   MOVEA     BMX(K)        CMXO42
287900010614     C                   MOVEA     CMXO42        WMX(P)
288000010614     C                   MOVE      *BLANKS       BMX(K)
288100010614      * inoltro
288200010614     C                   MOVEA     BIN(K)        CINO42
288300010614     C                   MOVEA     CINO42        WTI(P)
288400010614     C                   MOVE      *BLANKS       BIN(K)
288500010614      *
288600010614     C                   MOVE      '  '          BRG(K)
288700010614      *
288800010614    4C     K             IFEQ      1
288900010626     C                   EVAL      PDE(P) = '. . . . . . . . . . '
289000010614     C                   ELSE
289100110216     c                   eval      pde(p) = '.........................'
289200010614    4C                   END
289300010614     C                   GOTO      ENDCAL
289400010614      * non si tratta di regione
289500010614
289600010614   X3C                   ELSE
289700010614
289800010614     C                   MOVE      *BLANKS       WCT
289900010614     C                   Z-ADD     0             C
290000050204     c                   Eval      wragprv = *Off
290100010614      * città
290200010614     C                   ADD       1             P
290300010614     C                   MOVEA     BIG(K)        COMO42
290400010614     C                   MOVEA     COMO42        WIN(P)
290500010614     C                   MOVE      *BLANKS       BIG(K)
290600010614      * minimo
290700010614     C                   MOVEA     BMN(K)        CMNO42
290800010614     C                   MOVEA     CMNO42        WMN(P)
290900010614     C                   MOVE      *BLANKS       BMN(K)
291000010614      * massimo
291100010614     C                   MOVEA     BMX(K)        CMXO42
291200010614     C                   MOVEA     CMXO42        WMX(P)
291300010614     C                   MOVE      *BLANKS       BMX(K)
291400010614      * inoltro
291500010614     C                   MOVEA     BIN(K)        CINO42
291600010614     C                   MOVEA     CINO42        WTI(P)
291700010614     C                   MOVE      *BLANKS       BIN(K)
291800010614      * sigla della provincia
291900010614     C                   ADD       1             C
292000010614     C                   MOVE      BSI(K)        WCT(C)
292100010614      *
292200010614     C                   MOVE      '  '          BRG(K)
292300010614      **
292400010614      * DOPO AVER CARICATO LA PROV. LETTA NE CERCO ALTRE DA RAGGRUPPARE
292500010614     C     K             ADD       1             H
292600010614      *
292700050616    4C     H             DO        15            H
292800010614    5C     COMO42        IFEQ      BIG(H)
292900010614     C     CMNO42        ANDEQ     BMN(H)
293000010614     C     CMXO42        ANDEQ     BMX(H)
293100010614     C     CINO42        ANDEQ     BIN(H)
293200010614     C     BRG(H)        ANDNE     '9'
293300010614     C                   ADD       1             C
293400010614     C                   MOVE      BSI(H)        WCT(C)
293500010614     C                   MOVEL     '/'           WCT(C)
293600010614      *
293700010614     C                   MOVE      *BLANKS       BIG(H)
293800010614     C                   MOVE      *BLANKS       BMN(H)
293900010614     C                   MOVE      *BLANKS       BMX(H)
294000010614     C                   MOVE      *BLANKS       BIN(H)
294100010614     C                   MOVE      '  '          BRG(H)
294200050204     c                   Eval      wragprv = *On
294300010614      *
294400010614    5C                   ENDIF
294500010614    4C                   ENDDO
294600010614      *
294700010614     C                   ADD       1             C
294800010614     C                   MOVEA     *ALL'.'       WCT(C)
294900010614     C                   MOVEA     WCT           COMO29
295000010614     C                   MOVEA     COMO29        PDE(P)
295100050204
295200050204      * se tariffa estera e non ho raggruppato più province devo stampare
295300050204      * la descrizione della provincia e non il codice
295400050204     c                   If        wragprv = *Off and (*in02 or *in25)
295500050204     c                   Eval      como29 = *all'.'
295600050303     c                   If        nodes(k) <> 'N'
295700050204     c                   Eval      %subst(como29:1:(%len(%trim(dprv(k))))) =
295800050204     c                             %trim(dprv(k))
295900050303     c                   EndIf
296000050204     c                   Move      como29        pde(p)
296100050204     c                   EndIf
296200010614      *
296300010614    3C                   END
296400010614      *
296500010614    2C                   END
296600010614    1C                   END
296700010614      *
296800010614     C     ENDCAL        ENDSR
296900010614     C*-------------------------------------------------------------------------
297000010615     C*  STAREG  GESTIONE STAMPE REGIONE
297100010614     C*-------------------------------------------------------------------------
297200010614     C     STAREG        BEGSR
297300110216
297400110216     c                   clear                   giro              1 0
297500110216
297600110216      * stampo per prima 'ALTRI' cioè la regione
297700110216     c                   if        savk > 1
297800110216     c                   eval      k = savk
297900110216     c                   else
298000010614      * LOOP EMISSIONE RAGGRUPPAMENTI/REGIONE
298100010614     C                   Z-ADD     1             K
298200110216     c                   endif
298300110216
298400110216     c     rifai         tag
298500010614      *
298600010615    1C     WIN(K)        DOUEQ     *BLANKS
298700110216     c                   if        k = savk  and giro > 0
298800110216     c                   leave
298900110216     c                   endif
299000010614     C                   MOVEA     WIN(K)        COM
299100010614     C                   Z-ADD     1             B
299200010614     C                   DO        6             B
299300010614     C                   MOVE      COM(B)        PAM(B)
299400010614      * PER TARIFFA A VALORE
299500010614     C                   IF        *IN87 AND COM(B) > *ZEROS
299600010614     C                   MOVEL     '%'           AI2(B)
299700010614     C                   END
299800010614      *
299900010614     C                   END
300000010614      * VERIFICO SE C'E' IL MINIMO E IL MASSIMO
300100010614     C                   SETOFF                                       4849
300200010614      *
300300010615    2C                   IF        WMN(K) <> *BLANKS AND WMN(K) <> *ZEROS
300400010614     C                   SETON                                        48
300500010614      *
300600010614     C                   MOVEA     WMN(K)        COM
300700010614      *
300800010614     C                   DO        6             B
300900010614     C                   MOVE      COM(B)        PMN(B)
301000010614     C                   ENDDO
301100010614      *
301200010615    2C                   ENDIF
301300010614      **
301400010615    2C                   IF        WMX(K) <> *BLANKS AND WMX(K) <> *ZEROS
301500010614     C                   SETON                                        49
301600010614      **
301700010614     C                   MOVEA     WMX(K)        COM
301800010614      *
301900010614     C                   DO        6             B
302000010614     C                   MOVE      COM(B)        PMX(B)
302100010614     C                   ENDDO
302200010614      *
302300010615    2C                   ENDIF
302400010614      *
302500010614      * VERIFICO SE DEVO STAMPARE LE SPESE DI INOLTRO
302600010725     C                   SETOFF                                       57
302700010614      *
302800010615    2C                   IF        STINO = 'Y'
302900010725     C                   SETON                                        57
303000010614     C                   MOVEA     WTI(K)        COM
303100010614     C                   Z-ADD     1             B
303200010614      *
303300010614     C                   DO        6             B
303400010614     C                   MOVE      COM(B)        PIN(B)
303500010614     C                   END
303600010614      *
303700010615    2C                   ENDIF
303800010614      *
303900010614     C*
304000010615     C* VERIFICO SE CI SONO DEI VALORI A ZERO
304100010615     C*
304200010615      * VALORIZZO I CAMPI DI STAMPA
304300010615     C                   CLEAR                   TA49P03
304400010615      *
304500010615     C     PAM(1)        COMP      0                                  61
304600010615     C     PAM(2)        COMP      0                                  62
304700010615     C     PAM(3)        COMP      0                                  63
304800010615     C     PAM(4)        COMP      0                                  64
304900010622     C     PAM(5)        COMP      0                                  65
305000010622     C     PAM(6)        COMP      0                                  66
305100010615      *
305200010615     C                   Z-ADD     PAM(1)        TAR01
305300010615     C                   Z-ADD     PAM(2)        TAR02
305400010615     C                   Z-ADD     PAM(3)        TAR03
305500010615     C                   Z-ADD     PAM(4)        TAR04
305600010615     C                   Z-ADD     PAM(5)        TAR05
305700010615     C                   Z-ADD     PAM(6)        TAR06
305800010615      *
305900010615      * VALORIZZO LA PARTE DESCRITTIVA
306000010615     C                   EVAL      P03DES = DESREG + ' - ' + PDE(K)
306100010615      *
306200010615     C                   CLEAR                   DESREG
306300010615      *
306400010615      * SE TARIFFA A VALORE STAMPO LA %
306500010615     C                   IF        *IN87
306600010615     C                   MOVE      AI2(1)        PER01
306700010615     C                   MOVE      AI2(2)        PER02
306800010615     C                   MOVE      AI2(3)        PER03
306900010615     C                   MOVE      AI2(4)        PER04
307000010615     C                   MOVE      AI2(5)        PER05
307100010615     C                   MOVE      AI2(6)        PER06
307200010615     C                   ENDIF
307300010615      *
307400010622      * se of ristampo la testata tariffa
307500010622      *
307600010723     c                   if        *in80 or curline > 61
307700010627     c                   write     ta49pfin
307800010622     c                   write     ta49pPP
307900010625     c                   write     ta49p02a
308000010622     c                   write     ta49p02
308100010622     c                   setoff                                       80
308200010622     c                   endif
308300010622      *
308400010615     C                   WRITE     TA49P03
308500010615      *
308600010615     C*
308700010615     C* VERIFICO SE CI SONO DEI VALORI A ZERO
308800010615     C*
308900010615      * VALORIZZO I CAMPI DI STAMPA
309000010615     C                   CLEAR                   TA49P03
309100010615      * MINIMO
309200010615    2C                   IF        *IN48
309300010614     C     PMN(1)        COMP      0                                  61
309400010614     C     PMN(2)        COMP      0                                  62
309500010614     C     PMN(3)        COMP      0                                  63
309600010614     C     PMN(4)        COMP      0                                  64
309700010622     C     PMN(5)        COMP      0                                  65
309800010622     C     PMN(6)        COMP      0                                  66
309900010615      *
310000010615     C                   Z-ADD     PMN(1)        TAR01
310100010615     C                   Z-ADD     PMN(2)        TAR02
310200010615     C                   Z-ADD     PMN(3)        TAR03
310300010615     C                   Z-ADD     PMN(4)        TAR04
310400010615     C                   Z-ADD     PMN(5)        TAR05
310500010615     C                   Z-ADD     PMN(6)        TAR06
310600010615      *
310700010615      * VALORIZZO LA PARTE DESCRITTIVA
310800060606      * in lingua
310900060606     c                   select
311000060606     c                   when      i45lin = 'I'
311100010626     C                   EVAL      P03DES = '               Tariffa minima ...'
311200060606     c                   when      i45lin = '2'
311300060606     c                   eval      p03des = '               Minimum price ....'
311400060817     c                   when      i45lin = '3'
311500060817     c                   eval      p03des = '               Tarif minimum ....'
311600060817     c                   when      i45lin = '4'
311700060817     c                   eval      p03des = '               Mindestpreis .....'
311800060606     c                   endsl
311900010625      *
312000010625      * se of ristampo la testata tariffa
312100010625      *
312200010723     c                   if        *in80 or curline > 61
312300010627     c                   write     ta49pfin
312400010625     c                   write     ta49pPP
312500010625     c                   write     ta49p02a
312600010625     c                   write     ta49p02
312700010625     c                   setoff                                       80
312800010625     c                   endif
312900010625      *
313000010615     C                   WRITE     TA49P03
313100010615      *
313200010615    2C                   ENDIF
313300010615     C*
313400010615     C* VERIFICO SE CI SONO DEI VALORI A ZERO
313500010615     C*
313600010615      * VALORIZZO I CAMPI DI STAMPA
313700010615     C                   CLEAR                   TA49P03
313800010615      * MASSIMO
313900010615    2C                   IF        *IN49
314000010614     C     PMX(1)        COMP      0                                  61
314100010614     C     PMX(2)        COMP      0                                  62
314200010614     C     PMX(3)        COMP      0                                  63
314300010614     C     PMX(4)        COMP      0                                  64
314400010622     C     PMX(5)        COMP      0                                  65
314500010622     C     PMX(6)        COMP      0                                  66
314600010615      *
314700010615     C                   Z-ADD     PMX(1)        TAR01
314800010615     C                   Z-ADD     PMX(2)        TAR02
314900010615     C                   Z-ADD     PMX(3)        TAR03
315000010615     C                   Z-ADD     PMX(4)        TAR04
315100010615     C                   Z-ADD     PMX(5)        TAR05
315200010615     C                   Z-ADD     PMX(6)        TAR06
315300010615      *
315400010615      * VALORIZZO LA PARTE DESCRITTIVA
315500060606      * in lingua
315600060606     c                   select
315700060606     c                   when      i45lin = 'I'
315800010626     C                   EVAL      P03DES = '               Tariffa massima ...'
315900060606     c                   when      i45lin = '2'
316000060606     c                   eval      p03des = '               Maximum price ....'
316100060817     c                   when      i45lin = '3'
316200060817     c                   eval      p03des = '               Tarif maximum ....'
316300060817     c                   when      i45lin = '4'
316400060817     c                   eval      p03des = '               Höchstpreis ......'
316500060606     c                   endsl
316600010615      *
316700010625      *
316800010625      * se of ristampo la testata tariffa
316900010625      *
317000010723     c                   if        *in80 or curline > 61
317100010627     c                   write     ta49pfin
317200010625     c                   write     ta49pPP
317300010625     c                   write     ta49p02a
317400010625     c                   write     ta49p02
317500010625     c                   setoff                                       80
317600010625     c                   endif
317700010625      *
317800010615     C                   WRITE     TA49P03
317900010615      *
318000010615    2C                   ENDIF
318100010615     C*
318200010615     C* VERIFICO SE CI SONO DEI VALORI A ZERO
318300010615     C*
318400010615      * VALORIZZO I CAMPI DI STAMPA
318500010615     C                   CLEAR                   TA49P03
318600010615      * SPESE DI INOLTRO
318700010725    2c                   IF        *in57
318800010615      * verifico se ci sono dei valori da stampare
318900010615      *
319000010615     C                   XFOOT     PIN           TOTINO
319100010615    3C                   IF        TOTINO > *ZEROS
319200010615      *
319300010614     C     PIN(1)        COMP      0                                  61
319400010614     C     PIN(2)        COMP      0                                  62
319500010614     C     PIN(3)        COMP      0                                  63
319600010614     C     PIN(4)        COMP      0                                  64
319700010622     C     PIN(5)        COMP      0                                  65
319800010622     C     PIN(6)        COMP      0                                  66
319900010615      *
320000010615     C                   Z-ADD     PIN(1)        TAR01
320100010615     C                   Z-ADD     PIN(2)        TAR02
320200010615     C                   Z-ADD     PIN(3)        TAR03
320300010615     C                   Z-ADD     PIN(4)        TAR04
320400010615     C                   Z-ADD     PIN(5)        TAR05
320500010615     C                   Z-ADD     PIN(6)        TAR06
320600010615      *
320700010615      * VALORIZZO LA PARTE DESCRITTIVA
320800060630      * in lingua
320900060630     c                   select
321000060630     c                   when      i45lin = 'I'
321100010626     C                   EVAL      P03DES = '               Spese inoltro ....'
321200060630     c                   when      i45lin = '2'
321300060630     c                   eval      p03des = '               Plus costs .......'
321400060817     c                   when      i45lin = '3'
321500060817     c                   eval      p03des = '               Frais d''envoi ...'
321600060817     c                   when      i45lin = '4'
321700060817     c                   eval      p03des = '          osten für Weiterleitung'
321800060817     c                   endsl
321900010615      *
322000010625      *
322100010723     c                   if        *in80 or curline > 61
322200010627     c                   write     ta49pfin
322300010625     c                   write     ta49pPP
322400010625     c                   write     ta49p02a
322500010625     c                   write     ta49p02
322600010625     c                   setoff                                       80
322700010625     c                   endif
322800010625      *
322900010615     C                   WRITE     TA49P03
323000010615      *
323100010615    2C                   ENDIF
323200010615      *
323300010615    2C                   ENDIF
323400010614     C*
323500010614     C                   ADD       1             K
323600010615    1C                   END
323700110216
323800110216      * se ho stampato 'Altri' torno a loopare per stampare il resto
323900110216     c                   if        savk > 1 and giro = 0
324000110216     c                   eval      k = 1
324100110216     c                   eval      giro = 1
324200110216     c                   goto      rifai
324300110216     c                   endif
324400010615     C*
324500010614     C                   MOVEA     *BLANKS       AI2
324600010615      *
324700010614     C                   ENDSR
324800010625     C*-------------------------------------------------------------------------
324900010615     C*  PRODET  DETTAGLIO INIZIALE X  P R O V I N C I A
325000010625     C*-------------------------------------------------------------------------
325100010615     C     PRODET        BEGSR
325200010615      *
325300010622     C                   Z-ADD     1             K5
325400010615      * cerco il codice tassazione in schiera per TITAD
325500010622     C  N10TADCTS        LOOKUP    CCT(K5)                                35
325600010615      * cerco il codice tassazione in schiera per TITPD
325700010622     C   10TPDCTS        LOOKUP    CCT(K5)                                35
325800010615      *  se non trovato errore
325900010615     C  N35              SETON                                        99
326000010615     C     *IN99         IFEQ      '1'
326100010615     C                   MOVE      MSG(6)        ERRMSG
326200010615     C                   EXSR      STAERR
326300010615      *
326400010615     C                   ELSE
326500010615      *
326600010615      * TITAD
326700010615     C  N10              MOVE      TADORP        CODORP
326800010615      * TITPD
326900010615     C   10              MOVE      TPDORP        CODORP
327000010615      *
327100010622     C                   MOVE      SCT(K5)       CODSIG
327200050204     C                   MOVE      prvdes(k5)    desprv
327300050303     c                   Eval      nostdes = noctdes(k5)
327400010615     C*  PULIZIA SCHIERE PROVINCIA
327500010615     c                   if        not *in10
327600010615     C                   CLEAR                   TCT
327700010615     C                   CLEAR                   TMN
327800010615     C                   CLEAR                   TMX
327900010615     C                   CLEAR                   TIN
328000010615      *
328100010615     c                   else
328200010615      *
328300010615     C                   MOVEA     *ZEROS        TAP
328400010615     C                   MOVEA     *ZEROS        TPN
328500010615     C                   MOVEA     *ZEROS        TPX
328600010615      *
328700010615     C                   endif
328800010615      *
328900010615     C                   END
329000010615      *
329100010615     C                   ENDSR
329200010615     C*-----------------------------------------
329300010615     C*  REGDET  DETTAGLIO INIZIALE X  R E G I O N E
329400010615     C*-----------------------------------------
329500010615     C     REGDET        BEGSR
329600010615      *
329700010622     C                   Z-ADD     1             K5
329800010615      * cerco il codice tassazione in schiera per TITAD
329900010622     C  N10TADCTS        LOOKUP    CCT(K5)                                35
330000010615      * cerco il codice tassazione in schiera per TITPD
330100010622     C   10TPDCTS        LOOKUP    CCT(K5)                                35
330200010615      *  se non trovato errore
330300010615     C  N35              SETON                                        99
330400010615     C     *IN99         IFEQ      '1'
330500010615     C                   MOVE      MSG(6)        ERRMSG
330600010615     C                   EXSR      STAERR
330700010615      *
330800010615     C                   ELSE
330900010615      *
331000010622     C                   MOVE      RCT(K5)       CODREG
331100010615      * per tariffa DPD e linea partenza cliente non è  DPD regione IT diventa
331200010615      * regione EE
331300020312     c                   if        codreg = 'IT' and *in25 and §ogntw <> 'DPD'
331400010615     C                   MOVEL     'EE'          CODREG
331500010615     C                   ENDIF
331600010615      *
331700010622     C                   Z-ADD     1             K5
331800010622     C     CODREG        LOOKUP    CRE(K5)                                35
331900010615     C  N35              SETON                                        99
332000010615     C     *IN99         IFEQ      '1'
332100010615     C                   MOVE      MSG(7)        ERRMSG
332200010615     C                   EXSR      STAERR
332300010615     C                   ELSE
332400010615      *
332500010622     C                   MOVEL     DRE(K5)       DESREG
332600010615      *  PULIZIA SCHIERE REGIONE
332700010615     C                   MOVE      *BLANKS       BIG
332800010615     C                   MOVE      *BLANKS       BMN
332900010615     C                   MOVE      *BLANKS       BMX
333000010615     C                   MOVE      *BLANKS       BSI
333100010615     C                   MOVE      *BLANKS       BIN
333200010615     C                   CLEAR                   BRG
333300010615     C                   MOVE      *BLANKS       WIN
333400010615     C                   MOVE      *BLANKS       WMN
333500010615     C                   MOVE      *BLANKS       WMX
333600010615     C                   MOVE      *BLANKS       WTI
333700010622      *
333800010615     C                   MOVE      *BLANKS       PDE
333900010615     C                   END
334000010615     C                   END
334100010615      *
334200010615     C                   ENDSR
334300010615      *-------------------------------------------------------------------------
334400010615      *  SR_INOLTR  GESTIONA STAMPE INOLTRO SE PER TUTTI UGUALE
334500010615      *-------------------------------------------------------------------------
334600010615     C     SR_INOLTR     BEGSR
334700010615      *
334800010615      * SE TARIFFE DI INOLTRO UGUALI TRA LORO STAMPO
334900010615    1C     STINO         IFNE      'Y'
335000010615      * SE INOLTRO =0 NON STAMPO NULLA
335100010615     C                   Z-ADD     K4            B
335200010615     C                   DO        6             A
335300010615     C                   MOVE      PRO(B)        COM(A)
335400010615     C                   ADD       1             B
335500010615     C                   END
335600010615     C                   MOVEA     COM           CINO42
335700010615      *
335800010615      * SE INOLTRO > 0
335900010615      *
336000010615    2C                   IF        CINO42 <> *ZEROS
336100010615      *
336200010615     C                   Z-ADD     K4            K
336300010615    3C                   DO        6             A
336400010615     C                   Z-ADD     PRO(K)        PAM(A)
336500010615      * SE TARIFFA A VALORE E IMPORTO VALORIZZATO IMPOSTO %
336600010615     C                   IF        PAM(A) > *ZEROS AND *IN87
336700010615     C                   MOVEL     '%'           AI2(A)
336800010615     C                   END
336900010615     C                   ADD       1             K
337000010615    3C                   END
337100010615      * VALORIZZO I CAMPI DI STAMPA
337200010615     C                   CLEAR                   TA49P03
337300010615      *
337400010615     C     PAM(1)        COMP      0                                  61
337500010615     C     PAM(2)        COMP      0                                  62
337600010615     C     PAM(3)        COMP      0                                  63
337700010615     C     PAM(4)        COMP      0                                  64
337800010622     C     PAM(5)        COMP      0                                  65
337900010622     C     PAM(6)        COMP      0                                  66
338000010615      *
338100010615     C                   Z-ADD     PAM(1)        TAR01
338200010615     C                   Z-ADD     PAM(2)        TAR02
338300010615     C                   Z-ADD     PAM(3)        TAR03
338400010615     C                   Z-ADD     PAM(4)        TAR04
338500010615     C                   Z-ADD     PAM(5)        TAR05
338600010615     C                   Z-ADD     PAM(6)        TAR06
338700010615      *
338800010615      * VALORIZZO LA PARTE DESCRITTIVA
338900060630      * in lingua
339000060630     c                   select
339100060630     c                   when      i45lin = 'I'
339200010615     C                   EVAL      P03DES = 'Per località in provincia spese'  +
339300010615     c                             ' di inoltro'
339400060630     c                   when      i45lin = '2'
339500060630     C                   EVAL      P03DES = 'Per town in province forwarding'  +
339600060630     c                             ' costs'
339700060817     c                   when      i45lin = '3'
339800060817     C                   EVAL      P03DES = 'Pour les communes de la province,'+
339900060817     c                             ' frais d''envoi'
340000060817     c                   when      i45lin = '4'
340100060817     C                   EVAL      P03DES = 'Kosten für Weiterleitung-Ort in' +
340200060817     c                             ' Provinz'
340300060630     c                   endsl
340400010615      *
340500010615      * SE TARIFFA A VALORE STAMPO LA %
340600010615     C                   IF        *IN87
340700010615     C                   MOVE      AI2(1)        PER01
340800010615     C                   MOVE      AI2(2)        PER02
340900010615     C                   MOVE      AI2(3)        PER03
341000010615     C                   MOVE      AI2(4)        PER04
341100010615     C                   MOVE      AI2(5)        PER05
341200010615     C                   MOVE      AI2(6)        PER06
341300010615     C                   ENDIF
341400010625      *
341500010723     c                   if        *in80 or curline > 61
341600010627     c                   write     ta49pfin
341700010625     c                   write     ta49pPP
341800010625     c                   write     ta49p02a
341900010625     c                   write     ta49p02
342000010625     c                   setoff                                       80
342100010625     c                   endif
342200010625      *
342300010615      *
342400010615     C                   WRITE     TA49P03
342500010615      *
342600010615    2C                   endif
342700010615      *
342800010615    1C                   endif
342900010615      *
343000010615     C                   CLEAR                   AI2
343100010615      *
343200010615     C                   ENDSR
343300010615      *-------------------------------------------------------------------------
343400010615      *  SR_ARROTO  GESTIONA STAMPE ARROTONDAMENTI
343500010615      *-------------------------------------------------------------------------
343600010615     C     SR_ARROTO     BEGSR
343700010615      *
343800010615     c                   clear                   ta49p05
343900010615      * verifico gli importi degli arrotondamenti
344000010615      *
344100010621    1c                   if        tamarl > 0
344200010615      * verifico se esistono dei valori con i decimali
344300010615     C                   move      tamarf        w0030
344400010615     C                   eval      *in89 =(w0030 > 0)
344500010615      *
344600010621    2c                   if        not *in89
344700010615     C                   move      tamarl        w0030
344800010615     C                   eval      *in89 =(w0030 > 0)
344900010621    2c                   endif
345000010615      *
345100010621    2c                   if        not *in89
345200010615     C                   move      tamaro        w0030
345300010615     C                   eval      *in89 =(w0030 > 0)
345400010621    2c                   endif
345500010615      *
345600010615      * se 89 spento non esistono importi con decimali sposto in camp di comodo
345700010621    2c                   if        not *in89
345800010615     C                   Z-ADD     TAMARO        WARO2
345900010615     C                   Z-ADD     TAMARF        WARF2
346000010615     C                   Z-ADD     TAMARL        WARL2
346100010621    2c                   endif
346200010615      *
346300010615      * se arrotondamenti di 1 in 1 non stampo
346400010615      * se arrotondamento fino a diverso da arrotondamento oltre a
346500010615      *
346600010628    2c                   if        tamarf <> tamaro
346700010615      *
346800010615      * valorizzo i campi di stampa
346900060606      * in lingua
347000060606     c                   select
347100060606     c                   when      i45lin = 'I'
347200010615     c  N89              eval      p05ar1 = 'fino a ' + %editc(Warl2:'4') +
347300010615     c                                      ' di ' + %editc(warf2:'4')     +
347400010628     c                                      ' in ' + %editc(warf2:'4')
347500010615     c   89              eval      p05ar1 = 'fino a ' + %editc(tamarl:'4') +
347600010628     c                                      ' di ' + %editc(tamarf:'4')     +
347700010628     c                                      ' in ' + %editc(tamarf:'4')
347800060606     c                   when      i45lin = '2'
347900060630     c  n89              eval      p05ar1 = 'Up to ' +
348000060630     c                                      %editc(warl2:'4') + ' rounded ' +
348100060630     c                                      'to the nearest ' +
348200060630     c                                      %editc(warf2:'4')
348300060630     c   89              eval      p05ar1 = 'Up to ' +
348400060704     c                                      %editc(tamarl:'4') + ' rounded ' +
348500060630     c                                      'to the nearest ' +
348600060630     c                                      %editc(tamarf:'4')
348700060817     c                   when      i45lin = '3'
348800060817     c  n89              eval      p05ar1 = 'jusq''à  ' + %editc(Warl2:'4') +
348900060817     c                                      ' de ' + %editc(warf2:'4')     +
349000060817     c                                      ' en ' + %editc(warf2:'4')
349100060817     c   89              eval      p05ar1 = 'jusq''à  ' + %editc(tamarl:'4') +
349200060817     c                                      ' de ' + %editc(tamarf:'4')     +
349300060817     c                                      ' en ' + %editc(tamarf:'4')
349400060817     c                   when      i45lin = '4'
349500060817     c  n89              eval      p05ar1 = 'bis       ' + %editc(warl2:'4')  +
349600060817     c                                      ' von ' + %editc(warf2:'4') +
349700060817     c                                      ' auf ' + %editc(warf2:'4')
349800060817     c   89              eval      p05ar1 = 'bis       ' + %editc(tamarl:'4') +
349900060817     c                                      ' von ' + %editc(tamarf:'4')     +
350000060817     c                                      ' auf ' + %editc(tamarf:'4')
350100060606     c                   endsl
350200010615      *
350300010615      * se arrotondamento oltre a è maggiore di 1
350400010615      *
350500010615      * valorizzo i campi di stampa
350600060606      * in lingua
350700060606     c                   select
350800060606     c                   when      i45lin = 'I'
350900010725     c  N89              eval      p05ar2 = 'oltre  ' + '    '        +
351000010615     c                                      ' di ' + %editc(waro2:'4')     +
351100010628     c                                      ' in ' + %editc(waro2:'4')
351200010723     c   89              eval      p05ar2 = 'oltre  ' + '        '         +
351300010628     c                                      ' di ' + %editc(tamaro:'4')     +
351400010628     c                                      ' in ' + %editc(tamaro:'4')
351500060606     c                   when      i45lin = '2'
351600060704     c  n89              eval      p05ar2 = 'over ' + '     '  +
351700060630     c                                      ' to the nearest ' +
351800060630     c                                      %editc(waro2:'4')
351900060704     c   89              eval      p05ar2 = 'over ' + '     '  +
352000060630     c                                      ' to the nearest ' +
352100060630     c                                      %editc(tamaro:'4')
352200060817     c                   when      i45lin = '3'
352300060818     c  n89              eval      p05ar2 = 'au delà ' + '    '        +
352400060817     c                                      ' de ' + %editc(waro2:'4')     +
352500060817     c                                      ' en ' + %editc(waro2:'4')
352600060818     c   89              eval      p05ar2 = 'au delà ' + '        '         +
352700060817     c                                      ' de ' + %editc(tamaro:'4')     +
352800060817     c                                      ' en ' + %editc(tamaro:'4')
352900060817     c                   when      i45lin = '4'
353000060818     c  n89              eval      p05ar2 = 'mehr als' + '      '        +
353100060817     c                                      ' von ' + %editc(waro2:'4')     +
353200060817     c                                      ' auf ' + %editc(waro2:'4')
353300060818     c   89              eval      p05ar2 = 'mehr als' + '      '         +
353400060817     c                                      ' von ' + %editc(tamaro:'4')     +
353500060817     c                                      ' auf ' + %editc(tamaro:'4')
353600060606     c                   endsl
353700010615      *
353800010615
353900010627     c                   exsr      SR_OVER
354000010625      *
354100010615     c                   write     ta49p05
354200010615
354300010628      *
354400010628     C                   ELSE
354500010625      *
354600010625      * valorizzo i campi di stampa
354700060606      * in lingua
354800060606     c                   select
354900060606     c                   when      i45lin = 'I'
355000010628     c  N89              eval      p05ar1 = ' di ' + %editc(Warf2:'4') +
355100010628     c                                      ' in ' + %editc(waro2:'4')
355200010628     c   89              eval      p05ar1 = ' di ' + %editc(tamarf:'4') +
355300010628     c                                      ' in ' + %editc(tamaro:'4')
355400060606     c                   when      i45lin = '2'
355500060703     c  n89              eval      p05ar1 = 'Rounded to the nearest ' +
355600060606     c                                      %editc(warf2:'4')
355700060703     c   89              eval      p05ar1 = 'Rounded to the nearest ' +
355800060606     c                                      %editc(tamarf:'4')
355900060817     c                   when      i45lin = '3'
356000060817     c  n89              eval      p05ar1 = ' de ' + %editc(Warf2:'4') +
356100060817     c                                      ' en ' + %editc(waro2:'4')
356200060817     c   89              eval      p05ar1 = ' de ' + %editc(tamarf:'4') +
356300060817     c                                      ' en ' + %editc(tamaro:'4')
356400060817     c                   when      i45lin = '4'
356500060817     c  n89              eval      p05ar1 = ' von ' + %editc(Warf2:'4') +
356600060817     c                                      ' auf ' + %editc(waro2:'4')
356700060817     c   89              eval      p05ar1 = ' von ' + %editc(tamarf:'4') +
356800060817     c                                      ' auf ' + %editc(tamaro:'4')
356900060606     c                   endsl
357000010625
357100010628     c                   exsr      SR_OVER
357200010625      *
357300010628     c                   write     ta49p05
357400010625      *
357500010628    2c                   endif
357600010621    1c                   endif
357700010621      *
357800010615     C                   ENDSR
357900010615      *-------------------------------------------------------------------------
358000010615      *  SR_STARAP  GESTIONA STAMPE RAPPORTO PESO VOLUME
358100010615      *-------------------------------------------------------------------------
358200010615     C     SR_STARAP     BEGSR
358300010615      *
358400010615     c                   clear                   ta49p06
358500010615      *
358600010615      * Stampo solo se diverso da zero
358700010615      *
358800010615     C                   Z-ADD     K4            B
358900010615     C                   DO        6             A                 2 0
359000010615     C                   MOVE      RPV(B)        CO4(A)
359100010615     C                   ADD       1             B
359200010615     C                   END
359300010615     C                   MOVEA     CO4           WRPV
359400010615      *
359500010615      * Se rapporto peso volume uguale a zero scrivo Volume compreso in tariffa
359600010615    1C                   if        wrpv = *ZEROS
359700010625
359800010627     c                   exsr      SR_OVER
359900010625      *
360000010615     C                   write     ta49p06b
360100010615      *
360200010615     C                   ELSE
360300010615      * carico la schiera da stampare
360400010615     C                   CLEAR                   SPV
360500010615     C                   Z-ADD     K4            B
360600010615     C                   DO        6             A
360700010615     C                   MOVE      RPV(B)        SPV(A)
360800010615     C                   ADD       1             B
360900010615     C                   END
361000010615      * Valizzo il campo in stampa
361100010622     c                   eval      volsca = '      '+ %editc(spv(1):'4') +
361200010801     C                                      '         '      +
361300010622     c                                      %editc(spv(2):'4') +
361400010801     C                                      '         '      +
361500010622     c                                      %editc(spv(3):'4') +
361600010801     C                                      '         '      +
361700010622     c                                      %editc(spv(4):'4') +
361800010801     C                                      '         '      +
361900010622     c                                      %editc(spv(5):'4') +
362000010801     C                                      '         '      +
362100010615     c                                      %editc(spv(6):'4')
362200010625      *
362300010723     c                   if        *in80 or curline > 61
362400010627     c                   write     ta49pfin
362500010625     c                   write     ta49pPP
362600010625     c                   write     ta49p02a
362700010625     c                   write     ta49p02
362800010625     c                   setoff                                       80
362900010625     c                   endif
363000010625      *
363100010615     C                   write     ta49p06
363200010615    1C                   ENDIF
363300010615      *
363400010615      *
363500010615     C                   ENDSR
363600010615     C*
363700010618      *-------------------------------------------------------------------------
363800010618      *  SR_STATAM  STAMPA I DATI CHE SI TROVANO IN TESTATA TARIFFA
363900010618      *-------------------------------------------------------------------------
364000010618     C     SR_STATAM     BEGSR
364100010618      *
364200010618      * se offerta valorizzo con il numero della visita
364300010621    1c                   if        *in98
364400010618     c                   eval      p07ksc = visksc
364500010621     c                   movel     tamksc        p07nrv
364600010621     c                   move      tamksc        p07prv
364700091117     c                   if        $trattativa
364800091117     c                   eval      p07ntra = tamksc
364900091117     c                   endif
365000010618      *
365100010618     c                   else
365200010618      * tariffa
365300010618     c                   eval      p07ksc = tamksc
365400010621    1c                   endif
365500010618      *
365600091117     c                   if        $trattativa
365700091117     c                   eval      ptratta =
365800091117     c                             '(codice ' +
365900091117     c                             %editc(tamctr:'X') + '/' +
366000091117     c                             %subst(%editc(tamprg:'X'):3:1) + ')'
366100091117     c                   endif
366200091117     c                   if        not $trattativa
366300091117     c                   eval      ptariffa =
366400091117     c                             '(codice ' +
366500091117     c                             %editc(tamctr:'X') + ')'
366600091117     c                   endif
366700010625
366800010627     c                   exsr      SR_OVER
366900010625      *
367000010618     c                   write     ta49p07
367100010626      * se applicazione tariffa diversa dal normale stampo
367200010626     c                   if        §taf02 = 'S'
367300010626
367400010627     c                   exsr      SR_OVER
367500010626      *
367600010626     c                   write     ta49p07s
367700010626     c                   endif
367800010626      *
367900010627     c                   exsr      SR_OVER
368000010626      *
368100010626     c                   if        §taf02 = 'G'
368200010626     c                   write     ta49p07g
368300010626     c                   endif
368400010618      *
368500010618      * se tariffa DPD stampo alcune diciture fisse solo per queste tariffe
368600010618      *
368700010621    1c                   if        *in25
368800010618      * recupero il peso massimo consentito per la DPD
368900010618     c                   movel     '3I'          cod
369000010618     c                   movel(p)  'DPD'         key
369100010618      *
369200010618     c     tabkey        chain     tabel00f
369300010621    2c                   if        %found(tabel00f)
369400010618     c                   movel     tbluni        ds3idp
369500010618     c                   else
369600010618     c                   clear                   ds3idp
369700010621    2c                   endif
369800010618      * preparo il campo in stampa
369900060630      * in lingua
370000060630     c                   select
370100060630     c                   when      i45lin = 'I'
370200010618     c                   eval      com08a = 'MAX PESO COLLO KG.' +
370300010618     c                             %trim(%editc(§3ipkd:'2')) +
370400011114     c                             '- MAX VOLUME COLLO MC 0,25'
370500010618     c                   eval      com08B = 'La presente tariffa è da intender'+
370600010618     c                             'si a collo sul peso lordo reale (max ' +
370700010618     c                             %trim(%editc(§3ipkd:'2')) +
370800010618     c                             ').Nessun addebito sul volume.'
370900060630     c                   when      i45lin = '2'
371000060630     c                   eval      com08a = 'MAX PARCEL WIGHT' +
371100060630     c                             %trim(%editc(§3ipkd:'2')) + ' KG.' +
371200060817     c                             '- MAX PARCEL VOLUME 0,25 M³'
371300060630     c                   eval      com08B = 'This tariff is understood to be '+
371400060630     c                             'per parcel according to the actual gross '+
371500060630     c                             'weight (max ' +
371600060630     c                             %trim(%editc(§3ipkd:'2')) +
371700060630     c                             ').No charge to volume.'
371800060817     c                   when      i45lin = '3'
371900060817     c                   eval      com08a = 'POIDS MAXI COLIS' +
372000060817     c                             %trim(%editc(§3ipkd:'2')) + ' KG.' +
372100060817     c                             '- VOLUME MAXI COLIS 0,25 M³'
372200060817     c                   eval      com08B = 'Le présent tarif s''entend par '+
372300060817     c                             'colis sur le poids brut réel ('+
372400060817     c                             %trim(%editc(§3ipkd:'2')) +
372500060817     c                             ' maxi).Aucune facturation sur le volume.'
372600060817     c                   when      i45lin = '4'
372700060817     c                   eval      com08a = 'PACKSTÜCK-HÖCHSTGEWICHT KG.' +
372800060817     c                             %trim(%editc(§3ipkd:'2')) +
372900060817     c                             '- MAX. PACKSTÜCKVOLUMEN 0,25 KUBIKMETER'
373000060817     c                   eval      com08B = 'Der vorliegende Preis versteht '+
373100060817     c                             'sich pro Packstück und realem ' +
373200060817     c                             'Bruttogewicht (max.' +
373300060817     c                             %trim(%editc(§3ipkd:'2')) +
373400060817     c                             ').Für das Volumen werden keine Kosten ' +
373500060817     c                             'berechnet.'
373600060630     c                   endsl
373700010625
373800110119     c                   Z-Add     curline       totrig
373900110119     c                   Add       5             totrig
374000110119     c                   ExSr      Sr_Overpiu
374100010625
374200010618     c                   write     ta49p08
374300010621    1c                   endif
374400010618      * validità tariffa
374500010625
374600010627     c                   exsr      SR_OVER
374700010625
374800010626      * verifico se devo stampare "per:" solo se c'è o il numero spedizioni mini
374900010626      * me o massime o se c'è uno dei sui volumi auotmatici
375000010626     c                   if        tamlrc > 0 or tamlas > 0 or tamtin <> ' '
375100010626     c                   seton                                        26
375200010626     c                   endif
375300010618     c                   write     ta49p09
375400010618      * verifico se stampare spedizioni minime e massime
375500010621    1c                   if        tamlrc > 0 or tamlas > 0
375600010618     c                   eval      comlrc = tamlrc
375700010618     c                   eval      comlas = tamlas
375800010625
375900010627     c                   exsr      SR_OVER
376000010625
376100010618     c                   write     ta49p09a
376200010621    1c                   endif
376300010618      * verifico se devo stampare volume automatico
376400010621    1c                   if        tamtin = 'K'
376500010618      * volume automatico a  kg
376600010618     c                   z-add     tamkpm        comkpm
376700010621    1c                   endif
376800010618      *
376900010621    1c                   if        tamtin = 'C'
377000010618      * volume a collo
377100010618     c                   z-add     tamkpm        comckm
377200010621    1c                   endif
377300010618      * stampo volume automatico
377400010621    1c                   if        tamtin <> ' '
377500010625
377600010627     c                   exsr      SR_OVER
377700010625
377800010626    1c                   if        tamtin = 'K'
377900010618     c                   write     ta49p09b
378000010723     c                   write     ta49p09s
378100010626     c                   else
378200010626     c                   write     ta49p09c
378300010723     c                   write     ta49p09s
378400010626     c                   endif
378500010723      * se non c'è volume stampo una riga vuota prima delle note
378600010723     c                   else
378700010723     c                   write     ta49p09s
378800010723      *
378900010621    1c                   endif
379000010618      * stampo le note
379100010622     C                   Z-ADD     1             X
379200010622      *
379300010621    1c                   do        4             x
379400010618     c                   if        note(x) <> *blanks
379500010618     c                   movel     note(x)       comnot
379600010625
379700010627     c                   exsr      SR_OVER
379800010625
379900010618     c                   write     ta49p10
380000010618     c                   endif
380100010621    1c                   enddo
380200010618      * se c'è stampo l'addizionale di gestione
380300010621    1c                   if        tampag > 0
380400060606      * in lingua
380500060606     c                   select
380600060606     c                   when      i45lin = 'I'
380700041129     c                   Eval      comdag = 'Addizionale di gestione -
380800041129     c                             anni precedenti:'
380900060606     c                   when      i45lin = '2'
381000060606     c                   eval      comdag = 'Surcharge for previous -
381100060606     c                             years'' administration:'
381200060817     c                   when      i45lin = '3'
381300060817     c                   eval      comdag = 'Complémentaires de gestion -
381400060817     c                             années précédentes:'
381500060817     c                   when      i45lin = '4'
381600060817     c                   eval      comdag = 'Aufschlag für Bearbeitung -
381700060818     c                             von Vorjahren:'
381800060606     c                   endsl
381900041129     c                   eval      comcag = tampag
382000010626     c                   seton                                        27
382100010621    1c                   endif
382200041129      * se c'è stampo l'addizionale di gestione anno corrente
382300041129    1c                   if        tamppa > 0
382400041129     c                   If        *In27
382500060606      * in lingua
382600060606     c                   select
382700060606     c                   when      i45lin = 'I'
382800041129     c                   Eval      comdag1 = 'Addizionale di gestione -
382900041129     c                             corrente ......:'
383000060606     c                   when      i45lin = '2'
383100060606     c                   eval      comdag1 = 'Surcharge for current -
383200060818     c                             administration........:'
383300060817     c                   when      i45lin = '3'
383400060817     c                   eval      comdag1 = 'Complémentaire de gestion -
383500060818     c                             courante..........:'
383600060817     c                   when      i45lin = '4'
383700060817     c                   eval      comdag1 = 'Aufschlag für Bearbeitung -
383800060817     c                             des laufenden Jahres:'
383900060606     c                   endsl
384000041129     c                   eval      comcag1 = tamppa
384100041129     c                   seton                                        29
384200041129     c                   else
384300060606      * in lingua
384400060606     c                   select
384500060606     c                   when      i45lin = 'I'
384600041129     c                   Eval      comdag = 'Addizionale di gestione -
384700060818     c                             corrente:'
384800060606     c                   when      i45lin = '2'
384900060606     c                   eval      comdag = 'Surcharge for current -
385000060818     c                             administration:'
385100060817     c                   when      i45lin = '3'
385200060817     c                   eval      comdag = 'Complémentaire de gestion -
385300060818     c                             courante:'
385400060817     c                   when      i45lin = '4'
385500060817     c                   eval      comdag = 'Aufschlag für Bearbeitung -
385600060817     c                             des laufenden Jahres:'
385700060606     c                   endsl
385800041129     c                   eval      comcag = tamppa
385900041129     c                   seton                                        27
386000041129    1c                   endif
386100041129    1c                   endif
386200010626      *
386300010626      * stampo commenti prima dele tariffe particolari
386400010626
386500041220     c                   Add       3             curline
386600010627     c                   exsr      SR_OVER
386700010626
386800010626     c                   write     ta49p11
386900031119      * se c'è istat stampo
387000101015     c                   if        tamrct >  0
387100031119      * recupero la data di decorrenza
387200031119      *
387300101015     c     *iso          movel     dia(tamrct)   data_EUR
387400050527     C     *eur          movel     DATA_EUR      dtaist
387500031119      *
387600031119      * controllo se % diversa da 100
387700031119     c                   eval      *in11 = (tampri <> 100)
387800031119      * stampo
387900031119
388000150317     c                   Add       4             curline
388100031119     c                   exsr      SR_OVER
388200031119
388300031119     c                   write     ta49pist
388400031119      *
388500031119     c                   endif
388600021205      * stampo riga vuota
388700021205     c                   write     ta49p09s
388800010618      *
388900010618     c                   ENDSR
389000010618      *
389100010618      *-------------------------------------------------------------------------
389200010618      *  SR_STATPT  STAMPA TARIFFE PARTICOLARI
389300010618      *-------------------------------------------------------------------------
389400010618     C     SR_STATPT     BEGSR
389500010622      * accendo indicatore tariffe particolari
389600010622     c                   seton                                        10
389700021204
389800021204      * Pulisco le sk per stampa tariffe particolari su + righe
389900021204     c                   Clear                   Tpta
390000021204     c                   Clear                   Tptb
390100021204     c                   Clear                   Tptc
390200021204     c                   Clear                   Atp1
390300021204     c                   Clear                   Atpc
390400060307      * pulisco campo di comodo
390500060307     c                   clear                   savapl
390600021204
390700020109      * leggo la schiera ordinata delle tariffe particolari
390800020109      *
390900020109     c                   do        50            j
391000020109      * se valorizzata
391100020109     c                   if        cp(j) <> *blank
391200020109      * aggancio la tabella relativa le tariffe particolari
391300020109     c                   setoff                                       99
391400020109     c                   z-add     1             y
391500020109     c     cp(j)         lookup    ftc(y)                                 31
391600020109      * verifico se è una tariffa particolare da stampare  (Posta no Posta)
391700020109     c                   if        not *in31
391800020109     c                   seton                                        99
391900020109     c                   else
392000020109      *
392100020109     c                   endif
392200020109      *
392300020109     c                   SELECT
392400020109      * se tariffa stampabile (99 spento)
392500020109     c                   when      *in99
392600020109      * se è una tariffa particolare che vien stampata in modo standard
392700020109     c                   when      tst(y) = ' '
392800020109     c                   EXSR      SR_TAPARSTD
392900020109      * se è una tariffa particolare che non vien stampata in modo standard
393000020109     c                   when      tst(y) = 'N'
393100020109      * verifico che tariffa particolare è
393200020109      * elavazione limite risarcibile
393300020109     c                   if        ftc(y) = 'R'
393400020109     C                   EXSR      SR_ELLIRIS
393500020109     c                   endif
393600060301      * mandato assicurativo AC PLUS
393700020109     c                   if        ftc(y) = 'C'
393800020109     C                   EXSR      SR_MANDATO
393900020109     c                   endif
394000060301      * mandato assicurativo AC BASE
394100060301     c                   if        ftc(y) = 'd'
394200060301     c                   exsr      sr_acbase
394300060301     c                   endif
394400060816      * fuel surcharge
394500060816     c                   if        ftc(y) = 'f'
394600060816     c                   exsr      sr_fuel
394700060816     c                   endif
394800020109      *
394900020109     c                   ENDSL
395000020109      *
395100020109     c                   endif
395200020109      *
395300020109     c                   enddo
395400020109      *
395500010618     c                   ENDSR
395600021204
395700021204      *-------------------------------------------------------------------------
395800021204      *  Sr_StaTpt1   Stampa tariffe particolari da sk unica
395900021204      *-------------------------------------------------------------------------
396000021204     c     Sr_StaTpt1    BegSr
396100021204
396200021204     c                   Clear                   Conta
396300021204     c                   Eval      WokTp = *Off
396400021205      ***
396500021204      * Stampo prima le tariffe particolari con solo l'importo
396600021204Do  1c                   Do        50            Atp1
396700021204     c                   If        Tpta(Atp1) = *Blanks
396800021204     c                   Leave
396900021204     c                   EndIf
397000021204     c                   Eval      Woktp = *On
397100021204      * due tariffe per riga
397200021204If  2c                   If        Conta = *Zeros
397300021204     c                   Clear                   Comstda2
397400021204     c                   Clear                   Comstdb2
397500021204     c                   Movel(p)  Tpta(Atp1)    Comstda1
397600021204     c                   Movel(p)  Tptb(Atp1)    Comstdb1
397700021204     c                   Add       1             Conta
397800021204   x2c                   Else
397900021204     c                   Movel(p)  Tpta(Atp1)    Comstda2
398000021204     c                   Movel(p)  Tptb(Atp1)    Comstdb2
398100021204     c                   ExSr      Sr_Over
398200021204     c                   Write     Ta49pstd1
398300021204     c                   Clear                   Conta
398400021204    2c                   EndIf
398500021204
398600021204    1c                   EndDo
398700021204      * Stampo ultima riga non completa
398800021204     c                   If        WokTp = *On and Conta <> *Zeros
398900021204     c                   ExSr      Sr_Over
399000021204     c                   Write     Ta49pstd1
399100021204     c                   EndIf
399200021205      * stampo riga vuota
399300021205     c                   write     ta49p09s
399400021204
399500021204     c                   Eval      WokTp = *Off
399600021205     c                   Z-Add     *Zeros        CntList
399700021205      ***
399800021205      * Stampo per ultime le tariffe particolari "al costo"
399900021204Do  1c                   Do        50            Atpc
400000021205      * Stampo ultima riga
400100021204     c                   If        Tptc(Atpc) = *Blanks
400200021205     c                   Add       1             CntList
400300060703     c                   select
400400060703     c                   when      i45lin = 'I'
400500021205     c                   Eval      Comstdc1 = ': al costo'
400600060703     c                   when      i45lin = '2'
400700060703     c                   Eval      Comstdc1 = ': at cost'
400800060817     c                   when      i45lin = '3'
400900060817     c                   Eval      Comstdc1 = ': au coût'
401000060817     c                   when      i45lin = '4'
401100060817     c                   Eval      Comstdc1 = ':zum Preis'
401200060703     c                   endsl
401300021205     c                   ExSr      StpRiga
401400021204     c                   Leave
401500021204     c                   EndIf
401600021204
401700021205     c                   If        %Len(%Trim(Comstdc)) + 1 +
401800021205     c                             %len(%trim(Tptc(Atpc))) + 1 <= Lungh
401900021205     c                   ExSr      CrtRiga
402000021205     c                   Else
402100021205     c                   Add       1             CntList
402200021205     c                   Clear                   Comstdc1
402300021205     c                   ExSr      StpRiga
402400021205     c                   ExSr      CrtRiga
402500021205     c                   EndIf
402600021204
402700021204    1c                   EndDo
402800021204
402900021204     c                   EndSr
403000021205      *
403100021205      *-------------------------------------------------------------------------
403200021205      * CrtRiga - Routine di creazione riga tar.particolari "al costo"
403300021205      *-------------------------------------------------------------------------
403400021205     c     CrtRiga       BegSr
403500021205
403600021205     c                   If        Comstdc = *Blanks
403700021205     c                   Eval      Comstdc = %Trim(Comstdc) + %Trim(Tptc(Atpc))
403800021205     C                   Else
403900021205     c                   Eval      Comstdc = %Trim(Comstdc) + ','
404000021205     c                                       + %Trim(Tptc(Atpc))
404100021205     c                   EndIf
404200021205
404300021205     c                   EndSr
404400021205      *
404500021205      *-------------------------------------------------------------------------
404600021205      * StpRiga - Routine di stampa riga tar.particolari "al costo"
404700021205      *-------------------------------------------------------------------------
404800021205     c     StpRiga       BegSr
404900021205
405000021205     c                   If        Comstdc <> *Blanks
405100021205     c                   Eval      TxtInput = %trim(Comstdc)
405200021205     c                   If        (Cntlist > 0) and
405300021205     c                             (%len(%Trim(TxtInput)) > 75)
405400021205     c                   ExSr      ExeGiust
405500021205     c                   If        Not %error
405600021205     c                   Eval      Comstdc = TxtGiust
405700021205     c                   Else
405800021205     c                   Eval      Comstdc = TxtInput
405900021205     c                   EndIf
406000021205     c                   Else
406100021205     c                   Eval      Comstdc = TxtInput
406200021205     c                   EndIf
406300021205     c                   ExSr      Sr_Over
406400021205     c                   Write     Ta49pstdc
406500021205     c                   Clear                   Comstdc
406600021205     c                   EndIf
406700021205
406800021205     c                   EndSr
406900021205      *
407000021205      *-------------------------------------------------------------------------
407100021205      * EXEGIUST - Routine di "GIUSTIFICAZIONE" testo
407200021205      *-------------------------------------------------------------------------
407300021205     c     ExeGiust      BegSr
407400021205
407500021205     c                   Call(e)   'XGIUST'
407600021205     c                   Parm                    TxtInput
407700021205     c                   Parm                    Lungh
407800021205     c                   Parm                    TxtGiust
407900021205     c                   Parm                    TxtResto
408000021205
408100021205     c                   EndSr
408200010625      *
408300010625      *-------------------------------------------------------------------------
408400010625      *  SR_STATGC  STAMPA TARIFFE GIACENZA
408500010625      *-------------------------------------------------------------------------
408600010625     C     SR_STATGC     BEGSR
408700010625      *
408800010625     c                   if        *in98
408900010625     c     ds1ktm        chain     tiogc01l                           99
409000010625      *
409100010625     c                   else
409200010625      *
409300010625     c     ds1ktm        chain     titgc01l                           99
409400010625     c                   endif
409500010625      *
409600010625     c                   if        *in99 or tgcatb = 'A'
409700010625     c                   goto      endstatgc
409800010625     c                   endif
409900010625      * rapporto peso volume
410000010625     c                   eval      comgpv = tgcrpv * 100
410100010625      * preaparo i campi in stampa
410200010625     C                   z-add     tgcsgd        comsgd
410300010928     C                   z-add     tgcsgv        comsgv               28
410400010625     C                   z-add     tgcsgp        comsgp
410500010625     C                   z-add     tgcsgr        comsgr
410600010625     C                   z-add     tgcstm        comstm
410700010625     C                   z-add     tgcst1        comst1
410800010625     C                   z-add     tgcsg1        comsg1
410900010625     C                   z-add     tgcst2        comst2
411000010928      *
411100010928     C
411200010807      * se giorni di rientro sono a zero li recupero dalla tabella "2G"
411300010807      * in base al tipo tariffa
411400010807     c                   if        tgcggr = *zeros
411500010807      * tariffa italia
411600010807     c                   if        tamfie = 'I'
411700010807     c                   z-add     §2gggl        tgcggr
411800010807      * se DPD
411900010807     c   25              z-add     §2ggdp        tgcggr
412000010807      *
412100010807     c                   else
412200010807      * tariffa estera
412300010807     c                   z-add     §2ggre        tgcggr
412400010807      *
412500010807     c                   endif
412600010807      *
412700010807     c                   endif
412800010807      *
412900010807     C                   z-add     tgcggr        comggr
413000010625      *
413100010725     c                   if        *in80 or curline > 55
413200010725     c                   write     ta49pfin
413300010725     c                   write     ta49pPP
413400010725     c                   setoff                                       80
413500010725     c                   endif
413600010625      *
413700010625      *
413800010625     c                   write     ta49pgi
413900010625      * verifico se c'è la franchigia
414000010625     c                   if        tgcsgf > 0
414100010625     c                   z-add     tgcsgf        comsgf
414200010625      * verifico il falg applicazione franchigia
414300060703      * e stampo in lingua
414400060703     c                   select
414500060703     c                   when      i45lin = 'I'
414600010625     c                   if        tgcfaf = '1'
414700010625     c                   eval      comapf = cspe1
414800010625     c                   endif
414900010625     c                   if        tgcfaf = '2'
415000010625     c                   eval      comapf = cspe2
415100010625     c                   endif
415200060118     c                   if        tgcfaf = '3'
415300060118     c                   eval      comapf = cspe3
415400060118     c                   endif
415500060118      *
415600060703     c                   when      i45lin = '2'
415700060703     c                   if        tgcfaf = '1'
415800060703     c                   eval      comapf = cspe12
415900060703     c                   endif
416000060703     c                   if        tgcfaf = '2'
416100060703     c                   eval      comapf = cspe22
416200060703     c                   endif
416300060703     c                   if        tgcfaf = '3'
416400060703     c                   eval      comapf = cspe32
416500060703     c                   endif
416600060817      *
416700060817     c                   when      i45lin = '3'
416800060817     c                   if        tgcfaf = '1'
416900060817     c                   eval      comapf = cspe13
417000060817     c                   endif
417100060817     c                   if        tgcfaf = '2'
417200060817     c                   eval      comapf = cspe23
417300060817     c                   endif
417400060817     c                   if        tgcfaf = '3'
417500060817     c                   eval      comapf = cspe33
417600060817     c                   endif
417700060817      *
417800060817     c                   when      i45lin = '4'
417900060817     c                   if        tgcfaf = '1'
418000060817     c                   eval      comapf = cspe14
418100060817     c                   endif
418200060817     c                   if        tgcfaf = '2'
418300060817     c                   eval      comapf = cspe24
418400060817     c                   endif
418500060817     c                   if        tgcfaf = '3'
418600060817     c                   eval      comapf = cspe34
418700060817     c                   endif
418800060703     c                   endsl
418900010625      *
419000010627     c                   exsr      SR_OVER
419100010625      *
419200010625     c                   write     ta49pgif
419300010625      *
419400010625     c                   endif
419500031008
419600031008     c                   Add       2             curline
419700031008     c                   exsr      SR_OVER
419800031008     c                   write     ta49pgi1
419900010625      *
420000010625     c     endstatgc     endsr
420100010625      *
420200010618      *-------------------------------------------------------------------------
420300020109      *  SR_TAPARSTD STAMPA DATI DELLA TARIFFA PARTICOLARE STANDARD
420400010618      *-------------------------------------------------------------------------
420500020109     C     SR_TAPARSTD   BEGSR
420600010618      *
420700061207      * indicatore e campo di comodo per tariffa Fuel a scaglioni
420800061207      * sto stampando le tariffe particolari srandard ruindi pulisco il campo
420900061207      * e spengo l'indicatore
421000061207     c                   clear                   desumf
421100061207     c                   eval      *in12 = *off
421200010618      *
421300020109     C                   movel(P)  ftc(y)        savftc
421400020109     c                   movel(p)  edtc(y)       comstd
421500020109      * se si tratta della tariffa particolare isola accendo
421600020109     c                   if        ftc(y) = 'I'
421700020109     C                   SETON                                        06
421800020109     c                   endif
421900010628      *
422000061122     C  n98KTPT          CHAIN     TITPT01l
422100061122     c   98ktpt          chain     tiopt01l
422200010618      * se non trovato vado a fine senza stampare
422300061122     c                   if        not %found or tptatb = 'A'
422400020109      * verifico se devo stampare al costo
422500020109     c                   if        edac(y) = 'S'
422600021204
422700021204      * se non devo stampare righe aggiuntive salvo la tariffa particolare
422800021204      * in una sk che stamperò alla fine di tutte le tariffe particolari
422900021204If  3c                   If        Esud(y) <> 'S'
423000021204     c                   Add       1             Atpc
423100021204     c                   Movel     Comstd        Tptc(Atpc)
423200021204
423300021204      * se devo stampare righe aggiuntive stampo anche la tariffa particolare
423400021204   x3c                   Else
423500140527      * se sto stampando la tariffa particolare m stampo la seguente
423600140527      * dicitura
423700151203      * Bocchi 3/12/15 mi ha detto di togliere la dicitura per questa tariffa
423800151203      * e lasciarla al costo come le altre. fare suibito ed inviare senza
423900151203      * dire nulla a nessuno
424000151203      * pulisco dalla dicitura GRATIS il programma ma è bastato togliere
424100151203      * il flag di dicitura particolare nella tabella SP tariffa particolare "m"
424200151203      * che non stampa più
424300151203     c****               If        ftc(y) = 'm'
424400151203     c****               select
424500151203     c****               when      i45lin = 'I'
424600151203     c****               eval      comdstd = Cost_m_gratis
424700151203     c****               when      i45lin = '2'
424800151203     c****               eval      comdstd = Cost_m_gratisI
424900151203     c****               when      i45lin = '3'
425000151203     c****               eval      comdstd = Cost_m_gratisF
425100151203     c****               when      i45lin = '4'
425200151203     c****               eval      comdstd = Cost_m_gratisD
425300151203     c****               endsl
425400151203     c****               else
425500060703     c                   select
425600060703     c                   when      i45lin = 'I'
425700020109     c                   eval      comdstd = ': al costo       '
425800060703     c                   when      i45lin = '2'
425900060703     c                   eval      comdstd = ': at cost        '
426000060817     c                   when      i45lin = '3'
426100140528     c                   eval      comdstd = ': au coût        '
426200060817     c                   when      i45lin = '4'
426300140528     c                   eval      comdstd = ': zum Preis      '
426400060703     c                   endsl
426500151203     c*****              endif
426600020109      *
426700020109     c                   exsr      SR_OVER
426800020109      *
426900020109     c                   write     ta49pstd
427000020109      * vado a stampare righe aggiuntive se richieste
427100020109     c                   goto      sudtpstd
427200021204    3c                   EndIf
427300021204
427400020109     c                   else
427500020109      * altrimenti vado a fine e non stampo nulla
427600020109     c                   goto      endtpstd
427700010618     c                   endif
427800010618      *
427900020109     c                   endif
428000010618      * cerco se ha record validi con tariffa o minimo > 0
428100010618     c                   EXSR      RICREC
428200010618      * per 04 acceso stampo nessun addebito
428300010618     c                   if        *in04
428400020109     c                   goto      endtpstd
428500010618     c                   endif
428600010625      *
428700010625      * per 04 spento verifico quanti record dettaglio tariffario esistono
428800010625      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
428900010625     c                   if        recval = 1
429000010625     C                   exsr      sr_gesunico
429100021204
429200021204      * se la tariffa particolare è di 1 riga con solo l'importo
429300021204      * salvo la tariffa particolare in una sk che stamperò alla fine di
429400021204      * tutte le tariffe particolari prima della sk al costo
429500021204If  2c                   If        Esud(y) <> 'S' and
429600021204     c                             W_Tarparest = *Blanks and
429700021204     c                             oltre = *Zeros and TpdMin = *Zeros and
429800021204     c                             TpdMax = *Zeros and TptTpg <> '0' and
429900021204     c                             TptRpv = *Zeros and TpTArl = *Zeros
430000021204     c                   Add       1             Atp1
430100021204     c                   Z-Add     TpdItr        Witr
430200021204     c                   Eval      Tpta(Atp1) = Comstd
430300021204      * no percentuale
430400021204If  3c                   If        W_Per = ' '
430500021204     c                   Eval      Tptb(Atp1) = ': ' +
430600021204     c                                          Wrkdestpg + ' ' +
430700021204     c                                          %Editc(Witr:'3')
430800021204      * sì percentuale
430900021204   x3c                   Else
431000021204     c                   Eval      Tptb(Atp1) = ': ' +
431100021204     c                                          Wrkdestpg + ' ' +
431200021204     c                                          StpPer + ' % '
431300021204    3c                   EndIf
431400021204
431500021204      * se devo stampare righe aggiuntive o l'estensione della tariffa
431600021204      * particolare stampo anche la tariffa particolare
431700021204   x2c                   Else
431800010625      *
431900020109     c                   eval      comdstd = w_tarpar
432000010625      *
432100010627     c                   exsr      SR_OVER
432200010625      *
432300020109     c                   write     ta49pstd
432400010723      *
432500010723     c                   exsr      findet
432600021204    2c                   EndIf
432700010625     c                   else
432800010618      *
432900010618      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
433000010618      *
433100010625     c                   exsr      sr_gestpd
433200010625      *
433300010625     c                   endif
433400020109      *
433500020109     c     sudtpstd      tag
433600020109      *
433700020109      * verifico se devo stampare righe ulteriori
433800020109      *
433900020109     c                   if        esud(y) = 'S'
434000020109     c                   exsr      SR_OVER
434100091211      *
434200091211      * se tariffa particolare Bartolini 10:30
434300091211      *
434400091211     c                   if        ftc(y) = 'h'
434500160623     c                   exsr      RigheH1030
434600100107     c                   Z-Add     curline       totrig
434700100107     c                   Add       6             totrig
434800100107     c                   ExSr      Sr_Overpiu
434900091211     c                   write     ta49psh2
435000091211     c                   endif
435100020109      *
435200091211      * se tariffa particolare Priority
435300020109      *
435400020109     c                   if        ftc(y) = 'E'
435500020109     c                   write     ta49pse2
435600020109     c                   endif
435700020109      *
435800020109      * se tariffa particolare ritiro
435900020109      *
436000020109     c                   if        ftc(y) = 'U'
436100020109     c                   write     ta49por2
436200020109     c                   endif
436300020109      *
436400020109      * se tariffa particolare provvigione vettore
436500020109      *
436600020109     c                   if        ftc(y) = 'V'
436700020109      *
436800060606      * in lingua
436900060606     c                   select
437000060606     c                   when      i45lin = 'I'
437100020109     c                   eval      comint = 'Vettore'
437200060606     c                   when      i45lin = '2'
437300060606     c                   eval      comint = 'Carrier'
437400060817     c                   when      i45lin = '3'
437500060904     c                   eval      comint = ' du Transporteur'
437600060817     c                   when      i45lin = '4'
437700060817     c                   eval      comint = 'Frachtführe'
437800060606     c                   endsl
437900020109     c                   write     ta49ppc1
438000020109      *
438100020109      *  stampo le condizioni di rimborso solo se tipo pagamento che necessita
438200020109      * codice abi cab
438300020109     c                   if        §4uabi = 'S'
438400020109     c                   exsr      SR_OVER
438500020109     c                   write     ta49ppc2
438600020109     C                   endif
438700020109      *
438800020109     c                   endif
438900020109      *
439000020109      * se tariffa particolare provvigione mittente
439100020109      *
439200020109     c                   if        ftc(y) = 'W'
439300060606      * in lingua
439400060606     c                   select
439500060606     c                   when      i45lin = 'I'
439600020109     c                   eval      comint = 'Mittente'
439700060606     c                   when      i45lin = '2'
439800060606     c                   eval      comint = 'Sender'
439900060817     c                   when      i45lin = '3'
440000060904     c                   eval      comint = 'de l''Expéditeur'
440100060817     c                   when      i45lin = '4'
440200060817     c                   eval      comint = 'Absender'
440300060606     c                   endsl
440400020109     c                   write     ta49ppc1
440500020109     c                   endif
440600020109      *
440700020109     c                   endif
440800010618      *
440900020109     c                   setoff                                       06
441000020109      *
441100020109     C     endtpstd      endsr
441200010625      *-------------------------------------------------------------------------
441300010625      *  SR_ELLIRIS STAMPA DATI DELLA TARIFFA PARTICOLARE ELEVAZ.LIMITE RISARC.
441400010625      *-------------------------------------------------------------------------
441500010625     C     SR_ELLIRIS    BEGSR
441600010625      *
441700010625     C                   movel     'R '          savftc
441800010628      *
441900061122     C  n98KTPT          CHAIN     TITPT01l
442000061122     c   98ktpt          chain     tiopt01l
442100010625      * se non trovato vado a fine e non stampo nulla
442200061122     c                   if        not %found
442300010625     c                   goto      endelliris
442400010625     c                   endif
442500010625      * se trovato ma annullato vado a fine  e non stampo nulla
442600010625     c                   if        tptatb = 'A'
442700010625     c                   goto      endelliris
442800010625     c                   endif
442900010627      * se esiste stampo la scritta assicurazioni merci ed accendo indicatore
443000010627      * che mi segnala di aver stampato questa riga per non farlo dopo
443100010627      *
443200010627     c                   exsr      SR_OVER
443300010627      *
443400010627     c                   write     ta49PTAM
443500010627     C                   SETON                                        81
443600010625      * recupero i valori standard dalla tabella GEI
443700010625     C                   CLEAR                   DGEI
443800010625     C                   MOVEL     'C'           T02MOD
443900010625     C                   MOVEL     KNSIF         T02SIF
444000010625     C                   MOVEL     'GEI'         T02COD
444100011001      *
444200010625     C                   MOVEL     §TADIV        T02KE1
444300010625      *
444400010625     C                   CALL      'TIBS02R'
444500010625     C                   PARM                    KPJBA
444600160623     C                   PARM                    tibs02ds
444700010625      *
444800010625     C     T02ERR        IFEQ      *BLANKS
444900010625     C                   MOVEL     T02UNI        DGEI
445000010625     C                   ENDIF
445100010625     C*
445200010625      * verifico se limite risarcibile > di quello standard
445300010625     c                   if        tptvlm > §gelrp
445400010625     c                   seton                                        22
445500010625     c                   z-add     tptvlm        comelr
445600010625     c                   z-add     §gelrp        comlrp
445700010625      *
445800010625      * decodifico il flag valore merce
445900010625      *
446000010625     C                   movel     'TR'          COD
446100010625     C                   movel(P)  tptfvm        KEY
446200060605     c     tabkeyl       chain     tabel00f
446300010625      *
446400010625     c                   if        %found(tabel00f)
446500010625     c                   movel     TBLUNI        DSTR
446600010625     c                   else
446700010625     c                   clear                   DSTR
446800010625     c                   endif
446900010625      *
447000010625     c                   endif
447100010625      * validità mandato per le spedizioni in partenza e/o in arrivo
447200010625     c                   if        tptapl = ' '
447300060606      * in lingua
447400060606     c                   select
447500060606     c                   when      i45lin = 'I'
447600010625     c                   eval      comvma = 'PARTENZA e in ARRIVO'
447700060606     c                   when      i45lin = '2'
447800060703     c                   eval      comvma = 'DEPARTING and ARRIVING'
447900060817     c                   when      i45lin = '3'
448000060817     c                   eval      comvma = 'DÉPART ET ARRIVÉE'
448100060817     c                   when      i45lin = '4'
448200060817     c                   eval      comvma = 'ABFAHRT und EINGANG'
448300060606     c                   endsl
448400010625     c                   endif
448500010625      * partenza
448600010625     c                   if        tptapl = 'P'
448700060606      * in lingua
448800060606     c                   select
448900060606     c                   when      i45lin = 'I'
449000010625     c                   eval      comvma = 'PARTENZA'
449100060606     c                   when      i45lin = '2'
449200060703     c                   eval      comvma = 'DEPARTURE'
449300060817     c                   when      i45lin = '3'
449400060817     c                   eval      comvma = 'DÉPART'
449500060817     c                   when      i45lin = '4'
449600060817     c                   eval      comvma = 'ABFAHRT'
449700060606     c                   endsl
449800010625     c                   endif
449900010625      * arrivo
450000010625     c                   if        tptapl = 'A'
450100060606      * in lingua
450200060606     c                   select
450300060606     c                   when      i45lin = 'I'
450400010625     c                   eval      comvma = 'ARRIVO'
450500060606     c                   when      i45lin = '2'
450600060703     c                   eval      comvma = 'ARRIVAL'
450700060817     c                   when      i45lin = '3'
450800060817     c                   eval      comvma = 'ARRIVÉE'
450900060817     c                   when      i45lin = '4'
451000060817     c                   eval      comvma = 'EINGANG'
451100060606     c                   endsl
451200010625     c                   endif
451300010625      *
451400010625      * cerco se ha record validi con tariffa o minimo > 0
451500010625     c                   EXSR      RICREC
451600011001      *
451700011001     c                   if        *in22
451800011001     c                   exsr      SR_OVER
451900011001     c                   write     ta49pel1
452000011009     c                   else
452100011009     c                   exsr      SR_OVER
452200011009     c                   write     ta49pel2
452300011001     c                   endif
452400011001      *
452500010625      * per 04 acceso stampo nessun addebito
452600010625     c                   if        *in04
452700010906      *
452800060704     c                   select
452900060704     c                   when      i45lin = 'I'
453000011001     c                   eval      comma = ': nessun addebito'
453100060704     c                   when      i45lin = '2'
453200060704     c                   eval      comma = ': no charge      '
453300060817     c                   when      i45lin = '3'
453400060817     c                   eval      comma = ': aucun frais    '
453500060817     c                   when      i45lin = '4'
453600060817     c                   eval      comma = ': Keine Kosten   '
453700060704     c                   endsl
453800010625      *
453900010627     c                   exsr      SR_OVER
454000010625      *
454100010625      * elevazione limite risarcibile
454200011001     c                   write     ta49pma
454300010625     c                   write     ta49pvma
454400010625     c                   goto      endelliris
454500010625     c                   endif
454600010626      *
454700010626      * per 04 spento verifico quanti record dettaglio tariffario esistono
454800010626      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
454900010626     c                   if        recval = 1
455000010626     C                   exsr      sr_gesunico
455100011009      * se è tariffa a valore senza l'oltre e con elevazione stampo 1 riga <>
455200011009     c                   if        w_ellimris = 'Y'
455300011001      *
455400011001     c                   exsr      SR_OVER
455500011001      *
455600011001     c                   write     ta49pel
455700011001      *
455800011001     c                   else
455900011001      *
456000011001     c                   eval      comma = w_tarpar
456100010626      *
456200010627     c                   exsr      SR_OVER
456300010626      *
456400011001     c                   write     ta49pma
456500011001      *
456600011001     c                   endif
456700010919      *
456800010919      * stampo eventuale estensione scaglione unico
456900010919      *
457000010919     c                   exsr      sr_estes
457100010723      *
457200010723     c                   exsr      findet
457300010723      *
457400010626     c                   else
457500010625      *
457600010625      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
457700010625      *
457800010625     c                   exsr      sr_gestpd
457900010626      *
458000010626     c                   endif
458100011001      * stampo l'ultima riga
458200011001      *
458300011001     c                   exsr      SR_OVER
458400011001     c                   write     ta49pvma
458500010625      *
458600010625     C     endelliris    endsr
458700010625      *
458800010625      *-------------------------------------------------------------------------
458900010625      *  SR_MANDATO STAMPA DATI DELLA TARIFFA PARTICOLARE ASSICURAZIONE X CONTO
459000010625      *-------------------------------------------------------------------------
459100010625     C     SR_MANDATO    BEGSR
459200010625      *
459300010626     c                   setoff                                       2124
459400020109      *
459500020109     C                   movel     'C '          savftc
459600010625      *
459700010628      *
459800061122     C  n98KTPT          CHAIN     TITPT01l
459900061122     c   98ktpt          chain     tiopt01l
460000010625      * se non trovato vado a fine e non stampo nulla
460100061122     c                   if        not %found
460200010625     c                   goto      endmandato
460300010625     c                   endif
460400010625      * se trovato ma annullato vado a fine  e non stampo nulla
460500010625     c                   if        tptatb = 'A'
460600010625     c                   goto      endmandato
460700010625     c                   endif
460800010627      * se esiste stampo la scritta assicurazioni merci solo se 81 spento
460900010627      * cioè non l'ho già stampata prima
461000010627      *
461100010627     c                   exsr      SR_OVER
461200010627      *
461300010627     c  n81              write     ta49PTAM
461400010627     C  n81              SETON                                        81
461500010625      * verifico se mandato fittizio
461600010625     c                   if        tpttma = 'F'
461700010625     c                   seton                                        24
461800010625     c                   endif
461900010625      * verifico se mandato a valore variabile
462000010625     c                   if        tptvlm = 0
462100010625     c                   seton                                        21
462200010625     c                   endif
462300060307      * mi salvo il tptapl x stampa AC BASE
462400060307     c                   eval      savapl = tptapl
462500060515      * se mandatao fittizio pulisco il tipo applicazione
462600060515     c                   if        *in24
462700060515     c                   clear                   savapl
462800060515     c                   endif
462900010625      * validità mandato per le spedizioni in partenza e/o in arrivo
463000010625     c                   if        tptapl = ' '
463100060606      * in lingua
463200060606     c                   select
463300060606     c                   when      i45lin = 'I'
463400010625     c                   eval      comvma = 'PARTENZA e in ARRIVO'
463500060606     c                   when      i45lin = '2'
463600060703     c                   eval      comvma = 'DEPARTING and ARRIVING'
463700060817     c                   when      i45lin = '3'
463800060817     c                   eval      comvma = 'DÉPART ET ARRIVÉE'
463900060817     c                   when      i45lin = '4'
464000060817     c                   eval      comvma = 'ABFAHRT und EINGANG'
464100060606     c                   endsl
464200010625     c                   endif
464300010625      * partenza
464400010625     c                   if        tptapl = 'P'
464500060606      * in lingua
464600060606     c                   select
464700060606     c                   when      i45lin = 'I'
464800010625     c                   eval      comvma = 'PARTENZA'
464900060606     c                   when      i45lin = '2'
465000060703     c                   eval      comvma = 'DEPARTURE'
465100060817     c                   when      i45lin = '3'
465200060817     c                   eval      comvma = 'DÉPART'
465300060817     c                   when      i45lin = '4'
465400060817     c                   eval      comvma = 'ABFAHRT'
465500060606     c                   endsl
465600010625     c                   endif
465700010625      * arrivo
465800010625     c                   if        tptapl = 'A'
465900060606      * in lingua
466000060606     c                   select
466100060606     c                   when      i45lin = 'I'
466200010625     c                   eval      comvma = 'ARRIVO'
466300060606     c                   when      i45lin = '2'
466400060703     c                   eval      comvma = 'ARRIVAL'
466500060817     c                   when      i45lin = '3'
466600060817     c                   eval      comvma = 'ARRIVÉE'
466700060817     c                   when      i45lin = '4'
466800060817     c                   eval      comvma = 'EINGANG'
466900060606     c                   endsl
467000010625     c                   endif
467100010625      * se non è mandato a valore variabile e non è fittizio
467200010625     c                   if        not *in21 and not *in24
467300010625     c                   z-add     tptvlm        comvlm
467400010625      *
467500010625      * decodifico il flag valore merce
467600010625      *
467700010625     C                   movel     'TR'          COD
467800010625     C                   movel(P)  tptfvm        KEY
467900060605     c     tabkeyl       chain     tabel00f
468000010625      *
468100010625     c                   if        %found(tabel00f)
468200010625     c                   movel     TBLUNI        DSTR
468300010625     c                   else
468400010625     c                   clear                   DSTR
468500010625     c                   endif
468600010625      *
468700010625     C                   movel(P)  §TRDE2        DESFVM
468800010625      *
468900010625     c                   endif
469000010625      *
469100010625      * cerco se ha record validi con tariffa o minimo > 0
469200010625     c                   EXSR      RICREC
469300011001      *
469400011001      * mandato a valore variabile o con valore espresso (no fittizio)
469500011001     c                   if        not *in24
469600011001      * se mandato non a valore variabile
469700011001     c                   if        not *in21
469800011001     c                   write     ta49pma1
469900011001     c                   else
470000011001     c                   write     ta49pmav
470100011001     c                   endif
470200011001      *
470300011001     c                   else
470400011001     c                   write     ta49pmaf1
470500011001     c                   endif
470600010625      * per 04 acceso stampo nessun addebito
470700010625     c                   if        *in04
470800010625      * mandato a valore variabile o con valore espresso (no fittizio)
470900060704     c                   select
471000060704     c                   when      i45lin = 'I'
471100010626     c                   eval      comma = ': nessun addebito'
471200060704     c                   when      i45lin = '2'
471300060704     c                   eval      comma = ': no charge      '
471400060817     c                   when      i45lin = '3'
471500060817     c                   eval      comma = ': aucun frais    '
471600060817     c                   when      i45lin = '4'
471700060817     c                   eval      comma = ': Keine Kosten   '
471800060704     c                   endsl
471900010625      *
472000010627     c                   exsr      SR_OVER
472100011001      *
472200010625     c                   write     ta49pma
472300010625     c                   write     ta49pvma
472400010625     c                   goto      endmandato
472500010625     c                   endif
472600010626      *
472700010626      * per 04 spento verifico quanti record dettaglio tariffario esistono
472800010626      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
472900010626     c                   if        recval = 1
473000010626     C                   exsr      sr_gesunico
473100010626      *
473200010626     c                   eval      comma = w_tarpar
473300010626      *
473400010627     c                   exsr      SR_OVER
473500010626      *
473600010626     c                   write     ta49pma
473700010723      *
473800010723     c                   exsr      findet
473900010723      *
474000010626      *
474100010626     c                   else
474200010625      *
474300010625      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
474400010625      *
474500010625     c                   exsr      sr_gestpd
474600010626      *
474700010626     c                   endif
474800010625     C*
474900010625      * stampo l'ultima riga
475000010625      *
475100010627     c                   exsr      SR_OVER
475200010625     c                   write     ta49pvma
475300010625      *
475400010625      *
475500010625     C     endmandato    endsr
475600010628      *
475700060301      *-------------------------------------------------------------------------
475800060301      *  sr_acbase STAMPA DATI DELLA TARIFFA PARTICOLARE d
475900060301      *-------------------------------------------------------------------------
476000060301     c     sr_acbase     begsr
476100060301
476200060301      * la tariffa ac base non può essere fittizia e non può essere a mandato
476300060301      * variabile
476400060301
476500060301     c                   movel     'd '          savftc
476600060301
476700061122     c  n98ktpt          chain     titpt01l
476800061122     c   98ktpt          chain     tiopt01l
476900060301      * se non trovato vado a fine e non stampo nulla
477000061122     c                   if        not %found
477100060301     c                   goto      endacbase
477200060301     c                   endif
477300060301      * se trovato ma annullato vado a fine  e non stampo nulla
477400060301     c                   if        tptatb = 'A'
477500060301     c                   goto      endacbase
477600060301     c                   endif
477700060301      * se esiste stampo la scritta assicurazioni merci solo se 81 spento
477800060301      * cioè non l'ho già stampata prima
477900060301
478000060301     c                   exsr      SR_OVER
478100060301
478200060301     c  n81              write     ta49PTAM
478300060301     c  n81              seton                                        81
478400060301      * validità mandato per le spedizioni in partenza e/o in arrivo
478500060301     c                   if        tptapl = ' '
478600060606      * in lingua
478700060606     c                   select
478800060606     c                   when      i45lin = 'I'
478900060301     c                   eval      comvma = 'PARTENZA e in ARRIVO'
479000060606     c                   when      i45lin = '2'
479100060703     c                   eval      comvma = 'DEPARTING and ARRIVING'
479200060817     c                   when      i45lin = '3'
479300060817     c                   eval      comvma = 'DÉPART ET ARRIVÉE'
479400060817     c                   when      i45lin = '4'
479500060817     c                   eval      comvma = 'ABFAHRT und EINGANG'
479600060606     c                   endsl
479700060301     c                   endif
479800060301      * partenza
479900060301     c                   if        tptapl = 'P'
480000060606      * in lingua
480100060606     c                   select
480200060606     c                   when      i45lin = 'I'
480300060301     c                   eval      comvma = 'PARTENZA'
480400060606     c                   when      i45lin = '2'
480500060703     c                   eval      comvma = 'DEPARTURE'
480600060817     c                   when      i45lin = '3'
480700060817     c                   eval      comvma = 'DÉPART'
480800060817     c                   when      i45lin = '4'
480900060817     c                   eval      comvma = 'ABFAHRT'
481000060606     c                   endsl
481100060301     c                   endif
481200060301      * arrivo
481300060301     c                   if        tptapl = 'A'
481400060606      * in lingua
481500060606     c                   select
481600060606     c                   when      i45lin = 'I'
481700060301     c                   eval      comvma = 'ARRIVO'
481800060606     c                   when      i45lin = '2'
481900060703     c                   eval      comvma = 'ARRIVAL'
482000060817     c                   when      i45lin = '3'
482100060817     c                   eval      comvma = 'ARRIVÉE'
482200060817     c                   when      i45lin = '4'
482300060817     c                   eval      comvma = 'EINGANG'
482400060606     c                   endsl
482500060301     c                   endif
482600060307      * se AC PLUS valida in partenza stampo ARRIVO su AC BASE
482700060307     c                   if        savapl = 'P'
482800060606      * in lingua
482900060606     c                   select
483000060606     c                   when      i45lin = 'I'
483100060307     c                   eval      comvma = 'ARRIVO'
483200060606     c                   when      i45lin = '2'
483300060703     c                   eval      comvma = 'ARRIVAL'
483400060817     c                   when      i45lin = '3'
483500060817     c                   eval      comvma = 'ARRIVÉE'
483600060817     c                   when      i45lin = '4'
483700060817     c                   eval      comvma = 'EINGANG'
483800060606     c                   endsl
483900060307     c                   endif
484000060307      * se AC PLUS valida in arrivo stampo PARTENZA su AC BASE
484100060307     c                   if        savapl = 'A'
484200060606      * in lingua
484300060606     c                   select
484400060606     c                   when      i45lin = 'I'
484500060307     c                   eval      comvma = 'PARTENZA'
484600060606     c                   when      i45lin = '2'
484700060703     c                   eval      comvma = 'DEPARTURE'
484800060817     c                   when      i45lin = '3'
484900060817     c                   eval      comvma = 'DÉPART'
485000060817     c                   when      i45lin = '4'
485100060817     c                   eval      comvma = 'ABFAHRT'
485200060606     c                   endsl
485300060307     c                   endif
485400060301
485500060301     c                   z-add     tptvlm        comvlm
485600060301      *
485700060301      * decodifico il flag valore merce
485800060301      *
485900060301     c                   movel     'TR'          cod
486000060301     c                   movel(p)  tptfvm        key
486100060605     c     tabkeyl       chain     tabel00f
486200060301
486300060301     c                   if        %found(tabel00f)
486400060301     c                   movel     tbluni        dstr
486500060301     c                   else
486600060301     c                   clear                   dstr
486700060301     c                   endif
486800060301
486900060301     c                   movel(p)  §trde2        desfvm
487000060301
487100060301      * cerco se ha record validi con tariffa o minimo > 0
487200060301     c                   EXSR      RICREC
487300060301     c                   write     ta49pma2
487400060301      * per 04 acceso stampo nessun addebito
487500060301     c                   if        *in04
487600060301      * mandato a valore variabile o con valore espresso (no fittizio)
487700060704     c                   select
487800060704     c                   when      i45lin = 'I'
487900060301     c                   eval      comma = ': nessun addebito'
488000060704     c                   when      i45lin = '2'
488100060704     c                   eval      comma = ': no charge      '
488200060817     c                   when      i45lin = '3'
488300060817     c                   eval      comma = ': aucun frais    '
488400060817     c                   when      i45lin = '4'
488500060817     c                   eval      comma = ': Keine Kosten   '
488600060704     c                   endsl
488700060301      *
488800060301     c                   exsr      SR_OVER
488900060301      *
489000060301     c                   write     ta49pma
489100060301     c                   write     ta49pvma
489200060301     c                   goto      endacbase
489300060301     c                   endif
489400060301      *
489500060301      * per 04 spento verifico quanti record dettaglio tariffario esistono
489600060301      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
489700060301     c                   if        recval = 1
489800060301     C                   exsr      sr_gesunico
489900060301
490000060301     c                   eval      comma = w_tarpar
490100060301
490200060301     c                   exsr      SR_OVER
490300060301
490400060301     c                   write     ta49pma
490500060301
490600060301     c                   exsr      findet
490700060301
490800060301     c                   else
490900060301
491000060301      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
491100060301     c                   exsr      sr_gestpd
491200060301
491300060301     c                   endif
491400060301
491500060301      * stampo l'ultima riga
491600060301     c                   exsr      SR_OVER
491700060301     c                   write     ta49pvma
491800060301      *
491900060301     C     endacbase     endsr
492000060816      *
492100060816      *-------------------------------------------------------------------------
492200060816      *  sr_fuel   STAMPA DATI DELLA TARIFFA PARTICOLARE f
492300060816      *-------------------------------------------------------------------------
492400060816     c     sr_fuel       begsr
492500060816
492600060816     c                   movel     'f '          savftc
492700060816
492800061122     c  n98ktpt          chain     titpt01l
492900061122     c   98ktpt          chain     tiopt01l
493000060816      * se non trovato vado a fine e non stampo nulla
493100061122     c                   if        not %found
493200060816     c                   leavesr
493300060816     c                   endif
493400060816      * se trovato ma annullato vado a fine  e non stampo nulla
493500060816     c                   if        tptatb = 'A'
493600060816     c                   leavesr
493700060816     c                   endif
493800140218
493900140218     c                   eval      dTPT01 = TPTflo
494000140218     c                   eval      compma = §TPTpma
494100140218
494200060816      * cerco se ha record validi con tariffa o minimo > 0
494300060816     c                   exsr      ricrec
494400060816      * per 04 acceso tariffa negata
494500140218     c                   if        *in04 and compma = 0
494600061207      * non devo stampare la tariffa particolare
494700061207     c                   leavesr
494800061207     c                   endif
494900061207      * stampo intestazione
495000061207     c                   exsr      sr_over
495100061207     c                   write     ta49ptf
495200060816      * recupero il prezzo medio del gasolio
495300060816     c                   clear                   pmgpmg
495400060816     c     tptdpb        setll     tipmg01l
495500060816     c                   read      tipmg01l
495600060816      * devo controllare la data del prezzo medio che ho trovato
495700060816      * se la data del prezzo medio è troppo indietro non devo stampare
495800140224      * quindi stampo un errore, anche se il prezzo è a 0
495900140224      * faccio il controllo con 6 gg
496000061212     c                   if        pmgdtd > *zeros
496100060816     c     *iso          move      pmgdtd        data_iso1
496200061212     c                   else
496300061212     c                   eval      data_iso1 = *loval
496400061212     c                   endif
496500060816     c     *iso          move      tptdpb        data_iso2
496600060816     c     data_iso2     subdur    data_iso1     wgio:*d
496700140224     c                   IF        wgio > 6 or pmgpmg = 0
496800140224     c                   eval      errmsg = msg(1)
496900140224     c                   exsr      staerr
497000140224     c                   goto      fine
497100140224     c                   ENDIF
497200060816     c                   eval      comfpmg = pmgpmg
497300060816      * stampo il prezzo medio del gasolio in base alla data indicata
497400060816      * in tariffa
497500060816     c                   exsr      sr_over
497600060816     c                   write     ta49ptf1
497700140219      * per 04 acceso vuol dire che la tariffa è tutta negata quindi
497800140219      * non devo stampare niente.
497900140219     c                   IF        *in04
498000140219     c                   ELSE
498100060816      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
498200060816      * verifico quanti record dettaglio tariffario esistono
498300060816      * se 1 scaglione solo devo stampare una dicitura particolare
498400080925     c                   if        recval = 1 and oltre = 0
498500061207     c                   z-add     tpditr        comfitr
498600060816     c                   exsr      sr_over
498700060816     c                   write     ta49ptf2
498800060816      * se maggiori di 1 gestisco la stampa standard delle tariffe particolari
498900060816      * con più scaglioni
499000060816     c                   else
499100061207     c                   exsr      sr_over
499200061207     c                   write     ta49ptf4
499300061207      * forzo l'accensione dell'indicatore 12 per stampare la dicitura
499400061207      * particolare per la tariffa fuel
499500061207     c                   eval      *in12 = *on
499600061207      * imposto la dicitura particolare
499700070205      * in lingua
499800070205     c                   select
499900070205      * italiano
500000070205     c                   when      i45lin = 'I'
500100080910     c                   eval      desumf = 'Prezzo gasolio'
500200070205      * inglese
500300070205     c                   when      i45lin = '2'
500400140224     c                   eval      desumf = 'Diesel fuel price'
500500070205      * francese
500600070205     c                   when      i45lin = '3'
500700080912     c                   eval      desumf = 'Prix du gazole'
500800070205      * tedesco
500900070205     c                   when      i45lin = '4'
501000080912     c                   eval      desumf = 'Preis für Dieselkraftstoff'
501100070205     c                   endsl
501200061207     c                   exsr      sr_gestpd
501300060816     c                   endif
501400140219     c                   ENDIF
501500140218      * stampo % minimo applicazione
501600140218     c                   IF        compma > 0
501700140224     c                   exsr      sr_over
501800140218     c                   write     ta49ptf5
501900140218     c                   ENDIF
502000060816      * stampo l'ultima riga
502100060816     c                   exsr      sr_over
502200150217      * stampa in maniera diversa in base all'applicazione del VMA
502300150217     c                   IF        §TPTvma = *blanks
502400060816     c                   write     ta49ptf3
502500150217     c                   ELSE
502600150217     c                   write     ta49ptf6
502700150217     c                   ENDIF
502800060816
502900060816     c                   endsr
503000160623
503100160623      /free
503200160623       //--------------------------------------------------------------
503300160623       //?Prepara righe per Tariffa H10:30.
503400160623       //--------------------------------------------------------------
503500160623       BEGSR RigheH1030;
503600160623
503700160623       //?Recupero i limiti dalla tabella LTS
503800160623         clear TIBS02DS;
503900160623         clear dLTS;
504000160623         T02mod = 'C';
504100160623         T02sif = knsif;
504200160623         T02cod = 'LTS';
504300160623         T02ke1 = 'H';
504400160623         TNTBE_RicercaControllo (kpjba:tibs02ds);
504500160623         IF  T02err = *blanks;
504600160623           dLTS = T02uni;
504700160623         ENDIF;
504800160623
504900160623         IF  %subst(%editc(§LTSlmcm:'X'):3:3) > *zeros;
505000160623           Stpvlm = %editc(§LTSlmcm:'4');
505100160623         ELSE;
505200160623           Stpvlm = %subst(%editc(§LTSlmcm:'4'):1:2);
505300160623         ENDIF;
505400160623
505500160623         IF  %subst(%editc(§LTSlpkm:'X'):7:1) > *zeros;
505600160623           Stppkg = %editc(§LTSlpkm:'4');
505700160623         ELSE;
505800160623           Stppkg = %subst(%editc(§LTSlpkm:'4'):1:6);
505900160623         ENDIF;
506000160623
506100160623       //?Preparo le frasi con i limiti da tabella
506200160623         SELECT;
506300160623
506400160623         WHEN  I45lin = 'I';
506500160623         riga1h1030 = 'Il "SERVIZIO 10:30" sarà effettuato +
506600160623         esclusivamente per spedizioni con volume massimo di';
506700160623         riga1h1030 = %trim(riga1h1030) + ' ' + %trim(Stpvlm) +
506800160624         ' mc e di peso massimo';
506900160623
507000160623         riga2h1030 = 'Kg. ' + %trim(Stppkg) + ', destinate +
507100160623         unicamente alle località indicate sul sito Internet +
507200160623         www.BRT.it';
507300160623
507400160623         WHEN  I45lin = '2';
507500160623         riga1h1030 = 'The "10:30 SERVICE" service will be +
507600160623         only done for the shimpents whose maximum volume +
507700160623         corresponds to';
507800160623         riga1h1030 = %trim(riga1h1030) + ' ' + %trim(Stpvlm) +
507900160623         ' cubic metre,';
508000160623
508100160623         riga2h1030 = 'whose maximum wight corresponds +
508200160623         to Kg.';
508300160623         riga2h1030 = %trim(riga2h1030) + ' ' + %trim(Stppkg) +
508400160623         ' and whose delivery place is in the Internet site +
508500160623         www.BRT.it';
508600160623
508700160623         WHEN  I45lin = '3';
508800160623         riga1h1030 = 'Le service "SERVICE 10:30" sera effectué +
508900160623         uniquement pour les envois avec un volume maximum de';
509000160623         riga1h1030 = %trim(riga1h1030) + ' ' + %trim(Stpvlm) +
509100160623         ' mètre cube, ';
509200160623
509300160624         riga2h1030 = 'avec un poids maximum de Kg. ';
509400160623         riga2h1030 = %trim(riga2h1030) + ' ' + %trim(Stppkg) +
509500160623         ' et pour les localités indiquées sur le site Internet +
509600160623         www.BRT.it';
509700160623
509800160623         WHEN  I45lin = '4';
509900160623         riga1h1030 = 'Mit dem "10:30 SERVICE" Dienst können nur +
510000160623         Sendungen mit grösstem Umfang von';
510100160623         riga1h1030 = %trim(riga1h1030) + ' ' + %trim(Stpvlm) +
510200160623         ' Kubikmeter und grösstem Gewicht';
510300160623
510400160623         riga2h1030 = 'von';
510500160623         riga2h1030 = %trim(riga2h1030) + ' ' + %trim(Stppkg) +
510600160623         ' Kg, die nur an die Orten, die auf der BRT Webseite +
510700160623         sind, adressiert sind, geschickt werden.';
510800160623         ENDSL;
510900160623
511000160623       ENDSR;
511100160623      /end-free
511200060301      *
511300010625      *-------------------------------------------------------------------------
511400010625      * SR_GESTPD GESTIONE DEL DETTAGLIO TARIFFARIO UGUALE PER TUTTE LE TAR.PAR.
511500010625      *-------------------------------------------------------------------------
511600010625     c     SR_GESTPD     BEGSR
511700010625      * Decodfico tipo tariffa e scaglioni
511800010625     c                   movel     'TR'          COD
511900010625      *
512000010625     C                   CLEAR                   ATF
512100010625     c                   EXSR      DECTPG
512200010625      * elaboro 6 scaglioni alla volta
512300010625     c                   z-add     1             k4
512400010625      *
512500010625    1c                   dow       k4 <= 18
512600010625     C                   MOVEA     SGLD(K4)      SSLD
512700010625     C                   MOVEA     SGLB(K4)      SSLB
512800010625     C                   MOVEA     SGLA(K4)      SSLA
512900010625      * 1 DECIMALE
513000080901     C                   IF        *IN91 AND NOT *IN37 and not *in38
513100010625     C                   MOVEA     SG1A(K4)      SS1A
513200010625     C                   MOVEA     SG1D(K4)      SS1D
513300010625     C                   ENDIF
513400010625      * 2 DECIMALI
513500080901     C                   IF        *IN91 AND *IN37 and not *in38
513600010625     C                   MOVEA     SG2A(K4)      SS2A
513700010625     C                   MOVEA     SG2D(K4)      SS2D
513800010625     C                   ENDIF
513900080901      * 3 DECIMALI
514000080901     C                   IF        *IN91 AND *IN37 and  *in38
514100080901     C                   MOVEA     SG3A(K4)      SS3A
514200080901     C                   MOVEA     SG3D(K4)      SS3D
514300080901     C                   ENDIF
514400010625      *
514500010625      * CARICO GLI SCAGLIONI PER ELABORARE IL DETTAGLIO
514600010625      *
514700080916     C                   MOVEA     SCP(K4)       SSLP
514800010625     C*  INTESTAZIONE SCAGLIONI
514900080916     C                   Z-ADD     SSLP(1)       W0050             5 0
515000021220     C     W0050         COMP      99999                                  41
515100080916     C     SSLP(1)       COMP      *ZEROS                                 51
515200080916     C                   Z-ADD     SSLP(2)       W0050             5 0
515300010625     C     W0050         COMP      99999                                  42
515400080916     C     SSLP(2)       COMP      *ZEROS                                 52
515500080916     C                   Z-ADD     SSLP(3)       W0050             5 0
515600010625     C     W0050         COMP      99999                                  43
515700080916     C     SSLP(3)       COMP      *ZEROS                                 53
515800080916     C                   Z-ADD     SSLP(4)       W0050             5 0
515900010625     C     W0050         COMP      99999                                  44
516000080916     C     SSLP(4)       COMP      *ZEROS                                 54
516100080916     C                   Z-ADD     SSLP(5)       W0050             5 0
516200010625     C     W0050         COMP      99999                                  45
516300080916     C     SSLP(5)       COMP      *ZEROS                                 55
516400080916     C                   Z-ADD     SSLP(6)       W0050             5 0
516500010625     C     W0050         COMP      99999                                  46
516600080916     C     SSLP(6)       COMP      *ZEROS                                 56
516700010625     C*
516800010625     C* SE IL 41 E' ACCESO PERCHÈ IL PRIMO SCAGLIONE E' UGUALE A TUTTI 999999
516900010627     C* O IL SECONDO SCAGLIONE E' A ZERO
517000010627     C* MA SE NON SI TRATTA + DEL 1° SCAGLIONE IN ASSOLUTO DEVO STAMPARE "OLTRE
517100010625     C* LO SCAGLIONE PRECEDENTE
517200010625     C* QUINDI ACCENDO IL 40 E SPENGO IL 41
517300010625     C*
517400010625     C                   IF        *IN41 AND K4 <> 1
517500010625     C                   SETOFF                                       41
517600010625     C                   SETON                                        40
517700010625     C                   ENDIF
517800010625      *
517900010625     C                   EXSR      DECSCA
518000010625     C* LETTURA DEL DETTAGLIO
518100010625     C                   EXSR      LEGDET
518200010625      *
518300010625      * rielaboro altri 6 scaglioni
518400010625      *
518500010625     c                   add       6             k4
518600010625      * se scaglioni minori o uguali a 18 e scaglione valorizzato
518700010625    2c                   if        k4 <= 18 and
518800010625     c                             scp(k4) <> *zeros
518900010625      * pulisco schiere di comodo per la stampa degli scaglioni
519000010625     c                   clear                   ssld
519100010625     c                   clear                   sslb
519200010625     c                   clear                   ssla
519300010625     c                   clear                   ss1d
519400010625     c                   clear                   ss1a
519500010625     c                   clear                   ss2d
519600010625     c                   clear                   ss2a
519700080901     c                   clear                   ss3d
519800080901     c                   clear                   ss3a
519900080916     c                   clear                   sslp
520000010625      * spengo indicatori che condizionano la stampa
520100010625     c                   setoff                                       404142
520200010625     c                   setoff                                       434445
520300010625     c                   setoff                                       46
520400010725     c                   setoff                                       515253
520500010725     c                   setoff                                       545556
520600010625      *
520700010625     c                   else
520800010625      * non ci sono + scaglioni da stampare
520900010625     c                   eval      k4=19
521000010625     c                   leave
521100010625      *
521200010625    2c                   endif
521300010625      *
521400010625    1c                   enddo
521500010625      *
521600010625     C                   EXSR      FINDET
521700010625      *
521800010625     c                   endsr
521900010625      *-------------------------------------------------------------------------
522000010625      * SR_GESUNICO GESTIONE DETTAGLIO TARIFFARIO CON UNA SOLA RIGA
522100010625      *-------------------------------------------------------------------------
522200010625     c     SR_GESUNICO   BEGSR
522300010625      *
522400010625     c                   clear                   w_tarpar
522500010731     c                   clear                   w_tarparest
522600011001     c                   clear                   w_ellimris
522700010626      * Decodfico tipo pagamento
522800010625     c                   movel     'TR'          COD
522900010625     C                   movel(P)  tpttpg        KEY
523000060605     c     tabkeyl       chain     tabel00f
523100010625      *
523200010625     c                   if        %found(tabel00f)
523300010625     c                   movel     TBLUNI        DSTR
523400010625     c                   else
523500010625     c                   clear                   DSTR
523600010625     c                   endif
523700010625      *
523800010625      * verifico se scaglioni a valore
523900010625     c                   if        §trtap = 'S'
524000010625     c                   eval      W_per = '%'
524100010625     c                   else
524200010625     c                   clear                   W_per
524300010625     c                   endif
524400010626      *
524500010626      * decodifico a mio modo il tipo pagamento
524600010626      *
524700010626     C                   CLEAR                   WRKDESTPG
524800010626      *
524900060606      * in lingua
525000060606     c                   select
525100060817      * italiano
525200060606     c                   when      i45lin = 'I'
525300010626     c                   if        tpttpg = 'T'
525400010626     c                   eval      wrkdestpg = 'SUL TRASPORTO'
525500010626     c                   endif
525600010626
525700010626     c                   if        tpttpg = 'V'
525800010626     c                   eval      wrkdestpg = 'SUL VALORE   '
525900010626     c                   endif
526000010626
526100010626     c                   if        tpttpg = '0'
526200010626     c                   eval      wrkdestpg = 'A Q.LE       '
526300010626     c                   endif
526400010626
526500010626     c                   if        tpttpg = '1'
526600010626     c                   eval      wrkdestpg = 'A COLLO      '
526700010626     c                   endif
526800010626
526900010626     c                   if        tpttpg = '2'
527000010626     c                   eval      wrkdestpg = 'A SPEDIZIONE '
527100010626     c                   endif
527200010626
527300010626     c                   if        tpttpg = '3'
527400010626     c                   eval      wrkdestpg = 'A KG.        '
527500010626     c                   endif
527600010626
527700010626     c                   if        tpttpg = '4'
527800010626     c                   eval      wrkdestpg = 'A VALORE     '
527900010626     c                   endif
528000010626
528100010626     c                   if        tpttpg = '5'
528200010626     c                   eval      wrkdestpg = 'A QUANTITA'' '
528300010626     c                   endif
528400021209      * Tar.particolare Diritto Fatturazione
528500030207     c                   If        TptFtc = '§'
528600021209     c                   Eval      WrkDesTpg = 'A FATTURA   '
528700021209     c                   EndIf
528800021209      * Tar.particolare Gest.bancali a rendere
528900021209     c                   If        TptFtc = '='
529000021209     c                   Eval      WrkDesTpg = 'A BANCALE   '
529100021209     c                   EndIf
529200151117      * Tar.particolare Diritto di chiamata prenotazione ritiro telefonico
529300151117     c                   If        TptFtc = 't'
529400151117     c                   Eval      WrkDesTpg = 'A RITIRO    '
529500151117     c                   EndIf
529600060817
529700060817      * inglese
529800060606     c                   when      i45lin = '2'
529900060606     c                   if        tpttpg = 'T'
530000060703     c                   eval      wrkdestpg = 'ON TRANSPORT '
530100060606     c                   endif
530200060606
530300060606     c                   if        tpttpg = 'V'
530400060703     c                   eval      wrkdestpg = 'ON VALUE     '
530500060606     c                   endif
530600060606
530700060606     c                   if        tpttpg = '0'
530800060606     c                   eval      wrkdestpg = 'PER 100 KG.  '
530900060606     c                   endif
531000060606
531100060606     c                   if        tpttpg = '1'
531200060606     c                   eval      wrkdestpg = 'PER PARCEL   '
531300060606     c                   endif
531400060606
531500060606     c                   if        tpttpg = '2'
531600060606     c                   eval      wrkdestpg = 'PER SHIPMENT '
531700151117     c                   endif
531800060606
531900060606     c                   if        tpttpg = '3'
532000060606     c                   eval      wrkdestpg = 'PER KG.      '
532100060606     c                   endif
532200060606
532300060606     c                   if        tpttpg = '4'
532400060703     c                   eval      wrkdestpg = 'BY VALUE     '
532500060606     c                   endif
532600060606
532700060606     c                   if        tpttpg = '5'
532800060703     c                   eval      wrkdestpg = 'BY QUANTITY  '
532900060606     c                   endif
533000060606      * Tar.particolare Diritto Fatturazione
533100060606     c                   If        TptFtc = '§'
533200060606     c                   Eval      WrkDesTpg = 'PER INVOICE '
533300060606     c                   EndIf
533400060606      * Tar.particolare Gest.bancali a rendere
533500060606     c                   If        TptFtc = '='
533600060606     c                   Eval      WrkDesTpg = 'PER PALLET  '
533700060606     c                   EndIf
533800151117      * Tar.particolare Diritto di chiamata prenotazione ritiro telefonico
533900151117     c                   If        TptFtc = 't'
534000151118     c                   Eval      WrkDesTpg = 'PER PICK-UP '
534100151117     c                   EndIf
534200151117
534300060817
534400060817      * francese
534500060817     c                   when      i45lin = '3'
534600060817     c                   if        tpttpg = 'T'
534700060922     c                   eval      wrkdestpg = 'SUR LE TRANSP'
534800060817     c                   endif
534900060817
535000060817     c                   if        tpttpg = 'V'
535100060817     c                   eval      wrkdestpg = 'SUR LA VALEUR'
535200060817     c                   endif
535300060817
535400060817     c                   if        tpttpg = '0'
535500060817     c                   eval      wrkdestpg = 'AUX 100 KG   '
535600060817     c                   endif
535700060817
535800060817     c                   if        tpttpg = '1'
535900060817     c                   eval      wrkdestpg = 'AU COLIS     '
536000060817     c                   endif
536100060817
536200060817     c                   if        tpttpg = '2'
536300060817     c                   eval      wrkdestpg = 'PAR ENVOI    '
536400060817     c                   endif
536500060817
536600060817     c                   if        tpttpg = '3'
536700060817     c                   eval      wrkdestpg = 'AU KG        '
536800060817     c                   endif
536900060817
537000060817     c                   if        tpttpg = '4'
537100060817     c                   eval      wrkdestpg = 'A LA VALEUR  '
537200060817     c                   endif
537300060817
537400060817     c                   if        tpttpg = '5'
537500060817     c                   eval      wrkdestpg = 'A LA QUANTITÉ'
537600060817     c                   endif
537700060817      * Tar.particolare Diritto Fatturazione
537800060817     c                   If        TptFtc = '§'
537900060817     c                   Eval      WrkDesTpg = 'A FATTURA   '
538000060817     c                   EndIf
538100060817      * Tar.particolare Gest.bancali a rendere
538200060817     c                   If        TptFtc = '='
538300060922     c                   Eval      WrkDesTpg = 'A LA PALETTE'
538400060817     c                   EndIf
538500151117      * Tar.particolare Diritto di chiamata prenotazione ritiro telefonico
538600151117     c                   If        TptFtc = 't'
538700151118     c                   Eval      WrkDesTpg = 'PAR RETRAIT '
538800151117     c                   EndIf
538900060817
539000060817      * tedesco
539100060817     c                   when      i45lin = '4'
539200060817     c                   if        tpttpg = 'T'
539300060817     c                   eval      wrkdestpg = 'AUF TRANSPORT'
539400060817     c                   endif
539500060817
539600060817     c                   if        tpttpg = 'V'
539700060817     c                   eval      wrkdestpg = 'AUF WERT     '
539800060817     c                   endif
539900060817
540000060817     c                   if        tpttpg = '0'
540100060817     c                   eval      wrkdestpg = 'N. ZENTNER   '
540200060817     c                   endif
540300060817
540400060817     c                   if        tpttpg = '1'
540500060817     c                   eval      wrkdestpg = 'N. FRACHTSTÜCK'
540600060817     c                   endif
540700060817
540800060817     c                   if        tpttpg = '2'
540900060817     c                   eval      wrkdestpg = 'N. SENDUNG   '
541000060817     c                   endif
541100060817
541200060817     c                   if        tpttpg = '3'
541300060817     c                   eval      wrkdestpg = 'N. KG        '
541400060817     c                   endif
541500060817
541600060817     c                   if        tpttpg = '4'
541700060817     c                   eval      wrkdestpg = 'N. WERT      '
541800060817     c                   endif
541900060817
542000060817     c                   if        tpttpg = '5'
542100060817     c                   eval      wrkdestpg = 'N. MENGE     '
542200060817     c                   endif
542300060817      * Tar.particolare Diritto Fatturazione
542400060817     c                   If        TptFtc = '§'
542500060817     c                   Eval      WrkDesTpg = 'A FATTURA   '
542600060817     c                   EndIf
542700060817      * Tar.particolare Gest.bancali a rendere
542800060817     c                   If        TptFtc = '='
542900060922     c                   Eval      WrkDesTpg = 'N.PALETTE   '
543000060817     c                   EndIf
543100151117      * Tar.particolare Diritto di chiamata prenotazione ritiro telefonico
543200151117     c                   If        TptFtc = 't'
543300151118     c                   Eval      WrkDesTpg = 'N. BUCHUNG  '
543400151117     c                   EndIf
543500060817
543600060606     c                   endsl
543700010625      *
543800010625      * CERCO IL RECORD VALIDO
543900010625      *
544000010625      *
544100061122     C  n98ktpt          setll     titpd01l
544200061122     c   98ktpt          setll     tiopd01l
544300010625      *
544400010625     c                   do        *hival
544500061122     C  n98ktpt          reade     titpd01l
544600061122     c   98ktpt          reade     tiopd01l
544700010625      *
544800061122     c                   if        %eof
544900010625     c                   leave
545000010625     c                   endif
545100010625      *
545200010625     c                   if        tpdatb = 'A'
545300010625     c                   iter
545400010625     c                   endif
545500010625      *
545600010625      * se tariffa non valorizzata leggo la successiva
545700010625      * importo e minimo =  a zero
545800010625      *
545900010625     C                   IF        tpditr = 0 and tpdmin = 0
546000010625     c                   iter
546100010625     c                   else
546200010723      * se importo con percentuale verifico quanti decimali ha
546300010723     c                   if        W_per = '%'
546400010723     c                   z-add     tpditr        valper
546500010723     C                   eval      stpper = %EDITC(valper : '3')
546600010723      * VERIFICO SE HA DECIMALI VALORIZZATI
546700010723     C                   move      valper        w0030
546800010723     C                   move      w0030         w0020
546900010723     C                   move      w0020         w0010
547000010723      * tolgo gli zeri non significativi dopo la virgola
547100010723     c                   select
547200010723      *
547300010723     C                   when      w0030 = *zeros
547400010723     C                   move      '    '        stpper
547500010723      *
547600010723     c                   when      w0020 = *zeros
547700010723     c                   move      '  '          stpper
547800010723      *
547900010723     c                   when      w0010 = *zero
548000010723     c                   move      ' '           stpper
548100010723      *
548200010723     c                   endsl
548300010723      *
548400010723     c                   endif
548500011001      * se sto stampando l'elevazione limite risarcibile mi devo comportare
548600011009      * diversamente se tariffa a valore  e non è definito OLTRE e c'e
548700011009      * elevazione al limite
548800011001     c                   if        W_per = '%' and savftc = 'R ' and oltre = 0
548900011009     c                             and *in22
549000011001      * non definiscp W_tarpar in quanto gestisco in modo diverso
549100011001      * valorizzo il campo W_ellimris
549200011001     c                   eval      w_ellimris = 'Y'
549300011001      *
549400011001      * se esiste minimo e massimo devo valorizzare l'estensione
549500011001      *
549600011001      * solo minimo
549700011001      *
549800011001     c                   if        tpdmin >  0 and tpdmax = 0
549900060606      * in lingua
550000060606     c                   select
550100060606     c                   when      i45lin = 'I'
550200021205     c                   eval      W_TARPAREST = blk56
550300011001     C                             + ' minimo........' +
550400011001     c                             %editc(tpdmin:'3')
550500060606     c                   when      i45lin = '2'
550600060606     c                   eval      w_tarparest = blk56
550700060606     c                             + ' minimum.......' +
550800060606     c                             %editc(tpdmin:'3')
550900060817     c                   when      i45lin = '3'
551000060817     c                   eval      w_tarparest = blk56
551100060817     c                             + ' minimum.......' +
551200060817     c                             %editc(tpdmin:'3')
551300060817     c                   when      i45lin = '4'
551400060817     c                   eval      w_tarparest = blk56
551500060817     c                             + ' minde.........' +
551600060817     c                             %editc(tpdmin:'3')
551700060606     c                   endsl
551800011001     c                   endif
551900011001      *
552000011001      * solo massimo
552100011001      *
552200011001     c                   if        tpdmin =  0 and tpdmax > 0
552300060606      * in lingua
552400060606     c                   select
552500060606     c                   when      i45lin = 'I'
552600021205     c                   eval      W_TARPAREST = blk56
552700011001     C                             + ' massimo.......' +
552800011001     c                             %editc(tpdmax:'3')
552900060606     c                   when      i45lin = '2'
553000060606     c                   eval      w_tarparest = blk56
553100060606     c                             + ' maximum.......' +
553200060606     c                             %editc(tpdmax:'3')
553300060817     c                   when      i45lin = '3'
553400060817     c                   eval      w_tarparest = blk56
553500060817     c                             + ' maximum.......' +
553600060817     c                             %editc(tpdmax:'3')
553700060817     c                   when      i45lin = '4'
553800060817     c                   eval      w_tarparest = blk56
553900060817     c                             + ' höchste.......' +
554000060817     c                             %editc(tpdmax:'3')
554100060606     c                   endsl
554200011001     c                   endif
554300011001      *
554400011001      * sia mino che massimo
554500011001      *
554600011001     c                   if        tpdmin >  0 and tpdmax > 0
554700060606      * in lingua
554800060606     c                   select
554900060606     c                   when      i45lin = 'I'
555000030318     c                   eval      W_TARPAREST = blk10 + blk10 + blk12
555100060908     C                             + '...... minimo...' +
555200011001     c                             %editc(tpdmin:'3')
555300011009     C                             + ' massimo..' +
555400011001     c                             %editc(tpdmax:'3')
555500060606     c                   when      i45lin = '2'
555600060606     c                   eval      w_tarparest = blk10 + blk10 + blk12
555700060908     c                             + '...... minimum..' +
555800060606     c                             %editc(tpdmin:'3')
555900060606     c                             + ' maximum..' +
556000060606     c                             %editc(tpdmax:'3')
556100060817     c                   when      i45lin = '3'
556200060817     c                   eval      w_tarparest = blk10 + blk10 + blk12
556300060908     c                             + '...... minimum..' +
556400060817     c                             %editc(tpdmin:'3')
556500060817     c                             + ' maximum..' +
556600060817     c                             %editc(tpdmax:'3')
556700060817     c                   when      i45lin = '4'
556800060817     c                   eval      w_tarparest = blk10 + blk10 + blk12
556900060908     c                             + '...... minde....' +
557000060817     c                             %editc(tpdmin:'3')
557100060817     c                             + ' höchste..' +
557200060817     c                             %editc(tpdmax:'3')
557300060606     c                   endsl
557400011001     c                   endif
557500011001     c*
557600011001     c                   leave
557700011001     c                   endif
557800010731      *
557900010731      * verifico se devo stampare OLTRE
558000010731      *
558100010731     c                   if        oltre > 0
558200020114      *
558300020114      * verifico se ci sono decimali valorizzati
558400020114      *
558500020114     c                   clear                   *in88
558600020114     c                   clear                   oltrenodec
558700020114     C                   move      oltre         w0030
558800020114     C                   eval      *in88 =(w0030 > 0)
558900020114      * oltre senza decimali
559000020114     c                   if        not *in88
559100020114     c                   z-add     oltre         oltrenodec
559200020114     c                   endif
559300010731      * controllo se tariffa a q.le
559400010731     c                   if        tpttpg = '0'
559500010731     c                   eval      unmis = 'KG.'
559600010731     c                   else
559700010731     c                   clear                   unmis
559800010731     c                   endif
559900010731      *
560000010731      * stampo nella prima riga il valore e nella riga di estensione se ci sono
560100010731      * stampo il minimo e massimo
560200010731      *
560300010731      * no percentuale
560400010731     c                   if        w_per = ' '
560500060606      * in lingua
560600060606     c                   select
560700060606     c                   when      i45lin = 'I'
560800020114     c   88              eval      W_TARPAR = ': ' + wrkdestpg +
560900010731     c                             'oltre ' + (%editc(oltre:'3'))+
561000030318     c                             ' ' + unmis + blk10 + blk10 + blk12
561100021205     c                                                 + %editc(tpditr:'3')
561200020114      * no decimali
561300020114     c  n88              eval      W_TARPAR = ': ' + wrkdestpg +
561400020114     c                             'oltre ' + (%editc(oltrenodec:'3'))+
561500021205     c                             ' ' + unmis + blk36 + %editc(tpditr:'3')
561600060606
561700060606     c                   when      i45lin = '2'
561800060606     c   88              eval      w_tarpar = ': ' + wrkdestpg +
561900060606     c                             'over  ' + (%editc(oltre:'3'))+
562000060606     c                             ' ' + unmis + blk10 + blk10 + blk12
562100060606     c                                                 + %editc(tpditr:'3')
562200060606     c  n88              eval      w_tarpar = ': ' + wrkdestpg +
562300060606     c                             'over  ' + (%editc(oltrenodec:'3'))+
562400060606     c                             ' ' + unmis + blk36 + %editc(tpditr:'3')
562500060817     c                   when      i45lin = '3'
562600060817     c   88              eval      w_tarpar = ': ' + wrkdestpg +
562700060817     c                             'plus  ' + (%editc(oltre:'3'))+
562800060817     c                             ' ' + unmis + blk10 + blk10 + blk12
562900060817     c                                                 + %editc(tpditr:'3')
563000060817     c  n88              eval      w_tarpar = ': ' + wrkdestpg +
563100060817     c                             'plus  ' + (%editc(oltrenodec:'3'))+
563200060817     c                             ' ' + unmis + blk36 + %editc(tpditr:'3')
563300060817     c                   when      i45lin = '4'
563400060817     c   88              eval      w_tarpar = ': ' + wrkdestpg +
563500060817     c                             'mehrals ' + (%editc(oltre:'3'))+
563600060817     c                             ' ' + unmis + blk10 + blk10 + blk10
563700060817     c                                                 + %editc(tpditr:'3')
563800060817     c  n88              eval      w_tarpar = ': ' + wrkdestpg +
563900060817     c                             'mehrals ' + (%editc(oltrenodec:'3'))+
564000060817     c                             ' ' + unmis + blk36 + %editc(tpditr:'3')
564100060606     c                   endsl
564200010731     c                   else
564300010731      * si percentuale
564400060606      * in lingua
564500060606     c                   select
564600060606     c                   when      i45lin = 'I'
564700020114     c   88              eval      W_TARPAR = ': ' +
564800010731     c                             wrkdestpg + ' ' + stpper + ' % ' +
564900010731     c                             'oltre ' + (%editc(oltre:'3'))+
565000030318     c                             ' ' + unmis + blk21
565100020114      * no decimali
565200020114     c  n88              eval      W_TARPAR = ': ' +
565300020114     c                             wrkdestpg + ' ' + stpper + ' % ' +
565400020114     c                             'oltre ' + (%editc(oltrenodec:'3'))+
565500030318     c                             ' ' + unmis + blk25
565600060606
565700060606     c                   when      i45lin = '2'
565800060606     c   88              eval      w_tarpar = ': ' +
565900060606     c                             wrkdestpg + ' ' + stpper + ' % ' +
566000060606     c                             'over  ' + (%editc(oltre:'3'))+
566100060606     c                             ' ' + unmis + blk21
566200060606     c  n88              eval      w_tarpar = ': ' +
566300060606     c                             wrkdestpg + ' ' + stpper + ' % ' +
566400060606     c                             'over  ' + (%editc(oltrenodec:'3'))+
566500060606     c                             ' ' + unmis + blk25
566600060817     c                   when      i45lin = '3'
566700060817     c   88              eval      w_tarpar = ': ' +
566800060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
566900060817     c                             'plus  ' + (%editc(oltre:'3'))+
567000060817     c                             ' ' + unmis + blk21
567100060817     c  n88              eval      w_tarpar = ': ' +
567200060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
567300060817     c                             'plus  ' + (%editc(oltrenodec:'3'))+
567400060817     c                             ' ' + unmis + blk25
567500060817     c                   when      i45lin = '4'
567600060817     c   88              eval      w_tarpar = ': ' +
567700060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
567800060817     c                             'mehrals ' + (%editc(oltre:'3'))+
567900060817     c                             ' ' + unmis + blk21
568000060817     c  n88              eval      w_tarpar = ': ' +
568100060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
568200060817     c                             'mehrals ' + (%editc(oltrenodec:'3'))+
568300060817     c                             ' ' + unmis + blk25
568400060606     c                   endsl
568500010731     c                   endif
568600010731      *
568700010731      * se esiste minimo e massimo devo valorizzare anche l'estensione
568800010731      * tariffe particolari con l'oltre
568900010731      *
569000010731      * solo minimo
569100010731      *
569200010731     c                   if        tpdmin >  0 and tpdmax = 0
569300060606      * in lingua
569400060606     c                   select
569500060606     c                   when      i45lin = 'I'
569600021205     c                   eval      W_TARPAREST = blk56
569700010731     C                             + ' minimo........' +
569800010731     c                             %editc(tpdmin:'3')
569900060606     c                   when      i45lin = '2'
570000060606     c                   eval      w_tarparest = blk56
570100060606     c                             + ' minimum.......' +
570200060606     c                             %editc(tpdmin:'3')
570300060817     c                   when      i45lin = '3'
570400060817     c                   eval      w_tarparest = blk56
570500060817     c                             + ' minimum.......' +
570600060817     c                             %editc(tpdmin:'3')
570700060817     c                   when      i45lin = '4'
570800060817     c                   eval      w_tarparest = blk56
570900060817     c                             + ' minde.........' +
571000060817     c                             %editc(tpdmin:'3')
571100060606     c                   endsl
571200010731     c                   endif
571300010731      *
571400010731      * solo massimo
571500010731      *
571600010731     c                   if        tpdmin =  0 and tpdmax > 0
571700060606      * in lingua
571800060606     c                   select
571900060606     c                   when      i45lin = 'I'
572000021205     c                   eval      W_TARPAREST = blk56
572100010731     C                             + ' massimo.......' +
572200010731     c                             %editc(tpdmax:'3')
572300060606     c                   when      i45lin = '2'
572400060606     c                   eval      w_tarparest = blk56
572500060606     c                             + ' maximum.......' +
572600060606     c                             %editc(tpdmax:'3')
572700060817     c                   when      i45lin = '3'
572800060817     c                   eval      w_tarparest = blk56
572900060817     c                             + ' maximum.......' +
573000060817     c                             %editc(tpdmax:'3')
573100060817     c                   when      i45lin = '4'
573200060817     c                   eval      w_tarparest = blk56
573300060817     c                             + ' höchste.......' +
573400060817     c                             %editc(tpdmax:'3')
573500060606     c                   endsl
573600010731     c                   endif
573700010731      *
573800010731      * sia mino che massimo
573900010731      *
574000010731     c                   if        tpdmin >  0 and tpdmax > 0
574100060606      * in lingua
574200060606     c                   select
574300060606     c                   when      i45lin = 'I'
574400030318     c                   eval      W_TARPAREST = blk10 + blk10 + blk12
574500060908     C                             + '...... minimo...' +
574600010731     c                             %editc(tpdmin:'3')
574700011009     C                             + ' massimo..' +
574800010731     c                             %editc(tpdmax:'3')
574900060606     c                   when      i45lin = '2'
575000060606     c                   eval      w_tarparest = blk10 + blk10 + blk12
575100060908     C                             + '...... minimum..' +
575200060606     c                             %editc(tpdmin:'3')
575300060606     C                             + ' maximum..' +
575400060606     c                             %editc(tpdmax:'3')
575500060817     c                   when      i45lin = '3'
575600060817     c                   eval      w_tarparest = blk10 + blk10 + blk12
575700060908     C                             + '...... minimum..' +
575800060817     c                             %editc(tpdmin:'3')
575900060817     C                             + ' maximum..' +
576000060817     c                             %editc(tpdmax:'3')
576100060817     c                   when      i45lin = '4'
576200060817     c                   eval      w_tarparest = blk10 + blk10 + blk12
576300060908     c                             + '...... minde....' +
576400060817     c                             %editc(tpdmin:'3')
576500060817     c                             + ' höchste..' +
576600060817     c                             %editc(tpdmax:'3')
576700060606     c                   endsl
576800010731     c                   endif
576900010731      *
577000010731     c                   else
577100010731      *
577200010731     c
577300010723      * valorizzo il campo che conterrà la percetuale
577400010625      * se non c'è ne minimo ne massimo
577500010625     c                   if        tpdmin = 0 and tpdmax =  0
577600010723      * no percentuale
577700010723     c                   if        w_per = ' '
577800010626     c                   eval      W_TARPAR = ': ' + wrkdestpg +
577900021205     c                             blk56 + %editc(tpditr:'3')
578000010723     c                   else
578100010723      * si percentuale
578200010723     c                   eval      W_TARPAR = ': ' +
578300030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
578400030318     c                             blk10 + blk10 + blk25
578500010723     c                   endif
578600010723      *
578700010625     c                   endif
578800010625      * se c'è minimo
578900010625     c                   if        tpdmin > 0 and tpdmax = 0
579000010723      * no percentuale
579100010723     c                   if        w_per = ' '
579200060606      * in lingua
579300060606     c                   select
579400060606     c                   when      i45lin = 'I'
579500010626     c                   eval      W_TARPAR = ': ' + wrkdestpg +
579600030318     c                             blk10 + blk10 + blk12
579700060302     c                             + ' ' + %editc(tpditr:'3')
579800010626     C                             + ' minimo...' +
579900010723     c                             %editc(tpdmin:'3')
580000060606     c                   when      i45lin = '2'
580100060606     c                   eval      w_tarpar = ': ' + wrkdestpg +
580200060606     c                             blk10 + blk10 + blk12
580300060606     c                             + ' ' + %editc(tpditr:'3')
580400060606     C                             + ' minimum..' +
580500060606     c                             %editc(tpdmin:'3')
580600060817     c                   when      i45lin = '3'
580700060817     c                   eval      w_tarpar = ': ' + wrkdestpg +
580800060817     c                             blk10 + blk10 + blk12
580900060817     c                             + ' ' + %editc(tpditr:'3')
581000060817     C                             + ' minimum..' +
581100060817     c                             %editc(tpdmin:'3')
581200060817     c                   when      i45lin = '4'
581300060817     c                   eval      w_tarpar = ': ' + wrkdestpg +
581400060817     c                             blk10 + blk10 + blk12
581500060817     c                             + ' ' + %editc(tpditr:'3')
581600060817     c                             + ' minde....' +
581700060817     c                             %editc(tpdmin:'3')
581800060606     c                   endsl
581900010723     c                   else
582000010723      * si percentuale
582100060606      * in lingua
582200060606     c                   select
582300060606     c                   when      i45lin = 'I'
582400010723     c                   eval      W_TARPAR = ': ' +
582500030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
582600030318     c                             blk10 + blk25
582700010723     C                             + ' minimo...' +
582800010723     c                             %editc(tpdmin:'3')
582900060606     c                   when      i45lin = '2'
583000060606     c                   eval      w_tarpar = ': ' +
583100060606     c                             wrkdestpg + ' ' + stpper + ' % ' +
583200060606     c                             blk10 + blk25
583300060606     C                             + ' minimum..' +
583400060606     c                             %editc(tpdmin:'3')
583500060817     c                   when      i45lin = '3'
583600060817     c                   eval      w_tarpar = ': ' +
583700060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
583800060817     c                             blk10 + blk25
583900060817     c                             + ' minimum..' +
584000060817     c                             %editc(tpdmin:'3')
584100060817     c                   when      i45lin = '4'
584200060817     c                   eval      w_tarpar = ': ' +
584300060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
584400060817     c                             blk10 + blk25
584500060817     c                             + ' minde....' +
584600060817     c                             %editc(tpdmin:'3')
584700060606     c                   endsl
584800010723     c                   endif
584900010723      *
585000010625     c                   endif
585100010625      * se c'è solo massimo
585200010625     c                   if        tpdmin = 0 and tpdmax > 0
585300010723      * no percentuale
585400010723     c                   if        w_per = ' '
585500060606      * in lingua
585600060606     c                   select
585700060606     c                   when      i45lin = 'I'
585800010626     c                   eval      W_TARPAR = ': ' + wrkdestpg +
585900030318     c                             blk10 + blk10 + blk12
586000060302     c                             + ' ' + %editc(tpditr:'3')
586100010626     C                             + ' massimo..' +
586200010626     c                             %editc(tpdmax:'3')
586300060606     c                   when      i45lin = '2'
586400060606     c                   eval      w_tarpar = ': ' + wrkdestpg +
586500060606     c                             blk10 + blk10 + blk12
586600060606     c                             + ' ' + %editc(tpditr:'3')
586700060606     C                             + ' maximum..' +
586800060606     c                             %editc(tpdmax:'3')
586900060817     c                   when      i45lin = '3'
587000060817     c                   eval      w_tarpar = ': ' + wrkdestpg +
587100060817     c                             blk10 + blk10 + blk12
587200060817     c                             + ' ' + %editc(tpditr:'3')
587300060817     c                             + ' maximum..' +
587400060817     c                             %editc(tpdmax:'3')
587500060817     c                   when      i45lin = '4'
587600060817     c                   eval      w_tarpar = ': ' + wrkdestpg +
587700060817     c                             blk10 + blk10 + blk12
587800060817     c                             + ' ' + %editc(tpditr:'3')
587900060817     c                             + ' höchste..' +
588000060817     c                             %editc(tpdmax:'3')
588100060606     c                   endsl
588200010723     c                   else
588300010723      * si percentuale
588400060606      * in lingua
588500060606     c                   select
588600060606     c                   when      i45lin = 'I'
588700010723     c                   eval      W_TARPAR = ': ' +
588800030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
588900030318     c                             blk10 + blk25
589000010723     C                             + ' massimo..' +
589100010723     c                             %editc(tpdmax:'3')
589200060606     c                   when      i45lin = '2'
589300060606     c                   eval      w_tarpar = ': ' +
589400060606     c                             wrkdestpg + ' ' + stpper + ' % ' +
589500060606     c                             blk10 + blk25
589600060606     C                             + ' maximum..' +
589700060606     c                             %editc(tpdmax:'3')
589800060817     c                   when      i45lin = '3'
589900060817     c                   eval      w_tarpar = ': ' +
590000060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
590100060817     c                             blk10 + blk25
590200060817     c                             + ' maximum..' +
590300060817     c                             %editc(tpdmax:'3')
590400060817     c                   when      i45lin = '4'
590500060817     c                   eval      w_tarpar = ': ' +
590600060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
590700060817     c                             blk10 + blk25
590800060817     c                             + ' höchste..' +
590900060817     c                             %editc(tpdmax:'3')
591000060606     c                   endsl
591100010723     c                   endif
591200010723      *
591300010625     c                   endif
591400010625      * se c'è minimo e massimo
591500010625     c                   if        tpdmin > 0 and tpdmax > 0
591600010723      * no percentuale
591700010723     c                   if        w_per = ' '
591800060606      * in lingua
591900060606     c                   select
592000060606     c                   when      i45lin = 'I'
592100010626     c                   eval      W_TARPAR = ': ' + wrkdestpg +
592200021205     c                             ' ' + %editc(tpditr:'3') + blk10
592300060908     c                             + ' minimo....' +
592400010626     c                             %editc(tpdmin:'3')
592500010626     C                             + ' massimo..' +
592600010626     c                             %editc(tpdmax:'3')
592700060606     c                   when      i45lin = '2'
592800060606     c                   eval      w_tarpar = ': ' + wrkdestpg +
592900060606     c                             ' ' + %editc(tpditr:'3') + blk10
593000060908     c                             + ' minimum...' +
593100060606     c                             %editc(tpdmin:'3')
593200060606     c                             + ' maximum..' +
593300060606     c                             %editc(tpdmax:'3')
593400060817     c                   when      i45lin = '3'
593500060817     c                   eval      w_tarpar = ': ' + wrkdestpg +
593600060817     c                             ' ' + %editc(tpditr:'3') + blk10
593700060908     c                             + ' minimum...' +
593800060817     c                             %editc(tpdmin:'3')
593900060817     c                             + ' maximum..' +
594000060817     c                             %editc(tpdmax:'3')
594100060817     c                   when      i45lin = '4'
594200060817     c                   eval      w_tarpar = ': ' + wrkdestpg +
594300060817     c                             ' ' + %editc(tpditr:'3') + blk10
594400060908     c                             + ' minde.....' +
594500060817     c                             %editc(tpdmin:'3')
594600060817     c                             + ' höchste..' +
594700060817     c                             %editc(tpdmax:'3')
594800060606     c                   endsl
594900010723     c                   else
595000010723      * si percentuale
595100060606      * in lingua
595200060606     c                   select
595300060606     c                   when      i45lin = 'I'
595400010723     c                   eval      W_TARPAR = ': ' +
595500030318     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
595600060908     c                             + ' minimo..' +
595700010723     c                             %editc(tpdmin:'3')
595800060908     C                             + ' massimo...' +
595900010723     c                             %editc(tpdmax:'3')
596000060606     c                   when      i45lin = '2'
596100060606     c                   eval      w_tarpar = ': ' +
596200060606     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
596300060908     c                             + ' minimum..' +
596400060606     c                             %editc(tpdmin:'3')
596500060606     C                             + ' maximum..' +
596600060606     c                             %editc(tpdmax:'3')
596700060817     c                   when      i45lin = '3'
596800060817     c                   eval      w_tarpar = ': ' +
596900060817     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
597000060908     c                             + ' minimum..' +
597100060817     c                             %editc(tpdmin:'3')
597200060817     c                             + ' maximum..' +
597300060817     c                             %editc(tpdmax:'3')
597400060817     c                   when      i45lin = '4'
597500060817     c                   eval      w_tarpar = ': ' +
597600060817     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
597700060908     c                             + ' minde....' +
597800060817     c                             %editc(tpdmin:'3')
597900060817     c                             + ' höchste..' +
598000060817     c                             %editc(tpdmax:'3')
598100060606     c                   endsl
598200010723     c                   endif
598300010723      *
598400010625     c                   endif
598500010731      *
598600010731     c                   endif
598700010625     c                   leave
598800010625     c                   endif
598900010625     C
599000010625      *
599100010625     c                   enddo
599200010625      *
599300010625     c                   ENDSR
599400010625      *
599500010618      *-------------------------------------------------------------------------
599600010618      * RICREC  CONTROLLO SE ESISTE UN RECORD VALIDO E CON TARIFFA+MINIMO > 0
599700010618      *-------------------------------------------------------------------------
599800010618     c     RICREC        BEGSR
599900010618      *
600000010618     c                   setoff                                       04
600100010625     c                   z-add     *zeros        recval
600200010626     c                   clear                   wrkftc
600300010626     c                   clear                   wrkorp
600400010626     c                   clear                   wrksgl
600500010806     c                   clear                   recsgl
600600010724     c                   clear                   oltre
600700010724     c                   clear                   mintas
600800010801     c                   clear                   savorp1
600900010731     c                   clear                   W_TARPAREST
601000010618      *
601100061122     C  n98ktpt          setll     titpd01l
601200061122     c   98ktpt          setll     tiopd01l
601300010618      *
601400010618     c                   do        *hival
601500061122     C  n98ktpt          reade     titpd01l
601600061122     c   98ktpt          reade     tiopd01l
601700010618      *
601800061122     c                   if        %eof
601900010801      * se non ho il recval valorizzato accendo lo 04
602000010625     c                   if        recval = 0
602100010618     c                   seton                                        04
602200010625     c                   endif
602300010625      *
602400010618     c                   leave
602500010618     c                   endif
602600010618      *
602700010618     c                   if        tpdatb = 'A'
602800010618     c                   iter
602900010618     c                   endif
603000010618      *
603100010618      * se tariffa non valorizzata leggo la successiva
603200010618      * importo e minimo =  a zero
603300010618      * se valorizzata esco per gestire la stampa del dettaglio tariffario
603400010618      *
603500010618     C                   IF        tpditr = 0 and tpdmin = 0
603600010724      * mi salvo il valore dello scaglione precedente per stampare in un even-
603700010806      * tuale oltre nella riga della tariffa particolare se diverso da 99999
603800010806     c                   z-add     tpdsgl        w0050
603900010806     c                   if        w0050 <> 99999
604000010806      *
604100010724     c                   z-add     tpdsgl        oltre
604200010724      * mi salvo il primo scaglione per stampare il minimo tassabile in caso di
604300010724      * tariffa particolare a qle
604400010724     c                   if        mintas = 0
604500010724     c                   z-add     tpdsgl        mintas
604600010724     c                   endif
604700010806      *
604800010806     c                   endif
604900010801      * mi salvo ordinamento regione e provincia la prima volta
605000010801     c                   if        savorp1 = *blanks
605100010801     c                   eval      savorp1 = tpdorp
605200010806      * memorizzo la chiave che mi serve per riaggancare il primo
605300010806      * record valido
605400010806     c                   eval      wrkorp = tpdorp
605500010806     c                   eval      wrkftc = tpdftc
605600010806     c                   eval      wrksgl = tpdsgl
605700010806      *
605800010806     c                   endif
605900010724      *
606000010618     c                   iter
606100010618     c                   else
606200010625     c                   add       1             recval
606300010806      * memorizzo lo scaglioni perc hè nel caso di unisco scaglione valido
606400010806      * stampo unico scaglione solo se scaglione = a 99999
606500010806     c                   eval      recsgl = tpdsgl
606600010806      * memorizzo la chiave che mi serve per riaggancare il primo
606700010806      * record valido se è = a blanks
606800010806     c                   if        recval = 1 and wrkftc = '  '
606900010806     c                   eval      wrkorp = tpdorp
607000010806     c                   eval      wrkftc = tpdftc
607100010806     c                   eval      wrksgl = tpdsgl
607200010806     c                   endif
607300010626      * se trovati + di 1 record non proseguo in lettura
607400010626      *
607500010626     c                   if        recval > 1
607600010626      * mi riposiziono sul prima record valido trovato
607700061122     c  n98ktpdwrk       chain     titpd01l
607800061122     c   98ktpdwrk       chain     tiopd01l
607900010626      *
608000010626     c                   leave
608100010626     c                   else
608200010625     c                   iter
608300010626     c                   endif
608400010626      *
608500010618     c                   endif
608600010618     C*
608700010618    2C                   enddo
608800010801      * se al primo record valido verifico che non ha lo stesso ordinamento
608900010801      * regione e provincia del record a zero forzo per la stampa in dettaglio
609000010801      *
609100010801     c                   if        recval = 1 and tpdorp <> savorp1 and
609200010801     c                             savorp1 <> *blanks
609300010801     c                   eval      recval = 9
609400010806      * mi riposiziono sul prima record valido trovato
609500061122     c  n98ktpdwrk       chain     titpd01l
609600061122     c   98ktpdwrk       chain     tiopd01l
609700010801     c                   clear                   savorp1
609800010801     c                   endif
609900010724      * Se ho trovato un solo record valido verifico lo scaglione trovato
610000010801     c                   if        recval = 1
610100010806     c                   z-add     recsgl        w0050
610200011012     c                   if        w0050 <> 99999 and tpttpg <> '2'
610300010724      * se diverso da 99999 devo gestire il tutto come se avesse + scaglioni
610400011012      * e tipo tariffa non è a spedizione
610500010724     c                   eval      recval = 9
610600010724      * mi riposiziono sul prima record valido trovato
610700061122     c  n98ktpdwrk       chain     titpd01l
610800061122     c   98ktpdwrk       chain     tiopd01l
610900010724     c                   endif
611000010724     c                   endif
611100010618     C*
611200010618     C                   ENDSR
611300010618      *-------------------------------------------------------------------------
611400010618      *  DECTPG DECODIFICA TIPO TARIFFA E SCAGLIONI
611500010618      *-------------------------------------------------------------------------
611600010618     C     DECTPG        BEGSR
611700010618      *
611800010618     c                   setoff                                       09
611900010618      *
612000010618      * decodifico il tipo tariffa
612100010618      *
612200010618     C                   movel(P)  tpttpg        KEY
612300060605     c     tabkeyl       chain     tabel00f
612400010618      *
612500010618     c                   if        %found(tabel00f)
612600010618     c                   movel     TBLUNI        DSTR
612700010618     c                   else
612800010618     c                   clear                   DSTR
612900010618     c                   endif
613000010618      *
613100010618     C                   movel(P)  §TRDE1        DE1TPG
613200010618     C                   movel(P)  §TRDE3        DE3TPG
613300140522      * minimo tassabile
613400140522     c                   z-add     §trata        ctrata
613500010618      * verifico se scaglioni a valore
613600010618     c                   eval      *in09 = (§trtap = 'S')
613700010618      **
613800010618      * SCAGLIONI
613900010618      *
614000010618      * memorizzo gli scaglioni del dettaglio tariffa particolare
614100010618      *
614200010618     c                   EXSR      MEMSCA
614300010618      *
614400010618     c                   ENDSR
614500010618      *-------------------------------------------------------------------------
614600010618      *  MEMSCA CARICO SCAGLIONI TARIFFE PARTICOLARI
614700010618      *-------------------------------------------------------------------------
614800010618     C     MEMSCA        BEGSR
614900010618      *
615000010621     C                   SETOFF                                       089137
615100080901     c                   setoff                                       404138
615200010618      *
615300010618     C                   MOVEA     *ZEROS        SCP
615400080916     C                   MOVEA     *ZEROS        SSLP
615500010621     c                   clear                   sgld
615600010621     c                   clear                   sg1d
615700010621     c                   clear                   sg2d
615800080901     c                   clear                   sg3d
615900010621     c                   clear                   sgla
616000010621     c                   clear                   sg1a
616100010621     c                   clear                   sg2a
616200080901     c                   clear                   sg3a
616300010618      *
616400010618     c                   eval      rotcts = tpdcts
616500010618     c                   eval      rotpro = %subst(tpdorp:5:1)
616600010618     c                   eval      rotreg = %subst(tpdorp:3:2)
616700010618      * SALVO PER LETTURE FUTURE
616800010618     c                   eval      wrkcts = rotcts
616900010618     c                   eval      wrkpro = rotpro
617000010618     c                   eval      wrkreg = rotreg
617100010618      *
617200010618      * CERCO IL DETTAGLIO TARIFFARIO
617300010618      *
617400061122     C  n98ktpt          setll     titpd01l
617500061122     c   98ktpt          setll     tiopd01l
617600010618      *
617700010621    1c                   do        *hival
617800061122     C  n98ktpt          reade     titpd01l
617900061122     c   98ktpt          reade     tiopd01l
618000010618      *
618100061122    2c                   if        %eof
618200010618     c                   seton                                        04
618300010618     c                   leave
618400010621    2c                   endif
618500010618      *
618600010621    2c                   if        tpdatb = 'A'
618700010618     c                   iter
618800010621    2c                   endif
618900010618      *
619000010618     c                   exsr      regdet
619100010618     c   99              goto      FINE
619200010618     c                   exsr      prodet
619300010618     c   99              goto      FINE
619400010618      *
619500010618      * carico il primo scaglione
619600010618      *
619700010621     C                   z-add     tpdsgl        SCP(1)
619800010621      *
619900010801      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
620000010801      * a valore
620100080901     c                   if        not *in09 or tpdftc = 'f'
620200010801      *
620300010621     c                   move      tpdsgl        w0030
620400010621     c                   z-add     tpdsgl        w0050
620500010621      * se scaglione 99999 non controllo i decimali
620600010621    2c                   if        w0050 <> 99999 and w0030 > 0
620700010621     c                   seton                                        91
620800010621    2c                   endif
620900010621      * se con decimali verifico se valorizzato dal secondo in poi
621000010621    2c                   if        *in91
621100080901     c                   z-add     w0030         w0010
621200010621     c                   z-add     w0030         w0020
621300010621    3c                   if        w0020 > 0
621400010621     c                   seton                                        37
621500010621    3c                   endif
621600080901    3c                   if        w0010 > 0
621700080901     c                   seton                                        38
621800080901    3c                   endif
621900080910      * se si tratta di fuel surcharge faccio sempre 3 decimali
622000080910     c                   If        tpdftc = 'f'
622100080910     c                   seton                                        3837
622200080910    2c                   endif
622300010621    2c                   endif
622400010801      *
622500010801     c                   endif
622600050324
622700050324      * Se scaglione non è 99999
622800050324      * controllo che non sia maggiore di 9999 xchè non ci sta in stampa
622900050324      * in questo caso ERRORE ed esco dal programma
623000050324     c                   z-add     tpdsgl        w0050
623100050324     c                   If        w0050 <> 99999 and tpdsgl > 9999
623200050324     c                   move      msg(16)       errmsg
623300050324     c                   exsr      staerr
623400050324     c                   Goto      FINE
623500050324     c                   EndIf
623600010622      *
623700010622     C                   LEAVE
623800010622      *
623900010622    1C                   ENDDO
624000010622      *
624100010618     C                   z-add     1             K
624200010618      *
624300010618      * LETTURA TPD CON KEY KTPT CON LO STESSO CODICE TASSAZIONE
624400010618      *
624500010621    2C                   DO        *HIVAL
624600010618      *
624700061122     C  n98ktpt          reade     titpd01l
624800061122     c   98ktpt          reade     tiopd01l
624900010618      * se fine lettura
625000061122     C                   IF        %eof
625100010618     c                   leave
625200010618     c                   endif
625300010618      * se codice tassazione diverso
625400010618     c                   if        tpdcts <> rotcts
625500010618     c                   leave
625600010618     c                   endif
625700010618      * se annullato leggo il successivo
625800010618     c                   if        tpdatb <> ' '
625900010618     c                   iter
626000010618     c                   endif
626100010618      *
626200010618     C                   add       1             K
626300010618      * fino a 18 scaglioni carico
626400010621    3C                   if        k <= 18
626500010618      *
626600010621     C                   z-add     tpdsgl        SCP(K)
626700010621      *
626800010801      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
626900010801      * a valore
627000080901     c                   if        not *in09 or tpdftc = 'f'
627100010801      *
627200010621     c                   move      tpdsgl        w0030
627300010621     c                   z-add     tpdsgl        w0050
627400010621      * se scaglione 99999 non controllo i decimali
627500010621     c                   if        w0050 <> 99999 and w0030 > 0
627600010621     c                   seton                                        91
627700010621     c                   endif
627800010621      * se con decimali verifico se valorizzato dal secondo in poi
627900010621     c                   if        *in91
628000010621     c                   z-add     w0030         w0020
628100080901     c                   z-add     w0030         w0010
628200010621     c                   if        w0020 > 0
628300010621     c                   seton                                        37
628400010621     c                   endif
628500080901    3c                   if        w0010 > 0
628600080901     c                   seton                                        38
628700080901    3c                   endif
628800010621     c                   endif
628900010801      *
629000010801     c                   endif
629100050324
629200050324      * Se scaglione non è 99999
629300050324      * controllo che non sia maggiore di 9999 xchè non ci sta in stampa
629400050324      * in questo caso ERRORE ed esco dal programma
629500050324     c                   z-add     tpdsgl        w0050
629600050324     c                   If        w0050 <> 99999 and tpdsgl > 9999
629700050324     c                   move      msg(16)       errmsg
629800050324     c                   exsr      staerr
629900050324     c                   Goto      FINE
630000050324     c                   EndIf
630100010618      *
630200010621   x3C                   ELSE
630300010618      *
630400010618     C                   move      msg(15)       errmsg
630500010618     C                   exsr      staerr
630600050914      * Per errore esco dal pgm
630700050914     c                   Goto      FINE
630800010621    3C                   endif
630900010618      *
631000010621    2C                   enddo
631100010618      * memorizzo il numero scaglioni caricati
631200010618     C                   z-add     k             wnumsc
631300010618      *
631400010621      * preparo la schiera degli scaglioni da stampare
631500010621      *
631600010621     c                   z-add     1             k
631700010621     c                   clear                   k1
631800010621      *
631900010621    2c                   dow       scp(k) > 0
632000010621      * preparo lo scaglione DAL addizionato di 1 o 0,1 o 0,01 in base ai decima
632100010621      * li
632200010621     c                   z-add     scp(k)        w0050
632300010621      *
632400010621      * verifico se scaglione 99999
632500010621    3c                   if        w0050 <> 99999
632600010621      *
632700010621    4c                   if        k1 > 0
632800010621      * se non ha decimali
632900010621     c                   if        not *in91
633000010622     C                   eval      sgld(k) = scp(k1) + 1
633100010621     c                   endif
633200010621      *
633300080901     c                   if        *in91 and not *in37 and not *in38
633400010621      * se ha 1 decimale
633500010622     C                   eval      sg1d(k) = scp(k1) + 0,1
633600010621     c                   endif
633700010621      *
633800080901     c                   if        *in91 and *in37 and not *in38
633900010621      * se ha + di 1 decimale
634000010622     C                   eval      sg2d(k) = scp(k1) + 0,01
634100010621     c                   endif
634200080901      *
634300080901     c                   if        *in91 and *in37 and  *in38
634400080901      * se ha 3 decimale
634500080901     C                   eval      sg3d(k) = scp(k1) + 0,001
634600080901     c                   endif
634700010622      *
634800010622     c                   else
634900010622    4c                   if        k =  1
635000010622      * se non ha decimali
635100010622     c                   if        not *in91
635200010622     C                   eval      sgld(k) =  1
635300010622     c                   endif
635400010622      *
635500080901     c                   if        *in91 and not *in37 and not *in38
635600010622      * se ha 1 decimale
635700010622     C                   eval      sg1d(k) =  0,1
635800010622     c                   endif
635900010622      *
636000080901     c                   if        *in91 and *in37 and not *in38
636100010622      * se ha + di 1 decimale
636200010622     C                   eval      sg2d(k) =  0,01
636300010622     c                   endif
636400080901      *
636500080901     c                   if        *in91 and *in37 and *in38
636600080901      * se ha 3 decimali
636700080901     C                   eval      sg3d(k) =  0,001
636800080901     c                   endif
636900010621      *
637000010622     c                   endif
637100010622      *
637200010621    4c                   endif
637300010621      *
637400010621      * costruisco la schiera dell'applicazione tariffa finita che va sotto gli
637500010621      * scaglioni da/a
637600010621      * tariffa e in mancanza di quest'ultima prendo dalla tabella TR
637700010621      *
637800010621      * muovo unità di misura della tariffa nella schiera
637900060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
638000060925     c                   move      tpttpg        wctr1
638100151117      * chiodo a programma per la nuova tariffa particolare t= Diritto di
638200151117      * chiamata ritiro telefonico l'unità di misura "a ritiro" altrimenti
638300151117      * prenderebbe a spedizione
638400151117     c                   if        tptftc = 't'
638500151117     c                   eval      wctr1= 'R'
638600151117     c                   endif
638700060922     c                   exsr      sr_atfk
638800140522      * faccio i vari ragionamenti in base al valore della tariffa finita
638900140522      *
639000140522      * se scaglioni senza decimali
639100140522     c                   if        not *in91
639200140522
639300140522     c                   z-add     ctrata        wata
639400140522      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
639500140522      * scrivo "A SPED."
639600140522     c                   if        (wata > 0 and sgl(k) <= wata)
639700140522      * richiamo la routine (dove ho tutto fisso e in un unico posto)
639800140522     c                   clear                   wctr1
639900140522     c                   exsr      sr_atfk
640000140522     c                   endif
640100140522      *
640200140522     c                   else
640300140522      * scaglioni con decimali
640400140522     c                   z-add     ctrata        wata1
640500140522     c                   z-add     ctrata        wata2
640600140522      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
640700140522      * scrivo "A SPED."  1 decimale
640800140522     c                   if        (wata1 > 0 and sgl(k)<= wata1 and not *in37)
640900140522      * richiamo la routine (dove ho tutto fisso e in un unico posto)
641000140522     c                   clear                   wctr1
641100140522     c                   exsr      sr_atfk
641200140522     c                   endif
641300140522      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
641400140522      * scrivo "A SPED."  2 decimali
641500140522     c                   if        (wata2 > 0 and sgl(k) <= wata2 and *in37)
641600140522      * richiamo la routine (dove ho tutto fisso e in un unico posto)
641700140522     c                   clear                   wctr1
641800140522     c                   exsr      sr_atfk
641900140522     c                   endif
642000140522      *                                                      *in91 on
642100140522     c                   endif
642200010621      *
642300010621     c                   move      '/'           sglb(k)
642400010621      * preparo lo scaglione AL
642500010621      *
642600010621      * no decimali
642700010621    4c                   if        not *in91
642800010621     c                   z-add     scp(k)        sgla(k)
642900010621    4c                   endif
643000010621      * 1 decimale
643100080901    4c                   if        *in91 and not *in37  and not *in38
643200010621     c                   z-add     scp(k)        sg1a(k)
643300010621    4c                   endif
643400010621      * 2 decimali
643500080901    4c                   if        *in91 and *in37 and not *in38
643600010621     c                   z-add     scp(k)        sg2a(k)
643700010621    4c                   endif
643800080901      * 3 decimali
643900080901    4c                   if        *in91 and *in37 and  *in38
644000080901     c                   z-add     scp(k)        sg3a(k)
644100080901    4c                   endif
644200010621      *
644300010621   x3c                   else
644400010621      * scaglione 99999 è sempre maggiore della tariffa finita
644500060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
644600060925     c                   move      tpttpg        wctr1
644700151119      * chiodo a programma per la nuova tariffa particolare t= Diritto di
644800151119      * chiamata ritiro telefonico l'unità di misura "a ritiro" altrimenti
644900151119      * prenderebbe a spedizione
645000151119     c                   if        tptftc = 't'
645100151119     c                   eval      wctr1= 'R'
645200151119     c                   endif
645300060922     c                   exsr      sr_atfk
645400010621      *
645500010621    3c                   endif
645600010621      *
645700010621     c                   add       1             k
645800010621     c                   add       1             k1
645900010621      *
646000010621      * se maggiore di 18 esco
646100010621    3c                   if        k > 18
646200010621     c                   leave
646300010621    3c                   end
646400010621      *
646500010621    2c                   enddo
646600010621      *
646700010621      * se c'è solo uno scaglione non scrivo niente sotto
646800021220      * solo se lo scaglione è 99999
646900010621      *
647000021220     c                   if        k1 <= 1 and scp(k1) >= 99999
647100010621     c                   clear                   atf(1)
647200010621     c                   endif
647300010621      *
647400010621     c                   clear                   savsgl
647500010618      *
647600130905     C                   endsr
647700010618     C*
647800010619     C*------------------------------------------------------------------------
647900010619     C*  DECSCA DECODIFICO SCAGLIONI TARIFFE PARTICOLARI
648000010619     C*-------------------------------------------------------------------------
648100010619     C     DECSCA        BEGSR
648200010619      * imposto i campi di rottura salvati
648300010619     C                   MOVEL     WRKREG        ROTREG
648400010619     C                   MOVE      WRKPRO        ROTPRO
648500010619     C                   MOVE      WRKCTS        ROTCTS
648600010619     C                   MOVE      WRKCTS        TPDCTS
648700010619     C                   MOVE      WRKREG        COM04
648800010619     C                   MOVEL     COM04         TPDORP
648900010619     C                   MOVE      WRKPRO        TPDORP
649000010619      * pulisco le schiere
649100010619     C                   EXSR      REGDET
649200010619     C                   EXSR      PRODET
649300010625      *
649400010628      *
649500010628     c                   if        *in80 or curline > 56
649600010628     c                   write     ta49pfin
649700010628     c                   write     ta49pPP
649800010628     c                   setoff                                       80
649900010628     c                   endif
650000010625      *
650100010622
650200010622      * stampo la dicitura della tariffa
650300020109      * se tariffa che viene stampata in modo standard recupero la decodifica
650400020109      * dall'estensione della tabella 1P la tabella SP
650500020109      *
650600020109     c                   if        tst(y) = ' '
650700020109     c                   movel(p)  edtc(y)       comstd
650800020109     c                   clear                   comdstd
650900020109     c                   write     ta49pstd
651000020109      *
651100020109     c                   else
651200020109      * elevazione limite risarcibile
651300010625    3c                   if        savftc = 'R'
651400011001     c                   write     ta49pma
651500010625    3c                   endif
651600020109      * assicurazione per conto
651700010625    3c                   if        savftc = 'C' and not *in21
651800010625     c                   write     ta49pma
651900010625    3c                   endif
652000010625    3c                   if        savftc = 'C' and *in21
652100011001     c                   write     ta49pma
652200010625    3c                   endif
652300020109      *
652400020109     c                   endif
652500010622      * valorizzo i campi del prtf scaglioni
652600010622     c                   clear                   desum
652700010622     c                   clear                   dissca
652800010622     c                   clear                   prtatf
652900010622     c                   eval      desum = de1tpg
653000010622     c                   eval      ds3um = de3tpg
653100151119
653200151119      /free
653300151119       //?Decodifica personalizzata del tipo tariffa
653400151119         SELECT;
653500151119       //?Italiano
653600151119         WHEN  I45lin = 'I';
653700151119       // Tariffa particolare Diritto Fatturazione
653800151119           IF  TPTftc = '§';
653900151119             desum = 'A FATTURA';
654000151119             ds3um = 'A FATTURA';
654100151119           ENDIF;
654200151119       // Tariffa particolare Gestione Bancali a Rendere
654300151119           IF  TPTftc = '=';
654400151119             desum = 'A BANCALE';
654500151119             ds3um = 'A BANCALE';
654600151119           ENDIF;
654700151119       // Tariffa particolare Diritto di chiamata prenotazione
654800151119       // ritiro telefonico
654900151119           IF  TPTftc = 't';
655000151119             desum = 'A RITIRO';
655100151119             ds3um = 'A RITIRO';
655200151119           ENDIF;
655300151119       //?Inglese
655400151119         WHEN  I45lin = '2';
655500151119       // Tariffa particolare Diritto Fatturazione
655600151119           IF  TPTftc = '§';
655700151119             desum = 'PER INVOICE';
655800151119             ds3um = 'PER INVOICE';
655900151119           ENDIF;
656000151119       // Tariffa particolare Gestione Bancali a Rendere
656100151119           IF  TPTftc = '=';
656200151119             desum = 'PER PALLET';
656300151119             ds3um = 'PER PALLET';
656400151119           ENDIF;
656500151119       // Tariffa particolare Diritto di chiamata prenotazione
656600151119       // ritiro telefonico
656700151119           IF  TPTftc = 't';
656800151119             desum = 'PER PICK-UP';
656900151119             ds3um = 'PER PICK-UP';
657000151119           ENDIF;
657100151119       //?Francese
657200151119         WHEN  I45lin = '3';
657300151119       // Tariffa particolare Diritto Fatturazione
657400151119           IF  TPTftc = '§';
657500151119             desum = 'A FATTURA';
657600151119             ds3um = 'A FATTURA';
657700151119           ENDIF;
657800151119       // Tariffa particolare Gestione Bancali a Rendere
657900151119           IF  TPTftc = '=';
658000151119             desum = 'A LA PALLETE';
658100151119             ds3um = 'A LA PALLETE';
658200151119           ENDIF;
658300151119       // Tariffa particolare Diritto di chiamata prenotazione
658400151119       // ritiro telefonico
658500151119           IF  TPTftc = 't';
658600151119             desum = 'PAR RETRAIT';
658700151119             ds3um = 'PAR RETRAIT';
658800151119           ENDIF;
658900151119       //?Tedesco
659000151119         WHEN  I45lin = '4';
659100151119       // Tariffa particolare Diritto Fatturazione
659200151119           IF  TPTftc = '§';
659300151119             desum = 'A FATTURA';
659400151119             ds3um = 'A FATTURA';
659500151119           ENDIF;
659600151119       // Tariffa particolare Gestione Bancali a Rendere
659700151119           IF  TPTftc = '=';
659800151119             desum = 'N.PALETTE';
659900151119             ds3um = 'N.PALETTE';
660000151119           ENDIF;
660100151119       // Tariffa particolare Diritto di chiamata prenotazione
660200151119       // ritiro telefonico
660300151119           IF  TPTftc = 't';
660400151119             desum = 'N. BUCHUNG';
660500151119             ds3um = 'N. BUCHUNG';
660600151119           ENDIF;
660700151119         ENDSL;
660800151119      /end-free
660900151119
661000010622      * stampo la testata
661100010622     c                   EXSR      SR_STATESDET
661200010622      *
661300010622      *
661400010619      *
661500010619     C                   ENDSR
661600010619     O*---------------------------------------------------------------*
661700010619     O* LEGGO DETTAGLIO DI TPD
661800010619     O*---------------------------------------------------------------*
661900010619     C     LEGDET        BEGSR
662000010619      *
662100061122     C  n98KTPT          SETLL     TITPD01l
662200061122     c   98ktpt          setll     tiopd01l
662300010619      *
662400010620    1C                   DO        *HIVAL
662500061122     C  n98KTPT          READE     TITPD01l
662600061122     c   98ktpt          reade     tiopd01l
662700010619      *
662800061122     C                   IF        %EOF
662900010619     C                   LEAVE
663000010619     C                   ENDIF
663100010619      *
663200010619     C                   IF        TPDATB = 'A'
663300010619     C                   ITER
663400010619     C                   ENDIF
663500010619      *
663600010619      * SE SCAGLIONE 9999 VERIFICO SE E' L'ULTIMO
663700010621     C                   Z-ADD     TPDSGL        NUM4
663800010620    2C     NUM4          IFEQ      9999
663900010621     C     TPDSGL        ANDNE     *ALL'9'
664000010619      *
664100010621     C                   MOVEL     TPDORP        SAVORP
664200010621     C                   MOVEL     TPDSGL        SAVSGL
664300010619      *
664400010619     C                   SETOFF                                       35
664500010619      * verifico se esiste un'altro scaglione per lo stesso ORP
664600010620    3C                   DO        *HIVAL
664700061122     C  n98KTPD          READE     TITPD01l                               35
664800061122     c   98ktpd          reade     tiopd01l                               35
664900010619      * se annullato leggo il successivo
665000010619     C                   IF        NOT *IN35 AND TPDATB = 'A'
665100010619     C                   ITER
665200010619     C                   ENDIF
665300010619      * se non trovato o non trovato riaggancio il record precedente
665400061122     C  n98KTPD2         CHAIN     TITPD01l
665500061122     c   98ktpd2         chain     tiopd01l
665600010619      *
665700010619     C                   LEAVE
665800010619      *
665900010620    3C                   ENDDO
666000010619     C*
666100010619     C* NON C'E' ALTRO SCAGLIONE
666200010620    2C                   ENDIF
666300010619     C*
666400010619     C                   Z-ADD     1             K2
666500080916     C                   Z-ADD     TPDSGL        wsgl83
666600010619      * cerco lo scaglione nella schiera
666700080916     C     wsgl83        LOOKUP    SCP(1)                                 35
666800010619      * se non trovato cerco lo scaglione nella schiera stampa
666900080916     C   35wsgl83        LOOKUP    SSLP(K2)                               90
667000010619      * se non trovato nella schiera di sola stampa ma trovato nella schiera SCP
667100010619      * torno a leggere
667200010619     c                   if        *in35 and not *in90
667300010619     c                   iter
667400010619     c                   endif
667500010619      *
667600010619      * se non trovato nella schiera scp ERRORE  scaglione non previsto
667700010620    2C                   IF        not *in35
667800010619     C                   MOVEL     MSG(14)       ERRMSG
667900010619     C                   EXSR      STAERR
668000050914      * Per errore esco dal pgm
668100050914     c                   Goto      FINE
668200010619      *
668300010620   X2C                   ELSE
668400010619     c                   eval      compro = %subst(tpdorp:5:1)
668500010619     c                   eval      comreg = %subst(tpdorp:3:2)
668600010619      * CAMBIATA PROVINCIA
668700010622    3C                   IF        compro <>  rotpro or comreg <> rotreg
668800010619     C                   EXSR      PROTPT
668900010619     C                   EXSR      PRODET
669000010619     C   99              GOTO      FINE
669100010619     C                   MOVE      COMPRO        ROTPRO
669200010620    3C                   END
669300010619      * CAMBIATA REGIONE
669400010620    3C                   IF        COMREG <> ROTREG
669500010619     C                   EXSR      REGTPT
669600010619      *
669700010619     C                   EXSR      REGDET
669800010619     C   99              GOTO      FINE
669900010619     C                   MOVEL     COMREG        ROTREG
670000010620    3C                   END
670100010619      *
670200010619     C                   Z-ADD     TPDITR        TAP(K2)
670300010619     C                   Z-ADD     TPDMIN        TPN(K2)
670400010619     C                   Z-ADD     TPDMAX        TPX(K2)
670500010619     C                   MOVEL     TPDAIN        SAVAIP
670600010619    2C                   END
670700010619      *
670800010619    1C                   ENDDO
670900010620      *
671000010619     C                   EXSR      PROTPT
671100010619     C                   EXSR      REGTPT
671200010621      *
671300010619     C                   ENDSR
671400010620     O*---------------------------------------------------------------*
671500010620     O* PROTPT CAMBIO PROVINCIA
671600010620     O*---------------------------------------------------------------*
671700010620     C     PROTPT        BEGSR
671800010620     C                   Z-ADD     1             C
671900010621     C     *BLANKS       LOOKUP    BIG(C)                                 35
672000010620     C     *IN35         IFEQ      '0'
672100010620     C                   MOVE      MSG(10)       ERRMSG
672200010620     C                   EXSR      STAERR
672300050914      * Per errore esco dal pgm
672400050914     c                   Goto      FINE
672500010620     C                   ELSE
672600010621      *
672700010621     c                   do        6             b
672800010621     c                   move      tap(b)        com(b)
672900010621     c                   end
673000010621      *
673100010621     c                   movea     com           big(c)
673200010621      *
673300010621     c                   do        6             b
673400010621     c                   move      tpn(b)        com(b)
673500010621     c                   end
673600010621      *
673700010621     c                   movea     com           bmn(c)
673800010621      *
673900010621     c                   do        6             b
674000010621     c                   move      tpx(b)        com(b)
674100010621     c                   end
674200010621      *
674300010621     c                   movea     com           bmx(c)
674400010621     c
674500010620     C                   MOVE      CODSIG        BSI(C)
674600010620     C                   MOVE      SAVAIP        AIP(C)
674700010620     C                   MOVE      CODORP        BRG(C)
674800050204     c                   move      desprv        dprv(c)
674900050303     c                   Eval      nodes(c) = nostdes
675000010620     C                   END
675100010620     C                   MOVEL     ' '           SAVAIP
675200010620     C                   ENDSR
675300010620      *-------------------------------------------------------------------------
675400010620      * REGTPT CAMBIO REGIONE
675500010620      *-------------------------------------------------------------------------
675600010620     C     REGTPT        BEGSR
675700010620     C                   Z-ADD     0             NUM4
675800010620     C                   Z-ADD     1             K
675900110216
676000110216     c                   clear                   savk
676100110216
676200010620      * preparo le schiere che devono essere stampate
676300050616    1C                   DO        15            K
676400010620    2C     BRG(K)        IFNE      '  '
676500010622    3C     BRG(K)        IFEQ      '9'
676600110216     c                   eval      savk = k
676700010620      *
676800010622     C     K             IFEQ      1
676900010621     C                   MOVEL     *all'.'       PDE(K)
677000010620     C                   ELSE
677100110216     C                   EVAL      PDE(k) = '.............................'
677200010620     C                   END
677300010620     C                   ADD       1             NUM4
677400010620     C*
677500010622    3C                   ELSE
677600010620     C*
677700010620     C                   MOVEA     *BLANKS       WCT
677800010620     C                   Z-ADD     1             C
677900010620     C                   MOVE      BSI(K)        WCT(C)
678000050204     c                   Eval      wragprv = *Off
678100010621      *
678200010621      * città
678300010621     C                   ADD       1             P
678400010621     C                   MOVEA     BIG(K)        COMO42
678500010621     C                   MOVEA     COMO42        WIN(P)
678600010621      * minimo
678700010621     C                   MOVEA     BMN(K)        CMNO42
678800010621     C                   MOVEA     CMNO42        WMN(P)
678900010621      * massimo
679000010621     C                   MOVEA     BMX(K)        CMXO42
679100010621     C                   MOVEA     CMXO42        WMX(P)
679200010620     C                   MOVEL     AIP(K)        SAVAIP            1
679300010620      *
679400010620      * MEMORIZZO LE ALTRE PROVINCIE CHE HANNO LE STESSE TARIFFE
679500010620     C     K             ADD       1             H
679600050616    4C     H             DO        15            H
679700010620      *
679800010620     C     BRG(H)        IFNE      '9'
679900010621     C     como42        ANDEQ     big(H)
680000010621     C     cmno42        ANDEQ     bmn(H)
680100010621     C     cmxo42        ANDEQ     bmX(H)
680200010620     C     SAVAIP        ANDEQ     AIP(H)
680300010625      *
680400010625      * se isola controllo anche che non siano le province sondrio e como
680500010625     c                   IF        (*in06  and bsi(h)<>'CO' and bsi(h)<>'SO')
680600010625     c                             or not *in06
680700010620     C                   ADD       1             C
680800010620     C                   MOVEL     '/'           WCT(C)
680900010620     C                   MOVE      BSI(H)        WCT(C)
681000010621     C                   MOVEL     *BLANKS       big(H)
681100010621     C                   MOVEL     *BLANKS       bmn(H)
681200010621     C                   MOVEL     *BLANKS       bmx(H)
681300010620     C                   MOVEL     *BLANKS       BRG(H)
681400050204     c                   Eval      wragprv = *On
681500010625     c                   endif
681600010625      *
681700010620     C                   ENDIF
681800010620      *
681900010620   e4C                   ENDDO
682000010620     C*
682100010620     C* MEMORIZZO LA SCHIERA DELLE PROVINCIE CON STESSE TARIFFE
682200010620     C                   ADD       1             C
682300010621     C                   MOVEA     *all'.'       WCT(C)
682400010621     c                   movea     wct           como29
682500010621     C                   MOVEA     como29        PDE(K)
682600010625      * se sto stampando le isole recupero la decodifica delle isole
682700010625     c                   if        %subst(como29:2:2) = 'CO' and *in06
682800010625     c                   eval      PDE(k) = 'CAMPIONE D''ITALIA .........'
682900010625     C                   ENDIF
683000010625      *
683100010625     c                   if        %subst(como29:2:2) = 'SO' and *in06
683200010625     c                   eval      PDE(k) = 'LIVIGNO ....................'
683300010625     C                   ENDIF
683400050204
683500050204      * se tariffa estera e non ho raggruppato più province devo stampare
683600050204      * la descrizione della provincia e non il codice
683700050204     c                   If        wragprv = *Off and (*in02 or *in25)
683800050204     c                   Eval      como29 = *all'.'
683900050303     c                   If        nodes(k) <> 'N'
684000050204     c                   Eval      %subst(como29:1:(%len(%trim(dprv(k))))) =
684100050204     c                             %trim(dprv(k))
684200050303     c                   EndIf
684300050204     c                   Move      como29        pde(k)
684400050204     c                   EndIf
684500010620     C*
684600010622   e3C                   END
684700010620   e2C                   END
684800010620   e1C                   END
684900010620      ***
685000010620      *** STAMPA REGIONE
685100010620      ***
685200110216
685300110216     c                   clear                   giro              1 0
685400110216
685500110216      * stampo per prima 'ALTRI' cioè la regione
685600110216     c                   if        savk > 1
685700110216     c                   eval      k = savk
685800110216     c                   else
685900110216     c                   eval      k = 1
686000110216     c                   endif
686100110216
686200110216     c     rifai1        tag
686300010620      *
686400110216 1   c                   dow       brg(k) <> *blanks
686500110216     c                   if        k = savk  and giro > 0
686600110216     c                   leave
686700110216     c                   endif
686800010621      *
686900010622     c                   movea     BIG(k)        com
687000010621     c                   z-add     1             b
687100110216 2   c                   do        6             b
687200010621     c                   move      com(b)        tap(b)
687300010620      * se tariffa a valore di fianco al valore metto la percentuale
687400110216 3   c                   if        *in09 and tap(b) > 0
687500010622     C                   MOVEL     '%'           PER(B)
687600110216 3   C                   END
687700010620      *
687800110216 2   C                   END
687900010620      * applicazione inoltro (forse non si usa)
688000110216 2   C     AIP(K)        IFEQ      'S'
688100010620     C                   MOVEL     'SI'          STAAIP            2
688200010620     C                   ELSE
688300010620     C                   MOVEL     'NO'          STAAIP
688400110216 2   C                   END
688500010620      * VERIFICO SE C'E' IL MINIMO E IL MASSIMO
688600010620      *
688700010620     C                   SETOFF                                       7586
688800010621      *
688900010622    2C                   IF        BMN(K) <> *BLANKS AND BMN(K) <> *ZEROS
689000010621     C                   SETON                                        75
689100010621      *
689200010622     C                   MOVEA     BMN(K)        COM
689300010621      *
689400010621     C                   DO        6             B
689500010621     C                   MOVE      COM(B)        TPN(B)
689600010621     C                   ENDDO
689700010621      *
689800110216    2C                   ENDIF
689900010621      **
690000010622    2C                   IF        BMX(K) <> *BLANKS AND BMX(K) <> *ZEROS
690100010621     C                   SETON                                        86
690200010621      **
690300010622     C                   MOVEA     BMX(K)        COM
690400010621      *
690500010621     C                   DO        6             B
690600010621     C                   MOVE      COM(B)        TPX(B)
690700010621     C                   ENDDO
690800010621      *
690900010621    2C                   ENDIF
691000010620      *
691100010620      * gestione stampa tariffa particolare
691200010620      *
691300010620     c                   EXSR      SR_STPPAR
691400010621      *
691500010621      *
691600110216     c                   eval      k = k+1
691700010620    1C                   ENDDO
691800110216
691900110216      * se ho stampato 'Altri' torno a loopare per stampare il resto
692000110216     c                   if        savk > 1 and giro = 0
692100110216     c                   eval      k = 1
692200110216     c                   eval      giro = 1
692300110216     c                   goto      rifai1
692400110216     c                   endif
692500010620      *
692600010621     C                   MOVEA     *blanks       big
692700010621     C                   MOVEA     *blanks       bmn
692800010621     C                   MOVEA     *blanks       bmx
692900010621     C                   MOVEA     *blanks       win
693000010621     C                   MOVEA     *blanks       wmn
693100010621     C                   MOVEA     *blanks       wmx
693200010620     C                   MOVEA     *BLANKS       BSI
693300010620     C                   MOVEA     *BLANKS       AIP
693400010620     C                   MOVEA     *BLANKS       PER
693500010620     C                   MOVEA     *ZEROS        TAP
693600010620     C                   MOVEA     *ZEROS        TPN
693700010620     C                   MOVEA     *ZEROS        TPX
693800010620     C                   CLEAR                   BRG
693900010620      *
694000010620     C                   ENDSR
694100010620      *---------------------------------------------------------------*
694200010620      * SR_STPPAR STAMPA TARIFFE PARTICOLARI
694300010620      *---------------------------------------------------------------*
694400010620     C     SR_STPPAR     BEGSR
694500010620      * valorizzo il dettaglio tariffario in stampa
694600010620      *
694700010620     c                   clear                   ta49p03
694800010620      *
694900010620     c                   eval      p03des = desreg + ' ' + pde(k)
695000010620     c                   clear                   desreg
695100010620      * verifico se sono tutti valorizzati
695200010622     c     tap(1)        comp      0                                  61
695300010622     c     tap(2)        comp      0                                  62
695400080925      * se si tratta della tariffa particolare fuel  se valorizzato il secondo
695500080925      * scaglione valorizzo anche il precedente ed anche la %
695600080925     c                   If        savftc = 'f ' and *in62
695700080925     c                   eval      *in61 = *on
695800080925     c                   eval      per(1)= '%'
695900080925     c                   endif
696000010622     c     tap(3)        comp      0                                  63
696100080925      * se si tratta della tariffa particolare fuel  se valorizzato il terzo
696200080925      * scaglione valorizzo anche i precedenti ed anche le %
696300080925     c                   If        savftc = 'f ' and *in63
696400080925     c                   eval      *in61 = *on
696500080925     c                   eval      *in62 = *on
696600080925     c                   eval      per(1)= '%'
696700080925     c                   eval      per(2)= '%'
696800080925     c                   endif
696900010622     c     tap(4)        comp      0                                  64
697000080925      * se si tratta della tariffa particolare fuel  se valorizzato il quarto
697100080925      * scaglione valorizzo anche i precedenti ed anche le %
697200080925     c                   If        savftc = 'f ' and *in64
697300080925     c                   eval      *in61 = *on
697400080925     c                   eval      *in62 = *on
697500080925     c                   eval      *in63 = *on
697600080925     c                   eval      per(1)= '%'
697700080925     c                   eval      per(2)= '%'
697800080925     c                   eval      per(3)= '%'
697900080925     c                   endif
698000010622     c     tap(5)        comp      0                                  65
698100080925      * se si tratta della tariffa particolare fuel  se valorizzato il quinto
698200080925      * scaglione valorizzo anche i precedenti ed anche le %
698300080925     c                   If        savftc = 'f ' and *in65
698400080925     c                   eval      *in61 = *on
698500080925     c                   eval      *in62 = *on
698600080925     c                   eval      *in63 = *on
698700080925     c                   eval      *in64 = *on
698800080925     c                   eval      per(1)= '%'
698900080925     c                   eval      per(2)= '%'
699000080925     c                   eval      per(3)= '%'
699100080925     c                   eval      per(4)= '%'
699200080925     c                   endif
699300010622     c     tap(6)        comp      0                                  66
699400080925      * se si tratta della tariffa particolare fuel  se valorizzato il sesto
699500080925      * scaglione valorizzo anche i precedenti ed anche le %
699600080925     c                   If        savftc = 'f ' and *in66
699700080925     c                   eval      *in61 = *on
699800080925     c                   eval      *in62 = *on
699900080925     c                   eval      *in63 = *on
700000080925     c                   eval      *in64 = *on
700100080925     c                   eval      *in65 = *on
700200080925     c                   eval      per(1)= '%'
700300080925     c                   eval      per(2)= '%'
700400080925     c                   eval      per(3)= '%'
700500080925     c                   eval      per(4)= '%'
700600080925     c                   eval      per(5)= '%'
700700080925     c                   endif
700800010620      *
700900010620     c                   z-add     tap(1)        tar01
701000010620     c                   z-add     tap(2)        tar02
701100010620     c                   z-add     tap(3)        tar03
701200010620     c                   z-add     tap(4)        tar04
701300010620     c                   z-add     tap(5)        tar05
701400010620     c                   z-add     tap(6)        tar06
701500010621      * SE TARIFFA A VALORE STAMPO LA %
701600010621    2C                   IF        *IN09
701700010621     C                   MOVE      per(1)        PER01
701800010621     C                   MOVE      per(2)        PER02
701900010621     C                   MOVE      per(3)        PER03
702000010621     C                   MOVE      per(4)        PER04
702100010621     C                   MOVE      per(5)        PER05
702200010621     C                   MOVE      per(6)        PER06
702300010621    2C                   ENDIF
702400010621      *
702500010625      *
702600010723     c                   if        *in80 or curline > 61
702700010627     c                   write     ta49pfin
702800010625     c                   write     ta49pPP
702900010625     c                   write     ta49p02
703000010625     c                   setoff                                       80
703100010625     c                   endif
703200010625      *
703300010621     C                   WRITE     TA49P03
703400010621      *
703500010621     C*
703600010621     C* VERIFICO SE CI SONO DEI VALORI A ZERO
703700010621     C*
703800010621      * VALORIZZO I CAMPI DI STAMPA
703900010621     C                   CLEAR                   TA49P03
704000010621      * MINIMO
704100010621    2C                   IF        *IN75
704200010621     C     tpn(1)        COMP      0                                  61
704300010621     C     tpn(2)        COMP      0                                  62
704400010621     C     tpn(3)        COMP      0                                  63
704500010621     C     tpn(4)        COMP      0                                  64
704600010622     C     tpn(5)        COMP      0                                  65
704700010622     C     tpn(6)        COMP      0                                  66
704800010621      *
704900010621     C                   Z-ADD     tpn(1)        TAR01
705000010621     C                   Z-ADD     tpn(2)        TAR02
705100010621     C                   Z-ADD     tpn(3)        TAR03
705200010621     C                   Z-ADD     tpn(4)        TAR04
705300010621     C                   Z-ADD     tpn(5)        TAR05
705400010621     C                   Z-ADD     tpn(6)        TAR06
705500010621      *
705600010626      * VALORIZZO LA PARTE DESCRITTIVA
705700060606      * in lingua
705800060606     c                   select
705900060606     c                   when      i45lin = 'I'
706000010626     C                   EVAL      COMO29 = '           Tariffa minima ...'
706100060606     c                   when      i45lin = '2'
706200060606     c                   eval      como29 = '           Minimum price ....'
706300060817     c                   when      i45lin = '3'
706400060817     c                   eval      como29 = '           Tarif minimum ....'
706500060817     c                   when      i45lin = '4'
706600060817     c                   EVAL      COMO29 = '           Mindestpreis .....'
706700060606     c                   endsl
706800010626     C                   MOVE(p)   COMO29        P03DES
706900010621      *
707000010625      *
707100010723     c                   if        *in80 or curline > 61
707200010627     c                   write     ta49pfin
707300010625     c                   write     ta49pPP
707400010625     c                   write     ta49p02
707500010625     c                   setoff                                       80
707600010625     c                   endif
707700010625      *
707800010621     C                   WRITE     TA49P03
707900010621      *
708000010621    2C                   ENDIF
708100010621     C*
708200010621     C* VERIFICO SE CI SONO DEI VALORI A ZERO
708300010621     C*
708400010621      * VALORIZZO I CAMPI DI STAMPA
708500010621     C                   CLEAR                   TA49P03
708600010621      * MASSIMO
708700010621    2C                   IF        *IN86
708800010621     C     tpx(1)        COMP      0                                  61
708900010621     C     tpx(2)        COMP      0                                  62
709000010621     C     tpx(3)        COMP      0                                  63
709100010621     C     tpx(4)        COMP      0                                  64
709200010622     C     tpx(5)        COMP      0                                  65
709300010622     C     tpx(6)        COMP      0                                  66
709400010621      *
709500010621     C                   Z-ADD     tpx(1)        TAR01
709600010621     C                   Z-ADD     tpx(2)        TAR02
709700010621     C                   Z-ADD     tpx(3)        TAR03
709800010621     C                   Z-ADD     tpx(4)        TAR04
709900010621     C                   Z-ADD     tpx(5)        TAR05
710000010621     C                   Z-ADD     tpx(6)        TAR06
710100010621      *
710200010621      * VALORIZZO LA PARTE DESCRITTIVA
710300060606      * in lingua
710400060606     c                   select
710500060606     c                   when      i45lin = 'I'
710600010626     C                   EVAL      COMO29 = '           Tariffa massima ..'
710700060606     c                   when      i45lin = '2'
710800060606     c                   eval      como29 = '           Maximum price ....'
710900060817     c                   when      i45lin = '3'
711000060817     c                   eval      como29 = '           Tarif maximum ....'
711100060817     c                   when      i45lin = '4'
711200060817     c                   EVAL      COMO29 = '           Höchstpreis ......'
711300060606     c                   endsl
711400010626     C                   MOVE(p)   COMO29        P03DES
711500010621      *
711600010625      *
711700010723     c                   if        *in80 or curline > 61
711800010627     c                   write     ta49pfin
711900010625     c                   write     ta49pPP
712000010625     c                   write     ta49p02
712100010625     c                   setoff                                       80
712200010625     c                   endif
712300010625      *
712400010621     C                   WRITE     TA49P03
712500010621      *
712600010621    2C                   ENDIF
712700010621      *
712800010621      *
712900010621     C                   ENDSR
713000010622      *-------------------------------------------------------------------------
713100010622      * FINDET STAMPA FINALE SOTTO OGNI SINGOLA TARIFFA PARTICOLARE
713200010622      *-------------------------------------------------------------------------
713300010622     C     FINDET        BEGSR
713400010919      *
713500010919      * stampo eventuale estensione scaglione unico
713600010919      *
713700010919     c                   exsr      sr_estes
713800010919      *
713900010622     c                   clear                   ta49pTP
714000010622      * verifico se tariffa a q.le per stampare il minimo tassabile
714100010622     c                   eval      *in08 = (tpttpg = '0')
714200010622      * per 08 controllo il minimo tassabile
714300010724      * nel caso in cui si stampa + di uno scaglione verifico il 1° scaglione
714400010724     c                   if        *in08 and recval > 1
714500010622     c                   if        scp(1) < §trata
714600010622     c                   z-add     scp(1)        commts
714700010622     c                   else
714800010622     c                   z-add     §trata        commts
714900010622     c                   endif
715000010622     c                   endif
715100010724      * nel caso in cui si stampa solo uno scaglione verifico il 1° scaglione
715200010724      * salvato
715300010724     c                   if        *in08 and recval = 1
715400010725     c                   if        mintas < §trata and mintas > 0
715500010724     c                   z-add     mintas        commts
715600010724     c                   else
715700010724     c                   z-add     §trata        commts
715800010724     c                   endif
715900010724     c                   endif
716000010622      * verifico rapporto peso / volume
716100010622      *
716200010622     c                   if        tptrpv > 0
716300010622     c     tptrpv        mult      100           comrpv
716400010622     c                   seton                                        19
716500010622     c                   endif
716600010622      * verifico gli importi degli arrotondamenti
716700010622      *
716800010622    1c                   if        tptarl > 0
716900010622      * verifico se esistono dei valori con i decimali
717000010622     C                   move      tptarf        w0030
717100010622     C                   eval      *in89 =(w0030 > 0)
717200010622      *
717300010622    2c                   if        not *in89
717400010622     C                   move      tptarl        w0030
717500010622     C                   eval      *in89 =(w0030 > 0)
717600010622    2c                   endif
717700010622      *
717800010622    2c                   if        not *in89
717900010622     C                   move      tptaro        w0030
718000010622     C                   eval      *in89 =(w0030 > 0)
718100010622    2c                   endif
718200010622      *
718300010622      * se 89 spento non esistono importi con decimali sposto in camp di comodo
718400010622    2c                   if        not *in89
718500010622     C                   Z-ADD     TptARO        WARO2
718600010622     C                   Z-ADD     TptARF        WARF2
718700010622     C                   Z-ADD     TptARL        WARL2
718800010622    2c                   endif
718900010622      *
719000010622      * se arrotondamenti di 1 in 1 non stampo
719100010622      * se arrotondamento fino a diverso da arrotondamento oltre a
719200010622      *
719300010628    2c                   if        tptarf <> tptaro
719400010622      *
719500010622      * valorizzo i campi di stampa
719600060606      * in lingua
719700060606     c                   select
719800060606     c                   when      i45lin = 'I'
719900010622     c  N89              eval      arrot1 = 'fino a ' + %editc(Warl2:'4') +
720000010622     c                                      ' di ' + %editc(warf2:'4')     +
720100010628     c                                      ' in ' + %editc(warf2:'4')
720200010622     c   89              eval      arrot1 = 'fino a ' + %editc(tptarl:'4') +
720300010628     c                                      ' di ' + %editc(tptarf:'4')     +
720400010628     c                                      ' in ' + %editc(tptarf:'4')
720500060606     c                   when      i45lin = '2'
720600060703     c  n89              eval      arrot1 = 'Up to ' +
720700060703     c                                      %editc(warl2:'4') + ' rounded ' +
720800060703     c                                      'to the nearest ' +
720900060703     c                                      %editc(warf2:'4')
721000060703     c   89              eval      arrot1 = 'Up to ' +
721100060703     c                                      %editc(tptarl:'4') + ' rounded ' +
721200060703     c                                      'to the nearest ' +
721300060606     c                                      %editc(tptarl:'4')
721400060817     c                   when      i45lin = '3'
721500060817     c  N89              eval      arrot1 = 'jusq''à  ' + %editc(Warl2:'4') +
721600060817     c                                      ' de ' + %editc(warf2:'4')     +
721700060817     c                                      ' en ' + %editc(warf2:'4')
721800060817     c   89              eval      arrot1 = 'jusq''à  ' + %editc(tptarl:'4') +
721900060817     c                                      ' de ' + %editc(tptarf:'4')     +
722000060817     c                                      ' en ' + %editc(tptarf:'4')
722100060817     c                   when      i45lin = '4'
722200060817     c  N89              eval      arrot1 = 'bis       ' + %editc(Warl2:'4') +
722300060817     c                                      ' von ' + %editc(warf2:'4')     +
722400060817     c                                      ' auf ' + %editc(warf2:'4')
722500060817     c   89              eval      arrot1 = 'bis       ' + %editc(tptarl:'4') +
722600060817     c                                      ' von ' + %editc(tptarf:'4')     +
722700060817     c                                      ' auf ' + %editc(tptarf:'4')
722800060606     c                   endsl
722900010622
723000010622     c                   seton                                        18
723100010622      *
723200010622      * valorizzo i campi di stampa
723300060606      * in lingua
723400060606     c                   select
723500060606     c                   when      i45lin = 'I'
723600010725     c  N89              eval      arrot2 = 'oltre  ' + '    '        +
723700010622     c                                      ' di ' + %editc(waro2:'4')     +
723800010628     c                                      ' in ' + %editc(waro2:'4')
723900010725     c   89              eval      arrot2 = 'oltre  ' + '        '         +
724000010628     c                                      ' di ' + %editc(tptaro:'4')     +
724100010628     c                                      ' in ' + %editc(tptaro:'4')
724200060606     c                   when      i45lin = '2'
724300060704     c  n89              eval      arrot2 = 'over ' +  '     '  +
724400060703     c                                      ' to nearest ' +
724500060703     c                                      %editc(waro2:'4')
724600060704     c   89              eval      arrot2 = 'over ' +  '     '  +
724700060703     c                                      ' to nearest ' +
724800060703     c                                      %editc(tptaro:'4')
724900060817     c                   when      i45lin = '3'
725000060818     c  N89              eval      arrot2 = 'au delà ' + '    '        +
725100060817     c                                      ' de ' + %editc(waro2:'4')     +
725200060817     c                                      ' en ' + %editc(waro2:'4')
725300060818     c   89              eval      arrot2 = 'au delà ' + '        '         +
725400060817     c                                      ' de ' + %editc(tptaro:'4')     +
725500060817     c                                      ' en ' + %editc(tptaro:'4')
725600060817     c                   when      i45lin = '4'
725700060818     c  N89              eval      arrot2 = 'mehr als' + '      '        +
725800060817     c                                      ' von ' + %editc(waro2:'4')     +
725900060817     c                                      ' auf ' + %editc(waro2:'4')
726000060818     c   89              eval      arrot2 = 'mehr als' + '      '         +
726100060817     c                                      ' von ' + %editc(tptaro:'4')     +
726200060817     c                                      ' auf ' + %editc(tptaro:'4')
726300060606     c                   endsl
726400010628      *
726500010628     C                   ELSE
726600010625      *
726700010625      * valorizzo i campi di stampa
726800060606      * in lingua
726900060606     c                   select
727000060606     c                   when      i45lin = 'I'
727100010628     c  N89              eval      arrot1 = 'di ' + %editc(Warf2:'4') +
727200010628     c                                      ' in ' + %editc(waro2:'4')
727300010628     c   89              eval      arrot1 = 'di     ' + %editc(tptarf:'4') +
727400010628     c                                      ' in' + %editc(tptaro:'4')
727500060606     c                   when      i45lin = '2'
727600060703     c  n89              eval      arrot1 = 'Rounded to nearest ' +
727700060606     c                                      %editc(warf2:'4')
727800060703     c   89              eval      arrot1 = 'Rounded to nearest ' +
727900060606     c                                      %editc(tptarf:'4')
728000060817     c                   when      i45lin = '3'
728100060817     c  N89              eval      arrot1 = 'de ' + %editc(Warf2:'4') +
728200060817     c                                      ' en ' + %editc(waro2:'4')
728300060817     c   89              eval      arrot1 = 'di     ' + %editc(tptarf:'4') +
728400060817     c                                      ' in' + %editc(tptaro:'4')
728500060817     c                   when      i45lin = '4'
728600060817     c  N89              eval      arrot1 = 'von ' + %editc(Warf2:'4') +
728700060817     c                                      ' auf ' + %editc(waro2:'4')
728800060817     c   89              eval      arrot1 = 'von    ' + %editc(tptarf:'4') +
728900060817     c                                      ' auf' + %editc(tptaro:'4')
729000060606     c                   endsl
729100010625
729200010628     c                   seton                                        18
729300010625      *
729400010628    2C                   endif
729500010622      *
729600010622    1c                   endif
729700010917      *
729800010917      * verifico se arrot2 è uguale a blanks accendo il 20
729900010917     c                   eval      *in20 = (arrot2 <> *blanks)
730000010622      *
730100010917      * verifico se devo stampare questa riga
730200010917      * in assenza di minimo tassabile o arrotondamenti non stampo
730300010917      * *in08 minimo tassabile *in18 arrotondamenti *in19 volume automatico
730400010917      *
730500010917     c                   if        *in08 or *in18 or *in19
730600010627     c                   exsr      SR_OVER
730700010625      *
730800010622     c                   write     ta49ptp
730900010917      *
731000010917     c                   endif
731100010622      *
731200010622     C                   ENDSR
731300010919      *
731400010919      *---------------------------------------------------------------*
731500010919      * SR_ESTES Gestione stampa estensione scaglione unico
731600010919      *---------------------------------------------------------------*
731700010919     C     SR_ESTES      BEGSR
731800010919      *
731900010919      * se il campo del TARPAREST è diverso da blanks vuol dire che devo
732000010919      * stampare l'estensione della tariffa particolare
732100010919      *
732200010919     c                   if        W_TARPAREST <> *blanks
732300010919     c                   eval      comest = W_TARPAREST
732400010919     c                   exsr      SR_OVER
732500010919      *
732600010919     c                   write     ta49pest
732700010919      *
732800010919     c                   clear                   W_TARPAREST
732900010919      *
733000010919     c                   endif
733100010627      *
733200010919     C                   ENDSR
733300050526
733400050526      *---------------------------------------------------------------*
733500050526      * SR_COMUNICA controllo e stampa dei servizi on line + e-mail
733600050526      *---------------------------------------------------------------*
733700050526     c     SR_COMUNICA   BegSr
733800050526
733900050526      * Controllo se devo stampare i SERVIZI ON LINE
734000050526     c                   ExSr      Sr_Psw
734100050526      * Controllo se devo stampare la richiesta di almeno 1 mail
734200050526     c                   ExSr      Sr_Ema
734300050526
734400050526      * Stampo la testata se devo stampare la richiesta delle e-mail
734500050526     c                   If        wpsw = *on or wema85 = *on or
734600051206     c                             wema87 = *on or wema88 = *on or
734700090612     c                             wema84 = *on or wema89 = *on
734800050527     c                   ExSr      Sr_Over
734900050526     c                   Write     Ta49pema
735000050526      * E-mail per servizi on line
735100050526     c                   If        wpsw = *on
735200050526     c                   Z-Add     curline       totrig
735300050526     c                   Add       4             totrig
735400050527     c                   ExSr      Sr_Overpiu
735500050526     c                   Write     Ta49pema1
735600050526     c                   EndIf
735700050526      * Stampo la riga per la richiesta delle e-mail
735800050526     c                   If        wema85 = *on or wema87 = *on or
735900090612     c                             wema88 = *on or wema84 = *On or
736000090612     c                             wema89 = *on
736100050526     c                   ExSr      Sr_Over
736200050526     c                   Write     Ta49pema2
736300050526     c                   EndIf
736400050526      * E-mail per bonifico bancario
736500050526     c                   If        wema85 = *on
736600050526     c                   ExSr      Sr_Over
736700050526     c                   Write     Ta49pema3
736800050526     c                   EndIf
736900050526      * E-mail per giacenze
737000050526     c                   If        wema88 = *on
737100050526     c                   ExSr      Sr_Over
737200050526     c                   Write     Ta49pema4
737300050526     c                   EndIf
737400050526      * E-mail per danni
737500050526     c                   If        wema87 = *on
737600050526     c                   ExSr      Sr_Over
737700050526     c                   Write     Ta49pema5
737800050526     c                   EndIf
737900051206      * E-mail per invio fattura
738000051206     c                   If        wema84 = *on
738100051206     c                   ExSr      Sr_Over
738200051206     c                   Write     Ta49pema7
738300051206     c                   EndIf
738400090612      * E-mail per sollecito pagamenti
738500090612     c                   If        wema89 = *on
738600090612     c                   ExSr      Sr_Over
738700090612     c                   Write     Ta49pema8
738800090612     c                   EndIf
738900050526      * consenso dei dati
739000050526     c                   Z-Add     curline       totrig
739100050526     c                   Add       4             totrig
739200050527     c                   ExSr      Sr_Overpiu
739300050526     c                   Write     Ta49pema6
739400050526
739500050526     c                   EndIf
739600041229
739700050526     c                   EndSr
739800041229      *---------------------------------------------------------------*
739900041229      * SR_PSW stampa servizi ON LINE se offerta
740000041229      *---------------------------------------------------------------*
740100041229     c     SR_PSW        BegSr
740200041229
740300041229     c                   Eval      wpsw = *Off
740400041229
740500041229      * Se cliente nuovo
740600041229      * --> stampo sempre
740700041230if  1c                   If        VisCnw = 'S'
740800041229     c                   Eval      wpsw = *On
740900041229      * Se cliente non nuovo
741000041229      * --> stampo se non abilitato o se non è figlio di papà abilitato
741100041230   x1c                   Else
741200041230     c                   Move      *all'0'       vssksu
741300041230     c                   Move      visksc        vssksu
741400041230     c                   Eval      vssisv = 'TT'
741500041230     c     kTivss        Setll     Tivss02l
741600041230if  2c                   If        not %Equal(Tivss02l)
741700041230     c                   Clear                   Tibs10ds
741800041230     c                   Eval      d10tle = 'WW'
741900041230     c                   Eval      d10paf = 'P'
742000041230     c                   Eval      d10cod = visksc
742100041230     c                   Eval      d10drf = *date
742200041230     c                   Call      'TIBS10R'
742300041230     c                   Parm                    Tibs10ds
742400041230if  3c                   If        d10err = *Blanks and d10cop > *Zeros
742500041230     c                   Move      *all'0'       vssksu
742600041230     c                   Move      d10cop        vssksu
742700041230     c                   Eval      vssisv = 'TT'
742800041230     c     kTivss        Setll     Tivss02l
742900041230     c                   If        not %Equal(Tivss02l)
743000041230     c                   Eval      wpsw = *On
743100041230     c                   EndIf
743200041230e   3c                   EndIf
743300050112if  3c                   If        d10err <> *Blanks
743400050112     c                   Eval      wpsw = *On
743500050112e   3c                   EndIf
743600041230e   2c                   EndIf
743700041230e   1c                   EndIf
743800041229
743900041229     c                   EndSr
744000050525
744100050525      *---------------------------------------------------------------*
744200050526      * SR_EMA stampa delle e-mail da richiedere se offerta
744300050525      *---------------------------------------------------------------*
744400050526     c     Sr_Ema        BegSr
744500050526
744600050526     c                   Eval      wema85 = *Off
744700050526     c                   Eval      wema88 = *Off
744800050526     c                   Eval      wema87 = *Off
744900051206     c                   Eval      wema84 = *Off
745000090612     c                   Eval      wema89 = *Off
745100091112      * visita
745200050527     c                   eval      notapl = 'V'
745300050527     c                   movel     i45kcc        notke1
745400050527     c                   move      parsks        notke1
745500091112      * trattativa
745600091112     c                   if        $trattativa
745700091112     c                   eval      notapl = 'T'
745800091112     c                   endif
745900050527     c                   Clear                   notke2
746000050525
746100050526      * e-mail bonifico bancario nota 85
746200050526     c                   If        §4uabi = 'S'
746300050527      * nota della visita
746400050526     c                   movel     '85'          nottnt
746500050526     c     keyntc        Chain     tfntc01l
746600050527     c                   If        %Found(tfntc01l)
746700050527     c                   Goto      nota88
746800050527     c                   EndIf
746900050527      * nota del cliente se non è un cliente nuovo
747000050527     c                   If        visksc > *Zeros
747100050527     c                   eval      notapl = 'C'
747200050527     c                   move      visksc        notke1
747300050527     c     keyntc        Chain     tfntc01l
747400050527     c                   If        %Found(tfntc01l)
747500050527     c                   Goto      nota88
747600050527     c                   EndIf
747700050527     c                   EndIf
747800050527      * nota non trovata
747900050526     c                   Eval      wema85 = *On
748000050526     c                   EndIf
748100050527
748200050527     c     nota88        Tag
748300091112      * visita
748400050527     c                   eval      notapl = 'V'
748500050527     c                   move      parsks        notke1
748600091112      * trattativa
748700091112     c                   if        $trattativa
748800091112     c                   eval      notapl = 'T'
748900091112     c                   endif
749000050527
749100050527      * e-mail giacenze nota 88
749200050527     c                   movel     '88'          nottnt
749300050527      * nota della visita
749400050527     c     keyntc        Chain     tfntc01l
749500050527     c                   If        %Found(tfntc01l)
749600050527     c                   Goto      nota87
749700050527     c                   EndIf
749800050527      * nota del cliente se non è un cliente nuovo
749900050527     c                   If        visksc > *Zeros
750000050527     c                   eval      notapl = 'C'
750100050527     c                   move      visksc        notke1
750200050527     c     keyntc        Chain     tfntc01l
750300050527     c                   If        %Found(tfntc01l)
750400050527     c                   Goto      nota87
750500050527     c                   EndIf
750600050527     c                   EndIf
750700050527      * nota non trovata
750800050527     c                   Eval      wema88 = *On
750900050527
751000050527     c     nota87        Tag
751100091112      * visita
751200050527     c                   eval      notapl = 'V'
751300050527     c                   move      parsks        notke1
751400091112      * trattativa
751500091112     c                   if        $trattativa
751600091112     c                   eval      notapl = 'T'
751700091112     c                   endif
751800050527
751900050526      * e-mail danni nota 87
752000050527     c                   movel     '87'          nottnt
752100050527      * nota della visita
752200050527     c     keyntc        Chain     tfntc01l
752300050527     c                   If        %found(tfntc01l)
752400090612     c                   Goto      nota84
752500050527     c                   EndIf
752600050527      * nota del cliente se non è un cliente nuovo
752700050527     c                   If        visksc > *Zeros
752800050527     c                   eval      notapl = 'C'
752900050527     c                   move      visksc        notke1
753000050527     c     keyntc        Chain     tfntc01l
753100050527     c                   If        %Found(tfntc01l)
753200051206     c                   Goto      nota84
753300050527     c                   EndIf
753400050527     c                   EndIf
753500050527      * nota non trovata
753600050527     c                   Eval      wema87 = *On
753700051206
753800051206     c     nota84        Tag
753900091112      * visita
754000051206     c                   eval      notapl = 'V'
754100051206     c                   move      parsks        notke1
754200091112      * trattativa
754300091112     c                   if        $trattativa
754400091112     c                   eval      notapl = 'T'
754500091112     c                   endif
754600091112
754700051206      * e-mail invio fattura nota 84
754800051206     c                   movel     '84'          nottnt
754900051206      * nota della visita
755000051206     c     keyntc        Chain     tfntc01l
755100051206     c                   If        %found(tfntc01l)
755200090612     c                   Goto      nota89
755300051206     c                   EndIf
755400051206      * nota del cliente se non è un cliente nuovo
755500051206     c                   If        visksc > *Zeros
755600051206     c                   eval      notapl = 'C'
755700051206     c                   move      visksc        notke1
755800051206     c     keyntc        Chain     tfntc01l
755900051206     c                   If        %Found(tfntc01l)
756000090612     c                   Goto      nota89
756100051206     c                   EndIf
756200051206     c                   EndIf
756300051206      * nota non trovata
756400060203     c                   Eval      wema84 = *On
756500090612
756600090612     c     nota89        Tag
756700091112      * visita
756800090612     c                   eval      notapl = 'V'
756900090612     c                   move      parsks        notke1
757000091112      * trattativa
757100091112     c                   if        $trattativa
757200091112     c                   eval      notapl = 'T'
757300091112     c                   endif
757400090612
757500090612      * e-mail sollecito pagamenti 89
757600090612     c                   movel     '89'          nottnt
757700090612      * nota della visita
757800090612     c     keyntc        Chain     tfntc01l
757900090612     c                   If        %found(tfntc01l)
758000090612     c                   Leavesr
758100090612     c                   EndIf
758200090612      * nota del cliente se non è un cliente nuovo
758300090612     c                   If        visksc > *Zeros
758400090612     c                   eval      notapl = 'C'
758500090612     c                   move      visksc        notke1
758600090612     c     keyntc        Chain     tfntc01l
758700090612     c                   If        %Found(tfntc01l)
758800090612     c                   Leavesr
758900090612     c                   EndIf
759000090612     c                   EndIf
759100090612      * nota non trovata
759200090612     c                   Eval      wema89 = *On
759300050525
759400050525     c                   EndSr
759500041229
759600010627      *---------------------------------------------------------------*
759700010627      * SR_OVER gestione standarad overflow
759800010627      *---------------------------------------------------------------*
759900010627     C     SR_OVER       BEGSR
760000010627      *
760100010801     c                   if        *in80 or curline >= 61
760200010627     c                   write     ta49pfin
760300010627     c                   write     ta49pPP
760400010627     c                   setoff                                       80
760500010627     c                   endif
760600010627      *
760700010627     c                   endsr
760800050526
760900050526      *---------------------------------------------------------------*
761000050526      * SR_OVERPIU gestione standarad overflow con + righe
761100050526      *---------------------------------------------------------------*
761200050526     C     SR_OVERPIU    BEGSR
761300050526      *
761400050526     c                   if        *in80 or totrig >= 61
761500050527     c                   write     ta49pfin
761600050527     c                   write     ta49pPP
761700050526     c                   setoff                                       80
761800050526     c                   endif
761900050526      *
762000050526     c                   endsr
762100050526
762200010627      *
762300010618      *---------------------------------------------------------------*
762400010618      * STAMPA ERRORE
762500010618      *---------------------------------------------------------------*
762600010615     C     STAERR        BEGSR
762700050926      * se offerta valorizzo con il numero della visita
762800050926    2c                   if        *in98
762900050926     c                   eval      p07ksc = visksc
763000050926     c                   movel     tamksc        p07nrv
763100050926     c                   move      tamksc        p07prv
763200091117     c                   if        $trattativa
763300091117     c                   eval      p07ntra = tamksc
763400091117     c                   endif
763500050930      * tariffa
763600050926   x2c                   else
763700050926     c                   eval      p07ksc = tamksc
763800050926    2c                   endif
763900091117     c                   if        $trattativa
764000091117     c                   eval      ptratta =
764100091117     c                             '(codice ' +
764200091117     c                             %editc(tamctr:'X') + '/' +
764300091117     c                             %subst(%editc(tamprg:'X'):3:1) + ')'
764400091117     c                   endif
764500091117     c                   if        not $trattativa
764600091117     c                   eval      ptariffa =
764700091117     c                             '(codice ' +
764800091117     c                             %editc(tamctr:'X') + ')'
764900091117     c                   endif
765000050930
765100050930     c                   Write     ta49perr
765200050930     c                   If        woktar = *Off
765300050930     c                   Write     ta49p07
765400050930     c                   EndIf
765500050930
765600010615     C                   ENDSR
765700060922
765800060922      *---------------------------------------------------------------*
765900060922      * Imposto ATF lungo 7 nelle varie lingue
766000060922      *---------------------------------------------------------------*
766100060922     c     sr_atfk       begsr
766200060922
766300060922      * tariffa peso q.li 0
766400060925     c                   if        wctr1 = '0'
766500060922      * in lingua
766600060922     c                   select
766700060922     c                   when      i45lin = 'I'
766800060922     c                   eval      atf(k) = 'A Q.LE'
766900060922     c                   when      i45lin = '2'
767000060922     c                   eval      atf(k) = '100 KG'
767100060922     c                   when      i45lin = '3'
767200060922     c                   eval      atf(k) = '100 KG'
767300060922     c                   when      i45lin = '4'
767400060922     c                   eval      atf(k) = '100 KG'
767500060922     c                   endsl
767600060922     c                   endif
767700060922
767800060922      * tariffa colli 1
767900060925     c                   if        wctr1 = '1'
768000060922      * in lingua
768100060922     c                   select
768200060922     c                   when      i45lin = 'I'
768300060922     c                   eval      atf(k) = 'A COLLO'
768400060922     c                   when      i45lin = '2'
768500060922     c                   eval      atf(k) = 'P PARC.'
768600060922     c                   when      i45lin = '3'
768700060922     c                   eval      atf(k) = 'COLIS'
768800060922     c                   when      i45lin = '4'
768900060922     c                   eval      atf(k) = 'PRO PAK'
769000060922     c                   endsl
769100060922     c                   endif
769200060922
769300060922      * tariffa spedizione 2
769400060925     c                   if        wctr1 = '2'
769500060922      * in lingua
769600060922     c                   select
769700060922     c                   when      i45lin = 'I'
769800060922     c                   eval      atf(k) = 'A SPED.'
769900060922     c                   when      i45lin = '2'
770000060922     c                   eval      atf(k) = 'P SHIP.'
770100060922     c                   when      i45lin = '3'
770200060922     c                   eval      atf(k) = 'ENVOI'
770300060922     c                   when      i45lin = '4'
770400060922     c                   eval      atf(k) = 'PRO SEN'
770500060922     c                   endsl
770600060922     c                   endif
770700060922
770800060922      * tariffa peso kg 3
770900060925     c                   if        wctr1 = '3'
771000060922      * in lingua
771100060922     c                   select
771200060922     c                   when      i45lin = 'I'
771300060922     c                   eval      atf(k) = 'A KG.'
771400060922     c                   when      i45lin = '2'
771500060922     c                   eval      atf(k) = 'BY KG'
771600060922     c                   when      i45lin = '3'
771700060922     c                   eval      atf(k) = 'AU KG'
771800060922     c                   when      i45lin = '4'
771900060922     c                   eval      atf(k) = 'PRO KG'
772000060922     c                   endsl
772100060922     c                   endif
772200060922
772300060922      * tariffa a valore 4
772400060925     c                   if        wctr1 = '4'
772500060922      * in lingua
772600060922     c                   select
772700060922     c                   when      i45lin = 'I'
772800060922     c                   eval      atf(k) = 'A VAL.'
772900060922     c                   when      i45lin = '2'
773000060922     c                   eval      atf(k) = 'BY VALU'
773100060922     c                   when      i45lin = '3'
773200060922     c                   eval      atf(k) = 'VALEUR'
773300060922     c                   when      i45lin = '4'
773400060922     c                   eval      atf(k) = 'NACH WE'
773500060922     c                   endsl
773600060922     c                   endif
773700060922
773800060922      * tariffa a quantità 5
773900060925     c                   if        wctr1 = '5'
774000060922      * in lingua
774100060922     c                   select
774200060922     c                   when      i45lin = 'I'
774300060922     c                   eval      atf(k) = 'A Q.TA'
774400060922     c                   when      i45lin = '2'
774500060922     c                   eval      atf(k) = 'BY QUAN'
774600060922     c                   when      i45lin = '3'
774700060922     c                   eval      atf(k) = 'QUANT.'
774800060922     c                   when      i45lin = '4'
774900060922     c                   eval      atf(k) = 'NACH ME'
775000060922     c                   endsl
775100060922     c                   endif
775200060922
775300060922      * tariffa valore specifico V (usato nelle tariffe particolari)
775400060925     c                   if        wctr1 = 'V'
775500060922      * in lingua
775600060922     c                   select
775700060922     c                   when      i45lin = 'I'
775800060922     c                   eval      atf(k) = 'VAL.SPE'
775900060922     c                   when      i45lin = '2'
776000060922     c                   eval      atf(k) = 'SET VAL'
776100060922     c                   when      i45lin = '3'
776200060922     c                   eval      atf(k) = 'VAL.SPE'
776300060922     c                   when      i45lin = '4'
776400060922     c                   eval      atf(k) = 'SP.WERT'
776500060922     c                   endsl
776600060922     c                   endif
776700060922
776800060922      * questo è un chiodo per impostare 'A SPED.' quando lo richiede
776900060922      * il programma
777000060925     c                   if        wctr1 = ' '
777100060922      * in lingua
777200060922     c                   select
777300060922     c                   when      i45lin = 'I'
777400060922     c                   eval      atf(k) = 'A SPED.'
777500060922     c                   when      i45lin = '2'
777600060922     c                   eval      atf(k) = 'P SHIP.'
777700060922     c                   when      i45lin = '3'
777800060922     c                   eval      atf(k) = 'PAR ENV'
777900060922     c                   when      i45lin = '4'
778000060922     c                   eval      atf(k) = 'N.SEND'
778100060922     c                   endsl
778200060922     c                   endif
778300151117
778400151117      * tariffa a ritiro solo per tariffa particolare t Diritto di chiamata
778500151117      * ritiro telefonico
778600151117     c                   if        wctr1 = 'R'
778700151117      * in lingua
778800151117     c                   select
778900151117     c                   when      i45lin = 'I'
779000151117     c                   eval      atf(k) = 'A RIT.'
779100151117     c                   when      i45lin = '2'
779200151117     c                   eval      atf(k) = 'A RIT.'
779300151117     c                   when      i45lin = '3'
779400151117     c                   eval      atf(k) = 'A RIT.'
779500151117     c                   when      i45lin = '4'
779600151117     c                   eval      atf(k) = 'A RIT.'
779700151117     c                   endsl
779800151117     c                   endif
779900151117
780000060922
780100060922     c                   endsr
780200060922
780300060922      *---------------------------------------------------------------*
780400060922      * Imposto la parte finale del numero per il lasciato avviso
780500060922      *---------------------------------------------------------------*
780600060922     c     sr_numgp      begsr
780700060922
780800060922     c                   select
780900060922      * italiano sempre ° fisso nel printer file il campo qua lo pulisco
781000060922     c                   when      i45lin = 'I'
781100060922     c                   clear                   comnavd
781200060922      * inglese cambiano i primi tre poi sempre th
781300060922     c                   when      i45lin = '2'
781400060922     c                   eval      comnavd = 'th'
781500060922     c                   if        comnav = 1
781600060922     c                   eval      comnavd = 'st'
781700060922     c                   endif
781800060922     c                   if        comnav = 2
781900060922     c                   eval      comnavd = 'nd'
782000060922     c                   endif
782100060922     c                   if        comnav = 3
782200060922     c                   eval      comnavd = 'rd'
782300060922     c                   endif
782400060922      * francese cambia solo il primo il resto sempre ieme
782500060922     c                   when      i45lin = '3'
782600060922     c                   eval      comnavd = 'ieme'
782700060922     c                   if        comnav = 1
782800060922     c                   eval      comnavd = 'er'
782900060922     c                   endif
783000060922      * tedesco sempre . fisso nel printer file il campo qua lo pulisco
783100060922     c                   when      i45lin = '4'
783200060922     c                   clear                   comnavd
783300060922     c                   endsl
783400060922
783500060922     c                   endsr
783600060922
783700010606     C*-------------------------------------------------------------------------
783800010606     C*  SR_CARTAB  CARICAMENTO INIZIALE TABELLE
783900010606     C*-------------------------------------------------------------------------
784000010606     C     SR_CARTAB     BEGSR
784100010606     C*  carico la schiera di tutti i codici tassazione
784200010606     C                   clear                   K
784300010606     C                   clear                   K1
784400010606     C                   eval      cod = 'CT'
784500060704     c     tabk2l        setll     tabel00f
784600010531     C                   DO        *hival
784700060704     c     tabk2l        READE     Tabel00f
784800010531      *
784900010531     C                   If        %eof(tabel00f)
785000010531     C                   leave
785100010531     C                   endif
785200010531      *
785300010531     C                   IF        tblkey <> *blanks
785400010531     C                   add       1             K
785500010531     C                   movel     Tblkey        CCT(K)
785600010531     C                   movel     Tbluni        DSCT
785700010531     C                   move      §CTprv        SCT(K)
785800010531     C                   MOVE      §CTrap        RCT(K)
785900010531     C                   MOVE      §CTnar        NAR(K)
786000010531     C                   MOVE      §CTcor        DS6PR2
786100050204     c                   Move      §ctdes        prvdes(k)
786200050303     c                   Eval      noctdes(k) = §ctsta
786300010531      * se è una regione carico la schiera delle regioni
786400010531     C                   IF        ds6pr2 = '9'
786500010531     C                   add       1             K1
786600010531     C                   movel     Tblkey        CRE(K1)
786700010531     C                   move      §CTdes        DRE(K1)
786800010531     C                   endif
786900010531      *
787000010531     C                   endif
787100010531      *
787200010531     C                   ENDDO
787300010531      *
787400010531      *  carico la schiera delle nazioni con quella ITALIA
787500010531     C                   z-add     1             K
787600010531     C                   eval      cod = '15'
787700060724     C     TABK2l        SETLL     TABEL00F
787800010531     c                   DO        *HIVAL
787900060724     C     TABK2l        READE     TABEL00F
788000010531      *
788100010621     C                   IF          %eof(tabel00f)
788200010531     C                   leave
788300010531     c                   ENDIF
788400010531      *
788500010531      * se non è annullato
788600010531     C                   IF        Tblflg = ' '
788700010531      *
788800010531     C                   Movel     tbluni        DS15
788900010531      * carico solo la nazione ITALIA
789000010531     C                   IF        §15ita = 'I'
789100010531     C                   Movel     Tblkey        NAZ(K)
789200010531     C                   add       1             K
789300010531     C                   endif
789400010531      *
789500010531     C                   endif
789600010531      *
789700010531     C                   ENDDO
789800010807      *
789900010807      * aggancio la tabella delle variabili giacenza
790000010807     C                   eval      cod = '2G'
790100010807     C                   clear                   KEY
790200010807     C                   movel     '1'           KEY
790300010807     C     TABKEY        CHAIN     TABEL00F
790400010807     c                   if        %found(tabel00f)
790500010807     C                   movel     TBLUNI        DS2G
790600010807     c                   else
790700010807     C                   clear                   DS2G
790800010807     c                   endif
790900010807      *
791000021023      * Richiamo FnLv41 per verificare/creare testi e reperire ds1h
791100021022     C                   clear                   Ds1H
791200021022     C                   move      *off          $NoStp            1
791300021022     C                   clear                   Fnlv41ds
791400021022     C                   movel     '3'           I41Mod
791500021022     C                   movel     I45TTX        I41Let
791600021022     C                   CALL      'FNLV41R'
791700021022     C                   PARM                    FNLV41DS
791800021022      *
791900021022      * Se non ho errori stampo
792000021023     C     O41ERR        IFNE      'E'
792100021022     C                   movel     O41Dta        DS1H
792200010531      *
792300010531      * stampo indirizzo di filiale
792400010531     C                   eval      *in92 = (§1Hfv1 = 'S')
792500010531      * stampo indirizzo cliente
792600010531     C                   eval      *in93 = (§1Hfv2 = 'S')
792700010531      * stampo alla C.A.
792800010531     C                   eval      *in94 = (§1Hfv4 = 'S')
792900010531      * stampo "Telefonando chiedere .........."
793000010531     C                   eval      *in95 = (§1Hfv5 = 'S')
793100010531      * stampo la data
793200010531     C                   eval      *in96 = (§1Hfv6 = 'S')
793300010531     C* STAMPA RIFERIMENTI
793400010531     C                   eval      *in97 = (§1Hfv9 = 'S'and i45nr1 <> *blanks)
793500021022      *
793600021022     C                   ELSE
793700021022     C                   move      *on           $NoStp
793800021022     C                   ENDIF
793900101015
794000101015      /free
794100101015       //?Carico scatti ISTAT
794200101015         SISsca = 0;
794300101015         clear DIA;
794400101015         setll (SISsca) TISIS01L;
794500101015
794600101015         DOW  not $EoF;
794700101015
794800101015           read TISIS01L;
794900101015
795000101015           IF  %Eof(TISIS01L);
795100101015             $EoF = *on;
795200101015             leave;
795300101015           ENDIF;
795400101015
795500101015           DIA(SISsca) = SISdata;
795600101015
795700101015         ENDDO;
795800160623
795900101015
796000101015      /end-free
796100021022      *
796200010531     C                   ENDSR
796300080609
796400080609      *------------------------------------------------------------------------*
796500080609      * Carico filiali abilitate al lasciato avviso
796600080609      *------------------------------------------------------------------------*
796700080609     c     carlav        begsr
796800080609
796900080609     c                   clear                   sklav
797000080609     c                   clear                   xxx
797100080609
797200080609     c/exec sql
797300080609     c+ declare lav cursor for select tntbe01l.*
797400080609     c+ from tntbe01l
797500080609     c+ where tbecod='LAV' and tbeatb = ''
797600080609     c/end-exec
797700080609
797800080609     c/exec sql
797900080609     c+ open lav
798000080609     c/end-exec
798100080609
798200080609     c                   do        *hival
798300080609
798400080609     c/exec sql
798500080609     c+ fetch next from lav into :tntbeds
798600080609     c/end-exec
798700080609
798800080609     c                   select
798900080609     c                   when      sqlcod = 100
799000080609     c                   leave
799100080609     c                   when      sqlcod < 0
799200080609     c                   other
799300080609     c                   eval      dlav = tbeuni
799400080609      * carico la schiera della filiali abilitate
799500080616     c                   if        d§lavtar > *date
799600080609     c                   iter
799700080609     c                   endif
799800080609     c                   eval      xxx = xxx + 1
799900080609     c                   eval      sklav(xxx) = %dec(tbeke1:3:0)
800000080609
800100080609     c                   endsl
800200080609
800300080609     c                   enddo
800400080609
800500080609     c/exec sql
800600080609     c+ close lav
800700080609     c/end-exec
800800080609
800900080609
801000080609     c                   endsr
801100010530     C*------------------------------------------------------------------------
801200010530     C* ROUTINE INIZIALE
801300010530     C*------------------------------------------------------------------------
801400010530      *
801500010530     C     *INZSR        BEGSR
801600010530      *
801700010530     C     *ENTRY        PLIST
801800010530     C                   parm                    KPJBA
801900020919     C                   parm                    fnlv45ds
802000010628      *
802100020624     C                   z-add     1             codut             1 0
802200020624
802300020624      * reperisco i dati utente
802400020624     c     *dtaara       define    §azute        AzuteDs
802500020624     c     *dtaara       define    §datiute      DDatiUte
802600020624
802700020624     c                   in(E)     *dtaara
802800020624     c                   IF        %Error or RSUT = *blanks
802900020624     c                   call      'TIBS34R'
803000020624     c                   parm                    Tibs34Ds
803100020624     c                   in        *dtaara
803200020624     c                   ENDIF
803300110516
803400110516      * chiodo fisso 'BRT S.p.A.'
803500110516     c                   eval      rsutold = 'BRT S.p.A.'
803600020624      *
803700020919     C                   MOVEL     SIMFEL        simfelA           3
803800060704
803900060704      * lingua per tabelle
804000060704     c                   clear                   kcodut            1 0
804100060704     c                   select
804200060704     c                   when      i45lin = 'I'
804300060704     c                   eval      kcodut = 1
804400060704     c                   when      i45lin = '2'
804500060704     c                   eval      kcodut = 2
804600060704     c                   when      i45lin = '3'
804700060704     c                   eval      kcodut = 3
804800060704     c                   when      i45lin = '4'
804900060704     c                   eval      kcodut = 4
805000060704     c                   endsl
805100010530      *
805200010530      * preparo il campo PAGE per la numerazione delle pagine
805300010530     c                   eval      page = 1
805400010531      * carico le tabelle
805500010531     c                   exsr      SR_CARTAB
805600021022      *
805700021022      * proseguo se ho i dati per uso testi
805800021022     c                   if        $NoStp = *off
805900010531      *
806000010531      * TARIFFA O OFFERTA
806100010531      *
806200010531      *............ OFFERTA .....................
806300010531      *
806400110104     c                   if        i45ast = 'T'
806500010531      * accendo il 98
806600010531     c                   seton                                        98
806700091112      * se trattativa imposto flag
806800091112     c                   eval      $trattativa = (i45ast = 'T')
806900091117     c                   eval      *in85 = (i45ast = 'T')
807000010531      * valorizzo il campo del codice cliente tariffa con il numero visita
807100010531     c                   eval      parsks = i45nrv
807200010531      * apro i file visita/offerta
807300010531     c                   open      TFACO00F
807400010531     c                   open      TFIND00F
807500010531     c                   open      TFCLP00F
807600010531     c                   open      TNOFM01L
807700061122     c                   open      tiofd01L
807800010531     c                   open      TIOPT01L
807900010629     c                   open      TIOPD01L
808000010531     c                   open      TIOGC01L
808100091022     c                   if        i45ast = 'T'
808200091022     c                   open      Tivis05l
808300091022     c                   endif
808400041229
808500010531      *  i45ast = 'C'
808600010531     c                   else
808700010531      *
808800010531      *............ TARIFFA .....................
808900061122      * apro i file visita/offerta
809000061122     c                   open      TNtam01L
809100061122     c                   open      Titad04L
809200061122     c                   open      Titpt01L
809300061122     c                   open      TItpD01L
809400061122     c                   open      TItGC01L
809500010531      *
809600010531      * valorizzo il campo del codice cliente tariffa
809700010531     c                   eval      parsks = i45ksc
809800010531      *
809900010531     c                   endif
810000021022     c                   endif
810100010531      *
810200010531      *   DEFINIZIONI DELLE CHIAVI
810300010531      *
810400010531      * accesso per codice su TABEL00F
810500090608     c     TABKEY        klist
810600010531     c                   kfld                    codut
810700010531     c                   kfld                    cod
810800010531     c                   kfld                    key
810900060605
811000060605      * accesso per codice su TABEL00F in lingua
811100060605     c     tabkeyl       klist
811200060605     c                   kfld                    kcodut
811300060605     c                   kfld                    cod
811400060605     c                   kfld                    key
811500060606
811600060606      * accesso per codice su TABEL00F  parziale in lingua
811700060606     c     tabk2l        klist
811800060606     c                   kfld                    kcodut
811900060606     c                   kfld                    cod
812000060606
812100010531      * accesso TNTAM01L/TNOFM01L
812200010531     c     DS1KTM        klist
812300010531     c                   kfld                    ds1ksc
812400010531     c                   kfld                    ds1ctr
812500010531     c                   kfld                    ds1prg
812600010605      * accesso TITAD04L con linea partenza impostata
812700010605     c     DS1KTD        klist
812800010605     c                   kfld                    ds1ksc
812900010605     c                   kfld                    ds1ctr
813000010605     c                   kfld                    ds1prg
813100010605     c                   kfld                    comfil
813200010606      * accesso TITAD04L con chiave piena
813300010606     c     KEYTAD        klist
813400010606     c                   kfld                    tadksc
813500010606     c                   kfld                    tadctr
813600010606     c                   kfld                    tadprg
813700010606     c                   kfld                    tadlnp
813800010606     c                   kfld                    tadorp
813900010606     c                   kfld                    tadnaz
814000010606     c                   kfld                    tadcap
814100010618      ** accesso su TITPT e TITPD
814200010618     C     KTPT          KLIST
814300010618     C                   KFLD                    ds1ksc
814400010618     C                   KFLD                    ds1ctr
814500010618     C                   KFLD                    ds1prg
814600010618     C                   KFLD                    savftc
814700010621      ** accesso su  TITPD
814800010621     C     KTPD          KLIST
814900010621     C                   KFLD                    DS1KSC
815000010621     C                   KFLD                    DS1CTR
815100010621     C                   KFLD                    DS1PRG
815200010621     C                   KFLD                    SAVFTC
815300010621     C                   KFLD                    SAVORP
815400010621     C     KTPD2         KLIST
815500010621     C                   KFLD                    DS1KSC
815600010621     C                   KFLD                    DS1CTR
815700010621     C                   KFLD                    DS1PRG
815800010621     C                   KFLD                    SAVFTC
815900010621     C                   KFLD                    SAVORP
816000010621     C                   KFLD                    SAVSGL
816100010626     C     KTPDWRK       KLIST
816200010626     C                   KFLD                    DS1KSC
816300010626     C                   KFLD                    DS1CTR
816400010626     C                   KFLD                    DS1PRG
816500010626     C                   KFLD                    WRKFTC
816600010626     C                   KFLD                    WRKORP
816700010626     C                   KFLD                    WRKSGL
816800010601      ** accesso CNACO / TFACO
816900010601     C     DS7KAC        klist
817000010601     C                   kfld                    codut
817100010601     C                   kfld                    i45kcc
817200010601     C                   kfld                    ksc
817300010601      ** accesso TFNTC
817400010601     C     KEYNTC        klist
817500010601     c                   kfld                    notapl
817600010601     c                   kfld                    notke1
817700010601     c                   kfld                    notke2
817800010601     c                   kfld                    nottnt
817900010604      * accesso AZCPC03L
818000010621     c     kcpc          klist
818100010604     c                   kfld                    o96ver
818200010604     c                   kfld                    §ctnar
818300010604     c                   kfld                    comfil
818400041230      * accesso TIVSS02L
818500041230     c     kTivss        klist
818600041230     c                   kfld                    vssksu
818700041230     c                   kfld                    vssisv
818800080211
818900080211     c     kCnabi        Klist
819000080211     c                   Kfld                    pcabi
819100080211     c                   Kfld                    pccab
819200010531      *
819300010531     c                   ENDSR
819400010531      *
819500010530**   MSG
819600140224ATTENZIONE!!! Prezzo base Fuel mai rilevato settimanalmente dal Ministero!!   1
819700010530                                                                              2
819800010530CLIENTE NON CODIFICATO O ANNULLATO                                            3
819900010530NESSUNA RIGA DI DETTAGLIO X QUESTA TARIFFA/OFFERTA                            4
820000050615PIU' DI 18 SCAGLIONI X CODICE DI TASSAZIONE                                   5
820100010530CODICE TASSAZIONE NON TROVATO !!!!!!!!!!!      CUMELA MO' ?                   6
820200010530CODICE REGIONE DEL CODICE DI TASSAZIONE NON ESISTE IN TABELLA                 7
820300010530UN CODICE DI TASSAZIONE DELLA TARIFFA HA UNO SCAGLIONE NON PREVISTO           8
820400050616PIU' DI 15 PROVINCIE X 1 REGIONE? TABELLA ERRATA O  T N T A D  NON ALLINEATO  9
820500050616PIU' DI 15 PROVINCIE X 1 REGIONE? TABELLA ERRATA O  T N T P D  NON ALLINEATO  10
820600050615                                                                              1
820700010530TROPPI PAESI DI ARRIVO/CODICI DI TASSAZIONE PER LA REGIONE                    2
820800140224                                                                              3
820900010530UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON PREVISTO   4
821000050615PIU' DI 18 SCAGLIONI X CODICE DI TASSAZIONE PER LE TARIFFE PARTICOLARI        5
821100050324UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON VALIDO     6
