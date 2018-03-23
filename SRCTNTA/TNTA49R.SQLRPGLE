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
006400161223     F                                     usropn
006500010530     F*
006600010530     F*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**
006700010530     D*                 DEFINIZIONE SCHIERE
006800010530     D*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**
006900010530     D* Messaggi di errore
007000050324     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
007100010530     D*  TABELLE PROVINCIE E REGIONI
007200041220     D CCT             S              2    DIM(600)
007300041220     D SCT             S              2    DIM(600)
007400041220     D RCT             S              2    DIM(600)
007500041220     D NAR             S              3    DIM(600)
007600050204     d prvdes          s             20    dim(600)
007700050303     d noctdes         s              1    dim(600)
007800041220     D CRE             S              2    DIM(200)
007900041220     D DRE             S             20    DIM(200)
008000010530     D NAZ             S              3    DIM(20)
008100010601     D* CODIICI YTASSAZIONE DELLA LINEA DEL CLIENTE
008200041220     D CTL             S              2    DIM(600)
008300010530     D* SCHIERE PER TARIFFE PARTICOLARI
008400010530     D FTC             S              1    DIM(50)
008500010530     D DTC             S             25    DIM(50)
008600020108     D* ordinamento in stampa tariffe particolari
008700020108     D OST             S              2    DIM(50)
008800020108     D WOS             S              2    DIM(50)
008900020108     D TST             S              1    DIM(50)
009000020108     D CP              S              1    DIM(50)
009100020108     D* estensione tabella 1P per stampa
009200020108     D EDTC            S             31    DIM(50)
009300020108     D EDAC            S              1    DIM(50)
009400020109     D ESUD            S              1    DIM(50)
009500010601     D* SCHIERE PER LE NOTE
009600010601     D NOTE            S             78    DIM(4)
009700010606     D* --------------  SCHIERE TARIFFE IN DETTAGLIO ------------------------
009800010606     D* SCAGLIONI
009900010606     D SGL             S              7  2 DIM(18)
010000010606     D* INOLTRO
010100010606     D PRO             S              7  3 DIM(18)
010200010606     D* RAPPORTO PESO VOLUME
010300010606     D RPV             S              4  0 DIM(18)
010400010606     D*--------------- SCHIERE PER LA STAMPA -----------------
010500010606     D*"/" di divisione negli scaglioni da a
010600010606     D SGLB            S              1    DIM(18)
010700010606     D* SCAGLIONI senza decimali
010800010606     D SGLD            S              4  0 DIM(18)
010900010606     D SGLA            S              4  0 DIM(18)
011000010606     D* SCAGLIONI con 1 decimale
011100010606     D SG1D            S              3  1 DIM(18)
011200010606     D SG1A            S              3  1 DIM(18)
011300010606     D* SCAGLIONI con 2 decimali
011400010622     D SG2D            S              4  2 DIM(18)
011500010622     D SG2A            S              4  2 DIM(18)
011600080901     D* SCAGLIONI con 3 decimali
011700080901     D SG3D            S              4  3 DIM(18)
011800080901     D SG3A            S              4  3 DIM(18)
011900010606     D* DECODIFICA SOTTO GLI SCAGLIONI
012000010606     D ATF             S              7    DIM(18)
012100010607     D* ------------------- 6 SCAGLIONI PER LA STAMPA --------------------------
012200010607     D SSLB            S              1    DIM(6)
012300010607     D* SCAGLIONI SENZA DECIMALI
012400010607     D SSLD            S              4  0 DIM(6)
012500010607     D SSLA            S              4  0 DIM(6)
012600010607     D* SCAGLIONI CON 1 DECIMALI
012700010607     D SS1D            S              3  1 DIM(6)
012800010607     D SS1A            S              3  1 DIM(6)
012900010607     D* SCAGLIONI CON 2 DECIMALI
013000010622     D SS2D            S              4  2 DIM(6)
013100010622     D SS2A            S              4  2 DIM(6)
013200080901     D* SCAGLIONI CON 3 DECIMALI
013300080901     D SS3D            S              4  3 DIM(6)
013400080901     D SS3A            S              4  3 DIM(6)
013500010612     D* ------------------- 6 APPLICAZIONE TARIFFA FINITA ----------------------
013600010612     D STF             S              7    DIM(6)
013700010614     D* ------------------- 6 SCAGLIONI E DETTAGLI TARIFFARI -------------------
013800010614     D SSL             S              7  2 DIM(6)
013900080916     D SSLP            S              8  3 DIM(6)
014000010614     D TCT             S              7  3 DIM(6)
014100010614     D TMN             S              7  3 DIM(6)
014200010614     D TMX             S              7  3 DIM(6)
014300010614     D TIN             S              7  3 DIM(6)
014400010614     D* ------------------- SCHIERA DI COMODO ----------------------------------
014500010614     D COM             S              7    DIM(6)
014600010615     D CO4             S              4    DIM(6)
014700010614     D*  SKI TARIFFE X SCAGL. DI OGNI PROVINCIA(CTS) DELLA REGIONE
014800050616     D BIG             S             42    DIM(15)
014900050616     D BMN             S             42    DIM(15)
015000050616     D BMX             S             42    DIM(15)
015100050616     D BIN             S             42    DIM(15)
015200050616     D BSI             S              2    DIM(15)
015300050616     D BRG             S              1    DIM(15)
015400050616     d dprv            s             20    dim(15)
015500050616     d nodes           s              1    dim(15)
015600010614     D* SKIERE DI WORK
015700010614     D WIN             S             42    DIM(30)
015800010614     D WMN             S             42    DIM(30)
015900010614     D WMX             S             42    DIM(30)
016000050616     D WCT             S              3    DIM(15)
016100010614     D WTI             S             42    DIM(30)
016200010614     D* SKIERE DI STAMPA
016300010614     D PDE             S             29    DIM(30)
016400010614     D PAM             S              7  3 DIM(6)
016500010614     D PMN             S              7  3 DIM(6)
016600010614     D PMX             S              7  3 DIM(6)
016700010614     D PIN             S              7  3 DIM(6)
016800010614     D*     % PER SCAGLIONI A VALORE
016900010614     D AI2             S              1    DIM(6)
017000010615     D*     SCAGLIONI STAMPA RAPPORTO PESO VOLUME
017100010615     D SPV             S              4  0 DIM(18)
017200010618      *-------------------------------------------------------------------------
017300010618      *         SCHIERE SCAGLIONI DETTAGLIO TARIFFE PARTICOLARI
017400010618      *-------------------------------------------------------------------------
017500010618     D* --------------  SCHIERE TARIFFE IN DETTAGLIO ------------------------
017600010618     D* SCAGLIONI
017700010618     D*
017800080916     D SCP             S              8  3 DIM(18)
017900010619     D* TARIFFA PARTICOLARE DETTAGLIO
018000010621     D TAP             S              7  3 DIM(6)
018100010621     D TPN             S              7  3 DIM(6)
018200010621     D TPX             S              7  3 DIM(6)
018300010621     D AIP             S              1    DIM(6)
018400010620     D*
018500010621     D PER             S              1    DIM(6)
018600010619     D*
018700021204      *          SCHIERE PER STAMPA TARIFFE PARTICOLARI SU + RIGHE
018800021204     d TptA            s             31    Dim(50)
018900021204     d TptB            s             25    Dim(50)
019000021204     d Tptc            s             31    Dim(50)
019100010614     D*
019200031119     D*-------------------------------------------------------------------------
019300101015      * DATE SCATTI ISTAT
019400101015     D DIA             S              8  0 DIM(999)
019500060203
019600080609      * schiera filiali abilitate al lasciato avviso
019700080609     d sklav           s              3  0 dim(999)
019800010601      *-------------------------------------------------------------------------
019900010530     D* DS DEL FILE TABEL00F
020000010621     D DSCV          E DS
020100010530     D DS1H          E DS
020200010530     D DS1P          E DS
020300020109     D DSSP          E DS
020400010530     D DS2G          E DS
020500010618     D DS3IDP        E DS
020600010530     D DSCT          E DS
020700010530     D DS15          E DS
020800010531     D DSTR          E DS
020900010601     D DSFA          E DS
021000010614AI2  D DS4U          E DS
021100031119      *
021200010725     D* DS DEL FILE TNTBE00F
021300010725     D DTAA          E DS
021400161221     D* DS DEL FILE TNTBE00F modello della privacy
021500161221     D DATP          E DS
021600010531     D*
021700010625     D* VALORI STANDARD AZIENDALI
021800010625     D DGEI          E DS
021900020625      * limite c/assegni
022000020625     d dlca          e ds
022100010531     D* DS flag operativi del file tariffe
022200010531     D DSTA01        E DS
022300010601     D* DS organigramma per poste
022400010601     D OG143         E DS
022500020530      * DS organigramma per codice regione
022600020530     d og147         e ds
022700020624      *--------
022800020624      * Ds di riferimento al file esterno AzUte00F
022900020624      *--------
023000020624     d AzuteDs       e ds                  ExtName(AzUte00F)
023100020624      *--------
023200020624      * Ds per dati organigramma
023300020624      *--------
023400020624     d DDatiUte      e ds
023500020624      *--------
023600020624      * Parametri per richiamo al pgm di controllo profilo utenti
023700020624      *--------
023800020624     d Tibs34Ds      e ds                  Inz
023900020624
024000010531     D*
024100010530     D KPJBA         E DS
024200161223      // - flag di errore nel caso in cui non sia riuscita a stampare
024300161223      //   gli allegati condizioni generali e privacy.
024400161223      // questo flag verrà getsito dal pgm fnlv43r che emetterà un msg di errore
024500161223     D err_allegati          497    497
024600010531     D*
024700160623      // - Ricerca/Controllo tabelle
024800160623     d TIBS02DS      e ds                  inz
024900010625     D*
025000020919     D fnlv45ds      E DS
025100021022     D fnlv41ds      E DS
025200041230     d Tibs10ds      e ds
025300080609     d dlav          e ds
025400080609     d tntbeds       e ds                  extname(tntbe00f)
025500160623      // - Tabella limiti Tipo Servizio
025600160623     d dLTS          e ds
025700161221     D*********
025800161221     D* DS DI PROCEDURA
025900161221     D*********
026000161221     D TRBMATRR1DS     ds                  qualified inz
026100161221     D   piCodRegola                  3A
026200161221     D   piCodOperat                 15A
026300161221     D   piUsrData                   10A
026400161221     D   piTipoModulo                10A
026500161221     D   piPrinter                   10A
026600161221     D   poEsitoOK                    1A
026700161221
026800161221
026900161221     D*********
027000161221     D* PROTOTIPI CHIAMATE ESTERNE
027100161221     D*********
027200161221     D*
027300161221     D TRBMATRR1       PR
027400161221     D                                     extpgm('TRBMATRR1')
027500161221     D piCodRegola                         like(TRBMATRR1DS.piCodRegola)
027600161221     D piCodOperat                         like(TRBMATRR1DS.piCodOperat)
027700161221     D piUsrData                           like(TRBMATRR1DS.piUsrData)
027800161221     D piTipoModulo                        like(TRBMATRR1DS.piTipoModulo)
027900161221     D piPrinter                           like(TRBMATRR1DS.piPrinter)
028000161221     D poEsitoOK                           like(TRBMATRR1DS.poEsitoOK)
028100161221     D*
028200161221
028300010530      *-------------------------------------------------------------------------
028400010601      * ds interne
028500010601     D*
028600010601     D CDPKEY          DS
028700010601     D  CDPG                   1      3
028800010601     D  GGPG                   2      3
028900010601     D  CDLIN                  4      4
029000010627      *
029100010627     D PRTTAR          DS
029200010627     D  curline              367    368I 0
029300010627      *
029400010601      *-------------------------------------------------------------------------
029500010531      *
029600010531     D DATA_eur        S               D   DATFMT(*eur)
029700060816     d data_iso1       s               d   datfmt(*iso)
029800060816     d data_iso2       s               d   datfmt(*iso)
029900010531      *
030000130905     d wCMMcod         s                   like(CMMcod)   inz
030100010601     d cod             s                   like(tblcod)
030200010625     d costpg          s                   like(§fatpg)
030300010601     d ctrata          s                   like(§trata)
030400010531     D ds1ksc          s                   like(tamksc)
030500010531     D ds1ctr          s                   like(tamctr)
030600010531     D ds1prg          s                   like(tamprg)
030700010601     D notapl          s                   like(ntcapl)
030800010621     D notke1          s                   like(ntcnk1)
030900010621     D notke2          s                   like(ntcnk2)
031000010601     D nottnt          s                   like(ntctnt)
031100111103     D kcod            s                   like(tbecod)
031200010601     D key             s                   like(tblkey)
031300010601     D ksc             s                   like(acoksc)
031400010724     D oltre           s                   like(tpdsgl)
031500010531     D parsks          s                   like(tamksc)
031600060307     d savapl          s                   like(tptapl)
031700010618     D savftc          s                   like(tptftc)
031800010621     D savorp          s                   like(tpdorp)
031900010801     D savorp1         s                   like(tpdorp)
032000010621     D savsgl          s                   like(tpdsgl)
032100010806     D recsgl          s                   like(tpdsgl)
032200010604     D rotcap          s                   like(tadcap)
032300010604     D rotcts          s                   like(tadcts)
032400010604     D rotlnp          s                   like(tadlnp)
032500010604     D wrkcap          s                   like(tadcap)
032600010604     D wrkcts          s                   like(tadcts)
032700010626     D wrkftc          s                   like(tpdftc)
032800010604     D wrklnp          s                   like(tadlnp)
032900010626     D wrkorp          s                   like(tpdorp)
033000010626     D wrksgl          s                   like(tpdsgl)
033100010531      *-------------------------------------------------------------------------
033200021204     d atp1            s              2  0
033300021204     d atpc            s              2  0
033400010614     D b               s              2  0
033500021205     D blk10           s             10    INZ(*ALL'.')
033600030318     D blk12           s             13    INZ(*ALL'.')
033700030318     D blk21           s             21    INZ(*ALL'.')
033800030318     D blk25           s             25    INZ(*ALL'.')
033900021205     D blk36           s             36    INZ(*ALL'.')
034000021205     D blk56           s             56    INZ(*ALL'.')
034100010614     D c               s              2  0
034200010604      * se esiste il cap nel 1° scaglione metto 'S'
034300010604     D cap1scaglio     s              1
034400021205     d CntList         s              5i 0 Inz(*Zeros)
034500161221     D coda_allegati   s             10
034600010615     D codorp          s              1
034700010615     D codreg          s              2
034800010615     D codsig          s              2
034900010601     D comfil          s              3  0
035000020109     D comftc          s              1
035100010613     D compro          s              1
035200010613     D comreg          s              2
035300010621     D com04           s              4
035400010614     D como29          s             29
035500010614     D como42          s             42
035600010614     D cmno42          s             42
035700010614     D cmxo42          s             42
035800010614     D cino42          s             42
035900021204     d conta           s              1  0
036000010608     D ctrcap          s              1
036100010601     D datastampa      s             10
036200010723     D desctr          s             13
036300050204     d desprv          s             20
036400010615     D desreg          s             14
036500010618     D de1tpg          s              9
036600010618     D de3tpg          s             13
036700010801     D dissca2         s             63
036800010801     D dissca3         s             50
036900010801     D dissca4         s             37
037000010801     D dissca5         s             24
037100010801     D dissca6         s             11
037200010608     D ds3um           s             20
037300010531     D ds6pr2          s              1
037400010621     D errmsg          s             78
037500010614     D h               s              2  0
037600010601     D i96dri          s              8  0
037700020108     D j               s              3  0
037800020108     D jj              s              3  0
037900020108     D jjj             s              3  0
038000010605     D lnpcli          s              2
038100010724     D mintas          s              3  0
038200010619     D num4            s              4  0
038300050303     d nostdes         s              1
038400020114     D oltrenodec      s             10  0
038500010601     D o96ver          s              5  0
038600010614     D p               s              2  0
038700010531     D k               s              3  0
038800010531     D k1              s              3  0
038900010618     D k2              s              3  0
039000010607     D k4              s              3  0
039100010622     D k5              s              3  0
039200010604     D Y               s              3  0
039300010618     D X               s              2  0
039400010531     D unoctr          s              1  0
039500010625     D recval          s              3  0
039600161221     D regola          s              3
039700010604     D rotpro          s              1
039800010613     D rotreg          s              2
039900010531     D staddt          S             10
040000010531     D stadst          S             10
040100020624     d stalnp          s             20
040200010606     D stino           S              1
040300010725     D stpanno         s              4  0
040400010725     D stpmod          s              3  0
040500161221     D stpannoi        s              4  0
040600161221     D stpmodi         s              3  0
040700030318     D stpper          s              7
040800010615     D totino          s             15  3
040900050526     d totrig          s              5  0
041000010731     D unmis           s              3
041100030318     D valper          s              6  3
041200080211     d wagenzia        s             16
041300010615     D waro2           s              4  0
041400010615     D warf2           s              4  0
041500010615     D warl2           s              4  0
041600100506     D wata            s              5  0
041700100506     D wata1           s              6  1
041800100506     D wata2           s              7  2
041900080211     d wbanca          s             20
042000060925     d wctr1           s              1
042100051206     d wema84          s              1    Inz('0')
042200050526     d wema85          s              1    Inz('0')
042300050526     d wema87          s              1    Inz('0')
042400050526     d wema88          s              1    Inz('0')
042500090612     d wema89          s              1    Inz('0')
042600060816     d wgio            s              5  0
042700021204     d Witr            s              8  3
042800010618     D wnumsc          s              2  0
042900051006     d wnrscaglio      s              2  0
043000010625     D w_per           s              1
043100050930     d woktar          s              1    Inz('0')
043200021204     d WokTp           s              1    Inz('0')
043300080211     d wpos            s              2  0
043400080211     d wpos1           s              2  0
043500080211     d wpos2           s              2  0
043600041230     d wpsw            s              1    Inz('0')
043700050204     d wragprv         s              1    Inz('0')
043800010615     D wrpv            s             24
043900010626     D wrkdestpg       s             13
044000010604     D wrkreg          s              2
044100010604     D wrkpro          s              1
044200020624     D wrkslp          s             20
044300020703     D wrkprtlnp       s             80
044400010613     D wsgl72          S              7  2
044500080916     D wsgl83          S              8  3
044600011001     D w_ellimris      S              1
044700021205     D w_tarpar        S             83
044800021205     D w_tarparest     S             83
044900020531     d w_oklnp         s              1
045000020531     d w_okstp         s              1
045100010723     D w0010           S              1  0
045200010606     D w0020           S              2  0
045300010601     D w003a           S              3
045400010606     D w0030           S              3  0
045500010606     D w0050           S              5  0
045600051006     d xx              s              2  0
045700051006     d yy              s              2  0
045800080117     d xy              s              2  0
045900010606     D z               s              3  0
046000080609     d xxx             s              3  0
046100091112     d $trattativa     s              1n
046200101015     d $EoF            s              1n   inz(*off)
046300110216     d savk            s              3  0
046400160623     d Stpvlm          s              6a   inz
046500160623     d Stppkg          s              8a   inz
046600160623     d Decimali        s              3s 0 inz
046700021205      *-----------------
046800021205      * PARAMETRI DI PASSAGGIO AL PROGRAMMA DI "GIUSTIFICAZIONE" TESTO
046900021205      *-----------------
047000021205     d TxtInput        S            200    varying
047100021205     d Lungh           S              5i 0 inz(105)
047200021205     d TxtGiust        S            200    varying
047300021205     d TxtResto        S            200    varying
047400021205
047500010625     D* DEFINIZIONE COSTANTI
047600060817     D CSPE1           C                   CONST('solo sulle spese di sosta')
047700010625     D CSPE2           C                   CONST('su tutte le spese')
047800060817     D CSPE3           C                   CONST('su spese sosta,dossier, -
047900060817     D                                     varie')
048000060817     D CSPE12          C                   CONST('only on holding costs')
048100060703     D CSPE22          C                   CONST('on all costs')
048200060817     D CSPE32          C                   CONST('on holding, dossier, -
048300060817     D                                     various costs')
048400060817     D CSPE13          C                   CONST('uniquement sur les frais -
048500060817     D                                     d''arrêt')
048600060817     D CSPE23          C                   CONST('sur tous les frais')
048700060817     D CSPE33          C                   CONST('sur les frais d''arrêt, de -
048800060817     D                                     dossier, divers')
048900060817     D CSPE14          C                   CONST('nur auf Kosten für -
049000060817     D                                     Aufbewahrung')
049100060817     D CSPE24          C                   CONST('auf alle Kosten')
049200060817     D CSPE34          C                   CONST('uf Kosten für Aufbewahrung,-
049300060817     D                                     Bearbeitung,Verschiedenes')
049400140527     D Cost_m_gratis   C                   CONST(': GRATIS fino al 31/12/2015-
049500140528     D                                     . Euro 0,20 dal 1/1/2016.')
049600140528     D Cost_m_gratisI  C                   CONST(': FREE until 31/12/2015. Eu-
049700140528     D                                     ro 0,20 as of 1/1/2016.')
049800140528     D Cost_m_gratisF  C                   CONST(': GRATUIT jusqu''au 31/12/20-
049900140528     D                                     15. Euro 0,20 à partir du 1/1/2016.')
050000140528     D Cost_m_gratisD  C                   CONST(': KOSTENLOS bis zum 31.12.20-
050100140528     D                                     15. Euro 0,20 ab 01.01.2016.')
050200140218
050300140218       // -?DS campo TPTflo
050400140218     d dTPT01        e ds
050500160623
050600160623      //---------------------------------------------------------------
050700160623      //?Definizione Prototipi.
050800160623      //---------------------------------------------------------------
050900160623      /copy gaitrasrc/srcprotopr,TIBS02R
051000160623
051100160623      //---------------------------------------------------------------
051200091022
051300091022     itivis000
051400091022     i              visesi                      wisesi
051500091022
051600010530     C****************************************************************
051700010530     C*  RIEPILOGO INDICATORI
051800010530     C***************************************************************
051900010604     C* 02    - TARIFFA EUROEXPRESS
052000010618     C* 04    - TARIFFA PARTICOLARE A ZERO
052100010626     C* 06    - TARIFFA PARTICOLARE ISOLA
052200010618     C* 08    - STAMPA MINIMO TASSABILE PER TARIFFA A Q.LE
052300010801     C* 09    - TIPO PAGAMENTO A VALORE (TARIFFA %) TARIFFE PARTICOLARI
052400010626     C* 10    - LEGGO TARIFFE PARTICOLARI
052500061207      * 12    - STAMPA FUEL (tariffa "f ") A SCAGLIONI)
052600010626     C* 18    - STAMPA ARROTONDAMENTI
052700010626     C* 19    - STAMPA RAPPORTO PESO VOLUME
052800010626     C* 21    - MANDATO A VALORE VARIABILE
052900010626     C* 22    - ELEVAZIONE LIMITE RISARCIBILE
053000010626     C* 24    - MANDATO FITTIZIO
053100010531     C* 25    - TARIFFA DPD
053200010626     C* 26    - CONDIZIONA LA STAMPA DEL "PER" DI FIANCO ALLA VALIDITA' TARIFFA
053300041129     C* 27    - CONDIZIONA LA STAMPA DELL'ADDIZIONALE DI GESTIONE 1° riga
053400010928     C* 28    - CONDIZIONA LA STAMPA DELLE SPESE GIACENZA VARIE A SPEDIZIONE
053500041129     C* 29    - CONDIZIONA LA STAMPA DELL'ADDIZIONALE DI GESTIONE 2° riga
053600010626     C* 31    - INDICATORE DI COMODO
053700010626     C* 35    - INDICATORE DI COMODO
053800010606     C* 37    - SCAGLIONI CON UN SOLO DECIMALE
053900010618     C* 40/46 - CONDIZIONA LA STAMPA DEGLI SCAGLIONI
054000010615     C* 48    - STAMPO IL MINIMO
054100010615     C* 49    - STAMPO IL MASSIMO
054200010725     C* 51/56 - CONDIZIONA LA STAMPA DEGLI SCAGLIONI VUOTI
054300010725     C* 57    - STAMPO INOLTRO
054400010615     C* 61/66 - CONDIZIONA LA STAMPA DEL DETTAGLIO TARIFFARIO
054500010725     C* 72    - CONDIZIONA LA STAMPA DELL'ADDEBITO SPESE DI INCASSO
054600010628     C* 73    - CONDIZIONA LA STAMPA DEL RESPONSABILE TRASPORTI
054700010626     C* 75    - STAMPO MINIMO
054800010628     C* 77    - CONDIZIONA LA STAMPA DEL E.P.C.
054900010626     C* 78/79 - CONDIZIONA LA STAMPA DELLA TESTATA DEGLI SCAGLIONI
055000010627     C* 80    - OVERFLOW
055100010627     C* 81    - CONDIZIONA LA STAMPA DELLA TESTATA ASSICURAZIONI MERCE
055200091117      * 85    - Trattativa in stampa
055300010626     C* 86    - STAMPO MASSIMO
055400010531     C* 87    - TARIFFA A VALORE
055500020114     C* 88    - OLTRE        CON DECIMALI
055600020114     C* 89    - ARRONDAMENTI CON DECIMALI
055700010606     C* 91    - SE ACCESO SCAGLIONI CON DECIMALI
055800010530     C* 92/97 - STAMPA DATI DALLA DS1H
055900010530     C* 98    - ON PROVENGO RA OFFERTA
056000010530     C* 99    - INDICATORE GENERALE DI ERRORE
056100010530     C*****************************************************************
056200010531      *-------------------------------------------------------------------------
056300010531      *    T E S T A T A    S T A M P A    T A R I F F A
056400010531      *-------------------------------------------------------------------------
056500021022      *
056600021022      * Per errore di reperimento tetso vado a fine pgm
056700021022     C                   IF        $NoStp   = *on
056800021022     C                   GOTO      FINE
056900021022     C                   ENDIF
057000080609
057100080609     c/exec sql
057200080609     c+ set option dynusrprf = *owner, closqlcsr = *endmod
057300080609     c/end-exec
057400080609
057500080609      * carico tabella filiali attive al lasciato avviso
057600080609     c                   exsr      carlav
057700050930
057800050930     c                   eval      woktar = *Off
057900010531      *
058000010531      * RECUPERO IL CLIENTE
058100010601     C                   EXSR      SR_RECCLI
058200010601      *
058300010601      * SE 99 ACCESO ERRORE E VADO A FINE NON TROVATO  CLIENTE IN ANAGRAFICA
058400010601      *
058500010601     C                   IF        *IN99
058600010601     C                   GOTO      FINE
058700010601     C                   ENDIF
058800010601      *
058900010601      * DECODIFICO I DATI DEL CLIENTE
059000010601      *
059100010601     C                   EXSR      SR_DECANA
059200010601      *
059300010601      * RECUPERO NOTE TARIFFA/OFFERTA CLIENTE E RESPONSABILE TRASPORTI
059400010601      *
059500010601     C                   EXSR      SR_RECNOT
059600010601      *
059700010601      * DECODIFICO I DATI DEL PUNTO OPERATIVO
059800010601      *
059900010601     C                   EXSR      SR_DECPO
060000010601      *
060100010601      * aggancio testata tariffa / offerta
060200010621     c                   eval      ds1ksc = parsks
060300010531     c                   eval      ds1prg = i45prg
060400010531     c                   eval      ds1ctr = i45ctr
060500010601
060600061122     c  n98ds1ktm        chain     tntam000
060700061122     c   98ds1ktm        chain     tnofm000
060800010531
060900010531      * decodifica delle condizioni generali
061000010531
061100010531     c                   exsr      SR_DECTAM
061200020205
061300020205      * carico le tariffe particolari
061400020205     c                   exsr      Sr_Cartarp
061500010604      *
061600010604      * stampo la prima parte della tariffa
061700010604      *
061800010622     c                   write     ta49pPP
061900010604     c                   write     ta49p01
062000010531      *
062100010605      * elaboro dettaglio tariffario per recuperare scaglioni/inoltro/rpv
062200010604      *
062300010604     c                   exsr      SR_SCAGLIONI
062400050930      * se si sono vericati errori non continuo
062500050930     c                   if        *in99
062600050930     c                   goto      fine
062700050930     c                   endif
062800010606      *
062900010606      * verifico se devo stampare l'inoltro alla fine della tariffa xchè uguale
063000010606      * per tutti oppure no
063100010606     c                   exsr      SR_STINO
063200020214      * se si sono vericati errori non continuo
063300020214     c                   if        *in99
063400020214     c                   goto      fine
063500020214     c                   endif
063600010607      * elaboro e stampo il dettaglio tariffario
063700010606      *
063800010607     c                   exsr      SR_ELADET
063900010614      * se si sono vericati errori non continuo
064000010614     c                   if        *in99
064100010614     c                   goto      fine
064200010614     c                   endif
064300010628      *
064400010628      * stampo arrotondamenti
064500010628     c                   EXSR      SR_ARROTO
064600010618      * stampo i dati della testata tariffa
064700010618      *
064800010618     c                   exsr      SR_STATAM
064900050930     c                   eval      woktar = *On
065000010618      * routine gestione tariffe particolari
065100010618     c                   exsr      SR_STATPT
065200021204      * routine stampa sk unica tariffe particolari
065300021204     c                   ExSr      Sr_StaTpt1
065400010625      * routine di gestione tariffe giacenza
065500010625     c                   exsr      SR_STATGC
065600010625      * stampo condizioni di pagamento
065700010625      *
065800010627     c                   exsr      SR_OVER
065900010625      *
066000010625     c                   write     ta49pcon
066100010625      * se pagamento che utilizza la banca stampo anche questa riga
066200091016      * pagamento RIBA = '1' RID = '2'
066300091016     c                   if        costpg = '1' or costpg = '2'
066400010625      *
066500010627     c                   exsr      SR_OVER
066600010625      *
066700010625     c                   write     ta49pcon1
066800010625     c                   endif
066900021204      * Per tutte le tariffe stampo dicitura assicurazione merce
067000150908     c                   Add       4             curline
067100031008     c                   ExSr      Sr_Over
067200021204     c                   Write     Ta49pam
067300060324      * Per tutte le tariffe italia stampo anche la parte relativa all'AC BASE
067400060324     c                   If        Not *In25 and Not *In02
067500150917     c                   Add       2             curline
067600150917     c                   ExSr      Sr_Over
067700060324     c                   Write     Ta49pam1
067800060324     c                   endif
067900040211
068000040211      * Per tutte le tariffe italia stampo paragrafo x telefono destinatario
068100040211     c                   If        Not *In25 and Not *In02
068200040216     c                   Add       6             curline
068300040211     c                   ExSr      Sr_Over
068400040211     c                   Write     Ta49pte
068500040211     c                   EndIf
068600040211      * Per tutte le tariffe EEX stampo paragrafo x telefono destinatario
068700040211     c                   If        *In02
068800040211     c                   Add       3             curline
068900040211     c                   ExSr      Sr_Over
069000040211     c                   Write     Ta49ptee
069100040211     c                   EndIf
069200020624      * Per EuroExpress
069300020624     c                   if        *in02
069400020625      * cerco limite massimo per il c/assegno
069500020625     c                   clear                   dlca
069600160623     c                   clear                   tibs02ds
069700020625     c                   movel     'C'           t02mod
069800020625     c                   movel     knsif         t02sif
069900020625     c                   movel     'LCA'         t02cod
070000020625     c                   movel(p)  '011'         t02ke1
070100020625     c                   call      'TIBS02R'
070200020625     c                   parm                    kpjba
070300160623     c                   parm                    tibs02ds
070400020625     c                   if        t02err = *blanks
070500020625     c                   movel     t02uni        dlca
070600020625     c                   endif
070700020625     c                   eval      stalca = §lcax2
070800020412      * stampa dicitura fissa per porti assegnati export
070900031008     c                   Add       3             curline
071000031008     c                   exsr      SR_OVER
071100020412     c                   write     ta49pee
071200020624      * stampa dicitura fissa per contrassegni export
071300061116     c                   Add       8             curline
071400031008     c                   exsr      SR_OVER
071500020624     c                   write     ta49pee1
071600020412     c                   endif
071700050103      *
071800050526      * Se offerta
071900050526      * Capitolo dei servizi ON LINE + E-MAIL
072000050526     c                   If        *In98
072100050526     c                   ExSr      Sr_Comunica
072200050526     c                   EndIf
072300041229      * stampa condizioni
072400010625      *
072500161221     c                   Add       4             curline
072600161227      * per il tedesco ci sono 2 righe in più
072700161227     c                   IF        I45lin = '4'
072800161227     c                   Add       2             curline
072900161227     c                   ENDIF
073000010627     c                   exsr      SR_OVER
073100010625     c                   write     ta49pe1
073200010625      *
073300041229      * stampa condizioni e firme finali
073400010627     c                   exsr      SR_OVER
073500010625     c                   write     ta49pfi
073600010625      *
073700010627     c                   exsr      SR_OVER
073800010625     c                   write     ta49pe2
073900010627      *
074000010627     c                   exsr      SR_OVER
074100010627     c                   write     ta49pe2B
074200010627      *
074300010627     c                   exsr      SR_OVER
074400010627     c                   write     ta49pe2C
074500010627      *
074600010627     c                   exsr      SR_OVER
074700010627     c                   write     ta49pe2D
074800010627      *
074900010627     c                   exsr      SR_OVER
075000010627     c                   write     ta49pe2E
075100010627      *
075200010627     c                   exsr      SR_OVER
075300010627     c                   write     ta49pe2F
075400110905      *
075500110905     c                   exsr      SR_OVER
075600110905     c                   write     ta49pe2G
075700011009      *
075800011009     c                   exsr      SR_OVER
075900010625     c                   write     ta49pfi
076000161227     c                   eval      page = page - 1
076100161227     c                   write     ta49pfine
076200161223
076300161223     c                   close     tnta49p
076400010625      *
076500161223      * Stampo allegati condizioni generali e privacy se non mando la tariffa
076600161223      * via e-mail
076700161221
076800161223     c                   If        I45fax <> 'M'
076900161221      * Verifico in che lingua devo stampare gli allegati
077000161221     c                   If        i45lin = 'I'
077100161221     c                   eval      regola = 'TM1'
077200161221     c                   else
077300161221     c                   eval      regola = 'TM' + i45lin
077400161221     c                   endif
077500161221      /free
077600161221
077700161221        TRBMATRR1DS.piCodRegola  = regola;
077800161221        TRBMATRR1DS.piCodOperat  = '*ATTACH*';
077900161221        TRBMATRR1DS.piUsrData    = 'ALLEGATI';
078000161223        TRBMATRR1DS.piTipoModulo = 'LET1';
078100161221        TRBMATRR1DS.piPrinter    = coda_allegati;
078200161221        TRBMATRR1DS.poEsitoOK    = *blanks;
078300161221
078400161221        callP(e) TRBMATRR1 (TRBMATRR1DS.piCodRegola
078500161221                           :TRBMATRR1DS.piCodOperat
078600161221                           :TRBMATRR1DS.piUsrData
078700161221                           :TRBMATRR1DS.piTipoModulo
078800161221                           :TRBMATRR1DS.piPrinter
078900161221                           :TRBMATRR1DS.poEsitoOK
079000161221                           );
079100161223        err_allegati = TRBMATRR1DS.poEsitoOK ;
079200161221      /End-free
079300161221      *
079400161223     c                   endif
079500010614      *
079600010614     c     fine          tag
079700041229
079800010614     c                   seton                                        lr
079900010601     C*------------------------------------------------------------------------
080000010601     C*   SR_RECCLI  RECUPERO CLIENTE
080100010601     C*------------------------------------------------------------------------
080200010601      *
080300010601     C     SR_RECCLI     BEGSR
080400010601      *
080500010601      *
080600010601      * V I S I T A
080700010601      *
080800010601     C                   IF        *IN98
080900010601      *
081000091022     c                   if        i45ast = 'T'
081100091022     C     i45nrv        chain     tivis000
081200091022     c                   endif
081300010601      * recupero ANAGRAFICA CLIENTE
081400010601      * se cliente nuovo leggo nel file anagrafici visite
081500010601     C                   z-add     visnrv        ksc
081600010601     c     ds7kac        chain     tfaco000                           99
081700010601     c  n99ds7kac        chain     tfind000                           99
081800010601     c  n99ds7kac        chain     tfclp000                           99
081900010601      * se non trovato cerco nell'anagrafica clienti
082000010601     c                   if        *in99
082100010601     C                   z-add     visksc        ksc
082200010601     c     ds7kac        chain     cnaco000                           99
082300010601     c  n99ds7kac        chain     cnind000                           99
082400010601     c  n99ds7kac        chain     cnclp000                           99
082500010601     c                   end
082600010601      *
082700010601     c                   end
082800010601      *
082900010601      * RECUPERO IL CLIENTE
083000010601      *
083100010601      * T A R I F F A
083200010601      *
083300010601     C                   IF        not *IN98
083400010601     C                   z-add     parsks        ksc
083500010601     c     ds7kac        chain     cnaco000                           99
083600010601     c  n99ds7kac        chain     cnind000                           99
083700010601     c  n99ds7kac        chain     cnclp000                           99
083800010601     c                   endif
083900010601      * se il cliente sia in visita che in tariffa non è stato trovato
084000010601      * viene stampato l'errore
084100010601     c                   if        *in99
084200010601     c                   move      MSG(3)        errmsg
084300010601     c                   exsr      staerr
084400010622      *
084500010622     c                   else
084600010622      * se non c'è errore valorizzo i campi in stampa
084700010622     c                   eval      picrag = acorag
084800010622     c                   eval      picind = indvia
084900010622     c                   eval      piccap = indcae
085000010622     c                   eval      picloc = indcit
085100010622     c                   eval      picpro = indprv
085200010601     c                   endif
085300010601      *
085400010601     C                   ENDSR
085500010601     C*------------------------------------------------------------------------
085600010601     C*   SR_DECANA  DECODIFICA DATI CLIENTI PER LA STAMPA
085700010601     C*------------------------------------------------------------------------
085800010601      *
085900010601     C     SR_DECANA     BEGSR
086000010622      * valorizzo i numeri riferimento
086100010622     c                   eval      picnr1 = i45nr1
086200010622     c                   eval      picnr2 = i45nr2
086300010601      *
086400010601      * ---------- decodfico la nazione se diversa da italia ------------
086500010601      *
086600010601     c                   if        indsta <> *blanks
086700010601     c                   eval      cod ='15'
086800010601     c                   movel     indsta        w003a
086900010601     c                   movel(p)  w003a         key
087000060724     c     tabkeyl       chain     tabel00f
087100010601     c                   if        %found(tabel00f)
087200010621     c                   movel     tbluni        desnar
087300010601     c                   else
087400010621     c                   clear                   desnar
087500010601     c                   endif
087600010601      *
087700010601     c                   endif
087800010601      *
087900010601      * ------------------ decodifico il commerciale --------------------
088000010601      *
088100010601      * visita
088200130905     c                   clear                   wCMMcod
088300010601      *
088400010601     c                   if        *in98
088500010601      * commerciale visita
088600010601     c                   if        viscmm > 0
088700130905     c                   z-add     viscmm        wCMMcod
088800010601     c                   endif
088900010601      * tariffa
089000010601     c                   else
089100010601      * commerciale tariffa
089200010601     c                   if        clpage > 0
089300130905     c                   z-add     clpage        wCMMcod
089400010601     c                   endif
089500010601      *
089600010601     c                   endif
089700010601      *
089800130905     c                   if        wCMMcod <> *zero
089900010601      *
090000130905     c     wCMMcod       chain     AZCMM000
090100130905     c                   if        %found(AZCMM01L)
090200130905     c                   movel     CMMdes        descom
090300010601     c                   else
090400010601     c                   clear                   descom
090500010601     c                   endif
090600010601      *
090700010601     c                   endif
090800010601      *
090900010601      * ------------------- decodifico il pagamento -------------------------
091000010601      *
091100010621     c                   if        indcdp <> *blanks
091200010625     c                   move      indcdp        cdpg
091300010601     c                   eval      cdlin = '1'
091400010601     c                   movel(p)  cdpkey        key
091500010601     c                   eval      cod = 'FA'
091600010601      *
091700060605     c     tabkeyl       chain     tabel00f
091800010601     c                   if        %found(tabel00f)
091900010601     c                   movel     tbluni        dsfa
092000010601     c                   movel     §fades        descdp
092100010601      * mi salvo il tipo pagamento per pilotare la stampa della banca d'appoggio
092200010601      * oppure (es. Rimessa diretta non si stampa la banca d'appoggio)
092300010625     c                   eval      costpg = §fatpg
092400010601     c                   endif
092500010601      *
092600010601     c                   endif
092700010601      * ----- converto spese di incasso in euro
092800010621     c                   if        indsin > *zeros
092900010725      *
093000010725     C                   SETON                                        72
093100010601      *
093200030210
093300030210     c                   Move      IndSin        ValSin
093400030210
093500010601     c                   endif
093600010601      * ------------ banca d'appoggio ---------------------------------
093700080211     c                   clear                   pcabi
093800080211     c                   clear                   pccab
093900080211     c                   clear                   desbna
094000080211    1c                   if        indabi <> 99999 or indcab <> 99999
094100080211     c                   eval      pcabi = indabi
094200080211     c                   eval      pccab = indcab
094300080211     c     kcnabi        chain     cnabi00f
094400080211    2c                   if        %found(cnabi00f) and abiann <> '*'
094500080211     c                   clear                   wagenzia
094600080211     c                   clear                   wbanca
094700080211     c                   Eval      wbanca = abiist
094800080211      * Compongo il nome della banca con località o comune
094900080211    3c                   If        abiage = *Blanks
095000080211     c                   Select
095100080211     c                   When      abiloc <> *Blanks
095200080211     c                   Eval      wagenzia = abiloc
095300080211     c                   When      abicom <> *Blanks
095400080211     c                   Eval      wagenzia = abicom
095500080211     c                   EndSl
095600080211      * Compongo il nome della banca con agenzia + località o comune
095700080211   x3c                   Else
095800080211      * Cerco il primo byte vuoto dell'agenzia
095900080211     c                   Eval      wpos = %checkr(' ':abiage)
096000080211
096100080211     c                   Eval      wagenzia = %subst(abiage:1:wpos)
096200080211     c                   Eval      wpos1 = wpos + 2
096300080211     c                   Eval      wpos2 = 16 - wpos1
096400080211      * Se wpos2 inferiore a zero lo imposto a 0
096500080211     c                   If        wpos2 < *Zeros
096600080211     c                   Clear                   wpos2
096700080211     c                   EndIf
096800080211      * Se posso aggiungere anche solo 1 carattere procedo
096900080211    4c                   If        wpos > 1 and wpos < 16
097000080211      * Cerco di riempire quello che resta con la località o il comune
097100080211     c                   Select
097200080211     c                   When      abiloc <> *Blanks
097300080211     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
097400080211     c                             %subst(abiloc:1:wpos2)
097500080211     c                   When      abicom <> *Blanks
097600080211     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
097700080211     c                             %subst(abicom:1:wpos2)
097800080211     c                   EndSl
097900080211    4c                   EndIf
098000080211
098100080211    3c                   EndIf
098200080211    2c                   EndIf
098300080211     c                   eval      desbna = wbanca + wagenzia
098400080211    1c                   endif
098500010601      *
098600010601      * --------- decodifico tipo pagamento contrassegno ---------------
098700010601      *
098800010601     c                   eval      cod = '4U'
098900010601     c                   movel(p)  clpfpc        key
099000060605     c     tabkeyl       chain     tabel00f
099100010601     c                   if        %found(tabel00f)
099200010601     c                   movel     tbluni        ds4u
099300010621     c                   movel     §4udes        comaas
099400010601      * se è un tipo pagamento che richiede obbligatoriamente il codice ABI CAB
099500010601      * stampo anche la decodifica della banca d'appoggio
099600010601     c                   if        §4uabi = 'S'
099700080118     c                   eval      combab1 = %subst(clpbab:1:4)
099800080118     c                   eval      combab2 = %subst(clpbab:5:4)
099900080118     c                   eval      combab3 = %subst(clpbab:9:4)
100000080118     c                   eval      combab4 = %subst(clpbab:13:4)
100100080118     c                   eval      combab5 = %subst(clpbab:17:4)
100200080118     c                   eval      combab6 = %subst(clpbab:21:4)
100300080118     c                   eval      combab7 = %subst(clpbab:25:4)
100400080211     c                   eval      combab8 = %subst(clpbab:29:4)
100500010601     c                   endif
100600010601      *
100700010601     c                   endif
100800010601      *
100900010601     c                   ENDSR
101000010601     C*------------------------------------------------------------------------
101100010601     C*   SR_RECNOT  RECUPERA NOTE TARIFFE/OFFERTA CLIENTI
101200010601     C*------------------------------------------------------------------------
101300010601      *
101400010601     C     SR_RECNOT     BEGSR
101500010601      *
101600010601      * ---------- chiave note tariffe/offerta cliente ------------------
101700010601      *
101800010601      * visita
101900010601     c   98              eval      notapl = 'V'
102000010601      * tariffa
102100010601     c  N98              eval      notapl = 'C'
102200091112      * trattativa
102300091112     c                   if        $trattativa
102400091112     c                   eval      notapl = 'T'
102500091112     c                   endif
102600010625      *
102700010625     c                   clear                   b
102800010601      *
102900010601     c                   movel     I45KCC        notke1
103000010625     c                   move      parsks        notke1
103100010601      * note legate alla tariffa/offerta
103200010601     c                   movel     '10'          nottnt
103300010601     c                   movel     I45CTR        notke2
103400010601      *
103500010601     c     keyntc        setll     tfntc01l
103600010601     c                   do        *hival
103700010601     c     keyntc        reade     tfntc01l
103800010601
103900010601     c                   if        %eof(tfntc01l)
104000010601      * imposto la chiave senza il codice tariffa e mi riposiziono
104100010601     c                   clear                   notke2
104200010601     c     keyntc        setll     tfntc01l
104300010601     c                   leave
104400010601     c                   endif
104500010601
104600010601     c                   if        ntcflt = 'A'
104700010601      * se è annullato leggo il successivo
104800010601     c                   iter
104900010601     c                   else
105000010601
105100010601      * se invece non è annullato ed esiste mi riposiziono per il caricamento
105200010601      * della schiera di comodo delle note
105300010601     c     keyntc        setll     tfntc01l
105400010601     c                   leave
105500010601     c                   endif
105600010601
105700010601     c                   enddo
105800010601      * carico le note in schiera
105900010601     c                   do        *hival
106000010601     c     keyntc        reade     tfntc01l
106100010601
106200010601     c                   if        %eof(tfntc01l)
106300010601     c                   leave
106400010601     c                   endif
106500010601      * se annullato oppure è una nota interna leggo la successiva
106600010621     c                   if        ntcflt = 'A' or ntcsns <> 'S'
106700010601     c                   iter
106800010601     c                   endif
106900010601      *
107000010601     c                   add       1             B
107100010601     c                   movel     ntcrnt        note(b)
107200010601      * se b = 4 numero massimo di note da stampare vado a fine
107300010601     c                   if        b = 4
107400010601     c                   leave
107500010601     c                   endif
107600010601
107700010601     c                   enddo
107800010601
107900010601      * ------------ RESPONSABILE TRASPORTI ---------------------------------
108000091112
108100100923     c                   clear                   notke2
108200100923
108300091112      * se trattativa su cliente cerco con il cliente
108400091112     c                   if        $trattativa and visksc > 0
108500091112     c                   eval      notapl = 'C'
108600091112     c                   move      visksc        notke1
108700091112     c                   endif
108800010601
108900010601     c                   movel     '05'          nottnt
109000010601     c     keyntc        setll     tfntc01l
109100010601     c                   do        *hival
109200010601     c     keyntc        reade     tfntc01l
109300010601
109400010601     c                   if        %eof(tfntc01l)
109500010601     c                   leave
109600010601     c                   endif
109700010601      * se annullato oppure è una nota interna leggo la successiva
109800010621     c                   if        ntcflt = 'A' or ntcsns <> 'S'
109900010601     c                   iter
110000010601     c                   endif
110100010601      * se alla c.a. è uguale a blank carico la nota
110200010621     c                   if        resptra = *blanks
110300010601     c                   movel     ntcrnt        resptra
110400010628     C                   SETON                                        73
110500010601      *
110600010601     c                   else
110700010601      * se è già pieno scrivo nel nominativo del e.p.c. ed esco
110800010601     c                   movel     ntcrnt        resptra2
110900010628     C                   SETON                                        77
111000010601     c                   leave
111100010601     c                   endif
111200010601
111300010601     c                   enddo
111400010601
111500010601     c                   endsr
111600010601      *
111700010601     C*------------------------------------------------------------------------
111800010601     C*   SR_DECPO   DECODIFICA I DATI DEL PUNTO OPERATIVO
111900010601     C*------------------------------------------------------------------------
112000010601      *
112100010601     C     SR_DECPO      BEGSR
112200010601      *
112300010601      * Se visita e cliente nuovo recupero la filiale dai primi 3 campi del
112400010601      * codice commerciale
112500010601      *
112600010601     c                   if        *in98 and visksc = 0
112700010601     c                   movel     viscmm        comfil
112800010601     c                   endif
112900010601      * Se visita di un cliente esistente recupero il p.o. appartenenza
113000010601      * cliente
113100010601      *
113200010601     c                   if        *in98 and visksc > 0
113300010601     c                   movel     visksc        comfil
113400010601     c                   endif
113500010601      *
113600010601      * se tariffa recupero dal codice cliente
113700010601      *
113800010601     c                   if        not *in98
113900010601     c                   movel     parsks        comfil
114000010601     c                   endif
114100010601      *
114200010601      * decodifica
114300010601      *
114400010601     c     comfil        chain     azorg01l
114500010601      *
114600010601     C                   if        %found(azorg01l)
114700010601     c                   movel     orgdes        td1fil
114800010601     c                   movel     orgind        td1via
114900010601     c                   movel     orgcpf        td1cap
115000010601     c                   movel     orgloc        td1loc
115100010601     c                   movel     orgpro        td1prv
115200010601     c                   movel     orgtel        td2tel
115300010601     c                   movel     orgfax        td2tfx
115400010601      * verifico se cliente Posta
115500010601     c                   movel     orgde3        og143
115600020530      * cerco il codice regione
115700020530     c                   movel     orgde7        og147
115800020530     c
115900010601      *
116000010601     c                   else
116100010601     c                   clear                   orgcts
116200020531     c                   clear                   og147
116300010601      *
116400010601     c                   endif
116500010601      * costruisco la data stampa
116600010601     c                   movel     i45gds        data_eur
116700010601     c     *eur          movel     data_eur      datastampa
116800060606
116900060606      * imposto nelle varie lingue
117000060606     c                   select
117100060606     c                   when      i45lin = 'I'
117200010601     c                   eval      picdal = %trim(orgdes) + ',li ' + datastampa
117300060606     c                   when      i45lin = '2'
117400060606     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
117500060817     c                   when      i45lin = '3'
117600060817     c                   eval      picdal = %trim(orgdes) + ',le ' + datastampa
117700060817     c                   when      i45lin = '4'
117800060817     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
117900060606     c                   endsl
118000010601      *
118100010601     c                   endsr
118200010601      *
118300010601     C*------------------------------------------------------------------------
118400010531     C*   SR_DECTAM  DECODIFICA DELLA TESTATA TARIFFA
118500010531     C*------------------------------------------------------------------------
118600010531      *
118700010531     C     SR_DECTAM     BEGSR
118800010627      *
118900010627     C                   movel     TAMFLO        DSTA01
119000010627      * se divisa diversa da euro vado a fine PGM
119100010627     c                   if        §tadiv <> 'EUR'
119200010627     c                   goto      fine
119300010627     c                   endif
119400020123      * se tariffa FEDEX  vado a fine PGM
119500020123     c                   if        §tafed =  'S'
119600020123     c                   goto      fine
119700020123     c                   endif
119800010531      *
119900010531      *  Data decorrenza in stampa
120000010531     c     *iso          movel     tamddt        data_EUR
120100010531     C     *eur          movel     DATA_EUR      staddt
120200010531      *  Data scadenza in stampa
120300010531     c     *iso          movel     tamdst        data_EUR
120400010531     C     *eur          movel     DATA_EUR      stadst
120500010531
120600010531      ** verifico se è una tariffa DPD
120700010531      *
120800010531     C                   eval      *in25 = (§tadpd = 'S')
120900010604      ** verifico se è una tariffa EUROEXPRESS
121000010604      *
121100010604     C                   eval      *in02 = (tamfie = 'E')
121200060606
121300060606      * imposto l'intestazione della tariffa
121400060606      * tariffa EEX
121500060606     c                   if        *in02
121600060606     c                   select
121700060606     c                   when      i45lin = 'I'
121800060606     c                   eval      destar = 'TARIFFA EUROEXPRESS'
121900060606     c                   when      i45lin = '2'
122000060630     c                   eval      destar = 'EUROEXPRESS TARIFF'
122100060816     c                   when      i45lin = '3'
122200060816     c                   eval      destar = 'TARIF EUROEXPRESS'
122300060816     c                   when      i45lin = '4'
122400060816     c                   eval      destar = 'PREIS EUROEXPRESS'
122500060606     c                   endsl
122600060606     c                   endif
122700060606      * tariffa DPD
122800060606     c                   if        not *in02 and *in25
122900060606     c                   select
123000060606     c                   when      i45lin = 'I'
123100060606     c                   eval      destar = 'TARIFFA MONOCOLLO D.P.D'
123200060606     c                   when      i45lin = '2'
123300060630     c                   eval      destar = 'DPD SINGLE PARCEL TARIFF'
123400060816     c                   when      i45lin = '3'
123500060816     c                   eval      destar = 'TARIF UN SEUL COLIS D.P.D.'
123600060816     c                   when      i45lin = '4'
123700060816     c                   eval      destar = 'PREIS MONOPACKSTUECK D.P.D.'
123800060606     c                   endsl
123900060606     c                   endif
124000060606
124100010531      * recupero la divisa per decodifica in stampa e per eventuali
124200010531      * conversioni di importi ancora in lire in altri archivi
124300010531      * aggancio la tabella CV
124400010531      *
124500010531     C                   eval      cod= 'CV'
124600010531     C                   movel(p)  §TADIV        KEY
124700060605     c     tabkeyl       chain     tabel00f
124800010531      *
124900010531     c                   if        %found (tabel00f)
125000010531     C                   movel     TBLUNI        DSCV
125100010531     C                   movel     §CVDES        DESDIV
125200010531     c                   endif
125300010601      *
125400010601      * carico i codici tassazione del cliente dal cappario versione data
125500010601      * decorrenza tariffa in caso di stampa dei soli codici tassazione cliente
125600010601      *
125700010601     c                   z-add     tamddt        i96dri
125800010601     c                   call      'TISI96R'
125900010601     c                   parm                    i96dri
126000010601     c                   parm                    o96ver
126100010601      * aggancio tabella codici tassazione con quella del p.o. del cliente
126200010601      *
126300010601     c                   eval      cod = 'CT'
126400010601     c                   if        orgcts <> '  '
126500010601     c                   movel(p)  orgcts        key
126600010601     c                   else
126700010601     c                   movel(p)  *all'9'       key
126800010601     c                   endif
126900010601      *
127000060704     c     tabkeyl       chain     tabel00f
127100010601      *
127200010601     c                   if        %found(tabel00f)
127300010601     c                   movel     tbluni        dsct
127400010604      * recupero dal cappario i codici tassazione
127500010604     c                   z-add     1             y
127600010604     c     kcpc          setll     azcpc03l
127700010604      *
127800010604     c                   do        *hival
127900010604     c     kcpc          reade     azcpc03l
128000010604     c                   if        %eof(azcpc03l)
128100010604     c                   leave
128200010604     c                   endif
128300010604      *
128400010604     c                   if        cpcatb = ' '
128500010604     c     cpccts        lookup    ctl                                    31
128600010604      * carico
128700010604     c                   if        not *in31
128800010604     c                   movel     cpccts        ctl(y)
128900010604     c                   add       1             y
129000010604     c                   endif
129100010604      *
129200010604     c                   endif
129300010604      *
129400010604     c                   enddo
129500010604      *
129600010604     c                   endif
129700010604      *
129800010604      * decodifico la tariffa cliente
129900010604      *
130000010604     c                   movel     i45ctr        unoctr
130100010604     c                   movel(p)  unoctr        key
130200010604     c                   eval      cod = 'TR'
130300010604      *
130400060605     c     tabkeyl       chain     tabel00f
130500010604      *
130600010604     c                   if        %found(tabel00f)
130700010604     c                   movel     tbluni        dstr
130800010604      * valorizzo i campi descrittivi di stampa
130900011010     c                   movel     §trdst        desctr
131000010608     c                   movel     §trde1        desum
131100010604     c                   movel     §trde3        ds3um
131200010928      * se a quintale o a kg metto in  desum KG. e in desctr A PESO
131300010626     c                   if        unoctr = 0 or unoctr = 3
131400010626     c                   eval      desum = '       KG.'
131500010626     c                   endif
131600010604      * salvo il minimo tassabile
131700010604     c                   eval      ctrata  = §trata
131800010604      *
131900010604     c                   endif
132000010801      * tariffa a valore (per stampare scaglioni senza decimali)
132100010604     c                   eval      *in87 = (unoctr = 4)
132200010723      * valorizzo il campo note
132300010723     c                   eval      p07des = tamdcv
132400010928      * se il campo note è uguale a blank verifico se tariffa preferenziale
132500010928      * partenza o arrivo
132600011009     c                   if        p07des = *blanks and tambap =  'P'
132700060606      * in lingua
132800060606     c                   select
132900060606     c                   when      i45lin = 'I'
133000010928     c                   eval      p07des = 'Valida solo per traffico in +
133100010928     c                                       Partenza'
133200060606     c                   when      i45lin = '2'
133300060817     c                   eval      p07des = 'Valid only for Departing +
133400060817     c                                       traffic'
133500060817     c                   when      i45lin = '3'
133600060817     c                   eval      p07des = 'Valable uniquement pour le trafic +
133700060817     c                                       au départ'
133800060817     c                   when      i45lin = '4'
133900060817     c                   eval      p07des = 'Gilt nur für Abgehende Sendungen'
134000060606     c                   endsl
134100010928     c                   endif
134200010928      *
134300011009     c                   if        p07des = *blanks and tambap =  'A'
134400060606      * in lingua
134500060606     c                   select
134600060606     c                   when      i45lin = 'I'
134700010928     c                   eval      p07des = 'Valida solo per traffico in +
134800010928     c                                       Arrivo'
134900060606     c                   when      i45lin = '2'
135000060630     c                   eval      p07des = 'Valid only for Arriving traf+
135100060630     c                                       fic'
135200060817     c                   when      i45lin = '3'
135300060817     c                   eval      p07des = 'Valable uniquement pour le trafic +
135400060817     c                                       à l''arrivée'
135500060817     c                   when      i45lin = '4'
135600060817     c                   eval      p07des = 'Gilt nur für Eingehende Sendungen'
135700060606     c                   endsl
135800010928     c                   endif
135900040430      * TESTO PARTICOLARITÀ GIACENZA
136000040430      * se codice = GP metto 5 nel numero di avvisi prima di rendere la merce
136100040430      * altrimenti lascio 2
136200040430     c                   eval      comnav = 2
136300040430     c                   if        tamgga = 'GP'
136400040430     c                   eval      comnav = 5
136500040430     c                   endif
136600060922      * devo impostare anche la parte finale del numero nelle varie lingue
136700060922     c                   exsr      sr_numgp
136800010928      *
136900010725      * recupero in base alla data decorrenza la versione di prestampato condi-
137000010725      * zioni generali dalla tabella TAA
137100111103     c                   eval      kcod = 'TAA'
137200010725     c     kcod          setll     tntbe01l
137300010725     c                   do        *hival
137400010725     c     kcod          reade     tntbe01l
137500010725      * se fine file vado a fine
137600010725     c                   if        %eof(tntbe01l)
137700010725     c                   leave
137800010725     c                   endif
137900010725      * se annullato leggo il successivo
138000010725     c                   if        tbeatb <> ' '
138100010725     c                   iter
138200010725     c                   endif
138300010725      * mi salvo il modello e l'anno
138400010725     c                   movel     tbeuni        dtaa
138500010725      * mi salvo sempre il primo che ho letto
138600010725     c                   if        stpmod = 0
138700010725     c                   movel     tbeke1        stpmod
138800010725     c                   movel     §dccdad       stpanno
138900010725     c                   endif
139000060302      *?ATTENZIONE  ?
139100060302      * dal 01/03/06 i controlli vanno fatti con la data di stampa e non +
139200060302      * con la data decorrenza tariffa
139300060302      * verifico la data di stampa
139400060302      * se data di stampa maggiore della data di validità del modello leggo
139500060302     c                   if        i45ads >  §dccdad
139600010725     c                   movel     tbeke1        stpmod
139700010725     c                   movel     §dccdad       stpanno
139800010725     c                   iter
139900010725     c                   endif
140000060302      * se data di stampa = salvo a vado a fine
140100060302     c                   if        i45ads =  §dccdad
140200010725     c                   movel     tbeke1        stpmod
140300010725     c                   movel     §dccdad       stpanno
140400010725     c                   leave
140500010725     c                   endif
140600060302      * se data di stampa <  della data validita' vado a fine
140700060302     c                   if        i45ads <  §dccdad
140800010725     c                   leave
140900010725     c                   endif
141000010725      *
141100010725     c                   enddo
141200010604
141300161221      *
141400161221      * recupero in base alla data decorrenza la versione di prestampato delle
141500161221      * informativa privacy dalla tabella ATP
141600161221     c                   eval      kcod = 'ATP'
141700161221     c     kcod          setll     tntbe01l
141800161221     c                   do        *hival
141900161221     c     kcod          reade     tntbe01l
142000161221      * se fine file vado a fine
142100161221     c                   if        %eof(tntbe01l)
142200161221     c                   leave
142300161221     c                   endif
142400161221      * se annullato leggo il successivo
142500161221     c                   if        tbeatb <> ' '
142600161221     c                   iter
142700161221     c                   endif
142800161221      * mi salvo il modello e l'anno
142900161221     c                   movel     tbeuni        datp
143000161221      * mi salvo sempre il primo che ho letto
143100161221     c                   if        stpmodi = 0
143200161221     c                   movel     tbeke1        stpmodi
143300161221     c                   movel     §atpdad       stpannoi
143400161221     c                   endif
143500161221      * verifico la data di stampa
143600161221      * se data di stampa maggiore della data di validità del modello leggo
143700161221     c                   if        i45ads >  §atpdad
143800161221     c                   movel     tbeke1        stpmodi
143900161221     c                   movel     §atpdad       stpannoi
144000161221     c                   iter
144100161221     c                   endif
144200161221      * se data di stampa = salvo a vado a fine
144300161221     c                   if        i45ads =  §atpdad
144400161221     c                   movel     tbeke1        stpmodi
144500161221     c                   movel     §atpdad       stpannoi
144600161221     c                   leave
144700161221     c                   endif
144800161221      * se data di stampa <  della data validita' vado a fine
144900161221     c                   if        i45ads <  §atpdad
145000161221     c                   leave
145100161221     c                   endif
145200161221      *
145300161221     c                   enddo
145400010531     C                   ENDSR
145500020205
145600020205      *------------------------------------------------------------------------
145700020205      *   Sr_Cartarp  CARICO LE TARIFFE PARTICOLARI
145800020205      *------------------------------------------------------------------------
145900020205     C     Sr_Cartarp    begsr
146000020205
146100020205      * carico solo quelle che interessano la tariffa che sto stampando
146200020205     c                   eval      cod ='1P'
146300020205     c                   z-add     0             K
146400060606     c     tabk2l        setll     tabel00f
146500020205 b1  c                   do        *hival
146600060606     c     tabk2l        reade     tabel00f
146700020205      * Fine file
146800020205     c                   if        %eof(tabel00f)
146900020205     c                   leave
147000020205     c                   endif
147100020205      * escludo le annullate
147200020205     c                   if        tblflg <> ' '
147300020205     c                   iter
147400020205     c                   endif
147500080609      * se tariffa particolare c= lasciato avviso
147600080609      * per le filiali non abilitate, accetto cartello
147700080609      * non stampo
147800080609     c                   If        tblkey = 'c       '
147900051012     c                             and comfil <> 888
148000080609     c                   if        %lookup(comfil:sklav) = *zeros
148100080609     c                             and %lookup(999:sklav) = *zeros
148200051012     c                   iter
148300080609     c                   endif
148400051012     c                   endif
148500020205
148600020205     c                   movel     tbluni        ds1p
148700020205
148800020205      * Tariffa con servizio Poste escludo le tariffe particolari con
148900020205      * flag §1PFPT = 'N'
149000020205     c                   if        tamtsp = 'P' and §1pfpt = 'N'
149100020205     c                   iter
149200020205     c                   endif
149300020205      * Tariffa con Network DPD escludo le tariffe particolare con
149400020205      * flag §1PFDP = 'N'
149500020205     c                   if        §tadpd = 'S' and §1pfdp = 'N'
149600020205     c                   iter
149700020205     c                   endif
149800020205      * Per Tariffa FedEx non faccio controlli xchè non vengono lette
149900020205
150000020205      * Tariffa Italia escludo le tariffe particolare con
150100020205      * flag §1PTCO = 'N' e non deve essere servizio poste
150200020205     c                   if        tamfie = 'I' and §1ptco = 'N'
150300020205     c                             and tamtsp <> 'P'
150400020205     c                   iter
150500020205     c                   endif
150600020205      * Tariffa EuroExpress escludo le tariffe particolare con
150700020205      * flag §1PFIE = 'N'
150800020205     c                   if        tamfie = 'E' and §1pfie = 'N'
150900020205     c                   iter
151000020205     c                   endif
151100020614      * Per tariffe di cartello verifico che non sia tipo servizio "P"
151200020614     C                   if        tamfie = ' ' and §1ptco = 'N'
151300020614     c                             and tamtsp <> 'P'
151400020614     c                   iter
151500020614     c                   endif
151600020205
151700020205     c                   add       1             k
151800020205     c                   movel     tblkey        ftc(k)
151900020205     c                   movel     tbluni        dtc(k)
152000020205      * carico l'ordinamento in stampa
152100020205     c                   movel     §1post        ost(k)
152200020205      * carico il tipo stampa standard oppure no
152300020205     c                   movel     §1ptst        tst(k)
152400020205
152500020205 e1  c                   enddo
152600020205      * carico la schiera delle estensioni consegne particolari
152700020205     C                   eval      cod ='SP'
152800020205     C                   z-add     0             K
152900060606     c     tabk2l        setll     tabel00f
153000020205
153100020205 b1  C                   DO        *hival
153200020205      *
153300060606     c     tabk2l        reade     tabel00f
153400020205      *
153500020205     c                   if        %eof(tabel00f)
153600020205     c                   leave
153700020205     c                   endif
153800020205      *
153900020205 b2  C                   IF        tblflg = ' '
154000020205     C                   movel     TBLUNI        DSSP
154100020205      *
154200020205     c                   movel     TBLKEY        comftc
154300020205      *
154400020205      * carico tutte le consegne particolari
154500020205     C                   z-add     1             K
154600020205     c                   setoff                                       69
154700020205     C     comftc        lookup    FTC(K)                                 69
154800020205     c                   if        *in69
154900020205     C                   movel     §spdes        EDTC(K)
155000020205     C                   movel     §spdac        EDAC(K)
155100020205     C                   movel     §spsud        ESUD(K)
155200020205     c                   endif
155300020205      *
155400020205 e2  C                   endif
155500020205      *
155600020205 e1  C                   ENDDO
155700020205      * ordino le tariffe particolari in base all'ordine in stampa
155800020205      *
155900020205     c                   movel     OST           WOS
156000020205     c                   sorta     wos
156100020205 b1  c                   do        50            j
156200020205     c                   z-add     1             jj
156300020205     c                   setoff                                       69
156400020205     c                   if        wos(j) <> '99' and wos(j) <> *blank
156500020205     c     wos(j)        lookup    ost(jj)                                69
156600020205      * carico la schiera delle tariffe particolare in ordine di stampa
156700020205     c                   if        *in69 = *on
156800020205     c                   add       1             jjj
156900020205     c                   move      ftc(jj)       cp(jjj)
157000020205     c                   endif
157100020205     c                   endif
157200020205 e1  c                   enddo
157300020205
157400020205     c                   endsr
157500020205
157600010604     C*------------------------------------------------------------------------
157700010604     C*   SR_SCAGLIONI PREPARAZIONE SCHIERA SCAGLIONI
157800010604     C*------------------------------------------------------------------------
157900010604      *
158000010604     C     SR_SCAGLIONI  BEGSR
158100010604      *
158200010605      * CARICO SCAGLIONI
158300010604      *
158400010604     c                   setoff                                       99
158500010622     c                   z-add     0             k
158600020531     c                   eval      w_oklnp = *off
158700010605      * cerco prima con la linea di partenza del cliente
158800010605      *
158900061122     C  n98DS1KTD        SETLL     TITAD04l
159000061122     c   98ds1ktd        setll     tiofd01l
159100010605      * se non trovato con la linea di partenza del cliente lo segnalo
159200061122     c                   if        not %equal
159300010605     c                   eval      lnpcli = 'NO'
159400010605      * mi riposiziono con chiave generica
159500010604      *
159600061122     C  n98DS1KTM        SETLL     TITAD04l
159700061122     c   98ds1ktm        setll     tiofd01l
159800010605      * se non trovato nulla errore manca dettaglio tariffario
159900010604      *
160000061122     c                   if        not %equal
160100010605     c                   seton                                        99
160200010605     c                   move      msg(4)        errmsg
160300010605     c                   exsr      staerr
160400010605     c                   goto      endscaglia
160500010605     c                   endif
160600010605      *
160700010605     c                   endif
160800010605      *
160900010604     C                   DO        *HIVAL
161000010604      *
161100061122     C  n98DS1KTM        READE     TITAD04l
161200061122     c   98ds1ktm        reade     tiofd01l
161300010604      *
161400061122     C                   if        %eof
161500010606      * se fine lettura e non ho caricato nessuno scaglione (K = 0) e
161600010621      * ho letto solo dettaglio con cap valorizzato (cap1scaglio = 'S')
161700010606      * mi riposiziono con l'ultima chiave per caricare almeno gli scaglioni
161800010621     c                   if        k = 0 and cap1scaglio = 'S'
161900061122     c  n98keytad        setll     titad04l
162000061122     c   98keytad        setll     tiofd01l
162100010606      * valorizzo il flag del non controllo del cap perchè altrimenti non riesco
162200010606      * a caricare gli scaglioni
162300010606     c                   eval      ctrcap = 'N'
162400010606     c                   iter
162500010606      * altrimenti vado a fine ciclo lettura
162600010606     c                   else
162700010604     c                   leave
162800010604     c                   endif
162900010606      *
163000010606     c                   endif
163100010604      * se annullato rileggo
163200010604     c                   if        tadatb = 'A'
163300010604     c                   iter
163400010604     c                   endif
163500020531     c                   eval      w_okstp = *off
163600010604      * se I45DET = 'L' devo stampare solo le righe di dettaglio che hanno la
163700010604      *                 LNP uguale alla LNP del codice cliente
163800020531     c                   if        I45DET = 'L'
163900020531      * se ho già stampato una linea di partenza non stampo più niente
164000020531     c                   if        w_oklnp = *on and rotlnp <> 0 and
164100020531     c                             rotlnp <> tadlnp
164200020531     c                   iter
164300020531     c                   endif
164400020531     c                   exsr      sr_chklnp
164500020531     c                   if        w_okstp = *off
164600010604     c                   iter
164700010604     c                   endif
164800020531     c                   eval      w_oklnp = *on
164900020531     c                   endif
165000010604      * se I45DET = 'A' devo stampare solo le righe di dettaglio che hanno la
165100010604      *                 LNP uguale alla LNP del codice cliente o il codice
165200010604      *                 tassazione tra quelli pevisti dalla linea del codice
165300010604      *                 cliente
165400010604     c                   if        I45DET = 'A'
165500020531      * se ho già stampato una linea di partenza non stampo più niente
165600020531     c                   if        w_oklnp = *on and rotlnp <> 0 and
165700020531     c                             rotlnp <> tadlnp
165800020531     c                   iter
165900020531     c                   endif
166000020531     c                   exsr      sr_chklnp
166100010604     c     tadcts        lookup    ctl                                    35
166200020531     c                   if        not *in35 and w_okstp = *off
166300010604     c                   iter
166400010604     c                   endif
166500020531     c                   eval      w_oklnp = *on
166600010604      *
166700010604     c                   endif
166800010604      *
166900010604      * salvataggio dei campi per le rotture regione/provincia
167000010604      *
167100010605     c                   if        rotlnp = *zeros
167200010606      * se controllo del cap lo devo fare (ctrcap = ' ')
167300010606      * se cap valorizzato nel 1° scaglione lo segnalo per caricare schiera
167400010606      * inoltro quando non c'è cap  e ritorno a leggere
167500010621     c                   if        tadcap <>  *blanks  and ctrcap = ' '
167600010606     c                   eval      cap1scaglio = 'S'
167700010606     c                   iter
167800010606     c                   endif
167900010604     c                   eval      rotlnp = tadlnp
168000010604     c                   eval      rotcts = tadcts
168100010604     c                   eval      rotcap = tadcap
168200010604      *
168300010604      * controlli di rottura per regione e pulizie schiere
168400010605      *
168500010605     c                   exsr      regdet
168600010605     c   99              goto      endscaglia
168700010605     c                   exsr      prodet
168800010605     c   99              goto      endscaglia
168900010605      *
169000010605     c                   endif
169100010605      *
169200010605      * carico gli scaglioni per lnp cts e cap uguali
169300010605      *
169400010605     c                   if        tadlnp = rotlnp and tadcts = rotcts and
169500010605     c                             tadcap = rotcap
169600010605     c                   add       1             k
169700010605      *
169800010605     c                   if        k > 18
169900050914      * accendo l'indicatore di errore così esce dal pgm
170000050914     c                   seton                                        99
170100010605     c                   move      msg(5)        errmsg
170200010605     c                   exsr      staerr
170300050930     c                   goto      endscaglia
170400010605     c                   endif
170500010605      *
170600010605     c                   z-add     tadsgl        sgl(k)
170700010606      *
170800010801      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
170900010801      * a valore
171000010801     c                   if        not *in87
171100010801      *
171200010606     c                   move      tadsgl        w0030
171300010606     c                   z-add     tadsgl        w0050
171400010606      * se scaglione 99999 non controllo i decimali
171500010606     c                   if        w0050 <> 99999 and w0030 > 0
171600010606     c                   seton                                        91
171700010606     c                   endif
171800010606      * se con decimali verifico se valorizzato dal secondo in poi
171900010606     c                   if        *in91
172000010606     c                   z-add     w0030         w0020
172100010606     c                   if        w0020 > 0
172200010606     c                   seton                                        37
172300010606     c                   endif
172400010606     c                   endif
172500010801      *
172600010801     c                   endif
172700010606      * se non è valorizzato il CAP carico la schiera dell'inoltro
172800010606     c                   if        tadcap = *blanks
172900010606     c                   z-add     tadino        pro(k)
173000010606      * pulisco il flag che mi indica che sto caricando la schiera degli inoltri
173100010606     c                   clear                   cap1scaglio
173200010606     c                   endif
173300010606      * valorizzo la schiera del rapporto peso volume
173400010622     c     TADRPV        MULT      100           rpv(k)
173500010606      *
173600010606     c                   else
173700010606      * rottura di chiave
173800010606     c                   leave
173900010606      *
174000010606     c                   endif
174100010606      *
174200010606     c                   enddo
174300010606      *
174400010606      * se non ho caricato nessuno scaglione do errore di mancanza di dettaglio
174500010606      *
174600010606     c                   if        K = 0
174700010606     c                   seton                                        99
174800010606     c                   move      msg(4)        errmsg
174900010606     c                   exsr      staerr
175000010606     c                   goto      endscaglia
175100010606     c                   endif
175200051006      * memorizzo quanti scaglioni ho trovato
175300051006     c                   Eval      wnrscaglio = k
175400080117      * prima di stampare controllo se gli scaglioni sono stampabili
175500080117      * in stampa il campo è lungo 4
175600080117     c                   eval      xy = 1
175700080117     c                   for       xy by 1 to wnrscaglio
175800080117     c                   z-add     sgl(xy)       w0050
175900080117      * scaglioni senza decimali al massimo 9999
176000080117     c                   if        not *in91 and w0050 > 9999
176100080118     c                             and w0050 <> 99999
176200080117     c                   eval      *in99 = *on
176300080117     c                   eval      errmsg = 'Scaglione ' +
176400080117     c                             %editc(sgl(xy):'4') +
176500080117     c                             ' non stampabile'
176600080117     c                   exsr      staerr
176700080117     c                   leavesr
176800080117     c                   endif
176900080218      * scaglioni con 1 decimale al massimo 999,9
177000080218     c                   if        *in91 and not *in37 and w0050 > 999,9
177100080118     c                             and w0050 <> 99999
177200080117     c                   eval      *in99 = *on
177300080117     c                   eval      errmsg = 'Scaglione ' +
177400080117     c                             %editc(sgl(xy):'4') +
177500080117     c                             ' non stampabile'
177600080117     c                   exsr      staerr
177700080117     c                   leavesr
177800080117     c                   endif
177900080218      * scaglioni con 2 decimale al massimo 99,99
178000080218     c                   if        *in91 and *in37 and w0050 > 99,99
178100080118     c                             and w0050 <> 99999
178200080117     c                   eval      *in99 = *on
178300080117     c                   eval      errmsg = 'Scaglione ' +
178400080117     c                             %editc(sgl(xy):'4') +
178500080117     c                             ' non stampabile'
178600080117     c                   exsr      staerr
178700080117     c                   leavesr
178800080117     c                   endif
178900080117     c                   endfor
179000010606      *
179100010606      * preparo la schiera scaglioni da stampare
179200010606      *
179300010606     c                   exsr      sr_stascaglia
179400010606      *
179500010606     c     endscaglia    endsr
179600010606      *
179700010606     C*-------------------------------------------------------------------------
179800010606     C*  SR_STASCAGLIA PREPARO LA SCHIERA DEGLI SCAGLIONI DA STAMPARE
179900010606     C*-------------------------------------------------------------------------
180000010606     C     SR_STASCAGLIA BEGSR
180100010606      *
180200010606     c                   z-add     1             k
180300010606     c                   clear                   k1
180400010606      *
180500010606     c                   dow       sgl(k) > 0
180600010606      * preparo lo scaglione DAL addizionato di 1 o 0,1 o 0,01 in base ai decima
180700010606      * li
180800010606     c                   z-add     sgl(k)        w0050
180900010606      *
181000010606      * verifico se scaglione 99999
181100010606     c                   if        w0050 <> 99999
181200010606      *
181300010606     c                   if        k1 > 0
181400010606      * se non ha decimali
181500010606     c                   if        not *in91
181600010622     C                   eval      sgld(k) = sgl(k1) + 1
181700010606     c                   endif
181800010606      *
181900010606     c                   if        *in91 and not *in37
182000010606      * se ha 1 decimale
182100010622     C                   eval      sg1d(k) = sgl(k1) + 0,1
182200010606     c                   endif
182300010606      *
182400010606     c                   if        *in91 and *in37
182500010606      * se ha + di 1 decimale
182600010622     C                   eval      sg2d(k) = sgl(k1) + 0,01
182700010606     c                   endif
182800010606      *
182900010606     c                   endif
183000010606      *
183100010606      * costruisco la schiera dell'applicazione tariffa finita che va sotto gli
183200010606      * scaglioni da/a in base al valore della tariffa finita che è in testata
183300010606      * tariffa e in mancanza di quest'ultima prendo dalla tabella TR
183400010606      *
183500010606      * muovo unità di misura della tariffa nella schiera
183600060922      * devo fare tutto fisso a pgm perchè in tabella il campo è lungo 10
183700060922      * mentre in stampa è lungo 7
183800060922      * quindi devo personalizzarlo per ogni lingua
183900060925     c                   move      unoctr        wctr1
184000060922     c                   exsr      sr_atfk
184100010606      * faccio i vari ragionamenti in base al valore della tariffa finita
184200010606      *
184300010606      * se scaglioni senza decimali
184400011218     c                   if        not *in91
184500010606
184600011218     c                   z-add     tamata        wata
184700010606      * se in tariffa non è definita la recupero dalla tabella TR se maggiore 1
184800011218     c                   if        wata = 0 and ctrata > 1
184900011218     c                   z-add     ctrata        wata
185000011218     c                   endif
185100011218      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
185200011218      * scrivo "A SPED."
185300011218     c                   if        (wata > 0 and sgl(k) <= wata)
185400060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
185500060925     c                   clear                   wctr1
185600060922     c                   exsr      sr_atfk
185700011218     c                   endif
185800010606      *
185900011218     c                   else
186000010606      * scaglioni con decimali
186100011218     c                   z-add     tamata        wata1
186200011218     c                   z-add     tamata        wata2
186300010606      *
186400010606      * se in tariffa non è definita la recupero dalla tabella TR se maggiore 1
186500011218     c                   if        wata1 = 0 and ctrata > 1 and not *in37
186600011218     c                   z-add     ctrata        wata1
186700011218     c                   endif
186800010606      * se in tariffa non è definita la recupero dalla tabella TR se maggiore 1
186900011218     c                   if        wata2 = 0 and ctrata > 1 and *in37
187000011218     c                   z-add     ctrata        wata2
187100011218     c                   endif
187200010606      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
187300010606      * scrivo "A SPED."  1 decimale
187400011218     c                   if        (wata1 > 0 and sgl(k)<= wata1 and not *in37)
187500060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
187600060925     c                   clear                   wctr1
187700060922     c                   exsr      sr_atfk
187800011218     c                   endif
187900010606      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
188000010606      * scrivo "A SPED."  2 decimali
188100011218     c                   if        (wata2 > 0 and sgl(k) <= wata2 and *in37)
188200060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
188300060925     c                   clear                   wctr1
188400060922     c                   exsr      sr_atfk
188500011218     c                   endif
188600010606      *                                                      *in91 on
188700011218     c                   endif
188800011123
188900010606      *
189000010606     c                   move      '/'           sglb(k)
189100010606      * preparo lo scaglione AL
189200010606      *
189300010606      * no decimali
189400010606     c                   if        not *in91
189500010606     c                   z-add     sgl(k)        sgla(k)
189600010606     c                   endif
189700010606      * 1 decimale
189800010606     c                   if        *in91 and not *in37
189900010606     c                   z-add     sgl(k)        sg1a(k)
190000010606     c                   endif
190100010606      * 2 decimali
190200010606     c                   if        *in91 and *in37
190300010606     c                   z-add     sgl(k)        sg2a(k)
190400010606     c                   endif
190500010606      *
190600010606     c                   else
190700010606      * scaglione 99999 è sempre maggiore della tariffa finita
190800060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
190900060922      * la decodifica del tipo tariffa
191000060925     c                   move      unoctr        wctr1
191100060922     c                   exsr      sr_atfk
191200010606      *
191300010606     c                   endif
191400010606      *
191500010606     c                   add       1             k
191600010606     c                   add       1             k1
191700010606      *
191800010606      * se maggiore di 18 esco
191900010606     c                   if        k > 18
192000010606     c                   leave
192100010606     c                   end
192200010606      *
192300010606     c                   enddo
192400010606      *
192500010606      * se c'è solo uno scaglione non scrivo niente sotto
192600021220      * solo se lo scaglione è 99999
192700010606      *
192800021220     c                   if        k1 <= 1 and sgl(k1) >= 99999
192900010606     c                   clear                   atf(1)
193000010606     c                   endif
193100010606      *
193200010606     C                   ENDSR
193300010606     C*-------------------------------------------------------------------------
193400010606     C*  SR_STINO   CONTROLLO SE DEVO STAMPARE INOLTRO A FINE DETTAGLIO
193500010606     C*-------------------------------------------------------------------------
193600010606     C     SR_STINO      BEGSR
193700020531
193800020531     c                   eval      w_oklnp = *off
193900010606      *
194000010606      * leggo tutto il titad e verifico per ogni scaglione valido se
194100010606      * se l'inoltro è uguale
194200010606      *
194300061122     c  n98ds1ktm        setll     titad04l
194400061122     c   98ds1ktm        setll     tiofd01l
194500010606      *
194600010606     c                   do        *hival
194700061122     c  n98ds1ktm        reade     titad04l
194800061122     c   98ds1ktm        reade     tiofd01l
194900010606      *
195000061122     c                   if        %eof
195100010606     c                   leave
195200010606     c                   endif
195300010606      * se annullato rileggo
195400010606     c                   if        tadatb = 'A'
195500010606     c                   iter
195600010606     c                   endif
195700020531     c                   eval      w_okstp = *off
195800010606      * se I45DET = 'L' devo leggere  solo le righe di dettaglio che hanno la
195900010606      *                 LNP uguale alla LNP del codice cliente
196000020531     c                   if        I45DET = 'L'
196100020531      * se ho già stampato una linea di partenza non stampo più niente
196200020531     c                   if        w_oklnp = *on and rotlnp <> 0 and
196300020531     c                             rotlnp <> tadlnp
196400020531     c                   iter
196500020531     c                   endif
196600020531     c                   exsr      sr_chklnp
196700020531     c                   if        w_okstp = *off
196800010606     c                   iter
196900010606     c                   endif
197000020531     c                   eval      w_oklnp = *on
197100020531     c                   endif
197200010606      * se I45DET = 'A' devo leggere  solo le righe di dettaglio che hanno la
197300010606      *                 LNP uguale alla LNP del codice cliente o il codice
197400010606      *                 tassazione tra quelli pevisti dalla linea del codice
197500010606      *                 cliente
197600010606     c                   if        I45DET = 'A'
197700020531      * se ho già stampato una linea di partenza non stampo più niente
197800020531     c                   if        w_oklnp = *on and rotlnp <> 0 and
197900020531     c                             rotlnp <> tadlnp
198000020531     c                   iter
198100020531     c                   endif
198200020531     c                   exsr      sr_chklnp
198300010606     c     tadcts        lookup    ctl                                    35
198400020531     c                   if        not *in35 and w_okstp = *off
198500010606     c                   iter
198600010606     c                   endif
198700020531     c                   eval      w_oklnp = *on
198800010606      *
198900010606     c                   endif
199000010606      * decodifico la linea di partenza se non ancora fatto
199100010606      *
199200010606     c                   if        stalnp = *blanks
199300020531     c     tadlnp        chain     azorg01l
199400010606      *
199500020531     c                   if        %found(azorg01l)
199600020531     c                   movel     orgdes        stalnp
199700020624      * controllo se linea di partenza 9xx
199800020624     c                   if        orgfl2 = 'S'
199900020624      * italia
200000020624     c                   if        tadlnp = 950
200100060606      * in lingua
200200060606     c                   select
200300060606     c                   when      i45lin = 'I'
200400070312     c                   eval      prtlnp =
200500070313     c                             'Merce ritirata da tutte le ns. filiali'
200600020624     c                             + '  ' + stalnp
200700060606     c                   when      i45lin = '2'
200800060630     c                   eval      prtlnp = 'Goods collected from all our +
200900070313     c                              branches' + '  ' + stalnp
201000060817     c                   when      i45lin = '3'
201100070314     c                   eval      prtlnp = 'Marchandise retirée par toutes +
201200070314     c                              nos filiales' + '  ' + stalnp
201300060817     c                   when      i45lin = '4'
201400070313     c                   eval      prtlnp = 'Abholung in allen unseren Filialen'
201500060817     c                             + '  ' + stalnp
201600060606     c                   endsl
201700020624     c                   else
201800020624      * regione
201900060606      * in lingua
202000060606     c                   select
202100060606     c                   when      i45lin = 'I'
202200070312     c                   eval      prtlnp = 'Merce ritirata dalle ns. filiali +
202300070312     c                              della regione' + '  ' + stalnp
202400060606     c                   when      i45lin = '2'
202500070314     c                   eval      prtlnp = 'Goods collected from our branches +
202600060630     c                              in the region' + '  ' + stalnp
202700060817     c                   when      i45lin = '3'
202800070314     c                   eval      prtlnp = 'Marchandise retirée par nos +
202900070314     c                              filiales de la région' + '  ' + stalnp
203000060817     c                   when      i45lin = '4'
203100070314     c                   eval      prtlnp = 'Abholung in unseren Filialen der +
203200060817     c                              Region' + '  ' + stalnp
203300060606     c                   endsl
203400020624     c                   endif
203500020624     c                   else
203600020624      * lnp non è 9xx
203700060606      * in lingua
203800060606     c                   select
203900060606     c                   when      i45lin = 'I'
204000070312     c                   eval      prtlnp =
204100070312     c                             'Merce ritirata dalla ns. filiale di'
204200070312     c                             + '  ' + stalnp
204300060606     c                   when      i45lin = '2'
204400070314     c                   eval      prtlnp = 'Goods collected from our branch +
204500060630     c                              in' + '  ' + stalnp
204600060817     c                   when      i45lin = '3'
204700060817     c                   eval      prtlnp = 'Marchandise retirée par notre +
204800070314     c                              filiale de' + '  ' + stalnp
204900060817     c                   when      i45lin = '4'
205000070314     c                   eval      prtlnp = 'Abholung in unseren Filialen in'
205100060817     c                             + '  ' + stalnp
205200060606     c                   endsl
205300020624     c                   endif
205400020624
205500020531     c                   endif
205600010613      * salvo i campi per eventuali rotture utili in fulcro
205700010613     c                   eval      rotlnp = tadlnp
205800010613     c                   eval      rotcts = tadcts
205900010613     c                   eval      rotcap = tadcap
206000010613     c                   eval      rotpro = %subst(tadorp:5:1)
206100010613     c                   eval      rotreg = %subst(tadorp:3:2)
206200010618      * Salvo gli stessi campi di lavoro per letture in fulcro
206300010618     c                   eval      wrklnp = rotlnp
206400010618     c                   eval      wrkslp = stalnp
206500020703     c                   eval      wrkprtlnp = prtlnp
206600010618     c                   eval      wrkcts = rotcts
206700010618     c                   eval      wrkcap = rotcap
206800010618     c                   eval      wrkpro = rotpro
206900010618     c                   eval      wrkreg = rotreg
207000020206      * rotture per regione e provincia
207100020214     c                   exsr      regdet
207200020206      * per 99 errori vado a fine
207300020214     c   99              leave
207400020214     c                   exsr      prodet
207500020206      * per 99 errori vado a fine
207600020214     c   99              leave
207700020206      *
207800010606     c                   endif
207900010606      *
208000010606      * se cap valorizzato leggo il record successivo
208100010621     c                   if        tadcap <> *blanks
208200010606     c                   iter
208300010606     c                   endif
208400010606      * verifico se scaglione presente
208500010606      *
208600010606     c                   z-add     1             z
208700010613     c                   z-add     tadsgl        wsgl72
208800010613     c     Wsgl72        lookup    sgl(z)                                 35
208900010606     c                   if        not *in35
209000010606     c                   iter
209100010606     c                   endif
209200010606      * controllo l'inoltro nella schiera
209300010606      *
209400010606     c                   if        tadino <> pro(z)
209500010606     c                   eval      stino = 'Y'
209600010606     c                   leave
209700010606     c                   endif
209800010606      *
209900010606     c                   enddo
210000010606      *
210100010606     c                   endsr
210200010606     C*-------------------------------------------------------------------------
210300010607     C*  SR_ELADET  ELABORO E STAMPO IL DETTAGLIO TARIFFARIO
210400010606     C*-------------------------------------------------------------------------
210500010607     C     SR_ELADET     BEGSR
210600010606      *
210700010607      * elaboro 6 scaglioni alla volta
210800010607     C                   Z-ADD     1             K4
210900010607     C                   DOW       K4 <= 18
211000010607     C                   MOVEA     SGLD(K4)      SSLD
211100010607     C                   MOVEA     SGLB(K4)      SSLB
211200010607     C                   MOVEA     SGLA(K4)      SSLA
211300010607      * 1 DECIMALE
211400010607     C                   IF        *IN91 AND NOT *IN37
211500010607     C                   MOVEA     SG1A(K4)      SS1A
211600010607     C                   MOVEA     SG1D(K4)      SS1D
211700010607     C                   ENDIF
211800010607      * 2 DECIMALI
211900010607     C                   IF        *IN91 AND *IN37
212000010607     C                   MOVEA     SG2A(K4)      SS2A
212100010607     C                   MOVEA     SG2D(K4)      SS2D
212200010607     C                   ENDIF
212300010607      *
212400010607      * CARICO GLI SCAGLIONI PER ELABORARE IL DETTAGLIO
212500010607      *
212600010607     C                   MOVEA     SGL(K4)       SSL
212700010607     C*  INTESTAZIONE SCAGLIONI
212800010607     C                   Z-ADD     SSL(1)        W0050             5 0
212900021220     C     W0050         COMP      99999                                  41
213000010725     C     SSL(1)        COMP      *ZEROS                                 51
213100010607     C                   Z-ADD     SSL(2)        W0050             5 0
213200010607     C     W0050         COMP      99999                                  42
213300010725     C     SSL(2)        COMP      *ZEROS                                 52
213400010607     C                   Z-ADD     SSL(3)        W0050             5 0
213500010607     C     W0050         COMP      99999                                  43
213600010725     C     SSL(3)        COMP      *ZEROS                                 53
213700010607     C                   Z-ADD     SSL(4)        W0050             5 0
213800010607     C     W0050         COMP      99999                                  44
213900010725     C     SSL(4)        COMP      *ZEROS                                 54
214000010607     C                   Z-ADD     SSL(5)        W0050             5 0
214100010607     C     W0050         COMP      99999                                  45
214200010725     C     SSL(5)        COMP      *ZEROS                                 55
214300010607     C                   Z-ADD     SSL(6)        W0050             5 0
214400010607     C     W0050         COMP      99999                                  46
214500010725     C     SSL(6)        COMP      *ZEROS                                 56
214600010607     C*
214700010607     C* SE IL 41 E' ACCESO PERCHÈ IL PRIMO SCAGLIONE E' UGUALE A TUTTI 999999
214800010627     C* O IL SECONDO SCAGLIONE E' UGUALE A ZERO
214900010627     C* MA SE NON SI TRATTA + DEL 1° SCAGLIONE IN ASSOLUTO DEVO STAMPARE "OLTRE
215000010607     C* LO SCAGLIONE PRECEDENTE
215100010607     C* QUINDI ACCENDO IL 40 E SPENGO IL 41
215200010607     C*
215300010607     C                   IF        *IN41 AND K4 <> 1
215400010607     C                   SETOFF                                       41
215500010607     C                   SETON                                        40
215600010607     C                   ENDIF
215700061207      * indicatore e campo di comodo per tariffa Fuel a scaglioni
215800061207      * sto stampando il dettaglio tariffario quindi pulisco il campo e
215900061207      * spengo l'indicatore
216000061207     c                   clear                   desumf
216100061207     c                   eval      *in12 = *off
216200010607     C*****
216300010607     C*  STAMPA INTESTAZIONE INIZIALE
216400010607     C*****
216500010608      * preparo la testata del dettaglio tariffario
216600010608     c                   exsr      SR_STATESDET
216700010608      *
216800010607     C*****
216900010613     C*  STAMPA DETTAGLIO TARIFFARIO
217000010613     C*****
217100010613     C                   EXSR      SR_FULCRO
217200010607     C* 99 indicatore di errore risultante dalla routine FULCRO
217300010614     C   99              GOTO      ENDDET
217400010615      * vedo se devo stampare l'inoltro
217500010615     c                   EXSR      SR_INOLTR
217600010615      * Rapporto peso volume SOLO SE NON È DPD
217700010615     c  N25              EXSR      SR_STARAP
217800010618      *
217900010618      * rielaboro altri 6 scaglioni
218000010618      *
218100010618     c                   add       6             k4
218200010618      * se scaglioni minori o uguali a 18 e scaglione valorizzato
218300010618     c                   if        k4 <= 18 and
218400010618     c                             sgl(k4) <> *zeros
218500010618      * pulisco schiere di comodo per la stampa degli scaglioni
218600010618     c                   clear                   ssld
218700010618     c                   clear                   sslb
218800010618     c                   clear                   ssla
218900010618     c                   clear                   ss1d
219000010618     c                   clear                   ss1a
219100010618     c                   clear                   ss2d
219200010618     c                   clear                   ss2a
219300010618     c                   clear                   ssl
219400010618      * spengo indicatori che condizionano la stampa
219500010618     c                   setoff                                       404142
219600010618     c                   setoff                                       434445
219700010618     c                   setoff                                       46
219800010725     c                   setoff                                       515253
219900010725     c                   setoff                                       545556
220000010618      * Recupero i campi di lettura salvati la prima volta
220100010618      *
220200010618     c                   eval      rotlnp = wrklnp
220300010618     c                   eval      stalnp = wrkslp
220400020703     c                   eval      prtlnp = wrkprtlnp
220500010618     c                   eval      rotcts = wrkcts
220600010618     c                   eval      rotreg = wrkreg
220700010618     c                   eval      rotpro = wrkpro
220800010618     c                   eval      rotcap = wrkcap
220900010618      *
221000010618     c                   if        not *in10
221100010621     c                   eval      tadcts = wrkcts
221200010618     c                   move      rotreg        com04
221300010618     c                   movel     com04         tadorp
221400010618     c                   move      rotpro        tadorp
221500010618     c                   endif
221600010618      *
221700010618     c                   exsr      regdet
221800010618     c   99              goto      enddet
221900010618      *
222000010618     c                   exsr      prodet
222100010618     c   99              goto      enddet
222200010618      *
222300010618     c                   else
222400010618      * non ci sono + scaglioni da stampare
222500010618     c                   eval      k4=19
222600010618     c                   leave
222700010618      *
222800010618     c                   endif
222900010618      *
223000010618     c                   enddo
223100010614      *
223200010614     c     ENDDET        ENDSR
223300010614      *
223400010608     C*-------------------------------------------------------------------------
223500010608     C*  SR_STATESDET PREPARO E STAMPO TESTATA DETTAGLIO TARIFFARIO
223600010608     C*-------------------------------------------------------------------------
223700010608     C     SR_STATESDET  BEGSR
223800010725      *
223900010725     c                   setoff                                       79
224000010608      *
224100010608      * Se scaglioni senza decimali
224200010608      *
224300010608     c                   if        not *in91
224400010622     c                   eval      dissca = ' '+%editc(ssld(1):'3') + sslb(1) +
224500010801     c                                      %editc(ssla(1):'Z') + '    ' +
224600010608     c                                      %editc(ssld(2):'3') + sslb(2) +
224700010801     c                                      %editc(ssla(2):'Z') + '    ' +
224800010608     c                                      %editc(ssld(3):'3') + sslb(3) +
224900010801     c                                      %editc(ssla(3):'Z') + '    ' +
225000010608     c                                      %editc(ssld(4):'3') + sslb(4) +
225100010801     c                                      %editc(ssla(4):'Z') + '    ' +
225200010608     c                                      %editc(ssld(5):'3') + sslb(5) +
225300010801     c                                      %editc(ssla(5):'Z') + '    ' +
225400010608     c                                      %editc(ssld(6):'3') + sslb(6) +
225500010608     c                                      %editc(ssla(6):'Z')
225600010608     c                   endif
225700010608      *
225800010608      * Se scaglioni con 1 decimale
225900010608      *
226000080901     c                   if        *in91 and not *in37 and not *in38
226100010626     c                   eval      dissca = '   '+%editc(ss1d(1):'4') +sslb(1)+
226200010801     c                                      %editc(ss1a(1):'4') + '    ' +
226300010608     c                                      %editc(ss1d(2):'4') + sslb(2) +
226400010801     c                                      %editc(ss1a(2):'4') + '    ' +
226500010608     c                                      %editc(ss1d(3):'4') + sslb(3) +
226600010801     c                                      %editc(ss1a(3):'4') + '    ' +
226700010608     c                                      %editc(ss1d(4):'4') + sslb(4) +
226800010801     c                                      %editc(ss1a(4):'4') + '    ' +
226900010608     c                                      %editc(ss1d(5):'4') + sslb(5) +
227000010801     c                                      %editc(ss1a(5):'4') + '    ' +
227100010608     c                                      %editc(ss1d(6):'4') + sslb(6) +
227200010608     c                                      %editc(ss1a(6):'4')
227300010608     c                   endif
227400010608      *
227500010608      * Se scaglioni con 2 decimali
227600010608      *
227700080901     c                   if        *in91  and  *in37 and not *in38
227800010622     c                   eval      dissca = ' '+%editc(ss2d(1):'4') + sslb(1) +
227900010801     c                                      %editc(ss2a(1):'4') + '    ' +
228000010608     c                                      %editc(ss2d(2):'4') + sslb(2) +
228100010801     c                                      %editc(ss2a(2):'4') + '   ' +
228200010608     c                                      %editc(ss2d(3):'4') + sslb(3) +
228300010801     c                                      %editc(ss2a(3):'4') + '   ' +
228400010608     c                                      %editc(ss2d(4):'4') + sslb(4) +
228500010801     c                                      %editc(ss2a(4):'4') + '   ' +
228600010608     c                                      %editc(ss2d(5):'4') + sslb(5) +
228700010801     c                                      %editc(ss2a(5):'4') + '   ' +
228800010608     c                                      %editc(ss2d(6):'4') + sslb(6) +
228900010608     c                                      %editc(ss2a(6):'4')
229000010608     c                   endif
229100080901      *
229200080901      * Se scaglioni con 3 decimali
229300080901      *
229400080901     c                   if        *in91  and  *in37 and  *in38
229500080901     c                   eval      dissca = ' '+%editc(ss3d(1):'4') + sslb(1) +
229600080901     c                                      %editc(ss3a(1):'4') + '   ' +
229700080901     c                                      %editc(ss3d(2):'4') + sslb(2) +
229800080901     c                                      %editc(ss3a(2):'4') + '  ' +
229900080901     c                                      %editc(ss3d(3):'4') + sslb(3) +
230000080901     c                                      %editc(ss3a(3):'4') + '  ' +
230100080901     c                                      %editc(ss3d(4):'4') + sslb(4) +
230200080901     c                                      %editc(ss3a(4):'4') + '  ' +
230300080901     c                                      %editc(ss3d(5):'4') + sslb(5) +
230400080901     c                                      %editc(ss3a(5):'4') + '  ' +
230500080901     c                                      %editc(ss3d(6):'4') + sslb(6) +
230600080901     c                                      %editc(ss3a(6):'4')
230700080901     c                   endif
230800010608      * se 40 acceso il 1° scaglione è = 99999 dopo almeno 6 scaglioni
230900010608     c                   if        *in40
231000060606      * in lingua
231100060606     c                   select
231200060606     c                   when      i45lin = 'I'
231300010608     c                   eval      dissca = '    OLTRE '
231400060606     c                   when      i45lin = '2'
231500060606     c                   eval      dissca = '    OVER  '
231600060816     c                   when      i45lin = '3'
231700060817     c                   eval      dissca = '    PLUS  '
231800060816     c                   when      i45lin = '4'
231900060818     c                   eval      dissca = '  MEHR ALS'
232000060606     c                   endsl
232100010608     c                   endif
232200010608      * se 41 acceso il 1° scaglione è = 99999
232300010608     c                   if        *in41
232400010608     c                   clear                   desum
232500010608     c                   eval      dissca = ds3um
232600010626     c                   seton                                        79
232700010608     c                   endif
232800010608      * se 42 acceso il 2° scaglione è = 99999
232900010608     c                   if        *in42
233000010608     c                   clear                   dissca2
233100060606      * in lingua
233200060606     c                   select
233300060606     c                   when      i45lin = 'I'
233400010622     c                   eval      dissca2 = 'OLTRE  '
233500060606     c                   when      i45lin = '2'
233600060606     c                   eval      dissca2 = 'OVER   '
233700060817     c                   when      i45lin = '3'
233800060817     c                   eval      dissca2 = 'PLUS   '
233900060816     c                   when      i45lin = '4'
234000060817     c                   eval      dissca2 = 'MEHR ALS '
234100060606     c                   endsl
234200010622     c                   move      dissca2       dissca
234300010608     c                   endif
234400010725      * se 52 acceso il 2° scaglione è = 00000
234500010725     c                   if        *in52
234600010725     c                   clear                   dissca2
234700010725     c                   move      dissca2       dissca
234800010725     c                   endif
234900010608      * se 43 acceso il 3° scaglione è = 99999
235000010608     c                   if        *in43
235100010608     c                   clear                   dissca3
235200060606      * in lingua
235300060606     c                   select
235400060606     c                   when      i45lin = 'I'
235500010801     c                   eval      dissca3 = 'OLTRE  '
235600060606     c                   when      i45lin = '2'
235700060606     c                   eval      dissca3 = 'OVER   '
235800060817     c                   when      i45lin = '3'
235900060817     c                   eval      dissca3 = 'PLUS   '
236000060816     c                   when      i45lin = '4'
236100060817     c                   eval      dissca3 = 'MEHR ALS '
236200060606     c                   endsl
236300010622     c                   move      dissca3       dissca
236400010608     c                   endif
236500010725      * se 53 acceso il 3° scaglione è = 00000
236600010725     c                   if        *in53
236700010725     c                   clear                   dissca3
236800010725     c                   move      dissca3       dissca
236900010725     c                   endif
237000010608      * se 44 acceso il 4° scaglione è = 99999
237100010608     c                   if        *in44
237200010608     c                   clear                   dissca4
237300060606      * in lingua
237400060606     c                   select
237500060606     c                   when      i45lin = 'I'
237600010801     c                   eval      dissca4 = 'OLTRE  '
237700060606     c                   when      i45lin = '2'
237800060606     c                   eval      dissca4 = 'OVER   '
237900060817     c                   when      i45lin = '3'
238000060817     c                   eval      dissca4 = 'PLUS   '
238100060816     c                   when      i45lin = '4'
238200060817     c                   eval      dissca4 = 'MEHR ALS '
238300060606     c                   endsl
238400010622     c                   move      dissca4       dissca
238500010608     c                   endif
238600010725      * se 54 acceso il 4° scaglione è = 00000
238700010725     c                   if        *in54
238800010725     c                   clear                   dissca4
238900010725     c                   move      dissca4       dissca
239000010725     c                   endif
239100010608      * se 45 acceso il 5° scaglione è = 99999
239200010608     c                   if        *in45
239300010608     c                   clear                   dissca5
239400060606      * in lingua
239500060606     c                   select
239600060606     c                   when      i45lin = 'I'
239700010801     c                   eval      dissca5 = 'OLTRE  '
239800060606     c                   when      i45lin = '2'
239900060606     c                   eval      dissca5 = 'OVER   '
240000060817     c                   when      i45lin = '3'
240100060817     c                   eval      dissca5 = 'PLUS   '
240200060816     c                   when      i45lin = '4'
240300060817     c                   eval      dissca5 = 'MEHR ALS '
240400060606     c                   endsl
240500010622     c                   move      dissca5       dissca
240600010608     c                   endif
240700010725      * se 55 acceso il 5° scaglione è = 00000
240800010725     c                   if        *in55
240900010725     c                   clear                   dissca5
241000010725     c                   move      dissca5       dissca
241100010725     c                   endif
241200010608      * se 46 acceso il 6° scaglione è = 99999
241300010608     c                   if        *in46
241400010608     c                   clear                   dissca6
241500060606      * in lingua
241600060606     c                   select
241700060606     c                   when      i45lin = 'I'
241800010725     c                   eval      dissca6 = 'OLTRE '
241900060606     c                   when      i45lin = '2'
242000060606     c                   eval      dissca6 = 'OVER   '
242100060817     c                   when      i45lin = '3'
242200060817     c                   eval      dissca6 = 'PLUS   '
242300060816     c                   when      i45lin = '4'
242400060817     c                   eval      dissca6 = 'MEHR ALS '
242500060606     c                   endsl
242600010622     c                   move      dissca6       dissca
242700010608     c                   endif
242800010725      * se 56 acceso il 6° scaglione è = 00000
242900010725     c                   if        *in56
243000010725     c                   clear                   dissca6
243100010725     c                   move      dissca6       dissca
243200010725     c                   endif
243300010608     C*
243400010612      * prepatro la schiera dell'applicazione tariffa finita
243500010612     C                   Z-ADD     K4            B
243600010612     C                   DO        6             A
243700010612     C                   MOVE      ATF(B)        STF(A)
243800010612     C                   ADD       1             B
243900010612     C                   END
244000010612      *
244100010612     c                   clear                   prtatf
244200010612      *
244300010801     c                   eval      PRTATF = '    ' + stf(1) + '      '  +
244400010801     c                                      stf(2) + '      '    +
244500010801     c                                      stf(3) + '      '    +
244600010801     c                                      stf(4) + '      '    +
244700010626     c                                      stf(5) + '      '    +
244800010612     c                                      stf(6)
244900010612     C*
245000010917      *
245100010917     c                   if        *in80 or curline > 56
245200010917     c                   write     ta49pfin
245300010917     c                   write     ta49pPP
245400010917     c                   setoff                                       80
245500010917     c                   endif
245600010917      * se non stampo dettaglio tariffe particolari stampo linea di partenza
245700010625     c  n10              write     ta49p02a
245800010626      * SE TARIFFE PARTICOLARI NON STAMPO DA VOSTRO MAGAZZINO
245900010626     C                   IF        *IN10
246000010626     C                   SETON                                        78
246100010626     C                   ENDIF
246200061207
246300061207      * se tariffa particolare "f " Fuel
246400061207     c                   if        *in12
246500061207      * forzo l'accensione dell'indicatore 79 per non stampare la seconda riga
246600061207      * quando ho più scaglioni
246700061207     c                   eval      *in79 = *on
246800061207      * e pulisco il campo di stampa
246900061207     c                   clear                   prtatf
247000061207     c                   endif
247100010625      *
247200010612     C                   write     ta49p02
247300010612      *
247400010612     c                   ENDSR
247500010613      *
247600010613     C*-------------------------------------------------------------------------
247700010613     C*  SR_FULCRO  STAMPA DETTAGLIO TARIFFARIO
247800010613     C*-------------------------------------------------------------------------
247900010613     C     SR_FULCRO     BEGSR
248000020531
248100020531     c                   eval      w_oklnp = *off
248200010613      *
248300010613      * Leggo il dettaglio tariffario dall'inizio
248400010613      *
248500061122     C  n98DS1KTM        SETLL     TITAD04l
248600061122     c   98ds1ktm        setll     tiofd01l
248700010613     C                   DO        *HIVAL
248800010613      *
248900061122     C  n98DS1KTM        READE     TITAD04l
249000061122     c   98ds1ktm        reade     tiofd01l
249100010613      * se fine file
249200061122     c                   if        %eof
249300010613      *
249400010613      * se cap uguale a blank STAMPO totali per provincia
249500010613      *
249600010613     c                   if        rotcap = *blanks
249700010613     c                   exsr      protot
249800010613     c                   else
249900010613      *
250000010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
250100010613      *
250200010613     c                   exsr      captot
250300010613     c                   endif
250400010613      *
250500010613      * calcoli per regione
250600010613      *
250700010613     c                   exsr      calreg
250800010613      *
250900010613      * stampo i totali per regione
251000010613      *
251100010613     c  n99              exsr      stareg
251200010613      *
251300010613     c                   leave
251400010613      * eof
251500010613     c                   endif
251600010613      *
251700010613      * verifico il record letto se coerente con le richieste di stampa
251800010613      *
251900010613      * se annullato rileggo
252000010613     c                   if        tadatb = 'A'
252100010613     c                   iter
252200010613     c                   endif
252300020531     c                   eval      w_okstp = *off
252400010613      * se I45DET = 'L' devo stampare solo le righe di dettaglio che hanno la
252500010613      *                 LNP uguale alla LNP del codice cliente
252600020531     c                   if        I45DET = 'L'
252700020531      * se ho già stampato una linea di partenza non stampo più niente
252800020531     c                   if        w_oklnp = *on and rotlnp <> 0 and
252900020531     c                             rotlnp <> tadlnp
253000020531     c                   iter
253100020531     c                   endif
253200020531     c                   exsr      sr_chklnp
253300020531     c                   if        w_okstp = *off
253400010613     c                   iter
253500010613     c                   endif
253600020531     c                   eval      w_oklnp = *on
253700020531     c                   endif
253800010613      * se I45DET = 'A' devo stampare solo le righe di dettaglio che hanno la
253900010613      *                 LNP uguale alla LNP del codice cliente o il codice
254000010613      *                 tassazione tra quelli pevisti dalla linea del codice
254100010613      *                 cliente
254200010613     c                   if        I45DET = 'A'
254300020531      * se ho già stampato una linea di partenza non stampo più niente
254400020531     c                   if        w_oklnp = *on and rotlnp <> 0 and
254500020531     c                             rotlnp <> tadlnp
254600020531     c                   iter
254700020531     c                   endif
254800020531     c                   exsr      sr_chklnp
254900010613     c     tadcts        lookup    ctl                                    35
255000020531     c                   if        not *in35 and w_okstp = *off
255100010613     c                   iter
255200010613     c                   endif
255300020531     c                   eval      w_oklnp = *on
255400010613     c                   endif
255500010613      *
255600010613      * verifico se è uno scaglione previsto
255700010613      *
255800010613     c                   z-add     tadsgl        wsgl72
255900010614     c     wsgl72        lookup    ssl                                    35
256000010614      * se non trovato nei 6 scaglioni da stampare verifico se scaglione corrett
256100010614     c                   if        not *in35
256200010613     c     wsgl72        lookup    sgl                                    35
256300010613     c                   if        not *in35
256400010613      * errore scaglione non stampabile
256500050914      * esco dal pgm
256600050914     c                   seton                                        99
256700010613     c                   movel     msg(8)        errmsg
256800010613     c                   exsr      staerr
256900050914     c                   leave
257000010614     c                   endif
257100010613     c                   iter
257200010613     c                   endif
257300010613
257400010613      * preparo i campi per eventuali rotture
257500010613
257600010613     c                   eval      compro = %subst(tadorp:5:1)
257700010613     c                   eval      comreg = %subst(tadorp:3:2)
257800010614      *
257900010614     c                   SELECT
258000010614      *
258100010613      *-------------------------------------------------------------------------
258200010613      *           ROTTURA PER      L I N E A    D I    P A R T E N Z A
258300010613      *-------------------------------------------------------------------------
258400010614     c                   WHEN      tadlnp  <> rotlnp
258500010613     c                   eval      rotlnp = tadlnp
258600010613     c                   eval      rotreg = comreg
258700010613     c                   eval      rotpro = compro
258800010613      * verifico se cap valorizzato
258900010613     c                   if        rotcap = *blanks
259000010613      * se cap uguale a blank STAMPO totali per provincia
259100010613     c                   exsr      protot
259200010613     c                   else
259300010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
259400010613     c                   exsr      captot
259500010613     c                   endif
259600010613      *
259700010613     c                   eval      rotcap = tadcap
259800010613      * calcoli per regione
259900010613     c                   exsr      calreg
260000010613      * se ci sono errori vado a fine
260100010613     c   99              leave
260200010613      * stampo i totali per regione
260300010613     c                   exsr      stareg
260400010613      * decodifico la nuova linea di partenza
260500020531     c     tadlnp        chain     azorg01l
260600020531     c                   if        %found(azorg01l)
260700020531     c                   movel     orgdes        stalnp
260800020624      * controllo se linea di partenza 9xx
260900020624     c                   if        orgfl2 = 'S'
261000020624      * italia
261100020624     c                   if        tadlnp = 950
261200060704      * in lingua
261300060704     c                   select
261400060704     c                   when      i45lin = 'I'
261500070312     c                   eval      prtlnp =
261600070312     c                             'Merce ritirata da tutte le ns. filiali'
261700020624     c                             + '  ' + stalnp
261800060704     c                   when      i45lin = '2'
261900060704     c                   eval      prtlnp = 'Goods collected from all our +
262000070314     c                              branches' + '  ' + stalnp
262100060817     c                   when      i45lin = '3'
262200070314     c                   eval      prtlnp = 'Marchandise retirée par toutes +
262300070314     c                              nos filiales' + '  ' + stalnp
262400060817     c                   when      i45lin = '4'
262500070314     c                   eval      prtlnp = 'Abholung in allen unseren Filialen'
262600060817     c                             + '  ' + stalnp
262700060704     c                   endsl
262800020624     c                   else
262900020624      * regione
263000060704      * in lingua
263100060704     c                   select
263200060704     c                   when      i45lin = 'I'
263300070312     c                   eval      prtlnp = 'Merce ritirata dalle ns. filiali +
263400070312     c                              della regione' + '  ' + stalnp
263500060704     c                   when      i45lin = '2'
263600070314     c                   eval      prtlnp = 'Goods collected from our branches +
263700060704     c                              in the region' + '  ' + stalnp
263800060817     c                   when      i45lin = '3'
263900070314     c                   eval      prtlnp = 'Marchandise retirée par nos +
264000070314     c                              filiales de la région' + '  ' + stalnp
264100060817     c                   when      i45lin = '4'
264200070314     c                   eval      prtlnp = 'Abholung in unseren Filialen der +
264300060817     c                              Region' + '  ' + stalnp
264400060704     c                   endsl
264500020624     c                   endif
264600020624     c                   else
264700020624      * lnp non è 9xx
264800060704      * in lingua
264900060704     c                   select
265000060704     c                   when      i45lin = 'I'
265100070312     c                   eval      prtlnp =
265200070312     c                             'Merce ritirata dalla ns. filiale di'
265300020624     c                             + '  ' + stalnp
265400060704     c                   when      i45lin = '2'
265500070314     c                   eval      prtlnp = 'Goods collected from our branch +
265600060704     c                              in' + '  ' + stalnp
265700060817     c                   when      i45lin = '3'
265800060817     c                   eval      prtlnp = 'Marchandise retirée par notre +
265900070314     c                              filiale de' + '  ' + stalnp
266000060817     c                   when      i45lin = '4'
266100070314     c                   eval      prtlnp = 'Abholung in unseren Filialen in'
266200060817     c                             + '  ' + stalnp
266300060704     c                   endsl
266400020624     c                   endif
266500020624
266600020531     c                   endif
266700010613      * stampo la testata scaglioni con la nuova linea di partenza
266800010613      *
266900010625      *
267000010628     c                   if        *in80 or curline > 56
267100010628     c                   write     ta49pfin
267200010628     c                   write     ta49pPP
267300010628     c                   setoff                                       80
267400010628     c                   endif
267500010625      *
267600010625     c                   write     ta49p02a
267700010625     c                   write     ta49p02
267800010613      * pulizie e controllo dettaglio per regione
267900010613     c                   exsr      regdet
268000010613      * pulizie e controllo dettaglio per provincia
268100010613     c                   exsr      prodet
268200010613      * se ci sono errori vado a fine
268300010613     c   99              leave
268400010613      *
268500010613      *
268600010613      * sto leggendo per la stessa linea di partenza
268700010613      *
268800010613      *-------------------------------------------------------------------------
268900010613      *           ROTTURA PER      R E G I O N E
269000010613      *-------------------------------------------------------------------------
269100010614     c                   WHEN      comreg <> rotreg
269200010613     c                   eval      rotreg = comreg
269300010613     c                   eval      rotpro = compro
269400010613      * verifico se cap valorizzato
269500010613     c                   if        rotcap = *blanks
269600010613      * se cap uguale a blank STAMPO totali per provincia
269700010613     c                   exsr      protot
269800010613     c                   else
269900010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
270000010613     c                   exsr      captot
270100010613     c                   endif
270200010613      *
270300010613     c                   eval      rotcap = tadcap
270400010613      * calcoli per regione
270500010613     c                   exsr      calreg
270600010613      * se ci sono errori vado a fine
270700010613     c   99              leave
270800010613      * stampo i totali per regione
270900010613     c                   exsr      stareg
271000010613      * pulizie e controllo dettaglio per regione
271100010613     c                   exsr      regdet
271200010613      * pulizie e controllo dettaglio per provincia
271300010613     c                   exsr      prodet
271400010613      * se ci sono errori vado a fine
271500010613     c   99              leave
271600010613      *
271700010613      *
271800010613      * sto leggendo per la stessa linea di partenza / regione
271900010613      *
272000010613      *-------------------------------------------------------------------------
272100010613      *           ROTTURA PER      P R O V I N C I A
272200010613      *-------------------------------------------------------------------------
272300010614     c                   WHEN      compro <> rotpro
272400010613     c                   eval      rotpro = compro
272500010613      * verifico se cap valorizzato
272600010613     c                   if        rotcap = *blanks
272700010613      * se cap uguale a blank STAMPO totali per provincia
272800010613     c                   exsr      protot
272900010613     c                   else
273000010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
273100010613     c                   exsr      captot
273200010613     c                   endif
273300010613      *
273400010613     c                   eval      rotcap = tadcap
273500010613      * pulizie e controllo dettaglio per provincia
273600010613     c                   exsr      prodet
273700010613      * se ci sono errori vado a fine
273800010613     c   99              leave
273900010613      *
274000010613      *
274100010613      * sto leggendo per la stessa linea di partenza / regione / provincia
274200010613      *
274300010613      *-------------------------------------------------------------------------
274400010613      *           ROTTURA PER      P A E S E   D I    A R R I V O   (CAP)
274500010613      *-------------------------------------------------------------------------
274600010614     c                   WHEN      tadcap <> rotcap
274700010613      * verifico se cap valorizzato
274800010613     c                   if        rotcap = *blanks
274900010613      * se cap uguale a blank STAMPO totali per provincia
275000010613     c                   exsr      protot
275100010613     c                   else
275200010613      * se cap valorizzato STAMPO totali per PAESE DI ARRIVO  (CAP)
275300010613     c                   exsr      captot
275400010613     c                   endif
275500010613      * se ci sono errori vado a fine
275600010613     c   99              leave
275700010613      *
275800010613     c                   eval      rotcap = tadcap
275900010613      * pulisco le schiere di stampa
276000010613     c                   clear                   tct
276100010613     c                   clear                   tmn
276200010613     c                   clear                   tmx
276300010613     c                   clear                   tin
276400010613      *
276500010614     c                   ENDSL
276600010613      *
276700010613      *-------------------------------------------------------------------------
276800010613      *      E L A B O R O   L O   S C A G L I O N E
276900010613      *-------------------------------------------------------------------------
277000010613
277100010613      * ricerco lo SCAGLIONE di appartenenza
277200010613     c                   z-add     1             k1
277300010613     c                   z-add     tadsgl        wsgl72
277400010613     c     wsgl72        lookup    ssl(k1)                                35
277500010614      * se non trovato scrivo nel 1° scaglione
277600010614     c  n35              z-add     1             k1
277700010614      * valorizzo le schiere degli importi da stampare
277800010614     c                   z-add     taditr        tct(k1)
277900010614     c                   z-add     tadmin        tmn(k1)
278000010614     c                   z-add     tadmax        tmx(k1)
278100010614      * se devo stampare l'inoltro per ogni dettaglio
278200010614     c                   if        stino = 'Y'
278300010614     c                   z-add     tadino        tin(k1)
278400010614     c                   endif
278500010614      *
278600010614     c                   ENDDO
278700010614      *
278800010614     c                   ENDSR
278900020531
279000020531      *-------------------------------------------------------------------------
279100020531      *  SR_CHKLNP  controllo se LNP ok per la stampa
279200020531      *-------------------------------------------------------------------------
279300020531     c     sr_chklnp     begsr
279400020531
279500020531     c                   if        w_okstp = *off
279600020531     c                   select
279700020531     c                   when      tadlnp = comfil
279800020531     c                   eval      w_okstp = *on
279900020531     c                   when      tadlnp = §ogret
280000020531     c                   eval      w_okstp = *on
280100020531     c                   when      tadlnp = 950
280200020531     c                   eval      w_okstp = *on
280300020531     c                   endsl
280400020531     c                   endif
280500020531
280600020531     c                   endsr
280700020531
280800010614     C*-------------------------------------------------------------------------
280900010614     C*  PROTOT  TOTALI X  P R O V I N C I A
281000010614     C*-------------------------------------------------------------------------
281100010614     C     PROTOT        BEGSR
281200010614      *
281300010614      *  SALVO LA SCHIERA TCT NEL PRIMO BIG LIBERO
281400010614     C                   Z-ADD     1             K
281500010614     C     *BLANKS       LOOKUP    BIG(K)                                 35
281600010614
281700010614      * se non trovo nessun posto libero errore
281800010614
281900010614     C                   if        not *in35
282000010614     C                   seton                                        99
282100010614     C                   move      MSG(9)        errmsg
282200010614     C                   EXSR      STAERR
282300050930      * vado a fine se errore
282400050930     c                   goto      fine
282500010614     C                   else
282600051006      * Controllo se ok tutti gli scaglioni
282700051006     c                   ExSr      Sr_Ctrsca
282800010614      * carico la schiera delle province 6 alla volta
282900010614      * città
283000010614     C                   DO        6             B
283100010614     C                   MOVE      TCT(B)        COM(B)
283200010614     C                   END
283300010614     C                   MOVEA     COM           BIG(K)
283400010614      *
283500010614     C                   MOVE      CODSIG        BSI(K)
283600010614     C                   MOVE      CODORP        BRG(K)
283700050204     c                   move      desprv        dprv(k)
283800050303     c                   eval      nodes(k) = nostdes
283900010614      * minimo
284000010614     C                   DO        6             B
284100010614     C                   MOVE      TMN(B)        COM(B)
284200010614     C                   END
284300010614     C                   MOVEA     COM           BMN(K)
284400010614      * masismo
284500010614     C                   DO        6             B
284600010614     C                   MOVE      TMX(B)        COM(B)
284700010614     C                   END
284800010614     C                   MOVEA     COM           BMX(K)
284900010614      * inoltro
285000010614     C                   DO        6             B
285100010614     C                   MOVE      TIN(B)        COM(B)
285200010614     C                   END
285300010614     C                   MOVEA     COM           BIN(K)
285400010614      **
285500010614     C                   END
285600010614      *
285700010614     C                   ENDSR
285800010614     C*-------------------------------------------------------------------------
285900010614     C*  CAPTOT  TOTALI X  PAESE DI ARRIVO (CAP)
286000010614     C*-------------------------------------------------------------------------
286100010614     C     CAPTOT        BEGSR
286200010614      *  SALVO LA SCHIERA TCT DIRETTAMENTE NEL PDE/WIN
286300010614     C                   Z-ADD     1             P
286400010614     C     *BLANKS       LOOKUP    PDE(P)                                 35
286500010614      * se non trovato do errore
286600010614     c                   if        not *in35
286700010614     C                   seton                                        99
286800010614     C                   move      MSG(12)       errmsg
286900010614     C                   EXSR      STAERR
287000010614      *
287100010614     C                   ELSE
287200051006      * Controllo se ok tutti gli scaglioni
287300051006     c                   ExSr      Sr_Ctrsca
287400010614      *
287500010614     C                   MOVEL     *all'.'       PDE(P)
287600010614     C                   MOVE      rotcap        PDE(P)
287700010614      * città
287800010614     C                   DO        6             B
287900010614     C                   MOVE      TCT(B)        COM(B)
288000010614     C                   END
288100010614     C                   MOVEA     COM           WIN(P)
288200010614      * minimo
288300010614     C                   DO        6             B
288400010614     C                   MOVE      TMN(B)        COM(B)
288500010614     C                   END
288600010614     C                   MOVEA     COM           WMN(P)
288700010614      * massimo
288800010614     C                   DO        6             B
288900010614     C                   MOVE      TMX(B)        COM(B)
289000010614     C                   END
289100010614     C                   MOVEA     COM           WMX(P)
289200010614      * inoltro
289300010614     C                   DO        6             B
289400010614     C                   MOVE      TIN(B)        COM(B)
289500010614     C                   END
289600010614     C                   MOVEA     COM           WTI(P)
289700010614     C                   END
289800010614      *
289900010614     C                   ENDSR
290000051006      *-------------------------------------------------------------------------
290100051006      * Controllo se ok gli scaglioni da stampare
290200051006      *-------------------------------------------------------------------------
290300051006     c     Sr_Ctrsca     BegSr
290400051006
290500051006     c                   Select
290600051006
290700051006     c                   When      wnrscaglio > 12
290800051006      * sto stampando il primo o il secondo giro di scaglioni
290900051006      * vuol dire che devo stampare di sicuro 6 importi
291000051006     c                   If        k4 < 13
291100051006     c                   Eval      yy = 6
291200051006      * sto stampando il terzo giro di scaglioni
291300051006      * cerco quanti importi devo stampare
291400051006     c                   Else
291500060428     c                   Eval      yy = wnrscaglio - 12
291600051006     c                   EndIf
291700051006
291800051006     c                   When      wnrscaglio > 6 and wnrscaglio < 12
291900051006      * sto stampando il primo giro di scaglioni
292000051006      * vuol dire che devo stampare di sicuro 6 importi
292100051006     c                   If        k4 = 1
292200051006     c                   Eval      yy = 6
292300051006      * sto stampando il secondo giro di scaglioni
292400051006      * cerco quanti importi devo stampare
292500051006     c                   Else
292600051006     c                   Eval      yy = wnrscaglio - 6
292700051006     c                   EndIf
292800051006
292900051006     c                   When      wnrscaglio = 6
293000051006     c                   Eval      yy = 6
293100051006
293200051006     c                   When      wnrscaglio < 6
293300051006     c                   Eval      yy = wnrscaglio
293400051006     c                   EndSl
293500051006
293600051006      * controllo se ci sono degli importi a zero
293700051006     c                   Do        yy            xx
293800051006     c                   If        tct(xx) = *Zeros
293900051006     c                   Eval      errmsg = msg(8)
294000051006     c                   ExSr      staerr
294100051006     c                   Goto      fine
294200051006     c                   EndIf
294300051006     c                   EndDo
294400051006
294500051006     c                   EndSr
294600010614     C*-------------------------------------------------------------------------
294700010614     C*  CALREG  CALCOLO SKIERE X STAMPA REGIONE
294800010614     C*-------------------------------------------------------------------------
294900010614     C     CALREG        BEGSR
295000010614      *
295100010614      *                  N  O  T  E
295200010614      * Leggo BIG (schiera per ogni provincia della regione) e carico WIN/WCT
295300010614      * schiere di work)
295400010614      * X ogni provincia letta (quindi per ogni elemento) dopo averlo caricato
295500010614      * controllo se ci sono altre province uguali
295600010614      * se si accodo nella skiera dei COD/TASSAZ. che hanno la stessa tariffa
295700010614      * leggo i soli elementi con BRG>0 (a mano mano che li raggruppo
295800010614      * li abblenco). Mi fermo quando leggo la regione che è sempre
295900010614      * l'ultimo e va da solo : K=1 la descrizione è  CAPOLUOGHI
296000010614      *                         K>1 "    "    "    è  ALTRI
296100010614      *
296200010614      * RICERCO IL 1.O ELEMENTO NON OCCUPATO DA PAESI DI ARRIVO
296300010614     C                   DO        29            P
296400010614     C     PDE(P)        COMP      *blanks                                35
296500010614     C   35              SUB       1             P
296600010614     C  N35              END
296700110216
296800110216     c                   clear                   savk
296900010614      *
297000050616    1C                   DO        15            K
297100010614    2C     BRG(K)        IFNE      '  '
297200010614      * ELEMENTO VALIDO                                    -----------
297300010614      *
297400010614      * 9 = regione
297500010614      *
297600010614    3C     BRG(K)        IFEQ      '9'
297700010614     C                   ADD       1             P
297800110330     c                   eval      savk = p
297900010614      * errore
298000010614    4C     P             IFGT      30
298100010614     C                   SETON                                        99
298200010614     C                   MOVE      MSG(12)       ERRMSG
298300010614     C                   EXSR      STAERR
298400010614     C                   GOTO      ENDCAL
298500010614    4C                   END
298600010614      * città
298700010614     C                   MOVEA     BIG(K)        COMO42
298800010614     C                   MOVEA     COMO42        WIN(P)
298900010614     C                   MOVE      *BLANKS       BIG(K)
299000010614      * minimo
299100010614     C                   MOVEA     BMN(K)        CMNO42
299200010614     C                   MOVEA     CMNO42        WMN(P)
299300010614     C                   MOVE      *BLANKS       BMN(K)
299400010614      * massimo
299500010614     C                   MOVEA     BMX(K)        CMXO42
299600010614     C                   MOVEA     CMXO42        WMX(P)
299700010614     C                   MOVE      *BLANKS       BMX(K)
299800010614      * inoltro
299900010614     C                   MOVEA     BIN(K)        CINO42
300000010614     C                   MOVEA     CINO42        WTI(P)
300100010614     C                   MOVE      *BLANKS       BIN(K)
300200010614      *
300300010614     C                   MOVE      '  '          BRG(K)
300400010614      *
300500010614    4C     K             IFEQ      1
300600010626     C                   EVAL      PDE(P) = '. . . . . . . . . . '
300700010614     C                   ELSE
300800110216     c                   eval      pde(p) = '.........................'
300900010614    4C                   END
301000010614     C                   GOTO      ENDCAL
301100010614      * non si tratta di regione
301200010614
301300010614   X3C                   ELSE
301400010614
301500010614     C                   MOVE      *BLANKS       WCT
301600010614     C                   Z-ADD     0             C
301700050204     c                   Eval      wragprv = *Off
301800010614      * città
301900010614     C                   ADD       1             P
302000010614     C                   MOVEA     BIG(K)        COMO42
302100010614     C                   MOVEA     COMO42        WIN(P)
302200010614     C                   MOVE      *BLANKS       BIG(K)
302300010614      * minimo
302400010614     C                   MOVEA     BMN(K)        CMNO42
302500010614     C                   MOVEA     CMNO42        WMN(P)
302600010614     C                   MOVE      *BLANKS       BMN(K)
302700010614      * massimo
302800010614     C                   MOVEA     BMX(K)        CMXO42
302900010614     C                   MOVEA     CMXO42        WMX(P)
303000010614     C                   MOVE      *BLANKS       BMX(K)
303100010614      * inoltro
303200010614     C                   MOVEA     BIN(K)        CINO42
303300010614     C                   MOVEA     CINO42        WTI(P)
303400010614     C                   MOVE      *BLANKS       BIN(K)
303500010614      * sigla della provincia
303600010614     C                   ADD       1             C
303700010614     C                   MOVE      BSI(K)        WCT(C)
303800010614      *
303900010614     C                   MOVE      '  '          BRG(K)
304000010614      **
304100010614      * DOPO AVER CARICATO LA PROV. LETTA NE CERCO ALTRE DA RAGGRUPPARE
304200010614     C     K             ADD       1             H
304300010614      *
304400050616    4C     H             DO        15            H
304500010614    5C     COMO42        IFEQ      BIG(H)
304600010614     C     CMNO42        ANDEQ     BMN(H)
304700010614     C     CMXO42        ANDEQ     BMX(H)
304800010614     C     CINO42        ANDEQ     BIN(H)
304900010614     C     BRG(H)        ANDNE     '9'
305000010614     C                   ADD       1             C
305100010614     C                   MOVE      BSI(H)        WCT(C)
305200010614     C                   MOVEL     '/'           WCT(C)
305300010614      *
305400010614     C                   MOVE      *BLANKS       BIG(H)
305500010614     C                   MOVE      *BLANKS       BMN(H)
305600010614     C                   MOVE      *BLANKS       BMX(H)
305700010614     C                   MOVE      *BLANKS       BIN(H)
305800010614     C                   MOVE      '  '          BRG(H)
305900050204     c                   Eval      wragprv = *On
306000010614      *
306100010614    5C                   ENDIF
306200010614    4C                   ENDDO
306300010614      *
306400010614     C                   ADD       1             C
306500010614     C                   MOVEA     *ALL'.'       WCT(C)
306600010614     C                   MOVEA     WCT           COMO29
306700010614     C                   MOVEA     COMO29        PDE(P)
306800050204
306900050204      * se tariffa estera e non ho raggruppato più province devo stampare
307000050204      * la descrizione della provincia e non il codice
307100050204     c                   If        wragprv = *Off and (*in02 or *in25)
307200050204     c                   Eval      como29 = *all'.'
307300050303     c                   If        nodes(k) <> 'N'
307400050204     c                   Eval      %subst(como29:1:(%len(%trim(dprv(k))))) =
307500050204     c                             %trim(dprv(k))
307600050303     c                   EndIf
307700050204     c                   Move      como29        pde(p)
307800050204     c                   EndIf
307900010614      *
308000010614    3C                   END
308100010614      *
308200010614    2C                   END
308300010614    1C                   END
308400010614      *
308500010614     C     ENDCAL        ENDSR
308600010614     C*-------------------------------------------------------------------------
308700010615     C*  STAREG  GESTIONE STAMPE REGIONE
308800010614     C*-------------------------------------------------------------------------
308900010614     C     STAREG        BEGSR
309000110216
309100110216     c                   clear                   giro              1 0
309200110216
309300110216      * stampo per prima 'ALTRI' cioè la regione
309400110216     c                   if        savk > 1
309500110216     c                   eval      k = savk
309600110216     c                   else
309700010614      * LOOP EMISSIONE RAGGRUPPAMENTI/REGIONE
309800010614     C                   Z-ADD     1             K
309900110216     c                   endif
310000110216
310100110216     c     rifai         tag
310200010614      *
310300010615    1C     WIN(K)        DOUEQ     *BLANKS
310400110216     c                   if        k = savk  and giro > 0
310500110216     c                   leave
310600110216     c                   endif
310700010614     C                   MOVEA     WIN(K)        COM
310800010614     C                   Z-ADD     1             B
310900010614     C                   DO        6             B
311000010614     C                   MOVE      COM(B)        PAM(B)
311100010614      * PER TARIFFA A VALORE
311200010614     C                   IF        *IN87 AND COM(B) > *ZEROS
311300010614     C                   MOVEL     '%'           AI2(B)
311400010614     C                   END
311500010614      *
311600010614     C                   END
311700010614      * VERIFICO SE C'E' IL MINIMO E IL MASSIMO
311800010614     C                   SETOFF                                       4849
311900010614      *
312000010615    2C                   IF        WMN(K) <> *BLANKS AND WMN(K) <> *ZEROS
312100010614     C                   SETON                                        48
312200010614      *
312300010614     C                   MOVEA     WMN(K)        COM
312400010614      *
312500010614     C                   DO        6             B
312600010614     C                   MOVE      COM(B)        PMN(B)
312700010614     C                   ENDDO
312800010614      *
312900010615    2C                   ENDIF
313000010614      **
313100010615    2C                   IF        WMX(K) <> *BLANKS AND WMX(K) <> *ZEROS
313200010614     C                   SETON                                        49
313300010614      **
313400010614     C                   MOVEA     WMX(K)        COM
313500010614      *
313600010614     C                   DO        6             B
313700010614     C                   MOVE      COM(B)        PMX(B)
313800010614     C                   ENDDO
313900010614      *
314000010615    2C                   ENDIF
314100010614      *
314200010614      * VERIFICO SE DEVO STAMPARE LE SPESE DI INOLTRO
314300010725     C                   SETOFF                                       57
314400010614      *
314500010615    2C                   IF        STINO = 'Y'
314600010725     C                   SETON                                        57
314700010614     C                   MOVEA     WTI(K)        COM
314800010614     C                   Z-ADD     1             B
314900010614      *
315000010614     C                   DO        6             B
315100010614     C                   MOVE      COM(B)        PIN(B)
315200010614     C                   END
315300010614      *
315400010615    2C                   ENDIF
315500010614      *
315600010614     C*
315700010615     C* VERIFICO SE CI SONO DEI VALORI A ZERO
315800010615     C*
315900010615      * VALORIZZO I CAMPI DI STAMPA
316000010615     C                   CLEAR                   TA49P03
316100010615      *
316200010615     C     PAM(1)        COMP      0                                  61
316300010615     C     PAM(2)        COMP      0                                  62
316400010615     C     PAM(3)        COMP      0                                  63
316500010615     C     PAM(4)        COMP      0                                  64
316600010622     C     PAM(5)        COMP      0                                  65
316700010622     C     PAM(6)        COMP      0                                  66
316800010615      *
316900010615     C                   Z-ADD     PAM(1)        TAR01
317000010615     C                   Z-ADD     PAM(2)        TAR02
317100010615     C                   Z-ADD     PAM(3)        TAR03
317200010615     C                   Z-ADD     PAM(4)        TAR04
317300010615     C                   Z-ADD     PAM(5)        TAR05
317400010615     C                   Z-ADD     PAM(6)        TAR06
317500010615      *
317600010615      * VALORIZZO LA PARTE DESCRITTIVA
317700010615     C                   EVAL      P03DES = DESREG + ' - ' + PDE(K)
317800010615      *
317900010615     C                   CLEAR                   DESREG
318000010615      *
318100010615      * SE TARIFFA A VALORE STAMPO LA %
318200010615     C                   IF        *IN87
318300010615     C                   MOVE      AI2(1)        PER01
318400010615     C                   MOVE      AI2(2)        PER02
318500010615     C                   MOVE      AI2(3)        PER03
318600010615     C                   MOVE      AI2(4)        PER04
318700010615     C                   MOVE      AI2(5)        PER05
318800010615     C                   MOVE      AI2(6)        PER06
318900010615     C                   ENDIF
319000010615      *
319100010622      * se of ristampo la testata tariffa
319200010622      *
319300010723     c                   if        *in80 or curline > 61
319400010627     c                   write     ta49pfin
319500010622     c                   write     ta49pPP
319600010625     c                   write     ta49p02a
319700010622     c                   write     ta49p02
319800010622     c                   setoff                                       80
319900010622     c                   endif
320000010622      *
320100010615     C                   WRITE     TA49P03
320200010615      *
320300010615     C*
320400010615     C* VERIFICO SE CI SONO DEI VALORI A ZERO
320500010615     C*
320600010615      * VALORIZZO I CAMPI DI STAMPA
320700010615     C                   CLEAR                   TA49P03
320800010615      * MINIMO
320900010615    2C                   IF        *IN48
321000010614     C     PMN(1)        COMP      0                                  61
321100010614     C     PMN(2)        COMP      0                                  62
321200010614     C     PMN(3)        COMP      0                                  63
321300010614     C     PMN(4)        COMP      0                                  64
321400010622     C     PMN(5)        COMP      0                                  65
321500010622     C     PMN(6)        COMP      0                                  66
321600010615      *
321700010615     C                   Z-ADD     PMN(1)        TAR01
321800010615     C                   Z-ADD     PMN(2)        TAR02
321900010615     C                   Z-ADD     PMN(3)        TAR03
322000010615     C                   Z-ADD     PMN(4)        TAR04
322100010615     C                   Z-ADD     PMN(5)        TAR05
322200010615     C                   Z-ADD     PMN(6)        TAR06
322300010615      *
322400010615      * VALORIZZO LA PARTE DESCRITTIVA
322500060606      * in lingua
322600060606     c                   select
322700060606     c                   when      i45lin = 'I'
322800010626     C                   EVAL      P03DES = '               Tariffa minima ...'
322900060606     c                   when      i45lin = '2'
323000060606     c                   eval      p03des = '               Minimum price ....'
323100060817     c                   when      i45lin = '3'
323200060817     c                   eval      p03des = '               Tarif minimum ....'
323300060817     c                   when      i45lin = '4'
323400060817     c                   eval      p03des = '               Mindestpreis .....'
323500060606     c                   endsl
323600010625      *
323700010625      * se of ristampo la testata tariffa
323800010625      *
323900010723     c                   if        *in80 or curline > 61
324000010627     c                   write     ta49pfin
324100010625     c                   write     ta49pPP
324200010625     c                   write     ta49p02a
324300010625     c                   write     ta49p02
324400010625     c                   setoff                                       80
324500010625     c                   endif
324600010625      *
324700010615     C                   WRITE     TA49P03
324800010615      *
324900010615    2C                   ENDIF
325000010615     C*
325100010615     C* VERIFICO SE CI SONO DEI VALORI A ZERO
325200010615     C*
325300010615      * VALORIZZO I CAMPI DI STAMPA
325400010615     C                   CLEAR                   TA49P03
325500010615      * MASSIMO
325600010615    2C                   IF        *IN49
325700010614     C     PMX(1)        COMP      0                                  61
325800010614     C     PMX(2)        COMP      0                                  62
325900010614     C     PMX(3)        COMP      0                                  63
326000010614     C     PMX(4)        COMP      0                                  64
326100010622     C     PMX(5)        COMP      0                                  65
326200010622     C     PMX(6)        COMP      0                                  66
326300010615      *
326400010615     C                   Z-ADD     PMX(1)        TAR01
326500010615     C                   Z-ADD     PMX(2)        TAR02
326600010615     C                   Z-ADD     PMX(3)        TAR03
326700010615     C                   Z-ADD     PMX(4)        TAR04
326800010615     C                   Z-ADD     PMX(5)        TAR05
326900010615     C                   Z-ADD     PMX(6)        TAR06
327000010615      *
327100010615      * VALORIZZO LA PARTE DESCRITTIVA
327200060606      * in lingua
327300060606     c                   select
327400060606     c                   when      i45lin = 'I'
327500010626     C                   EVAL      P03DES = '               Tariffa massima ...'
327600060606     c                   when      i45lin = '2'
327700060606     c                   eval      p03des = '               Maximum price ....'
327800060817     c                   when      i45lin = '3'
327900060817     c                   eval      p03des = '               Tarif maximum ....'
328000060817     c                   when      i45lin = '4'
328100060817     c                   eval      p03des = '               Höchstpreis ......'
328200060606     c                   endsl
328300010615      *
328400010625      *
328500010625      * se of ristampo la testata tariffa
328600010625      *
328700010723     c                   if        *in80 or curline > 61
328800010627     c                   write     ta49pfin
328900010625     c                   write     ta49pPP
329000010625     c                   write     ta49p02a
329100010625     c                   write     ta49p02
329200010625     c                   setoff                                       80
329300010625     c                   endif
329400010625      *
329500010615     C                   WRITE     TA49P03
329600010615      *
329700010615    2C                   ENDIF
329800010615     C*
329900010615     C* VERIFICO SE CI SONO DEI VALORI A ZERO
330000010615     C*
330100010615      * VALORIZZO I CAMPI DI STAMPA
330200010615     C                   CLEAR                   TA49P03
330300010615      * SPESE DI INOLTRO
330400010725    2c                   IF        *in57
330500010615      * verifico se ci sono dei valori da stampare
330600010615      *
330700010615     C                   XFOOT     PIN           TOTINO
330800010615    3C                   IF        TOTINO > *ZEROS
330900010615      *
331000010614     C     PIN(1)        COMP      0                                  61
331100010614     C     PIN(2)        COMP      0                                  62
331200010614     C     PIN(3)        COMP      0                                  63
331300010614     C     PIN(4)        COMP      0                                  64
331400010622     C     PIN(5)        COMP      0                                  65
331500010622     C     PIN(6)        COMP      0                                  66
331600010615      *
331700010615     C                   Z-ADD     PIN(1)        TAR01
331800010615     C                   Z-ADD     PIN(2)        TAR02
331900010615     C                   Z-ADD     PIN(3)        TAR03
332000010615     C                   Z-ADD     PIN(4)        TAR04
332100010615     C                   Z-ADD     PIN(5)        TAR05
332200010615     C                   Z-ADD     PIN(6)        TAR06
332300010615      *
332400010615      * VALORIZZO LA PARTE DESCRITTIVA
332500060630      * in lingua
332600060630     c                   select
332700060630     c                   when      i45lin = 'I'
332800010626     C                   EVAL      P03DES = '               Spese inoltro ....'
332900060630     c                   when      i45lin = '2'
333000060630     c                   eval      p03des = '               Plus costs .......'
333100060817     c                   when      i45lin = '3'
333200060817     c                   eval      p03des = '               Frais d''envoi ...'
333300060817     c                   when      i45lin = '4'
333400060817     c                   eval      p03des = '          osten für Weiterleitung'
333500060817     c                   endsl
333600010615      *
333700010625      *
333800010723     c                   if        *in80 or curline > 61
333900010627     c                   write     ta49pfin
334000010625     c                   write     ta49pPP
334100010625     c                   write     ta49p02a
334200010625     c                   write     ta49p02
334300010625     c                   setoff                                       80
334400010625     c                   endif
334500010625      *
334600010615     C                   WRITE     TA49P03
334700010615      *
334800010615    2C                   ENDIF
334900010615      *
335000010615    2C                   ENDIF
335100010614     C*
335200010614     C                   ADD       1             K
335300010615    1C                   END
335400110216
335500110216      * se ho stampato 'Altri' torno a loopare per stampare il resto
335600110216     c                   if        savk > 1 and giro = 0
335700110216     c                   eval      k = 1
335800110216     c                   eval      giro = 1
335900110216     c                   goto      rifai
336000110216     c                   endif
336100010615     C*
336200010614     C                   MOVEA     *BLANKS       AI2
336300010615      *
336400010614     C                   ENDSR
336500010625     C*-------------------------------------------------------------------------
336600010615     C*  PRODET  DETTAGLIO INIZIALE X  P R O V I N C I A
336700010625     C*-------------------------------------------------------------------------
336800010615     C     PRODET        BEGSR
336900010615      *
337000010622     C                   Z-ADD     1             K5
337100010615      * cerco il codice tassazione in schiera per TITAD
337200010622     C  N10TADCTS        LOOKUP    CCT(K5)                                35
337300010615      * cerco il codice tassazione in schiera per TITPD
337400010622     C   10TPDCTS        LOOKUP    CCT(K5)                                35
337500010615      *  se non trovato errore
337600010615     C  N35              SETON                                        99
337700010615     C     *IN99         IFEQ      '1'
337800010615     C                   MOVE      MSG(6)        ERRMSG
337900010615     C                   EXSR      STAERR
338000010615      *
338100010615     C                   ELSE
338200010615      *
338300010615      * TITAD
338400010615     C  N10              MOVE      TADORP        CODORP
338500010615      * TITPD
338600010615     C   10              MOVE      TPDORP        CODORP
338700010615      *
338800010622     C                   MOVE      SCT(K5)       CODSIG
338900050204     C                   MOVE      prvdes(k5)    desprv
339000050303     c                   Eval      nostdes = noctdes(k5)
339100010615     C*  PULIZIA SCHIERE PROVINCIA
339200010615     c                   if        not *in10
339300010615     C                   CLEAR                   TCT
339400010615     C                   CLEAR                   TMN
339500010615     C                   CLEAR                   TMX
339600010615     C                   CLEAR                   TIN
339700010615      *
339800010615     c                   else
339900010615      *
340000010615     C                   MOVEA     *ZEROS        TAP
340100010615     C                   MOVEA     *ZEROS        TPN
340200010615     C                   MOVEA     *ZEROS        TPX
340300010615      *
340400010615     C                   endif
340500010615      *
340600010615     C                   END
340700010615      *
340800010615     C                   ENDSR
340900010615     C*-----------------------------------------
341000010615     C*  REGDET  DETTAGLIO INIZIALE X  R E G I O N E
341100010615     C*-----------------------------------------
341200010615     C     REGDET        BEGSR
341300010615      *
341400010622     C                   Z-ADD     1             K5
341500010615      * cerco il codice tassazione in schiera per TITAD
341600010622     C  N10TADCTS        LOOKUP    CCT(K5)                                35
341700010615      * cerco il codice tassazione in schiera per TITPD
341800010622     C   10TPDCTS        LOOKUP    CCT(K5)                                35
341900010615      *  se non trovato errore
342000010615     C  N35              SETON                                        99
342100010615     C     *IN99         IFEQ      '1'
342200010615     C                   MOVE      MSG(6)        ERRMSG
342300010615     C                   EXSR      STAERR
342400010615      *
342500010615     C                   ELSE
342600010615      *
342700010622     C                   MOVE      RCT(K5)       CODREG
342800010615      * per tariffa DPD e linea partenza cliente non è  DPD regione IT diventa
342900010615      * regione EE
343000020312     c                   if        codreg = 'IT' and *in25 and §ogntw <> 'DPD'
343100010615     C                   MOVEL     'EE'          CODREG
343200010615     C                   ENDIF
343300010615      *
343400010622     C                   Z-ADD     1             K5
343500010622     C     CODREG        LOOKUP    CRE(K5)                                35
343600010615     C  N35              SETON                                        99
343700010615     C     *IN99         IFEQ      '1'
343800010615     C                   MOVE      MSG(7)        ERRMSG
343900010615     C                   EXSR      STAERR
344000010615     C                   ELSE
344100010615      *
344200010622     C                   MOVEL     DRE(K5)       DESREG
344300010615      *  PULIZIA SCHIERE REGIONE
344400010615     C                   MOVE      *BLANKS       BIG
344500010615     C                   MOVE      *BLANKS       BMN
344600010615     C                   MOVE      *BLANKS       BMX
344700010615     C                   MOVE      *BLANKS       BSI
344800010615     C                   MOVE      *BLANKS       BIN
344900010615     C                   CLEAR                   BRG
345000010615     C                   MOVE      *BLANKS       WIN
345100010615     C                   MOVE      *BLANKS       WMN
345200010615     C                   MOVE      *BLANKS       WMX
345300010615     C                   MOVE      *BLANKS       WTI
345400010622      *
345500010615     C                   MOVE      *BLANKS       PDE
345600010615     C                   END
345700010615     C                   END
345800010615      *
345900010615     C                   ENDSR
346000010615      *-------------------------------------------------------------------------
346100010615      *  SR_INOLTR  GESTIONA STAMPE INOLTRO SE PER TUTTI UGUALE
346200010615      *-------------------------------------------------------------------------
346300010615     C     SR_INOLTR     BEGSR
346400010615      *
346500010615      * SE TARIFFE DI INOLTRO UGUALI TRA LORO STAMPO
346600010615    1C     STINO         IFNE      'Y'
346700010615      * SE INOLTRO =0 NON STAMPO NULLA
346800010615     C                   Z-ADD     K4            B
346900010615     C                   DO        6             A
347000010615     C                   MOVE      PRO(B)        COM(A)
347100010615     C                   ADD       1             B
347200010615     C                   END
347300010615     C                   MOVEA     COM           CINO42
347400010615      *
347500010615      * SE INOLTRO > 0
347600010615      *
347700010615    2C                   IF        CINO42 <> *ZEROS
347800010615      *
347900010615     C                   Z-ADD     K4            K
348000010615    3C                   DO        6             A
348100010615     C                   Z-ADD     PRO(K)        PAM(A)
348200010615      * SE TARIFFA A VALORE E IMPORTO VALORIZZATO IMPOSTO %
348300010615     C                   IF        PAM(A) > *ZEROS AND *IN87
348400010615     C                   MOVEL     '%'           AI2(A)
348500010615     C                   END
348600010615     C                   ADD       1             K
348700010615    3C                   END
348800010615      * VALORIZZO I CAMPI DI STAMPA
348900010615     C                   CLEAR                   TA49P03
349000010615      *
349100010615     C     PAM(1)        COMP      0                                  61
349200010615     C     PAM(2)        COMP      0                                  62
349300010615     C     PAM(3)        COMP      0                                  63
349400010615     C     PAM(4)        COMP      0                                  64
349500010622     C     PAM(5)        COMP      0                                  65
349600010622     C     PAM(6)        COMP      0                                  66
349700010615      *
349800010615     C                   Z-ADD     PAM(1)        TAR01
349900010615     C                   Z-ADD     PAM(2)        TAR02
350000010615     C                   Z-ADD     PAM(3)        TAR03
350100010615     C                   Z-ADD     PAM(4)        TAR04
350200010615     C                   Z-ADD     PAM(5)        TAR05
350300010615     C                   Z-ADD     PAM(6)        TAR06
350400010615      *
350500010615      * VALORIZZO LA PARTE DESCRITTIVA
350600060630      * in lingua
350700060630     c                   select
350800060630     c                   when      i45lin = 'I'
350900010615     C                   EVAL      P03DES = 'Per località in provincia spese'  +
351000010615     c                             ' di inoltro'
351100060630     c                   when      i45lin = '2'
351200060630     C                   EVAL      P03DES = 'Per town in province forwarding'  +
351300060630     c                             ' costs'
351400060817     c                   when      i45lin = '3'
351500060817     C                   EVAL      P03DES = 'Pour les communes de la province,'+
351600060817     c                             ' frais d''envoi'
351700060817     c                   when      i45lin = '4'
351800060817     C                   EVAL      P03DES = 'Kosten für Weiterleitung-Ort in' +
351900060817     c                             ' Provinz'
352000060630     c                   endsl
352100010615      *
352200010615      * SE TARIFFA A VALORE STAMPO LA %
352300010615     C                   IF        *IN87
352400010615     C                   MOVE      AI2(1)        PER01
352500010615     C                   MOVE      AI2(2)        PER02
352600010615     C                   MOVE      AI2(3)        PER03
352700010615     C                   MOVE      AI2(4)        PER04
352800010615     C                   MOVE      AI2(5)        PER05
352900010615     C                   MOVE      AI2(6)        PER06
353000010615     C                   ENDIF
353100010625      *
353200010723     c                   if        *in80 or curline > 61
353300010627     c                   write     ta49pfin
353400010625     c                   write     ta49pPP
353500010625     c                   write     ta49p02a
353600010625     c                   write     ta49p02
353700010625     c                   setoff                                       80
353800010625     c                   endif
353900010625      *
354000010615      *
354100010615     C                   WRITE     TA49P03
354200010615      *
354300010615    2C                   endif
354400010615      *
354500010615    1C                   endif
354600010615      *
354700010615     C                   CLEAR                   AI2
354800010615      *
354900010615     C                   ENDSR
355000010615      *-------------------------------------------------------------------------
355100010615      *  SR_ARROTO  GESTIONA STAMPE ARROTONDAMENTI
355200010615      *-------------------------------------------------------------------------
355300010615     C     SR_ARROTO     BEGSR
355400010615      *
355500010615     c                   clear                   ta49p05
355600010615      * verifico gli importi degli arrotondamenti
355700010615      *
355800010621    1c                   if        tamarl > 0
355900010615      * verifico se esistono dei valori con i decimali
356000010615     C                   move      tamarf        w0030
356100010615     C                   eval      *in89 =(w0030 > 0)
356200010615      *
356300010621    2c                   if        not *in89
356400010615     C                   move      tamarl        w0030
356500010615     C                   eval      *in89 =(w0030 > 0)
356600010621    2c                   endif
356700010615      *
356800010621    2c                   if        not *in89
356900010615     C                   move      tamaro        w0030
357000010615     C                   eval      *in89 =(w0030 > 0)
357100010621    2c                   endif
357200010615      *
357300010615      * se 89 spento non esistono importi con decimali sposto in camp di comodo
357400010621    2c                   if        not *in89
357500010615     C                   Z-ADD     TAMARO        WARO2
357600010615     C                   Z-ADD     TAMARF        WARF2
357700010615     C                   Z-ADD     TAMARL        WARL2
357800010621    2c                   endif
357900010615      *
358000010615      * se arrotondamenti di 1 in 1 non stampo
358100010615      * se arrotondamento fino a diverso da arrotondamento oltre a
358200010615      *
358300010628    2c                   if        tamarf <> tamaro
358400010615      *
358500010615      * valorizzo i campi di stampa
358600060606      * in lingua
358700060606     c                   select
358800060606     c                   when      i45lin = 'I'
358900010615     c  N89              eval      p05ar1 = 'fino a ' + %editc(Warl2:'4') +
359000010615     c                                      ' di ' + %editc(warf2:'4')     +
359100010628     c                                      ' in ' + %editc(warf2:'4')
359200010615     c   89              eval      p05ar1 = 'fino a ' + %editc(tamarl:'4') +
359300010628     c                                      ' di ' + %editc(tamarf:'4')     +
359400010628     c                                      ' in ' + %editc(tamarf:'4')
359500060606     c                   when      i45lin = '2'
359600060630     c  n89              eval      p05ar1 = 'Up to ' +
359700060630     c                                      %editc(warl2:'4') + ' rounded ' +
359800060630     c                                      'to the nearest ' +
359900060630     c                                      %editc(warf2:'4')
360000060630     c   89              eval      p05ar1 = 'Up to ' +
360100060704     c                                      %editc(tamarl:'4') + ' rounded ' +
360200060630     c                                      'to the nearest ' +
360300060630     c                                      %editc(tamarf:'4')
360400060817     c                   when      i45lin = '3'
360500060817     c  n89              eval      p05ar1 = 'jusq''à  ' + %editc(Warl2:'4') +
360600060817     c                                      ' de ' + %editc(warf2:'4')     +
360700060817     c                                      ' en ' + %editc(warf2:'4')
360800060817     c   89              eval      p05ar1 = 'jusq''à  ' + %editc(tamarl:'4') +
360900060817     c                                      ' de ' + %editc(tamarf:'4')     +
361000060817     c                                      ' en ' + %editc(tamarf:'4')
361100060817     c                   when      i45lin = '4'
361200060817     c  n89              eval      p05ar1 = 'bis       ' + %editc(warl2:'4')  +
361300060817     c                                      ' von ' + %editc(warf2:'4') +
361400060817     c                                      ' auf ' + %editc(warf2:'4')
361500060817     c   89              eval      p05ar1 = 'bis       ' + %editc(tamarl:'4') +
361600060817     c                                      ' von ' + %editc(tamarf:'4')     +
361700060817     c                                      ' auf ' + %editc(tamarf:'4')
361800060606     c                   endsl
361900010615      *
362000010615      * se arrotondamento oltre a è maggiore di 1
362100010615      *
362200010615      * valorizzo i campi di stampa
362300060606      * in lingua
362400060606     c                   select
362500060606     c                   when      i45lin = 'I'
362600010725     c  N89              eval      p05ar2 = 'oltre  ' + '    '        +
362700010615     c                                      ' di ' + %editc(waro2:'4')     +
362800010628     c                                      ' in ' + %editc(waro2:'4')
362900010723     c   89              eval      p05ar2 = 'oltre  ' + '        '         +
363000010628     c                                      ' di ' + %editc(tamaro:'4')     +
363100010628     c                                      ' in ' + %editc(tamaro:'4')
363200060606     c                   when      i45lin = '2'
363300060704     c  n89              eval      p05ar2 = 'over ' + '     '  +
363400060630     c                                      ' to the nearest ' +
363500060630     c                                      %editc(waro2:'4')
363600060704     c   89              eval      p05ar2 = 'over ' + '     '  +
363700060630     c                                      ' to the nearest ' +
363800060630     c                                      %editc(tamaro:'4')
363900060817     c                   when      i45lin = '3'
364000060818     c  n89              eval      p05ar2 = 'au delà ' + '    '        +
364100060817     c                                      ' de ' + %editc(waro2:'4')     +
364200060817     c                                      ' en ' + %editc(waro2:'4')
364300060818     c   89              eval      p05ar2 = 'au delà ' + '        '         +
364400060817     c                                      ' de ' + %editc(tamaro:'4')     +
364500060817     c                                      ' en ' + %editc(tamaro:'4')
364600060817     c                   when      i45lin = '4'
364700060818     c  n89              eval      p05ar2 = 'mehr als' + '      '        +
364800060817     c                                      ' von ' + %editc(waro2:'4')     +
364900060817     c                                      ' auf ' + %editc(waro2:'4')
365000060818     c   89              eval      p05ar2 = 'mehr als' + '      '         +
365100060817     c                                      ' von ' + %editc(tamaro:'4')     +
365200060817     c                                      ' auf ' + %editc(tamaro:'4')
365300060606     c                   endsl
365400010615      *
365500010615
365600010627     c                   exsr      SR_OVER
365700010625      *
365800010615     c                   write     ta49p05
365900010615
366000010628      *
366100010628     C                   ELSE
366200010625      *
366300010625      * valorizzo i campi di stampa
366400060606      * in lingua
366500060606     c                   select
366600060606     c                   when      i45lin = 'I'
366700010628     c  N89              eval      p05ar1 = ' di ' + %editc(Warf2:'4') +
366800010628     c                                      ' in ' + %editc(waro2:'4')
366900010628     c   89              eval      p05ar1 = ' di ' + %editc(tamarf:'4') +
367000010628     c                                      ' in ' + %editc(tamaro:'4')
367100060606     c                   when      i45lin = '2'
367200060703     c  n89              eval      p05ar1 = 'Rounded to the nearest ' +
367300060606     c                                      %editc(warf2:'4')
367400060703     c   89              eval      p05ar1 = 'Rounded to the nearest ' +
367500060606     c                                      %editc(tamarf:'4')
367600060817     c                   when      i45lin = '3'
367700060817     c  n89              eval      p05ar1 = ' de ' + %editc(Warf2:'4') +
367800060817     c                                      ' en ' + %editc(waro2:'4')
367900060817     c   89              eval      p05ar1 = ' de ' + %editc(tamarf:'4') +
368000060817     c                                      ' en ' + %editc(tamaro:'4')
368100060817     c                   when      i45lin = '4'
368200060817     c  n89              eval      p05ar1 = ' von ' + %editc(Warf2:'4') +
368300060817     c                                      ' auf ' + %editc(waro2:'4')
368400060817     c   89              eval      p05ar1 = ' von ' + %editc(tamarf:'4') +
368500060817     c                                      ' auf ' + %editc(tamaro:'4')
368600060606     c                   endsl
368700010625
368800010628     c                   exsr      SR_OVER
368900010625      *
369000010628     c                   write     ta49p05
369100010625      *
369200010628    2c                   endif
369300010621    1c                   endif
369400010621      *
369500010615     C                   ENDSR
369600010615      *-------------------------------------------------------------------------
369700010615      *  SR_STARAP  GESTIONA STAMPE RAPPORTO PESO VOLUME
369800010615      *-------------------------------------------------------------------------
369900010615     C     SR_STARAP     BEGSR
370000010615      *
370100010615     c                   clear                   ta49p06
370200010615      *
370300010615      * Stampo solo se diverso da zero
370400010615      *
370500010615     C                   Z-ADD     K4            B
370600010615     C                   DO        6             A                 2 0
370700010615     C                   MOVE      RPV(B)        CO4(A)
370800010615     C                   ADD       1             B
370900010615     C                   END
371000010615     C                   MOVEA     CO4           WRPV
371100010615      *
371200010615      * Se rapporto peso volume uguale a zero scrivo Volume compreso in tariffa
371300010615    1C                   if        wrpv = *ZEROS
371400010625
371500010627     c                   exsr      SR_OVER
371600010625      *
371700010615     C                   write     ta49p06b
371800010615      *
371900010615     C                   ELSE
372000010615      * carico la schiera da stampare
372100010615     C                   CLEAR                   SPV
372200010615     C                   Z-ADD     K4            B
372300010615     C                   DO        6             A
372400010615     C                   MOVE      RPV(B)        SPV(A)
372500010615     C                   ADD       1             B
372600010615     C                   END
372700010615      * Valizzo il campo in stampa
372800010622     c                   eval      volsca = '      '+ %editc(spv(1):'4') +
372900010801     C                                      '         '      +
373000010622     c                                      %editc(spv(2):'4') +
373100010801     C                                      '         '      +
373200010622     c                                      %editc(spv(3):'4') +
373300010801     C                                      '         '      +
373400010622     c                                      %editc(spv(4):'4') +
373500010801     C                                      '         '      +
373600010622     c                                      %editc(spv(5):'4') +
373700010801     C                                      '         '      +
373800010615     c                                      %editc(spv(6):'4')
373900010625      *
374000010723     c                   if        *in80 or curline > 61
374100010627     c                   write     ta49pfin
374200010625     c                   write     ta49pPP
374300010625     c                   write     ta49p02a
374400010625     c                   write     ta49p02
374500010625     c                   setoff                                       80
374600010625     c                   endif
374700010625      *
374800010615     C                   write     ta49p06
374900010615    1C                   ENDIF
375000010615      *
375100010615      *
375200010615     C                   ENDSR
375300010615     C*
375400010618      *-------------------------------------------------------------------------
375500010618      *  SR_STATAM  STAMPA I DATI CHE SI TROVANO IN TESTATA TARIFFA
375600010618      *-------------------------------------------------------------------------
375700010618     C     SR_STATAM     BEGSR
375800010618      *
375900010618      * se offerta valorizzo con il numero della visita
376000010621    1c                   if        *in98
376100010618     c                   eval      p07ksc = visksc
376200010621     c                   movel     tamksc        p07nrv
376300010621     c                   move      tamksc        p07prv
376400091117     c                   if        $trattativa
376500091117     c                   eval      p07ntra = tamksc
376600091117     c                   endif
376700010618      *
376800010618     c                   else
376900010618      * tariffa
377000010618     c                   eval      p07ksc = tamksc
377100010621    1c                   endif
377200010618      *
377300091117     c                   if        $trattativa
377400091117     c                   eval      ptratta =
377500091117     c                             '(codice ' +
377600091117     c                             %editc(tamctr:'X') + '/' +
377700091117     c                             %subst(%editc(tamprg:'X'):3:1) + ')'
377800091117     c                   endif
377900091117     c                   if        not $trattativa
378000091117     c                   eval      ptariffa =
378100091117     c                             '(codice ' +
378200091117     c                             %editc(tamctr:'X') + ')'
378300091117     c                   endif
378400010625
378500010627     c                   exsr      SR_OVER
378600010625      *
378700010618     c                   write     ta49p07
378800010626      * se applicazione tariffa diversa dal normale stampo
378900010626     c                   if        §taf02 = 'S'
379000010626
379100010627     c                   exsr      SR_OVER
379200010626      *
379300010626     c                   write     ta49p07s
379400010626     c                   endif
379500010626      *
379600010627     c                   exsr      SR_OVER
379700010626      *
379800010626     c                   if        §taf02 = 'G'
379900010626     c                   write     ta49p07g
380000010626     c                   endif
380100010618      *
380200010618      * se tariffa DPD stampo alcune diciture fisse solo per queste tariffe
380300010618      *
380400010621    1c                   if        *in25
380500010618      * recupero il peso massimo consentito per la DPD
380600010618     c                   movel     '3I'          cod
380700010618     c                   movel(p)  'DPD'         key
380800010618      *
380900010618     c     tabkey        chain     tabel00f
381000010621    2c                   if        %found(tabel00f)
381100010618     c                   movel     tbluni        ds3idp
381200010618     c                   else
381300010618     c                   clear                   ds3idp
381400010621    2c                   endif
381500010618      * preparo il campo in stampa
381600060630      * in lingua
381700060630     c                   select
381800060630     c                   when      i45lin = 'I'
381900010618     c                   eval      com08a = 'MAX PESO COLLO KG.' +
382000010618     c                             %trim(%editc(§3ipkd:'2')) +
382100011114     c                             '- MAX VOLUME COLLO MC 0,25'
382200010618     c                   eval      com08B = 'La presente tariffa è da intender'+
382300010618     c                             'si a collo sul peso lordo reale (max ' +
382400010618     c                             %trim(%editc(§3ipkd:'2')) +
382500010618     c                             ').Nessun addebito sul volume.'
382600060630     c                   when      i45lin = '2'
382700060630     c                   eval      com08a = 'MAX PARCEL WIGHT' +
382800060630     c                             %trim(%editc(§3ipkd:'2')) + ' KG.' +
382900060817     c                             '- MAX PARCEL VOLUME 0,25 M³'
383000060630     c                   eval      com08B = 'This tariff is understood to be '+
383100060630     c                             'per parcel according to the actual gross '+
383200060630     c                             'weight (max ' +
383300060630     c                             %trim(%editc(§3ipkd:'2')) +
383400060630     c                             ').No charge to volume.'
383500060817     c                   when      i45lin = '3'
383600060817     c                   eval      com08a = 'POIDS MAXI COLIS' +
383700060817     c                             %trim(%editc(§3ipkd:'2')) + ' KG.' +
383800060817     c                             '- VOLUME MAXI COLIS 0,25 M³'
383900060817     c                   eval      com08B = 'Le présent tarif s''entend par '+
384000060817     c                             'colis sur le poids brut réel ('+
384100060817     c                             %trim(%editc(§3ipkd:'2')) +
384200060817     c                             ' maxi).Aucune facturation sur le volume.'
384300060817     c                   when      i45lin = '4'
384400060817     c                   eval      com08a = 'PACKSTÜCK-HÖCHSTGEWICHT KG.' +
384500060817     c                             %trim(%editc(§3ipkd:'2')) +
384600060817     c                             '- MAX. PACKSTÜCKVOLUMEN 0,25 KUBIKMETER'
384700060817     c                   eval      com08B = 'Der vorliegende Preis versteht '+
384800060817     c                             'sich pro Packstück und realem ' +
384900060817     c                             'Bruttogewicht (max.' +
385000060817     c                             %trim(%editc(§3ipkd:'2')) +
385100060817     c                             ').Für das Volumen werden keine Kosten ' +
385200060817     c                             'berechnet.'
385300060630     c                   endsl
385400010625
385500110119     c                   Z-Add     curline       totrig
385600110119     c                   Add       5             totrig
385700110119     c                   ExSr      Sr_Overpiu
385800010625
385900010618     c                   write     ta49p08
386000010621    1c                   endif
386100010618      * validità tariffa
386200010625
386300010627     c                   exsr      SR_OVER
386400010625
386500010626      * verifico se devo stampare "per:" solo se c'è o il numero spedizioni mini
386600010626      * me o massime o se c'è uno dei sui volumi auotmatici
386700010626     c                   if        tamlrc > 0 or tamlas > 0 or tamtin <> ' '
386800010626     c                   seton                                        26
386900010626     c                   endif
387000010618     c                   write     ta49p09
387100010618      * verifico se stampare spedizioni minime e massime
387200010621    1c                   if        tamlrc > 0 or tamlas > 0
387300010618     c                   eval      comlrc = tamlrc
387400010618     c                   eval      comlas = tamlas
387500010625
387600010627     c                   exsr      SR_OVER
387700010625
387800010618     c                   write     ta49p09a
387900010621    1c                   endif
388000010618      * verifico se devo stampare volume automatico
388100010621    1c                   if        tamtin = 'K'
388200010618      * volume automatico a  kg
388300010618     c                   z-add     tamkpm        comkpm
388400010621    1c                   endif
388500010618      *
388600010621    1c                   if        tamtin = 'C'
388700010618      * volume a collo
388800010618     c                   z-add     tamkpm        comckm
388900010621    1c                   endif
389000010618      * stampo volume automatico
389100010621    1c                   if        tamtin <> ' '
389200010625
389300010627     c                   exsr      SR_OVER
389400010625
389500010626    1c                   if        tamtin = 'K'
389600010618     c                   write     ta49p09b
389700010723     c                   write     ta49p09s
389800010626     c                   else
389900010626     c                   write     ta49p09c
390000010723     c                   write     ta49p09s
390100010626     c                   endif
390200010723      * se non c'è volume stampo una riga vuota prima delle note
390300010723     c                   else
390400010723     c                   write     ta49p09s
390500010723      *
390600010621    1c                   endif
390700010618      * stampo le note
390800010622     C                   Z-ADD     1             X
390900010622      *
391000010621    1c                   do        4             x
391100010618     c                   if        note(x) <> *blanks
391200010618     c                   movel     note(x)       comnot
391300010625
391400010627     c                   exsr      SR_OVER
391500010625
391600010618     c                   write     ta49p10
391700010618     c                   endif
391800010621    1c                   enddo
391900010618      * se c'è stampo l'addizionale di gestione
392000010621    1c                   if        tampag > 0
392100060606      * in lingua
392200060606     c                   select
392300060606     c                   when      i45lin = 'I'
392400041129     c                   Eval      comdag = 'Addizionale di gestione -
392500041129     c                             anni precedenti:'
392600060606     c                   when      i45lin = '2'
392700060606     c                   eval      comdag = 'Surcharge for previous -
392800060606     c                             years'' administration:'
392900060817     c                   when      i45lin = '3'
393000060817     c                   eval      comdag = 'Complémentaires de gestion -
393100060817     c                             années précédentes:'
393200060817     c                   when      i45lin = '4'
393300060817     c                   eval      comdag = 'Aufschlag für Bearbeitung -
393400060818     c                             von Vorjahren:'
393500060606     c                   endsl
393600041129     c                   eval      comcag = tampag
393700010626     c                   seton                                        27
393800010621    1c                   endif
393900041129      * se c'è stampo l'addizionale di gestione anno corrente
394000041129    1c                   if        tamppa > 0
394100041129     c                   If        *In27
394200060606      * in lingua
394300060606     c                   select
394400060606     c                   when      i45lin = 'I'
394500041129     c                   Eval      comdag1 = 'Addizionale di gestione -
394600041129     c                             corrente ......:'
394700060606     c                   when      i45lin = '2'
394800060606     c                   eval      comdag1 = 'Surcharge for current -
394900060818     c                             administration........:'
395000060817     c                   when      i45lin = '3'
395100060817     c                   eval      comdag1 = 'Complémentaire de gestion -
395200060818     c                             courante..........:'
395300060817     c                   when      i45lin = '4'
395400060817     c                   eval      comdag1 = 'Aufschlag für Bearbeitung -
395500060817     c                             des laufenden Jahres:'
395600060606     c                   endsl
395700041129     c                   eval      comcag1 = tamppa
395800041129     c                   seton                                        29
395900041129     c                   else
396000060606      * in lingua
396100060606     c                   select
396200060606     c                   when      i45lin = 'I'
396300041129     c                   Eval      comdag = 'Addizionale di gestione -
396400060818     c                             corrente:'
396500060606     c                   when      i45lin = '2'
396600060606     c                   eval      comdag = 'Surcharge for current -
396700060818     c                             administration:'
396800060817     c                   when      i45lin = '3'
396900060817     c                   eval      comdag = 'Complémentaire de gestion -
397000060818     c                             courante:'
397100060817     c                   when      i45lin = '4'
397200060817     c                   eval      comdag = 'Aufschlag für Bearbeitung -
397300060817     c                             des laufenden Jahres:'
397400060606     c                   endsl
397500041129     c                   eval      comcag = tamppa
397600041129     c                   seton                                        27
397700041129    1c                   endif
397800041129    1c                   endif
397900010626      *
398000010626      * stampo commenti prima dele tariffe particolari
398100010626
398200041220     c                   Add       3             curline
398300010627     c                   exsr      SR_OVER
398400010626
398500010626     c                   write     ta49p11
398600031119      * se c'è istat stampo
398700101015     c                   if        tamrct >  0
398800031119      * recupero la data di decorrenza
398900031119      *
399000101015     c     *iso          movel     dia(tamrct)   data_EUR
399100050527     C     *eur          movel     DATA_EUR      dtaist
399200031119      *
399300031119      * controllo se % diversa da 100
399400031119     c                   eval      *in11 = (tampri <> 100)
399500031119      * stampo
399600031119
399700150317     c                   Add       4             curline
399800031119     c                   exsr      SR_OVER
399900031119
400000031119     c                   write     ta49pist
400100031119      *
400200031119     c                   endif
400300021205      * stampo riga vuota
400400021205     c                   write     ta49p09s
400500010618      *
400600010618     c                   ENDSR
400700010618      *
400800010618      *-------------------------------------------------------------------------
400900010618      *  SR_STATPT  STAMPA TARIFFE PARTICOLARI
401000010618      *-------------------------------------------------------------------------
401100010618     C     SR_STATPT     BEGSR
401200010622      * accendo indicatore tariffe particolari
401300010622     c                   seton                                        10
401400021204
401500021204      * Pulisco le sk per stampa tariffe particolari su + righe
401600021204     c                   Clear                   Tpta
401700021204     c                   Clear                   Tptb
401800021204     c                   Clear                   Tptc
401900021204     c                   Clear                   Atp1
402000021204     c                   Clear                   Atpc
402100060307      * pulisco campo di comodo
402200060307     c                   clear                   savapl
402300021204
402400020109      * leggo la schiera ordinata delle tariffe particolari
402500020109      *
402600020109     c                   do        50            j
402700020109      * se valorizzata
402800020109     c                   if        cp(j) <> *blank
402900020109      * aggancio la tabella relativa le tariffe particolari
403000020109     c                   setoff                                       99
403100020109     c                   z-add     1             y
403200020109     c     cp(j)         lookup    ftc(y)                                 31
403300020109      * verifico se è una tariffa particolare da stampare  (Posta no Posta)
403400020109     c                   if        not *in31
403500020109     c                   seton                                        99
403600020109     c                   else
403700020109      *
403800020109     c                   endif
403900020109      *
404000020109     c                   SELECT
404100020109      * se tariffa stampabile (99 spento)
404200020109     c                   when      *in99
404300020109      * se è una tariffa particolare che vien stampata in modo standard
404400020109     c                   when      tst(y) = ' '
404500020109     c                   EXSR      SR_TAPARSTD
404600020109      * se è una tariffa particolare che non vien stampata in modo standard
404700020109     c                   when      tst(y) = 'N'
404800020109      * verifico che tariffa particolare è
404900020109      * elavazione limite risarcibile
405000020109     c                   if        ftc(y) = 'R'
405100020109     C                   EXSR      SR_ELLIRIS
405200020109     c                   endif
405300060301      * mandato assicurativo AC PLUS
405400020109     c                   if        ftc(y) = 'C'
405500020109     C                   EXSR      SR_MANDATO
405600020109     c                   endif
405700060301      * mandato assicurativo AC BASE
405800060301     c                   if        ftc(y) = 'd'
405900060301     c                   exsr      sr_acbase
406000060301     c                   endif
406100060816      * fuel surcharge
406200060816     c                   if        ftc(y) = 'f'
406300060816     c                   exsr      sr_fuel
406400060816     c                   endif
406500020109      *
406600020109     c                   ENDSL
406700020109      *
406800020109     c                   endif
406900020109      *
407000020109     c                   enddo
407100020109      *
407200010618     c                   ENDSR
407300021204
407400021204      *-------------------------------------------------------------------------
407500021204      *  Sr_StaTpt1   Stampa tariffe particolari da sk unica
407600021204      *-------------------------------------------------------------------------
407700021204     c     Sr_StaTpt1    BegSr
407800021204
407900021204     c                   Clear                   Conta
408000021204     c                   Eval      WokTp = *Off
408100021205      ***
408200021204      * Stampo prima le tariffe particolari con solo l'importo
408300021204Do  1c                   Do        50            Atp1
408400021204     c                   If        Tpta(Atp1) = *Blanks
408500021204     c                   Leave
408600021204     c                   EndIf
408700021204     c                   Eval      Woktp = *On
408800021204      * due tariffe per riga
408900021204If  2c                   If        Conta = *Zeros
409000021204     c                   Clear                   Comstda2
409100021204     c                   Clear                   Comstdb2
409200021204     c                   Movel(p)  Tpta(Atp1)    Comstda1
409300021204     c                   Movel(p)  Tptb(Atp1)    Comstdb1
409400021204     c                   Add       1             Conta
409500021204   x2c                   Else
409600021204     c                   Movel(p)  Tpta(Atp1)    Comstda2
409700021204     c                   Movel(p)  Tptb(Atp1)    Comstdb2
409800021204     c                   ExSr      Sr_Over
409900021204     c                   Write     Ta49pstd1
410000021204     c                   Clear                   Conta
410100021204    2c                   EndIf
410200021204
410300021204    1c                   EndDo
410400021204      * Stampo ultima riga non completa
410500021204     c                   If        WokTp = *On and Conta <> *Zeros
410600021204     c                   ExSr      Sr_Over
410700021204     c                   Write     Ta49pstd1
410800021204     c                   EndIf
410900021205      * stampo riga vuota
411000021205     c                   write     ta49p09s
411100021204
411200021204     c                   Eval      WokTp = *Off
411300021205     c                   Z-Add     *Zeros        CntList
411400021205      ***
411500021205      * Stampo per ultime le tariffe particolari "al costo"
411600021204Do  1c                   Do        50            Atpc
411700021205      * Stampo ultima riga
411800021204     c                   If        Tptc(Atpc) = *Blanks
411900021205     c                   Add       1             CntList
412000060703     c                   select
412100060703     c                   when      i45lin = 'I'
412200021205     c                   Eval      Comstdc1 = ': al costo'
412300060703     c                   when      i45lin = '2'
412400060703     c                   Eval      Comstdc1 = ': at cost'
412500060817     c                   when      i45lin = '3'
412600060817     c                   Eval      Comstdc1 = ': au coût'
412700060817     c                   when      i45lin = '4'
412800060817     c                   Eval      Comstdc1 = ':zum Preis'
412900060703     c                   endsl
413000021205     c                   ExSr      StpRiga
413100021204     c                   Leave
413200021204     c                   EndIf
413300021204
413400021205     c                   If        %Len(%Trim(Comstdc)) + 1 +
413500021205     c                             %len(%trim(Tptc(Atpc))) + 1 <= Lungh
413600021205     c                   ExSr      CrtRiga
413700021205     c                   Else
413800021205     c                   Add       1             CntList
413900021205     c                   Clear                   Comstdc1
414000021205     c                   ExSr      StpRiga
414100021205     c                   ExSr      CrtRiga
414200021205     c                   EndIf
414300021204
414400021204    1c                   EndDo
414500021204
414600021204     c                   EndSr
414700021205      *
414800021205      *-------------------------------------------------------------------------
414900021205      * CrtRiga - Routine di creazione riga tar.particolari "al costo"
415000021205      *-------------------------------------------------------------------------
415100021205     c     CrtRiga       BegSr
415200021205
415300021205     c                   If        Comstdc = *Blanks
415400021205     c                   Eval      Comstdc = %Trim(Comstdc) + %Trim(Tptc(Atpc))
415500021205     C                   Else
415600021205     c                   Eval      Comstdc = %Trim(Comstdc) + ','
415700021205     c                                       + %Trim(Tptc(Atpc))
415800021205     c                   EndIf
415900021205
416000021205     c                   EndSr
416100021205      *
416200021205      *-------------------------------------------------------------------------
416300021205      * StpRiga - Routine di stampa riga tar.particolari "al costo"
416400021205      *-------------------------------------------------------------------------
416500021205     c     StpRiga       BegSr
416600021205
416700021205     c                   If        Comstdc <> *Blanks
416800021205     c                   Eval      TxtInput = %trim(Comstdc)
416900021205     c                   If        (Cntlist > 0) and
417000021205     c                             (%len(%Trim(TxtInput)) > 75)
417100021205     c                   ExSr      ExeGiust
417200021205     c                   If        Not %error
417300021205     c                   Eval      Comstdc = TxtGiust
417400021205     c                   Else
417500021205     c                   Eval      Comstdc = TxtInput
417600021205     c                   EndIf
417700021205     c                   Else
417800021205     c                   Eval      Comstdc = TxtInput
417900021205     c                   EndIf
418000021205     c                   ExSr      Sr_Over
418100021205     c                   Write     Ta49pstdc
418200021205     c                   Clear                   Comstdc
418300021205     c                   EndIf
418400021205
418500021205     c                   EndSr
418600021205      *
418700021205      *-------------------------------------------------------------------------
418800021205      * EXEGIUST - Routine di "GIUSTIFICAZIONE" testo
418900021205      *-------------------------------------------------------------------------
419000021205     c     ExeGiust      BegSr
419100021205
419200021205     c                   Call(e)   'XGIUST'
419300021205     c                   Parm                    TxtInput
419400021205     c                   Parm                    Lungh
419500021205     c                   Parm                    TxtGiust
419600021205     c                   Parm                    TxtResto
419700021205
419800021205     c                   EndSr
419900010625      *
420000010625      *-------------------------------------------------------------------------
420100010625      *  SR_STATGC  STAMPA TARIFFE GIACENZA
420200010625      *-------------------------------------------------------------------------
420300010625     C     SR_STATGC     BEGSR
420400010625      *
420500010625     c                   if        *in98
420600010625     c     ds1ktm        chain     tiogc01l                           99
420700010625      *
420800010625     c                   else
420900010625      *
421000010625     c     ds1ktm        chain     titgc01l                           99
421100010625     c                   endif
421200010625      *
421300010625     c                   if        *in99 or tgcatb = 'A'
421400010625     c                   goto      endstatgc
421500010625     c                   endif
421600010625      * rapporto peso volume
421700010625     c                   eval      comgpv = tgcrpv * 100
421800010625      * preaparo i campi in stampa
421900010625     C                   z-add     tgcsgd        comsgd
422000010928     C                   z-add     tgcsgv        comsgv               28
422100010625     C                   z-add     tgcsgp        comsgp
422200010625     C                   z-add     tgcsgr        comsgr
422300010625     C                   z-add     tgcstm        comstm
422400010625     C                   z-add     tgcst1        comst1
422500010625     C                   z-add     tgcsg1        comsg1
422600010625     C                   z-add     tgcst2        comst2
422700010928      *
422800010928     C
422900010807      * se giorni di rientro sono a zero li recupero dalla tabella "2G"
423000010807      * in base al tipo tariffa
423100010807     c                   if        tgcggr = *zeros
423200010807      * tariffa italia
423300010807     c                   if        tamfie = 'I'
423400010807     c                   z-add     §2gggl        tgcggr
423500010807      * se DPD
423600010807     c   25              z-add     §2ggdp        tgcggr
423700010807      *
423800010807     c                   else
423900010807      * tariffa estera
424000010807     c                   z-add     §2ggre        tgcggr
424100010807      *
424200010807     c                   endif
424300010807      *
424400010807     c                   endif
424500010807      *
424600010807     C                   z-add     tgcggr        comggr
424700010625      *
424800010725     c                   if        *in80 or curline > 55
424900010725     c                   write     ta49pfin
425000010725     c                   write     ta49pPP
425100010725     c                   setoff                                       80
425200010725     c                   endif
425300010625      *
425400010625      *
425500010625     c                   write     ta49pgi
425600010625      * verifico se c'è la franchigia
425700010625     c                   if        tgcsgf > 0
425800010625     c                   z-add     tgcsgf        comsgf
425900010625      * verifico il falg applicazione franchigia
426000060703      * e stampo in lingua
426100060703     c                   select
426200060703     c                   when      i45lin = 'I'
426300010625     c                   if        tgcfaf = '1'
426400010625     c                   eval      comapf = cspe1
426500010625     c                   endif
426600010625     c                   if        tgcfaf = '2'
426700010625     c                   eval      comapf = cspe2
426800010625     c                   endif
426900060118     c                   if        tgcfaf = '3'
427000060118     c                   eval      comapf = cspe3
427100060118     c                   endif
427200060118      *
427300060703     c                   when      i45lin = '2'
427400060703     c                   if        tgcfaf = '1'
427500060703     c                   eval      comapf = cspe12
427600060703     c                   endif
427700060703     c                   if        tgcfaf = '2'
427800060703     c                   eval      comapf = cspe22
427900060703     c                   endif
428000060703     c                   if        tgcfaf = '3'
428100060703     c                   eval      comapf = cspe32
428200060703     c                   endif
428300060817      *
428400060817     c                   when      i45lin = '3'
428500060817     c                   if        tgcfaf = '1'
428600060817     c                   eval      comapf = cspe13
428700060817     c                   endif
428800060817     c                   if        tgcfaf = '2'
428900060817     c                   eval      comapf = cspe23
429000060817     c                   endif
429100060817     c                   if        tgcfaf = '3'
429200060817     c                   eval      comapf = cspe33
429300060817     c                   endif
429400060817      *
429500060817     c                   when      i45lin = '4'
429600060817     c                   if        tgcfaf = '1'
429700060817     c                   eval      comapf = cspe14
429800060817     c                   endif
429900060817     c                   if        tgcfaf = '2'
430000060817     c                   eval      comapf = cspe24
430100060817     c                   endif
430200060817     c                   if        tgcfaf = '3'
430300060817     c                   eval      comapf = cspe34
430400060817     c                   endif
430500060703     c                   endsl
430600010625      *
430700010627     c                   exsr      SR_OVER
430800010625      *
430900010625     c                   write     ta49pgif
431000010625      *
431100010625     c                   endif
431200031008
431300031008     c                   Add       2             curline
431400031008     c                   exsr      SR_OVER
431500031008     c                   write     ta49pgi1
431600010625      *
431700010625     c     endstatgc     endsr
431800010625      *
431900010618      *-------------------------------------------------------------------------
432000020109      *  SR_TAPARSTD STAMPA DATI DELLA TARIFFA PARTICOLARE STANDARD
432100010618      *-------------------------------------------------------------------------
432200020109     C     SR_TAPARSTD   BEGSR
432300010618      *
432400061207      * indicatore e campo di comodo per tariffa Fuel a scaglioni
432500061207      * sto stampando le tariffe particolari srandard ruindi pulisco il campo
432600061207      * e spengo l'indicatore
432700061207     c                   clear                   desumf
432800061207     c                   eval      *in12 = *off
432900010618      *
433000020109     C                   movel(P)  ftc(y)        savftc
433100020109     c                   movel(p)  edtc(y)       comstd
433200020109      * se si tratta della tariffa particolare isola accendo
433300020109     c                   if        ftc(y) = 'I'
433400020109     C                   SETON                                        06
433500020109     c                   endif
433600010628      *
433700061122     C  n98KTPT          CHAIN     TITPT01l
433800061122     c   98ktpt          chain     tiopt01l
433900010618      * se non trovato vado a fine senza stampare
434000061122     c                   if        not %found or tptatb = 'A'
434100020109      * verifico se devo stampare al costo
434200020109     c                   if        edac(y) = 'S'
434300021204
434400021204      * se non devo stampare righe aggiuntive salvo la tariffa particolare
434500021204      * in una sk che stamperò alla fine di tutte le tariffe particolari
434600021204If  3c                   If        Esud(y) <> 'S'
434700021204     c                   Add       1             Atpc
434800021204     c                   Movel     Comstd        Tptc(Atpc)
434900021204
435000021204      * se devo stampare righe aggiuntive stampo anche la tariffa particolare
435100021204   x3c                   Else
435200140527      * se sto stampando la tariffa particolare m stampo la seguente
435300140527      * dicitura
435400151203      * Bocchi 3/12/15 mi ha detto di togliere la dicitura per questa tariffa
435500151203      * e lasciarla al costo come le altre. fare suibito ed inviare senza
435600151203      * dire nulla a nessuno
435700151203      * pulisco dalla dicitura GRATIS il programma ma è bastato togliere
435800151203      * il flag di dicitura particolare nella tabella SP tariffa particolare "m"
435900151203      * che non stampa più
436000151203     c****               If        ftc(y) = 'm'
436100151203     c****               select
436200151203     c****               when      i45lin = 'I'
436300151203     c****               eval      comdstd = Cost_m_gratis
436400151203     c****               when      i45lin = '2'
436500151203     c****               eval      comdstd = Cost_m_gratisI
436600151203     c****               when      i45lin = '3'
436700151203     c****               eval      comdstd = Cost_m_gratisF
436800151203     c****               when      i45lin = '4'
436900151203     c****               eval      comdstd = Cost_m_gratisD
437000151203     c****               endsl
437100151203     c****               else
437200060703     c                   select
437300060703     c                   when      i45lin = 'I'
437400020109     c                   eval      comdstd = ': al costo       '
437500060703     c                   when      i45lin = '2'
437600060703     c                   eval      comdstd = ': at cost        '
437700060817     c                   when      i45lin = '3'
437800140528     c                   eval      comdstd = ': au coût        '
437900060817     c                   when      i45lin = '4'
438000140528     c                   eval      comdstd = ': zum Preis      '
438100060703     c                   endsl
438200151203     c*****              endif
438300020109      *
438400020109     c                   exsr      SR_OVER
438500020109      *
438600020109     c                   write     ta49pstd
438700020109      * vado a stampare righe aggiuntive se richieste
438800020109     c                   goto      sudtpstd
438900021204    3c                   EndIf
439000021204
439100020109     c                   else
439200020109      * altrimenti vado a fine e non stampo nulla
439300020109     c                   goto      endtpstd
439400010618     c                   endif
439500010618      *
439600020109     c                   endif
439700010618      * cerco se ha record validi con tariffa o minimo > 0
439800010618     c                   EXSR      RICREC
439900010618      * per 04 acceso stampo nessun addebito
440000010618     c                   if        *in04
440100020109     c                   goto      endtpstd
440200010618     c                   endif
440300010625      *
440400010625      * per 04 spento verifico quanti record dettaglio tariffario esistono
440500010625      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
440600010625     c                   if        recval = 1
440700010625     C                   exsr      sr_gesunico
440800021204
440900021204      * se la tariffa particolare è di 1 riga con solo l'importo
441000021204      * salvo la tariffa particolare in una sk che stamperò alla fine di
441100021204      * tutte le tariffe particolari prima della sk al costo
441200021204If  2c                   If        Esud(y) <> 'S' and
441300021204     c                             W_Tarparest = *Blanks and
441400021204     c                             oltre = *Zeros and TpdMin = *Zeros and
441500021204     c                             TpdMax = *Zeros and TptTpg <> '0' and
441600021204     c                             TptRpv = *Zeros and TpTArl = *Zeros
441700021204     c                   Add       1             Atp1
441800021204     c                   Z-Add     TpdItr        Witr
441900021204     c                   Eval      Tpta(Atp1) = Comstd
442000021204      * no percentuale
442100021204If  3c                   If        W_Per = ' '
442200021204     c                   Eval      Tptb(Atp1) = ': ' +
442300021204     c                                          Wrkdestpg + ' ' +
442400021204     c                                          %Editc(Witr:'3')
442500021204      * sì percentuale
442600021204   x3c                   Else
442700021204     c                   Eval      Tptb(Atp1) = ': ' +
442800021204     c                                          Wrkdestpg + ' ' +
442900021204     c                                          StpPer + ' % '
443000021204    3c                   EndIf
443100021204
443200021204      * se devo stampare righe aggiuntive o l'estensione della tariffa
443300021204      * particolare stampo anche la tariffa particolare
443400021204   x2c                   Else
443500010625      *
443600020109     c                   eval      comdstd = w_tarpar
443700010625      *
443800010627     c                   exsr      SR_OVER
443900010625      *
444000020109     c                   write     ta49pstd
444100010723      *
444200010723     c                   exsr      findet
444300021204    2c                   EndIf
444400010625     c                   else
444500010618      *
444600010618      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
444700010618      *
444800010625     c                   exsr      sr_gestpd
444900010625      *
445000010625     c                   endif
445100020109      *
445200020109     c     sudtpstd      tag
445300020109      *
445400020109      * verifico se devo stampare righe ulteriori
445500020109      *
445600020109     c                   if        esud(y) = 'S'
445700020109     c                   exsr      SR_OVER
445800091211      *
445900091211      * se tariffa particolare Bartolini 10:30
446000091211      *
446100091211     c                   if        ftc(y) = 'h'
446200160623     c                   exsr      RigheH1030
446300100107     c                   Z-Add     curline       totrig
446400100107     c                   Add       6             totrig
446500100107     c                   ExSr      Sr_Overpiu
446600091211     c                   write     ta49psh2
446700091211     c                   endif
446800020109      *
446900091211      * se tariffa particolare Priority
447000020109      *
447100020109     c                   if        ftc(y) = 'E'
447200020109     c                   write     ta49pse2
447300020109     c                   endif
447400020109      *
447500020109      * se tariffa particolare ritiro
447600020109      *
447700020109     c                   if        ftc(y) = 'U'
447800020109     c                   write     ta49por2
447900020109     c                   endif
448000020109      *
448100020109      * se tariffa particolare provvigione vettore
448200020109      *
448300020109     c                   if        ftc(y) = 'V'
448400020109      *
448500060606      * in lingua
448600060606     c                   select
448700060606     c                   when      i45lin = 'I'
448800020109     c                   eval      comint = 'Vettore'
448900060606     c                   when      i45lin = '2'
449000060606     c                   eval      comint = 'Carrier'
449100060817     c                   when      i45lin = '3'
449200060904     c                   eval      comint = ' du Transporteur'
449300060817     c                   when      i45lin = '4'
449400060817     c                   eval      comint = 'Frachtführe'
449500060606     c                   endsl
449600020109     c                   write     ta49ppc1
449700020109      *
449800020109      *  stampo le condizioni di rimborso solo se tipo pagamento che necessita
449900020109      * codice abi cab
450000020109     c                   if        §4uabi = 'S'
450100020109     c                   exsr      SR_OVER
450200020109     c                   write     ta49ppc2
450300020109     C                   endif
450400020109      *
450500020109     c                   endif
450600020109      *
450700020109      * se tariffa particolare provvigione mittente
450800020109      *
450900020109     c                   if        ftc(y) = 'W'
451000060606      * in lingua
451100060606     c                   select
451200060606     c                   when      i45lin = 'I'
451300020109     c                   eval      comint = 'Mittente'
451400060606     c                   when      i45lin = '2'
451500060606     c                   eval      comint = 'Sender'
451600060817     c                   when      i45lin = '3'
451700060904     c                   eval      comint = 'de l''Expéditeur'
451800060817     c                   when      i45lin = '4'
451900060817     c                   eval      comint = 'Absender'
452000060606     c                   endsl
452100020109     c                   write     ta49ppc1
452200020109     c                   endif
452300020109      *
452400020109     c                   endif
452500010618      *
452600020109     c                   setoff                                       06
452700020109      *
452800020109     C     endtpstd      endsr
452900010625      *-------------------------------------------------------------------------
453000010625      *  SR_ELLIRIS STAMPA DATI DELLA TARIFFA PARTICOLARE ELEVAZ.LIMITE RISARC.
453100010625      *-------------------------------------------------------------------------
453200010625     C     SR_ELLIRIS    BEGSR
453300010625      *
453400010625     C                   movel     'R '          savftc
453500010628      *
453600061122     C  n98KTPT          CHAIN     TITPT01l
453700061122     c   98ktpt          chain     tiopt01l
453800010625      * se non trovato vado a fine e non stampo nulla
453900061122     c                   if        not %found
454000010625     c                   goto      endelliris
454100010625     c                   endif
454200010625      * se trovato ma annullato vado a fine  e non stampo nulla
454300010625     c                   if        tptatb = 'A'
454400010625     c                   goto      endelliris
454500010625     c                   endif
454600010627      * se esiste stampo la scritta assicurazioni merci ed accendo indicatore
454700010627      * che mi segnala di aver stampato questa riga per non farlo dopo
454800010627      *
454900010627     c                   exsr      SR_OVER
455000010627      *
455100010627     c                   write     ta49PTAM
455200010627     C                   SETON                                        81
455300010625      * recupero i valori standard dalla tabella GEI
455400010625     C                   CLEAR                   DGEI
455500010625     C                   MOVEL     'C'           T02MOD
455600010625     C                   MOVEL     KNSIF         T02SIF
455700010625     C                   MOVEL     'GEI'         T02COD
455800011001      *
455900010625     C                   MOVEL     §TADIV        T02KE1
456000010625      *
456100010625     C                   CALL      'TIBS02R'
456200010625     C                   PARM                    KPJBA
456300160623     C                   PARM                    tibs02ds
456400010625      *
456500010625     C     T02ERR        IFEQ      *BLANKS
456600010625     C                   MOVEL     T02UNI        DGEI
456700010625     C                   ENDIF
456800010625     C*
456900010625      * verifico se limite risarcibile > di quello standard
457000010625     c                   if        tptvlm > §gelrp
457100010625     c                   seton                                        22
457200010625     c                   z-add     tptvlm        comelr
457300010625     c                   z-add     §gelrp        comlrp
457400010625      *
457500010625      * decodifico il flag valore merce
457600010625      *
457700010625     C                   movel     'TR'          COD
457800010625     C                   movel(P)  tptfvm        KEY
457900060605     c     tabkeyl       chain     tabel00f
458000010625      *
458100010625     c                   if        %found(tabel00f)
458200010625     c                   movel     TBLUNI        DSTR
458300010625     c                   else
458400010625     c                   clear                   DSTR
458500010625     c                   endif
458600010625      *
458700010625     c                   endif
458800010625      * validità mandato per le spedizioni in partenza e/o in arrivo
458900010625     c                   if        tptapl = ' '
459000060606      * in lingua
459100060606     c                   select
459200060606     c                   when      i45lin = 'I'
459300010625     c                   eval      comvma = 'PARTENZA e in ARRIVO'
459400060606     c                   when      i45lin = '2'
459500060703     c                   eval      comvma = 'DEPARTING and ARRIVING'
459600060817     c                   when      i45lin = '3'
459700060817     c                   eval      comvma = 'DÉPART ET ARRIVÉE'
459800060817     c                   when      i45lin = '4'
459900060817     c                   eval      comvma = 'ABFAHRT und EINGANG'
460000060606     c                   endsl
460100010625     c                   endif
460200010625      * partenza
460300010625     c                   if        tptapl = 'P'
460400060606      * in lingua
460500060606     c                   select
460600060606     c                   when      i45lin = 'I'
460700010625     c                   eval      comvma = 'PARTENZA'
460800060606     c                   when      i45lin = '2'
460900060703     c                   eval      comvma = 'DEPARTURE'
461000060817     c                   when      i45lin = '3'
461100060817     c                   eval      comvma = 'DÉPART'
461200060817     c                   when      i45lin = '4'
461300060817     c                   eval      comvma = 'ABFAHRT'
461400060606     c                   endsl
461500010625     c                   endif
461600010625      * arrivo
461700010625     c                   if        tptapl = 'A'
461800060606      * in lingua
461900060606     c                   select
462000060606     c                   when      i45lin = 'I'
462100010625     c                   eval      comvma = 'ARRIVO'
462200060606     c                   when      i45lin = '2'
462300060703     c                   eval      comvma = 'ARRIVAL'
462400060817     c                   when      i45lin = '3'
462500060817     c                   eval      comvma = 'ARRIVÉE'
462600060817     c                   when      i45lin = '4'
462700060817     c                   eval      comvma = 'EINGANG'
462800060606     c                   endsl
462900010625     c                   endif
463000010625      *
463100010625      * cerco se ha record validi con tariffa o minimo > 0
463200010625     c                   EXSR      RICREC
463300011001      *
463400011001     c                   if        *in22
463500011001     c                   exsr      SR_OVER
463600011001     c                   write     ta49pel1
463700011009     c                   else
463800011009     c                   exsr      SR_OVER
463900011009     c                   write     ta49pel2
464000011001     c                   endif
464100011001      *
464200010625      * per 04 acceso stampo nessun addebito
464300010625     c                   if        *in04
464400010906      *
464500060704     c                   select
464600060704     c                   when      i45lin = 'I'
464700011001     c                   eval      comma = ': nessun addebito'
464800060704     c                   when      i45lin = '2'
464900060704     c                   eval      comma = ': no charge      '
465000060817     c                   when      i45lin = '3'
465100060817     c                   eval      comma = ': aucun frais    '
465200060817     c                   when      i45lin = '4'
465300060817     c                   eval      comma = ': Keine Kosten   '
465400060704     c                   endsl
465500010625      *
465600010627     c                   exsr      SR_OVER
465700010625      *
465800010625      * elevazione limite risarcibile
465900011001     c                   write     ta49pma
466000010625     c                   write     ta49pvma
466100010625     c                   goto      endelliris
466200010625     c                   endif
466300010626      *
466400010626      * per 04 spento verifico quanti record dettaglio tariffario esistono
466500010626      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
466600010626     c                   if        recval = 1
466700010626     C                   exsr      sr_gesunico
466800011009      * se è tariffa a valore senza l'oltre e con elevazione stampo 1 riga <>
466900011009     c                   if        w_ellimris = 'Y'
467000011001      *
467100011001     c                   exsr      SR_OVER
467200011001      *
467300011001     c                   write     ta49pel
467400011001      *
467500011001     c                   else
467600011001      *
467700011001     c                   eval      comma = w_tarpar
467800010626      *
467900010627     c                   exsr      SR_OVER
468000010626      *
468100011001     c                   write     ta49pma
468200011001      *
468300011001     c                   endif
468400010919      *
468500010919      * stampo eventuale estensione scaglione unico
468600010919      *
468700010919     c                   exsr      sr_estes
468800010723      *
468900010723     c                   exsr      findet
469000010723      *
469100010626     c                   else
469200010625      *
469300010625      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
469400010625      *
469500010625     c                   exsr      sr_gestpd
469600010626      *
469700010626     c                   endif
469800011001      * stampo l'ultima riga
469900011001      *
470000011001     c                   exsr      SR_OVER
470100011001     c                   write     ta49pvma
470200010625      *
470300010625     C     endelliris    endsr
470400010625      *
470500010625      *-------------------------------------------------------------------------
470600010625      *  SR_MANDATO STAMPA DATI DELLA TARIFFA PARTICOLARE ASSICURAZIONE X CONTO
470700010625      *-------------------------------------------------------------------------
470800010625     C     SR_MANDATO    BEGSR
470900010625      *
471000010626     c                   setoff                                       2124
471100020109      *
471200020109     C                   movel     'C '          savftc
471300010625      *
471400010628      *
471500061122     C  n98KTPT          CHAIN     TITPT01l
471600061122     c   98ktpt          chain     tiopt01l
471700010625      * se non trovato vado a fine e non stampo nulla
471800061122     c                   if        not %found
471900010625     c                   goto      endmandato
472000010625     c                   endif
472100010625      * se trovato ma annullato vado a fine  e non stampo nulla
472200010625     c                   if        tptatb = 'A'
472300010625     c                   goto      endmandato
472400010625     c                   endif
472500010627      * se esiste stampo la scritta assicurazioni merci solo se 81 spento
472600010627      * cioè non l'ho già stampata prima
472700010627      *
472800010627     c                   exsr      SR_OVER
472900010627      *
473000010627     c  n81              write     ta49PTAM
473100010627     C  n81              SETON                                        81
473200010625      * verifico se mandato fittizio
473300010625     c                   if        tpttma = 'F'
473400010625     c                   seton                                        24
473500010625     c                   endif
473600010625      * verifico se mandato a valore variabile
473700010625     c                   if        tptvlm = 0
473800010625     c                   seton                                        21
473900010625     c                   endif
474000060307      * mi salvo il tptapl x stampa AC BASE
474100060307     c                   eval      savapl = tptapl
474200060515      * se mandatao fittizio pulisco il tipo applicazione
474300060515     c                   if        *in24
474400060515     c                   clear                   savapl
474500060515     c                   endif
474600010625      * validità mandato per le spedizioni in partenza e/o in arrivo
474700010625     c                   if        tptapl = ' '
474800060606      * in lingua
474900060606     c                   select
475000060606     c                   when      i45lin = 'I'
475100010625     c                   eval      comvma = 'PARTENZA e in ARRIVO'
475200060606     c                   when      i45lin = '2'
475300060703     c                   eval      comvma = 'DEPARTING and ARRIVING'
475400060817     c                   when      i45lin = '3'
475500060817     c                   eval      comvma = 'DÉPART ET ARRIVÉE'
475600060817     c                   when      i45lin = '4'
475700060817     c                   eval      comvma = 'ABFAHRT und EINGANG'
475800060606     c                   endsl
475900010625     c                   endif
476000010625      * partenza
476100010625     c                   if        tptapl = 'P'
476200060606      * in lingua
476300060606     c                   select
476400060606     c                   when      i45lin = 'I'
476500010625     c                   eval      comvma = 'PARTENZA'
476600060606     c                   when      i45lin = '2'
476700060703     c                   eval      comvma = 'DEPARTURE'
476800060817     c                   when      i45lin = '3'
476900060817     c                   eval      comvma = 'DÉPART'
477000060817     c                   when      i45lin = '4'
477100060817     c                   eval      comvma = 'ABFAHRT'
477200060606     c                   endsl
477300010625     c                   endif
477400010625      * arrivo
477500010625     c                   if        tptapl = 'A'
477600060606      * in lingua
477700060606     c                   select
477800060606     c                   when      i45lin = 'I'
477900010625     c                   eval      comvma = 'ARRIVO'
478000060606     c                   when      i45lin = '2'
478100060703     c                   eval      comvma = 'ARRIVAL'
478200060817     c                   when      i45lin = '3'
478300060817     c                   eval      comvma = 'ARRIVÉE'
478400060817     c                   when      i45lin = '4'
478500060817     c                   eval      comvma = 'EINGANG'
478600060606     c                   endsl
478700010625     c                   endif
478800010625      * se non è mandato a valore variabile e non è fittizio
478900010625     c                   if        not *in21 and not *in24
479000010625     c                   z-add     tptvlm        comvlm
479100010625      *
479200010625      * decodifico il flag valore merce
479300010625      *
479400010625     C                   movel     'TR'          COD
479500010625     C                   movel(P)  tptfvm        KEY
479600060605     c     tabkeyl       chain     tabel00f
479700010625      *
479800010625     c                   if        %found(tabel00f)
479900010625     c                   movel     TBLUNI        DSTR
480000010625     c                   else
480100010625     c                   clear                   DSTR
480200010625     c                   endif
480300010625      *
480400010625     C                   movel(P)  §TRDE2        DESFVM
480500010625      *
480600010625     c                   endif
480700010625      *
480800010625      * cerco se ha record validi con tariffa o minimo > 0
480900010625     c                   EXSR      RICREC
481000011001      *
481100011001      * mandato a valore variabile o con valore espresso (no fittizio)
481200011001     c                   if        not *in24
481300011001      * se mandato non a valore variabile
481400011001     c                   if        not *in21
481500011001     c                   write     ta49pma1
481600011001     c                   else
481700011001     c                   write     ta49pmav
481800011001     c                   endif
481900011001      *
482000011001     c                   else
482100011001     c                   write     ta49pmaf1
482200011001     c                   endif
482300010625      * per 04 acceso stampo nessun addebito
482400010625     c                   if        *in04
482500010625      * mandato a valore variabile o con valore espresso (no fittizio)
482600060704     c                   select
482700060704     c                   when      i45lin = 'I'
482800010626     c                   eval      comma = ': nessun addebito'
482900060704     c                   when      i45lin = '2'
483000060704     c                   eval      comma = ': no charge      '
483100060817     c                   when      i45lin = '3'
483200060817     c                   eval      comma = ': aucun frais    '
483300060817     c                   when      i45lin = '4'
483400060817     c                   eval      comma = ': Keine Kosten   '
483500060704     c                   endsl
483600010625      *
483700010627     c                   exsr      SR_OVER
483800011001      *
483900010625     c                   write     ta49pma
484000010625     c                   write     ta49pvma
484100010625     c                   goto      endmandato
484200010625     c                   endif
484300010626      *
484400010626      * per 04 spento verifico quanti record dettaglio tariffario esistono
484500010626      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
484600010626     c                   if        recval = 1
484700010626     C                   exsr      sr_gesunico
484800010626      *
484900010626     c                   eval      comma = w_tarpar
485000010626      *
485100010627     c                   exsr      SR_OVER
485200010626      *
485300010626     c                   write     ta49pma
485400010723      *
485500010723     c                   exsr      findet
485600010723      *
485700010626      *
485800010626     c                   else
485900010625      *
486000010625      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
486100010625      *
486200010625     c                   exsr      sr_gestpd
486300010626      *
486400010626     c                   endif
486500010625     C*
486600010625      * stampo l'ultima riga
486700010625      *
486800010627     c                   exsr      SR_OVER
486900010625     c                   write     ta49pvma
487000010625      *
487100010625      *
487200010625     C     endmandato    endsr
487300010628      *
487400060301      *-------------------------------------------------------------------------
487500060301      *  sr_acbase STAMPA DATI DELLA TARIFFA PARTICOLARE d
487600060301      *-------------------------------------------------------------------------
487700060301     c     sr_acbase     begsr
487800060301
487900060301      * la tariffa ac base non può essere fittizia e non può essere a mandato
488000060301      * variabile
488100060301
488200060301     c                   movel     'd '          savftc
488300060301
488400061122     c  n98ktpt          chain     titpt01l
488500061122     c   98ktpt          chain     tiopt01l
488600060301      * se non trovato vado a fine e non stampo nulla
488700061122     c                   if        not %found
488800060301     c                   goto      endacbase
488900060301     c                   endif
489000060301      * se trovato ma annullato vado a fine  e non stampo nulla
489100060301     c                   if        tptatb = 'A'
489200060301     c                   goto      endacbase
489300060301     c                   endif
489400060301      * se esiste stampo la scritta assicurazioni merci solo se 81 spento
489500060301      * cioè non l'ho già stampata prima
489600060301
489700060301     c                   exsr      SR_OVER
489800060301
489900060301     c  n81              write     ta49PTAM
490000060301     c  n81              seton                                        81
490100060301      * validità mandato per le spedizioni in partenza e/o in arrivo
490200060301     c                   if        tptapl = ' '
490300060606      * in lingua
490400060606     c                   select
490500060606     c                   when      i45lin = 'I'
490600060301     c                   eval      comvma = 'PARTENZA e in ARRIVO'
490700060606     c                   when      i45lin = '2'
490800060703     c                   eval      comvma = 'DEPARTING and ARRIVING'
490900060817     c                   when      i45lin = '3'
491000060817     c                   eval      comvma = 'DÉPART ET ARRIVÉE'
491100060817     c                   when      i45lin = '4'
491200060817     c                   eval      comvma = 'ABFAHRT und EINGANG'
491300060606     c                   endsl
491400060301     c                   endif
491500060301      * partenza
491600060301     c                   if        tptapl = 'P'
491700060606      * in lingua
491800060606     c                   select
491900060606     c                   when      i45lin = 'I'
492000060301     c                   eval      comvma = 'PARTENZA'
492100060606     c                   when      i45lin = '2'
492200060703     c                   eval      comvma = 'DEPARTURE'
492300060817     c                   when      i45lin = '3'
492400060817     c                   eval      comvma = 'DÉPART'
492500060817     c                   when      i45lin = '4'
492600060817     c                   eval      comvma = 'ABFAHRT'
492700060606     c                   endsl
492800060301     c                   endif
492900060301      * arrivo
493000060301     c                   if        tptapl = 'A'
493100060606      * in lingua
493200060606     c                   select
493300060606     c                   when      i45lin = 'I'
493400060301     c                   eval      comvma = 'ARRIVO'
493500060606     c                   when      i45lin = '2'
493600060703     c                   eval      comvma = 'ARRIVAL'
493700060817     c                   when      i45lin = '3'
493800060817     c                   eval      comvma = 'ARRIVÉE'
493900060817     c                   when      i45lin = '4'
494000060817     c                   eval      comvma = 'EINGANG'
494100060606     c                   endsl
494200060301     c                   endif
494300060307      * se AC PLUS valida in partenza stampo ARRIVO su AC BASE
494400060307     c                   if        savapl = 'P'
494500060606      * in lingua
494600060606     c                   select
494700060606     c                   when      i45lin = 'I'
494800060307     c                   eval      comvma = 'ARRIVO'
494900060606     c                   when      i45lin = '2'
495000060703     c                   eval      comvma = 'ARRIVAL'
495100060817     c                   when      i45lin = '3'
495200060817     c                   eval      comvma = 'ARRIVÉE'
495300060817     c                   when      i45lin = '4'
495400060817     c                   eval      comvma = 'EINGANG'
495500060606     c                   endsl
495600060307     c                   endif
495700060307      * se AC PLUS valida in arrivo stampo PARTENZA su AC BASE
495800060307     c                   if        savapl = 'A'
495900060606      * in lingua
496000060606     c                   select
496100060606     c                   when      i45lin = 'I'
496200060307     c                   eval      comvma = 'PARTENZA'
496300060606     c                   when      i45lin = '2'
496400060703     c                   eval      comvma = 'DEPARTURE'
496500060817     c                   when      i45lin = '3'
496600060817     c                   eval      comvma = 'DÉPART'
496700060817     c                   when      i45lin = '4'
496800060817     c                   eval      comvma = 'ABFAHRT'
496900060606     c                   endsl
497000060307     c                   endif
497100060301
497200060301     c                   z-add     tptvlm        comvlm
497300060301      *
497400060301      * decodifico il flag valore merce
497500060301      *
497600060301     c                   movel     'TR'          cod
497700060301     c                   movel(p)  tptfvm        key
497800060605     c     tabkeyl       chain     tabel00f
497900060301
498000060301     c                   if        %found(tabel00f)
498100060301     c                   movel     tbluni        dstr
498200060301     c                   else
498300060301     c                   clear                   dstr
498400060301     c                   endif
498500060301
498600060301     c                   movel(p)  §trde2        desfvm
498700060301
498800060301      * cerco se ha record validi con tariffa o minimo > 0
498900060301     c                   EXSR      RICREC
499000060301     c                   write     ta49pma2
499100060301      * per 04 acceso stampo nessun addebito
499200060301     c                   if        *in04
499300060301      * mandato a valore variabile o con valore espresso (no fittizio)
499400060704     c                   select
499500060704     c                   when      i45lin = 'I'
499600060301     c                   eval      comma = ': nessun addebito'
499700060704     c                   when      i45lin = '2'
499800060704     c                   eval      comma = ': no charge      '
499900060817     c                   when      i45lin = '3'
500000060817     c                   eval      comma = ': aucun frais    '
500100060817     c                   when      i45lin = '4'
500200060817     c                   eval      comma = ': Keine Kosten   '
500300060704     c                   endsl
500400060301      *
500500060301     c                   exsr      SR_OVER
500600060301      *
500700060301     c                   write     ta49pma
500800060301     c                   write     ta49pvma
500900060301     c                   goto      endacbase
501000060301     c                   endif
501100060301      *
501200060301      * per 04 spento verifico quanti record dettaglio tariffario esistono
501300060301      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
501400060301     c                   if        recval = 1
501500060301     C                   exsr      sr_gesunico
501600060301
501700060301     c                   eval      comma = w_tarpar
501800060301
501900060301     c                   exsr      SR_OVER
502000060301
502100060301     c                   write     ta49pma
502200060301
502300060301     c                   exsr      findet
502400060301
502500060301     c                   else
502600060301
502700060301      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
502800060301     c                   exsr      sr_gestpd
502900060301
503000060301     c                   endif
503100060301
503200060301      * stampo l'ultima riga
503300060301     c                   exsr      SR_OVER
503400060301     c                   write     ta49pvma
503500060301      *
503600060301     C     endacbase     endsr
503700060816      *
503800060816      *-------------------------------------------------------------------------
503900060816      *  sr_fuel   STAMPA DATI DELLA TARIFFA PARTICOLARE f
504000060816      *-------------------------------------------------------------------------
504100060816     c     sr_fuel       begsr
504200060816
504300060816     c                   movel     'f '          savftc
504400060816
504500061122     c  n98ktpt          chain     titpt01l
504600061122     c   98ktpt          chain     tiopt01l
504700060816      * se non trovato vado a fine e non stampo nulla
504800061122     c                   if        not %found
504900060816     c                   leavesr
505000060816     c                   endif
505100060816      * se trovato ma annullato vado a fine  e non stampo nulla
505200060816     c                   if        tptatb = 'A'
505300060816     c                   leavesr
505400060816     c                   endif
505500140218
505600140218     c                   eval      dTPT01 = TPTflo
505700140218     c                   eval      compma = §TPTpma
505800140218
505900060816      * cerco se ha record validi con tariffa o minimo > 0
506000060816     c                   exsr      ricrec
506100060816      * per 04 acceso tariffa negata
506200140218     c                   if        *in04 and compma = 0
506300061207      * non devo stampare la tariffa particolare
506400061207     c                   leavesr
506500061207     c                   endif
506600061207      * stampo intestazione
506700061207     c                   exsr      sr_over
506800061207     c                   write     ta49ptf
506900060816      * recupero il prezzo medio del gasolio
507000060816     c                   clear                   pmgpmg
507100060816     c     tptdpb        setll     tipmg01l
507200060816     c                   read      tipmg01l
507300060816      * devo controllare la data del prezzo medio che ho trovato
507400060816      * se la data del prezzo medio è troppo indietro non devo stampare
507500140224      * quindi stampo un errore, anche se il prezzo è a 0
507600140224      * faccio il controllo con 6 gg
507700061212     c                   if        pmgdtd > *zeros
507800060816     c     *iso          move      pmgdtd        data_iso1
507900061212     c                   else
508000061212     c                   eval      data_iso1 = *loval
508100061212     c                   endif
508200060816     c     *iso          move      tptdpb        data_iso2
508300060816     c     data_iso2     subdur    data_iso1     wgio:*d
508400140224     c                   IF        wgio > 6 or pmgpmg = 0
508500140224     c                   eval      errmsg = msg(1)
508600140224     c                   exsr      staerr
508700140224     c                   goto      fine
508800140224     c                   ENDIF
508900060816     c                   eval      comfpmg = pmgpmg
509000060816      * stampo il prezzo medio del gasolio in base alla data indicata
509100060816      * in tariffa
509200060816     c                   exsr      sr_over
509300060816     c                   write     ta49ptf1
509400140219      * per 04 acceso vuol dire che la tariffa è tutta negata quindi
509500140219      * non devo stampare niente.
509600140219     c                   IF        *in04
509700140219     c                   ELSE
509800060816      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
509900060816      * verifico quanti record dettaglio tariffario esistono
510000060816      * se 1 scaglione solo devo stampare una dicitura particolare
510100080925     c                   if        recval = 1 and oltre = 0
510200061207     c                   z-add     tpditr        comfitr
510300060816     c                   exsr      sr_over
510400060816     c                   write     ta49ptf2
510500060816      * se maggiori di 1 gestisco la stampa standard delle tariffe particolari
510600060816      * con più scaglioni
510700060816     c                   else
510800061207     c                   exsr      sr_over
510900061207     c                   write     ta49ptf4
511000061207      * forzo l'accensione dell'indicatore 12 per stampare la dicitura
511100061207      * particolare per la tariffa fuel
511200061207     c                   eval      *in12 = *on
511300061207      * imposto la dicitura particolare
511400070205      * in lingua
511500070205     c                   select
511600070205      * italiano
511700070205     c                   when      i45lin = 'I'
511800080910     c                   eval      desumf = 'Prezzo gasolio'
511900070205      * inglese
512000070205     c                   when      i45lin = '2'
512100140224     c                   eval      desumf = 'Diesel fuel price'
512200070205      * francese
512300070205     c                   when      i45lin = '3'
512400080912     c                   eval      desumf = 'Prix du gazole'
512500070205      * tedesco
512600070205     c                   when      i45lin = '4'
512700080912     c                   eval      desumf = 'Preis für Dieselkraftstoff'
512800070205     c                   endsl
512900061207     c                   exsr      sr_gestpd
513000060816     c                   endif
513100140219     c                   ENDIF
513200140218      * stampo % minimo applicazione
513300140218     c                   IF        compma > 0
513400140224     c                   exsr      sr_over
513500140218     c                   write     ta49ptf5
513600140218     c                   ENDIF
513700060816      * stampo l'ultima riga
513800060816     c                   exsr      sr_over
513900150217      * stampa in maniera diversa in base all'applicazione del VMA
514000150217     c                   IF        §TPTvma = *blanks
514100060816     c                   write     ta49ptf3
514200150217     c                   ELSE
514300150217     c                   write     ta49ptf6
514400150217     c                   ENDIF
514500060816
514600060816     c                   endsr
514700160623
514800160623      /free
514900160623       //--------------------------------------------------------------
515000160623       //?Prepara righe per Tariffa H10:30.
515100160623       //--------------------------------------------------------------
515200160623       BEGSR RigheH1030;
515300160623
515400160623       //?Recupero i limiti dalla tabella LTS
515500160623         clear TIBS02DS;
515600160623         clear dLTS;
515700160623         T02mod = 'C';
515800160623         T02sif = knsif;
515900160623         T02cod = 'LTS';
516000160623         T02ke1 = 'H';
516100160623         TNTBE_RicercaControllo (kpjba:tibs02ds);
516200160623         IF  T02err = *blanks;
516300160623           dLTS = T02uni;
516400160623         ENDIF;
516500160623
516600160623         IF  %subst(%editc(§LTSlmcm:'X'):3:3) > *zeros;
516700160623           Stpvlm = %editc(§LTSlmcm:'4');
516800160623         ELSE;
516900160623           Stpvlm = %subst(%editc(§LTSlmcm:'4'):1:2);
517000160623         ENDIF;
517100160623
517200160623         IF  %subst(%editc(§LTSlpkm:'X'):7:1) > *zeros;
517300160623           Stppkg = %editc(§LTSlpkm:'4');
517400160623         ELSE;
517500160623           Stppkg = %subst(%editc(§LTSlpkm:'4'):1:6);
517600160623         ENDIF;
517700160623
517800160623       //?Preparo le frasi con i limiti da tabella
517900160623         SELECT;
518000160623
518100160623         WHEN  I45lin = 'I';
518200160623         riga1h1030 = 'Il "SERVIZIO 10:30" sarà effettuato +
518300160623         esclusivamente per spedizioni con volume massimo di';
518400160623         riga1h1030 = %trim(riga1h1030) + ' ' + %trim(Stpvlm) +
518500160624         ' mc e di peso massimo';
518600160623
518700160623         riga2h1030 = 'Kg. ' + %trim(Stppkg) + ', destinate +
518800160623         unicamente alle località indicate sul sito Internet +
518900161223         WWW.BRT.IT';
519000160623
519100160623         WHEN  I45lin = '2';
519200160623         riga1h1030 = 'The "10:30 SERVICE" service will be +
519300160623         only done for the shimpents whose maximum volume +
519400160623         corresponds to';
519500160623         riga1h1030 = %trim(riga1h1030) + ' ' + %trim(Stpvlm) +
519600160623         ' cubic metre,';
519700160623
519800160623         riga2h1030 = 'whose maximum wight corresponds +
519900160623         to Kg.';
520000160623         riga2h1030 = %trim(riga2h1030) + ' ' + %trim(Stppkg) +
520100160623         ' and whose delivery place is in the Internet site +
520200161223         WWW.BRT.IT';
520300160623
520400160623         WHEN  I45lin = '3';
520500160623         riga1h1030 = 'Le service "SERVICE 10:30" sera effectué +
520600160623         uniquement pour les envois avec un volume maximum de';
520700160623         riga1h1030 = %trim(riga1h1030) + ' ' + %trim(Stpvlm) +
520800160623         ' mètre cube, ';
520900160623
521000160624         riga2h1030 = 'avec un poids maximum de Kg. ';
521100160623         riga2h1030 = %trim(riga2h1030) + ' ' + %trim(Stppkg) +
521200160623         ' et pour les localités indiquées sur le site Internet +
521300161223         WWW.BRT.IT';
521400160623
521500160623         WHEN  I45lin = '4';
521600160623         riga1h1030 = 'Mit dem "10:30 SERVICE" Dienst können nur +
521700160623         Sendungen mit grösstem Umfang von';
521800160623         riga1h1030 = %trim(riga1h1030) + ' ' + %trim(Stpvlm) +
521900160623         ' Kubikmeter und grösstem Gewicht';
522000160623
522100160623         riga2h1030 = 'von';
522200160623         riga2h1030 = %trim(riga2h1030) + ' ' + %trim(Stppkg) +
522300160623         ' Kg, die nur an die Orten, die auf der BRT Webseite +
522400160623         sind, adressiert sind, geschickt werden.';
522500160623         ENDSL;
522600160623
522700160623       ENDSR;
522800160623      /end-free
522900060301      *
523000010625      *-------------------------------------------------------------------------
523100010625      * SR_GESTPD GESTIONE DEL DETTAGLIO TARIFFARIO UGUALE PER TUTTE LE TAR.PAR.
523200010625      *-------------------------------------------------------------------------
523300010625     c     SR_GESTPD     BEGSR
523400010625      * Decodfico tipo tariffa e scaglioni
523500010625     c                   movel     'TR'          COD
523600010625      *
523700010625     C                   CLEAR                   ATF
523800010625     c                   EXSR      DECTPG
523900010625      * elaboro 6 scaglioni alla volta
524000010625     c                   z-add     1             k4
524100010625      *
524200010625    1c                   dow       k4 <= 18
524300010625     C                   MOVEA     SGLD(K4)      SSLD
524400010625     C                   MOVEA     SGLB(K4)      SSLB
524500010625     C                   MOVEA     SGLA(K4)      SSLA
524600010625      * 1 DECIMALE
524700080901     C                   IF        *IN91 AND NOT *IN37 and not *in38
524800010625     C                   MOVEA     SG1A(K4)      SS1A
524900010625     C                   MOVEA     SG1D(K4)      SS1D
525000010625     C                   ENDIF
525100010625      * 2 DECIMALI
525200080901     C                   IF        *IN91 AND *IN37 and not *in38
525300010625     C                   MOVEA     SG2A(K4)      SS2A
525400010625     C                   MOVEA     SG2D(K4)      SS2D
525500010625     C                   ENDIF
525600080901      * 3 DECIMALI
525700080901     C                   IF        *IN91 AND *IN37 and  *in38
525800080901     C                   MOVEA     SG3A(K4)      SS3A
525900080901     C                   MOVEA     SG3D(K4)      SS3D
526000080901     C                   ENDIF
526100010625      *
526200010625      * CARICO GLI SCAGLIONI PER ELABORARE IL DETTAGLIO
526300010625      *
526400080916     C                   MOVEA     SCP(K4)       SSLP
526500010625     C*  INTESTAZIONE SCAGLIONI
526600080916     C                   Z-ADD     SSLP(1)       W0050             5 0
526700021220     C     W0050         COMP      99999                                  41
526800080916     C     SSLP(1)       COMP      *ZEROS                                 51
526900080916     C                   Z-ADD     SSLP(2)       W0050             5 0
527000010625     C     W0050         COMP      99999                                  42
527100080916     C     SSLP(2)       COMP      *ZEROS                                 52
527200080916     C                   Z-ADD     SSLP(3)       W0050             5 0
527300010625     C     W0050         COMP      99999                                  43
527400080916     C     SSLP(3)       COMP      *ZEROS                                 53
527500080916     C                   Z-ADD     SSLP(4)       W0050             5 0
527600010625     C     W0050         COMP      99999                                  44
527700080916     C     SSLP(4)       COMP      *ZEROS                                 54
527800080916     C                   Z-ADD     SSLP(5)       W0050             5 0
527900010625     C     W0050         COMP      99999                                  45
528000080916     C     SSLP(5)       COMP      *ZEROS                                 55
528100080916     C                   Z-ADD     SSLP(6)       W0050             5 0
528200010625     C     W0050         COMP      99999                                  46
528300080916     C     SSLP(6)       COMP      *ZEROS                                 56
528400010625     C*
528500010625     C* SE IL 41 E' ACCESO PERCHÈ IL PRIMO SCAGLIONE E' UGUALE A TUTTI 999999
528600010627     C* O IL SECONDO SCAGLIONE E' A ZERO
528700010627     C* MA SE NON SI TRATTA + DEL 1° SCAGLIONE IN ASSOLUTO DEVO STAMPARE "OLTRE
528800010625     C* LO SCAGLIONE PRECEDENTE
528900010625     C* QUINDI ACCENDO IL 40 E SPENGO IL 41
529000010625     C*
529100010625     C                   IF        *IN41 AND K4 <> 1
529200010625     C                   SETOFF                                       41
529300010625     C                   SETON                                        40
529400010625     C                   ENDIF
529500010625      *
529600010625     C                   EXSR      DECSCA
529700010625     C* LETTURA DEL DETTAGLIO
529800010625     C                   EXSR      LEGDET
529900010625      *
530000010625      * rielaboro altri 6 scaglioni
530100010625      *
530200010625     c                   add       6             k4
530300010625      * se scaglioni minori o uguali a 18 e scaglione valorizzato
530400010625    2c                   if        k4 <= 18 and
530500010625     c                             scp(k4) <> *zeros
530600010625      * pulisco schiere di comodo per la stampa degli scaglioni
530700010625     c                   clear                   ssld
530800010625     c                   clear                   sslb
530900010625     c                   clear                   ssla
531000010625     c                   clear                   ss1d
531100010625     c                   clear                   ss1a
531200010625     c                   clear                   ss2d
531300010625     c                   clear                   ss2a
531400080901     c                   clear                   ss3d
531500080901     c                   clear                   ss3a
531600080916     c                   clear                   sslp
531700010625      * spengo indicatori che condizionano la stampa
531800010625     c                   setoff                                       404142
531900010625     c                   setoff                                       434445
532000010625     c                   setoff                                       46
532100010725     c                   setoff                                       515253
532200010725     c                   setoff                                       545556
532300010625      *
532400010625     c                   else
532500010625      * non ci sono + scaglioni da stampare
532600010625     c                   eval      k4=19
532700010625     c                   leave
532800010625      *
532900010625    2c                   endif
533000010625      *
533100010625    1c                   enddo
533200010625      *
533300010625     C                   EXSR      FINDET
533400010625      *
533500010625     c                   endsr
533600010625      *-------------------------------------------------------------------------
533700010625      * SR_GESUNICO GESTIONE DETTAGLIO TARIFFARIO CON UNA SOLA RIGA
533800010625      *-------------------------------------------------------------------------
533900010625     c     SR_GESUNICO   BEGSR
534000010625      *
534100010625     c                   clear                   w_tarpar
534200010731     c                   clear                   w_tarparest
534300011001     c                   clear                   w_ellimris
534400010626      * Decodfico tipo pagamento
534500010625     c                   movel     'TR'          COD
534600010625     C                   movel(P)  tpttpg        KEY
534700060605     c     tabkeyl       chain     tabel00f
534800010625      *
534900010625     c                   if        %found(tabel00f)
535000010625     c                   movel     TBLUNI        DSTR
535100010625     c                   else
535200010625     c                   clear                   DSTR
535300010625     c                   endif
535400010625      *
535500010625      * verifico se scaglioni a valore
535600010625     c                   if        §trtap = 'S'
535700010625     c                   eval      W_per = '%'
535800010625     c                   else
535900010625     c                   clear                   W_per
536000010625     c                   endif
536100010626      *
536200010626      * decodifico a mio modo il tipo pagamento
536300010626      *
536400010626     C                   CLEAR                   WRKDESTPG
536500010626      *
536600060606      * in lingua
536700060606     c                   select
536800060817      * italiano
536900060606     c                   when      i45lin = 'I'
537000010626     c                   if        tpttpg = 'T'
537100010626     c                   eval      wrkdestpg = 'SUL TRASPORTO'
537200010626     c                   endif
537300010626
537400010626     c                   if        tpttpg = 'V'
537500010626     c                   eval      wrkdestpg = 'SUL VALORE   '
537600010626     c                   endif
537700010626
537800010626     c                   if        tpttpg = '0'
537900010626     c                   eval      wrkdestpg = 'A Q.LE       '
538000010626     c                   endif
538100010626
538200010626     c                   if        tpttpg = '1'
538300010626     c                   eval      wrkdestpg = 'A COLLO      '
538400010626     c                   endif
538500010626
538600010626     c                   if        tpttpg = '2'
538700010626     c                   eval      wrkdestpg = 'A SPEDIZIONE '
538800010626     c                   endif
538900010626
539000010626     c                   if        tpttpg = '3'
539100010626     c                   eval      wrkdestpg = 'A KG.        '
539200010626     c                   endif
539300010626
539400010626     c                   if        tpttpg = '4'
539500010626     c                   eval      wrkdestpg = 'A VALORE     '
539600010626     c                   endif
539700010626
539800010626     c                   if        tpttpg = '5'
539900010626     c                   eval      wrkdestpg = 'A QUANTITA'' '
540000010626     c                   endif
540100021209      * Tar.particolare Diritto Fatturazione
540200030207     c                   If        TptFtc = '§'
540300021209     c                   Eval      WrkDesTpg = 'A FATTURA   '
540400021209     c                   EndIf
540500021209      * Tar.particolare Gest.bancali a rendere
540600021209     c                   If        TptFtc = '='
540700021209     c                   Eval      WrkDesTpg = 'A BANCALE   '
540800021209     c                   EndIf
540900151117      * Tar.particolare Diritto di chiamata prenotazione ritiro telefonico
541000151117     c                   If        TptFtc = 't'
541100151117     c                   Eval      WrkDesTpg = 'A RITIRO    '
541200151117     c                   EndIf
541300060817
541400060817      * inglese
541500060606     c                   when      i45lin = '2'
541600060606     c                   if        tpttpg = 'T'
541700060703     c                   eval      wrkdestpg = 'ON TRANSPORT '
541800060606     c                   endif
541900060606
542000060606     c                   if        tpttpg = 'V'
542100060703     c                   eval      wrkdestpg = 'ON VALUE     '
542200060606     c                   endif
542300060606
542400060606     c                   if        tpttpg = '0'
542500060606     c                   eval      wrkdestpg = 'PER 100 KG.  '
542600060606     c                   endif
542700060606
542800060606     c                   if        tpttpg = '1'
542900060606     c                   eval      wrkdestpg = 'PER PARCEL   '
543000060606     c                   endif
543100060606
543200060606     c                   if        tpttpg = '2'
543300060606     c                   eval      wrkdestpg = 'PER SHIPMENT '
543400151117     c                   endif
543500060606
543600060606     c                   if        tpttpg = '3'
543700060606     c                   eval      wrkdestpg = 'PER KG.      '
543800060606     c                   endif
543900060606
544000060606     c                   if        tpttpg = '4'
544100060703     c                   eval      wrkdestpg = 'BY VALUE     '
544200060606     c                   endif
544300060606
544400060606     c                   if        tpttpg = '5'
544500060703     c                   eval      wrkdestpg = 'BY QUANTITY  '
544600060606     c                   endif
544700060606      * Tar.particolare Diritto Fatturazione
544800060606     c                   If        TptFtc = '§'
544900060606     c                   Eval      WrkDesTpg = 'PER INVOICE '
545000060606     c                   EndIf
545100060606      * Tar.particolare Gest.bancali a rendere
545200060606     c                   If        TptFtc = '='
545300060606     c                   Eval      WrkDesTpg = 'PER PALLET  '
545400060606     c                   EndIf
545500151117      * Tar.particolare Diritto di chiamata prenotazione ritiro telefonico
545600151117     c                   If        TptFtc = 't'
545700151118     c                   Eval      WrkDesTpg = 'PER PICK-UP '
545800151117     c                   EndIf
545900151117
546000060817
546100060817      * francese
546200060817     c                   when      i45lin = '3'
546300060817     c                   if        tpttpg = 'T'
546400060922     c                   eval      wrkdestpg = 'SUR LE TRANSP'
546500060817     c                   endif
546600060817
546700060817     c                   if        tpttpg = 'V'
546800060817     c                   eval      wrkdestpg = 'SUR LA VALEUR'
546900060817     c                   endif
547000060817
547100060817     c                   if        tpttpg = '0'
547200060817     c                   eval      wrkdestpg = 'AUX 100 KG   '
547300060817     c                   endif
547400060817
547500060817     c                   if        tpttpg = '1'
547600060817     c                   eval      wrkdestpg = 'AU COLIS     '
547700060817     c                   endif
547800060817
547900060817     c                   if        tpttpg = '2'
548000060817     c                   eval      wrkdestpg = 'PAR ENVOI    '
548100060817     c                   endif
548200060817
548300060817     c                   if        tpttpg = '3'
548400060817     c                   eval      wrkdestpg = 'AU KG        '
548500060817     c                   endif
548600060817
548700060817     c                   if        tpttpg = '4'
548800060817     c                   eval      wrkdestpg = 'A LA VALEUR  '
548900060817     c                   endif
549000060817
549100060817     c                   if        tpttpg = '5'
549200060817     c                   eval      wrkdestpg = 'A LA QUANTITÉ'
549300060817     c                   endif
549400060817      * Tar.particolare Diritto Fatturazione
549500060817     c                   If        TptFtc = '§'
549600060817     c                   Eval      WrkDesTpg = 'A FATTURA   '
549700060817     c                   EndIf
549800060817      * Tar.particolare Gest.bancali a rendere
549900060817     c                   If        TptFtc = '='
550000060922     c                   Eval      WrkDesTpg = 'A LA PALETTE'
550100060817     c                   EndIf
550200151117      * Tar.particolare Diritto di chiamata prenotazione ritiro telefonico
550300151117     c                   If        TptFtc = 't'
550400151118     c                   Eval      WrkDesTpg = 'PAR RETRAIT '
550500151117     c                   EndIf
550600060817
550700060817      * tedesco
550800060817     c                   when      i45lin = '4'
550900060817     c                   if        tpttpg = 'T'
551000060817     c                   eval      wrkdestpg = 'AUF TRANSPORT'
551100060817     c                   endif
551200060817
551300060817     c                   if        tpttpg = 'V'
551400060817     c                   eval      wrkdestpg = 'AUF WERT     '
551500060817     c                   endif
551600060817
551700060817     c                   if        tpttpg = '0'
551800060817     c                   eval      wrkdestpg = 'N. ZENTNER   '
551900060817     c                   endif
552000060817
552100060817     c                   if        tpttpg = '1'
552200060817     c                   eval      wrkdestpg = 'N. FRACHTSTÜCK'
552300060817     c                   endif
552400060817
552500060817     c                   if        tpttpg = '2'
552600060817     c                   eval      wrkdestpg = 'N. SENDUNG   '
552700060817     c                   endif
552800060817
552900060817     c                   if        tpttpg = '3'
553000060817     c                   eval      wrkdestpg = 'N. KG        '
553100060817     c                   endif
553200060817
553300060817     c                   if        tpttpg = '4'
553400060817     c                   eval      wrkdestpg = 'N. WERT      '
553500060817     c                   endif
553600060817
553700060817     c                   if        tpttpg = '5'
553800060817     c                   eval      wrkdestpg = 'N. MENGE     '
553900060817     c                   endif
554000060817      * Tar.particolare Diritto Fatturazione
554100060817     c                   If        TptFtc = '§'
554200060817     c                   Eval      WrkDesTpg = 'A FATTURA   '
554300060817     c                   EndIf
554400060817      * Tar.particolare Gest.bancali a rendere
554500060817     c                   If        TptFtc = '='
554600060922     c                   Eval      WrkDesTpg = 'N.PALETTE   '
554700060817     c                   EndIf
554800151117      * Tar.particolare Diritto di chiamata prenotazione ritiro telefonico
554900151117     c                   If        TptFtc = 't'
555000151118     c                   Eval      WrkDesTpg = 'N. BUCHUNG  '
555100151117     c                   EndIf
555200060817
555300060606     c                   endsl
555400010625      *
555500010625      * CERCO IL RECORD VALIDO
555600010625      *
555700010625      *
555800061122     C  n98ktpt          setll     titpd01l
555900061122     c   98ktpt          setll     tiopd01l
556000010625      *
556100010625     c                   do        *hival
556200061122     C  n98ktpt          reade     titpd01l
556300061122     c   98ktpt          reade     tiopd01l
556400010625      *
556500061122     c                   if        %eof
556600010625     c                   leave
556700010625     c                   endif
556800010625      *
556900010625     c                   if        tpdatb = 'A'
557000010625     c                   iter
557100010625     c                   endif
557200010625      *
557300010625      * se tariffa non valorizzata leggo la successiva
557400010625      * importo e minimo =  a zero
557500010625      *
557600010625     C                   IF        tpditr = 0 and tpdmin = 0
557700010625     c                   iter
557800010625     c                   else
557900010723      * se importo con percentuale verifico quanti decimali ha
558000010723     c                   if        W_per = '%'
558100010723     c                   z-add     tpditr        valper
558200010723     C                   eval      stpper = %EDITC(valper : '3')
558300010723      * VERIFICO SE HA DECIMALI VALORIZZATI
558400010723     C                   move      valper        w0030
558500010723     C                   move      w0030         w0020
558600010723     C                   move      w0020         w0010
558700010723      * tolgo gli zeri non significativi dopo la virgola
558800010723     c                   select
558900010723      *
559000010723     C                   when      w0030 = *zeros
559100010723     C                   move      '    '        stpper
559200010723      *
559300010723     c                   when      w0020 = *zeros
559400010723     c                   move      '  '          stpper
559500010723      *
559600010723     c                   when      w0010 = *zero
559700010723     c                   move      ' '           stpper
559800010723      *
559900010723     c                   endsl
560000010723      *
560100010723     c                   endif
560200011001      * se sto stampando l'elevazione limite risarcibile mi devo comportare
560300011009      * diversamente se tariffa a valore  e non è definito OLTRE e c'e
560400011009      * elevazione al limite
560500011001     c                   if        W_per = '%' and savftc = 'R ' and oltre = 0
560600011009     c                             and *in22
560700011001      * non definiscp W_tarpar in quanto gestisco in modo diverso
560800011001      * valorizzo il campo W_ellimris
560900011001     c                   eval      w_ellimris = 'Y'
561000011001      *
561100011001      * se esiste minimo e massimo devo valorizzare l'estensione
561200011001      *
561300011001      * solo minimo
561400011001      *
561500011001     c                   if        tpdmin >  0 and tpdmax = 0
561600060606      * in lingua
561700060606     c                   select
561800060606     c                   when      i45lin = 'I'
561900021205     c                   eval      W_TARPAREST = blk56
562000011001     C                             + ' minimo........' +
562100011001     c                             %editc(tpdmin:'3')
562200060606     c                   when      i45lin = '2'
562300060606     c                   eval      w_tarparest = blk56
562400060606     c                             + ' minimum.......' +
562500060606     c                             %editc(tpdmin:'3')
562600060817     c                   when      i45lin = '3'
562700060817     c                   eval      w_tarparest = blk56
562800060817     c                             + ' minimum.......' +
562900060817     c                             %editc(tpdmin:'3')
563000060817     c                   when      i45lin = '4'
563100060817     c                   eval      w_tarparest = blk56
563200060817     c                             + ' minde.........' +
563300060817     c                             %editc(tpdmin:'3')
563400060606     c                   endsl
563500011001     c                   endif
563600011001      *
563700011001      * solo massimo
563800011001      *
563900011001     c                   if        tpdmin =  0 and tpdmax > 0
564000060606      * in lingua
564100060606     c                   select
564200060606     c                   when      i45lin = 'I'
564300021205     c                   eval      W_TARPAREST = blk56
564400011001     C                             + ' massimo.......' +
564500011001     c                             %editc(tpdmax:'3')
564600060606     c                   when      i45lin = '2'
564700060606     c                   eval      w_tarparest = blk56
564800060606     c                             + ' maximum.......' +
564900060606     c                             %editc(tpdmax:'3')
565000060817     c                   when      i45lin = '3'
565100060817     c                   eval      w_tarparest = blk56
565200060817     c                             + ' maximum.......' +
565300060817     c                             %editc(tpdmax:'3')
565400060817     c                   when      i45lin = '4'
565500060817     c                   eval      w_tarparest = blk56
565600060817     c                             + ' höchste.......' +
565700060817     c                             %editc(tpdmax:'3')
565800060606     c                   endsl
565900011001     c                   endif
566000011001      *
566100011001      * sia mino che massimo
566200011001      *
566300011001     c                   if        tpdmin >  0 and tpdmax > 0
566400060606      * in lingua
566500060606     c                   select
566600060606     c                   when      i45lin = 'I'
566700030318     c                   eval      W_TARPAREST = blk10 + blk10 + blk12
566800060908     C                             + '...... minimo...' +
566900011001     c                             %editc(tpdmin:'3')
567000011009     C                             + ' massimo..' +
567100011001     c                             %editc(tpdmax:'3')
567200060606     c                   when      i45lin = '2'
567300060606     c                   eval      w_tarparest = blk10 + blk10 + blk12
567400060908     c                             + '...... minimum..' +
567500060606     c                             %editc(tpdmin:'3')
567600060606     c                             + ' maximum..' +
567700060606     c                             %editc(tpdmax:'3')
567800060817     c                   when      i45lin = '3'
567900060817     c                   eval      w_tarparest = blk10 + blk10 + blk12
568000060908     c                             + '...... minimum..' +
568100060817     c                             %editc(tpdmin:'3')
568200060817     c                             + ' maximum..' +
568300060817     c                             %editc(tpdmax:'3')
568400060817     c                   when      i45lin = '4'
568500060817     c                   eval      w_tarparest = blk10 + blk10 + blk12
568600060908     c                             + '...... minde....' +
568700060817     c                             %editc(tpdmin:'3')
568800060817     c                             + ' höchste..' +
568900060817     c                             %editc(tpdmax:'3')
569000060606     c                   endsl
569100011001     c                   endif
569200011001     c*
569300011001     c                   leave
569400011001     c                   endif
569500010731      *
569600010731      * verifico se devo stampare OLTRE
569700010731      *
569800010731     c                   if        oltre > 0
569900020114      *
570000020114      * verifico se ci sono decimali valorizzati
570100020114      *
570200020114     c                   clear                   *in88
570300020114     c                   clear                   oltrenodec
570400020114     C                   move      oltre         w0030
570500020114     C                   eval      *in88 =(w0030 > 0)
570600020114      * oltre senza decimali
570700020114     c                   if        not *in88
570800020114     c                   z-add     oltre         oltrenodec
570900020114     c                   endif
571000010731      * controllo se tariffa a q.le
571100010731     c                   if        tpttpg = '0'
571200010731     c                   eval      unmis = 'KG.'
571300010731     c                   else
571400010731     c                   clear                   unmis
571500010731     c                   endif
571600010731      *
571700010731      * stampo nella prima riga il valore e nella riga di estensione se ci sono
571800010731      * stampo il minimo e massimo
571900010731      *
572000010731      * no percentuale
572100010731     c                   if        w_per = ' '
572200060606      * in lingua
572300060606     c                   select
572400060606     c                   when      i45lin = 'I'
572500020114     c   88              eval      W_TARPAR = ': ' + wrkdestpg +
572600010731     c                             'oltre ' + (%editc(oltre:'3'))+
572700030318     c                             ' ' + unmis + blk10 + blk10 + blk12
572800021205     c                                                 + %editc(tpditr:'3')
572900020114      * no decimali
573000020114     c  n88              eval      W_TARPAR = ': ' + wrkdestpg +
573100020114     c                             'oltre ' + (%editc(oltrenodec:'3'))+
573200021205     c                             ' ' + unmis + blk36 + %editc(tpditr:'3')
573300060606
573400060606     c                   when      i45lin = '2'
573500060606     c   88              eval      w_tarpar = ': ' + wrkdestpg +
573600060606     c                             'over  ' + (%editc(oltre:'3'))+
573700060606     c                             ' ' + unmis + blk10 + blk10 + blk12
573800060606     c                                                 + %editc(tpditr:'3')
573900060606     c  n88              eval      w_tarpar = ': ' + wrkdestpg +
574000060606     c                             'over  ' + (%editc(oltrenodec:'3'))+
574100060606     c                             ' ' + unmis + blk36 + %editc(tpditr:'3')
574200060817     c                   when      i45lin = '3'
574300060817     c   88              eval      w_tarpar = ': ' + wrkdestpg +
574400060817     c                             'plus  ' + (%editc(oltre:'3'))+
574500060817     c                             ' ' + unmis + blk10 + blk10 + blk12
574600060817     c                                                 + %editc(tpditr:'3')
574700060817     c  n88              eval      w_tarpar = ': ' + wrkdestpg +
574800060817     c                             'plus  ' + (%editc(oltrenodec:'3'))+
574900060817     c                             ' ' + unmis + blk36 + %editc(tpditr:'3')
575000060817     c                   when      i45lin = '4'
575100060817     c   88              eval      w_tarpar = ': ' + wrkdestpg +
575200060817     c                             'mehrals ' + (%editc(oltre:'3'))+
575300060817     c                             ' ' + unmis + blk10 + blk10 + blk10
575400060817     c                                                 + %editc(tpditr:'3')
575500060817     c  n88              eval      w_tarpar = ': ' + wrkdestpg +
575600060817     c                             'mehrals ' + (%editc(oltrenodec:'3'))+
575700060817     c                             ' ' + unmis + blk36 + %editc(tpditr:'3')
575800060606     c                   endsl
575900010731     c                   else
576000010731      * si percentuale
576100060606      * in lingua
576200060606     c                   select
576300060606     c                   when      i45lin = 'I'
576400020114     c   88              eval      W_TARPAR = ': ' +
576500010731     c                             wrkdestpg + ' ' + stpper + ' % ' +
576600010731     c                             'oltre ' + (%editc(oltre:'3'))+
576700030318     c                             ' ' + unmis + blk21
576800020114      * no decimali
576900020114     c  n88              eval      W_TARPAR = ': ' +
577000020114     c                             wrkdestpg + ' ' + stpper + ' % ' +
577100020114     c                             'oltre ' + (%editc(oltrenodec:'3'))+
577200030318     c                             ' ' + unmis + blk25
577300060606
577400060606     c                   when      i45lin = '2'
577500060606     c   88              eval      w_tarpar = ': ' +
577600060606     c                             wrkdestpg + ' ' + stpper + ' % ' +
577700060606     c                             'over  ' + (%editc(oltre:'3'))+
577800060606     c                             ' ' + unmis + blk21
577900060606     c  n88              eval      w_tarpar = ': ' +
578000060606     c                             wrkdestpg + ' ' + stpper + ' % ' +
578100060606     c                             'over  ' + (%editc(oltrenodec:'3'))+
578200060606     c                             ' ' + unmis + blk25
578300060817     c                   when      i45lin = '3'
578400060817     c   88              eval      w_tarpar = ': ' +
578500060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
578600060817     c                             'plus  ' + (%editc(oltre:'3'))+
578700060817     c                             ' ' + unmis + blk21
578800060817     c  n88              eval      w_tarpar = ': ' +
578900060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
579000060817     c                             'plus  ' + (%editc(oltrenodec:'3'))+
579100060817     c                             ' ' + unmis + blk25
579200060817     c                   when      i45lin = '4'
579300060817     c   88              eval      w_tarpar = ': ' +
579400060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
579500060817     c                             'mehrals ' + (%editc(oltre:'3'))+
579600060817     c                             ' ' + unmis + blk21
579700060817     c  n88              eval      w_tarpar = ': ' +
579800060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
579900060817     c                             'mehrals ' + (%editc(oltrenodec:'3'))+
580000060817     c                             ' ' + unmis + blk25
580100060606     c                   endsl
580200010731     c                   endif
580300010731      *
580400010731      * se esiste minimo e massimo devo valorizzare anche l'estensione
580500010731      * tariffe particolari con l'oltre
580600010731      *
580700010731      * solo minimo
580800010731      *
580900010731     c                   if        tpdmin >  0 and tpdmax = 0
581000060606      * in lingua
581100060606     c                   select
581200060606     c                   when      i45lin = 'I'
581300021205     c                   eval      W_TARPAREST = blk56
581400010731     C                             + ' minimo........' +
581500010731     c                             %editc(tpdmin:'3')
581600060606     c                   when      i45lin = '2'
581700060606     c                   eval      w_tarparest = blk56
581800060606     c                             + ' minimum.......' +
581900060606     c                             %editc(tpdmin:'3')
582000060817     c                   when      i45lin = '3'
582100060817     c                   eval      w_tarparest = blk56
582200060817     c                             + ' minimum.......' +
582300060817     c                             %editc(tpdmin:'3')
582400060817     c                   when      i45lin = '4'
582500060817     c                   eval      w_tarparest = blk56
582600060817     c                             + ' minde.........' +
582700060817     c                             %editc(tpdmin:'3')
582800060606     c                   endsl
582900010731     c                   endif
583000010731      *
583100010731      * solo massimo
583200010731      *
583300010731     c                   if        tpdmin =  0 and tpdmax > 0
583400060606      * in lingua
583500060606     c                   select
583600060606     c                   when      i45lin = 'I'
583700021205     c                   eval      W_TARPAREST = blk56
583800010731     C                             + ' massimo.......' +
583900010731     c                             %editc(tpdmax:'3')
584000060606     c                   when      i45lin = '2'
584100060606     c                   eval      w_tarparest = blk56
584200060606     c                             + ' maximum.......' +
584300060606     c                             %editc(tpdmax:'3')
584400060817     c                   when      i45lin = '3'
584500060817     c                   eval      w_tarparest = blk56
584600060817     c                             + ' maximum.......' +
584700060817     c                             %editc(tpdmax:'3')
584800060817     c                   when      i45lin = '4'
584900060817     c                   eval      w_tarparest = blk56
585000060817     c                             + ' höchste.......' +
585100060817     c                             %editc(tpdmax:'3')
585200060606     c                   endsl
585300010731     c                   endif
585400010731      *
585500010731      * sia mino che massimo
585600010731      *
585700010731     c                   if        tpdmin >  0 and tpdmax > 0
585800060606      * in lingua
585900060606     c                   select
586000060606     c                   when      i45lin = 'I'
586100030318     c                   eval      W_TARPAREST = blk10 + blk10 + blk12
586200060908     C                             + '...... minimo...' +
586300010731     c                             %editc(tpdmin:'3')
586400011009     C                             + ' massimo..' +
586500010731     c                             %editc(tpdmax:'3')
586600060606     c                   when      i45lin = '2'
586700060606     c                   eval      w_tarparest = blk10 + blk10 + blk12
586800060908     C                             + '...... minimum..' +
586900060606     c                             %editc(tpdmin:'3')
587000060606     C                             + ' maximum..' +
587100060606     c                             %editc(tpdmax:'3')
587200060817     c                   when      i45lin = '3'
587300060817     c                   eval      w_tarparest = blk10 + blk10 + blk12
587400060908     C                             + '...... minimum..' +
587500060817     c                             %editc(tpdmin:'3')
587600060817     C                             + ' maximum..' +
587700060817     c                             %editc(tpdmax:'3')
587800060817     c                   when      i45lin = '4'
587900060817     c                   eval      w_tarparest = blk10 + blk10 + blk12
588000060908     c                             + '...... minde....' +
588100060817     c                             %editc(tpdmin:'3')
588200060817     c                             + ' höchste..' +
588300060817     c                             %editc(tpdmax:'3')
588400060606     c                   endsl
588500010731     c                   endif
588600010731      *
588700010731     c                   else
588800010731      *
588900010731     c
589000010723      * valorizzo il campo che conterrà la percetuale
589100010625      * se non c'è ne minimo ne massimo
589200010625     c                   if        tpdmin = 0 and tpdmax =  0
589300010723      * no percentuale
589400010723     c                   if        w_per = ' '
589500010626     c                   eval      W_TARPAR = ': ' + wrkdestpg +
589600021205     c                             blk56 + %editc(tpditr:'3')
589700010723     c                   else
589800010723      * si percentuale
589900010723     c                   eval      W_TARPAR = ': ' +
590000030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
590100030318     c                             blk10 + blk10 + blk25
590200010723     c                   endif
590300010723      *
590400010625     c                   endif
590500010625      * se c'è minimo
590600010625     c                   if        tpdmin > 0 and tpdmax = 0
590700010723      * no percentuale
590800010723     c                   if        w_per = ' '
590900060606      * in lingua
591000060606     c                   select
591100060606     c                   when      i45lin = 'I'
591200010626     c                   eval      W_TARPAR = ': ' + wrkdestpg +
591300030318     c                             blk10 + blk10 + blk12
591400060302     c                             + ' ' + %editc(tpditr:'3')
591500010626     C                             + ' minimo...' +
591600010723     c                             %editc(tpdmin:'3')
591700060606     c                   when      i45lin = '2'
591800060606     c                   eval      w_tarpar = ': ' + wrkdestpg +
591900060606     c                             blk10 + blk10 + blk12
592000060606     c                             + ' ' + %editc(tpditr:'3')
592100060606     C                             + ' minimum..' +
592200060606     c                             %editc(tpdmin:'3')
592300060817     c                   when      i45lin = '3'
592400060817     c                   eval      w_tarpar = ': ' + wrkdestpg +
592500060817     c                             blk10 + blk10 + blk12
592600060817     c                             + ' ' + %editc(tpditr:'3')
592700060817     C                             + ' minimum..' +
592800060817     c                             %editc(tpdmin:'3')
592900060817     c                   when      i45lin = '4'
593000060817     c                   eval      w_tarpar = ': ' + wrkdestpg +
593100060817     c                             blk10 + blk10 + blk12
593200060817     c                             + ' ' + %editc(tpditr:'3')
593300060817     c                             + ' minde....' +
593400060817     c                             %editc(tpdmin:'3')
593500060606     c                   endsl
593600010723     c                   else
593700010723      * si percentuale
593800060606      * in lingua
593900060606     c                   select
594000060606     c                   when      i45lin = 'I'
594100010723     c                   eval      W_TARPAR = ': ' +
594200030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
594300030318     c                             blk10 + blk25
594400010723     C                             + ' minimo...' +
594500010723     c                             %editc(tpdmin:'3')
594600060606     c                   when      i45lin = '2'
594700060606     c                   eval      w_tarpar = ': ' +
594800060606     c                             wrkdestpg + ' ' + stpper + ' % ' +
594900060606     c                             blk10 + blk25
595000060606     C                             + ' minimum..' +
595100060606     c                             %editc(tpdmin:'3')
595200060817     c                   when      i45lin = '3'
595300060817     c                   eval      w_tarpar = ': ' +
595400060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
595500060817     c                             blk10 + blk25
595600060817     c                             + ' minimum..' +
595700060817     c                             %editc(tpdmin:'3')
595800060817     c                   when      i45lin = '4'
595900060817     c                   eval      w_tarpar = ': ' +
596000060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
596100060817     c                             blk10 + blk25
596200060817     c                             + ' minde....' +
596300060817     c                             %editc(tpdmin:'3')
596400060606     c                   endsl
596500010723     c                   endif
596600010723      *
596700010625     c                   endif
596800010625      * se c'è solo massimo
596900010625     c                   if        tpdmin = 0 and tpdmax > 0
597000010723      * no percentuale
597100010723     c                   if        w_per = ' '
597200060606      * in lingua
597300060606     c                   select
597400060606     c                   when      i45lin = 'I'
597500010626     c                   eval      W_TARPAR = ': ' + wrkdestpg +
597600030318     c                             blk10 + blk10 + blk12
597700060302     c                             + ' ' + %editc(tpditr:'3')
597800010626     C                             + ' massimo..' +
597900010626     c                             %editc(tpdmax:'3')
598000060606     c                   when      i45lin = '2'
598100060606     c                   eval      w_tarpar = ': ' + wrkdestpg +
598200060606     c                             blk10 + blk10 + blk12
598300060606     c                             + ' ' + %editc(tpditr:'3')
598400060606     C                             + ' maximum..' +
598500060606     c                             %editc(tpdmax:'3')
598600060817     c                   when      i45lin = '3'
598700060817     c                   eval      w_tarpar = ': ' + wrkdestpg +
598800060817     c                             blk10 + blk10 + blk12
598900060817     c                             + ' ' + %editc(tpditr:'3')
599000060817     c                             + ' maximum..' +
599100060817     c                             %editc(tpdmax:'3')
599200060817     c                   when      i45lin = '4'
599300060817     c                   eval      w_tarpar = ': ' + wrkdestpg +
599400060817     c                             blk10 + blk10 + blk12
599500060817     c                             + ' ' + %editc(tpditr:'3')
599600060817     c                             + ' höchste..' +
599700060817     c                             %editc(tpdmax:'3')
599800060606     c                   endsl
599900010723     c                   else
600000010723      * si percentuale
600100060606      * in lingua
600200060606     c                   select
600300060606     c                   when      i45lin = 'I'
600400010723     c                   eval      W_TARPAR = ': ' +
600500030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
600600030318     c                             blk10 + blk25
600700010723     C                             + ' massimo..' +
600800010723     c                             %editc(tpdmax:'3')
600900060606     c                   when      i45lin = '2'
601000060606     c                   eval      w_tarpar = ': ' +
601100060606     c                             wrkdestpg + ' ' + stpper + ' % ' +
601200060606     c                             blk10 + blk25
601300060606     C                             + ' maximum..' +
601400060606     c                             %editc(tpdmax:'3')
601500060817     c                   when      i45lin = '3'
601600060817     c                   eval      w_tarpar = ': ' +
601700060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
601800060817     c                             blk10 + blk25
601900060817     c                             + ' maximum..' +
602000060817     c                             %editc(tpdmax:'3')
602100060817     c                   when      i45lin = '4'
602200060817     c                   eval      w_tarpar = ': ' +
602300060817     c                             wrkdestpg + ' ' + stpper + ' % ' +
602400060817     c                             blk10 + blk25
602500060817     c                             + ' höchste..' +
602600060817     c                             %editc(tpdmax:'3')
602700060606     c                   endsl
602800010723     c                   endif
602900010723      *
603000010625     c                   endif
603100010625      * se c'è minimo e massimo
603200010625     c                   if        tpdmin > 0 and tpdmax > 0
603300010723      * no percentuale
603400010723     c                   if        w_per = ' '
603500060606      * in lingua
603600060606     c                   select
603700060606     c                   when      i45lin = 'I'
603800010626     c                   eval      W_TARPAR = ': ' + wrkdestpg +
603900021205     c                             ' ' + %editc(tpditr:'3') + blk10
604000060908     c                             + ' minimo....' +
604100010626     c                             %editc(tpdmin:'3')
604200010626     C                             + ' massimo..' +
604300010626     c                             %editc(tpdmax:'3')
604400060606     c                   when      i45lin = '2'
604500060606     c                   eval      w_tarpar = ': ' + wrkdestpg +
604600060606     c                             ' ' + %editc(tpditr:'3') + blk10
604700060908     c                             + ' minimum...' +
604800060606     c                             %editc(tpdmin:'3')
604900060606     c                             + ' maximum..' +
605000060606     c                             %editc(tpdmax:'3')
605100060817     c                   when      i45lin = '3'
605200060817     c                   eval      w_tarpar = ': ' + wrkdestpg +
605300060817     c                             ' ' + %editc(tpditr:'3') + blk10
605400060908     c                             + ' minimum...' +
605500060817     c                             %editc(tpdmin:'3')
605600060817     c                             + ' maximum..' +
605700060817     c                             %editc(tpdmax:'3')
605800060817     c                   when      i45lin = '4'
605900060817     c                   eval      w_tarpar = ': ' + wrkdestpg +
606000060817     c                             ' ' + %editc(tpditr:'3') + blk10
606100060908     c                             + ' minde.....' +
606200060817     c                             %editc(tpdmin:'3')
606300060817     c                             + ' höchste..' +
606400060817     c                             %editc(tpdmax:'3')
606500060606     c                   endsl
606600010723     c                   else
606700010723      * si percentuale
606800060606      * in lingua
606900060606     c                   select
607000060606     c                   when      i45lin = 'I'
607100010723     c                   eval      W_TARPAR = ': ' +
607200030318     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
607300060908     c                             + ' minimo..' +
607400010723     c                             %editc(tpdmin:'3')
607500060908     C                             + ' massimo...' +
607600010723     c                             %editc(tpdmax:'3')
607700060606     c                   when      i45lin = '2'
607800060606     c                   eval      w_tarpar = ': ' +
607900060606     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
608000060908     c                             + ' minimum..' +
608100060606     c                             %editc(tpdmin:'3')
608200060606     C                             + ' maximum..' +
608300060606     c                             %editc(tpdmax:'3')
608400060817     c                   when      i45lin = '3'
608500060817     c                   eval      w_tarpar = ': ' +
608600060817     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
608700060908     c                             + ' minimum..' +
608800060817     c                             %editc(tpdmin:'3')
608900060817     c                             + ' maximum..' +
609000060817     c                             %editc(tpdmax:'3')
609100060817     c                   when      i45lin = '4'
609200060817     c                   eval      w_tarpar = ': ' +
609300060817     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
609400060908     c                             + ' minde....' +
609500060817     c                             %editc(tpdmin:'3')
609600060817     c                             + ' höchste..' +
609700060817     c                             %editc(tpdmax:'3')
609800060606     c                   endsl
609900010723     c                   endif
610000010723      *
610100010625     c                   endif
610200010731      *
610300010731     c                   endif
610400010625     c                   leave
610500010625     c                   endif
610600010625     C
610700010625      *
610800010625     c                   enddo
610900010625      *
611000010625     c                   ENDSR
611100010625      *
611200010618      *-------------------------------------------------------------------------
611300010618      * RICREC  CONTROLLO SE ESISTE UN RECORD VALIDO E CON TARIFFA+MINIMO > 0
611400010618      *-------------------------------------------------------------------------
611500010618     c     RICREC        BEGSR
611600010618      *
611700010618     c                   setoff                                       04
611800010625     c                   z-add     *zeros        recval
611900010626     c                   clear                   wrkftc
612000010626     c                   clear                   wrkorp
612100010626     c                   clear                   wrksgl
612200010806     c                   clear                   recsgl
612300010724     c                   clear                   oltre
612400010724     c                   clear                   mintas
612500010801     c                   clear                   savorp1
612600010731     c                   clear                   W_TARPAREST
612700010618      *
612800061122     C  n98ktpt          setll     titpd01l
612900061122     c   98ktpt          setll     tiopd01l
613000010618      *
613100010618     c                   do        *hival
613200061122     C  n98ktpt          reade     titpd01l
613300061122     c   98ktpt          reade     tiopd01l
613400010618      *
613500061122     c                   if        %eof
613600010801      * se non ho il recval valorizzato accendo lo 04
613700010625     c                   if        recval = 0
613800010618     c                   seton                                        04
613900010625     c                   endif
614000010625      *
614100010618     c                   leave
614200010618     c                   endif
614300010618      *
614400010618     c                   if        tpdatb = 'A'
614500010618     c                   iter
614600010618     c                   endif
614700010618      *
614800010618      * se tariffa non valorizzata leggo la successiva
614900010618      * importo e minimo =  a zero
615000010618      * se valorizzata esco per gestire la stampa del dettaglio tariffario
615100010618      *
615200010618     C                   IF        tpditr = 0 and tpdmin = 0
615300010724      * mi salvo il valore dello scaglione precedente per stampare in un even-
615400010806      * tuale oltre nella riga della tariffa particolare se diverso da 99999
615500010806     c                   z-add     tpdsgl        w0050
615600010806     c                   if        w0050 <> 99999
615700010806      *
615800010724     c                   z-add     tpdsgl        oltre
615900010724      * mi salvo il primo scaglione per stampare il minimo tassabile in caso di
616000010724      * tariffa particolare a qle
616100010724     c                   if        mintas = 0
616200010724     c                   z-add     tpdsgl        mintas
616300010724     c                   endif
616400010806      *
616500010806     c                   endif
616600010801      * mi salvo ordinamento regione e provincia la prima volta
616700010801     c                   if        savorp1 = *blanks
616800010801     c                   eval      savorp1 = tpdorp
616900010806      * memorizzo la chiave che mi serve per riaggancare il primo
617000010806      * record valido
617100010806     c                   eval      wrkorp = tpdorp
617200010806     c                   eval      wrkftc = tpdftc
617300010806     c                   eval      wrksgl = tpdsgl
617400010806      *
617500010806     c                   endif
617600010724      *
617700010618     c                   iter
617800010618     c                   else
617900010625     c                   add       1             recval
618000010806      * memorizzo lo scaglioni perc hè nel caso di unisco scaglione valido
618100010806      * stampo unico scaglione solo se scaglione = a 99999
618200010806     c                   eval      recsgl = tpdsgl
618300010806      * memorizzo la chiave che mi serve per riaggancare il primo
618400010806      * record valido se è = a blanks
618500010806     c                   if        recval = 1 and wrkftc = '  '
618600010806     c                   eval      wrkorp = tpdorp
618700010806     c                   eval      wrkftc = tpdftc
618800010806     c                   eval      wrksgl = tpdsgl
618900010806     c                   endif
619000010626      * se trovati + di 1 record non proseguo in lettura
619100010626      *
619200010626     c                   if        recval > 1
619300010626      * mi riposiziono sul prima record valido trovato
619400061122     c  n98ktpdwrk       chain     titpd01l
619500061122     c   98ktpdwrk       chain     tiopd01l
619600010626      *
619700010626     c                   leave
619800010626     c                   else
619900010625     c                   iter
620000010626     c                   endif
620100010626      *
620200010618     c                   endif
620300010618     C*
620400010618    2C                   enddo
620500010801      * se al primo record valido verifico che non ha lo stesso ordinamento
620600010801      * regione e provincia del record a zero forzo per la stampa in dettaglio
620700010801      *
620800010801     c                   if        recval = 1 and tpdorp <> savorp1 and
620900010801     c                             savorp1 <> *blanks
621000010801     c                   eval      recval = 9
621100010806      * mi riposiziono sul prima record valido trovato
621200061122     c  n98ktpdwrk       chain     titpd01l
621300061122     c   98ktpdwrk       chain     tiopd01l
621400010801     c                   clear                   savorp1
621500010801     c                   endif
621600010724      * Se ho trovato un solo record valido verifico lo scaglione trovato
621700010801     c                   if        recval = 1
621800010806     c                   z-add     recsgl        w0050
621900011012     c                   if        w0050 <> 99999 and tpttpg <> '2'
622000010724      * se diverso da 99999 devo gestire il tutto come se avesse + scaglioni
622100011012      * e tipo tariffa non è a spedizione
622200010724     c                   eval      recval = 9
622300010724      * mi riposiziono sul prima record valido trovato
622400061122     c  n98ktpdwrk       chain     titpd01l
622500061122     c   98ktpdwrk       chain     tiopd01l
622600010724     c                   endif
622700010724     c                   endif
622800010618     C*
622900010618     C                   ENDSR
623000010618      *-------------------------------------------------------------------------
623100010618      *  DECTPG DECODIFICA TIPO TARIFFA E SCAGLIONI
623200010618      *-------------------------------------------------------------------------
623300010618     C     DECTPG        BEGSR
623400010618      *
623500010618     c                   setoff                                       09
623600010618      *
623700010618      * decodifico il tipo tariffa
623800010618      *
623900010618     C                   movel(P)  tpttpg        KEY
624000060605     c     tabkeyl       chain     tabel00f
624100010618      *
624200010618     c                   if        %found(tabel00f)
624300010618     c                   movel     TBLUNI        DSTR
624400010618     c                   else
624500010618     c                   clear                   DSTR
624600010618     c                   endif
624700010618      *
624800010618     C                   movel(P)  §TRDE1        DE1TPG
624900010618     C                   movel(P)  §TRDE3        DE3TPG
625000140522      * minimo tassabile
625100140522     c                   z-add     §trata        ctrata
625200010618      * verifico se scaglioni a valore
625300010618     c                   eval      *in09 = (§trtap = 'S')
625400010618      **
625500010618      * SCAGLIONI
625600010618      *
625700010618      * memorizzo gli scaglioni del dettaglio tariffa particolare
625800010618      *
625900010618     c                   EXSR      MEMSCA
626000010618      *
626100010618     c                   ENDSR
626200010618      *-------------------------------------------------------------------------
626300010618      *  MEMSCA CARICO SCAGLIONI TARIFFE PARTICOLARI
626400010618      *-------------------------------------------------------------------------
626500010618     C     MEMSCA        BEGSR
626600010618      *
626700010621     C                   SETOFF                                       089137
626800080901     c                   setoff                                       404138
626900010618      *
627000010618     C                   MOVEA     *ZEROS        SCP
627100080916     C                   MOVEA     *ZEROS        SSLP
627200010621     c                   clear                   sgld
627300010621     c                   clear                   sg1d
627400010621     c                   clear                   sg2d
627500080901     c                   clear                   sg3d
627600010621     c                   clear                   sgla
627700010621     c                   clear                   sg1a
627800010621     c                   clear                   sg2a
627900080901     c                   clear                   sg3a
628000010618      *
628100010618     c                   eval      rotcts = tpdcts
628200010618     c                   eval      rotpro = %subst(tpdorp:5:1)
628300010618     c                   eval      rotreg = %subst(tpdorp:3:2)
628400010618      * SALVO PER LETTURE FUTURE
628500010618     c                   eval      wrkcts = rotcts
628600010618     c                   eval      wrkpro = rotpro
628700010618     c                   eval      wrkreg = rotreg
628800010618      *
628900010618      * CERCO IL DETTAGLIO TARIFFARIO
629000010618      *
629100061122     C  n98ktpt          setll     titpd01l
629200061122     c   98ktpt          setll     tiopd01l
629300010618      *
629400010621    1c                   do        *hival
629500061122     C  n98ktpt          reade     titpd01l
629600061122     c   98ktpt          reade     tiopd01l
629700010618      *
629800061122    2c                   if        %eof
629900010618     c                   seton                                        04
630000010618     c                   leave
630100010621    2c                   endif
630200010618      *
630300010621    2c                   if        tpdatb = 'A'
630400010618     c                   iter
630500010621    2c                   endif
630600010618      *
630700010618     c                   exsr      regdet
630800010618     c   99              goto      FINE
630900010618     c                   exsr      prodet
631000010618     c   99              goto      FINE
631100010618      *
631200010618      * carico il primo scaglione
631300010618      *
631400010621     C                   z-add     tpdsgl        SCP(1)
631500010621      *
631600010801      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
631700010801      * a valore
631800080901     c                   if        not *in09 or tpdftc = 'f'
631900010801      *
632000010621     c                   move      tpdsgl        w0030
632100010621     c                   z-add     tpdsgl        w0050
632200010621      * se scaglione 99999 non controllo i decimali
632300010621    2c                   if        w0050 <> 99999 and w0030 > 0
632400010621     c                   seton                                        91
632500010621    2c                   endif
632600010621      * se con decimali verifico se valorizzato dal secondo in poi
632700010621    2c                   if        *in91
632800080901     c                   z-add     w0030         w0010
632900010621     c                   z-add     w0030         w0020
633000010621    3c                   if        w0020 > 0
633100010621     c                   seton                                        37
633200010621    3c                   endif
633300080901    3c                   if        w0010 > 0
633400080901     c                   seton                                        38
633500080901    3c                   endif
633600080910      * se si tratta di fuel surcharge faccio sempre 3 decimali
633700080910     c                   If        tpdftc = 'f'
633800080910     c                   seton                                        3837
633900080910    2c                   endif
634000010621    2c                   endif
634100010801      *
634200010801     c                   endif
634300050324
634400050324      * Se scaglione non è 99999
634500050324      * controllo che non sia maggiore di 9999 xchè non ci sta in stampa
634600050324      * in questo caso ERRORE ed esco dal programma
634700050324     c                   z-add     tpdsgl        w0050
634800050324     c                   If        w0050 <> 99999 and tpdsgl > 9999
634900050324     c                   move      msg(16)       errmsg
635000050324     c                   exsr      staerr
635100050324     c                   Goto      FINE
635200050324     c                   EndIf
635300010622      *
635400010622     C                   LEAVE
635500010622      *
635600010622    1C                   ENDDO
635700010622      *
635800010618     C                   z-add     1             K
635900010618      *
636000010618      * LETTURA TPD CON KEY KTPT CON LO STESSO CODICE TASSAZIONE
636100010618      *
636200010621    2C                   DO        *HIVAL
636300010618      *
636400061122     C  n98ktpt          reade     titpd01l
636500061122     c   98ktpt          reade     tiopd01l
636600010618      * se fine lettura
636700061122     C                   IF        %eof
636800010618     c                   leave
636900010618     c                   endif
637000010618      * se codice tassazione diverso
637100010618     c                   if        tpdcts <> rotcts
637200010618     c                   leave
637300010618     c                   endif
637400010618      * se annullato leggo il successivo
637500010618     c                   if        tpdatb <> ' '
637600010618     c                   iter
637700010618     c                   endif
637800010618      *
637900010618     C                   add       1             K
638000010618      * fino a 18 scaglioni carico
638100010621    3C                   if        k <= 18
638200010618      *
638300010621     C                   z-add     tpdsgl        SCP(K)
638400010621      *
638500010801      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
638600010801      * a valore
638700080901     c                   if        not *in09 or tpdftc = 'f'
638800010801      *
638900010621     c                   move      tpdsgl        w0030
639000010621     c                   z-add     tpdsgl        w0050
639100010621      * se scaglione 99999 non controllo i decimali
639200010621     c                   if        w0050 <> 99999 and w0030 > 0
639300010621     c                   seton                                        91
639400010621     c                   endif
639500010621      * se con decimali verifico se valorizzato dal secondo in poi
639600010621     c                   if        *in91
639700010621     c                   z-add     w0030         w0020
639800080901     c                   z-add     w0030         w0010
639900010621     c                   if        w0020 > 0
640000010621     c                   seton                                        37
640100010621     c                   endif
640200080901    3c                   if        w0010 > 0
640300080901     c                   seton                                        38
640400080901    3c                   endif
640500010621     c                   endif
640600010801      *
640700010801     c                   endif
640800050324
640900050324      * Se scaglione non è 99999
641000050324      * controllo che non sia maggiore di 9999 xchè non ci sta in stampa
641100050324      * in questo caso ERRORE ed esco dal programma
641200050324     c                   z-add     tpdsgl        w0050
641300050324     c                   If        w0050 <> 99999 and tpdsgl > 9999
641400050324     c                   move      msg(16)       errmsg
641500050324     c                   exsr      staerr
641600050324     c                   Goto      FINE
641700050324     c                   EndIf
641800010618      *
641900010621   x3C                   ELSE
642000010618      *
642100010618     C                   move      msg(15)       errmsg
642200010618     C                   exsr      staerr
642300050914      * Per errore esco dal pgm
642400050914     c                   Goto      FINE
642500010621    3C                   endif
642600010618      *
642700010621    2C                   enddo
642800010618      * memorizzo il numero scaglioni caricati
642900010618     C                   z-add     k             wnumsc
643000010618      *
643100010621      * preparo la schiera degli scaglioni da stampare
643200010621      *
643300010621     c                   z-add     1             k
643400010621     c                   clear                   k1
643500010621      *
643600010621    2c                   dow       scp(k) > 0
643700010621      * preparo lo scaglione DAL addizionato di 1 o 0,1 o 0,01 in base ai decima
643800010621      * li
643900010621     c                   z-add     scp(k)        w0050
644000010621      *
644100010621      * verifico se scaglione 99999
644200010621    3c                   if        w0050 <> 99999
644300010621      *
644400010621    4c                   if        k1 > 0
644500010621      * se non ha decimali
644600010621     c                   if        not *in91
644700010622     C                   eval      sgld(k) = scp(k1) + 1
644800010621     c                   endif
644900010621      *
645000080901     c                   if        *in91 and not *in37 and not *in38
645100010621      * se ha 1 decimale
645200010622     C                   eval      sg1d(k) = scp(k1) + 0,1
645300010621     c                   endif
645400010621      *
645500080901     c                   if        *in91 and *in37 and not *in38
645600010621      * se ha + di 1 decimale
645700010622     C                   eval      sg2d(k) = scp(k1) + 0,01
645800010621     c                   endif
645900080901      *
646000080901     c                   if        *in91 and *in37 and  *in38
646100080901      * se ha 3 decimale
646200080901     C                   eval      sg3d(k) = scp(k1) + 0,001
646300080901     c                   endif
646400010622      *
646500010622     c                   else
646600010622    4c                   if        k =  1
646700010622      * se non ha decimali
646800010622     c                   if        not *in91
646900010622     C                   eval      sgld(k) =  1
647000010622     c                   endif
647100010622      *
647200080901     c                   if        *in91 and not *in37 and not *in38
647300010622      * se ha 1 decimale
647400010622     C                   eval      sg1d(k) =  0,1
647500010622     c                   endif
647600010622      *
647700080901     c                   if        *in91 and *in37 and not *in38
647800010622      * se ha + di 1 decimale
647900010622     C                   eval      sg2d(k) =  0,01
648000010622     c                   endif
648100080901      *
648200080901     c                   if        *in91 and *in37 and *in38
648300080901      * se ha 3 decimali
648400080901     C                   eval      sg3d(k) =  0,001
648500080901     c                   endif
648600010621      *
648700010622     c                   endif
648800010622      *
648900010621    4c                   endif
649000010621      *
649100010621      * costruisco la schiera dell'applicazione tariffa finita che va sotto gli
649200010621      * scaglioni da/a
649300010621      * tariffa e in mancanza di quest'ultima prendo dalla tabella TR
649400010621      *
649500010621      * muovo unità di misura della tariffa nella schiera
649600060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
649700060925     c                   move      tpttpg        wctr1
649800151117      * chiodo a programma per la nuova tariffa particolare t= Diritto di
649900151117      * chiamata ritiro telefonico l'unità di misura "a ritiro" altrimenti
650000151117      * prenderebbe a spedizione
650100151117     c                   if        tptftc = 't'
650200151117     c                   eval      wctr1= 'R'
650300151117     c                   endif
650400060922     c                   exsr      sr_atfk
650500140522      * faccio i vari ragionamenti in base al valore della tariffa finita
650600140522      *
650700140522      * se scaglioni senza decimali
650800140522     c                   if        not *in91
650900140522
651000140522     c                   z-add     ctrata        wata
651100140522      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
651200140522      * scrivo "A SPED."
651300140522     c                   if        (wata > 0 and sgl(k) <= wata)
651400140522      * richiamo la routine (dove ho tutto fisso e in un unico posto)
651500140522     c                   clear                   wctr1
651600140522     c                   exsr      sr_atfk
651700140522     c                   endif
651800140522      *
651900140522     c                   else
652000140522      * scaglioni con decimali
652100140522     c                   z-add     ctrata        wata1
652200140522     c                   z-add     ctrata        wata2
652300140522      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
652400140522      * scrivo "A SPED."  1 decimale
652500140522     c                   if        (wata1 > 0 and sgl(k)<= wata1 and not *in37)
652600140522      * richiamo la routine (dove ho tutto fisso e in un unico posto)
652700140522     c                   clear                   wctr1
652800140522     c                   exsr      sr_atfk
652900140522     c                   endif
653000140522      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
653100140522      * scrivo "A SPED."  2 decimali
653200140522     c                   if        (wata2 > 0 and sgl(k) <= wata2 and *in37)
653300140522      * richiamo la routine (dove ho tutto fisso e in un unico posto)
653400140522     c                   clear                   wctr1
653500140522     c                   exsr      sr_atfk
653600140522     c                   endif
653700140522      *                                                      *in91 on
653800140522     c                   endif
653900010621      *
654000010621     c                   move      '/'           sglb(k)
654100010621      * preparo lo scaglione AL
654200010621      *
654300010621      * no decimali
654400010621    4c                   if        not *in91
654500010621     c                   z-add     scp(k)        sgla(k)
654600010621    4c                   endif
654700010621      * 1 decimale
654800080901    4c                   if        *in91 and not *in37  and not *in38
654900010621     c                   z-add     scp(k)        sg1a(k)
655000010621    4c                   endif
655100010621      * 2 decimali
655200080901    4c                   if        *in91 and *in37 and not *in38
655300010621     c                   z-add     scp(k)        sg2a(k)
655400010621    4c                   endif
655500080901      * 3 decimali
655600080901    4c                   if        *in91 and *in37 and  *in38
655700080901     c                   z-add     scp(k)        sg3a(k)
655800080901    4c                   endif
655900010621      *
656000010621   x3c                   else
656100010621      * scaglione 99999 è sempre maggiore della tariffa finita
656200060922      * richiamo la routine (dove ho tutto fisso e in un unico posto)
656300060925     c                   move      tpttpg        wctr1
656400151119      * chiodo a programma per la nuova tariffa particolare t= Diritto di
656500151119      * chiamata ritiro telefonico l'unità di misura "a ritiro" altrimenti
656600151119      * prenderebbe a spedizione
656700151119     c                   if        tptftc = 't'
656800151119     c                   eval      wctr1= 'R'
656900151119     c                   endif
657000060922     c                   exsr      sr_atfk
657100010621      *
657200010621    3c                   endif
657300010621      *
657400010621     c                   add       1             k
657500010621     c                   add       1             k1
657600010621      *
657700010621      * se maggiore di 18 esco
657800010621    3c                   if        k > 18
657900010621     c                   leave
658000010621    3c                   end
658100010621      *
658200010621    2c                   enddo
658300010621      *
658400010621      * se c'è solo uno scaglione non scrivo niente sotto
658500021220      * solo se lo scaglione è 99999
658600010621      *
658700021220     c                   if        k1 <= 1 and scp(k1) >= 99999
658800010621     c                   clear                   atf(1)
658900010621     c                   endif
659000010621      *
659100010621     c                   clear                   savsgl
659200010618      *
659300130905     C                   endsr
659400010618     C*
659500010619     C*------------------------------------------------------------------------
659600010619     C*  DECSCA DECODIFICO SCAGLIONI TARIFFE PARTICOLARI
659700010619     C*-------------------------------------------------------------------------
659800010619     C     DECSCA        BEGSR
659900010619      * imposto i campi di rottura salvati
660000010619     C                   MOVEL     WRKREG        ROTREG
660100010619     C                   MOVE      WRKPRO        ROTPRO
660200010619     C                   MOVE      WRKCTS        ROTCTS
660300010619     C                   MOVE      WRKCTS        TPDCTS
660400010619     C                   MOVE      WRKREG        COM04
660500010619     C                   MOVEL     COM04         TPDORP
660600010619     C                   MOVE      WRKPRO        TPDORP
660700010619      * pulisco le schiere
660800010619     C                   EXSR      REGDET
660900010619     C                   EXSR      PRODET
661000010625      *
661100010628      *
661200010628     c                   if        *in80 or curline > 56
661300010628     c                   write     ta49pfin
661400010628     c                   write     ta49pPP
661500010628     c                   setoff                                       80
661600010628     c                   endif
661700010625      *
661800010622
661900010622      * stampo la dicitura della tariffa
662000020109      * se tariffa che viene stampata in modo standard recupero la decodifica
662100020109      * dall'estensione della tabella 1P la tabella SP
662200020109      *
662300020109     c                   if        tst(y) = ' '
662400020109     c                   movel(p)  edtc(y)       comstd
662500020109     c                   clear                   comdstd
662600020109     c                   write     ta49pstd
662700020109      *
662800020109     c                   else
662900020109      * elevazione limite risarcibile
663000010625    3c                   if        savftc = 'R'
663100011001     c                   write     ta49pma
663200010625    3c                   endif
663300020109      * assicurazione per conto
663400010625    3c                   if        savftc = 'C' and not *in21
663500010625     c                   write     ta49pma
663600010625    3c                   endif
663700010625    3c                   if        savftc = 'C' and *in21
663800011001     c                   write     ta49pma
663900010625    3c                   endif
664000020109      *
664100020109     c                   endif
664200010622      * valorizzo i campi del prtf scaglioni
664300010622     c                   clear                   desum
664400010622     c                   clear                   dissca
664500010622     c                   clear                   prtatf
664600010622     c                   eval      desum = de1tpg
664700010622     c                   eval      ds3um = de3tpg
664800151119
664900151119      /free
665000151119       //?Decodifica personalizzata del tipo tariffa
665100151119         SELECT;
665200151119       //?Italiano
665300151119         WHEN  I45lin = 'I';
665400151119       // Tariffa particolare Diritto Fatturazione
665500151119           IF  TPTftc = '§';
665600151119             desum = 'A FATTURA';
665700151119             ds3um = 'A FATTURA';
665800151119           ENDIF;
665900151119       // Tariffa particolare Gestione Bancali a Rendere
666000151119           IF  TPTftc = '=';
666100151119             desum = 'A BANCALE';
666200151119             ds3um = 'A BANCALE';
666300151119           ENDIF;
666400151119       // Tariffa particolare Diritto di chiamata prenotazione
666500151119       // ritiro telefonico
666600151119           IF  TPTftc = 't';
666700151119             desum = 'A RITIRO';
666800151119             ds3um = 'A RITIRO';
666900151119           ENDIF;
667000151119       //?Inglese
667100151119         WHEN  I45lin = '2';
667200151119       // Tariffa particolare Diritto Fatturazione
667300151119           IF  TPTftc = '§';
667400151119             desum = 'PER INVOICE';
667500151119             ds3um = 'PER INVOICE';
667600151119           ENDIF;
667700151119       // Tariffa particolare Gestione Bancali a Rendere
667800151119           IF  TPTftc = '=';
667900151119             desum = 'PER PALLET';
668000151119             ds3um = 'PER PALLET';
668100151119           ENDIF;
668200151119       // Tariffa particolare Diritto di chiamata prenotazione
668300151119       // ritiro telefonico
668400151119           IF  TPTftc = 't';
668500151119             desum = 'PER PICK-UP';
668600151119             ds3um = 'PER PICK-UP';
668700151119           ENDIF;
668800151119       //?Francese
668900151119         WHEN  I45lin = '3';
669000151119       // Tariffa particolare Diritto Fatturazione
669100151119           IF  TPTftc = '§';
669200151119             desum = 'A FATTURA';
669300151119             ds3um = 'A FATTURA';
669400151119           ENDIF;
669500151119       // Tariffa particolare Gestione Bancali a Rendere
669600151119           IF  TPTftc = '=';
669700151119             desum = 'A LA PALLETE';
669800151119             ds3um = 'A LA PALLETE';
669900151119           ENDIF;
670000151119       // Tariffa particolare Diritto di chiamata prenotazione
670100151119       // ritiro telefonico
670200151119           IF  TPTftc = 't';
670300151119             desum = 'PAR RETRAIT';
670400151119             ds3um = 'PAR RETRAIT';
670500151119           ENDIF;
670600151119       //?Tedesco
670700151119         WHEN  I45lin = '4';
670800151119       // Tariffa particolare Diritto Fatturazione
670900151119           IF  TPTftc = '§';
671000151119             desum = 'A FATTURA';
671100151119             ds3um = 'A FATTURA';
671200151119           ENDIF;
671300151119       // Tariffa particolare Gestione Bancali a Rendere
671400151119           IF  TPTftc = '=';
671500151119             desum = 'N.PALETTE';
671600151119             ds3um = 'N.PALETTE';
671700151119           ENDIF;
671800151119       // Tariffa particolare Diritto di chiamata prenotazione
671900151119       // ritiro telefonico
672000151119           IF  TPTftc = 't';
672100151119             desum = 'N. BUCHUNG';
672200151119             ds3um = 'N. BUCHUNG';
672300151119           ENDIF;
672400151119         ENDSL;
672500151119      /end-free
672600151119
672700010622      * stampo la testata
672800010622     c                   EXSR      SR_STATESDET
672900010622      *
673000010622      *
673100010619      *
673200010619     C                   ENDSR
673300010619     O*---------------------------------------------------------------*
673400010619     O* LEGGO DETTAGLIO DI TPD
673500010619     O*---------------------------------------------------------------*
673600010619     C     LEGDET        BEGSR
673700010619      *
673800061122     C  n98KTPT          SETLL     TITPD01l
673900061122     c   98ktpt          setll     tiopd01l
674000010619      *
674100010620    1C                   DO        *HIVAL
674200061122     C  n98KTPT          READE     TITPD01l
674300061122     c   98ktpt          reade     tiopd01l
674400010619      *
674500061122     C                   IF        %EOF
674600010619     C                   LEAVE
674700010619     C                   ENDIF
674800010619      *
674900010619     C                   IF        TPDATB = 'A'
675000010619     C                   ITER
675100010619     C                   ENDIF
675200010619      *
675300010619      * SE SCAGLIONE 9999 VERIFICO SE E' L'ULTIMO
675400010621     C                   Z-ADD     TPDSGL        NUM4
675500010620    2C     NUM4          IFEQ      9999
675600010621     C     TPDSGL        ANDNE     *ALL'9'
675700010619      *
675800010621     C                   MOVEL     TPDORP        SAVORP
675900010621     C                   MOVEL     TPDSGL        SAVSGL
676000010619      *
676100010619     C                   SETOFF                                       35
676200010619      * verifico se esiste un'altro scaglione per lo stesso ORP
676300010620    3C                   DO        *HIVAL
676400061122     C  n98KTPD          READE     TITPD01l                               35
676500061122     c   98ktpd          reade     tiopd01l                               35
676600010619      * se annullato leggo il successivo
676700010619     C                   IF        NOT *IN35 AND TPDATB = 'A'
676800010619     C                   ITER
676900010619     C                   ENDIF
677000010619      * se non trovato o non trovato riaggancio il record precedente
677100061122     C  n98KTPD2         CHAIN     TITPD01l
677200061122     c   98ktpd2         chain     tiopd01l
677300010619      *
677400010619     C                   LEAVE
677500010619      *
677600010620    3C                   ENDDO
677700010619     C*
677800010619     C* NON C'E' ALTRO SCAGLIONE
677900010620    2C                   ENDIF
678000010619     C*
678100010619     C                   Z-ADD     1             K2
678200080916     C                   Z-ADD     TPDSGL        wsgl83
678300010619      * cerco lo scaglione nella schiera
678400080916     C     wsgl83        LOOKUP    SCP(1)                                 35
678500010619      * se non trovato cerco lo scaglione nella schiera stampa
678600080916     C   35wsgl83        LOOKUP    SSLP(K2)                               90
678700010619      * se non trovato nella schiera di sola stampa ma trovato nella schiera SCP
678800010619      * torno a leggere
678900010619     c                   if        *in35 and not *in90
679000010619     c                   iter
679100010619     c                   endif
679200010619      *
679300010619      * se non trovato nella schiera scp ERRORE  scaglione non previsto
679400010620    2C                   IF        not *in35
679500010619     C                   MOVEL     MSG(14)       ERRMSG
679600010619     C                   EXSR      STAERR
679700050914      * Per errore esco dal pgm
679800050914     c                   Goto      FINE
679900010619      *
680000010620   X2C                   ELSE
680100010619     c                   eval      compro = %subst(tpdorp:5:1)
680200010619     c                   eval      comreg = %subst(tpdorp:3:2)
680300010619      * CAMBIATA PROVINCIA
680400010622    3C                   IF        compro <>  rotpro or comreg <> rotreg
680500010619     C                   EXSR      PROTPT
680600010619     C                   EXSR      PRODET
680700010619     C   99              GOTO      FINE
680800010619     C                   MOVE      COMPRO        ROTPRO
680900010620    3C                   END
681000010619      * CAMBIATA REGIONE
681100010620    3C                   IF        COMREG <> ROTREG
681200010619     C                   EXSR      REGTPT
681300010619      *
681400010619     C                   EXSR      REGDET
681500010619     C   99              GOTO      FINE
681600010619     C                   MOVEL     COMREG        ROTREG
681700010620    3C                   END
681800010619      *
681900010619     C                   Z-ADD     TPDITR        TAP(K2)
682000010619     C                   Z-ADD     TPDMIN        TPN(K2)
682100010619     C                   Z-ADD     TPDMAX        TPX(K2)
682200010619     C                   MOVEL     TPDAIN        SAVAIP
682300010619    2C                   END
682400010619      *
682500010619    1C                   ENDDO
682600010620      *
682700010619     C                   EXSR      PROTPT
682800010619     C                   EXSR      REGTPT
682900010621      *
683000010619     C                   ENDSR
683100010620     O*---------------------------------------------------------------*
683200010620     O* PROTPT CAMBIO PROVINCIA
683300010620     O*---------------------------------------------------------------*
683400010620     C     PROTPT        BEGSR
683500010620     C                   Z-ADD     1             C
683600010621     C     *BLANKS       LOOKUP    BIG(C)                                 35
683700010620     C     *IN35         IFEQ      '0'
683800010620     C                   MOVE      MSG(10)       ERRMSG
683900010620     C                   EXSR      STAERR
684000050914      * Per errore esco dal pgm
684100050914     c                   Goto      FINE
684200010620     C                   ELSE
684300010621      *
684400010621     c                   do        6             b
684500010621     c                   move      tap(b)        com(b)
684600010621     c                   end
684700010621      *
684800010621     c                   movea     com           big(c)
684900010621      *
685000010621     c                   do        6             b
685100010621     c                   move      tpn(b)        com(b)
685200010621     c                   end
685300010621      *
685400010621     c                   movea     com           bmn(c)
685500010621      *
685600010621     c                   do        6             b
685700010621     c                   move      tpx(b)        com(b)
685800010621     c                   end
685900010621      *
686000010621     c                   movea     com           bmx(c)
686100010621     c
686200010620     C                   MOVE      CODSIG        BSI(C)
686300010620     C                   MOVE      SAVAIP        AIP(C)
686400010620     C                   MOVE      CODORP        BRG(C)
686500050204     c                   move      desprv        dprv(c)
686600050303     c                   Eval      nodes(c) = nostdes
686700010620     C                   END
686800010620     C                   MOVEL     ' '           SAVAIP
686900010620     C                   ENDSR
687000010620      *-------------------------------------------------------------------------
687100010620      * REGTPT CAMBIO REGIONE
687200010620      *-------------------------------------------------------------------------
687300010620     C     REGTPT        BEGSR
687400010620     C                   Z-ADD     0             NUM4
687500010620     C                   Z-ADD     1             K
687600110216
687700110216     c                   clear                   savk
687800110216
687900010620      * preparo le schiere che devono essere stampate
688000050616    1C                   DO        15            K
688100010620    2C     BRG(K)        IFNE      '  '
688200010622    3C     BRG(K)        IFEQ      '9'
688300110216     c                   eval      savk = k
688400010620      *
688500010622     C     K             IFEQ      1
688600010621     C                   MOVEL     *all'.'       PDE(K)
688700010620     C                   ELSE
688800110216     C                   EVAL      PDE(k) = '.............................'
688900010620     C                   END
689000010620     C                   ADD       1             NUM4
689100010620     C*
689200010622    3C                   ELSE
689300010620     C*
689400010620     C                   MOVEA     *BLANKS       WCT
689500010620     C                   Z-ADD     1             C
689600010620     C                   MOVE      BSI(K)        WCT(C)
689700050204     c                   Eval      wragprv = *Off
689800010621      *
689900010621      * città
690000010621     C                   ADD       1             P
690100010621     C                   MOVEA     BIG(K)        COMO42
690200010621     C                   MOVEA     COMO42        WIN(P)
690300010621      * minimo
690400010621     C                   MOVEA     BMN(K)        CMNO42
690500010621     C                   MOVEA     CMNO42        WMN(P)
690600010621      * massimo
690700010621     C                   MOVEA     BMX(K)        CMXO42
690800010621     C                   MOVEA     CMXO42        WMX(P)
690900010620     C                   MOVEL     AIP(K)        SAVAIP            1
691000010620      *
691100010620      * MEMORIZZO LE ALTRE PROVINCIE CHE HANNO LE STESSE TARIFFE
691200010620     C     K             ADD       1             H
691300050616    4C     H             DO        15            H
691400010620      *
691500010620     C     BRG(H)        IFNE      '9'
691600010621     C     como42        ANDEQ     big(H)
691700010621     C     cmno42        ANDEQ     bmn(H)
691800010621     C     cmxo42        ANDEQ     bmX(H)
691900010620     C     SAVAIP        ANDEQ     AIP(H)
692000010625      *
692100010625      * se isola controllo anche che non siano le province sondrio e como
692200010625     c                   IF        (*in06  and bsi(h)<>'CO' and bsi(h)<>'SO')
692300010625     c                             or not *in06
692400010620     C                   ADD       1             C
692500010620     C                   MOVEL     '/'           WCT(C)
692600010620     C                   MOVE      BSI(H)        WCT(C)
692700010621     C                   MOVEL     *BLANKS       big(H)
692800010621     C                   MOVEL     *BLANKS       bmn(H)
692900010621     C                   MOVEL     *BLANKS       bmx(H)
693000010620     C                   MOVEL     *BLANKS       BRG(H)
693100050204     c                   Eval      wragprv = *On
693200010625     c                   endif
693300010625      *
693400010620     C                   ENDIF
693500010620      *
693600010620   e4C                   ENDDO
693700010620     C*
693800010620     C* MEMORIZZO LA SCHIERA DELLE PROVINCIE CON STESSE TARIFFE
693900010620     C                   ADD       1             C
694000010621     C                   MOVEA     *all'.'       WCT(C)
694100010621     c                   movea     wct           como29
694200010621     C                   MOVEA     como29        PDE(K)
694300010625      * se sto stampando le isole recupero la decodifica delle isole
694400010625     c                   if        %subst(como29:2:2) = 'CO' and *in06
694500010625     c                   eval      PDE(k) = 'CAMPIONE D''ITALIA .........'
694600010625     C                   ENDIF
694700010625      *
694800010625     c                   if        %subst(como29:2:2) = 'SO' and *in06
694900010625     c                   eval      PDE(k) = 'LIVIGNO ....................'
695000010625     C                   ENDIF
695100050204
695200050204      * se tariffa estera e non ho raggruppato più province devo stampare
695300050204      * la descrizione della provincia e non il codice
695400050204     c                   If        wragprv = *Off and (*in02 or *in25)
695500050204     c                   Eval      como29 = *all'.'
695600050303     c                   If        nodes(k) <> 'N'
695700050204     c                   Eval      %subst(como29:1:(%len(%trim(dprv(k))))) =
695800050204     c                             %trim(dprv(k))
695900050303     c                   EndIf
696000050204     c                   Move      como29        pde(k)
696100050204     c                   EndIf
696200010620     C*
696300010622   e3C                   END
696400010620   e2C                   END
696500010620   e1C                   END
696600010620      ***
696700010620      *** STAMPA REGIONE
696800010620      ***
696900110216
697000110216     c                   clear                   giro              1 0
697100110216
697200110216      * stampo per prima 'ALTRI' cioè la regione
697300110216     c                   if        savk > 1
697400110216     c                   eval      k = savk
697500110216     c                   else
697600110216     c                   eval      k = 1
697700110216     c                   endif
697800110216
697900110216     c     rifai1        tag
698000010620      *
698100110216 1   c                   dow       brg(k) <> *blanks
698200110216     c                   if        k = savk  and giro > 0
698300110216     c                   leave
698400110216     c                   endif
698500010621      *
698600010622     c                   movea     BIG(k)        com
698700010621     c                   z-add     1             b
698800110216 2   c                   do        6             b
698900010621     c                   move      com(b)        tap(b)
699000010620      * se tariffa a valore di fianco al valore metto la percentuale
699100110216 3   c                   if        *in09 and tap(b) > 0
699200010622     C                   MOVEL     '%'           PER(B)
699300110216 3   C                   END
699400010620      *
699500110216 2   C                   END
699600010620      * applicazione inoltro (forse non si usa)
699700110216 2   C     AIP(K)        IFEQ      'S'
699800010620     C                   MOVEL     'SI'          STAAIP            2
699900010620     C                   ELSE
700000010620     C                   MOVEL     'NO'          STAAIP
700100110216 2   C                   END
700200010620      * VERIFICO SE C'E' IL MINIMO E IL MASSIMO
700300010620      *
700400010620     C                   SETOFF                                       7586
700500010621      *
700600010622    2C                   IF        BMN(K) <> *BLANKS AND BMN(K) <> *ZEROS
700700010621     C                   SETON                                        75
700800010621      *
700900010622     C                   MOVEA     BMN(K)        COM
701000010621      *
701100010621     C                   DO        6             B
701200010621     C                   MOVE      COM(B)        TPN(B)
701300010621     C                   ENDDO
701400010621      *
701500110216    2C                   ENDIF
701600010621      **
701700010622    2C                   IF        BMX(K) <> *BLANKS AND BMX(K) <> *ZEROS
701800010621     C                   SETON                                        86
701900010621      **
702000010622     C                   MOVEA     BMX(K)        COM
702100010621      *
702200010621     C                   DO        6             B
702300010621     C                   MOVE      COM(B)        TPX(B)
702400010621     C                   ENDDO
702500010621      *
702600010621    2C                   ENDIF
702700010620      *
702800010620      * gestione stampa tariffa particolare
702900010620      *
703000010620     c                   EXSR      SR_STPPAR
703100010621      *
703200010621      *
703300110216     c                   eval      k = k+1
703400010620    1C                   ENDDO
703500110216
703600110216      * se ho stampato 'Altri' torno a loopare per stampare il resto
703700110216     c                   if        savk > 1 and giro = 0
703800110216     c                   eval      k = 1
703900110216     c                   eval      giro = 1
704000110216     c                   goto      rifai1
704100110216     c                   endif
704200010620      *
704300010621     C                   MOVEA     *blanks       big
704400010621     C                   MOVEA     *blanks       bmn
704500010621     C                   MOVEA     *blanks       bmx
704600010621     C                   MOVEA     *blanks       win
704700010621     C                   MOVEA     *blanks       wmn
704800010621     C                   MOVEA     *blanks       wmx
704900010620     C                   MOVEA     *BLANKS       BSI
705000010620     C                   MOVEA     *BLANKS       AIP
705100010620     C                   MOVEA     *BLANKS       PER
705200010620     C                   MOVEA     *ZEROS        TAP
705300010620     C                   MOVEA     *ZEROS        TPN
705400010620     C                   MOVEA     *ZEROS        TPX
705500010620     C                   CLEAR                   BRG
705600010620      *
705700010620     C                   ENDSR
705800010620      *---------------------------------------------------------------*
705900010620      * SR_STPPAR STAMPA TARIFFE PARTICOLARI
706000010620      *---------------------------------------------------------------*
706100010620     C     SR_STPPAR     BEGSR
706200010620      * valorizzo il dettaglio tariffario in stampa
706300010620      *
706400010620     c                   clear                   ta49p03
706500010620      *
706600010620     c                   eval      p03des = desreg + ' ' + pde(k)
706700010620     c                   clear                   desreg
706800010620      * verifico se sono tutti valorizzati
706900010622     c     tap(1)        comp      0                                  61
707000010622     c     tap(2)        comp      0                                  62
707100080925      * se si tratta della tariffa particolare fuel  se valorizzato il secondo
707200080925      * scaglione valorizzo anche il precedente ed anche la %
707300080925     c                   If        savftc = 'f ' and *in62
707400080925     c                   eval      *in61 = *on
707500080925     c                   eval      per(1)= '%'
707600080925     c                   endif
707700010622     c     tap(3)        comp      0                                  63
707800080925      * se si tratta della tariffa particolare fuel  se valorizzato il terzo
707900080925      * scaglione valorizzo anche i precedenti ed anche le %
708000080925     c                   If        savftc = 'f ' and *in63
708100080925     c                   eval      *in61 = *on
708200080925     c                   eval      *in62 = *on
708300080925     c                   eval      per(1)= '%'
708400080925     c                   eval      per(2)= '%'
708500080925     c                   endif
708600010622     c     tap(4)        comp      0                                  64
708700080925      * se si tratta della tariffa particolare fuel  se valorizzato il quarto
708800080925      * scaglione valorizzo anche i precedenti ed anche le %
708900080925     c                   If        savftc = 'f ' and *in64
709000080925     c                   eval      *in61 = *on
709100080925     c                   eval      *in62 = *on
709200080925     c                   eval      *in63 = *on
709300080925     c                   eval      per(1)= '%'
709400080925     c                   eval      per(2)= '%'
709500080925     c                   eval      per(3)= '%'
709600080925     c                   endif
709700010622     c     tap(5)        comp      0                                  65
709800080925      * se si tratta della tariffa particolare fuel  se valorizzato il quinto
709900080925      * scaglione valorizzo anche i precedenti ed anche le %
710000080925     c                   If        savftc = 'f ' and *in65
710100080925     c                   eval      *in61 = *on
710200080925     c                   eval      *in62 = *on
710300080925     c                   eval      *in63 = *on
710400080925     c                   eval      *in64 = *on
710500080925     c                   eval      per(1)= '%'
710600080925     c                   eval      per(2)= '%'
710700080925     c                   eval      per(3)= '%'
710800080925     c                   eval      per(4)= '%'
710900080925     c                   endif
711000010622     c     tap(6)        comp      0                                  66
711100080925      * se si tratta della tariffa particolare fuel  se valorizzato il sesto
711200080925      * scaglione valorizzo anche i precedenti ed anche le %
711300080925     c                   If        savftc = 'f ' and *in66
711400080925     c                   eval      *in61 = *on
711500080925     c                   eval      *in62 = *on
711600080925     c                   eval      *in63 = *on
711700080925     c                   eval      *in64 = *on
711800080925     c                   eval      *in65 = *on
711900080925     c                   eval      per(1)= '%'
712000080925     c                   eval      per(2)= '%'
712100080925     c                   eval      per(3)= '%'
712200080925     c                   eval      per(4)= '%'
712300080925     c                   eval      per(5)= '%'
712400080925     c                   endif
712500010620      *
712600010620     c                   z-add     tap(1)        tar01
712700010620     c                   z-add     tap(2)        tar02
712800010620     c                   z-add     tap(3)        tar03
712900010620     c                   z-add     tap(4)        tar04
713000010620     c                   z-add     tap(5)        tar05
713100010620     c                   z-add     tap(6)        tar06
713200010621      * SE TARIFFA A VALORE STAMPO LA %
713300010621    2C                   IF        *IN09
713400010621     C                   MOVE      per(1)        PER01
713500010621     C                   MOVE      per(2)        PER02
713600010621     C                   MOVE      per(3)        PER03
713700010621     C                   MOVE      per(4)        PER04
713800010621     C                   MOVE      per(5)        PER05
713900010621     C                   MOVE      per(6)        PER06
714000010621    2C                   ENDIF
714100010621      *
714200010625      *
714300010723     c                   if        *in80 or curline > 61
714400010627     c                   write     ta49pfin
714500010625     c                   write     ta49pPP
714600010625     c                   write     ta49p02
714700010625     c                   setoff                                       80
714800010625     c                   endif
714900010625      *
715000010621     C                   WRITE     TA49P03
715100010621      *
715200010621     C*
715300010621     C* VERIFICO SE CI SONO DEI VALORI A ZERO
715400010621     C*
715500010621      * VALORIZZO I CAMPI DI STAMPA
715600010621     C                   CLEAR                   TA49P03
715700010621      * MINIMO
715800010621    2C                   IF        *IN75
715900010621     C     tpn(1)        COMP      0                                  61
716000010621     C     tpn(2)        COMP      0                                  62
716100010621     C     tpn(3)        COMP      0                                  63
716200010621     C     tpn(4)        COMP      0                                  64
716300010622     C     tpn(5)        COMP      0                                  65
716400010622     C     tpn(6)        COMP      0                                  66
716500010621      *
716600010621     C                   Z-ADD     tpn(1)        TAR01
716700010621     C                   Z-ADD     tpn(2)        TAR02
716800010621     C                   Z-ADD     tpn(3)        TAR03
716900010621     C                   Z-ADD     tpn(4)        TAR04
717000010621     C                   Z-ADD     tpn(5)        TAR05
717100010621     C                   Z-ADD     tpn(6)        TAR06
717200010621      *
717300010626      * VALORIZZO LA PARTE DESCRITTIVA
717400060606      * in lingua
717500060606     c                   select
717600060606     c                   when      i45lin = 'I'
717700010626     C                   EVAL      COMO29 = '           Tariffa minima ...'
717800060606     c                   when      i45lin = '2'
717900060606     c                   eval      como29 = '           Minimum price ....'
718000060817     c                   when      i45lin = '3'
718100060817     c                   eval      como29 = '           Tarif minimum ....'
718200060817     c                   when      i45lin = '4'
718300060817     c                   EVAL      COMO29 = '           Mindestpreis .....'
718400060606     c                   endsl
718500010626     C                   MOVE(p)   COMO29        P03DES
718600010621      *
718700010625      *
718800010723     c                   if        *in80 or curline > 61
718900010627     c                   write     ta49pfin
719000010625     c                   write     ta49pPP
719100010625     c                   write     ta49p02
719200010625     c                   setoff                                       80
719300010625     c                   endif
719400010625      *
719500010621     C                   WRITE     TA49P03
719600010621      *
719700010621    2C                   ENDIF
719800010621     C*
719900010621     C* VERIFICO SE CI SONO DEI VALORI A ZERO
720000010621     C*
720100010621      * VALORIZZO I CAMPI DI STAMPA
720200010621     C                   CLEAR                   TA49P03
720300010621      * MASSIMO
720400010621    2C                   IF        *IN86
720500010621     C     tpx(1)        COMP      0                                  61
720600010621     C     tpx(2)        COMP      0                                  62
720700010621     C     tpx(3)        COMP      0                                  63
720800010621     C     tpx(4)        COMP      0                                  64
720900010622     C     tpx(5)        COMP      0                                  65
721000010622     C     tpx(6)        COMP      0                                  66
721100010621      *
721200010621     C                   Z-ADD     tpx(1)        TAR01
721300010621     C                   Z-ADD     tpx(2)        TAR02
721400010621     C                   Z-ADD     tpx(3)        TAR03
721500010621     C                   Z-ADD     tpx(4)        TAR04
721600010621     C                   Z-ADD     tpx(5)        TAR05
721700010621     C                   Z-ADD     tpx(6)        TAR06
721800010621      *
721900010621      * VALORIZZO LA PARTE DESCRITTIVA
722000060606      * in lingua
722100060606     c                   select
722200060606     c                   when      i45lin = 'I'
722300010626     C                   EVAL      COMO29 = '           Tariffa massima ..'
722400060606     c                   when      i45lin = '2'
722500060606     c                   eval      como29 = '           Maximum price ....'
722600060817     c                   when      i45lin = '3'
722700060817     c                   eval      como29 = '           Tarif maximum ....'
722800060817     c                   when      i45lin = '4'
722900060817     c                   EVAL      COMO29 = '           Höchstpreis ......'
723000060606     c                   endsl
723100010626     C                   MOVE(p)   COMO29        P03DES
723200010621      *
723300010625      *
723400010723     c                   if        *in80 or curline > 61
723500010627     c                   write     ta49pfin
723600010625     c                   write     ta49pPP
723700010625     c                   write     ta49p02
723800010625     c                   setoff                                       80
723900010625     c                   endif
724000010625      *
724100010621     C                   WRITE     TA49P03
724200010621      *
724300010621    2C                   ENDIF
724400010621      *
724500010621      *
724600010621     C                   ENDSR
724700010622      *-------------------------------------------------------------------------
724800010622      * FINDET STAMPA FINALE SOTTO OGNI SINGOLA TARIFFA PARTICOLARE
724900010622      *-------------------------------------------------------------------------
725000010622     C     FINDET        BEGSR
725100010919      *
725200010919      * stampo eventuale estensione scaglione unico
725300010919      *
725400010919     c                   exsr      sr_estes
725500010919      *
725600010622     c                   clear                   ta49pTP
725700010622      * verifico se tariffa a q.le per stampare il minimo tassabile
725800010622     c                   eval      *in08 = (tpttpg = '0')
725900010622      * per 08 controllo il minimo tassabile
726000010724      * nel caso in cui si stampa + di uno scaglione verifico il 1° scaglione
726100010724     c                   if        *in08 and recval > 1
726200010622     c                   if        scp(1) < §trata
726300010622     c                   z-add     scp(1)        commts
726400010622     c                   else
726500010622     c                   z-add     §trata        commts
726600010622     c                   endif
726700010622     c                   endif
726800010724      * nel caso in cui si stampa solo uno scaglione verifico il 1° scaglione
726900010724      * salvato
727000010724     c                   if        *in08 and recval = 1
727100010725     c                   if        mintas < §trata and mintas > 0
727200010724     c                   z-add     mintas        commts
727300010724     c                   else
727400010724     c                   z-add     §trata        commts
727500010724     c                   endif
727600010724     c                   endif
727700010622      * verifico rapporto peso / volume
727800010622      *
727900010622     c                   if        tptrpv > 0
728000010622     c     tptrpv        mult      100           comrpv
728100010622     c                   seton                                        19
728200010622     c                   endif
728300010622      * verifico gli importi degli arrotondamenti
728400010622      *
728500010622    1c                   if        tptarl > 0
728600010622      * verifico se esistono dei valori con i decimali
728700010622     C                   move      tptarf        w0030
728800010622     C                   eval      *in89 =(w0030 > 0)
728900010622      *
729000010622    2c                   if        not *in89
729100010622     C                   move      tptarl        w0030
729200010622     C                   eval      *in89 =(w0030 > 0)
729300010622    2c                   endif
729400010622      *
729500010622    2c                   if        not *in89
729600010622     C                   move      tptaro        w0030
729700010622     C                   eval      *in89 =(w0030 > 0)
729800010622    2c                   endif
729900010622      *
730000010622      * se 89 spento non esistono importi con decimali sposto in camp di comodo
730100010622    2c                   if        not *in89
730200010622     C                   Z-ADD     TptARO        WARO2
730300010622     C                   Z-ADD     TptARF        WARF2
730400010622     C                   Z-ADD     TptARL        WARL2
730500010622    2c                   endif
730600010622      *
730700010622      * se arrotondamenti di 1 in 1 non stampo
730800010622      * se arrotondamento fino a diverso da arrotondamento oltre a
730900010622      *
731000010628    2c                   if        tptarf <> tptaro
731100010622      *
731200010622      * valorizzo i campi di stampa
731300060606      * in lingua
731400060606     c                   select
731500060606     c                   when      i45lin = 'I'
731600010622     c  N89              eval      arrot1 = 'fino a ' + %editc(Warl2:'4') +
731700010622     c                                      ' di ' + %editc(warf2:'4')     +
731800010628     c                                      ' in ' + %editc(warf2:'4')
731900010622     c   89              eval      arrot1 = 'fino a ' + %editc(tptarl:'4') +
732000010628     c                                      ' di ' + %editc(tptarf:'4')     +
732100010628     c                                      ' in ' + %editc(tptarf:'4')
732200060606     c                   when      i45lin = '2'
732300060703     c  n89              eval      arrot1 = 'Up to ' +
732400060703     c                                      %editc(warl2:'4') + ' rounded ' +
732500060703     c                                      'to the nearest ' +
732600060703     c                                      %editc(warf2:'4')
732700060703     c   89              eval      arrot1 = 'Up to ' +
732800060703     c                                      %editc(tptarl:'4') + ' rounded ' +
732900060703     c                                      'to the nearest ' +
733000060606     c                                      %editc(tptarl:'4')
733100060817     c                   when      i45lin = '3'
733200060817     c  N89              eval      arrot1 = 'jusq''à  ' + %editc(Warl2:'4') +
733300060817     c                                      ' de ' + %editc(warf2:'4')     +
733400060817     c                                      ' en ' + %editc(warf2:'4')
733500060817     c   89              eval      arrot1 = 'jusq''à  ' + %editc(tptarl:'4') +
733600060817     c                                      ' de ' + %editc(tptarf:'4')     +
733700060817     c                                      ' en ' + %editc(tptarf:'4')
733800060817     c                   when      i45lin = '4'
733900060817     c  N89              eval      arrot1 = 'bis       ' + %editc(Warl2:'4') +
734000060817     c                                      ' von ' + %editc(warf2:'4')     +
734100060817     c                                      ' auf ' + %editc(warf2:'4')
734200060817     c   89              eval      arrot1 = 'bis       ' + %editc(tptarl:'4') +
734300060817     c                                      ' von ' + %editc(tptarf:'4')     +
734400060817     c                                      ' auf ' + %editc(tptarf:'4')
734500060606     c                   endsl
734600010622
734700010622     c                   seton                                        18
734800010622      *
734900010622      * valorizzo i campi di stampa
735000060606      * in lingua
735100060606     c                   select
735200060606     c                   when      i45lin = 'I'
735300010725     c  N89              eval      arrot2 = 'oltre  ' + '    '        +
735400010622     c                                      ' di ' + %editc(waro2:'4')     +
735500010628     c                                      ' in ' + %editc(waro2:'4')
735600010725     c   89              eval      arrot2 = 'oltre  ' + '        '         +
735700010628     c                                      ' di ' + %editc(tptaro:'4')     +
735800010628     c                                      ' in ' + %editc(tptaro:'4')
735900060606     c                   when      i45lin = '2'
736000060704     c  n89              eval      arrot2 = 'over ' +  '     '  +
736100060703     c                                      ' to nearest ' +
736200060703     c                                      %editc(waro2:'4')
736300060704     c   89              eval      arrot2 = 'over ' +  '     '  +
736400060703     c                                      ' to nearest ' +
736500060703     c                                      %editc(tptaro:'4')
736600060817     c                   when      i45lin = '3'
736700060818     c  N89              eval      arrot2 = 'au delà ' + '    '        +
736800060817     c                                      ' de ' + %editc(waro2:'4')     +
736900060817     c                                      ' en ' + %editc(waro2:'4')
737000060818     c   89              eval      arrot2 = 'au delà ' + '        '         +
737100060817     c                                      ' de ' + %editc(tptaro:'4')     +
737200060817     c                                      ' en ' + %editc(tptaro:'4')
737300060817     c                   when      i45lin = '4'
737400060818     c  N89              eval      arrot2 = 'mehr als' + '      '        +
737500060817     c                                      ' von ' + %editc(waro2:'4')     +
737600060817     c                                      ' auf ' + %editc(waro2:'4')
737700060818     c   89              eval      arrot2 = 'mehr als' + '      '         +
737800060817     c                                      ' von ' + %editc(tptaro:'4')     +
737900060817     c                                      ' auf ' + %editc(tptaro:'4')
738000060606     c                   endsl
738100010628      *
738200010628     C                   ELSE
738300010625      *
738400010625      * valorizzo i campi di stampa
738500060606      * in lingua
738600060606     c                   select
738700060606     c                   when      i45lin = 'I'
738800010628     c  N89              eval      arrot1 = 'di ' + %editc(Warf2:'4') +
738900010628     c                                      ' in ' + %editc(waro2:'4')
739000010628     c   89              eval      arrot1 = 'di     ' + %editc(tptarf:'4') +
739100010628     c                                      ' in' + %editc(tptaro:'4')
739200060606     c                   when      i45lin = '2'
739300060703     c  n89              eval      arrot1 = 'Rounded to nearest ' +
739400060606     c                                      %editc(warf2:'4')
739500060703     c   89              eval      arrot1 = 'Rounded to nearest ' +
739600060606     c                                      %editc(tptarf:'4')
739700060817     c                   when      i45lin = '3'
739800060817     c  N89              eval      arrot1 = 'de ' + %editc(Warf2:'4') +
739900060817     c                                      ' en ' + %editc(waro2:'4')
740000060817     c   89              eval      arrot1 = 'di     ' + %editc(tptarf:'4') +
740100060817     c                                      ' in' + %editc(tptaro:'4')
740200060817     c                   when      i45lin = '4'
740300060817     c  N89              eval      arrot1 = 'von ' + %editc(Warf2:'4') +
740400060817     c                                      ' auf ' + %editc(waro2:'4')
740500060817     c   89              eval      arrot1 = 'von    ' + %editc(tptarf:'4') +
740600060817     c                                      ' auf' + %editc(tptaro:'4')
740700060606     c                   endsl
740800010625
740900010628     c                   seton                                        18
741000010625      *
741100010628    2C                   endif
741200010622      *
741300010622    1c                   endif
741400010917      *
741500010917      * verifico se arrot2 è uguale a blanks accendo il 20
741600010917     c                   eval      *in20 = (arrot2 <> *blanks)
741700010622      *
741800010917      * verifico se devo stampare questa riga
741900010917      * in assenza di minimo tassabile o arrotondamenti non stampo
742000010917      * *in08 minimo tassabile *in18 arrotondamenti *in19 volume automatico
742100010917      *
742200010917     c                   if        *in08 or *in18 or *in19
742300010627     c                   exsr      SR_OVER
742400010625      *
742500010622     c                   write     ta49ptp
742600010917      *
742700010917     c                   endif
742800010622      *
742900010622     C                   ENDSR
743000010919      *
743100010919      *---------------------------------------------------------------*
743200010919      * SR_ESTES Gestione stampa estensione scaglione unico
743300010919      *---------------------------------------------------------------*
743400010919     C     SR_ESTES      BEGSR
743500010919      *
743600010919      * se il campo del TARPAREST è diverso da blanks vuol dire che devo
743700010919      * stampare l'estensione della tariffa particolare
743800010919      *
743900010919     c                   if        W_TARPAREST <> *blanks
744000010919     c                   eval      comest = W_TARPAREST
744100010919     c                   exsr      SR_OVER
744200010919      *
744300010919     c                   write     ta49pest
744400010919      *
744500010919     c                   clear                   W_TARPAREST
744600010919      *
744700010919     c                   endif
744800010627      *
744900010919     C                   ENDSR
745000050526
745100050526      *---------------------------------------------------------------*
745200050526      * SR_COMUNICA controllo e stampa dei servizi on line + e-mail
745300050526      *---------------------------------------------------------------*
745400050526     c     SR_COMUNICA   BegSr
745500050526
745600050526      * Controllo se devo stampare i SERVIZI ON LINE
745700050526     c                   ExSr      Sr_Psw
745800050526      * Controllo se devo stampare la richiesta di almeno 1 mail
745900050526     c                   ExSr      Sr_Ema
746000050526
746100050526      * Stampo la testata se devo stampare la richiesta delle e-mail
746200050526     c                   If        wpsw = *on or wema85 = *on or
746300051206     c                             wema87 = *on or wema88 = *on or
746400090612     c                             wema84 = *on or wema89 = *on
746500050527     c                   ExSr      Sr_Over
746600050526     c                   Write     Ta49pema
746700050526      * E-mail per servizi on line
746800050526     c                   If        wpsw = *on
746900050526     c                   Z-Add     curline       totrig
747000050526     c                   Add       4             totrig
747100050527     c                   ExSr      Sr_Overpiu
747200050526     c                   Write     Ta49pema1
747300050526     c                   EndIf
747400050526      * Stampo la riga per la richiesta delle e-mail
747500050526     c                   If        wema85 = *on or wema87 = *on or
747600090612     c                             wema88 = *on or wema84 = *On or
747700090612     c                             wema89 = *on
747800050526     c                   ExSr      Sr_Over
747900050526     c                   Write     Ta49pema2
748000050526     c                   EndIf
748100050526      * E-mail per bonifico bancario
748200050526     c                   If        wema85 = *on
748300050526     c                   ExSr      Sr_Over
748400050526     c                   Write     Ta49pema3
748500050526     c                   EndIf
748600050526      * E-mail per giacenze
748700050526     c                   If        wema88 = *on
748800050526     c                   ExSr      Sr_Over
748900050526     c                   Write     Ta49pema4
749000050526     c                   EndIf
749100050526      * E-mail per danni
749200050526     c                   If        wema87 = *on
749300050526     c                   ExSr      Sr_Over
749400050526     c                   Write     Ta49pema5
749500050526     c                   EndIf
749600051206      * E-mail per invio fattura
749700051206     c                   If        wema84 = *on
749800051206     c                   ExSr      Sr_Over
749900051206     c                   Write     Ta49pema7
750000051206     c                   EndIf
750100090612      * E-mail per sollecito pagamenti
750200090612     c                   If        wema89 = *on
750300090612     c                   ExSr      Sr_Over
750400090612     c                   Write     Ta49pema8
750500090612     c                   EndIf
750600050526      * consenso dei dati
750700050526     c                   Z-Add     curline       totrig
750800050526     c                   Add       4             totrig
750900050527     c                   ExSr      Sr_Overpiu
751000050526     c                   Write     Ta49pema6
751100050526
751200050526     c                   EndIf
751300041229
751400050526     c                   EndSr
751500041229      *---------------------------------------------------------------*
751600041229      * SR_PSW stampa servizi ON LINE se offerta
751700041229      *---------------------------------------------------------------*
751800041229     c     SR_PSW        BegSr
751900041229
752000041229     c                   Eval      wpsw = *Off
752100041229
752200041229      * Se cliente nuovo
752300041229      * --> stampo sempre
752400041230if  1c                   If        VisCnw = 'S'
752500041229     c                   Eval      wpsw = *On
752600041229      * Se cliente non nuovo
752700041229      * --> stampo se non abilitato o se non è figlio di papà abilitato
752800041230   x1c                   Else
752900041230     c                   Move      *all'0'       vssksu
753000041230     c                   Move      visksc        vssksu
753100041230     c                   Eval      vssisv = 'TT'
753200041230     c     kTivss        Setll     Tivss02l
753300041230if  2c                   If        not %Equal(Tivss02l)
753400041230     c                   Clear                   Tibs10ds
753500041230     c                   Eval      d10tle = 'WW'
753600041230     c                   Eval      d10paf = 'P'
753700041230     c                   Eval      d10cod = visksc
753800041230     c                   Eval      d10drf = *date
753900041230     c                   Call      'TIBS10R'
754000041230     c                   Parm                    Tibs10ds
754100041230if  3c                   If        d10err = *Blanks and d10cop > *Zeros
754200041230     c                   Move      *all'0'       vssksu
754300041230     c                   Move      d10cop        vssksu
754400041230     c                   Eval      vssisv = 'TT'
754500041230     c     kTivss        Setll     Tivss02l
754600041230     c                   If        not %Equal(Tivss02l)
754700041230     c                   Eval      wpsw = *On
754800041230     c                   EndIf
754900041230e   3c                   EndIf
755000050112if  3c                   If        d10err <> *Blanks
755100050112     c                   Eval      wpsw = *On
755200050112e   3c                   EndIf
755300041230e   2c                   EndIf
755400041230e   1c                   EndIf
755500041229
755600041229     c                   EndSr
755700050525
755800050525      *---------------------------------------------------------------*
755900050526      * SR_EMA stampa delle e-mail da richiedere se offerta
756000050525      *---------------------------------------------------------------*
756100050526     c     Sr_Ema        BegSr
756200050526
756300050526     c                   Eval      wema85 = *Off
756400050526     c                   Eval      wema88 = *Off
756500050526     c                   Eval      wema87 = *Off
756600051206     c                   Eval      wema84 = *Off
756700090612     c                   Eval      wema89 = *Off
756800091112      * visita
756900050527     c                   eval      notapl = 'V'
757000050527     c                   movel     i45kcc        notke1
757100050527     c                   move      parsks        notke1
757200091112      * trattativa
757300091112     c                   if        $trattativa
757400091112     c                   eval      notapl = 'T'
757500091112     c                   endif
757600050527     c                   Clear                   notke2
757700050525
757800050526      * e-mail bonifico bancario nota 85
757900050526     c                   If        §4uabi = 'S'
758000050527      * nota della visita
758100050526     c                   movel     '85'          nottnt
758200050526     c     keyntc        Chain     tfntc01l
758300050527     c                   If        %Found(tfntc01l)
758400050527     c                   Goto      nota88
758500050527     c                   EndIf
758600050527      * nota del cliente se non è un cliente nuovo
758700050527     c                   If        visksc > *Zeros
758800050527     c                   eval      notapl = 'C'
758900050527     c                   move      visksc        notke1
759000050527     c     keyntc        Chain     tfntc01l
759100050527     c                   If        %Found(tfntc01l)
759200050527     c                   Goto      nota88
759300050527     c                   EndIf
759400050527     c                   EndIf
759500050527      * nota non trovata
759600050526     c                   Eval      wema85 = *On
759700050526     c                   EndIf
759800050527
759900050527     c     nota88        Tag
760000091112      * visita
760100050527     c                   eval      notapl = 'V'
760200050527     c                   move      parsks        notke1
760300091112      * trattativa
760400091112     c                   if        $trattativa
760500091112     c                   eval      notapl = 'T'
760600091112     c                   endif
760700050527
760800050527      * e-mail giacenze nota 88
760900050527     c                   movel     '88'          nottnt
761000050527      * nota della visita
761100050527     c     keyntc        Chain     tfntc01l
761200050527     c                   If        %Found(tfntc01l)
761300050527     c                   Goto      nota87
761400050527     c                   EndIf
761500050527      * nota del cliente se non è un cliente nuovo
761600050527     c                   If        visksc > *Zeros
761700050527     c                   eval      notapl = 'C'
761800050527     c                   move      visksc        notke1
761900050527     c     keyntc        Chain     tfntc01l
762000050527     c                   If        %Found(tfntc01l)
762100050527     c                   Goto      nota87
762200050527     c                   EndIf
762300050527     c                   EndIf
762400050527      * nota non trovata
762500050527     c                   Eval      wema88 = *On
762600050527
762700050527     c     nota87        Tag
762800091112      * visita
762900050527     c                   eval      notapl = 'V'
763000050527     c                   move      parsks        notke1
763100091112      * trattativa
763200091112     c                   if        $trattativa
763300091112     c                   eval      notapl = 'T'
763400091112     c                   endif
763500050527
763600050526      * e-mail danni nota 87
763700050527     c                   movel     '87'          nottnt
763800050527      * nota della visita
763900050527     c     keyntc        Chain     tfntc01l
764000050527     c                   If        %found(tfntc01l)
764100090612     c                   Goto      nota84
764200050527     c                   EndIf
764300050527      * nota del cliente se non è un cliente nuovo
764400050527     c                   If        visksc > *Zeros
764500050527     c                   eval      notapl = 'C'
764600050527     c                   move      visksc        notke1
764700050527     c     keyntc        Chain     tfntc01l
764800050527     c                   If        %Found(tfntc01l)
764900051206     c                   Goto      nota84
765000050527     c                   EndIf
765100050527     c                   EndIf
765200050527      * nota non trovata
765300050527     c                   Eval      wema87 = *On
765400051206
765500051206     c     nota84        Tag
765600091112      * visita
765700051206     c                   eval      notapl = 'V'
765800051206     c                   move      parsks        notke1
765900091112      * trattativa
766000091112     c                   if        $trattativa
766100091112     c                   eval      notapl = 'T'
766200091112     c                   endif
766300091112
766400051206      * e-mail invio fattura nota 84
766500051206     c                   movel     '84'          nottnt
766600051206      * nota della visita
766700051206     c     keyntc        Chain     tfntc01l
766800051206     c                   If        %found(tfntc01l)
766900090612     c                   Goto      nota89
767000051206     c                   EndIf
767100051206      * nota del cliente se non è un cliente nuovo
767200051206     c                   If        visksc > *Zeros
767300051206     c                   eval      notapl = 'C'
767400051206     c                   move      visksc        notke1
767500051206     c     keyntc        Chain     tfntc01l
767600051206     c                   If        %Found(tfntc01l)
767700090612     c                   Goto      nota89
767800051206     c                   EndIf
767900051206     c                   EndIf
768000051206      * nota non trovata
768100060203     c                   Eval      wema84 = *On
768200090612
768300090612     c     nota89        Tag
768400091112      * visita
768500090612     c                   eval      notapl = 'V'
768600090612     c                   move      parsks        notke1
768700091112      * trattativa
768800091112     c                   if        $trattativa
768900091112     c                   eval      notapl = 'T'
769000091112     c                   endif
769100090612
769200090612      * e-mail sollecito pagamenti 89
769300090612     c                   movel     '89'          nottnt
769400090612      * nota della visita
769500090612     c     keyntc        Chain     tfntc01l
769600090612     c                   If        %found(tfntc01l)
769700090612     c                   Leavesr
769800090612     c                   EndIf
769900090612      * nota del cliente se non è un cliente nuovo
770000090612     c                   If        visksc > *Zeros
770100090612     c                   eval      notapl = 'C'
770200090612     c                   move      visksc        notke1
770300090612     c     keyntc        Chain     tfntc01l
770400090612     c                   If        %Found(tfntc01l)
770500090612     c                   Leavesr
770600090612     c                   EndIf
770700090612     c                   EndIf
770800090612      * nota non trovata
770900090612     c                   Eval      wema89 = *On
771000050525
771100050525     c                   EndSr
771200041229
771300010627      *---------------------------------------------------------------*
771400010627      * SR_OVER gestione standarad overflow
771500010627      *---------------------------------------------------------------*
771600010627     C     SR_OVER       BEGSR
771700010627      *
771800010801     c                   if        *in80 or curline >= 61
771900010627     c                   write     ta49pfin
772000010627     c                   write     ta49pPP
772100010627     c                   setoff                                       80
772200010627     c                   endif
772300010627      *
772400010627     c                   endsr
772500050526
772600050526      *---------------------------------------------------------------*
772700050526      * SR_OVERPIU gestione standarad overflow con + righe
772800050526      *---------------------------------------------------------------*
772900050526     C     SR_OVERPIU    BEGSR
773000050526      *
773100050526     c                   if        *in80 or totrig >= 61
773200050527     c                   write     ta49pfin
773300050527     c                   write     ta49pPP
773400050526     c                   setoff                                       80
773500050526     c                   endif
773600050526      *
773700050526     c                   endsr
773800050526
773900010627      *
774000010618      *---------------------------------------------------------------*
774100010618      * STAMPA ERRORE
774200010618      *---------------------------------------------------------------*
774300010615     C     STAERR        BEGSR
774400050926      * se offerta valorizzo con il numero della visita
774500050926    2c                   if        *in98
774600050926     c                   eval      p07ksc = visksc
774700050926     c                   movel     tamksc        p07nrv
774800050926     c                   move      tamksc        p07prv
774900091117     c                   if        $trattativa
775000091117     c                   eval      p07ntra = tamksc
775100091117     c                   endif
775200050930      * tariffa
775300050926   x2c                   else
775400050926     c                   eval      p07ksc = tamksc
775500050926    2c                   endif
775600091117     c                   if        $trattativa
775700091117     c                   eval      ptratta =
775800091117     c                             '(codice ' +
775900091117     c                             %editc(tamctr:'X') + '/' +
776000091117     c                             %subst(%editc(tamprg:'X'):3:1) + ')'
776100091117     c                   endif
776200091117     c                   if        not $trattativa
776300091117     c                   eval      ptariffa =
776400091117     c                             '(codice ' +
776500091117     c                             %editc(tamctr:'X') + ')'
776600091117     c                   endif
776700050930
776800050930     c                   Write     ta49perr
776900050930     c                   If        woktar = *Off
777000050930     c                   Write     ta49p07
777100050930     c                   EndIf
777200050930
777300010615     C                   ENDSR
777400060922
777500060922      *---------------------------------------------------------------*
777600060922      * Imposto ATF lungo 7 nelle varie lingue
777700060922      *---------------------------------------------------------------*
777800060922     c     sr_atfk       begsr
777900060922
778000060922      * tariffa peso q.li 0
778100060925     c                   if        wctr1 = '0'
778200060922      * in lingua
778300060922     c                   select
778400060922     c                   when      i45lin = 'I'
778500060922     c                   eval      atf(k) = 'A Q.LE'
778600060922     c                   when      i45lin = '2'
778700060922     c                   eval      atf(k) = '100 KG'
778800060922     c                   when      i45lin = '3'
778900060922     c                   eval      atf(k) = '100 KG'
779000060922     c                   when      i45lin = '4'
779100060922     c                   eval      atf(k) = '100 KG'
779200060922     c                   endsl
779300060922     c                   endif
779400060922
779500060922      * tariffa colli 1
779600060925     c                   if        wctr1 = '1'
779700060922      * in lingua
779800060922     c                   select
779900060922     c                   when      i45lin = 'I'
780000060922     c                   eval      atf(k) = 'A COLLO'
780100060922     c                   when      i45lin = '2'
780200060922     c                   eval      atf(k) = 'P PARC.'
780300060922     c                   when      i45lin = '3'
780400060922     c                   eval      atf(k) = 'COLIS'
780500060922     c                   when      i45lin = '4'
780600060922     c                   eval      atf(k) = 'PRO PAK'
780700060922     c                   endsl
780800060922     c                   endif
780900060922
781000060922      * tariffa spedizione 2
781100060925     c                   if        wctr1 = '2'
781200060922      * in lingua
781300060922     c                   select
781400060922     c                   when      i45lin = 'I'
781500060922     c                   eval      atf(k) = 'A SPED.'
781600060922     c                   when      i45lin = '2'
781700060922     c                   eval      atf(k) = 'P SHIP.'
781800060922     c                   when      i45lin = '3'
781900060922     c                   eval      atf(k) = 'ENVOI'
782000060922     c                   when      i45lin = '4'
782100060922     c                   eval      atf(k) = 'PRO SEN'
782200060922     c                   endsl
782300060922     c                   endif
782400060922
782500060922      * tariffa peso kg 3
782600060925     c                   if        wctr1 = '3'
782700060922      * in lingua
782800060922     c                   select
782900060922     c                   when      i45lin = 'I'
783000060922     c                   eval      atf(k) = 'A KG.'
783100060922     c                   when      i45lin = '2'
783200060922     c                   eval      atf(k) = 'BY KG'
783300060922     c                   when      i45lin = '3'
783400060922     c                   eval      atf(k) = 'AU KG'
783500060922     c                   when      i45lin = '4'
783600060922     c                   eval      atf(k) = 'PRO KG'
783700060922     c                   endsl
783800060922     c                   endif
783900060922
784000060922      * tariffa a valore 4
784100060925     c                   if        wctr1 = '4'
784200060922      * in lingua
784300060922     c                   select
784400060922     c                   when      i45lin = 'I'
784500060922     c                   eval      atf(k) = 'A VAL.'
784600060922     c                   when      i45lin = '2'
784700060922     c                   eval      atf(k) = 'BY VALU'
784800060922     c                   when      i45lin = '3'
784900060922     c                   eval      atf(k) = 'VALEUR'
785000060922     c                   when      i45lin = '4'
785100060922     c                   eval      atf(k) = 'NACH WE'
785200060922     c                   endsl
785300060922     c                   endif
785400060922
785500060922      * tariffa a quantità 5
785600060925     c                   if        wctr1 = '5'
785700060922      * in lingua
785800060922     c                   select
785900060922     c                   when      i45lin = 'I'
786000060922     c                   eval      atf(k) = 'A Q.TA'
786100060922     c                   when      i45lin = '2'
786200060922     c                   eval      atf(k) = 'BY QUAN'
786300060922     c                   when      i45lin = '3'
786400060922     c                   eval      atf(k) = 'QUANT.'
786500060922     c                   when      i45lin = '4'
786600060922     c                   eval      atf(k) = 'NACH ME'
786700060922     c                   endsl
786800060922     c                   endif
786900060922
787000060922      * tariffa valore specifico V (usato nelle tariffe particolari)
787100060925     c                   if        wctr1 = 'V'
787200060922      * in lingua
787300060922     c                   select
787400060922     c                   when      i45lin = 'I'
787500060922     c                   eval      atf(k) = 'VAL.SPE'
787600060922     c                   when      i45lin = '2'
787700060922     c                   eval      atf(k) = 'SET VAL'
787800060922     c                   when      i45lin = '3'
787900060922     c                   eval      atf(k) = 'VAL.SPE'
788000060922     c                   when      i45lin = '4'
788100060922     c                   eval      atf(k) = 'SP.WERT'
788200060922     c                   endsl
788300060922     c                   endif
788400060922
788500060922      * questo è un chiodo per impostare 'A SPED.' quando lo richiede
788600060922      * il programma
788700060925     c                   if        wctr1 = ' '
788800060922      * in lingua
788900060922     c                   select
789000060922     c                   when      i45lin = 'I'
789100060922     c                   eval      atf(k) = 'A SPED.'
789200060922     c                   when      i45lin = '2'
789300060922     c                   eval      atf(k) = 'P SHIP.'
789400060922     c                   when      i45lin = '3'
789500060922     c                   eval      atf(k) = 'PAR ENV'
789600060922     c                   when      i45lin = '4'
789700060922     c                   eval      atf(k) = 'N.SEND'
789800060922     c                   endsl
789900060922     c                   endif
790000151117
790100151117      * tariffa a ritiro solo per tariffa particolare t Diritto di chiamata
790200151117      * ritiro telefonico
790300151117     c                   if        wctr1 = 'R'
790400151117      * in lingua
790500151117     c                   select
790600151117     c                   when      i45lin = 'I'
790700151117     c                   eval      atf(k) = 'A RIT.'
790800151117     c                   when      i45lin = '2'
790900151117     c                   eval      atf(k) = 'A RIT.'
791000151117     c                   when      i45lin = '3'
791100151117     c                   eval      atf(k) = 'A RIT.'
791200151117     c                   when      i45lin = '4'
791300151117     c                   eval      atf(k) = 'A RIT.'
791400151117     c                   endsl
791500151117     c                   endif
791600151117
791700060922
791800060922     c                   endsr
791900060922
792000060922      *---------------------------------------------------------------*
792100060922      * Imposto la parte finale del numero per il lasciato avviso
792200060922      *---------------------------------------------------------------*
792300060922     c     sr_numgp      begsr
792400060922
792500060922     c                   select
792600060922      * italiano sempre ° fisso nel printer file il campo qua lo pulisco
792700060922     c                   when      i45lin = 'I'
792800060922     c                   clear                   comnavd
792900060922      * inglese cambiano i primi tre poi sempre th
793000060922     c                   when      i45lin = '2'
793100060922     c                   eval      comnavd = 'th'
793200060922     c                   if        comnav = 1
793300060922     c                   eval      comnavd = 'st'
793400060922     c                   endif
793500060922     c                   if        comnav = 2
793600060922     c                   eval      comnavd = 'nd'
793700060922     c                   endif
793800060922     c                   if        comnav = 3
793900060922     c                   eval      comnavd = 'rd'
794000060922     c                   endif
794100060922      * francese cambia solo il primo il resto sempre ieme
794200060922     c                   when      i45lin = '3'
794300060922     c                   eval      comnavd = 'ieme'
794400060922     c                   if        comnav = 1
794500060922     c                   eval      comnavd = 'er'
794600060922     c                   endif
794700060922      * tedesco sempre . fisso nel printer file il campo qua lo pulisco
794800060922     c                   when      i45lin = '4'
794900060922     c                   clear                   comnavd
795000060922     c                   endsl
795100060922
795200060922     c                   endsr
795300060922
795400010606     C*-------------------------------------------------------------------------
795500010606     C*  SR_CARTAB  CARICAMENTO INIZIALE TABELLE
795600010606     C*-------------------------------------------------------------------------
795700010606     C     SR_CARTAB     BEGSR
795800010606     C*  carico la schiera di tutti i codici tassazione
795900010606     C                   clear                   K
796000010606     C                   clear                   K1
796100010606     C                   eval      cod = 'CT'
796200060704     c     tabk2l        setll     tabel00f
796300010531     C                   DO        *hival
796400060704     c     tabk2l        READE     Tabel00f
796500010531      *
796600010531     C                   If        %eof(tabel00f)
796700010531     C                   leave
796800010531     C                   endif
796900010531      *
797000010531     C                   IF        tblkey <> *blanks
797100010531     C                   add       1             K
797200010531     C                   movel     Tblkey        CCT(K)
797300010531     C                   movel     Tbluni        DSCT
797400010531     C                   move      §CTprv        SCT(K)
797500010531     C                   MOVE      §CTrap        RCT(K)
797600010531     C                   MOVE      §CTnar        NAR(K)
797700010531     C                   MOVE      §CTcor        DS6PR2
797800050204     c                   Move      §ctdes        prvdes(k)
797900050303     c                   Eval      noctdes(k) = §ctsta
798000010531      * se è una regione carico la schiera delle regioni
798100010531     C                   IF        ds6pr2 = '9'
798200010531     C                   add       1             K1
798300010531     C                   movel     Tblkey        CRE(K1)
798400010531     C                   move      §CTdes        DRE(K1)
798500010531     C                   endif
798600010531      *
798700010531     C                   endif
798800010531      *
798900010531     C                   ENDDO
799000010531      *
799100010531      *  carico la schiera delle nazioni con quella ITALIA
799200010531     C                   z-add     1             K
799300010531     C                   eval      cod = '15'
799400060724     C     TABK2l        SETLL     TABEL00F
799500010531     c                   DO        *HIVAL
799600060724     C     TABK2l        READE     TABEL00F
799700010531      *
799800010621     C                   IF          %eof(tabel00f)
799900010531     C                   leave
800000010531     c                   ENDIF
800100010531      *
800200010531      * se non è annullato
800300010531     C                   IF        Tblflg = ' '
800400010531      *
800500010531     C                   Movel     tbluni        DS15
800600010531      * carico solo la nazione ITALIA
800700010531     C                   IF        §15ita = 'I'
800800010531     C                   Movel     Tblkey        NAZ(K)
800900010531     C                   add       1             K
801000010531     C                   endif
801100010531      *
801200010531     C                   endif
801300010531      *
801400010531     C                   ENDDO
801500010807      *
801600010807      * aggancio la tabella delle variabili giacenza
801700010807     C                   eval      cod = '2G'
801800010807     C                   clear                   KEY
801900010807     C                   movel     '1'           KEY
802000010807     C     TABKEY        CHAIN     TABEL00F
802100010807     c                   if        %found(tabel00f)
802200010807     C                   movel     TBLUNI        DS2G
802300010807     c                   else
802400010807     C                   clear                   DS2G
802500010807     c                   endif
802600010807      *
802700021023      * Richiamo FnLv41 per verificare/creare testi e reperire ds1h
802800021022     C                   clear                   Ds1H
802900021022     C                   move      *off          $NoStp            1
803000021022     C                   clear                   Fnlv41ds
803100021022     C                   movel     '3'           I41Mod
803200021022     C                   movel     I45TTX        I41Let
803300021022     C                   CALL      'FNLV41R'
803400021022     C                   PARM                    FNLV41DS
803500021022      *
803600021022      * Se non ho errori stampo
803700021023     C     O41ERR        IFNE      'E'
803800021022     C                   movel     O41Dta        DS1H
803900010531      *
804000010531      * stampo indirizzo di filiale
804100010531     C                   eval      *in92 = (§1Hfv1 = 'S')
804200010531      * stampo indirizzo cliente
804300010531     C                   eval      *in93 = (§1Hfv2 = 'S')
804400010531      * stampo alla C.A.
804500010531     C                   eval      *in94 = (§1Hfv4 = 'S')
804600010531      * stampo "Telefonando chiedere .........."
804700010531     C                   eval      *in95 = (§1Hfv5 = 'S')
804800010531      * stampo la data
804900010531     C                   eval      *in96 = (§1Hfv6 = 'S')
805000010531     C* STAMPA RIFERIMENTI
805100010531     C                   eval      *in97 = (§1Hfv9 = 'S'and i45nr1 <> *blanks)
805200021022      *
805300021022     C                   ELSE
805400021022     C                   move      *on           $NoStp
805500021022     C                   ENDIF
805600101015
805700101015      /free
805800101015       //?Carico scatti ISTAT
805900101015         SISsca = 0;
806000101015         clear DIA;
806100101015         setll (SISsca) TISIS01L;
806200101015
806300101015         DOW  not $EoF;
806400101015
806500101015           read TISIS01L;
806600101015
806700101015           IF  %Eof(TISIS01L);
806800101015             $EoF = *on;
806900101015             leave;
807000101015           ENDIF;
807100101015
807200101015           DIA(SISsca) = SISdata;
807300101015
807400101015         ENDDO;
807500160623
807600101015
807700101015      /end-free
807800021022      *
807900010531     C                   ENDSR
808000080609
808100080609      *------------------------------------------------------------------------*
808200080609      * Carico filiali abilitate al lasciato avviso
808300080609      *------------------------------------------------------------------------*
808400080609     c     carlav        begsr
808500080609
808600080609     c                   clear                   sklav
808700080609     c                   clear                   xxx
808800080609
808900080609     c/exec sql
809000080609     c+ declare lav cursor for select tntbe01l.*
809100080609     c+ from tntbe01l
809200080609     c+ where tbecod='LAV' and tbeatb = ''
809300080609     c/end-exec
809400080609
809500080609     c/exec sql
809600080609     c+ open lav
809700080609     c/end-exec
809800080609
809900080609     c                   do        *hival
810000080609
810100080609     c/exec sql
810200080609     c+ fetch next from lav into :tntbeds
810300080609     c/end-exec
810400080609
810500080609     c                   select
810600080609     c                   when      sqlcod = 100
810700080609     c                   leave
810800080609     c                   when      sqlcod < 0
810900080609     c                   other
811000080609     c                   eval      dlav = tbeuni
811100080609      * carico la schiera della filiali abilitate
811200080616     c                   if        d§lavtar > *date
811300080609     c                   iter
811400080609     c                   endif
811500080609     c                   eval      xxx = xxx + 1
811600080609     c                   eval      sklav(xxx) = %dec(tbeke1:3:0)
811700080609
811800080609     c                   endsl
811900080609
812000080609     c                   enddo
812100080609
812200080609     c/exec sql
812300080609     c+ close lav
812400080609     c/end-exec
812500080609
812600080609
812700080609     c                   endsr
812800010530     C*------------------------------------------------------------------------
812900010530     C* ROUTINE INIZIALE
813000010530     C*------------------------------------------------------------------------
813100010530      *
813200010530     C     *INZSR        BEGSR
813300010530      *
813400010530     C     *ENTRY        PLIST
813500010530     C                   parm                    KPJBA
813600020919     C                   parm                    fnlv45ds
813700010628      *
813800020624     C                   z-add     1             codut             1 0
813900161221      * imposta coda di stampa
814000161221     c                   eval      coda_allegati= kqotd
814100161223      * apro il file di stampa
814200161223     c                   open      tnta49p
814300020624
814400020624      * reperisco i dati utente
814500020624     c     *dtaara       define    §azute        AzuteDs
814600020624     c     *dtaara       define    §datiute      DDatiUte
814700020624
814800020624     c                   in(E)     *dtaara
814900020624     c                   IF        %Error or RSUT = *blanks
815000020624     c                   call      'TIBS34R'
815100020624     c                   parm                    Tibs34Ds
815200020624     c                   in        *dtaara
815300020624     c                   ENDIF
815400110516
815500110516      * chiodo fisso 'BRT S.p.A.'
815600110516     c                   eval      rsutold = 'BRT S.p.A.'
815700020624      *
815800020919     C                   MOVEL     SIMFEL        simfelA           3
815900060704
816000060704      * lingua per tabelle
816100060704     c                   clear                   kcodut            1 0
816200060704     c                   select
816300060704     c                   when      i45lin = 'I'
816400060704     c                   eval      kcodut = 1
816500060704     c                   when      i45lin = '2'
816600060704     c                   eval      kcodut = 2
816700060704     c                   when      i45lin = '3'
816800060704     c                   eval      kcodut = 3
816900060704     c                   when      i45lin = '4'
817000060704     c                   eval      kcodut = 4
817100060704     c                   endsl
817200010530      *
817300010530      * preparo il campo PAGE per la numerazione delle pagine
817400010530     c                   eval      page = 1
817500010531      * carico le tabelle
817600010531     c                   exsr      SR_CARTAB
817700021022      *
817800021022      * proseguo se ho i dati per uso testi
817900021022     c                   if        $NoStp = *off
818000010531      *
818100010531      * TARIFFA O OFFERTA
818200010531      *
818300010531      *............ OFFERTA .....................
818400010531      *
818500110104     c                   if        i45ast = 'T'
818600010531      * accendo il 98
818700010531     c                   seton                                        98
818800091112      * se trattativa imposto flag
818900091112     c                   eval      $trattativa = (i45ast = 'T')
819000091117     c                   eval      *in85 = (i45ast = 'T')
819100010531      * valorizzo il campo del codice cliente tariffa con il numero visita
819200010531     c                   eval      parsks = i45nrv
819300010531      * apro i file visita/offerta
819400010531     c                   open      TFACO00F
819500010531     c                   open      TFIND00F
819600010531     c                   open      TFCLP00F
819700010531     c                   open      TNOFM01L
819800061122     c                   open      tiofd01L
819900010531     c                   open      TIOPT01L
820000010629     c                   open      TIOPD01L
820100010531     c                   open      TIOGC01L
820200091022     c                   if        i45ast = 'T'
820300091022     c                   open      Tivis05l
820400091022     c                   endif
820500041229
820600010531      *  i45ast = 'C'
820700010531     c                   else
820800010531      *
820900010531      *............ TARIFFA .....................
821000061122      * apro i file visita/offerta
821100061122     c                   open      TNtam01L
821200061122     c                   open      Titad04L
821300061122     c                   open      Titpt01L
821400061122     c                   open      TItpD01L
821500061122     c                   open      TItGC01L
821600010531      *
821700010531      * valorizzo il campo del codice cliente tariffa
821800010531     c                   eval      parsks = i45ksc
821900010531      *
822000010531     c                   endif
822100021022     c                   endif
822200010531      *
822300010531      *   DEFINIZIONI DELLE CHIAVI
822400010531      *
822500010531      * accesso per codice su TABEL00F
822600090608     c     TABKEY        klist
822700010531     c                   kfld                    codut
822800010531     c                   kfld                    cod
822900010531     c                   kfld                    key
823000060605
823100060605      * accesso per codice su TABEL00F in lingua
823200060605     c     tabkeyl       klist
823300060605     c                   kfld                    kcodut
823400060605     c                   kfld                    cod
823500060605     c                   kfld                    key
823600060606
823700060606      * accesso per codice su TABEL00F  parziale in lingua
823800060606     c     tabk2l        klist
823900060606     c                   kfld                    kcodut
824000060606     c                   kfld                    cod
824100060606
824200010531      * accesso TNTAM01L/TNOFM01L
824300010531     c     DS1KTM        klist
824400010531     c                   kfld                    ds1ksc
824500010531     c                   kfld                    ds1ctr
824600010531     c                   kfld                    ds1prg
824700010605      * accesso TITAD04L con linea partenza impostata
824800010605     c     DS1KTD        klist
824900010605     c                   kfld                    ds1ksc
825000010605     c                   kfld                    ds1ctr
825100010605     c                   kfld                    ds1prg
825200010605     c                   kfld                    comfil
825300010606      * accesso TITAD04L con chiave piena
825400010606     c     KEYTAD        klist
825500010606     c                   kfld                    tadksc
825600010606     c                   kfld                    tadctr
825700010606     c                   kfld                    tadprg
825800010606     c                   kfld                    tadlnp
825900010606     c                   kfld                    tadorp
826000010606     c                   kfld                    tadnaz
826100010606     c                   kfld                    tadcap
826200010618      ** accesso su TITPT e TITPD
826300010618     C     KTPT          KLIST
826400010618     C                   KFLD                    ds1ksc
826500010618     C                   KFLD                    ds1ctr
826600010618     C                   KFLD                    ds1prg
826700010618     C                   KFLD                    savftc
826800010621      ** accesso su  TITPD
826900010621     C     KTPD          KLIST
827000010621     C                   KFLD                    DS1KSC
827100010621     C                   KFLD                    DS1CTR
827200010621     C                   KFLD                    DS1PRG
827300010621     C                   KFLD                    SAVFTC
827400010621     C                   KFLD                    SAVORP
827500010621     C     KTPD2         KLIST
827600010621     C                   KFLD                    DS1KSC
827700010621     C                   KFLD                    DS1CTR
827800010621     C                   KFLD                    DS1PRG
827900010621     C                   KFLD                    SAVFTC
828000010621     C                   KFLD                    SAVORP
828100010621     C                   KFLD                    SAVSGL
828200010626     C     KTPDWRK       KLIST
828300010626     C                   KFLD                    DS1KSC
828400010626     C                   KFLD                    DS1CTR
828500010626     C                   KFLD                    DS1PRG
828600010626     C                   KFLD                    WRKFTC
828700010626     C                   KFLD                    WRKORP
828800010626     C                   KFLD                    WRKSGL
828900010601      ** accesso CNACO / TFACO
829000010601     C     DS7KAC        klist
829100010601     C                   kfld                    codut
829200010601     C                   kfld                    i45kcc
829300010601     C                   kfld                    ksc
829400010601      ** accesso TFNTC
829500010601     C     KEYNTC        klist
829600010601     c                   kfld                    notapl
829700010601     c                   kfld                    notke1
829800010601     c                   kfld                    notke2
829900010601     c                   kfld                    nottnt
830000010604      * accesso AZCPC03L
830100010621     c     kcpc          klist
830200010604     c                   kfld                    o96ver
830300010604     c                   kfld                    §ctnar
830400010604     c                   kfld                    comfil
830500041230      * accesso TIVSS02L
830600041230     c     kTivss        klist
830700041230     c                   kfld                    vssksu
830800041230     c                   kfld                    vssisv
830900080211
831000080211     c     kCnabi        Klist
831100080211     c                   Kfld                    pcabi
831200080211     c                   Kfld                    pccab
831300010531      *
831400010531     c                   ENDSR
831500010531      *
831600010530**   MSG
831700140224ATTENZIONE!!! Prezzo base Fuel mai rilevato settimanalmente dal Ministero!!   1
831800010530                                                                              2
831900010530CLIENTE NON CODIFICATO O ANNULLATO                                            3
832000010530NESSUNA RIGA DI DETTAGLIO X QUESTA TARIFFA/OFFERTA                            4
832100050615PIU' DI 18 SCAGLIONI X CODICE DI TASSAZIONE                                   5
832200010530CODICE TASSAZIONE NON TROVATO !!!!!!!!!!!      CUMELA MO' ?                   6
832300010530CODICE REGIONE DEL CODICE DI TASSAZIONE NON ESISTE IN TABELLA                 7
832400010530UN CODICE DI TASSAZIONE DELLA TARIFFA HA UNO SCAGLIONE NON PREVISTO           8
832500050616PIU' DI 15 PROVINCIE X 1 REGIONE? TABELLA ERRATA O  T N T A D  NON ALLINEATO  9
832600050616PIU' DI 15 PROVINCIE X 1 REGIONE? TABELLA ERRATA O  T N T P D  NON ALLINEATO  10
832700050615                                                                              1
832800010530TROPPI PAESI DI ARRIVO/CODICI DI TASSAZIONE PER LA REGIONE                    2
832900140224                                                                              3
833000010530UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON PREVISTO   4
833100050615PIU' DI 18 SCAGLIONI X CODICE DI TASSAZIONE PER LE TARIFFE PARTICOLARI        5
833200050324UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON VALIDO     6
