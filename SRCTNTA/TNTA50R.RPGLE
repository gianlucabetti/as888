000100041230     H DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000200020108      *------------------------------------------------------------------*
000300020108      *        S T A M P A       T A R I F F E / O F F E R T E
000400020108      *                  P E R     C L I E N T E
000500020108      *        98 ON - OFFERTA            98 OFF- TARIFFA
000600030508      *        05 ON - Merci              05 OFF - Documenti
000700020108      *------------------------------------------------------------------*
000800020111
000900020107      * TESTATA E DETTAGLIO TARIFFE
001000061011     FTNTAM01L  IF   E           K DISK    usropn
001100020107     FTNOFM01L  IF   E           K DISK    USROPN
001200020107     F                                     RENAME(TNTAM000:TNOFM000)
001300061011     FTITAD04L  IF   E           K DISK    usropn
001400061011     ftiofd01l  if   e           k disk    usropn
001500061011     f                                     rename(titad000:tiofd000)
001600020111
001700020107      * TESTATA E DETTAGLIO TARIFFE PARTICOLARI
001800061011     FTITPT01L  IF   E           K DISK    usropn
001900020107     FTIOPT01L  IF   E           K DISK    USROPN
002000020107     F                                     RENAME(TITPT000:TIOPT000)
002100061011     FTITPD01L  IF   E           K DISK    usropn
002200020107     FTIOPD01L  IF   E           K DISK    USROPN
002300020107     F                                     RENAME(TITPD000:TIOPD000)
002400020111
002500020107      * TARIFFE GIACENZE
002600061011     FTITGC01L  IF   E           K DISK    usropn
002700020107     FTIOGC01L  IF   E           K DISK    USROPN
002800020107     F                                     RENAME(TITGC000:TIOGC000)
002900020111
003000020107      * VISITE
003100091022     FTiVIS05L  IF   E           K DISK    usropn
003200020111
003300020107      * NOTE TARIFFE
003400020107     FTFNTC01L  IF   E           K DISK
003500020111
003600020107      * PIANO DEI CONTI
003700020107     FCNACO00F  IF   E           K DISK
003800020107     FCNIND00F  IF   E           K DISK
003900020107     FCNCLP00F  IF   E           K DISK
004000020107     FTFACO00F  IF   E           K DISK    USROPN
004100020107     F                                     RENAME(CNACO000:TFACO000)
004200020107     FTFIND00F  IF   E           K DISK    USROPN
004300020107     F                                     RENAME(CNIND000:TFIND000)
004400020107     FTFCLP00F  IF   E           K DISK    USROPN
004500020107     F                                     RENAME(CNCLP000:TFCLP000)
004600020111
004700020107      * FILE VARI
004800020107     FAZORG01L  IF   E           K DISK
004900020107     FTABEL00F  IF   E           K DISK
005000020107     FTNTBE01L  IF   E           K DISK
005100130905     fAZCMM01L  if   e           k disk
005200050209     fTivss02l  if   e           k disk
005300080211     fCnabi00f  if   e           k disk
005400101015     ftisis01l  if   e           k disk
005500020107     FTNTA50P   O    E             PRINTER oflind(*in80) INFDS(PRTTAR)
005600161223     F                                     usropn
005700020108
005800020108      *------------------------------------------------------------------*
005900020108      *                 S C H I E R E
006000020108      *------------------------------------------------------------------*
006100020107      * Messaggi di errore
006200050324     d msg             s             78    dim(17) ctdata perrcd(1)
006300020108      * Note
006400020108     d note            s             78    dim(4)
006500020108      * Scaglioni
006600020109     d sgl             s              7  2 dim(200)
006700020110      * Importo
006800020116     d itr             s              7  3 dim(7)
006900020109      * Zona tariffaria
007000020116     d  cts            s              2    dim(7)
007100020109      * Descrizione zona tariffaria
007200020116     d dcts            s             10    dim(7)
007300020110
007400020111      *------------------------------------------------------------------*
007500020111      *  TABELLE PROVINCIE E REGIONI
007600041220     D CCT             S              2    DIM(600)
007700041220     D SCT             S              2    DIM(600)
007800041220     D RCT             S              2    DIM(600)
007900041220     D NAR             S              3    DIM(600)
008000041220     D CRE             S              2    DIM(200)
008100041220     D DRE             S             20    DIM(200)
008200020110     D NAZ             S              3    DIM(20)
008300020111      * SCHIERE PER TARIFFE PARTICOLARI
008400020110     D FTC             S              1    DIM(50)
008500020110     D DTC             S             25    DIM(50)
008600020204     d dfe             s              1    dim(50)
008700020111      * ordinamento in stampa tariffe particolari
008800020110     D OST             S              2    DIM(50)
008900020110     D WOS             S              2    DIM(50)
009000020110     D TST             S              1    DIM(50)
009100020110     D CP              S              1    DIM(50)
009200020111      * estensione tabella 1P per stampa
009300020110     D EDTC            S             31    DIM(50)
009400020110     D EDAC            S              1    DIM(50)
009500020110     D ESUD            S              1    DIM(50)
009600020110
009700020111      *--------------- SCHIERE PER LA STAMPA -----------------
009800020111      *"/" di divisione negli scaglioni da a
009900020110     D SGLB            S              1    DIM(18)
010000020111      * SCAGLIONI senza decimali
010100020110     D SGLD            S              4  0 DIM(18)
010200020110     D SGLA            S              4  0 DIM(18)
010300020111      * SCAGLIONI con 1 decimale
010400020110     D SG1D            S              3  1 DIM(18)
010500020110     D SG1A            S              3  1 DIM(18)
010600020111      * SCAGLIONI con 2 decimali
010700020110     D SG2D            S              4  2 DIM(18)
010800020110     D SG2A            S              4  2 DIM(18)
010900020111      * DECODIFICA SOTTO GLI SCAGLIONI
011000020110     D ATF             S              7    DIM(18)
011100020111      * ------------------- 6 SCAGLIONI PER LA STAMPA --------------------------
011200020110     D SSLB            S              1    DIM(6)
011300020111      * SCAGLIONI SENZA DECIMALI
011400020110     D SSLD            S              4  0 DIM(6)
011500020110     D SSLA            S              4  0 DIM(6)
011600020111      * SCAGLIONI CON 1 DECIMALI
011700020110     D SS1D            S              3  1 DIM(6)
011800020110     D SS1A            S              3  1 DIM(6)
011900020111      * SCAGLIONI CON 2 DECIMALI
012000020110     D SS2D            S              4  2 DIM(6)
012100020110     D SS2A            S              4  2 DIM(6)
012200020111      * ------------------- 6 APPLICAZIONE TARIFFA FINITA ----------------------
012300020110     D STF             S              7    DIM(6)
012400020111      * ------------------- 6 SCAGLIONI E DETTAGLI TARIFFARI -------------------
012500020110     D SSL             S              7  2 DIM(6)
012600020110     D TCT             S              7  3 DIM(6)
012700020110     D TMN             S              7  3 DIM(6)
012800020110     D TMX             S              7  3 DIM(6)
012900020110     D TIN             S              7  3 DIM(6)
013000020111      * ------------------- SCHIERA DI COMODO ----------------------------------
013100020110     D COM             S              7    DIM(6)
013200020111      *  SKI TARIFFE X SCAGL. DI OGNI PROVINCIA(CTS) DELLA REGIONE
013300050616     D BIG             S             42    DIM(15)
013400050616     D BMN             S             42    DIM(15)
013500050616     D BMX             S             42    DIM(15)
013600050616     D BIN             S             42    DIM(15)
013700050616     D BSI             S              2    DIM(15)
013800050616     D BRG             S              1    DIM(15)
013900020111      * SKIERE DI WORK
014000020110     D WIN             S             42    DIM(30)
014100020110     D WMN             S             42    DIM(30)
014200020110     D WMX             S             42    DIM(30)
014300050616     D WCT             S              3    DIM(15)
014400020110     D WTI             S             42    DIM(30)
014500020111      * SKIERE DI STAMPA
014600020110     D PDE             S             29    DIM(30)
014700020110      *-------------------------------------------------------------------------
014800020110      *         SCHIERE SCAGLIONI DETTAGLIO TARIFFE PARTICOLARI
014900020110      *-------------------------------------------------------------------------
015000020111      * --------------  SCHIERE TARIFFE IN DETTAGLIO ------------------------
015100020111      * SCAGLIONI
015200020111
015300020110     D SCP             S              7  2 DIM(18)
015400020111      * TARIFFA PARTICOLARE DETTAGLIO
015500020110     D TAP             S              7  3 DIM(6)
015600020110     D TPN             S              7  3 DIM(6)
015700020110     D TPX             S              7  3 DIM(6)
015800020110     D AIP             S              1    DIM(6)
015900020111
016000020110     D PER             S              1    DIM(6)
016100020111
016200031120     D*-------------------------------------------------------------------------
016300101015      * DATE SCATTI ISTAT
016400101015     D DIA             S              8  0 DIM(999)
016500020108
016600020108      *------------------------------------------------------------------*
016700020108      *                 D S  M U L T I P L E
016800020108      *------------------------------------------------------------------*
016900020110     d sglitr          ds                  occurs(200) inz
017000020109     d  sglp                          7  2
017100020110     d  itr1                          7  3
017200020110     d  itr2                          7  3
017300020110     d  itr3                          7  3
017400020110     d  itr4                          7  3
017500020110     d  itr5                          7  3
017600020110     d  itr6                          7  3
017700020116     d  itr7                          7  3
017800020108
017900021205      *          SCHIERE PER STAMPA TARIFFE PARTICOLARI SU + RIGHE
018000021205     d TptA            s             31    Dim(50)
018100021205     d TptB            s             25    Dim(50)
018200021205     d Tptc            s             31    Dim(50)
018300020107      *-------------------------------------------------------------------------
018400020107      * DS DEL FILE TABEL00F
018500020107     D DSCV          E DS
018600020107     D DS1H          E DS
018700020107     D DS1P          E DS
018800020110     D DSSP          E DS
018900020107     D DS2G          E DS
019000020110     D DSCT          E DS
019100020107     D DS15          E DS
019200020107     D DSTR          E DS
019300020107     D DSFA          E DS
019400020111     D DS4U          E DS
019500020107      * DS DEL FILE TNTBE00F
019600020107     D DTAA          E DS
019700161222      * DS DEL FILE TNTBE00F allegati privacy
019800161222     D DATP          E DS
019900020107      * VALORI STANDARD AZIENDALI
020000020107     D DGEI          E DS
020100030508      * Dati dft tariffe FedEx
020200030508     d dDfe          e ds
020300020107      * DS flag operativi del file tariffe
020400020107     D DSTA01        E DS
020500020625      *--------
020600020625      * Ds di riferimento al file esterno AzUte00F
020700020625      *--------
020800020625     d AzuteDs       e ds                  ExtName(AzUte00F)
020900020625      *--------
021000020625      * Ds per dati organigramma
021100020625      *--------
021200020625     d DDatiUte      e ds
021300020625      *--------
021400020625      * Parametri per richiamo al pgm di controllo profilo utenti
021500020625      *--------
021600020625     d Tibs34Ds      e ds                  Inz
021700020625
021800020111
021900020107     D KPJBA         E DS
022000020107     D  utente                 1      5
022100161223      // - flag di errore nel caso in cui non sia riuscita a stampare
022200161223      //   gli allegati condizioni generali e privacy.
022300161223      // questo flag verrà getsito dal pgm fnlv43r che emetterà un msg di errore
022400161223     D err_allegati          497    497
022500161223     D*
022600020111
022700020107     D TIBS02DS      E DS
022800020111
022900020107     D FNLV45DS      E DS
023000021022     D fnlv41ds      E DS
023100041230     d Tibs10ds      e ds
023200020111
023300020107      *-------------------------------------------------------------------------
023400020107      * ds interne
023500020111
023600020107     D CDPKEY          DS
023700020107     D  CDPG                   1      3
023800020107     D  GGPG                   2      3
023900020107     D  CDLIN                  4      4
024000020111
024100020107     D PRTTAR          DS
024200020107     D  curline              367    368I 0
024300020111
024400161222     D*********
024500161222     D* DS DI PROCEDURA
024600161222     D*********
024700161222     D TRBMATRR1DS     ds                  qualified inz
024800161222     D   piCodRegola                  3A
024900161222     D   piCodOperat                 15A
025000161222     D   piUsrData                   10A
025100161222     D   piTipoModulo                10A
025200161222     D   piPrinter                   10A
025300161222     D   poEsitoOK                    1A
025400161222
025500161222
025600161222     D*********
025700161222     D* PROTOTIPI CHIAMATE ESTERNE
025800161222     D*********
025900161222     D*
026000161222     D TRBMATRR1       PR
026100161222     D                                     extpgm('TRBMATRR1')
026200161222     D piCodRegola                         like(TRBMATRR1DS.piCodRegola)
026300161222     D piCodOperat                         like(TRBMATRR1DS.piCodOperat)
026400161222     D piUsrData                           like(TRBMATRR1DS.piUsrData)
026500161222     D piTipoModulo                        like(TRBMATRR1DS.piTipoModulo)
026600161222     D piPrinter                           like(TRBMATRR1DS.piPrinter)
026700161222     D poEsitoOK                           like(TRBMATRR1DS.poEsitoOK)
026800161222     D*
026900161222
027000020107      *-------------------------------------------------------------------------
027100020107     D DATA_eur        S               D   DATFMT(*eur)
027200020111
027300020111      *-------------------------------------------------------------------------
027400020109     d rotcts          s                   like(tadcts)
027500020109     d rotlnp          s                   like(tadlnp)
027600020109     d ctrata          s                   like(§trata)
027700020110     d wrkata          s                   like(tamata)
027800020109
027900030730     d totrig          s              5  0
028000020109     d unoctr          s              1  0
028100020109     d k               s              3  0
028200020109     d x               s              2  0
028300051006     d xx              s              2  0
028400051006     d yy              s              2  0
028500051006     d wsgl            s              7  2
028600020109     d wsgl70          s              7  0
028700020109     d wsgl71          s              7  1
028800020109     d wsgl72          s              7  2
028900020109     d wsgl70d         s              7  0
029000020109     d wsgl71d         s              7  1
029100020109     d wsgl72d         s              7  2
029200020109     d wsgl70a         s              7  0
029300020109     d wsgl71a         s              7  1
029400020109     d wsgl72a         s              7  2
029500020110     d sav_sglp        s              7  2
029600020109     d w0020           s              2  0
029700020109     d w0030           s              3  0
029800020109     d w0050           s              5  0
029900060726     d len             s              5u 0
030000080211     d wagenzia        s             16
030100080211     d wbanca          s             20
030200080211     d wpos            s              2  0
030300080211     d wpos1           s              2  0
030400080211     d wpos2           s              2  0
030500020111
030600020111      *-------------------------------------------------------------------------
030700130905     d wCMMcod         s                   like(CMMcod)   inz
030800060801     d blanks          s                   like(destar)
030900010601     d cod             s                   like(tblcod)
031000010625     d costpg          s                   like(§fatpg)
031100010531     D ds1ksc          s                   like(tamksc)
031200010531     D ds1ctr          s                   like(tamctr)
031300010531     D ds1prg          s                   like(tamprg)
031400010601     D notapl          s                   like(ntcapl)
031500010621     D notke1          s                   like(ntcnk1)
031600010621     D notke2          s                   like(ntcnk2)
031700010601     D nottnt          s                   like(ntctnt)
031800161222     D kcod            s                   like(tbecod)
031900010601     D key             s                   like(tblkey)
032000010601     D ksc             s                   like(acoksc)
032100010724     D oltre           s                   like(tpdsgl)
032200010531     D parsks          s                   like(tamksc)
032300060313     d savapl          s                   like(tptapl)
032400010618     D savftc          s                   like(tptftc)
032500010621     D savorp          s                   like(tpdorp)
032600010801     D savorp1         s                   like(tpdorp)
032700010621     D savsgl          s                   like(tpdsgl)
032800010806     D recsgl          s                   like(tpdsgl)
032900010604     D wrkcts          s                   like(tadcts)
033000010626     D wrkftc          s                   like(tpdftc)
033100010626     D wrkorp          s                   like(tpdorp)
033200010626     D wrksgl          s                   like(tpdsgl)
033300110127     D wrpv            s                   like(tadrpv)
033400010531      *-------------------------------------------------------------------------
033500020111     D a               s              2  0
033600021205     d atp1            s              2  0
033700021205     d atpc            s              2  0
033800010614     D b               s              2  0
033900021205     D blk10           s             10    INZ(*ALL'.')
034000030318     D blk12           s             12    INZ(*ALL'.')
034100030318     D blk21           s             21    INZ(*ALL'.')
034200030318     D blk25           s             25    INZ(*ALL'.')
034300130905     ***D blk36           s             36    INZ(*ALL'.')
034400021205     D blk56           s             56    INZ(*ALL'.')
034500010614     D c               s              2  0
034600021205     d CntList         s              5i 0 Inz(*Zeros)
034700161222     D coda_allegati   s             10
034800010615     D codorp          s              1
034900010615     D codreg          s              2
035000010615     D codsig          s              2
035100010601     D comfil          s              3  0
035200010628     D comftc          s              1
035300010613     D compro          s              1
035400010613     D comreg          s              2
035500010621     D com04           s              4
035600010614     D como29          s             29
035700010614     D como42          s             42
035800021205     d conta           s              1  0
035900010614     D cmno42          s             42
036000010614     D cmxo42          s             42
036100010601     D datastampa      s             10
036200010723     D desctr          s             13
036300010615     D desreg          s             14
036400010618     D de1tpg          s              9
036500010618     D de3tpg          s             13
036600010801     D dissca2         s             63
036700010801     D dissca3         s             50
036800010801     D dissca4         s             37
036900010801     D dissca5         s             24
037000010801     D dissca6         s             11
037100010608     D ds3um           s             20
037200010531     D ds6pr2          s              1
037300010621     D errmsg          s             78
037400010614     D h               s              2  0
037500020110     D j               s              3  0
037600020110     D jj              s              3  0
037700020110     D jjj             s              3  0
037800010724     D mintas          s              3  0
037900010619     D num4            s              4  0
038000010614     D p               s              2  0
038100010531     D k1              s              3  0
038200010618     D k2              s              3  0
038300010607     D k4              s              3  0
038400010622     D k5              s              3  0
038500010604     D Y               s              3  0
038600010625     D recval          s              3  0
038700161222     D regola          s              3
038800010604     D rotpro          s              1
038900010613     D rotreg          s              2
039000010531     D staddt          S             10
039100010531     D stadst          S             10
039200020624     d stalnp          s             20
039300010725     D stpanno         s              4  0
039400010725     D stpmod          s              3  0
039500161222     D stpannoi        s              4  0
039600161222     D stpmodi         s              3  0
039700030318     D stpper          s              7
039800010731     D unmis           s              3
039900030318     D valper          s              6  3
040000010615     D waro2           s              4  0
040100010615     D warf2           s              4  0
040200010615     D warl2           s              4  0
040300140724     d wata            s              5  0
040400140724     d wata1           s              6  1
040500140724     d wata2           s              7  2
040600051206     d wema84          s              1    Inz('0')
040700050527     d wema85          s              1    Inz('0')
040800050527     d wema87          s              1    Inz('0')
040900050527     d wema88          s              1    Inz('0')
041000090618     d wema89          s              1    Inz('0')
041100021205     d Witr            s              8  3
041200010618     D wnumsc          s              2  0
041300050930     d woktar          s              1    Inz('0')
041400021205     d WokTp           s              1    Inz('0')
041500041230     d wpsw            s              1    Inz('0')
041600010625     D w_per           s              1
041700010626     D wrkdestpg       s             13
041800010604     D wrkreg          s              2
041900010604     D wrkpro          s              1
042000011001     D w_ellimris      S              1
042100021205     D w_tarpar        S             83
042200021205     D w_tarparest     S             83
042300010723     D w0010           S              1  0
042400010601     D w003a           S              3
042500060925     d wctr            s              2  0
042600060925     d wctr1           s              1
042700050201     d woklnp          s              1    inz(*off)
042800050201     d wokstp          s              1    inz(*off)
042900091112     d $trattativa     s              1n
043000101015     d $EoF            s              1n   inz(*off)
043100110127     d Wvolta          s              1n   inz(*off)
043200021205      *-----------------
043300021205      * PARAMETRI DI PASSAGGIO AL PROGRAMMA DI "GIUSTIFICAZIONE" TESTO
043400021205      *-----------------
043500021205     d TxtInput        S            200    varying
043600021205     d Lungh           S              5i 0 inz(105)
043700021205     d TxtGiust        S            200    varying
043800021205     d TxtResto        S            200    varying
043900020111
044000020111      * DEFINIZIONE COSTANTI
044100060726     D CSPE1           C                   CONST('solo sulle spese di sosta')
044200010625     D CSPE2           C                   CONST('su tutte le spese')
044300060726     D CSPE3           C                   CONST('su spese sosta, dossier, -
044400060726     D                                     varie')
044500060726     D CSPE12          C                   CONST('only on holding costs')
044600060704     D CSPE22          C                   CONST('on all costs')
044700060726     D CSPE32          C                   CONST('on holding, dossier, -
044800060726     D                                     various costs')
044900060726     D CSPE13          C                   CONST('uniquement sur les frais -
045000060726     D                                     d''arrêt')
045100060726     D CSPE23          C                   CONST('sur tous les frais')
045200060726     D CSPE33          C                   CONST('sur les frais d''arrêt, de -
045300060726     D                                     dossier, divers')
045400060726     D CSPE14          C                   CONST('nur auf Kosten für -
045500060726     D                                     Aufbewahrung')
045600060726     D CSPE24          C                   CONST('auf alle Kosten')
045700060726     D CSPE34          C                   CONST('uf Kosten für Aufbewahrung,-
045800060726     D                                     Bearbeitung,Verschiedenes')
045900091022
046000091022     itivis000
046100091022     i              visesi                      wisesi
046200091022
046300020108      *------------------------------------------------------------------*
046400020108      *               RIEPILOGO INDICATORI
046500020108      *------------------------------------------------------------------*
046600020111      * 08    - STAMPA MINIMO TASSABILE PER TARIFFA A Q.LE
046700020111      * 09    - TIPO PAGAMENTO A VALORE (TARIFFA %) TARIFFE PARTICOLARI
046800020111      * 18    - STAMPA ARROTONDAMENTI
046900020111      * 19    - STAMPA RAPPORTO PESO VOLUME
047000020111      * 21    - MANDATO A VALORE VARIABILE
047100020111      * 22    - ELEVAZIONE LIMITE RISARCIBILE
047200020111      * 24    - MANDATO FITTIZIO
047300020111      * 26    - CONDIZIONA LA STAMPA DEL "PER" DI FIANCO ALLA VALIDITA' TARIFFA
047400041129      * 27    - CONDIZIONA LA STAMPA DELL'ADDIZIONALE DI GESTIONE 1° riga
047500020111      * 28    - CONDIZIONA LA STAMPA DELLE SPESE GIACENZA VARIE A SPEDIZIONE
047600041129      * 29    - CONDIZIONA LA STAMPA DELL'ADDIZIONALE DI GESTIONE 2° riga
047700020111      * 31    - INDICATORE DI COMODO
047800020109      * 35    - INDICATORE DI COMODO LOOKUP
047900020108      * 37    - SCAGLIONI CON UN SOLO DECIMALE
048000020111      * 40/46 - CONDIZIONA LA STAMPA DEGLI SCAGLIONI
048100020111      * 48    - STAMPO IL MINIMO
048200020111      * 49    - STAMPO IL MASSIMO
048300020110      * 50    - CONDIZIONA LA STAMPA DEL 'OLTRE' NEGLI SCAGLIONI
048400020111      * 51/56 - CONDIZIONA LA STAMPA DEGLI SCAGLIONI VUOTI
048500020111      * 57    - STAMPO INOLTRO
048600020111      * 61/66 - CONDIZIONA LA STAMPA DEL DETTAGLIO TARIFFARIO
048700020111      * 72    - CONDIZIONA LA STAMPA DELL'ADDEBITO SPESE DI INCASSO
048800020108      * 73    - CONDIZIONA LA STAMPA DEL RESPONSABILE TRASPORTI
048900020111      * 75    - STAMPO MINIMO
049000020108      * 77    - CONDIZIONA LA STAMPA DEL E.P.C.
049100020109      * 78/79 - CONDIZIONA LA STAMPA DELLA TESTATA DEGLI SCAGLIONI
049200020109      * 80    - OVERFLOW
049300020111      * 81    - CONDIZIONA LA STAMPA DELLA TESTATA ASSICURAZIONI MERCE
049400091117      * 85    - Trattativa in stampa
049500020111      * 86    - STAMPO MASSIMO
049600020108      * 87    - TARIFFA A VALORE
049700020111      * 88    - ARRONDAMENTI CON DECIMALI
049800020108      * 91    - SE ACCESO SCAGLIONI CON DECIMALI
049900020111      * 92/97 - STAMPA DATI DALLA DS1H
050000020108      * 98    - ON PROVENGO DA OFFERTA
050100020108      * 99    - INDICATORE GENERALE DI ERRORE
050200020111      *****************************************************************
050300020108
050400020108      *------------------------------------------------------------------*
050500010531      *    T E S T A T A    S T A M P A    T A R I F F A
050600020108      *------------------------------------------------------------------*
050700021022      *
050800021022      * Per errore di reperimento tetso vado a fine pgm
050900021022     C                   if        $NoStp   = *on
051000021022     C                   goto      fine
051100021022     C                   endif
051200021022      *
051300050930     c                   eval      woktar = *Off
051400020108
051500020108      * Recupero il cliente
051600020108     c                   exsr      Sr_Reccli
051700020108
051800020108      * Se 99 acceso errore e vado a fine PGM non trovato cliente in anafragica
051900020108
052000020108     c                   if        *in99
052100020108     c                   goto      fine
052200020108     c                   endif
052300020108
052400020108      * Decodifico i dati del cliente
052500020108
052600020108     c                   exsr      Sr_Decana
052700020108
052800020108      * Recupero note tariffa/offerta cliente e responsabile trasporti
052900020108
053000020108     c                   exsr      Sr_Recnot
053100020108
053200020108      * Decodifico i dati del punto operativo
053300020108
053400020108     c                   exsr      Sr_Decpo
053500020108
053600020108      * aggancio testata tariffa / offerta
053700020108     c                   eval      ds1ksc = parsks
053800020108     c                   eval      ds1prg = i45prg
053900020108     c                   eval      ds1ctr = i45ctr
054000020108
054100061011     c  n98ds1ktm        chain     tntam000
054200061011     c   98ds1ktm        chain     tnofm000
054300020108
054400020108      * decodifica delle condizioni generali
054500020108
054600020108     c                   exsr      Sr_Dectam
054700020108
054800020108      * stampo la prima parte della tariffa
054900020108
055000020107     c                   write     ta50pPP
055100020107     c                   write     ta50p01
055200020108
055300020108      * elaboro dettaglio tariffario per recuperare scaglioni
055400020108
055500020108     c                   exsr      Sr_Scaglioni
055600020109      * se si sono vericati errori non continuo
055700020109     c                   if        *in99
055800020109     c                   goto      fine
055900020109     c                   endif
056000020108
056100020108      * elaboro e stampo il dettaglio tariffario
056200020108
056300020108     c                   exsr      Sr_Eladet
056400020108      * se si sono vericati errori non continuo
056500020108     c                   if        *in99
056600020108     c                   goto      fine
056700020108     c                   endif
056800020110
056900020110      * stampo arrotondamenti
057000020110     c                   exsr      Sr_Arroto
057100020110
057200020110      * stampo i dati della testata tariffa
057300020110     c                   exsr      Sr_Statam
057400050930     c                   eval      woktar = *On
057500020110
057600020110      * routine gestione tariffe particolari
057700020110     c                   exsr      Sr_Statpt
057800021205
057900021205      * routine stampa sk unica tariffe particolari
058000021205     c                   ExSr      Sr_StaTpt1
058100020110
058200020110      * routine di gestione tariffe giacenza
058300020110     c                   exsr      Sr_Statgc
058400020110
058500020110      * stampo condizioni di pagamento
058600020110     c                   exsr      Sr_Over
058700020110     c                   write     ta50pcon
058800020110
058900020110      * se pagamento che utilizza la banca stampo anche questa riga
059000091016      * pagamento RIBA = '1' RID = '2'
059100091016     c***                if        costpg <> '0'
059200091016     c                   if        costpg = '1' or costpg = '2'
059300020110     c                   exsr      Sr_Over
059400020110     c                   write     ta50pcon1
059500020110     c                   endif
059600021204      * Per tutte le tariffe stampo dicitura assicurazione merce
059700150909     c                   Add       4             curline
059800030930     c                   ExSr      Sr_Over
059900021204     c                   Write     Ta50pam
060000040211
060100040211      * Stampo paragrafo x telefono destinatario
060200040216     c                   Add       3             curline
060300040211     c                   ExSr      Sr_Over
060400040211     c                   Write     Ta50pte
060500050103
060600050103      * stampa servizi ON LINE se offerta
060700050103     c                   If        *In98
060800050527     c                   ExSr      Sr_Comunica
060900050103     c                   EndIf
061000020110
061100041230      * stampa condizioni
061200161222     c                   Add       4             curline
061300161228      * per il tedesco ci sono 2 righe in più
061400161228     c                   IF        I45lin = '4'
061500161228     c                   Add       2             curline
061600161228     c                   ENDIF
061700020110     c                   exsr      Sr_Over
061800020110     c                   write     ta50pe1
061900041230
062000041230      * stampa condizioni e firme finali
062100020110
062200020110     c                   exsr      Sr_Over
062300020110     c                   write     ta50pfi
062400020110
062500020110     c                   exsr      Sr_Over
062600020110     c                   write     ta50pe2
062700020110
062800020110     c                   exsr      Sr_Over
062900020110     c                   write     ta50pe2B
063000020110
063100020110     c                   exsr      Sr_Over
063200020110     c                   write     ta50pe2C
063300020110
063400020110     c                   exsr      Sr_Over
063500020110     c                   write     ta50pe2D
063600020110
063700020110     c                   exsr      Sr_Over
063800020110     c                   write     ta50pe2E
063900020110
064000020110     c                   exsr      Sr_Over
064100020110     c                   write     ta50pe2F
064200110905
064300110905     c                   exsr      Sr_Over
064400110905     c                   write     ta50pe2G
064500020110
064600020110     c                   exsr      Sr_Over
064700020110     c                   write     ta50pfi
064800161228     c                   eval      page = page - 1
064900161228     c                   write     ta50pfine
065000161223
065100161223     c                   close     tnta50p
065200161223      *
065300161223      * Stampo allegati condizioni generali e privacy se non mando la tariffa
065400161223      * via e-mail
065500161223
065600161223     c                   If        I45fax <> 'M'
065700161222      *
065800161222      * Stampo allegati codnizioni generali e privacy
065900161222
066000161222      * Verifico in che lingua devo stampare gli allegati
066100161222     c                   If        i45lin = 'I'
066200161222     c                   eval      regola = 'TM1'
066300161222     c                   else
066400161222     c                   eval      regola = 'TM' + i45lin
066500161222     c                   endif
066600020109
066700161222      /free
066800161222
066900161222        TRBMATRR1DS.piCodRegola  = regola;
067000161222        TRBMATRR1DS.piCodOperat  = '*ATTACH*';
067100161222        TRBMATRR1DS.piUsrData    = 'ALLEGATI';
067200161223        TRBMATRR1DS.piTipoModulo = 'LET1';
067300161222        TRBMATRR1DS.piPrinter    = coda_allegati;
067400161222        TRBMATRR1DS.poEsitoOK    = *blanks;
067500161222
067600161222        callP(e) TRBMATRR1 (TRBMATRR1DS.piCodRegola
067700161222                           :TRBMATRR1DS.piCodOperat
067800161222                           :TRBMATRR1DS.piUsrData
067900161222                           :TRBMATRR1DS.piTipoModulo
068000161222                           :TRBMATRR1DS.piPrinter
068100161222                           :TRBMATRR1DS.poEsitoOK
068200161222                           );
068300161223        err_allegati = TRBMATRR1DS.poEsitoOK ;
068400161222
068500161222      /End-free
068600161222      *
068700161223     c                   endif
068800161222      *
068900020109     c     fine          tag
069000020109     c                   seton                                        lr
069100020109
069200020108      *------------------------------------------------------------------*
069300020108      *   Sr_Reccli       RECUPERO CLIENTE
069400020108      *------------------------------------------------------------------*
069500020108
069600020108     c     Sr_Reccli     begsr
069700020108
069800020108      * V I S I T A
069900020108
070000020108     c                   if        *in98
070100020108
070200110104     c**                 if        i45ast = 'V'
070300110104     c**   i45nrv        chain     tnvis000
070400110104     c**                 endif
070500091022     c                   if        i45ast = 'T'
070600091022     c     i45nrv        chain     tivis000
070700091022     c                   endif
070800020108      * recupero ANAGRAFICA CLIENTE
070900020108      * se cliente nuovo leggo nel file anagrafici visite
071000020108     c                   z-add     visnrv        ksc
071100020108     c     ds7kac        chain     tfaco000                           99
071200020108     c  n99ds7kac        chain     tfind000                           99
071300020108     c  n99ds7kac        chain     tfclp000                           99
071400020108      * se non trovato cerco nell'anagrafica clienti
071500020108     c                   if        *in99
071600020108     c                   z-add     visksc        ksc
071700020108     c     ds7kac        chain     cnaco000                           99
071800020108     c  n99ds7kac        chain     cnind000                           99
071900020108     c  n99ds7kac        chain     cnclp000                           99
072000020108     c                   end
072100020108
072200020108     c                   end
072300020108
072400020108      * RECUPERO IL CLIENTE
072500020108
072600020108      * T A R I F F A
072700020108
072800020108     c                   if        not *in98
072900020108     c                   z-add     parsks        ksc
073000020108     c     ds7kac        chain     cnaco000                           99
073100020108     c  n99ds7kac        chain     cnind000                           99
073200020108     c  n99ds7kac        chain     cnclp000                           99
073300020108     c                   endif
073400020108      * se il cliente sia in visita che in tariffa non è stato trovato
073500020108      * viene stampato l'errore
073600020108     c                   if        *in99
073700020108     c                   move      msg(3)        errmsg
073800020108     c                   exsr      Staerr
073900020108
074000010622     c                   else
074100010622      * se non c'è errore valorizzo i campi in stampa
074200010622     c                   eval      picrag = acorag
074300010622     c                   eval      picind = indvia
074400010622     c                   eval      piccap = indcae
074500010622     c                   eval      picloc = indcit
074600010622     c                   eval      picpro = indprv
074700010601     c                   endif
074800020108
074900020108     c                   endsr
075000020108
075100020108      *------------------------------------------------------------------*
075200020108      *   Sr_Decana       DECODIFICA DATI CLIENTI PER LA STAMPA
075300020108      *------------------------------------------------------------------*
075400020108
075500020108     c     Sr_Decana     begsr
075600020108      * valorizzo i numeri riferimento
075700020108     c                   eval      picnr1 = i45nr1
075800020108     c                   eval      picnr2 = i45nr2
075900130905      *
076000020108      * decodfico la nazione se diversa da italia
076100130905      *
076200020108     c                   if        indsta <> *blanks
076300020108     c                   eval      cod ='15'
076400020108     c                   movel     indsta        w003a
076500020108     c                   movel(p)  w003a         key
076600060724     c     tabkeyl       chain     tabel00f
076700020108     c                   if        %found(tabel00f)
076800020108     c                   movel     tbluni        desnar
076900020108     c                   else
077000020108     c                   clear                   desnar
077100020108     c                   endif
077200130905      *
077300020108     c                   endif
077400130905      *
077500020108      * decodifico il commerciale
077600130905     c                   clear                   wCMMcod
077700020108      * visita
077800020108     c                   if        *in98
077900020108      * commerciale visita
078000020108     c                   if        viscmm > 0
078100130905     c                   z-add     viscmm        wCMMcod
078200020108     c                   endif
078300020108      * tariffa
078400020108     c                   else
078500020108      * commerciale tariffa
078600020108     c                   if        clpage > 0
078700130905     c                   z-add     clpage        wCMMcod
078800020108     c                   endif
078900130905      *
079000020108     c                   endif
079100130905      *
079200130905     c                   if        wCMMcod <> *zero
079300130905      *
079400130905     c     wCMMcod       chain     AZCMM000
079500130905     c                   if        %found(AZCMM01L)
079600130905     c                   movel     CMMdes        descom
079700020108     c                   else
079800020108     c                   clear                   descom
079900020108     c                   endif
080000130905      *
080100020108     c                   endif
080200020108
080300020108      * decodifico il pagamento
080400020108
080500020108     c                   if        indcdp <> *blanks
080600020108     c                   move      indcdp        cdpg
080700020108     c                   eval      cdlin = '1'
080800020108     c                   movel(p)  cdpkey        key
080900020108     c                   eval      cod = 'FA'
081000020108
081100060704     c     tabkeyl       chain     tabel00f
081200020108     c                   if        %found(tabel00f)
081300020108     c                   movel     tbluni        dsfa
081400020108     c                   movel     §fades        descdp
081500020108      * mi salvo il tipo pagamento per pilotare la stampa della banca d'appoggio
081600020108      * oppure (es. Rimessa diretta non si stampa la banca d'appoggio)
081700020108     c                   eval      costpg = §fatpg
081800020108     c                   endif
081900020108
082000020108     c                   endif
082100020108      * converto spese di incasso in euro
082200020108     c                   if        indsin > *zeros
082300020108
082400020108     c                   seton                                        72
082500030210
082600030210     c                   Move      IndSin        ValSin
082700020108
082800020108     c                   endif
082900020108      * banca d'appoggio
083000080211     c                   clear                   pcabi
083100080211     c                   clear                   pccab
083200080211     c                   clear                   desbna
083300080211    1c                   if        indabi <> 99999 or indcab <> 99999
083400080211     c                   eval      pcabi = indabi
083500080211     c                   eval      pccab = indcab
083600080211     c     kcnabi        chain     cnabi00f
083700080211    2c                   if        %found(cnabi00f) and abiann <> '*'
083800080211     c                   clear                   wagenzia
083900080211     c                   clear                   wbanca
084000080211     c                   Eval      wbanca = abiist
084100080211      * Compongo il nome della banca con località o comune
084200080211    3c                   If        abiage = *Blanks
084300080211     c                   Select
084400080211     c                   When      abiloc <> *Blanks
084500080211     c                   Eval      wagenzia = abiloc
084600080211     c                   When      abicom <> *Blanks
084700080211     c                   Eval      wagenzia = abicom
084800080211     c                   EndSl
084900080211      * Compongo il nome della banca con agenzia + località o comune
085000080211   x3c                   Else
085100080211      * Cerco il primo byte vuoto dell'agenzia
085200080211     c                   Eval      wpos = %checkr(' ':abiage)
085300080211
085400080211     c                   Eval      wagenzia = %subst(abiage:1:wpos)
085500080211     c                   Eval      wpos1 = wpos + 2
085600080211     c                   Eval      wpos2 = 16 - wpos1
085700080211      * Se wpos2 inferiore a zero lo imposto a 0
085800080211     c                   If        wpos2 < *Zeros
085900080211     c                   Clear                   wpos2
086000080211     c                   EndIf
086100080211      * Se posso aggiungere anche solo 1 carattere procedo
086200080211    4c                   If        wpos > 1 and wpos < 16
086300080211      * Cerco di riempire quello che resta con la località o il comune
086400080211     c                   Select
086500080211     c                   When      abiloc <> *Blanks
086600080211     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
086700080211     c                             %subst(abiloc:1:wpos2)
086800080211     c                   When      abicom <> *Blanks
086900080211     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
087000080211     c                             %subst(abicom:1:wpos2)
087100080211     c                   EndSl
087200080211    4c                   EndIf
087300080211
087400080211    3c                   EndIf
087500080211    2c                   EndIf
087600080211     c                   eval      desbna = wbanca + wagenzia
087700080211    1c                   endif
087800020108
087900020108      * decodifico tipo pagamento contrassegno
088000020108
088100020108     c                   eval      cod = '4U'
088200020108     c                   movel(p)  clpfpc        key
088300060704     c     tabkeyl       chain     tabel00f
088400020108     c                   if        %found(tabel00f)
088500020108     c                   movel     tbluni        ds4u
088600020109     c                   movel     §4udes        comaas            9
088700020108      * se è un tipo pagamento che richiede obbligatoriamente il codice ABI CAB
088800020108      * stampo anche la decodifica della banca d'appoggio
088900020108     c                   if        §4uabi = 'S'
089000080118     c                   eval      combab1 = %subst(clpbab:1:4)
089100080118     c                   eval      combab2 = %subst(clpbab:5:4)
089200080118     c                   eval      combab3 = %subst(clpbab:9:4)
089300080118     c                   eval      combab4 = %subst(clpbab:13:4)
089400080118     c                   eval      combab5 = %subst(clpbab:17:4)
089500080118     c                   eval      combab6 = %subst(clpbab:21:4)
089600080118     c                   eval      combab7 = %subst(clpbab:25:4)
089700080211     c                   eval      combab8 = %subst(clpbab:29:4)
089800020108     c                   endif
089900020108
090000020108     c                   endif
090100020108
090200020108     c                   endsr
090300020108
090400020108      *------------------------------------------------------------------*
090500020108      *   Sr_Recnot       RECUPERA NOTE TARIFFE/OFFERTA CLIENTI
090600020108      *------------------------------------------------------------------*
090700020108
090800020108     c     Sr_Recnot     begsr
090900020108
091000020108      * chiave note tariffe/offerta cliente
091100020108
091200020108      * visita
091300020108     c   98              eval      notapl = 'V'
091400020108      * tariffa
091500020108     c  n98              eval      notapl = 'C'
091600091112      * trattativa
091700091112     c                   if        $trattativa
091800091112     c                   eval      notapl = 'T'
091900091112     c                   endif
092000020108
092100020108     c                   clear                   b
092200020108
092300020108     c                   movel     i45kcc        notke1
092400020108     c                   move      parsks        notke1
092500020108      * note legate alla tariffa/offerta
092600020108     c                   movel     '10'          nottnt
092700020108     c                   movel     i45ctr        notke2
092800020108
092900020108     c     keyntc        setll     tfntc01l
093000020108     c                   do        *hival
093100020108     c     keyntc        reade     tfntc01l
093200020108
093300020108     c                   if        %eof(tfntc01l)
093400020108      * imposto la chiave senza il codice tariffa e mi riposiziono
093500020108     c                   clear                   notke2
093600020108     c     keyntc        setll     tfntc01l
093700020108     c                   leave
093800020108     c                   endif
093900020108
094000020108     c                   if        ntcflt = 'A'
094100020108      * se è annullato leggo il successivo
094200020108     c                   iter
094300020108     c                   else
094400010601
094500020108      * se invece non è annullato ed esiste mi riposiziono per il caricamento
094600020108      * della schiera di comodo delle note
094700020108     c     keyntc        setll     tfntc01l
094800020108     c                   leave
094900020108     c                   endif
095000020108
095100020108     c                   enddo
095200020108      * carico le note in schiera
095300020108     c                   do        *hival
095400020108     c     keyntc        reade     tfntc01l
095500020108
095600020108     c                   if        %eof(tfntc01l)
095700020108     c                   leave
095800020108     c                   endif
095900020108      * se annullato oppure è una nota interna leggo la successiva
096000020108     c                   if        ntcflt = 'A' or ntcsns <> 'S'
096100020108     c                   iter
096200020108     c                   endif
096300020108
096400020108     c                   add       1             b
096500020108     c                   movel     ntcrnt        note(b)
096600020108      * se b = 4 numero massimo di note da stampare vado a fine
096700020108     c                   if        b = 4
096800020108     c                   leave
096900020108     c                   endif
097000020108
097100020108     c                   enddo
097200020108
097300020108      * Responsabile trasporti
097400100923     c                   clear                   notke2
097500091113
097600091113      * se trattativa su cliente cerco con il cliente
097700091113     c                   if        $trattativa and visksc > 0
097800091113     c                   eval      notapl = 'C'
097900091113     c                   move      visksc        notke1
098000091113     c                   endif
098100020108
098200020108     c                   movel     '05'          nottnt
098300020108      * cerco prima con in chiave il codice tariffa
098400020108     c     keyntc        setll     tfntc01l
098500020108     c                   do        *hival
098600020108     c     keyntc        reade     tfntc01l
098700020108
098800020108     c                   if        %eof(tfntc01l)
098900020108     c                   leave
099000020108     c                   endif
099100020108      * se annullato oppure è una nota interna leggo la successiva
099200020108     c                   if        ntcflt = 'A' or ntcsns <> 'S'
099300020108     c                   iter
099400020108     c                   endif
099500020108      * se alla c.a. è uguale a blank carico la nota
099600020108     c                   if        resptra = *blanks
099700020108     c                   movel     ntcrnt        resptra
099800020108     c                   seton                                        73
099900020108      *
100000020108     c                   else
100100020108      * se è già pieno scrivo nel nominativo del e.p.c. ed esco
100200020108     c                   movel     ntcrnt        resptra2
100300020108     c                   seton                                        77
100400020108     c                   leave
100500020108     c                   endif
100600020108
100700020108     c                   enddo
100800020108
100900020108     c                   endsr
101000020108
101100020108      *------------------------------------------------------------------*
101200020108      *   Sr_Decpo        DECODIFICA I DATI DEL PUNTO OPERATIVO
101300020108      *------------------------------------------------------------------*
101400020108
101500020108     c     Sr_Decpo      begsr
101600020108
101700020108      * Se visita e cliente nuovo recupero la filiale dai primi 3 campi del
101800020108      * codice commerciale
101900020108
102000020108     c                   if        *in98 and visksc = 0
102100020108     c                   movel     viscmm        comfil
102200020108     c                   endif
102300020108      * Se visita di un cliente esistente recupero il p.o. appartenenza
102400020108      * cliente
102500020108
102600020108     c                   if        *in98 and visksc > 0
102700020108     c                   movel     visksc        comfil
102800020108     c                   endif
102900020108
103000020108      * se tariffa recupero dal codice cliente
103100020108
103200010601     c                   if        not *in98
103300020108     c                   movel     parsks        comfil
103400020108     c                   endif
103500020108
103600020108      * decodifica
103700020108
103800020108     c     comfil        chain     azorg01l
103900020108
104000020108     c                   if        %found(azorg01l)
104100020108     c                   movel     orgdes        td1fil
104200020108     c                   movel     orgind        td1via
104300020108     c                   movel     orgcpf        td1cap
104400020108     c                   movel     orgloc        td1loc
104500020108     c                   movel     orgpro        td1prv
104600020108     c                   movel     orgtel        td2tel
104700020108     c                   movel     orgfax        td2tfx
104800020108     c                   endif
104900020108
105000020108      * costruisco la data stampa
105100020108     c                   movel     i45gds        data_eur
105200020108     c     *eur          movel     data_eur      datastampa
105300020108     c                   eval      picdal = %trim(orgdes) + ',li ' + datastampa
105400060704
105500060704      * imposto nelle varie lingue
105600060704     c                   select
105700060704     c                   when      i45lin = 'I'
105800060704     c                   eval      picdal = %trim(orgdes) + ',li ' + datastampa
105900060704     c                   when      i45lin = '2'
106000060704     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
106100060801     c                   when      i45lin = '3'
106200060801     c                   eval      picdal = %trim(orgdes) + ',le ' + datastampa
106300060712     c                   when      i45lin = '4'
106400060710     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
106500060704     c                   endsl
106600020108
106700020108     c                   endsr
106800020108
106900020108      *------------------------------------------------------------------*
107000020108      *   Sr_Dectam       DECODIFICA DELLA TESTATA TARIFFA
107100020108      *------------------------------------------------------------------*
107200020108
107300020108     c     Sr_Dectam     begsr
107400020108
107500020108     c                   movel     tamflo        dsta01
107600020108
107700020108      * se la tariffa non è FedEx vado a fine PGM
107800020108     c                   if        §tafed <> 'S'
107900020108     c                   goto      fine
108000020108     c                   endif
108100030508
108200030508      * tariffa merci/documenti
108300030508     c                   Move      TamCtr        WCtr
108400030508If  2c                   If        WCtr >= §Dfedalm and WCtr <= §Dfealm
108500030508     c                   Eval      *in05 = *On
108600030508    2c                   EndIf
108700030508If  2c                   If        WCtr >= §Dfedald and WCtr <= §Dfeald
108800030508     c                   Eval      *in05 = *Off
108900030508    2c                   EndIf
109000060704
109100060704      * imposto l'intestazione della tariffa
109200060704      * tariffa merci
109300060704     c                   if        *in05
109400060704     c                   select
109500060704     c                   when      i45lin = 'I'
109600060704     c                   eval      destar = 'TARIFFA FedEx Merci'
109700060704     c                   when      i45lin = '2'
109800060704     c                   eval      destar = 'TARIFF FedEx Goods'
109900060726     c                   when      i45lin = '3'
110000060726     c                   eval      destar = 'TARIF FedEx Marchandises'
110100060712     c                   when      i45lin = '4'
110200060726     c                   eval      destar = 'PREIS FedEx Ware'
110300060704     c                   endsl
110400060704      * tariffa documenti
110500060704     c                   else
110600060704     c                   select
110700060704     c                   when      i45lin = 'I'
110800060704     c                   eval      destar = 'TARIFFA FedEx Documenti'
110900060704     c                   when      i45lin = '2'
111000060704     c                   eval      destar = 'TARIFF FedEx Documents'
111100060726     c                   when      i45lin = '3'
111200060726     c                   eval      destar = 'TARIF FedEx Documents'
111300060712     c                   when      i45lin = '4'
111400060726     c                   eval      destar = 'PREIS FedEx Briefpost/Dokumente'
111500060704     c                   endsl
111600060704     c                   endif
111700060726      * centro la descrizione della tariffa
111800060801     c                   eval      len = (%len(destar) -
111900060801     c                                    %len(%trimr(destar))) / 2
112000060801     c                   eval      destar = %subst(blanks:1:len) +
112100060801     c                                      %trimr(destar)
112200020108
112300020108      * data decorrenza in stampa
112400020108     c     *iso          movel     tamddt        data_eur
112500020108     c     *eur          movel     data_eur      staddt
112600020108      * data scadenza in stampa
112700020108     c     *iso          movel     tamdst        data_eur
112800020108     c     *eur          movel     data_eur      stadst
112900010531
113000020108      * recupero la divisa per decodifica in stampa
113100020108     c                   eval      cod= 'CV'
113200020108     c                   movel(p)  §tadiv        key
113300060704     c     tabkeyl       chain     tabel00f
113400020108     c                   if        %found (tabel00f)
113500020108     c                   movel     tbluni        dscv
113600020108     c                   movel     §cvdes        desdiv
113700020108     c                   endif
113800020108
113900020108      * decodifico la tariffa cliente
114000020108     c                   movel     i45ctr        unoctr
114100020108     c                   movel(p)  unoctr        key
114200020108     c                   eval      cod = 'TR'
114300060704     c     tabkeyl       chain     tabel00f
114400020108     c                   if        %found(tabel00f)
114500020108     c                   movel     tbluni        dstr
114600020108      * valorizzo i campi descrittivi di stampa
114700020108     c                   movel     §trdst        desctr
114800020110     c                   movel     §trde1        desum
114900020108     c                   movel     §trde3        ds3um
115000020108      * se a quintale o a kg metto in  desum KG. e in desctr A PESO
115100020108     c                   if        unoctr = 0 or unoctr = 3
115200020108     c                   eval      desum = '       KG.'
115300020108     c                   endif
115400020109      * salvo il minimo tassabile
115500020109     c                   eval      ctrata  = §trata
115600020109
115700020108     c                   endif
115800020109
115900020108      * tariffa a valore (per stampare scaglioni senza decimali)
116000020108     c                   eval      *in87 = (unoctr = 4)
116100020109
116200020108      * valorizzo il campo note
116300020108     c                   eval      p07des = tamdcv
116400020109
116500020108      * se il campo note è uguale a blank verifico se tariffa preferenziale
116600020108      * partenza o arrivo
116700020108     c                   if        p07des = *blanks and tambap =  'P'
116800060704      * in lingua
116900060704     c                   select
117000060704     c                   when      i45lin = 'I'
117100020108     c                   eval      p07des = 'Valida solo per traffico in +
117200020108     c                                       Partenza'
117300060704     c                   when      i45lin = '2'
117400060704     c                   eval      p07des = 'Valid only for Departing tra+
117500060704     c                                       ffic'
117600060726     c                   when      i45lin = '3'
117700060726     c                   eval      p07des = 'Valable uniquement pour le +
117800060726     c                                       trafic au départ'
117900060712     c                   when      i45lin = '4'
118000060726     c                   eval      p07des = 'Gilt nur für Abgehende +
118100060726     c                                       Sendungen'
118200060704     c                   endsl
118300020108     c                   endif
118400020108
118500020108     c                   if        p07des = *blanks and tambap =  'A'
118600060704      * in lingua
118700060704     c                   select
118800060704     c                   when      i45lin = 'I'
118900020108     c                   eval      p07des = 'Valida solo per traffico in +
119000020108     c                                       Arrivo'
119100060704     c                   when      i45lin = '2'
119200060704     c                   eval      p07des = 'Valid only for Arriving traf+
119300060704     c                                       fic'
119400060726     c                   when      i45lin = '3'
119500060726     c                   eval      p07des = 'Valable uniquement pour le +
119600060726     c                                       trafic à l''arrivée'
119700060712     c                   when      i45lin = '4'
119800060726     c                   eval      p07des = 'Gilt nur für Eingehende +
119900060726     c                                       Sendungen'
120000060704     c                   endsl
120100020108     c                   endif
120200040430      * TESTO PARTICOLARITÀ GIACENZA
120300040430      * se codice = GP metto 5 nel numero di avvisi prima di rendere la merce
120400040430      * altrimenti lascio 2
120500040430     c                   eval      comnav = 2
120600040430     c                   if        tamgga = 'GP'
120700040430     c                   eval      comnav = 5
120800040430     c                   endif
120900060925      * devo impostare anche la parte finale del numero nelle varie lingue
121000060925     c                   exsr      sr_numgp
121100020108
121200020108      * recupero in base alla data decorrenza la versione di prestampato condi-
121300020108      * zioni generali dalla tabella TAA
121400161222     c                   eval      kcod = 'TAA'
121500020108     c     kcod          setll     tntbe01l
121600020108     c                   do        *hival
121700020108     c     kcod          reade     tntbe01l
121800020108      * se fine file vado a fine
121900020108     c                   if        %eof(tntbe01l)
122000020108     c                   leave
122100020108     c                   endif
122200020108      * se annullato leggo il successivo
122300020108     c                   if        tbeatb <> ' '
122400020108     c                   iter
122500020108     c                   endif
122600020108      * mi salvo il modello e l'anno
122700020108     c                   movel     tbeuni        dtaa
122800020108      * mi salvo sempre il primo che ho letto
122900020108     c                   if        stpmod = 0
123000020108     c                   movel     tbeke1        stpmod
123100020108     c                   movel     §dccdad       stpanno
123200020108     c                   endif
123300060313      *?ATTENZIONE  ?
123400060313      * dal 01/03/06 i controlli vanno fatti con la data di stampa e non +
123500060313      * con la data decorrenza tariffa
123600060313      * verifico la data di stampa
123700060313      * se data di stampa maggiore della data di validità del modello leggo
123800020108      * verifico la data di decorrenza
123900020108      * se data decorrenza maggiore della data di validità del modello leggo
124000060313     c                   if        i45ads >  §dccdad
124100020108     c                   movel     tbeke1        stpmod
124200020108     c                   movel     §dccdad       stpanno
124300020108     c                   iter
124400020108     c                   endif
124500020108      * se data decorrenza = salvo a vado a fine
124600060313     c                   if        i45ads =  §dccdad
124700020108     c                   movel     tbeke1        stpmod
124800020108     c                   movel     §dccdad       stpanno
124900020108     c                   leave
125000020108     c                   endif
125100020108      * se data decorrenza <  della data validita' vado a fine
125200060313     c                   if        i45ads <  §dccdad
125300020108     c                   leave
125400020108     c                   endif
125500020108
125600020108     c                   enddo
125700161222
125800161222      * recupero in base alla data di stampa la versione di allegato informa-
125900161222      * tiva Privacy dalla tabella ATP
126000161222     c                   eval      kcod = 'ATP'
126100161222     c     kcod          setll     tntbe01l
126200161222     c                   do        *hival
126300161222     c     kcod          reade     tntbe01l
126400161222      * se fine file vado a fine
126500161222     c                   if        %eof(tntbe01l)
126600161222     c                   leave
126700161222     c                   endif
126800161222      * se annullato leggo il successivo
126900161222     c                   if        tbeatb <> ' '
127000161222     c                   iter
127100161222     c                   endif
127200161222      * mi salvo il modello e l'anno
127300161222     c                   movel     tbeuni        datp
127400161222      * mi salvo sempre il primo che ho letto
127500161222     c                   if        stpmodi= 0
127600161222     c                   movel     tbeke1        stpmodi
127700161222     c                   movel     §atpdad       stpannoi
127800161222     c                   endif
127900161222      * se data di stampa maggiore della data di validità del modello leggo
128000161222     c                   if        i45ads >  §atpdad
128100161222     c                   movel     tbeke1        stpmodi
128200161222     c                   movel     §atpdad       stpannoi
128300161222     c                   iter
128400161222     c                   endif
128500161222      * se data decorrenza = salvo a vado a fine
128600161222     c                   if        i45ads =  §atpdad
128700161222     c                   movel     tbeke1        stpmodi
128800161222     c                   movel     §atpdad       stpannoi
128900161222     c                   leave
129000161222     c                   endif
129100161222      * se data stampa  <  della data validita' vado a fine
129200161222     c                   if        i45ads <  §atpdad
129300161222     c                   leave
129400161222     c                   endif
129500161222
129600161222     c                   enddo
129700020110
129800020110      * se non c'è la tariffa finita sul file la prendo dalla tabella TR
129900020110     c                   if        tamata = 0
130000020110     c                   z-add     §trata        wrkata
130100020110     c                   else
130200020110     c                   z-add     tamata        wrkata
130300020110     c                   endif
130400020108
130500020108     c                   endsr
130600020108
130700020108      *------------------------------------------------------------------*
130800020109      *   Sr_Scaglioni    CONTROLLO SCAGLIONI E ZONE TARIFFARIE
130900020108      *------------------------------------------------------------------*
131000020108
131100020108     c     Sr_Scaglioni  begsr
131200020108
131300020108     c                   setoff                                       99
131400050131     c                   eval      woklnp = *Off
131500020108
131600020108      * cerco prima con la linea di partenza del cliente
131700061011     c  n98ds1ktd        setll     titad04l
131800061011     c   98ds1ktd        setll     tiofd01l
131900020108
132000020108      * se non trovato con la linea di partenza del cliente
132100061011     c                   if        not %equal
132200020108      * mi riposiziono con chiave generica
132300061011     c  n98ds1ktm        setll     titad04l
132400061011     c   98ds1ktm        setll     tiofd01l
132500020108
132600020108      * se non trovato nulla errore manca dettaglio tariffario
132700061011     c                   if        not %equal
132800020108     c                   seton                                        99
132900020108     c                   move      msg(4)        errmsg
133000020108     c                   exsr      Staerr
133100020108     c                   goto      endscaglia
133200020108     c                   endif
133300020108
133400020108     c                   endif
133500020109
133600020109      * Leggo il dettaglio tariffe
133700020108     c                   do        *hival
133800020108
133900061011     c  n98ds1ktm        reade     titad04l
134000061011     c   98ds1ktm        reade     tiofd01l
134100020109      * fine file esco dal ciclo
134200061011     c                   if        %eof
134300020108     c                   leave
134400020108     c                   endif
134500020108      * se annullato rileggo
134600020108     c                   if        tadatb = 'A'
134700020108     c                   iter
134800020108     c                   endif
134900050131
135000050131     c                   Eval      wokstp = *Off
135100050131
135200050131      * se I45DET = 'L' devo stampare solo le righe di dettaglio che hanno la
135300050131      *                 LNP uguale alla LNP del codice cliente
135400050131     c                   If        I45DET = 'L'
135500050131      * se non è la prima lnp che leggo non stampa più niente
135600050131     c                   if        woklnp = *On and rotlnp <> 0 and
135700050131     c                             rotlnp <> tadlnp
135800050131     c                   iter
135900050131     c                   EndIf
136000050131     c                   ExSr      sr_Chklnp
136100050131     c                   if        wokstp = *Off
136200050131     c                   iter
136300050131     c                   EndIf
136400050131     c                   Eval      woklnp = *On
136500050131     c                   EndIf
136600020109
136700020109      * rottura per lnp
136800020109     c                   if        rotlnp <> tadlnp
136900020109
137000020109      * se non è la prima lnp che leggo esco dal ciclo
137100020109     c                   if        rotlnp <> 0
137200020109     c                   leave
137300020109     c                   endif
137400020109
137500020111     c                   z-add     0             k
137600020109     c                   z-add     0             x
137700020109     c                   eval      rotlnp = tadlnp
137800020109     c                   endif
137900020108
138000020109      * rottura per cts
138100020109     c                   if        rotcts <> tadcts
138200020109     c                   add       1             x
138300020109
138400020116      * errore per più di 7 zone tariffarie per lnp
138500020116     c                   if        x > 7
138600050914      * accendo l'indicatore per uscire dal pgm
138700050914     c                   seton                                        99
138800020109     c                   move      msg(16)       errmsg
138900020109     c                   exsr      Staerr
139000050930     c                   goto      endscaglia
139100020109     c                   endif
139200020109
139300020109     c                   eval      rotcts = tadcts
139400020111
139500020111     c                   endif
139600020109
139700020109      * se non è il primo giro di lettura x cts torno a leggere
139800020109      * tanto non devo più caricare gli scaglioni
139900020109     c                   if        x > 1
140000020109     c                   iter
140100020109     c                   endif
140200020108
140300020109      * carico gli scaglioni per lnp e cts uguali
140400020109
140500020109 b1  c                   if        tadlnp = rotlnp and tadcts = rotcts
140600020109
140700020108     c                   add       1             k
140800020109
140900020109      * errore per più di 200 scaglioni per tariffa
141000020108     c                   if        k > 200
141100050914      * accendo l'indicatore per uscire dal pgm
141200050914     c                   seton                                        99
141300020108     c                   move      msg(5)        errmsg
141400020108     c                   exsr      Staerr
141500050930     c                   goto      endscaglia
141600020108     c                   endif
141700020108
141800020109     c                   z-add     tadsgl        sgl(k)
141900020109
142000020109      * e verifico se gli scaglioni hanno dei decimali solo se non è tariffa
142100020109      * a valore
142200020109 b2  c                   if        not *in87
142300020109
142400020109     c                   move      tadsgl        w0030
142500020109     c                   z-add     tadsgl        w0050
142600020109      * se scaglione 99999 non controllo i decimali
142700020109     c                   if        w0050 <> 99999 and w0030 > 0
142800020109     c                   seton                                        91
142900020109     c                   endif
143000020109      * se con decimali verifico se valorizzato dal secondo in poi
143100020109     c                   if        *in91
143200020109     c                   z-add     w0030         w0020
143300020109     c                   if        w0020 > 0
143400020109     c                   seton                                        37
143500020109     c                   endif
143600020109     c                   endif
143700020109
143800020109 e2  c                   endif
143900020108
144000020109 e1  c                   endif
144100020108
144200020108     c                   enddo
144300020108
144400020108      * se non ho caricato nessuno scaglione do errore di mancanza di dettaglio
144500020109     c                   if        k = 0
144600020108     c                   seton                                        99
144700020108     c                   move      msg(4)        errmsg
144800020108     c                   exsr      Staerr
144900020109     c                   goto      endscaglia
145000020108     c                   endif
145100020108
145200020109     c     endscaglia    endsr
145300020108
145400020108      *-------------------------------------------------------------*
145500020108      *  Sr_Eladet   ELABORO E STAMPO IL DETTAGLIO TARIFFARIO
145600020108      *-------------------------------------------------------------*
145700020108
145800020108     c     Sr_Eladet     begsr
145900020109
146000020109     c                   clear                   rotlnp
146100020109     c                   clear                   rotcts
146200110127     c                   clear                   wrpv
146300050201     c                   eval      woklnp = *Off
146400110127     c                   eval      wvolta = *off
146500020109
146600020109      * Leggo il dettaglio tariffario dall'inizio
146700020109
146800061011     c  n98ds1ktm        setll     titad04l
146900061011     c   98ds1ktm        setll     tiofd01l
147000020109     c                   do        *hival
147100020109
147200061011     c  n98ds1ktm        reade     titad04l
147300061011     c   98ds1ktm        reade     tiofd01l
147400020109
147500020109      * fine file esco dal ciclo
147600061011     c                   if        %eof
147700020109     c                   leave
147800020109     c                   endif
147900020109
148000020109      * se annullato rileggo
148100020109     c                   if        tadatb = 'A'
148200020109     c                   iter
148300020109     c                   endif
148400050131
148500050201     c                   Eval      wokstp = *Off
148600050201
148700050201      * se I45DET = 'L' devo stampare solo le righe di dettaglio che hanno la
148800050201      *                 LNP uguale alla LNP del codice cliente
148900050201     c                   If        I45DET = 'L'
149000050201      * se non è la prima lnp che leggo non stampa più niente
149100050201     c                   if        woklnp = *On and rotlnp <> 0 and
149200050201     c                             rotlnp <> tadlnp
149300050201     c                   iter
149400050201     c                   EndIf
149500050201     c                   ExSr      sr_Chklnp
149600050201     c                   if        wokstp = *Off
149700050201     c                   iter
149800050201     c                   EndIf
149900050201     c                   Eval      woklnp = *On
150000050201     c                   EndIf
150100020109
150200020109      * verifico se è uno scaglione previsto
150300020109     c                   z-add     tadsgl        wsgl72
150400020109     c     wsgl72        lookup    sgl                                    35
150500020109     c                   if        not *in35
150600020109      * errore scaglione non stampabile
150700050914      * accendo l'indicatore ed esco dal ciclo
150800050914     c                   seton                                        99
150900020109     c                   movel     msg(8)        errmsg
151000020109     c                   exsr      Staerr
151100050930     c                   goto      fine
151200020109     c                   endif
151300020109
151400020109      * Stampo testata lnp
151500020109
151600020109 b1  c                   if        tadlnp  <> rotlnp
151700020109
151800020109      * Stampo il dettaglio tariffario caricato per lnp precedente
151900020109     c                   if        rotlnp > 0
152000020109     c                   exsr      Sr_Stptesdet
152100020109     c                   endif
152200020109
152300020109     c                   eval      rotlnp = tadlnp
152400020109     c                   z-add     0             x
152500020109
152600020109      * decodifico la nuova linea di partenza
152700020109     c     tadlnp        chain     azorg01l
152800020109     c                   if        %found(azorg01l)
152900020109     c                   movel     orgdes        stalnp
153000020624      * controllo se linea di partenza 9xx
153100020624     c                   if        orgfl2 = 'S'
153200020624      * italia
153300020624     c                   if        tadlnp = 950
153400060704      * in lingua
153500060704     c                   select
153600060704     c                   when      i45lin = 'I'
153700070312     c                   eval      prtlnp =
153800070313     c                             'Merce ritirata da tutte le ns. filiali'
153900020624     c                             + '  ' + stalnp
154000060704     c                   when      i45lin = '2'
154100060704     c                   eval      prtlnp = 'Goods collected from all our +
154200070314     c                              branches' + '  ' + stalnp
154300060726     c                   when      i45lin = '3'
154400070314     c                   eval      prtlnp = 'Merchandise retirée par tuotes +
154500070314     c                              nos filiales' + '  ' + stalnp
154600060712     c                   when      i45lin = '4'
154700070314     c                   eval      prtlnp = 'Abholung in allen unseren Filialen'
154800060712     c                             + '  ' + stalnp
154900060704     c                   endsl
155000020624     c                   else
155100020624      * regione
155200060704      * in lingua
155300060704     c                   select
155400060704     c                   when      i45lin = 'I'
155500070312     c                   eval      prtlnp = 'Merce ritirata dalle ns. filiali +
155600070312     c                              della regione' + '  ' + stalnp
155700060704     c                   when      i45lin = '2'
155800070314     c                   eval      prtlnp = 'Goods collected from our branches +
155900060704     c                              in the region' + '  ' + stalnp
156000060726     c                   when      i45lin = '3'
156100060726     c                   eval      prtlnp = 'Merchandise retirée par nos +
156200070314     c                              filiales de la région' + '  ' + stalnp
156300060712     c                   when      i45lin = '4'
156400070314     c                   eval      prtlnp = 'Abholung in unseren Filialen der +
156500060726     c                              Region' + '  '  + stalnp
156600060704     c                   endsl
156700020624     c                   endif
156800020624     c                   else
156900020624      * lnp non è 9xx
157000060704      * in lingua
157100060704     c                   select
157200060704     c                   when      i45lin = 'I'
157300070312     c                   eval      prtlnp =
157400070313     c                             'Merce ritirata dalla ns. filiale di'
157500020624     c                             + '  ' + stalnp
157600060704     c                   when      i45lin = '2'
157700070314     c                   eval      prtlnp = 'Goods collected from our branch +
157800060704     c                              in' + '  ' + stalnp
157900060726     c                   when      i45lin = '3'
158000060726     c                   eval      prtlnp = 'Merchandise retirée par notre +
158100070314     c                              filiale de' + '  ' + stalnp
158200060726     c                   when      i45lin = '4'
158300070314     c                   eval      prtlnp = 'Abholung in unseren Filialen in'
158400060712     c                             + '  ' + stalnp
158500060704     c                   endsl
158600020624     c                   endif
158700020624
158800020109     c                   endif
158900020109
159000020109      * overflow stampo fine pagina
159100020109     c                   if        *in80 or curline > 56
159200020109     c                   write     ta50pfin
159300020109     c                   write     ta50ppp
159400020109     c                   setoff                                       80
159500020109     c                   endif
159600020109
159700020109      * stampo la nuova linea di partenza
159800020109     c                   write     ta50p02a
159900020109 e1  c                   endif
160000020109
160100020109      * carico il dettaglio tariffario
160200020109     c                   exsr      Sr_Cardet
160300110127     c                   if        wvolta=*off
160400110127     c                   eval      wrpv=tadrpv
160500110127     c                   eval      wvolta=*on
160600110127     c                   endif
160700020109
160800020109     c                   enddo
160900020110
161000020110      * Stampo l'ultima linea di partenza letta
161100020110     c                   exsr      Sr_Stptesdet
161200020109
161300020211      * overflow stampo fine pagina
161400020211     c                   if        *in80 or curline > 56
161500020211     c                   write     ta50pfin
161600020211     c                   write     ta50ppp
161700020211     c                   setoff                                       80
161800020211     c                   endif
161900020211      * Rapporto peso volume
162000110127     c                   if        wrpv>0
162100110127     c                   eval      p06rpv=wrpv*100
162200020211     c                   write     ta50p06
162300110127     c                   endif
162400020109     c                   endsr
162500020109
162600020109      *-------------------------------------------------------------*
162700020109      *  Sr_Cardet   CARICO IL DETTAGLIO TARIFFARIO X LNP
162800020109      *-------------------------------------------------------------*
162900020110
163000020109     c     Sr_Cardet     begsr
163100020109
163200020109      * rottura per zona tariffaria
163300020109     c                   if        rotcts <> tadcts
163400020109     c                   z-add     0             k
163500020109     c                   add       1             x
163600020116     c                   if        x <= 7
163700020109     c                   move      tadcts        cts(x)
163800020110     c                   clear                   itr
163900020110     c                   endif
164000020110     c                   eval      rotcts = tadcts
164100020109     c                   endif
164200020110
164300020116      * se ci sono più di 7 zone tariffarie non devo stampare
164400020116     c                   if        x > 7
164500050914      * accendo l'indicatore per uscire dal pgm
164600050914     c                   seton                                        99
164700020110     c                   move      msg(16)       errmsg
164800020110     c                   exsr      Staerr
164900020110     c                   goto      endcar
165000020110     c                   endif
165100020109
165200020109     c                   add       1             k
165300020109     c     k             occur(e)  sglitr
165400020109     c                   if        not %error
165500020109     c                   z-add     taditr        itr(x)
165600020110     c                   select
165700020110     c                   when      itr(1) > 0
165800020110     c                   z-add     itr(1)        itr1
165900020110     c                   when      itr(2) > 0
166000020110     c                   z-add     itr(2)        itr2
166100020110     c                   when      itr(3) > 0
166200020110     c                   z-add     itr(3)        itr3
166300020110     c                   when      itr(4) > 0
166400020110     c                   z-add     itr(4)        itr4
166500020110     c                   when      itr(5) > 0
166600020110     c                   z-add     itr(5)        itr5
166700020110     c                   when      itr(6) > 0
166800020110     c                   z-add     itr(6)        itr6
166900020116     c                   when      itr(7) > 0
167000020116     c                   z-add     itr(7)        itr7
167100020110     c                   endsl
167200051006      * memorizzo lo scaglione se è il primo giro
167300051006     c                   If        x = 1
167400020109     c                   z-add     tadsgl        sglp
167500051006      * se non è il primo giro e sto cercando di caricare i dati su uno
167600051006      * scaglione diverso emetto errore ed esco dal pgm
167700051006     c                   Else
167800051006     c                   z-add     tadsgl        wsgl
167900051006     c                   If        wsgl <> sglp
168000051006     c                   move      msg(8)        errmsg
168100051006     c                   exsr      Staerr
168200051006     c                   goto      fine
168300051006     c                   EndIf
168400051006     c                   EndIf
168500020109     c                   endif
168600020109
168700020110     c     endcar        endsr
168800020109
168900020109      *-------------------------------------------------------------*
169000020109      *  Sr_Stptesdet STAMPO LA TESTATA E IL DETTAGLIO TARIFFARIO
169100020109      *-------------------------------------------------------------*
169200020110
169300020109     c     Sr_Stptesdet  begsr
169400020109
169500020110     c                   setoff                                       50
169600020109
169700020110     c                   do        200           k
169800020109     c     k             occur(e)  sglitr
169900020109     c                   if        not %error
170000020110     c                   if        sglp = 0
170100020110     c                   leave
170200020110     c                   endif
170300020109
170400020109      * se prima riga che leggo devo stampare prima la testata
170500020109     c                   if        k = 1
170600020109     c                   exsr      Sr_Stptes
170700020109     c                   endif
170800020109
170900020109      * stampo il dettaglio
171000020109     c                   exsr      Sr_Stpdet
171100020110
171200020110      * salvo lo scaglione che ho appena letto
171300020110     c                   eval      sav_sglp = sglp
171400020109
171500020109     c                   endif
171600020109
171700020109     c                   enddo
171800020109      * pulisco le schiere
171900020109     c                   clear                   cts
172000020109     c                   clear                   dcts
172100020110     c                   do        200           k
172200020110     c     k             occur     sglitr
172300020110     c                   if        not %error
172400020110     c                   clear                   sglp
172500020110     c                   clear                   itr1
172600020110     c                   clear                   itr2
172700020110     c                   clear                   itr3
172800020110     c                   clear                   itr4
172900020110     c                   clear                   itr5
173000020110     c                   clear                   itr6
173100020116     c                   clear                   itr7
173200020110     c                   endif
173300020110     c                   enddo
173400020109
173500020110     c                   endsr
173600020109
173700020109      *-------------------------------------------------------------*
173800020109      *  Sr_Stptes   STAMPO LA TESTATA DELLE ZONE TARIFFARIE
173900020109      *-------------------------------------------------------------*
174000020110
174100020109     c     Sr_Stptes     begsr
174200020109
174300020116     c                   do        7             x
174400020109     c                   eval      cod = 'CT'
174500020109     c                   if        cts(x) <> *blanks
174600020109     c                   movel(p)  cts(x)        key
174700060724     c     tabkeyl       chain     tabel00f
174800020109     c                   if        %found(tabel00f)
174900020109     c                   movel     tbluni        dcts(x)
175000020109     c                   endif
175100020109     c                   endif
175200020109     c                   enddo
175300020110
175400020110     c                   movel     dcts(1)       dzona1
175500020110     c                   movel     dcts(2)       dzona2
175600020110     c                   movel     dcts(3)       dzona3
175700020110     c                   movel     dcts(4)       dzona4
175800020110     c                   movel     dcts(5)       dzona5
175900020110     c                   movel     dcts(6)       dzona6
176000020116     c                   movel     dcts(7)       dzona7
176100020109
176200020109     c                   write     ta50p02
176300020109
176400020109     c                   endsr
176500020109
176600020109      *-------------------------------------------------------------*
176700020109      *  Sr_Stpdet   STAMPO DETTAGLIO - SCAGLIONI
176800020109      *-------------------------------------------------------------*
176900020110
177000020109     c     Sr_Stpdet     begsr
177100020109
177200020109      * se lo scaglione non supera la tariffa finita
177300020110 b1  c                   if        sglp <= wrkata
177400020109
177500020109      * stampo il singolo scaglione
177600020109
177700020109      * senza decimali
177800020109 b2  c                   if        not *in91
177900020109     c                   z-add     sglp          wsgl70
178000020110     c                   eval      p03sgld = %editc(wsgl70:'4')
178100020109 x2  c                   else
178200020109      * con 1 decmale
178300020109 b3  c                   if        not *in37
178400020109     c                   z-add     sglp          wsgl71
178500020110     c                   eval      p03sgld = %editc(wsgl71:'4')
178600020109 x3  c                   else
178700020109      * con 2 decimali
178800020109     c                   z-add     sglp          wsgl72
178900020110     c                   eval      p03sgld = %editc(wsgl72:'4')
179000020109 e3  c                   endif
179100020109 e2  c                   endif
179200020109
179300020109 x1  c                   else
179400020109      * stampo scaglione dal al
179500020109
179600020110     c                   if        not *in50
179700020110     c                   seton                                        50
179800020110     c                   z-add     wrkata        p03ata
179900020109     c                   write     ta50p03a
180000020109     c                   endif
180100020109
180200020109      * senza decimali
180300020109 b2  c                   if        not *in91
180400020110     c                   z-add     sav_sglp      wsgl70d
180500020110     c                   add       1             wsgl70d
180600020110     c                   z-add     sglp          wsgl70a
180700020110     c                   eval      p03sgld = %editc(wsgl70d:'4')
180800020110     c                   eval      p03sgla = %editc(wsgl70a:'4')
180900020109 x2  c                   else
181000020109      * con 1 decmale
181100020109 b3  c                   if        not *in37
181200020110     c                   z-add     sav_sglp      wsgl71d
181300020110     c                   add       0,1           wsgl71d
181400020110     c                   z-add     sglp          wsgl71a
181500020110     c                   eval      p03sgld = %editc(wsgl71d:'4')
181600020110     c                   eval      p03sgla = %editc(wsgl71a:'4')
181700020109 x3  c                   else
181800020109      * con 2 decimali
181900020110     c                   z-add     sav_sglp      wsgl72d
182000020110     c                   add       0,01          wsgl72d
182100020110     c                   z-add     sglp          wsgl72a
182200020110     c                   eval      p03sgld = %editc(wsgl72d:'4')
182300020110     c                   eval      p03sgla = %editc(wsgl72a:'4')
182400020109 e3  c                   endif
182500020109 e2  c                   endif
182600020109 e1  c                   endif
182700051006
182800051006      * Controllo scaglioni
182900051006     c                   ExSr      Sr_Ctrsca
183000020109
183100020109      * stampo tariffa
183200020110     c                   z-add     itr1          tar01
183300020110     c                   z-add     itr2          tar02
183400020110     c                   z-add     itr3          tar03
183500020110     c                   z-add     itr4          tar04
183600020110     c                   z-add     itr5          tar05
183700020110     c                   z-add     itr6          tar06
183800020116     c                   z-add     itr7          tar07
183900020109
184000020109      * se of ristampo la testata tariffa
184100020109
184200020109     c                   if        *in80 or curline > 61
184300020109     c                   write     ta50pfin
184400020109     c                   write     ta50pPP
184500020109     c                   write     ta50p02a
184600020109     c                   write     ta50p02
184700020109     c                   setoff                                       80
184800020109     c                   endif
184900020109
185000020109     c                   write     ta50p03
185100020109
185200020109     c                   endsr
185300051006
185400051006      *------------------------------------------------------------------*
185500051006      *  Sr_Ctrsca        CONTROLLO GLI SCAGLIONI
185600051006      *------------------------------------------------------------------*
185700051006     c     Sr_Ctrsca     BegSr
185800051006
185900051006     c                   Eval      xx = 1
186000051006     c     *Blanks       Lookup    cts(xx)                                35
186100051006     c                   If        Not *In35
186200051006     c                   Eval      yy = 7
186300051006     c                   Else
186400051006     c                   Eval      yy = xx -1
186500051006     c                   EndIf
186600051006
186700051006     c                   Eval      itr(1) = itr1
186800051006     c                   Eval      itr(2) = itr2
186900051006     c                   Eval      itr(3) = itr3
187000051006     c                   Eval      itr(4) = itr4
187100051006     c                   Eval      itr(5) = itr5
187200051006     c                   Eval      itr(6) = itr6
187300051006     c                   Eval      itr(7) = itr7
187400051006
187500051006     c                   Do        yy            xx
187600051006     c                   If        itr(xx) = *Zeros
187700051006     c                   Eval      errmsg = msg(8)
187800051006     c                   ExSr      Staerr
187900051006     c                   Goto      fine
188000051006     c                   EndIf
188100051006     c                   EndDo
188200051006
188300051006     c                   EndSr
188400020109
188500020110      *------------------------------------------------------------------*
188600020110      *  Sr_Arroto        GESTIONE STAMPE ARROTONDAMENTI
188700020110      *------------------------------------------------------------------*
188800020110
188900020110     c     Sr_Arroto     begsr
189000020110
189100020110     c                   clear                   ta50p05
189200020110      * verifico gli importi degli arrotondamenti
189300020110
189400020110    1c                   if        tamarl > 0
189500020110      * verifico se esistono dei valori con i decimali
189600020110     c                   move      tamarf        w0030
189700020110     c                   eval      *in89 =(w0030 > 0)
189800020111
189900020110    2c                   if        not *in89
190000020110     c                   move      tamarl        w0030
190100020110     c                   eval      *in89 =(w0030 > 0)
190200020110    2c                   endif
190300020111
190400020110    2c                   if        not *in89
190500020110     c                   move      tamaro        w0030
190600020110     c                   eval      *in89 =(w0030 > 0)
190700020110    2c                   endif
190800020110
190900020111      * se 89 spento non esistono importi con decimali sposto in campo di comodo
191000020110    2c                   if        not *in89
191100020110     c                   z-add     tamaro        waro2
191200020110     c                   z-add     tamarf        warf2
191300020110     c                   z-add     tamarl        warl2
191400020110    2c                   endif
191500020110
191600020110      * se arrotondamenti di 1 in 1 non stampo
191700020110      * se arrotondamento fino a diverso da arrotondamento oltre a
191800020110
191900020110    2c                   if        tamarf <> tamaro
192000020110
192100020110      * valorizzo i campi di stampa
192200060704      * in lingua
192300060704     c                   select
192400060704     c                   when      i45lin = 'I'
192500020110     c  n89              eval      p05ar1 = 'fino a ' + %editc(Warl2:'4') +
192600020110     c                                      ' di ' + %editc(warf2:'4')     +
192700020110     c                                      ' in ' + %editc(warf2:'4')
192800020110     c   89              eval      p05ar1 = 'fino a ' + %editc(tamarl:'4') +
192900020110     c                                      ' di ' + %editc(tamarf:'4')     +
193000020110     c                                      ' in ' + %editc(tamarf:'4')
193100060704     c                   when      i45lin = '2'
193200060704     c  n89              eval      p05ar1 = 'Up to ' +
193300060704     c                                      %editc(warl2:'4') + ' rounded ' +
193400060704     c                                      'to the nearest ' +
193500060704     c                                      %editc(warf2:'4')
193600060704     c   89              eval      p05ar1 = 'Up to ' +
193700060704     c                                      %editc(tamarl:'4') + ' rounded ' +
193800060704     c                                      'to the nearest ' +
193900060704     c                                      %editc(tamarf:'4')
194000060801     c                   when      i45lin = '3'
194100060801     c  n89              eval      p05ar1 = 'jusq''à ' + %editc(Warl2:'4') +
194200060801     c                                      ' de ' + %editc(warf2:'4')     +
194300060801     c                                      ' en ' + %editc(warf2:'4')
194400060818     c   89              eval      p05ar1 = 'jusq''à ' + %editc(tamarl:'4') +
194500060801     c                                      ' de ' + %editc(tamarf:'4')     +
194600060801     c                                      ' en ' + %editc(tamarf:'4')
194700060712     c                   when      i45lin = '4'
194800060818     c  n89              eval      p05ar1 = 'bis  ' + %editc(warl2:'4')  +
194900060710     c                                      ' von ' + %editc(warf2:'4') +
195000060710     c                                      ' auf ' + %editc(warf2:'4')
195100060818     c   89              eval      p05ar1 = 'bis  ' + %editc(tamarl:'4') +
195200060710     c                                      ' von ' + %editc(tamarf:'4')     +
195300060710     c                                      ' auf ' + %editc(tamarf:'4')
195400060704     c                   endsl
195500020110
195600020110      * se arrotondamento oltre a è maggiore di 1
195700020110
195800020110      * valorizzo i campi di stampa
195900060704      * in lingua
196000060704     c                   select
196100060704     c                   when      i45lin = 'I'
196200020110     c  n89              eval      p05ar2 = 'oltre  ' + '    '        +
196300020110     c                                      ' di ' + %editc(waro2:'4')     +
196400020110     c                                      ' in ' + %editc(waro2:'4')
196500020110     c   89              eval      p05ar2 = 'oltre  ' + '        '         +
196600020110     c                                      ' di ' + %editc(tamaro:'4')     +
196700020110     c                                      ' in ' + %editc(tamaro:'4')
196800060704     c                   when      i45lin = '2'
196900060704     c  n89              eval      p05ar2 = 'over ' + '         '  +
197000060704     c                                      ' to the nearest ' +
197100060704     c                                      %editc(waro2:'4')
197200060704     c   89              eval      p05ar2 = 'over ' + '         '  +
197300060704     c                                      ' to the nearest ' +
197400060704     c                                      %editc(tamaro:'4')
197500060801     c                   when      i45lin = '3'
197600060801     c  n89              eval      p05ar2 = 'au delà' + '    '        +
197700060801     c                                      ' de ' + %editc(waro2:'4')     +
197800060801     c                                      ' en ' + %editc(waro2:'4')
197900060801     c   89              eval      p05ar2 = 'au delà' + '        '         +
198000060801     c                                      ' de ' + %editc(tamaro:'4')     +
198100060801     c                                      ' en ' + %editc(tamaro:'4')
198200060712     c                   when      i45lin = '4'
198300060818     c  n89              eval      p05ar2 = 'mehr als ' + '    '        +
198400060710     c                                      ' von ' + %editc(waro2:'4')     +
198500060710     c                                      ' auf ' + %editc(waro2:'4')
198600060818     c   89              eval      p05ar2 = 'mehr als ' + '    '         +
198700060710     c                                      ' von ' + %editc(tamaro:'4')     +
198800060710     c                                      ' auf ' + %editc(tamaro:'4')
198900060704     c                   endsl
199000020110
199100020110     c                   exsr      Sr_Over
199200020110
199300020110     c                   write     ta50p05
199400020110
199500020110     c                   else
199600020110      * valorizzo i campi di stampa
199700060704      * in lingua
199800060704     c                   select
199900060704     c                   when      i45lin = 'I'
200000020110     c  n89              eval      p05ar1 = ' di ' + %editc(Warf2:'4') +
200100020110     c                                      ' in ' + %editc(waro2:'4')
200200020110     c   89              eval      p05ar1 = ' di ' + %editc(tamarf:'4') +
200300020110     c                                      ' in ' + %editc(tamaro:'4')
200400060704     c                   when      i45lin = '2'
200500060704     c  n89              eval      p05ar1 = 'Rounded to the nearest ' +
200600060704     c                                      %editc(warf2:'4')
200700060704     c   89              eval      p05ar1 = 'Rounded to the nearest ' +
200800060704     c                                      %editc(tamarf:'4')
200900060801     c                   when      i45lin = '3'
201000060801     c  n89              eval      p05ar1 = ' de ' + %editc(Warf2:'4') +
201100060801     c                                      ' en ' + %editc(waro2:'4')
201200060801     c   89              eval      p05ar1 = ' de ' + %editc(tamarf:'4') +
201300060801     c                                      ' en ' + %editc(tamaro:'4')
201400060712     c                   when      i45lin = '4'
201500060710     c  n89              eval      p05ar1 = ' von ' + %editc(Warf2:'4') +
201600060710     c                                      ' auf ' + %editc(waro2:'4')
201700060710     c   89              eval      p05ar1 = ' von ' + %editc(tamarf:'4') +
201800060710     c                                      ' auf ' + %editc(tamaro:'4')
201900060704     c                   endsl
202000020110
202100020110     c                   exsr      Sr_Over
202200020110
202300020110     c                   write     ta50p05
202400020110
202500020110    2c                   endif
202600020110    1c                   endif
202700020110
202800020110     c                   endsr
202900020110
203000020110      *------------------------------------------------------------------*
203100020110      *  Sr_Statam        STAMPA I DATI CHE SI TROVANO IN TESTATA TARIFFA
203200020110      *------------------------------------------------------------------*
203300020110
203400020110     c     Sr_Statam     begsr
203500020110
203600020110      * se offerta valorizzo con il numero della visita
203700020110    1c                   if        *in98
203800020110     c                   eval      p07ksc = visksc
203900020110     c                   movel     tamksc        p07nrv
204000020110     c                   move      tamksc        p07prv
204100091117     c                   if        $trattativa
204200091117     c                   eval      p07ntra = tamksc
204300091117     c                   endif
204400020110
204500020110     c                   else
204600020110      * tariffa
204700020110     c                   eval      p07ksc = tamksc
204800020110    1c                   endif
204900020110
205000091117     c                   if        $trattativa
205100091117     c                   eval      ptratta =
205200091117     c                             '(codice ' +
205300091117     c                             %editc(tamctr:'X') + '/' +
205400091117     c                             %subst(%editc(tamprg:'X'):3:1) + ')'
205500091117     c                   endif
205600091117     c                   if        not $trattativa
205700091117     c                   eval      ptariffa =
205800091117     c                             '(codice ' +
205900091117     c                             %editc(tamctr:'X') + ')'
206000091117     c                   endif
206100020110
206200020110     c                   exsr      Sr_Over
206300020110
206400020110     c                   write     ta50p07
206500020110      * se applicazione tariffa diversa dal normale stampo
206600020110     c                   if        §taf02 = 'S'
206700020110
206800020110     c                   exsr      Sr_Over
206900020110
207000020110     c                   write     ta50p07s
207100020110     c                   endif
207200020110
207300020110     c                   exsr      Sr_Over
207400020110
207500020110     c                   if        §taf02 = 'G'
207600020110     c                   write     ta50p07g
207700020110     c                   endif
207800020625
207900020625      * stampo alcune diciture fisse
208000020625     c                   exsr      Sr_Over
208100020625     c                   write     ta50p08
208200030930
208300030930If  1c                   If        *In05
208400030930     c                   Add       4             curline
208500030930     c                   exsr      Sr_Over
208600030930     c                   write     ta50p08e
208700030930     c                   Add       6             curline
208800030930     c                   exsr      Sr_Over
208900030930     c                   write     ta50p08m
209000030930   x1c                   Else
209100030930     c                   Add       2             curline
209200030930     c                   exsr      Sr_Over
209300030930     c                   write     ta50p08d
209400030930    1c                   EndIf
209500030930
209600030930     c                   Add       2             curline
209700030930     c                   exsr      Sr_Over
209800030930     c                   write     ta50p08z
209900020110
210000020110      * validità tariffa
210100020110     c                   exsr      Sr_Over
210200020110
210300020110      * verifico se devo stampare "per:" solo se c'è o il numero spedizioni mini
210400020110      * me o massime o se c'è uno dei sui volumi auotmatici
210500020110     c                   if        tamlrc > 0 or tamlas > 0 or tamtin <> ' '
210600020110     c                   seton                                        26
210700020110     c                   endif
210800020110     c                   write     ta50p09
210900020110      * verifico se stampare spedizioni minime e massime
211000020110    1c                   if        tamlrc > 0 or tamlas > 0
211100020110     c                   eval      comlrc = tamlrc
211200020110     c                   eval      comlas = tamlas
211300020110
211400020110     c                   exsr      Sr_Over
211500020110
211600020110     c                   write     ta50p09a
211700020110    1c                   endif
211800020110      * verifico se devo stampare volume automatico
211900020110    1c                   if        tamtin = 'K'
212000020110      * volume automatico a  kg
212100020110     c                   z-add     tamkpm        comkpm
212200020110    1c                   endif
212300020110
212400020110    1c                   if        tamtin = 'C'
212500020110      * volume a collo
212600020110     c                   z-add     tamkpm        comckm
212700020110    1c                   endif
212800020110      * stampo volume automatico
212900020110    1c                   if        tamtin <> ' '
213000020110
213100020110     c                   exsr      Sr_Over
213200020110
213300020110    1c                   if        tamtin = 'K'
213400020110     c                   write     ta50p09b
213500020110     c                   write     ta50p09s
213600020110     c                   else
213700020110     c                   write     ta50p09c
213800020110     c                   write     ta50p09s
213900020110     c                   endif
214000020110      * se non c'è volume stampo una riga vuota prima delle note
214100020110     c                   else
214200020110     c                   write     ta50p09s
214300020110
214400020110    1c                   endif
214500020110      * stampo le note
214600020110     c                   z-add     1             x
214700020110
214800020110    1c                   do        4             x
214900020110     c                   if        note(x) <> *blanks
215000020110     c                   movel     note(x)       comnot
215100020110
215200020110     c                   exsr      Sr_Over
215300020110
215400020110     c                   write     ta50p10
215500020110     c                   endif
215600020110    1c                   enddo
215700020110      * se c'è stampo l'addizionale di gestione
215800020110    1c                   if        tampag > 0
215900060704      * in lingua
216000060704     c                   select
216100060704     c                   when      i45lin = 'I'
216200041129     c                   Eval      comdag = 'Addizionale di gestione -
216300041129     c                             anni precedenti:'
216400060712     c                   when      i45lin = '2'
216500060712     c                   eval      comdag = 'Surcharge for previous -
216600060712     c                             years'' administration:'
216700060726     c                   when      i45lin = '3'
216800060726     c                   eval      comdag = 'Complémentaire de gestion -
216900060726     c                             années précédentes:'
217000060712     c                   when      i45lin = '4'
217100060726     c                   Eval      comdag = 'Aufschlag für Bearbeitung -
217200060726     c                             von Vorjahren:'
217300060712     c                   endsl
217400041129     c                   eval      comcag = tampag
217500020110     c                   seton                                        27
217600020110    1c                   endif
217700041129      * se c'è stampo l'addizionale di gestione anno corrente
217800041129    1c                   if        tamppa > 0
217900041129     c                   If        *In27
218000060704      * in lingua
218100060704     c                   select
218200060704     c                   when      i45lin = 'I'
218300041129     c                   Eval      comdag1 = 'Addizionale di gestione -
218400060818     c                             corrente.......:'
218500060704     c                   when      i45lin = '2'
218600060704     c                   eval      comdag1 = 'Surcharge for current -
218700060818     c                             administration........:'
218800060726     c                   when      i45lin = '3'
218900060726     c                   eval      comdag1 = 'Complémentaire de gestion -
219000060818     c                             courante..........:'
219100060712     c                   when      i45lin = '4'
219200060726     c                   Eval      comdag1 = 'Aufschlag für Bearbeitung -
219300060726     c                             des laufenden Jahres:'
219400060704     c                   endsl
219500041129     c                   eval      comcag1 = tamppa
219600041129     c                   seton                                        29
219700041129     c                   else
219800060704      * in lingua
219900060704     c                   select
220000060704     c                   when      i45lin = 'I'
220100041129     c                   Eval      comdag = 'Addizionale di gestione -
220200060726     c                             corrente:'
220300060704     c                   when      i45lin = '2'
220400060704     c                   eval      comdag = 'Surcharge for current -
220500060704     c                             administration:'
220600060726     c                   when      i45lin = '3'
220700060726     c                   eval      comdag = 'Complémentaire de gestion -
220800060726     c                             courante:'
220900060712     c                   when      i45lin = '4'
221000060726     c                   Eval      comdag = 'Aufschlag für Bearbeitung -
221100060726     c                             des laufenden Jahres:'
221200060704     c                   endsl
221300041129     c                   eval      comcag = tamppa
221400041129     c                   seton                                        27
221500041129    1c                   endif
221600041129    1c                   endif
221700020110
221800020110      * stampo commenti prima dele tariffe particolari
221900041222     c                   Add       3             curline
222000020110     c                   exsr      Sr_Over
222100020110
222200020110     c                   write     ta50p11
222300031120      * se c'è istat stampo
222400101015     c                   if        tamrct >  0
222500031120      * recupero la data di decorrenza
222600031120      *
222700050525      * se superiore alla data decorrenza della tariffa stampo la data
222800050525      * decorrenza ista
222900051124      * non + si stampa la data decorrenza dell'indice ISTAT
223000051124     c*****              If        dia(tamfli) >= tamddt
223100101015     c     *iso          movel     dia(tamrct)   data_EUR
223200050527     C     *eur          movel     DATA_EUR      dtaist
223300050525      * se la data  data decorrenza istat è inferiore alla data decorrenza
223400050525      * della tariffa stampo la data decorrenza tariffa
223500051124     c*******            Else
223600051124     c**** *iso          movel     tamddt        data_EUR
223700051124     C**** *eur          movel     DATA_EUR      dtaist
223800051124     c*******            EndIf
223900031120      *
224000031120      * controllo se % diversa da 100
224100031120     c                   eval      *in11 = (tampri <> 100)
224200031120      * stampo
224300031120
224400150317     c                   Add       4             curline
224500031120     c                   exsr      SR_OVER
224600031120
224700031120     c                   write     ta50pist
224800031120      *
224900031120     c                   endif
225000021205      * stampo riga vuota
225100021205     c                   write     ta50p09s
225200020110
225300020110     c                   endsr
225400050131
225500050131      *------------------------------------------------------------------*
225600050131      *  Sr_ChkLnp        CONTROLLO SE LNP OK X LA STAMPA
225700050131      *------------------------------------------------------------------*
225800050131
225900050131     c     Sr_ChkLnp     BegSr
226000050131
226100050131     c                   If        wokstp = *Off
226200050131     c                   Select
226300050131     c                   when      Tadlnp = Comfil
226400050131     c                   Eval      wokstp = *on
226500050131     c                   when      Tadlnp = 950
226600050131     c                   Eval      wokstp = *on
226700050131     c                   EndSl
226800050131     c                   EndIf
226900050131
227000050131     c                   EndSr
227100020110
227200020110      *------------------------------------------------------------------*
227300020110      *  Sr_Statpt        STAMPA TARIFFE PARTICOLARI
227400020110      *------------------------------------------------------------------*
227500020110
227600020110     c     Sr_Statpt     begsr
227700020110
227800020110      * accendo indicatore tariffe particolari
227900020110     c                   seton                                        10
228000021205
228100021205      * Pulisco le sk per stampa tariffe particolari su + righe
228200021205     c                   Clear                   Tpta
228300021205     c                   Clear                   Tptb
228400021205     c                   Clear                   Tptc
228500021205     c                   Clear                   Atp1
228600021205     c                   Clear                   Atpc
228700060313      * pulisco campo di comodo
228800060313     c                   clear                   savapl
228900020111
229000020110      * leggo la schiera ordinata delle tariffe particolari
229100020110     c                   do        50            j
229200020110      * se valorizzata
229300020110     c                   if        cp(j) <> *blank
229400020110      * aggancio la tabella relativa le tariffe particolari
229500020110     c                   setoff                                       99
229600020110     c                   z-add     1             y
229700020110     c     cp(j)         lookup    ftc(y)                                 31
229800020111      * verifico se è una tariffa particolare da stampare
229900020110     c                   if        not *in31
230000020110     c                   seton                                        99
230100020110     c                   else
230200020111
230300020110     c                   endif
230400020111
230500020110     c                   select
230600020110      * se tariffa stampabile (99 spento)
230700020110     c                   when      *in99
230800020110      * se è una tariffa particolare che vien stampata in modo standard
230900020110     c                   when      tst(y) = ' '
231000020110     c                   exsr      Sr_Taparstd
231100020110      * se è una tariffa particolare che non vien stampata in modo standard
231200020110     c                   when      tst(y) = 'N'
231300020110      * verifico che tariffa particolare è
231400020110      * elavazione limite risarcibile
231500020110     c                   if        ftc(y) = 'R'
231600020110     c                   exsr      Sr_Elliris
231700020110     c                   endif
231800060313      * mandato assicurativo AC PLUS
231900020110     c                   if        ftc(y) = 'C'
232000020110     c                   exsr      Sr_Mandato
232100020110     c                   endif
232200060313      * mandato assicurativo AC BASE
232300060313     c                   if        ftc(y) = 'd'
232400060313     c                   exsr      sr_acbase
232500060313     c                   endif
232600020110
232700020110     c                   endsl
232800020110
232900020110     c                   endif
233000020110
233100020110     c                   enddo
233200020110
233300020110     c                   endsr
233400021205
233500021205      *-------------------------------------------------------------------------
233600021205      *  Sr_StaTpt1   Stampa tariffe particolari da sk unica
233700021205      *-------------------------------------------------------------------------
233800021205     c     Sr_StaTpt1    BegSr
233900021205
234000021205     c                   Clear                   Conta
234100021205     c                   Eval      WokTp = *Off
234200021205      ***
234300021205      * Stampo prima le tariffe particolari con solo l'importo
234400021205Do  1c                   Do        50            Atp1
234500021205     c                   If        Tpta(Atp1) = *Blanks
234600021205     c                   Leave
234700021205     c                   EndIf
234800021205     c                   Eval      Woktp = *On
234900021205      * due tariffe per riga
235000021205If  2c                   If        Conta = *Zeros
235100021205     c                   Clear                   Comstda2
235200021205     c                   Clear                   Comstdb2
235300021205     c                   Movel(p)  Tpta(Atp1)    Comstda1
235400021205     c                   Movel(p)  Tptb(Atp1)    Comstdb1
235500021205     c                   Add       1             Conta
235600021205   x2c                   Else
235700021205     c                   Movel(p)  Tpta(Atp1)    Comstda2
235800021205     c                   Movel(p)  Tptb(Atp1)    Comstdb2
235900021205     c                   ExSr      Sr_Over
236000021205     c                   Write     Ta50pstd1
236100021205     c                   Clear                   Conta
236200021205    2c                   EndIf
236300021205
236400021205    1c                   EndDo
236500021205      * Stampo ultima riga non completa
236600021205     c                   If        WokTp = *On and Conta <> *Zeros
236700021205     c                   ExSr      Sr_Over
236800021205     c                   Write     Ta50pstd1
236900021205     c                   EndIf
237000021205      * stampo riga vuota
237100021205     c                   write     ta50p09s
237200021205
237300021205     c                   Eval      WokTp = *Off
237400021205     c                   Z-Add     *Zeros        CntList
237500021205      ***
237600021205      * Stampo per ultime le tariffe particolari "al costo"
237700021205Do  1c                   Do        50            Atpc
237800021205      * Stampo ultima riga
237900021205     c                   If        Tptc(Atpc) = *Blanks
238000021205     c                   Add       1             CntList
238100060704     c                   select
238200060704     c                   when      i45lin = 'I'
238300021205     c                   Eval      Comstdc1 = ': al costo'
238400060704     c                   when      i45lin = '2'
238500060704     c                   Eval      Comstdc1 = ': at cost'
238600060801     c                   when      i45lin = '3'
238700060801     c                   Eval      Comstdc1 = ': au coût'
238800060712     c                   when      i45lin = '4'
238900060712     c                   Eval      Comstdc1 = ':zum Preis'
239000060704     c                   endsl
239100021205     c                   ExSr      StpRiga
239200021205     c                   Leave
239300021205     c                   EndIf
239400021205
239500021205     c                   If        %Len(%Trim(Comstdc)) + 1 +
239600021205     c                             %len(%trim(Tptc(Atpc))) + 1 <= Lungh
239700021205     c                   ExSr      CrtRiga
239800021205     c                   Else
239900021205     c                   Add       1             CntList
240000021205     c                   Clear                   Comstdc1
240100021205     c                   ExSr      StpRiga
240200021205     c                   ExSr      CrtRiga
240300021205     c                   EndIf
240400021205
240500021205    1c                   EndDo
240600021205
240700021205     c                   EndSr
240800021205      *
240900021205      *-------------------------------------------------------------------------
241000021205      * CrtRiga - Routine di creazione riga tar.particolari "al costo"
241100021205      *-------------------------------------------------------------------------
241200021205     c     CrtRiga       BegSr
241300021205
241400021205     c                   If        Comstdc = *Blanks
241500021205     c                   Eval      Comstdc = %Trim(Comstdc) + %Trim(Tptc(Atpc))
241600021205     C                   Else
241700021205     c                   Eval      Comstdc = %Trim(Comstdc) + ','
241800021205     c                                       + %Trim(Tptc(Atpc))
241900021205     c                   EndIf
242000021205
242100021205     c                   EndSr
242200021205      *
242300021205      *-------------------------------------------------------------------------
242400021205      * StpRiga - Routine di stampa riga tar.particolari "al costo"
242500021205      *-------------------------------------------------------------------------
242600021205     c     StpRiga       BegSr
242700021205
242800021205     c                   If        Comstdc <> *Blanks
242900021205     c                   Eval      TxtInput = %trim(Comstdc)
243000021205     c                   If        (Cntlist > 0) and
243100021205     c                             (%len(%Trim(TxtInput)) > 75)
243200021205     c                   ExSr      ExeGiust
243300021205     c                   If        Not %error
243400021205     c                   Eval      Comstdc = TxtGiust
243500021205     c                   Else
243600021205     c                   Eval      Comstdc = TxtInput
243700021205     c                   EndIf
243800021205     c                   Else
243900021205     c                   Eval      Comstdc = TxtInput
244000021205     c                   EndIf
244100021205     c                   ExSr      Sr_Over
244200021205     c                   Write     Ta50pstdc
244300021205     c                   Clear                   Comstdc
244400021205     c                   EndIf
244500021205
244600021205     c                   EndSr
244700021205      *
244800021205      *-------------------------------------------------------------------------
244900021205      * EXEGIUST - Routine di "GIUSTIFICAZIONE" testo
245000021205      *-------------------------------------------------------------------------
245100021205     c     ExeGiust      BegSr
245200021205
245300021205     c                   Call(e)   'XGIUST'
245400021205     c                   Parm                    TxtInput
245500021205     c                   Parm                    Lungh
245600021205     c                   Parm                    TxtGiust
245700021205     c                   Parm                    TxtResto
245800021205
245900021205     c                   EndSr
246000020110
246100020110      *------------------------------------------------------------------*
246200020110      *  Sr_Statgc        STAMPA TARIFFE GIACENZA
246300020110      *------------------------------------------------------------------*
246400020110
246500020110     c     Sr_Statgc     begsr
246600020110
246700020110     c                   if        *in98
246800020110     c     ds1ktm        chain     tiogc01l                           99
246900020110
247000020110     c                   else
247100020110
247200020110     c     ds1ktm        chain     titgc01l                           99
247300020110     c                   endif
247400020110
247500020110     c                   if        *in99 or tgcatb = 'A'
247600020110     c                   goto      endstatgc
247700020110     c                   endif
247800020110      * rapporto peso volume
247900020110     c                   eval      comgpv = tgcrpv * 100
248000020110      * preaparo i campi in stampa
248100020110     c                   z-add     tgcsgd        comsgd
248200020110     c                   z-add     tgcsgv        comsgv               28
248300020110     c                   z-add     tgcsgp        comsgp
248400020110     c                   z-add     tgcsgr        comsgr
248500020110     c                   z-add     tgcstm        comstm
248600020110     c                   z-add     tgcst1        comst1
248700020110     c                   z-add     tgcsg1        comsg1
248800020110     c                   z-add     tgcst2        comst2
248900020110
249000020110      * se giorni di rientro sono a zero li recupero dalla tabella "2G"
249100020110     c                   if        tgcggr = *zeros
249200020213      * tariffa estera  FEDEX
249300020213     c                   z-add     §2ggfe        tgcggr
249400020110
249500020110
249600020110     c                   endif
249700020110
249800020110     c                   z-add     tgcggr        comggr
249900020110
250000020110     c                   if        *in80 or curline > 55
250100020110     c                   write     ta50pfin
250200020110     c                   write     ta50pPP
250300020110     c                   setoff                                       80
250400020110     c                   endif
250500020110
250600020110     c                   write     ta50pgi
250700020110      * verifico se c'è la franchigia
250800020110     c                   if        tgcsgf > 0
250900020110     c                   z-add     tgcsgf        comsgf
251000020110      * verifico il falg applicazione franchigia
251100060704      * e stampo in lingua
251200060704     c                   select
251300060704     c                   when      i45lin = 'I'
251400020110     c                   if        tgcfaf = '1'
251500020110     c                   eval      comapf = cspe1
251600020110     c                   endif
251700020110     c                   if        tgcfaf = '2'
251800020110     c                   eval      comapf = cspe2
251900020110     c                   endif
252000060118     c                   if        tgcfaf = '3'
252100060118     c                   eval      comapf = cspe3
252200060118     c                   endif
252300060704
252400060704     c                   when      i45lin = '2'
252500060704     c                   if        tgcfaf = '1'
252600060704     c                   eval      comapf = cspe12
252700060704     c                   endif
252800060704     c                   if        tgcfaf = '2'
252900060704     c                   eval      comapf = cspe22
253000060704     c                   endif
253100060704     c                   if        tgcfaf = '3'
253200060704     c                   eval      comapf = cspe32
253300060704     c                   endif
253400060726
253500060726     c                   when      i45lin = '3'
253600060726     c                   if        tgcfaf = '1'
253700060726     c                   eval      comapf = cspe13
253800060726     c                   endif
253900060726     c                   if        tgcfaf = '2'
254000060726     c                   eval      comapf = cspe23
254100060726     c                   endif
254200060726     c                   if        tgcfaf = '3'
254300060726     c                   eval      comapf = cspe33
254400060726     c                   endif
254500060712
254600060712     c                   when      i45lin = '4'
254700060712     c                   if        tgcfaf = '1'
254800060712     c                   eval      comapf = cspe14
254900060712     c                   endif
255000060712     c                   if        tgcfaf = '2'
255100060712     c                   eval      comapf = cspe24
255200060712     c                   endif
255300060712     c                   if        tgcfaf = '3'
255400060712     c                   eval      comapf = cspe34
255500060712     c                   endif
255600060704     c                   endsl
255700060118
255800020110     c                   exsr      Sr_Over
255900020110
256000020110     c                   write     ta50pgif
256100020110
256200020110     c                   endif
256300031008
256400031008     c                   Add       2             curline
256500031008     c                   exsr      Sr_Over
256600031008     c                   write     ta50pgi1
256700020110
256800020110     c     endstatgc     endsr
256900020110
257000020110      *------------------------------------------------------------------*
257100020110      *  Sr_Taparstd      STAMPA DATI DELLA TARIFFA PARTICOLARE STANDARD
257200020110      *------------------------------------------------------------------*
257300020110
257400020110     c     Sr_Taparstd   begsr
257500020110
257600020110     c                   movel(P)  ftc(y)        savftc
257700020110     c                   movel(p)  edtc(y)       comstd
257800020110      * se si tratta della tariffa particolare isola accendo
257900020110     c                   if        ftc(y) = 'I'
258000020110     c                   seton                                        06
258100020110     c                   endif
258200020110
258300061011     c  n98ktpt          chain     titpt01l
258400061011     c   98ktpt          chain     tiopt01l
258500020110      * se non trovato vado a fine senza stampare
258600061011     c                   if        not %found or tptatb = 'A'
258700020110      * verifico se devo stampare al costo
258800020110     c                   if        edac(y) = 'S'
258900021205
259000021205      * se non devo stampare righe aggiuntive salvo la tariffa particolare
259100021205      * in una sk che stamperò alla fine di tutte le tariffe particolari
259200021205If  3c                   If        Esud(y) <> 'S'
259300021205     c                   Add       1             Atpc
259400021205     c                   Movel     Comstd        Tptc(Atpc)
259500021205
259600021205      * se devo stampare righe aggiuntive stampo anche la tariffa particolare
259700021205   x3c                   Else
259800060704     c                   select
259900060704     c                   when      i45lin = 'I'
260000020110     c                   eval      comdstd = ': al costo       '
260100060704     c                   when      i45lin = '2'
260200060704     c                   eval      comdstd = ': at cost        '
260300060801     c                   when      i45lin = '3'
260400060801     c                   eval      comdstd = ': au coût        '
260500060712     c                   when      i45lin = '4'
260600060711     c                   eval      comdstd = ': zum Preis      '
260700060704     c                   endsl
260800020110
260900020110     c                   exsr      Sr_Over
261000020110
261100020110     c                   write     ta50pstd
261200020110      * vado a stampare righe aggiuntive se richieste
261300020110     c                   goto      sudtpstd
261400021205    3c                   EndIf
261500021205
261600020110     c                   else
261700020110      * altrimenti vado a fine e non stampo nulla
261800020110     c                   goto      endtpstd
261900020110     c                   endif
262000020110
262100020110     c                   endif
262200020110      * cerco se ha record validi con tariffa o minimo > 0
262300020110     c                   exsr      Ricerc
262400020110      * per 04 acceso stampo nessun addebito
262500020110     c                   if        *in04
262600020110     c                   goto      endtpstd
262700020110     c                   endif
262800020110
262900020110      * per 04 spento verifico quanti record dettaglio tariffario esistono
263000020110      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
263100020110     c                   if        recval = 1
263200020110     c                   exsr      Sr_Gesunico
263300021205
263400021205      * se la tariffa particolare è di 1 riga con solo l'importo
263500021205      * salvo la tariffa particolare in una sk che stamperò alla fine di
263600021205      * tutte le tariffe particolari prima della sk al costo
263700021205If  2c                   If        Esud(y) <> 'S' and
263800021205     c                             W_Tarparest = *Blanks and
263900021205     c                             oltre = *Zeros and TpdMin = *Zeros and
264000021205     c                             TpdMax = *Zeros and TptTpg <> '0' and
264100021205     c                             TptRpv = *Zeros and TpTArl = *Zeros
264200021205     c                   Add       1             Atp1
264300021205     c                   Z-Add     TpdItr        Witr
264400021205     c                   Eval      Tpta(Atp1) = Comstd
264500021205      * no percentuale
264600021205If  3c                   If        W_Per = ' '
264700021205     c                   Eval      Tptb(Atp1) = ': ' +
264800021205     c                                          Wrkdestpg + ' ' +
264900021205     c                                          %Editc(Witr:'3')
265000021205      * sì percentuale
265100021205   x3c                   Else
265200021205     c                   Eval      Tptb(Atp1) = ': ' +
265300021205     c                                          Wrkdestpg + ' ' +
265400021205     c                                          StpPer + ' % '
265500021205    3c                   EndIf
265600021205
265700021205      * se devo stampare righe aggiuntive o l'estensione della tariffa
265800021205      * particolare stampo anche la tariffa particolare
265900021205   x2c                   Else
266000020110
266100020110     c                   eval      comdstd = w_tarpar
266200020110
266300020110     c                   exsr      Sr_Over
266400020110
266500020110     c                   write     ta50pstd
266600020110
266700020110     c                   exsr      findet
266800021205    2c                   EndIf
266900020110     c                   else
267000020110
267100020110      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
267200020110     c                   exsr      Sr_Gestpd
267300020110
267400020110     c                   endif
267500020110
267600020110     c     sudtpstd      tag
267700020110
267800020110      * verifico se devo stampare righe ulteriori
267900020110     c                   if        esud(y) = 'S'
268000020110
268100020110     c                   exsr      Sr_Over
268200020110
268300020110      * se tariffa particolare espresso
268400020110     c                   if        ftc(y) = 'E'
268500020110     c                   write     ta50pse2
268600020110     c                   endif
268700020110
268800020110      * se tariffa particolare ritiro
268900020110     c                   if        ftc(y) = 'U'
269000020110     c                   write     ta50por2
269100020110     c                   endif
269200020110
269300020110      * se tariffa particolare provvigione vettore
269400020110     c                   if        ftc(y) = 'V'
269500020110
269600060704      * in lingua
269700060704     c                   select
269800060704     c                   when      i45lin = 'I'
269900020110     c                   eval      comint = 'Vettore'
270000060704     c                   when      i45lin = '2'
270100060704     c                   eval      comint = 'Carrier'
270200060801     c                   when      i45lin = '3'
270300060907     c                   eval      comint = ' du Transporteur'
270400060712     c                   when      i45lin = '4'
270500060726     c                   eval      comint = 'Frachtführe'
270600060704     c                   endsl
270700020110     c                   write     ta50ppc1
270800020110
270900020110      *  stampo le condizioni di rimborso solo se tipo pagamento che necessita
271000020110      * codice abi cab
271100020110     c                   if        §4uabi = 'S'
271200020110     c                   exsr      Sr_Over
271300020110     c                   write     ta50ppc2
271400020110     c                   endif
271500020110
271600020110     c                   endif
271700020110
271800020110      * se tariffa particolare provvigione mittente
271900020110     c                   if        ftc(y) = 'W'
272000060704      * in lingua
272100060704     c                   select
272200060704     c                   when      i45lin = 'I'
272300020110     c                   eval      comint = 'Mittente'
272400060704     c                   when      i45lin = '2'
272500060704     c                   eval      comint = 'Sender'
272600060801     c                   when      i45lin = '3'
272700060907     c                   eval      comint = 'de l''Expéditeur'
272800060712     c                   when      i45lin = '4'
272900060726     c                   eval      comint = 'Absender'
273000060704     c                   endsl
273100020110     c                   write     ta50ppc1
273200020110     c                   endif
273300040428
273400040428      * se tariffa particolare supplemento carburante
273500040428     c                   if        ftc(y) = '+'
273600040428     c                   write     ta50psc
273700040428
273800040428     c                   exsr      Sr_Over
273900040428     c                   write     ta50psc1
274000040428
274100040428     c                   exsr      Sr_Over
274200040428     c                   write     ta50psc2
274300040428
274400040428     c                   exsr      Sr_Over
274500040428     c                   write     ta50psc3
274600040428
274700040428     c                   endif
274800020110
274900020110     c                   endif
275000020110
275100020110     c                   setoff                                       06
275200020110
275300020110     c     endtpstd      endsr
275400020110
275500020110      *------------------------------------------------------------------*
275600020110      *  Sr_Elliris STAMPA DATI DELLA TARIFFA PARTICOLARE ELEVAZ.LIMITE RISARC.
275700020110      *------------------------------------------------------------------*
275800020110
275900020110     c     Sr_Elliris    begsr
276000020110
276100020110     c                   movel     'R '          savftc
276200020110
276300061011     C  n98KTPT          CHAIN     TITPT01l
276400061011     c   98ktpt          chain     tiopt01l
276500020110      * se non trovato vado a fine e non stampo nulla
276600061011     c                   if        not %found
276700020110     c                   goto      endelliris
276800020110     c                   endif
276900020110      * se trovato ma annullato vado a fine  e non stampo nulla
277000020110     c                   if        tptatb = 'A'
277100020110     c                   goto      endelliris
277200020110     c                   endif
277300020110      * se esiste stampo la scritta assicurazioni merci ed accendo indicatore
277400020110      * che mi segnala di aver stampato questa riga per non farlo dopo
277500020110     c                   exsr      Sr_Over
277600020110
277700020111     c                   write     ta50ptam
277800020110     c                   seton                                        81
277900020110      * recupero i valori standard dalla tabella GEI
278000020110     c                   clear                   dgei
278100020110     c                   movel     'C'           t02mod
278200020110     c                   movel     knsif         t02sif
278300020110     c                   movel     'GEI'         t02cod
278400020110
278500020110     c                   movel     §tadiv        t02ke1
278600020110
278700020110     c                   call      'TIBS02R'
278800020110     c                   parm                    kpjba
278900020110     c                   parm                    tibs02ds
279000020110
279100020110     c                   if        t02err = *blanks
279200020110     c                   movel     t02uni        dgei
279300020110     c                   endif
279400020110
279500020110      * verifico se limite risarcibile > di quello standard
279600020110     c                   if        tptvlm > §gelrp
279700020110     c                   seton                                        22
279800020110     c                   z-add     tptvlm        comelr
279900020110     c                   z-add     §gelrp        comlrp
280000020110
280100020110      * decodifico il flag valore merce
280200020110
280300020110     c                   movel     'TR'          cod
280400020110     c                   movel(P)  tptfvm        key
280500060704     c     tabkeyl       chain     tabel00f
280600020110
280700020110     c                   if        %found(tabel00f)
280800020110     c                   movel     tbluni        dstr
280900020110     c                   else
281000020110     c                   clear                   dstr
281100020110     c                   endif
281200020110
281300020110     c                   endif
281400020110      * validità mandato per le spedizioni in partenza e/o in arrivo
281500020110     c                   if        tptapl = ' '
281600060704      * in lingua
281700060704     c                   select
281800060704     c                   when      i45lin = 'I'
281900020110     c                   eval      comvma = 'PARTENZA e in ARRIVO'
282000060704     c                   when      i45lin = '2'
282100060704     c                   eval      comvma = 'DEPARTING and ARRIVING'
282200060726     c                   when      i45lin = '3'
282300060726     c                   eval      comvma = 'DÉPART ET ARRIVÉE'
282400060712     c                   when      i45lin = '4'
282500060726     c                   eval      comvma = 'ABFAHRT und EINGANG'
282600060704     c                   endsl
282700020110     c                   endif
282800020110      * partenza
282900020110     c                   if        tptapl = 'P'
283000060704      * in lingua
283100060704     c                   select
283200060704     c                   when      i45lin = 'I'
283300020110     c                   eval      comvma = 'PARTENZA'
283400060704     c                   when      i45lin = '2'
283500060704     c                   eval      comvma = 'DEPARTURE'
283600060726     c                   when      i45lin = '3'
283700060726     c                   eval      comvma = 'DÉPART'
283800060712     c                   when      i45lin = '4'
283900060726     c                   eval      comvma = 'ABFAHRT'
284000060704     c                   endsl
284100020110     c                   endif
284200020110      * arrivo
284300020110     c                   if        tptapl = 'A'
284400060704      * in lingua
284500060704     c                   select
284600060704     c                   when      i45lin = 'I'
284700020110     c                   eval      comvma = 'ARRIVO'
284800060704     c                   when      i45lin = '2'
284900060704     c                   eval      comvma = 'ARRIVAL'
285000060726     c                   when      i45lin = '3'
285100060726     c                   eval      comvma = 'ARRIVÉE'
285200060712     c                   when      i45lin = '4'
285300060726     c                   eval      comvma = 'EINGANG'
285400060704     c                   endsl
285500020110     c                   endif
285600020110
285700020110      * cerco se ha record validi con tariffa o minimo > 0
285800020110     c                   exsr      Ricerc
285900020110
286000020110     c                   if        *in22
286100020110     c                   exsr      Sr_Over
286200020110     c                   write     ta50pel1
286300020110     c                   else
286400020110     c                   exsr      Sr_Over
286500020110     c                   write     ta50pel2
286600020110     c                   endif
286700020110
286800020110      * per 04 acceso stampo nessun addebito
286900020110     c                   if        *in04
287000020110
287100060704     c                   select
287200060704     c                   when      i45lin = 'I'
287300020110     c                   eval      comma = ': nessun addebito'
287400060704     c                   when      i45lin = '2'
287500060704     c                   eval      comma = ': no charge      '
287600060728     c                   when      i45lin = '3'
287700060728     c                   eval      comma = ': aucun frais    '
287800060712     c                   when      i45lin = '4'
287900060728     c                   eval      comma = ': Keine Kosten   '
288000060704     c                   endsl
288100020110
288200020110     c                   exsr      Sr_Over
288300020110
288400020110      * elevazione limite risarcibile
288500020110     c                   write     ta50pma
288600020110     c                   write     ta50pvma
288700020110     c                   goto      endelliris
288800020110     c                   endif
288900020110
289000020110      * per 04 spento verifico quanti record dettaglio tariffario esistono
289100020110      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
289200020110     c                   if        recval = 1
289300020110     c                   exsr      Sr_Gesunico
289400020110      * se è tariffa a valore senza l'oltre e con elevazione stampo 1 riga <>
289500020110     c                   if        w_ellimris = 'Y'
289600020110
289700020110     c                   exsr      Sr_Over
289800020110
289900020110     c                   write     ta50pel
290000020110
290100020110     c                   else
290200020110
290300020110     c                   eval      comma = w_tarpar
290400020110
290500020110     c                   exsr      Sr_Over
290600020110
290700020110     c                   write     ta50pma
290800020110
290900020110     c                   endif
291000020110
291100020110      * stampo eventuale estensione scaglione unico
291200020110     c                   exsr      Sr_Estes
291300020110
291400020111     c                   exsr      Findet
291500020110
291600020110     c                   else
291700020110
291800020110      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
291900020110
292000020110     c                   exsr      Sr_Gestpd
292100020110
292200020110     c                   endif
292300020110      * stampo l'ultima riga
292400020110
292500020110     c                   exsr      Sr_Over
292600020110     c                   write     ta50pvma
292700020110
292800020110     c     endelliris    endsr
292900020110
293000020110      *------------------------------------------------------------------*
293100020110      *  Sr_Mandato STAMPA DATI DELLA TARIFFA PARTICOLARE ASSICURAZIONE X CONTO
293200020110      *------------------------------------------------------------------*
293300020110
293400020110     c     Sr_Mandato    begsr
293500020110
293600020110     c                   setoff                                       2124
293700020110
293800020110     c                   movel     'C '          savftc
293900020110
294000061011     c  n98ktpt          chain     titpt01l
294100061011     c   98ktpt          chain     tiopt01l
294200020110      * se non trovato vado a fine e non stampo nulla
294300061011     c                   if        not %found
294400020110     c                   goto      endmandato
294500020110     c                   endif
294600020110      * se trovato ma annullato vado a fine  e non stampo nulla
294700020110     c                   if        tptatb = 'A'
294800020110     c                   goto      endmandato
294900020110     c                   endif
295000020110      * se esiste stampo la scritta assicurazioni merci solo se 81 spento
295100020110      * cioè non l'ho già stampata prima
295200020110     c                   exsr      Sr_Over
295300020110
295400020110     c  n81              write     ta50PTAM
295500020110     c  n81              seton                                        81
295600020110      * verifico se mandato fittizio
295700020110     c                   if        tpttma = 'F'
295800020110     c                   seton                                        24
295900020110     c                   endif
296000020110      * verifico se mandato a valore variabile
296100020110     c                   if        tptvlm = 0
296200020110     c                   seton                                        21
296300020110     c                   endif
296400060313      * mi salvo il tptapl x stampa AC BASE
296500060313     c                   eval      savapl = tptapl
296600020110      * validità mandato per le spedizioni in partenza e/o in arrivo
296700020110     c                   if        tptapl = ' '
296800060704      * in lingua
296900060704     c                   select
297000060704     c                   when      i45lin = 'I'
297100020110     c                   eval      comvma = 'PARTENZA e in ARRIVO'
297200060704     c                   when      i45lin = '2'
297300060704     c                   eval      comvma = 'DEPARTING and ARRIVING'
297400060728     c                   when      i45lin = '3'
297500060728     c                   eval      comvma = 'DÉPART ET ARRIVÉE'
297600060712     c                   when      i45lin = '4'
297700060728     c                   eval      comvma = 'ABFAHRT und EINGANG'
297800060704     c                   endsl
297900020110     c                   endif
298000020110      * partenza
298100020110     c                   if        tptapl = 'P'
298200060704      * in lingua
298300060704     c                   select
298400060704     c                   when      i45lin = 'I'
298500020110     c                   eval      comvma = 'PARTENZA'
298600060704     c                   when      i45lin = '2'
298700060704     c                   eval      comvma = 'DEPARTURE'
298800060728     c                   when      i45lin = '3'
298900060728     c                   eval      comvma = 'DÉPART'
299000060712     c                   when      i45lin = '4'
299100060728     c                   eval      comvma = 'ABFAHRT'
299200060704     c                   endsl
299300020110     c                   endif
299400020110      * arrivo
299500020110     c                   if        tptapl = 'A'
299600060704      * in lingua
299700060704     c                   select
299800060704     c                   when      i45lin = 'I'
299900020110     c                   eval      comvma = 'ARRIVO'
300000060704     c                   when      i45lin = '2'
300100060704     c                   eval      comvma = 'ARRIVAL'
300200060728     c                   when      i45lin = '3'
300300060728     c                   eval      comvma = 'ARRIVÉE'
300400060712     c                   when      i45lin = '4'
300500060728     c                   eval      comvma = 'EINGANG'
300600060704     c                   endsl
300700020110     c                   endif
300800020110      * se non è mandato a valore variabile e non è fittizio
300900020110     c                   if        not *in21 and not *in24
301000020110     c                   z-add     tptvlm        comvlm
301100020111
301200020110      * decodifico il flag valore merce
301300020110     c                   movel     'TR'          cod
301400020110     c                   movel(P)  tptfvm        key
301500060704     c     tabkeyl       chain     tabel00f
301600020110
301700020110     c                   if        %found(tabel00f)
301800020110     c                   movel     tbluni        dstr
301900020110     c                   else
302000020110     c                   clear                   dstr
302100020110     c                   endif
302200020110
302300020110     c                   movel(p)  §trde2        desfvm
302400020110
302500020110     c                   endif
302600020110
302700020110      * cerco se ha record validi con tariffa o minimo > 0
302800020110     c                   exsr      Ricerc
302900020110
303000020110      * mandato a valore variabile o con valore espresso (no fittizio)
303100020110     c                   if        not *in24
303200020110      * se mandato non a valore variabile
303300020110     c                   if        not *in21
303400020110     c                   write     ta50pma1
303500020110     c                   else
303600020110     c                   write     ta50pmav
303700020110     c                   endif
303800020110
303900020110     c                   else
304000020110     c                   write     ta50pmaf1
304100020110     c                   endif
304200020110      * per 04 acceso stampo nessun addebito
304300020110     c                   if        *in04
304400020110      * mandato a valore variabile o con valore espresso (no fittizio)
304500060704     c                   select
304600060704     c                   when      i45lin = 'I'
304700020110     c                   eval      comma = ': nessun addebito'
304800060704     c                   when      i45lin = '2'
304900060704     c                   eval      comma = ': no charge      '
305000060728     c                   when      i45lin = '3'
305100060728     c                   eval      comma = ': aucun frais    '
305200060712     c                   when      i45lin = '4'
305300060728     c                   eval      comma = ': Keine Kosten   '
305400060704     c                   endsl
305500020110
305600020110     c                   exsr      Sr_Over
305700020110
305800020110     c                   write     ta50pma
305900020110     c                   write     ta50pvma
306000020110     c                   goto      endmandato
306100020110     c                   endif
306200020110
306300020110      * per 04 spento verifico quanti record dettaglio tariffario esistono
306400020110      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
306500020110     c                   if        recval = 1
306600020110     c                   exsr      Sr_Gesunico
306700020110
306800020110     c                   eval      comma = w_tarpar
306900020110
307000020110     c                   exsr      Sr_Over
307100020110
307200020110     c                   write     ta50pma
307300020110
307400020110     c                   exsr      findet
307500020110
307600020110     c                   else
307700020110
307800020110      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
307900020110     c                   exsr      Sr_Gestpd
308000020110
308100020110     c                   endif
308200020111
308300020110      * stampo l'ultima riga
308400020110     c                   exsr      Sr_Over
308500020110     c                   write     ta50pvma
308600020110
308700020110     c     endmandato    endsr
308800020110
308900060313      *-------------------------------------------------------------------------
309000060313      *  sr_acbase STAMPA DATI DELLA TARIFFA PARTICOLARE d
309100060313      *-------------------------------------------------------------------------
309200060313     c     sr_acbase     begsr
309300060313
309400060313      * la tariffa ac base non può essere fittizia e non può essere a mandato
309500060313      * variabile
309600060313
309700060313     c                   movel     'd '          savftc
309800060313
309900061011     c  n98ktpt          chain     titpt01l
310000061011     c   98ktpt          chain     tiopt01l
310100060313      * se non trovato vado a fine e non stampo nulla
310200061011     c                   if        not %found
310300060313     c                   goto      endacbase
310400060313     c                   endif
310500060313      * se trovato ma annullato vado a fine  e non stampo nulla
310600060313     c                   if        tptatb = 'A'
310700060313     c                   goto      endacbase
310800060313     c                   endif
310900060313      * se esiste stampo la scritta assicurazioni merci solo se 81 spento
311000060313      * cioè non l'ho già stampata prima
311100060313
311200060313     c                   exsr      SR_OVER
311300060313
311400060313     c  n81              write     ta50PTAM
311500060313     c  n81              seton                                        81
311600060313      * validità mandato per le spedizioni in partenza e/o in arrivo
311700060313     c                   if        tptapl = ' '
311800060704      * in lingua
311900060704     c                   select
312000060704     c                   when      i45lin = 'I'
312100060313     c                   eval      comvma = 'PARTENZA e in ARRIVO'
312200060704     c                   when      i45lin = '2'
312300060704     c                   eval      comvma = 'DEPARTING and ARRIVING'
312400060728     c                   when      i45lin = '3'
312500060728     c                   eval      comvma = 'DÉPART ET ARRIVÉE'
312600060712     c                   when      i45lin = '4'
312700060728     c                   eval      comvma = 'ABFAHRT und EINGANG'
312800060704     c                   endsl
312900060313     c                   endif
313000060313      * partenza
313100060313     c                   if        tptapl = 'P'
313200060704      * in lingua
313300060704     c                   select
313400060704     c                   when      i45lin = 'I'
313500060313     c                   eval      comvma = 'PARTENZA'
313600060704     c                   when      i45lin = '2'
313700060704     c                   eval      comvma = 'DEPARTURE'
313800060728     c                   when      i45lin = '3'
313900060728     c                   eval      comvma = 'DÉPART'
314000060712     c                   when      i45lin = '4'
314100060728     c                   eval      comvma = 'ABFAHRT'
314200060704     c                   endsl
314300060313     c                   endif
314400060313      * arrivo
314500060313     c                   if        tptapl = 'A'
314600060704      * in lingua
314700060704     c                   select
314800060704     c                   when      i45lin = 'I'
314900060313     c                   eval      comvma = 'ARRIVO'
315000060704     c                   when      i45lin = '2'
315100060704     c                   eval      comvma = 'ARRIVAL'
315200060728     c                   when      i45lin = '3'
315300060728     c                   eval      comvma = 'ARRIVÉE'
315400060712     c                   when      i45lin = '4'
315500060728     c                   eval      comvma = 'EINGANG'
315600060704     c                   endsl
315700060313     c                   endif
315800060313      * se AC PLUS valida in partenza stampo ARRIVO su AC BASE
315900060313     c                   if        savapl = 'P'
316000060704      * in lingua
316100060704     c                   select
316200060704     c                   when      i45lin = 'I'
316300060313     c                   eval      comvma = 'ARRIVO'
316400060704     c                   when      i45lin = '2'
316500060704     c                   eval      comvma = 'ARRIVAL'
316600060728     c                   when      i45lin = '3'
316700060728     c                   eval      comvma = 'ARRIVÉE'
316800060712     c                   when      i45lin = '4'
316900060728     c                   eval      comvma = 'EINGANG'
317000060704     c                   endsl
317100060313     c                   endif
317200060313      * se AC PLUS valida in arrivo stampo PARTENZA su AC BASE
317300060313     c                   if        savapl = 'A'
317400060704      * in lingua
317500060704     c                   select
317600060704     c                   when      i45lin = 'I'
317700060313     c                   eval      comvma = 'PARTENZA'
317800060704     c                   when      i45lin = '2'
317900060704     c                   eval      comvma = 'DEPARTURE'
318000060728     c                   when      i45lin = '3'
318100060728     c                   eval      comvma = 'DÉPART'
318200060712     c                   when      i45lin = '4'
318300060728     c                   eval      comvma = 'ABFAHRT'
318400060704     c                   endsl
318500060313     c                   endif
318600060313
318700060313     c                   z-add     tptvlm        comvlm
318800060313      *
318900060313      * decodifico il flag valore merce
319000060313      *
319100060313     c                   movel     'TR'          cod
319200060313     c                   movel(p)  tptfvm        key
319300060704     c     tabkeyl       chain     tabel00f
319400060313
319500060313     c                   if        %found(tabel00f)
319600060313     c                   movel     tbluni        dstr
319700060313     c                   else
319800060313     c                   clear                   dstr
319900060313     c                   endif
320000060313
320100060313     c                   movel(p)  §trde2        desfvm
320200060313
320300060313      * cerco se ha record validi con tariffa o minimo > 0
320400060313     c                   EXSR      Ricerc
320500060313     c                   write     ta50pma2
320600060313      * per 04 acceso stampo nessun addebito
320700060313     c                   if        *in04
320800060313      * mandato a valore variabile o con valore espresso (no fittizio)
320900060704     c                   select
321000060704     c                   when      i45lin = 'I'
321100060313     c                   eval      comma = ': nessun addebito'
321200060704     c                   when      i45lin = '2'
321300060704     c                   eval      comma = ': no charge      '
321400060728     c                   when      i45lin = '3'
321500060728     c                   eval      comma = ': aucun frais    '
321600060728     c                   when      i45lin = '4'
321700060728     c                   eval      comma = ': Keine Kosten   '
321800060704     c                   endsl
321900060313      *
322000060313     c                   exsr      SR_OVER
322100060313      *
322200060313     c                   write     ta50pma
322300060313     c                   write     ta50pvma
322400060313     c                   goto      endacbase
322500060313     c                   endif
322600060313      *
322700060313      * per 04 spento verifico quanti record dettaglio tariffario esistono
322800060313      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
322900060313     c                   if        recval = 1
323000060313     C                   exsr      sr_gesunico
323100060313
323200060313     c                   eval      comma = w_tarpar
323300060313
323400060313     c                   exsr      SR_OVER
323500060313
323600060313     c                   write     ta50pma
323700060313
323800060313     c                   exsr      findet
323900060313
324000060313     c                   else
324100060313
324200060313      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
324300060313     c                   exsr      sr_gestpd
324400060313
324500060313     c                   endif
324600060313
324700060313      * stampo l'ultima riga
324800060313     c                   exsr      SR_OVER
324900060313     c                   write     ta50pvma
325000060313      *
325100060313     C     endacbase     endsr
325200060313      *
325300020110      *------------------------------------------------------------------*
325400020110      * Sr_Gestpd GESTIONE DEL DETTAGLIO TARIFFARIO UGUALE PER TUTTE LE TAR.PAR.
325500020110      *------------------------------------------------------------------*
325600020110
325700020110     c     Sr_Gestpd     begsr
325800020110
325900020110      * Decodfico tipo tariffa e scaglioni
326000020110     c                   movel     'TR'          cod
326100020111
326200020110     c                   clear                   atf
326300020110     c                   exsr      Dectpg
326400020110      * elaboro 6 scaglioni alla volta
326500020110     c                   z-add     1             k4
326600020110
326700020110    1c                   dow       k4 <= 18
326800020110     c                   MOVEA     SGLD(K4)      SSLD
326900020110     c                   MOVEA     SGLB(K4)      SSLB
327000020110     c                   MOVEA     SGLA(K4)      SSLA
327100020110      * 1 DECIMALE
327200020110     c                   IF        *IN91 AND NOT *IN37
327300020110     c                   MOVEA     SG1A(K4)      SS1A
327400020110     c                   MOVEA     SG1D(K4)      SS1D
327500020110     c                   ENDIF
327600020110      * 2 DECIMALI
327700020110     c                   IF        *IN91 AND *IN37
327800020110     c                   MOVEA     SG2A(K4)      SS2A
327900020110     c                   MOVEA     SG2D(K4)      SS2D
328000020110     c                   ENDIF
328100020110
328200020110      * CARICO GLI SCAGLIONI PER ELABORARE IL DETTAGLIO
328300020110
328400020110     c                   MOVEA     SCP(K4)       SSL
328500020110      *  INTESTAZIONE SCAGLIONI
328600020110     c                   Z-ADD     SSL(1)        W0050             5 0
328700020110     c     W0050         COMP      99999                                  41
328800020110     c     SSL(1)        COMP      *ZEROS                                 51
328900020110     c                   Z-ADD     SSL(2)        W0050             5 0
329000020110     c     W0050         COMP      99999                                  42
329100021220     c**N41W0050         COMP      *zeros                                 41
329200020110     c     SSL(2)        COMP      *ZEROS                                 52
329300020110     c                   Z-ADD     SSL(3)        W0050             5 0
329400020110     c     W0050         COMP      99999                                  43
329500020110     c     SSL(3)        COMP      *ZEROS                                 53
329600020110     c                   Z-ADD     SSL(4)        W0050             5 0
329700020110     c     W0050         COMP      99999                                  44
329800020110     c     SSL(4)        COMP      *ZEROS                                 54
329900020110     c                   Z-ADD     SSL(5)        W0050             5 0
330000020110     c     W0050         COMP      99999                                  45
330100020110     c     SSL(5)        COMP      *ZEROS                                 55
330200020110     c                   Z-ADD     SSL(6)        W0050             5 0
330300020110     c     W0050         COMP      99999                                  46
330400020110     c     SSL(6)        COMP      *ZEROS                                 56
330500020110
330600020110      * SE IL 41 E' ACCESO PERCHÈ IL PRIMO SCAGLIONE E' UGUALE A TUTTI 999999
330700020110      * O IL SECONDO SCAGLIONE E' A ZERO
330800020110      * MA SE NON SI TRATTA + DEL 1° SCAGLIONE IN ASSOLUTO DEVO STAMPARE "OLTRE
330900020110      * LO SCAGLIONE PRECEDENTE
331000020110      * QUINDI ACCENDO IL 40 E SPENGO IL 41
331100020110
331200020110     c                   IF        *IN41 AND K4 <> 1
331300020110     c                   SETOFF                                       41
331400020110     c                   SETON                                        40
331500020110     c                   ENDIF
331600020110
331700020110     c                   exsr      Decsca
331800020110      * LETTURA DEL DETTAGLIO
331900020110     c                   exsr      Legdet
332000020110
332100020110      * rielaboro altri 6 scaglioni
332200020110     c                   add       6             k4
332300020110      * se scaglioni minori o uguali a 18 e scaglione valorizzato
332400020110    2c                   if        k4 <= 18 and
332500020110     c                             scp(k4) <> *zeros
332600020110      * pulisco schiere di comodo per la stampa degli scaglioni
332700020110     c                   clear                   ssld
332800020110     c                   clear                   sslb
332900020110     c                   clear                   ssla
333000020110     c                   clear                   ss1d
333100020110     c                   clear                   ss1a
333200020110     c                   clear                   ss2d
333300020110     c                   clear                   ss2a
333400020110     c                   clear                   ssl
333500020110      * spengo indicatori che condizionano la stampa
333600020110     c                   setoff                                       404142
333700020110     c                   setoff                                       434445
333800020110     c                   setoff                                       46
333900020110     c                   setoff                                       515253
334000020110     c                   setoff                                       545556
334100020110
334200020110     c                   else
334300020110      * non ci sono + scaglioni da stampare
334400020110     c                   eval      k4=19
334500020110     c                   leave
334600020110
334700020110    2c                   endif
334800020110
334900020110    1c                   enddo
335000020110
335100020110     c                   exsr      Findet
335200020110
335300020110     c                   endsr
335400020110
335500020110      *------------------------------------------------------------------*
335600020110      * Sr_Gesunico       GESTIONE DETTAGLIO TARIFFARIO CON UNA SOLA RIGA
335700020110      *------------------------------------------------------------------*
335800020110
335900020110     c     Sr_Gesunico   begsr
336000020110
336100020110     c                   clear                   w_tarpar
336200020110     c                   clear                   w_tarparest
336300020110     c                   clear                   w_ellimris
336400020110      * Decodfico tipo pagamento
336500020110     c                   movel     'TR'          cod
336600020110     c                   movel(p)  tpttpg        key
336700060704     c     tabkeyl       chain     tabel00f
336800020110
336900020110     c                   if        %found(tabel00f)
337000020110     c                   movel     tbluni        dstr
337100020110     c                   else
337200020110     c                   clear                   dstr
337300020110     c                   endif
337400020110
337500020110      * verifico se scaglioni a valore
337600020110     c                   if        §trtap = 'S'
337700020110     c                   eval      W_per = '%'
337800020110     c                   else
337900020110     c                   clear                   w_per
338000020110     c                   endif
338100020110
338200020110      * decodifico a mio modo il tipo pagamento
338300020110     c                   clear                   wrkdestpg
338400020110
338500060704      * in lingua
338600060704     c                   select
338700060728      * italiano
338800060704     c                   when      i45lin = 'I'
338900020110     c                   if        tpttpg = 'T'
339000020110     c                   eval      wrkdestpg = 'SUL TRASPORTO'
339100020110     c                   endif
339200020110
339300020110     c                   if        tpttpg = 'V'
339400020110     c                   eval      wrkdestpg = 'SUL VALORE   '
339500020110     c                   endif
339600020110
339700020110     c                   if        tpttpg = '0'
339800020110     c                   eval      wrkdestpg = 'A Q.LE       '
339900020110     c                   endif
340000020110
340100020110     c                   if        tpttpg = '1'
340200020110     c                   eval      wrkdestpg = 'A COLLO      '
340300020110     c                   endif
340400020110
340500020110     c                   if        tpttpg = '2'
340600020110     c                   eval      wrkdestpg = 'A SPEDIZIONE '
340700020110     c                   endif
340800020110
340900020110     c                   if        tpttpg = '3'
341000020110     c                   eval      wrkdestpg = 'A KG.        '
341100020110     c                   endif
341200020110
341300020110     c                   if        tpttpg = '4'
341400020110     c                   eval      wrkdestpg = 'A VALORE     '
341500020110     c                   endif
341600020110
341700020110     c                   if        tpttpg = '5'
341800020110     c                   eval      wrkdestpg = 'A QUANTITA'' '
341900020110     c                   endif
342000021210      * Tar.particolare Diritto Fatturazione
342100030207     c                   If        TptFtc = '§'
342200021210     c                   Eval      WrkDesTpg = 'A FATTURA   '
342300021210     c                   EndIf
342400021210      * Tar.particolare Gest.bancali a rendere
342500021210     c                   If        TptFtc = '='
342600021210     c                   Eval      WrkDesTpg = 'A BANCALE   '
342700021210     c                   EndIf
342800151118      * Tar.particolare Diritto di Chiamata prenotazione ritiro telefonico
342900151118     c                   If        TptFtc = 't'
343000151118     c                   Eval      WrkDesTpg = 'A RITIRO    '
343100151118     c                   EndIf
343200060704
343300060728      * inglese
343400060704     c                   when      i45lin = '2'
343500060704     c                   if        tpttpg = 'T'
343600060704     c                   eval      wrkdestpg = 'ON TRANSPORT '
343700060704     c                   endif
343800060704
343900060704     c                   if        tpttpg = 'V'
344000060704     c                   eval      wrkdestpg = 'ON VALUE     '
344100060704     c                   endif
344200060704
344300060704     c                   if        tpttpg = '0'
344400060704     c                   eval      wrkdestpg = 'PER 100 KG.  '
344500060704     c                   endif
344600060704
344700060704     c                   if        tpttpg = '1'
344800060704     c                   eval      wrkdestpg = 'PER PARCEL   '
344900060704     c                   endif
345000060704
345100060704     c                   if        tpttpg = '2'
345200060704     c                   eval      wrkdestpg = 'PER SHIPMENT '
345300060704     c                   endif
345400060704
345500060704     c                   if        tpttpg = '3'
345600060704     c                   eval      wrkdestpg = 'PER KG.      '
345700060704     c                   endif
345800060704
345900060704     c                   if        tpttpg = '4'
346000060704     c                   eval      wrkdestpg = 'BY VALUE     '
346100060704     c                   endif
346200060704
346300060704     c                   if        tpttpg = '5'
346400060704     c                   eval      wrkdestpg = 'BY QUANTITY  '
346500060704     c                   endif
346600060704      * Tar.particolare Diritto Fatturazione
346700060704     c                   If        TptFtc = '§'
346800060704     c                   Eval      WrkDesTpg = 'PER INVOICE '
346900060704     c                   EndIf
347000060704      * Tar.particolare Gest.bancali a rendere
347100060704     c                   If        TptFtc = '='
347200060704     c                   Eval      WrkDesTpg = 'PER PALLET  '
347300060704     c                   EndIf
347400151118      * Tar.particolare Diritto di Chiamata prenotazione ritiro telefonico
347500151118     c                   If        TptFtc = 't'
347600151118     c                   Eval      WrkDesTpg = 'PER PICK-UP '
347700151118     c                   EndIf
347800060728
347900060728      * francese
348000060728     c                   when      i45lin = '3'
348100060728     c                   if        tpttpg = 'T'
348200060925     c                   eval      wrkdestpg = 'SUR LE TRANSP'
348300060728     c                   endif
348400060728
348500060728     c                   if        tpttpg = 'V'
348600060728     c                   eval      wrkdestpg = 'SUR LA VALEUR'
348700060728     c                   endif
348800060728
348900060728     c                   if        tpttpg = '0'
349000060728     c                   eval      wrkdestpg = 'AUX 100 KG   '
349100060728     c                   endif
349200060728
349300060728     c                   if        tpttpg = '1'
349400060728     c                   eval      wrkdestpg = 'AU COLIS     '
349500060728     c                   endif
349600060728
349700060728     c                   if        tpttpg = '2'
349800060728     c                   eval      wrkdestpg = 'PAR ENVOI    '
349900060728     c                   endif
350000060728
350100060728     c                   if        tpttpg = '3'
350200060728     c                   eval      wrkdestpg = 'AU KG        '
350300060728     c                   endif
350400060728
350500060728     c                   if        tpttpg = '4'
350600060728     c                   eval      wrkdestpg = 'A LA VALEUR  '
350700060728     c                   endif
350800060728
350900060728     c                   if        tpttpg = '5'
351000060728     c                   eval      wrkdestpg = 'A LA QUANTITÉ'
351100060728     c                   endif
351200060728      * Tar.particolare Diritto Fatturazione
351300060728     c                   If        TptFtc = '§'
351400060728     c                   Eval      WrkDesTpg = 'A FATTURA   '
351500060728     c                   EndIf
351600060728      * Tar.particolare Gest.bancali a rendere
351700060728     c                   If        TptFtc = '='
351800060925     c                   Eval      WrkDesTpg = 'A LA PALETTE'
351900060728     c                   EndIf
352000151118      * Tar.particolare Diritto di Chiamata prenotazione ritiro telefonico
352100151118     c                   If        TptFtc = 't'
352200151118     c                   Eval      WrkDesTpg = 'PAR RETRAIT '
352300151118     c                   EndIf
352400060712
352500060728      * tedesco
352600060712     c                   when      i45lin = '4'
352700060712     c                   if        tpttpg = 'T'
352800060728     c                   eval      wrkdestpg = 'AUF TRANSPORT'
352900060712     c                   endif
353000060712
353100060712     c                   if        tpttpg = 'V'
353200060728     c                   eval      wrkdestpg = 'AUF WERT     '
353300060712     c                   endif
353400060712
353500060712     c                   if        tpttpg = '0'
353600060728     c                   eval      wrkdestpg = 'N. ZENTNER   '
353700060712     c                   endif
353800060712
353900060712     c                   if        tpttpg = '1'
354000060728     c                   eval      wrkdestpg = 'N. FRACHTSTÜCK'
354100060712     c                   endif
354200060712
354300060712     c                   if        tpttpg = '2'
354400060728     c                   eval      wrkdestpg = 'N. SENDUNG   '
354500060712     c                   endif
354600060712
354700060712     c                   if        tpttpg = '3'
354800060728     c                   eval      wrkdestpg = 'N. KG        '
354900060712     c                   endif
355000060712
355100060712     c                   if        tpttpg = '4'
355200060728     c                   eval      wrkdestpg = 'N. WERT      '
355300060712     c                   endif
355400060712
355500060712     c                   if        tpttpg = '5'
355600060728     c                   eval      wrkdestpg = 'N. MENGE     '
355700060712     c                   endif
355800060712      * Tar.particolare Diritto Fatturazione
355900060712     c                   If        TptFtc = '§'
356000060712     c                   Eval      WrkDesTpg = 'A FATTURA   '
356100060712     c                   EndIf
356200060712      * Tar.particolare Gest.bancali a rendere
356300060712     c                   If        TptFtc = '='
356400060925     c                   Eval      WrkDesTpg = 'N.PALETTE   '
356500060712     c                   EndIf
356600151118      * Tar.particolare Diritto di Chiamata prenotazione ritiro telefonico
356700151118     c                   If        TptFtc = 't'
356800151118     c                   Eval      WrkDesTpg = 'N. BUCHUNG  '
356900151118     c                   EndIf
357000060704     c                   endsl
357100020110
357200020110      * CERCO IL RECORD VALIDO
357300061011     c  n98ktpt          setll     titpd01l
357400061011     c   98ktpt          setll     tiopd01l
357500020110
357600020110     c                   do        *hival
357700061011     c  n98ktpt          reade     titpd01l
357800061011     c   98ktpt          reade     tiopd01l
357900020110
358000061011     c                   if        %eof
358100020110     c                   leave
358200020110     c                   endif
358300020110
358400020110     c                   if        tpdatb = 'A'
358500020110     c                   iter
358600020110     c                   endif
358700020110
358800020110      * se tariffa non valorizzata leggo la successiva
358900020110      * importo e minimo =  a zero
359000020110     c                   if        tpditr = 0 and tpdmin = 0
359100020110     c                   iter
359200020110     c                   else
359300020110      * se importo con percentuale verifico quanti decimali ha
359400020110     c                   if        W_per = '%'
359500020110     c                   z-add     tpditr        valper
359600020110     c                   eval      stpper = %EDITC(valper : '3')
359700020110      * VERIFICO SE HA DECIMALI VALORIZZATI
359800020110     c                   move      valper        w0030
359900020110     c                   move      w0030         w0020
360000020110     c                   move      w0020         w0010
360100020110      * tolgo gli zeri non significativi dopo la virgola
360200020110     c                   select
360300020110
360400020110     c                   when      w0030 = *zeros
360500020110     c                   move      '    '        stpper
360600020110
360700020110     c                   when      w0020 = *zeros
360800020110     c                   move      '  '          stpper
360900020110
361000020110     c                   when      w0010 = *zero
361100020110     c                   move      ' '           stpper
361200020110
361300020110     c                   endsl
361400020110
361500020110     c                   endif
361600020110      * se sto stampando l'elevazione limite risarcibile mi devo comportare
361700020111      * diversamente se tariffa a valore e non è definito OLTRE e c'e
361800020110      * elevazione al limite
361900020110     c                   if        w_per = '%' and savftc = 'R ' and oltre = 0
362000020110     c                             and *in22
362100020110      * non definiscp W_tarpar in quanto gestisco in modo diverso
362200020110      * valorizzo il campo W_ellimris
362300020110     c                   eval      w_ellimris = 'Y'
362400020110
362500020110      * se esiste minimo e massimo devo valorizzare l'estensione
362600020110
362700020110      * solo minimo
362800020110     c                   if        tpdmin >  0 and tpdmax = 0
362900060704      * in lingua
363000060704     c                   select
363100060704     c                   when      i45lin = 'I'
363200021205     c                   eval      w_tarparest = blk56
363300020110     C                             + ' minimo........' +
363400020110     c                             %editc(tpdmin:'3')
363500060704     c                   when      i45lin = '2'
363600060704     c                   eval      w_tarparest = blk56
363700060704     c                             + ' minimum.......' +
363800060704     c                             %editc(tpdmin:'3')
363900060728     c                   when      i45lin = '3'
364000060728     c                   eval      w_tarparest = blk56
364100060728     c                             + ' minimum.......' +
364200060728     c                             %editc(tpdmin:'3')
364300060712     c                   when      i45lin = '4'
364400060712     c                   eval      w_tarparest = blk56
364500060728     c                             + ' minde.........' +
364600060712     c                             %editc(tpdmin:'3')
364700060704     c                   endsl
364800020110     c                   endif
364900020110
365000020110      * solo massimo
365100020110     c                   if        tpdmin =  0 and tpdmax > 0
365200060704     c                   select
365300060704     c                   when      i45lin = 'I'
365400021205     c                   eval      w_tarparest = blk56
365500020110     c                             + ' massimo.......' +
365600020110     c                             %editc(tpdmax:'3')
365700060704     c                   when      i45lin = '2'
365800060704     c                   eval      w_tarparest = blk56
365900060704     c                             + ' maximum.......' +
366000060704     c                             %editc(tpdmax:'3')
366100060728     c                   when      i45lin = '3'
366200060728     c                   eval      w_tarparest = blk56
366300060728     c                             + ' maximum.......' +
366400060728     c                             %editc(tpdmax:'3')
366500060712     c                   when      i45lin = '4'
366600060712     c                   eval      w_tarparest = blk56
366700060728     c                             + ' höchste.......' +
366800060712     c                             %editc(tpdmax:'3')
366900060704     c                   endsl
367000020110     c                   endif
367100020110
367200020110      * sia mino che massimo
367300020110     c                   if        tpdmin >  0 and tpdmax > 0
367400060704      * in lingua
367500060704     c                   select
367600060704     c                   when      i45lin = 'I'
367700030318     c                   eval      w_tarparest = blk10 + blk10 + blk12
367800060908     c                             + '...... minimo...' +
367900020110     c                             %editc(tpdmin:'3')
368000020110     c                             + ' massimo..' +
368100020110     c                             %editc(tpdmax:'3')
368200060704     c                   when      i45lin = '2'
368300060704     c                   eval      w_tarparest = blk10 + blk10 + blk12
368400060908     c                             + '...... minimum..' +
368500060704     c                             %editc(tpdmin:'3')
368600060704     c                             + ' maximum..' +
368700060704     c                             %editc(tpdmax:'3')
368800060728     c                   when      i45lin = '3'
368900060728     c                   eval      w_tarparest = blk10 + blk10 + blk12
369000060908     c                             + '...... minimum..' +
369100060728     c                             %editc(tpdmin:'3')
369200060728     c                             + ' maximum..' +
369300060728     c                             %editc(tpdmax:'3')
369400060712     c                   when      i45lin = '4'
369500060712     c                   eval      w_tarparest = blk10 + blk10 + blk12
369600060908     c                             + '...... minde....' +
369700060712     c                             %editc(tpdmin:'3')
369800060728     c                             + ' höchste..' +
369900060712     c                             %editc(tpdmax:'3')
370000060704     c                   endsl
370100020110     c                   endif
370200020110
370300020110     c                   leave
370400020110     c                   endif
370500020110
370600020110      * verifico se devo stampare OLTRE
370700020110     c                   if        oltre > 0
370800020110      * controllo se tariffa a q.le
370900020110     c                   if        tpttpg = '0'
371000020110     c                   eval      unmis = 'KG.'
371100020110     c                   else
371200020110     c                   clear                   unmis
371300020110     c                   endif
371400020110
371500020110      * stampo nella prima riga il valore e nella riga di estensione se ci sono
371600020110      * stampo il minimo e massimo
371700020110
371800020110      * no percentuale
371900020110     c                   if        w_per = ' '
372000060704      * in lingua
372100060704     c                   select
372200060704     c                   when      i45lin = 'I'
372300020110     c                   eval      w_tarpar = ': ' + wrkdestpg +
372400020110     c                             'oltre ' + (%editc(oltre:'3'))+
372500030318     c                             ' ' + unmis + blk10 + blk10 + blk12
372600021205     c                                                 + %editc(tpditr:'3')
372700060704     c                   when      i45lin = '2'
372800060704     c                   eval      w_tarpar = ': ' + wrkdestpg +
372900060704     c                             'over  ' + (%editc(oltre:'3'))+
373000060704     c                             ' ' + unmis + blk10 + blk10 + blk12
373100060704     c                                                 + %editc(tpditr:'3')
373200060801     c                   when      i45lin = '3'
373300060801     c                   eval      w_tarpar = ': ' + wrkdestpg +
373400060801     c                             'plus  ' + (%editc(oltre:'3'))+
373500060801     c                             ' ' + unmis + blk10 + blk10 + blk12
373600060801     c                                                 + %editc(tpditr:'3')
373700060712     c                   when      i45lin = '4'
373800060712     c                   eval      w_tarpar = ': ' + wrkdestpg +
373900060728     c                             'mehrals ' + (%editc(oltre:'3'))+
374000060728     c                             ' ' + unmis + blk10 + blk10 + blk10
374100060712     c                                                 + %editc(tpditr:'3')
374200060704     c                   endsl
374300020110     c                   else
374400020110      * si percentuale
374500060704      * in lingua
374600060704     c                   select
374700060704     c                   when      i45lin = 'I'
374800020110     c                   eval      w_tarpar = ': ' +
374900020110     c                             wrkdestpg + ' ' + stpper + ' % ' +
375000020110     c                             'oltre ' + (%editc(oltre:'3'))+
375100030318     c                             ' ' + unmis + blk21
375200060704     c                   when      i45lin = '2'
375300060704     c                   eval      w_tarpar = ': ' +
375400060704     c                             wrkdestpg + ' ' + stpper + ' % ' +
375500060704     c                             'over  ' + (%editc(oltre:'3'))+
375600060704     c                             ' ' + unmis + blk21
375700060801     c                   when      i45lin = '3'
375800060801     c                   eval      w_tarpar = ': ' +
375900060801     c                             wrkdestpg + ' ' + stpper + ' % ' +
376000060801     c                             'plus  ' + (%editc(oltre:'3'))+
376100060801     c                             ' ' + unmis + blk21
376200060712     c                   when      i45lin = '4'
376300060712     c                   eval      w_tarpar = ': ' +
376400060712     c                             wrkdestpg + ' ' + stpper + ' % ' +
376500060728     c                             'mehrals ' + (%editc(oltre:'3'))+
376600060712     c                             ' ' + unmis + blk21
376700060704     c                   endsl
376800020110     c                   endif
376900020110
377000020110      * se esiste minimo e massimo devo valorizzare anche l'estensione
377100020110      * tariffe particolari con l'oltre
377200020110
377300020110      * solo minimo
377400020110     c                   if        tpdmin >  0 and tpdmax = 0
377500060704      * in lingua
377600060704     c                   select
377700060704     c                   when      i45lin = 'I'
377800021205     c                   eval      w_tarparest = blk56
377900020110     c                             + ' minimo........' +
378000020110     c                             %editc(tpdmin:'3')
378100060704     c                   when      i45lin = '2'
378200060704     c                   eval      w_tarparest = blk56
378300060704     c                             + ' minimum.......' +
378400060704     c                             %editc(tpdmin:'3')
378500060728     c                   when      i45lin = '3'
378600060728     c                   eval      w_tarparest = blk56
378700060728     c                             + ' minimum.......' +
378800060728     c                             %editc(tpdmin:'3')
378900060712     c                   when      i45lin = '4'
379000060712     c                   eval      w_tarparest = blk56
379100060728     c                             + ' minde.........' +
379200060712     c                             %editc(tpdmin:'3')
379300060704     c                   endsl
379400020110     c                   endif
379500020110
379600020110      * solo massimo
379700020110     c                   if        tpdmin =  0 and tpdmax > 0
379800060704      * in lingua
379900060704     c                   select
380000060704     c                   when      i45lin = 'I'
380100021205     c                   eval      w_tarparest = blk56
380200020110     c                             + ' massimo.......' +
380300020110     c                             %editc(tpdmax:'3')
380400060704     c                   when      i45lin = '2'
380500060704     c                   eval      w_tarparest = blk56
380600060704     c                             + ' maximum.......' +
380700060704     c                             %editc(tpdmax:'3')
380800060801     c                   when      i45lin = '3'
380900060728     c                   eval      w_tarparest = blk56
381000060728     c                             + ' maximum.......' +
381100060728     c                             %editc(tpdmax:'3')
381200060712     c                   when      i45lin = '4'
381300060712     c                   eval      w_tarparest = blk56
381400060728     c                             + ' höchste.......' +
381500060712     c                             %editc(tpdmax:'3')
381600060704     c                   endsl
381700020110     c                   endif
381800020110
381900020110      * sia mino che massimo
382000020110     c                   if        tpdmin >  0 and tpdmax > 0
382100060704      * in lingua
382200060704     c                   select
382300060704     c                   when      i45lin = 'I'
382400030318     c                   eval      w_tarparest = blk10 + blk10 + blk12
382500060908     c                             + '...... minimo...' +
382600020110     c                             %editc(tpdmin:'3')
382700020110     c                             + ' massimo..' +
382800020110     c                             %editc(tpdmax:'3')
382900060704     c                   when      i45lin = '2'
383000060704     c                   eval      w_tarparest = blk10 + blk10 + blk12
383100060908     C                             + '...... minimum..' +
383200060704     c                             %editc(tpdmin:'3')
383300060704     C                             + ' maximum..' +
383400060704     c                             %editc(tpdmax:'3')
383500060728     c                   when      i45lin = '3'
383600060728     c                   eval      w_tarparest = blk10 + blk10 + blk12
383700060908     C                             + '...... minimum..' +
383800060728     c                             %editc(tpdmin:'3')
383900060728     C                             + ' maximum..' +
384000060728     c                             %editc(tpdmax:'3')
384100060712     c                   when      i45lin = '4'
384200060712     c                   eval      w_tarparest = blk10 + blk10 + blk12
384300060908     c                             + '...... minde....' +
384400060712     c                             %editc(tpdmin:'3')
384500060728     c                             + ' höchste..' +
384600060712     c                             %editc(tpdmax:'3')
384700060704     c                   endsl
384800020110     c                   endif
384900020110
385000020110     c                   else
385100020110
385200020110     c
385300020110      * valorizzo il campo che conterrà la percetuale
385400020110      * se non c'è ne minimo ne massimo
385500020110     c                   if        tpdmin = 0 and tpdmax =  0
385600020110      * no percentuale
385700020110     c                   if        w_per = ' '
385800020110     c                   eval      w_tarpar = ': ' + wrkdestpg +
385900021205     c                             blk56 + %editc(tpditr:'3')
386000020110     c                   else
386100020110      * si percentuale
386200020110     c                   eval      w_tarpar = ': ' +
386300030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
386400030318     c                             blk10 + blk10 + blk25
386500020110     c                   endif
386600020110
386700020110     c                   endif
386800020110      * se c'è minimo
386900020110     c                   if        tpdmin > 0 and tpdmax = 0
387000020110      * no percentuale
387100020110     c                   if        w_per = ' '
387200060704      * in lingua
387300060704     c                   select
387400060704     c                   when      i45lin = 'I'
387500020110     c                   eval      w_tarpar = ': ' + wrkdestpg +
387600030318     c                             blk10 + blk10 + blk12
387700060313     c                             + ' ' + %editc(tpditr:'3')
387800020110     c                             + ' minimo...' +
387900020110     c                             %editc(tpdmin:'3')
388000060704     c                   when      i45lin = '2'
388100060704     c                   eval      w_tarpar = ': ' + wrkdestpg +
388200060704     c                             blk10 + blk10 + blk12
388300060704     c                             + ' ' + %editc(tpditr:'3')
388400060704     C                             + ' minimum..' +
388500060704     c                             %editc(tpdmin:'3')
388600060728     c                   when      i45lin = '3'
388700060728     c                   eval      w_tarpar = ': ' + wrkdestpg +
388800060728     c                             blk10 + blk10 + blk12
388900060728     c                             + ' ' + %editc(tpditr:'3')
389000060728     C                             + ' minimum..' +
389100060728     c                             %editc(tpdmin:'3')
389200060712     c                   when      i45lin = '4'
389300060712     c                   eval      w_tarpar = ': ' + wrkdestpg +
389400060712     c                             blk10 + blk10 + blk12
389500060712     c                             + ' ' + %editc(tpditr:'3')
389600060728     c                             + ' minde....' +
389700060712     c                             %editc(tpdmin:'3')
389800060704     c                   endsl
389900020110     c                   else
390000020110      * si percentuale
390100060704      * in lingua
390200060704     c                   select
390300060704     c                   when      i45lin = 'I'
390400020110     c                   eval      w_tarpar = ': ' +
390500030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
390600030318     c                             blk10 + blk25
390700020110     c                             + ' minimo...' +
390800020110     c                             %editc(tpdmin:'3')
390900060704     c                   when      i45lin = '2'
391000060704     c                   eval      w_tarpar = ': ' +
391100060704     c                             wrkdestpg + ' ' + stpper + ' % ' +
391200060704     c                             blk10 + blk25
391300060712     c                             + ' minimum..' +
391400060704     c                             %editc(tpdmin:'3')
391500060728     c                   when      i45lin = '3'
391600060728     c                   eval      w_tarpar = ': ' +
391700060728     c                             wrkdestpg + ' ' + stpper + ' % ' +
391800060728     c                             blk10 + blk25
391900060728     c                             + ' minimum..' +
392000060728     c                             %editc(tpdmin:'3')
392100060712     c                   when      i45lin = '4'
392200060712     c                   eval      w_tarpar = ': ' +
392300060712     c                             wrkdestpg + ' ' + stpper + ' % ' +
392400060712     c                             blk10 + blk25
392500060728     c                             + ' minde....' +
392600060712     c                             %editc(tpdmin:'3')
392700060704     c                   endsl
392800020110     c                   endif
392900020110
393000020110     c                   endif
393100020110      * se c'è solo massimo
393200020110     c                   if        tpdmin = 0 and tpdmax > 0
393300020110      * no percentuale
393400020110     c                   if        w_per = ' '
393500060704      * in lingua
393600060704     c                   select
393700060704     c                   when      i45lin = 'I'
393800020110     c                   eval      w_tarpar = ': ' + wrkdestpg +
393900030318     c                             blk10 + blk10 + blk12
394000060313     c                             + ' ' + %editc(tpditr:'3')
394100020110     c                             + ' massimo..' +
394200020110     c                             %editc(tpdmax:'3')
394300060704     c                   when      i45lin = '2'
394400060704     c                   eval      w_tarpar = ': ' + wrkdestpg +
394500060704     c                             blk10 + blk10 + blk12
394600060704     c                             + ' ' + %editc(tpditr:'3')
394700060712     c                             + ' maximum..' +
394800060704     c                             %editc(tpdmax:'3')
394900060728     c                   when      i45lin = '3'
395000060728     c                   eval      w_tarpar = ': ' + wrkdestpg +
395100060728     c                             blk10 + blk10 + blk12
395200060728     c                             + ' ' + %editc(tpditr:'3')
395300060728     c                             + ' maximum..' +
395400060728     c                             %editc(tpdmax:'3')
395500060712     c                   when      i45lin = '4'
395600060712     c                   eval      w_tarpar = ': ' + wrkdestpg +
395700060712     c                             blk10 + blk10 + blk12
395800060712     c                             + ' ' + %editc(tpditr:'3')
395900060728     c                             + ' höchste..' +
396000060712     c                             %editc(tpdmax:'3')
396100060704     c                   endsl
396200020110     c                   else
396300020110      * si percentuale
396400060704      * in lingua
396500060704     c                   select
396600060704     c                   when      i45lin = 'I'
396700020110     c                   eval      w_tarpar = ': ' +
396800030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
396900030318     c                             blk10 + blk25
397000020110     c                             + ' massimo..' +
397100020110     c                             %editc(tpdmax:'3')
397200060704     c                   when      i45lin = '2'
397300060704     c                   eval      w_tarpar = ': ' +
397400060704     c                             wrkdestpg + ' ' + stpper + ' % ' +
397500060704     c                             blk10 + blk25
397600060712     c                             + ' maximum..' +
397700060704     c                             %editc(tpdmax:'3')
397800060728     c                   when      i45lin = '3'
397900060728     c                   eval      w_tarpar = ': ' +
398000060728     c                             wrkdestpg + ' ' + stpper + ' % ' +
398100060728     c                             blk10 + blk25
398200060728     c                             + ' maximum..' +
398300060728     c                             %editc(tpdmax:'3')
398400060712     c                   when      i45lin = '4'
398500060712     c                   eval      w_tarpar = ': ' +
398600060712     c                             wrkdestpg + ' ' + stpper + ' % ' +
398700060712     c                             blk10 + blk25
398800060728     c                             + ' höchste..' +
398900060712     c                             %editc(tpdmax:'3')
399000060704     c                   endsl
399100020110     c                   endif
399200020110
399300020110     c                   endif
399400020110      * se c'è minimo e massimo
399500020110     c                   if        tpdmin > 0 and tpdmax > 0
399600020110      * no percentuale
399700020110     c                   if        w_per = ' '
399800060704      * in lingua
399900060704     c                   select
400000060704     c                   when      i45lin = 'I'
400100020110     c                   eval      w_tarpar = ': ' + wrkdestpg +
400200021205     c                             ' ' + %editc(tpditr:'3') + blk10
400300060908     c                             + ' minimo....' +
400400020110     c                             %editc(tpdmin:'3')
400500020110     c                             + ' massimo..' +
400600020110     c                             %editc(tpdmax:'3')
400700060704     c                   when      i45lin = '2'
400800060704     c                   eval      w_tarpar = ': ' + wrkdestpg +
400900060704     c                             ' ' + %editc(tpditr:'3') + blk10
401000060908     c                             + ' minimum...' +
401100060704     c                             %editc(tpdmin:'3')
401200060704     c                             + ' maximum..' +
401300060704     c                             %editc(tpdmax:'3')
401400060728     c                   when      i45lin = '3'
401500060728     c                   eval      w_tarpar = ': ' + wrkdestpg +
401600060728     c                             ' ' + %editc(tpditr:'3') + blk10
401700060908     c                             + ' minimum...' +
401800060728     c                             %editc(tpdmin:'3')
401900060728     c                             + ' maximum..' +
402000060728     c                             %editc(tpdmax:'3')
402100060712     c                   when      i45lin = '4'
402200060712     c                   eval      w_tarpar = ': ' + wrkdestpg +
402300060712     c                             ' ' + %editc(tpditr:'3') + blk10
402400060908     c                             + ' minde.....' +
402500060712     c                             %editc(tpdmin:'3')
402600060728     c                             + ' höchste..' +
402700060712     c                             %editc(tpdmax:'3')
402800060704     c                   endsl
402900020110     c                   else
403000020110      * si percentuale
403100060704      * in lingua
403200060704     c                   select
403300060704     c                   when      i45lin = 'I'
403400020110     c                   eval      w_tarpar = ': ' +
403500030318     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
403600060908     c                             + ' minimo..' +
403700020110     c                             %editc(tpdmin:'3')
403800020110     c                             + ' massimo..' +
403900020110     c                             %editc(tpdmax:'3')
404000060704     c                   when      i45lin = '2'
404100060704     c                   eval      w_tarpar = ': ' +
404200060704     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
404300060908     c                             + ' minimum..' +
404400060704     c                             %editc(tpdmin:'3')
404500060712     c                             + ' maximum..' +
404600060704     c                             %editc(tpdmax:'3')
404700060728     c                   when      i45lin = '3'
404800060728     c                   eval      w_tarpar = ': ' +
404900060728     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
405000060908     c                             + ' minimum..' +
405100060728     c                             %editc(tpdmin:'3')
405200060728     c                             + ' maximum..' +
405300060728     c                             %editc(tpdmax:'3')
405400060712     c                   when      i45lin = '4'
405500060712     c                   eval      w_tarpar = ': ' +
405600060712     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
405700060908     c                             + ' minde....' +
405800060712     c                             %editc(tpdmin:'3')
405900060728     c                             + ' höchste..' +
406000060712     c                             %editc(tpdmax:'3')
406100060704     c                   endsl
406200020110     c                   endif
406300020110
406400020110     c                   endif
406500020110
406600020110     c                   endif
406700020110     c                   leave
406800020110     c                   endif
406900020110
407000020110     c                   enddo
407100020110
407200020110     c                   endsr
407300020110
407400020110      *------------------------------------------------------------------*
407500020110      * Ricerc  CONTROLLO SE ESISTE UN RECORD VALIDO E CON TARIFFA+MINIMO > 0
407600020110      *------------------------------------------------------------------*
407700020110
407800020110     c     Ricerc        begsr
407900020110
408000020110     c                   setoff                                       04
408100020110     c                   z-add     *zeros        recval
408200020110     c                   clear                   wrkftc
408300020110     c                   clear                   wrkorp
408400020110     c                   clear                   wrksgl
408500020110     c                   clear                   recsgl
408600020110     c                   clear                   oltre
408700020110     c                   clear                   mintas
408800020110     c                   clear                   savorp1
408900020110     c                   clear                   w_tarparest
409000020110
409100061011     c  n98ktpt          setll     titpd01l
409200061011     c   98ktpt          setll     tiopd01l
409300020110
409400020110     c                   do        *hival
409500061011     c  n98ktpt          reade     titpd01l
409600061011     c   98ktpt          reade     tiopd01l
409700020110
409800061011     c                   if        %eof
409900020110      * se non ho il recval valorizzato accendo lo 04
410000020110     c                   if        recval = 0
410100020110     c                   seton                                        04
410200020110     c                   endif
410300020110
410400020110     c                   leave
410500020110     c                   endif
410600020110
410700020110     c                   if        tpdatb = 'A'
410800020110     c                   iter
410900020110     c                   endif
411000020110
411100020110      * se tariffa non valorizzata leggo la successiva
411200020110      * importo e minimo =  a zero
411300020110      * se valorizzata esco per gestire la stampa del dettaglio tariffario
411400020110
411500020110     c                   if        tpditr = 0 and tpdmin = 0
411600020110      * mi salvo il valore dello scaglione precedente per stampare in un even-
411700020110      * tuale oltre nella riga della tariffa particolare se diverso da 99999
411800020110     c                   z-add     tpdsgl        w0050
411900020110     c                   if        w0050 <> 99999
412000020110
412100020110     c                   z-add     tpdsgl        oltre
412200020110      * mi salvo il primo scaglione per stampare il minimo tassabile in caso di
412300020110      * tariffa particolare a qle
412400020110     c                   if        mintas = 0
412500020110     c                   z-add     tpdsgl        mintas
412600020110     c                   endif
412700020110
412800020110     c                   endif
412900020110      * mi salvo ordinamento regione e provincia la prima volta
413000020110     c                   if        savorp1 = *blanks
413100020110     c                   eval      savorp1 = tpdorp
413200020110      * memorizzo la chiave che mi serve per riaggancare il primo
413300020110      * record valido
413400020110     c                   eval      wrkorp = tpdorp
413500020110     c                   eval      wrkftc = tpdftc
413600020110     c                   eval      wrksgl = tpdsgl
413700020110
413800020110     c                   endif
413900020110
414000020110     c                   iter
414100020110     c                   else
414200020110     c                   add       1             recval
414300020111      * memorizzo lo scaglioni perchè nel caso di unico scaglione valido
414400020110      * stampo unico scaglione solo se scaglione = a 99999
414500020110     c                   eval      recsgl = tpdsgl
414600020110      * memorizzo la chiave che mi serve per riaggancare il primo
414700020110      * record valido se è = a blanks
414800020110     c                   if        recval = 1 and wrkftc = '  '
414900020110     c                   eval      wrkorp = tpdorp
415000020110     c                   eval      wrkftc = tpdftc
415100020110     c                   eval      wrksgl = tpdsgl
415200020110     c                   endif
415300020111
415400020110      * se trovati + di 1 record non proseguo in lettura
415500020110     c                   if        recval > 1
415600020110      * mi riposiziono sul prima record valido trovato
415700061011     c  n98ktpdwrk       chain     titpd01l
415800061011     c   98ktpdwrk       chain     tiopd01l
415900020110
416000020110     c                   leave
416100020110     c                   else
416200020110     c                   iter
416300020110     c                   endif
416400020110
416500020110     c                   endif
416600020110
416700020110    2c                   enddo
416800020110      * se al primo record valido verifico che non ha lo stesso ordinamento
416900020110      * regione e provincia del record a zero forzo per la stampa in dettaglio
417000020110     c                   if        recval = 1 and tpdorp <> savorp1 and
417100020110     c                             savorp1 <> *blanks
417200020110     c                   eval      recval = 9
417300020110      * mi riposiziono sul prima record valido trovato
417400061011     c  n98ktpdwrk       chain     titpd01l
417500061011     c   98ktpdwrk       chain     tiopd01l
417600020110     c                   clear                   savorp1
417700020110     c                   endif
417800020110      * Se ho trovato un solo record valido verifico lo scaglione trovato
417900020110     c                   if        recval = 1
418000020110     c                   z-add     recsgl        w0050
418100020110     c                   if        w0050 <> 99999 and tpttpg <> '2'
418200020110      * se diverso da 99999 devo gestire il tutto come se avesse + scaglioni
418300020110      * e tipo tariffa non è a spedizione
418400020110     c                   eval      recval = 9
418500020110      * mi riposiziono sul prima record valido trovato
418600061011     c  n98ktpdwrk       chain     titpd01l
418700061011     c   98ktpdwrk       chain     tiopd01l
418800020110     c                   endif
418900020110     c                   endif
419000020110
419100020110     c                   endsr
419200020110
419300020110      *------------------------------------------------------------------*
419400020110      *  Dectpg           DECODIFICA TIPO TARIFFA E SCAGLIONI
419500020110      *------------------------------------------------------------------*
419600020110
419700020110     c     Dectpg        begsr
419800020110
419900020110     c                   setoff                                       09
420000020110
420100020110      * decodifico il tipo tariffa
420200020110     c                   movel(p)  tpttpg        key
420300060704     c     tabkeyl       chain     tabel00f
420400020110      *
420500020110     c                   if        %found(tabel00f)
420600020110     c                   movel     tbluni        dstr
420700020110     c                   else
420800020110     c                   clear                   dstr
420900020110     c                   endif
421000020110
421100020110     c                   movel(p)  §trde1        de1tpg
421200020110     c                   movel(p)  §trde3        de3tpg
421300140724      * minimo tassabile
421400140724     c                   z-add     §trata        ctrata
421500020110      * verifico se scaglioni a valore
421600020110     c                   eval      *in09 = (§trtap = 'S')
421700020110
421800020110      * SCAGLIONI
421900020110
422000020110      * memorizzo gli scaglioni del dettaglio tariffa particolare
422100020110
422200020110     c                   exsr      Memsca
422300020110
422400020110     c                   endsr
422500020110
422600020110      *------------------------------------------------------------------*
422700020110      *  Memsca           CARICO SCAGLIONI TARIFFE PARTICOLARI
422800020110      *------------------------------------------------------------------*
422900020110     c     Memsca        begsr
423000020110
423100020110     c                   setoff                                       089137
423200020110     c                   setoff                                       4041
423300020110
423400020110     c                   movea     *zeros        scp
423500020110     c                   movea     *zeros        ssl
423600020110     c                   clear                   sgld
423700020110     c                   clear                   sg1d
423800020110     c                   clear                   sg2d
423900020110     c                   clear                   sgla
424000020110     c                   clear                   sg1a
424100020110     c                   clear                   sg2a
424200020110
424300020110     c                   eval      rotcts = tpdcts
424400020110     c                   eval      rotpro = %subst(tpdorp:5:1)
424500020110     c                   eval      rotreg = %subst(tpdorp:3:2)
424600020110      * SALVO PER LETTURE FUTURE
424700020110     c                   eval      wrkcts = rotcts
424800020110     c                   eval      wrkpro = rotpro
424900020110     c                   eval      wrkreg = rotreg
425000020110
425100020110      * CERCO IL DETTAGLIO TARIFFARIO
425200061011     c  n98ktpt          setll     titpd01l
425300061011     c   98ktpt          setll     tiopd01l
425400020110
425500020110    1c                   do        *hival
425600061011     c  n98ktpt          reade     titpd01l
425700061011     c   98ktpt          reade     tiopd01l
425800020110
425900061011    2c                   if        %eof
426000020110     c                   seton                                        04
426100020110     c                   leave
426200020110    2c                   endif
426300020110
426400020110    2c                   if        tpdatb = 'A'
426500020110     c                   iter
426600020110    2c                   endif
426700020110
426800020110     c                   exsr      Regdet
426900020110     c   99              goto      fine
427000020110     c                   exsr      Prodet
427100020110     c   99              goto      fine
427200020110
427300020110      * carico il primo scaglione
427400020110     c                   z-add     tpdsgl        scp(1)
427500020110
427600020110      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
427700020110      * a valore
427800020110     c                   if        not *in09
427900020110
428000020110     c                   move      tpdsgl        w0030
428100020110     c                   z-add     tpdsgl        w0050
428200020110      * se scaglione 99999 non controllo i decimali
428300020110    2c                   if        w0050 <> 99999 and w0030 > 0
428400020110     c                   seton                                        91
428500020110    2c                   endif
428600020110      * se con decimali verifico se valorizzato dal secondo in poi
428700020110    2c                   if        *in91
428800020110     c                   z-add     w0030         w0020
428900020110    3c                   if        w0020 > 0
429000020110     c                   seton                                        37
429100020110    3c                   endif
429200020110    2c                   endif
429300020110
429400020110     c                   endif
429500050324
429600050324      * Se scaglione non è 99999
429700050324      * controllo che non sia maggiore di 9999 xchè non ci sta in stampa
429800050324      * in questo caso ERRORE ed esco dal programma
429900050324     c                   z-add     tpdsgl        w0050
430000050324     c                   If        w0050 <> 99999 and tpdsgl > 9999
430100050324     c                   move      msg(17)       errmsg
430200050324     c                   exsr      staerr
430300050324     c                   Goto      FINE
430400050324     c                   EndIf
430500020110
430600020110     c                   leave
430700020110
430800020110    1c                   enddo
430900020110
431000020110     c                   z-add     1             K
431100020110
431200020110      * LETTURA TPD CON KEY KTPT CON LO STESSO CODICE TASSAZIONE
431300020110    2c                   do        *hival
431400020111
431500061011     c  n98ktpt          reade     titpd01l
431600061011     c   98ktpt          reade     tiopd01l
431700020110      * se fine lettura
431800061011     c                   if        %eof
431900020110     c                   leave
432000020110     c                   endif
432100020110      * se codice tassazione diverso
432200020110     c                   if        tpdcts <> rotcts
432300020110     c                   leave
432400020110     c                   endif
432500020110      * se annullato leggo il successivo
432600020110     c                   if        tpdatb <> ' '
432700020110     c                   iter
432800020110     c                   endif
432900020110
433000020110     c                   add       1             k
433100020110      * fino a 18 scaglioni carico
433200020110    3c                   if        k <= 18
433300020111
433400020110     c                   z-add     tpdsgl        scp(k)
433500020110
433600020110      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
433700020110      * a valore
433800020110     c                   if        not *in09
433900020110
434000020110     c                   move      tpdsgl        w0030
434100020110     c                   z-add     tpdsgl        w0050
434200020110      * se scaglione 99999 non controllo i decimali
434300020110     c                   if        w0050 <> 99999 and w0030 > 0
434400020110     c                   seton                                        91
434500020110     c                   endif
434600020110      * se con decimali verifico se valorizzato dal secondo in poi
434700020110     c                   if        *in91
434800020110     c                   z-add     w0030         w0020
434900020110     c                   if        w0020 > 0
435000020110     c                   seton                                        37
435100020110     c                   endif
435200020110     c                   endif
435300020110
435400020110     c                   endif
435500050324
435600050324      * Se scaglione non è 99999
435700050324      * controllo che non sia maggiore di 9999 xchè non ci sta in stampa
435800050324      * in questo caso ERRORE ed esco dal programma
435900050324     c                   z-add     tpdsgl        w0050
436000050324     c                   If        w0050 <> 99999 and tpdsgl > 9999
436100050324     c                   move      msg(17)       errmsg
436200050324     c                   exsr      staerr
436300050324     c                   Goto      FINE
436400050324     c                   EndIf
436500020110
436600020110   x3c                   else
436700020110
436800020110     c                   move      msg(15)       errmsg
436900020110     c                   exsr      staerr
437000050914     c                   Goto      FINE
437100020110    3c                   endif
437200020110
437300020110    2c                   enddo
437400020110      * memorizzo il numero scaglioni caricati
437500020110     c                   z-add     k             wnumsc
437600020110
437700020110      * preparo la schiera degli scaglioni da stampare
437800020110
437900020110     c                   z-add     1             k
438000020110     c                   clear                   k1
438100020110
438200020110    2c                   dow       scp(k) > 0
438300020110      * preparo lo scaglione DAL addizionato di 1 o 0,1 o 0,01 in base ai decima
438400020110      * li
438500020110     c                   z-add     scp(k)        w0050
438600020110
438700020110      * verifico se scaglione 99999
438800020110    3c                   if        w0050 <> 99999
438900020110
439000020110    4c                   if        k1 > 0
439100020110      * se non ha decimali
439200020110     c                   if        not *in91
439300020110     c                   eval      sgld(k) = scp(k1) + 1
439400020110     c                   endif
439500020110
439600020110     c                   if        *in91 and not *in37
439700020110      * se ha 1 decimale
439800020110     c                   eval      sg1d(k) = scp(k1) + 0,1
439900020110     c                   endif
440000020110
440100020110     c                   if        *in91 and *in37
440200020110      * se ha + di 1 decimale
440300020110     c                   eval      sg2d(k) = scp(k1) + 0,01
440400020110     c                   endif
440500020110
440600020110     c                   else
440700020110    4c                   if        k =  1
440800020110      * se non ha decimali
440900020110     c                   if        not *in91
441000020110     c                   eval      sgld(k) =  1
441100020110     c                   endif
441200020110
441300020110     c                   if        *in91 and not *in37
441400020110      * se ha 1 decimale
441500020110     c                   eval      sg1d(k) =  0,1
441600020110     c                   endif
441700020110
441800020110     c                   if        *in91 and *in37
441900020110      * se ha + di 1 decimale
442000020110     c                   eval      sg2d(k) =  0,01
442100020110     c                   endif
442200020110
442300020110     c                   endif
442400020110
442500020110    4c                   endif
442600020110
442700020110      * costruisco la schiera dell'applicazione tariffa finita che va sotto gli
442800020110      * scaglioni da/a
442900020110      * tariffa e in mancanza di quest'ultima prendo dalla tabella TR
443000020110
443100060925      * richiamo la routine (dove ho tutto fisso e in un unico posto)
443200060925     c                   move      tpttpg        wctr1
443300151119      * Chiodo a programma per la nuova tariffa particolare t= Diritto di
443400151119      * chiamata ritiro telefonico l'unità di misura "a ritiro" altrimenti
443500151119      * prenderebbe a spedizione
443600151119     c                   if        tptftc = 't'
443700151119     c                   eval      wctr1= 'R'
443800151119     c                   endif
443900060925     c                   exsr      sr_atfk
444000140724
444100140724      * faccio i vari ragionamenti in base al valore della tariffa finita
444200140724      *
444300140724      * se scaglioni senza decimali
444400140724     c                   if        not *in91
444500140724
444600140724     c                   z-add     ctrata        wata
444700140724      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
444800140724      * scrivo "A SPED."
444900140724     c                   if        (wata > 0 and sgl(k) <= wata)
445000140724      * richiamo la routine (dove ho tutto fisso e in un unico posto)
445100140724     c                   clear                   wctr1
445200140724     c                   exsr      sr_atfk
445300140724     c                   endif
445400140724      *
445500140724     c                   else
445600140724      * scaglioni con decimali
445700140724     c                   z-add     ctrata        wata1
445800140724     c                   z-add     ctrata        wata2
445900140724      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
446000140724      * scrivo "A SPED."  1 decimale
446100140724     c                   if        (wata1 > 0 and sgl(k)<= wata1 and not *in37)
446200140724      * richiamo la routine (dove ho tutto fisso e in un unico posto)
446300140724     c                   clear                   wctr1
446400140724     c                   exsr      sr_atfk
446500140724     c                   endif
446600140724      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
446700140724      * scrivo "A SPED."  2 decimali
446800140724     c                   if        (wata2 > 0 and sgl(k) <= wata2 and *in37)
446900140724      * richiamo la routine (dove ho tutto fisso e in un unico posto)
447000140724     c                   clear                   wctr1
447100140724     c                   exsr      sr_atfk
447200140724     c                   endif
447300140724      *                                                      *in91 on
447400140724     c                   endif
447500020110
447600020110     c                   move      '/'           sglb(k)
447700020110      * preparo lo scaglione AL
447800020110
447900020110      * no decimali
448000020110    4c                   if        not *in91
448100020110     c                   z-add     scp(k)        sgla(k)
448200020110    4c                   endif
448300020110      * 1 decimale
448400020110    4c                   if        *in91 and not *in37
448500020110     c                   z-add     scp(k)        sg1a(k)
448600020110    4c                   endif
448700020110      * 2 decimali
448800020110    4c                   if        *in91 and *in37
448900020110     c                   z-add     scp(k)        sg2a(k)
449000020110    4c                   endif
449100020110
449200020110   x3c                   else
449300020110      * scaglione 99999 è sempre maggiore della tariffa finita
449400060925      * richiamo la routine (dove ho tutto fisso e in un unico posto)
449500060925     c                   move      tpttpg        wctr1
449600151119      * chiodo a programma per la nuova tariffa particolare t= Diritto di
449700151119      * chiamata ritiro telefonico l'unità di misura "a ritiro" altrimenti
449800151119      * prenderebbe a spedizione
449900151119     c                   if        tptftc = 't'
450000151119     c                   eval      wctr1= 'R'
450100151119     c                   endif
450200060925     c                   exsr      sr_atfk
450300020110
450400020110    3c                   endif
450500020110
450600020110     c                   add       1             k
450700020110     c                   add       1             k1
450800020110
450900020110      * se maggiore di 18 esco
451000020110    3c                   if        k > 18
451100020110     c                   leave
451200020110    3c                   end
451300020110
451400020110    2c                   enddo
451500020110
451600020110      * se c'è solo uno scaglione non scrivo niente sotto
451700021220      * solo se lo scaglione è 99999
451800020110
451900021220     c                   if        k1 <= 1 and scp(k1) >= 99999
452000020110     c                   clear                   atf(1)
452100020110     c                   endif
452200020110
452300020110     c                   clear                   savsgl
452400020110
452500020111     c                   endsr
452600020110
452700020110      *------------------------------------------------------------------*
452800020110      *  Prodet           DETTAGLIO INIZIALE X  P R O V I N C I A
452900020110      *------------------------------------------------------------------*
453000020110
453100020110     c     Prodet        begsr
453200020110
453300020110     c                   Z-ADD     1             K5
453400020110      * cerco il codice tassazione in schiera per TITAD
453500020110     c  N10TADCTS        LOOKUP    CCT(K5)                                35
453600020110      * cerco il codice tassazione in schiera per TITPD
453700020110     c   10TPDCTS        LOOKUP    CCT(K5)                                35
453800020110      *  se non trovato errore
453900020110     c  N35              SETON                                        99
454000020110     c     *IN99         IFEQ      '1'
454100020110     c                   MOVE      MSG(6)        ERRMSG
454200020110     c                   EXSR      STAERR
454300020110
454400020110     c                   ELSE
454500020110
454600020110      * TITAD
454700020110     c  N10              MOVE      TADORP        CODORP
454800020110      * TITPD
454900020110     c   10              MOVE      TPDORP        CODORP
455000020110
455100020110     c                   MOVE      SCT(K5)       CODSIG
455200020110      *  PULIZIA SCHIERE PROVINCIA
455300020110     c                   if        not *in10
455400020110     c                   CLEAR                   TCT
455500020110     c                   CLEAR                   TMN
455600020110     c                   CLEAR                   TMX
455700020110     c                   CLEAR                   TIN
455800020110
455900020110     c                   else
456000020110
456100020110     c                   MOVEA     *ZEROS        TAP
456200020110     c                   MOVEA     *ZEROS        TPN
456300020110     c                   MOVEA     *ZEROS        TPX
456400020110
456500020110     c                   endif
456600020110
456700020110     c                   END
456800020110
456900020110     c                   endsr
457000020110
457100020110      *------------------------------------------------------------------*
457200020110      *  Regdet           DETTAGLIO INIZIALE X  R E G I O N E
457300020110      *------------------------------------------------------------------*
457400020110
457500020110     c     Regdet        begsr
457600020110
457700020110     c                   Z-ADD     1             K5
457800020110      * cerco il codice tassazione in schiera per TITAD
457900020110     c  N10TADCTS        LOOKUP    CCT(K5)                                35
458000020110      * cerco il codice tassazione in schiera per TITPD
458100020110     c   10TPDCTS        LOOKUP    CCT(K5)                                35
458200020110      *  se non trovato errore
458300020110     c  N35              SETON                                        99
458400020110     c     *IN99         IFEQ      '1'
458500020110     c                   MOVE      MSG(6)        ERRMSG
458600020110     c                   EXSR      STAERR
458700020110
458800020110     c                   ELSE
458900020110
459000020110     c                   MOVE      RCT(K5)       CODREG
459100020111
459200020110     c                   Z-ADD     1             K5
459300020110     c     CODREG        LOOKUP    CRE(K5)                                35
459400020110     c  N35              SETON                                        99
459500020110     c     *IN99         IFEQ      '1'
459600020110     c                   MOVE      MSG(7)        ERRMSG
459700020110     c                   EXSR      STAERR
459800020110     c                   ELSE
459900020110
460000020110     c                   MOVEL     DRE(K5)       DESREG
460100020110      *  PULIZIA SCHIERE REGIONE
460200020110     c                   MOVE      *BLANKS       BIG
460300020110     c                   MOVE      *BLANKS       BMN
460400020110     c                   MOVE      *BLANKS       BMX
460500020110     c                   MOVE      *BLANKS       BSI
460600020110     c                   MOVE      *BLANKS       BIN
460700020110     c                   CLEAR                   BRG
460800020110     c                   MOVE      *BLANKS       WIN
460900020110     c                   MOVE      *BLANKS       WMN
461000020110     c                   MOVE      *BLANKS       WMX
461100020110     c                   MOVE      *BLANKS       WTI
461200020110
461300020110     c                   MOVE      *BLANKS       PDE
461400020110     c                   END
461500020110     c                   END
461600020110
461700020110     c                   endsr
461800020110
461900020110      *------------------------------------------------------------------*
462000020110      *  Decsca           DECODIFICO SCAGLIONI TARIFFE PARTICOLARI
462100020110      *------------------------------------------------------------------*
462200020110
462300020110     c     Decsca        begsr
462400020110
462500020110      * imposto i campi di rottura salvati
462600020110     c                   MOVEL     WRKREG        ROTREG
462700020110     c                   MOVE      WRKPRO        ROTPRO
462800020110     c                   MOVE      WRKCTS        ROTCTS
462900020110     c                   MOVE      WRKCTS        TPDCTS
463000020110     c                   MOVE      WRKREG        COM04
463100020110     c                   MOVEL     COM04         TPDORP
463200020110     c                   MOVE      WRKPRO        TPDORP
463300020110      * pulisco le schiere
463400020110     c                   exsr      Regdet
463500020110     c                   exsr      Prodet
463600020110
463700020110     c                   if        *in80 or curline > 56
463800020110     c                   write     ta50pfin
463900020110     c                   write     ta50pPP
464000020110     c                   setoff                                       80
464100020110     c                   endif
464200020110
464300020110      * stampo la dicitura della tariffa
464400020110      * se tariffa che viene stampata in modo standard recupero la decodifica
464500020110      * dall'estensione della tabella 1P la tabella SP
464600020110
464700020110     c                   if        tst(y) = ' '
464800020110     c                   movel(p)  edtc(y)       comstd
464900020110     c                   clear                   comdstd
465000020110     c                   write     ta50pstd
465100020110
465200020110     c                   else
465300020110      * elevazione limite risarcibile
465400020110    3c                   if        savftc = 'R'
465500020110     c                   write     ta50pma
465600020110    3c                   endif
465700020110      * assicurazione per conto
465800020110    3c                   if        savftc = 'C' and not *in21
465900020110     c                   write     ta50pma
466000020110    3c                   endif
466100020110    3c                   if        savftc = 'C' and *in21
466200020110     c                   write     ta50pma
466300020110    3c                   endif
466400020110
466500020110     c                   endif
466600020110      * valorizzo i campi del prtf scaglioni
466700020110     c                   clear                   desum
466800020110     c                   clear                   dissca
466900020110     c                   clear                   prtatf
467000020110     c                   eval      desum = de1tpg
467100020110     c                   eval      ds3um = de3tpg
467200151119
467300151119      /free
467400151119       //?Decodifica personalizzata del tipo tariffa
467500151119         SELECT;
467600151119       //?Italiano
467700151119         WHEN  I45lin = 'I';
467800151119       // Tariffa particolare Diritto Fatturazione
467900151119           IF  TPTftc = '§';
468000151119             desum = 'A FATTURA';
468100151119             ds3um = 'A FATTURA';
468200151119           ENDIF;
468300151119       // Tariffa particolare Gestione Bancali a Rendere
468400151119           IF  TPTftc = '=';
468500151119             desum = 'A BANCALE';
468600151119             ds3um = 'A BANCALE';
468700151119           ENDIF;
468800151119       // Tariffa particolare Diritto di chiamata prenotazione
468900151119       // ritiro telefonico
469000151119           IF  TPTftc = 't';
469100151119             desum = 'A RITIRO';
469200151119             ds3um = 'A RITIRO';
469300151119           ENDIF;
469400151119       //?Inglese
469500151119         WHEN  I45lin = '2';
469600151119       // Tariffa particolare Diritto Fatturazione
469700151119           IF  TPTftc = '§';
469800151119             desum = 'PER INVOICE';
469900151119             ds3um = 'PER INVOICE';
470000151119           ENDIF;
470100151119       // Tariffa particolare Gestione Bancali a Rendere
470200151119           IF  TPTftc = '=';
470300151119             desum = 'PER PALLET';
470400151119             ds3um = 'PER PALLET';
470500151119           ENDIF;
470600151119       // Tariffa particolare Diritto di chiamata prenotazione
470700151119       // ritiro telefonico
470800151119           IF  TPTftc = 't';
470900151119             desum = 'PER PICK-UP';
471000151119             ds3um = 'PER PICK-UP';
471100151119           ENDIF;
471200151119       //?Francese
471300151119         WHEN  I45lin = '3';
471400151119       // Tariffa particolare Diritto Fatturazione
471500151119           IF  TPTftc = '§';
471600151119             desum = 'A FATTURA';
471700151119             ds3um = 'A FATTURA';
471800151119           ENDIF;
471900151119       // Tariffa particolare Gestione Bancali a Rendere
472000151119           IF  TPTftc = '=';
472100151119             desum = 'A LA PALLETE';
472200151119             ds3um = 'A LA PALLETE';
472300151119           ENDIF;
472400151119       // Tariffa particolare Diritto di chiamata prenotazione
472500151119       // ritiro telefonico
472600151119           IF  TPTftc = 't';
472700151119             desum = 'PAR RETRAIT';
472800151119             ds3um = 'PAR RETRAIT';
472900151119           ENDIF;
473000151119       //?Tedesco
473100151119         WHEN  I45lin = '4';
473200151119       // Tariffa particolare Diritto Fatturazione
473300151119           IF  TPTftc = '§';
473400151119             desum = 'A FATTURA';
473500151119             ds3um = 'A FATTURA';
473600151119           ENDIF;
473700151119       // Tariffa particolare Gestione Bancali a Rendere
473800151119           IF  TPTftc = '=';
473900151119             desum = 'N.PALETTE';
474000151119             ds3um = 'N.PALETTE';
474100151119           ENDIF;
474200151119       // Tariffa particolare Diritto di chiamata prenotazione
474300151119       // ritiro telefonico
474400151119           IF  TPTftc = 't';
474500151119             desum = 'N. BUCHUNG';
474600151119             ds3um = 'N. BUCHUNG';
474700151119           ENDIF;
474800151119         ENDSL;
474900151119      /end-free
475000151119
475100020110      * stampo la testata
475200020110     c                   exsr      Sr_Statesdet
475300020111
475400020110     c                   endsr
475500020110
475600020110      *------------------------------------------------------------------*
475700020110      *  Sr_Statesdet     PREPARO E STAMPO TESTATA DETTAGLIO TARIFFE PART.
475800020110      *------------------------------------------------------------------*
475900020110
476000020110     c     Sr_Statesdet  begsr
476100020110
476200020110     c                   setoff                                       79
476300020110
476400020110      * Se scaglioni senza decimali
476500020110
476600020110     c                   if        not *in91
476700020110     c                   eval      dissca = ' '+%editc(ssld(1):'3') + sslb(1) +
476800020110     c                                      %editc(ssla(1):'Z') + '    ' +
476900020110     c                                      %editc(ssld(2):'3') + sslb(2) +
477000020110     c                                      %editc(ssla(2):'Z') + '    ' +
477100020110     c                                      %editc(ssld(3):'3') + sslb(3) +
477200020110     c                                      %editc(ssla(3):'Z') + '    ' +
477300020110     c                                      %editc(ssld(4):'3') + sslb(4) +
477400020110     c                                      %editc(ssla(4):'Z') + '    ' +
477500020110     c                                      %editc(ssld(5):'3') + sslb(5) +
477600020110     c                                      %editc(ssla(5):'Z') + '    ' +
477700020110     c                                      %editc(ssld(6):'3') + sslb(6) +
477800020110     c                                      %editc(ssla(6):'Z')
477900020110     c                   endif
478000020110
478100020110      * Se scaglioni con 1 decimale
478200020110
478300020110     c                   if        *in91 and not *in37
478400020110     c                   eval      dissca = '   '+%editc(ss1d(1):'4') +sslb(1)+
478500020110     c                                      %editc(ss1a(1):'4') + '    ' +
478600020110     c                                      %editc(ss1d(2):'4') + sslb(2) +
478700020110     c                                      %editc(ss1a(2):'4') + '    ' +
478800020110     c                                      %editc(ss1d(3):'4') + sslb(3) +
478900020110     c                                      %editc(ss1a(3):'4') + '    ' +
479000020110     c                                      %editc(ss1d(4):'4') + sslb(4) +
479100020110     c                                      %editc(ss1a(4):'4') + '    ' +
479200020110     c                                      %editc(ss1d(5):'4') + sslb(5) +
479300020110     c                                      %editc(ss1a(5):'4') + '    ' +
479400020110     c                                      %editc(ss1d(6):'4') + sslb(6) +
479500020110     c                                      %editc(ss1a(6):'4')
479600020110     c                   endif
479700020110
479800020110      * Se scaglioni con 2 decimali
479900020110
480000020110     c                   if        *in91  and  *in37
480100020110     c                   eval      dissca = ' '+%editc(ss2d(1):'4') + sslb(1) +
480200020110     c                                      %editc(ss2a(1):'4') + '    ' +
480300020110     c                                      %editc(ss2d(2):'4') + sslb(2) +
480400020110     c                                      %editc(ss2a(2):'4') + '   ' +
480500020110     c                                      %editc(ss2d(3):'4') + sslb(3) +
480600020110     c                                      %editc(ss2a(3):'4') + '   ' +
480700020110     c                                      %editc(ss2d(4):'4') + sslb(4) +
480800020110     c                                      %editc(ss2a(4):'4') + '   ' +
480900020110     c                                      %editc(ss2d(5):'4') + sslb(5) +
481000020110     c                                      %editc(ss2a(5):'4') + '   ' +
481100020110     c                                      %editc(ss2d(6):'4') + sslb(6) +
481200020110     c                                      %editc(ss2a(6):'4')
481300020110     c                   endif
481400020110      * se 40 acceso il 1° scaglione è = 99999 dopo almeno 6 scaglioni
481500020110     c                   if        *in40
481600060704      * in lingua
481700060704     c                   select
481800060704     c                   when      i45lin = 'I'
481900020110     c                   eval      dissca = '    OLTRE '
482000060704     c                   when      i45lin = '2'
482100060704     c                   eval      dissca = '    OVER  '
482200060801     c                   when      i45lin = '3'
482300060801     c                   eval      dissca = '    PLUS  '
482400060712     c                   when      i45lin = '4'
482500060728     c                   eval      dissca = '  MEHR ALS'
482600060704     c                   endsl
482700020110     c                   endif
482800020110      * se 41 acceso il 1° scaglione è = 99999
482900020110     c                   if        *in41
483000020110     c                   clear                   desum
483100020110     c                   eval      dissca = ds3um
483200020110     c                   seton                                        79
483300020110     c                   endif
483400020110      * se 42 acceso il 2° scaglione è = 99999
483500020110     c                   if        *in42
483600020110     c                   clear                   dissca2
483700060704      * in lingua
483800060704     c                   select
483900060704     c                   when      i45lin = 'I'
484000020110     c                   eval      dissca2 = 'OLTRE  '
484100060704     c                   when      i45lin = '2'
484200060704     c                   eval      dissca2 = 'OVER   '
484300060801     c                   when      i45lin = '3'
484400060801     c                   eval      dissca2 = 'PLUS   '
484500060712     c                   when      i45lin = '4'
484600060728     c                   eval      dissca2 = 'MEHR ALS'
484700060704     c                   endsl
484800020110     c                   move      dissca2       dissca
484900020110     c                   endif
485000020110      * se 52 acceso il 2° scaglione è = 00000
485100020110     c                   if        *in52
485200020110     c                   clear                   dissca2
485300020110     c                   move      dissca2       dissca
485400020110     c                   endif
485500020110      * se 43 acceso il 3° scaglione è = 99999
485600020110     c                   if        *in43
485700020110     c                   clear                   dissca3
485800060704      * in lingua
485900060704     c                   select
486000060704     c                   when      i45lin = 'I'
486100020110     c                   eval      dissca3 = 'OLTRE  '
486200060704     c                   when      i45lin = '2'
486300060704     c                   eval      dissca3 = 'OVER   '
486400060801     c                   when      i45lin = '3'
486500060801     c                   eval      dissca3 = 'PLUS   '
486600060712     c                   when      i45lin = '4'
486700060728     c                   eval      dissca3 = 'MEHR ALS'
486800060704     c                   endsl
486900020110     c                   move      dissca3       dissca
487000020110     c                   endif
487100020110      * se 53 acceso il 3° scaglione è = 00000
487200020110     c                   if        *in53
487300020110     c                   clear                   dissca3
487400020110     c                   move      dissca3       dissca
487500020110     c                   endif
487600020110      * se 44 acceso il 4° scaglione è = 99999
487700020110     c                   if        *in44
487800020110     c                   clear                   dissca4
487900060704      * in lingua
488000060704     c                   select
488100060704     c                   when      i45lin = 'I'
488200020110     c                   eval      dissca4 = 'OLTRE  '
488300060704     c                   when      i45lin = '2'
488400060704     c                   eval      dissca4 = 'OVER   '
488500060801     c                   when      i45lin = '3'
488600060801     c                   eval      dissca4 = 'PLUS   '
488700060712     c                   when      i45lin = '4'
488800060728     c                   eval      dissca4 = 'MEHR ALS'
488900060704     c                   endsl
489000020110     c                   move      dissca4       dissca
489100020110     c                   endif
489200020110      * se 54 acceso il 4° scaglione è = 00000
489300020110     c                   if        *in54
489400020110     c                   clear                   dissca4
489500020110     c                   move      dissca4       dissca
489600020110     c                   endif
489700020110      * se 45 acceso il 5° scaglione è = 99999
489800020110     c                   if        *in45
489900020110     c                   clear                   dissca5
490000060704      * in lingua
490100060704     c                   select
490200060704     c                   when      i45lin = 'I'
490300020110     c                   eval      dissca5 = 'OLTRE  '
490400060704     c                   when      i45lin = '2'
490500060704     c                   eval      dissca5 = 'OVER   '
490600060801     c                   when      i45lin = '3'
490700060801     c                   eval      dissca5 = 'PLUS   '
490800060712     c                   when      i45lin = '4'
490900060728     c                   eval      dissca5 = 'MEHR ALS'
491000060704     c                   endsl
491100020110     c                   move      dissca5       dissca
491200020110     c                   endif
491300020110      * se 55 acceso il 5° scaglione è = 00000
491400020110     c                   if        *in55
491500020110     c                   clear                   dissca5
491600020110     c                   move      dissca5       dissca
491700020110     c                   endif
491800020110      * se 46 acceso il 6° scaglione è = 99999
491900020110     c                   if        *in46
492000020110     c                   clear                   dissca6
492100060704      * in lingua
492200060704     c                   select
492300060704     c                   when      i45lin = 'I'
492400020110     c                   eval      dissca6 = 'OLTRE '
492500060704     c                   when      i45lin = '2'
492600060704     c                   eval      dissca6 = 'OVER   '
492700060801     c                   when      i45lin = '3'
492800060801     c                   eval      dissca6 = 'PLUS   '
492900060712     c                   when      i45lin = '4'
493000060728     c                   eval      dissca6 = 'MEHR ALS'
493100060704     c                   endsl
493200020110     c                   move      dissca6       dissca
493300020110     c                   endif
493400020110      * se 56 acceso il 6° scaglione è = 00000
493500020110     c                   if        *in56
493600020110     c                   clear                   dissca6
493700020110     c                   move      dissca6       dissca
493800020110     c                   endif
493900020110
494000020111      * preparo la schiera dell'applicazione tariffa finita
494100020110     c                   Z-ADD     K4            B
494200020110     c                   DO        6             A
494300020110     c                   MOVE      ATF(B)        STF(A)
494400020110     c                   ADD       1             B
494500020110     c                   END
494600020110
494700020110     c                   clear                   prtatf
494800020110
494900020110     c                   eval      PRTATF = '    ' + stf(1) + '      '  +
495000020110     c                                      stf(2) + '      '    +
495100020110     c                                      stf(3) + '      '    +
495200020110     c                                      stf(4) + '      '    +
495300020110     c                                      stf(5) + '      '    +
495400020110     c                                      stf(6)
495500020110
495600020110     c                   if        *in80 or curline > 56
495700020110     c                   write     ta50pfin
495800020110     c                   write     ta50pPP
495900020110     c                   setoff                                       80
496000020110     c                   endif
496100020110      * se non stampo dettaglio tariffe particolari stampo linea di partenza
496200020110     c  n10              write     ta50p02a
496300020110      * SE TARIFFE PARTICOLARI NON STAMPO DA VOSTRO MAGAZZINO
496400020110     c                   IF        *IN10
496500020110     c                   SETON                                        78
496600020110     c                   ENDIF
496700020110
496800020110     c                   write     ta50p02b
496900020110
497000020110     c                   endsr
497100020110
497200020110      *------------------------------------------------------------------*
497300020110      * Legdet            LEGGO DETTAGLIO DI TPD
497400020110      *------------------------------------------------------------------*
497500020110
497600020110     c     Legdet        begsr
497700020110
497800061011     c  n98KTPT          SETLL     TITPD01l
497900061011     c   98ktpt          setll     tiopd01l
498000020110
498100020110    1c                   DO        *HIVAL
498200061011     c  n98KTPT          READE     TITPD01l
498300061011     c   98ktpt          reade     tiopd01l
498400020110
498500061011     c                   IF        %EOF
498600020110     c                   LEAVE
498700020110     c                   ENDIF
498800020110
498900020110     c                   IF        TPDATB = 'A'
499000020110     c                   ITER
499100020110     c                   ENDIF
499200020110
499300020110      * SE SCAGLIONE 9999 VERIFICO SE E' L'ULTIMO
499400020110     c                   Z-ADD     TPDSGL        NUM4
499500020110    2c     NUM4          IFEQ      9999
499600020110     c     TPDSGL        ANDNE     *ALL'9'
499700020110
499800020110     c                   MOVEL     TPDORP        SAVORP
499900020110     c                   MOVEL     TPDSGL        SAVSGL
500000020110
500100020110     c                   SETOFF                                       35
500200020110      * verifico se esiste un'altro scaglione per lo stesso ORP
500300020110    3c                   DO        *HIVAL
500400061011     c  n98KTPD          READE     TITPD01l                               35
500500061011     c   98ktpd          reade     tiopd01l                               35
500600020110      * se annullato leggo il successivo
500700020110     c                   IF        NOT *IN35 AND TPDATB = 'A'
500800020110     c                   ITER
500900020110     c                   ENDIF
501000020110      * se non trovato o non trovato riaggancio il record precedente
501100061011     c  n98KTPD2         CHAIN     TITPD01l
501200061011     c   98ktpd2         chain     tiopd01l
501300020110
501400020110     c                   LEAVE
501500020110
501600020110    3c                   ENDDO
501700020110
501800020110      * NON C'E' ALTRO SCAGLIONE
501900020110    2c                   ENDIF
502000020110
502100020110     c                   Z-ADD     1             K2
502200020110     c                   Z-ADD     TPDSGL        wsgl72
502300020110      * cerco lo scaglione nella schiera
502400020110     c     wsgl72        LOOKUP    SCP(1)                                 35
502500020110      * se non trovato cerco lo scaglione nella schiera stampa
502600020110     c   35wsgl72        LOOKUP    SSL(K2)                                90
502700020110      * se non trovato nella schiera di sola stampa ma trovato nella schiera SCP
502800020110      * torno a leggere
502900020110     c                   if        *in35 and not *in90
503000020110     c                   iter
503100020110     c                   endif
503200020110
503300020110      * se non trovato nella schiera scp ERRORE  scaglione non previsto
503400020110    2c                   IF        not *in35
503500020110     c                   MOVEL     MSG(14)       ERRMSG
503600020110     c                   exsr      Staerr
503700050914     c                   Goto      FINE
503800020110
503900020110   X2c                   else
504000020110     c                   eval      compro = %subst(tpdorp:5:1)
504100020110     c                   eval      comreg = %subst(tpdorp:3:2)
504200020110      * CAMBIATA PROVINCIA
504300020110    3c                   IF        compro <>  rotpro or comreg <> rotreg
504400020110     c                   EXSR      PROTPT
504500020110     c                   EXSR      PRODET
504600020110     c   99              GOTO      FINE
504700020110     c                   MOVE      COMPRO        ROTPRO
504800020110    3c                   END
504900020110      * CAMBIATA REGIONE
505000020110    3c                   if        comreg <> rotreg
505100020110     c                   exsr      Regtpt
505200020110
505300020110     c                   exsr      regdet
505400020110     c   99              goto      fine
505500020110     c                   movel     comreg        rotreg
505600020110    3c                   endif
505700020110
505800020110     c                   Z-ADD     TPDITR        TAP(K2)
505900020110     c                   Z-ADD     TPDMIN        TPN(K2)
506000020110     c                   Z-ADD     TPDMAX        TPX(K2)
506100020110     c                   MOVEL     TPDAIN        SAVAIP
506200020110    2c                   END
506300020110
506400020110    1c                   ENDDO
506500020110
506600020110     c                   exsr      Protpt
506700020111     c                   exsr      Regtpt
506800020110
506900020110     c                   endsr
507000020110
507100020110      *------------------------------------------------------------------*
507200020110      * Protpt            CAMBIO PROVINCIA
507300020110      *------------------------------------------------------------------*
507400020110
507500020110     c     Protpt        begsr
507600020110
507700020110     c                   z-add     1             c
507800020110     c     *blanks       lookup    big(c)                                 35
507900020110     c     *in35         ifeq      '0'
508000020110     c                   move      msg(10)       errmsg
508100020110     c                   exsr      Staerr
508200050914     c                   goto      fine
508300020110     c                   else
508400020110
508500020110     c                   do        6             b
508600020110     c                   move      tap(b)        com(b)
508700020110     c                   end
508800020110
508900020110     c                   movea     com           big(c)
509000020110
509100020110     c                   do        6             b
509200020110     c                   move      tpn(b)        com(b)
509300020110     c                   end
509400020110
509500020110     c                   movea     com           bmn(c)
509600020110
509700020110     c                   do        6             b
509800020110     c                   move      tpx(b)        com(b)
509900020110     c                   end
510000020110
510100020110     c                   movea     com           bmx(c)
510200020110     c
510300020110     c                   move      codsig        bsi(c)
510400020110     c                   move      savaip        aip(c)
510500020110     c                   move      codorp        brg(c)
510600020110     c                   end
510700020110
510800020110     c                   movel     ' '           savaip
510900020110
511000020110     c                   endsr
511100020110
511200020110      *------------------------------------------------------------------*
511300020110      * Regtpt            CAMBIO REGIONE
511400020110      *------------------------------------------------------------------*
511500020110
511600020110     c     Regtpt        begsr
511700020110
511800020110     c                   Z-ADD     0             NUM4
511900020110     c                   Z-ADD     1             K
512000020110      * preparo le schiere che devono essere stampate
512100050616    1c                   DO        15            K
512200020110    2c     BRG(K)        IFNE      '  '
512300020110    3c     BRG(K)        IFEQ      '9'
512400020110
512500020110     c     K             IFEQ      1
512600020110     c                   MOVEL     *all'.'       PDE(K)
512700020110     c                   ELSE
512800060704      * in lingua
512900060704     c                   select
513000060704     c                   when      i45lin = 'I'
513100020110     c                   eval      PDE(K) = 'ALTRI........................'
513200060704     c                   when      i45lin = '2'
513300060704     c                   eval      pde(k) = 'OTHERS.......................'
513400060801     c                   when      i45lin = '3'
513500060801     c                   eval      pde(k) = 'AUTRES.......................'
513600060712     c                   when      i45lin = '4'
513700060728     c                   eval      PDE(K) = 'ANDERE.......................'
513800060704     c                   endsl
513900020110     c                   END
514000020110     c                   ADD       1             NUM4
514100020110
514200020110    3c                   ELSE
514300020110
514400020110     c                   MOVEA     *BLANKS       WCT
514500020110     c                   Z-ADD     1             C
514600020110     c                   MOVE      BSI(K)        WCT(C)
514700020110
514800020110      * città
514900020110     c                   ADD       1             P
515000020110     c                   MOVEA     BIG(K)        COMO42
515100020110     c                   MOVEA     COMO42        WIN(P)
515200020110      * minimo
515300020110     c                   MOVEA     BMN(K)        CMNO42
515400020110     c                   MOVEA     CMNO42        WMN(P)
515500020110      * massimo
515600020110     c                   MOVEA     BMX(K)        CMXO42
515700020110     c                   MOVEA     CMXO42        WMX(P)
515800020110     c                   MOVEL     AIP(K)        SAVAIP            1
515900020110
516000020110      * MEMORIZZO LE ALTRE PROVINCIE CHE HANNO LE STESSE TARIFFE
516100020110     c     K             ADD       1             H
516200050616    4c     H             DO        15            H
516300020110
516400020110     c     BRG(H)        IFNE      '9'
516500020110     c     como42        ANDEQ     big(H)
516600020110     c     cmno42        ANDEQ     bmn(H)
516700020110     c     cmxo42        ANDEQ     bmX(H)
516800020110     c     SAVAIP        ANDEQ     AIP(H)
516900020110
517000020110      * se isola controllo anche che non siano le province sondrio e como
517100020110     c                   IF        (*in06  and bsi(h)<>'CO' and bsi(h)<>'SO')
517200020110     c                             or not *in06
517300020110     c                   ADD       1             C
517400020110     c                   MOVEL     '/'           WCT(C)
517500020110     c                   MOVE      BSI(H)        WCT(C)
517600020110     c                   MOVEL     *BLANKS       big(H)
517700020110     c                   MOVEL     *BLANKS       bmn(H)
517800020110     c                   MOVEL     *BLANKS       bmx(H)
517900020110     c                   MOVEL     *BLANKS       BRG(H)
518000020110     c                   endif
518100020110
518200020110     c                   ENDIF
518300020110
518400020110   e4c                   ENDDO
518500020110
518600020110      * MEMORIZZO LA SCHIERA DELLE PROVINCIE CON STESSE TARIFFE
518700020110     c                   ADD       1             C
518800020110     c                   MOVEA     *all'.'       WCT(C)
518900020110     c                   movea     wct           como29
519000020110     c                   MOVEA     como29        PDE(K)
519100020110      * se sto stampando le isole recupero la decodifica delle isole
519200020110     c                   if        %subst(como29:2:2) = 'CO' and *in06
519300020110     c                   eval      PDE(k) = 'CAMPIONE D''ITALIA .........'
519400020110     c                   ENDIF
519500020110
519600020110     c                   if        %subst(como29:2:2) = 'SO' and *in06
519700020110     c                   eval      PDE(k) = 'LIVIGNO ....................'
519800020110     c                   ENDIF
519900020110
520000020110   e3c                   END
520100020110   e2c                   END
520200020110   e1c                   END
520300020110
520400020110      * STAMPA REGIONE
520500050616    1c                   DO        15            K
520600020110
520700020110    2c     BRG(K)        IFNE      ' '
520800020110
520900020110     c                   movea     BIG(k)        com
521000020110     c                   z-add     1             b
521100020110     c                   do        6             b
521200020110     c                   move      com(b)        tap(b)
521300020110      * se tariffa a valore di fianco al valore metto la percentuale
521400020110     c                   if        *in09 and tap(b) > 0
521500020110     c                   MOVEL     '%'           PER(B)
521600020110     c                   END
521700020110
521800020110     c                   END
521900020110      * applicazione inoltro (forse non si usa)
522000020110     c     AIP(K)        IFEQ      'S'
522100020110     c                   MOVEL     'SI'          STAAIP            2
522200020110     c                   ELSE
522300020110     c                   MOVEL     'NO'          STAAIP
522400020110     c                   END
522500020111
522600020110      * VERIFICO SE C'E' IL MINIMO E IL MASSIMO
522700020110     c                   SETOFF                                       7586
522800020110
522900020110    2c                   IF        BMN(K) <> *BLANKS AND BMN(K) <> *ZEROS
523000020110     c                   SETON                                        75
523100020110
523200020110     c                   MOVEA     BMN(K)        COM
523300020110
523400020110     c                   DO        6             B
523500020110     c                   MOVE      COM(B)        TPN(B)
523600020110     c                   ENDDO
523700020110
523800020110    2c                   ENDIF
523900020110
524000020110    2c                   IF        BMX(K) <> *BLANKS AND BMX(K) <> *ZEROS
524100020110     c                   SETON                                        86
524200020110
524300020110     c                   MOVEA     BMX(K)        COM
524400020110
524500020110     c                   DO        6             B
524600020110     c                   MOVE      COM(B)        TPX(B)
524700020110     c                   ENDDO
524800020110
524900020110    2c                   ENDIF
525000020110
525100020110      * gestione stampa tariffa particolare
525200020110     c                   EXSR      SR_STPPAR
525300020110
525400020110    2c                   END
525500020110
525600020110    1c                   ENDDO
525700020110
525800020110     c                   MOVEA     *blanks       big
525900020110     c                   MOVEA     *blanks       bmn
526000020110     c                   MOVEA     *blanks       bmx
526100020110     c                   MOVEA     *blanks       win
526200020110     c                   MOVEA     *blanks       wmn
526300020110     c                   MOVEA     *blanks       wmx
526400020110     c                   MOVEA     *BLANKS       BSI
526500020110     c                   MOVEA     *BLANKS       AIP
526600020110     c                   MOVEA     *BLANKS       PER
526700020110     c                   MOVEA     *ZEROS        TAP
526800020110     c                   MOVEA     *ZEROS        TPN
526900020110     c                   MOVEA     *ZEROS        TPX
527000020110     c                   CLEAR                   BRG
527100020110
527200020110     c                   endsr
527300020110
527400020110      *------------------------------------------------------------------*
527500020110      * Sr_Stppar         STAMPA TARIFFE PARTICOLARI
527600020110      *------------------------------------------------------------------*
527700020110
527800020110     c     Sr_Stppar     begsr
527900020110
528000020110      * valorizzo il dettaglio tariffario in stampa
528100020110     c                   clear                   ta50p03b
528200020110
528300020110     c                   eval      p03des = desreg + ' ' + pde(k)
528400020110     c                   clear                   desreg
528500020110      * verifico se sono tutti valorizzati
528600020110     c     tap(1)        comp      0                                  61
528700020110     c     tap(2)        comp      0                                  62
528800020110     c     tap(3)        comp      0                                  63
528900020110     c     tap(4)        comp      0                                  64
529000020110     c     tap(5)        comp      0                                  65
529100020110     c     tap(6)        comp      0                                  66
529200020110
529300020110     c                   z-add     tap(1)        tar01p
529400020110     c                   z-add     tap(2)        tar02p
529500020110     c                   z-add     tap(3)        tar03p
529600020110     c                   z-add     tap(4)        tar04p
529700020110     c                   z-add     tap(5)        tar05p
529800020110     c                   z-add     tap(6)        tar06p
529900020110      * SE TARIFFA A VALORE STAMPO LA %
530000020110    2c                   IF        *IN09
530100020110     c                   MOVE      per(1)        PER01
530200020110     c                   MOVE      per(2)        PER02
530300020110     c                   MOVE      per(3)        PER03
530400020110     c                   MOVE      per(4)        PER04
530500020110     c                   MOVE      per(5)        PER05
530600020110     c                   MOVE      per(6)        PER06
530700020110    2c                   ENDIF
530800020110
530900020110     c                   if        *in80 or curline > 61
531000020110     c                   write     ta50pfin
531100020110     c                   write     ta50pPP
531200020110     c                   write     ta50p02b
531300020110     c                   setoff                                       80
531400020110     c                   endif
531500020110
531600020110     c                   write     ta50p03b
531700020110
531800020110      * VERIFICO SE CI SONO DEI VALORI A ZERO
531900020110
532000020110      * VALORIZZO I CAMPI DI STAMPA
532100020110     c                   clear                   ta50p03b
532200020110      * MINIMO
532300020110    2c                   IF        *IN75
532400020110     c     tpn(1)        COMP      0                                  61
532500020110     c     tpn(2)        COMP      0                                  62
532600020110     c     tpn(3)        COMP      0                                  63
532700020110     c     tpn(4)        COMP      0                                  64
532800020110     c     tpn(5)        COMP      0                                  65
532900020110     c     tpn(6)        COMP      0                                  66
533000020110
533100020110     c                   Z-ADD     tpn(1)        TAR01p
533200020110     c                   Z-ADD     tpn(2)        TAR02p
533300020110     c                   Z-ADD     tpn(3)        TAR03p
533400020110     c                   Z-ADD     tpn(4)        TAR04p
533500020110     c                   Z-ADD     tpn(5)        TAR05p
533600020110     c                   Z-ADD     tpn(6)        TAR06p
533700020110
533800020110      * VALORIZZO LA PARTE DESCRITTIVA
533900060704      * in lingua
534000060704     c                   select
534100060704     c                   when      i45lin = 'I'
534200020110     c                   EVAL      COMO29 = '           Tariffa minima ...'
534300060704     c                   when      i45lin = '2'
534400060704     c                   eval      como29 = '           Minimum price ....'
534500060728     c                   when      i45lin = '3'
534600060728     c                   eval      como29 = '           Tarif minimum ....'
534700060712     c                   when      i45lin = '4'
534800060728     c                   EVAL      COMO29 = '           Mindestpreis .....'
534900060704     c                   endsl
535000020110     c                   MOVE(p)   COMO29        P03DES
535100020110
535200020110     c                   if        *in80 or curline > 61
535300020110     c                   write     ta50pfin
535400020110     c                   write     ta50pPP
535500020110     c                   write     ta50p02b
535600020110     c                   setoff                                       80
535700020110     c                   endif
535800020110
535900020110     c                   write     ta50p03b
536000020110
536100020110    2c                   ENDIF
536200020110
536300020110      * VERIFICO SE CI SONO DEI VALORI A ZERO
536400020110
536500020110      * VALORIZZO I CAMPI DI STAMPA
536600020110     c                   clear                   ta50p03b
536700020110      * MASSIMO
536800020110    2c                   IF        *IN86
536900020110     c     tpx(1)        COMP      0                                  61
537000020110     c     tpx(2)        COMP      0                                  62
537100020110     c     tpx(3)        COMP      0                                  63
537200020110     c     tpx(4)        COMP      0                                  64
537300020110     c     tpx(5)        COMP      0                                  65
537400020110     c     tpx(6)        COMP      0                                  66
537500020110
537600020110     c                   Z-ADD     tpx(1)        TAR01p
537700020110     c                   Z-ADD     tpx(2)        TAR02p
537800020110     c                   Z-ADD     tpx(3)        TAR03p
537900020110     c                   Z-ADD     tpx(4)        TAR04p
538000020110     c                   Z-ADD     tpx(5)        TAR05p
538100020110     c                   Z-ADD     tpx(6)        TAR06p
538200020110
538300020110      * VALORIZZO LA PARTE DESCRITTIVA
538400060704      * in lingua
538500060704     c                   select
538600060704     c                   when      i45lin = 'I'
538700020110     c                   EVAL      COMO29 = '           Tariffa massima ..'
538800060704     c                   when      i45lin = '2'
538900060704     c                   eval      como29 = '           Maximum price ....'
539000060728     c                   when      i45lin = '3'
539100060728     c                   eval      como29 = '           Tarif maximum ....'
539200060712     c                   when      i45lin = '4'
539300060728     c                   EVAL      COMO29 = '           Höchstpreis ......'
539400060704     c                   endsl
539500020110     c                   MOVE(p)   COMO29        P03DES
539600020110
539700020110     c                   if        *in80 or curline > 61
539800020110     c                   write     ta50pfin
539900020110     c                   write     ta50pPP
540000020110     c                   write     ta50p02b
540100020110     c                   setoff                                       80
540200020110     c                   endif
540300020110
540400020110     c                   write     ta50p03b
540500020110
540600020110    2c                   ENDIF
540700020110
540800020110     c                   endsr
540900020110
541000020110      *------------------------------------------------------------------*
541100020110      * Findet            STAMPA FINALE SOTTO OGNI SINGOLA TARIFFA PARTICOLARE
541200020110      *------------------------------------------------------------------*
541300020110
541400020110     c     Findet        begsr
541500020110
541600020110      * stampo eventuale estensione scaglione unico
541700020110     c                   exsr      Sr_Estes
541800020110
541900020110     c                   clear                   ta50ptp
542000020110      * verifico se tariffa a q.le per stampare il minimo tassabile
542100020110     c                   eval      *in08 = (tpttpg = '0')
542200020110      * per 08 controllo il minimo tassabile
542300020110      * nel caso in cui si stampa + di uno scaglione verifico il 1° scaglione
542400020110     c                   if        *in08 and recval > 1
542500020110     c                   if        scp(1) < §trata
542600020110     c                   z-add     scp(1)        commts
542700020110     c                   else
542800020110     c                   z-add     §trata        commts
542900020110     c                   endif
543000020110     c                   endif
543100020110      * nel caso in cui si stampa solo uno scaglione verifico il 1° scaglione
543200020110      * salvato
543300020110     c                   if        *in08 and recval = 1
543400020110     c                   if        mintas < §trata and mintas > 0
543500020110     c                   z-add     mintas        commts
543600020110     c                   else
543700020110     c                   z-add     §trata        commts
543800020110     c                   endif
543900020110     c                   endif
544000020111
544100020110      * verifico rapporto peso / volume
544200040325      * per FEDEX fisso 1,67    (non più)
544300020110     c                   if        tptrpv > 0
544400040325     c     tptrpv        mult      100           comrpv
544500040325     c***  1,67          mult      100           comrpv
544600020110     c                   seton                                        19
544700020110     c                   endif
544800020111
544900020110      * verifico gli importi degli arrotondamenti
545000020110    1c                   if        tptarl > 0
545100020110      * verifico se esistono dei valori con i decimali
545200020110     c                   move      tptarf        w0030
545300020110     c                   eval      *in89 =(w0030 > 0)
545400020110
545500020110    2c                   if        not *in89
545600020110     c                   move      tptarl        w0030
545700020110     c                   eval      *in89 =(w0030 > 0)
545800020110    2c                   endif
545900020110
546000020110    2c                   if        not *in89
546100020110     c                   move      tptaro        w0030
546200020110     c                   eval      *in89 =(w0030 > 0)
546300020110    2c                   endif
546400020110
546500020111      * se 89 spento non esistono importi con decimali sposto in campo di comodo
546600020110    2c                   if        not *in89
546700020110     c                   Z-ADD     TptARO        WARO2
546800020110     c                   Z-ADD     TptARF        WARF2
546900020110     c                   Z-ADD     TptARL        WARL2
547000020110    2c                   endif
547100020110
547200020110      * se arrotondamenti di 1 in 1 non stampo
547300020110      * se arrotondamento fino a diverso da arrotondamento oltre a
547400020110    2c                   if        tptarf <> tptaro
547500020110
547600020110      * valorizzo i campi di stampa
547700060704      * in lingua
547800060704     c                   select
547900060704     c                   when      i45lin = 'I'
548000020110     c  N89              eval      arrot1 = 'fino a ' + %editc(Warl2:'4') +
548100020110     c                                      ' di ' + %editc(warf2:'4')     +
548200020110     c                                      ' in ' + %editc(warf2:'4')
548300020110     c   89              eval      arrot1 = 'fino a ' + %editc(tptarl:'4') +
548400020110     c                                      ' di ' + %editc(tptarf:'4')     +
548500020110     c                                      ' in ' + %editc(tptarf:'4')
548600060704     c                   when      i45lin = '2'
548700060704     c  n89              eval      arrot1 = 'Up to ' +
548800060704     c                                      %editc(warl2:'4') + ' rounded ' +
548900060704     c                                      'to the nearest ' +
549000060704     c                                      %editc(warf2:'4')
549100060704     c   89              eval      arrot1 = 'Up to ' +
549200060704     c                                      %editc(tptarl:'4') + ' rounded ' +
549300060704     c                                      'to the nearest ' +
549400060704     c                                      %editc(tptarl:'4')
549500060801     c                   when      i45lin = '3'
549600060818     c  N89              eval      arrot1 = 'jusq''à ' + %editc(Warl2:'4') +
549700060801     c                                      ' de ' + %editc(warf2:'4')     +
549800060801     c                                      ' en ' + %editc(warf2:'4')
549900060818     c   89              eval      arrot1 = 'jusq''à ' + %editc(tptarl:'4') +
550000060801     c                                      ' de ' + %editc(tptarf:'4')     +
550100060801     c                                      ' en ' + %editc(tptarf:'4')
550200060712     c                   when      i45lin = '4'
550300060818     c  N89              eval      arrot1 = 'bis  ' + %editc(Warl2:'4') +
550400060710     c                                      ' von ' + %editc(warf2:'4')     +
550500060710     c                                      ' auf ' + %editc(warf2:'4')
550600060818     c   89              eval      arrot1 = 'bis  ' + %editc(tptarl:'4') +
550700060710     c                                      ' von ' + %editc(tptarf:'4')     +
550800060710     c                                      ' auf ' + %editc(tptarf:'4')
550900060704     c                   endsl
551000020110
551100020110     c                   seton                                        18
551200020110
551300020110      * valorizzo i campi di stampa
551400060704      * in lingua
551500060704     c                   select
551600060704     c                   when      i45lin = 'I'
551700020110     c  N89              eval      arrot2 = 'oltre  ' + '    '        +
551800020110     c                                      ' di ' + %editc(waro2:'4')     +
551900020110     c                                      ' in ' + %editc(waro2:'4')
552000020110     c   89              eval      arrot2 = 'oltre  ' + '        '         +
552100020110     c                                      ' di ' + %editc(tptaro:'4')     +
552200020110     c                                      ' in ' + %editc(tptaro:'4')
552300060704     c                   when      i45lin = '2'
552400060704     c  n89              eval      arrot2 = 'over ' +  '         '  +
552500060704     c                                      ' to nearest ' +
552600060704     c                                      %editc(waro2:'4')
552700060704     c   89              eval      arrot2 = 'over ' +  '         '  +
552800060704     c                                      ' to nearest ' +
552900060704     c                                      %editc(tptaro:'4')
553000060801     c                   when      i45lin = '3'
553100060801     c  N89              eval      arrot2 = 'au delà' + '    '        +
553200060801     c                                      ' de ' + %editc(waro2:'4')     +
553300060801     c                                      ' en ' + %editc(waro2:'4')
553400060801     c   89              eval      arrot2 = 'au delà' + '        '         +
553500060801     c                                      ' de ' + %editc(tptaro:'4')     +
553600060801     c                                      ' en ' + %editc(tptaro:'4')
553700060712     c                   when      i45lin = '4'
553800060818     c  N89              eval      arrot2 = 'mehr als ' + '    '        +
553900060710     c                                      ' von ' + %editc(waro2:'4')     +
554000060710     c                                      ' auf ' + %editc(waro2:'4')
554100060818     c   89              eval      arrot2 = 'mehr als ' + '   '         +
554200060710     c                                      ' von ' + %editc(tptaro:'4')     +
554300060710     c                                      ' auf ' + %editc(tptaro:'4')
554400060704     c                   endsl
554500020110
554600020110     c                   ELSE
554700020110
554800020110      * valorizzo i campi di stampa
554900060704      * in lingua
555000060704     c                   select
555100060704     c                   when      i45lin = 'I'
555200020110     c  N89              eval      arrot1 = 'di ' + %editc(Warf2:'4') +
555300020110     c                                      ' in ' + %editc(waro2:'4')
555400020110     c   89              eval      arrot1 = 'di     ' + %editc(tptarf:'4') +
555500020110     c                                      ' in' + %editc(tptaro:'4')
555600060704     c                   when      i45lin = '2'
555700060704     c  n89              eval      arrot1 = 'Rounded to nearest ' +
555800060704     c                                      %editc(warf2:'4')
555900060704     c   89              eval      arrot1 = 'Rounded to nearest ' +
556000060704     c                                      %editc(tptarf:'4')
556100060801     c                   when      i45lin = '3'
556200060801     c  N89              eval      arrot1 = 'de ' + %editc(Warf2:'4') +
556300060801     c                                      ' en ' + %editc(waro2:'4')
556400060801     c   89              eval      arrot1 = 'di     ' + %editc(tptarf:'4') +
556500060801     c                                      ' in' + %editc(tptaro:'4')
556600060712     c                   when      i45lin = '4'
556700060710     c  N89              eval      arrot1 = 'von ' + %editc(Warf2:'4') +
556800060710     c                                      ' auf ' + %editc(waro2:'4')
556900060710     c   89              eval      arrot1 = 'von    ' + %editc(tptarf:'4') +
557000060710     c                                      ' auf' + %editc(tptaro:'4')
557100060704     c                   endsl
557200020110
557300020110     c                   seton                                        18
557400020110
557500020110    2c                   endif
557600020110
557700020110    1c                   endif
557800020110
557900020110      * verifico se arrot2 è uguale a blanks accendo il 20
558000020110     c                   eval      *in20 = (arrot2 <> *blanks)
558100020110
558200020110      * verifico se devo stampare questa riga
558300020110      * in assenza di minimo tassabile o arrotondamenti non stampo
558400020110      * *in08 minimo tassabile *in18 arrotondamenti *in19 volume automatico
558500020110     c                   if        *in08 or *in18 or *in19
558600020111     c                   exsr      Sr_Over
558700020111
558800020110     c                   write     ta50ptp
558900020111
559000020110     c                   endif
559100020111
559200020110     c                   endsr
559300020110
559400020110      *------------------------------------------------------------------*
559500020110      * Sr_Estes          GESTIONE STAMPA ESTENSIONE SCAGLIONE UNICO
559600020110      *------------------------------------------------------------------*
559700020110     c     Sr_Estes      begsr
559800020110
559900020110      * se il campo del TARPAREST è diverso da blanks vuol dire che devo
560000020110      * stampare l'estensione della tariffa particolare
560100020110     c                   if        w_tarparest <> *blanks
560200020110     c                   eval      comest = w_tarparest
560300020110     c                   exsr      Sr_Over
560400020110
560500020110     c                   write     ta50pest
560600020110
560700020110     c                   clear                   w_tarparest
560800020110
560900020110     c                   endif
561000020110
561100020110     c                   endsr
561200050527
561300050527      *---------------------------------------------------------------*
561400050527      * SR_COMUNICA controllo e stampa dei servizi on line + e-mail
561500050527      *---------------------------------------------------------------*
561600050527     c     SR_COMUNICA   BegSr
561700050527
561800050527      * Controllo se devo stampare i SERVIZI ON LINE
561900050527     c                   ExSr      Sr_Psw
562000050527      * Controllo se devo stampare la richiesta di almeno 1 mail
562100050527     c                   ExSr      Sr_Ema
562200050527
562300050527      * Stampo la testata se devo stampare la richiesta delle e-mail
562400050527     c                   If        wpsw = *on or wema85 = *on or
562500051206     c                             wema87 = *on or wema88 = *on or
562600090618     c                             wema84 = *On or wema89 = *on
562700050527     c                   ExSr      Sr_Over
562800050527     c                   Write     Ta50pema
562900050527      * E-mail per servizi on line
563000050527     c                   If        wpsw = *on
563100050527     c                   Z-Add     curline       totrig
563200050527     c                   Add       4             totrig
563300050527     c                   ExSr      Sr_Overpiu
563400050527     c                   Write     Ta50pema1
563500050527     c                   EndIf
563600050527      * Stampo la riga per la richiesta delle e-mail
563700050527     c                   If        wema85 = *on or wema87 = *on or
563800090618     c                             wema88 = *on or wema84 = *On or
563900090618     c                             wema89 = *on
564000050527     c                   ExSr      Sr_Over
564100050527     c                   Write     Ta50pema2
564200050527     c                   EndIf
564300050527      * E-mail per bonifico bancario
564400050527     c                   If        wema85 = *on
564500050527     c                   ExSr      Sr_Over
564600050527     c                   Write     Ta50pema3
564700050527     c                   EndIf
564800050527      * E-mail per giacenze
564900050527     c                   If        wema88 = *on
565000050527     c                   ExSr      Sr_Over
565100050527     c                   Write     Ta50pema4
565200050527     c                   EndIf
565300050527      * E-mail per danni
565400050527     c                   If        wema87 = *on
565500050527     c                   ExSr      Sr_Over
565600050527     c                   Write     Ta50pema5
565700050527     c                   EndIf
565800051206      * E-mail per invio fattura
565900051206     c                   If        wema84 = *on
566000051206     c                   ExSr      Sr_Over
566100051206     c                   Write     Ta50pema7
566200051206     c                   EndIf
566300090618      * E-mail per sollecito pagamenti
566400090618     c                   If        wema89 = *on
566500090618     c                   ExSr      Sr_Over
566600090618     c                   Write     Ta50pema8
566700090618     c                   EndIf
566800050527      * consenso dei dati
566900050527     c                   Z-Add     curline       totrig
567000050527     c                   Add       4             totrig
567100050527     c                   ExSr      Sr_Overpiu
567200050527     c                   Write     Ta50pema6
567300050527
567400050527     c                   EndIf
567500050527
567600050527     c                   EndSr
567700041230
567800041230      *---------------------------------------------------------------*
567900041230      * SR_PSW stampa servizi ON LINE se offerta
568000041230      *---------------------------------------------------------------*
568100041230     c     SR_PSW        BegSr
568200041230
568300041230     c                   Eval      wpsw = *Off
568400041230
568500041230      * Se cliente nuovo
568600041230      * --> stampo sempre
568700041230if  1c                   If        VisCnw = 'S'
568800041230     c                   Eval      wpsw = *On
568900041230      * Se cliente non nuovo
569000041230      * --> stampo se non abilitato o se non è figlio di papà abilitato
569100041230   x1c                   Else
569200041230     c                   Move      *all'0'       vssksu
569300041230     c                   Move      visksc        vssksu
569400041230     c                   Eval      vssisv = 'TT'
569500041230     c     kTivss        Setll     Tivss02l
569600041230if  2c                   If        Not %Equal(Tivss02l)
569700041230     c                   Clear                   Tibs10ds
569800041230     c                   Eval      d10tle = 'WW'
569900041230     c                   Eval      d10paf = 'P'
570000041230     c                   Eval      d10cod = visksc
570100041230     c                   Eval      d10drf = *date
570200041230     c                   Call      'TIBS10R'
570300041230     c                   Parm                    Tibs10ds
570400041230if  3c                   If        d10err = *Blanks and d10cop > *Zeros
570500041230     c                   Move      *all'0'       vssksu
570600041230     c                   Move      d10cop        vssksu
570700041230     c                   Eval      vssisv = 'TT'
570800041230     c     kTivss        Setll     Tivss02l
570900041230     c                   If        Not %Equal(Tivss02l)
571000041230     c                   Eval      wpsw = *On
571100041230     c                   EndIf
571200041230e   3c                   EndIf
571300050112if  3c                   If        d10err <> *Blanks
571400050112     c                   Eval      wpsw = *On
571500050112e   3c                   EndIf
571600041230e   2c                   EndIf
571700041230e   1c                   EndIf
571800041230
571900041230     c                   EndSr
572000050527
572100050527      *---------------------------------------------------------------*
572200050527      * SR_EMA stampa delle e-mail da richiedere se offerta
572300050527      *---------------------------------------------------------------*
572400050527     c     Sr_Ema        BegSr
572500050527
572600050527     c                   Eval      wema85 = *Off
572700050527     c                   Eval      wema88 = *Off
572800050527     c                   Eval      wema87 = *Off
572900051206     c                   Eval      wema84 = *Off
573000090618     c                   Eval      wema89 = *Off
573100050527
573200091113      * visita
573300050527     c                   eval      notapl = 'V'
573400050527     c                   movel     i45kcc        notke1
573500050527     c                   move      parsks        notke1
573600091113      * trattativa
573700091113     c                   if        $trattativa
573800091113     c                   eval      notapl = 'T'
573900091113     c                   endif
574000050527     c                   Clear                   notke2
574100050527
574200050527      * e-mail bonifico bancario nota 85
574300050527     c                   If        §4uabi = 'S'
574400050527      * nota della visita
574500050527     c                   movel     '85'          nottnt
574600050527     c     keyntc        Chain     tfntc01l
574700050527     c                   If        %Found(tfntc01l)
574800050527     c                   Goto      nota88
574900050527     c                   EndIf
575000050527      * nota del cliente se non è un cliente nuovo
575100050527     c                   If        visksc > *Zeros
575200050527     c                   eval      notapl = 'C'
575300050527     c                   move      visksc        notke1
575400050527     c     keyntc        Chain     tfntc01l
575500050527     c                   If        %Found(tfntc01l)
575600050527     c                   Goto      nota88
575700050527     c                   EndIf
575800050527     c                   EndIf
575900050527      * nota non trovata
576000050527     c                   Eval      wema85 = *On
576100050527     c                   EndIf
576200050527
576300050527     c     nota88        Tag
576400091113      * visita
576500050527     c                   eval      notapl = 'V'
576600050527     c                   move      parsks        notke1
576700091113      * trattativa
576800091113     c                   if        $trattativa
576900091113     c                   eval      notapl = 'T'
577000091113     c                   endif
577100050527
577200050527      * e-mail giacenze nota 88
577300050527     c                   movel     '88'          nottnt
577400050527      * nota della visita
577500050527     c     keyntc        Chain     tfntc01l
577600050527     c                   If        %Found(tfntc01l)
577700050527     c                   Goto      nota87
577800050527     c                   EndIf
577900050527      * nota del cliente se non è un cliente nuovo
578000050527     c                   If        visksc > *Zeros
578100050527     c                   eval      notapl = 'C'
578200050527     c                   move      visksc        notke1
578300050527     c     keyntc        Chain     tfntc01l
578400050527     c                   If        %Found(tfntc01l)
578500050527     c                   Goto      nota87
578600050527     c                   EndIf
578700050527     c                   EndIf
578800050527      * nota non trovata
578900050527     c                   Eval      wema88 = *On
579000050527
579100050527     c     nota87        Tag
579200091113      * visita
579300050527     c                   eval      notapl = 'V'
579400050527     c                   move      parsks        notke1
579500091113      * trattativa
579600091113     c                   if        $trattativa
579700091113     c                   eval      notapl = 'T'
579800091113     c                   endif
579900050527
580000050527      * e-mail danni nota 87
580100050527     c                   movel     '87'          nottnt
580200050527      * nota della visita
580300050527     c     keyntc        Chain     tfntc01l
580400050527     c                   If        %found(tfntc01l)
580500090618     c                   Goto      nota84
580600050527     c                   EndIf
580700050527      * nota del cliente se non è un cliente nuovo
580800050527     c                   If        visksc > *Zeros
580900050527     c                   eval      notapl = 'C'
581000050527     c                   move      visksc        notke1
581100050527     c     keyntc        Chain     tfntc01l
581200050527     c                   If        %Found(tfntc01l)
581300051206     c                   goto      nota84
581400050527     c                   EndIf
581500050527     c                   EndIf
581600050527      * nota non trovata
581700050527     c                   Eval      wema87 = *On
581800051206
581900051206     c     nota84        Tag
582000091113      * visita
582100051206     c                   eval      notapl = 'V'
582200051206     c                   move      parsks        notke1
582300091113      * trattativa
582400091113     c                   if        $trattativa
582500091113     c                   eval      notapl = 'T'
582600091113     c                   endif
582700091113
582800051206      * e-mail invio fattura nota 84
582900051206     c                   movel     '84'          nottnt
583000051206      * nota della visita
583100051206     c     keyntc        Chain     tfntc01l
583200051206     c                   If        %found(tfntc01l)
583300090618     c                   Goto      nota89
583400051206     c                   EndIf
583500051206      * nota del cliente se non è un cliente nuovo
583600051206     c                   If        visksc > *Zeros
583700051206     c                   eval      notapl = 'C'
583800051206     c                   move      visksc        notke1
583900051206     c     keyntc        Chain     tfntc01l
584000051206     c                   If        %Found(tfntc01l)
584100090618     c                   Goto      nota89
584200051206     c                   EndIf
584300051206     c                   EndIf
584400051206      * nota non trovata
584500060203     c                   Eval      wema84 = *On
584600090618
584700090618     c     nota89        Tag
584800091113      * visita
584900090618     c                   eval      notapl = 'V'
585000090618     c                   move      parsks        notke1
585100091113      * trattativa
585200091113     c                   if        $trattativa
585300091113     c                   eval      notapl = 'T'
585400091113     c                   endif
585500090618
585600090618      * e-mail sollecito pagamenti 89
585700090618     c                   movel     '89'          nottnt
585800090618      * nota della visita
585900090618     c     keyntc        Chain     tfntc01l
586000090618     c                   If        %found(tfntc01l)
586100090618     c                   Leavesr
586200090618     c                   EndIf
586300090618      * nota del cliente se non è un cliente nuovo
586400090618     c                   If        visksc > *Zeros
586500090618     c                   eval      notapl = 'C'
586600090618     c                   move      visksc        notke1
586700090618     c     keyntc        Chain     tfntc01l
586800090618     c                   If        %Found(tfntc01l)
586900090618     c                   Leavesr
587000090618     c                   EndIf
587100090618     c                   EndIf
587200090618      * nota non trovata
587300090618     c                   Eval      wema89 = *On
587400050527
587500050527     c                   EndSr
587600050527
587700020110
587800020110      *------------------------------------------------------------------*
587900020110      * Sr_Over           GESTIONE STANDARD OVERFLOW
588000020110      *------------------------------------------------------------------*
588100020110
588200020110     c     Sr_Over       BEGSR
588300020110
588400010801     c                   if        *in80 or curline >= 61
588500020109     c                   write     ta50pfin
588600020109     c                   write     ta50pPP
588700010627     c                   setoff                                       80
588800010627     c                   endif
588900020110
589000010627     c                   endsr
589100020108
589200030730      *---------------------------------------------------------------*
589300030730      * SR_OVERPIU gestione standarad overflow con + 3 righe
589400030730      *---------------------------------------------------------------*
589500030730     C     SR_OVERPIU    BEGSR
589600030730      *
589700030730     c                   if        *in80 or totrig >= 61
589800030730     c                   write     ta50pfin
589900030730     c                   write     ta50pPP
590000030730     c                   setoff                                       80
590100030730     c                   endif
590200030730      *
590300030730     c                   endsr
590400030730
590500020108      *------------------------------------------------------------------*
590600020108      *   Staerr          STAMPA ERRORE
590700020108      *------------------------------------------------------------------*
590800020108
590900020108     c     Staerr        begsr
591000020108
591100050926      * se offerta valorizzo con il numero della visita
591200050926    2c                   if        *in98
591300050926     c                   eval      p07ksc = visksc
591400050926     c                   movel     tamksc        p07nrv
591500050926     c                   move      tamksc        p07prv
591600091117     c                   if        $trattativa
591700091117     c                   eval      p07ntra = tamksc
591800091117     c                   endif
591900050930      * tariffa
592000050926   x2c                   else
592100050926     c                   eval      p07ksc = tamksc
592200050926    2c                   endif
592300091117     c                   if        $trattativa
592400091117     c                   eval      ptratta =
592500091117     c                             '(codice ' +
592600091117     c                             %editc(tamctr:'X') + '/' +
592700091117     c                             %subst(%editc(tamprg:'X'):3:1) + ')'
592800091117     c                   endif
592900091117     c                   if        not $trattativa
593000091117     c                   eval      ptariffa =
593100091117     c                             '(codice ' +
593200091117     c                             %editc(tamctr:'X') + ')'
593300091117     c                   endif
593400050930
593500050930     c                   Write     ta50perr
593600050930     c                   If        woktar = *Off
593700050930     c                   Write     ta50p07
593800050930     c                   EndIf
593900050926
594000020108     c                   endsr
594100060925
594200060925      *---------------------------------------------------------------*
594300060925      * Imposto ATF lungo 7 nelle varie lingue
594400060925      *---------------------------------------------------------------*
594500060925     c     sr_atfk       begsr
594600060925
594700060925      * tariffa peso q.li 0
594800060925     c                   if        wctr1 = '0'
594900060925      * in lingua
595000060925     c                   select
595100060925     c                   when      i45lin = 'I'
595200060925     c                   eval      atf(k) = 'A Q.LE'
595300060925     c                   when      i45lin = '2'
595400060925     c                   eval      atf(k) = '100 KG'
595500060925     c                   when      i45lin = '3'
595600060925     c                   eval      atf(k) = '100 KG'
595700060925     c                   when      i45lin = '4'
595800060925     c                   eval      atf(k) = '100 KG'
595900060925     c                   endsl
596000060925     c                   endif
596100060925
596200060925      * tariffa colli 1
596300060925     c                   if        wctr1 = '1'
596400060925      * in lingua
596500060925     c                   select
596600060925     c                   when      i45lin = 'I'
596700060925     c                   eval      atf(k) = 'A COLLO'
596800060925     c                   when      i45lin = '2'
596900060925     c                   eval      atf(k) = 'P PARC.'
597000060925     c                   when      i45lin = '3'
597100060925     c                   eval      atf(k) = 'COLIS'
597200060925     c                   when      i45lin = '4'
597300060925     c                   eval      atf(k) = 'PRO PAK'
597400060925     c                   endsl
597500060925     c                   endif
597600060925
597700060925      * tariffa spedizione 2
597800060925     c                   if        wctr1 = '2'
597900060925      * in lingua
598000060925     c                   select
598100060925     c                   when      i45lin = 'I'
598200060925     c                   eval      atf(k) = 'A SPED.'
598300060925     c                   when      i45lin = '2'
598400060925     c                   eval      atf(k) = 'P SHIP.'
598500060925     c                   when      i45lin = '3'
598600060925     c                   eval      atf(k) = 'ENVOI'
598700060925     c                   when      i45lin = '4'
598800060925     c                   eval      atf(k) = 'PRO SEN'
598900060925     c                   endsl
599000060925     c                   endif
599100060925
599200060925      * tariffa peso kg 3
599300060925     c                   if        wctr1 = '3'
599400060925      * in lingua
599500060925     c                   select
599600060925     c                   when      i45lin = 'I'
599700060925     c                   eval      atf(k) = 'A KG.'
599800060925     c                   when      i45lin = '2'
599900060925     c                   eval      atf(k) = 'BY KG'
600000060925     c                   when      i45lin = '3'
600100060925     c                   eval      atf(k) = 'AU KG'
600200060925     c                   when      i45lin = '4'
600300060925     c                   eval      atf(k) = 'PRO KG'
600400060925     c                   endsl
600500060925     c                   endif
600600060925
600700060925      * tariffa a valore 4
600800060925     c                   if        wctr1 = '4'
600900060925      * in lingua
601000060925     c                   select
601100060925     c                   when      i45lin = 'I'
601200060925     c                   eval      atf(k) = 'A VAL.'
601300060925     c                   when      i45lin = '2'
601400060925     c                   eval      atf(k) = 'BY VALU'
601500060925     c                   when      i45lin = '3'
601600060925     c                   eval      atf(k) = 'VALEUR'
601700060925     c                   when      i45lin = '4'
601800060925     c                   eval      atf(k) = 'NACH WE'
601900060925     c                   endsl
602000060925     c                   endif
602100060925
602200060925      * tariffa a quantità 5
602300060925     c                   if        wctr1 = '5'
602400060925      * in lingua
602500060925     c                   select
602600060925     c                   when      i45lin = 'I'
602700060925     c                   eval      atf(k) = 'A Q.TA'
602800060925     c                   when      i45lin = '2'
602900060925     c                   eval      atf(k) = 'BY QUAN'
603000060925     c                   when      i45lin = '3'
603100060925     c                   eval      atf(k) = 'QUANT.'
603200060925     c                   when      i45lin = '4'
603300060925     c                   eval      atf(k) = 'NACH ME'
603400060925     c                   endsl
603500060925     c                   endif
603600060925
603700060925      * tariffa valore specifico V (usato nelle tariffe particolari)
603800060925     c                   if        wctr1 = 'V'
603900060925      * in lingua
604000060925     c                   select
604100060925     c                   when      i45lin = 'I'
604200060925     c                   eval      atf(k) = 'VAL.SPE'
604300060925     c                   when      i45lin = '2'
604400060925     c                   eval      atf(k) = 'SET VAL'
604500060925     c                   when      i45lin = '3'
604600060925     c                   eval      atf(k) = 'VAL.SPE'
604700060925     c                   when      i45lin = '4'
604800060925     c                   eval      atf(k) = 'SP.WERT'
604900060925     c                   endsl
605000060925     c                   endif
605100140724
605200140724      * questo è un chiodo per impostare 'A SPED.' quando lo richiede
605300140724      * il programma
605400140724     c                   if        wctr1 = ' '
605500140724      * in lingua
605600140724     c                   select
605700140724     c                   when      i45lin = 'I'
605800140724     c                   eval      atf(k) = 'A SPED.'
605900140724     c                   when      i45lin = '2'
606000140724     c                   eval      atf(k) = 'P SHIP.'
606100140724     c                   when      i45lin = '3'
606200140724     c                   eval      atf(k) = 'PAR ENV'
606300140724     c                   when      i45lin = '4'
606400140724     c                   eval      atf(k) = 'N.SEND'
606500140724     c                   endsl
606600140724     c                   endif
606700151118
606800151118      * tariffa a ritiro solo per tariffa particolare t Diritto di chiamata
606900151118      * ritiro telefonico
607000151118     c                   if        wctr1 = 'R'
607100151118      * in lingua
607200151118     c                   select
607300151118     c                   when      i45lin = 'I'
607400151118     c                   eval      atf(k) = 'A RIT.'
607500151118     c                   when      i45lin = '2'
607600151118     c                   eval      atf(k) = 'A RIT.'
607700151118     c                   when      i45lin = '3'
607800151118     c                   eval      atf(k) = 'A RIT.'
607900151118     c                   when      i45lin = '4'
608000151118     c                   eval      atf(k) = 'A RIT.'
608100151118     c                   endsl
608200151118     c                   endif
608300060925
608400060925     c                   endsr
608500060925
608600060925      *---------------------------------------------------------------*
608700060925      * Imposto la parte finale del numero per il lasciato avviso
608800060925      *---------------------------------------------------------------*
608900060925     c     sr_numgp      begsr
609000060925
609100060925     c                   select
609200060925      * italiano sempre ° fisso nel printer file il campo qua lo pulisco
609300060925     c                   when      i45lin = 'I'
609400060925     c                   clear                   comnavd
609500060925      * inglese cambiano i primi tre poi sempre th
609600060925     c                   when      i45lin = '2'
609700060925     c                   eval      comnavd = 'th'
609800060925     c                   if        comnav = 1
609900060925     c                   eval      comnavd = 'st'
610000060925     c                   endif
610100060925     c                   if        comnav = 2
610200060925     c                   eval      comnavd = 'nd'
610300060925     c                   endif
610400060925     c                   if        comnav = 3
610500060925     c                   eval      comnavd = 'rd'
610600060925     c                   endif
610700060925      * francese cambia solo il primo il resto sempre ieme
610800060925     c                   when      i45lin = '3'
610900060925     c                   eval      comnavd = 'ieme'
611000060925     c                   if        comnav = 1
611100060925     c                   eval      comnavd = 'er'
611200060925     c                   endif
611300060925      * tedesco sempre . fisso nel printer file il campo qua lo pulisco
611400060925     c                   when      i45lin = '4'
611500060925     c                   clear                   comnavd
611600060925     c                   endsl
611700060925
611800060925     c                   endsr
611900060925
612000020108      *------------------------------------------------------------------*
612100020108      *  Sr_Cartab        CARICAMENTO INIZIALE TABELLE
612200020108      *------------------------------------------------------------------*
612300020108
612400020108     c     Sr_Cartab     begsr
612500020110      *  carico la schiera di tutti i codici tassazione
612600020110     c                   clear                   K
612700020110     c                   clear                   K1
612800020110     c                   eval      cod = 'CT'
612900060724     c     Tabk2l        setll     tabel00f
613000020110     c                   DO        *hival
613100060724     c     Tabk2l        READE     Tabel00f
613200020110
613300020110     c                   If        %eof(tabel00f)
613400020110     c                   leave
613500020110     c                   endif
613600020110
613700020110     c                   IF        tblkey <> *blanks
613800020110     c                   add       1             K
613900020110     c                   movel     Tblkey        CCT(K)
614000020110     c                   movel     Tbluni        DSCT
614100020110     c                   move      §CTprv        SCT(K)
614200020110     c                   MOVE      §CTrap        RCT(K)
614300020110     c                   MOVE      §CTnar        NAR(K)
614400020110     c                   MOVE      §CTcor        DS6PR2
614500020110      * se è una regione carico la schiera delle regioni
614600020110     c                   IF        ds6pr2 = '9'
614700020110     c                   add       1             K1
614800020110     c                   movel     Tblkey        CRE(K1)
614900020110     c                   move      §CTdes        DRE(K1)
615000020110     c                   endif
615100020110
615200020110     c                   endif
615300020110
615400020110     c                   ENDDO
615500020110
615600020110      *  carico la schiera delle nazioni con quella ITALIA
615700020110     c                   z-add     1             K
615800020110     c                   eval      cod = '15'
615900060724     c     TABK2l        SETLL     TABEL00F
616000020110     c                   DO        *HIVAL
616100060724     c     TABK2l        READE     TABEL00F
616200020110
616300020110     c                   IF          %eof(tabel00f)
616400020110     c                   leave
616500020110     c                   ENDIF
616600020110
616700020110      * se non è annullato
616800020110     c                   IF        Tblflg = ' '
616900020110
617000020110     c                   Movel     tbluni        DS15
617100020110      * carico solo la nazione ITALIA
617200020110     c                   IF        §15ita = 'I'
617300020110     c                   Movel     Tblkey        NAZ(K)
617400020110     c                   add       1             K
617500020110     c                   endif
617600020110
617700020110     c                   endif
617800020110
617900020110     c                   ENDDO
618000020110
618100020110      * carico la schiera delle consegne particolari
618200020204      * solo quelle che servono per FedEx
618300020204      * flag §1PFFE <> 'N'
618400020110     c                   eval      cod ='1P'
618500020110     c                   z-add     0             K
618600060704     c     TABK2l        SETLL     TABEL00F
618700020110
618800020110     c                   DO        *hival
618900020110
619000060704     c     TABK2l        READE     TABEL00F
619100020110
619200020110     c                   if        %eof(tabel00f)
619300020110     c                   leave
619400020110     c                   endif
619500020110
619600020205      * escludo gli annullati
619700020205     c                   if        tblflg <> ' '
619800020205     c                   iter
619900020205     c                   endif
620000020205
620100020205     c                   movel     TBLUNI        DS1P
620200020205     c                   if        §1pffe <> 'N'
620300020110
620400020110      * carico tutte le consegne particolari
620500020110     c                   add       1             K
620600020110     c                   movel     tblkey        FTC(K)
620700020110     c                   movel     tbluni        DTC(K)
620800020110      * carico l'ordinamento in stampa
620900020110     c                   movel     §1post        OST(K)
621000020110      * carico l'ordinamento in stampa
621100020110     c                   movel     §1ptst        TST(K)
621200020205      * carico il flag x recupero descrizione FedEx
621300020204     c                   movel     §1pdfe        dfe(K)
621400020110
621500020110     c                   endif
621600020110
621700020110     c                   ENDDO
621800020110
621900020110      * carico la schiera delle estensioni consegne particolari
622000020110     c                   eval      cod ='SP'
622100020110     c                   z-add     0             K
622200060704     c     TABK2l        SETLL     TABEL00F
622300020110
622400020110     c                   DO        *hival
622500020110
622600060704     c     TABK2l        READE     TABEL00F
622700020110
622800020110     c                   if        %eof(tabel00f)
622900020110     c                   leave
623000020110     c                   endif
623100020110
623200020110     c                   IF        tblflg = ' '
623300020110     c                   movel     TBLUNI        DSSP
623400020110
623500020110     c                   movel     TBLKEY        comftc
623600020110
623700020110      * carico tutte le consegne particolari
623800020110     c                   z-add     1             K
623900020110     c                   setoff                                       69
624000020110     c     comftc        lookup    FTC(K)                                 69
624100020110     c                   if        *in69
624200020204      * se la descrizione è da recuperare flag §1PDFE = 'S'
624300020204      * imposto già edtc = §SPDFE
624400020204     c                   if        dfe(k) = 'S'
624500020204     c                   movel     §spdfe        edtc(k)
624600020204     c                   else
624700020110     c                   movel     §spdes        EDTC(K)
624800020204     c                   endif
624900020110     c                   movel     §spdac        EDAC(K)
625000020110     c                   movel     §spsud        ESUD(K)
625100020110     c                   endif
625200020110
625300020110     c                   endif
625400020110
625500020110     c                   ENDDO
625600020110      * ordino le tariffe particolari in base all'ordine in stampa
625700020110
625800020110     c                   movel     OST           WOS
625900020110     c                   sorta     wos
626000020110     c                   do        50            j
626100020110     c                   z-add     1             jj
626200020110     c                   setoff                                       69
626300020110     c                   if        wos(j) <> '99' and wos(j) <> *blank
626400020110     c     wos(j)        lookup    ost(jj)                                69
626500020110      * carico la schiera delle tariffe particolare in ordine di stampa
626600020110     c                   if        *in69 = *on
626700020110     c                   add       1             jjj
626800020110     c                   move      ftc(jj)       cp(jjj)
626900020110     c                   endif
627000020110     c                   endif
627100020110     c                   enddo
627200020107
627300020107      * aggancio la tabella delle variabili giacenza
627400020108     c                   eval      cod = '2G'
627500020108     c                   clear                   key
627600020108     c                   movel     '1'           key
627700020108     c     tabkey        chain     tabel00f
627800020108     c                   if        %found(tabel00f)
627900020108     c                   movel     tbluni        ds2g
628000020108     c                   else
628100020108     c                   clear                   ds2g
628200020108     c                   endif
628300021022      *
628400021023      * Richiamo FnLv41 per verificare/creare testo e reperire ds1h
628500021022     C                   clear                   Ds1H
628600021022     C                   move      *off          $NoStp            1
628700021022     C                   clear                   Fnlv41ds
628800021022     C                   movel     '3'           I41Mod
628900021022     C                   movel     I45TTX        I41Let
629000021022     C                   CALL      'FNLV41R'
629100021022     C                   PARM                    FNLV41DS
629200021022      *
629300021022      * Se non ho errori stampo
629400021023     C     o41err        ifne      'E'
629500021022     C                   movel     o41dta        DS1H
629600020107
629700020107      * stampo indirizzo di filiale
629800020108     c                   eval      *in92 = (§1Hfv1 = 'S')
629900020107      * stampo indirizzo cliente
630000020108     c                   eval      *in93 = (§1Hfv2 = 'S')
630100020107      * stampo alla C.A.
630200020108     c                   eval      *in94 = (§1Hfv4 = 'S')
630300020107      * stampo "Telefonando chiedere .........."
630400020108     c                   eval      *in95 = (§1Hfv5 = 'S')
630500020107      * stampo la data
630600020108     c                   eval      *in96 = (§1Hfv6 = 'S')
630700020107      * STAMPA RIFERIMENTI
630800020108     c                   eval      *in97 = (§1Hfv9 = 'S'and i45nr1 <> *blanks)
630900021022      *
631000021022     C                   ELSE
631100021022     C                   move      *on           $NoStp
631200021022     C                   ENDIF
631300031120
631400101015      /free
631500101015       //?Carico scatti ISTAT
631600101015         SISsca = 0;
631700101015         clear DIA;
631800101015         setll (SISsca) TISIS01L;
631900101015
632000101015         DOW  not $EoF;
632100101015
632200101015           read TISIS01L;
632300101015
632400101015           IF  %Eof(TISIS01L);
632500101015             $EoF = *on;
632600101015             leave;
632700101015           ENDIF;
632800101015
632900101015           DIA(SISsca) = SISdata;
633000101015
633100101015         ENDDO;
633200101015
633300101015      /end-free
633400020107
633500020108     c                   endsr
633600020107
633700020110      *------------------------------------------------------------------*
633800020110      *                   ROUTINE INIZIALE
633900020110      *------------------------------------------------------------------*
634000020108
634100020108     c     *inzsr        begsr
634200020108
634300020108     c     *entry        plist
634400020108     c                   parm                    kpjba
634500020108     c                   parm                    fnlv45ds
634600020108
634700020625     c                   z-add     1             codut             1 0
634800161222      * imposta coda di stampa
634900161222     c                   eval      coda_allegati= kqotd
635000161223      * apro il file di stampa
635100161223     c                   open      tnta50p
635200161222
635300020625
635400020625      * reperisco i dati utente
635500020625     c     *dtaara       define    §azute        AzuteDs
635600020625     c     *dtaara       define    §datiute      DDatiUte
635700020625
635800020625     c                   in(E)     *dtaara
635900020625     c                   IF        %Error or RSUT = *blanks
636000020625     c                   call      'TIBS34R'
636100020625     c                   parm                    Tibs34Ds
636200020625     c                   in        *dtaara
636300020625     c                   ENDIF
636400110517
636500110517      * chiodo fisso 'BRT S.p.A.'
636600110517     c                   eval      rsutold = 'BRT S.p.A.'
636700020919      *
636800020919     C                   MOVEL     SIMFEL        simfelA           3
636900060704
637000060704      * lingua per tabelle
637100060704     c                   clear                   kcodut            1 0
637200060704     c                   select
637300060704     c                   when      i45lin = 'I'
637400060704     c                   eval      kcodut = 1
637500060704     c                   when      i45lin = '2'
637600060704     c                   eval      kcodut = 2
637700060712     c                   when      i45lin = '3'
637800060704     c                   eval      kcodut = 3
637900060704     c                   when      i45lin = '4'
638000060704     c                   eval      kcodut = 4
638100060704     c                   endsl
638200020625
638300020107      * preparo il campo PAGE per la numerazione delle pagine
638400020107     c                   eval      page = 1
638500020107      * carico le tabelle
638600020107     c                   exsr      Sr_Cartab
638700021022      *
638800021022      * proseguo se ho i dati per uso testi
638900021022     c                   if        $NoStp = *off
639000020108
639100020107      * TARIFFA O OFFERTA
639200020108
639300020108      * --- O F F E R T A ---
639400020108
639500110104     c**                 if        i45ast = 'V' or i45ast = 'T'
639600110104     c                   if        i45ast = 'T'
639700020108      * accendo il 98
639800020108     c                   seton                                        98
639900091113      * se trattativa imposto flag
640000091113     c                   eval      $trattativa = (i45ast = 'T')
640100091117     c                   eval      *in85 = (i45ast = 'T')
640200020108      * valorizzo il campo del codice cliente tariffa con il numero visita
640300020108     c                   eval      parsks = i45nrv
640400020108      * apro i file visita/offerta
640500020108     c                   open      tfaco00f
640600020108     c                   open      tfind00f
640700020108     c                   open      tfclp00f
640800020108     c                   open      tnofm01l
640900061011     c                   open      tiofd01l
641000020108     c                   open      tiopt01l
641100020108     c                   open      tiopd01l
641200020108     c                   open      tiogc01l
641300110104     c**                 if        i45ast = 'V'
641400110104     c**                 open      tnvis05l
641500110104     c**                 endif
641600091022     c                   if        i45ast = 'T'
641700091022     c                   open      Tivis05l
641800091022     c                   endif
641900020108      *  i45ast = 'C'
642000020108     c                   else
642100020108
642200020108      * --- T A R I F F A ---
642300061011      * apro i file visita/offerta
642400061011     c                   open      tntam01l
642500061011     c                   open      titad04l
642600061011     c                   open      titpt01l
642700061011     c                   open      titpd01l
642800061011     c                   open      titgc01l
642900020108
643000020108      * valorizzo il campo del codice cliente tariffa
643100020108     c                   eval      parsks = i45ksc
643200020108
643300020108     c                   endif
643400021022     c                   endif
643500030508
643600030508      * Prendo i dati di dft per le tariffe FedEx
643700030508     c                   clear                   ddfe
643800030508     c                   clear                   tibs02ds
643900030508     c                   movel     'C'           t02mod
644000030508     c                   movel     knsif         t02sif
644100030508     c                   movel     'DFE'         t02cod
644200030508     c                   movel(p)  'FED'         t02ke1
644300030508     c                   call      'TIBS02R'
644400030508     c                   parm                    kpjba
644500030508     c                   parm                    tibs02ds
644600030508     c                   if        t02err = *blanks
644700030508     c                   movel     t02uni        ddfe
644800030508     c                   endif
644900030508
645000020108      *------------------------------------------------------------------*
645100020108      *               DEFINIZIONI DELLE CHIAVI
645200020108      *------------------------------------------------------------------*
645300020108
645400020108      * --- TABEL00F ---
645500020108     c     tabkey        klist
645600020108     c                   kfld                    codut
645700020108     c                   kfld                    cod
645800020108     c                   kfld                    key
645900060704
646000060704      * accesso per codice su TABEL00F in lingua
646100060704     c     tabkeyl       klist
646200060704     c                   kfld                    kcodut
646300060704     c                   kfld                    cod
646400060704     c                   kfld                    key
646500060704
646600060704      * accesso per codice su TABEL00F  parziale in lingua
646700060704     c     tabk2l        klist
646800060704     c                   kfld                    kcodut
646900060704     c                   kfld                    cod
647000020108      * --- TNTAM01L/TNOFM01L ---
647100020108     c     ds1ktm        klist
647200020108     c                   kfld                    ds1ksc
647300020108     c                   kfld                    ds1ctr
647400020108     c                   kfld                    ds1prg
647500020108      * --- TITAD04L ---
647600020108     c     ds1ktd        klist
647700020108     c                   kfld                    ds1ksc
647800020108     c                   kfld                    ds1ctr
647900020108     c                   kfld                    ds1prg
648000020108     c                   kfld                    comfil
648100020108
648200020108      * --- TITPT e TITPD ---
648300020108     c     ktpt          klist
648400020108     c                   kfld                    ds1ksc
648500020108     c                   kfld                    ds1ctr
648600020108     c                   kfld                    ds1prg
648700020108     c                   kfld                    savftc
648800020108      * --- TITPD ---
648900020108     c     ktpd          klist
649000020108     c                   kfld                    ds1ksc
649100020108     c                   kfld                    ds1ctr
649200020108     c                   kfld                    ds1prg
649300020108     c                   kfld                    savftc
649400020108     c                   kfld                    savorp
649500020108
649600020108     c     ktpd2         klist
649700020108     c                   kfld                    ds1ksc
649800020108     c                   kfld                    ds1ctr
649900020108     c                   kfld                    ds1prg
650000020108     c                   kfld                    savftc
650100020108     c                   kfld                    savorp
650200020108     c                   kFLD                    savsgl
650300020108
650400020108     c     ktpdwrk       klist
650500020108     c                   kfld                    ds1ksc
650600020108     c                   kfld                    ds1ctr
650700020108     c                   kfld                    ds1prg
650800020108     c                   kfld                    wrkftc
650900020108     c                   kfld                    wrkorp
651000020108     c                   kfld                    wrksgl
651100020108      * --- CNACO / TFACO ---
651200020108     c     ds7kac        klist
651300020108     c                   kfld                    codut
651400020108     c                   kfld                    i45kcc
651500020108     c                   kfld                    ksc
651600020108      * --- TFNTC ---
651700020108     c     keyntc        klist
651800020108     c                   kfld                    notapl
651900020108     c                   kfld                    notke1
652000020108     c                   kfld                    notke2
652100020108     c                   kfld                    nottnt
652200041230      * accesso TIVSS02L
652300041230     c     kTivss        klist
652400041230     c                   kfld                    vssksu
652500041230     c                   kfld                    vssisv
652600080211
652700080211     c     kCnabi        Klist
652800080211     c                   Kfld                    pcabi
652900080211     c                   Kfld                    pccab
653000020108
653100020108     c                   endsr
653200010530**   MSG
653300020108       =====>  TARIFFA/OFFERTA NON STAMPABILE IN MODO CORRETTO  <=====        1
653400020108                                                                              2
653500020108CLIENTE NON CODIFICATO O ANNULLATO                                            3
653600020108NESSUNA RIGA DI DETTAGLIO X QUESTA TARIFFA/OFFERTA                            4
653700020108PIU' DI 200 SCAGLIONI X CODICE DI TASSAZIONE                                  5
653800010530CODICE TASSAZIONE NON TROVATO !!!!!!!!!!!      CUMELA MO' ?                   6
653900050616CODICE REGIONE DEL CODICE DI TASSAZIONE NON ESISTE IN TABELLA                 7
654000010530UN CODICE DI TASSAZIONE DELLA TARIFFA HA UNO SCAGLIONE NON PREVISTO           8
654100020109                                                                              9
654200050616PIU' DI 15 PROVINCIE X 1 REGIONE? TABELLA ERRATA O  T N T P D  NON ALLINEATO  10
654300050616                                                                              1
654400020109                                                                              2
654500010530XXXXXXXXXXXXXXXXXXXX    *** ERRORI IN STAMPA TARIFFA/OFFERTA ***              3
654600010530UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON PREVISTO   4
654700050616PIU' DI 18 SCAGLIONI X CODICE DI TASSAZIONE PER LE TARIFFE PARTICOLARI        5
654800020116PIU' DI 7 CODICI DI TASSAZIONE PER LINEA DI PARTENZA                          6
654900050324UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON VALIDO     7
