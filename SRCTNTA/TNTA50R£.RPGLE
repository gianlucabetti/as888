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
005600020108
005700020108      *------------------------------------------------------------------*
005800020108      *                 S C H I E R E
005900020108      *------------------------------------------------------------------*
006000020107      * Messaggi di errore
006100050324     d msg             s             78    dim(17) ctdata perrcd(1)
006200020108      * Note
006300020108     d note            s             78    dim(4)
006400020108      * Scaglioni
006500020109     d sgl             s              7  2 dim(200)
006600020110      * Importo
006700020116     d itr             s              7  3 dim(7)
006800020109      * Zona tariffaria
006900020116     d  cts            s              2    dim(7)
007000020109      * Descrizione zona tariffaria
007100020116     d dcts            s             10    dim(7)
007200020110
007300020111      *------------------------------------------------------------------*
007400020111      *  TABELLE PROVINCIE E REGIONI
007500041220     D CCT             S              2    DIM(600)
007600041220     D SCT             S              2    DIM(600)
007700041220     D RCT             S              2    DIM(600)
007800041220     D NAR             S              3    DIM(600)
007900041220     D CRE             S              2    DIM(200)
008000041220     D DRE             S             20    DIM(200)
008100020110     D NAZ             S              3    DIM(20)
008200020111      * SCHIERE PER TARIFFE PARTICOLARI
008300020110     D FTC             S              1    DIM(50)
008400020110     D DTC             S             25    DIM(50)
008500020204     d dfe             s              1    dim(50)
008600020111      * ordinamento in stampa tariffe particolari
008700020110     D OST             S              2    DIM(50)
008800020110     D WOS             S              2    DIM(50)
008900020110     D TST             S              1    DIM(50)
009000020110     D CP              S              1    DIM(50)
009100020111      * estensione tabella 1P per stampa
009200020110     D EDTC            S             31    DIM(50)
009300020110     D EDAC            S              1    DIM(50)
009400020110     D ESUD            S              1    DIM(50)
009500020110
009600020111      *--------------- SCHIERE PER LA STAMPA -----------------
009700020111      *"/" di divisione negli scaglioni da a
009800020110     D SGLB            S              1    DIM(18)
009900020111      * SCAGLIONI senza decimali
010000020110     D SGLD            S              4  0 DIM(18)
010100020110     D SGLA            S              4  0 DIM(18)
010200020111      * SCAGLIONI con 1 decimale
010300020110     D SG1D            S              3  1 DIM(18)
010400020110     D SG1A            S              3  1 DIM(18)
010500020111      * SCAGLIONI con 2 decimali
010600020110     D SG2D            S              4  2 DIM(18)
010700020110     D SG2A            S              4  2 DIM(18)
010800020111      * DECODIFICA SOTTO GLI SCAGLIONI
010900020110     D ATF             S              7    DIM(18)
011000020111      * ------------------- 6 SCAGLIONI PER LA STAMPA --------------------------
011100020110     D SSLB            S              1    DIM(6)
011200020111      * SCAGLIONI SENZA DECIMALI
011300020110     D SSLD            S              4  0 DIM(6)
011400020110     D SSLA            S              4  0 DIM(6)
011500020111      * SCAGLIONI CON 1 DECIMALI
011600020110     D SS1D            S              3  1 DIM(6)
011700020110     D SS1A            S              3  1 DIM(6)
011800020111      * SCAGLIONI CON 2 DECIMALI
011900020110     D SS2D            S              4  2 DIM(6)
012000020110     D SS2A            S              4  2 DIM(6)
012100020111      * ------------------- 6 APPLICAZIONE TARIFFA FINITA ----------------------
012200020110     D STF             S              7    DIM(6)
012300020111      * ------------------- 6 SCAGLIONI E DETTAGLI TARIFFARI -------------------
012400020110     D SSL             S              7  2 DIM(6)
012500020110     D TCT             S              7  3 DIM(6)
012600020110     D TMN             S              7  3 DIM(6)
012700020110     D TMX             S              7  3 DIM(6)
012800020110     D TIN             S              7  3 DIM(6)
012900020111      * ------------------- SCHIERA DI COMODO ----------------------------------
013000020110     D COM             S              7    DIM(6)
013100020111      *  SKI TARIFFE X SCAGL. DI OGNI PROVINCIA(CTS) DELLA REGIONE
013200050616     D BIG             S             42    DIM(15)
013300050616     D BMN             S             42    DIM(15)
013400050616     D BMX             S             42    DIM(15)
013500050616     D BIN             S             42    DIM(15)
013600050616     D BSI             S              2    DIM(15)
013700050616     D BRG             S              1    DIM(15)
013800020111      * SKIERE DI WORK
013900020110     D WIN             S             42    DIM(30)
014000020110     D WMN             S             42    DIM(30)
014100020110     D WMX             S             42    DIM(30)
014200050616     D WCT             S              3    DIM(15)
014300020110     D WTI             S             42    DIM(30)
014400020111      * SKIERE DI STAMPA
014500020110     D PDE             S             29    DIM(30)
014600020110      *-------------------------------------------------------------------------
014700020110      *         SCHIERE SCAGLIONI DETTAGLIO TARIFFE PARTICOLARI
014800020110      *-------------------------------------------------------------------------
014900020111      * --------------  SCHIERE TARIFFE IN DETTAGLIO ------------------------
015000020111      * SCAGLIONI
015100020111
015200020110     D SCP             S              7  2 DIM(18)
015300020111      * TARIFFA PARTICOLARE DETTAGLIO
015400020110     D TAP             S              7  3 DIM(6)
015500020110     D TPN             S              7  3 DIM(6)
015600020110     D TPX             S              7  3 DIM(6)
015700020110     D AIP             S              1    DIM(6)
015800020111
015900020110     D PER             S              1    DIM(6)
016000020111
016100031120     D*-------------------------------------------------------------------------
016200101015      * DATE SCATTI ISTAT
016300101015     D DIA             S              8  0 DIM(999)
016400020108
016500020108      *------------------------------------------------------------------*
016600020108      *                 D S  M U L T I P L E
016700020108      *------------------------------------------------------------------*
016800020110     d sglitr          ds                  occurs(200) inz
016900020109     d  sglp                          7  2
017000020110     d  itr1                          7  3
017100020110     d  itr2                          7  3
017200020110     d  itr3                          7  3
017300020110     d  itr4                          7  3
017400020110     d  itr5                          7  3
017500020110     d  itr6                          7  3
017600020116     d  itr7                          7  3
017700020108
017800021205      *          SCHIERE PER STAMPA TARIFFE PARTICOLARI SU + RIGHE
017900021205     d TptA            s             31    Dim(50)
018000021205     d TptB            s             25    Dim(50)
018100021205     d Tptc            s             31    Dim(50)
018200020107      *-------------------------------------------------------------------------
018300020107      * DS DEL FILE TABEL00F
018400020107     D DSCV          E DS
018500020107     D DS1H          E DS
018600020107     D DS1P          E DS
018700020110     D DSSP          E DS
018800020107     D DS2G          E DS
018900020110     D DSCT          E DS
019000020107     D DS15          E DS
019100020107     D DSTR          E DS
019200020107     D DSFA          E DS
019300020111     D DS4U          E DS
019400020107      * DS DEL FILE TNTBE00F
019500020107     D DTAA          E DS
019600020107      * VALORI STANDARD AZIENDALI
019700020107     D DGEI          E DS
019800030508      * Dati dft tariffe FedEx
019900030508     d dDfe          e ds
020000020107      * DS flag operativi del file tariffe
020100020107     D DSTA01        E DS
020200020625      *--------
020300020625      * Ds di riferimento al file esterno AzUte00F
020400020625      *--------
020500020625     d AzuteDs       e ds                  ExtName(AzUte00F)
020600020625      *--------
020700020625      * Ds per dati organigramma
020800020625      *--------
020900020625     d DDatiUte      e ds
021000020625      *--------
021100020625      * Parametri per richiamo al pgm di controllo profilo utenti
021200020625      *--------
021300020625     d Tibs34Ds      e ds                  Inz
021400020625
021500020111
021600020107     D KPJBA         E DS
021700020107     D  utente                 1      5
021800020111
021900020107     D TIBS02DS      E DS
022000020111
022100020107     D FNLV45DS      E DS
022200021022     D fnlv41ds      E DS
022300041230     d Tibs10ds      e ds
022400020111
022500020107      *-------------------------------------------------------------------------
022600020107      * ds interne
022700020111
022800020107     D CDPKEY          DS
022900020107     D  CDPG                   1      3
023000020107     D  GGPG                   2      3
023100020107     D  CDLIN                  4      4
023200020111
023300020107     D PRTTAR          DS
023400020107     D  curline              367    368I 0
023500020111
023600020107      *-------------------------------------------------------------------------
023700020107     D DATA_eur        S               D   DATFMT(*eur)
023800020111
023900020111      *-------------------------------------------------------------------------
024000020109     d rotcts          s                   like(tadcts)
024100020109     d rotlnp          s                   like(tadlnp)
024200020109     d ctrata          s                   like(§trata)
024300020110     d wrkata          s                   like(tamata)
024400020109
024500030730     d totrig          s              5  0
024600020109     d unoctr          s              1  0
024700020109     d k               s              3  0
024800020109     d x               s              2  0
024900051006     d xx              s              2  0
025000051006     d yy              s              2  0
025100051006     d wsgl            s              7  2
025200020109     d wsgl70          s              7  0
025300020109     d wsgl71          s              7  1
025400020109     d wsgl72          s              7  2
025500020109     d wsgl70d         s              7  0
025600020109     d wsgl71d         s              7  1
025700020109     d wsgl72d         s              7  2
025800020109     d wsgl70a         s              7  0
025900020109     d wsgl71a         s              7  1
026000020109     d wsgl72a         s              7  2
026100020110     d sav_sglp        s              7  2
026200020109     d w0020           s              2  0
026300020109     d w0030           s              3  0
026400020109     d w0050           s              5  0
026500060726     d len             s              5u 0
026600080211     d wagenzia        s             16
026700080211     d wbanca          s             20
026800080211     d wpos            s              2  0
026900080211     d wpos1           s              2  0
027000080211     d wpos2           s              2  0
027100020111
027200020111      *-------------------------------------------------------------------------
027300130905     d wCMMcod         s                   like(CMMcod)   inz
027400060801     d blanks          s                   like(destar)
027500010601     d cod             s                   like(tblcod)
027600010625     d costpg          s                   like(§fatpg)
027700010531     D ds1ksc          s                   like(tamksc)
027800010531     D ds1ctr          s                   like(tamctr)
027900010531     D ds1prg          s                   like(tamprg)
028000010601     D notapl          s                   like(ntcapl)
028100010621     D notke1          s                   like(ntcnk1)
028200010621     D notke2          s                   like(ntcnk2)
028300010601     D nottnt          s                   like(ntctnt)
028400010725     D kcod            s                   like(tbecod) inz('TAA')
028500010601     D key             s                   like(tblkey)
028600010601     D ksc             s                   like(acoksc)
028700010724     D oltre           s                   like(tpdsgl)
028800010531     D parsks          s                   like(tamksc)
028900060313     d savapl          s                   like(tptapl)
029000010618     D savftc          s                   like(tptftc)
029100010621     D savorp          s                   like(tpdorp)
029200010801     D savorp1         s                   like(tpdorp)
029300010621     D savsgl          s                   like(tpdsgl)
029400010806     D recsgl          s                   like(tpdsgl)
029500010604     D wrkcts          s                   like(tadcts)
029600010626     D wrkftc          s                   like(tpdftc)
029700010626     D wrkorp          s                   like(tpdorp)
029800010626     D wrksgl          s                   like(tpdsgl)
029900110127     D wrpv            s                   like(tadrpv)
030000010531      *-------------------------------------------------------------------------
030100020111     D a               s              2  0
030200021205     d atp1            s              2  0
030300021205     d atpc            s              2  0
030400010614     D b               s              2  0
030500021205     D blk10           s             10    INZ(*ALL'.')
030600030318     D blk12           s             12    INZ(*ALL'.')
030700030318     D blk21           s             21    INZ(*ALL'.')
030800030318     D blk25           s             25    INZ(*ALL'.')
030900130905     ***D blk36           s             36    INZ(*ALL'.')
031000021205     D blk56           s             56    INZ(*ALL'.')
031100010614     D c               s              2  0
031200021205     d CntList         s              5i 0 Inz(*Zeros)
031300010615     D codorp          s              1
031400010615     D codreg          s              2
031500010615     D codsig          s              2
031600010601     D comfil          s              3  0
031700010628     D comftc          s              1
031800010613     D compro          s              1
031900010613     D comreg          s              2
032000010621     D com04           s              4
032100010614     D como29          s             29
032200010614     D como42          s             42
032300021205     d conta           s              1  0
032400010614     D cmno42          s             42
032500010614     D cmxo42          s             42
032600010601     D datastampa      s             10
032700010723     D desctr          s             13
032800010615     D desreg          s             14
032900010618     D de1tpg          s              9
033000010618     D de3tpg          s             13
033100010801     D dissca2         s             63
033200010801     D dissca3         s             50
033300010801     D dissca4         s             37
033400010801     D dissca5         s             24
033500010801     D dissca6         s             11
033600010608     D ds3um           s             20
033700010531     D ds6pr2          s              1
033800010621     D errmsg          s             78
033900010614     D h               s              2  0
034000020110     D j               s              3  0
034100020110     D jj              s              3  0
034200020110     D jjj             s              3  0
034300010724     D mintas          s              3  0
034400010619     D num4            s              4  0
034500010614     D p               s              2  0
034600010531     D k1              s              3  0
034700010618     D k2              s              3  0
034800010607     D k4              s              3  0
034900010622     D k5              s              3  0
035000010604     D Y               s              3  0
035100010625     D recval          s              3  0
035200010604     D rotpro          s              1
035300010613     D rotreg          s              2
035400010531     D staddt          S             10
035500010531     D stadst          S             10
035600020624     d stalnp          s             20
035700010725     D stpanno         s              4  0
035800010725     D stpmod          s              3  0
035900030318     D stpper          s              7
036000010731     D unmis           s              3
036100030318     D valper          s              6  3
036200010615     D waro2           s              4  0
036300010615     D warf2           s              4  0
036400010615     D warl2           s              4  0
036500140724     d wata            s              5  0
036600140724     d wata1           s              6  1
036700140724     d wata2           s              7  2
036800051206     d wema84          s              1    Inz('0')
036900050527     d wema85          s              1    Inz('0')
037000050527     d wema87          s              1    Inz('0')
037100050527     d wema88          s              1    Inz('0')
037200090618     d wema89          s              1    Inz('0')
037300021205     d Witr            s              8  3
037400010618     D wnumsc          s              2  0
037500050930     d woktar          s              1    Inz('0')
037600021205     d WokTp           s              1    Inz('0')
037700041230     d wpsw            s              1    Inz('0')
037800010625     D w_per           s              1
037900010626     D wrkdestpg       s             13
038000010604     D wrkreg          s              2
038100010604     D wrkpro          s              1
038200011001     D w_ellimris      S              1
038300021205     D w_tarpar        S             83
038400021205     D w_tarparest     S             83
038500010723     D w0010           S              1  0
038600010601     D w003a           S              3
038700060925     d wctr            s              2  0
038800060925     d wctr1           s              1
038900050201     d woklnp          s              1    inz(*off)
039000050201     d wokstp          s              1    inz(*off)
039100091112     d $trattativa     s              1n
039200101015     d $EoF            s              1n   inz(*off)
039300110127     d Wvolta          s              1n   inz(*off)
039400021205      *-----------------
039500021205      * PARAMETRI DI PASSAGGIO AL PROGRAMMA DI "GIUSTIFICAZIONE" TESTO
039600021205      *-----------------
039700021205     d TxtInput        S            200    varying
039800021205     d Lungh           S              5i 0 inz(105)
039900021205     d TxtGiust        S            200    varying
040000021205     d TxtResto        S            200    varying
040100020111
040200020111      * DEFINIZIONE COSTANTI
040300060726     D CSPE1           C                   CONST('solo sulle spese di sosta')
040400010625     D CSPE2           C                   CONST('su tutte le spese')
040500060726     D CSPE3           C                   CONST('su spese sosta, dossier, -
040600060726     D                                     varie')
040700060726     D CSPE12          C                   CONST('only on holding costs')
040800060704     D CSPE22          C                   CONST('on all costs')
040900060726     D CSPE32          C                   CONST('on holding, dossier, -
041000060726     D                                     various costs')
041100060726     D CSPE13          C                   CONST('uniquement sur les frais -
041200060726     D                                     d''arrêt')
041300060726     D CSPE23          C                   CONST('sur tous les frais')
041400060726     D CSPE33          C                   CONST('sur les frais d''arrêt, de -
041500060726     D                                     dossier, divers')
041600060726     D CSPE14          C                   CONST('nur auf Kosten für -
041700060726     D                                     Aufbewahrung')
041800060726     D CSPE24          C                   CONST('auf alle Kosten')
041900060726     D CSPE34          C                   CONST('uf Kosten für Aufbewahrung,-
042000060726     D                                     Bearbeitung,Verschiedenes')
042100091022
042200091022     itivis000
042300091022     i              visesi                      wisesi
042400091022
042500020108      *------------------------------------------------------------------*
042600020108      *               RIEPILOGO INDICATORI
042700020108      *------------------------------------------------------------------*
042800020111      * 08    - STAMPA MINIMO TASSABILE PER TARIFFA A Q.LE
042900020111      * 09    - TIPO PAGAMENTO A VALORE (TARIFFA %) TARIFFE PARTICOLARI
043000020111      * 18    - STAMPA ARROTONDAMENTI
043100020111      * 19    - STAMPA RAPPORTO PESO VOLUME
043200020111      * 21    - MANDATO A VALORE VARIABILE
043300020111      * 22    - ELEVAZIONE LIMITE RISARCIBILE
043400020111      * 24    - MANDATO FITTIZIO
043500020111      * 26    - CONDIZIONA LA STAMPA DEL "PER" DI FIANCO ALLA VALIDITA' TARIFFA
043600041129      * 27    - CONDIZIONA LA STAMPA DELL'ADDIZIONALE DI GESTIONE 1° riga
043700020111      * 28    - CONDIZIONA LA STAMPA DELLE SPESE GIACENZA VARIE A SPEDIZIONE
043800041129      * 29    - CONDIZIONA LA STAMPA DELL'ADDIZIONALE DI GESTIONE 2° riga
043900020111      * 31    - INDICATORE DI COMODO
044000020109      * 35    - INDICATORE DI COMODO LOOKUP
044100020108      * 37    - SCAGLIONI CON UN SOLO DECIMALE
044200020111      * 40/46 - CONDIZIONA LA STAMPA DEGLI SCAGLIONI
044300020111      * 48    - STAMPO IL MINIMO
044400020111      * 49    - STAMPO IL MASSIMO
044500020110      * 50    - CONDIZIONA LA STAMPA DEL 'OLTRE' NEGLI SCAGLIONI
044600020111      * 51/56 - CONDIZIONA LA STAMPA DEGLI SCAGLIONI VUOTI
044700020111      * 57    - STAMPO INOLTRO
044800020111      * 61/66 - CONDIZIONA LA STAMPA DEL DETTAGLIO TARIFFARIO
044900020111      * 72    - CONDIZIONA LA STAMPA DELL'ADDEBITO SPESE DI INCASSO
045000020108      * 73    - CONDIZIONA LA STAMPA DEL RESPONSABILE TRASPORTI
045100020111      * 75    - STAMPO MINIMO
045200020108      * 77    - CONDIZIONA LA STAMPA DEL E.P.C.
045300020109      * 78/79 - CONDIZIONA LA STAMPA DELLA TESTATA DEGLI SCAGLIONI
045400020109      * 80    - OVERFLOW
045500020111      * 81    - CONDIZIONA LA STAMPA DELLA TESTATA ASSICURAZIONI MERCE
045600091117      * 85    - Trattativa in stampa
045700020111      * 86    - STAMPO MASSIMO
045800020108      * 87    - TARIFFA A VALORE
045900020111      * 88    - ARRONDAMENTI CON DECIMALI
046000020108      * 91    - SE ACCESO SCAGLIONI CON DECIMALI
046100020111      * 92/97 - STAMPA DATI DALLA DS1H
046200020108      * 98    - ON PROVENGO DA OFFERTA
046300020108      * 99    - INDICATORE GENERALE DI ERRORE
046400020111      *****************************************************************
046500020108
046600020108      *------------------------------------------------------------------*
046700010531      *    T E S T A T A    S T A M P A    T A R I F F A
046800020108      *------------------------------------------------------------------*
046900021022      *
047000021022      * Per errore di reperimento tetso vado a fine pgm
047100021022     C                   if        $NoStp   = *on
047200021022     C                   goto      fine
047300021022     C                   endif
047400021022      *
047500050930     c                   eval      woktar = *Off
047600020108
047700020108      * Recupero il cliente
047800020108     c                   exsr      Sr_Reccli
047900020108
048000020108      * Se 99 acceso errore e vado a fine PGM non trovato cliente in anafragica
048100020108
048200020108     c                   if        *in99
048300020108     c                   goto      fine
048400020108     c                   endif
048500020108
048600020108      * Decodifico i dati del cliente
048700020108
048800020108     c                   exsr      Sr_Decana
048900020108
049000020108      * Recupero note tariffa/offerta cliente e responsabile trasporti
049100020108
049200020108     c                   exsr      Sr_Recnot
049300020108
049400020108      * Decodifico i dati del punto operativo
049500020108
049600020108     c                   exsr      Sr_Decpo
049700020108
049800020108      * aggancio testata tariffa / offerta
049900020108     c                   eval      ds1ksc = parsks
050000020108     c                   eval      ds1prg = i45prg
050100020108     c                   eval      ds1ctr = i45ctr
050200020108
050300061011     c  n98ds1ktm        chain     tntam000
050400061011     c   98ds1ktm        chain     tnofm000
050500020108
050600020108      * decodifica delle condizioni generali
050700020108
050800020108     c                   exsr      Sr_Dectam
050900020108
051000020108      * stampo la prima parte della tariffa
051100020108
051200020107     c                   write     ta50pPP
051300020107     c                   write     ta50p01
051400020108
051500020108      * elaboro dettaglio tariffario per recuperare scaglioni
051600020108
051700020108     c                   exsr      Sr_Scaglioni
051800020109      * se si sono vericati errori non continuo
051900020109     c                   if        *in99
052000020109     c                   goto      fine
052100020109     c                   endif
052200020108
052300020108      * elaboro e stampo il dettaglio tariffario
052400020108
052500020108     c                   exsr      Sr_Eladet
052600020108      * se si sono vericati errori non continuo
052700020108     c                   if        *in99
052800020108     c                   goto      fine
052900020108     c                   endif
053000020110
053100020110      * stampo arrotondamenti
053200020110     c                   exsr      Sr_Arroto
053300020110
053400020110      * stampo i dati della testata tariffa
053500020110     c                   exsr      Sr_Statam
053600050930     c                   eval      woktar = *On
053700020110
053800020110      * routine gestione tariffe particolari
053900020110     c                   exsr      Sr_Statpt
054000021205
054100021205      * routine stampa sk unica tariffe particolari
054200021205     c                   ExSr      Sr_StaTpt1
054300020110
054400020110      * routine di gestione tariffe giacenza
054500020110     c                   exsr      Sr_Statgc
054600020110
054700020110      * stampo condizioni di pagamento
054800020110     c                   exsr      Sr_Over
054900020110     c                   write     ta50pcon
055000020110
055100020110      * se pagamento che utilizza la banca stampo anche questa riga
055200091016      * pagamento RIBA = '1' RID = '2'
055300091016     c***                if        costpg <> '0'
055400091016     c                   if        costpg = '1' or costpg = '2'
055500020110     c                   exsr      Sr_Over
055600020110     c                   write     ta50pcon1
055700020110     c                   endif
055800021204      * Per tutte le tariffe stampo dicitura assicurazione merce
055900150909     c                   Add       4             curline
056000030930     c                   ExSr      Sr_Over
056100021204     c                   Write     Ta50pam
056200040211
056300040211      * Stampo paragrafo x telefono destinatario
056400040216     c                   Add       3             curline
056500040211     c                   ExSr      Sr_Over
056600040211     c                   Write     Ta50pte
056700050103
056800050103      * stampa servizi ON LINE se offerta
056900050103     c                   If        *In98
057000050527     c                   ExSr      Sr_Comunica
057100050103     c                   EndIf
057200020110
057300041230      * stampa condizioni
057400020110     c                   exsr      Sr_Over
057500020110     c                   write     ta50pe1
057600041230
057700041230      * stampa condizioni e firme finali
057800020110
057900020110     c                   exsr      Sr_Over
058000020110     c                   write     ta50pfi
058100020110
058200020110     c                   exsr      Sr_Over
058300020110     c                   write     ta50pe2
058400020110
058500020110     c                   exsr      Sr_Over
058600020110     c                   write     ta50pe2B
058700020110
058800020110     c                   exsr      Sr_Over
058900020110     c                   write     ta50pe2C
059000020110
059100020110     c                   exsr      Sr_Over
059200020110     c                   write     ta50pe2D
059300020110
059400020110     c                   exsr      Sr_Over
059500020110     c                   write     ta50pe2E
059600020110
059700020110     c                   exsr      Sr_Over
059800020110     c                   write     ta50pe2F
059900110905
060000110905     c                   exsr      Sr_Over
060100110905     c                   write     ta50pe2G
060200020110
060300020110     c                   exsr      Sr_Over
060400020110     c                   write     ta50pfi
060500020109
060600020109     c     fine          tag
060700020109     c                   seton                                        lr
060800020109
060900020108      *------------------------------------------------------------------*
061000020108      *   Sr_Reccli       RECUPERO CLIENTE
061100020108      *------------------------------------------------------------------*
061200020108
061300020108     c     Sr_Reccli     begsr
061400020108
061500020108      * V I S I T A
061600020108
061700020108     c                   if        *in98
061800020108
061900110104     c**                 if        i45ast = 'V'
062000110104     c**   i45nrv        chain     tnvis000
062100110104     c**                 endif
062200091022     c                   if        i45ast = 'T'
062300091022     c     i45nrv        chain     tivis000
062400091022     c                   endif
062500020108      * recupero ANAGRAFICA CLIENTE
062600020108      * se cliente nuovo leggo nel file anagrafici visite
062700020108     c                   z-add     visnrv        ksc
062800020108     c     ds7kac        chain     tfaco000                           99
062900020108     c  n99ds7kac        chain     tfind000                           99
063000020108     c  n99ds7kac        chain     tfclp000                           99
063100020108      * se non trovato cerco nell'anagrafica clienti
063200020108     c                   if        *in99
063300020108     c                   z-add     visksc        ksc
063400020108     c     ds7kac        chain     cnaco000                           99
063500020108     c  n99ds7kac        chain     cnind000                           99
063600020108     c  n99ds7kac        chain     cnclp000                           99
063700020108     c                   end
063800020108
063900020108     c                   end
064000020108
064100020108      * RECUPERO IL CLIENTE
064200020108
064300020108      * T A R I F F A
064400020108
064500020108     c                   if        not *in98
064600020108     c                   z-add     parsks        ksc
064700020108     c     ds7kac        chain     cnaco000                           99
064800020108     c  n99ds7kac        chain     cnind000                           99
064900020108     c  n99ds7kac        chain     cnclp000                           99
065000020108     c                   endif
065100020108      * se il cliente sia in visita che in tariffa non è stato trovato
065200020108      * viene stampato l'errore
065300020108     c                   if        *in99
065400020108     c                   move      msg(3)        errmsg
065500020108     c                   exsr      Staerr
065600020108
065700010622     c                   else
065800010622      * se non c'è errore valorizzo i campi in stampa
065900010622     c                   eval      picrag = acorag
066000010622     c                   eval      picind = indvia
066100010622     c                   eval      piccap = indcae
066200010622     c                   eval      picloc = indcit
066300010622     c                   eval      picpro = indprv
066400010601     c                   endif
066500020108
066600020108     c                   endsr
066700020108
066800020108      *------------------------------------------------------------------*
066900020108      *   Sr_Decana       DECODIFICA DATI CLIENTI PER LA STAMPA
067000020108      *------------------------------------------------------------------*
067100020108
067200020108     c     Sr_Decana     begsr
067300020108      * valorizzo i numeri riferimento
067400020108     c                   eval      picnr1 = i45nr1
067500020108     c                   eval      picnr2 = i45nr2
067600130905      *
067700020108      * decodfico la nazione se diversa da italia
067800130905      *
067900020108     c                   if        indsta <> *blanks
068000020108     c                   eval      cod ='15'
068100020108     c                   movel     indsta        w003a
068200020108     c                   movel(p)  w003a         key
068300060724     c     tabkeyl       chain     tabel00f
068400020108     c                   if        %found(tabel00f)
068500020108     c                   movel     tbluni        desnar
068600020108     c                   else
068700020108     c                   clear                   desnar
068800020108     c                   endif
068900130905      *
069000020108     c                   endif
069100130905      *
069200020108      * decodifico il commerciale
069300130905     c                   clear                   wCMMcod
069400020108      * visita
069500020108     c                   if        *in98
069600020108      * commerciale visita
069700020108     c                   if        viscmm > 0
069800130905     c                   z-add     viscmm        wCMMcod
069900020108     c                   endif
070000020108      * tariffa
070100020108     c                   else
070200020108      * commerciale tariffa
070300020108     c                   if        clpage > 0
070400130905     c                   z-add     clpage        wCMMcod
070500020108     c                   endif
070600130905      *
070700020108     c                   endif
070800130905      *
070900130905     c                   if        wCMMcod <> *zero
071000130905      *
071100130905     c     wCMMcod       chain     AZCMM000
071200130905     c                   if        %found(AZCMM01L)
071300130905     c                   movel     CMMdes        descom
071400020108     c                   else
071500020108     c                   clear                   descom
071600020108     c                   endif
071700130905      *
071800020108     c                   endif
071900020108
072000020108      * decodifico il pagamento
072100020108
072200020108     c                   if        indcdp <> *blanks
072300020108     c                   move      indcdp        cdpg
072400020108     c                   eval      cdlin = '1'
072500020108     c                   movel(p)  cdpkey        key
072600020108     c                   eval      cod = 'FA'
072700020108
072800060704     c     tabkeyl       chain     tabel00f
072900020108     c                   if        %found(tabel00f)
073000020108     c                   movel     tbluni        dsfa
073100020108     c                   movel     §fades        descdp
073200020108      * mi salvo il tipo pagamento per pilotare la stampa della banca d'appoggio
073300020108      * oppure (es. Rimessa diretta non si stampa la banca d'appoggio)
073400020108     c                   eval      costpg = §fatpg
073500020108     c                   endif
073600020108
073700020108     c                   endif
073800020108      * converto spese di incasso in euro
073900020108     c                   if        indsin > *zeros
074000020108
074100020108     c                   seton                                        72
074200030210
074300030210     c                   Move      IndSin        ValSin
074400020108
074500020108     c                   endif
074600020108      * banca d'appoggio
074700080211     c                   clear                   pcabi
074800080211     c                   clear                   pccab
074900080211     c                   clear                   desbna
075000080211    1c                   if        indabi <> 99999 or indcab <> 99999
075100080211     c                   eval      pcabi = indabi
075200080211     c                   eval      pccab = indcab
075300080211     c     kcnabi        chain     cnabi00f
075400080211    2c                   if        %found(cnabi00f) and abiann <> '*'
075500080211     c                   clear                   wagenzia
075600080211     c                   clear                   wbanca
075700080211     c                   Eval      wbanca = abiist
075800080211      * Compongo il nome della banca con località o comune
075900080211    3c                   If        abiage = *Blanks
076000080211     c                   Select
076100080211     c                   When      abiloc <> *Blanks
076200080211     c                   Eval      wagenzia = abiloc
076300080211     c                   When      abicom <> *Blanks
076400080211     c                   Eval      wagenzia = abicom
076500080211     c                   EndSl
076600080211      * Compongo il nome della banca con agenzia + località o comune
076700080211   x3c                   Else
076800080211      * Cerco il primo byte vuoto dell'agenzia
076900080211     c                   Eval      wpos = %checkr(' ':abiage)
077000080211
077100080211     c                   Eval      wagenzia = %subst(abiage:1:wpos)
077200080211     c                   Eval      wpos1 = wpos + 2
077300080211     c                   Eval      wpos2 = 16 - wpos1
077400080211      * Se wpos2 inferiore a zero lo imposto a 0
077500080211     c                   If        wpos2 < *Zeros
077600080211     c                   Clear                   wpos2
077700080211     c                   EndIf
077800080211      * Se posso aggiungere anche solo 1 carattere procedo
077900080211    4c                   If        wpos > 1 and wpos < 16
078000080211      * Cerco di riempire quello che resta con la località o il comune
078100080211     c                   Select
078200080211     c                   When      abiloc <> *Blanks
078300080211     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
078400080211     c                             %subst(abiloc:1:wpos2)
078500080211     c                   When      abicom <> *Blanks
078600080211     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
078700080211     c                             %subst(abicom:1:wpos2)
078800080211     c                   EndSl
078900080211    4c                   EndIf
079000080211
079100080211    3c                   EndIf
079200080211    2c                   EndIf
079300080211     c                   eval      desbna = wbanca + wagenzia
079400080211    1c                   endif
079500020108
079600020108      * decodifico tipo pagamento contrassegno
079700020108
079800020108     c                   eval      cod = '4U'
079900020108     c                   movel(p)  clpfpc        key
080000060704     c     tabkeyl       chain     tabel00f
080100020108     c                   if        %found(tabel00f)
080200020108     c                   movel     tbluni        ds4u
080300020109     c                   movel     §4udes        comaas            9
080400020108      * se è un tipo pagamento che richiede obbligatoriamente il codice ABI CAB
080500020108      * stampo anche la decodifica della banca d'appoggio
080600020108     c                   if        §4uabi = 'S'
080700080118     c                   eval      combab1 = %subst(clpbab:1:4)
080800080118     c                   eval      combab2 = %subst(clpbab:5:4)
080900080118     c                   eval      combab3 = %subst(clpbab:9:4)
081000080118     c                   eval      combab4 = %subst(clpbab:13:4)
081100080118     c                   eval      combab5 = %subst(clpbab:17:4)
081200080118     c                   eval      combab6 = %subst(clpbab:21:4)
081300080118     c                   eval      combab7 = %subst(clpbab:25:4)
081400080211     c                   eval      combab8 = %subst(clpbab:29:4)
081500020108     c                   endif
081600020108
081700020108     c                   endif
081800020108
081900020108     c                   endsr
082000020108
082100020108      *------------------------------------------------------------------*
082200020108      *   Sr_Recnot       RECUPERA NOTE TARIFFE/OFFERTA CLIENTI
082300020108      *------------------------------------------------------------------*
082400020108
082500020108     c     Sr_Recnot     begsr
082600020108
082700020108      * chiave note tariffe/offerta cliente
082800020108
082900020108      * visita
083000020108     c   98              eval      notapl = 'V'
083100020108      * tariffa
083200020108     c  n98              eval      notapl = 'C'
083300091112      * trattativa
083400091112     c                   if        $trattativa
083500091112     c                   eval      notapl = 'T'
083600091112     c                   endif
083700020108
083800020108     c                   clear                   b
083900020108
084000020108     c                   movel     i45kcc        notke1
084100020108     c                   move      parsks        notke1
084200020108      * note legate alla tariffa/offerta
084300020108     c                   movel     '10'          nottnt
084400020108     c                   movel     i45ctr        notke2
084500020108
084600020108     c     keyntc        setll     tfntc01l
084700020108     c                   do        *hival
084800020108     c     keyntc        reade     tfntc01l
084900020108
085000020108     c                   if        %eof(tfntc01l)
085100020108      * imposto la chiave senza il codice tariffa e mi riposiziono
085200020108     c                   clear                   notke2
085300020108     c     keyntc        setll     tfntc01l
085400020108     c                   leave
085500020108     c                   endif
085600020108
085700020108     c                   if        ntcflt = 'A'
085800020108      * se è annullato leggo il successivo
085900020108     c                   iter
086000020108     c                   else
086100010601
086200020108      * se invece non è annullato ed esiste mi riposiziono per il caricamento
086300020108      * della schiera di comodo delle note
086400020108     c     keyntc        setll     tfntc01l
086500020108     c                   leave
086600020108     c                   endif
086700020108
086800020108     c                   enddo
086900020108      * carico le note in schiera
087000020108     c                   do        *hival
087100020108     c     keyntc        reade     tfntc01l
087200020108
087300020108     c                   if        %eof(tfntc01l)
087400020108     c                   leave
087500020108     c                   endif
087600020108      * se annullato oppure è una nota interna leggo la successiva
087700020108     c                   if        ntcflt = 'A' or ntcsns <> 'S'
087800020108     c                   iter
087900020108     c                   endif
088000020108
088100020108     c                   add       1             b
088200020108     c                   movel     ntcrnt        note(b)
088300020108      * se b = 4 numero massimo di note da stampare vado a fine
088400020108     c                   if        b = 4
088500020108     c                   leave
088600020108     c                   endif
088700020108
088800020108     c                   enddo
088900020108
089000020108      * Responsabile trasporti
089100100923     c                   clear                   notke2
089200091113
089300091113      * se trattativa su cliente cerco con il cliente
089400091113     c                   if        $trattativa and visksc > 0
089500091113     c                   eval      notapl = 'C'
089600091113     c                   move      visksc        notke1
089700091113     c                   endif
089800020108
089900020108     c                   movel     '05'          nottnt
090000020108      * cerco prima con in chiave il codice tariffa
090100020108     c     keyntc        setll     tfntc01l
090200020108     c                   do        *hival
090300020108     c     keyntc        reade     tfntc01l
090400020108
090500020108     c                   if        %eof(tfntc01l)
090600020108     c                   leave
090700020108     c                   endif
090800020108      * se annullato oppure è una nota interna leggo la successiva
090900020108     c                   if        ntcflt = 'A' or ntcsns <> 'S'
091000020108     c                   iter
091100020108     c                   endif
091200020108      * se alla c.a. è uguale a blank carico la nota
091300020108     c                   if        resptra = *blanks
091400020108     c                   movel     ntcrnt        resptra
091500020108     c                   seton                                        73
091600020108      *
091700020108     c                   else
091800020108      * se è già pieno scrivo nel nominativo del e.p.c. ed esco
091900020108     c                   movel     ntcrnt        resptra2
092000020108     c                   seton                                        77
092100020108     c                   leave
092200020108     c                   endif
092300020108
092400020108     c                   enddo
092500020108
092600020108     c                   endsr
092700020108
092800020108      *------------------------------------------------------------------*
092900020108      *   Sr_Decpo        DECODIFICA I DATI DEL PUNTO OPERATIVO
093000020108      *------------------------------------------------------------------*
093100020108
093200020108     c     Sr_Decpo      begsr
093300020108
093400020108      * Se visita e cliente nuovo recupero la filiale dai primi 3 campi del
093500020108      * codice commerciale
093600020108
093700020108     c                   if        *in98 and visksc = 0
093800020108     c                   movel     viscmm        comfil
093900020108     c                   endif
094000020108      * Se visita di un cliente esistente recupero il p.o. appartenenza
094100020108      * cliente
094200020108
094300020108     c                   if        *in98 and visksc > 0
094400020108     c                   movel     visksc        comfil
094500020108     c                   endif
094600020108
094700020108      * se tariffa recupero dal codice cliente
094800020108
094900010601     c                   if        not *in98
095000020108     c                   movel     parsks        comfil
095100020108     c                   endif
095200020108
095300020108      * decodifica
095400020108
095500020108     c     comfil        chain     azorg01l
095600020108
095700020108     c                   if        %found(azorg01l)
095800020108     c                   movel     orgdes        td1fil
095900020108     c                   movel     orgind        td1via
096000020108     c                   movel     orgcpf        td1cap
096100020108     c                   movel     orgloc        td1loc
096200020108     c                   movel     orgpro        td1prv
096300020108     c                   movel     orgtel        td2tel
096400020108     c                   movel     orgfax        td2tfx
096500020108     c                   endif
096600020108
096700020108      * costruisco la data stampa
096800020108     c                   movel     i45gds        data_eur
096900020108     c     *eur          movel     data_eur      datastampa
097000020108     c                   eval      picdal = %trim(orgdes) + ',li ' + datastampa
097100060704
097200060704      * imposto nelle varie lingue
097300060704     c                   select
097400060704     c                   when      i45lin = 'I'
097500060704     c                   eval      picdal = %trim(orgdes) + ',li ' + datastampa
097600060704     c                   when      i45lin = '2'
097700060704     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
097800060801     c                   when      i45lin = '3'
097900060801     c                   eval      picdal = %trim(orgdes) + ',le ' + datastampa
098000060712     c                   when      i45lin = '4'
098100060710     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
098200060704     c                   endsl
098300020108
098400020108     c                   endsr
098500020108
098600020108      *------------------------------------------------------------------*
098700020108      *   Sr_Dectam       DECODIFICA DELLA TESTATA TARIFFA
098800020108      *------------------------------------------------------------------*
098900020108
099000020108     c     Sr_Dectam     begsr
099100020108
099200020108     c                   movel     tamflo        dsta01
099300020108
099400020108      * se la tariffa non è FedEx vado a fine PGM
099500020108     c                   if        §tafed <> 'S'
099600020108     c                   goto      fine
099700020108     c                   endif
099800030508
099900030508      * tariffa merci/documenti
100000030508     c                   Move      TamCtr        WCtr
100100030508If  2c                   If        WCtr >= §Dfedalm and WCtr <= §Dfealm
100200030508     c                   Eval      *in05 = *On
100300030508    2c                   EndIf
100400030508If  2c                   If        WCtr >= §Dfedald and WCtr <= §Dfeald
100500030508     c                   Eval      *in05 = *Off
100600030508    2c                   EndIf
100700060704
100800060704      * imposto l'intestazione della tariffa
100900060704      * tariffa merci
101000060704     c                   if        *in05
101100060704     c                   select
101200060704     c                   when      i45lin = 'I'
101300060704     c                   eval      destar = 'TARIFFA FedEx Merci'
101400060704     c                   when      i45lin = '2'
101500060704     c                   eval      destar = 'TARIFF FedEx Goods'
101600060726     c                   when      i45lin = '3'
101700060726     c                   eval      destar = 'TARIF FedEx Marchandises'
101800060712     c                   when      i45lin = '4'
101900060726     c                   eval      destar = 'PREIS FedEx Ware'
102000060704     c                   endsl
102100060704      * tariffa documenti
102200060704     c                   else
102300060704     c                   select
102400060704     c                   when      i45lin = 'I'
102500060704     c                   eval      destar = 'TARIFFA FedEx Documenti'
102600060704     c                   when      i45lin = '2'
102700060704     c                   eval      destar = 'TARIFF FedEx Documents'
102800060726     c                   when      i45lin = '3'
102900060726     c                   eval      destar = 'TARIF FedEx Documents'
103000060712     c                   when      i45lin = '4'
103100060726     c                   eval      destar = 'PREIS FedEx Briefpost/Dokumente'
103200060704     c                   endsl
103300060704     c                   endif
103400060726      * centro la descrizione della tariffa
103500060801     c                   eval      len = (%len(destar) -
103600060801     c                                    %len(%trimr(destar))) / 2
103700060801     c                   eval      destar = %subst(blanks:1:len) +
103800060801     c                                      %trimr(destar)
103900020108
104000020108      * data decorrenza in stampa
104100020108     c     *iso          movel     tamddt        data_eur
104200020108     c     *eur          movel     data_eur      staddt
104300020108      * data scadenza in stampa
104400020108     c     *iso          movel     tamdst        data_eur
104500020108     c     *eur          movel     data_eur      stadst
104600010531
104700020108      * recupero la divisa per decodifica in stampa
104800020108     c                   eval      cod= 'CV'
104900020108     c                   movel(p)  §tadiv        key
105000060704     c     tabkeyl       chain     tabel00f
105100020108     c                   if        %found (tabel00f)
105200020108     c                   movel     tbluni        dscv
105300020108     c                   movel     §cvdes        desdiv
105400020108     c                   endif
105500020108
105600020108      * decodifico la tariffa cliente
105700020108     c                   movel     i45ctr        unoctr
105800020108     c                   movel(p)  unoctr        key
105900020108     c                   eval      cod = 'TR'
106000060704     c     tabkeyl       chain     tabel00f
106100020108     c                   if        %found(tabel00f)
106200020108     c                   movel     tbluni        dstr
106300020108      * valorizzo i campi descrittivi di stampa
106400020108     c                   movel     §trdst        desctr
106500020110     c                   movel     §trde1        desum
106600020108     c                   movel     §trde3        ds3um
106700020108      * se a quintale o a kg metto in  desum KG. e in desctr A PESO
106800020108     c                   if        unoctr = 0 or unoctr = 3
106900020108     c                   eval      desum = '       KG.'
107000020108     c                   endif
107100020109      * salvo il minimo tassabile
107200020109     c                   eval      ctrata  = §trata
107300020109
107400020108     c                   endif
107500020109
107600020108      * tariffa a valore (per stampare scaglioni senza decimali)
107700020108     c                   eval      *in87 = (unoctr = 4)
107800020109
107900020108      * valorizzo il campo note
108000020108     c                   eval      p07des = tamdcv
108100020109
108200020108      * se il campo note è uguale a blank verifico se tariffa preferenziale
108300020108      * partenza o arrivo
108400020108     c                   if        p07des = *blanks and tambap =  'P'
108500060704      * in lingua
108600060704     c                   select
108700060704     c                   when      i45lin = 'I'
108800020108     c                   eval      p07des = 'Valida solo per traffico in +
108900020108     c                                       Partenza'
109000060704     c                   when      i45lin = '2'
109100060704     c                   eval      p07des = 'Valid only for Departing tra+
109200060704     c                                       ffic'
109300060726     c                   when      i45lin = '3'
109400060726     c                   eval      p07des = 'Valable uniquement pour le +
109500060726     c                                       trafic au départ'
109600060712     c                   when      i45lin = '4'
109700060726     c                   eval      p07des = 'Gilt nur für Abgehende +
109800060726     c                                       Sendungen'
109900060704     c                   endsl
110000020108     c                   endif
110100020108
110200020108     c                   if        p07des = *blanks and tambap =  'A'
110300060704      * in lingua
110400060704     c                   select
110500060704     c                   when      i45lin = 'I'
110600020108     c                   eval      p07des = 'Valida solo per traffico in +
110700020108     c                                       Arrivo'
110800060704     c                   when      i45lin = '2'
110900060704     c                   eval      p07des = 'Valid only for Arriving traf+
111000060704     c                                       fic'
111100060726     c                   when      i45lin = '3'
111200060726     c                   eval      p07des = 'Valable uniquement pour le +
111300060726     c                                       trafic à l''arrivée'
111400060712     c                   when      i45lin = '4'
111500060726     c                   eval      p07des = 'Gilt nur für Eingehende +
111600060726     c                                       Sendungen'
111700060704     c                   endsl
111800020108     c                   endif
111900040430      * TESTO PARTICOLARITÀ GIACENZA
112000040430      * se codice = GP metto 5 nel numero di avvisi prima di rendere la merce
112100040430      * altrimenti lascio 2
112200040430     c                   eval      comnav = 2
112300040430     c                   if        tamgga = 'GP'
112400040430     c                   eval      comnav = 5
112500040430     c                   endif
112600060925      * devo impostare anche la parte finale del numero nelle varie lingue
112700060925     c                   exsr      sr_numgp
112800020108
112900020108      * recupero in base alla data decorrenza la versione di prestampato condi-
113000020108      * zioni generali dalla tabella TAA
113100020108     c     kcod          setll     tntbe01l
113200020108     c                   do        *hival
113300020108     c     kcod          reade     tntbe01l
113400020108      * se fine file vado a fine
113500020108     c                   if        %eof(tntbe01l)
113600020108     c                   leave
113700020108     c                   endif
113800020108      * se annullato leggo il successivo
113900020108     c                   if        tbeatb <> ' '
114000020108     c                   iter
114100020108     c                   endif
114200020108      * mi salvo il modello e l'anno
114300020108     c                   movel     tbeuni        dtaa
114400020108      * mi salvo sempre il primo che ho letto
114500020108     c                   if        stpmod = 0
114600020108     c                   movel     tbeke1        stpmod
114700020108     c                   movel     §dccdad       stpanno
114800020108     c                   endif
114900060313      *?ATTENZIONE  ?
115000060313      * dal 01/03/06 i controlli vanno fatti con la data di stampa e non +
115100060313      * con la data decorrenza tariffa
115200060313      * verifico la data di stampa
115300060313      * se data di stampa maggiore della data di validità del modello leggo
115400020108      * verifico la data di decorrenza
115500020108      * se data decorrenza maggiore della data di validità del modello leggo
115600060313     c                   if        i45ads >  §dccdad
115700020108     c                   movel     tbeke1        stpmod
115800020108     c                   movel     §dccdad       stpanno
115900020108     c                   iter
116000020108     c                   endif
116100020108      * se data decorrenza = salvo a vado a fine
116200060313     c                   if        i45ads =  §dccdad
116300020108     c                   movel     tbeke1        stpmod
116400020108     c                   movel     §dccdad       stpanno
116500020108     c                   leave
116600020108     c                   endif
116700020108      * se data decorrenza <  della data validita' vado a fine
116800060313     c                   if        i45ads <  §dccdad
116900020108     c                   leave
117000020108     c                   endif
117100020108
117200020108     c                   enddo
117300020110
117400020110      * se non c'è la tariffa finita sul file la prendo dalla tabella TR
117500020110     c                   if        tamata = 0
117600020110     c                   z-add     §trata        wrkata
117700020110     c                   else
117800020110     c                   z-add     tamata        wrkata
117900020110     c                   endif
118000020108
118100020108     c                   endsr
118200020108
118300020108      *------------------------------------------------------------------*
118400020109      *   Sr_Scaglioni    CONTROLLO SCAGLIONI E ZONE TARIFFARIE
118500020108      *------------------------------------------------------------------*
118600020108
118700020108     c     Sr_Scaglioni  begsr
118800020108
118900020108     c                   setoff                                       99
119000050131     c                   eval      woklnp = *Off
119100020108
119200020108      * cerco prima con la linea di partenza del cliente
119300061011     c  n98ds1ktd        setll     titad04l
119400061011     c   98ds1ktd        setll     tiofd01l
119500020108
119600020108      * se non trovato con la linea di partenza del cliente
119700061011     c                   if        not %equal
119800020108      * mi riposiziono con chiave generica
119900061011     c  n98ds1ktm        setll     titad04l
120000061011     c   98ds1ktm        setll     tiofd01l
120100020108
120200020108      * se non trovato nulla errore manca dettaglio tariffario
120300061011     c                   if        not %equal
120400020108     c                   seton                                        99
120500020108     c                   move      msg(4)        errmsg
120600020108     c                   exsr      Staerr
120700020108     c                   goto      endscaglia
120800020108     c                   endif
120900020108
121000020108     c                   endif
121100020109
121200020109      * Leggo il dettaglio tariffe
121300020108     c                   do        *hival
121400020108
121500061011     c  n98ds1ktm        reade     titad04l
121600061011     c   98ds1ktm        reade     tiofd01l
121700020109      * fine file esco dal ciclo
121800061011     c                   if        %eof
121900020108     c                   leave
122000020108     c                   endif
122100020108      * se annullato rileggo
122200020108     c                   if        tadatb = 'A'
122300020108     c                   iter
122400020108     c                   endif
122500050131
122600050131     c                   Eval      wokstp = *Off
122700050131
122800050131      * se I45DET = 'L' devo stampare solo le righe di dettaglio che hanno la
122900050131      *                 LNP uguale alla LNP del codice cliente
123000050131     c                   If        I45DET = 'L'
123100050131      * se non è la prima lnp che leggo non stampa più niente
123200050131     c                   if        woklnp = *On and rotlnp <> 0 and
123300050131     c                             rotlnp <> tadlnp
123400050131     c                   iter
123500050131     c                   EndIf
123600050131     c                   ExSr      sr_Chklnp
123700050131     c                   if        wokstp = *Off
123800050131     c                   iter
123900050131     c                   EndIf
124000050131     c                   Eval      woklnp = *On
124100050131     c                   EndIf
124200020109
124300020109      * rottura per lnp
124400020109     c                   if        rotlnp <> tadlnp
124500020109
124600020109      * se non è la prima lnp che leggo esco dal ciclo
124700020109     c                   if        rotlnp <> 0
124800020109     c                   leave
124900020109     c                   endif
125000020109
125100020111     c                   z-add     0             k
125200020109     c                   z-add     0             x
125300020109     c                   eval      rotlnp = tadlnp
125400020109     c                   endif
125500020108
125600020109      * rottura per cts
125700020109     c                   if        rotcts <> tadcts
125800020109     c                   add       1             x
125900020109
126000020116      * errore per più di 7 zone tariffarie per lnp
126100020116     c                   if        x > 7
126200050914      * accendo l'indicatore per uscire dal pgm
126300050914     c                   seton                                        99
126400020109     c                   move      msg(16)       errmsg
126500020109     c                   exsr      Staerr
126600050930     c                   goto      endscaglia
126700020109     c                   endif
126800020109
126900020109     c                   eval      rotcts = tadcts
127000020111
127100020111     c                   endif
127200020109
127300020109      * se non è il primo giro di lettura x cts torno a leggere
127400020109      * tanto non devo più caricare gli scaglioni
127500020109     c                   if        x > 1
127600020109     c                   iter
127700020109     c                   endif
127800020108
127900020109      * carico gli scaglioni per lnp e cts uguali
128000020109
128100020109 b1  c                   if        tadlnp = rotlnp and tadcts = rotcts
128200020109
128300020108     c                   add       1             k
128400020109
128500020109      * errore per più di 200 scaglioni per tariffa
128600020108     c                   if        k > 200
128700050914      * accendo l'indicatore per uscire dal pgm
128800050914     c                   seton                                        99
128900020108     c                   move      msg(5)        errmsg
129000020108     c                   exsr      Staerr
129100050930     c                   goto      endscaglia
129200020108     c                   endif
129300020108
129400020109     c                   z-add     tadsgl        sgl(k)
129500020109
129600020109      * e verifico se gli scaglioni hanno dei decimali solo se non è tariffa
129700020109      * a valore
129800020109 b2  c                   if        not *in87
129900020109
130000020109     c                   move      tadsgl        w0030
130100020109     c                   z-add     tadsgl        w0050
130200020109      * se scaglione 99999 non controllo i decimali
130300020109     c                   if        w0050 <> 99999 and w0030 > 0
130400020109     c                   seton                                        91
130500020109     c                   endif
130600020109      * se con decimali verifico se valorizzato dal secondo in poi
130700020109     c                   if        *in91
130800020109     c                   z-add     w0030         w0020
130900020109     c                   if        w0020 > 0
131000020109     c                   seton                                        37
131100020109     c                   endif
131200020109     c                   endif
131300020109
131400020109 e2  c                   endif
131500020108
131600020109 e1  c                   endif
131700020108
131800020108     c                   enddo
131900020108
132000020108      * se non ho caricato nessuno scaglione do errore di mancanza di dettaglio
132100020109     c                   if        k = 0
132200020108     c                   seton                                        99
132300020108     c                   move      msg(4)        errmsg
132400020108     c                   exsr      Staerr
132500020109     c                   goto      endscaglia
132600020108     c                   endif
132700020108
132800020109     c     endscaglia    endsr
132900020108
133000020108      *-------------------------------------------------------------*
133100020108      *  Sr_Eladet   ELABORO E STAMPO IL DETTAGLIO TARIFFARIO
133200020108      *-------------------------------------------------------------*
133300020108
133400020108     c     Sr_Eladet     begsr
133500020109
133600020109     c                   clear                   rotlnp
133700020109     c                   clear                   rotcts
133800110127     c                   clear                   wrpv
133900050201     c                   eval      woklnp = *Off
134000110127     c                   eval      wvolta = *off
134100020109
134200020109      * Leggo il dettaglio tariffario dall'inizio
134300020109
134400061011     c  n98ds1ktm        setll     titad04l
134500061011     c   98ds1ktm        setll     tiofd01l
134600020109     c                   do        *hival
134700020109
134800061011     c  n98ds1ktm        reade     titad04l
134900061011     c   98ds1ktm        reade     tiofd01l
135000020109
135100020109      * fine file esco dal ciclo
135200061011     c                   if        %eof
135300020109     c                   leave
135400020109     c                   endif
135500020109
135600020109      * se annullato rileggo
135700020109     c                   if        tadatb = 'A'
135800020109     c                   iter
135900020109     c                   endif
136000050131
136100050201     c                   Eval      wokstp = *Off
136200050201
136300050201      * se I45DET = 'L' devo stampare solo le righe di dettaglio che hanno la
136400050201      *                 LNP uguale alla LNP del codice cliente
136500050201     c                   If        I45DET = 'L'
136600050201      * se non è la prima lnp che leggo non stampa più niente
136700050201     c                   if        woklnp = *On and rotlnp <> 0 and
136800050201     c                             rotlnp <> tadlnp
136900050201     c                   iter
137000050201     c                   EndIf
137100050201     c                   ExSr      sr_Chklnp
137200050201     c                   if        wokstp = *Off
137300050201     c                   iter
137400050201     c                   EndIf
137500050201     c                   Eval      woklnp = *On
137600050201     c                   EndIf
137700020109
137800020109      * verifico se è uno scaglione previsto
137900020109     c                   z-add     tadsgl        wsgl72
138000020109     c     wsgl72        lookup    sgl                                    35
138100020109     c                   if        not *in35
138200020109      * errore scaglione non stampabile
138300050914      * accendo l'indicatore ed esco dal ciclo
138400050914     c                   seton                                        99
138500020109     c                   movel     msg(8)        errmsg
138600020109     c                   exsr      Staerr
138700050930     c                   goto      fine
138800020109     c                   endif
138900020109
139000020109      * Stampo testata lnp
139100020109
139200020109 b1  c                   if        tadlnp  <> rotlnp
139300020109
139400020109      * Stampo il dettaglio tariffario caricato per lnp precedente
139500020109     c                   if        rotlnp > 0
139600020109     c                   exsr      Sr_Stptesdet
139700020109     c                   endif
139800020109
139900020109     c                   eval      rotlnp = tadlnp
140000020109     c                   z-add     0             x
140100020109
140200020109      * decodifico la nuova linea di partenza
140300020109     c     tadlnp        chain     azorg01l
140400020109     c                   if        %found(azorg01l)
140500020109     c                   movel     orgdes        stalnp
140600020624      * controllo se linea di partenza 9xx
140700020624     c                   if        orgfl2 = 'S'
140800020624      * italia
140900020624     c                   if        tadlnp = 950
141000060704      * in lingua
141100060704     c                   select
141200060704     c                   when      i45lin = 'I'
141300070312     c                   eval      prtlnp =
141400070313     c                             'Merce ritirata da tutte le ns. filiali'
141500020624     c                             + '  ' + stalnp
141600060704     c                   when      i45lin = '2'
141700060704     c                   eval      prtlnp = 'Goods collected from all our +
141800070314     c                              branches' + '  ' + stalnp
141900060726     c                   when      i45lin = '3'
142000070314     c                   eval      prtlnp = 'Merchandise retirée par tuotes +
142100070314     c                              nos filiales' + '  ' + stalnp
142200060712     c                   when      i45lin = '4'
142300070314     c                   eval      prtlnp = 'Abholung in allen unseren Filialen'
142400060712     c                             + '  ' + stalnp
142500060704     c                   endsl
142600020624     c                   else
142700020624      * regione
142800060704      * in lingua
142900060704     c                   select
143000060704     c                   when      i45lin = 'I'
143100070312     c                   eval      prtlnp = 'Merce ritirata dalle ns. filiali +
143200070312     c                              della regione' + '  ' + stalnp
143300060704     c                   when      i45lin = '2'
143400070314     c                   eval      prtlnp = 'Goods collected from our branches +
143500060704     c                              in the region' + '  ' + stalnp
143600060726     c                   when      i45lin = '3'
143700060726     c                   eval      prtlnp = 'Merchandise retirée par nos +
143800070314     c                              filiales de la région' + '  ' + stalnp
143900060712     c                   when      i45lin = '4'
144000070314     c                   eval      prtlnp = 'Abholung in unseren Filialen der +
144100060726     c                              Region' + '  '  + stalnp
144200060704     c                   endsl
144300020624     c                   endif
144400020624     c                   else
144500020624      * lnp non è 9xx
144600060704      * in lingua
144700060704     c                   select
144800060704     c                   when      i45lin = 'I'
144900070312     c                   eval      prtlnp =
145000070313     c                             'Merce ritirata dalla ns. filiale di'
145100020624     c                             + '  ' + stalnp
145200060704     c                   when      i45lin = '2'
145300070314     c                   eval      prtlnp = 'Goods collected from our branch +
145400060704     c                              in' + '  ' + stalnp
145500060726     c                   when      i45lin = '3'
145600060726     c                   eval      prtlnp = 'Merchandise retirée par notre +
145700070314     c                              filiale de' + '  ' + stalnp
145800060726     c                   when      i45lin = '4'
145900070314     c                   eval      prtlnp = 'Abholung in unseren Filialen in'
146000060712     c                             + '  ' + stalnp
146100060704     c                   endsl
146200020624     c                   endif
146300020624
146400020109     c                   endif
146500020109
146600020109      * overflow stampo fine pagina
146700020109     c                   if        *in80 or curline > 56
146800020109     c                   write     ta50pfin
146900020109     c                   write     ta50ppp
147000020109     c                   setoff                                       80
147100020109     c                   endif
147200020109
147300020109      * stampo la nuova linea di partenza
147400020109     c                   write     ta50p02a
147500020109 e1  c                   endif
147600020109
147700020109      * carico il dettaglio tariffario
147800020109     c                   exsr      Sr_Cardet
147900110127     c                   if        wvolta=*off
148000110127     c                   eval      wrpv=tadrpv
148100110127     c                   eval      wvolta=*on
148200110127     c                   endif
148300020109
148400020109     c                   enddo
148500020110
148600020110      * Stampo l'ultima linea di partenza letta
148700020110     c                   exsr      Sr_Stptesdet
148800020109
148900020211      * overflow stampo fine pagina
149000020211     c                   if        *in80 or curline > 56
149100020211     c                   write     ta50pfin
149200020211     c                   write     ta50ppp
149300020211     c                   setoff                                       80
149400020211     c                   endif
149500020211      * Rapporto peso volume
149600110127     c                   if        wrpv>0
149700110127     c                   eval      p06rpv=wrpv*100
149800020211     c                   write     ta50p06
149900110127     c                   endif
150000020109     c                   endsr
150100020109
150200020109      *-------------------------------------------------------------*
150300020109      *  Sr_Cardet   CARICO IL DETTAGLIO TARIFFARIO X LNP
150400020109      *-------------------------------------------------------------*
150500020110
150600020109     c     Sr_Cardet     begsr
150700020109
150800020109      * rottura per zona tariffaria
150900020109     c                   if        rotcts <> tadcts
151000020109     c                   z-add     0             k
151100020109     c                   add       1             x
151200020116     c                   if        x <= 7
151300020109     c                   move      tadcts        cts(x)
151400020110     c                   clear                   itr
151500020110     c                   endif
151600020110     c                   eval      rotcts = tadcts
151700020109     c                   endif
151800020110
151900020116      * se ci sono più di 7 zone tariffarie non devo stampare
152000020116     c                   if        x > 7
152100050914      * accendo l'indicatore per uscire dal pgm
152200050914     c                   seton                                        99
152300020110     c                   move      msg(16)       errmsg
152400020110     c                   exsr      Staerr
152500020110     c                   goto      endcar
152600020110     c                   endif
152700020109
152800020109     c                   add       1             k
152900020109     c     k             occur(e)  sglitr
153000020109     c                   if        not %error
153100020109     c                   z-add     taditr        itr(x)
153200020110     c                   select
153300020110     c                   when      itr(1) > 0
153400020110     c                   z-add     itr(1)        itr1
153500020110     c                   when      itr(2) > 0
153600020110     c                   z-add     itr(2)        itr2
153700020110     c                   when      itr(3) > 0
153800020110     c                   z-add     itr(3)        itr3
153900020110     c                   when      itr(4) > 0
154000020110     c                   z-add     itr(4)        itr4
154100020110     c                   when      itr(5) > 0
154200020110     c                   z-add     itr(5)        itr5
154300020110     c                   when      itr(6) > 0
154400020110     c                   z-add     itr(6)        itr6
154500020116     c                   when      itr(7) > 0
154600020116     c                   z-add     itr(7)        itr7
154700020110     c                   endsl
154800051006      * memorizzo lo scaglione se è il primo giro
154900051006     c                   If        x = 1
155000020109     c                   z-add     tadsgl        sglp
155100051006      * se non è il primo giro e sto cercando di caricare i dati su uno
155200051006      * scaglione diverso emetto errore ed esco dal pgm
155300051006     c                   Else
155400051006     c                   z-add     tadsgl        wsgl
155500051006     c                   If        wsgl <> sglp
155600051006     c                   move      msg(8)        errmsg
155700051006     c                   exsr      Staerr
155800051006     c                   goto      fine
155900051006     c                   EndIf
156000051006     c                   EndIf
156100020109     c                   endif
156200020109
156300020110     c     endcar        endsr
156400020109
156500020109      *-------------------------------------------------------------*
156600020109      *  Sr_Stptesdet STAMPO LA TESTATA E IL DETTAGLIO TARIFFARIO
156700020109      *-------------------------------------------------------------*
156800020110
156900020109     c     Sr_Stptesdet  begsr
157000020109
157100020110     c                   setoff                                       50
157200020109
157300020110     c                   do        200           k
157400020109     c     k             occur(e)  sglitr
157500020109     c                   if        not %error
157600020110     c                   if        sglp = 0
157700020110     c                   leave
157800020110     c                   endif
157900020109
158000020109      * se prima riga che leggo devo stampare prima la testata
158100020109     c                   if        k = 1
158200020109     c                   exsr      Sr_Stptes
158300020109     c                   endif
158400020109
158500020109      * stampo il dettaglio
158600020109     c                   exsr      Sr_Stpdet
158700020110
158800020110      * salvo lo scaglione che ho appena letto
158900020110     c                   eval      sav_sglp = sglp
159000020109
159100020109     c                   endif
159200020109
159300020109     c                   enddo
159400020109      * pulisco le schiere
159500020109     c                   clear                   cts
159600020109     c                   clear                   dcts
159700020110     c                   do        200           k
159800020110     c     k             occur     sglitr
159900020110     c                   if        not %error
160000020110     c                   clear                   sglp
160100020110     c                   clear                   itr1
160200020110     c                   clear                   itr2
160300020110     c                   clear                   itr3
160400020110     c                   clear                   itr4
160500020110     c                   clear                   itr5
160600020110     c                   clear                   itr6
160700020116     c                   clear                   itr7
160800020110     c                   endif
160900020110     c                   enddo
161000020109
161100020110     c                   endsr
161200020109
161300020109      *-------------------------------------------------------------*
161400020109      *  Sr_Stptes   STAMPO LA TESTATA DELLE ZONE TARIFFARIE
161500020109      *-------------------------------------------------------------*
161600020110
161700020109     c     Sr_Stptes     begsr
161800020109
161900020116     c                   do        7             x
162000020109     c                   eval      cod = 'CT'
162100020109     c                   if        cts(x) <> *blanks
162200020109     c                   movel(p)  cts(x)        key
162300060724     c     tabkeyl       chain     tabel00f
162400020109     c                   if        %found(tabel00f)
162500020109     c                   movel     tbluni        dcts(x)
162600020109     c                   endif
162700020109     c                   endif
162800020109     c                   enddo
162900020110
163000020110     c                   movel     dcts(1)       dzona1
163100020110     c                   movel     dcts(2)       dzona2
163200020110     c                   movel     dcts(3)       dzona3
163300020110     c                   movel     dcts(4)       dzona4
163400020110     c                   movel     dcts(5)       dzona5
163500020110     c                   movel     dcts(6)       dzona6
163600020116     c                   movel     dcts(7)       dzona7
163700020109
163800020109     c                   write     ta50p02
163900020109
164000020109     c                   endsr
164100020109
164200020109      *-------------------------------------------------------------*
164300020109      *  Sr_Stpdet   STAMPO DETTAGLIO - SCAGLIONI
164400020109      *-------------------------------------------------------------*
164500020110
164600020109     c     Sr_Stpdet     begsr
164700020109
164800020109      * se lo scaglione non supera la tariffa finita
164900020110 b1  c                   if        sglp <= wrkata
165000020109
165100020109      * stampo il singolo scaglione
165200020109
165300020109      * senza decimali
165400020109 b2  c                   if        not *in91
165500020109     c                   z-add     sglp          wsgl70
165600020110     c                   eval      p03sgld = %editc(wsgl70:'4')
165700020109 x2  c                   else
165800020109      * con 1 decmale
165900020109 b3  c                   if        not *in37
166000020109     c                   z-add     sglp          wsgl71
166100020110     c                   eval      p03sgld = %editc(wsgl71:'4')
166200020109 x3  c                   else
166300020109      * con 2 decimali
166400020109     c                   z-add     sglp          wsgl72
166500020110     c                   eval      p03sgld = %editc(wsgl72:'4')
166600020109 e3  c                   endif
166700020109 e2  c                   endif
166800020109
166900020109 x1  c                   else
167000020109      * stampo scaglione dal al
167100020109
167200020110     c                   if        not *in50
167300020110     c                   seton                                        50
167400020110     c                   z-add     wrkata        p03ata
167500020109     c                   write     ta50p03a
167600020109     c                   endif
167700020109
167800020109      * senza decimali
167900020109 b2  c                   if        not *in91
168000020110     c                   z-add     sav_sglp      wsgl70d
168100020110     c                   add       1             wsgl70d
168200020110     c                   z-add     sglp          wsgl70a
168300020110     c                   eval      p03sgld = %editc(wsgl70d:'4')
168400020110     c                   eval      p03sgla = %editc(wsgl70a:'4')
168500020109 x2  c                   else
168600020109      * con 1 decmale
168700020109 b3  c                   if        not *in37
168800020110     c                   z-add     sav_sglp      wsgl71d
168900020110     c                   add       0,1           wsgl71d
169000020110     c                   z-add     sglp          wsgl71a
169100020110     c                   eval      p03sgld = %editc(wsgl71d:'4')
169200020110     c                   eval      p03sgla = %editc(wsgl71a:'4')
169300020109 x3  c                   else
169400020109      * con 2 decimali
169500020110     c                   z-add     sav_sglp      wsgl72d
169600020110     c                   add       0,01          wsgl72d
169700020110     c                   z-add     sglp          wsgl72a
169800020110     c                   eval      p03sgld = %editc(wsgl72d:'4')
169900020110     c                   eval      p03sgla = %editc(wsgl72a:'4')
170000020109 e3  c                   endif
170100020109 e2  c                   endif
170200020109 e1  c                   endif
170300051006
170400051006      * Controllo scaglioni
170500051006     c                   ExSr      Sr_Ctrsca
170600020109
170700020109      * stampo tariffa
170800020110     c                   z-add     itr1          tar01
170900020110     c                   z-add     itr2          tar02
171000020110     c                   z-add     itr3          tar03
171100020110     c                   z-add     itr4          tar04
171200020110     c                   z-add     itr5          tar05
171300020110     c                   z-add     itr6          tar06
171400020116     c                   z-add     itr7          tar07
171500020109
171600020109      * se of ristampo la testata tariffa
171700020109
171800020109     c                   if        *in80 or curline > 61
171900020109     c                   write     ta50pfin
172000020109     c                   write     ta50pPP
172100020109     c                   write     ta50p02a
172200020109     c                   write     ta50p02
172300020109     c                   setoff                                       80
172400020109     c                   endif
172500020109
172600020109     c                   write     ta50p03
172700020109
172800020109     c                   endsr
172900051006
173000051006      *------------------------------------------------------------------*
173100051006      *  Sr_Ctrsca        CONTROLLO GLI SCAGLIONI
173200051006      *------------------------------------------------------------------*
173300051006     c     Sr_Ctrsca     BegSr
173400051006
173500051006     c                   Eval      xx = 1
173600051006     c     *Blanks       Lookup    cts(xx)                                35
173700051006     c                   If        Not *In35
173800051006     c                   Eval      yy = 7
173900051006     c                   Else
174000051006     c                   Eval      yy = xx -1
174100051006     c                   EndIf
174200051006
174300051006     c                   Eval      itr(1) = itr1
174400051006     c                   Eval      itr(2) = itr2
174500051006     c                   Eval      itr(3) = itr3
174600051006     c                   Eval      itr(4) = itr4
174700051006     c                   Eval      itr(5) = itr5
174800051006     c                   Eval      itr(6) = itr6
174900051006     c                   Eval      itr(7) = itr7
175000051006
175100051006     c                   Do        yy            xx
175200051006     c                   If        itr(xx) = *Zeros
175300051006     c                   Eval      errmsg = msg(8)
175400051006     c                   ExSr      Staerr
175500051006     c                   Goto      fine
175600051006     c                   EndIf
175700051006     c                   EndDo
175800051006
175900051006     c                   EndSr
176000020109
176100020110      *------------------------------------------------------------------*
176200020110      *  Sr_Arroto        GESTIONE STAMPE ARROTONDAMENTI
176300020110      *------------------------------------------------------------------*
176400020110
176500020110     c     Sr_Arroto     begsr
176600020110
176700020110     c                   clear                   ta50p05
176800020110      * verifico gli importi degli arrotondamenti
176900020110
177000020110    1c                   if        tamarl > 0
177100020110      * verifico se esistono dei valori con i decimali
177200020110     c                   move      tamarf        w0030
177300020110     c                   eval      *in89 =(w0030 > 0)
177400020111
177500020110    2c                   if        not *in89
177600020110     c                   move      tamarl        w0030
177700020110     c                   eval      *in89 =(w0030 > 0)
177800020110    2c                   endif
177900020111
178000020110    2c                   if        not *in89
178100020110     c                   move      tamaro        w0030
178200020110     c                   eval      *in89 =(w0030 > 0)
178300020110    2c                   endif
178400020110
178500020111      * se 89 spento non esistono importi con decimali sposto in campo di comodo
178600020110    2c                   if        not *in89
178700020110     c                   z-add     tamaro        waro2
178800020110     c                   z-add     tamarf        warf2
178900020110     c                   z-add     tamarl        warl2
179000020110    2c                   endif
179100020110
179200020110      * se arrotondamenti di 1 in 1 non stampo
179300020110      * se arrotondamento fino a diverso da arrotondamento oltre a
179400020110
179500020110    2c                   if        tamarf <> tamaro
179600020110
179700020110      * valorizzo i campi di stampa
179800060704      * in lingua
179900060704     c                   select
180000060704     c                   when      i45lin = 'I'
180100020110     c  n89              eval      p05ar1 = 'fino a ' + %editc(Warl2:'4') +
180200020110     c                                      ' di ' + %editc(warf2:'4')     +
180300020110     c                                      ' in ' + %editc(warf2:'4')
180400020110     c   89              eval      p05ar1 = 'fino a ' + %editc(tamarl:'4') +
180500020110     c                                      ' di ' + %editc(tamarf:'4')     +
180600020110     c                                      ' in ' + %editc(tamarf:'4')
180700060704     c                   when      i45lin = '2'
180800060704     c  n89              eval      p05ar1 = 'Up to ' +
180900060704     c                                      %editc(warl2:'4') + ' rounded ' +
181000060704     c                                      'to the nearest ' +
181100060704     c                                      %editc(warf2:'4')
181200060704     c   89              eval      p05ar1 = 'Up to ' +
181300060704     c                                      %editc(tamarl:'4') + ' rounded ' +
181400060704     c                                      'to the nearest ' +
181500060704     c                                      %editc(tamarf:'4')
181600060801     c                   when      i45lin = '3'
181700060801     c  n89              eval      p05ar1 = 'jusq''à ' + %editc(Warl2:'4') +
181800060801     c                                      ' de ' + %editc(warf2:'4')     +
181900060801     c                                      ' en ' + %editc(warf2:'4')
182000060818     c   89              eval      p05ar1 = 'jusq''à ' + %editc(tamarl:'4') +
182100060801     c                                      ' de ' + %editc(tamarf:'4')     +
182200060801     c                                      ' en ' + %editc(tamarf:'4')
182300060712     c                   when      i45lin = '4'
182400060818     c  n89              eval      p05ar1 = 'bis  ' + %editc(warl2:'4')  +
182500060710     c                                      ' von ' + %editc(warf2:'4') +
182600060710     c                                      ' auf ' + %editc(warf2:'4')
182700060818     c   89              eval      p05ar1 = 'bis  ' + %editc(tamarl:'4') +
182800060710     c                                      ' von ' + %editc(tamarf:'4')     +
182900060710     c                                      ' auf ' + %editc(tamarf:'4')
183000060704     c                   endsl
183100020110
183200020110      * se arrotondamento oltre a è maggiore di 1
183300020110
183400020110      * valorizzo i campi di stampa
183500060704      * in lingua
183600060704     c                   select
183700060704     c                   when      i45lin = 'I'
183800020110     c  n89              eval      p05ar2 = 'oltre  ' + '    '        +
183900020110     c                                      ' di ' + %editc(waro2:'4')     +
184000020110     c                                      ' in ' + %editc(waro2:'4')
184100020110     c   89              eval      p05ar2 = 'oltre  ' + '        '         +
184200020110     c                                      ' di ' + %editc(tamaro:'4')     +
184300020110     c                                      ' in ' + %editc(tamaro:'4')
184400060704     c                   when      i45lin = '2'
184500060704     c  n89              eval      p05ar2 = 'over ' + '         '  +
184600060704     c                                      ' to the nearest ' +
184700060704     c                                      %editc(waro2:'4')
184800060704     c   89              eval      p05ar2 = 'over ' + '         '  +
184900060704     c                                      ' to the nearest ' +
185000060704     c                                      %editc(tamaro:'4')
185100060801     c                   when      i45lin = '3'
185200060801     c  n89              eval      p05ar2 = 'au delà' + '    '        +
185300060801     c                                      ' de ' + %editc(waro2:'4')     +
185400060801     c                                      ' en ' + %editc(waro2:'4')
185500060801     c   89              eval      p05ar2 = 'au delà' + '        '         +
185600060801     c                                      ' de ' + %editc(tamaro:'4')     +
185700060801     c                                      ' en ' + %editc(tamaro:'4')
185800060712     c                   when      i45lin = '4'
185900060818     c  n89              eval      p05ar2 = 'mehr als ' + '    '        +
186000060710     c                                      ' von ' + %editc(waro2:'4')     +
186100060710     c                                      ' auf ' + %editc(waro2:'4')
186200060818     c   89              eval      p05ar2 = 'mehr als ' + '    '         +
186300060710     c                                      ' von ' + %editc(tamaro:'4')     +
186400060710     c                                      ' auf ' + %editc(tamaro:'4')
186500060704     c                   endsl
186600020110
186700020110     c                   exsr      Sr_Over
186800020110
186900020110     c                   write     ta50p05
187000020110
187100020110     c                   else
187200020110      * valorizzo i campi di stampa
187300060704      * in lingua
187400060704     c                   select
187500060704     c                   when      i45lin = 'I'
187600020110     c  n89              eval      p05ar1 = ' di ' + %editc(Warf2:'4') +
187700020110     c                                      ' in ' + %editc(waro2:'4')
187800020110     c   89              eval      p05ar1 = ' di ' + %editc(tamarf:'4') +
187900020110     c                                      ' in ' + %editc(tamaro:'4')
188000060704     c                   when      i45lin = '2'
188100060704     c  n89              eval      p05ar1 = 'Rounded to the nearest ' +
188200060704     c                                      %editc(warf2:'4')
188300060704     c   89              eval      p05ar1 = 'Rounded to the nearest ' +
188400060704     c                                      %editc(tamarf:'4')
188500060801     c                   when      i45lin = '3'
188600060801     c  n89              eval      p05ar1 = ' de ' + %editc(Warf2:'4') +
188700060801     c                                      ' en ' + %editc(waro2:'4')
188800060801     c   89              eval      p05ar1 = ' de ' + %editc(tamarf:'4') +
188900060801     c                                      ' en ' + %editc(tamaro:'4')
189000060712     c                   when      i45lin = '4'
189100060710     c  n89              eval      p05ar1 = ' von ' + %editc(Warf2:'4') +
189200060710     c                                      ' auf ' + %editc(waro2:'4')
189300060710     c   89              eval      p05ar1 = ' von ' + %editc(tamarf:'4') +
189400060710     c                                      ' auf ' + %editc(tamaro:'4')
189500060704     c                   endsl
189600020110
189700020110     c                   exsr      Sr_Over
189800020110
189900020110     c                   write     ta50p05
190000020110
190100020110    2c                   endif
190200020110    1c                   endif
190300020110
190400020110     c                   endsr
190500020110
190600020110      *------------------------------------------------------------------*
190700020110      *  Sr_Statam        STAMPA I DATI CHE SI TROVANO IN TESTATA TARIFFA
190800020110      *------------------------------------------------------------------*
190900020110
191000020110     c     Sr_Statam     begsr
191100020110
191200020110      * se offerta valorizzo con il numero della visita
191300020110    1c                   if        *in98
191400020110     c                   eval      p07ksc = visksc
191500020110     c                   movel     tamksc        p07nrv
191600020110     c                   move      tamksc        p07prv
191700091117     c                   if        $trattativa
191800091117     c                   eval      p07ntra = tamksc
191900091117     c                   endif
192000020110
192100020110     c                   else
192200020110      * tariffa
192300020110     c                   eval      p07ksc = tamksc
192400020110    1c                   endif
192500020110
192600091117     c                   if        $trattativa
192700091117     c                   eval      ptratta =
192800091117     c                             '(codice ' +
192900091117     c                             %editc(tamctr:'X') + '/' +
193000091117     c                             %subst(%editc(tamprg:'X'):3:1) + ')'
193100091117     c                   endif
193200091117     c                   if        not $trattativa
193300091117     c                   eval      ptariffa =
193400091117     c                             '(codice ' +
193500091117     c                             %editc(tamctr:'X') + ')'
193600091117     c                   endif
193700020110
193800020110     c                   exsr      Sr_Over
193900020110
194000020110     c                   write     ta50p07
194100020110      * se applicazione tariffa diversa dal normale stampo
194200020110     c                   if        §taf02 = 'S'
194300020110
194400020110     c                   exsr      Sr_Over
194500020110
194600020110     c                   write     ta50p07s
194700020110     c                   endif
194800020110
194900020110     c                   exsr      Sr_Over
195000020110
195100020110     c                   if        §taf02 = 'G'
195200020110     c                   write     ta50p07g
195300020110     c                   endif
195400020625
195500020625      * stampo alcune diciture fisse
195600020625     c                   exsr      Sr_Over
195700020625     c                   write     ta50p08
195800030930
195900030930If  1c                   If        *In05
196000030930     c                   Add       4             curline
196100030930     c                   exsr      Sr_Over
196200030930     c                   write     ta50p08e
196300030930     c                   Add       6             curline
196400030930     c                   exsr      Sr_Over
196500030930     c                   write     ta50p08m
196600030930   x1c                   Else
196700030930     c                   Add       2             curline
196800030930     c                   exsr      Sr_Over
196900030930     c                   write     ta50p08d
197000030930    1c                   EndIf
197100030930
197200030930     c                   Add       2             curline
197300030930     c                   exsr      Sr_Over
197400030930     c                   write     ta50p08z
197500020110
197600020110      * validità tariffa
197700020110     c                   exsr      Sr_Over
197800020110
197900020110      * verifico se devo stampare "per:" solo se c'è o il numero spedizioni mini
198000020110      * me o massime o se c'è uno dei sui volumi auotmatici
198100020110     c                   if        tamlrc > 0 or tamlas > 0 or tamtin <> ' '
198200020110     c                   seton                                        26
198300020110     c                   endif
198400020110     c                   write     ta50p09
198500020110      * verifico se stampare spedizioni minime e massime
198600020110    1c                   if        tamlrc > 0 or tamlas > 0
198700020110     c                   eval      comlrc = tamlrc
198800020110     c                   eval      comlas = tamlas
198900020110
199000020110     c                   exsr      Sr_Over
199100020110
199200020110     c                   write     ta50p09a
199300020110    1c                   endif
199400020110      * verifico se devo stampare volume automatico
199500020110    1c                   if        tamtin = 'K'
199600020110      * volume automatico a  kg
199700020110     c                   z-add     tamkpm        comkpm
199800020110    1c                   endif
199900020110
200000020110    1c                   if        tamtin = 'C'
200100020110      * volume a collo
200200020110     c                   z-add     tamkpm        comckm
200300020110    1c                   endif
200400020110      * stampo volume automatico
200500020110    1c                   if        tamtin <> ' '
200600020110
200700020110     c                   exsr      Sr_Over
200800020110
200900020110    1c                   if        tamtin = 'K'
201000020110     c                   write     ta50p09b
201100020110     c                   write     ta50p09s
201200020110     c                   else
201300020110     c                   write     ta50p09c
201400020110     c                   write     ta50p09s
201500020110     c                   endif
201600020110      * se non c'è volume stampo una riga vuota prima delle note
201700020110     c                   else
201800020110     c                   write     ta50p09s
201900020110
202000020110    1c                   endif
202100020110      * stampo le note
202200020110     c                   z-add     1             x
202300020110
202400020110    1c                   do        4             x
202500020110     c                   if        note(x) <> *blanks
202600020110     c                   movel     note(x)       comnot
202700020110
202800020110     c                   exsr      Sr_Over
202900020110
203000020110     c                   write     ta50p10
203100020110     c                   endif
203200020110    1c                   enddo
203300020110      * se c'è stampo l'addizionale di gestione
203400020110    1c                   if        tampag > 0
203500060704      * in lingua
203600060704     c                   select
203700060704     c                   when      i45lin = 'I'
203800041129     c                   Eval      comdag = 'Addizionale di gestione -
203900041129     c                             anni precedenti:'
204000060712     c                   when      i45lin = '2'
204100060712     c                   eval      comdag = 'Surcharge for previous -
204200060712     c                             years'' administration:'
204300060726     c                   when      i45lin = '3'
204400060726     c                   eval      comdag = 'Complémentaire de gestion -
204500060726     c                             années précédentes:'
204600060712     c                   when      i45lin = '4'
204700060726     c                   Eval      comdag = 'Aufschlag für Bearbeitung -
204800060726     c                             von Vorjahren:'
204900060712     c                   endsl
205000041129     c                   eval      comcag = tampag
205100020110     c                   seton                                        27
205200020110    1c                   endif
205300041129      * se c'è stampo l'addizionale di gestione anno corrente
205400041129    1c                   if        tamppa > 0
205500041129     c                   If        *In27
205600060704      * in lingua
205700060704     c                   select
205800060704     c                   when      i45lin = 'I'
205900041129     c                   Eval      comdag1 = 'Addizionale di gestione -
206000060818     c                             corrente.......:'
206100060704     c                   when      i45lin = '2'
206200060704     c                   eval      comdag1 = 'Surcharge for current -
206300060818     c                             administration........:'
206400060726     c                   when      i45lin = '3'
206500060726     c                   eval      comdag1 = 'Complémentaire de gestion -
206600060818     c                             courante..........:'
206700060712     c                   when      i45lin = '4'
206800060726     c                   Eval      comdag1 = 'Aufschlag für Bearbeitung -
206900060726     c                             des laufenden Jahres:'
207000060704     c                   endsl
207100041129     c                   eval      comcag1 = tamppa
207200041129     c                   seton                                        29
207300041129     c                   else
207400060704      * in lingua
207500060704     c                   select
207600060704     c                   when      i45lin = 'I'
207700041129     c                   Eval      comdag = 'Addizionale di gestione -
207800060726     c                             corrente:'
207900060704     c                   when      i45lin = '2'
208000060704     c                   eval      comdag = 'Surcharge for current -
208100060704     c                             administration:'
208200060726     c                   when      i45lin = '3'
208300060726     c                   eval      comdag = 'Complémentaire de gestion -
208400060726     c                             courante:'
208500060712     c                   when      i45lin = '4'
208600060726     c                   Eval      comdag = 'Aufschlag für Bearbeitung -
208700060726     c                             des laufenden Jahres:'
208800060704     c                   endsl
208900041129     c                   eval      comcag = tamppa
209000041129     c                   seton                                        27
209100041129    1c                   endif
209200041129    1c                   endif
209300020110
209400020110      * stampo commenti prima dele tariffe particolari
209500041222     c                   Add       3             curline
209600020110     c                   exsr      Sr_Over
209700020110
209800020110     c                   write     ta50p11
209900031120      * se c'è istat stampo
210000101015     c                   if        tamrct >  0
210100031120      * recupero la data di decorrenza
210200031120      *
210300050525      * se superiore alla data decorrenza della tariffa stampo la data
210400050525      * decorrenza ista
210500051124      * non + si stampa la data decorrenza dell'indice ISTAT
210600051124     c*****              If        dia(tamfli) >= tamddt
210700101015     c     *iso          movel     dia(tamrct)   data_EUR
210800050527     C     *eur          movel     DATA_EUR      dtaist
210900050525      * se la data  data decorrenza istat è inferiore alla data decorrenza
211000050525      * della tariffa stampo la data decorrenza tariffa
211100051124     c*******            Else
211200051124     c**** *iso          movel     tamddt        data_EUR
211300051124     C**** *eur          movel     DATA_EUR      dtaist
211400051124     c*******            EndIf
211500031120      *
211600031120      * controllo se % diversa da 100
211700031120     c                   eval      *in11 = (tampri <> 100)
211800031120      * stampo
211900031120
212000150317     c                   Add       4             curline
212100031120     c                   exsr      SR_OVER
212200031120
212300031120     c                   write     ta50pist
212400031120      *
212500031120     c                   endif
212600021205      * stampo riga vuota
212700021205     c                   write     ta50p09s
212800020110
212900020110     c                   endsr
213000050131
213100050131      *------------------------------------------------------------------*
213200050131      *  Sr_ChkLnp        CONTROLLO SE LNP OK X LA STAMPA
213300050131      *------------------------------------------------------------------*
213400050131
213500050131     c     Sr_ChkLnp     BegSr
213600050131
213700050131     c                   If        wokstp = *Off
213800050131     c                   Select
213900050131     c                   when      Tadlnp = Comfil
214000050131     c                   Eval      wokstp = *on
214100050131     c                   when      Tadlnp = 950
214200050131     c                   Eval      wokstp = *on
214300050131     c                   EndSl
214400050131     c                   EndIf
214500050131
214600050131     c                   EndSr
214700020110
214800020110      *------------------------------------------------------------------*
214900020110      *  Sr_Statpt        STAMPA TARIFFE PARTICOLARI
215000020110      *------------------------------------------------------------------*
215100020110
215200020110     c     Sr_Statpt     begsr
215300020110
215400020110      * accendo indicatore tariffe particolari
215500020110     c                   seton                                        10
215600021205
215700021205      * Pulisco le sk per stampa tariffe particolari su + righe
215800021205     c                   Clear                   Tpta
215900021205     c                   Clear                   Tptb
216000021205     c                   Clear                   Tptc
216100021205     c                   Clear                   Atp1
216200021205     c                   Clear                   Atpc
216300060313      * pulisco campo di comodo
216400060313     c                   clear                   savapl
216500020111
216600020110      * leggo la schiera ordinata delle tariffe particolari
216700020110     c                   do        50            j
216800020110      * se valorizzata
216900020110     c                   if        cp(j) <> *blank
217000020110      * aggancio la tabella relativa le tariffe particolari
217100020110     c                   setoff                                       99
217200020110     c                   z-add     1             y
217300020110     c     cp(j)         lookup    ftc(y)                                 31
217400020111      * verifico se è una tariffa particolare da stampare
217500020110     c                   if        not *in31
217600020110     c                   seton                                        99
217700020110     c                   else
217800020111
217900020110     c                   endif
218000020111
218100020110     c                   select
218200020110      * se tariffa stampabile (99 spento)
218300020110     c                   when      *in99
218400020110      * se è una tariffa particolare che vien stampata in modo standard
218500020110     c                   when      tst(y) = ' '
218600020110     c                   exsr      Sr_Taparstd
218700020110      * se è una tariffa particolare che non vien stampata in modo standard
218800020110     c                   when      tst(y) = 'N'
218900020110      * verifico che tariffa particolare è
219000020110      * elavazione limite risarcibile
219100020110     c                   if        ftc(y) = 'R'
219200020110     c                   exsr      Sr_Elliris
219300020110     c                   endif
219400060313      * mandato assicurativo AC PLUS
219500020110     c                   if        ftc(y) = 'C'
219600020110     c                   exsr      Sr_Mandato
219700020110     c                   endif
219800060313      * mandato assicurativo AC BASE
219900060313     c                   if        ftc(y) = 'd'
220000060313     c                   exsr      sr_acbase
220100060313     c                   endif
220200020110
220300020110     c                   endsl
220400020110
220500020110     c                   endif
220600020110
220700020110     c                   enddo
220800020110
220900020110     c                   endsr
221000021205
221100021205      *-------------------------------------------------------------------------
221200021205      *  Sr_StaTpt1   Stampa tariffe particolari da sk unica
221300021205      *-------------------------------------------------------------------------
221400021205     c     Sr_StaTpt1    BegSr
221500021205
221600021205     c                   Clear                   Conta
221700021205     c                   Eval      WokTp = *Off
221800021205      ***
221900021205      * Stampo prima le tariffe particolari con solo l'importo
222000021205Do  1c                   Do        50            Atp1
222100021205     c                   If        Tpta(Atp1) = *Blanks
222200021205     c                   Leave
222300021205     c                   EndIf
222400021205     c                   Eval      Woktp = *On
222500021205      * due tariffe per riga
222600021205If  2c                   If        Conta = *Zeros
222700021205     c                   Clear                   Comstda2
222800021205     c                   Clear                   Comstdb2
222900021205     c                   Movel(p)  Tpta(Atp1)    Comstda1
223000021205     c                   Movel(p)  Tptb(Atp1)    Comstdb1
223100021205     c                   Add       1             Conta
223200021205   x2c                   Else
223300021205     c                   Movel(p)  Tpta(Atp1)    Comstda2
223400021205     c                   Movel(p)  Tptb(Atp1)    Comstdb2
223500021205     c                   ExSr      Sr_Over
223600021205     c                   Write     Ta50pstd1
223700021205     c                   Clear                   Conta
223800021205    2c                   EndIf
223900021205
224000021205    1c                   EndDo
224100021205      * Stampo ultima riga non completa
224200021205     c                   If        WokTp = *On and Conta <> *Zeros
224300021205     c                   ExSr      Sr_Over
224400021205     c                   Write     Ta50pstd1
224500021205     c                   EndIf
224600021205      * stampo riga vuota
224700021205     c                   write     ta50p09s
224800021205
224900021205     c                   Eval      WokTp = *Off
225000021205     c                   Z-Add     *Zeros        CntList
225100021205      ***
225200021205      * Stampo per ultime le tariffe particolari "al costo"
225300021205Do  1c                   Do        50            Atpc
225400021205      * Stampo ultima riga
225500021205     c                   If        Tptc(Atpc) = *Blanks
225600021205     c                   Add       1             CntList
225700060704     c                   select
225800060704     c                   when      i45lin = 'I'
225900021205     c                   Eval      Comstdc1 = ': al costo'
226000060704     c                   when      i45lin = '2'
226100060704     c                   Eval      Comstdc1 = ': at cost'
226200060801     c                   when      i45lin = '3'
226300060801     c                   Eval      Comstdc1 = ': au coût'
226400060712     c                   when      i45lin = '4'
226500060712     c                   Eval      Comstdc1 = ':zum Preis'
226600060704     c                   endsl
226700021205     c                   ExSr      StpRiga
226800021205     c                   Leave
226900021205     c                   EndIf
227000021205
227100021205     c                   If        %Len(%Trim(Comstdc)) + 1 +
227200021205     c                             %len(%trim(Tptc(Atpc))) + 1 <= Lungh
227300021205     c                   ExSr      CrtRiga
227400021205     c                   Else
227500021205     c                   Add       1             CntList
227600021205     c                   Clear                   Comstdc1
227700021205     c                   ExSr      StpRiga
227800021205     c                   ExSr      CrtRiga
227900021205     c                   EndIf
228000021205
228100021205    1c                   EndDo
228200021205
228300021205     c                   EndSr
228400021205      *
228500021205      *-------------------------------------------------------------------------
228600021205      * CrtRiga - Routine di creazione riga tar.particolari "al costo"
228700021205      *-------------------------------------------------------------------------
228800021205     c     CrtRiga       BegSr
228900021205
229000021205     c                   If        Comstdc = *Blanks
229100021205     c                   Eval      Comstdc = %Trim(Comstdc) + %Trim(Tptc(Atpc))
229200021205     C                   Else
229300021205     c                   Eval      Comstdc = %Trim(Comstdc) + ','
229400021205     c                                       + %Trim(Tptc(Atpc))
229500021205     c                   EndIf
229600021205
229700021205     c                   EndSr
229800021205      *
229900021205      *-------------------------------------------------------------------------
230000021205      * StpRiga - Routine di stampa riga tar.particolari "al costo"
230100021205      *-------------------------------------------------------------------------
230200021205     c     StpRiga       BegSr
230300021205
230400021205     c                   If        Comstdc <> *Blanks
230500021205     c                   Eval      TxtInput = %trim(Comstdc)
230600021205     c                   If        (Cntlist > 0) and
230700021205     c                             (%len(%Trim(TxtInput)) > 75)
230800021205     c                   ExSr      ExeGiust
230900021205     c                   If        Not %error
231000021205     c                   Eval      Comstdc = TxtGiust
231100021205     c                   Else
231200021205     c                   Eval      Comstdc = TxtInput
231300021205     c                   EndIf
231400021205     c                   Else
231500021205     c                   Eval      Comstdc = TxtInput
231600021205     c                   EndIf
231700021205     c                   ExSr      Sr_Over
231800021205     c                   Write     Ta50pstdc
231900021205     c                   Clear                   Comstdc
232000021205     c                   EndIf
232100021205
232200021205     c                   EndSr
232300021205      *
232400021205      *-------------------------------------------------------------------------
232500021205      * EXEGIUST - Routine di "GIUSTIFICAZIONE" testo
232600021205      *-------------------------------------------------------------------------
232700021205     c     ExeGiust      BegSr
232800021205
232900021205     c                   Call(e)   'XGIUST'
233000021205     c                   Parm                    TxtInput
233100021205     c                   Parm                    Lungh
233200021205     c                   Parm                    TxtGiust
233300021205     c                   Parm                    TxtResto
233400021205
233500021205     c                   EndSr
233600020110
233700020110      *------------------------------------------------------------------*
233800020110      *  Sr_Statgc        STAMPA TARIFFE GIACENZA
233900020110      *------------------------------------------------------------------*
234000020110
234100020110     c     Sr_Statgc     begsr
234200020110
234300020110     c                   if        *in98
234400020110     c     ds1ktm        chain     tiogc01l                           99
234500020110
234600020110     c                   else
234700020110
234800020110     c     ds1ktm        chain     titgc01l                           99
234900020110     c                   endif
235000020110
235100020110     c                   if        *in99 or tgcatb = 'A'
235200020110     c                   goto      endstatgc
235300020110     c                   endif
235400020110      * rapporto peso volume
235500020110     c                   eval      comgpv = tgcrpv * 100
235600020110      * preaparo i campi in stampa
235700020110     c                   z-add     tgcsgd        comsgd
235800020110     c                   z-add     tgcsgv        comsgv               28
235900020110     c                   z-add     tgcsgp        comsgp
236000020110     c                   z-add     tgcsgr        comsgr
236100020110     c                   z-add     tgcstm        comstm
236200020110     c                   z-add     tgcst1        comst1
236300020110     c                   z-add     tgcsg1        comsg1
236400020110     c                   z-add     tgcst2        comst2
236500020110
236600020110      * se giorni di rientro sono a zero li recupero dalla tabella "2G"
236700020110     c                   if        tgcggr = *zeros
236800020213      * tariffa estera  FEDEX
236900020213     c                   z-add     §2ggfe        tgcggr
237000020110
237100020110
237200020110     c                   endif
237300020110
237400020110     c                   z-add     tgcggr        comggr
237500020110
237600020110     c                   if        *in80 or curline > 55
237700020110     c                   write     ta50pfin
237800020110     c                   write     ta50pPP
237900020110     c                   setoff                                       80
238000020110     c                   endif
238100020110
238200020110     c                   write     ta50pgi
238300020110      * verifico se c'è la franchigia
238400020110     c                   if        tgcsgf > 0
238500020110     c                   z-add     tgcsgf        comsgf
238600020110      * verifico il falg applicazione franchigia
238700060704      * e stampo in lingua
238800060704     c                   select
238900060704     c                   when      i45lin = 'I'
239000020110     c                   if        tgcfaf = '1'
239100020110     c                   eval      comapf = cspe1
239200020110     c                   endif
239300020110     c                   if        tgcfaf = '2'
239400020110     c                   eval      comapf = cspe2
239500020110     c                   endif
239600060118     c                   if        tgcfaf = '3'
239700060118     c                   eval      comapf = cspe3
239800060118     c                   endif
239900060704
240000060704     c                   when      i45lin = '2'
240100060704     c                   if        tgcfaf = '1'
240200060704     c                   eval      comapf = cspe12
240300060704     c                   endif
240400060704     c                   if        tgcfaf = '2'
240500060704     c                   eval      comapf = cspe22
240600060704     c                   endif
240700060704     c                   if        tgcfaf = '3'
240800060704     c                   eval      comapf = cspe32
240900060704     c                   endif
241000060726
241100060726     c                   when      i45lin = '3'
241200060726     c                   if        tgcfaf = '1'
241300060726     c                   eval      comapf = cspe13
241400060726     c                   endif
241500060726     c                   if        tgcfaf = '2'
241600060726     c                   eval      comapf = cspe23
241700060726     c                   endif
241800060726     c                   if        tgcfaf = '3'
241900060726     c                   eval      comapf = cspe33
242000060726     c                   endif
242100060712
242200060712     c                   when      i45lin = '4'
242300060712     c                   if        tgcfaf = '1'
242400060712     c                   eval      comapf = cspe14
242500060712     c                   endif
242600060712     c                   if        tgcfaf = '2'
242700060712     c                   eval      comapf = cspe24
242800060712     c                   endif
242900060712     c                   if        tgcfaf = '3'
243000060712     c                   eval      comapf = cspe34
243100060712     c                   endif
243200060704     c                   endsl
243300060118
243400020110     c                   exsr      Sr_Over
243500020110
243600020110     c                   write     ta50pgif
243700020110
243800020110     c                   endif
243900031008
244000031008     c                   Add       2             curline
244100031008     c                   exsr      Sr_Over
244200031008     c                   write     ta50pgi1
244300020110
244400020110     c     endstatgc     endsr
244500020110
244600020110      *------------------------------------------------------------------*
244700020110      *  Sr_Taparstd      STAMPA DATI DELLA TARIFFA PARTICOLARE STANDARD
244800020110      *------------------------------------------------------------------*
244900020110
245000020110     c     Sr_Taparstd   begsr
245100020110
245200020110     c                   movel(P)  ftc(y)        savftc
245300020110     c                   movel(p)  edtc(y)       comstd
245400020110      * se si tratta della tariffa particolare isola accendo
245500020110     c                   if        ftc(y) = 'I'
245600020110     c                   seton                                        06
245700020110     c                   endif
245800020110
245900061011     c  n98ktpt          chain     titpt01l
246000061011     c   98ktpt          chain     tiopt01l
246100020110      * se non trovato vado a fine senza stampare
246200061011     c                   if        not %found or tptatb = 'A'
246300020110      * verifico se devo stampare al costo
246400020110     c                   if        edac(y) = 'S'
246500021205
246600021205      * se non devo stampare righe aggiuntive salvo la tariffa particolare
246700021205      * in una sk che stamperò alla fine di tutte le tariffe particolari
246800021205If  3c                   If        Esud(y) <> 'S'
246900021205     c                   Add       1             Atpc
247000021205     c                   Movel     Comstd        Tptc(Atpc)
247100021205
247200021205      * se devo stampare righe aggiuntive stampo anche la tariffa particolare
247300021205   x3c                   Else
247400060704     c                   select
247500060704     c                   when      i45lin = 'I'
247600020110     c                   eval      comdstd = ': al costo       '
247700060704     c                   when      i45lin = '2'
247800060704     c                   eval      comdstd = ': at cost        '
247900060801     c                   when      i45lin = '3'
248000060801     c                   eval      comdstd = ': au coût        '
248100060712     c                   when      i45lin = '4'
248200060711     c                   eval      comdstd = ': zum Preis      '
248300060704     c                   endsl
248400020110
248500020110     c                   exsr      Sr_Over
248600020110
248700020110     c                   write     ta50pstd
248800020110      * vado a stampare righe aggiuntive se richieste
248900020110     c                   goto      sudtpstd
249000021205    3c                   EndIf
249100021205
249200020110     c                   else
249300020110      * altrimenti vado a fine e non stampo nulla
249400020110     c                   goto      endtpstd
249500020110     c                   endif
249600020110
249700020110     c                   endif
249800020110      * cerco se ha record validi con tariffa o minimo > 0
249900020110     c                   exsr      Ricerc
250000020110      * per 04 acceso stampo nessun addebito
250100020110     c                   if        *in04
250200020110     c                   goto      endtpstd
250300020110     c                   endif
250400020110
250500020110      * per 04 spento verifico quanti record dettaglio tariffario esistono
250600020110      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
250700020110     c                   if        recval = 1
250800020110     c                   exsr      Sr_Gesunico
250900021205
251000021205      * se la tariffa particolare è di 1 riga con solo l'importo
251100021205      * salvo la tariffa particolare in una sk che stamperò alla fine di
251200021205      * tutte le tariffe particolari prima della sk al costo
251300021205If  2c                   If        Esud(y) <> 'S' and
251400021205     c                             W_Tarparest = *Blanks and
251500021205     c                             oltre = *Zeros and TpdMin = *Zeros and
251600021205     c                             TpdMax = *Zeros and TptTpg <> '0' and
251700021205     c                             TptRpv = *Zeros and TpTArl = *Zeros
251800021205     c                   Add       1             Atp1
251900021205     c                   Z-Add     TpdItr        Witr
252000021205     c                   Eval      Tpta(Atp1) = Comstd
252100021205      * no percentuale
252200021205If  3c                   If        W_Per = ' '
252300021205     c                   Eval      Tptb(Atp1) = ': ' +
252400021205     c                                          Wrkdestpg + ' ' +
252500021205     c                                          %Editc(Witr:'3')
252600021205      * sì percentuale
252700021205   x3c                   Else
252800021205     c                   Eval      Tptb(Atp1) = ': ' +
252900021205     c                                          Wrkdestpg + ' ' +
253000021205     c                                          StpPer + ' % '
253100021205    3c                   EndIf
253200021205
253300021205      * se devo stampare righe aggiuntive o l'estensione della tariffa
253400021205      * particolare stampo anche la tariffa particolare
253500021205   x2c                   Else
253600020110
253700020110     c                   eval      comdstd = w_tarpar
253800020110
253900020110     c                   exsr      Sr_Over
254000020110
254100020110     c                   write     ta50pstd
254200020110
254300020110     c                   exsr      findet
254400021205    2c                   EndIf
254500020110     c                   else
254600020110
254700020110      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
254800020110     c                   exsr      Sr_Gestpd
254900020110
255000020110     c                   endif
255100020110
255200020110     c     sudtpstd      tag
255300020110
255400020110      * verifico se devo stampare righe ulteriori
255500020110     c                   if        esud(y) = 'S'
255600020110
255700020110     c                   exsr      Sr_Over
255800020110
255900020110      * se tariffa particolare espresso
256000020110     c                   if        ftc(y) = 'E'
256100020110     c                   write     ta50pse2
256200020110     c                   endif
256300020110
256400020110      * se tariffa particolare ritiro
256500020110     c                   if        ftc(y) = 'U'
256600020110     c                   write     ta50por2
256700020110     c                   endif
256800020110
256900020110      * se tariffa particolare provvigione vettore
257000020110     c                   if        ftc(y) = 'V'
257100020110
257200060704      * in lingua
257300060704     c                   select
257400060704     c                   when      i45lin = 'I'
257500020110     c                   eval      comint = 'Vettore'
257600060704     c                   when      i45lin = '2'
257700060704     c                   eval      comint = 'Carrier'
257800060801     c                   when      i45lin = '3'
257900060907     c                   eval      comint = ' du Transporteur'
258000060712     c                   when      i45lin = '4'
258100060726     c                   eval      comint = 'Frachtführe'
258200060704     c                   endsl
258300020110     c                   write     ta50ppc1
258400020110
258500020110      *  stampo le condizioni di rimborso solo se tipo pagamento che necessita
258600020110      * codice abi cab
258700020110     c                   if        §4uabi = 'S'
258800020110     c                   exsr      Sr_Over
258900020110     c                   write     ta50ppc2
259000020110     c                   endif
259100020110
259200020110     c                   endif
259300020110
259400020110      * se tariffa particolare provvigione mittente
259500020110     c                   if        ftc(y) = 'W'
259600060704      * in lingua
259700060704     c                   select
259800060704     c                   when      i45lin = 'I'
259900020110     c                   eval      comint = 'Mittente'
260000060704     c                   when      i45lin = '2'
260100060704     c                   eval      comint = 'Sender'
260200060801     c                   when      i45lin = '3'
260300060907     c                   eval      comint = 'de l''Expéditeur'
260400060712     c                   when      i45lin = '4'
260500060726     c                   eval      comint = 'Absender'
260600060704     c                   endsl
260700020110     c                   write     ta50ppc1
260800020110     c                   endif
260900040428
261000040428      * se tariffa particolare supplemento carburante
261100040428     c                   if        ftc(y) = '+'
261200040428     c                   write     ta50psc
261300040428
261400040428     c                   exsr      Sr_Over
261500040428     c                   write     ta50psc1
261600040428
261700040428     c                   exsr      Sr_Over
261800040428     c                   write     ta50psc2
261900040428
262000040428     c                   exsr      Sr_Over
262100040428     c                   write     ta50psc3
262200040428
262300040428     c                   endif
262400020110
262500020110     c                   endif
262600020110
262700020110     c                   setoff                                       06
262800020110
262900020110     c     endtpstd      endsr
263000020110
263100020110      *------------------------------------------------------------------*
263200020110      *  Sr_Elliris STAMPA DATI DELLA TARIFFA PARTICOLARE ELEVAZ.LIMITE RISARC.
263300020110      *------------------------------------------------------------------*
263400020110
263500020110     c     Sr_Elliris    begsr
263600020110
263700020110     c                   movel     'R '          savftc
263800020110
263900061011     C  n98KTPT          CHAIN     TITPT01l
264000061011     c   98ktpt          chain     tiopt01l
264100020110      * se non trovato vado a fine e non stampo nulla
264200061011     c                   if        not %found
264300020110     c                   goto      endelliris
264400020110     c                   endif
264500020110      * se trovato ma annullato vado a fine  e non stampo nulla
264600020110     c                   if        tptatb = 'A'
264700020110     c                   goto      endelliris
264800020110     c                   endif
264900020110      * se esiste stampo la scritta assicurazioni merci ed accendo indicatore
265000020110      * che mi segnala di aver stampato questa riga per non farlo dopo
265100020110     c                   exsr      Sr_Over
265200020110
265300020111     c                   write     ta50ptam
265400020110     c                   seton                                        81
265500020110      * recupero i valori standard dalla tabella GEI
265600020110     c                   clear                   dgei
265700020110     c                   movel     'C'           t02mod
265800020110     c                   movel     knsif         t02sif
265900020110     c                   movel     'GEI'         t02cod
266000020110
266100020110     c                   movel     §tadiv        t02ke1
266200020110
266300020110     c                   call      'TIBS02R'
266400020110     c                   parm                    kpjba
266500020110     c                   parm                    tibs02ds
266600020110
266700020110     c                   if        t02err = *blanks
266800020110     c                   movel     t02uni        dgei
266900020110     c                   endif
267000020110
267100020110      * verifico se limite risarcibile > di quello standard
267200020110     c                   if        tptvlm > §gelrp
267300020110     c                   seton                                        22
267400020110     c                   z-add     tptvlm        comelr
267500020110     c                   z-add     §gelrp        comlrp
267600020110
267700020110      * decodifico il flag valore merce
267800020110
267900020110     c                   movel     'TR'          cod
268000020110     c                   movel(P)  tptfvm        key
268100060704     c     tabkeyl       chain     tabel00f
268200020110
268300020110     c                   if        %found(tabel00f)
268400020110     c                   movel     tbluni        dstr
268500020110     c                   else
268600020110     c                   clear                   dstr
268700020110     c                   endif
268800020110
268900020110     c                   endif
269000020110      * validità mandato per le spedizioni in partenza e/o in arrivo
269100020110     c                   if        tptapl = ' '
269200060704      * in lingua
269300060704     c                   select
269400060704     c                   when      i45lin = 'I'
269500020110     c                   eval      comvma = 'PARTENZA e in ARRIVO'
269600060704     c                   when      i45lin = '2'
269700060704     c                   eval      comvma = 'DEPARTING and ARRIVING'
269800060726     c                   when      i45lin = '3'
269900060726     c                   eval      comvma = 'DÉPART ET ARRIVÉE'
270000060712     c                   when      i45lin = '4'
270100060726     c                   eval      comvma = 'ABFAHRT und EINGANG'
270200060704     c                   endsl
270300020110     c                   endif
270400020110      * partenza
270500020110     c                   if        tptapl = 'P'
270600060704      * in lingua
270700060704     c                   select
270800060704     c                   when      i45lin = 'I'
270900020110     c                   eval      comvma = 'PARTENZA'
271000060704     c                   when      i45lin = '2'
271100060704     c                   eval      comvma = 'DEPARTURE'
271200060726     c                   when      i45lin = '3'
271300060726     c                   eval      comvma = 'DÉPART'
271400060712     c                   when      i45lin = '4'
271500060726     c                   eval      comvma = 'ABFAHRT'
271600060704     c                   endsl
271700020110     c                   endif
271800020110      * arrivo
271900020110     c                   if        tptapl = 'A'
272000060704      * in lingua
272100060704     c                   select
272200060704     c                   when      i45lin = 'I'
272300020110     c                   eval      comvma = 'ARRIVO'
272400060704     c                   when      i45lin = '2'
272500060704     c                   eval      comvma = 'ARRIVAL'
272600060726     c                   when      i45lin = '3'
272700060726     c                   eval      comvma = 'ARRIVÉE'
272800060712     c                   when      i45lin = '4'
272900060726     c                   eval      comvma = 'EINGANG'
273000060704     c                   endsl
273100020110     c                   endif
273200020110
273300020110      * cerco se ha record validi con tariffa o minimo > 0
273400020110     c                   exsr      Ricerc
273500020110
273600020110     c                   if        *in22
273700020110     c                   exsr      Sr_Over
273800020110     c                   write     ta50pel1
273900020110     c                   else
274000020110     c                   exsr      Sr_Over
274100020110     c                   write     ta50pel2
274200020110     c                   endif
274300020110
274400020110      * per 04 acceso stampo nessun addebito
274500020110     c                   if        *in04
274600020110
274700060704     c                   select
274800060704     c                   when      i45lin = 'I'
274900020110     c                   eval      comma = ': nessun addebito'
275000060704     c                   when      i45lin = '2'
275100060704     c                   eval      comma = ': no charge      '
275200060728     c                   when      i45lin = '3'
275300060728     c                   eval      comma = ': aucun frais    '
275400060712     c                   when      i45lin = '4'
275500060728     c                   eval      comma = ': Keine Kosten   '
275600060704     c                   endsl
275700020110
275800020110     c                   exsr      Sr_Over
275900020110
276000020110      * elevazione limite risarcibile
276100020110     c                   write     ta50pma
276200020110     c                   write     ta50pvma
276300020110     c                   goto      endelliris
276400020110     c                   endif
276500020110
276600020110      * per 04 spento verifico quanti record dettaglio tariffario esistono
276700020110      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
276800020110     c                   if        recval = 1
276900020110     c                   exsr      Sr_Gesunico
277000020110      * se è tariffa a valore senza l'oltre e con elevazione stampo 1 riga <>
277100020110     c                   if        w_ellimris = 'Y'
277200020110
277300020110     c                   exsr      Sr_Over
277400020110
277500020110     c                   write     ta50pel
277600020110
277700020110     c                   else
277800020110
277900020110     c                   eval      comma = w_tarpar
278000020110
278100020110     c                   exsr      Sr_Over
278200020110
278300020110     c                   write     ta50pma
278400020110
278500020110     c                   endif
278600020110
278700020110      * stampo eventuale estensione scaglione unico
278800020110     c                   exsr      Sr_Estes
278900020110
279000020111     c                   exsr      Findet
279100020110
279200020110     c                   else
279300020110
279400020110      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
279500020110
279600020110     c                   exsr      Sr_Gestpd
279700020110
279800020110     c                   endif
279900020110      * stampo l'ultima riga
280000020110
280100020110     c                   exsr      Sr_Over
280200020110     c                   write     ta50pvma
280300020110
280400020110     c     endelliris    endsr
280500020110
280600020110      *------------------------------------------------------------------*
280700020110      *  Sr_Mandato STAMPA DATI DELLA TARIFFA PARTICOLARE ASSICURAZIONE X CONTO
280800020110      *------------------------------------------------------------------*
280900020110
281000020110     c     Sr_Mandato    begsr
281100020110
281200020110     c                   setoff                                       2124
281300020110
281400020110     c                   movel     'C '          savftc
281500020110
281600061011     c  n98ktpt          chain     titpt01l
281700061011     c   98ktpt          chain     tiopt01l
281800020110      * se non trovato vado a fine e non stampo nulla
281900061011     c                   if        not %found
282000020110     c                   goto      endmandato
282100020110     c                   endif
282200020110      * se trovato ma annullato vado a fine  e non stampo nulla
282300020110     c                   if        tptatb = 'A'
282400020110     c                   goto      endmandato
282500020110     c                   endif
282600020110      * se esiste stampo la scritta assicurazioni merci solo se 81 spento
282700020110      * cioè non l'ho già stampata prima
282800020110     c                   exsr      Sr_Over
282900020110
283000020110     c  n81              write     ta50PTAM
283100020110     c  n81              seton                                        81
283200020110      * verifico se mandato fittizio
283300020110     c                   if        tpttma = 'F'
283400020110     c                   seton                                        24
283500020110     c                   endif
283600020110      * verifico se mandato a valore variabile
283700020110     c                   if        tptvlm = 0
283800020110     c                   seton                                        21
283900020110     c                   endif
284000060313      * mi salvo il tptapl x stampa AC BASE
284100060313     c                   eval      savapl = tptapl
284200020110      * validità mandato per le spedizioni in partenza e/o in arrivo
284300020110     c                   if        tptapl = ' '
284400060704      * in lingua
284500060704     c                   select
284600060704     c                   when      i45lin = 'I'
284700020110     c                   eval      comvma = 'PARTENZA e in ARRIVO'
284800060704     c                   when      i45lin = '2'
284900060704     c                   eval      comvma = 'DEPARTING and ARRIVING'
285000060728     c                   when      i45lin = '3'
285100060728     c                   eval      comvma = 'DÉPART ET ARRIVÉE'
285200060712     c                   when      i45lin = '4'
285300060728     c                   eval      comvma = 'ABFAHRT und EINGANG'
285400060704     c                   endsl
285500020110     c                   endif
285600020110      * partenza
285700020110     c                   if        tptapl = 'P'
285800060704      * in lingua
285900060704     c                   select
286000060704     c                   when      i45lin = 'I'
286100020110     c                   eval      comvma = 'PARTENZA'
286200060704     c                   when      i45lin = '2'
286300060704     c                   eval      comvma = 'DEPARTURE'
286400060728     c                   when      i45lin = '3'
286500060728     c                   eval      comvma = 'DÉPART'
286600060712     c                   when      i45lin = '4'
286700060728     c                   eval      comvma = 'ABFAHRT'
286800060704     c                   endsl
286900020110     c                   endif
287000020110      * arrivo
287100020110     c                   if        tptapl = 'A'
287200060704      * in lingua
287300060704     c                   select
287400060704     c                   when      i45lin = 'I'
287500020110     c                   eval      comvma = 'ARRIVO'
287600060704     c                   when      i45lin = '2'
287700060704     c                   eval      comvma = 'ARRIVAL'
287800060728     c                   when      i45lin = '3'
287900060728     c                   eval      comvma = 'ARRIVÉE'
288000060712     c                   when      i45lin = '4'
288100060728     c                   eval      comvma = 'EINGANG'
288200060704     c                   endsl
288300020110     c                   endif
288400020110      * se non è mandato a valore variabile e non è fittizio
288500020110     c                   if        not *in21 and not *in24
288600020110     c                   z-add     tptvlm        comvlm
288700020111
288800020110      * decodifico il flag valore merce
288900020110     c                   movel     'TR'          cod
289000020110     c                   movel(P)  tptfvm        key
289100060704     c     tabkeyl       chain     tabel00f
289200020110
289300020110     c                   if        %found(tabel00f)
289400020110     c                   movel     tbluni        dstr
289500020110     c                   else
289600020110     c                   clear                   dstr
289700020110     c                   endif
289800020110
289900020110     c                   movel(p)  §trde2        desfvm
290000020110
290100020110     c                   endif
290200020110
290300020110      * cerco se ha record validi con tariffa o minimo > 0
290400020110     c                   exsr      Ricerc
290500020110
290600020110      * mandato a valore variabile o con valore espresso (no fittizio)
290700020110     c                   if        not *in24
290800020110      * se mandato non a valore variabile
290900020110     c                   if        not *in21
291000020110     c                   write     ta50pma1
291100020110     c                   else
291200020110     c                   write     ta50pmav
291300020110     c                   endif
291400020110
291500020110     c                   else
291600020110     c                   write     ta50pmaf1
291700020110     c                   endif
291800020110      * per 04 acceso stampo nessun addebito
291900020110     c                   if        *in04
292000020110      * mandato a valore variabile o con valore espresso (no fittizio)
292100060704     c                   select
292200060704     c                   when      i45lin = 'I'
292300020110     c                   eval      comma = ': nessun addebito'
292400060704     c                   when      i45lin = '2'
292500060704     c                   eval      comma = ': no charge      '
292600060728     c                   when      i45lin = '3'
292700060728     c                   eval      comma = ': aucun frais    '
292800060712     c                   when      i45lin = '4'
292900060728     c                   eval      comma = ': Keine Kosten   '
293000060704     c                   endsl
293100020110
293200020110     c                   exsr      Sr_Over
293300020110
293400020110     c                   write     ta50pma
293500020110     c                   write     ta50pvma
293600020110     c                   goto      endmandato
293700020110     c                   endif
293800020110
293900020110      * per 04 spento verifico quanti record dettaglio tariffario esistono
294000020110      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
294100020110     c                   if        recval = 1
294200020110     c                   exsr      Sr_Gesunico
294300020110
294400020110     c                   eval      comma = w_tarpar
294500020110
294600020110     c                   exsr      Sr_Over
294700020110
294800020110     c                   write     ta50pma
294900020110
295000020110     c                   exsr      findet
295100020110
295200020110     c                   else
295300020110
295400020110      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
295500020110     c                   exsr      Sr_Gestpd
295600020110
295700020110     c                   endif
295800020111
295900020110      * stampo l'ultima riga
296000020110     c                   exsr      Sr_Over
296100020110     c                   write     ta50pvma
296200020110
296300020110     c     endmandato    endsr
296400020110
296500060313      *-------------------------------------------------------------------------
296600060313      *  sr_acbase STAMPA DATI DELLA TARIFFA PARTICOLARE d
296700060313      *-------------------------------------------------------------------------
296800060313     c     sr_acbase     begsr
296900060313
297000060313      * la tariffa ac base non può essere fittizia e non può essere a mandato
297100060313      * variabile
297200060313
297300060313     c                   movel     'd '          savftc
297400060313
297500061011     c  n98ktpt          chain     titpt01l
297600061011     c   98ktpt          chain     tiopt01l
297700060313      * se non trovato vado a fine e non stampo nulla
297800061011     c                   if        not %found
297900060313     c                   goto      endacbase
298000060313     c                   endif
298100060313      * se trovato ma annullato vado a fine  e non stampo nulla
298200060313     c                   if        tptatb = 'A'
298300060313     c                   goto      endacbase
298400060313     c                   endif
298500060313      * se esiste stampo la scritta assicurazioni merci solo se 81 spento
298600060313      * cioè non l'ho già stampata prima
298700060313
298800060313     c                   exsr      SR_OVER
298900060313
299000060313     c  n81              write     ta50PTAM
299100060313     c  n81              seton                                        81
299200060313      * validità mandato per le spedizioni in partenza e/o in arrivo
299300060313     c                   if        tptapl = ' '
299400060704      * in lingua
299500060704     c                   select
299600060704     c                   when      i45lin = 'I'
299700060313     c                   eval      comvma = 'PARTENZA e in ARRIVO'
299800060704     c                   when      i45lin = '2'
299900060704     c                   eval      comvma = 'DEPARTING and ARRIVING'
300000060728     c                   when      i45lin = '3'
300100060728     c                   eval      comvma = 'DÉPART ET ARRIVÉE'
300200060712     c                   when      i45lin = '4'
300300060728     c                   eval      comvma = 'ABFAHRT und EINGANG'
300400060704     c                   endsl
300500060313     c                   endif
300600060313      * partenza
300700060313     c                   if        tptapl = 'P'
300800060704      * in lingua
300900060704     c                   select
301000060704     c                   when      i45lin = 'I'
301100060313     c                   eval      comvma = 'PARTENZA'
301200060704     c                   when      i45lin = '2'
301300060704     c                   eval      comvma = 'DEPARTURE'
301400060728     c                   when      i45lin = '3'
301500060728     c                   eval      comvma = 'DÉPART'
301600060712     c                   when      i45lin = '4'
301700060728     c                   eval      comvma = 'ABFAHRT'
301800060704     c                   endsl
301900060313     c                   endif
302000060313      * arrivo
302100060313     c                   if        tptapl = 'A'
302200060704      * in lingua
302300060704     c                   select
302400060704     c                   when      i45lin = 'I'
302500060313     c                   eval      comvma = 'ARRIVO'
302600060704     c                   when      i45lin = '2'
302700060704     c                   eval      comvma = 'ARRIVAL'
302800060728     c                   when      i45lin = '3'
302900060728     c                   eval      comvma = 'ARRIVÉE'
303000060712     c                   when      i45lin = '4'
303100060728     c                   eval      comvma = 'EINGANG'
303200060704     c                   endsl
303300060313     c                   endif
303400060313      * se AC PLUS valida in partenza stampo ARRIVO su AC BASE
303500060313     c                   if        savapl = 'P'
303600060704      * in lingua
303700060704     c                   select
303800060704     c                   when      i45lin = 'I'
303900060313     c                   eval      comvma = 'ARRIVO'
304000060704     c                   when      i45lin = '2'
304100060704     c                   eval      comvma = 'ARRIVAL'
304200060728     c                   when      i45lin = '3'
304300060728     c                   eval      comvma = 'ARRIVÉE'
304400060712     c                   when      i45lin = '4'
304500060728     c                   eval      comvma = 'EINGANG'
304600060704     c                   endsl
304700060313     c                   endif
304800060313      * se AC PLUS valida in arrivo stampo PARTENZA su AC BASE
304900060313     c                   if        savapl = 'A'
305000060704      * in lingua
305100060704     c                   select
305200060704     c                   when      i45lin = 'I'
305300060313     c                   eval      comvma = 'PARTENZA'
305400060704     c                   when      i45lin = '2'
305500060704     c                   eval      comvma = 'DEPARTURE'
305600060728     c                   when      i45lin = '3'
305700060728     c                   eval      comvma = 'DÉPART'
305800060712     c                   when      i45lin = '4'
305900060728     c                   eval      comvma = 'ABFAHRT'
306000060704     c                   endsl
306100060313     c                   endif
306200060313
306300060313     c                   z-add     tptvlm        comvlm
306400060313      *
306500060313      * decodifico il flag valore merce
306600060313      *
306700060313     c                   movel     'TR'          cod
306800060313     c                   movel(p)  tptfvm        key
306900060704     c     tabkeyl       chain     tabel00f
307000060313
307100060313     c                   if        %found(tabel00f)
307200060313     c                   movel     tbluni        dstr
307300060313     c                   else
307400060313     c                   clear                   dstr
307500060313     c                   endif
307600060313
307700060313     c                   movel(p)  §trde2        desfvm
307800060313
307900060313      * cerco se ha record validi con tariffa o minimo > 0
308000060313     c                   EXSR      Ricerc
308100060313     c                   write     ta50pma2
308200060313      * per 04 acceso stampo nessun addebito
308300060313     c                   if        *in04
308400060313      * mandato a valore variabile o con valore espresso (no fittizio)
308500060704     c                   select
308600060704     c                   when      i45lin = 'I'
308700060313     c                   eval      comma = ': nessun addebito'
308800060704     c                   when      i45lin = '2'
308900060704     c                   eval      comma = ': no charge      '
309000060728     c                   when      i45lin = '3'
309100060728     c                   eval      comma = ': aucun frais    '
309200060728     c                   when      i45lin = '4'
309300060728     c                   eval      comma = ': Keine Kosten   '
309400060704     c                   endsl
309500060313      *
309600060313     c                   exsr      SR_OVER
309700060313      *
309800060313     c                   write     ta50pma
309900060313     c                   write     ta50pvma
310000060313     c                   goto      endacbase
310100060313     c                   endif
310200060313      *
310300060313      * per 04 spento verifico quanti record dettaglio tariffario esistono
310400060313      * se maggiori di 1 gestisco gli scaglioni se unico eseguo un'altra routine
310500060313     c                   if        recval = 1
310600060313     C                   exsr      sr_gesunico
310700060313
310800060313     c                   eval      comma = w_tarpar
310900060313
311000060313     c                   exsr      SR_OVER
311100060313
311200060313     c                   write     ta50pma
311300060313
311400060313     c                   exsr      findet
311500060313
311600060313     c                   else
311700060313
311800060313      * per 04 spento vuol dire che ho trovato la tariffa valorizzata
311900060313     c                   exsr      sr_gestpd
312000060313
312100060313     c                   endif
312200060313
312300060313      * stampo l'ultima riga
312400060313     c                   exsr      SR_OVER
312500060313     c                   write     ta50pvma
312600060313      *
312700060313     C     endacbase     endsr
312800060313      *
312900020110      *------------------------------------------------------------------*
313000020110      * Sr_Gestpd GESTIONE DEL DETTAGLIO TARIFFARIO UGUALE PER TUTTE LE TAR.PAR.
313100020110      *------------------------------------------------------------------*
313200020110
313300020110     c     Sr_Gestpd     begsr
313400020110
313500020110      * Decodfico tipo tariffa e scaglioni
313600020110     c                   movel     'TR'          cod
313700020111
313800020110     c                   clear                   atf
313900020110     c                   exsr      Dectpg
314000020110      * elaboro 6 scaglioni alla volta
314100020110     c                   z-add     1             k4
314200020110
314300020110    1c                   dow       k4 <= 18
314400020110     c                   MOVEA     SGLD(K4)      SSLD
314500020110     c                   MOVEA     SGLB(K4)      SSLB
314600020110     c                   MOVEA     SGLA(K4)      SSLA
314700020110      * 1 DECIMALE
314800020110     c                   IF        *IN91 AND NOT *IN37
314900020110     c                   MOVEA     SG1A(K4)      SS1A
315000020110     c                   MOVEA     SG1D(K4)      SS1D
315100020110     c                   ENDIF
315200020110      * 2 DECIMALI
315300020110     c                   IF        *IN91 AND *IN37
315400020110     c                   MOVEA     SG2A(K4)      SS2A
315500020110     c                   MOVEA     SG2D(K4)      SS2D
315600020110     c                   ENDIF
315700020110
315800020110      * CARICO GLI SCAGLIONI PER ELABORARE IL DETTAGLIO
315900020110
316000020110     c                   MOVEA     SCP(K4)       SSL
316100020110      *  INTESTAZIONE SCAGLIONI
316200020110     c                   Z-ADD     SSL(1)        W0050             5 0
316300020110     c     W0050         COMP      99999                                  41
316400020110     c     SSL(1)        COMP      *ZEROS                                 51
316500020110     c                   Z-ADD     SSL(2)        W0050             5 0
316600020110     c     W0050         COMP      99999                                  42
316700021220     c**N41W0050         COMP      *zeros                                 41
316800020110     c     SSL(2)        COMP      *ZEROS                                 52
316900020110     c                   Z-ADD     SSL(3)        W0050             5 0
317000020110     c     W0050         COMP      99999                                  43
317100020110     c     SSL(3)        COMP      *ZEROS                                 53
317200020110     c                   Z-ADD     SSL(4)        W0050             5 0
317300020110     c     W0050         COMP      99999                                  44
317400020110     c     SSL(4)        COMP      *ZEROS                                 54
317500020110     c                   Z-ADD     SSL(5)        W0050             5 0
317600020110     c     W0050         COMP      99999                                  45
317700020110     c     SSL(5)        COMP      *ZEROS                                 55
317800020110     c                   Z-ADD     SSL(6)        W0050             5 0
317900020110     c     W0050         COMP      99999                                  46
318000020110     c     SSL(6)        COMP      *ZEROS                                 56
318100020110
318200020110      * SE IL 41 E' ACCESO PERCHÈ IL PRIMO SCAGLIONE E' UGUALE A TUTTI 999999
318300020110      * O IL SECONDO SCAGLIONE E' A ZERO
318400020110      * MA SE NON SI TRATTA + DEL 1° SCAGLIONE IN ASSOLUTO DEVO STAMPARE "OLTRE
318500020110      * LO SCAGLIONE PRECEDENTE
318600020110      * QUINDI ACCENDO IL 40 E SPENGO IL 41
318700020110
318800020110     c                   IF        *IN41 AND K4 <> 1
318900020110     c                   SETOFF                                       41
319000020110     c                   SETON                                        40
319100020110     c                   ENDIF
319200020110
319300020110     c                   exsr      Decsca
319400020110      * LETTURA DEL DETTAGLIO
319500020110     c                   exsr      Legdet
319600020110
319700020110      * rielaboro altri 6 scaglioni
319800020110     c                   add       6             k4
319900020110      * se scaglioni minori o uguali a 18 e scaglione valorizzato
320000020110    2c                   if        k4 <= 18 and
320100020110     c                             scp(k4) <> *zeros
320200020110      * pulisco schiere di comodo per la stampa degli scaglioni
320300020110     c                   clear                   ssld
320400020110     c                   clear                   sslb
320500020110     c                   clear                   ssla
320600020110     c                   clear                   ss1d
320700020110     c                   clear                   ss1a
320800020110     c                   clear                   ss2d
320900020110     c                   clear                   ss2a
321000020110     c                   clear                   ssl
321100020110      * spengo indicatori che condizionano la stampa
321200020110     c                   setoff                                       404142
321300020110     c                   setoff                                       434445
321400020110     c                   setoff                                       46
321500020110     c                   setoff                                       515253
321600020110     c                   setoff                                       545556
321700020110
321800020110     c                   else
321900020110      * non ci sono + scaglioni da stampare
322000020110     c                   eval      k4=19
322100020110     c                   leave
322200020110
322300020110    2c                   endif
322400020110
322500020110    1c                   enddo
322600020110
322700020110     c                   exsr      Findet
322800020110
322900020110     c                   endsr
323000020110
323100020110      *------------------------------------------------------------------*
323200020110      * Sr_Gesunico       GESTIONE DETTAGLIO TARIFFARIO CON UNA SOLA RIGA
323300020110      *------------------------------------------------------------------*
323400020110
323500020110     c     Sr_Gesunico   begsr
323600020110
323700020110     c                   clear                   w_tarpar
323800020110     c                   clear                   w_tarparest
323900020110     c                   clear                   w_ellimris
324000020110      * Decodfico tipo pagamento
324100020110     c                   movel     'TR'          cod
324200020110     c                   movel(p)  tpttpg        key
324300060704     c     tabkeyl       chain     tabel00f
324400020110
324500020110     c                   if        %found(tabel00f)
324600020110     c                   movel     tbluni        dstr
324700020110     c                   else
324800020110     c                   clear                   dstr
324900020110     c                   endif
325000020110
325100020110      * verifico se scaglioni a valore
325200020110     c                   if        §trtap = 'S'
325300020110     c                   eval      W_per = '%'
325400020110     c                   else
325500020110     c                   clear                   w_per
325600020110     c                   endif
325700020110
325800020110      * decodifico a mio modo il tipo pagamento
325900020110     c                   clear                   wrkdestpg
326000020110
326100060704      * in lingua
326200060704     c                   select
326300060728      * italiano
326400060704     c                   when      i45lin = 'I'
326500020110     c                   if        tpttpg = 'T'
326600020110     c                   eval      wrkdestpg = 'SUL TRASPORTO'
326700020110     c                   endif
326800020110
326900020110     c                   if        tpttpg = 'V'
327000020110     c                   eval      wrkdestpg = 'SUL VALORE   '
327100020110     c                   endif
327200020110
327300020110     c                   if        tpttpg = '0'
327400020110     c                   eval      wrkdestpg = 'A Q.LE       '
327500020110     c                   endif
327600020110
327700020110     c                   if        tpttpg = '1'
327800020110     c                   eval      wrkdestpg = 'A COLLO      '
327900020110     c                   endif
328000020110
328100020110     c                   if        tpttpg = '2'
328200020110     c                   eval      wrkdestpg = 'A SPEDIZIONE '
328300020110     c                   endif
328400020110
328500020110     c                   if        tpttpg = '3'
328600020110     c                   eval      wrkdestpg = 'A KG.        '
328700020110     c                   endif
328800020110
328900020110     c                   if        tpttpg = '4'
329000020110     c                   eval      wrkdestpg = 'A VALORE     '
329100020110     c                   endif
329200020110
329300020110     c                   if        tpttpg = '5'
329400020110     c                   eval      wrkdestpg = 'A QUANTITA'' '
329500020110     c                   endif
329600021210      * Tar.particolare Diritto Fatturazione
329700030207     c                   If        TptFtc = '§'
329800021210     c                   Eval      WrkDesTpg = 'A FATTURA   '
329900021210     c                   EndIf
330000021210      * Tar.particolare Gest.bancali a rendere
330100021210     c                   If        TptFtc = '='
330200021210     c                   Eval      WrkDesTpg = 'A BANCALE   '
330300021210     c                   EndIf
330400151118      * Tar.particolare Diritto di Chiamata prenotazione ritiro telefonico
330500151118     c                   If        TptFtc = 't'
330600151118     c                   Eval      WrkDesTpg = 'A RITIRO    '
330700151118     c                   EndIf
330800060704
330900060728      * inglese
331000060704     c                   when      i45lin = '2'
331100060704     c                   if        tpttpg = 'T'
331200060704     c                   eval      wrkdestpg = 'ON TRANSPORT '
331300060704     c                   endif
331400060704
331500060704     c                   if        tpttpg = 'V'
331600060704     c                   eval      wrkdestpg = 'ON VALUE     '
331700060704     c                   endif
331800060704
331900060704     c                   if        tpttpg = '0'
332000060704     c                   eval      wrkdestpg = 'PER 100 KG.  '
332100060704     c                   endif
332200060704
332300060704     c                   if        tpttpg = '1'
332400060704     c                   eval      wrkdestpg = 'PER PARCEL   '
332500060704     c                   endif
332600060704
332700060704     c                   if        tpttpg = '2'
332800060704     c                   eval      wrkdestpg = 'PER SHIPMENT '
332900060704     c                   endif
333000060704
333100060704     c                   if        tpttpg = '3'
333200060704     c                   eval      wrkdestpg = 'PER KG.      '
333300060704     c                   endif
333400060704
333500060704     c                   if        tpttpg = '4'
333600060704     c                   eval      wrkdestpg = 'BY VALUE     '
333700060704     c                   endif
333800060704
333900060704     c                   if        tpttpg = '5'
334000060704     c                   eval      wrkdestpg = 'BY QUANTITY  '
334100060704     c                   endif
334200060704      * Tar.particolare Diritto Fatturazione
334300060704     c                   If        TptFtc = '§'
334400060704     c                   Eval      WrkDesTpg = 'PER INVOICE '
334500060704     c                   EndIf
334600060704      * Tar.particolare Gest.bancali a rendere
334700060704     c                   If        TptFtc = '='
334800060704     c                   Eval      WrkDesTpg = 'PER PALLET  '
334900060704     c                   EndIf
335000151118      * Tar.particolare Diritto di Chiamata prenotazione ritiro telefonico
335100151118     c                   If        TptFtc = 't'
335200151118     c                   Eval      WrkDesTpg = 'PER PICK-UP '
335300151118     c                   EndIf
335400060728
335500060728      * francese
335600060728     c                   when      i45lin = '3'
335700060728     c                   if        tpttpg = 'T'
335800060925     c                   eval      wrkdestpg = 'SUR LE TRANSP'
335900060728     c                   endif
336000060728
336100060728     c                   if        tpttpg = 'V'
336200060728     c                   eval      wrkdestpg = 'SUR LA VALEUR'
336300060728     c                   endif
336400060728
336500060728     c                   if        tpttpg = '0'
336600060728     c                   eval      wrkdestpg = 'AUX 100 KG   '
336700060728     c                   endif
336800060728
336900060728     c                   if        tpttpg = '1'
337000060728     c                   eval      wrkdestpg = 'AU COLIS     '
337100060728     c                   endif
337200060728
337300060728     c                   if        tpttpg = '2'
337400060728     c                   eval      wrkdestpg = 'PAR ENVOI    '
337500060728     c                   endif
337600060728
337700060728     c                   if        tpttpg = '3'
337800060728     c                   eval      wrkdestpg = 'AU KG        '
337900060728     c                   endif
338000060728
338100060728     c                   if        tpttpg = '4'
338200060728     c                   eval      wrkdestpg = 'A LA VALEUR  '
338300060728     c                   endif
338400060728
338500060728     c                   if        tpttpg = '5'
338600060728     c                   eval      wrkdestpg = 'A LA QUANTITÉ'
338700060728     c                   endif
338800060728      * Tar.particolare Diritto Fatturazione
338900060728     c                   If        TptFtc = '§'
339000060728     c                   Eval      WrkDesTpg = 'A FATTURA   '
339100060728     c                   EndIf
339200060728      * Tar.particolare Gest.bancali a rendere
339300060728     c                   If        TptFtc = '='
339400060925     c                   Eval      WrkDesTpg = 'A LA PALETTE'
339500060728     c                   EndIf
339600151118      * Tar.particolare Diritto di Chiamata prenotazione ritiro telefonico
339700151118     c                   If        TptFtc = 't'
339800151118     c                   Eval      WrkDesTpg = 'PAR RETRAIT '
339900151118     c                   EndIf
340000060712
340100060728      * tedesco
340200060712     c                   when      i45lin = '4'
340300060712     c                   if        tpttpg = 'T'
340400060728     c                   eval      wrkdestpg = 'AUF TRANSPORT'
340500060712     c                   endif
340600060712
340700060712     c                   if        tpttpg = 'V'
340800060728     c                   eval      wrkdestpg = 'AUF WERT     '
340900060712     c                   endif
341000060712
341100060712     c                   if        tpttpg = '0'
341200060728     c                   eval      wrkdestpg = 'N. ZENTNER   '
341300060712     c                   endif
341400060712
341500060712     c                   if        tpttpg = '1'
341600060728     c                   eval      wrkdestpg = 'N. FRACHTSTÜCK'
341700060712     c                   endif
341800060712
341900060712     c                   if        tpttpg = '2'
342000060728     c                   eval      wrkdestpg = 'N. SENDUNG   '
342100060712     c                   endif
342200060712
342300060712     c                   if        tpttpg = '3'
342400060728     c                   eval      wrkdestpg = 'N. KG        '
342500060712     c                   endif
342600060712
342700060712     c                   if        tpttpg = '4'
342800060728     c                   eval      wrkdestpg = 'N. WERT      '
342900060712     c                   endif
343000060712
343100060712     c                   if        tpttpg = '5'
343200060728     c                   eval      wrkdestpg = 'N. MENGE     '
343300060712     c                   endif
343400060712      * Tar.particolare Diritto Fatturazione
343500060712     c                   If        TptFtc = '§'
343600060712     c                   Eval      WrkDesTpg = 'A FATTURA   '
343700060712     c                   EndIf
343800060712      * Tar.particolare Gest.bancali a rendere
343900060712     c                   If        TptFtc = '='
344000060925     c                   Eval      WrkDesTpg = 'N.PALETTE   '
344100060712     c                   EndIf
344200151118      * Tar.particolare Diritto di Chiamata prenotazione ritiro telefonico
344300151118     c                   If        TptFtc = 't'
344400151118     c                   Eval      WrkDesTpg = 'N. BUCHUNG  '
344500151118     c                   EndIf
344600060704     c                   endsl
344700020110
344800020110      * CERCO IL RECORD VALIDO
344900061011     c  n98ktpt          setll     titpd01l
345000061011     c   98ktpt          setll     tiopd01l
345100020110
345200020110     c                   do        *hival
345300061011     c  n98ktpt          reade     titpd01l
345400061011     c   98ktpt          reade     tiopd01l
345500020110
345600061011     c                   if        %eof
345700020110     c                   leave
345800020110     c                   endif
345900020110
346000020110     c                   if        tpdatb = 'A'
346100020110     c                   iter
346200020110     c                   endif
346300020110
346400020110      * se tariffa non valorizzata leggo la successiva
346500020110      * importo e minimo =  a zero
346600020110     c                   if        tpditr = 0 and tpdmin = 0
346700020110     c                   iter
346800020110     c                   else
346900020110      * se importo con percentuale verifico quanti decimali ha
347000020110     c                   if        W_per = '%'
347100020110     c                   z-add     tpditr        valper
347200020110     c                   eval      stpper = %EDITC(valper : '3')
347300020110      * VERIFICO SE HA DECIMALI VALORIZZATI
347400020110     c                   move      valper        w0030
347500020110     c                   move      w0030         w0020
347600020110     c                   move      w0020         w0010
347700020110      * tolgo gli zeri non significativi dopo la virgola
347800020110     c                   select
347900020110
348000020110     c                   when      w0030 = *zeros
348100020110     c                   move      '    '        stpper
348200020110
348300020110     c                   when      w0020 = *zeros
348400020110     c                   move      '  '          stpper
348500020110
348600020110     c                   when      w0010 = *zero
348700020110     c                   move      ' '           stpper
348800020110
348900020110     c                   endsl
349000020110
349100020110     c                   endif
349200020110      * se sto stampando l'elevazione limite risarcibile mi devo comportare
349300020111      * diversamente se tariffa a valore e non è definito OLTRE e c'e
349400020110      * elevazione al limite
349500020110     c                   if        w_per = '%' and savftc = 'R ' and oltre = 0
349600020110     c                             and *in22
349700020110      * non definiscp W_tarpar in quanto gestisco in modo diverso
349800020110      * valorizzo il campo W_ellimris
349900020110     c                   eval      w_ellimris = 'Y'
350000020110
350100020110      * se esiste minimo e massimo devo valorizzare l'estensione
350200020110
350300020110      * solo minimo
350400020110     c                   if        tpdmin >  0 and tpdmax = 0
350500060704      * in lingua
350600060704     c                   select
350700060704     c                   when      i45lin = 'I'
350800021205     c                   eval      w_tarparest = blk56
350900020110     C                             + ' minimo........' +
351000020110     c                             %editc(tpdmin:'3')
351100060704     c                   when      i45lin = '2'
351200060704     c                   eval      w_tarparest = blk56
351300060704     c                             + ' minimum.......' +
351400060704     c                             %editc(tpdmin:'3')
351500060728     c                   when      i45lin = '3'
351600060728     c                   eval      w_tarparest = blk56
351700060728     c                             + ' minimum.......' +
351800060728     c                             %editc(tpdmin:'3')
351900060712     c                   when      i45lin = '4'
352000060712     c                   eval      w_tarparest = blk56
352100060728     c                             + ' minde.........' +
352200060712     c                             %editc(tpdmin:'3')
352300060704     c                   endsl
352400020110     c                   endif
352500020110
352600020110      * solo massimo
352700020110     c                   if        tpdmin =  0 and tpdmax > 0
352800060704     c                   select
352900060704     c                   when      i45lin = 'I'
353000021205     c                   eval      w_tarparest = blk56
353100020110     c                             + ' massimo.......' +
353200020110     c                             %editc(tpdmax:'3')
353300060704     c                   when      i45lin = '2'
353400060704     c                   eval      w_tarparest = blk56
353500060704     c                             + ' maximum.......' +
353600060704     c                             %editc(tpdmax:'3')
353700060728     c                   when      i45lin = '3'
353800060728     c                   eval      w_tarparest = blk56
353900060728     c                             + ' maximum.......' +
354000060728     c                             %editc(tpdmax:'3')
354100060712     c                   when      i45lin = '4'
354200060712     c                   eval      w_tarparest = blk56
354300060728     c                             + ' höchste.......' +
354400060712     c                             %editc(tpdmax:'3')
354500060704     c                   endsl
354600020110     c                   endif
354700020110
354800020110      * sia mino che massimo
354900020110     c                   if        tpdmin >  0 and tpdmax > 0
355000060704      * in lingua
355100060704     c                   select
355200060704     c                   when      i45lin = 'I'
355300030318     c                   eval      w_tarparest = blk10 + blk10 + blk12
355400060908     c                             + '...... minimo...' +
355500020110     c                             %editc(tpdmin:'3')
355600020110     c                             + ' massimo..' +
355700020110     c                             %editc(tpdmax:'3')
355800060704     c                   when      i45lin = '2'
355900060704     c                   eval      w_tarparest = blk10 + blk10 + blk12
356000060908     c                             + '...... minimum..' +
356100060704     c                             %editc(tpdmin:'3')
356200060704     c                             + ' maximum..' +
356300060704     c                             %editc(tpdmax:'3')
356400060728     c                   when      i45lin = '3'
356500060728     c                   eval      w_tarparest = blk10 + blk10 + blk12
356600060908     c                             + '...... minimum..' +
356700060728     c                             %editc(tpdmin:'3')
356800060728     c                             + ' maximum..' +
356900060728     c                             %editc(tpdmax:'3')
357000060712     c                   when      i45lin = '4'
357100060712     c                   eval      w_tarparest = blk10 + blk10 + blk12
357200060908     c                             + '...... minde....' +
357300060712     c                             %editc(tpdmin:'3')
357400060728     c                             + ' höchste..' +
357500060712     c                             %editc(tpdmax:'3')
357600060704     c                   endsl
357700020110     c                   endif
357800020110
357900020110     c                   leave
358000020110     c                   endif
358100020110
358200020110      * verifico se devo stampare OLTRE
358300020110     c                   if        oltre > 0
358400020110      * controllo se tariffa a q.le
358500020110     c                   if        tpttpg = '0'
358600020110     c                   eval      unmis = 'KG.'
358700020110     c                   else
358800020110     c                   clear                   unmis
358900020110     c                   endif
359000020110
359100020110      * stampo nella prima riga il valore e nella riga di estensione se ci sono
359200020110      * stampo il minimo e massimo
359300020110
359400020110      * no percentuale
359500020110     c                   if        w_per = ' '
359600060704      * in lingua
359700060704     c                   select
359800060704     c                   when      i45lin = 'I'
359900020110     c                   eval      w_tarpar = ': ' + wrkdestpg +
360000020110     c                             'oltre ' + (%editc(oltre:'3'))+
360100030318     c                             ' ' + unmis + blk10 + blk10 + blk12
360200021205     c                                                 + %editc(tpditr:'3')
360300060704     c                   when      i45lin = '2'
360400060704     c                   eval      w_tarpar = ': ' + wrkdestpg +
360500060704     c                             'over  ' + (%editc(oltre:'3'))+
360600060704     c                             ' ' + unmis + blk10 + blk10 + blk12
360700060704     c                                                 + %editc(tpditr:'3')
360800060801     c                   when      i45lin = '3'
360900060801     c                   eval      w_tarpar = ': ' + wrkdestpg +
361000060801     c                             'plus  ' + (%editc(oltre:'3'))+
361100060801     c                             ' ' + unmis + blk10 + blk10 + blk12
361200060801     c                                                 + %editc(tpditr:'3')
361300060712     c                   when      i45lin = '4'
361400060712     c                   eval      w_tarpar = ': ' + wrkdestpg +
361500060728     c                             'mehrals ' + (%editc(oltre:'3'))+
361600060728     c                             ' ' + unmis + blk10 + blk10 + blk10
361700060712     c                                                 + %editc(tpditr:'3')
361800060704     c                   endsl
361900020110     c                   else
362000020110      * si percentuale
362100060704      * in lingua
362200060704     c                   select
362300060704     c                   when      i45lin = 'I'
362400020110     c                   eval      w_tarpar = ': ' +
362500020110     c                             wrkdestpg + ' ' + stpper + ' % ' +
362600020110     c                             'oltre ' + (%editc(oltre:'3'))+
362700030318     c                             ' ' + unmis + blk21
362800060704     c                   when      i45lin = '2'
362900060704     c                   eval      w_tarpar = ': ' +
363000060704     c                             wrkdestpg + ' ' + stpper + ' % ' +
363100060704     c                             'over  ' + (%editc(oltre:'3'))+
363200060704     c                             ' ' + unmis + blk21
363300060801     c                   when      i45lin = '3'
363400060801     c                   eval      w_tarpar = ': ' +
363500060801     c                             wrkdestpg + ' ' + stpper + ' % ' +
363600060801     c                             'plus  ' + (%editc(oltre:'3'))+
363700060801     c                             ' ' + unmis + blk21
363800060712     c                   when      i45lin = '4'
363900060712     c                   eval      w_tarpar = ': ' +
364000060712     c                             wrkdestpg + ' ' + stpper + ' % ' +
364100060728     c                             'mehrals ' + (%editc(oltre:'3'))+
364200060712     c                             ' ' + unmis + blk21
364300060704     c                   endsl
364400020110     c                   endif
364500020110
364600020110      * se esiste minimo e massimo devo valorizzare anche l'estensione
364700020110      * tariffe particolari con l'oltre
364800020110
364900020110      * solo minimo
365000020110     c                   if        tpdmin >  0 and tpdmax = 0
365100060704      * in lingua
365200060704     c                   select
365300060704     c                   when      i45lin = 'I'
365400021205     c                   eval      w_tarparest = blk56
365500020110     c                             + ' minimo........' +
365600020110     c                             %editc(tpdmin:'3')
365700060704     c                   when      i45lin = '2'
365800060704     c                   eval      w_tarparest = blk56
365900060704     c                             + ' minimum.......' +
366000060704     c                             %editc(tpdmin:'3')
366100060728     c                   when      i45lin = '3'
366200060728     c                   eval      w_tarparest = blk56
366300060728     c                             + ' minimum.......' +
366400060728     c                             %editc(tpdmin:'3')
366500060712     c                   when      i45lin = '4'
366600060712     c                   eval      w_tarparest = blk56
366700060728     c                             + ' minde.........' +
366800060712     c                             %editc(tpdmin:'3')
366900060704     c                   endsl
367000020110     c                   endif
367100020110
367200020110      * solo massimo
367300020110     c                   if        tpdmin =  0 and tpdmax > 0
367400060704      * in lingua
367500060704     c                   select
367600060704     c                   when      i45lin = 'I'
367700021205     c                   eval      w_tarparest = blk56
367800020110     c                             + ' massimo.......' +
367900020110     c                             %editc(tpdmax:'3')
368000060704     c                   when      i45lin = '2'
368100060704     c                   eval      w_tarparest = blk56
368200060704     c                             + ' maximum.......' +
368300060704     c                             %editc(tpdmax:'3')
368400060801     c                   when      i45lin = '3'
368500060728     c                   eval      w_tarparest = blk56
368600060728     c                             + ' maximum.......' +
368700060728     c                             %editc(tpdmax:'3')
368800060712     c                   when      i45lin = '4'
368900060712     c                   eval      w_tarparest = blk56
369000060728     c                             + ' höchste.......' +
369100060712     c                             %editc(tpdmax:'3')
369200060704     c                   endsl
369300020110     c                   endif
369400020110
369500020110      * sia mino che massimo
369600020110     c                   if        tpdmin >  0 and tpdmax > 0
369700060704      * in lingua
369800060704     c                   select
369900060704     c                   when      i45lin = 'I'
370000030318     c                   eval      w_tarparest = blk10 + blk10 + blk12
370100060908     c                             + '...... minimo...' +
370200020110     c                             %editc(tpdmin:'3')
370300020110     c                             + ' massimo..' +
370400020110     c                             %editc(tpdmax:'3')
370500060704     c                   when      i45lin = '2'
370600060704     c                   eval      w_tarparest = blk10 + blk10 + blk12
370700060908     C                             + '...... minimum..' +
370800060704     c                             %editc(tpdmin:'3')
370900060704     C                             + ' maximum..' +
371000060704     c                             %editc(tpdmax:'3')
371100060728     c                   when      i45lin = '3'
371200060728     c                   eval      w_tarparest = blk10 + blk10 + blk12
371300060908     C                             + '...... minimum..' +
371400060728     c                             %editc(tpdmin:'3')
371500060728     C                             + ' maximum..' +
371600060728     c                             %editc(tpdmax:'3')
371700060712     c                   when      i45lin = '4'
371800060712     c                   eval      w_tarparest = blk10 + blk10 + blk12
371900060908     c                             + '...... minde....' +
372000060712     c                             %editc(tpdmin:'3')
372100060728     c                             + ' höchste..' +
372200060712     c                             %editc(tpdmax:'3')
372300060704     c                   endsl
372400020110     c                   endif
372500020110
372600020110     c                   else
372700020110
372800020110     c
372900020110      * valorizzo il campo che conterrà la percetuale
373000020110      * se non c'è ne minimo ne massimo
373100020110     c                   if        tpdmin = 0 and tpdmax =  0
373200020110      * no percentuale
373300020110     c                   if        w_per = ' '
373400020110     c                   eval      w_tarpar = ': ' + wrkdestpg +
373500021205     c                             blk56 + %editc(tpditr:'3')
373600020110     c                   else
373700020110      * si percentuale
373800020110     c                   eval      w_tarpar = ': ' +
373900030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
374000030318     c                             blk10 + blk10 + blk25
374100020110     c                   endif
374200020110
374300020110     c                   endif
374400020110      * se c'è minimo
374500020110     c                   if        tpdmin > 0 and tpdmax = 0
374600020110      * no percentuale
374700020110     c                   if        w_per = ' '
374800060704      * in lingua
374900060704     c                   select
375000060704     c                   when      i45lin = 'I'
375100020110     c                   eval      w_tarpar = ': ' + wrkdestpg +
375200030318     c                             blk10 + blk10 + blk12
375300060313     c                             + ' ' + %editc(tpditr:'3')
375400020110     c                             + ' minimo...' +
375500020110     c                             %editc(tpdmin:'3')
375600060704     c                   when      i45lin = '2'
375700060704     c                   eval      w_tarpar = ': ' + wrkdestpg +
375800060704     c                             blk10 + blk10 + blk12
375900060704     c                             + ' ' + %editc(tpditr:'3')
376000060704     C                             + ' minimum..' +
376100060704     c                             %editc(tpdmin:'3')
376200060728     c                   when      i45lin = '3'
376300060728     c                   eval      w_tarpar = ': ' + wrkdestpg +
376400060728     c                             blk10 + blk10 + blk12
376500060728     c                             + ' ' + %editc(tpditr:'3')
376600060728     C                             + ' minimum..' +
376700060728     c                             %editc(tpdmin:'3')
376800060712     c                   when      i45lin = '4'
376900060712     c                   eval      w_tarpar = ': ' + wrkdestpg +
377000060712     c                             blk10 + blk10 + blk12
377100060712     c                             + ' ' + %editc(tpditr:'3')
377200060728     c                             + ' minde....' +
377300060712     c                             %editc(tpdmin:'3')
377400060704     c                   endsl
377500020110     c                   else
377600020110      * si percentuale
377700060704      * in lingua
377800060704     c                   select
377900060704     c                   when      i45lin = 'I'
378000020110     c                   eval      w_tarpar = ': ' +
378100030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
378200030318     c                             blk10 + blk25
378300020110     c                             + ' minimo...' +
378400020110     c                             %editc(tpdmin:'3')
378500060704     c                   when      i45lin = '2'
378600060704     c                   eval      w_tarpar = ': ' +
378700060704     c                             wrkdestpg + ' ' + stpper + ' % ' +
378800060704     c                             blk10 + blk25
378900060712     c                             + ' minimum..' +
379000060704     c                             %editc(tpdmin:'3')
379100060728     c                   when      i45lin = '3'
379200060728     c                   eval      w_tarpar = ': ' +
379300060728     c                             wrkdestpg + ' ' + stpper + ' % ' +
379400060728     c                             blk10 + blk25
379500060728     c                             + ' minimum..' +
379600060728     c                             %editc(tpdmin:'3')
379700060712     c                   when      i45lin = '4'
379800060712     c                   eval      w_tarpar = ': ' +
379900060712     c                             wrkdestpg + ' ' + stpper + ' % ' +
380000060712     c                             blk10 + blk25
380100060728     c                             + ' minde....' +
380200060712     c                             %editc(tpdmin:'3')
380300060704     c                   endsl
380400020110     c                   endif
380500020110
380600020110     c                   endif
380700020110      * se c'è solo massimo
380800020110     c                   if        tpdmin = 0 and tpdmax > 0
380900020110      * no percentuale
381000020110     c                   if        w_per = ' '
381100060704      * in lingua
381200060704     c                   select
381300060704     c                   when      i45lin = 'I'
381400020110     c                   eval      w_tarpar = ': ' + wrkdestpg +
381500030318     c                             blk10 + blk10 + blk12
381600060313     c                             + ' ' + %editc(tpditr:'3')
381700020110     c                             + ' massimo..' +
381800020110     c                             %editc(tpdmax:'3')
381900060704     c                   when      i45lin = '2'
382000060704     c                   eval      w_tarpar = ': ' + wrkdestpg +
382100060704     c                             blk10 + blk10 + blk12
382200060704     c                             + ' ' + %editc(tpditr:'3')
382300060712     c                             + ' maximum..' +
382400060704     c                             %editc(tpdmax:'3')
382500060728     c                   when      i45lin = '3'
382600060728     c                   eval      w_tarpar = ': ' + wrkdestpg +
382700060728     c                             blk10 + blk10 + blk12
382800060728     c                             + ' ' + %editc(tpditr:'3')
382900060728     c                             + ' maximum..' +
383000060728     c                             %editc(tpdmax:'3')
383100060712     c                   when      i45lin = '4'
383200060712     c                   eval      w_tarpar = ': ' + wrkdestpg +
383300060712     c                             blk10 + blk10 + blk12
383400060712     c                             + ' ' + %editc(tpditr:'3')
383500060728     c                             + ' höchste..' +
383600060712     c                             %editc(tpdmax:'3')
383700060704     c                   endsl
383800020110     c                   else
383900020110      * si percentuale
384000060704      * in lingua
384100060704     c                   select
384200060704     c                   when      i45lin = 'I'
384300020110     c                   eval      w_tarpar = ': ' +
384400030318     c                             wrkdestpg + ' ' + stpper + ' % ' +
384500030318     c                             blk10 + blk25
384600020110     c                             + ' massimo..' +
384700020110     c                             %editc(tpdmax:'3')
384800060704     c                   when      i45lin = '2'
384900060704     c                   eval      w_tarpar = ': ' +
385000060704     c                             wrkdestpg + ' ' + stpper + ' % ' +
385100060704     c                             blk10 + blk25
385200060712     c                             + ' maximum..' +
385300060704     c                             %editc(tpdmax:'3')
385400060728     c                   when      i45lin = '3'
385500060728     c                   eval      w_tarpar = ': ' +
385600060728     c                             wrkdestpg + ' ' + stpper + ' % ' +
385700060728     c                             blk10 + blk25
385800060728     c                             + ' maximum..' +
385900060728     c                             %editc(tpdmax:'3')
386000060712     c                   when      i45lin = '4'
386100060712     c                   eval      w_tarpar = ': ' +
386200060712     c                             wrkdestpg + ' ' + stpper + ' % ' +
386300060712     c                             blk10 + blk25
386400060728     c                             + ' höchste..' +
386500060712     c                             %editc(tpdmax:'3')
386600060704     c                   endsl
386700020110     c                   endif
386800020110
386900020110     c                   endif
387000020110      * se c'è minimo e massimo
387100020110     c                   if        tpdmin > 0 and tpdmax > 0
387200020110      * no percentuale
387300020110     c                   if        w_per = ' '
387400060704      * in lingua
387500060704     c                   select
387600060704     c                   when      i45lin = 'I'
387700020110     c                   eval      w_tarpar = ': ' + wrkdestpg +
387800021205     c                             ' ' + %editc(tpditr:'3') + blk10
387900060908     c                             + ' minimo....' +
388000020110     c                             %editc(tpdmin:'3')
388100020110     c                             + ' massimo..' +
388200020110     c                             %editc(tpdmax:'3')
388300060704     c                   when      i45lin = '2'
388400060704     c                   eval      w_tarpar = ': ' + wrkdestpg +
388500060704     c                             ' ' + %editc(tpditr:'3') + blk10
388600060908     c                             + ' minimum...' +
388700060704     c                             %editc(tpdmin:'3')
388800060704     c                             + ' maximum..' +
388900060704     c                             %editc(tpdmax:'3')
389000060728     c                   when      i45lin = '3'
389100060728     c                   eval      w_tarpar = ': ' + wrkdestpg +
389200060728     c                             ' ' + %editc(tpditr:'3') + blk10
389300060908     c                             + ' minimum...' +
389400060728     c                             %editc(tpdmin:'3')
389500060728     c                             + ' maximum..' +
389600060728     c                             %editc(tpdmax:'3')
389700060712     c                   when      i45lin = '4'
389800060712     c                   eval      w_tarpar = ': ' + wrkdestpg +
389900060712     c                             ' ' + %editc(tpditr:'3') + blk10
390000060908     c                             + ' minde.....' +
390100060712     c                             %editc(tpdmin:'3')
390200060728     c                             + ' höchste..' +
390300060712     c                             %editc(tpdmax:'3')
390400060704     c                   endsl
390500020110     c                   else
390600020110      * si percentuale
390700060704      * in lingua
390800060704     c                   select
390900060704     c                   when      i45lin = 'I'
391000020110     c                   eval      w_tarpar = ': ' +
391100030318     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
391200060908     c                             + ' minimo..' +
391300020110     c                             %editc(tpdmin:'3')
391400020110     c                             + ' massimo..' +
391500020110     c                             %editc(tpdmax:'3')
391600060704     c                   when      i45lin = '2'
391700060704     c                   eval      w_tarpar = ': ' +
391800060704     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
391900060908     c                             + ' minimum..' +
392000060704     c                             %editc(tpdmin:'3')
392100060712     c                             + ' maximum..' +
392200060704     c                             %editc(tpdmax:'3')
392300060728     c                   when      i45lin = '3'
392400060728     c                   eval      w_tarpar = ': ' +
392500060728     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
392600060908     c                             + ' minimum..' +
392700060728     c                             %editc(tpdmin:'3')
392800060728     c                             + ' maximum..' +
392900060728     c                             %editc(tpdmax:'3')
393000060712     c                   when      i45lin = '4'
393100060712     c                   eval      w_tarpar = ': ' +
393200060712     c                             wrkdestpg +  ' ' + stpper + ' % ' + blk12
393300060908     c                             + ' minde....' +
393400060712     c                             %editc(tpdmin:'3')
393500060728     c                             + ' höchste..' +
393600060712     c                             %editc(tpdmax:'3')
393700060704     c                   endsl
393800020110     c                   endif
393900020110
394000020110     c                   endif
394100020110
394200020110     c                   endif
394300020110     c                   leave
394400020110     c                   endif
394500020110
394600020110     c                   enddo
394700020110
394800020110     c                   endsr
394900020110
395000020110      *------------------------------------------------------------------*
395100020110      * Ricerc  CONTROLLO SE ESISTE UN RECORD VALIDO E CON TARIFFA+MINIMO > 0
395200020110      *------------------------------------------------------------------*
395300020110
395400020110     c     Ricerc        begsr
395500020110
395600020110     c                   setoff                                       04
395700020110     c                   z-add     *zeros        recval
395800020110     c                   clear                   wrkftc
395900020110     c                   clear                   wrkorp
396000020110     c                   clear                   wrksgl
396100020110     c                   clear                   recsgl
396200020110     c                   clear                   oltre
396300020110     c                   clear                   mintas
396400020110     c                   clear                   savorp1
396500020110     c                   clear                   w_tarparest
396600020110
396700061011     c  n98ktpt          setll     titpd01l
396800061011     c   98ktpt          setll     tiopd01l
396900020110
397000020110     c                   do        *hival
397100061011     c  n98ktpt          reade     titpd01l
397200061011     c   98ktpt          reade     tiopd01l
397300020110
397400061011     c                   if        %eof
397500020110      * se non ho il recval valorizzato accendo lo 04
397600020110     c                   if        recval = 0
397700020110     c                   seton                                        04
397800020110     c                   endif
397900020110
398000020110     c                   leave
398100020110     c                   endif
398200020110
398300020110     c                   if        tpdatb = 'A'
398400020110     c                   iter
398500020110     c                   endif
398600020110
398700020110      * se tariffa non valorizzata leggo la successiva
398800020110      * importo e minimo =  a zero
398900020110      * se valorizzata esco per gestire la stampa del dettaglio tariffario
399000020110
399100020110     c                   if        tpditr = 0 and tpdmin = 0
399200020110      * mi salvo il valore dello scaglione precedente per stampare in un even-
399300020110      * tuale oltre nella riga della tariffa particolare se diverso da 99999
399400020110     c                   z-add     tpdsgl        w0050
399500020110     c                   if        w0050 <> 99999
399600020110
399700020110     c                   z-add     tpdsgl        oltre
399800020110      * mi salvo il primo scaglione per stampare il minimo tassabile in caso di
399900020110      * tariffa particolare a qle
400000020110     c                   if        mintas = 0
400100020110     c                   z-add     tpdsgl        mintas
400200020110     c                   endif
400300020110
400400020110     c                   endif
400500020110      * mi salvo ordinamento regione e provincia la prima volta
400600020110     c                   if        savorp1 = *blanks
400700020110     c                   eval      savorp1 = tpdorp
400800020110      * memorizzo la chiave che mi serve per riaggancare il primo
400900020110      * record valido
401000020110     c                   eval      wrkorp = tpdorp
401100020110     c                   eval      wrkftc = tpdftc
401200020110     c                   eval      wrksgl = tpdsgl
401300020110
401400020110     c                   endif
401500020110
401600020110     c                   iter
401700020110     c                   else
401800020110     c                   add       1             recval
401900020111      * memorizzo lo scaglioni perchè nel caso di unico scaglione valido
402000020110      * stampo unico scaglione solo se scaglione = a 99999
402100020110     c                   eval      recsgl = tpdsgl
402200020110      * memorizzo la chiave che mi serve per riaggancare il primo
402300020110      * record valido se è = a blanks
402400020110     c                   if        recval = 1 and wrkftc = '  '
402500020110     c                   eval      wrkorp = tpdorp
402600020110     c                   eval      wrkftc = tpdftc
402700020110     c                   eval      wrksgl = tpdsgl
402800020110     c                   endif
402900020111
403000020110      * se trovati + di 1 record non proseguo in lettura
403100020110     c                   if        recval > 1
403200020110      * mi riposiziono sul prima record valido trovato
403300061011     c  n98ktpdwrk       chain     titpd01l
403400061011     c   98ktpdwrk       chain     tiopd01l
403500020110
403600020110     c                   leave
403700020110     c                   else
403800020110     c                   iter
403900020110     c                   endif
404000020110
404100020110     c                   endif
404200020110
404300020110    2c                   enddo
404400020110      * se al primo record valido verifico che non ha lo stesso ordinamento
404500020110      * regione e provincia del record a zero forzo per la stampa in dettaglio
404600020110     c                   if        recval = 1 and tpdorp <> savorp1 and
404700020110     c                             savorp1 <> *blanks
404800020110     c                   eval      recval = 9
404900020110      * mi riposiziono sul prima record valido trovato
405000061011     c  n98ktpdwrk       chain     titpd01l
405100061011     c   98ktpdwrk       chain     tiopd01l
405200020110     c                   clear                   savorp1
405300020110     c                   endif
405400020110      * Se ho trovato un solo record valido verifico lo scaglione trovato
405500020110     c                   if        recval = 1
405600020110     c                   z-add     recsgl        w0050
405700020110     c                   if        w0050 <> 99999 and tpttpg <> '2'
405800020110      * se diverso da 99999 devo gestire il tutto come se avesse + scaglioni
405900020110      * e tipo tariffa non è a spedizione
406000020110     c                   eval      recval = 9
406100020110      * mi riposiziono sul prima record valido trovato
406200061011     c  n98ktpdwrk       chain     titpd01l
406300061011     c   98ktpdwrk       chain     tiopd01l
406400020110     c                   endif
406500020110     c                   endif
406600020110
406700020110     c                   endsr
406800020110
406900020110      *------------------------------------------------------------------*
407000020110      *  Dectpg           DECODIFICA TIPO TARIFFA E SCAGLIONI
407100020110      *------------------------------------------------------------------*
407200020110
407300020110     c     Dectpg        begsr
407400020110
407500020110     c                   setoff                                       09
407600020110
407700020110      * decodifico il tipo tariffa
407800020110     c                   movel(p)  tpttpg        key
407900060704     c     tabkeyl       chain     tabel00f
408000020110      *
408100020110     c                   if        %found(tabel00f)
408200020110     c                   movel     tbluni        dstr
408300020110     c                   else
408400020110     c                   clear                   dstr
408500020110     c                   endif
408600020110
408700020110     c                   movel(p)  §trde1        de1tpg
408800020110     c                   movel(p)  §trde3        de3tpg
408900140724      * minimo tassabile
409000140724     c                   z-add     §trata        ctrata
409100020110      * verifico se scaglioni a valore
409200020110     c                   eval      *in09 = (§trtap = 'S')
409300020110
409400020110      * SCAGLIONI
409500020110
409600020110      * memorizzo gli scaglioni del dettaglio tariffa particolare
409700020110
409800020110     c                   exsr      Memsca
409900020110
410000020110     c                   endsr
410100020110
410200020110      *------------------------------------------------------------------*
410300020110      *  Memsca           CARICO SCAGLIONI TARIFFE PARTICOLARI
410400020110      *------------------------------------------------------------------*
410500020110     c     Memsca        begsr
410600020110
410700020110     c                   setoff                                       089137
410800020110     c                   setoff                                       4041
410900020110
411000020110     c                   movea     *zeros        scp
411100020110     c                   movea     *zeros        ssl
411200020110     c                   clear                   sgld
411300020110     c                   clear                   sg1d
411400020110     c                   clear                   sg2d
411500020110     c                   clear                   sgla
411600020110     c                   clear                   sg1a
411700020110     c                   clear                   sg2a
411800020110
411900020110     c                   eval      rotcts = tpdcts
412000020110     c                   eval      rotpro = %subst(tpdorp:5:1)
412100020110     c                   eval      rotreg = %subst(tpdorp:3:2)
412200020110      * SALVO PER LETTURE FUTURE
412300020110     c                   eval      wrkcts = rotcts
412400020110     c                   eval      wrkpro = rotpro
412500020110     c                   eval      wrkreg = rotreg
412600020110
412700020110      * CERCO IL DETTAGLIO TARIFFARIO
412800061011     c  n98ktpt          setll     titpd01l
412900061011     c   98ktpt          setll     tiopd01l
413000020110
413100020110    1c                   do        *hival
413200061011     c  n98ktpt          reade     titpd01l
413300061011     c   98ktpt          reade     tiopd01l
413400020110
413500061011    2c                   if        %eof
413600020110     c                   seton                                        04
413700020110     c                   leave
413800020110    2c                   endif
413900020110
414000020110    2c                   if        tpdatb = 'A'
414100020110     c                   iter
414200020110    2c                   endif
414300020110
414400020110     c                   exsr      Regdet
414500020110     c   99              goto      fine
414600020110     c                   exsr      Prodet
414700020110     c   99              goto      fine
414800020110
414900020110      * carico il primo scaglione
415000020110     c                   z-add     tpdsgl        scp(1)
415100020110
415200020110      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
415300020110      * a valore
415400020110     c                   if        not *in09
415500020110
415600020110     c                   move      tpdsgl        w0030
415700020110     c                   z-add     tpdsgl        w0050
415800020110      * se scaglione 99999 non controllo i decimali
415900020110    2c                   if        w0050 <> 99999 and w0030 > 0
416000020110     c                   seton                                        91
416100020110    2c                   endif
416200020110      * se con decimali verifico se valorizzato dal secondo in poi
416300020110    2c                   if        *in91
416400020110     c                   z-add     w0030         w0020
416500020110    3c                   if        w0020 > 0
416600020110     c                   seton                                        37
416700020110    3c                   endif
416800020110    2c                   endif
416900020110
417000020110     c                   endif
417100050324
417200050324      * Se scaglione non è 99999
417300050324      * controllo che non sia maggiore di 9999 xchè non ci sta in stampa
417400050324      * in questo caso ERRORE ed esco dal programma
417500050324     c                   z-add     tpdsgl        w0050
417600050324     c                   If        w0050 <> 99999 and tpdsgl > 9999
417700050324     c                   move      msg(17)       errmsg
417800050324     c                   exsr      staerr
417900050324     c                   Goto      FINE
418000050324     c                   EndIf
418100020110
418200020110     c                   leave
418300020110
418400020110    1c                   enddo
418500020110
418600020110     c                   z-add     1             K
418700020110
418800020110      * LETTURA TPD CON KEY KTPT CON LO STESSO CODICE TASSAZIONE
418900020110    2c                   do        *hival
419000020111
419100061011     c  n98ktpt          reade     titpd01l
419200061011     c   98ktpt          reade     tiopd01l
419300020110      * se fine lettura
419400061011     c                   if        %eof
419500020110     c                   leave
419600020110     c                   endif
419700020110      * se codice tassazione diverso
419800020110     c                   if        tpdcts <> rotcts
419900020110     c                   leave
420000020110     c                   endif
420100020110      * se annullato leggo il successivo
420200020110     c                   if        tpdatb <> ' '
420300020110     c                   iter
420400020110     c                   endif
420500020110
420600020110     c                   add       1             k
420700020110      * fino a 18 scaglioni carico
420800020110    3c                   if        k <= 18
420900020111
421000020110     c                   z-add     tpdsgl        scp(k)
421100020110
421200020110      * verifico se gli scaglioni hanno dei decimali solo se non è tariffa
421300020110      * a valore
421400020110     c                   if        not *in09
421500020110
421600020110     c                   move      tpdsgl        w0030
421700020110     c                   z-add     tpdsgl        w0050
421800020110      * se scaglione 99999 non controllo i decimali
421900020110     c                   if        w0050 <> 99999 and w0030 > 0
422000020110     c                   seton                                        91
422100020110     c                   endif
422200020110      * se con decimali verifico se valorizzato dal secondo in poi
422300020110     c                   if        *in91
422400020110     c                   z-add     w0030         w0020
422500020110     c                   if        w0020 > 0
422600020110     c                   seton                                        37
422700020110     c                   endif
422800020110     c                   endif
422900020110
423000020110     c                   endif
423100050324
423200050324      * Se scaglione non è 99999
423300050324      * controllo che non sia maggiore di 9999 xchè non ci sta in stampa
423400050324      * in questo caso ERRORE ed esco dal programma
423500050324     c                   z-add     tpdsgl        w0050
423600050324     c                   If        w0050 <> 99999 and tpdsgl > 9999
423700050324     c                   move      msg(17)       errmsg
423800050324     c                   exsr      staerr
423900050324     c                   Goto      FINE
424000050324     c                   EndIf
424100020110
424200020110   x3c                   else
424300020110
424400020110     c                   move      msg(15)       errmsg
424500020110     c                   exsr      staerr
424600050914     c                   Goto      FINE
424700020110    3c                   endif
424800020110
424900020110    2c                   enddo
425000020110      * memorizzo il numero scaglioni caricati
425100020110     c                   z-add     k             wnumsc
425200020110
425300020110      * preparo la schiera degli scaglioni da stampare
425400020110
425500020110     c                   z-add     1             k
425600020110     c                   clear                   k1
425700020110
425800020110    2c                   dow       scp(k) > 0
425900020110      * preparo lo scaglione DAL addizionato di 1 o 0,1 o 0,01 in base ai decima
426000020110      * li
426100020110     c                   z-add     scp(k)        w0050
426200020110
426300020110      * verifico se scaglione 99999
426400020110    3c                   if        w0050 <> 99999
426500020110
426600020110    4c                   if        k1 > 0
426700020110      * se non ha decimali
426800020110     c                   if        not *in91
426900020110     c                   eval      sgld(k) = scp(k1) + 1
427000020110     c                   endif
427100020110
427200020110     c                   if        *in91 and not *in37
427300020110      * se ha 1 decimale
427400020110     c                   eval      sg1d(k) = scp(k1) + 0,1
427500020110     c                   endif
427600020110
427700020110     c                   if        *in91 and *in37
427800020110      * se ha + di 1 decimale
427900020110     c                   eval      sg2d(k) = scp(k1) + 0,01
428000020110     c                   endif
428100020110
428200020110     c                   else
428300020110    4c                   if        k =  1
428400020110      * se non ha decimali
428500020110     c                   if        not *in91
428600020110     c                   eval      sgld(k) =  1
428700020110     c                   endif
428800020110
428900020110     c                   if        *in91 and not *in37
429000020110      * se ha 1 decimale
429100020110     c                   eval      sg1d(k) =  0,1
429200020110     c                   endif
429300020110
429400020110     c                   if        *in91 and *in37
429500020110      * se ha + di 1 decimale
429600020110     c                   eval      sg2d(k) =  0,01
429700020110     c                   endif
429800020110
429900020110     c                   endif
430000020110
430100020110    4c                   endif
430200020110
430300020110      * costruisco la schiera dell'applicazione tariffa finita che va sotto gli
430400020110      * scaglioni da/a
430500020110      * tariffa e in mancanza di quest'ultima prendo dalla tabella TR
430600020110
430700060925      * richiamo la routine (dove ho tutto fisso e in un unico posto)
430800060925     c                   move      tpttpg        wctr1
430900151119      * Chiodo a programma per la nuova tariffa particolare t= Diritto di
431000151119      * chiamata ritiro telefonico l'unità di misura "a ritiro" altrimenti
431100151119      * prenderebbe a spedizione
431200151119     c                   if        tptftc = 't'
431300151119     c                   eval      wctr1= 'R'
431400151119     c                   endif
431500060925     c                   exsr      sr_atfk
431600140724
431700140724      * faccio i vari ragionamenti in base al valore della tariffa finita
431800140724      *
431900140724      * se scaglioni senza decimali
432000140724     c                   if        not *in91
432100140724
432200140724     c                   z-add     ctrata        wata
432300140724      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
432400140724      * scrivo "A SPED."
432500140724     c                   if        (wata > 0 and sgl(k) <= wata)
432600140724      * richiamo la routine (dove ho tutto fisso e in un unico posto)
432700140724     c                   clear                   wctr1
432800140724     c                   exsr      sr_atfk
432900140724     c                   endif
433000140724      *
433100140724     c                   else
433200140724      * scaglioni con decimali
433300140724     c                   z-add     ctrata        wata1
433400140724     c                   z-add     ctrata        wata2
433500140724      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
433600140724      * scrivo "A SPED."  1 decimale
433700140724     c                   if        (wata1 > 0 and sgl(k)<= wata1 and not *in37)
433800140724      * richiamo la routine (dove ho tutto fisso e in un unico posto)
433900140724     c                   clear                   wctr1
434000140724     c                   exsr      sr_atfk
434100140724     c                   endif
434200140724      * se tariffa finita > 0  e lo scaglione dal è minore della tariffa finita
434300140724      * scrivo "A SPED."  2 decimali
434400140724     c                   if        (wata2 > 0 and sgl(k) <= wata2 and *in37)
434500140724      * richiamo la routine (dove ho tutto fisso e in un unico posto)
434600140724     c                   clear                   wctr1
434700140724     c                   exsr      sr_atfk
434800140724     c                   endif
434900140724      *                                                      *in91 on
435000140724     c                   endif
435100020110
435200020110     c                   move      '/'           sglb(k)
435300020110      * preparo lo scaglione AL
435400020110
435500020110      * no decimali
435600020110    4c                   if        not *in91
435700020110     c                   z-add     scp(k)        sgla(k)
435800020110    4c                   endif
435900020110      * 1 decimale
436000020110    4c                   if        *in91 and not *in37
436100020110     c                   z-add     scp(k)        sg1a(k)
436200020110    4c                   endif
436300020110      * 2 decimali
436400020110    4c                   if        *in91 and *in37
436500020110     c                   z-add     scp(k)        sg2a(k)
436600020110    4c                   endif
436700020110
436800020110   x3c                   else
436900020110      * scaglione 99999 è sempre maggiore della tariffa finita
437000060925      * richiamo la routine (dove ho tutto fisso e in un unico posto)
437100060925     c                   move      tpttpg        wctr1
437200151119      * chiodo a programma per la nuova tariffa particolare t= Diritto di
437300151119      * chiamata ritiro telefonico l'unità di misura "a ritiro" altrimenti
437400151119      * prenderebbe a spedizione
437500151119     c                   if        tptftc = 't'
437600151119     c                   eval      wctr1= 'R'
437700151119     c                   endif
437800060925     c                   exsr      sr_atfk
437900020110
438000020110    3c                   endif
438100020110
438200020110     c                   add       1             k
438300020110     c                   add       1             k1
438400020110
438500020110      * se maggiore di 18 esco
438600020110    3c                   if        k > 18
438700020110     c                   leave
438800020110    3c                   end
438900020110
439000020110    2c                   enddo
439100020110
439200020110      * se c'è solo uno scaglione non scrivo niente sotto
439300021220      * solo se lo scaglione è 99999
439400020110
439500021220     c                   if        k1 <= 1 and scp(k1) >= 99999
439600020110     c                   clear                   atf(1)
439700020110     c                   endif
439800020110
439900020110     c                   clear                   savsgl
440000020110
440100020111     c                   endsr
440200020110
440300020110      *------------------------------------------------------------------*
440400020110      *  Prodet           DETTAGLIO INIZIALE X  P R O V I N C I A
440500020110      *------------------------------------------------------------------*
440600020110
440700020110     c     Prodet        begsr
440800020110
440900020110     c                   Z-ADD     1             K5
441000020110      * cerco il codice tassazione in schiera per TITAD
441100020110     c  N10TADCTS        LOOKUP    CCT(K5)                                35
441200020110      * cerco il codice tassazione in schiera per TITPD
441300020110     c   10TPDCTS        LOOKUP    CCT(K5)                                35
441400020110      *  se non trovato errore
441500020110     c  N35              SETON                                        99
441600020110     c     *IN99         IFEQ      '1'
441700020110     c                   MOVE      MSG(6)        ERRMSG
441800020110     c                   EXSR      STAERR
441900020110
442000020110     c                   ELSE
442100020110
442200020110      * TITAD
442300020110     c  N10              MOVE      TADORP        CODORP
442400020110      * TITPD
442500020110     c   10              MOVE      TPDORP        CODORP
442600020110
442700020110     c                   MOVE      SCT(K5)       CODSIG
442800020110      *  PULIZIA SCHIERE PROVINCIA
442900020110     c                   if        not *in10
443000020110     c                   CLEAR                   TCT
443100020110     c                   CLEAR                   TMN
443200020110     c                   CLEAR                   TMX
443300020110     c                   CLEAR                   TIN
443400020110
443500020110     c                   else
443600020110
443700020110     c                   MOVEA     *ZEROS        TAP
443800020110     c                   MOVEA     *ZEROS        TPN
443900020110     c                   MOVEA     *ZEROS        TPX
444000020110
444100020110     c                   endif
444200020110
444300020110     c                   END
444400020110
444500020110     c                   endsr
444600020110
444700020110      *------------------------------------------------------------------*
444800020110      *  Regdet           DETTAGLIO INIZIALE X  R E G I O N E
444900020110      *------------------------------------------------------------------*
445000020110
445100020110     c     Regdet        begsr
445200020110
445300020110     c                   Z-ADD     1             K5
445400020110      * cerco il codice tassazione in schiera per TITAD
445500020110     c  N10TADCTS        LOOKUP    CCT(K5)                                35
445600020110      * cerco il codice tassazione in schiera per TITPD
445700020110     c   10TPDCTS        LOOKUP    CCT(K5)                                35
445800020110      *  se non trovato errore
445900020110     c  N35              SETON                                        99
446000020110     c     *IN99         IFEQ      '1'
446100020110     c                   MOVE      MSG(6)        ERRMSG
446200020110     c                   EXSR      STAERR
446300020110
446400020110     c                   ELSE
446500020110
446600020110     c                   MOVE      RCT(K5)       CODREG
446700020111
446800020110     c                   Z-ADD     1             K5
446900020110     c     CODREG        LOOKUP    CRE(K5)                                35
447000020110     c  N35              SETON                                        99
447100020110     c     *IN99         IFEQ      '1'
447200020110     c                   MOVE      MSG(7)        ERRMSG
447300020110     c                   EXSR      STAERR
447400020110     c                   ELSE
447500020110
447600020110     c                   MOVEL     DRE(K5)       DESREG
447700020110      *  PULIZIA SCHIERE REGIONE
447800020110     c                   MOVE      *BLANKS       BIG
447900020110     c                   MOVE      *BLANKS       BMN
448000020110     c                   MOVE      *BLANKS       BMX
448100020110     c                   MOVE      *BLANKS       BSI
448200020110     c                   MOVE      *BLANKS       BIN
448300020110     c                   CLEAR                   BRG
448400020110     c                   MOVE      *BLANKS       WIN
448500020110     c                   MOVE      *BLANKS       WMN
448600020110     c                   MOVE      *BLANKS       WMX
448700020110     c                   MOVE      *BLANKS       WTI
448800020110
448900020110     c                   MOVE      *BLANKS       PDE
449000020110     c                   END
449100020110     c                   END
449200020110
449300020110     c                   endsr
449400020110
449500020110      *------------------------------------------------------------------*
449600020110      *  Decsca           DECODIFICO SCAGLIONI TARIFFE PARTICOLARI
449700020110      *------------------------------------------------------------------*
449800020110
449900020110     c     Decsca        begsr
450000020110
450100020110      * imposto i campi di rottura salvati
450200020110     c                   MOVEL     WRKREG        ROTREG
450300020110     c                   MOVE      WRKPRO        ROTPRO
450400020110     c                   MOVE      WRKCTS        ROTCTS
450500020110     c                   MOVE      WRKCTS        TPDCTS
450600020110     c                   MOVE      WRKREG        COM04
450700020110     c                   MOVEL     COM04         TPDORP
450800020110     c                   MOVE      WRKPRO        TPDORP
450900020110      * pulisco le schiere
451000020110     c                   exsr      Regdet
451100020110     c                   exsr      Prodet
451200020110
451300020110     c                   if        *in80 or curline > 56
451400020110     c                   write     ta50pfin
451500020110     c                   write     ta50pPP
451600020110     c                   setoff                                       80
451700020110     c                   endif
451800020110
451900020110      * stampo la dicitura della tariffa
452000020110      * se tariffa che viene stampata in modo standard recupero la decodifica
452100020110      * dall'estensione della tabella 1P la tabella SP
452200020110
452300020110     c                   if        tst(y) = ' '
452400020110     c                   movel(p)  edtc(y)       comstd
452500020110     c                   clear                   comdstd
452600020110     c                   write     ta50pstd
452700020110
452800020110     c                   else
452900020110      * elevazione limite risarcibile
453000020110    3c                   if        savftc = 'R'
453100020110     c                   write     ta50pma
453200020110    3c                   endif
453300020110      * assicurazione per conto
453400020110    3c                   if        savftc = 'C' and not *in21
453500020110     c                   write     ta50pma
453600020110    3c                   endif
453700020110    3c                   if        savftc = 'C' and *in21
453800020110     c                   write     ta50pma
453900020110    3c                   endif
454000020110
454100020110     c                   endif
454200020110      * valorizzo i campi del prtf scaglioni
454300020110     c                   clear                   desum
454400020110     c                   clear                   dissca
454500020110     c                   clear                   prtatf
454600020110     c                   eval      desum = de1tpg
454700020110     c                   eval      ds3um = de3tpg
454800151119
454900151119      /free
455000151119       //?Decodifica personalizzata del tipo tariffa
455100151119         SELECT;
455200151119       //?Italiano
455300151119         WHEN  I45lin = 'I';
455400151119       // Tariffa particolare Diritto Fatturazione
455500151119           IF  TPTftc = '§';
455600151119             desum = 'A FATTURA';
455700151119             ds3um = 'A FATTURA';
455800151119           ENDIF;
455900151119       // Tariffa particolare Gestione Bancali a Rendere
456000151119           IF  TPTftc = '=';
456100151119             desum = 'A BANCALE';
456200151119             ds3um = 'A BANCALE';
456300151119           ENDIF;
456400151119       // Tariffa particolare Diritto di chiamata prenotazione
456500151119       // ritiro telefonico
456600151119           IF  TPTftc = 't';
456700151119             desum = 'A RITIRO';
456800151119             ds3um = 'A RITIRO';
456900151119           ENDIF;
457000151119       //?Inglese
457100151119         WHEN  I45lin = '2';
457200151119       // Tariffa particolare Diritto Fatturazione
457300151119           IF  TPTftc = '§';
457400151119             desum = 'PER INVOICE';
457500151119             ds3um = 'PER INVOICE';
457600151119           ENDIF;
457700151119       // Tariffa particolare Gestione Bancali a Rendere
457800151119           IF  TPTftc = '=';
457900151119             desum = 'PER PALLET';
458000151119             ds3um = 'PER PALLET';
458100151119           ENDIF;
458200151119       // Tariffa particolare Diritto di chiamata prenotazione
458300151119       // ritiro telefonico
458400151119           IF  TPTftc = 't';
458500151119             desum = 'PER PICK-UP';
458600151119             ds3um = 'PER PICK-UP';
458700151119           ENDIF;
458800151119       //?Francese
458900151119         WHEN  I45lin = '3';
459000151119       // Tariffa particolare Diritto Fatturazione
459100151119           IF  TPTftc = '§';
459200151119             desum = 'A FATTURA';
459300151119             ds3um = 'A FATTURA';
459400151119           ENDIF;
459500151119       // Tariffa particolare Gestione Bancali a Rendere
459600151119           IF  TPTftc = '=';
459700151119             desum = 'A LA PALLETE';
459800151119             ds3um = 'A LA PALLETE';
459900151119           ENDIF;
460000151119       // Tariffa particolare Diritto di chiamata prenotazione
460100151119       // ritiro telefonico
460200151119           IF  TPTftc = 't';
460300151119             desum = 'PAR RETRAIT';
460400151119             ds3um = 'PAR RETRAIT';
460500151119           ENDIF;
460600151119       //?Tedesco
460700151119         WHEN  I45lin = '4';
460800151119       // Tariffa particolare Diritto Fatturazione
460900151119           IF  TPTftc = '§';
461000151119             desum = 'A FATTURA';
461100151119             ds3um = 'A FATTURA';
461200151119           ENDIF;
461300151119       // Tariffa particolare Gestione Bancali a Rendere
461400151119           IF  TPTftc = '=';
461500151119             desum = 'N.PALETTE';
461600151119             ds3um = 'N.PALETTE';
461700151119           ENDIF;
461800151119       // Tariffa particolare Diritto di chiamata prenotazione
461900151119       // ritiro telefonico
462000151119           IF  TPTftc = 't';
462100151119             desum = 'N. BUCHUNG';
462200151119             ds3um = 'N. BUCHUNG';
462300151119           ENDIF;
462400151119         ENDSL;
462500151119      /end-free
462600151119
462700020110      * stampo la testata
462800020110     c                   exsr      Sr_Statesdet
462900020111
463000020110     c                   endsr
463100020110
463200020110      *------------------------------------------------------------------*
463300020110      *  Sr_Statesdet     PREPARO E STAMPO TESTATA DETTAGLIO TARIFFE PART.
463400020110      *------------------------------------------------------------------*
463500020110
463600020110     c     Sr_Statesdet  begsr
463700020110
463800020110     c                   setoff                                       79
463900020110
464000020110      * Se scaglioni senza decimali
464100020110
464200020110     c                   if        not *in91
464300020110     c                   eval      dissca = ' '+%editc(ssld(1):'3') + sslb(1) +
464400020110     c                                      %editc(ssla(1):'Z') + '    ' +
464500020110     c                                      %editc(ssld(2):'3') + sslb(2) +
464600020110     c                                      %editc(ssla(2):'Z') + '    ' +
464700020110     c                                      %editc(ssld(3):'3') + sslb(3) +
464800020110     c                                      %editc(ssla(3):'Z') + '    ' +
464900020110     c                                      %editc(ssld(4):'3') + sslb(4) +
465000020110     c                                      %editc(ssla(4):'Z') + '    ' +
465100020110     c                                      %editc(ssld(5):'3') + sslb(5) +
465200020110     c                                      %editc(ssla(5):'Z') + '    ' +
465300020110     c                                      %editc(ssld(6):'3') + sslb(6) +
465400020110     c                                      %editc(ssla(6):'Z')
465500020110     c                   endif
465600020110
465700020110      * Se scaglioni con 1 decimale
465800020110
465900020110     c                   if        *in91 and not *in37
466000020110     c                   eval      dissca = '   '+%editc(ss1d(1):'4') +sslb(1)+
466100020110     c                                      %editc(ss1a(1):'4') + '    ' +
466200020110     c                                      %editc(ss1d(2):'4') + sslb(2) +
466300020110     c                                      %editc(ss1a(2):'4') + '    ' +
466400020110     c                                      %editc(ss1d(3):'4') + sslb(3) +
466500020110     c                                      %editc(ss1a(3):'4') + '    ' +
466600020110     c                                      %editc(ss1d(4):'4') + sslb(4) +
466700020110     c                                      %editc(ss1a(4):'4') + '    ' +
466800020110     c                                      %editc(ss1d(5):'4') + sslb(5) +
466900020110     c                                      %editc(ss1a(5):'4') + '    ' +
467000020110     c                                      %editc(ss1d(6):'4') + sslb(6) +
467100020110     c                                      %editc(ss1a(6):'4')
467200020110     c                   endif
467300020110
467400020110      * Se scaglioni con 2 decimali
467500020110
467600020110     c                   if        *in91  and  *in37
467700020110     c                   eval      dissca = ' '+%editc(ss2d(1):'4') + sslb(1) +
467800020110     c                                      %editc(ss2a(1):'4') + '    ' +
467900020110     c                                      %editc(ss2d(2):'4') + sslb(2) +
468000020110     c                                      %editc(ss2a(2):'4') + '   ' +
468100020110     c                                      %editc(ss2d(3):'4') + sslb(3) +
468200020110     c                                      %editc(ss2a(3):'4') + '   ' +
468300020110     c                                      %editc(ss2d(4):'4') + sslb(4) +
468400020110     c                                      %editc(ss2a(4):'4') + '   ' +
468500020110     c                                      %editc(ss2d(5):'4') + sslb(5) +
468600020110     c                                      %editc(ss2a(5):'4') + '   ' +
468700020110     c                                      %editc(ss2d(6):'4') + sslb(6) +
468800020110     c                                      %editc(ss2a(6):'4')
468900020110     c                   endif
469000020110      * se 40 acceso il 1° scaglione è = 99999 dopo almeno 6 scaglioni
469100020110     c                   if        *in40
469200060704      * in lingua
469300060704     c                   select
469400060704     c                   when      i45lin = 'I'
469500020110     c                   eval      dissca = '    OLTRE '
469600060704     c                   when      i45lin = '2'
469700060704     c                   eval      dissca = '    OVER  '
469800060801     c                   when      i45lin = '3'
469900060801     c                   eval      dissca = '    PLUS  '
470000060712     c                   when      i45lin = '4'
470100060728     c                   eval      dissca = '  MEHR ALS'
470200060704     c                   endsl
470300020110     c                   endif
470400020110      * se 41 acceso il 1° scaglione è = 99999
470500020110     c                   if        *in41
470600020110     c                   clear                   desum
470700020110     c                   eval      dissca = ds3um
470800020110     c                   seton                                        79
470900020110     c                   endif
471000020110      * se 42 acceso il 2° scaglione è = 99999
471100020110     c                   if        *in42
471200020110     c                   clear                   dissca2
471300060704      * in lingua
471400060704     c                   select
471500060704     c                   when      i45lin = 'I'
471600020110     c                   eval      dissca2 = 'OLTRE  '
471700060704     c                   when      i45lin = '2'
471800060704     c                   eval      dissca2 = 'OVER   '
471900060801     c                   when      i45lin = '3'
472000060801     c                   eval      dissca2 = 'PLUS   '
472100060712     c                   when      i45lin = '4'
472200060728     c                   eval      dissca2 = 'MEHR ALS'
472300060704     c                   endsl
472400020110     c                   move      dissca2       dissca
472500020110     c                   endif
472600020110      * se 52 acceso il 2° scaglione è = 00000
472700020110     c                   if        *in52
472800020110     c                   clear                   dissca2
472900020110     c                   move      dissca2       dissca
473000020110     c                   endif
473100020110      * se 43 acceso il 3° scaglione è = 99999
473200020110     c                   if        *in43
473300020110     c                   clear                   dissca3
473400060704      * in lingua
473500060704     c                   select
473600060704     c                   when      i45lin = 'I'
473700020110     c                   eval      dissca3 = 'OLTRE  '
473800060704     c                   when      i45lin = '2'
473900060704     c                   eval      dissca3 = 'OVER   '
474000060801     c                   when      i45lin = '3'
474100060801     c                   eval      dissca3 = 'PLUS   '
474200060712     c                   when      i45lin = '4'
474300060728     c                   eval      dissca3 = 'MEHR ALS'
474400060704     c                   endsl
474500020110     c                   move      dissca3       dissca
474600020110     c                   endif
474700020110      * se 53 acceso il 3° scaglione è = 00000
474800020110     c                   if        *in53
474900020110     c                   clear                   dissca3
475000020110     c                   move      dissca3       dissca
475100020110     c                   endif
475200020110      * se 44 acceso il 4° scaglione è = 99999
475300020110     c                   if        *in44
475400020110     c                   clear                   dissca4
475500060704      * in lingua
475600060704     c                   select
475700060704     c                   when      i45lin = 'I'
475800020110     c                   eval      dissca4 = 'OLTRE  '
475900060704     c                   when      i45lin = '2'
476000060704     c                   eval      dissca4 = 'OVER   '
476100060801     c                   when      i45lin = '3'
476200060801     c                   eval      dissca4 = 'PLUS   '
476300060712     c                   when      i45lin = '4'
476400060728     c                   eval      dissca4 = 'MEHR ALS'
476500060704     c                   endsl
476600020110     c                   move      dissca4       dissca
476700020110     c                   endif
476800020110      * se 54 acceso il 4° scaglione è = 00000
476900020110     c                   if        *in54
477000020110     c                   clear                   dissca4
477100020110     c                   move      dissca4       dissca
477200020110     c                   endif
477300020110      * se 45 acceso il 5° scaglione è = 99999
477400020110     c                   if        *in45
477500020110     c                   clear                   dissca5
477600060704      * in lingua
477700060704     c                   select
477800060704     c                   when      i45lin = 'I'
477900020110     c                   eval      dissca5 = 'OLTRE  '
478000060704     c                   when      i45lin = '2'
478100060704     c                   eval      dissca5 = 'OVER   '
478200060801     c                   when      i45lin = '3'
478300060801     c                   eval      dissca5 = 'PLUS   '
478400060712     c                   when      i45lin = '4'
478500060728     c                   eval      dissca5 = 'MEHR ALS'
478600060704     c                   endsl
478700020110     c                   move      dissca5       dissca
478800020110     c                   endif
478900020110      * se 55 acceso il 5° scaglione è = 00000
479000020110     c                   if        *in55
479100020110     c                   clear                   dissca5
479200020110     c                   move      dissca5       dissca
479300020110     c                   endif
479400020110      * se 46 acceso il 6° scaglione è = 99999
479500020110     c                   if        *in46
479600020110     c                   clear                   dissca6
479700060704      * in lingua
479800060704     c                   select
479900060704     c                   when      i45lin = 'I'
480000020110     c                   eval      dissca6 = 'OLTRE '
480100060704     c                   when      i45lin = '2'
480200060704     c                   eval      dissca6 = 'OVER   '
480300060801     c                   when      i45lin = '3'
480400060801     c                   eval      dissca6 = 'PLUS   '
480500060712     c                   when      i45lin = '4'
480600060728     c                   eval      dissca6 = 'MEHR ALS'
480700060704     c                   endsl
480800020110     c                   move      dissca6       dissca
480900020110     c                   endif
481000020110      * se 56 acceso il 6° scaglione è = 00000
481100020110     c                   if        *in56
481200020110     c                   clear                   dissca6
481300020110     c                   move      dissca6       dissca
481400020110     c                   endif
481500020110
481600020111      * preparo la schiera dell'applicazione tariffa finita
481700020110     c                   Z-ADD     K4            B
481800020110     c                   DO        6             A
481900020110     c                   MOVE      ATF(B)        STF(A)
482000020110     c                   ADD       1             B
482100020110     c                   END
482200020110
482300020110     c                   clear                   prtatf
482400020110
482500020110     c                   eval      PRTATF = '    ' + stf(1) + '      '  +
482600020110     c                                      stf(2) + '      '    +
482700020110     c                                      stf(3) + '      '    +
482800020110     c                                      stf(4) + '      '    +
482900020110     c                                      stf(5) + '      '    +
483000020110     c                                      stf(6)
483100020110
483200020110     c                   if        *in80 or curline > 56
483300020110     c                   write     ta50pfin
483400020110     c                   write     ta50pPP
483500020110     c                   setoff                                       80
483600020110     c                   endif
483700020110      * se non stampo dettaglio tariffe particolari stampo linea di partenza
483800020110     c  n10              write     ta50p02a
483900020110      * SE TARIFFE PARTICOLARI NON STAMPO DA VOSTRO MAGAZZINO
484000020110     c                   IF        *IN10
484100020110     c                   SETON                                        78
484200020110     c                   ENDIF
484300020110
484400020110     c                   write     ta50p02b
484500020110
484600020110     c                   endsr
484700020110
484800020110      *------------------------------------------------------------------*
484900020110      * Legdet            LEGGO DETTAGLIO DI TPD
485000020110      *------------------------------------------------------------------*
485100020110
485200020110     c     Legdet        begsr
485300020110
485400061011     c  n98KTPT          SETLL     TITPD01l
485500061011     c   98ktpt          setll     tiopd01l
485600020110
485700020110    1c                   DO        *HIVAL
485800061011     c  n98KTPT          READE     TITPD01l
485900061011     c   98ktpt          reade     tiopd01l
486000020110
486100061011     c                   IF        %EOF
486200020110     c                   LEAVE
486300020110     c                   ENDIF
486400020110
486500020110     c                   IF        TPDATB = 'A'
486600020110     c                   ITER
486700020110     c                   ENDIF
486800020110
486900020110      * SE SCAGLIONE 9999 VERIFICO SE E' L'ULTIMO
487000020110     c                   Z-ADD     TPDSGL        NUM4
487100020110    2c     NUM4          IFEQ      9999
487200020110     c     TPDSGL        ANDNE     *ALL'9'
487300020110
487400020110     c                   MOVEL     TPDORP        SAVORP
487500020110     c                   MOVEL     TPDSGL        SAVSGL
487600020110
487700020110     c                   SETOFF                                       35
487800020110      * verifico se esiste un'altro scaglione per lo stesso ORP
487900020110    3c                   DO        *HIVAL
488000061011     c  n98KTPD          READE     TITPD01l                               35
488100061011     c   98ktpd          reade     tiopd01l                               35
488200020110      * se annullato leggo il successivo
488300020110     c                   IF        NOT *IN35 AND TPDATB = 'A'
488400020110     c                   ITER
488500020110     c                   ENDIF
488600020110      * se non trovato o non trovato riaggancio il record precedente
488700061011     c  n98KTPD2         CHAIN     TITPD01l
488800061011     c   98ktpd2         chain     tiopd01l
488900020110
489000020110     c                   LEAVE
489100020110
489200020110    3c                   ENDDO
489300020110
489400020110      * NON C'E' ALTRO SCAGLIONE
489500020110    2c                   ENDIF
489600020110
489700020110     c                   Z-ADD     1             K2
489800020110     c                   Z-ADD     TPDSGL        wsgl72
489900020110      * cerco lo scaglione nella schiera
490000020110     c     wsgl72        LOOKUP    SCP(1)                                 35
490100020110      * se non trovato cerco lo scaglione nella schiera stampa
490200020110     c   35wsgl72        LOOKUP    SSL(K2)                                90
490300020110      * se non trovato nella schiera di sola stampa ma trovato nella schiera SCP
490400020110      * torno a leggere
490500020110     c                   if        *in35 and not *in90
490600020110     c                   iter
490700020110     c                   endif
490800020110
490900020110      * se non trovato nella schiera scp ERRORE  scaglione non previsto
491000020110    2c                   IF        not *in35
491100020110     c                   MOVEL     MSG(14)       ERRMSG
491200020110     c                   exsr      Staerr
491300050914     c                   Goto      FINE
491400020110
491500020110   X2c                   else
491600020110     c                   eval      compro = %subst(tpdorp:5:1)
491700020110     c                   eval      comreg = %subst(tpdorp:3:2)
491800020110      * CAMBIATA PROVINCIA
491900020110    3c                   IF        compro <>  rotpro or comreg <> rotreg
492000020110     c                   EXSR      PROTPT
492100020110     c                   EXSR      PRODET
492200020110     c   99              GOTO      FINE
492300020110     c                   MOVE      COMPRO        ROTPRO
492400020110    3c                   END
492500020110      * CAMBIATA REGIONE
492600020110    3c                   if        comreg <> rotreg
492700020110     c                   exsr      Regtpt
492800020110
492900020110     c                   exsr      regdet
493000020110     c   99              goto      fine
493100020110     c                   movel     comreg        rotreg
493200020110    3c                   endif
493300020110
493400020110     c                   Z-ADD     TPDITR        TAP(K2)
493500020110     c                   Z-ADD     TPDMIN        TPN(K2)
493600020110     c                   Z-ADD     TPDMAX        TPX(K2)
493700020110     c                   MOVEL     TPDAIN        SAVAIP
493800020110    2c                   END
493900020110
494000020110    1c                   ENDDO
494100020110
494200020110     c                   exsr      Protpt
494300020111     c                   exsr      Regtpt
494400020110
494500020110     c                   endsr
494600020110
494700020110      *------------------------------------------------------------------*
494800020110      * Protpt            CAMBIO PROVINCIA
494900020110      *------------------------------------------------------------------*
495000020110
495100020110     c     Protpt        begsr
495200020110
495300020110     c                   z-add     1             c
495400020110     c     *blanks       lookup    big(c)                                 35
495500020110     c     *in35         ifeq      '0'
495600020110     c                   move      msg(10)       errmsg
495700020110     c                   exsr      Staerr
495800050914     c                   goto      fine
495900020110     c                   else
496000020110
496100020110     c                   do        6             b
496200020110     c                   move      tap(b)        com(b)
496300020110     c                   end
496400020110
496500020110     c                   movea     com           big(c)
496600020110
496700020110     c                   do        6             b
496800020110     c                   move      tpn(b)        com(b)
496900020110     c                   end
497000020110
497100020110     c                   movea     com           bmn(c)
497200020110
497300020110     c                   do        6             b
497400020110     c                   move      tpx(b)        com(b)
497500020110     c                   end
497600020110
497700020110     c                   movea     com           bmx(c)
497800020110     c
497900020110     c                   move      codsig        bsi(c)
498000020110     c                   move      savaip        aip(c)
498100020110     c                   move      codorp        brg(c)
498200020110     c                   end
498300020110
498400020110     c                   movel     ' '           savaip
498500020110
498600020110     c                   endsr
498700020110
498800020110      *------------------------------------------------------------------*
498900020110      * Regtpt            CAMBIO REGIONE
499000020110      *------------------------------------------------------------------*
499100020110
499200020110     c     Regtpt        begsr
499300020110
499400020110     c                   Z-ADD     0             NUM4
499500020110     c                   Z-ADD     1             K
499600020110      * preparo le schiere che devono essere stampate
499700050616    1c                   DO        15            K
499800020110    2c     BRG(K)        IFNE      '  '
499900020110    3c     BRG(K)        IFEQ      '9'
500000020110
500100020110     c     K             IFEQ      1
500200020110     c                   MOVEL     *all'.'       PDE(K)
500300020110     c                   ELSE
500400060704      * in lingua
500500060704     c                   select
500600060704     c                   when      i45lin = 'I'
500700020110     c                   eval      PDE(K) = 'ALTRI........................'
500800060704     c                   when      i45lin = '2'
500900060704     c                   eval      pde(k) = 'OTHERS.......................'
501000060801     c                   when      i45lin = '3'
501100060801     c                   eval      pde(k) = 'AUTRES.......................'
501200060712     c                   when      i45lin = '4'
501300060728     c                   eval      PDE(K) = 'ANDERE.......................'
501400060704     c                   endsl
501500020110     c                   END
501600020110     c                   ADD       1             NUM4
501700020110
501800020110    3c                   ELSE
501900020110
502000020110     c                   MOVEA     *BLANKS       WCT
502100020110     c                   Z-ADD     1             C
502200020110     c                   MOVE      BSI(K)        WCT(C)
502300020110
502400020110      * città
502500020110     c                   ADD       1             P
502600020110     c                   MOVEA     BIG(K)        COMO42
502700020110     c                   MOVEA     COMO42        WIN(P)
502800020110      * minimo
502900020110     c                   MOVEA     BMN(K)        CMNO42
503000020110     c                   MOVEA     CMNO42        WMN(P)
503100020110      * massimo
503200020110     c                   MOVEA     BMX(K)        CMXO42
503300020110     c                   MOVEA     CMXO42        WMX(P)
503400020110     c                   MOVEL     AIP(K)        SAVAIP            1
503500020110
503600020110      * MEMORIZZO LE ALTRE PROVINCIE CHE HANNO LE STESSE TARIFFE
503700020110     c     K             ADD       1             H
503800050616    4c     H             DO        15            H
503900020110
504000020110     c     BRG(H)        IFNE      '9'
504100020110     c     como42        ANDEQ     big(H)
504200020110     c     cmno42        ANDEQ     bmn(H)
504300020110     c     cmxo42        ANDEQ     bmX(H)
504400020110     c     SAVAIP        ANDEQ     AIP(H)
504500020110
504600020110      * se isola controllo anche che non siano le province sondrio e como
504700020110     c                   IF        (*in06  and bsi(h)<>'CO' and bsi(h)<>'SO')
504800020110     c                             or not *in06
504900020110     c                   ADD       1             C
505000020110     c                   MOVEL     '/'           WCT(C)
505100020110     c                   MOVE      BSI(H)        WCT(C)
505200020110     c                   MOVEL     *BLANKS       big(H)
505300020110     c                   MOVEL     *BLANKS       bmn(H)
505400020110     c                   MOVEL     *BLANKS       bmx(H)
505500020110     c                   MOVEL     *BLANKS       BRG(H)
505600020110     c                   endif
505700020110
505800020110     c                   ENDIF
505900020110
506000020110   e4c                   ENDDO
506100020110
506200020110      * MEMORIZZO LA SCHIERA DELLE PROVINCIE CON STESSE TARIFFE
506300020110     c                   ADD       1             C
506400020110     c                   MOVEA     *all'.'       WCT(C)
506500020110     c                   movea     wct           como29
506600020110     c                   MOVEA     como29        PDE(K)
506700020110      * se sto stampando le isole recupero la decodifica delle isole
506800020110     c                   if        %subst(como29:2:2) = 'CO' and *in06
506900020110     c                   eval      PDE(k) = 'CAMPIONE D''ITALIA .........'
507000020110     c                   ENDIF
507100020110
507200020110     c                   if        %subst(como29:2:2) = 'SO' and *in06
507300020110     c                   eval      PDE(k) = 'LIVIGNO ....................'
507400020110     c                   ENDIF
507500020110
507600020110   e3c                   END
507700020110   e2c                   END
507800020110   e1c                   END
507900020110
508000020110      * STAMPA REGIONE
508100050616    1c                   DO        15            K
508200020110
508300020110    2c     BRG(K)        IFNE      ' '
508400020110
508500020110     c                   movea     BIG(k)        com
508600020110     c                   z-add     1             b
508700020110     c                   do        6             b
508800020110     c                   move      com(b)        tap(b)
508900020110      * se tariffa a valore di fianco al valore metto la percentuale
509000020110     c                   if        *in09 and tap(b) > 0
509100020110     c                   MOVEL     '%'           PER(B)
509200020110     c                   END
509300020110
509400020110     c                   END
509500020110      * applicazione inoltro (forse non si usa)
509600020110     c     AIP(K)        IFEQ      'S'
509700020110     c                   MOVEL     'SI'          STAAIP            2
509800020110     c                   ELSE
509900020110     c                   MOVEL     'NO'          STAAIP
510000020110     c                   END
510100020111
510200020110      * VERIFICO SE C'E' IL MINIMO E IL MASSIMO
510300020110     c                   SETOFF                                       7586
510400020110
510500020110    2c                   IF        BMN(K) <> *BLANKS AND BMN(K) <> *ZEROS
510600020110     c                   SETON                                        75
510700020110
510800020110     c                   MOVEA     BMN(K)        COM
510900020110
511000020110     c                   DO        6             B
511100020110     c                   MOVE      COM(B)        TPN(B)
511200020110     c                   ENDDO
511300020110
511400020110    2c                   ENDIF
511500020110
511600020110    2c                   IF        BMX(K) <> *BLANKS AND BMX(K) <> *ZEROS
511700020110     c                   SETON                                        86
511800020110
511900020110     c                   MOVEA     BMX(K)        COM
512000020110
512100020110     c                   DO        6             B
512200020110     c                   MOVE      COM(B)        TPX(B)
512300020110     c                   ENDDO
512400020110
512500020110    2c                   ENDIF
512600020110
512700020110      * gestione stampa tariffa particolare
512800020110     c                   EXSR      SR_STPPAR
512900020110
513000020110    2c                   END
513100020110
513200020110    1c                   ENDDO
513300020110
513400020110     c                   MOVEA     *blanks       big
513500020110     c                   MOVEA     *blanks       bmn
513600020110     c                   MOVEA     *blanks       bmx
513700020110     c                   MOVEA     *blanks       win
513800020110     c                   MOVEA     *blanks       wmn
513900020110     c                   MOVEA     *blanks       wmx
514000020110     c                   MOVEA     *BLANKS       BSI
514100020110     c                   MOVEA     *BLANKS       AIP
514200020110     c                   MOVEA     *BLANKS       PER
514300020110     c                   MOVEA     *ZEROS        TAP
514400020110     c                   MOVEA     *ZEROS        TPN
514500020110     c                   MOVEA     *ZEROS        TPX
514600020110     c                   CLEAR                   BRG
514700020110
514800020110     c                   endsr
514900020110
515000020110      *------------------------------------------------------------------*
515100020110      * Sr_Stppar         STAMPA TARIFFE PARTICOLARI
515200020110      *------------------------------------------------------------------*
515300020110
515400020110     c     Sr_Stppar     begsr
515500020110
515600020110      * valorizzo il dettaglio tariffario in stampa
515700020110     c                   clear                   ta50p03b
515800020110
515900020110     c                   eval      p03des = desreg + ' ' + pde(k)
516000020110     c                   clear                   desreg
516100020110      * verifico se sono tutti valorizzati
516200020110     c     tap(1)        comp      0                                  61
516300020110     c     tap(2)        comp      0                                  62
516400020110     c     tap(3)        comp      0                                  63
516500020110     c     tap(4)        comp      0                                  64
516600020110     c     tap(5)        comp      0                                  65
516700020110     c     tap(6)        comp      0                                  66
516800020110
516900020110     c                   z-add     tap(1)        tar01p
517000020110     c                   z-add     tap(2)        tar02p
517100020110     c                   z-add     tap(3)        tar03p
517200020110     c                   z-add     tap(4)        tar04p
517300020110     c                   z-add     tap(5)        tar05p
517400020110     c                   z-add     tap(6)        tar06p
517500020110      * SE TARIFFA A VALORE STAMPO LA %
517600020110    2c                   IF        *IN09
517700020110     c                   MOVE      per(1)        PER01
517800020110     c                   MOVE      per(2)        PER02
517900020110     c                   MOVE      per(3)        PER03
518000020110     c                   MOVE      per(4)        PER04
518100020110     c                   MOVE      per(5)        PER05
518200020110     c                   MOVE      per(6)        PER06
518300020110    2c                   ENDIF
518400020110
518500020110     c                   if        *in80 or curline > 61
518600020110     c                   write     ta50pfin
518700020110     c                   write     ta50pPP
518800020110     c                   write     ta50p02b
518900020110     c                   setoff                                       80
519000020110     c                   endif
519100020110
519200020110     c                   write     ta50p03b
519300020110
519400020110      * VERIFICO SE CI SONO DEI VALORI A ZERO
519500020110
519600020110      * VALORIZZO I CAMPI DI STAMPA
519700020110     c                   clear                   ta50p03b
519800020110      * MINIMO
519900020110    2c                   IF        *IN75
520000020110     c     tpn(1)        COMP      0                                  61
520100020110     c     tpn(2)        COMP      0                                  62
520200020110     c     tpn(3)        COMP      0                                  63
520300020110     c     tpn(4)        COMP      0                                  64
520400020110     c     tpn(5)        COMP      0                                  65
520500020110     c     tpn(6)        COMP      0                                  66
520600020110
520700020110     c                   Z-ADD     tpn(1)        TAR01p
520800020110     c                   Z-ADD     tpn(2)        TAR02p
520900020110     c                   Z-ADD     tpn(3)        TAR03p
521000020110     c                   Z-ADD     tpn(4)        TAR04p
521100020110     c                   Z-ADD     tpn(5)        TAR05p
521200020110     c                   Z-ADD     tpn(6)        TAR06p
521300020110
521400020110      * VALORIZZO LA PARTE DESCRITTIVA
521500060704      * in lingua
521600060704     c                   select
521700060704     c                   when      i45lin = 'I'
521800020110     c                   EVAL      COMO29 = '           Tariffa minima ...'
521900060704     c                   when      i45lin = '2'
522000060704     c                   eval      como29 = '           Minimum price ....'
522100060728     c                   when      i45lin = '3'
522200060728     c                   eval      como29 = '           Tarif minimum ....'
522300060712     c                   when      i45lin = '4'
522400060728     c                   EVAL      COMO29 = '           Mindestpreis .....'
522500060704     c                   endsl
522600020110     c                   MOVE(p)   COMO29        P03DES
522700020110
522800020110     c                   if        *in80 or curline > 61
522900020110     c                   write     ta50pfin
523000020110     c                   write     ta50pPP
523100020110     c                   write     ta50p02b
523200020110     c                   setoff                                       80
523300020110     c                   endif
523400020110
523500020110     c                   write     ta50p03b
523600020110
523700020110    2c                   ENDIF
523800020110
523900020110      * VERIFICO SE CI SONO DEI VALORI A ZERO
524000020110
524100020110      * VALORIZZO I CAMPI DI STAMPA
524200020110     c                   clear                   ta50p03b
524300020110      * MASSIMO
524400020110    2c                   IF        *IN86
524500020110     c     tpx(1)        COMP      0                                  61
524600020110     c     tpx(2)        COMP      0                                  62
524700020110     c     tpx(3)        COMP      0                                  63
524800020110     c     tpx(4)        COMP      0                                  64
524900020110     c     tpx(5)        COMP      0                                  65
525000020110     c     tpx(6)        COMP      0                                  66
525100020110
525200020110     c                   Z-ADD     tpx(1)        TAR01p
525300020110     c                   Z-ADD     tpx(2)        TAR02p
525400020110     c                   Z-ADD     tpx(3)        TAR03p
525500020110     c                   Z-ADD     tpx(4)        TAR04p
525600020110     c                   Z-ADD     tpx(5)        TAR05p
525700020110     c                   Z-ADD     tpx(6)        TAR06p
525800020110
525900020110      * VALORIZZO LA PARTE DESCRITTIVA
526000060704      * in lingua
526100060704     c                   select
526200060704     c                   when      i45lin = 'I'
526300020110     c                   EVAL      COMO29 = '           Tariffa massima ..'
526400060704     c                   when      i45lin = '2'
526500060704     c                   eval      como29 = '           Maximum price ....'
526600060728     c                   when      i45lin = '3'
526700060728     c                   eval      como29 = '           Tarif maximum ....'
526800060712     c                   when      i45lin = '4'
526900060728     c                   EVAL      COMO29 = '           Höchstpreis ......'
527000060704     c                   endsl
527100020110     c                   MOVE(p)   COMO29        P03DES
527200020110
527300020110     c                   if        *in80 or curline > 61
527400020110     c                   write     ta50pfin
527500020110     c                   write     ta50pPP
527600020110     c                   write     ta50p02b
527700020110     c                   setoff                                       80
527800020110     c                   endif
527900020110
528000020110     c                   write     ta50p03b
528100020110
528200020110    2c                   ENDIF
528300020110
528400020110     c                   endsr
528500020110
528600020110      *------------------------------------------------------------------*
528700020110      * Findet            STAMPA FINALE SOTTO OGNI SINGOLA TARIFFA PARTICOLARE
528800020110      *------------------------------------------------------------------*
528900020110
529000020110     c     Findet        begsr
529100020110
529200020110      * stampo eventuale estensione scaglione unico
529300020110     c                   exsr      Sr_Estes
529400020110
529500020110     c                   clear                   ta50ptp
529600020110      * verifico se tariffa a q.le per stampare il minimo tassabile
529700020110     c                   eval      *in08 = (tpttpg = '0')
529800020110      * per 08 controllo il minimo tassabile
529900020110      * nel caso in cui si stampa + di uno scaglione verifico il 1° scaglione
530000020110     c                   if        *in08 and recval > 1
530100020110     c                   if        scp(1) < §trata
530200020110     c                   z-add     scp(1)        commts
530300020110     c                   else
530400020110     c                   z-add     §trata        commts
530500020110     c                   endif
530600020110     c                   endif
530700020110      * nel caso in cui si stampa solo uno scaglione verifico il 1° scaglione
530800020110      * salvato
530900020110     c                   if        *in08 and recval = 1
531000020110     c                   if        mintas < §trata and mintas > 0
531100020110     c                   z-add     mintas        commts
531200020110     c                   else
531300020110     c                   z-add     §trata        commts
531400020110     c                   endif
531500020110     c                   endif
531600020111
531700020110      * verifico rapporto peso / volume
531800040325      * per FEDEX fisso 1,67    (non più)
531900020110     c                   if        tptrpv > 0
532000040325     c     tptrpv        mult      100           comrpv
532100040325     c***  1,67          mult      100           comrpv
532200020110     c                   seton                                        19
532300020110     c                   endif
532400020111
532500020110      * verifico gli importi degli arrotondamenti
532600020110    1c                   if        tptarl > 0
532700020110      * verifico se esistono dei valori con i decimali
532800020110     c                   move      tptarf        w0030
532900020110     c                   eval      *in89 =(w0030 > 0)
533000020110
533100020110    2c                   if        not *in89
533200020110     c                   move      tptarl        w0030
533300020110     c                   eval      *in89 =(w0030 > 0)
533400020110    2c                   endif
533500020110
533600020110    2c                   if        not *in89
533700020110     c                   move      tptaro        w0030
533800020110     c                   eval      *in89 =(w0030 > 0)
533900020110    2c                   endif
534000020110
534100020111      * se 89 spento non esistono importi con decimali sposto in campo di comodo
534200020110    2c                   if        not *in89
534300020110     c                   Z-ADD     TptARO        WARO2
534400020110     c                   Z-ADD     TptARF        WARF2
534500020110     c                   Z-ADD     TptARL        WARL2
534600020110    2c                   endif
534700020110
534800020110      * se arrotondamenti di 1 in 1 non stampo
534900020110      * se arrotondamento fino a diverso da arrotondamento oltre a
535000020110    2c                   if        tptarf <> tptaro
535100020110
535200020110      * valorizzo i campi di stampa
535300060704      * in lingua
535400060704     c                   select
535500060704     c                   when      i45lin = 'I'
535600020110     c  N89              eval      arrot1 = 'fino a ' + %editc(Warl2:'4') +
535700020110     c                                      ' di ' + %editc(warf2:'4')     +
535800020110     c                                      ' in ' + %editc(warf2:'4')
535900020110     c   89              eval      arrot1 = 'fino a ' + %editc(tptarl:'4') +
536000020110     c                                      ' di ' + %editc(tptarf:'4')     +
536100020110     c                                      ' in ' + %editc(tptarf:'4')
536200060704     c                   when      i45lin = '2'
536300060704     c  n89              eval      arrot1 = 'Up to ' +
536400060704     c                                      %editc(warl2:'4') + ' rounded ' +
536500060704     c                                      'to the nearest ' +
536600060704     c                                      %editc(warf2:'4')
536700060704     c   89              eval      arrot1 = 'Up to ' +
536800060704     c                                      %editc(tptarl:'4') + ' rounded ' +
536900060704     c                                      'to the nearest ' +
537000060704     c                                      %editc(tptarl:'4')
537100060801     c                   when      i45lin = '3'
537200060818     c  N89              eval      arrot1 = 'jusq''à ' + %editc(Warl2:'4') +
537300060801     c                                      ' de ' + %editc(warf2:'4')     +
537400060801     c                                      ' en ' + %editc(warf2:'4')
537500060818     c   89              eval      arrot1 = 'jusq''à ' + %editc(tptarl:'4') +
537600060801     c                                      ' de ' + %editc(tptarf:'4')     +
537700060801     c                                      ' en ' + %editc(tptarf:'4')
537800060712     c                   when      i45lin = '4'
537900060818     c  N89              eval      arrot1 = 'bis  ' + %editc(Warl2:'4') +
538000060710     c                                      ' von ' + %editc(warf2:'4')     +
538100060710     c                                      ' auf ' + %editc(warf2:'4')
538200060818     c   89              eval      arrot1 = 'bis  ' + %editc(tptarl:'4') +
538300060710     c                                      ' von ' + %editc(tptarf:'4')     +
538400060710     c                                      ' auf ' + %editc(tptarf:'4')
538500060704     c                   endsl
538600020110
538700020110     c                   seton                                        18
538800020110
538900020110      * valorizzo i campi di stampa
539000060704      * in lingua
539100060704     c                   select
539200060704     c                   when      i45lin = 'I'
539300020110     c  N89              eval      arrot2 = 'oltre  ' + '    '        +
539400020110     c                                      ' di ' + %editc(waro2:'4')     +
539500020110     c                                      ' in ' + %editc(waro2:'4')
539600020110     c   89              eval      arrot2 = 'oltre  ' + '        '         +
539700020110     c                                      ' di ' + %editc(tptaro:'4')     +
539800020110     c                                      ' in ' + %editc(tptaro:'4')
539900060704     c                   when      i45lin = '2'
540000060704     c  n89              eval      arrot2 = 'over ' +  '         '  +
540100060704     c                                      ' to nearest ' +
540200060704     c                                      %editc(waro2:'4')
540300060704     c   89              eval      arrot2 = 'over ' +  '         '  +
540400060704     c                                      ' to nearest ' +
540500060704     c                                      %editc(tptaro:'4')
540600060801     c                   when      i45lin = '3'
540700060801     c  N89              eval      arrot2 = 'au delà' + '    '        +
540800060801     c                                      ' de ' + %editc(waro2:'4')     +
540900060801     c                                      ' en ' + %editc(waro2:'4')
541000060801     c   89              eval      arrot2 = 'au delà' + '        '         +
541100060801     c                                      ' de ' + %editc(tptaro:'4')     +
541200060801     c                                      ' en ' + %editc(tptaro:'4')
541300060712     c                   when      i45lin = '4'
541400060818     c  N89              eval      arrot2 = 'mehr als ' + '    '        +
541500060710     c                                      ' von ' + %editc(waro2:'4')     +
541600060710     c                                      ' auf ' + %editc(waro2:'4')
541700060818     c   89              eval      arrot2 = 'mehr als ' + '   '         +
541800060710     c                                      ' von ' + %editc(tptaro:'4')     +
541900060710     c                                      ' auf ' + %editc(tptaro:'4')
542000060704     c                   endsl
542100020110
542200020110     c                   ELSE
542300020110
542400020110      * valorizzo i campi di stampa
542500060704      * in lingua
542600060704     c                   select
542700060704     c                   when      i45lin = 'I'
542800020110     c  N89              eval      arrot1 = 'di ' + %editc(Warf2:'4') +
542900020110     c                                      ' in ' + %editc(waro2:'4')
543000020110     c   89              eval      arrot1 = 'di     ' + %editc(tptarf:'4') +
543100020110     c                                      ' in' + %editc(tptaro:'4')
543200060704     c                   when      i45lin = '2'
543300060704     c  n89              eval      arrot1 = 'Rounded to nearest ' +
543400060704     c                                      %editc(warf2:'4')
543500060704     c   89              eval      arrot1 = 'Rounded to nearest ' +
543600060704     c                                      %editc(tptarf:'4')
543700060801     c                   when      i45lin = '3'
543800060801     c  N89              eval      arrot1 = 'de ' + %editc(Warf2:'4') +
543900060801     c                                      ' en ' + %editc(waro2:'4')
544000060801     c   89              eval      arrot1 = 'di     ' + %editc(tptarf:'4') +
544100060801     c                                      ' in' + %editc(tptaro:'4')
544200060712     c                   when      i45lin = '4'
544300060710     c  N89              eval      arrot1 = 'von ' + %editc(Warf2:'4') +
544400060710     c                                      ' auf ' + %editc(waro2:'4')
544500060710     c   89              eval      arrot1 = 'von    ' + %editc(tptarf:'4') +
544600060710     c                                      ' auf' + %editc(tptaro:'4')
544700060704     c                   endsl
544800020110
544900020110     c                   seton                                        18
545000020110
545100020110    2c                   endif
545200020110
545300020110    1c                   endif
545400020110
545500020110      * verifico se arrot2 è uguale a blanks accendo il 20
545600020110     c                   eval      *in20 = (arrot2 <> *blanks)
545700020110
545800020110      * verifico se devo stampare questa riga
545900020110      * in assenza di minimo tassabile o arrotondamenti non stampo
546000020110      * *in08 minimo tassabile *in18 arrotondamenti *in19 volume automatico
546100020110     c                   if        *in08 or *in18 or *in19
546200020111     c                   exsr      Sr_Over
546300020111
546400020110     c                   write     ta50ptp
546500020111
546600020110     c                   endif
546700020111
546800020110     c                   endsr
546900020110
547000020110      *------------------------------------------------------------------*
547100020110      * Sr_Estes          GESTIONE STAMPA ESTENSIONE SCAGLIONE UNICO
547200020110      *------------------------------------------------------------------*
547300020110     c     Sr_Estes      begsr
547400020110
547500020110      * se il campo del TARPAREST è diverso da blanks vuol dire che devo
547600020110      * stampare l'estensione della tariffa particolare
547700020110     c                   if        w_tarparest <> *blanks
547800020110     c                   eval      comest = w_tarparest
547900020110     c                   exsr      Sr_Over
548000020110
548100020110     c                   write     ta50pest
548200020110
548300020110     c                   clear                   w_tarparest
548400020110
548500020110     c                   endif
548600020110
548700020110     c                   endsr
548800050527
548900050527      *---------------------------------------------------------------*
549000050527      * SR_COMUNICA controllo e stampa dei servizi on line + e-mail
549100050527      *---------------------------------------------------------------*
549200050527     c     SR_COMUNICA   BegSr
549300050527
549400050527      * Controllo se devo stampare i SERVIZI ON LINE
549500050527     c                   ExSr      Sr_Psw
549600050527      * Controllo se devo stampare la richiesta di almeno 1 mail
549700050527     c                   ExSr      Sr_Ema
549800050527
549900050527      * Stampo la testata se devo stampare la richiesta delle e-mail
550000050527     c                   If        wpsw = *on or wema85 = *on or
550100051206     c                             wema87 = *on or wema88 = *on or
550200090618     c                             wema84 = *On or wema89 = *on
550300050527     c                   ExSr      Sr_Over
550400050527     c                   Write     Ta50pema
550500050527      * E-mail per servizi on line
550600050527     c                   If        wpsw = *on
550700050527     c                   Z-Add     curline       totrig
550800050527     c                   Add       4             totrig
550900050527     c                   ExSr      Sr_Overpiu
551000050527     c                   Write     Ta50pema1
551100050527     c                   EndIf
551200050527      * Stampo la riga per la richiesta delle e-mail
551300050527     c                   If        wema85 = *on or wema87 = *on or
551400090618     c                             wema88 = *on or wema84 = *On or
551500090618     c                             wema89 = *on
551600050527     c                   ExSr      Sr_Over
551700050527     c                   Write     Ta50pema2
551800050527     c                   EndIf
551900050527      * E-mail per bonifico bancario
552000050527     c                   If        wema85 = *on
552100050527     c                   ExSr      Sr_Over
552200050527     c                   Write     Ta50pema3
552300050527     c                   EndIf
552400050527      * E-mail per giacenze
552500050527     c                   If        wema88 = *on
552600050527     c                   ExSr      Sr_Over
552700050527     c                   Write     Ta50pema4
552800050527     c                   EndIf
552900050527      * E-mail per danni
553000050527     c                   If        wema87 = *on
553100050527     c                   ExSr      Sr_Over
553200050527     c                   Write     Ta50pema5
553300050527     c                   EndIf
553400051206      * E-mail per invio fattura
553500051206     c                   If        wema84 = *on
553600051206     c                   ExSr      Sr_Over
553700051206     c                   Write     Ta50pema7
553800051206     c                   EndIf
553900090618      * E-mail per sollecito pagamenti
554000090618     c                   If        wema89 = *on
554100090618     c                   ExSr      Sr_Over
554200090618     c                   Write     Ta50pema8
554300090618     c                   EndIf
554400050527      * consenso dei dati
554500050527     c                   Z-Add     curline       totrig
554600050527     c                   Add       4             totrig
554700050527     c                   ExSr      Sr_Overpiu
554800050527     c                   Write     Ta50pema6
554900050527
555000050527     c                   EndIf
555100050527
555200050527     c                   EndSr
555300041230
555400041230      *---------------------------------------------------------------*
555500041230      * SR_PSW stampa servizi ON LINE se offerta
555600041230      *---------------------------------------------------------------*
555700041230     c     SR_PSW        BegSr
555800041230
555900041230     c                   Eval      wpsw = *Off
556000041230
556100041230      * Se cliente nuovo
556200041230      * --> stampo sempre
556300041230if  1c                   If        VisCnw = 'S'
556400041230     c                   Eval      wpsw = *On
556500041230      * Se cliente non nuovo
556600041230      * --> stampo se non abilitato o se non è figlio di papà abilitato
556700041230   x1c                   Else
556800041230     c                   Move      *all'0'       vssksu
556900041230     c                   Move      visksc        vssksu
557000041230     c                   Eval      vssisv = 'TT'
557100041230     c     kTivss        Setll     Tivss02l
557200041230if  2c                   If        Not %Equal(Tivss02l)
557300041230     c                   Clear                   Tibs10ds
557400041230     c                   Eval      d10tle = 'WW'
557500041230     c                   Eval      d10paf = 'P'
557600041230     c                   Eval      d10cod = visksc
557700041230     c                   Eval      d10drf = *date
557800041230     c                   Call      'TIBS10R'
557900041230     c                   Parm                    Tibs10ds
558000041230if  3c                   If        d10err = *Blanks and d10cop > *Zeros
558100041230     c                   Move      *all'0'       vssksu
558200041230     c                   Move      d10cop        vssksu
558300041230     c                   Eval      vssisv = 'TT'
558400041230     c     kTivss        Setll     Tivss02l
558500041230     c                   If        Not %Equal(Tivss02l)
558600041230     c                   Eval      wpsw = *On
558700041230     c                   EndIf
558800041230e   3c                   EndIf
558900050112if  3c                   If        d10err <> *Blanks
559000050112     c                   Eval      wpsw = *On
559100050112e   3c                   EndIf
559200041230e   2c                   EndIf
559300041230e   1c                   EndIf
559400041230
559500041230     c                   EndSr
559600050527
559700050527      *---------------------------------------------------------------*
559800050527      * SR_EMA stampa delle e-mail da richiedere se offerta
559900050527      *---------------------------------------------------------------*
560000050527     c     Sr_Ema        BegSr
560100050527
560200050527     c                   Eval      wema85 = *Off
560300050527     c                   Eval      wema88 = *Off
560400050527     c                   Eval      wema87 = *Off
560500051206     c                   Eval      wema84 = *Off
560600090618     c                   Eval      wema89 = *Off
560700050527
560800091113      * visita
560900050527     c                   eval      notapl = 'V'
561000050527     c                   movel     i45kcc        notke1
561100050527     c                   move      parsks        notke1
561200091113      * trattativa
561300091113     c                   if        $trattativa
561400091113     c                   eval      notapl = 'T'
561500091113     c                   endif
561600050527     c                   Clear                   notke2
561700050527
561800050527      * e-mail bonifico bancario nota 85
561900050527     c                   If        §4uabi = 'S'
562000050527      * nota della visita
562100050527     c                   movel     '85'          nottnt
562200050527     c     keyntc        Chain     tfntc01l
562300050527     c                   If        %Found(tfntc01l)
562400050527     c                   Goto      nota88
562500050527     c                   EndIf
562600050527      * nota del cliente se non è un cliente nuovo
562700050527     c                   If        visksc > *Zeros
562800050527     c                   eval      notapl = 'C'
562900050527     c                   move      visksc        notke1
563000050527     c     keyntc        Chain     tfntc01l
563100050527     c                   If        %Found(tfntc01l)
563200050527     c                   Goto      nota88
563300050527     c                   EndIf
563400050527     c                   EndIf
563500050527      * nota non trovata
563600050527     c                   Eval      wema85 = *On
563700050527     c                   EndIf
563800050527
563900050527     c     nota88        Tag
564000091113      * visita
564100050527     c                   eval      notapl = 'V'
564200050527     c                   move      parsks        notke1
564300091113      * trattativa
564400091113     c                   if        $trattativa
564500091113     c                   eval      notapl = 'T'
564600091113     c                   endif
564700050527
564800050527      * e-mail giacenze nota 88
564900050527     c                   movel     '88'          nottnt
565000050527      * nota della visita
565100050527     c     keyntc        Chain     tfntc01l
565200050527     c                   If        %Found(tfntc01l)
565300050527     c                   Goto      nota87
565400050527     c                   EndIf
565500050527      * nota del cliente se non è un cliente nuovo
565600050527     c                   If        visksc > *Zeros
565700050527     c                   eval      notapl = 'C'
565800050527     c                   move      visksc        notke1
565900050527     c     keyntc        Chain     tfntc01l
566000050527     c                   If        %Found(tfntc01l)
566100050527     c                   Goto      nota87
566200050527     c                   EndIf
566300050527     c                   EndIf
566400050527      * nota non trovata
566500050527     c                   Eval      wema88 = *On
566600050527
566700050527     c     nota87        Tag
566800091113      * visita
566900050527     c                   eval      notapl = 'V'
567000050527     c                   move      parsks        notke1
567100091113      * trattativa
567200091113     c                   if        $trattativa
567300091113     c                   eval      notapl = 'T'
567400091113     c                   endif
567500050527
567600050527      * e-mail danni nota 87
567700050527     c                   movel     '87'          nottnt
567800050527      * nota della visita
567900050527     c     keyntc        Chain     tfntc01l
568000050527     c                   If        %found(tfntc01l)
568100090618     c                   Goto      nota84
568200050527     c                   EndIf
568300050527      * nota del cliente se non è un cliente nuovo
568400050527     c                   If        visksc > *Zeros
568500050527     c                   eval      notapl = 'C'
568600050527     c                   move      visksc        notke1
568700050527     c     keyntc        Chain     tfntc01l
568800050527     c                   If        %Found(tfntc01l)
568900051206     c                   goto      nota84
569000050527     c                   EndIf
569100050527     c                   EndIf
569200050527      * nota non trovata
569300050527     c                   Eval      wema87 = *On
569400051206
569500051206     c     nota84        Tag
569600091113      * visita
569700051206     c                   eval      notapl = 'V'
569800051206     c                   move      parsks        notke1
569900091113      * trattativa
570000091113     c                   if        $trattativa
570100091113     c                   eval      notapl = 'T'
570200091113     c                   endif
570300091113
570400051206      * e-mail invio fattura nota 84
570500051206     c                   movel     '84'          nottnt
570600051206      * nota della visita
570700051206     c     keyntc        Chain     tfntc01l
570800051206     c                   If        %found(tfntc01l)
570900090618     c                   Goto      nota89
571000051206     c                   EndIf
571100051206      * nota del cliente se non è un cliente nuovo
571200051206     c                   If        visksc > *Zeros
571300051206     c                   eval      notapl = 'C'
571400051206     c                   move      visksc        notke1
571500051206     c     keyntc        Chain     tfntc01l
571600051206     c                   If        %Found(tfntc01l)
571700090618     c                   Goto      nota89
571800051206     c                   EndIf
571900051206     c                   EndIf
572000051206      * nota non trovata
572100060203     c                   Eval      wema84 = *On
572200090618
572300090618     c     nota89        Tag
572400091113      * visita
572500090618     c                   eval      notapl = 'V'
572600090618     c                   move      parsks        notke1
572700091113      * trattativa
572800091113     c                   if        $trattativa
572900091113     c                   eval      notapl = 'T'
573000091113     c                   endif
573100090618
573200090618      * e-mail sollecito pagamenti 89
573300090618     c                   movel     '89'          nottnt
573400090618      * nota della visita
573500090618     c     keyntc        Chain     tfntc01l
573600090618     c                   If        %found(tfntc01l)
573700090618     c                   Leavesr
573800090618     c                   EndIf
573900090618      * nota del cliente se non è un cliente nuovo
574000090618     c                   If        visksc > *Zeros
574100090618     c                   eval      notapl = 'C'
574200090618     c                   move      visksc        notke1
574300090618     c     keyntc        Chain     tfntc01l
574400090618     c                   If        %Found(tfntc01l)
574500090618     c                   Leavesr
574600090618     c                   EndIf
574700090618     c                   EndIf
574800090618      * nota non trovata
574900090618     c                   Eval      wema89 = *On
575000050527
575100050527     c                   EndSr
575200050527
575300020110
575400020110      *------------------------------------------------------------------*
575500020110      * Sr_Over           GESTIONE STANDARD OVERFLOW
575600020110      *------------------------------------------------------------------*
575700020110
575800020110     c     Sr_Over       BEGSR
575900020110
576000010801     c                   if        *in80 or curline >= 61
576100020109     c                   write     ta50pfin
576200020109     c                   write     ta50pPP
576300010627     c                   setoff                                       80
576400010627     c                   endif
576500020110
576600010627     c                   endsr
576700020108
576800030730      *---------------------------------------------------------------*
576900030730      * SR_OVERPIU gestione standarad overflow con + 3 righe
577000030730      *---------------------------------------------------------------*
577100030730     C     SR_OVERPIU    BEGSR
577200030730      *
577300030730     c                   if        *in80 or totrig >= 61
577400030730     c                   write     ta50pfin
577500030730     c                   write     ta50pPP
577600030730     c                   setoff                                       80
577700030730     c                   endif
577800030730      *
577900030730     c                   endsr
578000030730
578100020108      *------------------------------------------------------------------*
578200020108      *   Staerr          STAMPA ERRORE
578300020108      *------------------------------------------------------------------*
578400020108
578500020108     c     Staerr        begsr
578600020108
578700050926      * se offerta valorizzo con il numero della visita
578800050926    2c                   if        *in98
578900050926     c                   eval      p07ksc = visksc
579000050926     c                   movel     tamksc        p07nrv
579100050926     c                   move      tamksc        p07prv
579200091117     c                   if        $trattativa
579300091117     c                   eval      p07ntra = tamksc
579400091117     c                   endif
579500050930      * tariffa
579600050926   x2c                   else
579700050926     c                   eval      p07ksc = tamksc
579800050926    2c                   endif
579900091117     c                   if        $trattativa
580000091117     c                   eval      ptratta =
580100091117     c                             '(codice ' +
580200091117     c                             %editc(tamctr:'X') + '/' +
580300091117     c                             %subst(%editc(tamprg:'X'):3:1) + ')'
580400091117     c                   endif
580500091117     c                   if        not $trattativa
580600091117     c                   eval      ptariffa =
580700091117     c                             '(codice ' +
580800091117     c                             %editc(tamctr:'X') + ')'
580900091117     c                   endif
581000050930
581100050930     c                   Write     ta50perr
581200050930     c                   If        woktar = *Off
581300050930     c                   Write     ta50p07
581400050930     c                   EndIf
581500050926
581600020108     c                   endsr
581700060925
581800060925      *---------------------------------------------------------------*
581900060925      * Imposto ATF lungo 7 nelle varie lingue
582000060925      *---------------------------------------------------------------*
582100060925     c     sr_atfk       begsr
582200060925
582300060925      * tariffa peso q.li 0
582400060925     c                   if        wctr1 = '0'
582500060925      * in lingua
582600060925     c                   select
582700060925     c                   when      i45lin = 'I'
582800060925     c                   eval      atf(k) = 'A Q.LE'
582900060925     c                   when      i45lin = '2'
583000060925     c                   eval      atf(k) = '100 KG'
583100060925     c                   when      i45lin = '3'
583200060925     c                   eval      atf(k) = '100 KG'
583300060925     c                   when      i45lin = '4'
583400060925     c                   eval      atf(k) = '100 KG'
583500060925     c                   endsl
583600060925     c                   endif
583700060925
583800060925      * tariffa colli 1
583900060925     c                   if        wctr1 = '1'
584000060925      * in lingua
584100060925     c                   select
584200060925     c                   when      i45lin = 'I'
584300060925     c                   eval      atf(k) = 'A COLLO'
584400060925     c                   when      i45lin = '2'
584500060925     c                   eval      atf(k) = 'P PARC.'
584600060925     c                   when      i45lin = '3'
584700060925     c                   eval      atf(k) = 'COLIS'
584800060925     c                   when      i45lin = '4'
584900060925     c                   eval      atf(k) = 'PRO PAK'
585000060925     c                   endsl
585100060925     c                   endif
585200060925
585300060925      * tariffa spedizione 2
585400060925     c                   if        wctr1 = '2'
585500060925      * in lingua
585600060925     c                   select
585700060925     c                   when      i45lin = 'I'
585800060925     c                   eval      atf(k) = 'A SPED.'
585900060925     c                   when      i45lin = '2'
586000060925     c                   eval      atf(k) = 'P SHIP.'
586100060925     c                   when      i45lin = '3'
586200060925     c                   eval      atf(k) = 'ENVOI'
586300060925     c                   when      i45lin = '4'
586400060925     c                   eval      atf(k) = 'PRO SEN'
586500060925     c                   endsl
586600060925     c                   endif
586700060925
586800060925      * tariffa peso kg 3
586900060925     c                   if        wctr1 = '3'
587000060925      * in lingua
587100060925     c                   select
587200060925     c                   when      i45lin = 'I'
587300060925     c                   eval      atf(k) = 'A KG.'
587400060925     c                   when      i45lin = '2'
587500060925     c                   eval      atf(k) = 'BY KG'
587600060925     c                   when      i45lin = '3'
587700060925     c                   eval      atf(k) = 'AU KG'
587800060925     c                   when      i45lin = '4'
587900060925     c                   eval      atf(k) = 'PRO KG'
588000060925     c                   endsl
588100060925     c                   endif
588200060925
588300060925      * tariffa a valore 4
588400060925     c                   if        wctr1 = '4'
588500060925      * in lingua
588600060925     c                   select
588700060925     c                   when      i45lin = 'I'
588800060925     c                   eval      atf(k) = 'A VAL.'
588900060925     c                   when      i45lin = '2'
589000060925     c                   eval      atf(k) = 'BY VALU'
589100060925     c                   when      i45lin = '3'
589200060925     c                   eval      atf(k) = 'VALEUR'
589300060925     c                   when      i45lin = '4'
589400060925     c                   eval      atf(k) = 'NACH WE'
589500060925     c                   endsl
589600060925     c                   endif
589700060925
589800060925      * tariffa a quantità 5
589900060925     c                   if        wctr1 = '5'
590000060925      * in lingua
590100060925     c                   select
590200060925     c                   when      i45lin = 'I'
590300060925     c                   eval      atf(k) = 'A Q.TA'
590400060925     c                   when      i45lin = '2'
590500060925     c                   eval      atf(k) = 'BY QUAN'
590600060925     c                   when      i45lin = '3'
590700060925     c                   eval      atf(k) = 'QUANT.'
590800060925     c                   when      i45lin = '4'
590900060925     c                   eval      atf(k) = 'NACH ME'
591000060925     c                   endsl
591100060925     c                   endif
591200060925
591300060925      * tariffa valore specifico V (usato nelle tariffe particolari)
591400060925     c                   if        wctr1 = 'V'
591500060925      * in lingua
591600060925     c                   select
591700060925     c                   when      i45lin = 'I'
591800060925     c                   eval      atf(k) = 'VAL.SPE'
591900060925     c                   when      i45lin = '2'
592000060925     c                   eval      atf(k) = 'SET VAL'
592100060925     c                   when      i45lin = '3'
592200060925     c                   eval      atf(k) = 'VAL.SPE'
592300060925     c                   when      i45lin = '4'
592400060925     c                   eval      atf(k) = 'SP.WERT'
592500060925     c                   endsl
592600060925     c                   endif
592700140724
592800140724      * questo è un chiodo per impostare 'A SPED.' quando lo richiede
592900140724      * il programma
593000140724     c                   if        wctr1 = ' '
593100140724      * in lingua
593200140724     c                   select
593300140724     c                   when      i45lin = 'I'
593400140724     c                   eval      atf(k) = 'A SPED.'
593500140724     c                   when      i45lin = '2'
593600140724     c                   eval      atf(k) = 'P SHIP.'
593700140724     c                   when      i45lin = '3'
593800140724     c                   eval      atf(k) = 'PAR ENV'
593900140724     c                   when      i45lin = '4'
594000140724     c                   eval      atf(k) = 'N.SEND'
594100140724     c                   endsl
594200140724     c                   endif
594300151118
594400151118      * tariffa a ritiro solo per tariffa particolare t Diritto di chiamata
594500151118      * ritiro telefonico
594600151118     c                   if        wctr1 = 'R'
594700151118      * in lingua
594800151118     c                   select
594900151118     c                   when      i45lin = 'I'
595000151118     c                   eval      atf(k) = 'A RIT.'
595100151118     c                   when      i45lin = '2'
595200151118     c                   eval      atf(k) = 'A RIT.'
595300151118     c                   when      i45lin = '3'
595400151118     c                   eval      atf(k) = 'A RIT.'
595500151118     c                   when      i45lin = '4'
595600151118     c                   eval      atf(k) = 'A RIT.'
595700151118     c                   endsl
595800151118     c                   endif
595900060925
596000060925     c                   endsr
596100060925
596200060925      *---------------------------------------------------------------*
596300060925      * Imposto la parte finale del numero per il lasciato avviso
596400060925      *---------------------------------------------------------------*
596500060925     c     sr_numgp      begsr
596600060925
596700060925     c                   select
596800060925      * italiano sempre ° fisso nel printer file il campo qua lo pulisco
596900060925     c                   when      i45lin = 'I'
597000060925     c                   clear                   comnavd
597100060925      * inglese cambiano i primi tre poi sempre th
597200060925     c                   when      i45lin = '2'
597300060925     c                   eval      comnavd = 'th'
597400060925     c                   if        comnav = 1
597500060925     c                   eval      comnavd = 'st'
597600060925     c                   endif
597700060925     c                   if        comnav = 2
597800060925     c                   eval      comnavd = 'nd'
597900060925     c                   endif
598000060925     c                   if        comnav = 3
598100060925     c                   eval      comnavd = 'rd'
598200060925     c                   endif
598300060925      * francese cambia solo il primo il resto sempre ieme
598400060925     c                   when      i45lin = '3'
598500060925     c                   eval      comnavd = 'ieme'
598600060925     c                   if        comnav = 1
598700060925     c                   eval      comnavd = 'er'
598800060925     c                   endif
598900060925      * tedesco sempre . fisso nel printer file il campo qua lo pulisco
599000060925     c                   when      i45lin = '4'
599100060925     c                   clear                   comnavd
599200060925     c                   endsl
599300060925
599400060925     c                   endsr
599500060925
599600020108      *------------------------------------------------------------------*
599700020108      *  Sr_Cartab        CARICAMENTO INIZIALE TABELLE
599800020108      *------------------------------------------------------------------*
599900020108
600000020108     c     Sr_Cartab     begsr
600100020110      *  carico la schiera di tutti i codici tassazione
600200020110     c                   clear                   K
600300020110     c                   clear                   K1
600400020110     c                   eval      cod = 'CT'
600500060724     c     Tabk2l        setll     tabel00f
600600020110     c                   DO        *hival
600700060724     c     Tabk2l        READE     Tabel00f
600800020110
600900020110     c                   If        %eof(tabel00f)
601000020110     c                   leave
601100020110     c                   endif
601200020110
601300020110     c                   IF        tblkey <> *blanks
601400020110     c                   add       1             K
601500020110     c                   movel     Tblkey        CCT(K)
601600020110     c                   movel     Tbluni        DSCT
601700020110     c                   move      §CTprv        SCT(K)
601800020110     c                   MOVE      §CTrap        RCT(K)
601900020110     c                   MOVE      §CTnar        NAR(K)
602000020110     c                   MOVE      §CTcor        DS6PR2
602100020110      * se è una regione carico la schiera delle regioni
602200020110     c                   IF        ds6pr2 = '9'
602300020110     c                   add       1             K1
602400020110     c                   movel     Tblkey        CRE(K1)
602500020110     c                   move      §CTdes        DRE(K1)
602600020110     c                   endif
602700020110
602800020110     c                   endif
602900020110
603000020110     c                   ENDDO
603100020110
603200020110      *  carico la schiera delle nazioni con quella ITALIA
603300020110     c                   z-add     1             K
603400020110     c                   eval      cod = '15'
603500060724     c     TABK2l        SETLL     TABEL00F
603600020110     c                   DO        *HIVAL
603700060724     c     TABK2l        READE     TABEL00F
603800020110
603900020110     c                   IF          %eof(tabel00f)
604000020110     c                   leave
604100020110     c                   ENDIF
604200020110
604300020110      * se non è annullato
604400020110     c                   IF        Tblflg = ' '
604500020110
604600020110     c                   Movel     tbluni        DS15
604700020110      * carico solo la nazione ITALIA
604800020110     c                   IF        §15ita = 'I'
604900020110     c                   Movel     Tblkey        NAZ(K)
605000020110     c                   add       1             K
605100020110     c                   endif
605200020110
605300020110     c                   endif
605400020110
605500020110     c                   ENDDO
605600020110
605700020110      * carico la schiera delle consegne particolari
605800020204      * solo quelle che servono per FedEx
605900020204      * flag §1PFFE <> 'N'
606000020110     c                   eval      cod ='1P'
606100020110     c                   z-add     0             K
606200060704     c     TABK2l        SETLL     TABEL00F
606300020110
606400020110     c                   DO        *hival
606500020110
606600060704     c     TABK2l        READE     TABEL00F
606700020110
606800020110     c                   if        %eof(tabel00f)
606900020110     c                   leave
607000020110     c                   endif
607100020110
607200020205      * escludo gli annullati
607300020205     c                   if        tblflg <> ' '
607400020205     c                   iter
607500020205     c                   endif
607600020205
607700020205     c                   movel     TBLUNI        DS1P
607800020205     c                   if        §1pffe <> 'N'
607900020110
608000020110      * carico tutte le consegne particolari
608100020110     c                   add       1             K
608200020110     c                   movel     tblkey        FTC(K)
608300020110     c                   movel     tbluni        DTC(K)
608400020110      * carico l'ordinamento in stampa
608500020110     c                   movel     §1post        OST(K)
608600020110      * carico l'ordinamento in stampa
608700020110     c                   movel     §1ptst        TST(K)
608800020205      * carico il flag x recupero descrizione FedEx
608900020204     c                   movel     §1pdfe        dfe(K)
609000020110
609100020110     c                   endif
609200020110
609300020110     c                   ENDDO
609400020110
609500020110      * carico la schiera delle estensioni consegne particolari
609600020110     c                   eval      cod ='SP'
609700020110     c                   z-add     0             K
609800060704     c     TABK2l        SETLL     TABEL00F
609900020110
610000020110     c                   DO        *hival
610100020110
610200060704     c     TABK2l        READE     TABEL00F
610300020110
610400020110     c                   if        %eof(tabel00f)
610500020110     c                   leave
610600020110     c                   endif
610700020110
610800020110     c                   IF        tblflg = ' '
610900020110     c                   movel     TBLUNI        DSSP
611000020110
611100020110     c                   movel     TBLKEY        comftc
611200020110
611300020110      * carico tutte le consegne particolari
611400020110     c                   z-add     1             K
611500020110     c                   setoff                                       69
611600020110     c     comftc        lookup    FTC(K)                                 69
611700020110     c                   if        *in69
611800020204      * se la descrizione è da recuperare flag §1PDFE = 'S'
611900020204      * imposto già edtc = §SPDFE
612000020204     c                   if        dfe(k) = 'S'
612100020204     c                   movel     §spdfe        edtc(k)
612200020204     c                   else
612300020110     c                   movel     §spdes        EDTC(K)
612400020204     c                   endif
612500020110     c                   movel     §spdac        EDAC(K)
612600020110     c                   movel     §spsud        ESUD(K)
612700020110     c                   endif
612800020110
612900020110     c                   endif
613000020110
613100020110     c                   ENDDO
613200020110      * ordino le tariffe particolari in base all'ordine in stampa
613300020110
613400020110     c                   movel     OST           WOS
613500020110     c                   sorta     wos
613600020110     c                   do        50            j
613700020110     c                   z-add     1             jj
613800020110     c                   setoff                                       69
613900020110     c                   if        wos(j) <> '99' and wos(j) <> *blank
614000020110     c     wos(j)        lookup    ost(jj)                                69
614100020110      * carico la schiera delle tariffe particolare in ordine di stampa
614200020110     c                   if        *in69 = *on
614300020110     c                   add       1             jjj
614400020110     c                   move      ftc(jj)       cp(jjj)
614500020110     c                   endif
614600020110     c                   endif
614700020110     c                   enddo
614800020107
614900020107      * aggancio la tabella delle variabili giacenza
615000020108     c                   eval      cod = '2G'
615100020108     c                   clear                   key
615200020108     c                   movel     '1'           key
615300020108     c     tabkey        chain     tabel00f
615400020108     c                   if        %found(tabel00f)
615500020108     c                   movel     tbluni        ds2g
615600020108     c                   else
615700020108     c                   clear                   ds2g
615800020108     c                   endif
615900021022      *
616000021023      * Richiamo FnLv41 per verificare/creare testo e reperire ds1h
616100021022     C                   clear                   Ds1H
616200021022     C                   move      *off          $NoStp            1
616300021022     C                   clear                   Fnlv41ds
616400021022     C                   movel     '3'           I41Mod
616500021022     C                   movel     I45TTX        I41Let
616600021022     C                   CALL      'FNLV41R'
616700021022     C                   PARM                    FNLV41DS
616800021022      *
616900021022      * Se non ho errori stampo
617000021023     C     o41err        ifne      'E'
617100021022     C                   movel     o41dta        DS1H
617200020107
617300020107      * stampo indirizzo di filiale
617400020108     c                   eval      *in92 = (§1Hfv1 = 'S')
617500020107      * stampo indirizzo cliente
617600020108     c                   eval      *in93 = (§1Hfv2 = 'S')
617700020107      * stampo alla C.A.
617800020108     c                   eval      *in94 = (§1Hfv4 = 'S')
617900020107      * stampo "Telefonando chiedere .........."
618000020108     c                   eval      *in95 = (§1Hfv5 = 'S')
618100020107      * stampo la data
618200020108     c                   eval      *in96 = (§1Hfv6 = 'S')
618300020107      * STAMPA RIFERIMENTI
618400020108     c                   eval      *in97 = (§1Hfv9 = 'S'and i45nr1 <> *blanks)
618500021022      *
618600021022     C                   ELSE
618700021022     C                   move      *on           $NoStp
618800021022     C                   ENDIF
618900031120
619000101015      /free
619100101015       //?Carico scatti ISTAT
619200101015         SISsca = 0;
619300101015         clear DIA;
619400101015         setll (SISsca) TISIS01L;
619500101015
619600101015         DOW  not $EoF;
619700101015
619800101015           read TISIS01L;
619900101015
620000101015           IF  %Eof(TISIS01L);
620100101015             $EoF = *on;
620200101015             leave;
620300101015           ENDIF;
620400101015
620500101015           DIA(SISsca) = SISdata;
620600101015
620700101015         ENDDO;
620800101015
620900101015      /end-free
621000020107
621100020108     c                   endsr
621200020107
621300020110      *------------------------------------------------------------------*
621400020110      *                   ROUTINE INIZIALE
621500020110      *------------------------------------------------------------------*
621600020108
621700020108     c     *inzsr        begsr
621800020108
621900020108     c     *entry        plist
622000020108     c                   parm                    kpjba
622100020108     c                   parm                    fnlv45ds
622200020108
622300020625     c                   z-add     1             codut             1 0
622400020625
622500020625      * reperisco i dati utente
622600020625     c     *dtaara       define    §azute        AzuteDs
622700020625     c     *dtaara       define    §datiute      DDatiUte
622800020625
622900020625     c                   in(E)     *dtaara
623000020625     c                   IF        %Error or RSUT = *blanks
623100020625     c                   call      'TIBS34R'
623200020625     c                   parm                    Tibs34Ds
623300020625     c                   in        *dtaara
623400020625     c                   ENDIF
623500110517
623600110517      * chiodo fisso 'BRT S.p.A.'
623700110517     c                   eval      rsutold = 'BRT S.p.A.'
623800020919      *
623900020919     C                   MOVEL     SIMFEL        simfelA           3
624000060704
624100060704      * lingua per tabelle
624200060704     c                   clear                   kcodut            1 0
624300060704     c                   select
624400060704     c                   when      i45lin = 'I'
624500060704     c                   eval      kcodut = 1
624600060704     c                   when      i45lin = '2'
624700060704     c                   eval      kcodut = 2
624800060712     c                   when      i45lin = '3'
624900060704     c                   eval      kcodut = 3
625000060704     c                   when      i45lin = '4'
625100060704     c                   eval      kcodut = 4
625200060704     c                   endsl
625300020625
625400020107      * preparo il campo PAGE per la numerazione delle pagine
625500020107     c                   eval      page = 1
625600020107      * carico le tabelle
625700020107     c                   exsr      Sr_Cartab
625800021022      *
625900021022      * proseguo se ho i dati per uso testi
626000021022     c                   if        $NoStp = *off
626100020108
626200020107      * TARIFFA O OFFERTA
626300020108
626400020108      * --- O F F E R T A ---
626500020108
626600110104     c**                 if        i45ast = 'V' or i45ast = 'T'
626700110104     c                   if        i45ast = 'T'
626800020108      * accendo il 98
626900020108     c                   seton                                        98
627000091113      * se trattativa imposto flag
627100091113     c                   eval      $trattativa = (i45ast = 'T')
627200091117     c                   eval      *in85 = (i45ast = 'T')
627300020108      * valorizzo il campo del codice cliente tariffa con il numero visita
627400020108     c                   eval      parsks = i45nrv
627500020108      * apro i file visita/offerta
627600020108     c                   open      tfaco00f
627700020108     c                   open      tfind00f
627800020108     c                   open      tfclp00f
627900020108     c                   open      tnofm01l
628000061011     c                   open      tiofd01l
628100020108     c                   open      tiopt01l
628200020108     c                   open      tiopd01l
628300020108     c                   open      tiogc01l
628400110104     c**                 if        i45ast = 'V'
628500110104     c**                 open      tnvis05l
628600110104     c**                 endif
628700091022     c                   if        i45ast = 'T'
628800091022     c                   open      Tivis05l
628900091022     c                   endif
629000020108      *  i45ast = 'C'
629100020108     c                   else
629200020108
629300020108      * --- T A R I F F A ---
629400061011      * apro i file visita/offerta
629500061011     c                   open      tntam01l
629600061011     c                   open      titad04l
629700061011     c                   open      titpt01l
629800061011     c                   open      titpd01l
629900061011     c                   open      titgc01l
630000020108
630100020108      * valorizzo il campo del codice cliente tariffa
630200020108     c                   eval      parsks = i45ksc
630300020108
630400020108     c                   endif
630500021022     c                   endif
630600030508
630700030508      * Prendo i dati di dft per le tariffe FedEx
630800030508     c                   clear                   ddfe
630900030508     c                   clear                   tibs02ds
631000030508     c                   movel     'C'           t02mod
631100030508     c                   movel     knsif         t02sif
631200030508     c                   movel     'DFE'         t02cod
631300030508     c                   movel(p)  'FED'         t02ke1
631400030508     c                   call      'TIBS02R'
631500030508     c                   parm                    kpjba
631600030508     c                   parm                    tibs02ds
631700030508     c                   if        t02err = *blanks
631800030508     c                   movel     t02uni        ddfe
631900030508     c                   endif
632000030508
632100020108      *------------------------------------------------------------------*
632200020108      *               DEFINIZIONI DELLE CHIAVI
632300020108      *------------------------------------------------------------------*
632400020108
632500020108      * --- TABEL00F ---
632600020108     c     tabkey        klist
632700020108     c                   kfld                    codut
632800020108     c                   kfld                    cod
632900020108     c                   kfld                    key
633000060704
633100060704      * accesso per codice su TABEL00F in lingua
633200060704     c     tabkeyl       klist
633300060704     c                   kfld                    kcodut
633400060704     c                   kfld                    cod
633500060704     c                   kfld                    key
633600060704
633700060704      * accesso per codice su TABEL00F  parziale in lingua
633800060704     c     tabk2l        klist
633900060704     c                   kfld                    kcodut
634000060704     c                   kfld                    cod
634100020108      * --- TNTAM01L/TNOFM01L ---
634200020108     c     ds1ktm        klist
634300020108     c                   kfld                    ds1ksc
634400020108     c                   kfld                    ds1ctr
634500020108     c                   kfld                    ds1prg
634600020108      * --- TITAD04L ---
634700020108     c     ds1ktd        klist
634800020108     c                   kfld                    ds1ksc
634900020108     c                   kfld                    ds1ctr
635000020108     c                   kfld                    ds1prg
635100020108     c                   kfld                    comfil
635200020108
635300020108      * --- TITPT e TITPD ---
635400020108     c     ktpt          klist
635500020108     c                   kfld                    ds1ksc
635600020108     c                   kfld                    ds1ctr
635700020108     c                   kfld                    ds1prg
635800020108     c                   kfld                    savftc
635900020108      * --- TITPD ---
636000020108     c     ktpd          klist
636100020108     c                   kfld                    ds1ksc
636200020108     c                   kfld                    ds1ctr
636300020108     c                   kfld                    ds1prg
636400020108     c                   kfld                    savftc
636500020108     c                   kfld                    savorp
636600020108
636700020108     c     ktpd2         klist
636800020108     c                   kfld                    ds1ksc
636900020108     c                   kfld                    ds1ctr
637000020108     c                   kfld                    ds1prg
637100020108     c                   kfld                    savftc
637200020108     c                   kfld                    savorp
637300020108     c                   kFLD                    savsgl
637400020108
637500020108     c     ktpdwrk       klist
637600020108     c                   kfld                    ds1ksc
637700020108     c                   kfld                    ds1ctr
637800020108     c                   kfld                    ds1prg
637900020108     c                   kfld                    wrkftc
638000020108     c                   kfld                    wrkorp
638100020108     c                   kfld                    wrksgl
638200020108      * --- CNACO / TFACO ---
638300020108     c     ds7kac        klist
638400020108     c                   kfld                    codut
638500020108     c                   kfld                    i45kcc
638600020108     c                   kfld                    ksc
638700020108      * --- TFNTC ---
638800020108     c     keyntc        klist
638900020108     c                   kfld                    notapl
639000020108     c                   kfld                    notke1
639100020108     c                   kfld                    notke2
639200020108     c                   kfld                    nottnt
639300041230      * accesso TIVSS02L
639400041230     c     kTivss        klist
639500041230     c                   kfld                    vssksu
639600041230     c                   kfld                    vssisv
639700080211
639800080211     c     kCnabi        Klist
639900080211     c                   Kfld                    pcabi
640000080211     c                   Kfld                    pccab
640100020108
640200020108     c                   endsr
640300010530**   MSG
640400020108       =====>  TARIFFA/OFFERTA NON STAMPABILE IN MODO CORRETTO  <=====        1
640500020108                                                                              2
640600020108CLIENTE NON CODIFICATO O ANNULLATO                                            3
640700020108NESSUNA RIGA DI DETTAGLIO X QUESTA TARIFFA/OFFERTA                            4
640800020108PIU' DI 200 SCAGLIONI X CODICE DI TASSAZIONE                                  5
640900010530CODICE TASSAZIONE NON TROVATO !!!!!!!!!!!      CUMELA MO' ?                   6
641000050616CODICE REGIONE DEL CODICE DI TASSAZIONE NON ESISTE IN TABELLA                 7
641100010530UN CODICE DI TASSAZIONE DELLA TARIFFA HA UNO SCAGLIONE NON PREVISTO           8
641200020109                                                                              9
641300050616PIU' DI 15 PROVINCIE X 1 REGIONE? TABELLA ERRATA O  T N T P D  NON ALLINEATO  10
641400050616                                                                              1
641500020109                                                                              2
641600010530XXXXXXXXXXXXXXXXXXXX    *** ERRORI IN STAMPA TARIFFA/OFFERTA ***              3
641700010530UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON PREVISTO   4
641800050616PIU' DI 18 SCAGLIONI X CODICE DI TASSAZIONE PER LE TARIFFE PARTICOLARI        5
641900020116PIU' DI 7 CODICI DI TASSAZIONE PER LINEA DI PARTENZA                          6
642000050324UN CODICE DI TASSAZIONE DELLA TARIFFA PARTIC. HA UNO SCAGLIONE NON VALIDO     7
