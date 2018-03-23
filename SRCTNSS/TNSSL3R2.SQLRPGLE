000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200091006     F* TNSSL3R2 H-----------------------------------------------------H
000300090204     F*-------   Statistiche spedizioni
000400080312     F*----------------------------------------------------------------H
000500121121      *? PER IL FILE WFLSS60F:
000600080312     F*? IL PGM NON CONTEGGIA LE SPEDIZIONI IMPORT PER LINEA DI ARRIVO
000700080312     F*? In un conteggio totale quindi di colli partenza e colli in arr
000800080312     F*? ivo ad una filiale, i colli IMPORT MANCANO
000900000000     F*----------------------------------------------------------------H
001000970527     FAZORG01L  IF   E           K DISK
001100990811     FTITAS38C  IF   E           K DISK
001200090205     Ffiar531c  IF   E           K DISK
001300970519     FTABEL00F  IF   E           K DISK
001400091006     Ftntbe01l  IF   E           K DISK
001500120905     FWFLSS61l  UF A E           K DISK
001600120904     FWSLSS50f  if A E             DISK
001700120904     FWSLSS55f  if A E             DISK    rename(wslss500:wslss550)
001800090206     FWfsmp00f  o  A E             DISK
001900090915     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)  usropn
002000980826     D FIL             S              3  0 DIM(400)                             FILIALI
002100090204     D DIL             S             20    DIM(400)                             descrizione filiali
002200111129     D car             S              3  0 DIM(400)                             codice area tif(ale
002300090204     D tif             S              1    DIM(400)                             tipo filiale
002400090204     D lpo             S              1  0 DIM(400)                             livello filiale
002500970529     D TBO             S              2    DIM(20)                              TIPI BOLLA OK
002600090915     D CMD             S              1    DIM(61) CTDATA PERRCD(61)
002700121121     d
002800121203     d*  ATTENZIONE!! Se si aumenta la skiera corregge anche il ciclo di caricamento
002900121203     d*   nell'SQL per il caricamento codici cli particolari (cicla per 1100)
003000121203     d
003100121126     D ksc_EX          S             10    DIM(1100)
003200121121     d* Solo per clienti Particolari: il terminal di riferimento ddel codice
003300121126     D ksc_tr          S              3  0 DIM(1100)
003400130819     D ksc_are         S              3  0 DIM(1100)
003500121121     d
003600120903     D* SCHIERA TABELLA FASCIE DI PESO (2L)
003700120903     D FPR             S              2    DIM(20)
003800120903     D KIR             S              6  0 DIM(20)
003900120903     D KFR             S              6  0 DIM(20)
004000091006     d
004100970527     D KPJBA         E DS
004200090204     D  §DAL                 247    254  0
004300090204     D  §AL                  255    262  0
004400090601     D  §opz                 263    263
004500090601     D  §tipo                264    264
004600110711     D  §stat                265    265
004700090204
004800090204     D og143         E DS
004900090204     D og148         E DS
005000090204
005100120903     D ds2l          E DS
005200970529     D DSTB          E DS
005300090204     D DS05          E DS
005400090205     D Dar5trs       E DS
005500091006     D dcxb          E DS
005600121121     D dcxq          E DS
005700110711     d
005800110711     d* DS x controllo riempimento schiere
005900110711     d trul0sds      e ds
006000090204
006100980826     D DSFIL           DS                  OCCURS(400)
006200970527     D  WTSPIP                 1      8  0
006300970527     D  WTSPIA                 9     16  0
006400970527     D  WTSPEI                17     24  0
006500970527     D  WTSPEE                25     32  0
006600970527     D  WTCLIP                33     40  0
006700970527     D  WTCLIA                41     48  0
006800970527     D  WTCLEI                49     56  0
006900970527     D  WTCLEE                57     64  0
007000980826     D  WTKGIP                65     75  1
007100980826     D  WTKGIA                76     86  1
007200980826     D  WTKGEI                87     97  1
007300980826     D  WTKGEE                98    108  1
007400980826     D  WTMCIP               109    119  3
007500980826     D  WTMCIA               120    130  3
007600980826     D  WTMCEI               131    140  3
007700980826     D  WTMCEE               141    150  3
007800000223     D  WTSPEF               151    158  0
007900000223     D  WTCLEF               159    166  0
008000000223     D  WTKGEF               167    177  1
008100000223     D  WTMCEF               178    188  3
008200000607     D  WTSPEA               189    196  0
008300000607     D  WTCLEA               197    204  0
008400000607     D  WTKGEA               205    215  1
008500000607     D  WTMCEA               216    226  3
008600000607     D  WTSPED               227    234  0
008700000607     D  WTCLED               235    242  0
008800000607     D  WTKGED               243    253  1
008900000630     D  WTMCED               254    264  3
009000000630     D  WTSPPA               265    272  0
009100000630     D  WTCLPA               273    280  0
009200000630     D  WTKGPA               281    291  1
009300000630     D  WTMCPA               292    302  3
009400020321     D  WTSPEX               303    310  0
009500020321     D  WTCLEX               311    318  0
009600020321     D  WTKGEX               319    329  1
009700020321     D  WTMCEX               330    340  3
009800090206     D  WTSPIP_R                           like(WTSPIP)
009900090206     D  WTSPIA_r                           like(wtspia)
010000090206     D  WTSPEI_r                           like(wtspei)
010100090206     D  WTSPEE_r                           like(wtspee)
010200090206     D  WTCLIP_r                           like(wtclip)
010300090206     D  WTCLIA_r                           like(wtclia)
010400090206     D  WTCLEI_r                           like(wtclei)
010500090206     D  WTCLEE_r                           like(wtclee)
010600090206     D  WTKGIP_r                           like(wtkgip)
010700090206     D  WTKGIA_r                           like(wtkgia)
010800090206     D  WTKGEI_r                           like(wtkgei)
010900090206     D  WTKGEE_r                           like(wtkgee)
011000090206     D  WTMCIP_r                           like(wtmcip)
011100090206     D  WTMCIA_r                           like(wtmcia)
011200090206     D  WTMCEI_r                           like(wtmcei)
011300090206     D  WTMCEE_r                           like(wtmcee)
011400090206     D  WTSPEF_r                           like(wtspef)
011500090206     D  WTCLEF_r                           like(wtclef)
011600090206     D  WTKGEF_r                           like(wtkgef)
011700090206     D  WTMCEF_r                           like(wtmcef)
011800090206     D  WTSPEA_r                           like(wtspea)
011900090206     D  WTCLEA_r                           like(wtclea)
012000090206     D  WTKGEA_r                           like(wtkgea)
012100090206     D  WTMCEA_r                           like(wtmcea)
012200090206     D  WTSPED_r                           like(wtsped)
012300090206     D  WTCLED_r                           like(wtcled)
012400090206     D  WTKGED_r                           like(wtkged)
012500090206     D  WTMCED_r                           like(wtmced)
012600090206     D  WTSPPA_r                           like(wtsppa)
012700090206     D  WTCLPA_r                           like(wtclpa)
012800090206     D  WTKGPA_r                           like(wtkgpa)
012900090206     D  WTMCPA_r                           like(wtmcpa)
013000090206     D  WTSPEX_r                           like(wtspex)
013100090206     D  WTCLEX_r                           like(wtclex)
013200090206     D  WTKGEX_r                           like(wtkgex)
013300090206     D  WTMCEX_r                           like(wtmcex)
013400091006     d* Giornaliero extra budget
013500091006     D  WTSPIP_E                           like(WTSPIP)
013600091006     D  WTSPIA_e                           like(wtspia)
013700091006     D  WTSPEI_e                           like(wtspei)
013800091006     D  WTSPEE_e                           like(wtspee)
013900091006     D  WTCLIP_e                           like(wtclip)
014000091006     D  WTCLIA_e                           like(wtclia)
014100091006     D  WTCLEI_e                           like(wtclei)
014200091006     D  WTCLEE_e                           like(wtclee)
014300091006     D  WTKGIP_e                           like(wtkgip)
014400091006     D  WTKGIA_e                           like(wtkgia)
014500091006     D  WTKGEI_e                           like(wtkgei)
014600091006     D  WTKGEE_e                           like(wtkgee)
014700091006     D  WTMCIP_e                           like(wtmcip)
014800091006     D  WTMCIA_e                           like(wtmcia)
014900091006     D  WTMCEI_e                           like(wtmcei)
015000091006     D  WTMCEE_e                           like(wtmcee)
015100091006     D  WTSPEF_e                           like(wtspef)
015200091006     D  WTCLEF_e                           like(wtclef)
015300091006     D  WTKGEF_e                           like(wtkgef)
015400091006     D  WTMCEF_e                           like(wtmcef)
015500091006     D  WTSPEA_e                           like(wtspea)
015600091006     D  WTCLEA_e                           like(wtclea)
015700091006     D  WTKGEA_e                           like(wtkgea)
015800091006     D  WTMCEA_e                           like(wtmcea)
015900091006     D  WTSPED_e                           like(wtsped)
016000091006     D  WTCLED_e                           like(wtcled)
016100091006     D  WTKGED_e                           like(wtkged)
016200091006     D  WTMCED_e                           like(wtmced)
016300091006     D  WTSPPA_e                           like(wtsppa)
016400091006     D  WTCLPA_e                           like(wtclpa)
016500091006     D  WTKGPA_e                           like(wtkgpa)
016600091006     D  WTMCPA_e                           like(wtmcpa)
016700091006     D  WTSPEX_e                           like(wtspex)
016800091006     D  WTCLEX_e                           like(wtclex)
016900091006     D  WTKGEX_e                           like(wtkgex)
017000091006     D  WTMCEX_e                           like(wtmcex)
017100091006     d* Riepilogativo extra budget
017200091006     D  WTSPIP_re                          like(WTSPIP)
017300091006     D  WTSPIA_re                          like(wtspia)
017400091006     D  WTSPEI_re                          like(wtspei)
017500091006     D  WTSPEE_re                          like(wtspee)
017600091006     D  WTCLIP_re                          like(wtclip)
017700091006     D  WTCLIA_re                          like(wtclia)
017800091006     D  WTCLEI_re                          like(wtclei)
017900091006     D  WTCLEE_re                          like(wtclee)
018000091006     D  WTKGIP_re                          like(wtkgip)
018100091006     D  WTKGIA_re                          like(wtkgia)
018200091006     D  WTKGEI_re                          like(wtkgei)
018300091006     D  WTKGEE_re                          like(wtkgee)
018400091006     D  WTMCIP_re                          like(wtmcip)
018500091006     D  WTMCIA_re                          like(wtmcia)
018600091006     D  WTMCEI_re                          like(wtmcei)
018700091006     D  WTMCEE_re                          like(wtmcee)
018800091006     D  WTSPEF_re                          like(wtspef)
018900091006     D  WTCLEF_re                          like(wtclef)
019000091006     D  WTKGEF_re                          like(wtkgef)
019100091006     D  WTMCEF_re                          like(wtmcef)
019200091006     D  WTSPEA_re                          like(wtspea)
019300091006     D  WTCLEA_re                          like(wtclea)
019400091006     D  WTKGEA_re                          like(wtkgea)
019500091006     D  WTMCEA_re                          like(wtmcea)
019600091006     D  WTSPED_re                          like(wtsped)
019700091006     D  WTCLED_re                          like(wtcled)
019800091006     D  WTKGED_re                          like(wtkged)
019900091006     D  WTMCED_re                          like(wtmced)
020000091006     D  WTSPPA_re                          like(wtsppa)
020100091006     D  WTCLPA_re                          like(wtclpa)
020200091006     D  WTKGPA_re                          like(wtkgpa)
020300091006     D  WTMCPA_re                          like(wtmcpa)
020400091006     D  WTSPEX_re                          like(wtspex)
020500091006     D  WTCLEX_re                          like(wtclex)
020600091006     D  WTKGEX_re                          like(wtkgex)
020700091006     D  WTMCEX_re                          like(wtmcex)
020800100402     d
020900100402     D DSFIL_e         DS                  OCCURS(400)
021000100402     D  eWTSPIP                1      8  0
021100100402     D  eWTSPIA                9     16  0
021200100402     D  eWTSPEI               17     24  0
021300100402     D  eWTSPEE               25     32  0
021400100402     D  eWTCLIP               33     40  0
021500100402     D  eWTCLIA               41     48  0
021600100402     D  eWTCLEI               49     56  0
021700100402     D  eWTCLEE               57     64  0
021800100402     D  eWTKGIP               65     75  1
021900100402     D  eWTKGIA               76     86  1
022000100402     D  eWTKGEI               87     97  1
022100100402     D  eWTKGEE               98    108  1
022200100402     D  eWTMCIP              109    119  3
022300100402     D  eWTMCIA              120    130  3
022400100402     D  eWTMCEI              131    140  3
022500100402     D  eWTMCEE              141    150  3
022600100402     D  eWTSPEF              151    158  0
022700100402     D  eWTCLEF              159    166  0
022800100402     D  eWTKGEF              167    177  1
022900100402     D  eWTMCEF              178    188  3
023000100402     D  eWTSPEA              189    196  0
023100100402     D  eWTCLEA              197    204  0
023200100402     D  eWTKGEA              205    215  1
023300100402     D  eWTMCEA              216    226  3
023400100402     D  eWTSPED              227    234  0
023500100402     D  eWTCLED              235    242  0
023600100402     D  eWTKGED              243    253  1
023700100402     D  eWTMCED              254    264  3
023800100402     D  eWTSPPA              265    272  0
023900100402     D  eWTCLPA              273    280  0
024000100402     D  eWTKGPA              281    291  1
024100100402     D  eWTMCPA              292    302  3
024200100402     D  eWTSPEX              303    310  0
024300100402     D  eWTCLEX              311    318  0
024400100402     D  eWTKGEX              319    329  1
024500100402     D  eWTMCEX              330    340  3
024600100402     D  eWTSPIP_R                          like(WTSPIP)
024700100402     D  eWTSPIA_r                          like(wtspia)
024800100402     D  eWTSPEI_r                          like(wtspei)
024900100402     D  eWTSPEE_r                          like(wtspee)
025000100402     D  eWTCLIP_r                          like(wtclip)
025100100402     D  eWTCLIA_r                          like(wtclia)
025200100402     D  eWTCLEI_r                          like(wtclei)
025300100402     D  eWTCLEE_r                          like(wtclee)
025400100402     D  eWTKGIP_r                          like(wtkgip)
025500100402     D  eWTKGIA_r                          like(wtkgia)
025600100402     D  eWTKGEI_r                          like(wtkgei)
025700100402     D  eWTKGEE_r                          like(wtkgee)
025800100402     D  eWTMCIP_r                          like(wtmcip)
025900100402     D  eWTMCIA_r                          like(wtmcia)
026000100402     D  eWTMCEI_r                          like(wtmcei)
026100100402     D  eWTMCEE_r                          like(wtmcee)
026200100402     D  eWTSPEF_r                          like(wtspef)
026300100402     D  eWTCLEF_r                          like(wtclef)
026400100402     D  eWTKGEF_r                          like(wtkgef)
026500100402     D  eWTMCEF_r                          like(wtmcef)
026600100402     D  eWTSPEA_r                          like(wtspea)
026700100402     D  eWTCLEA_r                          like(wtclea)
026800100402     D  eWTKGEA_r                          like(wtkgea)
026900100402     D  eWTMCEA_r                          like(wtmcea)
027000100402     D  eWTSPED_r                          like(wtsped)
027100100402     D  eWTCLED_r                          like(wtcled)
027200100402     D  eWTKGED_r                          like(wtkged)
027300100402     D  eWTMCED_r                          like(wtmced)
027400100402     D  eWTSPPA_r                          like(wtsppa)
027500100402     D  eWTCLPA_r                          like(wtclpa)
027600100402     D  eWTKGPA_r                          like(wtkgpa)
027700100402     D  eWTMCPA_r                          like(wtmcpa)
027800100402     D  eWTSPEX_r                          like(wtspex)
027900100402     D  eWTCLEX_r                          like(wtclex)
028000100402     D  eWTKGEX_r                          like(wtkgex)
028100100402     D  eWTMCEX_r                          like(wtmcex)
028200100402     d* Giornaliero extra budget
028300100402     D  eWTSPIP_E                          like(WTSPIP)
028400100402     D  eWTSPIA_e                          like(wtspia)
028500100402     D  eWTSPEI_e                          like(wtspei)
028600100402     D  eWTSPEE_e                          like(wtspee)
028700100402     D  eWTCLIP_e                          like(wtclip)
028800100402     D  eWTCLIA_e                          like(wtclia)
028900100402     D  eWTCLEI_e                          like(wtclei)
029000100402     D  eWTCLEE_e                          like(wtclee)
029100100402     D  eWTKGIP_e                          like(wtkgip)
029200100402     D  eWTKGIA_e                          like(wtkgia)
029300100402     D  eWTKGEI_e                          like(wtkgei)
029400100402     D  eWTKGEE_e                          like(wtkgee)
029500100402     D  eWTMCIP_e                          like(wtmcip)
029600100402     D  eWTMCIA_e                          like(wtmcia)
029700100402     D  eWTMCEI_e                          like(wtmcei)
029800100402     D  eWTMCEE_e                          like(wtmcee)
029900100402     D  eWTSPEF_e                          like(wtspef)
030000100402     D  eWTCLEF_e                          like(wtclef)
030100100402     D  eWTKGEF_e                          like(wtkgef)
030200100402     D  eWTMCEF_e                          like(wtmcef)
030300100402     D  eWTSPEA_e                          like(wtspea)
030400100402     D  eWTCLEA_e                          like(wtclea)
030500100402     D  eWTKGEA_e                          like(wtkgea)
030600100402     D  eWTMCEA_e                          like(wtmcea)
030700100402     D  eWTSPED_e                          like(wtsped)
030800100402     D  eWTCLED_e                          like(wtcled)
030900100402     D  eWTKGED_e                          like(wtkged)
031000100402     D  eWTMCED_e                          like(wtmced)
031100100402     D  eWTSPPA_e                          like(wtsppa)
031200100402     D  eWTCLPA_e                          like(wtclpa)
031300100402     D  eWTKGPA_e                          like(wtkgpa)
031400100402     D  eWTMCPA_e                          like(wtmcpa)
031500100402     D  eWTSPEX_e                          like(wtspex)
031600100402     D  eWTCLEX_e                          like(wtclex)
031700100402     D  eWTKGEX_e                          like(wtkgex)
031800100402     D  eWTMCEX_e                          like(wtmcex)
031900100402     d* Riepilogativo extra budget
032000100402     D  eWTSPIP_re                         like(WTSPIP)
032100100402     D  eWTSPIA_re                         like(wtspia)
032200100402     D  eWTSPEI_re                         like(wtspei)
032300100402     D  eWTSPEE_re                         like(wtspee)
032400100402     D  eWTCLIP_re                         like(wtclip)
032500100402     D  eWTCLIA_re                         like(wtclia)
032600100402     D  eWTCLEI_re                         like(wtclei)
032700100402     D  eWTCLEE_re                         like(wtclee)
032800100402     D  eWTKGIP_re                         like(wtkgip)
032900100402     D  eWTKGIA_re                         like(wtkgia)
033000100402     D  eWTKGEI_re                         like(wtkgei)
033100100402     D  eWTKGEE_re                         like(wtkgee)
033200100402     D  eWTMCIP_re                         like(wtmcip)
033300100402     D  eWTMCIA_re                         like(wtmcia)
033400100402     D  eWTMCEI_re                         like(wtmcei)
033500100402     D  eWTMCEE_re                         like(wtmcee)
033600100402     D  eWTSPEF_re                         like(wtspef)
033700100402     D  eWTCLEF_re                         like(wtclef)
033800100402     D  eWTKGEF_re                         like(wtkgef)
033900100402     D  eWTMCEF_re                         like(wtmcef)
034000100402     D  eWTSPEA_re                         like(wtspea)
034100100402     D  eWTCLEA_re                         like(wtclea)
034200100402     D  eWTKGEA_re                         like(wtkgea)
034300100402     D  eWTMCEA_re                         like(wtmcea)
034400100402     D  eWTSPED_re                         like(wtsped)
034500100402     D  eWTCLED_re                         like(wtcled)
034600100402     D  eWTKGED_re                         like(wtkged)
034700100402     D  eWTMCED_re                         like(wtmced)
034800100402     D  eWTSPPA_re                         like(wtsppa)
034900100402     D  eWTCLPA_re                         like(wtclpa)
035000100402     D  eWTKGPA_re                         like(wtkgpa)
035100100402     D  eWTMCPA_re                         like(wtmcpa)
035200100402     D  eWTSPEX_re                         like(wtspex)
035300100402     D  eWTCLEX_re                         like(wtclex)
035400100402     D  eWTKGEX_re                         like(wtkgex)
035500100402     D  eWTMCEX_re                         like(wtmcex)
035600100402     d
035700100402     D DSFIL_h         DS                  OCCURS(400)
035800100402     D  hWTSPIP                1      8  0
035900100402     D  hWTSPIA                9     16  0
036000100402     D  hWTSPEI               17     24  0
036100100402     D  hWTSPEE               25     32  0
036200100402     D  hWTCLIP               33     40  0
036300100402     D  hWTCLIA               41     48  0
036400100402     D  hWTCLEI               49     56  0
036500100402     D  hWTCLEE               57     64  0
036600100402     D  hWTKGIP               65     75  1
036700100402     D  hWTKGIA               76     86  1
036800100402     D  hWTKGEI               87     97  1
036900100402     D  hWTKGEE               98    108  1
037000100402     D  hWTMCIP              109    119  3
037100100402     D  hWTMCIA              120    130  3
037200100402     D  hWTMCEI              131    140  3
037300100402     D  hWTMCEE              141    150  3
037400100402     D  hWTSPEF              151    158  0
037500100402     D  hWTCLEF              159    166  0
037600100402     D  hWTKGEF              167    177  1
037700100402     D  hWTMCEF              178    188  3
037800100402     D  hWTSPEA              189    196  0
037900100402     D  hWTCLEA              197    204  0
038000100402     D  hWTKGEA              205    215  1
038100100402     D  hWTMCEA              216    226  3
038200100402     D  hWTSPED              227    234  0
038300100402     D  hWTCLED              235    242  0
038400100402     D  hWTKGED              243    253  1
038500100402     D  hWTMCED              254    264  3
038600100402     D  hWTSPPA              265    272  0
038700100402     D  hWTCLPA              273    280  0
038800100402     D  hWTKGPA              281    291  1
038900100402     D  hWTMCPA              292    302  3
039000100402     D  hWTSPEX              303    310  0
039100100402     D  hWTCLEX              311    318  0
039200100402     D  hWTKGEX              319    329  1
039300100402     D  hWTMCEX              330    340  3
039400100402     D  hWTSPIP_R                          like(WTSPIP)
039500100402     D  hWTSPIA_r                          like(wtspia)
039600100402     D  hWTSPEI_r                          like(wtspei)
039700100402     D  hWTSPEE_r                          like(wtspee)
039800100402     D  hWTCLIP_r                          like(wtclip)
039900100402     D  hWTCLIA_r                          like(wtclia)
040000100402     D  hWTCLEI_r                          like(wtclei)
040100100402     D  hWTCLEE_r                          like(wtclee)
040200100402     D  hWTKGIP_r                          like(wtkgip)
040300100402     D  hWTKGIA_r                          like(wtkgia)
040400100402     D  hWTKGEI_r                          like(wtkgei)
040500100402     D  hWTKGEE_r                          like(wtkgee)
040600100402     D  hWTMCIP_r                          like(wtmcip)
040700100402     D  hWTMCIA_r                          like(wtmcia)
040800100402     D  hWTMCEI_r                          like(wtmcei)
040900100402     D  hWTMCEE_r                          like(wtmcee)
041000100402     D  hWTSPEF_r                          like(wtspef)
041100100402     D  hWTCLEF_r                          like(wtclef)
041200100402     D  hWTKGEF_r                          like(wtkgef)
041300100402     D  hWTMCEF_r                          like(wtmcef)
041400100402     D  hWTSPEA_r                          like(wtspea)
041500100402     D  hWTCLEA_r                          like(wtclea)
041600100402     D  hWTKGEA_r                          like(wtkgea)
041700100402     D  hWTMCEA_r                          like(wtmcea)
041800100402     D  hWTSPED_r                          like(wtsped)
041900100402     D  hWTCLED_r                          like(wtcled)
042000100402     D  hWTKGED_r                          like(wtkged)
042100100402     D  hWTMCED_r                          like(wtmced)
042200100402     D  hWTSPPA_r                          like(wtsppa)
042300100402     D  hWTCLPA_r                          like(wtclpa)
042400100402     D  hWTKGPA_r                          like(wtkgpa)
042500100402     D  hWTMCPA_r                          like(wtmcpa)
042600100402     D  hWTSPEX_r                          like(wtspex)
042700100402     D  hWTCLEX_r                          like(wtclex)
042800100402     D  hWTKGEX_r                          like(wtkgex)
042900100402     D  hWTMCEX_r                          like(wtmcex)
043000100402     d* Giornaliero extra budget
043100100402     D  hWTSPIP_E                          like(WTSPIP)
043200100402     D  hWTSPIA_e                          like(wtspia)
043300100402     D  hWTSPEI_e                          like(wtspei)
043400100402     D  hWTSPEE_e                          like(wtspee)
043500100402     D  hWTCLIP_e                          like(wtclip)
043600100402     D  hWTCLIA_e                          like(wtclia)
043700100402     D  hWTCLEI_e                          like(wtclei)
043800100402     D  hWTCLEE_e                          like(wtclee)
043900100402     D  hWTKGIP_e                          like(wtkgip)
044000100402     D  hWTKGIA_e                          like(wtkgia)
044100100402     D  hWTKGEI_e                          like(wtkgei)
044200100402     D  hWTKGEE_e                          like(wtkgee)
044300100402     D  hWTMCIP_e                          like(wtmcip)
044400100402     D  hWTMCIA_e                          like(wtmcia)
044500100402     D  hWTMCEI_e                          like(wtmcei)
044600100402     D  hWTMCEE_e                          like(wtmcee)
044700100402     D  hWTSPEF_e                          like(wtspef)
044800100402     D  hWTCLEF_e                          like(wtclef)
044900100402     D  hWTKGEF_e                          like(wtkgef)
045000100402     D  hWTMCEF_e                          like(wtmcef)
045100100402     D  hWTSPEA_e                          like(wtspea)
045200100402     D  hWTCLEA_e                          like(wtclea)
045300100402     D  hWTKGEA_e                          like(wtkgea)
045400100402     D  hWTMCEA_e                          like(wtmcea)
045500100402     D  hWTSPED_e                          like(wtsped)
045600100402     D  hWTCLED_e                          like(wtcled)
045700100402     D  hWTKGED_e                          like(wtkged)
045800100402     D  hWTMCED_e                          like(wtmced)
045900100402     D  hWTSPPA_e                          like(wtsppa)
046000100402     D  hWTCLPA_e                          like(wtclpa)
046100100402     D  hWTKGPA_e                          like(wtkgpa)
046200100402     D  hWTMCPA_e                          like(wtmcpa)
046300100402     D  hWTSPEX_e                          like(wtspex)
046400100402     D  hWTCLEX_e                          like(wtclex)
046500100402     D  hWTKGEX_e                          like(wtkgex)
046600100402     D  hWTMCEX_e                          like(wtmcex)
046700100402     d* Riepilogativo extra budget
046800100402     D  hWTSPIP_re                         like(WTSPIP)
046900100402     D  hWTSPIA_re                         like(wtspia)
047000100402     D  hWTSPEI_re                         like(wtspei)
047100100402     D  hWTSPEE_re                         like(wtspee)
047200100402     D  hWTCLIP_re                         like(wtclip)
047300100402     D  hWTCLIA_re                         like(wtclia)
047400100402     D  hWTCLEI_re                         like(wtclei)
047500100402     D  hWTCLEE_re                         like(wtclee)
047600100402     D  hWTKGIP_re                         like(wtkgip)
047700100402     D  hWTKGIA_re                         like(wtkgia)
047800100402     D  hWTKGEI_re                         like(wtkgei)
047900100402     D  hWTKGEE_re                         like(wtkgee)
048000100402     D  hWTMCIP_re                         like(wtmcip)
048100100402     D  hWTMCIA_re                         like(wtmcia)
048200100402     D  hWTMCEI_re                         like(wtmcei)
048300100402     D  hWTMCEE_re                         like(wtmcee)
048400100402     D  hWTSPEF_re                         like(wtspef)
048500100402     D  hWTCLEF_re                         like(wtclef)
048600100402     D  hWTKGEF_re                         like(wtkgef)
048700100402     D  hWTMCEF_re                         like(wtmcef)
048800100402     D  hWTSPEA_re                         like(wtspea)
048900100402     D  hWTCLEA_re                         like(wtclea)
049000100402     D  hWTKGEA_re                         like(wtkgea)
049100100402     D  hWTMCEA_re                         like(wtmcea)
049200100402     D  hWTSPED_re                         like(wtsped)
049300100402     D  hWTCLED_re                         like(wtcled)
049400100402     D  hWTKGED_re                         like(wtkged)
049500100402     D  hWTMCED_re                         like(wtmced)
049600100402     D  hWTSPPA_re                         like(wtsppa)
049700100402     D  hWTCLPA_re                         like(wtclpa)
049800100402     D  hWTKGPA_re                         like(wtkgpa)
049900100402     D  hWTMCPA_re                         like(wtmcpa)
050000100402     D  hWTSPEX_re                         like(wtspex)
050100100402     D  hWTCLEX_re                         like(wtclex)
050200100402     D  hWTKGEX_re                         like(wtkgex)
050300100402     D  hWTMCEX_re                         like(wtmcex)
050400100402     d
050500090206     d* dati giornaliero
050600091006     D DSFIL_smp       DS                  OCCURS(400)
050700090206     D  Wmptsp_g                           like(wmptsp)
050800090206     D  wmptcp_g                           like(wmptcp)
050900090206     D  wmptkp_g                     11  1
051000090206     D  wmptmp_g                     10  3
051100090710
051200090206     D  Wmptsa_g                           like(wmptsa)
051300090206     D  wmptca_g                           like(wmptca)
051400090206     D  wmptka_g                     11  1
051500090206     D  wmptma_g                     10  3
051600090710
051700090206     D  Wmptspt_g                          like(wmptspt)
051800090206     D  wmptcpt_g                          like(wmptcpt)
051900090206     D  wmptkpt_g                    11  1
052000090206     D  wmptmpt_g                    10  3
052100090710
052200090710     D  Wmptsptc_g                         like(wmptspt)
052300090710     D  wmptcptc_g                         like(wmptcpt)
052400090710     D  wmptkptc_g                   11  1
052500090710     D  wmptmptc_g                   10  3
052600090710
052700090206     D  Wmptsat_g                          like(wmptsat)
052800090206     D  wmptcat_g                          like(wmptcat)
052900090206     D  wmptkat_g                    11  1
053000090206     D  wmptmat_g                    10  3
053100090710     d
053200090710     D  Wmptsatc_g                         like(wmptsat)
053300090710     D  wmptcatc_g                         like(wmptcat)
053400090710     D  wmptkatc_g                   11  1
053500090710     D  wmptmatc_g                   10  3
053600090710
053700090206     D  Wmptst_g                           like(wmptst)
053800090206     D  wmptct_g                           like(wmptct)
053900090206     D  wmptkt_g                     11  1
054000090206     D  wmptmt_g                     10  3
054100090710
054200090206     d* dati riepilogo
054300090206     D  Wmptsp_r                           like(wmptsp)
054400090206     D  wmptcp_r                           like(wmptcp)
054500090206     D  wmptkp_r                     11  1
054600090206     D  wmptmp_r                     10  3
054700090710
054800090206     D  Wmptsa_r                           like(wmptsa)
054900090206     D  wmptca_r                           like(wmptca)
055000090206     D  wmptka_r                     11  1
055100090206     D  wmptma_r                     10  3
055200090710
055300090206     D  Wmptspt_r                          like(wmptspt)
055400090206     D  wmptcpt_r                          like(wmptcpt)
055500090206     D  wmptkpt_r                    11  1
055600090206     D  wmptmpt_r                    10  3
055700090710
055800090710     D  Wmptsptc_r                         like(wmptspt)
055900090710     D  wmptcptc_r                         like(wmptcpt)
056000090710     D  wmptkptc_r                   11  1
056100090710     D  wmptmptc_r                   10  3
056200090710
056300090206     D  Wmptsat_r                          like(wmptsat)
056400090206     D  wmptcat_r                          like(wmptcat)
056500090206     D  wmptkat_r                    11  1
056600090206     D  wmptmat_r                    10  3
056700090710
056800090710     D  Wmptsatc_r                         like(wmptsat)
056900090710     D  wmptcatc_r                         like(wmptcat)
057000090710     D  wmptkatc_r                   11  1
057100090710     D  wmptmatc_r                   10  3
057200090710
057300090206     D  Wmptst_r                           like(wmptst)
057400090206     D  wmptct_r                           like(wmptct)
057500090206     D  wmptkt_r                     11  1
057600090206     D  wmptmt_r                     10  3
057700090204
057800120903     D II              S              3  0
057900120903     D kk              S              3  0
058000121126     D EE              S              4  0
058100130408     D ZZ              S              4  0
058200091006     D Wdatael         S              8  0
058300091007     D w010a           S             10
058400130408     D w010b           S             10
058500110801     D PTIPOCLI        S             12
058600110801     D skicli          S            132
058700130408     D Wgiro           S              1  0
058800090204     d dataiso         s               d   datfmt(*iso)
058900090204     d waas            s                   like(tasaas)
059000090204     d wmgs            s                   Like(tasmgs)
059100090224     d w_peso          s                   Like(taspkf)
059200090204     d k_fil           s                   Like(wmpfil)
059300090204     d k_tip           s                   Like(wmptip)
059400090204     d k_dat           s                   Like(wmpdal)
059500090205     d k_trd           s                   Like(ar5trd) inz('TRS')
059600091006     d savccm          s                   Like(tasccm)
059700091006     d savctr          s                   Like(tasctr)
059800110715     d savkpjbu        s                   Like(kpjbu)
059900120903     d ks_tie          s                   Like(wsstie)
060000120903     d ks_tip          s                   Like(wsstip)
060100120903     d ks_lin          s                   Like(wsslin)
060200120903     d ks_sca          s                   Like(wsskgf)
060300111028     d
060400111028     d titas00ds     E DS                  extname(titas00f)
060500090601     d
060600111028     d wrkgetlista     s          25767    varying
060700120904     d*
060800120904      * ---------------------------------------------------------
060900120904     i*
061000120904     iwslss500
061100120904     I              wsstkg                      wsstkg_ol
061200120904     I              wsstmc                      wsstmc_ol
061300120904     I              wsstkge                     wsstkge_ol
061400120904     I              wsstmce                     wsstmce_ol
061500120904     I              wsstkgh                     wsstkgh_ol
061600120904     I              wsstmch                     wsstmch_ol
061700120904     i*
061800120904     iwslss550
061900120904     I              wsstkg                      wsstkg_ex
062000120904     I              wsstmc                      wsstmc_ex
062100120904     I              wsstkge                     wsstkge_ex
062200120904     I              wsstmce                     wsstmce_ex
062300120904     I              wsstkgh                     wsstkgh_ex
062400120904     I              wsstmch                     wsstmch_ex
062500120904      * ---------------------------------------------------------
062600970527     C* RICEVIMENTO PARAMETRI
062700970527     C     *ENTRY        PLIST
062800970527     C                   PARM                    KPJBA
062900970527     C                   MOVEL     §DAL          WAAS
063000970527     C                   MOVE      §DAL          WMGS
063100090601     C                   MOVE      §tipo         Wtipo             1
063200970528     C*
063300970529     C                   EXSR      CARINZ
063400130408     c
063500130408     c                   eval      WGiro=1
063600130408     c                   exsr      Elabora
063700130408     c
063800130408     c* per i clienti particolari eseguo nuovo  SQL  su tasksc
063900130408     c                   if        §stat= 'P'
064000130408     c                   eval      WGiro=2
064100130408     c                   exsr      PrepaSQL2
064200130408     c                   exsr      Elabora
064300130408     c                   endif
064400130408     c
064500090206     c*
064600090206     C* SCARICO DS SU FILE
064700090206     c                   exsr      scrive_file
064800110801     c                   if        §stat=' '
064900110801     c                   eval      ptipocli='EXTRA BUDGET'
065000110801     c                   else
065100110801     c                   eval      ptipocli='PARTICOLARI '
065200110801     c                   endif
065300110801     c
065400090206     c                   except    testa
065500110801     C*
065600110801     c                   z-add     1             k
065700110801     c                   clear                   conta             3 0
065800110801     c                   clear                   skicli
065900110801     c                   dow       ksc_ex(k)<>*blanks
066000110801     c
066100110801     c                   if        §stat=' '
066200110801
066300110801     c                   eval      skicli=%trim(skicli)+' '+
066400110801     c                             %subst(ksc_ex(k):1:7)
066500110801     c                   add       7             conta
066600110801     c*
066700110801     c                   if        %subst(ksc_ex(k):8:3)<>'   '
066800110801     c                   eval      skicli=%trim(skicli)+'/'+
066900110801     c                             %subst(ksc_ex(k):8:3)
067000110801     c                   add       4             conta
067100110801     c                   endif
067200110801     c                   add       1             conta
067300110801     c
067400110801     c
067500110801     c                   if        conta>120
067600110801     c                   except    detksc
067700110801     c                   clear                   conta
067800110801     c                   clear                   skicli
067900110801     c                   endif
068000110801     c
068100110801     c                   else
068200110801     c
068300110801     c                   eval      skicli=%trim(skicli)+' '+
068400110801     c                             %subst(ksc_ex(k):1:7)
068500110801     c                   add       1             conta
068600110801     c
068700110801     c                   if        conta=16
068800110801     c                   except    detksc
068900110801     c                   clear                   conta
069000110801     c                   clear                   skicli
069100110801     c                   endif
069200110801     c
069300110801     c                   endif
069400110801     c
069500110801     c                   add       1             k
069600110801     c                   enddo
069700110801     c
069800110801     c                   if        conta>0
069900110801     c                   except    detksc
070000110801     c                   endif
070100970528     C*
070200970528     C                   SETON                                        LR
070300970519     C*
070400130408     C**********************************************************
070500130408     c     Elabora       BEGSR
070600130408     C**********************************************************
070700130408     C*
070800130408     c                   clear                   savccm
070900130408     C* LETTURA TITAS
071000130408     c                   if        §stat=' '
071100130408     C     KTAS          SETLL     TITAS38C
071200130408     c                   else
071300130408
071400130408     C/EXEC SQL
071500130408     C+ PREPARE s1 FROM :wrkgetlista
071600130408     C/END-EXEC
071700130408     C
071800130408     C/EXEC SQL
071900130408     C+ DECLARE A1 CURSOR FOR S1
072000130408     C/END-EXEC
072100130408     C
072200130408     C/EXEC SQL
072300130408     C+ OPEN a1
072400130408     C/END-EXEC
072500130408
072600130408     c                   endif
072700130408     c
072800130408     C                   DO        *HIVAL
072900130408     c
073000130408     c                   if        §stat=' '
073100130408     C                   READ      TITAS38C
073200130408     c* Termina il ciclo se fine file o per data spedizione > data al
073300130408     C                   if        %eof(titas38c) or ((tasaas*10000+tasmgs)>§al)
073400130408     c                   leave
073500130408     c                   endif
073600130408     c
073700130408     c                   else
073800130408     C/EXEC SQL
073900130408     C+ FETCH NEXT FROM a1 INTO :titas00ds
074000130408     C/END-EXEC
074100130408
074200130408     c                   select
074300130408
074400130408     c                   when      sqlcod = 100
074500130408     C/EXEC SQL
074600130408     C+ CLOSE a1
074700130408     C/END-EXEC
074800130408     c                   leave
074900130408     c                   when      sqlcod < 0
075000130408     c                   seton                                        H1
075100130408     C/EXEC SQL
075200130408     C+ CLOSE a1
075300130408     C/END-EXEC
075400130408     c                   leave
075500130408     c                   endsl
075600130408     c
075700130408     c                   endif
075800130408     c
075900130408     c* Scarto in base al tipo bolla
076000130408     c                   if        %lookup(tastbl:tbo)=0
076100130408     c                   iter
076200130408     c                   endif
076300130408     C*
076400130408     c
076500130408     c* EDPES : 01/06/09
076600130408     c* Per ora riutilizzo solo il peso bollettato/variato e non VDL
076700130408     c***                z-add     taspkf        w_peso
076800130408     c* EDPES:  01/09/09
076900130408     c**  riutilizzo il peso VDL
077000130408     c* imposto il peso che sarà da memorizzare secondo il seguente ordine:
077100130408     c* 1. VDL totale
077200130408     c* 2. VDL parziale se > di TASPKF
077300130408     c* 3. TASPKF
077400130408     c                   clear                   w_peso
077500170324     c                   if        (tasncp=tasncl and taspkc>0)
077600170324     c                                           or (tasncp>0 and taspkc>taspkf)
077700130408     c                   z-add     taspkc        w_peso
077800130408     c                   else
077900130408     c                   z-add     taspkf        w_peso
078000130408     c                   endif
078100130408     c
078200130408     c                   if        §stat=' '
078300130408     c                   exsr      sr_wfsmp
078400130408     c                   endif
078500130408     c
078600130408     c                   exsr      sr_wslss
078700130408     C                   ENDDO
078800130408     c                   ENDSR
078900090204     C**********************************************************
079000090204     C** Elaborazione per file wfsmp00f
079100090204     C**********************************************************
079200090204     C     sr_wfsmp      BEGSR
079300090204     c* linea di partenza
079400090206     c                   z-add     1             k
079500090206     c     taslnp        lookup    fil(k)                                 58
079600090206     c                   if        *in58
079700090206     c     k             occur     dsfil_smp
079800090205     c* . . . giornaliero
079900090206     c                   if        (tasaas*10000+tasmgs)=§al
080000090206     C                   add       1             Wmptsp_g
080100090206     C                   add       TASNCL        wmptcp_g
080200090224     C                   add       w_peso        Wmptkp_g
080300090206     C                   add       TASVLF        Wmptmp_g
080400090206     c                   endif
080500090204     c* . . . riepilogo
080600090206     C                   add       1             Wmptsp_r
080700090206     C                   add       TASNCL        wmptcp_r
080800090224     C                   add       w_peso        Wmptkp_r
080900090206     C                   add       TASVLF        Wmptmp_r
081000090206     c                   endif
081100090204     c* linea di arrivo
081200090206     c                   z-add     1             k
081300090206     c     taslna        lookup    fil(k)                                 58
081400090206     c                   if        *in58
081500090206     c     k             occur     dsfil_smp
081600090205     c* . . . giornaliero
081700090206     c                   if        (tasaas*10000+tasmgs)=§al
081800090206     C                   add       1             Wmptsa_g
081900090206     C                   add       TASNCL        wmptca_g
082000090224     C                   add       w_peso        Wmptka_g
082100090206     C                   add       TASVLF        Wmptma_g
082200090206     c                   endif
082300090205     c* . . . riepilogo
082400090206     C                   add       1             Wmptsa_r
082500090206     C                   add       TASNCL        wmptca_r
082600090224     C                   add       w_peso        Wmptka_r
082700090206     C                   add       TASVLF        Wmptma_r
082800090206     c                   endif
082900090710     c* terminal di partenza : esclusa la linea/terminal
083000090710     c                   if        tastfp<>taslnp
083100090206     c                   z-add     1             k
083200090206     c     tastfp        lookup    fil(k)                                 58
083300090206     c                   if        *in58
083400090206     c     k             occur     dsfil_smp
083500090205     c* . . . giornaliero
083600090206     c                   if        (tasaas*10000+tasmgs)=§al
083700090206     C                   add       1             Wmptspt_g
083800090206     C                   add       TASNCL        wmptcpt_g
083900090224     C                   add       w_peso        Wmptkpt_g
084000090206     C                   add       TASVLF        Wmptmpt_g
084100090206     c                   endif
084200090205     c* . . . riepilogo
084300090206     C                   add       1             Wmptspt_r
084400090206     C                   add       TASNCL        wmptcpt_r
084500090224     C                   add       w_peso        Wmptkpt_r
084600090206     C                   add       TASVLF        Wmptmpt_r
084700090206     c                   endif
084800090710     c                   endif
084900090710     c* terminal di partenza : tutto compresa la linea stessa
085000090710     c                   z-add     1             k
085100090710     c     tastfp        lookup    fil(k)                                 58
085200090710     c                   if        *in58
085300090710     c     k             occur     dsfil_smp
085400090710     c* . . . giornaliero
085500090710     c                   if        (tasaas*10000+tasmgs)=§al
085600090710     C                   add       1             Wmptsptc_g
085700090710     C                   add       TASNCL        wmptcptc_g
085800090710     C                   add       w_peso        Wmptkptc_g
085900090710     C                   add       TASVLF        Wmptmptc_g
086000090710     c                   endif
086100090710     c* . . . riepilogo
086200090710     C                   add       1             Wmptsptc_r
086300090710     C                   add       TASNCL        wmptcptc_r
086400090710     C                   add       w_peso        Wmptkptc_r
086500090710     C                   add       TASVLF        Wmptmptc_r
086600090710     c                   endif
086700090710     c
086800090710     c* terminal di arrivo : esclusa la linea/terminal
086900090710     c                   if        tastfa<>taslna
087000090206     c                   z-add     1             k
087100090206     c     tastfa        lookup    fil(k)                                 58
087200090206     c                   if        *in58
087300090206     c     k             occur     dsfil_smp
087400090205     c* . . . giornaliero
087500090206     c                   if        (tasaas*10000+tasmgs)=§al
087600090206     C                   add       1             Wmptsat_g
087700090206     C                   add       TASNCL        wmptcat_g
087800090224     C                   add       w_peso        Wmptkat_g
087900090206     C                   add       TASVLF        Wmptmat_g
088000090206     c                   endif
088100090205     c* . . . riepilogo
088200090206     C                   add       1             Wmptsat_r
088300090206     C                   add       TASNCL        wmptcat_r
088400090224     C                   add       w_peso        Wmptkat_r
088500090206     C                   add       TASVLF        Wmptmat_r
088600090205     c                   endif
088700090710     c                   endif
088800090710     c* terminal di arrivo : tutto compreso il terminal stesso
088900090710     c                   z-add     1             k
089000090710     c     tastfa        lookup    fil(k)                                 58
089100090710     c                   if        *in58
089200090710     c     k             occur     dsfil_smp
089300090710     c* . . . giornaliero
089400090710     c                   if        (tasaas*10000+tasmgs)=§al
089500090710     C                   add       1             Wmptsatc_g
089600090710     C                   add       TASNCL        wmptcatc_g
089700090710     C                   add       w_peso        Wmptkatc_g
089800090710     C                   add       TASVLF        Wmptmatc_g
089900090710     c                   endif
090000090710     c* . . . riepilogo
090100090710     C                   add       1             Wmptsatc_r
090200090710     C                   add       TASNCL        wmptcatc_r
090300090710     C                   add       w_peso        Wmptkatc_r
090400090710     C                   add       TASVLF        Wmptmatc_r
090500090710     c                   endif
090600090710     c
090700090204     c* filiali di transito
090800090209     c     kar5          chain     fiar531c
090900090205     c                   if        %found(fiar531c)
091000090205     c                   movel     ar5uni        dar5trs
091100090205     c*
091200090205     c                   eval      k_fil=§AR5FLP
091300090205     c                   exsr      agg_transiti
091400090205      *
091500090205     c                   if        §ar5fl2>0
091600090205     c                   eval      k_fil=§AR5FL2
091700090205     c                   exsr      agg_transiti
091800090205     c                   endif
091900090205      *
092000090205     c                   if        §ar5fl3>0
092100090205     c                   eval      k_fil=§AR5FL3
092200090205     c                   exsr      agg_transiti
092300090205     c                   endif
092400090205      *
092500090205     c                   if        §ar5fl4>0
092600090205     c                   eval      k_fil=§AR5FL4
092700090205     c                   exsr      agg_transiti
092800090205     c                   endif
092900090205      *
093000090205     c                   if        §ar5fl5>0
093100090205     c                   eval      k_fil=§AR5FL5
093200090205     c                   exsr      agg_transiti
093300090205     c                   endif
093400090205     c                   endif
093500090204     c
093600090204     C                   endsr
093700090205     C**********************************************************
093800090205     C** Aggiornamento/scrittura giornaliero e riepil. x transiti
093900090205     C**********************************************************
094000090205     C     agg_transiti  BEGSR
094100090206     c                   z-add     1             k
094200090206     c     k_fil         lookup    fil(k)                                 58
094300090206     c                   if        *in58
094400090206     c     k             occur     dsfil_smp
094500090205     c* . . . giornaliero
094600090206     c                   if        (tasaas*10000+tasmgs)=§al
094700090206     C                   add       1             Wmptst_g
094800090206     C                   add       TASNCL        wmptct_g
094900090224     C                   add       w_peso        Wmptkt_g
095000090206     C                   add       TASVLF        Wmptmt_g
095100090206     c                   endif
095200090205     c* . . . riepilogo
095300090206     C                   add       1             Wmptst_r
095400090206     C                   add       TASNCL        wmptct_r
095500090224     C                   add       w_peso        Wmptkt_r
095600090206     C                   add       TASVLF        Wmptmt_r
095700090205     c                   endif
095800090205     c                   endsr
095900090204     C**********************************************************
096000090206     C** Riempie campi wfsmp00f
096100090204     C**********************************************************
096200090206     C     rie_campi     BEGSR
096300090204     c* profilo utente e data elaborazione
096400090204     c                   eval      wmpusr=knmus
096500090204     c                   eval      wmpdel=wdatael
096600090206     c*
096700090206     c                   eval      wmpall=§al
096800090204     c* area e descrizione area
096900090204     c                   eval      wmpcar=car(k)
097000090204     C                   MOVEL(P)  '05'          tblcod
097100090204     C                   MOVEL(P)  car(K)        tblkey
097200090204     C     KTAB          CHAIN     TABEL00F                           30
097300090204     C                   IF        %found(tabel00f) and tblflg=*blanks
097400090204     C                   MOVEL     TBLUNI        DS05
097500090204     C                   MOVEL     §05DES        wmpdar
097600090204     C                   endif
097700090204     c* filiale,descrizione e livello filiale
097800090204     c                   eval      wmpdil=dil(K)
097900090206     c                   eval      wmpfil=fil(K)
098000090206     c                   eval      wmplif=lpo(K)
098100090204     c* tipo filiale Italia/Estero
098200090205     c                   eval      wmptif=tif(K)
098300090206     c* spedizioni,colli, kg,volume
098400090206     c                   if        wmptip='G'
098500090206     c                   z-add     wmptsp_G      wmptsp
098600090206     c                   z-add     wmptcp_G      wmptcp
098700090206     c                   z-add(h)  wmptkp_G      wmptkp
098800090206     c                   z-add(h)  wmptmp_G      wmptmp
098900090710
099000090206     c                   z-add     wmptsa_G      wmptsa
099100090206     c                   z-add     wmptca_G      wmptca
099200090206     c                   z-add(h)  wmptka_G      wmptka
099300090206     c                   z-add(h)  wmptma_G      wmptma
099400090710
099500090206     c                   z-add     wmptspt_G     wmptspt
099600090206     c                   z-add     wmptcpt_G     wmptcpt
099700090206     c                   z-add(h)  wmptkpt_G     wmptkpt
099800090206     c                   z-add(h)  wmptmpt_G     wmptmpt
099900090710
100000090710     c                   z-add     wmptsptc_G    wmptsptc
100100090710     c                   z-add     wmptcptc_G    wmptcptc
100200090710     c                   z-add(h)  wmptkptc_G    wmptkptc
100300090710     c                   z-add(h)  wmptmptc_G    wmptmptc
100400090710
100500090206     c                   z-add     wmptsat_G     wmptsat
100600090206     c                   z-add     wmptcat_G     wmptcat
100700090206     c                   z-add(h)  wmptkat_G     wmptkat
100800090206     c                   z-add(h)  wmptmat_G     wmptmat
100900090710
101000090710     c                   z-add     wmptsatc_G    wmptsatc
101100090710     c                   z-add     wmptcatc_G    wmptcatc
101200090710     c                   z-add(h)  wmptkatc_G    wmptkatc
101300090710     c                   z-add(h)  wmptmatc_G    wmptmatc
101400090710
101500090206     c                   z-add     wmptst_G      wmptst
101600090206     c                   z-add     wmptct_G      wmptct
101700090206     c                   z-add(h)  wmptkt_G      wmptkt
101800090206     c                   z-add(h)  wmptmt_G      wmptmt
101900090206     c                   else
102000090206     c                   z-add     wmptsp_r      wmptsp
102100090206     c                   z-add     wmptcp_r      wmptcp
102200090206     c                   z-add(h)  wmptkp_r      wmptkp
102300090206     c                   z-add(h)  wmptmp_r      wmptmp
102400090710
102500090206     c                   z-add     wmptsa_r      wmptsa
102600090206     c                   z-add     wmptca_r      wmptca
102700090206     c                   z-add(h)  wmptka_r      wmptka
102800090206     c                   z-add(h)  wmptma_r      wmptma
102900090710
103000090206     c                   z-add     wmptspt_r     wmptspt
103100090206     c                   z-add     wmptcpt_r     wmptcpt
103200090206     c                   z-add(h)  wmptkpt_r     wmptkpt
103300090206     c                   z-add(h)  wmptmpt_r     wmptmpt
103400090710
103500090710     c                   z-add     wmptsptc_r    wmptsptc
103600090710     c                   z-add     wmptcptc_r    wmptcptc
103700090710     c                   z-add(h)  wmptkptc_r    wmptkptc
103800090710     c                   z-add(h)  wmptmptc_r    wmptmptc
103900090710
104000090206     c                   z-add     wmptsat_r     wmptsat
104100090206     c                   z-add     wmptcat_r     wmptcat
104200090206     c                   z-add(h)  wmptkat_r     wmptkat
104300090206     c                   z-add(h)  wmptmat_r     wmptmat
104400090710
104500090710     c                   z-add     wmptsatc_r    wmptsatc
104600090710     c                   z-add     wmptcatc_r    wmptcatc
104700090710     c                   z-add(h)  wmptkatc_r    wmptkatc
104800090710     c                   z-add(h)  wmptmatc_r    wmptmatc
104900090710
105000090206     c                   z-add     wmptst_r      wmptst
105100090206     c                   z-add     wmptct_r      wmptct
105200090206     c                   z-add(h)  wmptkt_r      wmptkt
105300090206     c                   z-add(h)  wmptmt_r      wmptmt
105400090710
105500090206     c                   endif
105600090204     c                   endsr
105700090204     C**********************************************************
105800120905     C** Elaborazione per file WFLSS60f
105900090204     C**********************************************************
106000090204     C     sr_wslss      BEGSR
106100090204     C                   SETOFF                                       545556
106200090204     C                   SETOFF                                       48  52
106300111129     c
106400121123     c
106500111129     c                   z-add     1             kk
106600111129     c     taslnp        lookup    fil(kk)                                98
106700111129     c
106800111129     c                   if        *in98
106900111129     c* Euroexpress o Fedex o DPD
107000111129     c                   if        tif(kk) ='F' or tif(kk)='X' or tif(kk)='D'
107100111129     c                   seton                                        54
107200111129     c                   endif
107300090204     C*
107400111129     c                   endif
107500111129     c***
107600121123     c* L I N E A   D I   A R R I V O
107700111129     c***
107800111129     c                   z-add     1             kk
107900111129     c     taslna        lookup    fil(kk)                                98
108000111129     c
108100111129     c                   if        *in98
108200111129     c* Euroexpress o Fedex o DPD
108300111129     c                   if        tif(kk) ='F' or tif(kk)='X' or tif(kk)='D'
108400111129     c                   seton                                        55
108500111129     c                   endif
108600111129     C*
108700090204     C*
108800111129     c* Fedex
108900111129     c                   if        tif(kk) ='F'
109000111129     c                   seton                                        52
109100111129     c                   endif
109200111129     c
109300111129     C***  TASLNA        COMP      400                                    52     FEDEX
109400090204     C*
109500111129     c* DPD
109600111129     c                   if        tif(kk) ='D'
109700111129     c                   seton                                        56
109800111129     c                   endif
109900091006     c*
110000111129     c                   endif
110100121123     c
110200111129     c
110300091006     c* Verifico se si tratta di cliente extra budget --> 35 on
110400121121    1c                   if        §stat=' '
110500121121    2c                   if        tasccm<>savccm or tasctr<>savctr
110600121123     c                   z-add     1             EE
110700091007     c                   eval      w010a=%editc(tasccm:'X')+%editc(tasctr:'X')
110800091007     c     w010a         lookup    ksc_EX(EE)                             35
110900121121    3c                   if        not *in35
111000091007     c                   eval      %subst(w010a:8:3)='   '
111100091007     c     w010a         lookup    ksc_EX(EE)                             35
111200121121    3c                   endif
111300091006
111400091006     c                   eval      savccm=tasccm
111500091006     c                   eval      savctr=tasctr
111600121121    2c                   endif
111700110711     c
111800121121   x1c                   else
111900121121     c
112000121121     c* Per i clienti Particolari devo controllare il terminal della bolla se è quello
112100121121     c*   previsto
112200130408   2ac                   if        Wgiro=1
112300121121    2c                   if        tasccm<>savccm
112400121123     c                   z-add     1             EE
112500121121     c                   eval      w010a=%editc(tasccm:'X')+'   '
112600121121     c     w010a         lookup    ksc_EX(EE)                             35
112700121121     c
112800121121     c* E' impossibile che non lo trovi, ma se per caso succede, scarto la bolla
112900121121    3c                   if        *in35
113000121121     c                   eval      savccm=tasccm
113100121121   x3c                   else
113200121121     c                   clear                   savccm
113300121121    3c                   endif
113400121121    2c                   endif
113500130408   2ac                   endif
113600130408     c
113700130408     c* controllo il TASKSC per gli assegnati
113800130408   2ac                   if        Wgiro=2
113900130408     c                   z-add     1             EE
114000130408     c                   eval      w010a=%editc(tasksc:'X')+'   '
114100130408     c     w010a         lookup    ksc_EX(EE)                             35
114200130408     c
114300130408     c* E' impossibile che non lo trovi, ma se per caso succede, scarto la bolla
114400130408    3c                   if        *in35
114500130408     c
114600130408     c                   eval      savccm=tasksc
114700130408     c* Elaboro solo se il TASCCM non è compreso (altrimenti già elaborato nel primo giro)
114800130408     c                   z-add     1             ZZ
114900130408     c                   setoff                                       36
115000130408     c                   eval      w010b=%editc(tasccm:'X')+'   '
115100130408     c     w010b         lookup    ksc_EX(ZZ)                             36
115200130408     c                   if        *in36
115300130408     c                   setoff                                       3536
115400130408     c                   clear                   savccm
115500130408     c                   endif
115600130408     c
115700130408   x3c                   else
115800130408     c                   clear                   savccm
115900130408    3c                   endif
116000130408   2ac                   endif
116100121121     c
116200121121     c                   setoff                                       35
116300121123     c*
116400121123     c                   clear                   prtc_lnp          3 0
116500121123     c                   clear                   prtc_lna          3 0
116600121121     c
116700121121    2c                   if        savccm>0
116800121123     c                   select
116900130819     c*  cliente momentaneamente bloccato da non elaborare
117000130819    3c                   when      ksc_are(EE)=888
117100121123     c* abilitato a tutto:  memorizzo per qualsiasi terminal
117200130819    3c                   when      ksc_are(EE)=999
117300121121     c                   seton                                        35
117400130819     c**                 eval      prtc_lnp=tastfp
117500130819     c**                 eval      prtc_lna=tastfa
117600130819     c* memorizzo nel terminal di riferimento e non nel tfp/tfa della bolla a causa
117700130819     c*  delle bolle sia della 001 che della 275 che devono essere memrozzare insieme
117800130819     c                   eval      prtc_lnp=ksc_tr(EE)
117900130819     c                   eval      prtc_lna=ksc_tr(EE)
118000121123   x3c                   other
118100121123     c
118200130819     c* Abilito solo il terminal di partenza o di arrivo previsto dalla tabella in base all'area
118300130819    4c****               if        tastfp=ksc_tr(EE)
118400130819     c                   z-add     1             yy                4 0
118500130819     c     tastfp        lookup    fil(yy)                                58
118600130819    4c                   if        car(yy)=ksc_are(EE)
118700121121     c                   seton                                        35
118800130819     c**                 eval      prtc_lnp=tastfp
118900130819     c                   eval      prtc_lnp=ksc_tr(EE)
119000121121    4c                   endif
119100121123    c
119200130819    4c****               if        tastfa=ksc_tr(EE)
119300130819     c                   z-add     1             yy
119400130819     c     tastfa        lookup    fil(yy)                                58
119500130819    4c                   if        car(yy)=ksc_are(EE)
119600130819     c                   eval      prtc_lna=ksc_tr(EE)
119700121123     c                   seton                                        35
119800121123    4c                   endif
119900121123     c
120000121123    3c                   endsl
120100121121     c*
120200121121    2c                   endif
120300110711     c
120400121121    1c                   endif
120500121121     c
120600121121     c* Per i clienti Particolari --> 35 off bolla non da elaborare esco dal ciclo
120700121121     c*   visto che l'elaborazione è specifica solo per questi clienti
120800121121     c                   if        §stat='P' and not *in35
120900121121     c                   leavesr
121000121121     c                   endif
121100090204     C*
121200121123    1c                   if        §stat=' '
121300121123     c                   eval      prtc_lnp=taslnp
121400090204     C                   Z-ADD     1             K
121500090204     C     TASLNP        LOOKUP    FIL(K)                                 58
121600121123     c                   else
121700121123     c
121800121123     c* solo si tratta di terminal di partenza elaborabile procedo
121900121123     c                   if        prtc_lnp>0
122000121123     C                   Z-ADD     1             K
122100121123     C     prtc_lnp      LOOKUP    FIL(K)                                 58
122200121123     c                   endif
122300121123     c                   endif
122400121123     c
122500121123    1c                   if        prtc_lnp>0
122600121123     c
122700090204     C  N58              EXCEPT    ERR
122800090204     C     K             OCCUR     DSFIL
122900100402     C     K             OCCUR     DSFIL_e
123000100402     C     K             OCCUR     DSFIL_h
123100121123     c
123200090204     C  N54
123300090204     CANN55              DO                                                     ITALIA
123400100402     c* Spedizioni LNP Italia LNA Italia
123500100402     c                   exsr      speITA
123600090204     C                   ENDDO
123700100402     c
123800100402     c* Spedizioni IMPORT --> LNP Italia
123900090204     C   54              DO                                                     IMPORT
124000100402     c                   exsr      speIMPORT
124100090204     C                   ENDDO
124200100402     c
124300100402     c* Spedizioni lna EUROEXPRESS
124400090204     C   55
124500090204     CANN56
124600090204     CANN52              DO                                                     EXPORT
124700100402     c                   exsr      speEXPRESS
124800090204     C                   ENDDO
124900100402     c
125000100402     c* Spedizioni lna DPD
125100090204     C   55
125200090204     CAN 56              DO                                                     EXPORT
125300100402     c                   exsr      speDPD
125400090204     C                   ENDDO
125500100402     c
125600100402     c* Spedizioni lna FEDEX
125700090204     C   55
125800090204     CAN 52              DO                                                     EXPORT
125900100402     c                   exsr      speFEDEX
126000090204     C                   ENDDO
126100121123     c
126200121123    1c                   endif
126300100402     c
126400090204     C* MEMORIZZAZIONE PER LINEA ARRIVO
126500121123    1c                   if        §stat=' '
126600121123     c                   eval      prtc_lna=taslna
126700090204     C                   Z-ADD     1             K
126800090204     C     TASLNA        LOOKUP    FIL(K)                                 58
126900121123     c                   else
127000121123     c
127100121123     c* solo si tratta di terminal di arrivo elaborabile procedo
127200121123     c                   if        prtc_lna>0
127300121123     C                   Z-ADD     1             K
127400121123     C     prtc_lna      LOOKUP    FIL(K)                                 58
127500121123     c                   endif
127600121123     c                   endif
127700121123     c
127800121123    1c                   if        prtc_lna>0
127900121123     c
128000090204     C  N58              EXCEPT    ERR
128100090204     C     K             OCCUR     DSFIL
128200100402     C     K             OCCUR     DSFIL_e
128300100402     C     K             OCCUR     DSFIL_h
128400100402     c
128500090204     C  N54
128600100402     CANN55              DO
128700100402     c* Spedizioni LNP Italia LNA Italia
128800100402     c                   exsr      speITA_LNA
128900090204     C                   ENDDO
129000100402     c
129100100402     c* spedizioni EXPORT --> lna estera
129200090204     C   55              DO                                                     ESTERO ASS.+FRA.
129300100402     c                   exsr      speEXPORT
129400090204     C                   ENDDO
129500121123     c
129600121123     c                   endif
129700121123     c
129800090204     c                   endsr
129900090206     C**********************************************************
130000090206     C** scarica ds multiple su file
130100090206     C**********************************************************
130200090206     C     scrive_file   BEGSR
130300090206     c*
130400090206     C                   Z-ADD     §AL           WSSALL
130500090206     c*
130600110711    1C                   DO        400           K
130700090206     C     K             OCCUR     DSFIL_smp
130800090206     C     K             OCCUR     DSFIL
130900100402     C     K             OCCUR     DSFIL_e
131000100402     C     K             OCCUR     DSFIL_h
131100110711     c
131200110711    2c                   if        §stat=' '
131300090206     c* WFSMP
131400090206     c* giornaliero
131500090601     c* Lo scrivo solo per tipo "G" o "R"
131600090601     c                   if        Wtipo='G' or Wtipo='R'
131700090206     c                   if        (wmptsp_g>0 or wmptsa_g>0 or
131800090206     c                              wmptspt_g>0 or wmptsat_g>0 or
131900090206     c                              wmptst_g>0)
132000090206     c                   z-add     §al           wmpdal
132100090206     c                   eval      wmptip='G'
132200090206     c                   exsr      rie_campi
132300090206     c                   write     wfsmp000
132400090206     c                   endif
132500090601     c                   endif
132600090601     c
132700090601     c* riepilogo /precedente
132800090601     c* Lo scrivo solo per tipo "R"  se "P" lo scrivo con "P"
132900090601     c                   if        Wtipo='P' or Wtipo='R'
133000090206     c                   if        (wmptsp_r>0 or wmptsa_r>0 or
133100090206     c                              wmptspt_r>0 or wmptsat_r>0 or
133200090206     c                              wmptst_r>0)
133300090206     c                   z-add     §dal          wmpdal
133400090601     c                   if        Wtipo='R'
133500090206     c                   eval      wmptip='R'
133600090601     c                   else
133700090601     c                   eval      wmptip='P'
133800090601     c                   endif
133900090206     c                   exsr      rie_campi
134000090206     c                   write     wfsmp000
134100090206     c                   endif
134200090601     c                   endif
134300110711     c
134400110711    2c                   endif
134500120905     c* WFLSS60F
134600120904     c                   clear                   wsskgf
134700090206     c* giornaliero
134800090601     c                   if        Wtipo='G' or Wtipo='R'
134900110711     c
135000110711    2c                   if        §stat=' '
135100090601     c
135200090206     C     WTSPIP        IFGT      *ZERO
135300090206     C                   Z-ADD     §al           WSSDAL
135400090206     c                   movel     'G'           WSSTIE
135500090206     C                   MOVE      'IP'          WSSTIP
135600090206     C                   Z-ADD     FIL(K)        WSSLIN
135700090206     C                   Z-ADD     WTSPIP        WSSTSP
135800090206     C                   Z-ADD     WTCLIP        WSSTCL
135900090206     C                   Z-ADD(H)  WTKGIP        WSSTKG
136000090206     C                   Z-ADD(H)  WTMCIP        WSSTMC
136100120904     c*old
136200120904     C                   Z-ADD(H)  WTKGIP        WSSTKG_ol
136300120904     C                   Z-ADD(H)  WTMCIP        WSSTMC_ol
136400100402     c* spedizioni "E"
136500100402     C                   Z-ADD     eWTSPIP       WSSTSPe
136600100402     C                   Z-ADD     eWTCLIP       WSSTCLe
136700100402     C                   Z-ADD(H)  eWTKGIP       WSSTKGe
136800100402     C                   Z-ADD(H)  eWTMCIP       WSSTMCe
136900120904     c* old
137000120904     C                   Z-ADD(H)  eWTKGIP       WSSTKGe_ol
137100120904     C                   Z-ADD(H)  eWTMCIP       WSSTMCe_ol
137200100402     c* spedizioni "H"
137300100402     C                   Z-ADD     hWTSPIP       WSSTSPh
137400100402     C                   Z-ADD     hWTCLIP       WSSTCLh
137500100402     C                   Z-ADD(H)  hWTKGIP       WSSTKGh
137600100402     C                   Z-ADD(H)  hWTMCIP       WSSTMCh
137700120904     c* old
137800120904     C                   Z-ADD(H)  hWTKGIP       WSSTKGh_ol
137900120904     C                   Z-ADD(H)  hWTMCIP       WSSTMCh_ol
138000100402
138100120905     C                   WRITE     WFLSS600
138200120904     C                   WRITE     WSLSS500
138300090206     C                   ENDIF
138400100402
138500090206     C     WTSPIA        IFGT      *ZERO
138600090206     C                   Z-ADD     §al           WSSDAL
138700090206     c                   movel     'G'           WSSTIE
138800090206     C                   MOVE      'IA'          WSSTIP
138900090206     C                   Z-ADD     FIL(K)        WSSLIN
139000090206     C                   Z-ADD     WTSPIA        WSSTSP
139100090206     C                   Z-ADD     WTCLIA        WSSTCL
139200090206     C                   Z-ADD(H)  WTKGIA        WSSTKG
139300090206     C                   Z-ADD(H)  WTMCIA        WSSTMC
139400120904     c* old
139500120904     C                   Z-ADD(H)  WTKGIA        WSSTKG_ol
139600120904     C                   Z-ADD(H)  WTMCIA        WSSTMC_ol
139700100402     c* spedizioni "E"
139800100402     C                   Z-ADD     eWTSPIA       WSSTSPe
139900100402     C                   Z-ADD     eWTCLIA       WSSTCLe
140000100402     C                   Z-ADD(H)  eWTKGIA       WSSTKGe
140100100402     C                   Z-ADD(H)  eWTMCIA       WSSTMCe
140200120904     c* old
140300120904     C                   Z-ADD(H)  eWTKGIA       WSSTKGe_ol
140400120904     C                   Z-ADD(H)  eWTMCIA       WSSTMCe_ol
140500100402     c* spedizioni "H"
140600100402     C                   Z-ADD     hWTSPIA       WSSTSPh
140700100402     C                   Z-ADD     hWTCLIA       WSSTCLh
140800100402     C                   Z-ADD(H)  hWTKGIA       WSSTKGh
140900100402     C                   Z-ADD(H)  hWTMCIA       WSSTMCh
141000120904     c* old
141100120904     C                   Z-ADD(H)  hWTKGIA       WSSTKGh_ol
141200120904     C                   Z-ADD(H)  hWTMCIA       WSSTMCh_ol
141300100402     c
141400120905     C                   WRITE     WFLSS600
141500120904     C                   WRITE     WSLSS500
141600090206     C                   ENDIF
141700100402     c
141800090206     C     WTSPEI        IFGT      *ZERO
141900090206     C                   Z-ADD     §al           WSSDAL
142000090206     c                   movel     'G'           WSSTIE
142100090206     C                   MOVE      'EI'          WSSTIP
142200090206     C                   Z-ADD     FIL(K)        WSSLIN
142300090206     C                   Z-ADD     WTSPEI        WSSTSP
142400090206     C                   Z-ADD     WTCLEI        WSSTCL
142500090206     C                   Z-ADD(H)  WTKGEI        WSSTKG
142600090206     C                   Z-ADD(H)  WTMCEI        WSSTMC
142700120904     c* old
142800120904     C                   Z-ADD(H)  WTKGEI        WSSTKG_ol
142900120904     C                   Z-ADD(H)  WTMCEI        WSSTMC_ol
143000100402     c* spedizioni "E"
143100100402     C                   Z-ADD     eWTSPEI       WSSTSPe
143200100402     C                   Z-ADD     eWTCLEI       WSSTCLe
143300100402     C                   Z-ADD(H)  eWTKGEI       WSSTKGe
143400100402     C                   Z-ADD(H)  eWTMCEI       WSSTMCe
143500120904     c* old
143600120904     C                   Z-ADD(H)  eWTKGEI       WSSTKGe_ol
143700120904     C                   Z-ADD(H)  eWTMCEI       WSSTMCe_ol
143800100402     c* spedizioni "H"
143900100402     C                   Z-ADD     hWTSPEI       WSSTSPh
144000100402     C                   Z-ADD     hWTCLEI       WSSTCLh
144100100402     C                   Z-ADD(H)  hWTKGEI       WSSTKGh
144200100402     C                   Z-ADD(H)  hWTMCEI       WSSTMCh
144300120904     c* old
144400120904     C                   Z-ADD(H)  hWTKGEI       WSSTKGh_ol
144500120904     C                   Z-ADD(H)  hWTMCEI       WSSTMCh_ol
144600100402     c
144700120905     C                   WRITE     WFLSS600
144800120904     C                   WRITE     WSLSS500
144900120904     C                   ENDIF
145000100402     c
145100090206     C     WTSPEE        IFGT      *ZERO
145200090206     C                   Z-ADD     §al           WSSDAL
145300090206     c                   movel     'G'           WSSTIE
145400090206     C                   MOVE      'EE'          WSSTIP
145500090206     C                   Z-ADD     FIL(K)        WSSLIN
145600090206     C                   Z-ADD     WTSPEE        WSSTSP
145700090206     C                   Z-ADD     WTCLEE        WSSTCL
145800090206     C                   Z-ADD(H)  WTKGEE        WSSTKG
145900090206     C                   Z-ADD(H)  WTMCEE        WSSTMC
146000120904     c* old
146100120904     C                   Z-ADD(H)  WTKGEE        WSSTKG_ol
146200120904     C                   Z-ADD(H)  WTMCEE        WSSTMC_ol
146300100402     c* spedizioni "E"
146400100402     C                   Z-ADD     eWTSPEE       WSSTSPe
146500100402     C                   Z-ADD     eWTCLEE       WSSTCLe
146600100402     C                   Z-ADD(H)  eWTKGEE       WSSTKGe
146700100402     C                   Z-ADD(H)  eWTMCEE       WSSTMCe
146800120904     c* old
146900120904     C                   Z-ADD(H)  eWTKGEE       WSSTKGe_ol
147000120904     C                   Z-ADD(H)  eWTMCEE       WSSTMCe_ol
147100100402     c* spedizioni "H"
147200100402     C                   Z-ADD     hWTSPEE       WSSTSPh
147300100402     C                   Z-ADD     hWTCLEE       WSSTCLh
147400100402     C                   Z-ADD(H)  hWTKGEE       WSSTKGh
147500100402     C                   Z-ADD(H)  hWTMCEE       WSSTMCh
147600120904     c* old
147700120904     C                   Z-ADD(H)  hWTKGEE       WSSTKGh_ol
147800120904     C                   Z-ADD(H)  hWTMCEE       WSSTMCh_ol
147900100402     c
148000120905     C                   WRITE     WFLSS600
148100120904     C                   WRITE     WSLSS500
148200090206     C                   ENDIF
148300090206     C*
148400090206     C     WTSPED        IFGT      *ZERO
148500090206     C                   Z-ADD     §al           WSSDAL
148600090206     c                   movel     'G'           WSSTIE
148700090206     C                   MOVE      'ED'          WSSTIP
148800090206     C                   Z-ADD     FIL(K)        WSSLIN
148900090206     C                   Z-ADD     WTSPED        WSSTSP
149000090206     C                   Z-ADD     WTCLED        WSSTCL
149100090206     C                   Z-ADD(H)  WTKGED        WSSTKG
149200090206     C                   Z-ADD(H)  WTMCED        WSSTMC
149300120904     c* old
149400120904     C                   Z-ADD(H)  WTKGED        WSSTKG_ol
149500120904     C                   Z-ADD(H)  WTMCED        WSSTMC_ol
149600100402     c* spedizioni "E"
149700100402     C                   Z-ADD     eWTSPED       WSSTSPe
149800100402     C                   Z-ADD     eWTCLED       WSSTCLe
149900100402     C                   Z-ADD(H)  eWTKGED       WSSTKGe
150000100402     C                   Z-ADD(H)  eWTMCED       WSSTMCe
150100120904     c* old
150200120904     C                   Z-ADD(H)  eWTKGED       WSSTKGe_ol
150300120904     C                   Z-ADD(H)  eWTMCED       WSSTMCe_ol
150400100402     c* spedizioni "H"
150500100402     C                   Z-ADD     hWTSPED       WSSTSPh
150600100402     C                   Z-ADD     hWTCLED       WSSTCLh
150700100402     C                   Z-ADD(H)  hWTKGED       WSSTKGh
150800100402     C                   Z-ADD(H)  hWTMCED       WSSTMCh
150900120904     c* old
151000120904     C                   Z-ADD(H)  hWTKGED       WSSTKGh_ol
151100120904     C                   Z-ADD(H)  hWTMCED       WSSTMCh_ol
151200100402     c
151300120905     C                   WRITE     WFLSS600
151400120904     C                   WRITE     WSLSS500
151500090206     C                   ENDIF
151600090206     C*
151700090206     C     WTSPEX        IFGT      *ZERO
151800090206     C                   Z-ADD     §al           WSSDAL
151900090206     c                   movel     'G'           WSSTIE
152000090206     C                   MOVE      'EX'          WSSTIP
152100090206     C                   Z-ADD     FIL(K)        WSSLIN
152200090206     C                   Z-ADD     WTSPEX        WSSTSP
152300090206     C                   Z-ADD     WTCLEX        WSSTCL
152400090206     C                   Z-ADD(H)  WTKGEX        WSSTKG
152500090206     C                   Z-ADD(H)  WTMCEX        WSSTMC
152600120904     c* old
152700120904     C                   Z-ADD(H)  WTKGEX        WSSTKG_ol
152800120904     C                   Z-ADD(H)  WTMCEX        WSSTMC_ol
152900100402     c* spedizioni "E"
153000100402     C                   Z-ADD     eWTSPEX       WSSTSPe
153100100402     C                   Z-ADD     eWTCLEX       WSSTCLe
153200100402     C                   Z-ADD(H)  eWTKGEX       WSSTKGe
153300100402     C                   Z-ADD(H)  eWTMCEX       WSSTMCe
153400120904     c* old
153500120904     C                   Z-ADD(H)  eWTKGEX       WSSTKGe_ol
153600120904     C                   Z-ADD(H)  eWTMCEX       WSSTMCe_ol
153700100402     c* spedizioni "H"
153800100402     C                   Z-ADD     hWTSPEX       WSSTSPh
153900100402     C                   Z-ADD     hWTCLEX       WSSTCLh
154000100402     C                   Z-ADD(H)  hWTKGEX       WSSTKGh
154100100402     C                   Z-ADD(H)  hWTMCEX       WSSTMCh
154200120904     c* old
154300120904     C                   Z-ADD(H)  hWTKGEX       WSSTKGh_ol
154400120904     C                   Z-ADD(H)  hWTMCEX       WSSTMCh_ol
154500100402     c
154600120905     C                   WRITE     WFLSS600
154700120904     C                   WRITE     WSLSS500
154800090206     C                   ENDIF
154900090206     C*
155000090206     C     WTSPEA        IFGT      *ZERO
155100090206     C                   Z-ADD     §al           WSSDAL
155200090206     c                   movel     'G'           WSSTIE
155300090206     C                   MOVE      'EA'          WSSTIP
155400090206     C                   Z-ADD     FIL(K)        WSSLIN
155500090206     C                   Z-ADD     WTSPEA        WSSTSP
155600090206     C                   Z-ADD     WTCLEA        WSSTCL
155700090206     C                   Z-ADD(H)  WTKGEA        WSSTKG
155800090206     C                   Z-ADD(H)  WTMCEA        WSSTMC
155900120904     c* old
156000120904     C                   Z-ADD(H)  WTKGEA        WSSTKG_ol
156100120904     C                   Z-ADD(H)  WTMCEA        WSSTMC_ol
156200100402     c* spedizioni "E"
156300100402     C                   Z-ADD     eWTSPEA       WSSTSPe
156400100402     C                   Z-ADD     eWTCLEA       WSSTCLe
156500100402     C                   Z-ADD(H)  eWTKGEA       WSSTKGe
156600100402     C                   Z-ADD(H)  eWTMCEA       WSSTMCe
156700120904     c* old
156800120904     C                   Z-ADD(H)  eWTKGEA       WSSTKGe_ol
156900120904     C                   Z-ADD(H)  eWTMCEA       WSSTMCe_ol
157000100402     c* spedizioni "H"
157100100402     C                   Z-ADD     hWTSPEA       WSSTSPh
157200100402     C                   Z-ADD     hWTCLEA       WSSTCLh
157300100402     C                   Z-ADD(H)  hWTKGEA       WSSTKGh
157400100402     C                   Z-ADD(H)  hWTMCEA       WSSTMCh
157500120904     c* old
157600120904     C                   Z-ADD(H)  hWTKGEA       WSSTKGh_ol
157700120904     C                   Z-ADD(H)  hWTMCEA       WSSTMCh_ol
157800100402     c
157900120905     C                   WRITE     WFLSS600
158000120904     C                   WRITE     WSLSS500
158100090206     C                   ENDIF
158200090206     C*
158300090206     C     WTSPPA        IFGT      *ZERO
158400090206     C                   Z-ADD     §al           WSSDAL
158500090206     c                   movel     'G'           WSSTIE
158600090206     C                   MOVE      'PA'          WSSTIP
158700090206     C                   Z-ADD     FIL(K)        WSSLIN
158800090206     C                   Z-ADD     WTSPPA        WSSTSP
158900090206     C                   Z-ADD     WTCLPA        WSSTCL
159000090206     C                   Z-ADD(H)  WTKGPA        WSSTKG
159100090206     C                   Z-ADD(H)  WTMCPA        WSSTMC
159200120904     c* old
159300120904     C                   Z-ADD(H)  WTKGPA        WSSTKG_ol
159400120904     C                   Z-ADD(H)  WTMCPA        WSSTMC_ol
159500100402     c* spedizioni "E"
159600100402     C                   Z-ADD     eWTSPPA       WSSTSPe
159700100402     C                   Z-ADD     eWTCLPA       WSSTCLe
159800100402     C                   Z-ADD(H)  eWTKGPA       WSSTKGe
159900100402     C                   Z-ADD(H)  eWTMCPA       WSSTMCe
160000120904     c* old
160100120904     C                   Z-ADD(H)  eWTKGPA       WSSTKGe_ol
160200120904     C                   Z-ADD(H)  eWTMCPA       WSSTMCe_ol
160300100402     c* spedizioni "H"
160400100402     C                   Z-ADD     hWTSPPA       WSSTSPh
160500100402     C                   Z-ADD     hWTCLPA       WSSTCLh
160600100402     C                   Z-ADD(H)  hWTKGPA       WSSTKGh
160700100402     C                   Z-ADD(H)  hWTMCPA       WSSTMCh
160800120904     c* old
160900120904     C                   Z-ADD(H)  hWTKGPA       WSSTKGh_ol
161000120904     C                   Z-ADD(H)  hWTMCPA       WSSTMCh_ol
161100100402     c
161200120905     C                   WRITE     WFLSS600
161300120904     C                   WRITE     WSLSS500
161400090206     C                   ENDIF
161500110711     c
161600110711     c                   endif
161700110715     c* Extra budget o clienti particolari
161800110715     c                   if        §stat=' '
161900110715     c                   movel     'G'           WSSTIE
162000110715     c                   else
162100110715     c                   movel     'A'           WSSTIE
162200110715     c                   endif
162300110715     c
162400091006     C     WTSPIP_e      IFGT      *ZERO
162500091007     C                   Z-ADD     §al           WSSDAL
162600091006     C                   MOVE      'IP'          WSSTIP
162700091006     C                   Z-ADD     FIL(K)        WSSLIN
162800091006     C                   Z-ADD     WTSPIP_e      WSSTSP
162900091006     C                   Z-ADD     WTCLIP_e      WSSTCL
163000120904     C                   Z-ADD(H)  WTKGIP_e      WSSTKG_ex
163100120904     C                   Z-ADD(H)  WTMCIP_e      WSSTMC_ex
163200100402     c* spedizioni "E"
163300100402     C                   Z-ADD     eWTSPIP_e     WSSTSPe
163400100402     C                   Z-ADD     eWTCLIP_e     WSSTCLe
163500120904     C                   Z-ADD(H)  eWTKGIP_e     WSSTKGe_ex
163600120904     C                   Z-ADD(H)  eWTMCIP_e     WSSTMCe_ex
163700100402     c* spedizioni "H"
163800100402     C                   Z-ADD     hWTSPIP_e     WSSTSPh
163900100402     C                   Z-ADD     hWTCLIP_e     WSSTCLh
164000120904     C                   Z-ADD(H)  hWTKGIP_e     WSSTKGh_ex
164100120904     C                   Z-ADD(H)  hWTMCIP_e     WSSTMCh_ex
164200100402     c
164300120903     C                   WRITE     wslss550
164400091006     C                   ENDIF
164500100402     c
164600091006     C     WTSPIA_e      IFGT      *ZERO
164700091007     C                   Z-ADD     §al           WSSDAL
164800110715     c* Es - non importa riscrivere il falg: non da clear e già impostato sopra
164900110715     c*****              movel     'G'           WSSTIE
165000091006     C                   MOVE      'IA'          WSSTIP
165100091006     C                   Z-ADD     FIL(K)        WSSLIN
165200091006     C                   Z-ADD     WTSPIA_e      WSSTSP
165300091006     C                   Z-ADD     WTCLIA_e      WSSTCL
165400120904     C                   Z-ADD(H)  WTKGIA_e      WSSTKG_ex
165500120904     C                   Z-ADD(H)  WTMCIA_e      WSSTMC_ex
165600100402     c* spedizioni "E"
165700100402     C                   Z-ADD     eWTSPIA_e     WSSTSPe
165800100402     C                   Z-ADD     eWTCLIA_e     WSSTCLe
165900120904     C                   Z-ADD(H)  eWTKGIA_e     WSSTKGe_ex
166000120904     C                   Z-ADD(H)  eWTMCIA_e     WSSTMCe_ex
166100100402     c* spedizioni "H"
166200100402     C                   Z-ADD     hWTSPIA_e     WSSTSPh
166300100402     C                   Z-ADD     hWTCLIA_e     WSSTCLh
166400120904     C                   Z-ADD(H)  hWTKGIA_e     WSSTKGh_ex
166500120904     C                   Z-ADD(H)  hWTMCIA_e     WSSTMCh_ex
166600100402     c
166700091006     C                   WRITE     WSLSS550
166800091006     C                   ENDIF
166900100402     c
167000091006     C     WTSPEI_e      IFGT      *ZERO
167100091007     C                   Z-ADD     §al           WSSDAL
167200110715     c******             movel     'G'           WSSTIE
167300091006     C                   MOVE      'EI'          WSSTIP
167400091006     C                   Z-ADD     FIL(K)        WSSLIN
167500091006     C                   Z-ADD     WTSPEI_e      WSSTSP
167600091006     C                   Z-ADD     WTCLEI_e      WSSTCL
167700120904     C                   Z-ADD(H)  WTKGEI_e      WSSTKG_ex
167800120904     C                   Z-ADD(H)  WTMCEI_e      WSSTMC_ex
167900100402     c* spedizioni "E"
168000100402     C                   Z-ADD     eWTSPEI_e     WSSTSPe
168100100402     C                   Z-ADD     eWTCLEI_e     WSSTCLe
168200120904     C                   Z-ADD(H)  eWTKGEI_e     WSSTKGe_ex
168300120904     C                   Z-ADD(H)  eWTMCEI_e     WSSTMCe_ex
168400100402     c* spedizioni "H"
168500100402     C                   Z-ADD     hWTSPEI_e     WSSTSPh
168600100402     C                   Z-ADD     hWTCLEI_e     WSSTCLh
168700120904     C                   Z-ADD(H)  hWTKGEI_e     WSSTKGh_ex
168800120904     C                   Z-ADD(H)  hWTMCEI_e     WSSTMCh_ex
168900100402     c
169000091006     C                   WRITE     WSLSS550
169100091006     C                   ENDIF
169200100402     c
169300091006     C     WTSPEE_e      IFGT      *ZERO
169400091007     C                   Z-ADD     §al           WSSDAL
169500110715     c******             movel     'G'           WSSTIE
169600091006     C                   MOVE      'EE'          WSSTIP
169700091006     C                   Z-ADD     FIL(K)        WSSLIN
169800091006     C                   Z-ADD     WTSPEE_e      WSSTSP
169900091006     C                   Z-ADD     WTCLEE_e      WSSTCL
170000120904     C                   Z-ADD(H)  WTKGEE_e      WSSTKG_ex
170100120904     C                   Z-ADD(H)  WTMCEE_e      WSSTMC_ex
170200100402     c* spedizioni "E"
170300100402     C                   Z-ADD     eWTSPEE_e     WSSTSPe
170400100402     C                   Z-ADD     eWTCLEE_e     WSSTCLe
170500120904     C                   Z-ADD(H)  eWTKGEE_e     WSSTKGe_ex
170600120904     C                   Z-ADD(H)  eWTMCEE_e     WSSTMCe_ex
170700100402     c* spedizioni "H"
170800100402     C                   Z-ADD     hWTSPEE_e     WSSTSPh
170900100402     C                   Z-ADD     hWTCLEE_e     WSSTCLh
171000120904     C                   Z-ADD(H)  hWTKGEE_e     WSSTKGh_ex
171100120904     C                   Z-ADD(H)  hWTMCEE_e     WSSTMCh_ex
171200100402     c
171300091006     C                   WRITE     WSLSS550
171400091006     C                   ENDIF
171500091006     C*
171600091006     C     WTSPED_e      IFGT      *ZERO
171700091007     C                   Z-ADD     §al           WSSDAL
171800110715     c*******            movel     'G'           WSSTIE
171900091006     C                   MOVE      'ED'          WSSTIP
172000091006     C                   Z-ADD     FIL(K)        WSSLIN
172100091006     C                   Z-ADD     WTSPED_e      WSSTSP
172200091006     C                   Z-ADD     WTCLED_e      WSSTCL
172300120904     C                   Z-ADD(H)  WTKGED_e      WSSTKG_ex
172400120904     C                   Z-ADD(H)  WTMCED_e      WSSTMC_ex
172500100402     c* spedizioni "E"
172600100402     C                   Z-ADD     eWTSPED_e     WSSTSPe
172700100402     C                   Z-ADD     eWTCLED_e     WSSTCLe
172800120904     C                   Z-ADD(H)  eWTKGED_e     WSSTKGe_ex
172900120904     C                   Z-ADD(H)  eWTMCED_e     WSSTMCe_ex
173000100402     c* spedizioni "H"
173100100402     C                   Z-ADD     hWTSPED_e     WSSTSPh
173200100402     C                   Z-ADD     hWTCLED_e     WSSTCLh
173300120904     C                   Z-ADD(H)  hWTKGED_e     WSSTKGh_ex
173400120904     C                   Z-ADD(H)  hWTMCED_e     WSSTMCh_ex
173500100402     c
173600091006     C                   WRITE     WSLSS550
173700091006     C                   ENDIF
173800091006     C*
173900091006     C     WTSPEX_e      IFGT      *ZERO
174000091007     C                   Z-ADD     §al           WSSDAL
174100110715     c*******            movel     'G'           WSSTIE
174200091006     C                   MOVE      'EX'          WSSTIP
174300091006     C                   Z-ADD     FIL(K)        WSSLIN
174400091006     C                   Z-ADD     WTSPEX_e      WSSTSP
174500091006     C                   Z-ADD     WTCLEX_e      WSSTCL
174600120904     C                   Z-ADD(H)  WTKGEX_e      WSSTKG_ex
174700120904     C                   Z-ADD(H)  WTMCEX_e      WSSTMC_ex
174800100402     c* spedizioni "E"
174900100402     C                   Z-ADD     eWTSPEX_e     WSSTSPe
175000100402     C                   Z-ADD     eWTCLEX_e     WSSTCLe
175100120904     C                   Z-ADD(H)  eWTKGEX_e     WSSTKGe_ex
175200120904     C                   Z-ADD(H)  eWTMCEX_e     WSSTMCe_ex
175300100402     c* spedizioni "H"
175400100402     C                   Z-ADD     hWTSPEX_e     WSSTSPh
175500100402     C                   Z-ADD     hWTCLEX_e     WSSTCLh
175600120904     C                   Z-ADD(H)  hWTKGEX_e     WSSTKGh_ex
175700120904     C                   Z-ADD(H)  hWTMCEX_e     WSSTMCh_ex
175800100402     c
175900091006     C                   WRITE     WSLSS550
176000091006     C                   ENDIF
176100091006     C*
176200091006     C     WTSPEA_e      IFGT      *ZERO
176300091007     C                   Z-ADD     §al           WSSDAL
176400110715     c******             movel     'G'           WSSTIE
176500091006     C                   MOVE      'EA'          WSSTIP
176600091006     C                   Z-ADD     FIL(K)        WSSLIN
176700091006     C                   Z-ADD     WTSPEA_e      WSSTSP
176800091006     C                   Z-ADD     WTCLEA_e      WSSTCL
176900120904     C                   Z-ADD(H)  WTKGEA_e      WSSTKG_ex
177000120904     C                   Z-ADD(H)  WTMCEA_e      WSSTMC_ex
177100100402     c* spedizioni "E"
177200100402     C                   Z-ADD     eWTSPEA_e     WSSTSPe
177300100402     C                   Z-ADD     eWTCLEA_e     WSSTCLe
177400120904     C                   Z-ADD(H)  eWTKGEA_e     WSSTKGe_ex
177500120904     C                   Z-ADD(H)  eWTMCEA_e     WSSTMCe_ex
177600100402     c* spedizioni "H"
177700100402     C                   Z-ADD     hWTSPEA_e     WSSTSPh
177800100402     C                   Z-ADD     hWTCLEA_e     WSSTCLh
177900120904     C                   Z-ADD(H)  hWTKGEA_e     WSSTKGh_ex
178000120904     C                   Z-ADD(H)  hWTMCEA_e     WSSTMCh_ex
178100100402     c
178200091006     C                   WRITE     WSLSS550
178300091006     C                   ENDIF
178400091006     C*
178500091006     C     WTSPPA_e      IFGT      *ZERO
178600091007     C                   Z-ADD     §al           WSSDAL
178700110715     c*******            movel     'G'           WSSTIE
178800091006     C                   MOVE      'PA'          WSSTIP
178900091006     C                   Z-ADD     FIL(K)        WSSLIN
179000091006     C                   Z-ADD     WTSPPA_e      WSSTSP
179100091006     C                   Z-ADD     WTCLPA_e      WSSTCL
179200120904     C                   Z-ADD(H)  WTKGPA_e      WSSTKG_ex
179300120904     C                   Z-ADD(H)  WTMCPA_e      WSSTMC_ex
179400100402     c* spedizioni "E"
179500100402     C                   Z-ADD     eWTSPPA_e     WSSTSPe
179600100402     C                   Z-ADD     eWTCLPA_e     WSSTCLe
179700120904     C                   Z-ADD(H)  eWTKGPA_e     WSSTKGe_ex
179800120904     C                   Z-ADD(H)  eWTMCPA_e     WSSTMCe_ex
179900100402     c* spedizioni "H"
180000100402     C                   Z-ADD     hWTSPPA_e     WSSTSPh
180100100402     C                   Z-ADD     hWTCLPA_e     WSSTCLh
180200120904     C                   Z-ADD(H)  hWTKGPA_e     WSSTKGh_ex
180300120904     C                   Z-ADD(H)  hWTMCPA_e     WSSTMCh_ex
180400100402     c
180500091006     C                   WRITE     WSLSS550
180600091006     C                   ENDIF
180700091006
180800090601     C                   ENDIF
180900090206     c*
181000090206     c* riepilogo
181100090601     c                   if        Wtipo='P' or Wtipo='R'
181200110715     c                   if        §stat=' '
181300110715
181400090206     C     WTSPIP_r      IFGT      *ZERO
181500090206     C                   Z-ADD     §dal          WSSDAL
181600090601     c**                 movel     'R'           WSSTIE
181700090601     c                   movel     Wtipo         WSSTIE
181800090206     C                   MOVE      'IP'          WSSTIP
181900090206     C                   Z-ADD     FIL(K)        WSSLIN
182000090206     C                   Z-ADD     WTSPIP_r      WSSTSP
182100090206     C                   Z-ADD     WTCLIP_r      WSSTCL
182200090206     C                   Z-ADD(H)  WTKGIP_r      WSSTKG
182300090206     C                   Z-ADD(H)  WTMCIP_r      WSSTMC
182400120904     c* old
182500120904     C                   Z-ADD(H)  WTKGIP_r      WSSTKG_ol
182600120904     C                   Z-ADD(H)  WTMCIP_r      WSSTMC_ol
182700100402     c* spedizioni "E"
182800100402     C                   Z-ADD     eWTSPIP_r     WSSTSPe
182900100402     C                   Z-ADD     eWTCLIP_r     WSSTCLe
183000100402     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe
183100100402     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe
183200120904     c* old
183300120904     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe_ol
183400120904     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe_ol
183500100402     c* spedizioni "H"
183600100402     C                   Z-ADD     hWTSPIP_r     WSSTSPh
183700100402     C                   Z-ADD     hWTCLIP_r     WSSTCLh
183800100402     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh
183900100402     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh
184000120904     c* old
184100120904     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh_ol
184200120904     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh_ol
184300100402     c
184400120905     C                   WRITE     WFLSS600
184500120904     C                   WRITE     WSLSS500
184600090206     C                   ENDIF
184700100402     c
184800090206     C     WTSPIA_r      IFGT      *ZERO
184900090206     C                   Z-ADD     §dal          WSSDAL
185000090601     c**                 movel     'R'           WSSTIE
185100090601     c                   movel     Wtipo         WSSTIE
185200090206     C                   MOVE      'IA'          WSSTIP
185300090206     C                   Z-ADD     FIL(K)        WSSLIN
185400090206     C                   Z-ADD     WTSPIA_r      WSSTSP
185500090206     C                   Z-ADD     WTCLIA_r      WSSTCL
185600090206     C                   Z-ADD(H)  WTKGIA_r      WSSTKG
185700090206     C                   Z-ADD(H)  WTMCIA_r      WSSTMC
185800120904     c* old
185900120904     C                   Z-ADD(H)  WTKGIA_r      WSSTKG_ol
186000120904     C                   Z-ADD(H)  WTMCIA_r      WSSTMC_ol
186100100402     c* spedizioni "E"
186200100402     C                   Z-ADD     eWTSPIA_r     WSSTSPe
186300100402     C                   Z-ADD     eWTCLIA_r     WSSTCLe
186400100402     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe
186500100402     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe
186600120904     c* old
186700120904     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe_ol
186800120904     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe_ol
186900100402     c* spedizioni "H"
187000100402     C                   Z-ADD     hWTSPIA_r     WSSTSPh
187100100402     C                   Z-ADD     hWTCLIA_r     WSSTCLh
187200100402     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh
187300100402     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh
187400120904     c* old
187500120904     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh_ol
187600120904     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh_ol
187700100402     c
187800120905     C                   WRITE     WFLSS600
187900120904     C                   WRITE     WSLSS500
188000090206     C                   ENDIF
188100100402
188200090206     C     WTSPEI_R      IFGT      *ZERO
188300090206     C                   Z-ADD     §dal          WSSDAL
188400090601     c**                 movel     'R'           WSSTIE
188500090601     c                   movel     Wtipo         WSSTIE
188600090206     C                   MOVE      'EI'          WSSTIP
188700090206     C                   Z-ADD     FIL(K)        WSSLIN
188800090206     C                   Z-ADD     WTSPEI_r      WSSTSP
188900090206     C                   Z-ADD     WTCLEI_r      WSSTCL
189000090206     C                   Z-ADD(H)  WTKGEI_r      WSSTKG
189100090206     C                   Z-ADD(H)  WTMCEI_r      WSSTMC
189200120904     c* old
189300120904     C                   Z-ADD(H)  WTKGEI_r      WSSTKG_ol
189400120904     C                   Z-ADD(H)  WTMCEI_r      WSSTMC_ol
189500100402     c* Spedizioni "E"
189600100402     C                   Z-ADD     eWTSPEI_r     WSSTSPe
189700100402     C                   Z-ADD     eWTCLEI_r     WSSTCLe
189800100402     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe
189900100402     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe
190000120904     c* old
190100120904     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe_ol
190200120904     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe_ol
190300100402     c* Spedizioni "H"
190400100402     C                   Z-ADD     hWTSPEI_r     WSSTSPh
190500100402     C                   Z-ADD     hWTCLEI_r     WSSTCLh
190600100402     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh
190700100402     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh
190800120904     c* old
190900120904     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh_ol
191000120904     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh_ol
191100100402     c
191200120905     C                   WRITE     WFLSS600
191300120904     C                   WRITE     WSLSS500
191400090206     C                   ENDIF
191500100402     c
191600090206     C     WTSPEE_r      IFGT      *ZERO
191700090206     C                   Z-ADD     §dal          WSSDAL
191800090601     c**                 movel     'R'           WSSTIE
191900090601     c                   movel     Wtipo         WSSTIE
192000090206     C                   MOVE      'EE'          WSSTIP
192100090206     C                   Z-ADD     FIL(K)        WSSLIN
192200090206     C                   Z-ADD     WTSPEE_r      WSSTSP
192300090206     C                   Z-ADD     WTCLEE_r      WSSTCL
192400090206     C                   Z-ADD(H)  WTKGEE_r      WSSTKG
192500090206     C                   Z-ADD(H)  WTMCEE_r      WSSTMC
192600120904     c* old
192700120904     C                   Z-ADD(H)  WTKGEE_r      WSSTKG_ol
192800120904     C                   Z-ADD(H)  WTMCEE_r      WSSTMC_ol
192900100402     c* Spedizioni "E"
193000100402     C                   Z-ADD     eWTSPEE_r     WSSTSPe
193100100402     C                   Z-ADD     eWTCLEE_r     WSSTCLe
193200100402     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe
193300100402     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe
193400120904     c* old
193500120904     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe_ol
193600120904     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe_ol
193700100402     c* Spedizioni "H"
193800100402     C                   Z-ADD     hWTSPEE_r     WSSTSPh
193900100402     C                   Z-ADD     hWTCLEE_r     WSSTCLh
194000100402     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh
194100100402     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh
194200120904     c* old
194300120904     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh_ol
194400120904     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh_ol
194500100402     c
194600120905     C                   WRITE     WFLSS600
194700120904     C                   WRITE     WSLSS500
194800090206     C                   ENDIF
194900090206     C*
195000090206     C     WTSPED_r      IFGT      *ZERO
195100090206     C                   Z-ADD     §dal          WSSDAL
195200090601     c**                 movel     'R'           WSSTIE
195300090601     c                   movel     Wtipo         WSSTIE
195400090206     C                   MOVE      'ED'          WSSTIP
195500090206     C                   Z-ADD     FIL(K)        WSSLIN
195600090206     C                   Z-ADD     WTSPED_r      WSSTSP
195700090206     C                   Z-ADD     WTCLED_r      WSSTCL
195800090206     C                   Z-ADD(H)  WTKGED_r      WSSTKG
195900090206     C                   Z-ADD(H)  WTMCED_r      WSSTMC
196000120904     c* old
196100120904     C                   Z-ADD(H)  WTKGED_r      WSSTKG_ol
196200120904     C                   Z-ADD(H)  WTMCED_r      WSSTMC_ol
196300100402     c* Spedizioni "E"
196400100402     C                   Z-ADD     eWTSPED_r     WSSTSPe
196500100402     C                   Z-ADD     eWTCLED_r     WSSTCLe
196600100402     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe
196700100402     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe
196800120904     c* old
196900120904     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe_ol
197000120904     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe_ol
197100100402     c* Spedizioni "H"
197200100402     C                   Z-ADD     hWTSPED_r     WSSTSPh
197300100402     C                   Z-ADD     hWTCLED_r     WSSTCLh
197400100402     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh
197500100402     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh
197600120904     c* old
197700120904     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh_ol
197800120904     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh_ol
197900100402     c
198000120905     C                   WRITE     WFLSS600
198100120904     C                   WRITE     WSLSS500
198200090206     C                   ENDIF
198300090206     C*
198400090206     C     WTSPEX_r      IFGT      *ZERO
198500090206     C                   Z-ADD     §dal          WSSDAL
198600090601     c**                 movel     'R'           WSSTIE
198700090601     c                   movel     Wtipo         WSSTIE
198800090206     C                   MOVE      'EX'          WSSTIP
198900090206     C                   Z-ADD     FIL(K)        WSSLIN
199000090206     C                   Z-ADD     WTSPEX_r      WSSTSP
199100090206     C                   Z-ADD     WTCLEX_r      WSSTCL
199200090206     C                   Z-ADD(H)  WTKGEX_r      WSSTKG
199300090206     C                   Z-ADD(H)  WTMCEX_r      WSSTMC
199400120904     c* old
199500120904     C                   Z-ADD(H)  WTKGEX_r      WSSTKG_ol
199600120904     C                   Z-ADD(H)  WTMCEX_r      WSSTMC_ol
199700100402     c* Spedizioni "E"
199800100402     C                   Z-ADD     eWTSPEX_r     WSSTSPe
199900100402     C                   Z-ADD     eWTCLEX_r     WSSTCLe
200000100402     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe
200100100402     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe
200200120904     c* old
200300120904     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe_ol
200400120904     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe_ol
200500100402     c* Spedizioni "H"
200600100402     C                   Z-ADD     hWTSPEX_r     WSSTSPh
200700100402     C                   Z-ADD     hWTCLEX_r     WSSTCLh
200800100402     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh
200900100402     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh
201000120904     c* old
201100120904     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh_ol
201200120904     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh_ol
201300100402     c
201400120905     C                   WRITE     WFLSS600
201500120904     C                   WRITE     WSLSS500
201600090206     C                   ENDIF
201700090206     C*
201800090206     C     WTSPEA_r      IFGT      *ZERO
201900090206     C                   Z-ADD     §dal          WSSDAL
202000090601     c**                 movel     'R'           WSSTIE
202100090601     c                   movel     Wtipo         WSSTIE
202200090206     C                   MOVE      'EA'          WSSTIP
202300090206     C                   Z-ADD     FIL(K)        WSSLIN
202400090206     C                   Z-ADD     WTSPEA_r      WSSTSP
202500090206     C                   Z-ADD     WTCLEA_r      WSSTCL
202600090206     C                   Z-ADD(H)  WTKGEA_r      WSSTKG
202700090206     C                   Z-ADD(H)  WTMCEA_r      WSSTMC
202800120904     c* old
202900120904     C                   Z-ADD(H)  WTKGEA_r      WSSTKG_ol
203000120904     C                   Z-ADD(H)  WTMCEA_r      WSSTMC_ol
203100100402     c* Spedizioni "E"
203200100402     C                   Z-ADD     eWTSPEA_r     WSSTSPe
203300100402     C                   Z-ADD     eWTCLEA_r     WSSTCLe
203400100402     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe
203500100402     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe
203600120904     c* old
203700120904     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe_ol
203800120904     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe_ol
203900100402     c* Spedizioni "H"
204000100402     C                   Z-ADD     hWTSPEA_r     WSSTSPh
204100100402     C                   Z-ADD     hWTCLEA_r     WSSTCLh
204200100402     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh
204300100402     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh
204400120904     c* old
204500120904     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh_ol
204600120904     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh_ol
204700100402     c
204800120905     C                   WRITE     WFLSS600
204900120904     C                   WRITE     WSLSS500
205000090206     C                   ENDIF
205100090206     C*
205200090206     C     WTSPPA_r      IFGT      *ZERO
205300090206     C                   Z-ADD     §dal          WSSDAL
205400090601     c**                 movel     'R'           WSSTIE
205500090601     c                   movel     Wtipo         WSSTIE
205600090206     C                   MOVE      'PA'          WSSTIP
205700090206     C                   Z-ADD     FIL(K)        WSSLIN
205800090206     C                   Z-ADD     WTSPPA_r      WSSTSP
205900090206     C                   Z-ADD     WTCLPA_r      WSSTCL
206000090206     C                   Z-ADD(H)  WTKGPA_r      WSSTKG
206100090206     C                   Z-ADD(H)  WTMCPA_r      WSSTMC
206200120904     c* old
206300120904     C                   Z-ADD(H)  WTKGPA_r      WSSTKG_ol
206400120904     C                   Z-ADD(H)  WTMCPA_r      WSSTMC_ol
206500100402     c* Spedizioni "E"
206600100402     C                   Z-ADD     eWTSPPA_r     WSSTSPe
206700100402     C                   Z-ADD     eWTCLPA_r     WSSTCLe
206800100402     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe
206900100402     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe
207000120904     c* old
207100120904     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe_ol
207200120904     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe_ol
207300100402     c* Spedizioni "H"
207400100402     C                   Z-ADD     hWTSPPA_r     WSSTSPh
207500100402     C                   Z-ADD     hWTCLPA_r     WSSTCLh
207600100402     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh
207700100402     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh
207800120904     c* old
207900120904     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh_ol
208000120904     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh_ol
208100100402     c
208200120905     C                   WRITE     WFLSS600
208300120904     C                   WRITE     wslss500
208400090206     C                   ENDIF
208500110715     c
208600110715     c                   endif
208700110715     c
208800110715     c* Extra budget e clienti particolari
208900110715     c                   movel     wtipo         tipoC             1
209000110715     c                   if        §stat<>' ' and wtipo='R'
209100110715     c                   eval      tipoC='B'
209200110715     c                   endif
209300110715     c                   if        §stat<>' ' and wtipo='P'
209400110715     c                   eval      tipoC='C'
209500110715     c                   endif
209600110715     c
209700091006     C     WTSPIP_re     IFGT      *ZERO
209800091006     C                   Z-ADD     §dal          WSSDAL
209900110715     c                   movel     TipoC         WSSTIE
210000091006     C                   MOVE      'IP'          WSSTIP
210100091006     C                   Z-ADD     FIL(K)        WSSLIN
210200091006     C                   Z-ADD     WTSPIP_re     WSSTSP
210300091006     C                   Z-ADD     WTCLIP_re     WSSTCL
210400120904     C                   Z-ADD(H)  WTKGIP_re     WSSTKG_ex
210500120904     C                   Z-ADD(H)  WTMCIP_re     WSSTMC_ex
210600100402     c* Spedizioni "E"
210700100402     C                   Z-ADD     eWTSPIP_re    WSSTSPe
210800100402     C                   Z-ADD     eWTCLIP_re    WSSTCLe
210900120904     C                   Z-ADD(H)  eWTKGIP_re    WSSTKGe_ex
211000120904     C                   Z-ADD(H)  eWTMCIP_re    WSSTMCe_ex
211100100402     c* Spedizioni "H"
211200100402     C                   Z-ADD     hWTSPIP_re    WSSTSPh
211300100402     C                   Z-ADD     hWTCLIP_re    WSSTCLh
211400120904     C                   Z-ADD(H)  hWTKGIP_re    WSSTKGh_ex
211500120904     C                   Z-ADD(H)  hWTMCIP_re    WSSTMCh_ex
211600100402     c
211700091006     C                   WRITE     WSLSS550
211800091006     C                   ENDIF
211900100402     c
212000091006     C     WTSPIA_re     IFGT      *ZERO
212100091006     C                   Z-ADD     §dal          WSSDAL
212200110715     c                   movel     tipoC         WSSTIE
212300091006     C                   MOVE      'IA'          WSSTIP
212400091006     C                   Z-ADD     FIL(K)        WSSLIN
212500091006     C                   Z-ADD     WTSPIA_re     WSSTSP
212600091006     C                   Z-ADD     WTCLIA_re     WSSTCL
212700120904     C                   Z-ADD(H)  WTKGIA_re     WSSTKG_ex
212800120904     C                   Z-ADD(H)  WTMCIA_re     WSSTMC_ex
212900100402     c* Spedizioni "E"
213000100402     C                   Z-ADD     eWTSPIA_re    WSSTSPe
213100100402     C                   Z-ADD     eWTCLIA_re    WSSTCLe
213200120904     C                   Z-ADD(H)  eWTKGIA_re    WSSTKGe_ex
213300120904     C                   Z-ADD(H)  eWTMCIA_re    WSSTMCe_ex
213400100402     c* Spedizioni "H"
213500100402     C                   Z-ADD     hWTSPIA_re    WSSTSPh
213600100402     C                   Z-ADD     hWTCLIA_re    WSSTCLh
213700120904     C                   Z-ADD(H)  hWTKGIA_re    WSSTKGh_ex
213800120904     C                   Z-ADD(H)  hWTMCIA_re    WSSTMCh_ex
213900100402     c
214000091006     C                   WRITE     WSLSS550
214100091006     C                   ENDIF
214200100402     c
214300091006     C     WTSPEI_Re     IFGT      *ZERO
214400091006     C                   Z-ADD     §dal          WSSDAL
214500110715     c                   movel     tipoC         WSSTIE
214600091006     C                   MOVE      'EI'          WSSTIP
214700091006     C                   Z-ADD     FIL(K)        WSSLIN
214800091006     C                   Z-ADD     WTSPEI_re     WSSTSP
214900091006     C                   Z-ADD     WTCLEI_re     WSSTCL
215000120904     C                   Z-ADD(H)  WTKGEI_re     WSSTKG_ex
215100120904     C                   Z-ADD(H)  WTMCEI_re     WSSTMC_ex
215200100402     c* Spedizioni "E"
215300100402     C                   Z-ADD     eWTSPEI_re    WSSTSPe
215400100402     C                   Z-ADD     eWTCLEI_re    WSSTCLe
215500120904     C                   Z-ADD(H)  eWTKGEI_re    WSSTKGe_ex
215600120904     C                   Z-ADD(H)  eWTMCEI_re    WSSTMCe_ex
215700100402     c* Spedizioni "H"
215800100402     C                   Z-ADD     hWTSPEI_re    WSSTSPh
215900100402     C                   Z-ADD     hWTCLEI_re    WSSTCLh
216000120904     C                   Z-ADD(H)  hWTKGEI_re    WSSTKGh_ex
216100120904     C                   Z-ADD(H)  hWTMCEI_re    WSSTMCh_ex
216200100402     c
216300091006     C                   WRITE     WSLSS550
216400091006     C                   ENDIF
216500100402     c
216600091006     C     WTSPEE_re     IFGT      *ZERO
216700091006     C                   Z-ADD     §dal          WSSDAL
216800110715     c                   movel     tipoC         WSSTIE
216900091006     C                   MOVE      'EE'          WSSTIP
217000091006     C                   Z-ADD     FIL(K)        WSSLIN
217100091006     C                   Z-ADD     WTSPEE_re     WSSTSP
217200091006     C                   Z-ADD     WTCLEE_re     WSSTCL
217300120904     C                   Z-ADD(H)  WTKGEE_re     WSSTKG_ex
217400120904     C                   Z-ADD(H)  WTMCEE_re     WSSTMC_ex
217500100402     c* Spedizioni "E"
217600100402     C                   Z-ADD     eWTSPEE_re    WSSTSPe
217700100402     C                   Z-ADD     eWTCLEE_re    WSSTCLe
217800120904     C                   Z-ADD(H)  eWTKGEE_re    WSSTKGe_ex
217900120904     C                   Z-ADD(H)  eWTMCEE_re    WSSTMCe_ex
218000100402     c* Spedizioni "h"
218100100402     C                   Z-ADD     hWTSPEE_re    WSSTSPh
218200100402     C                   Z-ADD     hWTCLEE_re    WSSTCLh
218300120904     C                   Z-ADD(H)  hWTKGEE_re    WSSTKGh_ex
218400120904     C                   Z-ADD(H)  hWTMCEE_re    WSSTMCh_ex
218500100402     c
218600091006     C                   WRITE     WSLSS550
218700091006     C                   ENDIF
218800091006     C*
218900091006     C     WTSPED_re     IFGT      *ZERO
219000091006     C                   Z-ADD     §dal          WSSDAL
219100110715     c                   movel     tipoC         WSSTIE
219200091006     C                   MOVE      'ED'          WSSTIP
219300091006     C                   Z-ADD     FIL(K)        WSSLIN
219400091006     C                   Z-ADD     WTSPED_re     WSSTSP
219500091006     C                   Z-ADD     WTCLED_re     WSSTCL
219600120904     C                   Z-ADD(H)  WTKGED_re     WSSTKG_ex
219700120904     C                   Z-ADD(H)  WTMCED_re     WSSTMC_ex
219800100402     c* Spedizioni "E"
219900100402     C                   Z-ADD     eWTSPED_re    WSSTSPe
220000100402     C                   Z-ADD     eWTCLED_re    WSSTCLe
220100120904     C                   Z-ADD(H)  eWTKGED_re    WSSTKGe_ex
220200120904     C                   Z-ADD(H)  eWTMCED_re    WSSTMCe_ex
220300100402     c* Spedizioni "H"
220400100402     C                   Z-ADD     hWTSPED_re    WSSTSPh
220500100402     C                   Z-ADD     hWTCLED_re    WSSTCLh
220600120904     C                   Z-ADD(H)  hWTKGED_re    WSSTKGh_ex
220700120904     C                   Z-ADD(H)  hWTMCED_re    WSSTMCh_ex
220800100402     c
220900091006     C                   WRITE     WSLSS550
221000091006     C                   ENDIF
221100091006     C*
221200091006     C     WTSPEX_re     IFGT      *ZERO
221300091006     C                   Z-ADD     §dal          WSSDAL
221400110715     c                   movel     tipoC         WSSTIE
221500091006     C                   MOVE      'EX'          WSSTIP
221600091006     C                   Z-ADD     FIL(K)        WSSLIN
221700091006     C                   Z-ADD     WTSPEX_re     WSSTSP
221800091006     C                   Z-ADD     WTCLEX_re     WSSTCL
221900120904     C                   Z-ADD(H)  WTKGEX_re     WSSTKG_ex
222000120904     C                   Z-ADD(H)  WTMCEX_re     WSSTMC_ex
222100100402     c* Spedizioni "E"
222200100402     C                   Z-ADD     eWTSPEX_re    WSSTSPe
222300100402     C                   Z-ADD     eWTCLEX_re    WSSTCLe
222400120904     C                   Z-ADD(H)  eWTKGEX_re    WSSTKGe_ex
222500120904     C                   Z-ADD(H)  eWTMCEX_re    WSSTMCe_ex
222600100402     c* Spedizioni "H"
222700100402     C                   Z-ADD     hWTSPEX_re    WSSTSPh
222800100402     C                   Z-ADD     hWTCLEX_re    WSSTCLh
222900120904     C                   Z-ADD(H)  hWTKGEX_re    WSSTKGh_ex
223000120904     C                   Z-ADD(H)  hWTMCEX_re    WSSTMCh_ex
223100100402     c
223200091006     C                   WRITE     WSLSS550
223300091006     C                   ENDIF
223400091006     C*
223500091006     C     WTSPEA_re     IFGT      *ZERO
223600091006     C                   Z-ADD     §dal          WSSDAL
223700110715     c                   movel     tipoC         WSSTIE
223800091006     C                   MOVE      'EA'          WSSTIP
223900091006     C                   Z-ADD     FIL(K)        WSSLIN
224000091006     C                   Z-ADD     WTSPEA_re     WSSTSP
224100091006     C                   Z-ADD     WTCLEA_re     WSSTCL
224200120904     C                   Z-ADD(H)  WTKGEA_re     WSSTKG_ex
224300120904     C                   Z-ADD(H)  WTMCEA_re     WSSTMC_ex
224400100402     c* Spedizioni "E"
224500100402     C                   Z-ADD     eWTSPEA_re    WSSTSPe
224600100402     C                   Z-ADD     eWTCLEA_re    WSSTCLe
224700120904     C                   Z-ADD(H)  eWTKGEA_re    WSSTKGe_ex
224800120904     C                   Z-ADD(H)  eWTMCEA_re    WSSTMCe_ex
224900100402     c* Spedizioni "H"
225000100402     C                   Z-ADD     hWTSPEA_re    WSSTSPh
225100100402     C                   Z-ADD     hWTCLEA_re    WSSTCLh
225200120904     C                   Z-ADD(H)  hWTKGEA_re    WSSTKGh_ex
225300120904     C                   Z-ADD(H)  hWTMCEA_re    WSSTMCh_ex
225400100402     c
225500091006     C                   WRITE     WSLSS550
225600091006     C                   ENDIF
225700091006     C*
225800091006     C     WTSPPA_re     IFGT      *ZERO
225900091006     C                   Z-ADD     §dal          WSSDAL
226000110715     c                   movel     tipoC         WSSTIE
226100091006     C                   MOVE      'PA'          WSSTIP
226200091006     C                   Z-ADD     FIL(K)        WSSLIN
226300091006     C                   Z-ADD     WTSPPA_re     WSSTSP
226400091006     C                   Z-ADD     WTCLPA_re     WSSTCL
226500120904     C                   Z-ADD(H)  WTKGPA_re     WSSTKG_ex
226600120904     C                   Z-ADD(H)  WTMCPA_re     WSSTMC_ex
226700100402     c* Spedizioni "E"
226800100402     C                   Z-ADD     eWTSPPA_re    WSSTSPe
226900100402     C                   Z-ADD     eWTCLPA_re    WSSTCLe
227000120904     C                   Z-ADD(H)  eWTKGPA_re    WSSTKGe_ex
227100120904     C                   Z-ADD(H)  eWTMCPA_re    WSSTMCe_ex
227200100402     c* Spedizioni "H"
227300100402     C                   Z-ADD     hWTSPPA_re    WSSTSPh
227400100402     C                   Z-ADD     hWTCLPA_re    WSSTCLh
227500120904     C                   Z-ADD(H)  hWTKGPA_re    WSSTKGh_ex
227600120904     C                   Z-ADD(H)  hWTMCPA_re    WSSTMCh_ex
227700100402     c
227800091006     C                   WRITE     WSLSS550
227900091006     C                   ENDIF
228000091006     C                   ENDIF
228100100402
228200090206     c                   enddo
228300090206     c                   endsr
228400970527     C**********************************************************
228500970529     C** CARICAMENTO DATI INIZIALI
228600970527     C**********************************************************
228700970529     C     CARINZ        BEGSR
228800970905     C                   SETOFF                                       90
228900970529     C     KTAS          KLIST
229000970529     C                   KFLD                    WAAS
229100970529     C                   KFLD                    WMGS
229200970529     C     KTAB          KLIST
229300970529     C                   KFLD                    TBLKUT
229400970529     C                   KFLD                    TBLCOD
229500970529     C                   KFLD                    TBLKEY
229600970529     C     KTB           KLIST
229700970529     C                   KFLD                    TBLKUT
229800970529     C                   KFLD                    TBLCOD
229900970529     C                   Z-ADD     1             TBLKUT
230000090204     c     kwfsmp        klist
230100090204     c                   kfld                    k_fil
230200090204     c                   kfld                    k_tip
230300090204     c                   kfld                    k_dat
230400090205     C     kar5          KLIST
230500090205     C                   KFLD                    tasaas
230600090205     C                   KFLD                    taslnp
230700090205     C                   KFLD                    tasnrs
230800090205     C                   KFLD                    tasnsp
230900090205     C                   KFLD                    k_trd
231000090209     C                   KFLD                    tasflp
231100120903     c     kss61         klist
231200120903     c                   kfld                    ks_tie
231300120903     c                   kfld                    ks_tip
231400120903     c                   kfld                    ks_lin
231500120903     c                   kfld                    ks_sca
231600120903     c
231700090915     c* apertura file di stampa congelato in SAVE
231800090915     C* ESEGUO OVRPRTF
231900090915     C                   Z-ADD     61            LUNG             15 5
232000090915     C                   MOVEL     *BLANKS       COMMAN           80
232100090915     C                   MOVEA     CMD           COMMAN
232200090915     C                   CALL      'QCMDEXC'                            H1
232300090915     C                   PARM                    COMMAN
232400090915     C                   PARM                    LUNG
232500090915     C*
232600090915     C                   OPEN      QSYSPRT
232700090915     C*
232800970529     C*
232900970529     C** CARICAMENTO DS MULTIPLA  X DATI FILIALE
233000970527     C** PULIZIA DS
233100121127     C                   DO        400           K                 4 0
233200970527     C     K             OCCUR     DSFIL
233300970527     C                   CLEAR                   DSFIL
233400970527     C                   END
233500121127     C                   DO        400           K
233600100402     C     K             OCCUR     DSFIL_e
233700100402     C                   CLEAR                   DSFIL_e
233800100402     C                   END
233900121127     C                   DO        400           K
234000100402     C     K             OCCUR     DSFIL_h
234100100402     C                   CLEAR                   DSFIL_h
234200100402     C                   END
234300121127     C                   DO        400           K
234400090206     C     K             OCCUR     DSFIL_smp
234500090206     C                   CLEAR                   DSFIL_smp
234600090206     C                   END
234700970527     C*
234800970527     C                   Z-ADD     0             K
234900970527     C     *LOVAL        SETLL     AZORG01L
235000970527     C                   DO        *HIVAL
235100090204     C                   READ      AZORG01L
235200090204     c                   if        %eof(azorg01l)
235300090204     c                   leave
235400090204     c                   endif
235500970527     C     ORGFAG        IFEQ      'F'
235600970527     C     ORGFAG        OREQ      'A'
235700970527     C                   ADD       1             K
235800970527     C                   MOVEL     ORGFIL        FIL(K)
235900090204     C                   MOVEL     ORGdes        DIL(K)
236000090204     C                   z-add     ORGcar        car(K)
236100090204     c                   movel     orgde8        og148
236200090206     c                   if        §oglpo='1' or §oglpo='2'
236300090206     c                   move      §oglpo        lpo(K)
236400090206     c                   else
236500090206     c                   z-add     2             lpo(K)
236600090206     c                   endif
236700090204     c                   movel     orgde3        og143
236800090209     c**                 if        §ogntw='DPD' or §ogntw='EEX' or §ogntw='FED'
236900090209     c**                 move      'E'           tif(K)
237000090209     c**                 else
237100090209     c**                 move      'I'           tif(K)
237200090209     c**                 endif
237300090209     c                   select
237400090209     c                   when      §ogntw='DPD'
237500090209     c                   move      'D'           tif(K)
237600090209     c                   when      §ogntw='EEX'
237700090209     c                   move      'X'           tif(K)
237800090209     c                   when      §ogntw='FED'
237900090209     c                   move      'F'           tif(K)
238000090209     c                   other
238100090209     c                   move      'I'           tif(K)
238200090209     c                   endsl
238300970527     C                   ENDIF
238400090204     C                   ENDDO
238500970529     C*
238600970529     C                   Z-ADD     0             K
238700970529     C                   MOVEL     'TB'          TBLCOD
238800970529     C     KTB           SETLL     TABEL00F
238900970529     C                   DO        *HIVAL
239000090204     C     KTB           READE     TABEL00F
239100090204     c                   if        %eof(tabel00f)
239200090204     c                   leave
239300090204     c                   endif
239400970529     C     TBLFLG        IFNE      '*'
239500970529     C                   MOVEL     TBLUNI        DSTB
239600970529     C     §TBRBL        IFEQ      'N'
239700970529     C     §TBRBL        OREQ      'S'
239800970529     C                   ADD       1             K
239900970529     C                   MOVEL     TBLKEY        TBO(K)
240000970529     C                   ENDIF
240100970529     C                   ENDIF
240200090204     C                   ENDDO
240300120903     c*
240400120903     c* Carico scaglioni di peso
240500120903     C                   Z-ADD     0             II
240600120903     C                   MOVEL     '2L'          TBLCOD
240700120903     C     KTB           SETLL     TABEL00F
240800120903    1C                   DO        *HIVAL
240900120903     C     KTB           READE     TABEL00F
241000120903    2c                   if        %eof(tabel00f)
241100120903     c                   leave
241200120903    2c                   endif
241300120903    2C     TBLFLG        IFNE      '*'
241400120903     C                   MOVEL     TBLUNI        DS2l
241500120903     C                   MOVEL     TBLKEY        WTM03             3
241600120904     C                   MOVEL     TBLKEY        WTM05             5
241700120903    3C     WTM03         IFEQ      'S9R'
241800120903     C                   ADD       1             II
241900120903     C                   MOVE      WTM05         FPR(II)
242000120903     C                   Z-ADD     §2LKGI        KIR(II)
242100120903     C                   Z-ADD     §2LKGF        KFR(II)
242200120903    3C                   ENDIF
242300120903    2C                   ENDIF
242400120903     C                   ENDDO
242500091006     C*
242600091006     c* Lettura TABELLA clienti extra budget
242700110711     c                   if        §stat=' '
242800091006     C                   Z-ADD     0             K
242900091006     C                   MOVEL     'CXB'         TBecod
243000091006     C     tbecod        SETLL     tntbe01l
243100091006     C                   DO        *HIVAL
243200091006     C     tbecod        READE     tntbe01l
243300091006     c                   if        %eof(tntbe01l)
243400091006     c                   leave
243500091006     c                   endif
243600091006
243700091006     C     tbeatb        IFeq      ' '
243800091006     C                   MOVEL     TBeuni        Dcxb
243900091006     C                   ADD       1             K
244000091006     C                   MOVEL     TBeKE1        ksc_ex(k)
244100091007     C                   MOVE      §cxbctr       ksc_ex(k)
244200091006     C                   ENDIF
244300091006     C                   ENDDO
244400110711     c
244500110711     c                   else
244600121121     c* Lettura TABELLA clienti Particolari
244700110711     C                   Z-ADD     0             K
244800110711     C                   MOVEL     'CXQ'         TBecod
244900110711     C     tbecod        SETLL     tntbe01l
245000130819    1C                   DO        *HIVAL
245100110711     C     tbecod        READE     tntbe01l
245200130819    2c                   if        %eof(tntbe01l)
245300110711     c                   leave
245400130819    2c                   endif
245500110711
245600130819    2C     tbeatb        IFeq      ' '
245700110711     C                   ADD       1             K
245800110711     C                   MOVEL     TBeKE2        ksc_ex(k)
245900121121     c                   movel     tbeuni        dcxq
246000130819     c* Terminal di riferimento in cui memorizzare i dati
246100130819    3c                   if        §cxqterm>*zeros
246200121121     c                   movel     §cxqterm      ksc_tr(k)
246300130819     c* Area
246400130819    4c                   if        §cxqarea='T'
246500130819     c                   movel     999           ksc_are(k)
246600130819   x4c                   else
246700130819     c
246800130819     c     ksc_tr(k)     chain     azorg01l
246900130819    5c                   if        %found(azorg01l)
247000130819     c                   movel     orgcar        ksc_are(k)
247100130819   x5c                   else
247200130819     c                   movel     888           ksc_tr(K)
247300130909     c                   movel     888           ksc_are(k)
247400130819    5c                   endif
247500130819    4c                   endif
247600130819    c
247700130819   x3c                   else
247800130819     c* non riesco a memorizzare --> importo 888
247900130819     c                   movel     888           ksc_tr(k)
248000130909     c                   movel     888           ksc_are(k)
248100130819    3c                   endif
248200130819     c
248300121121     c
248400110711     C                   ENDIF
248500110711     C                   ENDDO
248600110711     c                   endif
248700110711     c*
248800110711     c* Controllo riempimento schiera
248900110715     c                   movel     kpjbu         savkpjbu
249000110711     c                   clear                   trul0sds
249100110711     c                   eval      i0stla='L'
249200110711     c                   eval      i0sski='KSC_EX'
249300110711     c                   eval      i0sele=%elem(ksc_ex)
249400110711     c                   eval      i0spie=k
249500110711     c                   eval      i0sfile='Cienti tab CXB-CXQ'
249600110711     c                   eval      i0ssif=knsif
249700110711     c                   eval      i0spgm='TNSSL3R2'
249800110711     c                   movel     trul0sds      kpjbu
249900110711     c                   call      'TRUL0SR'
250000110711     c                   parm                    kpjba
250100110715
250200110715     c                   movel     savkpjbu      kpjbu
250300090601     c
250400090204     c* data elaborazione nel formato AAAA/MM/GG
250500090204     c                   time                    dataiso
250600090204     c                   move      dataiso       wdatael
250700110711     c
250800110711     c                   if        §stat=' '
250900090601     c*
251000090601     c* Pulisco i file per i dati che devo rielaborare
251100090601     c                   eval      wrkgetlista =
251200120905     c                             'delete from WFLSS60F where wsstie= '''   +
251300090603     c                              Wtipo +  ''''
251400090601     c                   if        Wtipo='R'
251500090601     c                   eval      wrkgetlista =%trim(wrkgetlista)+
251600090603     c                              ' or  wsstie=''G'''
251700090601     c                   endif
251800090609     c*
251900090601     C/EXEC SQL
252000090601     C+ execute immediate :wrkgetlista
252100090601     C/END-EXEC
252200090609     c
252300120904     c*
252400120904     c* Pulisco i file per i dati che devo rielaborare
252500120904     c                   eval      wrkgetlista =
252600120904     c                             'delete from wslss50F where wsstie= '''   +
252700120904     c                              Wtipo +  ''''
252800120904     c                   if        Wtipo='R'
252900120904     c                   eval      wrkgetlista =%trim(wrkgetlista)+
253000120904     c                              ' or  wsstie=''G'''
253100120904     c                   endif
253200120904     c*
253300120904     C/EXEC SQL
253400120904     C+ execute immediate :wrkgetlista
253500120904     C/END-EXEC
253600120904     c*
253700090609     c* Pulisco i file per i dati che devo rielaborare
253800090609     c                   clear                   wrkgetlista
253900090609     c                   eval      wrkgetlista =
254000090609     c                             'delete from WFSMP00F where wmptip= '''   +
254100090609     c                              Wtipo +  ''''
254200090609     c                   if        Wtipo='R'
254300090609     c                   eval      wrkgetlista =%trim(wrkgetlista)+
254400090609     c                              ' or  wmptip=''G'''
254500090609     c                   endif
254600110711     c
254700090609     c*
254800090609     C/EXEC SQL
254900090609     C+ execute immediate :wrkgetlista
255000090609     C/END-EXEC
255100120904     c                   endif
255200091007     c*
255300091007     c* Pulisco il file clienti extra budget
255400091007     c                   clear                   wrkgetlista
255500110711     c
255600110711     c                   if        §stat=' '
255700091007     c                   eval      wrkgetlista =
255800120903     c                             'delete from wslss55F where wsstie= '''   +
255900091007     c                              Wtipo +  ''''
256000091007     c                   if        Wtipo='R'
256100091007     c                   eval      wrkgetlista =%trim(wrkgetlista)+
256200091007     c                              ' or  wsstie=''G'''
256300091007     c                   endif
256400111028     C/EXEC SQL
256500111028     C+ execute immediate :wrkgetlista
256600111028     C/END-EXEC
256700110711     c
256800110711     c                   else
256900111028     c
257000111028     c* Pulisco il file clienti particolari
257100110711     c                   select
257200110711     c                   when      Wtipo='R'
257300110711     c                   eval      wrkgetlista =
257400110715     c                             'delete from WSLSS55F where wsstie= ''A'' or-
257500110711     c                               wsstie= ''B'''
257600110711     c                   when      Wtipo='G'
257700110711     c                   eval      wrkgetlista =
257800110711     c                             'delete from WSLSS55F where wsstie= ''A'''
257900110711     c                   when      Wtipo='P'
258000110711     c                   eval      wrkgetlista =
258100110711     c                             'delete from WSLSS55F where wsstie= ''C'''
258200110711     c                   endsl
258300110711     c
258400091007     C/EXEC SQL
258500091007     C+ execute immediate :wrkgetlista
258600091007     C/END-EXEC
258700111028     c
258800111028     c* Preparo la lettura dei soli clienti particolari con SQL
258900111028     c                   clear                   wrkgetlista
259000111028     c                   eval      wrkgetlista =
259100111028     c                             'select * from titas00f where +
259200111028     c                             ((tasaas*10000) +tasmgs) >=' +
259300111028     c                             %editc(§dal:'X')+ ' and +
259400111028     c                             ((tasaas*10000) +tasmgs) <=' +
259500111028     c                             %editc(§al :'X')+ ' and tasccm in('
259600111028      * ciclo sulla sk dei cod bolla di recupero
259700121203     c                   Do        1100          XX
259800111028     c                   If        ksc_ex(XX) = *blanks
259900111028     c                   Leave
260000111028     c                   EndIf
260100111028     c
260200111028     c                   If        XX >1
260300111028     c                   eval      wrkgetlista =
260400111028     c                             %trim(wrkgetlista) + ','
260500111028     c                   EndIf
260600111028     c
260700111028     c                   eval      wrkgetlista =
260800111028     c                             %trim(wrkgetlista) + ' ''' +
260900111028     c                             %subst(ksc_ex(xx):1:7) + ''''
261000111028     c                   EndDo
261100111028     c
261200111028     c                   Eval      wrkgetlista =
261300111028     c                             %trim(wrkgetlista) + ')'
261400111028     c
261500111028     c
261600111028     c                   eval      wrkgetlista =%trimr(wrkgetlista)+' union '+
261700111028     c                             'select * from titas10f where +
261800111028     c                             ((tasaas*10000) +tasmgs) >=' +
261900111028     c                             %editc(§dal:'X')+ ' and +
262000111028     c                             ((tasaas*10000) +tasmgs) <=' +
262100111028     c                             %editc(§al :'X')+ ' and tasccm in('
262200111028      * ciclo sulla sk dei cod bolla di recupero
262300121203     c                   Do        1100          XX                4 0
262400111028     c                   If        ksc_ex(XX) = *blanks
262500111028     c                   Leave
262600111028     c                   EndIf
262700111028     c
262800111028     c                   If        XX >1
262900111028     c                   eval      wrkgetlista =
263000111028     c                             %trim(wrkgetlista) + ','
263100111028     c                   EndIf
263200111028     c
263300111028     c                   eval      wrkgetlista =
263400111028     c                             %trim(wrkgetlista) + ' ''' +
263500111028     c                             %subst(ksc_ex(xx):1:7) + ''''
263600111028     c                   EndDo
263700111028     c
263800111028     c                   Eval      wrkgetlista =
263900111028     c                             %trim(wrkgetlista) + ')'
264000111028     c
264100111028     c                   EndIf
264200111028     c
264300970527     C                   ENDSR
264400100402     c****************************************************************************
264500130408     c     Prepasql2     BEGSR
264600130408     c* Preparo la lettura dei soli clienti particolari con SQL
264700130408     c                   clear                   wrkgetlista
264800130408     c                   eval      wrkgetlista =
264900130408     c                             'select * from titas00f where +
265000130408     c                             ((tasaas*10000) +tasmgs) >=' +
265100130408     c                             %editc(§dal:'X')+ ' and +
265200130408     c                             ((tasaas*10000) +tasmgs) <=' +
265300130408     c                             %editc(§al :'X')+ ' and substr(TASTBL, +
265400130408     c                             1, 1)=''A'' and tasksc in('
265500130408      * ciclo sulla sk dei cod bolla di recupero
265600130408     c                   Do        1100          XX
265700130408     c                   If        ksc_ex(XX) = *blanks
265800130408     c                   Leave
265900130408     c                   EndIf
266000130408     c
266100130408     c                   If        XX >1
266200130408     c                   eval      wrkgetlista =
266300130408     c                             %trim(wrkgetlista) + ','
266400130408     c                   EndIf
266500130408     c
266600130408     c                   eval      wrkgetlista =
266700130408     c                             %trim(wrkgetlista) + ' ''' +
266800130408     c                             %subst(ksc_ex(xx):1:7) + ''''
266900130408     c                   EndDo
267000130408     c
267100130408     c                   Eval      wrkgetlista =
267200130408     c                             %trim(wrkgetlista) + ')'
267300130408     c
267400130408     c
267500130408     c                   eval      wrkgetlista =%trimr(wrkgetlista)+' union '+
267600130408     c                             'select * from titas10f where +
267700130408     c                             ((tasaas*10000) +tasmgs) >=' +
267800130408     c                             %editc(§dal:'X')+ ' and +
267900130408     c                             ((tasaas*10000) +tasmgs) <=' +
268000130408     c                             %editc(§al :'X')+ ' and substr(TASTBL, +
268100130408     c                             1, 1)=''A'' and tasksc in('
268200130408      * ciclo sulla sk dei cod bolla di recupero
268300130408     c                   Do        1100          XX                4 0
268400130408     c                   If        ksc_ex(XX) = *blanks
268500130408     c                   Leave
268600130408     c                   EndIf
268700130408     c
268800130408     c                   If        XX >1
268900130408     c                   eval      wrkgetlista =
269000130408     c                             %trim(wrkgetlista) + ','
269100130408     c                   EndIf
269200130408     c
269300130408     c                   eval      wrkgetlista =
269400130408     c                             %trim(wrkgetlista) + ' ''' +
269500130408     c                             %subst(ksc_ex(xx):1:7) + ''''
269600130408     c                   EndDo
269700130408     c
269800130408     c                   Eval      wrkgetlista =
269900130408     c                             %trim(wrkgetlista) + ')'
270000130408     c
270100130408     c
270200130408     C                   ENDSR
270300130408     c****************************************************************************
270400100402     c     SpeITA        BEGSR
270500100402     c* giornaliero
270600100402     c                   if        (tasaas*10000+tasmgs)=§al
270700100402     C                   ADD       1             WTSPIP
270800100402     C                   ADD       TASNCL        WTCLIP
270900100402     C                   ADD       w_peso        WTKGIP
271000100402     C                   ADD       TASVLF        WTMCIP
271100100402     c* tipo servizio  "E"
271200100402     c                   if        tastsp='E'
271300100402     C                   ADD       1             eWTSPIP
271400100402     C                   ADD       TASNCL        eWTCLIP
271500100402     C                   ADD       w_peso        eWTKGIP
271600100402     C                   ADD       TASVLF        eWTMCIP
271700100402     c                   endif
271800100402     c* tipo servizio  "H"
271900100402     c                   if        tastsp='H'
272000100402     C                   ADD       1             hWTSPIP
272100100402     C                   ADD       TASNCL        hWTCLIP
272200100402     C                   ADD       w_peso        hWTKGIP
272300100402     C                   ADD       TASVLF        hWTMCIP
272400100402     c                   endif
272500100402     c**
272600100402     c* Extra budget
272700100402     c                   if        *in35
272800100402     C                   ADD       1             WTSPIP_e
272900100402     C                   ADD       TASNCL        WTCLIP_e
273000100402     C                   ADD       w_peso        WTKGIP_e
273100100402     C                   ADD       TASVLF        WTMCIP_e
273200100402     c* tipo servizio  "E"
273300100402     c                   if        tastsp='E'
273400100402     C                   ADD       1             eWTSPIP_e
273500100402     C                   ADD       TASNCL        eWTCLIP_e
273600100402     C                   ADD       w_peso        eWTKGIP_e
273700100402     C                   ADD       TASVLF        eWTMCIP_e
273800100402     c                   endif
273900100402     c* tipo servizio  "H"
274000100402     c                   if        tastsp='H'
274100100402     C                   ADD       1             hWTSPIP_e
274200100402     C                   ADD       TASNCL        hWTCLIP_e
274300100402     C                   ADD       w_peso        hWTKGIP_e
274400100402     C                   ADD       TASVLF        hWTMCIP_e
274500100402     c                   endif
274600100402
274700100402     c                   endif
274800100402     c                   endif
274900100402     c* riepilogo
275000100402     C                   ADD       1             WTSPIP_r
275100100402     C                   ADD       TASNCL        WTCLIP_r
275200100402     C                   ADD       w_peso        WTKGIP_r
275300100402     C                   ADD       TASVLF        WTMCIP_r
275400100402     c* tipo servizio  "E"
275500100402     c                   if        tastsp='E'
275600100402     C                   ADD       1             eWTSPIP_r
275700100402     C                   ADD       TASNCL        eWTCLIP_r
275800100402     C                   ADD       w_peso        eWTKGIP_r
275900100402     C                   ADD       TASVLF        eWTMCIP_r
276000100402     c                   endif
276100100402     c* tipo servizio  "H"
276200100402     c                   if        tastsp='H'
276300100402     C                   ADD       1             hWTSPIP_r
276400100402     C                   ADD       TASNCL        hWTCLIP_r
276500100402     C                   ADD       w_peso        hWTKGIP_r
276600100402     C                   ADD       TASVLF        hWTMCIP_r
276700100402     c                   endif
276800100402     c* Extra budget
276900100402     c                   if        *in35
277000100402     C                   ADD       1             WTSPIP_re
277100100402     C                   ADD       TASNCL        WTCLIP_re
277200100402     C                   ADD       w_peso        WTKGIP_re
277300100402     C                   ADD       TASVLF        WTMCIP_re
277400100402     c* tipo servizio  "E"
277500100402     c                   if        tastsp='E'
277600100402     C                   ADD       1             eWTSPIP_re
277700100402     C                   ADD       TASNCL        eWTCLIP_re
277800100402     C                   ADD       w_peso        eWTKGIP_re
277900100402     C                   ADD       TASVLF        eWTMCIP_re
278000100402     c                   endif
278100100402     c* tipo servizio  "H"
278200100402     c                   if        tastsp='H'
278300100402     C                   ADD       1             hWTSPIP_re
278400100402     C                   ADD       TASNCL        hWTCLIP_re
278500100402     C                   ADD       w_peso        hWTKGIP_re
278600100402     C                   ADD       TASVLF        hWTMCIP_re
278700100402     c                   endif
278800100402     c
278900100402     c                   endif
279000120903     c
279100120903     c                   eval      ks_tip='IP'
279200120903     c                   exsr      spe_peso
279300120903     c
279400100402     c                   ENDSR
279500100402     c****************************************************************************
279600100402     c     SpeIMPORT     BEGSR
279700100402     c* giornaliero
279800100402     c                   if        (tasaas*10000+tasmgs)=§al
279900100402     C                   ADD       1             WTSPEI
280000100402     C                   ADD       TASNCL        WTCLEI
280100100402     C                   ADD       w_peso        WTKGEI
280200100402     C                   ADD       TASVLF        WTMCEI
280300100402     c* tipo servizio  "E"
280400100402     c                   if        tastsp='E'
280500100402     C                   ADD       1             eWTSPEI
280600100402     C                   ADD       TASNCL        eWTCLEI
280700100402     C                   ADD       w_peso        eWTKGEI
280800100402     C                   ADD       TASVLF        eWTMCEI
280900100402     c                   endif
281000100402     c* tipo servizio  "H"
281100100402     c                   if        tastsp='H'
281200100402     C                   ADD       1             hWTSPEI
281300100402     C                   ADD       TASNCL        hWTCLEI
281400100402     C                   ADD       w_peso        hWTKGEI
281500100402     C                   ADD       TASVLF        hWTMCEI
281600100402     c                   endif
281700100402     c* Extra Budget
281800100402     c                   if        *in35
281900100402     C                   ADD       1             WTSPEI_e
282000100402     C                   ADD       TASNCL        WTCLEI_e
282100100402     C                   ADD       w_peso        WTKGEI_e
282200100402     C                   ADD       TASVLF        WTMCEI_e
282300100402     c* tipo servizio  "E"
282400100402     c                   if        tastsp='E'
282500100402     C                   ADD       1             eWTSPEI_e
282600100402     C                   ADD       TASNCL        eWTCLEI_e
282700100402     C                   ADD       w_peso        eWTKGEI_e
282800100402     C                   ADD       TASVLF        eWTMCEI_e
282900100402     c                   endif
283000100402     c* tipo servizio  "H"
283100100402     c                   if        tastsp='H'
283200100402     C                   ADD       1             hWTSPEI_e
283300100402     C                   ADD       TASNCL        hWTCLEI_e
283400100402     C                   ADD       w_peso        hWTKGEI_e
283500100402     C                   ADD       TASVLF        hWTMCEI_e
283600100402     c                   endif
283700100402     c
283800100402     c                   endif
283900100402     c                   endif
284000100402     c* riepilogo
284100100402     C                   ADD       1             WTSPEI_r
284200100402     C                   ADD       TASNCL        WTCLEI_r
284300100402     C                   ADD       w_peso        WTKGEI_r
284400100402     C                   ADD       TASVLF        WTMCEI_r
284500100402     c* tipo servizio  "E"
284600100402     c                   if        tastsp='E'
284700100402     C                   ADD       1             eWTSPEI_r
284800100402     C                   ADD       TASNCL        eWTCLEI_r
284900100402     C                   ADD       w_peso        eWTKGEI_r
285000100402     C                   ADD       TASVLF        eWTMCEI_r
285100100402     c                   endif
285200100402     c* tipo servizio  "H"
285300100402     c                   if        tastsp='H'
285400100402     C                   ADD       1             hWTSPEI_r
285500100402     C                   ADD       TASNCL        hWTCLEI_r
285600100402     C                   ADD       w_peso        hWTKGEI_r
285700100402     C                   ADD       TASVLF        hWTMCEI_r
285800100402     c                   endif
285900100402     c* extra budget
286000100402     c                   if        *in35
286100100402     C                   ADD       1             WTSPEI_re
286200100402     C                   ADD       TASNCL        WTCLEI_re
286300100402     C                   ADD       w_peso        WTKGEI_re
286400100402     C                   ADD       TASVLF        WTMCEI_re
286500100402     c* tipo servizio  "E"
286600100402     c                   if        tastsp='E'
286700100402     C                   ADD       1             eWTSPEI_re
286800100402     C                   ADD       TASNCL        eWTCLEI_re
286900100402     C                   ADD       w_peso        eWTKGEI_re
287000100402     C                   ADD       TASVLF        eWTMCEI_re
287100100402     c                   endif
287200100402     c* tipo servizio  "H"
287300100402     c                   if        tastsp='H'
287400100402     C                   ADD       1             hWTSPEI_re
287500100402     C                   ADD       TASNCL        hWTCLEI_re
287600100402     C                   ADD       w_peso        hWTKGEI_re
287700100402     C                   ADD       TASVLF        hWTMCEI_re
287800100402     c                   endif
287900100402     c                   endif
288000120904     c
288100120904     c                   eval      ks_tip='EI'
288200120904     c                   exsr      spe_peso
288300120904     c
288400100402     c                   ENDSR
288500100402     c****************************************************************************
288600100402     c     speEXPRESS    BEGSR
288700100402     c* giornaliero
288800100402     c                   if        (tasaas*10000+tasmgs)=§al
288900100402     C                   ADD       1             WTSPEE                         EUROEXPRESS
289000100402     C                   ADD       TASNCL        WTCLEE
289100100402     C                   ADD       w_peso        WTKGEE
289200100402     C                   ADD       TASVLF        WTMCEE
289300100402     c* tipo servizio  "E"
289400100402     c                   if        tastsp='E'
289500100402     C                   ADD       1             eWTSPEE                         EUROEXPRESS
289600100402     C                   ADD       TASNCL        eWTCLEE
289700100402     C                   ADD       w_peso        eWTKGEE
289800100402     C                   ADD       TASVLF        eWTMCEE
289900100402     c                   endif
290000100402     c* tipo servizio  "H"
290100100402     c                   if        tastsp='H'
290200100402     C                   ADD       1             hWTSPEE                         EUROEXPRESS
290300100402     C                   ADD       TASNCL        hWTCLEE
290400100402     C                   ADD       w_peso        hWTKGEE
290500100402     C                   ADD       TASVLF        hWTMCEE
290600100402     c                   endif
290700100402     c* extra budget
290800100402     c                   if        *in35
290900100402     C                   ADD       1             WTSPEE_e                       EUROEXPRESS
291000100402     C                   ADD       TASNCL        WTCLEE_e
291100100402     C                   ADD       w_peso        WTKGEE_e
291200100402     C                   ADD       TASVLF        WTMCEE_e
291300100402     c* tipo servizio  "E"
291400100402     c                   if        tastsp='E'
291500100402     C                   ADD       1             eWTSPEE_e                       EUROEXPRESS
291600100402     C                   ADD       TASNCL        eWTCLEE_e
291700100402     C                   ADD       w_peso        eWTKGEE_e
291800100402     C                   ADD       TASVLF        eWTMCEE_e
291900100402     c                   endif
292000100402     c* tipo servizio  "H"
292100100402     c                   if        tastsp='H'
292200100402     C                   ADD       1             hWTSPEE_e                       EUROEXPRESS
292300100402     C                   ADD       TASNCL        hWTCLEE_e
292400100402     C                   ADD       w_peso        hWTKGEE_e
292500100402     C                   ADD       TASVLF        hWTMCEE_e
292600100402     c                   endif
292700100402     c
292800100402     c                   endif
292900100402     c                   endif
293000100402     c* riepilogo
293100100402     C                   ADD       1             WTSPEE_r                       EUROEXPRESS
293200100402     C                   ADD       TASNCL        WTCLEE_r
293300100402     C                   ADD       w_peso        WTKGEE_r
293400100402     C                   ADD       TASVLF        WTMCEE_r
293500100402     c* tipo servizio  "E"
293600100402     c                   if        tastsp='E'
293700100402     C                   ADD       1             eWTSPEE_r                       EUROEXPRESS
293800100402     C                   ADD       TASNCL        eWTCLEE_r
293900100402     C                   ADD       w_peso        eWTKGEE_r
294000100402     C                   ADD       TASVLF        eWTMCEE_r
294100100402     c                   endif
294200100402     c* tipo servizio  "H"
294300100402     c                   if        tastsp='H'
294400100402     C                   ADD       1             hWTSPEE_r                       EUROEXPRESS
294500100402     C                   ADD       TASNCL        hWTCLEE_r
294600100402     C                   ADD       w_peso        hWTKGEE_r
294700100402     C                   ADD       TASVLF        hWTMCEE_r
294800100402     c                   endif
294900100402     c* extra budget
295000100402     c                   if        *in35
295100100402     C                   ADD       1             WTSPEE_re                       EUROEXPRESS
295200100402     C                   ADD       TASNCL        WTCLEE_re
295300100402     C                   ADD       w_peso        WTKGEE_re
295400100402     C                   ADD       TASVLF        WTMCEE_re
295500100402     c* tipo servizio  "E"
295600100402     c                   if        tastsp='E'
295700100402     C                   ADD       1             eWTSPEE_re                       EUROEXPRESS
295800100402     C                   ADD       TASNCL        eWTCLEE_re
295900100402     C                   ADD       w_peso        eWTKGEE_re
296000100402     C                   ADD       TASVLF        eWTMCEE_re
296100100402     c                   endif
296200100402     c* tipo servizio  "H"
296300100402     c                   if        tastsp='H'
296400100402     C                   ADD       1             hWTSPEE_re                       EUROEXPRESS
296500100402     C                   ADD       TASNCL        hWTCLEE_re
296600100402     C                   ADD       w_peso        hWTKGEE_re
296700100402     C                   ADD       TASVLF        hWTMCEE_re
296800100402     c                   endif
296900100402     c                   endif
297000120904     c
297100120904     c                   eval      ks_tip='EE'
297200120904     c                   exsr      spe_peso
297300100402     c
297400100402     c                   ENDSR
297500100402     c****************************************************************************
297600100402     c     speDPD        BEGSR
297700100402     c* giornaliero
297800100402     c                   if        (tasaas*10000+tasmgs)=§al
297900100402     C                   ADD       1             WTSPED                         DPD
298000100402     C                   ADD       TASNCL        WTCLED
298100100402     C                   ADD       w_peso        WTKGED
298200100402     C                   ADD       TASVLF        WTMCED
298300100402     c* tipo servizio  "E"
298400100402     c                   if        tastsp='E'
298500100402     C                   ADD       1             eWTSPED                         DPD
298600100402     C                   ADD       TASNCL        eWTCLED
298700100402     C                   ADD       w_peso        eWTKGED
298800100402     C                   ADD       TASVLF        eWTMCED
298900100402     c                   endif
299000100402     c* tipo servizio  "H"
299100100402     c                   if        tastsp='H'
299200100402     C                   ADD       1             hWTSPED                         DPD
299300100402     C                   ADD       TASNCL        hWTCLED
299400100402     C                   ADD       w_peso        hWTKGED
299500100402     C                   ADD       TASVLF        hWTMCED
299600100402     c                   endif
299700100402     c* extra budget
299800100402     c                   if        *in35
299900100402     C                   ADD       1             WTSPED_e                       DPD
300000100402     C                   ADD       TASNCL        WTCLED_e
300100100402     C                   ADD       w_peso        WTKGED_e
300200100402     C                   ADD       TASVLF        WTMCED_e
300300100402     c* tipo servizio  "E"
300400100402     c                   if        tastsp='E'
300500100402     C                   ADD       1             eWTSPED_e                       DPD
300600100402     C                   ADD       TASNCL        eWTCLED_e
300700100402     C                   ADD       w_peso        eWTKGED_e
300800100402     C                   ADD       TASVLF        eWTMCED_e
300900100402     c                   endif
301000100402     c* tipo servizio  "H"
301100100402     c                   if        tastsp='H'
301200100402     C                   ADD       1             hWTSPED_e                       DPD
301300100402     C                   ADD       TASNCL        hWTCLED_e
301400100402     C                   ADD       w_peso        hWTKGED_e
301500100402     C                   ADD       TASVLF        hWTMCED_e
301600100402     c                   endif
301700100402     c
301800100402     c                   endif
301900100402     c                   endif
302000100402     c* riepilogo
302100100402     C                   ADD       1             WTSPED_r                       DPD
302200100402     C                   ADD       TASNCL        WTCLED_r
302300100402     C                   ADD       w_peso        WTKGED_r
302400100402     C                   ADD       TASVLF        WTMCED_r
302500100402     c* tipo servizio  "E"
302600100402     c                   if        tastsp='E'
302700100402     C                   ADD       1             eWTSPED_r                       DPD
302800100402     C                   ADD       TASNCL        eWTCLED_r
302900100402     C                   ADD       w_peso        eWTKGED_r
303000100402     C                   ADD       TASVLF        eWTMCED_r
303100100402     c                   endif
303200100402     c* tipo servizio  "H"
303300100402     c                   if        tastsp='H'
303400100402     C                   ADD       1             hWTSPED_r                       DPD
303500100402     C                   ADD       TASNCL        hWTCLED_r
303600100402     C                   ADD       w_peso        hWTKGED_r
303700100402     C                   ADD       TASVLF        hWTMCED_r
303800100402     c                   endif
303900100402     c* extra budget
304000100402     c                   if        *in35
304100100402     C                   ADD       1             WTSPED_re                       DPD
304200100402     C                   ADD       TASNCL        WTCLED_re
304300100402     C                   ADD       w_peso        WTKGED_re
304400100402     C                   ADD       TASVLF        WTMCED_re
304500100402     c* tipo servizio  "E"
304600100402     c                   if        tastsp='E'
304700100402     C                   ADD       1             eWTSPED_re                       DPD
304800100402     C                   ADD       TASNCL        eWTCLED_re
304900100402     C                   ADD       w_peso        eWTKGED_re
305000100402     C                   ADD       TASVLF        eWTMCED_re
305100100402     c                   endif
305200100402     c* tipo servizio  "H"
305300100402     c                   if        tastsp='H'
305400100402     C                   ADD       1             hWTSPED_re                       DPD
305500100402     C                   ADD       TASNCL        hWTCLED_re
305600100402     C                   ADD       w_peso        hWTKGED_re
305700100402     C                   ADD       TASVLF        hWTMCED_re
305800100402     c                   endif
305900100402     c                   endif
306000120904     c
306100120904     c                   eval      ks_tip='ED'
306200120904     c                   exsr      spe_peso
306300120904     c
306400100402     c                   ENDSR
306500100402     c****************************************************************************
306600100402     c     speFEDEX      BEGSR
306700100402     c* giornaliero
306800100402     c                   if        (tasaas*10000+tasmgs)=§al
306900100402     C                   ADD       1             WTSPEX                         FEDEX
307000100402     C                   ADD       TASNCL        WTCLEX
307100100402     C                   ADD       w_peso        WTKGEX
307200100402     C                   ADD       TASVLF        WTMCEX
307300100402     c* tipo servizio  "E"
307400100402     c                   if        tastsp='E'
307500100402     C                   ADD       1             eWTSPEX                         FEDEX
307600100402     C                   ADD       TASNCL        eWTCLEX
307700100402     C                   ADD       w_peso        eWTKGEX
307800100402     C                   ADD       TASVLF        eWTMCEX
307900100402     c                   endif
308000100402     c* tipo servizio  "H"
308100100402     c                   if        tastsp='H'
308200100402     C                   ADD       1             hWTSPEX                         FEDEX
308300100402     C                   ADD       TASNCL        hWTCLEX
308400100402     C                   ADD       w_peso        hWTKGEX
308500100402     C                   ADD       TASVLF        hWTMCEX
308600100402     c                   endif
308700100402     c* extra budget
308800100402     c                   if        *in35
308900100402     C                   ADD       1             WTSPEX_e                       FEDEX
309000100402     C                   ADD       TASNCL        WTCLEX_e
309100100402     C                   ADD       w_peso        WTKGEX_e
309200100402     C                   ADD       TASVLF        WTMCEX_e
309300100402     c* tipo servizio  "E"
309400100402     c                   if        tastsp='E'
309500100402     C                   ADD       1             eWTSPEX_e                       FEDEX
309600100402     C                   ADD       TASNCL        eWTCLEX_e
309700100402     C                   ADD       w_peso        eWTKGEX_e
309800100402     C                   ADD       TASVLF        eWTMCEX_e
309900100402     c                   endif
310000100402     c* tipo servizio  "H"
310100100402     c                   if        tastsp='H'
310200100402     C                   ADD       1             hWTSPEX_e                       FEDEX
310300100402     C                   ADD       TASNCL        hWTCLEX_e
310400100402     C                   ADD       w_peso        hWTKGEX_e
310500100402     C                   ADD       TASVLF        hWTMCEX_e
310600100402     c                   endif
310700100402     c                   endif
310800100402     c                   endif
310900100402     c* riepilogo
311000100402     C                   ADD       1             WTSPEX_r                       FEDEX
311100100402     C                   ADD       TASNCL        WTCLEX_r
311200100402     C                   ADD       w_peso        WTKGEX_r
311300100402     C                   ADD       TASVLF        WTMCEX_r
311400100402     c* tipo servizio  "E"
311500100402     c                   if        tastsp='E'
311600100402     C                   ADD       1             eWTSPEX_r                       FEDEX
311700100402     C                   ADD       TASNCL        eWTCLEX_r
311800100402     C                   ADD       w_peso        eWTKGEX_r
311900100402     C                   ADD       TASVLF        eWTMCEX_r
312000100402     c                   endif
312100100402     c* tipo servizio  "H"
312200100402     c                   if        tastsp='H'
312300100402     C                   ADD       1             hWTSPEX_r                       FEDEX
312400100402     C                   ADD       TASNCL        hWTCLEX_r
312500100402     C                   ADD       w_peso        hWTKGEX_r
312600100402     C                   ADD       TASVLF        hWTMCEX_r
312700100402     c                   endif
312800100402     c* Extra budget
312900100402     c                   if        *in35
313000100402     C                   ADD       1             WTSPEX_re                       FEDEX
313100100402     C                   ADD       TASNCL        WTCLEX_re
313200100402     C                   ADD       w_peso        WTKGEX_re
313300100402     C                   ADD       TASVLF        WTMCEX_re
313400100402     c* tipo servizio  "E"
313500100402     c                   if        tastsp='E'
313600100402     C                   ADD       1             eWTSPEX_re                       FEDEX
313700100402     C                   ADD       TASNCL        eWTCLEX_re
313800100402     C                   ADD       w_peso        eWTKGEX_re
313900100402     C                   ADD       TASVLF        eWTMCEX_re
314000100402     c                   endif
314100100402     c* tipo servizio  "H"
314200100402     c                   if        tastsp='H'
314300100402     C                   ADD       1             hWTSPEX_re                       FEDEX
314400100402     C                   ADD       TASNCL        hWTCLEX_re
314500100402     C                   ADD       w_peso        hWTKGEX_re
314600100402     C                   ADD       TASVLF        hWTMCEX_re
314700100402     c                   endif
314800100402     c
314900100402     c                   endif
315000120904     c
315100120904     c                   eval      ks_tip='EX'
315200120904     c                   exsr      spe_peso
315300120904     c
315400100402     c                   ENDSR
315500100402     c****************************************************************************
315600100402     c     speITA_LNA    BEGSR
315700100402     c* giornaliero
315800100402     c                   if        (tasaas*10000+tasmgs)=§al
315900100402     C                   ADD       1             WTSPIA
316000100402     C                   ADD       TASNCL        WTCLIA
316100100402     C                   ADD       W_peso        WTKGIA
316200100402     C                   ADD       TASVLF        WTMCIA
316300100402     c* tipo servizio  "E"
316400100402     c                   if        tastsp='E'
316500100402     C                   ADD       1             eWTSPIA
316600100402     C                   ADD       TASNCL        eWTCLIA
316700100402     C                   ADD       W_peso        eWTKGIA
316800100402     C                   ADD       TASVLF        eWTMCIA
316900100402     c                   endif
317000100402     c* tipo servizio  "H"
317100100402     c                   if        tastsp='H'
317200100402     C                   ADD       1             hWTSPIA
317300100402     C                   ADD       TASNCL        hWTCLIA
317400100402     C                   ADD       W_peso        hWTKGIA
317500100402     C                   ADD       TASVLF        hWTMCIA
317600100402     c                   endif
317700100402     c* extra budget
317800100402     c                   if        *in35
317900100402     C                   ADD       1             WTSPIA_e
318000100402     C                   ADD       TASNCL        WTCLIA_e
318100100402     C                   ADD       w_peso        WTKGIA_e
318200100402     C                   ADD       TASVLF        WTMCIA_e
318300100402     c* tipo servizio  "E"
318400100402     c                   if        tastsp='E'
318500100402     C                   ADD       1             eWTSPIA_e
318600100402     C                   ADD       TASNCL        eWTCLIA_e
318700100402     C                   ADD       w_peso        eWTKGIA_e
318800100402     C                   ADD       TASVLF        eWTMCIA_e
318900100402     c                   endif
319000100402     c* tipo servizio  "H"
319100100402     c                   if        tastsp='H'
319200100402     C                   ADD       1             hWTSPIA_e
319300100402     C                   ADD       TASNCL        hWTCLIA_e
319400100402     C                   ADD       w_peso        hWTKGIA_e
319500100402     C                   ADD       TASVLF        hWTMCIA_e
319600100402     c                   endif
319700100402     c
319800100402     c                   endif
319900100402     c                   endif
320000100402     c* riepilogo
320100100402     C                   ADD       1             WTSPIA_r
320200100402     C                   ADD       TASNCL        WTCLIA_r
320300100402     C                   ADD       w_peso        WTKGIA_r
320400100402     C                   ADD       TASVLF        WTMCIA_r
320500100402     c* tipo servizio  "E"
320600100402     c                   if        tastsp='E'
320700100402     C                   ADD       1             eWTSPIA_r
320800100402     C                   ADD       TASNCL        eWTCLIA_r
320900100402     C                   ADD       w_peso        eWTKGIA_r
321000100402     C                   ADD       TASVLF        eWTMCIA_r
321100100402     c                   endif
321200100402     c* tipo servizio  "H"
321300100402     c                   if        tastsp='H'
321400100402     C                   ADD       1             hWTSPIA_r
321500100402     C                   ADD       TASNCL        hWTCLIA_r
321600100402     C                   ADD       w_peso        hWTKGIA_r
321700100402     C                   ADD       TASVLF        hWTMCIA_r
321800100402     c                   endif
321900100402     c* extra budget
322000100402     c                   if        *in35
322100100402     C                   ADD       1             WTSPIA_re
322200100402     C                   ADD       TASNCL        WTCLIA_re
322300100402     C                   ADD       w_peso        WTKGIA_re
322400100402     C                   ADD       TASVLF        WTMCIA_re
322500100402     c* tipo servizio  "E"
322600100402     c                   if        tastsp='E'
322700100402     C                   ADD       1             eWTSPIA_re
322800100402     C                   ADD       TASNCL        eWTCLIA_re
322900100402     C                   ADD       w_peso        eWTKGIA_re
323000100402     C                   ADD       TASVLF        eWTMCIA_re
323100100402     c                   endif
323200100402     c* tipo servizio  "H"
323300100402     c                   if        tastsp='H'
323400100402     C                   ADD       1             hWTSPIA_re
323500100402     C                   ADD       TASNCL        hWTCLIA_re
323600100402     C                   ADD       w_peso        hWTKGIA_re
323700100402     C                   ADD       TASVLF        hWTMCIA_re
323800100402     c                   endif
323900100402     c                   endif
324000120904     c
324100120904     c                   eval      ks_tip='IA'
324200120904     c                   exsr      spe_peso
324300120904     c
324400100402     c                   ENDSR
324500100402     c****************************************************************************
324600100402     c     speEXPORT     BEGSR
324700100402     c* giornaliero
324800100402     c                   if        (tasaas*10000+tasmgs)=§al
324900100402     C                   ADD       1             WTSPEA
325000100402     C                   ADD       TASNCL        WTCLEA
325100100402     C                   ADD       w_peso        WTKGEA
325200100402     C                   ADD       TASVLF        WTMCEA
325300100402     c* tipo servizio  "E"
325400100402     c                   if        tastsp='E'
325500100402     C                   ADD       1             eWTSPEA
325600100402     C                   ADD       TASNCL        eWTCLEA
325700100402     C                   ADD       w_peso        eWTKGEA
325800100402     C                   ADD       TASVLF        eWTMCEA
325900100402     c                   endif
326000100402     c* tipo servizio  "H"
326100100402     c                   if        tastsp='H'
326200100402     C                   ADD       1             hWTSPEA
326300100402     C                   ADD       TASNCL        hWTCLEA
326400100402     C                   ADD       w_peso        hWTKGEA
326500100402     C                   ADD       TASVLF        hWTMCEA
326600100402     c                   endif
326700100402     c* extra budget
326800100402     c                   if        *in35
326900100402     C                   ADD       1             WTSPEA_e
327000100402     C                   ADD       TASNCL        WTCLEA_e
327100100402     C                   ADD       w_peso        WTKGEA_e
327200100402     C                   ADD       TASVLF        WTMCEA_e
327300100402     c* tipo servizio  "E"
327400100402     c                   if        tastsp='E'
327500100402     C                   ADD       1             eWTSPEA_e
327600100402     C                   ADD       TASNCL        eWTCLEA_e
327700100402     C                   ADD       w_peso        eWTKGEA_e
327800100402     C                   ADD       TASVLF        eWTMCEA_e
327900100402     c                   endif
328000100402     c* tipo servizio  "H"
328100100402     c                   if        tastsp='H'
328200100402     C                   ADD       1             hWTSPEA_e
328300100402     C                   ADD       TASNCL        hWTCLEA_e
328400100402     C                   ADD       w_peso        hWTKGEA_e
328500100402     C                   ADD       TASVLF        hWTMCEA_e
328600100402     c                   endif
328700100402     c
328800100402     c                   endif
328900100402     c                   endif
329000100402     c* riepilogo
329100100402     C                   ADD       1             WTSPEA_r
329200100402     C                   ADD       TASNCL        WTCLEA_r
329300100402     C                   ADD       w_peso        WTKGEA_r
329400100402     C                   ADD       TASVLF        WTMCEA_r
329500100402     c* tipo servizio  "E"
329600100402     c                   if        tastsp='E'
329700100402     C                   ADD       1             eWTSPEA_r
329800100402     C                   ADD       TASNCL        eWTCLEA_r
329900100402     C                   ADD       w_peso        eWTKGEA_r
330000100402     C                   ADD       TASVLF        eWTMCEA_r
330100100402     c                   endif
330200100402     c* tipo servizio  "H"
330300100402     c                   if        tastsp='H'
330400100402     C                   ADD       1             hWTSPEA_r
330500100402     C                   ADD       TASNCL        hWTCLEA_r
330600100402     C                   ADD       w_peso        hWTKGEA_r
330700100402     C                   ADD       TASVLF        hWTMCEA_r
330800100402     c                   endif
330900100402     c* extra budget
331000100402     c                   if        *in35
331100100402     C                   ADD       1             WTSPEA_re
331200100402     C                   ADD       TASNCL        WTCLEA_re
331300100402     C                   ADD       w_peso        WTKGEA_re
331400100402     C                   ADD       TASVLF        WTMCEA_re
331500100402     c* tipo servizio  "E"
331600100402     c                   if        tastsp='E'
331700100402     C                   ADD       1             eWTSPEA_re
331800100402     C                   ADD       TASNCL        eWTCLEA_re
331900100402     C                   ADD       w_peso        eWTKGEA_re
332000100402     C                   ADD       TASVLF        eWTMCEA_re
332100100402     c                   endif
332200100402     c* tipo servizio  "H"
332300100402     c                   if        tastsp='H'
332400100402     C                   ADD       1             hWTSPEA_re
332500100402     C                   ADD       TASNCL        hWTCLEA_re
332600100402     C                   ADD       w_peso        hWTKGEA_re
332700100402     C                   ADD       TASVLF        hWTMCEA_re
332800100402     c                   endif
332900100402     c
333000100402     c                   endif
333100120904     c
333200120904     c                   eval      ks_tip='EA'
333300120904     c                   exsr      spe_peso
333400120904     c
333500100402     c                   ENDSR
333600120903     c****************************************************************************
333700120903     c     SPE_peso      BEGSR
333800120903     c
333900120905     c* scaglioni di peso solo se richiesto
334000120905    1c                   if        §stat=' ' and §opz='4'
334100120903     c
334200120903     c* in base al peso trovo lo scaglione e faccio subito la write
334300120903     c                   z-add     1             II
334400120903     c                   clear                   ks_sca
334500120903     c                   dow       ks_sca  =0
334600120903     C     W_peso        IFGT      KIR(Ii)
334700120903     C     W_peso        ANDLE     KFR(Ii)
334800120904     C                   MOVEL     KFR(Ii)       ks_sca
334900120903     C                   endif
335000120903     c                   add       1             II
335100120903     c                   enddo
335200120903     c
335300120903     c* la write/update per peso la faccio subito
335400120903     c
335500120903    2c                   if        Wtipo='G' or Wtipo='R'
335600120903     c
335700120903    3c                   if        (tasaas*10000+tasmgs)=§al
335800120903     c                   eval      ks_tie='G'
335900120903     c                   eval      ks_lin=fil(k)
336000120905     c     kss61         chain     WFLSS61l
336100120903     c
336200120905     c                   if        not %found(WFLSS61l)
336300120905     c                   clear                   WFLSS600
336400120903     c                   eval      wsstie=ks_tie
336500120903     c                   eval      wsstip=ks_tip
336600120903     c                   eval      wsskgf=ks_sca
336700120904     c                   eval      wsslin=ks_lin
336800120903     c                   eval      wssdal=§al
336900120903     c                   eval      wssall=§al
337000120903     c                   endif
337100120903     c
337200120903     c                   exsr      Scrivi_pes
337300120903     c
337400120903    3c                   endif
337500120903     c
337600120903    2c                   endif
337700120903
337800120903    2c                   if        Wtipo='P' or Wtipo='R'
337900120903     c                   eval      ks_tie=wtipo
338000120903     c                   eval      ks_lin=fil(k)
338100120905     c     kss61         chain     WFLSS61l
338200120903     c
338300120905     c                   if        not %found(WFLSS61l)
338400120905     c                   clear                   WFLSS600
338500120903     c                   eval      wsstie=ks_tie
338600120903     c                   eval      wsstip=ks_tip
338700120903     c                   eval      wsskgf=ks_sca
338800120904     c                   eval      wsslin=ks_lin
338900120903     c                   eval      wssdal=§dal
339000120903     c                   eval      wssall=§al
339100120903     c                   endif
339200120903     c
339300120903     c                   exsr      Scrivi_pes
339400120903     c
339500120903    2c                   endif
339600120903     c
339700120903    1c                   endif
339800120903     c                   ENDSR
339900120903     c****************************************************************************
340000120903     c     Scrivi_pes    BEGSR
340100120903     c                   add       1             wsstsp
340200120903     C                   ADD       TASNCL        wsstcl
340300120903     C                   ADD       w_peso        wsstkg
340400120903     C                   ADD       TASVLF        wsstmc
340500120903     c                   if        tastsp='E'
340600120903     c                   add       1             wsstspe
340700120903     C                   ADD       TASNCL        wsstcle
340800120903     C                   ADD       w_peso        wsstkge
340900120903     C                   ADD       TASVLF        wsstmce
341000120903     c                   endif
341100120903     c                   if        tastsp='h'
341200120903     c                   add       1             wsstsph
341300120903     C                   ADD       TASNCL        wsstclh
341400120903     C                   ADD       w_peso        wsstkgh
341500120903     C                   ADD       TASVLF        wsstmch
341600120903     c                   endif
341700120903     c
341800120905     c                   if        not %found(WFLSS61l)
341900120905     c                   write     WFLSS600
342000120903     c                   else
342100120905     c                   update    WFLSS600
342200120903     c                   endif
342300120903     c
342400120903     c                   ENDSR
342500100402     c****************************************************************************
342600970519     OQSYSPRT   E            TESTA          2 02
342700090206     O                                         +  1 'Terminata elaborazione'
342800090206     o                                         +  2 'statistiche spedizioni'
342900090206     O                                         +  2 'DEL'
343000970519     O                       UDATE         Y   +  1
343100970528     O                       §DAL              +  5 '    /  /  '
343200970528     O                                         +  1 '>'
343300970528     O                       §AL               +  1 '    /  /  '
343400090206     O                                          120 'TNSSL3R1'
343500970519     O                       PAGE          Z    132
343600970519     O                                          128 'PAG.'
343700970529     O          E            TESTA          2
343800970529     O                                           14 'TIPI BOLLA OK:'
343900970529     O                       TBO               +  1
344000970529     O          E            ERR            1
344100970529     O                       TASLNP               3
344200970529     O                       TASLNA            +  1
344300970529     O                       TASAAS            +  1
344400970529     O                       TASMGS            +  1
344500970529     O                       TASNRS            +  1
344600970529     O                       TASNSP            +  1
344700970529     O                       TASTBL            +  1
344800110801     O          E            TESTA       1  1
344900110801     O                                            7 'Clienti'
345000110801     o                       Ptipocli         +   1
345100110801     O                                        +   1 ':'
345200110801     O          E            detksc         1
345300110801     o                       skicli             132
345400090915**
345500090915OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES)
