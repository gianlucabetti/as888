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
003400121121     d
003500120903     D* SCHIERA TABELLA FASCIE DI PESO (2L)
003600120903     D FPR             S              2    DIM(20)
003700120903     D KIR             S              6  0 DIM(20)
003800120903     D KFR             S              6  0 DIM(20)
003900091006     d
004000970527     D KPJBA         E DS
004100090204     D  §DAL                 247    254  0
004200090204     D  §AL                  255    262  0
004300090601     D  §opz                 263    263
004400090601     D  §tipo                264    264
004500110711     D  §stat                265    265
004600090204
004700090204     D og143         E DS
004800090204     D og148         E DS
004900090204
005000120903     D ds2l          E DS
005100970529     D DSTB          E DS
005200090204     D DS05          E DS
005300090205     D Dar5trs       E DS
005400091006     D dcxb          E DS
005500121121     D dcxq          E DS
005600110711     d
005700110711     d* DS x controllo riempimento schiere
005800110711     d trul0sds      e ds
005900090204
006000980826     D DSFIL           DS                  OCCURS(400)
006100970527     D  WTSPIP                 1      8  0
006200970527     D  WTSPIA                 9     16  0
006300970527     D  WTSPEI                17     24  0
006400970527     D  WTSPEE                25     32  0
006500970527     D  WTCLIP                33     40  0
006600970527     D  WTCLIA                41     48  0
006700970527     D  WTCLEI                49     56  0
006800970527     D  WTCLEE                57     64  0
006900980826     D  WTKGIP                65     75  1
007000980826     D  WTKGIA                76     86  1
007100980826     D  WTKGEI                87     97  1
007200980826     D  WTKGEE                98    108  1
007300980826     D  WTMCIP               109    119  3
007400980826     D  WTMCIA               120    130  3
007500980826     D  WTMCEI               131    140  3
007600980826     D  WTMCEE               141    150  3
007700000223     D  WTSPEF               151    158  0
007800000223     D  WTCLEF               159    166  0
007900000223     D  WTKGEF               167    177  1
008000000223     D  WTMCEF               178    188  3
008100000607     D  WTSPEA               189    196  0
008200000607     D  WTCLEA               197    204  0
008300000607     D  WTKGEA               205    215  1
008400000607     D  WTMCEA               216    226  3
008500000607     D  WTSPED               227    234  0
008600000607     D  WTCLED               235    242  0
008700000607     D  WTKGED               243    253  1
008800000630     D  WTMCED               254    264  3
008900000630     D  WTSPPA               265    272  0
009000000630     D  WTCLPA               273    280  0
009100000630     D  WTKGPA               281    291  1
009200000630     D  WTMCPA               292    302  3
009300020321     D  WTSPEX               303    310  0
009400020321     D  WTCLEX               311    318  0
009500020321     D  WTKGEX               319    329  1
009600020321     D  WTMCEX               330    340  3
009700090206     D  WTSPIP_R                           like(WTSPIP)
009800090206     D  WTSPIA_r                           like(wtspia)
009900090206     D  WTSPEI_r                           like(wtspei)
010000090206     D  WTSPEE_r                           like(wtspee)
010100090206     D  WTCLIP_r                           like(wtclip)
010200090206     D  WTCLIA_r                           like(wtclia)
010300090206     D  WTCLEI_r                           like(wtclei)
010400090206     D  WTCLEE_r                           like(wtclee)
010500090206     D  WTKGIP_r                           like(wtkgip)
010600090206     D  WTKGIA_r                           like(wtkgia)
010700090206     D  WTKGEI_r                           like(wtkgei)
010800090206     D  WTKGEE_r                           like(wtkgee)
010900090206     D  WTMCIP_r                           like(wtmcip)
011000090206     D  WTMCIA_r                           like(wtmcia)
011100090206     D  WTMCEI_r                           like(wtmcei)
011200090206     D  WTMCEE_r                           like(wtmcee)
011300090206     D  WTSPEF_r                           like(wtspef)
011400090206     D  WTCLEF_r                           like(wtclef)
011500090206     D  WTKGEF_r                           like(wtkgef)
011600090206     D  WTMCEF_r                           like(wtmcef)
011700090206     D  WTSPEA_r                           like(wtspea)
011800090206     D  WTCLEA_r                           like(wtclea)
011900090206     D  WTKGEA_r                           like(wtkgea)
012000090206     D  WTMCEA_r                           like(wtmcea)
012100090206     D  WTSPED_r                           like(wtsped)
012200090206     D  WTCLED_r                           like(wtcled)
012300090206     D  WTKGED_r                           like(wtkged)
012400090206     D  WTMCED_r                           like(wtmced)
012500090206     D  WTSPPA_r                           like(wtsppa)
012600090206     D  WTCLPA_r                           like(wtclpa)
012700090206     D  WTKGPA_r                           like(wtkgpa)
012800090206     D  WTMCPA_r                           like(wtmcpa)
012900090206     D  WTSPEX_r                           like(wtspex)
013000090206     D  WTCLEX_r                           like(wtclex)
013100090206     D  WTKGEX_r                           like(wtkgex)
013200090206     D  WTMCEX_r                           like(wtmcex)
013300091006     d* Giornaliero extra budget
013400091006     D  WTSPIP_E                           like(WTSPIP)
013500091006     D  WTSPIA_e                           like(wtspia)
013600091006     D  WTSPEI_e                           like(wtspei)
013700091006     D  WTSPEE_e                           like(wtspee)
013800091006     D  WTCLIP_e                           like(wtclip)
013900091006     D  WTCLIA_e                           like(wtclia)
014000091006     D  WTCLEI_e                           like(wtclei)
014100091006     D  WTCLEE_e                           like(wtclee)
014200091006     D  WTKGIP_e                           like(wtkgip)
014300091006     D  WTKGIA_e                           like(wtkgia)
014400091006     D  WTKGEI_e                           like(wtkgei)
014500091006     D  WTKGEE_e                           like(wtkgee)
014600091006     D  WTMCIP_e                           like(wtmcip)
014700091006     D  WTMCIA_e                           like(wtmcia)
014800091006     D  WTMCEI_e                           like(wtmcei)
014900091006     D  WTMCEE_e                           like(wtmcee)
015000091006     D  WTSPEF_e                           like(wtspef)
015100091006     D  WTCLEF_e                           like(wtclef)
015200091006     D  WTKGEF_e                           like(wtkgef)
015300091006     D  WTMCEF_e                           like(wtmcef)
015400091006     D  WTSPEA_e                           like(wtspea)
015500091006     D  WTCLEA_e                           like(wtclea)
015600091006     D  WTKGEA_e                           like(wtkgea)
015700091006     D  WTMCEA_e                           like(wtmcea)
015800091006     D  WTSPED_e                           like(wtsped)
015900091006     D  WTCLED_e                           like(wtcled)
016000091006     D  WTKGED_e                           like(wtkged)
016100091006     D  WTMCED_e                           like(wtmced)
016200091006     D  WTSPPA_e                           like(wtsppa)
016300091006     D  WTCLPA_e                           like(wtclpa)
016400091006     D  WTKGPA_e                           like(wtkgpa)
016500091006     D  WTMCPA_e                           like(wtmcpa)
016600091006     D  WTSPEX_e                           like(wtspex)
016700091006     D  WTCLEX_e                           like(wtclex)
016800091006     D  WTKGEX_e                           like(wtkgex)
016900091006     D  WTMCEX_e                           like(wtmcex)
017000091006     d* Riepilogativo extra budget
017100091006     D  WTSPIP_re                          like(WTSPIP)
017200091006     D  WTSPIA_re                          like(wtspia)
017300091006     D  WTSPEI_re                          like(wtspei)
017400091006     D  WTSPEE_re                          like(wtspee)
017500091006     D  WTCLIP_re                          like(wtclip)
017600091006     D  WTCLIA_re                          like(wtclia)
017700091006     D  WTCLEI_re                          like(wtclei)
017800091006     D  WTCLEE_re                          like(wtclee)
017900091006     D  WTKGIP_re                          like(wtkgip)
018000091006     D  WTKGIA_re                          like(wtkgia)
018100091006     D  WTKGEI_re                          like(wtkgei)
018200091006     D  WTKGEE_re                          like(wtkgee)
018300091006     D  WTMCIP_re                          like(wtmcip)
018400091006     D  WTMCIA_re                          like(wtmcia)
018500091006     D  WTMCEI_re                          like(wtmcei)
018600091006     D  WTMCEE_re                          like(wtmcee)
018700091006     D  WTSPEF_re                          like(wtspef)
018800091006     D  WTCLEF_re                          like(wtclef)
018900091006     D  WTKGEF_re                          like(wtkgef)
019000091006     D  WTMCEF_re                          like(wtmcef)
019100091006     D  WTSPEA_re                          like(wtspea)
019200091006     D  WTCLEA_re                          like(wtclea)
019300091006     D  WTKGEA_re                          like(wtkgea)
019400091006     D  WTMCEA_re                          like(wtmcea)
019500091006     D  WTSPED_re                          like(wtsped)
019600091006     D  WTCLED_re                          like(wtcled)
019700091006     D  WTKGED_re                          like(wtkged)
019800091006     D  WTMCED_re                          like(wtmced)
019900091006     D  WTSPPA_re                          like(wtsppa)
020000091006     D  WTCLPA_re                          like(wtclpa)
020100091006     D  WTKGPA_re                          like(wtkgpa)
020200091006     D  WTMCPA_re                          like(wtmcpa)
020300091006     D  WTSPEX_re                          like(wtspex)
020400091006     D  WTCLEX_re                          like(wtclex)
020500091006     D  WTKGEX_re                          like(wtkgex)
020600091006     D  WTMCEX_re                          like(wtmcex)
020700100402     d
020800100402     D DSFIL_e         DS                  OCCURS(400)
020900100402     D  eWTSPIP                1      8  0
021000100402     D  eWTSPIA                9     16  0
021100100402     D  eWTSPEI               17     24  0
021200100402     D  eWTSPEE               25     32  0
021300100402     D  eWTCLIP               33     40  0
021400100402     D  eWTCLIA               41     48  0
021500100402     D  eWTCLEI               49     56  0
021600100402     D  eWTCLEE               57     64  0
021700100402     D  eWTKGIP               65     75  1
021800100402     D  eWTKGIA               76     86  1
021900100402     D  eWTKGEI               87     97  1
022000100402     D  eWTKGEE               98    108  1
022100100402     D  eWTMCIP              109    119  3
022200100402     D  eWTMCIA              120    130  3
022300100402     D  eWTMCEI              131    140  3
022400100402     D  eWTMCEE              141    150  3
022500100402     D  eWTSPEF              151    158  0
022600100402     D  eWTCLEF              159    166  0
022700100402     D  eWTKGEF              167    177  1
022800100402     D  eWTMCEF              178    188  3
022900100402     D  eWTSPEA              189    196  0
023000100402     D  eWTCLEA              197    204  0
023100100402     D  eWTKGEA              205    215  1
023200100402     D  eWTMCEA              216    226  3
023300100402     D  eWTSPED              227    234  0
023400100402     D  eWTCLED              235    242  0
023500100402     D  eWTKGED              243    253  1
023600100402     D  eWTMCED              254    264  3
023700100402     D  eWTSPPA              265    272  0
023800100402     D  eWTCLPA              273    280  0
023900100402     D  eWTKGPA              281    291  1
024000100402     D  eWTMCPA              292    302  3
024100100402     D  eWTSPEX              303    310  0
024200100402     D  eWTCLEX              311    318  0
024300100402     D  eWTKGEX              319    329  1
024400100402     D  eWTMCEX              330    340  3
024500100402     D  eWTSPIP_R                          like(WTSPIP)
024600100402     D  eWTSPIA_r                          like(wtspia)
024700100402     D  eWTSPEI_r                          like(wtspei)
024800100402     D  eWTSPEE_r                          like(wtspee)
024900100402     D  eWTCLIP_r                          like(wtclip)
025000100402     D  eWTCLIA_r                          like(wtclia)
025100100402     D  eWTCLEI_r                          like(wtclei)
025200100402     D  eWTCLEE_r                          like(wtclee)
025300100402     D  eWTKGIP_r                          like(wtkgip)
025400100402     D  eWTKGIA_r                          like(wtkgia)
025500100402     D  eWTKGEI_r                          like(wtkgei)
025600100402     D  eWTKGEE_r                          like(wtkgee)
025700100402     D  eWTMCIP_r                          like(wtmcip)
025800100402     D  eWTMCIA_r                          like(wtmcia)
025900100402     D  eWTMCEI_r                          like(wtmcei)
026000100402     D  eWTMCEE_r                          like(wtmcee)
026100100402     D  eWTSPEF_r                          like(wtspef)
026200100402     D  eWTCLEF_r                          like(wtclef)
026300100402     D  eWTKGEF_r                          like(wtkgef)
026400100402     D  eWTMCEF_r                          like(wtmcef)
026500100402     D  eWTSPEA_r                          like(wtspea)
026600100402     D  eWTCLEA_r                          like(wtclea)
026700100402     D  eWTKGEA_r                          like(wtkgea)
026800100402     D  eWTMCEA_r                          like(wtmcea)
026900100402     D  eWTSPED_r                          like(wtsped)
027000100402     D  eWTCLED_r                          like(wtcled)
027100100402     D  eWTKGED_r                          like(wtkged)
027200100402     D  eWTMCED_r                          like(wtmced)
027300100402     D  eWTSPPA_r                          like(wtsppa)
027400100402     D  eWTCLPA_r                          like(wtclpa)
027500100402     D  eWTKGPA_r                          like(wtkgpa)
027600100402     D  eWTMCPA_r                          like(wtmcpa)
027700100402     D  eWTSPEX_r                          like(wtspex)
027800100402     D  eWTCLEX_r                          like(wtclex)
027900100402     D  eWTKGEX_r                          like(wtkgex)
028000100402     D  eWTMCEX_r                          like(wtmcex)
028100100402     d* Giornaliero extra budget
028200100402     D  eWTSPIP_E                          like(WTSPIP)
028300100402     D  eWTSPIA_e                          like(wtspia)
028400100402     D  eWTSPEI_e                          like(wtspei)
028500100402     D  eWTSPEE_e                          like(wtspee)
028600100402     D  eWTCLIP_e                          like(wtclip)
028700100402     D  eWTCLIA_e                          like(wtclia)
028800100402     D  eWTCLEI_e                          like(wtclei)
028900100402     D  eWTCLEE_e                          like(wtclee)
029000100402     D  eWTKGIP_e                          like(wtkgip)
029100100402     D  eWTKGIA_e                          like(wtkgia)
029200100402     D  eWTKGEI_e                          like(wtkgei)
029300100402     D  eWTKGEE_e                          like(wtkgee)
029400100402     D  eWTMCIP_e                          like(wtmcip)
029500100402     D  eWTMCIA_e                          like(wtmcia)
029600100402     D  eWTMCEI_e                          like(wtmcei)
029700100402     D  eWTMCEE_e                          like(wtmcee)
029800100402     D  eWTSPEF_e                          like(wtspef)
029900100402     D  eWTCLEF_e                          like(wtclef)
030000100402     D  eWTKGEF_e                          like(wtkgef)
030100100402     D  eWTMCEF_e                          like(wtmcef)
030200100402     D  eWTSPEA_e                          like(wtspea)
030300100402     D  eWTCLEA_e                          like(wtclea)
030400100402     D  eWTKGEA_e                          like(wtkgea)
030500100402     D  eWTMCEA_e                          like(wtmcea)
030600100402     D  eWTSPED_e                          like(wtsped)
030700100402     D  eWTCLED_e                          like(wtcled)
030800100402     D  eWTKGED_e                          like(wtkged)
030900100402     D  eWTMCED_e                          like(wtmced)
031000100402     D  eWTSPPA_e                          like(wtsppa)
031100100402     D  eWTCLPA_e                          like(wtclpa)
031200100402     D  eWTKGPA_e                          like(wtkgpa)
031300100402     D  eWTMCPA_e                          like(wtmcpa)
031400100402     D  eWTSPEX_e                          like(wtspex)
031500100402     D  eWTCLEX_e                          like(wtclex)
031600100402     D  eWTKGEX_e                          like(wtkgex)
031700100402     D  eWTMCEX_e                          like(wtmcex)
031800100402     d* Riepilogativo extra budget
031900100402     D  eWTSPIP_re                         like(WTSPIP)
032000100402     D  eWTSPIA_re                         like(wtspia)
032100100402     D  eWTSPEI_re                         like(wtspei)
032200100402     D  eWTSPEE_re                         like(wtspee)
032300100402     D  eWTCLIP_re                         like(wtclip)
032400100402     D  eWTCLIA_re                         like(wtclia)
032500100402     D  eWTCLEI_re                         like(wtclei)
032600100402     D  eWTCLEE_re                         like(wtclee)
032700100402     D  eWTKGIP_re                         like(wtkgip)
032800100402     D  eWTKGIA_re                         like(wtkgia)
032900100402     D  eWTKGEI_re                         like(wtkgei)
033000100402     D  eWTKGEE_re                         like(wtkgee)
033100100402     D  eWTMCIP_re                         like(wtmcip)
033200100402     D  eWTMCIA_re                         like(wtmcia)
033300100402     D  eWTMCEI_re                         like(wtmcei)
033400100402     D  eWTMCEE_re                         like(wtmcee)
033500100402     D  eWTSPEF_re                         like(wtspef)
033600100402     D  eWTCLEF_re                         like(wtclef)
033700100402     D  eWTKGEF_re                         like(wtkgef)
033800100402     D  eWTMCEF_re                         like(wtmcef)
033900100402     D  eWTSPEA_re                         like(wtspea)
034000100402     D  eWTCLEA_re                         like(wtclea)
034100100402     D  eWTKGEA_re                         like(wtkgea)
034200100402     D  eWTMCEA_re                         like(wtmcea)
034300100402     D  eWTSPED_re                         like(wtsped)
034400100402     D  eWTCLED_re                         like(wtcled)
034500100402     D  eWTKGED_re                         like(wtkged)
034600100402     D  eWTMCED_re                         like(wtmced)
034700100402     D  eWTSPPA_re                         like(wtsppa)
034800100402     D  eWTCLPA_re                         like(wtclpa)
034900100402     D  eWTKGPA_re                         like(wtkgpa)
035000100402     D  eWTMCPA_re                         like(wtmcpa)
035100100402     D  eWTSPEX_re                         like(wtspex)
035200100402     D  eWTCLEX_re                         like(wtclex)
035300100402     D  eWTKGEX_re                         like(wtkgex)
035400100402     D  eWTMCEX_re                         like(wtmcex)
035500100402     d
035600100402     D DSFIL_h         DS                  OCCURS(400)
035700100402     D  hWTSPIP                1      8  0
035800100402     D  hWTSPIA                9     16  0
035900100402     D  hWTSPEI               17     24  0
036000100402     D  hWTSPEE               25     32  0
036100100402     D  hWTCLIP               33     40  0
036200100402     D  hWTCLIA               41     48  0
036300100402     D  hWTCLEI               49     56  0
036400100402     D  hWTCLEE               57     64  0
036500100402     D  hWTKGIP               65     75  1
036600100402     D  hWTKGIA               76     86  1
036700100402     D  hWTKGEI               87     97  1
036800100402     D  hWTKGEE               98    108  1
036900100402     D  hWTMCIP              109    119  3
037000100402     D  hWTMCIA              120    130  3
037100100402     D  hWTMCEI              131    140  3
037200100402     D  hWTMCEE              141    150  3
037300100402     D  hWTSPEF              151    158  0
037400100402     D  hWTCLEF              159    166  0
037500100402     D  hWTKGEF              167    177  1
037600100402     D  hWTMCEF              178    188  3
037700100402     D  hWTSPEA              189    196  0
037800100402     D  hWTCLEA              197    204  0
037900100402     D  hWTKGEA              205    215  1
038000100402     D  hWTMCEA              216    226  3
038100100402     D  hWTSPED              227    234  0
038200100402     D  hWTCLED              235    242  0
038300100402     D  hWTKGED              243    253  1
038400100402     D  hWTMCED              254    264  3
038500100402     D  hWTSPPA              265    272  0
038600100402     D  hWTCLPA              273    280  0
038700100402     D  hWTKGPA              281    291  1
038800100402     D  hWTMCPA              292    302  3
038900100402     D  hWTSPEX              303    310  0
039000100402     D  hWTCLEX              311    318  0
039100100402     D  hWTKGEX              319    329  1
039200100402     D  hWTMCEX              330    340  3
039300100402     D  hWTSPIP_R                          like(WTSPIP)
039400100402     D  hWTSPIA_r                          like(wtspia)
039500100402     D  hWTSPEI_r                          like(wtspei)
039600100402     D  hWTSPEE_r                          like(wtspee)
039700100402     D  hWTCLIP_r                          like(wtclip)
039800100402     D  hWTCLIA_r                          like(wtclia)
039900100402     D  hWTCLEI_r                          like(wtclei)
040000100402     D  hWTCLEE_r                          like(wtclee)
040100100402     D  hWTKGIP_r                          like(wtkgip)
040200100402     D  hWTKGIA_r                          like(wtkgia)
040300100402     D  hWTKGEI_r                          like(wtkgei)
040400100402     D  hWTKGEE_r                          like(wtkgee)
040500100402     D  hWTMCIP_r                          like(wtmcip)
040600100402     D  hWTMCIA_r                          like(wtmcia)
040700100402     D  hWTMCEI_r                          like(wtmcei)
040800100402     D  hWTMCEE_r                          like(wtmcee)
040900100402     D  hWTSPEF_r                          like(wtspef)
041000100402     D  hWTCLEF_r                          like(wtclef)
041100100402     D  hWTKGEF_r                          like(wtkgef)
041200100402     D  hWTMCEF_r                          like(wtmcef)
041300100402     D  hWTSPEA_r                          like(wtspea)
041400100402     D  hWTCLEA_r                          like(wtclea)
041500100402     D  hWTKGEA_r                          like(wtkgea)
041600100402     D  hWTMCEA_r                          like(wtmcea)
041700100402     D  hWTSPED_r                          like(wtsped)
041800100402     D  hWTCLED_r                          like(wtcled)
041900100402     D  hWTKGED_r                          like(wtkged)
042000100402     D  hWTMCED_r                          like(wtmced)
042100100402     D  hWTSPPA_r                          like(wtsppa)
042200100402     D  hWTCLPA_r                          like(wtclpa)
042300100402     D  hWTKGPA_r                          like(wtkgpa)
042400100402     D  hWTMCPA_r                          like(wtmcpa)
042500100402     D  hWTSPEX_r                          like(wtspex)
042600100402     D  hWTCLEX_r                          like(wtclex)
042700100402     D  hWTKGEX_r                          like(wtkgex)
042800100402     D  hWTMCEX_r                          like(wtmcex)
042900100402     d* Giornaliero extra budget
043000100402     D  hWTSPIP_E                          like(WTSPIP)
043100100402     D  hWTSPIA_e                          like(wtspia)
043200100402     D  hWTSPEI_e                          like(wtspei)
043300100402     D  hWTSPEE_e                          like(wtspee)
043400100402     D  hWTCLIP_e                          like(wtclip)
043500100402     D  hWTCLIA_e                          like(wtclia)
043600100402     D  hWTCLEI_e                          like(wtclei)
043700100402     D  hWTCLEE_e                          like(wtclee)
043800100402     D  hWTKGIP_e                          like(wtkgip)
043900100402     D  hWTKGIA_e                          like(wtkgia)
044000100402     D  hWTKGEI_e                          like(wtkgei)
044100100402     D  hWTKGEE_e                          like(wtkgee)
044200100402     D  hWTMCIP_e                          like(wtmcip)
044300100402     D  hWTMCIA_e                          like(wtmcia)
044400100402     D  hWTMCEI_e                          like(wtmcei)
044500100402     D  hWTMCEE_e                          like(wtmcee)
044600100402     D  hWTSPEF_e                          like(wtspef)
044700100402     D  hWTCLEF_e                          like(wtclef)
044800100402     D  hWTKGEF_e                          like(wtkgef)
044900100402     D  hWTMCEF_e                          like(wtmcef)
045000100402     D  hWTSPEA_e                          like(wtspea)
045100100402     D  hWTCLEA_e                          like(wtclea)
045200100402     D  hWTKGEA_e                          like(wtkgea)
045300100402     D  hWTMCEA_e                          like(wtmcea)
045400100402     D  hWTSPED_e                          like(wtsped)
045500100402     D  hWTCLED_e                          like(wtcled)
045600100402     D  hWTKGED_e                          like(wtkged)
045700100402     D  hWTMCED_e                          like(wtmced)
045800100402     D  hWTSPPA_e                          like(wtsppa)
045900100402     D  hWTCLPA_e                          like(wtclpa)
046000100402     D  hWTKGPA_e                          like(wtkgpa)
046100100402     D  hWTMCPA_e                          like(wtmcpa)
046200100402     D  hWTSPEX_e                          like(wtspex)
046300100402     D  hWTCLEX_e                          like(wtclex)
046400100402     D  hWTKGEX_e                          like(wtkgex)
046500100402     D  hWTMCEX_e                          like(wtmcex)
046600100402     d* Riepilogativo extra budget
046700100402     D  hWTSPIP_re                         like(WTSPIP)
046800100402     D  hWTSPIA_re                         like(wtspia)
046900100402     D  hWTSPEI_re                         like(wtspei)
047000100402     D  hWTSPEE_re                         like(wtspee)
047100100402     D  hWTCLIP_re                         like(wtclip)
047200100402     D  hWTCLIA_re                         like(wtclia)
047300100402     D  hWTCLEI_re                         like(wtclei)
047400100402     D  hWTCLEE_re                         like(wtclee)
047500100402     D  hWTKGIP_re                         like(wtkgip)
047600100402     D  hWTKGIA_re                         like(wtkgia)
047700100402     D  hWTKGEI_re                         like(wtkgei)
047800100402     D  hWTKGEE_re                         like(wtkgee)
047900100402     D  hWTMCIP_re                         like(wtmcip)
048000100402     D  hWTMCIA_re                         like(wtmcia)
048100100402     D  hWTMCEI_re                         like(wtmcei)
048200100402     D  hWTMCEE_re                         like(wtmcee)
048300100402     D  hWTSPEF_re                         like(wtspef)
048400100402     D  hWTCLEF_re                         like(wtclef)
048500100402     D  hWTKGEF_re                         like(wtkgef)
048600100402     D  hWTMCEF_re                         like(wtmcef)
048700100402     D  hWTSPEA_re                         like(wtspea)
048800100402     D  hWTCLEA_re                         like(wtclea)
048900100402     D  hWTKGEA_re                         like(wtkgea)
049000100402     D  hWTMCEA_re                         like(wtmcea)
049100100402     D  hWTSPED_re                         like(wtsped)
049200100402     D  hWTCLED_re                         like(wtcled)
049300100402     D  hWTKGED_re                         like(wtkged)
049400100402     D  hWTMCED_re                         like(wtmced)
049500100402     D  hWTSPPA_re                         like(wtsppa)
049600100402     D  hWTCLPA_re                         like(wtclpa)
049700100402     D  hWTKGPA_re                         like(wtkgpa)
049800100402     D  hWTMCPA_re                         like(wtmcpa)
049900100402     D  hWTSPEX_re                         like(wtspex)
050000100402     D  hWTCLEX_re                         like(wtclex)
050100100402     D  hWTKGEX_re                         like(wtkgex)
050200100402     D  hWTMCEX_re                         like(wtmcex)
050300100402     d
050400090206     d* dati giornaliero
050500091006     D DSFIL_smp       DS                  OCCURS(400)
050600090206     D  Wmptsp_g                           like(wmptsp)
050700090206     D  wmptcp_g                           like(wmptcp)
050800090206     D  wmptkp_g                     11  1
050900090206     D  wmptmp_g                     10  3
051000090710
051100090206     D  Wmptsa_g                           like(wmptsa)
051200090206     D  wmptca_g                           like(wmptca)
051300090206     D  wmptka_g                     11  1
051400090206     D  wmptma_g                     10  3
051500090710
051600090206     D  Wmptspt_g                          like(wmptspt)
051700090206     D  wmptcpt_g                          like(wmptcpt)
051800090206     D  wmptkpt_g                    11  1
051900090206     D  wmptmpt_g                    10  3
052000090710
052100090710     D  Wmptsptc_g                         like(wmptspt)
052200090710     D  wmptcptc_g                         like(wmptcpt)
052300090710     D  wmptkptc_g                   11  1
052400090710     D  wmptmptc_g                   10  3
052500090710
052600090206     D  Wmptsat_g                          like(wmptsat)
052700090206     D  wmptcat_g                          like(wmptcat)
052800090206     D  wmptkat_g                    11  1
052900090206     D  wmptmat_g                    10  3
053000090710     d
053100090710     D  Wmptsatc_g                         like(wmptsat)
053200090710     D  wmptcatc_g                         like(wmptcat)
053300090710     D  wmptkatc_g                   11  1
053400090710     D  wmptmatc_g                   10  3
053500090710
053600090206     D  Wmptst_g                           like(wmptst)
053700090206     D  wmptct_g                           like(wmptct)
053800090206     D  wmptkt_g                     11  1
053900090206     D  wmptmt_g                     10  3
054000090710
054100090206     d* dati riepilogo
054200090206     D  Wmptsp_r                           like(wmptsp)
054300090206     D  wmptcp_r                           like(wmptcp)
054400090206     D  wmptkp_r                     11  1
054500090206     D  wmptmp_r                     10  3
054600090710
054700090206     D  Wmptsa_r                           like(wmptsa)
054800090206     D  wmptca_r                           like(wmptca)
054900090206     D  wmptka_r                     11  1
055000090206     D  wmptma_r                     10  3
055100090710
055200090206     D  Wmptspt_r                          like(wmptspt)
055300090206     D  wmptcpt_r                          like(wmptcpt)
055400090206     D  wmptkpt_r                    11  1
055500090206     D  wmptmpt_r                    10  3
055600090710
055700090710     D  Wmptsptc_r                         like(wmptspt)
055800090710     D  wmptcptc_r                         like(wmptcpt)
055900090710     D  wmptkptc_r                   11  1
056000090710     D  wmptmptc_r                   10  3
056100090710
056200090206     D  Wmptsat_r                          like(wmptsat)
056300090206     D  wmptcat_r                          like(wmptcat)
056400090206     D  wmptkat_r                    11  1
056500090206     D  wmptmat_r                    10  3
056600090710
056700090710     D  Wmptsatc_r                         like(wmptsat)
056800090710     D  wmptcatc_r                         like(wmptcat)
056900090710     D  wmptkatc_r                   11  1
057000090710     D  wmptmatc_r                   10  3
057100090710
057200090206     D  Wmptst_r                           like(wmptst)
057300090206     D  wmptct_r                           like(wmptct)
057400090206     D  wmptkt_r                     11  1
057500090206     D  wmptmt_r                     10  3
057600090204
057700120903     D II              S              3  0
057800120903     D kk              S              3  0
057900121126     D EE              S              4  0
058000091006     D Wdatael         S              8  0
058100091007     D w010a           S             10
058200110801     D PTIPOCLI        S             12
058300110801     D skicli          S            132
058400090204     d dataiso         s               d   datfmt(*iso)
058500090204     d waas            s                   like(tasaas)
058600090204     d wmgs            s                   Like(tasmgs)
058700090224     d w_peso          s                   Like(taspkf)
058800090204     d k_fil           s                   Like(wmpfil)
058900090204     d k_tip           s                   Like(wmptip)
059000090204     d k_dat           s                   Like(wmpdal)
059100090205     d k_trd           s                   Like(ar5trd) inz('TRS')
059200091006     d savccm          s                   Like(tasccm)
059300091006     d savctr          s                   Like(tasctr)
059400110715     d savkpjbu        s                   Like(kpjbu)
059500120903     d ks_tie          s                   Like(wsstie)
059600120903     d ks_tip          s                   Like(wsstip)
059700120903     d ks_lin          s                   Like(wsslin)
059800120903     d ks_sca          s                   Like(wsskgf)
059900111028     d
060000111028     d titas00ds     E DS                  extname(titas00f)
060100090601     d
060200111028     d wrkgetlista     s          25767    varying
060300120904     d*
060400120904      * ---------------------------------------------------------
060500120904     i*
060600120904     iwslss500
060700120904     I              wsstkg                      wsstkg_ol
060800120904     I              wsstmc                      wsstmc_ol
060900120904     I              wsstkge                     wsstkge_ol
061000120904     I              wsstmce                     wsstmce_ol
061100120904     I              wsstkgh                     wsstkgh_ol
061200120904     I              wsstmch                     wsstmch_ol
061300120904     i*
061400120904     iwslss550
061500120904     I              wsstkg                      wsstkg_ex
061600120904     I              wsstmc                      wsstmc_ex
061700120904     I              wsstkge                     wsstkge_ex
061800120904     I              wsstmce                     wsstmce_ex
061900120904     I              wsstkgh                     wsstkgh_ex
062000120904     I              wsstmch                     wsstmch_ex
062100120904      * ---------------------------------------------------------
062200970527     C* RICEVIMENTO PARAMETRI
062300970527     C     *ENTRY        PLIST
062400970527     C                   PARM                    KPJBA
062500970527     C                   MOVEL     §DAL          WAAS
062600970527     C                   MOVE      §DAL          WMGS
062700090601     C                   MOVE      §tipo         Wtipo             1
062800970528     C*
062900970529     C                   EXSR      CARINZ
063000951220     C*
063100990811     C* LETTURA TITAS
063200111028     c                   if        §stat=' '
063300990811     C     KTAS          SETLL     TITAS38C
063400111028     c                   else
063500111028
063600111028     C/EXEC SQL
063700111028     C+ PREPARE s1 FROM :wrkgetlista
063800111028     C/END-EXEC
063900111028     C
064000111028     C/EXEC SQL
064100111028     C+ DECLARE A1 CURSOR FOR S1
064200111028     C/END-EXEC
064300111028     C
064400111028     C/EXEC SQL
064500111028     C+ OPEN a1
064600111028     C/END-EXEC
064700111028
064800111028     c                   endif
064900111028     c
065000970527     C                   DO        *HIVAL
065100111028     c
065200111028     c                   if        §stat=' '
065300090204     C                   READ      TITAS38C
065400090204     c* Termina il ciclo se fine file o per data spedizione > data al
065500090204     C                   if        %eof(titas38c) or ((tasaas*10000+tasmgs)>§al)
065600090204     c                   leave
065700090204     c                   endif
065800111028     c
065900111028     c                   else
066000111028     C/EXEC SQL
066100111028     C+ FETCH NEXT FROM a1 INTO :titas00ds
066200111028     C/END-EXEC
066300111028
066400111028     c                   select
066500111028
066600111028     c                   when      sqlcod = 100
066700111028     C/EXEC SQL
066800111103     C+ CLOSE a1
066900111028     C/END-EXEC
067000111028     c                   leave
067100111028     c                   when      sqlcod < 0
067200111028     c                   seton                                        H1
067300111028     C/EXEC SQL
067400111103     C+ CLOSE a1
067500111028     C/END-EXEC
067600111028     c                   leave
067700111028     c                   endsl
067800111028     c
067900111028     c                   endif
068000111028     c
068100090204     c* Scarto in base al tipo bolla
068200090204     c                   if        %lookup(tastbl:tbo)=0
068300090204     c                   iter
068400090204     c                   endif
068500970527     C*
068600090601     c
068700090601     c* EDPES : 01/06/09
068800090601     c* Per ora riutilizzo solo il peso bollettato/variato e non VDL
068900090901     c***                z-add     taspkf        w_peso
069000090901     c* EDPES:  01/09/09
069100090901     c**  riutilizzo il peso VDL
069200090901     c* imposto il peso che sarà da memorizzare secondo il seguente ordine:
069300090901     c* 1. VDL totale
069400090901     c* 2. VDL parziale se > di TASPKF
069500090901     c* 3. TASPKF
069600090901     c                   clear                   w_peso
069700090901     c                   if        tasncp=tasncl or (tasncp>0 and taspkc>taspkf)
069800090901     c                   z-add     taspkc        w_peso
069900090901     c                   else
070000090901     c                   z-add     taspkf        w_peso
070100090901     c                   endif
070200090601     c
070300110711     c                   if        §stat=' '
070400090204     c                   exsr      sr_wfsmp
070500110711     c                   endif
070600110711     c
070700090204     c                   exsr      sr_wslss
070800090204     C                   ENDDO
070900090206     c*
071000090206     C* SCARICO DS SU FILE
071100090206     c                   exsr      scrive_file
071200110801     c                   if        §stat=' '
071300110801     c                   eval      ptipocli='EXTRA BUDGET'
071400110801     c                   else
071500110801     c                   eval      ptipocli='PARTICOLARI '
071600110801     c                   endif
071700110801     c
071800090206     c                   except    testa
071900110801     C*
072000110801     c                   z-add     1             k
072100110801     c                   clear                   conta             3 0
072200110801     c                   clear                   skicli
072300110801     c                   dow       ksc_ex(k)<>*blanks
072400110801     c
072500110801     c                   if        §stat=' '
072600110801
072700110801     c                   eval      skicli=%trim(skicli)+' '+
072800110801     c                             %subst(ksc_ex(k):1:7)
072900110801     c                   add       7             conta
073000110801     c*
073100110801     c                   if        %subst(ksc_ex(k):8:3)<>'   '
073200110801     c                   eval      skicli=%trim(skicli)+'/'+
073300110801     c                             %subst(ksc_ex(k):8:3)
073400110801     c                   add       4             conta
073500110801     c                   endif
073600110801     c                   add       1             conta
073700110801     c
073800110801     c
073900110801     c                   if        conta>120
074000110801     c                   except    detksc
074100110801     c                   clear                   conta
074200110801     c                   clear                   skicli
074300110801     c                   endif
074400110801     c
074500110801     c                   else
074600110801     c
074700110801     c                   eval      skicli=%trim(skicli)+' '+
074800110801     c                             %subst(ksc_ex(k):1:7)
074900110801     c                   add       1             conta
075000110801     c
075100110801     c                   if        conta=16
075200110801     c                   except    detksc
075300110801     c                   clear                   conta
075400110801     c                   clear                   skicli
075500110801     c                   endif
075600110801     c
075700110801     c                   endif
075800110801     c
075900110801     c                   add       1             k
076000110801     c                   enddo
076100110801     c
076200110801     c                   if        conta>0
076300110801     c                   except    detksc
076400110801     c                   endif
076500970528     C*
076600970528     C                   SETON                                        LR
076700970519     C*
076800090204     C**********************************************************
076900090204     C** Elaborazione per file wfsmp00f
077000090204     C**********************************************************
077100090204     C     sr_wfsmp      BEGSR
077200090204     c* linea di partenza
077300090206     c                   z-add     1             k
077400090206     c     taslnp        lookup    fil(k)                                 58
077500090206     c                   if        *in58
077600090206     c     k             occur     dsfil_smp
077700090205     c* . . . giornaliero
077800090206     c                   if        (tasaas*10000+tasmgs)=§al
077900090206     C                   add       1             Wmptsp_g
078000090206     C                   add       TASNCL        wmptcp_g
078100090224     C                   add       w_peso        Wmptkp_g
078200090206     C                   add       TASVLF        Wmptmp_g
078300090206     c                   endif
078400090204     c* . . . riepilogo
078500090206     C                   add       1             Wmptsp_r
078600090206     C                   add       TASNCL        wmptcp_r
078700090224     C                   add       w_peso        Wmptkp_r
078800090206     C                   add       TASVLF        Wmptmp_r
078900090206     c                   endif
079000090204     c* linea di arrivo
079100090206     c                   z-add     1             k
079200090206     c     taslna        lookup    fil(k)                                 58
079300090206     c                   if        *in58
079400090206     c     k             occur     dsfil_smp
079500090205     c* . . . giornaliero
079600090206     c                   if        (tasaas*10000+tasmgs)=§al
079700090206     C                   add       1             Wmptsa_g
079800090206     C                   add       TASNCL        wmptca_g
079900090224     C                   add       w_peso        Wmptka_g
080000090206     C                   add       TASVLF        Wmptma_g
080100090206     c                   endif
080200090205     c* . . . riepilogo
080300090206     C                   add       1             Wmptsa_r
080400090206     C                   add       TASNCL        wmptca_r
080500090224     C                   add       w_peso        Wmptka_r
080600090206     C                   add       TASVLF        Wmptma_r
080700090206     c                   endif
080800090710     c* terminal di partenza : esclusa la linea/terminal
080900090710     c                   if        tastfp<>taslnp
081000090206     c                   z-add     1             k
081100090206     c     tastfp        lookup    fil(k)                                 58
081200090206     c                   if        *in58
081300090206     c     k             occur     dsfil_smp
081400090205     c* . . . giornaliero
081500090206     c                   if        (tasaas*10000+tasmgs)=§al
081600090206     C                   add       1             Wmptspt_g
081700090206     C                   add       TASNCL        wmptcpt_g
081800090224     C                   add       w_peso        Wmptkpt_g
081900090206     C                   add       TASVLF        Wmptmpt_g
082000090206     c                   endif
082100090205     c* . . . riepilogo
082200090206     C                   add       1             Wmptspt_r
082300090206     C                   add       TASNCL        wmptcpt_r
082400090224     C                   add       w_peso        Wmptkpt_r
082500090206     C                   add       TASVLF        Wmptmpt_r
082600090206     c                   endif
082700090710     c                   endif
082800090710     c* terminal di partenza : tutto compresa la linea stessa
082900090710     c                   z-add     1             k
083000090710     c     tastfp        lookup    fil(k)                                 58
083100090710     c                   if        *in58
083200090710     c     k             occur     dsfil_smp
083300090710     c* . . . giornaliero
083400090710     c                   if        (tasaas*10000+tasmgs)=§al
083500090710     C                   add       1             Wmptsptc_g
083600090710     C                   add       TASNCL        wmptcptc_g
083700090710     C                   add       w_peso        Wmptkptc_g
083800090710     C                   add       TASVLF        Wmptmptc_g
083900090710     c                   endif
084000090710     c* . . . riepilogo
084100090710     C                   add       1             Wmptsptc_r
084200090710     C                   add       TASNCL        wmptcptc_r
084300090710     C                   add       w_peso        Wmptkptc_r
084400090710     C                   add       TASVLF        Wmptmptc_r
084500090710     c                   endif
084600090710     c
084700090710     c* terminal di arrivo : esclusa la linea/terminal
084800090710     c                   if        tastfa<>taslna
084900090206     c                   z-add     1             k
085000090206     c     tastfa        lookup    fil(k)                                 58
085100090206     c                   if        *in58
085200090206     c     k             occur     dsfil_smp
085300090205     c* . . . giornaliero
085400090206     c                   if        (tasaas*10000+tasmgs)=§al
085500090206     C                   add       1             Wmptsat_g
085600090206     C                   add       TASNCL        wmptcat_g
085700090224     C                   add       w_peso        Wmptkat_g
085800090206     C                   add       TASVLF        Wmptmat_g
085900090206     c                   endif
086000090205     c* . . . riepilogo
086100090206     C                   add       1             Wmptsat_r
086200090206     C                   add       TASNCL        wmptcat_r
086300090224     C                   add       w_peso        Wmptkat_r
086400090206     C                   add       TASVLF        Wmptmat_r
086500090205     c                   endif
086600090710     c                   endif
086700090710     c* terminal di arrivo : tutto compreso il terminal stesso
086800090710     c                   z-add     1             k
086900090710     c     tastfa        lookup    fil(k)                                 58
087000090710     c                   if        *in58
087100090710     c     k             occur     dsfil_smp
087200090710     c* . . . giornaliero
087300090710     c                   if        (tasaas*10000+tasmgs)=§al
087400090710     C                   add       1             Wmptsatc_g
087500090710     C                   add       TASNCL        wmptcatc_g
087600090710     C                   add       w_peso        Wmptkatc_g
087700090710     C                   add       TASVLF        Wmptmatc_g
087800090710     c                   endif
087900090710     c* . . . riepilogo
088000090710     C                   add       1             Wmptsatc_r
088100090710     C                   add       TASNCL        wmptcatc_r
088200090710     C                   add       w_peso        Wmptkatc_r
088300090710     C                   add       TASVLF        Wmptmatc_r
088400090710     c                   endif
088500090710     c
088600090204     c* filiali di transito
088700090209     c     kar5          chain     fiar531c
088800090205     c                   if        %found(fiar531c)
088900090205     c                   movel     ar5uni        dar5trs
089000090205     c*
089100090205     c                   eval      k_fil=§AR5FLP
089200090205     c                   exsr      agg_transiti
089300090205      *
089400090205     c                   if        §ar5fl2>0
089500090205     c                   eval      k_fil=§AR5FL2
089600090205     c                   exsr      agg_transiti
089700090205     c                   endif
089800090205      *
089900090205     c                   if        §ar5fl3>0
090000090205     c                   eval      k_fil=§AR5FL3
090100090205     c                   exsr      agg_transiti
090200090205     c                   endif
090300090205      *
090400090205     c                   if        §ar5fl4>0
090500090205     c                   eval      k_fil=§AR5FL4
090600090205     c                   exsr      agg_transiti
090700090205     c                   endif
090800090205      *
090900090205     c                   if        §ar5fl5>0
091000090205     c                   eval      k_fil=§AR5FL5
091100090205     c                   exsr      agg_transiti
091200090205     c                   endif
091300090205     c                   endif
091400090204     c
091500090204     C                   endsr
091600090205     C**********************************************************
091700090205     C** Aggiornamento/scrittura giornaliero e riepil. x transiti
091800090205     C**********************************************************
091900090205     C     agg_transiti  BEGSR
092000090206     c                   z-add     1             k
092100090206     c     k_fil         lookup    fil(k)                                 58
092200090206     c                   if        *in58
092300090206     c     k             occur     dsfil_smp
092400090205     c* . . . giornaliero
092500090206     c                   if        (tasaas*10000+tasmgs)=§al
092600090206     C                   add       1             Wmptst_g
092700090206     C                   add       TASNCL        wmptct_g
092800090224     C                   add       w_peso        Wmptkt_g
092900090206     C                   add       TASVLF        Wmptmt_g
093000090206     c                   endif
093100090205     c* . . . riepilogo
093200090206     C                   add       1             Wmptst_r
093300090206     C                   add       TASNCL        wmptct_r
093400090224     C                   add       w_peso        Wmptkt_r
093500090206     C                   add       TASVLF        Wmptmt_r
093600090205     c                   endif
093700090205     c                   endsr
093800090204     C**********************************************************
093900090206     C** Riempie campi wfsmp00f
094000090204     C**********************************************************
094100090206     C     rie_campi     BEGSR
094200090204     c* profilo utente e data elaborazione
094300090204     c                   eval      wmpusr=knmus
094400090204     c                   eval      wmpdel=wdatael
094500090206     c*
094600090206     c                   eval      wmpall=§al
094700090204     c* area e descrizione area
094800090204     c                   eval      wmpcar=car(k)
094900090204     C                   MOVEL(P)  '05'          tblcod
095000090204     C                   MOVEL(P)  car(K)        tblkey
095100090204     C     KTAB          CHAIN     TABEL00F                           30
095200090204     C                   IF        %found(tabel00f) and tblflg=*blanks
095300090204     C                   MOVEL     TBLUNI        DS05
095400090204     C                   MOVEL     §05DES        wmpdar
095500090204     C                   endif
095600090204     c* filiale,descrizione e livello filiale
095700090204     c                   eval      wmpdil=dil(K)
095800090206     c                   eval      wmpfil=fil(K)
095900090206     c                   eval      wmplif=lpo(K)
096000090204     c* tipo filiale Italia/Estero
096100090205     c                   eval      wmptif=tif(K)
096200090206     c* spedizioni,colli, kg,volume
096300090206     c                   if        wmptip='G'
096400090206     c                   z-add     wmptsp_G      wmptsp
096500090206     c                   z-add     wmptcp_G      wmptcp
096600090206     c                   z-add(h)  wmptkp_G      wmptkp
096700090206     c                   z-add(h)  wmptmp_G      wmptmp
096800090710
096900090206     c                   z-add     wmptsa_G      wmptsa
097000090206     c                   z-add     wmptca_G      wmptca
097100090206     c                   z-add(h)  wmptka_G      wmptka
097200090206     c                   z-add(h)  wmptma_G      wmptma
097300090710
097400090206     c                   z-add     wmptspt_G     wmptspt
097500090206     c                   z-add     wmptcpt_G     wmptcpt
097600090206     c                   z-add(h)  wmptkpt_G     wmptkpt
097700090206     c                   z-add(h)  wmptmpt_G     wmptmpt
097800090710
097900090710     c                   z-add     wmptsptc_G    wmptsptc
098000090710     c                   z-add     wmptcptc_G    wmptcptc
098100090710     c                   z-add(h)  wmptkptc_G    wmptkptc
098200090710     c                   z-add(h)  wmptmptc_G    wmptmptc
098300090710
098400090206     c                   z-add     wmptsat_G     wmptsat
098500090206     c                   z-add     wmptcat_G     wmptcat
098600090206     c                   z-add(h)  wmptkat_G     wmptkat
098700090206     c                   z-add(h)  wmptmat_G     wmptmat
098800090710
098900090710     c                   z-add     wmptsatc_G    wmptsatc
099000090710     c                   z-add     wmptcatc_G    wmptcatc
099100090710     c                   z-add(h)  wmptkatc_G    wmptkatc
099200090710     c                   z-add(h)  wmptmatc_G    wmptmatc
099300090710
099400090206     c                   z-add     wmptst_G      wmptst
099500090206     c                   z-add     wmptct_G      wmptct
099600090206     c                   z-add(h)  wmptkt_G      wmptkt
099700090206     c                   z-add(h)  wmptmt_G      wmptmt
099800090206     c                   else
099900090206     c                   z-add     wmptsp_r      wmptsp
100000090206     c                   z-add     wmptcp_r      wmptcp
100100090206     c                   z-add(h)  wmptkp_r      wmptkp
100200090206     c                   z-add(h)  wmptmp_r      wmptmp
100300090710
100400090206     c                   z-add     wmptsa_r      wmptsa
100500090206     c                   z-add     wmptca_r      wmptca
100600090206     c                   z-add(h)  wmptka_r      wmptka
100700090206     c                   z-add(h)  wmptma_r      wmptma
100800090710
100900090206     c                   z-add     wmptspt_r     wmptspt
101000090206     c                   z-add     wmptcpt_r     wmptcpt
101100090206     c                   z-add(h)  wmptkpt_r     wmptkpt
101200090206     c                   z-add(h)  wmptmpt_r     wmptmpt
101300090710
101400090710     c                   z-add     wmptsptc_r    wmptsptc
101500090710     c                   z-add     wmptcptc_r    wmptcptc
101600090710     c                   z-add(h)  wmptkptc_r    wmptkptc
101700090710     c                   z-add(h)  wmptmptc_r    wmptmptc
101800090710
101900090206     c                   z-add     wmptsat_r     wmptsat
102000090206     c                   z-add     wmptcat_r     wmptcat
102100090206     c                   z-add(h)  wmptkat_r     wmptkat
102200090206     c                   z-add(h)  wmptmat_r     wmptmat
102300090710
102400090710     c                   z-add     wmptsatc_r    wmptsatc
102500090710     c                   z-add     wmptcatc_r    wmptcatc
102600090710     c                   z-add(h)  wmptkatc_r    wmptkatc
102700090710     c                   z-add(h)  wmptmatc_r    wmptmatc
102800090710
102900090206     c                   z-add     wmptst_r      wmptst
103000090206     c                   z-add     wmptct_r      wmptct
103100090206     c                   z-add(h)  wmptkt_r      wmptkt
103200090206     c                   z-add(h)  wmptmt_r      wmptmt
103300090710
103400090206     c                   endif
103500090204     c                   endsr
103600090204     C**********************************************************
103700120905     C** Elaborazione per file WFLSS60f
103800090204     C**********************************************************
103900090204     C     sr_wslss      BEGSR
104000090204     C                   SETOFF                                       545556
104100090204     C                   SETOFF                                       48  52
104200111129     c
104300121123     c
104400111129     c                   z-add     1             kk
104500111129     c     taslnp        lookup    fil(kk)                                98
104600111129     c
104700111129     c                   if        *in98
104800111129     c* Euroexpress o Fedex o DPD
104900111129     c                   if        tif(kk) ='F' or tif(kk)='X' or tif(kk)='D'
105000111129     c                   seton                                        54
105100111129     c                   endif
105200111129     c
105300111129     C**   TASLNP        COMP      300                                54        ESTERO
105400111129     C*+N54TASLNP        COMP      190                                    54    + DPD
105500111129     C**N54TASLNP        COMP      191                                    54    + DPD
105600111129     C**N54TASLNP        COMP      192                                    54    + DPD
105700111129     C**N54TASLNP        COMP      193                                    54    + DPD
105800111129     C**N54TASLNP        COMP      194                                    54    + DPD
105900111129     C**N54TASLNP        COMP      195                                    54    + DPD
106000111129     C**N54TASLNP        COMP      196                                    54    + DPD
106100111129     C**N54TASLNP        COMP      197                                    54    + DPD
106200090204     C*
106300100402     C****     TASLNP        COMP      180                                    48     POSTE
106400090204     C*
106500111129     c                   endif
106600111129     c***
106700121123     c* L I N E A   D I   A R R I V O
106800111129     c***
106900111129     c                   z-add     1             kk
107000111129     c     taslna        lookup    fil(kk)                                98
107100111129     c
107200111129     c                   if        *in98
107300111129     c* Euroexpress o Fedex o DPD
107400111129     c                   if        tif(kk) ='F' or tif(kk)='X' or tif(kk)='D'
107500111129     c                   seton                                        55
107600111129     c                   endif
107700111129     C*
107800111129     C**   TASLNA        COMP      300                                55
107900111129     C**N55TASLNA        COMP      190                                    55    + DPD
108000111129     C**N55TASLNA        COMP      191                                    55    + DPD
108100111129     C**N55TASLNA        COMP      192                                    55    + DPD
108200111129     C*+N55TASLNA        COMP      193                                    55    + DPD
108300111129     C**N55TASLNA        COMP      194                                    55    + DPD
108400111129     C**N55TASLNA        COMP      195                                    55    + DPD
108500111129     C**N55TASLNA        COMP      196                                    55    + DPD
108600111129     C*+N55TASLNA        COMP      197                                    55    + DPD
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
109900111129     c
110000111129     C**   TASLNA        COMP      190                                    56     DPD
110100111129     C**N56TASLNA        COMP      191                                    56     DPD
110200111129     C**N56TASLNA        COMP      192                                    56     DPD
110300111129     C**N56TASLNA        COMP      193                                    56     DPD
110400111129     C**N56TASLNA        COMP      194                                    56     DPD
110500111129     C**N56TASLNA        COMP      195                                    56     DPD
110600111129     C**N56TASLNA        COMP      196                                    56     DPD
110700111129     C**N56TASLNA        COMP      197                                    56     DPD
110800091006     c*
110900111129     c                   endif
111000121123     c
111100111129     c
111200091006     c* Verifico se si tratta di cliente extra budget --> 35 on
111300121121    1c                   if        §stat=' '
111400121121    2c                   if        tasccm<>savccm or tasctr<>savctr
111500121123     c                   z-add     1             EE
111600091007     c                   eval      w010a=%editc(tasccm:'X')+%editc(tasctr:'X')
111700091007     c     w010a         lookup    ksc_EX(EE)                             35
111800121121    3c                   if        not *in35
111900091007     c                   eval      %subst(w010a:8:3)='   '
112000091007     c     w010a         lookup    ksc_EX(EE)                             35
112100121121    3c                   endif
112200091006
112300091006     c                   eval      savccm=tasccm
112400091006     c                   eval      savctr=tasctr
112500121121    2c                   endif
112600110711     c
112700121121   x1c                   else
112800121121     c
112900121121     c* Per i clienti Particolari devo controllare il terminal della bolla se è quello
113000121121     c*   previsto
113100121121    2c                   if        tasccm<>savccm
113200121123     c                   z-add     1             EE
113300121121     c                   eval      w010a=%editc(tasccm:'X')+'   '
113400121121     c     w010a         lookup    ksc_EX(EE)                             35
113500121121     c
113600121121     c* E' impossibile che non lo trovi, ma se per caso succede, scarto la bolla
113700121121    3c                   if        *in35
113800121121     c                   eval      savccm=tasccm
113900121121   x3c                   else
114000121121     c                   clear                   savccm
114100121121    3c                   endif
114200121121    2c                   endif
114300121121     c
114400121121     c                   setoff                                       35
114500121123     c*
114600121123     c                   clear                   prtc_lnp          3 0
114700121123     c                   clear                   prtc_lna          3 0
114800121121     c
114900121121    2c                   if        savccm>0
115000121123     c                   select
115100121123     c*  cliente momentaneamente bloccato
115200121123    3c                   when      ksc_tr(EE)=888
115300121123     c* abilitato a tutto:  memorizzo per qualsiasi terminal
115400121123    3c                   when      ksc_tr(EE)=999
115500121121     c                   seton                                        35
115600121123     c                   eval      prtc_lnp=tastfp
115700121123     c                   eval      prtc_lna=tastfa
115800121123   x3c                   other
115900121123     c
116000121123     c* Abilito solo il terminal di partenza o di arrivo previsto dalla tabella
116100121123    4c                   if        tastfp=ksc_tr(EE)
116200121121     c                   seton                                        35
116300121123     c                   eval      prtc_lnp=tastfp
116400121121    4c                   endif
116500121123    c
116600121123    4c                   if        tastfa=ksc_tr(EE)
116700121123     c                   eval      prtc_lna=tastfa
116800121123     c                   seton                                        35
116900121123    4c                   endif
117000121123     c
117100121123    3c                   endsl
117200121121     c*
117300121121    2c                   endif
117400110711     c
117500121121    1c                   endif
117600121121     c
117700121121     c* Per i clienti Particolari --> 35 off bolla non da elaborare esco dal ciclo
117800121121     c*   visto che l'elaborazione è specifica solo per questi clienti
117900121121     c                   if        §stat='P' and not *in35
118000121121     c                   leavesr
118100121121     c                   endif
118200090204     C*
118300121123    1c                   if        §stat=' '
118400121123     c                   eval      prtc_lnp=taslnp
118500090204     C                   Z-ADD     1             K
118600090204     C     TASLNP        LOOKUP    FIL(K)                                 58
118700121123     c                   else
118800121123     c
118900121123     c* solo si tratta di terminal di partenza elaborabile procedo
119000121123     c                   if        prtc_lnp>0
119100121123     C                   Z-ADD     1             K
119200121123     C     prtc_lnp      LOOKUP    FIL(K)                                 58
119300121123     c                   endif
119400121123     c                   endif
119500121123     c
119600121123    1c                   if        prtc_lnp>0
119700121123     c
119800090204     C  N58              EXCEPT    ERR
119900090204     C     K             OCCUR     DSFIL
120000100402     C     K             OCCUR     DSFIL_e
120100100402     C     K             OCCUR     DSFIL_h
120200121123     c
120300090204     C  N54
120400090204     CANN55              DO                                                     ITALIA
120500100402     c* Spedizioni LNP Italia LNA Italia
120600100402     c                   exsr      speITA
120700090204     C                   ENDDO
120800100402     c
120900100402     c* Spedizioni IMPORT --> LNP Italia
121000090204     C   54              DO                                                     IMPORT
121100100402     c                   exsr      speIMPORT
121200090204     C                   ENDDO
121300100402     c
121400100402     c* Spedizioni lna EUROEXPRESS
121500090204     C   55
121600090204     CANN56
121700090204     CANN52              DO                                                     EXPORT
121800100402     c                   exsr      speEXPRESS
121900090204     C                   ENDDO
122000100402     c
122100100402     c* Spedizioni lna DPD
122200090204     C   55
122300090204     CAN 56              DO                                                     EXPORT
122400100402     c                   exsr      speDPD
122500090204     C                   ENDDO
122600100402     c
122700100402     c* Spedizioni lna FEDEX
122800090204     C   55
122900090204     CAN 52              DO                                                     EXPORT
123000100402     c                   exsr      speFEDEX
123100090204     C                   ENDDO
123200121123     c
123300121123    1c                   endif
123400100402     c
123500090204     C* MEMORIZZAZIONE PER LINEA ARRIVO
123600121123    1c                   if        §stat=' '
123700121123     c                   eval      prtc_lna=taslna
123800090204     C                   Z-ADD     1             K
123900090204     C     TASLNA        LOOKUP    FIL(K)                                 58
124000121123     c                   else
124100121123     c
124200121123     c* solo si tratta di terminal di arrivo elaborabile procedo
124300121123     c                   if        prtc_lna>0
124400121123     C                   Z-ADD     1             K
124500121123     C     prtc_lna      LOOKUP    FIL(K)                                 58
124600121123     c                   endif
124700121123     c                   endif
124800121123     c
124900121123    1c                   if        prtc_lna>0
125000121123     c
125100090204     C  N58              EXCEPT    ERR
125200090204     C     K             OCCUR     DSFIL
125300100402     C     K             OCCUR     DSFIL_e
125400100402     C     K             OCCUR     DSFIL_h
125500100402     c
125600090204     C  N54
125700100402     CANN55              DO
125800100402     c* Spedizioni LNP Italia LNA Italia
125900100402     c                   exsr      speITA_LNA
126000090204     C                   ENDDO
126100100402     c
126200100402     c* spedizioni EXPORT --> lna estera
126300090204     C   55              DO                                                     ESTERO ASS.+FRA.
126400100402     c                   exsr      speEXPORT
126500090204     C                   ENDDO
126600121123     c
126700121123     c                   endif
126800121123     c
126900090204     c                   endsr
127000090206     C**********************************************************
127100090206     C** scarica ds multiple su file
127200090206     C**********************************************************
127300090206     C     scrive_file   BEGSR
127400090206     c*
127500090206     C                   Z-ADD     §AL           WSSALL
127600090206     c*
127700110711    1C                   DO        400           K
127800090206     C     K             OCCUR     DSFIL_smp
127900090206     C     K             OCCUR     DSFIL
128000100402     C     K             OCCUR     DSFIL_e
128100100402     C     K             OCCUR     DSFIL_h
128200110711     c
128300110711    2c                   if        §stat=' '
128400090206     c* WFSMP
128500090206     c* giornaliero
128600090601     c* Lo scrivo solo per tipo "G" o "R"
128700090601     c                   if        Wtipo='G' or Wtipo='R'
128800090206     c                   if        (wmptsp_g>0 or wmptsa_g>0 or
128900090206     c                              wmptspt_g>0 or wmptsat_g>0 or
129000090206     c                              wmptst_g>0)
129100090206     c                   z-add     §al           wmpdal
129200090206     c                   eval      wmptip='G'
129300090206     c                   exsr      rie_campi
129400090206     c                   write     wfsmp000
129500090206     c                   endif
129600090601     c                   endif
129700090601     c
129800090601     c* riepilogo /precedente
129900090601     c* Lo scrivo solo per tipo "R"  se "P" lo scrivo con "P"
130000090601     c                   if        Wtipo='P' or Wtipo='R'
130100090206     c                   if        (wmptsp_r>0 or wmptsa_r>0 or
130200090206     c                              wmptspt_r>0 or wmptsat_r>0 or
130300090206     c                              wmptst_r>0)
130400090206     c                   z-add     §dal          wmpdal
130500090601     c                   if        Wtipo='R'
130600090206     c                   eval      wmptip='R'
130700090601     c                   else
130800090601     c                   eval      wmptip='P'
130900090601     c                   endif
131000090206     c                   exsr      rie_campi
131100090206     c                   write     wfsmp000
131200090206     c                   endif
131300090601     c                   endif
131400110711     c
131500110711    2c                   endif
131600120905     c* WFLSS60F
131700120904     c                   clear                   wsskgf
131800090206     c* giornaliero
131900090601     c                   if        Wtipo='G' or Wtipo='R'
132000110711     c
132100110711    2c                   if        §stat=' '
132200090601     c
132300090206     C     WTSPIP        IFGT      *ZERO
132400090206     C                   Z-ADD     §al           WSSDAL
132500090206     c                   movel     'G'           WSSTIE
132600090206     C                   MOVE      'IP'          WSSTIP
132700090206     C                   Z-ADD     FIL(K)        WSSLIN
132800090206     C                   Z-ADD     WTSPIP        WSSTSP
132900090206     C                   Z-ADD     WTCLIP        WSSTCL
133000090206     C                   Z-ADD(H)  WTKGIP        WSSTKG
133100090206     C                   Z-ADD(H)  WTMCIP        WSSTMC
133200120904     c*old
133300120904     C                   Z-ADD(H)  WTKGIP        WSSTKG_ol
133400120904     C                   Z-ADD(H)  WTMCIP        WSSTMC_ol
133500100402     c* spedizioni "E"
133600100402     C                   Z-ADD     eWTSPIP       WSSTSPe
133700100402     C                   Z-ADD     eWTCLIP       WSSTCLe
133800100402     C                   Z-ADD(H)  eWTKGIP       WSSTKGe
133900100402     C                   Z-ADD(H)  eWTMCIP       WSSTMCe
134000120904     c* old
134100120904     C                   Z-ADD(H)  eWTKGIP       WSSTKGe_ol
134200120904     C                   Z-ADD(H)  eWTMCIP       WSSTMCe_ol
134300100402     c* spedizioni "H"
134400100402     C                   Z-ADD     hWTSPIP       WSSTSPh
134500100402     C                   Z-ADD     hWTCLIP       WSSTCLh
134600100402     C                   Z-ADD(H)  hWTKGIP       WSSTKGh
134700100402     C                   Z-ADD(H)  hWTMCIP       WSSTMCh
134800120904     c* old
134900120904     C                   Z-ADD(H)  hWTKGIP       WSSTKGh_ol
135000120904     C                   Z-ADD(H)  hWTMCIP       WSSTMCh_ol
135100100402
135200120905     C                   WRITE     WFLSS600
135300120904     C                   WRITE     WSLSS500
135400090206     C                   ENDIF
135500100402
135600090206     C     WTSPIA        IFGT      *ZERO
135700090206     C                   Z-ADD     §al           WSSDAL
135800090206     c                   movel     'G'           WSSTIE
135900090206     C                   MOVE      'IA'          WSSTIP
136000090206     C                   Z-ADD     FIL(K)        WSSLIN
136100090206     C                   Z-ADD     WTSPIA        WSSTSP
136200090206     C                   Z-ADD     WTCLIA        WSSTCL
136300090206     C                   Z-ADD(H)  WTKGIA        WSSTKG
136400090206     C                   Z-ADD(H)  WTMCIA        WSSTMC
136500120904     c* old
136600120904     C                   Z-ADD(H)  WTKGIA        WSSTKG_ol
136700120904     C                   Z-ADD(H)  WTMCIA        WSSTMC_ol
136800100402     c* spedizioni "E"
136900100402     C                   Z-ADD     eWTSPIA       WSSTSPe
137000100402     C                   Z-ADD     eWTCLIA       WSSTCLe
137100100402     C                   Z-ADD(H)  eWTKGIA       WSSTKGe
137200100402     C                   Z-ADD(H)  eWTMCIA       WSSTMCe
137300120904     c* old
137400120904     C                   Z-ADD(H)  eWTKGIA       WSSTKGe_ol
137500120904     C                   Z-ADD(H)  eWTMCIA       WSSTMCe_ol
137600100402     c* spedizioni "H"
137700100402     C                   Z-ADD     hWTSPIA       WSSTSPh
137800100402     C                   Z-ADD     hWTCLIA       WSSTCLh
137900100402     C                   Z-ADD(H)  hWTKGIA       WSSTKGh
138000100402     C                   Z-ADD(H)  hWTMCIA       WSSTMCh
138100120904     c* old
138200120904     C                   Z-ADD(H)  hWTKGIA       WSSTKGh_ol
138300120904     C                   Z-ADD(H)  hWTMCIA       WSSTMCh_ol
138400100402     c
138500120905     C                   WRITE     WFLSS600
138600120904     C                   WRITE     WSLSS500
138700090206     C                   ENDIF
138800100402     c
138900090206     C     WTSPEI        IFGT      *ZERO
139000090206     C                   Z-ADD     §al           WSSDAL
139100090206     c                   movel     'G'           WSSTIE
139200090206     C                   MOVE      'EI'          WSSTIP
139300090206     C                   Z-ADD     FIL(K)        WSSLIN
139400090206     C                   Z-ADD     WTSPEI        WSSTSP
139500090206     C                   Z-ADD     WTCLEI        WSSTCL
139600090206     C                   Z-ADD(H)  WTKGEI        WSSTKG
139700090206     C                   Z-ADD(H)  WTMCEI        WSSTMC
139800120904     c* old
139900120904     C                   Z-ADD(H)  WTKGEI        WSSTKG_ol
140000120904     C                   Z-ADD(H)  WTMCEI        WSSTMC_ol
140100100402     c* spedizioni "E"
140200100402     C                   Z-ADD     eWTSPEI       WSSTSPe
140300100402     C                   Z-ADD     eWTCLEI       WSSTCLe
140400100402     C                   Z-ADD(H)  eWTKGEI       WSSTKGe
140500100402     C                   Z-ADD(H)  eWTMCEI       WSSTMCe
140600120904     c* old
140700120904     C                   Z-ADD(H)  eWTKGEI       WSSTKGe_ol
140800120904     C                   Z-ADD(H)  eWTMCEI       WSSTMCe_ol
140900100402     c* spedizioni "H"
141000100402     C                   Z-ADD     hWTSPEI       WSSTSPh
141100100402     C                   Z-ADD     hWTCLEI       WSSTCLh
141200100402     C                   Z-ADD(H)  hWTKGEI       WSSTKGh
141300100402     C                   Z-ADD(H)  hWTMCEI       WSSTMCh
141400120904     c* old
141500120904     C                   Z-ADD(H)  hWTKGEI       WSSTKGh_ol
141600120904     C                   Z-ADD(H)  hWTMCEI       WSSTMCh_ol
141700100402     c
141800120905     C                   WRITE     WFLSS600
141900120904     C                   WRITE     WSLSS500
142000120904     C                   ENDIF
142100100402     c
142200090206     C     WTSPEE        IFGT      *ZERO
142300090206     C                   Z-ADD     §al           WSSDAL
142400090206     c                   movel     'G'           WSSTIE
142500090206     C                   MOVE      'EE'          WSSTIP
142600090206     C                   Z-ADD     FIL(K)        WSSLIN
142700090206     C                   Z-ADD     WTSPEE        WSSTSP
142800090206     C                   Z-ADD     WTCLEE        WSSTCL
142900090206     C                   Z-ADD(H)  WTKGEE        WSSTKG
143000090206     C                   Z-ADD(H)  WTMCEE        WSSTMC
143100120904     c* old
143200120904     C                   Z-ADD(H)  WTKGEE        WSSTKG_ol
143300120904     C                   Z-ADD(H)  WTMCEE        WSSTMC_ol
143400100402     c* spedizioni "E"
143500100402     C                   Z-ADD     eWTSPEE       WSSTSPe
143600100402     C                   Z-ADD     eWTCLEE       WSSTCLe
143700100402     C                   Z-ADD(H)  eWTKGEE       WSSTKGe
143800100402     C                   Z-ADD(H)  eWTMCEE       WSSTMCe
143900120904     c* old
144000120904     C                   Z-ADD(H)  eWTKGEE       WSSTKGe_ol
144100120904     C                   Z-ADD(H)  eWTMCEE       WSSTMCe_ol
144200100402     c* spedizioni "H"
144300100402     C                   Z-ADD     hWTSPEE       WSSTSPh
144400100402     C                   Z-ADD     hWTCLEE       WSSTCLh
144500100402     C                   Z-ADD(H)  hWTKGEE       WSSTKGh
144600100402     C                   Z-ADD(H)  hWTMCEE       WSSTMCh
144700120904     c* old
144800120904     C                   Z-ADD(H)  hWTKGEE       WSSTKGh_ol
144900120904     C                   Z-ADD(H)  hWTMCEE       WSSTMCh_ol
145000100402     c
145100120905     C                   WRITE     WFLSS600
145200120904     C                   WRITE     WSLSS500
145300090206     C                   ENDIF
145400090206     C*
145500090206     C     WTSPED        IFGT      *ZERO
145600090206     C                   Z-ADD     §al           WSSDAL
145700090206     c                   movel     'G'           WSSTIE
145800090206     C                   MOVE      'ED'          WSSTIP
145900090206     C                   Z-ADD     FIL(K)        WSSLIN
146000090206     C                   Z-ADD     WTSPED        WSSTSP
146100090206     C                   Z-ADD     WTCLED        WSSTCL
146200090206     C                   Z-ADD(H)  WTKGED        WSSTKG
146300090206     C                   Z-ADD(H)  WTMCED        WSSTMC
146400120904     c* old
146500120904     C                   Z-ADD(H)  WTKGED        WSSTKG_ol
146600120904     C                   Z-ADD(H)  WTMCED        WSSTMC_ol
146700100402     c* spedizioni "E"
146800100402     C                   Z-ADD     eWTSPED       WSSTSPe
146900100402     C                   Z-ADD     eWTCLED       WSSTCLe
147000100402     C                   Z-ADD(H)  eWTKGED       WSSTKGe
147100100402     C                   Z-ADD(H)  eWTMCED       WSSTMCe
147200120904     c* old
147300120904     C                   Z-ADD(H)  eWTKGED       WSSTKGe_ol
147400120904     C                   Z-ADD(H)  eWTMCED       WSSTMCe_ol
147500100402     c* spedizioni "H"
147600100402     C                   Z-ADD     hWTSPED       WSSTSPh
147700100402     C                   Z-ADD     hWTCLED       WSSTCLh
147800100402     C                   Z-ADD(H)  hWTKGED       WSSTKGh
147900100402     C                   Z-ADD(H)  hWTMCED       WSSTMCh
148000120904     c* old
148100120904     C                   Z-ADD(H)  hWTKGED       WSSTKGh_ol
148200120904     C                   Z-ADD(H)  hWTMCED       WSSTMCh_ol
148300100402     c
148400120905     C                   WRITE     WFLSS600
148500120904     C                   WRITE     WSLSS500
148600090206     C                   ENDIF
148700090206     C*
148800090206     C     WTSPEX        IFGT      *ZERO
148900090206     C                   Z-ADD     §al           WSSDAL
149000090206     c                   movel     'G'           WSSTIE
149100090206     C                   MOVE      'EX'          WSSTIP
149200090206     C                   Z-ADD     FIL(K)        WSSLIN
149300090206     C                   Z-ADD     WTSPEX        WSSTSP
149400090206     C                   Z-ADD     WTCLEX        WSSTCL
149500090206     C                   Z-ADD(H)  WTKGEX        WSSTKG
149600090206     C                   Z-ADD(H)  WTMCEX        WSSTMC
149700120904     c* old
149800120904     C                   Z-ADD(H)  WTKGEX        WSSTKG_ol
149900120904     C                   Z-ADD(H)  WTMCEX        WSSTMC_ol
150000100402     c* spedizioni "E"
150100100402     C                   Z-ADD     eWTSPEX       WSSTSPe
150200100402     C                   Z-ADD     eWTCLEX       WSSTCLe
150300100402     C                   Z-ADD(H)  eWTKGEX       WSSTKGe
150400100402     C                   Z-ADD(H)  eWTMCEX       WSSTMCe
150500120904     c* old
150600120904     C                   Z-ADD(H)  eWTKGEX       WSSTKGe_ol
150700120904     C                   Z-ADD(H)  eWTMCEX       WSSTMCe_ol
150800100402     c* spedizioni "H"
150900100402     C                   Z-ADD     hWTSPEX       WSSTSPh
151000100402     C                   Z-ADD     hWTCLEX       WSSTCLh
151100100402     C                   Z-ADD(H)  hWTKGEX       WSSTKGh
151200100402     C                   Z-ADD(H)  hWTMCEX       WSSTMCh
151300120904     c* old
151400120904     C                   Z-ADD(H)  hWTKGEX       WSSTKGh_ol
151500120904     C                   Z-ADD(H)  hWTMCEX       WSSTMCh_ol
151600100402     c
151700120905     C                   WRITE     WFLSS600
151800120904     C                   WRITE     WSLSS500
151900090206     C                   ENDIF
152000090206     C*
152100090206     C     WTSPEA        IFGT      *ZERO
152200090206     C                   Z-ADD     §al           WSSDAL
152300090206     c                   movel     'G'           WSSTIE
152400090206     C                   MOVE      'EA'          WSSTIP
152500090206     C                   Z-ADD     FIL(K)        WSSLIN
152600090206     C                   Z-ADD     WTSPEA        WSSTSP
152700090206     C                   Z-ADD     WTCLEA        WSSTCL
152800090206     C                   Z-ADD(H)  WTKGEA        WSSTKG
152900090206     C                   Z-ADD(H)  WTMCEA        WSSTMC
153000120904     c* old
153100120904     C                   Z-ADD(H)  WTKGEA        WSSTKG_ol
153200120904     C                   Z-ADD(H)  WTMCEA        WSSTMC_ol
153300100402     c* spedizioni "E"
153400100402     C                   Z-ADD     eWTSPEA       WSSTSPe
153500100402     C                   Z-ADD     eWTCLEA       WSSTCLe
153600100402     C                   Z-ADD(H)  eWTKGEA       WSSTKGe
153700100402     C                   Z-ADD(H)  eWTMCEA       WSSTMCe
153800120904     c* old
153900120904     C                   Z-ADD(H)  eWTKGEA       WSSTKGe_ol
154000120904     C                   Z-ADD(H)  eWTMCEA       WSSTMCe_ol
154100100402     c* spedizioni "H"
154200100402     C                   Z-ADD     hWTSPEA       WSSTSPh
154300100402     C                   Z-ADD     hWTCLEA       WSSTCLh
154400100402     C                   Z-ADD(H)  hWTKGEA       WSSTKGh
154500100402     C                   Z-ADD(H)  hWTMCEA       WSSTMCh
154600120904     c* old
154700120904     C                   Z-ADD(H)  hWTKGEA       WSSTKGh_ol
154800120904     C                   Z-ADD(H)  hWTMCEA       WSSTMCh_ol
154900100402     c
155000120905     C                   WRITE     WFLSS600
155100120904     C                   WRITE     WSLSS500
155200090206     C                   ENDIF
155300090206     C*
155400090206     C     WTSPPA        IFGT      *ZERO
155500090206     C                   Z-ADD     §al           WSSDAL
155600090206     c                   movel     'G'           WSSTIE
155700090206     C                   MOVE      'PA'          WSSTIP
155800090206     C                   Z-ADD     FIL(K)        WSSLIN
155900090206     C                   Z-ADD     WTSPPA        WSSTSP
156000090206     C                   Z-ADD     WTCLPA        WSSTCL
156100090206     C                   Z-ADD(H)  WTKGPA        WSSTKG
156200090206     C                   Z-ADD(H)  WTMCPA        WSSTMC
156300120904     c* old
156400120904     C                   Z-ADD(H)  WTKGPA        WSSTKG_ol
156500120904     C                   Z-ADD(H)  WTMCPA        WSSTMC_ol
156600100402     c* spedizioni "E"
156700100402     C                   Z-ADD     eWTSPPA       WSSTSPe
156800100402     C                   Z-ADD     eWTCLPA       WSSTCLe
156900100402     C                   Z-ADD(H)  eWTKGPA       WSSTKGe
157000100402     C                   Z-ADD(H)  eWTMCPA       WSSTMCe
157100120904     c* old
157200120904     C                   Z-ADD(H)  eWTKGPA       WSSTKGe_ol
157300120904     C                   Z-ADD(H)  eWTMCPA       WSSTMCe_ol
157400100402     c* spedizioni "H"
157500100402     C                   Z-ADD     hWTSPPA       WSSTSPh
157600100402     C                   Z-ADD     hWTCLPA       WSSTCLh
157700100402     C                   Z-ADD(H)  hWTKGPA       WSSTKGh
157800100402     C                   Z-ADD(H)  hWTMCPA       WSSTMCh
157900120904     c* old
158000120904     C                   Z-ADD(H)  hWTKGPA       WSSTKGh_ol
158100120904     C                   Z-ADD(H)  hWTMCPA       WSSTMCh_ol
158200100402     c
158300120905     C                   WRITE     WFLSS600
158400120904     C                   WRITE     WSLSS500
158500090206     C                   ENDIF
158600110711     c
158700110711     c                   endif
158800110715     c* Extra budget o clienti particolari
158900110715     c                   if        §stat=' '
159000110715     c                   movel     'G'           WSSTIE
159100110715     c                   else
159200110715     c                   movel     'A'           WSSTIE
159300110715     c                   endif
159400110715     c
159500091006     C     WTSPIP_e      IFGT      *ZERO
159600091007     C                   Z-ADD     §al           WSSDAL
159700091006     C                   MOVE      'IP'          WSSTIP
159800091006     C                   Z-ADD     FIL(K)        WSSLIN
159900091006     C                   Z-ADD     WTSPIP_e      WSSTSP
160000091006     C                   Z-ADD     WTCLIP_e      WSSTCL
160100120904     C                   Z-ADD(H)  WTKGIP_e      WSSTKG_ex
160200120904     C                   Z-ADD(H)  WTMCIP_e      WSSTMC_ex
160300100402     c* spedizioni "E"
160400100402     C                   Z-ADD     eWTSPIP_e     WSSTSPe
160500100402     C                   Z-ADD     eWTCLIP_e     WSSTCLe
160600120904     C                   Z-ADD(H)  eWTKGIP_e     WSSTKGe_ex
160700120904     C                   Z-ADD(H)  eWTMCIP_e     WSSTMCe_ex
160800100402     c* spedizioni "H"
160900100402     C                   Z-ADD     hWTSPIP_e     WSSTSPh
161000100402     C                   Z-ADD     hWTCLIP_e     WSSTCLh
161100120904     C                   Z-ADD(H)  hWTKGIP_e     WSSTKGh_ex
161200120904     C                   Z-ADD(H)  hWTMCIP_e     WSSTMCh_ex
161300100402     c
161400120903     C                   WRITE     wslss550
161500091006     C                   ENDIF
161600100402     c
161700091006     C     WTSPIA_e      IFGT      *ZERO
161800091007     C                   Z-ADD     §al           WSSDAL
161900110715     c* Es - non importa riscrivere il falg: non da clear e già impostato sopra
162000110715     c*****              movel     'G'           WSSTIE
162100091006     C                   MOVE      'IA'          WSSTIP
162200091006     C                   Z-ADD     FIL(K)        WSSLIN
162300091006     C                   Z-ADD     WTSPIA_e      WSSTSP
162400091006     C                   Z-ADD     WTCLIA_e      WSSTCL
162500120904     C                   Z-ADD(H)  WTKGIA_e      WSSTKG_ex
162600120904     C                   Z-ADD(H)  WTMCIA_e      WSSTMC_ex
162700100402     c* spedizioni "E"
162800100402     C                   Z-ADD     eWTSPIA_e     WSSTSPe
162900100402     C                   Z-ADD     eWTCLIA_e     WSSTCLe
163000120904     C                   Z-ADD(H)  eWTKGIA_e     WSSTKGe_ex
163100120904     C                   Z-ADD(H)  eWTMCIA_e     WSSTMCe_ex
163200100402     c* spedizioni "H"
163300100402     C                   Z-ADD     hWTSPIA_e     WSSTSPh
163400100402     C                   Z-ADD     hWTCLIA_e     WSSTCLh
163500120904     C                   Z-ADD(H)  hWTKGIA_e     WSSTKGh_ex
163600120904     C                   Z-ADD(H)  hWTMCIA_e     WSSTMCh_ex
163700100402     c
163800091006     C                   WRITE     WSLSS550
163900091006     C                   ENDIF
164000100402     c
164100091006     C     WTSPEI_e      IFGT      *ZERO
164200091007     C                   Z-ADD     §al           WSSDAL
164300110715     c******             movel     'G'           WSSTIE
164400091006     C                   MOVE      'EI'          WSSTIP
164500091006     C                   Z-ADD     FIL(K)        WSSLIN
164600091006     C                   Z-ADD     WTSPEI_e      WSSTSP
164700091006     C                   Z-ADD     WTCLEI_e      WSSTCL
164800120904     C                   Z-ADD(H)  WTKGEI_e      WSSTKG_ex
164900120904     C                   Z-ADD(H)  WTMCEI_e      WSSTMC_ex
165000100402     c* spedizioni "E"
165100100402     C                   Z-ADD     eWTSPEI_e     WSSTSPe
165200100402     C                   Z-ADD     eWTCLEI_e     WSSTCLe
165300120904     C                   Z-ADD(H)  eWTKGEI_e     WSSTKGe_ex
165400120904     C                   Z-ADD(H)  eWTMCEI_e     WSSTMCe_ex
165500100402     c* spedizioni "H"
165600100402     C                   Z-ADD     hWTSPEI_e     WSSTSPh
165700100402     C                   Z-ADD     hWTCLEI_e     WSSTCLh
165800120904     C                   Z-ADD(H)  hWTKGEI_e     WSSTKGh_ex
165900120904     C                   Z-ADD(H)  hWTMCEI_e     WSSTMCh_ex
166000100402     c
166100091006     C                   WRITE     WSLSS550
166200091006     C                   ENDIF
166300100402     c
166400091006     C     WTSPEE_e      IFGT      *ZERO
166500091007     C                   Z-ADD     §al           WSSDAL
166600110715     c******             movel     'G'           WSSTIE
166700091006     C                   MOVE      'EE'          WSSTIP
166800091006     C                   Z-ADD     FIL(K)        WSSLIN
166900091006     C                   Z-ADD     WTSPEE_e      WSSTSP
167000091006     C                   Z-ADD     WTCLEE_e      WSSTCL
167100120904     C                   Z-ADD(H)  WTKGEE_e      WSSTKG_ex
167200120904     C                   Z-ADD(H)  WTMCEE_e      WSSTMC_ex
167300100402     c* spedizioni "E"
167400100402     C                   Z-ADD     eWTSPEE_e     WSSTSPe
167500100402     C                   Z-ADD     eWTCLEE_e     WSSTCLe
167600120904     C                   Z-ADD(H)  eWTKGEE_e     WSSTKGe_ex
167700120904     C                   Z-ADD(H)  eWTMCEE_e     WSSTMCe_ex
167800100402     c* spedizioni "H"
167900100402     C                   Z-ADD     hWTSPEE_e     WSSTSPh
168000100402     C                   Z-ADD     hWTCLEE_e     WSSTCLh
168100120904     C                   Z-ADD(H)  hWTKGEE_e     WSSTKGh_ex
168200120904     C                   Z-ADD(H)  hWTMCEE_e     WSSTMCh_ex
168300100402     c
168400091006     C                   WRITE     WSLSS550
168500091006     C                   ENDIF
168600091006     C*
168700091006     C     WTSPED_e      IFGT      *ZERO
168800091007     C                   Z-ADD     §al           WSSDAL
168900110715     c*******            movel     'G'           WSSTIE
169000091006     C                   MOVE      'ED'          WSSTIP
169100091006     C                   Z-ADD     FIL(K)        WSSLIN
169200091006     C                   Z-ADD     WTSPED_e      WSSTSP
169300091006     C                   Z-ADD     WTCLED_e      WSSTCL
169400120904     C                   Z-ADD(H)  WTKGED_e      WSSTKG_ex
169500120904     C                   Z-ADD(H)  WTMCED_e      WSSTMC_ex
169600100402     c* spedizioni "E"
169700100402     C                   Z-ADD     eWTSPED_e     WSSTSPe
169800100402     C                   Z-ADD     eWTCLED_e     WSSTCLe
169900120904     C                   Z-ADD(H)  eWTKGED_e     WSSTKGe_ex
170000120904     C                   Z-ADD(H)  eWTMCED_e     WSSTMCe_ex
170100100402     c* spedizioni "H"
170200100402     C                   Z-ADD     hWTSPED_e     WSSTSPh
170300100402     C                   Z-ADD     hWTCLED_e     WSSTCLh
170400120904     C                   Z-ADD(H)  hWTKGED_e     WSSTKGh_ex
170500120904     C                   Z-ADD(H)  hWTMCED_e     WSSTMCh_ex
170600100402     c
170700091006     C                   WRITE     WSLSS550
170800091006     C                   ENDIF
170900091006     C*
171000091006     C     WTSPEX_e      IFGT      *ZERO
171100091007     C                   Z-ADD     §al           WSSDAL
171200110715     c*******            movel     'G'           WSSTIE
171300091006     C                   MOVE      'EX'          WSSTIP
171400091006     C                   Z-ADD     FIL(K)        WSSLIN
171500091006     C                   Z-ADD     WTSPEX_e      WSSTSP
171600091006     C                   Z-ADD     WTCLEX_e      WSSTCL
171700120904     C                   Z-ADD(H)  WTKGEX_e      WSSTKG_ex
171800120904     C                   Z-ADD(H)  WTMCEX_e      WSSTMC_ex
171900100402     c* spedizioni "E"
172000100402     C                   Z-ADD     eWTSPEX_e     WSSTSPe
172100100402     C                   Z-ADD     eWTCLEX_e     WSSTCLe
172200120904     C                   Z-ADD(H)  eWTKGEX_e     WSSTKGe_ex
172300120904     C                   Z-ADD(H)  eWTMCEX_e     WSSTMCe_ex
172400100402     c* spedizioni "H"
172500100402     C                   Z-ADD     hWTSPEX_e     WSSTSPh
172600100402     C                   Z-ADD     hWTCLEX_e     WSSTCLh
172700120904     C                   Z-ADD(H)  hWTKGEX_e     WSSTKGh_ex
172800120904     C                   Z-ADD(H)  hWTMCEX_e     WSSTMCh_ex
172900100402     c
173000091006     C                   WRITE     WSLSS550
173100091006     C                   ENDIF
173200091006     C*
173300091006     C     WTSPEA_e      IFGT      *ZERO
173400091007     C                   Z-ADD     §al           WSSDAL
173500110715     c******             movel     'G'           WSSTIE
173600091006     C                   MOVE      'EA'          WSSTIP
173700091006     C                   Z-ADD     FIL(K)        WSSLIN
173800091006     C                   Z-ADD     WTSPEA_e      WSSTSP
173900091006     C                   Z-ADD     WTCLEA_e      WSSTCL
174000120904     C                   Z-ADD(H)  WTKGEA_e      WSSTKG_ex
174100120904     C                   Z-ADD(H)  WTMCEA_e      WSSTMC_ex
174200100402     c* spedizioni "E"
174300100402     C                   Z-ADD     eWTSPEA_e     WSSTSPe
174400100402     C                   Z-ADD     eWTCLEA_e     WSSTCLe
174500120904     C                   Z-ADD(H)  eWTKGEA_e     WSSTKGe_ex
174600120904     C                   Z-ADD(H)  eWTMCEA_e     WSSTMCe_ex
174700100402     c* spedizioni "H"
174800100402     C                   Z-ADD     hWTSPEA_e     WSSTSPh
174900100402     C                   Z-ADD     hWTCLEA_e     WSSTCLh
175000120904     C                   Z-ADD(H)  hWTKGEA_e     WSSTKGh_ex
175100120904     C                   Z-ADD(H)  hWTMCEA_e     WSSTMCh_ex
175200100402     c
175300091006     C                   WRITE     WSLSS550
175400091006     C                   ENDIF
175500091006     C*
175600091006     C     WTSPPA_e      IFGT      *ZERO
175700091007     C                   Z-ADD     §al           WSSDAL
175800110715     c*******            movel     'G'           WSSTIE
175900091006     C                   MOVE      'PA'          WSSTIP
176000091006     C                   Z-ADD     FIL(K)        WSSLIN
176100091006     C                   Z-ADD     WTSPPA_e      WSSTSP
176200091006     C                   Z-ADD     WTCLPA_e      WSSTCL
176300120904     C                   Z-ADD(H)  WTKGPA_e      WSSTKG_ex
176400120904     C                   Z-ADD(H)  WTMCPA_e      WSSTMC_ex
176500100402     c* spedizioni "E"
176600100402     C                   Z-ADD     eWTSPPA_e     WSSTSPe
176700100402     C                   Z-ADD     eWTCLPA_e     WSSTCLe
176800120904     C                   Z-ADD(H)  eWTKGPA_e     WSSTKGe_ex
176900120904     C                   Z-ADD(H)  eWTMCPA_e     WSSTMCe_ex
177000100402     c* spedizioni "H"
177100100402     C                   Z-ADD     hWTSPPA_e     WSSTSPh
177200100402     C                   Z-ADD     hWTCLPA_e     WSSTCLh
177300120904     C                   Z-ADD(H)  hWTKGPA_e     WSSTKGh_ex
177400120904     C                   Z-ADD(H)  hWTMCPA_e     WSSTMCh_ex
177500100402     c
177600091006     C                   WRITE     WSLSS550
177700091006     C                   ENDIF
177800091006
177900090601     C                   ENDIF
178000090206     c*
178100090206     c* riepilogo
178200090601     c                   if        Wtipo='P' or Wtipo='R'
178300110715     c                   if        §stat=' '
178400110715
178500090206     C     WTSPIP_r      IFGT      *ZERO
178600090206     C                   Z-ADD     §dal          WSSDAL
178700090601     c**                 movel     'R'           WSSTIE
178800090601     c                   movel     Wtipo         WSSTIE
178900090206     C                   MOVE      'IP'          WSSTIP
179000090206     C                   Z-ADD     FIL(K)        WSSLIN
179100090206     C                   Z-ADD     WTSPIP_r      WSSTSP
179200090206     C                   Z-ADD     WTCLIP_r      WSSTCL
179300090206     C                   Z-ADD(H)  WTKGIP_r      WSSTKG
179400090206     C                   Z-ADD(H)  WTMCIP_r      WSSTMC
179500120904     c* old
179600120904     C                   Z-ADD(H)  WTKGIP_r      WSSTKG_ol
179700120904     C                   Z-ADD(H)  WTMCIP_r      WSSTMC_ol
179800100402     c* spedizioni "E"
179900100402     C                   Z-ADD     eWTSPIP_r     WSSTSPe
180000100402     C                   Z-ADD     eWTCLIP_r     WSSTCLe
180100100402     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe
180200100402     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe
180300120904     c* old
180400120904     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe_ol
180500120904     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe_ol
180600100402     c* spedizioni "H"
180700100402     C                   Z-ADD     hWTSPIP_r     WSSTSPh
180800100402     C                   Z-ADD     hWTCLIP_r     WSSTCLh
180900100402     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh
181000100402     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh
181100120904     c* old
181200120904     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh_ol
181300120904     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh_ol
181400100402     c
181500120905     C                   WRITE     WFLSS600
181600120904     C                   WRITE     WSLSS500
181700090206     C                   ENDIF
181800100402     c
181900090206     C     WTSPIA_r      IFGT      *ZERO
182000090206     C                   Z-ADD     §dal          WSSDAL
182100090601     c**                 movel     'R'           WSSTIE
182200090601     c                   movel     Wtipo         WSSTIE
182300090206     C                   MOVE      'IA'          WSSTIP
182400090206     C                   Z-ADD     FIL(K)        WSSLIN
182500090206     C                   Z-ADD     WTSPIA_r      WSSTSP
182600090206     C                   Z-ADD     WTCLIA_r      WSSTCL
182700090206     C                   Z-ADD(H)  WTKGIA_r      WSSTKG
182800090206     C                   Z-ADD(H)  WTMCIA_r      WSSTMC
182900120904     c* old
183000120904     C                   Z-ADD(H)  WTKGIA_r      WSSTKG_ol
183100120904     C                   Z-ADD(H)  WTMCIA_r      WSSTMC_ol
183200100402     c* spedizioni "E"
183300100402     C                   Z-ADD     eWTSPIA_r     WSSTSPe
183400100402     C                   Z-ADD     eWTCLIA_r     WSSTCLe
183500100402     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe
183600100402     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe
183700120904     c* old
183800120904     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe_ol
183900120904     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe_ol
184000100402     c* spedizioni "H"
184100100402     C                   Z-ADD     hWTSPIA_r     WSSTSPh
184200100402     C                   Z-ADD     hWTCLIA_r     WSSTCLh
184300100402     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh
184400100402     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh
184500120904     c* old
184600120904     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh_ol
184700120904     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh_ol
184800100402     c
184900120905     C                   WRITE     WFLSS600
185000120904     C                   WRITE     WSLSS500
185100090206     C                   ENDIF
185200100402
185300090206     C     WTSPEI_R      IFGT      *ZERO
185400090206     C                   Z-ADD     §dal          WSSDAL
185500090601     c**                 movel     'R'           WSSTIE
185600090601     c                   movel     Wtipo         WSSTIE
185700090206     C                   MOVE      'EI'          WSSTIP
185800090206     C                   Z-ADD     FIL(K)        WSSLIN
185900090206     C                   Z-ADD     WTSPEI_r      WSSTSP
186000090206     C                   Z-ADD     WTCLEI_r      WSSTCL
186100090206     C                   Z-ADD(H)  WTKGEI_r      WSSTKG
186200090206     C                   Z-ADD(H)  WTMCEI_r      WSSTMC
186300120904     c* old
186400120904     C                   Z-ADD(H)  WTKGEI_r      WSSTKG_ol
186500120904     C                   Z-ADD(H)  WTMCEI_r      WSSTMC_ol
186600100402     c* Spedizioni "E"
186700100402     C                   Z-ADD     eWTSPEI_r     WSSTSPe
186800100402     C                   Z-ADD     eWTCLEI_r     WSSTCLe
186900100402     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe
187000100402     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe
187100120904     c* old
187200120904     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe_ol
187300120904     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe_ol
187400100402     c* Spedizioni "H"
187500100402     C                   Z-ADD     hWTSPEI_r     WSSTSPh
187600100402     C                   Z-ADD     hWTCLEI_r     WSSTCLh
187700100402     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh
187800100402     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh
187900120904     c* old
188000120904     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh_ol
188100120904     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh_ol
188200100402     c
188300120905     C                   WRITE     WFLSS600
188400120904     C                   WRITE     WSLSS500
188500090206     C                   ENDIF
188600100402     c
188700090206     C     WTSPEE_r      IFGT      *ZERO
188800090206     C                   Z-ADD     §dal          WSSDAL
188900090601     c**                 movel     'R'           WSSTIE
189000090601     c                   movel     Wtipo         WSSTIE
189100090206     C                   MOVE      'EE'          WSSTIP
189200090206     C                   Z-ADD     FIL(K)        WSSLIN
189300090206     C                   Z-ADD     WTSPEE_r      WSSTSP
189400090206     C                   Z-ADD     WTCLEE_r      WSSTCL
189500090206     C                   Z-ADD(H)  WTKGEE_r      WSSTKG
189600090206     C                   Z-ADD(H)  WTMCEE_r      WSSTMC
189700120904     c* old
189800120904     C                   Z-ADD(H)  WTKGEE_r      WSSTKG_ol
189900120904     C                   Z-ADD(H)  WTMCEE_r      WSSTMC_ol
190000100402     c* Spedizioni "E"
190100100402     C                   Z-ADD     eWTSPEE_r     WSSTSPe
190200100402     C                   Z-ADD     eWTCLEE_r     WSSTCLe
190300100402     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe
190400100402     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe
190500120904     c* old
190600120904     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe_ol
190700120904     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe_ol
190800100402     c* Spedizioni "H"
190900100402     C                   Z-ADD     hWTSPEE_r     WSSTSPh
191000100402     C                   Z-ADD     hWTCLEE_r     WSSTCLh
191100100402     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh
191200100402     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh
191300120904     c* old
191400120904     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh_ol
191500120904     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh_ol
191600100402     c
191700120905     C                   WRITE     WFLSS600
191800120904     C                   WRITE     WSLSS500
191900090206     C                   ENDIF
192000090206     C*
192100090206     C     WTSPED_r      IFGT      *ZERO
192200090206     C                   Z-ADD     §dal          WSSDAL
192300090601     c**                 movel     'R'           WSSTIE
192400090601     c                   movel     Wtipo         WSSTIE
192500090206     C                   MOVE      'ED'          WSSTIP
192600090206     C                   Z-ADD     FIL(K)        WSSLIN
192700090206     C                   Z-ADD     WTSPED_r      WSSTSP
192800090206     C                   Z-ADD     WTCLED_r      WSSTCL
192900090206     C                   Z-ADD(H)  WTKGED_r      WSSTKG
193000090206     C                   Z-ADD(H)  WTMCED_r      WSSTMC
193100120904     c* old
193200120904     C                   Z-ADD(H)  WTKGED_r      WSSTKG_ol
193300120904     C                   Z-ADD(H)  WTMCED_r      WSSTMC_ol
193400100402     c* Spedizioni "E"
193500100402     C                   Z-ADD     eWTSPED_r     WSSTSPe
193600100402     C                   Z-ADD     eWTCLED_r     WSSTCLe
193700100402     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe
193800100402     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe
193900120904     c* old
194000120904     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe_ol
194100120904     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe_ol
194200100402     c* Spedizioni "H"
194300100402     C                   Z-ADD     hWTSPED_r     WSSTSPh
194400100402     C                   Z-ADD     hWTCLED_r     WSSTCLh
194500100402     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh
194600100402     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh
194700120904     c* old
194800120904     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh_ol
194900120904     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh_ol
195000100402     c
195100120905     C                   WRITE     WFLSS600
195200120904     C                   WRITE     WSLSS500
195300090206     C                   ENDIF
195400090206     C*
195500090206     C     WTSPEX_r      IFGT      *ZERO
195600090206     C                   Z-ADD     §dal          WSSDAL
195700090601     c**                 movel     'R'           WSSTIE
195800090601     c                   movel     Wtipo         WSSTIE
195900090206     C                   MOVE      'EX'          WSSTIP
196000090206     C                   Z-ADD     FIL(K)        WSSLIN
196100090206     C                   Z-ADD     WTSPEX_r      WSSTSP
196200090206     C                   Z-ADD     WTCLEX_r      WSSTCL
196300090206     C                   Z-ADD(H)  WTKGEX_r      WSSTKG
196400090206     C                   Z-ADD(H)  WTMCEX_r      WSSTMC
196500120904     c* old
196600120904     C                   Z-ADD(H)  WTKGEX_r      WSSTKG_ol
196700120904     C                   Z-ADD(H)  WTMCEX_r      WSSTMC_ol
196800100402     c* Spedizioni "E"
196900100402     C                   Z-ADD     eWTSPEX_r     WSSTSPe
197000100402     C                   Z-ADD     eWTCLEX_r     WSSTCLe
197100100402     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe
197200100402     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe
197300120904     c* old
197400120904     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe_ol
197500120904     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe_ol
197600100402     c* Spedizioni "H"
197700100402     C                   Z-ADD     hWTSPEX_r     WSSTSPh
197800100402     C                   Z-ADD     hWTCLEX_r     WSSTCLh
197900100402     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh
198000100402     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh
198100120904     c* old
198200120904     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh_ol
198300120904     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh_ol
198400100402     c
198500120905     C                   WRITE     WFLSS600
198600120904     C                   WRITE     WSLSS500
198700090206     C                   ENDIF
198800090206     C*
198900090206     C     WTSPEA_r      IFGT      *ZERO
199000090206     C                   Z-ADD     §dal          WSSDAL
199100090601     c**                 movel     'R'           WSSTIE
199200090601     c                   movel     Wtipo         WSSTIE
199300090206     C                   MOVE      'EA'          WSSTIP
199400090206     C                   Z-ADD     FIL(K)        WSSLIN
199500090206     C                   Z-ADD     WTSPEA_r      WSSTSP
199600090206     C                   Z-ADD     WTCLEA_r      WSSTCL
199700090206     C                   Z-ADD(H)  WTKGEA_r      WSSTKG
199800090206     C                   Z-ADD(H)  WTMCEA_r      WSSTMC
199900120904     c* old
200000120904     C                   Z-ADD(H)  WTKGEA_r      WSSTKG_ol
200100120904     C                   Z-ADD(H)  WTMCEA_r      WSSTMC_ol
200200100402     c* Spedizioni "E"
200300100402     C                   Z-ADD     eWTSPEA_r     WSSTSPe
200400100402     C                   Z-ADD     eWTCLEA_r     WSSTCLe
200500100402     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe
200600100402     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe
200700120904     c* old
200800120904     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe_ol
200900120904     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe_ol
201000100402     c* Spedizioni "H"
201100100402     C                   Z-ADD     hWTSPEA_r     WSSTSPh
201200100402     C                   Z-ADD     hWTCLEA_r     WSSTCLh
201300100402     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh
201400100402     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh
201500120904     c* old
201600120904     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh_ol
201700120904     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh_ol
201800100402     c
201900120905     C                   WRITE     WFLSS600
202000120904     C                   WRITE     WSLSS500
202100090206     C                   ENDIF
202200090206     C*
202300090206     C     WTSPPA_r      IFGT      *ZERO
202400090206     C                   Z-ADD     §dal          WSSDAL
202500090601     c**                 movel     'R'           WSSTIE
202600090601     c                   movel     Wtipo         WSSTIE
202700090206     C                   MOVE      'PA'          WSSTIP
202800090206     C                   Z-ADD     FIL(K)        WSSLIN
202900090206     C                   Z-ADD     WTSPPA_r      WSSTSP
203000090206     C                   Z-ADD     WTCLPA_r      WSSTCL
203100090206     C                   Z-ADD(H)  WTKGPA_r      WSSTKG
203200090206     C                   Z-ADD(H)  WTMCPA_r      WSSTMC
203300120904     c* old
203400120904     C                   Z-ADD(H)  WTKGPA_r      WSSTKG_ol
203500120904     C                   Z-ADD(H)  WTMCPA_r      WSSTMC_ol
203600100402     c* Spedizioni "E"
203700100402     C                   Z-ADD     eWTSPPA_r     WSSTSPe
203800100402     C                   Z-ADD     eWTCLPA_r     WSSTCLe
203900100402     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe
204000100402     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe
204100120904     c* old
204200120904     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe_ol
204300120904     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe_ol
204400100402     c* Spedizioni "H"
204500100402     C                   Z-ADD     hWTSPPA_r     WSSTSPh
204600100402     C                   Z-ADD     hWTCLPA_r     WSSTCLh
204700100402     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh
204800100402     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh
204900120904     c* old
205000120904     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh_ol
205100120904     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh_ol
205200100402     c
205300120905     C                   WRITE     WFLSS600
205400120904     C                   WRITE     wslss500
205500090206     C                   ENDIF
205600110715     c
205700110715     c                   endif
205800110715     c
205900110715     c* Extra budget e clienti particolari
206000110715     c                   movel     wtipo         tipoC             1
206100110715     c                   if        §stat<>' ' and wtipo='R'
206200110715     c                   eval      tipoC='B'
206300110715     c                   endif
206400110715     c                   if        §stat<>' ' and wtipo='P'
206500110715     c                   eval      tipoC='C'
206600110715     c                   endif
206700110715     c
206800091006     C     WTSPIP_re     IFGT      *ZERO
206900091006     C                   Z-ADD     §dal          WSSDAL
207000110715     c                   movel     TipoC         WSSTIE
207100091006     C                   MOVE      'IP'          WSSTIP
207200091006     C                   Z-ADD     FIL(K)        WSSLIN
207300091006     C                   Z-ADD     WTSPIP_re     WSSTSP
207400091006     C                   Z-ADD     WTCLIP_re     WSSTCL
207500120904     C                   Z-ADD(H)  WTKGIP_re     WSSTKG_ex
207600120904     C                   Z-ADD(H)  WTMCIP_re     WSSTMC_ex
207700100402     c* Spedizioni "E"
207800100402     C                   Z-ADD     eWTSPIP_re    WSSTSPe
207900100402     C                   Z-ADD     eWTCLIP_re    WSSTCLe
208000120904     C                   Z-ADD(H)  eWTKGIP_re    WSSTKGe_ex
208100120904     C                   Z-ADD(H)  eWTMCIP_re    WSSTMCe_ex
208200100402     c* Spedizioni "H"
208300100402     C                   Z-ADD     hWTSPIP_re    WSSTSPh
208400100402     C                   Z-ADD     hWTCLIP_re    WSSTCLh
208500120904     C                   Z-ADD(H)  hWTKGIP_re    WSSTKGh_ex
208600120904     C                   Z-ADD(H)  hWTMCIP_re    WSSTMCh_ex
208700100402     c
208800091006     C                   WRITE     WSLSS550
208900091006     C                   ENDIF
209000100402     c
209100091006     C     WTSPIA_re     IFGT      *ZERO
209200091006     C                   Z-ADD     §dal          WSSDAL
209300110715     c                   movel     tipoC         WSSTIE
209400091006     C                   MOVE      'IA'          WSSTIP
209500091006     C                   Z-ADD     FIL(K)        WSSLIN
209600091006     C                   Z-ADD     WTSPIA_re     WSSTSP
209700091006     C                   Z-ADD     WTCLIA_re     WSSTCL
209800120904     C                   Z-ADD(H)  WTKGIA_re     WSSTKG_ex
209900120904     C                   Z-ADD(H)  WTMCIA_re     WSSTMC_ex
210000100402     c* Spedizioni "E"
210100100402     C                   Z-ADD     eWTSPIA_re    WSSTSPe
210200100402     C                   Z-ADD     eWTCLIA_re    WSSTCLe
210300120904     C                   Z-ADD(H)  eWTKGIA_re    WSSTKGe_ex
210400120904     C                   Z-ADD(H)  eWTMCIA_re    WSSTMCe_ex
210500100402     c* Spedizioni "H"
210600100402     C                   Z-ADD     hWTSPIA_re    WSSTSPh
210700100402     C                   Z-ADD     hWTCLIA_re    WSSTCLh
210800120904     C                   Z-ADD(H)  hWTKGIA_re    WSSTKGh_ex
210900120904     C                   Z-ADD(H)  hWTMCIA_re    WSSTMCh_ex
211000100402     c
211100091006     C                   WRITE     WSLSS550
211200091006     C                   ENDIF
211300100402     c
211400091006     C     WTSPEI_Re     IFGT      *ZERO
211500091006     C                   Z-ADD     §dal          WSSDAL
211600110715     c                   movel     tipoC         WSSTIE
211700091006     C                   MOVE      'EI'          WSSTIP
211800091006     C                   Z-ADD     FIL(K)        WSSLIN
211900091006     C                   Z-ADD     WTSPEI_re     WSSTSP
212000091006     C                   Z-ADD     WTCLEI_re     WSSTCL
212100120904     C                   Z-ADD(H)  WTKGEI_re     WSSTKG_ex
212200120904     C                   Z-ADD(H)  WTMCEI_re     WSSTMC_ex
212300100402     c* Spedizioni "E"
212400100402     C                   Z-ADD     eWTSPEI_re    WSSTSPe
212500100402     C                   Z-ADD     eWTCLEI_re    WSSTCLe
212600120904     C                   Z-ADD(H)  eWTKGEI_re    WSSTKGe_ex
212700120904     C                   Z-ADD(H)  eWTMCEI_re    WSSTMCe_ex
212800100402     c* Spedizioni "H"
212900100402     C                   Z-ADD     hWTSPEI_re    WSSTSPh
213000100402     C                   Z-ADD     hWTCLEI_re    WSSTCLh
213100120904     C                   Z-ADD(H)  hWTKGEI_re    WSSTKGh_ex
213200120904     C                   Z-ADD(H)  hWTMCEI_re    WSSTMCh_ex
213300100402     c
213400091006     C                   WRITE     WSLSS550
213500091006     C                   ENDIF
213600100402     c
213700091006     C     WTSPEE_re     IFGT      *ZERO
213800091006     C                   Z-ADD     §dal          WSSDAL
213900110715     c                   movel     tipoC         WSSTIE
214000091006     C                   MOVE      'EE'          WSSTIP
214100091006     C                   Z-ADD     FIL(K)        WSSLIN
214200091006     C                   Z-ADD     WTSPEE_re     WSSTSP
214300091006     C                   Z-ADD     WTCLEE_re     WSSTCL
214400120904     C                   Z-ADD(H)  WTKGEE_re     WSSTKG_ex
214500120904     C                   Z-ADD(H)  WTMCEE_re     WSSTMC_ex
214600100402     c* Spedizioni "E"
214700100402     C                   Z-ADD     eWTSPEE_re    WSSTSPe
214800100402     C                   Z-ADD     eWTCLEE_re    WSSTCLe
214900120904     C                   Z-ADD(H)  eWTKGEE_re    WSSTKGe_ex
215000120904     C                   Z-ADD(H)  eWTMCEE_re    WSSTMCe_ex
215100100402     c* Spedizioni "h"
215200100402     C                   Z-ADD     hWTSPEE_re    WSSTSPh
215300100402     C                   Z-ADD     hWTCLEE_re    WSSTCLh
215400120904     C                   Z-ADD(H)  hWTKGEE_re    WSSTKGh_ex
215500120904     C                   Z-ADD(H)  hWTMCEE_re    WSSTMCh_ex
215600100402     c
215700091006     C                   WRITE     WSLSS550
215800091006     C                   ENDIF
215900091006     C*
216000091006     C     WTSPED_re     IFGT      *ZERO
216100091006     C                   Z-ADD     §dal          WSSDAL
216200110715     c                   movel     tipoC         WSSTIE
216300091006     C                   MOVE      'ED'          WSSTIP
216400091006     C                   Z-ADD     FIL(K)        WSSLIN
216500091006     C                   Z-ADD     WTSPED_re     WSSTSP
216600091006     C                   Z-ADD     WTCLED_re     WSSTCL
216700120904     C                   Z-ADD(H)  WTKGED_re     WSSTKG_ex
216800120904     C                   Z-ADD(H)  WTMCED_re     WSSTMC_ex
216900100402     c* Spedizioni "E"
217000100402     C                   Z-ADD     eWTSPED_re    WSSTSPe
217100100402     C                   Z-ADD     eWTCLED_re    WSSTCLe
217200120904     C                   Z-ADD(H)  eWTKGED_re    WSSTKGe_ex
217300120904     C                   Z-ADD(H)  eWTMCED_re    WSSTMCe_ex
217400100402     c* Spedizioni "H"
217500100402     C                   Z-ADD     hWTSPED_re    WSSTSPh
217600100402     C                   Z-ADD     hWTCLED_re    WSSTCLh
217700120904     C                   Z-ADD(H)  hWTKGED_re    WSSTKGh_ex
217800120904     C                   Z-ADD(H)  hWTMCED_re    WSSTMCh_ex
217900100402     c
218000091006     C                   WRITE     WSLSS550
218100091006     C                   ENDIF
218200091006     C*
218300091006     C     WTSPEX_re     IFGT      *ZERO
218400091006     C                   Z-ADD     §dal          WSSDAL
218500110715     c                   movel     tipoC         WSSTIE
218600091006     C                   MOVE      'EX'          WSSTIP
218700091006     C                   Z-ADD     FIL(K)        WSSLIN
218800091006     C                   Z-ADD     WTSPEX_re     WSSTSP
218900091006     C                   Z-ADD     WTCLEX_re     WSSTCL
219000120904     C                   Z-ADD(H)  WTKGEX_re     WSSTKG_ex
219100120904     C                   Z-ADD(H)  WTMCEX_re     WSSTMC_ex
219200100402     c* Spedizioni "E"
219300100402     C                   Z-ADD     eWTSPEX_re    WSSTSPe
219400100402     C                   Z-ADD     eWTCLEX_re    WSSTCLe
219500120904     C                   Z-ADD(H)  eWTKGEX_re    WSSTKGe_ex
219600120904     C                   Z-ADD(H)  eWTMCEX_re    WSSTMCe_ex
219700100402     c* Spedizioni "H"
219800100402     C                   Z-ADD     hWTSPEX_re    WSSTSPh
219900100402     C                   Z-ADD     hWTCLEX_re    WSSTCLh
220000120904     C                   Z-ADD(H)  hWTKGEX_re    WSSTKGh_ex
220100120904     C                   Z-ADD(H)  hWTMCEX_re    WSSTMCh_ex
220200100402     c
220300091006     C                   WRITE     WSLSS550
220400091006     C                   ENDIF
220500091006     C*
220600091006     C     WTSPEA_re     IFGT      *ZERO
220700091006     C                   Z-ADD     §dal          WSSDAL
220800110715     c                   movel     tipoC         WSSTIE
220900091006     C                   MOVE      'EA'          WSSTIP
221000091006     C                   Z-ADD     FIL(K)        WSSLIN
221100091006     C                   Z-ADD     WTSPEA_re     WSSTSP
221200091006     C                   Z-ADD     WTCLEA_re     WSSTCL
221300120904     C                   Z-ADD(H)  WTKGEA_re     WSSTKG_ex
221400120904     C                   Z-ADD(H)  WTMCEA_re     WSSTMC_ex
221500100402     c* Spedizioni "E"
221600100402     C                   Z-ADD     eWTSPEA_re    WSSTSPe
221700100402     C                   Z-ADD     eWTCLEA_re    WSSTCLe
221800120904     C                   Z-ADD(H)  eWTKGEA_re    WSSTKGe_ex
221900120904     C                   Z-ADD(H)  eWTMCEA_re    WSSTMCe_ex
222000100402     c* Spedizioni "H"
222100100402     C                   Z-ADD     hWTSPEA_re    WSSTSPh
222200100402     C                   Z-ADD     hWTCLEA_re    WSSTCLh
222300120904     C                   Z-ADD(H)  hWTKGEA_re    WSSTKGh_ex
222400120904     C                   Z-ADD(H)  hWTMCEA_re    WSSTMCh_ex
222500100402     c
222600091006     C                   WRITE     WSLSS550
222700091006     C                   ENDIF
222800091006     C*
222900091006     C     WTSPPA_re     IFGT      *ZERO
223000091006     C                   Z-ADD     §dal          WSSDAL
223100110715     c                   movel     tipoC         WSSTIE
223200091006     C                   MOVE      'PA'          WSSTIP
223300091006     C                   Z-ADD     FIL(K)        WSSLIN
223400091006     C                   Z-ADD     WTSPPA_re     WSSTSP
223500091006     C                   Z-ADD     WTCLPA_re     WSSTCL
223600120904     C                   Z-ADD(H)  WTKGPA_re     WSSTKG_ex
223700120904     C                   Z-ADD(H)  WTMCPA_re     WSSTMC_ex
223800100402     c* Spedizioni "E"
223900100402     C                   Z-ADD     eWTSPPA_re    WSSTSPe
224000100402     C                   Z-ADD     eWTCLPA_re    WSSTCLe
224100120904     C                   Z-ADD(H)  eWTKGPA_re    WSSTKGe_ex
224200120904     C                   Z-ADD(H)  eWTMCPA_re    WSSTMCe_ex
224300100402     c* Spedizioni "H"
224400100402     C                   Z-ADD     hWTSPPA_re    WSSTSPh
224500100402     C                   Z-ADD     hWTCLPA_re    WSSTCLh
224600120904     C                   Z-ADD(H)  hWTKGPA_re    WSSTKGh_ex
224700120904     C                   Z-ADD(H)  hWTMCPA_re    WSSTMCh_ex
224800100402     c
224900091006     C                   WRITE     WSLSS550
225000091006     C                   ENDIF
225100091006     C                   ENDIF
225200100402
225300090206     c                   enddo
225400090206     c                   endsr
225500970527     C**********************************************************
225600970529     C** CARICAMENTO DATI INIZIALI
225700970527     C**********************************************************
225800970529     C     CARINZ        BEGSR
225900970905     C                   SETOFF                                       90
226000970529     C     KTAS          KLIST
226100970529     C                   KFLD                    WAAS
226200970529     C                   KFLD                    WMGS
226300970529     C     KTAB          KLIST
226400970529     C                   KFLD                    TBLKUT
226500970529     C                   KFLD                    TBLCOD
226600970529     C                   KFLD                    TBLKEY
226700970529     C     KTB           KLIST
226800970529     C                   KFLD                    TBLKUT
226900970529     C                   KFLD                    TBLCOD
227000970529     C                   Z-ADD     1             TBLKUT
227100090204     c     kwfsmp        klist
227200090204     c                   kfld                    k_fil
227300090204     c                   kfld                    k_tip
227400090204     c                   kfld                    k_dat
227500090205     C     kar5          KLIST
227600090205     C                   KFLD                    tasaas
227700090205     C                   KFLD                    taslnp
227800090205     C                   KFLD                    tasnrs
227900090205     C                   KFLD                    tasnsp
228000090205     C                   KFLD                    k_trd
228100090209     C                   KFLD                    tasflp
228200120903     c     kss61         klist
228300120903     c                   kfld                    ks_tie
228400120903     c                   kfld                    ks_tip
228500120903     c                   kfld                    ks_lin
228600120903     c                   kfld                    ks_sca
228700120903     c
228800090915     c* apertura file di stampa congelato in SAVE
228900090915     C* ESEGUO OVRPRTF
229000090915     C                   Z-ADD     61            LUNG             15 5
229100090915     C                   MOVEL     *BLANKS       COMMAN           80
229200090915     C                   MOVEA     CMD           COMMAN
229300090915     C                   CALL      'QCMDEXC'                            H1
229400090915     C                   PARM                    COMMAN
229500090915     C                   PARM                    LUNG
229600090915     C*
229700090915     C                   OPEN      QSYSPRT
229800090915     C*
229900970529     C*
230000970529     C** CARICAMENTO DS MULTIPLA  X DATI FILIALE
230100970527     C** PULIZIA DS
230200121127     C                   DO        400           K                 4 0
230300970527     C     K             OCCUR     DSFIL
230400970527     C                   CLEAR                   DSFIL
230500970527     C                   END
230600121127     C                   DO        400           K
230700100402     C     K             OCCUR     DSFIL_e
230800100402     C                   CLEAR                   DSFIL_e
230900100402     C                   END
231000121127     C                   DO        400           K
231100100402     C     K             OCCUR     DSFIL_h
231200100402     C                   CLEAR                   DSFIL_h
231300100402     C                   END
231400121127     C                   DO        400           K
231500090206     C     K             OCCUR     DSFIL_smp
231600090206     C                   CLEAR                   DSFIL_smp
231700090206     C                   END
231800970527     C*
231900970527     C                   Z-ADD     0             K
232000970527     C     *LOVAL        SETLL     AZORG01L
232100970527     C                   DO        *HIVAL
232200090204     C                   READ      AZORG01L
232300090204     c                   if        %eof(azorg01l)
232400090204     c                   leave
232500090204     c                   endif
232600970527     C     ORGFAG        IFEQ      'F'
232700970527     C     ORGFAG        OREQ      'A'
232800970527     C                   ADD       1             K
232900970527     C                   MOVEL     ORGFIL        FIL(K)
233000090204     C                   MOVEL     ORGdes        DIL(K)
233100090204     C                   z-add     ORGcar        car(K)
233200090204     c                   movel     orgde8        og148
233300090206     c                   if        §oglpo='1' or §oglpo='2'
233400090206     c                   move      §oglpo        lpo(K)
233500090206     c                   else
233600090206     c                   z-add     2             lpo(K)
233700090206     c                   endif
233800090204     c                   movel     orgde3        og143
233900090209     c**                 if        §ogntw='DPD' or §ogntw='EEX' or §ogntw='FED'
234000090209     c**                 move      'E'           tif(K)
234100090209     c**                 else
234200090209     c**                 move      'I'           tif(K)
234300090209     c**                 endif
234400090209     c                   select
234500090209     c                   when      §ogntw='DPD'
234600090209     c                   move      'D'           tif(K)
234700090209     c                   when      §ogntw='EEX'
234800090209     c                   move      'X'           tif(K)
234900090209     c                   when      §ogntw='FED'
235000090209     c                   move      'F'           tif(K)
235100090209     c                   other
235200090209     c                   move      'I'           tif(K)
235300090209     c                   endsl
235400970527     C                   ENDIF
235500090204     C                   ENDDO
235600970529     C*
235700970529     C                   Z-ADD     0             K
235800970529     C                   MOVEL     'TB'          TBLCOD
235900970529     C     KTB           SETLL     TABEL00F
236000970529     C                   DO        *HIVAL
236100090204     C     KTB           READE     TABEL00F
236200090204     c                   if        %eof(tabel00f)
236300090204     c                   leave
236400090204     c                   endif
236500970529     C     TBLFLG        IFNE      '*'
236600970529     C                   MOVEL     TBLUNI        DSTB
236700970529     C     §TBRBL        IFEQ      'N'
236800970529     C     §TBRBL        OREQ      'S'
236900970529     C                   ADD       1             K
237000970529     C                   MOVEL     TBLKEY        TBO(K)
237100970529     C                   ENDIF
237200970529     C                   ENDIF
237300090204     C                   ENDDO
237400120903     c*
237500120903     c* Carico scaglioni di peso
237600120903     C                   Z-ADD     0             II
237700120903     C                   MOVEL     '2L'          TBLCOD
237800120903     C     KTB           SETLL     TABEL00F
237900120903    1C                   DO        *HIVAL
238000120903     C     KTB           READE     TABEL00F
238100120903    2c                   if        %eof(tabel00f)
238200120903     c                   leave
238300120903    2c                   endif
238400120903    2C     TBLFLG        IFNE      '*'
238500120903     C                   MOVEL     TBLUNI        DS2l
238600120903     C                   MOVEL     TBLKEY        WTM03             3
238700120904     C                   MOVEL     TBLKEY        WTM05             5
238800120903    3C     WTM03         IFEQ      'S9R'
238900120903     C                   ADD       1             II
239000120903     C                   MOVE      WTM05         FPR(II)
239100120903     C                   Z-ADD     §2LKGI        KIR(II)
239200120903     C                   Z-ADD     §2LKGF        KFR(II)
239300120903    3C                   ENDIF
239400120903    2C                   ENDIF
239500120903     C                   ENDDO
239600091006     C*
239700091006     c* Lettura TABELLA clienti extra budget
239800110711     c                   if        §stat=' '
239900091006     C                   Z-ADD     0             K
240000091006     C                   MOVEL     'CXB'         TBecod
240100091006     C     tbecod        SETLL     tntbe01l
240200091006     C                   DO        *HIVAL
240300091006     C     tbecod        READE     tntbe01l
240400091006     c                   if        %eof(tntbe01l)
240500091006     c                   leave
240600091006     c                   endif
240700091006
240800091006     C     tbeatb        IFeq      ' '
240900091006     C                   MOVEL     TBeuni        Dcxb
241000091006     C                   ADD       1             K
241100091006     C                   MOVEL     TBeKE1        ksc_ex(k)
241200091007     C                   MOVE      §cxbctr       ksc_ex(k)
241300091006     C                   ENDIF
241400091006     C                   ENDDO
241500110711     c
241600110711     c                   else
241700121121     c* Lettura TABELLA clienti Particolari
241800110711     C                   Z-ADD     0             K
241900110711     C                   MOVEL     'CXQ'         TBecod
242000110711     C     tbecod        SETLL     tntbe01l
242100110711     C                   DO        *HIVAL
242200110711     C     tbecod        READE     tntbe01l
242300110711     c                   if        %eof(tntbe01l)
242400110711     c                   leave
242500110711     c                   endif
242600110711
242700110711     C     tbeatb        IFeq      ' '
242800110711     C                   ADD       1             K
242900110711     C                   MOVEL     TBeKE2        ksc_ex(k)
243000121121     c                   movel     tbeuni        dcxq
243100121121     c                   if        §cxqterm>*zeros
243200121121     c                   movel     §cxqterm      ksc_tr(k)
243300121121     c                   else
243400121121     c                   movel     999           ksc_tr(k)
243500121121     c                   endif
243600121121     c
243700110711     C                   ENDIF
243800110711     C                   ENDDO
243900110711     c                   endif
244000110711     c*
244100110711     c* Controllo riempimento schiera
244200110715     c                   movel     kpjbu         savkpjbu
244300110711     c                   clear                   trul0sds
244400110711     c                   eval      i0stla='L'
244500110711     c                   eval      i0sski='KSC_EX'
244600110711     c                   eval      i0sele=%elem(ksc_ex)
244700110711     c                   eval      i0spie=k
244800110711     c                   eval      i0sfile='Cienti tab CXB-CXQ'
244900110711     c                   eval      i0ssif=knsif
245000110711     c                   eval      i0spgm='TNSSL3R2'
245100110711     c                   movel     trul0sds      kpjbu
245200110711     c                   call      'TRUL0SR'
245300110711     c                   parm                    kpjba
245400110715
245500110715     c                   movel     savkpjbu      kpjbu
245600090601     c
245700090204     c* data elaborazione nel formato AAAA/MM/GG
245800090204     c                   time                    dataiso
245900090204     c                   move      dataiso       wdatael
246000110711     c
246100110711     c                   if        §stat=' '
246200090601     c*
246300090601     c* Pulisco i file per i dati che devo rielaborare
246400090601     c                   eval      wrkgetlista =
246500120905     c                             'delete from WFLSS60F where wsstie= '''   +
246600090603     c                              Wtipo +  ''''
246700090601     c                   if        Wtipo='R'
246800090601     c                   eval      wrkgetlista =%trim(wrkgetlista)+
246900090603     c                              ' or  wsstie=''G'''
247000090601     c                   endif
247100090609     c*
247200090601     C/EXEC SQL
247300090601     C+ execute immediate :wrkgetlista
247400090601     C/END-EXEC
247500090609     c
247600120904     c*
247700120904     c* Pulisco i file per i dati che devo rielaborare
247800120904     c                   eval      wrkgetlista =
247900120904     c                             'delete from wslss50F where wsstie= '''   +
248000120904     c                              Wtipo +  ''''
248100120904     c                   if        Wtipo='R'
248200120904     c                   eval      wrkgetlista =%trim(wrkgetlista)+
248300120904     c                              ' or  wsstie=''G'''
248400120904     c                   endif
248500120904     c*
248600120904     C/EXEC SQL
248700120904     C+ execute immediate :wrkgetlista
248800120904     C/END-EXEC
248900120904     c*
249000090609     c* Pulisco i file per i dati che devo rielaborare
249100090609     c                   clear                   wrkgetlista
249200090609     c                   eval      wrkgetlista =
249300090609     c                             'delete from WFSMP00F where wmptip= '''   +
249400090609     c                              Wtipo +  ''''
249500090609     c                   if        Wtipo='R'
249600090609     c                   eval      wrkgetlista =%trim(wrkgetlista)+
249700090609     c                              ' or  wmptip=''G'''
249800090609     c                   endif
249900110711     c
250000090609     c*
250100090609     C/EXEC SQL
250200090609     C+ execute immediate :wrkgetlista
250300090609     C/END-EXEC
250400120904     c                   endif
250500091007     c*
250600091007     c* Pulisco il file clienti extra budget
250700091007     c                   clear                   wrkgetlista
250800110711     c
250900110711     c                   if        §stat=' '
251000091007     c                   eval      wrkgetlista =
251100120903     c                             'delete from wslss55F where wsstie= '''   +
251200091007     c                              Wtipo +  ''''
251300091007     c                   if        Wtipo='R'
251400091007     c                   eval      wrkgetlista =%trim(wrkgetlista)+
251500091007     c                              ' or  wsstie=''G'''
251600091007     c                   endif
251700111028     C/EXEC SQL
251800111028     C+ execute immediate :wrkgetlista
251900111028     C/END-EXEC
252000110711     c
252100110711     c                   else
252200111028     c
252300111028     c* Pulisco il file clienti particolari
252400110711     c                   select
252500110711     c                   when      Wtipo='R'
252600110711     c                   eval      wrkgetlista =
252700110715     c                             'delete from WSLSS55F where wsstie= ''A'' or-
252800110711     c                               wsstie= ''B'''
252900110711     c                   when      Wtipo='G'
253000110711     c                   eval      wrkgetlista =
253100110711     c                             'delete from WSLSS55F where wsstie= ''A'''
253200110711     c                   when      Wtipo='P'
253300110711     c                   eval      wrkgetlista =
253400110711     c                             'delete from WSLSS55F where wsstie= ''C'''
253500110711     c                   endsl
253600110711     c
253700091007     C/EXEC SQL
253800091007     C+ execute immediate :wrkgetlista
253900091007     C/END-EXEC
254000111028     c
254100111028     c* Preparo la lettura dei soli clienti particolari con SQL
254200111028     c                   clear                   wrkgetlista
254300111028     c                   eval      wrkgetlista =
254400111028     c                             'select * from titas00f where +
254500111028     c                             ((tasaas*10000) +tasmgs) >=' +
254600111028     c                             %editc(§dal:'X')+ ' and +
254700111028     c                             ((tasaas*10000) +tasmgs) <=' +
254800111028     c                             %editc(§al :'X')+ ' and tasccm in('
254900111028      * ciclo sulla sk dei cod bolla di recupero
255000121203     c                   Do        1100          XX
255100111028     c                   If        ksc_ex(XX) = *blanks
255200111028     c                   Leave
255300111028     c                   EndIf
255400111028     c
255500111028     c                   If        XX >1
255600111028     c                   eval      wrkgetlista =
255700111028     c                             %trim(wrkgetlista) + ','
255800111028     c                   EndIf
255900111028     c
256000111028     c                   eval      wrkgetlista =
256100111028     c                             %trim(wrkgetlista) + ' ''' +
256200111028     c                             %subst(ksc_ex(xx):1:7) + ''''
256300111028     c                   EndDo
256400111028     c
256500111028     c                   Eval      wrkgetlista =
256600111028     c                             %trim(wrkgetlista) + ')'
256700111028     c
256800111028     c
256900111028     c                   eval      wrkgetlista =%trimr(wrkgetlista)+' union '+
257000111028     c                             'select * from titas10f where +
257100111028     c                             ((tasaas*10000) +tasmgs) >=' +
257200111028     c                             %editc(§dal:'X')+ ' and +
257300111028     c                             ((tasaas*10000) +tasmgs) <=' +
257400111028     c                             %editc(§al :'X')+ ' and tasccm in('
257500111028      * ciclo sulla sk dei cod bolla di recupero
257600121203     c                   Do        1100          XX                4 0
257700111028     c                   If        ksc_ex(XX) = *blanks
257800111028     c                   Leave
257900111028     c                   EndIf
258000111028     c
258100111028     c                   If        XX >1
258200111028     c                   eval      wrkgetlista =
258300111028     c                             %trim(wrkgetlista) + ','
258400111028     c                   EndIf
258500111028     c
258600111028     c                   eval      wrkgetlista =
258700111028     c                             %trim(wrkgetlista) + ' ''' +
258800111028     c                             %subst(ksc_ex(xx):1:7) + ''''
258900111028     c                   EndDo
259000111028     c
259100111028     c                   Eval      wrkgetlista =
259200111028     c                             %trim(wrkgetlista) + ')'
259300111028     c
259400111028     c                   EndIf
259500111028     c
259600970527     C                   ENDSR
259700100402     c****************************************************************************
259800100402     c     SpeITA        BEGSR
259900100402     c* giornaliero
260000100402     c                   if        (tasaas*10000+tasmgs)=§al
260100100402     C                   ADD       1             WTSPIP
260200100402     C                   ADD       TASNCL        WTCLIP
260300100402     C                   ADD       w_peso        WTKGIP
260400100402     C                   ADD       TASVLF        WTMCIP
260500100402     c* tipo servizio  "E"
260600100402     c                   if        tastsp='E'
260700100402     C                   ADD       1             eWTSPIP
260800100402     C                   ADD       TASNCL        eWTCLIP
260900100402     C                   ADD       w_peso        eWTKGIP
261000100402     C                   ADD       TASVLF        eWTMCIP
261100100402     c                   endif
261200100402     c* tipo servizio  "H"
261300100402     c                   if        tastsp='H'
261400100402     C                   ADD       1             hWTSPIP
261500100402     C                   ADD       TASNCL        hWTCLIP
261600100402     C                   ADD       w_peso        hWTKGIP
261700100402     C                   ADD       TASVLF        hWTMCIP
261800100402     c                   endif
261900100402     c**
262000100402     c* Extra budget
262100100402     c                   if        *in35
262200100402     C                   ADD       1             WTSPIP_e
262300100402     C                   ADD       TASNCL        WTCLIP_e
262400100402     C                   ADD       w_peso        WTKGIP_e
262500100402     C                   ADD       TASVLF        WTMCIP_e
262600100402     c* tipo servizio  "E"
262700100402     c                   if        tastsp='E'
262800100402     C                   ADD       1             eWTSPIP_e
262900100402     C                   ADD       TASNCL        eWTCLIP_e
263000100402     C                   ADD       w_peso        eWTKGIP_e
263100100402     C                   ADD       TASVLF        eWTMCIP_e
263200100402     c                   endif
263300100402     c* tipo servizio  "H"
263400100402     c                   if        tastsp='H'
263500100402     C                   ADD       1             hWTSPIP_e
263600100402     C                   ADD       TASNCL        hWTCLIP_e
263700100402     C                   ADD       w_peso        hWTKGIP_e
263800100402     C                   ADD       TASVLF        hWTMCIP_e
263900100402     c                   endif
264000100402
264100100402     c                   endif
264200100402     c                   endif
264300100402     c* riepilogo
264400100402     C                   ADD       1             WTSPIP_r
264500100402     C                   ADD       TASNCL        WTCLIP_r
264600100402     C                   ADD       w_peso        WTKGIP_r
264700100402     C                   ADD       TASVLF        WTMCIP_r
264800100402     c* tipo servizio  "E"
264900100402     c                   if        tastsp='E'
265000100402     C                   ADD       1             eWTSPIP_r
265100100402     C                   ADD       TASNCL        eWTCLIP_r
265200100402     C                   ADD       w_peso        eWTKGIP_r
265300100402     C                   ADD       TASVLF        eWTMCIP_r
265400100402     c                   endif
265500100402     c* tipo servizio  "H"
265600100402     c                   if        tastsp='H'
265700100402     C                   ADD       1             hWTSPIP_r
265800100402     C                   ADD       TASNCL        hWTCLIP_r
265900100402     C                   ADD       w_peso        hWTKGIP_r
266000100402     C                   ADD       TASVLF        hWTMCIP_r
266100100402     c                   endif
266200100402     c* Extra budget
266300100402     c                   if        *in35
266400100402     C                   ADD       1             WTSPIP_re
266500100402     C                   ADD       TASNCL        WTCLIP_re
266600100402     C                   ADD       w_peso        WTKGIP_re
266700100402     C                   ADD       TASVLF        WTMCIP_re
266800100402     c* tipo servizio  "E"
266900100402     c                   if        tastsp='E'
267000100402     C                   ADD       1             eWTSPIP_re
267100100402     C                   ADD       TASNCL        eWTCLIP_re
267200100402     C                   ADD       w_peso        eWTKGIP_re
267300100402     C                   ADD       TASVLF        eWTMCIP_re
267400100402     c                   endif
267500100402     c* tipo servizio  "H"
267600100402     c                   if        tastsp='H'
267700100402     C                   ADD       1             hWTSPIP_re
267800100402     C                   ADD       TASNCL        hWTCLIP_re
267900100402     C                   ADD       w_peso        hWTKGIP_re
268000100402     C                   ADD       TASVLF        hWTMCIP_re
268100100402     c                   endif
268200100402     c
268300100402     c                   endif
268400120903     c
268500120903     c                   eval      ks_tip='IP'
268600120903     c                   exsr      spe_peso
268700120903     c
268800100402     c                   ENDSR
268900100402     c****************************************************************************
269000100402     c     SpeIMPORT     BEGSR
269100100402     c* giornaliero
269200100402     c                   if        (tasaas*10000+tasmgs)=§al
269300100402     C                   ADD       1             WTSPEI
269400100402     C                   ADD       TASNCL        WTCLEI
269500100402     C                   ADD       w_peso        WTKGEI
269600100402     C                   ADD       TASVLF        WTMCEI
269700100402     c* tipo servizio  "E"
269800100402     c                   if        tastsp='E'
269900100402     C                   ADD       1             eWTSPEI
270000100402     C                   ADD       TASNCL        eWTCLEI
270100100402     C                   ADD       w_peso        eWTKGEI
270200100402     C                   ADD       TASVLF        eWTMCEI
270300100402     c                   endif
270400100402     c* tipo servizio  "H"
270500100402     c                   if        tastsp='H'
270600100402     C                   ADD       1             hWTSPEI
270700100402     C                   ADD       TASNCL        hWTCLEI
270800100402     C                   ADD       w_peso        hWTKGEI
270900100402     C                   ADD       TASVLF        hWTMCEI
271000100402     c                   endif
271100100402     c* Extra Budget
271200100402     c                   if        *in35
271300100402     C                   ADD       1             WTSPEI_e
271400100402     C                   ADD       TASNCL        WTCLEI_e
271500100402     C                   ADD       w_peso        WTKGEI_e
271600100402     C                   ADD       TASVLF        WTMCEI_e
271700100402     c* tipo servizio  "E"
271800100402     c                   if        tastsp='E'
271900100402     C                   ADD       1             eWTSPEI_e
272000100402     C                   ADD       TASNCL        eWTCLEI_e
272100100402     C                   ADD       w_peso        eWTKGEI_e
272200100402     C                   ADD       TASVLF        eWTMCEI_e
272300100402     c                   endif
272400100402     c* tipo servizio  "H"
272500100402     c                   if        tastsp='H'
272600100402     C                   ADD       1             hWTSPEI_e
272700100402     C                   ADD       TASNCL        hWTCLEI_e
272800100402     C                   ADD       w_peso        hWTKGEI_e
272900100402     C                   ADD       TASVLF        hWTMCEI_e
273000100402     c                   endif
273100100402     c
273200100402     c                   endif
273300100402     c                   endif
273400100402     c* riepilogo
273500100402     C                   ADD       1             WTSPEI_r
273600100402     C                   ADD       TASNCL        WTCLEI_r
273700100402     C                   ADD       w_peso        WTKGEI_r
273800100402     C                   ADD       TASVLF        WTMCEI_r
273900100402     c* tipo servizio  "E"
274000100402     c                   if        tastsp='E'
274100100402     C                   ADD       1             eWTSPEI_r
274200100402     C                   ADD       TASNCL        eWTCLEI_r
274300100402     C                   ADD       w_peso        eWTKGEI_r
274400100402     C                   ADD       TASVLF        eWTMCEI_r
274500100402     c                   endif
274600100402     c* tipo servizio  "H"
274700100402     c                   if        tastsp='H'
274800100402     C                   ADD       1             hWTSPEI_r
274900100402     C                   ADD       TASNCL        hWTCLEI_r
275000100402     C                   ADD       w_peso        hWTKGEI_r
275100100402     C                   ADD       TASVLF        hWTMCEI_r
275200100402     c                   endif
275300100402     c* extra budget
275400100402     c                   if        *in35
275500100402     C                   ADD       1             WTSPEI_re
275600100402     C                   ADD       TASNCL        WTCLEI_re
275700100402     C                   ADD       w_peso        WTKGEI_re
275800100402     C                   ADD       TASVLF        WTMCEI_re
275900100402     c* tipo servizio  "E"
276000100402     c                   if        tastsp='E'
276100100402     C                   ADD       1             eWTSPEI_re
276200100402     C                   ADD       TASNCL        eWTCLEI_re
276300100402     C                   ADD       w_peso        eWTKGEI_re
276400100402     C                   ADD       TASVLF        eWTMCEI_re
276500100402     c                   endif
276600100402     c* tipo servizio  "H"
276700100402     c                   if        tastsp='H'
276800100402     C                   ADD       1             hWTSPEI_re
276900100402     C                   ADD       TASNCL        hWTCLEI_re
277000100402     C                   ADD       w_peso        hWTKGEI_re
277100100402     C                   ADD       TASVLF        hWTMCEI_re
277200100402     c                   endif
277300100402     c                   endif
277400120904     c
277500120904     c                   eval      ks_tip='EI'
277600120904     c                   exsr      spe_peso
277700120904     c
277800100402     c                   ENDSR
277900100402     c****************************************************************************
278000100402     c     speEXPRESS    BEGSR
278100100402     c* giornaliero
278200100402     c                   if        (tasaas*10000+tasmgs)=§al
278300100402     C                   ADD       1             WTSPEE                         EUROEXPRESS
278400100402     C                   ADD       TASNCL        WTCLEE
278500100402     C                   ADD       w_peso        WTKGEE
278600100402     C                   ADD       TASVLF        WTMCEE
278700100402     c* tipo servizio  "E"
278800100402     c                   if        tastsp='E'
278900100402     C                   ADD       1             eWTSPEE                         EUROEXPRESS
279000100402     C                   ADD       TASNCL        eWTCLEE
279100100402     C                   ADD       w_peso        eWTKGEE
279200100402     C                   ADD       TASVLF        eWTMCEE
279300100402     c                   endif
279400100402     c* tipo servizio  "H"
279500100402     c                   if        tastsp='H'
279600100402     C                   ADD       1             hWTSPEE                         EUROEXPRESS
279700100402     C                   ADD       TASNCL        hWTCLEE
279800100402     C                   ADD       w_peso        hWTKGEE
279900100402     C                   ADD       TASVLF        hWTMCEE
280000100402     c                   endif
280100100402     c* extra budget
280200100402     c                   if        *in35
280300100402     C                   ADD       1             WTSPEE_e                       EUROEXPRESS
280400100402     C                   ADD       TASNCL        WTCLEE_e
280500100402     C                   ADD       w_peso        WTKGEE_e
280600100402     C                   ADD       TASVLF        WTMCEE_e
280700100402     c* tipo servizio  "E"
280800100402     c                   if        tastsp='E'
280900100402     C                   ADD       1             eWTSPEE_e                       EUROEXPRESS
281000100402     C                   ADD       TASNCL        eWTCLEE_e
281100100402     C                   ADD       w_peso        eWTKGEE_e
281200100402     C                   ADD       TASVLF        eWTMCEE_e
281300100402     c                   endif
281400100402     c* tipo servizio  "H"
281500100402     c                   if        tastsp='H'
281600100402     C                   ADD       1             hWTSPEE_e                       EUROEXPRESS
281700100402     C                   ADD       TASNCL        hWTCLEE_e
281800100402     C                   ADD       w_peso        hWTKGEE_e
281900100402     C                   ADD       TASVLF        hWTMCEE_e
282000100402     c                   endif
282100100402     c
282200100402     c                   endif
282300100402     c                   endif
282400100402     c* riepilogo
282500100402     C                   ADD       1             WTSPEE_r                       EUROEXPRESS
282600100402     C                   ADD       TASNCL        WTCLEE_r
282700100402     C                   ADD       w_peso        WTKGEE_r
282800100402     C                   ADD       TASVLF        WTMCEE_r
282900100402     c* tipo servizio  "E"
283000100402     c                   if        tastsp='E'
283100100402     C                   ADD       1             eWTSPEE_r                       EUROEXPRESS
283200100402     C                   ADD       TASNCL        eWTCLEE_r
283300100402     C                   ADD       w_peso        eWTKGEE_r
283400100402     C                   ADD       TASVLF        eWTMCEE_r
283500100402     c                   endif
283600100402     c* tipo servizio  "H"
283700100402     c                   if        tastsp='H'
283800100402     C                   ADD       1             hWTSPEE_r                       EUROEXPRESS
283900100402     C                   ADD       TASNCL        hWTCLEE_r
284000100402     C                   ADD       w_peso        hWTKGEE_r
284100100402     C                   ADD       TASVLF        hWTMCEE_r
284200100402     c                   endif
284300100402     c* extra budget
284400100402     c                   if        *in35
284500100402     C                   ADD       1             WTSPEE_re                       EUROEXPRESS
284600100402     C                   ADD       TASNCL        WTCLEE_re
284700100402     C                   ADD       w_peso        WTKGEE_re
284800100402     C                   ADD       TASVLF        WTMCEE_re
284900100402     c* tipo servizio  "E"
285000100402     c                   if        tastsp='E'
285100100402     C                   ADD       1             eWTSPEE_re                       EUROEXPRESS
285200100402     C                   ADD       TASNCL        eWTCLEE_re
285300100402     C                   ADD       w_peso        eWTKGEE_re
285400100402     C                   ADD       TASVLF        eWTMCEE_re
285500100402     c                   endif
285600100402     c* tipo servizio  "H"
285700100402     c                   if        tastsp='H'
285800100402     C                   ADD       1             hWTSPEE_re                       EUROEXPRESS
285900100402     C                   ADD       TASNCL        hWTCLEE_re
286000100402     C                   ADD       w_peso        hWTKGEE_re
286100100402     C                   ADD       TASVLF        hWTMCEE_re
286200100402     c                   endif
286300100402     c                   endif
286400120904     c
286500120904     c                   eval      ks_tip='EE'
286600120904     c                   exsr      spe_peso
286700100402     c
286800100402     c                   ENDSR
286900100402     c****************************************************************************
287000100402     c     speDPD        BEGSR
287100100402     c* giornaliero
287200100402     c                   if        (tasaas*10000+tasmgs)=§al
287300100402     C                   ADD       1             WTSPED                         DPD
287400100402     C                   ADD       TASNCL        WTCLED
287500100402     C                   ADD       w_peso        WTKGED
287600100402     C                   ADD       TASVLF        WTMCED
287700100402     c* tipo servizio  "E"
287800100402     c                   if        tastsp='E'
287900100402     C                   ADD       1             eWTSPED                         DPD
288000100402     C                   ADD       TASNCL        eWTCLED
288100100402     C                   ADD       w_peso        eWTKGED
288200100402     C                   ADD       TASVLF        eWTMCED
288300100402     c                   endif
288400100402     c* tipo servizio  "H"
288500100402     c                   if        tastsp='H'
288600100402     C                   ADD       1             hWTSPED                         DPD
288700100402     C                   ADD       TASNCL        hWTCLED
288800100402     C                   ADD       w_peso        hWTKGED
288900100402     C                   ADD       TASVLF        hWTMCED
289000100402     c                   endif
289100100402     c* extra budget
289200100402     c                   if        *in35
289300100402     C                   ADD       1             WTSPED_e                       DPD
289400100402     C                   ADD       TASNCL        WTCLED_e
289500100402     C                   ADD       w_peso        WTKGED_e
289600100402     C                   ADD       TASVLF        WTMCED_e
289700100402     c* tipo servizio  "E"
289800100402     c                   if        tastsp='E'
289900100402     C                   ADD       1             eWTSPED_e                       DPD
290000100402     C                   ADD       TASNCL        eWTCLED_e
290100100402     C                   ADD       w_peso        eWTKGED_e
290200100402     C                   ADD       TASVLF        eWTMCED_e
290300100402     c                   endif
290400100402     c* tipo servizio  "H"
290500100402     c                   if        tastsp='H'
290600100402     C                   ADD       1             hWTSPED_e                       DPD
290700100402     C                   ADD       TASNCL        hWTCLED_e
290800100402     C                   ADD       w_peso        hWTKGED_e
290900100402     C                   ADD       TASVLF        hWTMCED_e
291000100402     c                   endif
291100100402     c
291200100402     c                   endif
291300100402     c                   endif
291400100402     c* riepilogo
291500100402     C                   ADD       1             WTSPED_r                       DPD
291600100402     C                   ADD       TASNCL        WTCLED_r
291700100402     C                   ADD       w_peso        WTKGED_r
291800100402     C                   ADD       TASVLF        WTMCED_r
291900100402     c* tipo servizio  "E"
292000100402     c                   if        tastsp='E'
292100100402     C                   ADD       1             eWTSPED_r                       DPD
292200100402     C                   ADD       TASNCL        eWTCLED_r
292300100402     C                   ADD       w_peso        eWTKGED_r
292400100402     C                   ADD       TASVLF        eWTMCED_r
292500100402     c                   endif
292600100402     c* tipo servizio  "H"
292700100402     c                   if        tastsp='H'
292800100402     C                   ADD       1             hWTSPED_r                       DPD
292900100402     C                   ADD       TASNCL        hWTCLED_r
293000100402     C                   ADD       w_peso        hWTKGED_r
293100100402     C                   ADD       TASVLF        hWTMCED_r
293200100402     c                   endif
293300100402     c* extra budget
293400100402     c                   if        *in35
293500100402     C                   ADD       1             WTSPED_re                       DPD
293600100402     C                   ADD       TASNCL        WTCLED_re
293700100402     C                   ADD       w_peso        WTKGED_re
293800100402     C                   ADD       TASVLF        WTMCED_re
293900100402     c* tipo servizio  "E"
294000100402     c                   if        tastsp='E'
294100100402     C                   ADD       1             eWTSPED_re                       DPD
294200100402     C                   ADD       TASNCL        eWTCLED_re
294300100402     C                   ADD       w_peso        eWTKGED_re
294400100402     C                   ADD       TASVLF        eWTMCED_re
294500100402     c                   endif
294600100402     c* tipo servizio  "H"
294700100402     c                   if        tastsp='H'
294800100402     C                   ADD       1             hWTSPED_re                       DPD
294900100402     C                   ADD       TASNCL        hWTCLED_re
295000100402     C                   ADD       w_peso        hWTKGED_re
295100100402     C                   ADD       TASVLF        hWTMCED_re
295200100402     c                   endif
295300100402     c                   endif
295400120904     c
295500120904     c                   eval      ks_tip='ED'
295600120904     c                   exsr      spe_peso
295700120904     c
295800100402     c                   ENDSR
295900100402     c****************************************************************************
296000100402     c     speFEDEX      BEGSR
296100100402     c* giornaliero
296200100402     c                   if        (tasaas*10000+tasmgs)=§al
296300100402     C                   ADD       1             WTSPEX                         FEDEX
296400100402     C                   ADD       TASNCL        WTCLEX
296500100402     C                   ADD       w_peso        WTKGEX
296600100402     C                   ADD       TASVLF        WTMCEX
296700100402     c* tipo servizio  "E"
296800100402     c                   if        tastsp='E'
296900100402     C                   ADD       1             eWTSPEX                         FEDEX
297000100402     C                   ADD       TASNCL        eWTCLEX
297100100402     C                   ADD       w_peso        eWTKGEX
297200100402     C                   ADD       TASVLF        eWTMCEX
297300100402     c                   endif
297400100402     c* tipo servizio  "H"
297500100402     c                   if        tastsp='H'
297600100402     C                   ADD       1             hWTSPEX                         FEDEX
297700100402     C                   ADD       TASNCL        hWTCLEX
297800100402     C                   ADD       w_peso        hWTKGEX
297900100402     C                   ADD       TASVLF        hWTMCEX
298000100402     c                   endif
298100100402     c* extra budget
298200100402     c                   if        *in35
298300100402     C                   ADD       1             WTSPEX_e                       FEDEX
298400100402     C                   ADD       TASNCL        WTCLEX_e
298500100402     C                   ADD       w_peso        WTKGEX_e
298600100402     C                   ADD       TASVLF        WTMCEX_e
298700100402     c* tipo servizio  "E"
298800100402     c                   if        tastsp='E'
298900100402     C                   ADD       1             eWTSPEX_e                       FEDEX
299000100402     C                   ADD       TASNCL        eWTCLEX_e
299100100402     C                   ADD       w_peso        eWTKGEX_e
299200100402     C                   ADD       TASVLF        eWTMCEX_e
299300100402     c                   endif
299400100402     c* tipo servizio  "H"
299500100402     c                   if        tastsp='H'
299600100402     C                   ADD       1             hWTSPEX_e                       FEDEX
299700100402     C                   ADD       TASNCL        hWTCLEX_e
299800100402     C                   ADD       w_peso        hWTKGEX_e
299900100402     C                   ADD       TASVLF        hWTMCEX_e
300000100402     c                   endif
300100100402     c                   endif
300200100402     c                   endif
300300100402     c* riepilogo
300400100402     C                   ADD       1             WTSPEX_r                       FEDEX
300500100402     C                   ADD       TASNCL        WTCLEX_r
300600100402     C                   ADD       w_peso        WTKGEX_r
300700100402     C                   ADD       TASVLF        WTMCEX_r
300800100402     c* tipo servizio  "E"
300900100402     c                   if        tastsp='E'
301000100402     C                   ADD       1             eWTSPEX_r                       FEDEX
301100100402     C                   ADD       TASNCL        eWTCLEX_r
301200100402     C                   ADD       w_peso        eWTKGEX_r
301300100402     C                   ADD       TASVLF        eWTMCEX_r
301400100402     c                   endif
301500100402     c* tipo servizio  "H"
301600100402     c                   if        tastsp='H'
301700100402     C                   ADD       1             hWTSPEX_r                       FEDEX
301800100402     C                   ADD       TASNCL        hWTCLEX_r
301900100402     C                   ADD       w_peso        hWTKGEX_r
302000100402     C                   ADD       TASVLF        hWTMCEX_r
302100100402     c                   endif
302200100402     c* Extra budget
302300100402     c                   if        *in35
302400100402     C                   ADD       1             WTSPEX_re                       FEDEX
302500100402     C                   ADD       TASNCL        WTCLEX_re
302600100402     C                   ADD       w_peso        WTKGEX_re
302700100402     C                   ADD       TASVLF        WTMCEX_re
302800100402     c* tipo servizio  "E"
302900100402     c                   if        tastsp='E'
303000100402     C                   ADD       1             eWTSPEX_re                       FEDEX
303100100402     C                   ADD       TASNCL        eWTCLEX_re
303200100402     C                   ADD       w_peso        eWTKGEX_re
303300100402     C                   ADD       TASVLF        eWTMCEX_re
303400100402     c                   endif
303500100402     c* tipo servizio  "H"
303600100402     c                   if        tastsp='H'
303700100402     C                   ADD       1             hWTSPEX_re                       FEDEX
303800100402     C                   ADD       TASNCL        hWTCLEX_re
303900100402     C                   ADD       w_peso        hWTKGEX_re
304000100402     C                   ADD       TASVLF        hWTMCEX_re
304100100402     c                   endif
304200100402     c
304300100402     c                   endif
304400120904     c
304500120904     c                   eval      ks_tip='EX'
304600120904     c                   exsr      spe_peso
304700120904     c
304800100402     c                   ENDSR
304900100402     c****************************************************************************
305000100402     c     speITA_LNA    BEGSR
305100100402     c* giornaliero
305200100402     c                   if        (tasaas*10000+tasmgs)=§al
305300100402     C                   ADD       1             WTSPIA
305400100402     C                   ADD       TASNCL        WTCLIA
305500100402     C                   ADD       W_peso        WTKGIA
305600100402     C                   ADD       TASVLF        WTMCIA
305700100402     c* tipo servizio  "E"
305800100402     c                   if        tastsp='E'
305900100402     C                   ADD       1             eWTSPIA
306000100402     C                   ADD       TASNCL        eWTCLIA
306100100402     C                   ADD       W_peso        eWTKGIA
306200100402     C                   ADD       TASVLF        eWTMCIA
306300100402     c                   endif
306400100402     c* tipo servizio  "H"
306500100402     c                   if        tastsp='H'
306600100402     C                   ADD       1             hWTSPIA
306700100402     C                   ADD       TASNCL        hWTCLIA
306800100402     C                   ADD       W_peso        hWTKGIA
306900100402     C                   ADD       TASVLF        hWTMCIA
307000100402     c                   endif
307100100402     c* extra budget
307200100402     c                   if        *in35
307300100402     C                   ADD       1             WTSPIA_e
307400100402     C                   ADD       TASNCL        WTCLIA_e
307500100402     C                   ADD       w_peso        WTKGIA_e
307600100402     C                   ADD       TASVLF        WTMCIA_e
307700100402     c* tipo servizio  "E"
307800100402     c                   if        tastsp='E'
307900100402     C                   ADD       1             eWTSPIA_e
308000100402     C                   ADD       TASNCL        eWTCLIA_e
308100100402     C                   ADD       w_peso        eWTKGIA_e
308200100402     C                   ADD       TASVLF        eWTMCIA_e
308300100402     c                   endif
308400100402     c* tipo servizio  "H"
308500100402     c                   if        tastsp='H'
308600100402     C                   ADD       1             hWTSPIA_e
308700100402     C                   ADD       TASNCL        hWTCLIA_e
308800100402     C                   ADD       w_peso        hWTKGIA_e
308900100402     C                   ADD       TASVLF        hWTMCIA_e
309000100402     c                   endif
309100100402     c
309200100402     c                   endif
309300100402     c                   endif
309400100402     c* riepilogo
309500100402     C                   ADD       1             WTSPIA_r
309600100402     C                   ADD       TASNCL        WTCLIA_r
309700100402     C                   ADD       w_peso        WTKGIA_r
309800100402     C                   ADD       TASVLF        WTMCIA_r
309900100402     c* tipo servizio  "E"
310000100402     c                   if        tastsp='E'
310100100402     C                   ADD       1             eWTSPIA_r
310200100402     C                   ADD       TASNCL        eWTCLIA_r
310300100402     C                   ADD       w_peso        eWTKGIA_r
310400100402     C                   ADD       TASVLF        eWTMCIA_r
310500100402     c                   endif
310600100402     c* tipo servizio  "H"
310700100402     c                   if        tastsp='H'
310800100402     C                   ADD       1             hWTSPIA_r
310900100402     C                   ADD       TASNCL        hWTCLIA_r
311000100402     C                   ADD       w_peso        hWTKGIA_r
311100100402     C                   ADD       TASVLF        hWTMCIA_r
311200100402     c                   endif
311300100402     c* extra budget
311400100402     c                   if        *in35
311500100402     C                   ADD       1             WTSPIA_re
311600100402     C                   ADD       TASNCL        WTCLIA_re
311700100402     C                   ADD       w_peso        WTKGIA_re
311800100402     C                   ADD       TASVLF        WTMCIA_re
311900100402     c* tipo servizio  "E"
312000100402     c                   if        tastsp='E'
312100100402     C                   ADD       1             eWTSPIA_re
312200100402     C                   ADD       TASNCL        eWTCLIA_re
312300100402     C                   ADD       w_peso        eWTKGIA_re
312400100402     C                   ADD       TASVLF        eWTMCIA_re
312500100402     c                   endif
312600100402     c* tipo servizio  "H"
312700100402     c                   if        tastsp='H'
312800100402     C                   ADD       1             hWTSPIA_re
312900100402     C                   ADD       TASNCL        hWTCLIA_re
313000100402     C                   ADD       w_peso        hWTKGIA_re
313100100402     C                   ADD       TASVLF        hWTMCIA_re
313200100402     c                   endif
313300100402     c                   endif
313400120904     c
313500120904     c                   eval      ks_tip='IA'
313600120904     c                   exsr      spe_peso
313700120904     c
313800100402     c                   ENDSR
313900100402     c****************************************************************************
314000100402     c     speEXPORT     BEGSR
314100100402     c* giornaliero
314200100402     c                   if        (tasaas*10000+tasmgs)=§al
314300100402     C                   ADD       1             WTSPEA
314400100402     C                   ADD       TASNCL        WTCLEA
314500100402     C                   ADD       w_peso        WTKGEA
314600100402     C                   ADD       TASVLF        WTMCEA
314700100402     c* tipo servizio  "E"
314800100402     c                   if        tastsp='E'
314900100402     C                   ADD       1             eWTSPEA
315000100402     C                   ADD       TASNCL        eWTCLEA
315100100402     C                   ADD       w_peso        eWTKGEA
315200100402     C                   ADD       TASVLF        eWTMCEA
315300100402     c                   endif
315400100402     c* tipo servizio  "H"
315500100402     c                   if        tastsp='H'
315600100402     C                   ADD       1             hWTSPEA
315700100402     C                   ADD       TASNCL        hWTCLEA
315800100402     C                   ADD       w_peso        hWTKGEA
315900100402     C                   ADD       TASVLF        hWTMCEA
316000100402     c                   endif
316100100402     c* extra budget
316200100402     c                   if        *in35
316300100402     C                   ADD       1             WTSPEA_e
316400100402     C                   ADD       TASNCL        WTCLEA_e
316500100402     C                   ADD       w_peso        WTKGEA_e
316600100402     C                   ADD       TASVLF        WTMCEA_e
316700100402     c* tipo servizio  "E"
316800100402     c                   if        tastsp='E'
316900100402     C                   ADD       1             eWTSPEA_e
317000100402     C                   ADD       TASNCL        eWTCLEA_e
317100100402     C                   ADD       w_peso        eWTKGEA_e
317200100402     C                   ADD       TASVLF        eWTMCEA_e
317300100402     c                   endif
317400100402     c* tipo servizio  "H"
317500100402     c                   if        tastsp='H'
317600100402     C                   ADD       1             hWTSPEA_e
317700100402     C                   ADD       TASNCL        hWTCLEA_e
317800100402     C                   ADD       w_peso        hWTKGEA_e
317900100402     C                   ADD       TASVLF        hWTMCEA_e
318000100402     c                   endif
318100100402     c
318200100402     c                   endif
318300100402     c                   endif
318400100402     c* riepilogo
318500100402     C                   ADD       1             WTSPEA_r
318600100402     C                   ADD       TASNCL        WTCLEA_r
318700100402     C                   ADD       w_peso        WTKGEA_r
318800100402     C                   ADD       TASVLF        WTMCEA_r
318900100402     c* tipo servizio  "E"
319000100402     c                   if        tastsp='E'
319100100402     C                   ADD       1             eWTSPEA_r
319200100402     C                   ADD       TASNCL        eWTCLEA_r
319300100402     C                   ADD       w_peso        eWTKGEA_r
319400100402     C                   ADD       TASVLF        eWTMCEA_r
319500100402     c                   endif
319600100402     c* tipo servizio  "H"
319700100402     c                   if        tastsp='H'
319800100402     C                   ADD       1             hWTSPEA_r
319900100402     C                   ADD       TASNCL        hWTCLEA_r
320000100402     C                   ADD       w_peso        hWTKGEA_r
320100100402     C                   ADD       TASVLF        hWTMCEA_r
320200100402     c                   endif
320300100402     c* extra budget
320400100402     c                   if        *in35
320500100402     C                   ADD       1             WTSPEA_re
320600100402     C                   ADD       TASNCL        WTCLEA_re
320700100402     C                   ADD       w_peso        WTKGEA_re
320800100402     C                   ADD       TASVLF        WTMCEA_re
320900100402     c* tipo servizio  "E"
321000100402     c                   if        tastsp='E'
321100100402     C                   ADD       1             eWTSPEA_re
321200100402     C                   ADD       TASNCL        eWTCLEA_re
321300100402     C                   ADD       w_peso        eWTKGEA_re
321400100402     C                   ADD       TASVLF        eWTMCEA_re
321500100402     c                   endif
321600100402     c* tipo servizio  "H"
321700100402     c                   if        tastsp='H'
321800100402     C                   ADD       1             hWTSPEA_re
321900100402     C                   ADD       TASNCL        hWTCLEA_re
322000100402     C                   ADD       w_peso        hWTKGEA_re
322100100402     C                   ADD       TASVLF        hWTMCEA_re
322200100402     c                   endif
322300100402     c
322400100402     c                   endif
322500120904     c
322600120904     c                   eval      ks_tip='EA'
322700120904     c                   exsr      spe_peso
322800120904     c
322900100402     c                   ENDSR
323000120903     c****************************************************************************
323100120903     c     SPE_peso      BEGSR
323200120903     c
323300120905     c* scaglioni di peso solo se richiesto
323400120905    1c                   if        §stat=' ' and §opz='4'
323500120903     c
323600120903     c* in base al peso trovo lo scaglione e faccio subito la write
323700120903     c                   z-add     1             II
323800120903     c                   clear                   ks_sca
323900120903     c                   dow       ks_sca  =0
324000120903     C     W_peso        IFGT      KIR(Ii)
324100120903     C     W_peso        ANDLE     KFR(Ii)
324200120904     C                   MOVEL     KFR(Ii)       ks_sca
324300120903     C                   endif
324400120903     c                   add       1             II
324500120903     c                   enddo
324600120903     c
324700120903     c* la write/update per peso la faccio subito
324800120903     c
324900120903    2c                   if        Wtipo='G' or Wtipo='R'
325000120903     c
325100120903    3c                   if        (tasaas*10000+tasmgs)=§al
325200120903     c                   eval      ks_tie='G'
325300120903     c                   eval      ks_lin=fil(k)
325400120905     c     kss61         chain     WFLSS61l
325500120903     c
325600120905     c                   if        not %found(WFLSS61l)
325700120905     c                   clear                   WFLSS600
325800120903     c                   eval      wsstie=ks_tie
325900120903     c                   eval      wsstip=ks_tip
326000120903     c                   eval      wsskgf=ks_sca
326100120904     c                   eval      wsslin=ks_lin
326200120903     c                   eval      wssdal=§al
326300120903     c                   eval      wssall=§al
326400120903     c                   endif
326500120903     c
326600120903     c                   exsr      Scrivi_pes
326700120903     c
326800120903    3c                   endif
326900120903     c
327000120903    2c                   endif
327100120903
327200120903    2c                   if        Wtipo='P' or Wtipo='R'
327300120903     c                   eval      ks_tie=wtipo
327400120903     c                   eval      ks_lin=fil(k)
327500120905     c     kss61         chain     WFLSS61l
327600120903     c
327700120905     c                   if        not %found(WFLSS61l)
327800120905     c                   clear                   WFLSS600
327900120903     c                   eval      wsstie=ks_tie
328000120903     c                   eval      wsstip=ks_tip
328100120903     c                   eval      wsskgf=ks_sca
328200120904     c                   eval      wsslin=ks_lin
328300120903     c                   eval      wssdal=§dal
328400120903     c                   eval      wssall=§al
328500120903     c                   endif
328600120903     c
328700120903     c                   exsr      Scrivi_pes
328800120903     c
328900120903    2c                   endif
329000120903     c
329100120903    1c                   endif
329200120903     c                   ENDSR
329300120903     c****************************************************************************
329400120903     c     Scrivi_pes    BEGSR
329500120903     c                   add       1             wsstsp
329600120903     C                   ADD       TASNCL        wsstcl
329700120903     C                   ADD       w_peso        wsstkg
329800120903     C                   ADD       TASVLF        wsstmc
329900120903     c                   if        tastsp='E'
330000120903     c                   add       1             wsstspe
330100120903     C                   ADD       TASNCL        wsstcle
330200120903     C                   ADD       w_peso        wsstkge
330300120903     C                   ADD       TASVLF        wsstmce
330400120903     c                   endif
330500120903     c                   if        tastsp='h'
330600120903     c                   add       1             wsstsph
330700120903     C                   ADD       TASNCL        wsstclh
330800120903     C                   ADD       w_peso        wsstkgh
330900120903     C                   ADD       TASVLF        wsstmch
331000120903     c                   endif
331100120903     c
331200120905     c                   if        not %found(WFLSS61l)
331300120905     c                   write     WFLSS600
331400120903     c                   else
331500120905     c                   update    WFLSS600
331600120903     c                   endif
331700120903     c
331800120903     c                   ENDSR
331900100402     c****************************************************************************
332000970519     OQSYSPRT   E            TESTA          2 02
332100090206     O                                         +  1 'Terminata elaborazione'
332200090206     o                                         +  2 'statistiche spedizioni'
332300090206     O                                         +  2 'DEL'
332400970519     O                       UDATE         Y   +  1
332500970528     O                       §DAL              +  5 '    /  /  '
332600970528     O                                         +  1 '>'
332700970528     O                       §AL               +  1 '    /  /  '
332800090206     O                                          120 'TNSSL3R1'
332900970519     O                       PAGE          Z    132
333000970519     O                                          128 'PAG.'
333100970529     O          E            TESTA          2
333200970529     O                                           14 'TIPI BOLLA OK:'
333300970529     O                       TBO               +  1
333400970529     O          E            ERR            1
333500970529     O                       TASLNP               3
333600970529     O                       TASLNA            +  1
333700970529     O                       TASAAS            +  1
333800970529     O                       TASMGS            +  1
333900970529     O                       TASNRS            +  1
334000970529     O                       TASNSP            +  1
334100970529     O                       TASTBL            +  1
334200110801     O          E            TESTA       1  1
334300110801     O                                            7 'Clienti'
334400110801     o                       Ptipocli         +   1
334500110801     O                                        +   1 ':'
334600110801     O          E            detksc         1
334700110801     o                       skicli             132
334800090915**
334900090915OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES)
